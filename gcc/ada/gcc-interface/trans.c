/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                T R A N S                                 *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *          Copyright (C) 1992-2016, Free Software Foundation, Inc.         *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed  with GNAT;  see file  COPYING3.  If not see *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "function.h"
#include "bitmap.h"
#include "tree.h"
#include "gimple-expr.h"
#include "stringpool.h"
#include "cgraph.h"
#include "predict.h"
#include "diagnostic.h"
#include "alias.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "stmt.h"
#include "varasm.h"
#include "output.h"
#include "libfuncs.h"	/* For set_stack_check_libfunc.  */
#include "tree-iterator.h"
#include "gimplify.h"
#include "opts.h"
#include "common/common-target.h"

#include "ada.h"
#include "adadecode.h"
#include "types.h"
#include "atree.h"
#include "namet.h"
#include "nlists.h"
#include "snames.h"
#include "stringt.h"
#include "uintp.h"
#include "urealp.h"
#include "fe.h"
#include "sinfo.h"
#include "einfo.h"
#include "gadaint.h"
#include "ada-tree.h"
#include "gigi.h"

/* We should avoid allocating more than ALLOCA_THRESHOLD bytes via alloca,
   for fear of running out of stack space.  If we need more, we use xmalloc
   instead.  */
#define ALLOCA_THRESHOLD 1000

/* Pointers to front-end tables accessed through macros.  */
struct Node *Nodes_Ptr;
struct Flags *Flags_Ptr;
Node_Id *Next_Node_Ptr;
Node_Id *Prev_Node_Ptr;
struct Elist_Header *Elists_Ptr;
struct Elmt_Item *Elmts_Ptr;
struct String_Entry *Strings_Ptr;
Char_Code *String_Chars_Ptr;
struct List_Header *List_Headers_Ptr;

/* Highest number in the front-end node table.  */
int max_gnat_nodes;

/* Current node being treated, in case abort called.  */
Node_Id error_gnat_node;

/* True when gigi is being called on an analyzed but unexpanded
   tree, and the only purpose of the call is to properly annotate
   types with representation information.  */
bool type_annotate_only;

/* List of N_Validate_Unchecked_Conversion nodes in the unit.  */
static vec<Node_Id> gnat_validate_uc_list;

/* When not optimizing, we cache the 'First, 'Last and 'Length attributes
   of unconstrained array IN parameters to avoid emitting a great deal of
   redundant instructions to recompute them each time.  */
struct GTY (()) parm_attr_d {
  int id; /* GTY doesn't like Entity_Id.  */
  int dim;
  tree first;
  tree last;
  tree length;
};

typedef struct parm_attr_d *parm_attr;


struct GTY(()) language_function {
  vec<parm_attr, va_gc> *parm_attr_cache;
  bitmap named_ret_val;
  vec<tree, va_gc> *other_ret_val;
  int gnat_ret;
};

#define f_parm_attr_cache \
  DECL_STRUCT_FUNCTION (current_function_decl)->language->parm_attr_cache

#define f_named_ret_val \
  DECL_STRUCT_FUNCTION (current_function_decl)->language->named_ret_val

#define f_other_ret_val \
  DECL_STRUCT_FUNCTION (current_function_decl)->language->other_ret_val

#define f_gnat_ret \
  DECL_STRUCT_FUNCTION (current_function_decl)->language->gnat_ret

/* A structure used to gather together information about a statement group.
   We use this to gather related statements, for example the "then" part
   of a IF.  In the case where it represents a lexical scope, we may also
   have a BLOCK node corresponding to it and/or cleanups.  */

struct GTY((chain_next ("%h.previous"))) stmt_group {
  struct stmt_group *previous;	/* Previous code group.  */
  tree stmt_list;		/* List of statements for this code group.  */
  tree block;			/* BLOCK for this code group, if any.  */
  tree cleanups;		/* Cleanups for this code group, if any.  */
};

static GTY(()) struct stmt_group *current_stmt_group;

/* List of unused struct stmt_group nodes.  */
static GTY((deletable)) struct stmt_group *stmt_group_free_list;

/* A structure used to record information on elaboration procedures
   we've made and need to process.

   ??? gnat_node should be Node_Id, but gengtype gets confused.  */

struct GTY((chain_next ("%h.next"))) elab_info {
  struct elab_info *next;	/* Pointer to next in chain.  */
  tree elab_proc;		/* Elaboration procedure.  */
  int gnat_node;		/* The N_Compilation_Unit.  */
};

static GTY(()) struct elab_info *elab_info_list;

/* Stack of exception pointer variables.  Each entry is the VAR_DECL
   that stores the address of the raised exception.  Nonzero means we
   are in an exception handler.  Not used in the zero-cost case.  */
static GTY(()) vec<tree, va_gc> *gnu_except_ptr_stack;

/* In ZCX case, current exception pointer.  Used to re-raise it.  */
static GTY(()) tree gnu_incoming_exc_ptr;

/* Stack for storing the current elaboration procedure decl.  */
static GTY(()) vec<tree, va_gc> *gnu_elab_proc_stack;

/* Stack of labels to be used as a goto target instead of a return in
   some functions.  See processing for N_Subprogram_Body.  */
static GTY(()) vec<tree, va_gc> *gnu_return_label_stack;

/* Stack of variable for the return value of a function with copy-in/copy-out
   parameters.  See processing for N_Subprogram_Body.  */
static GTY(()) vec<tree, va_gc> *gnu_return_var_stack;

/* Structure used to record information for a range check.  */
struct GTY(()) range_check_info_d {
  tree low_bound;
  tree high_bound;
  tree disp;
  bool neg_p;
  tree type;
  tree invariant_cond;
  tree inserted_cond;
};

typedef struct range_check_info_d *range_check_info;


/* Structure used to record information for a loop.  */
struct GTY(()) loop_info_d {
  tree stmt;
  tree loop_var;
  tree low_bound;
  tree high_bound;
  vec<range_check_info, va_gc> *checks;
  bool artificial;
  bool has_checks;
  bool warned_aggressive_loop_optimizations;
};

typedef struct loop_info_d *loop_info;


/* Stack of loop_info structures associated with LOOP_STMT nodes.  */
static GTY(()) vec<loop_info, va_gc> *gnu_loop_stack;

/* The stacks for N_{Push,Pop}_*_Label.  */
static GTY(()) vec<tree, va_gc> *gnu_constraint_error_label_stack;
static GTY(()) vec<tree, va_gc> *gnu_storage_error_label_stack;
static GTY(()) vec<tree, va_gc> *gnu_program_error_label_stack;

/* Map GNAT tree codes to GCC tree codes for simple expressions.  */
static enum tree_code gnu_codes[Number_Node_Kinds];

static void init_code_table (void);
static void Compilation_Unit_to_gnu (Node_Id);
static bool empty_stmt_list_p (tree);
static void record_code_position (Node_Id);
static void insert_code_for (Node_Id);
static void add_cleanup (tree, Node_Id);
static void add_stmt_list (List_Id);
static void push_exception_label_stack (vec<tree, va_gc> **, Entity_Id);
static tree build_stmt_group (List_Id, bool);
static inline bool stmt_group_may_fallthru (void);
static enum gimplify_status gnat_gimplify_stmt (tree *);
static void elaborate_all_entities (Node_Id);
static void process_freeze_entity (Node_Id);
static void process_decls (List_Id, List_Id, Node_Id, bool, bool);
static tree emit_range_check (tree, Node_Id, Node_Id);
static tree emit_check (tree, tree, int, Node_Id);
static tree build_unary_op_trapv (enum tree_code, tree, tree, Node_Id);
static tree build_binary_op_trapv (enum tree_code, tree, tree, tree, Node_Id);
static tree convert_with_check (Entity_Id, tree, bool, bool, bool, Node_Id);
static bool addressable_p (tree, tree);
static tree assoc_to_constructor (Entity_Id, Node_Id, tree);
static tree extract_values (tree, tree);
static tree pos_to_constructor (Node_Id, tree, Entity_Id);
static void validate_unchecked_conversion (Node_Id);
static tree maybe_implicit_deref (tree);
static void set_expr_location_from_node (tree, Node_Id, bool = false);
static void set_gnu_expr_location_from_node (tree, Node_Id);
static bool set_end_locus_from_node (tree, Node_Id);
static int lvalue_required_p (Node_Id, tree, bool, bool, bool);
static tree build_raise_check (int, enum exception_info_kind);
static tree create_init_temporary (const char *, tree, tree *, Node_Id);

/* Hooks for debug info back-ends, only supported and used in a restricted set
   of configurations.  */
static const char *extract_encoding (const char *) ATTRIBUTE_UNUSED;
static const char *decode_name (const char *) ATTRIBUTE_UNUSED;

/* This is the main program of the back-end.  It sets up all the table
   structures and then generates code.  */

void
gigi (Node_Id gnat_root,
      int max_gnat_node,
      int number_name ATTRIBUTE_UNUSED,
      struct Node *nodes_ptr,
      struct Flags *flags_ptr,
      Node_Id *next_node_ptr,
      Node_Id *prev_node_ptr,
      struct Elist_Header *elists_ptr,
      struct Elmt_Item *elmts_ptr,
      struct String_Entry *strings_ptr,
      Char_Code *string_chars_ptr,
      struct List_Header *list_headers_ptr,
      Nat number_file,
      struct File_Info_Type *file_info_ptr,
      Entity_Id standard_boolean,
      Entity_Id standard_integer,
      Entity_Id standard_character,
      Entity_Id standard_long_long_float,
      Entity_Id standard_exception_type,
      Int gigi_operating_mode)
{
  Node_Id gnat_iter;
  Entity_Id gnat_literal;
  tree t, ftype, int64_type;
  struct elab_info *info;
  int i;

  max_gnat_nodes = max_gnat_node;

  Nodes_Ptr = nodes_ptr;
  Flags_Ptr = flags_ptr;
  Next_Node_Ptr = next_node_ptr;
  Prev_Node_Ptr = prev_node_ptr;
  Elists_Ptr = elists_ptr;
  Elmts_Ptr = elmts_ptr;
  Strings_Ptr = strings_ptr;
  String_Chars_Ptr = string_chars_ptr;
  List_Headers_Ptr = list_headers_ptr;

  type_annotate_only = (gigi_operating_mode == 1);

  for (i = 0; i < number_file; i++)
    {
      /* Use the identifier table to make a permanent copy of the filename as
	 the name table gets reallocated after Gigi returns but before all the
	 debugging information is output.  The __gnat_to_canonical_file_spec
	 call translates filenames from pragmas Source_Reference that contain
	 host style syntax not understood by gdb.  */
      const char *filename
	= IDENTIFIER_POINTER
	   (get_identifier
	    (__gnat_to_canonical_file_spec
	     (Get_Name_String (file_info_ptr[i].File_Name))));

      /* We rely on the order isomorphism between files and line maps.  */
      gcc_assert ((int) LINEMAPS_ORDINARY_USED (line_table) == i);

      /* We create the line map for a source file at once, with a fixed number
	 of columns chosen to avoid jumping over the next power of 2.  */
      linemap_add (line_table, LC_ENTER, 0, filename, 1);
      linemap_line_start (line_table, file_info_ptr[i].Num_Source_Lines, 252);
      linemap_position_for_column (line_table, 252 - 1);
      linemap_add (line_table, LC_LEAVE, 0, NULL, 0);
    }

  gcc_assert (Nkind (gnat_root) == N_Compilation_Unit);

  /* Declare the name of the compilation unit as the first global
     name in order to make the middle-end fully deterministic.  */
  t = create_concat_name (Defining_Entity (Unit (gnat_root)), NULL);
  first_global_object_name = ggc_strdup (IDENTIFIER_POINTER (t));

  /* Initialize ourselves.  */
  init_code_table ();
  init_gnat_decl ();
  init_gnat_utils ();

  /* If we are just annotating types, give VOID_TYPE zero sizes to avoid
     errors.  */
  if (type_annotate_only)
    {
      TYPE_SIZE (void_type_node) = bitsize_zero_node;
      TYPE_SIZE_UNIT (void_type_node) = size_zero_node;
    }

  /* Enable GNAT stack checking method if needed */
  if (!Stack_Check_Probes_On_Target)
    set_stack_check_libfunc ("_gnat_stack_check");

  /* Retrieve alignment settings.  */
  double_float_alignment = get_target_double_float_alignment ();
  double_scalar_alignment = get_target_double_scalar_alignment ();

  /* Record the builtin types.  Define `integer' and `character' first so that
     dbx will output them first.  */
  record_builtin_type ("integer", integer_type_node, false);
  record_builtin_type ("character", char_type_node, false);
  record_builtin_type ("boolean", boolean_type_node, false);
  record_builtin_type ("void", void_type_node, false);

  /* Save the type we made for integer as the type for Standard.Integer.  */
  save_gnu_tree (Base_Type (standard_integer),
		 TYPE_NAME (integer_type_node),
		 false);

  /* Likewise for character as the type for Standard.Character.  */
  finish_character_type (char_type_node);
  save_gnu_tree (Base_Type (standard_character),
		 TYPE_NAME (char_type_node),
		 false);

  /* Likewise for boolean as the type for Standard.Boolean.  */
  save_gnu_tree (Base_Type (standard_boolean),
		 TYPE_NAME (boolean_type_node),
		 false);
  gnat_literal = First_Literal (Base_Type (standard_boolean));
  t = UI_To_gnu (Enumeration_Rep (gnat_literal), boolean_type_node);
  gcc_assert (t == boolean_false_node);
  t = create_var_decl (get_entity_name (gnat_literal), NULL_TREE,
		       boolean_type_node, t, true, false, false, false, false,
		       true, false, NULL, gnat_literal);
  save_gnu_tree (gnat_literal, t, false);
  gnat_literal = Next_Literal (gnat_literal);
  t = UI_To_gnu (Enumeration_Rep (gnat_literal), boolean_type_node);
  gcc_assert (t == boolean_true_node);
  t = create_var_decl (get_entity_name (gnat_literal), NULL_TREE,
		       boolean_type_node, t, true, false, false, false, false,
		       true, false, NULL, gnat_literal);
  save_gnu_tree (gnat_literal, t, false);

  /* Declare the building blocks of function nodes.  */
  void_list_node = build_tree_list (NULL_TREE, void_type_node);
  void_ftype = build_function_type_list (void_type_node, NULL_TREE);
  ptr_void_ftype = build_pointer_type (void_ftype);

  /* Now declare run-time functions.  */
  ftype = build_function_type_list (ptr_type_node, sizetype, NULL_TREE);
  malloc_decl
    = create_subprog_decl (get_identifier ("__gnat_malloc"), NULL_TREE,
			   ftype,
			   NULL_TREE, is_disabled, true, true, true, false,
			   NULL, Empty);
  DECL_IS_MALLOC (malloc_decl) = 1;

  ftype = build_function_type_list (void_type_node, ptr_type_node, NULL_TREE);
  free_decl
    = create_subprog_decl (get_identifier ("__gnat_free"), NULL_TREE,
			   ftype,
			   NULL_TREE, is_disabled, true, true, true, false,
			   NULL, Empty);

  ftype = build_function_type_list (ptr_type_node, ptr_type_node, sizetype,
				    NULL_TREE);
  realloc_decl
    = create_subprog_decl (get_identifier ("__gnat_realloc"), NULL_TREE,
			   ftype,
			   NULL_TREE, is_disabled, true, true, true, false,
			   NULL, Empty);

  /* This is used for 64-bit multiplication with overflow checking.  */
  int64_type = gnat_type_for_size (64, 0);
  mulv64_decl
    = create_subprog_decl (get_identifier ("__gnat_mulv64"), NULL_TREE,
			   build_function_type_list (int64_type, int64_type,
						     int64_type, NULL_TREE),
			   NULL_TREE, is_disabled, true, true, true, false,
			   NULL, Empty);

  /* Name of the _Parent field in tagged record types.  */
  parent_name_id = get_identifier (Get_Name_String (Name_uParent));

  /* Name of the Exception_Data type defined in System.Standard_Library.  */
  exception_data_name_id
    = get_identifier ("system__standard_library__exception_data");

  /* Make the types and functions used for exception processing.  */
  except_type_node = gnat_to_gnu_type (Base_Type (standard_exception_type));

  jmpbuf_type
    = build_array_type (gnat_type_for_mode (Pmode, 0),
			build_index_type (size_int (5)));
  record_builtin_type ("JMPBUF_T", jmpbuf_type, true);
  jmpbuf_ptr_type = build_pointer_type (jmpbuf_type);

  /* Functions to get and set the jumpbuf pointer for the current thread.  */
  get_jmpbuf_decl
    = create_subprog_decl
      (get_identifier ("system__soft_links__get_jmpbuf_address_soft"),
       NULL_TREE, build_function_type_list (jmpbuf_ptr_type, NULL_TREE),
       NULL_TREE, is_disabled, true, true, true, false, NULL, Empty);

  set_jmpbuf_decl
    = create_subprog_decl
      (get_identifier ("system__soft_links__set_jmpbuf_address_soft"),
       NULL_TREE, build_function_type_list (void_type_node, jmpbuf_ptr_type,
					    NULL_TREE),
       NULL_TREE, is_disabled, true, true, true, false, NULL, Empty);

  get_excptr_decl
    = create_subprog_decl
      (get_identifier ("system__soft_links__get_gnat_exception"), NULL_TREE,
       build_function_type_list (build_pointer_type (except_type_node),
				 NULL_TREE),
       NULL_TREE, is_disabled, true, true, true, false, NULL, Empty);

  not_handled_by_others_decl = get_identifier ("not_handled_by_others");
  for (t = TYPE_FIELDS (except_type_node); t; t = DECL_CHAIN (t))
    if (DECL_NAME (t) == not_handled_by_others_decl)
      {
	not_handled_by_others_decl = t;
	break;
      }
  gcc_assert (DECL_P (not_handled_by_others_decl));

  /* setjmp returns an integer and has one operand, which is a pointer to
     a jmpbuf.  */
  setjmp_decl
    = create_subprog_decl
      (get_identifier ("__builtin_setjmp"), NULL_TREE,
       build_function_type_list (integer_type_node, jmpbuf_ptr_type,
				 NULL_TREE),
       NULL_TREE, is_disabled, true, true, true, false, NULL, Empty);
  DECL_BUILT_IN_CLASS (setjmp_decl) = BUILT_IN_NORMAL;
  DECL_FUNCTION_CODE (setjmp_decl) = BUILT_IN_SETJMP;

  /* update_setjmp_buf updates a setjmp buffer from the current stack pointer
     address.  */
  update_setjmp_buf_decl
    = create_subprog_decl
      (get_identifier ("__builtin_update_setjmp_buf"), NULL_TREE,
       build_function_type_list (void_type_node, jmpbuf_ptr_type, NULL_TREE),
       NULL_TREE, is_disabled, true, true, true, false, NULL, Empty);
  DECL_BUILT_IN_CLASS (update_setjmp_buf_decl) = BUILT_IN_NORMAL;
  DECL_FUNCTION_CODE (update_setjmp_buf_decl) = BUILT_IN_UPDATE_SETJMP_BUF;

  /* Indicate that it never returns.  */
  ftype = build_function_type_list (void_type_node,
				    build_pointer_type (except_type_node),
				    NULL_TREE);
  ftype = build_qualified_type (ftype, TYPE_QUAL_VOLATILE);
  raise_nodefer_decl
    = create_subprog_decl
      (get_identifier ("__gnat_raise_nodefer_with_msg"), NULL_TREE, ftype,
       NULL_TREE, is_disabled, true, true, true, false, NULL, Empty);

  set_exception_parameter_decl
    = create_subprog_decl
      (get_identifier ("__gnat_set_exception_parameter"), NULL_TREE,
       build_function_type_list (void_type_node, ptr_type_node, ptr_type_node,
				 NULL_TREE),
       NULL_TREE, is_disabled, true, true, true, false, NULL, Empty);

  /* Hooks to call when entering/leaving an exception handler.  */
  ftype = build_function_type_list (void_type_node, ptr_type_node, NULL_TREE);

  begin_handler_decl
    = create_subprog_decl (get_identifier ("__gnat_begin_handler"), NULL_TREE,
			   ftype, NULL_TREE,
			   is_disabled, true, true, true, false, NULL, Empty);

  end_handler_decl
    = create_subprog_decl (get_identifier ("__gnat_end_handler"), NULL_TREE,
			   ftype, NULL_TREE,
			   is_disabled, true, true, true, false, NULL, Empty);

  unhandled_except_decl
    = create_subprog_decl (get_identifier ("__gnat_unhandled_except_handler"),
			   NULL_TREE, ftype, NULL_TREE,
			   is_disabled, true, true, true, false, NULL, Empty);

  /* Indicate that it never returns.  */
  ftype = build_qualified_type (ftype, TYPE_QUAL_VOLATILE);
  reraise_zcx_decl
    = create_subprog_decl (get_identifier ("__gnat_reraise_zcx"), NULL_TREE,
			   ftype, NULL_TREE,
			   is_disabled, true, true, true, false, NULL, Empty);

  /* Dummy objects to materialize "others" and "all others" in the exception
     tables.  These are exported by a-exexpr-gcc.adb, so see this unit for
     the types to use.  */
  others_decl
    = create_var_decl (get_identifier ("OTHERS"),
		       get_identifier ("__gnat_others_value"),
		       char_type_node, NULL_TREE,
		       true, false, true, false, false, true, false,
		       NULL, Empty);

  all_others_decl
    = create_var_decl (get_identifier ("ALL_OTHERS"),
		       get_identifier ("__gnat_all_others_value"),
		       char_type_node, NULL_TREE,
		       true, false, true, false, false, true, false,
		       NULL, Empty);

  unhandled_others_decl
    = create_var_decl (get_identifier ("UNHANDLED_OTHERS"),
		       get_identifier ("__gnat_unhandled_others_value"),
		       char_type_node, NULL_TREE,
		       true, false, true, false, false, true, false,
		       NULL, Empty);

  /* If in no exception handlers mode, all raise statements are redirected to
     __gnat_last_chance_handler.  No need to redefine raise_nodefer_decl since
     this procedure will never be called in this mode.  */
  if (No_Exception_Handlers_Set ())
    {
      /* Indicate that it never returns.  */
      ftype = build_function_type_list (void_type_node,
					build_pointer_type (char_type_node),
					integer_type_node, NULL_TREE);
      ftype = build_qualified_type (ftype, TYPE_QUAL_VOLATILE);
      tree decl
	= create_subprog_decl
	  (get_identifier ("__gnat_last_chance_handler"), NULL_TREE, ftype,
	   NULL_TREE, is_disabled, true, true, true, false, NULL, Empty);
      for (i = 0; i < (int) ARRAY_SIZE (gnat_raise_decls); i++)
	gnat_raise_decls[i] = decl;
    }
  else
    {
      /* Otherwise, make one decl for each exception reason.  */
      for (i = 0; i < (int) ARRAY_SIZE (gnat_raise_decls); i++)
	gnat_raise_decls[i] = build_raise_check (i, exception_simple);
      for (i = 0; i < (int) ARRAY_SIZE (gnat_raise_decls_ext); i++)
	gnat_raise_decls_ext[i]
	  = build_raise_check (i,
			       i == CE_Index_Check_Failed
			       || i == CE_Range_Check_Failed
			       || i == CE_Invalid_Data
			       ? exception_range : exception_column);
    }

  /* Build the special descriptor type and its null node if needed.  */
  if (TARGET_VTABLE_USES_DESCRIPTORS)
    {
      tree null_node = fold_convert (ptr_void_ftype, null_pointer_node);
      tree field_list = NULL_TREE;
      int j;
      vec<constructor_elt, va_gc> *null_vec = NULL;
      constructor_elt *elt;

      fdesc_type_node = make_node (RECORD_TYPE);
      vec_safe_grow (null_vec, TARGET_VTABLE_USES_DESCRIPTORS);
      elt = (null_vec->address () + TARGET_VTABLE_USES_DESCRIPTORS - 1);

      for (j = 0; j < TARGET_VTABLE_USES_DESCRIPTORS; j++)
	{
	  tree field
	    = create_field_decl (NULL_TREE, ptr_void_ftype, fdesc_type_node,
				 NULL_TREE, NULL_TREE, 0, 1);
	  DECL_CHAIN (field) = field_list;
	  field_list = field;
	  elt->index = field;
	  elt->value = null_node;
	  elt--;
	}

      finish_record_type (fdesc_type_node, nreverse (field_list), 0, false);
      record_builtin_type ("descriptor", fdesc_type_node, true);
      null_fdesc_node = gnat_build_constructor (fdesc_type_node, null_vec);
    }

  longest_float_type_node
    = get_unpadded_type (Base_Type (standard_long_long_float));

  main_identifier_node = get_identifier ("main");

  /* If we are using the GCC exception mechanism, let GCC know.  */
  if (Back_End_Exceptions ())
    gnat_init_gcc_eh ();

  /* Initialize the GCC support for FP operations.  */
  gnat_init_gcc_fp ();

  /* Install the builtins we might need, either internally or as user-available
     facilities for Intrinsic imports.  Note that this must be done after the
     GCC exception mechanism is initialized.  */
  gnat_install_builtins ();

  vec_safe_push (gnu_except_ptr_stack, NULL_TREE);
  vec_safe_push (gnu_constraint_error_label_stack, NULL_TREE);
  vec_safe_push (gnu_storage_error_label_stack, NULL_TREE);
  vec_safe_push (gnu_program_error_label_stack, NULL_TREE);

  /* Process any Pragma Ident for the main unit.  */
  if (Present (Ident_String (Main_Unit)))
    targetm.asm_out.output_ident
      (TREE_STRING_POINTER (gnat_to_gnu (Ident_String (Main_Unit))));

  /* Force -fno-strict-aliasing if the configuration pragma was seen.  */
  if (No_Strict_Aliasing_CP)
    flag_strict_aliasing = 0;

  /* Save the current optimization options again after the above possible
     global_options changes.  */
  optimization_default_node = build_optimization_node (&global_options);
  optimization_current_node = optimization_default_node;

  /* Now translate the compilation unit proper.  */
  Compilation_Unit_to_gnu (gnat_root);

  /* Disable -Waggressive-loop-optimizations since we implement our own
     version of the warning.  */
  warn_aggressive_loop_optimizations = 0;

  /* Then process the N_Validate_Unchecked_Conversion nodes.  We do this at
     the very end to avoid having to second-guess the front-end when we run
     into dummy nodes during the regular processing.  */
  for (i = 0; gnat_validate_uc_list.iterate (i, &gnat_iter); i++)
    validate_unchecked_conversion (gnat_iter);
  gnat_validate_uc_list.release ();

  /* Finally see if we have any elaboration procedures to deal with.  */
  for (info = elab_info_list; info; info = info->next)
    {
      tree gnu_body = DECL_SAVED_TREE (info->elab_proc);

      /* We should have a BIND_EXPR but it may not have any statements in it.
	 If it doesn't have any, we have nothing to do except for setting the
	 flag on the GNAT node.  Otherwise, process the function as others.  */
      tree gnu_stmts = gnu_body;
      if (TREE_CODE (gnu_stmts) == BIND_EXPR)
	gnu_stmts = BIND_EXPR_BODY (gnu_stmts);
      if (!gnu_stmts || empty_stmt_list_p (gnu_stmts))
	Set_Has_No_Elaboration_Code (info->gnat_node, 1);
      else
	{
	  begin_subprog_body (info->elab_proc);
	  end_subprog_body (gnu_body);
	  rest_of_subprog_body_compilation (info->elab_proc);
	}
    }

  /* Destroy ourselves.  */
  destroy_gnat_decl ();
  destroy_gnat_utils ();

  /* We cannot track the location of errors past this point.  */
  error_gnat_node = Empty;
}

/* Return a subprogram decl corresponding to __gnat_rcheck_xx for the given
   CHECK if KIND is EXCEPTION_SIMPLE, or else to __gnat_rcheck_xx_ext.  */

static tree
build_raise_check (int check, enum exception_info_kind kind)
{
  tree result, ftype;
  const char pfx[] = "__gnat_rcheck_";

  strcpy (Name_Buffer, pfx);
  Name_Len = sizeof (pfx) - 1;
  Get_RT_Exception_Name (check);

  if (kind == exception_simple)
    {
      Name_Buffer[Name_Len] = 0;
      ftype
	= build_function_type_list (void_type_node,
				    build_pointer_type (char_type_node),
				    integer_type_node, NULL_TREE);
    }
  else
    {
      tree t = (kind == exception_column ? NULL_TREE : integer_type_node);

      strcpy (Name_Buffer + Name_Len, "_ext");
      Name_Buffer[Name_Len + 4] = 0;
      ftype
	= build_function_type_list (void_type_node,
				    build_pointer_type (char_type_node),
				    integer_type_node, integer_type_node,
				    t, t, NULL_TREE);
    }

  /* Indicate that it never returns.  */
  ftype = build_qualified_type (ftype, TYPE_QUAL_VOLATILE);
  result
    = create_subprog_decl (get_identifier (Name_Buffer), NULL_TREE, ftype,
			   NULL_TREE, is_disabled, true, true, true, false,
			   NULL, Empty);

  return result;
}

/* Return a positive value if an lvalue is required for GNAT_NODE, which is
   an N_Attribute_Reference.  */

static int
lvalue_required_for_attribute_p (Node_Id gnat_node)
{
  switch (Get_Attribute_Id (Attribute_Name (gnat_node)))
    {
    case Attr_Pos:
    case Attr_Val:
    case Attr_Pred:
    case Attr_Succ:
    case Attr_First:
    case Attr_Last:
    case Attr_Range_Length:
    case Attr_Length:
    case Attr_Object_Size:
    case Attr_Value_Size:
    case Attr_Component_Size:
    case Attr_Descriptor_Size:
    case Attr_Max_Size_In_Storage_Elements:
    case Attr_Min:
    case Attr_Max:
    case Attr_Null_Parameter:
    case Attr_Passed_By_Reference:
    case Attr_Mechanism_Code:
    case Attr_Machine:
    case Attr_Model:
      return 0;

    case Attr_Address:
    case Attr_Access:
    case Attr_Unchecked_Access:
    case Attr_Unrestricted_Access:
    case Attr_Code_Address:
    case Attr_Pool_Address:
    case Attr_Size:
    case Attr_Alignment:
    case Attr_Bit_Position:
    case Attr_Position:
    case Attr_First_Bit:
    case Attr_Last_Bit:
    case Attr_Bit:
    case Attr_Asm_Input:
    case Attr_Asm_Output:
    default:
      return 1;
    }
}

/* Return a positive value if an lvalue is required for GNAT_NODE.  GNU_TYPE
   is the type that will be used for GNAT_NODE in the translated GNU tree.
   CONSTANT indicates whether the underlying object represented by GNAT_NODE
   is constant in the Ada sense.  If it is, ADDRESS_OF_CONSTANT indicates
   whether its value is the address of a constant and ALIASED whether it is
   aliased.  If it isn't, ADDRESS_OF_CONSTANT and ALIASED are ignored.

   The function climbs up the GNAT tree starting from the node and returns 1
   upon encountering a node that effectively requires an lvalue downstream.
   It returns int instead of bool to facilitate usage in non-purely binary
   logic contexts.  */

static int
lvalue_required_p (Node_Id gnat_node, tree gnu_type, bool constant,
		   bool address_of_constant, bool aliased)
{
  Node_Id gnat_parent = Parent (gnat_node), gnat_temp;

  switch (Nkind (gnat_parent))
    {
    case N_Reference:
      return 1;

    case N_Attribute_Reference:
      return lvalue_required_for_attribute_p (gnat_parent);

    case N_Parameter_Association:
    case N_Function_Call:
    case N_Procedure_Call_Statement:
      /* If the parameter is by reference, an lvalue is required.  */
      return (!constant
	      || must_pass_by_ref (gnu_type)
	      || default_pass_by_ref (gnu_type));

    case N_Indexed_Component:
      /* Only the array expression can require an lvalue.  */
      if (Prefix (gnat_parent) != gnat_node)
	return 0;

      /* ??? Consider that referencing an indexed component with a variable
	 index forces the whole aggregate to memory.  Note that testing only
	 for literals is conservative, any static expression in the RM sense
	 could probably be accepted with some additional work.  */
      for (gnat_temp = First (Expressions (gnat_parent));
	   Present (gnat_temp);
	   gnat_temp = Next (gnat_temp))
	if (Nkind (gnat_temp) != N_Character_Literal
	    && Nkind (gnat_temp) != N_Integer_Literal
	    && !(Is_Entity_Name (gnat_temp)
		 && Ekind (Entity (gnat_temp)) == E_Enumeration_Literal))
	  return 1;

      /* ... fall through ... */

    case N_Slice:
      /* Only the array expression can require an lvalue.  */
      if (Prefix (gnat_parent) != gnat_node)
	return 0;

      aliased |= Has_Aliased_Components (Etype (gnat_node));
      return lvalue_required_p (gnat_parent, gnu_type, constant,
				address_of_constant, aliased);

    case N_Selected_Component:
      aliased |= Is_Aliased (Entity (Selector_Name (gnat_parent)));
      return lvalue_required_p (gnat_parent, gnu_type, constant,
				address_of_constant, aliased);

    case N_Object_Renaming_Declaration:
      /* We need to preserve addresses through a renaming.  */
      return 1;

    case N_Object_Declaration:
      /* We cannot use a constructor if this is an atomic object because
	 the actual assignment might end up being done component-wise.  */
      return (!constant
	      ||(Is_Composite_Type (Underlying_Type (Etype (gnat_node)))
		 && Is_Atomic_Or_VFA (Defining_Entity (gnat_parent)))
	      /* We don't use a constructor if this is a class-wide object
		 because the effective type of the object is the equivalent
		 type of the class-wide subtype and it smashes most of the
		 data into an array of bytes to which we cannot convert.  */
	      || Ekind ((Etype (Defining_Entity (gnat_parent))))
		 == E_Class_Wide_Subtype);

    case N_Assignment_Statement:
      /* We cannot use a constructor if the LHS is an atomic object because
	 the actual assignment might end up being done component-wise.  */
      return (!constant
	      || Name (gnat_parent) == gnat_node
	      || (Is_Composite_Type (Underlying_Type (Etype (gnat_node)))
		  && Is_Entity_Name (Name (gnat_parent))
		  && Is_Atomic_Or_VFA (Entity (Name (gnat_parent)))));

    case N_Unchecked_Type_Conversion:
	if (!constant)
	  return 1;

      /* ... fall through ... */

    case N_Type_Conversion:
    case N_Qualified_Expression:
      /* We must look through all conversions because we may need to bypass
	 an intermediate conversion that is meant to be purely formal.  */
     return lvalue_required_p (gnat_parent,
			       get_unpadded_type (Etype (gnat_parent)),
			       constant, address_of_constant, aliased);

    case N_Allocator:
      /* We should only reach here through the N_Qualified_Expression case.
	 Force an lvalue for composite types since a block-copy to the newly
	 allocated area of memory is made.  */
      return Is_Composite_Type (Underlying_Type (Etype (gnat_node)));

   case N_Explicit_Dereference:
      /* We look through dereferences for address of constant because we need
	 to handle the special cases listed above.  */
      if (constant && address_of_constant)
	return lvalue_required_p (gnat_parent,
				  get_unpadded_type (Etype (gnat_parent)),
				  true, false, true);

      /* ... fall through ... */

    default:
      return 0;
    }

  gcc_unreachable ();
}

/* Return true if T is a constant DECL node that can be safely replaced
   by its initializer.  */

static bool
constant_decl_with_initializer_p (tree t)
{
  if (!TREE_CONSTANT (t) || !DECL_P (t) || !DECL_INITIAL (t))
    return false;

  /* Return false for aggregate types that contain a placeholder since
     their initializers cannot be manipulated easily.  */
  if (AGGREGATE_TYPE_P (TREE_TYPE (t))
      && !TYPE_IS_FAT_POINTER_P (TREE_TYPE (t))
      && type_contains_placeholder_p (TREE_TYPE (t)))
    return false;

  return true;
}

/* Return an expression equivalent to EXP but where constant DECL nodes
   have been replaced by their initializer.  */

static tree
fold_constant_decl_in_expr (tree exp)
{
  enum tree_code code = TREE_CODE (exp);
  tree op0;

  switch (code)
    {
    case CONST_DECL:
    case VAR_DECL:
      if (!constant_decl_with_initializer_p (exp))
	return exp;

      return DECL_INITIAL (exp);

    case COMPONENT_REF:
      op0 = fold_constant_decl_in_expr (TREE_OPERAND (exp, 0));
      if (op0 == TREE_OPERAND (exp, 0))
	return exp;

      return fold_build3 (COMPONENT_REF, TREE_TYPE (exp), op0,
			  TREE_OPERAND (exp, 1), NULL_TREE);

    case BIT_FIELD_REF:
      op0 = fold_constant_decl_in_expr (TREE_OPERAND (exp, 0));
      if (op0 == TREE_OPERAND (exp, 0))
	return exp;

      return fold_build3 (BIT_FIELD_REF, TREE_TYPE (exp), op0,
			  TREE_OPERAND (exp, 1), TREE_OPERAND (exp, 2));

    case ARRAY_REF:
    case ARRAY_RANGE_REF:
      /* If the index is not itself constant, then nothing can be folded.  */
      if (!TREE_CONSTANT (TREE_OPERAND (exp, 1)))
	return exp;
      op0 = fold_constant_decl_in_expr (TREE_OPERAND (exp, 0));
      if (op0 == TREE_OPERAND (exp, 0))
	return exp;

      return fold (build4 (code, TREE_TYPE (exp), op0, TREE_OPERAND (exp, 1),
			   TREE_OPERAND (exp, 2), NULL_TREE));

    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case VIEW_CONVERT_EXPR:
      op0 = fold_constant_decl_in_expr (TREE_OPERAND (exp, 0));
      if (op0 == TREE_OPERAND (exp, 0))
	return exp;

      return fold_build1 (code, TREE_TYPE (exp), op0);

    default:
      return exp;
    }

  gcc_unreachable ();
}

/* Subroutine of gnat_to_gnu to translate gnat_node, an N_Identifier,
   to a GCC tree, which is returned.  GNU_RESULT_TYPE_P is a pointer
   to where we should place the result type.  */

static tree
Identifier_to_gnu (Node_Id gnat_node, tree *gnu_result_type_p)
{
  Node_Id gnat_temp, gnat_temp_type;
  tree gnu_result, gnu_result_type;

  /* Whether we should require an lvalue for GNAT_NODE.  Needed in
     specific circumstances only, so evaluated lazily.  < 0 means
     unknown, > 0 means known true, 0 means known false.  */
  int require_lvalue = -1;

  /* If GNAT_NODE is a constant, whether we should use the initialization
     value instead of the constant entity, typically for scalars with an
     address clause when the parent doesn't require an lvalue.  */
  bool use_constant_initializer = false;

  /* If the Etype of this node is not the same as that of the Entity, then
     something went wrong, probably in generic instantiation.  However, this
     does not apply to types.  Since we sometime have strange Ekind's, just
     do this test for objects.  Moreover, if the Etype of the Entity is private
     or incomplete coming from a limited context, the Etype of the N_Identifier
     is allowed to be the full/non-limited view and we also consider a packed
     array type to be the same as the original type.  Similarly, a CW type is
     equivalent to a subtype of itself.  Finally, if the types are Itypes, one
     may be a copy of the other, which is also legal.  */
  gnat_temp = ((Nkind (gnat_node) == N_Defining_Identifier
		|| Nkind (gnat_node) == N_Defining_Operator_Symbol)
	       ? gnat_node : Entity (gnat_node));
  gnat_temp_type = Etype (gnat_temp);

  gcc_assert (Etype (gnat_node) == gnat_temp_type
	      || (Is_Packed (gnat_temp_type)
		  && (Etype (gnat_node)
		      == Packed_Array_Impl_Type (gnat_temp_type)))
	      || (Is_Class_Wide_Type (Etype (gnat_node)))
	      || (IN (Ekind (gnat_temp_type), Private_Kind)
		  && Present (Full_View (gnat_temp_type))
		  && ((Etype (gnat_node) == Full_View (gnat_temp_type))
		      || (Is_Packed (Full_View (gnat_temp_type))
			  && (Etype (gnat_node)
			      == Packed_Array_Impl_Type
			           (Full_View (gnat_temp_type))))))
	      || (IN (Ekind (gnat_temp_type), Incomplete_Kind)
		  && From_Limited_With (gnat_temp_type)
		  && Present (Non_Limited_View (gnat_temp_type))
		  && Etype (gnat_node) == Non_Limited_View (gnat_temp_type))
	      || (Is_Itype (Etype (gnat_node)) && Is_Itype (gnat_temp_type))
	      || !(Ekind (gnat_temp) == E_Variable
		   || Ekind (gnat_temp) == E_Component
		   || Ekind (gnat_temp) == E_Constant
		   || Ekind (gnat_temp) == E_Loop_Parameter
		   || IN (Ekind (gnat_temp), Formal_Kind)));

  /* If this is a reference to a deferred constant whose partial view is an
     unconstrained private type, the proper type is on the full view of the
     constant, not on the full view of the type, which may be unconstrained.

     This may be a reference to a type, for example in the prefix of the
     attribute Position, generated for dispatching code (see Make_DT in
     exp_disp,adb). In that case we need the type itself, not is parent,
     in particular if it is a derived type  */
  if (Ekind (gnat_temp) == E_Constant
      && Is_Private_Type (gnat_temp_type)
      && (Has_Unknown_Discriminants (gnat_temp_type)
	  || (Present (Full_View (gnat_temp_type))
 	      && Has_Discriminants (Full_View (gnat_temp_type))))
      && Present (Full_View (gnat_temp)))
    {
      gnat_temp = Full_View (gnat_temp);
      gnat_temp_type = Etype (gnat_temp);
    }
  else
    {
      /* We want to use the Actual_Subtype if it has already been elaborated,
	 otherwise the Etype.  Avoid using Actual_Subtype for packed arrays to
	 simplify things.  */
      if ((Ekind (gnat_temp) == E_Constant
	   || Ekind (gnat_temp) == E_Variable || Is_Formal (gnat_temp))
	  && !(Is_Array_Type (Etype (gnat_temp))
	       && Present (Packed_Array_Impl_Type (Etype (gnat_temp))))
	  && Present (Actual_Subtype (gnat_temp))
	  && present_gnu_tree (Actual_Subtype (gnat_temp)))
	gnat_temp_type = Actual_Subtype (gnat_temp);
      else
	gnat_temp_type = Etype (gnat_node);
    }

  /* Expand the type of this identifier first, in case it is an enumeral
     literal, which only get made when the type is expanded.  There is no
     order-of-elaboration issue here.  */
  gnu_result_type = get_unpadded_type (gnat_temp_type);

  /* If this is a non-imported elementary constant with an address clause,
     retrieve the value instead of a pointer to be dereferenced unless
     an lvalue is required.  This is generally more efficient and actually
     required if this is a static expression because it might be used
     in a context where a dereference is inappropriate, such as a case
     statement alternative or a record discriminant.  There is no possible
     volatile-ness short-circuit here since Volatile constants must be
     imported per C.6.  */
  if (Ekind (gnat_temp) == E_Constant
      && Is_Elementary_Type (gnat_temp_type)
      && !Is_Imported (gnat_temp)
      && Present (Address_Clause (gnat_temp)))
    {
      require_lvalue = lvalue_required_p (gnat_node, gnu_result_type, true,
					  false, Is_Aliased (gnat_temp));
      use_constant_initializer = !require_lvalue;
    }

  if (use_constant_initializer)
    {
      /* If this is a deferred constant, the initializer is attached to
	 the full view.  */
      if (Present (Full_View (gnat_temp)))
	gnat_temp = Full_View (gnat_temp);

      gnu_result = gnat_to_gnu (Expression (Declaration_Node (gnat_temp)));
    }
  else
    gnu_result = gnat_to_gnu_entity (gnat_temp, NULL_TREE, false);

  /* Some objects (such as parameters passed by reference, globals of
     variable size, and renamed objects) actually represent the address
     of the object.  In that case, we must do the dereference.  Likewise,
     deal with parameters to foreign convention subprograms.  */
  if (DECL_P (gnu_result)
      && (DECL_BY_REF_P (gnu_result)
	  || (TREE_CODE (gnu_result) == PARM_DECL
	      && DECL_BY_COMPONENT_PTR_P (gnu_result))))
    {
      const bool read_only = DECL_POINTS_TO_READONLY_P (gnu_result);

      /* If it's a PARM_DECL to foreign convention subprogram, convert it.  */
      if (TREE_CODE (gnu_result) == PARM_DECL
	  && DECL_BY_COMPONENT_PTR_P (gnu_result))
	gnu_result
	  = convert (build_pointer_type (gnu_result_type), gnu_result);

      /* If it's a CONST_DECL, return the underlying constant like below.  */
      else if (TREE_CODE (gnu_result) == CONST_DECL
	       && !(DECL_CONST_ADDRESS_P (gnu_result)
		    && lvalue_required_p (gnat_node, gnu_result_type, true,
					  true, false)))
	gnu_result = DECL_INITIAL (gnu_result);

      /* If it's a renaming pointer, get to the renamed object.  */
      if (TREE_CODE (gnu_result) == VAR_DECL
          && !DECL_LOOP_PARM_P (gnu_result)
	  && DECL_RENAMED_OBJECT (gnu_result))
	gnu_result = DECL_RENAMED_OBJECT (gnu_result);

      /* Otherwise, do the final dereference.  */
      else
	{
	  gnu_result = build_unary_op (INDIRECT_REF, NULL_TREE, gnu_result);

	  if ((TREE_CODE (gnu_result) == INDIRECT_REF
	       || TREE_CODE (gnu_result) == UNCONSTRAINED_ARRAY_REF)
	      && No (Address_Clause (gnat_temp)))
	    TREE_THIS_NOTRAP (gnu_result) = 1;

	  if (read_only)
	    TREE_READONLY (gnu_result) = 1;
	}
    }

  /* If we have a constant declaration and its initializer, try to return the
     latter to avoid the need to call fold in lots of places and the need for
     elaboration code if this identifier is used as an initializer itself.  */
  if (constant_decl_with_initializer_p (gnu_result))
    {
      bool constant_only = (TREE_CODE (gnu_result) == CONST_DECL
			    && !DECL_CONST_CORRESPONDING_VAR (gnu_result));
      bool address_of_constant = (TREE_CODE (gnu_result) == CONST_DECL
				  && DECL_CONST_ADDRESS_P (gnu_result));

      /* If there is a (corresponding) variable or this is the address of a
	 constant, we only want to return the initializer if an lvalue isn't
	 required.  Evaluate this now if we have not already done so.  */
      if ((!constant_only || address_of_constant) && require_lvalue < 0)
	require_lvalue
	  = lvalue_required_p (gnat_node, gnu_result_type, true,
			       address_of_constant, Is_Aliased (gnat_temp));

      /* Finally retrieve the initializer if this is deemed valid.  */
      if ((constant_only && !address_of_constant) || !require_lvalue)
	gnu_result = DECL_INITIAL (gnu_result);
    }

  /* But for a constant renaming we couldn't do that incrementally for its
     definition because of the need to return an lvalue so, if the present
     context doesn't itself require an lvalue, we try again here.  */
  else if (Ekind (gnat_temp) == E_Constant
	   && Is_Elementary_Type (gnat_temp_type)
	   && Present (Renamed_Object (gnat_temp)))
    {
      if (require_lvalue < 0)
	require_lvalue
	  = lvalue_required_p (gnat_node, gnu_result_type, true, false,
			       Is_Aliased (gnat_temp));
      if (!require_lvalue)
	gnu_result = fold_constant_decl_in_expr (gnu_result);
    }

  /* The GNAT tree has the type of a function set to its result type, so we
     adjust here.  Also use the type of the result if the Etype is a subtype
     that is nominally unconstrained.  Likewise if this is a deferred constant
     of a discriminated type whose full view can be elaborated statically, to
     avoid problematic conversions to the nominal subtype.  But remove any
     padding from the resulting type.  */
  if (TREE_CODE (TREE_TYPE (gnu_result)) == FUNCTION_TYPE
      || Is_Constr_Subt_For_UN_Aliased (gnat_temp_type)
      || (Ekind (gnat_temp) == E_Constant
	  && Present (Full_View (gnat_temp))
	  && Has_Discriminants (gnat_temp_type)
	  && TREE_CODE (gnu_result) == CONSTRUCTOR))
    {
      gnu_result_type = TREE_TYPE (gnu_result);
      if (TYPE_IS_PADDING_P (gnu_result_type))
	gnu_result_type = TREE_TYPE (TYPE_FIELDS (gnu_result_type));
    }

  *gnu_result_type_p = gnu_result_type;

  return gnu_result;
}

/* Subroutine of gnat_to_gnu to process gnat_node, an N_Pragma.  Return
   any statements we generate.  */

static tree
Pragma_to_gnu (Node_Id gnat_node)
{
  tree gnu_result = alloc_stmt_list ();
  unsigned char pragma_id;
  Node_Id gnat_temp;

  /* Do nothing if we are just annotating types and check for (and ignore)
     unrecognized pragmas.  */
  if (type_annotate_only
      || !Is_Pragma_Name (Chars (Pragma_Identifier (gnat_node))))
    return gnu_result;

  pragma_id = Get_Pragma_Id (Chars (Pragma_Identifier (gnat_node)));
  switch (pragma_id)
    {
    case Pragma_Inspection_Point:
      /* Do nothing at top level: all such variables are already viewable.  */
      if (global_bindings_p ())
	break;

      for (gnat_temp = First (Pragma_Argument_Associations (gnat_node));
	   Present (gnat_temp);
	   gnat_temp = Next (gnat_temp))
	{
	  Node_Id gnat_expr = Expression (gnat_temp);
	  tree gnu_expr = gnat_to_gnu (gnat_expr);
	  int use_address;
	  machine_mode mode;
	  tree asm_constraint = NULL_TREE;
#ifdef ASM_COMMENT_START
	  char *comment;
#endif

	  if (TREE_CODE (gnu_expr) == UNCONSTRAINED_ARRAY_REF)
	    gnu_expr = TREE_OPERAND (gnu_expr, 0);

	  /* Use the value only if it fits into a normal register,
	     otherwise use the address.  */
	  mode = TYPE_MODE (TREE_TYPE (gnu_expr));
	  use_address = ((GET_MODE_CLASS (mode) != MODE_INT
			  && GET_MODE_CLASS (mode) != MODE_PARTIAL_INT)
			 || GET_MODE_SIZE (mode) > UNITS_PER_WORD);

	  if (use_address)
	    gnu_expr = build_unary_op (ADDR_EXPR, NULL_TREE, gnu_expr);

#ifdef ASM_COMMENT_START
	  comment = concat (ASM_COMMENT_START,
			    " inspection point: ",
			    Get_Name_String (Chars (gnat_expr)),
			    use_address ? " address" : "",
			    " is in %0",
			    NULL);
	  asm_constraint = build_string (strlen (comment), comment);
	  free (comment);
#endif
	  gnu_expr = build5 (ASM_EXPR, void_type_node,
			     asm_constraint,
			     NULL_TREE,
			     tree_cons
			     (build_tree_list (NULL_TREE,
					       build_string (1, "g")),
			      gnu_expr, NULL_TREE),
			     NULL_TREE, NULL_TREE);
	  ASM_VOLATILE_P (gnu_expr) = 1;
	  set_expr_location_from_node (gnu_expr, gnat_node);
	  append_to_statement_list (gnu_expr, &gnu_result);
	}
      break;

    case Pragma_Loop_Optimize:
      for (gnat_temp = First (Pragma_Argument_Associations (gnat_node));
	   Present (gnat_temp);
	   gnat_temp = Next (gnat_temp))
	{
	  tree gnu_loop_stmt = gnu_loop_stack->last ()->stmt;

	  switch (Chars (Expression (gnat_temp)))
	    {
	    case Name_Ivdep:
	      LOOP_STMT_IVDEP (gnu_loop_stmt) = 1;
	      break;

	    case Name_No_Unroll:
	      LOOP_STMT_NO_UNROLL (gnu_loop_stmt) = 1;
	      break;

	    case Name_Unroll:
	      LOOP_STMT_UNROLL (gnu_loop_stmt) = 1;
	      break;

	    case Name_No_Vector:
	      LOOP_STMT_NO_VECTOR (gnu_loop_stmt) = 1;
	      break;

	    case Name_Vector:
	      LOOP_STMT_VECTOR (gnu_loop_stmt) = 1;
	      break;

	    default:
	      gcc_unreachable ();
	    }
	}
      break;

    case Pragma_Optimize:
      switch (Chars (Expression
		     (First (Pragma_Argument_Associations (gnat_node)))))
	{
	case Name_Off:
	  if (optimize)
	    post_error ("must specify -O0?", gnat_node);
	  break;

	case Name_Space:
	  if (!optimize_size)
	    post_error ("must specify -Os?", gnat_node);
	  break;

	case Name_Time:
	  if (!optimize)
	    post_error ("insufficient -O value?", gnat_node);
	  break;

	default:
	  gcc_unreachable ();
	}
      break;

    case Pragma_Reviewable:
      if (write_symbols == NO_DEBUG)
	post_error ("must specify -g?", gnat_node);
      break;

    case Pragma_Warning_As_Error:
    case Pragma_Warnings:
      {
	Node_Id gnat_expr;
	/* Preserve the location of the pragma.  */
	const location_t location = input_location;
	struct cl_option_handlers handlers;
	unsigned int option_index;
	diagnostic_t kind;
	bool imply;

	gnat_temp = First (Pragma_Argument_Associations (gnat_node));

	/* This is the String form: pragma Warning{s|_As_Error}(String).  */
	if (Nkind (Expression (gnat_temp)) == N_String_Literal)
	  {
	    switch (pragma_id)
	      {
	      case Pragma_Warning_As_Error:
		kind = DK_ERROR;
		imply = false;
		break;

	      case Pragma_Warnings:
		kind = DK_WARNING;
		imply = true;
		break;

	      default:
		gcc_unreachable ();
	      }

	    gnat_expr = Expression (gnat_temp);
	  }

	/* This is the On/Off form: pragma Warnings (On | Off [,String]).  */
	else if (Nkind (Expression (gnat_temp)) == N_Identifier)
	  {
	    switch (Chars (Expression (gnat_temp)))
	      {
		case Name_Off:
		  kind = DK_IGNORED;
		  break;

		case Name_On:
		  kind = DK_WARNING;
		  break;

		default:
		  gcc_unreachable ();
	      }

	    /* Deal with optional pattern (but ignore Reason => "...").  */
	    if (Present (Next (gnat_temp))
		&& Chars (Next (gnat_temp)) != Name_Reason)
	      {
		/* pragma Warnings (On | Off, Name) is handled differently.  */
		if (Nkind (Expression (Next (gnat_temp))) != N_String_Literal)
		  break;

	        gnat_expr = Expression (Next (gnat_temp));
	      }
	    else
	      gnat_expr = Empty;

	    imply = false;
	  }

	else
	  gcc_unreachable ();

	/* This is the same implementation as in the C family of compilers.  */
	const unsigned int lang_mask = CL_Ada | CL_COMMON;
	const char *arg = NULL;
	if (Present (gnat_expr))
	  {
	    tree gnu_expr = gnat_to_gnu (gnat_expr);
	    const char *option_string = TREE_STRING_POINTER (gnu_expr);
	    const int len = TREE_STRING_LENGTH (gnu_expr);
	    if (len < 3 || option_string[0] != '-' || option_string[1] != 'W')
	      break;
	    option_index = find_opt (option_string + 1, lang_mask);
	    if (option_index == OPT_SPECIAL_unknown)
	      {
		post_error ("?unknown -W switch", gnat_node);
		break;
	      }
	    else if (!(cl_options[option_index].flags & CL_WARNING))
	      {
		post_error ("?-W switch does not control warning", gnat_node);
		break;
	      }
	    else if (!(cl_options[option_index].flags & lang_mask))
	      {
		post_error ("?-W switch not valid for Ada", gnat_node);
		break;
	      }
	    if (cl_options[option_index].flags & CL_JOINED)
	      arg = option_string + 1 + cl_options[option_index].opt_len;
	  }
	else
	  option_index = 0;

	set_default_handlers (&handlers);
	control_warning_option (option_index, (int) kind, arg, imply, location,
				lang_mask, &handlers, &global_options,
				&global_options_set, global_dc);
      }
      break;

    default:
      break;
    }

  return gnu_result;
}


/* Check the inline status of nested function FNDECL wrt its parent function.

   If a non-inline nested function is referenced from an inline external
   function, we cannot honor both requests at the same time without cloning
   the nested function in the current unit since it is private to its unit.
   We could inline it as well but it's probably better to err on the side
   of too little inlining.

   This must be done only on nested functions present in the source code
   and not on nested functions generated by the compiler, e.g. finalizers,
   because they may be not marked inline and we don't want them to block
   the inlining of the parent function.  */

static void
check_inlining_for_nested_subprog (tree fndecl)
{
  if (DECL_IGNORED_P (current_function_decl) || DECL_IGNORED_P (fndecl))
    return;

  if (DECL_DECLARED_INLINE_P (fndecl))
    return;

  tree parent_decl = decl_function_context (fndecl);
  if (DECL_EXTERNAL (parent_decl) && DECL_DECLARED_INLINE_P (parent_decl))
    {
      const location_t loc1 = DECL_SOURCE_LOCATION (fndecl);
      const location_t loc2 = DECL_SOURCE_LOCATION (parent_decl);

      if (lookup_attribute ("always_inline", DECL_ATTRIBUTES (parent_decl)))
	{
	  error_at (loc1, "subprogram %q+F not marked Inline_Always", fndecl);
	  error_at (loc2, "parent subprogram cannot be inlined");
	}
      else
	{
	  warning_at (loc1, OPT_Winline, "subprogram %q+F not marked Inline",
		      fndecl);
	  warning_at (loc2, OPT_Winline, "parent subprogram cannot be inlined");
	}

      DECL_DECLARED_INLINE_P (parent_decl) = 0;
      DECL_UNINLINABLE (parent_decl) = 1;
    }
}

/* Return an expression for the length of TYPE, an integral type, computed in
   RESULT_TYPE, another integral type.

   We used to compute the length as MAX (hb - lb + 1, 0) which could overflow
   when lb == TYPE'First.  We now compute it as (hb >= lb) ? hb - lb + 1 : 0
   which would only overflow in much rarer cases, for extremely large arrays
   we expect never to encounter in practice.  Besides, the former computation
   required the use of potentially constraining signed arithmetics while the
   latter does not.  Note that the comparison must be done in the original
   base index type in order to avoid any overflow during the conversion.  */

static tree
get_type_length (tree type, tree result_type)
{
  tree comp_type = get_base_type (result_type);
  tree base_type = maybe_character_type (get_base_type (type));
  tree lb = convert (base_type, TYPE_MIN_VALUE (type));
  tree hb = convert (base_type, TYPE_MAX_VALUE (type));
  tree length
    = build_binary_op (PLUS_EXPR, comp_type,
		       build_binary_op (MINUS_EXPR, comp_type,
					convert (comp_type, hb),
					convert (comp_type, lb)),
		       build_int_cst (comp_type, 1));
  length
    = build_cond_expr (result_type,
		       build_binary_op (GE_EXPR, boolean_type_node, hb, lb),
		       convert (result_type, length),
		       build_int_cst (result_type, 0));
  return length;
}

/* Subroutine of gnat_to_gnu to translate GNAT_NODE, an N_Attribute node,
   to a GCC tree, which is returned.  GNU_RESULT_TYPE_P is a pointer to
   where we should place the result type.  ATTRIBUTE is the attribute ID.  */

static tree
Attribute_to_gnu (Node_Id gnat_node, tree *gnu_result_type_p, int attribute)
{
  const Node_Id gnat_prefix = Prefix (gnat_node);
  tree gnu_prefix = gnat_to_gnu (gnat_prefix);
  tree gnu_type = TREE_TYPE (gnu_prefix);
  tree gnu_expr, gnu_result_type, gnu_result = error_mark_node;
  bool prefix_unused = false;

  /* If the input is a NULL_EXPR, make a new one.  */
  if (TREE_CODE (gnu_prefix) == NULL_EXPR)
    {
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      *gnu_result_type_p = gnu_result_type;
      return build1 (NULL_EXPR, gnu_result_type, TREE_OPERAND (gnu_prefix, 0));
    }

  switch (attribute)
    {
    case Attr_Pos:
    case Attr_Val:
      /* These are just conversions since representation clauses for
	 enumeration types are handled in the front-end.  */
      gnu_expr = gnat_to_gnu (First (Expressions (gnat_node)));
      if (attribute == Attr_Pos)
	gnu_expr = maybe_character_value (gnu_expr);
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      gnu_result = convert (gnu_result_type, gnu_expr);
      break;

    case Attr_Pred:
    case Attr_Succ:
      /* These just add or subtract the constant 1 since representation
	 clauses for enumeration types are handled in the front-end.  */
      gnu_expr = gnat_to_gnu (First (Expressions (gnat_node)));
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      gnu_type = maybe_character_type (gnu_result_type);
      if (TREE_TYPE (gnu_expr) != gnu_type)
	gnu_expr = convert (gnu_type, gnu_expr);
      gnu_result
	= build_binary_op (attribute == Attr_Pred ? MINUS_EXPR : PLUS_EXPR,
			   gnu_type, gnu_expr, build_int_cst (gnu_type, 1));
      break;

    case Attr_Address:
    case Attr_Unrestricted_Access:
      /* Conversions don't change addresses but can cause us to miss the
	 COMPONENT_REF case below, so strip them off.  */
      gnu_prefix = remove_conversions (gnu_prefix,
				       !Must_Be_Byte_Aligned (gnat_node));

      /* If we are taking 'Address of an unconstrained object, this is the
	 pointer to the underlying array.  */
      if (attribute == Attr_Address)
	gnu_prefix = maybe_unconstrained_array (gnu_prefix);

      /* If we are building a static dispatch table, we have to honor
	 TARGET_VTABLE_USES_DESCRIPTORS if we want to be compatible
	 with the C++ ABI.  We do it in the non-static case as well,
	 see gnat_to_gnu_entity, case E_Access_Subprogram_Type.  */
      else if (TARGET_VTABLE_USES_DESCRIPTORS
	       && Is_Dispatch_Table_Entity (Etype (gnat_node)))
	{
	  tree gnu_field, t;
	  /* Descriptors can only be built here for top-level functions.  */
	  bool build_descriptor = (global_bindings_p () != 0);
	  int i;
	  vec<constructor_elt, va_gc> *gnu_vec = NULL;
	  constructor_elt *elt;

	  gnu_result_type = get_unpadded_type (Etype (gnat_node));

	  /* If we're not going to build the descriptor, we have to retrieve
	     the one which will be built by the linker (or by the compiler
	     later if a static chain is requested).  */
	  if (!build_descriptor)
	    {
	      gnu_result = build_unary_op (ADDR_EXPR, NULL_TREE, gnu_prefix);
	      gnu_result = fold_convert (build_pointer_type (gnu_result_type),
					 gnu_result);
	      gnu_result = build1 (INDIRECT_REF, gnu_result_type, gnu_result);
	    }

	  vec_safe_grow (gnu_vec, TARGET_VTABLE_USES_DESCRIPTORS);
	  elt = (gnu_vec->address () + TARGET_VTABLE_USES_DESCRIPTORS - 1);
	  for (gnu_field = TYPE_FIELDS (gnu_result_type), i = 0;
	       i < TARGET_VTABLE_USES_DESCRIPTORS;
	       gnu_field = DECL_CHAIN (gnu_field), i++)
	    {
	      if (build_descriptor)
		{
		  t = build2 (FDESC_EXPR, TREE_TYPE (gnu_field), gnu_prefix,
			      build_int_cst (NULL_TREE, i));
		  TREE_CONSTANT (t) = 1;
		}
	      else
		t = build3 (COMPONENT_REF, ptr_void_ftype, gnu_result,
			    gnu_field, NULL_TREE);

	      elt->index = gnu_field;
	      elt->value = t;
	      elt--;
	    }

	  gnu_result = gnat_build_constructor (gnu_result_type, gnu_vec);
	  break;
	}

      /* ... fall through ... */

    case Attr_Access:
    case Attr_Unchecked_Access:
    case Attr_Code_Address:
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      gnu_result
	= build_unary_op (((attribute == Attr_Address
			    || attribute == Attr_Unrestricted_Access)
			   && !Must_Be_Byte_Aligned (gnat_node))
			  ? ATTR_ADDR_EXPR : ADDR_EXPR,
			  gnu_result_type, gnu_prefix);

      /* For 'Code_Address, find an inner ADDR_EXPR and mark it so that we
	 don't try to build a trampoline.  */
      if (attribute == Attr_Code_Address)
	{
	  gnu_expr = remove_conversions (gnu_result, false);

	  if (TREE_CODE (gnu_expr) == ADDR_EXPR)
	    TREE_NO_TRAMPOLINE (gnu_expr) = TREE_CONSTANT (gnu_expr) = 1;
	}

      /* For 'Access, issue an error message if the prefix is a C++ method
	 since it can use a special calling convention on some platforms,
	 which cannot be propagated to the access type.  */
      else if (attribute == Attr_Access
	       && Nkind (gnat_prefix) == N_Identifier
	       && is_cplusplus_method (Entity (gnat_prefix)))
	post_error ("access to C++ constructor or member function not allowed",
		    gnat_node);

      /* For other address attributes applied to a nested function,
	 find an inner ADDR_EXPR and annotate it so that we can issue
	 a useful warning with -Wtrampolines.  */
      else if (TREE_CODE (TREE_TYPE (gnu_prefix)) == FUNCTION_TYPE)
	{
	  gnu_expr = remove_conversions (gnu_result, false);

	  if (TREE_CODE (gnu_expr) == ADDR_EXPR
	      && decl_function_context (TREE_OPERAND (gnu_expr, 0)))
	    {
	      set_expr_location_from_node (gnu_expr, gnat_node);

	      /* Also check the inlining status.  */
	      check_inlining_for_nested_subprog (TREE_OPERAND (gnu_expr, 0));

	      /* Check that we're not violating the No_Implicit_Dynamic_Code
		 restriction.  Be conservative if we don't know anything
		 about the trampoline strategy for the target.  */
	      Check_Implicit_Dynamic_Code_Allowed (gnat_node);
	    }
	}
      break;

    case Attr_Pool_Address:
      {
	tree gnu_ptr = gnu_prefix;
	tree gnu_obj_type;

	gnu_result_type = get_unpadded_type (Etype (gnat_node));

	/* If this is fat pointer, the object must have been allocated with the
	   template in front of the array.  So compute the template address; do
	   it by converting to a thin pointer.  */
	if (TYPE_IS_FAT_POINTER_P (TREE_TYPE (gnu_ptr)))
	  gnu_ptr
	    = convert (build_pointer_type
		       (TYPE_OBJECT_RECORD_TYPE
			(TYPE_UNCONSTRAINED_ARRAY (TREE_TYPE (gnu_ptr)))),
		       gnu_ptr);

	gnu_obj_type = TREE_TYPE (TREE_TYPE (gnu_ptr));

	/* If this is a thin pointer, the object must have been allocated with
	   the template in front of the array.  So compute the template address
	   and return it.  */
	if (TYPE_IS_THIN_POINTER_P (TREE_TYPE (gnu_ptr)))
	  gnu_ptr
	    = build_binary_op (POINTER_PLUS_EXPR, TREE_TYPE (gnu_ptr),
			       gnu_ptr,
			       fold_build1 (NEGATE_EXPR, sizetype,
					    byte_position
					    (DECL_CHAIN
					     TYPE_FIELDS ((gnu_obj_type)))));

	gnu_result = convert (gnu_result_type, gnu_ptr);
      }
      break;

    case Attr_Size:
    case Attr_Object_Size:
    case Attr_Value_Size:
    case Attr_Max_Size_In_Storage_Elements:
      gnu_expr = gnu_prefix;

      /* Remove NOPs and conversions between original and packable version
	 from GNU_EXPR, and conversions from GNU_PREFIX.  We use GNU_EXPR
	 to see if a COMPONENT_REF was involved.  */
      while (TREE_CODE (gnu_expr) == NOP_EXPR
	     || (TREE_CODE (gnu_expr) == VIEW_CONVERT_EXPR
		 && TREE_CODE (TREE_TYPE (gnu_expr)) == RECORD_TYPE
		 && TREE_CODE (TREE_TYPE (TREE_OPERAND (gnu_expr, 0)))
		    == RECORD_TYPE
		 && TYPE_NAME (TREE_TYPE (gnu_expr))
		    == TYPE_NAME (TREE_TYPE (TREE_OPERAND (gnu_expr, 0)))))
	gnu_expr = TREE_OPERAND (gnu_expr, 0);

      gnu_prefix = remove_conversions (gnu_prefix, true);
      prefix_unused = true;
      gnu_type = TREE_TYPE (gnu_prefix);

      /* Replace an unconstrained array type with the type of the underlying
	 array.  We can't do this with a call to maybe_unconstrained_array
	 since we may have a TYPE_DECL.  For 'Max_Size_In_Storage_Elements,
	 use the record type that will be used to allocate the object and its
	 template.  */
      if (TREE_CODE (gnu_type) == UNCONSTRAINED_ARRAY_TYPE)
	{
	  gnu_type = TYPE_OBJECT_RECORD_TYPE (gnu_type);
	  if (attribute != Attr_Max_Size_In_Storage_Elements)
	    gnu_type = TREE_TYPE (DECL_CHAIN (TYPE_FIELDS (gnu_type)));
	}

      /* If we're looking for the size of a field, return the field size.  */
      if (TREE_CODE (gnu_prefix) == COMPONENT_REF)
	gnu_result = DECL_SIZE (TREE_OPERAND (gnu_prefix, 1));

      /* Otherwise, if the prefix is an object, or if we are looking for
	 'Object_Size or 'Max_Size_In_Storage_Elements, the result is the
	 GCC size of the type.  We make an exception for padded objects,
	 as we do not take into account alignment promotions for the size.
	 This is in keeping with the object case of gnat_to_gnu_entity.  */
      else if ((TREE_CODE (gnu_prefix) != TYPE_DECL
		&& !(TYPE_IS_PADDING_P (gnu_type)
		     && TREE_CODE (gnu_expr) == COMPONENT_REF))
	       || attribute == Attr_Object_Size
	       || attribute == Attr_Max_Size_In_Storage_Elements)
	{
	  /* If this is a dereference and we have a special dynamic constrained
	     subtype on the prefix, use it to compute the size; otherwise, use
	     the designated subtype.  */
	  if (Nkind (gnat_prefix) == N_Explicit_Dereference)
	    {
	      Node_Id gnat_actual_subtype
		= Actual_Designated_Subtype (gnat_prefix);
	      tree gnu_ptr_type
		= TREE_TYPE (gnat_to_gnu (Prefix (gnat_prefix)));

	      if (TYPE_IS_FAT_OR_THIN_POINTER_P (gnu_ptr_type)
		  && Present (gnat_actual_subtype))
		{
		  tree gnu_actual_obj_type
		    = gnat_to_gnu_type (gnat_actual_subtype);
		  gnu_type
		    = build_unc_object_type_from_ptr (gnu_ptr_type,
						      gnu_actual_obj_type,
						      get_identifier ("SIZE"),
						      false);
		}
	    }

	  gnu_result = TYPE_SIZE (gnu_type);
	}

      /* Otherwise, the result is the RM size of the type.  */
      else
	gnu_result = rm_size (gnu_type);

      /* Deal with a self-referential size by returning the maximum size for
	 a type and by qualifying the size with the object otherwise.  */
      if (CONTAINS_PLACEHOLDER_P (gnu_result))
	{
	  if (TREE_CODE (gnu_prefix) == TYPE_DECL)
	    gnu_result = max_size (gnu_result, true);
	  else
	    gnu_result = substitute_placeholder_in_expr (gnu_result, gnu_expr);
	}

      /* If the type contains a template, subtract its size.  */
      if (TREE_CODE (gnu_type) == RECORD_TYPE
	  && TYPE_CONTAINS_TEMPLATE_P (gnu_type))
	gnu_result = size_binop (MINUS_EXPR, gnu_result,
				 DECL_SIZE (TYPE_FIELDS (gnu_type)));

      /* For 'Max_Size_In_Storage_Elements, adjust the unit.  */
      if (attribute == Attr_Max_Size_In_Storage_Elements)
	gnu_result = size_binop (CEIL_DIV_EXPR, gnu_result, bitsize_unit_node);

      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      break;

    case Attr_Alignment:
      {
	unsigned int align;

	if (TREE_CODE (gnu_prefix) == COMPONENT_REF
	    && TYPE_IS_PADDING_P (TREE_TYPE (TREE_OPERAND (gnu_prefix, 0))))
	  gnu_prefix = TREE_OPERAND (gnu_prefix, 0);

	gnu_type = TREE_TYPE (gnu_prefix);
	gnu_result_type = get_unpadded_type (Etype (gnat_node));
	prefix_unused = true;

	if (TREE_CODE (gnu_prefix) == COMPONENT_REF)
	  align = DECL_ALIGN (TREE_OPERAND (gnu_prefix, 1)) / BITS_PER_UNIT;
	else
	  {
	    Entity_Id gnat_type = Etype (gnat_prefix);
	    unsigned int double_align;
	    bool is_capped_double, align_clause;

	    /* If the default alignment of "double" or larger scalar types is
	       specifically capped and there is an alignment clause neither
	       on the type nor on the prefix itself, return the cap.  */
	    if ((double_align = double_float_alignment) > 0)
	      is_capped_double
		= is_double_float_or_array (gnat_type, &align_clause);
	    else if ((double_align = double_scalar_alignment) > 0)
	      is_capped_double
		= is_double_scalar_or_array (gnat_type, &align_clause);
	    else
	      is_capped_double = align_clause = false;

	    if (is_capped_double
		&& Nkind (gnat_prefix) == N_Identifier
		&& Present (Alignment_Clause (Entity (gnat_prefix))))
	      align_clause = true;

	    if (is_capped_double && !align_clause)
	      align = double_align;
	    else
	      align = TYPE_ALIGN (gnu_type) / BITS_PER_UNIT;
	  }

	gnu_result = size_int (align);
      }
      break;

    case Attr_First:
    case Attr_Last:
    case Attr_Range_Length:
      prefix_unused = true;

      if (INTEGRAL_TYPE_P (gnu_type) || TREE_CODE (gnu_type) == REAL_TYPE)
	{
	  gnu_result_type = get_unpadded_type (Etype (gnat_node));

	  if (attribute == Attr_First)
	    gnu_result = TYPE_MIN_VALUE (gnu_type);
	  else if (attribute == Attr_Last)
	    gnu_result = TYPE_MAX_VALUE (gnu_type);
	  else
	    gnu_result = get_type_length (gnu_type, gnu_result_type);
	  break;
	}

      /* ... fall through ... */

    case Attr_Length:
      {
	int Dimension = (Present (Expressions (gnat_node))
			 ? UI_To_Int (Intval (First (Expressions (gnat_node))))
			 : 1), i;
	struct parm_attr_d *pa = NULL;
	Entity_Id gnat_param = Empty;
	bool unconstrained_ptr_deref = false;

	/* Make sure any implicit dereference gets done.  */
	gnu_prefix = maybe_implicit_deref (gnu_prefix);
	gnu_prefix = maybe_unconstrained_array (gnu_prefix);

	/* We treat unconstrained array In parameters specially.  We also note
	   whether we are dereferencing a pointer to unconstrained array.  */
	if (!Is_Constrained (Etype (gnat_prefix)))
	  switch (Nkind (gnat_prefix))
	    {
	    case N_Identifier:
	      /* This is the direct case.  */
	      if (Ekind (Entity (gnat_prefix)) == E_In_Parameter)
		gnat_param = Entity (gnat_prefix);
	      break;

	    case N_Explicit_Dereference:
	      /* This is the indirect case.  Note that we need to be sure that
		 the access value cannot be null as we'll hoist the load.  */
	      if (Nkind (Prefix (gnat_prefix)) == N_Identifier
		  && Ekind (Entity (Prefix (gnat_prefix))) == E_In_Parameter)
		{
		  if (Can_Never_Be_Null (Entity (Prefix (gnat_prefix))))
		    gnat_param = Entity (Prefix (gnat_prefix));
		}
	      else
		unconstrained_ptr_deref = true;
	      break;

	    default:
	      break;
	  }

	/* If the prefix is the view conversion of a constrained array to an
	   unconstrained form, we retrieve the constrained array because we
	   might not be able to substitute the PLACEHOLDER_EXPR coming from
	   the conversion.  This can occur with the 'Old attribute applied
	   to a parameter with an unconstrained type, which gets rewritten
	   into a constrained local variable very late in the game.  */
	if (TREE_CODE (gnu_prefix) == VIEW_CONVERT_EXPR
	    && CONTAINS_PLACEHOLDER_P (TYPE_SIZE (TREE_TYPE (gnu_prefix)))
	    && !CONTAINS_PLACEHOLDER_P
	        (TYPE_SIZE (TREE_TYPE (TREE_OPERAND (gnu_prefix, 0)))))
	  gnu_type = TREE_TYPE (TREE_OPERAND (gnu_prefix, 0));
	else
	  gnu_type = TREE_TYPE (gnu_prefix);

	prefix_unused = true;
	gnu_result_type = get_unpadded_type (Etype (gnat_node));

	if (TYPE_CONVENTION_FORTRAN_P (gnu_type))
	  {
	    int ndim;
	    tree gnu_type_temp;

	    for (ndim = 1, gnu_type_temp = gnu_type;
		 TREE_CODE (TREE_TYPE (gnu_type_temp)) == ARRAY_TYPE
		 && TYPE_MULTI_ARRAY_P (TREE_TYPE (gnu_type_temp));
		 ndim++, gnu_type_temp = TREE_TYPE (gnu_type_temp))
	      ;

	    Dimension = ndim + 1 - Dimension;
	  }

	for (i = 1; i < Dimension; i++)
	  gnu_type = TREE_TYPE (gnu_type);

	gcc_assert (TREE_CODE (gnu_type) == ARRAY_TYPE);

	/* When not optimizing, look up the slot associated with the parameter
	   and the dimension in the cache and create a new one on failure.
	   Don't do this when the actual subtype needs debug info (this happens
	   with -gnatD): in elaborate_expression_1, we create variables that
	   hold the bounds, so caching attributes isn't very interesting and
	   causes dependency issues between these variables and cached
	   expressions.  */
	if (!optimize
	    && Present (gnat_param)
	    && !(Present (Actual_Subtype (gnat_param))
		 && Needs_Debug_Info (Actual_Subtype (gnat_param))))
	  {
	    FOR_EACH_VEC_SAFE_ELT (f_parm_attr_cache, i, pa)
	      if (pa->id == gnat_param && pa->dim == Dimension)
		break;

	    if (!pa)
	      {
		pa = ggc_cleared_alloc<parm_attr_d> ();
		pa->id = gnat_param;
		pa->dim = Dimension;
		vec_safe_push (f_parm_attr_cache, pa);
	      }
	  }

	/* Return the cached expression or build a new one.  */
	if (attribute == Attr_First)
	  {
	    if (pa && pa->first)
	      {
		gnu_result = pa->first;
		break;
	      }

	    gnu_result
	      = TYPE_MIN_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (gnu_type)));
	  }

	else if (attribute == Attr_Last)
	  {
	    if (pa && pa->last)
	      {
		gnu_result = pa->last;
		break;
	      }

	    gnu_result
	      = TYPE_MAX_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (gnu_type)));
	  }

	else /* attribute == Attr_Range_Length || attribute == Attr_Length  */
	  {
	    if (pa && pa->length)
	      {
		gnu_result = pa->length;
		break;
	      }

	    gnu_result
	      = get_type_length (TYPE_INDEX_TYPE (TYPE_DOMAIN (gnu_type)),
				 gnu_result_type);
	  }

	/* If this has a PLACEHOLDER_EXPR, qualify it by the object we are
	   handling.  Note that these attributes could not have been used on
	   an unconstrained array type.  */
	gnu_result = SUBSTITUTE_PLACEHOLDER_IN_EXPR (gnu_result, gnu_prefix);

	/* Cache the expression we have just computed.  Since we want to do it
	   at run time, we force the use of a SAVE_EXPR and let the gimplifier
	   create the temporary in the outermost binding level.  We will make
	   sure in Subprogram_Body_to_gnu that it is evaluated on all possible
	   paths by forcing its evaluation on entry of the function.  */
	if (pa)
	  {
	    gnu_result
	      = build1 (SAVE_EXPR, TREE_TYPE (gnu_result), gnu_result);
	    switch (attribute)
	      {
	      case Attr_First:
		pa->first = gnu_result;
		break;

	      case Attr_Last:
		pa->last = gnu_result;
		break;

	      case Attr_Length:
	      case Attr_Range_Length:
		pa->length = gnu_result;
		break;

	      default:
		gcc_unreachable ();
	      }
	  }

	/* Otherwise, evaluate it each time it is referenced.  */
	else
	  switch (attribute)
	    {
	    case Attr_First:
	    case Attr_Last:
	      /* If we are dereferencing a pointer to unconstrained array, we
		 need to capture the value because the pointed-to bounds may
		 subsequently be released.  */
	      if (unconstrained_ptr_deref)
		gnu_result
		  = build1 (SAVE_EXPR, TREE_TYPE (gnu_result), gnu_result);
	      break;

	    case Attr_Length:
	    case Attr_Range_Length:
	      /* Set the source location onto the predicate of the condition
		 but not if the expression is cached to avoid messing up the
		 debug info.  */
	      if (TREE_CODE (gnu_result) == COND_EXPR
		  && EXPR_P (TREE_OPERAND (gnu_result, 0)))
		set_expr_location_from_node (TREE_OPERAND (gnu_result, 0),
					     gnat_node);
	      break;

	    default:
	      gcc_unreachable ();
	    }

	break;
      }

    case Attr_Bit_Position:
    case Attr_Position:
    case Attr_First_Bit:
    case Attr_Last_Bit:
    case Attr_Bit:
      {
	HOST_WIDE_INT bitsize;
	HOST_WIDE_INT bitpos;
	tree gnu_offset;
	tree gnu_field_bitpos;
	tree gnu_field_offset;
	tree gnu_inner;
	machine_mode mode;
	int unsignedp, reversep, volatilep;

	gnu_result_type = get_unpadded_type (Etype (gnat_node));
	gnu_prefix = remove_conversions (gnu_prefix, true);
	prefix_unused = true;

	/* We can have 'Bit on any object, but if it isn't a COMPONENT_REF,
	   the result is 0.  Don't allow 'Bit on a bare component, though.  */
	if (attribute == Attr_Bit
	    && TREE_CODE (gnu_prefix) != COMPONENT_REF
	    && TREE_CODE (gnu_prefix) != FIELD_DECL)
	  {
	    gnu_result = integer_zero_node;
	    break;
	  }

	else
	  gcc_assert (TREE_CODE (gnu_prefix) == COMPONENT_REF
		      || (attribute == Attr_Bit_Position
			  && TREE_CODE (gnu_prefix) == FIELD_DECL));

	get_inner_reference (gnu_prefix, &bitsize, &bitpos, &gnu_offset,
			     &mode, &unsignedp, &reversep, &volatilep, false);

	if (TREE_CODE (gnu_prefix) == COMPONENT_REF)
	  {
	    gnu_field_bitpos = bit_position (TREE_OPERAND (gnu_prefix, 1));
	    gnu_field_offset = byte_position (TREE_OPERAND (gnu_prefix, 1));

	    for (gnu_inner = TREE_OPERAND (gnu_prefix, 0);
		 TREE_CODE (gnu_inner) == COMPONENT_REF
		 && DECL_INTERNAL_P (TREE_OPERAND (gnu_inner, 1));
		 gnu_inner = TREE_OPERAND (gnu_inner, 0))
	      {
		gnu_field_bitpos
		  = size_binop (PLUS_EXPR, gnu_field_bitpos,
				bit_position (TREE_OPERAND (gnu_inner, 1)));
		gnu_field_offset
		  = size_binop (PLUS_EXPR, gnu_field_offset,
				byte_position (TREE_OPERAND (gnu_inner, 1)));
	      }
	  }
	else if (TREE_CODE (gnu_prefix) == FIELD_DECL)
	  {
	    gnu_field_bitpos = bit_position (gnu_prefix);
	    gnu_field_offset = byte_position (gnu_prefix);
	  }
	else
	  {
	    gnu_field_bitpos = bitsize_zero_node;
	    gnu_field_offset = size_zero_node;
	  }

	switch (attribute)
	  {
	  case Attr_Position:
	    gnu_result = gnu_field_offset;
	    break;

	  case Attr_First_Bit:
	  case Attr_Bit:
	    gnu_result = size_int (bitpos % BITS_PER_UNIT);
	    break;

	  case Attr_Last_Bit:
	    gnu_result = bitsize_int (bitpos % BITS_PER_UNIT);
	    gnu_result = size_binop (PLUS_EXPR, gnu_result,
				     TYPE_SIZE (TREE_TYPE (gnu_prefix)));
	    /* ??? Avoid a large unsigned result that will overflow when
	       converted to the signed universal_integer.  */
	    if (integer_zerop (gnu_result))
	      gnu_result = integer_minus_one_node;
	    else
	      gnu_result
		= size_binop (MINUS_EXPR, gnu_result, bitsize_one_node);
	    break;

	  case Attr_Bit_Position:
	    gnu_result = gnu_field_bitpos;
	    break;
	  }

	/* If this has a PLACEHOLDER_EXPR, qualify it by the object we are
	   handling.  */
	gnu_result = SUBSTITUTE_PLACEHOLDER_IN_EXPR (gnu_result, gnu_prefix);
	break;
      }

    case Attr_Min:
    case Attr_Max:
      {
	tree gnu_lhs = gnat_to_gnu (First (Expressions (gnat_node)));
	tree gnu_rhs = gnat_to_gnu (Next (First (Expressions (gnat_node))));

	gnu_result_type = get_unpadded_type (Etype (gnat_node));

	/* The result of {MIN,MAX}_EXPR is unspecified if either operand is
	   a NaN so we implement the semantics of C99 f{min,max} to make it
	   predictable in this case: if either operand is a NaN, the other
	   is returned; if both operands are NaN's, a NaN is returned.  */
	if (SCALAR_FLOAT_TYPE_P (gnu_result_type)
	    && !Machine_Overflows_On_Target)
	  {
	    const bool lhs_side_effects_p = TREE_SIDE_EFFECTS (gnu_lhs);
	    const bool rhs_side_effects_p = TREE_SIDE_EFFECTS (gnu_rhs);
	    tree t = builtin_decl_explicit (BUILT_IN_ISNAN);
	    tree lhs_is_nan, rhs_is_nan;

	    /* If the operands have side-effects, they need to be evaluated
	       only once in spite of the multiple references in the result.  */
	    if (lhs_side_effects_p)
	      gnu_lhs = gnat_protect_expr (gnu_lhs);
	    if (rhs_side_effects_p)
	      gnu_rhs = gnat_protect_expr (gnu_rhs);

	    lhs_is_nan = fold_build2 (NE_EXPR, boolean_type_node,
				      build_call_expr (t, 1, gnu_lhs),
				      integer_zero_node);

	    rhs_is_nan = fold_build2 (NE_EXPR, boolean_type_node,
				      build_call_expr (t, 1, gnu_rhs),
				      integer_zero_node);

	    gnu_result = build_binary_op (attribute == Attr_Min
					  ? MIN_EXPR : MAX_EXPR,
					  gnu_result_type, gnu_lhs, gnu_rhs);
	    gnu_result = fold_build3 (COND_EXPR, gnu_result_type,
				      rhs_is_nan, gnu_lhs, gnu_result);
	    gnu_result = fold_build3 (COND_EXPR, gnu_result_type,
				      lhs_is_nan, gnu_rhs, gnu_result);

	    /* If the operands have side-effects, they need to be evaluated
	       before doing the tests above since the place they otherwise
	       would end up being evaluated at run time could be wrong.  */
	    if (lhs_side_effects_p)
	      gnu_result
		= build2 (COMPOUND_EXPR, gnu_result_type, gnu_lhs, gnu_result);

	    if (rhs_side_effects_p)
	      gnu_result
		= build2 (COMPOUND_EXPR, gnu_result_type, gnu_rhs, gnu_result);
	  }
	else
	  gnu_result = build_binary_op (attribute == Attr_Min
					? MIN_EXPR : MAX_EXPR,
					gnu_result_type, gnu_lhs, gnu_rhs);
      }
      break;

    case Attr_Passed_By_Reference:
      gnu_result = size_int (default_pass_by_ref (gnu_type)
			     || must_pass_by_ref (gnu_type));
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      break;

    case Attr_Component_Size:
      if (TREE_CODE (gnu_prefix) == COMPONENT_REF
	  && TYPE_IS_PADDING_P (TREE_TYPE (TREE_OPERAND (gnu_prefix, 0))))
	gnu_prefix = TREE_OPERAND (gnu_prefix, 0);

      gnu_prefix = maybe_implicit_deref (gnu_prefix);
      gnu_type = TREE_TYPE (gnu_prefix);

      if (TREE_CODE (gnu_type) == UNCONSTRAINED_ARRAY_TYPE)
	gnu_type = TREE_TYPE (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (gnu_type))));

      while (TREE_CODE (TREE_TYPE (gnu_type)) == ARRAY_TYPE
	     && TYPE_MULTI_ARRAY_P (TREE_TYPE (gnu_type)))
	gnu_type = TREE_TYPE (gnu_type);

      gcc_assert (TREE_CODE (gnu_type) == ARRAY_TYPE);

      /* Note this size cannot be self-referential.  */
      gnu_result = TYPE_SIZE (TREE_TYPE (gnu_type));
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      prefix_unused = true;
      break;

    case Attr_Descriptor_Size:
      gnu_type = TREE_TYPE (gnu_prefix);
      gcc_assert (TREE_CODE (gnu_type) == UNCONSTRAINED_ARRAY_TYPE);

      /* What we want is the offset of the ARRAY field in the record
	 that the thin pointer designates.  */
      gnu_type = TYPE_OBJECT_RECORD_TYPE (gnu_type);
      gnu_result = bit_position (DECL_CHAIN (TYPE_FIELDS (gnu_type)));
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      prefix_unused = true;
      break;

    case Attr_Null_Parameter:
      /* This is just a zero cast to the pointer type for our prefix and
	 dereferenced.  */
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      gnu_result
	= build_unary_op (INDIRECT_REF, NULL_TREE,
			  convert (build_pointer_type (gnu_result_type),
				   integer_zero_node));
      TREE_PRIVATE (gnu_result) = 1;
      break;

    case Attr_Mechanism_Code:
      {
	Entity_Id gnat_obj = Entity (gnat_prefix);
	int code;

	prefix_unused = true;
	gnu_result_type = get_unpadded_type (Etype (gnat_node));
	if (Present (Expressions (gnat_node)))
	  {
	    int i = UI_To_Int (Intval (First (Expressions (gnat_node))));

	    for (gnat_obj = First_Formal (gnat_obj); i > 1;
		 i--, gnat_obj = Next_Formal (gnat_obj))
	      ;
	  }

	code = Mechanism (gnat_obj);
	if (code == Default)
	  code = ((present_gnu_tree (gnat_obj)
		   && (DECL_BY_REF_P (get_gnu_tree (gnat_obj))
		       || ((TREE_CODE (get_gnu_tree (gnat_obj))
			    == PARM_DECL)
			   && (DECL_BY_COMPONENT_PTR_P
			       (get_gnu_tree (gnat_obj))))))
		  ? By_Reference : By_Copy);
	gnu_result = convert (gnu_result_type, size_int (- code));
      }
      break;

    case Attr_Model:
      /* We treat Model as identical to Machine.  This is true for at least
	 IEEE and some other nice floating-point systems.  */

      /* ... fall through ... */

    case Attr_Machine:
      /* The trick is to force the compiler to store the result in memory so
	 that we do not have extra precision used.  But do this only when this
	 is necessary, i.e. if FP_ARITH_MAY_WIDEN is true and the precision of
	 the type is lower than that of the longest floating-point type.  */
      prefix_unused = true;
      gnu_expr = gnat_to_gnu (First (Expressions (gnat_node)));
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      gnu_result = convert (gnu_result_type, gnu_expr);

      if (TREE_CODE (gnu_result) != REAL_CST
	  && fp_arith_may_widen
	  && TYPE_PRECISION (gnu_result_type)
	     < TYPE_PRECISION (longest_float_type_node))
	{
	  tree rec_type = make_node (RECORD_TYPE);
	  tree field
	    = create_field_decl (get_identifier ("OBJ"), gnu_result_type,
				 rec_type, NULL_TREE, NULL_TREE, 0, 0);
	  tree rec_val, asm_expr;

	  finish_record_type (rec_type, field, 0, false);

	  rec_val = build_constructor_single (rec_type, field, gnu_result);
	  rec_val = build1 (SAVE_EXPR, rec_type, rec_val);

	  asm_expr
	    = build5 (ASM_EXPR, void_type_node,
		      build_string (0, ""),
		      tree_cons (build_tree_list (NULL_TREE,
						  build_string (2, "=m")),
				 rec_val, NULL_TREE),
		      tree_cons (build_tree_list (NULL_TREE,
						  build_string (1, "m")),
				 rec_val, NULL_TREE),
		      NULL_TREE, NULL_TREE);
	  ASM_VOLATILE_P (asm_expr) = 1;

	  gnu_result
	    = build_compound_expr (gnu_result_type, asm_expr,
				   build_component_ref (rec_val, field,
							false));
	}
      break;

    case Attr_Deref:
      prefix_unused = true;
      gnu_expr = gnat_to_gnu (First (Expressions (gnat_node)));
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      /* This can be a random address so build an alias-all pointer type.  */
      gnu_expr
	= convert (build_pointer_type_for_mode (gnu_result_type, ptr_mode,
						true),
		   gnu_expr);
      gnu_result = build_unary_op (INDIRECT_REF, NULL_TREE, gnu_expr);
      break;

    default:
      /* This abort means that we have an unimplemented attribute.  */
      gcc_unreachable ();
    }

  /* If this is an attribute where the prefix was unused, force a use of it if
     it has a side-effect.  But don't do it if the prefix is just an entity
     name.  However, if an access check is needed, we must do it.  See second
     example in AARM 11.6(5.e).  */
  if (prefix_unused
      && TREE_SIDE_EFFECTS (gnu_prefix)
      && !Is_Entity_Name (gnat_prefix))
    gnu_result
      = build_compound_expr (TREE_TYPE (gnu_result), gnu_prefix, gnu_result);

  *gnu_result_type_p = gnu_result_type;
  return gnu_result;
}

/* Subroutine of gnat_to_gnu to translate gnat_node, an N_Case_Statement,
   to a GCC tree, which is returned.  */

static tree
Case_Statement_to_gnu (Node_Id gnat_node)
{
  tree gnu_result, gnu_expr, gnu_type, gnu_label;
  Node_Id gnat_when;
  location_t end_locus;
  bool may_fallthru = false;

  gnu_expr = gnat_to_gnu (Expression (gnat_node));
  gnu_expr = convert (get_base_type (TREE_TYPE (gnu_expr)), gnu_expr);
  gnu_expr = maybe_character_value (gnu_expr);
  gnu_type = TREE_TYPE (gnu_expr);

  /* We build a SWITCH_EXPR that contains the code with interspersed
     CASE_LABEL_EXPRs for each label.  */
  if (!Sloc_to_locus (End_Location (gnat_node), &end_locus))
    end_locus = input_location;
  gnu_label = create_artificial_label (end_locus);
  start_stmt_group ();

  for (gnat_when = First_Non_Pragma (Alternatives (gnat_node));
       Present (gnat_when);
       gnat_when = Next_Non_Pragma (gnat_when))
    {
      bool choices_added_p = false;
      Node_Id gnat_choice;

      /* First compile all the different case choices for the current WHEN
	 alternative.  */
      for (gnat_choice = First (Discrete_Choices (gnat_when));
	   Present (gnat_choice);
	   gnat_choice = Next (gnat_choice))
	{
	  tree gnu_low = NULL_TREE, gnu_high = NULL_TREE;
	  tree label = create_artificial_label (input_location);

	  switch (Nkind (gnat_choice))
	    {
	    case N_Range:
	      gnu_low = gnat_to_gnu (Low_Bound (gnat_choice));
	      gnu_high = gnat_to_gnu (High_Bound (gnat_choice));
	      break;

	    case N_Subtype_Indication:
	      gnu_low = gnat_to_gnu (Low_Bound (Range_Expression
						(Constraint (gnat_choice))));
	      gnu_high = gnat_to_gnu (High_Bound (Range_Expression
						  (Constraint (gnat_choice))));
	      break;

	    case N_Identifier:
	    case N_Expanded_Name:
	      /* This represents either a subtype range or a static value of
		 some kind; Ekind says which.  */
	      if (IN (Ekind (Entity (gnat_choice)), Type_Kind))
		{
		  tree gnu_type = get_unpadded_type (Entity (gnat_choice));

		  gnu_low = TYPE_MIN_VALUE (gnu_type);
		  gnu_high = TYPE_MAX_VALUE (gnu_type);
		  break;
		}

	      /* ... fall through ... */

	    case N_Character_Literal:
	    case N_Integer_Literal:
	      gnu_low = gnat_to_gnu (gnat_choice);
	      break;

	    case N_Others_Choice:
	      break;

	    default:
	      gcc_unreachable ();
	    }

	  /* Everything should be folded into constants at this point.  */
	  gcc_assert (!gnu_low  || TREE_CODE (gnu_low)  == INTEGER_CST);
	  gcc_assert (!gnu_high || TREE_CODE (gnu_high) == INTEGER_CST);

	  if (gnu_low && TREE_TYPE (gnu_low) != gnu_type)
	    gnu_low = convert (gnu_type, gnu_low);
	  if (gnu_high && TREE_TYPE (gnu_high) != gnu_type)
	    gnu_high = convert (gnu_type, gnu_high);

	  add_stmt_with_node (build_case_label (gnu_low, gnu_high, label),
			      gnat_choice);
	  choices_added_p = true;
	}

      /* This construct doesn't define a scope so we shouldn't push a binding
	 level around the statement list.  Except that we have always done so
	 historically and this makes it possible to reduce stack usage.  As a
	 compromise, we keep doing it for case statements, for which this has
	 never been problematic, but not for case expressions in Ada 2012.  */
      if (choices_added_p)
	{
	  const bool is_case_expression
	    = (Nkind (Parent (gnat_node)) == N_Expression_With_Actions);
	  tree group
	    = build_stmt_group (Statements (gnat_when), !is_case_expression);
	  bool group_may_fallthru = block_may_fallthru (group);
	  add_stmt (group);
	  if (group_may_fallthru)
	    {
	      tree stmt = build1 (GOTO_EXPR, void_type_node, gnu_label);
	      SET_EXPR_LOCATION (stmt, end_locus);
	      add_stmt (stmt);
	      may_fallthru = true;
	    }
	}
    }

  /* Now emit a definition of the label the cases branch to, if any.  */
  if (may_fallthru)
    add_stmt (build1 (LABEL_EXPR, void_type_node, gnu_label));
  gnu_result
    = build3 (SWITCH_EXPR, gnu_type, gnu_expr, end_stmt_group (), NULL_TREE);

  return gnu_result;
}

/* Return true if we are in the body of a loop.  */

static inline bool
inside_loop_p (void)
{
  return !vec_safe_is_empty (gnu_loop_stack);
}

/* Find out whether EXPR is a simple additive expression based on the iteration
   variable of some enclosing loop in the current function.  If so, return the
   loop and set *DISP to the displacement and *NEG_P to true if this is for a
   subtraction; otherwise, return NULL.  */

static struct loop_info_d *
find_loop_for (tree expr, tree *disp = NULL, bool *neg_p = NULL)
{
  tree var, add, cst;
  bool minus_p;
  struct loop_info_d *iter = NULL;
  unsigned int i;

  if (is_simple_additive_expression (expr, &add, &cst, &minus_p))
    {
      var = add;
      if (disp)
	*disp = cst;
      if (neg_p)
	*neg_p = minus_p;
    }
  else
    {
      var = expr;
      if (disp)
	*disp =  NULL_TREE;
      if (neg_p)
	*neg_p = false;
    }

  var = remove_conversions (var, false);

  if (TREE_CODE (var) != VAR_DECL)
    return NULL;

  if (decl_function_context (var) != current_function_decl)
    return NULL;

  gcc_assert (vec_safe_length (gnu_loop_stack) > 0);

  FOR_EACH_VEC_ELT_REVERSE (*gnu_loop_stack, i, iter)
    if (var == iter->loop_var)
      break;

  return iter;
}

/* Return true if VAL (of type TYPE) can equal the minimum value if MAX is
   false, or the maximum value if MAX is true, of TYPE.  */

static bool
can_equal_min_or_max_val_p (tree val, tree type, bool max)
{
  tree min_or_max_val = (max ? TYPE_MAX_VALUE (type) : TYPE_MIN_VALUE (type));

  if (TREE_CODE (min_or_max_val) != INTEGER_CST)
    return true;

  if (TREE_CODE (val) == NOP_EXPR)
    val = (max
	   ? TYPE_MAX_VALUE (TREE_TYPE (TREE_OPERAND (val, 0)))
	   : TYPE_MIN_VALUE (TREE_TYPE (TREE_OPERAND (val, 0))));

  if (TREE_CODE (val) != INTEGER_CST)
    return true;

  if (max)
    return tree_int_cst_lt (val, min_or_max_val) == 0;
  else
    return tree_int_cst_lt (min_or_max_val, val) == 0;
}

/* Return true if VAL (of type TYPE) can equal the minimum value of TYPE.
   If REVERSE is true, minimum value is taken as maximum value.  */

static inline bool
can_equal_min_val_p (tree val, tree type, bool reverse)
{
  return can_equal_min_or_max_val_p (val, type, reverse);
}

/* Return true if VAL (of type TYPE) can equal the maximum value of TYPE.
   If REVERSE is true, maximum value is taken as minimum value.  */

static inline bool
can_equal_max_val_p (tree val, tree type, bool reverse)
{
  return can_equal_min_or_max_val_p (val, type, !reverse);
}

/* Return true if VAL1 can be lower than VAL2.  */

static bool
can_be_lower_p (tree val1, tree val2)
{
  if (TREE_CODE (val1) == NOP_EXPR)
    val1 = TYPE_MIN_VALUE (TREE_TYPE (TREE_OPERAND (val1, 0)));

  if (TREE_CODE (val1) != INTEGER_CST)
    return true;

  if (TREE_CODE (val2) == NOP_EXPR)
    val2 = TYPE_MAX_VALUE (TREE_TYPE (TREE_OPERAND (val2, 0)));

  if (TREE_CODE (val2) != INTEGER_CST)
    return true;

  return tree_int_cst_lt (val1, val2);
}

/* Replace EXPR1 and EXPR2 by invariant expressions if possible.  Return
   true if both expressions have been replaced and false otherwise.  */

static bool
make_invariant (tree *expr1, tree *expr2)
{
  tree inv_expr1 = gnat_invariant_expr (*expr1);
  tree inv_expr2 = gnat_invariant_expr (*expr2);

  if (inv_expr1)
    *expr1 = inv_expr1;

  if (inv_expr2)
    *expr2 = inv_expr2;

  return inv_expr1 && inv_expr2;
}

/* Helper function for walk_tree, used by independent_iterations_p below.  */

static tree
scan_rhs_r (tree *tp, int *walk_subtrees, void *data)
{
  bitmap *params = (bitmap *)data;
  tree t = *tp;

  /* No need to walk into types or decls.  */
  if (IS_TYPE_OR_DECL_P (t))
    *walk_subtrees = 0;

  if (TREE_CODE (t) == PARM_DECL && bitmap_bit_p (*params, DECL_UID (t)))
    return t;

  return NULL_TREE;
}

/* Return true if STMT_LIST generates independent iterations in a loop.  */

static bool
independent_iterations_p (tree stmt_list)
{
  tree_stmt_iterator tsi;
  bitmap params = BITMAP_GGC_ALLOC();
  auto_vec<tree> rhs;
  tree iter;
  int i;

  if (TREE_CODE (stmt_list) == BIND_EXPR)
    stmt_list = BIND_EXPR_BODY (stmt_list);

  /* Scan the list and return false on anything that is not either a check
     or an assignment to a parameter with restricted aliasing.  */
  for (tsi = tsi_start (stmt_list); !tsi_end_p (tsi); tsi_next (&tsi))
    {
      tree stmt = tsi_stmt (tsi);

      switch (TREE_CODE (stmt))
	{
	case COND_EXPR:
	  {
	    if (COND_EXPR_ELSE (stmt))
	      return false;
	    if (TREE_CODE (COND_EXPR_THEN (stmt)) != CALL_EXPR)
	      return false;
	    tree func = get_callee_fndecl (COND_EXPR_THEN (stmt));
	    if (!(func && TREE_THIS_VOLATILE (func)))
	      return false;
	    break;
	  }

	case MODIFY_EXPR:
	  {
	    tree lhs = TREE_OPERAND (stmt, 0);
	    while (handled_component_p (lhs))
	      lhs = TREE_OPERAND (lhs, 0);
	    if (TREE_CODE (lhs) != INDIRECT_REF)
	      return false;
	    lhs = TREE_OPERAND (lhs, 0);
	    if (!(TREE_CODE (lhs) == PARM_DECL
		  && DECL_RESTRICTED_ALIASING_P (lhs)))
	      return false;
	    bitmap_set_bit (params, DECL_UID (lhs));
	    rhs.safe_push (TREE_OPERAND (stmt, 1));
	    break;
	  }

	default:
	  return false;
	}
    }

  /* At this point we know that the list contains only statements that will
     modify parameters with restricted aliasing.  Check that the statements
     don't at the time read from these parameters.  */
  FOR_EACH_VEC_ELT (rhs, i, iter)
    if (walk_tree_without_duplicates (&iter, scan_rhs_r, &params))
      return false;

  return true;
}

/* Subroutine of gnat_to_gnu to translate gnat_node, an N_Loop_Statement,
   to a GCC tree, which is returned.  */

static tree
Loop_Statement_to_gnu (Node_Id gnat_node)
{
  const Node_Id gnat_iter_scheme = Iteration_Scheme (gnat_node);
  struct loop_info_d *gnu_loop_info = ggc_cleared_alloc<loop_info_d> ();
  tree gnu_loop_stmt = build4 (LOOP_STMT, void_type_node, NULL_TREE,
			       NULL_TREE, NULL_TREE, NULL_TREE);
  tree gnu_loop_label = create_artificial_label (input_location);
  tree gnu_cond_expr = NULL_TREE, gnu_low = NULL_TREE, gnu_high = NULL_TREE;
  tree gnu_result;

  /* Push the loop_info structure associated with the LOOP_STMT.  */
  vec_safe_push (gnu_loop_stack, gnu_loop_info);

  /* Set location information for statement and end label.  */
  set_expr_location_from_node (gnu_loop_stmt, gnat_node);
  Sloc_to_locus (Sloc (End_Label (gnat_node)),
		 &DECL_SOURCE_LOCATION (gnu_loop_label));
  LOOP_STMT_LABEL (gnu_loop_stmt) = gnu_loop_label;

  /* Save the statement for later reuse.  */
  gnu_loop_info->stmt = gnu_loop_stmt;
  gnu_loop_info->artificial = !Comes_From_Source (gnat_node);

  /* Set the condition under which the loop must keep going.
     For the case "LOOP .... END LOOP;" the condition is always true.  */
  if (No (gnat_iter_scheme))
    ;

  /* For the case "WHILE condition LOOP ..... END LOOP;" it's immediate.  */
  else if (Present (Condition (gnat_iter_scheme)))
    LOOP_STMT_COND (gnu_loop_stmt)
      = gnat_to_gnu (Condition (gnat_iter_scheme));

  /* Otherwise we have an iteration scheme and the condition is given by the
     bounds of the subtype of the iteration variable.  */
  else
    {
      Node_Id gnat_loop_spec = Loop_Parameter_Specification (gnat_iter_scheme);
      Entity_Id gnat_loop_var = Defining_Entity (gnat_loop_spec);
      Entity_Id gnat_type = Etype (gnat_loop_var);
      tree gnu_type = get_unpadded_type (gnat_type);
      tree gnu_base_type = maybe_character_type (get_base_type (gnu_type));
      tree gnu_one_node = build_int_cst (gnu_base_type, 1);
      tree gnu_loop_var, gnu_loop_iv, gnu_first, gnu_last, gnu_stmt;
      enum tree_code update_code, test_code, shift_code;
      bool reverse = Reverse_Present (gnat_loop_spec), use_iv = false;

      gnu_low = convert (gnu_base_type, TYPE_MIN_VALUE (gnu_type));
      gnu_high = convert (gnu_base_type, TYPE_MAX_VALUE (gnu_type));

      /* We must disable modulo reduction for the iteration variable, if any,
	 in order for the loop comparison to be effective.  */
      if (reverse)
	{
	  gnu_first = gnu_high;
	  gnu_last = gnu_low;
	  update_code = MINUS_NOMOD_EXPR;
	  test_code = GE_EXPR;
	  shift_code = PLUS_NOMOD_EXPR;
	}
      else
	{
	  gnu_first = gnu_low;
	  gnu_last = gnu_high;
	  update_code = PLUS_NOMOD_EXPR;
	  test_code = LE_EXPR;
	  shift_code = MINUS_NOMOD_EXPR;
	}

      /* We use two different strategies to translate the loop, depending on
	 whether optimization is enabled.

	 If it is, we generate the canonical loop form expected by the loop
	 optimizer and the loop vectorizer, which is the do-while form:

	     ENTRY_COND
	   loop:
	     TOP_UPDATE
	     BODY
	     BOTTOM_COND
	     GOTO loop

	 This avoids an implicit dependency on loop header copying and makes
	 it possible to turn BOTTOM_COND into an inequality test.

	 If optimization is disabled, loop header copying doesn't come into
	 play and we try to generate the loop form with the fewer conditional
	 branches.  First, the default form, which is:

	   loop:
	     TOP_COND
	     BODY
	     BOTTOM_UPDATE
	     GOTO loop

	 It should catch most loops with constant ending point.  Then, if we
	 cannot, we try to generate the shifted form:

	   loop:
	     TOP_COND
	     TOP_UPDATE
	     BODY
	     GOTO loop

	 which should catch loops with constant starting point.  Otherwise, if
	 we cannot, we generate the fallback form:

	     ENTRY_COND
	   loop:
	     BODY
	     BOTTOM_COND
	     BOTTOM_UPDATE
	     GOTO loop

	 which works in all cases.  */

      if (optimize)
	{
	  /* We can use the do-while form directly if GNU_FIRST-1 doesn't
	     overflow.  */
	  if (!can_equal_min_val_p (gnu_first, gnu_base_type, reverse))
	    ;

	  /* Otherwise, use the do-while form with the help of a special
	     induction variable in the unsigned version of the base type
	     or the unsigned version of the size type, whichever is the
	     largest, in order to have wrap-around arithmetics for it.  */
	  else
	    {
	      if (TYPE_PRECISION (gnu_base_type)
		  > TYPE_PRECISION (size_type_node))
		gnu_base_type
		  = gnat_type_for_size (TYPE_PRECISION (gnu_base_type), 1);
	      else
		gnu_base_type = size_type_node;

	      gnu_first = convert (gnu_base_type, gnu_first);
	      gnu_last = convert (gnu_base_type, gnu_last);
	      gnu_one_node = build_int_cst (gnu_base_type, 1);
	      use_iv = true;
	    }

	  gnu_first
	    = build_binary_op (shift_code, gnu_base_type, gnu_first,
			       gnu_one_node);
	  LOOP_STMT_TOP_UPDATE_P (gnu_loop_stmt) = 1;
	  LOOP_STMT_BOTTOM_COND_P (gnu_loop_stmt) = 1;
	}
      else
	{
	  /* We can use the default form if GNU_LAST+1 doesn't overflow.  */
	  if (!can_equal_max_val_p (gnu_last, gnu_base_type, reverse))
	    ;

	  /* Otherwise, we can use the shifted form if neither GNU_FIRST-1 nor
	     GNU_LAST-1 does.  */
	  else if (!can_equal_min_val_p (gnu_first, gnu_base_type, reverse)
		   && !can_equal_min_val_p (gnu_last, gnu_base_type, reverse))
	    {
	      gnu_first
		= build_binary_op (shift_code, gnu_base_type, gnu_first,
				   gnu_one_node);
	      gnu_last
		= build_binary_op (shift_code, gnu_base_type, gnu_last,
				   gnu_one_node);
	      LOOP_STMT_TOP_UPDATE_P (gnu_loop_stmt) = 1;
	    }

	  /* Otherwise, use the fallback form.  */
	  else
	    LOOP_STMT_BOTTOM_COND_P (gnu_loop_stmt) = 1;
	}

      /* If we use the BOTTOM_COND, we can turn the test into an inequality
	 test but we may have to add ENTRY_COND to protect the empty loop.  */
      if (LOOP_STMT_BOTTOM_COND_P (gnu_loop_stmt))
	{
	  test_code = NE_EXPR;
	  if (can_be_lower_p (gnu_high, gnu_low))
	    {
	      gnu_cond_expr
		= build3 (COND_EXPR, void_type_node,
			  build_binary_op (LE_EXPR, boolean_type_node,
					   gnu_low, gnu_high),
			  NULL_TREE, alloc_stmt_list ());
	      set_expr_location_from_node (gnu_cond_expr, gnat_loop_spec);
	    }
	}

      /* Open a new nesting level that will surround the loop to declare the
	 iteration variable.  */
      start_stmt_group ();
      gnat_pushlevel ();

      /* If we use the special induction variable, create it and set it to
	 its initial value.  Morever, the regular iteration variable cannot
	 itself be initialized, lest the initial value wrapped around.  */
      if (use_iv)
	{
	  gnu_loop_iv
	    = create_init_temporary ("I", gnu_first, &gnu_stmt, gnat_loop_var);
	  add_stmt (gnu_stmt);
	  gnu_first = NULL_TREE;
	}
      else
	gnu_loop_iv = NULL_TREE;

      /* Declare the iteration variable and set it to its initial value.  */
      gnu_loop_var = gnat_to_gnu_entity (gnat_loop_var, gnu_first, true);
      if (DECL_BY_REF_P (gnu_loop_var))
	gnu_loop_var = build_unary_op (INDIRECT_REF, NULL_TREE, gnu_loop_var);
      else if (use_iv)
	{
	  gcc_assert (DECL_LOOP_PARM_P (gnu_loop_var));
	  SET_DECL_INDUCTION_VAR (gnu_loop_var, gnu_loop_iv);
	}
      gnu_loop_info->loop_var = gnu_loop_var;
      gnu_loop_info->low_bound = gnu_low;
      gnu_loop_info->high_bound = gnu_high;

      /* Do all the arithmetics in the base type.  */
      gnu_loop_var = convert (gnu_base_type, gnu_loop_var);

      /* Set either the top or bottom exit condition.  */
      if (use_iv)
        LOOP_STMT_COND (gnu_loop_stmt)
	  = build_binary_op (test_code, boolean_type_node, gnu_loop_iv,
			     gnu_last);
      else
        LOOP_STMT_COND (gnu_loop_stmt)
	  = build_binary_op (test_code, boolean_type_node, gnu_loop_var,
			     gnu_last);

      /* Set either the top or bottom update statement and give it the source
	 location of the iteration for better coverage info.  */
      if (use_iv)
	{
	  gnu_stmt
	    = build_binary_op (MODIFY_EXPR, NULL_TREE, gnu_loop_iv,
			       build_binary_op (update_code, gnu_base_type,
						gnu_loop_iv, gnu_one_node));
	  set_expr_location_from_node (gnu_stmt, gnat_iter_scheme);
	  append_to_statement_list (gnu_stmt,
				    &LOOP_STMT_UPDATE (gnu_loop_stmt));
	  gnu_stmt
	    = build_binary_op (MODIFY_EXPR, NULL_TREE, gnu_loop_var,
			       gnu_loop_iv);
	  set_expr_location_from_node (gnu_stmt, gnat_iter_scheme);
	  append_to_statement_list (gnu_stmt,
				    &LOOP_STMT_UPDATE (gnu_loop_stmt));
	}
      else
	{
	  gnu_stmt
	    = build_binary_op (MODIFY_EXPR, NULL_TREE, gnu_loop_var,
			       build_binary_op (update_code, gnu_base_type,
						gnu_loop_var, gnu_one_node));
	  set_expr_location_from_node (gnu_stmt, gnat_iter_scheme);
	  LOOP_STMT_UPDATE (gnu_loop_stmt) = gnu_stmt;
	}
    }

  /* If the loop was named, have the name point to this loop.  In this case,
     the association is not a DECL node, but the end label of the loop.  */
  if (Present (Identifier (gnat_node)))
    save_gnu_tree (Entity (Identifier (gnat_node)), gnu_loop_label, true);

  /* Make the loop body into its own block, so any allocated storage will be
     released every iteration.  This is needed for stack allocation.  */
  LOOP_STMT_BODY (gnu_loop_stmt)
    = build_stmt_group (Statements (gnat_node), true);
  TREE_SIDE_EFFECTS (gnu_loop_stmt) = 1;

  /* If we have an iteration scheme, then we are in a statement group.  Add
     the LOOP_STMT to it, finish it and make it the "loop".  */
  if (Present (gnat_iter_scheme) && No (Condition (gnat_iter_scheme)))
    {
      /* First, if we have computed invariant conditions for range (or index)
	 checks applied to the iteration variable, find out whether they can
	 be evaluated to false at compile time; otherwise, if there are not
	 too many of them, combine them with the original checks.  If loop
	 unswitching is enabled, do not require the loop bounds to be also
	 invariant, as their evaluation will still be ahead of the loop.  */
      if (vec_safe_length (gnu_loop_info->checks) > 0
	 && (make_invariant (&gnu_low, &gnu_high) || flag_unswitch_loops))
	{
	  struct range_check_info_d *rci;
	  unsigned int i, n_remaining_checks = 0;

	  FOR_EACH_VEC_ELT (*gnu_loop_info->checks, i, rci)
	    {
	      tree low_ok, high_ok;

	      if (rci->low_bound)
		{
		  tree gnu_adjusted_low = convert (rci->type, gnu_low);
		  if (rci->disp)
		    gnu_adjusted_low
		      = fold_build2 (rci->neg_p ? MINUS_EXPR : PLUS_EXPR,
				     rci->type, gnu_adjusted_low, rci->disp);
		  low_ok
		    = build_binary_op (GE_EXPR, boolean_type_node,
				       gnu_adjusted_low, rci->low_bound);
		}
	      else
		low_ok = boolean_true_node;

	      if (rci->high_bound)
		{
		  tree gnu_adjusted_high = convert (rci->type, gnu_high);
		  if (rci->disp)
		    gnu_adjusted_high
		      = fold_build2 (rci->neg_p ? MINUS_EXPR : PLUS_EXPR,
				     rci->type, gnu_adjusted_high, rci->disp);
		  high_ok
		    = build_binary_op (LE_EXPR, boolean_type_node,
				       gnu_adjusted_high, rci->high_bound);
		}
	      else
		high_ok = boolean_true_node;

	      tree range_ok
		= build_binary_op (TRUTH_ANDIF_EXPR, boolean_type_node,
				   low_ok, high_ok);

	      rci->invariant_cond
		= build_unary_op (TRUTH_NOT_EXPR, boolean_type_node, range_ok);

	      if (rci->invariant_cond == boolean_false_node)
		TREE_OPERAND (rci->inserted_cond, 0) = rci->invariant_cond;
	      else
		n_remaining_checks++;
	    }

	  /* Note that loop unswitching can only be applied a small number of
	     times to a given loop (PARAM_MAX_UNSWITCH_LEVEL default to 3).  */
	  if (0 < n_remaining_checks && n_remaining_checks <= 3
	      && optimize > 1 && !optimize_size)
	    FOR_EACH_VEC_ELT (*gnu_loop_info->checks, i, rci)
	      if (rci->invariant_cond != boolean_false_node)
		{
		  TREE_OPERAND (rci->inserted_cond, 0) = rci->invariant_cond;

		  if (flag_unswitch_loops)
		    add_stmt_with_node_force (rci->inserted_cond, gnat_node);
		}
	}

      /* Second, if loop vectorization is enabled and the iterations of the
	 loop can easily be proved as independent, mark the loop.  */
      if (optimize
	  && flag_tree_loop_vectorize
	  && independent_iterations_p (LOOP_STMT_BODY (gnu_loop_stmt)))
	LOOP_STMT_IVDEP (gnu_loop_stmt) = 1;

      add_stmt (gnu_loop_stmt);
      gnat_poplevel ();
      gnu_loop_stmt = end_stmt_group ();
    }

  /* If we have an outer COND_EXPR, that's our result and this loop is its
     "true" statement.  Otherwise, the result is the LOOP_STMT.  */
  if (gnu_cond_expr)
    {
      COND_EXPR_THEN (gnu_cond_expr) = gnu_loop_stmt;
      TREE_SIDE_EFFECTS (gnu_cond_expr) = 1;
      gnu_result = gnu_cond_expr;
    }
  else
    gnu_result = gnu_loop_stmt;

  gnu_loop_stack->pop ();

  return gnu_result;
}

/* This page implements a form of Named Return Value optimization modelled
   on the C++ optimization of the same name.  The main difference is that
   we disregard any semantical considerations when applying it here, the
   counterpart being that we don't try to apply it to semantically loaded
   return types, i.e. types with the TYPE_BY_REFERENCE_P flag set.

   We consider a function body of the following GENERIC form:

     return_type R1;
       [...]
     RETURN_EXPR [<retval> = ...]
       [...]
     RETURN_EXPR [<retval> = R1]
       [...]
     return_type Ri;
       [...]
     RETURN_EXPR [<retval> = ...]
       [...]
     RETURN_EXPR [<retval> = Ri]
       [...]

   and we try to fulfill a simple criterion that would make it possible to
   replace one or several Ri variables with the RESULT_DECL of the function.

   The first observation is that RETURN_EXPRs that don't directly reference
   any of the Ri variables on the RHS of their assignment are transparent wrt
   the optimization.  This is because the Ri variables aren't addressable so
   any transformation applied to them doesn't affect the RHS; moreover, the
   assignment writes the full <retval> object so existing values are entirely
   discarded.

   This property can be extended to some forms of RETURN_EXPRs that reference
   the Ri variables, for example CONSTRUCTORs, but isn't true in the general
   case, in particular when function calls are involved.

   Therefore the algorithm is as follows:

     1. Collect the list of candidates for a Named Return Value (Ri variables
	on the RHS of assignments of RETURN_EXPRs) as well as the list of the
	other expressions on the RHS of such assignments.

     2. Prune the members of the first list (candidates) that are referenced
	by a member of the second list (expressions).

     3. Extract a set of candidates with non-overlapping live ranges from the
	first list.  These are the Named Return Values.

     4. Adjust the relevant RETURN_EXPRs and replace the occurrences of the
	Named Return Values in the function with the RESULT_DECL.

   If the function returns an unconstrained type, things are a bit different
   because the anonymous return object is allocated on the secondary stack
   and RESULT_DECL is only a pointer to it.  Each return object can be of a
   different size and is allocated separately so we need not care about the
   aforementioned overlapping issues.  Therefore, we don't collect the other
   expressions and skip step #2 in the algorithm.  */

struct nrv_data
{
  bitmap nrv;
  tree result;
  Node_Id gnat_ret;
  hash_set<tree> *visited;
};

/* Return true if T is a Named Return Value.  */

static inline bool
is_nrv_p (bitmap nrv, tree t)
{
  return TREE_CODE (t) == VAR_DECL && bitmap_bit_p (nrv, DECL_UID (t));
}

/* Helper function for walk_tree, used by finalize_nrv below.  */

static tree
prune_nrv_r (tree *tp, int *walk_subtrees, void *data)
{
  struct nrv_data *dp = (struct nrv_data *)data;
  tree t = *tp;

  /* No need to walk into types or decls.  */
  if (IS_TYPE_OR_DECL_P (t))
    *walk_subtrees = 0;

  if (is_nrv_p (dp->nrv, t))
    bitmap_clear_bit (dp->nrv, DECL_UID (t));

  return NULL_TREE;
}

/* Prune Named Return Values in BLOCK and return true if there is still a
   Named Return Value in BLOCK or one of its sub-blocks.  */

static bool
prune_nrv_in_block (bitmap nrv, tree block)
{
  bool has_nrv = false;
  tree t;

  /* First recurse on the sub-blocks.  */
  for (t = BLOCK_SUBBLOCKS (block); t; t = BLOCK_CHAIN (t))
    has_nrv |= prune_nrv_in_block (nrv, t);

  /* Then make sure to keep at most one NRV per block.  */
  for (t = BLOCK_VARS (block); t; t = DECL_CHAIN (t))
    if (is_nrv_p (nrv, t))
      {
	if (has_nrv)
	  bitmap_clear_bit (nrv, DECL_UID (t));
	else
	  has_nrv = true;
      }

  return has_nrv;
}

/* Helper function for walk_tree, used by finalize_nrv below.  */

static tree
finalize_nrv_r (tree *tp, int *walk_subtrees, void *data)
{
  struct nrv_data *dp = (struct nrv_data *)data;
  tree t = *tp;

  /* No need to walk into types.  */
  if (TYPE_P (t))
    *walk_subtrees = 0;

  /* Change RETURN_EXPRs of NRVs to just refer to the RESULT_DECL; this is a
     nop, but differs from using NULL_TREE in that it indicates that we care
     about the value of the RESULT_DECL.  */
  else if (TREE_CODE (t) == RETURN_EXPR
	   && TREE_CODE (TREE_OPERAND (t, 0)) == INIT_EXPR)
    {
      tree ret_val = TREE_OPERAND (TREE_OPERAND (t, 0), 1);

      /* Strip useless conversions around the return value.  */
      if (gnat_useless_type_conversion (ret_val))
	ret_val = TREE_OPERAND (ret_val, 0);

      if (is_nrv_p (dp->nrv, ret_val))
	TREE_OPERAND (t, 0) = dp->result;
    }

  /* Replace the DECL_EXPR of NRVs with an initialization of the RESULT_DECL,
     if needed.  */
  else if (TREE_CODE (t) == DECL_EXPR
	   && is_nrv_p (dp->nrv, DECL_EXPR_DECL (t)))
    {
      tree var = DECL_EXPR_DECL (t), init;

      if (DECL_INITIAL (var))
	{
	  init = build_binary_op (INIT_EXPR, NULL_TREE, dp->result,
				  DECL_INITIAL (var));
	  SET_EXPR_LOCATION (init, EXPR_LOCATION (t));
	  DECL_INITIAL (var) = NULL_TREE;
	}
      else
	init = build_empty_stmt (EXPR_LOCATION (t));
      *tp = init;

      /* Identify the NRV to the RESULT_DECL for debugging purposes.  */
      SET_DECL_VALUE_EXPR (var, dp->result);
      DECL_HAS_VALUE_EXPR_P (var) = 1;
      /* ??? Kludge to avoid an assertion failure during inlining.  */
      DECL_SIZE (var) = bitsize_unit_node;
      DECL_SIZE_UNIT (var) = size_one_node;
    }

  /* And replace all uses of NRVs with the RESULT_DECL.  */
  else if (is_nrv_p (dp->nrv, t))
    *tp = convert (TREE_TYPE (t), dp->result);

  /* Avoid walking into the same tree more than once.  Unfortunately, we
     can't just use walk_tree_without_duplicates because it would only
     call us for the first occurrence of NRVs in the function body.  */
  if (dp->visited->add (*tp))
    *walk_subtrees = 0;

  return NULL_TREE;
}

/* Likewise, but used when the function returns an unconstrained type.  */

static tree
finalize_nrv_unc_r (tree *tp, int *walk_subtrees, void *data)
{
  struct nrv_data *dp = (struct nrv_data *)data;
  tree t = *tp;

  /* No need to walk into types.  */
  if (TYPE_P (t))
    *walk_subtrees = 0;

  /* We need to see the DECL_EXPR of NRVs before any other references so we
     walk the body of BIND_EXPR before walking its variables.  */
  else if (TREE_CODE (t) == BIND_EXPR)
    walk_tree (&BIND_EXPR_BODY (t), finalize_nrv_unc_r, data, NULL);

  /* Change RETURN_EXPRs of NRVs to assign to the RESULT_DECL only the final
     return value built by the allocator instead of the whole construct.  */
  else if (TREE_CODE (t) == RETURN_EXPR
	   && TREE_CODE (TREE_OPERAND (t, 0)) == INIT_EXPR)
    {
      tree ret_val = TREE_OPERAND (TREE_OPERAND (t, 0), 1);

      /* This is the construct returned by the allocator.  */
      if (TREE_CODE (ret_val) == COMPOUND_EXPR
	  && TREE_CODE (TREE_OPERAND (ret_val, 0)) == INIT_EXPR)
	{
	  tree rhs = TREE_OPERAND (TREE_OPERAND (ret_val, 0), 1);

	  if (TYPE_IS_FAT_POINTER_P (TREE_TYPE (ret_val)))
	    ret_val = CONSTRUCTOR_ELT (rhs, 1)->value;
	  else
	    ret_val = rhs;
	}

      /* Strip useless conversions around the return value.  */
      if (gnat_useless_type_conversion (ret_val)
	  || TREE_CODE (ret_val) == VIEW_CONVERT_EXPR)
	ret_val = TREE_OPERAND (ret_val, 0);

      /* Strip unpadding around the return value.  */
      if (TREE_CODE (ret_val) == COMPONENT_REF
	  && TYPE_IS_PADDING_P (TREE_TYPE (TREE_OPERAND (ret_val, 0))))
	ret_val = TREE_OPERAND (ret_val, 0);

      /* Assign the new return value to the RESULT_DECL.  */
      if (is_nrv_p (dp->nrv, ret_val))
	TREE_OPERAND (TREE_OPERAND (t, 0), 1)
	  = TREE_OPERAND (DECL_INITIAL (ret_val), 0);
    }

  /* Adjust the DECL_EXPR of NRVs to call the allocator and save the result
     into a new variable.  */
  else if (TREE_CODE (t) == DECL_EXPR
	   && is_nrv_p (dp->nrv, DECL_EXPR_DECL (t)))
    {
      tree saved_current_function_decl = current_function_decl;
      tree var = DECL_EXPR_DECL (t);
      tree alloc, p_array, new_var, new_ret;
      vec<constructor_elt, va_gc> *v;
      vec_alloc (v, 2);

      /* Create an artificial context to build the allocation.  */
      current_function_decl = decl_function_context (var);
      start_stmt_group ();
      gnat_pushlevel ();

      /* This will return a COMPOUND_EXPR with the allocation in the first
	 arm and the final return value in the second arm.  */
      alloc = build_allocator (TREE_TYPE (var), DECL_INITIAL (var),
			       TREE_TYPE (dp->result),
			       Procedure_To_Call (dp->gnat_ret),
			       Storage_Pool (dp->gnat_ret),
			       Empty, false);

      /* The new variable is built as a reference to the allocated space.  */
      new_var
	= build_decl (DECL_SOURCE_LOCATION (var), VAR_DECL, DECL_NAME (var),
		      build_reference_type (TREE_TYPE (var)));
      DECL_BY_REFERENCE (new_var) = 1;

      if (TYPE_IS_FAT_POINTER_P (TREE_TYPE (alloc)))
	{
	  tree cst = TREE_OPERAND (alloc, 1);

	  /* The new initial value is a COMPOUND_EXPR with the allocation in
	     the first arm and the value of P_ARRAY in the second arm.  */
	  DECL_INITIAL (new_var)
	    = build2 (COMPOUND_EXPR, TREE_TYPE (new_var),
		      TREE_OPERAND (alloc, 0),
		      CONSTRUCTOR_ELT (cst, 0)->value);

	  /* Build a modified CONSTRUCTOR that references NEW_VAR.  */
	  p_array = TYPE_FIELDS (TREE_TYPE (alloc));
	  CONSTRUCTOR_APPEND_ELT (v, p_array,
				  fold_convert (TREE_TYPE (p_array), new_var));
	  CONSTRUCTOR_APPEND_ELT (v, DECL_CHAIN (p_array),
				  CONSTRUCTOR_ELT (cst, 1)->value);
	  new_ret = build_constructor (TREE_TYPE (alloc), v);
	}
      else
	{
	  /* The new initial value is just the allocation.  */
	  DECL_INITIAL (new_var) = alloc;
	  new_ret = fold_convert (TREE_TYPE (alloc), new_var);
	}

      gnat_pushdecl (new_var, Empty);

      /* Destroy the artificial context and insert the new statements.  */
      gnat_zaplevel ();
      *tp = end_stmt_group ();
      current_function_decl = saved_current_function_decl;

      /* Chain NEW_VAR immediately after VAR and ignore the latter.  */
      DECL_CHAIN (new_var) = DECL_CHAIN (var);
      DECL_CHAIN (var) = new_var;
      DECL_IGNORED_P (var) = 1;

      /* Save the new return value and the dereference of NEW_VAR.  */
      DECL_INITIAL (var)
	= build2 (COMPOUND_EXPR, TREE_TYPE (var), new_ret,
		  build1 (INDIRECT_REF, TREE_TYPE (var), new_var));
      /* ??? Kludge to avoid messing up during inlining.  */
      DECL_CONTEXT (var) = NULL_TREE;
    }

  /* And replace all uses of NRVs with the dereference of NEW_VAR.  */
  else if (is_nrv_p (dp->nrv, t))
    *tp = TREE_OPERAND (DECL_INITIAL (t), 1);

  /* Avoid walking into the same tree more than once.  Unfortunately, we
     can't just use walk_tree_without_duplicates because it would only
     call us for the first occurrence of NRVs in the function body.  */
  if (dp->visited->add (*tp))
    *walk_subtrees = 0;

  return NULL_TREE;
}

/* Finalize the Named Return Value optimization for FNDECL.  The NRV bitmap
   contains the candidates for Named Return Value and OTHER is a list of
   the other return values.  GNAT_RET is a representative return node.  */

static void
finalize_nrv (tree fndecl, bitmap nrv, vec<tree, va_gc> *other, Node_Id gnat_ret)
{
  struct cgraph_node *node;
  struct nrv_data data;
  walk_tree_fn func;
  unsigned int i;
  tree iter;

  /* We shouldn't be applying the optimization to return types that we aren't
     allowed to manipulate freely.  */
  gcc_assert (!TYPE_IS_BY_REFERENCE_P (TREE_TYPE (TREE_TYPE (fndecl))));

  /* Prune the candidates that are referenced by other return values.  */
  data.nrv = nrv;
  data.result = NULL_TREE;
  data.gnat_ret = Empty;
  data.visited = NULL;
  FOR_EACH_VEC_SAFE_ELT (other, i, iter)
    walk_tree_without_duplicates (&iter, prune_nrv_r, &data);
  if (bitmap_empty_p (nrv))
    return;

  /* Prune also the candidates that are referenced by nested functions.  */
  node = cgraph_node::get_create (fndecl);
  for (node = node->nested; node; node = node->next_nested)
    walk_tree_without_duplicates (&DECL_SAVED_TREE (node->decl), prune_nrv_r,
				  &data);
  if (bitmap_empty_p (nrv))
    return;

  /* Extract a set of NRVs with non-overlapping live ranges.  */
  if (!prune_nrv_in_block (nrv, DECL_INITIAL (fndecl)))
    return;

  /* Adjust the relevant RETURN_EXPRs and replace the occurrences of NRVs.  */
  data.nrv = nrv;
  data.result = DECL_RESULT (fndecl);
  data.gnat_ret = gnat_ret;
  data.visited = new hash_set<tree>;
  if (TYPE_RETURN_UNCONSTRAINED_P (TREE_TYPE (fndecl)))
    func = finalize_nrv_unc_r;
  else
    func = finalize_nrv_r;
  walk_tree (&DECL_SAVED_TREE (fndecl), func, &data, NULL);
  delete data.visited;
}

/* Return true if RET_VAL can be used as a Named Return Value for the
   anonymous return object RET_OBJ.  */

static bool
return_value_ok_for_nrv_p (tree ret_obj, tree ret_val)
{
  if (TREE_CODE (ret_val) != VAR_DECL)
    return false;

  if (TREE_THIS_VOLATILE (ret_val))
    return false;

  if (DECL_CONTEXT (ret_val) != current_function_decl)
    return false;

  if (TREE_STATIC (ret_val))
    return false;

  if (TREE_ADDRESSABLE (ret_val))
    return false;

  if (ret_obj && DECL_ALIGN (ret_val) > DECL_ALIGN (ret_obj))
    return false;

  return true;
}

/* Build a RETURN_EXPR.  If RET_VAL is non-null, build a RETURN_EXPR around
   the assignment of RET_VAL to RET_OBJ.  Otherwise build a bare RETURN_EXPR
   around RESULT_OBJ, which may be null in this case.  */

static tree
build_return_expr (tree ret_obj, tree ret_val)
{
  tree result_expr;

  if (ret_val)
    {
      /* The gimplifier explicitly enforces the following invariant:

	      RETURN_EXPR
		  |
	       INIT_EXPR
	      /        \
	     /          \
	 RET_OBJ        ...

	 As a consequence, type consistency dictates that we use the type
	 of the RET_OBJ as the operation type.  */
      tree operation_type = TREE_TYPE (ret_obj);

      /* Convert the right operand to the operation type.  Note that this is
	 the transformation applied in the INIT_EXPR case of build_binary_op,
	 with the assumption that the type cannot involve a placeholder.  */
      if (operation_type != TREE_TYPE (ret_val))
	ret_val = convert (operation_type, ret_val);

      /* We always can use an INIT_EXPR for the return object.  */
      result_expr = build2 (INIT_EXPR, void_type_node, ret_obj, ret_val);

      /* If the function returns an aggregate type, find out whether this is
	 a candidate for Named Return Value.  If so, record it.  Otherwise,
	 if this is an expression of some kind, record it elsewhere.  */
      if (optimize
	  && AGGREGATE_TYPE_P (operation_type)
	  && !TYPE_IS_FAT_POINTER_P (operation_type)
	  && TYPE_MODE (operation_type) == BLKmode
	  && aggregate_value_p (operation_type, current_function_decl))
	{
	  /* Strip useless conversions around the return value.  */
	  if (gnat_useless_type_conversion (ret_val))
	    ret_val = TREE_OPERAND (ret_val, 0);

	  /* Now apply the test to the return value.  */
	  if (return_value_ok_for_nrv_p (ret_obj, ret_val))
	    {
	      if (!f_named_ret_val)
		f_named_ret_val = BITMAP_GGC_ALLOC ();
	      bitmap_set_bit (f_named_ret_val, DECL_UID (ret_val));
	    }

	  /* Note that we need not care about CONSTRUCTORs here, as they are
	     totally transparent given the read-compose-write semantics of
	     assignments from CONSTRUCTORs.  */
	  else if (EXPR_P (ret_val))
	    vec_safe_push (f_other_ret_val, ret_val);
	}
    }
  else
    result_expr = ret_obj;

  return build1 (RETURN_EXPR, void_type_node, result_expr);
}

/* Subroutine of gnat_to_gnu to process gnat_node, an N_Subprogram_Body.  We
   don't return anything.  */

static void
Subprogram_Body_to_gnu (Node_Id gnat_node)
{
  /* Defining identifier of a parameter to the subprogram.  */
  Entity_Id gnat_param;
  /* The defining identifier for the subprogram body. Note that if a
     specification has appeared before for this body, then the identifier
     occurring in that specification will also be a defining identifier and all
     the calls to this subprogram will point to that specification.  */
  Entity_Id gnat_subprog_id
    = (Present (Corresponding_Spec (gnat_node))
       ? Corresponding_Spec (gnat_node) : Defining_Entity (gnat_node));
  /* The FUNCTION_DECL node corresponding to the subprogram spec.   */
  tree gnu_subprog_decl;
  /* Its RESULT_DECL node.  */
  tree gnu_result_decl;
  /* Its FUNCTION_TYPE node.  */
  tree gnu_subprog_type;
  /* The TYPE_CI_CO_LIST of its FUNCTION_TYPE node, if any.  */
  tree gnu_cico_list;
  /* The entry in the CI_CO_LIST that represents a function return, if any.  */
  tree gnu_return_var_elmt = NULL_TREE;
  tree gnu_result;
  location_t locus;
  struct language_function *gnu_subprog_language;
  vec<parm_attr, va_gc> *cache;

  /* If this is a generic object or if it has been eliminated,
     ignore it.  */
  if (Ekind (gnat_subprog_id) == E_Generic_Procedure
      || Ekind (gnat_subprog_id) == E_Generic_Function
      || Is_Eliminated (gnat_subprog_id))
    return;

  /* If this subprogram acts as its own spec, define it.  Otherwise, just get
     the already-elaborated tree node.  However, if this subprogram had its
     elaboration deferred, we will already have made a tree node for it.  So
     treat it as not being defined in that case.  Such a subprogram cannot
     have an address clause or a freeze node, so this test is safe, though it
     does disable some otherwise-useful error checking.  */
  gnu_subprog_decl
    = gnat_to_gnu_entity (gnat_subprog_id, NULL_TREE,
			  Acts_As_Spec (gnat_node)
			  && !present_gnu_tree (gnat_subprog_id));
  gnu_result_decl = DECL_RESULT (gnu_subprog_decl);
  gnu_subprog_type = TREE_TYPE (gnu_subprog_decl);
  gnu_cico_list = TYPE_CI_CO_LIST (gnu_subprog_type);
  if (gnu_cico_list && TREE_VALUE (gnu_cico_list) == void_type_node)
    gnu_return_var_elmt = gnu_cico_list;

  /* If the function returns by invisible reference, make it explicit in the
     function body.  See gnat_to_gnu_entity, E_Subprogram_Type case.  */
  if (TREE_ADDRESSABLE (gnu_subprog_type))
    {
      TREE_TYPE (gnu_result_decl)
	= build_reference_type (TREE_TYPE (gnu_result_decl));
      relayout_decl (gnu_result_decl);
    }

  /* Set the line number in the decl to correspond to that of the body.  */
  Sloc_to_locus (Sloc (gnat_node), &locus);
  DECL_SOURCE_LOCATION (gnu_subprog_decl) = locus;

  /* Initialize the information structure for the function.  */
  allocate_struct_function (gnu_subprog_decl, false);
  gnu_subprog_language = ggc_cleared_alloc<language_function> ();
  DECL_STRUCT_FUNCTION (gnu_subprog_decl)->language = gnu_subprog_language;
  DECL_STRUCT_FUNCTION (gnu_subprog_decl)->function_start_locus = locus;
  set_cfun (NULL);

  begin_subprog_body (gnu_subprog_decl);

  /* If there are copy-in/copy-out parameters, we need to ensure that they are
     properly copied out by the return statement.  We do this by making a new
     block and converting any return into a goto to a label at the end of the
     block.  */
  if (gnu_cico_list)
    {
      tree gnu_return_var = NULL_TREE;

      vec_safe_push (gnu_return_label_stack,
		     create_artificial_label (input_location));

      start_stmt_group ();
      gnat_pushlevel ();

      /* If this is a function with copy-in/copy-out parameters and which does
	 not return by invisible reference, we also need a variable for the
	 return value to be placed.  */
      if (gnu_return_var_elmt && !TREE_ADDRESSABLE (gnu_subprog_type))
	{
	  tree gnu_return_type
	    = TREE_TYPE (TREE_PURPOSE (gnu_return_var_elmt));

	  gnu_return_var
	    = create_var_decl (get_identifier ("RETVAL"), NULL_TREE,
			       gnu_return_type, NULL_TREE,
			       false, false, false, false, false,
			       true, false, NULL, gnat_subprog_id);
	  TREE_VALUE (gnu_return_var_elmt) = gnu_return_var;
	}

      vec_safe_push (gnu_return_var_stack, gnu_return_var);

      /* See whether there are parameters for which we don't have a GCC tree
	 yet.  These must be Out parameters.  Make a VAR_DECL for them and
	 put it into TYPE_CI_CO_LIST, which must contain an empty entry too.
	 We can match up the entries because TYPE_CI_CO_LIST is in the order
	 of the parameters.  */
      for (gnat_param = First_Formal_With_Extras (gnat_subprog_id);
	   Present (gnat_param);
	   gnat_param = Next_Formal_With_Extras (gnat_param))
	if (!present_gnu_tree (gnat_param))
	  {
	    tree gnu_cico_entry = gnu_cico_list;
	    tree gnu_decl;

	    /* Skip any entries that have been already filled in; they must
	       correspond to In Out parameters.  */
	    while (gnu_cico_entry && TREE_VALUE (gnu_cico_entry))
	      gnu_cico_entry = TREE_CHAIN (gnu_cico_entry);

	    /* Do any needed dereferences for by-ref objects.  */
	    gnu_decl = gnat_to_gnu_entity (gnat_param, NULL_TREE, true);
	    gcc_assert (DECL_P (gnu_decl));
	    if (DECL_BY_REF_P (gnu_decl))
	      gnu_decl = build_unary_op (INDIRECT_REF, NULL_TREE, gnu_decl);

	    /* Do any needed references for padded types.  */
	    TREE_VALUE (gnu_cico_entry)
	      = convert (TREE_TYPE (TREE_PURPOSE (gnu_cico_entry)), gnu_decl);
	  }
    }
  else
    vec_safe_push (gnu_return_label_stack, NULL_TREE);

  /* Get a tree corresponding to the code for the subprogram.  */
  start_stmt_group ();
  gnat_pushlevel ();

  process_decls (Declarations (gnat_node), Empty, Empty, true, true);

  /* Generate the code of the subprogram itself.  A return statement will be
     present and any Out parameters will be handled there.  */
  add_stmt (gnat_to_gnu (Handled_Statement_Sequence (gnat_node)));
  gnat_poplevel ();
  gnu_result = end_stmt_group ();

  /* If we populated the parameter attributes cache, we need to make sure that
     the cached expressions are evaluated on all the possible paths leading to
     their uses.  So we force their evaluation on entry of the function.  */
  cache = gnu_subprog_language->parm_attr_cache;
  if (cache)
    {
      struct parm_attr_d *pa;
      int i;

      start_stmt_group ();

      FOR_EACH_VEC_ELT (*cache, i, pa)
	{
	  if (pa->first)
	    add_stmt_with_node_force (pa->first, gnat_node);
	  if (pa->last)
	    add_stmt_with_node_force (pa->last, gnat_node);
	  if (pa->length)
	    add_stmt_with_node_force (pa->length, gnat_node);
	}

      add_stmt (gnu_result);
      gnu_result = end_stmt_group ();

      gnu_subprog_language->parm_attr_cache = NULL;
    }

  /* If we are dealing with a return from an Ada procedure with parameters
     passed by copy-in/copy-out, we need to return a record containing the
     final values of these parameters.  If the list contains only one entry,
     return just that entry though.

     For a full description of the copy-in/copy-out parameter mechanism, see
     the part of the gnat_to_gnu_entity routine dealing with the translation
     of subprograms.

     We need to make a block that contains the definition of that label and
     the copying of the return value.  It first contains the function, then
     the label and copy statement.  */
  if (gnu_cico_list)
    {
      const Node_Id gnat_end_label
	= End_Label (Handled_Statement_Sequence (gnat_node));

      gnu_return_var_stack->pop ();

      add_stmt (gnu_result);
      add_stmt (build1 (LABEL_EXPR, void_type_node,
			gnu_return_label_stack->last ()));

      /* If this is a function which returns by invisible reference, the
	 return value has already been dealt with at the return statements,
	 so we only need to indirectly copy out the parameters.  */
      if (TREE_ADDRESSABLE (gnu_subprog_type))
	{
	  tree gnu_ret_deref
	    = build_unary_op (INDIRECT_REF, NULL_TREE, gnu_result_decl);
	  tree t;

	  gcc_assert (TREE_VALUE (gnu_cico_list) == void_type_node);

	  for (t = TREE_CHAIN (gnu_cico_list); t; t = TREE_CHAIN (t))
	    {
	      tree gnu_field_deref
		= build_component_ref (gnu_ret_deref, TREE_PURPOSE (t), true);
	      gnu_result = build2 (MODIFY_EXPR, void_type_node,
				   gnu_field_deref, TREE_VALUE (t));
	      add_stmt_with_node (gnu_result, gnat_end_label);
	    }
	}

      /* Otherwise, if this is a procedure or a function which does not return
	 by invisible reference, we can do a direct block-copy out.  */
      else
	{
	  tree gnu_retval;

	  if (list_length (gnu_cico_list) == 1)
	    gnu_retval = TREE_VALUE (gnu_cico_list);
	  else
	    gnu_retval
	      = build_constructor_from_list (TREE_TYPE (gnu_subprog_type),
					     gnu_cico_list);

	  gnu_result = build_return_expr (gnu_result_decl, gnu_retval);
	  add_stmt_with_node (gnu_result, gnat_end_label);
	}

      gnat_poplevel ();
      gnu_result = end_stmt_group ();
    }

  gnu_return_label_stack->pop ();

  /* Attempt setting the end_locus of our GCC body tree, typically a
     BIND_EXPR or STATEMENT_LIST, then the end_locus of our GCC subprogram
     declaration tree.  */
  set_end_locus_from_node (gnu_result, gnat_node);
  set_end_locus_from_node (gnu_subprog_decl, gnat_node);

  /* On SEH targets, install an exception handler around the main entry
     point to catch unhandled exceptions.  */
  if (DECL_NAME (gnu_subprog_decl) == main_identifier_node
      && targetm_common.except_unwind_info (&global_options) == UI_SEH)
    {
      tree t;
      tree etype;

      t = build_call_expr (builtin_decl_explicit (BUILT_IN_EH_POINTER),
			   1, integer_zero_node);
      t = build_call_n_expr (unhandled_except_decl, 1, t);

      etype = build_unary_op (ADDR_EXPR, NULL_TREE, unhandled_others_decl);
      etype = tree_cons (NULL_TREE, etype, NULL_TREE);

      t = build2 (CATCH_EXPR, void_type_node, etype, t);
      gnu_result = build2 (TRY_CATCH_EXPR, TREE_TYPE (gnu_result),
			   gnu_result, t);
    }

  end_subprog_body (gnu_result);

  /* Finally annotate the parameters and disconnect the trees for parameters
     that we have turned into variables since they are now unusable.  */
  for (gnat_param = First_Formal_With_Extras (gnat_subprog_id);
       Present (gnat_param);
       gnat_param = Next_Formal_With_Extras (gnat_param))
    {
      tree gnu_param = get_gnu_tree (gnat_param);
      bool is_var_decl = (TREE_CODE (gnu_param) == VAR_DECL);

      annotate_object (gnat_param, TREE_TYPE (gnu_param), NULL_TREE,
		       DECL_BY_REF_P (gnu_param));

      if (is_var_decl)
	save_gnu_tree (gnat_param, NULL_TREE, false);
    }

  /* Disconnect the variable created for the return value.  */
  if (gnu_return_var_elmt)
    TREE_VALUE (gnu_return_var_elmt) = void_type_node;

  /* If the function returns an aggregate type and we have candidates for
     a Named Return Value, finalize the optimization.  */
  if (optimize && gnu_subprog_language->named_ret_val)
    {
      finalize_nrv (gnu_subprog_decl,
		    gnu_subprog_language->named_ret_val,
		    gnu_subprog_language->other_ret_val,
		    gnu_subprog_language->gnat_ret);
      gnu_subprog_language->named_ret_val = NULL;
      gnu_subprog_language->other_ret_val = NULL;
    }

  /* If this is an inlined external function that has been marked uninlinable,
     drop the body and stop there.  Otherwise compile the body.  */
  if (DECL_EXTERNAL (gnu_subprog_decl) && DECL_UNINLINABLE (gnu_subprog_decl))
    DECL_SAVED_TREE (gnu_subprog_decl) = NULL_TREE;
  else
    rest_of_subprog_body_compilation (gnu_subprog_decl);
}

/* Return true if GNAT_NODE references an Atomic entity.  */

static bool
node_is_atomic (Node_Id gnat_node)
{
  Entity_Id gnat_entity;

  switch (Nkind (gnat_node))
    {
    case N_Identifier:
    case N_Expanded_Name:
      gnat_entity = Entity (gnat_node);
      if (Ekind (gnat_entity) != E_Variable)
	break;
      return Is_Atomic (gnat_entity) || Is_Atomic (Etype (gnat_entity));

    case N_Selected_Component:
      gnat_entity = Entity (Selector_Name (gnat_node));
      return Is_Atomic (gnat_entity) || Is_Atomic (Etype (gnat_entity));

    case N_Indexed_Component:
      if (Has_Atomic_Components (Etype (Prefix (gnat_node))))
	return true;
      if (Is_Entity_Name (Prefix (gnat_node))
	  && Has_Atomic_Components (Entity (Prefix (gnat_node))))
	return true;

      /* ... fall through ... */

    case N_Explicit_Dereference:
      return Is_Atomic (Etype (gnat_node));

    default:
      break;
    }

  return false;
}

/* Return true if GNAT_NODE references a Volatile_Full_Access entity.  */

static bool
node_has_volatile_full_access (Node_Id gnat_node)
{
  Entity_Id gnat_entity;

  switch (Nkind (gnat_node))
    {
    case N_Identifier:
    case N_Expanded_Name:
      gnat_entity = Entity (gnat_node);
      if (Ekind (gnat_entity) != E_Variable)
	break;
      return Is_Volatile_Full_Access (gnat_entity)
	     || Is_Volatile_Full_Access (Etype (gnat_entity));

    case N_Selected_Component:
      gnat_entity = Entity (Selector_Name (gnat_node));
      return Is_Volatile_Full_Access (gnat_entity)
	     || Is_Volatile_Full_Access (Etype (gnat_entity));

    case N_Indexed_Component:
    case N_Explicit_Dereference:
      return Is_Volatile_Full_Access (Etype (gnat_node));

    default:
      break;
    }

  return false;
}

/* Strip any type conversion on GNAT_NODE and return the result.  */

static Node_Id
gnat_strip_type_conversion (Node_Id gnat_node)
{
  Node_Kind kind = Nkind (gnat_node);

  if (kind == N_Type_Conversion || kind == N_Unchecked_Type_Conversion)
    gnat_node = Expression (gnat_node);

  return gnat_node;
}

/* Return true if GNAT_NODE requires outer atomic access, i.e. atomic access
   of an object of which GNAT_NODE is a component.  */

static bool
outer_atomic_access_required_p (Node_Id gnat_node)
{
  gnat_node = gnat_strip_type_conversion (gnat_node);

  while (true)
    {
      switch (Nkind (gnat_node))
	{
	case N_Identifier:
	case N_Expanded_Name:
	  if (No (Renamed_Object (Entity (gnat_node))))
	    return false;
	  gnat_node
	    = gnat_strip_type_conversion (Renamed_Object (Entity (gnat_node)));
	  break;

	case N_Indexed_Component:
	case N_Selected_Component:
	case N_Slice:
	  gnat_node = gnat_strip_type_conversion (Prefix (gnat_node));
	  if (node_has_volatile_full_access (gnat_node))
	    return true;
	  break;

	default:
	  return false;
	}
    }

  gcc_unreachable ();
}

/* Return true if GNAT_NODE requires atomic access and set SYNC according to
   the associated synchronization setting.  */

static bool
atomic_access_required_p (Node_Id gnat_node, bool *sync)
{
  const Node_Id gnat_parent = Parent (gnat_node);
  unsigned char attr_id;
  bool as_a_whole = true;

  /* First, scan the parent to find out cases where the flag is irrelevant.  */
  switch (Nkind (gnat_parent))
    {
    case N_Attribute_Reference:
      attr_id = Get_Attribute_Id (Attribute_Name (gnat_parent));
      /* Do not mess up machine code insertions.  */
      if (attr_id == Attr_Asm_Input || attr_id == Attr_Asm_Output)
	return false;

      /* Nothing to do if we are the prefix of an attribute, since we do not
	 want an atomic access for things like 'Size.  */

      /* ... fall through ... */

    case N_Reference:
      /* The N_Reference node is like an attribute.  */
      if (Prefix (gnat_parent) == gnat_node)
	return false;
      break;

    case N_Indexed_Component:
    case N_Selected_Component:
    case N_Slice:
      /* If we are the prefix, then the access is only partial.  */
      if (Prefix (gnat_parent) == gnat_node)
	as_a_whole = false;
      break;

    case N_Object_Renaming_Declaration:
      /* Nothing to do for the identifier in an object renaming declaration,
         the renaming itself does not need atomic access.  */
      return false;

    default:
      break;
    }

  /* Then, scan the node to find the atomic object.  */
  gnat_node = gnat_strip_type_conversion (gnat_node);

  /* For Atomic itself, only reads and updates of the object as a whole require
     atomic access (RM C.6 (15)).  But for Volatile_Full_Access, all reads and
     updates require atomic access.  */
  if (!(as_a_whole && node_is_atomic (gnat_node))
      && !node_has_volatile_full_access (gnat_node))
    return false;

  /* If an outer atomic access will also be required, it cancels this one.  */
  if (outer_atomic_access_required_p (gnat_node))
    return false;

  *sync = Atomic_Sync_Required (gnat_node);

  return true;
}

/* Create a temporary variable with PREFIX and TYPE, and return it.  */

static tree
create_temporary (const char *prefix, tree type)
{
  tree gnu_temp
    = create_var_decl (create_tmp_var_name (prefix), NULL_TREE,
		      type, NULL_TREE,
		      false, false, false, false, false,
		      true, false, NULL, Empty);
  return gnu_temp;
}

/* Create a temporary variable with PREFIX and initialize it with GNU_INIT.
   Put the initialization statement into GNU_INIT_STMT and annotate it with
   the SLOC of GNAT_NODE.  Return the temporary variable.  */

static tree
create_init_temporary (const char *prefix, tree gnu_init, tree *gnu_init_stmt,
		       Node_Id gnat_node)
{
  tree gnu_temp = create_temporary (prefix, TREE_TYPE (gnu_init));

  *gnu_init_stmt = build_binary_op (INIT_EXPR, NULL_TREE, gnu_temp, gnu_init);
  set_expr_location_from_node (*gnu_init_stmt, gnat_node);

  return gnu_temp;
}

/* Subroutine of gnat_to_gnu to translate gnat_node, either an N_Function_Call
   or an N_Procedure_Call_Statement, to a GCC tree, which is returned.
   GNU_RESULT_TYPE_P is a pointer to where we should place the result type.
   If GNU_TARGET is non-null, this must be a function call on the RHS of a
   N_Assignment_Statement and the result is to be placed into that object.
   If OUTER_ATOMIC_ACCESS is true, then the assignment to GNU_TARGET must be a
   load-modify-store sequence.  Otherwise, if ATOMIC_ACCESS is true, then the
   assignment to GNU_TARGET must be atomic.  If, in addition, ATOMIC_SYNC is
   true, then the assignment to GNU_TARGET requires atomic synchronization.  */

static tree
Call_to_gnu (Node_Id gnat_node, tree *gnu_result_type_p, tree gnu_target,
	     bool outer_atomic_access, bool atomic_access, bool atomic_sync)
{
  const bool function_call = (Nkind (gnat_node) == N_Function_Call);
  const bool returning_value = (function_call && !gnu_target);
  /* The GCC node corresponding to the GNAT subprogram name.  This can either
     be a FUNCTION_DECL node if we are dealing with a standard subprogram call,
     or an indirect reference expression (an INDIRECT_REF node) pointing to a
     subprogram.  */
  tree gnu_subprog = gnat_to_gnu (Name (gnat_node));
  /* The FUNCTION_TYPE node giving the GCC type of the subprogram.  */
  tree gnu_subprog_type = TREE_TYPE (gnu_subprog);
  /* The return type of the FUNCTION_TYPE.  */
  tree gnu_result_type = TREE_TYPE (gnu_subprog_type);
  tree gnu_subprog_addr = build_unary_op (ADDR_EXPR, NULL_TREE, gnu_subprog);
  vec<tree, va_gc> *gnu_actual_vec = NULL;
  tree gnu_name_list = NULL_TREE;
  tree gnu_stmt_list = NULL_TREE;
  tree gnu_after_list = NULL_TREE;
  tree gnu_retval = NULL_TREE;
  tree gnu_call, gnu_result;
  bool went_into_elab_proc = false;
  bool pushed_binding_level = false;
  Entity_Id gnat_formal;
  Node_Id gnat_actual;
  bool sync;

  gcc_assert (TREE_CODE (gnu_subprog_type) == FUNCTION_TYPE);

  /* If we are calling a stubbed function, raise Program_Error, but Elaborate
     all our args first.  */
  if (TREE_CODE (gnu_subprog) == FUNCTION_DECL && DECL_STUBBED_P (gnu_subprog))
    {
      tree call_expr = build_call_raise (PE_Stubbed_Subprogram_Called,
					 gnat_node, N_Raise_Program_Error);

      for (gnat_actual = First_Actual (gnat_node);
	   Present (gnat_actual);
	   gnat_actual = Next_Actual (gnat_actual))
	add_stmt (gnat_to_gnu (gnat_actual));

      if (returning_value)
	{
	  *gnu_result_type_p = gnu_result_type;
	  return build1 (NULL_EXPR, gnu_result_type, call_expr);
	}

      return call_expr;
    }

  /* For a call to a nested function, check the inlining status.  */
  if (TREE_CODE (gnu_subprog) == FUNCTION_DECL
      && decl_function_context (gnu_subprog))
    check_inlining_for_nested_subprog (gnu_subprog);

  /* The only way we can be making a call via an access type is if Name is an
     explicit dereference.  In that case, get the list of formal args from the
     type the access type is pointing to.  Otherwise, get the formals from the
     entity being called.  */
  if (Nkind (Name (gnat_node)) == N_Explicit_Dereference)
    gnat_formal = First_Formal_With_Extras (Etype (Name (gnat_node)));
  else if (Nkind (Name (gnat_node)) == N_Attribute_Reference)
    /* Assume here that this must be 'Elab_Body or 'Elab_Spec.  */
    gnat_formal = Empty;
  else
    gnat_formal = First_Formal_With_Extras (Entity (Name (gnat_node)));

  /* The lifetime of the temporaries created for the call ends right after the
     return value is copied, so we can give them the scope of the elaboration
     routine at top level.  */
  if (!current_function_decl)
    {
      current_function_decl = get_elaboration_procedure ();
      went_into_elab_proc = true;
    }

  /* First, create the temporary for the return value when:

       1. There is no target and the function has copy-in/copy-out parameters,
	  because we need to preserve the return value before copying back the
	  parameters.

       2. There is no target and the call is made for neither an object nor a
	  renaming declaration, nor a return statement, and the return type has
	  variable size, because in this case the gimplifier cannot create the
	  temporary, or more generally is simply an aggregate type, because the
	  gimplifier would create the temporary in the outermost scope instead
	  of locally.

       3. There is a target and it is a slice or an array with fixed size,
	  and the return type has variable size, because the gimplifier
	  doesn't handle these cases.

       4. There is no target and we have misaligned In Out or Out parameters
	  passed by reference, because we need to preserve the return value
	  before copying back the parameters.  However, in this case, we'll
	  defer creating the temporary, see below.

     This must be done before we push a binding level around the call, since
     we will pop it before copying the return value.  */
  if (function_call
      && ((!gnu_target && TYPE_CI_CO_LIST (gnu_subprog_type))
	  || (!gnu_target
	      && Nkind (Parent (gnat_node)) != N_Object_Declaration
	      && Nkind (Parent (gnat_node)) != N_Object_Renaming_Declaration
	      && Nkind (Parent (gnat_node)) != N_Simple_Return_Statement
	      && AGGREGATE_TYPE_P (gnu_result_type)
	      && !TYPE_IS_FAT_POINTER_P (gnu_result_type))
	  || (gnu_target
	      && (TREE_CODE (gnu_target) == ARRAY_RANGE_REF
		  || (TREE_CODE (TREE_TYPE (gnu_target)) == ARRAY_TYPE
		      && TREE_CODE (TYPE_SIZE (TREE_TYPE (gnu_target)))
			 == INTEGER_CST))
	      && TREE_CODE (TYPE_SIZE (gnu_result_type)) != INTEGER_CST)))
    {
      gnu_retval = create_temporary ("R", gnu_result_type);
      DECL_RETURN_VALUE_P (gnu_retval) = 1;
    }

  /* If we don't need a value or have already created it, push a binding level
     around the call.  This will narrow the lifetime of the temporaries we may
     need to make when translating the parameters as much as possible.  */
  if (!returning_value || gnu_retval)
    {
      start_stmt_group ();
      gnat_pushlevel ();
      pushed_binding_level = true;
    }

  /* Create the list of the actual parameters as GCC expects it, namely a
     chain of TREE_LIST nodes in which the TREE_VALUE field of each node
     is an expression and the TREE_PURPOSE field is null.  But skip Out
     parameters not passed by reference and that need not be copied in.  */
  for (gnat_actual = First_Actual (gnat_node);
       Present (gnat_actual);
       gnat_formal = Next_Formal_With_Extras (gnat_formal),
       gnat_actual = Next_Actual (gnat_actual))
    {
      Entity_Id gnat_formal_type = Etype (gnat_formal);
      tree gnu_formal_type = gnat_to_gnu_type (gnat_formal_type);
      tree gnu_formal = present_gnu_tree (gnat_formal)
			? get_gnu_tree (gnat_formal) : NULL_TREE;
      const bool is_true_formal_parm
	= gnu_formal && TREE_CODE (gnu_formal) == PARM_DECL;
      const bool is_by_ref_formal_parm
	= is_true_formal_parm
	  && (DECL_BY_REF_P (gnu_formal)
	      || DECL_BY_COMPONENT_PTR_P (gnu_formal));
      /* In the Out or In Out case, we must suppress conversions that yield
	 an lvalue but can nevertheless cause the creation of a temporary,
	 because we need the real object in this case, either to pass its
	 address if it's passed by reference or as target of the back copy
	 done after the call if it uses the copy-in/copy-out mechanism.
	 We do it in the In case too, except for an unchecked conversion
	 to an elementary type or a constrained composite type because it
	 alone can cause the actual to be misaligned and the addressability
	 test is applied to the real object.  */
      const bool suppress_type_conversion
	= ((Nkind (gnat_actual) == N_Unchecked_Type_Conversion
	    && (Ekind (gnat_formal) != E_In_Parameter
		|| (Is_Composite_Type (Underlying_Type (gnat_formal_type))
		    && !Is_Constrained (Underlying_Type (gnat_formal_type)))))
	   || (Nkind (gnat_actual) == N_Type_Conversion
	       && Is_Composite_Type (Underlying_Type (gnat_formal_type))));
      Node_Id gnat_name = suppress_type_conversion
			  ? Expression (gnat_actual) : gnat_actual;
      tree gnu_name = gnat_to_gnu (gnat_name), gnu_name_type;
      tree gnu_actual;

      /* If it's possible we may need to use this expression twice, make sure
	 that any side-effects are handled via SAVE_EXPRs; likewise if we need
	 to force side-effects before the call.  */
      if (Ekind (gnat_formal) != E_In_Parameter && !is_by_ref_formal_parm)
	{
	  tree init = NULL_TREE;
	  gnu_name = gnat_stabilize_reference (gnu_name, true, &init);
	  if (init)
	    gnu_name
	      = build_compound_expr (TREE_TYPE (gnu_name), init, gnu_name);
	}

      /* If we are passing a non-addressable parameter by reference, pass the
	 address of a copy.  In the Out or In Out case, set up to copy back
	 out after the call.  */
      if (is_by_ref_formal_parm
	  && (gnu_name_type = gnat_to_gnu_type (Etype (gnat_name)))
	  && !addressable_p (gnu_name, gnu_name_type))
	{
	  bool in_param = (Ekind (gnat_formal) == E_In_Parameter);
	  tree gnu_orig = gnu_name, gnu_temp, gnu_stmt;

	  /* Do not issue warnings for CONSTRUCTORs since this is not a copy
	     but sort of an instantiation for them.  */
	  if (TREE_CODE (gnu_name) == CONSTRUCTOR)
	    ;

	  /* If the type is passed by reference, a copy is not allowed.  */
	  else if (TYPE_IS_BY_REFERENCE_P (gnu_formal_type))
	    post_error ("misaligned actual cannot be passed by reference",
		        gnat_actual);

	  /* For users of Starlet we issue a warning because the interface
	     apparently assumes that by-ref parameters outlive the procedure
	     invocation.  The code still will not work as intended, but we
	     cannot do much better since low-level parts of the back-end
	     would allocate temporaries at will because of the misalignment
	     if we did not do so here.  */
	  else if (Is_Valued_Procedure (Entity (Name (gnat_node))))
	    {
	      post_error
		("?possible violation of implicit assumption", gnat_actual);
	      post_error_ne
		("?made by pragma Import_Valued_Procedure on &", gnat_actual,
		 Entity (Name (gnat_node)));
	      post_error_ne ("?because of misalignment of &", gnat_actual,
			     gnat_formal);
	    }

	  /* If the actual type of the object is already the nominal type,
	     we have nothing to do, except if the size is self-referential
	     in which case we'll remove the unpadding below.  */
	  if (TREE_TYPE (gnu_name) == gnu_name_type
	      && !CONTAINS_PLACEHOLDER_P (TYPE_SIZE (gnu_name_type)))
	    ;

	  /* Otherwise remove the unpadding from all the objects.  */
	  else if (TREE_CODE (gnu_name) == COMPONENT_REF
		   && TYPE_IS_PADDING_P
		      (TREE_TYPE (TREE_OPERAND (gnu_name, 0))))
	    gnu_orig = gnu_name = TREE_OPERAND (gnu_name, 0);

	  /* Otherwise convert to the nominal type of the object if needed.
	     There are several cases in which we need to make the temporary
	     using this type instead of the actual type of the object when
	     they are distinct, because the expectations of the callee would
	     otherwise not be met:
	       - if it's a justified modular type,
	       - if the actual type is a smaller form of it,
	       - if it's a smaller form of the actual type.  */
	  else if ((TREE_CODE (gnu_name_type) == RECORD_TYPE
		    && (TYPE_JUSTIFIED_MODULAR_P (gnu_name_type)
		        || smaller_form_type_p (TREE_TYPE (gnu_name),
					        gnu_name_type)))
		   || (INTEGRAL_TYPE_P (gnu_name_type)
		       && smaller_form_type_p (gnu_name_type,
					       TREE_TYPE (gnu_name))))
	    gnu_name = convert (gnu_name_type, gnu_name);

	  /* If this is an In Out or Out parameter and we're returning a value,
	     we need to create a temporary for the return value because we must
	     preserve it before copying back at the very end.  */
	  if (!in_param && returning_value && !gnu_retval)
	    {
	      gnu_retval = create_temporary ("R", gnu_result_type);
	      DECL_RETURN_VALUE_P (gnu_retval) = 1;
	    }

	  /* If we haven't pushed a binding level, push it now.  This will
	     narrow the lifetime of the temporary we are about to make as
	     much as possible.  */
	  if (!pushed_binding_level && (!returning_value || gnu_retval))
	    {
	      start_stmt_group ();
	      gnat_pushlevel ();
	      pushed_binding_level = true;
	    }

	  /* Create an explicit temporary holding the copy.  */
	  gnu_temp
	    = create_init_temporary ("A", gnu_name, &gnu_stmt, gnat_actual);

	  /* But initialize it on the fly like for an implicit temporary as
	     we aren't necessarily having a statement list.  */
	  gnu_name = build_compound_expr (TREE_TYPE (gnu_name), gnu_stmt,
					  gnu_temp);

	  /* Set up to move the copy back to the original if needed.  */
	  if (!in_param)
	    {
	      /* If the original is a COND_EXPR whose first arm isn't meant to
		 be further used, just deal with the second arm.  This is very
		 likely the conditional expression built for a check.  */
	      if (TREE_CODE (gnu_orig) == COND_EXPR
		  && TREE_CODE (TREE_OPERAND (gnu_orig, 1)) == COMPOUND_EXPR
		  && integer_zerop
		     (TREE_OPERAND (TREE_OPERAND (gnu_orig, 1), 1)))
		gnu_orig = TREE_OPERAND (gnu_orig, 2);

	      gnu_stmt
		= build_binary_op (MODIFY_EXPR, NULL_TREE, gnu_orig, gnu_temp);
	      set_expr_location_from_node (gnu_stmt, gnat_node);

	      append_to_statement_list (gnu_stmt, &gnu_after_list);
	    }
	}

      /* Start from the real object and build the actual.  */
      gnu_actual = gnu_name;

      /* If atomic access is required for an In or In Out actual parameter,
	 build the atomic load.  */
      if (is_true_formal_parm
	  && !is_by_ref_formal_parm
	  && Ekind (gnat_formal) != E_Out_Parameter
	  && atomic_access_required_p (gnat_actual, &sync))
	gnu_actual = build_atomic_load (gnu_actual, sync);

      /* If this was a procedure call, we may not have removed any padding.
	 So do it here for the part we will use as an input, if any.  */
      if (Ekind (gnat_formal) != E_Out_Parameter
	  && TYPE_IS_PADDING_P (TREE_TYPE (gnu_actual)))
	gnu_actual
	  = convert (get_unpadded_type (Etype (gnat_actual)), gnu_actual);

      /* Put back the conversion we suppressed above in the computation of the
	 real object.  And even if we didn't suppress any conversion there, we
	 may have suppressed a conversion to the Etype of the actual earlier,
	 since the parent is a procedure call, so put it back here.  */
      if (suppress_type_conversion
	  && Nkind (gnat_actual) == N_Unchecked_Type_Conversion)
	gnu_actual
	  = unchecked_convert (gnat_to_gnu_type (Etype (gnat_actual)),
			       gnu_actual, No_Truncation (gnat_actual));
      else
	gnu_actual
	  = convert (gnat_to_gnu_type (Etype (gnat_actual)), gnu_actual);

      /* Make sure that the actual is in range of the formal's type.  */
      if (Ekind (gnat_formal) != E_Out_Parameter
	  && Do_Range_Check (gnat_actual))
	gnu_actual
	  = emit_range_check (gnu_actual, gnat_formal_type, gnat_actual);

      /* Unless this is an In parameter, we must remove any justified modular
	 building from GNU_NAME to get an lvalue.  */
      if (Ekind (gnat_formal) != E_In_Parameter
	  && TREE_CODE (gnu_name) == CONSTRUCTOR
	  && TREE_CODE (TREE_TYPE (gnu_name)) == RECORD_TYPE
	  && TYPE_JUSTIFIED_MODULAR_P (TREE_TYPE (gnu_name)))
	gnu_name
	  = convert (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (gnu_name))), gnu_name);

      /* First see if the parameter is passed by reference.  */
      if (is_true_formal_parm && DECL_BY_REF_P (gnu_formal))
	{
	  if (Ekind (gnat_formal) != E_In_Parameter)
	    {
	      /* In Out or Out parameters passed by reference don't use the
		 copy-in/copy-out mechanism so the address of the real object
		 must be passed to the function.  */
	      gnu_actual = gnu_name;

	      /* If we have a padded type, be sure we've removed padding.  */
	      if (TYPE_IS_PADDING_P (TREE_TYPE (gnu_actual)))
		gnu_actual = convert (get_unpadded_type (Etype (gnat_actual)),
				      gnu_actual);

	      /* If we have the constructed subtype of an aliased object
		 with an unconstrained nominal subtype, the type of the
		 actual includes the template, although it is formally
		 constrained.  So we need to convert it back to the real
		 constructed subtype to retrieve the constrained part
		 and takes its address.  */
	      if (TREE_CODE (TREE_TYPE (gnu_actual)) == RECORD_TYPE
		  && TYPE_CONTAINS_TEMPLATE_P (TREE_TYPE (gnu_actual))
		  && Is_Constr_Subt_For_UN_Aliased (Etype (gnat_actual))
		  && Is_Array_Type (Underlying_Type (Etype (gnat_actual))))
		gnu_actual = convert (gnat_to_gnu_type (Etype (gnat_actual)),
				      gnu_actual);
	    }

	  /* There is no need to convert the actual to the formal's type before
	     taking its address.  The only exception is for unconstrained array
	     types because of the way we build fat pointers.  */
	  if (TREE_CODE (gnu_formal_type) == UNCONSTRAINED_ARRAY_TYPE)
	    {
	      /* Put back a view conversion for In Out or Out parameters.  */
	      if (Ekind (gnat_formal) != E_In_Parameter)
		gnu_actual = convert (gnat_to_gnu_type (Etype (gnat_actual)),
				      gnu_actual);
	      gnu_actual = convert (gnu_formal_type, gnu_actual);
	    }

	  /* The symmetry of the paths to the type of an entity is broken here
	     since arguments don't know that they will be passed by ref.  */
	  gnu_formal_type = TREE_TYPE (gnu_formal);
	  gnu_actual = build_unary_op (ADDR_EXPR, gnu_formal_type, gnu_actual);
	}

      /* Then see if the parameter is an array passed to a foreign convention
	 subprogram.  */
      else if (is_true_formal_parm && DECL_BY_COMPONENT_PTR_P (gnu_formal))
	{
	  gnu_formal_type = TREE_TYPE (gnu_formal);
	  gnu_actual = maybe_implicit_deref (gnu_actual);
	  gnu_actual = maybe_unconstrained_array (gnu_actual);

	  if (TYPE_IS_PADDING_P (gnu_formal_type))
	    {
	      gnu_formal_type = TREE_TYPE (TYPE_FIELDS (gnu_formal_type));
	      gnu_actual = convert (gnu_formal_type, gnu_actual);
	    }

	  /* Take the address of the object and convert to the proper pointer
	     type.  We'd like to actually compute the address of the beginning
	     of the array using an ADDR_EXPR of an ARRAY_REF, but there's a
	     possibility that the ARRAY_REF might return a constant and we'd be
	     getting the wrong address.  Neither approach is exactly correct,
	     but this is the most likely to work in all cases.  */
	  gnu_actual = build_unary_op (ADDR_EXPR, gnu_formal_type, gnu_actual);
	}

      /* Otherwise the parameter is passed by copy.  */
      else
	{
	  tree gnu_size;

	  if (Ekind (gnat_formal) != E_In_Parameter)
	    gnu_name_list = tree_cons (NULL_TREE, gnu_name, gnu_name_list);

	  /* If we didn't create a PARM_DECL for the formal, this means that
	     it is an Out parameter not passed by reference and that need not
	     be copied in.  In this case, the value of the actual need not be
	     read.  However, we still need to make sure that its side-effects
	     are evaluated before the call, so we evaluate its address.  */
	  if (!is_true_formal_parm)
	    {
	      if (TREE_SIDE_EFFECTS (gnu_name))
		{
		  tree addr = build_unary_op (ADDR_EXPR, NULL_TREE, gnu_name);
		  append_to_statement_list (addr, &gnu_stmt_list);
		}
	      continue;
	    }

	  gnu_actual = convert (gnu_formal_type, gnu_actual);

	  /* If this is 'Null_Parameter, pass a zero even though we are
	     dereferencing it.  */
	  if (TREE_CODE (gnu_actual) == INDIRECT_REF
	      && TREE_PRIVATE (gnu_actual)
	      && (gnu_size = TYPE_SIZE (TREE_TYPE (gnu_actual)))
	      && TREE_CODE (gnu_size) == INTEGER_CST
	      && compare_tree_int (gnu_size, BITS_PER_WORD) <= 0)
	    {
	      tree type_for_size
		= gnat_type_for_size (TREE_INT_CST_LOW (gnu_size), 1);
	      gnu_actual
		= unchecked_convert (DECL_ARG_TYPE (gnu_formal),
				     build_int_cst (type_for_size, 0),
				     false);
	    }
	  else
	    gnu_actual = convert (DECL_ARG_TYPE (gnu_formal), gnu_actual);
	}

      vec_safe_push (gnu_actual_vec, gnu_actual);
    }

  gnu_call
    = build_call_vec (gnu_result_type, gnu_subprog_addr, gnu_actual_vec);
  set_expr_location_from_node (gnu_call, gnat_node);

  /* If we have created a temporary for the return value, initialize it.  */
  if (gnu_retval)
    {
      tree gnu_stmt
	= build_binary_op (INIT_EXPR, NULL_TREE, gnu_retval, gnu_call);
      set_expr_location_from_node (gnu_stmt, gnat_node);
      append_to_statement_list (gnu_stmt, &gnu_stmt_list);
      gnu_call = gnu_retval;
    }

  /* If this is a subprogram with copy-in/copy-out parameters, we need to
     unpack the valued returned from the function into the In Out or Out
     parameters.  We deal with the function return (if this is an Ada
     function) below.  */
  if (TYPE_CI_CO_LIST (gnu_subprog_type))
    {
      /* List of FIELD_DECLs associated with the PARM_DECLs of the copy-in/
	 copy-out parameters.  */
      tree gnu_cico_list = TYPE_CI_CO_LIST (gnu_subprog_type);
      const int length = list_length (gnu_cico_list);

      /* The call sequence must contain one and only one call, even though the
	 function is pure.  Save the result into a temporary if needed.  */
      if (length > 1)
	{
	  if (!gnu_retval)
	    {
	      tree gnu_stmt;
	      gnu_call
		= create_init_temporary ("P", gnu_call, &gnu_stmt, gnat_node);
	      append_to_statement_list (gnu_stmt, &gnu_stmt_list);
	    }

	  gnu_name_list = nreverse (gnu_name_list);
	}

      /* The first entry is for the actual return value if this is a
	 function, so skip it.  */
      if (function_call)
	gnu_cico_list = TREE_CHAIN (gnu_cico_list);

      if (Nkind (Name (gnat_node)) == N_Explicit_Dereference)
	gnat_formal = First_Formal_With_Extras (Etype (Name (gnat_node)));
      else
	gnat_formal = First_Formal_With_Extras (Entity (Name (gnat_node)));

      for (gnat_actual = First_Actual (gnat_node);
	   Present (gnat_actual);
	   gnat_formal = Next_Formal_With_Extras (gnat_formal),
	   gnat_actual = Next_Actual (gnat_actual))
	/* If we are dealing with a copy-in/copy-out parameter, we must
	   retrieve its value from the record returned in the call.  */
	if (!(present_gnu_tree (gnat_formal)
	      && TREE_CODE (get_gnu_tree (gnat_formal)) == PARM_DECL
	      && (DECL_BY_REF_P (get_gnu_tree (gnat_formal))
		  || DECL_BY_COMPONENT_PTR_P (get_gnu_tree (gnat_formal))))
	    && Ekind (gnat_formal) != E_In_Parameter)
	  {
	    /* Get the value to assign to this Out or In Out parameter.  It is
	       either the result of the function if there is only a single such
	       parameter or the appropriate field from the record returned.  */
	    tree gnu_result
	      = length == 1
		? gnu_call
		: build_component_ref (gnu_call, TREE_PURPOSE (gnu_cico_list),
				       false);

	    /* If the actual is a conversion, get the inner expression, which
	       will be the real destination, and convert the result to the
	       type of the actual parameter.  */
	    tree gnu_actual
	      = maybe_unconstrained_array (TREE_VALUE (gnu_name_list));

	    /* If the result is a padded type, remove the padding.  */
	    if (TYPE_IS_PADDING_P (TREE_TYPE (gnu_result)))
	      gnu_result
		= convert (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (gnu_result))),
			   gnu_result);

	    /* If the actual is a type conversion, the real target object is
	       denoted by the inner Expression and we need to convert the
	       result to the associated type.
	       We also need to convert our gnu assignment target to this type
	       if the corresponding GNU_NAME was constructed from the GNAT
	       conversion node and not from the inner Expression.  */
	    if (Nkind (gnat_actual) == N_Type_Conversion)
	      {
		gnu_result
		  = convert_with_check
		    (Etype (Expression (gnat_actual)), gnu_result,
		     Do_Overflow_Check (gnat_actual),
		     Do_Range_Check (Expression (gnat_actual)),
		     Float_Truncate (gnat_actual), gnat_actual);

		if (!Is_Composite_Type (Underlying_Type (Etype (gnat_formal))))
		  gnu_actual = convert (TREE_TYPE (gnu_result), gnu_actual);
	      }

	    /* Unchecked conversions as actuals for Out parameters are not
	       allowed in user code because they are not variables, but do
	       occur in front-end expansions.  The associated GNU_NAME is
	       always obtained from the inner expression in such cases.  */
	    else if (Nkind (gnat_actual) == N_Unchecked_Type_Conversion)
	      gnu_result = unchecked_convert (TREE_TYPE (gnu_actual),
					      gnu_result,
					      No_Truncation (gnat_actual));
	    else
	      {
		if (Do_Range_Check (gnat_actual))
		  gnu_result
		    = emit_range_check (gnu_result, Etype (gnat_actual),
					gnat_actual);

		if (!(!TREE_CONSTANT (TYPE_SIZE (TREE_TYPE (gnu_actual)))
		      && TREE_CONSTANT (TYPE_SIZE (TREE_TYPE (gnu_result)))))
		  gnu_result = convert (TREE_TYPE (gnu_actual), gnu_result);
	      }

	    /* If an outer atomic access is required for an actual parameter,
	       build the load-modify-store sequence.  */
	    if (outer_atomic_access_required_p (gnat_actual))
	      gnu_result
		= build_load_modify_store (gnu_actual, gnu_result, gnat_node);

	    /* Or else, if simple atomic access is required, build the atomic
	       store.  */
	    else if (atomic_access_required_p (gnat_actual, &sync))
	      gnu_result = build_atomic_store (gnu_actual, gnu_result, sync);

	    /* Otherwise build a regular assignment.  */
	    else
	      gnu_result = build_binary_op (MODIFY_EXPR, NULL_TREE,
					    gnu_actual, gnu_result);

	    if (EXPR_P (gnu_result))
	      set_expr_location_from_node (gnu_result, gnat_node);
	    append_to_statement_list (gnu_result, &gnu_stmt_list);
	    gnu_cico_list = TREE_CHAIN (gnu_cico_list);
	    gnu_name_list = TREE_CHAIN (gnu_name_list);
	  }
    }

  /* If this is a function call, the result is the call expression unless a
     target is specified, in which case we copy the result into the target
     and return the assignment statement.  */
  if (function_call)
    {
      /* If this is a function with copy-in/copy-out parameters, extract the
	 return value from it and update the return type.  */
      if (TYPE_CI_CO_LIST (gnu_subprog_type))
	{
	  tree gnu_elmt = TYPE_CI_CO_LIST (gnu_subprog_type);
	  gnu_call
	    = build_component_ref (gnu_call, TREE_PURPOSE (gnu_elmt), false);
	  gnu_result_type = TREE_TYPE (gnu_call);
	}

      /* If the function returns an unconstrained array or by direct reference,
	 we have to dereference the pointer.  */
      if (TYPE_RETURN_UNCONSTRAINED_P (gnu_subprog_type)
	  || TYPE_RETURN_BY_DIRECT_REF_P (gnu_subprog_type))
	gnu_call = build_unary_op (INDIRECT_REF, NULL_TREE, gnu_call);

      if (gnu_target)
	{
	  Node_Id gnat_parent = Parent (gnat_node);
	  enum tree_code op_code;

	  /* If range check is needed, emit code to generate it.  */
	  if (Do_Range_Check (gnat_node))
	    gnu_call
	      = emit_range_check (gnu_call, Etype (Name (gnat_parent)),
				  gnat_parent);

	  /* ??? If the return type has variable size, then force the return
	     slot optimization as we would not be able to create a temporary.
	     That's what has been done historically.  */
	  if (return_type_with_variable_size_p (gnu_result_type))
	    op_code = INIT_EXPR;
	  else
	    op_code = MODIFY_EXPR;

	  /* Use the required method to move the result to the target.  */
	  if (outer_atomic_access)
	    gnu_call
	      = build_load_modify_store (gnu_target, gnu_call, gnat_node);
	  else if (atomic_access)
	    gnu_call = build_atomic_store (gnu_target, gnu_call, atomic_sync);
	  else
	    gnu_call
	      = build_binary_op (op_code, NULL_TREE, gnu_target, gnu_call);

	  if (EXPR_P (gnu_call))
	    set_expr_location_from_node (gnu_call, gnat_parent);
	  append_to_statement_list (gnu_call, &gnu_stmt_list);
	}
      else
	*gnu_result_type_p = get_unpadded_type (Etype (gnat_node));
    }

  /* Otherwise, if this is a procedure call statement without copy-in/copy-out
     parameters, the result is just the call statement.  */
  else if (!TYPE_CI_CO_LIST (gnu_subprog_type))
    append_to_statement_list (gnu_call, &gnu_stmt_list);

  /* Finally, add the copy back statements, if any.  */
  append_to_statement_list (gnu_after_list, &gnu_stmt_list);

  if (went_into_elab_proc)
    current_function_decl = NULL_TREE;

  /* If we have pushed a binding level, pop it and finish up the enclosing
     statement group.  */
  if (pushed_binding_level)
    {
      add_stmt (gnu_stmt_list);
      gnat_poplevel ();
      gnu_result = end_stmt_group ();
    }

  /* Otherwise, retrieve the statement list, if any.  */
  else if (gnu_stmt_list)
    gnu_result = gnu_stmt_list;

  /* Otherwise, just return the call expression.  */
  else
    return gnu_call;

  /* If we nevertheless need a value, make a COMPOUND_EXPR to return it.
     But first simplify if we have only one statement in the list.  */
  if (returning_value)
    {
      tree first = expr_first (gnu_result), last = expr_last (gnu_result);
      if (first == last)
	gnu_result = first;
      gnu_result
	= build_compound_expr (TREE_TYPE (gnu_call), gnu_result, gnu_call);
    }

  return gnu_result;
}

/* Subroutine of gnat_to_gnu to translate gnat_node, an
   N_Handled_Sequence_Of_Statements, to a GCC tree, which is returned.  */

static tree
Handled_Sequence_Of_Statements_to_gnu (Node_Id gnat_node)
{
  tree gnu_jmpsave_decl = NULL_TREE;
  tree gnu_jmpbuf_decl = NULL_TREE;
  /* If just annotating, ignore all EH and cleanups.  */
  bool gcc_eh = (!type_annotate_only
                 && Present (Exception_Handlers (gnat_node))
                 && Back_End_Exceptions ());
  bool fe_sjlj
    = (!type_annotate_only && Present (Exception_Handlers (gnat_node))
       && Exception_Mechanism == Front_End_SJLJ);
  bool at_end = !type_annotate_only && Present (At_End_Proc (gnat_node));
  bool binding_for_block = (at_end || gcc_eh || fe_sjlj);
  tree gnu_inner_block; /* The statement(s) for the block itself.  */
  tree gnu_result;
  tree gnu_expr;
  Node_Id gnat_temp;
  /* Node providing the sloc for the cleanup actions.  */
  Node_Id gnat_cleanup_loc_node = (Present (End_Label (gnat_node)) ?
                                   End_Label (gnat_node) :
                                   gnat_node);

  /* The GCC exception handling mechanism can handle both ZCX and SJLJ schemes
     and we have our own SJLJ mechanism.  To call the GCC mechanism, we call
     add_cleanup, and when we leave the binding, end_stmt_group will create
     the TRY_FINALLY_EXPR.

     ??? The region level calls down there have been specifically put in place
     for a ZCX context and currently the order in which things are emitted
     (region/handlers) is different from the SJLJ case. Instead of putting
     other calls with different conditions at other places for the SJLJ case,
     it seems cleaner to reorder things for the SJLJ case and generalize the
     condition to make it not ZCX specific.

     If there are any exceptions or cleanup processing involved, we need an
     outer statement group (for Fe_Sjlj) and binding level.  */
  if (binding_for_block)
    {
      start_stmt_group ();
      gnat_pushlevel ();
    }

  /* If using fe_sjlj, make the variables for the setjmp buffer and save
     area for address of previous buffer.  Do this first since we need to have
     the setjmp buf known for any decls in this block.  */
  if (fe_sjlj)
    {
      gnu_jmpsave_decl
	= create_var_decl (get_identifier ("JMPBUF_SAVE"), NULL_TREE,
			   jmpbuf_ptr_type,
			   build_call_n_expr (get_jmpbuf_decl, 0),
			   false, false, false, false, false, true, false,
			   NULL, gnat_node);

      /* The __builtin_setjmp receivers will immediately reinstall it.  Now
	 because of the unstructured form of EH used by fe_sjlj, there
	 might be forward edges going to __builtin_setjmp receivers on which
	 it is uninitialized, although they will never be actually taken.  */
      TREE_NO_WARNING (gnu_jmpsave_decl) = 1;
      gnu_jmpbuf_decl
	= create_var_decl (get_identifier ("JMP_BUF"), NULL_TREE,
			   jmpbuf_type,
			   NULL_TREE,
			   false, false, false, false, false, true, false,
			   NULL, gnat_node);

      set_block_jmpbuf_decl (gnu_jmpbuf_decl);

      /* When we exit this block, restore the saved value.  */
      add_cleanup (build_call_n_expr (set_jmpbuf_decl, 1, gnu_jmpsave_decl),
		   gnat_cleanup_loc_node);
    }

  /* If we are to call a function when exiting this block, add a cleanup
     to the binding level we made above.  Note that add_cleanup is FIFO
     so we must register this cleanup after the EH cleanup just above.  */
  if (at_end)
    {
      tree proc_decl = gnat_to_gnu (At_End_Proc (gnat_node));
      /* When not optimizing, disable inlining of finalizers as this can
	 create a more complex CFG in the parent function.  */
      if (!optimize)
	DECL_DECLARED_INLINE_P (proc_decl) = 0;
      add_cleanup (build_call_n_expr (proc_decl, 0), gnat_cleanup_loc_node);
    }

  /* Now build the tree for the declarations and statements inside this block.
     If this is SJLJ, set our jmp_buf as the current buffer.  */
  start_stmt_group ();

  if (fe_sjlj)
    {
      gnu_expr = build_call_n_expr (set_jmpbuf_decl, 1,
				    build_unary_op (ADDR_EXPR, NULL_TREE,
						    gnu_jmpbuf_decl));
      set_expr_location_from_node (gnu_expr, gnat_node);
      add_stmt (gnu_expr);
    }

  if (Present (First_Real_Statement (gnat_node)))
    process_decls (Statements (gnat_node), Empty,
		   First_Real_Statement (gnat_node), true, true);

  /* Generate code for each statement in the block.  */
  for (gnat_temp = (Present (First_Real_Statement (gnat_node))
		    ? First_Real_Statement (gnat_node)
		    : First (Statements (gnat_node)));
       Present (gnat_temp); gnat_temp = Next (gnat_temp))
    add_stmt (gnat_to_gnu (gnat_temp));
  gnu_inner_block = end_stmt_group ();

  /* Now generate code for the two exception models, if either is relevant for
     this block.  */
  if (fe_sjlj)
    {
      tree *gnu_else_ptr = 0;
      tree gnu_handler;

      /* Make a binding level for the exception handling declarations and code
	 and set up gnu_except_ptr_stack for the handlers to use.  */
      start_stmt_group ();
      gnat_pushlevel ();

      vec_safe_push (gnu_except_ptr_stack,
		     create_var_decl (get_identifier ("EXCEPT_PTR"), NULL_TREE,
				      build_pointer_type (except_type_node),
				      build_call_n_expr (get_excptr_decl, 0),
				      false, false, false, false, false,
				      true, false, NULL, gnat_node));

      /* Generate code for each handler. The N_Exception_Handler case does the
	 real work and returns a COND_EXPR for each handler, which we chain
	 together here.  */
      for (gnat_temp = First_Non_Pragma (Exception_Handlers (gnat_node));
	   Present (gnat_temp); gnat_temp = Next_Non_Pragma (gnat_temp))
	{
	  gnu_expr = gnat_to_gnu (gnat_temp);

	  /* If this is the first one, set it as the outer one. Otherwise,
	     point the "else" part of the previous handler to us. Then point
	     to our "else" part.  */
	  if (!gnu_else_ptr)
	    add_stmt (gnu_expr);
	  else
	    *gnu_else_ptr = gnu_expr;

	  gnu_else_ptr = &COND_EXPR_ELSE (gnu_expr);
	}

      /* If none of the exception handlers did anything, re-raise but do not
	 defer abortion.  */
      gnu_expr = build_call_n_expr (raise_nodefer_decl, 1,
				    gnu_except_ptr_stack->last ());
      set_expr_location_from_node
	(gnu_expr,
	 Present (End_Label (gnat_node)) ? End_Label (gnat_node) : gnat_node);

      if (gnu_else_ptr)
	*gnu_else_ptr = gnu_expr;
      else
	add_stmt (gnu_expr);

      /* End the binding level dedicated to the exception handlers and get the
	 whole statement group.  */
      gnu_except_ptr_stack->pop ();
      gnat_poplevel ();
      gnu_handler = end_stmt_group ();

      /* If the setjmp returns 1, we restore our incoming longjmp value and
	 then check the handlers.  */
      start_stmt_group ();
      add_stmt_with_node (build_call_n_expr (set_jmpbuf_decl, 1,
					     gnu_jmpsave_decl),
			  gnat_node);
      add_stmt (gnu_handler);
      gnu_handler = end_stmt_group ();

      /* This block is now "if (setjmp) ... <handlers> else <block>".  */
      gnu_result = build3 (COND_EXPR, void_type_node,
			   (build_call_n_expr
			    (setjmp_decl, 1,
			     build_unary_op (ADDR_EXPR, NULL_TREE,
					     gnu_jmpbuf_decl))),
			   gnu_handler, gnu_inner_block);
    }
  else if (gcc_eh)
    {
      tree gnu_handlers;
      location_t locus;

      /* First make a block containing the handlers.  */
      start_stmt_group ();
      for (gnat_temp = First_Non_Pragma (Exception_Handlers (gnat_node));
	   Present (gnat_temp);
	   gnat_temp = Next_Non_Pragma (gnat_temp))
	add_stmt (gnat_to_gnu (gnat_temp));
      gnu_handlers = end_stmt_group ();

      /* Now make the TRY_CATCH_EXPR for the block.  */
      gnu_result = build2 (TRY_CATCH_EXPR, void_type_node,
			   gnu_inner_block, gnu_handlers);
      /* Set a location.  We need to find a unique location for the dispatching
	 code, otherwise we can get coverage or debugging issues.  Try with
	 the location of the end label.  */
      if (Present (End_Label (gnat_node))
	  && Sloc_to_locus (Sloc (End_Label (gnat_node)), &locus))
	SET_EXPR_LOCATION (gnu_result, locus);
      else
        /* Clear column information so that the exception handler of an
           implicit transient block does not incorrectly inherit the slocs
           of a decision, which would otherwise confuse control flow based
           coverage analysis tools.  */
	set_expr_location_from_node (gnu_result, gnat_node, true);
    }
  else
    gnu_result = gnu_inner_block;

  /* Now close our outer block, if we had to make one.  */
  if (binding_for_block)
    {
      add_stmt (gnu_result);
      gnat_poplevel ();
      gnu_result = end_stmt_group ();
    }

  return gnu_result;
}

/* Subroutine of gnat_to_gnu to translate gnat_node, an N_Exception_Handler,
   to a GCC tree, which is returned.  This is the variant for front-end sjlj
   exception handling.  */

static tree
Exception_Handler_to_gnu_fe_sjlj (Node_Id gnat_node)
{
  /* Unless this is "Others" or the special "Non-Ada" exception for Ada, make
     an "if" statement to select the proper exceptions.  For "Others", exclude
     exceptions where Handled_By_Others is nonzero unless the All_Others flag
     is set. For "Non-ada", accept an exception if "Lang" is 'V'.  */
  tree gnu_choice = boolean_false_node;
  tree gnu_body = build_stmt_group (Statements (gnat_node), false);
  Node_Id gnat_temp;

  for (gnat_temp = First (Exception_Choices (gnat_node));
       gnat_temp; gnat_temp = Next (gnat_temp))
    {
      tree this_choice;

      if (Nkind (gnat_temp) == N_Others_Choice)
	{
	  if (All_Others (gnat_temp))
	    this_choice = boolean_true_node;
	  else
	    this_choice
	      = build_binary_op
		(EQ_EXPR, boolean_type_node,
		 convert
		 (integer_type_node,
		  build_component_ref
		  (build_unary_op
		   (INDIRECT_REF, NULL_TREE,
		    gnu_except_ptr_stack->last ()),
		   not_handled_by_others_decl,
		   false)),
		 integer_zero_node);
	}

      else if (Nkind (gnat_temp) == N_Identifier
	       || Nkind (gnat_temp) == N_Expanded_Name)
	{
	  Entity_Id gnat_ex_id = Entity (gnat_temp);
	  tree gnu_expr;

	  /* Exception may be a renaming. Recover original exception which is
	     the one elaborated and registered.  */
	  if (Present (Renamed_Object (gnat_ex_id)))
	    gnat_ex_id = Renamed_Object (gnat_ex_id);

	  gnu_expr = gnat_to_gnu_entity (gnat_ex_id, NULL_TREE, false);

	  this_choice
	    = build_binary_op
	      (EQ_EXPR, boolean_type_node,
	       gnu_except_ptr_stack->last (),
	       convert (TREE_TYPE (gnu_except_ptr_stack->last ()),
			build_unary_op (ADDR_EXPR, NULL_TREE, gnu_expr)));
}
      else
	gcc_unreachable ();

      gnu_choice = build_binary_op (TRUTH_ORIF_EXPR, boolean_type_node,
				    gnu_choice, this_choice);
    }

  return build3 (COND_EXPR, void_type_node, gnu_choice, gnu_body, NULL_TREE);
}

/* Subroutine of gnat_to_gnu to translate gnat_node, an N_Exception_Handler,
   to a GCC tree, which is returned.  This is the variant for GCC exception
   schemes.  */

static tree
Exception_Handler_to_gnu_gcc (Node_Id gnat_node)
{
  tree gnu_etypes_list = NULL_TREE;
  tree gnu_current_exc_ptr, prev_gnu_incoming_exc_ptr;
  Node_Id gnat_temp;

  /* We build a TREE_LIST of nodes representing what exception types this
     handler can catch, with special cases for others and all others cases.

     Each exception type is actually identified by a pointer to the exception
     id, or to a dummy object for "others" and "all others".  */
  for (gnat_temp = First (Exception_Choices (gnat_node));
       gnat_temp; gnat_temp = Next (gnat_temp))
    {
      tree gnu_expr, gnu_etype;

      if (Nkind (gnat_temp) == N_Others_Choice)
	{
	  gnu_expr = All_Others (gnat_temp) ? all_others_decl : others_decl;
	  gnu_etype = build_unary_op (ADDR_EXPR, NULL_TREE, gnu_expr);
	}
      else if (Nkind (gnat_temp) == N_Identifier
	       || Nkind (gnat_temp) == N_Expanded_Name)
	{
	  Entity_Id gnat_ex_id = Entity (gnat_temp);

	  /* Exception may be a renaming.  Recover original exception which is
	     the one elaborated and registered.  */
	  if (Present (Renamed_Object (gnat_ex_id)))
	    gnat_ex_id = Renamed_Object (gnat_ex_id);

	  gnu_expr = gnat_to_gnu_entity (gnat_ex_id, NULL_TREE, false);
	  gnu_etype = build_unary_op (ADDR_EXPR, NULL_TREE, gnu_expr);
	}
      else
	gcc_unreachable ();

      /* The GCC interface expects NULL to be passed for catch all handlers, so
	 it would be quite tempting to set gnu_etypes_list to NULL if gnu_etype
	 is integer_zero_node.  It would not work, however, because GCC's
	 notion of "catch all" is stronger than our notion of "others".  Until
	 we correctly use the cleanup interface as well, doing that would
	 prevent the "all others" handlers from being seen, because nothing
	 can be caught beyond a catch all from GCC's point of view.  */
      gnu_etypes_list = tree_cons (NULL_TREE, gnu_etype, gnu_etypes_list);
    }

  start_stmt_group ();
  gnat_pushlevel ();

  /* Expand a call to the begin_handler hook at the beginning of the handler,
     and arrange for a call to the end_handler hook to occur on every possible
     exit path.

     The hooks expect a pointer to the low level occurrence. This is required
     for our stack management scheme because a raise inside the handler pushes
     a new occurrence on top of the stack, which means that this top does not
     necessarily match the occurrence this handler was dealing with.

     __builtin_eh_pointer references the exception occurrence being
     propagated. Upon handler entry, this is the exception for which the
     handler is triggered. This might not be the case upon handler exit,
     however, as we might have a new occurrence propagated by the handler's
     body, and the end_handler hook called as a cleanup in this context.

     We use a local variable to retrieve the incoming value at handler entry
     time, and reuse it to feed the end_handler hook's argument at exit.  */

  gnu_current_exc_ptr
    = build_call_expr (builtin_decl_explicit (BUILT_IN_EH_POINTER),
		       1, integer_zero_node);
  prev_gnu_incoming_exc_ptr = gnu_incoming_exc_ptr;
  gnu_incoming_exc_ptr
    = create_var_decl (get_identifier ("EXPTR"), NULL_TREE,
		       ptr_type_node, gnu_current_exc_ptr,
		       false, false, false, false, false, true, true,
		       NULL, gnat_node);

  add_stmt_with_node (build_call_n_expr (begin_handler_decl, 1,
					 gnu_incoming_exc_ptr),
		      gnat_node);

  /* Declare and initialize the choice parameter, if present.  */
  if (Present (Choice_Parameter (gnat_node)))
    {
      tree gnu_param
	= gnat_to_gnu_entity (Choice_Parameter (gnat_node), NULL_TREE, true);

      add_stmt (build_call_n_expr
		(set_exception_parameter_decl, 2,
		 build_unary_op (ADDR_EXPR, NULL_TREE, gnu_param),
		 gnu_incoming_exc_ptr));
    }

  /* We don't have an End_Label at hand to set the location of the cleanup
     actions, so we use that of the exception handler itself instead.  */
  add_cleanup (build_call_n_expr (end_handler_decl, 1, gnu_incoming_exc_ptr),
	       gnat_node);
  add_stmt_list (Statements (gnat_node));
  gnat_poplevel ();

  gnu_incoming_exc_ptr = prev_gnu_incoming_exc_ptr;

  return
    build2 (CATCH_EXPR, void_type_node, gnu_etypes_list, end_stmt_group ());
}

/* Subroutine of gnat_to_gnu to generate code for an N_Compilation unit.  */

static void
Compilation_Unit_to_gnu (Node_Id gnat_node)
{
  const Node_Id gnat_unit = Unit (gnat_node);
  const bool body_p = (Nkind (gnat_unit) == N_Package_Body
		       || Nkind (gnat_unit) == N_Subprogram_Body);
  const Entity_Id gnat_unit_entity = Defining_Entity (gnat_unit);
  Entity_Id gnat_entity;
  Node_Id gnat_pragma;
  /* Make the decl for the elaboration procedure.  */
  tree gnu_elab_proc_decl
    = create_subprog_decl
      (create_concat_name (gnat_unit_entity, body_p ? "elabb" : "elabs"),
       NULL_TREE, void_ftype, NULL_TREE,
       is_disabled, true, false, true, true, NULL, gnat_unit);
  struct elab_info *info;

  vec_safe_push (gnu_elab_proc_stack, gnu_elab_proc_decl);
  DECL_ELABORATION_PROC_P (gnu_elab_proc_decl) = 1;

  /* Initialize the information structure for the function.  */
  allocate_struct_function (gnu_elab_proc_decl, false);
  set_cfun (NULL);

  current_function_decl = NULL_TREE;

  start_stmt_group ();
  gnat_pushlevel ();

  /* For a body, first process the spec if there is one.  */
  if (Nkind (gnat_unit) == N_Package_Body
      || (Nkind (gnat_unit) == N_Subprogram_Body && !Acts_As_Spec (gnat_node)))
    add_stmt (gnat_to_gnu (Library_Unit (gnat_node)));

  if (type_annotate_only && gnat_node == Cunit (Main_Unit))
    {
      elaborate_all_entities (gnat_node);

      if (Nkind (gnat_unit) == N_Subprogram_Declaration
	  || Nkind (gnat_unit) == N_Generic_Package_Declaration
	  || Nkind (gnat_unit) == N_Generic_Subprogram_Declaration)
	return;
    }

  /* Then process any pragmas and declarations preceding the unit.  */
  for (gnat_pragma = First (Context_Items (gnat_node));
       Present (gnat_pragma);
       gnat_pragma = Next (gnat_pragma))
    if (Nkind (gnat_pragma) == N_Pragma)
      add_stmt (gnat_to_gnu (gnat_pragma));
  process_decls (Declarations (Aux_Decls_Node (gnat_node)), Empty, Empty,
		 true, true);

  /* Process the unit itself.  */
  add_stmt (gnat_to_gnu (gnat_unit));

  /* Generate code for all the inlined subprograms.  */
  for (gnat_entity = First_Inlined_Subprogram (gnat_node);
       Present (gnat_entity);
       gnat_entity = Next_Inlined_Subprogram (gnat_entity))
    {
      Node_Id gnat_body;

      /* Without optimization, process only the required subprograms.  */
      if (!optimize && !Has_Pragma_Inline_Always (gnat_entity))
	continue;

      gnat_body = Parent (Declaration_Node (gnat_entity));
      if (Nkind (gnat_body) != N_Subprogram_Body)
	{
	  /* ??? This happens when only the spec of a package is provided.  */
	  if (No (Corresponding_Body (gnat_body)))
	    continue;

	  gnat_body
	    = Parent (Declaration_Node (Corresponding_Body (gnat_body)));
	}

      /* Define the entity first so we set DECL_EXTERNAL.  */
      gnat_to_gnu_entity (gnat_entity, NULL_TREE, false);
      add_stmt (gnat_to_gnu (gnat_body));
    }

  /* Process any pragmas and actions following the unit.  */
  add_stmt_list (Pragmas_After (Aux_Decls_Node (gnat_node)));
  add_stmt_list (Actions (Aux_Decls_Node (gnat_node)));
  finalize_from_limited_with ();

  /* Save away what we've made so far and finish it up.  */
  set_current_block_context (gnu_elab_proc_decl);
  gnat_poplevel ();
  DECL_SAVED_TREE (gnu_elab_proc_decl) = end_stmt_group ();
  set_end_locus_from_node (gnu_elab_proc_decl, gnat_unit);
  gnu_elab_proc_stack->pop ();

  /* Record this potential elaboration procedure for later processing.  */
  info = ggc_alloc<elab_info> ();
  info->next = elab_info_list;
  info->elab_proc = gnu_elab_proc_decl;
  info->gnat_node = gnat_node;
  elab_info_list = info;

  /* Force the processing for all nodes that remain in the queue.  */
  process_deferred_decl_context (true);
}

/* Mark COND, a boolean expression, as predicating a call to a noreturn
   function, i.e. predict that it is very likely false, and return it.

   The compiler will automatically predict the last edge leading to a call
   to a noreturn function as very unlikely taken.  This function makes it
   possible to expand the prediction to predecessors in case the condition
   is made up of several short-circuit operators.  */

static tree
build_noreturn_cond (tree cond)
{
  tree fn = builtin_decl_explicit (BUILT_IN_EXPECT);
  tree arg_types = TYPE_ARG_TYPES (TREE_TYPE (fn));
  tree pred_type = TREE_VALUE (arg_types);
  tree expected_type = TREE_VALUE (TREE_CHAIN (arg_types));

  tree t = build_call_expr (fn, 3,
			    fold_convert (pred_type, cond),
			    build_int_cst (expected_type, 0),
			    build_int_cst (integer_type_node,
					   PRED_NORETURN));

  return build1 (NOP_EXPR, boolean_type_node, t);
}

/* Subroutine of gnat_to_gnu to translate GNAT_RANGE, a node representing a
   range of values, into GNU_LOW and GNU_HIGH bounds.  */

static void
Range_to_gnu (Node_Id gnat_range, tree *gnu_low, tree *gnu_high)
{
  /* GNAT_RANGE is either an N_Range or an identifier denoting a subtype.  */
  switch (Nkind (gnat_range))
    {
    case N_Range:
      *gnu_low = gnat_to_gnu (Low_Bound (gnat_range));
      *gnu_high = gnat_to_gnu (High_Bound (gnat_range));
      break;

    case N_Expanded_Name:
    case N_Identifier:
      {
	tree gnu_range_type = get_unpadded_type (Entity (gnat_range));
	tree gnu_range_base_type = get_base_type (gnu_range_type);

	*gnu_low
	  = convert (gnu_range_base_type, TYPE_MIN_VALUE (gnu_range_type));
	*gnu_high
	  = convert (gnu_range_base_type, TYPE_MAX_VALUE (gnu_range_type));
      }
      break;

    default:
      gcc_unreachable ();
    }
}

/* Subroutine of gnat_to_gnu to translate GNAT_NODE, an N_Raise_xxx_Error,
   to a GCC tree and return it.  GNU_RESULT_TYPE_P is a pointer to where
   we should place the result type.  */

static tree
Raise_Error_to_gnu (Node_Id gnat_node, tree *gnu_result_type_p)
{
  const Node_Kind kind = Nkind (gnat_node);
  const int reason = UI_To_Int (Reason (gnat_node));
  const Node_Id gnat_cond = Condition (gnat_node);
  const bool with_extra_info
    = Exception_Extra_Info
      && !No_Exception_Handlers_Set ()
      && !get_exception_label (kind);
  tree gnu_result = NULL_TREE, gnu_cond = NULL_TREE;

  /* The following processing is not required for correctness.  Its purpose is
     to give more precise error messages and to record some information.  */
  switch (reason)
    {
    case CE_Access_Check_Failed:
      if (with_extra_info)
	gnu_result = build_call_raise_column (reason, gnat_node, kind);
      break;

    case CE_Index_Check_Failed:
    case CE_Range_Check_Failed:
    case CE_Invalid_Data:
      if (Present (gnat_cond) && Nkind (gnat_cond) == N_Op_Not)
	{
	  Node_Id gnat_index, gnat_type;
	  tree gnu_type, gnu_index, gnu_low_bound, gnu_high_bound, disp;
	  bool neg_p;
	  struct loop_info_d *loop;

	  switch (Nkind (Right_Opnd (gnat_cond)))
	    {
	    case N_In:
	      Range_to_gnu (Right_Opnd (Right_Opnd (gnat_cond)),
			    &gnu_low_bound, &gnu_high_bound);
	      break;

	    case N_Op_Ge:
	      gnu_low_bound = gnat_to_gnu (Right_Opnd (Right_Opnd (gnat_cond)));
	      gnu_high_bound = NULL_TREE;
	      break;

	    case N_Op_Le:
	      gnu_low_bound = NULL_TREE;
	      gnu_high_bound = gnat_to_gnu (Right_Opnd (Right_Opnd (gnat_cond)));
	      break;

	    default:
	      goto common;
	    }

	  gnat_index = Left_Opnd (Right_Opnd (gnat_cond));
	  gnat_type = Etype (gnat_index);
	  gnu_type = maybe_character_type (get_unpadded_type (gnat_type));
	  gnu_index = gnat_to_gnu (gnat_index);

	  if (TREE_TYPE (gnu_index) != gnu_type)
	    {
	      if (gnu_low_bound)
		gnu_low_bound = convert (gnu_type, gnu_low_bound);
	      if (gnu_high_bound)
		gnu_high_bound = convert (gnu_type, gnu_high_bound);
	      gnu_index = convert (gnu_type, gnu_index);
	    }

	  if (with_extra_info
	      && gnu_low_bound
	      && gnu_high_bound
	      && Known_Esize (gnat_type)
	      && UI_To_Int (Esize (gnat_type)) <= 32)
	    gnu_result
	      = build_call_raise_range (reason, gnat_node, kind, gnu_index,
					gnu_low_bound, gnu_high_bound);

	  /* If optimization is enabled and we are inside a loop, we try to
	     compute invariant conditions for checks applied to the iteration
	     variable, i.e. conditions that are independent of the variable
	     and necessary in order for the checks to fail in the course of
	     some iteration.  If we succeed, we consider an alternative:

	       1. If loop unswitching is enabled, we prepend these conditions
		  to the original conditions of the checks.  This will make it
		  possible for the loop unswitching pass to replace the loop
		  with two loops, one of which has the checks eliminated and
		  the other has the original checks reinstated, and a prologue
		  implementing a run-time selection.  The former loop will be
		  for example suitable for vectorization.

	       2. Otherwise, we instead append the conditions to the original
		  conditions of the checks.  At worse, if the conditions cannot
		  be evaluated at compile time, they will be evaluated as true
		  at run time only when the checks have already failed, thus
		  contributing negatively only to the size of the executable.
		  But the hope is that these invariant conditions be evaluated
		  at compile time to false, thus taking away the entire checks
		  with them.  */
	  if (optimize
	      && inside_loop_p ()
	      && (!gnu_low_bound
		  || (gnu_low_bound = gnat_invariant_expr (gnu_low_bound)))
	      && (!gnu_high_bound
		  || (gnu_high_bound = gnat_invariant_expr (gnu_high_bound)))
	      && (loop = find_loop_for (gnu_index, &disp, &neg_p)))
	    {
	      struct range_check_info_d *rci = ggc_alloc<range_check_info_d> ();
	      rci->low_bound = gnu_low_bound;
	      rci->high_bound = gnu_high_bound;
	      rci->disp = disp;
	      rci->neg_p = neg_p;
	      rci->type = gnu_type;
	      rci->inserted_cond
		= build1 (SAVE_EXPR, boolean_type_node, boolean_true_node);
	      vec_safe_push (loop->checks, rci);
	      loop->has_checks = true;
	      gnu_cond = build_noreturn_cond (gnat_to_gnu (gnat_cond));
	      if (flag_unswitch_loops)
		gnu_cond = build_binary_op (TRUTH_ANDIF_EXPR,
					    boolean_type_node,
					    rci->inserted_cond,
					    gnu_cond);
	      else
		gnu_cond = build_binary_op (TRUTH_ANDIF_EXPR,
					    boolean_type_node,
					    gnu_cond,
					    rci->inserted_cond);
	    }

	  /* Or else, if aggressive loop optimizations are enabled, we just
	     record that there are checks applied to iteration variables.  */
	  else if (optimize
		   && flag_aggressive_loop_optimizations
		   && inside_loop_p ()
		   && (loop = find_loop_for (gnu_index)))
	    loop->has_checks = true;
	}
      break;

    default:
      break;
    }

  /* The following processing does the common work.  */
common:
  if (!gnu_result)
    gnu_result = build_call_raise (reason, gnat_node, kind);
  set_expr_location_from_node (gnu_result, gnat_node);

  *gnu_result_type_p = get_unpadded_type (Etype (gnat_node));

  /* If the type is VOID, this is a statement, so we need to generate the code
     for the call.  Handle a condition, if there is one.  */
  if (VOID_TYPE_P (*gnu_result_type_p))
    {
      if (Present (gnat_cond))
	{
	  if (!gnu_cond)
	    gnu_cond = gnat_to_gnu (gnat_cond);
	  gnu_result = build3 (COND_EXPR, void_type_node, gnu_cond, gnu_result,
			       alloc_stmt_list ());
	}
    }
  else
    gnu_result = build1 (NULL_EXPR, *gnu_result_type_p, gnu_result);

  return gnu_result;
}

/* Return true if GNAT_NODE is on the LHS of an assignment or an actual
   parameter of a call.  */

static bool
lhs_or_actual_p (Node_Id gnat_node)
{
  Node_Id gnat_parent = Parent (gnat_node);
  Node_Kind kind = Nkind (gnat_parent);

  if (kind == N_Assignment_Statement && Name (gnat_parent) == gnat_node)
    return true;

  if ((kind == N_Procedure_Call_Statement || kind == N_Function_Call)
      && Name (gnat_parent) != gnat_node)
    return true;

  if (kind == N_Parameter_Association)
    return true;

  return false;
}

/* Return true if either GNAT_NODE or a view of GNAT_NODE is on the LHS
   of an assignment or an actual parameter of a call.  */

static bool
present_in_lhs_or_actual_p (Node_Id gnat_node)
{
  Node_Kind kind;

  if (lhs_or_actual_p (gnat_node))
    return true;

  kind = Nkind (Parent (gnat_node));

  if ((kind == N_Type_Conversion || kind == N_Unchecked_Type_Conversion)
      && lhs_or_actual_p (Parent (gnat_node)))
    return true;

  return false;
}

/* Return true if GNAT_NODE, an unchecked type conversion, is a no-op as far
   as gigi is concerned.  This is used to avoid conversions on the LHS.  */

static bool
unchecked_conversion_nop (Node_Id gnat_node)
{
  Entity_Id from_type, to_type;

  /* The conversion must be on the LHS of an assignment or an actual parameter
     of a call.  Otherwise, even if the conversion was essentially a no-op, it
     could de facto ensure type consistency and this should be preserved.  */
  if (!lhs_or_actual_p (gnat_node))
    return false;

  from_type = Etype (Expression (gnat_node));

  /* We're interested in artificial conversions generated by the front-end
     to make private types explicit, e.g. in Expand_Assign_Array.  */
  if (!Is_Private_Type (from_type))
    return false;

  from_type = Underlying_Type (from_type);
  to_type = Etype (gnat_node);

  /* The direct conversion to the underlying type is a no-op.  */
  if (to_type == from_type)
    return true;

  /* For an array subtype, the conversion to the PAIT is a no-op.  */
  if (Ekind (from_type) == E_Array_Subtype
      && to_type == Packed_Array_Impl_Type (from_type))
    return true;

  /* For a record subtype, the conversion to the type is a no-op.  */
  if (Ekind (from_type) == E_Record_Subtype
      && to_type == Etype (from_type))
    return true;

  return false;
}

/* Return true if GNAT_NODE represents a statement.  */

static bool
statement_node_p (Node_Id gnat_node)
{
  const Node_Kind kind = Nkind (gnat_node);

  if (kind == N_Label)
    return true;

  if (IN (kind, N_Statement_Other_Than_Procedure_Call))
    return true;

  if (kind == N_Procedure_Call_Statement)
    return true;

  if (IN (kind, N_Raise_xxx_Error) && Ekind (Etype (gnat_node)) == E_Void)
    return true;

  return false;
}

/* This function is the driver of the GNAT to GCC tree transformation process.
   It is the entry point of the tree transformer.  GNAT_NODE is the root of
   some GNAT tree.  Return the root of the corresponding GCC tree.  If this
   is an expression, return the GCC equivalent of the expression.  If this
   is a statement, return the statement or add it to the current statement
   group, in which case anything returned is to be interpreted as occurring
   after anything added.  */

tree
gnat_to_gnu (Node_Id gnat_node)
{
  const Node_Kind kind = Nkind (gnat_node);
  bool went_into_elab_proc = false;
  tree gnu_result = error_mark_node; /* Default to no value.  */
  tree gnu_result_type = void_type_node;
  tree gnu_expr, gnu_lhs, gnu_rhs;
  Node_Id gnat_temp;
  bool sync;

  /* Save node number for error message and set location information.  */
  error_gnat_node = gnat_node;
  Sloc_to_locus (Sloc (gnat_node), &input_location);

  /* If we are only annotating types and this node is a statement, return
     an empty statement list.  */
  if (type_annotate_only && statement_node_p (gnat_node))
    return alloc_stmt_list ();

  /* If we are only annotating types and this node is a subexpression, return
     a NULL_EXPR, but filter out nodes appearing in the expressions attached
     to packed array implementation types.  */
  if (type_annotate_only
      && IN (kind, N_Subexpr)
      && !(((IN (kind, N_Op) && kind != N_Op_Expon)
	    || kind == N_Type_Conversion)
	   && Is_Integer_Type (Etype (gnat_node)))
      && !(kind == N_Attribute_Reference
	   && Get_Attribute_Id (Attribute_Name (gnat_node)) == Attr_Length
	   && Ekind (Etype (Prefix (gnat_node))) == E_Array_Subtype
	   && !Is_Constr_Subt_For_U_Nominal (Etype (Prefix (gnat_node))))
      && kind != N_Expanded_Name
      && kind != N_Identifier
      && !Compile_Time_Known_Value (gnat_node))
    return build1 (NULL_EXPR, get_unpadded_type (Etype (gnat_node)),
		   build_call_raise (CE_Range_Check_Failed, gnat_node,
				     N_Raise_Constraint_Error));

  if ((statement_node_p (gnat_node) && kind != N_Null_Statement)
      || kind == N_Handled_Sequence_Of_Statements
      || kind == N_Implicit_Label_Declaration)
    {
      tree current_elab_proc = get_elaboration_procedure ();

      /* If this is a statement and we are at top level, it must be part of
	 the elaboration procedure, so mark us as being in that procedure.  */
      if (!current_function_decl)
	{
	  current_function_decl = current_elab_proc;
	  went_into_elab_proc = true;
	}

      /* If we are in the elaboration procedure, check if we are violating a
	 No_Elaboration_Code restriction by having a statement there.  Don't
	 check for a possible No_Elaboration_Code restriction violation on
	 N_Handled_Sequence_Of_Statements, as we want to signal an error on
	 every nested real statement instead.  This also avoids triggering
	 spurious errors on dummy (empty) sequences created by the front-end
	 for package bodies in some cases.  */
      if (current_function_decl == current_elab_proc
	  && kind != N_Handled_Sequence_Of_Statements
	  && kind != N_Implicit_Label_Declaration)
	Check_Elaboration_Code_Allowed (gnat_node);
    }

  switch (kind)
    {
      /********************************/
      /* Chapter 2: Lexical Elements  */
      /********************************/

    case N_Identifier:
    case N_Expanded_Name:
    case N_Operator_Symbol:
    case N_Defining_Identifier:
    case N_Defining_Operator_Symbol:
      gnu_result = Identifier_to_gnu (gnat_node, &gnu_result_type);

      /* If atomic access is required on the RHS, build the atomic load.  */
      if (atomic_access_required_p (gnat_node, &sync)
	  && !present_in_lhs_or_actual_p (gnat_node))
	gnu_result = build_atomic_load (gnu_result, sync);
      break;

    case N_Integer_Literal:
      {
	tree gnu_type;

	/* Get the type of the result, looking inside any padding and
	   justified modular types.  Then get the value in that type.  */
	gnu_type = gnu_result_type = get_unpadded_type (Etype (gnat_node));

	if (TREE_CODE (gnu_type) == RECORD_TYPE
	    && TYPE_JUSTIFIED_MODULAR_P (gnu_type))
	  gnu_type = TREE_TYPE (TYPE_FIELDS (gnu_type));

	gnu_result = UI_To_gnu (Intval (gnat_node), gnu_type);

	/* If the result overflows (meaning it doesn't fit in its base type),
	   abort.  We would like to check that the value is within the range
	   of the subtype, but that causes problems with subtypes whose usage
	   will raise Constraint_Error and with biased representation, so
	   we don't.  */
	gcc_assert (!TREE_OVERFLOW (gnu_result));
      }
      break;

    case N_Character_Literal:
      /* If a Entity is present, it means that this was one of the
	 literals in a user-defined character type.  In that case,
	 just return the value in the CONST_DECL.  Otherwise, use the
	 character code.  In that case, the base type should be an
	 INTEGER_TYPE, but we won't bother checking for that.  */
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      if (Present (Entity (gnat_node)))
	gnu_result = DECL_INITIAL (get_gnu_tree (Entity (gnat_node)));
      else
	gnu_result
	  = build_int_cst (gnu_result_type,
			   UI_To_CC (Char_Literal_Value (gnat_node)));
      break;

    case N_Real_Literal:
      gnu_result_type = get_unpadded_type (Etype (gnat_node));

      /* If this is of a fixed-point type, the value we want is the value of
	 the corresponding integer.  */
      if (IN (Ekind (Underlying_Type (Etype (gnat_node))), Fixed_Point_Kind))
	{
	  gnu_result = UI_To_gnu (Corresponding_Integer_Value (gnat_node),
				  gnu_result_type);
	  gcc_assert (!TREE_OVERFLOW (gnu_result));
	}

      else
	{
	  Ureal ur_realval = Realval (gnat_node);

	  /* First convert the value to a machine number if it isn't already.
	     That will force the base to 2 for non-zero values and simplify
	     the rest of the logic.  */
	  if (!Is_Machine_Number (gnat_node))
	    ur_realval
	      = Machine (Base_Type (Underlying_Type (Etype (gnat_node))),
			 ur_realval, Round_Even, gnat_node);

	  if (UR_Is_Zero (ur_realval))
	    gnu_result = build_real (gnu_result_type, dconst0);
	  else
	    {
	      REAL_VALUE_TYPE tmp;

	      gnu_result = UI_To_gnu (Numerator (ur_realval), gnu_result_type);

	      /* The base must be 2 as Machine guarantees this, so we scale
		 the value, which we know can fit in the mantissa of the type
		 (hence the use of that type above).  */
	      gcc_assert (Rbase (ur_realval) == 2);
	      real_ldexp (&tmp, &TREE_REAL_CST (gnu_result),
			  - UI_To_Int (Denominator (ur_realval)));
	      gnu_result = build_real (gnu_result_type, tmp);
	    }

	  /* Now see if we need to negate the result.  Do it this way to
	     properly handle -0.  */
	  if (UR_Is_Negative (Realval (gnat_node)))
	    gnu_result
	      = build_unary_op (NEGATE_EXPR, get_base_type (gnu_result_type),
				gnu_result);
	}

      break;

    case N_String_Literal:
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      if (TYPE_PRECISION (TREE_TYPE (gnu_result_type)) == HOST_BITS_PER_CHAR)
	{
	  String_Id gnat_string = Strval (gnat_node);
	  int length = String_Length (gnat_string);
	  int i;
	  char *string;
	  if (length >= ALLOCA_THRESHOLD)
	    string = XNEWVEC (char, length + 1);
	  else
	    string = (char *) alloca (length + 1);

	  /* Build the string with the characters in the literal.  Note
	     that Ada strings are 1-origin.  */
	  for (i = 0; i < length; i++)
	    string[i] = Get_String_Char (gnat_string, i + 1);

	  /* Put a null at the end of the string in case it's in a context
	     where GCC will want to treat it as a C string.  */
	  string[i] = 0;

	  gnu_result = build_string (length, string);

	  /* Strings in GCC don't normally have types, but we want
	     this to not be converted to the array type.  */
	  TREE_TYPE (gnu_result) = gnu_result_type;

	  if (length >= ALLOCA_THRESHOLD)
	    free (string);
	}
      else
	{
	  /* Build a list consisting of each character, then make
	     the aggregate.  */
	  String_Id gnat_string = Strval (gnat_node);
	  int length = String_Length (gnat_string);
	  int i;
	  tree gnu_idx = TYPE_MIN_VALUE (TYPE_DOMAIN (gnu_result_type));
	  tree gnu_one_node = convert (TREE_TYPE (gnu_idx), integer_one_node);
	  vec<constructor_elt, va_gc> *gnu_vec;
	  vec_alloc (gnu_vec, length);

	  for (i = 0; i < length; i++)
	    {
	      tree t = build_int_cst (TREE_TYPE (gnu_result_type),
				      Get_String_Char (gnat_string, i + 1));

	      CONSTRUCTOR_APPEND_ELT (gnu_vec, gnu_idx, t);
	      gnu_idx = int_const_binop (PLUS_EXPR, gnu_idx, gnu_one_node);
	    }

	  gnu_result = gnat_build_constructor (gnu_result_type, gnu_vec);
	}
      break;

    case N_Pragma:
      gnu_result = Pragma_to_gnu (gnat_node);
      break;

    /**************************************/
    /* Chapter 3: Declarations and Types  */
    /**************************************/

    case N_Subtype_Declaration:
    case N_Full_Type_Declaration:
    case N_Incomplete_Type_Declaration:
    case N_Private_Type_Declaration:
    case N_Private_Extension_Declaration:
    case N_Task_Type_Declaration:
      process_type (Defining_Entity (gnat_node));
      gnu_result = alloc_stmt_list ();
      break;

    case N_Object_Declaration:
    case N_Exception_Declaration:
      gnat_temp = Defining_Entity (gnat_node);
      gnu_result = alloc_stmt_list ();

      /* If we are just annotating types and this object has an unconstrained
	 or task type, don't elaborate it.   */
      if (type_annotate_only
	  && (((Is_Array_Type (Etype (gnat_temp))
		|| Is_Record_Type (Etype (gnat_temp)))
	       && !Is_Constrained (Etype (gnat_temp)))
	    || Is_Concurrent_Type (Etype (gnat_temp))))
	break;

      if (Present (Expression (gnat_node))
	  && !(kind == N_Object_Declaration && No_Initialization (gnat_node))
	  && (!type_annotate_only
	      || Compile_Time_Known_Value (Expression (gnat_node))))
	{
	  gnu_expr = gnat_to_gnu (Expression (gnat_node));
	  if (Do_Range_Check (Expression (gnat_node)))
	    gnu_expr
	      = emit_range_check (gnu_expr, Etype (gnat_temp), gnat_node);

	  if (type_annotate_only && TREE_CODE (gnu_expr) == ERROR_MARK)
	    gnu_expr = NULL_TREE;
	}
      else
	gnu_expr = NULL_TREE;

      /* If this is a deferred constant with an address clause, we ignore the
	 full view since the clause is on the partial view and we cannot have
	 2 different GCC trees for the object.  The only bits of the full view
	 we will use is the initializer, but it will be directly fetched.  */
      if (Ekind (gnat_temp) == E_Constant
	  && Present (Address_Clause (gnat_temp))
	  && Present (Full_View (gnat_temp)))
	save_gnu_tree (Full_View (gnat_temp), error_mark_node, true);

      /* If this object has its elaboration delayed, we must force evaluation
	 of GNU_EXPR now and save it for the freeze point.  Note that we need
	 not do anything special at the global level since the lifetime of the
	 temporary is fully contained within the elaboration routine.  */
      if (Present (Freeze_Node (gnat_temp)))
	{
	  if (gnu_expr)
	    {
	      gnu_result = gnat_save_expr (gnu_expr);
	      save_gnu_tree (gnat_node, gnu_result, true);
	    }
	}
      else
	gnat_to_gnu_entity (gnat_temp, gnu_expr, true);
      break;

    case N_Object_Renaming_Declaration:
      gnat_temp = Defining_Entity (gnat_node);
      gnu_result = alloc_stmt_list ();

      /* Don't do anything if this renaming is handled by the front end or if
	 we are just annotating types and this object has a composite or task
	 type, don't elaborate it.  */
      if (!Is_Renaming_Of_Object (gnat_temp)
	  && ! (type_annotate_only
		&& (Is_Array_Type (Etype (gnat_temp))
		    || Is_Record_Type (Etype (gnat_temp))
		    || Is_Concurrent_Type (Etype (gnat_temp)))))
	{
	  tree gnu_temp
	    = gnat_to_gnu_entity (gnat_temp,
				  gnat_to_gnu (Renamed_Object (gnat_temp)),
				  true);
	  /* See case 2 of renaming in gnat_to_gnu_entity.  */
	  if (TREE_SIDE_EFFECTS (gnu_temp))
	    gnu_result = build_unary_op (ADDR_EXPR, NULL_TREE, gnu_temp);
	}
      break;

    case N_Exception_Renaming_Declaration:
      gnat_temp = Defining_Entity (gnat_node);
      gnu_result = alloc_stmt_list ();

      /* See the above case for the rationale.  */
      if (Present (Renamed_Entity (gnat_temp)))
	{
	  tree gnu_temp
	    = gnat_to_gnu_entity (gnat_temp,
				  gnat_to_gnu (Renamed_Entity (gnat_temp)),
				  true);
	  if (TREE_SIDE_EFFECTS (gnu_temp))
	    gnu_result = build_unary_op (ADDR_EXPR, NULL_TREE, gnu_temp);
	}
      break;

    case N_Subprogram_Renaming_Declaration:
      {
	const Node_Id gnat_renaming = Defining_Entity (gnat_node);
	const Node_Id gnat_renamed = Renamed_Entity (gnat_renaming);

	gnu_result = alloc_stmt_list ();

	/* Materializing renamed subprograms will only benefit the debugging
	   information as they aren't referenced in the generated code.  So
	   skip them when they aren't needed.  Avoid doing this if:

	     - there is a freeze node: in this case the renamed entity is not
	       elaborated yet,
	     - the renamed subprogram is intrinsic: it will not be available in
	       the debugging information (note that both or only one of the
	       renaming and the renamed subprograms can be intrinsic).  */
	if (!type_annotate_only
	    && Needs_Debug_Info (gnat_renaming)
	    && No (Freeze_Node (gnat_renaming))
	    && Present (gnat_renamed)
	    && (Ekind (gnat_renamed) == E_Function
		|| Ekind (gnat_renamed) == E_Procedure)
	    && !Is_Intrinsic_Subprogram (gnat_renaming)
	    && !Is_Intrinsic_Subprogram (gnat_renamed))
	  gnat_to_gnu_entity (gnat_renaming, gnat_to_gnu (gnat_renamed), true);
	break;
      }

    case N_Implicit_Label_Declaration:
      gnat_to_gnu_entity (Defining_Entity (gnat_node), NULL_TREE, true);
      gnu_result = alloc_stmt_list ();
      break;

    case N_Number_Declaration:
    case N_Package_Renaming_Declaration:
      /* These are fully handled in the front end.  */
      /* ??? For package renamings, find a way to use GENERIC namespaces so
	 that we get proper debug information for them.  */
      gnu_result = alloc_stmt_list ();
      break;

    /*************************************/
    /* Chapter 4: Names and Expressions  */
    /*************************************/

    case N_Explicit_Dereference:
      gnu_result = gnat_to_gnu (Prefix (gnat_node));
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      gnu_result = build_unary_op (INDIRECT_REF, NULL_TREE, gnu_result);

      /* If atomic access is required on the RHS, build the atomic load.  */
      if (atomic_access_required_p (gnat_node, &sync)
	  && !present_in_lhs_or_actual_p (gnat_node))
	gnu_result = build_atomic_load (gnu_result, sync);
      break;

    case N_Indexed_Component:
      {
	tree gnu_array_object = gnat_to_gnu (Prefix (gnat_node));
	tree gnu_type;
	int ndim;
	int i;
	Node_Id *gnat_expr_array;

	gnu_array_object = maybe_implicit_deref (gnu_array_object);

	/* Convert vector inputs to their representative array type, to fit
	   what the code below expects.  */
	if (VECTOR_TYPE_P (TREE_TYPE (gnu_array_object)))
	  {
	    if (present_in_lhs_or_actual_p (gnat_node))
	      gnat_mark_addressable (gnu_array_object);
	    gnu_array_object = maybe_vector_array (gnu_array_object);
	  }

	gnu_array_object = maybe_unconstrained_array (gnu_array_object);

	/* If we got a padded type, remove it too.  */
	if (TYPE_IS_PADDING_P (TREE_TYPE (gnu_array_object)))
	  gnu_array_object
	    = convert (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (gnu_array_object))),
		       gnu_array_object);

	/* The failure of this assertion will very likely come from a missing
	   expansion for a packed array access.  */
	gcc_assert (TREE_CODE (TREE_TYPE (gnu_array_object)) == ARRAY_TYPE);

	/* First compute the number of dimensions of the array, then
	   fill the expression array, the order depending on whether
	   this is a Convention_Fortran array or not.  */
	for (ndim = 1, gnu_type = TREE_TYPE (gnu_array_object);
	     TREE_CODE (TREE_TYPE (gnu_type)) == ARRAY_TYPE
	     && TYPE_MULTI_ARRAY_P (TREE_TYPE (gnu_type));
	     ndim++, gnu_type = TREE_TYPE (gnu_type))
	  ;

	gnat_expr_array = XALLOCAVEC (Node_Id, ndim);

	if (TYPE_CONVENTION_FORTRAN_P (TREE_TYPE (gnu_array_object)))
	  for (i = ndim - 1, gnat_temp = First (Expressions (gnat_node));
	       i >= 0;
	       i--, gnat_temp = Next (gnat_temp))
	    gnat_expr_array[i] = gnat_temp;
	else
	  for (i = 0, gnat_temp = First (Expressions (gnat_node));
	       i < ndim;
	       i++, gnat_temp = Next (gnat_temp))
	    gnat_expr_array[i] = gnat_temp;

	/* Start with the prefix and build the successive references.  */
	gnu_result = gnu_array_object;

	for (i = 0, gnu_type = TREE_TYPE (gnu_array_object);
	     i < ndim;
	     i++, gnu_type = TREE_TYPE (gnu_type))
	  {
	    gcc_assert (TREE_CODE (gnu_type) == ARRAY_TYPE);
	    gnat_temp = gnat_expr_array[i];
	    gnu_expr = maybe_character_value (gnat_to_gnu (gnat_temp));
	    struct loop_info_d *loop;

	    gnu_result
	      = build_binary_op (ARRAY_REF, NULL_TREE, gnu_result, gnu_expr);

	    /* Array accesses are bound-checked so they cannot trap, but this
	       is valid only if they are not hoisted ahead of the check.  We
	       need to mark them as no-trap to get decent loop optimizations
	       in the presence of -fnon-call-exceptions, so we do it when we
	       know that the original expression had no side-effects.  */
	    if (TREE_CODE (gnu_result) == ARRAY_REF
		&& !(Nkind (gnat_temp) == N_Identifier
		     && Ekind (Entity (gnat_temp)) == E_Constant))
	      TREE_THIS_NOTRAP (gnu_result) = 1;

	    /* If aggressive loop optimizations are enabled, we warn for loops
	       overrunning a simple array of size 1 not at the end of a record.
	       This is aimed to catch misuses of the trailing array idiom.  */
	    if (optimize
		&& flag_aggressive_loop_optimizations
		&& inside_loop_p ()
		&& TREE_CODE (TREE_TYPE (gnu_type)) != ARRAY_TYPE
		&& TREE_CODE (gnu_array_object) != ARRAY_REF
		&& tree_int_cst_equal (TYPE_MIN_VALUE (TYPE_DOMAIN (gnu_type)),
				       TYPE_MAX_VALUE (TYPE_DOMAIN (gnu_type)))
		&& !array_at_struct_end_p (gnu_result)
		&& (loop = find_loop_for (gnu_expr))
		&& !loop->artificial
		&& !loop->has_checks
		&& tree_int_cst_equal (TYPE_MIN_VALUE (TYPE_DOMAIN (gnu_type)),
				       loop->low_bound)
		&& can_be_lower_p (loop->low_bound, loop->high_bound)
		&& !loop->warned_aggressive_loop_optimizations
		&& warning (OPT_Waggressive_loop_optimizations,
			    "out-of-bounds access may be optimized away"))
	      {
		inform (EXPR_LOCATION (loop->stmt), "containing loop");
		loop->warned_aggressive_loop_optimizations = true;
	      }
	  }

	gnu_result_type = get_unpadded_type (Etype (gnat_node));

	/* If atomic access is required on the RHS, build the atomic load.  */
	if (atomic_access_required_p (gnat_node, &sync)
	    && !present_in_lhs_or_actual_p (gnat_node))
	  gnu_result = build_atomic_load (gnu_result, sync);
      }
      break;

    case N_Slice:
      {
	tree gnu_array_object = gnat_to_gnu (Prefix (gnat_node));

	gnu_result_type = get_unpadded_type (Etype (gnat_node));

	gnu_array_object = maybe_implicit_deref (gnu_array_object);
	gnu_array_object = maybe_unconstrained_array (gnu_array_object);

	gnu_expr = TYPE_MIN_VALUE (TYPE_DOMAIN (gnu_result_type));
	gnu_expr = maybe_character_value (gnu_expr);

	/* If this is a slice with non-constant size of an array with constant
	   size, set the maximum size for the allocation of temporaries.  */
	if (!TREE_CONSTANT (TYPE_SIZE_UNIT (gnu_result_type))
	    && TREE_CONSTANT (TYPE_SIZE_UNIT (TREE_TYPE (gnu_array_object))))
	  TYPE_ARRAY_MAX_SIZE (gnu_result_type)
	    = TYPE_SIZE_UNIT (TREE_TYPE (gnu_array_object));

	gnu_result = build_binary_op (ARRAY_RANGE_REF, gnu_result_type,
				      gnu_array_object, gnu_expr);
      }
      break;

    case N_Selected_Component:
      {
	Entity_Id gnat_prefix = Prefix (gnat_node);
	Entity_Id gnat_field = Entity (Selector_Name (gnat_node));
	tree gnu_prefix = gnat_to_gnu (gnat_prefix);
	tree gnu_field;

	gnu_prefix = maybe_implicit_deref (gnu_prefix);

	/* For discriminant references in tagged types always substitute the
	   corresponding discriminant as the actual selected component.  */
	if (Is_Tagged_Type (Underlying_Type (Etype (gnat_prefix))))
	  while (Present (Corresponding_Discriminant (gnat_field)))
	    gnat_field = Corresponding_Discriminant (gnat_field);

	/* For discriminant references of untagged types always substitute the
	   corresponding stored discriminant.  */
	else if (Present (Corresponding_Discriminant (gnat_field)))
	  gnat_field = Original_Record_Component (gnat_field);

	/* Handle extracting the real or imaginary part of a complex.
	   The real part is the first field and the imaginary the last.  */
	if (TREE_CODE (TREE_TYPE (gnu_prefix)) == COMPLEX_TYPE)
	  gnu_result = build_unary_op (Present (Next_Entity (gnat_field))
				       ? REALPART_EXPR : IMAGPART_EXPR,
				       NULL_TREE, gnu_prefix);
	else
	  {
	    gnu_field = gnat_to_gnu_field_decl (gnat_field);

	    gnu_result
	      = build_component_ref (gnu_prefix, gnu_field,
				     (Nkind (Parent (gnat_node))
				      == N_Attribute_Reference)
				     && lvalue_required_for_attribute_p
					(Parent (gnat_node)));
	  }

	gnu_result_type = get_unpadded_type (Etype (gnat_node));

	/* If atomic access is required on the RHS, build the atomic load.  */
	if (atomic_access_required_p (gnat_node, &sync)
	    && !present_in_lhs_or_actual_p (gnat_node))
	  gnu_result = build_atomic_load (gnu_result, sync);
      }
      break;

    case N_Attribute_Reference:
      {
	/* The attribute designator.  */
	const int attr = Get_Attribute_Id (Attribute_Name (gnat_node));

	/* The Elab_Spec and Elab_Body attributes are special in that Prefix
	   is a unit, not an object with a GCC equivalent.  */
	if (attr == Attr_Elab_Spec || attr == Attr_Elab_Body)
	  return
	    create_subprog_decl (create_concat_name
				 (Entity (Prefix (gnat_node)),
				  attr == Attr_Elab_Body ? "elabb" : "elabs"),
				 NULL_TREE, void_ftype, NULL_TREE, is_disabled,
				 true, true, true, true, NULL, gnat_node);

	gnu_result = Attribute_to_gnu (gnat_node, &gnu_result_type, attr);
      }
      break;

    case N_Reference:
      /* Like 'Access as far as we are concerned.  */
      gnu_result = gnat_to_gnu (Prefix (gnat_node));
      gnu_result = build_unary_op (ADDR_EXPR, NULL_TREE, gnu_result);
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      break;

    case N_Aggregate:
    case N_Extension_Aggregate:
      {
	tree gnu_aggr_type;

	/* ??? It is wrong to evaluate the type now, but there doesn't
	   seem to be any other practical way of doing it.  */

	gcc_assert (!Expansion_Delayed (gnat_node));

	gnu_aggr_type = gnu_result_type
	  = get_unpadded_type (Etype (gnat_node));

	if (TREE_CODE (gnu_result_type) == RECORD_TYPE
	    && TYPE_CONTAINS_TEMPLATE_P (gnu_result_type))
	  gnu_aggr_type
	    = TREE_TYPE (DECL_CHAIN (TYPE_FIELDS (gnu_result_type)));
	else if (TREE_CODE (gnu_result_type) == VECTOR_TYPE)
	  gnu_aggr_type = TYPE_REPRESENTATIVE_ARRAY (gnu_result_type);

	if (Null_Record_Present (gnat_node))
	  gnu_result = gnat_build_constructor (gnu_aggr_type,
					       NULL);

	else if (TREE_CODE (gnu_aggr_type) == RECORD_TYPE
		 || TREE_CODE (gnu_aggr_type) == UNION_TYPE)
	  gnu_result
	    = assoc_to_constructor (Etype (gnat_node),
				    First (Component_Associations (gnat_node)),
				    gnu_aggr_type);
	else if (TREE_CODE (gnu_aggr_type) == ARRAY_TYPE)
	  gnu_result = pos_to_constructor (First (Expressions (gnat_node)),
					   gnu_aggr_type,
					   Component_Type (Etype (gnat_node)));
	else if (TREE_CODE (gnu_aggr_type) == COMPLEX_TYPE)
	  gnu_result
	    = build_binary_op
	      (COMPLEX_EXPR, gnu_aggr_type,
	       gnat_to_gnu (Expression (First
					(Component_Associations (gnat_node)))),
	       gnat_to_gnu (Expression
			    (Next
			     (First (Component_Associations (gnat_node))))));
	else
	  gcc_unreachable ();

	gnu_result = convert (gnu_result_type, gnu_result);
      }
      break;

    case N_Null:
      if (TARGET_VTABLE_USES_DESCRIPTORS
	  && Ekind (Etype (gnat_node)) == E_Access_Subprogram_Type
	  && Is_Dispatch_Table_Entity (Etype (gnat_node)))
	gnu_result = null_fdesc_node;
      else
	gnu_result = null_pointer_node;
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      break;

    case N_Type_Conversion:
    case N_Qualified_Expression:
      gnu_expr = maybe_character_value (gnat_to_gnu (Expression (gnat_node)));
      gnu_result_type = get_unpadded_type (Etype (gnat_node));

      /* If this is a qualified expression for a tagged type, we mark the type
	 as used.  Because of polymorphism, this might be the only reference to
	 the tagged type in the program while objects have it as dynamic type.
	 The debugger needs to see it to display these objects properly.  */
      if (kind == N_Qualified_Expression && Is_Tagged_Type (Etype (gnat_node)))
	used_types_insert (gnu_result_type);

      gnu_result
	= convert_with_check (Etype (gnat_node), gnu_expr,
			      Do_Overflow_Check (gnat_node),
			      Do_Range_Check (Expression (gnat_node)),
			      kind == N_Type_Conversion
			      && Float_Truncate (gnat_node), gnat_node);
      break;

    case N_Unchecked_Type_Conversion:
      gnu_expr = maybe_character_value (gnat_to_gnu (Expression (gnat_node)));

      /* Skip further processing if the conversion is deemed a no-op.  */
      if (unchecked_conversion_nop (gnat_node))
	{
	  gnu_result = gnu_expr;
	  gnu_result_type = TREE_TYPE (gnu_result);
	  break;
	}

      gnu_result_type = get_unpadded_type (Etype (gnat_node));

      /* If the result is a pointer type, see if we are improperly
	 converting to a stricter alignment.  */
      if (STRICT_ALIGNMENT && POINTER_TYPE_P (gnu_result_type)
	  && IN (Ekind (Etype (gnat_node)), Access_Kind))
	{
	  unsigned int align = known_alignment (gnu_expr);
	  tree gnu_obj_type = TREE_TYPE (gnu_result_type);
	  unsigned int oalign = TYPE_ALIGN (gnu_obj_type);

	  if (align != 0 && align < oalign && !TYPE_ALIGN_OK (gnu_obj_type))
	    post_error_ne_tree_2
	      ("?source alignment (^) '< alignment of & (^)",
	       gnat_node, Designated_Type (Etype (gnat_node)),
	       size_int (align / BITS_PER_UNIT), oalign / BITS_PER_UNIT);
	}

      /* If we are converting a descriptor to a function pointer, first
	 build the pointer.  */
      if (TARGET_VTABLE_USES_DESCRIPTORS
	  && TREE_TYPE (gnu_expr) == fdesc_type_node
	  && POINTER_TYPE_P (gnu_result_type))
	gnu_expr = build_unary_op (ADDR_EXPR, NULL_TREE, gnu_expr);

      gnu_result = unchecked_convert (gnu_result_type, gnu_expr,
				      No_Truncation (gnat_node));
      break;

    case N_In:
    case N_Not_In:
      {
	tree gnu_obj = gnat_to_gnu (Left_Opnd (gnat_node));
	tree gnu_low, gnu_high;

	Range_to_gnu (Right_Opnd (gnat_node), &gnu_low, &gnu_high);
	gnu_result_type = get_unpadded_type (Etype (gnat_node));

	tree gnu_op_type = maybe_character_type (TREE_TYPE (gnu_obj));
	if (TREE_TYPE (gnu_obj) != gnu_op_type)
	  {
	    gnu_obj = convert (gnu_op_type, gnu_obj);
	    gnu_low = convert (gnu_op_type, gnu_low);
	    gnu_high = convert (gnu_op_type, gnu_high);
	  }

	/* If LOW and HIGH are identical, perform an equality test.  Otherwise,
	   ensure that GNU_OBJ is evaluated only once and perform a full range
	   test.  */
	if (operand_equal_p (gnu_low, gnu_high, 0))
	  gnu_result
	    = build_binary_op (EQ_EXPR, gnu_result_type, gnu_obj, gnu_low);
	else
	  {
	    tree t1, t2;
	    gnu_obj = gnat_protect_expr (gnu_obj);
	    t1 = build_binary_op (GE_EXPR, gnu_result_type, gnu_obj, gnu_low);
	    if (EXPR_P (t1))
	      set_expr_location_from_node (t1, gnat_node);
	    t2 = build_binary_op (LE_EXPR, gnu_result_type, gnu_obj, gnu_high);
	    if (EXPR_P (t2))
	      set_expr_location_from_node (t2, gnat_node);
	    gnu_result
	      = build_binary_op (TRUTH_ANDIF_EXPR, gnu_result_type, t1, t2);
	  }

	if (kind == N_Not_In)
	  gnu_result
	    = invert_truthvalue_loc (EXPR_LOCATION (gnu_result), gnu_result);
      }
      break;

    case N_Op_Divide:
      gnu_lhs = gnat_to_gnu (Left_Opnd (gnat_node));
      gnu_rhs = gnat_to_gnu (Right_Opnd (gnat_node));
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      gnu_result = build_binary_op (FLOAT_TYPE_P (gnu_result_type)
				    ? RDIV_EXPR
				    : (Rounded_Result (gnat_node)
				       ? ROUND_DIV_EXPR : TRUNC_DIV_EXPR),
				    gnu_result_type, gnu_lhs, gnu_rhs);
      break;

    case N_Op_And:
    case N_Op_Or:
    case N_Op_Xor:
      /* These can either be operations on booleans or on modular types.
	 Fall through for boolean types since that's the way GNU_CODES is
	 set up.  */
      if (Is_Modular_Integer_Type (Underlying_Type (Etype (gnat_node))))
	{
	  enum tree_code code
	    = (kind == N_Op_Or ? BIT_IOR_EXPR
	       : kind == N_Op_And ? BIT_AND_EXPR
	       : BIT_XOR_EXPR);

	  gnu_lhs = gnat_to_gnu (Left_Opnd (gnat_node));
	  gnu_rhs = gnat_to_gnu (Right_Opnd (gnat_node));
	  gnu_result_type = get_unpadded_type (Etype (gnat_node));
	  gnu_result = build_binary_op (code, gnu_result_type,
					gnu_lhs, gnu_rhs);
	  break;
	}

      /* ... fall through ... */

    case N_Op_Eq:
    case N_Op_Ne:
    case N_Op_Lt:
    case N_Op_Le:
    case N_Op_Gt:
    case N_Op_Ge:
    case N_Op_Add:
    case N_Op_Subtract:
    case N_Op_Multiply:
    case N_Op_Mod:
    case N_Op_Rem:
    case N_Op_Rotate_Left:
    case N_Op_Rotate_Right:
    case N_Op_Shift_Left:
    case N_Op_Shift_Right:
    case N_Op_Shift_Right_Arithmetic:
    case N_And_Then:
    case N_Or_Else:
      {
	enum tree_code code = gnu_codes[kind];
	bool ignore_lhs_overflow = false;
	location_t saved_location = input_location;
	tree gnu_type;

	gnu_lhs = gnat_to_gnu (Left_Opnd (gnat_node));
	gnu_rhs = gnat_to_gnu (Right_Opnd (gnat_node));
	gnu_type = gnu_result_type = get_unpadded_type (Etype (gnat_node));

	/* Pending generic support for efficient vector logical operations in
	   GCC, convert vectors to their representative array type view and
	   fallthrough.  */
	gnu_lhs = maybe_vector_array (gnu_lhs);
	gnu_rhs = maybe_vector_array (gnu_rhs);

	/* If this is a comparison operator, convert any references to an
	   unconstrained array value into a reference to the actual array.  */
	if (TREE_CODE_CLASS (code) == tcc_comparison)
	  {
	    gnu_lhs = maybe_unconstrained_array (gnu_lhs);
	    gnu_rhs = maybe_unconstrained_array (gnu_rhs);

	    tree gnu_op_type = maybe_character_type (TREE_TYPE (gnu_lhs));
	    if (TREE_TYPE (gnu_lhs) != gnu_op_type)
	      {
		gnu_lhs = convert (gnu_op_type, gnu_lhs);
		gnu_rhs = convert (gnu_op_type, gnu_rhs);
	      }
	  }

	/* If this is a shift whose count is not guaranteed to be correct,
	   we need to adjust the shift count.  */
	if (IN (kind, N_Op_Shift) && !Shift_Count_OK (gnat_node))
	  {
	    tree gnu_count_type = get_base_type (TREE_TYPE (gnu_rhs));
	    tree gnu_max_shift
	      = convert (gnu_count_type, TYPE_SIZE (gnu_type));

	    if (kind == N_Op_Rotate_Left || kind == N_Op_Rotate_Right)
	      gnu_rhs = build_binary_op (TRUNC_MOD_EXPR, gnu_count_type,
					 gnu_rhs, gnu_max_shift);
	    else if (kind == N_Op_Shift_Right_Arithmetic)
	      gnu_rhs
		= build_binary_op
		  (MIN_EXPR, gnu_count_type,
		   build_binary_op (MINUS_EXPR,
				    gnu_count_type,
				    gnu_max_shift,
				    build_int_cst (gnu_count_type, 1)),
		   gnu_rhs);
	  }

	/* For right shifts, the type says what kind of shift to do,
	   so we may need to choose a different type.  In this case,
	   we have to ignore integer overflow lest it propagates all
	   the way down and causes a CE to be explicitly raised.  */
	if (kind == N_Op_Shift_Right && !TYPE_UNSIGNED (gnu_type))
	  {
	    gnu_type = gnat_unsigned_type_for (gnu_type);
	    ignore_lhs_overflow = true;
	  }
	else if (kind == N_Op_Shift_Right_Arithmetic
		 && TYPE_UNSIGNED (gnu_type))
	  {
	    gnu_type = gnat_signed_type_for (gnu_type);
	    ignore_lhs_overflow = true;
	  }

	if (gnu_type != gnu_result_type)
	  {
	    tree gnu_old_lhs = gnu_lhs;
	    gnu_lhs = convert (gnu_type, gnu_lhs);
	    if (TREE_CODE (gnu_lhs) == INTEGER_CST && ignore_lhs_overflow)
	      TREE_OVERFLOW (gnu_lhs) = TREE_OVERFLOW (gnu_old_lhs);
	    gnu_rhs = convert (gnu_type, gnu_rhs);
	  }

	/* Instead of expanding overflow checks for addition, subtraction
	   and multiplication itself, the front end will leave this to
	   the back end when Backend_Overflow_Checks_On_Target is set.
	   As the back end itself does not know yet how to properly
	   do overflow checking, do it here.  The goal is to push
	   the expansions further into the back end over time.  */
	if (Do_Overflow_Check (gnat_node)
	    && Backend_Overflow_Checks_On_Target
	    && (code == PLUS_EXPR || code == MINUS_EXPR || code == MULT_EXPR)
	    && !TYPE_UNSIGNED (gnu_type)
	    && !FLOAT_TYPE_P (gnu_type))
	  gnu_result = build_binary_op_trapv (code, gnu_type,
					      gnu_lhs, gnu_rhs, gnat_node);
	else
	  {
	    /* Some operations, e.g. comparisons of arrays, generate complex
	       trees that need to be annotated while they are being built.  */
	    input_location = saved_location;
	    gnu_result = build_binary_op (code, gnu_type, gnu_lhs, gnu_rhs);
	  }

	/* If this is a logical shift with the shift count not verified,
	   we must return zero if it is too large.  We cannot compensate
	   above in this case.  */
	if ((kind == N_Op_Shift_Left || kind == N_Op_Shift_Right)
	    && !Shift_Count_OK (gnat_node))
	  gnu_result
	    = build_cond_expr
	      (gnu_type,
	       build_binary_op (GE_EXPR, boolean_type_node,
				gnu_rhs,
				convert (TREE_TYPE (gnu_rhs),
					 TYPE_SIZE (gnu_type))),
	       build_int_cst (gnu_type, 0),
	       gnu_result);
      }
      break;

    case N_If_Expression:
      {
	tree gnu_cond = gnat_to_gnu (First (Expressions (gnat_node)));
	tree gnu_true = gnat_to_gnu (Next (First (Expressions (gnat_node))));
	tree gnu_false
	  = gnat_to_gnu (Next (Next (First (Expressions (gnat_node)))));

	gnu_result_type = get_unpadded_type (Etype (gnat_node));
	gnu_result
	  = build_cond_expr (gnu_result_type, gnu_cond, gnu_true, gnu_false);
      }
      break;

    case N_Op_Plus:
      gnu_result = gnat_to_gnu (Right_Opnd (gnat_node));
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      break;

    case N_Op_Not:
      /* This case can apply to a boolean or a modular type.
	 Fall through for a boolean operand since GNU_CODES is set
	 up to handle this.  */
      if (Is_Modular_Integer_Type (Underlying_Type (Etype (gnat_node))))
	{
	  gnu_expr = gnat_to_gnu (Right_Opnd (gnat_node));
	  gnu_result_type = get_unpadded_type (Etype (gnat_node));
	  gnu_result = build_unary_op (BIT_NOT_EXPR, gnu_result_type,
				       gnu_expr);
	  break;
	}

      /* ... fall through ... */

    case N_Op_Minus:
    case N_Op_Abs:
      gnu_expr = gnat_to_gnu (Right_Opnd (gnat_node));
      gnu_result_type = get_unpadded_type (Etype (gnat_node));

      if (Do_Overflow_Check (gnat_node)
	  && !TYPE_UNSIGNED (gnu_result_type)
	  && !FLOAT_TYPE_P (gnu_result_type))
	gnu_result
	  = build_unary_op_trapv (gnu_codes[kind],
				  gnu_result_type, gnu_expr, gnat_node);
      else
	gnu_result = build_unary_op (gnu_codes[kind],
				     gnu_result_type, gnu_expr);
      break;

    case N_Allocator:
      {
	tree gnu_init = 0;
	tree gnu_type;
	bool ignore_init_type = false;

	gnat_temp = Expression (gnat_node);

	/* The Expression operand can either be an N_Identifier or
	   Expanded_Name, which must represent a type, or a
	   N_Qualified_Expression, which contains both the object type and an
	   initial value for the object.  */
	if (Nkind (gnat_temp) == N_Identifier
	    || Nkind (gnat_temp) == N_Expanded_Name)
	  gnu_type = gnat_to_gnu_type (Entity (gnat_temp));
	else if (Nkind (gnat_temp) == N_Qualified_Expression)
	  {
	    Entity_Id gnat_desig_type
	      = Designated_Type (Underlying_Type (Etype (gnat_node)));

	    ignore_init_type = Has_Constrained_Partial_View (gnat_desig_type);
	    gnu_init = gnat_to_gnu (Expression (gnat_temp));

	    gnu_init = maybe_unconstrained_array (gnu_init);
	    if (Do_Range_Check (Expression (gnat_temp)))
	      gnu_init
		= emit_range_check (gnu_init, gnat_desig_type, gnat_temp);

	    if (Is_Elementary_Type (gnat_desig_type)
		|| Is_Constrained (gnat_desig_type))
	      gnu_type = gnat_to_gnu_type (gnat_desig_type);
	    else
	      {
		gnu_type = gnat_to_gnu_type (Etype (Expression (gnat_temp)));
		if (TREE_CODE (gnu_type) == UNCONSTRAINED_ARRAY_TYPE)
		  gnu_type = TREE_TYPE (gnu_init);
	      }

	    /* See the N_Qualified_Expression case for the rationale.  */
	    if (Is_Tagged_Type (gnat_desig_type))
	      used_types_insert (gnu_type);

	    gnu_init = convert (gnu_type, gnu_init);
	  }
	else
	  gcc_unreachable ();

	gnu_result_type = get_unpadded_type (Etype (gnat_node));
	return build_allocator (gnu_type, gnu_init, gnu_result_type,
				Procedure_To_Call (gnat_node),
				Storage_Pool (gnat_node), gnat_node,
				ignore_init_type);
      }
      break;

    /**************************/
    /* Chapter 5: Statements  */
    /**************************/

    case N_Label:
      gnu_result = build1 (LABEL_EXPR, void_type_node,
			   gnat_to_gnu (Identifier (gnat_node)));
      break;

    case N_Null_Statement:
      /* When not optimizing, turn null statements from source into gotos to
	 the next statement that the middle-end knows how to preserve.  */
      if (!optimize && Comes_From_Source (gnat_node))
	{
	  tree stmt, label = create_label_decl (NULL_TREE, gnat_node);
	  DECL_IGNORED_P (label) = 1;
	  start_stmt_group ();
	  stmt = build1 (GOTO_EXPR, void_type_node, label);
	  set_expr_location_from_node (stmt, gnat_node);
	  add_stmt (stmt);
	  stmt = build1 (LABEL_EXPR, void_type_node, label);
	  set_expr_location_from_node (stmt, gnat_node);
	  add_stmt (stmt);
	  gnu_result = end_stmt_group ();
	}
      else
	gnu_result = alloc_stmt_list ();
      break;

    case N_Assignment_Statement:
      /* Get the LHS and RHS of the statement and convert any reference to an
	 unconstrained array into a reference to the underlying array.  */
      gnu_lhs = maybe_unconstrained_array (gnat_to_gnu (Name (gnat_node)));

      /* If the type has a size that overflows, convert this into raise of
	 Storage_Error: execution shouldn't have gotten here anyway.  */
      if (TREE_CODE (TYPE_SIZE_UNIT (TREE_TYPE (gnu_lhs))) == INTEGER_CST
	   && !valid_constant_size_p (TYPE_SIZE_UNIT (TREE_TYPE (gnu_lhs))))
	gnu_result = build_call_raise (SE_Object_Too_Large, gnat_node,
				       N_Raise_Storage_Error);
      else if (Nkind (Expression (gnat_node)) == N_Function_Call)
	{
	  bool outer_atomic_access
	    = outer_atomic_access_required_p (Name (gnat_node));
	  bool atomic_access
	    = !outer_atomic_access
	      && atomic_access_required_p (Name (gnat_node), &sync);
	  gnu_result
	    = Call_to_gnu (Expression (gnat_node), &gnu_result_type, gnu_lhs,
			   outer_atomic_access, atomic_access, sync);
	}
      else
	{
	  const Node_Id gnat_expr = Expression (gnat_node);
	  const Entity_Id gnat_type
	    = Underlying_Type (Etype (Name (gnat_node)));
	  const bool regular_array_type_p
	    = (Is_Array_Type (gnat_type) && !Is_Bit_Packed_Array (gnat_type));
	  const bool use_memset_p
	    = (regular_array_type_p
	       && Nkind (gnat_expr) == N_Aggregate
	       && Is_Others_Aggregate (gnat_expr));

	  /* If we'll use memset, we need to find the inner expression.  */
	  if (use_memset_p)
	    {
	      Node_Id gnat_inner
		= Expression (First (Component_Associations (gnat_expr)));
	      while (Nkind (gnat_inner) == N_Aggregate
		     && Is_Others_Aggregate (gnat_inner))
		gnat_inner
		  = Expression (First (Component_Associations (gnat_inner)));
	      gnu_rhs = gnat_to_gnu (gnat_inner);
	    }
	  else
	    gnu_rhs = maybe_unconstrained_array (gnat_to_gnu (gnat_expr));

	  /* If range check is needed, emit code to generate it.  */
	  if (Do_Range_Check (gnat_expr))
	    gnu_rhs = emit_range_check (gnu_rhs, Etype (Name (gnat_node)),
					gnat_node);

	  /* If an outer atomic access is required on the LHS, build the load-
	     modify-store sequence.  */
	  if (outer_atomic_access_required_p (Name (gnat_node)))
	    gnu_result = build_load_modify_store (gnu_lhs, gnu_rhs, gnat_node);

	  /* Or else, if atomic access is required, build the atomic store.  */
	  else if (atomic_access_required_p (Name (gnat_node), &sync))
	    gnu_result = build_atomic_store (gnu_lhs, gnu_rhs, sync);

	  /* Or else, use memset when the conditions are met.  */
	  else if (use_memset_p)
	    {
	      tree value = fold_convert (integer_type_node, gnu_rhs);
	      tree to = gnu_lhs;
	      tree type = TREE_TYPE (to);
	      tree size
	        = SUBSTITUTE_PLACEHOLDER_IN_EXPR (TYPE_SIZE_UNIT (type), to);
	      tree to_ptr = build_fold_addr_expr (to);
	      tree t = builtin_decl_explicit (BUILT_IN_MEMSET);
	      if (TREE_CODE (value) == INTEGER_CST)
		{
		  tree mask
		    = build_int_cst (integer_type_node,
				     ((HOST_WIDE_INT) 1 << BITS_PER_UNIT) - 1);
		  value = int_const_binop (BIT_AND_EXPR, value, mask);
		}
	      gnu_result = build_call_expr (t, 3, to_ptr, value, size);
	    }

	  /* Otherwise build a regular assignment.  */
	  else
	    gnu_result
	      = build_binary_op (MODIFY_EXPR, NULL_TREE, gnu_lhs, gnu_rhs);

	  /* If the assignment type is a regular array and the two sides are
	     not completely disjoint, play safe and use memmove.  But don't do
	     it for a bit-packed array as it might not be byte-aligned.  */
	  if (TREE_CODE (gnu_result) == MODIFY_EXPR
	      && regular_array_type_p
	      && !(Forwards_OK (gnat_node) && Backwards_OK (gnat_node)))
	    {
	      tree to = TREE_OPERAND (gnu_result, 0);
	      tree from = TREE_OPERAND (gnu_result, 1);
	      tree type = TREE_TYPE (from);
	      tree size
	        = SUBSTITUTE_PLACEHOLDER_IN_EXPR (TYPE_SIZE_UNIT (type), from);
	      tree to_ptr = build_fold_addr_expr (to);
	      tree from_ptr = build_fold_addr_expr (from);
	      tree t = builtin_decl_explicit (BUILT_IN_MEMMOVE);
	      gnu_result = build_call_expr (t, 3, to_ptr, from_ptr, size);
	   }
	}
      break;

    case N_If_Statement:
      {
	tree *gnu_else_ptr; /* Point to put next "else if" or "else".  */

	/* Make the outer COND_EXPR.  Avoid non-determinism.  */
	gnu_result = build3 (COND_EXPR, void_type_node,
			     gnat_to_gnu (Condition (gnat_node)),
			     NULL_TREE, NULL_TREE);
	COND_EXPR_THEN (gnu_result)
	  = build_stmt_group (Then_Statements (gnat_node), false);
	TREE_SIDE_EFFECTS (gnu_result) = 1;
	gnu_else_ptr = &COND_EXPR_ELSE (gnu_result);

	/* Now make a COND_EXPR for each of the "else if" parts.  Put each
	   into the previous "else" part and point to where to put any
	   outer "else".  Also avoid non-determinism.  */
	if (Present (Elsif_Parts (gnat_node)))
	  for (gnat_temp = First (Elsif_Parts (gnat_node));
	       Present (gnat_temp); gnat_temp = Next (gnat_temp))
	    {
	      gnu_expr = build3 (COND_EXPR, void_type_node,
				 gnat_to_gnu (Condition (gnat_temp)),
				 NULL_TREE, NULL_TREE);
	      COND_EXPR_THEN (gnu_expr)
		= build_stmt_group (Then_Statements (gnat_temp), false);
	      TREE_SIDE_EFFECTS (gnu_expr) = 1;
	      set_expr_location_from_node (gnu_expr, gnat_temp);
	      *gnu_else_ptr = gnu_expr;
	      gnu_else_ptr = &COND_EXPR_ELSE (gnu_expr);
	    }

	*gnu_else_ptr = build_stmt_group (Else_Statements (gnat_node), false);
      }
      break;

    case N_Case_Statement:
      gnu_result = Case_Statement_to_gnu (gnat_node);
      break;

    case N_Loop_Statement:
      gnu_result = Loop_Statement_to_gnu (gnat_node);
      break;

    case N_Block_Statement:
      /* The only way to enter the block is to fall through to it.  */
      if (stmt_group_may_fallthru ())
	{
	  start_stmt_group ();
	  gnat_pushlevel ();
	  process_decls (Declarations (gnat_node), Empty, Empty, true, true);
	  add_stmt (gnat_to_gnu (Handled_Statement_Sequence (gnat_node)));
	  gnat_poplevel ();
	  gnu_result = end_stmt_group ();
	}
      else
	gnu_result = alloc_stmt_list ();
      break;

    case N_Exit_Statement:
      gnu_result
	= build2 (EXIT_STMT, void_type_node,
		  (Present (Condition (gnat_node))
		   ? gnat_to_gnu (Condition (gnat_node)) : NULL_TREE),
		  (Present (Name (gnat_node))
		   ? get_gnu_tree (Entity (Name (gnat_node)))
		   : LOOP_STMT_LABEL (gnu_loop_stack->last ()->stmt)));
      break;

    case N_Simple_Return_Statement:
      {
	tree gnu_ret_obj, gnu_ret_val;

	/* If the subprogram is a function, we must return the expression.  */
	if (Present (Expression (gnat_node)))
	  {
	    tree gnu_subprog_type = TREE_TYPE (current_function_decl);

	    /* If this function has copy-in/copy-out parameters parameters and
	       doesn't return by invisible reference, get the real object for
	       the return.  See Subprogram_Body_to_gnu.  */
	    if (TYPE_CI_CO_LIST (gnu_subprog_type)
		&& !TREE_ADDRESSABLE (gnu_subprog_type))
	      gnu_ret_obj = gnu_return_var_stack->last ();
	    else
	      gnu_ret_obj = DECL_RESULT (current_function_decl);

	    /* Get the GCC tree for the expression to be returned.  */
	    gnu_ret_val = gnat_to_gnu (Expression (gnat_node));

	    /* Do not remove the padding from GNU_RET_VAL if the inner type is
	       self-referential since we want to allocate the fixed size.  */
	    if (TREE_CODE (gnu_ret_val) == COMPONENT_REF
		&& type_is_padding_self_referential
		   (TREE_TYPE (TREE_OPERAND (gnu_ret_val, 0))))
	      gnu_ret_val = TREE_OPERAND (gnu_ret_val, 0);

	    /* If the function returns by direct reference, return a pointer
	       to the return value.  */
	    if (TYPE_RETURN_BY_DIRECT_REF_P (gnu_subprog_type)
		|| By_Ref (gnat_node))
	      gnu_ret_val = build_unary_op (ADDR_EXPR, NULL_TREE, gnu_ret_val);

	    /* Otherwise, if it returns an unconstrained array, we have to
	       allocate a new version of the result and return it.  */
	    else if (TYPE_RETURN_UNCONSTRAINED_P (gnu_subprog_type))
	      {
		gnu_ret_val = maybe_unconstrained_array (gnu_ret_val);

		/* And find out whether this is a candidate for Named Return
		   Value.  If so, record it.  */
		if (!TYPE_CI_CO_LIST (gnu_subprog_type) && optimize)
		  {
		    tree ret_val = gnu_ret_val;

		    /* Strip useless conversions around the return value.  */
		    if (gnat_useless_type_conversion (ret_val))
		      ret_val = TREE_OPERAND (ret_val, 0);

		    /* Strip unpadding around the return value.  */
		    if (TREE_CODE (ret_val) == COMPONENT_REF
			&& TYPE_IS_PADDING_P
			   (TREE_TYPE (TREE_OPERAND (ret_val, 0))))
		      ret_val = TREE_OPERAND (ret_val, 0);

		    /* Now apply the test to the return value.  */
		    if (return_value_ok_for_nrv_p (NULL_TREE, ret_val))
		      {
			if (!f_named_ret_val)
			  f_named_ret_val = BITMAP_GGC_ALLOC ();
			bitmap_set_bit (f_named_ret_val, DECL_UID (ret_val));
			if (!f_gnat_ret)
			  f_gnat_ret = gnat_node;
		      }
		  }

		gnu_ret_val = build_allocator (TREE_TYPE (gnu_ret_val),
					       gnu_ret_val,
					       TREE_TYPE (gnu_ret_obj),
					       Procedure_To_Call (gnat_node),
					       Storage_Pool (gnat_node),
					       gnat_node, false);
	      }

	    /* Otherwise, if it returns by invisible reference, dereference
	       the pointer it is passed using the type of the return value
	       and build the copy operation manually.  This ensures that we
	       don't copy too much data, for example if the return type is
	       unconstrained with a maximum size.  */
	    else if (TREE_ADDRESSABLE (gnu_subprog_type))
	      {
		tree gnu_ret_deref
		  = build_unary_op (INDIRECT_REF, TREE_TYPE (gnu_ret_val),
				    gnu_ret_obj);
		gnu_result = build2 (INIT_EXPR, void_type_node,
				     gnu_ret_deref, gnu_ret_val);
		add_stmt_with_node (gnu_result, gnat_node);
		gnu_ret_val = NULL_TREE;
	      }
	  }

	else
	  gnu_ret_obj = gnu_ret_val = NULL_TREE;

	/* If we have a return label defined, convert this into a branch to
	   that label.  The return proper will be handled elsewhere.  */
	if (gnu_return_label_stack->last ())
	  {
	    if (gnu_ret_val)
	      add_stmt (build_binary_op (MODIFY_EXPR, NULL_TREE, gnu_ret_obj,
					 gnu_ret_val));

	    gnu_result = build1 (GOTO_EXPR, void_type_node,
				 gnu_return_label_stack->last ());

	    /* When not optimizing, make sure the return is preserved.  */
	    if (!optimize && Comes_From_Source (gnat_node))
	      DECL_ARTIFICIAL (gnu_return_label_stack->last ()) = 0;
	  }

	/* Otherwise, build a regular return.  */
	else
	  gnu_result = build_return_expr (gnu_ret_obj, gnu_ret_val);
      }
      break;

    case N_Goto_Statement:
      gnu_result
	= build1 (GOTO_EXPR, void_type_node, gnat_to_gnu (Name (gnat_node)));
      break;

    /***************************/
    /* Chapter 6: Subprograms  */
    /***************************/

    case N_Subprogram_Declaration:
      /* Unless there is a freeze node, declare the entity.  We consider
	 this a definition even though we're not generating code for the
	 subprogram because we will be making the corresponding GCC node.
	 When there is a freeze node, it is considered the definition of
	 the subprogram and we do nothing until after it is encountered.
	 That's an efficiency issue: the types involved in the profile
	 are far more likely to be frozen between the declaration and
	 the freeze node than before the declaration, so we save some
	 updates of the GCC node by waiting until the freeze node.
	 The counterpart is that we assume that there is no reference
	 to the subprogram between the declaration and the freeze node
	 in the expanded code; otherwise, it will be interpreted as an
	 external reference and very likely give rise to a link failure.  */
      if (No (Freeze_Node (Defining_Entity (Specification (gnat_node)))))
	gnat_to_gnu_entity (Defining_Entity (Specification (gnat_node)),
			    NULL_TREE, true);
      gnu_result = alloc_stmt_list ();
      break;

    case N_Abstract_Subprogram_Declaration:
      /* This subprogram doesn't exist for code generation purposes, but we
	 have to elaborate the types of any parameters and result, unless
	 they are imported types (nothing to generate in this case).

	 The parameter list may contain types with freeze nodes, e.g. not null
	 subtypes, so the subprogram itself may carry a freeze node, in which
	 case its elaboration must be deferred.  */

      /* Process the parameter types first.  */
      if (No (Freeze_Node (Defining_Entity (Specification (gnat_node)))))
      for (gnat_temp
	   = First_Formal_With_Extras
	      (Defining_Entity (Specification (gnat_node)));
	   Present (gnat_temp);
	   gnat_temp = Next_Formal_With_Extras (gnat_temp))
	if (Is_Itype (Etype (gnat_temp))
	    && !From_Limited_With (Etype (gnat_temp)))
	  gnat_to_gnu_entity (Etype (gnat_temp), NULL_TREE, false);

      /* Then the result type, set to Standard_Void_Type for procedures.  */
      {
	Entity_Id gnat_temp_type
	  = Etype (Defining_Entity (Specification (gnat_node)));

	if (Is_Itype (gnat_temp_type) && !From_Limited_With (gnat_temp_type))
	  gnat_to_gnu_entity (Etype (gnat_temp_type), NULL_TREE, false);
      }

      gnu_result = alloc_stmt_list ();
      break;

    case N_Defining_Program_Unit_Name:
      /* For a child unit identifier go up a level to get the specification.
	 We get this when we try to find the spec of a child unit package
	 that is the compilation unit being compiled.  */
      gnu_result = gnat_to_gnu (Parent (gnat_node));
      break;

    case N_Subprogram_Body:
      Subprogram_Body_to_gnu (gnat_node);
      gnu_result = alloc_stmt_list ();
      break;

    case N_Function_Call:
    case N_Procedure_Call_Statement:
      gnu_result = Call_to_gnu (gnat_node, &gnu_result_type, NULL_TREE,
				false, false, false);
      break;

    /************************/
    /* Chapter 7: Packages  */
    /************************/

    case N_Package_Declaration:
      gnu_result = gnat_to_gnu (Specification (gnat_node));
      break;

    case N_Package_Specification:

      start_stmt_group ();
      process_decls (Visible_Declarations (gnat_node),
		     Private_Declarations (gnat_node), Empty, true, true);
      gnu_result = end_stmt_group ();
      break;

    case N_Package_Body:

      /* If this is the body of a generic package - do nothing.  */
      if (Ekind (Corresponding_Spec (gnat_node)) == E_Generic_Package)
	{
	  gnu_result = alloc_stmt_list ();
	  break;
	}

      start_stmt_group ();
      process_decls (Declarations (gnat_node), Empty, Empty, true, true);

      if (Present (Handled_Statement_Sequence (gnat_node)))
	add_stmt (gnat_to_gnu (Handled_Statement_Sequence (gnat_node)));

      gnu_result = end_stmt_group ();
      break;

    /********************************/
    /* Chapter 8: Visibility Rules  */
    /********************************/

    case N_Use_Package_Clause:
    case N_Use_Type_Clause:
      /* Nothing to do here - but these may appear in list of declarations.  */
      gnu_result = alloc_stmt_list ();
      break;

    /*********************/
    /* Chapter 9: Tasks  */
    /*********************/

    case N_Protected_Type_Declaration:
      gnu_result = alloc_stmt_list ();
      break;

    case N_Single_Task_Declaration:
      gnat_to_gnu_entity (Defining_Entity (gnat_node), NULL_TREE, true);
      gnu_result = alloc_stmt_list ();
      break;

    /*********************************************************/
    /* Chapter 10: Program Structure and Compilation Issues  */
    /*********************************************************/

    case N_Compilation_Unit:
      /* This is not called for the main unit on which gigi is invoked.  */
      Compilation_Unit_to_gnu (gnat_node);
      gnu_result = alloc_stmt_list ();
      break;

    case N_Subunit:
      gnu_result = gnat_to_gnu (Proper_Body (gnat_node));
      break;

    case N_Entry_Body:
    case N_Protected_Body:
    case N_Task_Body:
      /* These nodes should only be present when annotating types.  */
      gcc_assert (type_annotate_only);
      process_decls (Declarations (gnat_node), Empty, Empty, true, true);
      gnu_result = alloc_stmt_list ();
      break;

    case N_Subprogram_Body_Stub:
    case N_Package_Body_Stub:
    case N_Protected_Body_Stub:
    case N_Task_Body_Stub:
      /* Simply process whatever unit is being inserted.  */
      if (Present (Library_Unit (gnat_node)))
	gnu_result = gnat_to_gnu (Unit (Library_Unit (gnat_node)));
      else
	{
	  gcc_assert (type_annotate_only);
	  gnu_result = alloc_stmt_list ();
	}
      break;

    /***************************/
    /* Chapter 11: Exceptions  */
    /***************************/

    case N_Handled_Sequence_Of_Statements:
      /* If there is an At_End procedure attached to this node, and the EH
	 mechanism is front-end, we must have at least a corresponding At_End
	 handler, unless the No_Exception_Handlers restriction is set.  */
      gcc_assert (type_annotate_only
		  || !Front_End_Exceptions ()
		  || No (At_End_Proc (gnat_node))
		  || Present (Exception_Handlers (gnat_node))
		  || No_Exception_Handlers_Set ());

      gnu_result = Handled_Sequence_Of_Statements_to_gnu (gnat_node);
      break;

    case N_Exception_Handler:
      if (Exception_Mechanism == Front_End_SJLJ)
	gnu_result = Exception_Handler_to_gnu_fe_sjlj (gnat_node);
      else if (Back_End_Exceptions ())
	gnu_result = Exception_Handler_to_gnu_gcc (gnat_node);
      else
	gcc_unreachable ();
      break;

    case N_Raise_Statement:
      /* Only for reraise in back-end exceptions mode.  */
      gcc_assert (No (Name (gnat_node))
                  && Back_End_Exceptions ());

      start_stmt_group ();
      gnat_pushlevel ();

      /* Clear the current exception pointer so that the occurrence won't be
	 deallocated.  */
      gnu_expr = create_var_decl (get_identifier ("SAVED_EXPTR"), NULL_TREE,
				  ptr_type_node, gnu_incoming_exc_ptr,
				  false, false, false, false, false,
				  true, true, NULL, gnat_node);

      add_stmt (build_binary_op (MODIFY_EXPR, NULL_TREE, gnu_incoming_exc_ptr,
				 build_int_cst (ptr_type_node, 0)));
      add_stmt (build_call_n_expr (reraise_zcx_decl, 1, gnu_expr));
      gnat_poplevel ();
      gnu_result = end_stmt_group ();
      break;

    case N_Push_Constraint_Error_Label:
      push_exception_label_stack (&gnu_constraint_error_label_stack,
				  Exception_Label (gnat_node));
      break;

    case N_Push_Storage_Error_Label:
      push_exception_label_stack (&gnu_storage_error_label_stack,
				  Exception_Label (gnat_node));
      break;

    case N_Push_Program_Error_Label:
      push_exception_label_stack (&gnu_program_error_label_stack,
				  Exception_Label (gnat_node));
      break;

    case N_Pop_Constraint_Error_Label:
      gnu_constraint_error_label_stack->pop ();
      break;

    case N_Pop_Storage_Error_Label:
      gnu_storage_error_label_stack->pop ();
      break;

    case N_Pop_Program_Error_Label:
      gnu_program_error_label_stack->pop ();
      break;

    /******************************/
    /* Chapter 12: Generic Units  */
    /******************************/

    case N_Generic_Function_Renaming_Declaration:
    case N_Generic_Package_Renaming_Declaration:
    case N_Generic_Procedure_Renaming_Declaration:
    case N_Generic_Package_Declaration:
    case N_Generic_Subprogram_Declaration:
    case N_Package_Instantiation:
    case N_Procedure_Instantiation:
    case N_Function_Instantiation:
      /* These nodes can appear on a declaration list but there is nothing to
	 to be done with them.  */
      gnu_result = alloc_stmt_list ();
      break;

    /**************************************************/
    /* Chapter 13: Representation Clauses and         */
    /*             Implementation-Dependent Features  */
    /**************************************************/

    case N_Attribute_Definition_Clause:
      gnu_result = alloc_stmt_list ();

      /* The only one we need to deal with is 'Address since, for the others,
	 the front-end puts the information elsewhere.  */
      if (Get_Attribute_Id (Chars (gnat_node)) != Attr_Address)
	break;

      /* And we only deal with 'Address if the object has a Freeze node.  */
      gnat_temp = Entity (Name (gnat_node));
      if (No (Freeze_Node (gnat_temp)))
	break;

      /* Get the value to use as the address and save it as the equivalent
	 for the object.  When it is frozen, gnat_to_gnu_entity will do the
	 right thing.  */
      save_gnu_tree (gnat_temp, gnat_to_gnu (Expression (gnat_node)), true);
      break;

    case N_Enumeration_Representation_Clause:
    case N_Record_Representation_Clause:
    case N_At_Clause:
      /* We do nothing with these.  SEM puts the information elsewhere.  */
      gnu_result = alloc_stmt_list ();
      break;

    case N_Code_Statement:
      if (!type_annotate_only)
	{
	  tree gnu_template = gnat_to_gnu (Asm_Template (gnat_node));
	  tree gnu_inputs = NULL_TREE, gnu_outputs = NULL_TREE;
	  tree gnu_clobbers = NULL_TREE, tail;
	  bool allows_mem, allows_reg, fake;
	  int ninputs, noutputs, i;
	  const char **oconstraints;
	  const char *constraint;
	  char *clobber;

	  /* First retrieve the 3 operand lists built by the front-end.  */
	  Setup_Asm_Outputs (gnat_node);
	  while (Present (gnat_temp = Asm_Output_Variable ()))
	    {
	      tree gnu_value = gnat_to_gnu (gnat_temp);
	      tree gnu_constr = build_tree_list (NULL_TREE, gnat_to_gnu
						 (Asm_Output_Constraint ()));

	      gnu_outputs = tree_cons (gnu_constr, gnu_value, gnu_outputs);
	      Next_Asm_Output ();
	    }

	  Setup_Asm_Inputs (gnat_node);
	  while (Present (gnat_temp = Asm_Input_Value ()))
	    {
	      tree gnu_value = gnat_to_gnu (gnat_temp);
	      tree gnu_constr = build_tree_list (NULL_TREE, gnat_to_gnu
						 (Asm_Input_Constraint ()));

	      gnu_inputs = tree_cons (gnu_constr, gnu_value, gnu_inputs);
	      Next_Asm_Input ();
	    }

	  Clobber_Setup (gnat_node);
	  while ((clobber = Clobber_Get_Next ()))
	    gnu_clobbers
	      = tree_cons (NULL_TREE,
			   build_string (strlen (clobber) + 1, clobber),
			   gnu_clobbers);

	  /* Then perform some standard checking and processing on the
	     operands.  In particular, mark them addressable if needed.  */
	  gnu_outputs = nreverse (gnu_outputs);
	  noutputs = list_length (gnu_outputs);
	  gnu_inputs = nreverse (gnu_inputs);
	  ninputs = list_length (gnu_inputs);
	  oconstraints = XALLOCAVEC (const char *, noutputs);

	  for (i = 0, tail = gnu_outputs; tail; ++i, tail = TREE_CHAIN (tail))
	    {
	      tree output = TREE_VALUE (tail);
	      constraint
		= TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (tail)));
	      oconstraints[i] = constraint;

	      if (parse_output_constraint (&constraint, i, ninputs, noutputs,
					   &allows_mem, &allows_reg, &fake))
		{
		  /* If the operand is going to end up in memory,
		     mark it addressable.  Note that we don't test
		     allows_mem like in the input case below; this
		     is modelled on the C front-end.  */
		  if (!allows_reg)
		    {
		      output = remove_conversions (output, false);
		      if (TREE_CODE (output) == CONST_DECL
			  && DECL_CONST_CORRESPONDING_VAR (output))
			output = DECL_CONST_CORRESPONDING_VAR (output);
		      if (!gnat_mark_addressable (output))
			output = error_mark_node;
		    }
		}
	      else
		output = error_mark_node;

	      TREE_VALUE (tail) = output;
	    }

	  for (i = 0, tail = gnu_inputs; tail; ++i, tail = TREE_CHAIN (tail))
	    {
	      tree input = TREE_VALUE (tail);
	      constraint
		= TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (tail)));

	      if (parse_input_constraint (&constraint, i, ninputs, noutputs,
					  0, oconstraints,
					  &allows_mem, &allows_reg))
		{
		  /* If the operand is going to end up in memory,
		     mark it addressable.  */
		  if (!allows_reg && allows_mem)
		    {
		      input = remove_conversions (input, false);
		      if (TREE_CODE (input) == CONST_DECL
			  && DECL_CONST_CORRESPONDING_VAR (input))
			input = DECL_CONST_CORRESPONDING_VAR (input);
		      if (!gnat_mark_addressable (input))
			input = error_mark_node;
		    }
		}
	      else
		input = error_mark_node;

	      TREE_VALUE (tail) = input;
	    }

	  gnu_result = build5 (ASM_EXPR,  void_type_node,
			       gnu_template, gnu_outputs,
			       gnu_inputs, gnu_clobbers, NULL_TREE);
	  ASM_VOLATILE_P (gnu_result) = Is_Asm_Volatile (gnat_node);
	}
      else
	gnu_result = alloc_stmt_list ();

      break;

    /****************/
    /* Added Nodes  */
    /****************/

    case N_Expression_With_Actions:
      /* This construct doesn't define a scope so we don't push a binding
	 level around the statement list, but we wrap it in a SAVE_EXPR to
	 protect it from unsharing.  Elaborate the expression as part of the
	 same statement group as the actions so that the type declaration
	 gets inserted there as well.  This ensures that the type elaboration
	 code is issued past the actions computing values on which it might
	 depend.  */
      start_stmt_group ();
      add_stmt_list (Actions (gnat_node));
      gnu_expr = gnat_to_gnu (Expression (gnat_node));
      gnu_result = end_stmt_group ();

      gnu_result = build1 (SAVE_EXPR, void_type_node, gnu_result);
      TREE_SIDE_EFFECTS (gnu_result) = 1;

      gnu_result
	= build_compound_expr (TREE_TYPE (gnu_expr), gnu_result, gnu_expr);
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      break;

    case N_Freeze_Entity:
      start_stmt_group ();
      process_freeze_entity (gnat_node);
      process_decls (Actions (gnat_node), Empty, Empty, true, true);
      gnu_result = end_stmt_group ();
      break;

    case N_Freeze_Generic_Entity:
      gnu_result = alloc_stmt_list ();
      break;

    case N_Itype_Reference:
      if (!present_gnu_tree (Itype (gnat_node)))
	process_type (Itype (gnat_node));
      gnu_result = alloc_stmt_list ();
      break;

    case N_Free_Statement:
      if (!type_annotate_only)
	{
	  tree gnu_ptr = gnat_to_gnu (Expression (gnat_node));
	  tree gnu_ptr_type = TREE_TYPE (gnu_ptr);
	  tree gnu_obj_type, gnu_actual_obj_type;

	  /* If this is a thin pointer, we must first dereference it to create
	     a fat pointer, then go back below to a thin pointer.  The reason
	     for this is that we need to have a fat pointer someplace in order
	     to properly compute the size.  */
	  if (TYPE_IS_THIN_POINTER_P (TREE_TYPE (gnu_ptr)))
	    gnu_ptr = build_unary_op (ADDR_EXPR, NULL_TREE,
				      build_unary_op (INDIRECT_REF, NULL_TREE,
						      gnu_ptr));

	  /* If this is a fat pointer, the object must have been allocated with
	     the template in front of the array.  So pass the template address,
	     and get the total size; do it by converting to a thin pointer.  */
	  if (TYPE_IS_FAT_POINTER_P (TREE_TYPE (gnu_ptr)))
	    gnu_ptr
	      = convert (build_pointer_type
			 (TYPE_OBJECT_RECORD_TYPE
			  (TYPE_UNCONSTRAINED_ARRAY (TREE_TYPE (gnu_ptr)))),
			 gnu_ptr);

	  gnu_obj_type = TREE_TYPE (TREE_TYPE (gnu_ptr));

	  /* If this is a thin pointer, the object must have been allocated with
	     the template in front of the array.  So pass the template address,
	     and get the total size.  */
	  if (TYPE_IS_THIN_POINTER_P (TREE_TYPE (gnu_ptr)))
	    gnu_ptr
	      = build_binary_op (POINTER_PLUS_EXPR, TREE_TYPE (gnu_ptr),
				 gnu_ptr,
				 fold_build1 (NEGATE_EXPR, sizetype,
					      byte_position
					      (DECL_CHAIN
					       TYPE_FIELDS ((gnu_obj_type)))));

	  /* If we have a special dynamic constrained subtype on the node, use
	     it to compute the size; otherwise, use the designated subtype.  */
	  if (Present (Actual_Designated_Subtype (gnat_node)))
	    {
	      gnu_actual_obj_type
		= gnat_to_gnu_type (Actual_Designated_Subtype (gnat_node));

	      if (TYPE_IS_FAT_OR_THIN_POINTER_P (gnu_ptr_type))
		gnu_actual_obj_type
		  = build_unc_object_type_from_ptr (gnu_ptr_type,
						    gnu_actual_obj_type,
						    get_identifier ("DEALLOC"),
						    false);
	    }
	  else
	    gnu_actual_obj_type = gnu_obj_type;

	  tree gnu_size = TYPE_SIZE_UNIT (gnu_actual_obj_type);
	  gnu_size = SUBSTITUTE_PLACEHOLDER_IN_EXPR (gnu_size, gnu_ptr);

	  gnu_result
	      = build_call_alloc_dealloc (gnu_ptr, gnu_size, gnu_obj_type,
					  Procedure_To_Call (gnat_node),
					  Storage_Pool (gnat_node),
					  gnat_node);
	}
      break;

    case N_Raise_Constraint_Error:
    case N_Raise_Program_Error:
    case N_Raise_Storage_Error:
      if (type_annotate_only)
	gnu_result = alloc_stmt_list ();
      else
	gnu_result = Raise_Error_to_gnu (gnat_node, &gnu_result_type);
      break;

    case N_Validate_Unchecked_Conversion:
      /* The only validation we currently do on an unchecked conversion is
	 that of aliasing assumptions.  */
      if (flag_strict_aliasing)
	gnat_validate_uc_list.safe_push (gnat_node);
      gnu_result = alloc_stmt_list ();
      break;

    case N_Function_Specification:
    case N_Procedure_Specification:
    case N_Op_Concat:
    case N_Component_Association:
      /* These nodes should only be present when annotating types.  */
      gcc_assert (type_annotate_only);
      gnu_result = alloc_stmt_list ();
      break;

    default:
      /* Other nodes are not supposed to reach here.  */
      gcc_unreachable ();
    }

  /* If we pushed the processing of the elaboration routine, pop it back.  */
  if (went_into_elab_proc)
    current_function_decl = NULL_TREE;

  /* When not optimizing, turn boolean rvalues B into B != false tests
     so that we can put the location information of the reference to B on
     the inequality operator for better debug info.  */
  if (!optimize
      && TREE_CODE (gnu_result) != INTEGER_CST
      && TREE_CODE (gnu_result) != TYPE_DECL
      && (kind == N_Identifier
	  || kind == N_Expanded_Name
	  || kind == N_Explicit_Dereference
	  || kind == N_Function_Call
	  || kind == N_Indexed_Component
	  || kind == N_Selected_Component)
      && TREE_CODE (get_base_type (gnu_result_type)) == BOOLEAN_TYPE
      && !lvalue_required_p (gnat_node, gnu_result_type, false, false, false))
    {
      gnu_result
	= build_binary_op (NE_EXPR, gnu_result_type,
			   convert (gnu_result_type, gnu_result),
			   convert (gnu_result_type, boolean_false_node));
      if (TREE_CODE (gnu_result) != INTEGER_CST)
	set_gnu_expr_location_from_node (gnu_result, gnat_node);
    }

  /* Set the location information on the result if it's not a simple name.
     Note that we may have no result if we tried to build a CALL_EXPR node
     to a procedure with no side-effects and optimization is enabled.  */
  else if (kind != N_Identifier && gnu_result && EXPR_P (gnu_result))
    set_gnu_expr_location_from_node (gnu_result, gnat_node);

  /* If we're supposed to return something of void_type, it means we have
     something we're elaborating for effect, so just return.  */
  if (TREE_CODE (gnu_result_type) == VOID_TYPE)
    return gnu_result;

  /* If the result is a constant that overflowed, raise Constraint_Error.  */
  if (TREE_CODE (gnu_result) == INTEGER_CST && TREE_OVERFLOW (gnu_result))
    {
      post_error ("?`Constraint_Error` will be raised at run time", gnat_node);
      gnu_result
	= build1 (NULL_EXPR, gnu_result_type,
		  build_call_raise (CE_Overflow_Check_Failed, gnat_node,
				    N_Raise_Constraint_Error));
    }

  /* If the result has side-effects and is of an unconstrained type, protect
     the expression in case it will be referenced multiple times, i.e. for
     its value and to compute the size of an object.  But do it neither for
     an object nor a renaming declaration, nor a return statement of a call
     to a function that returns an unconstrained record type with default
     discriminant, because there is no size to be computed in these cases
     and this will create a useless temporary.  We must do this before any
     conversions.  */
  if (TREE_SIDE_EFFECTS (gnu_result)
      && (TREE_CODE (gnu_result_type) == UNCONSTRAINED_ARRAY_TYPE
	  || CONTAINS_PLACEHOLDER_P (TYPE_SIZE (gnu_result_type)))
      && !(TREE_CODE (gnu_result) == CALL_EXPR
	   && type_is_padding_self_referential (TREE_TYPE (gnu_result))
	   && (Nkind (Parent (gnat_node)) == N_Object_Declaration
	       || Nkind (Parent (gnat_node)) == N_Object_Renaming_Declaration
	       || Nkind (Parent (gnat_node)) == N_Simple_Return_Statement)))
    gnu_result = gnat_protect_expr (gnu_result);

  /* Now convert the result to the result type, unless we are in one of the
     following cases:

       1. If this is the LHS of an assignment or an actual parameter of a
	  call, return the result almost unmodified since the RHS will have
	  to be converted to our type in that case, unless the result type
	  has a simpler size.  Likewise if there is just a no-op unchecked
	  conversion in-between.  Similarly, don't convert integral types
	  that are the operands of an unchecked conversion since we need
	  to ignore those conversions (for 'Valid).

       2. If we have a label (which doesn't have any well-defined type), a
	  field or an error, return the result almost unmodified.  Similarly,
	  if the two types are record types with the same name, don't convert.
	  This will be the case when we are converting from a packable version
	  of a type to its original type and we need those conversions to be
	  NOPs in order for assignments into these types to work properly.

       3. If the type is void or if we have no result, return error_mark_node
	  to show we have no result.

       4. If this is a call to a function that returns with variable size and
	  the call is used as the expression in either an object or a renaming
	  declaration, return the result unmodified because we want to use the
	  return slot optimization in this case.

       5. Finally, if the type of the result is already correct.  */

  if (Present (Parent (gnat_node))
      && (lhs_or_actual_p (gnat_node)
	  || (Nkind (Parent (gnat_node)) == N_Unchecked_Type_Conversion
	      && unchecked_conversion_nop (Parent (gnat_node)))
	  || (Nkind (Parent (gnat_node)) == N_Unchecked_Type_Conversion
	      && !AGGREGATE_TYPE_P (gnu_result_type)
	      && !AGGREGATE_TYPE_P (TREE_TYPE (gnu_result))))
      && !(TYPE_SIZE (gnu_result_type)
	   && TYPE_SIZE (TREE_TYPE (gnu_result))
	   && (AGGREGATE_TYPE_P (gnu_result_type)
	       == AGGREGATE_TYPE_P (TREE_TYPE (gnu_result)))
	   && ((TREE_CODE (TYPE_SIZE (gnu_result_type)) == INTEGER_CST
		&& (TREE_CODE (TYPE_SIZE (TREE_TYPE (gnu_result)))
		    != INTEGER_CST))
	       || (TREE_CODE (TYPE_SIZE (gnu_result_type)) != INTEGER_CST
		   && !CONTAINS_PLACEHOLDER_P (TYPE_SIZE (gnu_result_type))
		   && (CONTAINS_PLACEHOLDER_P
		       (TYPE_SIZE (TREE_TYPE (gnu_result))))))
	   && !(TREE_CODE (gnu_result_type) == RECORD_TYPE
		&& TYPE_JUSTIFIED_MODULAR_P (gnu_result_type))))
    {
      /* Remove padding only if the inner object is of self-referential
	 size: in that case it must be an object of unconstrained type
	 with a default discriminant and we want to avoid copying too
	 much data.  */
      if (type_is_padding_self_referential (TREE_TYPE (gnu_result)))
	gnu_result = convert (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (gnu_result))),
			      gnu_result);
    }

  else if (TREE_CODE (gnu_result) == LABEL_DECL
	   || TREE_CODE (gnu_result) == FIELD_DECL
	   || TREE_CODE (gnu_result) == ERROR_MARK
	   || (TYPE_NAME (gnu_result_type)
	       == TYPE_NAME (TREE_TYPE (gnu_result))
	       && TREE_CODE (gnu_result_type) == RECORD_TYPE
	       && TREE_CODE (TREE_TYPE (gnu_result)) == RECORD_TYPE))
    {
      /* Remove any padding.  */
      if (TYPE_IS_PADDING_P (TREE_TYPE (gnu_result)))
	gnu_result = convert (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (gnu_result))),
			      gnu_result);
    }

  else if (gnu_result == error_mark_node || gnu_result_type == void_type_node)
    gnu_result = error_mark_node;

  else if (Present (Parent (gnat_node))
	   && (Nkind (Parent (gnat_node)) == N_Object_Declaration
	       || Nkind (Parent (gnat_node)) == N_Object_Renaming_Declaration)
	   && TREE_CODE (gnu_result) == CALL_EXPR
	   && return_type_with_variable_size_p (TREE_TYPE (gnu_result)))
    ;

  else if (TREE_TYPE (gnu_result) != gnu_result_type)
    gnu_result = convert (gnu_result_type, gnu_result);

  /* We don't need any NOP_EXPR or NON_LVALUE_EXPR on the result.  */
  while ((TREE_CODE (gnu_result) == NOP_EXPR
	  || TREE_CODE (gnu_result) == NON_LVALUE_EXPR)
	 && TREE_TYPE (TREE_OPERAND (gnu_result, 0)) == TREE_TYPE (gnu_result))
    gnu_result = TREE_OPERAND (gnu_result, 0);

  return gnu_result;
}

/* Similar to gnat_to_gnu, but discard any object that might be created in
   the course of the translation of GNAT_NODE, which must be an "external"
   expression in the sense that it will be elaborated elsewhere.  */

tree
gnat_to_gnu_external (Node_Id gnat_node)
{
  const int save_force_global = force_global;
  bool went_into_elab_proc = false;

  /* Force the local context and create a fake scope that we zap
     at the end so declarations will not be stuck either in the
     global varpool or in the current scope.  */
  if (!current_function_decl)
    {
      current_function_decl = get_elaboration_procedure ();
      went_into_elab_proc = true;
    }
  force_global = 0;
  gnat_pushlevel ();

  tree gnu_result = gnat_to_gnu (gnat_node);

  gnat_zaplevel ();
  force_global = save_force_global;
  if (went_into_elab_proc)
    current_function_decl = NULL_TREE;

  /* Do not import locations from external units.  */
  if (gnu_result && EXPR_P (gnu_result))
    SET_EXPR_LOCATION (gnu_result, UNKNOWN_LOCATION);

  return gnu_result;
}

/* Subroutine of above to push the exception label stack.  GNU_STACK is
   a pointer to the stack to update and GNAT_LABEL, if present, is the
   label to push onto the stack.  */

static void
push_exception_label_stack (vec<tree, va_gc> **gnu_stack, Entity_Id gnat_label)
{
  tree gnu_label = (Present (gnat_label)
		    ? gnat_to_gnu_entity (gnat_label, NULL_TREE, false)
		    : NULL_TREE);

  vec_safe_push (*gnu_stack, gnu_label);
}

/* Return true if the statement list STMT_LIST is empty.  */

static bool
empty_stmt_list_p (tree stmt_list)
{
  tree_stmt_iterator tsi;

  for (tsi = tsi_start (stmt_list); !tsi_end_p (tsi); tsi_next (&tsi))
    {
      tree stmt = tsi_stmt (tsi);

      /* Anything else than an empty STMT_STMT counts as something.  */
      if (TREE_CODE (stmt) != STMT_STMT || STMT_STMT_STMT (stmt))
	return false;
    }

  return true;
}

/* Record the current code position in GNAT_NODE.  */

static void
record_code_position (Node_Id gnat_node)
{
  tree stmt_stmt = build1 (STMT_STMT, void_type_node, NULL_TREE);

  add_stmt_with_node (stmt_stmt, gnat_node);
  save_gnu_tree (gnat_node, stmt_stmt, true);
}

/* Insert the code for GNAT_NODE at the position saved for that node.  */

static void
insert_code_for (Node_Id gnat_node)
{
  tree code = gnat_to_gnu (gnat_node);

  /* It's too late to remove the STMT_STMT itself at this point.  */
  if (!empty_stmt_list_p (code))
    STMT_STMT_STMT (get_gnu_tree (gnat_node)) = code;

  save_gnu_tree (gnat_node, NULL_TREE, true);
}

/* Start a new statement group chained to the previous group.  */

void
start_stmt_group (void)
{
  struct stmt_group *group = stmt_group_free_list;

  /* First see if we can get one from the free list.  */
  if (group)
    stmt_group_free_list = group->previous;
  else
    group = ggc_alloc<stmt_group> ();

  group->previous = current_stmt_group;
  group->stmt_list = group->block = group->cleanups = NULL_TREE;
  current_stmt_group = group;
}

/* Add GNU_STMT to the current statement group.  If it is an expression with
   no effects, it is ignored.  */

void
add_stmt (tree gnu_stmt)
{
  append_to_statement_list (gnu_stmt, &current_stmt_group->stmt_list);
}

/* Similar, but the statement is always added, regardless of side-effects.  */

void
add_stmt_force (tree gnu_stmt)
{
  append_to_statement_list_force (gnu_stmt, &current_stmt_group->stmt_list);
}

/* Like add_stmt, but set the location of GNU_STMT to that of GNAT_NODE.  */

void
add_stmt_with_node (tree gnu_stmt, Node_Id gnat_node)
{
  /* Do not emit a location for renamings that come from generic instantiation,
     they are likely to disturb debugging.  */
  if (Present (gnat_node)
      && !renaming_from_generic_instantiation_p (gnat_node))
    set_expr_location_from_node (gnu_stmt, gnat_node);
  add_stmt (gnu_stmt);
}

/* Similar, but the statement is always added, regardless of side-effects.  */

void
add_stmt_with_node_force (tree gnu_stmt, Node_Id gnat_node)
{
  if (Present (gnat_node))
    set_expr_location_from_node (gnu_stmt, gnat_node);
  add_stmt_force (gnu_stmt);
}

/* Add a declaration statement for GNU_DECL to the current statement group.
   Get SLOC from Entity_Id.  */

void
add_decl_expr (tree gnu_decl, Entity_Id gnat_entity)
{
  tree type = TREE_TYPE (gnu_decl);
  tree gnu_stmt, gnu_init;

  /* If this is a variable that Gigi is to ignore, we may have been given
     an ERROR_MARK.  So test for it.  We also might have been given a
     reference for a renaming.  So only do something for a decl.  Also
     ignore a TYPE_DECL for an UNCONSTRAINED_ARRAY_TYPE.  */
  if (!DECL_P (gnu_decl)
      || (TREE_CODE (gnu_decl) == TYPE_DECL
	  && TREE_CODE (type) == UNCONSTRAINED_ARRAY_TYPE))
    return;

  gnu_stmt = build1 (DECL_EXPR, void_type_node, gnu_decl);

  /* If we are external or global, we don't want to output the DECL_EXPR for
     this DECL node since we already have evaluated the expressions in the
     sizes and positions as globals and doing it again would be wrong.  */
  if (DECL_EXTERNAL (gnu_decl) || global_bindings_p ())
    {
      /* Mark everything as used to prevent node sharing with subprograms.
	 Note that walk_tree knows how to deal with TYPE_DECL, but neither
	 VAR_DECL nor CONST_DECL.  This appears to be somewhat arbitrary.  */
      MARK_VISITED (gnu_stmt);
      if (TREE_CODE (gnu_decl) == VAR_DECL
	  || TREE_CODE (gnu_decl) == CONST_DECL)
	{
	  MARK_VISITED (DECL_SIZE (gnu_decl));
	  MARK_VISITED (DECL_SIZE_UNIT (gnu_decl));
	  MARK_VISITED (DECL_INITIAL (gnu_decl));
	}
      /* In any case, we have to deal with our own TYPE_ADA_SIZE field.  */
      else if (TREE_CODE (gnu_decl) == TYPE_DECL
	       && RECORD_OR_UNION_TYPE_P (type)
	       && !TYPE_FAT_POINTER_P (type))
	MARK_VISITED (TYPE_ADA_SIZE (type));
    }
  else
    add_stmt_with_node (gnu_stmt, gnat_entity);

  /* If this is a variable and an initializer is attached to it, it must be
     valid for the context.  Similar to init_const in create_var_decl.  */
  if (TREE_CODE (gnu_decl) == VAR_DECL
      && (gnu_init = DECL_INITIAL (gnu_decl))
      && (!gnat_types_compatible_p (type, TREE_TYPE (gnu_init))
	  || (TREE_STATIC (gnu_decl)
	      && !initializer_constant_valid_p (gnu_init,
						TREE_TYPE (gnu_init)))))
    {
      DECL_INITIAL (gnu_decl) = NULL_TREE;
      if (TREE_READONLY (gnu_decl))
	{
	  TREE_READONLY (gnu_decl) = 0;
	  DECL_READONLY_ONCE_ELAB (gnu_decl) = 1;
	}

      /* If GNU_DECL has a padded type, convert it to the unpadded
	 type so the assignment is done properly.  */
      if (TYPE_IS_PADDING_P (type))
	gnu_decl = convert (TREE_TYPE (TYPE_FIELDS (type)), gnu_decl);

      gnu_stmt = build_binary_op (INIT_EXPR, NULL_TREE, gnu_decl, gnu_init);
      add_stmt_with_node (gnu_stmt, gnat_entity);
    }
}

/* Callback for walk_tree to mark the visited trees rooted at *TP.  */

static tree
mark_visited_r (tree *tp, int *walk_subtrees, void *data ATTRIBUTE_UNUSED)
{
  tree t = *tp;

  if (TREE_VISITED (t))
    *walk_subtrees = 0;

  /* Don't mark a dummy type as visited because we want to mark its sizes
     and fields once it's filled in.  */
  else if (!TYPE_IS_DUMMY_P (t))
    TREE_VISITED (t) = 1;

  if (TYPE_P (t))
    TYPE_SIZES_GIMPLIFIED (t) = 1;

  return NULL_TREE;
}

/* Mark nodes rooted at T with TREE_VISITED and types as having their
   sized gimplified.  We use this to indicate all variable sizes and
   positions in global types may not be shared by any subprogram.  */

void
mark_visited (tree t)
{
  walk_tree (&t, mark_visited_r, NULL, NULL);
}

/* Add GNU_CLEANUP, a cleanup action, to the current code group and
   set its location to that of GNAT_NODE if present, but with column info
   cleared so that conditional branches generated as part of the cleanup
   code do not interfere with coverage analysis tools.  */

static void
add_cleanup (tree gnu_cleanup, Node_Id gnat_node)
{
  if (Present (gnat_node))
    set_expr_location_from_node (gnu_cleanup, gnat_node, true);
  append_to_statement_list (gnu_cleanup, &current_stmt_group->cleanups);
}

/* Set the BLOCK node corresponding to the current code group to GNU_BLOCK.  */

void
set_block_for_group (tree gnu_block)
{
  gcc_assert (!current_stmt_group->block);
  current_stmt_group->block = gnu_block;
}

/* Return code corresponding to the current code group.  It is normally
   a STATEMENT_LIST, but may also be a BIND_EXPR or TRY_FINALLY_EXPR if
   BLOCK or cleanups were set.  */

tree
end_stmt_group (void)
{
  struct stmt_group *group = current_stmt_group;
  tree gnu_retval = group->stmt_list;

  /* If this is a null list, allocate a new STATEMENT_LIST.  Then, if there
     are cleanups, make a TRY_FINALLY_EXPR.  Last, if there is a BLOCK,
     make a BIND_EXPR.  Note that we nest in that because the cleanup may
     reference variables in the block.  */
  if (!gnu_retval)
    gnu_retval = alloc_stmt_list ();

  if (group->cleanups)
    gnu_retval = build2 (TRY_FINALLY_EXPR, void_type_node, gnu_retval,
			 group->cleanups);

  if (current_stmt_group->block)
    gnu_retval = build3 (BIND_EXPR, void_type_node, BLOCK_VARS (group->block),
			 gnu_retval, group->block);

  /* Remove this group from the stack and add it to the free list.  */
  current_stmt_group = group->previous;
  group->previous = stmt_group_free_list;
  stmt_group_free_list = group;

  return gnu_retval;
}

/* Return whether the current statement group may fall through.  */

static inline bool
stmt_group_may_fallthru (void)
{
  if (current_stmt_group->stmt_list)
    return block_may_fallthru (current_stmt_group->stmt_list);
  else
    return true;
}

/* Add a list of statements from GNAT_LIST, a possibly-empty list of
   statements.*/

static void
add_stmt_list (List_Id gnat_list)
{
  Node_Id gnat_node;

  if (Present (gnat_list))
    for (gnat_node = First (gnat_list); Present (gnat_node);
	 gnat_node = Next (gnat_node))
      add_stmt (gnat_to_gnu (gnat_node));
}

/* Build a tree from GNAT_LIST, a possibly-empty list of statements.
   If BINDING_P is true, push and pop a binding level around the list.  */

static tree
build_stmt_group (List_Id gnat_list, bool binding_p)
{
  start_stmt_group ();

  if (binding_p)
    gnat_pushlevel ();

  add_stmt_list (gnat_list);

  if (binding_p)
    gnat_poplevel ();

  return end_stmt_group ();
}

/* Generate GIMPLE in place for the expression at *EXPR_P.  */

int
gnat_gimplify_expr (tree *expr_p, gimple_seq *pre_p,
		    gimple_seq *post_p ATTRIBUTE_UNUSED)
{
  tree expr = *expr_p;
  tree type = TREE_TYPE (expr);
  tree op;

  if (IS_ADA_STMT (expr))
    return gnat_gimplify_stmt (expr_p);

  switch (TREE_CODE (expr))
    {
    case NULL_EXPR:
      /* If this is an aggregate type, build a null pointer of the appropriate
	 type and dereference it.  */
      if (AGGREGATE_TYPE_P (type)
	  || TREE_CODE (type) == UNCONSTRAINED_ARRAY_TYPE)
	*expr_p = build_unary_op (INDIRECT_REF, NULL_TREE,
				  convert (build_pointer_type (type),
					   integer_zero_node));
      /* Otherwise, just make a VAR_DECL.  */
      else
	{
	  *expr_p = create_tmp_var (type, NULL);
	  TREE_NO_WARNING (*expr_p) = 1;
	}

      gimplify_and_add (TREE_OPERAND (expr, 0), pre_p);
      return GS_OK;

    case UNCONSTRAINED_ARRAY_REF:
      /* We should only do this if we are just elaborating for side-effects,
	 but we can't know that yet.  */
      *expr_p = TREE_OPERAND (*expr_p, 0);
      return GS_OK;

    case ADDR_EXPR:
      op = TREE_OPERAND (expr, 0);

      /* If we are taking the address of a constant CONSTRUCTOR, make sure it
	 is put into static memory.  We know that it's going to be read-only
	 given the semantics we have and it must be in static memory when the
	 reference is in an elaboration procedure.  */
      if (TREE_CODE (op) == CONSTRUCTOR && TREE_CONSTANT (op))
	{
	  tree addr = build_fold_addr_expr (tree_output_constant_def (op));
	  *expr_p = fold_convert (type, addr);
	  return GS_ALL_DONE;
	}

      /* Replace atomic loads with their first argument.  That's necessary
	 because the gimplifier would create a temporary otherwise.  */
      if (TREE_SIDE_EFFECTS (op))
	while (handled_component_p (op) || CONVERT_EXPR_P (op))
	  {
	    tree inner = TREE_OPERAND (op, 0);
	    if (TREE_CODE (inner) == CALL_EXPR && call_is_atomic_load (inner))
	      {
		tree t = CALL_EXPR_ARG (inner, 0);
		if (TREE_CODE (t) == NOP_EXPR)
		  t = TREE_OPERAND (t, 0);
		if (TREE_CODE (t) == ADDR_EXPR)
		  TREE_OPERAND (op, 0) = TREE_OPERAND (t, 0);
		else
		  TREE_OPERAND (op, 0) = build_fold_indirect_ref (t);
	      }
	    else
	      op = inner;
	  }

      return GS_UNHANDLED;

    case VIEW_CONVERT_EXPR:
      op = TREE_OPERAND (expr, 0);

      /* If we are view-converting a CONSTRUCTOR or a call from an aggregate
	 type to a scalar one, explicitly create the local temporary.  That's
	 required if the type is passed by reference.  */
      if ((TREE_CODE (op) == CONSTRUCTOR || TREE_CODE (op) == CALL_EXPR)
	  && AGGREGATE_TYPE_P (TREE_TYPE (op))
	  && !AGGREGATE_TYPE_P (type))
	{
	  tree mod, new_var = create_tmp_var_raw (TREE_TYPE (op), "C");
	  gimple_add_tmp_var (new_var);

	  mod = build2 (INIT_EXPR, TREE_TYPE (new_var), new_var, op);
	  gimplify_and_add (mod, pre_p);

	  TREE_OPERAND (expr, 0) = new_var;
	  return GS_OK;
	}

      return GS_UNHANDLED;

    case DECL_EXPR:
      op = DECL_EXPR_DECL (expr);

      /* The expressions for the RM bounds must be gimplified to ensure that
	 they are properly elaborated.  See gimplify_decl_expr.  */
      if ((TREE_CODE (op) == TYPE_DECL || TREE_CODE (op) == VAR_DECL)
	  && !TYPE_SIZES_GIMPLIFIED (TREE_TYPE (op)))
	switch (TREE_CODE (TREE_TYPE (op)))
	  {
	  case INTEGER_TYPE:
	  case ENUMERAL_TYPE:
	  case BOOLEAN_TYPE:
	  case REAL_TYPE:
	    {
	      tree type = TYPE_MAIN_VARIANT (TREE_TYPE (op)), t, val;

	      val = TYPE_RM_MIN_VALUE (type);
	      if (val)
		{
		  gimplify_one_sizepos (&val, pre_p);
		  for (t = type; t; t = TYPE_NEXT_VARIANT (t))
		    SET_TYPE_RM_MIN_VALUE (t, val);
		}

	      val = TYPE_RM_MAX_VALUE (type);
	      if (val)
		{
		  gimplify_one_sizepos (&val, pre_p);
		  for (t = type; t; t = TYPE_NEXT_VARIANT (t))
		    SET_TYPE_RM_MAX_VALUE (t, val);
		}

	    }
	    break;

	  default:
	    break;
	  }

      /* ... fall through ... */

    default:
      return GS_UNHANDLED;
    }
}

/* Generate GIMPLE in place for the statement at *STMT_P.  */

static enum gimplify_status
gnat_gimplify_stmt (tree *stmt_p)
{
  tree stmt = *stmt_p;

  switch (TREE_CODE (stmt))
    {
    case STMT_STMT:
      *stmt_p = STMT_STMT_STMT (stmt);
      return GS_OK;

    case LOOP_STMT:
      {
	tree gnu_start_label = create_artificial_label (input_location);
	tree gnu_cond = LOOP_STMT_COND (stmt);
	tree gnu_update = LOOP_STMT_UPDATE (stmt);
	tree gnu_end_label = LOOP_STMT_LABEL (stmt);

	/* Build the condition expression from the test, if any.  */
	if (gnu_cond)
	  {
	    /* Deal with the optimization hints.  */
	    if (LOOP_STMT_IVDEP (stmt))
	      gnu_cond = build2 (ANNOTATE_EXPR, TREE_TYPE (gnu_cond), gnu_cond,
				 build_int_cst (integer_type_node,
						annot_expr_ivdep_kind));
	    if (LOOP_STMT_NO_VECTOR (stmt))
	      gnu_cond = build2 (ANNOTATE_EXPR, TREE_TYPE (gnu_cond), gnu_cond,
				 build_int_cst (integer_type_node,
						annot_expr_no_vector_kind));
	    if (LOOP_STMT_VECTOR (stmt))
	      gnu_cond = build2 (ANNOTATE_EXPR, TREE_TYPE (gnu_cond), gnu_cond,
				 build_int_cst (integer_type_node,
						annot_expr_vector_kind));

	    gnu_cond
	      = build3 (COND_EXPR, void_type_node, gnu_cond, NULL_TREE,
			build1 (GOTO_EXPR, void_type_node, gnu_end_label));
	  }

	/* Set to emit the statements of the loop.  */
	*stmt_p = NULL_TREE;

	/* We first emit the start label and then a conditional jump to the
	   end label if there's a top condition, then the update if it's at
	   the top, then the body of the loop, then a conditional jump to
	   the end label if there's a bottom condition, then the update if
	   it's at the bottom, and finally a jump to the start label and the
	   definition of the end label.  */
	append_to_statement_list (build1 (LABEL_EXPR, void_type_node,
					  gnu_start_label),
				  stmt_p);

        if (gnu_cond && !LOOP_STMT_BOTTOM_COND_P (stmt))
	  append_to_statement_list (gnu_cond, stmt_p);

        if (gnu_update && LOOP_STMT_TOP_UPDATE_P (stmt))
	  append_to_statement_list (gnu_update, stmt_p);

	append_to_statement_list (LOOP_STMT_BODY (stmt), stmt_p);

        if (gnu_cond && LOOP_STMT_BOTTOM_COND_P (stmt))
	  append_to_statement_list (gnu_cond, stmt_p);

        if (gnu_update && !LOOP_STMT_TOP_UPDATE_P (stmt))
	  append_to_statement_list (gnu_update, stmt_p);

	tree t = build1 (GOTO_EXPR, void_type_node, gnu_start_label);
	SET_EXPR_LOCATION (t, DECL_SOURCE_LOCATION (gnu_end_label));
	append_to_statement_list (t, stmt_p);

	append_to_statement_list (build1 (LABEL_EXPR, void_type_node,
					  gnu_end_label),
				  stmt_p);
	return GS_OK;
      }

    case EXIT_STMT:
      /* Build a statement to jump to the corresponding end label, then
	 see if it needs to be conditional.  */
      *stmt_p = build1 (GOTO_EXPR, void_type_node, EXIT_STMT_LABEL (stmt));
      if (EXIT_STMT_COND (stmt))
	*stmt_p = build3 (COND_EXPR, void_type_node,
			  EXIT_STMT_COND (stmt), *stmt_p, alloc_stmt_list ());
      return GS_OK;

    default:
      gcc_unreachable ();
    }
}

/* Force a reference to each of the entities in GNAT_PACKAGE recursively.

   This routine is exclusively called in type_annotate mode, to compute DDA
   information for types in withed units, for ASIS use.  */

static void
elaborate_all_entities_for_package (Entity_Id gnat_package)
{
  Entity_Id gnat_entity;

  for (gnat_entity = First_Entity (gnat_package);
       Present (gnat_entity);
       gnat_entity = Next_Entity (gnat_entity))
    {
      const Entity_Kind kind = Ekind (gnat_entity);

      /* We are interested only in entities visible from the main unit.  */
      if (!Is_Public (gnat_entity))
	continue;

      /* Skip stuff internal to the compiler.  */
      if (Convention (gnat_entity) == Convention_Intrinsic)
	continue;
      if (kind == E_Operator)
	continue;
      if (IN (kind, Subprogram_Kind) && Is_Intrinsic_Subprogram (gnat_entity))
	continue;
      if (Is_Itype (gnat_entity))
	continue;

      /* Skip named numbers.  */
      if (IN (kind, Named_Kind))
	continue;

      /* Skip generic declarations.  */
      if (IN (kind, Generic_Unit_Kind))
	continue;

      /* Skip formal objects.  */
      if (IN (kind, Formal_Object_Kind))
	continue;

      /* Skip package bodies.  */
      if (kind == E_Package_Body)
	continue;

      /* Skip limited views that point back to the main unit.  */
      if (IN (kind, Incomplete_Kind)
	  && From_Limited_With (gnat_entity)
	  && In_Extended_Main_Code_Unit (Non_Limited_View (gnat_entity)))
	continue;

      /* Skip types that aren't frozen.  */
      if (IN (kind, Type_Kind) && !Is_Frozen (gnat_entity))
	continue;

      /* Recurse on real packages that aren't in the main unit.  */
      if (kind == E_Package)
	{
	  if (No (Renamed_Entity (gnat_entity))
	      && !In_Extended_Main_Code_Unit (gnat_entity))
	    elaborate_all_entities_for_package (gnat_entity);
	}
      else
	gnat_to_gnu_entity (gnat_entity, NULL_TREE, false);
    }
}

/* Force a reference to each of the entities in packages withed by GNAT_NODE.
   Operate recursively but check that we aren't elaborating something more
   than once.

   This routine is exclusively called in type_annotate mode, to compute DDA
   information for types in withed units, for ASIS use.  */

static void
elaborate_all_entities (Node_Id gnat_node)
{
  Entity_Id gnat_with_clause;

  /* Process each unit only once.  As we trace the context of all relevant
     units transitively, including generic bodies, we may encounter the
     same generic unit repeatedly.  */
  if (!present_gnu_tree (gnat_node))
     save_gnu_tree (gnat_node, integer_zero_node, true);

  /* Save entities in all context units.  A body may have an implicit_with
     on its own spec, if the context includes a child unit, so don't save
     the spec twice.  */
  for (gnat_with_clause = First (Context_Items (gnat_node));
       Present (gnat_with_clause);
       gnat_with_clause = Next (gnat_with_clause))
    if (Nkind (gnat_with_clause) == N_With_Clause
	&& !present_gnu_tree (Library_Unit (gnat_with_clause))
	&& Library_Unit (gnat_with_clause) != Library_Unit (Cunit (Main_Unit)))
      {
	Node_Id gnat_unit = Library_Unit (gnat_with_clause);
	Entity_Id gnat_entity = Entity (Name (gnat_with_clause));

	elaborate_all_entities (gnat_unit);

	if (Ekind (gnat_entity) == E_Package)
	  elaborate_all_entities_for_package (gnat_entity);

	else if (Ekind (gnat_entity) == E_Generic_Package)
	  {
	    Node_Id gnat_body = Corresponding_Body (Unit (gnat_unit));

	    /* Retrieve compilation unit node of generic body.  */
	    while (Present (gnat_body)
		   && Nkind (gnat_body) != N_Compilation_Unit)
	      gnat_body = Parent (gnat_body);

	    /* If body is available, elaborate its context.  */
	    if (Present (gnat_body))
	      elaborate_all_entities (gnat_body);
	  }
      }

  if (Nkind (Unit (gnat_node)) == N_Package_Body)
    elaborate_all_entities (Library_Unit (gnat_node));
}

/* Do the processing of GNAT_NODE, an N_Freeze_Entity.  */

static void
process_freeze_entity (Node_Id gnat_node)
{
  const Entity_Id gnat_entity = Entity (gnat_node);
  const Entity_Kind kind = Ekind (gnat_entity);
  tree gnu_old, gnu_new;

  /* If this is a package, we need to generate code for the package.  */
  if (kind == E_Package)
    {
      insert_code_for
	(Parent (Corresponding_Body
		 (Parent (Declaration_Node (gnat_entity)))));
      return;
    }

  /* Don't do anything for class-wide types as they are always transformed
     into their root type.  */
  if (kind == E_Class_Wide_Type)
    return;

  /* Check for an old definition if this isn't an object with address clause,
     since the saved GCC tree is the address expression in that case.  */
  gnu_old
    = present_gnu_tree (gnat_entity) && No (Address_Clause (gnat_entity))
      ? get_gnu_tree (gnat_entity) : NULL_TREE;

  /* Don't do anything for subprograms that may have been elaborated before
     their freeze nodes.  This can happen, for example, because of an inner
     call in an instance body or because of previous compilation of a spec
     for inlining purposes.  */
  if (gnu_old
      && ((TREE_CODE (gnu_old) == FUNCTION_DECL
	   && (kind == E_Function || kind == E_Procedure))
	  || (TREE_CODE (TREE_TYPE (gnu_old)) == FUNCTION_TYPE
	      && kind == E_Subprogram_Type)))
    return;

  /* If we have a non-dummy type old tree, we have nothing to do, except for
     aborting, since this node was never delayed as it should have been.  We
     let this happen for concurrent types and their Corresponding_Record_Type,
     however, because each might legitimately be elaborated before its own
     freeze node, e.g. while processing the other.  */
  if (gnu_old
      && !(TREE_CODE (gnu_old) == TYPE_DECL
	   && TYPE_IS_DUMMY_P (TREE_TYPE (gnu_old))))
    {
      gcc_assert (Is_Concurrent_Type (gnat_entity)
		  || (IN (kind, Record_Kind)
		      && Is_Concurrent_Record_Type (gnat_entity)));
      return;
    }

  /* Reset the saved tree, if any, and elaborate the object or type for real.
     If there is a full view, elaborate it and use the result.  And, if this
     is the root type of a class-wide type, reuse it for the latter.  */
  if (gnu_old)
    {
      save_gnu_tree (gnat_entity, NULL_TREE, false);

      if (IN (kind, Incomplete_Or_Private_Kind)
	  && Present (Full_View (gnat_entity)))
	{
	  Entity_Id full_view = Full_View (gnat_entity);

	  save_gnu_tree (full_view, NULL_TREE, false);

          if (IN (Ekind (full_view), Private_Kind)
	      && Present (Underlying_Full_View (full_view)))
	    {
	      full_view = Underlying_Full_View (full_view);
	      save_gnu_tree (full_view, NULL_TREE, false);
	    }
	}

      if (IN (kind, Type_Kind)
	  && Present (Class_Wide_Type (gnat_entity))
	  && Root_Type (Class_Wide_Type (gnat_entity)) == gnat_entity)
	save_gnu_tree (Class_Wide_Type (gnat_entity), NULL_TREE, false);
    }

  if (IN (kind, Incomplete_Or_Private_Kind)
      && Present (Full_View (gnat_entity)))
    {
      Entity_Id full_view = Full_View (gnat_entity);

      if (IN (Ekind (full_view), Private_Kind)
	  && Present (Underlying_Full_View (full_view)))
	full_view = Underlying_Full_View (full_view);

      gnu_new = gnat_to_gnu_entity (full_view, NULL_TREE, true);

      /* Propagate back-annotations from full view to partial view.  */
      if (Unknown_Alignment (gnat_entity))
	Set_Alignment (gnat_entity, Alignment (full_view));

      if (Unknown_Esize (gnat_entity))
	Set_Esize (gnat_entity, Esize (full_view));

      if (Unknown_RM_Size (gnat_entity))
	Set_RM_Size (gnat_entity, RM_Size (full_view));

      /* The above call may have defined this entity (the simplest example
	 of this is when we have a private enumeral type since the bounds
	 will have the public view).  */
      if (!present_gnu_tree (gnat_entity))
	save_gnu_tree (gnat_entity, gnu_new, false);
    }
  else
    {
      tree gnu_init
	= (Nkind (Declaration_Node (gnat_entity)) == N_Object_Declaration
	   && present_gnu_tree (Declaration_Node (gnat_entity)))
	  ? get_gnu_tree (Declaration_Node (gnat_entity)) : NULL_TREE;

      gnu_new = gnat_to_gnu_entity (gnat_entity, gnu_init, true);
    }

  if (IN (kind, Type_Kind)
      && Present (Class_Wide_Type (gnat_entity))
      && Root_Type (Class_Wide_Type (gnat_entity)) == gnat_entity)
    save_gnu_tree (Class_Wide_Type (gnat_entity), gnu_new, false);

  /* If we have an old type and we've made pointers to this type, update those
     pointers.  If this is a Taft amendment type in the main unit, we need to
     mark the type as used since other units referencing it don't see the full
     declaration and, therefore, cannot mark it as used themselves.  */
  if (gnu_old)
    {
      update_pointer_to (TYPE_MAIN_VARIANT (TREE_TYPE (gnu_old)),
			 TREE_TYPE (gnu_new));
      if (TYPE_DUMMY_IN_PROFILE_P (TREE_TYPE (gnu_old)))
	update_profiles_with (TREE_TYPE (gnu_old));
      if (DECL_TAFT_TYPE_P (gnu_old))
	used_types_insert (TREE_TYPE (gnu_new));
    }
}

/* Elaborate decls in the lists GNAT_DECLS and GNAT_DECLS2, if present.
   We make two passes, one to elaborate anything other than bodies (but
   we declare a function if there was no spec).  The second pass
   elaborates the bodies.

   GNAT_END_LIST gives the element in the list past the end.  Normally,
   this is Empty, but can be First_Real_Statement for a
   Handled_Sequence_Of_Statements.

   We make a complete pass through both lists if PASS1P is true, then make
   the second pass over both lists if PASS2P is true.  The lists usually
   correspond to the public and private parts of a package.  */

static void
process_decls (List_Id gnat_decls, List_Id gnat_decls2,
	       Node_Id gnat_end_list, bool pass1p, bool pass2p)
{
  List_Id gnat_decl_array[2];
  Node_Id gnat_decl;
  int i;

  gnat_decl_array[0] = gnat_decls, gnat_decl_array[1] = gnat_decls2;

  if (pass1p)
    for (i = 0; i <= 1; i++)
      if (Present (gnat_decl_array[i]))
	for (gnat_decl = First (gnat_decl_array[i]);
	     gnat_decl != gnat_end_list; gnat_decl = Next (gnat_decl))
	  {
	    /* For package specs, we recurse inside the declarations,
	       thus taking the two pass approach inside the boundary.  */
	    if (Nkind (gnat_decl) == N_Package_Declaration
		&& (Nkind (Specification (gnat_decl)
			   == N_Package_Specification)))
	      process_decls (Visible_Declarations (Specification (gnat_decl)),
			     Private_Declarations (Specification (gnat_decl)),
			     Empty, true, false);

	    /* Similarly for any declarations in the actions of a
	       freeze node.  */
	    else if (Nkind (gnat_decl) == N_Freeze_Entity)
	      {
		process_freeze_entity (gnat_decl);
		process_decls (Actions (gnat_decl), Empty, Empty, true, false);
	      }

	    /* Package bodies with freeze nodes get their elaboration deferred
	       until the freeze node, but the code must be placed in the right
	       place, so record the code position now.  */
	    else if (Nkind (gnat_decl) == N_Package_Body
		     && Present (Freeze_Node (Corresponding_Spec (gnat_decl))))
	      record_code_position (gnat_decl);

	    else if (Nkind (gnat_decl) == N_Package_Body_Stub
		     && Present (Library_Unit (gnat_decl))
		     && Present (Freeze_Node
				 (Corresponding_Spec
				  (Proper_Body (Unit
						(Library_Unit (gnat_decl)))))))
	      record_code_position
		(Proper_Body (Unit (Library_Unit (gnat_decl))));

	    /* We defer most subprogram bodies to the second pass.  */
	    else if (Nkind (gnat_decl) == N_Subprogram_Body)
	      {
		if (Acts_As_Spec (gnat_decl))
		  {
		    Node_Id gnat_subprog_id = Defining_Entity (gnat_decl);

		    if (Ekind (gnat_subprog_id) != E_Generic_Procedure
			&& Ekind (gnat_subprog_id) != E_Generic_Function)
		      gnat_to_gnu_entity (gnat_subprog_id, NULL_TREE, true);
		  }
	      }

	    /* For bodies and stubs that act as their own specs, the entity
	       itself must be elaborated in the first pass, because it may
	       be used in other declarations.  */
	    else if (Nkind (gnat_decl) == N_Subprogram_Body_Stub)
	      {
		Node_Id gnat_subprog_id
		  = Defining_Entity (Specification (gnat_decl));

		    if (Ekind (gnat_subprog_id) != E_Subprogram_Body
			&& Ekind (gnat_subprog_id) != E_Generic_Procedure
			&& Ekind (gnat_subprog_id) != E_Generic_Function)
		      gnat_to_gnu_entity (gnat_subprog_id, NULL_TREE, true);
	      }

	    /* Concurrent stubs stand for the corresponding subprogram bodies,
	       which are deferred like other bodies.  */
	    else if (Nkind (gnat_decl) == N_Task_Body_Stub
		     || Nkind (gnat_decl) == N_Protected_Body_Stub)
	      ;

	    /* Renamed subprograms may not be elaborated yet at this point
	       since renamings do not trigger freezing.  Wait for the second
	       pass to take care of them.  */
	    else if (Nkind (gnat_decl) == N_Subprogram_Renaming_Declaration)
	      ;

	    else
	      add_stmt (gnat_to_gnu (gnat_decl));
	  }

  /* Here we elaborate everything we deferred above except for package bodies,
     which are elaborated at their freeze nodes.  Note that we must also
     go inside things (package specs and freeze nodes) the first pass did.  */
  if (pass2p)
    for (i = 0; i <= 1; i++)
      if (Present (gnat_decl_array[i]))
	for (gnat_decl = First (gnat_decl_array[i]);
	     gnat_decl != gnat_end_list; gnat_decl = Next (gnat_decl))
	  {
	    if (Nkind (gnat_decl) == N_Subprogram_Body
		|| Nkind (gnat_decl) == N_Subprogram_Body_Stub
		|| Nkind (gnat_decl) == N_Task_Body_Stub
		|| Nkind (gnat_decl) == N_Protected_Body_Stub)
	      add_stmt (gnat_to_gnu (gnat_decl));

	    else if (Nkind (gnat_decl) == N_Package_Declaration
		     && (Nkind (Specification (gnat_decl)
				== N_Package_Specification)))
	      process_decls (Visible_Declarations (Specification (gnat_decl)),
			     Private_Declarations (Specification (gnat_decl)),
			     Empty, false, true);

	    else if (Nkind (gnat_decl) == N_Freeze_Entity)
	      process_decls (Actions (gnat_decl), Empty, Empty, false, true);

	    else if (Nkind (gnat_decl) == N_Subprogram_Renaming_Declaration)
	      add_stmt (gnat_to_gnu (gnat_decl));
	  }
}

/* Make a unary operation of kind CODE using build_unary_op, but guard
   the operation by an overflow check.  CODE can be one of NEGATE_EXPR
   or ABS_EXPR.  GNU_TYPE is the type desired for the result.  Usually
   the operation is to be performed in that type.  GNAT_NODE is the gnat
   node conveying the source location for which the error should be
   signaled.  */

static tree
build_unary_op_trapv (enum tree_code code, tree gnu_type, tree operand,
		      Node_Id gnat_node)
{
  gcc_assert (code == NEGATE_EXPR || code == ABS_EXPR);

  operand = gnat_protect_expr (operand);

  return emit_check (build_binary_op (EQ_EXPR, boolean_type_node,
				      operand, TYPE_MIN_VALUE (gnu_type)),
		     build_unary_op (code, gnu_type, operand),
		     CE_Overflow_Check_Failed, gnat_node);
}

/* Make a binary operation of kind CODE using build_binary_op, but guard
   the operation by an overflow check.  CODE can be one of PLUS_EXPR,
   MINUS_EXPR or MULT_EXPR.  GNU_TYPE is the type desired for the result.
   Usually the operation is to be performed in that type.  GNAT_NODE is
   the GNAT node conveying the source location for which the error should
   be signaled.  */

static tree
build_binary_op_trapv (enum tree_code code, tree gnu_type, tree left,
		       tree right, Node_Id gnat_node)
{
  const unsigned int precision = TYPE_PRECISION (gnu_type);
  tree lhs = gnat_protect_expr (left);
  tree rhs = gnat_protect_expr (right);
  tree type_max = TYPE_MAX_VALUE (gnu_type);
  tree type_min = TYPE_MIN_VALUE (gnu_type);
  tree gnu_expr, check;
  int sgn;

  /* Assert that the precision is a power of 2.  */
  gcc_assert ((precision & (precision - 1)) == 0);

  /* Prefer a constant on the RHS to simplify checks.  */
  if (TREE_CODE (rhs) != INTEGER_CST
      && TREE_CODE (lhs) == INTEGER_CST
      && (code == PLUS_EXPR || code == MULT_EXPR))
    {
      tree tmp = lhs;
      lhs = rhs;
      rhs = tmp;
    }

  gnu_expr = build_binary_op (code, gnu_type, lhs, rhs);

  /* If we can fold the expression to a constant, just return it.
     The caller will deal with overflow, no need to generate a check.  */
  if (TREE_CODE (gnu_expr) == INTEGER_CST)
    return gnu_expr;

  /* If no operand is a constant, we use the generic implementation.  */
  if (TREE_CODE (lhs) != INTEGER_CST && TREE_CODE (rhs) != INTEGER_CST)
    {
      /* Never inline a 64-bit mult for a 32-bit target, it's way too long.  */
      if (code == MULT_EXPR && precision == 64 && BITS_PER_WORD < 64)
	{
	  tree int64 = gnat_type_for_size (64, 0);
	  return convert (gnu_type, build_call_n_expr (mulv64_decl, 2,
						       convert (int64, lhs),
						       convert (int64, rhs)));
	}

      enum internal_fn icode;

      switch (code)
	{
	case PLUS_EXPR:
	  icode = IFN_ADD_OVERFLOW;
	  break;
	case MINUS_EXPR:
	  icode = IFN_SUB_OVERFLOW;
	  break;
	case MULT_EXPR:
	  icode = IFN_MUL_OVERFLOW;
	  break;
	default:
	  gcc_unreachable ();
	}

      tree gnu_ctype = build_complex_type (gnu_type);
      tree call
	= build_call_expr_internal_loc (UNKNOWN_LOCATION, icode, gnu_ctype, 2,
					lhs, rhs);
      tree tgt = save_expr (call);
      gnu_expr = build1 (REALPART_EXPR, gnu_type, tgt);
      check
	= convert (boolean_type_node, build1 (IMAGPART_EXPR, gnu_type, tgt));
      return
	emit_check (check, gnu_expr, CE_Overflow_Check_Failed, gnat_node);
   }

  /* If one operand is a constant, we expose the overflow condition to enable
     a subsequent simplication or even elimination.  */
  switch (code)
    {
    case PLUS_EXPR:
      sgn = tree_int_cst_sgn (rhs);
      if (sgn > 0)
	/* When rhs > 0, overflow when lhs > type_max - rhs.  */
	check = build_binary_op (GT_EXPR, boolean_type_node, lhs,
				 build_binary_op (MINUS_EXPR, gnu_type,
						  type_max, rhs));
      else if (sgn < 0)
	/* When rhs < 0, overflow when lhs < type_min - rhs.  */
	check = build_binary_op (LT_EXPR, boolean_type_node, lhs,
				 build_binary_op (MINUS_EXPR, gnu_type,
						  type_min, rhs));
      else
	return gnu_expr;
      break;

    case MINUS_EXPR:
      if (TREE_CODE (lhs) == INTEGER_CST)
	{
	  sgn = tree_int_cst_sgn (lhs);
	  if (sgn > 0)
	    /* When lhs > 0, overflow when rhs < lhs - type_max.  */
	    check = build_binary_op (LT_EXPR, boolean_type_node, rhs,
				     build_binary_op (MINUS_EXPR, gnu_type,
						      lhs, type_max));
	  else if (sgn < 0)
	    /* When lhs < 0, overflow when rhs > lhs - type_min.  */
	    check = build_binary_op (GT_EXPR, boolean_type_node, rhs,
				     build_binary_op (MINUS_EXPR, gnu_type,
						      lhs, type_min));
	  else
	    return gnu_expr;
	}
      else
	{
	  sgn = tree_int_cst_sgn (rhs);
	  if (sgn > 0)
	    /* When rhs > 0, overflow when lhs < type_min + rhs.  */
	    check = build_binary_op (LT_EXPR, boolean_type_node, lhs,
				     build_binary_op (PLUS_EXPR, gnu_type,
						      type_min, rhs));
	  else if (sgn < 0)
	    /* When rhs < 0, overflow when lhs > type_max + rhs.  */
	    check = build_binary_op (GT_EXPR, boolean_type_node, lhs,
				     build_binary_op (PLUS_EXPR, gnu_type,
						      type_max, rhs));
	  else
	    return gnu_expr;
	}
      break;

    case MULT_EXPR:
      sgn = tree_int_cst_sgn (rhs);
      if (sgn > 0)
	{
	  if (integer_onep (rhs))
	    return gnu_expr;

	  tree lb = build_binary_op (TRUNC_DIV_EXPR, gnu_type, type_min, rhs);
	  tree ub = build_binary_op (TRUNC_DIV_EXPR, gnu_type, type_max, rhs);

	  /* When rhs > 1, overflow outside [type_min/rhs; type_max/rhs].  */
	  check
	    = build_binary_op (TRUTH_ORIF_EXPR, boolean_type_node,
			       build_binary_op (LT_EXPR, boolean_type_node,
						lhs, lb),
			       build_binary_op (GT_EXPR, boolean_type_node,
						lhs, ub));
	}
      else if (sgn < 0)
	{
	  tree lb = build_binary_op (TRUNC_DIV_EXPR, gnu_type, type_max, rhs);
	  tree ub = build_binary_op (TRUNC_DIV_EXPR, gnu_type, type_min, rhs);

	  if (integer_minus_onep (rhs))
	    /* When rhs == -1, overflow if lhs == type_min.  */
	    check
	      = build_binary_op (EQ_EXPR, boolean_type_node, lhs, type_min);
	  else
	    /* When rhs < -1, overflow outside [type_max/rhs; type_min/rhs].  */
	    check
	      = build_binary_op (TRUTH_ORIF_EXPR, boolean_type_node,
				 build_binary_op (LT_EXPR, boolean_type_node,
						  lhs, lb),
				 build_binary_op (GT_EXPR, boolean_type_node,
						  lhs, ub));
	}
      else
	return gnu_expr;
      break;

    default:
      gcc_unreachable ();
    }

  return emit_check (check, gnu_expr, CE_Overflow_Check_Failed, gnat_node);
}

/* Emit code for a range check.  GNU_EXPR is the expression to be checked,
   GNAT_RANGE_TYPE the gnat type or subtype containing the bounds against
   which we have to check.  GNAT_NODE is the GNAT node conveying the source
   location for which the error should be signaled.  */

static tree
emit_range_check (tree gnu_expr, Entity_Id gnat_range_type, Node_Id gnat_node)
{
  tree gnu_range_type = get_unpadded_type (gnat_range_type);
  tree gnu_compare_type = get_base_type (TREE_TYPE (gnu_expr));

  /* If GNU_EXPR has GNAT_RANGE_TYPE as its base type, no check is needed.
     This can for example happen when translating 'Val or 'Value.  */
  if (gnu_compare_type == gnu_range_type)
    return gnu_expr;

  /* Range checks can only be applied to types with ranges.  */
  gcc_assert (INTEGRAL_TYPE_P (gnu_range_type)
              || SCALAR_FLOAT_TYPE_P (gnu_range_type));

  /* If GNU_EXPR has an integral type that is narrower than GNU_RANGE_TYPE,
     we can't do anything since we might be truncating the bounds.  No
     check is needed in this case.  */
  if (INTEGRAL_TYPE_P (TREE_TYPE (gnu_expr))
      && (TYPE_PRECISION (gnu_compare_type)
	  < TYPE_PRECISION (get_base_type (gnu_range_type))))
    return gnu_expr;

  /* Checked expressions must be evaluated only once.  */
  gnu_expr = gnat_protect_expr (gnu_expr);

  /* Note that the form of the check is
	(not (expr >= lo)) or (not (expr <= hi))
     the reason for this slightly convoluted form is that NaNs
     are not considered to be in range in the float case.  */
  return emit_check
    (build_binary_op (TRUTH_ORIF_EXPR, boolean_type_node,
		      invert_truthvalue
		      (build_binary_op (GE_EXPR, boolean_type_node,
				        convert (gnu_compare_type, gnu_expr),
				        convert (gnu_compare_type,
						 TYPE_MIN_VALUE
						 (gnu_range_type)))),
		      invert_truthvalue
		      (build_binary_op (LE_EXPR, boolean_type_node,
					convert (gnu_compare_type, gnu_expr),
					convert (gnu_compare_type,
						 TYPE_MAX_VALUE
						 (gnu_range_type))))),
     gnu_expr, CE_Range_Check_Failed, gnat_node);
}

/* GNU_COND contains the condition corresponding to an index, overflow or
   range check of value GNU_EXPR.  Build a COND_EXPR that returns GNU_EXPR
   if GNU_COND is false and raises a CONSTRAINT_ERROR if GNU_COND is true.
   REASON is the code that says why the exception is raised.  GNAT_NODE is
   the node conveying the source location for which the error should be
   signaled.

   We used to propagate TREE_SIDE_EFFECTS from GNU_EXPR to the COND_EXPR,
   overwriting the setting inherited from the call statement, on the ground
   that the expression need not be evaluated just for the check.  However
   that's incorrect because, in the GCC type system, its value is presumed
   to be valid so its comparison against the type bounds always yields true
   and, therefore, could be done without evaluating it; given that it can
   be a computation that overflows the bounds, the language may require the
   check to fail and thus the expression to be evaluated in this case.  */

static tree
emit_check (tree gnu_cond, tree gnu_expr, int reason, Node_Id gnat_node)
{
  tree gnu_call
    = build_call_raise (reason, gnat_node, N_Raise_Constraint_Error);
  return
    fold_build3 (COND_EXPR, TREE_TYPE (gnu_expr), gnu_cond,
		 build2 (COMPOUND_EXPR, TREE_TYPE (gnu_expr), gnu_call,
			 SCALAR_FLOAT_TYPE_P (TREE_TYPE (gnu_expr))
			 ? build_real (TREE_TYPE (gnu_expr), dconst0)
			 : build_int_cst (TREE_TYPE (gnu_expr), 0)),
		 gnu_expr);
}

/* Return an expression that converts GNU_EXPR to GNAT_TYPE, doing overflow
   checks if OVERFLOW_P is true and range checks if RANGE_P is true.
   GNAT_TYPE is known to be an integral type.  If TRUNCATE_P true, do a
   float to integer conversion with truncation; otherwise round.
   GNAT_NODE is the GNAT node conveying the source location for which the
   error should be signaled.  */

static tree
convert_with_check (Entity_Id gnat_type, tree gnu_expr, bool overflowp,
		    bool rangep, bool truncatep, Node_Id gnat_node)
{
  tree gnu_type = get_unpadded_type (gnat_type);
  tree gnu_in_type = TREE_TYPE (gnu_expr);
  tree gnu_in_basetype = get_base_type (gnu_in_type);
  tree gnu_base_type = get_base_type (gnu_type);
  tree gnu_result = gnu_expr;

  /* If we are not doing any checks, the output is an integral type and the
     input is not a floating-point type, just do the conversion.  This is
     required for packed array types and is simpler in all cases anyway.   */
  if (!rangep
      && !overflowp
      && INTEGRAL_TYPE_P (gnu_base_type)
      && !FLOAT_TYPE_P (gnu_in_type))
    return convert (gnu_type, gnu_expr);

  /* First convert the expression to its base type.  This
     will never generate code, but makes the tests below much simpler.
     But don't do this if converting from an integer type to an unconstrained
     array type since then we need to get the bounds from the original
     (unpacked) type.  */
  if (TREE_CODE (gnu_type) != UNCONSTRAINED_ARRAY_TYPE)
    gnu_result = convert (gnu_in_basetype, gnu_result);

  /* If overflow checks are requested,  we need to be sure the result will
     fit in the output base type.  But don't do this if the input
     is integer and the output floating-point.  */
  if (overflowp
      && !(FLOAT_TYPE_P (gnu_base_type) && INTEGRAL_TYPE_P (gnu_in_basetype)))
    {
      /* Ensure GNU_EXPR only gets evaluated once.  */
      tree gnu_input = gnat_protect_expr (gnu_result);
      tree gnu_cond = boolean_false_node;
      tree gnu_in_lb = TYPE_MIN_VALUE (gnu_in_basetype);
      tree gnu_in_ub = TYPE_MAX_VALUE (gnu_in_basetype);
      tree gnu_out_lb = TYPE_MIN_VALUE (gnu_base_type);
      tree gnu_out_ub = TYPE_MAX_VALUE (gnu_base_type);

      /* Convert the lower bounds to signed types, so we're sure we're
	 comparing them properly.  Likewise, convert the upper bounds
	 to unsigned types.  */
      if (INTEGRAL_TYPE_P (gnu_in_basetype) && TYPE_UNSIGNED (gnu_in_basetype))
	gnu_in_lb
	  = convert (gnat_signed_type_for (gnu_in_basetype), gnu_in_lb);

      if (INTEGRAL_TYPE_P (gnu_in_basetype)
	  && !TYPE_UNSIGNED (gnu_in_basetype))
	gnu_in_ub
	  = convert (gnat_unsigned_type_for (gnu_in_basetype), gnu_in_ub);

      if (INTEGRAL_TYPE_P (gnu_base_type) && TYPE_UNSIGNED (gnu_base_type))
	gnu_out_lb
	  = convert (gnat_signed_type_for (gnu_base_type), gnu_out_lb);

      if (INTEGRAL_TYPE_P (gnu_base_type) && !TYPE_UNSIGNED (gnu_base_type))
	gnu_out_ub
	  = convert (gnat_unsigned_type_for (gnu_base_type), gnu_out_ub);

      /* Check each bound separately and only if the result bound
	 is tighter than the bound on the input type.  Note that all the
	 types are base types, so the bounds must be constant. Also,
	 the comparison is done in the base type of the input, which
	 always has the proper signedness.  First check for input
	 integer (which means output integer), output float (which means
	 both float), or mixed, in which case we always compare.
	 Note that we have to do the comparison which would *fail* in the
	 case of an error since if it's an FP comparison and one of the
	 values is a NaN or Inf, the comparison will fail.  */
      if (INTEGRAL_TYPE_P (gnu_in_basetype)
	  ? tree_int_cst_lt (gnu_in_lb, gnu_out_lb)
	  : (FLOAT_TYPE_P (gnu_base_type)
	     ? real_less (&TREE_REAL_CST (gnu_in_lb),
			  &TREE_REAL_CST (gnu_out_lb))
	     : 1))
	gnu_cond
	  = invert_truthvalue
	    (build_binary_op (GE_EXPR, boolean_type_node,
			      gnu_input, convert (gnu_in_basetype,
						  gnu_out_lb)));

      if (INTEGRAL_TYPE_P (gnu_in_basetype)
	  ? tree_int_cst_lt (gnu_out_ub, gnu_in_ub)
	  : (FLOAT_TYPE_P (gnu_base_type)
	     ? real_less (&TREE_REAL_CST (gnu_out_ub),
			  &TREE_REAL_CST (gnu_in_lb))
	     : 1))
	gnu_cond
	  = build_binary_op (TRUTH_ORIF_EXPR, boolean_type_node, gnu_cond,
			     invert_truthvalue
			     (build_binary_op (LE_EXPR, boolean_type_node,
					       gnu_input,
					       convert (gnu_in_basetype,
							gnu_out_ub))));

      if (!integer_zerop (gnu_cond))
	gnu_result = emit_check (gnu_cond, gnu_input,
				 CE_Overflow_Check_Failed, gnat_node);
    }

  /* Now convert to the result base type.  If this is a non-truncating
     float-to-integer conversion, round.  */
  if (INTEGRAL_TYPE_P (gnu_base_type)
      && FLOAT_TYPE_P (gnu_in_basetype)
      && !truncatep)
    {
      REAL_VALUE_TYPE half_minus_pred_half, pred_half;
      tree gnu_conv, gnu_zero, gnu_comp, calc_type;
      tree gnu_pred_half, gnu_add_pred_half, gnu_subtract_pred_half;
      const struct real_format *fmt;

      /* The following calculations depend on proper rounding to even
	 of each arithmetic operation.  In order to prevent excess
	 precision from spoiling this property, use the widest hardware
	 floating-point type if FP_ARITH_MAY_WIDEN is true.  */
      calc_type
	= fp_arith_may_widen ? longest_float_type_node : gnu_in_basetype;

      /* Compute the exact value calc_type'Pred (0.5) at compile time.  */
      fmt = REAL_MODE_FORMAT (TYPE_MODE (calc_type));
      real_2expN (&half_minus_pred_half, -(fmt->p) - 1, TYPE_MODE (calc_type));
      real_arithmetic (&pred_half, MINUS_EXPR, &dconsthalf,
		       &half_minus_pred_half);
      gnu_pred_half = build_real (calc_type, pred_half);

      /* If the input is strictly negative, subtract this value
	 and otherwise add it from the input.  For 0.5, the result
	 is exactly between 1.0 and the machine number preceding 1.0
	 (for calc_type).  Since the last bit of 1.0 is even, this 0.5
	 will round to 1.0, while all other number with an absolute
	 value less than 0.5 round to 0.0.  For larger numbers exactly
	 halfway between integers, rounding will always be correct as
	 the true mathematical result will be closer to the higher
	 integer compared to the lower one.  So, this constant works
	 for all floating-point numbers.

	 The reason to use the same constant with subtract/add instead
	 of a positive and negative constant is to allow the comparison
	 to be scheduled in parallel with retrieval of the constant and
	 conversion of the input to the calc_type (if necessary).  */

      gnu_zero = build_real (gnu_in_basetype, dconst0);
      gnu_result = gnat_protect_expr (gnu_result);
      gnu_conv = convert (calc_type, gnu_result);
      gnu_comp
	= fold_build2 (GE_EXPR, boolean_type_node, gnu_result, gnu_zero);
      gnu_add_pred_half
	= fold_build2 (PLUS_EXPR, calc_type, gnu_conv, gnu_pred_half);
      gnu_subtract_pred_half
	= fold_build2 (MINUS_EXPR, calc_type, gnu_conv, gnu_pred_half);
      gnu_result = fold_build3 (COND_EXPR, calc_type, gnu_comp,
				gnu_add_pred_half, gnu_subtract_pred_half);
    }

  if (TREE_CODE (gnu_base_type) == INTEGER_TYPE
      && TYPE_HAS_ACTUAL_BOUNDS_P (gnu_base_type)
      && TREE_CODE (gnu_result) == UNCONSTRAINED_ARRAY_REF)
    gnu_result = unchecked_convert (gnu_base_type, gnu_result, false);
  else
    gnu_result = convert (gnu_base_type, gnu_result);

  /* Finally, do the range check if requested.  Note that if the result type
     is a modular type, the range check is actually an overflow check.  */
  if (rangep
      || (TREE_CODE (gnu_base_type) == INTEGER_TYPE
	  && TYPE_MODULAR_P (gnu_base_type) && overflowp))
    gnu_result = emit_range_check (gnu_result, gnat_type, gnat_node);

  return convert (gnu_type, gnu_result);
}

/* Return true if GNU_EXPR can be directly addressed.  This is the case
   unless it is an expression involving computation or if it involves a
   reference to a bitfield or to an object not sufficiently aligned for
   its type.  If GNU_TYPE is non-null, return true only if GNU_EXPR can
   be directly addressed as an object of this type.

   *** Notes on addressability issues in the Ada compiler ***

   This predicate is necessary in order to bridge the gap between Gigi
   and the middle-end about addressability of GENERIC trees.  A tree
   is said to be addressable if it can be directly addressed, i.e. if
   its address can be taken, is a multiple of the type's alignment on
   strict-alignment architectures and returns the first storage unit
   assigned to the object represented by the tree.

   In the C family of languages, everything is in practice addressable
   at the language level, except for bit-fields.  This means that these
   compilers will take the address of any tree that doesn't represent
   a bit-field reference and expect the result to be the first storage
   unit assigned to the object.  Even in cases where this will result
   in unaligned accesses at run time, nothing is supposed to be done
   and the program is considered as erroneous instead (see PR c/18287).

   The implicit assumptions made in the middle-end are in keeping with
   the C viewpoint described above:
     - the address of a bit-field reference is supposed to be never
       taken; the compiler (generally) will stop on such a construct,
     - any other tree is addressable if it is formally addressable,
       i.e. if it is formally allowed to be the operand of ADDR_EXPR.

   In Ada, the viewpoint is the opposite one: nothing is addressable
   at the language level unless explicitly declared so.  This means
   that the compiler will both make sure that the trees representing
   references to addressable ("aliased" in Ada parlance) objects are
   addressable and make no real attempts at ensuring that the trees
   representing references to non-addressable objects are addressable.

   In the first case, Ada is effectively equivalent to C and handing
   down the direct result of applying ADDR_EXPR to these trees to the
   middle-end works flawlessly.  In the second case, Ada cannot afford
   to consider the program as erroneous if the address of trees that
   are not addressable is requested for technical reasons, unlike C;
   as a consequence, the Ada compiler must arrange for either making
   sure that this address is not requested in the middle-end or for
   compensating by inserting temporaries if it is requested in Gigi.

   The first goal can be achieved because the middle-end should not
   request the address of non-addressable trees on its own; the only
   exception is for the invocation of low-level block operations like
   memcpy, for which the addressability requirements are lower since
   the type's alignment can be disregarded.  In practice, this means
   that Gigi must make sure that such operations cannot be applied to
   non-BLKmode bit-fields.

   The second goal is achieved by means of the addressable_p predicate,
   which computes whether a temporary must be inserted by Gigi when the
   address of a tree is requested; if so, the address of the temporary
   will be used in lieu of that of the original tree and some glue code
   generated to connect everything together.  */

static bool
addressable_p (tree gnu_expr, tree gnu_type)
{
  /* For an integral type, the size of the actual type of the object may not
     be greater than that of the expected type, otherwise an indirect access
     in the latter type wouldn't correctly set all the bits of the object.  */
  if (gnu_type
      && INTEGRAL_TYPE_P (gnu_type)
      && smaller_form_type_p (gnu_type, TREE_TYPE (gnu_expr)))
    return false;

  /* The size of the actual type of the object may not be smaller than that
     of the expected type, otherwise an indirect access in the latter type
     would be larger than the object.  But only record types need to be
     considered in practice for this case.  */
  if (gnu_type
      && TREE_CODE (gnu_type) == RECORD_TYPE
      && smaller_form_type_p (TREE_TYPE (gnu_expr), gnu_type))
    return false;

  switch (TREE_CODE (gnu_expr))
    {
    case VAR_DECL:
    case PARM_DECL:
    case FUNCTION_DECL:
    case RESULT_DECL:
      /* All DECLs are addressable: if they are in a register, we can force
	 them to memory.  */
      return true;

    case UNCONSTRAINED_ARRAY_REF:
    case INDIRECT_REF:
      /* Taking the address of a dereference yields the original pointer.  */
      return true;

    case STRING_CST:
    case INTEGER_CST:
      /* Taking the address yields a pointer to the constant pool.  */
      return true;

    case CONSTRUCTOR:
      /* Taking the address of a static constructor yields a pointer to the
	 tree constant pool.  */
      return TREE_STATIC (gnu_expr) ? true : false;

    case NULL_EXPR:
    case SAVE_EXPR:
    case CALL_EXPR:
    case PLUS_EXPR:
    case MINUS_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case BIT_AND_EXPR:
    case BIT_NOT_EXPR:
      /* All rvalues are deemed addressable since taking their address will
	 force a temporary to be created by the middle-end.  */
      return true;

    case COMPOUND_EXPR:
      /* The address of a compound expression is that of its 2nd operand.  */
      return addressable_p (TREE_OPERAND (gnu_expr, 1), gnu_type);

    case COND_EXPR:
      /* We accept &COND_EXPR as soon as both operands are addressable and
	 expect the outcome to be the address of the selected operand.  */
      return (addressable_p (TREE_OPERAND (gnu_expr, 1), NULL_TREE)
	      && addressable_p (TREE_OPERAND (gnu_expr, 2), NULL_TREE));

    case COMPONENT_REF:
      return (((!DECL_BIT_FIELD (TREE_OPERAND (gnu_expr, 1))
		/* Even with DECL_BIT_FIELD cleared, we have to ensure that
		   the field is sufficiently aligned, in case it is subject
		   to a pragma Component_Alignment.  But we don't need to
		   check the alignment of the containing record, as it is
		   guaranteed to be not smaller than that of its most
		   aligned field that is not a bit-field.  */
		&& (!STRICT_ALIGNMENT
		    || DECL_ALIGN (TREE_OPERAND (gnu_expr, 1))
		       >= TYPE_ALIGN (TREE_TYPE (gnu_expr))))
	       /* The field of a padding record is always addressable.  */
	       || TYPE_IS_PADDING_P (TREE_TYPE (TREE_OPERAND (gnu_expr, 0))))
	      && addressable_p (TREE_OPERAND (gnu_expr, 0), NULL_TREE));

    case ARRAY_REF:  case ARRAY_RANGE_REF:
    case REALPART_EXPR:  case IMAGPART_EXPR:
    case NOP_EXPR:
      return addressable_p (TREE_OPERAND (gnu_expr, 0), NULL_TREE);

    case CONVERT_EXPR:
      return (AGGREGATE_TYPE_P (TREE_TYPE (gnu_expr))
	      && addressable_p (TREE_OPERAND (gnu_expr, 0), NULL_TREE));

    case VIEW_CONVERT_EXPR:
      {
	/* This is addressable if we can avoid a copy.  */
	tree type = TREE_TYPE (gnu_expr);
	tree inner_type = TREE_TYPE (TREE_OPERAND (gnu_expr, 0));
	return (((TYPE_MODE (type) == TYPE_MODE (inner_type)
		  && (!STRICT_ALIGNMENT
		      || TYPE_ALIGN (type) <= TYPE_ALIGN (inner_type)
		      || TYPE_ALIGN (inner_type) >= BIGGEST_ALIGNMENT))
		 || ((TYPE_MODE (type) == BLKmode
		      || TYPE_MODE (inner_type) == BLKmode)
		     && (!STRICT_ALIGNMENT
			 || TYPE_ALIGN (type) <= TYPE_ALIGN (inner_type)
			 || TYPE_ALIGN (inner_type) >= BIGGEST_ALIGNMENT
			 || TYPE_ALIGN_OK (type)
			 || TYPE_ALIGN_OK (inner_type))))
		&& addressable_p (TREE_OPERAND (gnu_expr, 0), NULL_TREE));
      }

    default:
      return false;
    }
}

/* Do the processing for the declaration of a GNAT_ENTITY, a type or subtype.
   If a Freeze node exists for the entity, delay the bulk of the processing.
   Otherwise make a GCC type for GNAT_ENTITY and set up the correspondence.  */

void
process_type (Entity_Id gnat_entity)
{
  tree gnu_old
    = present_gnu_tree (gnat_entity) ? get_gnu_tree (gnat_entity) : NULL_TREE;

  /* If we are to delay elaboration of this type, just do any elaboration
     needed for expressions within the declaration and make a dummy node
     for it and its Full_View (if any), in case something points to it.
     Do not do this if it has already been done (the only way that can
     happen is if the private completion is also delayed).  */
  if (Present (Freeze_Node (gnat_entity)))
    {
      elaborate_entity (gnat_entity);

      if (!gnu_old)
	{
	  tree gnu_decl = TYPE_STUB_DECL (make_dummy_type (gnat_entity));
	  save_gnu_tree (gnat_entity, gnu_decl, false);
	  if (IN (Ekind (gnat_entity), Incomplete_Or_Private_Kind)
	      && Present (Full_View (gnat_entity)))
	    {
	      if (Has_Completion_In_Body (gnat_entity))
		DECL_TAFT_TYPE_P (gnu_decl) = 1;
	      save_gnu_tree (Full_View (gnat_entity), gnu_decl, false);
	    }
	}

      return;
    }

  /* If we saved away a dummy type for this node, it means that this made the
     type that corresponds to the full type of an incomplete type.  Clear that
     type for now and then update the type in the pointers below.  But, if the
     saved type is not dummy, it very likely means that we have a use before
     declaration for the type in the tree, what we really cannot handle.  */
  if (gnu_old)
    {
      gcc_assert (TREE_CODE (gnu_old) == TYPE_DECL
		  && TYPE_IS_DUMMY_P (TREE_TYPE (gnu_old)));

      save_gnu_tree (gnat_entity, NULL_TREE, false);
    }

  /* Now fully elaborate the type.  */
  tree gnu_new = gnat_to_gnu_entity (gnat_entity, NULL_TREE, true);
  gcc_assert (TREE_CODE (gnu_new) == TYPE_DECL);

  /* If we have an old type and we've made pointers to this type, update those
     pointers.  If this is a Taft amendment type in the main unit, we need to
     mark the type as used since other units referencing it don't see the full
     declaration and, therefore, cannot mark it as used themselves.  */
  if (gnu_old)
    {
      update_pointer_to (TYPE_MAIN_VARIANT (TREE_TYPE (gnu_old)),
			 TREE_TYPE (gnu_new));
      if (DECL_TAFT_TYPE_P (gnu_old))
	used_types_insert (TREE_TYPE (gnu_new));
    }

  /* If this is a record type corresponding to a task or protected type
     that is a completion of an incomplete type, perform a similar update
     on the type.  ??? Including protected types here is a guess.  */
  if (IN (Ekind (gnat_entity), Record_Kind)
      && Is_Concurrent_Record_Type (gnat_entity)
      && present_gnu_tree (Corresponding_Concurrent_Type (gnat_entity)))
    {
      tree gnu_task_old
	= get_gnu_tree (Corresponding_Concurrent_Type (gnat_entity));

      save_gnu_tree (Corresponding_Concurrent_Type (gnat_entity),
		     NULL_TREE, false);
      save_gnu_tree (Corresponding_Concurrent_Type (gnat_entity),
		     gnu_new, false);

      update_pointer_to (TYPE_MAIN_VARIANT (TREE_TYPE (gnu_task_old)),
			 TREE_TYPE (gnu_new));
    }
}

/* GNAT_ENTITY is the type of the resulting constructor, GNAT_ASSOC is the
   front of the Component_Associations of an N_Aggregate and GNU_TYPE is the
   GCC type of the corresponding record type.  Return the CONSTRUCTOR.  */

static tree
assoc_to_constructor (Entity_Id gnat_entity, Node_Id gnat_assoc, tree gnu_type)
{
  tree gnu_list = NULL_TREE, gnu_result;

  /* We test for GNU_FIELD being empty in the case where a variant
     was the last thing since we don't take things off GNAT_ASSOC in
     that case.  We check GNAT_ASSOC in case we have a variant, but it
     has no fields.  */

  for (; Present (gnat_assoc); gnat_assoc = Next (gnat_assoc))
    {
      Node_Id gnat_field = First (Choices (gnat_assoc));
      tree gnu_field = gnat_to_gnu_field_decl (Entity (gnat_field));
      tree gnu_expr = gnat_to_gnu (Expression (gnat_assoc));

      /* The expander is supposed to put a single component selector name
	 in every record component association.  */
      gcc_assert (No (Next (gnat_field)));

      /* Ignore fields that have Corresponding_Discriminants since we'll
	 be setting that field in the parent.  */
      if (Present (Corresponding_Discriminant (Entity (gnat_field)))
	  && Is_Tagged_Type (Scope (Entity (gnat_field))))
	continue;

      /* Also ignore discriminants of Unchecked_Unions.  */
      if (Is_Unchecked_Union (gnat_entity)
	  && Ekind (Entity (gnat_field)) == E_Discriminant)
	continue;

      /* Before assigning a value in an aggregate make sure range checks
	 are done if required.  Then convert to the type of the field.  */
      if (Do_Range_Check (Expression (gnat_assoc)))
	gnu_expr = emit_range_check (gnu_expr, Etype (gnat_field), Empty);

      gnu_expr = convert (TREE_TYPE (gnu_field), gnu_expr);

      /* Add the field and expression to the list.  */
      gnu_list = tree_cons (gnu_field, gnu_expr, gnu_list);
    }

  gnu_result = extract_values (gnu_list, gnu_type);

  if (flag_checking)
    {
      /* Verify that every entry in GNU_LIST was used.  */
      for (; gnu_list; gnu_list = TREE_CHAIN (gnu_list))
	gcc_assert (TREE_ADDRESSABLE (gnu_list));
    }

  return gnu_result;
}

/* Build a possibly nested constructor for array aggregates.  GNAT_EXPR is
   the first element of an array aggregate.  It may itself be an aggregate.
   GNU_ARRAY_TYPE is the GCC type corresponding to the array aggregate.
   GNAT_COMPONENT_TYPE is the type of the array component; it is needed
   for range checking.  */

static tree
pos_to_constructor (Node_Id gnat_expr, tree gnu_array_type,
		    Entity_Id gnat_component_type)
{
  tree gnu_index = TYPE_MIN_VALUE (TYPE_DOMAIN (gnu_array_type));
  tree gnu_expr;
  vec<constructor_elt, va_gc> *gnu_expr_vec = NULL;

  for ( ; Present (gnat_expr); gnat_expr = Next (gnat_expr))
    {
      /* If the expression is itself an array aggregate then first build the
	 innermost constructor if it is part of our array (multi-dimensional
	 case).  */
      if (Nkind (gnat_expr) == N_Aggregate
	  && TREE_CODE (TREE_TYPE (gnu_array_type)) == ARRAY_TYPE
	  && TYPE_MULTI_ARRAY_P (TREE_TYPE (gnu_array_type)))
	gnu_expr = pos_to_constructor (First (Expressions (gnat_expr)),
				       TREE_TYPE (gnu_array_type),
				       gnat_component_type);
      else
	{
	  gnu_expr = gnat_to_gnu (gnat_expr);

	  /* Before assigning the element to the array, make sure it is
	     in range.  */
	  if (Do_Range_Check (gnat_expr))
	    gnu_expr = emit_range_check (gnu_expr, gnat_component_type, Empty);
	}

      CONSTRUCTOR_APPEND_ELT (gnu_expr_vec, gnu_index,
			      convert (TREE_TYPE (gnu_array_type), gnu_expr));

      gnu_index = int_const_binop (PLUS_EXPR, gnu_index,
				   convert (TREE_TYPE (gnu_index),
					    integer_one_node));
    }

  return gnat_build_constructor (gnu_array_type, gnu_expr_vec);
}

/* Subroutine of assoc_to_constructor: VALUES is a list of field associations,
   some of which are from RECORD_TYPE.  Return a CONSTRUCTOR consisting of the
   associations that are from RECORD_TYPE.  If we see an internal record, make
   a recursive call to fill it in as well.  */

static tree
extract_values (tree values, tree record_type)
{
  tree field, tem;
  vec<constructor_elt, va_gc> *v = NULL;

  for (field = TYPE_FIELDS (record_type); field; field = DECL_CHAIN (field))
    {
      tree value = 0;

      /* _Parent is an internal field, but may have values in the aggregate,
	 so check for values first.  */
      if ((tem = purpose_member (field, values)))
	{
	  value = TREE_VALUE (tem);
	  TREE_ADDRESSABLE (tem) = 1;
	}

      else if (DECL_INTERNAL_P (field))
	{
	  value = extract_values (values, TREE_TYPE (field));
	  if (TREE_CODE (value) == CONSTRUCTOR
	      && vec_safe_is_empty (CONSTRUCTOR_ELTS (value)))
	    value = 0;
	}
      else
	/* If we have a record subtype, the names will match, but not the
	   actual FIELD_DECLs.  */
	for (tem = values; tem; tem = TREE_CHAIN (tem))
	  if (DECL_NAME (TREE_PURPOSE (tem)) == DECL_NAME (field))
	    {
	      value = convert (TREE_TYPE (field), TREE_VALUE (tem));
	      TREE_ADDRESSABLE (tem) = 1;
	    }

      if (!value)
	continue;

      CONSTRUCTOR_APPEND_ELT (v, field, value);
    }

  return gnat_build_constructor (record_type, v);
}

/* Process a N_Validate_Unchecked_Conversion node.  */

static void
validate_unchecked_conversion (Node_Id gnat_node)
{
  tree gnu_source_type = gnat_to_gnu_type (Source_Type (gnat_node));
  tree gnu_target_type = gnat_to_gnu_type (Target_Type (gnat_node));

  /* If the target is a pointer type, see if we are either converting from a
     non-pointer or from a pointer to a type with a different alias set and
     warn if so, unless the pointer has been marked to alias everything.  */
  if (POINTER_TYPE_P (gnu_target_type)
      && !TYPE_REF_CAN_ALIAS_ALL (gnu_target_type))
    {
      tree gnu_source_desig_type = POINTER_TYPE_P (gnu_source_type)
				   ? TREE_TYPE (gnu_source_type)
				   : NULL_TREE;
      tree gnu_target_desig_type = TREE_TYPE (gnu_target_type);
      alias_set_type target_alias_set = get_alias_set (gnu_target_desig_type);

      if (target_alias_set != 0
	  && (!POINTER_TYPE_P (gnu_source_type)
	      || !alias_sets_conflict_p (get_alias_set (gnu_source_desig_type),
					 target_alias_set)))
	{
	  post_error_ne ("?possible aliasing problem for type&",
			 gnat_node, Target_Type (gnat_node));
	  post_error ("\\?use -fno-strict-aliasing switch for references",
		      gnat_node);
	  post_error_ne ("\\?or use `pragma No_Strict_Aliasing (&);`",
			 gnat_node, Target_Type (gnat_node));
	}
    }

  /* Likewise if the target is a fat pointer type, but we have no mechanism to
     mitigate the problem in this case, so we unconditionally warn.  */
  else if (TYPE_IS_FAT_POINTER_P (gnu_target_type))
    {
      tree gnu_source_desig_type
	= TYPE_IS_FAT_POINTER_P (gnu_source_type)
	  ? TREE_TYPE (TREE_TYPE (TYPE_FIELDS (gnu_source_type)))
	  : NULL_TREE;
      tree gnu_target_desig_type
	= TREE_TYPE (TREE_TYPE (TYPE_FIELDS (gnu_target_type)));
      alias_set_type target_alias_set = get_alias_set (gnu_target_desig_type);

      if (target_alias_set != 0
	  && (!TYPE_IS_FAT_POINTER_P (gnu_source_type)
	      || !alias_sets_conflict_p (get_alias_set (gnu_source_desig_type),
					 target_alias_set)))
	{
	  post_error_ne ("?possible aliasing problem for type&",
			 gnat_node, Target_Type (gnat_node));
	  post_error ("\\?use -fno-strict-aliasing switch for references",
		      gnat_node);
	}
    }
}

/* EXP is to be treated as an array or record.  Handle the cases when it is
   an access object and perform the required dereferences.  */

static tree
maybe_implicit_deref (tree exp)
{
  /* If the type is a pointer, dereference it.  */
  if (POINTER_TYPE_P (TREE_TYPE (exp))
      || TYPE_IS_FAT_POINTER_P (TREE_TYPE (exp)))
    exp = build_unary_op (INDIRECT_REF, NULL_TREE, exp);

  /* If we got a padded type, remove it too.  */
  if (TYPE_IS_PADDING_P (TREE_TYPE (exp)))
    exp = convert (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (exp))), exp);

  return exp;
}

/* Convert SLOC into LOCUS.  Return true if SLOC corresponds to a source code
   location and false if it doesn't.  If CLEAR_COLUMN is true, set the column
   information to 0.  */

bool
Sloc_to_locus (Source_Ptr Sloc, location_t *locus, bool clear_column)
{
  if (Sloc == No_Location)
    return false;

  if (Sloc <= Standard_Location)
    {
      *locus = BUILTINS_LOCATION;
      return false;
    }

  Source_File_Index file = Get_Source_File_Index (Sloc);
  Logical_Line_Number line = Get_Logical_Line_Number (Sloc);
  Column_Number column = (clear_column ? 0 : Get_Column_Number (Sloc));
  line_map_ordinary *map = LINEMAPS_ORDINARY_MAP_AT (line_table, file - 1);

  /* We can have zero if pragma Source_Reference is in effect.  */
  if (line < 1)
    line = 1;

  /* Translate the location.  */
  *locus = linemap_position_for_line_and_column (line_table, map,
						 line, column);

  return true;
}

/* Similar to set_expr_location, but start with the Sloc of GNAT_NODE and
   don't do anything if it doesn't correspond to a source location.  And,
   if CLEAR_COLUMN is true, set the column information to 0.  */

static void
set_expr_location_from_node (tree node, Node_Id gnat_node, bool clear_column)
{
  location_t locus;

  if (!Sloc_to_locus (Sloc (gnat_node), &locus, clear_column))
    return;

  SET_EXPR_LOCATION (node, locus);
}

/* More elaborate version of set_expr_location_from_node to be used in more
   general contexts, for example the result of the translation of a generic
   GNAT node.  */

static void
set_gnu_expr_location_from_node (tree node, Node_Id gnat_node)
{
  /* Set the location information on the node if it is a real expression.
     References can be reused for multiple GNAT nodes and they would get
     the location information of their last use.  Also make sure not to
     overwrite an existing location as it is probably more precise.  */

  switch (TREE_CODE (node))
    {
    CASE_CONVERT:
    case NON_LVALUE_EXPR:
    case SAVE_EXPR:
      break;

    case COMPOUND_EXPR:
      if (EXPR_P (TREE_OPERAND (node, 1)))
	set_gnu_expr_location_from_node (TREE_OPERAND (node, 1), gnat_node);

      /* ... fall through ... */

    default:
      if (!REFERENCE_CLASS_P (node) && !EXPR_HAS_LOCATION (node))
	{
	  set_expr_location_from_node (node, gnat_node);
	  set_end_locus_from_node (node, gnat_node);
	}
      break;
    }
}

/* Set the end_locus information for GNU_NODE, if any, from an explicit end
   location associated with GNAT_NODE or GNAT_NODE itself, whichever makes
   most sense.  Return true if a sensible assignment was performed.  */

static bool
set_end_locus_from_node (tree gnu_node, Node_Id gnat_node)
{
  Node_Id gnat_end_label;
  location_t end_locus;

  /* Pick the GNAT node of which we'll take the sloc to assign to the GCC node
     end_locus when there is one.  We consider only GNAT nodes with a possible
     End_Label attached.  If the End_Label actually was unassigned, fallback
     on the original node.  We'd better assign an explicit sloc associated with
     the outer construct in any case.  */

  switch (Nkind (gnat_node))
    {
    case N_Package_Body:
    case N_Subprogram_Body:
    case N_Block_Statement:
      gnat_end_label = End_Label (Handled_Statement_Sequence (gnat_node));
      break;

    case N_Package_Declaration:
      gnat_end_label = End_Label (Specification (gnat_node));
      break;

    default:
      return false;
    }

  if (Present (gnat_end_label))
    gnat_node = gnat_end_label;

  /* Some expanded subprograms have neither an End_Label nor a Sloc
     attached.  Notify that to callers.  For a block statement with no
     End_Label, clear column information, so that the tree for a
     transient block does not receive the sloc of a source condition.  */
  if (!Sloc_to_locus (Sloc (gnat_node), &end_locus,
                      No (gnat_end_label)
                      && (Nkind (gnat_node) == N_Block_Statement)))
    return false;

  switch (TREE_CODE (gnu_node))
    {
    case BIND_EXPR:
      BLOCK_SOURCE_END_LOCATION (BIND_EXPR_BLOCK (gnu_node)) = end_locus;
      return true;

    case FUNCTION_DECL:
      DECL_STRUCT_FUNCTION (gnu_node)->function_end_locus = end_locus;
      return true;

    default:
      return false;
    }
}

/* Return a colon-separated list of encodings contained in encoded Ada
   name.  */

static const char *
extract_encoding (const char *name)
{
  char *encoding = (char *) ggc_alloc_atomic (strlen (name));
  get_encoding (name, encoding);
  return encoding;
}

/* Extract the Ada name from an encoded name.  */

static const char *
decode_name (const char *name)
{
  char *decoded = (char *) ggc_alloc_atomic (strlen (name) * 2 + 60);
  __gnat_decode (name, decoded, 0);
  return decoded;
}

/* Post an error message.  MSG is the error message, properly annotated.
   NODE is the node at which to post the error and the node to use for the
   '&' substitution.  */

void
post_error (const char *msg, Node_Id node)
{
  String_Template temp;
  String_Pointer sp;

  if (No (node))
    return;

  temp.Low_Bound = 1;
  temp.High_Bound = strlen (msg);
  sp.Bounds = &temp;
  sp.Array = msg;
  Error_Msg_N (sp, node);
}

/* Similar to post_error, but NODE is the node at which to post the error and
   ENT is the node to use for the '&' substitution.  */

void
post_error_ne (const char *msg, Node_Id node, Entity_Id ent)
{
  String_Template temp;
  String_Pointer sp;

  if (No (node))
    return;

  temp.Low_Bound = 1;
  temp.High_Bound = strlen (msg);
  sp.Bounds = &temp;
  sp.Array = msg;
  Error_Msg_NE (sp, node, ent);
}

/* Similar to post_error_ne, but NUM is the number to use for the '^'.  */

void
post_error_ne_num (const char *msg, Node_Id node, Entity_Id ent, int num)
{
  Error_Msg_Uint_1 = UI_From_Int (num);
  post_error_ne (msg, node, ent);
}

/* Similar to post_error_ne, but T is a GCC tree representing the number to
   write.  If T represents a constant, the text inside curly brackets in
   MSG will be output (presumably including a '^').  Otherwise it will not
   be output and the text inside square brackets will be output instead.  */

void
post_error_ne_tree (const char *msg, Node_Id node, Entity_Id ent, tree t)
{
  char *new_msg = XALLOCAVEC (char, strlen (msg) + 1);
  char start_yes, end_yes, start_no, end_no;
  const char *p;
  char *q;

  if (TREE_CODE (t) == INTEGER_CST)
    {
      Error_Msg_Uint_1 = UI_From_gnu (t);
      start_yes = '{', end_yes = '}', start_no = '[', end_no = ']';
    }
  else
    start_yes = '[', end_yes = ']', start_no = '{', end_no = '}';

  for (p = msg, q = new_msg; *p; p++)
    {
      if (*p == start_yes)
	for (p++; *p != end_yes; p++)
	  *q++ = *p;
      else if (*p == start_no)
	for (p++; *p != end_no; p++)
	  ;
      else
	*q++ = *p;
    }

  *q = 0;

  post_error_ne (new_msg, node, ent);
}

/* Similar to post_error_ne_tree, but NUM is a second integer to write.  */

void
post_error_ne_tree_2 (const char *msg, Node_Id node, Entity_Id ent, tree t,
		      int num)
{
  Error_Msg_Uint_2 = UI_From_Int (num);
  post_error_ne_tree (msg, node, ent, t);
}

/* Return a label to branch to for the exception type in KIND or NULL_TREE
   if none.  */

tree
get_exception_label (char kind)
{
  switch (kind)
    {
    case N_Raise_Constraint_Error:
      return gnu_constraint_error_label_stack->last ();

    case N_Raise_Storage_Error:
      return gnu_storage_error_label_stack->last ();

    case N_Raise_Program_Error:
      return gnu_program_error_label_stack->last ();

    default:
      break;
    }

  return NULL_TREE;
}

/* Return the decl for the current elaboration procedure.  */

tree
get_elaboration_procedure (void)
{
  return gnu_elab_proc_stack->last ();
}

/* Initialize the table that maps GNAT codes to GCC codes for simple
   binary and unary operations.  */

static void
init_code_table (void)
{
  gnu_codes[N_Op_And] = TRUTH_AND_EXPR;
  gnu_codes[N_Op_Or] = TRUTH_OR_EXPR;
  gnu_codes[N_Op_Xor] = TRUTH_XOR_EXPR;
  gnu_codes[N_Op_Eq] = EQ_EXPR;
  gnu_codes[N_Op_Ne] = NE_EXPR;
  gnu_codes[N_Op_Lt] = LT_EXPR;
  gnu_codes[N_Op_Le] = LE_EXPR;
  gnu_codes[N_Op_Gt] = GT_EXPR;
  gnu_codes[N_Op_Ge] = GE_EXPR;
  gnu_codes[N_Op_Add] = PLUS_EXPR;
  gnu_codes[N_Op_Subtract] = MINUS_EXPR;
  gnu_codes[N_Op_Multiply] = MULT_EXPR;
  gnu_codes[N_Op_Mod] = FLOOR_MOD_EXPR;
  gnu_codes[N_Op_Rem] = TRUNC_MOD_EXPR;
  gnu_codes[N_Op_Minus] = NEGATE_EXPR;
  gnu_codes[N_Op_Abs] = ABS_EXPR;
  gnu_codes[N_Op_Not] = TRUTH_NOT_EXPR;
  gnu_codes[N_Op_Rotate_Left] = LROTATE_EXPR;
  gnu_codes[N_Op_Rotate_Right] = RROTATE_EXPR;
  gnu_codes[N_Op_Shift_Left] = LSHIFT_EXPR;
  gnu_codes[N_Op_Shift_Right] = RSHIFT_EXPR;
  gnu_codes[N_Op_Shift_Right_Arithmetic] = RSHIFT_EXPR;
  gnu_codes[N_And_Then] = TRUTH_ANDIF_EXPR;
  gnu_codes[N_Or_Else] = TRUTH_ORIF_EXPR;
}

#include "gt-ada-trans.h"
