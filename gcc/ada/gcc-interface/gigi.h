/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                 G I G I                                  *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *          Copyright (C) 1992-2018, Free Software Foundation, Inc.         *
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

/* Declare all functions and types used by gigi.  */

/* Given GNAT_ENTITY, a GNAT defining identifier node, which denotes some Ada
   entity, this routine returns the equivalent GCC tree for that entity
   (an ..._DECL node) and associates the ..._DECL node with the input GNAT
   defining identifier.

   If GNAT_ENTITY is a variable or a constant declaration, GNU_EXPR gives its
   initial value (in GCC tree form). This is optional for variables.
   For renamed entities, GNU_EXPR gives the object being renamed.

   DEFINITION is true if this call is intended for a definition.  This is used
   for separate compilation where it is necessary to know whether an external
   declaration or a definition must be created if the GCC equivalent was not
   created previously.  */
extern tree gnat_to_gnu_entity (Entity_Id gnat_entity, tree gnu_expr,
                                bool definition);

/* Similar, but if the returned value is a COMPONENT_REF, return the
   FIELD_DECL.  */
extern tree gnat_to_gnu_field_decl (Entity_Id gnat_entity);

/* Similar, but GNAT_ENTITY is assumed to refer to a GNAT type.  Return
   the GCC type corresponding to that entity.  */
extern tree gnat_to_gnu_type (Entity_Id gnat_entity);

/* Update the GCC tree previously built for the profiles involving GNU_TYPE,
   a dummy type which appears in profiles.  */
extern void update_profiles_with (tree gnu_type);

/* Start a new statement group chained to the previous group.  */
extern void start_stmt_group (void);

/* Add GNU_STMT to the current statement group.  If it is an expression with
   no effects, it is ignored.  */
extern void add_stmt (tree gnu_stmt);

/* Similar, but the statement is always added, regardless of side-effects.  */
extern void add_stmt_force (tree gnu_stmt);

/* Like add_stmt, but set the location of GNU_STMT to that of GNAT_NODE.  */
extern void add_stmt_with_node (tree gnu_stmt, Node_Id gnat_node);

/* Similar, but the statement is always added, regardless of side-effects.  */
extern void add_stmt_with_node_force (tree gnu_stmt, Node_Id gnat_node);

/* Return code corresponding to the current code group.  It is normally
   a STATEMENT_LIST, but may also be a BIND_EXPR or TRY_FINALLY_EXPR if
   BLOCK or cleanups were set.  */
extern tree end_stmt_group (void);

/* Set the BLOCK node corresponding to the current code group to GNU_BLOCK.  */
extern void set_block_for_group (tree);

/* Add a declaration statement for GNU_DECL to the current statement group.
   Get the SLOC to be put onto the statement from GNAT_NODE.  */
extern void add_decl_expr (tree gnu_decl, Node_Id gnat_node);

/* Mark nodes rooted at T with TREE_VISITED and types as having their
   sized gimplified.  We use this to indicate all variable sizes and
   positions in global types may not be shared by any subprogram.  */
extern void mark_visited (tree t);

/* This macro calls the above function but short-circuits the common
   case of a constant to save time and also checks for NULL.  */
#define MARK_VISITED(EXP)		\
do {					\
  if((EXP) && !CONSTANT_CLASS_P (EXP))	\
    mark_visited (EXP);			\
} while (0)

/* Finalize the processing of From_Limited_With incomplete types.  */
extern void finalize_from_limited_with (void);

/* Return the equivalent type to be used for GNAT_ENTITY, if it's a kind
   of type (such E_Task_Type) that has a different type which Gigi uses
   for its representation.  If the type does not have a special type for
   its representation, return GNAT_ENTITY.  */
extern Entity_Id Gigi_Equivalent_Type (Entity_Id gnat_entity);

/* Given GNAT_ENTITY, elaborate all expressions that are required to
   be elaborated at the point of its definition, but do nothing else.  */
extern void elaborate_entity (Entity_Id gnat_entity);

/* Get the unpadded version of a GNAT type.  */
extern tree get_unpadded_type (Entity_Id gnat_entity);

/* Create a record type that contains a SIZE bytes long field of TYPE with a
    starting bit position so that it is aligned to ALIGN bits, and leaving at
    least ROOM bytes free before the field.  BASE_ALIGN is the alignment the
    record is guaranteed to get.  GNAT_NODE is used for the position of the
    associated TYPE_DECL.  */
extern tree make_aligning_type (tree type, unsigned int align, tree size,
				unsigned int base_align, int room, Node_Id);

/* TYPE is a RECORD_TYPE, UNION_TYPE or QUAL_UNION_TYPE that is being used
   as the field type of a packed record if IN_RECORD is true, or as the
   component type of a packed array if IN_RECORD is false.  See if we can
   rewrite it either as a type that has a non-BLKmode, which we can pack
   tighter in the packed record case, or as a smaller type with at most
   MAX_ALIGN alignment if the value is non-zero.  If so, return the new
   type; if not, return the original type.  */
extern tree make_packable_type (tree type, bool in_record,
				unsigned int max_align = 0);

/* Given a type TYPE, return a new type whose size is appropriate for SIZE.
   If TYPE is the best type, return it.  Otherwise, make a new type.  We
   only support new integral and pointer types.  FOR_BIASED is true if
   we are making a biased type.  */
extern tree make_type_from_size (tree type, tree size_tree, bool for_biased);

/* Ensure that TYPE has SIZE and ALIGN.  Make and return a new padded type
   if needed.  We have already verified that SIZE and ALIGN are large enough.
   GNAT_ENTITY is used to name the resulting record and to issue a warning.
   IS_COMPONENT_TYPE is true if this is being done for the component type of
   an array.  IS_USER_TYPE is true if the original type needs to be completed.
   DEFINITION is true if this type is being defined.  SET_RM_SIZE is true if
   the RM size of the resulting type is to be set to SIZE too; in this case,
   the padded type is canonicalized before being returned.  */
extern tree maybe_pad_type (tree type, tree size, unsigned int align,
			    Entity_Id gnat_entity, bool is_component_type,
			    bool is_user_type, bool definition,
			    bool set_rm_size);

/* Return true if padded TYPE was built with an RM size.  */
extern bool pad_type_has_rm_size (tree type);

/* Return a copy of the padded TYPE but with reverse storage order.  */
extern tree set_reverse_storage_order_on_pad_type (tree type);

enum alias_set_op
{
  ALIAS_SET_COPY,
  ALIAS_SET_SUBSET,
  ALIAS_SET_SUPERSET
};

/* Relate the alias sets of GNU_NEW_TYPE and GNU_OLD_TYPE according to OP.
   If this is a multi-dimensional array type, do this recursively.

   OP may be
   - ALIAS_SET_COPY:     the new set is made a copy of the old one.
   - ALIAS_SET_SUPERSET: the new set is made a superset of the old one.
   - ALIAS_SET_SUBSET:   the new set is made a subset of the old one.  */
extern void relate_alias_sets (tree gnu_new_type, tree gnu_old_type,
			       enum alias_set_op op);

/* Given GNAT_ENTITY, an object (constant, variable, parameter, exception)
   and GNU_TYPE, its corresponding GCC type, set Esize and Alignment to the
   size and alignment used by Gigi.  Prefer SIZE over TYPE_SIZE if non-null.
   BY_REF is true if the object is used by reference.  */
extern void annotate_object (Entity_Id gnat_entity, tree gnu_type, tree size,
			     bool by_ref);

/* Return the variant part of RECORD_TYPE, if any.  Otherwise return NULL.  */
extern tree get_variant_part (tree record_type);

/* Given a type T, a FIELD_DECL F, and a replacement value R, return a new
   type with all size expressions that contain F updated by replacing F
   with R.  If F is NULL_TREE, always make a new RECORD_TYPE, even if
   nothing has changed.  */
extern tree substitute_in_type (tree t, tree f, tree r);

/* Return the RM size of GNU_TYPE.  This is the actual number of bits
   needed to represent the object.  */
extern tree rm_size (tree gnu_type);

/* Return the name to be used for GNAT_ENTITY.  If a type, create a
   fully-qualified name, possibly with type information encoding.
   Otherwise, return the name.  */
extern tree get_entity_name (Entity_Id gnat_entity);

/* Return an identifier representing the external name to be used for
   GNAT_ENTITY.  If SUFFIX is specified, the name is followed by "___"
   and the specified suffix.  */
extern tree create_concat_name (Entity_Id gnat_entity, const char *suffix);

/* Given GNU_NAME, an IDENTIFIER_NODE containing a name and SUFFIX, a
   string, return a new IDENTIFIER_NODE that is the concatenation of
   the name followed by "___" and the specified suffix.  */
extern tree concat_name (tree gnu_name, const char *suffix);

/* Initialize data structures of the decl.c module.  */
extern void init_gnat_decl (void);

/* Destroy data structures of the decl.c module.  */
extern void destroy_gnat_decl (void);

/* Highest number in the front-end node table.  */
extern int max_gnat_nodes;

/* Current node being treated, in case abort called.  */
extern Node_Id error_gnat_node;

/* True when gigi is being called on an analyzed but unexpanded
   tree, and the only purpose of the call is to properly annotate
   types with representation information.  */
extern bool type_annotate_only;

/* This structure must be kept synchronized with Call_Back_End.  */
struct File_Info_Type
{
  File_Name_Type File_Name;
  Instance_Id    Instance;
  Nat            Num_Source_Lines;
};

#ifdef __cplusplus
extern "C" {
#endif

/* This is the main program of the back-end.  It sets up all the table
   structures and then generates code.  */
extern void gigi (Node_Id gnat_root,
	          int max_gnat_node,
                  int number_name,
		  struct Node *nodes_ptr,
		  struct Flags *Flags_Ptr,
		  Node_Id *next_node_ptr,
		  Node_Id *prev_node_ptr,
		  struct Elist_Header *elists_ptr,
                  struct Elmt_Item *elmts_ptr,
                  struct String_Entry *strings_ptr,
                  Char_Code *strings_chars_ptr,
                  struct List_Header *list_headers_ptr,
                  Nat number_file,
                  struct File_Info_Type *file_info_ptr,
                  Entity_Id standard_boolean,
                  Entity_Id standard_integer,
                  Entity_Id standard_character,
                  Entity_Id standard_long_long_float,
                  Entity_Id standard_exception_type,
                  Int gigi_operating_mode);

#ifdef __cplusplus
}
#endif

/* GNAT_NODE is the root of some GNAT tree.  Return the root of the
   GCC tree corresponding to that GNAT tree.  */
extern tree gnat_to_gnu (Node_Id gnat_node);

/* Similar to gnat_to_gnu, but discard any object that might be created in
   the course of the translation of GNAT_NODE, which must be an "external"
   expression in the sense that it will be elaborated elsewhere.  */
extern tree gnat_to_gnu_external (Node_Id gnat_node);

/* GNU_STMT is a statement.  We generate code for that statement.  */
extern void gnat_expand_stmt (tree gnu_stmt);

/* Generate GIMPLE in place for the expression at *EXPR_P.  */
extern int gnat_gimplify_expr (tree *expr_p, gimple_seq *pre_p, gimple_seq *);

/* Do the processing for the declaration of a GNAT_ENTITY, a type.  If
   a separate Freeze node exists, delay the bulk of the processing.  Otherwise
   make a GCC type for GNAT_ENTITY and set up the correspondence.  */
extern void process_type (Entity_Id gnat_entity);

/* Convert SLOC into LOCUS.  Return true if SLOC corresponds to a source code
   location and false if it doesn't.  If CLEAR_COLUMN is true, set the column
   information to 0.  */
extern bool Sloc_to_locus (Source_Ptr Sloc, location_t *locus,
			   bool clear_column = false);

/* Post an error message.  MSG is the error message, properly annotated.
   NODE is the node at which to post the error and the node to use for the
   '&' substitution.  */
extern void post_error (const char *msg, Node_Id node);

/* Similar to post_error, but NODE is the node at which to post the error and
   ENT is the node to use for the '&' substitution.  */
extern void post_error_ne (const char *msg, Node_Id node, Entity_Id ent);

/* Similar to post_error_ne, but NUM is the number to use for the '^'.  */
extern void post_error_ne_num (const char *msg, Node_Id node, Entity_Id ent,
                               int num);

/* Similar to post_error_ne, but T is a GCC tree representing the number to
   write.  If T represents a constant, the text inside curly brackets in
   MSG will be output (presumably including a '^').  Otherwise it will not
   be output and the text inside square brackets will be output instead.  */
extern void post_error_ne_tree (const char *msg, Node_Id node, Entity_Id ent,
                                tree t);

/* Similar to post_error_ne_tree, but NUM is a second integer to write.  */
extern void post_error_ne_tree_2 (const char *msg, Node_Id node, Entity_Id ent,
                                  tree t, int num);

/* Return a label to branch to for the exception type in KIND or Empty
   if none.  */
extern Entity_Id get_exception_label (char kind);

/* If nonzero, pretend we are allocating at global level.  */
extern int force_global;

/* The default alignment of "double" floating-point types, i.e. floating
   point types whose size is equal to 64 bits, or 0 if this alignment is
   not specifically capped.  */
extern int double_float_alignment;

/* The default alignment of "double" or larger scalar types, i.e. scalar
   types whose size is greater or equal to 64 bits, or 0 if this alignment
   is not specifically capped.  */
extern int double_scalar_alignment;

/* True if floating-point arithmetics may use wider intermediate results.  */
extern bool fp_arith_may_widen;

/* Data structures used to represent attributes.  */

enum attrib_type
{
  ATTR_MACHINE_ATTRIBUTE,
  ATTR_LINK_ALIAS,
  ATTR_LINK_SECTION,
  ATTR_LINK_CONSTRUCTOR,
  ATTR_LINK_DESTRUCTOR,
  ATTR_THREAD_LOCAL_STORAGE,
  ATTR_WEAK_EXTERNAL
};

struct attrib
{
  struct attrib *next;
  enum attrib_type type;
  tree name;
  tree args;
  Node_Id error_point;
};

/* Table of machine-independent internal attributes.  */
extern const struct attribute_spec gnat_internal_attribute_table[];

/* Define the entries in the standard data array.  */
enum standard_datatypes
{
  /* The longest floating-point type.  */
  ADT_longest_float_type,

  /* The type of an exception.  */
  ADT_except_type,

  /* Function type declaration -- void T() */
  ADT_void_ftype,

  /* Type declaration node  <==> typedef void *T() */
  ADT_ptr_void_ftype,

  /* Type declaration node  <==> typedef virtual void *T() */
  ADT_fdesc_type,

  /* Null pointer for above type.  */
  ADT_null_fdesc,

  /* Value 1 in signed bitsizetype.  */
  ADT_sbitsize_one_node,

  /* Value BITS_PER_UNIT in signed bitsizetype.  */
  ADT_sbitsize_unit_node,

  /* Function declaration node for run-time allocation function.  */
  ADT_malloc_decl,

  /* Function declaration node for run-time freeing function.  */
  ADT_free_decl,

  /* Function declaration node for run-time reallocation function.  */
  ADT_realloc_decl,

  /* Function decl node for 64-bit multiplication with overflow checking.  */
  ADT_mulv64_decl,

  /* Identifier for the name of the _Parent field in tagged record types.  */
  ADT_parent_name_id,

  /* Identifier for the name of the Exception_Data type.  */
  ADT_exception_data_name_id,

  /* Types and decls used by the SJLJ exception mechanism.  */
  ADT_jmpbuf_type,
  ADT_jmpbuf_ptr_type,
  ADT_get_jmpbuf_decl,
  ADT_set_jmpbuf_decl,
  ADT_get_excptr_decl,
  ADT_not_handled_by_others_decl,
  ADT_setjmp_decl,
  ADT_update_setjmp_buf_decl,
  ADT_raise_nodefer_decl,

  /* Types and decls used by the ZCX exception mechanism.  */
  ADT_reraise_zcx_decl,
  ADT_set_exception_parameter_decl,
  ADT_begin_handler_decl,
  ADT_end_handler_decl,
  ADT_unhandled_except_decl,
  ADT_others_decl,
  ADT_all_others_decl,
  ADT_unhandled_others_decl,

  ADT_LAST
};

/* Define kind of exception information associated with raise statements.  */
enum exception_info_kind
{
  /* Simple exception information: file:line.  */
  exception_simple,
  /* Range exception information: file:line + index, first, last.  */
  exception_range,
  /* Column exception information: file:line:column.  */
  exception_column
};

/* Define the inline status of a subprogram.  */
enum inline_status_t
{
  /* Inlining is suppressed for the subprogram.  */
  is_suppressed,
  /* No inlining is requested for the subprogram.  */
  is_disabled,
  /* Inlining is requested for the subprogram.  */
  is_enabled,
  /* Inlining is required for the subprogram.  */
  is_required
};

extern GTY(()) tree gnat_std_decls[(int) ADT_LAST];
extern GTY(()) tree gnat_raise_decls[(int) LAST_REASON_CODE + 1];
extern GTY(()) tree gnat_raise_decls_ext[(int) LAST_REASON_CODE + 1];

#define longest_float_type_node gnat_std_decls[(int) ADT_longest_float_type]
#define except_type_node gnat_std_decls[(int) ADT_except_type]
#define void_ftype gnat_std_decls[(int) ADT_void_ftype]
#define ptr_void_ftype gnat_std_decls[(int) ADT_ptr_void_ftype]
#define fdesc_type_node gnat_std_decls[(int) ADT_fdesc_type]
#define null_fdesc_node gnat_std_decls[(int) ADT_null_fdesc]
#define sbitsize_one_node gnat_std_decls[(int) ADT_sbitsize_one_node]
#define sbitsize_unit_node gnat_std_decls[(int) ADT_sbitsize_unit_node]
#define malloc_decl gnat_std_decls[(int) ADT_malloc_decl]
#define free_decl gnat_std_decls[(int) ADT_free_decl]
#define realloc_decl gnat_std_decls[(int) ADT_realloc_decl]
#define mulv64_decl gnat_std_decls[(int) ADT_mulv64_decl]
#define parent_name_id gnat_std_decls[(int) ADT_parent_name_id]
#define exception_data_name_id gnat_std_decls[(int) ADT_exception_data_name_id]
#define jmpbuf_type gnat_std_decls[(int) ADT_jmpbuf_type]
#define jmpbuf_ptr_type gnat_std_decls[(int) ADT_jmpbuf_ptr_type]
#define get_jmpbuf_decl gnat_std_decls[(int) ADT_get_jmpbuf_decl]
#define set_jmpbuf_decl gnat_std_decls[(int) ADT_set_jmpbuf_decl]
#define get_excptr_decl gnat_std_decls[(int) ADT_get_excptr_decl]
#define not_handled_by_others_decl \
	  gnat_std_decls[(int) ADT_not_handled_by_others_decl]
#define setjmp_decl gnat_std_decls[(int) ADT_setjmp_decl]
#define update_setjmp_buf_decl gnat_std_decls[(int) ADT_update_setjmp_buf_decl]
#define raise_nodefer_decl gnat_std_decls[(int) ADT_raise_nodefer_decl]
#define reraise_zcx_decl gnat_std_decls[(int) ADT_reraise_zcx_decl]
#define set_exception_parameter_decl \
	  gnat_std_decls[(int) ADT_set_exception_parameter_decl]
#define begin_handler_decl gnat_std_decls[(int) ADT_begin_handler_decl]
#define others_decl gnat_std_decls[(int) ADT_others_decl]
#define all_others_decl gnat_std_decls[(int) ADT_all_others_decl]
#define unhandled_others_decl gnat_std_decls[(int) ADT_unhandled_others_decl]
#define end_handler_decl gnat_std_decls[(int) ADT_end_handler_decl]
#define unhandled_except_decl gnat_std_decls[(int) ADT_unhandled_except_decl]

/* Routines expected by the gcc back-end. They must have exactly the same
   prototype and names as below.  */

/* Return true if we are in the global binding level.  */
extern bool global_bindings_p (void);

/* Enter and exit a new binding level.  */
extern void gnat_pushlevel (void);
extern void gnat_poplevel (void);
extern void gnat_zaplevel (void);

/* Set SUPERCONTEXT of the BLOCK for the current binding level to FNDECL
   and point FNDECL to this BLOCK.  */
extern void set_current_block_context (tree fndecl);

/* Set the jmpbuf_decl for the current binding level to DECL.  */
extern void set_block_jmpbuf_decl (tree decl);

/* Get the setjmp_decl, if any, for the current binding level.  */
extern tree get_block_jmpbuf_decl (void);

/* Record DECL as belonging to the current lexical scope and use GNAT_NODE
   for location information and flag propagation.  */
extern void gnat_pushdecl (tree decl, Node_Id gnat_node);

/* Initialize the GCC support for exception handling.  */
extern void gnat_init_gcc_eh (void);

/* Initialize the GCC support for floating-point operations.  */
extern void gnat_init_gcc_fp (void);

/* Install the builtin functions we might need.  */
extern void gnat_install_builtins (void);

/* Return an integer type with the number of bits of precision given by
   PRECISION.  UNSIGNEDP is nonzero if the type is unsigned; otherwise
   it is a signed type.  */
extern tree gnat_type_for_size (unsigned precision, int unsignedp);

/* Return a data type that has machine mode MODE.  UNSIGNEDP selects
   an unsigned type; otherwise a signed type is returned.  */
extern tree gnat_type_for_mode (machine_mode mode, int unsignedp);

/* Perform final processing on global declarations.  */
extern void gnat_write_global_declarations (void);

/* Return the signed or unsigned version of TYPE_NODE, a scalar type, the
   signedness being specified by UNSIGNEDP.  */
extern tree gnat_signed_or_unsigned_type_for (int unsignedp, tree type_node);

/* Return 1 if the types T1 and T2 are compatible, i.e. if they can be
   transparently converted to each other.  */
extern int gnat_types_compatible_p (tree t1, tree t2);

/* Return true if EXPR is a useless type conversion.  */
extern bool gnat_useless_type_conversion (tree expr);

/* Return true if T, a {FUNCTION,METHOD}_TYPE, has the specified flags.  */
extern bool fntype_same_flags_p (const_tree, tree, bool, bool, bool);

/* Create an expression whose value is that of EXPR,
   converted to type TYPE.  The TREE_TYPE of the value
   is always TYPE.  This function implements all reasonable
   conversions; callers should filter out those that are
   not permitted by the language being compiled.  */
extern tree convert (tree type, tree expr);

/* Create an expression whose value is that of EXPR converted to the common
   index type, which is sizetype.  */
extern tree convert_to_index_type (tree expr);

/* Routines created solely for the tree translator's sake. Their prototypes
   can be changed as desired.  */

/* Initialize data structures of the utils.c module.  */
extern void init_gnat_utils (void);

/* Destroy data structures of the utils.c module.  */
extern void destroy_gnat_utils (void);

/* GNAT_ENTITY is a GNAT tree node for a defining identifier.
   GNU_DECL is the GCC tree which is to be associated with
   GNAT_ENTITY. Such gnu tree node is always an ..._DECL node.
   If NO_CHECK is nonzero, the latter check is suppressed.
   If GNU_DECL is zero, a previous association is to be reset.  */
extern void save_gnu_tree (Entity_Id gnat_entity, tree gnu_decl,
                           bool no_check);

/* GNAT_ENTITY is a GNAT tree node for a defining identifier.
   Return the ..._DECL node that was associated with it.  If there is no tree
   node associated with GNAT_ENTITY, abort.  */
extern tree get_gnu_tree (Entity_Id gnat_entity);

/* Return nonzero if a GCC tree has been associated with GNAT_ENTITY.  */
extern bool present_gnu_tree (Entity_Id gnat_entity);

/* Make a dummy type corresponding to GNAT_TYPE.  */
extern tree make_dummy_type (Entity_Id gnat_type);

/* Return the dummy type that was made for GNAT_TYPE, if any.  */
extern tree get_dummy_type (Entity_Id gnat_type);

/* Build dummy fat and thin pointer types whose designated type is specified
   by GNAT_DESIG_TYPE/GNU_DESIG_TYPE and attach them to the latter.  */
extern void build_dummy_unc_pointer_types (Entity_Id gnat_desig_type,
					   tree gnu_desig_type);

/* Record TYPE as a builtin type for Ada.  NAME is the name of the type.
   ARTIFICIAL_P is true if the type was generated by the compiler.  */
extern void record_builtin_type (const char *name, tree type,
				 bool artificial_p);

/* Finish constructing the character type CHAR_TYPE.  */
extern void finish_character_type (tree char_type);

/* Given a record type RECORD_TYPE and a list of FIELD_DECL nodes FIELD_LIST,
   finish constructing the record type as a fat pointer type.  */
extern void finish_fat_pointer_type (tree record_type, tree field_list);

/* Given a record type RECORD_TYPE and a list of FIELD_DECL nodes FIELD_LIST,
   finish constructing the record or union type.  If REP_LEVEL is zero, this
   record has no representation clause and so will be entirely laid out here.
   If REP_LEVEL is one, this record has a representation clause and has been
   laid out already; only set the sizes and alignment.  If REP_LEVEL is two,
   this record is derived from a parent record and thus inherits its layout;
   only make a pass on the fields to finalize them.  DEBUG_INFO_P is true if
   additional debug info needs to be output for this type.  */
extern void finish_record_type (tree record_type, tree field_list,
				int rep_level, bool debug_info_p);

/* Wrap up compilation of RECORD_TYPE, i.e. output additional debug info
   associated with it.  It need not be invoked directly in most cases as
   finish_record_type takes care of doing so.  */
extern void rest_of_record_type_compilation (tree record_type);

/* Append PARALLEL_TYPE on the chain of parallel types for TYPE.  */
extern void add_parallel_type (tree type, tree parallel_type);

/* Return a copy of TYPE, but safe to modify in any way.  */
extern tree copy_type (tree type);

/* Return a subtype of sizetype with range MIN to MAX and whose
   TYPE_INDEX_TYPE is INDEX.  GNAT_NODE is used for the position
   of the associated TYPE_DECL.  */
extern tree create_index_type (tree min, tree max, tree index,
			       Node_Id gnat_node);

/* Return a subtype of TYPE with range MIN to MAX.  If TYPE is NULL,
   sizetype is used.  */
extern tree create_range_type (tree type, tree min, tree max);

/* Return a TYPE_DECL node suitable for the TYPE_STUB_DECL field of TYPE.
   NAME gives the name of the type to be used in the declaration.  */
extern tree create_type_stub_decl (tree name, tree type);

/* Return a TYPE_DECL node for TYPE.  NAME gives the name of the type to be
   used in the declaration.  ARTIFICIAL_P is true if the declaration was
   generated by the compiler.  DEBUG_INFO_P is true if we need to write
   debug information about this type.  GNAT_NODE is used for the position
   of the decl.  */
extern tree create_type_decl (tree name, tree type, bool artificial_p,
			      bool debug_info_p, Node_Id gnat_node);

/* Return a VAR_DECL or CONST_DECL node.

   NAME gives the name of the variable.  ASM_NAME is its assembler name
   (if provided).  TYPE is its data type (a GCC ..._TYPE node).  INIT is
   the GCC tree for an optional initial expression; NULL_TREE if none.

   CONST_FLAG is true if this variable is constant, in which case we might
   return a CONST_DECL node unless CONST_DECL_ALLOWED_P is false.

   PUBLIC_FLAG is true if this is for a reference to a public entity or for a
   definition to be made visible outside of the current compilation unit, for
   instance variable definitions in a package specification.

   EXTERN_FLAG is true when processing an external variable declaration (as
   opposed to a definition: no storage is to be allocated for the variable).

   STATIC_FLAG is only relevant when not at top level and indicates whether
   to always allocate storage to the variable.

   VOLATILE_FLAG is true if this variable is declared as volatile.

   ARTIFICIAL_P is true if the variable was generated by the compiler.

   DEBUG_INFO_P is true if we need to write debug information for it.

   ATTR_LIST is the list of attributes to be attached to the variable.

   GNAT_NODE is used for the position of the decl.  */
extern tree create_var_decl (tree name, tree asm_name, tree type, tree init,
			     bool const_flag, bool public_flag,
			     bool extern_flag, bool static_flag,
			     bool volatile_flag,
			     bool artificial_p, bool debug_info_p,
			     struct attrib *attr_list, Node_Id gnat_node,
			     bool const_decl_allowed_p = true);

/* Return a FIELD_DECL node.  NAME is the field's name, TYPE is its type and
   RECORD_TYPE is the type of the enclosing record.  If SIZE is nonzero, it
   is the specified size of the field.  If POS is nonzero, it is the bit
   position.  PACKED is 1 if the enclosing record is packed, -1 if it has
   Component_Alignment of Storage_Unit.  If ADDRESSABLE is nonzero, it
   means we are allowed to take the address of the field; if it is negative,
   we should not make a bitfield, which is used by make_aligning_type.  */
extern tree create_field_decl (tree name, tree type, tree record_type,
			       tree size, tree pos, int packed,
			       int addressable);

/* Return a PARM_DECL node with NAME and TYPE.  */
extern tree create_param_decl (tree name, tree type);

/* Return a LABEL_DECL with NAME.  GNAT_NODE is used for the position of
   the decl.  */
extern tree create_label_decl (tree name, Node_Id gnat_node);

/* Return a FUNCTION_DECL node.  NAME is the name of the subprogram, ASM_NAME
   its assembler name, TYPE its type (a FUNCTION_TYPE node), PARAM_DECL_LIST
   the list of its parameters (a list of PARM_DECL nodes chained through the
   DECL_CHAIN field).

   INLINE_STATUS describes the inline flags to be set on the FUNCTION_DECL.

   PUBLIC_FLAG is true if this is for a reference to a public entity or for a
   definition to be made visible outside of the current compilation unit.

   EXTERN_FLAG is true when processing an external subprogram declaration.

   ARTIFICIAL_P is true if the subprogram was generated by the compiler.

   DEBUG_INFO_P is true if we need to write debug information for it.

   DEFINITION is true if the subprogram is to be considered as a definition.

   ATTR_LIST is the list of attributes to be attached to the subprogram.

   GNAT_NODE is used for the position of the decl.  */
extern tree create_subprog_decl (tree name, tree asm_name, tree type,
				 tree param_decl_list,
				 enum inline_status_t inline_status,
				 bool public_flag, bool extern_flag,
				 bool artificial_p, bool debug_info_p,
				 bool definition, struct attrib *attr_list,
				 Node_Id gnat_node);

/* Given a subprogram declaration DECL, its assembler name and its type,
   finish constructing the subprogram declaration from ASM_NAME and TYPE.  */
extern void finish_subprog_decl (tree decl, tree asm_name, tree type);

/* Process the attributes in ATTR_LIST for NODE, which is either a DECL or
   a TYPE.  If IN_PLACE is true, the tree pointed to by NODE should not be
   changed.  GNAT_NODE is used for the position of error messages.  */
extern void process_attributes (tree *node, struct attrib **attr_list,
				bool in_place, Node_Id gnat_node);

/* Set up the framework for generating code for SUBPROG_DECL, a subprogram
   body. This routine needs to be invoked before processing the declarations
   appearing in the subprogram.  */
extern void begin_subprog_body (tree subprog_decl);

/* Finish translating the current subprogram and set its BODY.  */
extern void end_subprog_body (tree body);

/* Wrap up compilation of SUBPROG_DECL, a subprogram body.  */
extern void rest_of_subprog_body_compilation (tree subprog_decl);

/* Build a template of type TEMPLATE_TYPE from the array bounds of ARRAY_TYPE.
   EXPR is an expression that we can use to locate any PLACEHOLDER_EXPRs.
   Return a constructor for the template.  */
extern tree build_template (tree template_type, tree array_type, tree expr);

/* Build a type to be used to represent an aliased object whose nominal type
   is an unconstrained array.  This consists of a RECORD_TYPE containing a
   field of TEMPLATE_TYPE and a field of OBJECT_TYPE, which is an ARRAY_TYPE.
   If ARRAY_TYPE is that of an unconstrained array, this is used to represent
   an arbitrary unconstrained object.  Use NAME as the name of the record.
   DEBUG_INFO_P is true if we need to write debug information for the type.  */
extern tree build_unc_object_type (tree template_type, tree object_type,
				   tree name, bool debug_info_p);

/* Same as build_unc_object_type, but taking a thin or fat pointer type
   instead of the template type.  */
extern tree build_unc_object_type_from_ptr (tree thin_fat_ptr_type,
					    tree object_type, tree name,
					    bool debug_info_p);

/* Update anything previously pointing to OLD_TYPE to point to NEW_TYPE.  In
   the normal case this is just two adjustments, but we have more to do
   if NEW is an UNCONSTRAINED_ARRAY_TYPE.  */
extern void update_pointer_to (tree old_type, tree new_type);

/* EXP is an expression for the size of an object.  If this size contains
   discriminant references, replace them with the maximum (if MAX_P) or
   minimum (if !MAX_P) possible value of the discriminant.  */
extern tree max_size (tree exp, bool max_p);

/* Remove all conversions that are done in EXP.  This includes converting
   from a padded type or to a left-justified modular type.  If TRUE_ADDRESS
   is true, always return the address of the containing object even if
   the address is not bit-aligned.  */
extern tree remove_conversions (tree exp, bool true_address);

/* If EXP's type is an UNCONSTRAINED_ARRAY_TYPE, return an expression that
   refers to the underlying array.  If its type has TYPE_CONTAINS_TEMPLATE_P,
   likewise return an expression pointing to the underlying array.  */
extern tree maybe_unconstrained_array (tree exp);

/* Return an expression that does an unchecked conversion of EXPR to TYPE.
   If NOTRUNC_P is true, truncation operations should be suppressed.  */
extern tree unchecked_convert (tree type, tree expr, bool notrunc_p);

/* Return the appropriate GCC tree code for the specified GNAT_TYPE,
   the latter being a record type as predicated by Is_Record_Type.  */
extern enum tree_code tree_code_for_record_type (Entity_Id gnat_type);

/* Return true if GNAT_TYPE is a "double" floating-point type, i.e. whose
   size is equal to 64 bits, or an array of such a type.  Set ALIGN_CLAUSE
   according to the presence of an alignment clause on the type or, if it
   is an array, on the component type.  */
extern bool is_double_float_or_array (Entity_Id gnat_type,
				      bool *align_clause);

/* Return true if GNAT_TYPE is a "double" or larger scalar type, i.e. whose
   size is greater or equal to 64 bits, or an array of such a type.  Set
   ALIGN_CLAUSE according to the presence of an alignment clause on the
   type or, if it is an array, on the component type.  */
extern bool is_double_scalar_or_array (Entity_Id gnat_type,
				       bool *align_clause);

/* Return true if GNU_TYPE is suitable as the type of a non-aliased
   component of an aggregate type.  */
extern bool type_for_nonaliased_component_p (tree gnu_type);

/* Return true if TYPE is a smaller form of ORIG_TYPE.  */
extern bool smaller_form_type_p (tree type, tree orig_type);

/* Return the base type of TYPE.  */
extern tree get_base_type (tree type);

/* EXP is a GCC tree representing an address.  See if we can find how
   strictly the object at that address is aligned.   Return that alignment
   strictly the object at that address is aligned.   Return that alignment
   in bits.  If we don't know anything about the alignment, return 0.  */
extern unsigned int known_alignment (tree exp);

/* Return true if VALUE is a multiple of FACTOR. FACTOR must be a power
   of 2.  */
extern bool value_factor_p (tree value, HOST_WIDE_INT factor);

/* Build an atomic load for the underlying atomic object in SRC.  SYNC is
   true if the load requires synchronization.  */
extern tree build_atomic_load (tree src, bool sync);

/* Build an atomic store from SRC to the underlying atomic object in DEST.
   SYNC is true if the store requires synchronization.  */
extern tree build_atomic_store (tree dest, tree src, bool sync);

/* Build a load-modify-store sequence from SRC to DEST.  GNAT_NODE is used for
   the location of the sequence.  Note that, even if the load and the store are
   both atomic, the sequence itself is not atomic.  */
extern tree build_load_modify_store (tree dest, tree src, Node_Id gnat_node);

/* Make a binary operation of kind OP_CODE.  RESULT_TYPE is the type
   desired for the result.  Usually the operation is to be performed
   in that type.  For MODIFY_EXPR and ARRAY_REF, RESULT_TYPE may be 0
   in which case the type to be used will be derived from the operands.
   Don't fold the result if NO_FOLD is true.  */
extern tree build_binary_op (enum tree_code op_code, tree result_type,
			     tree left_operand, tree right_operand,
			     bool no_fold=false);

/* Similar, but make unary operation.  */
extern tree build_unary_op (enum tree_code op_code, tree result_type,
                            tree operand);

/* Similar, but for COND_EXPR.  */
extern tree build_cond_expr (tree result_type, tree condition_operand,
                             tree true_operand, tree false_operand);

/* Similar, but for COMPOUND_EXPR.  */
extern tree build_compound_expr (tree result_type, tree stmt_operand,
				 tree expr_operand);

/* Conveniently construct a function call expression.  FNDECL names the
   function to be called, N is the number of arguments, and the "..."
   parameters are the argument expressions.  Unlike build_call_expr
   this doesn't fold the call, hence it will always return a CALL_EXPR.  */
extern tree build_call_n_expr (tree fndecl, int n, ...);

/* Build a call to a function that raises an exception and passes file name
   and line number, if requested.  MSG says which exception function to call.
   GNAT_NODE is the node conveying the source location for which the error
   should be signaled, or Empty in which case the error is signaled for the
   current location.  KIND says which kind of exception node this is for,
   among N_Raise_{Constraint,Storage,Program}_Error.  */
extern tree build_call_raise (int msg, Node_Id gnat_node, char kind);

/* Similar to build_call_raise, with extra information about the column
   where the check failed.  */
extern tree build_call_raise_column (int msg, Node_Id gnat_node, char kind);

/* Similar to build_call_raise_column, for an index or range check exception ,
   with extra information of the form "INDEX out of range FIRST..LAST".  */
extern tree build_call_raise_range (int msg, Node_Id gnat_node, char kind,
				    tree index, tree first, tree last);

/* Return a CONSTRUCTOR of TYPE whose elements are V.  This is not the
   same as build_constructor in the language-independent tree.c.  */
extern tree gnat_build_constructor (tree type, vec<constructor_elt, va_gc> *v);

/* Return a COMPONENT_REF to access FIELD in RECORD, or NULL_EXPR and generate
   a Constraint_Error if the field is not found in the record.  Don't fold the
   result if NO_FOLD is true.  */
extern tree build_component_ref (tree record, tree field, bool no_fold);

/* Build a GCC tree to call an allocation or deallocation function.
   If GNU_OBJ is nonzero, it is an object to deallocate.  Otherwise,
   generate an allocator.

   GNU_SIZE is the number of bytes to allocate and GNU_TYPE is the contained
   object type, used to determine the to-be-honored address alignment.
   GNAT_PROC, if present, is a procedure to call and GNAT_POOL is the storage
   pool to use.  If not present, malloc and free are used.  GNAT_NODE is used
   to provide an error location for restriction violation messages.  */
extern tree build_call_alloc_dealloc (tree gnu_obj, tree gnu_size,
                                      tree gnu_type, Entity_Id gnat_proc,
				      Entity_Id gnat_pool, Node_Id gnat_node);

/* Build a GCC tree to correspond to allocating an object of TYPE whose
   initial value if INIT, if INIT is nonzero.  Convert the expression to
   RESULT_TYPE, which must be some type of pointer.  Return the tree.

   GNAT_PROC and GNAT_POOL optionally give the procedure to call and
   the storage pool to use.  GNAT_NODE is used to provide an error
   location for restriction violation messages.  If IGNORE_INIT_TYPE is
   true, ignore the type of INIT for the purpose of determining the size;
   this will cause the maximum size to be allocated if TYPE is of
   self-referential size.  */
extern tree build_allocator (tree type, tree init, tree result_type,
                             Entity_Id gnat_proc, Entity_Id gnat_pool,
                             Node_Id gnat_node, bool);

/* Indicate that we need to take the address of T and that it therefore
   should not be allocated in a register.  Returns true if successful.  */
extern bool gnat_mark_addressable (tree t);

/* Save EXP for later use or reuse.  This is equivalent to save_expr in tree.c
   but we know how to handle our own nodes.  */
extern tree gnat_save_expr (tree exp);

/* Protect EXP for immediate reuse.  This is a variant of gnat_save_expr that
   is optimized under the assumption that EXP's value doesn't change before
   its subsequent reuse(s) except through its potential reevaluation.  */
extern tree gnat_protect_expr (tree exp);

/* This is equivalent to stabilize_reference in tree.c but we know how to
   handle our own nodes and we take extra arguments.  FORCE says whether to
   force evaluation of everything in REF.  INIT is set to the first arm of
   a COMPOUND_EXPR present in REF, if any.  */
extern tree gnat_stabilize_reference (tree ref, bool force, tree *init);

/* Rewrite reference REF and call FUNC on each expression within REF in the
   process.  DATA is passed unmodified to FUNC.  INIT is set to the first
   arm of a COMPOUND_EXPR present in REF, if any.  */
typedef tree (*rewrite_fn) (tree, void *);
extern tree gnat_rewrite_reference (tree ref, rewrite_fn func, void *data,
				    tree *init);

/* This is equivalent to get_inner_reference in expr.c but it returns the
   ultimate containing object only if the reference (lvalue) is constant,
   i.e. if it doesn't depend on the context in which it is evaluated.  */
extern tree get_inner_constant_reference (tree exp);

/* Return true if EXPR is the addition or the subtraction of a constant and,
   if so, set *ADD to the addend, *CST to the constant and *MINUS_P to true
   if this is a subtraction.  */
extern bool is_simple_additive_expression (tree expr, tree *add, tree *cst,
					   bool *minus_p);

/* If EXPR is an expression that is invariant in the current function, in the
   sense that it can be evaluated anywhere in the function and any number of
   times, return EXPR or an equivalent expression.  Otherwise return NULL.  */
extern tree gnat_invariant_expr (tree expr);

/* Implementation of the builtin_function langhook.  */
extern tree gnat_builtin_function (tree decl);

/* Search the chain of currently reachable declarations for a builtin
   FUNCTION_DECL node corresponding to function NAME (an IDENTIFIER_NODE).
   Return the first node found, if any, or NULL_TREE otherwise.  */
extern tree builtin_decl_for (tree name);

/* GNU_TYPE is a type. Determine if it should be passed by reference by
   default.  */
extern bool default_pass_by_ref (tree gnu_type);

/* GNU_TYPE is the type of a subprogram parameter.  Determine from the type
   if it should be passed by reference.  */
extern bool must_pass_by_ref (tree gnu_type);

/* Return the size of the FP mode with precision PREC.  */
extern int fp_prec_to_size (int prec);

/* Return the precision of the FP mode with size SIZE.  */
extern int fp_size_to_prec (int size);

/* Return whether GNAT_NODE is a defining identifier for a renaming that comes
   from the parameter association for the instantiation of a generic.  We do
   not want to emit source location for them: the code generated for their
   initialization is likely to disturb debugging.  */
extern bool renaming_from_instantiation_p (Node_Id gnat_node);

/* Try to process all nodes in the deferred context queue.  Keep in the queue
   the ones that cannot be processed yet, remove the other ones.  If FORCE is
   true, force the processing for all nodes, use the global context when nodes
   don't have a GNU translation.  */
extern void process_deferred_decl_context (bool force);

/* Return the innermost scope, starting at GNAT_NODE, we are be interested in
   the debug info, or Empty if there is no such scope.  If not NULL, set
   IS_SUBPROGRAM to whether the returned entity is a subprogram.  */
extern Entity_Id get_debug_scope (Node_Id gnat_node, bool *is_subprogram);

/* Return whether EXPR, which is the renamed object in an object renaming
   declaration, can be materialized as a reference (REFERENCE_TYPE).  This
   should be synchronized with Exp_Dbug.Debug_Renaming_Declaration.  */
extern bool can_materialize_object_renaming_p (Node_Id expr);

#ifdef __cplusplus
extern "C" {
#endif

/* These functions return the basic data type sizes and related parameters
   about the target machine.  */
extern Pos get_target_bits_per_unit (void);
extern Pos get_target_bits_per_word (void);
extern Pos get_target_char_size (void);
extern Pos get_target_wchar_t_size (void);
extern Pos get_target_short_size (void);
extern Pos get_target_int_size (void);
extern Pos get_target_long_size (void);
extern Pos get_target_long_long_size (void);
extern Pos get_target_pointer_size (void);
extern Pos get_target_maximum_default_alignment (void);
extern Pos get_target_system_allocator_alignment (void);
extern Pos get_target_maximum_allowed_alignment (void);
extern Pos get_target_maximum_alignment (void);
extern Nat get_target_float_words_be (void);
extern Nat get_target_words_be (void);
extern Nat get_target_bytes_be (void);
extern Nat get_target_bits_be (void);
extern Nat get_target_strict_alignment (void);
extern Nat get_target_double_float_alignment (void);
extern Nat get_target_double_scalar_alignment (void);

/* This function is called by the front-end to enumerate all the supported
   modes for the machine, as well as some predefined C types.  */
extern void enumerate_modes (void (*f) (const char *, int, int, int, int, int,
					int, int));

#ifdef __cplusplus
}
#endif

/* If EXP's type is a VECTOR_TYPE, return EXP converted to the associated
   TYPE_REPRESENTATIVE_ARRAY.  */

static inline tree
maybe_vector_array (tree exp)
{
  tree etype = TREE_TYPE (exp);

  if (VECTOR_TYPE_P (etype))
    exp = convert (TYPE_REPRESENTATIVE_ARRAY (etype), exp);

  return exp;
}

/* Return the smallest power of 2 larger than X.  */

static inline unsigned HOST_WIDE_INT
ceil_pow2 (unsigned HOST_WIDE_INT x)
{
  return (unsigned HOST_WIDE_INT) 1 << ceil_log2 (x);
}

/* Return true if EXP, a CALL_EXPR, is an atomic load.  */

static inline bool
call_is_atomic_load (tree exp)
{
  tree fndecl = get_callee_fndecl (exp);

  if (!(fndecl && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL))
    return false;

  enum built_in_function code = DECL_FUNCTION_CODE (fndecl);
  return BUILT_IN_ATOMIC_LOAD_N <= code && code <= BUILT_IN_ATOMIC_LOAD_16;
}

/* Return true if TYPE is padding a self-referential type.  */

static inline bool
type_is_padding_self_referential (tree type)
{
  if (!TYPE_IS_PADDING_P (type))
    return false;

  return CONTAINS_PLACEHOLDER_P (DECL_SIZE (TYPE_FIELDS (type)));
}

/* Return true if a function returning TYPE doesn't return a fixed size.  */

static inline bool
return_type_with_variable_size_p (tree type)
{
  if (TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
    return true;

  /* Return true for an unconstrained type with default discriminant, see
     the E_Subprogram_Type case of gnat_to_gnu_entity.  */
  if (type_is_padding_self_referential (type))
    return true;

  return false;
}

/* Return the unsigned version of TYPE_NODE, a scalar type.  */

static inline tree
gnat_unsigned_type_for (tree type_node)
{
  return gnat_signed_or_unsigned_type_for (1, type_node);
}

/* Return the signed version of TYPE_NODE, a scalar type.  */

static inline tree
gnat_signed_type_for (tree type_node)
{
  return gnat_signed_or_unsigned_type_for (0, type_node);
}

/* Adjust the character type TYPE if need be.  */

static inline tree
maybe_character_type (tree type)
{
  if (TYPE_STRING_FLAG (type) && !TYPE_UNSIGNED (type))
    type = gnat_unsigned_type_for (type);

  return type;
}

/* Adjust the character value EXPR if need be.  */

static inline tree
maybe_character_value (tree expr)
{
  tree type = TREE_TYPE (expr);

  if (TYPE_STRING_FLAG (type) && !TYPE_UNSIGNED (type))
    {
      type = gnat_unsigned_type_for (type);
      expr = convert (type, expr);
    }

  return expr;
}

/* Return the debug type of TYPE if it exists, otherwise TYPE itself.  */

static inline tree
maybe_debug_type (tree type)
{
  if (TYPE_CAN_HAVE_DEBUG_TYPE_P (type) && TYPE_DEBUG_TYPE (type))
    type = TYPE_DEBUG_TYPE (type);

  return type;
}

/* Like build_qualified_type, but TYPE_QUALS is added to the existing
   qualifiers on TYPE.  */

static inline tree
change_qualified_type (tree type, int type_quals)
{
  /* Qualifiers must be put on the associated array type.  */
  if (TREE_CODE (type) == UNCONSTRAINED_ARRAY_TYPE)
    return type;

  return build_qualified_type (type, TYPE_QUALS (type) | type_quals);
}
