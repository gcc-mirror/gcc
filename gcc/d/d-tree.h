/* d-tree.h -- Definitions and declarations for code generation.
   Copyright (C) 2006-2025 Free Software Foundation, Inc.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_D_TREE_H
#define GCC_D_TREE_H

/* Forward type declarations to avoid including unnecessary headers.  */

class Dsymbol;
class Declaration;
class AggregateDeclaration;
class ClassDeclaration;
class EnumDeclaration;
class FuncDeclaration;
class StructDeclaration;
class TypeInfoDeclaration;
class VarDeclaration;
class Expression;
class ClassReferenceExp;
class IndexExp;
class SliceExp;
class Module;
class Statement;
class Type;
class TypeFunction;
class Parameter;
struct BaseClass;
struct Scope;
struct Loc;

template <typename TYPE> struct Array;
typedef Array <Expression *> Expressions;

/* Usage of TREE_LANG_FLAG_?:
   0: METHOD_CALL_EXPR
   1: CALL_EXPR_WARN_IF_UNUSED (in CALL_EXPR).

   Usage of TYPE_LANG_FLAG_?:
   0: TYPE_SHARED
   1: TYPE_IMAGINARY_FLOAT (in REAL_TYPE).
      ANON_AGGR_TYPE_P (in RECORD_TYPE, UNION_TYPE).
   2: CLASS_TYPE_P (in RECORD_TYPE).
   3: TYPE_DYNAMIC_ARRAY (in RECORD_TYPE).
   4: TYPE_DELEGATE (in RECORD_TYPE).
   5: TYPE_ASSOCIATIVE_ARRAY (in RECORD_TYPE).

   Usage of TYPE_LANG_SLOT_?:
   1: TYPE_FORWARD_REFERENCES (in RECORD_TYPE, UNION_TYPE).

   Usage of DECL_LANG_FLAG_?:
   0: LABEL_VARIABLE_CASE (in LABEL_DECL).
      DECL_BUILT_IN_CTFE (in FUNCTION_DECL).
   1: DECL_IN_UNITTEST_CONDITION_P (in FUNCTION_DECL).
   2: DECL_INSTANTIATED (in FUNCTION_DECL, VAR_DECL).  */

/* Language-specific tree checkers.  */

#define VAR_OR_FUNCTION_DECL_CHECK(NODE) \
  TREE_CHECK2 (NODE, VAR_DECL, FUNCTION_DECL)

#define AGGREGATE_OR_ENUM_TYPE_CHECK(NODE) \
  TREE_CHECK4 (NODE, RECORD_TYPE, UNION_TYPE, \
	       QUAL_UNION_TYPE, ENUMERAL_TYPE)

/* The kinds of scopes we recognize.  */

enum level_kind
{
  level_block,		/* An ordinary block scope.  */
  level_try,		/* A try-block.  */
  level_catch,		/* A catch-block.  */
  level_finally,	/* A finally-block.  */
  level_cond,		/* The scope for an if condition.  */
  level_switch,		/* The scope for a switch statement.  */
  level_loop,		/* A for, do-while, or unrolled-loop block.  */
  level_with,		/* The scope for a with statement.  */
  level_function	/* The block representing an entire function.  */
};

/* List of codes for internally recognised compiler intrinsics.  */

enum intrinsic_code
{
#define DEF_D_INTRINSIC(CODE, B, N, M, D, C, F) CODE,

#include "intrinsics.def"

#undef DEF_D_INTRINSIC
  INTRINSIC_LAST
};

/* For use with break and continue statements.  */

enum bc_kind
{
  bc_break    = 0,
  bc_continue = 1
};

/* The datatype used to implement D scope.  It is needed primarily to support
   the back-end, but also helps debugging information for local variables.  */

struct GTY((chain_next ("%h.level_chain"))) binding_level
{
  /* A chain of declarations for all variables, constants and functions.
     These are in the reverse of the order supplied.  */
  tree names;

  /* For each level (except the global one), a chain of BLOCK nodes for
     all the levels that were entered and exited one level down.  */
  tree blocks;

  /* The binding level this one is contained in.  */
  binding_level *level_chain;

  /* The kind of scope this object represents.  */
  ENUM_BITFIELD (level_kind) kind : 4;
};

/* The binding level currently in effect.  */
extern GTY(()) binding_level *current_binding_level;
extern GTY(()) binding_level *global_binding_level;

/* Used only for jumps to as-yet undefined labels, since jumps to
   defined labels can have their validity checked immediately.  */

struct GTY((chain_next ("%h.next"))) d_label_use_entry
{
  d_label_use_entry *next;

  /* The frontend Statement associated with the jump.  */
  Statement * GTY((skip)) statement;

  /* The binding level to which this entry is *currently* attached.
     This is initially the binding level in which the goto appeared,
     but is modified as scopes are closed.  */
  binding_level *level;
};

/* A list of all LABEL_DECLs in the function that have names.  Here so
   we can clear out their names' definitions at the end of the
   function, and so we can check the validity of jumps to these labels.  */

struct GTY(()) d_label_entry
{
  /* The label decl itself.  */
  tree label;

  /* The frontend Statement associated with the label.  */
  Statement * GTY((skip)) statement;

  /* The binding level to which the label is *currently* attached.
     This is initially set to the binding level in which the label
     is defined, but is modified as scopes are closed.  */
  binding_level *level;

  /* A list of forward references of the label.  */
  d_label_use_entry *fwdrefs;

  /* The following bits are set after the label is defined, and are
     updated as scopes are popped.  They indicate that a backward jump
     to the label will illegally enter a scope of the given flavor.  */
  bool in_try_scope;
  bool in_catch_scope;

  /* If set, the label we reference represents a break/continue pair.  */
  bool bc_label;
};

/* Frame information for a function declaration.  */

struct GTY(()) tree_frame_info
{
  struct tree_common common;
  tree frame_type;
};

/* True if the function creates a nested frame.  */
#define FRAMEINFO_CREATES_FRAME(NODE) \
  (TREE_LANG_FLAG_0 (FUNCFRAME_INFO_CHECK (NODE)))

/* True if the function has a static chain passed in its DECL_ARGUMENTS.  */
#define FRAMEINFO_STATIC_CHAIN(NODE) \
  (TREE_LANG_FLAG_1 (FUNCFRAME_INFO_CHECK (NODE)))

/* True if the function frame is a closure (initialized on the heap).  */
#define FRAMEINFO_IS_CLOSURE(NODE) \
  (TREE_LANG_FLAG_2 (FUNCFRAME_INFO_CHECK (NODE)))

#define FRAMEINFO_TYPE(NODE) \
  (((tree_frame_info *) FUNCFRAME_INFO_CHECK (NODE))->frame_type)

/* Language-dependent contents of an identifier.  */

struct GTY(()) lang_identifier
{
  struct tree_identifier common;

  /* The identifier as the user sees it.  */
  tree pretty_ident;

  /* The back-end tree associated with this identifier.  */
  tree decl_tree;

  /* The frontend Declaration associated with this identifier.  */
  Declaration * GTY((skip)) dsymbol;
  AggregateDeclaration * GTY((skip)) daggregate;
};

#define IDENTIFIER_LANG_SPECIFIC(NODE) \
  ((struct lang_identifier *) IDENTIFIER_NODE_CHECK (NODE))

#define IDENTIFIER_PRETTY_NAME(NODE) \
  (IDENTIFIER_LANG_SPECIFIC (NODE)->pretty_ident)

#define IDENTIFIER_DECL_TREE(NODE) \
  (IDENTIFIER_LANG_SPECIFIC (NODE)->decl_tree)

#define IDENTIFIER_DSYMBOL(NODE) \
  (IDENTIFIER_LANG_SPECIFIC (NODE)->dsymbol)

#define IDENTIFIER_DAGGREGATE(NODE) \
  (IDENTIFIER_LANG_SPECIFIC (NODE)->daggregate)

/* Global state pertinent to the current function.  */

struct GTY(()) language_function
{
  /* Our function and enclosing module.  */
  FuncDeclaration * GTY((skip)) function;
  Module * GTY((skip)) module;

  /* Static chain of function, for D2, this is a closure.  */
  tree static_chain;

  /* Stack of statement lists being collected while we are
     compiling the function.  */
  vec <tree, va_gc> *stmt_list;

  /* Variables that are in scope that will need destruction later.  */
  vec <tree, va_gc> *vars_in_scope;

  /* Table of all used or defined labels in the function.  */
  hash_map <Statement *, d_label_entry> *labels;
};

/* The D front end types have not been integrated into the GCC garbage
   collection system.  Handle this by using the "skip" attribute.  */

struct GTY(()) lang_decl
{
  Declaration * GTY((skip)) decl;

  /* FIELD_DECL in frame struct that this variable is allocated in.  */
  tree frame_field;

  /* RESULT_DECL in a function that returns by nrvo.  */
  tree named_result;

  /* Chain of DECL_LANG_THUNKS in a function.  */
  tree thunks;

  /* In a FUNCTION_DECL, this is the THUNK_LANG_OFFSET.  */
  int offset;

  /* In a FUNCTION_DECL, if this is an intrinsic, the code for it.  */
  enum intrinsic_code intrinsic;

  /* FUNCFRAME_INFO in a function that has non-local references.  */
  tree frame_info;
};

/* The current D per-function global variables.  */

#define d_function_chain (cfun ? cfun->language : NULL)

/* The D frontend Declaration AST for GCC decl NODE.  */
#define DECL_LANG_FRONTEND(NODE) \
  (DECL_LANG_SPECIFIC (NODE) \
   ? DECL_LANG_SPECIFIC (NODE)->decl : NULL)

#define SET_DECL_LANG_FRAME_FIELD(NODE, VAL) \
  DECL_LANG_SPECIFIC (NODE)->frame_field = VAL

#define DECL_LANG_FRAME_FIELD(NODE) \
  (DECL_P (NODE) \
   ? DECL_LANG_SPECIFIC (NODE)->frame_field : NULL)

#define SET_DECL_LANG_NRVO(NODE, VAL) \
  DECL_LANG_SPECIFIC (NODE)->named_result = VAL

#define DECL_LANG_NRVO(NODE) \
  (DECL_P (NODE) \
   ? DECL_LANG_SPECIFIC (NODE)->named_result : NULL)

#define DECL_LANG_THUNKS(NODE) \
  DECL_LANG_SPECIFIC (NODE)->thunks

#define THUNK_LANG_OFFSET(NODE) \
  DECL_LANG_SPECIFIC (NODE)->offset

#define DECL_INTRINSIC_CODE(NODE) \
  DECL_LANG_SPECIFIC (NODE)->intrinsic

#define DECL_LANG_FRAMEINFO(NODE) \
  DECL_LANG_SPECIFIC (NODE)->frame_info

/* The lang_type field is not set for every GCC type.  */

struct GTY(()) lang_type
{
  Type * GTY((skip)) type;
};

/* The D frontend Type AST for GCC type NODE.  */
#define TYPE_LANG_FRONTEND(NODE) \
  (TYPE_LANG_SPECIFIC (NODE) \
   ? TYPE_LANG_SPECIFIC (NODE)->type : NULL)


enum d_tree_node_structure_enum
{
  TS_D_GENERIC,
  TS_D_IDENTIFIER,
  TS_D_FRAMEINFO,
  LAST_TS_D_ENUM
};

/* The resulting tree type.  */

union GTY((desc ("d_tree_node_structure (&%h)"),
	   chain_next ("CODE_CONTAINS_STRUCT (TREE_CODE (&%h.generic), TS_COMMON)"
		       " ? ((union lang_tree_node *) TREE_CHAIN (&%h.generic)) : NULL")))
lang_tree_node
{
  union tree_node GTY ((tag ("TS_D_GENERIC"),
			desc ("tree_node_structure (&%h)"))) generic;
  lang_identifier GTY ((tag ("TS_D_IDENTIFIER"))) identifier;
  tree_frame_info GTY ((tag ("TS_D_FRAMEINFO"))) frameinfo;
};

/* True if the Tdelegate typed expression is not really a variable,
   but a literal function / method reference.  */
#define METHOD_CALL_EXPR(NODE) \
  (TREE_LANG_FLAG_0 (NODE))

/* True if the CALL_EXPR is free of side effects, and its return value
   should not be discarded.  */
#define CALL_EXPR_WARN_IF_UNUSED(NODE) \
  (TREE_LANG_FLAG_1 (CALL_EXPR_CHECK (NODE)))

/* True if the type was declared 'shared'.  */
#define TYPE_SHARED(NODE) \
  (TYPE_LANG_FLAG_0 (NODE))

/* True if the type is an imaginary float type.  */
#define TYPE_IMAGINARY_FLOAT(NODE) \
  (TYPE_LANG_FLAG_1 (REAL_TYPE_CHECK (NODE)))

/* True if the type is an anonymous record or union.  */
#define ANON_AGGR_TYPE_P(NODE) \
  (TYPE_LANG_FLAG_1 (RECORD_OR_UNION_CHECK (NODE)))

/* True if the type is the underlying record for a class.  */
#define CLASS_TYPE_P(NODE) \
  (TYPE_LANG_FLAG_2 (RECORD_TYPE_CHECK (NODE)))

/* True if the type is a D dynamic array.  */
#define TYPE_DYNAMIC_ARRAY(NODE) \
  (TYPE_LANG_FLAG_3 (RECORD_TYPE_CHECK (NODE)))

/* True if the type is a D delegate.  */
#define TYPE_DELEGATE(NODE) \
  (TYPE_LANG_FLAG_4 (RECORD_TYPE_CHECK (NODE)))

/* True if the type is a D associative array.  */
#define TYPE_ASSOCIATIVE_ARRAY(NODE) \
  (TYPE_LANG_FLAG_5 (RECORD_TYPE_CHECK (NODE)))

/* In an incomplete RECORD_TYPE, UNION_TYPE, or ENUMERAL_TYPE, a list of field
   declarations whose type would be completed by completing that type.  */
#define TYPE_FORWARD_REFERENCES(NODE) \
  (TYPE_LANG_SLOT_1 (AGGREGATE_OR_ENUM_TYPE_CHECK (NODE)))

/* True if the decl is a variable case label decl.  */
#define LABEL_VARIABLE_CASE(NODE) \
  (DECL_LANG_FLAG_0 (LABEL_DECL_CHECK (NODE)))

/* True if the decl is a CTFE built-in.  */
#define DECL_BUILT_IN_CTFE(NODE) \
  (DECL_LANG_FLAG_0 (FUNCTION_DECL_CHECK (NODE)))

/* True if the decl is only compiled in when unittests are turned on.  */
#define DECL_IN_UNITTEST_CONDITION_P(NODE) \
  (DECL_LANG_FLAG_1 (FUNCTION_DECL_CHECK (NODE)))

/* True if the decl comes from a template instance.  */
#define DECL_INSTANTIATED(NODE) \
  (DECL_LANG_FLAG_2 (VAR_OR_FUNCTION_DECL_CHECK (NODE)))

enum d_tree_index
{
  DTI_VTABLE_ENTRY_TYPE,
  DTI_VTBL_PTR_TYPE,
  DTI_VTBL_INTERFACE_TYPE,

  DTI_BOOL_TYPE,
  DTI_CHAR_TYPE,
  DTI_WCHAR_TYPE,
  DTI_DCHAR_TYPE,

  DTI_BYTE_TYPE,
  DTI_UBYTE_TYPE,
  DTI_SHORT_TYPE,
  DTI_USHORT_TYPE,
  DTI_INT_TYPE,
  DTI_UINT_TYPE,
  DTI_LONG_TYPE,
  DTI_ULONG_TYPE,
  DTI_CENT_TYPE,
  DTI_UCENT_TYPE,

  DTI_IFLOAT_TYPE,
  DTI_IDOUBLE_TYPE,
  DTI_IREAL_TYPE,

  DTI_UNKNOWN_TYPE,

  DTI_ARRAY_TYPE,
  DTI_NULL_ARRAY,
  DTI_BOTTOM_TYPE,

  DTI_BOOL_FALSE,
  DTI_BOOL_TRUE,

  DTI_MAX
};

extern GTY(()) tree d_global_trees[DTI_MAX];

#define vtable_entry_type		d_global_trees[DTI_VTABLE_ENTRY_TYPE]
#define vtbl_ptr_type_node		d_global_trees[DTI_VTBL_PTR_TYPE]
#define vtbl_interface_type_node	d_global_trees[DTI_VTBL_INTERFACE_TYPE]
/* D built-in language types.  */
#define d_bool_type			d_global_trees[DTI_BOOL_TYPE]
#define d_byte_type			d_global_trees[DTI_BYTE_TYPE]
#define d_ubyte_type			d_global_trees[DTI_UBYTE_TYPE]
#define d_short_type			d_global_trees[DTI_SHORT_TYPE]
#define d_ushort_type			d_global_trees[DTI_USHORT_TYPE]
#define d_int_type			d_global_trees[DTI_INT_TYPE]
#define d_uint_type			d_global_trees[DTI_UINT_TYPE]
#define d_long_type			d_global_trees[DTI_LONG_TYPE]
#define d_ulong_type			d_global_trees[DTI_ULONG_TYPE]
#define d_cent_type			d_global_trees[DTI_CENT_TYPE]
#define d_ucent_type			d_global_trees[DTI_UCENT_TYPE]
/* Imaginary floating-point types.  */
#define ifloat_type_node		d_global_trees[DTI_IFLOAT_TYPE]
#define idouble_type_node		d_global_trees[DTI_IDOUBLE_TYPE]
#define ireal_type_node			d_global_trees[DTI_IREAL_TYPE]
/* UTF-8, 16 and 32 types.  */
#define char8_type_node			d_global_trees[DTI_CHAR_TYPE]
#define char16_type_node		d_global_trees[DTI_DCHAR_TYPE]
#define char32_type_node		d_global_trees[DTI_WCHAR_TYPE]
/* Empty record type used as placeholder when real type is unknown.  */
#define unknown_type_node		d_global_trees[DTI_UNKNOWN_TYPE]
/* Generic dynamic array type void[].  */
#define array_type_node			d_global_trees[DTI_ARRAY_TYPE]
/* Null initializer for dynamic arrays.  */
#define null_array_node			d_global_trees[DTI_NULL_ARRAY]
/* The bottom type, referred to as `noreturn` in code.  */
#define noreturn_type_node		d_global_trees[DTI_BOTTOM_TYPE]
/* D boolean values are always byte-sized, unlike boolean_type_node.  */
#define d_bool_false_node		d_global_trees[DTI_BOOL_FALSE]
#define d_bool_true_node		d_global_trees[DTI_BOOL_TRUE]

/* A prefix for internal variables, which are not user-visible.  */
#if !defined (NO_DOT_IN_LABEL)
# define GDC_PREFIX(x) "gdc." x
#elif !defined (NO_DOLLAR_IN_LABEL)
# define GDC_PREFIX(x) "gdc$" x
#else
# define GDC_PREFIX(x) "gdc_" x
#endif

/* Internally recognised D runtime library functions.  */

enum libcall_fn
{
#define DEF_D_RUNTIME(CODE, N, T, P, F) LIBCALL_ ## CODE,

#include "runtime.def"

#undef DEF_D_RUNTIME
  LIBCALL_LAST
};

/* Gate for when the D frontend makes an early call into the codegen pass, such
   as when it requires target information or CTFE evaluation.  As full semantic
   may not be completed, we only want to build the superficial tree structure
   without finishing any decls or types.  */
extern bool doing_semantic_analysis_p;

/* In d-attribs.c.  */
extern tree insert_type_attribute (tree, const char *, tree = NULL_TREE);
extern tree insert_decl_attribute (tree, const char *, tree = NULL_TREE);
extern void apply_user_attributes (Dsymbol *, tree);

/* In d-builtins.cc.  */
extern const struct scoped_attribute_specs d_langhook_gnu_attribute_table;
extern const struct scoped_attribute_specs d_langhook_common_attribute_table;
extern Type *build_frontend_type (tree);

extern tree d_builtin_function (tree);
extern tree d_builtin_function_ext_scope (tree);
extern void d_init_builtins (void);
extern void d_register_builtin_type (tree, const char *);
extern void d_build_builtins_module (Module *);
extern void d_maybe_set_builtin (Module *);
extern Expression *d_eval_constant_expression (const Loc &, tree);
extern void d_init_versions (void);

/* In d-codegen.cc.  */
extern location_t make_location_t (const Loc &);
extern tree d_decl_context (Dsymbol *);
extern tree copy_aggregate_type (tree);
extern bool declaration_reference_p (Declaration *);
extern tree declaration_type (Declaration *);
extern bool parameter_reference_p (Parameter *);
extern tree parameter_type (Parameter *);
extern tree build_integer_cst (dinteger_t, tree = d_int_type);
extern tree build_float_cst (const real_t &, Type *);
extern tree d_array_length (tree);
extern tree d_array_ptr (tree);
extern tree d_array_value (tree, tree, tree);
extern tree get_array_length (tree, Type *);
extern tree build_class_binfo (tree, ClassDeclaration *);
extern tree build_interface_binfo (tree, ClassDeclaration *, unsigned &);
extern tree delegate_method (tree);
extern tree delegate_object (tree);
extern tree build_delegate_cst (tree, tree, Type *);
extern tree build_method_call (tree, tree, Type *);
extern void extract_from_method_call (tree, tree &, tree &);
extern tree build_typeof_null_value (Type *);
extern tree build_vindex_ref (tree, tree, size_t);
extern tree d_save_expr (tree);
extern tree stabilize_expr (tree *);
extern tree build_target_expr (tree, tree);
extern tree force_target_expr (tree);
extern tree build_address (tree);
extern tree d_mark_addressable (tree, bool = true);
extern tree d_mark_used (tree);
extern tree d_mark_read (tree);
extern tree build_memcmp_call (tree, tree, tree);
extern tree build_memcpy_call (tree, tree, tree);
extern tree build_memset_call (tree, tree = NULL_TREE);
extern bool identity_compare_p (StructDeclaration *);
extern tree build_float_identity (tree_code, tree, tree);
extern tree build_struct_comparison (tree_code, StructDeclaration *,
				     tree, tree);
extern tree build_array_struct_comparison (tree_code, StructDeclaration *,
					   tree, tree, tree);
extern tree build_struct_literal (tree, vec <constructor_elt, va_gc> *);
extern tree component_ref (tree, tree);
extern tree build_assign (tree_code, tree, tree);
extern tree modify_expr (tree, tree);
extern tree build_nop (tree, tree);
extern tree build_vconvert (tree, tree);
extern tree build_boolop (tree_code, tree, tree);
extern tree build_condition (tree, tree, tree, tree);
extern tree build_vcondition (tree, tree, tree);
extern tree compound_expr (tree, tree);
extern tree return_expr (tree);
extern tree size_mult_expr (tree, tree);
extern tree real_part (tree);
extern tree imaginary_part (tree);
extern tree complex_expr (tree, tree, tree);
extern tree underlying_complex_expr (tree, tree);
extern tree indirect_ref (tree, tree);
extern tree build_deref (tree);
extern tree build_pointer_index (tree, tree);
extern tree build_offset_op (tree_code, tree, tree);
extern tree build_offset (tree, tree);
extern tree build_memref (tree, tree, tree);
extern tree build_array_set (tree, tree, tree);
extern tree build_array_from_val (Type *, tree);
extern tree build_array_from_exprs (Type *, Expressions *, bool);
extern tree void_okay_p (tree);
extern tree build_assert_call (const Loc &, libcall_fn, tree = NULL_TREE);
extern tree build_array_bounds_call (const Loc &);
extern tree build_bounds_index_condition (IndexExp *, tree, tree);
extern tree build_bounds_slice_condition (SliceExp *, tree, tree, tree);
extern bool array_bounds_check (void);
extern bool checkaction_trap_p (void);
extern TypeFunction *get_function_type (Type *);
extern bool call_side_effect_free_p (FuncDeclaration *, Type *);
extern bool call_by_alias_p (FuncDeclaration *, FuncDeclaration *);
extern tree d_build_call_expr (FuncDeclaration *, tree, Expressions *);
extern tree d_build_call (TypeFunction *, tree, tree, Expressions *);
extern tree build_float_modulus (tree, tree, tree);
extern tree build_vthis_function (tree, tree);
extern tree error_no_frame_access (Dsymbol *);
extern tree get_frame_for_symbol (Dsymbol *);
extern tree build_vthis (AggregateDeclaration *);
extern void build_closure (FuncDeclaration *);
extern tree get_frameinfo (FuncDeclaration *);
extern tree get_framedecl (FuncDeclaration *, FuncDeclaration *);

/* In d-convert.cc.  */
extern bool decl_with_nonnull_addr_p (const_tree);
extern tree d_truthvalue_conversion (tree);
extern tree d_convert (tree, tree);
extern tree convert_expr (tree, Type *, Type *);
extern tree convert_for_rvalue (tree, Type *, Type *);
extern tree convert_for_assignment (Expression *, Type *, bool = false);
extern tree convert_for_argument (Expression *, Parameter *);
extern tree convert_for_condition (tree, Type *);
extern tree d_array_convert (Expression *);
extern tree d_array_convert (Type *, Expression *);

/* In d-gimplify.cc.  */
extern int d_gimplify_expr (tree *, gimple_seq *, gimple_seq *);

/* In d-incpath.cc.  */
extern void add_import_paths (const char *, const char *, bool);

/* In d-lang.cc.  */
extern void d_add_builtin_module (Module *);
extern d_tree_node_structure_enum d_tree_node_structure (lang_tree_node *);
extern struct lang_type *build_lang_type (Type *);
extern struct lang_decl *build_lang_decl (Declaration *);
extern tree d_pushdecl (tree);
extern void d_keep (tree);

/* In decl.cc.  */
extern const char *d_mangle_decl (Dsymbol *);
extern tree mangle_internal_decl (Dsymbol *, const char *, const char *);
extern void build_decl_tree (Dsymbol *);
extern tree get_symbol_decl (Declaration *);
extern tree declare_extern_var (tree, tree);
extern void declare_local_var (VarDeclaration *);
extern tree build_local_temp (tree);
extern tree get_decl_tree (Declaration *);
extern void d_finish_decl (tree);
extern tree make_thunk (FuncDeclaration *, int);
extern tree start_function (FuncDeclaration *);
extern void finish_function (tree);
extern tree get_vtable_decl (ClassDeclaration *);
extern tree build_new_class_expr (ClassReferenceExp *);
extern tree aggregate_initializer_decl (AggregateDeclaration *);
extern tree layout_struct_initializer (StructDeclaration *);
extern tree layout_class_initializer (ClassDeclaration *);
extern tree enum_initializer_decl (EnumDeclaration *);
extern tree build_artificial_decl (tree, tree, const char * = NULL);
extern tree create_field_decl (tree, const char *, int, int);
extern void build_type_decl (tree, Dsymbol *);
extern void set_linkage_for_decl (tree);
extern void set_visibility_for_decl (tree, Dsymbol *);

/* In expr.cc.  */
extern tree build_expr (Expression *, bool = false, bool = false);
extern tree build_expr_dtor (Expression *);
extern tree build_return_dtor (Expression *, Type *, TypeFunction *);

/* In imports.cc.  */
extern tree build_import_decl (Dsymbol *);

/* In intrinsics.cc.  */
extern void maybe_set_intrinsic (FuncDeclaration *);
extern tree maybe_expand_intrinsic (tree);
extern tree maybe_reject_intrinsic (tree);

/* In modules.cc.  */
extern void build_module_tree (Module *);
extern tree d_module_context (void);
extern void register_module_decl (Declaration *);
extern void d_defer_declaration (Declaration *);
extern void d_finish_compilation (tree *, int);

/* In runtime.cc.  */
extern tree build_libcall (libcall_fn, Type *, int ...);

/* In typeinfo.cc.  */
extern bool have_typeinfo_p (ClassDeclaration *);
extern tree layout_typeinfo (TypeInfoDeclaration *);
extern tree layout_classinfo (ClassDeclaration *);
extern unsigned base_vtable_offset (ClassDeclaration *, BaseClass *);
extern tree get_typeinfo_decl (TypeInfoDeclaration *);
extern tree get_classinfo_decl (ClassDeclaration *);
extern void check_typeinfo_type (const Loc &, Scope *, Expression * = NULL);
extern tree build_typeinfo (const Loc &, Type *, Expression * = NULL);
extern tree build_typeinfo (Expression *, Type *);
extern void create_typeinfo (Type *, Module *);
extern void create_tinfo_types (Module *);
extern void layout_cpp_typeinfo (ClassDeclaration *);
extern tree get_cpp_typeinfo_decl (ClassDeclaration *);
extern bool speculative_type_p (Type *);

/* In toir.cc.  */
extern void push_binding_level (level_kind);
extern tree pop_binding_level (void);
extern void push_stmt_list (void);
extern tree pop_stmt_list (void);
extern void add_stmt (tree);
extern void build_function_body (FuncDeclaration *);

/* In types.cc.  */
extern tree d_unsigned_type (tree);
extern tree d_signed_type (tree);
extern bool valist_array_p (Type *);
extern bool empty_aggregate_p (tree);
extern bool same_type_p (Type *, Type *);
extern Type *get_object_type (void);
extern tree make_array_type (Type *, unsigned HOST_WIDE_INT);
extern tree make_struct_type (const char *, int n, ...);
extern tree insert_type_modifiers (tree, unsigned);
extern void insert_aggregate_field (tree, tree, size_t);
extern void finish_aggregate_type (unsigned, unsigned, tree);
extern tree build_ctype (Type *);

#endif  /* GCC_D_TREE_H  */
