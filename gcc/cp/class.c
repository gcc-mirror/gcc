/* Functions related to building classes and their related objects.
   Copyright (C) 1987, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002  Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com)

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


/* High-level class interface.  */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "cp-tree.h"
#include "flags.h"
#include "rtl.h"
#include "output.h"
#include "toplev.h"
#include "ggc.h"
#include "lex.h"
#include "target.h"

/* The number of nested classes being processed.  If we are not in the
   scope of any class, this is zero.  */

int current_class_depth;

/* In order to deal with nested classes, we keep a stack of classes.
   The topmost entry is the innermost class, and is the entry at index
   CURRENT_CLASS_DEPTH  */

typedef struct class_stack_node {
  /* The name of the class.  */
  tree name;

  /* The _TYPE node for the class.  */
  tree type;

  /* The access specifier pending for new declarations in the scope of
     this class.  */
  tree access;

  /* If were defining TYPE, the names used in this class.  */
  splay_tree names_used;
}* class_stack_node_t;

typedef struct vtbl_init_data_s
{
  /* The base for which we're building initializers.  */
  tree binfo;
  /* The type of the most-derived type.  */
  tree derived;
  /* The binfo for the dynamic type. This will be TYPE_BINFO (derived),
     unless ctor_vtbl_p is true.  */
  tree rtti_binfo;
  /* The negative-index vtable initializers built up so far.  These
     are in order from least negative index to most negative index.  */
  tree inits;
  /* The last (i.e., most negative) entry in INITS.  */
  tree* last_init;
  /* The binfo for the virtual base for which we're building
     vcall offset initializers.  */
  tree vbase;
  /* The functions in vbase for which we have already provided vcall
     offsets.  */
  varray_type fns;
  /* The vtable index of the next vcall or vbase offset.  */
  tree index;
  /* Nonzero if we are building the initializer for the primary
     vtable.  */
  int primary_vtbl_p;
  /* Nonzero if we are building the initializer for a construction
     vtable.  */
  int ctor_vtbl_p;
  /* True when adding vcall offset entries to the vtable.  False when
     merely computing the indices.  */
  bool generate_vcall_entries;
} vtbl_init_data;

/* The type of a function passed to walk_subobject_offsets.  */
typedef int (*subobject_offset_fn) PARAMS ((tree, tree, splay_tree));

/* The stack itself.  This is an dynamically resized array.  The
   number of elements allocated is CURRENT_CLASS_STACK_SIZE.  */
static int current_class_stack_size;
static class_stack_node_t current_class_stack;

/* An array of all local classes present in this translation unit, in
   declaration order.  */
varray_type local_classes;

static tree get_vfield_name PARAMS ((tree));
static void finish_struct_anon PARAMS ((tree));
static tree get_vtable_name PARAMS ((tree));
static tree get_basefndecls PARAMS ((tree, tree));
static int build_primary_vtable PARAMS ((tree, tree));
static int build_secondary_vtable PARAMS ((tree, tree));
static void finish_vtbls PARAMS ((tree));
static void modify_vtable_entry PARAMS ((tree, tree, tree, tree, tree *));
static tree delete_duplicate_fields_1 PARAMS ((tree, tree));
static void delete_duplicate_fields PARAMS ((tree));
static void finish_struct_bits PARAMS ((tree));
static int alter_access PARAMS ((tree, tree, tree));
static void handle_using_decl PARAMS ((tree, tree));
static void check_for_override PARAMS ((tree, tree));
static tree dfs_modify_vtables PARAMS ((tree, void *));
static tree modify_all_vtables PARAMS ((tree, tree));
static void determine_primary_base PARAMS ((tree));
static void finish_struct_methods PARAMS ((tree));
static void maybe_warn_about_overly_private_class PARAMS ((tree));
static int field_decl_cmp PARAMS ((const tree *, const tree *));
static int method_name_cmp PARAMS ((const tree *, const tree *));
static void add_implicitly_declared_members PARAMS ((tree, int, int, int));
static tree fixed_type_or_null PARAMS ((tree, int *, int *));
static tree resolve_address_of_overloaded_function PARAMS ((tree, tree, 
							    tsubst_flags_t, 
							    int, int, tree));
static tree build_vtable_entry_ref PARAMS ((tree, tree, tree));
static tree build_vtbl_ref_1 PARAMS ((tree, tree));
static tree build_vtbl_initializer PARAMS ((tree, tree, tree, tree, int *));
static int count_fields PARAMS ((tree));
static int add_fields_to_vec PARAMS ((tree, tree, int));
static void check_bitfield_decl PARAMS ((tree));
static void check_field_decl (tree, tree, int *, int *, int *, int *);
static void check_field_decls (tree, tree *, int *, int *, int *);
static tree *build_base_field (record_layout_info, tree, splay_tree, tree *);
static void build_base_fields (record_layout_info, splay_tree, tree *);
static void check_methods PARAMS ((tree));
static void remove_zero_width_bit_fields PARAMS ((tree));
static void check_bases PARAMS ((tree, int *, int *, int *));
static void check_bases_and_members (tree);
static tree create_vtable_ptr (tree, tree *);
static void include_empty_classes (record_layout_info);
static void layout_class_type (tree, tree *);
static void fixup_pending_inline PARAMS ((tree));
static void fixup_inline_methods PARAMS ((tree));
static void set_primary_base PARAMS ((tree, tree));
static void propagate_binfo_offsets PARAMS ((tree, tree, tree));
static void layout_virtual_bases (record_layout_info, splay_tree);
static tree dfs_set_offset_for_unshared_vbases PARAMS ((tree, void *));
static void build_vbase_offset_vtbl_entries PARAMS ((tree, vtbl_init_data *));
static void add_vcall_offset_vtbl_entries_r PARAMS ((tree, vtbl_init_data *));
static void add_vcall_offset_vtbl_entries_1 PARAMS ((tree, vtbl_init_data *));
static void build_vcall_offset_vtbl_entries PARAMS ((tree, vtbl_init_data *));
static void add_vcall_offset (tree, tree, vtbl_init_data *);
static void layout_vtable_decl PARAMS ((tree, int));
static tree dfs_find_final_overrider PARAMS ((tree, void *));
static tree find_final_overrider PARAMS ((tree, tree, tree));
static int make_new_vtable PARAMS ((tree, tree));
static int maybe_indent_hierarchy PARAMS ((FILE *, int, int));
static void dump_class_hierarchy_r PARAMS ((FILE *, int, tree, tree, int));
static void dump_class_hierarchy PARAMS ((tree));
static void dump_array PARAMS ((FILE *, tree));
static void dump_vtable PARAMS ((tree, tree, tree));
static void dump_vtt PARAMS ((tree, tree));
static tree build_vtable PARAMS ((tree, tree, tree));
static void initialize_vtable PARAMS ((tree, tree));
static void initialize_array PARAMS ((tree, tree));
static void layout_nonempty_base_or_field PARAMS ((record_layout_info,
						   tree, tree, splay_tree));
static tree end_of_class PARAMS ((tree, int));
static bool layout_empty_base PARAMS ((tree, tree, splay_tree, tree));
static void accumulate_vtbl_inits PARAMS ((tree, tree, tree, tree, tree));
static tree dfs_accumulate_vtbl_inits PARAMS ((tree, tree, tree, tree,
					       tree));
static void build_rtti_vtbl_entries PARAMS ((tree, vtbl_init_data *));
static void build_vcall_and_vbase_vtbl_entries PARAMS ((tree, 
							vtbl_init_data *));
static void force_canonical_binfo_r PARAMS ((tree, tree, tree, tree));
static void force_canonical_binfo PARAMS ((tree, tree, tree, tree));
static tree dfs_unshared_virtual_bases PARAMS ((tree, void *));
static void mark_primary_bases PARAMS ((tree));
static tree mark_primary_virtual_base PARAMS ((tree, tree));
static void clone_constructors_and_destructors PARAMS ((tree));
static tree build_clone PARAMS ((tree, tree));
static void update_vtable_entry_for_fn PARAMS ((tree, tree, tree, tree *));
static tree copy_virtuals PARAMS ((tree));
static void build_ctor_vtbl_group PARAMS ((tree, tree));
static void build_vtt PARAMS ((tree));
static tree binfo_ctor_vtable PARAMS ((tree));
static tree *build_vtt_inits PARAMS ((tree, tree, tree *, tree *));
static tree dfs_build_secondary_vptr_vtt_inits PARAMS ((tree, void *));
static tree dfs_ctor_vtable_bases_queue_p PARAMS ((tree, void *data));
static tree dfs_fixup_binfo_vtbls PARAMS ((tree, void *));
static tree get_original_base PARAMS ((tree, tree));
static tree dfs_get_primary_binfo PARAMS ((tree, void*));
static int record_subobject_offset PARAMS ((tree, tree, splay_tree));
static int check_subobject_offset PARAMS ((tree, tree, splay_tree));
static int walk_subobject_offsets PARAMS ((tree, subobject_offset_fn,
					   tree, splay_tree, tree, int));
static void record_subobject_offsets PARAMS ((tree, tree, splay_tree, int));
static int layout_conflict_p PARAMS ((tree, tree, splay_tree, int));
static int splay_tree_compare_integer_csts PARAMS ((splay_tree_key k1,
						    splay_tree_key k2));
static void warn_about_ambiguous_bases PARAMS ((tree));
static bool type_requires_array_cookie PARAMS ((tree));
static bool contains_empty_class_p (tree);
static tree dfs_base_derived_from (tree, void *);
static bool base_derived_from (tree, tree);
static int empty_base_at_nonzero_offset_p (tree, tree, splay_tree);
static tree end_of_base (tree);
static tree get_vcall_index (tree, tree);

/* Macros for dfs walking during vtt construction. See
   dfs_ctor_vtable_bases_queue_p, dfs_build_secondary_vptr_vtt_inits
   and dfs_fixup_binfo_vtbls.  */
#define VTT_TOP_LEVEL_P(NODE) TREE_UNSIGNED (NODE)
#define VTT_MARKED_BINFO_P(NODE) TREE_USED (NODE)

/* Variables shared between class.c and call.c.  */

#ifdef GATHER_STATISTICS
int n_vtables = 0;
int n_vtable_entries = 0;
int n_vtable_searches = 0;
int n_vtable_elems = 0;
int n_convert_harshness = 0;
int n_compute_conversion_costs = 0;
int n_build_method_call = 0;
int n_inner_fields_searched = 0;
#endif

/* Convert to or from a base subobject.  EXPR is an expression of type
   `A' or `A*', an expression of type `B' or `B*' is returned.  To
   convert A to a base B, CODE is PLUS_EXPR and BINFO is the binfo for
   the B base instance within A.  To convert base A to derived B, CODE
   is MINUS_EXPR and BINFO is the binfo for the A instance within B.
   In this latter case, A must not be a morally virtual base of B.
   NONNULL is true if EXPR is known to be non-NULL (this is only
   needed when EXPR is of pointer type).  CV qualifiers are preserved
   from EXPR.  */

tree
build_base_path (code, expr, binfo, nonnull)
     enum tree_code code;
     tree expr;
     tree binfo;
     int nonnull;
{
  tree v_binfo = NULL_TREE;
  tree d_binfo = NULL_TREE;
  tree probe;
  tree offset;
  tree target_type;
  tree null_test = NULL;
  tree ptr_target_type;
  int fixed_type_p;
  int want_pointer = TREE_CODE (TREE_TYPE (expr)) == POINTER_TYPE;

  if (expr == error_mark_node || binfo == error_mark_node || !binfo)
    return error_mark_node;

  for (probe = binfo; probe; probe = BINFO_INHERITANCE_CHAIN (probe))
    {
      d_binfo = probe;
      if (!v_binfo && TREE_VIA_VIRTUAL (probe))
	v_binfo = probe;
    }

  probe = TYPE_MAIN_VARIANT (TREE_TYPE (expr));
  if (want_pointer)
    probe = TYPE_MAIN_VARIANT (TREE_TYPE (probe));
  
  my_friendly_assert (code == MINUS_EXPR
		      ? same_type_p (BINFO_TYPE (binfo), probe)
		      : code == PLUS_EXPR
		      ? same_type_p (BINFO_TYPE (d_binfo), probe)
		      : false, 20010723);
  
  if (code == MINUS_EXPR && v_binfo)
    {
      error ("cannot convert from base `%T' to derived type `%T' via virtual base `%T'",
	     BINFO_TYPE (binfo), BINFO_TYPE (d_binfo), BINFO_TYPE (v_binfo));
      return error_mark_node;
    }

  if (!want_pointer)
    /* This must happen before the call to save_expr.  */
    expr = build_unary_op (ADDR_EXPR, expr, 0);

  fixed_type_p = resolves_to_fixed_type_p (expr, &nonnull);
  if (fixed_type_p <= 0 && TREE_SIDE_EFFECTS (expr))
    expr = save_expr (expr);

  if (want_pointer && !nonnull)
    null_test = build (EQ_EXPR, boolean_type_node, expr, integer_zero_node);
  
  offset = BINFO_OFFSET (binfo);
  
  if (v_binfo && fixed_type_p <= 0)
    {
      /* Going via virtual base V_BINFO.  We need the static offset
         from V_BINFO to BINFO, and the dynamic offset from D_BINFO to
         V_BINFO.  That offset is an entry in D_BINFO's vtable.  */
      tree v_offset;

      if (fixed_type_p < 0 && in_base_initializer)
	{
	  /* In a base member initializer, we cannot rely on
	     the vtable being set up. We have to use the vtt_parm.  */
	  tree derived = v_binfo;

	  while (BINFO_INHERITANCE_CHAIN (derived))
	    derived = BINFO_INHERITANCE_CHAIN (derived);
	  
	  v_offset = build (PLUS_EXPR, TREE_TYPE (current_vtt_parm),
			    current_vtt_parm, BINFO_VPTR_INDEX (derived));
	  
	  v_offset = build1 (INDIRECT_REF,
			     TREE_TYPE (TYPE_VFIELD (BINFO_TYPE (derived))),
			     v_offset);
	}
      else
	v_offset = build_vfield_ref (build_indirect_ref (expr, NULL),
				     TREE_TYPE (TREE_TYPE (expr)));
      
      v_binfo = binfo_for_vbase (BINFO_TYPE (v_binfo), BINFO_TYPE (d_binfo));
      
      v_offset = build (PLUS_EXPR, TREE_TYPE (v_offset),
			v_offset,  BINFO_VPTR_FIELD (v_binfo));
      v_offset = build1 (NOP_EXPR, 
			 build_pointer_type (ptrdiff_type_node),
			 v_offset);
      v_offset = build_indirect_ref (v_offset, NULL);
      TREE_CONSTANT (v_offset) = 1;

      offset = cp_convert (ptrdiff_type_node,
			   size_diffop (offset, BINFO_OFFSET (v_binfo)));

      if (!integer_zerop (offset))
	v_offset = build (code, ptrdiff_type_node, v_offset, offset);

      if (fixed_type_p < 0)
	/* Negative fixed_type_p means this is a constructor or destructor;
	   virtual base layout is fixed in in-charge [cd]tors, but not in
	   base [cd]tors.  */
	offset = build (COND_EXPR, ptrdiff_type_node,
			build (EQ_EXPR, boolean_type_node,
			       current_in_charge_parm, integer_zero_node),
			v_offset,
			BINFO_OFFSET (binfo));
      else
	offset = v_offset;
    }

  target_type = code == PLUS_EXPR ? BINFO_TYPE (binfo) : BINFO_TYPE (d_binfo);
  
  target_type = cp_build_qualified_type
    (target_type, cp_type_quals (TREE_TYPE (TREE_TYPE (expr))));
  ptr_target_type = build_pointer_type (target_type);
  if (want_pointer)
    target_type = ptr_target_type;
  
  expr = build1 (NOP_EXPR, ptr_target_type, expr);

  if (!integer_zerop (offset))
    expr = build (code, ptr_target_type, expr, offset);
  else
    null_test = NULL;
  
  if (!want_pointer)
    expr = build_indirect_ref (expr, NULL);

  if (null_test)
    expr = build (COND_EXPR, target_type, null_test,
		  build1 (NOP_EXPR, target_type, integer_zero_node),
		  expr);

  return expr;
}

/* Convert OBJECT to the base TYPE.  If CHECK_ACCESS is true, an error
   message is emitted if TYPE is inaccessible.  OBJECT is assumed to
   be non-NULL.  */

tree
convert_to_base (tree object, tree type, bool check_access)
{
  tree binfo;

  binfo = lookup_base (TREE_TYPE (object), type, 
		       check_access ? ba_check : ba_ignore, 
		       NULL);
  if (!binfo || binfo == error_mark_node)
    return error_mark_node;

  return build_base_path (PLUS_EXPR, object, binfo, /*nonnull=*/1);
}

/* EXPR is an expression with class type.  BASE is a base class (a
   BINFO) of that class type.  Returns EXPR, converted to the BASE
   type.  This function assumes that EXPR is the most derived class;
   therefore virtual bases can be found at their static offsets.  */

tree
convert_to_base_statically (tree expr, tree base)
{
  tree expr_type;

  expr_type = TREE_TYPE (expr);
  if (!same_type_p (expr_type, BINFO_TYPE (base)))
    {
      tree pointer_type;

      pointer_type = build_pointer_type (expr_type);
      expr = build_unary_op (ADDR_EXPR, expr, /*noconvert=*/1);
      if (!integer_zerop (BINFO_OFFSET (base)))
	  expr = build (PLUS_EXPR, pointer_type, expr, 
			build_nop (pointer_type, BINFO_OFFSET (base)));
      expr = build_nop (build_pointer_type (BINFO_TYPE (base)), expr);
      expr = build1 (INDIRECT_REF, BINFO_TYPE (base), expr);
    }

  return expr;
}


/* Virtual function things.  */

static tree
build_vtable_entry_ref (array_ref, instance, idx)
     tree array_ref, instance, idx;
{
  tree i, i2, vtable, first_fn, basetype;

  basetype = TREE_TYPE (instance);
  if (TREE_CODE (basetype) == REFERENCE_TYPE)
    basetype = TREE_TYPE (basetype);

  vtable = get_vtbl_decl_for_binfo (TYPE_BINFO (basetype));
  first_fn = TYPE_BINFO_VTABLE (basetype);

  i = fold (build_array_ref (first_fn, idx));
  i = fold (build_c_cast (ptrdiff_type_node,
			  build_unary_op (ADDR_EXPR, i, 0)));
  i2 = fold (build_array_ref (vtable, build_int_2 (0,0)));
  i2 = fold (build_c_cast (ptrdiff_type_node,
			   build_unary_op (ADDR_EXPR, i2, 0)));
  i = fold (cp_build_binary_op (MINUS_EXPR, i, i2));

  if (TREE_CODE (i) != INTEGER_CST)
    abort ();

  return build (VTABLE_REF, TREE_TYPE (array_ref), array_ref, vtable, i);
}

/* Given an object INSTANCE, return an expression which yields the
   vtable element corresponding to INDEX.  There are many special
   cases for INSTANCE which we take care of here, mainly to avoid
   creating extra tree nodes when we don't have to.  */

static tree
build_vtbl_ref_1 (instance, idx)
     tree instance, idx;
{
  tree aref;
  tree vtbl = NULL_TREE;

  /* Try to figure out what a reference refers to, and
     access its virtual function table directly.  */

  int cdtorp = 0;
  tree fixed_type = fixed_type_or_null (instance, NULL, &cdtorp);

  tree basetype = TREE_TYPE (instance);
  if (TREE_CODE (basetype) == REFERENCE_TYPE)
    basetype = TREE_TYPE (basetype);

  if (fixed_type && !cdtorp)
    {
      tree binfo = lookup_base (fixed_type, basetype,
				ba_ignore|ba_quiet, NULL);
      if (binfo)
	vtbl = BINFO_VTABLE (binfo);
    }

  if (!vtbl)
    {
      vtbl = build_vfield_ref (instance, basetype);
    }

  assemble_external (vtbl);

  aref = build_array_ref (vtbl, idx);
  TREE_CONSTANT (aref) = 1;

  return aref;
}

tree
build_vtbl_ref (instance, idx)
     tree instance, idx;
{
  tree aref = build_vtbl_ref_1 (instance, idx);

  if (flag_vtable_gc)
    aref = build_vtable_entry_ref (aref, instance, idx);

  return aref;
}

/* Given an object INSTANCE, return an expression which yields a
   function pointer corresponding to vtable element INDEX.  */

tree
build_vfn_ref (instance, idx)
     tree instance, idx;
{
  tree aref = build_vtbl_ref_1 (instance, idx);

  /* When using function descriptors, the address of the
     vtable entry is treated as a function pointer.  */
  if (TARGET_VTABLE_USES_DESCRIPTORS)
    aref = build1 (NOP_EXPR, TREE_TYPE (aref),
		   build_unary_op (ADDR_EXPR, aref, /*noconvert=*/1));

  if (flag_vtable_gc)
    aref = build_vtable_entry_ref (aref, instance, idx);

  return aref;
}

/* Return the name of the virtual function table (as an IDENTIFIER_NODE)
   for the given TYPE.  */

static tree
get_vtable_name (type)
     tree type;
{
  return mangle_vtbl_for_type (type);
}

/* Return an IDENTIFIER_NODE for the name of the virtual table table
   for TYPE.  */

tree
get_vtt_name (type)
     tree type;
{
  return mangle_vtt_for_type (type);
}

/* Create a VAR_DECL for a primary or secondary vtable for CLASS_TYPE.
   (For a secondary vtable for B-in-D, CLASS_TYPE should be D, not B.)
   Use NAME for the name of the vtable, and VTABLE_TYPE for its type.  */

static tree
build_vtable (class_type, name, vtable_type)
     tree class_type;
     tree name;
     tree vtable_type;
{
  tree decl;

  decl = build_lang_decl (VAR_DECL, name, vtable_type);
  /* vtable names are already mangled; give them their DECL_ASSEMBLER_NAME
     now to avoid confusion in mangle_decl.  */
  SET_DECL_ASSEMBLER_NAME (decl, name);
  DECL_CONTEXT (decl) = class_type;
  DECL_ARTIFICIAL (decl) = 1;
  TREE_STATIC (decl) = 1;
  TREE_READONLY (decl) = 1;
  DECL_VIRTUAL_P (decl) = 1;
  DECL_ALIGN (decl) = TARGET_VTABLE_ENTRY_ALIGN;

  import_export_vtable (decl, class_type, 0);

  return decl;
}

/* Get the VAR_DECL of the vtable for TYPE. TYPE need not be polymorphic,
   or even complete.  If this does not exist, create it.  If COMPLETE is
   nonzero, then complete the definition of it -- that will render it
   impossible to actually build the vtable, but is useful to get at those
   which are known to exist in the runtime.  */

tree 
get_vtable_decl (type, complete)
     tree type;
     int complete;
{
  tree decl;

  if (CLASSTYPE_VTABLES (type))
    return CLASSTYPE_VTABLES (type);
  
  decl = build_vtable (type, get_vtable_name (type), void_type_node);
  CLASSTYPE_VTABLES (type) = decl;

  /* At one time the vtable info was grabbed 2 words at a time.  This
     fails on sparc unless you have 8-byte alignment.  (tiemann) */
  DECL_ALIGN (decl) = MAX (TYPE_ALIGN (double_type_node),
			   DECL_ALIGN (decl));

  if (complete)
    {
      DECL_EXTERNAL (decl) = 1;
      cp_finish_decl (decl, NULL_TREE, NULL_TREE, 0);
    }

  return decl;
}

/* Returns a copy of the BINFO_VIRTUALS list in BINFO.  The
   BV_VCALL_INDEX for each entry is cleared.  */

static tree
copy_virtuals (binfo)
     tree binfo;
{
  tree copies;
  tree t;

  copies = copy_list (BINFO_VIRTUALS (binfo));
  for (t = copies; t; t = TREE_CHAIN (t))
    BV_VCALL_INDEX (t) = NULL_TREE;

  return copies;
}

/* Build the primary virtual function table for TYPE.  If BINFO is
   non-NULL, build the vtable starting with the initial approximation
   that it is the same as the one which is the head of the association
   list.  Returns a nonzero value if a new vtable is actually
   created.  */

static int
build_primary_vtable (binfo, type)
     tree binfo, type;
{
  tree decl;
  tree virtuals;

  decl = get_vtable_decl (type, /*complete=*/0);
  
  if (binfo)
    {
      if (BINFO_NEW_VTABLE_MARKED (binfo, type))
	/* We have already created a vtable for this base, so there's
	   no need to do it again.  */
	return 0;
      
      virtuals = copy_virtuals (binfo);
      TREE_TYPE (decl) = TREE_TYPE (get_vtbl_decl_for_binfo (binfo));
      DECL_SIZE (decl) = TYPE_SIZE (TREE_TYPE (decl));
      DECL_SIZE_UNIT (decl) = TYPE_SIZE_UNIT (TREE_TYPE (decl));
    }
  else
    {
      my_friendly_assert (TREE_CODE (TREE_TYPE (decl)) == VOID_TYPE,
                          20000118);
      virtuals = NULL_TREE;
    }

#ifdef GATHER_STATISTICS
  n_vtables += 1;
  n_vtable_elems += list_length (virtuals);
#endif

  /* Initialize the association list for this type, based
     on our first approximation.  */
  TYPE_BINFO_VTABLE (type) = decl;
  TYPE_BINFO_VIRTUALS (type) = virtuals;
  SET_BINFO_NEW_VTABLE_MARKED (TYPE_BINFO (type), type);
  return 1;
}

/* Give BINFO a new virtual function table which is initialized
   with a skeleton-copy of its original initialization.  The only
   entry that changes is the `delta' entry, so we can really
   share a lot of structure.

   FOR_TYPE is the most derived type which caused this table to
   be needed.

   Returns nonzero if we haven't met BINFO before.

   The order in which vtables are built (by calling this function) for
   an object must remain the same, otherwise a binary incompatibility
   can result.  */

static int
build_secondary_vtable (binfo, for_type)
     tree binfo, for_type;
{
  my_friendly_assert (binfo == CANONICAL_BINFO (binfo, for_type), 20010605);

  if (BINFO_NEW_VTABLE_MARKED (binfo, for_type))
    /* We already created a vtable for this base.  There's no need to
       do it again.  */
    return 0;

  /* Remember that we've created a vtable for this BINFO, so that we
     don't try to do so again.  */
  SET_BINFO_NEW_VTABLE_MARKED (binfo, for_type);
  
  /* Make fresh virtual list, so we can smash it later.  */
  BINFO_VIRTUALS (binfo) = copy_virtuals (binfo);

  /* Secondary vtables are laid out as part of the same structure as
     the primary vtable.  */
  BINFO_VTABLE (binfo) = NULL_TREE;
  return 1;
}

/* Create a new vtable for BINFO which is the hierarchy dominated by
   T. Return nonzero if we actually created a new vtable.  */

static int
make_new_vtable (t, binfo)
     tree t;
     tree binfo;
{
  if (binfo == TYPE_BINFO (t))
    /* In this case, it is *type*'s vtable we are modifying.  We start
       with the approximation that its vtable is that of the
       immediate base class.  */
    /* ??? This actually passes TYPE_BINFO (t), not the primary base binfo,
       since we've updated DECL_CONTEXT (TYPE_VFIELD (t)) by now.  */
    return build_primary_vtable (TYPE_BINFO (DECL_CONTEXT (TYPE_VFIELD (t))),
				 t);
  else
    /* This is our very own copy of `basetype' to play with.  Later,
       we will fill in all the virtual functions that override the
       virtual functions in these base classes which are not defined
       by the current type.  */
    return build_secondary_vtable (binfo, t);
}

/* Make *VIRTUALS, an entry on the BINFO_VIRTUALS list for BINFO
   (which is in the hierarchy dominated by T) list FNDECL as its
   BV_FN.  DELTA is the required constant adjustment from the `this'
   pointer where the vtable entry appears to the `this' required when
   the function is actually called.  */

static void
modify_vtable_entry (t, binfo, fndecl, delta, virtuals)
     tree t;
     tree binfo;
     tree fndecl;
     tree delta;
     tree *virtuals;
{
  tree v;

  v = *virtuals;

  if (fndecl != BV_FN (v)
      || !tree_int_cst_equal (delta, BV_DELTA (v)))
    {
      tree base_fndecl;

      /* We need a new vtable for BINFO.  */
      if (make_new_vtable (t, binfo))
	{
	  /* If we really did make a new vtable, we also made a copy
	     of the BINFO_VIRTUALS list.  Now, we have to find the
	     corresponding entry in that list.  */
	  *virtuals = BINFO_VIRTUALS (binfo);
	  while (BV_FN (*virtuals) != BV_FN (v))
	    *virtuals = TREE_CHAIN (*virtuals);
	  v = *virtuals;
	}

      base_fndecl = BV_FN (v);
      BV_DELTA (v) = delta;
      BV_VCALL_INDEX (v) = NULL_TREE;
      BV_FN (v) = fndecl;
    }
}


/* Add method METHOD to class TYPE.  If ERROR_P is true, we are adding
   the method after the class has already been defined because a
   declaration for it was seen.  (Even though that is erroneous, we
   add the method for improved error recovery.)  */

void
add_method (type, method, error_p)
     tree type;
     tree method;
     int error_p;
{
  int using = (DECL_CONTEXT (method) != type);
  int len;
  int slot;
  tree method_vec;
  int template_conv_p = (TREE_CODE (method) == TEMPLATE_DECL
			 && DECL_TEMPLATE_CONV_FN_P (method));

  if (!CLASSTYPE_METHOD_VEC (type))
    /* Make a new method vector.  We start with 8 entries.  We must
       allocate at least two (for constructors and destructors), and
       we're going to end up with an assignment operator at some point
       as well.
       
       We could use a TREE_LIST for now, and convert it to a TREE_VEC
       in finish_struct, but we would probably waste more memory
       making the links in the list than we would by over-allocating
       the size of the vector here.  Furthermore, we would complicate
       all the code that expects this to be a vector.  */
    CLASSTYPE_METHOD_VEC (type) = make_tree_vec (8);

  method_vec = CLASSTYPE_METHOD_VEC (type);
  len = TREE_VEC_LENGTH (method_vec);

  /* Constructors and destructors go in special slots.  */
  if (DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P (method))
    slot = CLASSTYPE_CONSTRUCTOR_SLOT;
  else if (DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P (method))
    slot = CLASSTYPE_DESTRUCTOR_SLOT;
  else
    {
      int have_template_convs_p = 0;
      
      /* See if we already have an entry with this name.  */
      for (slot = CLASSTYPE_FIRST_CONVERSION_SLOT; slot < len; ++slot)
	{
	  tree m = TREE_VEC_ELT (method_vec, slot);

	  if (!m)
	    break;
	  m = OVL_CURRENT (m);
	  
	  if (template_conv_p)
	    {
	      have_template_convs_p = (TREE_CODE (m) == TEMPLATE_DECL
				       && DECL_TEMPLATE_CONV_FN_P (m));
	      
	      /* If we need to move things up, see if there's
		 space.  */
	      if (!have_template_convs_p)
		{
		  slot = len - 1;
		  if (TREE_VEC_ELT (method_vec, slot))
		    slot++;
		}
	      break;
	    }
	  if (DECL_NAME (m) == DECL_NAME (method))
	    break;
	}
      
      if (slot == len)
	{
	  /* We need a bigger method vector.  */
	  int new_len;
	  tree new_vec;

	  /* In the non-error case, we are processing a class
	     definition.  Double the size of the vector to give room
	     for new methods.  */
	  if (!error_p)
	    new_len = 2 * len;
	  /* In the error case, the vector is already complete.  We
	     don't expect many errors, and the rest of the front-end
	     will get confused if there are empty slots in the vector.  */
	  else
	    new_len = len + 1;

	  new_vec = make_tree_vec (new_len);
	  memcpy (&TREE_VEC_ELT (new_vec, 0), &TREE_VEC_ELT (method_vec, 0),
		  len * sizeof (tree));
	  len = new_len;
	  method_vec = CLASSTYPE_METHOD_VEC (type) = new_vec;
	}

      if (DECL_CONV_FN_P (method) && !TREE_VEC_ELT (method_vec, slot))
	{
	  /* Type conversion operators have to come before ordinary
	     methods; add_conversions depends on this to speed up
	     looking for conversion operators.  So, if necessary, we
	     slide some of the vector elements up.  In theory, this
	     makes this algorithm O(N^2) but we don't expect many
	     conversion operators.  */
	  if (template_conv_p)
	    slot = CLASSTYPE_FIRST_CONVERSION_SLOT;
	  else
	    for (slot = CLASSTYPE_FIRST_CONVERSION_SLOT; slot < len; ++slot)
	      {
		tree fn = TREE_VEC_ELT (method_vec, slot);
  
		if (!fn)
		  /* There are no more entries in the vector, so we
		     can insert the new conversion operator here.  */
		  break;
  		  
		if (!DECL_CONV_FN_P (OVL_CURRENT (fn)))
		  /* We can insert the new function right at the
		     SLOTth position.  */
		  break;
	      }

	  if (template_conv_p && have_template_convs_p)
	    /*OK*/;
	  else if (!TREE_VEC_ELT (method_vec, slot))
	    /* There is nothing in the Ith slot, so we can avoid
	       moving anything.  */
		; 
	  else
	    {
	      /* We know the last slot in the vector is empty
		 because we know that at this point there's room
		 for a new function.  */
	      memmove (&TREE_VEC_ELT (method_vec, slot + 1),
		       &TREE_VEC_ELT (method_vec, slot),
		       (len - slot - 1) * sizeof (tree));
	      TREE_VEC_ELT (method_vec, slot) = NULL_TREE;
	    }
	}
    }
      
  if (template_class_depth (type))
    /* TYPE is a template class.  Don't issue any errors now; wait
       until instantiation time to complain.  */
    ;
  else
    {
      tree fns;

      /* Check to see if we've already got this method.  */
      for (fns = TREE_VEC_ELT (method_vec, slot);
	   fns;
	   fns = OVL_NEXT (fns))
	{
	  tree fn = OVL_CURRENT (fns);
	  tree parms1;
	  tree parms2;
	  bool same = 1;

	  if (TREE_CODE (fn) != TREE_CODE (method))
	    continue;

	  /* [over.load] Member function declarations with the
	     same name and the same parameter types cannot be
	     overloaded if any of them is a static member
	     function declaration.

	     [namespace.udecl] When a using-declaration brings names
	     from a base class into a derived class scope, member
	     functions in the derived class override and/or hide member
	     functions with the same name and parameter types in a base
	     class (rather than conflicting).  */
	  parms1 = TYPE_ARG_TYPES (TREE_TYPE (fn));
	  parms2 = TYPE_ARG_TYPES (TREE_TYPE (method));

	  /* Compare the quals on the 'this' parm.  Don't compare
	     the whole types, as used functions are treated as
	     coming from the using class in overload resolution.  */
	  if (! DECL_STATIC_FUNCTION_P (fn)
	      && ! DECL_STATIC_FUNCTION_P (method)
	      && (TYPE_QUALS (TREE_TYPE (TREE_VALUE (parms1)))
		  != TYPE_QUALS (TREE_TYPE (TREE_VALUE (parms2)))))
	    same = 0;
	  
	  /* For templates, the template parms must be identical.  */
	  if (TREE_CODE (fn) == TEMPLATE_DECL
	      && !comp_template_parms (DECL_TEMPLATE_PARMS (fn),
				       DECL_TEMPLATE_PARMS (method)))
	    same = 0;
	  
	  if (! DECL_STATIC_FUNCTION_P (fn))
	    parms1 = TREE_CHAIN (parms1);
	  if (! DECL_STATIC_FUNCTION_P (method))
	    parms2 = TREE_CHAIN (parms2);

	  if (same && compparms (parms1, parms2) 
	      && (!DECL_CONV_FN_P (fn) 
		  || same_type_p (TREE_TYPE (TREE_TYPE (fn)),
				  TREE_TYPE (TREE_TYPE (method)))))
	    {
	      if (using && DECL_CONTEXT (fn) == type)
		/* Defer to the local function.  */
		return;
	      else
		{
		  cp_error_at ("`%#D' and `%#D' cannot be overloaded",
			       method, fn, method);

		  /* We don't call duplicate_decls here to merge
		     the declarations because that will confuse
		     things if the methods have inline
		     definitions.  In particular, we will crash
		     while processing the definitions.  */
		  return;
		}
	    }
	}
    }

  /* Actually insert the new method.  */
  TREE_VEC_ELT (method_vec, slot) 
    = build_overload (method, TREE_VEC_ELT (method_vec, slot));

  /* Add the new binding.  */ 
  if (!DECL_CONSTRUCTOR_P (method)
      && !DECL_DESTRUCTOR_P (method))
    push_class_level_binding (DECL_NAME (method),
			      TREE_VEC_ELT (method_vec, slot));
}

/* Subroutines of finish_struct.  */

/* Look through the list of fields for this struct, deleting
   duplicates as we go.  This must be recursive to handle
   anonymous unions.

   FIELD is the field which may not appear anywhere in FIELDS.
   FIELD_PTR, if non-null, is the starting point at which
   chained deletions may take place.
   The value returned is the first acceptable entry found
   in FIELDS.

   Note that anonymous fields which are not of UNION_TYPE are
   not duplicates, they are just anonymous fields.  This happens
   when we have unnamed bitfields, for example.  */

static tree
delete_duplicate_fields_1 (field, fields)
     tree field, fields;
{
  tree x;
  tree prev = 0;
  if (DECL_NAME (field) == 0)
    {
      if (! ANON_AGGR_TYPE_P (TREE_TYPE (field)))
	return fields;

      for (x = TYPE_FIELDS (TREE_TYPE (field)); x; x = TREE_CHAIN (x))
	fields = delete_duplicate_fields_1 (x, fields);
      return fields;
    }
  else
    {
      for (x = fields; x; prev = x, x = TREE_CHAIN (x))
	{
	  if (DECL_NAME (x) == 0)
	    {
	      if (! ANON_AGGR_TYPE_P (TREE_TYPE (x)))
		continue;
	      TYPE_FIELDS (TREE_TYPE (x))
		= delete_duplicate_fields_1 (field, TYPE_FIELDS (TREE_TYPE (x)));
	      if (TYPE_FIELDS (TREE_TYPE (x)) == 0)
		{
		  if (prev == 0)
		    fields = TREE_CHAIN (fields);
		  else
		    TREE_CHAIN (prev) = TREE_CHAIN (x);
		}
	    }
	  else if (TREE_CODE (field) == USING_DECL)
	    /* A using declaration is allowed to appear more than
	       once.  We'll prune these from the field list later, and
	       handle_using_decl will complain about invalid multiple
	       uses.  */
	    ;
	  else if (DECL_NAME (field) == DECL_NAME (x))
	    {
	      if (TREE_CODE (field) == CONST_DECL
		  && TREE_CODE (x) == CONST_DECL)
		cp_error_at ("duplicate enum value `%D'", x);
	      else if (TREE_CODE (field) == CONST_DECL
		       || TREE_CODE (x) == CONST_DECL)
		cp_error_at ("duplicate field `%D' (as enum and non-enum)",
			     x);
	      else if (DECL_DECLARES_TYPE_P (field)
		       && DECL_DECLARES_TYPE_P (x))
		{
		  if (same_type_p (TREE_TYPE (field), TREE_TYPE (x)))
		    continue;
		  cp_error_at ("duplicate nested type `%D'", x);
		}
	      else if (DECL_DECLARES_TYPE_P (field)
		       || DECL_DECLARES_TYPE_P (x))
		{
		  /* Hide tag decls.  */
		  if ((TREE_CODE (field) == TYPE_DECL
		       && DECL_ARTIFICIAL (field))
		      || (TREE_CODE (x) == TYPE_DECL
			  && DECL_ARTIFICIAL (x)))
		    continue;
		  cp_error_at ("duplicate field `%D' (as type and non-type)",
			       x);
		}
	      else
		cp_error_at ("duplicate member `%D'", x);
	      if (prev == 0)
		fields = TREE_CHAIN (fields);
	      else
		TREE_CHAIN (prev) = TREE_CHAIN (x);
	    }
	}
    }
  return fields;
}

static void
delete_duplicate_fields (fields)
     tree fields;
{
  tree x;
  for (x = fields; x && TREE_CHAIN (x); x = TREE_CHAIN (x))
    TREE_CHAIN (x) = delete_duplicate_fields_1 (x, TREE_CHAIN (x));
}

/* Change the access of FDECL to ACCESS in T.  Return 1 if change was
   legit, otherwise return 0.  */

static int
alter_access (t, fdecl, access)
     tree t;
     tree fdecl;
     tree access;
{
  tree elem;

  if (!DECL_LANG_SPECIFIC (fdecl))
    retrofit_lang_decl (fdecl);

  if (DECL_DISCRIMINATOR_P (fdecl))
    abort ();

  elem = purpose_member (t, DECL_ACCESS (fdecl));
  if (elem)
    {
      if (TREE_VALUE (elem) != access)
	{
	  if (TREE_CODE (TREE_TYPE (fdecl)) == FUNCTION_DECL)
	    cp_error_at ("conflicting access specifications for method `%D', ignored", TREE_TYPE (fdecl));
	  else
	    error ("conflicting access specifications for field `%s', ignored",
		   IDENTIFIER_POINTER (DECL_NAME (fdecl)));
	}
      else
	{
	  /* They're changing the access to the same thing they changed
	     it to before.  That's OK.  */
	  ;
	}
    }
  else
    {
      enforce_access (t, fdecl);
      DECL_ACCESS (fdecl) = tree_cons (t, access, DECL_ACCESS (fdecl));
      return 1;
    }
  return 0;
}

/* Process the USING_DECL, which is a member of T.  */

static void
handle_using_decl (using_decl, t)
     tree using_decl;
     tree t;
{
  tree ctype = DECL_INITIAL (using_decl);
  tree name = DECL_NAME (using_decl);
  tree access
    = TREE_PRIVATE (using_decl) ? access_private_node
    : TREE_PROTECTED (using_decl) ? access_protected_node
    : access_public_node;
  tree fdecl, binfo;
  tree flist = NULL_TREE;
  tree old_value;

  if (ctype == error_mark_node)
    return;

  binfo = lookup_base (t, ctype, ba_any, NULL);
  if (! binfo)
    {
      error_not_base_type (t, ctype);
      return;
    }
  
  if (constructor_name_p (name, ctype))
    {
      cp_error_at ("`%D' names constructor", using_decl);
      return;
    }
  if (constructor_name_p (name, t))
    {
      cp_error_at ("`%D' invalid in `%T'", using_decl, t);
      return;
    }

  fdecl = lookup_member (binfo, name, 0, 0);
  
  if (!fdecl)
    {
      cp_error_at ("no members matching `%D' in `%#T'", using_decl, ctype);
      return;
    }

  if (BASELINK_P (fdecl))
    /* Ignore base type this came from.  */
    fdecl = BASELINK_FUNCTIONS (fdecl);

  old_value = IDENTIFIER_CLASS_VALUE (name);
  if (old_value)
    {
      if (is_overloaded_fn (old_value))
	old_value = OVL_CURRENT (old_value);

      if (DECL_P (old_value) && DECL_CONTEXT (old_value) == t)
	/* OK */;
      else
	old_value = NULL_TREE;
    }

  if (is_overloaded_fn (fdecl))
    flist = fdecl;

  if (! old_value)
    ;
  else if (is_overloaded_fn (old_value))
    {
      if (flist)
	/* It's OK to use functions from a base when there are functions with
	   the same name already present in the current class.  */;
      else
	{
	  cp_error_at ("`%D' invalid in `%#T'", using_decl, t);
	  cp_error_at ("  because of local method `%#D' with same name",
		       OVL_CURRENT (old_value));
	  return;
	}
    }
  else if (!DECL_ARTIFICIAL (old_value))
    {
      cp_error_at ("`%D' invalid in `%#T'", using_decl, t);
      cp_error_at ("  because of local member `%#D' with same name", old_value);
      return;
    }
  
  /* Make type T see field decl FDECL with access ACCESS.*/
  if (flist)
    for (; flist; flist = OVL_NEXT (flist))
      {
	add_method (t, OVL_CURRENT (flist), /*error_p=*/0);
	alter_access (t, OVL_CURRENT (flist), access);
      }
  else
    alter_access (t, fdecl, access);
}

/* Run through the base clases of T, updating
   CANT_HAVE_DEFAULT_CTOR_P, CANT_HAVE_CONST_CTOR_P, and
   NO_CONST_ASN_REF_P.  Also set flag bits in T based on properties of
   the bases.  */

static void
check_bases (t, cant_have_default_ctor_p, cant_have_const_ctor_p,
	     no_const_asn_ref_p)
     tree t;
     int *cant_have_default_ctor_p;
     int *cant_have_const_ctor_p;
     int *no_const_asn_ref_p;
{
  int n_baseclasses;
  int i;
  int seen_non_virtual_nearly_empty_base_p;
  tree binfos;

  binfos = TYPE_BINFO_BASETYPES (t);
  n_baseclasses = CLASSTYPE_N_BASECLASSES (t);
  seen_non_virtual_nearly_empty_base_p = 0;

  /* An aggregate cannot have baseclasses.  */
  CLASSTYPE_NON_AGGREGATE (t) |= (n_baseclasses != 0);

  for (i = 0; i < n_baseclasses; ++i) 
    {
      tree base_binfo;
      tree basetype;

      /* Figure out what base we're looking at.  */
      base_binfo = TREE_VEC_ELT (binfos, i);
      basetype = TREE_TYPE (base_binfo);

      /* If the type of basetype is incomplete, then we already
	 complained about that fact (and we should have fixed it up as
	 well).  */
      if (!COMPLETE_TYPE_P (basetype))
	{
	  int j;
	  /* The base type is of incomplete type.  It is
	     probably best to pretend that it does not
	     exist.  */
	  if (i == n_baseclasses-1)
	    TREE_VEC_ELT (binfos, i) = NULL_TREE;
	  TREE_VEC_LENGTH (binfos) -= 1;
	  n_baseclasses -= 1;
	  for (j = i; j+1 < n_baseclasses; j++)
	    TREE_VEC_ELT (binfos, j) = TREE_VEC_ELT (binfos, j+1);
	  continue;
	}

      /* Effective C++ rule 14.  We only need to check TYPE_POLYMORPHIC_P
	 here because the case of virtual functions but non-virtual
	 dtor is handled in finish_struct_1.  */
      if (warn_ecpp && ! TYPE_POLYMORPHIC_P (basetype)
	  && TYPE_HAS_DESTRUCTOR (basetype))
	warning ("base class `%#T' has a non-virtual destructor",
		    basetype);

      /* If the base class doesn't have copy constructors or
	 assignment operators that take const references, then the
	 derived class cannot have such a member automatically
	 generated.  */
      if (! TYPE_HAS_CONST_INIT_REF (basetype))
	*cant_have_const_ctor_p = 1;
      if (TYPE_HAS_ASSIGN_REF (basetype)
	  && !TYPE_HAS_CONST_ASSIGN_REF (basetype))
	*no_const_asn_ref_p = 1;
      /* Similarly, if the base class doesn't have a default
	 constructor, then the derived class won't have an
	 automatically generated default constructor.  */
      if (TYPE_HAS_CONSTRUCTOR (basetype)
	  && ! TYPE_HAS_DEFAULT_CONSTRUCTOR (basetype))
	{
	  *cant_have_default_ctor_p = 1;
	  if (! TYPE_HAS_CONSTRUCTOR (t))
            pedwarn ("base `%T' with only non-default constructor in class without a constructor",
                        basetype);
	}

      if (TREE_VIA_VIRTUAL (base_binfo))
	/* A virtual base does not effect nearly emptiness.  */
	;
      else if (CLASSTYPE_NEARLY_EMPTY_P (basetype))
	{
	  if (seen_non_virtual_nearly_empty_base_p)
	    /* And if there is more than one nearly empty base, then the
	       derived class is not nearly empty either.  */
	    CLASSTYPE_NEARLY_EMPTY_P (t) = 0;
	  else
	    /* Remember we've seen one.  */
	    seen_non_virtual_nearly_empty_base_p = 1;
	}
      else if (!is_empty_class (basetype))
	/* If the base class is not empty or nearly empty, then this
	   class cannot be nearly empty.  */
	CLASSTYPE_NEARLY_EMPTY_P (t) = 0;

      /* A lot of properties from the bases also apply to the derived
	 class.  */
      TYPE_NEEDS_CONSTRUCTING (t) |= TYPE_NEEDS_CONSTRUCTING (basetype);
      TYPE_HAS_NONTRIVIAL_DESTRUCTOR (t) 
	|= TYPE_HAS_NONTRIVIAL_DESTRUCTOR (basetype);
      TYPE_HAS_COMPLEX_ASSIGN_REF (t) 
	|= TYPE_HAS_COMPLEX_ASSIGN_REF (basetype);
      TYPE_HAS_COMPLEX_INIT_REF (t) |= TYPE_HAS_COMPLEX_INIT_REF (basetype);
      TYPE_OVERLOADS_CALL_EXPR (t) |= TYPE_OVERLOADS_CALL_EXPR (basetype);
      TYPE_OVERLOADS_ARRAY_REF (t) |= TYPE_OVERLOADS_ARRAY_REF (basetype);
      TYPE_OVERLOADS_ARROW (t) |= TYPE_OVERLOADS_ARROW (basetype);
      TYPE_POLYMORPHIC_P (t) |= TYPE_POLYMORPHIC_P (basetype);
      CLASSTYPE_CONTAINS_EMPTY_CLASS_P (t) 
	|= CLASSTYPE_CONTAINS_EMPTY_CLASS_P (basetype);
    }
}

/* Binfo FROM is within a virtual hierarchy which is being reseated to
   TO. Move primary information from FROM to TO, and recursively traverse
   into FROM's bases. The hierarchy is dominated by TYPE.  MAPPINGS is an
   assoc list of binfos that have already been reseated.  */

static void
force_canonical_binfo_r (to, from, type, mappings)
     tree to;
     tree from;
     tree type;
     tree mappings;
{
  int i, n_baseclasses = BINFO_N_BASETYPES (from);

  my_friendly_assert (to != from, 20010905);
  BINFO_INDIRECT_PRIMARY_P (to)
          = BINFO_INDIRECT_PRIMARY_P (from);
  BINFO_INDIRECT_PRIMARY_P (from) = 0;
  BINFO_UNSHARED_MARKED (to) = BINFO_UNSHARED_MARKED (from);
  BINFO_UNSHARED_MARKED (from) = 0;
  BINFO_LOST_PRIMARY_P (to) = BINFO_LOST_PRIMARY_P (from);
  BINFO_LOST_PRIMARY_P (from) = 0;
  if (BINFO_PRIMARY_P (from))
    {
      tree primary = BINFO_PRIMARY_BASE_OF (from);
      tree assoc;
      
      /* We might have just moved the primary base too, see if it's on our
         mappings.  */
      assoc = purpose_member (primary, mappings);
      if (assoc)
        primary = TREE_VALUE (assoc);
      BINFO_PRIMARY_BASE_OF (to) = primary;
      BINFO_PRIMARY_BASE_OF (from) = NULL_TREE;
    }
  my_friendly_assert (same_type_p (BINFO_TYPE (to), BINFO_TYPE (from)),
		      20010104);
  mappings = tree_cons (from, to, mappings);

  if (CLASSTYPE_HAS_PRIMARY_BASE_P (BINFO_TYPE (from))
      && TREE_VIA_VIRTUAL (CLASSTYPE_PRIMARY_BINFO (BINFO_TYPE (from))))
    {
      tree from_primary = get_primary_binfo (from);
      
      if (BINFO_PRIMARY_BASE_OF (from_primary) == from)
	force_canonical_binfo (get_primary_binfo (to), from_primary,
			       type, mappings);
    }
  
  for (i = 0; i != n_baseclasses; i++)
    {
      tree from_binfo = BINFO_BASETYPE (from, i);
      tree to_binfo = BINFO_BASETYPE (to, i);

      if (TREE_VIA_VIRTUAL (from_binfo))
        {
	  if (BINFO_PRIMARY_P (from_binfo) &&
	      purpose_member (BINFO_PRIMARY_BASE_OF (from_binfo), mappings))
	    /* This base is a primary of some binfo we have already
	       reseated. We must reseat this one too.  */
            force_canonical_binfo (to_binfo, from_binfo, type, mappings);
        }
      else
        force_canonical_binfo_r (to_binfo, from_binfo, type, mappings);
    }
}

/* FROM is the canonical binfo for a virtual base. It is being reseated to
   make TO the canonical binfo, within the hierarchy dominated by TYPE.
   MAPPINGS is an assoc list of binfos that have already been reseated.
   Adjust any non-virtual bases within FROM, and also move any virtual bases
   which are canonical.  This complication arises because selecting primary
   bases walks in inheritance graph order, but we don't share binfos for
   virtual bases, hence we can fill in the primaries for a virtual base,
   and then discover that a later base requires the virtual as its
   primary.  */

static void
force_canonical_binfo (to, from, type, mappings)
     tree to;
     tree from;
     tree type;
     tree mappings;
{
  tree assoc = purpose_member (BINFO_TYPE (to),
		               CLASSTYPE_VBASECLASSES (type));
  if (TREE_VALUE (assoc) != to)
    {
      TREE_VALUE (assoc) = to;
      force_canonical_binfo_r (to, from, type, mappings);
    }
}

/* Make BASE_BINFO the a primary virtual base within the hierarchy
   dominated by TYPE. Returns BASE_BINFO, if it is not already one, NULL
   otherwise (because something else has already made it primary).  */

static tree
mark_primary_virtual_base (base_binfo, type)
     tree base_binfo;
     tree type;
{
  tree shared_binfo = binfo_for_vbase (BINFO_TYPE (base_binfo), type);

  if (BINFO_PRIMARY_P (shared_binfo))
    {
      /* It's already allocated in the hierarchy. BINFO won't have a
         primary base in this hierarchy, even though the complete object
         BINFO is for, would do.  */
      return NULL_TREE;
    }
     
  /* We need to make sure that the assoc list
     CLASSTYPE_VBASECLASSES of TYPE, indicates this particular
     primary BINFO for the virtual base, as this is the one
     that'll really exist.  */
  if (base_binfo != shared_binfo)
    force_canonical_binfo (base_binfo, shared_binfo, type, NULL);

  return base_binfo;
}

/* If BINFO is an unmarked virtual binfo for a class with a primary virtual
   base, then BINFO has no primary base in this graph.  Called from
   mark_primary_bases.  DATA is the most derived type.  */

static tree dfs_unshared_virtual_bases (binfo, data)
     tree binfo;
     void *data;
{
  tree t = (tree) data;
  
  if (!BINFO_UNSHARED_MARKED (binfo)
      && CLASSTYPE_HAS_PRIMARY_BASE_P (BINFO_TYPE (binfo)))
    {
      /* This morally virtual base has a primary base when it
         is a complete object. We need to locate the shared instance
         of this binfo in the type dominated by T. We duplicate the
         primary base information from there to here.  */
      tree vbase;
      tree unshared_base;
      
      for (vbase = binfo; !TREE_VIA_VIRTUAL (vbase);
	   vbase = BINFO_INHERITANCE_CHAIN (vbase))
	continue;
      unshared_base = get_original_base (binfo,
					 binfo_for_vbase (BINFO_TYPE (vbase),
							  t));
      my_friendly_assert (unshared_base != binfo, 20010612);
      BINFO_LOST_PRIMARY_P (binfo) = BINFO_LOST_PRIMARY_P (unshared_base);
      if (!BINFO_LOST_PRIMARY_P (binfo))
	BINFO_PRIMARY_BASE_OF (get_primary_binfo (binfo)) = binfo;
    }
  
  if (binfo != TYPE_BINFO (t))
    /* The vtable fields will have been copied when duplicating the
       base binfos. That information is bogus, make sure we don't try
       and use it.  */
    BINFO_VTABLE (binfo) = NULL_TREE;

  /* If this is a virtual primary base, make sure its offset matches
     that which it is primary for.  */
  if (BINFO_PRIMARY_P (binfo) && TREE_VIA_VIRTUAL (binfo) &&
      binfo_for_vbase (BINFO_TYPE (binfo), t) == binfo)
    {
      tree delta = size_diffop (BINFO_OFFSET (BINFO_PRIMARY_BASE_OF (binfo)),
				BINFO_OFFSET (binfo));
      if (!integer_zerop (delta))
	propagate_binfo_offsets (binfo, delta, t);
    }
  
  BINFO_UNSHARED_MARKED (binfo) = 0;
  return NULL;
}

/* Set BINFO_PRIMARY_BASE_OF for all binfos in the hierarchy
   dominated by TYPE that are primary bases.  */

static void
mark_primary_bases (type)
     tree type;
{
  tree binfo;
  
  /* Walk the bases in inheritance graph order.  */
  for (binfo = TYPE_BINFO (type); binfo; binfo = TREE_CHAIN (binfo))
    {
      tree base_binfo;
      
      if (!CLASSTYPE_HAS_PRIMARY_BASE_P (BINFO_TYPE (binfo)))
        /* Not a dynamic base.  */
        continue;

      base_binfo = get_primary_binfo (binfo);

      if (TREE_VIA_VIRTUAL (base_binfo))
        base_binfo = mark_primary_virtual_base (base_binfo, type);

      if (base_binfo)
        BINFO_PRIMARY_BASE_OF (base_binfo) = binfo;
      else
	BINFO_LOST_PRIMARY_P (binfo) = 1;
      
      BINFO_UNSHARED_MARKED (binfo) = 1;
    }
  /* There could remain unshared morally virtual bases which were not
     visited in the inheritance graph walk. These bases will have lost
     their virtual primary base (should they have one). We must now
     find them. Also we must fix up the BINFO_OFFSETs of primary
     virtual bases. We could not do that as we went along, as they
     were originally copied from the bases we inherited from by
     unshare_base_binfos. That may have decided differently about
     where a virtual primary base went.  */
  dfs_walk (TYPE_BINFO (type), dfs_unshared_virtual_bases, NULL, type);
}

/* Make the BINFO the primary base of T.  */

static void
set_primary_base (t, binfo)
     tree t;
     tree binfo;
{
  tree basetype;

  CLASSTYPE_PRIMARY_BINFO (t) = binfo;
  basetype = BINFO_TYPE (binfo);
  TYPE_BINFO_VTABLE (t) = TYPE_BINFO_VTABLE (basetype);
  TYPE_BINFO_VIRTUALS (t) = TYPE_BINFO_VIRTUALS (basetype);
  TYPE_VFIELD (t) = TYPE_VFIELD (basetype);
}

/* Determine the primary class for T.  */

static void
determine_primary_base (t)
     tree t;
{
  int i, n_baseclasses = CLASSTYPE_N_BASECLASSES (t);
  tree vbases;
  tree type_binfo;

  /* If there are no baseclasses, there is certainly no primary base.  */
  if (n_baseclasses == 0)
    return;

  type_binfo = TYPE_BINFO (t);

  for (i = 0; i < n_baseclasses; i++)
    {
      tree base_binfo = BINFO_BASETYPE (type_binfo, i);
      tree basetype = BINFO_TYPE (base_binfo);

      if (TYPE_CONTAINS_VPTR_P (basetype))
	{
	  /* We prefer a non-virtual base, although a virtual one will
	     do.  */
	  if (TREE_VIA_VIRTUAL (base_binfo))
	    continue;

	  if (!CLASSTYPE_HAS_PRIMARY_BASE_P (t))
	    {
	      set_primary_base (t, base_binfo);
	      CLASSTYPE_VFIELDS (t) = copy_list (CLASSTYPE_VFIELDS (basetype));
	    }
	  else
	    {
	      tree vfields;

	      /* Only add unique vfields, and flatten them out as we go.  */
	      for (vfields = CLASSTYPE_VFIELDS (basetype);
		   vfields;
		   vfields = TREE_CHAIN (vfields))
		if (VF_BINFO_VALUE (vfields) == NULL_TREE
		    || ! TREE_VIA_VIRTUAL (VF_BINFO_VALUE (vfields)))
		  CLASSTYPE_VFIELDS (t) 
		    = tree_cons (base_binfo, 
				 VF_BASETYPE_VALUE (vfields),
				 CLASSTYPE_VFIELDS (t));
	    }
	}
    }

  if (!TYPE_VFIELD (t))
    CLASSTYPE_PRIMARY_BINFO (t) = NULL_TREE;

  /* Find the indirect primary bases - those virtual bases which are primary
     bases of something else in this hierarchy.  */
  for (vbases = CLASSTYPE_VBASECLASSES (t);
       vbases;
       vbases = TREE_CHAIN (vbases)) 
    {
      tree vbase_binfo = TREE_VALUE (vbases);

      /* See if this virtual base is an indirect primary base.  To be so,
         it must be a primary base within the hierarchy of one of our
         direct bases.  */
      for (i = 0; i < n_baseclasses; ++i) 
	{
	  tree basetype = TYPE_BINFO_BASETYPE (t, i);
	  tree v;

	  for (v = CLASSTYPE_VBASECLASSES (basetype); 
	       v; 
	       v = TREE_CHAIN (v))
	    {
	      tree base_vbase = TREE_VALUE (v);
	      
	      if (BINFO_PRIMARY_P (base_vbase)
		  && same_type_p (BINFO_TYPE (base_vbase),
	                          BINFO_TYPE (vbase_binfo)))
		{
		  BINFO_INDIRECT_PRIMARY_P (vbase_binfo) = 1;
		  break;
		}
	    }

	  /* If we've discovered that this virtual base is an indirect
	     primary base, then we can move on to the next virtual
	     base.  */
	  if (BINFO_INDIRECT_PRIMARY_P (vbase_binfo))
	    break;
	}
    }

  /* A "nearly-empty" virtual base class can be the primary base
     class, if no non-virtual polymorphic base can be found.  */
  if (!CLASSTYPE_HAS_PRIMARY_BASE_P (t))
    {
      /* If not NULL, this is the best primary base candidate we have
         found so far.  */
      tree candidate = NULL_TREE;
      tree base_binfo;

      /* Loop over the baseclasses.  */
      for (base_binfo = TYPE_BINFO (t);
	   base_binfo;
	   base_binfo = TREE_CHAIN (base_binfo))
	{
	  tree basetype = BINFO_TYPE (base_binfo);

	  if (TREE_VIA_VIRTUAL (base_binfo) 
	      && CLASSTYPE_NEARLY_EMPTY_P (basetype))
	    {
	      /* If this is not an indirect primary base, then it's
		 definitely our primary base.  */
	      if (!BINFO_INDIRECT_PRIMARY_P (base_binfo))
		{
		  candidate = base_binfo;
		  break;
		}

	      /* If this is an indirect primary base, it still could be
	         our primary base -- unless we later find there's another
	         nearly-empty virtual base that isn't an indirect
	         primary base.  */
	      if (!candidate)
		candidate = base_binfo;
	    }
	}

      /* If we've got a primary base, use it.  */
      if (candidate)
	{
	  set_primary_base (t, candidate);
	  CLASSTYPE_VFIELDS (t) 
	    = copy_list (CLASSTYPE_VFIELDS (BINFO_TYPE (candidate)));
	}	
    }

  /* Mark the primary base classes at this point.  */
  mark_primary_bases (t);
}

/* Set memoizing fields and bits of T (and its variants) for later
   use.  */

static void
finish_struct_bits (t)
     tree t;
{
  int i, n_baseclasses = CLASSTYPE_N_BASECLASSES (t);

  /* Fix up variants (if any).  */
  tree variants = TYPE_NEXT_VARIANT (t);
  while (variants)
    {
      /* These fields are in the _TYPE part of the node, not in
	 the TYPE_LANG_SPECIFIC component, so they are not shared.  */
      TYPE_HAS_CONSTRUCTOR (variants) = TYPE_HAS_CONSTRUCTOR (t);
      TYPE_HAS_DESTRUCTOR (variants) = TYPE_HAS_DESTRUCTOR (t);
      TYPE_NEEDS_CONSTRUCTING (variants) = TYPE_NEEDS_CONSTRUCTING (t);
      TYPE_HAS_NONTRIVIAL_DESTRUCTOR (variants) 
	= TYPE_HAS_NONTRIVIAL_DESTRUCTOR (t);

      TYPE_BASE_CONVS_MAY_REQUIRE_CODE_P (variants) 
	= TYPE_BASE_CONVS_MAY_REQUIRE_CODE_P (t);
      TYPE_POLYMORPHIC_P (variants) = TYPE_POLYMORPHIC_P (t);
      TYPE_USES_VIRTUAL_BASECLASSES (variants) = TYPE_USES_VIRTUAL_BASECLASSES (t);
      /* Copy whatever these are holding today.  */
      TYPE_MIN_VALUE (variants) = TYPE_MIN_VALUE (t);
      TYPE_MAX_VALUE (variants) = TYPE_MAX_VALUE (t);
      TYPE_FIELDS (variants) = TYPE_FIELDS (t);
      TYPE_SIZE (variants) = TYPE_SIZE (t);
      TYPE_SIZE_UNIT (variants) = TYPE_SIZE_UNIT (t);
      variants = TYPE_NEXT_VARIANT (variants);
    }

  if (n_baseclasses && TYPE_POLYMORPHIC_P (t))
    /* For a class w/o baseclasses, `finish_struct' has set
       CLASS_TYPE_ABSTRACT_VIRTUALS correctly (by
       definition). Similarly for a class whose base classes do not
       have vtables. When neither of these is true, we might have
       removed abstract virtuals (by providing a definition), added
       some (by declaring new ones), or redeclared ones from a base
       class. We need to recalculate what's really an abstract virtual
       at this point (by looking in the vtables).  */
      get_pure_virtuals (t);

  if (n_baseclasses)
    {
      /* Notice whether this class has type conversion functions defined.  */
      tree binfo = TYPE_BINFO (t);
      tree binfos = BINFO_BASETYPES (binfo);
      tree basetype;

      for (i = n_baseclasses-1; i >= 0; i--)
	{
	  basetype = BINFO_TYPE (TREE_VEC_ELT (binfos, i));

	  TYPE_HAS_CONVERSION (t) |= TYPE_HAS_CONVERSION (basetype);
	}
    }

  /* If this type has a copy constructor or a destructor, force its mode to
     be BLKmode, and force its TREE_ADDRESSABLE bit to be nonzero.  This
     will cause it to be passed by invisible reference and prevent it from
     being returned in a register.  */
  if (! TYPE_HAS_TRIVIAL_INIT_REF (t) || TYPE_HAS_NONTRIVIAL_DESTRUCTOR (t))
    {
      tree variants;
      DECL_MODE (TYPE_MAIN_DECL (t)) = BLKmode;
      for (variants = t; variants; variants = TYPE_NEXT_VARIANT (variants))
	{
	  TYPE_MODE (variants) = BLKmode;
	  TREE_ADDRESSABLE (variants) = 1;
	}
    }
}

/* Issue warnings about T having private constructors, but no friends,
   and so forth.  

   HAS_NONPRIVATE_METHOD is nonzero if T has any non-private methods or
   static members.  HAS_NONPRIVATE_STATIC_FN is nonzero if T has any
   non-private static member functions.  */

static void
maybe_warn_about_overly_private_class (t)
     tree t;
{
  int has_member_fn = 0;
  int has_nonprivate_method = 0;
  tree fn;

  if (!warn_ctor_dtor_privacy
      /* If the class has friends, those entities might create and
	 access instances, so we should not warn.  */
      || (CLASSTYPE_FRIEND_CLASSES (t)
	  || DECL_FRIENDLIST (TYPE_MAIN_DECL (t)))
      /* We will have warned when the template was declared; there's
	 no need to warn on every instantiation.  */
      || CLASSTYPE_TEMPLATE_INSTANTIATION (t))
    /* There's no reason to even consider warning about this 
       class.  */
    return;
    
  /* We only issue one warning, if more than one applies, because
     otherwise, on code like:

     class A {
       // Oops - forgot `public:'
       A();
       A(const A&);
       ~A();
     };

     we warn several times about essentially the same problem.  */

  /* Check to see if all (non-constructor, non-destructor) member
     functions are private.  (Since there are no friends or
     non-private statics, we can't ever call any of the private member
     functions.)  */
  for (fn = TYPE_METHODS (t); fn; fn = TREE_CHAIN (fn))
    /* We're not interested in compiler-generated methods; they don't
       provide any way to call private members.  */
    if (!DECL_ARTIFICIAL (fn)) 
      {
	if (!TREE_PRIVATE (fn))
	  {
	    if (DECL_STATIC_FUNCTION_P (fn)) 
	      /* A non-private static member function is just like a
		 friend; it can create and invoke private member
		 functions, and be accessed without a class
		 instance.  */
	      return;
		
	    has_nonprivate_method = 1;
	    /* Keep searching for a static member function.  */
	  }
	else if (!DECL_CONSTRUCTOR_P (fn) && !DECL_DESTRUCTOR_P (fn))
	  has_member_fn = 1;
      } 

  if (!has_nonprivate_method && has_member_fn) 
    {
      /* There are no non-private methods, and there's at least one
	 private member function that isn't a constructor or
	 destructor.  (If all the private members are
	 constructors/destructors we want to use the code below that
	 issues error messages specifically referring to
	 constructors/destructors.)  */
      int i;
      tree binfos = BINFO_BASETYPES (TYPE_BINFO (t));
      for (i = 0; i < CLASSTYPE_N_BASECLASSES (t); i++)
	if (TREE_VIA_PUBLIC (TREE_VEC_ELT (binfos, i))
	    || TREE_VIA_PROTECTED (TREE_VEC_ELT (binfos, i)))
	  {
	    has_nonprivate_method = 1;
	    break;
	  }
      if (!has_nonprivate_method) 
	{
	  warning ("all member functions in class `%T' are private", t);
	  return;
	}
    }

  /* Even if some of the member functions are non-private, the class
     won't be useful for much if all the constructors or destructors
     are private: such an object can never be created or destroyed.  */
  if (TYPE_HAS_DESTRUCTOR (t))
    {
      tree dtor = TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (t), 1);

      if (TREE_PRIVATE (dtor))
	{
	  warning ("`%#T' only defines a private destructor and has no friends",
		      t);
	  return;
	}
    }

  if (TYPE_HAS_CONSTRUCTOR (t))
    {
      int nonprivate_ctor = 0;
	  
      /* If a non-template class does not define a copy
	 constructor, one is defined for it, enabling it to avoid
	 this warning.  For a template class, this does not
	 happen, and so we would normally get a warning on:

	   template <class T> class C { private: C(); };  
	  
	 To avoid this asymmetry, we check TYPE_HAS_INIT_REF.  All
	 complete non-template or fully instantiated classes have this
	 flag set.  */
      if (!TYPE_HAS_INIT_REF (t))
	nonprivate_ctor = 1;
      else 
	for (fn = TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (t), 0);
	     fn;
	     fn = OVL_NEXT (fn)) 
	  {
	    tree ctor = OVL_CURRENT (fn);
	    /* Ideally, we wouldn't count copy constructors (or, in
	       fact, any constructor that takes an argument of the
	       class type as a parameter) because such things cannot
	       be used to construct an instance of the class unless
	       you already have one.  But, for now at least, we're
	       more generous.  */
	    if (! TREE_PRIVATE (ctor))
	      {
		nonprivate_ctor = 1;
		break;
	      }
	  }

      if (nonprivate_ctor == 0)
	{
	  warning ("`%#T' only defines private constructors and has no friends",
		      t);
	  return;
	}
    }
}

/* Function to help qsort sort FIELD_DECLs by name order.  */

static int
field_decl_cmp (x, y)
     const tree *x, *y;
{
  if (DECL_NAME (*x) == DECL_NAME (*y))
    /* A nontype is "greater" than a type.  */
    return DECL_DECLARES_TYPE_P (*y) - DECL_DECLARES_TYPE_P (*x);
  if (DECL_NAME (*x) == NULL_TREE)
    return -1;
  if (DECL_NAME (*y) == NULL_TREE)
    return 1;
  if (DECL_NAME (*x) < DECL_NAME (*y))
    return -1;
  return 1;
}

/* Comparison function to compare two TYPE_METHOD_VEC entries by name.  */

static int
method_name_cmp (m1, m2)
     const tree *m1, *m2;
{
  if (*m1 == NULL_TREE && *m2 == NULL_TREE)
    return 0;
  if (*m1 == NULL_TREE)
    return -1;
  if (*m2 == NULL_TREE)
    return 1;
  if (DECL_NAME (OVL_CURRENT (*m1)) < DECL_NAME (OVL_CURRENT (*m2)))
    return -1;
  return 1;
}

/* Warn about duplicate methods in fn_fields.  Also compact method
   lists so that lookup can be made faster.

   Data Structure: List of method lists.  The outer list is a
   TREE_LIST, whose TREE_PURPOSE field is the field name and the
   TREE_VALUE is the DECL_CHAIN of the FUNCTION_DECLs.  TREE_CHAIN
   links the entire list of methods for TYPE_METHODS.  Friends are
   chained in the same way as member functions (? TREE_CHAIN or
   DECL_CHAIN), but they live in the TREE_TYPE field of the outer
   list.  That allows them to be quickly deleted, and requires no
   extra storage.

   Sort methods that are not special (i.e., constructors, destructors,
   and type conversion operators) so that we can find them faster in
   search.  */

static void
finish_struct_methods (t)
     tree t;
{
  tree fn_fields;
  tree method_vec;
  int slot, len;

  if (!TYPE_METHODS (t))
    {
      /* Clear these for safety; perhaps some parsing error could set
	 these incorrectly.  */
      TYPE_HAS_CONSTRUCTOR (t) = 0;
      TYPE_HAS_DESTRUCTOR (t) = 0;
      CLASSTYPE_METHOD_VEC (t) = NULL_TREE;
      return;
    }

  method_vec = CLASSTYPE_METHOD_VEC (t);
  my_friendly_assert (method_vec != NULL_TREE, 19991215);
  len = TREE_VEC_LENGTH (method_vec);

  /* First fill in entry 0 with the constructors, entry 1 with destructors,
     and the next few with type conversion operators (if any).  */
  for (fn_fields = TYPE_METHODS (t); fn_fields; 
       fn_fields = TREE_CHAIN (fn_fields))
    /* Clear out this flag.  */
    DECL_IN_AGGR_P (fn_fields) = 0;

  if (TYPE_HAS_DESTRUCTOR (t) && !CLASSTYPE_DESTRUCTORS (t))
    /* We thought there was a destructor, but there wasn't.  Some
       parse errors cause this anomalous situation.  */
    TYPE_HAS_DESTRUCTOR (t) = 0;
    
  /* Issue warnings about private constructors and such.  If there are
     no methods, then some public defaults are generated.  */
  maybe_warn_about_overly_private_class (t);

  /* Now sort the methods.  */
  while (len > 2 && TREE_VEC_ELT (method_vec, len-1) == NULL_TREE)
    len--;
  TREE_VEC_LENGTH (method_vec) = len;

  /* The type conversion ops have to live at the front of the vec, so we
     can't sort them.  */
  for (slot = 2; slot < len; ++slot)
    {
      tree fn = TREE_VEC_ELT (method_vec, slot);
  
      if (!DECL_CONV_FN_P (OVL_CURRENT (fn)))
	break;
    }
  if (len - slot > 1)
    qsort (&TREE_VEC_ELT (method_vec, slot), len-slot, sizeof (tree),
	   (int (*)(const void *, const void *))method_name_cmp);
}

/* Emit error when a duplicate definition of a type is seen.  Patch up.  */

void
duplicate_tag_error (t)
     tree t;
{
  error ("redefinition of `%#T'", t);
  cp_error_at ("previous definition of `%#T'", t);

  /* Pretend we haven't defined this type.  */

  /* All of the component_decl's were TREE_CHAINed together in the parser.
     finish_struct_methods walks these chains and assembles all methods with
     the same base name into DECL_CHAINs. Now we don't need the parser chains
     anymore, so we unravel them.  */

  /* This used to be in finish_struct, but it turns out that the
     TREE_CHAIN is used by dbxout_type_methods and perhaps some other
     things...  */
  if (CLASSTYPE_METHOD_VEC (t)) 
    {
      tree method_vec = CLASSTYPE_METHOD_VEC (t);
      int i, len  = TREE_VEC_LENGTH (method_vec);
      for (i = 0; i < len; i++)
	{
	  tree unchain = TREE_VEC_ELT (method_vec, i);
	  while (unchain != NULL_TREE) 
	    {
	      TREE_CHAIN (OVL_CURRENT (unchain)) = NULL_TREE;
	      unchain = OVL_NEXT (unchain);
	    }
	}
    }

  if (TYPE_LANG_SPECIFIC (t))
    {
      tree binfo = TYPE_BINFO (t);
      int interface_only = CLASSTYPE_INTERFACE_ONLY (t);
      int interface_unknown = CLASSTYPE_INTERFACE_UNKNOWN (t);
      tree template_info = CLASSTYPE_TEMPLATE_INFO (t);
      int use_template = CLASSTYPE_USE_TEMPLATE (t);

      memset ((char *) TYPE_LANG_SPECIFIC (t), 0, sizeof (struct lang_type));
      BINFO_BASETYPES(binfo) = NULL_TREE;

      TYPE_LANG_SPECIFIC (t)->u.h.is_lang_type_class = 1;
      TYPE_BINFO (t) = binfo;
      CLASSTYPE_INTERFACE_ONLY (t) = interface_only;
      SET_CLASSTYPE_INTERFACE_UNKNOWN_X (t, interface_unknown);
      TYPE_REDEFINED (t) = 1;
      CLASSTYPE_TEMPLATE_INFO (t) = template_info;
      CLASSTYPE_USE_TEMPLATE (t) = use_template;
      CLASSTYPE_DECL_LIST (t) = NULL_TREE;
    }
  TYPE_SIZE (t) = NULL_TREE;
  TYPE_MODE (t) = VOIDmode;
  TYPE_FIELDS (t) = NULL_TREE;
  TYPE_METHODS (t) = NULL_TREE;
  TYPE_VFIELD (t) = NULL_TREE;
  TYPE_CONTEXT (t) = NULL_TREE;
  
  /* Clear TYPE_LANG_FLAGS -- those in TYPE_LANG_SPECIFIC are cleared above.  */
  TYPE_LANG_FLAG_0 (t) = 0;
  TYPE_LANG_FLAG_1 (t) = 0;
  TYPE_LANG_FLAG_2 (t) = 0;
  TYPE_LANG_FLAG_3 (t) = 0;
  TYPE_LANG_FLAG_4 (t) = 0;
  TYPE_LANG_FLAG_5 (t) = 0;
  TYPE_LANG_FLAG_6 (t) = 0;
  /* But not this one.  */
  SET_IS_AGGR_TYPE (t, 1);
}

/* Make BINFO's vtable have N entries, including RTTI entries,
   vbase and vcall offsets, etc.  Set its type and call the backend
   to lay it out.  */

static void
layout_vtable_decl (binfo, n)
     tree binfo;
     int n;
{
  tree atype;
  tree vtable;

  atype = build_cplus_array_type (vtable_entry_type, 
				  build_index_type (size_int (n - 1)));
  layout_type (atype);

  /* We may have to grow the vtable.  */
  vtable = get_vtbl_decl_for_binfo (binfo);
  if (!same_type_p (TREE_TYPE (vtable), atype))
    {
      TREE_TYPE (vtable) = atype;
      DECL_SIZE (vtable) = DECL_SIZE_UNIT (vtable) = NULL_TREE;
      layout_decl (vtable, 0);

      /* At one time the vtable info was grabbed 2 words at a time.  This
	 fails on SPARC unless you have 8-byte alignment.  */
      DECL_ALIGN (vtable) = MAX (TYPE_ALIGN (double_type_node),
				 DECL_ALIGN (vtable));
    }
}

/* True iff FNDECL and BASE_FNDECL (both non-static member functions)
   have the same signature.  */

int
same_signature_p (fndecl, base_fndecl)
     tree fndecl, base_fndecl;
{
  /* One destructor overrides another if they are the same kind of
     destructor.  */
  if (DECL_DESTRUCTOR_P (base_fndecl) && DECL_DESTRUCTOR_P (fndecl)
      && special_function_p (base_fndecl) == special_function_p (fndecl))
    return 1;
  /* But a non-destructor never overrides a destructor, nor vice
     versa, nor do different kinds of destructors override
     one-another.  For example, a complete object destructor does not
     override a deleting destructor.  */
  if (DECL_DESTRUCTOR_P (base_fndecl) || DECL_DESTRUCTOR_P (fndecl))
    return 0;

  if (DECL_NAME (fndecl) == DECL_NAME (base_fndecl))
    {
      tree types, base_types;
      types = TYPE_ARG_TYPES (TREE_TYPE (fndecl));
      base_types = TYPE_ARG_TYPES (TREE_TYPE (base_fndecl));
      if ((TYPE_QUALS (TREE_TYPE (TREE_VALUE (base_types)))
	   == TYPE_QUALS (TREE_TYPE (TREE_VALUE (types))))
	  && compparms (TREE_CHAIN (base_types), TREE_CHAIN (types)))
	return 1;
    }
  return 0;
}

/* Called from base_derived_from via dfs_walk.  */

static tree
dfs_base_derived_from (tree binfo, void *data)
{
  tree base = (tree) data;

  if (same_type_p (TREE_TYPE (base), TREE_TYPE (binfo))
      && tree_int_cst_equal (BINFO_OFFSET (base), BINFO_OFFSET (binfo)))
    return error_mark_node;

  return NULL_TREE;
}

/* Returns TRUE if DERIVED is a binfo containing the binfo BASE as a
   subobject.  */
 
static bool
base_derived_from (tree derived, tree base)
{
  return dfs_walk (derived, dfs_base_derived_from, NULL, base) != NULL_TREE;
}

typedef struct find_final_overrider_data_s {
  /* The function for which we are trying to find a final overrider.  */
  tree fn;
  /* The base class in which the function was declared.  */
  tree declaring_base;
  /* The most derived class in the hierarchy.  */
  tree most_derived_type;
  /* The candidate overriders.  */
  tree candidates;
} find_final_overrider_data;

/* Called from find_final_overrider via dfs_walk.  */

static tree
dfs_find_final_overrider (binfo, data)
     tree binfo;
     void *data;
{
  find_final_overrider_data *ffod = (find_final_overrider_data *) data;

  if (same_type_p (BINFO_TYPE (binfo), 
		   BINFO_TYPE (ffod->declaring_base))
      && tree_int_cst_equal (BINFO_OFFSET (binfo),
			     BINFO_OFFSET (ffod->declaring_base)))
    {
      tree path;
      tree method;

      /* We haven't found an overrider yet.  */
      method = NULL_TREE;
      /* We've found a path to the declaring base.  Walk down the path
	 looking for an overrider for FN.  */
      path = reverse_path (binfo);
      while (!same_type_p (BINFO_TYPE (TREE_VALUE (path)),
			   ffod->most_derived_type))
	path = TREE_CHAIN (path);
      while (path)
	{
	  method = look_for_overrides_here (BINFO_TYPE (TREE_VALUE (path)),
					    ffod->fn);
	  if (method)
	    {
	      path = TREE_VALUE (path);
	      break;
	    }

	  path = TREE_CHAIN (path);
	}

      /* If we found an overrider, record the overriding function, and
	 the base from which it came.  */
      if (path)
	{
	  tree *candidate;

	  /* Remove any candidates overridden by this new function.  */
	  candidate = &ffod->candidates;
	  while (*candidate)
	    {
	      /* If *CANDIDATE overrides METHOD, then METHOD
		 cannot override anything else on the list.  */
	      if (base_derived_from (TREE_VALUE (*candidate), path))
		  return NULL_TREE;
	      /* If METHOD overrides *CANDIDATE, remove *CANDIDATE.  */
	      if (base_derived_from (path, TREE_VALUE (*candidate)))
		*candidate = TREE_CHAIN (*candidate);
	      else
		candidate = &TREE_CHAIN (*candidate);
	    }

	  /* Add the new function.  */
	  ffod->candidates = tree_cons (method, path, ffod->candidates);
	}
    }

  return NULL_TREE;
}

/* Returns a TREE_LIST whose TREE_PURPOSE is the final overrider for
   FN and whose TREE_VALUE is the binfo for the base where the
   overriding occurs.  BINFO (in the hierarchy dominated by the binfo
   DERIVED) is the base object in which FN is declared.  */

static tree
find_final_overrider (derived, binfo, fn)
     tree derived;
     tree binfo;
     tree fn;
{
  find_final_overrider_data ffod;

  /* Getting this right is a little tricky.  This is valid:

       struct S { virtual void f (); };
       struct T { virtual void f (); };
       struct U : public S, public T { };

     even though calling `f' in `U' is ambiguous.  But, 

       struct R { virtual void f(); };
       struct S : virtual public R { virtual void f (); };
       struct T : virtual public R { virtual void f (); };
       struct U : public S, public T { };

     is not -- there's no way to decide whether to put `S::f' or
     `T::f' in the vtable for `R'.  
     
     The solution is to look at all paths to BINFO.  If we find
     different overriders along any two, then there is a problem.  */
  ffod.fn = fn;
  ffod.declaring_base = binfo;
  ffod.most_derived_type = BINFO_TYPE (derived);
  ffod.candidates = NULL_TREE;

  dfs_walk (derived,
	    dfs_find_final_overrider,
	    NULL,
	    &ffod);

  /* If there was no winner, issue an error message.  */
  if (!ffod.candidates || TREE_CHAIN (ffod.candidates))
    {
      error ("no unique final overrider for `%D' in `%T'", fn, 
	     BINFO_TYPE (derived));
      return error_mark_node;
    }

  return ffod.candidates;
}

/* Return the index of the vcall offset for FN when TYPE is used as a
   virtual base.  */

static tree
get_vcall_index (tree fn, tree type)
{
  tree v;

  for (v = CLASSTYPE_VCALL_INDICES (type); v; v = TREE_CHAIN (v))
    if ((DECL_DESTRUCTOR_P (fn) && DECL_DESTRUCTOR_P (TREE_PURPOSE (v)))
	|| same_signature_p (fn, TREE_PURPOSE (v)))
      break;

  /* There should always be an appropriate index.  */
  my_friendly_assert (v, 20021103);

  return TREE_VALUE (v);
}

/* Update an entry in the vtable for BINFO, which is in the hierarchy
   dominated by T.  FN has been overriden in BINFO; VIRTUALS points to the
   corresponding position in the BINFO_VIRTUALS list.  */

static void
update_vtable_entry_for_fn (t, binfo, fn, virtuals)
     tree t;
     tree binfo;
     tree fn;
     tree *virtuals;
{
  tree b;
  tree overrider;
  tree delta;
  tree virtual_base;
  tree first_defn;
  bool lost = false;

  /* Find the nearest primary base (possibly binfo itself) which defines
     this function; this is the class the caller will convert to when
     calling FN through BINFO.  */
  for (b = binfo; ; b = get_primary_binfo (b))
    {
      if (look_for_overrides_here (BINFO_TYPE (b), fn))
	break;

      /* The nearest definition is from a lost primary.  */
      if (BINFO_LOST_PRIMARY_P (b))
	lost = true;
    }
  first_defn = b;

  /* Find the final overrider.  */
  overrider = find_final_overrider (TYPE_BINFO (t), b, fn);
  if (overrider == error_mark_node)
    return;

  /* Check for unsupported covariant returns again now that we've
     calculated the base offsets.  */
  check_final_overrider (TREE_PURPOSE (overrider), fn);

  /* Assume that we will produce a thunk that convert all the way to
     the final overrider, and not to an intermediate virtual base.  */
  virtual_base = NULL_TREE;

  /* See if we can convert to an intermediate virtual base first, and then
     use the vcall offset located there to finish the conversion.  */
  for (; b; b = BINFO_INHERITANCE_CHAIN (b))
    {
      /* If we find the final overrider, then we can stop
	 walking.  */
      if (same_type_p (BINFO_TYPE (b), 
		       BINFO_TYPE (TREE_VALUE (overrider))))
	break;

      /* If we find a virtual base, and we haven't yet found the
	 overrider, then there is a virtual base between the
	 declaring base (first_defn) and the final overrider.  */
      if (!virtual_base && TREE_VIA_VIRTUAL (b))
	virtual_base = b;
    }

  /* Compute the constant adjustment to the `this' pointer.  The
     `this' pointer, when this function is called, will point at BINFO
     (or one of its primary bases, which are at the same offset).  */
  if (virtual_base)
    /* The `this' pointer needs to be adjusted from the declaration to
       the nearest virtual base.  */
    delta = size_diffop (BINFO_OFFSET (virtual_base),
			 BINFO_OFFSET (first_defn));
  else if (lost)
    /* If the nearest definition is in a lost primary, we don't need an
       entry in our vtable.  Except possibly in a constructor vtable,
       if we happen to get our primary back.  In that case, the offset
       will be zero, as it will be a primary base.  */
    delta = size_zero_node;
  else
    /* The `this' pointer needs to be adjusted from pointing to
       BINFO to pointing at the base where the final overrider
       appears.  */
    delta = size_diffop (BINFO_OFFSET (TREE_VALUE (overrider)),
			 BINFO_OFFSET (binfo));

  modify_vtable_entry (t, 
		       binfo, 
		       TREE_PURPOSE (overrider),
		       delta,
		       virtuals);

  if (virtual_base)
    BV_VCALL_INDEX (*virtuals) 
      = get_vcall_index (TREE_PURPOSE (overrider),
			 BINFO_TYPE (virtual_base));
}

/* Called from modify_all_vtables via dfs_walk.  */

static tree
dfs_modify_vtables (binfo, data)
     tree binfo;
     void *data;
{
  if (/* There's no need to modify the vtable for a non-virtual
         primary base; we're not going to use that vtable anyhow.
	 We do still need to do this for virtual primary bases, as they
	 could become non-primary in a construction vtable.  */
      (!BINFO_PRIMARY_P (binfo) || TREE_VIA_VIRTUAL (binfo))
      /* Similarly, a base without a vtable needs no modification.  */
      && CLASSTYPE_VFIELDS (BINFO_TYPE (binfo)))
    {
      tree t;
      tree virtuals;
      tree old_virtuals;

      t = (tree) data;

      make_new_vtable (t, binfo);
      
      /* Now, go through each of the virtual functions in the virtual
	 function table for BINFO.  Find the final overrider, and
	 update the BINFO_VIRTUALS list appropriately.  */
      for (virtuals = BINFO_VIRTUALS (binfo),
	     old_virtuals = BINFO_VIRTUALS (TYPE_BINFO (BINFO_TYPE (binfo)));
	   virtuals;
	   virtuals = TREE_CHAIN (virtuals),
	     old_virtuals = TREE_CHAIN (old_virtuals))
	update_vtable_entry_for_fn (t, 
				    binfo, 
				    BV_FN (old_virtuals),
				    &virtuals);
    }

  SET_BINFO_MARKED (binfo);

  return NULL_TREE;
}

/* Update all of the primary and secondary vtables for T.  Create new
   vtables as required, and initialize their RTTI information.  Each
   of the functions in VIRTUALS is declared in T and may override a
   virtual function from a base class; find and modify the appropriate
   entries to point to the overriding functions.  Returns a list, in
   declaration order, of the virtual functions that are declared in T,
   but do not appear in the primary base class vtable, and which
   should therefore be appended to the end of the vtable for T.  */

static tree
modify_all_vtables (t, virtuals)
     tree t;
     tree virtuals;
{
  tree binfo = TYPE_BINFO (t);
  tree *fnsp;

  /* Update all of the vtables.  */
  dfs_walk (binfo, 
	    dfs_modify_vtables, 
	    dfs_unmarked_real_bases_queue_p,
	    t);
  dfs_walk (binfo, dfs_unmark, dfs_marked_real_bases_queue_p, t);

  /* Add virtual functions not already in our primary vtable. These
     will be both those introduced by this class, and those overridden
     from secondary bases.  It does not include virtuals merely
     inherited from secondary bases.  */
  for (fnsp = &virtuals; *fnsp; )
    {
      tree fn = TREE_VALUE (*fnsp);

      if (!value_member (fn, BINFO_VIRTUALS (binfo))
	  || DECL_VINDEX (fn) == error_mark_node)
	{
	  /* We don't need to adjust the `this' pointer when
	     calling this function.  */
	  BV_DELTA (*fnsp) = integer_zero_node;
	  BV_VCALL_INDEX (*fnsp) = NULL_TREE;

	  /* This is a function not already in our vtable.  Keep it.  */
	  fnsp = &TREE_CHAIN (*fnsp);
	}
      else
	/* We've already got an entry for this function.  Skip it.  */
	*fnsp = TREE_CHAIN (*fnsp);
    }

  return virtuals;
}

/* Get the base virtual function declarations in T that have the
   indicated NAME.  */

static tree
get_basefndecls (name, t)
     tree name, t;
{
  tree methods;
  tree base_fndecls = NULL_TREE;
  int n_baseclasses = CLASSTYPE_N_BASECLASSES (t);
  int i;

  /* Find virtual functions in T with the indicated NAME.  */
  i = lookup_fnfields_1 (t, name);
  if (i != -1)
    for (methods = TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (t), i);
	 methods;
	 methods = OVL_NEXT (methods))
      {
	tree method = OVL_CURRENT (methods);

	if (TREE_CODE (method) == FUNCTION_DECL
	    && DECL_VINDEX (method))
	  base_fndecls = tree_cons (NULL_TREE, method, base_fndecls);
      }

  if (base_fndecls)
    return base_fndecls;

  for (i = 0; i < n_baseclasses; i++)
    {
      tree basetype = TYPE_BINFO_BASETYPE (t, i);
      base_fndecls = chainon (get_basefndecls (name, basetype),
			      base_fndecls);
    }

  return base_fndecls;
}

/* If this declaration supersedes the declaration of
   a method declared virtual in the base class, then
   mark this field as being virtual as well.  */

static void
check_for_override (decl, ctype)
     tree decl, ctype;
{
  if (TREE_CODE (decl) == TEMPLATE_DECL)
    /* In [temp.mem] we have:

         A specialization of a member function template does not
         override a virtual function from a base class.  */
    return;
  if ((DECL_DESTRUCTOR_P (decl)
       || IDENTIFIER_VIRTUAL_P (DECL_NAME (decl)))
      && look_for_overrides (ctype, decl)
      && !DECL_STATIC_FUNCTION_P (decl))
    /* Set DECL_VINDEX to a value that is neither an INTEGER_CST nor
       the error_mark_node so that we know it is an overriding
       function.  */
    DECL_VINDEX (decl) = decl;

  if (DECL_VIRTUAL_P (decl))
    {
      if (!DECL_VINDEX (decl))
	DECL_VINDEX (decl) = error_mark_node;
      IDENTIFIER_VIRTUAL_P (DECL_NAME (decl)) = 1;
    }
}

/* Warn about hidden virtual functions that are not overridden in t.
   We know that constructors and destructors don't apply.  */

void
warn_hidden (t)
     tree t;
{
  tree method_vec = CLASSTYPE_METHOD_VEC (t);
  int n_methods = method_vec ? TREE_VEC_LENGTH (method_vec) : 0;
  int i;

  /* We go through each separately named virtual function.  */
  for (i = 2; i < n_methods && TREE_VEC_ELT (method_vec, i); ++i)
    {
      tree fns;
      tree name;
      tree fndecl;
      tree base_fndecls;
      int j;

      /* All functions in this slot in the CLASSTYPE_METHOD_VEC will
	 have the same name.  Figure out what name that is.  */
      name = DECL_NAME (OVL_CURRENT (TREE_VEC_ELT (method_vec, i)));
      /* There are no possibly hidden functions yet.  */
      base_fndecls = NULL_TREE;
      /* Iterate through all of the base classes looking for possibly
	 hidden functions.  */
      for (j = 0; j < CLASSTYPE_N_BASECLASSES (t); j++)
	{
	  tree basetype = TYPE_BINFO_BASETYPE (t, j);
	  base_fndecls = chainon (get_basefndecls (name, basetype),
				  base_fndecls);
	}

      /* If there are no functions to hide, continue.  */
      if (!base_fndecls)
	continue;

      /* Remove any overridden functions.  */
      for (fns = TREE_VEC_ELT (method_vec, i); fns; fns = OVL_NEXT (fns))
	{
	  fndecl = OVL_CURRENT (fns);
	  if (DECL_VINDEX (fndecl))
	    {
	      tree *prev = &base_fndecls;
	      
	      while (*prev) 
		/* If the method from the base class has the same
		   signature as the method from the derived class, it
		   has been overridden.  */
		if (same_signature_p (fndecl, TREE_VALUE (*prev)))
		  *prev = TREE_CHAIN (*prev);
		else
		  prev = &TREE_CHAIN (*prev);
	    }
	}

      /* Now give a warning for all base functions without overriders,
	 as they are hidden.  */
      while (base_fndecls) 
	{
	  /* Here we know it is a hider, and no overrider exists.  */
	  cp_warning_at ("`%D' was hidden", TREE_VALUE (base_fndecls));
	  cp_warning_at ("  by `%D'", 
			 OVL_CURRENT (TREE_VEC_ELT (method_vec, i)));
	  base_fndecls = TREE_CHAIN (base_fndecls);
	}
    }
}

/* Check for things that are invalid.  There are probably plenty of other
   things we should check for also.  */

static void
finish_struct_anon (t)
     tree t;
{
  tree field;

  for (field = TYPE_FIELDS (t); field; field = TREE_CHAIN (field))
    {
      if (TREE_STATIC (field))
	continue;
      if (TREE_CODE (field) != FIELD_DECL)
	continue;

      if (DECL_NAME (field) == NULL_TREE
	  && ANON_AGGR_TYPE_P (TREE_TYPE (field)))
	{
	  tree elt = TYPE_FIELDS (TREE_TYPE (field));
	  for (; elt; elt = TREE_CHAIN (elt))
	    {
	      /* We're generally only interested in entities the user
		 declared, but we also find nested classes by noticing
		 the TYPE_DECL that we create implicitly.  You're
		 allowed to put one anonymous union inside another,
		 though, so we explicitly tolerate that.  We use
		 TYPE_ANONYMOUS_P rather than ANON_AGGR_TYPE_P so that
		 we also allow unnamed types used for defining fields.  */
	      if (DECL_ARTIFICIAL (elt) 
		  && (!DECL_IMPLICIT_TYPEDEF_P (elt)
		      || TYPE_ANONYMOUS_P (TREE_TYPE (elt))))
		continue;

	      if (DECL_NAME (elt) == constructor_name (t))
		cp_pedwarn_at ("ISO C++ forbids member `%D' with same name as enclosing class",
			       elt);

	      if (TREE_CODE (elt) != FIELD_DECL)
		{
		  cp_pedwarn_at ("`%#D' invalid; an anonymous union can only have non-static data members",
				 elt);
		  continue;
		}

	      if (TREE_PRIVATE (elt))
		cp_pedwarn_at ("private member `%#D' in anonymous union",
			       elt);
	      else if (TREE_PROTECTED (elt))
		cp_pedwarn_at ("protected member `%#D' in anonymous union",
			       elt);

	      TREE_PRIVATE (elt) = TREE_PRIVATE (field);
	      TREE_PROTECTED (elt) = TREE_PROTECTED (field);
	    }
	}
    }
}

/* Add T to CLASSTYPE_DECL_LIST of current_class_type which
   will be used later during class template instantiation.
   When FRIEND_P is zero, T can be a static member data (VAR_DECL),
   a non-static member data (FIELD_DECL), a member function
   (FUNCTION_DECL), a nested type (RECORD_TYPE, ENUM_TYPE), 
   a typedef (TYPE_DECL) or a member class template (TEMPLATE_DECL)
   When FRIEND_P is nonzero, T is either a friend class
   (RECORD_TYPE, TEMPLATE_DECL) or a friend function
   (FUNCTION_DECL, TEMPLATE_DECL).  */

void
maybe_add_class_template_decl_list (type, t, friend_p)
     tree type;
     tree t;
     int friend_p;
{
  /* Save some memory by not creating TREE_LIST if TYPE is not template.  */
  if (CLASSTYPE_TEMPLATE_INFO (type))
    CLASSTYPE_DECL_LIST (type)
      = tree_cons (friend_p ? NULL_TREE : type,
		   t, CLASSTYPE_DECL_LIST (type));
}

/* Create default constructors, assignment operators, and so forth for
   the type indicated by T, if they are needed.
   CANT_HAVE_DEFAULT_CTOR, CANT_HAVE_CONST_CTOR, and
   CANT_HAVE_CONST_ASSIGNMENT are nonzero if, for whatever reason, the
   class cannot have a default constructor, copy constructor taking a
   const reference argument, or an assignment operator taking a const
   reference, respectively.  If a virtual destructor is created, its
   DECL is returned; otherwise the return value is NULL_TREE.  */

static void
add_implicitly_declared_members (t, cant_have_default_ctor,
				 cant_have_const_cctor,
				 cant_have_const_assignment)
     tree t;
     int cant_have_default_ctor;
     int cant_have_const_cctor;
     int cant_have_const_assignment;
{
  tree default_fn;
  tree implicit_fns = NULL_TREE;
  tree virtual_dtor = NULL_TREE;
  tree *f;

  ++adding_implicit_members;

  /* Destructor.  */
  if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (t) && !TYPE_HAS_DESTRUCTOR (t))
    {
      default_fn = implicitly_declare_fn (sfk_destructor, t, /*const_p=*/0);
      check_for_override (default_fn, t);

      /* If we couldn't make it work, then pretend we didn't need it.  */
      if (default_fn == void_type_node)
	TYPE_HAS_NONTRIVIAL_DESTRUCTOR (t) = 0;
      else
	{
	  TREE_CHAIN (default_fn) = implicit_fns;
	  implicit_fns = default_fn;

	  if (DECL_VINDEX (default_fn))
	    virtual_dtor = default_fn;
	}
    }
  else
    /* Any non-implicit destructor is non-trivial.  */
    TYPE_HAS_NONTRIVIAL_DESTRUCTOR (t) |= TYPE_HAS_DESTRUCTOR (t);

  /* Default constructor.  */
  if (! TYPE_HAS_CONSTRUCTOR (t) && ! cant_have_default_ctor)
    {
      default_fn = implicitly_declare_fn (sfk_constructor, t, /*const_p=*/0);
      TREE_CHAIN (default_fn) = implicit_fns;
      implicit_fns = default_fn;
    }

  /* Copy constructor.  */
  if (! TYPE_HAS_INIT_REF (t) && ! TYPE_FOR_JAVA (t))
    {
      /* ARM 12.18: You get either X(X&) or X(const X&), but
	 not both.  --Chip  */
      default_fn 
	= implicitly_declare_fn (sfk_copy_constructor, t,
				 /*const_p=*/!cant_have_const_cctor);
      TREE_CHAIN (default_fn) = implicit_fns;
      implicit_fns = default_fn;
    }

  /* Assignment operator.  */
  if (! TYPE_HAS_ASSIGN_REF (t) && ! TYPE_FOR_JAVA (t))
    {
      default_fn 
	= implicitly_declare_fn (sfk_assignment_operator, t,
				 /*const_p=*/!cant_have_const_assignment);
      TREE_CHAIN (default_fn) = implicit_fns;
      implicit_fns = default_fn;
    }

  /* Now, hook all of the new functions on to TYPE_METHODS,
     and add them to the CLASSTYPE_METHOD_VEC.  */
  for (f = &implicit_fns; *f; f = &TREE_CHAIN (*f))
    {
      add_method (t, *f, /*error_p=*/0);
      maybe_add_class_template_decl_list (current_class_type, *f, /*friend_p=*/0);
    }
  if (abi_version_at_least (2))
    /* G++ 3.2 put the implicit destructor at the *beginning* of the
       list, which cause the destructor to be emitted in an incorrect
       location in the vtable.  */
    TYPE_METHODS (t) = chainon (TYPE_METHODS (t), implicit_fns);
  else
    {
      if (warn_abi && virtual_dtor)
	warning ("vtable layout for class `%T' may not be ABI-compliant "
		 "and may change in a future version of GCC due to implicit "
		 "virtual destructor",
		 t);
      *f = TYPE_METHODS (t);
      TYPE_METHODS (t) = implicit_fns;
    }

  --adding_implicit_members;
}

/* Subroutine of finish_struct_1.  Recursively count the number of fields
   in TYPE, including anonymous union members.  */

static int
count_fields (fields)
     tree fields;
{
  tree x;
  int n_fields = 0;
  for (x = fields; x; x = TREE_CHAIN (x))
    {
      if (TREE_CODE (x) == FIELD_DECL && ANON_AGGR_TYPE_P (TREE_TYPE (x)))
	n_fields += count_fields (TYPE_FIELDS (TREE_TYPE (x)));
      else
	n_fields += 1;
    }
  return n_fields;
}

/* Subroutine of finish_struct_1.  Recursively add all the fields in the
   TREE_LIST FIELDS to the TREE_VEC FIELD_VEC, starting at offset IDX.  */

static int
add_fields_to_vec (fields, field_vec, idx)
     tree fields, field_vec;
     int idx;
{
  tree x;
  for (x = fields; x; x = TREE_CHAIN (x))
    {
      if (TREE_CODE (x) == FIELD_DECL && ANON_AGGR_TYPE_P (TREE_TYPE (x)))
	idx = add_fields_to_vec (TYPE_FIELDS (TREE_TYPE (x)), field_vec, idx);
      else
	TREE_VEC_ELT (field_vec, idx++) = x;
    }
  return idx;
}

/* FIELD is a bit-field.  We are finishing the processing for its
   enclosing type.  Issue any appropriate messages and set appropriate
   flags.  */

static void
check_bitfield_decl (field)
     tree field;
{
  tree type = TREE_TYPE (field);
  tree w = NULL_TREE;

  /* Detect invalid bit-field type.  */
  if (DECL_INITIAL (field)
      && ! INTEGRAL_TYPE_P (TREE_TYPE (field)))
    {
      cp_error_at ("bit-field `%#D' with non-integral type", field);
      w = error_mark_node;
    }

  /* Detect and ignore out of range field width.  */
  if (DECL_INITIAL (field))
    {
      w = DECL_INITIAL (field);

      /* Avoid the non_lvalue wrapper added by fold for PLUS_EXPRs.  */
      STRIP_NOPS (w);

      /* detect invalid field size.  */
      if (TREE_CODE (w) == CONST_DECL)
	w = DECL_INITIAL (w);
      else
	w = decl_constant_value (w);

      if (TREE_CODE (w) != INTEGER_CST)
	{
	  cp_error_at ("bit-field `%D' width not an integer constant",
		       field);
	  w = error_mark_node;
	}
      else if (tree_int_cst_sgn (w) < 0)
	{
	  cp_error_at ("negative width in bit-field `%D'", field);
	  w = error_mark_node;
	}
      else if (integer_zerop (w) && DECL_NAME (field) != 0)
	{
	  cp_error_at ("zero width for bit-field `%D'", field);
	  w = error_mark_node;
	}
      else if (compare_tree_int (w, TYPE_PRECISION (type)) > 0
	       && TREE_CODE (type) != ENUMERAL_TYPE
	       && TREE_CODE (type) != BOOLEAN_TYPE)
	cp_warning_at ("width of `%D' exceeds its type", field);
      else if (TREE_CODE (type) == ENUMERAL_TYPE
	       && (0 > compare_tree_int (w,
					 min_precision (TYPE_MIN_VALUE (type),
							TREE_UNSIGNED (type)))
		   ||  0 > compare_tree_int (w,
					     min_precision
					     (TYPE_MAX_VALUE (type),
					      TREE_UNSIGNED (type)))))
	cp_warning_at ("`%D' is too small to hold all values of `%#T'",
		       field, type);
    }
  
  /* Remove the bit-field width indicator so that the rest of the
     compiler does not treat that value as an initializer.  */
  DECL_INITIAL (field) = NULL_TREE;

  if (w != error_mark_node)
    {
      DECL_SIZE (field) = convert (bitsizetype, w);
      DECL_BIT_FIELD (field) = 1;

      if (integer_zerop (w)
	  && ! (* targetm.ms_bitfield_layout_p) (DECL_FIELD_CONTEXT (field)))
	{
#ifdef EMPTY_FIELD_BOUNDARY
	  DECL_ALIGN (field) = MAX (DECL_ALIGN (field), 
				    EMPTY_FIELD_BOUNDARY);
#endif
#ifdef PCC_BITFIELD_TYPE_MATTERS
	  if (PCC_BITFIELD_TYPE_MATTERS)
	    {
	      DECL_ALIGN (field) = MAX (DECL_ALIGN (field), 
					TYPE_ALIGN (type));
	      DECL_USER_ALIGN (field) |= TYPE_USER_ALIGN (type);
	    }
#endif
	}
    }
  else
    {
      /* Non-bit-fields are aligned for their type.  */
      DECL_BIT_FIELD (field) = 0;
      CLEAR_DECL_C_BIT_FIELD (field);
      DECL_ALIGN (field) = MAX (DECL_ALIGN (field), TYPE_ALIGN (type));
      DECL_USER_ALIGN (field) |= TYPE_USER_ALIGN (type);
    }
}

/* FIELD is a non bit-field.  We are finishing the processing for its
   enclosing type T.  Issue any appropriate messages and set appropriate
   flags.  */

static void
check_field_decl (field, t, cant_have_const_ctor,
		  cant_have_default_ctor, no_const_asn_ref,
		  any_default_members)
     tree field;
     tree t;
     int *cant_have_const_ctor;
     int *cant_have_default_ctor;
     int *no_const_asn_ref;
     int *any_default_members;
{
  tree type = strip_array_types (TREE_TYPE (field));

  /* An anonymous union cannot contain any fields which would change
     the settings of CANT_HAVE_CONST_CTOR and friends.  */
  if (ANON_UNION_TYPE_P (type))
    ;
  /* And, we don't set TYPE_HAS_CONST_INIT_REF, etc., for anonymous
     structs.  So, we recurse through their fields here.  */
  else if (ANON_AGGR_TYPE_P (type))
    {
      tree fields;

      for (fields = TYPE_FIELDS (type); fields; fields = TREE_CHAIN (fields))
	if (TREE_CODE (fields) == FIELD_DECL && !DECL_C_BIT_FIELD (field))
	  check_field_decl (fields, t, cant_have_const_ctor,
			    cant_have_default_ctor, no_const_asn_ref,
			    any_default_members);
    }
  /* Check members with class type for constructors, destructors,
     etc.  */
  else if (CLASS_TYPE_P (type))
    {
      /* Never let anything with uninheritable virtuals
	 make it through without complaint.  */
      abstract_virtuals_error (field, type);
		      
      if (TREE_CODE (t) == UNION_TYPE)
	{
	  if (TYPE_NEEDS_CONSTRUCTING (type))
	    cp_error_at ("member `%#D' with constructor not allowed in union",
			 field);
	  if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (type))
	    cp_error_at ("member `%#D' with destructor not allowed in union",
			 field);
	  if (TYPE_HAS_COMPLEX_ASSIGN_REF (type))
	    cp_error_at ("member `%#D' with copy assignment operator not allowed in union",
			 field);
	}
      else
	{
	  TYPE_NEEDS_CONSTRUCTING (t) |= TYPE_NEEDS_CONSTRUCTING (type);
	  TYPE_HAS_NONTRIVIAL_DESTRUCTOR (t) 
	    |= TYPE_HAS_NONTRIVIAL_DESTRUCTOR (type);
	  TYPE_HAS_COMPLEX_ASSIGN_REF (t) |= TYPE_HAS_COMPLEX_ASSIGN_REF (type);
	  TYPE_HAS_COMPLEX_INIT_REF (t) |= TYPE_HAS_COMPLEX_INIT_REF (type);
	}

      if (!TYPE_HAS_CONST_INIT_REF (type))
	*cant_have_const_ctor = 1;

      if (!TYPE_HAS_CONST_ASSIGN_REF (type))
	*no_const_asn_ref = 1;

      if (TYPE_HAS_CONSTRUCTOR (type)
	  && ! TYPE_HAS_DEFAULT_CONSTRUCTOR (type))
	*cant_have_default_ctor = 1;
    }
  if (DECL_INITIAL (field) != NULL_TREE)
    {
      /* `build_class_init_list' does not recognize
	 non-FIELD_DECLs.  */
      if (TREE_CODE (t) == UNION_TYPE && any_default_members != 0)
	cp_error_at ("multiple fields in union `%T' initialized");
      *any_default_members = 1;
    }
}

/* Check the data members (both static and non-static), class-scoped
   typedefs, etc., appearing in the declaration of T.  Issue
   appropriate diagnostics.  Sets ACCESS_DECLS to a list (in
   declaration order) of access declarations; each TREE_VALUE in this
   list is a USING_DECL.

   In addition, set the following flags:

     EMPTY_P
       The class is empty, i.e., contains no non-static data members.

     CANT_HAVE_DEFAULT_CTOR_P
       This class cannot have an implicitly generated default
       constructor.

     CANT_HAVE_CONST_CTOR_P
       This class cannot have an implicitly generated copy constructor
       taking a const reference.

     CANT_HAVE_CONST_ASN_REF
       This class cannot have an implicitly generated assignment
       operator taking a const reference.

   All of these flags should be initialized before calling this
   function.

   Returns a pointer to the end of the TYPE_FIELDs chain; additional
   fields can be added by adding to this chain.  */

static void
check_field_decls (tree t, tree *access_decls,
		   int *cant_have_default_ctor_p, 
		   int *cant_have_const_ctor_p,
		   int *no_const_asn_ref_p)
{
  tree *field;
  tree *next;
  int has_pointers;
  int any_default_members;

  /* First, delete any duplicate fields.  */
  delete_duplicate_fields (TYPE_FIELDS (t));

  /* Assume there are no access declarations.  */
  *access_decls = NULL_TREE;
  /* Assume this class has no pointer members.  */
  has_pointers = 0;
  /* Assume none of the members of this class have default
     initializations.  */
  any_default_members = 0;

  for (field = &TYPE_FIELDS (t); *field; field = next)
    {
      tree x = *field;
      tree type = TREE_TYPE (x);

      next = &TREE_CHAIN (x);

      if (TREE_CODE (x) == FIELD_DECL)
	{
	  DECL_PACKED (x) |= TYPE_PACKED (t);

	  if (DECL_C_BIT_FIELD (x) && integer_zerop (DECL_INITIAL (x)))
	    /* We don't treat zero-width bitfields as making a class
	       non-empty.  */
	    ;
	  else
	    {
	      tree element_type;

	      /* The class is non-empty.  */
	      CLASSTYPE_EMPTY_P (t) = 0;
	      /* The class is not even nearly empty.  */
	      CLASSTYPE_NEARLY_EMPTY_P (t) = 0;
	      /* If one of the data members contains an empty class,
		 so does T.  */
	      element_type = strip_array_types (type);
	      if (CLASS_TYPE_P (element_type) 
		  && CLASSTYPE_CONTAINS_EMPTY_CLASS_P (element_type))
		CLASSTYPE_CONTAINS_EMPTY_CLASS_P (t) = 1;
	    }
	}

      if (TREE_CODE (x) == USING_DECL)
	{
	  /* Prune the access declaration from the list of fields.  */
	  *field = TREE_CHAIN (x);

	  /* Save the access declarations for our caller.  */
	  *access_decls = tree_cons (NULL_TREE, x, *access_decls);

	  /* Since we've reset *FIELD there's no reason to skip to the
	     next field.  */
	  next = field;
	  continue;
	}

      if (TREE_CODE (x) == TYPE_DECL
	  || TREE_CODE (x) == TEMPLATE_DECL)
	continue;

      /* If we've gotten this far, it's a data member, possibly static,
	 or an enumerator.  */

      DECL_CONTEXT (x) = t;

      /* ``A local class cannot have static data members.'' ARM 9.4 */
      if (current_function_decl && TREE_STATIC (x))
	cp_error_at ("field `%D' in local class cannot be static", x);

      /* Perform error checking that did not get done in
	 grokdeclarator.  */
      if (TREE_CODE (type) == FUNCTION_TYPE)
	{
	  cp_error_at ("field `%D' invalidly declared function type",
		       x);
	  type = build_pointer_type (type);
	  TREE_TYPE (x) = type;
	}
      else if (TREE_CODE (type) == METHOD_TYPE)
	{
	  cp_error_at ("field `%D' invalidly declared method type", x);
	  type = build_pointer_type (type);
	  TREE_TYPE (x) = type;
	}
      else if (TREE_CODE (type) == OFFSET_TYPE)
	{
	  cp_error_at ("field `%D' invalidly declared offset type", x);
	  type = build_pointer_type (type);
	  TREE_TYPE (x) = type;
	}

      if (type == error_mark_node)
	continue;
	  
      /* When this goes into scope, it will be a non-local reference.  */
      DECL_NONLOCAL (x) = 1;

      if (TREE_CODE (x) == CONST_DECL)
	continue;

      if (TREE_CODE (x) == VAR_DECL)
	{
	  if (TREE_CODE (t) == UNION_TYPE)
	    /* Unions cannot have static members.  */
	    cp_error_at ("field `%D' declared static in union", x);
	      
	  continue;
	}

      /* Now it can only be a FIELD_DECL.  */

      if (TREE_PRIVATE (x) || TREE_PROTECTED (x))
	CLASSTYPE_NON_AGGREGATE (t) = 1;

      /* If this is of reference type, check if it needs an init.
	 Also do a little ANSI jig if necessary.  */
      if (TREE_CODE (type) == REFERENCE_TYPE)
 	{
	  CLASSTYPE_NON_POD_P (t) = 1;
	  if (DECL_INITIAL (x) == NULL_TREE)
	    SET_CLASSTYPE_REF_FIELDS_NEED_INIT (t, 1);

	  /* ARM $12.6.2: [A member initializer list] (or, for an
	     aggregate, initialization by a brace-enclosed list) is the
	     only way to initialize nonstatic const and reference
	     members.  */
	  *cant_have_default_ctor_p = 1;
	  TYPE_HAS_COMPLEX_ASSIGN_REF (t) = 1;

	  if (! TYPE_HAS_CONSTRUCTOR (t) && CLASSTYPE_NON_AGGREGATE (t)
	      && extra_warnings)
            cp_warning_at ("non-static reference `%#D' in class without a constructor", x);
	}

      type = strip_array_types (type);
      
      if (TREE_CODE (type) == POINTER_TYPE)
	has_pointers = 1;

      if (DECL_MUTABLE_P (x) || TYPE_HAS_MUTABLE_P (type))
	CLASSTYPE_HAS_MUTABLE (t) = 1;

      if (! pod_type_p (type))
        /* DR 148 now allows pointers to members (which are POD themselves),
           to be allowed in POD structs.  */
	CLASSTYPE_NON_POD_P (t) = 1;

      if (! zero_init_p (type))
	CLASSTYPE_NON_ZERO_INIT_P (t) = 1;

      /* If any field is const, the structure type is pseudo-const.  */
      if (CP_TYPE_CONST_P (type))
	{
	  C_TYPE_FIELDS_READONLY (t) = 1;
	  if (DECL_INITIAL (x) == NULL_TREE)
	    SET_CLASSTYPE_READONLY_FIELDS_NEED_INIT (t, 1);

	  /* ARM $12.6.2: [A member initializer list] (or, for an
	     aggregate, initialization by a brace-enclosed list) is the
	     only way to initialize nonstatic const and reference
	     members.  */
	  *cant_have_default_ctor_p = 1;
	  TYPE_HAS_COMPLEX_ASSIGN_REF (t) = 1;

	  if (! TYPE_HAS_CONSTRUCTOR (t) && CLASSTYPE_NON_AGGREGATE (t)
	      && extra_warnings)
            cp_warning_at ("non-static const member `%#D' in class without a constructor", x);
	}
      /* A field that is pseudo-const makes the structure likewise.  */
      else if (IS_AGGR_TYPE (type))
	{
	  C_TYPE_FIELDS_READONLY (t) |= C_TYPE_FIELDS_READONLY (type);
	  SET_CLASSTYPE_READONLY_FIELDS_NEED_INIT (t,
	    CLASSTYPE_READONLY_FIELDS_NEED_INIT (t)
	    | CLASSTYPE_READONLY_FIELDS_NEED_INIT (type));
	}

      /* Core issue 80: A nonstatic data member is required to have a
	 different name from the class iff the class has a
	 user-defined constructor.  */
      if (DECL_NAME (x) == constructor_name (t)
	  && TYPE_HAS_CONSTRUCTOR (t))
	cp_pedwarn_at ("field `%#D' with same name as class", x);

      /* We set DECL_C_BIT_FIELD in grokbitfield.
	 If the type and width are valid, we'll also set DECL_BIT_FIELD.  */
      if (DECL_C_BIT_FIELD (x))
	check_bitfield_decl (x);
      else
	check_field_decl (x, t,
			  cant_have_const_ctor_p,
			  cant_have_default_ctor_p, 
			  no_const_asn_ref_p,
			  &any_default_members);
    }

  /* Effective C++ rule 11.  */
  if (has_pointers && warn_ecpp && TYPE_HAS_CONSTRUCTOR (t)
      && ! (TYPE_HAS_INIT_REF (t) && TYPE_HAS_ASSIGN_REF (t)))
    {
      warning ("`%#T' has pointer data members", t);
      
      if (! TYPE_HAS_INIT_REF (t))
	{
	  warning ("  but does not override `%T(const %T&)'", t, t);
	  if (! TYPE_HAS_ASSIGN_REF (t))
	    warning ("  or `operator=(const %T&)'", t);
	}
      else if (! TYPE_HAS_ASSIGN_REF (t))
	warning ("  but does not override `operator=(const %T&)'", t);
    }


  /* Check anonymous struct/anonymous union fields.  */
  finish_struct_anon (t);

  /* We've built up the list of access declarations in reverse order.
     Fix that now.  */
  *access_decls = nreverse (*access_decls);
}

/* If TYPE is an empty class type, records its OFFSET in the table of
   OFFSETS.  */

static int
record_subobject_offset (type, offset, offsets)
     tree type;
     tree offset;
     splay_tree offsets;
{
  splay_tree_node n;

  if (!is_empty_class (type))
    return 0;

  /* Record the location of this empty object in OFFSETS.  */
  n = splay_tree_lookup (offsets, (splay_tree_key) offset);
  if (!n)
    n = splay_tree_insert (offsets, 
			   (splay_tree_key) offset,
			   (splay_tree_value) NULL_TREE);
  n->value = ((splay_tree_value) 
	      tree_cons (NULL_TREE,
			 type,
			 (tree) n->value));

  return 0;
}

/* Returns nonzero if TYPE is an empty class type and there is
   already an entry in OFFSETS for the same TYPE as the same OFFSET.  */

static int
check_subobject_offset (type, offset, offsets)
     tree type;
     tree offset;
     splay_tree offsets;
{
  splay_tree_node n;
  tree t;

  if (!is_empty_class (type))
    return 0;

  /* Record the location of this empty object in OFFSETS.  */
  n = splay_tree_lookup (offsets, (splay_tree_key) offset);
  if (!n)
    return 0;

  for (t = (tree) n->value; t; t = TREE_CHAIN (t))
    if (same_type_p (TREE_VALUE (t), type))
      return 1;

  return 0;
}

/* Walk through all the subobjects of TYPE (located at OFFSET).  Call
   F for every subobject, passing it the type, offset, and table of
   OFFSETS.  If VBASES_P is one, then virtual non-primary bases should
   be traversed.

   If MAX_OFFSET is non-NULL, then subobjects with an offset greater
   than MAX_OFFSET will not be walked.

   If F returns a nonzero value, the traversal ceases, and that value
   is returned.  Otherwise, returns zero.  */

static int
walk_subobject_offsets (type, f, offset, offsets, max_offset, vbases_p)
     tree type;
     subobject_offset_fn f;
     tree offset;
     splay_tree offsets;
     tree max_offset;
     int vbases_p;
{
  int r = 0;
  tree type_binfo = NULL_TREE;

  /* If this OFFSET is bigger than the MAX_OFFSET, then we should
     stop.  */
  if (max_offset && INT_CST_LT (max_offset, offset))
    return 0;

  if (!TYPE_P (type)) 
    {
      if (abi_version_at_least (2))
	type_binfo = type;
      type = BINFO_TYPE (type);
    }

  if (CLASS_TYPE_P (type))
    {
      tree field;
      tree binfo;
      int i;

      /* Avoid recursing into objects that are not interesting.  */
      if (!CLASSTYPE_CONTAINS_EMPTY_CLASS_P (type))
	return 0;

      /* Record the location of TYPE.  */
      r = (*f) (type, offset, offsets);
      if (r)
	return r;

      /* Iterate through the direct base classes of TYPE.  */
      if (!type_binfo)
	type_binfo = TYPE_BINFO (type);
      for (i = 0; i < BINFO_N_BASETYPES (type_binfo); ++i)
	{
	  tree binfo_offset;

	  binfo = BINFO_BASETYPE (type_binfo, i);

	  if (abi_version_at_least (2) 
	      && TREE_VIA_VIRTUAL (binfo))
	    continue;

	  if (!vbases_p 
	      && TREE_VIA_VIRTUAL (binfo) 
	      && !BINFO_PRIMARY_P (binfo))
	    continue;

	  if (!abi_version_at_least (2))
	    binfo_offset = size_binop (PLUS_EXPR,
				       offset,
				       BINFO_OFFSET (binfo));
	  else
	    {
	      tree orig_binfo;
	      /* We cannot rely on BINFO_OFFSET being set for the base
		 class yet, but the offsets for direct non-virtual
		 bases can be calculated by going back to the TYPE.  */
	      orig_binfo = BINFO_BASETYPE (TYPE_BINFO (type), i);
	      binfo_offset = size_binop (PLUS_EXPR,	      
					 offset,
					 BINFO_OFFSET (orig_binfo));
	    }

	  r = walk_subobject_offsets (binfo,
				      f,
				      binfo_offset,
				      offsets,
				      max_offset,
				      (abi_version_at_least (2) 
				       ? /*vbases_p=*/0 : vbases_p));
	  if (r)
	    return r;
	}

      if (abi_version_at_least (2))
	{
	  tree vbase;

	  /* Iterate through the virtual base classes of TYPE.  In G++
	     3.2, we included virtual bases in the direct base class
	     loop above, which results in incorrect results; the
	     correct offsets for virtual bases are only known when
	     working with the most derived type.  */
	  if (vbases_p)
	    for (vbase = CLASSTYPE_VBASECLASSES (type);
		 vbase;
		 vbase = TREE_CHAIN (vbase))
	      {
		binfo = TREE_VALUE (vbase);
		r = walk_subobject_offsets (binfo,
					    f,
					    size_binop (PLUS_EXPR,
							offset,
							BINFO_OFFSET (binfo)),
					    offsets,
					    max_offset,
					    /*vbases_p=*/0);
		if (r)
		  return r;
	      }
	  else
	    {
	      /* We still have to walk the primary base, if it is
		 virtual.  (If it is non-virtual, then it was walked
		 above.)  */
	      vbase = get_primary_binfo (type_binfo);
	      if (vbase && TREE_VIA_VIRTUAL (vbase))
		{
		  tree derived = type_binfo;
		  while (BINFO_INHERITANCE_CHAIN (derived))
		    derived = BINFO_INHERITANCE_CHAIN (derived);
		  derived = TREE_TYPE (derived);
		  vbase = binfo_for_vbase (TREE_TYPE (vbase), derived);

		  if (BINFO_PRIMARY_BASE_OF (vbase) == type_binfo)
		    {
		      r = (walk_subobject_offsets 
			   (vbase, f, offset,
			    offsets, max_offset, /*vbases_p=*/0));
		      if (r)
			return r;
		    }
		}
	    }
	}

      /* Iterate through the fields of TYPE.  */
      for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	if (TREE_CODE (field) == FIELD_DECL && !DECL_ARTIFICIAL (field))
	  {
	    tree field_offset;

	    if (abi_version_at_least (2))
	      field_offset = byte_position (field);
	    else
	      /* In G++ 3.2, DECL_FIELD_OFFSET was used.  */
	      field_offset = DECL_FIELD_OFFSET (field);

	    r = walk_subobject_offsets (TREE_TYPE (field),
					f,
					size_binop (PLUS_EXPR,
						    offset,
						    field_offset),
					offsets,
					max_offset,
					/*vbases_p=*/1);
	    if (r)
	      return r;
	  }
    }
  else if (TREE_CODE (type) == ARRAY_TYPE)
    {
      tree element_type = strip_array_types (type);
      tree domain = TYPE_DOMAIN (type);
      tree index;

      /* Avoid recursing into objects that are not interesting.  */
      if (!CLASS_TYPE_P (element_type)
	  || !CLASSTYPE_CONTAINS_EMPTY_CLASS_P (element_type))
	return 0;

      /* Step through each of the elements in the array.  */
      for (index = size_zero_node;
	   /* G++ 3.2 had an off-by-one error here.  */
	   (abi_version_at_least (2) 
	    ? !INT_CST_LT (TYPE_MAX_VALUE (domain), index)
	    : INT_CST_LT (index, TYPE_MAX_VALUE (domain)));
	   index = size_binop (PLUS_EXPR, index, size_one_node))
	{
	  r = walk_subobject_offsets (TREE_TYPE (type),
				      f,
				      offset,
				      offsets,
				      max_offset,
				      /*vbases_p=*/1);
	  if (r)
	    return r;
	  offset = size_binop (PLUS_EXPR, offset, 
			       TYPE_SIZE_UNIT (TREE_TYPE (type)));
	  /* If this new OFFSET is bigger than the MAX_OFFSET, then
	     there's no point in iterating through the remaining
	     elements of the array.  */
	  if (max_offset && INT_CST_LT (max_offset, offset))
	    break;
	}
    }

  return 0;
}

/* Record all of the empty subobjects of TYPE (located at OFFSET) in
   OFFSETS.  If VBASES_P is nonzero, virtual bases of TYPE are
   examined.  */

static void
record_subobject_offsets (type, offset, offsets, vbases_p)
     tree type;
     tree offset;
     splay_tree offsets;
     int vbases_p;
{
  walk_subobject_offsets (type, record_subobject_offset, offset,
			  offsets, /*max_offset=*/NULL_TREE, vbases_p);
}

/* Returns nonzero if any of the empty subobjects of TYPE (located at
   OFFSET) conflict with entries in OFFSETS.  If VBASES_P is nonzero,
   virtual bases of TYPE are examined.  */

static int
layout_conflict_p (type, offset, offsets, vbases_p)
     tree type;
     tree offset;
     splay_tree offsets;
     int vbases_p;
{
  splay_tree_node max_node;

  /* Get the node in OFFSETS that indicates the maximum offset where
     an empty subobject is located.  */
  max_node = splay_tree_max (offsets);
  /* If there aren't any empty subobjects, then there's no point in
     performing this check.  */
  if (!max_node)
    return 0;

  return walk_subobject_offsets (type, check_subobject_offset, offset,
				 offsets, (tree) (max_node->key),
				 vbases_p);
}

/* DECL is a FIELD_DECL corresponding either to a base subobject of a
   non-static data member of the type indicated by RLI.  BINFO is the
   binfo corresponding to the base subobject, OFFSETS maps offsets to
   types already located at those offsets.  This function determines
   the position of the DECL.  */

static void
layout_nonempty_base_or_field (record_layout_info rli, 
			       tree decl, 
			       tree binfo, 
			       splay_tree offsets)
{
  tree t = rli->t;
  tree offset = NULL_TREE;
  bool field_p;
  tree type;
  
  if (binfo)
    {
      /* For the purposes of determining layout conflicts, we want to
	 use the class type of BINFO; TREE_TYPE (DECL) will be the
	 CLASSTYPE_AS_BASE version, which does not contain entries for
	 zero-sized bases.  */
      type = TREE_TYPE (binfo);
      field_p = false;
    }
  else
    {
      type = TREE_TYPE (decl);
      field_p = true;
    }

  /* Try to place the field.  It may take more than one try if we have
     a hard time placing the field without putting two objects of the
     same type at the same address.  */
  while (1)
    {
      struct record_layout_info_s old_rli = *rli;

      /* Place this field.  */
      place_field (rli, decl);
      offset = byte_position (decl);
 
      /* We have to check to see whether or not there is already
	 something of the same type at the offset we're about to use.
	 For example:
	 
	 struct S {};
	 struct T : public S { int i; };
	 struct U : public S, public T {};
	 
	 Here, we put S at offset zero in U.  Then, we can't put T at
	 offset zero -- its S component would be at the same address
	 as the S we already allocated.  So, we have to skip ahead.
	 Since all data members, including those whose type is an
	 empty class, have nonzero size, any overlap can happen only
	 with a direct or indirect base-class -- it can't happen with
	 a data member.  */
      /* G++ 3.2 did not check for overlaps when placing a non-empty
	 virtual base.  */
      if (!abi_version_at_least (2) && binfo && TREE_VIA_VIRTUAL (binfo))
	break;
      if (layout_conflict_p (field_p ? type : binfo, offset, 
			     offsets, field_p))
	{
	  /* Strip off the size allocated to this field.  That puts us
	     at the first place we could have put the field with
	     proper alignment.  */
	  *rli = old_rli;

	  /* Bump up by the alignment required for the type.  */
	  rli->bitpos
	    = size_binop (PLUS_EXPR, rli->bitpos, 
			  bitsize_int (binfo 
				       ? CLASSTYPE_ALIGN (type)
				       : TYPE_ALIGN (type)));
	  normalize_rli (rli);
	}
      else
	/* There was no conflict.  We're done laying out this field.  */
	break;
    }

  /* Now that we know where it will be placed, update its
     BINFO_OFFSET.  */
  if (binfo && CLASS_TYPE_P (BINFO_TYPE (binfo)))
    /* Indirect virtual bases may have a nonzero BINFO_OFFSET at
       this point because their BINFO_OFFSET is copied from another
       hierarchy.  Therefore, we may not need to add the entire
       OFFSET.  */
    propagate_binfo_offsets (binfo, 
			     size_diffop (convert (ssizetype, offset),
					  convert (ssizetype, 
						   BINFO_OFFSET (binfo))),
			     t);
}

/* Returns true if TYPE is empty and OFFSET is nonzero.  */

static int
empty_base_at_nonzero_offset_p (tree type,
				tree offset,
				splay_tree offsets ATTRIBUTE_UNUSED)
{
  return is_empty_class (type) && !integer_zerop (offset);
}

/* Layout the empty base BINFO.  EOC indicates the byte currently just
   past the end of the class, and should be correctly aligned for a
   class of the type indicated by BINFO; OFFSETS gives the offsets of
   the empty bases allocated so far. T is the most derived
   type.  Return nonzero iff we added it at the end.  */

static bool
layout_empty_base (binfo, eoc, offsets, t)
     tree binfo;
     tree eoc;
     splay_tree offsets;
     tree t;
{
  tree alignment;
  tree basetype = BINFO_TYPE (binfo);
  bool atend = false;

  /* This routine should only be used for empty classes.  */
  my_friendly_assert (is_empty_class (basetype), 20000321);
  alignment = ssize_int (CLASSTYPE_ALIGN_UNIT (basetype));
  
  if (abi_version_at_least (2))
    BINFO_OFFSET (binfo) = size_zero_node;
  if (warn_abi && !integer_zerop (BINFO_OFFSET (binfo)))
    warning ("offset of empty base `%T' may not be ABI-compliant and may"
	     "change in a future version of GCC",
	     BINFO_TYPE (binfo));

  /* This is an empty base class.  We first try to put it at offset
     zero.  */
  if (layout_conflict_p (binfo,
			 BINFO_OFFSET (binfo),
			 offsets, 
			 /*vbases_p=*/0))
    {
      /* That didn't work.  Now, we move forward from the next
	 available spot in the class.  */
      atend = true;
      propagate_binfo_offsets (binfo, convert (ssizetype, eoc), t);
      while (1) 
	{
	  if (!layout_conflict_p (binfo,
				  BINFO_OFFSET (binfo), 
				  offsets,
				  /*vbases_p=*/0))
	    /* We finally found a spot where there's no overlap.  */
	    break;

	  /* There's overlap here, too.  Bump along to the next spot.  */
	  propagate_binfo_offsets (binfo, alignment, t);
	}
    }
  return atend;
}

/* Layout the the base given by BINFO in the class indicated by RLI.
   *BASE_ALIGN is a running maximum of the alignments of
   any base class.  OFFSETS gives the location of empty base
   subobjects.  T is the most derived type.  Return nonzero if the new
   object cannot be nearly-empty.  A new FIELD_DECL is inserted at
   *NEXT_FIELD, unless BINFO is for an empty base class.  

   Returns the location at which the next field should be inserted.  */

static tree *
build_base_field (record_layout_info rli, tree binfo,
		  splay_tree offsets, tree *next_field)
{
  tree t = rli->t;
  tree basetype = BINFO_TYPE (binfo);

  if (!COMPLETE_TYPE_P (basetype))
    /* This error is now reported in xref_tag, thus giving better
       location information.  */
    return next_field;
  
  /* Place the base class.  */
  if (!is_empty_class (basetype))
    {
      tree decl;

      /* The containing class is non-empty because it has a non-empty
	 base class.  */
      CLASSTYPE_EMPTY_P (t) = 0;
      
      /* Create the FIELD_DECL.  */
      decl = build_decl (FIELD_DECL, NULL_TREE, CLASSTYPE_AS_BASE (basetype));
      DECL_ARTIFICIAL (decl) = 1;
      DECL_FIELD_CONTEXT (decl) = t;
      DECL_SIZE (decl) = CLASSTYPE_SIZE (basetype);
      DECL_SIZE_UNIT (decl) = CLASSTYPE_SIZE_UNIT (basetype);
      DECL_ALIGN (decl) = CLASSTYPE_ALIGN (basetype);
      DECL_USER_ALIGN (decl) = CLASSTYPE_USER_ALIGN (basetype);
      DECL_IGNORED_P (decl) = 1;

      /* Try to place the field.  It may take more than one try if we
	 have a hard time placing the field without putting two
	 objects of the same type at the same address.  */
      layout_nonempty_base_or_field (rli, decl, binfo, offsets);
      /* Add the new FIELD_DECL to the list of fields for T.  */
      TREE_CHAIN (decl) = *next_field;
      *next_field = decl;
      next_field = &TREE_CHAIN (decl);
    }
  else
    {
      tree eoc;
      bool atend;

      /* On some platforms (ARM), even empty classes will not be
	 byte-aligned.  */
      eoc = round_up (rli_size_unit_so_far (rli),
		      CLASSTYPE_ALIGN_UNIT (basetype));
      atend = layout_empty_base (binfo, eoc, offsets, t);
      /* A nearly-empty class "has no proper base class that is empty,
	 not morally virtual, and at an offset other than zero."  */
      if (!TREE_VIA_VIRTUAL (binfo) && CLASSTYPE_NEARLY_EMPTY_P (t))
	{
	  if (atend)
	    CLASSTYPE_NEARLY_EMPTY_P (t) = 0;
	  /* The check above (used in G++ 3.2) is insufficient  because
	     an empty class placed at offset zero might itself have an
	     empty base at a nonzero offset.  */
	  else if (walk_subobject_offsets (basetype, 
					   empty_base_at_nonzero_offset_p,
					   size_zero_node,
					   /*offsets=*/NULL,
					   /*max_offset=*/NULL_TREE,
					   /*vbases_p=*/true))
	    {
	      if (abi_version_at_least (2))
		CLASSTYPE_NEARLY_EMPTY_P (t) = 0;
	      else if (warn_abi)
		warning ("class `%T' will be considered nearly empty in a "
			 "future version of GCC", t);
	    }
	}
	
      /* We do not create a FIELD_DECL for empty base classes because
	 it might overlap some other field.  We want to be able to
	 create CONSTRUCTORs for the class by iterating over the
	 FIELD_DECLs, and the back end does not handle overlapping
	 FIELD_DECLs.  */

      /* An empty virtual base causes a class to be non-empty
	 -- but in that case we do not need to clear CLASSTYPE_EMPTY_P
	 here because that was already done when the virtual table
	 pointer was created.  */
    }

  /* Record the offsets of BINFO and its base subobjects.  */
  record_subobject_offsets (binfo,
			    BINFO_OFFSET (binfo),
			    offsets, 
			    /*vbases_p=*/0);

  return next_field;
}

/* Layout all of the non-virtual base classes.  Record empty
   subobjects in OFFSETS.  T is the most derived type.  Return nonzero
   if the type cannot be nearly empty.  The fields created
   corresponding to the base classes will be inserted at
   *NEXT_FIELD.  */

static void
build_base_fields (record_layout_info rli,
		   splay_tree offsets, tree *next_field)
{
  /* Chain to hold all the new FIELD_DECLs which stand in for base class
     subobjects.  */
  tree t = rli->t;
  int n_baseclasses = CLASSTYPE_N_BASECLASSES (t);
  int i;

  /* The primary base class is always allocated first.  */
  if (CLASSTYPE_HAS_PRIMARY_BASE_P (t))
    next_field = build_base_field (rli, CLASSTYPE_PRIMARY_BINFO (t),
				   offsets, next_field);

  /* Now allocate the rest of the bases.  */
  for (i = 0; i < n_baseclasses; ++i)
    {
      tree base_binfo;

      base_binfo = BINFO_BASETYPE (TYPE_BINFO (t), i);

      /* The primary base was already allocated above, so we don't
	 need to allocate it again here.  */
      if (base_binfo == CLASSTYPE_PRIMARY_BINFO (t))
	continue;

      /* A primary virtual base class is allocated just like any other
	 base class, but a non-primary virtual base is allocated
	 later, in layout_virtual_bases.  */
      if (TREE_VIA_VIRTUAL (base_binfo) 
	  && !BINFO_PRIMARY_P (base_binfo))
	continue;

      next_field = build_base_field (rli, base_binfo,
				     offsets, next_field);
    }
}

/* Go through the TYPE_METHODS of T issuing any appropriate
   diagnostics, figuring out which methods override which other
   methods, and so forth.  */

static void
check_methods (t)
     tree t;
{
  tree x;

  for (x = TYPE_METHODS (t); x; x = TREE_CHAIN (x))
    {
      /* If this was an evil function, don't keep it in class.  */
      if (DECL_ASSEMBLER_NAME_SET_P (x) 
	  && IDENTIFIER_ERROR_LOCUS (DECL_ASSEMBLER_NAME (x)))
	continue;

      check_for_override (x, t);
      if (DECL_PURE_VIRTUAL_P (x) && ! DECL_VINDEX (x))
	cp_error_at ("initializer specified for non-virtual method `%D'", x);

      /* The name of the field is the original field name
	 Save this in auxiliary field for later overloading.  */
      if (DECL_VINDEX (x))
	{
	  TYPE_POLYMORPHIC_P (t) = 1;
	  if (DECL_PURE_VIRTUAL_P (x))
	    CLASSTYPE_PURE_VIRTUALS (t)
	      = tree_cons (NULL_TREE, x, CLASSTYPE_PURE_VIRTUALS (t));
	}
    }
}

/* FN is a constructor or destructor.  Clone the declaration to create
   a specialized in-charge or not-in-charge version, as indicated by
   NAME.  */

static tree
build_clone (fn, name)
     tree fn;
     tree name;
{
  tree parms;
  tree clone;

  /* Copy the function.  */
  clone = copy_decl (fn);
  /* Remember where this function came from.  */
  DECL_CLONED_FUNCTION (clone) = fn;
  DECL_ABSTRACT_ORIGIN (clone) = fn;
  /* Reset the function name.  */
  DECL_NAME (clone) = name;
  SET_DECL_ASSEMBLER_NAME (clone, NULL_TREE);
  /* There's no pending inline data for this function.  */
  DECL_PENDING_INLINE_INFO (clone) = NULL;
  DECL_PENDING_INLINE_P (clone) = 0;
  /* And it hasn't yet been deferred.  */
  DECL_DEFERRED_FN (clone) = 0;

  /* The base-class destructor is not virtual.  */
  if (name == base_dtor_identifier)
    {
      DECL_VIRTUAL_P (clone) = 0;
      if (TREE_CODE (clone) != TEMPLATE_DECL)
	DECL_VINDEX (clone) = NULL_TREE;
    }

  /* If there was an in-charge parameter, drop it from the function
     type.  */
  if (DECL_HAS_IN_CHARGE_PARM_P (clone))
    {
      tree basetype;
      tree parmtypes;
      tree exceptions;

      exceptions = TYPE_RAISES_EXCEPTIONS (TREE_TYPE (clone));
      basetype = TYPE_METHOD_BASETYPE (TREE_TYPE (clone));
      parmtypes = TYPE_ARG_TYPES (TREE_TYPE (clone));
      /* Skip the `this' parameter.  */
      parmtypes = TREE_CHAIN (parmtypes);
      /* Skip the in-charge parameter.  */
      parmtypes = TREE_CHAIN (parmtypes);
      /* And the VTT parm, in a complete [cd]tor.  */
      if (DECL_HAS_VTT_PARM_P (fn)
	  && ! DECL_NEEDS_VTT_PARM_P (clone))
	parmtypes = TREE_CHAIN (parmtypes);
       /* If this is subobject constructor or destructor, add the vtt
	 parameter.  */
      TREE_TYPE (clone) 
	= build_cplus_method_type (basetype,
				   TREE_TYPE (TREE_TYPE (clone)),
				   parmtypes);
      if (exceptions)
	TREE_TYPE (clone) = build_exception_variant (TREE_TYPE (clone),
						     exceptions);
    }

  /* Copy the function parameters.  But, DECL_ARGUMENTS on a TEMPLATE_DECL
     aren't function parameters; those are the template parameters.  */
  if (TREE_CODE (clone) != TEMPLATE_DECL)
    {
      DECL_ARGUMENTS (clone) = copy_list (DECL_ARGUMENTS (clone));
      /* Remove the in-charge parameter.  */
      if (DECL_HAS_IN_CHARGE_PARM_P (clone))
	{
	  TREE_CHAIN (DECL_ARGUMENTS (clone))
	    = TREE_CHAIN (TREE_CHAIN (DECL_ARGUMENTS (clone)));
	  DECL_HAS_IN_CHARGE_PARM_P (clone) = 0;
	}
      /* And the VTT parm, in a complete [cd]tor.  */
      if (DECL_HAS_VTT_PARM_P (fn))
	{
	  if (DECL_NEEDS_VTT_PARM_P (clone))
	    DECL_HAS_VTT_PARM_P (clone) = 1;
	  else
	    {
	      TREE_CHAIN (DECL_ARGUMENTS (clone))
		= TREE_CHAIN (TREE_CHAIN (DECL_ARGUMENTS (clone)));
	      DECL_HAS_VTT_PARM_P (clone) = 0;
	    }
	}

      for (parms = DECL_ARGUMENTS (clone); parms; parms = TREE_CHAIN (parms))
	{
	  DECL_CONTEXT (parms) = clone;
	  cxx_dup_lang_specific_decl (parms);
	}
    }

  /* Create the RTL for this function.  */
  SET_DECL_RTL (clone, NULL_RTX);
  rest_of_decl_compilation (clone, NULL, /*top_level=*/1, at_eof);
  
  /* Make it easy to find the CLONE given the FN.  */
  TREE_CHAIN (clone) = TREE_CHAIN (fn);
  TREE_CHAIN (fn) = clone;

  /* If this is a template, handle the DECL_TEMPLATE_RESULT as well.  */
  if (TREE_CODE (clone) == TEMPLATE_DECL)
    {
      tree result;

      DECL_TEMPLATE_RESULT (clone) 
	= build_clone (DECL_TEMPLATE_RESULT (clone), name);
      result = DECL_TEMPLATE_RESULT (clone);
      DECL_TEMPLATE_INFO (result) = copy_node (DECL_TEMPLATE_INFO (result));
      DECL_TI_TEMPLATE (result) = clone;
    }
  else if (DECL_DEFERRED_FN (fn))
    defer_fn (clone);

  return clone;
}

/* Produce declarations for all appropriate clones of FN.  If
   UPDATE_METHOD_VEC_P is nonzero, the clones are added to the
   CLASTYPE_METHOD_VEC as well.  */

void
clone_function_decl (fn, update_method_vec_p)
     tree fn;
     int update_method_vec_p;
{
  tree clone;

  /* Avoid inappropriate cloning.  */
  if (TREE_CHAIN (fn)
      && DECL_CLONED_FUNCTION (TREE_CHAIN (fn)))
    return;

  if (DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P (fn))
    {
      /* For each constructor, we need two variants: an in-charge version
	 and a not-in-charge version.  */
      clone = build_clone (fn, complete_ctor_identifier);
      if (update_method_vec_p)
	add_method (DECL_CONTEXT (clone), clone, /*error_p=*/0);
      clone = build_clone (fn, base_ctor_identifier);
      if (update_method_vec_p)
	add_method (DECL_CONTEXT (clone), clone, /*error_p=*/0);
    }
  else
    {
      my_friendly_assert (DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P (fn), 20000411);

      /* For each destructor, we need three variants: an in-charge
	 version, a not-in-charge version, and an in-charge deleting
	 version.  We clone the deleting version first because that
	 means it will go second on the TYPE_METHODS list -- and that
	 corresponds to the correct layout order in the virtual
	 function table.  

         For a non-virtual destructor, we do not build a deleting
	 destructor.  */
      if (DECL_VIRTUAL_P (fn))
	{
	  clone = build_clone (fn, deleting_dtor_identifier);
	  if (update_method_vec_p)
	    add_method (DECL_CONTEXT (clone), clone, /*error_p=*/0);
	}
      clone = build_clone (fn, complete_dtor_identifier);
      if (update_method_vec_p)
	add_method (DECL_CONTEXT (clone), clone, /*error_p=*/0);
      clone = build_clone (fn, base_dtor_identifier);
      if (update_method_vec_p)
	add_method (DECL_CONTEXT (clone), clone, /*error_p=*/0);
    }

  /* Note that this is an abstract function that is never emitted.  */
  DECL_ABSTRACT (fn) = 1;
}

/* DECL is an in charge constructor, which is being defined. This will
   have had an in class declaration, from whence clones were
   declared. An out-of-class definition can specify additional default
   arguments. As it is the clones that are involved in overload
   resolution, we must propagate the information from the DECL to its
   clones.  */

void
adjust_clone_args (decl)
     tree decl;
{
  tree clone;
  
  for (clone = TREE_CHAIN (decl); clone && DECL_CLONED_FUNCTION (clone);
       clone = TREE_CHAIN (clone))
    {
      tree orig_clone_parms = TYPE_ARG_TYPES (TREE_TYPE (clone));
      tree orig_decl_parms = TYPE_ARG_TYPES (TREE_TYPE (decl));
      tree decl_parms, clone_parms;

      clone_parms = orig_clone_parms;
      
      /* Skip the 'this' parameter.  */
      orig_clone_parms = TREE_CHAIN (orig_clone_parms);
      orig_decl_parms = TREE_CHAIN (orig_decl_parms);

      if (DECL_HAS_IN_CHARGE_PARM_P (decl))
	orig_decl_parms = TREE_CHAIN (orig_decl_parms);
      if (DECL_HAS_VTT_PARM_P (decl))
	orig_decl_parms = TREE_CHAIN (orig_decl_parms);
      
      clone_parms = orig_clone_parms;
      if (DECL_HAS_VTT_PARM_P (clone))
	clone_parms = TREE_CHAIN (clone_parms);
      
      for (decl_parms = orig_decl_parms; decl_parms;
	   decl_parms = TREE_CHAIN (decl_parms),
	     clone_parms = TREE_CHAIN (clone_parms))
	{
	  my_friendly_assert (same_type_p (TREE_TYPE (decl_parms),
					   TREE_TYPE (clone_parms)), 20010424);
	  
	  if (TREE_PURPOSE (decl_parms) && !TREE_PURPOSE (clone_parms))
	    {
	      /* A default parameter has been added. Adjust the
		 clone's parameters.  */
	      tree exceptions = TYPE_RAISES_EXCEPTIONS (TREE_TYPE (clone));
	      tree basetype = TYPE_METHOD_BASETYPE (TREE_TYPE (clone));
	      tree type;

	      clone_parms = orig_decl_parms;

	      if (DECL_HAS_VTT_PARM_P (clone))
		{
		  clone_parms = tree_cons (TREE_PURPOSE (orig_clone_parms),
					   TREE_VALUE (orig_clone_parms),
					   clone_parms);
		  TREE_TYPE (clone_parms) = TREE_TYPE (orig_clone_parms);
		}
	      type = build_cplus_method_type (basetype,
					      TREE_TYPE (TREE_TYPE (clone)),
					      clone_parms);
	      if (exceptions)
		type = build_exception_variant (type, exceptions);
	      TREE_TYPE (clone) = type;
	      
	      clone_parms = NULL_TREE;
	      break;
	    }
	}
      my_friendly_assert (!clone_parms, 20010424);
    }
}

/* For each of the constructors and destructors in T, create an
   in-charge and not-in-charge variant.  */

static void
clone_constructors_and_destructors (t)
     tree t;
{
  tree fns;

  /* If for some reason we don't have a CLASSTYPE_METHOD_VEC, we bail
     out now.  */
  if (!CLASSTYPE_METHOD_VEC (t))
    return;

  for (fns = CLASSTYPE_CONSTRUCTORS (t); fns; fns = OVL_NEXT (fns))
    clone_function_decl (OVL_CURRENT (fns), /*update_method_vec_p=*/1);
  for (fns = CLASSTYPE_DESTRUCTORS (t); fns; fns = OVL_NEXT (fns))
    clone_function_decl (OVL_CURRENT (fns), /*update_method_vec_p=*/1);
}

/* Remove all zero-width bit-fields from T.  */

static void
remove_zero_width_bit_fields (t)
     tree t;
{
  tree *fieldsp;

  fieldsp = &TYPE_FIELDS (t); 
  while (*fieldsp)
    {
      if (TREE_CODE (*fieldsp) == FIELD_DECL
	  && DECL_C_BIT_FIELD (*fieldsp) 
	  && DECL_INITIAL (*fieldsp))
	*fieldsp = TREE_CHAIN (*fieldsp);
      else
	fieldsp = &TREE_CHAIN (*fieldsp);
    }
}

/* Returns TRUE iff we need a cookie when dynamically allocating an
   array whose elements have the indicated class TYPE.  */

static bool
type_requires_array_cookie (type)
     tree type;
{
  tree fns;
  bool has_two_argument_delete_p = false;

  my_friendly_assert (CLASS_TYPE_P (type), 20010712);

  /* If there's a non-trivial destructor, we need a cookie.  In order
     to iterate through the array calling the destructor for each
     element, we'll have to know how many elements there are.  */
  if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (type))
    return true;

  /* If the usual deallocation function is a two-argument whose second
     argument is of type `size_t', then we have to pass the size of
     the array to the deallocation function, so we will need to store
     a cookie.  */
  fns = lookup_fnfields (TYPE_BINFO (type), 
			 ansi_opname (VEC_DELETE_EXPR),
			 /*protect=*/0);
  /* If there are no `operator []' members, or the lookup is
     ambiguous, then we don't need a cookie.  */
  if (!fns || fns == error_mark_node)
    return false;
  /* Loop through all of the functions.  */
  for (fns = BASELINK_FUNCTIONS (fns); fns; fns = OVL_NEXT (fns))
    {
      tree fn;
      tree second_parm;

      /* Select the current function.  */
      fn = OVL_CURRENT (fns);
      /* See if this function is a one-argument delete function.  If
	 it is, then it will be the usual deallocation function.  */
      second_parm = TREE_CHAIN (TYPE_ARG_TYPES (TREE_TYPE (fn)));
      if (second_parm == void_list_node)
	return false;
      /* Otherwise, if we have a two-argument function and the second
	 argument is `size_t', it will be the usual deallocation
	 function -- unless there is one-argument function, too.  */
      if (TREE_CHAIN (second_parm) == void_list_node
	  && same_type_p (TREE_VALUE (second_parm), sizetype))
	has_two_argument_delete_p = true;
    }

  return has_two_argument_delete_p;
}

/* Check the validity of the bases and members declared in T.  Add any
   implicitly-generated functions (like copy-constructors and
   assignment operators).  Compute various flag bits (like
   CLASSTYPE_NON_POD_T) for T.  This routine works purely at the C++
   level: i.e., independently of the ABI in use.  */

static void
check_bases_and_members (tree t)
{
  /* Nonzero if we are not allowed to generate a default constructor
     for this case.  */
  int cant_have_default_ctor;
  /* Nonzero if the implicitly generated copy constructor should take
     a non-const reference argument.  */
  int cant_have_const_ctor;
  /* Nonzero if the the implicitly generated assignment operator
     should take a non-const reference argument.  */
  int no_const_asn_ref;
  tree access_decls;

  /* By default, we use const reference arguments and generate default
     constructors.  */
  cant_have_default_ctor = 0;
  cant_have_const_ctor = 0;
  no_const_asn_ref = 0;

  /* Check all the base-classes.  */
  check_bases (t, &cant_have_default_ctor, &cant_have_const_ctor,
	       &no_const_asn_ref);

  /* Check all the data member declarations.  */
  check_field_decls (t, &access_decls,
		     &cant_have_default_ctor,
		     &cant_have_const_ctor,
		     &no_const_asn_ref);

  /* Check all the method declarations.  */
  check_methods (t);

  /* A nearly-empty class has to be vptr-containing; a nearly empty
     class contains just a vptr.  */
  if (!TYPE_CONTAINS_VPTR_P (t))
    CLASSTYPE_NEARLY_EMPTY_P (t) = 0;

  /* Do some bookkeeping that will guide the generation of implicitly
     declared member functions.  */
  TYPE_HAS_COMPLEX_INIT_REF (t)
    |= (TYPE_HAS_INIT_REF (t) 
	|| TYPE_USES_VIRTUAL_BASECLASSES (t)
	|| TYPE_POLYMORPHIC_P (t));
  TYPE_NEEDS_CONSTRUCTING (t)
    |= (TYPE_HAS_CONSTRUCTOR (t) 
	|| TYPE_USES_VIRTUAL_BASECLASSES (t)
	|| TYPE_POLYMORPHIC_P (t));
  CLASSTYPE_NON_AGGREGATE (t) |= (TYPE_HAS_CONSTRUCTOR (t)
				  || TYPE_POLYMORPHIC_P (t));
  CLASSTYPE_NON_POD_P (t)
    |= (CLASSTYPE_NON_AGGREGATE (t) || TYPE_HAS_DESTRUCTOR (t) 
	|| TYPE_HAS_ASSIGN_REF (t));
  TYPE_HAS_REAL_ASSIGN_REF (t) |= TYPE_HAS_ASSIGN_REF (t);
  TYPE_HAS_COMPLEX_ASSIGN_REF (t)
    |= TYPE_HAS_ASSIGN_REF (t) || TYPE_CONTAINS_VPTR_P (t);

  /* Synthesize any needed methods.  Note that methods will be synthesized
     for anonymous unions; grok_x_components undoes that.  */
  add_implicitly_declared_members (t, cant_have_default_ctor,
				   cant_have_const_ctor,
				   no_const_asn_ref);

  /* Create the in-charge and not-in-charge variants of constructors
     and destructors.  */
  clone_constructors_and_destructors (t);

  /* Process the using-declarations.  */
  for (; access_decls; access_decls = TREE_CHAIN (access_decls))
    handle_using_decl (TREE_VALUE (access_decls), t);

  /* Build and sort the CLASSTYPE_METHOD_VEC.  */
  finish_struct_methods (t);

  /* Figure out whether or not we will need a cookie when dynamically
     allocating an array of this type.  */
  TYPE_LANG_SPECIFIC (t)->u.c.vec_new_uses_cookie
    = type_requires_array_cookie (t);
}

/* If T needs a pointer to its virtual function table, set TYPE_VFIELD
   accordingly.  If a new vfield was created (because T doesn't have a
   primary base class), then the newly created field is returned.  It
   is not added to the TYPE_FIELDS list; it is the caller's
   responsibility to do that.  Accumulate declared virtual functions
   on VIRTUALS_P.  */

static tree
create_vtable_ptr (t, virtuals_p)
     tree t;
     tree *virtuals_p;
{
  tree fn;

  /* Collect the virtual functions declared in T.  */
  for (fn = TYPE_METHODS (t); fn; fn = TREE_CHAIN (fn))
    if (DECL_VINDEX (fn) && !DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P (fn)
	&& TREE_CODE (DECL_VINDEX (fn)) != INTEGER_CST)
      {
	tree new_virtual = make_node (TREE_LIST);
	
	BV_FN (new_virtual) = fn;
	BV_DELTA (new_virtual) = integer_zero_node;

	TREE_CHAIN (new_virtual) = *virtuals_p;
	*virtuals_p = new_virtual;
      }
  
  /* If we couldn't find an appropriate base class, create a new field
     here.  Even if there weren't any new virtual functions, we might need a
     new virtual function table if we're supposed to include vptrs in
     all classes that need them.  */
  if (!TYPE_VFIELD (t) && (*virtuals_p || TYPE_CONTAINS_VPTR_P (t)))
    {
      /* We build this decl with vtbl_ptr_type_node, which is a
	 `vtable_entry_type*'.  It might seem more precise to use
	 `vtable_entry_type (*)[N]' where N is the number of firtual
	 functions.  However, that would require the vtable pointer in
	 base classes to have a different type than the vtable pointer
	 in derived classes.  We could make that happen, but that
	 still wouldn't solve all the problems.  In particular, the
	 type-based alias analysis code would decide that assignments
	 to the base class vtable pointer can't alias assignments to
	 the derived class vtable pointer, since they have different
	 types.  Thus, in an derived class destructor, where the base
	 class constructor was inlined, we could generate bad code for
	 setting up the vtable pointer.  

         Therefore, we use one type for all vtable pointers.  We still
	 use a type-correct type; it's just doesn't indicate the array
	 bounds.  That's better than using `void*' or some such; it's
	 cleaner, and it let's the alias analysis code know that these
	 stores cannot alias stores to void*!  */
      tree field;

      field = build_decl (FIELD_DECL, get_vfield_name (t), vtbl_ptr_type_node);
      SET_DECL_ASSEMBLER_NAME (field, get_identifier (VFIELD_BASE));
      DECL_VIRTUAL_P (field) = 1;
      DECL_ARTIFICIAL (field) = 1;
      DECL_FIELD_CONTEXT (field) = t;
      DECL_FCONTEXT (field) = t;
      DECL_ALIGN (field) = TYPE_ALIGN (vtbl_ptr_type_node);
      DECL_USER_ALIGN (field) = TYPE_USER_ALIGN (vtbl_ptr_type_node);
      
      TYPE_VFIELD (t) = field;
      
      /* This class is non-empty.  */
      CLASSTYPE_EMPTY_P (t) = 0;

      if (CLASSTYPE_N_BASECLASSES (t))
	/* If there were any baseclasses, they can't possibly be at
	   offset zero any more, because that's where the vtable
	   pointer is.  So, converting to a base class is going to
	   take work.  */
	TYPE_BASE_CONVS_MAY_REQUIRE_CODE_P (t) = 1;

      return field;
    }

  return NULL_TREE;
}

/* Fixup the inline function given by INFO now that the class is
   complete.  */

static void
fixup_pending_inline (fn)
     tree fn;
{
  if (DECL_PENDING_INLINE_INFO (fn))
    {
      tree args = DECL_ARGUMENTS (fn);
      while (args)
	{
	  DECL_CONTEXT (args) = fn;
	  args = TREE_CHAIN (args);
	}
    }
}

/* Fixup the inline methods and friends in TYPE now that TYPE is
   complete.  */

static void
fixup_inline_methods (type)
     tree type;
{
  tree method = TYPE_METHODS (type);

  if (method && TREE_CODE (method) == TREE_VEC)
    {
      if (TREE_VEC_ELT (method, 1))
	method = TREE_VEC_ELT (method, 1);
      else if (TREE_VEC_ELT (method, 0))
	method = TREE_VEC_ELT (method, 0);
      else
	method = TREE_VEC_ELT (method, 2);
    }

  /* Do inline member functions.  */
  for (; method; method = TREE_CHAIN (method))
    fixup_pending_inline (method);

  /* Do friends.  */
  for (method = CLASSTYPE_INLINE_FRIENDS (type); 
       method; 
       method = TREE_CHAIN (method))
    fixup_pending_inline (TREE_VALUE (method));
  CLASSTYPE_INLINE_FRIENDS (type) = NULL_TREE;
}

/* Add OFFSET to all base types of BINFO which is a base in the
   hierarchy dominated by T.

   OFFSET, which is a type offset, is number of bytes.  */

static void
propagate_binfo_offsets (binfo, offset, t)
     tree binfo;
     tree offset;
     tree t;
{
  int i;
  tree primary_binfo;

  /* Update BINFO's offset.  */
  BINFO_OFFSET (binfo)
    = convert (sizetype, 
	       size_binop (PLUS_EXPR,
			   convert (ssizetype, BINFO_OFFSET (binfo)),
			   offset));

  /* Find the primary base class.  */
  primary_binfo = get_primary_binfo (binfo);

  /* Scan all of the bases, pushing the BINFO_OFFSET adjust
     downwards.  */
  for (i = -1; i < BINFO_N_BASETYPES (binfo); ++i)
    {
      tree base_binfo;

      /* On the first time through the loop, do the primary base.
	 Because the primary base need not be an immediate base, we
	 must handle the primary base specially.  */
      if (i == -1) 
	{
	  if (!primary_binfo) 
	    continue;

	  base_binfo = primary_binfo;
	}
      else
	{
	  base_binfo = BINFO_BASETYPE (binfo, i);
	  /* Don't do the primary base twice.  */
	  if (base_binfo == primary_binfo)
	    continue;
	}

      /* Skip virtual bases that aren't our canonical primary base.  */
      if (TREE_VIA_VIRTUAL (base_binfo)
	  && (BINFO_PRIMARY_BASE_OF (base_binfo) != binfo
	      || base_binfo != binfo_for_vbase (BINFO_TYPE (base_binfo), t)))
	continue;

      propagate_binfo_offsets (base_binfo, offset, t);
    }
}

/* Called via dfs_walk from layout_virtual bases.  */

static tree
dfs_set_offset_for_unshared_vbases (binfo, data)
     tree binfo;
     void *data;
{
  /* If this is a virtual base, make sure it has the same offset as
     the shared copy.  If it's a primary base, then we know it's
     correct.  */
  if (TREE_VIA_VIRTUAL (binfo))
    {
      tree t = (tree) data;
      tree vbase;
      tree offset;
      
      vbase = binfo_for_vbase (BINFO_TYPE (binfo), t);
      if (vbase != binfo)
	{
	  offset = size_diffop (BINFO_OFFSET (vbase), BINFO_OFFSET (binfo));
	  propagate_binfo_offsets (binfo, offset, t);
	}
    }

  return NULL_TREE;
}

/* Set BINFO_OFFSET for all of the virtual bases for RLI->T.  Update
   TYPE_ALIGN and TYPE_SIZE for T.  OFFSETS gives the location of
   empty subobjects of T.  */

static void
layout_virtual_bases (record_layout_info rli, splay_tree offsets)
{
  tree vbases;
  tree t = rli->t;
  bool first_vbase = true;
  tree *next_field;

  if (CLASSTYPE_N_BASECLASSES (t) == 0)
    return;

  if (!abi_version_at_least(2))
    {
      /* In G++ 3.2, we incorrectly rounded the size before laying out
	 the virtual bases.  */
      finish_record_layout (rli, /*free_p=*/false);
#ifdef STRUCTURE_SIZE_BOUNDARY
      /* Packed structures don't need to have minimum size.  */
      if (! TYPE_PACKED (t))
	TYPE_ALIGN (t) = MAX (TYPE_ALIGN (t), STRUCTURE_SIZE_BOUNDARY);
#endif
      rli->offset = TYPE_SIZE_UNIT (t);
      rli->bitpos = bitsize_zero_node;
      rli->record_align = TYPE_ALIGN (t);
    }

  /* Find the last field.  The artificial fields created for virtual
     bases will go after the last extant field to date.  */
  next_field = &TYPE_FIELDS (t);
  while (*next_field)
    next_field = &TREE_CHAIN (*next_field);

  /* Go through the virtual bases, allocating space for each virtual
     base that is not already a primary base class.  These are
     allocated in inheritance graph order.  */
  for (vbases = TYPE_BINFO (t);
       vbases; 
       vbases = TREE_CHAIN (vbases))
    {
      tree vbase;

      if (!TREE_VIA_VIRTUAL (vbases))
	continue;

      vbase = binfo_for_vbase (BINFO_TYPE (vbases), t);

      if (!BINFO_PRIMARY_P (vbase))
	{
	  tree basetype = TREE_TYPE (vbase);

	  /* This virtual base is not a primary base of any class in the
	     hierarchy, so we have to add space for it.  */
	  next_field = build_base_field (rli, vbase,
					 offsets, next_field);

	  /* If the first virtual base might have been placed at a
	     lower address, had we started from CLASSTYPE_SIZE, rather
	     than TYPE_SIZE, issue a warning.  There can be both false
	     positives and false negatives from this warning in rare
	     cases; to deal with all the possibilities would probably
	     require performing both layout algorithms and comparing
	     the results which is not particularly tractable.  */
	  if (warn_abi
	      && first_vbase
	      && (tree_int_cst_lt 
		  (size_binop (CEIL_DIV_EXPR,
			       round_up (CLASSTYPE_SIZE (t),
					 CLASSTYPE_ALIGN (basetype)),
			       bitsize_unit_node),
		   BINFO_OFFSET (vbase))))
	    warning ("offset of virtual base `%T' is not ABI-compliant and may change in a future version of GCC",
		     basetype);

	  first_vbase = false;
	}
    }

  /* Now, go through the TYPE_BINFO hierarchy, setting the
     BINFO_OFFSETs correctly for all non-primary copies of the virtual
     bases and their direct and indirect bases.  The ambiguity checks
     in lookup_base depend on the BINFO_OFFSETs being set
     correctly.  */
  dfs_walk (TYPE_BINFO (t), dfs_set_offset_for_unshared_vbases, NULL, t);
}

/* Returns the offset of the byte just past the end of the base class
   BINFO.  */

static tree
end_of_base (tree binfo)
{
  tree size;

  if (is_empty_class (BINFO_TYPE (binfo)))
    /* An empty class has zero CLASSTYPE_SIZE_UNIT, but we need to
       allocate some space for it. It cannot have virtual bases, so
       TYPE_SIZE_UNIT is fine.  */
    size = TYPE_SIZE_UNIT (BINFO_TYPE (binfo));
  else
    size = CLASSTYPE_SIZE_UNIT (BINFO_TYPE (binfo));

  return size_binop (PLUS_EXPR, BINFO_OFFSET (binfo), size);
}

/* Returns the offset of the byte just past the end of the base class
   with the highest offset in T.  If INCLUDE_VIRTUALS_P is zero, then
   only non-virtual bases are included.  */

static tree
end_of_class (t, include_virtuals_p)
     tree t;
     int include_virtuals_p;
{
  tree result = size_zero_node;
  tree binfo;
  tree offset;
  int i;

  for (i = 0; i < CLASSTYPE_N_BASECLASSES (t); ++i)
    {
      binfo = BINFO_BASETYPE (TYPE_BINFO (t), i);

      if (!include_virtuals_p
	  && TREE_VIA_VIRTUAL (binfo) 
	  && BINFO_PRIMARY_BASE_OF (binfo) != TYPE_BINFO (t))
	continue;

      offset = end_of_base (binfo);
      if (INT_CST_LT_UNSIGNED (result, offset))
	result = offset;
    }

  /* G++ 3.2 did not check indirect virtual bases.  */
  if (abi_version_at_least (2) && include_virtuals_p)
    for (binfo = CLASSTYPE_VBASECLASSES (t); 
	 binfo; 
	 binfo = TREE_CHAIN (binfo))
      {
	offset = end_of_base (TREE_VALUE (binfo));
	if (INT_CST_LT_UNSIGNED (result, offset))
	  result = offset;
      }

  return result;
}

/* Warn about bases of T that are inaccessible because they are
   ambiguous.  For example:

     struct S {};
     struct T : public S {};
     struct U : public S, public T {};

   Here, `(S*) new U' is not allowed because there are two `S'
   subobjects of U.  */

static void
warn_about_ambiguous_bases (t)
     tree t;
{
  int i;
  tree vbases;
  tree basetype;

  /* Check direct bases.  */
  for (i = 0; i < CLASSTYPE_N_BASECLASSES (t); ++i)
    {
      basetype = TYPE_BINFO_BASETYPE (t, i);

      if (!lookup_base (t, basetype, ba_ignore | ba_quiet, NULL))
	warning ("direct base `%T' inaccessible in `%T' due to ambiguity",
		 basetype, t);
    }

  /* Check for ambiguous virtual bases.  */
  if (extra_warnings)
    for (vbases = CLASSTYPE_VBASECLASSES (t); 
	 vbases; 
	 vbases = TREE_CHAIN (vbases))
      {
	basetype = BINFO_TYPE (TREE_VALUE (vbases));
	
	if (!lookup_base (t, basetype, ba_ignore | ba_quiet, NULL))
	  warning ("virtual base `%T' inaccessible in `%T' due to ambiguity",
		   basetype, t);
      }
}

/* Compare two INTEGER_CSTs K1 and K2.  */

static int
splay_tree_compare_integer_csts (k1, k2)
     splay_tree_key k1;
     splay_tree_key k2;
{
  return tree_int_cst_compare ((tree) k1, (tree) k2);
}

/* Increase the size indicated in RLI to account for empty classes
   that are "off the end" of the class.  */

static void
include_empty_classes (record_layout_info rli)
{
  tree eoc;
  tree rli_size;

  /* It might be the case that we grew the class to allocate a
     zero-sized base class.  That won't be reflected in RLI, yet,
     because we are willing to overlay multiple bases at the same
     offset.  However, now we need to make sure that RLI is big enough
     to reflect the entire class.  */
  eoc = end_of_class (rli->t, 
		      CLASSTYPE_AS_BASE (rli->t) != NULL_TREE);
  rli_size = rli_size_unit_so_far (rli);
  if (TREE_CODE (rli_size) == INTEGER_CST
      && INT_CST_LT_UNSIGNED (rli_size, eoc))
    {
      rli->bitpos = round_up (rli->bitpos, BITS_PER_UNIT);
      rli->bitpos 
	= size_binop (PLUS_EXPR, 
		      rli->bitpos,
		      size_binop (MULT_EXPR,
				  convert (bitsizetype,
					   size_binop (MINUS_EXPR,
						       eoc, rli_size)),
				  bitsize_int (BITS_PER_UNIT)));
      normalize_rli (rli);
    }
}

/* Calculate the TYPE_SIZE, TYPE_ALIGN, etc for T.  Calculate
   BINFO_OFFSETs for all of the base-classes.  Position the vtable
   pointer.  Accumulate declared virtual functions on VIRTUALS_P.  */

static void
layout_class_type (tree t, tree *virtuals_p)
{
  tree non_static_data_members;
  tree field;
  tree vptr;
  record_layout_info rli;
  /* Maps offsets (represented as INTEGER_CSTs) to a TREE_LIST of
     types that appear at that offset.  */
  splay_tree empty_base_offsets;
  /* True if the last field layed out was a bit-field.  */
  bool last_field_was_bitfield = false;
  /* The location at which the next field should be inserted.  */
  tree *next_field;
  /* T, as a base class.  */
  tree base_t;

  /* Keep track of the first non-static data member.  */
  non_static_data_members = TYPE_FIELDS (t);

  /* Start laying out the record.  */
  rli = start_record_layout (t);

  /* If possible, we reuse the virtual function table pointer from one
     of our base classes.  */
  determine_primary_base (t);

  /* Create a pointer to our virtual function table.  */
  vptr = create_vtable_ptr (t, virtuals_p);

  /* The vptr is always the first thing in the class.  */
  if (vptr)
    {
      TREE_CHAIN (vptr) = TYPE_FIELDS (t);
      TYPE_FIELDS (t) = vptr;
      next_field = &TREE_CHAIN (vptr);
      place_field (rli, vptr);
    }
  else
    next_field = &TYPE_FIELDS (t);

  /* Build FIELD_DECLs for all of the non-virtual base-types.  */
  empty_base_offsets = splay_tree_new (splay_tree_compare_integer_csts, 
				       NULL, NULL);
  build_base_fields (rli, empty_base_offsets, next_field);
  
  /* Layout the non-static data members.  */
  for (field = non_static_data_members; field; field = TREE_CHAIN (field))
    {
      tree type;
      tree padding;

      /* We still pass things that aren't non-static data members to
	 the back-end, in case it wants to do something with them.  */
      if (TREE_CODE (field) != FIELD_DECL)
	{
	  place_field (rli, field);
	  /* If the static data member has incomplete type, keep track
	     of it so that it can be completed later.  (The handling 
	     of pending statics in finish_record_layout is
	     insufficient; consider:

	       struct S1;
	       struct S2 { static S1 s1; };
	       
             At this point, finish_record_layout will be called, but
	     S1 is still incomplete.)  */
	  if (TREE_CODE (field) == VAR_DECL)
	    maybe_register_incomplete_var (field);
	  continue;
	}

      type = TREE_TYPE (field);

      /* If this field is a bit-field whose width is greater than its
	 type, then there are some special rules for allocating
	 it.  */
      if (DECL_C_BIT_FIELD (field)
	  && INT_CST_LT (TYPE_SIZE (type), DECL_SIZE (field)))
	{
	  integer_type_kind itk;
	  tree integer_type;

	  /* We must allocate the bits as if suitably aligned for the
	     longest integer type that fits in this many bits.  type
	     of the field.  Then, we are supposed to use the left over
	     bits as additional padding.  */
	  for (itk = itk_char; itk != itk_none; ++itk)
	    if (INT_CST_LT (DECL_SIZE (field), 
			    TYPE_SIZE (integer_types[itk])))
	      break;

	  /* ITK now indicates a type that is too large for the
	     field.  We have to back up by one to find the largest
	     type that fits.  */
	  integer_type = integer_types[itk - 1];

	  if (abi_version_at_least (2) && TREE_CODE (t) == UNION_TYPE)
	    /* In a union, the padding field must have the full width
	       of the bit-field; all fields start at offset zero.  */
	    padding = DECL_SIZE (field);
	  else
	    {
	      if (warn_abi && TREE_CODE (t) == UNION_TYPE)
		warning ("size assigned to `%T' may not be "
			 "ABI-compliant and may change in a future "
			 "version of GCC", 
			 t);
	      padding = size_binop (MINUS_EXPR, DECL_SIZE (field),
				    TYPE_SIZE (integer_type));
	    }
	  DECL_SIZE (field) = TYPE_SIZE (integer_type);
	  DECL_ALIGN (field) = TYPE_ALIGN (integer_type);
	  DECL_USER_ALIGN (field) = TYPE_USER_ALIGN (integer_type);
	  layout_nonempty_base_or_field (rli, field, NULL_TREE,
					 empty_base_offsets);
	  /* Now that layout has been performed, set the size of the
	     field to the size of its declared type; the rest of the
	     field is effectively invisible.  */
	  DECL_SIZE (field) = TYPE_SIZE (type);
	  /* We must also reset the DECL_MODE of the field.  */
	  if (abi_version_at_least (2))
	    DECL_MODE (field) = TYPE_MODE (type);
	  else if (warn_abi
		   && DECL_MODE (field) != TYPE_MODE (type))
	    /* Versions of G++ before G++ 3.4 did not reset the
	       DECL_MODE.  */
	    warning ("the offset of `%D' may not be ABI-compliant and may "
		     "change in a future version of GCC", field);
	}
      else
	{
	  padding = NULL_TREE;
	  layout_nonempty_base_or_field (rli, field, NULL_TREE,
					 empty_base_offsets);
	}

      /* Remember the location of any empty classes in FIELD.  */
      if (abi_version_at_least (2))
	record_subobject_offsets (TREE_TYPE (field), 
				  byte_position(field),
				  empty_base_offsets,
				  /*vbases_p=*/1);

      /* If a bit-field does not immediately follow another bit-field,
	 and yet it starts in the middle of a byte, we have failed to
	 comply with the ABI.  */
      if (warn_abi
	  && DECL_C_BIT_FIELD (field) 
	  && !last_field_was_bitfield
	  && !integer_zerop (size_binop (TRUNC_MOD_EXPR,
					 DECL_FIELD_BIT_OFFSET (field),
					 bitsize_unit_node)))
	cp_warning_at ("offset of `%D' is not ABI-compliant and may change in a future version of GCC", 
		       field);

      /* G++ used to use DECL_FIELD_OFFSET as if it were the byte
	 offset of the field.  */
      if (warn_abi 
	  && !tree_int_cst_equal (DECL_FIELD_OFFSET (field),
				  byte_position (field))
	  && contains_empty_class_p (TREE_TYPE (field)))
	cp_warning_at ("`%D' contains empty classes which may cause base "
		       "classes to be placed at different locations in a "
		       "future version of GCC",
		       field);

      /* If we needed additional padding after this field, add it
	 now.  */
      if (padding)
	{
	  tree padding_field;

	  padding_field = build_decl (FIELD_DECL, 
				      NULL_TREE,
				      char_type_node); 
	  DECL_BIT_FIELD (padding_field) = 1;
	  DECL_SIZE (padding_field) = padding;
	  DECL_ALIGN (padding_field) = 1;
	  DECL_USER_ALIGN (padding_field) = 0;
	  layout_nonempty_base_or_field (rli, padding_field,
					 NULL_TREE, 
					 empty_base_offsets);
	}

      last_field_was_bitfield = DECL_C_BIT_FIELD (field);
    }

  if (abi_version_at_least (2) && !integer_zerop (rli->bitpos))
    {
      /* Make sure that we are on a byte boundary so that the size of
	 the class without virtual bases will always be a round number
	 of bytes.  */
      rli->bitpos = round_up (rli->bitpos, BITS_PER_UNIT);
      normalize_rli (rli);
    }

  /* G++ 3.2 does not allow virtual bases to be overlaid with tail
     padding.  */
  if (!abi_version_at_least (2))
    include_empty_classes(rli);

  /* Delete all zero-width bit-fields from the list of fields.  Now
     that the type is laid out they are no longer important.  */
  remove_zero_width_bit_fields (t);

  /* Create the version of T used for virtual bases.  We do not use
     make_aggr_type for this version; this is an artificial type.  For
     a POD type, we just reuse T.  */
  if (CLASSTYPE_NON_POD_P (t) || CLASSTYPE_EMPTY_P (t))
    {
      base_t = make_node (TREE_CODE (t));
      
      /* Set the size and alignment for the new type.  In G++ 3.2, all
	 empty classes were considered to have size zero when used as
	 base classes.  */
      if (!abi_version_at_least (2) && CLASSTYPE_EMPTY_P (t))
	{
	  TYPE_SIZE (base_t) = bitsize_zero_node;
	  TYPE_SIZE_UNIT (base_t) = size_zero_node;
	  if (warn_abi && !integer_zerop (rli_size_unit_so_far (rli)))
	    warning ("layout of classes derived from empty class `%T' "
		     "may change in a future version of GCC",
		     t);
	}
      else
	{
	  tree eoc;

	  /* If the ABI version is not at least two, and the last
	     field was a bit-field, RLI may not be on a byte
	     boundary.  In particular, rli_size_unit_so_far might
	     indicate the last complete byte, while rli_size_so_far
	     indicates the total number of bits used.  Therefore,
	     rli_size_so_far, rather than rli_size_unit_so_far, is
	     used to compute TYPE_SIZE_UNIT.  */
	  eoc = end_of_class (t, /*include_virtuals_p=*/0);
	  TYPE_SIZE_UNIT (base_t) 
	    = size_binop (MAX_EXPR,
			  convert (sizetype,
				   size_binop (CEIL_DIV_EXPR,
					       rli_size_so_far (rli),
					       bitsize_int (BITS_PER_UNIT))),
			  eoc);
	  TYPE_SIZE (base_t) 
	    = size_binop (MAX_EXPR,
			  rli_size_so_far (rli),
			  size_binop (MULT_EXPR,
				      convert (bitsizetype, eoc),
				      bitsize_int (BITS_PER_UNIT)));
	}
      TYPE_ALIGN (base_t) = rli->record_align;
      TYPE_USER_ALIGN (base_t) = TYPE_USER_ALIGN (t);

      /* Copy the fields from T.  */
      next_field = &TYPE_FIELDS (base_t);
      for (field = TYPE_FIELDS (t); field; field = TREE_CHAIN (field))
	if (TREE_CODE (field) == FIELD_DECL)
	  {
	    *next_field = build_decl (FIELD_DECL,
				      DECL_NAME (field), 
				      TREE_TYPE (field));
	    DECL_CONTEXT (*next_field) = base_t;
	    DECL_FIELD_OFFSET (*next_field) = DECL_FIELD_OFFSET (field);
	    DECL_FIELD_BIT_OFFSET (*next_field)
	      = DECL_FIELD_BIT_OFFSET (field);
	    next_field = &TREE_CHAIN (*next_field);
	  }

      /* Record the base version of the type.  */
      CLASSTYPE_AS_BASE (t) = base_t;
      TYPE_CONTEXT (base_t) = t;
    }
  else
    CLASSTYPE_AS_BASE (t) = t;

  /* Every empty class contains an empty class.  */
  if (CLASSTYPE_EMPTY_P (t))
    CLASSTYPE_CONTAINS_EMPTY_CLASS_P (t) = 1;

  /* Set the TYPE_DECL for this type to contain the right
     value for DECL_OFFSET, so that we can use it as part
     of a COMPONENT_REF for multiple inheritance.  */
  layout_decl (TYPE_MAIN_DECL (t), 0);

  /* Now fix up any virtual base class types that we left lying
     around.  We must get these done before we try to lay out the
     virtual function table.  As a side-effect, this will remove the
     base subobject fields.  */
  layout_virtual_bases (rli, empty_base_offsets);

  /* Make sure that empty classes are reflected in RLI at this 
     point.  */
  include_empty_classes(rli);

  /* Make sure not to create any structures with zero size.  */
  if (integer_zerop (rli_size_unit_so_far (rli)) && CLASSTYPE_EMPTY_P (t))
    place_field (rli, 
		 build_decl (FIELD_DECL, NULL_TREE, char_type_node));

  /* Let the back-end lay out the type.  */
  finish_record_layout (rli, /*free_p=*/true);

  /* Warn about bases that can't be talked about due to ambiguity.  */
  warn_about_ambiguous_bases (t);

  /* Clean up.  */
  splay_tree_delete (empty_base_offsets);
}

/* Returns the virtual function with which the vtable for TYPE is
   emitted, or NULL_TREE if that heuristic is not applicable to TYPE.  */

static tree
key_method (tree type)
{
  tree method;

  if (TYPE_FOR_JAVA (type)
      || processing_template_decl
      || CLASSTYPE_TEMPLATE_INSTANTIATION (type)
      || CLASSTYPE_INTERFACE_KNOWN (type))
    return NULL_TREE;

  for (method = TYPE_METHODS (type); method != NULL_TREE;
       method = TREE_CHAIN (method))
    if (DECL_VINDEX (method) != NULL_TREE
	&& ! DECL_DECLARED_INLINE_P (method)
	&& ! DECL_PURE_VIRTUAL_P (method))
      return method;

  return NULL_TREE;
}

/* Perform processing required when the definition of T (a class type)
   is complete.  */

void
finish_struct_1 (t)
     tree t;
{
  tree x;
  /* A TREE_LIST.  The TREE_VALUE of each node is a FUNCTION_DECL.  */
  tree virtuals = NULL_TREE;
  int n_fields = 0;
  tree vfield;

  if (COMPLETE_TYPE_P (t))
    {
      if (IS_AGGR_TYPE (t))
	error ("redefinition of `%#T'", t);
      else
	abort ();
      popclass ();
      return;
    }

  /* If this type was previously laid out as a forward reference,
     make sure we lay it out again.  */
  TYPE_SIZE (t) = NULL_TREE;
  CLASSTYPE_GOT_SEMICOLON (t) = 0;
  CLASSTYPE_PRIMARY_BINFO (t) = NULL_TREE;

  fixup_inline_methods (t);
  
  /* Make assumptions about the class; we'll reset the flags if
     necessary.  */
  CLASSTYPE_EMPTY_P (t) = 1;
  CLASSTYPE_NEARLY_EMPTY_P (t) = 1;
  CLASSTYPE_CONTAINS_EMPTY_CLASS_P (t) = 0;

  /* Do end-of-class semantic processing: checking the validity of the
     bases and members and add implicitly generated methods.  */
  check_bases_and_members (t);

  /* Find the key method */
    if (TYPE_CONTAINS_VPTR_P (t))
    {
      CLASSTYPE_KEY_METHOD (t) = key_method (t);

      /* If a polymorphic class has no key method, we may emit the vtable
	 in every translation unit where the class definition appears. */
      if (CLASSTYPE_KEY_METHOD (t) == NULL_TREE)
	keyed_classes = tree_cons (NULL_TREE, t, keyed_classes);
    }

  /* Layout the class itself.  */
  layout_class_type (t, &virtuals);

  /* Make sure that we get our own copy of the vfield FIELD_DECL.  */
  vfield = TYPE_VFIELD (t);
  if (vfield && CLASSTYPE_HAS_PRIMARY_BASE_P (t))
    {
      tree primary = CLASSTYPE_PRIMARY_BINFO (t);

      my_friendly_assert (same_type_p (DECL_FIELD_CONTEXT (vfield),
				       BINFO_TYPE (primary)),
			  20010726);
      /* The vtable better be at the start.  */
      my_friendly_assert (integer_zerop (DECL_FIELD_OFFSET (vfield)),
			  20010726);
      my_friendly_assert (integer_zerop (BINFO_OFFSET (primary)),
			  20010726);
      
      vfield = copy_decl (vfield);
      DECL_FIELD_CONTEXT (vfield) = t;
      TYPE_VFIELD (t) = vfield;
    }
  else
    my_friendly_assert (!vfield || DECL_FIELD_CONTEXT (vfield) == t, 20010726);

  virtuals = modify_all_vtables (t, nreverse (virtuals));

  /* If we created a new vtbl pointer for this class, add it to the
     list.  */
  if (TYPE_VFIELD (t) && !CLASSTYPE_HAS_PRIMARY_BASE_P (t))
    CLASSTYPE_VFIELDS (t) 
      = chainon (CLASSTYPE_VFIELDS (t), build_tree_list (NULL_TREE, t));

  /* If necessary, create the primary vtable for this class.  */
  if (virtuals || TYPE_CONTAINS_VPTR_P (t))
    {
      /* We must enter these virtuals into the table.  */
      if (!CLASSTYPE_HAS_PRIMARY_BASE_P (t))
	build_primary_vtable (NULL_TREE, t);
      else if (! BINFO_NEW_VTABLE_MARKED (TYPE_BINFO (t), t))
	/* Here we know enough to change the type of our virtual
	   function table, but we will wait until later this function.  */
	build_primary_vtable (CLASSTYPE_PRIMARY_BINFO (t), t);
    }

  if (TYPE_CONTAINS_VPTR_P (t))
    {
      int vindex;
      tree fn;

      if (TYPE_BINFO_VTABLE (t))
	my_friendly_assert (DECL_VIRTUAL_P (TYPE_BINFO_VTABLE (t)),
			    20000116);
      if (!CLASSTYPE_HAS_PRIMARY_BASE_P (t))
	my_friendly_assert (TYPE_BINFO_VIRTUALS (t) == NULL_TREE,
			    20000116);

      /* Add entries for virtual functions introduced by this class.  */
      TYPE_BINFO_VIRTUALS (t) = chainon (TYPE_BINFO_VIRTUALS (t), virtuals);

      /* Set DECL_VINDEX for all functions declared in this class.  */
      for (vindex = 0, fn = BINFO_VIRTUALS (TYPE_BINFO (t)); 
	   fn; 
	   fn = TREE_CHAIN (fn), 
	     vindex += (TARGET_VTABLE_USES_DESCRIPTORS
			? TARGET_VTABLE_USES_DESCRIPTORS : 1))
	if (TREE_CODE (DECL_VINDEX (BV_FN (fn))) != INTEGER_CST)
	  DECL_VINDEX (BV_FN (fn)) = build_shared_int_cst (vindex);
    }

  finish_struct_bits (t);

  /* Complete the rtl for any static member objects of the type we're
     working on.  */
  for (x = TYPE_FIELDS (t); x; x = TREE_CHAIN (x))
    if (TREE_CODE (x) == VAR_DECL && TREE_STATIC (x)
	&& same_type_p (TYPE_MAIN_VARIANT (TREE_TYPE (x)), t))
      DECL_MODE (x) = TYPE_MODE (t);

  /* Done with FIELDS...now decide whether to sort these for
     faster lookups later.

     We use a small number because most searches fail (succeeding
     ultimately as the search bores through the inheritance
     hierarchy), and we want this failure to occur quickly.  */

  n_fields = count_fields (TYPE_FIELDS (t));
  if (n_fields > 7)
    {
      tree field_vec = make_tree_vec (n_fields);
      add_fields_to_vec (TYPE_FIELDS (t), field_vec, 0);
      qsort (&TREE_VEC_ELT (field_vec, 0), n_fields, sizeof (tree),
	     (int (*)(const void *, const void *))field_decl_cmp);
      if (! DECL_LANG_SPECIFIC (TYPE_MAIN_DECL (t)))
	retrofit_lang_decl (TYPE_MAIN_DECL (t));
      DECL_SORTED_FIELDS (TYPE_MAIN_DECL (t)) = field_vec;
    }

  if (TYPE_HAS_CONSTRUCTOR (t))
    {
      tree vfields = CLASSTYPE_VFIELDS (t);

      for (vfields = CLASSTYPE_VFIELDS (t);
	   vfields; vfields = TREE_CHAIN (vfields))
	/* Mark the fact that constructor for T could affect anybody
	   inheriting from T who wants to initialize vtables for
	   VFIELDS's type.  */
	if (VF_BINFO_VALUE (vfields))
	  TREE_ADDRESSABLE (vfields) = 1;
    }

  /* Make the rtl for any new vtables we have created, and unmark
     the base types we marked.  */
  finish_vtbls (t);
  
  /* Build the VTT for T.  */
  build_vtt (t);

  if (warn_nonvdtor && TYPE_POLYMORPHIC_P (t) && TYPE_HAS_DESTRUCTOR (t)
      && DECL_VINDEX (TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (t), 1)) == NULL_TREE)
    warning ("`%#T' has virtual functions but non-virtual destructor", t);

  complete_vars (t);

  if (warn_overloaded_virtual)
    warn_hidden (t);

  maybe_suppress_debug_info (t);

  dump_class_hierarchy (t);
  
  /* Finish debugging output for this type.  */
  rest_of_type_compilation (t, ! LOCAL_CLASS_P (t));
}

/* When T was built up, the member declarations were added in reverse
   order.  Rearrange them to declaration order.  */

void
unreverse_member_declarations (t)
     tree t;
{
  tree next;
  tree prev;
  tree x;

  /* The following lists are all in reverse order.  Put them in
     declaration order now.  */
  TYPE_METHODS (t) = nreverse (TYPE_METHODS (t));
  CLASSTYPE_DECL_LIST (t) = nreverse (CLASSTYPE_DECL_LIST (t));

  /* Actually, for the TYPE_FIELDS, only the non TYPE_DECLs are in
     reverse order, so we can't just use nreverse.  */
  prev = NULL_TREE;
  for (x = TYPE_FIELDS (t); 
       x && TREE_CODE (x) != TYPE_DECL; 
       x = next)
    {
      next = TREE_CHAIN (x);
      TREE_CHAIN (x) = prev;
      prev = x;
    }
  if (prev)
    {
      TREE_CHAIN (TYPE_FIELDS (t)) = x;
      if (prev)
	TYPE_FIELDS (t) = prev;
    }
}

tree
finish_struct (t, attributes)
     tree t, attributes;
{
  const char *saved_filename = input_filename;
  int saved_lineno = lineno;

  /* Now that we've got all the field declarations, reverse everything
     as necessary.  */
  unreverse_member_declarations (t);

  cplus_decl_attributes (&t, attributes, (int) ATTR_FLAG_TYPE_IN_PLACE);

  /* Nadger the current location so that diagnostics point to the start of
     the struct, not the end.  */
  input_filename = DECL_SOURCE_FILE (TYPE_NAME (t));
  lineno = DECL_SOURCE_LINE (TYPE_NAME (t));

  if (processing_template_decl)
    {
      finish_struct_methods (t);
      TYPE_SIZE (t) = bitsize_zero_node;
    }
  else
    finish_struct_1 (t);

  input_filename = saved_filename;
  lineno = saved_lineno;

  TYPE_BEING_DEFINED (t) = 0;

  if (current_class_type)
    popclass ();
  else
    error ("trying to finish struct, but kicked out due to previous parse errors");

  if (processing_template_decl && at_function_scope_p ())
    add_stmt (build_min (TAG_DEFN, t));

  return t;
}

/* Return the dynamic type of INSTANCE, if known.
   Used to determine whether the virtual function table is needed
   or not.

   *NONNULL is set iff INSTANCE can be known to be nonnull, regardless
   of our knowledge of its type.  *NONNULL should be initialized
   before this function is called.  */

static tree
fixed_type_or_null (instance, nonnull, cdtorp)
     tree instance;
     int *nonnull;
     int *cdtorp;
{
  switch (TREE_CODE (instance))
    {
    case INDIRECT_REF:
      if (POINTER_TYPE_P (TREE_TYPE (instance)))
	return NULL_TREE;
      else
	return fixed_type_or_null (TREE_OPERAND (instance, 0),
				   nonnull, cdtorp);

    case CALL_EXPR:
      /* This is a call to a constructor, hence it's never zero.  */
      if (TREE_HAS_CONSTRUCTOR (instance))
	{
	  if (nonnull)
	    *nonnull = 1;
	  return TREE_TYPE (instance);
	}
      return NULL_TREE;

    case SAVE_EXPR:
      /* This is a call to a constructor, hence it's never zero.  */
      if (TREE_HAS_CONSTRUCTOR (instance))
	{
	  if (nonnull)
	    *nonnull = 1;
	  return TREE_TYPE (instance);
	}
      return fixed_type_or_null (TREE_OPERAND (instance, 0), nonnull, cdtorp);

    case RTL_EXPR:
      return NULL_TREE;

    case PLUS_EXPR:
    case MINUS_EXPR:
      if (TREE_CODE (TREE_OPERAND (instance, 0)) == ADDR_EXPR)
	return fixed_type_or_null (TREE_OPERAND (instance, 0), nonnull, cdtorp);
      if (TREE_CODE (TREE_OPERAND (instance, 1)) == INTEGER_CST)
	/* Propagate nonnull.  */
	return fixed_type_or_null (TREE_OPERAND (instance, 0), nonnull, cdtorp);
      return NULL_TREE;

    case NOP_EXPR:
    case CONVERT_EXPR:
      return fixed_type_or_null (TREE_OPERAND (instance, 0), nonnull, cdtorp);

    case ADDR_EXPR:
      if (nonnull)
	*nonnull = 1;
      return fixed_type_or_null (TREE_OPERAND (instance, 0), nonnull, cdtorp);

    case COMPONENT_REF:
      return fixed_type_or_null (TREE_OPERAND (instance, 1), nonnull, cdtorp);

    case VAR_DECL:
    case FIELD_DECL:
      if (TREE_CODE (TREE_TYPE (instance)) == ARRAY_TYPE
	  && IS_AGGR_TYPE (TREE_TYPE (TREE_TYPE (instance))))
	{
	  if (nonnull)
	    *nonnull = 1;
	  return TREE_TYPE (TREE_TYPE (instance));
	}
      /* fall through...  */
    case TARGET_EXPR:
    case PARM_DECL:
    case RESULT_DECL:
      if (IS_AGGR_TYPE (TREE_TYPE (instance)))
	{
	  if (nonnull)
	    *nonnull = 1;
	  return TREE_TYPE (instance);
	}
      else if (instance == current_class_ptr)
        {
          if (nonnull)
            *nonnull = 1;
        
          /* if we're in a ctor or dtor, we know our type.  */
          if (DECL_LANG_SPECIFIC (current_function_decl)
              && (DECL_CONSTRUCTOR_P (current_function_decl)
                  || DECL_DESTRUCTOR_P (current_function_decl)))
            {
              if (cdtorp)
                *cdtorp = 1;
              return TREE_TYPE (TREE_TYPE (instance));
            }
        }
      else if (TREE_CODE (TREE_TYPE (instance)) == REFERENCE_TYPE)
        {
          /* Reference variables should be references to objects.  */
          if (nonnull)
	    *nonnull = 1;
	  
	  /* DECL_VAR_MARKED_P is used to prevent recursion; a
	     variable's initializer may refer to the variable
	     itself.  */
	  if (TREE_CODE (instance) == VAR_DECL 
	      && DECL_INITIAL (instance)
	      && !DECL_VAR_MARKED_P (instance))
	    {
	      tree type;
	      DECL_VAR_MARKED_P (instance) = 1;
	      type = fixed_type_or_null (DECL_INITIAL (instance),
					 nonnull, cdtorp);
	      DECL_VAR_MARKED_P (instance) = 0;
	      return type;
	    }
	}
      return NULL_TREE;

    default:
      return NULL_TREE;
    }
}

/* Return nonzero if the dynamic type of INSTANCE is known, and
   equivalent to the static type.  We also handle the case where
   INSTANCE is really a pointer. Return negative if this is a
   ctor/dtor. There the dynamic type is known, but this might not be
   the most derived base of the original object, and hence virtual
   bases may not be layed out according to this type.

   Used to determine whether the virtual function table is needed
   or not.

   *NONNULL is set iff INSTANCE can be known to be nonnull, regardless
   of our knowledge of its type.  *NONNULL should be initialized
   before this function is called.  */

int
resolves_to_fixed_type_p (instance, nonnull)
     tree instance;
     int *nonnull;
{
  tree t = TREE_TYPE (instance);
  int cdtorp = 0;
  
  tree fixed = fixed_type_or_null (instance, nonnull, &cdtorp);
  if (fixed == NULL_TREE)
    return 0;
  if (POINTER_TYPE_P (t))
    t = TREE_TYPE (t);
  if (!same_type_ignoring_top_level_qualifiers_p (t, fixed))
    return 0;
  return cdtorp ? -1 : 1;
}


void
init_class_processing ()
{
  current_class_depth = 0;
  current_class_stack_size = 10;
  current_class_stack 
    = (class_stack_node_t) xmalloc (current_class_stack_size 
				    * sizeof (struct class_stack_node));
  VARRAY_TREE_INIT (local_classes, 8, "local_classes");

  access_default_node = build_int_2 (0, 0);
  access_public_node = build_int_2 (ak_public, 0);
  access_protected_node = build_int_2 (ak_protected, 0);
  access_private_node = build_int_2 (ak_private, 0);
  access_default_virtual_node = build_int_2 (4, 0);
  access_public_virtual_node = build_int_2 (4 | ak_public, 0);
  access_protected_virtual_node = build_int_2 (4 | ak_protected, 0);
  access_private_virtual_node = build_int_2 (4 | ak_private, 0);

  ridpointers[(int) RID_PUBLIC] = access_public_node;
  ridpointers[(int) RID_PRIVATE] = access_private_node;
  ridpointers[(int) RID_PROTECTED] = access_protected_node;
}

/* Set current scope to NAME. CODE tells us if this is a
   STRUCT, UNION, or ENUM environment.

   NAME may end up being NULL_TREE if this is an anonymous or
   late-bound struct (as in "struct { ... } foo;")  */

/* Set global variables CURRENT_CLASS_NAME and CURRENT_CLASS_TYPE to
   appropriate values, found by looking up the type definition of
   NAME (as a CODE).

   If MODIFY is 1, we set IDENTIFIER_CLASS_VALUE's of names
   which can be seen locally to the class.  They are shadowed by
   any subsequent local declaration (including parameter names).

   If MODIFY is 2, we set IDENTIFIER_CLASS_VALUE's of names
   which have static meaning (i.e., static members, static
   member functions, enum declarations, etc).

   If MODIFY is 3, we set IDENTIFIER_CLASS_VALUE of names
   which can be seen locally to the class (as in 1), but
   know that we are doing this for declaration purposes
   (i.e. friend foo::bar (int)).

   So that we may avoid calls to lookup_name, we cache the _TYPE
   nodes of local TYPE_DECLs in the TREE_TYPE field of the name.

   For multiple inheritance, we perform a two-pass depth-first search
   of the type lattice.  The first pass performs a pre-order search,
   marking types after the type has had its fields installed in
   the appropriate IDENTIFIER_CLASS_VALUE slot.  The second pass merely
   unmarks the marked types.  If a field or member function name
   appears in an ambiguous way, the IDENTIFIER_CLASS_VALUE of
   that name becomes `error_mark_node'.  */

void
pushclass (type, modify)
     tree type;
     int modify;
{
  type = TYPE_MAIN_VARIANT (type);

  /* Make sure there is enough room for the new entry on the stack.  */
  if (current_class_depth + 1 >= current_class_stack_size) 
    {
      current_class_stack_size *= 2;
      current_class_stack
	= (class_stack_node_t) xrealloc (current_class_stack,
					 current_class_stack_size
					 * sizeof (struct class_stack_node));
    }

  /* Insert a new entry on the class stack.  */
  current_class_stack[current_class_depth].name = current_class_name;
  current_class_stack[current_class_depth].type = current_class_type;
  current_class_stack[current_class_depth].access = current_access_specifier;
  current_class_stack[current_class_depth].names_used = 0;
  current_class_depth++;

  /* Now set up the new type.  */
  current_class_name = TYPE_NAME (type);
  if (TREE_CODE (current_class_name) == TYPE_DECL)
    current_class_name = DECL_NAME (current_class_name);
  current_class_type = type;

  /* By default, things in classes are private, while things in
     structures or unions are public.  */
  current_access_specifier = (CLASSTYPE_DECLARED_CLASS (type) 
			      ? access_private_node 
			      : access_public_node);

  if (previous_class_type != NULL_TREE
      && (type != previous_class_type 
	  || !COMPLETE_TYPE_P (previous_class_type))
      && current_class_depth == 1)
    {
      /* Forcibly remove any old class remnants.  */
      invalidate_class_lookup_cache ();
    }

  /* If we're about to enter a nested class, clear
     IDENTIFIER_CLASS_VALUE for the enclosing classes.  */
  if (modify && current_class_depth > 1)
    clear_identifier_class_values ();

  pushlevel_class ();

  if (modify)
    {
      if (type != previous_class_type || current_class_depth > 1)
	push_class_decls (type);
      else
	{
	  tree item;

	  /* We are re-entering the same class we just left, so we
	     don't have to search the whole inheritance matrix to find
	     all the decls to bind again.  Instead, we install the
	     cached class_shadowed list, and walk through it binding
	     names and setting up IDENTIFIER_TYPE_VALUEs.  */
	  set_class_shadows (previous_class_values);
	  for (item = previous_class_values; item; item = TREE_CHAIN (item))
	    {
	      tree id = TREE_PURPOSE (item);
	      tree decl = TREE_TYPE (item);

	      push_class_binding (id, decl);
	      if (TREE_CODE (decl) == TYPE_DECL)
		set_identifier_type_value (id, TREE_TYPE (decl));
	    }
	  unuse_fields (type);
	}

      cxx_remember_type_decls (CLASSTYPE_NESTED_UDTS (type));
    }
}

/* When we exit a toplevel class scope, we save the
   IDENTIFIER_CLASS_VALUEs so that we can restore them quickly if we
   reenter the class.  Here, we've entered some other class, so we
   must invalidate our cache.  */

void
invalidate_class_lookup_cache ()
{
  tree t;
  
  /* The IDENTIFIER_CLASS_VALUEs are no longer valid.  */
  for (t = previous_class_values; t; t = TREE_CHAIN (t))
    IDENTIFIER_CLASS_VALUE (TREE_PURPOSE (t)) = NULL_TREE;

  previous_class_values = NULL_TREE;
  previous_class_type = NULL_TREE;
}
 
/* Get out of the current class scope. If we were in a class scope
   previously, that is the one popped to.  */

void
popclass ()
{
  poplevel_class ();
  pop_class_decls ();

  current_class_depth--;
  current_class_name = current_class_stack[current_class_depth].name;
  current_class_type = current_class_stack[current_class_depth].type;
  current_access_specifier = current_class_stack[current_class_depth].access;
  if (current_class_stack[current_class_depth].names_used)
    splay_tree_delete (current_class_stack[current_class_depth].names_used);
}

/* Returns 1 if current_class_type is either T or a nested type of T.
   We start looking from 1 because entry 0 is from global scope, and has
   no type.  */

int
currently_open_class (t)
     tree t;
{
  int i;
  if (t == current_class_type)
    return 1;
  for (i = 1; i < current_class_depth; ++i)
    if (current_class_stack [i].type == t)
      return 1;
  return 0;
}

/* If either current_class_type or one of its enclosing classes are derived
   from T, return the appropriate type.  Used to determine how we found
   something via unqualified lookup.  */

tree
currently_open_derived_class (t)
     tree t;
{
  int i;

  if (DERIVED_FROM_P (t, current_class_type))
    return current_class_type;

  for (i = current_class_depth - 1; i > 0; --i)
    if (DERIVED_FROM_P (t, current_class_stack[i].type))
      return current_class_stack[i].type;

  return NULL_TREE;
}

/* When entering a class scope, all enclosing class scopes' names with
   static meaning (static variables, static functions, types and enumerators)
   have to be visible.  This recursive function calls pushclass for all
   enclosing class contexts until global or a local scope is reached.
   TYPE is the enclosed class and MODIFY is equivalent with the pushclass
   formal of the same name.  */

void
push_nested_class (type, modify)
     tree type;
     int modify;
{
  tree context;

  /* A namespace might be passed in error cases, like A::B:C.  */
  if (type == NULL_TREE 
      || type == error_mark_node 
      || TREE_CODE (type) == NAMESPACE_DECL
      || ! IS_AGGR_TYPE (type)
      || TREE_CODE (type) == TEMPLATE_TYPE_PARM
      || TREE_CODE (type) == BOUND_TEMPLATE_TEMPLATE_PARM)
    return;
  
  context = DECL_CONTEXT (TYPE_MAIN_DECL (type));

  if (context && CLASS_TYPE_P (context))
    push_nested_class (context, 2);
  pushclass (type, modify);
}

/* Undoes a push_nested_class call.  MODIFY is passed on to popclass.  */

void
pop_nested_class ()
{
  tree context = DECL_CONTEXT (TYPE_MAIN_DECL (current_class_type));

  popclass ();
  if (context && CLASS_TYPE_P (context))
    pop_nested_class ();
}

/* Returns the number of extern "LANG" blocks we are nested within.  */

int
current_lang_depth ()
{
  return VARRAY_ACTIVE_SIZE (current_lang_base);
}

/* Set global variables CURRENT_LANG_NAME to appropriate value
   so that behavior of name-mangling machinery is correct.  */

void
push_lang_context (name)
     tree name;
{
  VARRAY_PUSH_TREE (current_lang_base, current_lang_name);

  if (name == lang_name_cplusplus)
    {
      current_lang_name = name;
    }
  else if (name == lang_name_java)
    {
      current_lang_name = name;
      /* DECL_IGNORED_P is initially set for these types, to avoid clutter.
	 (See record_builtin_java_type in decl.c.)  However, that causes
	 incorrect debug entries if these types are actually used.
	 So we re-enable debug output after extern "Java".  */
      DECL_IGNORED_P (TYPE_NAME (java_byte_type_node)) = 0;
      DECL_IGNORED_P (TYPE_NAME (java_short_type_node)) = 0;
      DECL_IGNORED_P (TYPE_NAME (java_int_type_node)) = 0;
      DECL_IGNORED_P (TYPE_NAME (java_long_type_node)) = 0;
      DECL_IGNORED_P (TYPE_NAME (java_float_type_node)) = 0;
      DECL_IGNORED_P (TYPE_NAME (java_double_type_node)) = 0;
      DECL_IGNORED_P (TYPE_NAME (java_char_type_node)) = 0;
      DECL_IGNORED_P (TYPE_NAME (java_boolean_type_node)) = 0;
    }
  else if (name == lang_name_c)
    {
      current_lang_name = name;
    }
  else
    error ("language string `\"%s\"' not recognized", IDENTIFIER_POINTER (name));
}
  
/* Get out of the current language scope.  */

void
pop_lang_context ()
{
  current_lang_name = VARRAY_TOP_TREE (current_lang_base);
  VARRAY_POP (current_lang_base);
}

/* Type instantiation routines.  */

/* Given an OVERLOAD and a TARGET_TYPE, return the function that
   matches the TARGET_TYPE.  If there is no satisfactory match, return
   error_mark_node, and issue an error message if COMPLAIN is
   nonzero.  Permit pointers to member function if PTRMEM is nonzero.
   If TEMPLATE_ONLY, the name of the overloaded function
   was a template-id, and EXPLICIT_TARGS are the explicitly provided
   template arguments.  */

static tree
resolve_address_of_overloaded_function (target_type, 
					overload,
					flags,
	                                ptrmem,
					template_only,
					explicit_targs)
     tree target_type;
     tree overload;
     tsubst_flags_t flags;
     int ptrmem;
     int template_only;
     tree explicit_targs;
{
  /* Here's what the standard says:
     
       [over.over]

       If the name is a function template, template argument deduction
       is done, and if the argument deduction succeeds, the deduced
       arguments are used to generate a single template function, which
       is added to the set of overloaded functions considered.

       Non-member functions and static member functions match targets of
       type "pointer-to-function" or "reference-to-function."  Nonstatic
       member functions match targets of type "pointer-to-member
       function;" the function type of the pointer to member is used to
       select the member function from the set of overloaded member
       functions.  If a nonstatic member function is selected, the
       reference to the overloaded function name is required to have the
       form of a pointer to member as described in 5.3.1.

       If more than one function is selected, any template functions in
       the set are eliminated if the set also contains a non-template
       function, and any given template function is eliminated if the
       set contains a second template function that is more specialized
       than the first according to the partial ordering rules 14.5.5.2.
       After such eliminations, if any, there shall remain exactly one
       selected function.  */

  int is_ptrmem = 0;
  int is_reference = 0;
  /* We store the matches in a TREE_LIST rooted here.  The functions
     are the TREE_PURPOSE, not the TREE_VALUE, in this list, for easy
     interoperability with most_specialized_instantiation.  */
  tree matches = NULL_TREE;
  tree fn;

  /* By the time we get here, we should be seeing only real
     pointer-to-member types, not the internal POINTER_TYPE to
     METHOD_TYPE representation.  */
  my_friendly_assert (!(TREE_CODE (target_type) == POINTER_TYPE
			&& (TREE_CODE (TREE_TYPE (target_type)) 
			    == METHOD_TYPE)), 0);

  if (TREE_CODE (overload) == COMPONENT_REF)
    overload = TREE_OPERAND (overload, 1);

  /* Check that the TARGET_TYPE is reasonable.  */
  if (TYPE_PTRFN_P (target_type))
    /* This is OK.  */;
  else if (TYPE_PTRMEMFUNC_P (target_type))
    /* This is OK, too.  */
    is_ptrmem = 1;
  else if (TREE_CODE (target_type) == FUNCTION_TYPE)
    {
      /* This is OK, too.  This comes from a conversion to reference
	 type.  */
      target_type = build_reference_type (target_type);
      is_reference = 1;
    }
  else 
    {
      if (flags & tf_error)
	error ("\
cannot resolve overloaded function `%D' based on conversion to type `%T'", 
		  DECL_NAME (OVL_FUNCTION (overload)), target_type);
      return error_mark_node;
    }
  
  /* If we can find a non-template function that matches, we can just
     use it.  There's no point in generating template instantiations
     if we're just going to throw them out anyhow.  But, of course, we
     can only do this when we don't *need* a template function.  */
  if (!template_only)
    {
      tree fns;

      for (fns = overload; fns; fns = OVL_CHAIN (fns))
	{
	  tree fn = OVL_FUNCTION (fns);
	  tree fntype;

	  if (TREE_CODE (fn) == TEMPLATE_DECL)
	    /* We're not looking for templates just yet.  */
	    continue;

	  if ((TREE_CODE (TREE_TYPE (fn)) == METHOD_TYPE)
	      != is_ptrmem)
	    /* We're looking for a non-static member, and this isn't
	       one, or vice versa.  */
	    continue;

	  /* Ignore anticipated decls of undeclared builtins.  */
	  if (DECL_ANTICIPATED (fn))
	    continue;

	  /* See if there's a match.  */
	  fntype = TREE_TYPE (fn);
	  if (is_ptrmem)
	    fntype = build_ptrmemfunc_type (build_pointer_type (fntype));
	  else if (!is_reference)
	    fntype = build_pointer_type (fntype);

	  if (can_convert_arg (target_type, fntype, fn))
	    matches = tree_cons (fn, NULL_TREE, matches);
	}
    }

  /* Now, if we've already got a match (or matches), there's no need
     to proceed to the template functions.  But, if we don't have a
     match we need to look at them, too.  */
  if (!matches) 
    {
      tree target_fn_type;
      tree target_arg_types;
      tree target_ret_type;
      tree fns;

      if (is_ptrmem)
	target_fn_type
	  = TREE_TYPE (TYPE_PTRMEMFUNC_FN_TYPE (target_type));
      else
	target_fn_type = TREE_TYPE (target_type);
      target_arg_types = TYPE_ARG_TYPES (target_fn_type);
      target_ret_type = TREE_TYPE (target_fn_type);

      /* Never do unification on the 'this' parameter.  */
      if (TREE_CODE (target_fn_type) == METHOD_TYPE)
	target_arg_types = TREE_CHAIN (target_arg_types);
	  
      for (fns = overload; fns; fns = OVL_CHAIN (fns))
	{
	  tree fn = OVL_FUNCTION (fns);
	  tree instantiation;
	  tree instantiation_type;
	  tree targs;

	  if (TREE_CODE (fn) != TEMPLATE_DECL)
	    /* We're only looking for templates.  */
	    continue;

	  if ((TREE_CODE (TREE_TYPE (fn)) == METHOD_TYPE)
	      != is_ptrmem)
	    /* We're not looking for a non-static member, and this is
	       one, or vice versa.  */
	    continue;

	  /* Try to do argument deduction.  */
	  targs = make_tree_vec (DECL_NTPARMS (fn));
	  if (fn_type_unification (fn, explicit_targs, targs,
				   target_arg_types, target_ret_type,
				   DEDUCE_EXACT, -1) != 0)
	    /* Argument deduction failed.  */
	    continue;

	  /* Instantiate the template.  */
	  instantiation = instantiate_template (fn, targs);
	  if (instantiation == error_mark_node)
	    /* Instantiation failed.  */
	    continue;

	  /* See if there's a match.  */
	  instantiation_type = TREE_TYPE (instantiation);
	  if (is_ptrmem)
	    instantiation_type = 
	      build_ptrmemfunc_type (build_pointer_type (instantiation_type));
	  else if (!is_reference)
	    instantiation_type = build_pointer_type (instantiation_type);
	  if (can_convert_arg (target_type, instantiation_type, instantiation))
	    matches = tree_cons (instantiation, fn, matches);
	}

      /* Now, remove all but the most specialized of the matches.  */
      if (matches)
	{
	  tree match = most_specialized_instantiation (matches);

	  if (match != error_mark_node)
	    matches = tree_cons (match, NULL_TREE, NULL_TREE);
	}
    }

  /* Now we should have exactly one function in MATCHES.  */
  if (matches == NULL_TREE)
    {
      /* There were *no* matches.  */
      if (flags & tf_error)
	{
 	  error ("no matches converting function `%D' to type `%#T'", 
		    DECL_NAME (OVL_FUNCTION (overload)),
		    target_type);

	  /* print_candidates expects a chain with the functions in
             TREE_VALUE slots, so we cons one up here (we're losing anyway,
             so why be clever?).  */
          for (; overload; overload = OVL_NEXT (overload))
            matches = tree_cons (NULL_TREE, OVL_CURRENT (overload),
				 matches);
          
	  print_candidates (matches);
	}
      return error_mark_node;
    }
  else if (TREE_CHAIN (matches))
    {
      /* There were too many matches.  */

      if (flags & tf_error)
	{
	  tree match;

 	  error ("converting overloaded function `%D' to type `%#T' is ambiguous", 
		    DECL_NAME (OVL_FUNCTION (overload)),
		    target_type);

	  /* Since print_candidates expects the functions in the
	     TREE_VALUE slot, we flip them here.  */
	  for (match = matches; match; match = TREE_CHAIN (match))
	    TREE_VALUE (match) = TREE_PURPOSE (match);

	  print_candidates (matches);
	}
      
      return error_mark_node;
    }

  /* Good, exactly one match.  Now, convert it to the correct type.  */
  fn = TREE_PURPOSE (matches);

  if (DECL_NONSTATIC_MEMBER_FUNCTION_P (fn)
      && !ptrmem && !flag_ms_extensions)
    {
      static int explained;
      
      if (!(flags & tf_error))
        return error_mark_node;

      pedwarn ("assuming pointer to member `%D'", fn);
      if (!explained)
        {
          pedwarn ("(a pointer to member can only be formed with `&%E')", fn);
          explained = 1;
        }
    }

  /* If we're doing overload resolution purely for the purpose of
     determining conversion sequences, we should not consider the
     function used.  If this conversion sequence is selected, the
     function will be marked as used at this point.  */
  if (!(flags & tf_conv))
    mark_used (fn);

  if (TYPE_PTRFN_P (target_type) || TYPE_PTRMEMFUNC_P (target_type))
    return build_unary_op (ADDR_EXPR, fn, 0);
  else
    {
      /* The target must be a REFERENCE_TYPE.  Above, build_unary_op
	 will mark the function as addressed, but here we must do it
	 explicitly.  */
      cxx_mark_addressable (fn);

      return fn;
    }
}

/* This function will instantiate the type of the expression given in
   RHS to match the type of LHSTYPE.  If errors exist, then return
   error_mark_node. FLAGS is a bit mask.  If ITF_COMPLAIN is set, then
   we complain on errors.  If we are not complaining, never modify rhs,
   as overload resolution wants to try many possible instantiations, in
   the hope that at least one will work.
   
   For non-recursive calls, LHSTYPE should be a function, pointer to
   function, or a pointer to member function.  */

tree
instantiate_type (lhstype, rhs, flags)
     tree lhstype, rhs;
     tsubst_flags_t flags;
{
  tsubst_flags_t flags_in = flags;
  int complain = (flags & tf_error);
  int strict = (flags & tf_no_attributes)
               ? COMPARE_NO_ATTRIBUTES : COMPARE_STRICT;
  int allow_ptrmem = flags & tf_ptrmem_ok;
  
  flags &= ~tf_ptrmem_ok;
  
  if (TREE_CODE (lhstype) == UNKNOWN_TYPE)
    {
      if (complain)
	error ("not enough type information");
      return error_mark_node;
    }

  if (TREE_TYPE (rhs) != NULL_TREE && ! (type_unknown_p (rhs)))
    {
      if (comptypes (lhstype, TREE_TYPE (rhs), strict))
	return rhs;
      if (complain)
	error ("argument of type `%T' does not match `%T'",
		  TREE_TYPE (rhs), lhstype);
      return error_mark_node;
    }

  if (TREE_CODE (rhs) == BASELINK)
    rhs = BASELINK_FUNCTIONS (rhs);

  /* We don't overwrite rhs if it is an overloaded function.
     Copying it would destroy the tree link.  */
  if (TREE_CODE (rhs) != OVERLOAD)
    rhs = copy_node (rhs);

  /* This should really only be used when attempting to distinguish
     what sort of a pointer to function we have.  For now, any
     arithmetic operation which is not supported on pointers
     is rejected as an error.  */

  switch (TREE_CODE (rhs))
    {
    case TYPE_EXPR:
    case CONVERT_EXPR:
    case SAVE_EXPR:
    case CONSTRUCTOR:
    case BUFFER_REF:
      abort ();
      return error_mark_node;

    case INDIRECT_REF:
    case ARRAY_REF:
      {
	tree new_rhs;

	new_rhs = instantiate_type (build_pointer_type (lhstype),
				    TREE_OPERAND (rhs, 0), flags);
	if (new_rhs == error_mark_node)
	  return error_mark_node;

	TREE_TYPE (rhs) = lhstype;
	TREE_OPERAND (rhs, 0) = new_rhs;
	return rhs;
      }

    case NOP_EXPR:
      rhs = copy_node (TREE_OPERAND (rhs, 0));
      TREE_TYPE (rhs) = unknown_type_node;
      return instantiate_type (lhstype, rhs, flags);

    case COMPONENT_REF:
      return instantiate_type (lhstype, TREE_OPERAND (rhs, 1), flags);

    case OFFSET_REF:
      rhs = TREE_OPERAND (rhs, 1);
      if (BASELINK_P (rhs))
	return instantiate_type (lhstype, BASELINK_FUNCTIONS (rhs),
	                         flags | allow_ptrmem);

      /* This can happen if we are forming a pointer-to-member for a
	 member template.  */
      my_friendly_assert (TREE_CODE (rhs) == TEMPLATE_ID_EXPR, 0);

      /* Fall through.  */

    case TEMPLATE_ID_EXPR:
      {
	tree fns = TREE_OPERAND (rhs, 0);
	tree args = TREE_OPERAND (rhs, 1);

	return
	  resolve_address_of_overloaded_function (lhstype,
						  fns,
						  flags_in,
	                                          allow_ptrmem,
						  /*template_only=*/1,
						  args);
      }

    case OVERLOAD:
      return 
	resolve_address_of_overloaded_function (lhstype, 
						rhs,
						flags_in,
	                                        allow_ptrmem,
						/*template_only=*/0,
						/*explicit_targs=*/NULL_TREE);

    case TREE_LIST:
      /* Now we should have a baselink.  */
      my_friendly_assert (BASELINK_P (rhs), 990412);

      return instantiate_type (lhstype, BASELINK_FUNCTIONS (rhs), flags);

    case CALL_EXPR:
      /* This is too hard for now.  */
      abort ();
      return error_mark_node;

    case PLUS_EXPR:
    case MINUS_EXPR:
    case COMPOUND_EXPR:
      TREE_OPERAND (rhs, 0)
	= instantiate_type (lhstype, TREE_OPERAND (rhs, 0), flags);
      if (TREE_OPERAND (rhs, 0) == error_mark_node)
	return error_mark_node;
      TREE_OPERAND (rhs, 1)
	= instantiate_type (lhstype, TREE_OPERAND (rhs, 1), flags);
      if (TREE_OPERAND (rhs, 1) == error_mark_node)
	return error_mark_node;

      TREE_TYPE (rhs) = lhstype;
      return rhs;

    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case RDIV_EXPR:
    case TRUNC_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case ROUND_MOD_EXPR:
    case FIX_ROUND_EXPR:
    case FIX_FLOOR_EXPR:
    case FIX_CEIL_EXPR:
    case FIX_TRUNC_EXPR:
    case FLOAT_EXPR:
    case NEGATE_EXPR:
    case ABS_EXPR:
    case MAX_EXPR:
    case MIN_EXPR:
    case FFS_EXPR:

    case BIT_AND_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:

    case PREINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      if (complain)
	error ("invalid operation on uninstantiated type");
      return error_mark_node;

    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_NOT_EXPR:
      if (complain)
	error ("not enough type information");
      return error_mark_node;

    case COND_EXPR:
      if (type_unknown_p (TREE_OPERAND (rhs, 0)))
	{
	  if (complain)
	    error ("not enough type information");
	  return error_mark_node;
	}
      TREE_OPERAND (rhs, 1)
	= instantiate_type (lhstype, TREE_OPERAND (rhs, 1), flags);
      if (TREE_OPERAND (rhs, 1) == error_mark_node)
	return error_mark_node;
      TREE_OPERAND (rhs, 2)
	= instantiate_type (lhstype, TREE_OPERAND (rhs, 2), flags);
      if (TREE_OPERAND (rhs, 2) == error_mark_node)
	return error_mark_node;

      TREE_TYPE (rhs) = lhstype;
      return rhs;

    case MODIFY_EXPR:
      TREE_OPERAND (rhs, 1)
	= instantiate_type (lhstype, TREE_OPERAND (rhs, 1), flags);
      if (TREE_OPERAND (rhs, 1) == error_mark_node)
	return error_mark_node;

      TREE_TYPE (rhs) = lhstype;
      return rhs;
      
    case ADDR_EXPR:
    {
      if (PTRMEM_OK_P (rhs))
        flags |= tf_ptrmem_ok;
      
      return instantiate_type (lhstype, TREE_OPERAND (rhs, 0), flags);
    }
    case ENTRY_VALUE_EXPR:
      abort ();
      return error_mark_node;

    case ERROR_MARK:
      return error_mark_node;

    default:
      abort ();
      return error_mark_node;
    }
}

/* Return the name of the virtual function pointer field
   (as an IDENTIFIER_NODE) for the given TYPE.  Note that
   this may have to look back through base types to find the
   ultimate field name.  (For single inheritance, these could
   all be the same name.  Who knows for multiple inheritance).  */

static tree
get_vfield_name (type)
     tree type;
{
  tree binfo = TYPE_BINFO (type);
  char *buf;

  while (BINFO_BASETYPES (binfo)
	 && TYPE_CONTAINS_VPTR_P (BINFO_TYPE (BINFO_BASETYPE (binfo, 0)))
	 && ! TREE_VIA_VIRTUAL (BINFO_BASETYPE (binfo, 0)))
    binfo = BINFO_BASETYPE (binfo, 0);

  type = BINFO_TYPE (binfo);
  buf = (char *) alloca (sizeof (VFIELD_NAME_FORMAT)
			 + TYPE_NAME_LENGTH (type) + 2);
  sprintf (buf, VFIELD_NAME_FORMAT,
	   IDENTIFIER_POINTER (constructor_name (type)));
  return get_identifier (buf);
}

void
print_class_statistics ()
{
#ifdef GATHER_STATISTICS
  fprintf (stderr, "convert_harshness = %d\n", n_convert_harshness);
  fprintf (stderr, "compute_conversion_costs = %d\n", n_compute_conversion_costs);
  fprintf (stderr, "build_method_call = %d (inner = %d)\n",
	   n_build_method_call, n_inner_fields_searched);
  if (n_vtables)
    {
      fprintf (stderr, "vtables = %d; vtable searches = %d\n",
	       n_vtables, n_vtable_searches);
      fprintf (stderr, "vtable entries = %d; vtable elems = %d\n",
	       n_vtable_entries, n_vtable_elems);
    }
#endif
}

/* Build a dummy reference to ourselves so Derived::Base (and A::A) works,
   according to [class]:
                                          The class-name is also inserted
   into  the scope of the class itself.  For purposes of access checking,
   the inserted class name is treated as if it were a public member name.  */

void
build_self_reference ()
{
  tree name = constructor_name (current_class_type);
  tree value = build_lang_decl (TYPE_DECL, name, current_class_type);
  tree saved_cas;

  DECL_NONLOCAL (value) = 1;
  DECL_CONTEXT (value) = current_class_type;
  DECL_ARTIFICIAL (value) = 1;
  SET_DECL_SELF_REFERENCE_P (value);

  if (processing_template_decl)
    value = push_template_decl (value);

  saved_cas = current_access_specifier;
  current_access_specifier = access_public_node;
  finish_member_declaration (value);
  current_access_specifier = saved_cas;
}

/* Returns 1 if TYPE contains only padding bytes.  */

int
is_empty_class (type)
     tree type;
{
  if (type == error_mark_node)
    return 0;

  if (! IS_AGGR_TYPE (type))
    return 0;

  /* In G++ 3.2, whether or not a class was empty was determined by
     looking at its size.  */
  if (abi_version_at_least (2))
    return CLASSTYPE_EMPTY_P (type);
  else
    return integer_zerop (CLASSTYPE_SIZE (type));
}

/* Returns true if TYPE contains an empty class.  */

static bool
contains_empty_class_p (tree type)
{
  if (is_empty_class (type))
    return true;
  if (CLASS_TYPE_P (type))
    {
      tree field;
      int i;

      for (i = 0; i < CLASSTYPE_N_BASECLASSES (type); ++i)
	if (contains_empty_class_p (TYPE_BINFO_BASETYPE (type, i)))
	  return true;
      for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	if (TREE_CODE (field) == FIELD_DECL
	    && !DECL_ARTIFICIAL (field)
	    && is_empty_class (TREE_TYPE (field)))
	  return true;
    }
  else if (TREE_CODE (type) == ARRAY_TYPE)
    return contains_empty_class_p (TREE_TYPE (type));
  return false;
}

/* Find the enclosing class of the given NODE.  NODE can be a *_DECL or
   a *_TYPE node.  NODE can also be a local class.  */

tree
get_enclosing_class (type)
     tree type;
{
  tree node = type;

  while (node && TREE_CODE (node) != NAMESPACE_DECL)
    {
      switch (TREE_CODE_CLASS (TREE_CODE (node)))
	{
	case 'd':
	  node = DECL_CONTEXT (node);
	  break;

	case 't':
	  if (node != type)
	    return node;
	  node = TYPE_CONTEXT (node);
	  break;

	default:
	  abort ();
	}
    }
  return NULL_TREE;
}

/* Return 1 if TYPE or one of its enclosing classes is derived from BASE.  */

int
is_base_of_enclosing_class (base, type)
     tree base, type;
{
  while (type)
    {
      if (lookup_base (type, base, ba_any, NULL))
	return 1;

      type = get_enclosing_class (type);
    }
  return 0;
}

/* Note that NAME was looked up while the current class was being
   defined and that the result of that lookup was DECL.  */

void
maybe_note_name_used_in_class (name, decl)
     tree name;
     tree decl;
{
  splay_tree names_used;

  /* If we're not defining a class, there's nothing to do.  */
  if (!innermost_scope_is_class_p ())
    return;
  
  /* If there's already a binding for this NAME, then we don't have
     anything to worry about.  */
  if (IDENTIFIER_CLASS_VALUE (name))
    return;

  if (!current_class_stack[current_class_depth - 1].names_used)
    current_class_stack[current_class_depth - 1].names_used
      = splay_tree_new (splay_tree_compare_pointers, 0, 0);
  names_used = current_class_stack[current_class_depth - 1].names_used;

  splay_tree_insert (names_used,
		     (splay_tree_key) name, 
		     (splay_tree_value) decl);
}

/* Note that NAME was declared (as DECL) in the current class.  Check
   to see that the declaration is valid.  */

void
note_name_declared_in_class (name, decl)
     tree name;
     tree decl;
{
  splay_tree names_used;
  splay_tree_node n;

  /* Look to see if we ever used this name.  */
  names_used 
    = current_class_stack[current_class_depth - 1].names_used;
  if (!names_used)
    return;

  n = splay_tree_lookup (names_used, (splay_tree_key) name);
  if (n)
    {
      /* [basic.scope.class]
	 
	 A name N used in a class S shall refer to the same declaration
	 in its context and when re-evaluated in the completed scope of
	 S.  */
      error ("declaration of `%#D'", decl);
      cp_error_at ("changes meaning of `%D' from `%+#D'", 
		   DECL_NAME (OVL_CURRENT (decl)),
		   (tree) n->value);
    }
}

/* Returns the VAR_DECL for the complete vtable associated with BINFO.
   Secondary vtables are merged with primary vtables; this function
   will return the VAR_DECL for the primary vtable.  */

tree
get_vtbl_decl_for_binfo (binfo)
     tree binfo;
{
  tree decl;

  decl = BINFO_VTABLE (binfo);
  if (decl && TREE_CODE (decl) == PLUS_EXPR)
    {
      my_friendly_assert (TREE_CODE (TREE_OPERAND (decl, 0)) == ADDR_EXPR,
			  2000403);
      decl = TREE_OPERAND (TREE_OPERAND (decl, 0), 0);
    }
  if (decl)
    my_friendly_assert (TREE_CODE (decl) == VAR_DECL, 20000403);
  return decl;
}

/* Called from get_primary_binfo via dfs_walk.  DATA is a TREE_LIST
   who's TREE_PURPOSE is the TYPE of the required primary base and
   who's TREE_VALUE is a list of candidate binfos that we fill in.  */

static tree
dfs_get_primary_binfo (binfo, data)
     tree binfo;
     void *data;
{
  tree cons = (tree) data;
  tree primary_base = TREE_PURPOSE (cons);

  if (TREE_VIA_VIRTUAL (binfo) 
      && same_type_p (BINFO_TYPE (binfo), primary_base))
    /* This is the right type of binfo, but it might be an unshared
       instance, and the shared instance is later in the dfs walk.  We
       must keep looking.  */
    TREE_VALUE (cons) = tree_cons (NULL, binfo, TREE_VALUE (cons));
  
  return NULL_TREE;
}

/* Returns the unshared binfo for the primary base of BINFO.  Note
   that in a complex hierarchy the resulting BINFO may not actually
   *be* primary.  In particular if the resulting BINFO is a virtual
   base, and it occurs elsewhere in the hierarchy, then this
   occurrence may not actually be a primary base in the complete
   object.  Check BINFO_PRIMARY_P to be sure.  */

tree
get_primary_binfo (binfo)
     tree binfo;
{
  tree primary_base;
  tree result = NULL_TREE;
  tree virtuals;
  
  primary_base = CLASSTYPE_PRIMARY_BINFO (BINFO_TYPE (binfo));
  if (!primary_base)
    return NULL_TREE;

  /* A non-virtual primary base is always a direct base, and easy to
     find.  */
  if (!TREE_VIA_VIRTUAL (primary_base))
    {
      int i;

      /* Scan the direct basetypes until we find a base with the same
	 type as the primary base.  */
      for (i = 0; i < BINFO_N_BASETYPES (binfo); ++i)
	{
	  tree base_binfo = BINFO_BASETYPE (binfo, i);
	  
	  if (same_type_p (BINFO_TYPE (base_binfo),
			   BINFO_TYPE (primary_base)))
	    return base_binfo;
	}

      /* We should always find the primary base.  */
      abort ();
    }

  /* For a primary virtual base, we have to scan the entire hierarchy
     rooted at BINFO; the virtual base could be an indirect virtual
     base.  There could be more than one instance of the primary base
     in the hierarchy, and if one is the canonical binfo we want that
     one.  If it exists, it should be the first one we find, but as a
     consistency check we find them all and make sure.  */
  virtuals = build_tree_list (BINFO_TYPE (primary_base), NULL_TREE);
  dfs_walk (binfo, dfs_get_primary_binfo, NULL, virtuals);
  virtuals = TREE_VALUE (virtuals);
  
  /* We must have found at least one instance.  */
  my_friendly_assert (virtuals, 20010612);

  if (TREE_CHAIN (virtuals))
    {
      /* We found more than one instance of the base. We must make
         sure that, if one is the canonical one, it is the first one
         we found. As the chain is in reverse dfs order, that means
         the last on the list.  */
      tree complete_binfo;
      tree canonical;
      
      for (complete_binfo = binfo;
	   BINFO_INHERITANCE_CHAIN (complete_binfo);
	   complete_binfo = BINFO_INHERITANCE_CHAIN (complete_binfo))
	continue;
      canonical = binfo_for_vbase (BINFO_TYPE (primary_base),
				   BINFO_TYPE (complete_binfo));
      
      for (; virtuals; virtuals = TREE_CHAIN (virtuals))
	{
	  result = TREE_VALUE (virtuals);

	  if (canonical == result)
	    {
	      /* This is the unshared instance. Make sure it was the
		 first one found.  */
	      my_friendly_assert (!TREE_CHAIN (virtuals), 20010612);
	      break;
	    }
	}
    }
  else
    result = TREE_VALUE (virtuals);
  return result;
}

/* If INDENTED_P is zero, indent to INDENT. Return nonzero.  */

static int
maybe_indent_hierarchy (stream, indent, indented_p)
     FILE *stream;
     int indent;
     int indented_p;
{
  if (!indented_p)
    fprintf (stream, "%*s", indent, "");
  return 1;
}

/* Dump the offsets of all the bases rooted at BINFO (in the hierarchy
   dominated by T) to stderr.  INDENT should be zero when called from
   the top level; it is incremented recursively.  */

static void
dump_class_hierarchy_r (stream, flags, t, binfo, indent)
     FILE *stream;
     int flags;
     tree t;
     tree binfo;
     int indent;
{
  int i;
  int indented = 0;
  
  indented = maybe_indent_hierarchy (stream, indent, 0);
  fprintf (stream, "%s (0x%lx) ",
	   type_as_string (binfo, TFF_PLAIN_IDENTIFIER),
	   (unsigned long) binfo);
  fprintf (stream, HOST_WIDE_INT_PRINT_DEC,
	   tree_low_cst (BINFO_OFFSET (binfo), 0));
  if (is_empty_class (BINFO_TYPE (binfo)))
    fprintf (stream, " empty");
  else if (CLASSTYPE_NEARLY_EMPTY_P (BINFO_TYPE (binfo)))
    fprintf (stream, " nearly-empty");
  if (TREE_VIA_VIRTUAL (binfo))
    {
      tree canonical = binfo_for_vbase (BINFO_TYPE (binfo), t);

      fprintf (stream, " virtual");
      if (canonical == binfo)
        fprintf (stream, " canonical");
      else
        fprintf (stream, " non-canonical");
    }
  fprintf (stream, "\n");

  indented = 0;
  if (BINFO_PRIMARY_BASE_OF (binfo))
    {
      indented = maybe_indent_hierarchy (stream, indent + 3, indented);
      fprintf (stream, " primary-for %s (0x%lx)",
	       type_as_string (BINFO_PRIMARY_BASE_OF (binfo),
			       TFF_PLAIN_IDENTIFIER),
	       (unsigned long)BINFO_PRIMARY_BASE_OF (binfo));
    }
  if (BINFO_LOST_PRIMARY_P (binfo))
    {
      indented = maybe_indent_hierarchy (stream, indent + 3, indented);
      fprintf (stream, " lost-primary");
    }
  if (indented)
    fprintf (stream, "\n");

  if (!(flags & TDF_SLIM))
    {
      int indented = 0;
      
      if (BINFO_SUBVTT_INDEX (binfo))
	{
	  indented = maybe_indent_hierarchy (stream, indent + 3, indented);
	  fprintf (stream, " subvttidx=%s",
		   expr_as_string (BINFO_SUBVTT_INDEX (binfo),
				   TFF_PLAIN_IDENTIFIER));
	}
      if (BINFO_VPTR_INDEX (binfo))
	{
	  indented = maybe_indent_hierarchy (stream, indent + 3, indented);
	  fprintf (stream, " vptridx=%s",
		   expr_as_string (BINFO_VPTR_INDEX (binfo),
				   TFF_PLAIN_IDENTIFIER));
	}
      if (BINFO_VPTR_FIELD (binfo))
	{
	  indented = maybe_indent_hierarchy (stream, indent + 3, indented);
	  fprintf (stream, " vbaseoffset=%s",
		   expr_as_string (BINFO_VPTR_FIELD (binfo),
				   TFF_PLAIN_IDENTIFIER));
	}
      if (BINFO_VTABLE (binfo))
	{
	  indented = maybe_indent_hierarchy (stream, indent + 3, indented);
	  fprintf (stream, " vptr=%s",
		   expr_as_string (BINFO_VTABLE (binfo),
				   TFF_PLAIN_IDENTIFIER));
	}
      
      if (indented)
	fprintf (stream, "\n");
    }
  

  for (i = 0; i < BINFO_N_BASETYPES (binfo); ++i)
    dump_class_hierarchy_r (stream, flags,
			    t, BINFO_BASETYPE (binfo, i),
			    indent + 2);
}

/* Dump the BINFO hierarchy for T.  */

static void
dump_class_hierarchy (t)
     tree t;
{
  int flags;
  FILE *stream = dump_begin (TDI_class, &flags);

  if (!stream)
    return;
  
  fprintf (stream, "Class %s\n", type_as_string (t, TFF_PLAIN_IDENTIFIER));
  fprintf (stream, "   size=%lu align=%lu\n",
	   (unsigned long)(tree_low_cst (TYPE_SIZE (t), 0) / BITS_PER_UNIT),
	   (unsigned long)(TYPE_ALIGN (t) / BITS_PER_UNIT));
  dump_class_hierarchy_r (stream, flags, t, TYPE_BINFO (t), 0);
  fprintf (stream, "\n");
  dump_end (TDI_class, stream);
}

static void
dump_array (stream, decl)
     FILE *stream;
     tree decl;
{
  tree inits;
  int ix;
  HOST_WIDE_INT elt;
  tree size = TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (decl)));

  elt = (tree_low_cst (TYPE_SIZE (TREE_TYPE (TREE_TYPE (decl))), 0)
	 / BITS_PER_UNIT);
  fprintf (stream, "%s:", decl_as_string (decl, TFF_PLAIN_IDENTIFIER));
  fprintf (stream, " %s entries",
	   expr_as_string (size_binop (PLUS_EXPR, size, size_one_node),
			   TFF_PLAIN_IDENTIFIER));
  fprintf (stream, "\n");

  for (ix = 0, inits = TREE_OPERAND (DECL_INITIAL (decl), 1);
       inits; ix++, inits = TREE_CHAIN (inits))
    fprintf (stream, "%-4ld  %s\n", (long)(ix * elt),
	     expr_as_string (TREE_VALUE (inits), TFF_PLAIN_IDENTIFIER));
}

static void
dump_vtable (t, binfo, vtable)
     tree t;
     tree binfo;
     tree vtable;
{
  int flags;
  FILE *stream = dump_begin (TDI_class, &flags);

  if (!stream)
    return;

  if (!(flags & TDF_SLIM))
    {
      int ctor_vtbl_p = TYPE_BINFO (t) != binfo;
      
      fprintf (stream, "%s for %s",
	       ctor_vtbl_p ? "Construction vtable" : "Vtable",
	       type_as_string (binfo, TFF_PLAIN_IDENTIFIER));
      if (ctor_vtbl_p)
	{
	  if (!TREE_VIA_VIRTUAL (binfo))
	    fprintf (stream, " (0x%lx instance)", (unsigned long)binfo);
	  fprintf (stream, " in %s", type_as_string (t, TFF_PLAIN_IDENTIFIER));
	}
      fprintf (stream, "\n");
      dump_array (stream, vtable);
      fprintf (stream, "\n");
    }
  
  dump_end (TDI_class, stream);
}

static void
dump_vtt (t, vtt)
     tree t;
     tree vtt;
{
  int flags;
  FILE *stream = dump_begin (TDI_class, &flags);

  if (!stream)
    return;

  if (!(flags & TDF_SLIM))
    {
      fprintf (stream, "VTT for %s\n",
	       type_as_string (t, TFF_PLAIN_IDENTIFIER));
      dump_array (stream, vtt);
      fprintf (stream, "\n");
    }
  
  dump_end (TDI_class, stream);
}

/* Virtual function table initialization.  */

/* Create all the necessary vtables for T and its base classes.  */

static void
finish_vtbls (t)
     tree t;
{
  tree list;
  tree vbase;
  int i;

  /* We lay out the primary and secondary vtables in one contiguous
     vtable.  The primary vtable is first, followed by the non-virtual
     secondary vtables in inheritance graph order.  */
  list = build_tree_list (TYPE_BINFO_VTABLE (t), NULL_TREE);
  accumulate_vtbl_inits (TYPE_BINFO (t), TYPE_BINFO (t),
			 TYPE_BINFO (t), t, list);
  
  /* Then come the virtual bases, also in inheritance graph order.  */
  for (vbase = TYPE_BINFO (t); vbase; vbase = TREE_CHAIN (vbase))
    {
      tree real_base;
	  
      if (!TREE_VIA_VIRTUAL (vbase))
	continue;
          
      /* Although we walk in inheritance order, that might not get the
         canonical base.  */
      real_base = binfo_for_vbase (BINFO_TYPE (vbase), t);
          
      accumulate_vtbl_inits (real_base, real_base,
			     TYPE_BINFO (t), t, list);
    }

  /* Fill in BINFO_VPTR_FIELD in the immediate binfos for our virtual
     base classes, for the benefit of the debugging backends.  */
  for (i = 0; i < BINFO_N_BASETYPES (TYPE_BINFO (t)); ++i)
    {
      tree base = BINFO_BASETYPE (TYPE_BINFO (t), i);
      if (TREE_VIA_VIRTUAL (base))
	{
	  vbase = binfo_for_vbase (BINFO_TYPE (base), t);
	  BINFO_VPTR_FIELD (base) = BINFO_VPTR_FIELD (vbase);
	}
    }

  if (TYPE_BINFO_VTABLE (t))
    initialize_vtable (TYPE_BINFO (t), TREE_VALUE (list));
}

/* Initialize the vtable for BINFO with the INITS.  */

static void
initialize_vtable (binfo, inits)
     tree binfo;
     tree inits;
{
  tree decl;

  layout_vtable_decl (binfo, list_length (inits));
  decl = get_vtbl_decl_for_binfo (binfo);
  initialize_array (decl, inits);
  dump_vtable (BINFO_TYPE (binfo), binfo, decl);
}

/* Initialize DECL (a declaration for a namespace-scope array) with
   the INITS.  */

static void
initialize_array (decl, inits)
  tree decl;
  tree inits;
{
  tree context;

  context = DECL_CONTEXT (decl);
  DECL_CONTEXT (decl) = NULL_TREE;
  DECL_INITIAL (decl) = build_nt (CONSTRUCTOR, NULL_TREE, inits);
  TREE_HAS_CONSTRUCTOR (DECL_INITIAL (decl)) = 1;
  cp_finish_decl (decl, DECL_INITIAL (decl), NULL_TREE, 0);
  DECL_CONTEXT (decl) = context;
}

/* Build the VTT (virtual table table) for T.
   A class requires a VTT if it has virtual bases.
   
   This holds
   1 - primary virtual pointer for complete object T
   2 - secondary VTTs for each direct non-virtual base of T which requires a
       VTT
   3 - secondary virtual pointers for each direct or indirect base of T which
       has virtual bases or is reachable via a virtual path from T.
   4 - secondary VTTs for each direct or indirect virtual base of T.
   
   Secondary VTTs look like complete object VTTs without part 4.  */

static void
build_vtt (t)
     tree t;
{
  tree inits;
  tree type;
  tree vtt;
  tree index;

  /* Build up the initializers for the VTT.  */
  inits = NULL_TREE;
  index = size_zero_node;
  build_vtt_inits (TYPE_BINFO (t), t, &inits, &index);

  /* If we didn't need a VTT, we're done.  */
  if (!inits)
    return;

  /* Figure out the type of the VTT.  */
  type = build_index_type (size_int (list_length (inits) - 1));
  type = build_cplus_array_type (const_ptr_type_node, type);
				 
  /* Now, build the VTT object itself.  */
  vtt = build_vtable (t, get_vtt_name (t), type);
  initialize_array (vtt, inits);
  /* Add the VTT to the vtables list.  */
  TREE_CHAIN (vtt) = TREE_CHAIN (CLASSTYPE_VTABLES (t));
  TREE_CHAIN (CLASSTYPE_VTABLES (t)) = vtt;

  dump_vtt (t, vtt);
}

/* The type corresponding to BASE_BINFO is a base of the type of BINFO, but
   from within some hierarchy which is inherited from the type of BINFO.
   Return BASE_BINFO's equivalent binfo from the hierarchy dominated by
   BINFO.  */

static tree
get_original_base (base_binfo, binfo)
     tree base_binfo;
     tree binfo;
{
  tree derived;
  int ix;
  
  if (same_type_p (BINFO_TYPE (base_binfo), BINFO_TYPE (binfo)))
    return binfo;
  if (TREE_VIA_VIRTUAL (base_binfo))
    return binfo_for_vbase (BINFO_TYPE (base_binfo), BINFO_TYPE (binfo));
  derived = get_original_base (BINFO_INHERITANCE_CHAIN (base_binfo), binfo);
  
  for (ix = 0; ix != BINFO_N_BASETYPES (derived); ix++)
    if (same_type_p (BINFO_TYPE (base_binfo),
                     BINFO_TYPE (BINFO_BASETYPE (derived, ix))))
      return BINFO_BASETYPE (derived, ix);
  abort ();
  return NULL;
}

/* When building a secondary VTT, BINFO_VTABLE is set to a TREE_LIST with
   PURPOSE the RTTI_BINFO, VALUE the real vtable pointer for this binfo,
   and CHAIN the vtable pointer for this binfo after construction is
   complete.  VALUE can also be another BINFO, in which case we recurse.  */

static tree
binfo_ctor_vtable (binfo)
     tree binfo;
{
  tree vt;

  while (1)
    {
      vt = BINFO_VTABLE (binfo);
      if (TREE_CODE (vt) == TREE_LIST)
	vt = TREE_VALUE (vt);
      if (TREE_CODE (vt) == TREE_VEC)
	binfo = vt;
      else
	break;
    }

  return vt;
}

/* Recursively build the VTT-initializer for BINFO (which is in the
   hierarchy dominated by T).  INITS points to the end of the initializer
   list to date.  INDEX is the VTT index where the next element will be
   replaced.  Iff BINFO is the binfo for T, this is the top level VTT (i.e.
   not a subvtt for some base of T).  When that is so, we emit the sub-VTTs
   for virtual bases of T. When it is not so, we build the constructor
   vtables for the BINFO-in-T variant.  */

static tree *
build_vtt_inits (binfo, t, inits, index)
     tree binfo;
     tree t;
     tree *inits;
     tree *index;
{
  int i;
  tree b;
  tree init;
  tree secondary_vptrs;
  int top_level_p = same_type_p (TREE_TYPE (binfo), t);

  /* We only need VTTs for subobjects with virtual bases.  */
  if (!TYPE_USES_VIRTUAL_BASECLASSES (BINFO_TYPE (binfo)))
    return inits;

  /* We need to use a construction vtable if this is not the primary
     VTT.  */
  if (!top_level_p)
    {
      build_ctor_vtbl_group (binfo, t);

      /* Record the offset in the VTT where this sub-VTT can be found.  */
      BINFO_SUBVTT_INDEX (binfo) = *index;
    }

  /* Add the address of the primary vtable for the complete object.  */
  init = binfo_ctor_vtable (binfo);
  *inits = build_tree_list (NULL_TREE, init);
  inits = &TREE_CHAIN (*inits);
  if (top_level_p)
    {
      my_friendly_assert (!BINFO_VPTR_INDEX (binfo), 20010129);
      BINFO_VPTR_INDEX (binfo) = *index;
    }
  *index = size_binop (PLUS_EXPR, *index, TYPE_SIZE_UNIT (ptr_type_node));
		       
  /* Recursively add the secondary VTTs for non-virtual bases.  */
  for (i = 0; i < BINFO_N_BASETYPES (binfo); ++i)
    {
      b = BINFO_BASETYPE (binfo, i);
      if (!TREE_VIA_VIRTUAL (b))
	inits = build_vtt_inits (BINFO_BASETYPE (binfo, i), t, 
				 inits, index);
    }
      
  /* Add secondary virtual pointers for all subobjects of BINFO with
     either virtual bases or reachable along a virtual path, except
     subobjects that are non-virtual primary bases.  */
  secondary_vptrs = tree_cons (t, NULL_TREE, BINFO_TYPE (binfo));
  TREE_TYPE (secondary_vptrs) = *index;
  VTT_TOP_LEVEL_P (secondary_vptrs) = top_level_p;
  VTT_MARKED_BINFO_P (secondary_vptrs) = 0;
  
  dfs_walk_real (binfo,
		 dfs_build_secondary_vptr_vtt_inits,
		 NULL,
	         dfs_ctor_vtable_bases_queue_p,
		 secondary_vptrs);
  VTT_MARKED_BINFO_P (secondary_vptrs) = 1;
  dfs_walk (binfo, dfs_unmark, dfs_ctor_vtable_bases_queue_p,
            secondary_vptrs);

  *index = TREE_TYPE (secondary_vptrs);

  /* The secondary vptrs come back in reverse order.  After we reverse
     them, and add the INITS, the last init will be the first element
     of the chain.  */
  secondary_vptrs = TREE_VALUE (secondary_vptrs);
  if (secondary_vptrs)
    {
      *inits = nreverse (secondary_vptrs);
      inits = &TREE_CHAIN (secondary_vptrs);
      my_friendly_assert (*inits == NULL_TREE, 20000517);
    }

  /* Add the secondary VTTs for virtual bases.  */
  if (top_level_p)
    for (b = TYPE_BINFO (BINFO_TYPE (binfo)); b; b = TREE_CHAIN (b))
      {
	tree vbase;
	
	if (!TREE_VIA_VIRTUAL (b))
	  continue;
	
	vbase = binfo_for_vbase (BINFO_TYPE (b), t);
	inits = build_vtt_inits (vbase, t, inits, index);
      }

  if (!top_level_p)
    {
      tree data = tree_cons (t, binfo, NULL_TREE);
      VTT_TOP_LEVEL_P (data) = 0;
      VTT_MARKED_BINFO_P (data) = 0;
      
      dfs_walk (binfo, dfs_fixup_binfo_vtbls,
	        dfs_ctor_vtable_bases_queue_p,
	        data);
    }

  return inits;
}

/* Called from build_vtt_inits via dfs_walk.  BINFO is the binfo
   for the base in most derived. DATA is a TREE_LIST who's
   TREE_CHAIN is the type of the base being
   constructed whilst this secondary vptr is live.  The TREE_UNSIGNED
   flag of DATA indicates that this is a constructor vtable.  The
   TREE_TOP_LEVEL flag indicates that this is the primary VTT.  */

static tree
dfs_build_secondary_vptr_vtt_inits (binfo, data)
     tree binfo;
     void *data;
{
  tree l; 
  tree t;
  tree init;
  tree index;
  int top_level_p;

  l = (tree) data;
  t = TREE_CHAIN (l);
  top_level_p = VTT_TOP_LEVEL_P (l);
  
  SET_BINFO_MARKED (binfo);

  /* We don't care about bases that don't have vtables.  */
  if (!TYPE_VFIELD (BINFO_TYPE (binfo)))
    return NULL_TREE;

  /* We're only interested in proper subobjects of T.  */
  if (same_type_p (BINFO_TYPE (binfo), t))
    return NULL_TREE;

  /* We're not interested in non-virtual primary bases.  */
  if (!TREE_VIA_VIRTUAL (binfo) && BINFO_PRIMARY_P (binfo))
    return NULL_TREE;

  /* If BINFO has virtual bases or is reachable via a virtual path
     from T, it'll have a secondary vptr.  */
  if (!TYPE_USES_VIRTUAL_BASECLASSES (BINFO_TYPE (binfo))
      && !binfo_via_virtual (binfo, t))
    return NULL_TREE;

  /* Record the index where this secondary vptr can be found.  */
  index = TREE_TYPE (l);
  if (top_level_p)
    {
      my_friendly_assert (!BINFO_VPTR_INDEX (binfo), 20010129);
      BINFO_VPTR_INDEX (binfo) = index;
    }
  TREE_TYPE (l) = size_binop (PLUS_EXPR, index, 
			      TYPE_SIZE_UNIT (ptr_type_node));

  /* Add the initializer for the secondary vptr itself.  */
  if (top_level_p && TREE_VIA_VIRTUAL (binfo))
    {
      /* It's a primary virtual base, and this is not the construction
         vtable. Find the base this is primary of in the inheritance graph,
         and use that base's vtable now.  */
      while (BINFO_PRIMARY_BASE_OF (binfo))
        binfo = BINFO_PRIMARY_BASE_OF (binfo);
    }
  init = binfo_ctor_vtable (binfo);
  TREE_VALUE (l) = tree_cons (NULL_TREE, init, TREE_VALUE (l));

  return NULL_TREE;
}

/* dfs_walk_real predicate for building vtables. DATA is a TREE_LIST,
   VTT_MARKED_BINFO_P indicates whether marked or unmarked bases
   should be walked.  TREE_PURPOSE is the TREE_TYPE that dominates the
   hierarchy.  */

static tree
dfs_ctor_vtable_bases_queue_p (binfo, data)
     tree binfo;
     void *data;
{
  if (TREE_VIA_VIRTUAL (binfo))
     /* Get the shared version.  */
    binfo = binfo_for_vbase (BINFO_TYPE (binfo), TREE_PURPOSE ((tree) data));

  if (!BINFO_MARKED (binfo) == VTT_MARKED_BINFO_P ((tree) data))
    return NULL_TREE;
  return binfo;
}

/* Called from build_vtt_inits via dfs_walk. After building constructor
   vtables and generating the sub-vtt from them, we need to restore the
   BINFO_VTABLES that were scribbled on.  DATA is a TREE_LIST whose
   TREE_VALUE is the TREE_TYPE of the base whose sub vtt was generated.  */

static tree
dfs_fixup_binfo_vtbls (binfo, data)
     tree binfo;
     void *data;
{
  CLEAR_BINFO_MARKED (binfo);

  /* We don't care about bases that don't have vtables.  */
  if (!TYPE_VFIELD (BINFO_TYPE (binfo)))
    return NULL_TREE;

  /* If we scribbled the construction vtable vptr into BINFO, clear it
     out now.  */
  if (BINFO_VTABLE (binfo)
      && TREE_CODE (BINFO_VTABLE (binfo)) == TREE_LIST
      && (TREE_PURPOSE (BINFO_VTABLE (binfo)) 
	  == TREE_VALUE ((tree) data)))
    BINFO_VTABLE (binfo) = TREE_CHAIN (BINFO_VTABLE (binfo));

  return NULL_TREE;
}

/* Build the construction vtable group for BINFO which is in the
   hierarchy dominated by T.  */

static void
build_ctor_vtbl_group (binfo, t)
     tree binfo;
     tree t;
{
  tree list;
  tree type;
  tree vtbl;
  tree inits;
  tree id;
  tree vbase;

  /* See if we've already created this construction vtable group.  */
  id = mangle_ctor_vtbl_for_type (t, binfo);
  if (IDENTIFIER_GLOBAL_VALUE (id))
    return;

  my_friendly_assert (!same_type_p (BINFO_TYPE (binfo), t), 20010124);
  /* Build a version of VTBL (with the wrong type) for use in
     constructing the addresses of secondary vtables in the
     construction vtable group.  */
  vtbl = build_vtable (t, id, ptr_type_node);
  list = build_tree_list (vtbl, NULL_TREE);
  accumulate_vtbl_inits (binfo, TYPE_BINFO (TREE_TYPE (binfo)),
			 binfo, t, list);

  /* Add the vtables for each of our virtual bases using the vbase in T
     binfo.  */
  for (vbase = TYPE_BINFO (BINFO_TYPE (binfo)); 
       vbase; 
       vbase = TREE_CHAIN (vbase))
    {
      tree b;
      tree orig_base;

      if (!TREE_VIA_VIRTUAL (vbase))
	continue;
      b = binfo_for_vbase (BINFO_TYPE (vbase), t);
      orig_base = binfo_for_vbase (BINFO_TYPE (vbase), BINFO_TYPE (binfo));
      
      accumulate_vtbl_inits (b, orig_base, binfo, t, list);
    }
  inits = TREE_VALUE (list);

  /* Figure out the type of the construction vtable.  */
  type = build_index_type (size_int (list_length (inits) - 1));
  type = build_cplus_array_type (vtable_entry_type, type);
  TREE_TYPE (vtbl) = type;

  /* Initialize the construction vtable.  */
  CLASSTYPE_VTABLES (t) = chainon (CLASSTYPE_VTABLES (t), vtbl);
  initialize_array (vtbl, inits);
  dump_vtable (t, binfo, vtbl);
}

/* Add the vtbl initializers for BINFO (and its bases other than
   non-virtual primaries) to the list of INITS.  BINFO is in the
   hierarchy dominated by T.  RTTI_BINFO is the binfo within T of
   the constructor the vtbl inits should be accumulated for. (If this
   is the complete object vtbl then RTTI_BINFO will be TYPE_BINFO (T).)
   ORIG_BINFO is the binfo for this object within BINFO_TYPE (RTTI_BINFO).
   BINFO is the active base equivalent of ORIG_BINFO in the inheritance
   graph of T. Both BINFO and ORIG_BINFO will have the same BINFO_TYPE,
   but are not necessarily the same in terms of layout.  */

static void
accumulate_vtbl_inits (binfo, orig_binfo, rtti_binfo, t, inits)
     tree binfo;
     tree orig_binfo;
     tree rtti_binfo;
     tree t;
     tree inits;
{
  int i;
  int ctor_vtbl_p = !same_type_p (BINFO_TYPE (rtti_binfo), t);

  my_friendly_assert (same_type_p (BINFO_TYPE (binfo),
				   BINFO_TYPE (orig_binfo)),
		      20000517);

  /* If it doesn't have a vptr, we don't do anything.  */
  if (!TYPE_CONTAINS_VPTR_P (BINFO_TYPE (binfo)))
    return;
  
  /* If we're building a construction vtable, we're not interested in
     subobjects that don't require construction vtables.  */
  if (ctor_vtbl_p 
      && !TYPE_USES_VIRTUAL_BASECLASSES (BINFO_TYPE (binfo))
      && !binfo_via_virtual (orig_binfo, BINFO_TYPE (rtti_binfo)))
    return;

  /* Build the initializers for the BINFO-in-T vtable.  */
  TREE_VALUE (inits) 
    = chainon (TREE_VALUE (inits),
	       dfs_accumulate_vtbl_inits (binfo, orig_binfo,
					  rtti_binfo, t, inits));
		      
  /* Walk the BINFO and its bases.  We walk in preorder so that as we
     initialize each vtable we can figure out at what offset the
     secondary vtable lies from the primary vtable.  We can't use
     dfs_walk here because we need to iterate through bases of BINFO
     and RTTI_BINFO simultaneously.  */
  for (i = 0; i < BINFO_N_BASETYPES (binfo); ++i)
    {
      tree base_binfo = BINFO_BASETYPE (binfo, i);
      
      /* Skip virtual bases.  */
      if (TREE_VIA_VIRTUAL (base_binfo))
	continue;
      accumulate_vtbl_inits (base_binfo,
			     BINFO_BASETYPE (orig_binfo, i),
			     rtti_binfo, t,
			     inits);
    }
}

/* Called from accumulate_vtbl_inits.  Returns the initializers for
   the BINFO vtable.  */

static tree
dfs_accumulate_vtbl_inits (binfo, orig_binfo, rtti_binfo, t, l)
     tree binfo;
     tree orig_binfo;
     tree rtti_binfo;
     tree t;
     tree l;
{
  tree inits = NULL_TREE;
  tree vtbl = NULL_TREE;
  int ctor_vtbl_p = !same_type_p (BINFO_TYPE (rtti_binfo), t);

  if (ctor_vtbl_p
      && TREE_VIA_VIRTUAL (orig_binfo) && BINFO_PRIMARY_P (orig_binfo))
    {
      /* In the hierarchy of BINFO_TYPE (RTTI_BINFO), this is a
	 primary virtual base.  If it is not the same primary in
	 the hierarchy of T, we'll need to generate a ctor vtable
	 for it, to place at its location in T.  If it is the same
	 primary, we still need a VTT entry for the vtable, but it
	 should point to the ctor vtable for the base it is a
	 primary for within the sub-hierarchy of RTTI_BINFO.
	      
	 There are three possible cases:
	      
	 1) We are in the same place.
	 2) We are a primary base within a lost primary virtual base of
	 RTTI_BINFO.
	 3) We are primary to something not a base of RTTI_BINFO.  */
	  
      tree b = BINFO_PRIMARY_BASE_OF (binfo);
      tree last = NULL_TREE;

      /* First, look through the bases we are primary to for RTTI_BINFO
	 or a virtual base.  */
      for (; b; b = BINFO_PRIMARY_BASE_OF (b))
	{
	  last = b;
	  if (TREE_VIA_VIRTUAL (b) || b == rtti_binfo)
	    break;
	}
      /* If we run out of primary links, keep looking down our
	 inheritance chain; we might be an indirect primary.  */
      if (b == NULL_TREE)
	for (b = last; b; b = BINFO_INHERITANCE_CHAIN (b))
	  if (TREE_VIA_VIRTUAL (b) || b == rtti_binfo)
	    break;

      /* If we found RTTI_BINFO, this is case 1.  If we found a virtual
	 base B and it is a base of RTTI_BINFO, this is case 2.  In
	 either case, we share our vtable with LAST, i.e. the
	 derived-most base within B of which we are a primary.  */
      if (b == rtti_binfo
	  || (b && binfo_for_vbase (BINFO_TYPE (b),
				    BINFO_TYPE (rtti_binfo))))
	/* Just set our BINFO_VTABLE to point to LAST, as we may not have
	   set LAST's BINFO_VTABLE yet.  We'll extract the actual vptr in
	   binfo_ctor_vtable after everything's been set up.  */
	vtbl = last;

      /* Otherwise, this is case 3 and we get our own.  */
    }
  else if (!BINFO_NEW_VTABLE_MARKED (orig_binfo, BINFO_TYPE (rtti_binfo)))
    return inits;

  if (!vtbl)
    {
      tree index;
      int non_fn_entries;

      /* Compute the initializer for this vtable.  */
      inits = build_vtbl_initializer (binfo, orig_binfo, t, rtti_binfo,
				      &non_fn_entries);

      /* Figure out the position to which the VPTR should point.  */
      vtbl = TREE_PURPOSE (l);
      vtbl = build1 (ADDR_EXPR, 
		     vtbl_ptr_type_node,
		     vtbl);
      TREE_CONSTANT (vtbl) = 1;
      index = size_binop (PLUS_EXPR,
			  size_int (non_fn_entries),
			  size_int (list_length (TREE_VALUE (l))));
      index = size_binop (MULT_EXPR,
			  TYPE_SIZE_UNIT (vtable_entry_type),
			  index);
      vtbl = build (PLUS_EXPR, TREE_TYPE (vtbl), vtbl, index);
      TREE_CONSTANT (vtbl) = 1;
    }

  if (ctor_vtbl_p)
    /* For a construction vtable, we can't overwrite BINFO_VTABLE.
       So, we make a TREE_LIST.  Later, dfs_fixup_binfo_vtbls will
       straighten this out.  */
    BINFO_VTABLE (binfo) = tree_cons (rtti_binfo, vtbl, BINFO_VTABLE (binfo));
  else if (BINFO_PRIMARY_P (binfo) && TREE_VIA_VIRTUAL (binfo))
    inits = NULL_TREE;
  else
     /* For an ordinary vtable, set BINFO_VTABLE.  */
    BINFO_VTABLE (binfo) = vtbl;

  return inits;
}

/* Construct the initializer for BINFO's virtual function table.  BINFO
   is part of the hierarchy dominated by T.  If we're building a
   construction vtable, the ORIG_BINFO is the binfo we should use to
   find the actual function pointers to put in the vtable - but they
   can be overridden on the path to most-derived in the graph that
   ORIG_BINFO belongs.  Otherwise,
   ORIG_BINFO should be the same as BINFO.  The RTTI_BINFO is the
   BINFO that should be indicated by the RTTI information in the
   vtable; it will be a base class of T, rather than T itself, if we
   are building a construction vtable.

   The value returned is a TREE_LIST suitable for wrapping in a
   CONSTRUCTOR to use as the DECL_INITIAL for a vtable.  If
   NON_FN_ENTRIES_P is not NULL, *NON_FN_ENTRIES_P is set to the
   number of non-function entries in the vtable.  

   It might seem that this function should never be called with a
   BINFO for which BINFO_PRIMARY_P holds, the vtable for such a
   base is always subsumed by a derived class vtable.  However, when
   we are building construction vtables, we do build vtables for
   primary bases; we need these while the primary base is being
   constructed.  */

static tree
build_vtbl_initializer (binfo, orig_binfo, t, rtti_binfo, non_fn_entries_p)
     tree binfo;
     tree orig_binfo;
     tree t;
     tree rtti_binfo;
     int *non_fn_entries_p;
{
  tree v, b;
  tree vfun_inits;
  tree vbase;
  vtbl_init_data vid;

  /* Initialize VID.  */
  memset (&vid, 0, sizeof (vid));
  vid.binfo = binfo;
  vid.derived = t;
  vid.rtti_binfo = rtti_binfo;
  vid.last_init = &vid.inits;
  vid.primary_vtbl_p = (binfo == TYPE_BINFO (t));
  vid.ctor_vtbl_p = !same_type_p (BINFO_TYPE (rtti_binfo), t);
  vid.generate_vcall_entries = true;
  /* The first vbase or vcall offset is at index -3 in the vtable.  */
  vid.index = ssize_int (-3 * TARGET_VTABLE_DATA_ENTRY_DISTANCE);

  /* Add entries to the vtable for RTTI.  */
  build_rtti_vtbl_entries (binfo, &vid);

  /* Create an array for keeping track of the functions we've
     processed.  When we see multiple functions with the same
     signature, we share the vcall offsets.  */
  VARRAY_TREE_INIT (vid.fns, 32, "fns");
  /* Add the vcall and vbase offset entries.  */
  build_vcall_and_vbase_vtbl_entries (binfo, &vid);
  /* Clear BINFO_VTABLE_PATH_MARKED; it's set by
     build_vbase_offset_vtbl_entries.  */
  for (vbase = CLASSTYPE_VBASECLASSES (t); 
       vbase; 
       vbase = TREE_CHAIN (vbase))
    CLEAR_BINFO_VTABLE_PATH_MARKED (TREE_VALUE (vbase));

  /* If the target requires padding between data entries, add that now.  */
  if (TARGET_VTABLE_DATA_ENTRY_DISTANCE > 1)
    {
      tree cur, *prev;

      for (prev = &vid.inits; (cur = *prev); prev = &TREE_CHAIN (cur))
	{
	  tree add = cur;
	  int i;

	  for (i = 1; i < TARGET_VTABLE_DATA_ENTRY_DISTANCE; ++i)
	    add = tree_cons (NULL_TREE,
			     build1 (NOP_EXPR, vtable_entry_type,
				     null_pointer_node),
			     add);
	  *prev = add;
	}
    }

  if (non_fn_entries_p)
    *non_fn_entries_p = list_length (vid.inits);

  /* Go through all the ordinary virtual functions, building up
     initializers.  */
  vfun_inits = NULL_TREE;
  for (v = BINFO_VIRTUALS (orig_binfo); v; v = TREE_CHAIN (v))
    {
      tree delta;
      tree vcall_index;
      tree fn;
      tree init = NULL_TREE;
      
      fn = BV_FN (v);

      /* If the only definition of this function signature along our
	 primary base chain is from a lost primary, this vtable slot will
	 never be used, so just zero it out.  This is important to avoid
	 requiring extra thunks which cannot be generated with the function.

	 We first check this in update_vtable_entry_for_fn, so we handle
	 restored primary bases properly; we also need to do it here so we
	 zero out unused slots in ctor vtables, rather than filling themff
	 with erroneous values (though harmless, apart from relocation
	 costs).  */
      for (b = binfo; ; b = get_primary_binfo (b))
	{
	  /* We found a defn before a lost primary; go ahead as normal.  */
	  if (look_for_overrides_here (BINFO_TYPE (b), fn))
	    break;

	  /* The nearest definition is from a lost primary; clear the
	     slot.  */
	  if (BINFO_LOST_PRIMARY_P (b))
	    {
	      init = size_zero_node;
	      break;
	    }
	}

      if (! init)
	{
	  /* Pull the offset for `this', and the function to call, out of
	     the list.  */
	  delta = BV_DELTA (v);
	  vcall_index = BV_VCALL_INDEX (v);

	  my_friendly_assert (TREE_CODE (delta) == INTEGER_CST, 19990727);
	  my_friendly_assert (TREE_CODE (fn) == FUNCTION_DECL, 19990727);

	  /* You can't call an abstract virtual function; it's abstract.
	     So, we replace these functions with __pure_virtual.  */
	  if (DECL_PURE_VIRTUAL_P (fn))
	    fn = abort_fndecl;
	  else if (!integer_zerop (delta) || vcall_index)
	    fn = make_thunk (fn, delta, vcall_index);
	  /* Take the address of the function, considering it to be of an
	     appropriate generic type.  */
	  init = build1 (ADDR_EXPR, vfunc_ptr_type_node, fn);
	  /* The address of a function can't change.  */
	  TREE_CONSTANT (init) = 1;
	}

      /* And add it to the chain of initializers.  */
      if (TARGET_VTABLE_USES_DESCRIPTORS)
	{
	  int i;
	  if (init == size_zero_node)
	    for (i = 0; i < TARGET_VTABLE_USES_DESCRIPTORS; ++i)
	      vfun_inits = tree_cons (NULL_TREE, init, vfun_inits);
	  else
	    for (i = 0; i < TARGET_VTABLE_USES_DESCRIPTORS; ++i)
	      {
		tree fdesc = build (FDESC_EXPR, vfunc_ptr_type_node,
				    TREE_OPERAND (init, 0),
				    build_int_2 (i, 0));
		TREE_CONSTANT (fdesc) = 1;

		vfun_inits = tree_cons (NULL_TREE, fdesc, vfun_inits);
	      }
	}
      else
        vfun_inits = tree_cons (NULL_TREE, init, vfun_inits);
    }

  /* The initializers for virtual functions were built up in reverse
     order; straighten them out now.  */
  vfun_inits = nreverse (vfun_inits);
  
  /* The negative offset initializers are also in reverse order.  */
  vid.inits = nreverse (vid.inits);

  /* Chain the two together.  */
  return chainon (vid.inits, vfun_inits);
}

/* Adds to vid->inits the initializers for the vbase and vcall
   offsets in BINFO, which is in the hierarchy dominated by T.  */

static void
build_vcall_and_vbase_vtbl_entries (binfo, vid)
     tree binfo;
     vtbl_init_data *vid;
{
  tree b;

  /* If this is a derived class, we must first create entries
     corresponding to the primary base class.  */
  b = get_primary_binfo (binfo);
  if (b)
    build_vcall_and_vbase_vtbl_entries (b, vid);

  /* Add the vbase entries for this base.  */
  build_vbase_offset_vtbl_entries (binfo, vid);
  /* Add the vcall entries for this base.  */
  build_vcall_offset_vtbl_entries (binfo, vid);
}

/* Returns the initializers for the vbase offset entries in the vtable
   for BINFO (which is part of the class hierarchy dominated by T), in
   reverse order.  VBASE_OFFSET_INDEX gives the vtable index
   where the next vbase offset will go.  */

static void
build_vbase_offset_vtbl_entries (binfo, vid)
     tree binfo;
     vtbl_init_data *vid;
{
  tree vbase;
  tree t;
  tree non_primary_binfo;

  /* If there are no virtual baseclasses, then there is nothing to
     do.  */
  if (!TYPE_USES_VIRTUAL_BASECLASSES (BINFO_TYPE (binfo)))
    return;

  t = vid->derived;
  
  /* We might be a primary base class.  Go up the inheritance hierarchy
     until we find the most derived class of which we are a primary base:
     it is the offset of that which we need to use.  */
  non_primary_binfo = binfo;
  while (BINFO_INHERITANCE_CHAIN (non_primary_binfo))
    {
      tree b;

      /* If we have reached a virtual base, then it must be a primary
	 base (possibly multi-level) of vid->binfo, or we wouldn't
	 have called build_vcall_and_vbase_vtbl_entries for it.  But it
	 might be a lost primary, so just skip down to vid->binfo.  */
      if (TREE_VIA_VIRTUAL (non_primary_binfo))
	{
	  non_primary_binfo = vid->binfo;
	  break;
	}

      b = BINFO_INHERITANCE_CHAIN (non_primary_binfo);
      if (get_primary_binfo (b) != non_primary_binfo)
	break;
      non_primary_binfo = b;
    }

  /* Go through the virtual bases, adding the offsets.  */
  for (vbase = TYPE_BINFO (BINFO_TYPE (binfo));
       vbase;
       vbase = TREE_CHAIN (vbase))
    {
      tree b;
      tree delta;
      
      if (!TREE_VIA_VIRTUAL (vbase))
	continue;

      /* Find the instance of this virtual base in the complete
	 object.  */
      b = binfo_for_vbase (BINFO_TYPE (vbase), t);

      /* If we've already got an offset for this virtual base, we
	 don't need another one.  */
      if (BINFO_VTABLE_PATH_MARKED (b))
	continue;
      SET_BINFO_VTABLE_PATH_MARKED (b);

      /* Figure out where we can find this vbase offset.  */
      delta = size_binop (MULT_EXPR, 
			  vid->index,
			  convert (ssizetype,
				   TYPE_SIZE_UNIT (vtable_entry_type)));
      if (vid->primary_vtbl_p)
	BINFO_VPTR_FIELD (b) = delta;

      if (binfo != TYPE_BINFO (t))
	{
	  tree orig_vbase;

	  /* Find the instance of this virtual base in the type of BINFO.  */
	  orig_vbase = binfo_for_vbase (BINFO_TYPE (vbase),
					BINFO_TYPE (binfo));

	  /* The vbase offset had better be the same.  */
	  if (!tree_int_cst_equal (delta,
				   BINFO_VPTR_FIELD (orig_vbase)))
	    abort ();
	}

      /* The next vbase will come at a more negative offset.  */
      vid->index = size_binop (MINUS_EXPR, vid->index,
			       ssize_int (TARGET_VTABLE_DATA_ENTRY_DISTANCE));

      /* The initializer is the delta from BINFO to this virtual base.
	 The vbase offsets go in reverse inheritance-graph order, and
	 we are walking in inheritance graph order so these end up in
	 the right order.  */
      delta = size_diffop (BINFO_OFFSET (b), BINFO_OFFSET (non_primary_binfo));
      
      *vid->last_init 
	= build_tree_list (NULL_TREE,
			   fold (build1 (NOP_EXPR, 
					 vtable_entry_type,
					 delta)));
      vid->last_init = &TREE_CHAIN (*vid->last_init);
    }
}

/* Adds the initializers for the vcall offset entries in the vtable
   for BINFO (which is part of the class hierarchy dominated by VID->DERIVED)
   to VID->INITS.  */

static void
build_vcall_offset_vtbl_entries (binfo, vid)
     tree binfo;
     vtbl_init_data *vid;
{
  /* We only need these entries if this base is a virtual base.  We
     compute the indices -- but do not add to the vtable -- when
     building the main vtable for a class.  */
  if (TREE_VIA_VIRTUAL (binfo) || binfo == TYPE_BINFO (vid->derived))
    {
      /* We need a vcall offset for each of the virtual functions in this
	 vtable.  For example:

	   class A { virtual void f (); };
	   class B1 : virtual public A { virtual void f (); };
	   class B2 : virtual public A { virtual void f (); };
	   class C: public B1, public B2 { virtual void f (); };

	 A C object has a primary base of B1, which has a primary base of A.  A
	 C also has a secondary base of B2, which no longer has a primary base
	 of A.  So the B2-in-C construction vtable needs a secondary vtable for
	 A, which will adjust the A* to a B2* to call f.  We have no way of
	 knowing what (or even whether) this offset will be when we define B2,
	 so we store this "vcall offset" in the A sub-vtable and look it up in
	 a "virtual thunk" for B2::f.

	 We need entries for all the functions in our primary vtable and
	 in our non-virtual bases' secondary vtables.  */
      vid->vbase = binfo;
      /* If we are just computing the vcall indices -- but do not need
	 the actual entries -- not that.  */
      if (!TREE_VIA_VIRTUAL (binfo))
	vid->generate_vcall_entries = false;
      /* Now, walk through the non-virtual bases, adding vcall offsets.  */
      add_vcall_offset_vtbl_entries_r (binfo, vid);
    }
}

/* Build vcall offsets, starting with those for BINFO.  */

static void
add_vcall_offset_vtbl_entries_r (binfo, vid)
     tree binfo;
     vtbl_init_data *vid;
{
  int i;
  tree primary_binfo;

  /* Don't walk into virtual bases -- except, of course, for the
     virtual base for which we are building vcall offsets.  Any
     primary virtual base will have already had its offsets generated
     through the recursion in build_vcall_and_vbase_vtbl_entries.  */
  if (TREE_VIA_VIRTUAL (binfo) && vid->vbase != binfo)
    return;
  
  /* If BINFO has a primary base, process it first.  */
  primary_binfo = get_primary_binfo (binfo);
  if (primary_binfo)
    add_vcall_offset_vtbl_entries_r (primary_binfo, vid);

  /* Add BINFO itself to the list.  */
  add_vcall_offset_vtbl_entries_1 (binfo, vid);

  /* Scan the non-primary bases of BINFO.  */
  for (i = 0; i < BINFO_N_BASETYPES (binfo); ++i) 
    {
      tree base_binfo;
      
      base_binfo = BINFO_BASETYPE (binfo, i);
      if (base_binfo != primary_binfo)
	add_vcall_offset_vtbl_entries_r (base_binfo, vid);
    }
}

/* Called from build_vcall_offset_vtbl_entries_r.  */

static void
add_vcall_offset_vtbl_entries_1 (binfo, vid)
     tree binfo;
     vtbl_init_data* vid;
{
  /* Make entries for the rest of the virtuals.  */
  if (abi_version_at_least (2))
    {
      tree orig_fn;

      /* The ABI requires that the methods be processed in declaration
	 order.  G++ 3.2 used the order in the vtable.  */
      for (orig_fn = TYPE_METHODS (BINFO_TYPE (binfo));
	   orig_fn;
	   orig_fn = TREE_CHAIN (orig_fn))
	if (DECL_VINDEX (orig_fn))
	  add_vcall_offset (orig_fn, binfo, vid);
    }
  else
    {
      tree derived_virtuals;
      tree base_virtuals;
      tree orig_virtuals;
      /* If BINFO is a primary base, the most derived class which has
	 BINFO as a primary base; otherwise, just BINFO.  */
      tree non_primary_binfo;

      /* We might be a primary base class.  Go up the inheritance hierarchy
	 until we find the most derived class of which we are a primary base:
	 it is the BINFO_VIRTUALS there that we need to consider.  */
      non_primary_binfo = binfo;
      while (BINFO_INHERITANCE_CHAIN (non_primary_binfo))
	{
	  tree b;

	  /* If we have reached a virtual base, then it must be vid->vbase,
	     because we ignore other virtual bases in
	     add_vcall_offset_vtbl_entries_r.  In turn, it must be a primary
	     base (possibly multi-level) of vid->binfo, or we wouldn't
	     have called build_vcall_and_vbase_vtbl_entries for it.  But it
	     might be a lost primary, so just skip down to vid->binfo.  */
	  if (TREE_VIA_VIRTUAL (non_primary_binfo))
	    {
	      if (non_primary_binfo != vid->vbase)
		abort ();
	      non_primary_binfo = vid->binfo;
	      break;
	    }

	  b = BINFO_INHERITANCE_CHAIN (non_primary_binfo);
	  if (get_primary_binfo (b) != non_primary_binfo)
	    break;
	  non_primary_binfo = b;
	}

      if (vid->ctor_vtbl_p)
	/* For a ctor vtable we need the equivalent binfo within the hierarchy
	   where rtti_binfo is the most derived type.  */
	non_primary_binfo = get_original_base
          (non_primary_binfo, TYPE_BINFO (BINFO_TYPE (vid->rtti_binfo)));
      
      for (base_virtuals = BINFO_VIRTUALS (binfo),
	     derived_virtuals = BINFO_VIRTUALS (non_primary_binfo),
	     orig_virtuals = BINFO_VIRTUALS (TYPE_BINFO (BINFO_TYPE (binfo)));
	   base_virtuals;
	   base_virtuals = TREE_CHAIN (base_virtuals),
	     derived_virtuals = TREE_CHAIN (derived_virtuals),
	     orig_virtuals = TREE_CHAIN (orig_virtuals))
	{
	  tree orig_fn;

	  /* Find the declaration that originally caused this function to
	     be present in BINFO_TYPE (binfo).  */
	  orig_fn = BV_FN (orig_virtuals);

	  /* When processing BINFO, we only want to generate vcall slots for
	     function slots introduced in BINFO.  So don't try to generate
	     one if the function isn't even defined in BINFO.  */
	  if (!same_type_p (DECL_CONTEXT (orig_fn), BINFO_TYPE (binfo)))
	    continue;

	  add_vcall_offset (orig_fn, binfo, vid);
	}
    }
}

/* Add a vcall offset entry for ORIG_FN to the vtable.  */

static void
add_vcall_offset (tree orig_fn, tree binfo, vtbl_init_data *vid)
{
  size_t i;
  tree vcall_offset;

  /* If there is already an entry for a function with the same
     signature as FN, then we do not need a second vcall offset.
     Check the list of functions already present in the derived
     class vtable.  */
  for (i = 0; i < VARRAY_ACTIVE_SIZE (vid->fns); ++i) 
    {
      tree derived_entry;

      derived_entry = VARRAY_TREE (vid->fns, i);
      if (same_signature_p (derived_entry, orig_fn)
	  /* We only use one vcall offset for virtual destructors,
	     even though there are two virtual table entries.  */
	  || (DECL_DESTRUCTOR_P (derived_entry)
	      && DECL_DESTRUCTOR_P (orig_fn)))
	return;
    }

  /* If we are building these vcall offsets as part of building
     the vtable for the most derived class, remember the vcall
     offset.  */
  if (vid->binfo == TYPE_BINFO (vid->derived))
    CLASSTYPE_VCALL_INDICES (vid->derived) 
      = tree_cons (orig_fn, vid->index, 
		   CLASSTYPE_VCALL_INDICES (vid->derived));

  /* The next vcall offset will be found at a more negative
     offset.  */
  vid->index = size_binop (MINUS_EXPR, vid->index,
			   ssize_int (TARGET_VTABLE_DATA_ENTRY_DISTANCE));

  /* Keep track of this function.  */
  VARRAY_PUSH_TREE (vid->fns, orig_fn);

  if (vid->generate_vcall_entries)
    {
      tree base;
      tree fn;

      /* Find the overriding function.  */
      fn = find_final_overrider (vid->rtti_binfo, binfo, orig_fn);
      if (fn == error_mark_node)
	vcall_offset = build1 (NOP_EXPR, vtable_entry_type,
			       integer_zero_node);
      else
	{
	  base = TREE_VALUE (fn);

	  /* The vbase we're working on is a primary base of
	     vid->binfo.  But it might be a lost primary, so its
	     BINFO_OFFSET might be wrong, so we just use the
	     BINFO_OFFSET from vid->binfo.  */
	  vcall_offset = size_diffop (BINFO_OFFSET (base),
				      BINFO_OFFSET (vid->binfo));
	  vcall_offset = fold (build1 (NOP_EXPR, vtable_entry_type,
				       vcall_offset));
	}
      /* Add the intiailizer to the vtable.  */
      *vid->last_init = build_tree_list (NULL_TREE, vcall_offset);
      vid->last_init = &TREE_CHAIN (*vid->last_init);
    }
}

/* Return vtbl initializers for the RTTI entries coresponding to the
   BINFO's vtable.  The RTTI entries should indicate the object given
   by VID->rtti_binfo.  */

static void
build_rtti_vtbl_entries (binfo, vid)
     tree binfo;
     vtbl_init_data *vid;
{
  tree b;
  tree t;
  tree basetype;
  tree offset;
  tree decl;
  tree init;

  basetype = BINFO_TYPE (binfo);
  t = BINFO_TYPE (vid->rtti_binfo);

  /* To find the complete object, we will first convert to our most
     primary base, and then add the offset in the vtbl to that value.  */
  b = binfo;
  while (CLASSTYPE_HAS_PRIMARY_BASE_P (BINFO_TYPE (b))
         && !BINFO_LOST_PRIMARY_P (b))
    {
      tree primary_base;

      primary_base = get_primary_binfo (b);
      my_friendly_assert (BINFO_PRIMARY_BASE_OF (primary_base) == b, 20010127);
      b = primary_base;
    }
  offset = size_diffop (BINFO_OFFSET (vid->rtti_binfo), BINFO_OFFSET (b));

  /* The second entry is the address of the typeinfo object.  */
  if (flag_rtti)
    decl = build_address (get_tinfo_decl (t));
  else
    decl = integer_zero_node;
  
  /* Convert the declaration to a type that can be stored in the
     vtable.  */
  init = build_nop (vfunc_ptr_type_node, decl);
  *vid->last_init = build_tree_list (NULL_TREE, init);
  vid->last_init = &TREE_CHAIN (*vid->last_init);

  /* Add the offset-to-top entry.  It comes earlier in the vtable that
     the the typeinfo entry.  Convert the offset to look like a
     function pointer, so that we can put it in the vtable.  */
  init = build_nop (vfunc_ptr_type_node, offset);
  *vid->last_init = build_tree_list (NULL_TREE, init);
  vid->last_init = &TREE_CHAIN (*vid->last_init);
}
