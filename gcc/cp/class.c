/* Functions related to building classes and their related objects.
   Copyright (C) 1987-2014 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com)

This file is part of GCC.

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


/* High-level class interface.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "stringpool.h"
#include "stor-layout.h"
#include "attribs.h"
#include "pointer-set.h"
#include "hash-table.h"
#include "cp-tree.h"
#include "flags.h"
#include "toplev.h"
#include "target.h"
#include "convert.h"
#include "cgraph.h"
#include "dumpfile.h"
#include "splay-tree.h"
#include "gimplify.h"

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

  /* Nonzero if this class is no longer open, because of a call to
     push_to_top_level.  */
  size_t hidden;
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
  vec<constructor_elt, va_gc> *inits;
  /* The binfo for the virtual base for which we're building
     vcall offset initializers.  */
  tree vbase;
  /* The functions in vbase for which we have already provided vcall
     offsets.  */
  vec<tree, va_gc> *fns;
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
typedef int (*subobject_offset_fn) (tree, tree, splay_tree);

/* The stack itself.  This is a dynamically resized array.  The
   number of elements allocated is CURRENT_CLASS_STACK_SIZE.  */
static int current_class_stack_size;
static class_stack_node_t current_class_stack;

/* The size of the largest empty class seen in this translation unit.  */
static GTY (()) tree sizeof_biggest_empty_class;

/* An array of all local classes present in this translation unit, in
   declaration order.  */
vec<tree, va_gc> *local_classes;

static tree get_vfield_name (tree);
static void finish_struct_anon (tree);
static tree get_vtable_name (tree);
static tree get_basefndecls (tree, tree);
static int build_primary_vtable (tree, tree);
static int build_secondary_vtable (tree);
static void finish_vtbls (tree);
static void modify_vtable_entry (tree, tree, tree, tree, tree *);
static void finish_struct_bits (tree);
static int alter_access (tree, tree, tree);
static void handle_using_decl (tree, tree);
static tree dfs_modify_vtables (tree, void *);
static tree modify_all_vtables (tree, tree);
static void determine_primary_bases (tree);
static void finish_struct_methods (tree);
static void maybe_warn_about_overly_private_class (tree);
static int method_name_cmp (const void *, const void *);
static int resort_method_name_cmp (const void *, const void *);
static void add_implicitly_declared_members (tree, tree*, int, int);
static tree fixed_type_or_null (tree, int *, int *);
static tree build_simple_base_path (tree expr, tree binfo);
static tree build_vtbl_ref_1 (tree, tree);
static void build_vtbl_initializer (tree, tree, tree, tree, int *,
				    vec<constructor_elt, va_gc> **);
static int count_fields (tree);
static int add_fields_to_record_type (tree, struct sorted_fields_type*, int);
static void insert_into_classtype_sorted_fields (tree, tree, int);
static bool check_bitfield_decl (tree);
static void check_field_decl (tree, tree, int *, int *, int *);
static void check_field_decls (tree, tree *, int *, int *);
static tree *build_base_field (record_layout_info, tree, splay_tree, tree *);
static void build_base_fields (record_layout_info, splay_tree, tree *);
static void check_methods (tree);
static void remove_zero_width_bit_fields (tree);
static void check_bases (tree, int *, int *);
static void check_bases_and_members (tree);
static tree create_vtable_ptr (tree, tree *);
static void include_empty_classes (record_layout_info);
static void layout_class_type (tree, tree *);
static void propagate_binfo_offsets (tree, tree);
static void layout_virtual_bases (record_layout_info, splay_tree);
static void build_vbase_offset_vtbl_entries (tree, vtbl_init_data *);
static void add_vcall_offset_vtbl_entries_r (tree, vtbl_init_data *);
static void add_vcall_offset_vtbl_entries_1 (tree, vtbl_init_data *);
static void build_vcall_offset_vtbl_entries (tree, vtbl_init_data *);
static void add_vcall_offset (tree, tree, vtbl_init_data *);
static void layout_vtable_decl (tree, int);
static tree dfs_find_final_overrider_pre (tree, void *);
static tree dfs_find_final_overrider_post (tree, void *);
static tree find_final_overrider (tree, tree, tree);
static int make_new_vtable (tree, tree);
static tree get_primary_binfo (tree);
static int maybe_indent_hierarchy (FILE *, int, int);
static tree dump_class_hierarchy_r (FILE *, int, tree, tree, int);
static void dump_class_hierarchy (tree);
static void dump_class_hierarchy_1 (FILE *, int, tree);
static void dump_array (FILE *, tree);
static void dump_vtable (tree, tree, tree);
static void dump_vtt (tree, tree);
static void dump_thunk (FILE *, int, tree);
static tree build_vtable (tree, tree, tree);
static void initialize_vtable (tree, vec<constructor_elt, va_gc> *);
static void layout_nonempty_base_or_field (record_layout_info,
					   tree, tree, splay_tree);
static tree end_of_class (tree, int);
static bool layout_empty_base (record_layout_info, tree, tree, splay_tree);
static void accumulate_vtbl_inits (tree, tree, tree, tree, tree,
				   vec<constructor_elt, va_gc> **);
static void dfs_accumulate_vtbl_inits (tree, tree, tree, tree, tree,
				       vec<constructor_elt, va_gc> **);
static void build_rtti_vtbl_entries (tree, vtbl_init_data *);
static void build_vcall_and_vbase_vtbl_entries (tree, vtbl_init_data *);
static void clone_constructors_and_destructors (tree);
static tree build_clone (tree, tree);
static void update_vtable_entry_for_fn (tree, tree, tree, tree *, unsigned);
static void build_ctor_vtbl_group (tree, tree);
static void build_vtt (tree);
static tree binfo_ctor_vtable (tree);
static void build_vtt_inits (tree, tree, vec<constructor_elt, va_gc> **,
			     tree *);
static tree dfs_build_secondary_vptr_vtt_inits (tree, void *);
static tree dfs_fixup_binfo_vtbls (tree, void *);
static int record_subobject_offset (tree, tree, splay_tree);
static int check_subobject_offset (tree, tree, splay_tree);
static int walk_subobject_offsets (tree, subobject_offset_fn,
				   tree, splay_tree, tree, int);
static void record_subobject_offsets (tree, tree, splay_tree, bool);
static int layout_conflict_p (tree, tree, splay_tree, int);
static int splay_tree_compare_integer_csts (splay_tree_key k1,
					    splay_tree_key k2);
static void warn_about_ambiguous_bases (tree);
static bool type_requires_array_cookie (tree);
static bool contains_empty_class_p (tree);
static bool base_derived_from (tree, tree);
static int empty_base_at_nonzero_offset_p (tree, tree, splay_tree);
static tree end_of_base (tree);
static tree get_vcall_index (tree, tree);

/* Variables shared between class.c and call.c.  */

int n_vtables = 0;
int n_vtable_entries = 0;
int n_vtable_searches = 0;
int n_vtable_elems = 0;
int n_convert_harshness = 0;
int n_compute_conversion_costs = 0;
int n_inner_fields_searched = 0;

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
build_base_path (enum tree_code code,
		 tree expr,
		 tree binfo,
		 int nonnull,
		 tsubst_flags_t complain)
{
  tree v_binfo = NULL_TREE;
  tree d_binfo = NULL_TREE;
  tree probe;
  tree offset;
  tree target_type;
  tree null_test = NULL;
  tree ptr_target_type;
  int fixed_type_p;
  int want_pointer = TYPE_PTR_P (TREE_TYPE (expr));
  bool has_empty = false;
  bool virtual_access;

  if (expr == error_mark_node || binfo == error_mark_node || !binfo)
    return error_mark_node;

  for (probe = binfo; probe; probe = BINFO_INHERITANCE_CHAIN (probe))
    {
      d_binfo = probe;
      if (is_empty_class (BINFO_TYPE (probe)))
	has_empty = true;
      if (!v_binfo && BINFO_VIRTUAL_P (probe))
	v_binfo = probe;
    }

  probe = TYPE_MAIN_VARIANT (TREE_TYPE (expr));
  if (want_pointer)
    probe = TYPE_MAIN_VARIANT (TREE_TYPE (probe));

  if (code == PLUS_EXPR
      && !SAME_BINFO_TYPE_P (BINFO_TYPE (d_binfo), probe))
    {
      /* This can happen when adjust_result_of_qualified_name_lookup can't
	 find a unique base binfo in a call to a member function.  We
	 couldn't give the diagnostic then since we might have been calling
	 a static member function, so we do it now.  */
      if (complain & tf_error)
	{
	  tree base = lookup_base (probe, BINFO_TYPE (d_binfo),
				   ba_unique, NULL, complain);
	  gcc_assert (base == error_mark_node);
	}
      return error_mark_node;
    }

  gcc_assert ((code == MINUS_EXPR
	       && SAME_BINFO_TYPE_P (BINFO_TYPE (binfo), probe))
	      || code == PLUS_EXPR);

  if (binfo == d_binfo)
    /* Nothing to do.  */
    return expr;

  if (code == MINUS_EXPR && v_binfo)
    {
      if (complain & tf_error)
	{
	  if (SAME_BINFO_TYPE_P (BINFO_TYPE (binfo), BINFO_TYPE (v_binfo)))
	    {
	      if (want_pointer)
		error ("cannot convert from pointer to base class %qT to "
		       "pointer to derived class %qT because the base is "
		       "virtual", BINFO_TYPE (binfo), BINFO_TYPE (d_binfo));
	      else
		error ("cannot convert from base class %qT to derived "
		       "class %qT because the base is virtual",
		       BINFO_TYPE (binfo), BINFO_TYPE (d_binfo));
	    }	      
	  else
	    {
	      if (want_pointer)
		error ("cannot convert from pointer to base class %qT to "
		       "pointer to derived class %qT via virtual base %qT",
		       BINFO_TYPE (binfo), BINFO_TYPE (d_binfo),
		       BINFO_TYPE (v_binfo));
	      else
		error ("cannot convert from base class %qT to derived "
		       "class %qT via virtual base %qT", BINFO_TYPE (binfo),
		       BINFO_TYPE (d_binfo), BINFO_TYPE (v_binfo));
	    }
	}
      return error_mark_node;
    }

  if (!want_pointer)
    /* This must happen before the call to save_expr.  */
    expr = cp_build_addr_expr (expr, complain);
  else
    expr = mark_rvalue_use (expr);

  offset = BINFO_OFFSET (binfo);
  fixed_type_p = resolves_to_fixed_type_p (expr, &nonnull);
  target_type = code == PLUS_EXPR ? BINFO_TYPE (binfo) : BINFO_TYPE (d_binfo);
  /* TARGET_TYPE has been extracted from BINFO, and, is therefore always
     cv-unqualified.  Extract the cv-qualifiers from EXPR so that the
     expression returned matches the input.  */
  target_type = cp_build_qualified_type
    (target_type, cp_type_quals (TREE_TYPE (TREE_TYPE (expr))));
  ptr_target_type = build_pointer_type (target_type);

  /* Do we need to look in the vtable for the real offset?  */
  virtual_access = (v_binfo && fixed_type_p <= 0);

  /* Don't bother with the calculations inside sizeof; they'll ICE if the
     source type is incomplete and the pointer value doesn't matter.  In a
     template (even in fold_non_dependent_expr), we don't have vtables set
     up properly yet, and the value doesn't matter there either; we're just
     interested in the result of overload resolution.  */
  if (cp_unevaluated_operand != 0
      || in_template_function ())
    {
      expr = build_nop (ptr_target_type, expr);
      if (!want_pointer)
	expr = build_indirect_ref (EXPR_LOCATION (expr), expr, RO_NULL);
      return expr;
    }

  /* If we're in an NSDMI, we don't have the full constructor context yet
     that we need for converting to a virtual base, so just build a stub
     CONVERT_EXPR and expand it later in bot_replace.  */
  if (virtual_access && fixed_type_p < 0
      && current_scope () != current_function_decl)
    {
      expr = build1 (CONVERT_EXPR, ptr_target_type, expr);
      CONVERT_EXPR_VBASE_PATH (expr) = true;
      if (!want_pointer)
	expr = build_indirect_ref (EXPR_LOCATION (expr), expr, RO_NULL);
      return expr;
    }

  /* Do we need to check for a null pointer?  */
  if (want_pointer && !nonnull)
    {
      /* If we know the conversion will not actually change the value
	 of EXPR, then we can avoid testing the expression for NULL.
	 We have to avoid generating a COMPONENT_REF for a base class
	 field, because other parts of the compiler know that such
	 expressions are always non-NULL.  */
      if (!virtual_access && integer_zerop (offset))
	return build_nop (ptr_target_type, expr);
      null_test = error_mark_node;
    }

  /* Protect against multiple evaluation if necessary.  */
  if (TREE_SIDE_EFFECTS (expr) && (null_test || virtual_access))
    expr = save_expr (expr);

  /* Now that we've saved expr, build the real null test.  */
  if (null_test)
    {
      tree zero = cp_convert (TREE_TYPE (expr), nullptr_node, complain);
      null_test = fold_build2_loc (input_location, NE_EXPR, boolean_type_node,
			       expr, zero);
    }

  /* If this is a simple base reference, express it as a COMPONENT_REF.  */
  if (code == PLUS_EXPR && !virtual_access
      /* We don't build base fields for empty bases, and they aren't very
	 interesting to the optimizers anyway.  */
      && !has_empty)
    {
      expr = cp_build_indirect_ref (expr, RO_NULL, complain);
      expr = build_simple_base_path (expr, binfo);
      if (want_pointer)
	expr = build_address (expr);
      target_type = TREE_TYPE (expr);
      goto out;
    }

  if (virtual_access)
    {
      /* Going via virtual base V_BINFO.  We need the static offset
	 from V_BINFO to BINFO, and the dynamic offset from D_BINFO to
	 V_BINFO.  That offset is an entry in D_BINFO's vtable.  */
      tree v_offset;

      if (fixed_type_p < 0 && in_base_initializer)
	{
	  /* In a base member initializer, we cannot rely on the
	     vtable being set up.  We have to indirect via the
	     vtt_parm.  */
	  tree t;

	  t = TREE_TYPE (TYPE_VFIELD (current_class_type));
	  t = build_pointer_type (t);
	  v_offset = convert (t, current_vtt_parm);
	  v_offset = cp_build_indirect_ref (v_offset, RO_NULL, complain);
	}
      else
	v_offset = build_vfield_ref (cp_build_indirect_ref (expr, RO_NULL,
                                                            complain),
				     TREE_TYPE (TREE_TYPE (expr)));

      v_offset = fold_build_pointer_plus (v_offset, BINFO_VPTR_FIELD (v_binfo));
      v_offset = build1 (NOP_EXPR,
			 build_pointer_type (ptrdiff_type_node),
			 v_offset);
      v_offset = cp_build_indirect_ref (v_offset, RO_NULL, complain);
      TREE_CONSTANT (v_offset) = 1;

      offset = convert_to_integer (ptrdiff_type_node,
				   size_diffop_loc (input_location, offset,
						BINFO_OFFSET (v_binfo)));

      if (!integer_zerop (offset))
	v_offset = build2 (code, ptrdiff_type_node, v_offset, offset);

      if (fixed_type_p < 0)
	/* Negative fixed_type_p means this is a constructor or destructor;
	   virtual base layout is fixed in in-charge [cd]tors, but not in
	   base [cd]tors.  */
	offset = build3 (COND_EXPR, ptrdiff_type_node,
			 build2 (EQ_EXPR, boolean_type_node,
				 current_in_charge_parm, integer_zero_node),
			 v_offset,
			 convert_to_integer (ptrdiff_type_node,
					     BINFO_OFFSET (binfo)));
      else
	offset = v_offset;
    }

  if (want_pointer)
    target_type = ptr_target_type;

  expr = build1 (NOP_EXPR, ptr_target_type, expr);

  if (!integer_zerop (offset))
    {
      offset = fold_convert (sizetype, offset);
      if (code == MINUS_EXPR)
	offset = fold_build1_loc (input_location, NEGATE_EXPR, sizetype, offset);
      expr = fold_build_pointer_plus (expr, offset);
    }
  else
    null_test = NULL;

  if (!want_pointer)
    expr = cp_build_indirect_ref (expr, RO_NULL, complain);

 out:
  if (null_test)
    expr = fold_build3_loc (input_location, COND_EXPR, target_type, null_test, expr,
			    build_zero_cst (target_type));

  return expr;
}

/* Subroutine of build_base_path; EXPR and BINFO are as in that function.
   Perform a derived-to-base conversion by recursively building up a
   sequence of COMPONENT_REFs to the appropriate base fields.  */

static tree
build_simple_base_path (tree expr, tree binfo)
{
  tree type = BINFO_TYPE (binfo);
  tree d_binfo = BINFO_INHERITANCE_CHAIN (binfo);
  tree field;

  if (d_binfo == NULL_TREE)
    {
      tree temp;

      gcc_assert (TYPE_MAIN_VARIANT (TREE_TYPE (expr)) == type);

      /* Transform `(a, b).x' into `(*(a, &b)).x', `(a ? b : c).x'
	 into `(*(a ?  &b : &c)).x', and so on.  A COND_EXPR is only
	 an lvalue in the front end; only _DECLs and _REFs are lvalues
	 in the back end.  */
      temp = unary_complex_lvalue (ADDR_EXPR, expr);
      if (temp)
	expr = cp_build_indirect_ref (temp, RO_NULL, tf_warning_or_error);

      return expr;
    }

  /* Recurse.  */
  expr = build_simple_base_path (expr, d_binfo);

  for (field = TYPE_FIELDS (BINFO_TYPE (d_binfo));
       field; field = DECL_CHAIN (field))
    /* Is this the base field created by build_base_field?  */
    if (TREE_CODE (field) == FIELD_DECL
	&& DECL_FIELD_IS_BASE (field)
	&& TREE_TYPE (field) == type
	/* If we're looking for a field in the most-derived class,
	   also check the field offset; we can have two base fields
	   of the same type if one is an indirect virtual base and one
	   is a direct non-virtual base.  */
	&& (BINFO_INHERITANCE_CHAIN (d_binfo)
	    || tree_int_cst_equal (byte_position (field),
				   BINFO_OFFSET (binfo))))
      {
	/* We don't use build_class_member_access_expr here, as that
	   has unnecessary checks, and more importantly results in
	   recursive calls to dfs_walk_once.  */
	int type_quals = cp_type_quals (TREE_TYPE (expr));

	expr = build3 (COMPONENT_REF,
		       cp_build_qualified_type (type, type_quals),
		       expr, field, NULL_TREE);
	expr = fold_if_not_in_template (expr);

	/* Mark the expression const or volatile, as appropriate.
	   Even though we've dealt with the type above, we still have
	   to mark the expression itself.  */
	if (type_quals & TYPE_QUAL_CONST)
	  TREE_READONLY (expr) = 1;
	if (type_quals & TYPE_QUAL_VOLATILE)
	  TREE_THIS_VOLATILE (expr) = 1;

	return expr;
      }

  /* Didn't find the base field?!?  */
  gcc_unreachable ();
}

/* Convert OBJECT to the base TYPE.  OBJECT is an expression whose
   type is a class type or a pointer to a class type.  In the former
   case, TYPE is also a class type; in the latter it is another
   pointer type.  If CHECK_ACCESS is true, an error message is emitted
   if TYPE is inaccessible.  If OBJECT has pointer type, the value is
   assumed to be non-NULL.  */

tree
convert_to_base (tree object, tree type, bool check_access, bool nonnull,
		 tsubst_flags_t complain)
{
  tree binfo;
  tree object_type;

  if (TYPE_PTR_P (TREE_TYPE (object)))
    {
      object_type = TREE_TYPE (TREE_TYPE (object));
      type = TREE_TYPE (type);
    }
  else
    object_type = TREE_TYPE (object);

  binfo = lookup_base (object_type, type, check_access ? ba_check : ba_unique,
		       NULL, complain);
  if (!binfo || binfo == error_mark_node)
    return error_mark_node;

  return build_base_path (PLUS_EXPR, object, binfo, nonnull, complain);
}

/* EXPR is an expression with unqualified class type.  BASE is a base
   binfo of that class type.  Returns EXPR, converted to the BASE
   type.  This function assumes that EXPR is the most derived class;
   therefore virtual bases can be found at their static offsets.  */

tree
convert_to_base_statically (tree expr, tree base)
{
  tree expr_type;

  expr_type = TREE_TYPE (expr);
  if (!SAME_BINFO_TYPE_P (BINFO_TYPE (base), expr_type))
    {
      /* If this is a non-empty base, use a COMPONENT_REF.  */
      if (!is_empty_class (BINFO_TYPE (base)))
	return build_simple_base_path (expr, base);

      /* We use fold_build2 and fold_convert below to simplify the trees
	 provided to the optimizers.  It is not safe to call these functions
	 when processing a template because they do not handle C++-specific
	 trees.  */
      gcc_assert (!processing_template_decl);
      expr = cp_build_addr_expr (expr, tf_warning_or_error);
      if (!integer_zerop (BINFO_OFFSET (base)))
        expr = fold_build_pointer_plus_loc (input_location,
					    expr, BINFO_OFFSET (base));
      expr = fold_convert (build_pointer_type (BINFO_TYPE (base)), expr);
      expr = build_fold_indirect_ref_loc (input_location, expr);
    }

  return expr;
}


tree
build_vfield_ref (tree datum, tree type)
{
  tree vfield, vcontext;

  if (datum == error_mark_node)
    return error_mark_node;

  /* First, convert to the requested type.  */
  if (!same_type_ignoring_top_level_qualifiers_p (TREE_TYPE (datum), type))
    datum = convert_to_base (datum, type, /*check_access=*/false,
			     /*nonnull=*/true, tf_warning_or_error);

  /* Second, the requested type may not be the owner of its own vptr.
     If not, convert to the base class that owns it.  We cannot use
     convert_to_base here, because VCONTEXT may appear more than once
     in the inheritance hierarchy of TYPE, and thus direct conversion
     between the types may be ambiguous.  Following the path back up
     one step at a time via primary bases avoids the problem.  */
  vfield = TYPE_VFIELD (type);
  vcontext = DECL_CONTEXT (vfield);
  while (!same_type_ignoring_top_level_qualifiers_p (vcontext, type))
    {
      datum = build_simple_base_path (datum, CLASSTYPE_PRIMARY_BINFO (type));
      type = TREE_TYPE (datum);
    }

  return build3 (COMPONENT_REF, TREE_TYPE (vfield), datum, vfield, NULL_TREE);
}

/* Given an object INSTANCE, return an expression which yields the
   vtable element corresponding to INDEX.  There are many special
   cases for INSTANCE which we take care of here, mainly to avoid
   creating extra tree nodes when we don't have to.  */

static tree
build_vtbl_ref_1 (tree instance, tree idx)
{
  tree aref;
  tree vtbl = NULL_TREE;

  /* Try to figure out what a reference refers to, and
     access its virtual function table directly.  */

  int cdtorp = 0;
  tree fixed_type = fixed_type_or_null (instance, NULL, &cdtorp);

  tree basetype = non_reference (TREE_TYPE (instance));

  if (fixed_type && !cdtorp)
    {
      tree binfo = lookup_base (fixed_type, basetype,
				ba_unique, NULL, tf_none);
      if (binfo && binfo != error_mark_node)
	vtbl = unshare_expr (BINFO_VTABLE (binfo));
    }

  if (!vtbl)
    vtbl = build_vfield_ref (instance, basetype);

  aref = build_array_ref (input_location, vtbl, idx);
  TREE_CONSTANT (aref) |= TREE_CONSTANT (vtbl) && TREE_CONSTANT (idx);

  return aref;
}

tree
build_vtbl_ref (tree instance, tree idx)
{
  tree aref = build_vtbl_ref_1 (instance, idx);

  return aref;
}

/* Given a stable object pointer INSTANCE_PTR, return an expression which
   yields a function pointer corresponding to vtable element INDEX.  */

tree
build_vfn_ref (tree instance_ptr, tree idx)
{
  tree aref;

  aref = build_vtbl_ref_1 (cp_build_indirect_ref (instance_ptr, RO_NULL,
                                                  tf_warning_or_error), 
                           idx);

  /* When using function descriptors, the address of the
     vtable entry is treated as a function pointer.  */
  if (TARGET_VTABLE_USES_DESCRIPTORS)
    aref = build1 (NOP_EXPR, TREE_TYPE (aref),
		   cp_build_addr_expr (aref, tf_warning_or_error));

  /* Remember this as a method reference, for later devirtualization.  */
  aref = build3 (OBJ_TYPE_REF, TREE_TYPE (aref), aref, instance_ptr, idx);

  return aref;
}

/* Return the name of the virtual function table (as an IDENTIFIER_NODE)
   for the given TYPE.  */

static tree
get_vtable_name (tree type)
{
  return mangle_vtbl_for_type (type);
}

/* DECL is an entity associated with TYPE, like a virtual table or an
   implicitly generated constructor.  Determine whether or not DECL
   should have external or internal linkage at the object file
   level.  This routine does not deal with COMDAT linkage and other
   similar complexities; it simply sets TREE_PUBLIC if it possible for
   entities in other translation units to contain copies of DECL, in
   the abstract.  */

void
set_linkage_according_to_type (tree /*type*/, tree decl)
{
  TREE_PUBLIC (decl) = 1;
  determine_visibility (decl);
}

/* Create a VAR_DECL for a primary or secondary vtable for CLASS_TYPE.
   (For a secondary vtable for B-in-D, CLASS_TYPE should be D, not B.)
   Use NAME for the name of the vtable, and VTABLE_TYPE for its type.  */

static tree
build_vtable (tree class_type, tree name, tree vtable_type)
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
  DECL_VTABLE_OR_VTT_P (decl) = 1;
  /* At one time the vtable info was grabbed 2 words at a time.  This
     fails on sparc unless you have 8-byte alignment.  (tiemann) */
  DECL_ALIGN (decl) = MAX (TYPE_ALIGN (double_type_node),
			   DECL_ALIGN (decl));
  set_linkage_according_to_type (class_type, decl);
  /* The vtable has not been defined -- yet.  */
  DECL_EXTERNAL (decl) = 1;
  DECL_NOT_REALLY_EXTERN (decl) = 1;

  /* Mark the VAR_DECL node representing the vtable itself as a
     "gratuitous" one, thereby forcing dwarfout.c to ignore it.  It
     is rather important that such things be ignored because any
     effort to actually generate DWARF for them will run into
     trouble when/if we encounter code like:

     #pragma interface
     struct S { virtual void member (); };

     because the artificial declaration of the vtable itself (as
     manufactured by the g++ front end) will say that the vtable is
     a static member of `S' but only *after* the debug output for
     the definition of `S' has already been output.  This causes
     grief because the DWARF entry for the definition of the vtable
     will try to refer back to an earlier *declaration* of the
     vtable as a static member of `S' and there won't be one.  We
     might be able to arrange to have the "vtable static member"
     attached to the member list for `S' before the debug info for
     `S' get written (which would solve the problem) but that would
     require more intrusive changes to the g++ front end.  */
  DECL_IGNORED_P (decl) = 1;

  return decl;
}

/* Get the VAR_DECL of the vtable for TYPE. TYPE need not be polymorphic,
   or even complete.  If this does not exist, create it.  If COMPLETE is
   nonzero, then complete the definition of it -- that will render it
   impossible to actually build the vtable, but is useful to get at those
   which are known to exist in the runtime.  */

tree
get_vtable_decl (tree type, int complete)
{
  tree decl;

  if (CLASSTYPE_VTABLES (type))
    return CLASSTYPE_VTABLES (type);

  decl = build_vtable (type, get_vtable_name (type), vtbl_type_node);
  CLASSTYPE_VTABLES (type) = decl;

  if (complete)
    {
      DECL_EXTERNAL (decl) = 1;
      cp_finish_decl (decl, NULL_TREE, false, NULL_TREE, 0);
    }

  return decl;
}

/* Build the primary virtual function table for TYPE.  If BINFO is
   non-NULL, build the vtable starting with the initial approximation
   that it is the same as the one which is the head of the association
   list.  Returns a nonzero value if a new vtable is actually
   created.  */

static int
build_primary_vtable (tree binfo, tree type)
{
  tree decl;
  tree virtuals;

  decl = get_vtable_decl (type, /*complete=*/0);

  if (binfo)
    {
      if (BINFO_NEW_VTABLE_MARKED (binfo))
	/* We have already created a vtable for this base, so there's
	   no need to do it again.  */
	return 0;

      virtuals = copy_list (BINFO_VIRTUALS (binfo));
      TREE_TYPE (decl) = TREE_TYPE (get_vtbl_decl_for_binfo (binfo));
      DECL_SIZE (decl) = TYPE_SIZE (TREE_TYPE (decl));
      DECL_SIZE_UNIT (decl) = TYPE_SIZE_UNIT (TREE_TYPE (decl));
    }
  else
    {
      gcc_assert (TREE_TYPE (decl) == vtbl_type_node);
      virtuals = NULL_TREE;
    }

  if (GATHER_STATISTICS)
    {
      n_vtables += 1;
      n_vtable_elems += list_length (virtuals);
    }

  /* Initialize the association list for this type, based
     on our first approximation.  */
  BINFO_VTABLE (TYPE_BINFO (type)) = decl;
  BINFO_VIRTUALS (TYPE_BINFO (type)) = virtuals;
  SET_BINFO_NEW_VTABLE_MARKED (TYPE_BINFO (type));
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
build_secondary_vtable (tree binfo)
{
  if (BINFO_NEW_VTABLE_MARKED (binfo))
    /* We already created a vtable for this base.  There's no need to
       do it again.  */
    return 0;

  /* Remember that we've created a vtable for this BINFO, so that we
     don't try to do so again.  */
  SET_BINFO_NEW_VTABLE_MARKED (binfo);

  /* Make fresh virtual list, so we can smash it later.  */
  BINFO_VIRTUALS (binfo) = copy_list (BINFO_VIRTUALS (binfo));

  /* Secondary vtables are laid out as part of the same structure as
     the primary vtable.  */
  BINFO_VTABLE (binfo) = NULL_TREE;
  return 1;
}

/* Create a new vtable for BINFO which is the hierarchy dominated by
   T. Return nonzero if we actually created a new vtable.  */

static int
make_new_vtable (tree t, tree binfo)
{
  if (binfo == TYPE_BINFO (t))
    /* In this case, it is *type*'s vtable we are modifying.  We start
       with the approximation that its vtable is that of the
       immediate base class.  */
    return build_primary_vtable (binfo, t);
  else
    /* This is our very own copy of `basetype' to play with.  Later,
       we will fill in all the virtual functions that override the
       virtual functions in these base classes which are not defined
       by the current type.  */
    return build_secondary_vtable (binfo);
}

/* Make *VIRTUALS, an entry on the BINFO_VIRTUALS list for BINFO
   (which is in the hierarchy dominated by T) list FNDECL as its
   BV_FN.  DELTA is the required constant adjustment from the `this'
   pointer where the vtable entry appears to the `this' required when
   the function is actually called.  */

static void
modify_vtable_entry (tree t,
		     tree binfo,
		     tree fndecl,
		     tree delta,
		     tree *virtuals)
{
  tree v;

  v = *virtuals;

  if (fndecl != BV_FN (v)
      || !tree_int_cst_equal (delta, BV_DELTA (v)))
    {
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

      BV_DELTA (v) = delta;
      BV_VCALL_INDEX (v) = NULL_TREE;
      BV_FN (v) = fndecl;
    }
}


/* Add method METHOD to class TYPE.  If USING_DECL is non-null, it is
   the USING_DECL naming METHOD.  Returns true if the method could be
   added to the method vec.  */

bool
add_method (tree type, tree method, tree using_decl)
{
  unsigned slot;
  tree overload;
  bool template_conv_p = false;
  bool conv_p;
  vec<tree, va_gc> *method_vec;
  bool complete_p;
  bool insert_p = false;
  tree current_fns;
  tree fns;

  if (method == error_mark_node)
    return false;

  complete_p = COMPLETE_TYPE_P (type);
  conv_p = DECL_CONV_FN_P (method);
  if (conv_p)
    template_conv_p = (TREE_CODE (method) == TEMPLATE_DECL
		       && DECL_TEMPLATE_CONV_FN_P (method));

  method_vec = CLASSTYPE_METHOD_VEC (type);
  if (!method_vec)
    {
      /* Make a new method vector.  We start with 8 entries.  We must
	 allocate at least two (for constructors and destructors), and
	 we're going to end up with an assignment operator at some
	 point as well.  */
      vec_alloc (method_vec, 8);
      /* Create slots for constructors and destructors.  */
      method_vec->quick_push (NULL_TREE);
      method_vec->quick_push (NULL_TREE);
      CLASSTYPE_METHOD_VEC (type) = method_vec;
    }

  /* Maintain TYPE_HAS_USER_CONSTRUCTOR, etc.  */
  grok_special_member_properties (method);

  /* Constructors and destructors go in special slots.  */
  if (DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P (method))
    slot = CLASSTYPE_CONSTRUCTOR_SLOT;
  else if (DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P (method))
    {
      slot = CLASSTYPE_DESTRUCTOR_SLOT;

      if (TYPE_FOR_JAVA (type))
	{
	  if (!DECL_ARTIFICIAL (method))
	    error ("Java class %qT cannot have a destructor", type);
	  else if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (type))
	    error ("Java class %qT cannot have an implicit non-trivial "
		   "destructor",
		   type);
	}
    }
  else
    {
      tree m;

      insert_p = true;
      /* See if we already have an entry with this name.  */
      for (slot = CLASSTYPE_FIRST_CONVERSION_SLOT;
	   vec_safe_iterate (method_vec, slot, &m);
	   ++slot)
	{
	  m = OVL_CURRENT (m);
	  if (template_conv_p)
	    {
	      if (TREE_CODE (m) == TEMPLATE_DECL
		  && DECL_TEMPLATE_CONV_FN_P (m))
		insert_p = false;
	      break;
	    }
	  if (conv_p && !DECL_CONV_FN_P (m))
	    break;
	  if (DECL_NAME (m) == DECL_NAME (method))
	    {
	      insert_p = false;
	      break;
	    }
	  if (complete_p
	      && !DECL_CONV_FN_P (m)
	      && DECL_NAME (m) > DECL_NAME (method))
	    break;
	}
    }
  current_fns = insert_p ? NULL_TREE : (*method_vec)[slot];

  /* Check to see if we've already got this method.  */
  for (fns = current_fns; fns; fns = OVL_NEXT (fns))
    {
      tree fn = OVL_CURRENT (fns);
      tree fn_type;
      tree method_type;
      tree parms1;
      tree parms2;

      if (TREE_CODE (fn) != TREE_CODE (method))
	continue;

      /* [over.load] Member function declarations with the
	 same name and the same parameter types cannot be
	 overloaded if any of them is a static member
	 function declaration.

	 [over.load] Member function declarations with the same name and
	 the same parameter-type-list as well as member function template
	 declarations with the same name, the same parameter-type-list, and
	 the same template parameter lists cannot be overloaded if any of
	 them, but not all, have a ref-qualifier.

	 [namespace.udecl] When a using-declaration brings names
	 from a base class into a derived class scope, member
	 functions in the derived class override and/or hide member
	 functions with the same name and parameter types in a base
	 class (rather than conflicting).  */
      fn_type = TREE_TYPE (fn);
      method_type = TREE_TYPE (method);
      parms1 = TYPE_ARG_TYPES (fn_type);
      parms2 = TYPE_ARG_TYPES (method_type);

      /* Compare the quals on the 'this' parm.  Don't compare
	 the whole types, as used functions are treated as
	 coming from the using class in overload resolution.  */
      if (! DECL_STATIC_FUNCTION_P (fn)
	  && ! DECL_STATIC_FUNCTION_P (method)
	  /* Either both or neither need to be ref-qualified for
	     differing quals to allow overloading.  */
	  && (FUNCTION_REF_QUALIFIED (fn_type)
	      == FUNCTION_REF_QUALIFIED (method_type))
	  && (type_memfn_quals (fn_type) != type_memfn_quals (method_type)
	      || type_memfn_rqual (fn_type) != type_memfn_rqual (method_type)))
	  continue;

      /* For templates, the return type and template parameters
	 must be identical.  */
      if (TREE_CODE (fn) == TEMPLATE_DECL
	  && (!same_type_p (TREE_TYPE (fn_type),
			    TREE_TYPE (method_type))
	      || !comp_template_parms (DECL_TEMPLATE_PARMS (fn),
				       DECL_TEMPLATE_PARMS (method))))
	continue;

      if (! DECL_STATIC_FUNCTION_P (fn))
	parms1 = TREE_CHAIN (parms1);
      if (! DECL_STATIC_FUNCTION_P (method))
	parms2 = TREE_CHAIN (parms2);

      if (compparms (parms1, parms2)
	  && (!DECL_CONV_FN_P (fn)
	      || same_type_p (TREE_TYPE (fn_type),
			      TREE_TYPE (method_type))))
	{
	  /* For function versions, their parms and types match
	     but they are not duplicates.  Record function versions
	     as and when they are found.  extern "C" functions are
	     not treated as versions.  */
	  if (TREE_CODE (fn) == FUNCTION_DECL
	      && TREE_CODE (method) == FUNCTION_DECL
	      && !DECL_EXTERN_C_P (fn)
	      && !DECL_EXTERN_C_P (method)
	      && targetm.target_option.function_versions (fn, method))
 	    {
	      /* Mark functions as versions if necessary.  Modify the mangled
		 decl name if necessary.  */
	      if (!DECL_FUNCTION_VERSIONED (fn))
		{
		  DECL_FUNCTION_VERSIONED (fn) = 1;
		  if (DECL_ASSEMBLER_NAME_SET_P (fn))
		    mangle_decl (fn);
		}
	      if (!DECL_FUNCTION_VERSIONED (method))
		{
		  DECL_FUNCTION_VERSIONED (method) = 1;
		  if (DECL_ASSEMBLER_NAME_SET_P (method))
		    mangle_decl (method);
		}
	      record_function_versions (fn, method);
	      continue;
	    }
	  if (DECL_INHERITED_CTOR_BASE (method))
	    {
	      if (DECL_INHERITED_CTOR_BASE (fn))
		{
		  error_at (DECL_SOURCE_LOCATION (method),
			    "%q#D inherited from %qT", method,
			    DECL_INHERITED_CTOR_BASE (method));
		  error_at (DECL_SOURCE_LOCATION (fn),
			    "conflicts with version inherited from %qT",
			    DECL_INHERITED_CTOR_BASE (fn));
		}
	      /* Otherwise defer to the other function.  */
	      return false;
	    }
	  if (using_decl)
	    {
	      if (DECL_CONTEXT (fn) == type)
		/* Defer to the local function.  */
		return false;
	    }
	  else
	    {
	      error ("%q+#D cannot be overloaded", method);
	      error ("with %q+#D", fn);
	    }

	  /* We don't call duplicate_decls here to merge the
	     declarations because that will confuse things if the
	     methods have inline definitions.  In particular, we
	     will crash while processing the definitions.  */
	  return false;
	}
    }

  /* A class should never have more than one destructor.  */
  if (current_fns && DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P (method))
    return false;

  /* Add the new binding.  */
  if (using_decl)
    {
      overload = ovl_cons (method, current_fns);
      OVL_USED (overload) = true;
    }
  else
    overload = build_overload (method, current_fns);

  if (conv_p)
    TYPE_HAS_CONVERSION (type) = 1;
  else if (slot >= CLASSTYPE_FIRST_CONVERSION_SLOT && !complete_p)
    push_class_level_binding (DECL_NAME (method), overload);

  if (insert_p)
    {
      bool reallocated;

      /* We only expect to add few methods in the COMPLETE_P case, so
	 just make room for one more method in that case.  */
      if (complete_p)
	reallocated = vec_safe_reserve_exact (method_vec, 1);
      else
	reallocated = vec_safe_reserve (method_vec, 1);
      if (reallocated)
	CLASSTYPE_METHOD_VEC (type) = method_vec;
      if (slot == method_vec->length ())
	method_vec->quick_push (overload);
      else
	method_vec->quick_insert (slot, overload);
    }
  else
    /* Replace the current slot.  */
    (*method_vec)[slot] = overload;
  return true;
}

/* Subroutines of finish_struct.  */

/* Change the access of FDECL to ACCESS in T.  Return 1 if change was
   legit, otherwise return 0.  */

static int
alter_access (tree t, tree fdecl, tree access)
{
  tree elem;

  if (!DECL_LANG_SPECIFIC (fdecl))
    retrofit_lang_decl (fdecl);

  gcc_assert (!DECL_DISCRIMINATOR_P (fdecl));

  elem = purpose_member (t, DECL_ACCESS (fdecl));
  if (elem)
    {
      if (TREE_VALUE (elem) != access)
	{
	  if (TREE_CODE (TREE_TYPE (fdecl)) == FUNCTION_DECL)
	    error ("conflicting access specifications for method"
		   " %q+D, ignored", TREE_TYPE (fdecl));
	  else
	    error ("conflicting access specifications for field %qE, ignored",
		   DECL_NAME (fdecl));
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
      perform_or_defer_access_check (TYPE_BINFO (t), fdecl, fdecl,
				     tf_warning_or_error);
      DECL_ACCESS (fdecl) = tree_cons (t, access, DECL_ACCESS (fdecl));
      return 1;
    }
  return 0;
}

/* Process the USING_DECL, which is a member of T.  */

static void
handle_using_decl (tree using_decl, tree t)
{
  tree decl = USING_DECL_DECLS (using_decl);
  tree name = DECL_NAME (using_decl);
  tree access
    = TREE_PRIVATE (using_decl) ? access_private_node
    : TREE_PROTECTED (using_decl) ? access_protected_node
    : access_public_node;
  tree flist = NULL_TREE;
  tree old_value;

  gcc_assert (!processing_template_decl && decl);

  old_value = lookup_member (t, name, /*protect=*/0, /*want_type=*/false,
			     tf_warning_or_error);
  if (old_value)
    {
      if (is_overloaded_fn (old_value))
	old_value = OVL_CURRENT (old_value);

      if (DECL_P (old_value) && DECL_CONTEXT (old_value) == t)
	/* OK */;
      else
	old_value = NULL_TREE;
    }

  cp_emit_debug_info_for_using (decl, USING_DECL_SCOPE (using_decl));

  if (is_overloaded_fn (decl))
    flist = decl;

  if (! old_value)
    ;
  else if (is_overloaded_fn (old_value))
    {
      if (flist)
	/* It's OK to use functions from a base when there are functions with
	   the same name already present in the current class.  */;
      else
	{
	  error ("%q+D invalid in %q#T", using_decl, t);
	  error ("  because of local method %q+#D with same name",
		 OVL_CURRENT (old_value));
	  return;
	}
    }
  else if (!DECL_ARTIFICIAL (old_value))
    {
      error ("%q+D invalid in %q#T", using_decl, t);
      error ("  because of local member %q+#D with same name", old_value);
      return;
    }

  /* Make type T see field decl FDECL with access ACCESS.  */
  if (flist)
    for (; flist; flist = OVL_NEXT (flist))
      {
	add_method (t, OVL_CURRENT (flist), using_decl);
	alter_access (t, OVL_CURRENT (flist), access);
      }
  else
    alter_access (t, decl, access);
}

/* walk_tree callback for check_abi_tags: if the type at *TP involves any
   types with abi tags, add the corresponding identifiers to the VEC in
   *DATA and set IDENTIFIER_MARKED.  */

struct abi_tag_data
{
  tree t;
  tree subob;
};

static tree
find_abi_tags_r (tree *tp, int */*walk_subtrees*/, void *data)
{
  if (!OVERLOAD_TYPE_P (*tp))
    return NULL_TREE;

  if (tree attributes = lookup_attribute ("abi_tag", TYPE_ATTRIBUTES (*tp)))
    {
      struct abi_tag_data *p = static_cast<struct abi_tag_data*>(data);
      for (tree list = TREE_VALUE (attributes); list;
	   list = TREE_CHAIN (list))
	{
	  tree tag = TREE_VALUE (list);
	  tree id = get_identifier (TREE_STRING_POINTER (tag));
	  if (!IDENTIFIER_MARKED (id))
	    {
	      if (TYPE_P (p->subob))
		{
		  warning (OPT_Wabi_tag, "%qT does not have the %E abi tag "
			   "that base %qT has", p->t, tag, p->subob);
		  inform (location_of (p->subob), "%qT declared here",
			  p->subob);
		}
	      else
		{
		  warning (OPT_Wabi_tag, "%qT does not have the %E abi tag "
			   "that %qT (used in the type of %qD) has",
			   p->t, tag, *tp, p->subob);
		  inform (location_of (p->subob), "%qD declared here",
			  p->subob);
		  inform (location_of (*tp), "%qT declared here", *tp);
		}
	    }
	}
    }
  return NULL_TREE;
}

/* Set IDENTIFIER_MARKED on all the ABI tags on T and its (transitively
   complete) template arguments.  */

static void
mark_type_abi_tags (tree t, bool val)
{
  tree attributes = lookup_attribute ("abi_tag", TYPE_ATTRIBUTES (t));
  if (attributes)
    {
      for (tree list = TREE_VALUE (attributes); list;
	   list = TREE_CHAIN (list))
	{
	  tree tag = TREE_VALUE (list);
	  tree id = get_identifier (TREE_STRING_POINTER (tag));
	  IDENTIFIER_MARKED (id) = val;
	}
    }

  /* Also mark ABI tags from template arguments.  */
  if (CLASSTYPE_TEMPLATE_INFO (t))
    {
      tree args = CLASSTYPE_TI_ARGS (t);
      for (int i = 0; i < TMPL_ARGS_DEPTH (args); ++i)
	{
	  tree level = TMPL_ARGS_LEVEL (args, i+1);
	  for (int j = 0; j < TREE_VEC_LENGTH (level); ++j)
	    {
	      tree arg = TREE_VEC_ELT (level, j);
	      if (CLASS_TYPE_P (arg))
		mark_type_abi_tags (arg, val);
	    }
	}
    }
}

/* Check that class T has all the abi tags that subobject SUBOB has, or
   warn if not.  */

static void
check_abi_tags (tree t, tree subob)
{
  mark_type_abi_tags (t, true);

  tree subtype = TYPE_P (subob) ? subob : TREE_TYPE (subob);
  struct abi_tag_data data = { t, subob };

  cp_walk_tree_without_duplicates (&subtype, find_abi_tags_r, &data);

  mark_type_abi_tags (t, false);
}

/* Run through the base classes of T, updating CANT_HAVE_CONST_CTOR_P,
   and NO_CONST_ASN_REF_P.  Also set flag bits in T based on
   properties of the bases.  */

static void
check_bases (tree t,
	     int* cant_have_const_ctor_p,
	     int* no_const_asn_ref_p)
{
  int i;
  bool seen_non_virtual_nearly_empty_base_p = 0;
  int seen_tm_mask = 0;
  tree base_binfo;
  tree binfo;
  tree field = NULL_TREE;

  if (!CLASSTYPE_NON_STD_LAYOUT (t))
    for (field = TYPE_FIELDS (t); field; field = DECL_CHAIN (field))
      if (TREE_CODE (field) == FIELD_DECL)
	break;

  for (binfo = TYPE_BINFO (t), i = 0;
       BINFO_BASE_ITERATE (binfo, i, base_binfo); i++)
    {
      tree basetype = TREE_TYPE (base_binfo);

      gcc_assert (COMPLETE_TYPE_P (basetype));

      if (CLASSTYPE_FINAL (basetype))
        error ("cannot derive from %<final%> base %qT in derived type %qT",
               basetype, t);

      /* If any base class is non-literal, so is the derived class.  */
      if (!CLASSTYPE_LITERAL_P (basetype))
        CLASSTYPE_LITERAL_P (t) = false;

      /* Effective C++ rule 14.  We only need to check TYPE_POLYMORPHIC_P
	 here because the case of virtual functions but non-virtual
	 dtor is handled in finish_struct_1.  */
      if (!TYPE_POLYMORPHIC_P (basetype))
	warning (OPT_Weffc__,
		 "base class %q#T has a non-virtual destructor", basetype);

      /* If the base class doesn't have copy constructors or
	 assignment operators that take const references, then the
	 derived class cannot have such a member automatically
	 generated.  */
      if (TYPE_HAS_COPY_CTOR (basetype)
	  && ! TYPE_HAS_CONST_COPY_CTOR (basetype))
	*cant_have_const_ctor_p = 1;
      if (TYPE_HAS_COPY_ASSIGN (basetype)
	  && !TYPE_HAS_CONST_COPY_ASSIGN (basetype))
	*no_const_asn_ref_p = 1;

      if (BINFO_VIRTUAL_P (base_binfo))
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
      TYPE_HAS_COMPLEX_COPY_ASSIGN (t)
	|= (TYPE_HAS_COMPLEX_COPY_ASSIGN (basetype)
	    || !TYPE_HAS_COPY_ASSIGN (basetype));
      TYPE_HAS_COMPLEX_COPY_CTOR (t) |= (TYPE_HAS_COMPLEX_COPY_CTOR (basetype)
					 || !TYPE_HAS_COPY_CTOR (basetype));
      TYPE_HAS_COMPLEX_MOVE_ASSIGN (t)
	|= TYPE_HAS_COMPLEX_MOVE_ASSIGN (basetype);
      TYPE_HAS_COMPLEX_MOVE_CTOR (t) |= TYPE_HAS_COMPLEX_MOVE_CTOR (basetype);
      TYPE_POLYMORPHIC_P (t) |= TYPE_POLYMORPHIC_P (basetype);
      CLASSTYPE_CONTAINS_EMPTY_CLASS_P (t)
	|= CLASSTYPE_CONTAINS_EMPTY_CLASS_P (basetype);
      TYPE_HAS_COMPLEX_DFLT (t) |= (!TYPE_HAS_DEFAULT_CONSTRUCTOR (basetype)
				    || TYPE_HAS_COMPLEX_DFLT (basetype));
      SET_CLASSTYPE_READONLY_FIELDS_NEED_INIT
	(t, CLASSTYPE_READONLY_FIELDS_NEED_INIT (t)
	 | CLASSTYPE_READONLY_FIELDS_NEED_INIT (basetype));
      SET_CLASSTYPE_REF_FIELDS_NEED_INIT
	(t, CLASSTYPE_REF_FIELDS_NEED_INIT (t)
	 | CLASSTYPE_REF_FIELDS_NEED_INIT (basetype));

      /*  A standard-layout class is a class that:
	  ...
	  * has no non-standard-layout base classes,  */
      CLASSTYPE_NON_STD_LAYOUT (t) |= CLASSTYPE_NON_STD_LAYOUT (basetype);
      if (!CLASSTYPE_NON_STD_LAYOUT (t))
	{
	  tree basefield;
	  /* ...has no base classes of the same type as the first non-static
	     data member...  */
	  if (field && DECL_CONTEXT (field) == t
	      && (same_type_ignoring_top_level_qualifiers_p
		  (TREE_TYPE (field), basetype)))
	    CLASSTYPE_NON_STD_LAYOUT (t) = 1;
	  else
	    /* ...either has no non-static data members in the most-derived
	       class and at most one base class with non-static data
	       members, or has no base classes with non-static data
	       members */
	    for (basefield = TYPE_FIELDS (basetype); basefield;
		 basefield = DECL_CHAIN (basefield))
	      if (TREE_CODE (basefield) == FIELD_DECL)
		{
		  if (field)
		    CLASSTYPE_NON_STD_LAYOUT (t) = 1;
		  else
		    field = basefield;
		  break;
		}
	}

      /* Don't bother collecting tm attributes if transactional memory
	 support is not enabled.  */
      if (flag_tm)
	{
	  tree tm_attr = find_tm_attribute (TYPE_ATTRIBUTES (basetype));
	  if (tm_attr)
	    seen_tm_mask |= tm_attr_to_mask (tm_attr);
	}

      check_abi_tags (t, basetype);
    }

  /* If one of the base classes had TM attributes, and the current class
     doesn't define its own, then the current class inherits one.  */
  if (seen_tm_mask && !find_tm_attribute (TYPE_ATTRIBUTES (t)))
    {
      tree tm_attr = tm_mask_to_attr (seen_tm_mask & -seen_tm_mask);
      TYPE_ATTRIBUTES (t) = tree_cons (tm_attr, NULL, TYPE_ATTRIBUTES (t));
    }
}

/* Determine all the primary bases within T.  Sets BINFO_PRIMARY_BASE_P for
   those that are primaries.  Sets BINFO_LOST_PRIMARY_P for those
   that have had a nearly-empty virtual primary base stolen by some
   other base in the hierarchy.  Determines CLASSTYPE_PRIMARY_BASE for
   T.  */

static void
determine_primary_bases (tree t)
{
  unsigned i;
  tree primary = NULL_TREE;
  tree type_binfo = TYPE_BINFO (t);
  tree base_binfo;

  /* Determine the primary bases of our bases.  */
  for (base_binfo = TREE_CHAIN (type_binfo); base_binfo;
       base_binfo = TREE_CHAIN (base_binfo))
    {
      tree primary = CLASSTYPE_PRIMARY_BINFO (BINFO_TYPE (base_binfo));

      /* See if we're the non-virtual primary of our inheritance
	 chain.  */
      if (!BINFO_VIRTUAL_P (base_binfo))
	{
	  tree parent = BINFO_INHERITANCE_CHAIN (base_binfo);
	  tree parent_primary = CLASSTYPE_PRIMARY_BINFO (BINFO_TYPE (parent));

	  if (parent_primary
	      && SAME_BINFO_TYPE_P (BINFO_TYPE (base_binfo),
				    BINFO_TYPE (parent_primary)))
	    /* We are the primary binfo.  */
	    BINFO_PRIMARY_P (base_binfo) = 1;
	}
      /* Determine if we have a virtual primary base, and mark it so.
       */
      if (primary && BINFO_VIRTUAL_P (primary))
	{
	  tree this_primary = copied_binfo (primary, base_binfo);

	  if (BINFO_PRIMARY_P (this_primary))
	    /* Someone already claimed this base.  */
	    BINFO_LOST_PRIMARY_P (base_binfo) = 1;
	  else
	    {
	      tree delta;

	      BINFO_PRIMARY_P (this_primary) = 1;
	      BINFO_INHERITANCE_CHAIN (this_primary) = base_binfo;

	      /* A virtual binfo might have been copied from within
		 another hierarchy. As we're about to use it as a
		 primary base, make sure the offsets match.  */
	      delta = size_diffop_loc (input_location,
				   convert (ssizetype,
					    BINFO_OFFSET (base_binfo)),
				   convert (ssizetype,
					    BINFO_OFFSET (this_primary)));

	      propagate_binfo_offsets (this_primary, delta);
	    }
	}
    }

  /* First look for a dynamic direct non-virtual base.  */
  for (i = 0; BINFO_BASE_ITERATE (type_binfo, i, base_binfo); i++)
    {
      tree basetype = BINFO_TYPE (base_binfo);

      if (TYPE_CONTAINS_VPTR_P (basetype) && !BINFO_VIRTUAL_P (base_binfo))
	{
	  primary = base_binfo;
	  goto found;
	}
    }

  /* A "nearly-empty" virtual base class can be the primary base
     class, if no non-virtual polymorphic base can be found.  Look for
     a nearly-empty virtual dynamic base that is not already a primary
     base of something in the hierarchy.  If there is no such base,
     just pick the first nearly-empty virtual base.  */

  for (base_binfo = TREE_CHAIN (type_binfo); base_binfo;
       base_binfo = TREE_CHAIN (base_binfo))
    if (BINFO_VIRTUAL_P (base_binfo)
	&& CLASSTYPE_NEARLY_EMPTY_P (BINFO_TYPE (base_binfo)))
      {
	if (!BINFO_PRIMARY_P (base_binfo))
	  {
	    /* Found one that is not primary.  */
	    primary = base_binfo;
	    goto found;
	  }
	else if (!primary)
	  /* Remember the first candidate.  */
	  primary = base_binfo;
      }

 found:
  /* If we've got a primary base, use it.  */
  if (primary)
    {
      tree basetype = BINFO_TYPE (primary);

      CLASSTYPE_PRIMARY_BINFO (t) = primary;
      if (BINFO_PRIMARY_P (primary))
	/* We are stealing a primary base.  */
	BINFO_LOST_PRIMARY_P (BINFO_INHERITANCE_CHAIN (primary)) = 1;
      BINFO_PRIMARY_P (primary) = 1;
      if (BINFO_VIRTUAL_P (primary))
	{
	  tree delta;

	  BINFO_INHERITANCE_CHAIN (primary) = type_binfo;
	  /* A virtual binfo might have been copied from within
	     another hierarchy. As we're about to use it as a primary
	     base, make sure the offsets match.  */
	  delta = size_diffop_loc (input_location, ssize_int (0),
			       convert (ssizetype, BINFO_OFFSET (primary)));

	  propagate_binfo_offsets (primary, delta);
	}

      primary = TYPE_BINFO (basetype);

      TYPE_VFIELD (t) = TYPE_VFIELD (basetype);
      BINFO_VTABLE (type_binfo) = BINFO_VTABLE (primary);
      BINFO_VIRTUALS (type_binfo) = BINFO_VIRTUALS (primary);
    }
}

/* Update the variant types of T.  */

void
fixup_type_variants (tree t)
{
  tree variants;

  if (!t)
    return;

  for (variants = TYPE_NEXT_VARIANT (t);
       variants;
       variants = TYPE_NEXT_VARIANT (variants))
    {
      /* These fields are in the _TYPE part of the node, not in
	 the TYPE_LANG_SPECIFIC component, so they are not shared.  */
      TYPE_HAS_USER_CONSTRUCTOR (variants) = TYPE_HAS_USER_CONSTRUCTOR (t);
      TYPE_NEEDS_CONSTRUCTING (variants) = TYPE_NEEDS_CONSTRUCTING (t);
      TYPE_HAS_NONTRIVIAL_DESTRUCTOR (variants)
	= TYPE_HAS_NONTRIVIAL_DESTRUCTOR (t);

      TYPE_POLYMORPHIC_P (variants) = TYPE_POLYMORPHIC_P (t);

      TYPE_BINFO (variants) = TYPE_BINFO (t);

      /* Copy whatever these are holding today.  */
      TYPE_VFIELD (variants) = TYPE_VFIELD (t);
      TYPE_METHODS (variants) = TYPE_METHODS (t);
      TYPE_FIELDS (variants) = TYPE_FIELDS (t);
    }
}

/* Early variant fixups: we apply attributes at the beginning of the class
   definition, and we need to fix up any variants that have already been
   made via elaborated-type-specifier so that check_qualified_type works.  */

void
fixup_attribute_variants (tree t)
{
  tree variants;

  if (!t)
    return;

  for (variants = TYPE_NEXT_VARIANT (t);
       variants;
       variants = TYPE_NEXT_VARIANT (variants))
    {
      /* These are the two fields that check_qualified_type looks at and
	 are affected by attributes.  */
      TYPE_ATTRIBUTES (variants) = TYPE_ATTRIBUTES (t);
      TYPE_ALIGN (variants) = TYPE_ALIGN (t);
    }
}

/* Set memoizing fields and bits of T (and its variants) for later
   use.  */

static void
finish_struct_bits (tree t)
{
  /* Fix up variants (if any).  */
  fixup_type_variants (t);

  if (BINFO_N_BASE_BINFOS (TYPE_BINFO (t)) && TYPE_POLYMORPHIC_P (t))
    /* For a class w/o baseclasses, 'finish_struct' has set
       CLASSTYPE_PURE_VIRTUALS correctly (by definition).
       Similarly for a class whose base classes do not have vtables.
       When neither of these is true, we might have removed abstract
       virtuals (by providing a definition), added some (by declaring
       new ones), or redeclared ones from a base class.  We need to
       recalculate what's really an abstract virtual at this point (by
       looking in the vtables).  */
    get_pure_virtuals (t);

  /* If this type has a copy constructor or a destructor, force its
     mode to be BLKmode, and force its TREE_ADDRESSABLE bit to be
     nonzero.  This will cause it to be passed by invisible reference
     and prevent it from being returned in a register.  */
  if (type_has_nontrivial_copy_init (t)
      || TYPE_HAS_NONTRIVIAL_DESTRUCTOR (t))
    {
      tree variants;
      DECL_MODE (TYPE_MAIN_DECL (t)) = BLKmode;
      for (variants = t; variants; variants = TYPE_NEXT_VARIANT (variants))
	{
	  SET_TYPE_MODE (variants, BLKmode);
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
maybe_warn_about_overly_private_class (tree t)
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
  for (fn = TYPE_METHODS (t); fn; fn = DECL_CHAIN (fn))
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
      unsigned i;
      tree binfo = TYPE_BINFO (t);

      for (i = 0; i != BINFO_N_BASE_BINFOS (binfo); i++)
	if (BINFO_BASE_ACCESS (binfo, i) != access_private_node)
	  {
	    has_nonprivate_method = 1;
	    break;
	  }
      if (!has_nonprivate_method)
	{
	  warning (OPT_Wctor_dtor_privacy,
		   "all member functions in class %qT are private", t);
	  return;
	}
    }

  /* Even if some of the member functions are non-private, the class
     won't be useful for much if all the constructors or destructors
     are private: such an object can never be created or destroyed.  */
  fn = CLASSTYPE_DESTRUCTORS (t);
  if (fn && TREE_PRIVATE (fn))
    {
      warning (OPT_Wctor_dtor_privacy,
	       "%q#T only defines a private destructor and has no friends",
	       t);
      return;
    }

  /* Warn about classes that have private constructors and no friends.  */
  if (TYPE_HAS_USER_CONSTRUCTOR (t)
      /* Implicitly generated constructors are always public.  */
      && (!CLASSTYPE_LAZY_DEFAULT_CTOR (t)
	  || !CLASSTYPE_LAZY_COPY_CTOR (t)))
    {
      int nonprivate_ctor = 0;

      /* If a non-template class does not define a copy
	 constructor, one is defined for it, enabling it to avoid
	 this warning.  For a template class, this does not
	 happen, and so we would normally get a warning on:

	   template <class T> class C { private: C(); };

	 To avoid this asymmetry, we check TYPE_HAS_COPY_CTOR.  All
	 complete non-template or fully instantiated classes have this
	 flag set.  */
      if (!TYPE_HAS_COPY_CTOR (t))
	nonprivate_ctor = 1;
      else
	for (fn = CLASSTYPE_CONSTRUCTORS (t); fn; fn = OVL_NEXT (fn))
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
	  warning (OPT_Wctor_dtor_privacy,
		   "%q#T only defines private constructors and has no friends",
		   t);
	  return;
	}
    }
}

static struct {
  gt_pointer_operator new_value;
  void *cookie;
} resort_data;

/* Comparison function to compare two TYPE_METHOD_VEC entries by name.  */

static int
method_name_cmp (const void* m1_p, const void* m2_p)
{
  const tree *const m1 = (const tree *) m1_p;
  const tree *const m2 = (const tree *) m2_p;

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

/* This routine compares two fields like method_name_cmp but using the
   pointer operator in resort_field_decl_data.  */

static int
resort_method_name_cmp (const void* m1_p, const void* m2_p)
{
  const tree *const m1 = (const tree *) m1_p;
  const tree *const m2 = (const tree *) m2_p;
  if (*m1 == NULL_TREE && *m2 == NULL_TREE)
    return 0;
  if (*m1 == NULL_TREE)
    return -1;
  if (*m2 == NULL_TREE)
    return 1;
  {
    tree d1 = DECL_NAME (OVL_CURRENT (*m1));
    tree d2 = DECL_NAME (OVL_CURRENT (*m2));
    resort_data.new_value (&d1, resort_data.cookie);
    resort_data.new_value (&d2, resort_data.cookie);
    if (d1 < d2)
      return -1;
  }
  return 1;
}

/* Resort TYPE_METHOD_VEC because pointers have been reordered.  */

void
resort_type_method_vec (void* obj,
			void* /*orig_obj*/,
			gt_pointer_operator new_value,
			void* cookie)
{
  vec<tree, va_gc> *method_vec = (vec<tree, va_gc> *) obj;
  int len = vec_safe_length (method_vec);
  size_t slot;
  tree fn;

  /* The type conversion ops have to live at the front of the vec, so we
     can't sort them.  */
  for (slot = CLASSTYPE_FIRST_CONVERSION_SLOT;
       vec_safe_iterate (method_vec, slot, &fn);
       ++slot)
    if (!DECL_CONV_FN_P (OVL_CURRENT (fn)))
      break;

  if (len - slot > 1)
    {
      resort_data.new_value = new_value;
      resort_data.cookie = cookie;
      qsort (method_vec->address () + slot, len - slot, sizeof (tree),
	     resort_method_name_cmp);
    }
}

/* Warn about duplicate methods in fn_fields.

   Sort methods that are not special (i.e., constructors, destructors,
   and type conversion operators) so that we can find them faster in
   search.  */

static void
finish_struct_methods (tree t)
{
  tree fn_fields;
  vec<tree, va_gc> *method_vec;
  int slot, len;

  method_vec = CLASSTYPE_METHOD_VEC (t);
  if (!method_vec)
    return;

  len = method_vec->length ();

  /* Clear DECL_IN_AGGR_P for all functions.  */
  for (fn_fields = TYPE_METHODS (t); fn_fields;
       fn_fields = DECL_CHAIN (fn_fields))
    DECL_IN_AGGR_P (fn_fields) = 0;

  /* Issue warnings about private constructors and such.  If there are
     no methods, then some public defaults are generated.  */
  maybe_warn_about_overly_private_class (t);

  /* The type conversion ops have to live at the front of the vec, so we
     can't sort them.  */
  for (slot = CLASSTYPE_FIRST_CONVERSION_SLOT;
       method_vec->iterate (slot, &fn_fields);
       ++slot)
    if (!DECL_CONV_FN_P (OVL_CURRENT (fn_fields)))
      break;
  if (len - slot > 1)
    qsort (method_vec->address () + slot,
	   len-slot, sizeof (tree), method_name_cmp);
}

/* Make BINFO's vtable have N entries, including RTTI entries,
   vbase and vcall offsets, etc.  Set its type and call the back end
   to lay it out.  */

static void
layout_vtable_decl (tree binfo, int n)
{
  tree atype;
  tree vtable;

  atype = build_array_of_n_type (vtable_entry_type, n);
  layout_type (atype);

  /* We may have to grow the vtable.  */
  vtable = get_vtbl_decl_for_binfo (binfo);
  if (!same_type_p (TREE_TYPE (vtable), atype))
    {
      TREE_TYPE (vtable) = atype;
      DECL_SIZE (vtable) = DECL_SIZE_UNIT (vtable) = NULL_TREE;
      layout_decl (vtable, 0);
    }
}

/* True iff FNDECL and BASE_FNDECL (both non-static member functions)
   have the same signature.  */

int
same_signature_p (const_tree fndecl, const_tree base_fndecl)
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

  if (DECL_NAME (fndecl) == DECL_NAME (base_fndecl)
      || (DECL_CONV_FN_P (fndecl)
	  && DECL_CONV_FN_P (base_fndecl)
	  && same_type_p (DECL_CONV_FN_TYPE (fndecl),
			  DECL_CONV_FN_TYPE (base_fndecl))))
    {
      tree fntype = TREE_TYPE (fndecl);
      tree base_fntype = TREE_TYPE (base_fndecl);
      if (type_memfn_quals (fntype) == type_memfn_quals (base_fntype)
	  && type_memfn_rqual (fntype) == type_memfn_rqual (base_fntype)
	  && compparms (FUNCTION_FIRST_USER_PARMTYPE (fndecl),
			FUNCTION_FIRST_USER_PARMTYPE (base_fndecl)))
	return 1;
    }
  return 0;
}

/* Returns TRUE if DERIVED is a binfo containing the binfo BASE as a
   subobject.  */

static bool
base_derived_from (tree derived, tree base)
{
  tree probe;

  for (probe = base; probe; probe = BINFO_INHERITANCE_CHAIN (probe))
    {
      if (probe == derived)
	return true;
      else if (BINFO_VIRTUAL_P (probe))
	/* If we meet a virtual base, we can't follow the inheritance
	   any more.  See if the complete type of DERIVED contains
	   such a virtual base.  */
	return (binfo_for_vbase (BINFO_TYPE (probe), BINFO_TYPE (derived))
		!= NULL_TREE);
    }
  return false;
}

typedef struct find_final_overrider_data_s {
  /* The function for which we are trying to find a final overrider.  */
  tree fn;
  /* The base class in which the function was declared.  */
  tree declaring_base;
  /* The candidate overriders.  */
  tree candidates;
  /* Path to most derived.  */
  vec<tree> path;
} find_final_overrider_data;

/* Add the overrider along the current path to FFOD->CANDIDATES.
   Returns true if an overrider was found; false otherwise.  */

static bool
dfs_find_final_overrider_1 (tree binfo,
			    find_final_overrider_data *ffod,
			    unsigned depth)
{
  tree method;

  /* If BINFO is not the most derived type, try a more derived class.
     A definition there will overrider a definition here.  */
  if (depth)
    {
      depth--;
      if (dfs_find_final_overrider_1
	  (ffod->path[depth], ffod, depth))
	return true;
    }

  method = look_for_overrides_here (BINFO_TYPE (binfo), ffod->fn);
  if (method)
    {
      tree *candidate = &ffod->candidates;

      /* Remove any candidates overridden by this new function.  */
      while (*candidate)
	{
	  /* If *CANDIDATE overrides METHOD, then METHOD
	     cannot override anything else on the list.  */
	  if (base_derived_from (TREE_VALUE (*candidate), binfo))
	    return true;
	  /* If METHOD overrides *CANDIDATE, remove *CANDIDATE.  */
	  if (base_derived_from (binfo, TREE_VALUE (*candidate)))
	    *candidate = TREE_CHAIN (*candidate);
	  else
	    candidate = &TREE_CHAIN (*candidate);
	}

      /* Add the new function.  */
      ffod->candidates = tree_cons (method, binfo, ffod->candidates);
      return true;
    }

  return false;
}

/* Called from find_final_overrider via dfs_walk.  */

static tree
dfs_find_final_overrider_pre (tree binfo, void *data)
{
  find_final_overrider_data *ffod = (find_final_overrider_data *) data;

  if (binfo == ffod->declaring_base)
    dfs_find_final_overrider_1 (binfo, ffod, ffod->path.length ());
  ffod->path.safe_push (binfo);

  return NULL_TREE;
}

static tree
dfs_find_final_overrider_post (tree /*binfo*/, void *data)
{
  find_final_overrider_data *ffod = (find_final_overrider_data *) data;
  ffod->path.pop ();

  return NULL_TREE;
}

/* Returns a TREE_LIST whose TREE_PURPOSE is the final overrider for
   FN and whose TREE_VALUE is the binfo for the base where the
   overriding occurs.  BINFO (in the hierarchy dominated by the binfo
   DERIVED) is the base object in which FN is declared.  */

static tree
find_final_overrider (tree derived, tree binfo, tree fn)
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
  if (DECL_THUNK_P (fn))
    fn = THUNK_TARGET (fn);

  /* Determine the depth of the hierarchy.  */
  ffod.fn = fn;
  ffod.declaring_base = binfo;
  ffod.candidates = NULL_TREE;
  ffod.path.create (30);

  dfs_walk_all (derived, dfs_find_final_overrider_pre,
		dfs_find_final_overrider_post, &ffod);

  ffod.path.release ();

  /* If there was no winner, issue an error message.  */
  if (!ffod.candidates || TREE_CHAIN (ffod.candidates))
    return error_mark_node;

  return ffod.candidates;
}

/* Return the index of the vcall offset for FN when TYPE is used as a
   virtual base.  */

static tree
get_vcall_index (tree fn, tree type)
{
  vec<tree_pair_s, va_gc> *indices = CLASSTYPE_VCALL_INDICES (type);
  tree_pair_p p;
  unsigned ix;

  FOR_EACH_VEC_SAFE_ELT (indices, ix, p)
    if ((DECL_DESTRUCTOR_P (fn) && DECL_DESTRUCTOR_P (p->purpose))
	|| same_signature_p (fn, p->purpose))
      return p->value;

  /* There should always be an appropriate index.  */
  gcc_unreachable ();
}

/* Update an entry in the vtable for BINFO, which is in the hierarchy
   dominated by T.  FN is the old function; VIRTUALS points to the
   corresponding position in the new BINFO_VIRTUALS list.  IX is the index
   of that entry in the list.  */

static void
update_vtable_entry_for_fn (tree t, tree binfo, tree fn, tree* virtuals,
			    unsigned ix)
{
  tree b;
  tree overrider;
  tree delta;
  tree virtual_base;
  tree first_defn;
  tree overrider_fn, overrider_target;
  tree target_fn = DECL_THUNK_P (fn) ? THUNK_TARGET (fn) : fn;
  tree over_return, base_return;
  bool lost = false;

  /* Find the nearest primary base (possibly binfo itself) which defines
     this function; this is the class the caller will convert to when
     calling FN through BINFO.  */
  for (b = binfo; ; b = get_primary_binfo (b))
    {
      gcc_assert (b);
      if (look_for_overrides_here (BINFO_TYPE (b), target_fn))
	break;

      /* The nearest definition is from a lost primary.  */
      if (BINFO_LOST_PRIMARY_P (b))
	lost = true;
    }
  first_defn = b;

  /* Find the final overrider.  */
  overrider = find_final_overrider (TYPE_BINFO (t), b, target_fn);
  if (overrider == error_mark_node)
    {
      error ("no unique final overrider for %qD in %qT", target_fn, t);
      return;
    }
  overrider_target = overrider_fn = TREE_PURPOSE (overrider);

  /* Check for adjusting covariant return types.  */
  over_return = TREE_TYPE (TREE_TYPE (overrider_target));
  base_return = TREE_TYPE (TREE_TYPE (target_fn));

  if (POINTER_TYPE_P (over_return)
      && TREE_CODE (over_return) == TREE_CODE (base_return)
      && CLASS_TYPE_P (TREE_TYPE (over_return))
      && CLASS_TYPE_P (TREE_TYPE (base_return))
      /* If the overrider is invalid, don't even try.  */
      && !DECL_INVALID_OVERRIDER_P (overrider_target))
    {
      /* If FN is a covariant thunk, we must figure out the adjustment
	 to the final base FN was converting to. As OVERRIDER_TARGET might
	 also be converting to the return type of FN, we have to
	 combine the two conversions here.  */
      tree fixed_offset, virtual_offset;

      over_return = TREE_TYPE (over_return);
      base_return = TREE_TYPE (base_return);

      if (DECL_THUNK_P (fn))
	{
	  gcc_assert (DECL_RESULT_THUNK_P (fn));
	  fixed_offset = ssize_int (THUNK_FIXED_OFFSET (fn));
	  virtual_offset = THUNK_VIRTUAL_OFFSET (fn);
	}
      else
	fixed_offset = virtual_offset = NULL_TREE;

      if (virtual_offset)
	/* Find the equivalent binfo within the return type of the
	   overriding function. We will want the vbase offset from
	   there.  */
	virtual_offset = binfo_for_vbase (BINFO_TYPE (virtual_offset),
					  over_return);
      else if (!same_type_ignoring_top_level_qualifiers_p
	       (over_return, base_return))
	{
	  /* There was no existing virtual thunk (which takes
	     precedence).  So find the binfo of the base function's
	     return type within the overriding function's return type.
	     We cannot call lookup base here, because we're inside a
	     dfs_walk, and will therefore clobber the BINFO_MARKED
	     flags.  Fortunately we know the covariancy is valid (it
	     has already been checked), so we can just iterate along
	     the binfos, which have been chained in inheritance graph
	     order.  Of course it is lame that we have to repeat the
	     search here anyway -- we should really be caching pieces
	     of the vtable and avoiding this repeated work.  */
	  tree thunk_binfo, base_binfo;

	  /* Find the base binfo within the overriding function's
	     return type.  We will always find a thunk_binfo, except
	     when the covariancy is invalid (which we will have
	     already diagnosed).  */
	  for (base_binfo = TYPE_BINFO (base_return),
	       thunk_binfo = TYPE_BINFO (over_return);
	       thunk_binfo;
	       thunk_binfo = TREE_CHAIN (thunk_binfo))
	    if (SAME_BINFO_TYPE_P (BINFO_TYPE (thunk_binfo),
				   BINFO_TYPE (base_binfo)))
	      break;

	  /* See if virtual inheritance is involved.  */
	  for (virtual_offset = thunk_binfo;
	       virtual_offset;
	       virtual_offset = BINFO_INHERITANCE_CHAIN (virtual_offset))
	    if (BINFO_VIRTUAL_P (virtual_offset))
	      break;

	  if (virtual_offset
	      || (thunk_binfo && !BINFO_OFFSET_ZEROP (thunk_binfo)))
	    {
	      tree offset = convert (ssizetype, BINFO_OFFSET (thunk_binfo));

	      if (virtual_offset)
		{
		  /* We convert via virtual base.  Adjust the fixed
		     offset to be from there.  */
		  offset = 
		    size_diffop (offset,
				 convert (ssizetype,
					  BINFO_OFFSET (virtual_offset)));
		}
	      if (fixed_offset)
		/* There was an existing fixed offset, this must be
		   from the base just converted to, and the base the
		   FN was thunking to.  */
		fixed_offset = size_binop (PLUS_EXPR, fixed_offset, offset);
	      else
		fixed_offset = offset;
	    }
	}

      if (fixed_offset || virtual_offset)
	/* Replace the overriding function with a covariant thunk.  We
	   will emit the overriding function in its own slot as
	   well.  */
	overrider_fn = make_thunk (overrider_target, /*this_adjusting=*/0,
				   fixed_offset, virtual_offset);
    }
  else
    gcc_assert (DECL_INVALID_OVERRIDER_P (overrider_target) ||
		!DECL_THUNK_P (fn));

  /* If we need a covariant thunk, then we may need to adjust first_defn.
     The ABI specifies that the thunks emitted with a function are
     determined by which bases the function overrides, so we need to be
     sure that we're using a thunk for some overridden base; even if we
     know that the necessary this adjustment is zero, there may not be an
     appropriate zero-this-adjusment thunk for us to use since thunks for
     overriding virtual bases always use the vcall offset.

     Furthermore, just choosing any base that overrides this function isn't
     quite right, as this slot won't be used for calls through a type that
     puts a covariant thunk here.  Calling the function through such a type
     will use a different slot, and that slot is the one that determines
     the thunk emitted for that base.

     So, keep looking until we find the base that we're really overriding
     in this slot: the nearest primary base that doesn't use a covariant
     thunk in this slot.  */
  if (overrider_target != overrider_fn)
    {
      if (BINFO_TYPE (b) == DECL_CONTEXT (overrider_target))
	/* We already know that the overrider needs a covariant thunk.  */
	b = get_primary_binfo (b);
      for (; ; b = get_primary_binfo (b))
	{
	  tree main_binfo = TYPE_BINFO (BINFO_TYPE (b));
	  tree bv = chain_index (ix, BINFO_VIRTUALS (main_binfo));
	  if (!DECL_THUNK_P (TREE_VALUE (bv)))
	    break;
	  if (BINFO_LOST_PRIMARY_P (b))
	    lost = true;
	}
      first_defn = b;
    }

  /* Assume that we will produce a thunk that convert all the way to
     the final overrider, and not to an intermediate virtual base.  */
  virtual_base = NULL_TREE;

  /* See if we can convert to an intermediate virtual base first, and then
     use the vcall offset located there to finish the conversion.  */
  for (; b; b = BINFO_INHERITANCE_CHAIN (b))
    {
      /* If we find the final overrider, then we can stop
	 walking.  */
      if (SAME_BINFO_TYPE_P (BINFO_TYPE (b),
			     BINFO_TYPE (TREE_VALUE (overrider))))
	break;

      /* If we find a virtual base, and we haven't yet found the
	 overrider, then there is a virtual base between the
	 declaring base (first_defn) and the final overrider.  */
      if (BINFO_VIRTUAL_P (b))
	{
	  virtual_base = b;
	  break;
	}
    }

  /* Compute the constant adjustment to the `this' pointer.  The
     `this' pointer, when this function is called, will point at BINFO
     (or one of its primary bases, which are at the same offset).  */
  if (virtual_base)
    /* The `this' pointer needs to be adjusted from the declaration to
       the nearest virtual base.  */
    delta = size_diffop_loc (input_location,
			 convert (ssizetype, BINFO_OFFSET (virtual_base)),
			 convert (ssizetype, BINFO_OFFSET (first_defn)));
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
    delta = size_diffop_loc (input_location,
			 convert (ssizetype,
				  BINFO_OFFSET (TREE_VALUE (overrider))),
			 convert (ssizetype, BINFO_OFFSET (binfo)));

  modify_vtable_entry (t, binfo, overrider_fn, delta, virtuals);

  if (virtual_base)
    BV_VCALL_INDEX (*virtuals)
      = get_vcall_index (overrider_target, BINFO_TYPE (virtual_base));
  else
    BV_VCALL_INDEX (*virtuals) = NULL_TREE;

  BV_LOST_PRIMARY (*virtuals) = lost;
}

/* Called from modify_all_vtables via dfs_walk.  */

static tree
dfs_modify_vtables (tree binfo, void* data)
{
  tree t = (tree) data;
  tree virtuals;
  tree old_virtuals;
  unsigned ix;

  if (!TYPE_CONTAINS_VPTR_P (BINFO_TYPE (binfo)))
    /* A base without a vtable needs no modification, and its bases
       are uninteresting.  */
    return dfs_skip_bases;

  if (SAME_BINFO_TYPE_P (BINFO_TYPE (binfo), t)
      && !CLASSTYPE_HAS_PRIMARY_BASE_P (t))
    /* Don't do the primary vtable, if it's new.  */
    return NULL_TREE;

  if (BINFO_PRIMARY_P (binfo) && !BINFO_VIRTUAL_P (binfo))
    /* There's no need to modify the vtable for a non-virtual primary
       base; we're not going to use that vtable anyhow.  We do still
       need to do this for virtual primary bases, as they could become
       non-primary in a construction vtable.  */
    return NULL_TREE;

  make_new_vtable (t, binfo);

  /* Now, go through each of the virtual functions in the virtual
     function table for BINFO.  Find the final overrider, and update
     the BINFO_VIRTUALS list appropriately.  */
  for (ix = 0, virtuals = BINFO_VIRTUALS (binfo),
	 old_virtuals = BINFO_VIRTUALS (TYPE_BINFO (BINFO_TYPE (binfo)));
       virtuals;
       ix++, virtuals = TREE_CHAIN (virtuals),
	 old_virtuals = TREE_CHAIN (old_virtuals))
    update_vtable_entry_for_fn (t,
				binfo,
				BV_FN (old_virtuals),
				&virtuals, ix);

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
modify_all_vtables (tree t, tree virtuals)
{
  tree binfo = TYPE_BINFO (t);
  tree *fnsp;

  /* Mangle the vtable name before entering dfs_walk (c++/51884).  */
  if (TYPE_CONTAINS_VPTR_P (t))
    get_vtable_decl (t, false);

  /* Update all of the vtables.  */
  dfs_walk_once (binfo, dfs_modify_vtables, NULL, t);

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
get_basefndecls (tree name, tree t)
{
  tree methods;
  tree base_fndecls = NULL_TREE;
  int n_baseclasses = BINFO_N_BASE_BINFOS (TYPE_BINFO (t));
  int i;

  /* Find virtual functions in T with the indicated NAME.  */
  i = lookup_fnfields_1 (t, name);
  if (i != -1)
    for (methods = (*CLASSTYPE_METHOD_VEC (t))[i];
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
      tree basetype = BINFO_TYPE (BINFO_BASE_BINFO (TYPE_BINFO (t), i));
      base_fndecls = chainon (get_basefndecls (name, basetype),
			      base_fndecls);
    }

  return base_fndecls;
}

/* If this declaration supersedes the declaration of
   a method declared virtual in the base class, then
   mark this field as being virtual as well.  */

void
check_for_override (tree decl, tree ctype)
{
  bool overrides_found = false;
  if (TREE_CODE (decl) == TEMPLATE_DECL)
    /* In [temp.mem] we have:

	 A specialization of a member function template does not
	 override a virtual function from a base class.  */
    return;
  if ((DECL_DESTRUCTOR_P (decl)
       || IDENTIFIER_VIRTUAL_P (DECL_NAME (decl))
       || DECL_CONV_FN_P (decl))
      && look_for_overrides (ctype, decl)
      && !DECL_STATIC_FUNCTION_P (decl))
    /* Set DECL_VINDEX to a value that is neither an INTEGER_CST nor
       the error_mark_node so that we know it is an overriding
       function.  */
    {
      DECL_VINDEX (decl) = decl;
      overrides_found = true;
    }

  if (DECL_VIRTUAL_P (decl))
    {
      if (!DECL_VINDEX (decl))
	DECL_VINDEX (decl) = error_mark_node;
      IDENTIFIER_VIRTUAL_P (DECL_NAME (decl)) = 1;
      if (DECL_DESTRUCTOR_P (decl))
	TYPE_HAS_NONTRIVIAL_DESTRUCTOR (ctype) = true;
    }
  else if (DECL_FINAL_P (decl))
    error ("%q+#D marked final, but is not virtual", decl);
  if (DECL_OVERRIDE_P (decl) && !overrides_found)
    error ("%q+#D marked override, but does not override", decl);
}

/* Warn about hidden virtual functions that are not overridden in t.
   We know that constructors and destructors don't apply.  */

static void
warn_hidden (tree t)
{
  vec<tree, va_gc> *method_vec = CLASSTYPE_METHOD_VEC (t);
  tree fns;
  size_t i;

  /* We go through each separately named virtual function.  */
  for (i = CLASSTYPE_FIRST_CONVERSION_SLOT;
       vec_safe_iterate (method_vec, i, &fns);
       ++i)
    {
      tree fn;
      tree name;
      tree fndecl;
      tree base_fndecls;
      tree base_binfo;
      tree binfo;
      int j;

      /* All functions in this slot in the CLASSTYPE_METHOD_VEC will
	 have the same name.  Figure out what name that is.  */
      name = DECL_NAME (OVL_CURRENT (fns));
      /* There are no possibly hidden functions yet.  */
      base_fndecls = NULL_TREE;
      /* Iterate through all of the base classes looking for possibly
	 hidden functions.  */
      for (binfo = TYPE_BINFO (t), j = 0;
	   BINFO_BASE_ITERATE (binfo, j, base_binfo); j++)
	{
	  tree basetype = BINFO_TYPE (base_binfo);
	  base_fndecls = chainon (get_basefndecls (name, basetype),
				  base_fndecls);
	}

      /* If there are no functions to hide, continue.  */
      if (!base_fndecls)
	continue;

      /* Remove any overridden functions.  */
      for (fn = fns; fn; fn = OVL_NEXT (fn))
	{
	  fndecl = OVL_CURRENT (fn);
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
	  warning (OPT_Woverloaded_virtual, "%q+D was hidden", TREE_VALUE (base_fndecls));
	  warning (OPT_Woverloaded_virtual, "  by %q+D", fns);
	  base_fndecls = TREE_CHAIN (base_fndecls);
	}
    }
}

/* Recursive helper for finish_struct_anon.  */

static void
finish_struct_anon_r (tree field, bool complain)
{
  bool is_union = TREE_CODE (TREE_TYPE (field)) == UNION_TYPE;
  tree elt = TYPE_FIELDS (TREE_TYPE (field));
  for (; elt; elt = DECL_CHAIN (elt))
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

      if (TREE_CODE (elt) != FIELD_DECL)
	{
	  if (complain)
	    {
	      if (is_union)
		permerror (input_location,
			   "%q+#D invalid; an anonymous union can "
			   "only have non-static data members", elt);
	      else
		permerror (input_location,
			   "%q+#D invalid; an anonymous struct can "
			   "only have non-static data members", elt);
	    }
	  continue;
	}

      if (complain)
	{
	  if (TREE_PRIVATE (elt))
	    {
	      if (is_union)
		permerror (input_location,
			   "private member %q+#D in anonymous union", elt);
	      else
		permerror (input_location,
			   "private member %q+#D in anonymous struct", elt);
	    }
	  else if (TREE_PROTECTED (elt))
	    {
	      if (is_union)
		permerror (input_location,
			   "protected member %q+#D in anonymous union", elt);
	      else
		permerror (input_location,
			   "protected member %q+#D in anonymous struct", elt);
	    }
	}

      TREE_PRIVATE (elt) = TREE_PRIVATE (field);
      TREE_PROTECTED (elt) = TREE_PROTECTED (field);

      /* Recurse into the anonymous aggregates to handle correctly
	 access control (c++/24926):

	 class A {
	   union {
	     union {
	       int i;
	     };
	   };
	 };

	 int j=A().i;  */
      if (DECL_NAME (elt) == NULL_TREE
	  && ANON_AGGR_TYPE_P (TREE_TYPE (elt)))
	finish_struct_anon_r (elt, /*complain=*/false);
    }
}

/* Check for things that are invalid.  There are probably plenty of other
   things we should check for also.  */

static void
finish_struct_anon (tree t)
{
  for (tree field = TYPE_FIELDS (t); field; field = DECL_CHAIN (field))
    {
      if (TREE_STATIC (field))
	continue;
      if (TREE_CODE (field) != FIELD_DECL)
	continue;

      if (DECL_NAME (field) == NULL_TREE
	  && ANON_AGGR_TYPE_P (TREE_TYPE (field)))
	finish_struct_anon_r (field, /*complain=*/true);
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
maybe_add_class_template_decl_list (tree type, tree t, int friend_p)
{
  /* Save some memory by not creating TREE_LIST if TYPE is not template.  */
  if (CLASSTYPE_TEMPLATE_INFO (type))
    CLASSTYPE_DECL_LIST (type)
      = tree_cons (friend_p ? NULL_TREE : type,
		   t, CLASSTYPE_DECL_LIST (type));
}

/* This function is called from declare_virt_assop_and_dtor via
   dfs_walk_all.

   DATA is a type that direcly or indirectly inherits the base
   represented by BINFO.  If BINFO contains a virtual assignment [copy
   assignment or move assigment] operator or a virtual constructor,
   declare that function in DATA if it hasn't been already declared.  */

static tree
dfs_declare_virt_assop_and_dtor (tree binfo, void *data)
{
  tree bv, fn, t = (tree)data;
  tree opname = ansi_assopname (NOP_EXPR);

  gcc_assert (t && CLASS_TYPE_P (t));
  gcc_assert (binfo && TREE_CODE (binfo) == TREE_BINFO);

  if (!TYPE_CONTAINS_VPTR_P (BINFO_TYPE (binfo)))
    /* A base without a vtable needs no modification, and its bases
       are uninteresting.  */
    return dfs_skip_bases;

  if (BINFO_PRIMARY_P (binfo))
    /* If this is a primary base, then we have already looked at the
       virtual functions of its vtable.  */
    return NULL_TREE;

  for (bv = BINFO_VIRTUALS (binfo); bv; bv = TREE_CHAIN (bv))
    {
      fn = BV_FN (bv);

      if (DECL_NAME (fn) == opname)
	{
	  if (CLASSTYPE_LAZY_COPY_ASSIGN (t))
	    lazily_declare_fn (sfk_copy_assignment, t);
	  if (CLASSTYPE_LAZY_MOVE_ASSIGN (t))
	    lazily_declare_fn (sfk_move_assignment, t);
	}
      else if (DECL_DESTRUCTOR_P (fn)
	       && CLASSTYPE_LAZY_DESTRUCTOR (t))
	lazily_declare_fn (sfk_destructor, t);
    }

  return NULL_TREE;
}

/* If the class type T has a direct or indirect base that contains a
   virtual assignment operator or a virtual destructor, declare that
   function in T if it hasn't been already declared.  */

static void
declare_virt_assop_and_dtor (tree t)
{
  if (!(TYPE_POLYMORPHIC_P (t)
	&& (CLASSTYPE_LAZY_COPY_ASSIGN (t)
	    || CLASSTYPE_LAZY_MOVE_ASSIGN (t)
	    || CLASSTYPE_LAZY_DESTRUCTOR (t))))
    return;

  dfs_walk_all (TYPE_BINFO (t),
		dfs_declare_virt_assop_and_dtor,
		NULL, t);
}

/* Declare the inheriting constructor for class T inherited from base
   constructor CTOR with the parameter array PARMS of size NPARMS.  */

static void
one_inheriting_sig (tree t, tree ctor, tree *parms, int nparms)
{
  /* We don't declare an inheriting ctor that would be a default,
     copy or move ctor for derived or base.  */
  if (nparms == 0)
    return;
  if (nparms == 1
      && TREE_CODE (parms[0]) == REFERENCE_TYPE)
    {
      tree parm = TYPE_MAIN_VARIANT (TREE_TYPE (parms[0]));
      if (parm == t || parm == DECL_CONTEXT (ctor))
	return;
    }

  tree parmlist = void_list_node;
  for (int i = nparms - 1; i >= 0; i--)
    parmlist = tree_cons (NULL_TREE, parms[i], parmlist);
  tree fn = implicitly_declare_fn (sfk_inheriting_constructor,
				   t, false, ctor, parmlist);
  if (add_method (t, fn, NULL_TREE))
    {
      DECL_CHAIN (fn) = TYPE_METHODS (t);
      TYPE_METHODS (t) = fn;
    }
}

/* Declare all the inheriting constructors for class T inherited from base
   constructor CTOR.  */

static void
one_inherited_ctor (tree ctor, tree t)
{
  tree parms = FUNCTION_FIRST_USER_PARMTYPE (ctor);

  tree *new_parms = XALLOCAVEC (tree, list_length (parms));
  int i = 0;
  for (; parms && parms != void_list_node; parms = TREE_CHAIN (parms))
    {
      if (TREE_PURPOSE (parms))
	one_inheriting_sig (t, ctor, new_parms, i);
      new_parms[i++] = TREE_VALUE (parms);
    }
  one_inheriting_sig (t, ctor, new_parms, i);
  if (parms == NULL_TREE)
    {
      warning (OPT_Winherited_variadic_ctor,
	       "the ellipsis in %qD is not inherited", ctor);
      inform (DECL_SOURCE_LOCATION (ctor), "%qD declared here", ctor);
    }
}

/* Create default constructors, assignment operators, and so forth for
   the type indicated by T, if they are needed.  CANT_HAVE_CONST_CTOR,
   and CANT_HAVE_CONST_ASSIGNMENT are nonzero if, for whatever reason,
   the class cannot have a default constructor, copy constructor
   taking a const reference argument, or an assignment operator taking
   a const reference, respectively.  */

static void
add_implicitly_declared_members (tree t, tree* access_decls,
				 int cant_have_const_cctor,
				 int cant_have_const_assignment)
{
  bool move_ok = false;

  if (cxx_dialect >= cxx11 && !CLASSTYPE_DESTRUCTORS (t)
      && !TYPE_HAS_COPY_CTOR (t) && !TYPE_HAS_COPY_ASSIGN (t)
      && !type_has_move_constructor (t) && !type_has_move_assign (t))
    move_ok = true;

  /* Destructor.  */
  if (!CLASSTYPE_DESTRUCTORS (t))
    {
      /* In general, we create destructors lazily.  */
      CLASSTYPE_LAZY_DESTRUCTOR (t) = 1;

      if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (t)
	  && TYPE_FOR_JAVA (t))
	/* But if this is a Java class, any non-trivial destructor is
	   invalid, even if compiler-generated.  Therefore, if the
	   destructor is non-trivial we create it now.  */
	lazily_declare_fn (sfk_destructor, t);
    }

  /* [class.ctor]

     If there is no user-declared constructor for a class, a default
     constructor is implicitly declared.  */
  if (! TYPE_HAS_USER_CONSTRUCTOR (t))
    {
      TYPE_HAS_DEFAULT_CONSTRUCTOR (t) = 1;
      CLASSTYPE_LAZY_DEFAULT_CTOR (t) = 1;
      if (cxx_dialect >= cxx11)
	TYPE_HAS_CONSTEXPR_CTOR (t)
	  /* This might force the declaration.  */
	  = type_has_constexpr_default_constructor (t);
    }

  /* [class.ctor]

     If a class definition does not explicitly declare a copy
     constructor, one is declared implicitly.  */
  if (! TYPE_HAS_COPY_CTOR (t) && ! TYPE_FOR_JAVA (t))
    {
      TYPE_HAS_COPY_CTOR (t) = 1;
      TYPE_HAS_CONST_COPY_CTOR (t) = !cant_have_const_cctor;
      CLASSTYPE_LAZY_COPY_CTOR (t) = 1;
      if (move_ok)
	CLASSTYPE_LAZY_MOVE_CTOR (t) = 1;
    }

  /* If there is no assignment operator, one will be created if and
     when it is needed.  For now, just record whether or not the type
     of the parameter to the assignment operator will be a const or
     non-const reference.  */
  if (!TYPE_HAS_COPY_ASSIGN (t) && !TYPE_FOR_JAVA (t))
    {
      TYPE_HAS_COPY_ASSIGN (t) = 1;
      TYPE_HAS_CONST_COPY_ASSIGN (t) = !cant_have_const_assignment;
      CLASSTYPE_LAZY_COPY_ASSIGN (t) = 1;
      if (move_ok)
	CLASSTYPE_LAZY_MOVE_ASSIGN (t) = 1;
    }

  /* We can't be lazy about declaring functions that might override
     a virtual function from a base class.  */
  declare_virt_assop_and_dtor (t);

  while (*access_decls)
    {
      tree using_decl = TREE_VALUE (*access_decls);
      tree decl = USING_DECL_DECLS (using_decl);
      if (DECL_NAME (using_decl) == ctor_identifier)
	{
	  /* declare, then remove the decl */
	  tree ctor_list = decl;
	  location_t loc = input_location;
	  input_location = DECL_SOURCE_LOCATION (using_decl);
	  if (ctor_list)
	    for (; ctor_list; ctor_list = OVL_NEXT (ctor_list))
	      one_inherited_ctor (OVL_CURRENT (ctor_list), t);
	  *access_decls = TREE_CHAIN (*access_decls);
	  input_location = loc;
	}
      else
	access_decls = &TREE_CHAIN (*access_decls);
    }
}

/* Subroutine of insert_into_classtype_sorted_fields.  Recursively
   count the number of fields in TYPE, including anonymous union
   members.  */

static int
count_fields (tree fields)
{
  tree x;
  int n_fields = 0;
  for (x = fields; x; x = DECL_CHAIN (x))
    {
      if (TREE_CODE (x) == FIELD_DECL && ANON_AGGR_TYPE_P (TREE_TYPE (x)))
	n_fields += count_fields (TYPE_FIELDS (TREE_TYPE (x)));
      else
	n_fields += 1;
    }
  return n_fields;
}

/* Subroutine of insert_into_classtype_sorted_fields.  Recursively add
   all the fields in the TREE_LIST FIELDS to the SORTED_FIELDS_TYPE
   elts, starting at offset IDX.  */

static int
add_fields_to_record_type (tree fields, struct sorted_fields_type *field_vec, int idx)
{
  tree x;
  for (x = fields; x; x = DECL_CHAIN (x))
    {
      if (TREE_CODE (x) == FIELD_DECL && ANON_AGGR_TYPE_P (TREE_TYPE (x)))
	idx = add_fields_to_record_type (TYPE_FIELDS (TREE_TYPE (x)), field_vec, idx);
      else
	field_vec->elts[idx++] = x;
    }
  return idx;
}

/* Add all of the enum values of ENUMTYPE, to the FIELD_VEC elts,
   starting at offset IDX.  */

static int
add_enum_fields_to_record_type (tree enumtype,
				struct sorted_fields_type *field_vec,
				int idx)
{
  tree values;
  for (values = TYPE_VALUES (enumtype); values; values = TREE_CHAIN (values))
      field_vec->elts[idx++] = TREE_VALUE (values);
  return idx;
}

/* FIELD is a bit-field.  We are finishing the processing for its
   enclosing type.  Issue any appropriate messages and set appropriate
   flags.  Returns false if an error has been diagnosed.  */

static bool
check_bitfield_decl (tree field)
{
  tree type = TREE_TYPE (field);
  tree w;

  /* Extract the declared width of the bitfield, which has been
     temporarily stashed in DECL_INITIAL.  */
  w = DECL_INITIAL (field);
  gcc_assert (w != NULL_TREE);
  /* Remove the bit-field width indicator so that the rest of the
     compiler does not treat that value as an initializer.  */
  DECL_INITIAL (field) = NULL_TREE;

  /* Detect invalid bit-field type.  */
  if (!INTEGRAL_OR_ENUMERATION_TYPE_P (type))
    {
      error ("bit-field %q+#D with non-integral type", field);
      w = error_mark_node;
    }
  else
    {
      location_t loc = input_location;
      /* Avoid the non_lvalue wrapper added by fold for PLUS_EXPRs.  */
      STRIP_NOPS (w);

      /* detect invalid field size.  */
      input_location = DECL_SOURCE_LOCATION (field);
      w = cxx_constant_value (w);
      input_location = loc;

      if (TREE_CODE (w) != INTEGER_CST)
	{
	  error ("bit-field %q+D width not an integer constant", field);
	  w = error_mark_node;
	}
      else if (tree_int_cst_sgn (w) < 0)
	{
	  error ("negative width in bit-field %q+D", field);
	  w = error_mark_node;
	}
      else if (integer_zerop (w) && DECL_NAME (field) != 0)
	{
	  error ("zero width for bit-field %q+D", field);
	  w = error_mark_node;
	}
      else if ((TREE_CODE (type) != ENUMERAL_TYPE
		&& TREE_CODE (type) != BOOLEAN_TYPE
		&& compare_tree_int (w, TYPE_PRECISION (type)) > 0)
	       || ((TREE_CODE (type) == ENUMERAL_TYPE
		    || TREE_CODE (type) == BOOLEAN_TYPE)
		   && tree_int_cst_lt (TYPE_SIZE (type), w)))
	warning (0, "width of %q+D exceeds its type", field);
      else if (TREE_CODE (type) == ENUMERAL_TYPE
	       && (0 > (compare_tree_int
			(w, TYPE_PRECISION (ENUM_UNDERLYING_TYPE (type))))))
	warning (0, "%q+D is too small to hold all values of %q#T", field, type);
    }

  if (w != error_mark_node)
    {
      DECL_SIZE (field) = convert (bitsizetype, w);
      DECL_BIT_FIELD (field) = 1;
      return true;
    }
  else
    {
      /* Non-bit-fields are aligned for their type.  */
      DECL_BIT_FIELD (field) = 0;
      CLEAR_DECL_C_BIT_FIELD (field);
      return false;
    }
}

/* FIELD is a non bit-field.  We are finishing the processing for its
   enclosing type T.  Issue any appropriate messages and set appropriate
   flags.  */

static void
check_field_decl (tree field,
		  tree t,
		  int* cant_have_const_ctor,
		  int* no_const_asn_ref,
		  int* any_default_members)
{
  tree type = strip_array_types (TREE_TYPE (field));

  /* In C++98 an anonymous union cannot contain any fields which would change
     the settings of CANT_HAVE_CONST_CTOR and friends.  */
  if (ANON_UNION_TYPE_P (type) && cxx_dialect < cxx11)
    ;
  /* And, we don't set TYPE_HAS_CONST_COPY_CTOR, etc., for anonymous
     structs.  So, we recurse through their fields here.  */
  else if (ANON_AGGR_TYPE_P (type))
    {
      tree fields;

      for (fields = TYPE_FIELDS (type); fields; fields = DECL_CHAIN (fields))
	if (TREE_CODE (fields) == FIELD_DECL && !DECL_C_BIT_FIELD (field))
	  check_field_decl (fields, t, cant_have_const_ctor,
			    no_const_asn_ref, any_default_members);
    }
  /* Check members with class type for constructors, destructors,
     etc.  */
  else if (CLASS_TYPE_P (type))
    {
      /* Never let anything with uninheritable virtuals
	 make it through without complaint.  */
      abstract_virtuals_error (field, type);

      if (TREE_CODE (t) == UNION_TYPE && cxx_dialect < cxx11)
	{
	  static bool warned;
	  int oldcount = errorcount;
	  if (TYPE_NEEDS_CONSTRUCTING (type))
	    error ("member %q+#D with constructor not allowed in union",
		   field);
	  if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (type))
	    error ("member %q+#D with destructor not allowed in union", field);
	  if (TYPE_HAS_COMPLEX_COPY_ASSIGN (type))
	    error ("member %q+#D with copy assignment operator not allowed in union",
		   field);
	  if (!warned && errorcount > oldcount)
	    {
	      inform (DECL_SOURCE_LOCATION (field), "unrestricted unions "
		      "only available with -std=c++11 or -std=gnu++11");
	      warned = true;
	    }
	}
      else
	{
	  TYPE_NEEDS_CONSTRUCTING (t) |= TYPE_NEEDS_CONSTRUCTING (type);
	  TYPE_HAS_NONTRIVIAL_DESTRUCTOR (t)
	    |= TYPE_HAS_NONTRIVIAL_DESTRUCTOR (type);
	  TYPE_HAS_COMPLEX_COPY_ASSIGN (t)
	    |= (TYPE_HAS_COMPLEX_COPY_ASSIGN (type)
		|| !TYPE_HAS_COPY_ASSIGN (type));
	  TYPE_HAS_COMPLEX_COPY_CTOR (t) |= (TYPE_HAS_COMPLEX_COPY_CTOR (type)
					     || !TYPE_HAS_COPY_CTOR (type));
	  TYPE_HAS_COMPLEX_MOVE_ASSIGN (t) |= TYPE_HAS_COMPLEX_MOVE_ASSIGN (type);
	  TYPE_HAS_COMPLEX_MOVE_CTOR (t) |= TYPE_HAS_COMPLEX_MOVE_CTOR (type);
	  TYPE_HAS_COMPLEX_DFLT (t) |= (!TYPE_HAS_DEFAULT_CONSTRUCTOR (type)
					|| TYPE_HAS_COMPLEX_DFLT (type));
	}

      if (TYPE_HAS_COPY_CTOR (type)
	  && !TYPE_HAS_CONST_COPY_CTOR (type))
	*cant_have_const_ctor = 1;

      if (TYPE_HAS_COPY_ASSIGN (type)
	  && !TYPE_HAS_CONST_COPY_ASSIGN (type))
	*no_const_asn_ref = 1;
    }

  check_abi_tags (t, field);

  if (DECL_INITIAL (field) != NULL_TREE)
    {
      /* `build_class_init_list' does not recognize
	 non-FIELD_DECLs.  */
      if (TREE_CODE (t) == UNION_TYPE && *any_default_members != 0)
	error ("multiple fields in union %qT initialized", t);
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
		   int *cant_have_const_ctor_p,
		   int *no_const_asn_ref_p)
{
  tree *field;
  tree *next;
  bool has_pointers;
  int any_default_members;
  int cant_pack = 0;
  int field_access = -1;

  /* Assume there are no access declarations.  */
  *access_decls = NULL_TREE;
  /* Assume this class has no pointer members.  */
  has_pointers = false;
  /* Assume none of the members of this class have default
     initializations.  */
  any_default_members = 0;

  for (field = &TYPE_FIELDS (t); *field; field = next)
    {
      tree x = *field;
      tree type = TREE_TYPE (x);
      int this_field_access;

      next = &DECL_CHAIN (x);

      if (TREE_CODE (x) == USING_DECL)
	{
	  /* Save the access declarations for our caller.  */
	  *access_decls = tree_cons (NULL_TREE, x, *access_decls);
	  continue;
	}

      if (TREE_CODE (x) == TYPE_DECL
	  || TREE_CODE (x) == TEMPLATE_DECL)
	continue;

      /* If we've gotten this far, it's a data member, possibly static,
	 or an enumerator.  */
      if (TREE_CODE (x) != CONST_DECL)
	DECL_CONTEXT (x) = t;

      /* When this goes into scope, it will be a non-local reference.  */
      DECL_NONLOCAL (x) = 1;

      if (TREE_CODE (t) == UNION_TYPE)
	{
	  /* [class.union]

	     If a union contains a static data member, or a member of
	     reference type, the program is ill-formed.  */
	  if (VAR_P (x))
	    {
	      error ("%q+D may not be static because it is a member of a union", x);
	      continue;
	    }
	  if (TREE_CODE (type) == REFERENCE_TYPE)
	    {
	      error ("%q+D may not have reference type %qT because"
		     " it is a member of a union",
		     x, type);
	      continue;
	    }
	}

      /* Perform error checking that did not get done in
	 grokdeclarator.  */
      if (TREE_CODE (type) == FUNCTION_TYPE)
	{
	  error ("field %q+D invalidly declared function type", x);
	  type = build_pointer_type (type);
	  TREE_TYPE (x) = type;
	}
      else if (TREE_CODE (type) == METHOD_TYPE)
	{
	  error ("field %q+D invalidly declared method type", x);
	  type = build_pointer_type (type);
	  TREE_TYPE (x) = type;
	}

      if (type == error_mark_node)
	continue;

      if (TREE_CODE (x) == CONST_DECL || VAR_P (x))
	continue;

      /* Now it can only be a FIELD_DECL.  */

      if (TREE_PRIVATE (x) || TREE_PROTECTED (x))
	CLASSTYPE_NON_AGGREGATE (t) = 1;

      /* If at least one non-static data member is non-literal, the whole
         class becomes non-literal.  Note: if the type is incomplete we
	 will complain later on.  */
      if (COMPLETE_TYPE_P (type) && !literal_type_p (type))
        CLASSTYPE_LITERAL_P (t) = false;

      /* A standard-layout class is a class that:
	 ...
	 has the same access control (Clause 11) for all non-static data members,
         ...  */
      this_field_access = TREE_PROTECTED (x) ? 1 : TREE_PRIVATE (x) ? 2 : 0;
      if (field_access == -1)
	field_access = this_field_access;
      else if (this_field_access != field_access)
	CLASSTYPE_NON_STD_LAYOUT (t) = 1;

      /* If this is of reference type, check if it needs an init.  */
      if (TREE_CODE (type) == REFERENCE_TYPE)
	{
	  CLASSTYPE_NON_LAYOUT_POD_P (t) = 1;
	  CLASSTYPE_NON_STD_LAYOUT (t) = 1;
	  if (DECL_INITIAL (x) == NULL_TREE)
	    SET_CLASSTYPE_REF_FIELDS_NEED_INIT (t, 1);

	  /* ARM $12.6.2: [A member initializer list] (or, for an
	     aggregate, initialization by a brace-enclosed list) is the
	     only way to initialize nonstatic const and reference
	     members.  */
	  TYPE_HAS_COMPLEX_COPY_ASSIGN (t) = 1;
	  TYPE_HAS_COMPLEX_MOVE_ASSIGN (t) = 1;
	}

      type = strip_array_types (type);

      if (TYPE_PACKED (t))
	{
	  if (!layout_pod_type_p (type) && !TYPE_PACKED (type))
	    {
	      warning
		(0,
		 "ignoring packed attribute because of unpacked non-POD field %q+#D",
		 x);
	      cant_pack = 1;
	    }
	  else if (DECL_C_BIT_FIELD (x)
		   || TYPE_ALIGN (TREE_TYPE (x)) > BITS_PER_UNIT)
	    DECL_PACKED (x) = 1;
	}

      if (DECL_C_BIT_FIELD (x) && integer_zerop (DECL_INITIAL (x)))
	/* We don't treat zero-width bitfields as making a class
	   non-empty.  */
	;
      else
	{
	  /* The class is non-empty.  */
	  CLASSTYPE_EMPTY_P (t) = 0;
	  /* The class is not even nearly empty.  */
	  CLASSTYPE_NEARLY_EMPTY_P (t) = 0;
	  /* If one of the data members contains an empty class,
	     so does T.  */
	  if (CLASS_TYPE_P (type)
	      && CLASSTYPE_CONTAINS_EMPTY_CLASS_P (type))
	    CLASSTYPE_CONTAINS_EMPTY_CLASS_P (t) = 1;
	}

      /* This is used by -Weffc++ (see below). Warn only for pointers
	 to members which might hold dynamic memory. So do not warn
	 for pointers to functions or pointers to members.  */
      if (TYPE_PTR_P (type)
	  && !TYPE_PTRFN_P (type))
	has_pointers = true;

      if (CLASS_TYPE_P (type))
	{
	  if (CLASSTYPE_REF_FIELDS_NEED_INIT (type))
	    SET_CLASSTYPE_REF_FIELDS_NEED_INIT (t, 1);
	  if (CLASSTYPE_READONLY_FIELDS_NEED_INIT (type))
	    SET_CLASSTYPE_READONLY_FIELDS_NEED_INIT (t, 1);
	}

      if (DECL_MUTABLE_P (x) || TYPE_HAS_MUTABLE_P (type))
	CLASSTYPE_HAS_MUTABLE (t) = 1;

      if (DECL_MUTABLE_P (x))
	{
	  if (CP_TYPE_CONST_P (type))
	    {
	      error ("member %q+D cannot be declared both %<const%> "
		     "and %<mutable%>", x);
	      continue;
	    }
	  if (TREE_CODE (type) == REFERENCE_TYPE)
	    {
	      error ("member %q+D cannot be declared as a %<mutable%> "
		     "reference", x);
	      continue;
	    }
	}

      if (! layout_pod_type_p (type))
	/* DR 148 now allows pointers to members (which are POD themselves),
	   to be allowed in POD structs.  */
	CLASSTYPE_NON_LAYOUT_POD_P (t) = 1;

      if (!std_layout_type_p (type))
	CLASSTYPE_NON_STD_LAYOUT (t) = 1;

      if (! zero_init_p (type))
	CLASSTYPE_NON_ZERO_INIT_P (t) = 1;

      /* We set DECL_C_BIT_FIELD in grokbitfield.
	 If the type and width are valid, we'll also set DECL_BIT_FIELD.  */
      if (! DECL_C_BIT_FIELD (x) || ! check_bitfield_decl (x))
	check_field_decl (x, t,
			  cant_have_const_ctor_p,
			  no_const_asn_ref_p,
			  &any_default_members);

      /* Now that we've removed bit-field widths from DECL_INITIAL,
	 anything left in DECL_INITIAL is an NSDMI that makes the class
	 non-aggregate.  */
      if (DECL_INITIAL (x))
	CLASSTYPE_NON_AGGREGATE (t) = true;

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
	  TYPE_HAS_COMPLEX_COPY_ASSIGN (t) = 1;
	  TYPE_HAS_COMPLEX_MOVE_ASSIGN (t) = 1;
	}
      /* A field that is pseudo-const makes the structure likewise.  */
      else if (CLASS_TYPE_P (type))
	{
	  C_TYPE_FIELDS_READONLY (t) |= C_TYPE_FIELDS_READONLY (type);
	  SET_CLASSTYPE_READONLY_FIELDS_NEED_INIT (t,
	    CLASSTYPE_READONLY_FIELDS_NEED_INIT (t)
	    | CLASSTYPE_READONLY_FIELDS_NEED_INIT (type));
	}

      /* Core issue 80: A nonstatic data member is required to have a
	 different name from the class iff the class has a
	 user-declared constructor.  */
      if (constructor_name_p (DECL_NAME (x), t)
	  && TYPE_HAS_USER_CONSTRUCTOR (t))
	permerror (input_location, "field %q+#D with same name as class", x);
    }

  /* Effective C++ rule 11: if a class has dynamic memory held by pointers,
     it should also define a copy constructor and an assignment operator to
     implement the correct copy semantic (deep vs shallow, etc.). As it is
     not feasible to check whether the constructors do allocate dynamic memory
     and store it within members, we approximate the warning like this:

     -- Warn only if there are members which are pointers
     -- Warn only if there is a non-trivial constructor (otherwise,
	there cannot be memory allocated).
     -- Warn only if there is a non-trivial destructor. We assume that the
	user at least implemented the cleanup correctly, and a destructor
	is needed to free dynamic memory.

     This seems enough for practical purposes.  */
  if (warn_ecpp
      && has_pointers
      && TYPE_HAS_USER_CONSTRUCTOR (t)
      && TYPE_HAS_NONTRIVIAL_DESTRUCTOR (t)
      && !(TYPE_HAS_COPY_CTOR (t) && TYPE_HAS_COPY_ASSIGN (t)))
    {
      warning (OPT_Weffc__, "%q#T has pointer data members", t);

      if (! TYPE_HAS_COPY_CTOR (t))
	{
	  warning (OPT_Weffc__,
		   "  but does not override %<%T(const %T&)%>", t, t);
	  if (!TYPE_HAS_COPY_ASSIGN (t))
	    warning (OPT_Weffc__, "  or %<operator=(const %T&)%>", t);
	}
      else if (! TYPE_HAS_COPY_ASSIGN (t))
	warning (OPT_Weffc__,
		 "  but does not override %<operator=(const %T&)%>", t);
    }

  /* Non-static data member initializers make the default constructor
     non-trivial.  */
  if (any_default_members)
    {
      TYPE_NEEDS_CONSTRUCTING (t) = true;
      TYPE_HAS_COMPLEX_DFLT (t) = true;
    }

  /* If any of the fields couldn't be packed, unset TYPE_PACKED.  */
  if (cant_pack)
    TYPE_PACKED (t) = 0;

  /* Check anonymous struct/anonymous union fields.  */
  finish_struct_anon (t);

  /* We've built up the list of access declarations in reverse order.
     Fix that now.  */
  *access_decls = nreverse (*access_decls);
}

/* If TYPE is an empty class type, records its OFFSET in the table of
   OFFSETS.  */

static int
record_subobject_offset (tree type, tree offset, splay_tree offsets)
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
check_subobject_offset (tree type, tree offset, splay_tree offsets)
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
walk_subobject_offsets (tree type,
			subobject_offset_fn f,
			tree offset,
			splay_tree offsets,
			tree max_offset,
			int vbases_p)
{
  int r = 0;
  tree type_binfo = NULL_TREE;

  /* If this OFFSET is bigger than the MAX_OFFSET, then we should
     stop.  */
  if (max_offset && INT_CST_LT (max_offset, offset))
    return 0;

  if (type == error_mark_node)
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
      for (i = 0; BINFO_BASE_ITERATE (type_binfo, i, binfo); i++)
	{
	  tree binfo_offset;

	  if (abi_version_at_least (2)
	      && BINFO_VIRTUAL_P (binfo))
	    continue;

	  if (!vbases_p
	      && BINFO_VIRTUAL_P (binfo)
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
	      orig_binfo = BINFO_BASE_BINFO (TYPE_BINFO (type), i);
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

      if (abi_version_at_least (2) && CLASSTYPE_VBASECLASSES (type))
	{
	  unsigned ix;
	  vec<tree, va_gc> *vbases;

	  /* Iterate through the virtual base classes of TYPE.  In G++
	     3.2, we included virtual bases in the direct base class
	     loop above, which results in incorrect results; the
	     correct offsets for virtual bases are only known when
	     working with the most derived type.  */
	  if (vbases_p)
	    for (vbases = CLASSTYPE_VBASECLASSES (type), ix = 0;
		 vec_safe_iterate (vbases, ix, &binfo); ix++)
	      {
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
	      tree vbase = get_primary_binfo (type_binfo);

	      if (vbase && BINFO_VIRTUAL_P (vbase)
		  && BINFO_PRIMARY_P (vbase)
		  && BINFO_INHERITANCE_CHAIN (vbase) == type_binfo)
		{
		  r = (walk_subobject_offsets
		       (vbase, f, offset,
			offsets, max_offset, /*vbases_p=*/0));
		  if (r)
		    return r;
		}
	    }
	}

      /* Iterate through the fields of TYPE.  */
      for (field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
	if (TREE_CODE (field) == FIELD_DECL
	    && TREE_TYPE (field) != error_mark_node
	    && !DECL_ARTIFICIAL (field))
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

/* Record all of the empty subobjects of TYPE (either a type or a
   binfo).  If IS_DATA_MEMBER is true, then a non-static data member
   is being placed at OFFSET; otherwise, it is a base class that is
   being placed at OFFSET.  */

static void
record_subobject_offsets (tree type,
			  tree offset,
			  splay_tree offsets,
			  bool is_data_member)
{
  tree max_offset;
  /* If recording subobjects for a non-static data member or a
     non-empty base class , we do not need to record offsets beyond
     the size of the biggest empty class.  Additional data members
     will go at the end of the class.  Additional base classes will go
     either at offset zero (if empty, in which case they cannot
     overlap with offsets past the size of the biggest empty class) or
     at the end of the class.

     However, if we are placing an empty base class, then we must record
     all offsets, as either the empty class is at offset zero (where
     other empty classes might later be placed) or at the end of the
     class (where other objects might then be placed, so other empty
     subobjects might later overlap).  */
  if (is_data_member
      || !is_empty_class (BINFO_TYPE (type)))
    max_offset = sizeof_biggest_empty_class;
  else
    max_offset = NULL_TREE;
  walk_subobject_offsets (type, record_subobject_offset, offset,
			  offsets, max_offset, is_data_member);
}

/* Returns nonzero if any of the empty subobjects of TYPE (located at
   OFFSET) conflict with entries in OFFSETS.  If VBASES_P is nonzero,
   virtual bases of TYPE are examined.  */

static int
layout_conflict_p (tree type,
		   tree offset,
		   splay_tree offsets,
		   int vbases_p)
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
	 For example, consider:

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
      /* In a union, overlap is permitted; all members are placed at
	 offset zero.  */
      if (TREE_CODE (rli->t) == UNION_TYPE)
	break;
      /* G++ 3.2 did not check for overlaps when placing a non-empty
	 virtual base.  */
      if (!abi_version_at_least (2) && binfo && BINFO_VIRTUAL_P (binfo))
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
			     size_diffop_loc (input_location,
					  convert (ssizetype, offset),
					  convert (ssizetype,
						   BINFO_OFFSET (binfo))));
}

/* Returns true if TYPE is empty and OFFSET is nonzero.  */

static int
empty_base_at_nonzero_offset_p (tree type,
				tree offset,
				splay_tree /*offsets*/)
{
  return is_empty_class (type) && !integer_zerop (offset);
}

/* Layout the empty base BINFO.  EOC indicates the byte currently just
   past the end of the class, and should be correctly aligned for a
   class of the type indicated by BINFO; OFFSETS gives the offsets of
   the empty bases allocated so far. T is the most derived
   type.  Return nonzero iff we added it at the end.  */

static bool
layout_empty_base (record_layout_info rli, tree binfo,
		   tree eoc, splay_tree offsets)
{
  tree alignment;
  tree basetype = BINFO_TYPE (binfo);
  bool atend = false;

  /* This routine should only be used for empty classes.  */
  gcc_assert (is_empty_class (basetype));
  alignment = ssize_int (CLASSTYPE_ALIGN_UNIT (basetype));

  if (!integer_zerop (BINFO_OFFSET (binfo)))
    {
      if (abi_version_at_least (2))
	propagate_binfo_offsets
	  (binfo, size_diffop_loc (input_location,
			       size_zero_node, BINFO_OFFSET (binfo)));
      else
	warning (OPT_Wabi,
		 "offset of empty base %qT may not be ABI-compliant and may"
		 "change in a future version of GCC",
		 BINFO_TYPE (binfo));
    }

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
      propagate_binfo_offsets (binfo, convert (ssizetype, eoc));
      while (1)
	{
	  if (!layout_conflict_p (binfo,
				  BINFO_OFFSET (binfo),
				  offsets,
				  /*vbases_p=*/0))
	    /* We finally found a spot where there's no overlap.  */
	    break;

	  /* There's overlap here, too.  Bump along to the next spot.  */
	  propagate_binfo_offsets (binfo, alignment);
	}
    }

  if (CLASSTYPE_USER_ALIGN (basetype))
    {
      rli->record_align = MAX (rli->record_align, CLASSTYPE_ALIGN (basetype));
      if (warn_packed)
	rli->unpacked_align = MAX (rli->unpacked_align, CLASSTYPE_ALIGN (basetype));
      TYPE_USER_ALIGN (rli->t) = 1;
    }

  return atend;
}

/* Layout the base given by BINFO in the class indicated by RLI.
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
      decl = build_decl (input_location,
			 FIELD_DECL, NULL_TREE, CLASSTYPE_AS_BASE (basetype));
      DECL_ARTIFICIAL (decl) = 1;
      DECL_IGNORED_P (decl) = 1;
      DECL_FIELD_CONTEXT (decl) = t;
      if (CLASSTYPE_AS_BASE (basetype))
	{
	  DECL_SIZE (decl) = CLASSTYPE_SIZE (basetype);
	  DECL_SIZE_UNIT (decl) = CLASSTYPE_SIZE_UNIT (basetype);
	  DECL_ALIGN (decl) = CLASSTYPE_ALIGN (basetype);
	  DECL_USER_ALIGN (decl) = CLASSTYPE_USER_ALIGN (basetype);
	  DECL_MODE (decl) = TYPE_MODE (basetype);
	  DECL_FIELD_IS_BASE (decl) = 1;

	  /* Try to place the field.  It may take more than one try if we
	     have a hard time placing the field without putting two
	     objects of the same type at the same address.  */
	  layout_nonempty_base_or_field (rli, decl, binfo, offsets);
	  /* Add the new FIELD_DECL to the list of fields for T.  */
	  DECL_CHAIN (decl) = *next_field;
	  *next_field = decl;
	  next_field = &DECL_CHAIN (decl);
	}
    }
  else
    {
      tree eoc;
      bool atend;

      /* On some platforms (ARM), even empty classes will not be
	 byte-aligned.  */
      eoc = round_up_loc (input_location,
		      rli_size_unit_so_far (rli),
		      CLASSTYPE_ALIGN_UNIT (basetype));
      atend = layout_empty_base (rli, binfo, eoc, offsets);
      /* A nearly-empty class "has no proper base class that is empty,
	 not morally virtual, and at an offset other than zero."  */
      if (!BINFO_VIRTUAL_P (binfo) && CLASSTYPE_NEARLY_EMPTY_P (t))
	{
	  if (atend)
	    CLASSTYPE_NEARLY_EMPTY_P (t) = 0;
	  /* The check above (used in G++ 3.2) is insufficient because
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
	      else
		warning (OPT_Wabi,
			 "class %qT will be considered nearly empty in a "
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
			    /*is_data_member=*/false);

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
  int n_baseclasses = BINFO_N_BASE_BINFOS (TYPE_BINFO (t));
  int i;

  /* The primary base class is always allocated first.  */
  if (CLASSTYPE_HAS_PRIMARY_BASE_P (t))
    next_field = build_base_field (rli, CLASSTYPE_PRIMARY_BINFO (t),
				   offsets, next_field);

  /* Now allocate the rest of the bases.  */
  for (i = 0; i < n_baseclasses; ++i)
    {
      tree base_binfo;

      base_binfo = BINFO_BASE_BINFO (TYPE_BINFO (t), i);

      /* The primary base was already allocated above, so we don't
	 need to allocate it again here.  */
      if (base_binfo == CLASSTYPE_PRIMARY_BINFO (t))
	continue;

      /* Virtual bases are added at the end (a primary virtual base
	 will have already been added).  */
      if (BINFO_VIRTUAL_P (base_binfo))
	continue;

      next_field = build_base_field (rli, base_binfo,
				     offsets, next_field);
    }
}

/* Go through the TYPE_METHODS of T issuing any appropriate
   diagnostics, figuring out which methods override which other
   methods, and so forth.  */

static void
check_methods (tree t)
{
  tree x;

  for (x = TYPE_METHODS (t); x; x = DECL_CHAIN (x))
    {
      check_for_override (x, t);
      if (DECL_PURE_VIRTUAL_P (x) && ! DECL_VINDEX (x))
	error ("initializer specified for non-virtual method %q+D", x);
      /* The name of the field is the original field name
	 Save this in auxiliary field for later overloading.  */
      if (DECL_VINDEX (x))
	{
	  TYPE_POLYMORPHIC_P (t) = 1;
	  if (DECL_PURE_VIRTUAL_P (x))
	    vec_safe_push (CLASSTYPE_PURE_VIRTUALS (t), x);
	}
      /* All user-provided destructors are non-trivial.
         Constructors and assignment ops are handled in
	 grok_special_member_properties.  */
      if (DECL_DESTRUCTOR_P (x) && user_provided_p (x))
	TYPE_HAS_NONTRIVIAL_DESTRUCTOR (t) = 1;
    }
}

/* FN is a constructor or destructor.  Clone the declaration to create
   a specialized in-charge or not-in-charge version, as indicated by
   NAME.  */

static tree
build_clone (tree fn, tree name)
{
  tree parms;
  tree clone;

  /* Copy the function.  */
  clone = copy_decl (fn);
  /* Reset the function name.  */
  DECL_NAME (clone) = name;
  SET_DECL_ASSEMBLER_NAME (clone, NULL_TREE);
  /* Remember where this function came from.  */
  DECL_ABSTRACT_ORIGIN (clone) = fn;
  /* Make it easy to find the CLONE given the FN.  */
  DECL_CHAIN (clone) = DECL_CHAIN (fn);
  DECL_CHAIN (fn) = clone;

  /* If this is a template, do the rest on the DECL_TEMPLATE_RESULT.  */
  if (TREE_CODE (clone) == TEMPLATE_DECL)
    {
      tree result = build_clone (DECL_TEMPLATE_RESULT (clone), name);
      DECL_TEMPLATE_RESULT (clone) = result;
      DECL_TEMPLATE_INFO (result) = copy_node (DECL_TEMPLATE_INFO (result));
      DECL_TI_TEMPLATE (result) = clone;
      TREE_TYPE (clone) = TREE_TYPE (result);
      return clone;
    }

  DECL_CLONED_FUNCTION (clone) = fn;
  /* There's no pending inline data for this function.  */
  DECL_PENDING_INLINE_INFO (clone) = NULL;
  DECL_PENDING_INLINE_P (clone) = 0;

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
	= build_method_type_directly (basetype,
				      TREE_TYPE (TREE_TYPE (clone)),
				      parmtypes);
      if (exceptions)
	TREE_TYPE (clone) = build_exception_variant (TREE_TYPE (clone),
						     exceptions);
      TREE_TYPE (clone)
	= cp_build_type_attribute_variant (TREE_TYPE (clone),
					   TYPE_ATTRIBUTES (TREE_TYPE (fn)));
    }

  /* Copy the function parameters.  */
  DECL_ARGUMENTS (clone) = copy_list (DECL_ARGUMENTS (clone));
  /* Remove the in-charge parameter.  */
  if (DECL_HAS_IN_CHARGE_PARM_P (clone))
    {
      DECL_CHAIN (DECL_ARGUMENTS (clone))
	= DECL_CHAIN (DECL_CHAIN (DECL_ARGUMENTS (clone)));
      DECL_HAS_IN_CHARGE_PARM_P (clone) = 0;
    }
  /* And the VTT parm, in a complete [cd]tor.  */
  if (DECL_HAS_VTT_PARM_P (fn))
    {
      if (DECL_NEEDS_VTT_PARM_P (clone))
	DECL_HAS_VTT_PARM_P (clone) = 1;
      else
	{
	  DECL_CHAIN (DECL_ARGUMENTS (clone))
	    = DECL_CHAIN (DECL_CHAIN (DECL_ARGUMENTS (clone)));
	  DECL_HAS_VTT_PARM_P (clone) = 0;
	}
    }

  for (parms = DECL_ARGUMENTS (clone); parms; parms = DECL_CHAIN (parms))
    {
      DECL_CONTEXT (parms) = clone;
      cxx_dup_lang_specific_decl (parms);
    }

  /* Create the RTL for this function.  */
  SET_DECL_RTL (clone, NULL);
  rest_of_decl_compilation (clone, /*top_level=*/1, at_eof);

  if (pch_file)
    note_decl_for_pch (clone);

  return clone;
}

/* Implementation of DECL_CLONED_FUNCTION and DECL_CLONED_FUNCTION_P, do
   not invoke this function directly.

   For a non-thunk function, returns the address of the slot for storing
   the function it is a clone of.  Otherwise returns NULL_TREE.

   If JUST_TESTING, looks through TEMPLATE_DECL and returns NULL if
   cloned_function is unset.  This is to support the separate
   DECL_CLONED_FUNCTION and DECL_CLONED_FUNCTION_P modes; using the latter
   on a template makes sense, but not the former.  */

tree *
decl_cloned_function_p (const_tree decl, bool just_testing)
{
  tree *ptr;
  if (just_testing)
    decl = STRIP_TEMPLATE (decl);

  if (TREE_CODE (decl) != FUNCTION_DECL
      || !DECL_LANG_SPECIFIC (decl)
      || DECL_LANG_SPECIFIC (decl)->u.fn.thunk_p)
    {
#if defined ENABLE_TREE_CHECKING && (GCC_VERSION >= 2007)
      if (!just_testing)
	lang_check_failed (__FILE__, __LINE__, __FUNCTION__);
      else
#endif
	return NULL;
    }

  ptr = &DECL_LANG_SPECIFIC (decl)->u.fn.u5.cloned_function;
  if (just_testing && *ptr == NULL_TREE)
    return NULL;
  else
    return ptr;
}

/* Produce declarations for all appropriate clones of FN.  If
   UPDATE_METHOD_VEC_P is nonzero, the clones are added to the
   CLASTYPE_METHOD_VEC as well.  */

void
clone_function_decl (tree fn, int update_method_vec_p)
{
  tree clone;

  /* Avoid inappropriate cloning.  */
  if (DECL_CHAIN (fn)
      && DECL_CLONED_FUNCTION_P (DECL_CHAIN (fn)))
    return;

  if (DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P (fn))
    {
      /* For each constructor, we need two variants: an in-charge version
	 and a not-in-charge version.  */
      clone = build_clone (fn, complete_ctor_identifier);
      if (update_method_vec_p)
	add_method (DECL_CONTEXT (clone), clone, NULL_TREE);
      clone = build_clone (fn, base_ctor_identifier);
      if (update_method_vec_p)
	add_method (DECL_CONTEXT (clone), clone, NULL_TREE);
    }
  else
    {
      gcc_assert (DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P (fn));

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
	    add_method (DECL_CONTEXT (clone), clone, NULL_TREE);
	}
      clone = build_clone (fn, complete_dtor_identifier);
      if (update_method_vec_p)
	add_method (DECL_CONTEXT (clone), clone, NULL_TREE);
      clone = build_clone (fn, base_dtor_identifier);
      if (update_method_vec_p)
	add_method (DECL_CONTEXT (clone), clone, NULL_TREE);
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
adjust_clone_args (tree decl)
{
  tree clone;

  for (clone = DECL_CHAIN (decl); clone && DECL_CLONED_FUNCTION_P (clone);
       clone = DECL_CHAIN (clone))
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
	  gcc_assert (same_type_p (TREE_TYPE (decl_parms),
				   TREE_TYPE (clone_parms)));

	  if (TREE_PURPOSE (decl_parms) && !TREE_PURPOSE (clone_parms))
	    {
	      /* A default parameter has been added. Adjust the
		 clone's parameters.  */
	      tree exceptions = TYPE_RAISES_EXCEPTIONS (TREE_TYPE (clone));
	      tree attrs = TYPE_ATTRIBUTES (TREE_TYPE (clone));
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
	      type = build_method_type_directly (basetype,
						 TREE_TYPE (TREE_TYPE (clone)),
						 clone_parms);
	      if (exceptions)
		type = build_exception_variant (type, exceptions);
	      if (attrs)
		type = cp_build_type_attribute_variant (type, attrs);
	      TREE_TYPE (clone) = type;

	      clone_parms = NULL_TREE;
	      break;
	    }
	}
      gcc_assert (!clone_parms);
    }
}

/* For each of the constructors and destructors in T, create an
   in-charge and not-in-charge variant.  */

static void
clone_constructors_and_destructors (tree t)
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

/* Deduce noexcept for a destructor DTOR.  */

void
deduce_noexcept_on_destructor (tree dtor)
{
  if (!TYPE_RAISES_EXCEPTIONS (TREE_TYPE (dtor)))
    {
      tree ctx = DECL_CONTEXT (dtor);
      tree implicit_fn = implicitly_declare_fn (sfk_destructor, ctx,
						/*const_p=*/false,
						NULL, NULL);
      tree eh_spec = TYPE_RAISES_EXCEPTIONS (TREE_TYPE (implicit_fn));
      TREE_TYPE (dtor) = build_exception_variant (TREE_TYPE (dtor), eh_spec);
    }
}

/* For each destructor in T, deduce noexcept:

   12.4/3: A declaration of a destructor that does not have an
   exception-specification is implicitly considered to have the
   same exception-specification as an implicit declaration (15.4).  */

static void
deduce_noexcept_on_destructors (tree t)
{
  /* If for some reason we don't have a CLASSTYPE_METHOD_VEC, we bail
     out now.  */
  if (!CLASSTYPE_METHOD_VEC (t))
    return;

  for (tree fns = CLASSTYPE_DESTRUCTORS (t); fns; fns = OVL_NEXT (fns))
    deduce_noexcept_on_destructor (OVL_CURRENT (fns));
}

/* Subroutine of set_one_vmethod_tm_attributes.  Search base classes
   of TYPE for virtual functions which FNDECL overrides.  Return a
   mask of the tm attributes found therein.  */

static int
look_for_tm_attr_overrides (tree type, tree fndecl)
{
  tree binfo = TYPE_BINFO (type);
  tree base_binfo;
  int ix, found = 0;

  for (ix = 0; BINFO_BASE_ITERATE (binfo, ix, base_binfo); ++ix)
    {
      tree o, basetype = BINFO_TYPE (base_binfo);

      if (!TYPE_POLYMORPHIC_P (basetype))
	continue;

      o = look_for_overrides_here (basetype, fndecl);
      if (o)
	found |= tm_attr_to_mask (find_tm_attribute
				  (TYPE_ATTRIBUTES (TREE_TYPE (o))));
      else
	found |= look_for_tm_attr_overrides (basetype, fndecl);
    }

  return found;
}

/* Subroutine of set_method_tm_attributes.  Handle the checks and
   inheritance for one virtual method FNDECL.  */

static void
set_one_vmethod_tm_attributes (tree type, tree fndecl)
{
  tree tm_attr;
  int found, have;

  found = look_for_tm_attr_overrides (type, fndecl);

  /* If FNDECL doesn't actually override anything (i.e. T is the
     class that first declares FNDECL virtual), then we're done.  */
  if (found == 0)
    return;

  tm_attr = find_tm_attribute (TYPE_ATTRIBUTES (TREE_TYPE (fndecl)));
  have = tm_attr_to_mask (tm_attr);

  /* Intel STM Language Extension 3.0, Section 4.2 table 4:
     tm_pure must match exactly, otherwise no weakening of
     tm_safe > tm_callable > nothing.  */
  /* ??? The tm_pure attribute didn't make the transition to the
     multivendor language spec.  */
  if (have == TM_ATTR_PURE)
    {
      if (found != TM_ATTR_PURE)
	{
	  found &= -found;
	  goto err_override;
	}
    }
  /* If the overridden function is tm_pure, then FNDECL must be.  */
  else if (found == TM_ATTR_PURE && tm_attr)
    goto err_override;
  /* Look for base class combinations that cannot be satisfied.  */
  else if (found != TM_ATTR_PURE && (found & TM_ATTR_PURE))
    {
      found &= ~TM_ATTR_PURE;
      found &= -found;
      error_at (DECL_SOURCE_LOCATION (fndecl),
		"method overrides both %<transaction_pure%> and %qE methods",
		tm_mask_to_attr (found));
    }
  /* If FNDECL did not declare an attribute, then inherit the most
     restrictive one.  */
  else if (tm_attr == NULL)
    {
      apply_tm_attr (fndecl, tm_mask_to_attr (found & -found));
    }
  /* Otherwise validate that we're not weaker than a function
     that is being overridden.  */
  else
    {
      found &= -found;
      if (found <= TM_ATTR_CALLABLE && have > found)
	goto err_override;
    }
  return;

 err_override:
  error_at (DECL_SOURCE_LOCATION (fndecl),
	    "method declared %qE overriding %qE method",
	    tm_attr, tm_mask_to_attr (found));
}

/* For each of the methods in T, propagate a class-level tm attribute.  */

static void
set_method_tm_attributes (tree t)
{
  tree class_tm_attr, fndecl;

  /* Don't bother collecting tm attributes if transactional memory
     support is not enabled.  */
  if (!flag_tm)
    return;

  /* Process virtual methods first, as they inherit directly from the
     base virtual function and also require validation of new attributes.  */
  if (TYPE_CONTAINS_VPTR_P (t))
    {
      tree vchain;
      for (vchain = BINFO_VIRTUALS (TYPE_BINFO (t)); vchain;
	   vchain = TREE_CHAIN (vchain))
	{
	  fndecl = BV_FN (vchain);
	  if (DECL_THUNK_P (fndecl))
	    fndecl = THUNK_TARGET (fndecl);
	  set_one_vmethod_tm_attributes (t, fndecl);
	}
    }

  /* If the class doesn't have an attribute, nothing more to do.  */
  class_tm_attr = find_tm_attribute (TYPE_ATTRIBUTES (t));
  if (class_tm_attr == NULL)
    return;

  /* Any method that does not yet have a tm attribute inherits
     the one from the class.  */
  for (fndecl = TYPE_METHODS (t); fndecl; fndecl = TREE_CHAIN (fndecl))
    {
      if (!find_tm_attribute (TYPE_ATTRIBUTES (TREE_TYPE (fndecl))))
	apply_tm_attr (fndecl, class_tm_attr);
    }
}

/* Returns true iff class T has a user-defined constructor other than
   the default constructor.  */

bool
type_has_user_nondefault_constructor (tree t)
{
  tree fns;

  if (!TYPE_HAS_USER_CONSTRUCTOR (t))
    return false;

  for (fns = CLASSTYPE_CONSTRUCTORS (t); fns; fns = OVL_NEXT (fns))
    {
      tree fn = OVL_CURRENT (fns);
      if (!DECL_ARTIFICIAL (fn)
	  && (TREE_CODE (fn) == TEMPLATE_DECL
	      || (skip_artificial_parms_for (fn, DECL_ARGUMENTS (fn))
		  != NULL_TREE)))
	return true;
    }

  return false;
}

/* Returns the defaulted constructor if T has one. Otherwise, returns
   NULL_TREE.  */

tree
in_class_defaulted_default_constructor (tree t)
{
  tree fns, args;

  if (!TYPE_HAS_USER_CONSTRUCTOR (t))
    return NULL_TREE;

  for (fns = CLASSTYPE_CONSTRUCTORS (t); fns; fns = OVL_NEXT (fns))
    {
      tree fn = OVL_CURRENT (fns);

      if (DECL_DEFAULTED_IN_CLASS_P (fn))
	{
	  args = FUNCTION_FIRST_USER_PARMTYPE (fn);
	  while (args && TREE_PURPOSE (args))
	    args = TREE_CHAIN (args);
	  if (!args || args == void_list_node)
	    return fn;
	}
    }

  return NULL_TREE;
}

/* Returns true iff FN is a user-provided function, i.e. user-declared
   and not defaulted at its first declaration; or explicit, private,
   protected, or non-const.  */

bool
user_provided_p (tree fn)
{
  if (TREE_CODE (fn) == TEMPLATE_DECL)
    return true;
  else
    return (!DECL_ARTIFICIAL (fn)
	    && !(DECL_INITIALIZED_IN_CLASS_P (fn)
		 && (DECL_DEFAULTED_FN (fn) || DECL_DELETED_FN (fn))));
}

/* Returns true iff class T has a user-provided constructor.  */

bool
type_has_user_provided_constructor (tree t)
{
  tree fns;

  if (!CLASS_TYPE_P (t))
    return false;

  if (!TYPE_HAS_USER_CONSTRUCTOR (t))
    return false;

  /* This can happen in error cases; avoid crashing.  */
  if (!CLASSTYPE_METHOD_VEC (t))
    return false;

  for (fns = CLASSTYPE_CONSTRUCTORS (t); fns; fns = OVL_NEXT (fns))
    if (user_provided_p (OVL_CURRENT (fns)))
      return true;

  return false;
}

/* Returns true iff class T has a user-provided default constructor.  */

bool
type_has_user_provided_default_constructor (tree t)
{
  tree fns;

  if (!TYPE_HAS_USER_CONSTRUCTOR (t))
    return false;

  for (fns = CLASSTYPE_CONSTRUCTORS (t); fns; fns = OVL_NEXT (fns))
    {
      tree fn = OVL_CURRENT (fns);
      if (TREE_CODE (fn) == FUNCTION_DECL
	  && user_provided_p (fn)
	  && sufficient_parms_p (FUNCTION_FIRST_USER_PARMTYPE (fn)))
	return true;
    }

  return false;
}

/* TYPE is being used as a virtual base, and has a non-trivial move
   assignment.  Return true if this is due to there being a user-provided
   move assignment in TYPE or one of its subobjects; if there isn't, then
   multiple move assignment can't cause any harm.  */

bool
vbase_has_user_provided_move_assign (tree type)
{
  /* Does the type itself have a user-provided move assignment operator?  */
  for (tree fns
	 = lookup_fnfields_slot_nolazy (type, ansi_assopname (NOP_EXPR));
       fns; fns = OVL_NEXT (fns))
    {
      tree fn = OVL_CURRENT (fns);
      if (move_fn_p (fn) && user_provided_p (fn))
	return true;
    }

  /* Do any of its bases?  */
  tree binfo = TYPE_BINFO (type);
  tree base_binfo;
  for (int i = 0; BINFO_BASE_ITERATE (binfo, i, base_binfo); ++i)
    if (vbase_has_user_provided_move_assign (BINFO_TYPE (base_binfo)))
      return true;

  /* Or non-static data members?  */
  for (tree field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
    {
      if (TREE_CODE (field) == FIELD_DECL
	  && CLASS_TYPE_P (TREE_TYPE (field))
	  && vbase_has_user_provided_move_assign (TREE_TYPE (field)))
	return true;
    }

  /* Seems not.  */
  return false;
}

/* If default-initialization leaves part of TYPE uninitialized, returns
   a DECL for the field or TYPE itself (DR 253).  */

tree
default_init_uninitialized_part (tree type)
{
  tree t, r, binfo;
  int i;

  type = strip_array_types (type);
  if (!CLASS_TYPE_P (type))
    return type;
  if (type_has_user_provided_default_constructor (type))
    return NULL_TREE;
  for (binfo = TYPE_BINFO (type), i = 0;
       BINFO_BASE_ITERATE (binfo, i, t); ++i)
    {
      r = default_init_uninitialized_part (BINFO_TYPE (t));
      if (r)
	return r;
    }
  for (t = TYPE_FIELDS (type); t; t = DECL_CHAIN (t))
    if (TREE_CODE (t) == FIELD_DECL
	&& !DECL_ARTIFICIAL (t)
	&& !DECL_INITIAL (t))
      {
	r = default_init_uninitialized_part (TREE_TYPE (t));
	if (r)
	  return DECL_P (r) ? r : t;
      }

  return NULL_TREE;
}

/* Returns true iff for class T, a trivial synthesized default constructor
   would be constexpr.  */

bool
trivial_default_constructor_is_constexpr (tree t)
{
  /* A defaulted trivial default constructor is constexpr
     if there is nothing to initialize.  */
  gcc_assert (!TYPE_HAS_COMPLEX_DFLT (t));
  return is_really_empty_class (t);
}

/* Returns true iff class T has a constexpr default constructor.  */

bool
type_has_constexpr_default_constructor (tree t)
{
  tree fns;

  if (!CLASS_TYPE_P (t))
    {
      /* The caller should have stripped an enclosing array.  */
      gcc_assert (TREE_CODE (t) != ARRAY_TYPE);
      return false;
    }
  if (CLASSTYPE_LAZY_DEFAULT_CTOR (t))
    {
      if (!TYPE_HAS_COMPLEX_DFLT (t))
	return trivial_default_constructor_is_constexpr (t);
      /* Non-trivial, we need to check subobject constructors.  */
      lazily_declare_fn (sfk_constructor, t);
    }
  fns = locate_ctor (t);
  return (fns && DECL_DECLARED_CONSTEXPR_P (fns));
}

/* Returns true iff class TYPE has a virtual destructor.  */

bool
type_has_virtual_destructor (tree type)
{
  tree dtor;

  if (!CLASS_TYPE_P (type))
    return false;

  gcc_assert (COMPLETE_TYPE_P (type));
  dtor = CLASSTYPE_DESTRUCTORS (type);
  return (dtor && DECL_VIRTUAL_P (dtor));
}

/* Returns true iff class T has a move constructor.  */

bool
type_has_move_constructor (tree t)
{
  tree fns;

  if (CLASSTYPE_LAZY_MOVE_CTOR (t))
    {
      gcc_assert (COMPLETE_TYPE_P (t));
      lazily_declare_fn (sfk_move_constructor, t);
    }

  if (!CLASSTYPE_METHOD_VEC (t))
    return false;

  for (fns = CLASSTYPE_CONSTRUCTORS (t); fns; fns = OVL_NEXT (fns))
    if (move_fn_p (OVL_CURRENT (fns)))
      return true;

  return false;
}

/* Returns true iff class T has a move assignment operator.  */

bool
type_has_move_assign (tree t)
{
  tree fns;

  if (CLASSTYPE_LAZY_MOVE_ASSIGN (t))
    {
      gcc_assert (COMPLETE_TYPE_P (t));
      lazily_declare_fn (sfk_move_assignment, t);
    }

  for (fns = lookup_fnfields_slot_nolazy (t, ansi_assopname (NOP_EXPR));
       fns; fns = OVL_NEXT (fns))
    if (move_fn_p (OVL_CURRENT (fns)))
      return true;

  return false;
}

/* Returns true iff class T has a move constructor that was explicitly
   declared in the class body.  Note that this is different from
   "user-provided", which doesn't include functions that are defaulted in
   the class.  */

bool
type_has_user_declared_move_constructor (tree t)
{
  tree fns;

  if (CLASSTYPE_LAZY_MOVE_CTOR (t))
    return false;

  if (!CLASSTYPE_METHOD_VEC (t))
    return false;

  for (fns = CLASSTYPE_CONSTRUCTORS (t); fns; fns = OVL_NEXT (fns))
    {
      tree fn = OVL_CURRENT (fns);
      if (move_fn_p (fn) && !DECL_ARTIFICIAL (fn))
	return true;
    }

  return false;
}

/* Returns true iff class T has a move assignment operator that was
   explicitly declared in the class body.  */

bool
type_has_user_declared_move_assign (tree t)
{
  tree fns;

  if (CLASSTYPE_LAZY_MOVE_ASSIGN (t))
    return false;

  for (fns = lookup_fnfields_slot_nolazy (t, ansi_assopname (NOP_EXPR));
       fns; fns = OVL_NEXT (fns))
    {
      tree fn = OVL_CURRENT (fns);
      if (move_fn_p (fn) && !DECL_ARTIFICIAL (fn))
	return true;
    }

  return false;
}

/* Nonzero if we need to build up a constructor call when initializing an
   object of this class, either because it has a user-declared constructor
   or because it doesn't have a default constructor (so we need to give an
   error if no initializer is provided).  Use TYPE_NEEDS_CONSTRUCTING when
   what you care about is whether or not an object can be produced by a
   constructor (e.g. so we don't set TREE_READONLY on const variables of
   such type); use this function when what you care about is whether or not
   to try to call a constructor to create an object.  The latter case is
   the former plus some cases of constructors that cannot be called.  */

bool
type_build_ctor_call (tree t)
{
  tree inner;
  if (TYPE_NEEDS_CONSTRUCTING (t))
    return true;
  inner = strip_array_types (t);
  if (!CLASS_TYPE_P (inner) || ANON_AGGR_TYPE_P (inner))
    return false;
  if (!TYPE_HAS_DEFAULT_CONSTRUCTOR (inner))
    return true;
  if (cxx_dialect < cxx11)
    return false;
  /* A user-declared constructor might be private, and a constructor might
     be trivial but deleted.  */
  for (tree fns = lookup_fnfields_slot (inner, complete_ctor_identifier);
       fns; fns = OVL_NEXT (fns))
    {
      tree fn = OVL_CURRENT (fns);
      if (!DECL_ARTIFICIAL (fn)
	  || DECL_DELETED_FN (fn))
	return true;
    }
  return false;
}

/* Like type_build_ctor_call, but for destructors.  */

bool
type_build_dtor_call (tree t)
{
  tree inner;
  if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (t))
    return true;
  inner = strip_array_types (t);
  if (!CLASS_TYPE_P (inner) || ANON_AGGR_TYPE_P (inner)
      || !COMPLETE_TYPE_P (inner))
    return false;
  if (cxx_dialect < cxx11)
    return false;
  /* A user-declared destructor might be private, and a destructor might
     be trivial but deleted.  */
  for (tree fns = lookup_fnfields_slot (inner, complete_dtor_identifier);
       fns; fns = OVL_NEXT (fns))
    {
      tree fn = OVL_CURRENT (fns);
      if (!DECL_ARTIFICIAL (fn)
	  || DECL_DELETED_FN (fn))
	return true;
    }
  return false;
}

/* Remove all zero-width bit-fields from T.  */

static void
remove_zero_width_bit_fields (tree t)
{
  tree *fieldsp;

  fieldsp = &TYPE_FIELDS (t);
  while (*fieldsp)
    {
      if (TREE_CODE (*fieldsp) == FIELD_DECL
	  && DECL_C_BIT_FIELD (*fieldsp)
          /* We should not be confused by the fact that grokbitfield
	     temporarily sets the width of the bit field into
	     DECL_INITIAL (*fieldsp).
	     check_bitfield_decl eventually sets DECL_SIZE (*fieldsp)
	     to that width.  */
	  && integer_zerop (DECL_SIZE (*fieldsp)))
	*fieldsp = DECL_CHAIN (*fieldsp);
      else
	fieldsp = &DECL_CHAIN (*fieldsp);
    }
}

/* Returns TRUE iff we need a cookie when dynamically allocating an
   array whose elements have the indicated class TYPE.  */

static bool
type_requires_array_cookie (tree type)
{
  tree fns;
  bool has_two_argument_delete_p = false;

  gcc_assert (CLASS_TYPE_P (type));

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
      /* Do not consider this function if its second argument is an
	 ellipsis.  */
      if (!second_parm)
	continue;
      /* Otherwise, if we have a two-argument function and the second
	 argument is `size_t', it will be the usual deallocation
	 function -- unless there is one-argument function, too.  */
      if (TREE_CHAIN (second_parm) == void_list_node
	  && same_type_p (TREE_VALUE (second_parm), size_type_node))
	has_two_argument_delete_p = true;
    }

  return has_two_argument_delete_p;
}

/* Finish computing the `literal type' property of class type T.

   At this point, we have already processed base classes and
   non-static data members.  We need to check whether the copy
   constructor is trivial, the destructor is trivial, and there
   is a trivial default constructor or at least one constexpr
   constructor other than the copy constructor.  */

static void
finalize_literal_type_property (tree t)
{
  tree fn;

  if (cxx_dialect < cxx11
      || TYPE_HAS_NONTRIVIAL_DESTRUCTOR (t))
    CLASSTYPE_LITERAL_P (t) = false;
  else if (CLASSTYPE_LITERAL_P (t) && !TYPE_HAS_TRIVIAL_DFLT (t)
	   && CLASSTYPE_NON_AGGREGATE (t)
	   && !TYPE_HAS_CONSTEXPR_CTOR (t))
    CLASSTYPE_LITERAL_P (t) = false;

  if (!CLASSTYPE_LITERAL_P (t))
    for (fn = TYPE_METHODS (t); fn; fn = DECL_CHAIN (fn))
      if (DECL_DECLARED_CONSTEXPR_P (fn)
	  && TREE_CODE (fn) != TEMPLATE_DECL
	  && DECL_NONSTATIC_MEMBER_FUNCTION_P (fn)
	  && !DECL_CONSTRUCTOR_P (fn))
	{
	  DECL_DECLARED_CONSTEXPR_P (fn) = false;
	  if (!DECL_GENERATED_P (fn))
	    {
	      error ("enclosing class of constexpr non-static member "
		     "function %q+#D is not a literal type", fn);
	      explain_non_literal_class (t);
	    }
	}
}

/* T is a non-literal type used in a context which requires a constant
   expression.  Explain why it isn't literal.  */

void
explain_non_literal_class (tree t)
{
  static struct pointer_set_t *diagnosed;

  if (!CLASS_TYPE_P (t))
    return;
  t = TYPE_MAIN_VARIANT (t);

  if (diagnosed == NULL)
    diagnosed = pointer_set_create ();
  if (pointer_set_insert (diagnosed, t) != 0)
    /* Already explained.  */
    return;

  inform (0, "%q+T is not literal because:", t);
  if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (t))
    inform (0, "  %q+T has a non-trivial destructor", t);
  else if (CLASSTYPE_NON_AGGREGATE (t)
	   && !TYPE_HAS_TRIVIAL_DFLT (t)
	   && !TYPE_HAS_CONSTEXPR_CTOR (t))
    {
      inform (0, "  %q+T is not an aggregate, does not have a trivial "
	      "default constructor, and has no constexpr constructor that "
	      "is not a copy or move constructor", t);
      if (TYPE_HAS_DEFAULT_CONSTRUCTOR (t)
	  && !type_has_user_provided_default_constructor (t))
	{
	  /* Note that we can't simply call locate_ctor because when the
	     constructor is deleted it just returns NULL_TREE.  */
	  tree fns;
	  for (fns = CLASSTYPE_CONSTRUCTORS (t); fns; fns = OVL_NEXT (fns))
	    {
	      tree fn = OVL_CURRENT (fns);
	      tree parms = TYPE_ARG_TYPES (TREE_TYPE (fn));

	      parms = skip_artificial_parms_for (fn, parms);

	      if (sufficient_parms_p (parms))
		{
		  if (DECL_DELETED_FN (fn))
		    maybe_explain_implicit_delete (fn);
		  else
		    explain_invalid_constexpr_fn (fn);
		  break;
		}
	    }
	}
    }
  else
    {
      tree binfo, base_binfo, field; int i;
      for (binfo = TYPE_BINFO (t), i = 0;
	   BINFO_BASE_ITERATE (binfo, i, base_binfo); i++)
	{
	  tree basetype = TREE_TYPE (base_binfo);
	  if (!CLASSTYPE_LITERAL_P (basetype))
	    {
	      inform (0, "  base class %qT of %q+T is non-literal",
		      basetype, t);
	      explain_non_literal_class (basetype);
	      return;
	    }
	}
      for (field = TYPE_FIELDS (t); field; field = TREE_CHAIN (field))
	{
	  tree ftype;
	  if (TREE_CODE (field) != FIELD_DECL)
	    continue;
	  ftype = TREE_TYPE (field);
	  if (!literal_type_p (ftype))
	    {
	      inform (0, "  non-static data member %q+D has "
		      "non-literal type", field);
	      if (CLASS_TYPE_P (ftype))
		explain_non_literal_class (ftype);
	    }
	}
    }
}

/* Check the validity of the bases and members declared in T.  Add any
   implicitly-generated functions (like copy-constructors and
   assignment operators).  Compute various flag bits (like
   CLASSTYPE_NON_LAYOUT_POD_T) for T.  This routine works purely at the C++
   level: i.e., independently of the ABI in use.  */

static void
check_bases_and_members (tree t)
{
  /* Nonzero if the implicitly generated copy constructor should take
     a non-const reference argument.  */
  int cant_have_const_ctor;
  /* Nonzero if the implicitly generated assignment operator
     should take a non-const reference argument.  */
  int no_const_asn_ref;
  tree access_decls;
  bool saved_complex_asn_ref;
  bool saved_nontrivial_dtor;
  tree fn;

  /* By default, we use const reference arguments and generate default
     constructors.  */
  cant_have_const_ctor = 0;
  no_const_asn_ref = 0;

  /* Check all the base-classes.  */
  check_bases (t, &cant_have_const_ctor,
	       &no_const_asn_ref);

  /* Deduce noexcept on destructors.  This needs to happen after we've set
     triviality flags appropriately for our bases.  */
  if (cxx_dialect >= cxx11)
    deduce_noexcept_on_destructors (t);

  /* Check all the method declarations.  */
  check_methods (t);

  /* Save the initial values of these flags which only indicate whether
     or not the class has user-provided functions.  As we analyze the
     bases and members we can set these flags for other reasons.  */
  saved_complex_asn_ref = TYPE_HAS_COMPLEX_COPY_ASSIGN (t);
  saved_nontrivial_dtor = TYPE_HAS_NONTRIVIAL_DESTRUCTOR (t);

  /* Check all the data member declarations.  We cannot call
     check_field_decls until we have called check_bases check_methods,
     as check_field_decls depends on TYPE_HAS_NONTRIVIAL_DESTRUCTOR
     being set appropriately.  */
  check_field_decls (t, &access_decls,
		     &cant_have_const_ctor,
		     &no_const_asn_ref);

  /* A nearly-empty class has to be vptr-containing; a nearly empty
     class contains just a vptr.  */
  if (!TYPE_CONTAINS_VPTR_P (t))
    CLASSTYPE_NEARLY_EMPTY_P (t) = 0;

  /* Do some bookkeeping that will guide the generation of implicitly
     declared member functions.  */
  TYPE_HAS_COMPLEX_COPY_CTOR (t) |= TYPE_CONTAINS_VPTR_P (t);
  TYPE_HAS_COMPLEX_MOVE_CTOR (t) |= TYPE_CONTAINS_VPTR_P (t);
  /* We need to call a constructor for this class if it has a
     user-provided constructor, or if the default constructor is going
     to initialize the vptr.  (This is not an if-and-only-if;
     TYPE_NEEDS_CONSTRUCTING is set elsewhere if bases or members
     themselves need constructing.)  */
  TYPE_NEEDS_CONSTRUCTING (t)
    |= (type_has_user_provided_constructor (t) || TYPE_CONTAINS_VPTR_P (t));
  /* [dcl.init.aggr]

     An aggregate is an array or a class with no user-provided
     constructors ... and no virtual functions.  

     Again, other conditions for being an aggregate are checked
     elsewhere.  */
  CLASSTYPE_NON_AGGREGATE (t)
    |= (type_has_user_provided_constructor (t) || TYPE_POLYMORPHIC_P (t));
  /* This is the C++98/03 definition of POD; it changed in C++0x, but we
     retain the old definition internally for ABI reasons.  */
  CLASSTYPE_NON_LAYOUT_POD_P (t)
    |= (CLASSTYPE_NON_AGGREGATE (t)
	|| saved_nontrivial_dtor || saved_complex_asn_ref);
  CLASSTYPE_NON_STD_LAYOUT (t) |= TYPE_CONTAINS_VPTR_P (t);
  TYPE_HAS_COMPLEX_COPY_ASSIGN (t) |= TYPE_CONTAINS_VPTR_P (t);
  TYPE_HAS_COMPLEX_MOVE_ASSIGN (t) |= TYPE_CONTAINS_VPTR_P (t);
  TYPE_HAS_COMPLEX_DFLT (t) |= TYPE_CONTAINS_VPTR_P (t);

  /* If the class has no user-declared constructor, but does have
     non-static const or reference data members that can never be
     initialized, issue a warning.  */
  if (warn_uninitialized
      /* Classes with user-declared constructors are presumed to
	 initialize these members.  */
      && !TYPE_HAS_USER_CONSTRUCTOR (t)
      /* Aggregates can be initialized with brace-enclosed
	 initializers.  */
      && CLASSTYPE_NON_AGGREGATE (t))
    {
      tree field;

      for (field = TYPE_FIELDS (t); field; field = DECL_CHAIN (field))
	{
	  tree type;

	  if (TREE_CODE (field) != FIELD_DECL
	      || DECL_INITIAL (field) != NULL_TREE)
	    continue;

	  type = TREE_TYPE (field);
	  if (TREE_CODE (type) == REFERENCE_TYPE)
	    warning (OPT_Wuninitialized, "non-static reference %q+#D "
		     "in class without a constructor", field);
	  else if (CP_TYPE_CONST_P (type)
		   && (!CLASS_TYPE_P (type)
		       || !TYPE_HAS_DEFAULT_CONSTRUCTOR (type)))
	    warning (OPT_Wuninitialized, "non-static const member %q+#D "
		     "in class without a constructor", field);
	}
    }

  /* Synthesize any needed methods.  */
  add_implicitly_declared_members (t, &access_decls,
				   cant_have_const_ctor,
				   no_const_asn_ref);

  /* Check defaulted declarations here so we have cant_have_const_ctor
     and don't need to worry about clones.  */
  for (fn = TYPE_METHODS (t); fn; fn = DECL_CHAIN (fn))
    if (!DECL_ARTIFICIAL (fn) && DECL_DEFAULTED_IN_CLASS_P (fn))
      {
	int copy = copy_fn_p (fn);
	if (copy > 0)
	  {
	    bool imp_const_p
	      = (DECL_CONSTRUCTOR_P (fn) ? !cant_have_const_ctor
		 : !no_const_asn_ref);
	    bool fn_const_p = (copy == 2);

	    if (fn_const_p && !imp_const_p)
	      /* If the function is defaulted outside the class, we just
		 give the synthesis error.  */
	      error ("%q+D declared to take const reference, but implicit "
		     "declaration would take non-const", fn);
	  }
	defaulted_late_check (fn);
      }

  if (LAMBDA_TYPE_P (t))
    {
      /* "The closure type associated with a lambda-expression has a deleted
	 default constructor and a deleted copy assignment operator."  */
      TYPE_NEEDS_CONSTRUCTING (t) = 1;
      TYPE_HAS_COMPLEX_DFLT (t) = 1;
      TYPE_HAS_COMPLEX_COPY_ASSIGN (t) = 1;
      CLASSTYPE_LAZY_MOVE_ASSIGN (t) = 0;

      /* "This class type is not an aggregate."  */
      CLASSTYPE_NON_AGGREGATE (t) = 1;
    }

  /* Compute the 'literal type' property before we
     do anything with non-static member functions.  */
  finalize_literal_type_property (t);

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
create_vtable_ptr (tree t, tree* virtuals_p)
{
  tree fn;

  /* Collect the virtual functions declared in T.  */
  for (fn = TYPE_METHODS (t); fn; fn = DECL_CHAIN (fn))
    if (DECL_VINDEX (fn) && !DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P (fn)
	&& TREE_CODE (DECL_VINDEX (fn)) != INTEGER_CST)
      {
	tree new_virtual = make_node (TREE_LIST);

	BV_FN (new_virtual) = fn;
	BV_DELTA (new_virtual) = integer_zero_node;
	BV_VCALL_INDEX (new_virtual) = NULL_TREE;

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
	 `vtable_entry_type (*)[N]' where N is the number of virtual
	 functions.  However, that would require the vtable pointer in
	 base classes to have a different type than the vtable pointer
	 in derived classes.  We could make that happen, but that
	 still wouldn't solve all the problems.  In particular, the
	 type-based alias analysis code would decide that assignments
	 to the base class vtable pointer can't alias assignments to
	 the derived class vtable pointer, since they have different
	 types.  Thus, in a derived class destructor, where the base
	 class constructor was inlined, we could generate bad code for
	 setting up the vtable pointer.

	 Therefore, we use one type for all vtable pointers.  We still
	 use a type-correct type; it's just doesn't indicate the array
	 bounds.  That's better than using `void*' or some such; it's
	 cleaner, and it let's the alias analysis code know that these
	 stores cannot alias stores to void*!  */
      tree field;

      field = build_decl (input_location, 
			  FIELD_DECL, get_vfield_name (t), vtbl_ptr_type_node);
      DECL_VIRTUAL_P (field) = 1;
      DECL_ARTIFICIAL (field) = 1;
      DECL_FIELD_CONTEXT (field) = t;
      DECL_FCONTEXT (field) = t;
      if (TYPE_PACKED (t))
	DECL_PACKED (field) = 1;

      TYPE_VFIELD (t) = field;

      /* This class is non-empty.  */
      CLASSTYPE_EMPTY_P (t) = 0;

      return field;
    }

  return NULL_TREE;
}

/* Add OFFSET to all base types of BINFO which is a base in the
   hierarchy dominated by T.

   OFFSET, which is a type offset, is number of bytes.  */

static void
propagate_binfo_offsets (tree binfo, tree offset)
{
  int i;
  tree primary_binfo;
  tree base_binfo;

  /* Update BINFO's offset.  */
  BINFO_OFFSET (binfo)
    = convert (sizetype,
	       size_binop (PLUS_EXPR,
			   convert (ssizetype, BINFO_OFFSET (binfo)),
			   offset));

  /* Find the primary base class.  */
  primary_binfo = get_primary_binfo (binfo);

  if (primary_binfo && BINFO_INHERITANCE_CHAIN (primary_binfo) == binfo)
    propagate_binfo_offsets (primary_binfo, offset);

  /* Scan all of the bases, pushing the BINFO_OFFSET adjust
     downwards.  */
  for (i = 0; BINFO_BASE_ITERATE (binfo, i, base_binfo); ++i)
    {
      /* Don't do the primary base twice.  */
      if (base_binfo == primary_binfo)
	continue;

      if (BINFO_VIRTUAL_P (base_binfo))
	continue;

      propagate_binfo_offsets (base_binfo, offset);
    }
}

/* Set BINFO_OFFSET for all of the virtual bases for RLI->T.  Update
   TYPE_ALIGN and TYPE_SIZE for T.  OFFSETS gives the location of
   empty subobjects of T.  */

static void
layout_virtual_bases (record_layout_info rli, splay_tree offsets)
{
  tree vbase;
  tree t = rli->t;
  bool first_vbase = true;
  tree *next_field;

  if (BINFO_N_BASE_BINFOS (TYPE_BINFO (t)) == 0)
    return;

  if (!abi_version_at_least(2))
    {
      /* In G++ 3.2, we incorrectly rounded the size before laying out
	 the virtual bases.  */
      finish_record_layout (rli, /*free_p=*/false);
#ifdef STRUCTURE_SIZE_BOUNDARY
      /* Packed structures don't need to have minimum size.  */
      if (! TYPE_PACKED (t))
	TYPE_ALIGN (t) = MAX (TYPE_ALIGN (t), (unsigned) STRUCTURE_SIZE_BOUNDARY);
#endif
      rli->offset = TYPE_SIZE_UNIT (t);
      rli->bitpos = bitsize_zero_node;
      rli->record_align = TYPE_ALIGN (t);
    }

  /* Find the last field.  The artificial fields created for virtual
     bases will go after the last extant field to date.  */
  next_field = &TYPE_FIELDS (t);
  while (*next_field)
    next_field = &DECL_CHAIN (*next_field);

  /* Go through the virtual bases, allocating space for each virtual
     base that is not already a primary base class.  These are
     allocated in inheritance graph order.  */
  for (vbase = TYPE_BINFO (t); vbase; vbase = TREE_CHAIN (vbase))
    {
      if (!BINFO_VIRTUAL_P (vbase))
	continue;

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
			       round_up_loc (input_location,
					 CLASSTYPE_SIZE (t),
					 CLASSTYPE_ALIGN (basetype)),
			       bitsize_unit_node),
		   BINFO_OFFSET (vbase))))
	    warning (OPT_Wabi,
		     "offset of virtual base %qT is not ABI-compliant and "
		     "may change in a future version of GCC",
		     basetype);

	  first_vbase = false;
	}
    }
}

/* Returns the offset of the byte just past the end of the base class
   BINFO.  */

static tree
end_of_base (tree binfo)
{
  tree size;

  if (!CLASSTYPE_AS_BASE (BINFO_TYPE (binfo)))
    size = TYPE_SIZE_UNIT (char_type_node);
  else if (is_empty_class (BINFO_TYPE (binfo)))
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
end_of_class (tree t, int include_virtuals_p)
{
  tree result = size_zero_node;
  vec<tree, va_gc> *vbases;
  tree binfo;
  tree base_binfo;
  tree offset;
  int i;

  for (binfo = TYPE_BINFO (t), i = 0;
       BINFO_BASE_ITERATE (binfo, i, base_binfo); ++i)
    {
      if (!include_virtuals_p
	  && BINFO_VIRTUAL_P (base_binfo)
	  && (!BINFO_PRIMARY_P (base_binfo)
	      || BINFO_INHERITANCE_CHAIN (base_binfo) != TYPE_BINFO (t)))
	continue;

      offset = end_of_base (base_binfo);
      if (INT_CST_LT_UNSIGNED (result, offset))
	result = offset;
    }

  /* G++ 3.2 did not check indirect virtual bases.  */
  if (abi_version_at_least (2) && include_virtuals_p)
    for (vbases = CLASSTYPE_VBASECLASSES (t), i = 0;
	 vec_safe_iterate (vbases, i, &base_binfo); i++)
      {
	offset = end_of_base (base_binfo);
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
warn_about_ambiguous_bases (tree t)
{
  int i;
  vec<tree, va_gc> *vbases;
  tree basetype;
  tree binfo;
  tree base_binfo;

  /* If there are no repeated bases, nothing can be ambiguous.  */
  if (!CLASSTYPE_REPEATED_BASE_P (t))
    return;

  /* Check direct bases.  */
  for (binfo = TYPE_BINFO (t), i = 0;
       BINFO_BASE_ITERATE (binfo, i, base_binfo); ++i)
    {
      basetype = BINFO_TYPE (base_binfo);

      if (!uniquely_derived_from_p (basetype, t))
	warning (0, "direct base %qT inaccessible in %qT due to ambiguity",
		 basetype, t);
    }

  /* Check for ambiguous virtual bases.  */
  if (extra_warnings)
    for (vbases = CLASSTYPE_VBASECLASSES (t), i = 0;
	 vec_safe_iterate (vbases, i, &binfo); i++)
      {
	basetype = BINFO_TYPE (binfo);

	if (!uniquely_derived_from_p (basetype, t))
	  warning (OPT_Wextra, "virtual base %qT inaccessible in %qT due "
		   "to ambiguity", basetype, t);
      }
}

/* Compare two INTEGER_CSTs K1 and K2.  */

static int
splay_tree_compare_integer_csts (splay_tree_key k1, splay_tree_key k2)
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
      if (!abi_version_at_least (2))
	/* In version 1 of the ABI, the size of a class that ends with
	   a bitfield was not rounded up to a whole multiple of a
	   byte.  Because rli_size_unit_so_far returns only the number
	   of fully allocated bytes, any extra bits were not included
	   in the size.  */
	rli->bitpos = round_down (rli->bitpos, BITS_PER_UNIT);
      else
	/* The size should have been rounded to a whole byte.  */
	gcc_assert (tree_int_cst_equal
		    (rli->bitpos, round_down (rli->bitpos, BITS_PER_UNIT)));
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
  /* True if the last field laid out was a bit-field.  */
  bool last_field_was_bitfield = false;
  /* The location at which the next field should be inserted.  */
  tree *next_field;
  /* T, as a base class.  */
  tree base_t;

  /* Keep track of the first non-static data member.  */
  non_static_data_members = TYPE_FIELDS (t);

  /* Start laying out the record.  */
  rli = start_record_layout (t);

  /* Mark all the primary bases in the hierarchy.  */
  determine_primary_bases (t);

  /* Create a pointer to our virtual function table.  */
  vptr = create_vtable_ptr (t, virtuals_p);

  /* The vptr is always the first thing in the class.  */
  if (vptr)
    {
      DECL_CHAIN (vptr) = TYPE_FIELDS (t);
      TYPE_FIELDS (t) = vptr;
      next_field = &DECL_CHAIN (vptr);
      place_field (rli, vptr);
    }
  else
    next_field = &TYPE_FIELDS (t);

  /* Build FIELD_DECLs for all of the non-virtual base-types.  */
  empty_base_offsets = splay_tree_new (splay_tree_compare_integer_csts,
				       NULL, NULL);
  build_base_fields (rli, empty_base_offsets, next_field);

  /* Layout the non-static data members.  */
  for (field = non_static_data_members; field; field = DECL_CHAIN (field))
    {
      tree type;
      tree padding;

      /* We still pass things that aren't non-static data members to
	 the back end, in case it wants to do something with them.  */
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
	  if (VAR_P (field))
	    {
	      maybe_register_incomplete_var (field);
	      /* The visibility of static data members is determined
		 at their point of declaration, not their point of
		 definition.  */
	      determine_visibility (field);
	    }
	  continue;
	}

      type = TREE_TYPE (field);
      if (type == error_mark_node)
	continue;

      padding = NULL_TREE;

      /* If this field is a bit-field whose width is greater than its
	 type, then there are some special rules for allocating
	 it.  */
      if (DECL_C_BIT_FIELD (field)
	  && INT_CST_LT (TYPE_SIZE (type), DECL_SIZE (field)))
	{
	  unsigned int itk;
	  tree integer_type;
	  bool was_unnamed_p = false;
	  /* We must allocate the bits as if suitably aligned for the
	     longest integer type that fits in this many bits.  type
	     of the field.  Then, we are supposed to use the left over
	     bits as additional padding.  */
	  for (itk = itk_char; itk != itk_none; ++itk)
	    if (integer_types[itk] != NULL_TREE
		&& (INT_CST_LT (size_int (MAX_FIXED_MODE_SIZE),
				TYPE_SIZE (integer_types[itk]))
		    || INT_CST_LT (DECL_SIZE (field),
				   TYPE_SIZE (integer_types[itk]))))
	      break;

	  /* ITK now indicates a type that is too large for the
	     field.  We have to back up by one to find the largest
	     type that fits.  */
	  do
	  {
            --itk;
	    integer_type = integer_types[itk];
	  } while (itk > 0 && integer_type == NULL_TREE);

	  /* Figure out how much additional padding is required.  GCC
	     3.2 always created a padding field, even if it had zero
	     width.  */
	  if (!abi_version_at_least (2)
	      || INT_CST_LT (TYPE_SIZE (integer_type), DECL_SIZE (field)))
	    {
	      if (abi_version_at_least (2) && TREE_CODE (t) == UNION_TYPE)
		/* In a union, the padding field must have the full width
		   of the bit-field; all fields start at offset zero.  */
		padding = DECL_SIZE (field);
	      else
		{
		  if (TREE_CODE (t) == UNION_TYPE)
		    warning (OPT_Wabi, "size assigned to %qT may not be "
			     "ABI-compliant and may change in a future "
			     "version of GCC",
			     t);
		  padding = size_binop (MINUS_EXPR, DECL_SIZE (field),
					TYPE_SIZE (integer_type));
		}
	    }
#ifdef PCC_BITFIELD_TYPE_MATTERS
	  /* An unnamed bitfield does not normally affect the
	     alignment of the containing class on a target where
	     PCC_BITFIELD_TYPE_MATTERS.  But, the C++ ABI does not
	     make any exceptions for unnamed bitfields when the
	     bitfields are longer than their types.  Therefore, we
	     temporarily give the field a name.  */
	  if (PCC_BITFIELD_TYPE_MATTERS && !DECL_NAME (field))
	    {
	      was_unnamed_p = true;
	      DECL_NAME (field) = make_anon_name ();
	    }
#endif
	  DECL_SIZE (field) = TYPE_SIZE (integer_type);
	  DECL_ALIGN (field) = TYPE_ALIGN (integer_type);
	  DECL_USER_ALIGN (field) = TYPE_USER_ALIGN (integer_type);
	  layout_nonempty_base_or_field (rli, field, NULL_TREE,
					 empty_base_offsets);
	  if (was_unnamed_p)
	    DECL_NAME (field) = NULL_TREE;
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
	    warning (OPT_Wabi,
		     "the offset of %qD may not be ABI-compliant and may "
		     "change in a future version of GCC", field);
	}
      else
	layout_nonempty_base_or_field (rli, field, NULL_TREE,
				       empty_base_offsets);

      /* Remember the location of any empty classes in FIELD.  */
      if (abi_version_at_least (2))
	record_subobject_offsets (TREE_TYPE (field),
				  byte_position(field),
				  empty_base_offsets,
				  /*is_data_member=*/true);

      /* If a bit-field does not immediately follow another bit-field,
	 and yet it starts in the middle of a byte, we have failed to
	 comply with the ABI.  */
      if (warn_abi
	  && DECL_C_BIT_FIELD (field)
	  /* The TREE_NO_WARNING flag gets set by Objective-C when
	     laying out an Objective-C class.  The ObjC ABI differs
	     from the C++ ABI, and so we do not want a warning
	     here.  */
	  && !TREE_NO_WARNING (field)
	  && !last_field_was_bitfield
	  && !integer_zerop (size_binop (TRUNC_MOD_EXPR,
					 DECL_FIELD_BIT_OFFSET (field),
					 bitsize_unit_node)))
	warning (OPT_Wabi, "offset of %q+D is not ABI-compliant and may "
		 "change in a future version of GCC", field);

      /* G++ used to use DECL_FIELD_OFFSET as if it were the byte
	 offset of the field.  */
      if (warn_abi
	  && !abi_version_at_least (2)
	  && !tree_int_cst_equal (DECL_FIELD_OFFSET (field),
				  byte_position (field))
	  && contains_empty_class_p (TREE_TYPE (field)))
	warning (OPT_Wabi, "%q+D contains empty classes which may cause base "
		 "classes to be placed at different locations in a "
		 "future version of GCC", field);

      /* The middle end uses the type of expressions to determine the
	 possible range of expression values.  In order to optimize
	 "x.i > 7" to "false" for a 2-bit bitfield "i", the middle end
	 must be made aware of the width of "i", via its type.

	 Because C++ does not have integer types of arbitrary width,
	 we must (for the purposes of the front end) convert from the
	 type assigned here to the declared type of the bitfield
	 whenever a bitfield expression is used as an rvalue.
	 Similarly, when assigning a value to a bitfield, the value
	 must be converted to the type given the bitfield here.  */
      if (DECL_C_BIT_FIELD (field))
	{
	  unsigned HOST_WIDE_INT width;
	  tree ftype = TREE_TYPE (field);
	  width = tree_to_uhwi (DECL_SIZE (field));
	  if (width != TYPE_PRECISION (ftype))
	    {
	      TREE_TYPE (field)
		= c_build_bitfield_integer_type (width,
						 TYPE_UNSIGNED (ftype));
	      TREE_TYPE (field)
		= cp_build_qualified_type (TREE_TYPE (field),
					   cp_type_quals (ftype));
	    }
	}

      /* If we needed additional padding after this field, add it
	 now.  */
      if (padding)
	{
	  tree padding_field;

	  padding_field = build_decl (input_location,
				      FIELD_DECL,
				      NULL_TREE,
				      char_type_node);
	  DECL_BIT_FIELD (padding_field) = 1;
	  DECL_SIZE (padding_field) = padding;
	  DECL_CONTEXT (padding_field) = t;
	  DECL_ARTIFICIAL (padding_field) = 1;
	  DECL_IGNORED_P (padding_field) = 1;
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
      rli->bitpos = round_up_loc (input_location, rli->bitpos, BITS_PER_UNIT);
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
     make_class_type for this version; this is an artificial type.  For
     a POD type, we just reuse T.  */
  if (CLASSTYPE_NON_LAYOUT_POD_P (t) || CLASSTYPE_EMPTY_P (t))
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
	    warning (OPT_Wabi,
		     "layout of classes derived from empty class %qT "
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
      for (field = TYPE_FIELDS (t); field; field = DECL_CHAIN (field))
	if (TREE_CODE (field) == FIELD_DECL)
	  {
	    *next_field = build_decl (input_location,
				      FIELD_DECL,
				      DECL_NAME (field),
				      TREE_TYPE (field));
	    DECL_CONTEXT (*next_field) = base_t;
	    DECL_FIELD_OFFSET (*next_field) = DECL_FIELD_OFFSET (field);
	    DECL_FIELD_BIT_OFFSET (*next_field)
	      = DECL_FIELD_BIT_OFFSET (field);
	    DECL_SIZE (*next_field) = DECL_SIZE (field);
	    DECL_MODE (*next_field) = DECL_MODE (field);
	    next_field = &DECL_CHAIN (*next_field);
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
		 build_decl (input_location,
			     FIELD_DECL, NULL_TREE, char_type_node));

  /* If this is a non-POD, declaring it packed makes a difference to how it
     can be used as a field; don't let finalize_record_size undo it.  */
  if (TYPE_PACKED (t) && !layout_pod_type_p (t))
    rli->packed_maybe_necessary = true;

  /* Let the back end lay out the type.  */
  finish_record_layout (rli, /*free_p=*/true);

  if (TYPE_SIZE_UNIT (t)
      && TREE_CODE (TYPE_SIZE_UNIT (t)) == INTEGER_CST
      && !TREE_OVERFLOW (TYPE_SIZE_UNIT (t))
      && !valid_constant_size_p (TYPE_SIZE_UNIT (t)))
    error ("type %qT is too large", t);

  /* Warn about bases that can't be talked about due to ambiguity.  */
  warn_about_ambiguous_bases (t);

  /* Now that we're done with layout, give the base fields the real types.  */
  for (field = TYPE_FIELDS (t); field; field = DECL_CHAIN (field))
    if (DECL_ARTIFICIAL (field) && IS_FAKE_BASE_TYPE (TREE_TYPE (field)))
      TREE_TYPE (field) = TYPE_CONTEXT (TREE_TYPE (field));

  /* Clean up.  */
  splay_tree_delete (empty_base_offsets);

  if (CLASSTYPE_EMPTY_P (t)
      && tree_int_cst_lt (sizeof_biggest_empty_class,
			  TYPE_SIZE_UNIT (t)))
    sizeof_biggest_empty_class = TYPE_SIZE_UNIT (t);
}

/* Determine the "key method" for the class type indicated by TYPE,
   and set CLASSTYPE_KEY_METHOD accordingly.  */

void
determine_key_method (tree type)
{
  tree method;

  if (TYPE_FOR_JAVA (type)
      || processing_template_decl
      || CLASSTYPE_TEMPLATE_INSTANTIATION (type)
      || CLASSTYPE_INTERFACE_KNOWN (type))
    return;

  /* The key method is the first non-pure virtual function that is not
     inline at the point of class definition.  On some targets the
     key function may not be inline; those targets should not call
     this function until the end of the translation unit.  */
  for (method = TYPE_METHODS (type); method != NULL_TREE;
       method = DECL_CHAIN (method))
    if (DECL_VINDEX (method) != NULL_TREE
	&& ! DECL_DECLARED_INLINE_P (method)
	&& ! DECL_PURE_VIRTUAL_P (method))
      {
	CLASSTYPE_KEY_METHOD (type) = method;
	break;
      }

  return;
}


/* Allocate and return an instance of struct sorted_fields_type with
   N fields.  */

static struct sorted_fields_type *
sorted_fields_type_new (int n)
{
  struct sorted_fields_type *sft;
  sft = ggc_alloc_sorted_fields_type (sizeof (struct sorted_fields_type)
				      + n * sizeof (tree));
  sft->len = n;

  return sft;
}


/* Perform processing required when the definition of T (a class type)
   is complete.  */

void
finish_struct_1 (tree t)
{
  tree x;
  /* A TREE_LIST.  The TREE_VALUE of each node is a FUNCTION_DECL.  */
  tree virtuals = NULL_TREE;

  if (COMPLETE_TYPE_P (t))
    {
      gcc_assert (MAYBE_CLASS_TYPE_P (t));
      error ("redefinition of %q#T", t);
      popclass ();
      return;
    }

  /* If this type was previously laid out as a forward reference,
     make sure we lay it out again.  */
  TYPE_SIZE (t) = NULL_TREE;
  CLASSTYPE_PRIMARY_BINFO (t) = NULL_TREE;

  /* Make assumptions about the class; we'll reset the flags if
     necessary.  */
  CLASSTYPE_EMPTY_P (t) = 1;
  CLASSTYPE_NEARLY_EMPTY_P (t) = 1;
  CLASSTYPE_CONTAINS_EMPTY_CLASS_P (t) = 0;
  CLASSTYPE_LITERAL_P (t) = true;

  /* Do end-of-class semantic processing: checking the validity of the
     bases and members and add implicitly generated methods.  */
  check_bases_and_members (t);

  /* Find the key method.  */
  if (TYPE_CONTAINS_VPTR_P (t))
    {
      /* The Itanium C++ ABI permits the key method to be chosen when
	 the class is defined -- even though the key method so
	 selected may later turn out to be an inline function.  On
	 some systems (such as ARM Symbian OS) the key method cannot
	 be determined until the end of the translation unit.  On such
	 systems, we leave CLASSTYPE_KEY_METHOD set to NULL, which
	 will cause the class to be added to KEYED_CLASSES.  Then, in
	 finish_file we will determine the key method.  */
      if (targetm.cxx.key_method_may_be_inline ())
	determine_key_method (t);

      /* If a polymorphic class has no key method, we may emit the vtable
	 in every translation unit where the class definition appears.  */
      if (CLASSTYPE_KEY_METHOD (t) == NULL_TREE)
	keyed_classes = tree_cons (NULL_TREE, t, keyed_classes);
    }

  /* Layout the class itself.  */
  layout_class_type (t, &virtuals);
  if (CLASSTYPE_AS_BASE (t) != t)
    /* We use the base type for trivial assignments, and hence it
       needs a mode.  */
    compute_record_mode (CLASSTYPE_AS_BASE (t));

  virtuals = modify_all_vtables (t, nreverse (virtuals));

  /* If necessary, create the primary vtable for this class.  */
  if (virtuals || TYPE_CONTAINS_VPTR_P (t))
    {
      /* We must enter these virtuals into the table.  */
      if (!CLASSTYPE_HAS_PRIMARY_BASE_P (t))
	build_primary_vtable (NULL_TREE, t);
      else if (! BINFO_NEW_VTABLE_MARKED (TYPE_BINFO (t)))
	/* Here we know enough to change the type of our virtual
	   function table, but we will wait until later this function.  */
	build_primary_vtable (CLASSTYPE_PRIMARY_BINFO (t), t);

      /* If we're warning about ABI tags, check the types of the new
	 virtual functions.  */
      if (warn_abi_tag)
	for (tree v = virtuals; v; v = TREE_CHAIN (v))
	  check_abi_tags (t, TREE_VALUE (v));
    }

  if (TYPE_CONTAINS_VPTR_P (t))
    {
      int vindex;
      tree fn;

      if (BINFO_VTABLE (TYPE_BINFO (t)))
	gcc_assert (DECL_VIRTUAL_P (BINFO_VTABLE (TYPE_BINFO (t))));
      if (!CLASSTYPE_HAS_PRIMARY_BASE_P (t))
	gcc_assert (BINFO_VIRTUALS (TYPE_BINFO (t)) == NULL_TREE);

      /* Add entries for virtual functions introduced by this class.  */
      BINFO_VIRTUALS (TYPE_BINFO (t))
	= chainon (BINFO_VIRTUALS (TYPE_BINFO (t)), virtuals);

      /* Set DECL_VINDEX for all functions declared in this class.  */
      for (vindex = 0, fn = BINFO_VIRTUALS (TYPE_BINFO (t));
	   fn;
	   fn = TREE_CHAIN (fn),
	     vindex += (TARGET_VTABLE_USES_DESCRIPTORS
			? TARGET_VTABLE_USES_DESCRIPTORS : 1))
	{
	  tree fndecl = BV_FN (fn);

	  if (DECL_THUNK_P (fndecl))
	    /* A thunk. We should never be calling this entry directly
	       from this vtable -- we'd use the entry for the non
	       thunk base function.  */
	    DECL_VINDEX (fndecl) = NULL_TREE;
	  else if (TREE_CODE (DECL_VINDEX (fndecl)) != INTEGER_CST)
	    DECL_VINDEX (fndecl) = build_int_cst (NULL_TREE, vindex);
	}
    }

  finish_struct_bits (t);
  set_method_tm_attributes (t);

  /* Complete the rtl for any static member objects of the type we're
     working on.  */
  for (x = TYPE_FIELDS (t); x; x = DECL_CHAIN (x))
    if (VAR_P (x) && TREE_STATIC (x)
        && TREE_TYPE (x) != error_mark_node
	&& same_type_p (TYPE_MAIN_VARIANT (TREE_TYPE (x)), t))
      DECL_MODE (x) = TYPE_MODE (t);

  /* Done with FIELDS...now decide whether to sort these for
     faster lookups later.

     We use a small number because most searches fail (succeeding
     ultimately as the search bores through the inheritance
     hierarchy), and we want this failure to occur quickly.  */

  insert_into_classtype_sorted_fields (TYPE_FIELDS (t), t, 8);

  /* Complain if one of the field types requires lower visibility.  */
  constrain_class_visibility (t);

  /* Make the rtl for any new vtables we have created, and unmark
     the base types we marked.  */
  finish_vtbls (t);

  /* Build the VTT for T.  */
  build_vtt (t);

  /* This warning does not make sense for Java classes, since they
     cannot have destructors.  */
  if (!TYPE_FOR_JAVA (t) && warn_nonvdtor && TYPE_POLYMORPHIC_P (t))
    {
      tree dtor;

      dtor = CLASSTYPE_DESTRUCTORS (t);
      if (/* An implicitly declared destructor is always public.  And,
	     if it were virtual, we would have created it by now.  */
	  !dtor
	  || (!DECL_VINDEX (dtor)
	      && (/* public non-virtual */
		  (!TREE_PRIVATE (dtor) && !TREE_PROTECTED (dtor))
		   || (/* non-public non-virtual with friends */
		       (TREE_PRIVATE (dtor) || TREE_PROTECTED (dtor))
			&& (CLASSTYPE_FRIEND_CLASSES (t)
			|| DECL_FRIENDLIST (TYPE_MAIN_DECL (t)))))))
	warning (OPT_Wnon_virtual_dtor,
		 "%q#T has virtual functions and accessible"
		 " non-virtual destructor", t);
    }

  complete_vars (t);

  if (warn_overloaded_virtual)
    warn_hidden (t);

  /* Class layout, assignment of virtual table slots, etc., is now
     complete.  Give the back end a chance to tweak the visibility of
     the class or perform any other required target modifications.  */
  targetm.cxx.adjust_class_at_definition (t);

  maybe_suppress_debug_info (t);

  if (flag_vtable_verify)
    vtv_save_class_info (t);

  dump_class_hierarchy (t);

  /* Finish debugging output for this type.  */
  rest_of_type_compilation (t, ! LOCAL_CLASS_P (t));

  if (TYPE_TRANSPARENT_AGGR (t))
    {
      tree field = first_field (t);
      if (field == NULL_TREE || error_operand_p (field))
	{
	  error ("type transparent %q#T does not have any fields", t);
	  TYPE_TRANSPARENT_AGGR (t) = 0;
	}
      else if (DECL_ARTIFICIAL (field))
	{
	  if (DECL_FIELD_IS_BASE (field))
	    error ("type transparent class %qT has base classes", t);
	  else
	    {
	      gcc_checking_assert (DECL_VIRTUAL_P (field));
	      error ("type transparent class %qT has virtual functions", t);
	    }
	  TYPE_TRANSPARENT_AGGR (t) = 0;
	}
      else if (TYPE_MODE (t) != DECL_MODE (field))
	{
	  error ("type transparent %q#T cannot be made transparent because "
		 "the type of the first field has a different ABI from the "
		 "class overall", t);
	  TYPE_TRANSPARENT_AGGR (t) = 0;
	}
    }
}

/* Insert FIELDS into T for the sorted case if the FIELDS count is
   equal to THRESHOLD or greater than THRESHOLD.  */

static void 
insert_into_classtype_sorted_fields (tree fields, tree t, int threshold)
{
  int n_fields = count_fields (fields);
  if (n_fields >= threshold)
    {
      struct sorted_fields_type *field_vec = sorted_fields_type_new (n_fields);
      add_fields_to_record_type (fields, field_vec, 0);
      qsort (field_vec->elts, n_fields, sizeof (tree), field_decl_cmp);
      CLASSTYPE_SORTED_FIELDS (t) = field_vec;
    }
}

/* Insert lately defined enum ENUMTYPE into T for the sorted case.  */

void
insert_late_enum_def_into_classtype_sorted_fields (tree enumtype, tree t)
{
  struct sorted_fields_type *sorted_fields = CLASSTYPE_SORTED_FIELDS (t);
  if (sorted_fields)
    {
      int i;
      int n_fields
	= list_length (TYPE_VALUES (enumtype)) + sorted_fields->len;
      struct sorted_fields_type *field_vec = sorted_fields_type_new (n_fields);
      
      for (i = 0; i < sorted_fields->len; ++i)
	field_vec->elts[i] = sorted_fields->elts[i];

      add_enum_fields_to_record_type (enumtype, field_vec,
				      sorted_fields->len);
      qsort (field_vec->elts, n_fields, sizeof (tree), field_decl_cmp);
      CLASSTYPE_SORTED_FIELDS (t) = field_vec;
    }
}

/* When T was built up, the member declarations were added in reverse
   order.  Rearrange them to declaration order.  */

void
unreverse_member_declarations (tree t)
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
      next = DECL_CHAIN (x);
      DECL_CHAIN (x) = prev;
      prev = x;
    }
  if (prev)
    {
      DECL_CHAIN (TYPE_FIELDS (t)) = x;
      if (prev)
	TYPE_FIELDS (t) = prev;
    }
}

tree
finish_struct (tree t, tree attributes)
{
  location_t saved_loc = input_location;

  /* Now that we've got all the field declarations, reverse everything
     as necessary.  */
  unreverse_member_declarations (t);

  cplus_decl_attributes (&t, attributes, (int) ATTR_FLAG_TYPE_IN_PLACE);

  /* Nadger the current location so that diagnostics point to the start of
     the struct, not the end.  */
  input_location = DECL_SOURCE_LOCATION (TYPE_NAME (t));

  if (processing_template_decl)
    {
      tree x;

      finish_struct_methods (t);
      TYPE_SIZE (t) = bitsize_zero_node;
      TYPE_SIZE_UNIT (t) = size_zero_node;

      /* We need to emit an error message if this type was used as a parameter
	 and it is an abstract type, even if it is a template. We construct
	 a simple CLASSTYPE_PURE_VIRTUALS list without taking bases into
	 account and we call complete_vars with this type, which will check
	 the PARM_DECLS. Note that while the type is being defined,
	 CLASSTYPE_PURE_VIRTUALS contains the list of the inline friends
	 (see CLASSTYPE_INLINE_FRIENDS) so we need to clear it.  */
      CLASSTYPE_PURE_VIRTUALS (t) = NULL;
      for (x = TYPE_METHODS (t); x; x = DECL_CHAIN (x))
	if (DECL_PURE_VIRTUAL_P (x))
	  vec_safe_push (CLASSTYPE_PURE_VIRTUALS (t), x);
      complete_vars (t);
      /* We need to add the target functions to the CLASSTYPE_METHOD_VEC if
	 an enclosing scope is a template class, so that this function be
	 found by lookup_fnfields_1 when the using declaration is not
	 instantiated yet.  */
      for (x = TYPE_FIELDS (t); x; x = DECL_CHAIN (x))
	if (TREE_CODE (x) == USING_DECL)
	  {
	    tree fn = strip_using_decl (x);
	    if (is_overloaded_fn (fn))
	      for (; fn; fn = OVL_NEXT (fn))
		add_method (t, OVL_CURRENT (fn), x);
	  }

      /* Remember current #pragma pack value.  */
      TYPE_PRECISION (t) = maximum_field_alignment;

      /* Fix up any variants we've already built.  */
      for (x = TYPE_NEXT_VARIANT (t); x; x = TYPE_NEXT_VARIANT (x))
	{
	  TYPE_SIZE (x) = TYPE_SIZE (t);
	  TYPE_SIZE_UNIT (x) = TYPE_SIZE_UNIT (t);
	  TYPE_FIELDS (x) = TYPE_FIELDS (t);
	  TYPE_METHODS (x) = TYPE_METHODS (t);
	}
    }
  else
    finish_struct_1 (t);

  input_location = saved_loc;

  TYPE_BEING_DEFINED (t) = 0;

  if (current_class_type)
    popclass ();
  else
    error ("trying to finish struct, but kicked out due to previous parse errors");

  if (processing_template_decl && at_function_scope_p ()
      /* Lambdas are defined by the LAMBDA_EXPR.  */
      && !LAMBDA_TYPE_P (t))
    add_stmt (build_min (TAG_DEFN, t));

  return t;
}

/* Hash table to avoid endless recursion when handling references.  */
static hash_table <pointer_hash <tree_node> > fixed_type_or_null_ref_ht;

/* Return the dynamic type of INSTANCE, if known.
   Used to determine whether the virtual function table is needed
   or not.

   *NONNULL is set iff INSTANCE can be known to be nonnull, regardless
   of our knowledge of its type.  *NONNULL should be initialized
   before this function is called.  */

static tree
fixed_type_or_null (tree instance, int *nonnull, int *cdtorp)
{
#define RECUR(T) fixed_type_or_null((T), nonnull, cdtorp)

  switch (TREE_CODE (instance))
    {
    case INDIRECT_REF:
      if (POINTER_TYPE_P (TREE_TYPE (instance)))
	return NULL_TREE;
      else
	return RECUR (TREE_OPERAND (instance, 0));

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
      return RECUR (TREE_OPERAND (instance, 0));

    case POINTER_PLUS_EXPR:
    case PLUS_EXPR:
    case MINUS_EXPR:
      if (TREE_CODE (TREE_OPERAND (instance, 0)) == ADDR_EXPR)
	return RECUR (TREE_OPERAND (instance, 0));
      if (TREE_CODE (TREE_OPERAND (instance, 1)) == INTEGER_CST)
	/* Propagate nonnull.  */
	return RECUR (TREE_OPERAND (instance, 0));

      return NULL_TREE;

    CASE_CONVERT:
      return RECUR (TREE_OPERAND (instance, 0));

    case ADDR_EXPR:
      instance = TREE_OPERAND (instance, 0);
      if (nonnull)
	{
	  /* Just because we see an ADDR_EXPR doesn't mean we're dealing
	     with a real object -- given &p->f, p can still be null.  */
	  tree t = get_base_address (instance);
	  /* ??? Probably should check DECL_WEAK here.  */
	  if (t && DECL_P (t))
	    *nonnull = 1;
	}
      return RECUR (instance);

    case COMPONENT_REF:
      /* If this component is really a base class reference, then the field
	 itself isn't definitive.  */
      if (DECL_FIELD_IS_BASE (TREE_OPERAND (instance, 1)))
	return RECUR (TREE_OPERAND (instance, 0));
      return RECUR (TREE_OPERAND (instance, 1));

    case VAR_DECL:
    case FIELD_DECL:
      if (TREE_CODE (TREE_TYPE (instance)) == ARRAY_TYPE
	  && MAYBE_CLASS_TYPE_P (TREE_TYPE (TREE_TYPE (instance))))
	{
	  if (nonnull)
	    *nonnull = 1;
	  return TREE_TYPE (TREE_TYPE (instance));
	}
      /* fall through...  */
    case TARGET_EXPR:
    case PARM_DECL:
    case RESULT_DECL:
      if (MAYBE_CLASS_TYPE_P (TREE_TYPE (instance)))
	{
	  if (nonnull)
	    *nonnull = 1;
	  return TREE_TYPE (instance);
	}
      else if (instance == current_class_ptr)
	{
	  if (nonnull)
	    *nonnull = 1;

	  /* if we're in a ctor or dtor, we know our type.  If
	     current_class_ptr is set but we aren't in a function, we're in
	     an NSDMI (and therefore a constructor).  */
	  if (current_scope () != current_function_decl
	      || (DECL_LANG_SPECIFIC (current_function_decl)
		  && (DECL_CONSTRUCTOR_P (current_function_decl)
		      || DECL_DESTRUCTOR_P (current_function_decl))))
	    {
	      if (cdtorp)
		*cdtorp = 1;
	      return TREE_TYPE (TREE_TYPE (instance));
	    }
	}
      else if (TREE_CODE (TREE_TYPE (instance)) == REFERENCE_TYPE)
	{
	  /* We only need one hash table because it is always left empty.  */
	  if (!fixed_type_or_null_ref_ht.is_created ())
	    fixed_type_or_null_ref_ht.create (37); 

	  /* Reference variables should be references to objects.  */
	  if (nonnull)
	    *nonnull = 1;

	  /* Enter the INSTANCE in a table to prevent recursion; a
	     variable's initializer may refer to the variable
	     itself.  */
	  if (VAR_P (instance)
	      && DECL_INITIAL (instance)
	      && !type_dependent_expression_p_push (DECL_INITIAL (instance))
	      && !fixed_type_or_null_ref_ht.find (instance))
	    {
	      tree type;
	      tree_node **slot;

	      slot = fixed_type_or_null_ref_ht.find_slot (instance, INSERT);
	      *slot = instance;
	      type = RECUR (DECL_INITIAL (instance));
	      fixed_type_or_null_ref_ht.remove_elt (instance);

	      return type;
	    }
	}
      return NULL_TREE;

    default:
      return NULL_TREE;
    }
#undef RECUR
}

/* Return nonzero if the dynamic type of INSTANCE is known, and
   equivalent to the static type.  We also handle the case where
   INSTANCE is really a pointer. Return negative if this is a
   ctor/dtor. There the dynamic type is known, but this might not be
   the most derived base of the original object, and hence virtual
   bases may not be laid out according to this type.

   Used to determine whether the virtual function table is needed
   or not.

   *NONNULL is set iff INSTANCE can be known to be nonnull, regardless
   of our knowledge of its type.  *NONNULL should be initialized
   before this function is called.  */

int
resolves_to_fixed_type_p (tree instance, int* nonnull)
{
  tree t = TREE_TYPE (instance);
  int cdtorp = 0;
  tree fixed;

  /* processing_template_decl can be false in a template if we're in
     fold_non_dependent_expr, but we still want to suppress this check.  */
  if (in_template_function ())
    {
      /* In a template we only care about the type of the result.  */
      if (nonnull)
	*nonnull = true;
      return true;
    }

  fixed = fixed_type_or_null (instance, nonnull, &cdtorp);
  if (fixed == NULL_TREE)
    return 0;
  if (POINTER_TYPE_P (t))
    t = TREE_TYPE (t);
  if (!same_type_ignoring_top_level_qualifiers_p (t, fixed))
    return 0;
  return cdtorp ? -1 : 1;
}


void
init_class_processing (void)
{
  current_class_depth = 0;
  current_class_stack_size = 10;
  current_class_stack
    = XNEWVEC (struct class_stack_node, current_class_stack_size);
  vec_alloc (local_classes, 8);
  sizeof_biggest_empty_class = size_zero_node;

  ridpointers[(int) RID_PUBLIC] = access_public_node;
  ridpointers[(int) RID_PRIVATE] = access_private_node;
  ridpointers[(int) RID_PROTECTED] = access_protected_node;
}

/* Restore the cached PREVIOUS_CLASS_LEVEL.  */

static void
restore_class_cache (void)
{
  tree type;

  /* We are re-entering the same class we just left, so we don't
     have to search the whole inheritance matrix to find all the
     decls to bind again.  Instead, we install the cached
     class_shadowed list and walk through it binding names.  */
  push_binding_level (previous_class_level);
  class_binding_level = previous_class_level;
  /* Restore IDENTIFIER_TYPE_VALUE.  */
  for (type = class_binding_level->type_shadowed;
       type;
       type = TREE_CHAIN (type))
    SET_IDENTIFIER_TYPE_VALUE (TREE_PURPOSE (type), TREE_TYPE (type));
}

/* Set global variables CURRENT_CLASS_NAME and CURRENT_CLASS_TYPE as
   appropriate for TYPE.

   So that we may avoid calls to lookup_name, we cache the _TYPE
   nodes of local TYPE_DECLs in the TREE_TYPE field of the name.

   For multiple inheritance, we perform a two-pass depth-first search
   of the type lattice.  */

void
pushclass (tree type)
{
  class_stack_node_t csn;

  type = TYPE_MAIN_VARIANT (type);

  /* Make sure there is enough room for the new entry on the stack.  */
  if (current_class_depth + 1 >= current_class_stack_size)
    {
      current_class_stack_size *= 2;
      current_class_stack
	= XRESIZEVEC (struct class_stack_node, current_class_stack,
		      current_class_stack_size);
    }

  /* Insert a new entry on the class stack.  */
  csn = current_class_stack + current_class_depth;
  csn->name = current_class_name;
  csn->type = current_class_type;
  csn->access = current_access_specifier;
  csn->names_used = 0;
  csn->hidden = 0;
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

  if (previous_class_level
      && type != previous_class_level->this_entity
      && current_class_depth == 1)
    {
      /* Forcibly remove any old class remnants.  */
      invalidate_class_lookup_cache ();
    }

  if (!previous_class_level
      || type != previous_class_level->this_entity
      || current_class_depth > 1)
    pushlevel_class ();
  else
    restore_class_cache ();
}

/* When we exit a toplevel class scope, we save its binding level so
   that we can restore it quickly.  Here, we've entered some other
   class, so we must invalidate our cache.  */

void
invalidate_class_lookup_cache (void)
{
  previous_class_level = NULL;
}

/* Get out of the current class scope. If we were in a class scope
   previously, that is the one popped to.  */

void
popclass (void)
{
  poplevel_class ();

  current_class_depth--;
  current_class_name = current_class_stack[current_class_depth].name;
  current_class_type = current_class_stack[current_class_depth].type;
  current_access_specifier = current_class_stack[current_class_depth].access;
  if (current_class_stack[current_class_depth].names_used)
    splay_tree_delete (current_class_stack[current_class_depth].names_used);
}

/* Mark the top of the class stack as hidden.  */

void
push_class_stack (void)
{
  if (current_class_depth)
    ++current_class_stack[current_class_depth - 1].hidden;
}

/* Mark the top of the class stack as un-hidden.  */

void
pop_class_stack (void)
{
  if (current_class_depth)
    --current_class_stack[current_class_depth - 1].hidden;
}

/* Returns 1 if the class type currently being defined is either T or
   a nested type of T.  */

bool
currently_open_class (tree t)
{
  int i;

  if (!CLASS_TYPE_P (t))
    return false;

  t = TYPE_MAIN_VARIANT (t);

  /* We start looking from 1 because entry 0 is from global scope,
     and has no type.  */
  for (i = current_class_depth; i > 0; --i)
    {
      tree c;
      if (i == current_class_depth)
	c = current_class_type;
      else
	{
	  if (current_class_stack[i].hidden)
	    break;
	  c = current_class_stack[i].type;
	}
      if (!c)
	continue;
      if (same_type_p (c, t))
	return true;
    }
  return false;
}

/* If either current_class_type or one of its enclosing classes are derived
   from T, return the appropriate type.  Used to determine how we found
   something via unqualified lookup.  */

tree
currently_open_derived_class (tree t)
{
  int i;

  /* The bases of a dependent type are unknown.  */
  if (dependent_type_p (t))
    return NULL_TREE;

  if (!current_class_type)
    return NULL_TREE;

  if (DERIVED_FROM_P (t, current_class_type))
    return current_class_type;

  for (i = current_class_depth - 1; i > 0; --i)
    {
      if (current_class_stack[i].hidden)
	break;
      if (DERIVED_FROM_P (t, current_class_stack[i].type))
	return current_class_stack[i].type;
    }

  return NULL_TREE;
}

/* Returns the innermost class type which is not a lambda closure type.  */

tree
current_nonlambda_class_type (void)
{
  int i;

  /* We start looking from 1 because entry 0 is from global scope,
     and has no type.  */
  for (i = current_class_depth; i > 0; --i)
    {
      tree c;
      if (i == current_class_depth)
	c = current_class_type;
      else
	{
	  if (current_class_stack[i].hidden)
	    break;
	  c = current_class_stack[i].type;
	}
      if (!c)
	continue;
      if (!LAMBDA_TYPE_P (c))
	return c;
    }
  return NULL_TREE;
}

/* When entering a class scope, all enclosing class scopes' names with
   static meaning (static variables, static functions, types and
   enumerators) have to be visible.  This recursive function calls
   pushclass for all enclosing class contexts until global or a local
   scope is reached.  TYPE is the enclosed class.  */

void
push_nested_class (tree type)
{
  /* A namespace might be passed in error cases, like A::B:C.  */
  if (type == NULL_TREE
      || !CLASS_TYPE_P (type))
    return;

  push_nested_class (DECL_CONTEXT (TYPE_MAIN_DECL (type)));

  pushclass (type);
}

/* Undoes a push_nested_class call.  */

void
pop_nested_class (void)
{
  tree context = DECL_CONTEXT (TYPE_MAIN_DECL (current_class_type));

  popclass ();
  if (context && CLASS_TYPE_P (context))
    pop_nested_class ();
}

/* Returns the number of extern "LANG" blocks we are nested within.  */

int
current_lang_depth (void)
{
  return vec_safe_length (current_lang_base);
}

/* Set global variables CURRENT_LANG_NAME to appropriate value
   so that behavior of name-mangling machinery is correct.  */

void
push_lang_context (tree name)
{
  vec_safe_push (current_lang_base, current_lang_name);

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
    error ("language string %<\"%E\"%> not recognized", name);
}

/* Get out of the current language scope.  */

void
pop_lang_context (void)
{
  current_lang_name = current_lang_base->pop ();
}

/* Type instantiation routines.  */

/* Given an OVERLOAD and a TARGET_TYPE, return the function that
   matches the TARGET_TYPE.  If there is no satisfactory match, return
   error_mark_node, and issue an error & warning messages under
   control of FLAGS.  Permit pointers to member function if FLAGS
   permits.  If TEMPLATE_ONLY, the name of the overloaded function was
   a template-id, and EXPLICIT_TARGS are the explicitly provided
   template arguments.  

   If OVERLOAD is for one or more member functions, then ACCESS_PATH
   is the base path used to reference those member functions.  If
   the address is resolved to a member function, access checks will be
   performed and errors issued if appropriate.  */

static tree
resolve_address_of_overloaded_function (tree target_type,
					tree overload,
					tsubst_flags_t flags,
					bool template_only,
					tree explicit_targs,
					tree access_path)
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
  /* We store the matches in a TREE_LIST rooted here.  The functions
     are the TREE_PURPOSE, not the TREE_VALUE, in this list, for easy
     interoperability with most_specialized_instantiation.  */
  tree matches = NULL_TREE;
  tree fn;
  tree target_fn_type;

  /* By the time we get here, we should be seeing only real
     pointer-to-member types, not the internal POINTER_TYPE to
     METHOD_TYPE representation.  */
  gcc_assert (!TYPE_PTR_P (target_type)
	      || TREE_CODE (TREE_TYPE (target_type)) != METHOD_TYPE);

  gcc_assert (is_overloaded_fn (overload));

  /* Check that the TARGET_TYPE is reasonable.  */
  if (TYPE_PTRFN_P (target_type)
      || TYPE_REFFN_P (target_type))
    /* This is OK.  */;
  else if (TYPE_PTRMEMFUNC_P (target_type))
    /* This is OK, too.  */
    is_ptrmem = 1;
  else if (TREE_CODE (target_type) == FUNCTION_TYPE)
    /* This is OK, too.  This comes from a conversion to reference
       type.  */
    target_type = build_reference_type (target_type);
  else
    {
      if (flags & tf_error)
	error ("cannot resolve overloaded function %qD based on"
	       " conversion to type %qT",
	       DECL_NAME (OVL_FUNCTION (overload)), target_type);
      return error_mark_node;
    }

  /* Non-member functions and static member functions match targets of type
     "pointer-to-function" or "reference-to-function."  Nonstatic member
     functions match targets of type "pointer-to-member-function;" the
     function type of the pointer to member is used to select the member
     function from the set of overloaded member functions.

     So figure out the FUNCTION_TYPE that we want to match against.  */
  target_fn_type = static_fn_type (target_type);

  /* If we can find a non-template function that matches, we can just
     use it.  There's no point in generating template instantiations
     if we're just going to throw them out anyhow.  But, of course, we
     can only do this when we don't *need* a template function.  */
  if (!template_only)
    {
      tree fns;

      for (fns = overload; fns; fns = OVL_NEXT (fns))
	{
	  tree fn = OVL_CURRENT (fns);

	  if (TREE_CODE (fn) == TEMPLATE_DECL)
	    /* We're not looking for templates just yet.  */
	    continue;

	  if ((TREE_CODE (TREE_TYPE (fn)) == METHOD_TYPE)
	      != is_ptrmem)
	    /* We're looking for a non-static member, and this isn't
	       one, or vice versa.  */
	    continue;

	  /* Ignore functions which haven't been explicitly
	     declared.  */
	  if (DECL_ANTICIPATED (fn))
	    continue;

	  /* See if there's a match.  */
	  if (same_type_p (target_fn_type, static_fn_type (fn)))
	    matches = tree_cons (fn, NULL_TREE, matches);
	}
    }

  /* Now, if we've already got a match (or matches), there's no need
     to proceed to the template functions.  But, if we don't have a
     match we need to look at them, too.  */
  if (!matches)
    {
      tree target_arg_types;
      tree target_ret_type;
      tree fns;
      tree *args;
      unsigned int nargs, ia;
      tree arg;

      target_arg_types = TYPE_ARG_TYPES (target_fn_type);
      target_ret_type = TREE_TYPE (target_fn_type);

      nargs = list_length (target_arg_types);
      args = XALLOCAVEC (tree, nargs);
      for (arg = target_arg_types, ia = 0;
	   arg != NULL_TREE && arg != void_list_node;
	   arg = TREE_CHAIN (arg), ++ia)
	args[ia] = TREE_VALUE (arg);
      nargs = ia;

      for (fns = overload; fns; fns = OVL_NEXT (fns))
	{
	  tree fn = OVL_CURRENT (fns);
	  tree instantiation;
	  tree targs;

	  if (TREE_CODE (fn) != TEMPLATE_DECL)
	    /* We're only looking for templates.  */
	    continue;

	  if ((TREE_CODE (TREE_TYPE (fn)) == METHOD_TYPE)
	      != is_ptrmem)
	    /* We're not looking for a non-static member, and this is
	       one, or vice versa.  */
	    continue;

	  tree ret = target_ret_type;

	  /* If the template has a deduced return type, don't expose it to
	     template argument deduction.  */
	  if (undeduced_auto_decl (fn))
	    ret = NULL_TREE;

	  /* Try to do argument deduction.  */
	  targs = make_tree_vec (DECL_NTPARMS (fn));
	  instantiation = fn_type_unification (fn, explicit_targs, targs, args,
					       nargs, ret,
					      DEDUCE_EXACT, LOOKUP_NORMAL,
					       false, false);
	  if (instantiation == error_mark_node)
	    /* Instantiation failed.  */
	    continue;

	  /* And now force instantiation to do return type deduction.  */
	  if (undeduced_auto_decl (instantiation))
	    {
	      ++function_depth;
	      instantiate_decl (instantiation, /*defer*/false, /*class*/false);
	      --function_depth;

	      require_deduced_type (instantiation);
	    }

	  /* See if there's a match.  */
	  if (same_type_p (target_fn_type, static_fn_type (instantiation)))
	    matches = tree_cons (instantiation, fn, matches);
	}

      /* Now, remove all but the most specialized of the matches.  */
      if (matches)
	{
	  tree match = most_specialized_instantiation (matches);

	  if (match != error_mark_node)
	    matches = tree_cons (TREE_PURPOSE (match),
				 NULL_TREE,
				 NULL_TREE);
	}
    }

  /* Now we should have exactly one function in MATCHES.  */
  if (matches == NULL_TREE)
    {
      /* There were *no* matches.  */
      if (flags & tf_error)
	{
	  error ("no matches converting function %qD to type %q#T",
		 DECL_NAME (OVL_CURRENT (overload)),
		 target_type);

	  print_candidates (overload);
	}
      return error_mark_node;
    }
  else if (TREE_CHAIN (matches))
    {
      /* There were too many matches.  First check if they're all
	 the same function.  */
      tree match = NULL_TREE;

      fn = TREE_PURPOSE (matches);

      /* For multi-versioned functions, more than one match is just fine and
	 decls_match will return false as they are different.  */
      for (match = TREE_CHAIN (matches); match; match = TREE_CHAIN (match))
	if (!decls_match (fn, TREE_PURPOSE (match))
	    && !targetm.target_option.function_versions
	       (fn, TREE_PURPOSE (match)))
          break;

      if (match)
	{
	  if (flags & tf_error)
	    {
	      error ("converting overloaded function %qD to type %q#T is ambiguous",
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
    }

  /* Good, exactly one match.  Now, convert it to the correct type.  */
  fn = TREE_PURPOSE (matches);

  if (DECL_NONSTATIC_MEMBER_FUNCTION_P (fn)
      && !(flags & tf_ptrmem_ok) && !flag_ms_extensions)
    {
      static int explained;

      if (!(flags & tf_error))
	return error_mark_node;

      permerror (input_location, "assuming pointer to member %qD", fn);
      if (!explained)
	{
	  inform (input_location, "(a pointer to member can only be formed with %<&%E%>)", fn);
	  explained = 1;
	}
    }

  /* If a pointer to a function that is multi-versioned is requested, the
     pointer to the dispatcher function is returned instead.  This works
     well because indirectly calling the function will dispatch the right
     function version at run-time.  */
  if (DECL_FUNCTION_VERSIONED (fn))
    {
      fn = get_function_version_dispatcher (fn);
      if (fn == NULL)
	return error_mark_node;
      /* Mark all the versions corresponding to the dispatcher as used.  */
      if (!(flags & tf_conv))
	mark_versions_used (fn);
    }

  /* If we're doing overload resolution purely for the purpose of
     determining conversion sequences, we should not consider the
     function used.  If this conversion sequence is selected, the
     function will be marked as used at this point.  */
  if (!(flags & tf_conv))
    {
      /* Make =delete work with SFINAE.  */
      if (DECL_DELETED_FN (fn) && !(flags & tf_error))
	return error_mark_node;
      
      mark_used (fn);
    }

  /* We could not check access to member functions when this
     expression was originally created since we did not know at that
     time to which function the expression referred.  */
  if (DECL_FUNCTION_MEMBER_P (fn))
    {
      gcc_assert (access_path);
      perform_or_defer_access_check (access_path, fn, fn, flags);
    }

  if (TYPE_PTRFN_P (target_type) || TYPE_PTRMEMFUNC_P (target_type))
    return cp_build_addr_expr (fn, flags);
  else
    {
      /* The target must be a REFERENCE_TYPE.  Above, cp_build_unary_op
	 will mark the function as addressed, but here we must do it
	 explicitly.  */
      cxx_mark_addressable (fn);

      return fn;
    }
}

/* This function will instantiate the type of the expression given in
   RHS to match the type of LHSTYPE.  If errors exist, then return
   error_mark_node. FLAGS is a bit mask.  If TF_ERROR is set, then
   we complain on errors.  If we are not complaining, never modify rhs,
   as overload resolution wants to try many possible instantiations, in
   the hope that at least one will work.

   For non-recursive calls, LHSTYPE should be a function, pointer to
   function, or a pointer to member function.  */

tree
instantiate_type (tree lhstype, tree rhs, tsubst_flags_t flags)
{
  tsubst_flags_t flags_in = flags;
  tree access_path = NULL_TREE;

  flags &= ~tf_ptrmem_ok;

  if (lhstype == unknown_type_node)
    {
      if (flags & tf_error)
	error ("not enough type information");
      return error_mark_node;
    }

  if (TREE_TYPE (rhs) != NULL_TREE && ! (type_unknown_p (rhs)))
    {
      tree fntype = non_reference (lhstype);
      if (same_type_p (fntype, TREE_TYPE (rhs)))
	return rhs;
      if (flag_ms_extensions
	  && TYPE_PTRMEMFUNC_P (fntype)
	  && !TYPE_PTRMEMFUNC_P (TREE_TYPE (rhs)))
	/* Microsoft allows `A::f' to be resolved to a
	   pointer-to-member.  */
	;
      else
	{
	  if (flags & tf_error)
	    error ("cannot convert %qE from type %qT to type %qT",
		   rhs, TREE_TYPE (rhs), fntype);
	  return error_mark_node;
	}
    }

  if (BASELINK_P (rhs))
    {
      access_path = BASELINK_ACCESS_BINFO (rhs);
      rhs = BASELINK_FUNCTIONS (rhs);
    }

  /* If we are in a template, and have a NON_DEPENDENT_EXPR, we cannot
     deduce any type information.  */
  if (TREE_CODE (rhs) == NON_DEPENDENT_EXPR)
    {
      if (flags & tf_error)
	error ("not enough type information");
      return error_mark_node;
    }

  /* There only a few kinds of expressions that may have a type
     dependent on overload resolution.  */
  gcc_assert (TREE_CODE (rhs) == ADDR_EXPR
	      || TREE_CODE (rhs) == COMPONENT_REF
	      || is_overloaded_fn (rhs)
	      || (flag_ms_extensions && TREE_CODE (rhs) == FUNCTION_DECL));

  /* This should really only be used when attempting to distinguish
     what sort of a pointer to function we have.  For now, any
     arithmetic operation which is not supported on pointers
     is rejected as an error.  */

  switch (TREE_CODE (rhs))
    {
    case COMPONENT_REF:
      {
	tree member = TREE_OPERAND (rhs, 1);

	member = instantiate_type (lhstype, member, flags);
	if (member != error_mark_node
	    && TREE_SIDE_EFFECTS (TREE_OPERAND (rhs, 0)))
	  /* Do not lose object's side effects.  */
	  return build2 (COMPOUND_EXPR, TREE_TYPE (member),
			 TREE_OPERAND (rhs, 0), member);
	return member;
      }

    case OFFSET_REF:
      rhs = TREE_OPERAND (rhs, 1);
      if (BASELINK_P (rhs))
	return instantiate_type (lhstype, rhs, flags_in);

      /* This can happen if we are forming a pointer-to-member for a
	 member template.  */
      gcc_assert (TREE_CODE (rhs) == TEMPLATE_ID_EXPR);

      /* Fall through.  */

    case TEMPLATE_ID_EXPR:
      {
	tree fns = TREE_OPERAND (rhs, 0);
	tree args = TREE_OPERAND (rhs, 1);

	return
	  resolve_address_of_overloaded_function (lhstype, fns, flags_in,
						  /*template_only=*/true,
						  args, access_path);
      }

    case OVERLOAD:
    case FUNCTION_DECL:
      return
	resolve_address_of_overloaded_function (lhstype, rhs, flags_in,
						/*template_only=*/false,
						/*explicit_targs=*/NULL_TREE,
						access_path);

    case ADDR_EXPR:
    {
      if (PTRMEM_OK_P (rhs))
	flags |= tf_ptrmem_ok;

      return instantiate_type (lhstype, TREE_OPERAND (rhs, 0), flags);
    }

    case ERROR_MARK:
      return error_mark_node;

    default:
      gcc_unreachable ();
    }
  return error_mark_node;
}

/* Return the name of the virtual function pointer field
   (as an IDENTIFIER_NODE) for the given TYPE.  Note that
   this may have to look back through base types to find the
   ultimate field name.  (For single inheritance, these could
   all be the same name.  Who knows for multiple inheritance).  */

static tree
get_vfield_name (tree type)
{
  tree binfo, base_binfo;
  char *buf;

  for (binfo = TYPE_BINFO (type);
       BINFO_N_BASE_BINFOS (binfo);
       binfo = base_binfo)
    {
      base_binfo = BINFO_BASE_BINFO (binfo, 0);

      if (BINFO_VIRTUAL_P (base_binfo)
	  || !TYPE_CONTAINS_VPTR_P (BINFO_TYPE (base_binfo)))
	break;
    }

  type = BINFO_TYPE (binfo);
  buf = (char *) alloca (sizeof (VFIELD_NAME_FORMAT)
			 + TYPE_NAME_LENGTH (type) + 2);
  sprintf (buf, VFIELD_NAME_FORMAT,
	   IDENTIFIER_POINTER (constructor_name (type)));
  return get_identifier (buf);
}

void
print_class_statistics (void)
{
  if (! GATHER_STATISTICS)
    return;

  fprintf (stderr, "convert_harshness = %d\n", n_convert_harshness);
  fprintf (stderr, "compute_conversion_costs = %d\n", n_compute_conversion_costs);
  if (n_vtables)
    {
      fprintf (stderr, "vtables = %d; vtable searches = %d\n",
	       n_vtables, n_vtable_searches);
      fprintf (stderr, "vtable entries = %d; vtable elems = %d\n",
	       n_vtable_entries, n_vtable_elems);
    }
}

/* Build a dummy reference to ourselves so Derived::Base (and A::A) works,
   according to [class]:
					  The class-name is also inserted
   into  the scope of the class itself.  For purposes of access checking,
   the inserted class name is treated as if it were a public member name.  */

void
build_self_reference (void)
{
  tree name = constructor_name (current_class_type);
  tree value = build_lang_decl (TYPE_DECL, name, current_class_type);
  tree saved_cas;

  DECL_NONLOCAL (value) = 1;
  DECL_CONTEXT (value) = current_class_type;
  DECL_ARTIFICIAL (value) = 1;
  SET_DECL_SELF_REFERENCE_P (value);
  set_underlying_type (value);

  if (processing_template_decl)
    value = push_template_decl (value);

  saved_cas = current_access_specifier;
  current_access_specifier = access_public_node;
  finish_member_declaration (value);
  current_access_specifier = saved_cas;
}

/* Returns 1 if TYPE contains only padding bytes.  */

int
is_empty_class (tree type)
{
  if (type == error_mark_node)
    return 0;

  if (! CLASS_TYPE_P (type))
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
      tree binfo;
      tree base_binfo;
      int i;

      for (binfo = TYPE_BINFO (type), i = 0;
	   BINFO_BASE_ITERATE (binfo, i, base_binfo); ++i)
	if (contains_empty_class_p (BINFO_TYPE (base_binfo)))
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

/* Returns true if TYPE contains no actual data, just various
   possible combinations of empty classes and possibly a vptr.  */

bool
is_really_empty_class (tree type)
{
  if (CLASS_TYPE_P (type))
    {
      tree field;
      tree binfo;
      tree base_binfo;
      int i;

      /* CLASSTYPE_EMPTY_P isn't set properly until the class is actually laid
	 out, but we'd like to be able to check this before then.  */
      if (COMPLETE_TYPE_P (type) && is_empty_class (type))
	return true;

      for (binfo = TYPE_BINFO (type), i = 0;
	   BINFO_BASE_ITERATE (binfo, i, base_binfo); ++i)
	if (!is_really_empty_class (BINFO_TYPE (base_binfo)))
	  return false;
      for (field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
	if (TREE_CODE (field) == FIELD_DECL
	    && !DECL_ARTIFICIAL (field)
	    && !is_really_empty_class (TREE_TYPE (field)))
	  return false;
      return true;
    }
  else if (TREE_CODE (type) == ARRAY_TYPE)
    return is_really_empty_class (TREE_TYPE (type));
  return false;
}

/* Note that NAME was looked up while the current class was being
   defined and that the result of that lookup was DECL.  */

void
maybe_note_name_used_in_class (tree name, tree decl)
{
  splay_tree names_used;

  /* If we're not defining a class, there's nothing to do.  */
  if (!(innermost_scope_kind() == sk_class
	&& TYPE_BEING_DEFINED (current_class_type)
	&& !LAMBDA_TYPE_P (current_class_type)))
    return;

  /* If there's already a binding for this NAME, then we don't have
     anything to worry about.  */
  if (lookup_member (current_class_type, name,
		     /*protect=*/0, /*want_type=*/false, tf_warning_or_error))
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
note_name_declared_in_class (tree name, tree decl)
{
  splay_tree names_used;
  splay_tree_node n;

  /* Look to see if we ever used this name.  */
  names_used
    = current_class_stack[current_class_depth - 1].names_used;
  if (!names_used)
    return;
  /* The C language allows members to be declared with a type of the same
     name, and the C++ standard says this diagnostic is not required.  So
     allow it in extern "C" blocks unless predantic is specified.
     Allow it in all cases if -ms-extensions is specified.  */
  if ((!pedantic && current_lang_name == lang_name_c)
      || flag_ms_extensions)
    return;
  n = splay_tree_lookup (names_used, (splay_tree_key) name);
  if (n)
    {
      /* [basic.scope.class]

	 A name N used in a class S shall refer to the same declaration
	 in its context and when re-evaluated in the completed scope of
	 S.  */
      permerror (input_location, "declaration of %q#D", decl);
      permerror (input_location, "changes meaning of %qD from %q+#D",
	       DECL_NAME (OVL_CURRENT (decl)), (tree) n->value);
    }
}

/* Returns the VAR_DECL for the complete vtable associated with BINFO.
   Secondary vtables are merged with primary vtables; this function
   will return the VAR_DECL for the primary vtable.  */

tree
get_vtbl_decl_for_binfo (tree binfo)
{
  tree decl;

  decl = BINFO_VTABLE (binfo);
  if (decl && TREE_CODE (decl) == POINTER_PLUS_EXPR)
    {
      gcc_assert (TREE_CODE (TREE_OPERAND (decl, 0)) == ADDR_EXPR);
      decl = TREE_OPERAND (TREE_OPERAND (decl, 0), 0);
    }
  if (decl)
    gcc_assert (VAR_P (decl));
  return decl;
}


/* Returns the binfo for the primary base of BINFO.  If the resulting
   BINFO is a virtual base, and it is inherited elsewhere in the
   hierarchy, then the returned binfo might not be the primary base of
   BINFO in the complete object.  Check BINFO_PRIMARY_P or
   BINFO_LOST_PRIMARY_P to be sure.  */

static tree
get_primary_binfo (tree binfo)
{
  tree primary_base;

  primary_base = CLASSTYPE_PRIMARY_BINFO (BINFO_TYPE (binfo));
  if (!primary_base)
    return NULL_TREE;

  return copied_binfo (primary_base, binfo);
}

/* If INDENTED_P is zero, indent to INDENT. Return nonzero.  */

static int
maybe_indent_hierarchy (FILE * stream, int indent, int indented_p)
{
  if (!indented_p)
    fprintf (stream, "%*s", indent, "");
  return 1;
}

/* Dump the offsets of all the bases rooted at BINFO to STREAM.
   INDENT should be zero when called from the top level; it is
   incremented recursively.  IGO indicates the next expected BINFO in
   inheritance graph ordering.  */

static tree
dump_class_hierarchy_r (FILE *stream,
			int flags,
			tree binfo,
			tree igo,
			int indent)
{
  int indented = 0;
  tree base_binfo;
  int i;

  indented = maybe_indent_hierarchy (stream, indent, 0);
  fprintf (stream, "%s (0x" HOST_WIDE_INT_PRINT_HEX ") ",
	   type_as_string (BINFO_TYPE (binfo), TFF_PLAIN_IDENTIFIER),
	   (HOST_WIDE_INT) (uintptr_t) binfo);
  if (binfo != igo)
    {
      fprintf (stream, "alternative-path\n");
      return igo;
    }
  igo = TREE_CHAIN (binfo);

  fprintf (stream, HOST_WIDE_INT_PRINT_DEC,
	   tree_to_shwi (BINFO_OFFSET (binfo)));
  if (is_empty_class (BINFO_TYPE (binfo)))
    fprintf (stream, " empty");
  else if (CLASSTYPE_NEARLY_EMPTY_P (BINFO_TYPE (binfo)))
    fprintf (stream, " nearly-empty");
  if (BINFO_VIRTUAL_P (binfo))
    fprintf (stream, " virtual");
  fprintf (stream, "\n");

  indented = 0;
  if (BINFO_PRIMARY_P (binfo))
    {
      indented = maybe_indent_hierarchy (stream, indent + 3, indented);
      fprintf (stream, " primary-for %s (0x" HOST_WIDE_INT_PRINT_HEX ")",
	       type_as_string (BINFO_TYPE (BINFO_INHERITANCE_CHAIN (binfo)),
			       TFF_PLAIN_IDENTIFIER),
	       (HOST_WIDE_INT) (uintptr_t) BINFO_INHERITANCE_CHAIN (binfo));
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

  for (i = 0; BINFO_BASE_ITERATE (binfo, i, base_binfo); i++)
    igo = dump_class_hierarchy_r (stream, flags, base_binfo, igo, indent + 2);

  return igo;
}

/* Dump the BINFO hierarchy for T.  */

static void
dump_class_hierarchy_1 (FILE *stream, int flags, tree t)
{
  fprintf (stream, "Class %s\n", type_as_string (t, TFF_PLAIN_IDENTIFIER));
  fprintf (stream, "   size=%lu align=%lu\n",
	   (unsigned long)(tree_to_shwi (TYPE_SIZE (t)) / BITS_PER_UNIT),
	   (unsigned long)(TYPE_ALIGN (t) / BITS_PER_UNIT));
  fprintf (stream, "   base size=%lu base align=%lu\n",
	   (unsigned long)(tree_to_shwi (TYPE_SIZE (CLASSTYPE_AS_BASE (t)))
			   / BITS_PER_UNIT),
	   (unsigned long)(TYPE_ALIGN (CLASSTYPE_AS_BASE (t))
			   / BITS_PER_UNIT));
  dump_class_hierarchy_r (stream, flags, TYPE_BINFO (t), TYPE_BINFO (t), 0);
  fprintf (stream, "\n");
}

/* Debug interface to hierarchy dumping.  */

void
debug_class (tree t)
{
  dump_class_hierarchy_1 (stderr, TDF_SLIM, t);
}

static void
dump_class_hierarchy (tree t)
{
  int flags;
  FILE *stream = dump_begin (TDI_class, &flags);

  if (stream)
    {
      dump_class_hierarchy_1 (stream, flags, t);
      dump_end (TDI_class, stream);
    }
}

static void
dump_array (FILE * stream, tree decl)
{
  tree value;
  unsigned HOST_WIDE_INT ix;
  HOST_WIDE_INT elt;
  tree size = TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (decl)));

  elt = (tree_to_shwi (TYPE_SIZE (TREE_TYPE (TREE_TYPE (decl))))
	 / BITS_PER_UNIT);
  fprintf (stream, "%s:", decl_as_string (decl, TFF_PLAIN_IDENTIFIER));
  fprintf (stream, " %s entries",
	   expr_as_string (size_binop (PLUS_EXPR, size, size_one_node),
			   TFF_PLAIN_IDENTIFIER));
  fprintf (stream, "\n");

  FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (DECL_INITIAL (decl)),
			      ix, value)
    fprintf (stream, "%-4ld  %s\n", (long)(ix * elt),
	     expr_as_string (value, TFF_PLAIN_IDENTIFIER));
}

static void
dump_vtable (tree t, tree binfo, tree vtable)
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
	       type_as_string (BINFO_TYPE (binfo), TFF_PLAIN_IDENTIFIER));
      if (ctor_vtbl_p)
	{
	  if (!BINFO_VIRTUAL_P (binfo))
	    fprintf (stream, " (0x" HOST_WIDE_INT_PRINT_HEX " instance)",
		     (HOST_WIDE_INT) (uintptr_t) binfo);
	  fprintf (stream, " in %s", type_as_string (t, TFF_PLAIN_IDENTIFIER));
	}
      fprintf (stream, "\n");
      dump_array (stream, vtable);
      fprintf (stream, "\n");
    }

  dump_end (TDI_class, stream);
}

static void
dump_vtt (tree t, tree vtt)
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

/* Dump a function or thunk and its thunkees.  */

static void
dump_thunk (FILE *stream, int indent, tree thunk)
{
  static const char spaces[] = "        ";
  tree name = DECL_NAME (thunk);
  tree thunks;

  fprintf (stream, "%.*s%p %s %s", indent, spaces,
	   (void *)thunk,
	   !DECL_THUNK_P (thunk) ? "function"
	   : DECL_THIS_THUNK_P (thunk) ? "this-thunk" : "covariant-thunk",
	   name ? IDENTIFIER_POINTER (name) : "<unset>");
  if (DECL_THUNK_P (thunk))
    {
      HOST_WIDE_INT fixed_adjust = THUNK_FIXED_OFFSET (thunk);
      tree virtual_adjust = THUNK_VIRTUAL_OFFSET (thunk);

      fprintf (stream, " fixed=" HOST_WIDE_INT_PRINT_DEC, fixed_adjust);
      if (!virtual_adjust)
	/*NOP*/;
      else if (DECL_THIS_THUNK_P (thunk))
	fprintf (stream, " vcall="  HOST_WIDE_INT_PRINT_DEC,
		 tree_to_shwi (virtual_adjust));
      else
	fprintf (stream, " vbase=" HOST_WIDE_INT_PRINT_DEC "(%s)",
		 tree_to_shwi (BINFO_VPTR_FIELD (virtual_adjust)),
		 type_as_string (BINFO_TYPE (virtual_adjust), TFF_SCOPE));
      if (THUNK_ALIAS (thunk))
	fprintf (stream, " alias to %p", (void *)THUNK_ALIAS (thunk));
    }
  fprintf (stream, "\n");
  for (thunks = DECL_THUNKS (thunk); thunks; thunks = TREE_CHAIN (thunks))
    dump_thunk (stream, indent + 2, thunks);
}

/* Dump the thunks for FN.  */

void
debug_thunks (tree fn)
{
  dump_thunk (stderr, 0, fn);
}

/* Virtual function table initialization.  */

/* Create all the necessary vtables for T and its base classes.  */

static void
finish_vtbls (tree t)
{
  tree vbase;
  vec<constructor_elt, va_gc> *v = NULL;
  tree vtable = BINFO_VTABLE (TYPE_BINFO (t));

  /* We lay out the primary and secondary vtables in one contiguous
     vtable.  The primary vtable is first, followed by the non-virtual
     secondary vtables in inheritance graph order.  */
  accumulate_vtbl_inits (TYPE_BINFO (t), TYPE_BINFO (t), TYPE_BINFO (t),
			 vtable, t, &v);

  /* Then come the virtual bases, also in inheritance graph order.  */
  for (vbase = TYPE_BINFO (t); vbase; vbase = TREE_CHAIN (vbase))
    {
      if (!BINFO_VIRTUAL_P (vbase))
	continue;
      accumulate_vtbl_inits (vbase, vbase, TYPE_BINFO (t), vtable, t, &v);
    }

  if (BINFO_VTABLE (TYPE_BINFO (t)))
    initialize_vtable (TYPE_BINFO (t), v);
}

/* Initialize the vtable for BINFO with the INITS.  */

static void
initialize_vtable (tree binfo, vec<constructor_elt, va_gc> *inits)
{
  tree decl;

  layout_vtable_decl (binfo, vec_safe_length (inits));
  decl = get_vtbl_decl_for_binfo (binfo);
  initialize_artificial_var (decl, inits);
  dump_vtable (BINFO_TYPE (binfo), binfo, decl);
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
build_vtt (tree t)
{
  tree type;
  tree vtt;
  tree index;
  vec<constructor_elt, va_gc> *inits;

  /* Build up the initializers for the VTT.  */
  inits = NULL;
  index = size_zero_node;
  build_vtt_inits (TYPE_BINFO (t), t, &inits, &index);

  /* If we didn't need a VTT, we're done.  */
  if (!inits)
    return;

  /* Figure out the type of the VTT.  */
  type = build_array_of_n_type (const_ptr_type_node,
                                inits->length ());

  /* Now, build the VTT object itself.  */
  vtt = build_vtable (t, mangle_vtt_for_type (t), type);
  initialize_artificial_var (vtt, inits);
  /* Add the VTT to the vtables list.  */
  DECL_CHAIN (vtt) = DECL_CHAIN (CLASSTYPE_VTABLES (t));
  DECL_CHAIN (CLASSTYPE_VTABLES (t)) = vtt;

  dump_vtt (t, vtt);
}

/* When building a secondary VTT, BINFO_VTABLE is set to a TREE_LIST with
   PURPOSE the RTTI_BINFO, VALUE the real vtable pointer for this binfo,
   and CHAIN the vtable pointer for this binfo after construction is
   complete.  VALUE can also be another BINFO, in which case we recurse.  */

static tree
binfo_ctor_vtable (tree binfo)
{
  tree vt;

  while (1)
    {
      vt = BINFO_VTABLE (binfo);
      if (TREE_CODE (vt) == TREE_LIST)
	vt = TREE_VALUE (vt);
      if (TREE_CODE (vt) == TREE_BINFO)
	binfo = vt;
      else
	break;
    }

  return vt;
}

/* Data for secondary VTT initialization.  */
typedef struct secondary_vptr_vtt_init_data_s
{
  /* Is this the primary VTT? */
  bool top_level_p;

  /* Current index into the VTT.  */
  tree index;

  /* Vector of initializers built up.  */
  vec<constructor_elt, va_gc> *inits;

  /* The type being constructed by this secondary VTT.  */
  tree type_being_constructed;
} secondary_vptr_vtt_init_data;

/* Recursively build the VTT-initializer for BINFO (which is in the
   hierarchy dominated by T).  INITS points to the end of the initializer
   list to date.  INDEX is the VTT index where the next element will be
   replaced.  Iff BINFO is the binfo for T, this is the top level VTT (i.e.
   not a subvtt for some base of T).  When that is so, we emit the sub-VTTs
   for virtual bases of T. When it is not so, we build the constructor
   vtables for the BINFO-in-T variant.  */

static void
build_vtt_inits (tree binfo, tree t, vec<constructor_elt, va_gc> **inits,
		 tree *index)
{
  int i;
  tree b;
  tree init;
  secondary_vptr_vtt_init_data data;
  int top_level_p = SAME_BINFO_TYPE_P (BINFO_TYPE (binfo), t);

  /* We only need VTTs for subobjects with virtual bases.  */
  if (!CLASSTYPE_VBASECLASSES (BINFO_TYPE (binfo)))
    return;

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
  CONSTRUCTOR_APPEND_ELT (*inits, NULL_TREE, init);
  if (top_level_p)
    {
      gcc_assert (!BINFO_VPTR_INDEX (binfo));
      BINFO_VPTR_INDEX (binfo) = *index;
    }
  *index = size_binop (PLUS_EXPR, *index, TYPE_SIZE_UNIT (ptr_type_node));

  /* Recursively add the secondary VTTs for non-virtual bases.  */
  for (i = 0; BINFO_BASE_ITERATE (binfo, i, b); ++i)
    if (!BINFO_VIRTUAL_P (b))
      build_vtt_inits (b, t, inits, index);

  /* Add secondary virtual pointers for all subobjects of BINFO with
     either virtual bases or reachable along a virtual path, except
     subobjects that are non-virtual primary bases.  */
  data.top_level_p = top_level_p;
  data.index = *index;
  data.inits = *inits;
  data.type_being_constructed = BINFO_TYPE (binfo);

  dfs_walk_once (binfo, dfs_build_secondary_vptr_vtt_inits, NULL, &data);

  *index = data.index;

  /* data.inits might have grown as we added secondary virtual pointers.
     Make sure our caller knows about the new vector.  */
  *inits = data.inits;

  if (top_level_p)
    /* Add the secondary VTTs for virtual bases in inheritance graph
       order.  */
    for (b = TYPE_BINFO (BINFO_TYPE (binfo)); b; b = TREE_CHAIN (b))
      {
	if (!BINFO_VIRTUAL_P (b))
	  continue;

	build_vtt_inits (b, t, inits, index);
      }
  else
    /* Remove the ctor vtables we created.  */
    dfs_walk_all (binfo, dfs_fixup_binfo_vtbls, NULL, binfo);
}

/* Called from build_vtt_inits via dfs_walk.  BINFO is the binfo for the base
   in most derived. DATA is a SECONDARY_VPTR_VTT_INIT_DATA structure.  */

static tree
dfs_build_secondary_vptr_vtt_inits (tree binfo, void *data_)
{
  secondary_vptr_vtt_init_data *data = (secondary_vptr_vtt_init_data *)data_;

  /* We don't care about bases that don't have vtables.  */
  if (!TYPE_VFIELD (BINFO_TYPE (binfo)))
    return dfs_skip_bases;

  /* We're only interested in proper subobjects of the type being
     constructed.  */
  if (SAME_BINFO_TYPE_P (BINFO_TYPE (binfo), data->type_being_constructed))
    return NULL_TREE;

  /* We're only interested in bases with virtual bases or reachable
     via a virtual path from the type being constructed.  */
  if (!(CLASSTYPE_VBASECLASSES (BINFO_TYPE (binfo))
	|| binfo_via_virtual (binfo, data->type_being_constructed)))
    return dfs_skip_bases;

  /* We're not interested in non-virtual primary bases.  */
  if (!BINFO_VIRTUAL_P (binfo) && BINFO_PRIMARY_P (binfo))
    return NULL_TREE;

  /* Record the index where this secondary vptr can be found.  */
  if (data->top_level_p)
    {
      gcc_assert (!BINFO_VPTR_INDEX (binfo));
      BINFO_VPTR_INDEX (binfo) = data->index;

      if (BINFO_VIRTUAL_P (binfo))
	{
	  /* It's a primary virtual base, and this is not a
	     construction vtable.  Find the base this is primary of in
	     the inheritance graph, and use that base's vtable
	     now.  */
	  while (BINFO_PRIMARY_P (binfo))
	    binfo = BINFO_INHERITANCE_CHAIN (binfo);
	}
    }

  /* Add the initializer for the secondary vptr itself.  */
  CONSTRUCTOR_APPEND_ELT (data->inits, NULL_TREE, binfo_ctor_vtable (binfo));

  /* Advance the vtt index.  */
  data->index = size_binop (PLUS_EXPR, data->index,
			    TYPE_SIZE_UNIT (ptr_type_node));

  return NULL_TREE;
}

/* Called from build_vtt_inits via dfs_walk. After building
   constructor vtables and generating the sub-vtt from them, we need
   to restore the BINFO_VTABLES that were scribbled on.  DATA is the
   binfo of the base whose sub vtt was generated.  */

static tree
dfs_fixup_binfo_vtbls (tree binfo, void* data)
{
  tree vtable = BINFO_VTABLE (binfo);

  if (!TYPE_CONTAINS_VPTR_P (BINFO_TYPE (binfo)))
    /* If this class has no vtable, none of its bases do.  */
    return dfs_skip_bases;

  if (!vtable)
    /* This might be a primary base, so have no vtable in this
       hierarchy.  */
    return NULL_TREE;

  /* If we scribbled the construction vtable vptr into BINFO, clear it
     out now.  */
  if (TREE_CODE (vtable) == TREE_LIST
      && (TREE_PURPOSE (vtable) == (tree) data))
    BINFO_VTABLE (binfo) = TREE_CHAIN (vtable);

  return NULL_TREE;
}

/* Build the construction vtable group for BINFO which is in the
   hierarchy dominated by T.  */

static void
build_ctor_vtbl_group (tree binfo, tree t)
{
  tree type;
  tree vtbl;
  tree id;
  tree vbase;
  vec<constructor_elt, va_gc> *v;

  /* See if we've already created this construction vtable group.  */
  id = mangle_ctor_vtbl_for_type (t, binfo);
  if (IDENTIFIER_GLOBAL_VALUE (id))
    return;

  gcc_assert (!SAME_BINFO_TYPE_P (BINFO_TYPE (binfo), t));
  /* Build a version of VTBL (with the wrong type) for use in
     constructing the addresses of secondary vtables in the
     construction vtable group.  */
  vtbl = build_vtable (t, id, ptr_type_node);
  DECL_CONSTRUCTION_VTABLE_P (vtbl) = 1;
  /* Don't export construction vtables from shared libraries.  Even on
     targets that don't support hidden visibility, this tells
     can_refer_decl_in_current_unit_p not to assume that it's safe to
     access from a different compilation unit (bz 54314).  */
  DECL_VISIBILITY (vtbl) = VISIBILITY_HIDDEN;
  DECL_VISIBILITY_SPECIFIED (vtbl) = true;

  v = NULL;
  accumulate_vtbl_inits (binfo, TYPE_BINFO (TREE_TYPE (binfo)),
			 binfo, vtbl, t, &v);

  /* Add the vtables for each of our virtual bases using the vbase in T
     binfo.  */
  for (vbase = TYPE_BINFO (BINFO_TYPE (binfo));
       vbase;
       vbase = TREE_CHAIN (vbase))
    {
      tree b;

      if (!BINFO_VIRTUAL_P (vbase))
	continue;
      b = copied_binfo (vbase, binfo);

      accumulate_vtbl_inits (b, vbase, binfo, vtbl, t, &v);
    }

  /* Figure out the type of the construction vtable.  */
  type = build_array_of_n_type (vtable_entry_type, v->length ());
  layout_type (type);
  TREE_TYPE (vtbl) = type;
  DECL_SIZE (vtbl) = DECL_SIZE_UNIT (vtbl) = NULL_TREE;
  layout_decl (vtbl, 0);

  /* Initialize the construction vtable.  */
  CLASSTYPE_VTABLES (t) = chainon (CLASSTYPE_VTABLES (t), vtbl);
  initialize_artificial_var (vtbl, v);
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
accumulate_vtbl_inits (tree binfo,
		       tree orig_binfo,
		       tree rtti_binfo,
		       tree vtbl,
		       tree t,
		       vec<constructor_elt, va_gc> **inits)
{
  int i;
  tree base_binfo;
  int ctor_vtbl_p = !SAME_BINFO_TYPE_P (BINFO_TYPE (rtti_binfo), t);

  gcc_assert (SAME_BINFO_TYPE_P (BINFO_TYPE (binfo), BINFO_TYPE (orig_binfo)));

  /* If it doesn't have a vptr, we don't do anything.  */
  if (!TYPE_CONTAINS_VPTR_P (BINFO_TYPE (binfo)))
    return;

  /* If we're building a construction vtable, we're not interested in
     subobjects that don't require construction vtables.  */
  if (ctor_vtbl_p
      && !CLASSTYPE_VBASECLASSES (BINFO_TYPE (binfo))
      && !binfo_via_virtual (orig_binfo, BINFO_TYPE (rtti_binfo)))
    return;

  /* Build the initializers for the BINFO-in-T vtable.  */
  dfs_accumulate_vtbl_inits (binfo, orig_binfo, rtti_binfo, vtbl, t, inits);

  /* Walk the BINFO and its bases.  We walk in preorder so that as we
     initialize each vtable we can figure out at what offset the
     secondary vtable lies from the primary vtable.  We can't use
     dfs_walk here because we need to iterate through bases of BINFO
     and RTTI_BINFO simultaneously.  */
  for (i = 0; BINFO_BASE_ITERATE (binfo, i, base_binfo); ++i)
    {
      /* Skip virtual bases.  */
      if (BINFO_VIRTUAL_P (base_binfo))
	continue;
      accumulate_vtbl_inits (base_binfo,
			     BINFO_BASE_BINFO (orig_binfo, i),
			     rtti_binfo, vtbl, t,
			     inits);
    }
}

/* Called from accumulate_vtbl_inits.  Adds the initializers for the
   BINFO vtable to L.  */

static void
dfs_accumulate_vtbl_inits (tree binfo,
			   tree orig_binfo,
			   tree rtti_binfo,
			   tree orig_vtbl,
			   tree t,
			   vec<constructor_elt, va_gc> **l)
{
  tree vtbl = NULL_TREE;
  int ctor_vtbl_p = !SAME_BINFO_TYPE_P (BINFO_TYPE (rtti_binfo), t);
  int n_inits;

  if (ctor_vtbl_p
      && BINFO_VIRTUAL_P (orig_binfo) && BINFO_PRIMARY_P (orig_binfo))
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

      tree b;
      tree last = NULL_TREE;

      /* First, look through the bases we are primary to for RTTI_BINFO
	 or a virtual base.  */
      b = binfo;
      while (BINFO_PRIMARY_P (b))
	{
	  b = BINFO_INHERITANCE_CHAIN (b);
	  last = b;
	  if (BINFO_VIRTUAL_P (b) || b == rtti_binfo)
	    goto found;
	}
      /* If we run out of primary links, keep looking down our
	 inheritance chain; we might be an indirect primary.  */
      for (b = last; b; b = BINFO_INHERITANCE_CHAIN (b))
	if (BINFO_VIRTUAL_P (b) || b == rtti_binfo)
	  break;
    found:

      /* If we found RTTI_BINFO, this is case 1.  If we found a virtual
	 base B and it is a base of RTTI_BINFO, this is case 2.  In
	 either case, we share our vtable with LAST, i.e. the
	 derived-most base within B of which we are a primary.  */
      if (b == rtti_binfo
	  || (b && binfo_for_vbase (BINFO_TYPE (b), BINFO_TYPE (rtti_binfo))))
	/* Just set our BINFO_VTABLE to point to LAST, as we may not have
	   set LAST's BINFO_VTABLE yet.  We'll extract the actual vptr in
	   binfo_ctor_vtable after everything's been set up.  */
	vtbl = last;

      /* Otherwise, this is case 3 and we get our own.  */
    }
  else if (!BINFO_NEW_VTABLE_MARKED (orig_binfo))
    return;

  n_inits = vec_safe_length (*l);

  if (!vtbl)
    {
      tree index;
      int non_fn_entries;

      /* Add the initializer for this vtable.  */
      build_vtbl_initializer (binfo, orig_binfo, t, rtti_binfo,
                              &non_fn_entries, l);

      /* Figure out the position to which the VPTR should point.  */
      vtbl = build1 (ADDR_EXPR, vtbl_ptr_type_node, orig_vtbl);
      index = size_binop (MULT_EXPR,
			  TYPE_SIZE_UNIT (vtable_entry_type),
			  size_int (non_fn_entries + n_inits));
      vtbl = fold_build_pointer_plus (vtbl, index);
    }

  if (ctor_vtbl_p)
    /* For a construction vtable, we can't overwrite BINFO_VTABLE.
       So, we make a TREE_LIST.  Later, dfs_fixup_binfo_vtbls will
       straighten this out.  */
    BINFO_VTABLE (binfo) = tree_cons (rtti_binfo, vtbl, BINFO_VTABLE (binfo));
  else if (BINFO_PRIMARY_P (binfo) && BINFO_VIRTUAL_P (binfo))
    /* Throw away any unneeded intializers.  */
    (*l)->truncate (n_inits);
  else
     /* For an ordinary vtable, set BINFO_VTABLE.  */
    BINFO_VTABLE (binfo) = vtbl;
}

static GTY(()) tree abort_fndecl_addr;

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

static void
build_vtbl_initializer (tree binfo,
			tree orig_binfo,
			tree t,
			tree rtti_binfo,
			int* non_fn_entries_p,
			vec<constructor_elt, va_gc> **inits)
{
  tree v;
  vtbl_init_data vid;
  unsigned ix, jx;
  tree vbinfo;
  vec<tree, va_gc> *vbases;
  constructor_elt *e;

  /* Initialize VID.  */
  memset (&vid, 0, sizeof (vid));
  vid.binfo = binfo;
  vid.derived = t;
  vid.rtti_binfo = rtti_binfo;
  vid.primary_vtbl_p = SAME_BINFO_TYPE_P (BINFO_TYPE (binfo), t);
  vid.ctor_vtbl_p = !SAME_BINFO_TYPE_P (BINFO_TYPE (rtti_binfo), t);
  vid.generate_vcall_entries = true;
  /* The first vbase or vcall offset is at index -3 in the vtable.  */
  vid.index = ssize_int(-3 * TARGET_VTABLE_DATA_ENTRY_DISTANCE);

  /* Add entries to the vtable for RTTI.  */
  build_rtti_vtbl_entries (binfo, &vid);

  /* Create an array for keeping track of the functions we've
     processed.  When we see multiple functions with the same
     signature, we share the vcall offsets.  */
  vec_alloc (vid.fns, 32);
  /* Add the vcall and vbase offset entries.  */
  build_vcall_and_vbase_vtbl_entries (binfo, &vid);

  /* Clear BINFO_VTABLE_PATH_MARKED; it's set by
     build_vbase_offset_vtbl_entries.  */
  for (vbases = CLASSTYPE_VBASECLASSES (t), ix = 0;
       vec_safe_iterate (vbases, ix, &vbinfo); ix++)
    BINFO_VTABLE_PATH_MARKED (vbinfo) = 0;

  /* If the target requires padding between data entries, add that now.  */
  if (TARGET_VTABLE_DATA_ENTRY_DISTANCE > 1)
    {
      int n_entries = vec_safe_length (vid.inits);

      vec_safe_grow (vid.inits, TARGET_VTABLE_DATA_ENTRY_DISTANCE * n_entries);

      /* Move data entries into their new positions and add padding
	 after the new positions.  Iterate backwards so we don't
	 overwrite entries that we would need to process later.  */
      for (ix = n_entries - 1;
	   vid.inits->iterate (ix, &e);
	   ix--)
	{
	  int j;
	  int new_position = (TARGET_VTABLE_DATA_ENTRY_DISTANCE * ix
			      + (TARGET_VTABLE_DATA_ENTRY_DISTANCE - 1));

	  (*vid.inits)[new_position] = *e;

	  for (j = 1; j < TARGET_VTABLE_DATA_ENTRY_DISTANCE; ++j)
	    {
	      constructor_elt *f = &(*vid.inits)[new_position - j];
	      f->index = NULL_TREE;
	      f->value = build1 (NOP_EXPR, vtable_entry_type,
				 null_pointer_node);
	    }
	}
    }

  if (non_fn_entries_p)
    *non_fn_entries_p = vec_safe_length (vid.inits);

  /* The initializers for virtual functions were built up in reverse
     order.  Straighten them out and add them to the running list in one
     step.  */
  jx = vec_safe_length (*inits);
  vec_safe_grow (*inits, jx + vid.inits->length ());

  for (ix = vid.inits->length () - 1;
       vid.inits->iterate (ix, &e);
       ix--, jx++)
    (**inits)[jx] = *e;

  /* Go through all the ordinary virtual functions, building up
     initializers.  */
  for (v = BINFO_VIRTUALS (orig_binfo); v; v = TREE_CHAIN (v))
    {
      tree delta;
      tree vcall_index;
      tree fn, fn_original;
      tree init = NULL_TREE;

      fn = BV_FN (v);
      fn_original = fn;
      if (DECL_THUNK_P (fn))
	{
	  if (!DECL_NAME (fn))
	    finish_thunk (fn);
	  if (THUNK_ALIAS (fn))
	    {
	      fn = THUNK_ALIAS (fn);
	      BV_FN (v) = fn;
	    }
	  fn_original = THUNK_TARGET (fn);
	}

      /* If the only definition of this function signature along our
	 primary base chain is from a lost primary, this vtable slot will
	 never be used, so just zero it out.  This is important to avoid
	 requiring extra thunks which cannot be generated with the function.

	 We first check this in update_vtable_entry_for_fn, so we handle
	 restored primary bases properly; we also need to do it here so we
	 zero out unused slots in ctor vtables, rather than filling them
	 with erroneous values (though harmless, apart from relocation
	 costs).  */
      if (BV_LOST_PRIMARY (v))
	init = size_zero_node;

      if (! init)
	{
	  /* Pull the offset for `this', and the function to call, out of
	     the list.  */
	  delta = BV_DELTA (v);
	  vcall_index = BV_VCALL_INDEX (v);

	  gcc_assert (TREE_CODE (delta) == INTEGER_CST);
	  gcc_assert (TREE_CODE (fn) == FUNCTION_DECL);

	  /* You can't call an abstract virtual function; it's abstract.
	     So, we replace these functions with __pure_virtual.  */
	  if (DECL_PURE_VIRTUAL_P (fn_original))
	    {
	      fn = abort_fndecl;
	      if (!TARGET_VTABLE_USES_DESCRIPTORS)
		{
		  if (abort_fndecl_addr == NULL)
		    abort_fndecl_addr
		      = fold_convert (vfunc_ptr_type_node,
				      build_fold_addr_expr (fn));
		  init = abort_fndecl_addr;
		}
	    }
	  /* Likewise for deleted virtuals.  */
	  else if (DECL_DELETED_FN (fn_original))
	    {
	      fn = get_identifier ("__cxa_deleted_virtual");
	      if (!get_global_value_if_present (fn, &fn))
		fn = push_library_fn (fn, (build_function_type_list
					   (void_type_node, NULL_TREE)),
				      NULL_TREE, ECF_NORETURN);
	      if (!TARGET_VTABLE_USES_DESCRIPTORS)
		init = fold_convert (vfunc_ptr_type_node,
				     build_fold_addr_expr (fn));
	    }
	  else
	    {
	      if (!integer_zerop (delta) || vcall_index)
		{
		  fn = make_thunk (fn, /*this_adjusting=*/1, delta, vcall_index);
		  if (!DECL_NAME (fn))
		    finish_thunk (fn);
		}
	      /* Take the address of the function, considering it to be of an
		 appropriate generic type.  */
	      if (!TARGET_VTABLE_USES_DESCRIPTORS)
		init = fold_convert (vfunc_ptr_type_node,
				     build_fold_addr_expr (fn));
	    }
	}

      /* And add it to the chain of initializers.  */
      if (TARGET_VTABLE_USES_DESCRIPTORS)
	{
	  int i;
	  if (init == size_zero_node)
	    for (i = 0; i < TARGET_VTABLE_USES_DESCRIPTORS; ++i)
	      CONSTRUCTOR_APPEND_ELT (*inits, NULL_TREE, init);
	  else
	    for (i = 0; i < TARGET_VTABLE_USES_DESCRIPTORS; ++i)
	      {
		tree fdesc = build2 (FDESC_EXPR, vfunc_ptr_type_node,
				     fn, build_int_cst (NULL_TREE, i));
		TREE_CONSTANT (fdesc) = 1;

		CONSTRUCTOR_APPEND_ELT (*inits, NULL_TREE, fdesc);
	      }
	}
      else
	CONSTRUCTOR_APPEND_ELT (*inits, NULL_TREE, init);
    }
}

/* Adds to vid->inits the initializers for the vbase and vcall
   offsets in BINFO, which is in the hierarchy dominated by T.  */

static void
build_vcall_and_vbase_vtbl_entries (tree binfo, vtbl_init_data* vid)
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
build_vbase_offset_vtbl_entries (tree binfo, vtbl_init_data* vid)
{
  tree vbase;
  tree t;
  tree non_primary_binfo;

  /* If there are no virtual baseclasses, then there is nothing to
     do.  */
  if (!CLASSTYPE_VBASECLASSES (BINFO_TYPE (binfo)))
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
      if (BINFO_VIRTUAL_P (non_primary_binfo))
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

      if (!BINFO_VIRTUAL_P (vbase))
	continue;

      /* Find the instance of this virtual base in the complete
	 object.  */
      b = copied_binfo (vbase, binfo);

      /* If we've already got an offset for this virtual base, we
	 don't need another one.  */
      if (BINFO_VTABLE_PATH_MARKED (b))
	continue;
      BINFO_VTABLE_PATH_MARKED (b) = 1;

      /* Figure out where we can find this vbase offset.  */
      delta = size_binop (MULT_EXPR,
			  vid->index,
			  convert (ssizetype,
				   TYPE_SIZE_UNIT (vtable_entry_type)));
      if (vid->primary_vtbl_p)
	BINFO_VPTR_FIELD (b) = delta;

      if (binfo != TYPE_BINFO (t))
	/* The vbase offset had better be the same.  */
	gcc_assert (tree_int_cst_equal (delta, BINFO_VPTR_FIELD (vbase)));

      /* The next vbase will come at a more negative offset.  */
      vid->index = size_binop (MINUS_EXPR, vid->index,
			       ssize_int (TARGET_VTABLE_DATA_ENTRY_DISTANCE));

      /* The initializer is the delta from BINFO to this virtual base.
	 The vbase offsets go in reverse inheritance-graph order, and
	 we are walking in inheritance graph order so these end up in
	 the right order.  */
      delta = size_diffop_loc (input_location,
			   BINFO_OFFSET (b), BINFO_OFFSET (non_primary_binfo));

      CONSTRUCTOR_APPEND_ELT (vid->inits, NULL_TREE,
			      fold_build1_loc (input_location, NOP_EXPR,
					       vtable_entry_type, delta));
    }
}

/* Adds the initializers for the vcall offset entries in the vtable
   for BINFO (which is part of the class hierarchy dominated by VID->DERIVED)
   to VID->INITS.  */

static void
build_vcall_offset_vtbl_entries (tree binfo, vtbl_init_data* vid)
{
  /* We only need these entries if this base is a virtual base.  We
     compute the indices -- but do not add to the vtable -- when
     building the main vtable for a class.  */
  if (binfo == TYPE_BINFO (vid->derived)
      || (BINFO_VIRTUAL_P (binfo) 
	  /* If BINFO is RTTI_BINFO, then (since BINFO does not
	     correspond to VID->DERIVED), we are building a primary
	     construction virtual table.  Since this is a primary
	     virtual table, we do not need the vcall offsets for
	     BINFO.  */
	  && binfo != vid->rtti_binfo))
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
      if (!BINFO_VIRTUAL_P (binfo))
	vid->generate_vcall_entries = false;
      /* Now, walk through the non-virtual bases, adding vcall offsets.  */
      add_vcall_offset_vtbl_entries_r (binfo, vid);
    }
}

/* Build vcall offsets, starting with those for BINFO.  */

static void
add_vcall_offset_vtbl_entries_r (tree binfo, vtbl_init_data* vid)
{
  int i;
  tree primary_binfo;
  tree base_binfo;

  /* Don't walk into virtual bases -- except, of course, for the
     virtual base for which we are building vcall offsets.  Any
     primary virtual base will have already had its offsets generated
     through the recursion in build_vcall_and_vbase_vtbl_entries.  */
  if (BINFO_VIRTUAL_P (binfo) && vid->vbase != binfo)
    return;

  /* If BINFO has a primary base, process it first.  */
  primary_binfo = get_primary_binfo (binfo);
  if (primary_binfo)
    add_vcall_offset_vtbl_entries_r (primary_binfo, vid);

  /* Add BINFO itself to the list.  */
  add_vcall_offset_vtbl_entries_1 (binfo, vid);

  /* Scan the non-primary bases of BINFO.  */
  for (i = 0; BINFO_BASE_ITERATE (binfo, i, base_binfo); ++i)
    if (base_binfo != primary_binfo)
      add_vcall_offset_vtbl_entries_r (base_binfo, vid);
}

/* Called from build_vcall_offset_vtbl_entries_r.  */

static void
add_vcall_offset_vtbl_entries_1 (tree binfo, vtbl_init_data* vid)
{
  /* Make entries for the rest of the virtuals.  */
  if (abi_version_at_least (2))
    {
      tree orig_fn;

      /* The ABI requires that the methods be processed in declaration
	 order.  G++ 3.2 used the order in the vtable.  */
      for (orig_fn = TYPE_METHODS (BINFO_TYPE (binfo));
	   orig_fn;
	   orig_fn = DECL_CHAIN (orig_fn))
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
	  if (BINFO_VIRTUAL_P (non_primary_binfo))
	    {
	      gcc_assert (non_primary_binfo == vid->vbase);
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
	non_primary_binfo
	  = original_binfo (non_primary_binfo, vid->rtti_binfo);

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
	  if (!SAME_BINFO_TYPE_P (BINFO_TYPE (binfo), DECL_CONTEXT (orig_fn)))
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
  tree derived_entry;

  /* If there is already an entry for a function with the same
     signature as FN, then we do not need a second vcall offset.
     Check the list of functions already present in the derived
     class vtable.  */
  FOR_EACH_VEC_SAFE_ELT (vid->fns, i, derived_entry)
    {
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
    {
      tree_pair_s elt = {orig_fn, vid->index};
      vec_safe_push (CLASSTYPE_VCALL_INDICES (vid->derived), elt);
    }

  /* The next vcall offset will be found at a more negative
     offset.  */
  vid->index = size_binop (MINUS_EXPR, vid->index,
			   ssize_int (TARGET_VTABLE_DATA_ENTRY_DISTANCE));

  /* Keep track of this function.  */
  vec_safe_push (vid->fns, orig_fn);

  if (vid->generate_vcall_entries)
    {
      tree base;
      tree fn;

      /* Find the overriding function.  */
      fn = find_final_overrider (vid->rtti_binfo, binfo, orig_fn);
      if (fn == error_mark_node)
	vcall_offset = build_zero_cst (vtable_entry_type);
      else
	{
	  base = TREE_VALUE (fn);

	  /* The vbase we're working on is a primary base of
	     vid->binfo.  But it might be a lost primary, so its
	     BINFO_OFFSET might be wrong, so we just use the
	     BINFO_OFFSET from vid->binfo.  */
	  vcall_offset = size_diffop_loc (input_location,
				      BINFO_OFFSET (base),
				      BINFO_OFFSET (vid->binfo));
	  vcall_offset = fold_build1_loc (input_location,
				      NOP_EXPR, vtable_entry_type,
				      vcall_offset);
	}
      /* Add the initializer to the vtable.  */
      CONSTRUCTOR_APPEND_ELT (vid->inits, NULL_TREE, vcall_offset);
    }
}

/* Return vtbl initializers for the RTTI entries corresponding to the
   BINFO's vtable.  The RTTI entries should indicate the object given
   by VID->rtti_binfo.  */

static void
build_rtti_vtbl_entries (tree binfo, vtbl_init_data* vid)
{
  tree b;
  tree t;
  tree offset;
  tree decl;
  tree init;

  t = BINFO_TYPE (vid->rtti_binfo);

  /* To find the complete object, we will first convert to our most
     primary base, and then add the offset in the vtbl to that value.  */
  b = binfo;
  while (CLASSTYPE_HAS_PRIMARY_BASE_P (BINFO_TYPE (b))
	 && !BINFO_LOST_PRIMARY_P (b))
    {
      tree primary_base;

      primary_base = get_primary_binfo (b);
      gcc_assert (BINFO_PRIMARY_P (primary_base)
		  && BINFO_INHERITANCE_CHAIN (primary_base) == b);
      b = primary_base;
    }
  offset = size_diffop_loc (input_location,
			BINFO_OFFSET (vid->rtti_binfo), BINFO_OFFSET (b));

  /* The second entry is the address of the typeinfo object.  */
  if (flag_rtti)
    decl = build_address (get_tinfo_decl (t));
  else
    decl = integer_zero_node;

  /* Convert the declaration to a type that can be stored in the
     vtable.  */
  init = build_nop (vfunc_ptr_type_node, decl);
  CONSTRUCTOR_APPEND_ELT (vid->inits, NULL_TREE, init);

  /* Add the offset-to-top entry.  It comes earlier in the vtable than
     the typeinfo entry.  Convert the offset to look like a
     function pointer, so that we can put it in the vtable.  */
  init = build_nop (vfunc_ptr_type_node, offset);
  CONSTRUCTOR_APPEND_ELT (vid->inits, NULL_TREE, init);
}

/* TRUE iff TYPE is uniquely derived from PARENT.  Ignores
   accessibility.  */

bool
uniquely_derived_from_p (tree parent, tree type)
{
  tree base = lookup_base (type, parent, ba_unique, NULL, tf_none);
  return base && base != error_mark_node;
}

/* TRUE iff TYPE is publicly & uniquely derived from PARENT.  */

bool
publicly_uniquely_derived_p (tree parent, tree type)
{
  tree base = lookup_base (type, parent, ba_ignore_scope | ba_check,
			   NULL, tf_none);
  return base && base != error_mark_node;
}

/* CTX1 and CTX2 are declaration contexts.  Return the innermost common
   class between them, if any.  */

tree
common_enclosing_class (tree ctx1, tree ctx2)
{
  if (!TYPE_P (ctx1) || !TYPE_P (ctx2))
    return NULL_TREE;
  gcc_assert (ctx1 == TYPE_MAIN_VARIANT (ctx1)
	      && ctx2 == TYPE_MAIN_VARIANT (ctx2));
  if (ctx1 == ctx2)
    return ctx1;
  for (tree t = ctx1; TYPE_P (t); t = TYPE_CONTEXT (t))
    TYPE_MARKED_P (t) = true;
  tree found = NULL_TREE;
  for (tree t = ctx2; TYPE_P (t); t = TYPE_CONTEXT (t))
    if (TYPE_MARKED_P (t))
      {
	found = t;
	break;
      }
  for (tree t = ctx1; TYPE_P (t); t = TYPE_CONTEXT (t))
    TYPE_MARKED_P (t) = false;
  return found;
}

#include "gt-cp-class.h"
