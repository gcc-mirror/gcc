/* Functions related to building classes and their related objects.
   Copyright (C) 1987, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000 Free Software Foundation, Inc.
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

#include "obstack.h"
#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

/* This is how we tell when two virtual member functions are really the
   same.  */
#define SAME_FN(FN1DECL, FN2DECL) (DECL_ASSEMBLER_NAME (FN1DECL) == DECL_ASSEMBLER_NAME (FN2DECL))

extern void set_class_shadows PARAMS ((tree));

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

/* The stack itself.  This is an dynamically resized array.  The
   number of elements allocated is CURRENT_CLASS_STACK_SIZE.  */
static int current_class_stack_size;
static class_stack_node_t current_class_stack;

static tree get_vfield_name PARAMS ((tree));
static void finish_struct_anon PARAMS ((tree));
static tree build_vbase_pointer PARAMS ((tree, tree));
static tree build_vtable_entry PARAMS ((tree, tree, tree));
static tree get_vtable_name PARAMS ((tree));
static tree get_derived_offset PARAMS ((tree, tree));
static tree get_basefndecls PARAMS ((tree, tree));
static void set_rtti_entry PARAMS ((tree, tree, tree));
static int build_primary_vtable PARAMS ((tree, tree));
static int build_secondary_vtable PARAMS ((tree, tree));
static tree dfs_finish_vtbls PARAMS ((tree, void *));
static tree dfs_accumulate_vtbl_inits PARAMS ((tree, void *));
static void finish_vtbls PARAMS ((tree));
static void modify_vtable_entry PARAMS ((tree, tree, tree, tree, tree *));
static void add_virtual_function PARAMS ((tree *, tree *, int *, tree, tree));
static tree delete_duplicate_fields_1 PARAMS ((tree, tree));
static void delete_duplicate_fields PARAMS ((tree));
static void finish_struct_bits PARAMS ((tree));
static int alter_access PARAMS ((tree, tree, tree));
static void handle_using_decl PARAMS ((tree, tree));
static int overrides PARAMS ((tree, tree));
static int strictly_overrides PARAMS ((tree, tree));
static void mark_overriders PARAMS ((tree, tree));
static void check_for_override PARAMS ((tree, tree));
static tree dfs_modify_vtables PARAMS ((tree, void *));
static tree modify_all_vtables PARAMS ((tree, int *, tree));
static void determine_primary_base PARAMS ((tree, int *));
static void finish_struct_methods PARAMS ((tree));
static void maybe_warn_about_overly_private_class PARAMS ((tree));
static int field_decl_cmp PARAMS ((const tree *, const tree *));
static int method_name_cmp PARAMS ((const tree *, const tree *));
static tree add_implicitly_declared_members PARAMS ((tree, int, int, int));
static tree fixed_type_or_null PARAMS ((tree, int *));
static tree resolve_address_of_overloaded_function PARAMS ((tree, tree, int,
							  int, tree));
static void build_vtable_entry_ref PARAMS ((tree, tree, tree));
static tree build_vtbl_initializer PARAMS ((tree, tree));
static int count_fields PARAMS ((tree));
static int add_fields_to_vec PARAMS ((tree, tree, int));
static void check_bitfield_decl PARAMS ((tree));
static void check_field_decl PARAMS ((tree, tree, int *, int *, int *, int *));
static void check_field_decls PARAMS ((tree, tree *, int *, int *, int *, 
				     int *));
static void build_base_field PARAMS ((record_layout_info, tree, int *,
				      unsigned int *, varray_type *));
static varray_type build_base_fields PARAMS ((record_layout_info, int *));
static tree build_vbase_pointer_fields PARAMS ((record_layout_info, int *));
static tree build_vtbl_or_vbase_field PARAMS ((tree, tree, tree, tree, tree,
					       int *));
static void check_methods PARAMS ((tree));
static void remove_zero_width_bit_fields PARAMS ((tree));
static void check_bases PARAMS ((tree, int *, int *, int *));
static void check_bases_and_members PARAMS ((tree, int *));
static tree create_vtable_ptr PARAMS ((tree, int *, int *, tree *, tree *));
static void layout_class_type PARAMS ((tree, int *, int *, tree *, tree *));
static void fixup_pending_inline PARAMS ((struct pending_inline *));
static void fixup_inline_methods PARAMS ((tree));
static void set_primary_base PARAMS ((tree, int, int *));
static tree dfs_propagate_binfo_offsets PARAMS ((tree, void *));
static void propagate_binfo_offsets PARAMS ((tree, tree));
static void layout_basetypes PARAMS ((tree));
static void layout_virtual_bases PARAMS ((tree));
static tree dfs_set_offset_for_shared_vbases PARAMS ((tree, void *));
static tree dfs_set_offset_for_unshared_vbases PARAMS ((tree, void *));
static tree dfs_build_vbase_offset_vtbl_entries PARAMS ((tree, void *));
static tree build_vbase_offset_vtbl_entries PARAMS ((tree, tree));
static tree dfs_vcall_offset_queue_p PARAMS ((tree, void *));
static tree dfs_build_vcall_offset_vtbl_entries PARAMS ((tree, void *));
static tree build_vcall_offset_vtbl_entries PARAMS ((tree, tree));
static tree dfs_count_virtuals PARAMS ((tree, void *));
static void start_vtable PARAMS ((tree, int *));
static void layout_vtable_decl PARAMS ((tree, int));
static int num_vfun_entries PARAMS ((tree));
static tree dfs_find_final_overrider PARAMS ((tree, void *));
static tree find_final_overrider PARAMS ((tree, tree, tree));
static tree dfs_find_base PARAMS ((tree, void *));
static int make_new_vtable PARAMS ((tree, tree));
extern void dump_class_hierarchy PARAMS ((tree, int));
static tree build_vtable PARAMS ((tree, tree, tree));
static void initialize_vtable PARAMS ((tree, tree));
static void layout_nonempty_base_or_field PARAMS ((record_layout_info,
						   tree, tree,
						   varray_type));

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

/* Virtual base class layout.  */

/* Returns a list of virtual base class pointers as a chain of
   FIELD_DECLS.  */

static tree
build_vbase_pointer_fields (rli, empty_p)
     record_layout_info rli;
     int *empty_p;
{
  /* Chain to hold all the new FIELD_DECLs which point at virtual
     base classes.  */
  tree rec = rli->t;
  tree vbase_decls = NULL_TREE;
  tree binfos = TYPE_BINFO_BASETYPES (rec);
  int n_baseclasses = CLASSTYPE_N_BASECLASSES (rec);
  tree decl;
  int i;

  /* Under the new ABI, there are no vbase pointers in the object.
     Instead, the offsets are stored in the vtable.  */
  if (vbase_offsets_in_vtable_p ())
    return NULL_TREE;

  /* Loop over the baseclasses, adding vbase pointers as needed.  */
  for (i = 0; i < n_baseclasses; i++)
    {
      register tree base_binfo = TREE_VEC_ELT (binfos, i);
      register tree basetype = BINFO_TYPE (base_binfo);

      if (!COMPLETE_TYPE_P (basetype))
	/* This error is now reported in xref_tag, thus giving better
	   location information.  */
	continue;

      /* All basetypes are recorded in the association list of the
	 derived type.  */

      if (TREE_VIA_VIRTUAL (base_binfo))
	{
	  int j;
	  const char *name;

	  /* The offset for a virtual base class is only used in computing
	     virtual function tables and for initializing virtual base
	     pointers.  It is built once `get_vbase_types' is called.  */

	  /* If this basetype can come from another vbase pointer
	     without an additional indirection, we will share
	     that pointer.  If an indirection is involved, we
	     make our own pointer.  */
	  for (j = 0; j < n_baseclasses; j++)
	    {
	      tree other_base_binfo = TREE_VEC_ELT (binfos, j);
	      if (! TREE_VIA_VIRTUAL (other_base_binfo)
		  && BINFO_FOR_VBASE (basetype, BINFO_TYPE (other_base_binfo)))
		goto got_it;
	    }
	  FORMAT_VBASE_NAME (name, basetype);
	  decl = build_vtbl_or_vbase_field (get_identifier (name), 
					    get_identifier (VTABLE_BASE),
					    build_pointer_type (basetype),
					    rec,
					    basetype,
					    empty_p);
	  BINFO_VPTR_FIELD (base_binfo) = decl;
	  TREE_CHAIN (decl) = vbase_decls;
	  layout_field (rli, decl);
	  vbase_decls = decl;
	  *empty_p = 0;

	got_it:
	  /* The space this decl occupies has already been accounted for.  */
	  ;
	}
    }

  return vbase_decls;
}

/* Called from build_vbase_offset_vtbl_entries via dfs_walk.  */

static tree
dfs_build_vbase_offset_vtbl_entries (binfo, data)
     tree binfo;
     void *data;
{
  tree list = (tree) data;

  if (TREE_TYPE (list) == binfo)
    /* The TREE_TYPE of LIST is the base class from which we started
       walking.  If that BINFO is virtual it's not a virtual baseclass
       of itself.  */
    ;
  else if (TREE_VIA_VIRTUAL (binfo))
    {
      tree init;
      tree vbase;

      /* Remember the index to the vbase offset for this virtual
	 base.  */
      vbase = BINFO_FOR_VBASE (TREE_TYPE (binfo), TREE_PURPOSE (list));
      if (!TREE_VALUE (list))
	BINFO_VPTR_FIELD (vbase) = build_int_2 (-1, 0);
      else
	{
	  BINFO_VPTR_FIELD (vbase) = TREE_PURPOSE (TREE_VALUE (list));
	  BINFO_VPTR_FIELD (vbase)
	    = fold (build (MINUS_EXPR, integer_type_node,
			   BINFO_VPTR_FIELD (vbase), integer_one_node));
	}

      /* And record the offset at which this virtual base lies in the
	 vtable.  */
      init = BINFO_OFFSET (binfo);
      TREE_VALUE (list) = tree_cons (BINFO_VPTR_FIELD (vbase),
				     init, TREE_VALUE (list));
    }

  SET_BINFO_VTABLE_PATH_MARKED (binfo);
  
  return NULL_TREE;
}

/* Returns the initializers for the vbase offset entries in the vtable
   for BINFO (which is part of the class hierarchy dominated by T), in
   reverse order.  */

static tree
build_vbase_offset_vtbl_entries (binfo, t)
     tree binfo;
     tree t;
{
  tree inits;
  tree init;
  tree list;

  /* Under the old ABI, pointers to virtual bases are stored in each
     object.  */
  if (!vbase_offsets_in_vtable_p ())
    return NULL_TREE;

  /* If there are no virtual baseclasses, then there is nothing to
     do.  */
  if (!TYPE_USES_VIRTUAL_BASECLASSES (BINFO_TYPE (binfo)))
    return NULL_TREE;

  inits = NULL_TREE;

  /* The offsets are allocated in the reverse order of a
     depth-first left-to-right traversal of the hierarchy.  We use
     BINFO_VTABLE_PATH_MARKED because we are ourselves during a
     dfs_walk, and so BINFO_MARKED is already in use.  */
  list = build_tree_list (t, NULL_TREE);
  TREE_TYPE (list) = binfo;
  dfs_walk (binfo,
	    dfs_build_vbase_offset_vtbl_entries,
	    dfs_vtable_path_unmarked_real_bases_queue_p,
	    list);
  dfs_walk (binfo,
	    dfs_vtable_path_unmark,
	    dfs_vtable_path_marked_real_bases_queue_p,
	    list);
  inits = nreverse (TREE_VALUE (list));

  /* We've now got offsets in the right order.  However, the offsets
     we've stored are offsets from the beginning of the complete
     object, and we need offsets from this BINFO.  */
  for (init = inits; init; init = TREE_CHAIN (init))
    {
      /* The dfs_build_vbase_offset_vtbl_entries routine uses the
	 TREE_PURPOSE to scribble in.  But, we need to clear it now so
	 that the values are not perceived as labeled initializers.  */
      TREE_PURPOSE (init) = NULL_TREE;
      TREE_VALUE (init)
	= fold (build1 (NOP_EXPR, vtable_entry_type,
			size_diffop (TREE_VALUE (init),
				     BINFO_OFFSET (binfo))));
    }

  return inits;
}

typedef struct vcall_offset_data_s
{
  /* The binfo for the most-derived type.  */
  tree derived;
  /* The binfo for the virtual base for which we're building
     initializers.  */
  tree vbase;
  /* The vcall offset initializers built up so far.  */
  tree inits;
  /* The number of vcall offsets accumulated.  */
  int offsets;
} vcall_offset_data;

/* Called from build_vcall_offset_vtbl_entries via dfs_walk.  */

static tree
dfs_vcall_offset_queue_p (binfo, data)
     tree binfo;
     void *data;
{
  vcall_offset_data* vod = (vcall_offset_data *) data;

  return (binfo == vod->vbase) ? binfo : dfs_skip_vbases (binfo, NULL);
}

/* Called from build_vcall_offset_vtbl_entries via dfs_walk.  */

static tree
dfs_build_vcall_offset_vtbl_entries (binfo, data)
     tree binfo;
     void *data;
{
  vcall_offset_data* vod;
  tree virtuals;
  tree binfo_inits;

  /* Primary bases are not interesting; all of the virtual
     function table entries have been overridden.  */
  if (BINFO_PRIMARY_MARKED_P (binfo))
     return NULL_TREE;

  vod = (vcall_offset_data *) data;
  binfo_inits = NULL_TREE;

  /* We chain the offsets on in reverse order.  That's correct --
     build_vtbl_initializer will straighten them out.  */
  for (virtuals = skip_rtti_stuff (binfo,
				   BINFO_TYPE (binfo),
				   NULL);
       virtuals;
       virtuals = TREE_CHAIN (virtuals))
    {
      /* Figure out what function we're looking at.  */
      tree fn = TREE_VALUE (virtuals);
      tree base = DECL_CONTEXT (fn);
      /* The FN comes from BASE.  So, we must caculate the adjustment
	 from the virtual base that derived from BINFO to BASE.  */
      tree base_binfo = get_binfo (base, vod->derived, /*protect=*/0);

      binfo_inits
	= tree_cons (NULL_TREE,
		     fold (build1 (NOP_EXPR, vtable_entry_type,
				   size_diffop (BINFO_OFFSET (base_binfo),
						BINFO_OFFSET (vod->vbase)))),
		     binfo_inits);
    }

  /* Now add the initializers we've just created to the list that will
     be returned to our caller.  */
  vod->inits = chainon (vod->inits, binfo_inits);

  return NULL_TREE;
}

/* Returns the initializers for the vcall offset entries in the vtable
   for BINFO (which is part of the class hierarchy dominated by T), in
   reverse order.  */

static tree
build_vcall_offset_vtbl_entries (binfo, t)
     tree binfo;
     tree t;
{
  vcall_offset_data vod;

  /* Under the old ABI, the adjustments to the `this' pointer were made
     elsewhere.  */
  if (!vcall_offsets_in_vtable_p ())
    return NULL_TREE;

  /* We only need these entries if this base is a virtual base.  */
  if (!TREE_VIA_VIRTUAL (binfo))
    return NULL_TREE;

  /* We need a vcall offset for each of the virtual functions in this
     vtable.  For example:

       class A { virtual void f (); };
       class B : virtual public A { };
       class C: virtual public A, public B {};
      
     Now imagine:

       B* b = new C;
       b->f();

     The location of `A' is not at a fixed offset relative to `B'; the
     offset depends on the complete object derived from `B'.  So, 
     `B' vtable contains an entry for `f' that indicates by what
     amount the `this' pointer for `B' needs to be adjusted to arrive
     at `A'.  

     We need entries for all the functions in our primary vtable and
     in our non-virtual bases vtables.  For each base, the entries
     appear in the same order as in the base; but the bases themselves
     appear in reverse depth-first, left-to-right order.  */
  vod.derived = t;
  vod.vbase = binfo;
  vod.inits = NULL_TREE;
  dfs_walk (binfo,
	    dfs_build_vcall_offset_vtbl_entries,
	    dfs_vcall_offset_queue_p,
	    &vod);

  return vod.inits;
}

/* Returns a pointer to the virtual base class of EXP that has the
   indicated TYPE.  EXP is of class type, not a pointer type.  */

static tree
build_vbase_pointer (exp, type)
     tree exp, type;
{
  if (vbase_offsets_in_vtable_p ())
    {
      tree vbase;
      tree vbase_ptr;

      /* Find the shared copy of TYPE; that's where the vtable offset
	 is recorded.  */
      vbase = BINFO_FOR_VBASE (type, TREE_TYPE (exp));
      /* Find the virtual function table pointer.  */
      vbase_ptr = build_vfield_ref (exp, TREE_TYPE (exp));
      /* Compute the location where the offset will lie.  */
      vbase_ptr = build_binary_op (PLUS_EXPR, 
				   vbase_ptr,
				   BINFO_VPTR_FIELD (vbase));
      vbase_ptr = build1 (NOP_EXPR, 
			  build_pointer_type (ptrdiff_type_node),
			  vbase_ptr);
      /* Add the contents of this location to EXP.  */
      return build (PLUS_EXPR,
		    build_pointer_type (type),
		    build_unary_op (ADDR_EXPR, exp, /*noconvert=*/0),
		    build1 (INDIRECT_REF, ptrdiff_type_node, vbase_ptr));
    }
  else
    {
      char *name;
      FORMAT_VBASE_NAME (name, type);
      return build_component_ref (exp, get_identifier (name), NULL_TREE, 0);
    }
}

/* Build multi-level access to EXPR using hierarchy path PATH.
   CODE is PLUS_EXPR if we are going with the grain,
   and MINUS_EXPR if we are not (in which case, we cannot traverse
   virtual baseclass links).

   TYPE is the type we want this path to have on exit.

   NONNULL is non-zero if  we know (for any reason) that EXPR is
   not, in fact, zero.  */

tree
build_vbase_path (code, type, expr, path, nonnull)
     enum tree_code code;
     tree type, expr, path;
     int nonnull;
{
  register int changed = 0;
  tree last = NULL_TREE, last_virtual = NULL_TREE;
  int fixed_type_p;
  tree null_expr = 0, nonnull_expr;
  tree basetype;
  tree offset = integer_zero_node;

  if (BINFO_INHERITANCE_CHAIN (path) == NULL_TREE)
    return build1 (NOP_EXPR, type, expr);

  /* We could do better if we had additional logic to convert back to the
     unconverted type (the static type of the complete object), and then
     convert back to the type we want.  Until that is done, we only optimize
     if the complete type is the same type as expr has.  */
  fixed_type_p = resolves_to_fixed_type_p (expr, &nonnull);

  if (!fixed_type_p && TREE_SIDE_EFFECTS (expr))
    expr = save_expr (expr);
  nonnull_expr = expr;

  path = reverse_path (path);

  basetype = BINFO_TYPE (path);

  while (path)
    {
      if (TREE_VIA_VIRTUAL (TREE_VALUE (path)))
	{
	  last_virtual = BINFO_TYPE (TREE_VALUE (path));
	  if (code == PLUS_EXPR)
	    {
	      changed = ! fixed_type_p;

	      if (changed)
		{
		  tree ind;

		  /* We already check for ambiguous things in the caller, just
		     find a path.  */
		  if (last)
		    {
		      tree binfo = get_binfo (last, TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (nonnull_expr))), 0);
		      nonnull_expr = convert_pointer_to_real (binfo, nonnull_expr);
		    }
		  ind = build_indirect_ref (nonnull_expr, NULL_PTR);
		  nonnull_expr = build_vbase_pointer (ind, last_virtual);
		  if (nonnull == 0
		      && TREE_CODE (type) == POINTER_TYPE
		      && null_expr == NULL_TREE)
		    {
		      null_expr = build1 (NOP_EXPR, build_pointer_type (last_virtual), integer_zero_node);
		      expr = build (COND_EXPR, build_pointer_type (last_virtual),
				    build (EQ_EXPR, boolean_type_node, expr,
					   integer_zero_node),
				    null_expr, nonnull_expr);
		    }
		}
	      /* else we'll figure out the offset below.  */

	      /* Happens in the case of parse errors.  */
	      if (nonnull_expr == error_mark_node)
		return error_mark_node;
	    }
	  else
	    {
	      cp_error ("cannot cast up from virtual baseclass `%T'",
			  last_virtual);
	      return error_mark_node;
	    }
	}
      last = TREE_VALUE (path);
      path = TREE_CHAIN (path);
    }
  /* LAST is now the last basetype assoc on the path.  */

  /* A pointer to a virtual base member of a non-null object
     is non-null.  Therefore, we only need to test for zeroness once.
     Make EXPR the canonical expression to deal with here.  */
  if (null_expr)
    {
      TREE_OPERAND (expr, 2) = nonnull_expr;
      TREE_TYPE (expr) = TREE_TYPE (TREE_OPERAND (expr, 1))
	= TREE_TYPE (nonnull_expr);
    }
  else
    expr = nonnull_expr;

  /* If we go through any virtual base pointers, make sure that
     casts to BASETYPE from the last virtual base class use
     the right value for BASETYPE.  */
  if (changed)
    {
      tree intype = TREE_TYPE (TREE_TYPE (expr));

      if (TYPE_MAIN_VARIANT (intype) != BINFO_TYPE (last))
	offset
	  = BINFO_OFFSET (get_binfo (last, TYPE_MAIN_VARIANT (intype), 0));
    }
  else
    offset = BINFO_OFFSET (last);

  if (! integer_zerop (offset))
    {
      /* Bash types to make the backend happy.  */
      offset = cp_convert (type, offset);

      /* If expr might be 0, we need to preserve that zeroness.  */
      if (nonnull == 0)
	{
	  if (null_expr)
	    TREE_TYPE (null_expr) = type;
	  else
	    null_expr = build1 (NOP_EXPR, type, integer_zero_node);
	  if (TREE_SIDE_EFFECTS (expr))
	    expr = save_expr (expr);

	  return build (COND_EXPR, type,
			build (EQ_EXPR, boolean_type_node, expr, integer_zero_node),
			null_expr,
			build (code, type, expr, offset));
	}
      else return build (code, type, expr, offset);
    }

  /* Cannot change the TREE_TYPE of a NOP_EXPR here, since it may
     be used multiple times in initialization of multiple inheritance.  */
  if (null_expr)
    {
      TREE_TYPE (expr) = type;
      return expr;
    }
  else
    return build1 (NOP_EXPR, type, expr);
}


/* Virtual function things.  */

/* Build an entry in the virtual function table.  DELTA is the offset
   for the `this' pointer.  VCALL_INDEX is the vtable index containing
   the vcall offset; zero if none.  ENTRY is the virtual function
   table entry itself.  It's TREE_TYPE must be VFUNC_PTR_TYPE_NODE,
   but it may not actually be a virtual function table pointer.  (For
   example, it might be the address of the RTTI object, under the new
   ABI.)  */

static tree
build_vtable_entry (delta, vcall_index, entry)
     tree delta;
     tree vcall_index;
     tree entry;
{
  if (flag_vtable_thunks)
    {
      HOST_WIDE_INT idelta;
      HOST_WIDE_INT ivindex;

      idelta = tree_low_cst (delta, 0);
      ivindex = tree_low_cst (vcall_index, 0);
      if ((idelta || ivindex) 
	  && ! DECL_PURE_VIRTUAL_P (TREE_OPERAND (entry, 0)))
	{
	  entry = make_thunk (entry, idelta, ivindex);
	  entry = build1 (ADDR_EXPR, vtable_entry_type, entry);
	  TREE_READONLY (entry) = 1;
	  TREE_CONSTANT (entry) = 1;
	}
#ifdef GATHER_STATISTICS
      n_vtable_entries += 1;
#endif
      return entry;
    }
  else
    {
      extern int flag_huge_objects;
      tree elems = tree_cons (NULL_TREE, delta,
			      tree_cons (NULL_TREE, integer_zero_node,
					 build_tree_list (NULL_TREE, entry)));
      tree entry = build (CONSTRUCTOR, vtable_entry_type, NULL_TREE, elems);

      /* We don't use vcall offsets when not using vtable thunks.  */
      my_friendly_assert (integer_zerop (vcall_index), 20000125);

      /* DELTA used to be constructed by `size_int' and/or size_binop,
	 which caused overflow problems when it was negative.  That should
	 be fixed now.  */

      if (! int_fits_type_p (delta, delta_type_node))
	{
	  if (flag_huge_objects)
	    sorry ("object size exceeds built-in limit for virtual function table implementation");
	  else
	    sorry ("object size exceeds normal limit for virtual function table implementation, recompile all source and use -fhuge-objects");
	}
      
      TREE_CONSTANT (entry) = 1;
      TREE_STATIC (entry) = 1;
      TREE_READONLY (entry) = 1;

#ifdef GATHER_STATISTICS
      n_vtable_entries += 1;
#endif

      return entry;
    }
}

/* We want to give the assembler the vtable identifier as well as
   the offset to the function pointer.  So we generate

   __asm__ __volatile__ (".vtable_entry %c0, %c1"
      : : "s"(&class_vtable),
          "i"((long)&vtbl[idx].pfn - (long)&vtbl[0])); */

static void
build_vtable_entry_ref (basetype, vtbl, idx)
     tree basetype, vtbl, idx;
{
  static char asm_stmt[] = ".vtable_entry %c0, %c1";
  tree s, i, i2;

  s = build_unary_op (ADDR_EXPR, TYPE_BINFO_VTABLE (basetype), 0);
  s = build_tree_list (build_string (1, "s"), s);

  i = build_array_ref (vtbl, idx);
  if (!flag_vtable_thunks)
    i = build_component_ref (i, pfn_identifier, vtable_entry_type, 0);
  i = build_c_cast (ptrdiff_type_node, build_unary_op (ADDR_EXPR, i, 0));
  i2 = build_array_ref (vtbl, build_int_2(0,0));
  i2 = build_c_cast (ptrdiff_type_node, build_unary_op (ADDR_EXPR, i2, 0));
  i = build_binary_op (MINUS_EXPR, i, i2);
  i = build_tree_list (build_string (1, "i"), i);

  finish_asm_stmt (ridpointers[RID_VOLATILE],
		   build_string (sizeof(asm_stmt)-1, asm_stmt),
		   NULL_TREE, chainon (s, i), NULL_TREE);
}

/* Given an object INSTANCE, return an expression which yields the
   virtual function vtable element corresponding to INDEX.  There are
   many special cases for INSTANCE which we take care of here, mainly
   to avoid creating extra tree nodes when we don't have to.  */

tree
build_vtbl_ref (instance, idx)
     tree instance, idx;
{
  tree vtbl, aref;
  tree basetype = TREE_TYPE (instance);

  if (TREE_CODE (basetype) == REFERENCE_TYPE)
    basetype = TREE_TYPE (basetype);

  if (instance == current_class_ref)
    vtbl = build_vfield_ref (instance, basetype);
  else
    {
      if (optimize)
	{
	  /* Try to figure out what a reference refers to, and
	     access its virtual function table directly.  */
	  tree ref = NULL_TREE;

	  if (TREE_CODE (instance) == INDIRECT_REF
	      && TREE_CODE (TREE_TYPE (TREE_OPERAND (instance, 0))) == REFERENCE_TYPE)
	    ref = TREE_OPERAND (instance, 0);
	  else if (TREE_CODE (TREE_TYPE (instance)) == REFERENCE_TYPE)
	    ref = instance;

	  if (ref && TREE_CODE (ref) == VAR_DECL
	      && DECL_INITIAL (ref))
	    {
	      tree init = DECL_INITIAL (ref);

	      while (TREE_CODE (init) == NOP_EXPR
		     || TREE_CODE (init) == NON_LVALUE_EXPR)
		init = TREE_OPERAND (init, 0);
	      if (TREE_CODE (init) == ADDR_EXPR)
		{
		  init = TREE_OPERAND (init, 0);
		  if (IS_AGGR_TYPE (TREE_TYPE (init))
		      && (TREE_CODE (init) == PARM_DECL
			  || TREE_CODE (init) == VAR_DECL))
		    instance = init;
		}
	    }
	}

      if (IS_AGGR_TYPE (TREE_TYPE (instance))
	  && (TREE_CODE (instance) == RESULT_DECL
	      || TREE_CODE (instance) == PARM_DECL
	      || TREE_CODE (instance) == VAR_DECL))
	vtbl = TYPE_BINFO_VTABLE (basetype);
      else
	vtbl = build_vfield_ref (instance, basetype);
    }

  assemble_external (vtbl);

  if (flag_vtable_gc)
    build_vtable_entry_ref (basetype, vtbl, idx);

  aref = build_array_ref (vtbl, idx);

  return aref;
}

/* Given an object INSTANCE, return an expression which yields the
   virtual function corresponding to INDEX.  There are many special
   cases for INSTANCE which we take care of here, mainly to avoid
   creating extra tree nodes when we don't have to.  */

tree
build_vfn_ref (ptr_to_instptr, instance, idx)
     tree *ptr_to_instptr, instance;
     tree idx;
{
  tree aref = build_vtbl_ref (instance, idx);

  /* When using thunks, there is no extra delta, and we get the pfn
     directly.  */
  if (flag_vtable_thunks)
    return aref;

  if (ptr_to_instptr)
    {
      /* Save the intermediate result in a SAVE_EXPR so we don't have to
	 compute each component of the virtual function pointer twice.  */ 
      if (TREE_CODE (aref) == INDIRECT_REF)
	TREE_OPERAND (aref, 0) = save_expr (TREE_OPERAND (aref, 0));

      *ptr_to_instptr
	= build (PLUS_EXPR, TREE_TYPE (*ptr_to_instptr),
		 *ptr_to_instptr,
		 cp_convert (ptrdiff_type_node,
			     build_component_ref (aref, delta_identifier, NULL_TREE, 0)));
    }

  return build_component_ref (aref, pfn_identifier, NULL_TREE, 0);
}

/* Return the name of the virtual function table (as an IDENTIFIER_NODE)
   for the given TYPE.  */

static tree
get_vtable_name (type)
     tree type;
{
  tree type_id = build_typename_overload (type);
  char *buf = (char *) alloca (strlen (VTABLE_NAME_PREFIX)
			       + IDENTIFIER_LENGTH (type_id) + 2);
  const char *ptr = IDENTIFIER_POINTER (type_id);
  int i;
  for (i = 0; ptr[i] == OPERATOR_TYPENAME_FORMAT[i]; i++) ;
#if 0
  /* We don't take off the numbers; build_secondary_vtable uses the
     DECL_ASSEMBLER_NAME for the type, which includes the number
     in `3foo'.  If we were to pull them off here, we'd end up with
     something like `_vt.foo.3bar', instead of a uniform definition.  */
  while (ptr[i] >= '0' && ptr[i] <= '9')
    i += 1;
#endif
  sprintf (buf, "%s%s", VTABLE_NAME_PREFIX, ptr+i);
  return get_identifier (buf);
}

/* Return the offset to the main vtable for a given base BINFO.  */

tree
get_vfield_offset (binfo)
     tree binfo;
{
  tree tmp
    = size_binop (FLOOR_DIV_EXPR,
		  bit_position (TYPE_VFIELD (BINFO_TYPE (binfo))),
		  bitsize_int (BITS_PER_UNIT));

  return size_binop (PLUS_EXPR, convert (sizetype, tmp),
		     BINFO_OFFSET (binfo));
}

/* Get the offset to the start of the original binfo that we derived
   this binfo from.  If we find TYPE first, return the offset only
   that far.  The shortened search is useful because the this pointer
   on method calling is expected to point to a DECL_CONTEXT (fndecl)
   object, and not a baseclass of it.   */


static tree
get_derived_offset (binfo, type)
     tree binfo, type;
{
  tree offset1 = get_vfield_offset (TYPE_BINFO (BINFO_TYPE (binfo)));
  tree offset2;
  int i;

  while (BINFO_BASETYPES (binfo)
	 && (i = CLASSTYPE_VFIELD_PARENT (BINFO_TYPE (binfo))) != -1)
    {
      tree binfos = BINFO_BASETYPES (binfo);
      if (BINFO_TYPE (binfo) == type)
	break;
      binfo = TREE_VEC_ELT (binfos, i);
    }

  offset2 = get_vfield_offset (TYPE_BINFO (BINFO_TYPE (binfo)));
  return size_binop (MINUS_EXPR, offset1, offset2);
}

/* Update the rtti info for this class.  */

static void
set_rtti_entry (virtuals, offset, type)
     tree virtuals, offset, type;
{
  tree decl;

  if (CLASSTYPE_COM_INTERFACE (type))
    return;

  if (flag_rtti)
    decl = get_tinfo_decl (type);
  else if (!new_abi_rtti_p ())
    /* If someone tries to get RTTI information for a type compiled
       without RTTI, they're out of luck.  By calling __pure_virtual
       in this case, we give a small clue as to what went wrong.  We
       could consider having a __no_typeinfo function as well, for a
       more specific hint.  */
    decl = abort_fndecl;
  else
    /* For the new-abi, we just point to the type_info object.  */
    decl = NULL_TREE;

  if (flag_vtable_thunks)
    {
      /* The first slot holds the offset.  */
      BV_DELTA (virtuals) = offset;
      BV_VCALL_INDEX (virtuals) = integer_zero_node;

      /* The next node holds the decl.  */
      virtuals = TREE_CHAIN (virtuals);
      offset = integer_zero_node;
    }

  /* This slot holds the function to call.  */
  BV_DELTA (virtuals) = offset;
  BV_VCALL_INDEX (virtuals) = integer_zero_node;
  BV_FN (virtuals) = decl;
}

/* Create a VAR_DECL for a primary or secondary vtable for
   CLASS_TYPE.  Use NAME for the name of the vtable, and VTABLE_TYPE
   for its type.  */

static tree
build_vtable (class_type, name, vtable_type)
     tree class_type;
     tree name;
     tree vtable_type;
{
  tree decl;

  decl = build_lang_decl (VAR_DECL, name, vtable_type);
  DECL_CONTEXT (decl) = class_type;
  DECL_ARTIFICIAL (decl) = 1;
  TREE_STATIC (decl) = 1;
#ifndef WRITABLE_VTABLES
  /* Make them READONLY by default. (mrs) */
  TREE_READONLY (decl) = 1;
#endif
  DECL_VIRTUAL_P (decl) = 1;
  import_export_vtable (decl, class_type, 0);

  return decl;
}

/* Get the VAR_DECL of the vtable for TYPE. TYPE need not be polymorphic,
   or even complete.  If this does not exist, create it.  If COMPLETE is
   non-zero, then complete the definition of it -- that will render it
   impossible to actually build the vtable, but is useful to get at those
   which are known to exist in the runtime.  */

tree 
get_vtable_decl (type, complete)
     tree type;
     int complete;
{
  tree name = get_vtable_name (type);
  tree decl = IDENTIFIER_GLOBAL_VALUE (name);
  
  if (decl)
    {
      my_friendly_assert (TREE_CODE (decl) == VAR_DECL
                          && DECL_VIRTUAL_P (decl), 20000118);
      return decl;
    }
  
  decl = build_vtable (type, name, void_type_node);
  decl = pushdecl_top_level (decl);
  SET_IDENTIFIER_GLOBAL_VALUE (name, decl);
  
  /* At one time the vtable info was grabbed 2 words at a time.  This
     fails on sparc unless you have 8-byte alignment.  (tiemann) */
  DECL_ALIGN (decl) = MAX (TYPE_ALIGN (double_type_node),
			   DECL_ALIGN (decl));

  if (complete)
    cp_finish_decl (decl, NULL_TREE, NULL_TREE, 0);

  return decl;
}

/* Build the primary virtual function table for TYPE.  If BINFO is
   non-NULL, build the vtable starting with the initial approximation
   that it is the same as the one which is the head of the association
   list.  Returns a non-zero value if a new vtable is actually
   created.  */

static int
build_primary_vtable (binfo, type)
     tree binfo, type;
{
  tree virtuals, decl;

  decl = get_vtable_decl (type, /*complete=*/0);
  
  if (binfo)
    {
      tree offset;

      if (BINFO_NEW_VTABLE_MARKED (binfo))
	/* We have already created a vtable for this base, so there's
	   no need to do it again.  */
	return 0;
      
      virtuals = copy_list (BINFO_VIRTUALS (binfo));
      TREE_TYPE (decl) = TREE_TYPE (BINFO_VTABLE (binfo));
      DECL_SIZE (decl) = TYPE_SIZE (TREE_TYPE (BINFO_VTABLE (binfo)));
      DECL_SIZE_UNIT (decl)
	= TYPE_SIZE_UNIT (TREE_TYPE (BINFO_VTABLE (binfo)));

      /* Now do rtti stuff.  */
      offset = get_derived_offset (TYPE_BINFO (type), NULL_TREE);
      offset = size_diffop (size_zero_node, offset);
      set_rtti_entry (virtuals, offset, type);
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

  binfo = TYPE_BINFO (type);
  SET_BINFO_NEW_VTABLE_MARKED (binfo);
  return 1;
}

/* Give TYPE a new virtual function table which is initialized
   with a skeleton-copy of its original initialization.  The only
   entry that changes is the `delta' entry, so we can really
   share a lot of structure.

   FOR_TYPE is the derived type which caused this table to
   be needed.

   BINFO is the type association which provided TYPE for FOR_TYPE.

   The order in which vtables are built (by calling this function) for
   an object must remain the same, otherwise a binary incompatibility
   can result.  */

static int
build_secondary_vtable (binfo, for_type)
     tree binfo, for_type;
{
  tree basetype;
  tree orig_decl = BINFO_VTABLE (binfo);
  tree name;
  tree new_decl;
  tree offset;
  tree path = binfo;
  char *buf, *buf2;
  char joiner = '_';
  int i;

#ifdef JOINER
  joiner = JOINER;
#endif

  if (TREE_VIA_VIRTUAL (binfo))
    my_friendly_assert (binfo == BINFO_FOR_VBASE (BINFO_TYPE (binfo),
						  current_class_type),
			170);

  if (BINFO_NEW_VTABLE_MARKED (binfo))
    /* We already created a vtable for this base.  There's no need to
       do it again.  */
    return 0;

  /* Remember that we've created a vtable for this BINFO, so that we
     don't try to do so again.  */
  SET_BINFO_NEW_VTABLE_MARKED (binfo);
  
  /* Make fresh virtual list, so we can smash it later.  */
  BINFO_VIRTUALS (binfo) = copy_list (BINFO_VIRTUALS (binfo));

  if (TREE_VIA_VIRTUAL (binfo))
    {
      tree binfo1 = BINFO_FOR_VBASE (BINFO_TYPE (binfo), for_type);

      /* XXX - This should never happen, if it does, the caller should
	 ensure that the binfo is from for_type's binfos, not from any
	 base type's.  We can remove all this code after a while.  */
      if (binfo1 != binfo)
	warning ("internal inconsistency: binfo offset error for rtti");

      offset = BINFO_OFFSET (binfo1);
    }
  else
    offset = BINFO_OFFSET (binfo);

  set_rtti_entry (BINFO_VIRTUALS (binfo),
		  size_diffop (size_zero_node, offset),
		  for_type);

  /* In the new ABI, secondary vtables are laid out as part of the
     same structure as the primary vtable.  */
  if (merge_primary_and_secondary_vtables_p ())
    {
      BINFO_VTABLE (binfo) = NULL_TREE;
      return 1;
    }

  /* Create the declaration for the secondary vtable.  */
  basetype = TYPE_MAIN_VARIANT (BINFO_TYPE (binfo));
  buf2 = TYPE_ASSEMBLER_NAME_STRING (basetype);
  i = TYPE_ASSEMBLER_NAME_LENGTH (basetype) + 1;

  /* We know that the vtable that we are going to create doesn't exist
     yet in the global namespace, and when we finish, it will be
     pushed into the global namespace.  In complex MI hierarchies, we
     have to loop while the name we are thinking of adding is globally
     defined, adding more name components to the vtable name as we
     loop, until the name is unique.  This is because in complex MI
     cases, we might have the same base more than once.  This means
     that the order in which this function is called for vtables must
     remain the same, otherwise binary compatibility can be
     compromised.  */

  while (1)
    {
      char *buf1 = (char *) alloca (TYPE_ASSEMBLER_NAME_LENGTH (for_type)
				    + 1 + i);
      char *new_buf2;

      sprintf (buf1, "%s%c%s", TYPE_ASSEMBLER_NAME_STRING (for_type), joiner,
	       buf2);
      buf = (char *) alloca (strlen (VTABLE_NAME_PREFIX) + strlen (buf1) + 1);
      sprintf (buf, "%s%s", VTABLE_NAME_PREFIX, buf1);
      name = get_identifier (buf);

      /* If this name doesn't clash, then we can use it, otherwise
	 we add more to the name until it is unique.  */

      if (! IDENTIFIER_GLOBAL_VALUE (name))
	break;

      /* Set values for next loop through, if the name isn't unique.  */

      path = BINFO_INHERITANCE_CHAIN (path);

      /* We better not run out of stuff to make it unique.  */
      my_friendly_assert (path != NULL_TREE, 368);

      basetype = TYPE_MAIN_VARIANT (BINFO_TYPE (path));

      if (for_type == basetype)
	{
	  /* If we run out of basetypes in the path, we have already
	     found created a vtable with that name before, we now
	     resort to tacking on _%d to distinguish them.  */
	  int j = 2;
	  i = TYPE_ASSEMBLER_NAME_LENGTH (basetype) + 1 + i + 1 + 3;
	  buf1 = (char *) alloca (i);
	  do {
	    sprintf (buf1, "%s%c%s%c%d",
		     TYPE_ASSEMBLER_NAME_STRING (basetype), joiner,
		     buf2, joiner, j);
	    buf = (char *) alloca (strlen (VTABLE_NAME_PREFIX)
				   + strlen (buf1) + 1);
	    sprintf (buf, "%s%s", VTABLE_NAME_PREFIX, buf1);
	    name = get_identifier (buf);

	    /* If this name doesn't clash, then we can use it,
	       otherwise we add something different to the name until
	       it is unique.  */
	  } while (++j <= 999 && IDENTIFIER_GLOBAL_VALUE (name));

	  /* Hey, they really like MI don't they?  Increase the 3
             above to 6, and the 999 to 999999.  :-)  */
	  my_friendly_assert (j <= 999, 369);

	  break;
	}

      i = TYPE_ASSEMBLER_NAME_LENGTH (basetype) + 1 + i;
      new_buf2 = (char *) alloca (i);
      sprintf (new_buf2, "%s%c%s",
	       TYPE_ASSEMBLER_NAME_STRING (basetype), joiner, buf2);
      buf2 = new_buf2;
    }

  new_decl = build_vtable (for_type, name, TREE_TYPE (orig_decl));
  DECL_ALIGN (new_decl) = DECL_ALIGN (orig_decl);
  BINFO_VTABLE (binfo) = pushdecl_top_level (new_decl);

#ifdef GATHER_STATISTICS
  n_vtables += 1;
  n_vtable_elems += list_length (BINFO_VIRTUALS (binfo));
#endif

  return 1;
}

/* Create a new vtable for BINFO which is the hierarchy dominated by
   T.  */

static int
make_new_vtable (t, binfo)
     tree t;
     tree binfo;
{
  if (binfo == TYPE_BINFO (t))
    /* In this case, it is *type*'s vtable we are modifying.  We start
       with the approximation that it's vtable is that of the
       immediate base class.  */
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
   BV_FN.  DELTA is the required adjustment from the `this' pointer
   where the vtable entry appears to the `this' required when the
   function is actually called.  */

static void
modify_vtable_entry (t, binfo, fndecl, delta, virtuals)
     tree t;
     tree binfo;
     tree fndecl;
     tree delta;
     tree *virtuals;
{
  tree vcall_index;
  tree v;

  v = *virtuals;
  vcall_index = integer_zero_node;

  if (fndecl != BV_FN (v)
      || !tree_int_cst_equal (delta, BV_DELTA (v))
      || !tree_int_cst_equal (vcall_index, BV_VCALL_INDEX (v)))
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
      BV_VCALL_INDEX (v) = vcall_index;
      BV_FN (v) = fndecl;

      /* Now assign virtual dispatch information, if unset.  We can
	 dispatch this, through any overridden base function.  */
      if (TREE_CODE (DECL_VINDEX (fndecl)) != INTEGER_CST)
	{
	  DECL_VINDEX (fndecl) = DECL_VINDEX (base_fndecl);
	  DECL_VIRTUAL_CONTEXT (fndecl) = DECL_VIRTUAL_CONTEXT (base_fndecl);
	}
    }
}

/* Call this function whenever its known that a vtable for T is going
   to be needed.  It's safe to call it more than once.  *HAS_VIRTUAL_P
   is initialized to the number of slots that are reserved at the
   beginning of the vtable for RTTI information.  */

static void
start_vtable (t, has_virtual_p)
     tree t;
     int *has_virtual_p;
{
  if (*has_virtual_p == 0 && ! CLASSTYPE_COM_INTERFACE (t))
    {
      /* If we are using thunks, use two slots at the front, one
	 for the offset pointer, one for the tdesc pointer.
         For ARM-style vtables, use the same slot for both.  */
      if (flag_vtable_thunks)
	*has_virtual_p = 2;
      else
	*has_virtual_p = 1;
    }
}

/* Add a virtual function to all the appropriate vtables for the class
   T.  DECL_VINDEX(X) should be error_mark_node, if we want to
   allocate a new slot in our table.  If it is error_mark_node, we
   know that no other function from another vtable is overridden by X.
   HAS_VIRTUAL keeps track of how many virtuals there are in our main
   vtable for the type, and we build upon the NEW_VIRTUALS list
   and return it.  */

static void
add_virtual_function (new_virtuals_p, overridden_virtuals_p,
		      has_virtual, fndecl, t)
     tree *new_virtuals_p;
     tree *overridden_virtuals_p;
     int *has_virtual;
     tree fndecl;
     tree t; /* Structure type.  */
{
  tree new_virtual;

  /* If this function doesn't override anything from a base class, we
     can just assign it a new DECL_VINDEX now.  Otherwise, if it does
     override something, we keep it around and assign its DECL_VINDEX
     later, in modify_all_vtables.  */
  if (TREE_CODE (DECL_VINDEX (fndecl)) == INTEGER_CST)
    /* We've already dealt with this function.  */
    return;

  new_virtual = build_tree_list (integer_zero_node, fndecl);
  BV_VCALL_INDEX (new_virtual) = integer_zero_node;

  if (DECL_VINDEX (fndecl) == error_mark_node)
    {
      /* FNDECL is a new virtual function; it doesn't override any
	 virtual function in a base class.  */

      /* We remember that this was the base sub-object for rtti.  */
      CLASSTYPE_RTTI (t) = t;

      start_vtable (t, has_virtual);

      /* Now assign virtual dispatch information.  */
      DECL_VINDEX (fndecl) = build_shared_int_cst ((*has_virtual)++);
      DECL_VIRTUAL_CONTEXT (fndecl) = t;

      /* Save the state we've computed on the NEW_VIRTUALS list.  */
      TREE_CHAIN (new_virtual) = *new_virtuals_p;
      *new_virtuals_p = new_virtual;
    }
  else
    {
      /* FNDECL overrides a function from a base class.  */
      TREE_CHAIN (new_virtual) = *overridden_virtuals_p;
      *overridden_virtuals_p = new_virtual;
    }
}

extern struct obstack *current_obstack;

/* Add method METHOD to class TYPE.

   If non-NULL, FIELDS is the entry in the METHOD_VEC vector entry of
   the class type where the method should be added.  */

void
add_method (type, fields, method)
     tree type, *fields, method;
{
  int using = (DECL_CONTEXT (method) != type);
  
  if (fields && *fields)
    *fields = build_overload (method, *fields);
  else 
    {
      int len;
      int slot;
      tree method_vec;

      if (!CLASSTYPE_METHOD_VEC (type))
	/* Make a new method vector.  We start with 8 entries.  We must
	   allocate at least two (for constructors and destructors), and
	   we're going to end up with an assignment operator at some
	   point as well.  

	   We could use a TREE_LIST for now, and convert it to a
	   TREE_VEC in finish_struct, but we would probably waste more
	   memory making the links in the list than we would by
	   over-allocating the size of the vector here.  Furthermore,
	   we would complicate all the code that expects this to be a
	   vector.  */
	CLASSTYPE_METHOD_VEC (type) = make_tree_vec (8);

      method_vec = CLASSTYPE_METHOD_VEC (type);
      len = TREE_VEC_LENGTH (method_vec);

      if (DECL_NAME (method) == constructor_name (type))
	/* A new constructor or destructor.  Constructors go in 
	   slot 0; destructors go in slot 1.  */
	slot = DESTRUCTOR_NAME_P (DECL_ASSEMBLER_NAME (method)) ? 1 : 0;
      else
	{
	  /* See if we already have an entry with this name.  */
	  for (slot = 2; slot < len; ++slot)
	    if (!TREE_VEC_ELT (method_vec, slot)
		|| (DECL_NAME (OVL_CURRENT (TREE_VEC_ELT (method_vec, 
							  slot))) 
		    == DECL_NAME (method)))
	      break;
		
	  if (slot == len)
	    {
	      /* We need a bigger method vector.  */
	      tree new_vec = make_tree_vec (2 * len);
	      bcopy ((PTR) &TREE_VEC_ELT (method_vec, 0),
		     (PTR) &TREE_VEC_ELT (new_vec, 0),
		     len * sizeof (tree));
	      len = 2 * len;
	      method_vec = CLASSTYPE_METHOD_VEC (type) = new_vec;
	    }

	  if (DECL_CONV_FN_P (method) && !TREE_VEC_ELT (method_vec, slot))
	    {
	      /* Type conversion operators have to come before
		 ordinary methods; add_conversions depends on this to
		 speed up looking for conversion operators.  So, if
		 necessary, we slide some of the vector elements up.
		 In theory, this makes this algorithm O(N^2) but we
		 don't expect many conversion operators.  */
	      for (slot = 2; slot < len; ++slot)
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
  
	      if (!TREE_VEC_ELT (method_vec, slot))
		/* There is nothing in the Ith slot, so we can avoid
		   moving anything.  */
		; 
	      else
		{
		  /* We know the last slot in the vector is empty
		     because we know that at this point there's room
		     for a new function.  */
		  bcopy ((PTR) &TREE_VEC_ELT (method_vec, slot),
			 (PTR) &TREE_VEC_ELT (method_vec, slot + 1),
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
		 
	      if (TREE_CODE (fn) != TREE_CODE (method))
		continue;

	      if (TREE_CODE (method) != TEMPLATE_DECL)
		{
		  /* [over.load] Member function declarations with the
		     same name and the same parameter types cannot be
		     overloaded if any of them is a static member
		     function declaration.  */
		  if ((DECL_STATIC_FUNCTION_P (fn)
		       != DECL_STATIC_FUNCTION_P (method))
		      || using)
		    {
		      tree parms1 = TYPE_ARG_TYPES (TREE_TYPE (fn));
		      tree parms2 = TYPE_ARG_TYPES (TREE_TYPE (method));

		      if (! DECL_STATIC_FUNCTION_P (fn))
			parms1 = TREE_CHAIN (parms1);
		      if (! DECL_STATIC_FUNCTION_P (method))
			parms2 = TREE_CHAIN (parms2);

		      if (compparms (parms1, parms2))
			{
			  if (using)
			    /* Defer to the local function.  */
			    return;
			  else
			    cp_error ("`%#D' and `%#D' cannot be overloaded",
				      fn, method);
			}
		    }

		  /* Since this is an ordinary function in a
		     non-template class, it's mangled name can be used
		     as a unique identifier.  This technique is only
		     an optimization; we would get the same results if
		     we just used decls_match here.  */
		  if (DECL_ASSEMBLER_NAME (fn) 
		      != DECL_ASSEMBLER_NAME (method))
		    continue;
		}
	      else if (!decls_match (fn, method))
		continue;

	      /* There has already been a declaration of this method
		 or member template.  */
	      cp_error_at ("`%D' has already been declared in `%T'", 
			   method, type);

	      /* We don't call duplicate_decls here to merge the
		 declarations because that will confuse things if the
		 methods have inline definitions.  In particular, we
		 will crash while processing the definitions.  */
	      return;
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
	    /* A using declaration may is allowed to appear more than
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
  tree elem = purpose_member (t, DECL_ACCESS (fdecl));
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

  binfo = binfo_or_else (ctype, t);
  if (! binfo)
    return;
  
  if (name == constructor_name (ctype)
      || name == constructor_name_full (ctype))
    {
      cp_error_at ("using-declaration for constructor", using_decl);
      return;
    }

  fdecl = lookup_member (binfo, name, 0, 0);
  
  if (!fdecl)
    {
      cp_error_at ("no members matching `%D' in `%#T'", using_decl, ctype);
      return;
    }

  if (BASELINK_P (fdecl))
    /* Ignore base type this came from. */
    fdecl = TREE_VALUE (fdecl);

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
  else if (! DECL_LANG_SPECIFIC (fdecl))
    my_friendly_abort (20000221);

  if (! old_value)
    ;
  else if (is_overloaded_fn (old_value))
    {
      if (flist)
	/* It's OK to use functions from a base when there are functions with
	   the same name already present in the current class.  */;
      else
	{
	  cp_error ("`%D' invalid in `%#T'", using_decl, t);
	  cp_error_at ("  because of local method `%#D' with same name",
		       OVL_CURRENT (old_value));
	  return;
	}
    }
  else
    {
      cp_error ("`%D' invalid in `%#T'", using_decl, t);
      cp_error_at ("  because of local field `%#D' with same name", old_value);
      return;
    }
  
  /* Make type T see field decl FDECL with access ACCESS.*/
  if (flist)
    for (; flist; flist = OVL_NEXT (flist))
      {
	add_method (t, 0, OVL_CURRENT (flist));
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
  int seen_nearly_empty_base_p;
  tree binfos;

  binfos = TYPE_BINFO_BASETYPES (t);
  n_baseclasses = CLASSTYPE_N_BASECLASSES (t);
  seen_nearly_empty_base_p = 0;

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
	cp_warning ("base class `%#T' has a non-virtual destructor",
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
            cp_pedwarn ("base `%T' with only non-default constructor in class without a constructor",
                        basetype);
	}

      /* If the base class is not empty or nearly empty, then this
	 class cannot be nearly empty.  */
      if (!CLASSTYPE_NEARLY_EMPTY_P (basetype) && !is_empty_class (basetype))
	CLASSTYPE_NEARLY_EMPTY_P (t) = 0;
      /* And if there is more than one nearly empty base, then the
	 derived class is not nearly empty either.  */
      else if (CLASSTYPE_NEARLY_EMPTY_P (basetype) 
	       && seen_nearly_empty_base_p)
	CLASSTYPE_NEARLY_EMPTY_P (t) = 0;
      /* If this is the first nearly empty base class, then remember
	 that we saw it.  */
      else if (CLASSTYPE_NEARLY_EMPTY_P (basetype))
	seen_nearly_empty_base_p = 1;

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

      /* Derived classes can implicitly become COMified if their bases
	 are COM.  */
      if (CLASSTYPE_COM_INTERFACE (basetype))
	CLASSTYPE_COM_INTERFACE (t) = 1;
      else if (i == 0 && CLASSTYPE_COM_INTERFACE (t))
	{
	  cp_error 
	    ("COM interface type `%T' with non-COM leftmost base class `%T'",
	     t, basetype);
	  CLASSTYPE_COM_INTERFACE (t) = 0;
	}
    }
}

/* Make the Ith baseclass of T its primary base.  */

static void
set_primary_base (t, i, has_virtual_p)
     tree t;
     int i;
     int *has_virtual_p;
{
  tree basetype;

  CLASSTYPE_VFIELD_PARENT (t) = i;
  basetype = BINFO_TYPE (CLASSTYPE_PRIMARY_BINFO (t));
  TYPE_BINFO_VTABLE (t) = TYPE_BINFO_VTABLE (basetype);
  TYPE_BINFO_VIRTUALS (t) = TYPE_BINFO_VIRTUALS (basetype);
  TYPE_VFIELD (t) = TYPE_VFIELD (basetype);
  CLASSTYPE_RTTI (t) = CLASSTYPE_RTTI (basetype);
  *has_virtual_p = CLASSTYPE_VSIZE (basetype);
}

/* Determine the primary class for T.  */

static void
determine_primary_base (t, has_virtual_p)
     tree t;
     int *has_virtual_p;
{
  int i, n_baseclasses = CLASSTYPE_N_BASECLASSES (t);

  /* If there are no baseclasses, there is certainly no primary base.  */
  if (n_baseclasses == 0)
    return;

  *has_virtual_p = 0;

  for (i = 0; i < n_baseclasses; i++)
    {
      tree base_binfo = TREE_VEC_ELT (TYPE_BINFO_BASETYPES (t), i);
      tree basetype = BINFO_TYPE (base_binfo);

      if (TYPE_CONTAINS_VPTR_P (basetype))
	{
	  /* Even a virtual baseclass can contain our RTTI
	     information.  But, we prefer a non-virtual polymorphic
	     baseclass.  */
	  if (!CLASSTYPE_HAS_PRIMARY_BASE_P (t))
	    CLASSTYPE_RTTI (t) = CLASSTYPE_RTTI (basetype);

	  /* A virtual baseclass can't be the primary base under the
	     old ABI.  And under the new ABI we still prefer a
	     non-virtual base.  */
	  if (TREE_VIA_VIRTUAL (base_binfo))
	    continue;

	  if (!CLASSTYPE_HAS_PRIMARY_BASE_P (t))
	    {
	      set_primary_base (t, i, has_virtual_p);
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

	      if (*has_virtual_p == 0)
		set_primary_base (t, i, has_virtual_p);
	    }
	}
    }

  if (!TYPE_VFIELD (t))
    CLASSTYPE_VFIELD_PARENT (t) = -1;

  /* The new ABI allows for the use of a "nearly-empty" virtual base
     class as the primary base class if no non-virtual polymorphic
     base can be found.  */
  if (flag_new_abi && !CLASSTYPE_HAS_PRIMARY_BASE_P (t))
    for (i = 0; i < n_baseclasses; ++i)
      {
	tree base_binfo = TREE_VEC_ELT (TYPE_BINFO_BASETYPES (t), i);
	tree basetype = BINFO_TYPE (base_binfo);

	if (TREE_VIA_VIRTUAL (base_binfo) 
	    && CLASSTYPE_NEARLY_EMPTY_P (basetype))
	  {
	    set_primary_base (t, i, has_virtual_p);
	    CLASSTYPE_VFIELDS (t) = copy_list (CLASSTYPE_VFIELDS (basetype));
	    break;
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

  /* If this type has a copy constructor, force its mode to be BLKmode, and
     force its TREE_ADDRESSABLE bit to be nonzero.  This will cause it to
     be passed by invisible reference and prevent it from being returned in
     a register.

     Also do this if the class has BLKmode but can still be returned in
     registers, since function_cannot_inline_p won't let us inline
     functions returning such a type.  This affects the HP-PA.  */
  if (! TYPE_HAS_TRIVIAL_INIT_REF (t)
      || (TYPE_MODE (t) == BLKmode && ! aggregate_value_p (t)
	  && CLASSTYPE_NON_AGGREGATE (t)))
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
	    break;
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
	  cp_warning ("all member functions in class `%T' are private", t);
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
	  cp_warning ("`%#T' only defines a private destructor and has no friends",
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
	  cp_warning ("`%#T' only defines private constructors and has no friends",
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

   If there are any constructors/destructors, they are moved to the
   front of the list.  This makes pushclass more efficient.

   @@ The above comment is obsolete.  It mostly describes what add_method
   @@ and add_implicitly_declared_members do.

   Sort methods that are not special (i.e., constructors, destructors, and
   type conversion operators) so that we can find them faster in search.  */

static void
finish_struct_methods (t)
     tree t;
{
  tree fn_fields;
  tree method_vec;
  tree ctor_name = constructor_name (t);
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
    {
      tree fn_name = DECL_NAME (fn_fields);

      /* Clear out this flag.

	 @@ Doug may figure out how to break
	 @@ this with nested classes and friends.  */
      DECL_IN_AGGR_P (fn_fields) = 0;

      /* Note here that a copy ctor is private, so we don't dare generate
 	 a default copy constructor for a class that has a member
 	 of this type without making sure they have access to it.  */
      if (fn_name == ctor_name)
 	{
 	  tree parmtypes = FUNCTION_ARG_CHAIN (fn_fields);
 	  tree parmtype = parmtypes ? TREE_VALUE (parmtypes) : void_type_node;
	  
 	  if (TREE_CODE (parmtype) == REFERENCE_TYPE
 	      && TYPE_MAIN_VARIANT (TREE_TYPE (parmtype)) == t)
 	    {
 	      if (TREE_CHAIN (parmtypes) == NULL_TREE
 		  || TREE_CHAIN (parmtypes) == void_list_node
 		  || TREE_PURPOSE (TREE_CHAIN (parmtypes)))
 		{
 		  if (TREE_PROTECTED (fn_fields))
 		    TYPE_HAS_NONPUBLIC_CTOR (t) = 1;
 		  else if (TREE_PRIVATE (fn_fields))
 		    TYPE_HAS_NONPUBLIC_CTOR (t) = 2;
 		}
 	    }
	}
      else if (fn_name == ansi_opname[(int) MODIFY_EXPR])
	{
	  tree parmtype = TREE_VALUE (FUNCTION_ARG_CHAIN (fn_fields));

	  if (copy_assignment_arg_p (parmtype, DECL_VIRTUAL_P (fn_fields)))
	    {
	      if (TREE_PROTECTED (fn_fields))
		TYPE_HAS_NONPUBLIC_ASSIGN_REF (t) = 1;
	      else if (TREE_PRIVATE (fn_fields))
		TYPE_HAS_NONPUBLIC_ASSIGN_REF (t) = 2;
	    }
	}
    }

  if (TYPE_HAS_DESTRUCTOR (t) && !TREE_VEC_ELT (method_vec, 1))
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
  cp_error ("redefinition of `%#T'", t);
  cp_error_at ("previous definition here", t);

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

      bzero ((char *) TYPE_LANG_SPECIFIC (t), sizeof (struct lang_type));
      BINFO_BASETYPES(binfo) = NULL_TREE;

      TYPE_BINFO (t) = binfo;
      CLASSTYPE_INTERFACE_ONLY (t) = interface_only;
      SET_CLASSTYPE_INTERFACE_UNKNOWN_X (t, interface_unknown);
      TYPE_REDEFINED (t) = 1;
      CLASSTYPE_TEMPLATE_INFO (t) = template_info;
      CLASSTYPE_USE_TEMPLATE (t) = use_template;
    }
  TYPE_SIZE (t) = NULL_TREE;
  TYPE_MODE (t) = VOIDmode;
  TYPE_FIELDS (t) = NULL_TREE;
  TYPE_METHODS (t) = NULL_TREE;
  TYPE_VFIELD (t) = NULL_TREE;
  TYPE_CONTEXT (t) = NULL_TREE;
  TYPE_NONCOPIED_PARTS (t) = NULL_TREE;
}

/* Make the BINFO's vtablehave N entries, including RTTI entries,
   vbase and vcall offsets, etc.  Set its type and call the backend
   to lay it out.  */

static void
layout_vtable_decl (binfo, n)
     tree binfo;
     int n;
{
  tree itype;
  tree atype;

  itype = size_int (n);
  atype = build_cplus_array_type (vtable_entry_type, 
				  build_index_type (itype));
  layout_type (atype);

  /* We may have to grow the vtable.  */
  if (!same_type_p (TREE_TYPE (BINFO_VTABLE (binfo)), atype))
    {
      tree vtable = BINFO_VTABLE (binfo);

      TREE_TYPE (vtable) = atype;
      DECL_SIZE (vtable) = DECL_SIZE_UNIT (vtable) = 0;
      layout_decl (vtable, 0);

      /* At one time the vtable info was grabbed 2 words at a time.  This
	 fails on Sparc unless you have 8-byte alignment.  */
      DECL_ALIGN (vtable) = MAX (TYPE_ALIGN (double_type_node),
				 DECL_ALIGN (vtable));
    }
}

/* Returns the number of virtual function table entries (excluding
   RTTI information, vbase and vcall offests, etc.) in the vtable for
   BINFO.  */

static int
num_vfun_entries (binfo)
     tree binfo;
{
  return list_length (skip_rtti_stuff (binfo,
				       BINFO_TYPE (binfo),
				       NULL));
}

/* Called from num_extra_vtbl_entries via dfs_walk.  */

static tree
dfs_count_virtuals (binfo, data)
     tree binfo;
     void *data;
{
  /* Non-primary bases are not interesting; all of the virtual
     function table entries have been overridden.  */
  if (!BINFO_PRIMARY_MARKED_P (binfo))
    ((vcall_offset_data *) data)->offsets += num_vfun_entries (binfo);
  
  return NULL_TREE;
}

/* Returns the number of extra entries (at negative indices) required
   for BINFO's vtable.  */

tree
num_extra_vtbl_entries (binfo)
     tree binfo;
{
  tree type;
  int entries;

  type = BINFO_TYPE (binfo);
  entries = 0;

  /* There is an entry for the offset to each virtual base.  */
  if (vbase_offsets_in_vtable_p ())
    entries += list_length (CLASSTYPE_VBASECLASSES (type));

  /* If this is a virtual base, there are entries for each virtual
     function defined in this class or its bases.  */
  if (vcall_offsets_in_vtable_p () && TREE_VIA_VIRTUAL (binfo))
    {
      vcall_offset_data vod;

      vod.vbase = binfo;
      vod.offsets = 0;
      dfs_walk (binfo,
		dfs_count_virtuals,
		dfs_vcall_offset_queue_p,
		&vod);
      entries += vod.offsets;
    }
      
  return entries ? size_int (entries) : size_zero_node;
}

/* Returns the offset (in bytes) from the beginning of BINFO's vtable
   where the vptr should actually point.  */

tree
size_extra_vtbl_entries (binfo)
     tree binfo;
{
  tree offset = size_binop (MULT_EXPR, TYPE_SIZE_UNIT (vtable_entry_type),
			    num_extra_vtbl_entries (binfo));
  return fold (offset);
}

/* Construct the initializer for BINFOs virtual function table.  BINFO
   is part of the hierarchy dominated by T.  The value returned is a
   TREE_LIST suitable for wrapping in a CONSTRUCTOR to use as the
   DECL_INITIAL for a vtable.  */

static tree
build_vtbl_initializer (binfo, t)
     tree binfo;
     tree t;
{
  tree v = BINFO_VIRTUALS (binfo);
  tree inits = NULL_TREE;
  tree type = BINFO_TYPE (binfo);

  /* Add entries to the vtable that indicate how to adjust the this
     pointer when calling a virtual function in this class.  */
  inits = build_vcall_offset_vtbl_entries (binfo, t);

  /* Add entries to the vtable for offsets to our virtual bases.  */
  inits = chainon (build_vbase_offset_vtbl_entries (binfo, t),
		   inits);

  /* Process the RTTI stuff at the head of the list.  If we're not
     using vtable thunks, then the RTTI entry is just an ordinary
     function, and we can process it just like the other virtual
     function entries.  */
  if (!CLASSTYPE_COM_INTERFACE (type) && flag_vtable_thunks)
    {
      tree offset;
      tree init;

      /* The first entry is an offset.  */
      offset = TREE_PURPOSE (v);
      my_friendly_assert (TREE_CODE (offset) == INTEGER_CST,
			  19990727);

      /* Convert the offset to look like a function pointer, so that
	 we can put it in the vtable.  */
      init = build1 (NOP_EXPR, vfunc_ptr_type_node, offset);
      TREE_CONSTANT (init) = 1;
      inits = tree_cons (NULL_TREE, init, inits);

      v = TREE_CHAIN (v);
      
      if (new_abi_rtti_p ())
        {
          tree decl = TREE_VALUE (v);
          
          if (decl)
            decl = build_unary_op (ADDR_EXPR, decl, 0);
          else
            decl = integer_zero_node;
          decl = build1 (NOP_EXPR, vfunc_ptr_type_node, decl);
          TREE_CONSTANT (decl) = 1;
          decl = build_vtable_entry (integer_zero_node, integer_zero_node,
                                     decl);
          inits = tree_cons (NULL_TREE, decl, inits);
          
          v = TREE_CHAIN (v);
        }
      /* In the old abi the second entry (the tdesc pointer) is
	 just an ordinary function, so it can be dealt with like the
	 virtual functions.  */
    }

  /* Go through all the ordinary virtual functions, building up
     initializers.  */
  while (v)
    {
      tree delta;
      tree vcall_index;
      tree fn;
      tree pfn;
      tree init;

      /* Pull the offset for `this', and the function to call, out of
	 the list.  */
      delta = BV_DELTA (v);
      vcall_index = BV_VCALL_INDEX (v);
      fn = BV_FN (v);
      my_friendly_assert (TREE_CODE (delta) == INTEGER_CST, 19990727);
      my_friendly_assert (TREE_CODE (fn) == FUNCTION_DECL, 19990727);

      /* You can't call an abstract virtual function; it's abstract.
	 So, we replace these functions with __pure_virtual.  */
      if (DECL_PURE_VIRTUAL_P (fn))
	fn = abort_fndecl;

      /* Take the address of the function, considering it to be of an
	 appropriate generic type.  */
      pfn = build1 (ADDR_EXPR, vfunc_ptr_type_node, fn);
      /* The address of a function can't change.  */
      TREE_CONSTANT (pfn) = 1;
      /* Enter it in the vtable.  */
      init = build_vtable_entry (delta, vcall_index, pfn);
      /* And add it to the chain of initializers.  */
      inits = tree_cons (NULL_TREE, init, inits);

      /* Keep going.  */
      v = TREE_CHAIN (v);
    }

  /* The initializers were built up in reverse order; straighten them
     out now.  */
  return nreverse (inits);
}

/* Initialize the vtable for BINFO with the INITS.  */

static void
initialize_vtable (binfo, inits)
     tree binfo;
     tree inits;
{
  tree context;
  tree decl;

  layout_vtable_decl (binfo, list_length (inits));
  decl = BINFO_VTABLE (binfo);
  context = DECL_CONTEXT (decl);
  DECL_CONTEXT (decl) = 0;
  DECL_INITIAL (decl) = build_nt (CONSTRUCTOR, NULL_TREE, inits);
  cp_finish_decl (decl, DECL_INITIAL (decl), NULL_TREE, 0);
  DECL_CONTEXT (decl) = context;
}

/* Called from finish_vtbls via dfs_walk.  */

static tree
dfs_finish_vtbls (binfo, data)
     tree binfo;
     void *data;
{
  if (!BINFO_PRIMARY_MARKED_P (binfo)
      && CLASSTYPE_VFIELDS (BINFO_TYPE (binfo))
      && BINFO_NEW_VTABLE_MARKED (binfo))
    initialize_vtable (binfo, 
		       build_vtbl_initializer (binfo, (tree) data));

  CLEAR_BINFO_NEW_VTABLE_MARKED (binfo);
  SET_BINFO_MARKED (binfo);

  return NULL_TREE;
}

/* Called from finish_vtbls via dfs_walk when using the new ABI.
   Accumulates the vtable initializers for all of the vtables into
   TREE_VALUE (DATA).  */

static tree
dfs_accumulate_vtbl_inits (binfo, data)
     tree binfo;
     void *data;
{
  if (!BINFO_PRIMARY_MARKED_P (binfo)
      && CLASSTYPE_VFIELDS (BINFO_TYPE (binfo))
      && BINFO_NEW_VTABLE_MARKED (binfo))
    {
      tree l;
      tree t;

      l = (tree) data;
      t = TREE_PURPOSE (l);

      /* If this is a secondary vtable, record its location.  */
      if (binfo != TYPE_BINFO (t))
	{
	  tree vtbl;

	  vtbl = TYPE_BINFO_VTABLE (t);
	  vtbl = build1 (ADDR_EXPR, 
			 build_pointer_type (TREE_TYPE (vtbl)),
			 vtbl);
	  BINFO_VTABLE (binfo)
	    = build (PLUS_EXPR, TREE_TYPE (vtbl), vtbl,
		     size_binop (MULT_EXPR,
				 TYPE_SIZE_UNIT (TREE_TYPE (vtbl)),
				 size_int (list_length (TREE_VALUE (l)))));
	}

      /* Add the initializers for this vtable to the initailizers for
	 the other vtables we've already got.  */
      TREE_VALUE (l) 
	= chainon (TREE_VALUE (l),
		   build_vtbl_initializer (binfo, t));
    }

  CLEAR_BINFO_NEW_VTABLE_MARKED (binfo);
  SET_BINFO_MARKED (binfo);

  return NULL_TREE;
}

/* Create all the necessary vtables for T and its base classes.  */

static void
finish_vtbls (t)
     tree t;
{
  if (merge_primary_and_secondary_vtables_p ())
    {
      tree list;

      /* Under the new ABI, we lay out the primary and secondary
	 vtables in one contiguous vtable.  The primary vtable is
	 first, followed by the secondary vtables as encountered in a
	 pre-order depth-first left-to-right traversal.  */
      list = build_tree_list (t, NULL_TREE);
      dfs_walk_real (TYPE_BINFO (t), 
		     dfs_accumulate_vtbl_inits,
		     NULL, 
		     dfs_unmarked_real_bases_queue_p, 
		     list);
      if (TYPE_BINFO_VTABLE (t))
	initialize_vtable (TYPE_BINFO (t), TREE_VALUE (list));
    }
  else
    dfs_walk (TYPE_BINFO (t), dfs_finish_vtbls, 
	      dfs_unmarked_real_bases_queue_p, t);

  dfs_walk (TYPE_BINFO (t), dfs_unmark, 
	    dfs_marked_real_bases_queue_p, t);
}

/* True if we should override the given BASE_FNDECL with the given
   FNDECL.  */

static int
overrides (fndecl, base_fndecl)
     tree fndecl, base_fndecl;
{
  /* Destructors have special names.  */
  if (DESTRUCTOR_NAME_P (DECL_ASSEMBLER_NAME (base_fndecl))
      && DESTRUCTOR_NAME_P (DECL_ASSEMBLER_NAME (fndecl)))
    return 1;
  if (DESTRUCTOR_NAME_P (DECL_ASSEMBLER_NAME (base_fndecl))
      || DESTRUCTOR_NAME_P (DECL_ASSEMBLER_NAME (fndecl)))
    return 0;
  if (DECL_NAME (fndecl) == DECL_NAME (base_fndecl))
    {
      tree types, base_types;
#if 0
      retypes = TREE_TYPE (TREE_TYPE (fndecl));
      base_retypes = TREE_TYPE (TREE_TYPE (base_fndecl));
#endif
      types = TYPE_ARG_TYPES (TREE_TYPE (fndecl));
      base_types = TYPE_ARG_TYPES (TREE_TYPE (base_fndecl));
      if ((TYPE_QUALS (TREE_TYPE (TREE_VALUE (base_types)))
	   == TYPE_QUALS (TREE_TYPE (TREE_VALUE (types))))
	  && compparms (TREE_CHAIN (base_types), TREE_CHAIN (types)))
	return 1;
    }
  return 0;
}

typedef struct find_final_overrider_data_s {
  /* The function for which we are trying to find a final overrider.  */
  tree fn;
  /* The base class in which the function was declared.  */
  tree declaring_base;
  /* The most derived class in the hierarchy.  */
  tree most_derived_type;
  /* The final overriding function.  */
  tree overriding_fn;
  /* The BINFO for the class in which the final overriding function
     appears.  */
  tree overriding_base;
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

      /* We've found a path to the declaring base.  Walk down the path
	 looking for an overrider for FN.  */
      for (path = reverse_path (binfo); 
	   path; 
	   path = TREE_CHAIN (path))
	{
	  for (method = TYPE_METHODS (BINFO_TYPE (TREE_VALUE (path)));
	       method;
	       method = TREE_CHAIN (method))
	    if (DECL_VIRTUAL_P (method) && overrides (method, ffod->fn))
	      break;

	  if (method)
	    break;
	}

      /* If we found an overrider, record the overriding function, and
	 the base from which it came.  */
      if (path)
	{
	  if (ffod->overriding_fn && ffod->overriding_fn != method)
	    {
	      /* We've found a different overrider along a different
		 path.  That can be OK if the new one overrides the
		 old one.  Consider:
	      
	           struct S { virtual void f(); };
	           struct T : public virtual S { virtual void f(); };
	           struct U : public virtual S, public virtual T {};
	      
	         Here `T::f' is the final overrider for `S::f'.  */
	      if (strictly_overrides (method, ffod->overriding_fn))
		{
		  ffod->overriding_fn = method;
		  ffod->overriding_base = TREE_VALUE (path);
		}
	      else if (!strictly_overrides (ffod->overriding_fn, method))
		{
		  cp_error ("no unique final overrider for `%D' in `%T'", 
			    ffod->most_derived_type,
			    ffod->fn);
		  cp_error ("candidates are: `%#D'", ffod->overriding_fn);
		  cp_error ("                `%#D'", method);
		  return error_mark_node;
		}
	    }
	  else if (ffod->overriding_base
		   && (!tree_int_cst_equal 
		       (BINFO_OFFSET (TREE_VALUE (path)),
			BINFO_OFFSET (ffod->overriding_base))))
	    {
	      /* We've found two instances of the same base that
		 provide overriders.  */
	      cp_error ("no unique final overrider for `%D' since there two instances of `%T' in `%T'", 
			ffod->fn,
			BINFO_TYPE (ffod->overriding_base),
			ffod->most_derived_type);
	      return error_mark_node;
	    }
	  else
	    {
	      ffod->overriding_fn = method;
	      ffod->overriding_base = TREE_VALUE (path);
	    }
	}
    }

  return NULL_TREE;
}

/* Returns a TREE_LIST whose TREE_PURPOSE is the final overrider for
   FN and whose TREE_VALUE is the binfo for the base where the
   overriding occurs.  BINFO (in the hierarchy dominated by T) is the
   base object in which FN is declared.  */

static tree
find_final_overrider (t, binfo, fn)
     tree t;
     tree binfo;
     tree fn;
{
  find_final_overrider_data ffod;

  /* Getting this right is a little tricky.  This is legal:

       struct S { virtual void f (); };
       struct T { virtual void f (); };
       struct U : public S, public T { };

     even though calling `f' in `U' is ambiguous.  But, 

       struct R { virtual void f(); };
       struct S : virtual public R { virtual void f (); };
       struct T : virtual public R { virtual void f (); };
       struct U : public S, public T { };

     is not -- there's no way  to decide whether to put `S::f' or
     `T::f' in the vtable for `R'.  
     
     The solution is to look at all paths to BINFO.  If we find
     different overriders along any two, then there is a problem.  */
  ffod.fn = fn;
  ffod.declaring_base = binfo;
  ffod.most_derived_type = t;
  ffod.overriding_fn = NULL_TREE;
  ffod.overriding_base = NULL_TREE;

  if (dfs_walk (TYPE_BINFO (t),
		dfs_find_final_overrider,
		NULL,
		&ffod))
    return error_mark_node;

  return build_tree_list (ffod.overriding_fn, ffod.overriding_base);
}

/* Return the BINFO_VIRTUALS list for BINFO, without the RTTI stuff at
   the front.  If non-NULL, N is set to the number of entries
   skipped.  */

tree
skip_rtti_stuff (binfo, t, n)
     tree binfo;
     tree t;
     HOST_WIDE_INT *n;
{
  tree virtuals;

  if (CLASSTYPE_COM_INTERFACE (t))
    return 0;

  if (n)
    *n = 0;
  virtuals = BINFO_VIRTUALS (binfo);
  if (virtuals)
    {
      /* We always reserve a slot for the offset/tdesc entry.  */
      if (n)
	++*n;
      virtuals = TREE_CHAIN (virtuals);
    }
  if (flag_vtable_thunks && virtuals)
    {
      /* The second slot is reserved for the tdesc pointer when thunks
         are used.  */
      if (n)
	++*n;
      virtuals = TREE_CHAIN (virtuals);
    }

  return virtuals;
}

/* Called via dfs_walk.  Returns BINFO if BINFO has the same type as
   DATA (which is really an _TYPE node).  */

static tree
dfs_find_base (binfo, data)
     tree binfo;
     void *data;
{
  return (same_type_p (BINFO_TYPE (binfo), (tree) data)
	  ? binfo : NULL_TREE);
}

/* Called from modify_all_vtables via dfs_walk.  */

static tree
dfs_modify_vtables (binfo, data)
     tree binfo;
     void *data;
{
  if (/* There's no need to modify the vtable for a primary base;
	 we're not going to use that vtable anyhow.  */
      !BINFO_PRIMARY_MARKED_P (binfo)
      /* Similarly, a base without a vtable needs no modification.  */
      && CLASSTYPE_VFIELDS (BINFO_TYPE (binfo)))
    {
      tree t;
      tree virtuals;
      tree old_virtuals;

      t = (tree) data;

      /* If we're supporting RTTI then we always need a new vtable to
	 point to the RTTI information.  Under the new ABI we may need
	 a new vtable to contain vcall and vbase offsets.  */
      if (flag_rtti || flag_new_abi)
	make_new_vtable (t, binfo);
      
      /* Now, go through each of the virtual functions in the virtual
	 function table for BINFO.  Find the final overrider, and
	 update the BINFO_VIRTUALS list appropriately.  */
      for (virtuals = skip_rtti_stuff (binfo, BINFO_TYPE (binfo), NULL),
	     old_virtuals = skip_rtti_stuff (TYPE_BINFO (BINFO_TYPE (binfo)),
					     BINFO_TYPE (binfo),
					     NULL);
	   virtuals;
	   virtuals = TREE_CHAIN (virtuals),
	     old_virtuals = TREE_CHAIN (old_virtuals))
	{
	  tree b;
	  tree fn;
	  tree overrider;
	  tree vindex;
	  tree delta;
	  HOST_WIDE_INT vindex_val, i;


	  /* Find the function which originally caused this vtable
	     entry to be present.  */
	  fn = BV_FN (old_virtuals);
	  vindex = DECL_VINDEX (fn);
	  b = dfs_walk (binfo, dfs_find_base, NULL, DECL_VIRTUAL_CONTEXT (fn));
	  fn = skip_rtti_stuff (TYPE_BINFO (BINFO_TYPE (b)),
				BINFO_TYPE (b),
				&i);
	  vindex_val = tree_low_cst (vindex, 0);
	  while (i < vindex_val)
	    {
	      fn = TREE_CHAIN (fn);
	      ++i;
	    }
	  fn = BV_FN (fn);

	  /* Handle the case of a virtual function defined in BINFO
	     itself.  */
	  overrider = find_final_overrider (t, b, fn);
	  if (overrider == error_mark_node)
	    continue;

	  /* The `this' pointer needs to be adjusted from pointing to
	     BINFO to pointing at the base where the final overrider
	     appears.  */
	  delta = size_binop (PLUS_EXPR,
			      get_derived_offset (binfo,
						  DECL_VIRTUAL_CONTEXT (fn)),
			      BINFO_OFFSET (binfo));
	  delta = size_diffop (BINFO_OFFSET (TREE_VALUE (overrider)), delta);

	  modify_vtable_entry (t, 
			       binfo, 
			       TREE_PURPOSE (overrider),
			       delta,
			       &virtuals);
	}
    }

  SET_BINFO_MARKED (binfo);

  return NULL_TREE;
}

/* Update all of the primary and secondary vtables for T.  Create new
   vtables as required, and initialize their RTTI information.  Each
   of the functions in OVERRIDDEN_VIRTUALS overrides a virtual
   function from a base class; find and modify the appropriate entries
   to point to the overriding functions.  Returns a list, in
   declaration order, of the functions that are overridden in this
   class, but do not appear in the primary base class vtable, and
   which should therefore be appended to the end of the vtable for T.  */

static tree
modify_all_vtables (t, has_virtual_p, overridden_virtuals)
     tree t;
     int *has_virtual_p;
     tree overridden_virtuals;
{
  tree binfo;

  binfo = TYPE_BINFO (t);

  /* Update all of the vtables.  */
  dfs_walk (binfo, 
	    dfs_modify_vtables, 
	    dfs_unmarked_real_bases_queue_p,
	    t);
  dfs_walk (binfo, dfs_unmark, dfs_marked_real_bases_queue_p, t);

  /* If we should include overriding functions for secondary vtables
     in our primary vtable, add them now.  */
  if (all_overridden_vfuns_in_vtables_p ())
    {
      tree *fnsp = &overridden_virtuals;

      while (*fnsp)
	{
	  tree fn = TREE_VALUE (*fnsp);

	  if (BINFO_VIRTUALS (binfo)
	      && !value_member (fn, BINFO_VIRTUALS (binfo)))
	    {
	      /* We know we need a vtable for this class now.  */
	      start_vtable (t, has_virtual_p);
	      /* Set the vtable index.  */
	      DECL_VINDEX (fn) 
		= build_shared_int_cst ((*has_virtual_p)++);
	      /* We don't need to convert to a base class when calling
		 this function.  */
	      DECL_VIRTUAL_CONTEXT (fn) = t;

	      /* We don't need to adjust the `this' pointer when
		 calling this function.  */
	      BV_DELTA (*fnsp) = integer_zero_node;
	      BV_VCALL_INDEX (*fnsp) = integer_zero_node;

	      /* This is an overridden function not already in our
		 vtable.  Keep it.  */
	      fnsp = &TREE_CHAIN (*fnsp);
	    }
	  else
	    /* We've already got an entry for this function.  Skip
	       it.  */
	    *fnsp = TREE_CHAIN (*fnsp);
	}
    }
  else
    overridden_virtuals = NULL_TREE;

  return overridden_virtuals;
}

/* Here, we already know that they match in every respect.
   All we have to check is where they had their declarations.  */

static int 
strictly_overrides (fndecl1, fndecl2)
     tree fndecl1, fndecl2;
{
  int distance = get_base_distance (DECL_CONTEXT (fndecl2),
				    DECL_CONTEXT (fndecl1),
				    0, (tree *)0);
  if (distance == -2 || distance > 0)
    return 1;
  return 0;
}

/* Get the base virtual function declarations in T that are either
   overridden or hidden by FNDECL as a list.  We set TREE_PURPOSE with
   the overrider/hider.  */

static tree
get_basefndecls (fndecl, t)
     tree fndecl, t;
{
  tree methods = TYPE_METHODS (t);
  tree base_fndecls = NULL_TREE;
  tree binfos = BINFO_BASETYPES (TYPE_BINFO (t));
  int i, n_baseclasses = binfos ? TREE_VEC_LENGTH (binfos) : 0;

  while (methods)
    {
      if (TREE_CODE (methods) == FUNCTION_DECL
	  && DECL_VINDEX (methods) != NULL_TREE
	  && DECL_NAME (fndecl) == DECL_NAME (methods))
	base_fndecls = tree_cons (fndecl, methods, base_fndecls);

      methods = TREE_CHAIN (methods);
    }

  if (base_fndecls)
    return base_fndecls;

  for (i = 0; i < n_baseclasses; i++)
    {
      tree base_binfo = TREE_VEC_ELT (binfos, i);
      tree basetype = BINFO_TYPE (base_binfo);

      base_fndecls = chainon (get_basefndecls (fndecl, basetype),
			      base_fndecls);
    }

  return base_fndecls;
}

/* Mark the functions that have been hidden with their overriders.
   Since we start out with all functions already marked with a hider,
   no need to mark functions that are just hidden.

   Subroutine of warn_hidden.  */

static void
mark_overriders (fndecl, base_fndecls)
     tree fndecl, base_fndecls;
{
  for (; base_fndecls; base_fndecls = TREE_CHAIN (base_fndecls))
    {
      if (overrides (fndecl, TREE_VALUE (base_fndecls)))
	TREE_PURPOSE (base_fndecls) = fndecl;
    }
}

/* If this declaration supersedes the declaration of
   a method declared virtual in the base class, then
   mark this field as being virtual as well.  */

static void
check_for_override (decl, ctype)
     tree decl, ctype;
{
  tree binfos = BINFO_BASETYPES (TYPE_BINFO (ctype));
  int i, n_baselinks = binfos ? TREE_VEC_LENGTH (binfos) : 0;
  int virtualp = DECL_VIRTUAL_P (decl);
  int found_overriden_fn = 0;

  for (i = 0; i < n_baselinks; i++)
    {
      tree base_binfo = TREE_VEC_ELT (binfos, i);
      if (TYPE_POLYMORPHIC_P (BINFO_TYPE (base_binfo)))
	{
	  tree tmp = get_matching_virtual
	    (base_binfo, decl,
	     DESTRUCTOR_NAME_P (DECL_ASSEMBLER_NAME (decl)));

	  if (tmp && !found_overriden_fn)
	    {
	      /* If this function overrides some virtual in some base
		 class, then the function itself is also necessarily
		 virtual, even if the user didn't explicitly say so.  */
	      DECL_VIRTUAL_P (decl) = 1;

	      /* The TMP we really want is the one from the deepest
		 baseclass on this path, taking care not to
		 duplicate if we have already found it (via another
		 path to its virtual baseclass.  */
	      if (TREE_CODE (TREE_TYPE (decl)) == FUNCTION_TYPE)
		{
		  cp_error_at ("`static %#D' cannot be declared", decl);
		  cp_error_at ("  since `virtual %#D' declared in base class",
			       tmp);
		  break;
		}
	      virtualp = 1;

	      /* Set DECL_VINDEX to a value that is neither an
		 INTEGER_CST nor the error_mark_node so that
		 add_virtual_function will realize this is an
		 overridden function.  */
	      DECL_VINDEX (decl) 
		= tree_cons (tmp, NULL_TREE, DECL_VINDEX (decl));
	      
	      /* We now know that DECL overrides something,
		 which is all that is important.  But, we must
		 continue to iterate through all the base-classes
		 in order to allow get_matching_virtual to check for
		 various illegal overrides.  */
	      found_overriden_fn = 1;
	    }
	}
    }
  if (virtualp)
    {
      if (DECL_VINDEX (decl) == NULL_TREE)
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
      tree fns = TREE_VEC_ELT (method_vec, i);
      tree fndecl = NULL_TREE;

      tree base_fndecls = NULL_TREE;
      tree binfos = BINFO_BASETYPES (TYPE_BINFO (t));
      int i, n_baseclasses = binfos ? TREE_VEC_LENGTH (binfos) : 0;

      /* First see if we have any virtual functions in this batch.  */
      for (; fns; fns = OVL_NEXT (fns))
	{
	  fndecl = OVL_CURRENT (fns);
	  if (DECL_VINDEX (fndecl))
	    break;
	}

      if (fns == NULL_TREE)
	continue;

      /* First we get a list of all possible functions that might be
	 hidden from each base class.  */
      for (i = 0; i < n_baseclasses; i++)
	{
	  tree base_binfo = TREE_VEC_ELT (binfos, i);
	  tree basetype = BINFO_TYPE (base_binfo);

	  base_fndecls = chainon (get_basefndecls (fndecl, basetype),
				  base_fndecls);
	}

      fns = OVL_NEXT (fns);

      /* ...then mark up all the base functions with overriders, preferring
	 overriders to hiders.  */
      if (base_fndecls)
	for (; fns; fns = OVL_NEXT (fns))
	  {
	    fndecl = OVL_CURRENT (fns);
	    if (DECL_VINDEX (fndecl))
	      mark_overriders (fndecl, base_fndecls);
	  }

      /* Now give a warning for all base functions without overriders,
	 as they are hidden.  */
      for (; base_fndecls; base_fndecls = TREE_CHAIN (base_fndecls))
	{
	  if (! overrides (TREE_PURPOSE (base_fndecls),
			   TREE_VALUE (base_fndecls)))
	    {
	      /* Here we know it is a hider, and no overrider exists.  */
	      cp_warning_at ("`%D' was hidden", TREE_VALUE (base_fndecls));
	      cp_warning_at ("  by `%D'", TREE_PURPOSE (base_fndecls));
	    }
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
	      if (DECL_ARTIFICIAL (elt))
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

extern int interface_only, interface_unknown;

/* Create default constructors, assignment operators, and so forth for
   the type indicated by T, if they are needed.
   CANT_HAVE_DEFAULT_CTOR, CANT_HAVE_CONST_CTOR, and
   CANT_HAVE_ASSIGNMENT are nonzero if, for whatever reason, the class
   cannot have a default constructor, copy constructor taking a const
   reference argument, or an assignment operator, respectively.  If a
   virtual destructor is created, its DECL is returned; otherwise the
   return value is NULL_TREE.  */

static tree
add_implicitly_declared_members (t, cant_have_default_ctor,
				 cant_have_const_cctor,
				 cant_have_assignment)
     tree t;
     int cant_have_default_ctor;
     int cant_have_const_cctor;
     int cant_have_assignment;
{
  tree default_fn;
  tree implicit_fns = NULL_TREE;
  tree name = TYPE_IDENTIFIER (t);
  tree virtual_dtor = NULL_TREE;
  tree *f;

  /* Destructor.  */
  if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (t) && !TYPE_HAS_DESTRUCTOR (t))
    {
      default_fn = cons_up_default_function (t, name, 0);
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
      default_fn = cons_up_default_function (t, name, 2);
      TREE_CHAIN (default_fn) = implicit_fns;
      implicit_fns = default_fn;
    }

  /* Copy constructor.  */
  if (! TYPE_HAS_INIT_REF (t) && ! TYPE_FOR_JAVA (t))
    {
      /* ARM 12.18: You get either X(X&) or X(const X&), but
	 not both.  --Chip  */
      default_fn = cons_up_default_function (t, name,
					     3 + cant_have_const_cctor);
      TREE_CHAIN (default_fn) = implicit_fns;
      implicit_fns = default_fn;
    }

  /* Assignment operator.  */
  if (! TYPE_HAS_ASSIGN_REF (t) && ! TYPE_FOR_JAVA (t))
    {
      default_fn = cons_up_default_function (t, name,
					     5 + cant_have_assignment);
      TREE_CHAIN (default_fn) = implicit_fns;
      implicit_fns = default_fn;
    }

  /* Now, hook all of the new functions on to TYPE_METHODS,
     and add them to the CLASSTYPE_METHOD_VEC.  */
  for (f = &implicit_fns; *f; f = &TREE_CHAIN (*f))
    add_method (t, 0, *f);
  *f = TYPE_METHODS (t);
  TYPE_METHODS (t) = implicit_fns;

  return virtual_dtor;
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
      else if (TREE_READONLY_DECL_P (w))
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

      if (integer_zerop (w))
	{
#ifdef EMPTY_FIELD_BOUNDARY
	  DECL_ALIGN (field) = MAX (DECL_ALIGN (field), 
				    EMPTY_FIELD_BOUNDARY);
#endif
#ifdef PCC_BITFIELD_TYPE_MATTERS
	  if (PCC_BITFIELD_TYPE_MATTERS)
	    DECL_ALIGN (field) = MAX (DECL_ALIGN (field), 
				      TYPE_ALIGN (type));
#endif
	}
    }
  else
    {
      /* Non-bit-fields are aligned for their type.  */
      DECL_BIT_FIELD (field) = 0;
      CLEAR_DECL_C_BIT_FIELD (field);
      DECL_ALIGN (field) = MAX (DECL_ALIGN (field), TYPE_ALIGN (type));
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
	if (TREE_CODE (field) == FIELD_DECL && !DECL_C_BIT_FIELD (field))
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

  /* Non-bit-fields are aligned for their type, except packed fields
     which require only BITS_PER_UNIT alignment.  */
  DECL_ALIGN (field) = MAX (DECL_ALIGN (field), 
			    (DECL_PACKED (field) 
			     ? BITS_PER_UNIT
			     : TYPE_ALIGN (TREE_TYPE (field))));
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
check_field_decls (t, access_decls, empty_p, 
		   cant_have_default_ctor_p, cant_have_const_ctor_p,
		   no_const_asn_ref_p)
     tree t;
     tree *access_decls;
     int *empty_p;
     int *cant_have_default_ctor_p;
     int *cant_have_const_ctor_p;
     int *no_const_asn_ref_p;
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

      GNU_xref_member (current_class_name, x);

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
	      /* The class is non-empty.  */
	      *empty_p = 0;
	      /* The class is not even nearly empty.  */
	      CLASSTYPE_NEARLY_EMPTY_P (t) = 0;
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

      DECL_FIELD_CONTEXT (x) = t;

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
	  
      DECL_SAVED_INSNS (x) = 0;

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
	    CLASSTYPE_REF_FIELDS_NEED_INIT (t) = 1;

	  /* ARM $12.6.2: [A member initializer list] (or, for an
	     aggregate, initialization by a brace-enclosed list) is the
	     only way to initialize nonstatic const and reference
	     members.  */
	  *cant_have_default_ctor_p = 1;
	  TYPE_HAS_COMPLEX_ASSIGN_REF (t) = 1;

	  if (! TYPE_HAS_CONSTRUCTOR (t) && extra_warnings)
	    {
	      if (DECL_NAME (x))
		cp_warning_at ("non-static reference `%#D' in class without a constructor", x);
	      else
		cp_warning_at ("non-static reference in class without a constructor", x);
	    }
	}

      type = strip_array_types (type);
      
      if (TREE_CODE (type) == POINTER_TYPE)
	has_pointers = 1;

      if (DECL_MUTABLE_P (x) || TYPE_HAS_MUTABLE_P (type))
	CLASSTYPE_HAS_MUTABLE (t) = 1;

      if (! pod_type_p (type)
	  /* For some reason, pointers to members are POD types themselves,
	     but are not allowed in POD structs.  Silly.  */
	  || TYPE_PTRMEM_P (type) || TYPE_PTRMEMFUNC_P (type))
	CLASSTYPE_NON_POD_P (t) = 1;

      /* If any field is const, the structure type is pseudo-const.  */
      if (CP_TYPE_CONST_P (type))
	{
	  C_TYPE_FIELDS_READONLY (t) = 1;
	  if (DECL_INITIAL (x) == NULL_TREE)
	    CLASSTYPE_READONLY_FIELDS_NEED_INIT (t) = 1;

	  /* ARM $12.6.2: [A member initializer list] (or, for an
	     aggregate, initialization by a brace-enclosed list) is the
	     only way to initialize nonstatic const and reference
	     members.  */
	  *cant_have_default_ctor_p = 1;
	  TYPE_HAS_COMPLEX_ASSIGN_REF (t) = 1;

	  if (! TYPE_HAS_CONSTRUCTOR (t) && extra_warnings)
	    {
	      if (DECL_NAME (x))
		cp_warning_at ("non-static const member `%#D' in class without a constructor", x);
	      else
		cp_warning_at ("non-static const member in class without a constructor", x);
	    }
	}
      /* A field that is pseudo-const makes the structure likewise.  */
      else if (IS_AGGR_TYPE (type))
	{
	  C_TYPE_FIELDS_READONLY (t) |= C_TYPE_FIELDS_READONLY (type);
	  CLASSTYPE_READONLY_FIELDS_NEED_INIT (t) 
	    |= CLASSTYPE_READONLY_FIELDS_NEED_INIT (type);
	}

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
      cp_warning ("`%#T' has pointer data members", t);
      
      if (! TYPE_HAS_INIT_REF (t))
	{
	  cp_warning ("  but does not override `%T(const %T&)'", t, t);
	  if (! TYPE_HAS_ASSIGN_REF (t))
	    cp_warning ("  or `operator=(const %T&)'", t);
	}
      else if (! TYPE_HAS_ASSIGN_REF (t))
	cp_warning ("  but does not override `operator=(const %T&)'", t);
    }


  /* Check anonymous struct/anonymous union fields.  */
  finish_struct_anon (t);

  /* We've built up the list of access declarations in reverse order.
     Fix that now.  */
  *access_decls = nreverse (*access_decls);
}

/* Return a FIELD_DECL for a pointer-to-virtual-table or
   pointer-to-virtual-base.  The NAME, ASSEMBLER_NAME, and TYPE of the
   field are as indicated.  The CLASS_TYPE in which this field occurs
   is also indicated.  FCONTEXT is the type that is needed for the debug
   info output routines.  *EMPTY_P is set to a non-zero value by this
   function to indicate that a class containing this field is
   non-empty.  */

static tree
build_vtbl_or_vbase_field (name, assembler_name, type, class_type, fcontext,
			   empty_p)
     tree name;
     tree assembler_name;
     tree type;
     tree class_type;
     tree fcontext;
     int *empty_p;
{
  tree field;

  /* This class is non-empty.  */
  *empty_p = 0;

  /* Build the FIELD_DECL.  */
  field = build_lang_decl (FIELD_DECL, name, type);
  DECL_ASSEMBLER_NAME (field) = assembler_name;
  DECL_VIRTUAL_P (field) = 1;
  DECL_ARTIFICIAL (field) = 1;
  DECL_FIELD_CONTEXT (field) = class_type;
  DECL_FCONTEXT (field) = fcontext;
  DECL_SAVED_INSNS (field) = 0;
  DECL_ALIGN (field) = TYPE_ALIGN (type);

  /* Return it.  */
  return field;
}

/* Return the BINFO_OFFSET for BINFO as a native integer, not an
   INTEGER_CST.  */

static unsigned HOST_WIDE_INT
get_binfo_offset_as_int (binfo)
     tree binfo;
{
  tree offset;

  offset = BINFO_OFFSET (binfo);
  my_friendly_assert (TREE_CODE (offset) == INTEGER_CST, 20000313);
  my_friendly_assert (TREE_INT_CST_HIGH (offset) == 0, 20000313);

  return (unsigned HOST_WIDE_INT) TREE_INT_CST_LOW (offset);
}

/* Record the type of BINFO in the slot in DATA (which is really a
   `varray_type *') corresponding to the BINFO_OFFSET.  */

static tree
dfs_record_base_offsets (binfo, data)
     tree binfo;
     void *data;
{
  varray_type *v;
  unsigned HOST_WIDE_INT offset = get_binfo_offset_as_int (binfo);

  v = (varray_type *) data;
  while (VARRAY_SIZE (*v) <= offset)
    VARRAY_GROW (*v, 2 * VARRAY_SIZE (*v));
  VARRAY_TREE (*v, offset) = tree_cons (NULL_TREE,
					BINFO_TYPE (binfo),
					VARRAY_TREE (*v, offset));

  return NULL_TREE;
}

/* Returns non-NULL if there is already an entry in DATA (which is
   really a `varray_type') indicating that an object with the same
   type of BINFO is already at the BINFO_OFFSET for BINFO.  */

static tree
dfs_search_base_offsets (binfo, data)
     tree binfo;
     void *data;
{
  if (is_empty_class (BINFO_TYPE (binfo)))
    {
      varray_type v = (varray_type) data;
      unsigned HOST_WIDE_INT offset;
      tree t;

      /* Find the offset for this BINFO.  */
      offset = get_binfo_offset_as_int (binfo);
      /* If we haven't yet encountered any objects at offsets that
	 big, then there's no conflict.  */
      if (VARRAY_SIZE (v) <= offset)
	return NULL_TREE;
      /* Otherwise, go through the objects already allocated at this
	 offset.  */
      for (t = VARRAY_TREE (v, offset); t; t = TREE_CHAIN (t))
	if (same_type_p (TREE_VALUE (t), BINFO_TYPE (binfo)))
	  return binfo;
    }

  return NULL_TREE;
}

/* DECL is a FIELD_DECL corresponding either to a base subobject of a
   non-static data member of the type indicated by RLI.  BINFO is the
   binfo corresponding to the base subobject, or, if this is a
   non-static data-member, a dummy BINFO for the type of the data
   member.  BINFO may be NULL if checks to see if the field overlaps
   an existing field with the same type are not required.  V maps
   offsets to types already located at those offsets.  This function
   determines the position of the DECL.  */

static void
layout_nonempty_base_or_field (rli, decl, binfo, v)
     record_layout_info rli;
     tree decl;
     tree binfo;
     varray_type v;
{
  /* Try to place the field.  It may take more than one try if we have
     a hard time placing the field without putting two objects of the
     same type at the same address.  */
  while (1)
    {
      tree offset;

      /* Layout this field.  */
      layout_field (rli, decl);
      
      /* Now that we know where it wil be placed, update its
	 BINFO_OFFSET.  */
      offset = size_int (CEIL (TREE_INT_CST_LOW (DECL_FIELD_BITPOS (decl)),
			       BITS_PER_UNIT));
      if (binfo)
	propagate_binfo_offsets (binfo, offset);
 
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
	 empty class, have non-zero size, any overlap can happen only
	 with a direct or indirect base-class -- it can't happen with
	 a data member.  */
      if (binfo && flag_new_abi && dfs_walk (binfo,
					     dfs_search_base_offsets,
					     dfs_skip_vbases,
					     v))
	{
	  /* Undo the propogate_binfo_offsets call.  */
	  offset = convert (sizetype,
			    size_diffop (size_zero_node, offset));
	  propagate_binfo_offsets (binfo, offset);

	  /* Strip off the size allocated to this field.  That puts us
	     at the first place we could have put the field with
	     proper alignment.  */
	  rli->const_size -= TREE_INT_CST_LOW (DECL_SIZE (decl));
	  /* Bump up by th alignment required for the type, without
	     virtual base classes.  */
	  rli->const_size += CLASSTYPE_ALIGN (BINFO_TYPE (binfo));
	}
      else
	/* There was no conflict.  We're done laying out this field.  */
	break;
    }
}

/* Build a FIELD_DECL for the base given by BINFO in the class
   *indicated by RLI.  If the new object is non-empty, clear *EMPTY_P.
   *BASE_ALIGN is a running maximum of the alignments of any base
   *class.  */

static void
build_base_field (rli, binfo, empty_p, base_align, v)
     record_layout_info rli;
     tree binfo;
     int *empty_p;
     unsigned int *base_align;
     varray_type *v;
{
  tree basetype = BINFO_TYPE (binfo);
  tree decl;

  if (!COMPLETE_TYPE_P (basetype))
    /* This error is now reported in xref_tag, thus giving better
       location information.  */
    return;
  
  decl = build_lang_decl (FIELD_DECL, NULL_TREE, basetype);
  DECL_ARTIFICIAL (decl) = 1;
  DECL_FIELD_CONTEXT (decl) = rli->t;
  DECL_SIZE (decl) = CLASSTYPE_SIZE (basetype);
  DECL_SIZE_UNIT (decl) = CLASSTYPE_SIZE_UNIT (basetype);
  DECL_ALIGN (decl) = CLASSTYPE_ALIGN (basetype);
  
  if (! flag_new_abi)
    {
      /* Brain damage for backwards compatibility.  For no good
	 reason, the old layout_basetypes made every base at least
	 as large as the alignment for the bases up to that point,
	 gratuitously wasting space.  So we do the same thing
	 here.  */
      *base_align = MAX (*base_align, DECL_ALIGN (decl));
      DECL_SIZE (decl)
	= size_binop (MAX_EXPR, DECL_SIZE (decl), bitsize_int (*base_align));
      DECL_SIZE_UNIT (decl)
	= size_binop (MAX_EXPR, DECL_SIZE_UNIT (decl),
		      size_int (*base_align / BITS_PER_UNIT));
    }

  if (!integer_zerop (DECL_SIZE (decl)))
    {
      /* The containing class is non-empty because it has a non-empty
	 base class.  */
      *empty_p = 0;

      /* Try to place the field.  It may take more than one try if we
	 have a hard time placing the field without putting two
	 objects of the same type at the same address.  */
      layout_nonempty_base_or_field (rli, decl, binfo, *v);
    }
  else
    {
      /* This code assumes that zero-sized classes have one-byte
	 alignment.  There might someday be a system where that's not
	 true.  */
      my_friendly_assert (TYPE_ALIGN (basetype) == BITS_PER_UNIT, 
			  20000314);

      /* This is an empty base class.  We first try to put it at
	 offset zero.  */
      if (dfs_walk (binfo, dfs_search_base_offsets, dfs_skip_vbases, *v))
	{
	  /* That didn't work.  Now, we move forward from the next
	     available spot in the class.  */
	  propagate_binfo_offsets (binfo, size_int (rli->const_size));
	  while (1) 
	    {
	      if (!dfs_walk (binfo, dfs_search_base_offsets, 
			     dfs_skip_vbases, *v))
		/* We finally found a spot where there's no overlap.  */
		break;

	      /* There's overlap here, too.  Bump along to the next
		 spot.  */
	      propagate_binfo_offsets (binfo, size_one_node);
	    }
	}
    }

  /* Check for inaccessible base classes.  If the same base class
     appears more than once in the hierarchy, but isn't virtual, then
     it's ambiguous.  */
  if (get_base_distance (basetype, rli->t, 0, NULL) == -2)
    cp_warning ("direct base `%T' inaccessible in `%T' due to ambiguity",
		basetype, rli->t);
  
  /* Record the offsets of BINFO and its base subobjects.  */
  dfs_walk (binfo,
	    dfs_record_base_offsets,
	    dfs_skip_vbases,
	    v);
}

/* Layout all of the non-virtual base classes.  Returns a map from
   offsets to types present at those offsets.  */

static varray_type
build_base_fields (rli, empty_p)
     record_layout_info rli;
     int *empty_p;
{
  /* Chain to hold all the new FIELD_DECLs which stand in for base class
     subobjects.  */
  tree rec = rli->t;
  int n_baseclasses = CLASSTYPE_N_BASECLASSES (rec);
  int i;
  varray_type v;
  unsigned int base_align = 0;

  /* Create the table mapping offsets to empty base classes.  */
  VARRAY_TREE_INIT (v, 32, "v");

  /* Under the new ABI, the primary base class is always allocated
     first.  */
  if (flag_new_abi && CLASSTYPE_HAS_PRIMARY_BASE_P (rec))
    build_base_field (rli, CLASSTYPE_PRIMARY_BINFO (rec), 
		      empty_p, &base_align, &v);

  /* Now allocate the rest of the bases.  */
  for (i = 0; i < n_baseclasses; ++i)
    {
      tree base_binfo;

      /* Under the new ABI, the primary base was already allocated
	 above, so we don't need to allocate it again here.  */
      if (flag_new_abi && i == CLASSTYPE_VFIELD_PARENT (rec))
	continue;

      base_binfo = BINFO_BASETYPE (TYPE_BINFO (rec), i);

      /* A primary virtual base class is allocated just like any other
	 base class, but a non-primary virtual base is allocated
	 later, in layout_basetypes.  */
      if (TREE_VIA_VIRTUAL (base_binfo) 
	  && !BINFO_PRIMARY_MARKED_P (base_binfo))
	continue;

      build_base_field (rli, base_binfo, empty_p, &base_align, &v);
    }

  return v;
}

/* Go through the TYPE_METHODS of T issuing any appropriate
   diagnostics, figuring out which methods override which other
   methods, and so forth.  */

static void
check_methods (t)
     tree t;
{
  tree x;
  int seen_one_arg_array_delete_p = 0;

  for (x = TYPE_METHODS (t); x; x = TREE_CHAIN (x))
    {
      GNU_xref_member (current_class_name, x);

      /* If this was an evil function, don't keep it in class.  */
      if (IDENTIFIER_ERROR_LOCUS (DECL_ASSEMBLER_NAME (x)))
	continue;

      DECL_SAVED_INSNS (x) = 0;
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

      if (DECL_ARRAY_DELETE_OPERATOR_P (x))
	{
	  tree second_parm;

	  /* When dynamically allocating an array of this type, we
	     need a "cookie" to record how many elements we allocated,
	     even if the array elements have no non-trivial
	     destructor, if the usual array deallocation function
	     takes a second argument of type size_t.  The standard (in
	     [class.free]) requires that the second argument be set
	     correctly.  */
	  second_parm = TREE_CHAIN (TYPE_ARG_TYPES (TREE_TYPE (x)));
	  /* This is overly conservative, but we must maintain this
	     behavior for backwards compatibility.  */
	  if (!flag_new_abi && second_parm != void_list_node)
	    TYPE_VEC_DELETE_TAKES_SIZE (t) = 1;
	  /* Under the new ABI, we choose only those function that are
	     explicitly declared as `operator delete[] (void *,
	     size_t)'.  */
	  else if (flag_new_abi 
		   && !seen_one_arg_array_delete_p
		   && second_parm
		   && TREE_CHAIN (second_parm) == void_list_node
		   && same_type_p (TREE_VALUE (second_parm), sizetype))
	    TYPE_VEC_DELETE_TAKES_SIZE (t) = 1;
	  /* If there's no second parameter, then this is the usual
	     deallocation function.  */
	  else if (second_parm == void_list_node)
	    seen_one_arg_array_delete_p = 1;
	}
    }
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

/* Check the validity of the bases and members declared in T.  Add any
   implicitly-generated functions (like copy-constructors and
   assignment operators).  Compute various flag bits (like
   CLASSTYPE_NON_POD_T) for T.  This routine works purely at the C++
   level: i.e., independently of the ABI in use.  */

static void
check_bases_and_members (t, empty_p)
     tree t;
     int *empty_p;
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

  /* Assume that the class is nearly empty; we'll clear this flag if
     it turns out not to be nearly empty.  */
  CLASSTYPE_NEARLY_EMPTY_P (t) = 1;

  /* Check all the base-classes. */
  check_bases (t, &cant_have_default_ctor, &cant_have_const_ctor,
	       &no_const_asn_ref);

  /* Check all the data member declarations.  */
  check_field_decls (t, &access_decls, empty_p,
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
    |= TYPE_HAS_ASSIGN_REF (t) || TYPE_USES_VIRTUAL_BASECLASSES (t);

  /* Synthesize any needed methods.  Note that methods will be synthesized
     for anonymous unions; grok_x_components undoes that.  */
  add_implicitly_declared_members (t, cant_have_default_ctor,
				   cant_have_const_ctor,
				   no_const_asn_ref);

  /* Process the using-declarations.  */
  for (; access_decls; access_decls = TREE_CHAIN (access_decls))
    handle_using_decl (TREE_VALUE (access_decls), t);

  /* Build and sort the CLASSTYPE_METHOD_VEC.  */
  finish_struct_methods (t);
}

/* If T needs a pointer to its virtual function table, set TYPE_VFIELD
   accordingly.  If a new vfield was created (because T doesn't have a
   primary base class), then the newly created field is returned.  It
   is not added to the TYPE_FIELDS list; it is the callers
   responsibility to do that.  */

static tree
create_vtable_ptr (t, empty_p, has_virtual_p, 
		   new_virtuals_p, overridden_virtuals_p)
     tree t;
     int *empty_p;
     int *has_virtual_p;
     tree *new_virtuals_p;
     tree *overridden_virtuals_p;
{
  tree fn;

  /* Loop over the virtual functions, adding them to our various
     vtables.  */
  for (fn = TYPE_METHODS (t); fn; fn = TREE_CHAIN (fn))
    if (DECL_VINDEX (fn))
      add_virtual_function (new_virtuals_p, overridden_virtuals_p,
			    has_virtual_p, fn, t);

  /* Even if there weren't any new virtual functions, we might need a
     new virtual function table if we're supposed to include vptrs in
     all classes that need them.  */
  if (TYPE_CONTAINS_VPTR_P (t) && vptrs_present_everywhere_p ())
    start_vtable (t, has_virtual_p);
    
  /* If we couldn't find an appropriate base class, create a new field
     here.  */
  if (*has_virtual_p && !TYPE_VFIELD (t))
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
      TYPE_VFIELD (t) 
	= build_vtbl_or_vbase_field (get_vfield_name (t),
				     get_identifier (VFIELD_BASE),
				     vtbl_ptr_type_node,
				     t,
				     t,
				     empty_p);

      if (flag_new_abi && CLASSTYPE_N_BASECLASSES (t))
	/* If there were any baseclasses, they can't possibly be at
	   offset zero any more, because that's where the vtable
	   pointer is.  So, converting to a base class is going to
	   take work.  */
	TYPE_BASE_CONVS_MAY_REQUIRE_CODE_P (t) = 1;

      return TYPE_VFIELD (t);
    }

  return NULL_TREE;
}

/* Fixup the inline function given by INFO now that the class is
   complete.  */

static void
fixup_pending_inline (info)
     struct pending_inline *info;
{
  if (info)
    {
      tree args;
      tree fn = info->fndecl;

      args = DECL_ARGUMENTS (fn);
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
    fixup_pending_inline (DECL_PENDING_INLINE_INFO (method));

  /* Do friends.  */
  for (method = CLASSTYPE_INLINE_FRIENDS (type); 
       method; 
       method = TREE_CHAIN (method))
    fixup_pending_inline (DECL_PENDING_INLINE_INFO (TREE_VALUE (method)));
  CLASSTYPE_INLINE_FRIENDS (type) = NULL_TREE;
}

/* Called from propagate_binfo_offsets via dfs_walk.  */

static tree
dfs_propagate_binfo_offsets (binfo, data)
     tree binfo; 
     void *data;
{
  tree offset = (tree) data;

  /* Update the BINFO_OFFSET for this base.  */
  BINFO_OFFSET (binfo) = fold (build (PLUS_EXPR,
				      sizetype,
				      BINFO_OFFSET (binfo), 
				      offset));

  SET_BINFO_MARKED (binfo);

  return NULL_TREE;
}

/* Add OFFSET to all base types of BINFO which is a base in the
   hierarchy dominated by T.

   OFFSET, which is a type offset, is number of bytes.

   Note that we don't have to worry about having two paths to the
   same base type, since this type owns its association list.  */

static void
propagate_binfo_offsets (binfo, offset)
     tree binfo;
     tree offset;
{
  dfs_walk (binfo, 
	    dfs_propagate_binfo_offsets, 
	    dfs_skip_nonprimary_vbases_unmarkedp,
	    offset);
  dfs_walk (binfo,
	    dfs_unmark,
	    dfs_skip_nonprimary_vbases_markedp,
	    NULL);
}

/* Called via dfs_walk from layout_virtual bases.  */

static tree
dfs_set_offset_for_shared_vbases (binfo, data)
     tree binfo;
     void *data;
{
  if (TREE_VIA_VIRTUAL (binfo) && BINFO_PRIMARY_MARKED_P (binfo))
    {
      /* Update the shared copy.  */
      tree shared_binfo;

      shared_binfo = BINFO_FOR_VBASE (BINFO_TYPE (binfo), (tree) data);
      BINFO_OFFSET (shared_binfo) = BINFO_OFFSET (binfo);
    }

  return NULL_TREE;
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
  if (TREE_VIA_VIRTUAL (binfo) && !BINFO_PRIMARY_MARKED_P (binfo))
    {
      tree t = (tree) data;
      tree vbase;
      tree offset;
      
      vbase = BINFO_FOR_VBASE (BINFO_TYPE (binfo), t);
      offset = size_diffop (BINFO_OFFSET (vbase), BINFO_OFFSET (binfo));
      propagate_binfo_offsets (binfo, offset);
    }

  return NULL_TREE;
}

/* Set BINFO_OFFSET for all of the virtual bases for T.  Update
   TYPE_ALIGN and TYPE_SIZE for T.  */

static void
layout_virtual_bases (t)
     tree t;
{
  tree vbase;
  unsigned HOST_WIDE_INT dsize;

  /* DSIZE is the size of the class without the virtual bases.  */
  dsize = tree_low_cst (TYPE_SIZE (t), 1);

  /* Make every class have alignment of at least one.  */
  TYPE_ALIGN (t) = MAX (TYPE_ALIGN (t), BITS_PER_UNIT);

  /* Go through the virtual bases, allocating space for each virtual
     base that is not already a primary base class.  */
  for (vbase = CLASSTYPE_VBASECLASSES (t); 
       vbase; 
       vbase = TREE_CHAIN (vbase))
    if (!BINFO_VBASE_PRIMARY_P (vbase))
      {
	/* This virtual base is not a primary base of any class in the
	   hierarchy, so we have to add space for it.  */
	tree basetype;
	unsigned int desired_align;

	basetype = BINFO_TYPE (vbase);
	desired_align = TYPE_ALIGN (basetype);
	TYPE_ALIGN (t) = MAX (TYPE_ALIGN (t), desired_align);

	/* Add padding so that we can put the virtual base class at an
	   appropriately aligned offset.  */
	dsize = CEIL (dsize, desired_align) * desired_align;
	/* And compute the offset of the virtual base.  */
	propagate_binfo_offsets (vbase, 
				 size_int (CEIL (dsize, BITS_PER_UNIT)));
	/* Every virtual baseclass takes a least a UNIT, so that we can
	   take it's address and get something different for each base.  */
	dsize += MAX (BITS_PER_UNIT,
		      tree_low_cst (CLASSTYPE_SIZE (basetype), 0));
      }

  /* Make sure that all of the CLASSTYPE_VBASECLASSES have their
     BINFO_OFFSET set correctly.  Those we just allocated certainly
     will.  The others are primary baseclasses; we walk the hierarchy
     to find the primary copies and update the shared copy.  */
  dfs_walk (TYPE_BINFO (t), 
	    dfs_set_offset_for_shared_vbases, 
	    dfs_unmarked_real_bases_queue_p,
	    t);

  /* Now, go through the TYPE_BINFO hierarchy again, setting the
     BINFO_OFFSETs correctly for all non-primary copies of the virtual
     bases and their direct and indirect bases.  The ambiguity checks
     in get_base_distance depend on the BINFO_OFFSETs being set
     correctly.  */
  dfs_walk (TYPE_BINFO (t), dfs_set_offset_for_unshared_vbases, NULL, t);
  for (vbase = CLASSTYPE_VBASECLASSES (t);
       vbase;
       vbase = TREE_CHAIN (vbase))
    dfs_walk (vbase, dfs_set_offset_for_unshared_vbases, NULL, t);

  /* Now, make sure that the total size of the type is a multiple of
     its alignment.  */
  dsize = CEIL (dsize, TYPE_ALIGN (t)) * TYPE_ALIGN (t);
  TYPE_SIZE (t) = bitsize_int (dsize);
  TYPE_SIZE_UNIT (t) = convert (sizetype,
				size_binop (FLOOR_DIV_EXPR, TYPE_SIZE (t),
					    bitsize_int (BITS_PER_UNIT)));
}

/* Finish the work of layout_record, now taking virtual bases into account.
   Also compute the actual offsets that our base classes will have.
   This must be performed after the fields are laid out, since virtual
   baseclasses must lay down at the end of the record.  */

static void
layout_basetypes (rec)
     tree rec;
{
  tree vbase_types;

  if (CLASSTYPE_N_BASECLASSES (rec) == 0)
    return;

#ifdef STRUCTURE_SIZE_BOUNDARY
  /* Packed structures don't need to have minimum size.  */
  if (! TYPE_PACKED (rec))
    TYPE_ALIGN (rec) = MAX (TYPE_ALIGN (rec), STRUCTURE_SIZE_BOUNDARY);
#endif

  /* Allocate the virtual base classes.  */
  layout_virtual_bases (rec);

  /* Get all the virtual base types that this type uses.  The
     TREE_VALUE slot holds the virtual baseclass type.  Note that
     get_vbase_types makes copies of the virtual base BINFOs, so that
     the vbase_types are unshared.  */
  for (vbase_types = CLASSTYPE_VBASECLASSES (rec); vbase_types;
       vbase_types = TREE_CHAIN (vbase_types))
    if (extra_warnings)
      {
	tree basetype = BINFO_TYPE (vbase_types);
	if (get_base_distance (basetype, rec, 0, (tree*)0) == -2)
	  cp_warning ("virtual base `%T' inaccessible in `%T' due to ambiguity",
		      basetype, rec);
      }
}

/* Calculate the TYPE_SIZE, TYPE_ALIGN, etc for T.  Calculate
   BINFO_OFFSETs for all of the base-classes.  Position the vtable
   pointer.  */

static void
layout_class_type (t, empty_p, has_virtual_p, 
		   new_virtuals_p, overridden_virtuals_p)
     tree t;
     int *empty_p;
     int *has_virtual_p;
     tree *new_virtuals_p;
     tree *overridden_virtuals_p;
{
  tree non_static_data_members;
  tree field;
  tree vptr;
  record_layout_info rli;
  varray_type v;
  int i;

  /* Keep track of the first non-static data member.  */
  non_static_data_members = TYPE_FIELDS (t);

  /* Initialize the layout information.  */
  rli = new_record_layout_info (t);

  /* If possible, we reuse the virtual function table pointer from one
     of our base classes.  */
  determine_primary_base (t, has_virtual_p);

  /* Create a pointer to our virtual function table.  */
  vptr = create_vtable_ptr (t, empty_p, has_virtual_p,
			    new_virtuals_p, overridden_virtuals_p);

  /* Under the new ABI, the vptr is always the first thing in the
     class.  */
  if (flag_new_abi && vptr)
    {
      TYPE_FIELDS (t) = chainon (vptr, TYPE_FIELDS (t));
      layout_field (rli, vptr);
    }

  /* Add pointers to all of our virtual base-classes.  */
  TYPE_FIELDS (t) = chainon (build_vbase_pointer_fields (rli, empty_p),
			     TYPE_FIELDS (t));
  /* Build FIELD_DECLs for all of the non-virtual base-types.  */
  v = build_base_fields (rli, empty_p);

  /* CLASSTYPE_INLINE_FRIENDS is really TYPE_NONCOPIED_PARTS.  Thus,
     we have to save this before we start modifying
     TYPE_NONCOPIED_PARTS.  */
  fixup_inline_methods (t);

  /* Layout the non-static data members.  */
  for (field = non_static_data_members; 
       field; 
       field = TREE_CHAIN (field))
    {
      tree binfo;
      tree type;
      tree padding;

      /* We still pass things that aren't non-static data members to
	 the back-end, in case it wants to do something with them.  */
      if (TREE_CODE (field) != FIELD_DECL)
	{
	  layout_field (rli, field);
	  continue;
	}

      type = TREE_TYPE (field);

      /* If this field is a bit-field whose width is greater than its
	 type, then there are some special rules for allocating it
	 under the new ABI.  Under the old ABI, there were no special
	 rules, but the back-end can't handle bitfields longer than a
	 `long long', so we use the same mechanism.  */
      if (DECL_C_BIT_FIELD (field)
	  && ((flag_new_abi 
	       && INT_CST_LT (TYPE_SIZE (type), DECL_SIZE (field)))
	      || (!flag_new_abi
		  && compare_tree_int (DECL_SIZE (field),
				       TYPE_PRECISION
				       (long_long_unsigned_type_node)) > 0)))
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
	  padding = size_diffop (DECL_SIZE (field), 
				 TYPE_SIZE (integer_type));
	  DECL_SIZE (field) = TYPE_SIZE (integer_type);
	  DECL_ALIGN (field) = TYPE_ALIGN (integer_type);
	}
      else
	padding = NULL_TREE;

      /* Create a dummy BINFO corresponding to this field.  */
      binfo = make_binfo (size_zero_node, type, NULL_TREE, NULL_TREE);
      unshare_base_binfos (binfo);
      layout_nonempty_base_or_field (rli, field, binfo, v);

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
	  layout_nonempty_base_or_field (rli, padding_field, NULL_TREE, v);
	}
    }

  /* Clean up.  */
  VARRAY_FREE (v);
  
  /* It might be the case that we grew the class to allocate a
     zero-sized base class.  That won't be reflected in RLI, yet,
     because we are willing to overlay multiple bases at the same
     offset.  However, now we need to make sure that RLI is big enough
     to reflect the entire class.  */
  for (i = 0; i < CLASSTYPE_N_BASECLASSES (t); ++i)
    {
      tree base_binfo;
      unsigned HOST_WIDE_INT offset;

      base_binfo = BINFO_BASETYPE (TYPE_BINFO (t), i);
      offset = get_binfo_offset_as_int (base_binfo);
      if (offset * BITS_PER_UNIT > rli->const_size)
	rli->const_size = (offset + 1) * BITS_PER_UNIT;
    }

  /* We make all structures have at least one element, so that they
     have non-zero size.  In the new ABI, the class may be empty even
     if it has basetypes.  Therefore, we add the fake field after all
     the other fields; if there are already FIELD_DECLs on the list,
     their offsets will not be disturbed.  */
  if (*empty_p)
    {
      tree padding;

      padding = build_lang_decl (FIELD_DECL, NULL_TREE, char_type_node);
      layout_field (rli, padding);
      TYPE_NONCOPIED_PARTS (t) 
	= tree_cons (NULL_TREE, padding, TYPE_NONCOPIED_PARTS (t));
      TREE_STATIC (TYPE_NONCOPIED_PARTS (t)) = 1;
    }

  /* Under the old ABI, the vptr comes at the very end of the 
     class.   */
  if (!flag_new_abi && vptr)
    {
      layout_field (rli, vptr);
      TYPE_FIELDS (t) = chainon (TYPE_FIELDS (t), vptr);
    }
  
  /* Let the back-end lay out the type. Note that at this point we
     have only included non-virtual base-classes; we will lay out the
     virtual base classes later.  So, the TYPE_SIZE/TYPE_ALIGN after
     this call are not necessarily correct; they are just the size and
     alignment when no virtual base clases are used.  */
  finish_record_layout (rli);

  /* Delete all zero-width bit-fields from the list of fields.  Now
     that the type is laid out they are no longer important.  */
  remove_zero_width_bit_fields (t);

  /* Remember the size and alignment of the class before adding
     the virtual bases.  */
  if (*empty_p && flag_new_abi)
    {
      CLASSTYPE_SIZE (t) = bitsize_int (0);
      CLASSTYPE_SIZE_UNIT (t) = size_zero_node;
    }
  else if (flag_new_abi && TYPE_HAS_COMPLEX_INIT_REF (t)
	   && TYPE_HAS_COMPLEX_ASSIGN_REF (t))
    {
      CLASSTYPE_SIZE (t) = TYPE_BINFO_SIZE (t);
      CLASSTYPE_SIZE_UNIT (t) = TYPE_BINFO_SIZE_UNIT (t);
    }
  else
    {
      CLASSTYPE_SIZE (t) = TYPE_SIZE (t);
      CLASSTYPE_SIZE_UNIT (t) = TYPE_SIZE_UNIT (t);
    }

  CLASSTYPE_ALIGN (t) = TYPE_ALIGN (t);

  /* Set the TYPE_DECL for this type to contain the right
     value for DECL_OFFSET, so that we can use it as part
     of a COMPONENT_REF for multiple inheritance.  */
  layout_decl (TYPE_MAIN_DECL (t), 0);

  /* Now fix up any virtual base class types that we left lying
     around.  We must get these done before we try to lay out the
     virtual function table.  As a side-effect, this will remove the
     base subobject fields.  */
  layout_basetypes (t);
}
     
/* Create a RECORD_TYPE or UNION_TYPE node for a C struct or union declaration
   (or C++ class declaration).

   For C++, we must handle the building of derived classes.
   Also, C++ allows static class members.  The way that this is
   handled is to keep the field name where it is (as the DECL_NAME
   of the field), and place the overloaded decl in the bit position
   of the field.  layout_record and layout_union will know about this.

   More C++ hair: inline functions have text in their
   DECL_PENDING_INLINE_INFO nodes which must somehow be parsed into
   meaningful tree structure.  After the struct has been laid out, set
   things up so that this can happen.

   And still more: virtual functions.  In the case of single inheritance,
   when a new virtual function is seen which redefines a virtual function
   from the base class, the new virtual function is placed into
   the virtual function table at exactly the same address that
   it had in the base class.  When this is extended to multiple
   inheritance, the same thing happens, except that multiple virtual
   function tables must be maintained.  The first virtual function
   table is treated in exactly the same way as in the case of single
   inheritance.  Additional virtual function tables have different
   DELTAs, which tell how to adjust `this' to point to the right thing.

   ATTRIBUTES is the set of decl attributes to be applied, if any.  */

void
finish_struct_1 (t)
     tree t;
{
  tree x;
  int has_virtual;
  /* The NEW_VIRTUALS is a TREE_LIST.  The TREE_VALUE of each node is
     a FUNCTION_DECL.  Each of these functions is a virtual function
     declared in T that does not override any virtual function from a
     base class.  */
  tree new_virtuals = NULL_TREE;
  /* The OVERRIDDEN_VIRTUALS list is like the NEW_VIRTUALS list,
     except that each declaration here overrides the declaration from
     a base class.  */
  tree overridden_virtuals = NULL_TREE;
  int n_fields = 0;
  tree vfield;
  int empty = 1;

  if (COMPLETE_TYPE_P (t))
    {
      if (IS_AGGR_TYPE (t))
	cp_error ("redefinition of `%#T'", t);
      else
	my_friendly_abort (172);
      popclass ();
      return;
    }

  GNU_xref_decl (current_function_decl, t);

  /* If this type was previously laid out as a forward reference,
     make sure we lay it out again.  */
  TYPE_SIZE (t) = NULL_TREE;
  CLASSTYPE_GOT_SEMICOLON (t) = 0;
  CLASSTYPE_VFIELD_PARENT (t) = -1;
  has_virtual = 0;
  CLASSTYPE_RTTI (t) = NULL_TREE;

  /* Do end-of-class semantic processing: checking the validity of the
     bases and members and add implicitly generated methods.  */
  check_bases_and_members (t, &empty);

  /* Layout the class itself.  */
  layout_class_type (t, &empty, &has_virtual,
		     &new_virtuals, &overridden_virtuals);

  /* Set up the DECL_FIELD_BITPOS of the vfield if we need to, as we
     might need to know it for setting up the offsets in the vtable
     (or in thunks) below.  */
  vfield = TYPE_VFIELD (t);
  if (vfield != NULL_TREE
      && DECL_FIELD_CONTEXT (vfield) != t)
    {
      tree binfo = get_binfo (DECL_FIELD_CONTEXT (vfield), t, 0);
      tree offset = convert (bitsizetype, BINFO_OFFSET (binfo));

      vfield = copy_node (vfield);
      copy_lang_decl (vfield);

      if (! integer_zerop (offset))
	offset = size_binop (MULT_EXPR, offset, bitsize_int (BITS_PER_UNIT));

      DECL_FIELD_CONTEXT (vfield) = t;
      DECL_FIELD_BITPOS (vfield)
	= size_binop (PLUS_EXPR, offset, bit_position (vfield));
      TYPE_VFIELD (t) = vfield;
    }

  overridden_virtuals 
    = modify_all_vtables (t, &has_virtual, nreverse (overridden_virtuals));

  /* If necessary, create the primary vtable for this class.  */
  if (new_virtuals
      || overridden_virtuals
      || (TYPE_CONTAINS_VPTR_P (t) && vptrs_present_everywhere_p ()))
    {
      new_virtuals = nreverse (new_virtuals);
      /* We must enter these virtuals into the table.  */
      if (!CLASSTYPE_HAS_PRIMARY_BASE_P (t))
	{
	  if (! CLASSTYPE_COM_INTERFACE (t))
	    {
	      /* The second slot is for the tdesc pointer when thunks
		 are used.  */
	      if (flag_vtable_thunks)
		new_virtuals = tree_cons (NULL_TREE, NULL_TREE, new_virtuals);

	      /* The first slot is for the rtti offset.  */
	      new_virtuals = tree_cons (NULL_TREE, NULL_TREE, new_virtuals);

	      set_rtti_entry (new_virtuals,
			      convert (ssizetype, integer_zero_node), t);
	    }
	  build_primary_vtable (NULL_TREE, t);
	}
      else if (! BINFO_NEW_VTABLE_MARKED (TYPE_BINFO (t)))
	/* Here we know enough to change the type of our virtual
	   function table, but we will wait until later this function.  */
	build_primary_vtable (CLASSTYPE_PRIMARY_BINFO (t), t);

      /* If this type has basetypes with constructors, then those
	 constructors might clobber the virtual function table.  But
	 they don't if the derived class shares the exact vtable of the base
	 class.  */

      CLASSTYPE_NEEDS_VIRTUAL_REINIT (t) = 1;
    }
  /* If we didn't need a new vtable, see if we should copy one from
     the base.  */
  else if (CLASSTYPE_HAS_PRIMARY_BASE_P (t))
    {
      tree binfo = CLASSTYPE_PRIMARY_BINFO (t);

      /* This class contributes nothing new to the virtual function
	 table.  However, it may have declared functions which
	 went into the virtual function table "inherited" from the
	 base class.  If so, we grab a copy of those updated functions,
	 and pretend they are ours.  */

      /* See if we should steal the virtual info from base class.  */
      if (TYPE_BINFO_VTABLE (t) == NULL_TREE)
	TYPE_BINFO_VTABLE (t) = BINFO_VTABLE (binfo);
      if (TYPE_BINFO_VIRTUALS (t) == NULL_TREE)
	TYPE_BINFO_VIRTUALS (t) = BINFO_VIRTUALS (binfo);
      if (TYPE_BINFO_VTABLE (t) != BINFO_VTABLE (binfo))
	CLASSTYPE_NEEDS_VIRTUAL_REINIT (t) = 1;
    }

  if (TYPE_CONTAINS_VPTR_P (t))
    {
      if (TYPE_BINFO_VTABLE (t))
	my_friendly_assert (DECL_VIRTUAL_P (TYPE_BINFO_VTABLE (t)),
			    20000116);
      if (!CLASSTYPE_HAS_PRIMARY_BASE_P (t))
	my_friendly_assert (TYPE_BINFO_VIRTUALS (t) == NULL_TREE,
			    20000116);

      CLASSTYPE_VSIZE (t) = has_virtual;
      /* Entries for virtual functions defined in the primary base are
	 followed by entries for new functions unique to this class.  */
      TYPE_BINFO_VIRTUALS (t) 
	= chainon (TYPE_BINFO_VIRTUALS (t), new_virtuals);
      /* Finally, add entries for functions that override virtuals
	 from non-primary bases.  */
      TYPE_BINFO_VIRTUALS (t) 
	= chainon (TYPE_BINFO_VIRTUALS (t), overridden_virtuals);
    }

  /* If we created a new vtbl pointer for this class, add it to the
     list.  */
  if (TYPE_VFIELD (t) && CLASSTYPE_VFIELD_PARENT (t) == -1)
    CLASSTYPE_VFIELDS (t) 
      = chainon (CLASSTYPE_VFIELDS (t), build_tree_list (NULL_TREE, t));

  finish_struct_bits (t);

  /* Complete the rtl for any static member objects of the type we're
     working on.  */
  for (x = TYPE_FIELDS (t); x; x = TREE_CHAIN (x))
    {
      if (TREE_CODE (x) == VAR_DECL && TREE_STATIC (x)
	  && TREE_TYPE (x) == t)
	{
	  DECL_MODE (x) = TYPE_MODE (t);
	  make_decl_rtl (x, NULL, 0);
	}
    }

  /* Done with FIELDS...now decide whether to sort these for
     faster lookups later.

     The C front-end only does this when n_fields > 15.  We use
     a smaller number because most searches fail (succeeding
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

      while (vfields)
	{
	  /* Mark the fact that constructor for T
	     could affect anybody inheriting from T
	     who wants to initialize vtables for VFIELDS's type.  */
	  if (VF_DERIVED_VALUE (vfields))
	    TREE_ADDRESSABLE (vfields) = 1;
	  vfields = TREE_CHAIN (vfields);
	}
    }

  /* Make the rtl for any new vtables we have created, and unmark
     the base types we marked.  */
  finish_vtbls (t);

  if (CLASSTYPE_VSIZE (t) != 0)
    {
      /* In addition to this one, all the other vfields should be listed.  */
      /* Before that can be done, we have to have FIELD_DECLs for them, and
	 a place to find them.  */
      TYPE_NONCOPIED_PARTS (t) 
	= tree_cons (default_conversion (TYPE_BINFO_VTABLE (t)),
		     TYPE_VFIELD (t), TYPE_NONCOPIED_PARTS (t));

      if (warn_nonvdtor && TYPE_HAS_DESTRUCTOR (t)
	  && DECL_VINDEX (TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (t), 1)) == NULL_TREE)
	cp_warning ("`%#T' has virtual functions but non-virtual destructor",
		    t);
    }

  hack_incomplete_structures (t);

  if (warn_overloaded_virtual)
    warn_hidden (t);

  maybe_suppress_debug_info (t);

  /* Finish debugging output for this type.  */
  rest_of_type_compilation (t, toplevel_bindings_p ());
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

  /* The TYPE_FIELDS, TYPE_METHODS, and CLASSTYPE_TAGS are all in
     reverse order.  Put them in declaration order now.  */
  TYPE_METHODS (t) = nreverse (TYPE_METHODS (t));
  CLASSTYPE_TAGS (t) = nreverse (CLASSTYPE_TAGS (t));

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
  /* Now that we've got all the field declarations, reverse everything
     as necessary.  */
  unreverse_member_declarations (t);

  cplus_decl_attributes (t, attributes, NULL_TREE);

  if (processing_template_decl)
    {
      finish_struct_methods (t);
      TYPE_SIZE (t) = integer_zero_node;
    }
  else
    finish_struct_1 (t);

  TYPE_BEING_DEFINED (t) = 0;

  if (current_class_type)
    popclass ();
  else
    error ("trying to finish struct, but kicked out due to previous parse errors.");

  if (processing_template_decl)
    {
      tree scope = current_scope ();
      if (scope && TREE_CODE (scope) == FUNCTION_DECL)
	add_tree (build_min (TAG_DEFN, t));
    }

  return t;
}

/* Return the dynamic type of INSTANCE, if known.
   Used to determine whether the virtual function table is needed
   or not.

   *NONNULL is set iff INSTANCE can be known to be nonnull, regardless
   of our knowledge of its type.  *NONNULL should be initialized
   before this function is called.  */

static tree
fixed_type_or_null (instance, nonnull)
     tree instance;
     int *nonnull;
{
  switch (TREE_CODE (instance))
    {
    case INDIRECT_REF:
      /* Check that we are not going through a cast of some sort.  */
      if (TREE_TYPE (instance)
	  == TREE_TYPE (TREE_TYPE (TREE_OPERAND (instance, 0))))
	instance = TREE_OPERAND (instance, 0);
      /* fall through...  */
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
      return fixed_type_or_null (TREE_OPERAND (instance, 0), nonnull);

    case RTL_EXPR:
      return NULL_TREE;

    case PLUS_EXPR:
    case MINUS_EXPR:
      if (TREE_CODE (TREE_OPERAND (instance, 1)) == INTEGER_CST)
	/* Propagate nonnull.  */
	fixed_type_or_null (TREE_OPERAND (instance, 0), nonnull);
      if (TREE_CODE (TREE_OPERAND (instance, 0)) == ADDR_EXPR)
	return fixed_type_or_null (TREE_OPERAND (instance, 0), nonnull);
      return NULL_TREE;

    case NOP_EXPR:
    case CONVERT_EXPR:
      return fixed_type_or_null (TREE_OPERAND (instance, 0), nonnull);

    case ADDR_EXPR:
      if (nonnull)
	*nonnull = 1;
      return fixed_type_or_null (TREE_OPERAND (instance, 0), nonnull);

    case COMPONENT_REF:
      return fixed_type_or_null (TREE_OPERAND (instance, 1), nonnull);

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
      if (IS_AGGR_TYPE (TREE_TYPE (instance)))
	{
	  if (nonnull)
	    *nonnull = 1;
	  return TREE_TYPE (instance);
	}
      else if (nonnull)
	{
	  if (instance == current_class_ptr
	      && flag_this_is_variable <= 0)
	    {
	      /* Normally, 'this' must be non-null.  */
	      if (flag_this_is_variable == 0)
		*nonnull = 1;

	      /* <0 means we're in a constructor and we know our type.  */
	      if (flag_this_is_variable < 0)
		return TREE_TYPE (TREE_TYPE (instance));
	    }
	  else if (TREE_CODE (TREE_TYPE (instance)) == REFERENCE_TYPE)
	    /* Reference variables should be references to objects.  */
	    *nonnull = 1;
	}
      return NULL_TREE;

    default:
      return NULL_TREE;
    }
}

/* Return non-zero if the dynamic type of INSTANCE is known, and equivalent
   to the static type.  We also handle the case where INSTANCE is really
   a pointer.

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
  tree fixed = fixed_type_or_null (instance, nonnull);
  if (fixed == NULL_TREE)
    return 0;
  if (POINTER_TYPE_P (t))
    t = TREE_TYPE (t);
  return same_type_p (TYPE_MAIN_VARIANT (t), TYPE_MAIN_VARIANT (fixed));
}


void
init_class_processing ()
{
  current_class_depth = 0;
  current_class_stack_size = 10;
  current_class_stack 
    = (class_stack_node_t) xmalloc (current_class_stack_size 
				    * sizeof (struct class_stack_node));

  access_default_node = build_int_2 (0, 0);
  access_public_node = build_int_2 (1, 0);
  access_protected_node = build_int_2 (2, 0);
  access_private_node = build_int_2 (3, 0);
  access_default_virtual_node = build_int_2 (4, 0);
  access_public_virtual_node = build_int_2 (5, 0);
  access_protected_virtual_node = build_int_2 (6, 0);
  access_private_virtual_node = build_int_2 (7, 0);
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

#if 0
  if (CLASSTYPE_TEMPLATE_INFO (type))
    overload_template_name (type);
#endif

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

      storetags (CLASSTYPE_TAGS (type));
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
  
  /* This code can be seen as a cache miss.  When we've cached a
     class' scope's bindings and we can't use them, we need to reset
     them.  This is it!  */
  for (t = previous_class_values; t; t = TREE_CHAIN (t))
    IDENTIFIER_CLASS_VALUE (TREE_PURPOSE (t)) = NULL_TREE;
  
  previous_class_type = NULL_TREE;
}
 
/* Get out of the current class scope. If we were in a class scope
   previously, that is the one popped to.  */

void
popclass ()
{
  poplevel_class ();
  /* Since poplevel_class does the popping of class decls nowadays,
     this really only frees the obstack used for these decls.  */
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
      || TREE_CODE (type) == TEMPLATE_TEMPLATE_PARM)
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

/* Set global variables CURRENT_LANG_NAME to appropriate value
   so that behavior of name-mangling machinery is correct.  */

void
push_lang_context (name)
     tree name;
{
  *current_lang_stack++ = current_lang_name;
  if (current_lang_stack - &VARRAY_TREE (current_lang_base, 0)
      >= (ptrdiff_t) VARRAY_SIZE (current_lang_base))
    {
      size_t old_size = VARRAY_SIZE (current_lang_base);

      VARRAY_GROW (current_lang_base, old_size + 10);
      current_lang_stack = &VARRAY_TREE (current_lang_base, old_size);
    }

  if (name == lang_name_cplusplus)
    {
      strict_prototype = strict_prototypes_lang_cplusplus;
      current_lang_name = name;
    }
  else if (name == lang_name_java)
    {
      strict_prototype = strict_prototypes_lang_cplusplus;
      current_lang_name = name;
      /* DECL_IGNORED_P is initially set for these types, to avoid clutter.
	 (See record_builtin_java_type in decl.c.)  However, that causes
	 incorrect debug entries if these types are actually used.
	 So we re-enable debug output after extern "Java". */
      DECL_IGNORED_P (java_byte_type_node) = 0;
      DECL_IGNORED_P (java_short_type_node) = 0;
      DECL_IGNORED_P (java_int_type_node) = 0;
      DECL_IGNORED_P (java_long_type_node) = 0;
      DECL_IGNORED_P (java_float_type_node) = 0;
      DECL_IGNORED_P (java_double_type_node) = 0;
      DECL_IGNORED_P (java_char_type_node) = 0;
      DECL_IGNORED_P (java_boolean_type_node) = 0;
    }
  else if (name == lang_name_c)
    {
      strict_prototype = strict_prototypes_lang_c;
      current_lang_name = name;
    }
  else
    error ("language string `\"%s\"' not recognized", IDENTIFIER_POINTER (name));
}
  
/* Get out of the current language scope.  */

void
pop_lang_context ()
{
  /* Clear the current entry so that garbage collector won't hold on
     to it.  */
  *current_lang_stack = NULL_TREE;
  current_lang_name = *--current_lang_stack;
  if (current_lang_name == lang_name_cplusplus
      || current_lang_name == lang_name_java)
    strict_prototype = strict_prototypes_lang_cplusplus;
  else if (current_lang_name == lang_name_c)
    strict_prototype = strict_prototypes_lang_c;
}

/* Type instantiation routines.  */

/* Given an OVERLOAD and a TARGET_TYPE, return the function that
   matches the TARGET_TYPE.  If there is no satisfactory match, return
   error_mark_node, and issue an error message if COMPLAIN is
   non-zero.  If TEMPLATE_ONLY, the name of the overloaded function
   was a template-id, and EXPLICIT_TARGS are the explicitly provided
   template arguments.  */

static tree
resolve_address_of_overloaded_function (target_type, 
					overload,
					complain, 
					template_only,
					explicit_targs)
     tree target_type;
     tree overload;
     int complain;
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

  /* Check that the TARGET_TYPE is reasonable.  */
  if (TYPE_PTRFN_P (target_type))
    /* This is OK.  */
    ;
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
      if (complain)
	cp_error("cannot resolve overloaded function `%D' based on conversion to type `%T'", 
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
				   DEDUCE_EXACT) != 0)
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
	  tree match = most_specialized_instantiation (matches, 
						       explicit_targs);

	  if (match != error_mark_node)
	    matches = tree_cons (match, NULL_TREE, NULL_TREE);
	}
    }

  /* Now we should have exactly one function in MATCHES.  */
  if (matches == NULL_TREE)
    {
      /* There were *no* matches.  */
      if (complain)
	{
 	  cp_error ("no matches converting function `%D' to type `%#T'", 
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

      if (complain)
	{
	  tree match;

 	  cp_error ("converting overloaded function `%D' to type `%#T' is ambiguous", 
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

  mark_used (fn);

  if (TYPE_PTRFN_P (target_type) || TYPE_PTRMEMFUNC_P (target_type))
    return build_unary_op (ADDR_EXPR, fn, 0);
  else
    {
      /* The target must be a REFERENCE_TYPE.  Above, build_unary_op
	 will mark the function as addressed, but here we must do it
	 explicitly.  */
      mark_addressable (fn);

      return fn;
    }
}

/* This function will instantiate the type of the expression given in
   RHS to match the type of LHSTYPE.  If errors exist, then return
   error_mark_node.  We only complain is COMPLAIN is set.  If we are
   not complaining, never modify rhs, as overload resolution wants to
   try many possible instantiations, in hopes that at least one will
   work.

   FLAGS is a bitmask, as we see at the top of the function.

   For non-recursive calls, LHSTYPE should be a function, pointer to
   function, or a pointer to member function.  */

tree
instantiate_type (lhstype, rhs, flags)
     tree lhstype, rhs;
     int flags;
{
  int complain = (flags & 1);
  int strict = (flags & 2) ? COMPARE_NO_ATTRIBUTES : COMPARE_STRICT;

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
	cp_error ("argument of type `%T' does not match `%T'",
		  TREE_TYPE (rhs), lhstype);
      return error_mark_node;
    }

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
      my_friendly_abort (177);
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
      {
	tree r = instantiate_type (lhstype, TREE_OPERAND (rhs, 1), flags);

	if (r != error_mark_node && TYPE_PTRMEMFUNC_P (lhstype)
	    && complain && !flag_ms_extensions)
	  {
	    /* Note: we check this after the recursive call to avoid
	       complaining about cases where overload resolution fails.  */

	    tree t = TREE_TYPE (TREE_OPERAND (rhs, 0));
	    tree fn = PTRMEM_CST_MEMBER (r);

	    my_friendly_assert (TREE_CODE (r) == PTRMEM_CST, 990811);

	    cp_pedwarn
	      ("object-dependent reference to `%E' can only be used in a call",
	       DECL_NAME (fn));
	    cp_pedwarn
	      ("  to form a pointer to member function, say `&%T::%E'",
	       t, DECL_NAME (fn));
	  }

	return r;
      }

    case OFFSET_REF:
      rhs = TREE_OPERAND (rhs, 1);
      if (BASELINK_P (rhs))
	return instantiate_type (lhstype, TREE_VALUE (rhs), flags);

      /* This can happen if we are forming a pointer-to-member for a
	 member template.  */
      my_friendly_assert (TREE_CODE (rhs) == TEMPLATE_ID_EXPR, 0);

      /* Fall through.  */

    case TEMPLATE_ID_EXPR:
      return 
	resolve_address_of_overloaded_function (lhstype,
						TREE_OPERAND (rhs, 0),
						complain,
						/*template_only=*/1,
						TREE_OPERAND (rhs, 1));

    case OVERLOAD:
      return 
	resolve_address_of_overloaded_function (lhstype, 
						rhs,
						complain,
						/*template_only=*/0,
						/*explicit_targs=*/NULL_TREE);

    case TREE_LIST:
      /* Now we should have a baselink. */
      my_friendly_assert (BASELINK_P (rhs), 990412);

      return instantiate_type (lhstype, TREE_VALUE (rhs), flags);

    case CALL_EXPR:
      /* This is too hard for now.  */
      my_friendly_abort (183);
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
      return instantiate_type (lhstype, TREE_OPERAND (rhs, 0), flags);

    case ENTRY_VALUE_EXPR:
      my_friendly_abort (184);
      return error_mark_node;

    case ERROR_MARK:
      return error_mark_node;

    default:
      my_friendly_abort (185);
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
  sprintf (buf, VFIELD_NAME_FORMAT, TYPE_NAME_STRING (type));
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
  tree t;

  if (type == error_mark_node)
    return 0;

  if (! IS_AGGR_TYPE (type))
    return 0;

  if (flag_new_abi)
    return integer_zerop (CLASSTYPE_SIZE (type));

  if (TYPE_BINFO_BASETYPES (type))
    return 0;
  t = TYPE_FIELDS (type);
  while (t && TREE_CODE (t) != FIELD_DECL)
    t = TREE_CHAIN (t);
  return (t == NULL_TREE);
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
	  my_friendly_abort (0);
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
      if (get_binfo (base, type, 0))
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
  if (!current_class_type || !TYPE_BEING_DEFINED (current_class_type))
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
   to see that the declaration is legal.  */

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
      cp_error ("declaration of `%#D'", decl);
      cp_error_at ("changes meaning of `%s' from `%+#D'", 
		   IDENTIFIER_POINTER (DECL_NAME (OVL_CURRENT (decl))),
		   (tree) n->value);
    }
}

/* Dump the offsets of all the bases rooted at BINFO to stderr.
   INDENT should be zero when called from the top level; it is
   incremented recursively.  */

void
dump_class_hierarchy (binfo, indent)
     tree binfo;
     int indent;
{
  int i;

  fprintf (stderr, "%*s0x%lx (%s) ", indent, "",
	   (unsigned long) binfo,
	   type_as_string (binfo, TS_PLAIN));
  fprintf (stderr, HOST_WIDE_INT_PRINT_DEC,
	   tree_low_cst (BINFO_OFFSET (binfo), 0));
  fprintf (stderr, " %s\n",
	   BINFO_PRIMARY_MARKED_P (binfo) ? "primary" : "");

  for (i = 0; i < BINFO_N_BASETYPES (binfo); ++i)
    dump_class_hierarchy (BINFO_BASETYPE (binfo, i), indent + 2);
}
