/* Functions related to building classes and their related objects.
   Copyright (C) 1987, 92, 93, 94, 95, 96, 1997 Free Software Foundation, Inc.
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
#include "tree.h"
#include <stdio.h>
#include "cp-tree.h"
#include "flags.h"
#include "rtl.h"
#include "output.h"

#include "obstack.h"
#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

extern struct obstack permanent_obstack;

/* This is how we tell when two virtual member functions are really the
   same.  */
#define SAME_FN(FN1DECL, FN2DECL) (DECL_ASSEMBLER_NAME (FN1DECL) == DECL_ASSEMBLER_NAME (FN2DECL))

extern void set_class_shadows PROTO ((tree));

/* Way of stacking class types.  */
static tree *current_class_base, *current_class_stack;
static int current_class_stacksize;
int current_class_depth;

struct class_level
{
  /* The previous class level.  */
  struct class_level *level_chain;

  /* The class instance variable, as a PARM_DECL.  */
  tree decl;
  /* The class instance variable, as an object.  */
  tree object;
  /* The virtual function table pointer
     for the class instance variable.  */
  tree vtable_decl;

  /* Name of the current class.  */
  tree name;
  /* Type of the current class.  */
  tree type;

  /* Flags for this class level.  */
  int this_is_variable;
  int memoized_lookups;
  int save_memoized;
  int unused;
};

/* The current_class_ptr is the pointer to the current class.
   current_class_ref is the actual current class.  */
tree current_class_ptr, current_class_ref;

/* The following two can be derived from the previous one */
tree current_class_name;	/* IDENTIFIER_NODE: name of current class */
tree current_class_type;	/* _TYPE: the type of the current class */
tree previous_class_type;	/* _TYPE: the previous type that was a class */
tree previous_class_values;		/* TREE_LIST: copy of the class_shadowed list
				   when leaving an outermost class scope.  */

struct base_info;

static tree get_vfield_name PROTO((tree));
static void finish_struct_anon PROTO((tree));
static tree build_vbase_pointer PROTO((tree, tree));
static int complete_type_p PROTO((tree));
static int typecode_p PROTO((tree, enum tree_code));
static tree build_vtable_entry PROTO((tree, tree));
static tree get_vtable_name PROTO((tree));
static tree get_derived_offset PROTO((tree, tree));
static tree get_basefndecls PROTO((tree, tree));
static void set_rtti_entry PROTO((tree, tree, tree));
static tree build_vtable PROTO((tree, tree));
static void prepare_fresh_vtable PROTO((tree, tree));
static void fixup_vtable_deltas1 PROTO((tree, tree));
static void fixup_vtable_deltas PROTO((tree, int, tree));
static void grow_method PROTO((tree, tree *));
static void finish_vtbls PROTO((tree, int, tree));
static void modify_vtable_entry PROTO((tree, tree, tree));
static tree get_vtable_entry_n PROTO((tree, unsigned HOST_WIDE_INT));
static void add_virtual_function PROTO((tree *, tree *, int *, tree, tree));
static tree delete_duplicate_fields_1 PROTO((tree, tree));
static void delete_duplicate_fields PROTO((tree));
static void finish_struct_bits PROTO((tree, int));
static int alter_access PROTO((tree, tree, tree));
static int overrides PROTO((tree, tree));
static int strictly_overrides PROTO((tree, tree));
static void merge_overrides PROTO((tree, tree, int, tree));
static void override_one_vtable PROTO((tree, tree, tree));
static void mark_overriders PROTO((tree, tree));
static void check_for_override PROTO((tree, tree));
static tree maybe_fixup_vptrs PROTO((tree, tree, tree));
static tree get_class_offset_1 PROTO((tree, tree, tree, tree, tree));
static tree get_class_offset PROTO((tree, tree, tree, tree));
static void modify_one_vtable PROTO((tree, tree, tree, tree));
static void modify_all_vtables PROTO((tree, tree, tree));
static void modify_all_direct_vtables PROTO((tree, int, tree, tree,
					     tree));
static void modify_all_indirect_vtables PROTO((tree, int, int, tree,
					       tree, tree));
static void build_class_init_list PROTO((tree));
static int finish_base_struct PROTO((tree, struct base_info *, tree));

/* Way of stacking language names.  */
tree *current_lang_base, *current_lang_stack;
int current_lang_stacksize;

/* Names of languages we recognize.  */
tree lang_name_c, lang_name_cplusplus;
tree current_lang_name;

/* When layout out an aggregate type, the size of the
   basetypes (virtual and non-virtual) is passed to layout_record
   via this node.  */
static tree base_layout_decl;

/* Constants used for access control.  */
tree access_default_node; /* 0 */
tree access_public_node; /* 1 */
tree access_protected_node; /* 2 */
tree access_private_node; /* 3 */
tree access_default_virtual_node; /* 4 */
tree access_public_virtual_node; /* 5 */
tree access_private_virtual_node; /* 6 */

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

/* Virtual baseclass things.  */

static tree
build_vbase_pointer (exp, type)
     tree exp, type;
{
  char *name;

  name = (char *) alloca (TYPE_NAME_LENGTH (type) + sizeof (VBASE_NAME) + 1);
  sprintf (name, VBASE_NAME_FORMAT, TYPE_NAME_STRING (type));
  return build_component_ref (exp, get_identifier (name), NULL_TREE, 0);
}

/* Is the type of the EXPR, the complete type of the object?
   If we are going to be wrong, we must be conservative, and return 0.  */

static int
complete_type_p (expr)
     tree expr;
{
  tree type = TYPE_MAIN_VARIANT (TREE_TYPE (expr));
  while (1)
    {
      switch (TREE_CODE (expr))
	{
	case SAVE_EXPR:
	case INDIRECT_REF:
	case ADDR_EXPR:
	case NOP_EXPR:
	case CONVERT_EXPR:
	  expr = TREE_OPERAND (expr, 0);
	  continue;

	case CALL_EXPR: 
	  if (! TREE_HAS_CONSTRUCTOR (expr))
	    break;
	  /* fall through...  */
	case VAR_DECL:
	case FIELD_DECL:
	  if (TREE_CODE (TREE_TYPE (expr)) == ARRAY_TYPE
	      && IS_AGGR_TYPE (TREE_TYPE (TREE_TYPE (expr)))
	      && TYPE_MAIN_VARIANT (TREE_TYPE (expr)) == type)
	    return 1;
	  /* fall through...  */
	case TARGET_EXPR:
	case PARM_DECL:
	  if (IS_AGGR_TYPE (TREE_TYPE (expr))
	      && TYPE_MAIN_VARIANT (TREE_TYPE (expr)) == type)
	    return 1;
	  /* fall through...  */
	case PLUS_EXPR:
	default:
	  break;
	}
      break;
    }
  return 0;
}

/* Build multi-level access to EXPR using hierarchy path PATH.
   CODE is PLUS_EXPR if we are going with the grain,
   and MINUS_EXPR if we are not (in which case, we cannot traverse
   virtual baseclass links).

   TYPE is the type we want this path to have on exit.

   ALIAS_THIS is non-zero if EXPR in an expression involving `this'.  */

tree
build_vbase_path (code, type, expr, path, alias_this)
     enum tree_code code;
     tree type, expr, path;
     int alias_this;
{
  register int changed = 0;
  tree last = NULL_TREE, last_virtual = NULL_TREE;
  int nonnull = 0;
  int fixed_type_p;
  tree null_expr = 0, nonnull_expr;
  tree basetype;
  tree offset = integer_zero_node;

  if (BINFO_INHERITANCE_CHAIN (path) == NULL_TREE)
    return build1 (NOP_EXPR, type, expr);

  if (nonnull == 0 && (alias_this && flag_this_is_variable <= 0))
    nonnull = 1;

#if 0
  /* We need additional logic to convert back to the unconverted type
     (the static type of the complete object), and then convert back
     to the type we want.  Until that is done, or until we can
     recognize when that is, we cannot do the short cut logic. (mrs) */
  fixed_type_p = resolves_to_fixed_type_p (expr, &nonnull);
#else
  /* Do this, until we can undo any previous conversions.  See net35.C
     for a testcase.  */
  fixed_type_p = complete_type_p (expr);
#endif

  if (!fixed_type_p && TREE_SIDE_EFFECTS (expr))
    expr = save_expr (expr);
  nonnull_expr = expr;

  if (BINFO_INHERITANCE_CHAIN (path))
    {
      tree reverse_path = NULL_TREE;

      push_expression_obstack ();
      while (path)
	{
	  tree r = copy_node (path);
	  BINFO_INHERITANCE_CHAIN (r) = reverse_path;
	  reverse_path = r;
	  path = BINFO_INHERITANCE_CHAIN (path);
	}
      path = reverse_path;
      pop_obstacks ();
    }

  basetype = BINFO_TYPE (path);

  while (path)
    {
      if (TREE_VIA_VIRTUAL (path))
	{
	  last_virtual = BINFO_TYPE (path);
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
		      && (TREE_CODE (type) == POINTER_TYPE
			  || !flag_assume_nonnull_objects)
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
      last = path;
      path = BINFO_INHERITANCE_CHAIN (path);
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
	{
	  tree binfo = get_binfo (last, TYPE_MAIN_VARIANT (intype), 0);
	  offset = BINFO_OFFSET (binfo);
	}
    }
  else
    {
      if (last_virtual)
	{
	  offset = BINFO_OFFSET (binfo_member (last_virtual,
					       CLASSTYPE_VBASECLASSES (basetype)));
	  offset = size_binop (PLUS_EXPR, offset, BINFO_OFFSET (last));
	}
      else
	offset = BINFO_OFFSET (last);
    }

  if (TREE_INT_CST_LOW (offset))
    {
      /* Bash types to make the backend happy.  */
      offset = cp_convert (type, offset);
#if 0
      /* This shouldn't be necessary.  (mrs) */
      expr = build1 (NOP_EXPR, type, expr);
#endif

      /* For multiple inheritance: if `this' can be set by any
	 function, then it could be 0 on entry to any function.
	 Preserve such zeroness here.  Otherwise, only in the
	 case of constructors need we worry, and in those cases,
	 it will be zero, or initialized to some valid value to
	 which we may add.  */
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

/* Build an entry in the virtual function table.
   DELTA is the offset for the `this' pointer.
   PFN is an ADDR_EXPR containing a pointer to the virtual function.
   Note that the index (DELTA2) in the virtual function table
   is always 0.  */

static tree
build_vtable_entry (delta, pfn)
     tree delta, pfn;
{
  if (flag_vtable_thunks)
    {
      HOST_WIDE_INT idelta = TREE_INT_CST_LOW (delta);
      if (idelta && ! DECL_ABSTRACT_VIRTUAL_P (TREE_OPERAND (pfn, 0)))
	{
	  pfn = build1 (ADDR_EXPR, vtable_entry_type,
			make_thunk (pfn, idelta));
	  TREE_READONLY (pfn) = 1;
	  TREE_CONSTANT (pfn) = 1;
	}
#ifdef GATHER_STATISTICS
      n_vtable_entries += 1;
#endif
      return pfn;
    }
  else
    {
      extern int flag_huge_objects;
      tree elems = expr_tree_cons (NULL_TREE, delta,
			      expr_tree_cons (NULL_TREE, integer_zero_node,
					 build_expr_list (NULL_TREE, pfn)));
      tree entry = build (CONSTRUCTOR, vtable_entry_type, NULL_TREE, elems);

      /* DELTA is constructed by `size_int', which means it may be an
	 unsigned quantity on some platforms.  Therefore, we cannot use
	 `int_fits_type_p', because when DELTA is really negative,
	 `force_fit_type' will make it look like a very large number.  */

      if ((TREE_INT_CST_LOW (TYPE_MAX_VALUE (delta_type_node))
	   < TREE_INT_CST_LOW (delta))
	  || (TREE_INT_CST_LOW (delta)
	      < TREE_INT_CST_LOW (TYPE_MIN_VALUE (delta_type_node))))
	if (flag_huge_objects)
	  sorry ("object size exceeds built-in limit for virtual function table implementation");
	else
	  sorry ("object size exceeds normal limit for virtual function table implementation, recompile all source and use -fhuge-objects");

      TREE_CONSTANT (entry) = 1;
      TREE_STATIC (entry) = 1;
      TREE_READONLY (entry) = 1;

#ifdef GATHER_STATISTICS
      n_vtable_entries += 1;
#endif

      return entry;
    }
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
    vtbl = build_indirect_ref (build_vfield_ref (instance, basetype),
			       NULL_PTR);
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
	vtbl = build_indirect_ref (build_vfield_ref (instance, basetype),
				   NULL_PTR);
    }
  assemble_external (vtbl);
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
  char *buf = (char *) alloca (strlen (VTABLE_NAME_FORMAT)
			       + IDENTIFIER_LENGTH (type_id) + 2);
  char *ptr = IDENTIFIER_POINTER (type_id);
  int i;
  for (i = 0; ptr[i] == OPERATOR_TYPENAME_FORMAT[i]; i++) ;
#if 0
  /* We don't take off the numbers; prepare_fresh_vtable uses the
     DECL_ASSEMBLER_NAME for the type, which includes the number
     in `3foo'.  If we were to pull them off here, we'd end up with
     something like `_vt.foo.3bar', instead of a uniform definition.  */
  while (ptr[i] >= '0' && ptr[i] <= '9')
    i += 1;
#endif
  sprintf (buf, VTABLE_NAME_FORMAT, ptr+i);
  return get_identifier (buf);
}

/* Return the offset to the main vtable for a given base BINFO.  */

tree
get_vfield_offset (binfo)
     tree binfo;
{
  return size_binop (PLUS_EXPR,
		     size_binop (FLOOR_DIV_EXPR,
				 DECL_FIELD_BITPOS (CLASSTYPE_VFIELD (BINFO_TYPE (binfo))),
				 size_int (BITS_PER_UNIT)),
		     BINFO_OFFSET (binfo));
}

/* Get the offset to the start of the original binfo that we derived
   this binfo from.  If we find TYPE first, return the offset only
   that far.  The shortened search is useful because the this pointer
   on method calling is expected to point to a DECL_CONTEXT (fndecl)
   object, and not a baseclass of it.  */

static tree
get_derived_offset (binfo, type)
     tree binfo, type;
{
  tree offset1 = get_vfield_offset (TYPE_BINFO (BINFO_TYPE (binfo)));
  tree offset2;
  int i;
  while (BINFO_BASETYPES (binfo)
	 && (i=CLASSTYPE_VFIELD_PARENT (BINFO_TYPE (binfo))) != -1)
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
  tree vfn;

  if (flag_rtti)
    vfn = build1 (ADDR_EXPR, vfunc_ptr_type_node, get_tinfo_fn (type));
  else
    vfn = build1 (NOP_EXPR, vfunc_ptr_type_node, size_zero_node);
  TREE_CONSTANT (vfn) = 1;

  if (! flag_vtable_thunks)
    TREE_VALUE (virtuals) = build_vtable_entry (offset, vfn);
  else
    {
      tree voff = build1 (NOP_EXPR, vfunc_ptr_type_node, offset);
      TREE_CONSTANT (voff) = 1;

      TREE_VALUE (virtuals) = build_vtable_entry (size_zero_node, voff);

      /* The second slot is for the tdesc pointer when thunks are used.  */
      TREE_VALUE (TREE_CHAIN (virtuals))
	= build_vtable_entry (size_zero_node, vfn);
    }
}

/* Build a virtual function for type TYPE.
   If BINFO is non-NULL, build the vtable starting with the initial
   approximation that it is the same as the one which is the head of
   the association list.  */

static tree
build_vtable (binfo, type)
     tree binfo, type;
{
  tree name = get_vtable_name (type);
  tree virtuals, decl;

  if (binfo)
    {
      tree offset;

      virtuals = copy_list (BINFO_VIRTUALS (binfo));
      decl = build_decl (VAR_DECL, name, TREE_TYPE (BINFO_VTABLE (binfo)));

      /* Now do rtti stuff.  */
      offset = get_derived_offset (TYPE_BINFO (type), NULL_TREE);
      offset = size_binop (MINUS_EXPR, size_zero_node, offset);
      set_rtti_entry (virtuals, offset, type);
    }
  else
    {
      virtuals = NULL_TREE;
      decl = build_decl (VAR_DECL, name, void_type_node);
    }

#ifdef GATHER_STATISTICS
  n_vtables += 1;
  n_vtable_elems += list_length (virtuals);
#endif

  /* Set TREE_PUBLIC and TREE_EXTERN as appropriate.  */
  import_export_vtable (decl, type, 0);

  IDENTIFIER_GLOBAL_VALUE (name) = decl = pushdecl_top_level (decl);
  /* Initialize the association list for this type, based
     on our first approximation.  */
  TYPE_BINFO_VTABLE (type) = decl;
  TYPE_BINFO_VIRTUALS (type) = virtuals;

  DECL_ARTIFICIAL (decl) = 1;
  TREE_STATIC (decl) = 1;
#ifndef WRITABLE_VTABLES
  /* Make them READONLY by default. (mrs) */
  TREE_READONLY (decl) = 1;
#endif
  /* At one time the vtable info was grabbed 2 words at a time.  This
     fails on sparc unless you have 8-byte alignment.  (tiemann) */
  DECL_ALIGN (decl) = MAX (TYPE_ALIGN (double_type_node),
			   DECL_ALIGN (decl));

  /* Why is this conditional? (mrs) */
  if (binfo && write_virtuals >= 0)
    DECL_VIRTUAL_P (decl) = 1;
  DECL_CONTEXT (decl) = type;

  binfo = TYPE_BINFO (type);
  SET_BINFO_NEW_VTABLE_MARKED (binfo);
  return decl;
}

extern tree signed_size_zero_node;

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

static void
prepare_fresh_vtable (binfo, for_type)
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
      char *buf1 = (char *) alloca (TYPE_ASSEMBLER_NAME_LENGTH (for_type) + 1 + i);
      char *new_buf2;

      sprintf (buf1, "%s%c%s", TYPE_ASSEMBLER_NAME_STRING (for_type), joiner,
	       buf2);
      buf = (char *) alloca (strlen (VTABLE_NAME_FORMAT) + strlen (buf1) + 1);
      sprintf (buf, VTABLE_NAME_FORMAT, buf1);
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

      /* We better not run out of stuff to make it unique.  */
      my_friendly_assert (for_type != basetype, 369);

      i = TYPE_ASSEMBLER_NAME_LENGTH (basetype) + 1 + i;
      new_buf2 = (char *) alloca (i);
      sprintf (new_buf2, "%s%c%s",
	       TYPE_ASSEMBLER_NAME_STRING (basetype), joiner, buf2);
      buf2 = new_buf2;
    }

  new_decl = build_decl (VAR_DECL, name, TREE_TYPE (orig_decl));
  /* Remember which class this vtable is really for.  */
  DECL_CONTEXT (new_decl) = for_type;

  DECL_ARTIFICIAL (new_decl) = 1;
  TREE_STATIC (new_decl) = 1;
  BINFO_VTABLE (binfo) = pushdecl_top_level (new_decl);
  DECL_VIRTUAL_P (new_decl) = 1;
#ifndef WRITABLE_VTABLES
  /* Make them READONLY by default. (mrs) */
  TREE_READONLY (new_decl) = 1;
#endif
  DECL_ALIGN (new_decl) = DECL_ALIGN (orig_decl);

  /* Make fresh virtual list, so we can smash it later.  */
  BINFO_VIRTUALS (binfo) = copy_list (BINFO_VIRTUALS (binfo));

  if (TREE_VIA_VIRTUAL (binfo))
    {
      tree binfo1 = binfo_member (BINFO_TYPE (binfo), 
				  CLASSTYPE_VBASECLASSES (for_type));

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
		  size_binop (MINUS_EXPR, signed_size_zero_node, offset),
		  for_type);

#ifdef GATHER_STATISTICS
  n_vtables += 1;
  n_vtable_elems += list_length (BINFO_VIRTUALS (binfo));
#endif

  /* Set TREE_PUBLIC and TREE_EXTERN as appropriate.  */
  import_export_vtable (new_decl, for_type, 0);

  if (TREE_VIA_VIRTUAL (binfo))
    my_friendly_assert (binfo == binfo_member (BINFO_TYPE (binfo),
				   CLASSTYPE_VBASECLASSES (current_class_type)),
			170);
  SET_BINFO_NEW_VTABLE_MARKED (binfo);
}

#if 0
/* Access the virtual function table entry that logically
   contains BASE_FNDECL.  VIRTUALS is the virtual function table's
   initializer.  We can run off the end, when dealing with virtual
   destructors in MI situations, return NULL_TREE in that case.  */

static tree
get_vtable_entry (virtuals, base_fndecl)
     tree virtuals, base_fndecl;
{
  unsigned HOST_WIDE_INT n = (HOST_BITS_PER_WIDE_INT >= BITS_PER_WORD
	   ? (TREE_INT_CST_LOW (DECL_VINDEX (base_fndecl))
	      & (((unsigned HOST_WIDE_INT)1<<(BITS_PER_WORD-1))-1))
	   : TREE_INT_CST_LOW (DECL_VINDEX (base_fndecl)));

#ifdef GATHER_STATISTICS
  n_vtable_searches += n;
#endif

  while (n > 0 && virtuals)
    {
      --n;
      virtuals = TREE_CHAIN (virtuals);
    }
  return virtuals;
}
#endif

/* Put new entry ENTRY into virtual function table initializer
   VIRTUALS.

   Also update DECL_VINDEX (FNDECL).  */

static void
modify_vtable_entry (old_entry_in_list, new_entry, fndecl)
     tree old_entry_in_list, new_entry, fndecl;
{
  tree base_fndecl = TREE_OPERAND (FNADDR_FROM_VTABLE_ENTRY (TREE_VALUE (old_entry_in_list)), 0);

#ifdef NOTQUITE
  cp_warning ("replaced %D with %D", DECL_ASSEMBLER_NAME (base_fndecl),
	      DECL_ASSEMBLER_NAME (fndecl));
#endif
  TREE_VALUE (old_entry_in_list) = new_entry;

  /* Now assign virtual dispatch information, if unset.  */
  /* We can dispatch this, through any overridden base function.  */
  if (TREE_CODE (DECL_VINDEX (fndecl)) != INTEGER_CST)
    {
      DECL_VINDEX (fndecl) = DECL_VINDEX (base_fndecl);
      DECL_CONTEXT (fndecl) = DECL_CONTEXT (base_fndecl);
    }
}

/* Access the virtual function table entry N.  VIRTUALS is the virtual
   function table's initializer.  */

static tree
get_vtable_entry_n (virtuals, n)
     tree virtuals;
     unsigned HOST_WIDE_INT n;
{
  while (n > 0)
    {
      --n;
      virtuals = TREE_CHAIN (virtuals);
    }
  return virtuals;
}

/* Add a virtual function to all the appropriate vtables for the class
   T.  DECL_VINDEX(X) should be error_mark_node, if we want to
   allocate a new slot in our table.  If it is error_mark_node, we
   know that no other function from another vtable is overridden by X.
   HAS_VIRTUAL keeps track of how many virtuals there are in our main
   vtable for the type, and we build upon the PENDING_VIRTUALS list
   and return it.  */

static void
add_virtual_function (pv, phv, has_virtual, fndecl, t)
     tree *pv, *phv;
     int *has_virtual;
     tree fndecl;
     tree t; /* Structure type.  */
{
  tree pending_virtuals = *pv;
  tree pending_hard_virtuals = *phv;

  /* FUNCTION_TYPEs and OFFSET_TYPEs no longer freely
     convert to void *.  Make such a conversion here.  */
  tree vfn = build1 (ADDR_EXPR, vfunc_ptr_type_node, fndecl);
  TREE_CONSTANT (vfn) = 1;

#ifndef DUMB_USER
  if (current_class_type == 0)
    cp_warning ("internal problem, current_class_type is zero when adding `%D', please report",
		fndecl);
  if (current_class_type && t != current_class_type)
    cp_warning ("internal problem, current_class_type differs when adding `%D', please report",
		fndecl);
#endif

  /* If the virtual function is a redefinition of a prior one,
     figure out in which base class the new definition goes,
     and if necessary, make a fresh virtual function table
     to hold that entry.  */
  if (DECL_VINDEX (fndecl) == error_mark_node)
    {
      tree entry;

      /* We remember that this was the base sub-object for rtti.  */
      CLASSTYPE_RTTI (t) = t;

      /* If we are using thunks, use two slots at the front, one
	 for the offset pointer, one for the tdesc pointer.  */
      if (*has_virtual == 0 && flag_vtable_thunks)
	{
	  *has_virtual = 1;
	}

      /* Build a new INT_CST for this DECL_VINDEX.  */
      {
	static tree index_table[256];
	tree idx;
	/* We skip a slot for the offset/tdesc entry.  */
	int i = ++(*has_virtual);

	if (i >= 256 || index_table[i] == 0)
	  {
	    idx = build_int_2 (i, 0);
	    if (i < 256)
	      index_table[i] = idx;
	  }
	else
	  idx = index_table[i];

	/* Now assign virtual dispatch information.  */
	DECL_VINDEX (fndecl) = idx;
	DECL_CONTEXT (fndecl) = t;
      }
      entry = build_vtable_entry (integer_zero_node, vfn);
      pending_virtuals = tree_cons (DECL_VINDEX (fndecl), entry, pending_virtuals);
    }
  /* Might already be INTEGER_CST if declared twice in class.  We will
     give error later or we've already given it.  */
  else if (TREE_CODE (DECL_VINDEX (fndecl)) != INTEGER_CST)
    {
      /* Need an entry in some other virtual function table.
         Deal with this after we have laid out our virtual base classes.  */
      pending_hard_virtuals = temp_tree_cons (fndecl, vfn, pending_hard_virtuals);
    }
  *pv = pending_virtuals;
  *phv = pending_hard_virtuals;
}

/* Obstack on which to build the vector of class methods.  */
struct obstack class_obstack;
extern struct obstack *current_obstack;

/* Add method METHOD to class TYPE.  This is used when a method
   has been defined which did not initially appear in the class definition,
   and helps cut down on spurious error messages.

   FIELDS is the entry in the METHOD_VEC vector entry of the class type where
   the method should be added.  */

void
add_method (type, fields, method)
     tree type, *fields, method;
{
  /* We must make a copy of METHOD here, since we must be sure that
     we have exclusive title to this method's DECL_CHAIN.  */
  tree decl;

  push_obstacks (&permanent_obstack, &permanent_obstack);
  {
    decl = copy_node (method);
    if (DECL_RTL (decl) == 0
        && (!processing_template_decl
	    || !uses_template_parms (decl)))
      {
	make_function_rtl (decl);
	DECL_RTL (method) = DECL_RTL (decl);
      }
  }

  if (fields && *fields)
    {
      /* Take care not to hide destructor.  */
      DECL_CHAIN (decl) = DECL_CHAIN (*fields);
      DECL_CHAIN (*fields) = decl;
    }
  else if (CLASSTYPE_METHOD_VEC (type) == 0)
    {
      tree method_vec = make_node (TREE_VEC);
      if (TYPE_IDENTIFIER (type) == DECL_NAME (decl))
	{
	  /* ??? Is it possible for there to have been enough room in the
	     current chunk for the tree_vec structure but not a tree_vec
	     plus a tree*?  Will this work in that case?  */
	  obstack_free (current_obstack, method_vec);
	  obstack_blank (current_obstack, sizeof (struct tree_vec) + sizeof (tree *));
	  if (DESTRUCTOR_NAME_P (DECL_ASSEMBLER_NAME (decl)))
	    TREE_VEC_ELT (method_vec, 1) = decl;
	  else
	    TREE_VEC_ELT (method_vec, 0) = decl;
	  TREE_VEC_LENGTH (method_vec) = 2;
	}
      else
	{
	  /* ??? Is it possible for there to have been enough room in the
	     current chunk for the tree_vec structure but not a tree_vec
	     plus a tree*?  Will this work in that case?  */
	  obstack_free (current_obstack, method_vec);
	  obstack_blank (current_obstack, sizeof (struct tree_vec) + 2*sizeof (tree *));
	  TREE_VEC_ELT (method_vec, 2) = decl;
	  TREE_VEC_LENGTH (method_vec) = 3;
	  obstack_finish (current_obstack);
	}
      CLASSTYPE_METHOD_VEC (type) = method_vec;
    }
  else
    {
      tree method_vec = CLASSTYPE_METHOD_VEC (type);
      int len = TREE_VEC_LENGTH (method_vec);

      /* Adding a new ctor or dtor.  This is easy because our
         METHOD_VEC always has a slot for such entries.  */
      if (TYPE_IDENTIFIER (type) == DECL_NAME (decl))
	{
	  int idx = !!DESTRUCTOR_NAME_P (DECL_ASSEMBLER_NAME (decl));
	  /* TREE_VEC_ELT (method_vec, idx) = decl; */
	  if (decl != TREE_VEC_ELT (method_vec, idx))
	    {
	      DECL_CHAIN (decl) = TREE_VEC_ELT (method_vec, idx);
	      TREE_VEC_ELT (method_vec, idx) = decl;
	    }
	}
      else
	{
	  /* This is trickier.  We try to extend the TREE_VEC in-place,
	     but if that does not work, we copy all its data to a new
	     TREE_VEC that's large enough.  */
	  struct obstack *ob = &class_obstack;
	  tree *end = (tree *)obstack_next_free (ob);

	  if (end != TREE_VEC_END (method_vec))
	    {
	      ob = current_obstack;
	      TREE_VEC_LENGTH (method_vec) += 1;
	      TREE_VEC_ELT (method_vec, len) = NULL_TREE;
	      method_vec = copy_node (method_vec);
	      TREE_VEC_LENGTH (method_vec) -= 1;
	    }
	  else
	    {
	      tree tmp_vec = (tree) obstack_base (ob);
	      if (obstack_room (ob) < sizeof (tree))
		{
		  obstack_blank (ob, sizeof (struct tree_common)
				 + tree_code_length[(int) TREE_VEC]
				   * sizeof (char *)
				 + len * sizeof (tree));
		  tmp_vec = (tree) obstack_base (ob);
		  bcopy ((char *) method_vec, (char *) tmp_vec,
			 (sizeof (struct tree_common)
			  + tree_code_length[(int) TREE_VEC] * sizeof (char *)
			  + (len-1) * sizeof (tree)));
		  method_vec = tmp_vec;
		}
	      else
		obstack_blank (ob, sizeof (tree));
	    }

	  obstack_finish (ob);
	  TREE_VEC_ELT (method_vec, len) = decl;
	  TREE_VEC_LENGTH (method_vec) = len + 1;
	  CLASSTYPE_METHOD_VEC (type) = method_vec;

	  if (TYPE_BINFO_BASETYPES (type) && CLASSTYPE_BASELINK_VEC (type))
	    {
	      /* ??? May be better to know whether these can be extended?  */
	      tree baselink_vec = CLASSTYPE_BASELINK_VEC (type);

	      TREE_VEC_LENGTH (baselink_vec) += 1;
	      CLASSTYPE_BASELINK_VEC (type) = copy_node (baselink_vec);
	      TREE_VEC_LENGTH (baselink_vec) -= 1;

	      TREE_VEC_ELT (CLASSTYPE_BASELINK_VEC (type), len) = 0;
	    }
	}
    }
  DECL_CONTEXT (decl) = type;
  DECL_CLASS_CONTEXT (decl) = type;

  pop_obstacks ();
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
      if (TREE_CODE (TREE_TYPE (field)) != UNION_TYPE)
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
	      if (TREE_CODE (TREE_TYPE (x)) != UNION_TYPE)
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
	  else
	    {
	      if (DECL_NAME (field) == DECL_NAME (x))
		{
		  if (TREE_CODE (field) == CONST_DECL
		      && TREE_CODE (x) == CONST_DECL)
		    cp_error_at ("duplicate enum value `%D'", x);
		  else if (TREE_CODE (field) == CONST_DECL
			   || TREE_CODE (x) == CONST_DECL)
		    cp_error_at ("duplicate field `%D' (as enum and non-enum)",
				x);
		  else if (TREE_CODE (field) == TYPE_DECL
			   && TREE_CODE (x) == TYPE_DECL)
		    {
		      if (TREE_TYPE (field) == TREE_TYPE (x))
			continue;
		      cp_error_at ("duplicate nested type `%D'", x);
		    }
		  else if (TREE_CODE (field) == TYPE_DECL
			   || TREE_CODE (x) == TYPE_DECL)
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

/* Change the access of FDECL to ACCESS in T.
   Return 1 if change was legit, otherwise return 0.  */

static int
alter_access (t, fdecl, access)
     tree t;
     tree fdecl;
     tree access;
{
  tree elem = purpose_member (t, DECL_ACCESS (fdecl));
  if (elem && TREE_VALUE (elem) != access)
    {
      if (TREE_CODE (TREE_TYPE (fdecl)) == FUNCTION_DECL)
	{
	  cp_error_at ("conflicting access specifications for method `%D', ignored", TREE_TYPE (fdecl));
	}
      else
	error ("conflicting access specifications for field `%s', ignored",
	       IDENTIFIER_POINTER (DECL_NAME (fdecl)));
    }
  else if (TREE_PRIVATE (fdecl))
    {
      if (access != access_private_node)
	cp_error_at ("cannot make private `%D' non-private", fdecl);
      goto alter;
    }
  else if (TREE_PROTECTED (fdecl))
    {
      if (access != access_protected_node)
	cp_error_at ("cannot make protected `%D' non-protected", fdecl);
      goto alter;
    }
  /* ARM 11.3: an access declaration may not be used to restrict access
     to a member that is accessible in the base class.  */
  else if (access != access_public_node)
    cp_error_at ("cannot reduce access of public member `%D'", fdecl);
  else if (elem == NULL_TREE)
    {
    alter:
      DECL_ACCESS (fdecl) = tree_cons (t, access, DECL_ACCESS (fdecl));
      return 1;
    }
  return 0;
}

/* If FOR_TYPE needs to reinitialize virtual function table pointers
   for TYPE's sub-objects, add such reinitializations to BASE_INIT_LIST.
   Returns BASE_INIT_LIST appropriately modified.  */

static tree
maybe_fixup_vptrs (for_type, binfo, base_init_list)
     tree for_type, binfo, base_init_list;
{
  /* Now reinitialize any slots that don't fall under our virtual
     function table pointer.  */
  tree vfields = CLASSTYPE_VFIELDS (BINFO_TYPE (binfo));
  while (vfields)
    {
      tree basetype = VF_NORMAL_VALUE (vfields)
	? TYPE_MAIN_VARIANT (VF_NORMAL_VALUE (vfields))
	  : VF_BASETYPE_VALUE (vfields);

      tree base_binfo = get_binfo (basetype, for_type, 0);
      /* Punt until this is implemented.  */
      if (1 /* BINFO_MODIFIED (base_binfo) */)
	{
	  tree base_offset = get_vfield_offset (base_binfo);
	  if (! tree_int_cst_equal (base_offset, get_vfield_offset (TYPE_BINFO (for_type)))
	      && ! tree_int_cst_equal (base_offset, get_vfield_offset (binfo)))
	    base_init_list = tree_cons (error_mark_node, base_binfo,
					base_init_list);
	}
      vfields = TREE_CHAIN (vfields);
    }
  return base_init_list;
}

/* If TYPE does not have a constructor, then the compiler must
   manually deal with all of the initialization this type requires.

   If a base initializer exists only to fill in the virtual function
   table pointer, then we mark that fact with the TREE_VIRTUAL bit.
   This way, we avoid multiple initializations of the same field by
   each virtual function table up the class hierarchy.

   Virtual base class pointers are not initialized here.  They are
   initialized only at the "top level" of object creation.  If we
   initialized them here, we would have to skip a lot of work.  */

static void
build_class_init_list (type)
     tree type;
{
  tree base_init_list = NULL_TREE;
  tree member_init_list = NULL_TREE;

  /* Since we build member_init_list and base_init_list using
     tree_cons, backwards fields the all through work.  */
  tree x;
  tree binfos = BINFO_BASETYPES (TYPE_BINFO (type));
  int i, n_baseclasses = binfos ? TREE_VEC_LENGTH (binfos) : 0;

  for (x = TYPE_FIELDS (type); x; x = TREE_CHAIN (x))
    {
      if (TREE_CODE (x) != FIELD_DECL)
	continue;

      if (TYPE_NEEDS_CONSTRUCTING (TREE_TYPE (x))
	  || DECL_INITIAL (x) != NULL_TREE)
	member_init_list = tree_cons (x, type, member_init_list);
    }
  member_init_list = nreverse (member_init_list);

  /* We will end up doing this last.  Need special marker
     to avoid infinite regress.  */
  if (TYPE_VIRTUAL_P (type))
    {
      base_init_list = build_tree_list (error_mark_node, TYPE_BINFO (type));
      if (CLASSTYPE_NEEDS_VIRTUAL_REINIT (type) == 0)
	TREE_VALUE (base_init_list) = NULL_TREE;
      TREE_ADDRESSABLE (base_init_list) = 1;
    }

  /* Each base class which needs to have initialization
     of some kind gets to make such requests known here.  */
  for (i = n_baseclasses-1; i >= 0; i--)
    {
      tree base_binfo = TREE_VEC_ELT (binfos, i);
      tree blist;

      /* Don't initialize virtual baseclasses this way.  */
      if (TREE_VIA_VIRTUAL (base_binfo))
	continue;

      if (TYPE_HAS_CONSTRUCTOR (BINFO_TYPE (base_binfo)))
	{
	  /* ...and the last shall come first...  */
	  base_init_list = maybe_fixup_vptrs (type, base_binfo, base_init_list);
	  base_init_list = tree_cons (NULL_TREE, base_binfo, base_init_list);
	  continue;
	}

      if ((blist = CLASSTYPE_BASE_INIT_LIST (BINFO_TYPE (base_binfo))) == NULL_TREE)
	/* Nothing to initialize.  */
	continue;

      /* ...ditto...  */
      base_init_list = maybe_fixup_vptrs (type, base_binfo, base_init_list);

      /* This is normally true for single inheritance.
	 The win is we can shrink the chain of initializations
	 to be done by only converting to the actual type
	 we are interested in.  */
      if (TREE_VALUE (blist)
	  && TREE_CODE (TREE_VALUE (blist)) == TREE_VEC
	  && tree_int_cst_equal (BINFO_OFFSET (base_binfo),
				 BINFO_OFFSET (TREE_VALUE (blist))))
	{
	  if (base_init_list)
	    {
	      /* Does it do more than just fill in a
		 virtual function table pointer?  */
	      if (! TREE_ADDRESSABLE (blist))
		base_init_list = build_tree_list (blist, base_init_list);
	      /* Can we get by just with the virtual function table
		 pointer that it fills in?  */
	      else if (TREE_ADDRESSABLE (base_init_list)
		       && TREE_VALUE (base_init_list) == 0)
		base_init_list = blist;
	      /* Maybe, but it is not obvious as the previous case.  */
	      else if (! CLASSTYPE_NEEDS_VIRTUAL_REINIT (type))
		{
		  tree last = tree_last (base_init_list);
		  while (TREE_VALUE (last)
			 && TREE_CODE (TREE_VALUE (last)) == TREE_LIST)
		    last = tree_last (TREE_VALUE (last));
		  if (TREE_VALUE (last) == 0)
		    base_init_list = build_tree_list (blist, base_init_list);
		}
	    }
	  else
	    base_init_list = blist;
	}
      else
	{
	  /* The function expand_aggr_init knows how to do the
	     initialization of `basetype' without getting
	     an explicit `blist'.  */
	  if (base_init_list)
	    base_init_list = tree_cons (NULL_TREE, base_binfo, base_init_list);
	  else
	    base_init_list = CLASSTYPE_BINFO_AS_LIST (BINFO_TYPE (base_binfo));
	}
    }

  if (base_init_list)
    if (member_init_list)
      CLASSTYPE_BASE_INIT_LIST (type) = build_tree_list (base_init_list, member_init_list);
    else
      CLASSTYPE_BASE_INIT_LIST (type) = base_init_list;
  else if (member_init_list)
    CLASSTYPE_BASE_INIT_LIST (type) = member_init_list;
}

struct base_info
{
  int has_virtual;
  int max_has_virtual;
  int n_ancestors;
  tree vfield;
  tree vfields;
  tree rtti;
  char cant_have_default_ctor;
  char cant_have_const_ctor;
  char no_const_asn_ref;
};

/* Record information about type T derived from its base classes.
   Store most of that information in T itself, and place the
   remaining information in the struct BASE_INFO.

   Propagate basetype offsets throughout the lattice.  Note that the
   lattice topped by T is really a pair: it's a DAG that gives the
   structure of the derivation hierarchy, and it's a list of the
   virtual baseclasses that appear anywhere in the DAG.  When a vbase
   type appears in the DAG, it's offset is 0, and it's children start
   their offsets from that point.  When a vbase type appears in the list,
   its offset is the offset it has in the hierarchy, and its children's
   offsets include that offset in theirs.

   Returns the index of the first base class to have virtual functions,
   or -1 if no such base class.

   Note that at this point TYPE_BINFO (t) != t_binfo.  */

static int
finish_base_struct (t, b, t_binfo)
     tree t;
     struct base_info *b;
     tree t_binfo;
{
  tree binfos = BINFO_BASETYPES (t_binfo);
  int i, n_baseclasses = binfos ? TREE_VEC_LENGTH (binfos) : 0;
  int first_vfn_base_index = -1;
  bzero ((char *) b, sizeof (struct base_info));

  for (i = 0; i < n_baseclasses; i++)
    {
      tree base_binfo = TREE_VEC_ELT (binfos, i);
      tree basetype = BINFO_TYPE (base_binfo);

      /* If the type of basetype is incomplete, then
	 we already complained about that fact
	 (and we should have fixed it up as well).  */
      if (TYPE_SIZE (basetype) == 0)
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
	}

      if (! TYPE_HAS_CONST_INIT_REF (basetype))
	b->cant_have_const_ctor = 1;

      if (TYPE_HAS_CONSTRUCTOR (basetype)
	  && ! TYPE_HAS_DEFAULT_CONSTRUCTOR (basetype))
	{
	  b->cant_have_default_ctor = 1;
	  if (! TYPE_HAS_CONSTRUCTOR (t))
	    {
	      cp_pedwarn ("base `%T' with only non-default constructor",
			  basetype);
	      cp_pedwarn ("in class without a constructor");
	    }
	}

      if (TYPE_HAS_ASSIGN_REF (basetype)
	  && !TYPE_HAS_CONST_ASSIGN_REF (basetype))
	b->no_const_asn_ref = 1;

      b->n_ancestors += CLASSTYPE_N_SUPERCLASSES (basetype);
      TYPE_NEEDS_CONSTRUCTING (t) |= TYPE_NEEDS_CONSTRUCTING (basetype);
      TYPE_NEEDS_DESTRUCTOR (t) |= TYPE_NEEDS_DESTRUCTOR (basetype);
      TYPE_HAS_COMPLEX_ASSIGN_REF (t) |= TYPE_HAS_COMPLEX_ASSIGN_REF (basetype);
      TYPE_HAS_COMPLEX_INIT_REF (t) |= TYPE_HAS_COMPLEX_INIT_REF (basetype);

      TYPE_OVERLOADS_CALL_EXPR (t) |= TYPE_OVERLOADS_CALL_EXPR (basetype);
      TYPE_OVERLOADS_ARRAY_REF (t) |= TYPE_OVERLOADS_ARRAY_REF (basetype);
      TYPE_OVERLOADS_ARROW (t) |= TYPE_OVERLOADS_ARROW (basetype);

      if (! TREE_VIA_VIRTUAL (base_binfo)
	  && BINFO_BASETYPES (base_binfo))
	{
	  tree base_binfos = BINFO_BASETYPES (base_binfo);
	  tree chain = NULL_TREE;
	  int j;

	  /* Now unshare the structure beneath BASE_BINFO.  */
	  for (j = TREE_VEC_LENGTH (base_binfos)-1;
	       j >= 0; j--)
	    {
	      tree base_base_binfo = TREE_VEC_ELT (base_binfos, j);
	      if (! TREE_VIA_VIRTUAL (base_base_binfo))
		TREE_VEC_ELT (base_binfos, j)
		  = make_binfo (BINFO_OFFSET (base_base_binfo),
				base_base_binfo,
				BINFO_VTABLE (base_base_binfo),
				BINFO_VIRTUALS (base_base_binfo),
				chain);
	      chain = TREE_VEC_ELT (base_binfos, j);
	      TREE_VIA_PUBLIC (chain) = TREE_VIA_PUBLIC (base_base_binfo);
	      TREE_VIA_PROTECTED (chain) = TREE_VIA_PROTECTED (base_base_binfo);
	      BINFO_INHERITANCE_CHAIN (chain) = base_binfo;
	    }

	  /* Completely unshare potentially shared data, and
	     update what is ours.  */
	  propagate_binfo_offsets (base_binfo, BINFO_OFFSET (base_binfo));
	}

      if (! TREE_VIA_VIRTUAL (base_binfo))
	CLASSTYPE_N_SUPERCLASSES (t) += 1;

      if (TYPE_VIRTUAL_P (basetype))
	{
	  /* Ensure that this is set from at least a virtual base
             class.  */
	  if (b->rtti == NULL_TREE)
	    b->rtti = CLASSTYPE_RTTI (basetype);

	  /* Don't borrow virtuals from virtual baseclasses.  */
	  if (TREE_VIA_VIRTUAL (base_binfo))
	    continue;

	  if (first_vfn_base_index < 0)
	    {
	      tree vfields;
	      first_vfn_base_index = i;

	      /* Update these two, now that we know what vtable we are
		 going to extend.  This is so that we can add virtual
		 functions, and override them properly.  */
	      BINFO_VTABLE (t_binfo) = TYPE_BINFO_VTABLE (basetype);
	      BINFO_VIRTUALS (t_binfo) = TYPE_BINFO_VIRTUALS (basetype);
	      b->has_virtual = CLASSTYPE_VSIZE (basetype);
	      b->vfield = CLASSTYPE_VFIELD (basetype);
	      b->vfields = copy_list (CLASSTYPE_VFIELDS (basetype));
	      vfields = b->vfields;
	      while (vfields)
		{
		  if (VF_BINFO_VALUE (vfields) == NULL_TREE
		      || ! TREE_VIA_VIRTUAL (VF_BINFO_VALUE (vfields)))
		    {
		      tree value = VF_BASETYPE_VALUE (vfields);
		      if (DECL_NAME (CLASSTYPE_VFIELD (value))
			  == DECL_NAME (CLASSTYPE_VFIELD (basetype)))
			VF_NORMAL_VALUE (b->vfields) = basetype;
		      else
			VF_NORMAL_VALUE (b->vfields) = VF_NORMAL_VALUE (vfields);
		    }
		  vfields = TREE_CHAIN (vfields);
		}
	      CLASSTYPE_VFIELD (t) = b->vfield;
	    }
	  else
	    {
	      /* Only add unique vfields, and flatten them out as we go.  */
	      tree vfields = CLASSTYPE_VFIELDS (basetype);
	      while (vfields)
		{
		  if (VF_BINFO_VALUE (vfields) == NULL_TREE
		      || ! TREE_VIA_VIRTUAL (VF_BINFO_VALUE (vfields)))
		    {
		      tree value = VF_BASETYPE_VALUE (vfields);
		      b->vfields = tree_cons (base_binfo, value, b->vfields);
		      if (DECL_NAME (CLASSTYPE_VFIELD (value))
			  == DECL_NAME (CLASSTYPE_VFIELD (basetype)))
			VF_NORMAL_VALUE (b->vfields) = basetype;
		      else
			VF_NORMAL_VALUE (b->vfields) = VF_NORMAL_VALUE (vfields);
		    }
		  vfields = TREE_CHAIN (vfields);
		}

	      if (b->has_virtual == 0)
		{
		  first_vfn_base_index = i;

		  /* Update these two, now that we know what vtable we are
		     going to extend.  This is so that we can add virtual
		     functions, and override them properly.  */
		  BINFO_VTABLE (t_binfo) = TYPE_BINFO_VTABLE (basetype);
		  BINFO_VIRTUALS (t_binfo) = TYPE_BINFO_VIRTUALS (basetype);
		  b->has_virtual = CLASSTYPE_VSIZE (basetype);
		  b->vfield = CLASSTYPE_VFIELD (basetype);
		  CLASSTYPE_VFIELD (t) = b->vfield;
		  /* When we install the first one, set the VF_NORMAL_VALUE
		     to be the current class, as this it is the most derived
		     class.  Hopefully, this is not set to something else
		     later.  (mrs) */
		  vfields = b->vfields;
		  while (vfields)
		    {
		      if (DECL_NAME (CLASSTYPE_VFIELD (t))
			  == DECL_NAME (CLASSTYPE_VFIELD (basetype)))
			{
			  VF_NORMAL_VALUE (vfields) = t;
			  /* There should only be one of them!  And it should
			     always be found, if we get into here.  (mrs)  */
			  break;
			}
		      vfields = TREE_CHAIN (vfields);
		    }
		}
	    }
	}
    }

  /* Must come after offsets are fixed for all bases.  */
  for (i = 0; i < n_baseclasses; i++)
    {
      tree base_binfo = TREE_VEC_ELT (binfos, i);
      tree basetype = BINFO_TYPE (base_binfo);

      if (get_base_distance (basetype, t_binfo, 0, (tree*)0) == -2)
	{
	  cp_warning ("direct base `%T' inaccessible in `%T' due to ambiguity",
		      basetype, t);
	}
    }
  {
    tree v = get_vbase_types (t_binfo);

    for (; v; v = TREE_CHAIN (v))
      {
	tree basetype = BINFO_TYPE (v);
	if (get_base_distance (basetype, t_binfo, 0, (tree*)0) == -2)
	  {
	    if (extra_warnings)
	      cp_warning ("virtual base `%T' inaccessible in `%T' due to ambiguity",
			  basetype, t);
	  }
      }
  }    

  {
    tree vfields;
    /* Find the base class with the largest number of virtual functions.  */
    for (vfields = b->vfields; vfields; vfields = TREE_CHAIN (vfields))
      {
	if (CLASSTYPE_VSIZE (VF_BASETYPE_VALUE (vfields)) > b->max_has_virtual)
	  b->max_has_virtual = CLASSTYPE_VSIZE (VF_BASETYPE_VALUE (vfields));
	if (VF_DERIVED_VALUE (vfields)
	    && CLASSTYPE_VSIZE (VF_DERIVED_VALUE (vfields)) > b->max_has_virtual)
	  b->max_has_virtual = CLASSTYPE_VSIZE (VF_DERIVED_VALUE (vfields));
      }
  }

  if (b->vfield == 0)
    /* If all virtual functions come only from virtual baseclasses.  */
    return -1;

  /* Update the rtti base if we have a non-virtual base class version
     of it.  */
  b->rtti = CLASSTYPE_RTTI (BINFO_TYPE (TREE_VEC_ELT (binfos, first_vfn_base_index)));

  return first_vfn_base_index;
}

static int
typecode_p (type, code)
     tree type;
     enum tree_code code;
{
  return (TREE_CODE (type) == code
	  || (TREE_CODE (type) == REFERENCE_TYPE
	      && TREE_CODE (TREE_TYPE (type)) == code));
}

/* Set memoizing fields and bits of T (and its variants) for later use.
   MAX_HAS_VIRTUAL is the largest size of any T's virtual function tables.  */

static void
finish_struct_bits (t, max_has_virtual)
     tree t;
     int max_has_virtual;
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
      TYPE_NEEDS_DESTRUCTOR (variants) = TYPE_NEEDS_DESTRUCTOR (t);

      TYPE_USES_COMPLEX_INHERITANCE (variants) = TYPE_USES_COMPLEX_INHERITANCE (t);
      TYPE_VIRTUAL_P (variants) = TYPE_VIRTUAL_P (t);
      TYPE_USES_VIRTUAL_BASECLASSES (variants) = TYPE_USES_VIRTUAL_BASECLASSES (t);
      /* Copy whatever these are holding today.  */
      TYPE_MIN_VALUE (variants) = TYPE_MIN_VALUE (t);
      TYPE_MAX_VALUE (variants) = TYPE_MAX_VALUE (t);
      TYPE_FIELDS (variants) = TYPE_FIELDS (t);
      TYPE_SIZE (variants) = TYPE_SIZE (t);
      variants = TYPE_NEXT_VARIANT (variants);
    }

  if (n_baseclasses && max_has_virtual)
    {
      /* Done by `finish_struct' for classes without baseclasses.  */
      int might_have_abstract_virtuals = CLASSTYPE_ABSTRACT_VIRTUALS (t) != 0;
      tree binfos = TYPE_BINFO_BASETYPES (t);
      for (i = n_baseclasses-1; i >= 0; i--)
	{
	  might_have_abstract_virtuals
	    |= (CLASSTYPE_ABSTRACT_VIRTUALS (BINFO_TYPE (TREE_VEC_ELT (binfos, i))) != 0);
	  if (might_have_abstract_virtuals)
	    break;
	}
      if (might_have_abstract_virtuals)
	{
	  /* We use error_mark_node from override_one_vtable to signal
	     an artificial abstract.  */
	  if (CLASSTYPE_ABSTRACT_VIRTUALS (t) == error_mark_node)
	    CLASSTYPE_ABSTRACT_VIRTUALS (t) = NULL_TREE;
	  CLASSTYPE_ABSTRACT_VIRTUALS (t) = get_abstract_virtuals (t);
	}
    }

  if (n_baseclasses)
    {
      /* Notice whether this class has type conversion functions defined.  */
      tree binfo = TYPE_BINFO (t);
      tree binfos = BINFO_BASETYPES (binfo);
      tree basetype;

      for (i = n_baseclasses-1; i >= 0; i--)
	{
	  basetype = BINFO_TYPE (TREE_VEC_ELT (binfos, i));

	  if (TYPE_HAS_CONVERSION (basetype))
	    {
	      TYPE_HAS_CONVERSION (t) = 1;
	      TYPE_HAS_INT_CONVERSION (t) |= TYPE_HAS_INT_CONVERSION (basetype);
	      TYPE_HAS_REAL_CONVERSION (t) |= TYPE_HAS_REAL_CONVERSION (basetype);
	    }
	  if (CLASSTYPE_MAX_DEPTH (basetype) >= CLASSTYPE_MAX_DEPTH (t))
	    CLASSTYPE_MAX_DEPTH (t) = CLASSTYPE_MAX_DEPTH (basetype) + 1;
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

/* Add FNDECL to the method_vec growing on the class_obstack.  Used by
   finish_struct_methods.  Note, FNDECL cannot be a constructor or
   destructor, those cases are handled by the caller.  */

static void
grow_method (fndecl, method_vec_ptr)
     tree fndecl;
     tree *method_vec_ptr;
{
  tree method_vec = (tree)obstack_base (&class_obstack);

  /* Start off past the constructors and destructor.  */
  tree *testp = &TREE_VEC_ELT (method_vec, 2);

  while (testp < (tree *) obstack_next_free (&class_obstack)
	 && (*testp == NULL_TREE || DECL_NAME (*testp) != DECL_NAME (fndecl)))
    testp++;

  if (testp < (tree *) obstack_next_free (&class_obstack))
    {
      tree *p;
      for (p = testp; *p; )
	p = &DECL_CHAIN (*p);
      *p = fndecl;
    }
  else
    {
      obstack_ptr_grow (&class_obstack, fndecl);
      *method_vec_ptr = (tree)obstack_base (&class_obstack);
    }
}

/* Warn about duplicate methods in fn_fields.  Also compact method
   lists so that lookup can be made faster.

   Algorithm: Outer loop builds lists by method name.  Inner loop
   checks for redundant method names within a list.

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

   We also link each field which has shares a name with its baseclass
   to the head of the list of fields for that base class.  This allows
   us to reduce search time in places like `build_method_call' to
   consider only reasonably likely functions.  */

tree
finish_struct_methods (t, fn_fields, nonprivate_method)
     tree t;
     tree fn_fields;
     int nonprivate_method;
{
  tree method_vec;
  tree save_fn_fields = fn_fields;
  tree ctor_name = constructor_name (t);
  int i, n_baseclasses = CLASSTYPE_N_BASECLASSES (t);

  /* Now prepare to gather fn_fields into vector.  */
  struct obstack *ambient_obstack = current_obstack;
  current_obstack = &class_obstack;
  method_vec = make_tree_vec (2);
  current_obstack = ambient_obstack;

  /* Now make this a live vector.  */
  obstack_free (&class_obstack, method_vec);

  /* Save room for constructors and destructors.  */
  obstack_blank (&class_obstack, sizeof (struct tree_vec) + sizeof (struct tree *));

  /* First fill in entry 0 with the constructors, entry 1 with destructors,
     and the next few with type conversion operators (if any).  */

  for (; fn_fields; fn_fields = TREE_CHAIN (fn_fields))
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
	  if (DESTRUCTOR_NAME_P (DECL_ASSEMBLER_NAME (fn_fields)))
	    {	    
	      /* Destructors go in slot 1.  */
	      DECL_CHAIN (fn_fields) = TREE_VEC_ELT (method_vec, 1);
	      TREE_VEC_ELT (method_vec, 1) = fn_fields;
	    }
	  else
	    {
	      /* Constructors go in slot 0.  */
	      DECL_CHAIN (fn_fields) = TREE_VEC_ELT (method_vec, 0);
	      TREE_VEC_ELT (method_vec, 0) = fn_fields;
	    }
 	}
      else if (IDENTIFIER_TYPENAME_P (fn_name))
	{
	  tree return_type = TREE_TYPE (TREE_TYPE (fn_fields));

	  if (typecode_p (return_type, INTEGER_TYPE)
	      || typecode_p (return_type, BOOLEAN_TYPE)
	      || typecode_p (return_type, ENUMERAL_TYPE))
	    TYPE_HAS_INT_CONVERSION (t) = 1;
	  else if (typecode_p (return_type, REAL_TYPE))
	    TYPE_HAS_REAL_CONVERSION (t) = 1;

	  grow_method (fn_fields, &method_vec);
	}
    }

  fn_fields = save_fn_fields;
  for (; fn_fields; fn_fields = TREE_CHAIN (fn_fields))
    {
      tree fn_name = DECL_NAME (fn_fields);

      if (fn_name == ctor_name || IDENTIFIER_TYPENAME_P (fn_name))
	continue;

      if (fn_name == ansi_opname[(int) MODIFY_EXPR])
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

      grow_method (fn_fields, &method_vec);
    }

  TREE_VEC_LENGTH (method_vec) = (tree *)obstack_next_free (&class_obstack)
    - (&TREE_VEC_ELT (method_vec, 0));
  obstack_finish (&class_obstack);
  CLASSTYPE_METHOD_VEC (t) = method_vec;

  if (nonprivate_method == 0
      && CLASSTYPE_FRIEND_CLASSES (t) == NULL_TREE
      && DECL_FRIENDLIST (TYPE_MAIN_DECL (t)) == NULL_TREE)
    {
      tree binfos = BINFO_BASETYPES (TYPE_BINFO (t));
      for (i = 0; i < n_baseclasses; i++)
	if (TREE_VIA_PUBLIC (TREE_VEC_ELT (binfos, i))
	    || TREE_VIA_PROTECTED (TREE_VEC_ELT (binfos, i)))
	  {
	    nonprivate_method = 1;
	    break;
	  }
      if (nonprivate_method == 0 
	  && warn_ctor_dtor_privacy)
	cp_warning ("all member functions in class `%T' are private", t);
    }

  /* Warn if all destructors are private (in which case this class is
     effectively unusable.  */
  if (TYPE_HAS_DESTRUCTOR (t))
    {
      tree dtor = TREE_VEC_ELT (method_vec, 1);

      /* Wild parse errors can cause this to happen.  */
      if (dtor == NULL_TREE)
	TYPE_HAS_DESTRUCTOR (t) = 0;
      else if (TREE_PRIVATE (dtor)
	       && CLASSTYPE_FRIEND_CLASSES (t) == NULL_TREE
	       && DECL_FRIENDLIST (TYPE_MAIN_DECL (t)) == NULL_TREE
	       && warn_ctor_dtor_privacy)
	cp_warning ("`%#T' only defines a private destructor and has no friends",
		    t);
    }

  /* Now for each member function (except for constructors and
     destructors), compute where member functions of the same
     name reside in base classes.  */
  if (n_baseclasses != 0
      && TREE_VEC_LENGTH (method_vec) > 2)
    {
      int len = TREE_VEC_LENGTH (method_vec);
      tree baselink_vec = make_tree_vec (len);
      int any_links = 0;
      tree baselink_binfo = build_tree_list (NULL_TREE, TYPE_BINFO (t));

      for (i = 2; i < len; i++)
	{
	  TREE_VEC_ELT (baselink_vec, i)
	    = get_baselinks (baselink_binfo, t, DECL_NAME (TREE_VEC_ELT (method_vec, i)));
	  if (TREE_VEC_ELT (baselink_vec, i) != 0)
	    any_links = 1;
	}
      if (any_links != 0)
	CLASSTYPE_BASELINK_VEC (t) = baselink_vec;
      else
	obstack_free (current_obstack, baselink_vec);
    }

  /* Now, figure out what any member template specializations were
     specializing.  */
  for (i = 0; i < TREE_VEC_LENGTH (method_vec); ++i)
    {
      tree fn;
      for (fn = TREE_VEC_ELT (method_vec, i);
	   fn != NULL_TREE;
	   fn = DECL_CHAIN (fn))
	if (DECL_TEMPLATE_SPECIALIZATION (fn))
	  {
	    tree f;
	    tree spec_args;

	    /* If there is a template, and t uses template parms, we
	       are dealing with a specialization of a member
	       template in a template class, and we must grab the
	       template, rather than the function.  */
	    if (DECL_TI_TEMPLATE (fn) && uses_template_parms (t))
	      f = DECL_TI_TEMPLATE (fn);
	    else
	      f = fn;

	    /* We want the specialization arguments, which will be the
	       innermost ones.  */
	    if (DECL_TI_ARGS (f) 
		&& TREE_CODE (DECL_TI_ARGS (f)) == TREE_VEC)
	      spec_args 
		= TREE_VEC_ELT (DECL_TI_ARGS (f), 0);
	    else
	      spec_args = DECL_TI_ARGS (f);
		
	    check_explicit_specialization 
	      (lookup_template_function (DECL_NAME (f), spec_args),
	       f, 0, 1);

	    /* Now, the assembler name will be correct for fn, so we
	       make its RTL.  */
	    DECL_RTL (f) = 0;
	    make_decl_rtl (f, NULL_PTR, 1);
	    if (f != fn)
	      {
		DECL_RTL (fn) = 0;
		make_decl_rtl (fn, NULL_PTR, 1);
	      }
	  }
    }

  return method_vec;
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
	      TREE_CHAIN (unchain) = NULL_TREE;
	      unchain = DECL_CHAIN (unchain);
	    }
	}
    }

  if (TYPE_LANG_SPECIFIC (t))
    {
      tree as_list = CLASSTYPE_AS_LIST (t);
      tree binfo = TYPE_BINFO (t);
      tree binfo_as_list = CLASSTYPE_BINFO_AS_LIST (t);
      int interface_only = CLASSTYPE_INTERFACE_ONLY (t);
      int interface_unknown = CLASSTYPE_INTERFACE_UNKNOWN (t);

      bzero ((char *) TYPE_LANG_SPECIFIC (t), sizeof (struct lang_type));
      BINFO_BASETYPES(binfo) = NULL_TREE;

      CLASSTYPE_AS_LIST (t) = as_list;
      TYPE_BINFO (t) = binfo;
      CLASSTYPE_BINFO_AS_LIST (t) = binfo_as_list;
      CLASSTYPE_INTERFACE_ONLY (t) = interface_only;
      SET_CLASSTYPE_INTERFACE_UNKNOWN_X (t, interface_unknown);
      CLASSTYPE_VBASE_SIZE (t) = integer_zero_node;
      TYPE_REDEFINED (t) = 1;
    }
  TYPE_SIZE (t) = NULL_TREE;
  TYPE_MODE (t) = VOIDmode;
  TYPE_FIELDS (t) = NULL_TREE;
  TYPE_METHODS (t) = NULL_TREE;
  TYPE_VFIELD (t) = NULL_TREE;
  TYPE_CONTEXT (t) = NULL_TREE;
}

/* finish up all new vtables.  */

static void
finish_vtbls (binfo, do_self, t)
     tree binfo;
     int do_self;
     tree t;
{
  tree binfos = BINFO_BASETYPES (binfo);
  int i, n_baselinks = binfos ? TREE_VEC_LENGTH (binfos) : 0;

  /* Should we use something besides CLASSTYPE_VFIELDS? */
  if (do_self && CLASSTYPE_VFIELDS (BINFO_TYPE (binfo)))
    {
      if (BINFO_NEW_VTABLE_MARKED (binfo))
	{
	  tree decl, context;

	  decl = BINFO_VTABLE (binfo);
	  context = DECL_CONTEXT (decl);
	  DECL_CONTEXT (decl) = 0;
	  if (write_virtuals >= 0
	      && DECL_INITIAL (decl) != BINFO_VIRTUALS (binfo))
	    DECL_INITIAL (decl) = build_nt (CONSTRUCTOR, NULL_TREE,
					    BINFO_VIRTUALS (binfo));
	  cp_finish_decl (decl, DECL_INITIAL (decl), NULL_TREE, 0, 0);
	  DECL_CONTEXT (decl) = context;
	}
      CLEAR_BINFO_NEW_VTABLE_MARKED (binfo);
    }

  for (i = 0; i < n_baselinks; i++)
    {
      tree base_binfo = TREE_VEC_ELT (binfos, i);
      int is_not_base_vtable
	= i != CLASSTYPE_VFIELD_PARENT (BINFO_TYPE (binfo));
      if (TREE_VIA_VIRTUAL (base_binfo))
	{
	  base_binfo = binfo_member (BINFO_TYPE (base_binfo), CLASSTYPE_VBASECLASSES (t));
	}
      finish_vtbls (base_binfo, is_not_base_vtable, t);
    }
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
      if ((TYPE_READONLY (TREE_TYPE (TREE_VALUE (base_types)))
	   == TYPE_READONLY (TREE_TYPE (TREE_VALUE (types))))
	  && compparms (TREE_CHAIN (base_types), TREE_CHAIN (types), 3))
	return 1;
    }
  return 0;
}

static tree
get_class_offset_1 (parent, binfo, context, t, fndecl)
     tree parent, binfo, context, t, fndecl;
{
  tree binfos = BINFO_BASETYPES (binfo);
  int i, n_baselinks = binfos ? TREE_VEC_LENGTH (binfos) : 0;
  tree rval = NULL_TREE;

  if (binfo == parent)
    return error_mark_node;

  for (i = 0; i < n_baselinks; i++)
    {
      tree base_binfo = TREE_VEC_ELT (binfos, i);
      tree nrval;

      if (TREE_VIA_VIRTUAL (base_binfo))
	base_binfo = binfo_member (BINFO_TYPE (base_binfo),
				   CLASSTYPE_VBASECLASSES (t));
      nrval = get_class_offset_1 (parent, base_binfo, context, t, fndecl);
      /* See if we have a new value */
      if (nrval && (nrval != error_mark_node || rval==0))
	{
	  /* Only compare if we have two offsets */
	  if (rval && rval != error_mark_node
	      && ! tree_int_cst_equal (nrval, rval))
	    {
	      /* Only give error if the two offsets are different */
	      error ("every virtual function must have a unique final overrider");
	      cp_error ("  found two (or more) `%T' class subobjects in `%T'", context, t);
	      cp_error ("  with virtual `%D' from virtual base class", fndecl);
	      return rval;
	    }
	  rval = nrval;
	}
	
      if (rval && BINFO_TYPE (binfo) == context)
	{
	  my_friendly_assert (rval == error_mark_node
			      || tree_int_cst_equal (rval, BINFO_OFFSET (binfo)), 999);
	  rval = BINFO_OFFSET (binfo);
	}
    }
  return rval;
}

/* Get the offset to the CONTEXT subobject that is related to the
   given BINFO.  */

static tree
get_class_offset (context, t, binfo, fndecl)
     tree context, t, binfo, fndecl;
{
  tree first_binfo = binfo;
  tree offset;
  int i;

  if (context == t)
    return integer_zero_node;

  if (BINFO_TYPE (binfo) == context)
    return BINFO_OFFSET (binfo);

  /* Check less derived binfos first.  */
  while (BINFO_BASETYPES (binfo)
	 && (i=CLASSTYPE_VFIELD_PARENT (BINFO_TYPE (binfo))) != -1)
    {
      tree binfos = BINFO_BASETYPES (binfo);
      binfo = TREE_VEC_ELT (binfos, i);
      if (BINFO_TYPE (binfo) == context)
	return BINFO_OFFSET (binfo);
    }

  /* Ok, not found in the less derived binfos, now check the more
     derived binfos.  */
  offset = get_class_offset_1 (first_binfo, TYPE_BINFO (t), context, t, fndecl);
  if (offset==0 || TREE_CODE (offset) != INTEGER_CST)
    my_friendly_abort (999);	/* we have to find it.  */
  return offset;
}

/* Skip RTTI information at the front of the virtual list.  */

unsigned HOST_WIDE_INT
skip_rtti_stuff (virtuals)
     tree *virtuals;
{
  int n;

  n = 0;
  if (*virtuals)
    {
      /* We always reserve a slot for the offset/tdesc entry.  */
      ++n;
      *virtuals = TREE_CHAIN (*virtuals);
    }
  if (flag_vtable_thunks && *virtuals)
    {
      /* The second slot is reserved for the tdesc pointer when thunks
         are used.  */
      ++n;
      *virtuals = TREE_CHAIN (*virtuals);
    }
  return n;
}

static void
modify_one_vtable (binfo, t, fndecl, pfn)
     tree binfo, t, fndecl, pfn;
{
  tree virtuals = BINFO_VIRTUALS (binfo);
  unsigned HOST_WIDE_INT n;
  
  /* update rtti entry */
  if (flag_rtti)
    {
      if (binfo == TYPE_BINFO (t))
	{
	  if (! BINFO_NEW_VTABLE_MARKED (binfo))
	    build_vtable (TYPE_BINFO (DECL_CONTEXT (CLASSTYPE_VFIELD (t))), t);
	}
      else
	{
	  if (! BINFO_NEW_VTABLE_MARKED (binfo))
	    prepare_fresh_vtable (binfo, t);
	}
    }
  if (fndecl == NULL_TREE)
    return;

  n = skip_rtti_stuff (&virtuals);

  while (virtuals)
    {
      tree current_fndecl = TREE_VALUE (virtuals);
      current_fndecl = FNADDR_FROM_VTABLE_ENTRY (current_fndecl);
      current_fndecl = TREE_OPERAND (current_fndecl, 0);
      if (current_fndecl && overrides (fndecl, current_fndecl))
	{
	  tree base_offset, offset;
	  tree context = DECL_CLASS_CONTEXT (fndecl);
	  tree vfield = CLASSTYPE_VFIELD (t);
	  tree this_offset;

	  offset = get_class_offset (context, t, binfo, fndecl);

	  /* Find the right offset for the this pointer based on the
	     base class we just found.  We have to take into
	     consideration the virtual base class pointers that we
	     stick in before the virtual function table pointer.

	     Also, we want just the delta between the most base class
	     that we derived this vfield from and us.  */
	  base_offset = size_binop (PLUS_EXPR,
				    get_derived_offset (binfo, DECL_CONTEXT (current_fndecl)),
				    BINFO_OFFSET (binfo));
	  this_offset = size_binop (MINUS_EXPR, offset, base_offset);

	  /* Make sure we can modify the derived association with immunity.  */
	  if (TREE_USED (binfo))
	    my_friendly_assert (0, 999);

	  if (binfo == TYPE_BINFO (t))
	    {
	      /* In this case, it is *type*'s vtable we are modifying.
		 We start with the approximation that it's vtable is that
		 of the immediate base class.  */
	      if (! BINFO_NEW_VTABLE_MARKED (binfo))
		build_vtable (TYPE_BINFO (DECL_CONTEXT (vfield)), t);
	    }
	  else
	    {
	      /* This is our very own copy of `basetype' to play with.
		 Later, we will fill in all the virtual functions
		 that override the virtual functions in these base classes
		 which are not defined by the current type.  */
	      if (! BINFO_NEW_VTABLE_MARKED (binfo))
		prepare_fresh_vtable (binfo, t);
	    }

#ifdef NOTQUITE
	  cp_warning ("in %D", DECL_NAME (BINFO_VTABLE (binfo)));
#endif
	  modify_vtable_entry (get_vtable_entry_n (BINFO_VIRTUALS (binfo), n),
			       build_vtable_entry (this_offset, pfn),
			       fndecl);
	}
      ++n;
      virtuals = TREE_CHAIN (virtuals);
    }
}

/* These are the ones that are not through virtual base classes.  */

static void
modify_all_direct_vtables (binfo, do_self, t, fndecl, pfn)
     tree binfo;
     int do_self;
     tree t, fndecl, pfn;
{
  tree binfos = BINFO_BASETYPES (binfo);
  int i, n_baselinks = binfos ? TREE_VEC_LENGTH (binfos) : 0;

  /* Should we use something besides CLASSTYPE_VFIELDS? */
  if (do_self && CLASSTYPE_VFIELDS (BINFO_TYPE (binfo)))
    {
      modify_one_vtable (binfo, t, fndecl, pfn);
    }

  for (i = 0; i < n_baselinks; i++)
    {
      tree base_binfo = TREE_VEC_ELT (binfos, i);
      int is_not_base_vtable
	= i != CLASSTYPE_VFIELD_PARENT (BINFO_TYPE (binfo));
      if (! TREE_VIA_VIRTUAL (base_binfo))
	modify_all_direct_vtables (base_binfo, is_not_base_vtable, t, fndecl, pfn);
    }
}

/* Fixup all the delta entries in this one vtable that need updating.  */

static void
fixup_vtable_deltas1 (binfo, t)
     tree binfo, t;
{
  tree virtuals = BINFO_VIRTUALS (binfo);
  unsigned HOST_WIDE_INT n;
  
  n = skip_rtti_stuff (&virtuals);

  while (virtuals)
    {
      tree fndecl = TREE_VALUE (virtuals);
      tree pfn = FNADDR_FROM_VTABLE_ENTRY (fndecl);
      tree delta = DELTA_FROM_VTABLE_ENTRY (fndecl);
      fndecl = TREE_OPERAND (pfn, 0);
      if (fndecl)
	{
	  tree base_offset, offset;
	  tree context = DECL_CLASS_CONTEXT (fndecl);
	  tree vfield = CLASSTYPE_VFIELD (t);
	  tree this_offset;

	  offset = get_class_offset (context, t, binfo, fndecl);

	  /* Find the right offset for the this pointer based on the
	     base class we just found.  We have to take into
	     consideration the virtual base class pointers that we
	     stick in before the virtual function table pointer.

	     Also, we want just the delta between the most base class
	     that we derived this vfield from and us.  */
	  base_offset = size_binop (PLUS_EXPR,
				    get_derived_offset (binfo, DECL_CONTEXT (fndecl)),
				    BINFO_OFFSET (binfo));
	  this_offset = size_binop (MINUS_EXPR, offset, base_offset);

	  if (! tree_int_cst_equal (this_offset, delta))
	    {
	      /* Make sure we can modify the derived association with immunity.  */
	      if (TREE_USED (binfo))
		my_friendly_assert (0, 999);

	      if (binfo == TYPE_BINFO (t))
		{
		  /* In this case, it is *type*'s vtable we are modifying.
		     We start with the approximation that it's vtable is that
		     of the immediate base class.  */
		  if (! BINFO_NEW_VTABLE_MARKED (binfo))
		    build_vtable (TYPE_BINFO (DECL_CONTEXT (vfield)), t);
		}
	      else
		{
		  /* This is our very own copy of `basetype' to play with.
		     Later, we will fill in all the virtual functions
		     that override the virtual functions in these base classes
		     which are not defined by the current type.  */
		  if (! BINFO_NEW_VTABLE_MARKED (binfo))
		    prepare_fresh_vtable (binfo, t);
		}

	      modify_vtable_entry (get_vtable_entry_n (BINFO_VIRTUALS (binfo), n),
				   build_vtable_entry (this_offset, pfn),
				   fndecl);
	    }
	}
      ++n;
      virtuals = TREE_CHAIN (virtuals);
    }
}

/* Fixup all the delta entries in all the direct vtables that need updating.
   This happens when we have non-overridden virtual functions from a
   virtual base class, that are at a different offset, in the new
   hierarchy, because the layout of the virtual bases has changed.  */

static void
fixup_vtable_deltas (binfo, init_self, t)
     tree binfo;
     int init_self;
     tree t;
{
  tree binfos = BINFO_BASETYPES (binfo);
  int i, n_baselinks = binfos ? TREE_VEC_LENGTH (binfos) : 0;

  for (i = 0; i < n_baselinks; i++)
    {
      tree base_binfo = TREE_VEC_ELT (binfos, i);
      int is_not_base_vtable
	= i != CLASSTYPE_VFIELD_PARENT (BINFO_TYPE (binfo));
      if (! TREE_VIA_VIRTUAL (base_binfo))
	fixup_vtable_deltas (base_binfo, is_not_base_vtable, t);
    }
  /* Should we use something besides CLASSTYPE_VFIELDS? */
  if (init_self && CLASSTYPE_VFIELDS (BINFO_TYPE (binfo)))
    {
      fixup_vtable_deltas1 (binfo, t);
    }
}

/* These are the ones that are through virtual base classes.  */

static void
modify_all_indirect_vtables (binfo, do_self, via_virtual, t, fndecl, pfn)
     tree binfo;
     int do_self, via_virtual;
     tree t, fndecl, pfn;
{
  tree binfos = BINFO_BASETYPES (binfo);
  int i, n_baselinks = binfos ? TREE_VEC_LENGTH (binfos) : 0;

  /* Should we use something besides CLASSTYPE_VFIELDS? */
  if (do_self && via_virtual && CLASSTYPE_VFIELDS (BINFO_TYPE (binfo)))
    {
      modify_one_vtable (binfo, t, fndecl, pfn);
    }

  for (i = 0; i < n_baselinks; i++)
    {
      tree base_binfo = TREE_VEC_ELT (binfos, i);
      int is_not_base_vtable
	= i != CLASSTYPE_VFIELD_PARENT (BINFO_TYPE (binfo));
      if (TREE_VIA_VIRTUAL (base_binfo))
	{
	  via_virtual = 1;
	  base_binfo = binfo_member (BINFO_TYPE (base_binfo), CLASSTYPE_VBASECLASSES (t));
	}
      modify_all_indirect_vtables (base_binfo, is_not_base_vtable, via_virtual, t, fndecl, pfn);
    }
}

static void
modify_all_vtables (t, fndecl, vfn)
     tree t, fndecl, vfn;
{
  /* Do these first, so that we will make use of any non-virtual class's
     vtable, over a virtual classes vtable.  */
  modify_all_direct_vtables (TYPE_BINFO (t), 1, t, fndecl, vfn);
  if (TYPE_USES_VIRTUAL_BASECLASSES (t))
    modify_all_indirect_vtables (TYPE_BINFO (t), 1, 0, t, fndecl, vfn);
}

/* Here, we already know that they match in every respect.
   All we have to check is where they had their declarations.  */

static int 
strictly_overrides (fndecl1, fndecl2)
     tree fndecl1, fndecl2;
{
  int distance = get_base_distance (DECL_CLASS_CONTEXT (fndecl2),
				    DECL_CLASS_CONTEXT (fndecl1),
				    0, (tree *)0);
  if (distance == -2 || distance > 0)
    return 1;
  return 0;
}

/* Merge overrides for one vtable.
   If we want to merge in same function, we are fine.
   else
     if one has a DECL_CLASS_CONTEXT that is a parent of the
       other, than choose the more derived one
     else
       potentially ill-formed (see 10.3 [class.virtual])
       we have to check later to see if there was an
       override in this class.  If there was ok, if not
       then it is ill-formed.  (mrs)

   We take special care to reuse a vtable, if we can.  */

static void
override_one_vtable (binfo, old, t)
     tree binfo, old, t;
{
  tree virtuals = BINFO_VIRTUALS (binfo);
  tree old_virtuals = BINFO_VIRTUALS (old);
  enum { REUSE_NEW, REUSE_OLD, UNDECIDED, NEITHER } choose = UNDECIDED;

  /* If we have already committed to modifying it, then don't try and
     reuse another vtable.  */
  if (BINFO_NEW_VTABLE_MARKED (binfo))
    choose = NEITHER;

  skip_rtti_stuff (&virtuals);
  skip_rtti_stuff (&old_virtuals);

  while (virtuals)
    {
      tree fndecl = TREE_VALUE (virtuals);
      tree old_fndecl = TREE_VALUE (old_virtuals);
      fndecl = FNADDR_FROM_VTABLE_ENTRY (fndecl);
      old_fndecl = FNADDR_FROM_VTABLE_ENTRY (old_fndecl);
      fndecl = TREE_OPERAND (fndecl, 0);
      old_fndecl = TREE_OPERAND (old_fndecl, 0);
      /* First check to see if they are the same.  */
      if (DECL_ASSEMBLER_NAME (fndecl) == DECL_ASSEMBLER_NAME (old_fndecl))
	{
	  /* No need to do anything.  */
	}
      else if (strictly_overrides (fndecl, old_fndecl))
	{
	  if (choose == UNDECIDED)
	    choose = REUSE_NEW;
	  else if (choose == REUSE_OLD)
	    {
	      choose = NEITHER;
	      if (! BINFO_NEW_VTABLE_MARKED (binfo))
		{
		  prepare_fresh_vtable (binfo, t);
		  override_one_vtable (binfo, old, t);
		  return;
		}
	    }
	}
      else if (strictly_overrides (old_fndecl, fndecl))
	{
	  if (choose == UNDECIDED)
	    choose = REUSE_OLD;
	  else if (choose == REUSE_NEW)
	    {
	      choose = NEITHER;
	      if (! BINFO_NEW_VTABLE_MARKED (binfo))
		{
		  prepare_fresh_vtable (binfo, t);
		  override_one_vtable (binfo, old, t);
		  return;
		}
	      TREE_VALUE (virtuals) = TREE_VALUE (old_virtuals);
	    }
	  else if (choose == NEITHER)
	    {
	      TREE_VALUE (virtuals) = TREE_VALUE (old_virtuals);
	    }  
	}
      else
	{
	  choose = NEITHER;
	  if (! BINFO_NEW_VTABLE_MARKED (binfo))
	    {
	      prepare_fresh_vtable (binfo, t);
	      override_one_vtable (binfo, old, t);
	      return;
	    }
	  {
	    /* This MUST be overridden, or the class is ill-formed.  */
	    /* For now, we just make it abstract.  */
	    tree fndecl = TREE_OPERAND (FNADDR_FROM_VTABLE_ENTRY (TREE_VALUE (virtuals)), 0);
	    tree vfn;

	    fndecl = copy_node (fndecl);
	    copy_lang_decl (fndecl);
	    DECL_ABSTRACT_VIRTUAL_P (fndecl) = 1;
	    /* Make sure we search for it later.  */
	    if (! CLASSTYPE_ABSTRACT_VIRTUALS (t))
	      CLASSTYPE_ABSTRACT_VIRTUALS (t) = error_mark_node;

	    vfn = build1 (ADDR_EXPR, vfunc_ptr_type_node, fndecl);
	    TREE_CONSTANT (vfn) = 1;
	    
	    /* We can use integer_zero_node, as we will will core dump
	       if this is used anyway.  */
	    TREE_VALUE (virtuals) = build_vtable_entry (integer_zero_node, vfn);
	  }
	}
      virtuals = TREE_CHAIN (virtuals);
      old_virtuals = TREE_CHAIN (old_virtuals);
    }

  /* Let's reuse the old vtable.  */
  if (choose == REUSE_OLD)
    {
      BINFO_VTABLE (binfo) = BINFO_VTABLE (old);
      BINFO_VIRTUALS (binfo) = BINFO_VIRTUALS (old);
    }
}

/* Merge in overrides for virtual bases.
   BINFO is the hierarchy we want to modify, and OLD has the potential
   overrides.  */

static void
merge_overrides (binfo, old, do_self, t)
     tree binfo, old;
     int do_self;
     tree t;
{
  tree binfos = BINFO_BASETYPES (binfo);
  tree old_binfos = BINFO_BASETYPES (old);
  int i, n_baselinks = binfos ? TREE_VEC_LENGTH (binfos) : 0;

  /* Should we use something besides CLASSTYPE_VFIELDS? */
  if (do_self && CLASSTYPE_VFIELDS (BINFO_TYPE (binfo)))
    {
      override_one_vtable (binfo, old, t);
    }

  for (i = 0; i < n_baselinks; i++)
    {
      tree base_binfo = TREE_VEC_ELT (binfos, i);
      tree old_base_binfo = TREE_VEC_ELT (old_binfos, i);
      int is_not_base_vtable
	= i != CLASSTYPE_VFIELD_PARENT (BINFO_TYPE (binfo));
      if (! TREE_VIA_VIRTUAL (base_binfo))
	merge_overrides (base_binfo, old_base_binfo, is_not_base_vtable, t);
    }
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
	base_fndecls = temp_tree_cons (fndecl, methods, base_fndecls);

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
   no need to mark functions that are just hidden.  */

static void
mark_overriders (fndecl, base_fndecls)
     tree fndecl, base_fndecls;
{
  while (base_fndecls)
    {
      if (overrides (TREE_VALUE (base_fndecls), fndecl))
	TREE_PURPOSE (base_fndecls) = fndecl;

      base_fndecls = TREE_CHAIN (base_fndecls);
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

  for (i = 0; i < n_baselinks; i++)
    {
      tree base_binfo = TREE_VEC_ELT (binfos, i);
      if (TYPE_VIRTUAL_P (BINFO_TYPE (base_binfo))
	  || flag_all_virtual == 1)
	{
	  tree tmp = get_matching_virtual
	    (base_binfo, decl,
	     DESTRUCTOR_NAME_P (DECL_ASSEMBLER_NAME (decl)));
	  if (tmp)
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
		  cp_error_at ("method `%D' may not be declared static",
			       decl);
		  cp_error_at ("(since `%D' declared virtual in base class.)",
			       tmp);
		  break;
		}
	      virtualp = 1;

#if 0 /* The signature of an overriding function is not changed.  */
	      {
		/* The argument types may have changed...  */
		tree type = TREE_TYPE (decl);
		tree argtypes = TYPE_ARG_TYPES (type);
		tree base_variant = TREE_TYPE (TREE_VALUE (argtypes));
		tree raises = TYPE_RAISES_EXCEPTIONS (type);

		argtypes = commonparms (TREE_CHAIN (TYPE_ARG_TYPES (TREE_TYPE (tmp))),
					TREE_CHAIN (argtypes));
		/* But the return type has not.  */
		type = build_cplus_method_type (base_variant, TREE_TYPE (type), argtypes);
		if (raises)
		  type = build_exception_variant (type, raises);
		TREE_TYPE (decl) = type;
	      }
#endif
	      DECL_VINDEX (decl)
		= tree_cons (NULL_TREE, tmp, DECL_VINDEX (decl));
	      break;
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
  for (i = 2; i < n_methods; ++i)
    {
      tree fndecl = TREE_VEC_ELT (method_vec, i);

      tree base_fndecls = NULL_TREE;
      tree binfos = BINFO_BASETYPES (TYPE_BINFO (t));
      int i, n_baseclasses = binfos ? TREE_VEC_LENGTH (binfos) : 0;

      if (DECL_VINDEX (fndecl) == NULL_TREE)
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

      if (TREE_CHAIN (fndecl)
	  && DECL_NAME (TREE_CHAIN (fndecl)) == DECL_NAME (fndecl))
	  fndecl = TREE_CHAIN (fndecl);
	else
	  fndecl = NULL_TREE;

      /* ...then mark up all the base functions with overriders, preferring
	 overriders to hiders.  */
      if (base_fndecls)
	while (fndecl)
	  {
	    mark_overriders (fndecl, base_fndecls);
	    
	    if (TREE_CHAIN (fndecl)
		&& DECL_NAME (TREE_CHAIN (fndecl)) == DECL_NAME (fndecl))
	      fndecl = TREE_CHAIN (fndecl);
	    else
	      fndecl = NULL_TREE;
	  }

      /* Now give a warning for all base functions without overriders,
	 as they are hidden.  */
      while (base_fndecls)
	{
	  if (! overrides (TREE_VALUE (base_fndecls),
			   TREE_PURPOSE (base_fndecls)))
	    {
	      /* Here we know it is a hider, and no overrider exists.  */
	      cp_warning_at ("`%D' was hidden", TREE_VALUE (base_fndecls));
	      cp_warning_at ("  by `%D'", TREE_PURPOSE (base_fndecls));
	    }

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
	  && TREE_CODE (TREE_TYPE (field)) == UNION_TYPE)
	{
	  tree* uelt = &TYPE_FIELDS (TREE_TYPE (field));
	  for (; *uelt; uelt = &TREE_CHAIN (*uelt))
	    {
	      if (TREE_CODE (*uelt) != FIELD_DECL)
		continue;

	      if (TREE_PRIVATE (*uelt))
		cp_pedwarn_at ("private member `%#D' in anonymous union",
			       *uelt);
	      else if (TREE_PROTECTED (*uelt))
		cp_pedwarn_at ("protected member `%#D' in anonymous union",
			       *uelt);

	      TREE_PRIVATE (*uelt) = TREE_PRIVATE (field);
	      TREE_PROTECTED (*uelt) = TREE_PROTECTED (field);
	    }
	}
    }
}

extern int interface_only, interface_unknown;

/* Create a RECORD_TYPE or UNION_TYPE node for a C struct or union declaration
   (or C++ class declaration).

   For C++, we must handle the building of derived classes.
   Also, C++ allows static class members.  The way that this is
   handled is to keep the field name where it is (as the DECL_NAME
   of the field), and place the overloaded decl in the DECL_FIELD_BITPOS
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

   LIST_OF_FIELDLISTS is just that.  The elements of the list are
   TREE_LIST elements, whose TREE_PURPOSE field tells what access
   the list has, and the TREE_VALUE slot gives the actual fields.

   ATTRIBUTES is the set of decl attributes to be applied, if any.

   If flag_all_virtual == 1, then we lay all functions into
   the virtual function table, as though they were declared
   virtual.  Constructors do not lay down in the virtual function table.

   If flag_all_virtual == 2, then we lay all functions into
   the virtual function table, such that virtual functions
   occupy a space by themselves, and then all functions
   of the class occupy a space by themselves.  This is illustrated
   in the following diagram:

   class A; class B : A;

	Class A's vtbl:			Class B's vtbl:
    --------------------------------------------------------------------
   | A's virtual functions|		| B's virtual functions		|
   |			  |		| (may inherit some from A).	|
    --------------------------------------------------------------------
   | All of A's functions |		| All of A's functions		|
   | (such as a->A::f).	  |		| (such as b->A::f)		|
    --------------------------------------------------------------------
					| B's new virtual functions	|
					| (not defined in A.)		|
					 -------------------------------
					| All of B's functions		|
					| (such as b->B::f)		|
					 -------------------------------

   this allows the program to make references to any function, virtual
   or otherwise in a type-consistent manner.  */

tree
finish_struct_1 (t, warn_anon)
     tree t;
     int warn_anon;
{
  int old;
  tree name = TYPE_IDENTIFIER (t);
  enum tree_code code = TREE_CODE (t);
  tree fields = TYPE_FIELDS (t);
  tree fn_fields = TYPE_METHODS (t);
  tree x, last_x, method_vec;
  int all_virtual;
  int has_virtual;
  int max_has_virtual;
  tree pending_virtuals = NULL_TREE;
  tree pending_hard_virtuals = NULL_TREE;
  tree abstract_virtuals = NULL_TREE;
  tree vfield;
  tree vfields;
  int cant_have_default_ctor;
  int cant_have_const_ctor;
  int no_const_asn_ref;

  /* The index of the first base class which has virtual
     functions.  Only applied to non-virtual baseclasses.  */
  int first_vfn_base_index;

  int n_baseclasses;
  int any_default_members = 0;
  int const_sans_init = 0;
  int ref_sans_init = 0;
  int nonprivate_method = 0;
  tree t_binfo = TYPE_BINFO (t);
  tree access_decls = NULL_TREE;
  int aggregate = 1;
  int empty = 1;
  int has_pointers = 0;

  if (warn_anon && code != UNION_TYPE && ANON_AGGRNAME_P (TYPE_IDENTIFIER (t)))
    pedwarn ("anonymous class type not used to declare any objects");

  if (TYPE_SIZE (t))
    {
      if (IS_AGGR_TYPE (t))
	cp_error ("redefinition of `%#T'", t);
      else
	my_friendly_abort (172);
      popclass (0);
      return t;
    }

  GNU_xref_decl (current_function_decl, t);

  /* If this type was previously laid out as a forward reference,
     make sure we lay it out again.  */

  TYPE_SIZE (t) = NULL_TREE;
  CLASSTYPE_GOT_SEMICOLON (t) = 0;

#if 0
  /* This is in general too late to do this.  I moved the main case up to
     left_curly, what else needs to move?  */
  if (! IS_SIGNATURE (t))
    {
      my_friendly_assert (CLASSTYPE_INTERFACE_ONLY (t) == interface_only, 999);
      my_friendly_assert (CLASSTYPE_INTERFACE_KNOWN (t) == ! interface_unknown, 999);
    }
#endif

#if 0
  if (flag_rtti)
    build_t_desc (t, 0);
#endif

  TYPE_BINFO (t) = NULL_TREE;

  old = suspend_momentary ();

  /* Install struct as DECL_FIELD_CONTEXT of each field decl.
     Also process specified field sizes.
     Set DECL_FIELD_SIZE to the specified size, or 0 if none specified.
     The specified size is found in the DECL_INITIAL.
     Store 0 there, except for ": 0" fields (so we can find them
     and delete them, below).  */

  if (t_binfo && BINFO_BASETYPES (t_binfo))
    n_baseclasses = TREE_VEC_LENGTH (BINFO_BASETYPES (t_binfo));
  else
    n_baseclasses = 0;

  if (n_baseclasses > 0)
    {
      struct base_info base_info;

      /* If using multiple inheritance, this may cause variants of our
	 basetypes to be used (instead of their canonical forms).  */
      tree vf = layout_basetypes (t, BINFO_BASETYPES (t_binfo));
      last_x = tree_last (vf);
      fields = chainon (vf, fields);

      first_vfn_base_index = finish_base_struct (t, &base_info, t_binfo);
      /* Remember where we got our vfield from.  */
      CLASSTYPE_VFIELD_PARENT (t) = first_vfn_base_index;
      has_virtual = base_info.has_virtual;
      max_has_virtual = base_info.max_has_virtual;
      CLASSTYPE_N_SUPERCLASSES (t) += base_info.n_ancestors;
      vfield = base_info.vfield;
      vfields = base_info.vfields;
      CLASSTYPE_RTTI (t) = base_info.rtti;
      cant_have_default_ctor = base_info.cant_have_default_ctor;
      cant_have_const_ctor = base_info.cant_have_const_ctor;
      no_const_asn_ref = base_info.no_const_asn_ref;
      n_baseclasses = TREE_VEC_LENGTH (BINFO_BASETYPES (t_binfo));
      aggregate = 0;
    }
  else
    {
      first_vfn_base_index = -1;
      has_virtual = 0;
      max_has_virtual = has_virtual;
      vfield = NULL_TREE;
      vfields = NULL_TREE;
      CLASSTYPE_RTTI (t) = NULL_TREE;
      last_x = NULL_TREE;
      cant_have_default_ctor = 0;
      cant_have_const_ctor = 0;
      no_const_asn_ref = 0;
    }

#if 0
  /* Both of these should be done before now.  */
  if (write_virtuals == 3 && CLASSTYPE_INTERFACE_KNOWN (t)
      && ! IS_SIGNATURE (t))
    {
      my_friendly_assert (CLASSTYPE_INTERFACE_ONLY (t) == interface_only, 999);
      my_friendly_assert (CLASSTYPE_VTABLE_NEEDS_WRITING (t) == ! interface_only, 999);
    }
#endif

  /* The three of these are approximations which may later be
     modified.  Needed at this point to make add_virtual_function
     and modify_vtable_entries work.  */
  TREE_CHAIN (t_binfo) = TYPE_BINFO (t);
  TYPE_BINFO (t) = t_binfo;
  CLASSTYPE_VFIELDS (t) = vfields;
  CLASSTYPE_VFIELD (t) = vfield;

  if (IS_SIGNATURE (t))
    all_virtual = 0;
  else if (flag_all_virtual == 1 && TYPE_OVERLOADS_METHOD_CALL_EXPR (t))
    all_virtual = 1;
  else
    all_virtual = 0;

  for (x = TYPE_METHODS (t); x; x = TREE_CHAIN (x))
    {
      GNU_xref_member (current_class_name, x);

      nonprivate_method |= ! TREE_PRIVATE (x);

      /* If this was an evil function, don't keep it in class.  */
      if (IDENTIFIER_ERROR_LOCUS (DECL_ASSEMBLER_NAME (x)))
	continue;

      DECL_CLASS_CONTEXT (x) = t;

      /* Do both of these, even though they're in the same union;
	 if the insn `r' member and the size `i' member are
	 different sizes, as on the alpha, the larger of the two
	 will end up with garbage in it.  */
      DECL_SAVED_INSNS (x) = NULL_RTX;
      DECL_FIELD_SIZE (x) = 0;

      check_for_override (x, t);
      if (DECL_ABSTRACT_VIRTUAL_P (x) && ! DECL_VINDEX (x))
	cp_error_at ("initializer specified for non-virtual method `%D'", x);

      /* The name of the field is the original field name
	 Save this in auxiliary field for later overloading.  */
      if (DECL_VINDEX (x)
	  || (all_virtual == 1 && ! DECL_CONSTRUCTOR_P (x)))
	{
	  add_virtual_function (&pending_virtuals, &pending_hard_virtuals,
				&has_virtual, x, t);
	  if (DECL_ABSTRACT_VIRTUAL_P (x))
	    abstract_virtuals = tree_cons (NULL_TREE, x, abstract_virtuals);
#if 0
	  /* XXX Why did I comment this out?  (jason) */
	  else
	    TREE_USED (x) = 1;
#endif
	}
    }

  for (x = TYPE_FIELDS (t); x; x = TREE_CHAIN (x))
    {
      GNU_xref_member (current_class_name, x);

      if (TREE_CODE (x) == FIELD_DECL)
	{
	  DECL_PACKED (x) |= TYPE_PACKED (t);
	  empty = 0;
	}

      /* Handle access declarations.  */
      if (TREE_CODE (x) == USING_DECL)
	{
	  tree ctype = DECL_INITIAL (x);
	  tree sname = DECL_NAME (x);
	  tree access
	    = TREE_PRIVATE (x) ? access_private_node
			       : TREE_PROTECTED (x) ? access_protected_node
						    : access_public_node;
	  tree fdecl, binfo;

	  if (last_x)
	    TREE_CHAIN (last_x) = TREE_CHAIN (x);
	  else
	    fields = TREE_CHAIN (x);

	  binfo = binfo_or_else (ctype, t);
	  if (! binfo)
	    continue;

	  if (sname == constructor_name (ctype)
	      || sname == constructor_name_full (ctype))
	    cp_error_at ("using-declaration for constructor", x);

	  fdecl = lookup_field (binfo, sname, 0, 0);
	  if (! fdecl)
	    fdecl = lookup_fnfields (binfo, sname, 0);

	  if (fdecl)
	    access_decls = scratch_tree_cons (access, fdecl, access_decls);
	  else
	    cp_error_at ("no members matching `%D' in `%#T'", x, ctype);
	  continue;
	}

      last_x = x;

      if (TREE_CODE (x) == TYPE_DECL)
	continue;

      /* If we've gotten this far, it's a data member, possibly static,
	 or an enumerator.  */

      DECL_FIELD_CONTEXT (x) = t;

      /* ``A local class cannot have static data members.'' ARM 9.4 */
      if (current_function_decl && TREE_STATIC (x))
	cp_error_at ("field `%D' in local class cannot be static", x);

      /* Perform error checking that did not get done in
	 grokdeclarator.  */
      if (TREE_CODE (TREE_TYPE (x)) == FUNCTION_TYPE)
	{
	  cp_error_at ("field `%D' invalidly declared function type",
		       x);
	  TREE_TYPE (x) = build_pointer_type (TREE_TYPE (x));
	}
      else if (TREE_CODE (TREE_TYPE (x)) == METHOD_TYPE)
	{
	  cp_error_at ("field `%D' invalidly declared method type", x);
	  TREE_TYPE (x) = build_pointer_type (TREE_TYPE (x));
	}
      else if (TREE_CODE (TREE_TYPE (x)) == OFFSET_TYPE)
	{
	  cp_error_at ("field `%D' invalidly declared offset type", x);
	  TREE_TYPE (x) = build_pointer_type (TREE_TYPE (x));
	}

#if 0
      if (DECL_NAME (x) == constructor_name (t))
	cant_have_default_ctor = 1;
#endif

      if (TREE_TYPE (x) == error_mark_node)
	continue;
	  
      DECL_SAVED_INSNS (x) = NULL_RTX;
      DECL_FIELD_SIZE (x) = 0;

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
	aggregate = 0;

      /* If this is of reference type, check if it needs an init.
	 Also do a little ANSI jig if necessary.  */
      if (TREE_CODE (TREE_TYPE (x)) == REFERENCE_TYPE)
 	{
	  if (DECL_INITIAL (x) == NULL_TREE)
	    ref_sans_init = 1;

	  /* ARM $12.6.2: [A member initializer list] (or, for an
	     aggregate, initialization by a brace-enclosed list) is the
	     only way to initialize nonstatic const and reference
	     members.  */
	  cant_have_default_ctor = 1;
	  TYPE_HAS_COMPLEX_ASSIGN_REF (t) = 1;

	  if (! TYPE_HAS_CONSTRUCTOR (t) && extra_warnings)
	    {
	      if (DECL_NAME (x))
		cp_warning_at ("non-static reference `%#D' in class without a constructor", x);
	      else
		cp_warning_at ("non-static reference in class without a constructor", x);
	    }
	}

      if (TREE_CODE (TREE_TYPE (x)) == POINTER_TYPE)
	has_pointers = 1;

      /* If any field is const, the structure type is pseudo-const.  */
      if (TREE_READONLY (x))
	{
	  C_TYPE_FIELDS_READONLY (t) = 1;
	  if (DECL_INITIAL (x) == NULL_TREE)
	    const_sans_init = 1;

	  /* ARM $12.6.2: [A member initializer list] (or, for an
	     aggregate, initialization by a brace-enclosed list) is the
	     only way to initialize nonstatic const and reference
	     members.  */
	  cant_have_default_ctor = 1;
	  TYPE_HAS_COMPLEX_ASSIGN_REF (t) = 1;

	  if (! TYPE_HAS_CONSTRUCTOR (t) && !IS_SIGNATURE (t)
	      && extra_warnings)
	    {
	      if (DECL_NAME (x))
		cp_warning_at ("non-static const member `%#D' in class without a constructor", x);
	      else
		cp_warning_at ("non-static const member in class without a constructor", x);
	    }
	}
      else
	{
	  /* A field that is pseudo-const makes the structure
	     likewise.  */
	  tree t1 = TREE_TYPE (x);
	  while (TREE_CODE (t1) == ARRAY_TYPE)
	    t1 = TREE_TYPE (t1);
	  if (IS_AGGR_TYPE (t1))
	    {
	      if (C_TYPE_FIELDS_READONLY (t1))
		C_TYPE_FIELDS_READONLY (t) = 1;
	      if (CLASSTYPE_READONLY_FIELDS_NEED_INIT (t1))
		const_sans_init = 1;
	    }
	}

      /* We set DECL_BIT_FIELD tentatively in grokbitfield.
	 If the type and width are valid, we'll keep it set.
	 Otherwise, the flag is cleared.  */
      if (DECL_BIT_FIELD (x))
	{
	  DECL_BIT_FIELD (x) = 0;
	  /* Invalid bit-field size done by grokfield.  */
	  /* Detect invalid bit-field type.  */
	  if (DECL_INITIAL (x)
	      && ! INTEGRAL_TYPE_P (TREE_TYPE (x)))
	    {
	      cp_error_at ("bit-field `%#D' with non-integral type", x);
	      DECL_INITIAL (x) = NULL;
	    }

	  /* Detect and ignore out of range field width.  */
	  if (DECL_INITIAL (x))
	    {
	      tree w = DECL_INITIAL (x);
	      register int width;

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
			       x);
		  DECL_INITIAL (x) = NULL_TREE;
		}
	      else if (width = TREE_INT_CST_LOW (w),
		       width < 0)
		{
		  DECL_INITIAL (x) = NULL;
		  cp_error_at ("negative width in bit-field `%D'", x);
		}
	      else if (width == 0 && DECL_NAME (x) != 0)
		{
		  DECL_INITIAL (x) = NULL;
		  cp_error_at ("zero width for bit-field `%D'", x);
		}
	      else if (width
		       > TYPE_PRECISION (long_long_unsigned_type_node))
		{
		  /* The backend will dump if you try to use something
		     too big; avoid that.  */
		  DECL_INITIAL (x) = NULL;
		  sorry ("bit-fields larger than %d bits",
			 TYPE_PRECISION (long_long_unsigned_type_node));
		  cp_error_at ("  in declaration of `%D'", x);
		}
	      else if (width > TYPE_PRECISION (TREE_TYPE (x))
		       && TREE_CODE (TREE_TYPE (x)) != ENUMERAL_TYPE)
		{
		  cp_warning_at ("width of `%D' exceeds its type", x);
		}
	      else if (TREE_CODE (TREE_TYPE (x)) == ENUMERAL_TYPE
		       && ((min_precision (TYPE_MIN_VALUE (TREE_TYPE (x)),
					   TREE_UNSIGNED (TREE_TYPE (x))) > width)
			   || (min_precision (TYPE_MAX_VALUE (TREE_TYPE (x)),
					      TREE_UNSIGNED (TREE_TYPE (x))) > width)))
		{
		  cp_warning_at ("`%D' is too small to hold all values of `%#T'",
				 x, TREE_TYPE (x));
		}

	      if (DECL_INITIAL (x) == NULL_TREE)
		;
	      else if (width == 0)
		{
#ifdef EMPTY_FIELD_BOUNDARY
		  DECL_ALIGN (x) = MAX (DECL_ALIGN (x), EMPTY_FIELD_BOUNDARY);
#endif
#ifdef PCC_BITFIELD_TYPE_MATTERS
		  DECL_ALIGN (x) = MAX (DECL_ALIGN (x),
					TYPE_ALIGN (TREE_TYPE (x)));
#endif
		}
	      else
		{
		  DECL_INITIAL (x) = NULL_TREE;
		  DECL_FIELD_SIZE (x) = width;
		  DECL_BIT_FIELD (x) = 1;
		}
	    }
	  else
	    /* Non-bit-fields are aligned for their type.  */
	    DECL_ALIGN (x) = MAX (DECL_ALIGN (x), TYPE_ALIGN (TREE_TYPE (x)));
	}
      else
	{
	  tree type = TREE_TYPE (x);

	  while (TREE_CODE (type) == ARRAY_TYPE)
	    type = TREE_TYPE (type);

	  if (TYPE_LANG_SPECIFIC (type) && ! ANON_UNION_P (x)
	      && ! TYPE_PTRMEMFUNC_P (type))
	    {
	      /* Never let anything with uninheritable virtuals
		 make it through without complaint.  */
	      if (CLASSTYPE_ABSTRACT_VIRTUALS (type))
		abstract_virtuals_error (x, type);
		      
	      /* Don't let signatures make it through either.  */
	      if (IS_SIGNATURE (type))
		signature_error (x, type);
		      
	      if (code == UNION_TYPE)
		{
		  char *fie = NULL;
		  if (TYPE_NEEDS_CONSTRUCTING (type))
		    fie = "constructor";
		  else if (TYPE_NEEDS_DESTRUCTOR (type))
		    fie = "destructor";
		  else if (TYPE_HAS_REAL_ASSIGNMENT (type))
		    fie = "assignment operator";
		  if (fie)
		    cp_error_at ("member `%#D' with %s not allowed in union", x,
				 fie);
		}
	      else
		{
		  TYPE_NEEDS_CONSTRUCTING (t) |= TYPE_NEEDS_CONSTRUCTING (type);
		  TYPE_NEEDS_DESTRUCTOR (t) |= TYPE_NEEDS_DESTRUCTOR (type);
		  TYPE_HAS_COMPLEX_ASSIGN_REF (t) |= TYPE_HAS_COMPLEX_ASSIGN_REF (type);
		  TYPE_HAS_COMPLEX_INIT_REF (t) |= TYPE_HAS_COMPLEX_INIT_REF (type);
		}

	      if (!TYPE_HAS_CONST_INIT_REF (type))
		cant_have_const_ctor = 1;

	      if (!TYPE_HAS_CONST_ASSIGN_REF (type))
		no_const_asn_ref = 1;

	      if (TYPE_HAS_CONSTRUCTOR (type)
		  && ! TYPE_HAS_DEFAULT_CONSTRUCTOR (type))
		{
		  cant_have_default_ctor = 1;
#if 0
		  /* This is wrong for aggregates.  */
		  if (! TYPE_HAS_CONSTRUCTOR (t))
		    {
		      if (DECL_NAME (x))
			cp_pedwarn_at ("member `%#D' with only non-default constructor", x);
		      else
			cp_pedwarn_at ("member with only non-default constructor", x);
		      cp_pedwarn_at ("in class without a constructor",
				     x);
		    }
#endif
		}
	    }
	  if (DECL_INITIAL (x) != NULL_TREE)
	    {
	      /* `build_class_init_list' does not recognize
		 non-FIELD_DECLs.  */
	      if (code == UNION_TYPE && any_default_members != 0)
		cp_error_at ("multiple fields in union `%T' initialized");
	      any_default_members = 1;
	    }
	}
    }

  /* If this type has any constant members which did not come
     with their own initialization, mark that fact here.  It is
     not an error here, since such types can be saved either by their
     constructors, or by fortuitous initialization.  */
  CLASSTYPE_READONLY_FIELDS_NEED_INIT (t) = const_sans_init;
  CLASSTYPE_REF_FIELDS_NEED_INIT (t) = ref_sans_init;
  CLASSTYPE_ABSTRACT_VIRTUALS (t) = abstract_virtuals;

  /* Synthesize any needed methods.  Note that methods will be synthesized
     for anonymous unions; grok_x_components undoes that.  */

  if (! fn_fields)
    nonprivate_method = 1;

  if (TYPE_NEEDS_DESTRUCTOR (t) && !TYPE_HAS_DESTRUCTOR (t)
      && !IS_SIGNATURE (t))
    {
      /* Here we must cons up a destructor on the fly.  */
      tree dtor = cons_up_default_function (t, name, 0);
      check_for_override (dtor, t);

      /* If we couldn't make it work, then pretend we didn't need it.  */
      if (dtor == void_type_node)
	TYPE_NEEDS_DESTRUCTOR (t) = 0;
      else
	{
	  /* Link dtor onto end of fn_fields.  */

	  TREE_CHAIN (dtor) = fn_fields;
	  fn_fields = dtor;

	  if (DECL_VINDEX (dtor))
	    add_virtual_function (&pending_virtuals, &pending_hard_virtuals,
				  &has_virtual, dtor, t);
	  nonprivate_method = 1;
	}
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

  TYPE_NEEDS_DESTRUCTOR (t) |= TYPE_HAS_DESTRUCTOR (t);

  TYPE_HAS_COMPLEX_INIT_REF (t)
    |= (TYPE_HAS_INIT_REF (t) || TYPE_USES_VIRTUAL_BASECLASSES (t)
	|| has_virtual || any_default_members);
  TYPE_NEEDS_CONSTRUCTING (t)
    |= (TYPE_HAS_CONSTRUCTOR (t) || TYPE_USES_VIRTUAL_BASECLASSES (t)
	|| has_virtual || any_default_members);
  if (! IS_SIGNATURE (t))
    CLASSTYPE_NON_AGGREGATE (t)
      = ! aggregate || has_virtual || TYPE_HAS_CONSTRUCTOR (t);

  /* ARM $12.1: A default constructor will be generated for a class X
     only if no constructor has been declared for class X.  So we
     check TYPE_HAS_CONSTRUCTOR also, to make sure we don't generate
     one if they declared a constructor in this class.  */
  if (! TYPE_HAS_CONSTRUCTOR (t) && ! cant_have_default_ctor
      && ! IS_SIGNATURE (t))
    {
      tree default_fn = cons_up_default_function (t, name, 2);
      TREE_CHAIN (default_fn) = fn_fields;
      fn_fields = default_fn;
    }

  /* Create default copy constructor, if needed.  */
  if (! TYPE_HAS_INIT_REF (t) && ! IS_SIGNATURE (t))
    {
      /* ARM 12.18: You get either X(X&) or X(const X&), but
	 not both.  --Chip  */
      tree default_fn = cons_up_default_function (t, name,
						  3 + cant_have_const_ctor);
      TREE_CHAIN (default_fn) = fn_fields;
      fn_fields = default_fn;
    }

  TYPE_HAS_REAL_ASSIGNMENT (t) |= TYPE_HAS_ASSIGNMENT (t);
  TYPE_HAS_REAL_ASSIGN_REF (t) |= TYPE_HAS_ASSIGN_REF (t);
  TYPE_HAS_COMPLEX_ASSIGN_REF (t)
    |= TYPE_HAS_ASSIGN_REF (t) || TYPE_USES_VIRTUAL_BASECLASSES (t);

  if (! TYPE_HAS_ASSIGN_REF (t) && ! IS_SIGNATURE (t))
    {
      tree default_fn = cons_up_default_function (t, name,
						  5 + no_const_asn_ref);
      TREE_CHAIN (default_fn) = fn_fields;
      fn_fields = default_fn;
    }

  if (fn_fields)
    {
      TYPE_METHODS (t) = fn_fields;
      method_vec = finish_struct_methods (t, fn_fields, nonprivate_method);

      if (TYPE_HAS_CONSTRUCTOR (t)
	  && CLASSTYPE_FRIEND_CLASSES (t) == NULL_TREE
	  && DECL_FRIENDLIST (TYPE_MAIN_DECL (t)) == NULL_TREE)
	{
	  int nonprivate_ctor = 0;
	  tree ctor;

	  for (ctor = TREE_VEC_ELT (method_vec, 0);
	       ctor;
	       ctor = DECL_CHAIN (ctor))
	    if (! TREE_PRIVATE (ctor))
	      {
		nonprivate_ctor = 1;
		break;
	      }

	  if (nonprivate_ctor == 0 && warn_ctor_dtor_privacy)
	    cp_warning ("`%#T' only defines private constructors and has no friends",
			t);
	}
    }
  else
    {
      method_vec = 0;

      /* Just in case these got accidentally
	 filled in by syntax errors.  */
      TYPE_HAS_CONSTRUCTOR (t) = 0;
      TYPE_HAS_DESTRUCTOR (t) = 0;
    }

  {
    int n_methods = method_vec ? TREE_VEC_LENGTH (method_vec) : 0;
    
    for (access_decls = nreverse (access_decls); access_decls;
	 access_decls = TREE_CHAIN (access_decls))
      {
	tree fdecl = TREE_VALUE (access_decls);
	tree flist = NULL_TREE;
	tree name;
	tree access = TREE_PURPOSE (access_decls);
	int i = 2;
	tree tmp;

	if (TREE_CODE (fdecl) == TREE_LIST)
	  {
	    flist = fdecl;
	    fdecl = TREE_VALUE (flist);
	  }

	name = DECL_NAME (fdecl);

	for (; i < n_methods; i++)
	  if (DECL_NAME (TREE_VEC_ELT (method_vec, i)) == name)
	    {
	      cp_error ("cannot adjust access to `%#D' in `%#T'", fdecl, t);
	      cp_error_at ("  because of local method `%#D' with same name",
			   TREE_VEC_ELT (method_vec, i));
	      fdecl = NULL_TREE;
	      break;
	    }

	if (! fdecl)
	  continue;
	
	for (tmp = fields; tmp; tmp = TREE_CHAIN (tmp))
	  if (DECL_NAME (tmp) == name)
	    {
	      cp_error ("cannot adjust access to `%#D' in `%#T'", fdecl, t);
	      cp_error_at ("  because of local field `%#D' with same name", tmp);
	      fdecl = NULL_TREE;
	      break;
	    }

	if (!fdecl)
	  continue;
	
	/* Make type T see field decl FDECL with access ACCESS.*/
	if (flist)
	  {
	    fdecl = TREE_VALUE (flist);
	    while (fdecl)
	      {
		if (alter_access (t, fdecl, access) == 0)
		  break;
		fdecl = DECL_CHAIN (fdecl);
	      }
	  }
	else
	  alter_access (t, fdecl, access);
      }
    
  }

  if (vfield == NULL_TREE && has_virtual)
    {
      /* We build this decl with ptr_type_node, and
	 change the type when we know what it should be.  */
      vfield = build_lang_field_decl (FIELD_DECL, get_vfield_name (t),
				      ptr_type_node);
      /* If you change any of the below, take a look at all the
	 other VFIELD_BASEs and VTABLE_BASEs in the code, and change
	 them too.  */
      DECL_ASSEMBLER_NAME (vfield) = get_identifier (VFIELD_BASE);
      CLASSTYPE_VFIELD (t) = vfield;
      DECL_VIRTUAL_P (vfield) = 1;
      DECL_ARTIFICIAL (vfield) = 1;
      DECL_FIELD_CONTEXT (vfield) = t;
      DECL_CLASS_CONTEXT (vfield) = t;
      DECL_FCONTEXT (vfield) = t;
      DECL_SAVED_INSNS (vfield) = NULL_RTX;
      DECL_FIELD_SIZE (vfield) = 0;
      DECL_ALIGN (vfield) = TYPE_ALIGN (ptr_type_node);
#if 0
      /* This is more efficient, but breaks binary compatibility, turn
	 it on sometime when we don't care.  If we turn it on, we also
	 have to enable the code in dfs_init_vbase_pointers.  */
      /* vfield is always first entry in structure.  */
      TREE_CHAIN (vfield) = fields;
      fields = vfield;
#else
      if (last_x)
	{
	  my_friendly_assert (TREE_CHAIN (last_x) == NULL_TREE, 175);
	  TREE_CHAIN (last_x) = vfield;
	  last_x = vfield;
	}
      else
	fields = vfield;
#endif
      empty = 0;
      vfields = chainon (vfields, CLASSTYPE_AS_LIST (t));
    }

  /* Now DECL_INITIAL is null on all members except for zero-width bit-fields.
     And they have already done their work.

     C++: maybe we will support default field initialization some day...  */

  /* Delete all zero-width bit-fields from the front of the fieldlist */
  while (fields && DECL_BIT_FIELD (fields)
	 && DECL_INITIAL (fields))
    fields = TREE_CHAIN (fields);
  /* Delete all such fields from the rest of the fields.  */
  for (x = fields; x;)
    {
      if (TREE_CHAIN (x) && DECL_BIT_FIELD (TREE_CHAIN (x))
	  && DECL_INITIAL (TREE_CHAIN (x)))
	TREE_CHAIN (x) = TREE_CHAIN (TREE_CHAIN (x));
      else
	x = TREE_CHAIN (x);
    }
  /* Delete all duplicate fields from the fields */
  delete_duplicate_fields (fields);

  /* Catch function/field name conflict.  We don't need to do this for a
     signature, since it can only contain the fields constructed in
     append_signature_fields.  */
  if (! IS_SIGNATURE (t))
    {
      int n_methods = method_vec ? TREE_VEC_LENGTH (method_vec) : 0;
      for (x = fields; x; x = TREE_CHAIN (x))
	{
	  tree name = DECL_NAME (x);
	  int i = 2;

	  if (TREE_CODE (x) == TYPE_DECL && DECL_ARTIFICIAL (x))
	    continue;

	  for (; i < n_methods; ++i)
	    if (DECL_NAME (TREE_VEC_ELT (method_vec, i)) == name)
	      {
		cp_error_at ("data member `%#D' conflicts with", x);
		cp_error_at ("function member `%#D'",
			     TREE_VEC_ELT (method_vec, i));
		break;
	      }
	}
    }

  /* Now we have the final fieldlist for the data fields.  Record it,
     then lay out the structure or union (including the fields).  */

  TYPE_FIELDS (t) = fields;

  /* Pass layout information about base classes to layout_type, if any.  */
  if (n_baseclasses)
    {
      tree pseudo_basetype = TREE_TYPE (base_layout_decl);

      TREE_CHAIN (base_layout_decl) = TYPE_FIELDS (t);
      TYPE_FIELDS (t) = base_layout_decl;

      TYPE_SIZE (pseudo_basetype) = CLASSTYPE_SIZE (t);
      TYPE_MODE (pseudo_basetype) = TYPE_MODE (t);
      TYPE_ALIGN (pseudo_basetype) = CLASSTYPE_ALIGN (t);
      DECL_ALIGN (base_layout_decl) = TYPE_ALIGN (pseudo_basetype);
      /* Don't re-use old size.  */
      DECL_SIZE (base_layout_decl) = NULL_TREE;
    }
  else if (empty)
    {
      /* C++: do not let empty structures exist.  */
      tree decl = build_lang_field_decl
	(FIELD_DECL, NULL_TREE, char_type_node);
      TREE_CHAIN (decl) = TYPE_FIELDS (t);
      TYPE_FIELDS (t) = decl;
    }

  layout_type (t);

  finish_struct_anon (t);

  if (n_baseclasses || empty)
    TYPE_FIELDS (t) = TREE_CHAIN (TYPE_FIELDS (t));

  /* Set the TYPE_DECL for this type to contain the right
     value for DECL_OFFSET, so that we can use it as part
     of a COMPONENT_REF for multiple inheritance.  */

  layout_decl (TYPE_MAIN_DECL (t), 0);

  /* Now fix up any virtual base class types that we left lying
     around.  We must get these done before we try to lay out the
     virtual function table.  */
  pending_hard_virtuals = nreverse (pending_hard_virtuals);

  if (TYPE_USES_VIRTUAL_BASECLASSES (t))
    {
      tree vbases;

      max_has_virtual = layout_vbasetypes (t, max_has_virtual);
      vbases = CLASSTYPE_VBASECLASSES (t);
      CLASSTYPE_N_VBASECLASSES (t) = list_length (vbases);

      {
	/* Now fixup overrides of all functions in vtables from all
	   direct or indirect virtual base classes.  */
	tree binfos = BINFO_BASETYPES (TYPE_BINFO (t));
	int i, n_baseclasses = binfos ? TREE_VEC_LENGTH (binfos) : 0;

	for (i = 0; i < n_baseclasses; i++)
	  {
	    tree base_binfo = TREE_VEC_ELT (binfos, i);
	    tree basetype = BINFO_TYPE (base_binfo);
	    tree vbases;

	    vbases = CLASSTYPE_VBASECLASSES (basetype);
	    while (vbases)
	      {
		merge_overrides (binfo_member (BINFO_TYPE (vbases),
					       CLASSTYPE_VBASECLASSES (t)),
				 vbases, 1, t);
		vbases = TREE_CHAIN (vbases);
	      }
	  }
	}
    }

  /* Set up the DECL_FIELD_BITPOS of the vfield if we need to, as we
     might need to know it for setting up the offsets in the vtable
     (or in thunks) below.  */
  if (vfield != NULL_TREE
      && DECL_FIELD_CONTEXT (vfield) != t)
    {
      tree binfo = get_binfo (DECL_FIELD_CONTEXT (vfield), t, 0);
      tree offset = BINFO_OFFSET (binfo);

      vfield = copy_node (vfield);
      copy_lang_decl (vfield);

      if (! integer_zerop (offset))
	offset = size_binop (MULT_EXPR, offset, size_int (BITS_PER_UNIT));
      DECL_FIELD_CONTEXT (vfield) = t;
      DECL_CLASS_CONTEXT (vfield) = t;
      DECL_FIELD_BITPOS (vfield)
	= size_binop (PLUS_EXPR, offset, DECL_FIELD_BITPOS (vfield));
      CLASSTYPE_VFIELD (t) = vfield;
    }
    
#ifdef NOTQUITE
  cp_warning ("Doing hard virtuals for %T...", t);
#endif

  if (has_virtual > max_has_virtual)
    max_has_virtual = has_virtual;
  if (max_has_virtual > 0)
    TYPE_VIRTUAL_P (t) = 1;

  if (flag_rtti && TYPE_VIRTUAL_P (t) && !pending_hard_virtuals)
    modify_all_vtables (t, NULL_TREE, NULL_TREE);

  while (pending_hard_virtuals)
    {
      modify_all_vtables (t,
			  TREE_PURPOSE (pending_hard_virtuals),
			  TREE_VALUE (pending_hard_virtuals));
      pending_hard_virtuals = TREE_CHAIN (pending_hard_virtuals);
    }
  
  if (TYPE_USES_VIRTUAL_BASECLASSES (t))
    {
      tree vbases;
      /* Now fixup any virtual function entries from virtual bases
	 that have different deltas.  This has to come after we do the
	 pending hard virtuals, as we might have a function that comes
	 from multiple virtual base instances that is only overridden
	 by a hard virtual above.  */
      vbases = CLASSTYPE_VBASECLASSES (t);
      while (vbases)
	{
	  /* We might be able to shorten the amount of work we do by
	     only doing this for vtables that come from virtual bases
	     that have differing offsets, but don't want to miss any
	     entries.  */
	  fixup_vtable_deltas (vbases, 1, t);
	  vbases = TREE_CHAIN (vbases);
	}
    }

  /* Under our model of GC, every C++ class gets its own virtual
     function table, at least virtually.  */
  if (pending_virtuals)
    {
      pending_virtuals = nreverse (pending_virtuals);
      /* We must enter these virtuals into the table.  */
      if (first_vfn_base_index < 0)
	{
	  /* The second slot is for the tdesc pointer when thunks are used.  */
	  if (flag_vtable_thunks)
	    pending_virtuals = tree_cons (NULL_TREE, NULL_TREE, pending_virtuals);

	  /* The first slot is for the rtti offset.  */
	  pending_virtuals = tree_cons (NULL_TREE, NULL_TREE, pending_virtuals);

	  set_rtti_entry (pending_virtuals, size_zero_node, t);
	  build_vtable (NULL_TREE, t);
	}
      else
	{
	  /* Here we know enough to change the type of our virtual
	     function table, but we will wait until later this function.  */

	  if (! BINFO_NEW_VTABLE_MARKED (TYPE_BINFO (t)))
	    build_vtable (TREE_VEC_ELT (TYPE_BINFO_BASETYPES (t), first_vfn_base_index), t);
	}

      /* If this type has basetypes with constructors, then those
	 constructors might clobber the virtual function table.  But
	 they don't if the derived class shares the exact vtable of the base
	 class.  */

      CLASSTYPE_NEEDS_VIRTUAL_REINIT (t) = 1;
    }
  else if (first_vfn_base_index >= 0)
    {
      tree binfo = TREE_VEC_ELT (TYPE_BINFO_BASETYPES (t), first_vfn_base_index);
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

  if (max_has_virtual || first_vfn_base_index >= 0)
    {
      CLASSTYPE_VSIZE (t) = has_virtual;
      if (first_vfn_base_index >= 0)
	{
	  if (pending_virtuals)
	    TYPE_BINFO_VIRTUALS (t) = chainon (TYPE_BINFO_VIRTUALS (t),
						pending_virtuals);
	}
      else if (has_virtual)
	{
	  TYPE_BINFO_VIRTUALS (t) = pending_virtuals;
	  if (write_virtuals >= 0)
	    DECL_VIRTUAL_P (TYPE_BINFO_VTABLE (t)) = 1;
	}
    }

  /* Now lay out the virtual function table.  */
  if (has_virtual)
    {
      tree atype, itype;

      if (TREE_TYPE (vfield) == ptr_type_node)
	{
	  /* We must create a pointer to this table because
	     the one inherited from base class does not exist.
	     We will fill in the type when we know what it
	     should really be.  Use `size_int' so values are memoized
	     in common cases.  */
	  itype = build_index_type (size_int (has_virtual));
	  atype = build_array_type (vtable_entry_type, itype);
	  layout_type (atype);
	  TREE_TYPE (vfield) = build_pointer_type (atype);
	}
      else
	{
	  atype = TREE_TYPE (TREE_TYPE (vfield));

	  if (has_virtual != TREE_INT_CST_LOW (TYPE_MAX_VALUE (TYPE_DOMAIN (atype))))
	    {
	      /* We must extend (or create) the boundaries on this array,
		 because we picked up virtual functions from multiple
		 base classes.  */
	      itype = build_index_type (size_int (has_virtual));
	      atype = build_array_type (vtable_entry_type, itype);
	      layout_type (atype);
	      vfield = copy_node (vfield);
	      TREE_TYPE (vfield) = build_pointer_type (atype);
	    }
	}

      CLASSTYPE_VFIELD (t) = vfield;
      if (TREE_TYPE (TYPE_BINFO_VTABLE (t)) != atype)
	{
	  TREE_TYPE (TYPE_BINFO_VTABLE (t)) = atype;
	  DECL_SIZE (TYPE_BINFO_VTABLE (t)) = 0;
	  layout_decl (TYPE_BINFO_VTABLE (t), 0);
	  /* At one time the vtable info was grabbed 2 words at a time.  This
	     fails on sparc unless you have 8-byte alignment.  (tiemann) */
	  DECL_ALIGN (TYPE_BINFO_VTABLE (t))
	    = MAX (TYPE_ALIGN (double_type_node),
		   DECL_ALIGN (TYPE_BINFO_VTABLE (t)));
	}
    }
  else if (first_vfn_base_index >= 0)
    CLASSTYPE_VFIELD (t) = vfield;
  CLASSTYPE_VFIELDS (t) = vfields;

  finish_struct_bits (t, max_has_virtual);

  /* Complete the rtl for any static member objects of the type we're
     working on.  */
  for (x = fields; x; x = TREE_CHAIN (x))
    {
      if (TREE_CODE (x) == VAR_DECL && TREE_STATIC (x)
	  && TREE_TYPE (x) == t)
	{
	  DECL_MODE (x) = TYPE_MODE (t);
	  make_decl_rtl (x, NULL, 0);
	}
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
      if (any_default_members != 0)
	build_class_init_list (t);
    }
  else if (TYPE_NEEDS_CONSTRUCTING (t))
    build_class_init_list (t);

  /* Write out inline function definitions.  */
  do_inline_function_hair (t, CLASSTYPE_INLINE_FRIENDS (t));
  CLASSTYPE_INLINE_FRIENDS (t) = 0;

  if (CLASSTYPE_VSIZE (t) != 0)
    {
#if 0
      /* This is now done above.  */
      if (DECL_FIELD_CONTEXT (vfield) != t)
	{
	  tree binfo = get_binfo (DECL_FIELD_CONTEXT (vfield), t, 0);
	  tree offset = BINFO_OFFSET (binfo);

	  vfield = copy_node (vfield);
	  copy_lang_decl (vfield);

	  if (! integer_zerop (offset))
	    offset = size_binop (MULT_EXPR, offset, size_int (BITS_PER_UNIT));
	  DECL_FIELD_CONTEXT (vfield) = t;
	  DECL_CLASS_CONTEXT (vfield) = t;
	  DECL_FIELD_BITPOS (vfield)
	    = size_binop (PLUS_EXPR, offset, DECL_FIELD_BITPOS (vfield));
	  CLASSTYPE_VFIELD (t) = vfield;
	}
#endif

      /* In addition to this one, all the other vfields should be listed.  */
      /* Before that can be done, we have to have FIELD_DECLs for them, and
	 a place to find them.  */
      TYPE_NONCOPIED_PARTS (t) = build_tree_list (default_conversion (TYPE_BINFO_VTABLE (t)), vfield);

      if (warn_nonvdtor && TYPE_HAS_DESTRUCTOR (t)
	  && DECL_VINDEX (TREE_VEC_ELT (method_vec, 1)) == NULL_TREE)
	cp_warning ("`%#T' has virtual functions but non-virtual destructor",
		    t);
    }

  /* Make the rtl for any new vtables we have created, and unmark
     the base types we marked.  */
  finish_vtbls (TYPE_BINFO (t), 1, t);
  hack_incomplete_structures (t);

#if 0
  if (TYPE_NAME (t) && TYPE_IDENTIFIER (t))
    undo_template_name_overload (TYPE_IDENTIFIER (t), 1);
#endif

  resume_momentary (old);

  if (warn_overloaded_virtual)
    warn_hidden (t);

#if 0
  /* This has to be done after we have sorted out what to do with
     the enclosing type.  */
  if (write_symbols != DWARF_DEBUG)
    {
      /* Be smarter about nested classes here.  If a type is nested,
	 only output it if we would output the enclosing type.  */
      if (DECL_CONTEXT (TYPE_MAIN_DECL (t))
	  && TREE_CODE_CLASS (TREE_CODE (DECL_CONTEXT (TYPE_MAIN_DECL (t)))) == 't')
	DECL_IGNORED_P (TYPE_MAIN_DECL (t)) = TREE_ASM_WRITTEN (TYPE_MAIN_DECL (t));
    }
#endif

  if (write_symbols != DWARF_DEBUG && write_symbols != DWARF2_DEBUG)
    {
      /* If the type has methods, we want to think about cutting down
	 the amount of symbol table stuff we output.  The value stored in
	 the TYPE_DECL's DECL_IGNORED_P slot is a first approximation.
	 For example, if a member function is seen and we decide to
	 write out that member function, then we can change the value
	 of the DECL_IGNORED_P slot, and the type will be output when
	 that member function's debug info is written out.

	 We can't do this with DWARF, which does not support name
	 references between translation units.  */
      if (CLASSTYPE_METHOD_VEC (t))
	{
	  extern tree pending_vtables;

	  /* Don't output full info about any type
	     which does not have its implementation defined here.  */
	  if (TYPE_VIRTUAL_P (t) && write_virtuals == 2)
	    TYPE_DECL_SUPPRESS_DEBUG (TYPE_MAIN_DECL (t))
	      = (value_member (TYPE_IDENTIFIER (t), pending_vtables) == 0);
	  else if (CLASSTYPE_INTERFACE_ONLY (t))
	    TYPE_DECL_SUPPRESS_DEBUG (TYPE_MAIN_DECL (t)) = 1;
#if 0
	  /* XXX do something about this.  */
	  else if (CLASSTYPE_INTERFACE_UNKNOWN (t))
	    /* Only a first approximation!  */
	    TYPE_DECL_SUPPRESS_DEBUG (TYPE_MAIN_DECL (t)) = 1;
#endif
	}
      else if (CLASSTYPE_INTERFACE_ONLY (t))
	TYPE_DECL_SUPPRESS_DEBUG (TYPE_MAIN_DECL (t)) = 1;
    }

  /* Finish debugging output for this type.  */
  rest_of_type_compilation (t, toplevel_bindings_p ());

  return t;
}

tree
finish_struct (t, list_of_fieldlists, attributes, warn_anon)
     tree t, list_of_fieldlists, attributes;
     int warn_anon;
{
  tree fields = NULL_TREE;
  tree *tail = &TYPE_METHODS (t);
  tree name = TYPE_NAME (t);
  tree x, last_x = NULL_TREE;
  tree access;
  tree dummy = NULL_TREE;

  if (TREE_CODE (name) == TYPE_DECL)
    {
      extern int lineno;
	  
      DECL_SOURCE_FILE (name) = input_filename;
      /* For TYPE_DECL that are not typedefs (those marked with a line
	 number of zero, we don't want to mark them as real typedefs.
	 If this fails one needs to make sure real typedefs have a
	 previous line number, even if it is wrong, that way the below
	 will fill in the right line number.  (mrs) */
      if (DECL_SOURCE_LINE (name))
	DECL_SOURCE_LINE (name) = lineno;
      CLASSTYPE_SOURCE_LINE (t) = lineno;
      name = DECL_NAME (name);
    }

  /* Append the fields we need for constructing signature tables.  */
  if (IS_SIGNATURE (t))
    append_signature_fields (list_of_fieldlists);

  /* Move our self-reference declaration to the end of the field list so
     any real field with the same name takes precedence.  */
  if (list_of_fieldlists
      && TREE_VALUE (list_of_fieldlists)
      && DECL_ARTIFICIAL (TREE_VALUE (list_of_fieldlists)))
    {
      dummy = TREE_VALUE (list_of_fieldlists);
      list_of_fieldlists = TREE_CHAIN (list_of_fieldlists);
    }

  if (last_x && list_of_fieldlists)
    TREE_CHAIN (last_x) = TREE_VALUE (list_of_fieldlists);

  while (list_of_fieldlists)
    {
      access = TREE_PURPOSE (list_of_fieldlists);

      /* For signatures, we made all methods `public' in the parser and
	 reported an error if a access specifier was used.  */
      if (access == access_default_node)
	{
	  if (CLASSTYPE_DECLARED_CLASS (t) == 0)
	    access = access_public_node;
	  else
	    access = access_private_node;
	}

      for (x = TREE_VALUE (list_of_fieldlists); x; x = TREE_CHAIN (x))
	{
	  TREE_PRIVATE (x) = access == access_private_node;
	  TREE_PROTECTED (x) = access == access_protected_node;

	  /* Check for inconsistent use of this name in the class body.
             Enums, types and static vars have already been checked.  */
	  if (TREE_CODE (x) != TYPE_DECL && TREE_CODE (x) != USING_DECL
	      && TREE_CODE (x) != CONST_DECL && TREE_CODE (x) != VAR_DECL)
	    {
	      tree name = DECL_NAME (x);
	      tree icv;

	      /* Don't get confused by access decls.  */
	      if (name && TREE_CODE (name) == IDENTIFIER_NODE)
		icv = IDENTIFIER_CLASS_VALUE (name);
	      else
		icv = NULL_TREE;

	      if (icv
		  /* Don't complain about constructors.  */
		  && name != constructor_name (current_class_type)
		  /* Or inherited names.  */
		  && id_in_current_class (name)
		  /* Or shadowed tags.  */
		  && !(TREE_CODE (icv) == TYPE_DECL
		       && DECL_CONTEXT (icv) == t))
		{
		  cp_error_at ("declaration of identifier `%D' as `%+#D'",
			       name, x);
		  cp_error_at ("conflicts with other use in class as `%#D'",
			       icv);
		}
	    }

	  if (TREE_CODE (x) == FUNCTION_DECL 
	      || DECL_FUNCTION_TEMPLATE_P (x))
	    {
	      DECL_CLASS_CONTEXT (x) = t;
	      if (last_x)
		TREE_CHAIN (last_x) = TREE_CHAIN (x);
	      /* Link x onto end of TYPE_METHODS.  */
	      *tail = x;
	      tail = &TREE_CHAIN (x);
	      continue;
	    }

	  if (TREE_CODE (x) != TYPE_DECL)
	    DECL_FIELD_CONTEXT (x) = t;

	  if (! fields)
	    fields = x;
	  last_x = x;
	}
      list_of_fieldlists = TREE_CHAIN (list_of_fieldlists);
      /* link the tail while we have it! */
      if (last_x)
	{
	  TREE_CHAIN (last_x) = NULL_TREE;

	  if (list_of_fieldlists
	      && TREE_VALUE (list_of_fieldlists)
	      && TREE_CODE (TREE_VALUE (list_of_fieldlists)) != FUNCTION_DECL)
	    TREE_CHAIN (last_x) = TREE_VALUE (list_of_fieldlists);
	}
    }

  /* Now add the tags, if any, to the list of TYPE_DECLs
     defined for this type.  */
  if (CLASSTYPE_TAGS (t) || dummy)
    {
      /* The list of tags was built up in pushtag in reverse order; we need
	 to fix that so that enumerators will be processed in forward order
	 in template instantiation.  */
      CLASSTYPE_TAGS (t) = x = nreverse (CLASSTYPE_TAGS (t));
      while (x)
	{
	  tree tag = TYPE_MAIN_DECL (TREE_VALUE (x));

	  TREE_NONLOCAL_FLAG (TREE_VALUE (x)) = 0;
	  x = TREE_CHAIN (x);
	  last_x = chainon (last_x, tag);
	}
      if (dummy)
	last_x = chainon (last_x, dummy);
      if (fields == NULL_TREE)
	fields = last_x;
      CLASSTYPE_LOCAL_TYPEDECLS (t) = 1;
    }

  *tail = NULL_TREE;
  TYPE_FIELDS (t) = fields;

  cplus_decl_attributes (t, attributes, NULL_TREE);

  if (processing_template_decl)
    {
      tree d = getdecls ();
      for (; d; d = TREE_CHAIN (d))
	{
	  /* If this is the decl for the class or one of the template
             parms, we've seen all the injected decls.  */
	  if ((TREE_CODE (d) == TYPE_DECL
	       && (TREE_TYPE (d) == t
		   || TREE_CODE (TREE_TYPE (d)) == TEMPLATE_TYPE_PARM))
	      || TREE_CODE (d) == CONST_DECL)
	    break;
	  /* Don't inject cache decls.  */
	  else if (IDENTIFIER_TEMPLATE (DECL_NAME (d)))
	    continue;
	  DECL_TEMPLATE_INJECT (CLASSTYPE_TI_TEMPLATE (t))
	    = tree_cons (NULL_TREE, d,
			 DECL_TEMPLATE_INJECT (CLASSTYPE_TI_TEMPLATE (t)));
	}
      CLASSTYPE_METHOD_VEC (t)
	= finish_struct_methods (t, TYPE_METHODS (t), 1);
      TYPE_SIZE (t) = integer_zero_node;
    }      
  else
    t = finish_struct_1 (t, warn_anon);

  TYPE_BEING_DEFINED (t) = 0;

  if (current_class_type)
    popclass (0);
  else
    error ("trying to finish struct, but kicked out due to previous parse errors.");

  return t;
}

/* Return non-zero if the effective type of INSTANCE is static.
   Used to determine whether the virtual function table is needed
   or not.

   *NONNULL is set iff INSTANCE can be known to be nonnull, regardless
   of our knowledge of its type.  */

int
resolves_to_fixed_type_p (instance, nonnull)
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
	  return 1;
	}
      return 0;

    case SAVE_EXPR:
      /* This is a call to a constructor, hence it's never zero.  */
      if (TREE_HAS_CONSTRUCTOR (instance))
	{
	  if (nonnull)
	    *nonnull = 1;
	  return 1;
	}
      return resolves_to_fixed_type_p (TREE_OPERAND (instance, 0), nonnull);

    case RTL_EXPR:
      /* This is a call to `new', hence it's never zero.  */
      if (TREE_CALLS_NEW (instance))
	{
	  if (nonnull)
	    *nonnull = 1;
	  return 1;
	}
      return 0;

    case PLUS_EXPR:
    case MINUS_EXPR:
      if (TREE_CODE (TREE_OPERAND (instance, 1)) == INTEGER_CST)
	/* Propagate nonnull.  */
	resolves_to_fixed_type_p (TREE_OPERAND (instance, 0), nonnull);
      if (TREE_CODE (TREE_OPERAND (instance, 0)) == ADDR_EXPR)
	return resolves_to_fixed_type_p (TREE_OPERAND (instance, 0), nonnull);
      return 0;

    case NOP_EXPR:
    case CONVERT_EXPR:
      return resolves_to_fixed_type_p (TREE_OPERAND (instance, 0), nonnull);

    case ADDR_EXPR:
      if (nonnull)
	*nonnull = 1;
      return resolves_to_fixed_type_p (TREE_OPERAND (instance, 0), nonnull);

    case COMPONENT_REF:
      return resolves_to_fixed_type_p (TREE_OPERAND (instance, 1), nonnull);

    case VAR_DECL:
    case FIELD_DECL:
      if (TREE_CODE (TREE_TYPE (instance)) == ARRAY_TYPE
	  && IS_AGGR_TYPE (TREE_TYPE (TREE_TYPE (instance))))
	{
	  if (nonnull)
	    *nonnull = 1;
	  return 1;
	}
      /* fall through...  */
    case TARGET_EXPR:
    case PARM_DECL:
      if (IS_AGGR_TYPE (TREE_TYPE (instance)))
	{
	  if (nonnull)
	    *nonnull = 1;
	  return 1;
	}
      else if (nonnull)
	{
	  if (instance == current_class_ptr
	      && flag_this_is_variable <= 0)
	    {
	      /* Some people still use `this = 0' inside destructors.  */
	      *nonnull = ! DESTRUCTOR_NAME_P (DECL_ASSEMBLER_NAME (current_function_decl));
	      /* In a constructor, we know our type.  */
	      if (flag_this_is_variable < 0)
		return 1;
	    }
	  else if (TREE_CODE (TREE_TYPE (instance)) == REFERENCE_TYPE)
	    /* Reference variables should be references to objects.  */
	    *nonnull = 1;
	}
      return 0;

    default:
      return 0;
    }
}

void
init_class_processing ()
{
  current_class_depth = 0;
  current_class_stacksize = 10;
  current_class_base = (tree *)xmalloc(current_class_stacksize * sizeof (tree));
  current_class_stack = current_class_base;

  current_lang_stacksize = 10;
  current_lang_base = (tree *)xmalloc(current_lang_stacksize * sizeof (tree));
  current_lang_stack = current_lang_base;

  access_default_node = build_int_2 (0, 0);
  access_public_node = build_int_2 (1, 0);
  access_protected_node = build_int_2 (2, 0);
  access_private_node = build_int_2 (3, 0);
  access_default_virtual_node = build_int_2 (4, 0);
  access_public_virtual_node = build_int_2 (5, 0);
  access_private_virtual_node = build_int_2 (6, 0);

  /* Keep these values lying around.  */
  base_layout_decl = build_lang_field_decl (FIELD_DECL, NULL_TREE, error_mark_node);
  TREE_TYPE (base_layout_decl) = make_node (RECORD_TYPE);

  gcc_obstack_init (&class_obstack);
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
  push_memoized_context (type, modify);

  current_class_depth++;
  *current_class_stack++ = current_class_name;
  *current_class_stack++ = current_class_type;
  if (current_class_stack >= current_class_base + current_class_stacksize)
    {
      current_class_base
	= (tree *)xrealloc (current_class_base,
			    sizeof (tree) * (current_class_stacksize + 10));
      current_class_stack = current_class_base + current_class_stacksize;
      current_class_stacksize += 10;
    }

  current_class_name = TYPE_NAME (type);
  if (TREE_CODE (current_class_name) == TYPE_DECL)
    current_class_name = DECL_NAME (current_class_name);
  current_class_type = type;

  if (previous_class_type != NULL_TREE
      && (type != previous_class_type || TYPE_SIZE (previous_class_type) == NULL_TREE)
      && current_class_depth == 1)
    {
      /* Forcibly remove any old class remnants.  */
      popclass (-1);
      previous_class_type = NULL_TREE;
    }

  pushlevel_class ();

#if 0
  if (CLASSTYPE_TEMPLATE_INFO (type))
    overload_template_name (type);
#endif

  if (modify)
    {
      tree tags;
      tree this_fndecl = current_function_decl;

      if (current_function_decl
	  && DECL_CONTEXT (current_function_decl)
	  && TREE_CODE (DECL_CONTEXT (current_function_decl)) == FUNCTION_DECL)
	current_function_decl = DECL_CONTEXT (current_function_decl);
      else
	current_function_decl = NULL_TREE;

      if (type != previous_class_type || current_class_depth > 1)
	{
	  build_mi_matrix (type);
	  push_class_decls (type);
	  free_mi_matrix ();
	}
      else
	{
	  tree item;

	  /* Hooray, we successfully cached; let's just install the
	     cached class_shadowed list, and walk through it to get the
	     IDENTIFIER_TYPE_VALUEs correct.  */
	  set_class_shadows (previous_class_values);
	  for (item = previous_class_values; item; item = TREE_CHAIN (item))
	    {
	      tree id = TREE_PURPOSE (item);
	      tree decl = IDENTIFIER_CLASS_VALUE (id);

	      if (TREE_CODE (decl) == TYPE_DECL)
		set_identifier_type_value (id, TREE_TYPE (decl));
	    }
	  unuse_fields (type);
	}

      for (tags = CLASSTYPE_TAGS (type); tags; tags = TREE_CHAIN (tags))
	{
	  TREE_NONLOCAL_FLAG (TREE_VALUE (tags)) = 1;
	  if (! TREE_PURPOSE (tags))
	    continue;
	  pushtag (TREE_PURPOSE (tags), TREE_VALUE (tags), 0);
	}

      current_function_decl = this_fndecl;
    }
}
 
/* Get out of the current class scope. If we were in a class scope
   previously, that is the one popped to.  The flag MODIFY tells whether
   the current scope declarations needs to be modified as a result of
   popping to the previous scope.  0 is used for class definitions.  */

void
popclass (modify)
     int modify;
{
  if (modify < 0)
    {
      /* Back this old class out completely.  */
      tree tags = CLASSTYPE_TAGS (previous_class_type);
      tree t;

      /* This code can be seen as a cache miss.  When we've cached a
	 class' scope's bindings and we can't use them, we need to reset
	 them.  This is it!  */
      for (t = previous_class_values; t; t = TREE_CHAIN (t))
	IDENTIFIER_CLASS_VALUE (TREE_PURPOSE (t)) = NULL_TREE;
      while (tags)
	{
	  TREE_NONLOCAL_FLAG (TREE_VALUE (tags)) = 0;
	  tags = TREE_CHAIN (tags);
	}
      goto ret;
    }

  if (modify)
    {
      /* Just remove from this class what didn't make
	 it into IDENTIFIER_CLASS_VALUE.  */
      tree tags = CLASSTYPE_TAGS (current_class_type);

      while (tags)
	{
	  TREE_NONLOCAL_FLAG (TREE_VALUE (tags)) = 0;
	  tags = TREE_CHAIN (tags);
	}
    }

  /* Force clearing of IDENTIFIER_CLASS_VALUEs after a class definition,
     since not all class decls make it there currently.  */
  poplevel_class (! modify);

  /* Since poplevel_class does the popping of class decls nowadays,
     this really only frees the obstack used for these decls.
     That's why it had to be moved down here.  */
  if (modify)
    pop_class_decls ();

  current_class_depth--;
  current_class_type = *--current_class_stack;
  current_class_name = *--current_class_stack;

  pop_memoized_context (modify);

 ret:
  ;
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

  if (type == NULL_TREE || type == error_mark_node || ! IS_AGGR_TYPE (type)
      || TREE_CODE (type) == TEMPLATE_TYPE_PARM)
    return;
  
  context = DECL_CONTEXT (TYPE_MAIN_DECL (type));

  if (context && TREE_CODE (context) == RECORD_TYPE)
    push_nested_class (context, 2);
  pushclass (type, modify);
}

/* Undoes a push_nested_class call.  MODIFY is passed on to popclass.  */

void
pop_nested_class (modify)
     int modify;
{
  tree context = DECL_CONTEXT (TYPE_MAIN_DECL (current_class_type));

  popclass (modify);
  if (context && TREE_CODE (context) == RECORD_TYPE)
    pop_nested_class (modify);
}

/* Set global variables CURRENT_LANG_NAME to appropriate value
   so that behavior of name-mangling machinery is correct.  */

void
push_lang_context (name)
     tree name;
{
  *current_lang_stack++ = current_lang_name;
  if (current_lang_stack >= current_lang_base + current_lang_stacksize)
    {
      current_lang_base
	= (tree *)xrealloc (current_lang_base,
			    sizeof (tree) * (current_lang_stacksize + 10));
      current_lang_stack = current_lang_base + current_lang_stacksize;
      current_lang_stacksize += 10;
    }

  if (name == lang_name_cplusplus)
    {
      strict_prototype = strict_prototypes_lang_cplusplus;
      current_lang_name = name;
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
  current_lang_name = *--current_lang_stack;
  if (current_lang_name == lang_name_cplusplus)
    strict_prototype = strict_prototypes_lang_cplusplus;
  else if (current_lang_name == lang_name_c)
    strict_prototype = strict_prototypes_lang_c;
}

/* Type instantiation routines.  */

/* This function will instantiate the type of the expression given in
   RHS to match the type of LHSTYPE.  If errors exist, then return
   error_mark_node.  If only complain is COMPLAIN is set.  If we are
   not complaining, never modify rhs, as overload resolution wants to
   try many possible instantiations, in hopes that at least one will
   work.

   This function is used in build_modify_expr, convert_arguments,
   build_c_cast, and compute_conversion_costs.  */

tree
instantiate_type (lhstype, rhs, complain)
     tree lhstype, rhs;
     int complain;
{
  tree explicit_targs = NULL_TREE;

  if (TREE_CODE (lhstype) == UNKNOWN_TYPE)
    {
      if (complain)
	error ("not enough type information");
      return error_mark_node;
    }

  if (TREE_TYPE (rhs) != NULL_TREE && ! (type_unknown_p (rhs)))
    return rhs;

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
				    TREE_OPERAND (rhs, 0), complain);
	if (new_rhs == error_mark_node)
	  return error_mark_node;

	TREE_TYPE (rhs) = lhstype;
	TREE_OPERAND (rhs, 0) = new_rhs;
	return rhs;
      }

    case NOP_EXPR:
      rhs = copy_node (TREE_OPERAND (rhs, 0));
      TREE_TYPE (rhs) = unknown_type_node;
      return instantiate_type (lhstype, rhs, complain);

    case COMPONENT_REF:
      {
	tree field = TREE_OPERAND (rhs, 1);
	if (TREE_CODE (field) == TREE_LIST)
	  {
	    tree function = instantiate_type (lhstype, field, complain);
	    if (function == error_mark_node)
	      return error_mark_node;
	    my_friendly_assert (TREE_CODE (function) == FUNCTION_DECL, 185);
	    if (DECL_VINDEX (function))
	      {
		tree base = TREE_OPERAND (rhs, 0);
		tree base_ptr = build_unary_op (ADDR_EXPR, base, 0);
		if (base_ptr == error_mark_node)
		  return error_mark_node;
		base_ptr = convert_pointer_to (DECL_CONTEXT (function), base_ptr);
		if (base_ptr == error_mark_node)
		  return error_mark_node;
		return build_vfn_ref (&base_ptr, base, DECL_VINDEX (function));
	      }
	    mark_used (function);
	    return function;
	  }

	my_friendly_assert (TREE_CODE (field) == FIELD_DECL, 178);
	my_friendly_assert (!(TREE_CODE (TREE_TYPE (field)) == FUNCTION_TYPE
			      || TREE_CODE (TREE_TYPE (field)) == METHOD_TYPE),
			    179);

	TREE_TYPE (rhs) = lhstype;
	/* First look for an exact match  */

	while (field && TREE_TYPE (field) != lhstype)
	  field = DECL_CHAIN (field);
	if (field)
	  {
	    TREE_OPERAND (rhs, 1) = field;
	    mark_used (field);
	    return rhs;
	  }

	/* No exact match found, look for a compatible function.  */
	field = TREE_OPERAND (rhs, 1);
	while (field && ! comptypes (lhstype, TREE_TYPE (field), 0))
	  field = DECL_CHAIN (field);
	if (field)
	  {
	    TREE_OPERAND (rhs, 1) = field;
	    field = DECL_CHAIN (field);
	    while (field && ! comptypes (lhstype, TREE_TYPE (field), 0))
	      field = DECL_CHAIN (field);
	    if (field)
	      {
		if (complain)
		  error ("ambiguous overload for COMPONENT_REF requested");
		return error_mark_node;
	      }
	  }
	else
	  {
	    if (complain)
	      error ("no appropriate overload exists for COMPONENT_REF");
	    return error_mark_node;
	  }
	return rhs;
      }

    case TEMPLATE_ID_EXPR:
      {
	explicit_targs = TREE_OPERAND (rhs, 1);
	rhs = TREE_OPERAND (rhs, 0);
      }
    /* fall through */

    case TREE_LIST:
      {
	tree elem, baselink, name;
	int globals = overloaded_globals_p (rhs);

	/* First look for an exact match.  Search either overloaded
	   functions or member functions.  May have to undo what
	   `default_conversion' might do to lhstype.  */

	if (TYPE_PTRMEMFUNC_P (lhstype))
	  lhstype = TYPE_PTRMEMFUNC_FN_TYPE (lhstype);

	if (TREE_CODE (lhstype) == POINTER_TYPE)
	  if (TREE_CODE (TREE_TYPE (lhstype)) == FUNCTION_TYPE
	      || TREE_CODE (TREE_TYPE (lhstype)) == METHOD_TYPE)
	    lhstype = TREE_TYPE (lhstype);
	  else
	    {
	      if (complain)
		error ("invalid type combination for overload");
	      return error_mark_node;
	    }

	if (TREE_CODE (lhstype) != FUNCTION_TYPE && globals > 0)
	  {
	    if (complain)
	      cp_error ("cannot resolve overloaded function `%D' based on non-function type",
		     TREE_PURPOSE (rhs));
	    return error_mark_node;
	  }

	if (globals > 0)
	  {
	    elem = get_first_fn (rhs);
	    /* If there are explicit_targs, only a template function
	       can match.  */
	    if (explicit_targs == NULL_TREE)
	      while (elem)
		if (! comptypes (lhstype, TREE_TYPE (elem), 1))
		  elem = DECL_CHAIN (elem);
		else
		  {
		    mark_used (elem);
		    return elem;
		  }

	    /* No exact match found, look for a compatible template.  */
	    {
	      tree save_elem = 0;
	      for (elem = get_first_fn (rhs); elem; elem = DECL_CHAIN (elem))
		if (TREE_CODE (elem) == TEMPLATE_DECL)
		  {
		    int n = DECL_NTPARMS (elem);
		    tree t = make_scratch_vec (n);
		    int i, d = 0;
		    i = type_unification
		      (DECL_INNERMOST_TEMPLATE_PARMS (elem), 
		       &TREE_VEC_ELT (t, 0), TYPE_ARG_TYPES (TREE_TYPE (elem)),
		       TYPE_ARG_TYPES (lhstype), explicit_targs, &d,
		       1, 1);
		    if (i == 0)
		      {
			if (save_elem)
			  {
			    cp_error ("ambiguous template instantiation converting to `%#T'", lhstype);
			    return error_mark_node;
			  }
			save_elem = instantiate_template (elem, t);
			/* Check the return type.  */
			if (! comptypes (TREE_TYPE (lhstype),
					 TREE_TYPE (TREE_TYPE (save_elem)), 1))
			  save_elem = 0;
		      }
		  }
	      if (save_elem)
		{
		  mark_used (save_elem);
		  return save_elem;
		}
	    }

	    /* If there are explicit_targs, only a template function
	       can match.  */
	    if (explicit_targs == NULL_TREE) 
	      {
		/* No match found, look for a compatible function.  */
		elem = get_first_fn (rhs);
		while (elem && comp_target_types (lhstype,
						  TREE_TYPE (elem), 1) <= 0)
		  elem = DECL_CHAIN (elem);
		if (elem)
		  {
		    tree save_elem = elem;
		    elem = DECL_CHAIN (elem);
		    while (elem 
			   && comp_target_types (lhstype,
						 TREE_TYPE (elem), 0) <= 0)
		      elem = DECL_CHAIN (elem);
		    if (elem)
		      {
			if (complain)
			  {
			    cp_error 
			      ("cannot resolve overload to target type `%#T'",
			       lhstype);
			    cp_error_at ("  ambiguity between `%#D'",
					 save_elem); 
			    cp_error_at ("  and `%#D', at least", elem);
			  }
			return error_mark_node;
		      }
		    mark_used (save_elem);
		    return save_elem;
		  }
	      }
	    if (complain)
	      {
		cp_error ("cannot resolve overload to target type `%#T'",
			  lhstype);
		cp_error 
		  ("  because no suitable overload of function `%D' exists",
		   TREE_PURPOSE (rhs));
	      }
	    return error_mark_node;
	  }

	if (TREE_NONLOCAL_FLAG (rhs))
	  {
	    /* Got to get it as a baselink.  */
	    rhs = lookup_fnfields (TYPE_BINFO (current_class_type),
				   TREE_PURPOSE (rhs), 0);
	  }
	else
	  {
	    my_friendly_assert (TREE_CHAIN (rhs) == NULL_TREE, 181);
	    if (TREE_CODE (TREE_VALUE (rhs)) == TREE_LIST)
	      rhs = TREE_VALUE (rhs);
	    my_friendly_assert (TREE_CODE (TREE_VALUE (rhs)) == FUNCTION_DECL,
				182);
	  }

	for (baselink = rhs; baselink;
	     baselink = next_baselink (baselink))
	  {
	    elem = TREE_VALUE (baselink);
	    while (elem)
	      if (comptypes (lhstype, TREE_TYPE (elem), 1))
		{
		  mark_used (elem);
		  return elem;
		}
	      else
		elem = DECL_CHAIN (elem);
	  }

	/* No exact match found, look for a compatible method.  */
	for (baselink = rhs; baselink;
	     baselink = next_baselink (baselink))
	  {
	    elem = TREE_VALUE (baselink);
	    while (elem && comp_target_types (lhstype,
					      TREE_TYPE (elem), 1) <= 0)
	      elem = DECL_CHAIN (elem);
	    if (elem)
	      {
		tree save_elem = elem;
		elem = DECL_CHAIN (elem);
		while (elem && comp_target_types (lhstype,
						  TREE_TYPE (elem), 0) <= 0)
		  elem = DECL_CHAIN (elem);
		if (elem)
		  {
		    if (complain)
		      error ("ambiguous overload for overloaded method requested");
		    return error_mark_node;
		  }
		mark_used (save_elem);
		return save_elem;
	      }
	    name = DECL_NAME (TREE_VALUE (rhs));
#if 0
	    if (TREE_CODE (lhstype) == FUNCTION_TYPE && globals < 0)
	      {
		/* Try to instantiate from non-member functions.  */
		rhs = lookup_name_nonclass (name);
		if (rhs && TREE_CODE (rhs) == TREE_LIST)
		  {
		    /* This code seems to be missing a `return'.  */
		    my_friendly_abort (4);
		    instantiate_type (lhstype, rhs, complain);
		  }
	      }
#endif
	  }
	if (complain)
	  cp_error ("no compatible member functions named `%D'", name);
	return error_mark_node;
      }

    case CALL_EXPR:
      /* This is too hard for now.  */
      my_friendly_abort (183);
      return error_mark_node;

    case PLUS_EXPR:
    case MINUS_EXPR:
    case COMPOUND_EXPR:
      TREE_OPERAND (rhs, 0)
	= instantiate_type (lhstype, TREE_OPERAND (rhs, 0), complain);
      if (TREE_OPERAND (rhs, 0) == error_mark_node)
	return error_mark_node;
      TREE_OPERAND (rhs, 1)
	= instantiate_type (lhstype, TREE_OPERAND (rhs, 1), complain);
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
	= instantiate_type (lhstype, TREE_OPERAND (rhs, 1), complain);
      if (TREE_OPERAND (rhs, 1) == error_mark_node)
	return error_mark_node;
      TREE_OPERAND (rhs, 2)
	= instantiate_type (lhstype, TREE_OPERAND (rhs, 2), complain);
      if (TREE_OPERAND (rhs, 2) == error_mark_node)
	return error_mark_node;

      TREE_TYPE (rhs) = lhstype;
      return rhs;

    case MODIFY_EXPR:
      TREE_OPERAND (rhs, 1)
	= instantiate_type (lhstype, TREE_OPERAND (rhs, 1), complain);
      if (TREE_OPERAND (rhs, 1) == error_mark_node)
	return error_mark_node;

      TREE_TYPE (rhs) = lhstype;
      return rhs;
      
    case ADDR_EXPR:
      if (TYPE_PTRMEMFUNC_P (lhstype))
	lhstype = TYPE_PTRMEMFUNC_FN_TYPE (lhstype);
      else if (TREE_CODE (lhstype) != POINTER_TYPE)
	{
	  if (complain)
	    error ("type for resolving address of overloaded function must be pointer type");
	  return error_mark_node;
	}
      {
	tree fn = instantiate_type (TREE_TYPE (lhstype), TREE_OPERAND (rhs, 0), complain);
	if (fn == error_mark_node)
	  return error_mark_node;
	mark_addressable (fn);
	TREE_TYPE (rhs) = lhstype;
	TREE_OPERAND (rhs, 0) = fn;
	TREE_CONSTANT (rhs) = staticp (fn);
	if (TREE_CODE (lhstype) == POINTER_TYPE
	    && TREE_CODE (TREE_TYPE (lhstype)) == METHOD_TYPE)
	  {
	    build_ptrmemfunc_type (lhstype);
	    rhs = build_ptrmemfunc (lhstype, rhs, 0);
	  }
      }
      return rhs;

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
	 && TYPE_VIRTUAL_P (BINFO_TYPE (BINFO_BASETYPE (binfo, 0)))
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

/* Push an obstack which is sufficiently long-lived to hold such class
   decls that may be cached in the previous_class_values list.  For now, let's
   use the permanent obstack, later we may create a dedicated obstack just
   for this purpose.  The effect is undone by pop_obstacks.  */

void
maybe_push_cache_obstack ()
{
  push_obstacks_nochange ();
  if (current_class_depth == 1)
    current_obstack = &permanent_obstack;
}

/* Build a dummy reference to ourselves so Derived::Base (and A::A) works,
   according to [class]:
                                          The class-name is also inserted
   into  the scope of the class itself.  For purposes of access checking,
   the inserted class name is treated as if it were a public member name.  */

tree
build_self_reference ()
{
  tree name = constructor_name (current_class_type);
  tree value = build_lang_decl (TYPE_DECL, name, current_class_type);
  DECL_NONLOCAL (value) = 1;
  DECL_CONTEXT (value) = current_class_type;
  DECL_CLASS_CONTEXT (value) = current_class_type;
  CLASSTYPE_LOCAL_TYPEDECLS (current_class_type) = 1;
  DECL_ARTIFICIAL (value) = 1;

  pushdecl_class_level (value);
  return value;
}
