/* Handle initialization things in C++.
   Copyright (C) 1987, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
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
#include "rtl.h"
#include "cp-tree.h"
#include "flags.h"
#include "output.h"
#include "except.h"
#include "expr.h"
#include "toplev.h"
#include "ggc.h"

static void expand_aggr_vbase_init_1 PARAMS ((tree, tree, tree, tree));
static void construct_virtual_bases PARAMS ((tree, tree, tree, tree, tree));
static void expand_aggr_init_1 PARAMS ((tree, tree, tree, tree, int));
static void expand_default_init PARAMS ((tree, tree, tree, tree, int));
static tree build_vec_delete_1 PARAMS ((tree, tree, tree, tree, int));
static void perform_member_init PARAMS ((tree, tree, tree, int));
static void sort_base_init PARAMS ((tree, tree *, tree *));
static tree build_builtin_delete_call PARAMS ((tree));
static int member_init_ok_or_else PARAMS ((tree, tree, const char *));
static void expand_virtual_init PARAMS ((tree, tree));
static tree sort_member_init PARAMS ((tree));
static tree initializing_context PARAMS ((tree));
static void expand_cleanup_for_base PARAMS ((tree, tree));
static tree get_temp_regvar PARAMS ((tree, tree));
static tree dfs_initialize_vtbl_ptrs PARAMS ((tree, void *));
static tree build_new_1	PARAMS ((tree));
static tree get_cookie_size PARAMS ((tree));

/* Set up local variable for this file.  MUST BE CALLED AFTER
   INIT_DECL_PROCESSING.  */

static tree BI_header_type, BI_header_size;

void init_init_processing ()
{
  tree fields[1];

  minus_one_node = build_int_2 (-1, -1);

  /* Define the structure that holds header information for
     arrays allocated via operator new.  */
  BI_header_type = make_aggr_type (RECORD_TYPE);
  nelts_identifier = get_identifier ("nelts");
  fields[0] = build_lang_decl (FIELD_DECL, nelts_identifier, sizetype);

  /* Use the biggest alignment supported by the target to prevent operator
     new from returning misaligned pointers. */
  TYPE_ALIGN (BI_header_type) = BIGGEST_ALIGNMENT;
  finish_builtin_type (BI_header_type, "__new_cookie", fields,
		       0, BI_header_type);
  BI_header_size = size_in_bytes (BI_header_type);

  ggc_add_tree_root (&BI_header_type, 1);
  ggc_add_tree_root (&BI_header_size, 1);
}

/* Called from initialize_vtbl_ptrs via dfs_walk.  */

static tree
dfs_initialize_vtbl_ptrs (binfo, data)
     tree binfo;
     void *data;
{
  if (!BINFO_PRIMARY_MARKED_P (binfo) 
      && CLASSTYPE_VFIELDS (BINFO_TYPE (binfo)))
    {
      tree base_ptr = TREE_VALUE ((tree) data);

      if (TREE_VIA_VIRTUAL (binfo))
	base_ptr = convert_pointer_to_vbase (BINFO_TYPE (binfo),
					     base_ptr);
      else
	base_ptr 
	  = build_vbase_path (PLUS_EXPR, 
			      build_pointer_type (BINFO_TYPE (binfo)),
			      base_ptr,
			      binfo,
			      /*nonnull=*/1);

      expand_virtual_init (binfo, base_ptr);
    }

  SET_BINFO_MARKED (binfo);

  return NULL_TREE;
}

/* Initialize all the vtable pointers for the hierarchy dominated by
   TYPE. */

void
initialize_vtbl_ptrs (type, addr)
     tree type;
     tree addr;
{
  tree list = build_tree_list (type, addr);

  /* Walk through the hierarchy, initializing the vptr in each base
     class.  We do these in pre-order because under the new ABI we
     can't find the virtual bases for a class until we've initialized
     the vtbl for that class.  */
  dfs_walk_real (TYPE_BINFO (type), dfs_initialize_vtbl_ptrs, 
		 NULL, dfs_unmarked_real_bases_queue_p, list);
  dfs_walk (TYPE_BINFO (type), dfs_unmark,
	    dfs_marked_real_bases_queue_p, type);
  if (TYPE_USES_VIRTUAL_BASECLASSES (type))
    expand_indirect_vtbls_init (TYPE_BINFO (type), addr);
}


/* 348 - 351 */
/* Subroutine of emit_base_init.  */

static void
perform_member_init (member, name, init, explicit)
     tree member, name, init;
     int explicit;
{
  tree decl;
  tree type = TREE_TYPE (member);

  decl = build_component_ref (current_class_ref, name, NULL_TREE, explicit);

  if (decl == error_mark_node)
    return;

  /* Deal with this here, as we will get confused if we try to call the
     assignment op for an anonymous union.  This can happen in a
     synthesized copy constructor.  */
  if (ANON_AGGR_TYPE_P (type))
    {
      init = build (INIT_EXPR, type, decl, TREE_VALUE (init));
      finish_expr_stmt (init);
    }
  else if (TYPE_NEEDS_CONSTRUCTING (type)
	   || (init && TYPE_HAS_CONSTRUCTOR (type)))
    {
      /* Since `init' is already a TREE_LIST on the current_member_init_list,
	 only build it into one if we aren't already a list.  */
      if (init != NULL_TREE && TREE_CODE (init) != TREE_LIST)
	init = build_tree_list (NULL_TREE, init);

      if (explicit
	  && TREE_CODE (type) == ARRAY_TYPE
	  && init != NULL_TREE
	  && TREE_CHAIN (init) == NULL_TREE
	  && TREE_CODE (TREE_TYPE (TREE_VALUE (init))) == ARRAY_TYPE)
	{
	  /* Initialization of one array from another.  */
	  finish_expr_stmt 
	    (build_vec_init (TREE_OPERAND (decl, 1), decl,
			     array_type_nelts (type), TREE_VALUE (init), 1));
	}
      else
	finish_expr_stmt (build_aggr_init (decl, init, 0));
    }
  else
    {
      if (init == NULL_TREE)
	{
	  if (explicit)
	    {
	      /* default-initialization.  */
	      if (AGGREGATE_TYPE_P (type))
		{
		  /* This is a default initialization of an aggregate,
		     but not one of non-POD class type.  We cleverly
		     notice that the initialization rules in such a
		     case are the same as for initialization with an
		     empty brace-initialization list.  We don't want
		     to call build_modify_expr as that will go looking
		     for constructors and such.  */
		  tree e = build (CONSTRUCTOR, type, NULL_TREE, NULL_TREE);
		  TREE_SIDE_EFFECTS (e) = 1;
		  finish_expr_stmt (build (INIT_EXPR, type, decl, e));
		}
 	      else if (TREE_CODE (type) == REFERENCE_TYPE)
		cp_error ("default-initialization of `%#D', which has reference type",
			  member);
	      else
		init = integer_zero_node;
	    }
	  /* member traversal: note it leaves init NULL */
	  else if (TREE_CODE (TREE_TYPE (member)) == REFERENCE_TYPE)
	    cp_pedwarn ("uninitialized reference member `%D'", member);
	}
      else if (TREE_CODE (init) == TREE_LIST)
	{
	  /* There was an explicit member initialization.  Do some
	     work in that case.  */
	  if (TREE_CHAIN (init))
	    {
	      warning ("initializer list treated as compound expression");
	      init = build_compound_expr (init);
	    }
	  else
	    init = TREE_VALUE (init);
	}

      if (init)
	finish_expr_stmt (build_modify_expr (decl, INIT_EXPR, init));
    }

  if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (type))
    {
      tree expr;

      expr = build_component_ref (current_class_ref, name, NULL_TREE,
				  explicit);
      expr = build_delete (type, expr, integer_zero_node,
			   LOOKUP_NONVIRTUAL|LOOKUP_DESTRUCTOR, 0);

      if (expr != error_mark_node)
	finish_subobject (expr);
    }
}

extern int warn_reorder;

/* Subroutine of emit_member_init.  */

static tree
sort_member_init (t)
     tree t;
{
  tree x, member, name, field;
  tree init_list = NULL_TREE;
  int last_pos = 0;
  tree last_field = NULL_TREE;

  for (member = TYPE_FIELDS (t); member ; member = TREE_CHAIN (member))
    {
      int pos;

      /* member could be, for example, a CONST_DECL for an enumerated
	 tag; we don't want to try to initialize that, since it already
	 has a value.  */
      if (TREE_CODE (member) != FIELD_DECL || !DECL_NAME (member))
	continue;

      for (x = current_member_init_list, pos = 0; x; x = TREE_CHAIN (x), ++pos)
	{
	  /* If we cleared this out, then pay no attention to it.  */
	  if (TREE_PURPOSE (x) == NULL_TREE)
	    continue;
	  name = TREE_PURPOSE (x);

	  if (TREE_CODE (name) == IDENTIFIER_NODE)
	    field = IDENTIFIER_CLASS_VALUE (name);
	  else
	    {
	      my_friendly_assert (TREE_CODE (name) == FIELD_DECL, 348); 
	      field = name;
	    }

	  /* If one member shadows another, get the outermost one.  */
	  if (TREE_CODE (field) == TREE_LIST)
	    field = TREE_VALUE (field);

	  if (field == member)
	    {
	      if (warn_reorder)
		{
		  if (pos < last_pos)
		    {
		      cp_warning_at ("member initializers for `%#D'", last_field);
		      cp_warning_at ("  and `%#D'", field);
		      warning ("  will be re-ordered to match declaration order");
		    }
		  last_pos = pos;
		  last_field = field;
		}

	      /* Make sure we won't try to work on this init again.  */
	      TREE_PURPOSE (x) = NULL_TREE;
	      x = build_tree_list (name, TREE_VALUE (x));
	      goto got_it;
	    }
	}

      /* If we didn't find MEMBER in the list, create a dummy entry
	 so the two lists (INIT_LIST and the list of members) will be
	 symmetrical.  */
      x = build_tree_list (NULL_TREE, NULL_TREE);
    got_it:
      init_list = chainon (init_list, x); 
    }

  /* Initializers for base members go at the end.  */
  for (x = current_member_init_list ; x ; x = TREE_CHAIN (x))
    {
      name = TREE_PURPOSE (x);
      if (name)
	{
	  if (purpose_member (name, init_list))
	    {
	      cp_error ("multiple initializations given for member `%D'",
			IDENTIFIER_CLASS_VALUE (name));
	      continue;
	    }
	      
	  init_list = chainon (init_list,
			       build_tree_list (name, TREE_VALUE (x)));
	  TREE_PURPOSE (x) = NULL_TREE;
	}
    }

  return init_list;
}

static void
sort_base_init (t, rbase_ptr, vbase_ptr)
     tree t, *rbase_ptr, *vbase_ptr;
{
  tree binfos = BINFO_BASETYPES (TYPE_BINFO (t));
  int n_baseclasses = binfos ? TREE_VEC_LENGTH (binfos) : 0;

  int i;
  tree x;
  tree last;

  /* For warn_reorder.  */
  int last_pos = 0;
  tree last_base = NULL_TREE;

  tree rbases = NULL_TREE;
  tree vbases = NULL_TREE;

  /* First walk through and splice out vbase and invalid initializers.
     Also replace names with binfos.  */

  last = tree_cons (NULL_TREE, NULL_TREE, current_base_init_list);
  for (x = TREE_CHAIN (last); x; x = TREE_CHAIN (x))
    {
      tree basetype = TREE_PURPOSE (x);
      tree binfo = NULL_TREE;

      if (basetype == NULL_TREE)
	{
	  /* Initializer for single base class.  Must not
	     use multiple inheritance or this is ambiguous.  */
	  switch (n_baseclasses)
	    {
	    case 0:
	      cp_error ("`%T' does not have a base class to initialize",
			current_class_type);
	      return;
	    case 1:
	      break;
	    default:
	      cp_error ("unnamed initializer ambiguous for `%T' which uses multiple inheritance",
			current_class_type);
	      return;
	    }
	  binfo = TREE_VEC_ELT (binfos, 0);
	}
      else if (is_aggr_type (basetype, 1))
	{
	  binfo = binfo_or_else (basetype, t);
	  if (binfo == NULL_TREE)
	    continue;

	  /* Virtual base classes are special cases.  Their initializers
	     are recorded with this constructor, and they are used when
	     this constructor is the top-level constructor called.  */
	  if (TREE_VIA_VIRTUAL (binfo))
	    {
	      tree v = BINFO_FOR_VBASE (BINFO_TYPE (binfo), t);
	      vbases = tree_cons (v, TREE_VALUE (x), vbases);
	      continue;
	    }
	  else
	    {
	      /* Otherwise, if it is not an immediate base class, complain.  */
	      for (i = n_baseclasses-1; i >= 0; i--)
		if (BINFO_TYPE (binfo) == BINFO_TYPE (TREE_VEC_ELT (binfos, i)))
		  break;
	      if (i < 0)
		{
		  cp_error ("`%T' is not an immediate base class of `%T'",
			    basetype, current_class_type);
		  continue;
		}
	    }
	}
      else
	my_friendly_abort (365);

      TREE_PURPOSE (x) = binfo;
      TREE_CHAIN (last) = x;
      last = x;
    }
  TREE_CHAIN (last) = NULL_TREE;

  /* Now walk through our regular bases and make sure they're initialized.  */

  for (i = 0; i < n_baseclasses; ++i)
    {
      tree base_binfo = TREE_VEC_ELT (binfos, i);
      int pos;

      if (TREE_VIA_VIRTUAL (base_binfo))
	continue;

      for (x = current_base_init_list, pos = 0; x; x = TREE_CHAIN (x), ++pos)
	{
	  tree binfo = TREE_PURPOSE (x);

	  if (binfo == NULL_TREE)
	    continue;

	  if (binfo == base_binfo)
	    {
	      if (warn_reorder)
		{
		  if (pos < last_pos)
		    {
		      cp_warning_at ("base initializers for `%#T'", last_base);
		      cp_warning_at ("  and `%#T'", BINFO_TYPE (binfo));
		      warning ("  will be re-ordered to match inheritance order");
		    }
		  last_pos = pos;
		  last_base = BINFO_TYPE (binfo);
		}

	      /* Make sure we won't try to work on this init again.  */
	      TREE_PURPOSE (x) = NULL_TREE;
	      x = build_tree_list (binfo, TREE_VALUE (x));
	      goto got_it;
	    }
	}

      /* If we didn't find BASE_BINFO in the list, create a dummy entry
	 so the two lists (RBASES and the list of bases) will be
	 symmetrical.  */
      x = build_tree_list (NULL_TREE, NULL_TREE);
    got_it:
      rbases = chainon (rbases, x);
    }

  *rbase_ptr = rbases;
  *vbase_ptr = vbases;
}

/* Perform whatever initializations have yet to be done on the base
   class of the class variable.  These actions are in the global
   variable CURRENT_BASE_INIT_LIST.  Such an action could be
   NULL_TREE, meaning that the user has explicitly called the base
   class constructor with no arguments.

   If there is a need for a call to a constructor, we must surround
   that call with a pushlevel/poplevel pair, since we are technically
   at the PARM level of scope.

   Argument IMMEDIATELY, if zero, forces a new sequence to be
   generated to contain these new insns, so it can be emitted later.
   This sequence is saved in the global variable BASE_INIT_EXPR.
   Otherwise, the insns are emitted into the current sequence.

   Note that emit_base_init does *not* initialize virtual base
   classes.  That is done specially, elsewhere.  */

tree
emit_base_init (t)
     tree t;
{
  tree member;
  tree mem_init_list;
  tree rbase_init_list, vbase_init_list;
  tree t_binfo = TYPE_BINFO (t);
  tree binfos = BINFO_BASETYPES (t_binfo);
  int i, n_baseclasses = binfos ? TREE_VEC_LENGTH (binfos) : 0;
  tree stmt_expr;
  tree compound_stmt;

  mem_init_list = sort_member_init (t);
  current_member_init_list = NULL_TREE;

  sort_base_init (t, &rbase_init_list, &vbase_init_list);
  current_base_init_list = NULL_TREE;

  begin_init_stmts (&stmt_expr, &compound_stmt);
  
  /* First, initialize the virtual base classes, if we are
     constructing the most-derived object.  */
  if (TYPE_USES_VIRTUAL_BASECLASSES (t))
    {
      tree first_arg = TREE_CHAIN (DECL_ARGUMENTS (current_function_decl));
      construct_virtual_bases (t, current_class_ref, current_class_ptr,
			       vbase_init_list, first_arg);
    }

  /* Now, perform initialization of non-virtual base classes.  */
  for (i = 0; i < n_baseclasses; i++)
    {
      tree base_binfo = TREE_VEC_ELT (binfos, i);
      tree init = void_list_node;

      if (TREE_VIA_VIRTUAL (base_binfo))
	continue;

      my_friendly_assert (BINFO_INHERITANCE_CHAIN (base_binfo) == t_binfo,
			  999);

      if (TREE_PURPOSE (rbase_init_list))
	init = TREE_VALUE (rbase_init_list);
      else if (TYPE_NEEDS_CONSTRUCTING (BINFO_TYPE (base_binfo)))
	{
	  init = NULL_TREE;
	  if (extra_warnings && copy_args_p (current_function_decl))
	    cp_warning ("base class `%#T' should be explicitly initialized in the copy constructor",
			BINFO_TYPE (base_binfo));
	}

      if (init != void_list_node)
	{
	  member = convert_pointer_to_real (base_binfo, current_class_ptr);
	  expand_aggr_init_1 (base_binfo, NULL_TREE,
			      build_indirect_ref (member, NULL_PTR), init,
			      LOOKUP_NORMAL);
	}

      expand_cleanup_for_base (base_binfo, NULL_TREE);
      rbase_init_list = TREE_CHAIN (rbase_init_list);
    }

  /* Initialize the vtable pointers for the class.  */
  initialize_vtbl_ptrs (t, current_class_ptr);

  for (member = TYPE_FIELDS (t); member; member = TREE_CHAIN (member))
    {
      tree init, name;
      int from_init_list;

      /* member could be, for example, a CONST_DECL for an enumerated
	 tag; we don't want to try to initialize that, since it already
	 has a value.  */
      if (TREE_CODE (member) != FIELD_DECL || !DECL_NAME (member))
	continue;

      /* See if we had a user-specified member initialization.  */
      if (TREE_PURPOSE (mem_init_list))
	{
	  name = TREE_PURPOSE (mem_init_list);
	  init = TREE_VALUE (mem_init_list);
	  from_init_list = 1;

	  my_friendly_assert (TREE_CODE (name) == IDENTIFIER_NODE
			      || TREE_CODE (name) == FIELD_DECL, 349);
	}
      else
	{
	  name = DECL_NAME (member);
	  init = DECL_INITIAL (member);

	  from_init_list = 0;

	  /* Effective C++ rule 12.  */
	  if (warn_ecpp && init == NULL_TREE
	      && !DECL_ARTIFICIAL (member)
	      && TREE_CODE (TREE_TYPE (member)) != ARRAY_TYPE)
	    cp_warning ("`%D' should be initialized in the member initialization list", member);	    
	}

      perform_member_init (member, name, init, from_init_list);
      mem_init_list = TREE_CHAIN (mem_init_list);
    }

  /* Now initialize any members from our bases.  */
  while (mem_init_list)
    {
      tree name, init, field;

      if (TREE_PURPOSE (mem_init_list))
	{
	  name = TREE_PURPOSE (mem_init_list);
	  init = TREE_VALUE (mem_init_list);

	  if (TREE_CODE (name) == IDENTIFIER_NODE)
	    field = IDENTIFIER_CLASS_VALUE (name);
	  else
	    field = name;

	  /* If one member shadows another, get the outermost one.  */
	  if (TREE_CODE (field) == TREE_LIST)
	    {
	      field = TREE_VALUE (field);
	      if (decl_type_context (field) != current_class_type)
		cp_error ("field `%D' not in immediate context", field);
	    }

	  perform_member_init (field, name, init, 1);
	}
      mem_init_list = TREE_CHAIN (mem_init_list);
    }

  /* All the implicit try blocks we built up will be zapped
     when we come to a real binding contour boundary.  */
  return finish_init_stmts (stmt_expr, compound_stmt);
}

/* Check that all fields are properly initialized after
   an assignment to `this'.  Called only when such an assignment
   is actually noted.  */

void
check_base_init (t)
     tree t;
{
  tree member;
  for (member = TYPE_FIELDS (t); member; member = TREE_CHAIN (member))
    if (DECL_NAME (member) && TREE_USED (member))
      cp_error ("field `%D' used before initialized (after assignment to `this')",
		member);
}

/* This code sets up the virtual function tables appropriate for
   the pointer DECL.  It is a one-ply initialization.

   BINFO is the exact type that DECL is supposed to be.  In
   multiple inheritance, this might mean "C's A" if C : A, B.  */

static void
expand_virtual_init (binfo, decl)
     tree binfo, decl;
{
  tree type = BINFO_TYPE (binfo);
  tree vtbl, vtbl_ptr;
  tree vtype, vtype_binfo;

  /* Compute the location of the vtable.  */
  vtype = DECL_CONTEXT (TYPE_VFIELD (type));
  vtype_binfo = get_binfo (vtype, TREE_TYPE (TREE_TYPE (decl)), 0);
  vtbl = BINFO_VTABLE (binfo_value (DECL_FIELD_CONTEXT (TYPE_VFIELD (type)), binfo));

  if (TREE_CODE (vtbl) == VAR_DECL)
    {
      assemble_external (vtbl);
      TREE_USED (vtbl) = 1;
      vtbl = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (vtbl)), vtbl);
    }
  else
    /* Under the new ABI, secondary vtables are stored with the
       primary vtable.  So, the BINFO_VTABLE may be an expression for
       computing the secondary vtable, rather than the secondary
       vtable itself.  */
    my_friendly_assert (merge_primary_and_secondary_vtables_p (), 
			20000220);

  /* Under the new ABI, we need to point into the middle of the
     vtable.  */
  if (vbase_offsets_in_vtable_p ())
    vtbl = build (PLUS_EXPR, TREE_TYPE (vtbl), vtbl, 
		  size_extra_vtbl_entries (binfo));

  /* Compute the location of the vtpr.  */
  decl = convert_pointer_to_real (vtype_binfo, decl);
  vtbl_ptr = build_vfield_ref (build_indirect_ref (decl, NULL_PTR), vtype);
  if (vtbl_ptr == error_mark_node)
    return;

  /* Assign the vtable to the vptr.  */
  vtbl = convert_force (TREE_TYPE (vtbl_ptr), vtbl, 0);
  finish_expr_stmt (build_modify_expr (vtbl_ptr, NOP_EXPR, vtbl));
}

/* If an exception is thrown in a constructor, those base classes already
   constructed must be destroyed.  This function creates the cleanup
   for BINFO, which has just been constructed.  If FLAG is non-NULL,
   it is a DECL which is non-zero when this base needs to be
   destroyed.  */

static void
expand_cleanup_for_base (binfo, flag)
     tree binfo;
     tree flag;
{
  tree expr;

  if (TYPE_HAS_TRIVIAL_DESTRUCTOR (BINFO_TYPE (binfo)))
    return;

  /* Call the destructor.  */
  expr = (build_scoped_method_call
	  (current_class_ref, binfo, dtor_identifier,
	   build_tree_list (NULL_TREE, integer_zero_node)));
  if (flag)
    expr = fold (build (COND_EXPR, void_type_node,
			truthvalue_conversion (flag),
			expr, integer_zero_node));

  finish_subobject (expr);
}

/* Subroutine of `expand_aggr_vbase_init'.
   BINFO is the binfo of the type that is being initialized.
   INIT_LIST is the list of initializers for the virtual baseclass.  */

static void
expand_aggr_vbase_init_1 (binfo, exp, addr, init_list)
     tree binfo, exp, addr, init_list;
{
  tree init = purpose_member (binfo, init_list);
  tree ref = build_indirect_ref (addr, NULL_PTR);

  if (init)
    init = TREE_VALUE (init);
  /* Call constructors, but don't set up vtables.  */
  expand_aggr_init_1 (binfo, exp, ref, init, LOOKUP_COMPLAIN);
}

/* Construct the virtual base-classes of THIS_REF (whose address is
   THIS_PTR).  The object has the indicated TYPE.  The construction
   actually takes place only if FLAG is non-zero.  INIT_LIST is list
   of initializations for constructors to perform.  */

static void
construct_virtual_bases (type, this_ref, this_ptr, init_list, flag)
     tree type;
     tree this_ref;
     tree this_ptr;
     tree init_list;
     tree flag;
{
  tree vbases;

  /* If there are no virtual baseclasses, we shouldn't even be here.  */
  my_friendly_assert (TYPE_USES_VIRTUAL_BASECLASSES (type), 19990621);

  /* First set the pointers in our object that tell us where to find
     our virtual baseclasses.  */
  if (!vbase_offsets_in_vtable_p ())
    {
      tree if_stmt;
      tree result;

      if_stmt = begin_if_stmt ();
      finish_if_stmt_cond (flag, if_stmt);
      result = init_vbase_pointers (type, this_ptr);
      if (result)
	finish_expr_stmt (build_compound_expr (result));
      finish_then_clause (if_stmt);
      finish_if_stmt ();
    }

  /* Now, run through the baseclasses, initializing each.  */ 
  for (vbases = CLASSTYPE_VBASECLASSES (type); vbases;
       vbases = TREE_CHAIN (vbases))
    {
      tree inner_if_stmt;
      tree compound_stmt;
      tree exp;

      /* If there are virtual base classes with destructors, we need to
	 emit cleanups to destroy them if an exception is thrown during
	 the construction process.  These exception regions (i.e., the
	 period during which the cleanups must occur) begin from the time
	 the construction is complete to the end of the function.  If we
	 create a conditional block in which to initialize the
	 base-classes, then the cleanup region for the virtual base begins
	 inside a block, and ends outside of that block.  This situation
	 confuses the sjlj exception-handling code.  Therefore, we do not
	 create a single conditional block, but one for each
	 initialization.  (That way the cleanup regions always begin
	 in the outer block.)  We trust the back-end to figure out
	 that the FLAG will not change across initializations, and
	 avoid doing multiple tests.  */
      inner_if_stmt = begin_if_stmt ();
      finish_if_stmt_cond (flag, inner_if_stmt);
      compound_stmt = begin_compound_stmt (/*has_no_scope=*/1);

      /* Compute the location of the virtual base.  If we're
	 constructing virtual bases, then we must be the most derived
	 class.  Therefore, we don't have to look up the virtual base;
	 we already know where it is.  */
      exp = build (PLUS_EXPR,
		   TREE_TYPE (this_ptr),
		   this_ptr,
		   fold (build1 (NOP_EXPR, TREE_TYPE (this_ptr),
				 BINFO_OFFSET (vbases))));
      exp = build1 (NOP_EXPR, 
		    build_pointer_type (BINFO_TYPE (vbases)), 
		    exp);

      expand_aggr_vbase_init_1 (vbases, this_ref, exp, init_list);
      finish_compound_stmt (/*has_no_scope=*/1, compound_stmt);
      finish_then_clause (inner_if_stmt);
      finish_if_stmt ();
      
      expand_cleanup_for_base (vbases, flag);
    }
}

/* Find the context in which this FIELD can be initialized.  */

static tree
initializing_context (field)
     tree field;
{
  tree t = DECL_CONTEXT (field);

  /* Anonymous union members can be initialized in the first enclosing
     non-anonymous union context.  */
  while (t && ANON_AGGR_TYPE_P (t))
    t = TYPE_CONTEXT (t);
  return t;
}

/* Function to give error message if member initialization specification
   is erroneous.  FIELD is the member we decided to initialize.
   TYPE is the type for which the initialization is being performed.
   FIELD must be a member of TYPE.
   
   MEMBER_NAME is the name of the member.  */

static int
member_init_ok_or_else (field, type, member_name)
     tree field;
     tree type;
     const char *member_name;
{
  if (field == error_mark_node)
    return 0;
  if (field == NULL_TREE || initializing_context (field) != type)
    {
      cp_error ("class `%T' does not have any field named `%s'", type,
		member_name);
      return 0;
    }
  if (TREE_STATIC (field))
    {
      cp_error ("field `%#D' is static; only point of initialization is its declaration",
		field);
      return 0;
    }

  return 1;
}

/* If NAME is a viable field name for the aggregate DECL,
   and PARMS is a viable parameter list, then expand an _EXPR
   which describes this initialization.

   Note that we do not need to chase through the class's base classes
   to look for NAME, because if it's in that list, it will be handled
   by the constructor for that base class.

   We do not yet have a fixed-point finder to instantiate types
   being fed to overloaded constructors.  If there is a unique
   constructor, then argument types can be got from that one.

   If INIT is non-NULL, then it the initialization should
   be placed in `current_base_init_list', where it will be processed
   by `emit_base_init'.  */

void
expand_member_init (exp, name, init)
     tree exp, name, init;
{
  tree basetype = NULL_TREE, field;
  tree type;

  if (exp == NULL_TREE)
    return;			/* complain about this later */

  type = TYPE_MAIN_VARIANT (TREE_TYPE (exp));

  if (name && TREE_CODE (name) == TYPE_DECL)
    {
      basetype = TYPE_MAIN_VARIANT (TREE_TYPE (name));
      name = DECL_NAME (name);
    }

  if (name == NULL_TREE && IS_AGGR_TYPE (type))
    switch (CLASSTYPE_N_BASECLASSES (type))
      {
      case 0:
	error ("base class initializer specified, but no base class to initialize");
	return;
      case 1:
	basetype = TYPE_BINFO_BASETYPE (type, 0);
	break;
      default:
	error ("initializer for unnamed base class ambiguous");
	cp_error ("(type `%T' uses multiple inheritance)", type);
	return;
      }

  my_friendly_assert (init != NULL_TREE, 0);

  /* The grammar should not allow fields which have names that are
     TYPENAMEs.  Therefore, if the field has a non-NULL TREE_TYPE, we
     may assume that this is an attempt to initialize a base class
     member of the current type.  Otherwise, it is an attempt to
     initialize a member field.  */

  if (init == void_type_node)
    init = NULL_TREE;

  if (name == NULL_TREE || basetype)
    {
      tree base_init;

      if (name == NULL_TREE)
	{
#if 0
	  if (basetype)
	    name = TYPE_IDENTIFIER (basetype);
	  else
	    {
	      error ("no base class to initialize");
	      return;
	    }
#endif
	}
      else if (basetype != type
	       && ! current_template_parms
	       && ! vec_binfo_member (basetype,
				      TYPE_BINFO_BASETYPES (type))
	       && ! BINFO_FOR_VBASE (basetype, type))
	{
	  if (IDENTIFIER_CLASS_VALUE (name))
	    goto try_member;
	  if (TYPE_USES_VIRTUAL_BASECLASSES (type))
	    cp_error ("type `%T' is not an immediate or virtual basetype for `%T'",
		      basetype, type);
	  else
	    cp_error ("type `%T' is not an immediate basetype for `%T'",
		      basetype, type);
	  return;
	}

      if (purpose_member (basetype, current_base_init_list))
	{
	  cp_error ("base class `%T' already initialized", basetype);
	  return;
	}

      if (warn_reorder && current_member_init_list)
	{
	  cp_warning ("base initializer for `%T'", basetype);
	  warning ("   will be re-ordered to precede member initializations");
	}

      base_init = build_tree_list (basetype, init);
      current_base_init_list = chainon (current_base_init_list, base_init);
    }
  else
    {
      tree member_init;

    try_member:
      field = lookup_field (type, name, 1, 0);

      if (! member_init_ok_or_else (field, type, IDENTIFIER_POINTER (name)))
	return;

      if (purpose_member (name, current_member_init_list))
	{
	  cp_error ("field `%D' already initialized", field);
	  return;
	}

      member_init = build_tree_list (name, init);
      current_member_init_list = chainon (current_member_init_list, member_init);
    }
}

/* We are about to generate some complex initialization code.
   Conceptually, it is all a single expression.  However, we may want
   to include conditionals, loops, and other such statement-level
   constructs.  Therefore, we build the initialization code inside a
   statement-expression.  This function starts such an expression.
   STMT_EXPR_P and COMPOUND_STMT_P are filled in by this function;
   pass them back to finish_init_stmts when the expression is
   complete.  */

void
begin_init_stmts (stmt_expr_p, compound_stmt_p)
     tree *stmt_expr_p;
     tree *compound_stmt_p;
{
  *stmt_expr_p = begin_stmt_expr ();
  *compound_stmt_p = begin_compound_stmt (/*has_no_scope=*/1);
}

/* Finish out the statement-expression begun by the previous call to
   begin_init_stmts.  Returns the statement-expression itself.  */

tree
finish_init_stmts (stmt_expr, compound_stmt)
     tree stmt_expr;
     tree compound_stmt;
{
  finish_compound_stmt (/*has_no_scope=*/1, compound_stmt);
  stmt_expr = finish_stmt_expr (stmt_expr);

  /* To avoid spurious warnings about unused values, we set 
     TREE_USED.  */
  if (stmt_expr)
    TREE_USED (stmt_expr) = 1;

  return stmt_expr;
}

/* This is like `expand_member_init', only it stores one aggregate
   value into another.

   INIT comes in two flavors: it is either a value which
   is to be stored in EXP, or it is a parameter list
   to go to a constructor, which will operate on EXP.
   If INIT is not a parameter list for a constructor, then set
   LOOKUP_ONLYCONVERTING.
   If FLAGS is LOOKUP_ONLYCONVERTING then it is the = init form of
   the initializer, if FLAGS is 0, then it is the (init) form.
   If `init' is a CONSTRUCTOR, then we emit a warning message,
   explaining that such initializations are invalid.

   If INIT resolves to a CALL_EXPR which happens to return
   something of the type we are looking for, then we know
   that we can safely use that call to perform the
   initialization.

   The virtual function table pointer cannot be set up here, because
   we do not really know its type.

   Virtual baseclass pointers are also set up here.

   This never calls operator=().

   When initializing, nothing is CONST.

   A default copy constructor may have to be used to perform the
   initialization.

   A constructor or a conversion operator may have to be used to
   perform the initialization, but not both, as it would be ambiguous.  */

tree
build_aggr_init (exp, init, flags)
     tree exp, init;
     int flags;
{
  tree stmt_expr;
  tree compound_stmt;
  int destroy_temps;
  tree type = TREE_TYPE (exp);
  int was_const = TREE_READONLY (exp);
  int was_volatile = TREE_THIS_VOLATILE (exp);

  if (init == error_mark_node)
    return error_mark_node;

  TREE_READONLY (exp) = 0;
  TREE_THIS_VOLATILE (exp) = 0;

  if (init && TREE_CODE (init) != TREE_LIST)
    flags |= LOOKUP_ONLYCONVERTING;

  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      /* Must arrange to initialize each element of EXP
	 from elements of INIT.  */
      tree itype = init ? TREE_TYPE (init) : NULL_TREE;
      if (CP_TYPE_QUALS (type) != TYPE_UNQUALIFIED)
	{
	  TREE_TYPE (exp) = TYPE_MAIN_VARIANT (type);
	  if (init)
	    TREE_TYPE (init) = TYPE_MAIN_VARIANT (itype);
	}
      if (init && TREE_TYPE (init) == NULL_TREE)
	{
	  /* Handle bad initializers like:
	     class COMPLEX {
	     public:
	       double re, im;
	       COMPLEX(double r = 0.0, double i = 0.0) {re = r; im = i;};
	       ~COMPLEX() {};
	     };

	     int main(int argc, char **argv) {
	       COMPLEX zees(1.0, 0.0)[10];
	     }
	  */
	  error ("bad array initializer");
	  return error_mark_node;
	}
      stmt_expr = build_vec_init (exp, exp, array_type_nelts (type), init,
				  init && same_type_p (TREE_TYPE (init),
						       TREE_TYPE (exp)));
      TREE_READONLY (exp) = was_const;
      TREE_THIS_VOLATILE (exp) = was_volatile;
      TREE_TYPE (exp) = type;
      if (init)
	TREE_TYPE (init) = itype;
      return stmt_expr;
    }

  if (TREE_CODE (exp) == VAR_DECL || TREE_CODE (exp) == PARM_DECL)
    /* just know that we've seen something for this node */
    TREE_USED (exp) = 1;

  TREE_TYPE (exp) = TYPE_MAIN_VARIANT (type);
  begin_init_stmts (&stmt_expr, &compound_stmt);
  destroy_temps = stmts_are_full_exprs_p;
  stmts_are_full_exprs_p = 0;
  expand_aggr_init_1 (TYPE_BINFO (type), exp, exp,
		      init, LOOKUP_NORMAL|flags);
  stmt_expr = finish_init_stmts (stmt_expr, compound_stmt);
  stmts_are_full_exprs_p = destroy_temps;
  TREE_TYPE (exp) = type;
  TREE_READONLY (exp) = was_const;
  TREE_THIS_VOLATILE (exp) = was_volatile;

  return stmt_expr;
}

static void
expand_default_init (binfo, true_exp, exp, init, flags)
     tree binfo;
     tree true_exp, exp;
     tree init;
     int flags;
{
  tree type = TREE_TYPE (exp);

  /* It fails because there may not be a constructor which takes
     its own type as the first (or only parameter), but which does
     take other types via a conversion.  So, if the thing initializing
     the expression is a unit element of type X, first try X(X&),
     followed by initialization by X.  If neither of these work
     out, then look hard.  */
  tree rval;
  tree parms;

  if (init && TREE_CODE (init) != TREE_LIST
      && (flags & LOOKUP_ONLYCONVERTING))
    {
      /* Base subobjects should only get direct-initialization.  */
      if (true_exp != exp)
	abort ();

      if (flags & DIRECT_BIND)
	/* Do nothing.  We hit this in two cases:  Reference initialization,
	   where we aren't initializing a real variable, so we don't want
	   to run a new constructor; and catching an exception, where we
	   have already built up the constructor call so we could wrap it
	   in an exception region.  */;
      else if (TREE_CODE (init) == CONSTRUCTOR)
	/* A brace-enclosed initializer has whatever type is
	   required.  There's no need to convert it.  */
	;
      else
	init = ocp_convert (type, init, CONV_IMPLICIT|CONV_FORCE_TEMP, flags);

      if (TREE_CODE (init) == TRY_CATCH_EXPR)
	/* We need to protect the initialization of a catch parm
	   with a call to terminate(), which shows up as a TRY_CATCH_EXPR
	   around the TARGET_EXPR for the copy constructor.  See
	   expand_start_catch_block.  */
	TREE_OPERAND (init, 0) = build (INIT_EXPR, TREE_TYPE (exp), exp,
					TREE_OPERAND (init, 0));
      else
	init = build (INIT_EXPR, TREE_TYPE (exp), exp, init);
      TREE_SIDE_EFFECTS (init) = 1;
      finish_expr_stmt (init);
      return;
    }

  if (init == NULL_TREE
      || (TREE_CODE (init) == TREE_LIST && ! TREE_TYPE (init)))
    {
      parms = init;
      if (parms)
	init = TREE_VALUE (parms);
    }
  else
    parms = build_tree_list (NULL_TREE, init);

  if (TYPE_USES_VIRTUAL_BASECLASSES (type))
    {
      if (true_exp == exp)
	parms = tree_cons (NULL_TREE, integer_one_node, parms);
      else
	parms = tree_cons (NULL_TREE, integer_zero_node, parms);
      flags |= LOOKUP_HAS_IN_CHARGE;
    }

  rval = build_method_call (exp, ctor_identifier,
			    parms, binfo, flags);
  if (TREE_SIDE_EFFECTS (rval))
    finish_expr_stmt (rval);
}

/* This function is responsible for initializing EXP with INIT
   (if any).

   BINFO is the binfo of the type for who we are performing the
   initialization.  For example, if W is a virtual base class of A and B,
   and C : A, B.
   If we are initializing B, then W must contain B's W vtable, whereas
   were we initializing C, W must contain C's W vtable.

   TRUE_EXP is nonzero if it is the true expression being initialized.
   In this case, it may be EXP, or may just contain EXP.  The reason we
   need this is because if EXP is a base element of TRUE_EXP, we
   don't necessarily know by looking at EXP where its virtual
   baseclass fields should really be pointing.  But we do know
   from TRUE_EXP.  In constructors, we don't know anything about
   the value being initialized.

   FLAGS is just passes to `build_method_call'.  See that function for
   its description.  */

static void
expand_aggr_init_1 (binfo, true_exp, exp, init, flags)
     tree binfo;
     tree true_exp, exp;
     tree init;
     int flags;
{
  tree type = TREE_TYPE (exp);

  my_friendly_assert (init != error_mark_node && type != error_mark_node, 211);

  /* Use a function returning the desired type to initialize EXP for us.
     If the function is a constructor, and its first argument is
     NULL_TREE, know that it was meant for us--just slide exp on
     in and expand the constructor.  Constructors now come
     as TARGET_EXPRs.  */

  if (init && TREE_CODE (exp) == VAR_DECL
      && TREE_CODE (init) == CONSTRUCTOR
      && TREE_HAS_CONSTRUCTOR (init))
    {
      /* If store_init_value returns NULL_TREE, the INIT has been
	 record in the DECL_INITIAL for EXP.  That means there's
	 nothing more we have to do.  */
      if (!store_init_value (exp, init))
	{
	  if (!building_stmt_tree ())
	    expand_decl_init (exp);
	}
      else
	finish_expr_stmt (build (INIT_EXPR, type, exp, init));
      return;
    }

  /* We know that expand_default_init can handle everything we want
     at this point.  */
  expand_default_init (binfo, true_exp, exp, init, flags);
}

/* Report an error if NAME is not the name of a user-defined,
   aggregate type.  If OR_ELSE is nonzero, give an error message.  */

int
is_aggr_typedef (name, or_else)
     tree name;
     int or_else;
{
  tree type;

  if (name == error_mark_node)
    return 0;

  if (IDENTIFIER_HAS_TYPE_VALUE (name))
    type = IDENTIFIER_TYPE_VALUE (name);
  else
    {
      if (or_else)
	cp_error ("`%T' is not an aggregate typedef", name);
      return 0;
    }

  if (! IS_AGGR_TYPE (type)
      && TREE_CODE (type) != TEMPLATE_TYPE_PARM
      && TREE_CODE (type) != TEMPLATE_TEMPLATE_PARM)
    {
      if (or_else)
	cp_error ("`%T' is not an aggregate type", type);
      return 0;
    }
  return 1;
}

/* Report an error if TYPE is not a user-defined, aggregate type.  If
   OR_ELSE is nonzero, give an error message.  */

int
is_aggr_type (type, or_else)
     tree type;
     int or_else;
{
  if (type == error_mark_node)
    return 0;

  if (! IS_AGGR_TYPE (type)
      && TREE_CODE (type) != TEMPLATE_TYPE_PARM
      && TREE_CODE (type) != TEMPLATE_TEMPLATE_PARM)
    {
      if (or_else)
	cp_error ("`%T' is not an aggregate type", type);
      return 0;
    }
  return 1;
}

/* Like is_aggr_typedef, but returns typedef if successful.  */

tree
get_aggr_from_typedef (name, or_else)
     tree name;
     int or_else;
{
  tree type;

  if (name == error_mark_node)
    return NULL_TREE;

  if (IDENTIFIER_HAS_TYPE_VALUE (name))
    type = IDENTIFIER_TYPE_VALUE (name);
  else
    {
      if (or_else)
	cp_error ("`%T' fails to be an aggregate typedef", name);
      return NULL_TREE;
    }

  if (! IS_AGGR_TYPE (type)
      && TREE_CODE (type) != TEMPLATE_TYPE_PARM
      && TREE_CODE (type) != TEMPLATE_TEMPLATE_PARM)
    {
      if (or_else)
	cp_error ("type `%T' is of non-aggregate type", type);
      return NULL_TREE;
    }
  return type;
}

tree
get_type_value (name)
     tree name;
{
  if (name == error_mark_node)
    return NULL_TREE;

  if (IDENTIFIER_HAS_TYPE_VALUE (name))
    return IDENTIFIER_TYPE_VALUE (name);
  else
    return NULL_TREE;
}


/* This code could just as well go in `class.c', but is placed here for
   modularity.  */

/* For an expression of the form TYPE :: NAME (PARMLIST), build
   the appropriate function call.  */

tree
build_member_call (type, name, parmlist)
     tree type, name, parmlist;
{
  tree t;
  tree method_name;
  int dtor = 0;
  tree basetype_path, decl;

  if (TREE_CODE (name) == TEMPLATE_ID_EXPR
      && TREE_CODE (type) == NAMESPACE_DECL)
    {
      /* 'name' already refers to the decls from the namespace, since we
	 hit do_identifier for template_ids.  */
      method_name = TREE_OPERAND (name, 0);
      /* FIXME: Since we don't do independent names right yet, the
	 name might also be a LOOKUP_EXPR. Once we resolve this to a
	 real decl earlier, this can go. This may happen during
	 tsubst'ing.  */
      if (TREE_CODE (method_name) == LOOKUP_EXPR)
	{
	  method_name = lookup_namespace_name 
	    (type, TREE_OPERAND (method_name, 0));
	  TREE_OPERAND (name, 0) = method_name;
	}
      my_friendly_assert (is_overloaded_fn (method_name), 980519);
      return build_x_function_call (name, parmlist, current_class_ref);
    }

  if (type == std_node)
    return build_x_function_call (do_scoped_id (name, 0), parmlist,
				  current_class_ref);
  if (TREE_CODE (type) == NAMESPACE_DECL)
    return build_x_function_call (lookup_namespace_name (type, name),
				  parmlist, current_class_ref);

  if (TREE_CODE (name) == TEMPLATE_ID_EXPR)
    {
      method_name = TREE_OPERAND (name, 0);
      if (TREE_CODE (method_name) == COMPONENT_REF)
	method_name = TREE_OPERAND (method_name, 1);
      if (is_overloaded_fn (method_name))
	method_name = DECL_NAME (OVL_CURRENT (method_name));
      TREE_OPERAND (name, 0) = method_name;
    }
  else
    method_name = name;

  if (TREE_CODE (method_name) == BIT_NOT_EXPR)
    {
      method_name = TREE_OPERAND (method_name, 0);
      dtor = 1;
    }

  /* This shouldn't be here, and build_member_call shouldn't appear in
     parse.y!  (mrs)  */
  if (type && TREE_CODE (type) == IDENTIFIER_NODE
      && get_aggr_from_typedef (type, 0) == 0)
    {
      tree ns = lookup_name (type, 0);
      if (ns && TREE_CODE (ns) == NAMESPACE_DECL)
	{
	  return build_x_function_call (build_offset_ref (type, name), parmlist, current_class_ref);
	}
    }

  if (type == NULL_TREE || ! is_aggr_type (type, 1))
    return error_mark_node;

  /* An operator we did not like.  */
  if (name == NULL_TREE)
    return error_mark_node;

  if (dtor)
    {
      cp_error ("cannot call destructor `%T::~%T' without object", type,
		method_name);
      return error_mark_node;
    }

  decl = maybe_dummy_object (type, &basetype_path);

  /* Convert 'this' to the specified type to disambiguate conversion
     to the function's context.  Apparently Standard C++ says that we
     shouldn't do this.  */
  if (decl == current_class_ref
      && ! pedantic
      && ACCESSIBLY_UNIQUELY_DERIVED_P (type, current_class_type))
    {
      tree olddecl = current_class_ptr;
      tree oldtype = TREE_TYPE (TREE_TYPE (olddecl));
      if (oldtype != type)
	{
	  tree newtype = build_qualified_type (type, TYPE_QUALS (oldtype));
	  decl = convert_force (build_pointer_type (newtype), olddecl, 0);
	  decl = build_indirect_ref (decl, NULL_PTR);
	}
    }

  if (method_name == constructor_name (type)
      || method_name == constructor_name_full (type))
    return build_functional_cast (type, parmlist);
  if (lookup_fnfields (basetype_path, method_name, 0))
    return build_method_call (decl, 
			      TREE_CODE (name) == TEMPLATE_ID_EXPR
			      ? name : method_name,
			      parmlist, basetype_path,
			      LOOKUP_NORMAL|LOOKUP_NONVIRTUAL);
  if (TREE_CODE (name) == IDENTIFIER_NODE
      && ((t = lookup_field (TYPE_BINFO (type), name, 1, 0))))
    {
      if (t == error_mark_node)
	return error_mark_node;
      if (TREE_CODE (t) == FIELD_DECL)
	{
	  if (is_dummy_object (decl))
	    {
	      cp_error ("invalid use of non-static field `%D'", t);
	      return error_mark_node;
	    }
	  decl = build (COMPONENT_REF, TREE_TYPE (t), decl, t);
	}
      else if (TREE_CODE (t) == VAR_DECL)
	decl = t;
      else
	{
	  cp_error ("invalid use of member `%D'", t);
	  return error_mark_node;
	}
      if (TYPE_LANG_SPECIFIC (TREE_TYPE (decl)))
	return build_opfncall (CALL_EXPR, LOOKUP_NORMAL, decl,
			       parmlist, NULL_TREE);
      return build_function_call (decl, parmlist);
    }
  else
    {
      cp_error ("no method `%T::%D'", type, name);
      return error_mark_node;
    }
}

/* Build a reference to a member of an aggregate.  This is not a
   C++ `&', but really something which can have its address taken,
   and then act as a pointer to member, for example TYPE :: FIELD
   can have its address taken by saying & TYPE :: FIELD.

   @@ Prints out lousy diagnostics for operator <typename>
   @@ fields.

   @@ This function should be rewritten and placed in search.c.  */

tree
build_offset_ref (type, name)
     tree type, name;
{
  tree decl, t = error_mark_node;
  tree member;
  tree basebinfo = NULL_TREE;
  tree orig_name = name;

  /* class templates can come in as TEMPLATE_DECLs here.  */
  if (TREE_CODE (name) == TEMPLATE_DECL)
    return name;

  if (type == std_node)
    return do_scoped_id (name, 0);

  if (processing_template_decl || uses_template_parms (type))
    return build_min_nt (SCOPE_REF, type, name);

  /* Handle namespace names fully here.  */
  if (TREE_CODE (type) == NAMESPACE_DECL)
    {
      t = lookup_namespace_name (type, name);
      if (t != error_mark_node && ! type_unknown_p (t))
	{
	  mark_used (t);
	  t = convert_from_reference (t);
	}
      return t;
    }

  if (type == NULL_TREE || ! is_aggr_type (type, 1))
    return error_mark_node;

  if (TREE_CODE (name) == TEMPLATE_ID_EXPR)
    {
      /* If the NAME is a TEMPLATE_ID_EXPR, we are looking at
	 something like `a.template f<int>' or the like.  For the most
	 part, we treat this just like a.f.  We do remember, however,
	 the template-id that was used.  */
      name = TREE_OPERAND (orig_name, 0);

      if (TREE_CODE (name) == LOOKUP_EXPR)
	/* This can happen during tsubst'ing.  */
	name = TREE_OPERAND (name, 0);

      my_friendly_assert (TREE_CODE (name) == IDENTIFIER_NODE, 0);
    }

  if (TREE_CODE (name) == BIT_NOT_EXPR)
    {
      if (! check_dtor_name (type, name))
	cp_error ("qualified type `%T' does not match destructor name `~%T'",
		  type, TREE_OPERAND (name, 0));
      name = dtor_identifier;
    }
#if 0
  /* I think this is wrong, but the draft is unclear.  --jason 6/15/98 */
  else if (name == constructor_name_full (type)
	   || name == constructor_name (type))
    name = ctor_identifier;
#endif

  if (TYPE_SIZE (complete_type (type)) == 0
      && !TYPE_BEING_DEFINED (type))
    {
      cp_error ("incomplete type `%T' does not have member `%D'", type,
		name);
      return error_mark_node;
    }

  decl = maybe_dummy_object (type, &basebinfo);

  member = lookup_member (basebinfo, name, 1, 0);

  if (member == error_mark_node)
    return error_mark_node;

  /* A lot of this logic is now handled in lookup_member.  */
  if (member && BASELINK_P (member))
    {
      /* Go from the TREE_BASELINK to the member function info.  */
      tree fnfields = member;
      t = TREE_VALUE (fnfields);

      if (TREE_CODE (orig_name) == TEMPLATE_ID_EXPR)
	{
	  /* The FNFIELDS are going to contain functions that aren't
	     necessarily templates, and templates that don't
	     necessarily match the explicit template parameters.  We
	     save all the functions, and the explicit parameters, and
	     then figure out exactly what to instantiate with what
	     arguments in instantiate_type.  */

	  if (TREE_CODE (t) != OVERLOAD)
	    /* The code in instantiate_type which will process this
	       expects to encounter OVERLOADs, not raw functions.  */
	    t = ovl_cons (t, NULL_TREE);

	  return build (OFFSET_REF, 
			unknown_type_node,
			decl,
			build (TEMPLATE_ID_EXPR, 
			       TREE_TYPE (t),
			       t,
			       TREE_OPERAND (orig_name, 1)));
	}

      if (!really_overloaded_fn (t))
	{
	  /* Get rid of a potential OVERLOAD around it */
	  t = OVL_CURRENT (t);

	  /* unique functions are handled easily.  */
	  if (!enforce_access (basebinfo, t))
	    return error_mark_node;
	  mark_used (t);
	  if (DECL_STATIC_FUNCTION_P (t))
	    return t;
	  return build (OFFSET_REF, TREE_TYPE (t), decl, t);
	}

      TREE_TYPE (fnfields) = unknown_type_node;
      return build (OFFSET_REF, unknown_type_node, decl, fnfields);
    }

  t = member;

  if (t == NULL_TREE)
    {
      cp_error ("`%D' is not a member of type `%T'", name, type);
      return error_mark_node;
    }

  if (TREE_CODE (t) == TYPE_DECL)
    {
      TREE_USED (t) = 1;
      return t;
    }
  /* static class members and class-specific enum
     values can be returned without further ado.  */
  if (TREE_CODE (t) == VAR_DECL || TREE_CODE (t) == CONST_DECL)
    {
      mark_used (t);
      return convert_from_reference (t);
    }

  if (TREE_CODE (t) == FIELD_DECL && DECL_C_BIT_FIELD (t))
    {
      cp_error ("illegal pointer to bit field `%D'", t);
      return error_mark_node;
    }

  /* static class functions too.  */
  if (TREE_CODE (t) == FUNCTION_DECL
      && TREE_CODE (TREE_TYPE (t)) == FUNCTION_TYPE)
    my_friendly_abort (53);

  /* In member functions, the form `type::name' is no longer
     equivalent to `this->type::name', at least not until
     resolve_offset_ref.  */
  return build (OFFSET_REF, build_offset_type (type, TREE_TYPE (t)), decl, t);
}

/* If a OFFSET_REF made it through to here, then it did
   not have its address taken.  */

tree
resolve_offset_ref (exp)
     tree exp;
{
  tree type = TREE_TYPE (exp);
  tree base = NULL_TREE;
  tree member;
  tree basetype, addr;

  if (TREE_CODE (exp) == OFFSET_REF)
    {
      member = TREE_OPERAND (exp, 1);
      base = TREE_OPERAND (exp, 0);
    }
  else
    {
      my_friendly_assert (TREE_CODE (type) == OFFSET_TYPE, 214);
      if (TYPE_OFFSET_BASETYPE (type) != current_class_type)
	{
	  error ("object missing in use of pointer-to-member construct");
	  return error_mark_node;
	}
      member = exp;
      type = TREE_TYPE (type);
      base = current_class_ref;
    }

  if (BASELINK_P (member))
    {
      if (! flag_ms_extensions)
	cp_pedwarn ("assuming & on overloaded member function");
      return build_unary_op (ADDR_EXPR, exp, 0);
    }

  if (TREE_CODE (TREE_TYPE (member)) == METHOD_TYPE)
    {
      if (! flag_ms_extensions)
	cp_pedwarn ("assuming & on `%E'", member);
      return build_unary_op (ADDR_EXPR, exp, 0);
    }

  if ((TREE_CODE (member) == VAR_DECL
       && ! TYPE_PTRMEMFUNC_P (TREE_TYPE (member))
       && ! TYPE_PTRMEM_P (TREE_TYPE (member)))
      || TREE_CODE (TREE_TYPE (member)) == FUNCTION_TYPE)
    {
      /* These were static members.  */
      if (mark_addressable (member) == 0)
	return error_mark_node;
      return member;
    }

  if (TREE_CODE (TREE_TYPE (member)) == POINTER_TYPE
      && TREE_CODE (TREE_TYPE (TREE_TYPE (member))) == METHOD_TYPE)
    return member;

  /* Syntax error can cause a member which should
     have been seen as static to be grok'd as non-static.  */
  if (TREE_CODE (member) == FIELD_DECL && current_class_ref == NULL_TREE)
    {
      if (TREE_ADDRESSABLE (member) == 0)
	{
	  cp_error_at ("member `%D' is non-static but referenced as a static member",
		       member);
	  error ("at this point in file");
	  TREE_ADDRESSABLE (member) = 1;
	}
      return error_mark_node;
    }

  /* The first case is really just a reference to a member of `this'.  */
  if (TREE_CODE (member) == FIELD_DECL
      && (base == current_class_ref || is_dummy_object (base)))
    {
      tree expr;

      basetype = DECL_CONTEXT (member);

      /* Try to get to basetype from 'this'; if that doesn't work,
         nothing will.  */
      base = current_class_ref;

      /* First convert to the intermediate base specified, if appropriate.  */
      if (TREE_CODE (exp) == OFFSET_REF && TREE_CODE (type) == OFFSET_TYPE)
	base = build_scoped_ref (base, TYPE_OFFSET_BASETYPE (type));

      addr = build_unary_op (ADDR_EXPR, base, 0);
      addr = convert_pointer_to (basetype, addr);

      if (addr == error_mark_node)
	return error_mark_node;

      expr = build (COMPONENT_REF, TREE_TYPE (member),
		    build_indirect_ref (addr, NULL_PTR), member);
      return convert_from_reference (expr);
    }

  /* Ensure that we have an object.  */
  if (is_dummy_object (base))
    addr = error_mark_node;
  else
    /* If this is a reference to a member function, then return the
       address of the member function (which may involve going
       through the object's vtable), otherwise, return an expression
       for the dereferenced pointer-to-member construct.  */
    addr = build_unary_op (ADDR_EXPR, base, 0);

  if (TYPE_PTRMEM_P (TREE_TYPE (member)))
    {
      if (addr == error_mark_node)
	{
	  cp_error ("object missing in `%E'", exp);
	  return error_mark_node;
	}

      basetype = TYPE_OFFSET_BASETYPE (TREE_TYPE (TREE_TYPE (member)));
      addr = convert_pointer_to (basetype, addr);
      member = cp_convert (ptrdiff_type_node, member);

      /* Pointer to data members are offset by one, so that a null
	 pointer with a real value of 0 is distinguishable from an
	 offset of the first member of a structure.  */
      member = build_binary_op (MINUS_EXPR, member,
				cp_convert (ptrdiff_type_node, integer_one_node));

      return build1 (INDIRECT_REF, type,
		     build (PLUS_EXPR, build_pointer_type (type),
			    addr, member));
    }
  else if (TYPE_PTRMEMFUNC_P (TREE_TYPE (member)))
    {
      return get_member_function_from_ptrfunc (&addr, member);
    }
  my_friendly_abort (56);
  /* NOTREACHED */
  return NULL_TREE;
}

/* Return either DECL or its known constant value (if it has one).  */

tree
decl_constant_value (decl)
     tree decl;
{
  if (! TREE_THIS_VOLATILE (decl)
      && DECL_INITIAL (decl)
      && DECL_INITIAL (decl) != error_mark_node
      /* This is invalid if initial value is not constant.
	 If it has either a function call, a memory reference,
	 or a variable, then re-evaluating it could give different results.  */
      && TREE_CONSTANT (DECL_INITIAL (decl))
      /* Check for cases where this is sub-optimal, even though valid.  */
      && TREE_CODE (DECL_INITIAL (decl)) != CONSTRUCTOR)
    return DECL_INITIAL (decl);
  return decl;
}

/* Common subroutines of build_new and build_vec_delete.  */

/* Call the global __builtin_delete to delete ADDR.  */

static tree
build_builtin_delete_call (addr)
     tree addr;
{
  mark_used (global_delete_fndecl);
  return build_call (global_delete_fndecl, 
		     void_type_node, build_tree_list (NULL_TREE, addr));
}

/* Generate a C++ "new" expression. DECL is either a TREE_LIST
   (which needs to go through some sort of groktypename) or it
   is the name of the class we are newing. INIT is an initialization value.
   It is either an EXPRLIST, an EXPR_NO_COMMAS, or something in braces.
   If INIT is void_type_node, it means do *not* call a constructor
   for this instance.

   For types with constructors, the data returned is initialized
   by the appropriate constructor.

   Whether the type has a constructor or not, if it has a pointer
   to a virtual function table, then that pointer is set up
   here.

   Unless I am mistaken, a call to new () will return initialized
   data regardless of whether the constructor itself is private or
   not.  NOPE; new fails if the constructor is private (jcm).

   Note that build_new does nothing to assure that any special
   alignment requirements of the type are met.  Rather, it leaves
   it up to malloc to do the right thing.  Otherwise, folding to
   the right alignment cal cause problems if the user tries to later
   free the memory returned by `new'.

   PLACEMENT is the `placement' list for user-defined operator new ().  */

extern int flag_check_new;

tree
build_new (placement, decl, init, use_global_new)
     tree placement;
     tree decl, init;
     int use_global_new;
{
  tree type, rval;
  tree nelts = NULL_TREE, t;
  int has_array = 0;

  if (decl == error_mark_node)
    return error_mark_node;

  if (TREE_CODE (decl) == TREE_LIST)
    {
      tree absdcl = TREE_VALUE (decl);
      tree last_absdcl = NULL_TREE;

      if (current_function_decl
	  && DECL_CONSTRUCTOR_P (current_function_decl))
	my_friendly_assert (immediate_size_expand == 0, 19990926);

      nelts = integer_one_node;

      if (absdcl && TREE_CODE (absdcl) == CALL_EXPR)
	my_friendly_abort (215);
      while (absdcl && TREE_CODE (absdcl) == INDIRECT_REF)
	{
	  last_absdcl = absdcl;
	  absdcl = TREE_OPERAND (absdcl, 0);
	}

      if (absdcl && TREE_CODE (absdcl) == ARRAY_REF)
	{
	  /* probably meant to be a vec new */
	  tree this_nelts;

	  while (TREE_OPERAND (absdcl, 0)
		 && TREE_CODE (TREE_OPERAND (absdcl, 0)) == ARRAY_REF)
	    {
	      last_absdcl = absdcl;
	      absdcl = TREE_OPERAND (absdcl, 0);
	    }

	  has_array = 1;
	  this_nelts = TREE_OPERAND (absdcl, 1);
	  if (this_nelts != error_mark_node)
	    {
	      if (this_nelts == NULL_TREE)
		error ("new of array type fails to specify size");
	      else if (processing_template_decl)
		{
		  nelts = this_nelts;
		  absdcl = TREE_OPERAND (absdcl, 0);
		}
	      else
		{
		  int flags = pedantic ? WANT_INT : (WANT_INT | WANT_ENUM);
		  if (build_expr_type_conversion (flags, this_nelts, 0)
		      == NULL_TREE)
		    pedwarn ("size in array new must have integral type");

		  this_nelts = save_expr (cp_convert (sizetype, this_nelts));
		  absdcl = TREE_OPERAND (absdcl, 0);
	          if (this_nelts == integer_zero_node)
		    {
		      warning ("zero size array reserves no space");
		      nelts = integer_zero_node;
		    }
		  else
		    nelts = build_binary_op (MULT_EXPR, nelts, this_nelts);
		}
	    }
	  else
	    nelts = integer_zero_node;
	}

      if (last_absdcl)
	TREE_OPERAND (last_absdcl, 0) = absdcl;
      else
	TREE_VALUE (decl) = absdcl;

      type = groktypename (decl);
      if (! type || type == error_mark_node)
	return error_mark_node;
    }
  else if (TREE_CODE (decl) == IDENTIFIER_NODE)
    {
      if (IDENTIFIER_HAS_TYPE_VALUE (decl))
	{
	  /* An aggregate type.  */
	  type = IDENTIFIER_TYPE_VALUE (decl);
	  decl = TYPE_MAIN_DECL (type);
	}
      else
	{
	  /* A builtin type.  */
	  decl = lookup_name (decl, 1);
	  my_friendly_assert (TREE_CODE (decl) == TYPE_DECL, 215);
	  type = TREE_TYPE (decl);
	}
    }
  else if (TREE_CODE (decl) == TYPE_DECL)
    {
      type = TREE_TYPE (decl);
    }
  else
    {
      type = decl;
      decl = TYPE_MAIN_DECL (type);
    }

  if (processing_template_decl)
    {
      if (has_array)
	t = tree_cons (tree_cons (NULL_TREE, type, NULL_TREE),
		       build_min_nt (ARRAY_REF, NULL_TREE, nelts),
		       NULL_TREE);
      else
	t = type;
	
      rval = build_min_nt (NEW_EXPR, placement, t, init);
      NEW_EXPR_USE_GLOBAL (rval) = use_global_new;
      return rval;
    }

  /* ``A reference cannot be created by the new operator.  A reference
     is not an object (8.2.2, 8.4.3), so a pointer to it could not be
     returned by new.'' ARM 5.3.3 */
  if (TREE_CODE (type) == REFERENCE_TYPE)
    {
      error ("new cannot be applied to a reference type");
      type = TREE_TYPE (type);
    }

  if (TREE_CODE (type) == FUNCTION_TYPE)
    {
      error ("new cannot be applied to a function type");
      return error_mark_node;
    }

  /* When the object being created is an array, the new-expression yields a
     pointer to the initial element (if any) of the array.  For example,
     both new int and new int[10] return an int*.  5.3.4.  */
  if (TREE_CODE (type) == ARRAY_TYPE && has_array == 0)
    {
      nelts = array_type_nelts_top (type);
      has_array = 1;
      type = TREE_TYPE (type);
    }

  if (has_array)
    t = build_nt (ARRAY_REF, type, nelts);
  else
    t = type;

  rval = build (NEW_EXPR, build_pointer_type (type), placement, t, init);
  NEW_EXPR_USE_GLOBAL (rval) = use_global_new;
  TREE_SIDE_EFFECTS (rval) = 1;
  rval = build_new_1 (rval);
  if (rval == error_mark_node)
    return error_mark_node;

  /* Wrap it in a NOP_EXPR so warn_if_unused_value doesn't complain.  */
  rval = build1 (NOP_EXPR, TREE_TYPE (rval), rval);
  TREE_NO_UNUSED_WARNING (rval) = 1;

  return rval;
}

/* Given a Java class, return a decl for the corresponding java.lang.Class. */

tree
build_java_class_ref (type)
     tree type;
{
  tree name, class_decl;
  static tree CL_prefix = NULL_TREE;
  if (CL_prefix == NULL_TREE)
    CL_prefix = get_identifier("_CL_");
  if (jclass_node == NULL_TREE)
    {
      jclass_node = IDENTIFIER_GLOBAL_VALUE (get_identifier("jclass"));
      if (jclass_node == NULL_TREE)
	fatal("call to Java constructor, while `jclass' undefined");
      jclass_node = TREE_TYPE (jclass_node);
    }
  name = build_overload_with_type (CL_prefix, type);
  class_decl = IDENTIFIER_GLOBAL_VALUE (name);
  if (class_decl == NULL_TREE)
    {
      class_decl = build_decl (VAR_DECL, name, TREE_TYPE (jclass_node));
      TREE_STATIC (class_decl) = 1;
      DECL_EXTERNAL (class_decl) = 1;
      TREE_PUBLIC (class_decl) = 1;
      DECL_ARTIFICIAL (class_decl) = 1;
      DECL_IGNORED_P (class_decl) = 1;
      pushdecl_top_level (class_decl);
      make_decl_rtl (class_decl, NULL_PTR, 1);
    }
  return class_decl;
}

/* Returns teh size of the cookie to use when allocating an array
   whose elements have the indicated TYPE.  Assumes that it is already
   known that a cookie is needed.  */

static tree
get_cookie_size (type)
     tree type;
{
  tree cookie_size;

  if (flag_new_abi)
    {
      /* Under the new ABI, we need to allocate an additional max
	 (sizeof (size_t), alignof (true_type)) bytes.  */
      tree sizetype_size;
      tree type_align;

      sizetype_size = size_in_bytes (sizetype);
      type_align = size_int (TYPE_ALIGN_UNIT (type));
      if (INT_CST_LT_UNSIGNED (type_align, sizetype_size))
	cookie_size = sizetype_size;
      else
	cookie_size = type_align;
    }
  else
    cookie_size = BI_header_size;

  return cookie_size;
}

/* Called from cplus_expand_expr when expanding a NEW_EXPR.  The return
   value is immediately handed to expand_expr.  */

static tree
build_new_1 (exp)
     tree exp;
{
  tree placement, init;
  tree type, true_type, size, rval;
  tree nelts = NULL_TREE;
  tree alloc_expr, alloc_node = NULL_TREE;
  int has_array = 0;
  enum tree_code code;
  int use_cookie, nothrow, check_new;
  /* Nonzero if the user wrote `::new' rather than just `new'.  */
  int globally_qualified_p;
  /* Nonzero if we're going to call a global operator new, rather than
     a class-specific version.  */
  int use_global_new;
  int use_java_new = 0;
  /* If non-NULL, the number of extra bytes to allocate at the
     beginning of the storage allocated for an array-new expression in
     order to store the number of elements.  */
  tree cookie_size = NULL_TREE;

  placement = TREE_OPERAND (exp, 0);
  type = TREE_OPERAND (exp, 1);
  init = TREE_OPERAND (exp, 2);
  globally_qualified_p = NEW_EXPR_USE_GLOBAL (exp);

  if (TREE_CODE (type) == ARRAY_REF)
    {
      has_array = 1;
      nelts = TREE_OPERAND (type, 1);
      type = TREE_OPERAND (type, 0);
    }
  true_type = type;

  code = has_array ? VEC_NEW_EXPR : NEW_EXPR;

  if (CP_TYPE_QUALS (type))
    type = TYPE_MAIN_VARIANT (type);

  /* If our base type is an array, then make sure we know how many elements
     it has.  */
  while (TREE_CODE (true_type) == ARRAY_TYPE)
    {
      tree this_nelts = array_type_nelts_top (true_type);
      nelts = build_binary_op (MULT_EXPR, nelts, this_nelts);
      true_type = TREE_TYPE (true_type);
    }

  if (!complete_type_or_else (true_type, exp))
    return error_mark_node;

  if (has_array)
    size = fold (build_binary_op (MULT_EXPR, size_in_bytes (true_type),
				  nelts));
  else
    size = size_in_bytes (type);

  if (TREE_CODE (true_type) == VOID_TYPE)
    {
      error ("invalid type `void' for new");
      return error_mark_node;
    }

  if (abstract_virtuals_error (NULL_TREE, true_type))
    return error_mark_node;

  /* Figure out whether or not we're going to use the global operator
     new.  */
  if (!globally_qualified_p
      && IS_AGGR_TYPE (true_type)
      && ((!has_array && TYPE_HAS_NEW_OPERATOR (true_type))
	  || (has_array && TYPE_HAS_ARRAY_NEW_OPERATOR (true_type))))
    use_global_new = 0;
  else
    use_global_new = 1;

  /* We only need cookies for arrays containing types for which we
     need cookies.  */
  if (!has_array || !TYPE_VEC_NEW_USES_COOKIE (true_type))
    use_cookie = 0;
  /* When using placement new, users may not realize that they need
     the extra storage.  Under the old ABI, we don't allocate the
     cookie whenever they use one placement argument of type `void
     *'.  Under the new ABI, we require that the operator called be
     the global placement operator delete[].  */
  else if (placement && !TREE_CHAIN (placement) 
	   && same_type_p (TREE_TYPE (TREE_VALUE (placement)),
			   ptr_type_node))
    use_cookie = (!flag_new_abi || !use_global_new);
  /* Otherwise, we need the cookie.  */
  else
    use_cookie = 1;

  /* Compute the number of extra bytes to allocate, now that we know
     whether or not we need the cookie.  */
  if (use_cookie)
    {
      cookie_size = get_cookie_size (true_type);
      size = size_binop (PLUS_EXPR, size, cookie_size);
    }

  if (has_array && init && pedantic)
    cp_pedwarn ("initialization in array new");

  /* Allocate the object.  */
  
  if (! placement && TYPE_FOR_JAVA (true_type))
    {
      tree class_addr, alloc_decl;
      tree class_decl = build_java_class_ref (true_type);
      tree class_size = size_in_bytes (true_type);
      static char alloc_name[] = "_Jv_AllocObject";
      use_java_new = 1;
      alloc_decl = IDENTIFIER_GLOBAL_VALUE (get_identifier (alloc_name));
      if (alloc_decl == NULL_TREE)
	fatal("call to Java constructor, while `%s' undefined", alloc_name);
      class_addr = build1 (ADDR_EXPR, jclass_node, class_decl);
      rval = build_function_call (alloc_decl,
				  tree_cons (NULL_TREE, class_addr,
					     build_tree_list (NULL_TREE,
							      class_size)));
      rval = cp_convert (build_pointer_type (true_type), rval);
    }
  else
    {
      tree fnname;
      tree args;

      args = tree_cons (NULL_TREE, size, placement);
      fnname = ansi_opname[code];

      if (use_global_new)
	rval = (build_new_function_call 
		(lookup_function_nonclass (fnname, args),
		 args));
      else
	rval = build_method_call (build_dummy_object (true_type),
				  fnname, args, NULL_TREE,
				  LOOKUP_NORMAL);
      rval = cp_convert (build_pointer_type (true_type), rval);
    }

  /*        unless an allocation function is declared with an empty  excep-
     tion-specification  (_except.spec_),  throw(), it indicates failure to
     allocate storage by throwing a bad_alloc exception  (clause  _except_,
     _lib.bad.alloc_); it returns a non-null pointer otherwise If the allo-
     cation function is declared  with  an  empty  exception-specification,
     throw(), it returns null to indicate failure to allocate storage and a
     non-null pointer otherwise.

     So check for a null exception spec on the op new we just called.  */

  nothrow = 0;
  if (rval)
    {
      /* The CALL_EXPR.  */
      tree t = TREE_OPERAND (rval, 0);
      /* The function.  */
      t = TREE_OPERAND (TREE_OPERAND (t, 0), 0);
      nothrow = TYPE_NOTHROW_P (TREE_TYPE (t));
    }
  check_new = (flag_check_new || nothrow) && ! use_java_new;

  if ((check_new || flag_exceptions) && rval)
    {
      alloc_expr = get_target_expr (rval);
      alloc_node = rval = TREE_OPERAND (alloc_expr, 0);
    }
  else
    alloc_expr = NULL_TREE;

  /* if rval is NULL_TREE I don't have to allocate it, but are we totally
     sure we have some extra bytes in that case for the BI_header_size
     cookies? And how does that interact with the code below? (mrs) */
  /* Finish up some magic for new'ed arrays */
  if (use_cookie && rval != NULL_TREE)
    {
      tree cookie, exp1;
      rval = convert (string_type_node, rval); /* for ptr arithmetic */
      rval = save_expr (build_binary_op (PLUS_EXPR, rval, cookie_size));
      /* Store the number of bytes allocated so that we can know how
	 many elements to destroy later.  */
      if (flag_new_abi)
	{
	  /* Under the new ABI, we use the last sizeof (size_t) bytes
	     to store the number of elements.  */
	  cookie = build_indirect_ref (build (MINUS_EXPR,
					      build_pointer_type (sizetype),
					      rval,
					      size_in_bytes (sizetype)),
				       NULL_PTR);
	  exp1 = build (MODIFY_EXPR, void_type_node, cookie, nelts);
	}
      else
	{
	  cookie 
	    = build_indirect_ref (build (MINUS_EXPR,
					 build_pointer_type (BI_header_type),
					 rval, cookie_size), NULL_PTR);
	  exp1 = build (MODIFY_EXPR, void_type_node,
			build_component_ref (cookie, nelts_identifier,
					     NULL_TREE, 0),
			nelts);
	}

      /* Build `(cookie = nelts, rval)' and use that as the complete
	 expression.  */
      rval = cp_convert (build_pointer_type (true_type), rval);
      rval = build_compound_expr
	(tree_cons (NULL_TREE, exp1,
		    build_tree_list (NULL_TREE, rval)));
    }

  if (rval == error_mark_node)
    return error_mark_node;

  /* Don't call any constructors or do any initialization.  */
  if (init == void_type_node)
    goto done;

  if (TYPE_NEEDS_CONSTRUCTING (type) || init)
    {
      if (! TYPE_NEEDS_CONSTRUCTING (type)
	  && ! IS_AGGR_TYPE (type) && ! has_array)
	{
	  /* We are processing something like `new int (10)', which
	     means allocate an int, and initialize it with 10.  */
	  tree deref;
	  tree deref_type;

	  /* At present RVAL is a temporary variable, created to hold
	     the value from the call to `operator new'.  We transform
	     it to (*RVAL = INIT, RVAL).  */
	  rval = save_expr (rval);
	  deref = build_indirect_ref (rval, NULL_PTR);

	  /* Even for something like `new const int (10)' we must
	     allow the expression to be non-const while we do the
	     initialization.  */
	  deref_type = TREE_TYPE (deref);
	  if (CP_TYPE_CONST_P (deref_type))
	    TREE_TYPE (deref) 
	      = cp_build_qualified_type (deref_type,
					 CP_TYPE_QUALS (deref_type) 
					 & ~TYPE_QUAL_CONST);
	  TREE_READONLY (deref) = 0;

	  if (TREE_CHAIN (init) != NULL_TREE)
	    pedwarn ("initializer list being treated as compound expression");
	  else if (TREE_CODE (init) == CONSTRUCTOR)
	    {
	      pedwarn ("initializer list appears where operand should be used");
	      init = TREE_OPERAND (init, 1);
	    }
	  init = build_compound_expr (init);

	  init = convert_for_initialization (deref, type, init, LOOKUP_NORMAL,
					     "new", NULL_TREE, 0);
	  rval = build (COMPOUND_EXPR, TREE_TYPE (rval),
			build_modify_expr (deref, NOP_EXPR, init),
			rval);
	  TREE_NO_UNUSED_WARNING (rval) = 1;
	  TREE_SIDE_EFFECTS (rval) = 1;
	}
      else if (! has_array)
	{
	  tree newrval;
	  /* Constructors are never virtual. If it has an initialization, we
	     need to complain if we aren't allowed to use the ctor that took
	     that argument.  */
	  int flags = LOOKUP_NORMAL|LOOKUP_NONVIRTUAL|LOOKUP_COMPLAIN;

	  if (rval && TYPE_USES_VIRTUAL_BASECLASSES (true_type))
	    {
	      init = tree_cons (NULL_TREE, integer_one_node, init);
	      flags |= LOOKUP_HAS_IN_CHARGE;
	    }

	  if (use_java_new)
	    rval = save_expr (rval);
	  newrval = rval;

	  if (newrval && TREE_CODE (TREE_TYPE (newrval)) == POINTER_TYPE)
	    newrval = build_indirect_ref (newrval, NULL_PTR);

	  newrval = build_method_call (newrval, ctor_identifier,
				       init, TYPE_BINFO (true_type), flags);

	  if (newrval == NULL_TREE || newrval == error_mark_node)
	    return error_mark_node;

	  /* Java constructors compiled by jc1 do not return this. */
	  if (use_java_new)
	    newrval = build (COMPOUND_EXPR, TREE_TYPE (newrval),
			     newrval, rval);
	  rval = newrval;
	  TREE_HAS_CONSTRUCTOR (rval) = 1;
	}
      else
	rval = (build_vec_init
		(NULL_TREE, 
		 save_expr (rval),
		 build_binary_op (MINUS_EXPR, nelts, integer_one_node),
		 init,
		 /*from_array=*/0));

      /* If any part of the object initialization terminates by throwing an
	 exception and a suitable deallocation function can be found, the
	 deallocation function is called to free the memory in which the
	 object was being constructed, after which the exception continues
	 to propagate in the context of the new-expression. If no
	 unambiguous matching deallocation function can be found,
	 propagating the exception does not cause the object's memory to be
	 freed.  */
      if (flag_exceptions && alloc_expr && ! use_java_new)
	{
	  enum tree_code dcode = has_array ? VEC_DELETE_EXPR : DELETE_EXPR;
	  tree cleanup, fn = NULL_TREE;
	  int flags = (LOOKUP_NORMAL 
		       | (globally_qualified_p * LOOKUP_GLOBAL));

	  /* The Standard is unclear here, but the right thing to do
             is to use the same method for finding deallocation
             functions that we use for finding allocation functions.  */
	  flags |= LOOKUP_SPECULATIVELY;

	  /* We expect alloc_expr to look like a TARGET_EXPR around
	     a NOP_EXPR around the CALL_EXPR we want.  */
	  fn = TREE_OPERAND (alloc_expr, 1);
	  fn = TREE_OPERAND (fn, 0);

	  cleanup = build_op_delete_call (dcode, alloc_node, size, flags, fn);

	  /* Ack!  First we allocate the memory.  Then we set our sentry
	     variable to true, and expand a cleanup that deletes the memory
	     if sentry is true.  Then we run the constructor and store the
	     returned pointer in buf.  Then we clear sentry and return buf.  */

	  if (cleanup)
	    {
	      tree end, sentry, begin, buf, t = TREE_TYPE (rval);

	      begin = get_target_expr (boolean_true_node);
	      sentry = TREE_OPERAND (begin, 0);

	      TREE_OPERAND (begin, 2)
		= build (COND_EXPR, void_type_node, sentry,
			 cleanup, void_zero_node);

	      rval = get_target_expr (rval);

	      end = build (MODIFY_EXPR, TREE_TYPE (sentry),
			   sentry, boolean_false_node);

	      buf = TREE_OPERAND (rval, 0);

	      rval = build (COMPOUND_EXPR, t, begin,
			    build (COMPOUND_EXPR, t, rval,
				   build (COMPOUND_EXPR, t, end, buf)));
	    }
	}
    }
  else if (CP_TYPE_CONST_P (true_type))
    cp_error ("uninitialized const in `new' of `%#T'", true_type);

 done:

  if (alloc_expr && rval == alloc_node)
    {
      rval = TREE_OPERAND (alloc_expr, 1);
      alloc_expr = NULL_TREE;
    }

  if (check_new && alloc_expr)
    {
      /* Did we modify the storage?  */
      tree ifexp = build_binary_op (NE_EXPR, alloc_node,
				    integer_zero_node);
      rval = build_conditional_expr (ifexp, rval, alloc_node);
    }

  if (alloc_expr)
    rval = build (COMPOUND_EXPR, TREE_TYPE (rval), alloc_expr, rval);

  if (rval && TREE_TYPE (rval) != build_pointer_type (type))
    {
      /* The type of new int [3][3] is not int *, but int [3] * */
      rval = build_c_cast (build_pointer_type (type), rval);
    }

  return rval;
}

static tree
build_vec_delete_1 (base, maxindex, type, auto_delete_vec, use_global_delete)
     tree base, maxindex, type;
     tree auto_delete_vec;
     int use_global_delete;
{
  tree virtual_size;
  tree ptype = build_pointer_type (type = complete_type (type));
  tree size_exp = size_in_bytes (type);

  /* Temporary variables used by the loop.  */
  tree tbase, tbase_init;

  /* This is the body of the loop that implements the deletion of a
     single element, and moves temp variables to next elements.  */
  tree body;

  /* This is the LOOP_EXPR that governs the deletion of the elements.  */
  tree loop;

  /* This is the thing that governs what to do after the loop has run.  */
  tree deallocate_expr = 0;

  /* This is the BIND_EXPR which holds the outermost iterator of the
     loop.  It is convenient to set this variable up and test it before
     executing any other code in the loop.
     This is also the containing expression returned by this function.  */
  tree controller = NULL_TREE;

  if (! IS_AGGR_TYPE (type) || TYPE_HAS_TRIVIAL_DESTRUCTOR (type))
    {
      loop = integer_zero_node;
      goto no_destructor;
    }

  /* The below is short by BI_header_size */
  virtual_size = size_binop (MULT_EXPR, size_exp,
			     convert (sizetype, maxindex));

  tbase = create_temporary_var (ptype);
  tbase_init = build_modify_expr (tbase, NOP_EXPR,
				  fold (build (PLUS_EXPR, ptype,
					       base,
					       virtual_size)));
  DECL_REGISTER (tbase) = 1;
  controller = build (BIND_EXPR, void_type_node, tbase, NULL_TREE, NULL_TREE);
  TREE_SIDE_EFFECTS (controller) = 1;

  body = NULL_TREE;

  body = tree_cons (NULL_TREE,
		    build_delete (ptype, tbase, integer_two_node,
				  LOOKUP_NORMAL|LOOKUP_DESTRUCTOR, 1),
		    body);

  body = tree_cons (NULL_TREE,
		    build_modify_expr (tbase, NOP_EXPR, build (MINUS_EXPR, ptype, tbase, size_exp)),
		    body);

  body = tree_cons (NULL_TREE,
		    build (EXIT_EXPR, void_type_node,
			   build (EQ_EXPR, boolean_type_node, base, tbase)),
		    body);

  loop = build (LOOP_EXPR, void_type_node, build_compound_expr (body));

  loop = tree_cons (NULL_TREE, tbase_init,
		    tree_cons (NULL_TREE, loop, NULL_TREE));
  loop = build_compound_expr (loop);

 no_destructor:
  /* If the delete flag is one, or anything else with the low bit set,
     delete the storage.  */
  if (auto_delete_vec == integer_zero_node)
    deallocate_expr = integer_zero_node;
  else
    {
      tree base_tbd;

      /* The below is short by BI_header_size */
      virtual_size = size_binop (MULT_EXPR, size_exp,
				 convert (sizetype, maxindex));

      if (! TYPE_VEC_NEW_USES_COOKIE (type))
	/* no header */
	base_tbd = base;
      else
	{
	  tree cookie_size;

	  cookie_size = get_cookie_size (type);
	  base_tbd 
	    = cp_convert (ptype,
			  build_binary_op (MINUS_EXPR,
					   cp_convert (string_type_node, base),
					   cookie_size));
	  /* True size with header.  */
	  virtual_size = size_binop (PLUS_EXPR, virtual_size, cookie_size);
	}
      deallocate_expr = build_x_delete (base_tbd,
					2 | use_global_delete,
					virtual_size);
      deallocate_expr = fold (build (COND_EXPR, void_type_node,
				     fold (build (BIT_AND_EXPR,
						  integer_type_node,
						  auto_delete_vec,
						  integer_one_node)),
				     deallocate_expr, integer_zero_node));
    }

  if (loop && deallocate_expr != integer_zero_node)
    {
      body = tree_cons (NULL_TREE, loop,
			tree_cons (NULL_TREE, deallocate_expr, NULL_TREE));
      body = build_compound_expr (body);
    }
  else
    body = loop;

  /* Outermost wrapper: If pointer is null, punt.  */
  body = fold (build (COND_EXPR, void_type_node,
		      fold (build (NE_EXPR, boolean_type_node, base,
				   integer_zero_node)),
		      body, integer_zero_node));
  body = build1 (NOP_EXPR, void_type_node, body);

  if (controller)
    {
      TREE_OPERAND (controller, 1) = body;
      return controller;
    }
  else
    return cp_convert (void_type_node, body);
}

tree
create_temporary_var (type)
     tree type;
{
  tree decl;
 
  decl = build_decl (VAR_DECL, NULL_TREE, type);
  TREE_USED (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_SOURCE_FILE (decl) = input_filename;
  DECL_SOURCE_LINE (decl) = lineno;
  DECL_IGNORED_P (decl) = 1;
  DECL_CONTEXT (decl) = current_function_decl;

  return decl;
}

/* Create a new temporary variable of the indicated TYPE, initialized
   to INIT.

   It is not entered into current_binding_level, because that breaks
   things when it comes time to do final cleanups (which take place
   "outside" the binding contour of the function).  */

static tree
get_temp_regvar (type, init)
     tree type, init;
{
  tree decl;

  decl = create_temporary_var (type);
  if (building_stmt_tree ())
    add_decl_stmt (decl);
  if (!building_stmt_tree ())
    DECL_RTL (decl) = assign_temp (type, 2, 0, 1);
  finish_expr_stmt (build_modify_expr (decl, INIT_EXPR, init));

  return decl;
}

/* `build_vec_init' returns tree structure that performs
   initialization of a vector of aggregate types.

   DECL is passed only for error reporting, and provides line number
   and source file name information.
   BASE is the space where the vector will be.  For a vector of Ts,
     the type of BASE is `T*'.
   MAXINDEX is the maximum index of the array (one less than the
	    number of elements).
   INIT is the (possibly NULL) initializer.

   FROM_ARRAY is 0 if we should init everything with INIT
   (i.e., every element initialized from INIT).
   FROM_ARRAY is 1 if we should index into INIT in parallel
   with initialization of DECL.
   FROM_ARRAY is 2 if we should index into INIT in parallel,
   but use assignment instead of initialization.  */

tree
build_vec_init (decl, base, maxindex, init, from_array)
     tree decl, base, maxindex, init;
     int from_array;
{
  tree rval;
  tree base2 = NULL_TREE;
  tree size;
  tree itype = NULL_TREE;
  tree iterator;
  /* The type of an element in the array.  */
  tree type;
  /* The type of a pointer to an element in the array.  */
  tree ptype;
  tree stmt_expr;
  tree compound_stmt;
  int destroy_temps;
  tree try_block = NULL_TREE;
  tree try_body = NULL_TREE;
  int num_initialized_elts = 0;

  maxindex = cp_convert (ptrdiff_type_node, maxindex);
  if (maxindex == error_mark_node)
    return error_mark_node;

  type = TREE_TYPE (TREE_TYPE (base));
  ptype = build_pointer_type (type);
  size = size_in_bytes (type);

  /* The code we are generating looks like:

       T* t1 = (T*) base;
       T* rval = base;
       ptrdiff_t iterator = maxindex;
       try {
         ... initializations from CONSTRUCTOR ...
         if (iterator != -1) {
	   do {
	     ... initialize *base ...
	     ++base;
	   } while (--iterator != -1);
	 }
       } catch (...) {
         ... destroy elements that were constructed ...
       }
       
     We can omit the try and catch blocks if we know that the
     initialization will never throw an exception, or if the array
     elements do not have destructors.  If we have a CONSTRUCTOR to
     give us initialization information, we emit code to initialize
     each of the elements before the loop in the try block, and then
     iterate over fewer elements.  We can omit the loop completely if
     the elements of the array do not have constructors.  

     We actually wrap the entire body of the above in a STMT_EXPR, for
     tidiness.  

     When copying from array to another, when the array elements have
     only trivial copy constructors, we should use __builtin_memcpy
     rather than generating a loop.  That way, we could take advantage
     of whatever cleverness the back-end has for dealing with copies
     of blocks of memory.  */

  begin_init_stmts (&stmt_expr, &compound_stmt);
  destroy_temps = stmts_are_full_exprs_p;
  stmts_are_full_exprs_p = 0;
  rval = get_temp_regvar (ptype, 
			  cp_convert (ptype, default_conversion (base)));
  base = get_temp_regvar (ptype, rval);
  iterator = get_temp_regvar (ptrdiff_type_node, maxindex);

  /* Protect the entire array initialization so that we can destroy
     the partially constructed array if an exception is thrown.  */
  if (flag_exceptions && TYPE_HAS_NONTRIVIAL_DESTRUCTOR (type))
    {
      try_block = begin_try_block ();
      try_body = begin_compound_stmt (/*has_no_scope=*/1);
    }

  if (init != NULL_TREE && TREE_CODE (init) == CONSTRUCTOR
      && (!decl || same_type_p (TREE_TYPE (init), TREE_TYPE (decl))))
    {
      /* Do non-default initialization resulting from brace-enclosed
	 initializers.  */

      tree elts;
      from_array = 0;

      for (elts = CONSTRUCTOR_ELTS (init); elts; elts = TREE_CHAIN (elts))
	{
	  tree elt = TREE_VALUE (elts);
	  tree baseref = build1 (INDIRECT_REF, type, base);

	  num_initialized_elts++;

	  if (IS_AGGR_TYPE (type) || TREE_CODE (type) == ARRAY_TYPE)
	    finish_expr_stmt (build_aggr_init (baseref, elt, 0));
	  else
	    finish_expr_stmt (build_modify_expr (baseref, NOP_EXPR,
						 elt));

	  finish_expr_stmt (build_modify_expr 
			    (base, 
			     NOP_EXPR,
			     build (PLUS_EXPR, build_pointer_type (type),
				    base, size)));
	  finish_expr_stmt (build_modify_expr
			    (iterator,
			     NOP_EXPR,
			     build (MINUS_EXPR, ptrdiff_type_node,
				    iterator, integer_one_node)));
	}

      /* Clear out INIT so that we don't get confused below.  */
      init = NULL_TREE;
    }
  else if (from_array)
    {
      /* If initializing one array from another, initialize element by
	 element.  We rely upon the below calls the do argument
	 checking.  */ 
      if (decl == NULL_TREE)
	{
	  sorry ("initialization of array from dissimilar array type");
	  return error_mark_node;
	}
      if (init)
	{
	  base2 = default_conversion (init);
	  itype = TREE_TYPE (base2);
	  base2 = get_temp_regvar (itype, base2);
	  itype = TREE_TYPE (itype);
	}
      else if (TYPE_LANG_SPECIFIC (type)
	       && TYPE_NEEDS_CONSTRUCTING (type)
	       && ! TYPE_HAS_DEFAULT_CONSTRUCTOR (type))
	{
	  error ("initializer ends prematurely");
	  return error_mark_node;
	}
    }

  /* Now, default-initialize any remaining elements.  We don't need to
     do that if a) the type does not need constructing, or b) we've
     already initialized all the elements.

     We do need to keep going if we're copying an array.  */

  if (from_array
      || (TYPE_NEEDS_CONSTRUCTING (type)
	  && ! (TREE_CODE (maxindex) == INTEGER_CST
		&& (num_initialized_elts
		    == (HOST_WIDE_INT) TREE_INT_CST_LOW (maxindex) + 1))))
    {
      /* If the ITERATOR is equal to -1, then we don't have to loop;
	 we've already initialized all the elements.  */
      tree if_stmt;
      tree do_stmt;
      tree do_body;
      tree elt_init;

      if_stmt = begin_if_stmt ();
      finish_if_stmt_cond (build (NE_EXPR, boolean_type_node,
				  iterator, minus_one_node),
			   if_stmt);

      /* Otherwise, loop through the elements.  */
      do_stmt = begin_do_stmt ();
      do_body = begin_compound_stmt (/*has_no_scope=*/1);

      /* When we're not building a statement-tree, things are a little
	 complicated.  If, when we recursively call build_aggr_init,
	 an expression containing a TARGET_EXPR is expanded, then it
	 may get a cleanup.  Then, the result of that expression is
	 passed to finish_expr_stmt, which will call
	 expand_start_target_temps/expand_end_target_temps.  However,
	 the latter call will not cause the cleanup to run because
	 that block will still be on the block stack.  So, we call
	 expand_start_target_temps here manually; the corresponding
	 call to expand_end_target_temps below will cause the cleanup
	 to be performed.  */
      if (!building_stmt_tree ())
	expand_start_target_temps ();

      if (from_array)
	{
	  tree to = build1 (INDIRECT_REF, type, base);
	  tree from;

	  if (base2)
	    from = build1 (INDIRECT_REF, itype, base2);
	  else
	    from = NULL_TREE;

	  if (from_array == 2)
	    elt_init = build_modify_expr (to, NOP_EXPR, from);
	  else if (TYPE_NEEDS_CONSTRUCTING (type))
	    elt_init = build_aggr_init (to, from, 0);
	  else if (from)
	    elt_init = build_modify_expr (to, NOP_EXPR, from);
	  else
	    my_friendly_abort (57);
	}
      else if (TREE_CODE (type) == ARRAY_TYPE)
	{
	  if (init != 0)
	    sorry ("cannot initialize multi-dimensional array with initializer");
	  elt_init = (build_vec_init 
		      (decl, 
		       build1 (NOP_EXPR, 
			       build_pointer_type (TREE_TYPE (type)),
			       base),
		       array_type_nelts (type), 0, 0));
	}
      else
	elt_init = build_aggr_init (build1 (INDIRECT_REF, type, base), 
				    init, 0);
      
      /* The initialization of each array element is a
	 full-expression.  */
      if (!building_stmt_tree ())
	{
	  finish_expr_stmt (elt_init);
	  expand_end_target_temps ();
	}
      else
	{
	  stmts_are_full_exprs_p = 1;
	  finish_expr_stmt (elt_init);
	  stmts_are_full_exprs_p = 0;
	}

      finish_expr_stmt (build_modify_expr
			(base,
			 NOP_EXPR,
			 build (PLUS_EXPR, build_pointer_type (type), 
				base, size)));
      if (base2)
	finish_expr_stmt (build_modify_expr
			  (base2,
			   NOP_EXPR,
			   build (PLUS_EXPR, build_pointer_type (type), 
				  base2, size)));

      finish_compound_stmt (/*has_no_scope=*/1, do_body);
      finish_do_body (do_stmt);
      finish_do_stmt (build (NE_EXPR, boolean_type_node,
			     build (PREDECREMENT_EXPR, 
				    ptrdiff_type_node, 
				    iterator,
				    integer_one_node), 
			     minus_one_node),
		      do_stmt);

      finish_then_clause (if_stmt);
      finish_if_stmt ();
    }

  /* Make sure to cleanup any partially constructed elements.  */
  if (flag_exceptions && TYPE_HAS_NONTRIVIAL_DESTRUCTOR (type))
    {
      tree e;

      finish_compound_stmt (/*has_no_scope=*/1, try_body);
      finish_cleanup_try_block (try_block);
      e = build_vec_delete_1 (rval,
			      build_binary_op (MINUS_EXPR, maxindex, 
					       iterator),
			      type,
			      /*auto_delete_vec=*/integer_zero_node,
			      /*use_global_delete=*/0);
      finish_cleanup (e, try_block);
    }

  /* The value of the array initialization is the address of the
     first element in the array.  */
  finish_expr_stmt (rval);

  stmt_expr = finish_init_stmts (stmt_expr, compound_stmt);
  stmts_are_full_exprs_p = destroy_temps;
  return stmt_expr;
}

/* Free up storage of type TYPE, at address ADDR.

   TYPE is a POINTER_TYPE and can be ptr_type_node for no special type
   of pointer.

   VIRTUAL_SIZE is the amount of storage that was allocated, and is
   used as the second argument to operator delete.  It can include
   things like padding and magic size cookies.  It has virtual in it,
   because if you have a base pointer and you delete through a virtual
   destructor, it should be the size of the dynamic object, not the
   static object, see Free Store 12.5 ISO C++.

   This does not call any destructors.  */

tree
build_x_delete (addr, which_delete, virtual_size)
     tree addr;
     int which_delete;
     tree virtual_size;
{
  int use_global_delete = which_delete & 1;
  int use_vec_delete = !!(which_delete & 2);
  enum tree_code code = use_vec_delete ? VEC_DELETE_EXPR : DELETE_EXPR;
  int flags = LOOKUP_NORMAL | (use_global_delete * LOOKUP_GLOBAL);

  return build_op_delete_call (code, addr, virtual_size, flags, NULL_TREE);
}

/* Generate a call to a destructor. TYPE is the type to cast ADDR to.
   ADDR is an expression which yields the store to be destroyed.
   AUTO_DELETE is nonzero if a call to DELETE should be made or not.
   If in the program, (AUTO_DELETE & 2) is non-zero, we tear down the
   virtual baseclasses.
   If in the program, (AUTO_DELETE & 1) is non-zero, then we deallocate.

   FLAGS is the logical disjunction of zero or more LOOKUP_
   flags.  See cp-tree.h for more info.

   This function does not delete an object's virtual base classes.  */

tree
build_delete (type, addr, auto_delete, flags, use_global_delete)
     tree type, addr;
     tree auto_delete;
     int flags;
     int use_global_delete;
{
  tree member;
  tree expr;
  tree ref;

  if (addr == error_mark_node)
    return error_mark_node;

  /* Can happen when CURRENT_EXCEPTION_OBJECT gets its type
     set to `error_mark_node' before it gets properly cleaned up.  */
  if (type == error_mark_node)
    return error_mark_node;

  type = TYPE_MAIN_VARIANT (type);

  if (TREE_CODE (type) == POINTER_TYPE)
    {
      type = TYPE_MAIN_VARIANT (TREE_TYPE (type));
      if (type != void_type_node && !complete_type_or_else (type, addr))
	return error_mark_node;
      if (TREE_CODE (type) == ARRAY_TYPE)
	goto handle_array;
      if (! IS_AGGR_TYPE (type))
	{
	  /* Call the builtin operator delete.  */
	  return build_builtin_delete_call (addr);
	}
      if (TREE_SIDE_EFFECTS (addr))
	addr = save_expr (addr);

      /* throw away const and volatile on target type of addr */
      addr = convert_force (build_pointer_type (type), addr, 0);
      ref = build_indirect_ref (addr, NULL_PTR);
    }
  else if (TREE_CODE (type) == ARRAY_TYPE)
    {
    handle_array:
      if (TREE_SIDE_EFFECTS (addr))
	addr = save_expr (addr);
      if (TYPE_DOMAIN (type) == NULL_TREE)
	{
	  error ("unknown array size in delete");
	  return error_mark_node;
	}
      return build_vec_delete (addr, array_type_nelts (type),
			       auto_delete, use_global_delete);
    }
  else
    {
      /* Don't check PROTECT here; leave that decision to the
	 destructor.  If the destructor is accessible, call it,
	 else report error.  */
      addr = build_unary_op (ADDR_EXPR, addr, 0);
      if (TREE_SIDE_EFFECTS (addr))
	addr = save_expr (addr);

      if (TREE_CONSTANT (addr))
	addr = convert_pointer_to (type, addr);
      else
	addr = convert_force (build_pointer_type (type), addr, 0);

      ref = build_indirect_ref (addr, NULL_PTR);
    }

  my_friendly_assert (IS_AGGR_TYPE (type), 220);

  if (TYPE_HAS_TRIVIAL_DESTRUCTOR (type))
    {
      if (auto_delete == integer_zero_node)
	return void_zero_node;

      return build_op_delete_call
	(DELETE_EXPR, addr, c_sizeof_nowarn (type),
	 LOOKUP_NORMAL | (use_global_delete * LOOKUP_GLOBAL),
	 NULL_TREE);
    }

  /* Below, we will reverse the order in which these calls are made.
     If we have a destructor, then that destructor will take care
     of the base classes; otherwise, we must do that here.  */
  if (TYPE_HAS_DESTRUCTOR (type))
    {
      tree passed_auto_delete;
      tree do_delete = NULL_TREE;
      tree ifexp;

      if (use_global_delete)
	{
	  tree cond = fold (build (BIT_AND_EXPR, integer_type_node,
				   auto_delete, integer_one_node));
	  tree call = build_builtin_delete_call (addr);

	  cond = fold (build (COND_EXPR, void_type_node, cond,
			      call, void_zero_node));
	  if (cond != void_zero_node)
	    do_delete = cond;

	  passed_auto_delete = fold (build (BIT_AND_EXPR, integer_type_node,
					    auto_delete, integer_two_node));
	}
      else
	passed_auto_delete = auto_delete;

      expr = build_method_call
	(ref, dtor_identifier, build_tree_list (NULL_TREE, passed_auto_delete),
	 NULL_TREE, flags);

      if (do_delete)
	expr = build (COMPOUND_EXPR, void_type_node, expr, do_delete);

      if (flags & LOOKUP_DESTRUCTOR)
	/* Explicit destructor call; don't check for null pointer.  */
	ifexp = integer_one_node;
      else
	/* Handle deleting a null pointer.  */
	ifexp = fold (build_binary_op (NE_EXPR, addr, integer_zero_node));

      if (ifexp != integer_one_node)
	expr = build (COND_EXPR, void_type_node,
		      ifexp, expr, void_zero_node);

      return expr;
    }
  else
    {
      /* We only get here from finish_function for a destructor.  */
      tree binfos = BINFO_BASETYPES (TYPE_BINFO (type));
      int i, n_baseclasses = binfos ? TREE_VEC_LENGTH (binfos) : 0;
      tree base_binfo = n_baseclasses > 0 ? TREE_VEC_ELT (binfos, 0) : NULL_TREE;
      tree exprstmt = NULL_TREE;
      tree parent_auto_delete = auto_delete;
      tree cond;

      /* Set this again before we call anything, as we might get called
	 recursively.  */
      TYPE_HAS_DESTRUCTOR (type) = 1;

      /* If we have member delete or vbases, we call delete in
	 finish_function.  */
      if (auto_delete == integer_zero_node)
	cond = NULL_TREE;
      else if (base_binfo == NULL_TREE
	       || TYPE_HAS_TRIVIAL_DESTRUCTOR (BINFO_TYPE (base_binfo)))
	{
	  cond = build (COND_EXPR, void_type_node,
			build (BIT_AND_EXPR, integer_type_node, auto_delete, integer_one_node),
			build_builtin_delete_call (addr),
			void_zero_node);
	}
      else
	cond = NULL_TREE;

      if (cond)
	exprstmt = build_tree_list (NULL_TREE, cond);

      if (base_binfo
	  && ! TREE_VIA_VIRTUAL (base_binfo)
	  && TYPE_HAS_NONTRIVIAL_DESTRUCTOR (BINFO_TYPE (base_binfo)))
	{
	  tree this_auto_delete;

	  if (BINFO_OFFSET_ZEROP (base_binfo))
	    this_auto_delete = parent_auto_delete;
	  else
	    this_auto_delete = integer_zero_node;

	  expr = build_scoped_method_call
	    (ref, base_binfo, dtor_identifier,
	     build_tree_list (NULL_TREE, this_auto_delete));
	  exprstmt = tree_cons (NULL_TREE, expr, exprstmt);
	}

      /* Take care of the remaining baseclasses.  */
      for (i = 1; i < n_baseclasses; i++)
	{
	  base_binfo = TREE_VEC_ELT (binfos, i);
	  if (TYPE_HAS_TRIVIAL_DESTRUCTOR (BINFO_TYPE (base_binfo))
	      || TREE_VIA_VIRTUAL (base_binfo))
	    continue;

	  expr = build_scoped_method_call
	    (ref, base_binfo, dtor_identifier,
	     build_tree_list (NULL_TREE, integer_zero_node));

	  exprstmt = tree_cons (NULL_TREE, expr, exprstmt);
	}

      for (member = TYPE_FIELDS (type); member; member = TREE_CHAIN (member))
	{
	  if (TREE_CODE (member) != FIELD_DECL)
	    continue;
	  if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (TREE_TYPE (member)))
	    {
	      tree this_member = build_component_ref (ref, DECL_NAME (member), NULL_TREE, 0);
	      tree this_type = TREE_TYPE (member);
	      expr = build_delete (this_type, this_member, integer_two_node, flags, 0);
	      exprstmt = tree_cons (NULL_TREE, expr, exprstmt);
	    }
	}

      if (exprstmt)
	return build_compound_expr (exprstmt);
      /* Virtual base classes make this function do nothing.  */
      return void_zero_node;
    }
}

/* For type TYPE, delete the virtual baseclass objects of DECL.  */

tree
build_vbase_delete (type, decl)
     tree type, decl;
{
  tree vbases = CLASSTYPE_VBASECLASSES (type);
  tree result = NULL_TREE;
  tree addr = build_unary_op (ADDR_EXPR, decl, 0);

  my_friendly_assert (addr != error_mark_node, 222);

  while (vbases)
    {
      tree this_addr = convert_force (build_pointer_type (BINFO_TYPE (vbases)),
				      addr, 0);
      result = tree_cons (NULL_TREE,
			  build_delete (TREE_TYPE (this_addr), this_addr,
					integer_zero_node,
					LOOKUP_NORMAL|LOOKUP_DESTRUCTOR, 0),
			  result);
      vbases = TREE_CHAIN (vbases);
    }
  return build_compound_expr (nreverse (result));
}

/* Build a C++ vector delete expression.
   MAXINDEX is the number of elements to be deleted.
   ELT_SIZE is the nominal size of each element in the vector.
   BASE is the expression that should yield the store to be deleted.
   This function expands (or synthesizes) these calls itself.
   AUTO_DELETE_VEC says whether the container (vector) should be deallocated.

   This also calls delete for virtual baseclasses of elements of the vector.

   Update: MAXINDEX is no longer needed.  The size can be extracted from the
   start of the vector for pointers, and from the type for arrays.  We still
   use MAXINDEX for arrays because it happens to already have one of the
   values we'd have to extract.  (We could use MAXINDEX with pointers to
   confirm the size, and trap if the numbers differ; not clear that it'd
   be worth bothering.)  */

tree
build_vec_delete (base, maxindex, auto_delete_vec, use_global_delete)
     tree base, maxindex;
     tree auto_delete_vec;
     int use_global_delete;
{
  tree type;

  if (TREE_CODE (base) == OFFSET_REF)
    base = resolve_offset_ref (base);

  type = TREE_TYPE (base);

  base = stabilize_reference (base);

  /* Since we can use base many times, save_expr it.  */
  if (TREE_SIDE_EFFECTS (base))
    base = save_expr (base);

  if (TREE_CODE (type) == POINTER_TYPE)
    {
      /* Step back one from start of vector, and read dimension.  */
      tree cookie_addr;

      if (flag_new_abi)
	{
	  cookie_addr = build (MINUS_EXPR,
			       build_pointer_type (sizetype),
			       base,
			       TYPE_SIZE_UNIT (sizetype));
	  maxindex = build_indirect_ref (cookie_addr, NULL_PTR);
	}
      else
	{
	  tree cookie;

	  cookie_addr = build (MINUS_EXPR, build_pointer_type (BI_header_type),
			       base, BI_header_size);
	  cookie = build_indirect_ref (cookie_addr, NULL_PTR);
	  maxindex = build_component_ref (cookie, nelts_identifier, 
					  NULL_TREE, 0);
	}

      type = strip_array_types (TREE_TYPE (type));
    }
  else if (TREE_CODE (type) == ARRAY_TYPE)
    {
      /* get the total number of things in the array, maxindex is a bad name */
      maxindex = array_type_nelts_total (type);
      type = strip_array_types (type);
      base = build_unary_op (ADDR_EXPR, base, 1);
    }
  else
    {
      if (base != error_mark_node)
	error ("type to vector delete is neither pointer or array type");
      return error_mark_node;
    }

  return build_vec_delete_1 (base, maxindex, type, auto_delete_vec,
			     use_global_delete);
}
