/* Handle initialization things in C++.
   Copyright (C) 1987, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002 Free Software Foundation, Inc.
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
#include "expr.h"
#include "cp-tree.h"
#include "flags.h"
#include "output.h"
#include "except.h"
#include "toplev.h"
#include "ggc.h"

static void expand_aggr_vbase_init_1 PARAMS ((tree, tree, tree, tree));
static void construct_virtual_bases PARAMS ((tree, tree, tree, tree, tree));
static void expand_aggr_init_1 PARAMS ((tree, tree, tree, tree, int));
static void expand_default_init PARAMS ((tree, tree, tree, tree, int));
static tree build_vec_delete_1 PARAMS ((tree, tree, tree, special_function_kind, int));
static void perform_member_init PARAMS ((tree, tree, int));
static void sort_base_init PARAMS ((tree, tree, tree *, tree *));
static tree build_builtin_delete_call PARAMS ((tree));
static int member_init_ok_or_else PARAMS ((tree, tree, tree));
static void expand_virtual_init PARAMS ((tree, tree));
static tree sort_member_init PARAMS ((tree, tree));
static tree initializing_context PARAMS ((tree));
static void expand_cleanup_for_base PARAMS ((tree, tree));
static tree get_temp_regvar PARAMS ((tree, tree));
static tree dfs_initialize_vtbl_ptrs PARAMS ((tree, void *));
static tree build_default_init PARAMS ((tree));
static tree build_new_1	PARAMS ((tree));
static tree get_cookie_size PARAMS ((tree));
static tree build_dtor_call PARAMS ((tree, special_function_kind, int));
static tree build_field_list PARAMS ((tree, tree, int *));
static tree build_vtbl_address PARAMS ((tree));

/* Set up local variable for this file.  MUST BE CALLED AFTER
   INIT_DECL_PROCESSING.  */

static tree BI_header_type;

void init_init_processing ()
{
  tree fields[1];

  /* Define the structure that holds header information for
     arrays allocated via operator new.  */
  BI_header_type = make_aggr_type (RECORD_TYPE);
  fields[0] = build_decl (FIELD_DECL, nelts_identifier, sizetype);

  finish_builtin_type (BI_header_type, "__new_cookie", fields,
		       0, double_type_node);

  ggc_add_tree_root (&BI_header_type, 1);
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
  if (building_stmt_tree ())
    *stmt_expr_p = begin_stmt_expr ();
  else
    *stmt_expr_p = begin_global_stmt_expr ();
  
  if (building_stmt_tree ())
    *compound_stmt_p = begin_compound_stmt (/*has_no_scope=*/1);
}

/* Finish out the statement-expression begun by the previous call to
   begin_init_stmts.  Returns the statement-expression itself.  */

tree
finish_init_stmts (stmt_expr, compound_stmt)
     tree stmt_expr;
     tree compound_stmt;

{  
  if (building_stmt_tree ())
    finish_compound_stmt (/*has_no_scope=*/1, compound_stmt);
  
  if (building_stmt_tree ())
    {
      stmt_expr = finish_stmt_expr (stmt_expr);
      STMT_EXPR_NO_SCOPE (stmt_expr) = true;
    }
  else
    stmt_expr = finish_global_stmt_expr (stmt_expr);
  
  /* To avoid spurious warnings about unused values, we set 
     TREE_USED.  */
  if (stmt_expr)
    TREE_USED (stmt_expr) = 1;

  return stmt_expr;
}

/* Constructors */

/* Called from initialize_vtbl_ptrs via dfs_walk.  BINFO is the base
   which we want to initialize the vtable pointer for, DATA is
   TREE_LIST whose TREE_VALUE is the this ptr expression.  */

static tree
dfs_initialize_vtbl_ptrs (binfo, data)
     tree binfo;
     void *data;
{
  if ((!BINFO_PRIMARY_P (binfo) || TREE_VIA_VIRTUAL (binfo))
      && CLASSTYPE_VFIELDS (BINFO_TYPE (binfo)))
    {
      tree base_ptr = TREE_VALUE ((tree) data);

      base_ptr = build_base_path (PLUS_EXPR, base_ptr, binfo, /*nonnull=*/1);

      expand_virtual_init (binfo, base_ptr);
    }

  SET_BINFO_MARKED (binfo);

  return NULL_TREE;
}

/* Initialize all the vtable pointers in the object pointed to by
   ADDR.  */

void
initialize_vtbl_ptrs (addr)
     tree addr;
{
  tree list;
  tree type;

  type = TREE_TYPE (TREE_TYPE (addr));
  list = build_tree_list (type, addr);

  /* Walk through the hierarchy, initializing the vptr in each base
     class.  We do these in pre-order because can't find the virtual
     bases for a class until we've initialized the vtbl for that
     class.  */
  dfs_walk_real (TYPE_BINFO (type), dfs_initialize_vtbl_ptrs, 
		 NULL, dfs_unmarked_real_bases_queue_p, list);
  dfs_walk (TYPE_BINFO (type), dfs_unmark,
	    dfs_marked_real_bases_queue_p, type);
}

/* Types containing pointers to data members cannot be
   zero-initialized with zeros, because the NULL value for such
   pointers is -1.

   TYPE is a type that requires such zero initialization.  The
   returned value is the initializer.  */

tree
build_forced_zero_init (type)
     tree type;
{
  tree init = NULL;

  if (AGGREGATE_TYPE_P (type) && !TYPE_PTRMEMFUNC_P (type))
    {
      /* This is a default initialization of an aggregate, but not one of
	 non-POD class type.  We cleverly notice that the initialization
	 rules in such a case are the same as for initialization with an
	 empty brace-initialization list.  */
      init = build (CONSTRUCTOR, NULL_TREE, NULL_TREE, NULL_TREE);
    }
  else if (TREE_CODE (type) == REFERENCE_TYPE)
    /*   --if T is a reference type, no initialization is performed.  */
    return NULL_TREE;
  else
    {
      init = integer_zero_node;
      
      if (TREE_CODE (type) == ENUMERAL_TYPE)
        /* We must make enumeral types the right type. */
        init = fold (build1 (NOP_EXPR, type, init));
    }

  init = digest_init (type, init, 0);

  return init;
}

/* [dcl.init]:

  To default-initialize an object of type T means:

  --if T is a non-POD class type (clause _class_), the default construc-
    tor  for  T is called (and the initialization is ill-formed if T has
    no accessible default constructor);

  --if T is an array type, each element is default-initialized;

  --otherwise, the storage for the object is zero-initialized.

  A program that calls for default-initialization of an entity of refer-
  ence type is ill-formed.  */

static tree
build_default_init (type)
     tree type;
{
  tree init = NULL_TREE;

  if (TYPE_NEEDS_CONSTRUCTING (type))
    /* Other code will handle running the default constructor.  We can't do
       anything with a CONSTRUCTOR for arrays here, as that would imply
       copy-initialization.  */
    return NULL_TREE;

  return build_forced_zero_init (type);
}

/* Subroutine of emit_base_init.  */

static void
perform_member_init (member, init, explicit)
     tree member, init;
     int explicit;
{
  tree decl;
  tree type = TREE_TYPE (member);

  decl = build_component_ref (current_class_ref, member, NULL_TREE, explicit);

  if (decl == error_mark_node)
    return;

  /* Deal with this here, as we will get confused if we try to call the
     assignment op for an anonymous union.  This can happen in a
     synthesized copy constructor.  */
  if (ANON_AGGR_TYPE_P (type))
    {
      if (init)
	{
	  init = build (INIT_EXPR, type, decl, TREE_VALUE (init));
	  finish_expr_stmt (init);
	}
    }
  else if (TYPE_NEEDS_CONSTRUCTING (type)
	   || (init && TYPE_HAS_CONSTRUCTOR (type)))
    {
      /* Since `init' is already a TREE_LIST on the member_init_list,
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
	  finish_expr_stmt (build_vec_init (decl, TREE_VALUE (init), 1));
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
	      init = build_default_init (type);
	      if (TREE_CODE (type) == REFERENCE_TYPE)
		warning
		  ("default-initialization of `%#D', which has reference type",
		   member);
	    }
	  /* member traversal: note it leaves init NULL */
	  else if (TREE_CODE (type) == REFERENCE_TYPE)
	    pedwarn ("uninitialized reference member `%D'", member);
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

      expr = build_component_ref (current_class_ref, member, NULL_TREE,
				  explicit);
      expr = build_delete (type, expr, sfk_complete_destructor,
			   LOOKUP_NONVIRTUAL|LOOKUP_DESTRUCTOR, 0);

      if (expr != error_mark_node)
	finish_subobject (expr);
    }
}

/* Returns a TREE_LIST containing (as the TREE_PURPOSE of each node) all
   the FIELD_DECLs on the TYPE_FIELDS list for T, in reverse order.  */

static tree 
build_field_list (t, list, uses_unions_p)
     tree t;
     tree list;
     int *uses_unions_p;
{
  tree fields;

  /* Note whether or not T is a union.  */
  if (TREE_CODE (t) == UNION_TYPE)
    *uses_unions_p = 1;

  for (fields = TYPE_FIELDS (t); fields; fields = TREE_CHAIN (fields))
    {
      /* Skip CONST_DECLs for enumeration constants and so forth.  */
      if (TREE_CODE (fields) != FIELD_DECL)
	continue;
      
      /* Keep track of whether or not any fields are unions.  */
      if (TREE_CODE (TREE_TYPE (fields)) == UNION_TYPE)
	*uses_unions_p = 1;

      /* For an anonymous struct or union, we must recursively
	 consider the fields of the anonymous type.  They can be
	 directly initialized from the constructor.  */
      if (ANON_AGGR_TYPE_P (TREE_TYPE (fields)))
	{
	  /* Add this field itself.  Synthesized copy constructors
	     initialize the entire aggregate.  */
	  list = tree_cons (fields, NULL_TREE, list);
	  /* And now add the fields in the anonymous aggregate.  */
	  list = build_field_list (TREE_TYPE (fields), list, 
				   uses_unions_p);
	}
      /* Add this field.  */
      else if (DECL_NAME (fields))
	list = tree_cons (fields, NULL_TREE, list);
    }

  return list;
}

/* The MEMBER_INIT_LIST is a TREE_LIST.  The TREE_PURPOSE of each list
   gives a FIELD_DECL in T that needs initialization.  The TREE_VALUE
   gives the initializer, or list of initializer arguments.  Sort the
   MEMBER_INIT_LIST, returning a version that contains the same
   information but in the order that the fields should actually be
   initialized.  Perform error-checking in the process.  */

static tree
sort_member_init (t, member_init_list)
     tree t;
     tree member_init_list;
{
  tree init_list;
  tree last_field;
  tree init;
  int uses_unions_p;

  /* Build up a list of the various fields, in sorted order.  */
  init_list = nreverse (build_field_list (t, NULL_TREE, &uses_unions_p));

  /* Go through the explicit initializers, adding them to the
     INIT_LIST.  */
  last_field = init_list;
  for (init = member_init_list; init; init = TREE_CHAIN (init))
    {
      tree f;
      tree initialized_field;

      initialized_field = TREE_PURPOSE (init);
      my_friendly_assert (TREE_CODE (initialized_field) == FIELD_DECL,
			  20000516);

      /* If the explicit initializers are in sorted order, then the
	 INITIALIZED_FIELD will be for a field following the
	 LAST_FIELD.  */
      for (f = last_field; f; f = TREE_CHAIN (f))
	if (TREE_PURPOSE (f) == initialized_field)
	  break;

      /* Give a warning, if appropriate.  */
      if (warn_reorder && !f)
	{
	  cp_warning_at ("member initializers for `%#D'", 
			 TREE_PURPOSE (last_field));
	  cp_warning_at ("  and `%#D'", initialized_field);
	  warning ("  will be re-ordered to match declaration order");
	}

      /* Look again, from the beginning of the list.  We must find the
	 field on this loop.  */
      if (!f)
	{
	  f = init_list;
	  while (TREE_PURPOSE (f) != initialized_field)
	    f = TREE_CHAIN (f);
	}

      /* If there was already an explicit initializer for this field,
	 issue an error.  */
      if (TREE_TYPE (f))
	error ("multiple initializations given for member `%D'",
		  initialized_field);
      else
	{
	  /* Mark the field as explicitly initialized.  */
	  TREE_TYPE (f) = error_mark_node;
	  /* And insert the initializer.  */
	  TREE_VALUE (f) = TREE_VALUE (init);
	}

      /* Remember the location of the last explicitly initialized
	 field.  */
      last_field = f;
    }

  /* [class.base.init]

     If a ctor-initializer specifies more than one mem-initializer for
     multiple members of the same union (including members of
     anonymous unions), the ctor-initializer is ill-formed.  */
  if (uses_unions_p)
    {
      last_field = NULL_TREE;
      for (init = init_list; init; init = TREE_CHAIN (init))
	{
	  tree field;
	  tree field_type;
	  int done;

	  /* Skip uninitialized members.  */
	  if (!TREE_TYPE (init))
	    continue;
	  /* See if this field is a member of a union, or a member of a
	     structure contained in a union, etc.  */
	  field = TREE_PURPOSE (init);
	  for (field_type = DECL_CONTEXT (field);
	       !same_type_p (field_type, t);
	       field_type = TYPE_CONTEXT (field_type))
	    if (TREE_CODE (field_type) == UNION_TYPE)
	      break;
	  /* If this field is not a member of a union, skip it.  */
	  if (TREE_CODE (field_type) != UNION_TYPE)
	    continue;

	  /* It's only an error if we have two initializers for the same
	     union type.  */
	  if (!last_field)
	    {
	      last_field = field;
	      continue;
	    }

	  /* See if LAST_FIELD and the field initialized by INIT are
	     members of the same union.  If so, there's a problem,
	     unless they're actually members of the same structure
	     which is itself a member of a union.  For example, given:

	       union { struct { int i; int j; }; };

	     initializing both `i' and `j' makes sense.  */
	  field_type = DECL_CONTEXT (field);
	  done = 0;
	  do
	    {
	      tree last_field_type;

	      last_field_type = DECL_CONTEXT (last_field);
	      while (1)
		{
		  if (same_type_p (last_field_type, field_type))
		    {
		      if (TREE_CODE (field_type) == UNION_TYPE)
			error ("initializations for multiple members of `%T'",
				  last_field_type);
		      done = 1;
		      break;
		    }

		  if (same_type_p (last_field_type, t))
		    break;

		  last_field_type = TYPE_CONTEXT (last_field_type);
		}
	      
	      /* If we've reached the outermost class, then we're
		 done.  */
	      if (same_type_p (field_type, t))
		break;

	      field_type = TYPE_CONTEXT (field_type);
	    }
	  while (!done);

	  last_field = field;
	}
    }

  return init_list;
}

/* Like sort_member_init, but used for initializers of base classes.
   *RBASE_PTR is filled in with the initializers for non-virtual bases;
   vbase_ptr gets the virtual bases.  */

static void
sort_base_init (t, base_init_list, rbase_ptr, vbase_ptr)
     tree t;
     tree base_init_list;
     tree *rbase_ptr, *vbase_ptr;
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
     Also replace types with binfos.  */

  last = tree_cons (NULL_TREE, NULL_TREE, base_init_list);
  for (x = TREE_CHAIN (last); x; x = TREE_CHAIN (x))
    {
      tree basetype = TREE_PURPOSE (x);
      tree binfo = (TREE_CODE (basetype) == TREE_VEC
		    ? basetype : binfo_or_else (basetype, t));
      
      if (binfo == NULL_TREE)
	/* BASETYPE might be an inaccessible direct base (because it
	   is also an indirect base).  */
	continue;

      if (TREE_VIA_VIRTUAL (binfo))
	{
	  /* Virtual base classes are special cases.  Their
	     initializers are recorded with this constructor, and they
	     are used when this constructor is the top-level
	     constructor called.  */
	  tree v = binfo_for_vbase (BINFO_TYPE (binfo), t);
	  vbases = tree_cons (v, TREE_VALUE (x), vbases);
	}
      else
	{
	  /* Otherwise, it must be an immediate base class.  */
	  my_friendly_assert
	    (same_type_p (BINFO_TYPE (BINFO_INHERITANCE_CHAIN (binfo)),
			  t), 20011113);

	  TREE_PURPOSE (x) = binfo;
	  TREE_CHAIN (last) = x;
	  last = x;
	}
    }
  TREE_CHAIN (last) = NULL_TREE;

  /* Now walk through our regular bases and make sure they're initialized.  */

  for (i = 0; i < n_baseclasses; ++i)
    {
      /* The base for which we're currently initializing.  */
      tree base_binfo = TREE_VEC_ELT (binfos, i);
      /* The initializer for BASE_BINFO.  */
      tree init;
      int pos;

      if (TREE_VIA_VIRTUAL (base_binfo))
	continue;

      /* We haven't found the BASE_BINFO yet.  */
      init = NULL_TREE;
      /* Loop through all the explicitly initialized bases, looking
	 for an appropriate initializer.  */
      for (x = base_init_list, pos = 0; x; x = TREE_CHAIN (x), ++pos)
	{
	  tree binfo = TREE_PURPOSE (x);

	  if (binfo == base_binfo && !init)
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
	      init = build_tree_list (binfo, TREE_VALUE (x));
	    }
	  else if (binfo == base_binfo)
	    {
	      error ("base class `%T' already initialized", 
			BINFO_TYPE (binfo));
	      break;
	    }
	}

      /* If we didn't find BASE_BINFO in the list, create a dummy entry
	 so the two lists (RBASES and the list of bases) will be
	 symmetrical.  */
      if (!init)
	init = build_tree_list (NULL_TREE, NULL_TREE);
      rbases = chainon (rbases, init);
    }

  *rbase_ptr = rbases;
  *vbase_ptr = vbases;
}

/* Perform whatever initializations have yet to be done on the base
   class, and non-static data members, of the CURRENT_CLASS_TYPE.
   These actions are given by the BASE_INIT_LIST and MEM_INIT_LIST,
   respectively.

   If there is a need for a call to a constructor, we must surround
   that call with a pushlevel/poplevel pair, since we are technically
   at the PARM level of scope.  */

void
emit_base_init (mem_init_list, base_init_list)
     tree mem_init_list;
     tree base_init_list;
{
  tree member;
  tree rbase_init_list, vbase_init_list;
  tree t = current_class_type;
  tree t_binfo = TYPE_BINFO (t);
  tree binfos = BINFO_BASETYPES (t_binfo);
  int i;
  int n_baseclasses = BINFO_N_BASETYPES (t_binfo);

  mem_init_list = sort_member_init (t, mem_init_list);
  sort_base_init (t, base_init_list, &rbase_init_list, &vbase_init_list);

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
	  if (extra_warnings 
	      && DECL_COPY_CONSTRUCTOR_P (current_function_decl))
	    warning ("base class `%#T' should be explicitly initialized in the copy constructor",
			BINFO_TYPE (base_binfo));
	}

      if (init != void_list_node)
	{
	  member = build_base_path (PLUS_EXPR, current_class_ptr,
				    base_binfo, 1);
	  expand_aggr_init_1 (base_binfo, NULL_TREE,
			      build_indirect_ref (member, NULL), init,
			      LOOKUP_NORMAL);
	}

      expand_cleanup_for_base (base_binfo, NULL_TREE);
      rbase_init_list = TREE_CHAIN (rbase_init_list);
    }

  /* Initialize the vtable pointers for the class.  */
  initialize_vtbl_ptrs (current_class_ptr);

  while (mem_init_list)
    {
      tree init;
      tree member;
      int from_init_list;

      member = TREE_PURPOSE (mem_init_list);

      /* See if we had a user-specified member initialization.  */
      if (TREE_TYPE (mem_init_list))
	{
	  init = TREE_VALUE (mem_init_list);
	  from_init_list = 1;
	}
      else
	{
	  init = DECL_INITIAL (member);
	  from_init_list = 0;

	  /* Effective C++ rule 12.  */
	  if (warn_ecpp && init == NULL_TREE
	      && !DECL_ARTIFICIAL (member)
	      && TREE_CODE (TREE_TYPE (member)) != ARRAY_TYPE)
	    warning ("`%D' should be initialized in the member initialization list", member);	    
	}

      perform_member_init (member, init, from_init_list);
      mem_init_list = TREE_CHAIN (mem_init_list);
    }
}

/* Returns the address of the vtable (i.e., the value that should be
   assigned to the vptr) for BINFO.  */

static tree
build_vtbl_address (binfo)
     tree binfo;
{
  tree binfo_for = binfo;
  tree vtbl;

  if (BINFO_VPTR_INDEX (binfo) && TREE_VIA_VIRTUAL (binfo)
      && BINFO_PRIMARY_P (binfo))
    /* If this is a virtual primary base, then the vtable we want to store
       is that for the base this is being used as the primary base of.  We
       can't simply skip the initialization, because we may be expanding the
       inits of a subobject constructor where the virtual base layout
       can be different.  */
    while (BINFO_PRIMARY_BASE_OF (binfo_for))
      binfo_for = BINFO_PRIMARY_BASE_OF (binfo_for);

  /* Figure out what vtable BINFO's vtable is based on, and mark it as
     used.  */
  vtbl = get_vtbl_decl_for_binfo (binfo_for);
  assemble_external (vtbl);
  TREE_USED (vtbl) = 1;

  /* Now compute the address to use when initializing the vptr.  */
  vtbl = BINFO_VTABLE (binfo_for);
  if (TREE_CODE (vtbl) == VAR_DECL)
    {
      vtbl = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (vtbl)), vtbl);
      TREE_CONSTANT (vtbl) = 1;
    }

  return vtbl;
}

/* This code sets up the virtual function tables appropriate for
   the pointer DECL.  It is a one-ply initialization.

   BINFO is the exact type that DECL is supposed to be.  In
   multiple inheritance, this might mean "C's A" if C : A, B.  */

static void
expand_virtual_init (binfo, decl)
     tree binfo, decl;
{
  tree vtbl, vtbl_ptr;
  tree vtt_index;

  /* Compute the initializer for vptr.  */
  vtbl = build_vtbl_address (binfo);

  /* We may get this vptr from a VTT, if this is a subobject
     constructor or subobject destructor.  */
  vtt_index = BINFO_VPTR_INDEX (binfo);
  if (vtt_index)
    {
      tree vtbl2;
      tree vtt_parm;

      /* Compute the value to use, when there's a VTT.  */
      vtt_parm = current_vtt_parm;
      vtbl2 = build (PLUS_EXPR, 
		     TREE_TYPE (vtt_parm), 
		     vtt_parm,
		     vtt_index);
      vtbl2 = build1 (INDIRECT_REF, TREE_TYPE (vtbl), vtbl2);

      /* The actual initializer is the VTT value only in the subobject
	 constructor.  In maybe_clone_body we'll substitute NULL for
	 the vtt_parm in the case of the non-subobject constructor.  */
      vtbl = build (COND_EXPR, 
		    TREE_TYPE (vtbl), 
		    build (EQ_EXPR, boolean_type_node,
			   current_in_charge_parm, integer_zero_node),
		    vtbl2, 
		    vtbl);
    }

  /* Compute the location of the vtpr.  */
  vtbl_ptr = build_vfield_ref (build_indirect_ref (decl, NULL),
			       TREE_TYPE (binfo));
  my_friendly_assert (vtbl_ptr != error_mark_node, 20010730);

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
	  (current_class_ref, binfo, base_dtor_identifier, NULL_TREE));
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
  tree ref = build_indirect_ref (addr, NULL);

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

  /* Now, run through the baseclasses, initializing each.  */ 
  for (vbases = CLASSTYPE_VBASECLASSES (type); vbases;
       vbases = TREE_CHAIN (vbases))
    {
      tree inner_if_stmt;
      tree compound_stmt;
      tree exp;
      tree vbase;

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
      vbase = TREE_VALUE (vbases);
      exp = build (PLUS_EXPR,
		   TREE_TYPE (this_ptr),
		   this_ptr,
		   fold (build1 (NOP_EXPR, TREE_TYPE (this_ptr),
				 BINFO_OFFSET (vbase))));
      exp = build1 (NOP_EXPR, 
		    build_pointer_type (BINFO_TYPE (vbase)), 
		    exp);

      expand_aggr_vbase_init_1 (vbase, this_ref, exp, init_list);
      finish_compound_stmt (/*has_no_scope=*/1, compound_stmt);
      finish_then_clause (inner_if_stmt);
      finish_if_stmt ();
      
      expand_cleanup_for_base (vbase, flag);
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
     tree member_name;
{
  if (field == error_mark_node)
    return 0;
  if (field == NULL_TREE || initializing_context (field) != type)
    {
      error ("class `%T' does not have any field named `%D'", type,
		member_name);
      return 0;
    }
  if (TREE_STATIC (field))
    {
      error ("field `%#D' is static; the only point of initialization is its definition",
		field);
      return 0;
    }

  return 1;
}

/* EXP is an expression of aggregate type. NAME is an IDENTIFIER_NODE
   which names a field, or it is a _TYPE node or TYPE_DECL which names
   a base for that type.  INIT is a parameter list for that field's or
   base's constructor.  Check the validity of NAME, and return a
   TREE_LIST of the base _TYPE or FIELD_DECL and the INIT. EXP is used
   only to get its type.  If NAME is invalid, return NULL_TREE and
   issue a diagnostic.

   An old style unnamed direct single base construction is permitted,
   where NAME is NULL.  */

tree
expand_member_init (exp, name, init)
     tree exp, name, init;
{
  tree basetype = NULL_TREE, field;
  tree type;

  if (exp == NULL_TREE)
    return NULL_TREE;

  type = TYPE_MAIN_VARIANT (TREE_TYPE (exp));
  my_friendly_assert (IS_AGGR_TYPE (type), 20011113);

  if (!name)
    {
      /* This is an obsolete unnamed base class initializer.  The
	 parser will already have warned about its use.  */
      switch (CLASSTYPE_N_BASECLASSES (type))
	{
	case 0:
	  error ("unnamed initializer for `%T', which has no base classes",
		    type);
	  return NULL_TREE;
	case 1:
	  basetype = TYPE_BINFO_BASETYPE (type, 0);
	  break;
	default:
	  error ("unnamed initializer for `%T', which uses multiple inheritance",
		    type);
	  return NULL_TREE;
      }
    }
  else if (TYPE_P (name))
    {
      basetype = name;
      name = TYPE_NAME (name);
    }
  else if (TREE_CODE (name) == TYPE_DECL)
    basetype = TYPE_MAIN_VARIANT (TREE_TYPE (name));

  my_friendly_assert (init != NULL_TREE, 0);

  if (init == void_type_node)
    init = NULL_TREE;

  if (basetype)
    {
      if (current_template_parms)
	;
      else if (vec_binfo_member (basetype, TYPE_BINFO_BASETYPES (type)))
	/* A direct base.  */;
      else if (binfo_for_vbase (basetype, type))
	/* A virtual base.  */;
      else
	{
	  if (TYPE_USES_VIRTUAL_BASECLASSES (type))
	    error ("type `%D' is not a direct or virtual base of `%T'",
		      name, type);
	  else
	    error ("type `%D' is not a direct base of `%T'",
		      name, type);
	  return NULL_TREE;
	}

      init = build_tree_list (basetype, init);
    }
  else
    {
      field = lookup_field (type, name, 1, 0);

      if (! member_init_ok_or_else (field, type, name))
	return NULL_TREE;

      init = build_tree_list (field, init);
    }

  return init;
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
      
      if (init && !itype)
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
      if (cp_type_quals (type) != TYPE_UNQUALIFIED)
	TREE_TYPE (exp) = TYPE_MAIN_VARIANT (type);
      if (itype && cp_type_quals (itype) != TYPE_UNQUALIFIED)
	TREE_TYPE (init) = TYPE_MAIN_VARIANT (itype);
      stmt_expr = build_vec_init (exp, init,
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
  destroy_temps = stmts_are_full_exprs_p ();
  current_stmt_tree ()->stmts_are_full_exprs_p = 0;
  expand_aggr_init_1 (TYPE_BINFO (type), exp, exp,
		      init, LOOKUP_NORMAL|flags);
  stmt_expr = finish_init_stmts (stmt_expr, compound_stmt);
  current_stmt_tree ()->stmts_are_full_exprs_p = destroy_temps;
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
  tree ctor_name;

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

  if (true_exp == exp)
    ctor_name = complete_ctor_identifier;
  else
    ctor_name = base_ctor_identifier;

  rval = build_method_call (exp, ctor_name, parms, binfo, flags);
  if (TREE_SIDE_EFFECTS (rval))
    {
      if (building_stmt_tree ())
	finish_expr_stmt (rval);
      else
	genrtl_expr_stmt (rval);
    }
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
      && TREE_CODE (type) != BOUND_TEMPLATE_TEMPLATE_PARM)
    {
      if (or_else)
	error ("`%T' is not an aggregate type", type);
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
	error ("`%T' fails to be an aggregate typedef", name);
      return NULL_TREE;
    }

  if (! IS_AGGR_TYPE (type)
      && TREE_CODE (type) != TEMPLATE_TYPE_PARM
      && TREE_CODE (type) != BOUND_TEMPLATE_TEMPLATE_PARM)
    {
      if (or_else)
	error ("type `%T' is of non-aggregate type", type);
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

  if (DECL_P (name))
    name = DECL_NAME (name);

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
	  return build_x_function_call (build_offset_ref (type, name),
					parmlist, current_class_ref);
	}
    }

  if (type == NULL_TREE || ! is_aggr_type (type, 1))
    return error_mark_node;

  /* An operator we did not like.  */
  if (name == NULL_TREE)
    return error_mark_node;

  if (dtor)
    {
      error ("cannot call destructor `%T::~%T' without object", type,
		method_name);
      return error_mark_node;
    }

  decl = maybe_dummy_object (type, &basetype_path);

  /* Convert 'this' to the specified type to disambiguate conversion
     to the function's context.  */
  if (decl == current_class_ref
      && ACCESSIBLY_UNIQUELY_DERIVED_P (type, current_class_type))
    {
      tree olddecl = current_class_ptr;
      tree oldtype = TREE_TYPE (TREE_TYPE (olddecl));
      if (oldtype != type)
	{
	  tree newtype = build_qualified_type (type, TYPE_QUALS (oldtype));
	  decl = convert_force (build_pointer_type (newtype), olddecl, 0);
	  decl = build_indirect_ref (decl, NULL);
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
	      error ("invalid use of non-static field `%D'", t);
	      return error_mark_node;
	    }
	  decl = build (COMPONENT_REF, TREE_TYPE (t), decl, t);
	}
      else if (TREE_CODE (t) == VAR_DECL)
	decl = t;
      else
	{
	  error ("invalid use of member `%D'", t);
	  return error_mark_node;
	}
      if (TYPE_LANG_SPECIFIC (TREE_TYPE (decl)))
	return build_opfncall (CALL_EXPR, LOOKUP_NORMAL, decl,
			       parmlist, NULL_TREE);
      return build_function_call (decl, parmlist);
    }
  else
    {
      error ("no method `%T::%D'", type, name);
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

  if (processing_template_decl || uses_template_parms (type))
    return build_min_nt (SCOPE_REF, type, name);

  if (TREE_CODE (name) == TEMPLATE_ID_EXPR)
    {
      /* If the NAME is a TEMPLATE_ID_EXPR, we are looking at
	 something like `a.template f<int>' or the like.  For the most
	 part, we treat this just like a.f.  We do remember, however,
	 the template-id that was used.  */
      name = TREE_OPERAND (orig_name, 0);

      if (DECL_P (name))
	name = DECL_NAME (name);
      else
	{
	  if (TREE_CODE (name) == LOOKUP_EXPR)
	    /* This can happen during tsubst'ing.  */
	    name = TREE_OPERAND (name, 0);
	  else
	    {
	      if (TREE_CODE (name) == COMPONENT_REF)
		name = TREE_OPERAND (name, 1);
	      if (TREE_CODE (name) == OVERLOAD)
		name = DECL_NAME (OVL_CURRENT (name));
	    }
	}

      my_friendly_assert (TREE_CODE (name) == IDENTIFIER_NODE, 0);
    }

  if (type == NULL_TREE)
    return error_mark_node;
  
  /* Handle namespace names fully here.  */
  if (TREE_CODE (type) == NAMESPACE_DECL)
    {
      t = lookup_namespace_name (type, name);
      if (t == error_mark_node)
        return t;
      if (TREE_CODE (orig_name) == TEMPLATE_ID_EXPR)
        /* Reconstruct the TEMPLATE_ID_EXPR.  */
        t = build (TEMPLATE_ID_EXPR, TREE_TYPE (t),
                   t, TREE_OPERAND (orig_name, 1));
      if (! type_unknown_p (t))
	{
	  mark_used (t);
	  t = convert_from_reference (t);
	}
      return t;
    }

  if (! is_aggr_type (type, 1))
    return error_mark_node;

  if (TREE_CODE (name) == BIT_NOT_EXPR)
    {
      if (! check_dtor_name (type, name))
	error ("qualified type `%T' does not match destructor name `~%T'",
		  type, TREE_OPERAND (name, 0));
      name = dtor_identifier;
    }

  if (!COMPLETE_TYPE_P (complete_type (type))
      && !TYPE_BEING_DEFINED (type))
    {
      error ("incomplete type `%T' does not have member `%D'", type,
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

          t = build (TEMPLATE_ID_EXPR, TREE_TYPE (t), t,
	             TREE_OPERAND (orig_name, 1));
	  t = build (OFFSET_REF, unknown_type_node, decl, t);
          
          PTRMEM_OK_P (t) = 1;
          	  
	  return t;
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
	  t = build (OFFSET_REF, TREE_TYPE (t), decl, t);
	  PTRMEM_OK_P (t) = 1;
	  return t;
	}

      TREE_TYPE (fnfields) = unknown_type_node;
      
      t = build (OFFSET_REF, unknown_type_node, decl, fnfields);
      PTRMEM_OK_P (t) = 1;
      return t;
    }

  t = member;

  if (t == NULL_TREE)
    {
      error ("`%D' is not a member of type `%T'", name, type);
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
      error ("illegal pointer to bit-field `%D'", t);
      return error_mark_node;
    }

  /* static class functions too.  */
  if (TREE_CODE (t) == FUNCTION_DECL
      && TREE_CODE (TREE_TYPE (t)) == FUNCTION_TYPE)
    abort ();

  /* In member functions, the form `type::name' is no longer
     equivalent to `this->type::name', at least not until
     resolve_offset_ref.  */
  t = build (OFFSET_REF, build_offset_type (type, TREE_TYPE (t)), decl, t);
  PTRMEM_OK_P (t) = 1;
  return t;
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

  if (BASELINK_P (member) || TREE_CODE (member) == TEMPLATE_ID_EXPR)
    return build_unary_op (ADDR_EXPR, exp, 0);
  
  if (TREE_CODE (TREE_TYPE (member)) == METHOD_TYPE)
    {
      if (!flag_ms_extensions)
        /* A single non-static member, make sure we don't allow a
           pointer-to-member.  */
        exp = ovl_cons (member, NULL_TREE);
      
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
      cp_error_at ("member `%D' is non-static but referenced as a static member",
		   member);
      error ("at this point in file");
      return error_mark_node;
    }

  /* The first case is really just a reference to a member of `this'.  */
  if (TREE_CODE (member) == FIELD_DECL
      && (base == current_class_ref || is_dummy_object (base)))
    {
      tree binfo = TYPE_BINFO (current_class_type);

      /* Try to get to basetype from 'this'; if that doesn't work,
         nothing will.  */
      base = current_class_ref;

      /* First convert to the intermediate base specified, if appropriate.  */
      if (TREE_CODE (exp) == OFFSET_REF && TREE_CODE (type) == OFFSET_TYPE)
	{
	  binfo = binfo_or_else (TYPE_OFFSET_BASETYPE (type),
				 current_class_type);
	  if (!binfo)
	    return error_mark_node;
	  base = build_base_path (PLUS_EXPR, base, binfo, 1);
	}

      return build_component_ref (base, member, binfo, 1);
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
	  error ("object missing in `%E'", exp);
	  return error_mark_node;
	}

      basetype = TYPE_OFFSET_BASETYPE (TREE_TYPE (TREE_TYPE (member)));
      basetype = lookup_base (TREE_TYPE (TREE_TYPE (addr)),
			      basetype, ba_check, NULL);
      addr = build_base_path (PLUS_EXPR, addr, basetype, 1);
      
      member = cp_convert (ptrdiff_type_node, member);

      addr = build (PLUS_EXPR, build_pointer_type (type), addr, member);
      return build_indirect_ref (addr, 0);
    }
  else if (TYPE_PTRMEMFUNC_P (TREE_TYPE (member)))
    {
      return get_member_function_from_ptrfunc (&addr, member);
    }
  abort ();
  /* NOTREACHED */
  return NULL_TREE;
}

/* If DECL is a `const' declaration, and its value is a known
   constant, then return that value.  */

tree
decl_constant_value (decl)
     tree decl;
{
  if (TREE_READONLY_DECL_P (decl)
      && ! TREE_THIS_VOLATILE (decl)
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
  return build_call (global_delete_fndecl, build_tree_list (NULL_TREE, addr));
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
	abort ();
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
		  if (build_expr_type_conversion (WANT_INT | WANT_ENUM, 
						  this_nelts, 0)
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
		    nelts = cp_build_binary_op (MULT_EXPR, nelts, this_nelts);
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
  tree name = NULL_TREE, class_decl;
  static tree CL_suffix = NULL_TREE;
  if (CL_suffix == NULL_TREE)
    CL_suffix = get_identifier("class$");
  if (jclass_node == NULL_TREE)
    {
      jclass_node = IDENTIFIER_GLOBAL_VALUE (get_identifier ("jclass"));
      if (jclass_node == NULL_TREE)
	fatal_error ("call to Java constructor, while `jclass' undefined");

      jclass_node = TREE_TYPE (jclass_node);
    }

  /* Mangle the class$ field */
  {
    tree field;
    for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
      if (DECL_NAME (field) == CL_suffix)
	{
	  mangle_decl (field);
	  name = DECL_ASSEMBLER_NAME (field);
	  break;
	}
    if (!field)
      internal_error ("can't find class$");
    }

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
      make_decl_rtl (class_decl, NULL);
    }
  return class_decl;
}

/* Returns the size of the cookie to use when allocating an array
   whose elements have the indicated TYPE.  Assumes that it is already
   known that a cookie is needed.  */

static tree
get_cookie_size (type)
     tree type;
{
  tree cookie_size;

  /* We need to allocate an additional max (sizeof (size_t), alignof
     (true_type)) bytes.  */
  tree sizetype_size;
  tree type_align;
  
  sizetype_size = size_in_bytes (sizetype);
  type_align = size_int (TYPE_ALIGN_UNIT (type));
  if (INT_CST_LT_UNSIGNED (type_align, sizetype_size))
    cookie_size = sizetype_size;
  else
    cookie_size = type_align;

  return cookie_size;
}

/* Called from cplus_expand_expr when expanding a NEW_EXPR.  The return
   value is immediately handed to expand_expr.  */

static tree
build_new_1 (exp)
     tree exp;
{
  tree placement, init;
  tree type, true_type, size, rval, t;
  tree full_type;
  tree nelts = NULL_TREE;
  tree alloc_call, alloc_expr, alloc_node;
  tree alloc_fn;
  tree cookie_expr, init_expr;
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
  /* True if the function we are calling is a placement allocation
     function.  */
  bool placement_allocation_fn_p;

  placement = TREE_OPERAND (exp, 0);
  type = TREE_OPERAND (exp, 1);
  init = TREE_OPERAND (exp, 2);
  globally_qualified_p = NEW_EXPR_USE_GLOBAL (exp);

  if (TREE_CODE (type) == ARRAY_REF)
    {
      has_array = 1;
      nelts = TREE_OPERAND (type, 1);
      type = TREE_OPERAND (type, 0);

      full_type = cp_build_binary_op (MINUS_EXPR, nelts, integer_one_node);
      full_type = build_index_type (full_type);
      full_type = build_cplus_array_type (type, full_type);
    }
  else
    full_type = type;

  true_type = type;

  code = has_array ? VEC_NEW_EXPR : NEW_EXPR;

  /* If our base type is an array, then make sure we know how many elements
     it has.  */
  while (TREE_CODE (true_type) == ARRAY_TYPE)
    {
      tree this_nelts = array_type_nelts_top (true_type);
      nelts = cp_build_binary_op (MULT_EXPR, nelts, this_nelts);
      true_type = TREE_TYPE (true_type);
    }

  if (!complete_type_or_else (true_type, exp))
    return error_mark_node;

  size = size_in_bytes (true_type);
  if (has_array)
    size = size_binop (MULT_EXPR, size, convert (sizetype, nelts));

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
      && (has_array
	  ? TYPE_HAS_ARRAY_NEW_OPERATOR (true_type)
	  : TYPE_HAS_NEW_OPERATOR (true_type)))
    use_global_new = 0;
  else
    use_global_new = 1;

  /* We only need cookies for arrays containing types for which we
     need cookies.  */
  if (!has_array || !TYPE_VEC_NEW_USES_COOKIE (true_type))
    use_cookie = 0;
  /* When using placement new, users may not realize that they need
     the extra storage.  We require that the operator called be
     the global placement operator new[].  */
  else if (placement && !TREE_CHAIN (placement) 
	   && same_type_p (TREE_TYPE (TREE_VALUE (placement)),
			   ptr_type_node))
    use_cookie = !use_global_new;
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

  /* Allocate the object.  */
  
  if (! placement && TYPE_FOR_JAVA (true_type))
    {
      tree class_addr, alloc_decl;
      tree class_decl = build_java_class_ref (true_type);
      tree class_size = size_in_bytes (true_type);
      static const char alloc_name[] = "_Jv_AllocObject";
      use_java_new = 1;
      alloc_decl = IDENTIFIER_GLOBAL_VALUE (get_identifier (alloc_name));
      if (alloc_decl == NULL_TREE)
	fatal_error ("call to Java constructor with `%s' undefined",
		     alloc_name);

      class_addr = build1 (ADDR_EXPR, jclass_node, class_decl);
      alloc_call = (build_function_call
		    (alloc_decl,
		     tree_cons (NULL_TREE, class_addr,
				build_tree_list (NULL_TREE, class_size))));
    }
  else
    {
      tree fnname;
      tree args;

      args = tree_cons (NULL_TREE, size, placement);
      fnname = ansi_opname (code);

      if (use_global_new)
	alloc_call = (build_new_function_call 
		      (lookup_function_nonclass (fnname, args),
		       args));
      else
	alloc_call = build_method_call (build_dummy_object (true_type),
					fnname, args, NULL_TREE,
					LOOKUP_NORMAL);
    }

  if (alloc_call == error_mark_node)
    return error_mark_node;

  /* The ALLOC_CALL should be a CALL_EXPR -- or a COMPOUND_EXPR whose
     right-hand-side is ultimately a CALL_EXPR -- and the first
     operand should be the address of a known FUNCTION_DECL.  */
  t = alloc_call;
  while (TREE_CODE (t) == COMPOUND_EXPR) 
    t = TREE_OPERAND (t, 1);
  alloc_fn = get_callee_fndecl (t);
  my_friendly_assert (alloc_fn != NULL_TREE, 20020325);
  /* Now, check to see if this function is actually a placement
     allocation function.  This can happen even when PLACEMENT is NULL
     because we might have something like:

       struct S { void* operator new (size_t, int i = 0); };

     A call to `new S' will get this allocation function, even though
     there is no explicit placement argument.  If there is more than
     one argument, or there are variable arguments, then this is a
     placement allocation function.  */
  placement_allocation_fn_p 
    = (type_num_arguments (TREE_TYPE (alloc_fn)) > 1 
       || varargs_function_p (alloc_fn));

  /*        unless an allocation function is declared with an empty  excep-
     tion-specification  (_except.spec_),  throw(), it indicates failure to
     allocate storage by throwing a bad_alloc exception  (clause  _except_,
     _lib.bad.alloc_); it returns a non-null pointer otherwise If the allo-
     cation function is declared  with  an  empty  exception-specification,
     throw(), it returns null to indicate failure to allocate storage and a
     non-null pointer otherwise.

     So check for a null exception spec on the op new we just called.  */

  nothrow = TYPE_NOTHROW_P (TREE_TYPE (alloc_fn));
  check_new = (flag_check_new || nothrow) && ! use_java_new;

  alloc_expr = alloc_call;

  if (use_cookie)
    /* Adjust so we're pointing to the start of the object.  */
    alloc_expr = build (PLUS_EXPR, TREE_TYPE (alloc_expr),
			alloc_expr, cookie_size);

  /* While we're working, use a pointer to the type we've actually
     allocated.  */
  alloc_expr = convert (build_pointer_type (full_type), alloc_expr);

  /* Now save the allocation expression so we only evaluate it once.  */
  alloc_expr = get_target_expr (alloc_expr);
  alloc_node = TREE_OPERAND (alloc_expr, 0);

  /* Now initialize the cookie.  */
  if (use_cookie)
    {
      tree cookie;

      /* Store the number of bytes allocated so that we can know how
	 many elements to destroy later.  We use the last sizeof
	 (size_t) bytes to store the number of elements.  */
      cookie = build (MINUS_EXPR, build_pointer_type (sizetype),
		      alloc_node, size_in_bytes (sizetype));
      cookie = build_indirect_ref (cookie, NULL);

      cookie_expr = build (MODIFY_EXPR, void_type_node, cookie, nelts);
      TREE_SIDE_EFFECTS (cookie_expr) = 1;
    }
  else
    cookie_expr = NULL_TREE;

  /* Now initialize the allocated object.  */
  init_expr = NULL_TREE;
  if (TYPE_NEEDS_CONSTRUCTING (type) || init)
    {
      init_expr = build_indirect_ref (alloc_node, NULL);

      if (init == void_zero_node)
	init = build_default_init (full_type);
      else if (init && pedantic && has_array)
	pedwarn ("ISO C++ forbids initialization in array new");

      if (has_array)
	init_expr = build_vec_init (init_expr, init, 0);
      else if (TYPE_NEEDS_CONSTRUCTING (type))
	init_expr = build_method_call (init_expr, 
				       complete_ctor_identifier,
				       init, TYPE_BINFO (true_type),
				       LOOKUP_NORMAL);
      else
	{
	  /* We are processing something like `new int (10)', which
	     means allocate an int, and initialize it with 10.  */

	  if (TREE_CODE (init) == TREE_LIST)
	    {
	      if (TREE_CHAIN (init) != NULL_TREE)
		pedwarn
		  ("initializer list being treated as compound expression");
	      init = build_compound_expr (init);
	    }
	  else if (TREE_CODE (init) == CONSTRUCTOR
		   && TREE_TYPE (init) == NULL_TREE)
	    {
	      pedwarn ("ISO C++ forbids aggregate initializer to new");
	      init = digest_init (type, init, 0);
	    }

	  init_expr = build_modify_expr (init_expr, INIT_EXPR, init);
	}

      if (init_expr == error_mark_node)
	return error_mark_node;

      /* If any part of the object initialization terminates by throwing an
	 exception and a suitable deallocation function can be found, the
	 deallocation function is called to free the memory in which the
	 object was being constructed, after which the exception continues
	 to propagate in the context of the new-expression. If no
	 unambiguous matching deallocation function can be found,
	 propagating the exception does not cause the object's memory to be
	 freed.  */
      if (flag_exceptions && ! use_java_new)
	{
	  enum tree_code dcode = has_array ? VEC_DELETE_EXPR : DELETE_EXPR;
	  tree cleanup;
	  int flags = (LOOKUP_NORMAL 
		       | (globally_qualified_p * LOOKUP_GLOBAL));
	  tree delete_node;

	  if (use_cookie)
	    /* Subtract the padding back out to get to the pointer returned
	       from operator new.  */
	    delete_node = fold (build (MINUS_EXPR, TREE_TYPE (alloc_node),
				       alloc_node, cookie_size));
	  else
	    delete_node = alloc_node;

	  /* The Standard is unclear here, but the right thing to do
             is to use the same method for finding deallocation
             functions that we use for finding allocation functions.  */
	  flags |= LOOKUP_SPECULATIVELY;

	  cleanup = build_op_delete_call (dcode, delete_node, size, flags,
					  (placement_allocation_fn_p 
					   ? alloc_call : NULL_TREE));

	  /* Ack!  First we allocate the memory.  Then we set our sentry
	     variable to true, and expand a cleanup that deletes the memory
	     if sentry is true.  Then we run the constructor, and finally
	     clear the sentry.

	     It would be nice to be able to handle this without the sentry
	     variable, perhaps with a TRY_CATCH_EXPR, but this doesn't
	     work.  We allocate the space first, so if there are any
	     temporaries with cleanups in the constructor args we need this
	     EH region to extend until end of full-expression to preserve
	     nesting.

	     If the backend had some mechanism so that we could force the
	     allocation to be expanded after all the other args to the
	     constructor, that would fix the nesting problem and we could
	     do away with this complexity.  But that would complicate other
	     things; in particular, it would make it difficult to bail out
	     if the allocation function returns null.  */

	  if (cleanup)
	    {
	      tree end, sentry, begin;

	      begin = get_target_expr (boolean_true_node);
	      sentry = TREE_OPERAND (begin, 0);

	      TREE_OPERAND (begin, 2)
		= build (COND_EXPR, void_type_node, sentry,
			 cleanup, void_zero_node);

	      end = build (MODIFY_EXPR, TREE_TYPE (sentry),
			   sentry, boolean_false_node);

	      init_expr
		= build (COMPOUND_EXPR, void_type_node, begin,
			 build (COMPOUND_EXPR, void_type_node, init_expr,
				end));
	    }
	}
    }
  else if (CP_TYPE_CONST_P (true_type))
    error ("uninitialized const in `new' of `%#T'", true_type);

  /* Now build up the return value in reverse order.  */

  rval = alloc_node;

  if (init_expr)
    rval = build (COMPOUND_EXPR, TREE_TYPE (rval), init_expr, rval);
  if (cookie_expr)
    rval = build (COMPOUND_EXPR, TREE_TYPE (rval), cookie_expr, rval);

  if (rval == alloc_node)
    /* If we didn't modify anything, strip the TARGET_EXPR and return the
       (adjusted) call.  */
    rval = TREE_OPERAND (alloc_expr, 1);
  else
    {
      if (check_new)
	{
	  tree ifexp = cp_build_binary_op (NE_EXPR, alloc_node,
					   integer_zero_node);
	  rval = build_conditional_expr (ifexp, rval, alloc_node);
	}

      rval = build (COMPOUND_EXPR, TREE_TYPE (rval), alloc_expr, rval);
    }

  /* Now strip the outer ARRAY_TYPE, so we return a pointer to the first
     element.  */
  rval = convert (build_pointer_type (type), rval);

  return rval;
}

static tree
build_vec_delete_1 (base, maxindex, type, auto_delete_vec, use_global_delete)
     tree base, maxindex, type;
     special_function_kind auto_delete_vec;
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

  /* We should only have 1-D arrays here.  */
  if (TREE_CODE (type) == ARRAY_TYPE)
    abort ();

  if (! IS_AGGR_TYPE (type) || TYPE_HAS_TRIVIAL_DESTRUCTOR (type))
    {
      loop = integer_zero_node;
      goto no_destructor;
    }

  /* The below is short by the cookie size.  */
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
		    build_delete (ptype, tbase, sfk_complete_destructor,
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
  deallocate_expr = integer_zero_node;
  if (auto_delete_vec != sfk_base_destructor)
    {
      tree base_tbd;

      /* The below is short by the cookie size.  */
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
			  cp_build_binary_op (MINUS_EXPR,
					      cp_convert (string_type_node, 
							  base),
					      cookie_size));
	  /* True size with header.  */
	  virtual_size = size_binop (PLUS_EXPR, virtual_size, cookie_size);
	}

      if (auto_delete_vec == sfk_deleting_destructor)
	deallocate_expr = build_x_delete (base_tbd,
					  2 | use_global_delete,
					  virtual_size);
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

/* Create an unnamed variable of the indicated TYPE.  */ 

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
    SET_DECL_RTL (decl, assign_temp (type, 2, 0, 1));
  finish_expr_stmt (build_modify_expr (decl, INIT_EXPR, init));

  return decl;
}

/* `build_vec_init' returns tree structure that performs
   initialization of a vector of aggregate types.

   BASE is a reference to the vector, of ARRAY_TYPE.
   INIT is the (possibly NULL) initializer.

   FROM_ARRAY is 0 if we should init everything with INIT
   (i.e., every element initialized from INIT).
   FROM_ARRAY is 1 if we should index into INIT in parallel
   with initialization of DECL.
   FROM_ARRAY is 2 if we should index into INIT in parallel,
   but use assignment instead of initialization.  */

tree
build_vec_init (base, init, from_array)
     tree base, init;
     int from_array;
{
  tree rval;
  tree base2 = NULL_TREE;
  tree size;
  tree itype = NULL_TREE;
  tree iterator;
  /* The type of the array.  */
  tree atype = TREE_TYPE (base);
  /* The type of an element in the array.  */
  tree type = TREE_TYPE (atype);
  /* The type of a pointer to an element in the array.  */
  tree ptype;
  tree stmt_expr;
  tree compound_stmt;
  int destroy_temps;
  tree try_block = NULL_TREE;
  tree try_body = NULL_TREE;
  int num_initialized_elts = 0;
  tree maxindex = array_type_nelts (TREE_TYPE (base));

  if (maxindex == error_mark_node)
    return error_mark_node;

  /* For g++.ext/arrnew.C.  */
  if (init && TREE_CODE (init) == CONSTRUCTOR && TREE_TYPE (init) == NULL_TREE)
    init = digest_init (atype, init, 0);
      
  if (init && !TYPE_NEEDS_CONSTRUCTING (type)
      && ((TREE_CODE (init) == CONSTRUCTOR
	   /* Don't do this if the CONSTRUCTOR might contain something
	      that might throw and require us to clean up.  */
	   && (CONSTRUCTOR_ELTS (init) == NULL_TREE
	       || ! TYPE_HAS_NONTRIVIAL_DESTRUCTOR (target_type (type))))
	  || from_array))
    {
      /* Do non-default initialization of POD arrays resulting from
	 brace-enclosed initializers.  In this case, digest_init and
	 store_constructor will handle the semantics for us.  */

      stmt_expr = build (INIT_EXPR, atype, base, init);
      TREE_SIDE_EFFECTS (stmt_expr) = 1;
      return stmt_expr;
    }

  maxindex = cp_convert (ptrdiff_type_node, maxindex);
  ptype = build_pointer_type (type);
  size = size_in_bytes (type);
  if (TREE_CODE (TREE_TYPE (base)) == ARRAY_TYPE)
    base = cp_convert (ptype, default_conversion (base));

  /* The code we are generating looks like:

       T* t1 = (T*) base;
       T* rval = t1;
       ptrdiff_t iterator = maxindex;
       try {
	 do {
	   ... initialize *t1 ...
	   ++t1;
	 } while (--iterator != -1);
       } catch (...) {
         ... destroy elements that were constructed ...
       }
       return rval;
       
     We can omit the try and catch blocks if we know that the
     initialization will never throw an exception, or if the array
     elements do not have destructors.  We can omit the loop completely if
     the elements of the array do not have constructors.  

     We actually wrap the entire body of the above in a STMT_EXPR, for
     tidiness.  

     When copying from array to another, when the array elements have
     only trivial copy constructors, we should use __builtin_memcpy
     rather than generating a loop.  That way, we could take advantage
     of whatever cleverness the back-end has for dealing with copies
     of blocks of memory.  */

  begin_init_stmts (&stmt_expr, &compound_stmt);
  destroy_temps = stmts_are_full_exprs_p ();
  current_stmt_tree ()->stmts_are_full_exprs_p = 0;
  rval = get_temp_regvar (ptype, base);
  base = get_temp_regvar (ptype, rval);
  iterator = get_temp_regvar (ptrdiff_type_node, maxindex);

  /* Protect the entire array initialization so that we can destroy
     the partially constructed array if an exception is thrown.
     But don't do this if we're assigning.  */
  if (flag_exceptions && TYPE_HAS_NONTRIVIAL_DESTRUCTOR (type)
      && from_array != 2)
    {
      try_block = begin_try_block ();
      try_body = begin_compound_stmt (/*has_no_scope=*/1);
    }

  if (init != NULL_TREE && TREE_CODE (init) == CONSTRUCTOR)
    {
      /* Do non-default initialization of non-POD arrays resulting from
	 brace-enclosed initializers.  */

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

	  finish_expr_stmt (build_unary_op (PREINCREMENT_EXPR, base, 0));
	  finish_expr_stmt (build_unary_op (PREDECREMENT_EXPR, iterator, 0));
	}

      /* Clear out INIT so that we don't get confused below.  */
      init = NULL_TREE;
    }
  else if (from_array)
    {
      /* If initializing one array from another, initialize element by
	 element.  We rely upon the below calls the do argument
	 checking.  */ 
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
	  && ! (host_integerp (maxindex, 0)
		&& (num_initialized_elts
		    == tree_low_cst (maxindex, 0) + 1))))
    {
      /* If the ITERATOR is equal to -1, then we don't have to loop;
	 we've already initialized all the elements.  */
      tree if_stmt;
      tree do_stmt;
      tree do_body;
      tree elt_init;

      if_stmt = begin_if_stmt ();
      finish_if_stmt_cond (build (NE_EXPR, boolean_type_node,
				  iterator, integer_minus_one_node),
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
	    abort ();
	}
      else if (TREE_CODE (type) == ARRAY_TYPE)
	{
	  if (init != 0)
	    sorry
	      ("cannot initialize multi-dimensional array with initializer");
	  elt_init = build_vec_init (build1 (INDIRECT_REF, type, base),
				     0, 0);
	}
      else
	elt_init = build_aggr_init (build1 (INDIRECT_REF, type, base), 
				    init, 0);
      
      /* The initialization of each array element is a
	 full-expression, as per core issue 124.  */
      if (!building_stmt_tree ())
	{
	  genrtl_expr_stmt (elt_init);
	  expand_end_target_temps ();
	}
      else
	{
	  current_stmt_tree ()->stmts_are_full_exprs_p = 1;
	  finish_expr_stmt (elt_init);
	  current_stmt_tree ()->stmts_are_full_exprs_p = 0;
	}

      finish_expr_stmt (build_unary_op (PREINCREMENT_EXPR, base, 0));
      if (base2)
	finish_expr_stmt (build_unary_op (PREINCREMENT_EXPR, base2, 0));

      finish_compound_stmt (/*has_no_scope=*/1, do_body);
      finish_do_body (do_stmt);
      finish_do_stmt (build (NE_EXPR, boolean_type_node,
			     build_unary_op (PREDECREMENT_EXPR, iterator, 0),
			     integer_minus_one_node),
		      do_stmt);

      finish_then_clause (if_stmt);
      finish_if_stmt ();
    }

  /* Make sure to cleanup any partially constructed elements.  */
  if (flag_exceptions && TYPE_HAS_NONTRIVIAL_DESTRUCTOR (type)
      && from_array != 2)
    {
      tree e;
      tree m = cp_build_binary_op (MINUS_EXPR, maxindex, iterator);

      /* Flatten multi-dimensional array since build_vec_delete only
	 expects one-dimensional array.  */
      if (TREE_CODE (type) == ARRAY_TYPE)
	{
	  m = cp_build_binary_op (MULT_EXPR, m,
				  array_type_nelts_total (type));
	  type = strip_array_types (type);
	}

      finish_compound_stmt (/*has_no_scope=*/1, try_body);
      finish_cleanup_try_block (try_block);
      e = build_vec_delete_1 (rval, m,
			      type,
			      sfk_base_destructor,
			      /*use_global_delete=*/0);
      finish_cleanup (e, try_block);
    }

  /* The value of the array initialization is the address of the
     first element in the array.  */
  finish_expr_stmt (rval);

  stmt_expr = finish_init_stmts (stmt_expr, compound_stmt);
  current_stmt_tree ()->stmts_are_full_exprs_p = destroy_temps;
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

/* Call the DTOR_KIND destructor for EXP.  FLAGS are as for
   build_delete.  */

static tree
build_dtor_call (exp, dtor_kind, flags)
     tree exp;
     special_function_kind dtor_kind;
     int flags;
{
  tree name;

  switch (dtor_kind)
    {
    case sfk_complete_destructor:
      name = complete_dtor_identifier;
      break;

    case sfk_base_destructor:
      name = base_dtor_identifier;
      break;

    case sfk_deleting_destructor:
      name = deleting_dtor_identifier;
      break;

    default:
      abort ();
    }
  return build_method_call (exp, name, NULL_TREE, NULL_TREE, flags);
}

/* Generate a call to a destructor. TYPE is the type to cast ADDR to.
   ADDR is an expression which yields the store to be destroyed.
   AUTO_DELETE is the name of the destructor to call, i.e., either
   sfk_complete_destructor, sfk_base_destructor, or
   sfk_deleting_destructor.

   FLAGS is the logical disjunction of zero or more LOOKUP_
   flags.  See cp-tree.h for more info.  */

tree
build_delete (type, addr, auto_delete, flags, use_global_delete)
     tree type, addr;
     special_function_kind auto_delete;
     int flags;
     int use_global_delete;
{
  tree expr;

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
      if (!VOID_TYPE_P (type) && !complete_type_or_else (type, addr))
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

      addr = convert_force (build_pointer_type (type), addr, 0);
    }

  my_friendly_assert (IS_AGGR_TYPE (type), 220);

  if (TYPE_HAS_TRIVIAL_DESTRUCTOR (type))
    {
      if (auto_delete != sfk_deleting_destructor)
	return void_zero_node;

      return build_op_delete_call
	(DELETE_EXPR, addr, c_sizeof_nowarn (type),
	 LOOKUP_NORMAL | (use_global_delete * LOOKUP_GLOBAL),
	 NULL_TREE);
    }
  else
    {
      tree do_delete = NULL_TREE;
      tree ifexp;

      my_friendly_assert (TYPE_HAS_DESTRUCTOR (type), 20011213);

      /* For `::delete x', we must not use the deleting destructor
	 since then we would not be sure to get the global `operator
	 delete'.  */
      if (use_global_delete && auto_delete == sfk_deleting_destructor)
	{
	  /* We will use ADDR multiple times so we must save it.  */
	  addr = save_expr (addr);
	  /* Delete the object. */
	  do_delete = build_builtin_delete_call (addr);
	  /* Otherwise, treat this like a complete object destructor
	     call.  */
	  auto_delete = sfk_complete_destructor;
	}
      /* If the destructor is non-virtual, there is no deleting
	 variant.  Instead, we must explicitly call the appropriate
	 `operator delete' here.  */
      else if (!DECL_VIRTUAL_P (CLASSTYPE_DESTRUCTORS (type))
	       && auto_delete == sfk_deleting_destructor)
	{
	  /* We will use ADDR multiple times so we must save it.  */
	  addr = save_expr (addr);
	  /* Build the call.  */
	  do_delete = build_op_delete_call (DELETE_EXPR,
					    addr,
					    c_sizeof_nowarn (type),
					    LOOKUP_NORMAL,
					    NULL_TREE);
	  /* Call the complete object destructor.  */
	  auto_delete = sfk_complete_destructor;
	}
      else if (auto_delete == sfk_deleting_destructor
	       && TYPE_GETS_REG_DELETE (type))
	{
	  /* Make sure we have access to the member op delete, even though
	     we'll actually be calling it from the destructor.  */
	  build_op_delete_call (DELETE_EXPR, addr, c_sizeof_nowarn (type),
				LOOKUP_NORMAL, NULL_TREE);
	}

      expr = build_dtor_call (build_indirect_ref (addr, NULL),
			      auto_delete, flags);
      if (do_delete)
	expr = build (COMPOUND_EXPR, void_type_node, expr, do_delete);

      if (flags & LOOKUP_DESTRUCTOR)
	/* Explicit destructor call; don't check for null pointer.  */
	ifexp = integer_one_node;
      else
	/* Handle deleting a null pointer.  */
	ifexp = fold (cp_build_binary_op (NE_EXPR, addr, integer_zero_node));

      if (ifexp != integer_one_node)
	expr = build (COND_EXPR, void_type_node,
		      ifexp, expr, void_zero_node);

      return expr;
    }
}

/* At the end of a destructor, call the destructors for our base classes
   and members.

   Called from finish_destructor_body.  */

void
perform_base_cleanups ()
{
  tree binfos;
  int i, n_baseclasses;
  tree member;
  tree expr;
  tree member_destructions = NULL;
  tree vbase_destructions = NULL;

  for (member = TYPE_FIELDS (current_class_type); member;
       member = TREE_CHAIN (member))
    {
      if (TREE_CODE (member) != FIELD_DECL)
	continue;
      if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (TREE_TYPE (member)))
	{
	  tree this_member = (build_component_ref
			      (current_class_ref, member,
			       NULL_TREE, 0));
	  tree this_type = TREE_TYPE (member);
	  expr = build_delete (this_type, this_member,
			       sfk_complete_destructor,
			       LOOKUP_NONVIRTUAL|LOOKUP_DESTRUCTOR|LOOKUP_NORMAL,
			       0);
	  if (!member_destructions)
	    member_destructions = expr;
	  else
	    member_destructions = build (COMPOUND_EXPR, 
					 TREE_TYPE (member_destructions),
					 expr,
					 member_destructions);
	}
    }
  if (member_destructions)
    finish_expr_stmt (member_destructions);

  binfos = BINFO_BASETYPES (TYPE_BINFO (current_class_type));
  n_baseclasses = CLASSTYPE_N_BASECLASSES (current_class_type);

  /* Take care of the remaining baseclasses.  */
  for (i = n_baseclasses - 1; i >= 0; i--)
    {
      tree base_binfo = TREE_VEC_ELT (binfos, i);
      if (TYPE_HAS_TRIVIAL_DESTRUCTOR (BINFO_TYPE (base_binfo))
	  || TREE_VIA_VIRTUAL (base_binfo))
	continue;

      expr = build_scoped_method_call (current_class_ref, base_binfo,
				       base_dtor_identifier,
				       NULL_TREE);

      finish_expr_stmt (expr);
    }

  /* Run destructors for all virtual baseclasses.  */
  if (TYPE_USES_VIRTUAL_BASECLASSES (current_class_type))
    {
      tree vbases;
      tree cond = (condition_conversion
		   (build (BIT_AND_EXPR, integer_type_node,
			   current_in_charge_parm,
			   integer_two_node)));

      vbases = CLASSTYPE_VBASECLASSES (current_class_type);
      /* The CLASSTYPE_VBASECLASSES list is in initialization
	 order, which is also the right order for pushing cleanups.  */
      for (; vbases;
	   vbases = TREE_CHAIN (vbases))
	{
	  tree vbase = TREE_VALUE (vbases);
	  tree base_type = BINFO_TYPE (vbase);

	  if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (base_type))
	    {
	      tree base_ptr_type = build_pointer_type (base_type);
	      expr = current_class_ptr;
	          
	      /* Convert to the basetype here, as we know the layout is
		 fixed. What is more, if we let build_method_call do it,
		 it will use the vtable, which may have been clobbered
		 by the deletion of our primary base.  */
                  
	      expr = build1 (NOP_EXPR, base_ptr_type, expr);
	      expr = build (PLUS_EXPR, base_ptr_type, expr,
			    BINFO_OFFSET (vbase));
	      expr = build_indirect_ref (expr, NULL);
	      expr = build_method_call (expr, base_dtor_identifier,
					NULL_TREE, vbase,
					LOOKUP_NORMAL);
	      expr = build (COND_EXPR, void_type_node, cond,
			    expr, void_zero_node);
	      if (!vbase_destructions)
		vbase_destructions = expr;
	      else
		vbase_destructions = build (COMPOUND_EXPR, 
					    TREE_TYPE (vbase_destructions),
					    expr,
					    vbase_destructions);
	    }
	}
    }
  if (vbase_destructions)
    finish_expr_stmt (vbase_destructions);
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
      tree this_addr 
	= convert_force (build_pointer_type (BINFO_TYPE (TREE_VALUE (vbases))),
			 addr, 0);
      result = tree_cons (NULL_TREE,
			  build_delete (TREE_TYPE (this_addr), this_addr,
					sfk_base_destructor,
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
     special_function_kind auto_delete_vec;
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

      type = strip_array_types (TREE_TYPE (type));
      cookie_addr = build (MINUS_EXPR,
			   build_pointer_type (sizetype),
			   base,
			   TYPE_SIZE_UNIT (sizetype));
      maxindex = build_indirect_ref (cookie_addr, NULL);
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
