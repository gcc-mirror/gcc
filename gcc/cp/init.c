/* Handle initialization things in C++.
   Copyright (C) 1987, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003 Free Software Foundation, Inc.
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

static void construct_virtual_base (tree, tree);
static void expand_aggr_init_1 PARAMS ((tree, tree, tree, tree, int));
static void expand_default_init PARAMS ((tree, tree, tree, tree, int));
static tree build_vec_delete_1 PARAMS ((tree, tree, tree, special_function_kind, int));
static void perform_member_init (tree, tree);
static tree build_builtin_delete_call PARAMS ((tree));
static int member_init_ok_or_else PARAMS ((tree, tree, tree));
static void expand_virtual_init PARAMS ((tree, tree));
static tree sort_mem_initializers (tree, tree);
static tree initializing_context PARAMS ((tree));
static void expand_cleanup_for_base PARAMS ((tree, tree));
static tree get_temp_regvar PARAMS ((tree, tree));
static tree dfs_initialize_vtbl_ptrs PARAMS ((tree, void *));
static tree build_default_init PARAMS ((tree, tree));
static tree build_new_1	PARAMS ((tree));
static tree get_cookie_size PARAMS ((tree));
static tree build_dtor_call PARAMS ((tree, special_function_kind, int));
static tree build_field_list PARAMS ((tree, tree, int *));
static tree build_vtbl_address PARAMS ((tree));

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
     class.  We do these in pre-order because we can't find the virtual
     bases for a class until we've initialized the vtbl for that
     class.  */
  dfs_walk_real (TYPE_BINFO (type), dfs_initialize_vtbl_ptrs, 
		 NULL, dfs_unmarked_real_bases_queue_p, list);
  dfs_walk (TYPE_BINFO (type), dfs_unmark,
	    dfs_marked_real_bases_queue_p, type);
}

/* Return an expression for the zero-initialization of an object with
   type T.  This expression will either be a constant (in the case
   that T is a scalar), or a CONSTRUCTOR (in the case that T is an
   aggregate).  In either case, the value can be used as DECL_INITIAL
   for a decl of the indicated TYPE; it is a valid static initializer.
   If NELTS is non-NULL, and TYPE is an ARRAY_TYPE, NELTS is the
   number of elements in the array.  If STATIC_STORAGE_P is TRUE,
   initializers are only generated for entities for which
   zero-initialization does not simply mean filling the storage with
   zero bytes.  */

tree
build_zero_init (tree type, tree nelts, bool static_storage_p)
{
  tree init = NULL_TREE;

  /* [dcl.init]

     To zero-initialization storage for an object of type T means:

     -- if T is a scalar type, the storage is set to the value of zero
        converted to T.

     -- if T is a non-union class type, the storage for each nonstatic
        data member and each base-class subobject is zero-initialized.

     -- if T is a union type, the storage for its first data member is
        zero-initialized.

     -- if T is an array type, the storage for each element is
        zero-initialized.

     -- if T is a reference type, no initialization is performed.  */

  my_friendly_assert (nelts == NULL_TREE || TREE_CODE (nelts) == INTEGER_CST,
		      20030618);

  if (type == error_mark_node)
    ;
  else if (static_storage_p && zero_init_p (type))
    /* In order to save space, we do not explicitly build initializers
       for items that do not need them.  GCC's semantics are that
       items with static storage duration that are not otherwise
       initialized are initialized to zero.  */
    ;
  else if (SCALAR_TYPE_P (type))
    init = convert (type, integer_zero_node);
  else if (CLASS_TYPE_P (type))
    {
      tree field;
      tree inits;

      /* Build a constructor to contain the initializations.  */
      init = build (CONSTRUCTOR, type, NULL_TREE, NULL_TREE);
      /* Iterate over the fields, building initializations.  */
      inits = NULL_TREE;
      for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	{
	  if (TREE_CODE (field) != FIELD_DECL)
	    continue;

	  /* Note that for class types there will be FIELD_DECLs
	     corresponding to base classes as well.  Thus, iterating
	     over TYPE_FIELDs will result in correct initialization of
	     all of the subobjects.  */
	  if (static_storage_p && !zero_init_p (TREE_TYPE (field)))
	    inits = tree_cons (field, 
			       build_zero_init (TREE_TYPE (field),
						/*nelts=*/NULL_TREE,
						static_storage_p),
			       inits);

	  /* For unions, only the first field is initialized.  */
	  if (TREE_CODE (type) == UNION_TYPE)
	    break;
	}
      CONSTRUCTOR_ELTS (init) = nreverse (inits);
    }
  else if (TREE_CODE (type) == ARRAY_TYPE)
    {
      tree index;
      tree max_index;
      tree inits;

      /* Build a constructor to contain the initializations.  */
      init = build (CONSTRUCTOR, type, NULL_TREE, NULL_TREE);
      /* Iterate over the array elements, building initializations.  */
      inits = NULL_TREE;
      max_index = nelts ? nelts : array_type_nelts (type);
      my_friendly_assert (TREE_CODE (max_index) == INTEGER_CST, 20030618);

      for (index = size_zero_node;
	   !tree_int_cst_lt (max_index, index);
	   index = size_binop (PLUS_EXPR, index, size_one_node))
	inits = tree_cons (index,
			   build_zero_init (TREE_TYPE (type),
					    /*nelts=*/NULL_TREE,
					    static_storage_p),
			   inits);
      CONSTRUCTOR_ELTS (init) = nreverse (inits);
    }
  else if (TREE_CODE (type) == REFERENCE_TYPE)
    ;
  else
    abort ();

  /* In all cases, the initializer is a constant.  */
  if (init)
    TREE_CONSTANT (init) = 1;

  return init;
}

/* Build an expression for the default-initialization of an object of
   the indicated TYPE.  If NELTS is non-NULL, and TYPE is an
   ARRAY_TYPE, NELTS is the number of elements in the array.  If
   initialization of TYPE requires calling constructors, this function
   returns NULL_TREE; the caller is responsible for arranging for the
   constructors to be called.  */

static tree
build_default_init (type, nelts)
     tree type;
     tree nelts;
{
  /* [dcl.init]:

    To default-initialize an object of type T means:

    --if T is a non-POD class type (clause _class_), the default construc-
      tor  for  T is called (and the initialization is ill-formed if T has
      no accessible default constructor);

    --if T is an array type, each element is default-initialized;

    --otherwise, the storage for the object is zero-initialized.

    A program that calls for default-initialization of an entity of refer-
    ence type is ill-formed.  */

  /* If TYPE_NEEDS_CONSTRUCTING is true, the caller is responsible for
     performing the initialization.  This is confusing in that some
     non-PODs do not have TYPE_NEEDS_CONSTRUCTING set.  (For example,
     a class with a pointer-to-data member as a non-static data member
     does not have TYPE_NEEDS_CONSTRUCTING set.)  Therefore, we end up
     passing non-PODs to build_zero_init below, which is contrary to
     the semantics quoted above from [dcl.init].  

     It happens, however, that the behavior of the constructor the
     standard says we should have generated would be precisely the
     same as that obtained by calling build_zero_init below, so things
     work out OK.  */
  if (TYPE_NEEDS_CONSTRUCTING (type)
      || (nelts && TREE_CODE (nelts) != INTEGER_CST))
    return NULL_TREE;
      
  /* At this point, TYPE is either a POD class type, an array of POD
     classes, or something even more inoccuous.  */
  return build_zero_init (type, nelts, /*static_storage_p=*/false);
}

/* Initialize MEMBER, a FIELD_DECL, with INIT, a TREE_LIST of
   arguments.  If TREE_LIST is void_type_node, an empty initializer
   list was given; if NULL_TREE no initializer was given.  */

static void
perform_member_init (tree member, tree init)
{
  tree decl;
  tree type = TREE_TYPE (member);
  bool explicit;

  explicit = (init != NULL_TREE);

  /* Effective C++ rule 12 requires that all data members be
     initialized.  */
  if (warn_ecpp && !explicit && TREE_CODE (type) != ARRAY_TYPE)
    warning ("`%D' should be initialized in the member initialization "
	     "list", 
	     member);

  if (init == void_type_node)
    init = NULL_TREE;

  /* Get an lvalue for the data member.  */
  decl = build_class_member_access_expr (current_class_ref, member,
					 /*access_path=*/NULL_TREE,
					 /*preserve_reference=*/true);
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
      if (explicit
	  && TREE_CODE (type) == ARRAY_TYPE
	  && init != NULL_TREE
	  && TREE_CHAIN (init) == NULL_TREE
	  && TREE_CODE (TREE_TYPE (TREE_VALUE (init))) == ARRAY_TYPE)
	{
	  /* Initialization of one array from another.  */
	  finish_expr_stmt (build_vec_init (decl, NULL_TREE, TREE_VALUE (init),
					    /* from_array=*/1));
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
	      init = build_default_init (type, /*nelts=*/NULL_TREE);
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

      expr = build_class_member_access_expr (current_class_ref, member,
					     /*access_path=*/NULL_TREE,
					     /*preserve_reference=*/false);
      expr = build_delete (type, expr, sfk_complete_destructor,
			   LOOKUP_NONVIRTUAL|LOOKUP_DESTRUCTOR, 0);

      if (expr != error_mark_node)
	finish_eh_cleanup (expr);
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

  *uses_unions_p = 0;

  /* Note whether or not T is a union.  */
  if (TREE_CODE (t) == UNION_TYPE)
    *uses_unions_p = 1;

  for (fields = TYPE_FIELDS (t); fields; fields = TREE_CHAIN (fields))
    {
      /* Skip CONST_DECLs for enumeration constants and so forth.  */
      if (TREE_CODE (fields) != FIELD_DECL || DECL_ARTIFICIAL (fields))
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

/* The MEM_INITS are a TREE_LIST.  The TREE_PURPOSE of each list gives
   a FIELD_DECL or BINFO in T that needs initialization.  The
   TREE_VALUE gives the initializer, or list of initializer arguments.

   Return a TREE_LIST containing all of the initializations required
   for T, in the order in which they should be performed.  The output
   list has the same format as the input.  */

static tree
sort_mem_initializers (tree t, tree mem_inits)
{
  tree init;
  tree base;
  tree sorted_inits;
  tree next_subobject;
  int i;
  int uses_unions_p;

  /* Build up a list of initializations.  The TREE_PURPOSE of entry
     will be the subobject (a FIELD_DECL or BINFO) to initialize.  The
     TREE_VALUE will be the constructor arguments, or NULL if no
     explicit initialization was provided.  */
  sorted_inits = NULL_TREE;
  /* Process the virtual bases.  */
  for (base = CLASSTYPE_VBASECLASSES (t); base; base = TREE_CHAIN (base))
    sorted_inits = tree_cons (TREE_VALUE (base), NULL_TREE, sorted_inits);
  /* Process the direct bases.  */
  for (i = 0; i < CLASSTYPE_N_BASECLASSES (t); ++i)
    {
      base = BINFO_BASETYPE (TYPE_BINFO (t), i);
      if (!TREE_VIA_VIRTUAL (base))
	sorted_inits = tree_cons (base, NULL_TREE, sorted_inits);
    }
  /* Process the non-static data members.  */
  sorted_inits = build_field_list (t, sorted_inits, &uses_unions_p);
  /* Reverse the entire list of initializations, so that they are in
     the order that they will actually be performed.  */
  sorted_inits = nreverse (sorted_inits);

  /* If the user presented the initializers in an order different from
     that in which they will actually occur, we issue a warning.  Keep
     track of the next subobject which can be explicitly initialized
     without issuing a warning.  */
  next_subobject = sorted_inits;

  /* Go through the explicit initializers, filling in TREE_PURPOSE in
     the SORTED_INITS.  */
  for (init = mem_inits; init; init = TREE_CHAIN (init))
    {
      tree subobject;
      tree subobject_init;

      subobject = TREE_PURPOSE (init);

      /* If the explicit initializers are in sorted order, then
	 SUBOBJECT will be NEXT_SUBOBJECT, or something following 
	 it.  */
      for (subobject_init = next_subobject; 
	   subobject_init; 
	   subobject_init = TREE_CHAIN (subobject_init))
	if (TREE_PURPOSE (subobject_init) == subobject)
	  break;

      /* Issue a warning if the explicit initializer order does not
	 match that which will actually occur.  */
      if (warn_reorder && !subobject_init)
	{
	  if (TREE_CODE (TREE_PURPOSE (next_subobject)) == FIELD_DECL)
	    cp_warning_at ("`%D' will be initialized after",
			   TREE_PURPOSE (next_subobject));
	  else
	    warning ("base `%T' will be initialized after",
		     TREE_PURPOSE (next_subobject));
	  if (TREE_CODE (subobject) == FIELD_DECL)
	    cp_warning_at ("  `%#D'", subobject);
	  else
	    warning ("  base `%T'", subobject);
	}

      /* Look again, from the beginning of the list.  */
      if (!subobject_init)
	{
	  subobject_init = sorted_inits;
	  while (TREE_PURPOSE (subobject_init) != subobject)
	    subobject_init = TREE_CHAIN (subobject_init);
	}
	
      /* It is invalid to initialize the same subobject more than
	 once.  */
      if (TREE_VALUE (subobject_init))
	{
	  if (TREE_CODE (subobject) == FIELD_DECL)
	    error ("multiple initializations given for `%D'", subobject);
	  else
	    error ("multiple initializations given for base `%T'", 
		   subobject);
	}

      /* Record the initialization.  */
      TREE_VALUE (subobject_init) = TREE_VALUE (init);
      next_subobject = subobject_init;
    }

  /* [class.base.init]

     If a ctor-initializer specifies more than one mem-initializer for
     multiple members of the same union (including members of
     anonymous unions), the ctor-initializer is ill-formed.  */
  if (uses_unions_p)
    {
      tree last_field = NULL_TREE;
      for (init = sorted_inits; init; init = TREE_CHAIN (init))
	{
	  tree field;
	  tree field_type;
	  int done;

	  /* Skip uninitialized members and base classes.  */
	  if (!TREE_VALUE (init) 
	      || TREE_CODE (TREE_PURPOSE (init)) != FIELD_DECL)
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

  return sorted_inits;
}

/* Initialize all bases and members of CURRENT_CLASS_TYPE.  MEM_INITS
   is a TREE_LIST giving the explicit mem-initializer-list for the
   constructor.  The TREE_PURPOSE of each entry is a subobject (a
   FIELD_DECL or a BINFO) of the CURRENT_CLASS_TYPE.  The TREE_VALUE
   is a TREE_LIST giving the arguments to the constructor or
   void_type_node for an empty list of arguments.  */

void
emit_mem_initializers (tree mem_inits)
{
  /* Sort the mem-initializers into the order in which the
     initializations should be performed.  */
  mem_inits = sort_mem_initializers (current_class_type, mem_inits);

  in_base_initializer = 1;
  
  /* Initialize base classes.  */
  while (mem_inits 
	 && TREE_CODE (TREE_PURPOSE (mem_inits)) != FIELD_DECL)
    {
      tree subobject = TREE_PURPOSE (mem_inits);
      tree arguments = TREE_VALUE (mem_inits);

      /* If these initializations are taking place in a copy
	 constructor, the base class should probably be explicitly
	 initialized.  */
      if (extra_warnings && !arguments 
	  && DECL_COPY_CONSTRUCTOR_P (current_function_decl)
	  && TYPE_NEEDS_CONSTRUCTING (BINFO_TYPE (subobject)))
	warning ("base class `%#T' should be explicitly initialized in the "
		 "copy constructor",
		 BINFO_TYPE (subobject));

      /* If an explicit -- but empty -- initializer list was present,
	 treat it just like default initialization at this point.  */
      if (arguments == void_type_node)
	arguments = NULL_TREE;

      /* Initialize the base.  */
      if (TREE_VIA_VIRTUAL (subobject))
	construct_virtual_base (subobject, arguments);
      else
	{
	  tree base_addr;
	  
	  base_addr = build_base_path (PLUS_EXPR, current_class_ptr,
				       subobject, 1);
	  expand_aggr_init_1 (subobject, NULL_TREE,
			      build_indirect_ref (base_addr, NULL), 
			      arguments,
			      LOOKUP_NORMAL);
	  expand_cleanup_for_base (subobject, NULL_TREE);
	}

      mem_inits = TREE_CHAIN (mem_inits);
    }
  in_base_initializer = 0;

  /* Initialize the vptrs.  */
  initialize_vtbl_ptrs (current_class_ptr);
  
  /* Initialize the data members.  */
  while (mem_inits)
    {
      perform_member_init (TREE_PURPOSE (mem_inits),
			   TREE_VALUE (mem_inits));
      mem_inits = TREE_CHAIN (mem_inits);
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
   it is a DECL which is nonzero when this base needs to be
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
  expr = build_special_member_call (current_class_ref, 
				    base_dtor_identifier,
				    NULL_TREE,
				    binfo,
				    LOOKUP_NORMAL | LOOKUP_NONVIRTUAL);
  if (flag)
    expr = fold (build (COND_EXPR, void_type_node,
			c_common_truthvalue_conversion (flag),
			expr, integer_zero_node));

  finish_eh_cleanup (expr);
}

/* Construct the virtual base-class VBASE passing the ARGUMENTS to its
   constructor.  */

static void
construct_virtual_base (tree vbase, tree arguments)
{
  tree inner_if_stmt;
  tree compound_stmt;
  tree exp;
  tree flag;  

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
  flag = TREE_CHAIN (DECL_ARGUMENTS (current_function_decl));
  inner_if_stmt = begin_if_stmt ();
  finish_if_stmt_cond (flag, inner_if_stmt);
  compound_stmt = begin_compound_stmt (/*has_no_scope=*/1);

  /* Compute the location of the virtual base.  If we're
     constructing virtual bases, then we must be the most derived
     class.  Therefore, we don't have to look up the virtual base;
     we already know where it is.  */
  exp = build (PLUS_EXPR,
	       TREE_TYPE (current_class_ptr),
	       current_class_ptr,
	       fold (build1 (NOP_EXPR, TREE_TYPE (current_class_ptr),
			     BINFO_OFFSET (vbase))));
  exp = build1 (NOP_EXPR, 
		build_pointer_type (BINFO_TYPE (vbase)), 
		exp);
  exp = build1 (INDIRECT_REF, BINFO_TYPE (vbase), exp);

  expand_aggr_init_1 (vbase, current_class_ref, exp,
		      arguments, LOOKUP_COMPLAIN);
  finish_compound_stmt (/*has_no_scope=*/1, compound_stmt);
  finish_then_clause (inner_if_stmt);
  finish_if_stmt ();

  expand_cleanup_for_base (vbase, flag);
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

/* NAME is a FIELD_DECL, an IDENTIFIER_NODE which names a field, or it
   is a _TYPE node or TYPE_DECL which names a base for that type.
   Check the validity of NAME, and return either the base _TYPE, base
   binfo, or the FIELD_DECL of the member.  If NAME is invalid, return
   NULL_TREE and issue a diagnostic.

   An old style unnamed direct single base construction is permitted,
   where NAME is NULL.  */

tree
expand_member_init (tree name)
{
  tree basetype;
  tree field;

  if (!current_class_ref)
    return NULL_TREE;

  if (!name)
    {
      /* This is an obsolete unnamed base class initializer.  The
	 parser will already have warned about its use.  */
      switch (CLASSTYPE_N_BASECLASSES (current_class_type))
	{
	case 0:
	  error ("unnamed initializer for `%T', which has no base classes",
		 current_class_type);
	  return NULL_TREE;
	case 1:
	  basetype = TYPE_BINFO_BASETYPE (current_class_type, 0);
	  break;
	default:
	  error ("unnamed initializer for `%T', which uses multiple inheritance",
		 current_class_type);
	  return NULL_TREE;
      }
    }
  else if (TYPE_P (name))
    {
      basetype = TYPE_MAIN_VARIANT (name);
      name = TYPE_NAME (name);
    }
  else if (TREE_CODE (name) == TYPE_DECL)
    basetype = TYPE_MAIN_VARIANT (TREE_TYPE (name));
  else
    basetype = NULL_TREE;

  if (basetype)
    {
      tree binfo;

      if (current_template_parms)
	return basetype;

      binfo = lookup_base (current_class_type, basetype, 
			   ba_ignore, NULL);
      if (binfo)
	{
	  if (TREE_VIA_VIRTUAL (binfo))
	    binfo = binfo_for_vbase (basetype, current_class_type);
	  else if (BINFO_INHERITANCE_CHAIN (binfo) 
		   != TYPE_BINFO (current_class_type))
	    binfo = NULL_TREE;
	}
      if (!binfo)
	{
	  if (TYPE_USES_VIRTUAL_BASECLASSES (current_class_type))
	    error ("type `%D' is not a direct or virtual base of `%T'",
		   name, current_class_type);
	  else
	    error ("type `%D' is not a direct base of `%T'",
		   name, current_class_type);
	  return NULL_TREE;
	}

      if (binfo)
	return binfo;
    }
  else
    {
      if (TREE_CODE (name) == IDENTIFIER_NODE)
	field = lookup_field (current_class_type, name, 1, 0);
      else
	field = name;

      if (member_init_ok_or_else (field, current_class_type, name))
	return field;
    }

  return NULL_TREE;
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
      stmt_expr = build_vec_init (exp, NULL_TREE, init,
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

/* Like build_aggr_init, but not just for aggregates.  */

tree
build_init (decl, init, flags)
     tree decl, init;
     int flags;
{
  tree expr;

  if (IS_AGGR_TYPE (TREE_TYPE (decl))
      || TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE)
    expr = build_aggr_init (decl, init, flags);
  else
    expr = build (INIT_EXPR, TREE_TYPE (decl), decl, init);

  return expr;
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
      else if (TREE_CODE (init) == CONSTRUCTOR 
	       && TREE_HAS_CONSTRUCTOR (init))
	{
	  /* A brace-enclosed initializer for an aggregate.  */
	  my_friendly_assert (CP_AGGREGATE_TYPE_P (type), 20021016);
	  init = digest_init (type, init, (tree *)NULL);
	}
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

  rval = build_special_member_call (exp, ctor_name, parms, binfo, flags);
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
  my_friendly_assert (building_stmt_tree (), 20021010);

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
      if (store_init_value (exp, init))
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
  tree fns;
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
      return finish_call_expr (name, parmlist, /*disallow_virtual=*/true);
    }

  if (DECL_P (name))
    name = DECL_NAME (name);

  if (TREE_CODE (type) == NAMESPACE_DECL)
    return finish_call_expr (lookup_namespace_name (type, name),
			     parmlist,
			     /*disallow_virtual=*/true);

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
	return finish_call_expr (lookup_namespace_name (ns, name),
				 parmlist,
				 /*disallow_virtual=*/true);
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

  fns = lookup_fnfields (basetype_path, method_name, 0);
  if (fns)
    {
      if (TREE_CODE (name) == TEMPLATE_ID_EXPR)
	BASELINK_FUNCTIONS (fns) = build_nt (TEMPLATE_ID_EXPR,
					     BASELINK_FUNCTIONS (fns),
					     TREE_OPERAND (name, 1));
      return build_new_method_call (decl, fns, parmlist,
				    /*conversion_path=*/NULL_TREE,
				    LOOKUP_NORMAL|LOOKUP_NONVIRTUAL);
    }

  /* Convert 'this' to the specified type to disambiguate conversion
     to the function's context.  */
  if (decl == current_class_ref
      /* ??? this is wrong, but if this conversion is invalid we need to
	 defer it until we know whether we are calling a static or
	 non-static member function.  Be conservative for now.  */
      && ACCESSIBLY_UNIQUELY_DERIVED_P (type, current_class_type))
    {
      basetype_path = NULL_TREE;
      decl = build_scoped_ref (decl, type, &basetype_path);
      if (decl == error_mark_node)
	return error_mark_node;
    }

  if (constructor_name_p (method_name, type))
    return build_functional_cast (type, parmlist);
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

  if (BASELINK_P (name) || DECL_P (name))
    member = name;
  else
    {
      member = lookup_member (basebinfo, name, 1, 0);
      
      if (member == error_mark_node)
	return error_mark_node;
    }

  /* A lot of this logic is now handled in lookup_member.  */
  if (member && BASELINK_P (member))
    {
      /* Go from the TREE_BASELINK to the member function info.  */
      tree fnfields = member;
      t = BASELINK_FUNCTIONS (fnfields);

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

      if (TREE_CODE (t) != TEMPLATE_ID_EXPR && !really_overloaded_fn (t))
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
      error ("invalid pointer to bit-field `%D'", t);
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
      if (!cxx_mark_addressable (member))
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
      tree binfo = NULL_TREE;

      /* Try to get to basetype from 'this'; if that doesn't work,
         nothing will.  */
      base = current_class_ref;

      /* First convert to the intermediate base specified, if appropriate.  */
      if (TREE_CODE (exp) == OFFSET_REF && TREE_CODE (type) == OFFSET_TYPE)
	base = build_scoped_ref (base, TYPE_OFFSET_BASETYPE (type), &binfo);

      return build_class_member_access_expr (base, member,
					     /*access_path=*/NULL_TREE,
					     /*preserve_reference=*/false);
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

/* Given a Java class, return a decl for the corresponding java.lang.Class.  */

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
  tree outer_nelts = NULL_TREE;
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
      nelts = outer_nelts = TREE_OPERAND (type, 1);
      type = TREE_OPERAND (type, 0);

      /* Use an incomplete array type to avoid VLA headaches.  */
      full_type = build_cplus_array_type (type, NULL_TREE);
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
					fnname, args, 
					TYPE_BINFO (true_type),
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
	init = build_default_init (full_type, nelts);
      else if (init && pedantic && has_array)
	pedwarn ("ISO C++ forbids initialization in array new");

      if (has_array)
	init_expr
	  = build_vec_init (init_expr,
			    cp_build_binary_op (MINUS_EXPR, outer_nelts,
						integer_one_node),
			    init, /*from_array=*/0);
      else if (TYPE_NEEDS_CONSTRUCTING (type))
	init_expr = build_special_member_call (init_expr, 
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
	     if the allocation function returns null.  Er, no, it wouldn't;
	     we just don't run the constructor.  The standard says it's
	     unspecified whether or not the args are evaluated.  */

	  if (cleanup)
	    {
	      tree end, sentry, begin;

	      begin = get_target_expr (boolean_true_node);
	      CLEANUP_EH_ONLY (begin) = 1;

	      sentry = TARGET_EXPR_SLOT (begin);

	      TARGET_EXPR_CLEANUP (begin)
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
	  tree nullexp;
	  tree ifexp;

	  nullexp = convert (TREE_TYPE (alloc_node),
			     use_cookie ? cookie_size : size_zero_node);
	  ifexp = cp_build_binary_op (NE_EXPR, alloc_node, nullexp);
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
   MAXINDEX is the maximum index of the array (one less than the
     number of elements).  It is only used if
     TYPE_DOMAIN (TREE_TYPE (BASE)) == NULL_TREE.
   INIT is the (possibly NULL) initializer.

   FROM_ARRAY is 0 if we should init everything with INIT
   (i.e., every element initialized from INIT).
   FROM_ARRAY is 1 if we should index into INIT in parallel
   with initialization of DECL.
   FROM_ARRAY is 2 if we should index into INIT in parallel,
   but use assignment instead of initialization.  */

tree
build_vec_init (base, maxindex, init, from_array)
     tree base, init, maxindex;
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

  if (TYPE_DOMAIN (atype))
    maxindex = array_type_nelts (atype);

  if (maxindex == NULL_TREE || maxindex == error_mark_node)
    return error_mark_node;

  if (init
      && (from_array == 2
	  ? (!CLASS_TYPE_P (type) || !TYPE_HAS_COMPLEX_ASSIGN_REF (type))
	  : !TYPE_NEEDS_CONSTRUCTING (type))
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
				     0, 0, 0);
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
  return build_method_call (exp, name, NULL_TREE, 
			    TYPE_BINFO (TREE_TYPE (exp)), flags);
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
      if (TREE_CODE (type) == ARRAY_TYPE)
	goto handle_array;

      if (VOID_TYPE_P (type)
	  /* We don't want to warn about delete of void*, only other
	     incomplete types.  Deleting other incomplete types
	     invokes undefined behavior, but it is not ill-formed, so
	     compile to something that would even do The Right Thing
	     (TM) should the type have a trivial dtor and no delete
	     operator.  */
	  || !complete_type_or_diagnostic (type, addr, 1)
	  || !IS_AGGR_TYPE (type))
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
	(DELETE_EXPR, addr, cxx_sizeof_nowarn (type),
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
	  /* Delete the object.  */
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
					    cxx_sizeof_nowarn (type),
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
	  build_op_delete_call (DELETE_EXPR, addr, cxx_sizeof_nowarn (type),
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

/* At the beginning of a destructor, push cleanups that will call the
   destructors for our base classes and members.

   Called from begin_destructor_body.  */

void
push_base_cleanups ()
{
  tree binfos;
  int i, n_baseclasses;
  tree member;
  tree expr;

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
	      expr = build_special_member_call (current_class_ref, 
						base_dtor_identifier,
						NULL_TREE,
						vbase,
						(LOOKUP_NORMAL 
						 | LOOKUP_NONVIRTUAL));
	      expr = build (COND_EXPR, void_type_node, cond,
			    expr, void_zero_node);
	      finish_decl_cleanup (NULL_TREE, expr);
	    }
	}
    }

  binfos = BINFO_BASETYPES (TYPE_BINFO (current_class_type));
  n_baseclasses = CLASSTYPE_N_BASECLASSES (current_class_type);

  /* Take care of the remaining baseclasses.  */
  for (i = 0; i < n_baseclasses; i++)
    {
      tree base_binfo = TREE_VEC_ELT (binfos, i);
      if (TYPE_HAS_TRIVIAL_DESTRUCTOR (BINFO_TYPE (base_binfo))
	  || TREE_VIA_VIRTUAL (base_binfo))
	continue;

      expr = build_special_member_call (current_class_ref, 
					base_dtor_identifier,
					NULL_TREE, base_binfo, 
					LOOKUP_NORMAL | LOOKUP_NONVIRTUAL);
      finish_decl_cleanup (NULL_TREE, expr);
    }

  for (member = TYPE_FIELDS (current_class_type); member;
       member = TREE_CHAIN (member))
    {
      if (TREE_CODE (member) != FIELD_DECL || DECL_ARTIFICIAL (member))
	continue;
      if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (TREE_TYPE (member)))
	{
	  tree this_member = (build_class_member_access_expr 
			      (current_class_ref, member, 
			       /*access_path=*/NULL_TREE,
			       /*preserve_reference=*/false));
	  tree this_type = TREE_TYPE (member);
	  expr = build_delete (this_type, this_member,
			       sfk_complete_destructor,
			       LOOKUP_NONVIRTUAL|LOOKUP_DESTRUCTOR|LOOKUP_NORMAL,
			       0);
	  finish_decl_cleanup (NULL_TREE, expr);
	}
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

  if (TREE_CODE (type) == POINTER_TYPE)
    {
      /* Step back one from start of vector, and read dimension.  */
      tree cookie_addr;

      if (TREE_SIDE_EFFECTS (base))
	base = save_expr (base);
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
      if (TREE_SIDE_EFFECTS (base))
	base = save_expr (base);
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
