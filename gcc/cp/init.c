/* Handle initialization things in C++.
   Copyright (C) 1987, 89, 92-98, 1999 Free Software Foundation, Inc.
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

/* In C++, structures with well-defined constructors are initialized by
   those constructors, unasked.  CURRENT_BASE_INIT_LIST
   holds a list of stmts for a BASE_INIT term in the grammar.
   This list has one element for each base class which must be
   initialized.  The list elements are [basename, init], with
   type basetype.  This allows the possibly anachronistic form
   (assuming d : a, b, c) "d (int a) : c(a+5), b (a-4), a (a+3)"
   where each successive term can be handed down the constructor
   line.  Perhaps this was not intended.  */
tree current_base_init_list, current_member_init_list;

static void expand_aggr_vbase_init_1 PROTO((tree, tree, tree, tree));
static void construct_virtual_bases PROTO((tree, tree, tree, tree, tree));
static void expand_aggr_init_1 PROTO((tree, tree, tree, tree, int));
static void expand_default_init PROTO((tree, tree, tree, tree, int));
static tree build_vec_delete_1 PROTO((tree, tree, tree, tree, tree,
				      int));
static void perform_member_init PROTO((tree, tree, tree, int));
static void sort_base_init PROTO((tree, tree *, tree *));
static tree build_builtin_delete_call PROTO((tree));
static int member_init_ok_or_else PROTO((tree, tree, const char *));
static void expand_virtual_init PROTO((tree, tree));
static tree sort_member_init PROTO((tree));
static tree initializing_context PROTO((tree));
static void expand_vec_init_try_block PROTO((tree));
static void expand_vec_init_catch_clause PROTO((tree, tree, tree, tree));
static tree build_java_class_ref PROTO((tree));
static void expand_cleanup_for_base PROTO((tree, tree, tree));
static int  pvbasecount PROTO((tree, int));

/* Cache the identifier nodes for the magic field of a new cookie.  */
static tree nc_nelts_field_id;

static tree minus_one;

/* Set up local variable for this file.  MUST BE CALLED AFTER
   INIT_DECL_PROCESSING.  */

static tree BI_header_type, BI_header_size;

void init_init_processing ()
{
  tree fields[1];

  minus_one = build_int_2 (-1, -1);

  /* Define the structure that holds header information for
     arrays allocated via operator new.  */
  BI_header_type = make_lang_type (RECORD_TYPE);
  nc_nelts_field_id = get_identifier ("nelts");
  fields[0] = build_lang_field_decl (FIELD_DECL, nc_nelts_field_id, sizetype);
  finish_builtin_type (BI_header_type, "__new_cookie", fields,
		       0, double_type_node);
  BI_header_size = size_in_bytes (BI_header_type);
}

/* Subroutine of emit_base_init.  For BINFO, initialize all the
   virtual function table pointers, except those that come from
   virtual base classes.  Initialize binfo's vtable pointer, if
   INIT_SELF is true.  CAN_ELIDE is true when we know that all virtual
   function table pointers in all bases have been initialized already,
   probably because their constructors have just be run.  ADDR is the
   pointer to the object whos vtables we are going to initialize.

   REAL_BINFO is usually the same as BINFO, except when addr is not of
   pointer to the type of the real derived type that we want to
   initialize for.  This is the case when addr is a pointer to a sub
   object of a complete object, and we only want to do part of the
   complete object's initialization of vtable pointers.  This is done
   for all virtual table pointers in virtual base classes.  REAL_BINFO
   is used to find the BINFO_VTABLE that we initialize with.  BINFO is
   used for conversions of addr to subobjects.

   BINFO_TYPE (real_binfo) must be BINFO_TYPE (binfo).

   Relies upon binfo being inside TYPE_BINFO (TREE_TYPE (TREE_TYPE
   (addr))).  */

void
expand_direct_vtbls_init (real_binfo, binfo, init_self, can_elide, addr)
     tree real_binfo, binfo, addr;
     int init_self, can_elide;
{
  tree real_binfos = BINFO_BASETYPES (real_binfo);
  tree binfos = BINFO_BASETYPES (binfo);
  int i, n_baselinks = real_binfos ? TREE_VEC_LENGTH (real_binfos) : 0;

  for (i = 0; i < n_baselinks; i++)
    {
      tree real_base_binfo = TREE_VEC_ELT (real_binfos, i);
      tree base_binfo = TREE_VEC_ELT (binfos, i);
      int is_not_base_vtable
	= i != CLASSTYPE_VFIELD_PARENT (BINFO_TYPE (real_binfo));
      if (! TREE_VIA_VIRTUAL (real_base_binfo))
	expand_direct_vtbls_init (real_base_binfo, base_binfo,
				  is_not_base_vtable, can_elide, addr);
    }
#if 0
  /* Before turning this on, make sure it is correct.  */
  if (can_elide && ! BINFO_MODIFIED (binfo))
    return;
#endif
  /* Should we use something besides CLASSTYPE_VFIELDS? */
  if (init_self && CLASSTYPE_VFIELDS (BINFO_TYPE (real_binfo)))
    {
      tree base_ptr = convert_pointer_to_real (binfo, addr);
      expand_virtual_init (real_binfo, base_ptr);
    }
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

  expand_start_target_temps ();

  if (TYPE_NEEDS_CONSTRUCTING (type)
      || (init && TYPE_HAS_CONSTRUCTOR (type)))
    {
      /* Since `init' is already a TREE_LIST on the current_member_init_list,
	 only build it into one if we aren't already a list.  */
      if (init != NULL_TREE && TREE_CODE (init) != TREE_LIST)
	init = build_expr_list (NULL_TREE, init);

      decl = build_component_ref (current_class_ref, name, NULL_TREE, explicit);

      if (explicit
	  && TREE_CODE (type) == ARRAY_TYPE
	  && init != NULL_TREE
	  && TREE_CHAIN (init) == NULL_TREE
	  && TREE_CODE (TREE_TYPE (TREE_VALUE (init))) == ARRAY_TYPE)
	{
	  /* Initialization of one array from another.  */
	  expand_vec_init (TREE_OPERAND (decl, 1), decl,
			   array_type_nelts (type), TREE_VALUE (init), 1);
	}
      else
	expand_aggr_init (decl, init, 0);
    }
  else
    {
      if (init == NULL_TREE)
	{
	  if (explicit)
	    {
	      /* default-initialization.  */
	      if (AGGREGATE_TYPE_P (type))
		init = build (CONSTRUCTOR, type, NULL_TREE, NULL_TREE);
	      else if (TREE_CODE (type) == REFERENCE_TYPE)
		{
		  cp_error ("default-initialization of `%#D', which has reference type",
			    member);
		  init = error_mark_node;
		}
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

      /* We only build this with a null init if we got it from the
	 current_member_init_list.  */
      if (init || explicit)
	{
	  decl = build_component_ref (current_class_ref, name, NULL_TREE,
				      explicit);
	  expand_expr_stmt (build_modify_expr (decl, INIT_EXPR, init));
	}
    }

  expand_end_target_temps ();
  free_temp_slots ();

  if (TYPE_NEEDS_DESTRUCTOR (type))
    {
      tree expr;

      /* All cleanups must be on the function_obstack.  */
      push_obstacks_nochange ();
      resume_temporary_allocation ();

      expr = build_component_ref (current_class_ref, name, NULL_TREE,
				  explicit);
      expr = build_delete (type, expr, integer_zero_node,
			   LOOKUP_NONVIRTUAL|LOOKUP_DESTRUCTOR, 0);

      if (expr != error_mark_node)
	add_partial_entry (expr);

      pop_obstacks ();
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

#if 0
	  /* This happens in templates, since the IDENTIFIER is replaced
             with the COMPONENT_REF in tsubst_expr.  */
	  field = (TREE_CODE (name) == COMPONENT_REF
		   ? TREE_OPERAND (name, 1) : IDENTIFIER_CLASS_VALUE (name));
#else
	  /* Let's find out when this happens.  */
	  my_friendly_assert (TREE_CODE (name) != COMPONENT_REF, 348);
	  field = IDENTIFIER_CLASS_VALUE (name);
#endif

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
	      tree v = CLASSTYPE_VBASECLASSES (t);
	      while (BINFO_TYPE (v) != BINFO_TYPE (binfo))
		v = TREE_CHAIN (v);

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

/* Invoke a base-class destructor. REF is the object being destroyed,
   BINFO is the base class, and DTOR_ARG indicates whether the base
   class should invoke delete.  */

tree
build_base_dtor_call (ref, binfo, dtor_arg)
     tree ref, binfo, dtor_arg;
{
  tree args = NULL_TREE;
  tree vlist = lookup_name (vlist_identifier, 0);
  tree call, decr;

  if (TYPE_USES_PVBASES (BINFO_TYPE (binfo)))
    {
      args = expr_tree_cons (NULL_TREE, vlist, args);
      dtor_arg = build (BIT_IOR_EXPR, integer_type_node,
			dtor_arg, build_int_2 (4, 0));
      dtor_arg = fold (dtor_arg);
    }
  args = expr_tree_cons (NULL_TREE, dtor_arg, args);
  call = build_scoped_method_call (ref, binfo, dtor_identifier, args);

  if (!TYPE_USES_PVBASES (BINFO_TYPE (binfo)))
    /* For plain inheritance, do not try to adjust __vlist. */
    return call;

  /* Now decrement __vlist by the number of slots consumed by the base
     dtor. */
  decr = build_int_2 (pvbasecount (BINFO_TYPE (binfo), 0), 0);
  decr = build_binary_op (MINUS_EXPR, vlist, decr);
  decr = build_modify_expr (vlist, NOP_EXPR, decr);

  return build (COMPOUND_EXPR, void_type_node, call, decr);
}

/* Return the number of vlist entries needed to initialize TYPE,
   depending on whether it is IN_CHARGE. */

static int
pvbasecount (type, in_charge)
     tree type;
     int in_charge;
{
  int i;
  int result = 0;
  tree vbase;

  for (vbase = (CLASSTYPE_VBASECLASSES (type)); vbase;
       vbase = TREE_CHAIN (vbase))
    {
      result += list_length (CLASSTYPE_VFIELDS (BINFO_TYPE (vbase)));
      if (in_charge)
	result += pvbasecount (BINFO_TYPE (vbase), 0);
    }

  for (i=0; i < CLASSTYPE_N_BASECLASSES (type); i++)
    {
      tree base = TREE_VEC_ELT (TYPE_BINFO_BASETYPES (type), i);
      if (TREE_VIA_VIRTUAL (base))
	continue;
      result += pvbasecount (BINFO_TYPE (base), 0);
    }
  return result;
}

void
init_vlist (t)
     tree t;
{
  char *name;
  tree expr;
  tree vlist = lookup_name (vlist_identifier, 0);

  name = alloca (strlen (VLIST_NAME_FORMAT) 
		 + TYPE_ASSEMBLER_NAME_LENGTH (t) + 2);
  sprintf (name, VLIST_NAME_FORMAT, TYPE_ASSEMBLER_NAME_STRING (t));

  expr = get_identifier (name);
  expr = lookup_name (expr, 0);
  expr = build1 (ADDR_EXPR, TREE_TYPE (vlist), expr);
  if (DECL_DESTRUCTOR_FOR_PVBASE_P (current_function_decl))
    /* Move to the end of the vlist. */
    expr = build_binary_op (PLUS_EXPR, expr, 
			    build_int_2 (pvbasecount (t, 1), 0));
  expand_expr_stmt (build_modify_expr (vlist, NOP_EXPR, expr));
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

extern tree base_init_expr, rtl_expr_chain;

void
emit_base_init (t, immediately)
     tree t;
     int immediately;
{
  tree member;
  tree mem_init_list;
  tree rbase_init_list, vbase_init_list;
  tree t_binfo = TYPE_BINFO (t);
  tree binfos = BINFO_BASETYPES (t_binfo);
  int i, n_baseclasses = binfos ? TREE_VEC_LENGTH (binfos) : 0;
  tree expr = NULL_TREE;
  tree vlist = lookup_name (vlist_identifier, 0);

  if (! immediately)
    {
      int momentary;
      do_pending_stack_adjust ();
      /* Make the RTL_EXPR node temporary, not momentary,
	 so that rtl_expr_chain doesn't become garbage.  */
      momentary = suspend_momentary ();
      expr = make_node (RTL_EXPR);
      resume_momentary (momentary);
      start_sequence_for_rtl_expr (expr); 
    }

  if (write_symbols == NO_DEBUG)
    /* As a matter of principle, `start_sequence' should do this.  */
    emit_note (0, -1);
  else
    /* Always emit a line number note so we can step into constructors.  */
    emit_line_note_force (DECL_SOURCE_FILE (current_function_decl),
			  DECL_SOURCE_LINE (current_function_decl));

  mem_init_list = sort_member_init (t);
  current_member_init_list = NULL_TREE;

  sort_base_init (t, &rbase_init_list, &vbase_init_list);
  current_base_init_list = NULL_TREE;

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
	  expand_start_target_temps ();

	  member = convert_pointer_to_real (base_binfo, current_class_ptr);
	  expand_aggr_init_1 (base_binfo, NULL_TREE,
			      build_indirect_ref (member, NULL_PTR), init,
			      LOOKUP_NORMAL);

	  expand_end_target_temps ();
	  free_temp_slots ();
	}

      expand_cleanup_for_base (base_binfo, vlist, NULL_TREE);
      rbase_init_list = TREE_CHAIN (rbase_init_list);
    }

  /* Initialize all the virtual function table fields that
     do come from virtual base classes.  */
  if (TYPE_USES_VIRTUAL_BASECLASSES (t))
    expand_indirect_vtbls_init (t_binfo, current_class_ref, current_class_ptr);

  /* Initialize all the virtual function table fields that
     do not come from virtual base classes.  */
  expand_direct_vtbls_init (t_binfo, t_binfo, 1, 1, current_class_ptr);

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

#if 0
	  if (TREE_CODE (name) == COMPONENT_REF)
	    name = DECL_NAME (TREE_OPERAND (name, 1));
#else
	  /* Also see if it's ever a COMPONENT_REF here.  If it is, we
	     need to do `expand_assignment (name, init, 0, 0);' and
	     a continue.  */
	  my_friendly_assert (TREE_CODE (name) != COMPONENT_REF, 349);
#endif
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
	  /* XXX: this may need the COMPONENT_REF operand 0 check if
	     it turns out we actually get them.  */
	  field = IDENTIFIER_CLASS_VALUE (name);

	  /* If one member shadows another, get the outermost one.  */
	  if (TREE_CODE (field) == TREE_LIST)
	    {
	      field = TREE_VALUE (field);
	      if (decl_type_context (field) != current_class_type)
		cp_error ("field `%D' not in immediate context", field);
	    }

#if 0
	  /* It turns out if you have an anonymous union in the
	     class, a member from it can end up not being on the
	     list of fields (rather, the type is), and therefore
	     won't be seen by the for loop above.  */

	  /* The code in this for loop is derived from a general loop
	     which had this check in it.  Theoretically, we've hit
	     every initialization for the list of members in T, so
	     we shouldn't have anything but these left in this list.  */
	  my_friendly_assert (DECL_FIELD_CONTEXT (field) != t, 351);
#endif

	  perform_member_init (field, name, init, 1);
	}
      mem_init_list = TREE_CHAIN (mem_init_list);
    }

  if (! immediately)
    {
      do_pending_stack_adjust ();
      my_friendly_assert (base_init_expr == 0, 207);
      base_init_expr = expr;
      TREE_TYPE (expr) = void_type_node;
      RTL_EXPR_RTL (expr) = const0_rtx;
      RTL_EXPR_SEQUENCE (expr) = get_insns ();
      rtl_expr_chain = tree_cons (NULL_TREE, expr, rtl_expr_chain);
      end_sequence ();
      TREE_SIDE_EFFECTS (expr) = 1;
    }

  /* All the implicit try blocks we built up will be zapped
     when we come to a real binding contour boundary.  */
}

/* Check that all fields are properly initialized after
   an assignment to `this'.  */

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

  /* This code is crusty.  Should be simple, like:
     vtbl = BINFO_VTABLE (binfo);
     */
  vtype = DECL_CONTEXT (CLASSTYPE_VFIELD (type));
  vtype_binfo = get_binfo (vtype, TREE_TYPE (TREE_TYPE (decl)), 0);
  vtbl = BINFO_VTABLE (binfo_value (DECL_FIELD_CONTEXT (CLASSTYPE_VFIELD (type)), binfo));
  assemble_external (vtbl);
  TREE_USED (vtbl) = 1;
  vtbl = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (vtbl)), vtbl);
  decl = convert_pointer_to_real (vtype_binfo, decl);
  vtbl_ptr = build_vfield_ref (build_indirect_ref (decl, NULL_PTR), vtype);
  if (vtbl_ptr == error_mark_node)
    return;

  /* Have to convert VTBL since array sizes may be different.  */
  vtbl = convert_force (TREE_TYPE (vtbl_ptr), vtbl, 0);
  expand_expr_stmt (build_modify_expr (vtbl_ptr, NOP_EXPR, vtbl));
}

/* If an exception is thrown in a constructor, those base classes already
   constructed must be destroyed.  This function creates the cleanup
   for BINFO, which has just been constructed.  If FLAG is non-NULL,
   it is a DECL which is non-zero when this base needs to be
   destroyed.  */

static void
expand_cleanup_for_base (binfo, vlist, flag)
     tree binfo;
     tree vlist;
     tree flag;
{
  tree expr;

  if (TYPE_NEEDS_DESTRUCTOR (BINFO_TYPE (binfo)))
    {
      /* All cleanups must be on the function_obstack.  */
      push_obstacks_nochange ();
      resume_temporary_allocation ();

      /* Call the destructor.  */
      expr = build_base_dtor_call (current_class_ref, binfo,
      				   integer_zero_node);
      if (flag)
	expr = fold (build (COND_EXPR, void_type_node,
			    truthvalue_conversion (flag),
			    expr, integer_zero_node));

      pop_obstacks ();
      add_partial_entry (expr);
    }

  if (TYPE_USES_PVBASES (BINFO_TYPE (binfo)))
    {
      /* Increment vlist by number of base's vbase classes. */
      expr = build_int_2 (pvbasecount (BINFO_TYPE (binfo), 0), 0);
      expr = build_binary_op (PLUS_EXPR, vlist, expr);
      expr = build_modify_expr (vlist, NOP_EXPR, expr);
      expand_expr_stmt (expr);
    }
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

  expand_start_target_temps ();

  if (init)
    init = TREE_VALUE (init);
  /* Call constructors, but don't set up vtables.  */
  expand_aggr_init_1 (binfo, exp, ref, init, LOOKUP_COMPLAIN);

  expand_end_target_temps ();
  free_temp_slots ();
}

/* Construct the virtual base-classes of THIS_REF (whose address is
   THIS_PTR).  The object has the indicated TYPE.  The construction
   actually takes place only if FLAG is non-zero.  INIT_LIST is list
   of initialization for constructor to perform.  */

static void
construct_virtual_bases (type, this_ref, this_ptr, init_list, flag)
     tree type;
     tree this_ref;
     tree this_ptr;
     tree init_list;
     tree flag;
{
  tree vbases;
  tree result;
  tree vlist = NULL_TREE;

  /* If there are no virtual baseclasses, we shouldn't even be here.  */
  my_friendly_assert (TYPE_USES_VIRTUAL_BASECLASSES (type), 19990621);

  /* First set the pointers in our object that tell us where to find
     our virtual baseclasses.  */
  expand_start_cond (flag, 0);
  if (TYPE_USES_PVBASES (type))
    {
      init_vlist (type);
      vlist = lookup_name (vlist_identifier, 0);
    }
  result = init_vbase_pointers (type, this_ptr);
  if (result)
    expand_expr_stmt (build_compound_expr (result));
  expand_end_cond ();

  /* Now, run through the baseclasses, initializing each.  */ 
  for (vbases = CLASSTYPE_VBASECLASSES (type); vbases;
       vbases = TREE_CHAIN (vbases))
    {
      tree tmp = purpose_member (vbases, result);
      
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
      expand_start_cond (flag, 0);
      expand_aggr_vbase_init_1 (vbases, this_ref,
				TREE_OPERAND (TREE_VALUE (tmp), 0),
				init_list);
      expand_end_cond ();
      
      expand_cleanup_for_base (vbases, vlist, flag);
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
  while (t && ANON_UNION_TYPE_P (t))
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
	       && ! binfo_member (basetype, CLASSTYPE_VBASECLASSES (type)))
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

   ALIAS_THIS is nonzero iff we are initializing something which is
   essentially an alias for current_class_ref.  In this case, the base
   constructor may move it on us, and we must keep track of such
   deviations.

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

void
expand_aggr_init (exp, init, flags)
     tree exp, init;
     int flags;
{
  tree type = TREE_TYPE (exp);
  int was_const = TREE_READONLY (exp);
  int was_volatile = TREE_THIS_VOLATILE (exp);

  if (init == error_mark_node)
    return;

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
	  return;
	}
      expand_vec_init (exp, exp, array_type_nelts (type), init,
		       init && same_type_p (TREE_TYPE (init),
					    TREE_TYPE (exp)));
      TREE_READONLY (exp) = was_const;
      TREE_THIS_VOLATILE (exp) = was_volatile;
      TREE_TYPE (exp) = type;
      if (init)
	TREE_TYPE (init) = itype;
      return;
    }

  if (TREE_CODE (exp) == VAR_DECL || TREE_CODE (exp) == PARM_DECL)
    /* just know that we've seen something for this node */
    TREE_USED (exp) = 1;

#if 0
  /* If initializing from a GNU C CONSTRUCTOR, consider the elts in the
     constructor as parameters to an implicit GNU C++ constructor.  */
  if (init && TREE_CODE (init) == CONSTRUCTOR
      && TYPE_HAS_CONSTRUCTOR (type)
      && TREE_TYPE (init) == type)
    init = CONSTRUCTOR_ELTS (init);
#endif

  TREE_TYPE (exp) = TYPE_MAIN_VARIANT (type);
  expand_aggr_init_1 (TYPE_BINFO (type), exp, exp,
		      init, LOOKUP_NORMAL|flags);
  TREE_TYPE (exp) = type;
  TREE_READONLY (exp) = was_const;
  TREE_THIS_VOLATILE (exp) = was_volatile;
}

static tree
no_vlist_base_init (rval, exp, init, binfo, flags)
     tree rval, exp, init, binfo;
     int flags;
{
  tree nrval, func, parms;

  /* Obtain the vlist-expecting ctor.  */
  func = rval;
  my_friendly_assert (TREE_CODE (func) == CALL_EXPR, 20000131);
  func = TREE_OPERAND (func, 0);
  my_friendly_assert (TREE_CODE (func) == ADDR_EXPR, 20000132);

  if (init == NULL_TREE
      || (TREE_CODE (init) == TREE_LIST && ! TREE_TYPE (init)))
    {
      parms = init;
      if (parms)
	init = TREE_VALUE (parms);
    }
  else
    parms = build_expr_list (NULL_TREE, init);

  flags &= ~LOOKUP_HAS_VLIST;

  parms = expr_tree_cons (NULL_TREE, integer_zero_node, parms);
  flags |= LOOKUP_HAS_IN_CHARGE;
  
  nrval = build_method_call (exp, ctor_identifier,
			     parms, binfo, flags);

  func = build (NE_EXPR, boolean_type_node,
		func, null_pointer_node);
  nrval = build (COND_EXPR, void_type_node,
		 func, rval, nrval);
  return nrval;
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
  tree vlist = NULL_TREE;
  tree orig_init = init;

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
      expand_expr_stmt (init);
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
    parms = build_expr_list (NULL_TREE, init);

  if (TYPE_USES_VIRTUAL_BASECLASSES (type))
    {
      if (TYPE_USES_PVBASES (type))
	{
	  /* In compatibility mode, when not calling a base ctor,
	     we do not pass the vlist argument.  */
	  if (true_exp == exp)
	    vlist = flag_vtable_thunks_compat? NULL_TREE : vlist_zero_node;
	  else
	    vlist = lookup_name (vlist_identifier, 0);
	      
	  if (vlist)
	    {
	      parms = expr_tree_cons (NULL_TREE, vlist, parms);
	      flags |= LOOKUP_HAS_VLIST;
	    }
	}
      if (true_exp == exp)
	parms = expr_tree_cons (NULL_TREE, integer_one_node, parms);
      else
	parms = expr_tree_cons (NULL_TREE, integer_zero_node, parms);
      flags |= LOOKUP_HAS_IN_CHARGE;
    }

  rval = build_method_call (exp, ctor_identifier,
			    parms, binfo, flags);
  if (vlist && true_exp != exp && flag_vtable_thunks_compat)
    {
      rval = no_vlist_base_init (rval, exp, orig_init, binfo, flags);
    }
  if (TREE_SIDE_EFFECTS (rval))
    expand_expr_stmt (rval);
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

   ALIAS_THIS serves the same purpose it serves for expand_aggr_init.

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
      tree t = store_init_value (exp, init);
      if (!t)
	{
	  expand_decl_init (exp);
	  return;
	}
      t = build (INIT_EXPR, type, exp, init);
      TREE_SIDE_EFFECTS (t) = 1;
      expand_expr_stmt (t);
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

  /* A lot of this logic is now handled in lookup_field and
     lookup_fnfield.  */
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
	  basebinfo = TREE_PURPOSE (fnfields);
	  if (!enforce_access (basebinfo, t))
	    return error_mark_node;
	  mark_used (t);
	  if (DECL_STATIC_FUNCTION_P (t))
	    return t;
	  return build (OFFSET_REF, TREE_TYPE (t), decl, t);
	}

      /* FNFIELDS is most likely allocated on the search_obstack,
	 which will go away after this class scope.  If we need
	 to save this value for later (i.e. for use as an initializer
	 for a static variable), then do so here.

	 ??? The smart thing to do for the case of saving initializers
	 is to resolve them before we're done with this scope.  */
      if (!TREE_PERMANENT (fnfields)
	  && ! allocation_temporary_p ())
	fnfields = copy_list (fnfields);

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
      cp_pedwarn ("assuming & on overloaded member function");
      return build_unary_op (ADDR_EXPR, exp, 0);
    }

  if (TREE_CODE (TREE_TYPE (member)) == METHOD_TYPE)
    {
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
      tree basetype_path;
      tree expr;

      if (TREE_CODE (exp) == OFFSET_REF && TREE_CODE (type) == OFFSET_TYPE)
	basetype = TYPE_OFFSET_BASETYPE (type);
      else
	basetype = DECL_CONTEXT (member);

      base = current_class_ptr;
      
      if (get_base_distance (basetype, TREE_TYPE (TREE_TYPE (base)), 0, &basetype_path) < 0)
	{
	  error_not_base_type (basetype, TREE_TYPE (TREE_TYPE (base)));
	  return error_mark_node;
	}
      /* Kludge: we need to use basetype_path now, because
	 convert_pointer_to will bash it.  */
      enforce_access (basetype_path, member);
      addr = convert_pointer_to (basetype, base);

      /* Even in the case of illegal access, we form the
	 COMPONENT_REF; that will allow better error recovery than
	 just feeding back error_mark_node.  */
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
		     void_type_node, build_expr_list (NULL_TREE, addr));
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

  tree pending_sizes = NULL_TREE;

  if (decl == error_mark_node)
    return error_mark_node;

  if (TREE_CODE (decl) == TREE_LIST)
    {
      tree absdcl = TREE_VALUE (decl);
      tree last_absdcl = NULL_TREE;
      int old_immediate_size_expand = 0;

      if (current_function_decl
	  && DECL_CONSTRUCTOR_P (current_function_decl))
	{
	  old_immediate_size_expand = immediate_size_expand;
	  immediate_size_expand = 0;
	}

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
	{
	  immediate_size_expand = old_immediate_size_expand;
	  return error_mark_node;
	}

      if (current_function_decl
	  && DECL_CONSTRUCTOR_P (current_function_decl))
	{
	  pending_sizes = get_pending_sizes ();
	  immediate_size_expand = old_immediate_size_expand;
	}
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
	t = min_tree_cons (min_tree_cons (NULL_TREE, type, NULL_TREE),
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

  /* Wrap it in a NOP_EXPR so warn_if_unused_value doesn't complain.  */
  rval = build1 (NOP_EXPR, TREE_TYPE (rval), rval);
  TREE_NO_UNUSED_WARNING (rval) = 1;

  if (pending_sizes)
    rval = build_compound_expr (chainon (pending_sizes,
					 build_expr_list (NULL_TREE, rval)));

  return rval;
}

/* If non-NULL, a POINTER_TYPE equivalent to (java::lang::Class*). */

static tree jclass_node = NULL_TREE;

/* Given a Java class, return a decl for the corresponding java.lang.Class. */

static tree
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
      push_obstacks_nochange ();
      end_temporary_allocation ();
      class_decl = build_decl (VAR_DECL, name, TREE_TYPE (jclass_node));
      TREE_STATIC (class_decl) = 1;
      DECL_EXTERNAL (class_decl) = 1;
      TREE_PUBLIC (class_decl) = 1;
      DECL_ARTIFICIAL (class_decl) = 1;
      DECL_IGNORED_P (class_decl) = 1;
      pushdecl_top_level (class_decl);
      make_decl_rtl (class_decl, NULL_PTR, 1);
      pop_obstacks ();
    }
  return class_decl;
}

/* Called from cplus_expand_expr when expanding a NEW_EXPR.  The return
   value is immediately handed to expand_expr.  */

tree
build_new_1 (exp)
     tree exp;
{
  tree placement, init;
  tree type, true_type, size, rval;
  tree nelts = NULL_TREE;
  tree alloc_expr, alloc_node = NULL_TREE;
  int has_array = 0;
  enum tree_code code = NEW_EXPR;
  int use_cookie, nothrow, check_new;
  int use_global_new;
  int use_java_new = 0;

  placement = TREE_OPERAND (exp, 0);
  type = TREE_OPERAND (exp, 1);
  init = TREE_OPERAND (exp, 2);
  use_global_new = NEW_EXPR_USE_GLOBAL (exp);

  if (TREE_CODE (type) == ARRAY_REF)
    {
      has_array = 1;
      nelts = TREE_OPERAND (type, 1);
      type = TREE_OPERAND (type, 0);
    }
  true_type = type;

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

  if (TYPE_LANG_SPECIFIC (true_type)
      && CLASSTYPE_ABSTRACT_VIRTUALS (true_type))
    {
      abstract_virtuals_error (NULL_TREE, true_type);
      return error_mark_node;
    }

  if (TYPE_LANG_SPECIFIC (true_type) && IS_SIGNATURE (true_type))
    {
      signature_error (NULL_TREE, true_type);
      return error_mark_node;
    }
  
  /* When we allocate an array, and the corresponding deallocation
     function takes a second argument of type size_t, and that's the
     "usual deallocation function", we allocate some extra space at
     the beginning of the array to store the size of the array.

     Well, that's what we should do.  For backwards compatibility, we
     have to do this whenever there's a two-argument array-delete
     operator. 

     FIXME: For -fnew-abi, we don't have to maintain backwards
     compatibility and we should fix this.  */
  use_cookie = (has_array && TYPE_VEC_NEW_USES_COOKIE (true_type)
		&& ! (placement && ! TREE_CHAIN (placement)
		      && TREE_TYPE (TREE_VALUE (placement)) == ptr_type_node));

  if (use_cookie)
    {
      tree extra = BI_header_size;

      size = size_binop (PLUS_EXPR, size, extra);
    }

  if (has_array)
    {
      code = VEC_NEW_EXPR;

      if (init && pedantic)
	cp_pedwarn ("initialization in array new");
    }

  /* Allocate the object.  */
  
  if (! has_array && ! placement && flag_this_is_variable > 0
      && TYPE_NEEDS_CONSTRUCTING (true_type) && init != void_type_node)
    {
      if (init == NULL_TREE || TREE_CODE (init) == TREE_LIST)
	rval = NULL_TREE;
      else
	{
	  error ("constructors take parameter lists");
	  return error_mark_node;
	}
    }
  else if (! placement && TYPE_FOR_JAVA (true_type))
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
      int susp = 0;

      if (flag_exceptions)
	/* We will use RVAL when generating an exception handler for
	   this new-expression, so we must save it.  */
	susp = suspend_momentary ();

      rval = build_op_new_call
	(code, true_type, expr_tree_cons (NULL_TREE, size, placement),
	 LOOKUP_NORMAL | (use_global_new * LOOKUP_GLOBAL));
      rval = cp_convert (build_pointer_type (true_type), rval);

      if (flag_exceptions)
	resume_momentary (susp);
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
      tree extra = BI_header_size;
      tree cookie, exp1;
      rval = convert (string_type_node, rval); /* for ptr arithmetic */
      rval = save_expr (build_binary_op (PLUS_EXPR, rval, extra));
      /* Store header info.  */
      cookie = build_indirect_ref (build (MINUS_EXPR,
					  build_pointer_type (BI_header_type),
					  rval, extra), NULL_PTR);
      exp1 = build (MODIFY_EXPR, void_type_node,
		    build_component_ref (cookie, nc_nelts_field_id,
					 NULL_TREE, 0),
		    nelts);
      TREE_SIDE_EFFECTS (exp1) = 1;
      rval = cp_convert (build_pointer_type (true_type), rval);
      rval = build_compound_expr
	(expr_tree_cons (NULL_TREE, exp1,
			 build_expr_list (NULL_TREE, rval)));
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
	      if (TYPE_USES_PVBASES (true_type)
                  && !flag_vtable_thunks_compat)
		{
		  init = expr_tree_cons (NULL_TREE, vlist_zero_node, init);
		  flags |= LOOKUP_HAS_VLIST;
		}
	      init = expr_tree_cons (NULL_TREE, integer_one_node, init);
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
	rval = build (VEC_INIT_EXPR, TREE_TYPE (rval),
		      save_expr (rval), init, nelts);

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
	  int flags = LOOKUP_NORMAL | (use_global_new * LOOKUP_GLOBAL);

	  /* All cleanups must last longer than normal.  */
	  int yes = suspend_momentary ();

	  if (placement)
	    {
	      flags |= LOOKUP_SPECULATIVELY;

	      /* We expect alloc_expr to look like a TARGET_EXPR around
		 a NOP_EXPR around the CALL_EXPR we want.  */
	      fn = TREE_OPERAND (alloc_expr, 1);
	      fn = TREE_OPERAND (fn, 0);
	    }

	  /* Copy size to the saveable obstack.  */
	  size = mapcar (size, permanent_p);

	  cleanup = build_op_delete_call (dcode, alloc_node, size, flags, fn);

	  resume_momentary (yes);

	  /* Ack!  First we allocate the memory.  Then we set our sentry
	     variable to true, and expand a cleanup that deletes the memory
	     if sentry is true.  Then we run the constructor and store the
	     returned pointer in buf.  Then we clear sentry and return buf.  */

	  if (cleanup)
	    {
	      tree end, sentry, begin, buf, t = TREE_TYPE (rval);

	      begin = get_target_expr (boolean_true_node);
	      sentry = TREE_OPERAND (begin, 0);

	      yes = suspend_momentary ();
	      TREE_OPERAND (begin, 2)
		= build (COND_EXPR, void_type_node, sentry,
			 cleanup, void_zero_node);
	      resume_momentary (yes);

	      rval = get_target_expr (rval);

	      end = build (MODIFY_EXPR, TREE_TYPE (sentry),
			   sentry, boolean_false_node);
	      TREE_SIDE_EFFECTS (end) = 1;

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
build_vec_delete_1 (base, maxindex, type, auto_delete_vec, auto_delete,
		    use_global_delete)
     tree base, maxindex, type;
     tree auto_delete_vec, auto_delete;
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

  if (! IS_AGGR_TYPE (type) || ! TYPE_NEEDS_DESTRUCTOR (type))
    {
      loop = integer_zero_node;
      goto no_destructor;
    }

  /* The below is short by BI_header_size */
  virtual_size = fold (size_binop (MULT_EXPR, size_exp, maxindex));

  tbase = build_decl (VAR_DECL, NULL_TREE, ptype);
  tbase_init = build_modify_expr (tbase, NOP_EXPR,
				  fold (build (PLUS_EXPR, ptype,
					       base,
					       virtual_size)));
  DECL_REGISTER (tbase) = 1;
  controller = build (BIND_EXPR, void_type_node, tbase, NULL_TREE, NULL_TREE);
  TREE_SIDE_EFFECTS (controller) = 1;

  if (auto_delete != integer_zero_node
      && auto_delete != integer_two_node)
    {
      tree base_tbd = cp_convert (ptype,
				  build_binary_op (MINUS_EXPR,
						   cp_convert (ptr_type_node, base),
						   BI_header_size));
      /* This is the real size */
      virtual_size = size_binop (PLUS_EXPR, virtual_size, BI_header_size);
      body = build_expr_list (NULL_TREE,
			      build_x_delete (base_tbd,
					      2 | use_global_delete,
					      virtual_size));
      body = build (COND_EXPR, void_type_node,
		    build (BIT_AND_EXPR, integer_type_node,
			   auto_delete, integer_one_node),
		    body, integer_zero_node);
    }
  else
    body = NULL_TREE;

  body = expr_tree_cons (NULL_TREE,
		    build_delete (ptype, tbase, auto_delete,
				  LOOKUP_NORMAL|LOOKUP_DESTRUCTOR, 1),
		    body);

  body = expr_tree_cons (NULL_TREE,
		    build_modify_expr (tbase, NOP_EXPR, build (MINUS_EXPR, ptype, tbase, size_exp)),
		    body);

  body = expr_tree_cons (NULL_TREE,
		    build (EXIT_EXPR, void_type_node,
			   build (EQ_EXPR, boolean_type_node, base, tbase)),
		    body);

  loop = build (LOOP_EXPR, void_type_node, build_compound_expr (body));

  loop = expr_tree_cons (NULL_TREE, tbase_init,
		    expr_tree_cons (NULL_TREE, loop, NULL_TREE));
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
      virtual_size = fold (size_binop (MULT_EXPR, size_exp, maxindex));

      if (! TYPE_VEC_NEW_USES_COOKIE (type))
	/* no header */
	base_tbd = base;
      else
	{
	  base_tbd = cp_convert (ptype,
				 build_binary_op (MINUS_EXPR,
						  cp_convert (string_type_node, base),
						  BI_header_size));
	  /* True size with header.  */
	  virtual_size = size_binop (PLUS_EXPR, virtual_size, BI_header_size);
	}
      deallocate_expr = build_x_delete (base_tbd,
					2 | use_global_delete,
					virtual_size);
      if (auto_delete_vec != integer_one_node)
	deallocate_expr = build (COND_EXPR, void_type_node,
				 build (BIT_AND_EXPR, integer_type_node,
					auto_delete_vec, integer_one_node),
				 deallocate_expr, integer_zero_node);
    }

  if (loop && deallocate_expr != integer_zero_node)
    {
      body = expr_tree_cons (NULL_TREE, loop,
			expr_tree_cons (NULL_TREE, deallocate_expr, NULL_TREE));
      body = build_compound_expr (body);
    }
  else
    body = loop;

  /* Outermost wrapper: If pointer is null, punt.  */
  body = build (COND_EXPR, void_type_node,
		build (NE_EXPR, boolean_type_node, base, integer_zero_node),
		body, integer_zero_node);
  body = build1 (NOP_EXPR, void_type_node, body);

  if (controller)
    {
      TREE_OPERAND (controller, 1) = body;
      return controller;
    }
  else
    return cp_convert (void_type_node, body);
}

/* Protect the vector initialization with a try-block so that we can
   destroy the first few elements if constructing a later element
   causes an exception to be thrown.  TYPE is the type of the array
   elements.  */

static void
expand_vec_init_try_block (type)
     tree type;
{
  if (!TYPE_NEEDS_DESTRUCTOR (type) || !flag_exceptions)
    return;

  /* The code we generate looks like:

       try {
         // Initialize the vector.
       } catch (...) {
         // Destory the elements that need destroying.
	 throw;
       } 

     Here we're just beginning the `try'.  */

  expand_eh_region_start ();
}

/* Add code to destroy the array elements constructed so far if the
   construction of some element in the array causes an exception to be
   thrown.  RVAL is the address of the last element in the array.
   TYPE is the type of the array elements.  MAXINDEX is the maximum
   allowable index into the array.  ITERATOR is an integer variable
   indicating how many elements remain to be constructed.  */

static void
expand_vec_init_catch_clause (rval, type, maxindex, iterator)
     tree rval;
     tree type;
     tree maxindex;
     tree iterator;
{
  tree e;
  tree cleanup;

  if (!TYPE_NEEDS_DESTRUCTOR (type) || !flag_exceptions)
    return;
    
  /* We have to ensure that this can live to the cleanup expansion
     time, since we know it is only ever needed once, generate code
     now.  */
  push_obstacks_nochange ();
  resume_temporary_allocation ();

  cleanup = make_node (RTL_EXPR);
  TREE_TYPE (cleanup) = void_type_node;
  RTL_EXPR_RTL (cleanup) = const0_rtx;
  TREE_SIDE_EFFECTS (cleanup) = 1;
  do_pending_stack_adjust ();
  start_sequence_for_rtl_expr (cleanup);
    
  e = build_vec_delete_1 (rval,
			  build_binary_op (MINUS_EXPR, maxindex, 
					   iterator),
			  type,
			  /*auto_delete_vec=*/integer_zero_node,
			  /*auto_delete=*/integer_zero_node,
			  /*use_global_delete=*/0);
  expand_expr (e, const0_rtx, VOIDmode, EXPAND_NORMAL);

  do_pending_stack_adjust ();
  RTL_EXPR_SEQUENCE (cleanup) = get_insns ();
  end_sequence ();
  cleanup = protect_with_terminate (cleanup);
  expand_eh_region_end (cleanup);
  pop_obstacks ();
}

/* `expand_vec_init' performs initialization of a vector of aggregate
   types.

   DECL is passed only for error reporting, and provides line number
   and source file name information.
   BASE is the space where the vector will be.
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
expand_vec_init (decl, base, maxindex, init, from_array)
     tree decl, base, maxindex, init;
     int from_array;
{
  tree rval;
  tree base2 = NULL_TREE;
  tree type = TREE_TYPE (TREE_TYPE (base));
  tree size;
  tree itype = NULL_TREE;
  tree iterator;
  int num_initialized_elts = 0;

  maxindex = cp_convert (ptrdiff_type_node, maxindex);
  if (maxindex == error_mark_node)
    return error_mark_node;

  if (current_function_decl == NULL_TREE)
    {
      rval = make_tree_vec (3);
      TREE_VEC_ELT (rval, 0) = base;
      TREE_VEC_ELT (rval, 1) = maxindex;
      TREE_VEC_ELT (rval, 2) = init;
      return rval;
    }

  size = size_in_bytes (type);

  base = default_conversion (base);
  base = cp_convert (build_pointer_type (type), base);
  rval = get_temp_regvar (build_pointer_type (type), base);
  base = get_temp_regvar (build_pointer_type (type), base);
  iterator = get_temp_regvar (ptrdiff_type_node, maxindex);

  /* Protect the entire array initialization so that we can destroy
     the partially constructed array if an exception is thrown.  */
  expand_vec_init_try_block (type);

  if (init != NULL_TREE && TREE_CODE (init) == CONSTRUCTOR
      && (!decl || same_type_p (TREE_TYPE (init), TREE_TYPE (decl))))
    {
      /* Do non-default initialization resulting from brace-enclosed
	 initializers.  */

      tree elts;
      tree baseref = build1 (INDIRECT_REF, type, base);

      from_array = 0;

      for (elts = CONSTRUCTOR_ELTS (init); elts; elts = TREE_CHAIN (elts))
	{
	  tree elt = TREE_VALUE (elts);

	  num_initialized_elts++;

	  if (IS_AGGR_TYPE (type) || TREE_CODE (type) == ARRAY_TYPE)
	    expand_aggr_init (baseref, elt, 0);
	  else
	    expand_assignment (baseref, elt, 0, 0);

	  expand_assignment (base, 
			     build (PLUS_EXPR, build_pointer_type (type),
				    base, size),
			     0, 0);
	  expand_assignment (iterator,
			     build (MINUS_EXPR, ptrdiff_type_node,
				    iterator, integer_one_node),
			     0, 0);
	}

      /* Clear out INIT so that we don't get confused below.  */
      init = NULL_TREE;

      if (obey_regdecls)
	use_variable (DECL_RTL (base));
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
	  && !(TREE_CODE (maxindex) == INTEGER_CST
	       && num_initialized_elts == TREE_INT_CST_LOW (maxindex) + 1)))
    {
      /* If the ITERATOR is equal to -1, then we don't have to loop;
	 we've already initialized all the elements.  */
      expand_start_cond (build (NE_EXPR, boolean_type_node,
				iterator, minus_one),
			 0);

      /* Otherwise, loop through the elements.  */
      expand_start_loop_continue_elsewhere (1);
  
      /* The initialization of each array element is a full-expression.  */
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
	    expand_expr_stmt (build_modify_expr (to, NOP_EXPR, from));
	  else if (TYPE_NEEDS_CONSTRUCTING (type))
	    expand_aggr_init (to, from, 0);
	  else if (from)
	    expand_assignment (to, from, 0, 0);
	  else
	    my_friendly_abort (57);
	}
      else if (TREE_CODE (type) == ARRAY_TYPE)
	{
	  if (init != 0)
	    sorry ("cannot initialize multi-dimensional array with initializer");
	  expand_vec_init (decl, 
			   build1 (NOP_EXPR, 
				   build_pointer_type (TREE_TYPE
						       (type)),
				   base),
			   array_type_nelts (type), 0, 0);
	}
      else
	expand_aggr_init (build1 (INDIRECT_REF, type, base), init, 0);

      expand_assignment (base,
			 build (PLUS_EXPR, build_pointer_type (type), 
				base, size), 0, 0);
      if (base2)
	expand_assignment (base2,
			   build (PLUS_EXPR, build_pointer_type (type), 
				  base2, size), 0, 0);

      /* Cleanup any temporaries needed for the initial value.  */
      expand_end_target_temps ();
  
      expand_loop_continue_here ();
      expand_exit_loop_if_false (0, build (NE_EXPR, boolean_type_node,
					   build (PREDECREMENT_EXPR, 
						  ptrdiff_type_node, 
						  iterator,
						  integer_one_node), 
					   minus_one));
  
      if (obey_regdecls)
	{
	  use_variable (DECL_RTL (base));
	  if (base2)
	    use_variable (DECL_RTL (base2));
	}

      expand_end_loop ();
      expand_end_cond ();
    }

  /* Make sure to cleanup any partially constructed elements.  */
  expand_vec_init_catch_clause (rval, type, maxindex, iterator);

  if (obey_regdecls)
    {
      use_variable (DECL_RTL (iterator));
      use_variable (DECL_RTL (rval));
    }

  return rval;
}

/* Free up storage of type TYPE, at address ADDR.

   TYPE is a POINTER_TYPE and can be ptr_type_node for no special type
   of pointer.

   VIRTUAL_SIZE is the amount of storage that was allocated, and is
   used as the second argument to operator delete.  It can include
   things like padding and magic size cookies.  It has virtual in it,
   because if you have a base pointer and you delete through a virtual
   destructor, it should be the size of the dynamic object, not the
   static object, see Free Store 12.5 ANSI C++ WP.

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
			       auto_delete, integer_zero_node,
			       use_global_delete);
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

  if (! TYPE_NEEDS_DESTRUCTOR (type))
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

      /* Maybe pass vlist pointer to destructor.  */
      if (TYPE_USES_PVBASES (type))
	{
	  /* Pass vlist_zero even if in backwards compatibility mode,
	     as the extra argument should not hurt if it is not used.  */
	  expr = build_expr_list (NULL_TREE, vlist_zero_node);
	  flags |= LOOKUP_HAS_VLIST;
	}
      else
	expr = NULL_TREE;

      expr = expr_tree_cons (NULL_TREE, passed_auto_delete, expr);

      expr = build_method_call (ref, dtor_identifier, expr,
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
	       || ! TYPE_NEEDS_DESTRUCTOR (BINFO_TYPE (base_binfo)))
	{
	  cond = build (COND_EXPR, void_type_node,
			build (BIT_AND_EXPR, integer_type_node, auto_delete, integer_one_node),
			build_builtin_delete_call (addr),
			void_zero_node);
	}
      else
	cond = NULL_TREE;

      if (cond)
	exprstmt = build_expr_list (NULL_TREE, cond);

      if (base_binfo
	  && ! TREE_VIA_VIRTUAL (base_binfo)
	  && TYPE_NEEDS_DESTRUCTOR (BINFO_TYPE (base_binfo)))
	{
	  tree this_auto_delete;

	  /* Should the base invoke delete? */
	  if (BINFO_OFFSET_ZEROP (base_binfo))
	    this_auto_delete = parent_auto_delete;
	  else
	    this_auto_delete = integer_zero_node;

	  expr = build_base_dtor_call (ref, base_binfo, this_auto_delete);
	  exprstmt = expr_tree_cons (NULL_TREE, expr, exprstmt);
	}

      /* Take care of the remaining baseclasses.  */
      for (i = 1; i < n_baseclasses; i++)
	{
	  base_binfo = TREE_VEC_ELT (binfos, i);
	  if (! TYPE_NEEDS_DESTRUCTOR (BINFO_TYPE (base_binfo))
	      || TREE_VIA_VIRTUAL (base_binfo))
	    continue;

	  expr = build_base_dtor_call (ref, base_binfo, integer_zero_node);

	  exprstmt = expr_tree_cons (NULL_TREE, expr, exprstmt);
	}

      for (member = TYPE_FIELDS (type); member; member = TREE_CHAIN (member))
	{
	  if (TREE_CODE (member) != FIELD_DECL)
	    continue;
	  if (TYPE_NEEDS_DESTRUCTOR (TREE_TYPE (member)))
	    {
	      tree this_member = build_component_ref (ref, DECL_NAME (member), NULL_TREE, 0);
	      tree this_type = TREE_TYPE (member);
	      expr = build_delete (this_type, this_member, integer_two_node, flags, 0);
	      exprstmt = expr_tree_cons (NULL_TREE, expr, exprstmt);
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
      result = expr_tree_cons (NULL_TREE,
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
   AUTO_DELETE say whether each item in the container should be deallocated.

   This also calls delete for virtual baseclasses of elements of the vector.

   Update: MAXINDEX is no longer needed.  The size can be extracted from the
   start of the vector for pointers, and from the type for arrays.  We still
   use MAXINDEX for arrays because it happens to already have one of the
   values we'd have to extract.  (We could use MAXINDEX with pointers to
   confirm the size, and trap if the numbers differ; not clear that it'd
   be worth bothering.)  */

tree
build_vec_delete (base, maxindex, auto_delete_vec, auto_delete,
		  use_global_delete)
     tree base, maxindex;
     tree auto_delete_vec, auto_delete;
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
      tree cookie_addr = build (MINUS_EXPR, build_pointer_type (BI_header_type),
				base, BI_header_size);
      tree cookie = build_indirect_ref (cookie_addr, NULL_PTR);
      maxindex = build_component_ref (cookie, nc_nelts_field_id, NULL_TREE, 0);
      do
	type = TREE_TYPE (type);
      while (TREE_CODE (type) == ARRAY_TYPE);
    }
  else if (TREE_CODE (type) == ARRAY_TYPE)
    {
      /* get the total number of things in the array, maxindex is a bad name */
      maxindex = array_type_nelts_total (type);
      while (TREE_CODE (type) == ARRAY_TYPE)
	type = TREE_TYPE (type);
      base = build_unary_op (ADDR_EXPR, base, 1);
    }
  else
    {
      if (base != error_mark_node)
	error ("type to vector delete is neither pointer or array type");
      return error_mark_node;
    }

  return build_vec_delete_1 (base, maxindex, type, auto_delete_vec, auto_delete,
			     use_global_delete);
}
