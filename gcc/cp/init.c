/* Handle initialization things in C++.
   Copyright (C) 1987, 89, 92, 93, 94, 1995 Free Software Foundation, Inc.
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


/* High-level class interface. */

#include "config.h"
#include "tree.h"
#include "rtl.h"
#include "cp-tree.h"
#include "flags.h"
#include "output.h"

#undef NULL
#define NULL 0

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

void emit_base_init ();
void check_base_init ();
static void expand_aggr_vbase_init ();
void expand_member_init ();
void expand_aggr_init ();

static void expand_aggr_init_1 ();
static void expand_recursive_init_1 ();
static void expand_recursive_init ();
static void expand_virtual_init PROTO((tree, tree));
tree expand_vec_init ();

static void add_friend (), add_friends ();

/* Cache _builtin_new and _builtin_delete exprs.  */
static tree BIN, BID, BIVN, BIVD;

/* Cache the identifier nodes for the two magic field of a new cookie.  */
static tree nc_nelts_field_id;
#if 0
static tree nc_ptr_2comp_field_id;
#endif

static tree minus_one;

/* Set up local variable for this file.  MUST BE CALLED AFTER
   INIT_DECL_PROCESSING.  */

tree BI_header_type, BI_header_size;

void init_init_processing ()
{
  tree fields[1];

  /* Define implicit `operator new' and `operator delete' functions.  */
  BIN = default_conversion (get_first_fn (IDENTIFIER_GLOBAL_VALUE (ansi_opname[(int) NEW_EXPR])));
  TREE_USED (TREE_OPERAND (BIN, 0)) = 0;
  BID = default_conversion (get_first_fn (IDENTIFIER_GLOBAL_VALUE (ansi_opname[(int) DELETE_EXPR])));
  TREE_USED (TREE_OPERAND (BID, 0)) = 0;
  BIVN = default_conversion (get_first_fn (IDENTIFIER_GLOBAL_VALUE (ansi_opname[(int) VEC_NEW_EXPR])));
  TREE_USED (TREE_OPERAND (BIVN, 0)) = 0;
  BIVD = default_conversion (get_first_fn (IDENTIFIER_GLOBAL_VALUE (ansi_opname[(int) VEC_DELETE_EXPR])));
  TREE_USED (TREE_OPERAND (BIVD, 0)) = 0;
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
      int is_not_base_vtable =
	i != CLASSTYPE_VFIELD_PARENT (BINFO_TYPE (real_binfo));
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
perform_member_init (member, name, init, explicit, protect_list)
     tree member, name, init, *protect_list;
     int explicit;
{
  tree decl;
  tree type = TREE_TYPE (member);

  if (TYPE_NEEDS_CONSTRUCTING (type)
      || (init && TYPE_HAS_CONSTRUCTOR (type)))
    {
      /* Since `init' is already a TREE_LIST on the current_member_init_list,
	 only build it into one if we aren't already a list.  */
      if (init != NULL_TREE && TREE_CODE (init) != TREE_LIST)
	init = build_tree_list (NULL_TREE, init);

      decl = build_component_ref (C_C_D, name, 0, explicit);

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
	expand_aggr_init (decl, init, 0, 0);
    }
  else
    {
      if (init == NULL_TREE)
	{
	  if (explicit)
	    {
	      cp_error ("incomplete initializer for member `%D' of class `%T' which has no constructor",
			member, current_class_type);
	      init = error_mark_node;
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
	  decl = build_component_ref (C_C_D, name, 0, explicit);
	  expand_expr_stmt (build_modify_expr (decl, INIT_EXPR, init));
	}
    }
  expand_cleanups_to (NULL_TREE);

  if (TYPE_NEEDS_DESTRUCTOR (type))
    {
      tree expr = build_component_ref (C_C_D, name, 0, explicit);
      expr = build_delete (type, expr, integer_zero_node,
			   LOOKUP_NONVIRTUAL|LOOKUP_DESTRUCTOR, 0);

      if (expr != error_mark_node)
	{
	  start_protect ();
	  *protect_list = tree_cons (NULL_TREE, expr, *protect_list);
	}
    }
}

extern int warn_reorder;

/* Subroutine of emit_member_init.  */
static tree
sort_member_init (t)
     tree t;
{
  tree x, member, name, field, init;
  tree init_list = NULL_TREE;
  tree fields_to_unmark = NULL_TREE;
  int last_pos = 0;
  tree last_field;

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
      tree basename = TREE_PURPOSE (x);
      tree binfo;

      if (basename == NULL_TREE)
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
      else if (is_aggr_typedef (basename, 1))
	{
	  binfo = binfo_or_else (IDENTIFIER_TYPE_VALUE (basename), t);
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
			    IDENTIFIER_TYPE_VALUE (basename),
			    current_class_type);
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

/* Perform partial cleanups for a base for exception handling.  */
static tree
build_partial_cleanup_for (binfo)
     tree binfo;
{
  tree expr = convert_pointer_to_real (binfo,
				       build_unary_op (ADDR_EXPR, C_C_D, 0));

  return build_delete (TREE_TYPE (expr),
		       expr,
		       integer_zero_node,
		       LOOKUP_NONVIRTUAL|LOOKUP_DESTRUCTOR, 0);
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
  extern tree in_charge_identifier;

  tree member, x;
  tree mem_init_list;
  tree rbase_init_list, vbase_init_list;
  tree t_binfo = TYPE_BINFO (t);
  tree binfos = BINFO_BASETYPES (t_binfo);
  int i, n_baseclasses = binfos ? TREE_VEC_LENGTH (binfos) : 0;
  tree expr = NULL_TREE;

  my_friendly_assert (protect_list == NULL_TREE, 999);

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

  if (TYPE_USES_VIRTUAL_BASECLASSES (t))
    {
      tree first_arg = TREE_CHAIN (DECL_ARGUMENTS (current_function_decl));

      expand_start_cond (first_arg, 0);
      expand_aggr_vbase_init (t_binfo, C_C_D, current_class_decl,
			      vbase_init_list);
      expand_end_cond ();
    }

  /* Now, perform initialization of non-virtual base classes.  */
  for (i = 0; i < n_baseclasses; i++)
    {
      tree base = current_class_decl;
      tree base_binfo = TREE_VEC_ELT (binfos, i);
      tree init = void_list_node;

      if (TREE_VIA_VIRTUAL (base_binfo))
	continue;

#if 0 /* Once unsharing happens soon enough.  */
      my_friendly_assert (BINFO_INHERITANCE_CHAIN (base_binfo) == t_binfo);
#else
      BINFO_INHERITANCE_CHAIN (base_binfo) = t_binfo;
#endif

      if (TREE_PURPOSE (rbase_init_list))
	init = TREE_VALUE (rbase_init_list);
      else if (TYPE_NEEDS_CONSTRUCTING (BINFO_TYPE (base_binfo)))
	init = NULL_TREE;

      if (init != void_list_node)
	{
	  member = convert_pointer_to_real (base_binfo, current_class_decl);
	  expand_aggr_init_1 (base_binfo, 0,
			      build_indirect_ref (member, NULL_PTR), init,
			      BINFO_OFFSET_ZEROP (base_binfo), LOOKUP_NORMAL);
	  expand_cleanups_to (NULL_TREE);
	}

      if (TYPE_NEEDS_DESTRUCTOR (BINFO_TYPE (base_binfo)))
	{
	  start_protect ();
	  protect_list = tree_cons (NULL_TREE,
				    build_partial_cleanup_for (base_binfo),
				    protect_list);
	}

      rbase_init_list = TREE_CHAIN (rbase_init_list);
    }

  /* Initialize all the virtual function table fields that
     do come from virtual base classes. */
  if (TYPE_USES_VIRTUAL_BASECLASSES (t))
    expand_indirect_vtbls_init (t_binfo, C_C_D, current_class_decl, 0);

  /* Initialize all the virtual function table fields that
     do not come from virtual base classes.  */
  expand_direct_vtbls_init (t_binfo, t_binfo, 1, 1, current_class_decl);

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

	  /* Also see if it's ever a COMPONENT_REF here.  If it is, we
	     need to do `expand_assignment (name, init, 0, 0);' and
	     a continue.  */
	  my_friendly_assert (TREE_CODE (name) != COMPONENT_REF, 349);
	}
      else
	{
	  name = DECL_NAME (member);
	  init = DECL_INITIAL (member);

	  from_init_list = 0;
	}

      perform_member_init (member, name, init, from_init_list, &protect_list);
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

	  perform_member_init (field, name, init, 1, &protect_list);
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
  expand_aggr_init_1 (binfo, exp, ref, init, 0, LOOKUP_COMPLAIN);
  expand_cleanups_to (NULL_TREE);
}

/* Initialize this object's virtual base class pointers.  This must be
   done only at the top-level of the object being constructed.

   INIT_LIST is list of initialization for constructor to perform.  */
static void
expand_aggr_vbase_init (binfo, exp, addr, init_list)
     tree binfo;
     tree exp;
     tree addr;
     tree init_list;
{
  tree type = BINFO_TYPE (binfo);

  if (TYPE_USES_VIRTUAL_BASECLASSES (type))
    {
      tree result = init_vbase_pointers (type, addr);
      tree vbases;

      if (result)
	expand_expr_stmt (build_compound_expr (result));

      for (vbases = CLASSTYPE_VBASECLASSES (type); vbases;
	   vbases = TREE_CHAIN (vbases))
	{
	  tree tmp = purpose_member (vbases, result);
	  expand_aggr_vbase_init_1 (vbases, exp,
				    TREE_OPERAND (TREE_VALUE (tmp), 0),
				    init_list);
	}
    }
}

/* Subroutine to perform parser actions for member initialization.
   S_ID is the scoped identifier.
   NAME is the name of the member.
   INIT is the initializer, or `void_type_node' if none.  */
void
do_member_init (s_id, name, init)
     tree s_id, name, init;
{
  tree binfo, base;

  if (current_class_type == NULL_TREE
      || ! is_aggr_typedef (s_id, 1))
    return;
  binfo = get_binfo (IDENTIFIER_TYPE_VALUE (s_id),
			  current_class_type, 1);
  if (binfo == error_mark_node)
    return;
  if (binfo == 0)
    {
      error_not_base_type (IDENTIFIER_TYPE_VALUE (s_id), current_class_type);
      return;
    }

  base = convert_pointer_to (binfo, current_class_decl);
  expand_member_init (build_indirect_ref (base, NULL_PTR), name, init);
}

/* Function to give error message if member initialization specification
   is erroneous.  FIELD is the member we decided to initialize.
   TYPE is the type for which the initialization is being performed.
   FIELD must be a member of TYPE, or the base type from which FIELD
   comes must not need a constructor.
   
   MEMBER_NAME is the name of the member.  */

static int
member_init_ok_or_else (field, type, member_name)
     tree field;
     tree type;
     char *member_name;
{
  if (field == error_mark_node)
    return 0;
  if (field == NULL_TREE)
    {
      cp_error ("class `%T' does not have any field named `%s'", type,
		member_name);
      return 0;
    }
  if (DECL_CONTEXT (field) != type
      && TYPE_NEEDS_CONSTRUCTING (DECL_CONTEXT (field)))
    {
      if (current_function_decl && DECL_CONSTRUCTOR_P (current_function_decl))
	cp_error ("initialization of `%D' inside constructor for `%T'",
		  field, type);
      else
	cp_error ("member `%D' comes from base class needing constructor",
		  field);
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
  extern tree ptr_type_node;	/* should be in tree.h */

  tree basetype = NULL_TREE, field;
  tree parm;
  tree rval, type;
  tree actual_name;

  if (exp == NULL_TREE)
    return;			/* complain about this later */

  type = TYPE_MAIN_VARIANT (TREE_TYPE (exp));

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

  if (init)
    {
      /* The grammar should not allow fields which have names
	 that are TYPENAMEs.  Therefore, if the field has
	 a non-NULL TREE_TYPE, we may assume that this is an
	 attempt to initialize a base class member of the current
	 type.  Otherwise, it is an attempt to initialize a
	 member field.  */

      if (init == void_type_node)
	init = NULL_TREE;

      if (name == NULL_TREE || IDENTIFIER_HAS_TYPE_VALUE (name))
	{
	  tree base_init;

	  if (name == NULL_TREE)
	    {
/*
	      if (basetype)
		name = TYPE_IDENTIFIER (basetype);
	      else
		{
		  error ("no base class to initialize");
		  return;
		}
*/
	    }
	  else
	    {
	      basetype = IDENTIFIER_TYPE_VALUE (name);
	      if (basetype != type
		  && ! binfo_member (basetype, TYPE_BINFO (type))
		  && ! binfo_member (basetype, CLASSTYPE_VBASECLASSES (type)))
		{
		  if (IDENTIFIER_CLASS_VALUE (name))
		    goto try_member;
		  if (TYPE_USES_VIRTUAL_BASECLASSES (type))
		    error ("type `%s' is not an immediate or virtual basetype for `%s'",
			   IDENTIFIER_POINTER (name),
			   TYPE_NAME_STRING (type));
		  else
		    error ("type `%s' is not an immediate basetype for `%s'",
			   IDENTIFIER_POINTER (name),
			   TYPE_NAME_STRING (type));
		  return;
		}
	    }

	  if (purpose_member (name, current_base_init_list))
	    {
	      error ("base class `%s' already initialized",
		     IDENTIFIER_POINTER (name));
	      return;
	    }

	  base_init = build_tree_list (name, init);
	  TREE_TYPE (base_init) = basetype;
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
	      error ("field `%s' already initialized", IDENTIFIER_POINTER (name));
	      return;
	    }

	  member_init = build_tree_list (name, init);
	  TREE_TYPE (member_init) = TREE_TYPE (field);
	  current_member_init_list = chainon (current_member_init_list, member_init);
	}
      return;
    }
  else if (name == NULL_TREE)
    {
      compiler_error ("expand_member_init: name == NULL_TREE");
      return;
    }

  basetype = type;
  field = lookup_field (basetype, name, 0, 0);

  if (! member_init_ok_or_else (field, basetype, IDENTIFIER_POINTER (name)))
    return;

  /* now see if there is a constructor for this type
     which will take these args. */

  if (TYPE_HAS_CONSTRUCTOR (TREE_TYPE (field)))
    {
      tree parmtypes, fndecl;

      if (TREE_CODE (exp) == VAR_DECL || TREE_CODE (exp) == PARM_DECL)
	{
	  /* just know that we've seen something for this node */
	  DECL_INITIAL (exp) = error_mark_node;
	  TREE_USED (exp) = 1;
	}
      type = TYPE_MAIN_VARIANT (TREE_TYPE (field));
      actual_name = TYPE_IDENTIFIER (type);
      parm = build_component_ref (exp, name, 0, 0);

      /* Now get to the constructor.  */
      fndecl = TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (type), 0);
      /* Get past destructor, if any.  */
      if (TYPE_HAS_DESTRUCTOR (type))
	fndecl = DECL_CHAIN (fndecl);

      if (fndecl)
	my_friendly_assert (TREE_CODE (fndecl) == FUNCTION_DECL, 209);

      /* If the field is unique, we can use the parameter
	 types to guide possible type instantiation.  */
      if (DECL_CHAIN (fndecl) == NULL_TREE)
	{
	  /* There was a confusion here between
	     FIELD and FNDECL.  The following code
	     should be correct, but abort is here
	     to make sure.  */
	  my_friendly_abort (48);
	  parmtypes = FUNCTION_ARG_CHAIN (fndecl);
	}
      else
	{
	  parmtypes = NULL_TREE;
	  fndecl = NULL_TREE;
	}

      init = convert_arguments (parm, parmtypes, NULL_TREE, fndecl, LOOKUP_NORMAL);
      if (init == NULL_TREE || TREE_TYPE (init) != error_mark_node)
	rval = build_method_call (NULL_TREE, actual_name, init, NULL_TREE, LOOKUP_NORMAL);
      else
	return;

      if (rval != error_mark_node)
	{
	  /* Now, fill in the first parm with our guy */
	  TREE_VALUE (TREE_OPERAND (rval, 1))
	    = build_unary_op (ADDR_EXPR, parm, 0);
	  TREE_TYPE (rval) = ptr_type_node;
	  TREE_SIDE_EFFECTS (rval) = 1;
	}
    }
  else if (TYPE_NEEDS_CONSTRUCTING (TREE_TYPE (field)))
    {
      parm = build_component_ref (exp, name, 0, 0);
      expand_aggr_init (parm, NULL_TREE, 0, 0);
      rval = error_mark_node;
    }

  /* Now initialize the member.  It does not have to
     be of aggregate type to receive initialization.  */
  if (rval != error_mark_node)
    expand_expr_stmt (rval);
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
   essentially an alias for C_C_D.  In this case, the base constructor
   may move it on us, and we must keep track of such deviations.

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
   perform the initialization, but not both, as it would be ambiguous.
   */

void
expand_aggr_init (exp, init, alias_this, flags)
     tree exp, init;
     int alias_this;
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
      if (TYPE_READONLY (TREE_TYPE (type)) || TYPE_VOLATILE (TREE_TYPE (type)))
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
		       init && comptypes (TREE_TYPE (init), TREE_TYPE (exp), 1));
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
		      init, alias_this, LOOKUP_NORMAL|flags);
  TREE_TYPE (exp) = type;
  TREE_READONLY (exp) = was_const;
  TREE_THIS_VOLATILE (exp) = was_volatile;
}

static void
expand_default_init (binfo, true_exp, exp, type, init, alias_this, flags)
     tree binfo;
     tree true_exp, exp;
     tree type;
     tree init;
     int alias_this;
     int flags;
{
  /* It fails because there may not be a constructor which takes
     its own type as the first (or only parameter), but which does
     take other types via a conversion.  So, if the thing initializing
     the expression is a unit element of type X, first try X(X&),
     followed by initialization by X.  If neither of these work
     out, then look hard.  */
  tree rval;
  tree parms;

  if (init == NULL_TREE
      || (TREE_CODE (init) == TREE_LIST && ! TREE_TYPE (init)))
    {
      parms = init;
      if (parms)
	init = TREE_VALUE (parms);
    }
  else if (TREE_CODE (init) == INDIRECT_REF && TREE_HAS_CONSTRUCTOR (init)
	   && TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (TREE_TYPE (init)))
    {
      rval = convert_for_initialization (exp, type, init, 0, 0, 0, 0);
      TREE_USED (rval) = 1;
      expand_expr_stmt (rval);
      return;
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

  if (init && TREE_CHAIN (parms) == NULL_TREE
      && TYPE_HAS_TRIVIAL_INIT_REF (type)
      && TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (TREE_TYPE (init)))
    {
      rval = build (INIT_EXPR, type, exp, init);
      TREE_SIDE_EFFECTS (rval) = 1;
      expand_expr_stmt (rval);
    }
  else
    {
      if (flags & LOOKUP_ONLYCONVERTING)
	flags |= LOOKUP_NO_CONVERSION;
      rval = build_method_call (exp, constructor_name_full (type),
				parms, binfo, flags);

      /* Private, protected, or otherwise unavailable.  */
      if (rval == error_mark_node)
	{
	  if (flags & LOOKUP_COMPLAIN)
	    cp_error ("in base initialization for %sclass `%T'",
		      TREE_VIA_VIRTUAL (binfo) ? "virtual base " : "",
		      binfo);
	}
      else if (rval == NULL_TREE)
	my_friendly_abort (361);
      else
	{
	  /* p. 222: if the base class assigns to `this', then that
	     value is used in the derived class.  */
	  if ((flag_this_is_variable & 1) && alias_this)
	    {
	      TREE_TYPE (rval) = TREE_TYPE (current_class_decl);
	      expand_assignment (current_class_decl, rval, 0, 0);
	    }
	  else
	    expand_expr_stmt (rval);
	}
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

   ALIAS_THIS serves the same purpose it serves for expand_aggr_init.

   FLAGS is just passes to `build_method_call'.  See that function for
   its description.  */

static void
expand_aggr_init_1 (binfo, true_exp, exp, init, alias_this, flags)
     tree binfo;
     tree true_exp, exp;
     tree init;
     int alias_this;
     int flags;
{
  tree type = TREE_TYPE (exp);
  tree init_type = NULL_TREE;

  my_friendly_assert (init != error_mark_node && type != error_mark_node, 211);

  /* Use a function returning the desired type to initialize EXP for us.
     If the function is a constructor, and its first argument is
     NULL_TREE, know that it was meant for us--just slide exp on
     in and expand the constructor.  Constructors now come
     as TARGET_EXPRs.  */
  if (init)
    {
      tree init_list = NULL_TREE;

      if (TREE_CODE (init) == TREE_LIST)
	{
	  init_list = init;
	  if (TREE_CHAIN (init) == NULL_TREE)
	    init = TREE_VALUE (init);
	}

      init_type = TREE_TYPE (init);

      if (TREE_CODE (init) != TREE_LIST)
	{
	  if (TREE_CODE (init_type) == ERROR_MARK)
	    return;

#if 0
	  /* These lines are found troublesome 5/11/89.  */
	  if (TREE_CODE (init_type) == REFERENCE_TYPE)
	    init_type = TREE_TYPE (init_type);
#endif

	  /* This happens when we use C++'s functional cast notation.
	     If the types match, then just use the TARGET_EXPR
	     directly.  Otherwise, we need to create the initializer
	     separately from the object being initialized.  */
	  if (TREE_CODE (init) == TARGET_EXPR)
	    {
	      if (TYPE_MAIN_VARIANT (init_type) == TYPE_MAIN_VARIANT (type))
		{
		  if (TREE_CODE (exp) == VAR_DECL
		      || TREE_CODE (exp) == RESULT_DECL)
		    /* Unify the initialization targets.  */
		    DECL_RTL (TREE_OPERAND (init, 0)) = DECL_RTL (exp);
		  else
		    DECL_RTL (TREE_OPERAND (init, 0)) = expand_expr (exp, NULL_RTX, 0, 0);

		  expand_expr_stmt (init);
		  return;
		}
	      else
		{
		  init = TREE_OPERAND (init, 1);
		  init = build (CALL_EXPR, init_type,
				TREE_OPERAND (init, 0), TREE_OPERAND (init, 1), 0);
		  TREE_SIDE_EFFECTS (init) = 1;
		    if (init_list)
		      TREE_VALUE (init_list) = init;
		}
	    }

	  if (init_type == type && TREE_CODE (init) == CALL_EXPR
#if 0
	      /* It is valid to directly initialize from a CALL_EXPR
		 without going through X(X&), apparently.  */
	      && ! TYPE_GETS_INIT_REF (type)
#endif
	      )
	    {
	      /* A CALL_EXPR is a legitimate form of initialization, so
		 we should not print this warning message.  */
#if 0
	      /* Should have gone away due to 5/11/89 change.  */
	      if (TREE_CODE (TREE_TYPE (init)) == REFERENCE_TYPE)
		init = convert_from_reference (init);
#endif
	      expand_assignment (exp, init, 0, 0);
	      if (exp == DECL_RESULT (current_function_decl))
		{
		  /* Failing this assertion means that the return value
		     from receives multiple initializations.  */
		  my_friendly_assert (DECL_INITIAL (exp) == NULL_TREE
				      || DECL_INITIAL (exp) == error_mark_node,
				      212);
		  DECL_INITIAL (exp) = init;
		}
	      return;
	    }
	  else if (init_type == type
		   && TREE_CODE (init) == COND_EXPR)
	    {
	      /* Push value to be initialized into the cond, where possible.
	         Avoid spurious warning messages when initializing the
		 result of this function.  */
	      TREE_OPERAND (init, 1)
		= build_modify_expr (exp, INIT_EXPR, TREE_OPERAND (init, 1));
	      if (exp == DECL_RESULT (current_function_decl))
		DECL_INITIAL (exp) = NULL_TREE;
	      TREE_OPERAND (init, 2)
		= build_modify_expr (exp, INIT_EXPR, TREE_OPERAND (init, 2));
	      if (exp == DECL_RESULT (current_function_decl))
		DECL_INITIAL (exp) = init;
	      TREE_SIDE_EFFECTS (init) = 1;
	      expand_expr (init, const0_rtx, VOIDmode, 0);
	      free_temp_slots ();
	      return;
	    }
	}

      /* We did not know what we were initializing before.  Now we do.  */
      if (TREE_CODE (init) == TARGET_EXPR)
	{
	  tree tmp = TREE_OPERAND (TREE_OPERAND (init, 1), 1);

	  if (TREE_CODE (TREE_VALUE (tmp)) == NOP_EXPR
	      && TREE_OPERAND (TREE_VALUE (tmp), 0) == integer_zero_node)
	    {
	      /* In order for this to work for RESULT_DECLs, if their
		 type has a constructor, then they must be BLKmode
		 so that they will be meaningfully addressable.  */
	      tree arg = build_unary_op (ADDR_EXPR, exp, 0);
	      init = TREE_OPERAND (init, 1);
	      init = build (CALL_EXPR, build_pointer_type (TREE_TYPE (init)),
			    TREE_OPERAND (init, 0), TREE_OPERAND (init, 1), 0);
	      TREE_SIDE_EFFECTS (init) = 1;
	      TREE_VALUE (TREE_OPERAND (init, 1))
		= convert_pointer_to (TREE_TYPE (TREE_TYPE (TREE_VALUE (tmp))), arg);

	      if (alias_this)
		{
		  expand_assignment (current_function_decl, init, 0, 0);
		  return;
		}
	      if (exp == DECL_RESULT (current_function_decl))
		{
		  if (DECL_INITIAL (DECL_RESULT (current_function_decl)))
		    fatal ("return value from function receives multiple initializations");
		  DECL_INITIAL (exp) = init;
		}
	      expand_expr_stmt (init);
	      return;
	    }
	}

      if (TREE_CODE (exp) == VAR_DECL
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

      /* Handle this case: when calling a constructor: xyzzy foo(bar);
	 which really means:  xyzzy foo = bar; Ugh!

	 More useful for this case: xyzzy *foo = new xyzzy (bar);  */

      if (! TYPE_NEEDS_CONSTRUCTING (type) && ! IS_AGGR_TYPE (type))
	{
	  if (init_list && TREE_CHAIN (init_list))
	    {
	      warning ("initializer list being treated as compound expression");
	      init = convert (type, build_compound_expr (init_list));
	      if (init == error_mark_node)
		return;
	    }

	  expand_assignment (exp, init, 0, 0);

	  return;
	}
      /* See whether we can go through a type conversion operator.
	 This wins over going through a non-existent constructor.  If
	 there is a constructor, it is ambiguous.  */
      if (TREE_CODE (init) != TREE_LIST)
	{
	  tree ttype = TREE_CODE (init_type) == REFERENCE_TYPE
	    ? TREE_TYPE (init_type) : init_type;

	  if (ttype != type && IS_AGGR_TYPE (ttype))
	    {
	      tree rval = build_type_conversion (CONVERT_EXPR, type, init, 0);

	      if (rval)
		{
		  /* See if there is a constructor for``type'' that takes a
		     ``ttype''-typed object. */
		  tree parms = build_tree_list (NULL_TREE, init);
		  tree as_cons = NULL_TREE;
		  if (TYPE_HAS_CONSTRUCTOR (type))
		    as_cons = build_method_call (exp, constructor_name_full (type),
						 parms, binfo,
						 LOOKUP_SPECULATIVELY|LOOKUP_NO_CONVERSION);
		  if (as_cons != NULL_TREE && as_cons != error_mark_node)
		    /* ANSI C++ June 5 1992 WP 12.3.2.6.1 */
		    cp_error ("ambiguity between conversion to `%T' and constructor",
			      type);
		  else
		    expand_assignment (exp, rval, 0, 0);
		  return;
		}
	    }
	}
    }

  /* Handle default copy constructors here, does not matter if there is
     a constructor or not.  */
  if (type == init_type && IS_AGGR_TYPE (type)
      && init && TREE_CODE (init) != TREE_LIST)
    expand_default_init (binfo, true_exp, exp, type, init, alias_this, flags);
  /* Not sure why this is here... */
  else if (TYPE_HAS_CONSTRUCTOR (type))
    expand_default_init (binfo, true_exp, exp, type, init, alias_this, flags);
  else if (TREE_CODE (type) == ARRAY_TYPE)
    {
      if (TYPE_NEEDS_CONSTRUCTING (TREE_TYPE (type)))
	expand_vec_init (exp, exp, array_type_nelts (type), init, 0);
      else if (TYPE_VIRTUAL_P (TREE_TYPE (type)))
	sorry ("arrays of objects with virtual functions but no constructors");
    }
  else
    expand_recursive_init (binfo, true_exp, exp, init,
			   CLASSTYPE_BASE_INIT_LIST (type), alias_this);
}

/* A pointer which holds the initializer.  First call to
   expand_aggr_init gets this value pointed to, and sets it to init_null.  */
static tree *init_ptr, init_null;

/* Subroutine of expand_recursive_init:

   ADDR is the address of the expression being initialized.
   INIT_LIST is the cons-list of initializations to be performed.
   ALIAS_THIS is its same, lovable self.  */
static void
expand_recursive_init_1 (binfo, true_exp, addr, init_list, alias_this)
     tree binfo, true_exp, addr;
     tree init_list;
     int alias_this;
{
  while (init_list)
    {
      if (TREE_PURPOSE (init_list))
	{
	  if (TREE_CODE (TREE_PURPOSE (init_list)) == FIELD_DECL)
	    {
	      tree member = TREE_PURPOSE (init_list);
	      tree subexp = build_indirect_ref (convert_pointer_to (TREE_VALUE (init_list), addr), NULL_PTR);
	      tree member_base = build (COMPONENT_REF, TREE_TYPE (member), subexp, member);
	      if (IS_AGGR_TYPE (TREE_TYPE (member)))
		expand_aggr_init (member_base, DECL_INITIAL (member), 0, 0);
	      else if (TREE_CODE (TREE_TYPE (member)) == ARRAY_TYPE
		       && TYPE_NEEDS_CONSTRUCTING (TREE_TYPE (member)))
		{
		  member_base = save_expr (default_conversion (member_base));
		  expand_vec_init (member, member_base,
				   array_type_nelts (TREE_TYPE (member)),
				   DECL_INITIAL (member), 0);
		}
	      else
		expand_expr_stmt (build_modify_expr (member_base, INIT_EXPR, DECL_INITIAL (member)));
	    }
	  else if (TREE_CODE (TREE_PURPOSE (init_list)) == TREE_LIST)
	    {
	      expand_recursive_init_1 (binfo, true_exp, addr, TREE_PURPOSE (init_list), alias_this);
	      expand_recursive_init_1 (binfo, true_exp, addr, TREE_VALUE (init_list), alias_this);
	    }
	  else if (TREE_CODE (TREE_PURPOSE (init_list)) == ERROR_MARK)
	    {
	      /* Only initialize the virtual function tables if we
		 are initializing the ultimate users of those vtables.  */
	      if (TREE_VALUE (init_list))
		{
		  /* We have to ensure that the first argment to
		     expand_virtual_init is in binfo's hierarchy.  */
		  /* Is it the case that this is exactly the right binfo? */
		  /* If it is ok, then fixup expand_virtual_init, to make
		     it much simpler. */
		  expand_virtual_init (get_binfo (TREE_VALUE (init_list), binfo, 0),
				      addr);
		  if (TREE_VALUE (init_list) == binfo
		      && TYPE_USES_VIRTUAL_BASECLASSES (BINFO_TYPE (binfo)))
		    expand_indirect_vtbls_init (binfo, true_exp, addr, 1);
		}
	    }
	  else
	    my_friendly_abort (49);
	}
      else if (TREE_VALUE (init_list)
	       && TREE_CODE (TREE_VALUE (init_list)) == TREE_VEC)
	{
	  tree subexp = build_indirect_ref (convert_pointer_to (TREE_VALUE (init_list), addr), NULL_PTR);
	  expand_aggr_init_1 (binfo, true_exp, subexp, *init_ptr,
			      alias_this && BINFO_OFFSET_ZEROP (TREE_VALUE (init_list)),
			      LOOKUP_COMPLAIN);

	  /* INIT_PTR is used up.  */
	  init_ptr = &init_null;
	}
      else
	my_friendly_abort (50);
      init_list = TREE_CHAIN (init_list);
    }
}

/* Initialize EXP with INIT.  Type EXP does not have a constructor,
   but it has a baseclass with a constructor or a virtual function
   table which needs initializing.

   INIT_LIST is a cons-list describing what parts of EXP actually
   need to be initialized.  INIT is given to the *unique*, first
   constructor within INIT_LIST.  If there are multiple first
   constructors, such as with multiple inheritance, INIT must
   be zero or an ambiguity error is reported.

   ALIAS_THIS is passed from `expand_aggr_init'.  See comments
   there.  */

static void
expand_recursive_init (binfo, true_exp, exp, init, init_list, alias_this)
     tree binfo, true_exp, exp, init;
     tree init_list;
     int alias_this;
{
  tree *old_init_ptr = init_ptr;
  tree addr = build_unary_op (ADDR_EXPR, exp, 0);
  init_ptr = &init;

  if (true_exp == exp && TYPE_USES_VIRTUAL_BASECLASSES (BINFO_TYPE (binfo)))
    {
      expand_aggr_vbase_init (binfo, exp, addr, init_list);
      expand_indirect_vtbls_init (binfo, true_exp, addr, 1);
    }
  expand_recursive_init_1 (binfo, true_exp, addr, init_list, alias_this);

  if (*init_ptr)
    {
      tree type = TREE_TYPE (exp);

      if (TREE_CODE (type) == REFERENCE_TYPE)
	type = TREE_TYPE (type);
      if (IS_AGGR_TYPE (type))
	cp_error ("unexpected argument to constructor `%T'", type);
      else
	error ("unexpected argument to constructor");
    }
  init_ptr = old_init_ptr;
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
      && TREE_CODE (type) != TEMPLATE_TYPE_PARM)
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
      && TREE_CODE (type) != TEMPLATE_TYPE_PARM)
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

/* For an expression of the form CNAME :: NAME (PARMLIST), build
   the appropriate function call.  */
tree
build_member_call (cname, name, parmlist)
     tree cname, name, parmlist;
{
  tree type, t;
  tree method_name = name;
  int dtor = 0;
  int dont_use_this = 0;
  tree basetype_path, decl;

  if (TREE_CODE (method_name) == BIT_NOT_EXPR)
    {
      method_name = TREE_OPERAND (method_name, 0);
      dtor = 1;
    }

  if (TREE_CODE (cname) == SCOPE_REF)
    cname = resolve_scope_to_name (NULL_TREE, cname);

  /* This shouldn't be here, and build_member_call shouldn't appear in
     parse.y!  (mrs)  */
  if (cname && get_aggr_from_typedef (cname, 0) == 0
      && TREE_CODE (cname) == IDENTIFIER_NODE)
    {
      tree ns = lookup_name (cname, 0);
      if (ns && TREE_CODE (ns) == NAMESPACE_DECL)
	{
	  return build_x_function_call (build_offset_ref (cname, name), parmlist, current_class_decl);
	}
    }

  if (cname == NULL_TREE || ! (type = get_aggr_from_typedef (cname, 1)))
    return error_mark_node;

  /* An operator we did not like.  */
  if (name == NULL_TREE)
    return error_mark_node;

  if (dtor)
    {
#if 0
      /* Everything can explicitly call a destructor; see 12.4 */
      if (! TYPE_HAS_DESTRUCTOR (type))
	cp_error ("type `%#T' does not have a destructor", type);
      else
#endif
      cp_error ("cannot call destructor `%T::~%T' without object", type,
		method_name);
      return error_mark_node;
    }

  /* No object?  Then just fake one up, and let build_method_call
     figure out what to do.  */
  if (current_class_type == 0
      || get_base_distance (type, current_class_type, 0, &basetype_path) == -1)
    dont_use_this = 1;

  if (dont_use_this)
    {
      basetype_path = TYPE_BINFO (type);
      decl = build1 (NOP_EXPR, build_pointer_type (type), error_mark_node);
    }
  else if (current_class_decl == 0)
    {
      dont_use_this = 1;
      decl = build1 (NOP_EXPR, build_pointer_type (type), error_mark_node);
    }
  else
    {
      tree olddecl = current_class_decl;
      tree oldtype = TREE_TYPE (TREE_TYPE (olddecl));
      if (oldtype != type)
	{
	  tree newtype = build_type_variant (type, TYPE_READONLY (oldtype),
					     TYPE_VOLATILE (oldtype));
	  decl = convert_force (build_pointer_type (newtype), olddecl, 0);
	}
      else
	decl = olddecl;
    }

  decl = build_indirect_ref (decl, NULL_PTR);

  if (method_name == constructor_name (type)
      || method_name == constructor_name_full (type))
    return build_functional_cast (type, parmlist);
  if (t = lookup_fnfields (basetype_path, method_name, 0))
    return build_method_call (decl, method_name, parmlist, basetype_path,
			      LOOKUP_NORMAL|LOOKUP_NONVIRTUAL);
  if (TREE_CODE (name) == IDENTIFIER_NODE
      && ((t = lookup_field (TYPE_BINFO (type), name, 1, 0))))
    {
      if (t == error_mark_node)
	return error_mark_node;
      if (TREE_CODE (t) == FIELD_DECL)
	{
	  if (dont_use_this)
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
      if (TYPE_LANG_SPECIFIC (TREE_TYPE (decl))
	  && TYPE_OVERLOADS_CALL_EXPR (TREE_TYPE (decl)))
	return build_opfncall (CALL_EXPR, LOOKUP_NORMAL, decl, parmlist, NULL_TREE);
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
   and then act as a pointer to member, for example CNAME :: FIELD
   can have its address taken by saying & CNAME :: FIELD.

   @@ Prints out lousy diagnostics for operator <typename>
   @@ fields.

   @@ This function should be rewritten and placed in search.c.  */
tree
build_offset_ref (cname, name)
     tree cname, name;
{
  tree decl, type, fnfields, fields, t = error_mark_node;
  tree basetypes = NULL_TREE;
  int dtor = 0;

  if (TREE_CODE (cname) == SCOPE_REF)
    cname = resolve_scope_to_name (NULL_TREE, cname);

  /* Handle namespace names fully here.  */
  if (TREE_CODE (cname) == IDENTIFIER_NODE
      && get_aggr_from_typedef (cname, 0) == 0)
    {
      tree ns = lookup_name (cname, 0);
      tree val;
      if (ns && TREE_CODE (ns) == NAMESPACE_DECL)
	{
	  val = lookup_namespace_name (ns, name);
	  if (val)
	    return val;
	  cp_error ("namespace `%D' has no member named `%D'", ns, name);
	  return error_mark_node;
	}
    }

  if (cname == NULL_TREE || ! is_aggr_typedef (cname, 1))
    return error_mark_node;

  type = IDENTIFIER_TYPE_VALUE (cname);

  if (TREE_CODE (name) == BIT_NOT_EXPR)
    {
      dtor = 1;
      name = TREE_OPERAND (name, 0);
    }

  if (TYPE_SIZE (type) == 0)
    {
      t = IDENTIFIER_CLASS_VALUE (name);
      if (t == 0)
	{
	  cp_error ("incomplete type `%T' does not have member `%D'", type,
		      name);
	  return error_mark_node;
	}
      if (TREE_CODE (t) == TYPE_DECL || TREE_CODE (t) == VAR_DECL
	  || TREE_CODE (t) == CONST_DECL)
	{
	  TREE_USED (t) = 1;
	  return t;
	}
      if (TREE_CODE (t) == FIELD_DECL)
	sorry ("use of member in incomplete aggregate type");
      else if (TREE_CODE (t) == FUNCTION_DECL)
	sorry ("use of member function in incomplete aggregate type");
      else
	my_friendly_abort (52);
      return error_mark_node;
    }

#if 0
  if (TREE_CODE (name) == TYPE_EXPR)
    /* Pass a TYPE_DECL to build_component_type_expr.  */
    return build_component_type_expr (TYPE_NAME (TREE_TYPE (cname)),
				      name, NULL_TREE, 1);
#endif

  if (current_class_type == 0
      || get_base_distance (type, current_class_type, 0, &basetypes) == -1)
    {
      basetypes = TYPE_BINFO (type);
      decl = build1 (NOP_EXPR,
		     IDENTIFIER_TYPE_VALUE (cname),
		     error_mark_node);
    }
  else if (current_class_decl == 0)
    decl = build1 (NOP_EXPR, IDENTIFIER_TYPE_VALUE (cname),
		   error_mark_node);
  else
    decl = C_C_D;

  fnfields = lookup_fnfields (basetypes, name, 1);
  fields = lookup_field (basetypes, name, 0, 0);

  if (fields == error_mark_node || fnfields == error_mark_node)
    return error_mark_node;

  /* A lot of this logic is now handled in lookup_field and
     lookup_fnfield. */
  if (fnfields)
    {
      basetypes = TREE_PURPOSE (fnfields);

      /* Go from the TREE_BASELINK to the member function info.  */
      t = TREE_VALUE (fnfields);

      if (fields)
	{
	  if (DECL_FIELD_CONTEXT (fields) == DECL_FIELD_CONTEXT (t))
	    {
	      error ("ambiguous member reference: member `%s' defined as both field and function",
		     IDENTIFIER_POINTER (name));
	      return error_mark_node;
	    }
	  if (UNIQUELY_DERIVED_FROM_P (DECL_FIELD_CONTEXT (fields), DECL_FIELD_CONTEXT (t)))
	    ;
	  else if (UNIQUELY_DERIVED_FROM_P (DECL_FIELD_CONTEXT (t), DECL_FIELD_CONTEXT (fields)))
	    t = fields;
	  else
	    {
	      error ("ambiguous member reference: member `%s' derives from distinct classes in multiple inheritance lattice");
	      return error_mark_node;
	    }
	}

      if (t == TREE_VALUE (fnfields))
	{
	  extern int flag_save_memoized_contexts;

	  if (DECL_CHAIN (t) == NULL_TREE || dtor)
	    {
	      enum access_type access;

	      /* unique functions are handled easily.  */
	    unique:
	      access = compute_access (basetypes, t);
	      if (access == access_protected)
		{
		  cp_error_at ("member function `%#D' is protected", t);
		  error ("in this context");
		  return error_mark_node;
		}
	      if (access == access_private)
		{
		  cp_error_at ("member function `%#D' is private", t);
		  error ("in this context");
		  return error_mark_node;
		}
	      assemble_external (t);
	      return build (OFFSET_REF, TREE_TYPE (t), decl, t);
	    }

	  /* overloaded functions may need more work.  */
	  if (cname == name)
	    {
	      if (TYPE_HAS_DESTRUCTOR (type)
		  && DECL_CHAIN (DECL_CHAIN (t)) == NULL_TREE)
		{
		  t = DECL_CHAIN (t);
		  goto unique;
		}
	    }
	  /* FNFIELDS is most likely allocated on the search_obstack,
	     which will go away after this class scope.  If we need
	     to save this value for later (either for memoization
	     or for use as an initializer for a static variable), then
	     do so here.

	     ??? The smart thing to do for the case of saving initializers
	     is to resolve them before we're done with this scope.  */
	  if (!TREE_PERMANENT (fnfields)
	      && ((flag_save_memoized_contexts && global_bindings_p ())
		  || ! allocation_temporary_p ()))
	    fnfields = copy_list (fnfields);

	  for (t = TREE_VALUE (fnfields); t; t = DECL_CHAIN (t))
	    assemble_external (t);

	  t = build_tree_list (error_mark_node, fnfields);
	  TREE_TYPE (t) = build_offset_type (type, unknown_type_node);
	  return t;
	}
    }

  /* Now that we know we are looking for a field, see if we
     have access to that field.  Lookup_field will give us the
     error message.  */

  t = lookup_field (basetypes, name, 1, 0);

  if (t == error_mark_node)
    return error_mark_node;

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
      assemble_external (t);
      TREE_USED (t) = 1;
      return t;
    }

  if (TREE_CODE (t) == FIELD_DECL && DECL_BIT_FIELD (t))
    {
      cp_error ("illegal pointer to bit field `%D'", t);
      return error_mark_node;
    }

  /* static class functions too.  */
  if (TREE_CODE (t) == FUNCTION_DECL && TREE_CODE (TREE_TYPE (t)) == FUNCTION_TYPE)
    my_friendly_abort (53);

  /* In member functions, the form `cname::name' is no longer
     equivalent to `this->cname::name'.  */
  return build (OFFSET_REF, build_offset_type (type, TREE_TYPE (t)), decl, t);
}

/* Given an object EXP and a member function reference MEMBER,
   return the address of the actual member function.  */
tree
get_member_function (exp_addr_ptr, exp, member)
     tree *exp_addr_ptr;
     tree exp, member;
{
  tree ctype = TREE_TYPE (exp);
  tree function = save_expr (build_unary_op (ADDR_EXPR, member, 0));

  if (TYPE_VIRTUAL_P (ctype)
      || (flag_all_virtual == 1 && TYPE_OVERLOADS_METHOD_CALL_EXPR (ctype)))
    {
      tree e0, e1, e3;
      tree exp_addr;

      /* Save away the unadulterated `this' pointer.  */
      exp_addr = save_expr (*exp_addr_ptr);

      /* Cast function to signed integer.  */
      e0 = build1 (NOP_EXPR, integer_type_node, function);

      /* There is a hack here that takes advantage of
	 twos complement arithmetic, and the fact that
	 there are more than one UNITS to the WORD.
	 If the high bit is set for the `function',
	 then we pretend it is a virtual function,
	 and the array indexing will knock this bit
	 out the top, leaving a valid index.  */
      if (UNITS_PER_WORD <= 1)
	my_friendly_abort (54);

      e1 = build (GT_EXPR, boolean_type_node, e0, integer_zero_node);
      e1 = build_compound_expr (tree_cons (NULL_TREE, exp_addr,
					   build_tree_list (NULL_TREE, e1)));
      e1 = save_expr (e1);

      if (TREE_SIDE_EFFECTS (*exp_addr_ptr))
	{
	  exp = build_indirect_ref (exp_addr, NULL_PTR);
	  *exp_addr_ptr = exp_addr;
	}

      /* This is really hairy: if the function pointer is a pointer
	 to a non-virtual member function, then we can't go mucking
	 with the `this' pointer (any more than we already have to
	 this point).  If it is a pointer to a virtual member function,
	 then we have to adjust the `this' pointer according to
	 what the virtual function table tells us.  */

      e3 = build_vfn_ref (exp_addr_ptr, exp, e0);
      my_friendly_assert (e3 != error_mark_node, 213);

      /* Change this pointer type from `void *' to the
	 type it is really supposed to be.  */
      TREE_TYPE (e3) = TREE_TYPE (function);

      /* If non-virtual, use what we had originally.  Otherwise,
	 use the value we get from the virtual function table.  */
      *exp_addr_ptr = build_conditional_expr (e1, exp_addr, *exp_addr_ptr);

      function = build_conditional_expr (e1, function, e3);
    }
  return build_indirect_ref (function, NULL_PTR);
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

  if (TREE_CODE (exp) == TREE_LIST)
    return build_unary_op (ADDR_EXPR, exp, 0);

  if (TREE_CODE (exp) != OFFSET_REF)
    {
      my_friendly_assert (TREE_CODE (type) == OFFSET_TYPE, 214);
      if (TYPE_OFFSET_BASETYPE (type) != current_class_type)
	{
	  error ("object missing in use of pointer-to-member construct");
	  return error_mark_node;
	}
      member = exp;
      type = TREE_TYPE (type);
      base = C_C_D;
    }
  else
    {
      member = TREE_OPERAND (exp, 1);
      base = TREE_OPERAND (exp, 0);
    }

  if ((TREE_CODE (member) == VAR_DECL
       && ! TYPE_PTRMEMFUNC_P (TREE_TYPE (member)))
      || TREE_CODE (TREE_TYPE (member)) == FUNCTION_TYPE)
    {
      /* These were static members.  */
      if (mark_addressable (member) == 0)
	return error_mark_node;
      return member;
    }

  /* Syntax error can cause a member which should
     have been seen as static to be grok'd as non-static.  */
  if (TREE_CODE (member) == FIELD_DECL && C_C_D == NULL_TREE)
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
      && (base == C_C_D
	  || (TREE_CODE (base) == NOP_EXPR
	      && TREE_OPERAND (base, 0) == error_mark_node)))
    {
      tree basetype_path;
      enum access_type access;

      if (TREE_CODE (exp) == OFFSET_REF && TREE_CODE (type) == OFFSET_TYPE)
	basetype = TYPE_OFFSET_BASETYPE (type);
      else
	basetype = DECL_CONTEXT (member);

      base = current_class_decl;
      
      if (get_base_distance (basetype, TREE_TYPE (TREE_TYPE (base)), 0, &basetype_path) < 0)
	{
	  error_not_base_type (basetype, TREE_TYPE (TREE_TYPE (base)));
	  return error_mark_node;
	}
      addr = convert_pointer_to (basetype, base);
      access = compute_access (basetype_path, member);
      if (access == access_public)
	return build (COMPONENT_REF, TREE_TYPE (member),
		      build_indirect_ref (addr, NULL_PTR), member);
      if (access == access_protected)
	{
	  cp_error_at ("member `%D' is protected", member);
	  error ("in this context");
	  return error_mark_node;
	}
      if (access == access_private)
	{
	  cp_error_at ("member `%D' is private", member);
	  error ("in this context");
	  return error_mark_node;
	}
      my_friendly_abort (55);
    }

  /* If this is a reference to a member function, then return
     the address of the member function (which may involve going
     through the object's vtable), otherwise, return an expression
     for the dereferenced pointer-to-member construct.  */
  addr = build_unary_op (ADDR_EXPR, base, 0);

  if (TREE_CODE (TREE_TYPE (member)) == METHOD_TYPE)
    {
      basetype = DECL_CLASS_CONTEXT (member);
      addr = convert_pointer_to (basetype, addr);
      return build_unary_op (ADDR_EXPR, get_member_function (&addr, build_indirect_ref (addr, NULL_PTR), member), 0);
    }
  else if (TREE_CODE (TREE_TYPE (member)) == OFFSET_TYPE)
    {
      basetype = TYPE_OFFSET_BASETYPE (TREE_TYPE (member));
      addr = convert_pointer_to (basetype, addr);
      member = convert (ptrdiff_type_node,
			build_unary_op (ADDR_EXPR, member, 0));
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
#if 0
      /* These may be necessary for C, but they break C++.  */
      ! TREE_PUBLIC (decl)
      /* Don't change a variable array bound or initial value to a constant
	 in a place where a variable is invalid.  */
      && ! pedantic
#endif /* 0 */
      && DECL_INITIAL (decl) != 0
      && TREE_CODE (DECL_INITIAL (decl)) != ERROR_MARK
      /* This is invalid if initial value is not constant.
	 If it has either a function call, a memory reference,
	 or a variable, then re-evaluating it could give different results.  */
      && TREE_CONSTANT (DECL_INITIAL (decl))
      /* Check for cases where this is sub-optimal, even though valid.  */
      && TREE_CODE (DECL_INITIAL (decl)) != CONSTRUCTOR
#if 0
      /* We must allow this to work outside of functions so that
	 static constants can be used for array sizes.  */
      && current_function_decl != 0
      && DECL_MODE (decl) != BLKmode
#endif
      )
    return DECL_INITIAL (decl);
  return decl;
}

/* Friend handling routines.  */
/* Friend data structures:

   Lists of friend functions come from TYPE_DECL nodes.  Since all
   aggregate types are automatically typedef'd, these nodes are guaranteed
   to exist.

   The TREE_PURPOSE of a friend list is the name of the friend,
   and its TREE_VALUE is another list.

   For each element of that list, either the TREE_VALUE or the TREE_PURPOSE
   will be filled in, but not both.  The TREE_VALUE of that list is an
   individual function which is a friend.  The TREE_PURPOSE of that list
   indicates a type in which all functions by that name are friends.

   Lists of friend classes come from _TYPE nodes.  Love that consistency
   thang.  */

int
is_friend_type (type1, type2)
     tree type1, type2;
{
  return is_friend (type1, type2);
}

int
is_friend (type, supplicant)
     tree type, supplicant;
{
  int declp;
  register tree list;

  if (supplicant == NULL_TREE || type == NULL_TREE)
    return 0;

  declp = (TREE_CODE_CLASS (TREE_CODE (supplicant)) == 'd');

  if (declp)
    /* It's a function decl.  */
    {
      tree list = DECL_FRIENDLIST (TYPE_NAME (type));
      tree name = DECL_NAME (supplicant);
      tree ctype;

      if (DECL_FUNCTION_MEMBER_P (supplicant))
	ctype = DECL_CLASS_CONTEXT (supplicant);
      else
	ctype = NULL_TREE;

      for (; list ; list = TREE_CHAIN (list))
	{
	  if (name == TREE_PURPOSE (list))
	    {
	      tree friends = TREE_VALUE (list);
	      name = DECL_ASSEMBLER_NAME (supplicant);
	      for (; friends ; friends = TREE_CHAIN (friends))
		{
		  if (ctype == TREE_PURPOSE (friends))
		    return 1;
		  if (name == DECL_ASSEMBLER_NAME (TREE_VALUE (friends)))
		    return 1;
		}
	      break;
	    }
	}
    }
  else
    /* It's a type. */
    {
      if (type == supplicant)
	return 1;
      
      list = CLASSTYPE_FRIEND_CLASSES (TREE_TYPE (TYPE_NAME (type)));
      for (; list ; list = TREE_CHAIN (list))
	if (supplicant == TREE_VALUE (list))
	  return 1;
    }      

  {
    tree context;

    if (! declp)
      context = DECL_CONTEXT (TYPE_NAME (supplicant));
    else if (DECL_FUNCTION_MEMBER_P (supplicant))
      context = DECL_CLASS_CONTEXT (supplicant);
    else
      context = NULL_TREE;

    if (context)
      return is_friend (type, context);
  }

  return 0;
}

/* Add a new friend to the friends of the aggregate type TYPE.
   DECL is the FUNCTION_DECL of the friend being added.  */
static void
add_friend (type, decl)
     tree type, decl;
{
  tree typedecl = TYPE_NAME (type);
  tree list = DECL_FRIENDLIST (typedecl);
  tree name = DECL_NAME (decl);

  while (list)
    {
      if (name == TREE_PURPOSE (list))
	{
	  tree friends = TREE_VALUE (list);
	  for (; friends ; friends = TREE_CHAIN (friends))
	    {
	      if (decl == TREE_VALUE (friends))
		{
		  cp_warning ("`%D' is already a friend of class `%T'",
			      decl, type);
		  cp_warning_at ("previous friend declaration of `%D'",
				 TREE_VALUE (friends));
		  return;
		}
	    }
	  TREE_VALUE (list) = tree_cons (error_mark_node, decl,
					 TREE_VALUE (list));
	  return;
	}
      list = TREE_CHAIN (list);
    }
  DECL_FRIENDLIST (typedecl)
    = tree_cons (DECL_NAME (decl), build_tree_list (error_mark_node, decl),
		 DECL_FRIENDLIST (typedecl));
  if (DECL_NAME (decl) == ansi_opname[(int) MODIFY_EXPR])
    {
      tree parmtypes = TYPE_ARG_TYPES (TREE_TYPE (decl));
      TYPE_HAS_ASSIGNMENT (TREE_TYPE (typedecl)) = 1;
      if (parmtypes && TREE_CHAIN (parmtypes))
	{
	  tree parmtype = TREE_VALUE (TREE_CHAIN (parmtypes));
	  if (TREE_CODE (parmtype) == REFERENCE_TYPE
	      && TREE_TYPE (parmtypes) == TREE_TYPE (typedecl))
	    TYPE_HAS_ASSIGN_REF (TREE_TYPE (typedecl)) = 1;
	}
    }
}

/* Declare that every member function NAME in FRIEND_TYPE
   (which may be NULL_TREE) is a friend of type TYPE.  */
static void
add_friends (type, name, friend_type)
     tree type, name, friend_type;
{
  tree typedecl = TYPE_NAME (type);
  tree list = DECL_FRIENDLIST (typedecl);

  while (list)
    {
      if (name == TREE_PURPOSE (list))
	{
	  tree friends = TREE_VALUE (list);
	  while (friends && TREE_PURPOSE (friends) != friend_type)
	    friends = TREE_CHAIN (friends);
	  if (friends)
	    if (friend_type)
	      warning ("method `%s::%s' is already a friend of class",
		       TYPE_NAME_STRING (friend_type),
		       IDENTIFIER_POINTER (name));
	    else
	      warning ("function `%s' is already a friend of class `%s'",
		       IDENTIFIER_POINTER (name),
		       IDENTIFIER_POINTER (DECL_NAME (typedecl)));
	  else
	    TREE_VALUE (list) = tree_cons (friend_type, NULL_TREE,
					   TREE_VALUE (list));
	  return;
	}
      list = TREE_CHAIN (list);
    }
  DECL_FRIENDLIST (typedecl) =
    tree_cons (name,
	       build_tree_list (friend_type, NULL_TREE),
	       DECL_FRIENDLIST (typedecl));
  if (! strncmp (IDENTIFIER_POINTER (name),
		 IDENTIFIER_POINTER (ansi_opname[(int) MODIFY_EXPR]),
		 strlen (IDENTIFIER_POINTER (ansi_opname[(int) MODIFY_EXPR]))))
    {
      TYPE_HAS_ASSIGNMENT (TREE_TYPE (typedecl)) = 1;
      sorry ("declaring \"friend operator =\" will not find \"operator = (X&)\" if it exists");
    }
}

/* Set up a cross reference so that type TYPE will make member function
   CTYPE::DECL a friend when CTYPE is finally defined.  For more than
   one, set up a cross reference so that functions with the name DECL
   and type CTYPE know that they are friends of TYPE.  */
static void
xref_friend (type, decl, ctype)
     tree type, decl, ctype;
{
  tree friend_decl = TYPE_NAME (ctype);
#if 0
  tree typedecl = TYPE_NAME (type);
  tree t = tree_cons (NULL_TREE, ctype, DECL_UNDEFINED_FRIENDS (typedecl));

  DECL_UNDEFINED_FRIENDS (typedecl) = t;
#else
  tree t = 0;
#endif
  SET_DECL_WAITING_FRIENDS (friend_decl,
			    tree_cons (type, t,
				       DECL_WAITING_FRIENDS (friend_decl)));
  TREE_TYPE (DECL_WAITING_FRIENDS (friend_decl)) = decl;
}

/* Make FRIEND_TYPE a friend class to TYPE.  If FRIEND_TYPE has already
   been defined, we make all of its member functions friends of
   TYPE.  If not, we make it a pending friend, which can later be added
   when its definition is seen.  If a type is defined, then its TYPE_DECL's
   DECL_UNDEFINED_FRIENDS contains a (possibly empty) list of friend
   classes that are not defined.  If a type has not yet been defined,
   then the DECL_WAITING_FRIENDS contains a list of types
   waiting to make it their friend.  Note that these two can both
   be in use at the same time!  */
void
make_friend_class (type, friend_type)
     tree type, friend_type;
{
  tree classes;

  if (IS_SIGNATURE (type))
    {
      error ("`friend' declaration in signature definition");
      return;
    }
  if (IS_SIGNATURE (friend_type))
    {
      error ("signature type `%s' declared `friend'",
	     IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (friend_type))));
      return;
    }
  if (type == friend_type)
    {
      pedwarn ("class `%s' is implicitly friends with itself",
	       TYPE_NAME_STRING (type));
      return;
    }

  GNU_xref_hier (TYPE_NAME_STRING (type),
		 TYPE_NAME_STRING (friend_type), 0, 0, 1);

  classes = CLASSTYPE_FRIEND_CLASSES (type);
  while (classes && TREE_VALUE (classes) != friend_type)
    classes = TREE_CHAIN (classes);
  if (classes)
    warning ("class `%s' is already friends with class `%s'",
	     TYPE_NAME_STRING (TREE_VALUE (classes)), TYPE_NAME_STRING (type));
  else
    {
      CLASSTYPE_FRIEND_CLASSES (type)
	= tree_cons (NULL_TREE, friend_type, CLASSTYPE_FRIEND_CLASSES (type));
    }
}

/* Main friend processor.  This is large, and for modularity purposes,
   has been removed from grokdeclarator.  It returns `void_type_node'
   to indicate that something happened, though a FIELD_DECL is
   not returned.

   CTYPE is the class this friend belongs to.

   DECLARATOR is the name of the friend.

   DECL is the FUNCTION_DECL that the friend is.

   In case we are parsing a friend which is part of an inline
   definition, we will need to store PARM_DECL chain that comes
   with it into the DECL_ARGUMENTS slot of the FUNCTION_DECL.

   FLAGS is just used for `grokclassfn'.

   QUALS say what special qualifies should apply to the object
   pointed to by `this'.  */
tree
do_friend (ctype, declarator, decl, parmdecls, flags, quals)
     tree ctype, declarator, decl, parmdecls;
     enum overload_flags flags;
     tree quals;
{
  /* Every decl that gets here is a friend of something.  */
  DECL_FRIEND_P (decl) = 1;

  if (ctype)
    {
      tree cname = TYPE_NAME (ctype);
      if (TREE_CODE (cname) == TYPE_DECL)
	cname = DECL_NAME (cname);

      /* A method friend.  */
      if (TREE_CODE (decl) == FUNCTION_DECL)
	{
	  if (flags == NO_SPECIAL && ctype && declarator == cname)
	    DECL_CONSTRUCTOR_P (decl) = 1;

	  /* This will set up DECL_ARGUMENTS for us.  */
	  grokclassfn (ctype, cname, decl, flags, quals);
	  if (TYPE_SIZE (ctype) != 0)
	    check_classfn (ctype, cname, decl);

	  if (TREE_TYPE (decl) != error_mark_node)
	    {
	      if (TYPE_SIZE (ctype))
		{
		  /* We don't call pushdecl here yet, or ever on this
		     actual FUNCTION_DECL.  We must preserve its TREE_CHAIN
		     until the end.  */
		  make_decl_rtl (decl, NULL_PTR, 1);
		  add_friend (current_class_type, decl);
		}
	      else
		{
		  register char *classname
		    = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (ctype)));

		  error ("member declared as friend before type `%s' defined",
			 classname);
		}
	    }
	}
      else
	{
	  /* Possibly a bunch of method friends.  */

	  /* Get the class they belong to.  */
	  tree ctype = IDENTIFIER_TYPE_VALUE (cname);

	  /* This class is defined, use its methods now.  */
	  if (TYPE_SIZE (ctype))
	    {
	      tree fields = lookup_fnfields (TYPE_BINFO (ctype), declarator, 0);
	      if (fields)
		add_friends (current_class_type, declarator, ctype);
	      else
		error ("method `%s' is not a member of class `%s'",
		       IDENTIFIER_POINTER (declarator),
		       IDENTIFIER_POINTER (cname));
	    }
	  else
	    /* Note: DECLARATOR actually has more than one; in this
	       case, we're making sure that fns with the name DECLARATOR
	       and type CTYPE know they are friends of the current
	       class type.  */
	    xref_friend (current_class_type, declarator, ctype);
	  decl = void_type_node;
	}
    }
  else if (TREE_CODE (decl) == FUNCTION_DECL
	   && ((IDENTIFIER_LENGTH (declarator) == 4
		&& IDENTIFIER_POINTER (declarator)[0] == 'm'
		&& ! strcmp (IDENTIFIER_POINTER (declarator), "main"))
	       || (IDENTIFIER_LENGTH (declarator) > 10
		   && IDENTIFIER_POINTER (declarator)[0] == '_'
		   && IDENTIFIER_POINTER (declarator)[1] == '_'
		   && strncmp (IDENTIFIER_POINTER (declarator)+2,
			       "builtin_", 8) == 0)))
    {
      /* raw "main", and builtin functions never gets overloaded,
	 but they can become friends.  */
      add_friend (current_class_type, decl);
      DECL_FRIEND_P (decl) = 1;
      decl = void_type_node;
    }
  /* A global friend.
     @@ or possibly a friend from a base class ?!?  */
  else if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      /* Friends must all go through the overload machinery,
	 even though they may not technically be overloaded.

	 Note that because classes all wind up being top-level
	 in their scope, their friend wind up in top-level scope as well.  */
      DECL_ASSEMBLER_NAME (decl)
	= build_decl_overload (declarator, TYPE_ARG_TYPES (TREE_TYPE (decl)),
			       TREE_CODE (TREE_TYPE (decl)) == METHOD_TYPE);
      DECL_ARGUMENTS (decl) = parmdecls;
      DECL_CLASS_CONTEXT (decl) = current_class_type;

      /* We can call pushdecl here, because the TREE_CHAIN of this
	 FUNCTION_DECL is not needed for other purposes.  */
      decl = pushdecl (decl);

      make_decl_rtl (decl, NULL_PTR, 1);
      add_friend (current_class_type, decl);

      DECL_FRIEND_P (decl) = 1;
#if 0
      TREE_OVERLOADED (declarator) = 1;
#endif
    }
  else
    {
      /* @@ Should be able to ingest later definitions of this function
	 before use.  */
      tree decl = lookup_name_nonclass (declarator);
      if (decl == NULL_TREE)
	{
	  warning ("implicitly declaring `%s' as struct",
		   IDENTIFIER_POINTER (declarator));
	  decl = xref_tag (record_type_node, declarator, NULL_TREE, 1);
	  decl = TYPE_NAME (decl);
	}

      /* Allow abbreviated declarations of overloaded functions,
	 but not if those functions are really class names.  */
      if (TREE_CODE (decl) == TREE_LIST && TREE_TYPE (TREE_PURPOSE (decl)))
	{
	  warning ("`friend %s' archaic, use `friend class %s' instead",
		   IDENTIFIER_POINTER (declarator),
		   IDENTIFIER_POINTER (declarator));
	  decl = TREE_TYPE (TREE_PURPOSE (decl));
	}

      if (TREE_CODE (decl) == TREE_LIST)
	add_friends (current_class_type, TREE_PURPOSE (decl), NULL_TREE);
      else
	make_friend_class (current_class_type, TREE_TYPE (decl));
      decl = void_type_node;
    }
  return decl;
}

/* TYPE has now been defined.  It may, however, have a number of things
   waiting make make it their friend.  We resolve these references
   here.  */
void
embrace_waiting_friends (type)
     tree type;
{
  tree decl = TYPE_NAME (type);
  tree waiters;

  if (TREE_CODE (decl) != TYPE_DECL)
    return;

  for (waiters = DECL_WAITING_FRIENDS (decl); waiters;
       waiters = TREE_CHAIN (waiters))
    {
      tree waiter = TREE_PURPOSE (waiters);
#if 0
      tree waiter_prev = TREE_VALUE (waiters);
#endif
      tree decl = TREE_TYPE (waiters);
      tree name = decl ? (TREE_CODE (decl) == IDENTIFIER_NODE
			  ? decl : DECL_NAME (decl)) : NULL_TREE;
      if (name)
	{
	  /* @@ There may be work to be done since we have not verified
	     @@ consistency between original and friend declarations
	     @@ of the functions waiting to become friends.  */
	  tree field = lookup_fnfields (TYPE_BINFO (type), name, 0);
	  if (field)
	    if (decl == name)
	      add_friends (waiter, name, type);
	    else
	      add_friend (waiter, decl);
	  else
	    error_with_file_and_line (DECL_SOURCE_FILE (TYPE_NAME (waiter)),
				      DECL_SOURCE_LINE (TYPE_NAME (waiter)),
				      "no method `%s' defined in class `%s' to be friend",
				      IDENTIFIER_POINTER (DECL_NAME (TREE_TYPE (waiters))),
				      TYPE_NAME_STRING (type));
	}
      else
	make_friend_class (type, waiter);

#if 0
      if (TREE_CHAIN (waiter_prev))
	TREE_CHAIN (waiter_prev) = TREE_CHAIN (TREE_CHAIN (waiter_prev));
      else
	DECL_UNDEFINED_FRIENDS (TYPE_NAME (waiter)) = NULL_TREE;
#endif
    }
}

/* Common subroutines of build_new and build_vec_delete.  */

/* Common interface for calling "builtin" functions that are not
   really builtin.  */

tree
build_builtin_call (type, node, arglist)
     tree type;
     tree node;
     tree arglist;
{
  tree rval = build (CALL_EXPR, type, node, arglist, 0);
  TREE_SIDE_EFFECTS (rval) = 1;
  assemble_external (TREE_OPERAND (node, 0));
  TREE_USED (TREE_OPERAND (node, 0)) = 1;
  return rval;
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
  tree type, true_type, size, rval;
  tree nelts;
  tree alloc_expr, alloc_temp;
  int has_array = 0;
  enum tree_code code = NEW_EXPR;

  tree pending_sizes = NULL_TREE;

  if (decl == error_mark_node)
    return error_mark_node;

  if (TREE_CODE (decl) == TREE_LIST)
    {
      tree absdcl = TREE_VALUE (decl);
      tree last_absdcl = NULL_TREE;
      int old_immediate_size_expand;

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
	      else
		{
		  this_nelts = save_expr (convert (sizetype, this_nelts));
		  absdcl = TREE_OPERAND (absdcl, 0);
	          if (this_nelts == integer_zero_node)
		    {
		      warning ("zero size array reserves no space");
		      nelts = integer_zero_node;
		    }
		  else
		    nelts = build_binary_op (MULT_EXPR, nelts, this_nelts, 1);
		}
	    }
	  else
	    nelts = integer_zero_node;
	}

      if (last_absdcl)
	TREE_OPERAND (last_absdcl, 0) = absdcl;
      else
	TREE_VALUE (decl) = absdcl;

      type = true_type = groktypename (decl);
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
	  decl = TYPE_NAME (type);
	}
      else
	{
	  /* A builtin type.  */
	  decl = lookup_name (decl, 1);
	  my_friendly_assert (TREE_CODE (decl) == TYPE_DECL, 215);
	  type = TREE_TYPE (decl);
	}
      true_type = type;
    }
  else if (TREE_CODE (decl) == TYPE_DECL)
    {
      type = TREE_TYPE (decl);
      true_type = type;
    }
  else
    {
      type = decl;
      true_type = type;
      decl = TYPE_NAME (type);
    }

  /* ``A reference cannot be created by the new operator.  A reference
     is not an object (8.2.2, 8.4.3), so a pointer to it could not be
     returned by new.'' ARM 5.3.3 */
  if (TREE_CODE (type) == REFERENCE_TYPE)
    {
      error ("new cannot be applied to a reference type");
      type = true_type = TREE_TYPE (type);
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
      type = true_type = TREE_TYPE (type);
    }

  if (TYPE_READONLY (type) || TYPE_VOLATILE (type))
    type = TYPE_MAIN_VARIANT (type);

  /* If our base type is an array, then make sure we know how many elements
     it has.  */
  while (TREE_CODE (true_type) == ARRAY_TYPE)
    {
      tree this_nelts = array_type_nelts_top (true_type);
      nelts = build_binary_op (MULT_EXPR, nelts, this_nelts, 1);
      true_type = TREE_TYPE (true_type);
    }
  if (has_array)
    size = fold (build_binary_op (MULT_EXPR, size_in_bytes (true_type),
				  nelts, 1));
  else
    size = size_in_bytes (type);

  if (true_type == void_type_node)
    {
      error ("invalid type `void' for new");
      return error_mark_node;
    }

  if (TYPE_SIZE (true_type) == 0)
    {
      incomplete_type_error (0, true_type);
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

  /* Get a little extra space to store a couple of things before the new'ed
     array. */
  if (has_array && TYPE_VEC_NEW_USES_COOKIE (true_type))
    {
      tree extra = BI_header_size;

      size = size_binop (PLUS_EXPR, size, extra);
    }

  if (has_array)
    code = VEC_NEW_EXPR;

  /* Allocate the object. */
  if (! use_global_new && TYPE_LANG_SPECIFIC (true_type)
      && (TYPE_GETS_NEW (true_type) & (1 << has_array)))
    rval = build_opfncall (code, LOOKUP_NORMAL,
			   build_pointer_type (true_type), size, placement);
  else if (placement)
    {
      rval = build_opfncall (code, LOOKUP_GLOBAL|LOOKUP_COMPLAIN,
			     ptr_type_node, size, placement);
      rval = convert (build_pointer_type (true_type), rval);
    }
  else if (! has_array && flag_this_is_variable > 0
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
  else
    {
      rval = build_builtin_call (build_pointer_type (true_type),
				 has_array ? BIVN : BIN,
				 build_tree_list (NULL_TREE, size));
#if 0
      /* See comment above as to why this is disabled.  */
      if (alignment)
	{
	  rval = build (PLUS_EXPR, build_pointer_type (true_type), rval,
			alignment);
	  rval = build (BIT_AND_EXPR, build_pointer_type (true_type),
			rval, build1 (BIT_NOT_EXPR, integer_type_node,
				      alignment));
	}
#endif
      TREE_CALLS_NEW (rval) = 1;
    }

  if (flag_check_new && rval)
    {
      /* For array new, we need to make sure that the call to new is
	 not expanded as part of the RTL_EXPR for the initialization,
	 so we can't just use save_expr here.  */

      alloc_temp = get_temp_name (TREE_TYPE (rval), 0);
      alloc_expr = build (MODIFY_EXPR, TREE_TYPE (rval), alloc_temp, rval);
      TREE_SIDE_EFFECTS (alloc_expr) = 1;
      rval = alloc_temp;
    }
  else
    alloc_expr = NULL_TREE;

  /* if rval is NULL_TREE I don't have to allocate it, but are we totally
     sure we have some extra bytes in that case for the BI_header_size
     cookies? And how does that interact with the code below? (mrs) */
  /* Finish up some magic for new'ed arrays */
  if (has_array && TYPE_VEC_NEW_USES_COOKIE (true_type) && rval != NULL_TREE)
    {
      tree extra = BI_header_size;
      tree cookie, exp1;
      rval = convert (ptr_type_node, rval);    /* convert to void * first */
      rval = convert (string_type_node, rval); /* lets not add void* and ints */
      rval = save_expr (build_binary_op (PLUS_EXPR, rval, extra, 1));
      /* Store header info.  */
      cookie = build_indirect_ref (build (MINUS_EXPR, build_pointer_type (BI_header_type),
					  rval, extra), NULL_PTR);
      exp1 = build (MODIFY_EXPR, void_type_node,
		    build_component_ref (cookie, nc_nelts_field_id, 0, 0),
		    nelts);
      TREE_SIDE_EFFECTS (exp1) = 1;
      rval = convert (build_pointer_type (true_type), rval);
      TREE_CALLS_NEW (rval) = 1;
      TREE_SIDE_EFFECTS (rval) = 1;
      rval = build_compound_expr (tree_cons (NULL_TREE, exp1,
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
	  /* New 2.0 interpretation: `new int (10)' means
	     allocate an int, and initialize it with 10.  */
	  tree deref;

	  rval = save_expr (rval);
	  deref = build_indirect_ref (rval, NULL_PTR);
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
	  TREE_CALLS_NEW (rval) = 1;
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

	  newrval = rval;

	  if (newrval && TREE_CODE (TREE_TYPE (newrval)) == POINTER_TYPE)
	    newrval = build_indirect_ref (newrval, NULL_PTR);

	  newrval = build_method_call (newrval, constructor_name_full (true_type),
				       init, NULL_TREE, flags);

	  if (newrval)
	    {
	      rval = newrval;
	      TREE_HAS_CONSTRUCTOR (rval) = 1;
	    }
	  else
	    rval = error_mark_node;
	}
      else if (current_function_decl == NULL_TREE)
	{
	  extern tree static_aggregates;

	  /* In case of static initialization, SAVE_EXPR is good enough.  */
	  rval = save_expr (rval);
	  rval = copy_to_permanent (rval);
	  init = copy_to_permanent (init);
	  init = expand_vec_init (decl, rval,
				  build_binary_op (MINUS_EXPR, nelts,
						   integer_one_node, 1),
				  init, 0);
	  init = copy_to_permanent (init);
	  static_aggregates = perm_tree_cons (init, rval, static_aggregates);
	}
      else
	{
	  /* Have to wrap this in RTL_EXPR for two cases:
	     in base or member initialization and if we
	     are a branch of a ?: operator.  Since we
	     can't easily know the latter, just do it always.  */
	  tree xval = make_node (RTL_EXPR);

	  /* If we want to check the value of the allocation expression,
             and the number of elements in the array is not a constant, we
             *must* expand the SAVE_EXPR for nelts in alloc_expr before we
             expand it in the actual initialization.  So we need to build up
             an RTL_EXPR for alloc_expr.  Sigh.  */
	  if (alloc_expr && ! TREE_CONSTANT (nelts))
	    {
	      tree xval = make_node (RTL_EXPR);
	      rtx rtxval;
	      TREE_TYPE (xval) = TREE_TYPE (alloc_expr);
	      do_pending_stack_adjust ();
	      start_sequence_for_rtl_expr (xval);
	      emit_note (0, -1);
	      rtxval = expand_expr (alloc_expr, NULL, VOIDmode, 0);
	      do_pending_stack_adjust ();
	      TREE_SIDE_EFFECTS (xval) = 1;
	      RTL_EXPR_SEQUENCE (xval) = get_insns ();
	      end_sequence ();
	      RTL_EXPR_RTL (xval) = rtxval;
	      TREE_TYPE (xval) = TREE_TYPE (alloc_expr);
	      alloc_expr = xval;
	    }

	  TREE_TYPE (xval) = TREE_TYPE (rval);
	  do_pending_stack_adjust ();
	  start_sequence_for_rtl_expr (xval);

	  /* As a matter of principle, `start_sequence' should do this.  */
	  emit_note (0, -1);

	  rval = save_expr (rval);
	  rval = expand_vec_init (decl, rval,
				  build_binary_op (MINUS_EXPR, nelts,
						   integer_one_node, 1),
				  init, 0);

	  do_pending_stack_adjust ();

	  TREE_SIDE_EFFECTS (xval) = 1;
	  TREE_CALLS_NEW (xval) = 1;
	  RTL_EXPR_SEQUENCE (xval) = get_insns ();
	  end_sequence ();

	  if (TREE_CODE (rval) == SAVE_EXPR)
	    {
	      /* Errors may cause this to not get evaluated.  */
	      if (SAVE_EXPR_RTL (rval) == 0)
		SAVE_EXPR_RTL (rval) = const0_rtx;
	      RTL_EXPR_RTL (xval) = SAVE_EXPR_RTL (rval);
	    }
	  else
	    {
	      my_friendly_assert (TREE_CODE (rval) == VAR_DECL, 217);
	      RTL_EXPR_RTL (xval) = DECL_RTL (rval);
	    }
	  rval = xval;
	}
    }
  else if (TYPE_READONLY (true_type))
    cp_error ("uninitialized const in `new' of `%#T'", true_type);

 done:

  if (alloc_expr)
    {
      /* Did we modify the storage?  */
      if (rval != alloc_temp)
	{
	  tree ifexp = build_binary_op (NE_EXPR, alloc_expr,
					integer_zero_node, 1);
	  rval = build_conditional_expr (ifexp, rval, alloc_temp);
	}
      else
	rval = alloc_expr;
    }

  if (rval && TREE_TYPE (rval) != build_pointer_type (type))
    {
      /* The type of new int [3][3] is not int *, but int [3] * */
      rval = build_c_cast (build_pointer_type (type), rval, 0);
    }

  if (pending_sizes)
    rval = build_compound_expr (chainon (pending_sizes,
					 build_tree_list (NULL_TREE, rval)));

  if (flag_gc)
    {
      extern tree gc_visible;
      tree objbits;
      tree update_expr;

      rval = save_expr (rval);
      /* We don't need a `headof' operation to do this because
	 we know where the object starts.  */
      objbits = build1 (INDIRECT_REF, unsigned_type_node,
			build (MINUS_EXPR, ptr_type_node,
			       rval, c_sizeof_nowarn (unsigned_type_node)));
      update_expr = build_modify_expr (objbits, BIT_IOR_EXPR, gc_visible);
      rval = build_compound_expr (tree_cons (NULL_TREE, rval,
					     tree_cons (NULL_TREE, update_expr,
							build_tree_list (NULL_TREE, rval))));
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
  tree ptype = build_pointer_type (type);
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

  /* This is the BLOCK to record the symbol binding for debugging.  */
  tree block;

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
  controller = build (BIND_EXPR, void_type_node, tbase, 0, 0);
  TREE_SIDE_EFFECTS (controller) = 1;
  block = build_block (tbase, 0, 0, 0, 0);
  add_block_current_level (block);

  if (auto_delete != integer_zero_node
      && auto_delete != integer_two_node)
    {
      tree base_tbd = convert (ptype,
			       build_binary_op (MINUS_EXPR,
						convert (ptr_type_node, base),
						BI_header_size,
						1));
      /* This is the real size */
      virtual_size = size_binop (PLUS_EXPR, virtual_size, BI_header_size);
      body = build_tree_list (NULL_TREE,
			      build_x_delete (ptype, base_tbd,
					      2 | use_global_delete,
					      virtual_size));
      body = build (COND_EXPR, void_type_node,
		    build (BIT_AND_EXPR, integer_type_node,
			   auto_delete, integer_one_node),
		    body, integer_zero_node);
    }
  else
    body = NULL_TREE;

  body = tree_cons (NULL_TREE,
		    build_delete (ptype, tbase, auto_delete,
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
  if (auto_delete_vec == integer_zero_node
      || auto_delete_vec == integer_two_node)
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
	  base_tbd = convert (ptype,
			      build_binary_op (MINUS_EXPR,
					       convert (string_type_node, base),
					       BI_header_size,
					       1));
	  /* True size with header. */
	  virtual_size = size_binop (PLUS_EXPR, virtual_size, BI_header_size);
	}
      deallocate_expr = build_x_delete (ptype, base_tbd,
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
      body = tree_cons (NULL_TREE, loop,
			tree_cons (NULL_TREE, deallocate_expr, NULL_TREE));
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
    return convert (void_type_node, body);
}

/* Build a tree to cleanup partially built arrays.
   BASE is that starting address of the array.
   COUNT is the count of objects that have been built, that need destroying.
   TYPE is the type of elements in the array.  */
static tree
build_array_eh_cleanup (base, count, type)
     tree base, count, type;
{
  tree expr = build_vec_delete_1 (base, count, type, integer_two_node,
				  integer_zero_node, 0);
  return expr;
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
  tree iterator, base2 = NULL_TREE;
  tree type = TREE_TYPE (TREE_TYPE (base));
  tree size;

  maxindex = convert (integer_type_node, maxindex);
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

  /* Set to zero in case size is <= 0.  Optimizer will delete this if
     it is not needed.  */
  rval = get_temp_regvar (build_pointer_type (type),
			  convert (build_pointer_type (type), null_pointer_node));
  base = default_conversion (base);
  base = convert (build_pointer_type (type), base);
  expand_assignment (rval, base, 0, 0);
  base = get_temp_regvar (build_pointer_type (type), base);

  if (init != NULL_TREE
      && TREE_CODE (init) == CONSTRUCTOR
      && TREE_TYPE (init) == TREE_TYPE (decl))
    {
      /* Initialization of array from {...}.  */
      tree elts = CONSTRUCTOR_ELTS (init);
      tree baseref = build1 (INDIRECT_REF, type, base);
      tree baseinc = build (PLUS_EXPR, build_pointer_type (type), base, size);
      int host_i = TREE_INT_CST_LOW (maxindex);

      if (IS_AGGR_TYPE (type))
	{
	  while (elts)
	    {
	      host_i -= 1;
	      expand_aggr_init (baseref, TREE_VALUE (elts), 0, 0);

	      expand_assignment (base, baseinc, 0, 0);
	      elts = TREE_CHAIN (elts);
	    }
	  /* Initialize any elements by default if possible.  */
	  if (host_i >= 0)
	    {
	      if (TYPE_NEEDS_CONSTRUCTING (type) == 0)
		{
		  if (obey_regdecls)
		    use_variable (DECL_RTL (base));
		  goto done_init;
		}

	      iterator = get_temp_regvar (integer_type_node,
					  build_int_2 (host_i, 0));
	      init = NULL_TREE;
	      goto init_by_default;
	    }
	}
      else
	while (elts)
	  {
	    expand_assignment (baseref, TREE_VALUE (elts), 0, 0);

	    expand_assignment (base, baseinc, 0, 0);
	    elts = TREE_CHAIN (elts);
	  }

      if (obey_regdecls)
	use_variable (DECL_RTL (base));
    }
  else
    {
      tree itype;

      iterator = get_temp_regvar (integer_type_node, maxindex);

    init_by_default:

      /* If initializing one array from another,
	 initialize element by element.  */
      if (from_array)
	{
	  /* We rely upon the below calls the do argument checking */
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

      expand_start_cond (build (GE_EXPR, boolean_type_node,
				iterator, integer_zero_node), 0);
      if (TYPE_NEEDS_DESTRUCTOR (type))
	start_protect ();
      expand_start_loop_continue_elsewhere (1);

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
	    expand_aggr_init (to, from, 0, 0);
	  else if (from)
	    expand_assignment (to, from, 0, 0);
	  else
	    my_friendly_abort (57);
	}
      else if (TREE_CODE (type) == ARRAY_TYPE)
	{
	  if (init != 0)
	    sorry ("cannot initialize multi-dimensional array with initializer");
	  expand_vec_init (decl, build1 (NOP_EXPR, build_pointer_type (TREE_TYPE (type)), base),
			   array_type_nelts (type), 0, 0);
	}
      else
	expand_aggr_init (build1 (INDIRECT_REF, type, base), init, 0, 0);

      expand_assignment (base,
			 build (PLUS_EXPR, build_pointer_type (type), base, size),
			 0, 0);
      if (base2)
	expand_assignment (base2,
			   build (PLUS_EXPR, build_pointer_type (type), base2, size), 0, 0);
      expand_loop_continue_here ();
      expand_exit_loop_if_false (0, build (NE_EXPR, boolean_type_node,
					   build (PREDECREMENT_EXPR, integer_type_node, iterator, integer_one_node), minus_one));

      if (obey_regdecls)
	{
	  use_variable (DECL_RTL (base));
	  if (base2)
	    use_variable (DECL_RTL (base2));
	}
      expand_end_loop ();
      if (TYPE_NEEDS_DESTRUCTOR (type))
	end_protect (build_array_eh_cleanup (rval,
					     build_binary_op (MINUS_EXPR,
							      maxindex,
							      iterator,
							      1),
					     type));
      expand_end_cond ();
      if (obey_regdecls)
	use_variable (DECL_RTL (iterator));
    }
 done_init:

  if (obey_regdecls)
    use_variable (DECL_RTL (rval));
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
build_x_delete (type, addr, which_delete, virtual_size)
     tree type, addr;
     int which_delete;
     tree virtual_size;
{
  int use_global_delete = which_delete & 1;
  int use_vec_delete = !!(which_delete & 2);
  tree rval;
  enum tree_code code = use_vec_delete ? VEC_DELETE_EXPR : DELETE_EXPR;

  if (! use_global_delete && TYPE_LANG_SPECIFIC (TREE_TYPE (type))
      && (TYPE_GETS_DELETE (TREE_TYPE (type)) & (1 << use_vec_delete)))
    rval = build_opfncall (code, LOOKUP_NORMAL, addr, virtual_size, NULL_TREE);
  else
    rval = build_builtin_call (void_type_node, use_vec_delete ? BIVD : BID,
			       build_tree_list (NULL_TREE, addr));
  return rval;
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
  tree function, parms;
  tree member;
  tree expr;
  tree ref;
  int ptr;

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
      if (TYPE_SIZE (type) == 0)
	{
	  incomplete_type_error (0, type);
	  return error_mark_node;
	}
      if (TREE_CODE (type) == ARRAY_TYPE)
	goto handle_array;
      if (! IS_AGGR_TYPE (type))
	{
	  /* Call the builtin operator delete.  */
	  return build_builtin_call (void_type_node, BID,
				     build_tree_list (NULL_TREE, addr));
	}
      if (TREE_SIDE_EFFECTS (addr))
	addr = save_expr (addr);

      /* throw away const and volatile on target type of addr */
      addr = convert_force (build_pointer_type (type), addr, 0);
      ref = build_indirect_ref (addr, NULL_PTR);
      ptr = 1;
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
			       c_sizeof_nowarn (TREE_TYPE (type)),
			       auto_delete, integer_two_node,
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

      if (TREE_CODE (addr) == NOP_EXPR
	  && TREE_OPERAND (addr, 0) == current_class_decl)
	ref = C_C_D;
      else
	ref = build_indirect_ref (addr, NULL_PTR);
      ptr = 0;
    }

  my_friendly_assert (IS_AGGR_TYPE (type), 220);

  if (! TYPE_NEEDS_DESTRUCTOR (type))
    {
      if (auto_delete == integer_zero_node)
	return void_zero_node;

      /* Pass the size of the object down to the operator delete() in
	 addition to the ADDR.  */
      if (TYPE_GETS_REG_DELETE (type) && !use_global_delete)
	{
	  tree virtual_size = c_sizeof_nowarn (type);
	  return build_opfncall (DELETE_EXPR, LOOKUP_NORMAL, addr,
				 virtual_size, NULL_TREE);
	}

      /* Call the builtin operator delete.  */
      return build_builtin_call (void_type_node, BID,
				 build_tree_list (NULL_TREE, addr));
    }
  parms = build_tree_list (NULL_TREE, addr);

  /* Below, we will reverse the order in which these calls are made.
     If we have a destructor, then that destructor will take care
     of the base classes; otherwise, we must do that here.  */
  if (TYPE_HAS_DESTRUCTOR (type))
    {
      tree dtor = DECL_MAIN_VARIANT (TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (type), 0));
      tree basetypes = TYPE_BINFO (type);
      tree passed_auto_delete;
      tree do_delete = NULL_TREE;

      if (use_global_delete)
	{
	  tree cond = fold (build (BIT_AND_EXPR, integer_type_node,
				   auto_delete, integer_one_node));
	  tree call = build_builtin_call
	    (void_type_node, BID, build_tree_list (NULL_TREE, addr));

	  cond = fold (build (COND_EXPR, void_type_node, cond,
			      call, void_zero_node));
	  if (cond != void_zero_node)
	    do_delete = cond;

	  passed_auto_delete = fold (build (BIT_AND_EXPR, integer_type_node,
					    auto_delete, integer_two_node));
	}
      else
	passed_auto_delete = auto_delete;

      if (flags & LOOKUP_PROTECT)
	{
	  enum access_type access = compute_access (basetypes, dtor);

	  if (access == access_private)
	    {
	      if (flags & LOOKUP_COMPLAIN)
		cp_error ("destructor for type `%T' is private in this scope", type);
	      return error_mark_node;
	    }
	  else if (access == access_protected)
	    {
	      if (flags & LOOKUP_COMPLAIN)
		cp_error ("destructor for type `%T' is protected in this scope", type);
	      return error_mark_node;
	    }
	}

      /* Once we are in a destructor, try not going through
	 the virtual function table to find the next destructor.  */
      if (DECL_VINDEX (dtor)
	  && ! (flags & LOOKUP_NONVIRTUAL)
	  && TREE_CODE (auto_delete) != PARM_DECL
	  && (ptr == 1 || ! resolves_to_fixed_type_p (ref, 0)))
	{
	  tree binfo, basetype;
	  /* The code below is probably all broken.  See call.c for the
	     complete right way to do this. this offsets may not be right
	     in the below.  (mrs) */
	  /* This destructor must be called via virtual function table.  */
	  dtor = TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (DECL_CONTEXT (dtor)), 0);
	  basetype = DECL_CLASS_CONTEXT (dtor);
	  binfo = get_binfo (basetype,
			     TREE_TYPE (TREE_TYPE (TREE_VALUE (parms))),
			     0);
	  expr = convert_pointer_to_real (binfo, TREE_VALUE (parms));
	  if (expr != TREE_VALUE (parms))
	    {
	      expr = fold (expr);
	      ref = build_indirect_ref (expr, NULL_PTR);
	      TREE_VALUE (parms) = expr;
	    }
	  function = build_vfn_ref (&TREE_VALUE (parms), ref, DECL_VINDEX (dtor));
	  if (function == error_mark_node)
	    return error_mark_node;
	  TREE_TYPE (function) = build_pointer_type (TREE_TYPE (dtor));
	  TREE_CHAIN (parms) = build_tree_list (NULL_TREE, passed_auto_delete);
	  expr = build_function_call (function, parms);
	  if (do_delete)
	    expr = build (COMPOUND_EXPR, void_type_node, expr, do_delete);
	  if (ptr && (flags & LOOKUP_DESTRUCTOR) == 0)
	    {
	      /* Handle the case where a virtual destructor is
		 being called on an item that is 0.

		 @@ Does this really need to be done?  */
	      tree ifexp = build_binary_op(NE_EXPR, addr, integer_zero_node,1);
#if 0
	      if (TREE_CODE (ref) == VAR_DECL
		  || TREE_CODE (ref) == COMPONENT_REF)
		warning ("losing in build_delete");
#endif
	      expr = build (COND_EXPR, void_type_node,
			    ifexp, expr, void_zero_node);
	    }
	}
      else
	{
	  tree ifexp;

	  if ((flags & LOOKUP_DESTRUCTOR)
	      || TREE_CODE (ref) == VAR_DECL
	      || TREE_CODE (ref) == PARM_DECL
	      || TREE_CODE (ref) == COMPONENT_REF
	      || TREE_CODE (ref) == ARRAY_REF)
	    /* These can't be 0.  */
	    ifexp = integer_one_node;
	  else
	    /* Handle the case where a non-virtual destructor is
	       being called on an item that is 0.  */
	    ifexp = build_binary_op (NE_EXPR, addr, integer_zero_node, 1);

	  /* Used to mean that this destructor was known to be empty,
	     but that's now obsolete.  */
	  my_friendly_assert (DECL_INITIAL (dtor) != void_type_node, 221);

	  TREE_CHAIN (parms) = build_tree_list (NULL_TREE, passed_auto_delete);
	  expr = build_function_call (dtor, parms);
	  if (do_delete)
	    expr = build (COMPOUND_EXPR, void_type_node, expr, do_delete);

	  if (ifexp != integer_one_node)
	    expr = build (COND_EXPR, void_type_node,
			  ifexp, expr, void_zero_node);
	}
      return expr;
    }
  else
    {
      /* This can get visibilities wrong.  */
      tree binfos = BINFO_BASETYPES (TYPE_BINFO (type));
      int i, n_baseclasses = binfos ? TREE_VEC_LENGTH (binfos) : 0;
      tree base_binfo = n_baseclasses > 0 ? TREE_VEC_ELT (binfos, 0) : NULL_TREE;
      tree exprstmt = NULL_TREE;
      tree parent_auto_delete = auto_delete;
      tree cond;

      /* If this type does not have a destructor, but does have
	 operator delete, call the parent parent destructor (if any),
	 but let this node do the deleting.  Otherwise, it is ok
	 to let the parent destructor do the deleting.  */
      if (TYPE_GETS_REG_DELETE (type) && !use_global_delete)
	{
	  parent_auto_delete = integer_zero_node;
	  if (auto_delete == integer_zero_node)
	    cond = NULL_TREE;
	  else
	    {
	      tree virtual_size;

	        /* This is probably wrong. It should be the size of the
		   virtual object being deleted.  */
	      virtual_size = c_sizeof_nowarn (type);

	      expr = build_opfncall (DELETE_EXPR, LOOKUP_NORMAL, addr,
				     virtual_size, NULL_TREE);
	      if (expr == error_mark_node)
		return error_mark_node;
	      if (auto_delete != integer_one_node)
		cond = build (COND_EXPR, void_type_node,
			      build (BIT_AND_EXPR, integer_type_node,
				     auto_delete, integer_one_node),
			      expr, void_zero_node);
	      else
		cond = expr;
	    }
	}
      else if (base_binfo == NULL_TREE
	       || (TREE_VIA_VIRTUAL (base_binfo) == 0
		   && ! TYPE_NEEDS_DESTRUCTOR (BINFO_TYPE (base_binfo))))
	{
	  tree virtual_size;

	  /* This is probably wrong. It should be the size of the virtual
	     object being deleted.  */
	  virtual_size = c_sizeof_nowarn (type);

	  cond = build (COND_EXPR, void_type_node,
			build (BIT_AND_EXPR, integer_type_node, auto_delete, integer_one_node),
			build_builtin_call (void_type_node, BID,
					    build_tree_list (NULL_TREE, addr)),
			void_zero_node);
	}
      else
	cond = NULL_TREE;

      if (cond)
	exprstmt = build_tree_list (NULL_TREE, cond);

      if (base_binfo
	  && ! TREE_VIA_VIRTUAL (base_binfo)
	  && TYPE_NEEDS_DESTRUCTOR (BINFO_TYPE (base_binfo)))
	{
	  tree this_auto_delete;

	  if (BINFO_OFFSET_ZEROP (base_binfo))
	    this_auto_delete = parent_auto_delete;
	  else
	    this_auto_delete = integer_zero_node;

	  expr = build_delete (build_pointer_type (BINFO_TYPE (base_binfo)), addr,
			       this_auto_delete, flags, 0);
	  exprstmt = tree_cons (NULL_TREE, expr, exprstmt);
	}

      /* Take care of the remaining baseclasses.  */
      for (i = 1; i < n_baseclasses; i++)
	{
	  base_binfo = TREE_VEC_ELT (binfos, i);
	  if (! TYPE_NEEDS_DESTRUCTOR (BINFO_TYPE (base_binfo))
	      || TREE_VIA_VIRTUAL (base_binfo))
	    continue;

	  /* May be zero offset if other baseclasses are virtual.  */
	  expr = fold (build (PLUS_EXPR, build_pointer_type (BINFO_TYPE (base_binfo)),
			      addr, BINFO_OFFSET (base_binfo)));

	  expr = build_delete (build_pointer_type (BINFO_TYPE (base_binfo)), expr,
			       integer_zero_node,
			       flags, 0);

	  exprstmt = tree_cons (NULL_TREE, expr, exprstmt);
	}

      for (member = TYPE_FIELDS (type); member; member = TREE_CHAIN (member))
	{
	  if (TREE_CODE (member) != FIELD_DECL)
	    continue;
	  if (TYPE_NEEDS_DESTRUCTOR (TREE_TYPE (member)))
	    {
	      tree this_member = build_component_ref (ref, DECL_NAME (member), 0, 0);
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
   AUTO_DELETE say whether each item in the container should be deallocated.

   This also calls delete for virtual baseclasses of elements of the vector.

   Update: MAXINDEX is no longer needed.  The size can be extracted from the
   start of the vector for pointers, and from the type for arrays.  We still
   use MAXINDEX for arrays because it happens to already have one of the
   values we'd have to extract.  (We could use MAXINDEX with pointers to
   confirm the size, and trap if the numbers differ; not clear that it'd
   be worth bothering.)  */
tree
build_vec_delete (base, maxindex, elt_size, auto_delete_vec, auto_delete,
		  use_global_delete)
     tree base, maxindex, elt_size;
     tree auto_delete_vec, auto_delete;
     int use_global_delete;
{
  tree type;

  if (TREE_CODE (base) == OFFSET_REF)
    base = resolve_offset_ref (base);

  type = TREE_TYPE (base);

  base = stabilize_reference (base);

  /* Since we can use base many times, save_expr it. */
  if (TREE_SIDE_EFFECTS (base))
    base = save_expr (base);

  if (TREE_CODE (type) == POINTER_TYPE)
    {
      /* Step back one from start of vector, and read dimension.  */
      tree cookie_addr = build (MINUS_EXPR, build_pointer_type (BI_header_type),
				base, BI_header_size);
      tree cookie = build_indirect_ref (cookie_addr, NULL_PTR);
      maxindex = build_component_ref (cookie, nc_nelts_field_id, 0, 0);
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
      error ("type to vector delete is neither pointer or array type");
      return error_mark_node;
    }

  return build_vec_delete_1 (base, maxindex, type, auto_delete_vec, auto_delete,
			     use_global_delete);
}
