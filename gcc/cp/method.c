/* Handle the hair of processing (but not expanding) inline functions.
   Also manage function and variable name overloading.
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


/* Handle method declarations.  */
#include "config.h"
#include "system.h"
#include "tree.h"
#include "cp-tree.h"
#include "obstack.h"
#include "rtl.h"
#include "expr.h"
#include "output.h"
#include "hard-reg-set.h"
#include "flags.h"
#include "toplev.h"
#include "ggc.h"
#include "tm_p.h"

/* TREE_LIST of the current inline functions that need to be
   processed.  */
struct pending_inline *pending_inlines;

static void do_build_assign_ref PARAMS ((tree));
static void do_build_copy_constructor PARAMS ((tree));


/* Set the mangled name (DECL_ASSEMBLER_NAME) for DECL.  */

void
set_mangled_name_for_decl (decl)
     tree decl;
{
  if (processing_template_decl)
    /* There's no need to mangle the name of a template function.  */
    return;

  DECL_ASSEMBLER_NAME (decl) = mangle_decl (decl);
  return;
}

/* Given a tree_code CODE, and some arguments (at least one),
   attempt to use an overloaded operator on the arguments.

   For unary operators, only the first argument need be checked.
   For binary operators, both arguments may need to be checked.

   Member functions can convert class references to class pointers,
   for one-level deep indirection.  More than that is not supported.
   Operators [](), ()(), and ->() must be member functions.

   We call function call building calls with LOOKUP_COMPLAIN if they
   are our only hope.  This is true when we see a vanilla operator
   applied to something of aggregate type.  If this fails, we are free
   to return `error_mark_node', because we will have reported the
   error.

   Operators NEW and DELETE overload in funny ways: operator new takes
   a single `size' parameter, and operator delete takes a pointer to the
   storage being deleted.  When overloading these operators, success is
   assumed.  If there is a failure, report an error message and return
   `error_mark_node'.  */

/* NOSTRICT */
tree
build_opfncall (code, flags, xarg1, xarg2, arg3)
     enum tree_code code;
     int flags;
     tree xarg1, xarg2, arg3;
{
  return build_new_op (code, flags, xarg1, xarg2, arg3);
}

/* This function takes an identifier, ID, and attempts to figure out what
   it means. There are a number of possible scenarios, presented in increasing
   order of hair:

   1) not in a class's scope
   2) in class's scope, member name of the class's method
   3) in class's scope, but not a member name of the class
   4) in class's scope, member name of a class's variable

   NAME is $1 from the bison rule. It is an IDENTIFIER_NODE.
   VALUE is $$ from the bison rule. It is the value returned by lookup_name ($1)

   As a last ditch, try to look up the name as a label and return that
   address.

   Values which are declared as being of REFERENCE_TYPE are
   automatically dereferenced here (as a hack to make the
   compiler faster).  */

tree
hack_identifier (value, name)
     tree value, name;
{
  tree type;

  if (value == error_mark_node)
    {
      if (current_class_name)
	{
	  tree fields = lookup_fnfields (TYPE_BINFO (current_class_type), name, 1);
	  if (fields == error_mark_node)
	    return error_mark_node;
	  if (fields)
	    {
	      tree fndecl;

	      fndecl = TREE_VALUE (fields);
	      my_friendly_assert (TREE_CODE (fndecl) == FUNCTION_DECL, 251);
	      /* I could not trigger this code. MvL */
	      my_friendly_abort (980325);
#ifdef DEAD
	      if (DECL_CHAIN (fndecl) == NULL_TREE)
		{
		  warning ("methods cannot be converted to function pointers");
		  return fndecl;
		}
	      else
		{
		  error ("ambiguous request for method pointer `%s'",
			 IDENTIFIER_POINTER (name));
		  return error_mark_node;
		}
#endif
	    }
	}
      if (flag_labels_ok && IDENTIFIER_LABEL_VALUE (name))
	{
	  return IDENTIFIER_LABEL_VALUE (name);
	}
      return error_mark_node;
    }

  type = TREE_TYPE (value);
  if (TREE_CODE (value) == FIELD_DECL)
    {
      if (current_class_ptr == NULL_TREE)
	{
	  if (current_function_decl 
	      && DECL_STATIC_FUNCTION_P (current_function_decl))
	    cp_error ("invalid use of member `%D' in static member function",
		      value);
	  else
	    /* We can get here when processing a bad default
	       argument, like:
	         struct S { int a; void f(int i = a); }  */
	    cp_error ("invalid use of member `%D'", value);

	  return error_mark_node;
	}
      TREE_USED (current_class_ptr) = 1;

      /* Mark so that if we are in a constructor, and then find that
	 this field was initialized by a base initializer,
	 we can emit an error message.  */
      TREE_USED (value) = 1;
      value = build_component_ref (current_class_ref, name, NULL_TREE, 1);
    }
  else if ((TREE_CODE (value) == FUNCTION_DECL
	    && DECL_FUNCTION_MEMBER_P (value))
	   || (TREE_CODE (value) == OVERLOAD
	       && DECL_FUNCTION_MEMBER_P (OVL_CURRENT (value))))
    {
      tree decl;

      if (TREE_CODE (value) == OVERLOAD)
	value = OVL_CURRENT (value);

      decl = maybe_dummy_object (DECL_CONTEXT (value), 0);
      value = build_component_ref (decl, name, NULL_TREE, 1);
    }
  else if (really_overloaded_fn (value))
    ;
  else if (TREE_CODE (value) == OVERLOAD)
    /* not really overloaded function */
    mark_used (OVL_FUNCTION (value));
  else if (TREE_CODE (value) == TREE_LIST)
    {
      /* Ambiguous reference to base members, possibly other cases?.  */
      tree t = value;
      while (t && TREE_CODE (t) == TREE_LIST)
	{
	  mark_used (TREE_VALUE (t));
	  t = TREE_CHAIN (t);
	}
    }
  else if (TREE_CODE (value) == NAMESPACE_DECL)
    {
      cp_error ("use of namespace `%D' as expression", value);
      return error_mark_node;
    }
  else if (DECL_CLASS_TEMPLATE_P (value))
    {
      cp_error ("use of class template `%T' as expression", value);
      return error_mark_node;
    }
  else
    mark_used (value);

  if (TREE_CODE (value) == VAR_DECL || TREE_CODE (value) == PARM_DECL
      || TREE_CODE (value) == RESULT_DECL)
    {
      tree context = decl_function_context (value);
      if (context != NULL_TREE && context != current_function_decl
	  && ! TREE_STATIC (value))
	{
	  cp_error ("use of %s from containing function",
		      (TREE_CODE (value) == VAR_DECL
		       ? "`auto' variable" : "parameter"));
	  cp_error_at ("  `%#D' declared here", value);
	  value = error_mark_node;
	}
    }

  if (DECL_P (value) && DECL_NONLOCAL (value))
    {
      if (DECL_CLASS_SCOPE_P (value)
	  && DECL_CONTEXT (value) != current_class_type)
	{
	  tree path;
	  path = currently_open_derived_class (DECL_CONTEXT (value));
	  enforce_access (path, value);
	}
    }
  else if (TREE_CODE (value) == TREE_LIST 
	   && TREE_TYPE (value) == error_mark_node)
    {
      error ("request for member `%s' is ambiguous in multiple inheritance lattice",
	     IDENTIFIER_POINTER (name));
      print_candidates (value);
      return error_mark_node;
    }

  if (! processing_template_decl)
    value = convert_from_reference (value);
  return value;
}


/* Return a thunk to FUNCTION.  For a virtual thunk, DELTA is the
   offset to this used to locate the vptr, and VCALL_INDEX is used to
   look up the eventual subobject location.  For a non-virtual thunk,
   DELTA is the offset to this and VCALL_INDEX is zero.  */

tree
make_thunk (function, delta, vcall_index)
     tree function;
     int delta;
     int vcall_index;
{
  tree thunk_id;
  tree thunk;
  tree func_decl;
  int vcall_offset = vcall_index * int_size_in_bytes (vtable_entry_type);

  if (TREE_CODE (function) != ADDR_EXPR)
    abort ();
  func_decl = TREE_OPERAND (function, 0);
  if (TREE_CODE (func_decl) != FUNCTION_DECL)
    abort ();

  thunk_id = mangle_thunk (TREE_OPERAND (function, 0),  delta, vcall_offset);
  thunk = IDENTIFIER_GLOBAL_VALUE (thunk_id);
  if (thunk && !DECL_THUNK_P (thunk))
    {
      cp_error ("implementation-reserved name `%D' used", thunk_id);
      thunk = NULL_TREE;
      SET_IDENTIFIER_GLOBAL_VALUE (thunk_id, thunk);
    }
  if (thunk == NULL_TREE)
    {
      thunk = build_decl (FUNCTION_DECL, thunk_id, TREE_TYPE (func_decl));
      DECL_LANG_SPECIFIC (thunk) = DECL_LANG_SPECIFIC (func_decl);
      copy_lang_decl (func_decl);
      DECL_CONTEXT (thunk) = DECL_CONTEXT (func_decl);
      TREE_READONLY (thunk) = TREE_READONLY (func_decl);
      TREE_THIS_VOLATILE (thunk) = TREE_THIS_VOLATILE (func_decl);
      comdat_linkage (thunk);
      SET_DECL_THUNK_P (thunk);
      DECL_INITIAL (thunk) = function;
      THUNK_DELTA (thunk) = delta;
      THUNK_VCALL_OFFSET (thunk) = vcall_offset;
      /* The thunk itself is not a constructor or destructor, even if
       the thing it is thunking to is.  */
      DECL_INTERFACE_KNOWN (thunk) = 1;
      DECL_NOT_REALLY_EXTERN (thunk) = 1;
      DECL_SAVED_FUNCTION_DATA (thunk) = NULL;
      DECL_DESTRUCTOR_P (thunk) = 0;
      DECL_CONSTRUCTOR_P (thunk) = 0;
      DECL_EXTERNAL (thunk) = 1;
      DECL_ARTIFICIAL (thunk) = 1;
      DECL_VTT_PARM (thunk) = NULL_TREE;
      /* Even if this thunk is a member of a local class, we don't
	 need a static chain.  */
      DECL_NO_STATIC_CHAIN (thunk) = 1;
      /* The THUNK is not a pending inline, even if the FUNC_DECL is.  */
      DECL_PENDING_INLINE_P (thunk) = 0;
      /* Nor has it been deferred.  */
      DECL_DEFERRED_FN (thunk) = 0;
      /* So that finish_file can write out any thunks that need to be: */
      pushdecl_top_level (thunk);
    }
  return thunk;
}

/* Emit the definition of a C++ multiple inheritance vtable thunk.  */

void
emit_thunk (thunk_fndecl)
     tree thunk_fndecl;
{
  tree fnaddr;
  tree function;
  int delta;
  int vcall_offset;

  if (TREE_ASM_WRITTEN (thunk_fndecl))
    return;

  if (TREE_CODE (DECL_INITIAL (thunk_fndecl)) != ADDR_EXPR)
    /* We already turned this thunk into an ordinary function.
       There's no need to process this thunk again.  (We can't just
       clear DECL_THUNK_P because that will confuse
       FNADDR_FROM_VTABLE_ENTRY and friends.)  */
    return;

  fnaddr = DECL_INITIAL (thunk_fndecl);
  function = TREE_OPERAND (fnaddr, 0);
  delta = THUNK_DELTA (thunk_fndecl);
  vcall_offset = THUNK_VCALL_OFFSET (thunk_fndecl);

  TREE_ADDRESSABLE (function) = 1;
  mark_used (function);

  if (current_function_decl)
    abort ();

  if (flag_syntax_only)
    {
      TREE_ASM_WRITTEN (thunk_fndecl) = 1;
      return;
    }

#ifdef ASM_OUTPUT_MI_THUNK
  if (vcall_offset == 0)
    {
      const char *fnname;
      current_function_decl = thunk_fndecl;
      /* Make sure we build up its RTL before we go onto the
	 temporary obstack.  */
      make_function_rtl (thunk_fndecl);
      DECL_RESULT (thunk_fndecl)
	= build_decl (RESULT_DECL, 0, integer_type_node);
      fnname = XSTR (XEXP (DECL_RTL (thunk_fndecl), 0), 0);
      init_function_start (thunk_fndecl, input_filename, lineno);
      current_function_is_thunk = 1;
      assemble_start_function (thunk_fndecl, fnname);
      ASM_OUTPUT_MI_THUNK (asm_out_file, thunk_fndecl, delta, function);
      assemble_end_function (thunk_fndecl, fnname);
      current_function_decl = 0;
      cfun = 0;
      TREE_ASM_WRITTEN (thunk_fndecl) = 1;
    }
  else
#endif /* ASM_OUTPUT_MI_THUNK */
  {
  /* If we don't have the necessary macro for efficient thunks, generate a
     thunk function that just makes a call to the real function.
     Unfortunately, this doesn't work for varargs.  */

    tree a, t;

    if (varargs_function_p (function))
      cp_error ("generic thunk code fails for method `%#D' which uses `...'",
		function);

    /* Set up clone argument trees for the thunk.  */
    t = NULL_TREE;
    for (a = DECL_ARGUMENTS (function); a; a = TREE_CHAIN (a))
      {
	tree x = copy_node (a);
	TREE_CHAIN (x) = t;
	DECL_CONTEXT (x) = thunk_fndecl;
	t = x;
      }
    a = nreverse (t);
    DECL_ARGUMENTS (thunk_fndecl) = a;
    DECL_RESULT (thunk_fndecl) = NULL_TREE;

    push_to_top_level ();
    start_function (NULL_TREE, thunk_fndecl, NULL_TREE, SF_PRE_PARSED);
    store_parm_decls ();

    /* Adjust the this pointer by the constant.  */
    t = ssize_int (delta);
    TREE_TYPE (t) = signed_type (sizetype);
    t = fold (build (PLUS_EXPR, TREE_TYPE (a), a, t));
    /* If there's a vcall offset, look up that value in the vtable and
       adjust the `this' pointer again.  */
    if (vcall_offset != 0)
      {
	tree orig_this;

	t = save_expr (t);
	orig_this = t;
	/* The vptr is always at offset zero in the object.  */
	t = build1 (NOP_EXPR,
		    build_pointer_type (build_pointer_type 
					(vtable_entry_type)),
		    t);
	/* Form the vtable address.  */
	t = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (t)), t);
	/* Find the entry with the vcall offset.  */
	t = build (PLUS_EXPR, TREE_TYPE (t), t, ssize_int (vcall_offset));
	/* Calculate the offset itself.  */
	t = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (t)), t);
	/* Adjust the `this' pointer.  */
	t = fold (build (PLUS_EXPR,
			 TREE_TYPE (orig_this),
			 orig_this,
			 t));
      }

    /* Build up the call to the real function.  */
    t = tree_cons (NULL_TREE, t, NULL_TREE);
    for (a = TREE_CHAIN (a); a; a = TREE_CHAIN (a))
      t = tree_cons (NULL_TREE, a, t);
    t = nreverse (t);
    t = build_call (function, t);
    if (!same_type_p (TREE_TYPE (t), void_type_node))
      finish_return_stmt (t);
    else
      finish_expr_stmt (t);

    /* The back-end expects DECL_INITIAL to contain a BLOCK, so we
       create one.  */
    DECL_INITIAL (thunk_fndecl) = make_node (BLOCK);
    BLOCK_VARS (DECL_INITIAL (thunk_fndecl)) 
      = DECL_ARGUMENTS (thunk_fndecl);
    expand_body (finish_function (0));
    pop_from_top_level ();
  }
}

/* Code for synthesizing methods which have default semantics defined.  */

/* Generate code for default X(X&) constructor.  */

static void
do_build_copy_constructor (fndecl)
     tree fndecl;
{
  tree parm = TREE_CHAIN (DECL_ARGUMENTS (fndecl));
  tree t;

  if (DECL_HAS_IN_CHARGE_PARM_P (fndecl))
    parm = TREE_CHAIN (parm);
  parm = convert_from_reference (parm);

  if (TYPE_HAS_TRIVIAL_INIT_REF (current_class_type)
      && is_empty_class (current_class_type))
    /* Don't copy the padding byte; it might not have been allocated
       if *this is a base subobject.  */;
  else if (TYPE_HAS_TRIVIAL_INIT_REF (current_class_type))
    {
      t = build (INIT_EXPR, void_type_node, current_class_ref, parm);
      finish_expr_stmt (t);
    }
  else
    {
      tree fields = TYPE_FIELDS (current_class_type);
      int n_bases = CLASSTYPE_N_BASECLASSES (current_class_type);
      tree binfos = TYPE_BINFO_BASETYPES (current_class_type);
      int i;

      /* Initialize all the base-classes.  */
      for (t = CLASSTYPE_VBASECLASSES (current_class_type); t;
	   t = TREE_CHAIN (t))
	current_base_init_list 
	  = tree_cons (BINFO_TYPE (TREE_VALUE (t)), parm, 
		       current_base_init_list);
      for (i = 0; i < n_bases; ++i)
	{
	  t = TREE_VEC_ELT (binfos, i);
	  if (TREE_VIA_VIRTUAL (t))
	    continue; 

	  current_base_init_list 
	    = tree_cons (BINFO_TYPE (t), parm, current_base_init_list);
	}

      for (; fields; fields = TREE_CHAIN (fields))
	{
	  tree init, t;
	  tree field = fields;

	  if (TREE_CODE (field) != FIELD_DECL)
	    continue;

	  init = parm;
	  if (DECL_NAME (field))
	    {
	      if (VFIELD_NAME_P (DECL_NAME (field)))
		continue;
	      if (VBASE_NAME_P (DECL_NAME (field)))
		continue;

	      /* True for duplicate members.  */
	      if (IDENTIFIER_CLASS_VALUE (DECL_NAME (field)) != field)
		continue;
	    }
	  else if ((t = TREE_TYPE (field)) != NULL_TREE
		   && ANON_AGGR_TYPE_P (t)
		   && TYPE_FIELDS (t) != NULL_TREE)
	    /* Just use the field; anonymous types can't have
	       nontrivial copy ctors or assignment ops.  */;
	  else
	    continue;

	  init = build (COMPONENT_REF, TREE_TYPE (field), init, field);
	  init = build_tree_list (NULL_TREE, init);

	  current_member_init_list
	    = tree_cons (field, init, current_member_init_list);
	}
      current_member_init_list = nreverse (current_member_init_list);
      current_base_init_list = nreverse (current_base_init_list);
      setup_vtbl_ptr ();
    }
}

static void
do_build_assign_ref (fndecl)
     tree fndecl;
{
  tree parm = TREE_CHAIN (DECL_ARGUMENTS (fndecl));
  tree compound_stmt;

  compound_stmt = begin_compound_stmt (/*has_no_scope=*/0);
  parm = convert_from_reference (parm);

  if (TYPE_HAS_TRIVIAL_ASSIGN_REF (current_class_type)
      && is_empty_class (current_class_type))
    /* Don't copy the padding byte; it might not have been allocated
       if *this is a base subobject.  */;
  else if (TYPE_HAS_TRIVIAL_ASSIGN_REF (current_class_type))
    {
      tree t = build (MODIFY_EXPR, void_type_node, current_class_ref, parm);
      finish_expr_stmt (t);
    }
  else
    {
      tree fields = TYPE_FIELDS (current_class_type);
      int n_bases = CLASSTYPE_N_BASECLASSES (current_class_type);
      tree binfos = TYPE_BINFO_BASETYPES (current_class_type);
      int i;

      for (i = 0; i < n_bases; ++i)
	{
	  tree basetype = BINFO_TYPE (TREE_VEC_ELT (binfos, i));
	  tree p = convert_to_reference
	    (build_reference_type (basetype), parm,
	     CONV_IMPLICIT|CONV_CONST, LOOKUP_COMPLAIN, NULL_TREE);
	  p = convert_from_reference (p);
	  p = build_member_call (basetype, ansi_assopname (NOP_EXPR),
				 build_tree_list (NULL_TREE, p));
	  finish_expr_stmt (p);
	}
      for (; fields; fields = TREE_CHAIN (fields))
	{
	  tree comp, init, t;
	  tree field = fields;

	  if (TREE_CODE (field) != FIELD_DECL)
	    continue;

	  if (CP_TYPE_CONST_P (TREE_TYPE (field)))
	    {
	      if (DECL_NAME (field))
		cp_error ("non-static const member `%#D', can't use default assignment operator", field);
	      else
		cp_error ("non-static const member in type `%T', can't use default assignment operator", current_class_type);
	      continue;
	    }
	  else if (TREE_CODE (TREE_TYPE (field)) == REFERENCE_TYPE)
	    {
	      if (DECL_NAME (field))
		cp_error ("non-static reference member `%#D', can't use default assignment operator", field);
	      else
		cp_error ("non-static reference member in type `%T', can't use default assignment operator", current_class_type);
	      continue;
	    }

	  comp = current_class_ref;
	  init = parm;

	  if (DECL_NAME (field))
	    {
	      if (VFIELD_NAME_P (DECL_NAME (field)))
		continue;
	      if (VBASE_NAME_P (DECL_NAME (field)))
		continue;

	      /* True for duplicate members.  */
	      if (IDENTIFIER_CLASS_VALUE (DECL_NAME (field)) != field)
		continue;
	    }
	  else if ((t = TREE_TYPE (field)) != NULL_TREE
		   && ANON_AGGR_TYPE_P (t)
		   && TYPE_FIELDS (t) != NULL_TREE)
	    /* Just use the field; anonymous types can't have
	       nontrivial copy ctors or assignment ops.  */;
	  else
	    continue;

	  comp = build (COMPONENT_REF, TREE_TYPE (field), comp, field);
	  init = build (COMPONENT_REF, TREE_TYPE (field), init, field);

	  finish_expr_stmt (build_modify_expr (comp, NOP_EXPR, init));
	}
    }
  finish_return_stmt (current_class_ref);
  finish_compound_stmt (/*has_no_scope=*/0, compound_stmt);
}

void
synthesize_method (fndecl)
     tree fndecl;
{
  int nested = (current_function_decl != NULL_TREE);
  tree context = decl_function_context (fndecl);
  int need_body = 1;

  if (at_eof)
    import_export_decl (fndecl);

  /* If we've been asked to synthesize a clone, just synthesize the
     cloned function instead.  Doing so will automatically fill in the
     body for the clone.  */
  if (DECL_CLONED_FUNCTION_P (fndecl))
    {
      synthesize_method (DECL_CLONED_FUNCTION (fndecl));
      return;
    }

  if (! context)
    push_to_top_level ();
  else if (nested)
    push_function_context_to (context);

  /* Put the function definition at the position where it is needed,
     rather than within the body of the class.  That way, an error
     during the generation of the implicit body points at the place
     where the attempt to generate the function occurs, giving the
     user a hint as to why we are attempting to generate the
     function. */
  DECL_SOURCE_LINE (fndecl) = lineno;
  DECL_SOURCE_FILE (fndecl) = input_filename;

  interface_unknown = 1;
  start_function (NULL_TREE, fndecl, NULL_TREE, SF_DEFAULT | SF_PRE_PARSED);
  store_parm_decls ();
  clear_last_expr ();

  if (DECL_OVERLOADED_OPERATOR_P (fndecl) == NOP_EXPR)
    {
      do_build_assign_ref (fndecl);
      need_body = 0;
    }
  else if (DECL_DESTRUCTOR_P (fndecl))
    setup_vtbl_ptr ();
  else
    {
      tree arg_chain = FUNCTION_ARG_CHAIN (fndecl);
      if (DECL_HAS_IN_CHARGE_PARM_P (fndecl))
	arg_chain = TREE_CHAIN (arg_chain);
      if (arg_chain != void_list_node)
	do_build_copy_constructor (fndecl);
      else if (TYPE_NEEDS_CONSTRUCTING (current_class_type))
	setup_vtbl_ptr ();
    }

  /* If we haven't yet generated the body of the function, just
     generate an empty compound statement.  */
  if (need_body)
    {
      tree compound_stmt;
      compound_stmt = begin_compound_stmt (/*has_no_scope=*/0);
      finish_compound_stmt (/*has_no_scope=*/0, compound_stmt);
    }

  expand_body (finish_function (0));

  extract_interface_info ();
  if (! context)
    pop_from_top_level ();
  else if (nested)
    pop_function_context_from (context);
}

/* Implicitly declare the special function indicated by KIND, as a
   member of TYPE.  For copy constructors and assignment operators,
   CONST_P indicates whether these functions should take a const
   reference argument or a non-const reference.  */

tree
implicitly_declare_fn (kind, type, const_p)
     special_function_kind kind;
     tree type;
     int const_p;
{
  tree declspecs = NULL_TREE;
  tree fn, args = NULL_TREE;
  tree argtype;
  int retref = 0;
  tree name = constructor_name (TYPE_IDENTIFIER (type));

  switch (kind)
    {
      /* Destructors.  */
    case sfk_destructor:
      name = build_parse_node (BIT_NOT_EXPR, name);
      args = void_list_node;
      break;

    case sfk_constructor:
      /* Default constructor.  */
      args = void_list_node;
      break;

    case sfk_copy_constructor:
      if (const_p)
	type = build_qualified_type (type, TYPE_QUAL_CONST);
      argtype = build_reference_type (type);
      args = tree_cons (NULL_TREE,
			build_tree_list (hash_tree_chain (argtype, NULL_TREE),
					 get_identifier ("_ctor_arg")),
			void_list_node);
      break;

    case sfk_assignment_operator:
      retref = 1;
      declspecs = build_decl_list (NULL_TREE, type);

      if (const_p)
	type = build_qualified_type (type, TYPE_QUAL_CONST);

      name = ansi_assopname (NOP_EXPR);

      argtype = build_reference_type (type);
      args = tree_cons (NULL_TREE,
			build_tree_list (hash_tree_chain (argtype, NULL_TREE),
					 get_identifier ("_ctor_arg")),
			void_list_node);
      break;

    default:
      my_friendly_abort (59);
    }

  TREE_PARMLIST (args) = 1;

  {
    tree declarator = make_call_declarator (name, args, NULL_TREE, NULL_TREE);
    if (retref)
      declarator = build_parse_node (ADDR_EXPR, declarator);

    fn = grokfield (declarator, declspecs, NULL_TREE, NULL_TREE, NULL_TREE);
  }

  my_friendly_assert (TREE_CODE (fn) == FUNCTION_DECL, 20000408);

  if (kind != sfk_constructor && kind != sfk_destructor)
    SET_DECL_ARTIFICIAL (TREE_CHAIN (DECL_ARGUMENTS (fn)));
  SET_DECL_ARTIFICIAL (fn);
  DECL_NOT_REALLY_EXTERN (fn) = 1;
  DECL_THIS_INLINE (fn) = 1;
  DECL_INLINE (fn) = 1;
  defer_fn (fn);
  
  return fn;
}
