/* Process declarations and variables for C++ compiler.
   Copyright (C) 1988, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
   Hacked by Michael Tiemann (tiemann@cygnus.com)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


/* Process declarations and symbol lookup for C++ front end.
   Also constructs types; the standard scalar types at initialization,
   and structure, union, array and enum types when they are declared.  */

/* ??? not all decl nodes are given the most useful possible
   line numbers.  For example, the CONST_DECLs for enum values.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "expr.h"
#include "flags.h"
#include "cp-tree.h"
#include "decl.h"
#include "lex.h"
#include "output.h"
#include "except.h"
#include "toplev.h"
#include "timevar.h"
#include "cpplib.h"
#include "target.h"
#include "c-common.h"
#include "cgraph.h"
#include "tree-inline.h"
extern cpp_reader *parse_in;

/* This structure contains information about the initializations
   and/or destructions required for a particular priority level.  */
typedef struct priority_info_s {
  /* Nonzero if there have been any initializations at this priority
     throughout the translation unit.  */
  int initializations_p;
  /* Nonzero if there have been any destructions at this priority
     throughout the translation unit.  */
  int destructions_p;
} *priority_info;

static void mark_vtable_entries (tree);
static void grok_function_init (tree, tree);
static bool maybe_emit_vtables (tree);
static tree build_anon_union_vars (tree);
static bool acceptable_java_type (tree);
static tree start_objects (int, int);
static void finish_objects (int, int, tree);
static tree start_static_storage_duration_function (unsigned);
static void finish_static_storage_duration_function (tree);
static priority_info get_priority_info (int);
static void do_static_initialization (tree, tree);
static void do_static_destruction (tree);
static tree start_static_initialization_or_destruction (tree, int);
static void finish_static_initialization_or_destruction (tree);
static void generate_ctor_or_dtor_function (bool, int, location_t *);
static int generate_ctor_and_dtor_functions_for_priority (splay_tree_node,
                                                          void *);
static tree prune_vars_needing_no_initialization (tree *);
static void write_out_vars (tree);
static void import_export_class (tree);
static tree get_guard_bits (tree);

/* A list of static class variables.  This is needed, because a
   static class variable can be declared inside the class without
   an initializer, and then initialized, statically, outside the class.  */
static GTY(()) varray_type pending_statics;
#define pending_statics_used \
  (pending_statics ? pending_statics->elements_used : 0)

/* A list of functions which were declared inline, but which we
   may need to emit outline anyway.  */
static GTY(()) varray_type deferred_fns;
#define deferred_fns_used \
  (deferred_fns ? deferred_fns->elements_used : 0)

/* Flag used when debugging spew.c */

extern int spew_debug;

/* Nonzero if we're done parsing and into end-of-file activities.  */

int at_eof;

/* Functions called along with real static constructors and destructors.  */

tree static_ctors;
tree static_dtors;


/* Incorporate `const' and `volatile' qualifiers for member functions.
   FUNCTION is a TYPE_DECL or a FUNCTION_DECL.
   QUALS is a list of qualifiers.  Returns any explicit
   top-level qualifiers of the method's this pointer, anything other than
   TYPE_UNQUALIFIED will be an extension.  */

int
grok_method_quals (tree ctype, tree function, tree quals)
{
  tree fntype = TREE_TYPE (function);
  tree raises = TYPE_RAISES_EXCEPTIONS (fntype);
  int type_quals = TYPE_UNQUALIFIED;
  int dup_quals = TYPE_UNQUALIFIED;
  int this_quals = TYPE_UNQUALIFIED;

  do
    {
      int tq = cp_type_qual_from_rid (TREE_VALUE (quals));
      
      if ((type_quals | this_quals) & tq)
	dup_quals |= tq;
      else if (tq & TYPE_QUAL_RESTRICT)
        this_quals |= tq;
      else
	type_quals |= tq;
      quals = TREE_CHAIN (quals);
    } 
  while (quals);

  if (dup_quals != TYPE_UNQUALIFIED)
    error ("duplicate type qualifiers in %s declaration",
	      TREE_CODE (function) == FUNCTION_DECL 
	      ? "member function" : "type");

  ctype = cp_build_qualified_type (ctype, type_quals);
  fntype = build_method_type_directly (ctype, TREE_TYPE (fntype),
				       (TREE_CODE (fntype) == METHOD_TYPE
					? TREE_CHAIN (TYPE_ARG_TYPES (fntype))
					: TYPE_ARG_TYPES (fntype)));
  if (raises)
    fntype = build_exception_variant (fntype, raises);

  TREE_TYPE (function) = fntype;
  return this_quals;
}

/* A subroutine of the parser, to handle a component list.  */

void
grok_x_components (tree specs)
{
  tree t;

  specs = strip_attrs (specs);

  check_tag_decl (specs);
  t = groktypename (build_tree_list (specs, NULL_TREE)); 

  /* The only case where we need to do anything additional here is an
     anonymous union field, e.g.: `struct S { union { int i; }; };'.  */
  if (t == NULL_TREE || !ANON_AGGR_TYPE_P (t))
    return;

  fixup_anonymous_aggr (t);
  finish_member_declaration (build_decl (FIELD_DECL, NULL_TREE, t)); 
}

/* Build a PARM_DECL with NAME and TYPE, and set DECL_ARG_TYPE
   appropriately.  */

tree
cp_build_parm_decl (tree name, tree type)
{
  tree parm = build_decl (PARM_DECL, name, type);
  /* DECL_ARG_TYPE is only used by the back end and the back end never
     sees templates.  */
  if (!processing_template_decl)
    DECL_ARG_TYPE (parm) = type_passed_as (type);
  return parm;
}

/* Returns a PARM_DECL for a parameter of the indicated TYPE, with the
   indicated NAME.  */

tree
build_artificial_parm (tree name, tree type)
{
  tree parm = cp_build_parm_decl (name, type);
  DECL_ARTIFICIAL (parm) = 1;
  /* All our artificial parms are implicitly `const'; they cannot be
     assigned to.  */
  TREE_READONLY (parm) = 1;
  return parm;
}

/* Constructors for types with virtual baseclasses need an "in-charge" flag
   saying whether this constructor is responsible for initialization of
   virtual baseclasses or not.  All destructors also need this "in-charge"
   flag, which additionally determines whether or not the destructor should
   free the memory for the object.

   This function adds the "in-charge" flag to member function FN if
   appropriate.  It is called from grokclassfn and tsubst.
   FN must be either a constructor or destructor.

   The in-charge flag follows the 'this' parameter, and is followed by the
   VTT parm (if any), then the user-written parms.  */

void
maybe_retrofit_in_chrg (tree fn)
{
  tree basetype, arg_types, parms, parm, fntype;

  /* If we've already add the in-charge parameter don't do it again.  */
  if (DECL_HAS_IN_CHARGE_PARM_P (fn))
    return;

  /* When processing templates we can't know, in general, whether or
     not we're going to have virtual baseclasses.  */
  if (processing_template_decl)
    return;

  /* We don't need an in-charge parameter for constructors that don't
     have virtual bases.  */
  if (DECL_CONSTRUCTOR_P (fn)
      && !TYPE_USES_VIRTUAL_BASECLASSES (DECL_CONTEXT (fn)))
    return;

  arg_types = TYPE_ARG_TYPES (TREE_TYPE (fn));
  basetype = TREE_TYPE (TREE_VALUE (arg_types));
  arg_types = TREE_CHAIN (arg_types);

  parms = TREE_CHAIN (DECL_ARGUMENTS (fn));

  /* If this is a subobject constructor or destructor, our caller will
     pass us a pointer to our VTT.  */
  if (TYPE_USES_VIRTUAL_BASECLASSES (DECL_CONTEXT (fn)))
    {
      parm = build_artificial_parm (vtt_parm_identifier, vtt_parm_type);

      /* First add it to DECL_ARGUMENTS between 'this' and the real args...  */
      TREE_CHAIN (parm) = parms;
      parms = parm;

      /* ...and then to TYPE_ARG_TYPES.  */
      arg_types = hash_tree_chain (vtt_parm_type, arg_types);

      DECL_HAS_VTT_PARM_P (fn) = 1;
    }

  /* Then add the in-charge parm (before the VTT parm).  */
  parm = build_artificial_parm (in_charge_identifier, integer_type_node);
  TREE_CHAIN (parm) = parms;
  parms = parm;
  arg_types = hash_tree_chain (integer_type_node, arg_types);

  /* Insert our new parameter(s) into the list.  */
  TREE_CHAIN (DECL_ARGUMENTS (fn)) = parms;

  /* And rebuild the function type.  */
  fntype = build_method_type_directly (basetype, TREE_TYPE (TREE_TYPE (fn)),
				       arg_types);
  if (TYPE_RAISES_EXCEPTIONS (TREE_TYPE (fn)))
    fntype = build_exception_variant (fntype,
				      TYPE_RAISES_EXCEPTIONS (TREE_TYPE (fn)));
  TREE_TYPE (fn) = fntype;

  /* Now we've got the in-charge parameter.  */
  DECL_HAS_IN_CHARGE_PARM_P (fn) = 1;
}

/* Classes overload their constituent function names automatically.
   When a function name is declared in a record structure,
   its name is changed to it overloaded name.  Since names for
   constructors and destructors can conflict, we place a leading
   '$' for destructors.

   CNAME is the name of the class we are grokking for.

   FUNCTION is a FUNCTION_DECL.  It was created by `grokdeclarator'.

   FLAGS contains bits saying what's special about today's
   arguments.  1 == DESTRUCTOR.  2 == OPERATOR.

   If FUNCTION is a destructor, then we must add the `auto-delete' field
   as a second parameter.  There is some hair associated with the fact
   that we must "declare" this variable in the manner consistent with the
   way the rest of the arguments were declared.

   QUALS are the qualifiers for the this pointer.  */

void
grokclassfn (tree ctype, tree function, enum overload_flags flags, tree quals)
{
  tree fn_name = DECL_NAME (function);
  int this_quals = TYPE_UNQUALIFIED;

  /* Even within an `extern "C"' block, members get C++ linkage.  See
     [dcl.link] for details.  */
  SET_DECL_LANGUAGE (function, lang_cplusplus);

  if (fn_name == NULL_TREE)
    {
      error ("name missing for member function");
      fn_name = get_identifier ("<anonymous>");
      DECL_NAME (function) = fn_name;
    }

  if (quals)
    this_quals = grok_method_quals (ctype, function, quals);

  if (TREE_CODE (TREE_TYPE (function)) == METHOD_TYPE)
    {
      /* Must add the class instance variable up front.  */
      /* Right now we just make this a pointer.  But later
	 we may wish to make it special.  */
      tree type = TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (function)));
      tree qual_type;
      tree parm;

      /* The `this' parameter is implicitly `const'; it cannot be
	 assigned to.  */
      this_quals |= TYPE_QUAL_CONST;
      qual_type = cp_build_qualified_type (type, this_quals);
      parm = build_artificial_parm (this_identifier, qual_type);
      c_apply_type_quals_to_decl (this_quals, parm);
      TREE_CHAIN (parm) = DECL_ARGUMENTS (function);
      DECL_ARGUMENTS (function) = parm;
    }

  DECL_CONTEXT (function) = ctype;

  if (flags == DTOR_FLAG)
    DECL_DESTRUCTOR_P (function) = 1;

  if (flags == DTOR_FLAG || DECL_CONSTRUCTOR_P (function))
    maybe_retrofit_in_chrg (function);
}

/* Create an ARRAY_REF, checking for the user doing things backwards
   along the way.  */

tree
grok_array_decl (tree array_expr, tree index_exp)
{
  tree type;
  tree expr;
  tree orig_array_expr = array_expr;
  tree orig_index_exp = index_exp;

  if (error_operand_p (array_expr) || error_operand_p (index_exp))
    return error_mark_node;

  if (processing_template_decl)
    {
      if (type_dependent_expression_p (array_expr)
	  || type_dependent_expression_p (index_exp))
	return build_min_nt (ARRAY_REF, array_expr, index_exp);
      array_expr = build_non_dependent_expr (array_expr);
      index_exp = build_non_dependent_expr (index_exp);
    }

  type = TREE_TYPE (array_expr);
  my_friendly_assert (type, 20030626);
  type = non_reference (type);

  /* If they have an `operator[]', use that.  */
  if (IS_AGGR_TYPE (type) || IS_AGGR_TYPE (TREE_TYPE (index_exp)))
    expr = build_new_op (ARRAY_REF, LOOKUP_NORMAL,
			 array_expr, index_exp, NULL_TREE,
			 /*overloaded_p=*/NULL);
  else
    {
      tree p1, p2, i1, i2;

      /* Otherwise, create an ARRAY_REF for a pointer or array type.
	 It is a little-known fact that, if `a' is an array and `i' is
	 an int, you can write `i[a]', which means the same thing as
	 `a[i]'.  */
      if (TREE_CODE (type) == ARRAY_TYPE)
	p1 = array_expr;
      else
	p1 = build_expr_type_conversion (WANT_POINTER, array_expr, false);

      if (TREE_CODE (TREE_TYPE (index_exp)) == ARRAY_TYPE)
	p2 = index_exp;
      else
	p2 = build_expr_type_conversion (WANT_POINTER, index_exp, false);

      i1 = build_expr_type_conversion (WANT_INT | WANT_ENUM, array_expr, 
				       false);
      i2 = build_expr_type_conversion (WANT_INT | WANT_ENUM, index_exp, 
				       false);

      if ((p1 && i2) && (i1 && p2))
	error ("ambiguous conversion for array subscript");

      if (p1 && i2)
	array_expr = p1, index_exp = i2;
      else if (i1 && p2)
	array_expr = p2, index_exp = i1;
      else
	{
	  error ("invalid types `%T[%T]' for array subscript",
		    type, TREE_TYPE (index_exp));
	  return error_mark_node;
	}

      if (array_expr == error_mark_node || index_exp == error_mark_node)
	error ("ambiguous conversion for array subscript");

      expr = build_array_ref (array_expr, index_exp);
    }
  if (processing_template_decl && expr != error_mark_node)
    return build_min_non_dep (ARRAY_REF, expr,
			      orig_array_expr, orig_index_exp);
  return expr;
}

/* Given the cast expression EXP, checking out its validity.   Either return
   an error_mark_node if there was an unavoidable error, return a cast to
   void for trying to delete a pointer w/ the value 0, or return the
   call to delete.  If DOING_VEC is true, we handle things differently
   for doing an array delete.
   Implements ARM $5.3.4.  This is called from the parser.  */

tree
delete_sanity (tree exp, tree size, bool doing_vec, int use_global_delete)
{
  tree t, type;

  if (exp == error_mark_node)
    return exp;

  if (processing_template_decl)
    {
      t = build_min (DELETE_EXPR, void_type_node, exp, size);
      DELETE_EXPR_USE_GLOBAL (t) = use_global_delete;
      DELETE_EXPR_USE_VEC (t) = doing_vec;
      TREE_SIDE_EFFECTS (t) = 1;
      return t;
    }

  exp = convert_from_reference (exp);

  /* An array can't have been allocated by new, so complain.  */
  if (TREE_CODE (exp) == VAR_DECL
      && TREE_CODE (TREE_TYPE (exp)) == ARRAY_TYPE)
    warning ("deleting array `%#D'", exp);

  t = build_expr_type_conversion (WANT_POINTER, exp, true);

  if (t == NULL_TREE || t == error_mark_node)
    {
      error ("type `%#T' argument given to `delete', expected pointer",
		TREE_TYPE (exp));
      return error_mark_node;
    }

  type = TREE_TYPE (t);

  /* As of Valley Forge, you can delete a pointer to const.  */

  /* You can't delete functions.  */
  if (TREE_CODE (TREE_TYPE (type)) == FUNCTION_TYPE)
    {
      error ("cannot delete a function.  Only pointer-to-objects are valid arguments to `delete'");
      return error_mark_node;
    }

  /* Deleting ptr to void is undefined behavior [expr.delete/3].  */
  if (TREE_CODE (TREE_TYPE (type)) == VOID_TYPE)
    {
      warning ("deleting `%T' is undefined", type);
      doing_vec = 0;
    }

  /* Deleting a pointer with the value zero is valid and has no effect.  */
  if (integer_zerop (t))
    return build1 (NOP_EXPR, void_type_node, t);

  if (doing_vec)
    return build_vec_delete (t, /*maxindex=*/NULL_TREE, 
			     sfk_deleting_destructor,
			     use_global_delete);
  else
    return build_delete (type, t, sfk_deleting_destructor,
			 LOOKUP_NORMAL, use_global_delete);
}

/* Report an error if the indicated template declaration is not the
   sort of thing that should be a member template.  */

void
check_member_template (tree tmpl)
{
  tree decl;

  my_friendly_assert (TREE_CODE (tmpl) == TEMPLATE_DECL, 0);
  decl = DECL_TEMPLATE_RESULT (tmpl);

  if (TREE_CODE (decl) == FUNCTION_DECL
      || (TREE_CODE (decl) == TYPE_DECL
	  && IS_AGGR_TYPE (TREE_TYPE (decl))))
    {
      if (current_function_decl)
	/* 14.5.2.2 [temp.mem]
	   
	   A local class shall not have member templates.  */
	error ("invalid declaration of member template `%#D' in local class",
		  decl);
      
      if (TREE_CODE (decl) == FUNCTION_DECL && DECL_VIRTUAL_P (decl))
	{
	  /* 14.5.2.3 [temp.mem]

	     A member function template shall not be virtual.  */
	  error 
	    ("invalid use of `virtual' in template declaration of `%#D'",
	     decl);
	  DECL_VIRTUAL_P (decl) = 0;
	}

      /* The debug-information generating code doesn't know what to do
	 with member templates.  */ 
      DECL_IGNORED_P (tmpl) = 1;
    } 
  else
    error ("template declaration of `%#D'", decl);
}

/* Return true iff TYPE is a valid Java parameter or return type.  */

static bool
acceptable_java_type (tree type)
{
  if (TREE_CODE (type) == VOID_TYPE || TYPE_FOR_JAVA (type))
    return 1;
  if (TREE_CODE (type) == POINTER_TYPE || TREE_CODE (type) == REFERENCE_TYPE)
    {
      type = TREE_TYPE (type);
      if (TREE_CODE (type) == RECORD_TYPE)
	{
	  tree args;  int i;
	  if (! TYPE_FOR_JAVA (type))
	    return false;
	  if (! CLASSTYPE_TEMPLATE_INFO (type))
	    return true;
	  args = CLASSTYPE_TI_ARGS (type);
	  i = TREE_VEC_LENGTH (args);
	  while (--i >= 0)
	    {
	      type = TREE_VEC_ELT (args, i);
	      if (TREE_CODE (type) == POINTER_TYPE)
		type = TREE_TYPE (type);
	      if (! TYPE_FOR_JAVA (type))
		return false;
	    }
	  return true;
	}
    }
  return false;
}

/* For a METHOD in a Java class CTYPE, return true if
   the parameter and return types are valid Java types.
   Otherwise, print appropriate error messages, and return false.  */

bool
check_java_method (tree method)
{
  bool jerr = false;
  tree arg_types = TYPE_ARG_TYPES (TREE_TYPE (method));
  tree ret_type = TREE_TYPE (TREE_TYPE (method));

  if (!acceptable_java_type (ret_type))
    {
      error ("Java method '%D' has non-Java return type `%T'",
		method, ret_type);
      jerr = true;
    }

  arg_types = TREE_CHAIN (arg_types);
  if (DECL_HAS_IN_CHARGE_PARM_P (method))
    arg_types = TREE_CHAIN (arg_types);
  if (DECL_HAS_VTT_PARM_P (method))
    arg_types = TREE_CHAIN (arg_types);
  
  for (; arg_types != NULL_TREE; arg_types = TREE_CHAIN (arg_types))
    {
      tree type = TREE_VALUE (arg_types);
      if (!acceptable_java_type (type))
	{
	  error ("Java method '%D' has non-Java parameter type `%T'",
		    method, type);
	  jerr = true;
	}
    }
  return !jerr;
}

/* Sanity check: report error if this function FUNCTION is not
   really a member of the class (CTYPE) it is supposed to belong to.
   CNAME is the same here as it is for grokclassfn above.
   TEMPLATE_HEADER_P is true when this declaration comes with a
   template header.  */

tree
check_classfn (tree ctype, tree function, bool template_header_p)
{
  int ix;
  int is_template;
  
  if (DECL_USE_TEMPLATE (function)
      && !(TREE_CODE (function) == TEMPLATE_DECL
	   && DECL_TEMPLATE_SPECIALIZATION (function))
      && is_member_template (DECL_TI_TEMPLATE (function)))
    /* Since this is a specialization of a member template,
       we're not going to find the declaration in the class.
       For example, in:
       
         struct S { template <typename T> void f(T); };
         template <> void S::f(int);
       
       we're not going to find `S::f(int)', but there's no
       reason we should, either.  We let our callers know we didn't
       find the method, but we don't complain.  */
    return NULL_TREE;

  /* OK, is this a definition of a member template?  */
  is_template = (TREE_CODE (function) == TEMPLATE_DECL
		 || template_header_p);

  ix = lookup_fnfields_1 (complete_type (ctype),
			  DECL_CONSTRUCTOR_P (function) ? ctor_identifier :
			  DECL_DESTRUCTOR_P (function) ? dtor_identifier :
			  DECL_NAME (function));

  if (ix >= 0)
    {
      tree methods = CLASSTYPE_METHOD_VEC (ctype);
      tree fndecls, fndecl = 0;
      bool is_conv_op;
      bool pop_p;
      const char *format = NULL;
      
      pop_p = push_scope (ctype);
      for (fndecls = TREE_VEC_ELT (methods, ix);
	   fndecls; fndecls = OVL_NEXT (fndecls))
	{
	  tree p1, p2;
	  
	  fndecl = OVL_CURRENT (fndecls);
	  p1 = TYPE_ARG_TYPES (TREE_TYPE (function));
	  p2 = TYPE_ARG_TYPES (TREE_TYPE (fndecl));

	  /* We cannot simply call decls_match because this doesn't
	     work for static member functions that are pretending to
	     be methods, and because the name may have been changed by
	     asm("new_name").  */ 
	      
	   /* Get rid of the this parameter on functions that become
	      static.  */
	  if (DECL_STATIC_FUNCTION_P (fndecl)
	      && TREE_CODE (TREE_TYPE (function)) == METHOD_TYPE)
	    p1 = TREE_CHAIN (p1);

	  /* A member template definition only matches a member template
	     declaration.  */
	  if (is_template != (TREE_CODE (fndecl) == TEMPLATE_DECL))
	    continue;
	      
	  if (same_type_p (TREE_TYPE (TREE_TYPE (function)),
			   TREE_TYPE (TREE_TYPE (fndecl)))
	      && compparms (p1, p2)
	      && (DECL_TEMPLATE_SPECIALIZATION (function)
		  == DECL_TEMPLATE_SPECIALIZATION (fndecl))
	      && (!DECL_TEMPLATE_SPECIALIZATION (function)
		  || (DECL_TI_TEMPLATE (function) 
		      == DECL_TI_TEMPLATE (fndecl))))
	    break;
	}
      if (pop_p)
	pop_scope (ctype);
      if (fndecls)
	return OVL_CURRENT (fndecls);
      error ("prototype for `%#D' does not match any in class `%T'",
	     function, ctype);
      is_conv_op = DECL_CONV_FN_P (fndecl);

      if (is_conv_op)
	ix = CLASSTYPE_FIRST_CONVERSION_SLOT;
      fndecls = TREE_VEC_ELT (methods, ix);
      while (fndecls)
	{
	  fndecl = OVL_CURRENT (fndecls);
	  fndecls = OVL_NEXT (fndecls);

	  if (!fndecls && is_conv_op)
	    {
	      if (TREE_VEC_LENGTH (methods) > ix)
		{
		  ix++;
		  fndecls = TREE_VEC_ELT (methods, ix);
		  if (!DECL_CONV_FN_P (OVL_CURRENT (fndecls)))
		    {
		      fndecls = NULL_TREE;
		      is_conv_op = false;
		    }
		}
	      else
		is_conv_op = false;
	    }
	  if (format)
	    format = "                %#D";
	  else if (fndecls)
	    format = "candidates are: %#D";
	  else
	    format = "candidate is: %#D";
	  cp_error_at (format, fndecl);
	}
    }
  else if (!COMPLETE_TYPE_P (ctype))
    cxx_incomplete_type_error (function, ctype);
  else
    error ("no `%#D' member function declared in class `%T'",
	   function, ctype);

  /* If we did not find the method in the class, add it to avoid
     spurious errors (unless the CTYPE is not yet defined, in which
     case we'll only confuse ourselves when the function is declared
     properly within the class.  */
  if (COMPLETE_TYPE_P (ctype))
    add_method (ctype, function, /*error_p=*/1);
  return NULL_TREE;
}

/* We have just processed the DECL, which is a static data member.
   Its initializer, if present, is INIT.  The ASMSPEC_TREE, if
   present, is the assembly-language name for the data member.
   FLAGS is as for cp_finish_decl.  */

void
finish_static_data_member_decl (tree decl, tree init, tree asmspec_tree,
                                int flags)
{
  my_friendly_assert (TREE_PUBLIC (decl), 0);

  DECL_CONTEXT (decl) = current_class_type;

  /* We cannot call pushdecl here, because that would fill in the
     TREE_CHAIN of our decl.  Instead, we modify cp_finish_decl to do
     the right thing, namely, to put this decl out straight away.  */
  /* current_class_type can be NULL_TREE in case of error.  */
  if (!asmspec_tree && current_class_type)
    DECL_INITIAL (decl) = error_mark_node;

  if (! processing_template_decl)
    {
      if (!pending_statics)
	VARRAY_TREE_INIT (pending_statics, 32, "pending_statics");
      VARRAY_PUSH_TREE (pending_statics, decl);
    }

  if (LOCAL_CLASS_P (current_class_type))
    pedwarn ("local class `%#T' shall not have static data member `%#D'",
	     current_class_type, decl);

  /* Static consts need not be initialized in the class definition.  */
  if (init != NULL_TREE && TYPE_NEEDS_CONSTRUCTING (TREE_TYPE (decl)))
    {
      static int explained = 0;
	  
      error ("initializer invalid for static member with constructor");
      if (!explained)
        {
	  error ("(an out of class initialization is required)");
	  explained = 1;
	}
      init = NULL_TREE;
    }
  /* Force the compiler to know when an uninitialized static const
     member is being used.  */
  if (CP_TYPE_CONST_P (TREE_TYPE (decl)) && init == 0)
    TREE_USED (decl) = 1;
  DECL_INITIAL (decl) = init;
  DECL_IN_AGGR_P (decl) = 1;

  cp_finish_decl (decl, init, asmspec_tree, flags);
}

/* Process the specs, declarator (NULL if omitted) and width (NULL if omitted)
   of a structure component, returning a _DECL node.
   QUALS is a list of type qualifiers for this decl (such as for declaring
   const member functions).

   This is done during the parsing of the struct declaration.
   The _DECL nodes are chained together and the lot of them
   are ultimately passed to `build_struct' to make the RECORD_TYPE node.

   If class A defines that certain functions in class B are friends, then
   the way I have set things up, it is B who is interested in permission
   granted by A.  However, it is in A's context that these declarations
   are parsed.  By returning a void_type_node, class A does not attempt
   to incorporate the declarations of the friends within its structure.

   DO NOT MAKE ANY CHANGES TO THIS CODE WITHOUT MAKING CORRESPONDING
   CHANGES TO CODE IN `start_method'.  */

tree
grokfield (tree declarator, tree declspecs, tree init, tree asmspec_tree,
           tree attrlist)
{
  tree value;
  const char *asmspec = 0;
  int flags = LOOKUP_ONLYCONVERTING;

  if (declspecs == NULL_TREE
      && TREE_CODE (declarator) == SCOPE_REF
      && TREE_CODE (TREE_OPERAND (declarator, 1)) == IDENTIFIER_NODE)
    {
      /* Access declaration */
      if (! IS_AGGR_TYPE_CODE (TREE_CODE (TREE_OPERAND (declarator, 0))))
	;
      else if (TREE_COMPLEXITY (declarator) == current_class_depth)
	pop_nested_class ();
      return do_class_using_decl (declarator);
    }

  if (init
      && TREE_CODE (init) == TREE_LIST
      && TREE_VALUE (init) == error_mark_node
      && TREE_CHAIN (init) == NULL_TREE)
    init = NULL_TREE;

  value = grokdeclarator (declarator, declspecs, FIELD, init != 0, &attrlist);
  if (! value || error_operand_p (value))
    /* friend or constructor went bad.  */
    return error_mark_node;

  if (TREE_CODE (value) == TYPE_DECL && init)
    {
      error ("typedef `%D' is initialized (use __typeof__ instead)", value);
      init = NULL_TREE;
    }

  /* Pass friendly classes back.  */
  if (value == void_type_node)
    return value;

  /* Pass friend decls back.  */
  if ((TREE_CODE (value) == FUNCTION_DECL
       || TREE_CODE (value) == TEMPLATE_DECL)
      && DECL_CONTEXT (value) != current_class_type)
    return value;

  if (DECL_NAME (value) != NULL_TREE
      && IDENTIFIER_POINTER (DECL_NAME (value))[0] == '_'
      && ! strcmp (IDENTIFIER_POINTER (DECL_NAME (value)), "_vptr"))
    error ("member `%D' conflicts with virtual function table field name",
	      value);

  /* Stash away type declarations.  */
  if (TREE_CODE (value) == TYPE_DECL)
    {
      DECL_NONLOCAL (value) = 1;
      DECL_CONTEXT (value) = current_class_type;

      if (processing_template_decl)
	value = push_template_decl (value);

      return value;
    }

  if (DECL_IN_AGGR_P (value))
    {
      error ("`%D' is already defined in `%T'", value,
		DECL_CONTEXT (value));
      return void_type_node;
    }

  if (asmspec_tree)
    asmspec = TREE_STRING_POINTER (asmspec_tree);

  if (init)
    {
      if (TREE_CODE (value) == FUNCTION_DECL)
	{
	  grok_function_init (value, init);
	  init = NULL_TREE;
	}
      else if (pedantic && TREE_CODE (value) != VAR_DECL)
	/* Already complained in grokdeclarator.  */
	init = NULL_TREE;
      else
	{
	  /* We allow initializers to become parameters to base
             initializers.  */
	  if (TREE_CODE (init) == TREE_LIST)
	    {
	      if (TREE_CHAIN (init) == NULL_TREE)
		init = TREE_VALUE (init);
	      else
		init = digest_init (TREE_TYPE (value), init, (tree *)0);
	    }

	  if (!processing_template_decl)
	    {
	      if (TREE_CODE (init) == CONST_DECL)
		init = DECL_INITIAL (init);
	      else if (TREE_READONLY_DECL_P (init))
		init = decl_constant_value (init);
	      else if (TREE_CODE (init) == CONSTRUCTOR)
		init = digest_init (TREE_TYPE (value), init, (tree *)0);
	      if (init != error_mark_node && ! TREE_CONSTANT (init))
		{
		  /* We can allow references to things that are effectively
		     static, since references are initialized with the
		     address.  */
		  if (TREE_CODE (TREE_TYPE (value)) != REFERENCE_TYPE
		      || (TREE_STATIC (init) == 0
			  && (!DECL_P (init) || DECL_EXTERNAL (init) == 0)))
		    {
		      error ("field initializer is not constant");
		      init = error_mark_node;
		    }
		}
	    }
	}
    }

  if (processing_template_decl
      && (TREE_CODE (value) == VAR_DECL || TREE_CODE (value) == FUNCTION_DECL))
    {
      value = push_template_decl (value);
      if (error_operand_p (value))
	return error_mark_node;
    }

  if (attrlist)
    cplus_decl_attributes (&value, attrlist, 0);

  if (TREE_CODE (value) == VAR_DECL)
    {
      finish_static_data_member_decl (value, init, asmspec_tree, 
				      flags);
      return value;
    }
  if (TREE_CODE (value) == FIELD_DECL)
    {
      if (asmspec)
	error ("`asm' specifiers are not permitted on non-static data members");
      if (DECL_INITIAL (value) == error_mark_node)
	init = error_mark_node;
      cp_finish_decl (value, init, NULL_TREE, flags);
      DECL_INITIAL (value) = init;
      DECL_IN_AGGR_P (value) = 1;
      return value;
    }
  if (TREE_CODE (value) == FUNCTION_DECL)
    {
      if (asmspec)
	{
	  /* This must override the asm specifier which was placed
	     by grokclassfn.  Lay this out fresh.  */
	  SET_DECL_RTL (value, NULL_RTX);
	  change_decl_assembler_name (value, get_identifier (asmspec));
	}
      if (!DECL_FRIEND_P (value))
	grok_special_member_properties (value);
      
      cp_finish_decl (value, init, asmspec_tree, flags);

      /* Pass friends back this way.  */
      if (DECL_FRIEND_P (value))
	return void_type_node;

      DECL_IN_AGGR_P (value) = 1;
      return value;
    }
  abort ();
  /* NOTREACHED */
  return NULL_TREE;
}

/* Like `grokfield', but for bitfields.
   WIDTH is non-NULL for bit fields only, and is an INTEGER_CST node.  */

tree
grokbitfield (tree declarator, tree declspecs, tree width)
{
  tree value = grokdeclarator (declarator, declspecs, BITFIELD, 0, NULL);

  if (! value) return NULL_TREE; /* friends went bad.  */

  /* Pass friendly classes back.  */
  if (TREE_CODE (value) == VOID_TYPE)
    return void_type_node;

  if (TREE_CODE (value) == TYPE_DECL)
    {
      error ("cannot declare `%D' to be a bit-field type", value);
      return NULL_TREE;
    }

  /* Usually, finish_struct_1 catches bitfields with invalid types.
     But, in the case of bitfields with function type, we confuse
     ourselves into thinking they are member functions, so we must
     check here.  */
  if (TREE_CODE (value) == FUNCTION_DECL)
    {
      error ("cannot declare bit-field `%D' with function type",
	     DECL_NAME (value));
      return NULL_TREE;
    }

  if (DECL_IN_AGGR_P (value))
    {
      error ("`%D' is already defined in the class %T", value,
		  DECL_CONTEXT (value));
      return void_type_node;
    }

  if (TREE_STATIC (value))
    {
      error ("static member `%D' cannot be a bit-field", value);
      return NULL_TREE;
    }
  cp_finish_decl (value, NULL_TREE, NULL_TREE, 0);

  if (width != error_mark_node)
    {
      constant_expression_warning (width);
      DECL_INITIAL (value) = width;
      SET_DECL_C_BIT_FIELD (value);
    }

  DECL_IN_AGGR_P (value) = 1;
  return value;
}

/* When a function is declared with an initializer,
   do the right thing.  Currently, there are two possibilities:

   class B
   {
    public:
     // initialization possibility #1.
     virtual void f () = 0;
     int g ();
   };
   
   class D1 : B
   {
    public:
     int d1;
     // error, no f ();
   };
   
   class D2 : B
   {
    public:
     int d2;
     void f ();
   };
   
   class D3 : B
   {
    public:
     int d3;
     // initialization possibility #2
     void f () = B::f;
   };

*/

static void
grok_function_init (tree decl, tree init)
{
  /* An initializer for a function tells how this function should
     be inherited.  */
  tree type = TREE_TYPE (decl);

  if (TREE_CODE (type) == FUNCTION_TYPE)
    error ("initializer specified for non-member function `%D'", decl);
  else if (integer_zerop (init))
    DECL_PURE_VIRTUAL_P (decl) = 1;
  else
    error ("invalid initializer for virtual method `%D'", decl);
}

void
cplus_decl_attributes (tree *decl, tree attributes, int flags)
{
  if (*decl == NULL_TREE || *decl == void_type_node)
    return;

  if (TREE_CODE (*decl) == TEMPLATE_DECL)
    decl = &DECL_TEMPLATE_RESULT (*decl);

  decl_attributes (decl, attributes, flags);

  if (TREE_CODE (*decl) == TYPE_DECL)
    SET_IDENTIFIER_TYPE_VALUE (DECL_NAME (*decl), TREE_TYPE (*decl));
}

/* Defer the compilation of the FN until the end of compilation.  */

void
defer_fn (tree fn)
{
  if (DECL_DEFERRED_FN (fn))
    return;
  DECL_DEFERRED_FN (fn) = 1;
  DECL_DEFER_OUTPUT (fn) = 1;
  if (!deferred_fns)
    VARRAY_TREE_INIT (deferred_fns, 32, "deferred_fns");

  VARRAY_PUSH_TREE (deferred_fns, fn);
}

/* Walks through the namespace- or function-scope anonymous union OBJECT,
   building appropriate ALIAS_DECLs.  Returns one of the fields for use in
   the mangled name.  */

static tree
build_anon_union_vars (tree object)
{
  tree type = TREE_TYPE (object);
  tree main_decl = NULL_TREE;
  tree field;

  /* Rather than write the code to handle the non-union case,
     just give an error.  */
  if (TREE_CODE (type) != UNION_TYPE)
    error ("anonymous struct not inside named type");

  for (field = TYPE_FIELDS (type); 
       field != NULL_TREE; 
       field = TREE_CHAIN (field))
    {
      tree decl;
      tree ref;

      if (DECL_ARTIFICIAL (field))
	continue;
      if (TREE_CODE (field) != FIELD_DECL)
	{
	  cp_pedwarn_at ("\
`%#D' invalid; an anonymous union can only have non-static data members",
			 field);
	  continue;
	}

      if (TREE_PRIVATE (field))
	cp_pedwarn_at ("private member `%#D' in anonymous union", field);
      else if (TREE_PROTECTED (field))
	cp_pedwarn_at ("protected member `%#D' in anonymous union", field);

      if (processing_template_decl)
	ref = build_min_nt (COMPONENT_REF, object, DECL_NAME (field));
      else
	ref = build_class_member_access_expr (object, field, NULL_TREE,
					      false);

      if (DECL_NAME (field))
	{
	  decl = build_decl (ALIAS_DECL, DECL_NAME (field), TREE_TYPE (field));
	  DECL_INITIAL (decl) = ref;	    
	  TREE_PUBLIC (decl) = 0;
	  TREE_STATIC (decl) = 0;
	  DECL_EXTERNAL (decl) = 1;
	  decl = pushdecl (decl);
	}
      else if (ANON_AGGR_TYPE_P (TREE_TYPE (field)))
	decl = build_anon_union_vars (ref);
      else
	decl = 0;

      if (main_decl == NULL_TREE)
	main_decl = decl;
    }

  return main_decl;
}

/* Finish off the processing of a UNION_TYPE structure.  If the union is an
   anonymous union, then all members must be laid out together.  PUBLIC_P
   is nonzero if this union is not declared static.  */

void
finish_anon_union (tree anon_union_decl)
{
  tree type = TREE_TYPE (anon_union_decl);
  tree main_decl;
  bool public_p = TREE_PUBLIC (anon_union_decl);

  /* The VAR_DECL's context is the same as the TYPE's context.  */
  DECL_CONTEXT (anon_union_decl) = DECL_CONTEXT (TYPE_NAME (type));
  
  if (TYPE_FIELDS (type) == NULL_TREE)
    return;

  if (public_p)
    {
      error ("namespace-scope anonymous aggregates must be static");
      return;
    }

  main_decl = build_anon_union_vars (anon_union_decl);
  if (main_decl == NULL_TREE)
    {
      warning ("anonymous union with no members");
      return;
    }

  if (!processing_template_decl)
    {
      /* Use main_decl to set the mangled name.  */
      DECL_NAME (anon_union_decl) = DECL_NAME (main_decl);
      mangle_decl (anon_union_decl);
      DECL_NAME (anon_union_decl) = NULL_TREE;
    }

  pushdecl (anon_union_decl);
  if (building_stmt_tree ()
      && at_function_scope_p ())
    add_decl_stmt (anon_union_decl);
  else if (!processing_template_decl)
    rest_of_decl_compilation (anon_union_decl, NULL,
			      toplevel_bindings_p (), at_eof);
}

/* Auxiliary functions to make type signatures for
   `operator new' and `operator delete' correspond to
   what compiler will be expecting.  */

tree
coerce_new_type (tree type)
{
  int e = 0;
  tree args = TYPE_ARG_TYPES (type);

  my_friendly_assert (TREE_CODE (type) == FUNCTION_TYPE, 20001107);
  
  if (!same_type_p (TREE_TYPE (type), ptr_type_node))
    e = 1, error ("`operator new' must return type `%T'", ptr_type_node);

  if (!args || args == void_list_node
      || !same_type_p (TREE_VALUE (args), size_type_node))
    {
      e = 2;
      if (args && args != void_list_node)
        args = TREE_CHAIN (args);
      pedwarn ("`operator new' takes type `size_t' (`%T') as first parameter", size_type_node);
    }
  switch (e)
  {
    case 2:
      args = tree_cons (NULL_TREE, size_type_node, args);
      /* Fall through.  */
    case 1:
      type = build_exception_variant
              (build_function_type (ptr_type_node, args),
               TYPE_RAISES_EXCEPTIONS (type));
      /* Fall through.  */
    default:;
  }
  return type;
}

tree
coerce_delete_type (tree type)
{
  int e = 0;
  tree args = TYPE_ARG_TYPES (type);
  
  my_friendly_assert (TREE_CODE (type) == FUNCTION_TYPE, 20001107);

  if (!same_type_p (TREE_TYPE (type), void_type_node))
    e = 1, error ("`operator delete' must return type `%T'", void_type_node);

  if (!args || args == void_list_node
      || !same_type_p (TREE_VALUE (args), ptr_type_node))
    {
      e = 2;
      if (args && args != void_list_node)
        args = TREE_CHAIN (args);
      error ("`operator delete' takes type `%T' as first parameter", ptr_type_node);
    }
  switch (e)
  {
    case 2:
      args = tree_cons (NULL_TREE, ptr_type_node, args);
      /* Fall through.  */
    case 1:
      type = build_exception_variant
              (build_function_type (void_type_node, args),
               TYPE_RAISES_EXCEPTIONS (type));
      /* Fall through.  */
    default:;
  }

  return type;
}

static void
mark_vtable_entries (tree decl)
{
  tree entries = CONSTRUCTOR_ELTS (DECL_INITIAL (decl));

  for (; entries; entries = TREE_CHAIN (entries))
    {
      tree fnaddr = TREE_VALUE (entries);
      tree fn;

      STRIP_NOPS (fnaddr);

      if (TREE_CODE (fnaddr) != ADDR_EXPR
	  && TREE_CODE (fnaddr) != FDESC_EXPR)
	/* This entry is an offset: a virtual base class offset, a
	   virtual call offset, an RTTI offset, etc.  */
	continue;

      fn = TREE_OPERAND (fnaddr, 0);
      TREE_ADDRESSABLE (fn) = 1;
      /* When we don't have vcall offsets, we output thunks whenever
	 we output the vtables that contain them.  With vcall offsets,
	 we know all the thunks we'll need when we emit a virtual
	 function, so we emit the thunks there instead.  */
      if (DECL_THUNK_P (fn)) 
	use_thunk (fn, /*emit_p=*/0);
      mark_used (fn);
    }
}

/* Set DECL up to have the closest approximation of "initialized common"
   linkage available.  */

void
comdat_linkage (tree decl)
{
  if (flag_weak)
    make_decl_one_only (decl);
  else if (TREE_CODE (decl) == FUNCTION_DECL 
	   || (TREE_CODE (decl) == VAR_DECL && DECL_ARTIFICIAL (decl)))
    /* We can just emit function and compiler-generated variables
       statically; having multiple copies is (for the most part) only
       a waste of space.  

       There are two correctness issues, however: the address of a
       template instantiation with external linkage should be the
       same, independent of what translation unit asks for the
       address, and this will not hold when we emit multiple copies of
       the function.  However, there's little else we can do.  

       Also, by default, the typeinfo implementation assumes that
       there will be only one copy of the string used as the name for
       each type.  Therefore, if weak symbols are unavailable, the
       run-time library should perform a more conservative check; it
       should perform a string comparison, rather than an address
       comparison.  */
    TREE_PUBLIC (decl) = 0;
  else
    {
      /* Static data member template instantiations, however, cannot
	 have multiple copies.  */
      if (DECL_INITIAL (decl) == 0
	  || DECL_INITIAL (decl) == error_mark_node)
	DECL_COMMON (decl) = 1;
      else if (EMPTY_CONSTRUCTOR_P (DECL_INITIAL (decl)))
	{
	  DECL_COMMON (decl) = 1;
	  DECL_INITIAL (decl) = error_mark_node;
	}
      else if (!DECL_EXPLICIT_INSTANTIATION (decl))
	{
	  /* We can't do anything useful; leave vars for explicit
	     instantiation.  */
	  DECL_EXTERNAL (decl) = 1;
	  DECL_NOT_REALLY_EXTERN (decl) = 0;
	}
    }

  if (DECL_LANG_SPECIFIC (decl))
    DECL_COMDAT (decl) = 1;
}

/* For win32 we also want to put explicit instantiations in
   linkonce sections, so that they will be merged with implicit
   instantiations; otherwise we get duplicate symbol errors.  */

void
maybe_make_one_only (tree decl)
{
  /* We used to say that this was not necessary on targets that support weak
     symbols, because the implicit instantiations will defer to the explicit
     one.  However, that's not actually the case in SVR4; a strong definition
     after a weak one is an error.  Also, not making explicit
     instantiations one_only means that we can end up with two copies of
     some template instantiations.  */
  if (! flag_weak)
    return;

  /* We can't set DECL_COMDAT on functions, or finish_file will think
     we can get away with not emitting them if they aren't used.  We need
     to for variables so that cp_finish_decl will update their linkage,
     because their DECL_INITIAL may not have been set properly yet.  */

  make_decl_one_only (decl);

  if (TREE_CODE (decl) == VAR_DECL)
    {
      DECL_COMDAT (decl) = 1;
      /* Mark it needed so we don't forget to emit it.  */
      mark_referenced (DECL_ASSEMBLER_NAME (decl));
    }
}

/* Set TREE_PUBLIC and/or DECL_EXTERN on the vtable DECL,
   based on TYPE and other static flags.

   Note that anything public is tagged TREE_PUBLIC, whether
   it's public in this file or in another one.  */

void
import_export_vtable (tree decl, tree type, int final)
{
  if (DECL_INTERFACE_KNOWN (decl))
    return;

  if (TYPE_FOR_JAVA (type))
    {
      TREE_PUBLIC (decl) = 1;
      DECL_EXTERNAL (decl) = 1;
      DECL_INTERFACE_KNOWN (decl) = 1;
    }
  else if (CLASSTYPE_INTERFACE_KNOWN (type))
    {
      TREE_PUBLIC (decl) = 1;
      DECL_EXTERNAL (decl) = CLASSTYPE_INTERFACE_ONLY (type);
      DECL_INTERFACE_KNOWN (decl) = 1;
    }
  else
    {
      /* We can only wait to decide if we have real non-inline virtual
	 functions in our class, or if we come from a template.  */

      int found = (CLASSTYPE_TEMPLATE_INSTANTIATION (type)
		   || CLASSTYPE_KEY_METHOD (type) != NULL_TREE);

      if (final || ! found)
	{
	  comdat_linkage (decl);
	  DECL_EXTERNAL (decl) = 0;
	}
      else
	{
	  TREE_PUBLIC (decl) = 1;
	  DECL_EXTERNAL (decl) = 1;
	}
    }
}

/* Determine whether or not we want to specifically import or export CTYPE,
   using various heuristics.  */

static void
import_export_class (tree ctype)
{
  /* -1 for imported, 1 for exported.  */
  int import_export = 0;

  /* It only makes sense to call this function at EOF.  The reason is
     that this function looks at whether or not the first non-inline
     non-abstract virtual member function has been defined in this
     translation unit.  But, we can't possibly know that until we've
     seen the entire translation unit.  */
  my_friendly_assert (at_eof, 20000226);

  if (CLASSTYPE_INTERFACE_KNOWN (ctype))
    return;

  /* If MULTIPLE_SYMBOL_SPACES is defined and we saw a #pragma interface,
     we will have CLASSTYPE_INTERFACE_ONLY set but not
     CLASSTYPE_INTERFACE_KNOWN.  In that case, we don't want to use this
     heuristic because someone will supply a #pragma implementation
     elsewhere, and deducing it here would produce a conflict.  */
  if (CLASSTYPE_INTERFACE_ONLY (ctype))
    return;

  if (lookup_attribute ("dllimport", TYPE_ATTRIBUTES (ctype)))
    import_export = -1;
  else if (lookup_attribute ("dllexport", TYPE_ATTRIBUTES (ctype)))
    import_export = 1;

  /* If we got -fno-implicit-templates, we import template classes that
     weren't explicitly instantiated.  */
  if (import_export == 0
      && CLASSTYPE_IMPLICIT_INSTANTIATION (ctype)
      && ! flag_implicit_templates)
    import_export = -1;

  /* Base our import/export status on that of the first non-inline,
     non-pure virtual function, if any.  */
  if (import_export == 0
      && TYPE_POLYMORPHIC_P (ctype))
    {
      tree method = CLASSTYPE_KEY_METHOD (ctype);
      if (method)
	import_export = (DECL_REALLY_EXTERN (method) ? -1 : 1);
    }

#ifdef MULTIPLE_SYMBOL_SPACES
  if (import_export == -1)
    import_export = 0;
#endif

  if (import_export)
    {
      SET_CLASSTYPE_INTERFACE_KNOWN (ctype);
      CLASSTYPE_INTERFACE_ONLY (ctype) = (import_export < 0);
    }
}

/* Return true if VAR has already been provided to the back end; in that
   case VAR should not be modified further by the front end.  */
static bool
var_finalized_p (tree var)
{
  if (flag_unit_at_a_time)
    return cgraph_varpool_node (var)->finalized;
  else
    return TREE_ASM_WRITTEN (var);
}

/* If necessary, write out the vtables for the dynamic class CTYPE.
   Returns true if any vtables were emitted.  */

static bool
maybe_emit_vtables (tree ctype)
{
  tree vtbl;
  tree primary_vtbl;
  bool needed = false;

  /* If the vtables for this class have already been emitted there is
     nothing more to do.  */
  primary_vtbl = CLASSTYPE_VTABLES (ctype);
  if (var_finalized_p (primary_vtbl))
    return false;
  /* Ignore dummy vtables made by get_vtable_decl.  */
  if (TREE_TYPE (primary_vtbl) == void_type_node)
    return false;

  import_export_class (ctype);

  /* See if any of the vtables are needed.  */
  for (vtbl = CLASSTYPE_VTABLES (ctype); vtbl; vtbl = TREE_CHAIN (vtbl))
    {
      import_export_vtable (vtbl, ctype, 1);
      if (!DECL_EXTERNAL (vtbl) && DECL_NEEDED_P (vtbl))
	break;
    }
  if (!vtbl)
    {
      /* If the references to this class' vtables are optimized away,
	 still emit the appropriate debugging information.  See
	 dfs_debug_mark.  */
      if (DECL_COMDAT (primary_vtbl) 
	  && CLASSTYPE_DEBUG_REQUESTED (ctype))
	note_debug_info_needed (ctype);
      return false;
    }
  else if (TREE_PUBLIC (vtbl) && !DECL_COMDAT (vtbl))
    needed = true;
  

  /* The ABI requires that we emit all of the vtables if we emit any
     of them.  */
  for (vtbl = CLASSTYPE_VTABLES (ctype); vtbl; vtbl = TREE_CHAIN (vtbl))
    {
      /* Write it out.  */
      import_export_vtable (vtbl, ctype, 1);
      mark_vtable_entries (vtbl);

      /* If we know that DECL is needed, mark it as such for the varpool.  */
      if (needed)
	cgraph_varpool_mark_needed_node (cgraph_varpool_node (vtbl));

      if (TREE_TYPE (DECL_INITIAL (vtbl)) == 0)
	{
	  /* It had better be all done at compile-time.  */
	  if (store_init_value (vtbl, DECL_INITIAL (vtbl)))
	    abort ();
	}

      if (write_symbols == DWARF_DEBUG || write_symbols == DWARF2_DEBUG)
	{
	  /* Mark the VAR_DECL node representing the vtable itself as a
	     "gratuitous" one, thereby forcing dwarfout.c to ignore it.
	     It is rather important that such things be ignored because
	     any effort to actually generate DWARF for them will run
	     into trouble when/if we encounter code like:

		#pragma interface
		struct S { virtual void member (); };

	      because the artificial declaration of the vtable itself (as
	      manufactured by the g++ front end) will say that the vtable
	      is a static member of `S' but only *after* the debug output
	      for the definition of `S' has already been output.  This causes
	      grief because the DWARF entry for the definition of the vtable
	      will try to refer back to an earlier *declaration* of the
	      vtable as a static member of `S' and there won't be one.
	      We might be able to arrange to have the "vtable static member"
	      attached to the member list for `S' before the debug info for
	      `S' get written (which would solve the problem) but that would
	      require more intrusive changes to the g++ front end.  */

	  DECL_IGNORED_P (vtbl) = 1;
	}

      /* Always make vtables weak.  */
      if (flag_weak)
	comdat_linkage (vtbl);

      rest_of_decl_compilation (vtbl, NULL, 1, 1);

      /* Because we're only doing syntax-checking, we'll never end up
	 actually marking the variable as written.  */
      if (flag_syntax_only)
	TREE_ASM_WRITTEN (vtbl) = 1;
    }

  /* Since we're writing out the vtable here, also write the debug
     info.  */
  note_debug_info_needed (ctype);

  return true;
}

/* Determines the proper settings of TREE_PUBLIC and DECL_EXTERNAL for an
   inline function or template instantiation at end-of-file.  */

void
import_export_decl (tree decl)
{
  if (DECL_INTERFACE_KNOWN (decl))
    return;

  if (DECL_TEMPLATE_INSTANTIATION (decl)
      || DECL_FRIEND_PSEUDO_TEMPLATE_INSTANTIATION (decl))
    {
      DECL_NOT_REALLY_EXTERN (decl) = 1;
      if ((DECL_IMPLICIT_INSTANTIATION (decl)
	   || DECL_FRIEND_PSEUDO_TEMPLATE_INSTANTIATION (decl))
	  && (flag_implicit_templates
	      || (flag_implicit_inline_templates
		  && TREE_CODE (decl) == FUNCTION_DECL 
		  && DECL_DECLARED_INLINE_P (decl))))
	{
	  if (!TREE_PUBLIC (decl))
	    /* Templates are allowed to have internal linkage.  See 
	       [basic.link].  */
	    ;
	  else
	    comdat_linkage (decl);
	}
      else
	{
	  DECL_EXTERNAL (decl) = 1;
	  DECL_NOT_REALLY_EXTERN (decl) = 0;
	}
    }
  else if (DECL_FUNCTION_MEMBER_P (decl))
    {
      if (!DECL_DECLARED_INLINE_P (decl))
	{
	  tree ctype = DECL_CONTEXT (decl);
	  import_export_class (ctype);
	  if (CLASSTYPE_INTERFACE_KNOWN (ctype))
	    {
	      DECL_NOT_REALLY_EXTERN (decl)
		= ! (CLASSTYPE_INTERFACE_ONLY (ctype)
		     || (DECL_DECLARED_INLINE_P (decl) 
			 && ! flag_implement_inlines
			 && !DECL_VINDEX (decl)));

	      if (!DECL_NOT_REALLY_EXTERN (decl))
		DECL_EXTERNAL (decl) = 1;

	      /* Always make artificials weak.  */
	      if (DECL_ARTIFICIAL (decl) && flag_weak)
		comdat_linkage (decl);
	      else
		maybe_make_one_only (decl);
	    }
	}
      else
	comdat_linkage (decl);
    }
  else
    comdat_linkage (decl);

  DECL_INTERFACE_KNOWN (decl) = 1;
}

/* Here, we only decide whether or not the tinfo node should be
   emitted with the vtable.  IS_IN_LIBRARY is nonzero iff the
   typeinfo for TYPE should be in the runtime library.  */

void
import_export_tinfo (tree decl, tree type, bool is_in_library)
{
  if (DECL_INTERFACE_KNOWN (decl))
    return;
  
  if (IS_AGGR_TYPE (type))
    import_export_class (type);
      
  if (IS_AGGR_TYPE (type) && CLASSTYPE_INTERFACE_KNOWN (type)
      && TYPE_POLYMORPHIC_P (type)
      /* If -fno-rtti, we're not necessarily emitting this stuff with
	 the class, so go ahead and emit it now.  This can happen when
	 a class is used in exception handling.  */
      && flag_rtti)
    {
      DECL_NOT_REALLY_EXTERN (decl) = !CLASSTYPE_INTERFACE_ONLY (type);
      DECL_COMDAT (decl) = 0;
    }
  else
    {
      DECL_NOT_REALLY_EXTERN (decl) = 1;
      DECL_COMDAT (decl) = 1;
    }

  /* Now override some cases.  */
  if (flag_weak)
    DECL_COMDAT (decl) = 1;
  else if (is_in_library)
    DECL_COMDAT (decl) = 0;
  
  DECL_INTERFACE_KNOWN (decl) = 1;
}

/* Return an expression that performs the destruction of DECL, which
   must be a VAR_DECL whose type has a non-trivial destructor, or is
   an array whose (innermost) elements have a non-trivial destructor.  */

tree
build_cleanup (tree decl)
{
  tree temp;
  tree type = TREE_TYPE (decl);

  /* This function should only be called for declarations that really
     require cleanups.  */
  my_friendly_assert (!TYPE_HAS_TRIVIAL_DESTRUCTOR (type), 20030106);

  /* Treat all objects with destructors as used; the destructor may do
     something substantive.  */
  mark_used (decl);

  if (TREE_CODE (type) == ARRAY_TYPE)
    temp = decl;
  else
    {
      cxx_mark_addressable (decl);
      temp = build1 (ADDR_EXPR, build_pointer_type (type), decl);
    }
  temp = build_delete (TREE_TYPE (temp), temp,
		       sfk_complete_destructor,
		       LOOKUP_NORMAL|LOOKUP_NONVIRTUAL|LOOKUP_DESTRUCTOR, 0);
  return temp;
}

/* Returns the initialization guard variable for the variable DECL,
   which has static storage duration.  */

tree
get_guard (tree decl)
{
  tree sname;
  tree guard;

  sname = mangle_guard_variable (decl);
  guard = IDENTIFIER_GLOBAL_VALUE (sname);
  if (! guard)
    {
      tree guard_type;

      /* We use a type that is big enough to contain a mutex as well
	 as an integer counter.  */
      guard_type = long_long_integer_type_node;
      guard = build_decl (VAR_DECL, sname, guard_type);
      
      /* The guard should have the same linkage as what it guards.  */
      TREE_PUBLIC (guard) = TREE_PUBLIC (decl);
      TREE_STATIC (guard) = TREE_STATIC (decl);
      DECL_COMMON (guard) = DECL_COMMON (decl);
      DECL_ONE_ONLY (guard) = DECL_ONE_ONLY (decl);
      if (TREE_PUBLIC (decl))
        DECL_WEAK (guard) = DECL_WEAK (decl);
      
      DECL_ARTIFICIAL (guard) = 1;
      TREE_USED (guard) = 1;
      pushdecl_top_level_and_finish (guard, NULL_TREE);
    }
  return guard;
}

/* Return those bits of the GUARD variable that should be set when the
   guarded entity is actually initialized.  */

static tree
get_guard_bits (tree guard)
{
  /* We only set the first byte of the guard, in order to leave room
     for a mutex in the high-order bits.  */
  guard = build1 (ADDR_EXPR, 
		  build_pointer_type (TREE_TYPE (guard)),
		  guard);
  guard = build1 (NOP_EXPR, 
		  build_pointer_type (char_type_node), 
		  guard);
  guard = build1 (INDIRECT_REF, char_type_node, guard);

  return guard;
}

/* Return an expression which determines whether or not the GUARD
   variable has already been initialized.  */

tree
get_guard_cond (tree guard)
{
  tree guard_value;

  /* Check to see if the GUARD is zero.  */
  guard = get_guard_bits (guard);
  guard_value = integer_zero_node;
  if (!same_type_p (TREE_TYPE (guard_value), TREE_TYPE (guard)))
    guard_value = convert (TREE_TYPE (guard), guard_value);
  return cp_build_binary_op (EQ_EXPR, guard, guard_value);
}

/* Return an expression which sets the GUARD variable, indicating that
   the variable being guarded has been initialized.  */

tree
set_guard (tree guard)
{
  tree guard_init;

  /* Set the GUARD to one.  */
  guard = get_guard_bits (guard);
  guard_init = integer_one_node;
  if (!same_type_p (TREE_TYPE (guard_init), TREE_TYPE (guard)))
    guard_init = convert (TREE_TYPE (guard), guard_init);
  return build_modify_expr (guard, NOP_EXPR, guard_init);
}

/* Start the process of running a particular set of global constructors
   or destructors.  Subroutine of do_[cd]tors.  */

static tree
start_objects (int method_type, int initp)
{
  tree fnname;
  tree body;
  char type[10];

  /* Make ctor or dtor function.  METHOD_TYPE may be 'I' or 'D'.  */

  if (initp != DEFAULT_INIT_PRIORITY)
    {
      char joiner;

#ifdef JOINER
      joiner = JOINER;
#else
      joiner = '_';
#endif

      sprintf (type, "%c%c%.5u", method_type, joiner, initp);
    }
  else
    sprintf (type, "%c", method_type);

  fnname = get_file_function_name_long (type);

  start_function (void_list_node,
		  make_call_declarator (fnname, void_list_node, NULL_TREE,
					NULL_TREE),
		  NULL_TREE, SF_DEFAULT);

  /* It can be a static function as long as collect2 does not have
     to scan the object file to find its ctor/dtor routine.  */
  TREE_PUBLIC (current_function_decl) = ! targetm.have_ctors_dtors;

  /* Mark this declaration as used to avoid spurious warnings.  */
  TREE_USED (current_function_decl) = 1;

  /* Mark this function as a global constructor or destructor.  */
  if (method_type == 'I')
    DECL_GLOBAL_CTOR_P (current_function_decl) = 1;
  else
    DECL_GLOBAL_DTOR_P (current_function_decl) = 1;
  DECL_LANG_SPECIFIC (current_function_decl)->decl_flags.u2sel = 1;

  body = begin_compound_stmt (/*has_no_scope=*/false);

  /* We cannot allow these functions to be elided, even if they do not
     have external linkage.  And, there's no point in deferring
     compilation of thes functions; they're all going to have to be
     out anyhow.  */
  current_function_cannot_inline
    = "static constructors and destructors cannot be inlined";

  return body;
}

/* Finish the process of running a particular set of global constructors
   or destructors.  Subroutine of do_[cd]tors.  */

static void
finish_objects (int method_type, int initp, tree body)
{
  tree fn;

  /* Finish up.  */
  finish_compound_stmt (body);
  fn = finish_function (0);
  expand_or_defer_fn (fn);

  /* When only doing semantic analysis, and no RTL generation, we
     can't call functions that directly emit assembly code; there is
     no assembly file in which to put the code.  */
  if (flag_syntax_only)
    return;

  if (targetm.have_ctors_dtors)
    {
      rtx fnsym = XEXP (DECL_RTL (fn), 0);
      if (method_type == 'I')
	(* targetm.asm_out.constructor) (fnsym, initp);
      else
	(* targetm.asm_out.destructor) (fnsym, initp);
    }
}

/* The names of the parameters to the function created to handle
   initializations and destructions for objects with static storage
   duration.  */
#define INITIALIZE_P_IDENTIFIER "__initialize_p"
#define PRIORITY_IDENTIFIER "__priority"

/* The name of the function we create to handle initializations and
   destructions for objects with static storage duration.  */
#define SSDF_IDENTIFIER "__static_initialization_and_destruction"

/* The declaration for the __INITIALIZE_P argument.  */
static GTY(()) tree initialize_p_decl;

/* The declaration for the __PRIORITY argument.  */
static GTY(()) tree priority_decl;

/* The declaration for the static storage duration function.  */
static GTY(()) tree ssdf_decl;

/* All the static storage duration functions created in this
   translation unit.  */
static GTY(()) varray_type ssdf_decls;

/* A map from priority levels to information about that priority
   level.  There may be many such levels, so efficient lookup is
   important.  */
static splay_tree priority_info_map;

/* Begins the generation of the function that will handle all
   initialization and destruction of objects with static storage
   duration.  The function generated takes two parameters of type
   `int': __INITIALIZE_P and __PRIORITY.  If __INITIALIZE_P is
   nonzero, it performs initializations.  Otherwise, it performs
   destructions.  It only performs those initializations or
   destructions with the indicated __PRIORITY.  The generated function
   returns no value.  

   It is assumed that this function will only be called once per
   translation unit.  */

static tree
start_static_storage_duration_function (unsigned count)
{
  tree parm_types;
  tree type;
  tree body;
  char id[sizeof (SSDF_IDENTIFIER) + 1 /* '\0' */ + 32];

  /* Create the identifier for this function.  It will be of the form
     SSDF_IDENTIFIER_<number>.  */
  sprintf (id, "%s_%u", SSDF_IDENTIFIER, count);

  /* Create the parameters.  */
  parm_types = void_list_node;
  parm_types = tree_cons (NULL_TREE, integer_type_node, parm_types);
  parm_types = tree_cons (NULL_TREE, integer_type_node, parm_types);
  type = build_function_type (void_type_node, parm_types);

  /* Create the FUNCTION_DECL itself.  */
  ssdf_decl = build_lang_decl (FUNCTION_DECL, 
			       get_identifier (id),
			       type);
  TREE_PUBLIC (ssdf_decl) = 0;
  DECL_ARTIFICIAL (ssdf_decl) = 1;

  /* Put this function in the list of functions to be called from the
     static constructors and destructors.  */
  if (!ssdf_decls)
    {
      VARRAY_TREE_INIT (ssdf_decls, 32, "ssdf_decls");

      /* Take this opportunity to initialize the map from priority
	 numbers to information about that priority level.  */
      priority_info_map = splay_tree_new (splay_tree_compare_ints,
					  /*delete_key_fn=*/0,
					  /*delete_value_fn=*/
					  (splay_tree_delete_value_fn) &free);

      /* We always need to generate functions for the
	 DEFAULT_INIT_PRIORITY so enter it now.  That way when we walk
	 priorities later, we'll be sure to find the
	 DEFAULT_INIT_PRIORITY.  */
      get_priority_info (DEFAULT_INIT_PRIORITY);
    }

  VARRAY_PUSH_TREE (ssdf_decls, ssdf_decl);

  /* Create the argument list.  */
  initialize_p_decl = cp_build_parm_decl
    (get_identifier (INITIALIZE_P_IDENTIFIER), integer_type_node);
  DECL_CONTEXT (initialize_p_decl) = ssdf_decl;
  TREE_USED (initialize_p_decl) = 1;
  priority_decl = cp_build_parm_decl
    (get_identifier (PRIORITY_IDENTIFIER), integer_type_node);
  DECL_CONTEXT (priority_decl) = ssdf_decl;
  TREE_USED (priority_decl) = 1;

  TREE_CHAIN (initialize_p_decl) = priority_decl;
  DECL_ARGUMENTS (ssdf_decl) = initialize_p_decl;

  /* Put the function in the global scope.  */
  pushdecl (ssdf_decl);

  /* Start the function itself.  This is equivalent to declaring the
     function as:

       static void __ssdf (int __initialize_p, init __priority_p);
       
     It is static because we only need to call this function from the
     various constructor and destructor functions for this module.  */
  start_function (/*specs=*/NULL_TREE, 
		  ssdf_decl,
		  /*attrs=*/NULL_TREE,
		  SF_PRE_PARSED);

  /* Set up the scope of the outermost block in the function.  */
  body = begin_compound_stmt (/*has_no_scope=*/false);

  /* This function must not be deferred because we are depending on
     its compilation to tell us what is TREE_SYMBOL_REFERENCED.  */
  current_function_cannot_inline 
    = "static storage duration functions cannot be inlined";

  return body;
}

/* Finish the generation of the function which performs initialization
   and destruction of objects with static storage duration.  After
   this point, no more such objects can be created.  */

static void
finish_static_storage_duration_function (tree body)
{
  /* Close out the function.  */
  finish_compound_stmt (body);
  expand_or_defer_fn (finish_function (0));
}

/* Return the information about the indicated PRIORITY level.  If no
   code to handle this level has yet been generated, generate the
   appropriate prologue.  */

static priority_info
get_priority_info (int priority)
{
  priority_info pi;
  splay_tree_node n;

  n = splay_tree_lookup (priority_info_map, 
			 (splay_tree_key) priority);
  if (!n)
    {
      /* Create a new priority information structure, and insert it
	 into the map.  */
      pi = xmalloc (sizeof (struct priority_info_s));
      pi->initializations_p = 0;
      pi->destructions_p = 0;
      splay_tree_insert (priority_info_map,
			 (splay_tree_key) priority,
			 (splay_tree_value) pi);
    }
  else
    pi = (priority_info) n->value;

  return pi;
}

/* Set up to handle the initialization or destruction of DECL.  If
   INITP is nonzero, we are initializing the variable.  Otherwise, we
   are destroying it.  */

static tree
start_static_initialization_or_destruction (tree decl, int initp)
{
  tree guard_if_stmt = NULL_TREE;
  int priority;
  tree cond;
  tree guard;
  tree init_cond;
  priority_info pi;

  /* Figure out the priority for this declaration.  */
  priority = DECL_INIT_PRIORITY (decl);
  if (!priority)
    priority = DEFAULT_INIT_PRIORITY;

  /* Remember that we had an initialization or finalization at this
     priority.  */
  pi = get_priority_info (priority);
  if (initp)
    pi->initializations_p = 1;
  else
    pi->destructions_p = 1;

  /* Trick the compiler into thinking we are at the file and line
     where DECL was declared so that error-messages make sense, and so
     that the debugger will show somewhat sensible file and line
     information.  */
  input_location = DECL_SOURCE_LOCATION (decl);

  /* Because of:

       [class.access.spec]

       Access control for implicit calls to the constructors,
       the conversion functions, or the destructor called to
       create and destroy a static data member is performed as
       if these calls appeared in the scope of the member's
       class.  

     we pretend we are in a static member function of the class of
     which the DECL is a member.  */
  if (member_p (decl))
    {
      DECL_CONTEXT (current_function_decl) = DECL_CONTEXT (decl);
      DECL_STATIC_FUNCTION_P (current_function_decl) = 1;
    }
  
  /* Conditionalize this initialization on being in the right priority
     and being initializing/finalizing appropriately.  */
  guard_if_stmt = begin_if_stmt ();
  cond = cp_build_binary_op (EQ_EXPR,
			     priority_decl,
			     build_int_2 (priority, 0));
  init_cond = initp ? integer_one_node : integer_zero_node;
  init_cond = cp_build_binary_op (EQ_EXPR,
				  initialize_p_decl,
				  init_cond);
  cond = cp_build_binary_op (TRUTH_ANDIF_EXPR, cond, init_cond);

  /* Assume we don't need a guard.  */
  guard = NULL_TREE;
  /* We need a guard if this is an object with external linkage that
     might be initialized in more than one place.  (For example, a
     static data member of a template, when the data member requires
     construction.)  */
  if (TREE_PUBLIC (decl) && (DECL_COMMON (decl) 
			     || DECL_ONE_ONLY (decl)
			     || DECL_WEAK (decl)))
    {
      tree guard_cond;

      guard = get_guard (decl);

      /* When using __cxa_atexit, we just check the GUARD as we would
	 for a local static.  */
      if (flag_use_cxa_atexit)
	{
	  /* When using __cxa_atexit, we never try to destroy
	     anything from a static destructor.  */
	  my_friendly_assert (initp, 20000629);
	  guard_cond = get_guard_cond (guard);
	}
      /* If we don't have __cxa_atexit, then we will be running
	 destructors from .fini sections, or their equivalents.  So,
	 we need to know how many times we've tried to initialize this
	 object.  We do initializations only if the GUARD is zero,
	 i.e., if we are the first to initialize the variable.  We do
	 destructions only if the GUARD is one, i.e., if we are the
	 last to destroy the variable.  */
      else if (initp)
	guard_cond 
	  = cp_build_binary_op (EQ_EXPR,
				build_unary_op (PREINCREMENT_EXPR,
						guard,
						/*noconvert=*/1),
				integer_one_node);
      else
	guard_cond 
	  = cp_build_binary_op (EQ_EXPR,
				build_unary_op (PREDECREMENT_EXPR,
						guard,
						/*noconvert=*/1),
				integer_zero_node);

      cond = cp_build_binary_op (TRUTH_ANDIF_EXPR, cond, guard_cond);
    }

  finish_if_stmt_cond (cond, guard_if_stmt);

  /* If we're using __cxa_atexit, we have not already set the GUARD,
     so we must do so now.  */
  if (guard && initp && flag_use_cxa_atexit)
    finish_expr_stmt (set_guard (guard));

  return guard_if_stmt;
}

/* We've just finished generating code to do an initialization or
   finalization.  GUARD_IF_STMT is the if-statement we used to guard
   the initialization.  */

static void
finish_static_initialization_or_destruction (tree guard_if_stmt)
{
  finish_then_clause (guard_if_stmt);
  finish_if_stmt ();

  /* Now that we're done with DECL we don't need to pretend to be a
     member of its class any longer.  */
  DECL_CONTEXT (current_function_decl) = NULL_TREE;
  DECL_STATIC_FUNCTION_P (current_function_decl) = 0;
}

/* Generate code to do the initialization of DECL, a VAR_DECL with
   static storage duration.  The initialization is INIT.  */

static void
do_static_initialization (tree decl, tree init)
{
  tree guard_if_stmt;

  /* Set up for the initialization.  */
  guard_if_stmt
    = start_static_initialization_or_destruction (decl,
						  /*initp=*/1);

  /* Perform the initialization.  */
  if (init)
    finish_expr_stmt (init);

  /* If we're using __cxa_atexit, register a a function that calls the
     destructor for the object.  */
  if (flag_use_cxa_atexit)
    register_dtor_fn (decl);

  /* Finsh up.  */
  finish_static_initialization_or_destruction (guard_if_stmt);
}

/* Generate code to do the static destruction of DECL.  If DECL may be
   initialized more than once in different object files, GUARD is the
   guard variable to check.  PRIORITY is the priority for the
   destruction.  */

static void
do_static_destruction (tree decl)
{
  tree guard_if_stmt;

  /* If we're using __cxa_atexit, then destructors are registered
     immediately after objects are initialized.  */
  my_friendly_assert (!flag_use_cxa_atexit, 20000121);

  /* If we don't need a destructor, there's nothing to do.  */
  if (TYPE_HAS_TRIVIAL_DESTRUCTOR (TREE_TYPE (decl)))
    return;

  /* Actually do the destruction.  */
  guard_if_stmt = start_static_initialization_or_destruction (decl,
							      /*initp=*/0);
  finish_expr_stmt (build_cleanup (decl));
  finish_static_initialization_or_destruction (guard_if_stmt);
}

/* VARS is a list of variables with static storage duration which may
   need initialization and/or finalization.  Remove those variables
   that don't really need to be initialized or finalized, and return
   the resulting list.  The order in which the variables appear in
   VARS is in reverse order of the order in which they should actually
   be initialized.  The list we return is in the unreversed order;
   i.e., the first variable should be initialized first.  */

static tree
prune_vars_needing_no_initialization (tree *vars)
{
  tree *var = vars;
  tree result = NULL_TREE;

  while (*var)
    {
      tree t = *var;
      tree decl = TREE_VALUE (t);
      tree init = TREE_PURPOSE (t);

      /* Deal gracefully with error.  */
      if (decl == error_mark_node)
	{
	  var = &TREE_CHAIN (t);
	  continue;
	}

      /* The only things that can be initialized are variables.  */
      my_friendly_assert (TREE_CODE (decl) == VAR_DECL, 19990420);

      /* If this object is not defined, we don't need to do anything
	 here.  */
      if (DECL_EXTERNAL (decl))
	{
	  var = &TREE_CHAIN (t);
	  continue;
	}

      /* Also, if the initializer already contains errors, we can bail
	 out now.  */
      if (init && TREE_CODE (init) == TREE_LIST 
	  && value_member (error_mark_node, init))
	{
	  var = &TREE_CHAIN (t);
	  continue;
	}

      /* This variable is going to need initialization and/or
	 finalization, so we add it to the list.  */
      *var = TREE_CHAIN (t);
      TREE_CHAIN (t) = result;
      result = t;
    }

  return result;
}

/* Make sure we have told the back end about all the variables in
   VARS.  */

static void
write_out_vars (tree vars)
{
  tree v;

  for (v = vars; v; v = TREE_CHAIN (v))
    if (!var_finalized_p (TREE_VALUE (v)))
      rest_of_decl_compilation (TREE_VALUE (v), 0, 1, 1);
}

/* Generate a static constructor (if CONSTRUCTOR_P) or destructor
   (otherwise) that will initialize all gobal objects with static
   storage duration having the indicated PRIORITY.  */

static void
generate_ctor_or_dtor_function (bool constructor_p, int priority,
				location_t *locus)
{
  char function_key;
  tree arguments;
  tree fndecl;
  tree body;
  size_t i;

  input_location = *locus;
  locus->line++;
  
  /* We use `I' to indicate initialization and `D' to indicate
     destruction.  */
  function_key = constructor_p ? 'I' : 'D';

  /* We emit the function lazily, to avoid generating empty
     global constructors and destructors.  */
  body = NULL_TREE;

  /* Call the static storage duration function with appropriate
     arguments.  */
  if (ssdf_decls)
    for (i = 0; i < ssdf_decls->elements_used; ++i) 
      {
	fndecl = VARRAY_TREE (ssdf_decls, i);

	/* Calls to pure or const functions will expand to nothing.  */
	if (! (flags_from_decl_or_type (fndecl) & (ECF_CONST | ECF_PURE)))
	  {
	    if (! body)
	      body = start_objects (function_key, priority);

	    arguments = tree_cons (NULL_TREE, build_int_2 (priority, 0), 
				   NULL_TREE);
	    arguments = tree_cons (NULL_TREE, build_int_2 (constructor_p, 0),
				   arguments);
	    finish_expr_stmt (build_function_call (fndecl, arguments));
	  }
      }

  /* If we're generating code for the DEFAULT_INIT_PRIORITY, throw in
     calls to any functions marked with attributes indicating that
     they should be called at initialization- or destruction-time.  */
  if (priority == DEFAULT_INIT_PRIORITY)
    {
      tree fns;

      for (fns = constructor_p ? static_ctors : static_dtors; 
	   fns;
	   fns = TREE_CHAIN (fns))
	{
	  fndecl = TREE_VALUE (fns);

	  /* Calls to pure/const functions will expand to nothing.  */
	  if (! (flags_from_decl_or_type (fndecl) & (ECF_CONST | ECF_PURE)))
	    {
	      if (! body)
		body = start_objects (function_key, priority);
	      finish_expr_stmt (build_function_call (fndecl, NULL_TREE));
	    }
	}
    }

  /* Close out the function.  */
  if (body)
    finish_objects (function_key, priority, body);
}

/* Generate constructor and destructor functions for the priority
   indicated by N.  */

static int
generate_ctor_and_dtor_functions_for_priority (splay_tree_node n, void * data)
{
  location_t *locus = data;
  int priority = (int) n->key;
  priority_info pi = (priority_info) n->value;

  /* Generate the functions themselves, but only if they are really
     needed.  */
  if (pi->initializations_p
      || (priority == DEFAULT_INIT_PRIORITY && static_ctors))
    generate_ctor_or_dtor_function (/*constructor_p=*/true, priority, locus);
  if (pi->destructions_p
      || (priority == DEFAULT_INIT_PRIORITY && static_dtors))
    generate_ctor_or_dtor_function (/*constructor_p=*/false, priority, locus);

  /* Keep iterating.  */
  return 0;
}

/* Called via LANGHOOK_CALLGRAPH_ANALYZE_EXPR.  It is supposed to mark
   decls referenced from frontend specific constructs; it will be called
   only for language-specific tree nodes.

   Here we must deal with member pointers.  */

tree
cxx_callgraph_analyze_expr (tree *tp, int *walk_subtrees ATTRIBUTE_UNUSED,
			    tree from ATTRIBUTE_UNUSED)
{
  tree t = *tp;

  if (flag_unit_at_a_time)
    switch (TREE_CODE (t))
      {
      case PTRMEM_CST:
	if (TYPE_PTRMEMFUNC_P (TREE_TYPE (t)))
	  cgraph_mark_needed_node (cgraph_node (PTRMEM_CST_MEMBER (t)));
	break;
      case BASELINK:
	if (TREE_CODE (BASELINK_FUNCTIONS (t)) == FUNCTION_DECL)
	  cgraph_mark_needed_node (cgraph_node (BASELINK_FUNCTIONS (t)));
	break;

      default:
	break;
      }

  return NULL;
}

/* This routine is called from the last rule in yyparse ().
   Its job is to create all the code needed to initialize and
   destroy the global aggregates.  We do the destruction
   first, since that way we only need to reverse the decls once.  */

void
finish_file (void)
{
  tree vars;
  bool reconsider;
  size_t i;
  location_t locus;
  unsigned ssdf_count = 0;

  locus = input_location;
  at_eof = 1;

  /* Bad parse errors.  Just forget about it.  */
  if (! global_bindings_p () || current_class_type || decl_namespace_list)
    return;

  if (pch_file)
    c_common_write_pch ();

  /* Otherwise, GDB can get confused, because in only knows
     about source for LINENO-1 lines.  */
  input_line -= 1;

  interface_unknown = 1;
  interface_only = 0;

  /* We now have to write out all the stuff we put off writing out.
     These include:

       o Template specializations that we have not yet instantiated,
         but which are needed.
       o Initialization and destruction for non-local objects with
         static storage duration.  (Local objects with static storage
	 duration are initialized when their scope is first entered,
	 and are cleaned up via atexit.)
       o Virtual function tables.  

     All of these may cause others to be needed.  For example,
     instantiating one function may cause another to be needed, and
     generating the initializer for an object may cause templates to be
     instantiated, etc., etc.  */

  timevar_push (TV_VARCONST);

  emit_support_tinfos ();
  
  do 
    {
      tree t;
      size_t n_old, n_new;

      reconsider = false;

      /* If there are templates that we've put off instantiating, do
	 them now.  */
      instantiate_pending_templates ();
      ggc_collect ();

      /* Write out virtual tables as required.  Note that writing out
  	 the virtual table for a template class may cause the
 	 instantiation of members of that class.  If we write out
 	 vtables then we remove the class from our list so we don't
 	 have to look at it again.  */
 
      while (keyed_classes != NULL_TREE
 	     && maybe_emit_vtables (TREE_VALUE (keyed_classes)))
 	{
  	  reconsider = true;
 	  keyed_classes = TREE_CHAIN (keyed_classes);
 	}
 
      t = keyed_classes;
      if (t != NULL_TREE)
 	{
 	  tree next = TREE_CHAIN (t);
 
 	  while (next)
 	    {
 	      if (maybe_emit_vtables (TREE_VALUE (next)))
 		{
 		  reconsider = true;
 		  TREE_CHAIN (t) = TREE_CHAIN (next);
 		}
 	      else
 		t = next;
 
 	      next = TREE_CHAIN (t);
 	    }
 	}
       
      /* Write out needed type info variables.  We have to be careful
 	 looping through unemitted decls, because emit_tinfo_decl may
 	 cause other variables to be needed.  We stick new elements
 	 (and old elements that we may need to reconsider) at the end
 	 of the array, then shift them back to the beginning once we're
 	 done.  */
  
      n_old = VARRAY_ACTIVE_SIZE (unemitted_tinfo_decls);
      for (i = 0; i < n_old; ++i)
  	{
  	  tree tinfo_decl = VARRAY_TREE (unemitted_tinfo_decls, i);
  	  if (emit_tinfo_decl (tinfo_decl))
 	    reconsider = true;
  	  else
  	    VARRAY_PUSH_TREE (unemitted_tinfo_decls, tinfo_decl);
  	}
  
      /* The only elements we want to keep are the new ones.  Copy
  	 them to the beginning of the array, then get rid of the
  	 leftovers.  */
      n_new = VARRAY_ACTIVE_SIZE (unemitted_tinfo_decls) - n_old;
      if (n_new)
	memmove (&VARRAY_TREE (unemitted_tinfo_decls, 0),
		 &VARRAY_TREE (unemitted_tinfo_decls, n_old),
		 n_new * sizeof (tree));
      memset (&VARRAY_TREE (unemitted_tinfo_decls, n_new),
  	      0, n_old * sizeof (tree));
      VARRAY_ACTIVE_SIZE (unemitted_tinfo_decls) = n_new;

      /* The list of objects with static storage duration is built up
	 in reverse order.  We clear STATIC_AGGREGATES so that any new
	 aggregates added during the initialization of these will be
	 initialized in the correct order when we next come around the
	 loop.  */
      vars = prune_vars_needing_no_initialization (&static_aggregates);

      if (vars)
	{
	  tree v;

	  /* We need to start a new initialization function each time
	     through the loop.  That's because we need to know which
	     vtables have been referenced, and TREE_SYMBOL_REFERENCED
	     isn't computed until a function is finished, and written
	     out.  That's a deficiency in the back-end.  When this is
	     fixed, these initialization functions could all become
	     inline, with resulting performance improvements.  */
	  tree ssdf_body;

	  /* Set the line and file, so that it is obviously not from
	     the source file.  */
	  input_location = locus;
	  ssdf_body = start_static_storage_duration_function (ssdf_count);

	  /* Make sure the back end knows about all the variables.  */
	  write_out_vars (vars);

	  /* First generate code to do all the initializations.  */
	  for (v = vars; v; v = TREE_CHAIN (v))
	    do_static_initialization (TREE_VALUE (v),
				      TREE_PURPOSE (v));

	  /* Then, generate code to do all the destructions.  Do these
	     in reverse order so that the most recently constructed
	     variable is the first destroyed.  If we're using
	     __cxa_atexit, then we don't need to do this; functions
	     were registered at initialization time to destroy the
	     local statics.  */
	  if (!flag_use_cxa_atexit)
	    {
	      vars = nreverse (vars);
	      for (v = vars; v; v = TREE_CHAIN (v))
		do_static_destruction (TREE_VALUE (v));
	    }
	  else
	    vars = NULL_TREE;

	  /* Finish up the static storage duration function for this
	     round.  */
	  input_location = locus;
	  finish_static_storage_duration_function (ssdf_body);

	  /* All those initializations and finalizations might cause
	     us to need more inline functions, more template
	     instantiations, etc.  */
	  reconsider = true;
	  ssdf_count++;
	  locus.line++;
	}
      
      for (i = 0; i < deferred_fns_used; ++i)
	{
	  tree decl = VARRAY_TREE (deferred_fns, i);

	  /* Does it need synthesizing?  */
	  if (DECL_ARTIFICIAL (decl) && ! DECL_INITIAL (decl)
	      && TREE_USED (decl)
	      && (! DECL_REALLY_EXTERN (decl) || DECL_INLINE (decl)))
	    {
	      /* Even though we're already at the top-level, we push
		 there again.  That way, when we pop back a few lines
		 hence, all of our state is restored.  Otherwise,
		 finish_function doesn't clean things up, and we end
		 up with CURRENT_FUNCTION_DECL set.  */
	      push_to_top_level ();
	      synthesize_method (decl);
	      pop_from_top_level ();
	      reconsider = true;
	    }

	  /* If the function has no body, avoid calling
	     import_export_decl.  On a system without weak symbols,
	     calling import_export_decl will make an inline template
	     instantiation "static", which will result in errors about
	     the use of undefined functions if there is no body for
	     the function.  */
	  if (!DECL_SAVED_TREE (decl))
	    continue;

	  import_export_decl (decl);

	  /* We lie to the back-end, pretending that some functions
	     are not defined when they really are.  This keeps these
	     functions from being put out unnecessarily.  But, we must
	     stop lying when the functions are referenced, or if they
	     are not comdat since they need to be put out now.  This
	     is done in a separate for cycle, because if some deferred
	     function is contained in another deferred function later
	     in deferred_fns varray, rest_of_compilation would skip
	     this function and we really cannot expand the same
	     function twice.  */
	  if (DECL_NOT_REALLY_EXTERN (decl)
	      && DECL_INITIAL (decl)
	      && DECL_NEEDED_P (decl))
	    DECL_EXTERNAL (decl) = 0;

	  /* If we're going to need to write this function out, and
	     there's already a body for it, create RTL for it now.
	     (There might be no body if this is a method we haven't
	     gotten around to synthesizing yet.)  */
	  if (!DECL_EXTERNAL (decl)
	      && DECL_NEEDED_P (decl)
	      && DECL_SAVED_TREE (decl)
	      && !TREE_ASM_WRITTEN (decl)
	      && (!flag_unit_at_a_time 
		  || !cgraph_node (decl)->local.finalized))
	    {
	      /* We will output the function; no longer consider it in this
		 loop.  */
	      DECL_DEFER_OUTPUT (decl) = 0;
	      /* Generate RTL for this function now that we know we
		 need it.  */
	      expand_or_defer_fn (decl);
	      /* If we're compiling -fsyntax-only pretend that this
		 function has been written out so that we don't try to
		 expand it again.  */
	      if (flag_syntax_only)
		TREE_ASM_WRITTEN (decl) = 1;
	      reconsider = true;
	    }
	}

      if (walk_namespaces (wrapup_globals_for_namespace, /*data=*/0))
	reconsider = true;

      /* Static data members are just like namespace-scope globals.  */
      for (i = 0; i < pending_statics_used; ++i) 
	{
	  tree decl = VARRAY_TREE (pending_statics, i);
	  if (var_finalized_p (decl))
	    continue;
	  import_export_decl (decl);
	  if (DECL_NOT_REALLY_EXTERN (decl) && ! DECL_IN_AGGR_P (decl))
	    DECL_EXTERNAL (decl) = 0;
	}
      if (pending_statics
	  && wrapup_global_declarations (&VARRAY_TREE (pending_statics, 0),
					 pending_statics_used))
	reconsider = true;

      if (cgraph_assemble_pending_functions ())
	reconsider = true;
    } 
  while (reconsider);

  /* All used inline functions must have a definition at this point.  */
  for (i = 0; i < deferred_fns_used; ++i)
    {
      tree decl = VARRAY_TREE (deferred_fns, i);

      if (TREE_USED (decl) && DECL_DECLARED_INLINE_P (decl)
	  && !(TREE_ASM_WRITTEN (decl) || DECL_SAVED_TREE (decl)
	       /* An explicit instantiation can be used to specify
	          that the body is in another unit. It will have
	          already verified there was a definition.  */
	       || DECL_EXPLICIT_INSTANTIATION (decl)))
	{
	  cp_warning_at ("inline function `%D' used but never defined", decl);
	  /* This symbol is effectively an "extern" declaration now.
	     This is not strictly necessary, but removes a duplicate
	     warning.  */
	  TREE_PUBLIC (decl) = 1;
	}
      
    }
  
  /* We give C linkage to static constructors and destructors.  */
  push_lang_context (lang_name_c);

  /* Generate initialization and destruction functions for all
     priorities for which they are required.  */
  if (priority_info_map)
    splay_tree_foreach (priority_info_map, 
			generate_ctor_and_dtor_functions_for_priority,
			/*data=*/&locus);
  else
    {
      
      if (static_ctors)
	generate_ctor_or_dtor_function (/*constructor_p=*/true,
					DEFAULT_INIT_PRIORITY, &locus);
      if (static_dtors)
	generate_ctor_or_dtor_function (/*constructor_p=*/false,
					DEFAULT_INIT_PRIORITY, &locus);
    }

  /* We're done with the splay-tree now.  */
  if (priority_info_map)
    splay_tree_delete (priority_info_map);

  /* We're done with static constructors, so we can go back to "C++"
     linkage now.  */
  pop_lang_context ();

  if (flag_unit_at_a_time)
    {
      cgraph_finalize_compilation_unit ();
      cgraph_optimize ();
    }

  /* Now, issue warnings about static, but not defined, functions,
     etc., and emit debugging information.  */
  walk_namespaces (wrapup_globals_for_namespace, /*data=*/&reconsider);
  if (pending_statics)
    check_global_declarations (&VARRAY_TREE (pending_statics, 0),
			       pending_statics_used);

  finish_repo ();

  /* The entire file is now complete.  If requested, dump everything
     to a file.  */
  {
    int flags;
    FILE *stream = dump_begin (TDI_all, &flags);

    if (stream)
      {
	dump_node (global_namespace, flags & ~TDF_SLIM, stream);
	dump_end (TDI_all, stream);
      }
  }
  
  timevar_pop (TV_VARCONST);

  if (flag_detailed_statistics)
    {
      dump_tree_statistics ();
      dump_time_statistics ();
    }
  input_location = locus;
}

/* FN is an OFFSET_REF, DOTSTAR_EXPR or MEMBER_REF indicating the
   function to call in parse-tree form; it has not yet been
   semantically analyzed.  ARGS are the arguments to the function.
   They have already been semantically analyzed.  */

tree
build_offset_ref_call_from_tree (tree fn, tree args)
{
  tree orig_fn;
  tree orig_args;
  tree expr;
  tree object;

  orig_fn = fn;
  orig_args = args;
  object = TREE_OPERAND (fn, 0);

  if (processing_template_decl)
    {
      my_friendly_assert (TREE_CODE (fn) == DOTSTAR_EXPR
			  || TREE_CODE (fn) == MEMBER_REF,
			  20030708);
      if (type_dependent_expression_p (fn)
	  || any_type_dependent_arguments_p (args))
	return build_min_nt (CALL_EXPR, fn, args);

      /* Transform the arguments and add the implicit "this"
	 parameter.  That must be done before the FN is transformed
	 because we depend on the form of FN.  */
      args = build_non_dependent_args (args);
      if (TREE_CODE (fn) == DOTSTAR_EXPR)
	object = build_unary_op (ADDR_EXPR, object, 0);
      object = build_non_dependent_expr (object);
      args = tree_cons (NULL_TREE, object, args);
      /* Now that the arguments are done, transform FN.  */
      fn = build_non_dependent_expr (fn);
    }

  /* A qualified name corresponding to a bound pointer-to-member is
     represented as an OFFSET_REF:

	struct B { void g(); };
	void (B::*p)();
	void B::g() { (this->*p)(); }  */
  if (TREE_CODE (fn) == OFFSET_REF)
    {
      tree object_addr = build_unary_op (ADDR_EXPR, object, 0);
      fn = TREE_OPERAND (fn, 1);
      fn = get_member_function_from_ptrfunc (&object_addr, fn);
      args = tree_cons (NULL_TREE, object_addr, args);
    }

  expr = build_function_call (fn, args);
  if (processing_template_decl && expr != error_mark_node)
    return build_min_non_dep (CALL_EXPR, expr, orig_fn, orig_args);
  return expr;
}
  

void
check_default_args (tree x)
{
  tree arg = TYPE_ARG_TYPES (TREE_TYPE (x));
  bool saw_def = false;
  int i = 0 - (TREE_CODE (TREE_TYPE (x)) == METHOD_TYPE);
  for (; arg && arg != void_list_node; arg = TREE_CHAIN (arg), ++i)
    {
      if (TREE_PURPOSE (arg))
	saw_def = true;
      else if (saw_def)
	{
	  cp_error_at ("default argument missing for parameter %P of `%+#D'",
		       i, x);
	  break;
	}
    }
}

void
mark_used (tree decl)
{
  TREE_USED (decl) = 1;
  if (processing_template_decl || skip_evaluation)
    return;

  if (TREE_CODE (decl) == FUNCTION_DECL && DECL_DECLARED_INLINE_P (decl)
      && !TREE_ASM_WRITTEN (decl))
    /* Remember it, so we can check it was defined.  */
    defer_fn (decl);

  assemble_external (decl);

  /* Is it a synthesized method that needs to be synthesized?  */
  if (TREE_CODE (decl) == FUNCTION_DECL
      && DECL_NONSTATIC_MEMBER_FUNCTION_P (decl)
      && DECL_ARTIFICIAL (decl) 
      && !DECL_THUNK_P (decl)
      && ! DECL_INITIAL (decl)
      /* Kludge: don't synthesize for default args.  */
      && current_function_decl)
    {
      synthesize_method (decl);
      /* If we've already synthesized the method we don't need to
	 instantiate it, so we can return right away.  */
      return;
    }

  /* If this is a function or variable that is an instance of some
     template, we now know that we will need to actually do the
     instantiation. We check that DECL is not an explicit
     instantiation because that is not checked in instantiate_decl.  */
  if ((DECL_NON_THUNK_FUNCTION_P (decl) || TREE_CODE (decl) == VAR_DECL)
      && DECL_LANG_SPECIFIC (decl) && DECL_TEMPLATE_INFO (decl)
      && (!DECL_EXPLICIT_INSTANTIATION (decl)
	  || (TREE_CODE (decl) == FUNCTION_DECL 
	      && DECL_INLINE (DECL_TEMPLATE_RESULT 
			      (template_for_substitution (decl))))))
    {
      bool defer;

      /* Normally, we put off instantiating functions in order to
	 improve compile times.  Maintaining a stack of active
	 functions is expensive, and the inliner knows to
	 instantiate any functions it might need.

	 However, if instantiating this function might help us mark
	 the current function TREE_NOTHROW, we go ahead and
	 instantiate it now.  
	 
	 This is not needed for unit-at-a-time since we reorder the functions
	 in topological order anyway.
	 */
      defer = (!flag_exceptions
	       || flag_unit_at_a_time
	       || !optimize
	       || TREE_CODE (decl) != FUNCTION_DECL
	       /* If the called function can't throw, we don't need to
		  generate its body to find that out.  */
	       || TREE_NOTHROW (decl)
	       || !cfun
	       || !current_function_decl
	       /* If we already know the current function can't throw,
		  then we don't need to work hard to prove it.  */
	       || TREE_NOTHROW (current_function_decl)
	       /* If we already know that the current function *can*
		  throw, there's no point in gathering more
		  information.  */
	       || cp_function_chain->can_throw);

      instantiate_decl (decl, defer);
    }
}

#include "gt-cp-decl2.h"
