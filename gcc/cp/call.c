/* Functions related to invoking methods and overloaded functions.
   Copyright (C) 1987, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com) and
   modified by Brendan Kehoe (brendan@cygnus.com).

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
#include "output.h"
#include "flags.h"
#include "rtl.h"
#include "toplev.h"
#include "defaults.h"
#include "expr.h"

#include "obstack.h"
#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

extern int inhibit_warnings;

static tree build_new_method_call PARAMS ((tree, tree, tree, tree, int));

static tree build_field_call PARAMS ((tree, tree, tree, tree));
static struct z_candidate * tourney PARAMS ((struct z_candidate *));
static int equal_functions PARAMS ((tree, tree));
static int joust PARAMS ((struct z_candidate *, struct z_candidate *, int));
static int compare_ics PARAMS ((tree, tree));
static tree build_over_call PARAMS ((struct z_candidate *, tree, int));
static tree convert_like PARAMS ((tree, tree));
static void op_error PARAMS ((enum tree_code, enum tree_code, tree, tree,
			    tree, const char *));
static tree build_object_call PARAMS ((tree, tree));
static tree resolve_args PARAMS ((tree));
static struct z_candidate * build_user_type_conversion_1
	PARAMS ((tree, tree, int));
static void print_z_candidates PARAMS ((struct z_candidate *));
static tree build_this PARAMS ((tree));
static struct z_candidate * splice_viable PARAMS ((struct z_candidate *));
static int any_viable PARAMS ((struct z_candidate *));
static struct z_candidate * add_template_candidate
	PARAMS ((struct z_candidate *, tree, tree, tree, tree, tree, int,
	       unification_kind_t));
static struct z_candidate * add_template_candidate_real
	PARAMS ((struct z_candidate *, tree, tree, tree, tree, tree, int,
	       tree, unification_kind_t));
static struct z_candidate * add_template_conv_candidate 
        PARAMS ((struct z_candidate *, tree, tree, tree, tree));
static struct z_candidate * add_builtin_candidates
	PARAMS ((struct z_candidate *, enum tree_code, enum tree_code,
	       tree, tree *, int));
static struct z_candidate * add_builtin_candidate
	PARAMS ((struct z_candidate *, enum tree_code, enum tree_code,
	       tree, tree, tree, tree *, tree *, int));
static int is_complete PARAMS ((tree));
static struct z_candidate * build_builtin_candidate 
	PARAMS ((struct z_candidate *, tree, tree, tree, tree *, tree *,
	       int));
static struct z_candidate * add_conv_candidate 
	PARAMS ((struct z_candidate *, tree, tree, tree));
static struct z_candidate * add_function_candidate 
	PARAMS ((struct z_candidate *, tree, tree, tree, int));
static tree implicit_conversion PARAMS ((tree, tree, tree, int));
static tree standard_conversion PARAMS ((tree, tree, tree));
static tree reference_binding PARAMS ((tree, tree, tree, int));
static tree non_reference PARAMS ((tree));
static tree build_conv PARAMS ((enum tree_code, tree, tree));
static int is_subseq PARAMS ((tree, tree));
static int maybe_handle_ref_bind PARAMS ((tree*, tree*));
static void maybe_handle_implicit_object PARAMS ((tree*));
static struct z_candidate * add_candidate PARAMS ((struct z_candidate *,
						   tree, tree, int));
static tree source_type PARAMS ((tree));
static void add_warning PARAMS ((struct z_candidate *, struct z_candidate *));
static int reference_related_p PARAMS ((tree, tree));
static int reference_compatible_p PARAMS ((tree, tree));
static tree convert_class_to_reference PARAMS ((tree, tree, tree));
static tree direct_reference_binding PARAMS ((tree, tree));
static int promoted_arithmetic_type_p PARAMS ((tree));
static tree conditional_conversion PARAMS ((tree, tree));

tree
build_vfield_ref (datum, type)
     tree datum, type;
{
  tree rval;

  if (datum == error_mark_node)
    return error_mark_node;

  if (TREE_CODE (TREE_TYPE (datum)) == REFERENCE_TYPE)
    datum = convert_from_reference (datum);

  if (! TYPE_BASE_CONVS_MAY_REQUIRE_CODE_P (type))
    rval = build (COMPONENT_REF, TREE_TYPE (TYPE_VFIELD (type)),
		  datum, TYPE_VFIELD (type));
  else
    rval = build_component_ref (datum, DECL_NAME (TYPE_VFIELD (type)), NULL_TREE, 0);

  return rval;
}

/* Build a call to a member of an object.  I.e., one that overloads
   operator ()(), or is a pointer-to-function or pointer-to-method.  */

static tree
build_field_call (basetype_path, instance_ptr, name, parms)
     tree basetype_path, instance_ptr, name, parms;
{
  tree field, instance;

  if (name == ctor_identifier || name == dtor_identifier)
    return NULL_TREE;

  /* Speed up the common case.  */
  if (instance_ptr == current_class_ptr
      && IDENTIFIER_CLASS_VALUE (name) == NULL_TREE)
    return NULL_TREE;

  field = lookup_field (basetype_path, name, 1, 0);

  if (field == error_mark_node || field == NULL_TREE)
    return field;

  if (TREE_CODE (field) == FIELD_DECL || TREE_CODE (field) == VAR_DECL)
    {
      /* If it's a field, try overloading operator (),
	 or calling if the field is a pointer-to-function.  */
      instance = build_indirect_ref (instance_ptr, NULL_PTR);
      instance = build_component_ref_1 (instance, field, 0);

      if (instance == error_mark_node)
	return error_mark_node;

      if (IS_AGGR_TYPE (TREE_TYPE (instance)))
	return build_opfncall (CALL_EXPR, LOOKUP_NORMAL,
			       instance, parms, NULL_TREE);
      else if (TREE_CODE (TREE_TYPE (instance)) == POINTER_TYPE)
	{
	  if (TREE_CODE (TREE_TYPE (TREE_TYPE (instance))) == FUNCTION_TYPE)
	    return build_function_call (instance, parms);
	  else if (TREE_CODE (TREE_TYPE (TREE_TYPE (instance)))
		   == METHOD_TYPE)
	    return build_function_call
	      (instance, tree_cons (NULL_TREE, instance_ptr, parms));
	}
    }

  return NULL_TREE;
}

/* Returns nonzero iff the destructor name specified in NAME
   (a BIT_NOT_EXPR) matches BASETYPE.  The operand of NAME can take many
   forms...  */

int
check_dtor_name (basetype, name)
     tree basetype, name;
{
  name = TREE_OPERAND (name, 0);

  /* Just accept something we've already complained about.  */
  if (name == error_mark_node)
    return 1;

  if (TREE_CODE (name) == TYPE_DECL)
    name = TREE_TYPE (name);
  else if (TREE_CODE_CLASS (TREE_CODE (name)) == 't')
    /* OK */;
  else if (TREE_CODE (name) == IDENTIFIER_NODE)
    {
      if ((IS_AGGR_TYPE (basetype) && name == constructor_name (basetype))
	  || (TREE_CODE (basetype) == ENUMERAL_TYPE
	      && name == TYPE_IDENTIFIER (basetype)))
	name = basetype;
      else
	name = get_type_value (name);
    }
  else
    my_friendly_abort (980605);

  if (name && TYPE_MAIN_VARIANT (basetype) == TYPE_MAIN_VARIANT (name))
    return 1;
  return 0;
}

/* Build a method call of the form `EXP->SCOPES::NAME (PARMS)'.
   This is how virtual function calls are avoided.  */

tree
build_scoped_method_call (exp, basetype, name, parms)
     tree exp, basetype, name, parms;
{
  /* Because this syntactic form does not allow
     a pointer to a base class to be `stolen',
     we need not protect the derived->base conversion
     that happens here.
     
     @@ But we do have to check access privileges later.  */
  tree binfo, decl;
  tree type = TREE_TYPE (exp);

  if (type == error_mark_node
      || basetype == error_mark_node)
    return error_mark_node;

  if (processing_template_decl)
    {
      if (TREE_CODE (name) == BIT_NOT_EXPR
	  && TREE_CODE (TREE_OPERAND (name, 0)) == IDENTIFIER_NODE)
	{
	  tree type = get_aggr_from_typedef (TREE_OPERAND (name, 0), 0);
	  if (type)
	    name = build_min_nt (BIT_NOT_EXPR, type);
	}
      name = build_min_nt (SCOPE_REF, basetype, name);
      return build_min_nt (METHOD_CALL_EXPR, name, exp, parms, NULL_TREE);
    }

  if (TREE_CODE (type) == REFERENCE_TYPE)
    type = TREE_TYPE (type);

  if (TREE_CODE (basetype) == TREE_VEC)
    {
      binfo = basetype;
      basetype = BINFO_TYPE (binfo);
    }
  else
    binfo = NULL_TREE;

  /* Check the destructor call syntax.  */
  if (TREE_CODE (name) == BIT_NOT_EXPR)
    {
      /* We can get here if someone writes their destructor call like
	 `obj.NS::~T()'; this isn't really a scoped method call, so hand
	 it off.  */
      if (TREE_CODE (basetype) == NAMESPACE_DECL)
	return build_method_call (exp, name, parms, NULL_TREE, LOOKUP_NORMAL);

      if (! check_dtor_name (basetype, name))
	cp_error ("qualified type `%T' does not match destructor name `~%T'",
		  basetype, TREE_OPERAND (name, 0));

      /* Destructors can be "called" for simple types; see 5.2.4 and 12.4 Note
	 that explicit ~int is caught in the parser; this deals with typedefs
	 and template parms.  */
      if (! IS_AGGR_TYPE (basetype))
	{
	  if (TYPE_MAIN_VARIANT (type) != TYPE_MAIN_VARIANT (basetype))
	    cp_error ("type of `%E' does not match destructor type `%T' (type was `%T')",
		      exp, basetype, type);

	  return cp_convert (void_type_node, exp);
	}
    }

  if (! is_aggr_type (basetype, 1))
    return error_mark_node;

  if (! IS_AGGR_TYPE (type))
    {
      cp_error ("base object `%E' of scoped method call is of non-aggregate type `%T'",
		exp, type);
      return error_mark_node;
    }

  if (! binfo)
    {
      binfo = get_binfo (basetype, type, 1);
      if (binfo == error_mark_node)
	return error_mark_node;
      if (! binfo)
	error_not_base_type (basetype, type);
    }

  if (binfo)
    {
      if (TREE_CODE (exp) == INDIRECT_REF)
	decl = build_indirect_ref
	  (convert_pointer_to_real
	   (binfo, build_unary_op (ADDR_EXPR, exp, 0)), NULL_PTR);
      else
	decl = build_scoped_ref (exp, basetype);

      /* Call to a destructor.  */
      if (TREE_CODE (name) == BIT_NOT_EXPR)
	{
	  if (! TYPE_HAS_DESTRUCTOR (TREE_TYPE (decl)))
	    return cp_convert (void_type_node, exp);
	  
	  return build_delete (TREE_TYPE (decl), decl, integer_two_node,
			       LOOKUP_NORMAL|LOOKUP_NONVIRTUAL|LOOKUP_DESTRUCTOR,
			       0);
	}

      /* Call to a method.  */
      return build_method_call (decl, name, parms, binfo,
				LOOKUP_NORMAL|LOOKUP_NONVIRTUAL);
    }
  return error_mark_node;
}

/* We want the address of a function or method.  We avoid creating a
   pointer-to-member function.  */

tree
build_addr_func (function)
     tree function;
{
  tree type = TREE_TYPE (function);

  /* We have to do these by hand to avoid real pointer to member
     functions.  */
  if (TREE_CODE (type) == METHOD_TYPE)
    {
      tree addr;

      type = build_pointer_type (type);

      if (mark_addressable (function) == 0)
	return error_mark_node;

      addr = build1 (ADDR_EXPR, type, function);

      /* Address of a static or external variable or function counts
	 as a constant */
      if (staticp (function))
	TREE_CONSTANT (addr) = 1;

      function = addr;
    }
  else
    function = default_conversion (function);

  return function;
}

/* Build a CALL_EXPR, we can handle FUNCTION_TYPEs, METHOD_TYPEs, or
   POINTER_TYPE to those.  Note, pointer to member function types
   (TYPE_PTRMEMFUNC_P) must be handled by our callers.  */

tree
build_call (function, result_type, parms)
     tree function, result_type, parms;
{
  int is_constructor = 0;
  tree tmp;
  tree decl;

  function = build_addr_func (function);

  if (TYPE_PTRMEMFUNC_P (TREE_TYPE (function)))
    {
      sorry ("unable to call pointer to member function here");
      return error_mark_node;
    }

  if (TREE_CODE (function) == ADDR_EXPR
      && TREE_CODE (TREE_OPERAND (function, 0)) == FUNCTION_DECL)
    decl = TREE_OPERAND (function, 0);
  else
    decl = NULL_TREE;

  if (decl && DECL_CONSTRUCTOR_P (decl))
    is_constructor = 1;

  if (decl)
    my_friendly_assert (TREE_USED (decl), 990125);

  /* Don't pass empty class objects by value.  This is useful
     for tags in STL, which are used to control overload resolution.
     We don't need to handle other cases of copying empty classes.  */
  if (! decl || ! DECL_BUILT_IN (decl))
    for (tmp = parms; tmp; tmp = TREE_CHAIN (tmp))
      if (is_empty_class (TREE_TYPE (TREE_VALUE (tmp)))
	  && ! TREE_ADDRESSABLE (TREE_TYPE (TREE_VALUE (tmp))))
	{
	  tree t = build (EMPTY_CLASS_EXPR, TREE_TYPE (TREE_VALUE (tmp)));
	  TREE_VALUE (tmp) = build (COMPOUND_EXPR, TREE_TYPE (t),
				    TREE_VALUE (tmp), t);
	}

  function = build_nt (CALL_EXPR, function, parms, NULL_TREE);
  TREE_HAS_CONSTRUCTOR (function) = is_constructor;
  TREE_TYPE (function) = result_type;
  TREE_SIDE_EFFECTS (function) = 1;
  
  return function;
}

/* Build something of the form ptr->method (args)
   or object.method (args).  This can also build
   calls to constructors, and find friends.

   Member functions always take their class variable
   as a pointer.

   INSTANCE is a class instance.

   NAME is the name of the method desired, usually an IDENTIFIER_NODE.

   PARMS help to figure out what that NAME really refers to.

   BASETYPE_PATH, if non-NULL, contains a chain from the type of INSTANCE
   down to the real instance type to use for access checking.  We need this
   information to get protected accesses correct.  This parameter is used
   by build_member_call.

   FLAGS is the logical disjunction of zero or more LOOKUP_
   flags.  See cp-tree.h for more info.

   If this is all OK, calls build_function_call with the resolved
   member function.

   This function must also handle being called to perform
   initialization, promotion/coercion of arguments, and
   instantiation of default parameters.

   Note that NAME may refer to an instance variable name.  If
   `operator()()' is defined for the type of that field, then we return
   that result.  */

#ifdef GATHER_STATISTICS
extern int n_build_method_call;
#endif

tree
build_method_call (instance, name, parms, basetype_path, flags)
     tree instance, name, parms, basetype_path;
     int flags;
{
  tree basetype, instance_ptr;

#ifdef GATHER_STATISTICS
  n_build_method_call++;
#endif

  if (instance == error_mark_node
      || name == error_mark_node
      || parms == error_mark_node
      || (instance != NULL_TREE && TREE_TYPE (instance) == error_mark_node))
    return error_mark_node;

  if (processing_template_decl)
    {
      /* We need to process template parm names here so that tsubst catches
	 them properly.  Other type names can wait.  */
      if (TREE_CODE (name) == BIT_NOT_EXPR)
	{
	  tree type = NULL_TREE;

	  if (TREE_CODE (TREE_OPERAND (name, 0)) == IDENTIFIER_NODE)
	    type = get_aggr_from_typedef (TREE_OPERAND (name, 0), 0);
	  else if (TREE_CODE (TREE_OPERAND (name, 0)) == TYPE_DECL)
	    type = TREE_TYPE (TREE_OPERAND (name, 0));

	  if (type && TREE_CODE (type) == TEMPLATE_TYPE_PARM)
	    name = build_min_nt (BIT_NOT_EXPR, type);
	}

      return build_min_nt (METHOD_CALL_EXPR, name, instance, parms, NULL_TREE);
    }

  if (TREE_CODE (name) == BIT_NOT_EXPR)
    {
      if (parms)
	error ("destructors take no parameters");
      basetype = TREE_TYPE (instance);
      if (TREE_CODE (basetype) == REFERENCE_TYPE)
	basetype = TREE_TYPE (basetype);

      if (! check_dtor_name (basetype, name))
	cp_error
	  ("destructor name `~%T' does not match type `%T' of expression",
	   TREE_OPERAND (name, 0), basetype);

      if (! TYPE_HAS_DESTRUCTOR (complete_type (basetype)))
	return cp_convert (void_type_node, instance);
      instance = default_conversion (instance);
      instance_ptr = build_unary_op (ADDR_EXPR, instance, 0);
      return build_delete (build_pointer_type (basetype),
			   instance_ptr, integer_two_node,
			   LOOKUP_NORMAL|LOOKUP_DESTRUCTOR, 0);
    }

  return build_new_method_call (instance, name, parms, basetype_path, flags);
}

/* New overloading code.  */

struct z_candidate {
  tree fn;
  tree convs;
  tree second_conv;
  int viable;
  tree basetype_path;
  tree template;
  tree warnings;
  struct z_candidate *next;
};

#define IDENTITY_RANK 0
#define EXACT_RANK 1
#define PROMO_RANK 2
#define STD_RANK 3
#define PBOOL_RANK 4
#define USER_RANK 5
#define ELLIPSIS_RANK 6
#define BAD_RANK 7

#define ICS_RANK(NODE)				\
  (ICS_BAD_FLAG (NODE) ? BAD_RANK   \
   : ICS_ELLIPSIS_FLAG (NODE) ? ELLIPSIS_RANK	\
   : ICS_USER_FLAG (NODE) ? USER_RANK		\
   : ICS_STD_RANK (NODE))

#define ICS_STD_RANK(NODE) TREE_COMPLEXITY (NODE)

#define ICS_USER_FLAG(NODE) TREE_LANG_FLAG_0 (NODE)
#define ICS_ELLIPSIS_FLAG(NODE) TREE_LANG_FLAG_1 (NODE)
#define ICS_THIS_FLAG(NODE) TREE_LANG_FLAG_2 (NODE)
#define ICS_BAD_FLAG(NODE) TREE_LANG_FLAG_3 (NODE)

/* In a REF_BIND or a BASE_CONV, this indicates that a temporary
   should be created to hold the result of the conversion.  */
#define NEED_TEMPORARY_P(NODE) (TREE_LANG_FLAG_4 ((NODE)))

#define USER_CONV_CAND(NODE) \
  ((struct z_candidate *)WRAPPER_PTR (TREE_OPERAND (NODE, 1)))
#define USER_CONV_FN(NODE) (USER_CONV_CAND (NODE)->fn)

int
null_ptr_cst_p (t)
     tree t;
{
  /* [conv.ptr]

     A null pointer constant is an integral constant expression
     (_expr.const_) rvalue of integer type that evaluates to zero.  */
  if (t == null_node
      || (CP_INTEGRAL_TYPE_P (TREE_TYPE (t)) && integer_zerop (t)))
    return 1;
  return 0;
}

static tree
build_conv (code, type, from)
     enum tree_code code;
     tree type, from;
{
  tree t = build1 (code, type, from);
  int rank = ICS_STD_RANK (from);
  switch (code)
    {
    case PTR_CONV:
    case PMEM_CONV:
    case BASE_CONV:
    case STD_CONV:
      if (rank < STD_RANK)
	rank = STD_RANK;
      break;

    case QUAL_CONV:
      if (rank < EXACT_RANK)
	rank = EXACT_RANK;

    default:
      break;
    }
  ICS_STD_RANK (t) = rank;
  ICS_USER_FLAG (t) = ICS_USER_FLAG (from);
  ICS_BAD_FLAG (t) = ICS_BAD_FLAG (from);
  return t;
}

static tree
non_reference (t)
     tree t;
{
  if (TREE_CODE (t) == REFERENCE_TYPE)
    t = TREE_TYPE (t);
  return t;
}

tree
strip_top_quals (t)
     tree t;
{
  if (TREE_CODE (t) == ARRAY_TYPE)
    return t;
  return TYPE_MAIN_VARIANT (t);
}

/* Returns the standard conversion path (see [conv]) from type FROM to type
   TO, if any.  For proper handling of null pointer constants, you must
   also pass the expression EXPR to convert from.  */

static tree
standard_conversion (to, from, expr)
     tree to, from, expr;
{
  enum tree_code fcode, tcode;
  tree conv;
  int fromref = 0;

  if (TREE_CODE (to) == REFERENCE_TYPE)
    to = TREE_TYPE (to);
  if (TREE_CODE (from) == REFERENCE_TYPE)
    {
      fromref = 1;
      from = TREE_TYPE (from);
    }
  to = strip_top_quals (to);
  from = strip_top_quals (from);

  if ((TYPE_PTRFN_P (to) || TYPE_PTRMEMFUNC_P (to))
      && expr && type_unknown_p (expr))
    {
      expr = instantiate_type (to, expr, 0);
      if (expr == error_mark_node)
	return NULL_TREE;
      from = TREE_TYPE (expr);
    }

  fcode = TREE_CODE (from);
  tcode = TREE_CODE (to);

  conv = build1 (IDENTITY_CONV, from, expr);

  if (fcode == FUNCTION_TYPE)
    {
      from = build_pointer_type (from);
      fcode = TREE_CODE (from);
      conv = build_conv (LVALUE_CONV, from, conv);
    }
  else if (fcode == ARRAY_TYPE)
    {
      from = build_pointer_type (TREE_TYPE (from));
      fcode = TREE_CODE (from);
      conv = build_conv (LVALUE_CONV, from, conv);
    }
  else if (fromref || (expr && real_lvalue_p (expr)))
    conv = build_conv (RVALUE_CONV, from, conv);

  if (same_type_p (from, to))
    return conv;

  if ((tcode == POINTER_TYPE || TYPE_PTRMEMFUNC_P (to))
      && expr && null_ptr_cst_p (expr))
    {
      conv = build_conv (STD_CONV, to, conv);
    }
  else if (tcode == POINTER_TYPE && fcode == POINTER_TYPE)
    {
      enum tree_code ufcode = TREE_CODE (TREE_TYPE (from));
      enum tree_code utcode = TREE_CODE (TREE_TYPE (to));

      if (same_type_p (TYPE_MAIN_VARIANT (TREE_TYPE (from)),
		       TYPE_MAIN_VARIANT (TREE_TYPE (to))))
	;
      else if (utcode == VOID_TYPE && ufcode != OFFSET_TYPE
	       && ufcode != FUNCTION_TYPE)
	{
	  from = build_pointer_type
	    (cp_build_qualified_type (void_type_node, 
				      CP_TYPE_QUALS (TREE_TYPE (from))));
	  conv = build_conv (PTR_CONV, from, conv);
	}
      else if (ufcode == OFFSET_TYPE && utcode == OFFSET_TYPE)
	{
	  tree fbase = TYPE_OFFSET_BASETYPE (TREE_TYPE (from));
	  tree tbase = TYPE_OFFSET_BASETYPE (TREE_TYPE (to));

	  if (DERIVED_FROM_P (fbase, tbase)
	      && (same_type_p 
		  (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (from))),
		   TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (to))))))
	    {
	      from = build_offset_type (tbase, TREE_TYPE (TREE_TYPE (from)));
	      from = build_pointer_type (from);
	      conv = build_conv (PMEM_CONV, from, conv);
	    }
	}
      else if (IS_AGGR_TYPE (TREE_TYPE (from))
	       && IS_AGGR_TYPE (TREE_TYPE (to)))
	{
	  if (DERIVED_FROM_P (TREE_TYPE (to), TREE_TYPE (from)))
	    {
	      from = 
		cp_build_qualified_type (TREE_TYPE (to),
					 CP_TYPE_QUALS (TREE_TYPE (from)));
	      from = build_pointer_type (from);
	      conv = build_conv (PTR_CONV, from, conv);
	    }
	}

      if (same_type_p (from, to))
	/* OK */;
      else if (comp_ptr_ttypes (TREE_TYPE (to), TREE_TYPE (from)))
	conv = build_conv (QUAL_CONV, to, conv);
      else if (expr && string_conv_p (to, expr, 0))
	/* converting from string constant to char *.  */
	conv = build_conv (QUAL_CONV, to, conv);
      else if (ptr_reasonably_similar (TREE_TYPE (to), TREE_TYPE (from)))
	{
	  conv = build_conv (PTR_CONV, to, conv);
	  ICS_BAD_FLAG (conv) = 1;
	}
      else
	return 0;

      from = to;
    }
  else if (TYPE_PTRMEMFUNC_P (to) && TYPE_PTRMEMFUNC_P (from))
    {
      tree fromfn = TREE_TYPE (TYPE_PTRMEMFUNC_FN_TYPE (from));
      tree tofn = TREE_TYPE (TYPE_PTRMEMFUNC_FN_TYPE (to));
      tree fbase = TREE_TYPE (TREE_VALUE (TYPE_ARG_TYPES (fromfn)));
      tree tbase = TREE_TYPE (TREE_VALUE (TYPE_ARG_TYPES (tofn)));

      if (! DERIVED_FROM_P (fbase, tbase)
	  || ! same_type_p (TREE_TYPE (fromfn), TREE_TYPE (tofn))
	  || ! compparms (TREE_CHAIN (TYPE_ARG_TYPES (fromfn)),
			  TREE_CHAIN (TYPE_ARG_TYPES (tofn)))
	  || CP_TYPE_QUALS (fbase) != CP_TYPE_QUALS (tbase))
	return 0;

      from = cp_build_qualified_type (tbase, CP_TYPE_QUALS (fbase));
      from = build_cplus_method_type (from, TREE_TYPE (fromfn),
				      TREE_CHAIN (TYPE_ARG_TYPES (fromfn)));
      from = build_ptrmemfunc_type (build_pointer_type (from));
      conv = build_conv (PMEM_CONV, from, conv);
    }
  else if (tcode == BOOLEAN_TYPE)
    {
      if (! (INTEGRAL_CODE_P (fcode) || fcode == REAL_TYPE
	     || fcode == POINTER_TYPE || TYPE_PTRMEMFUNC_P (from)))
	return 0;

      conv = build_conv (STD_CONV, to, conv);
      if (fcode == POINTER_TYPE
	  || (TYPE_PTRMEMFUNC_P (from) && ICS_STD_RANK (conv) < PBOOL_RANK))
	ICS_STD_RANK (conv) = PBOOL_RANK;
    }
  /* We don't check for ENUMERAL_TYPE here because there are no standard
     conversions to enum type.  */
  else if (tcode == INTEGER_TYPE || tcode == BOOLEAN_TYPE
	   || tcode == REAL_TYPE)
    {
      if (! (INTEGRAL_CODE_P (fcode) || fcode == REAL_TYPE))
	return 0;
      conv = build_conv (STD_CONV, to, conv);

      /* Give this a better rank if it's a promotion.  */
      if (to == type_promotes_to (from)
	  && ICS_STD_RANK (TREE_OPERAND (conv, 0)) <= PROMO_RANK)
	ICS_STD_RANK (conv) = PROMO_RANK;
    }
  else if (IS_AGGR_TYPE (to) && IS_AGGR_TYPE (from)
	   && is_properly_derived_from (from, to))
    {
      if (TREE_CODE (conv) == RVALUE_CONV)
	conv = TREE_OPERAND (conv, 0);
      conv = build_conv (BASE_CONV, to, conv);
      /* The derived-to-base conversion indicates the initialization
	 of a parameter with base type from an object of a derived
	 type.  A temporary object is created to hold the result of
	 the conversion.  */
      NEED_TEMPORARY_P (conv) = 1;
    }
  else
    return 0;

  return conv;
}

/* Returns non-zero if T1 is reference-related to T2.  */

static int
reference_related_p (t1, t2)
     tree t1;
     tree t2;
{
  t1 = TYPE_MAIN_VARIANT (t1);
  t2 = TYPE_MAIN_VARIANT (t2);

  /* [dcl.init.ref]

     Given types "cv1 T1" and "cv2 T2," "cv1 T1" is reference-related
     to "cv2 T2" if T1 is the same type as T2, or T1 is a base class
     of T2.  */
  return (same_type_p (t1, t2)
	  || (CLASS_TYPE_P (t1) && CLASS_TYPE_P (t2)
	      && DERIVED_FROM_P (t1, t2)));
}

/* Returns non-zero if T1 is reference-compatible with T2.  */

static int
reference_compatible_p (t1, t2)
     tree t1;
     tree t2;
{
  /* [dcl.init.ref]

     "cv1 T1" is reference compatible with "cv2 T2" if T1 is
     reference-related to T2 and cv1 is the same cv-qualification as,
     or greater cv-qualification than, cv2.  */
  return (reference_related_p (t1, t2)
	  && at_least_as_qualified_p (t1, t2));
}

/* Determine whether or not the EXPR (of class type S) can be
   converted to T as in [over.match.ref].  */

static tree
convert_class_to_reference (t, s, expr)
     tree t;
     tree s;
     tree expr;
{
  tree conversions;
  tree arglist;
  tree conv;
  struct z_candidate *candidates;
  struct z_candidate *cand;

  /* [over.match.ref]

     Assuming that "cv1 T" is the underlying type of the reference
     being initialized, and "cv S" is the type of the initializer
     expression, with S a class type, the candidate functions are
     selected as follows:

     --The conversion functions of S and its base classes are
       considered.  Those that are not hidden within S and yield type
       "reference to cv2 T2", where "cv1 T" is reference-compatible
       (_dcl.init.ref_) with "cv2 T2", are candidate functions.

     The argument list has one argument, which is the initializer
     expression.  */

  candidates = 0;

  /* Conceptually, we should take the address of EXPR and put it in
     the argument list.  Unfortunately, however, that can result in
     error messages, which we should not issue now because we are just
     trying to find a conversion operator.  Therefore, we use NULL,
     cast to the appropriate type.  */
  arglist = build_int_2 (0, 0);
  TREE_TYPE (arglist) = build_pointer_type (s);
  arglist = build_tree_list (NULL_TREE, arglist);
  
  for (conversions = lookup_conversions (s);
       conversions;
       conversions = TREE_CHAIN (conversions))
    {
      tree fns = TREE_VALUE (conversions);

      for (; fns; fns = OVL_NEXT (fns))
	{
	  tree f = OVL_CURRENT (fns);
	  tree t2 = TREE_TYPE (TREE_TYPE (f));
	  struct z_candidate *old_candidates = candidates;

	  /* If this is a template function, try to get an exact
             match.  */
	  if (TREE_CODE (f) == TEMPLATE_DECL)
	    {
	      candidates 
		= add_template_candidate (candidates,
					  f, s,
					  NULL_TREE,
					  arglist,
					  build_reference_type (t),
					  LOOKUP_NORMAL,
					  DEDUCE_CONV);
	      
	      if (candidates != old_candidates)
		{
		  /* Now, see if the conversion function really returns
		     an lvalue of the appropriate type.  From the
		     point of view of unification, simply returning an
		     rvalue of the right type is good enough.  */
		  f = candidates->fn;
		  t2 = TREE_TYPE (TREE_TYPE (f));
		  if (TREE_CODE (t2) != REFERENCE_TYPE
		      || !reference_compatible_p (t, TREE_TYPE (t2)))
		    candidates = candidates->next;
		}
	    }
	  else if (TREE_CODE (t2) == REFERENCE_TYPE
		   && reference_compatible_p (t, TREE_TYPE (t2)))
	    candidates 
	      = add_function_candidate (candidates, f, s, arglist, 
					LOOKUP_NORMAL);

	  if (candidates != old_candidates)
	    candidates->basetype_path = TYPE_BINFO (s);
	}
    }

  /* If none of the conversion functions worked out, let our caller
     know.  */
  if (!any_viable (candidates))
    return NULL_TREE;
  
  candidates = splice_viable (candidates);
  cand = tourney (candidates);
  if (!cand)
    return NULL_TREE;

  conv = build1 (IDENTITY_CONV, s, expr);
  conv = build_conv (USER_CONV,
		     non_reference (TREE_TYPE (TREE_TYPE (cand->fn))),
		     conv);
  TREE_OPERAND (conv, 1) = build_expr_ptr_wrapper (cand);
  ICS_USER_FLAG (conv) = 1;
  if (cand->viable == -1)
    ICS_BAD_FLAG (conv) = 1;
  cand->second_conv = conv;

  return conv;
}

/* A reference of the indicated TYPE is being bound directly to the
   expression represented by the implicit conversion sequence CONV.
   Return a conversion sequence for this binding.  */

static tree
direct_reference_binding (type, conv)
     tree type;
     tree conv;
{
  tree t = TREE_TYPE (type);

  /* [over.ics.rank] 
     
     When a parameter of reference type binds directly
     (_dcl.init.ref_) to an argument expression, the implicit
     conversion sequence is the identity conversion, unless the
     argument expression has a type that is a derived class of the
     parameter type, in which case the implicit conversion sequence is
     a derived-to-base Conversion.
	 
     If the parameter binds directly to the result of applying a
     conversion function to the argument expression, the implicit
     conversion sequence is a user-defined conversion sequence
     (_over.ics.user_), with the second standard conversion sequence
     either an identity conversion or, if the conversion function
     returns an entity of a type that is a derived class of the
     parameter type, a derived-to-base conversion.  */
  if (!same_type_p (TYPE_MAIN_VARIANT (t),
		    TYPE_MAIN_VARIANT (TREE_TYPE (conv))))
    {
      /* Represent the derived-to-base conversion.  */
      conv = build_conv (BASE_CONV, t, conv);
      /* We will actually be binding to the base-class subobject in
	 the derived class, so we mark this conversion appropriately.
	 That way, convert_like knows not to generate a temporary.  */
      NEED_TEMPORARY_P (conv) = 0;
    }
  return build_conv (REF_BIND, type, conv);
}

/* Returns the conversion path from type FROM to reference type TO for
   purposes of reference binding.  For lvalue binding, either pass a
   reference type to FROM or an lvalue expression to EXPR.  If the
   reference will be bound to a temporary, NEED_TEMPORARY_P is set for
   the conversion returned.  */

static tree
reference_binding (rto, rfrom, expr, flags)
     tree rto, rfrom, expr;
     int flags;
{
  tree conv = NULL_TREE;
  tree to = TREE_TYPE (rto);
  tree from = rfrom;
  int related_p;
  int compatible_p;
  cp_lvalue_kind lvalue_p = clk_none;

  if (TREE_CODE (to) == FUNCTION_TYPE && expr && type_unknown_p (expr))
    {
      expr = instantiate_type (to, expr, 0);
      if (expr == error_mark_node)
	return NULL_TREE;
      from = TREE_TYPE (expr);
    }

  if (TREE_CODE (from) == REFERENCE_TYPE)
    {
      /* Anything with reference type is an lvalue.  */
      lvalue_p = clk_ordinary;
      from = TREE_TYPE (from);
    }
  else if (expr)
    lvalue_p = real_lvalue_p (expr);

  /* Figure out whether or not the types are reference-related and
     reference compatible.  We have do do this after stripping
     references from FROM.  */
  related_p = reference_related_p (to, from);
  compatible_p = reference_compatible_p (to, from);

  if (lvalue_p && compatible_p)
    {
      /* [dcl.init.ref]

	 If the intializer expression 
	 
	 -- is an lvalue (but not an lvalue for a bit-field), and "cv1 T1"
	    is reference-compatible with "cv2 T2,"
	 
	 the reference is bound directly to the initializer exprssion
	 lvalue.  */
      conv = build1 (IDENTITY_CONV, from, expr);
      conv = direct_reference_binding (rto, conv);
      if ((lvalue_p & clk_bitfield) != 0 
	  && CP_TYPE_CONST_NON_VOLATILE_P (to))
	/* For the purposes of overload resolution, we ignore the fact
	   this expression is a bitfield. (In particular,
	   [over.ics.ref] says specifically that a function with a
	   non-const reference parameter is viable even if the
	   argument is a bitfield.)

	   However, when we actually call the function we must create
	   a temporary to which to bind the reference.  If the
	   reference is volatile, or isn't const, then we cannot make
	   a temporary, so we just issue an error when the conversion
	   actually occurs.  */
	NEED_TEMPORARY_P (conv) = 1;
      return conv;
    }
  else if (CLASS_TYPE_P (from) && !(flags & LOOKUP_NO_CONVERSION))
    {
      /* [dcl.init.ref]

	 If the initializer exprsesion

	 -- has a class type (i.e., T2 is a class type) can be
	    implicitly converted to an lvalue of type "cv3 T3," where
	    "cv1 T1" is reference-compatible with "cv3 T3".  (this
	    conversion is selected by enumerating the applicable
	    conversion functions (_over.match.ref_) and choosing the
	    best one through overload resolution.  (_over.match_). 

        the reference is bound to the lvalue result of the conversion
	in the second case.  */
      conv = convert_class_to_reference (to, from, expr);
      if (conv)
	return direct_reference_binding (rto, conv);
    }

  /* From this point on, we conceptually need temporaries, even if we
     elide them.  Only the cases above are "direct bindings".  */
  if (flags & LOOKUP_NO_TEMP_BIND)
    return NULL_TREE;

  /* [over.ics.rank]
     
     When a parameter of reference type is not bound directly to an
     argument expression, the conversion sequence is the one required
     to convert the argument expression to the underlying type of the
     reference according to _over.best.ics_.  Conceptually, this
     conversion sequence corresponds to copy-initializing a temporary
     of the underlying type with the argument expression.  Any
     difference in top-level cv-qualification is subsumed by the
     initialization itself and does not constitute a conversion.  */

  /* [dcl.init.ref]

     Otherwise, the reference shall be to a non-volatile const type.  */
  if (!CP_TYPE_CONST_NON_VOLATILE_P (to))
    return NULL_TREE;

  /* [dcl.init.ref]
     
     If the initializer expression is an rvalue, with T2 a class type,
     and "cv1 T1" is reference-compatible with "cv2 T2", the reference
     is bound in one of the following ways:
     
     -- The reference is bound to the object represented by the rvalue
        or to a sub-object within that object.  

     In this case, the implicit conversion sequence is supposed to be
     same as we would obtain by generating a temporary.  Fortunately,
     if the types are reference compatible, then this is either an
     identity conversion or the derived-to-base conversion, just as
     for direct binding.  */
  if (CLASS_TYPE_P (from) && compatible_p)
    {
      conv = build1 (IDENTITY_CONV, from, expr);
      return direct_reference_binding (rto, conv);
    }

  /* [dcl.init.ref]

     Otherwise, a temporary of type "cv1 T1" is created and
     initialized from the initializer expression using the rules for a
     non-reference copy initialization.  If T1 is reference-related to
     T2, cv1 must be the same cv-qualification as, or greater
     cv-qualification than, cv2; otherwise, the program is ill-formed.  */
  if (related_p && !at_least_as_qualified_p (to, from))
    return NULL_TREE;

  conv = implicit_conversion (to, from, expr, flags);
  if (!conv)
    return NULL_TREE;

  conv = build_conv (REF_BIND, rto, conv);
  /* This reference binding, unlike those above, requires the
     creation of a temporary.  */
  NEED_TEMPORARY_P (conv) = 1;

  return conv;
}

/* Returns the implicit conversion sequence (see [over.ics]) from type FROM
   to type TO.  The optional expression EXPR may affect the conversion.
   FLAGS are the usual overloading flags.  Only LOOKUP_NO_CONVERSION is
   significant.  */

static tree
implicit_conversion (to, from, expr, flags)
     tree to, from, expr;
     int flags;
{
  tree conv;
  struct z_candidate *cand;

  /* Resolve expressions like `A::p' that we thought might become
     pointers-to-members.  */
  if (expr && TREE_CODE (expr) == OFFSET_REF)
    {
      expr = resolve_offset_ref (expr);
      from = TREE_TYPE (expr);
    }

  if (from == error_mark_node || to == error_mark_node
      || expr == error_mark_node)
    return NULL_TREE;

  /* Make sure both the FROM and TO types are complete so that
     user-defined conversions are available.  */
  complete_type (from);
  complete_type (to);

  if (TREE_CODE (to) == REFERENCE_TYPE)
    conv = reference_binding (to, from, expr, flags);
  else
    conv = standard_conversion (to, from, expr);

  if (conv)
    ;
  else if (expr != NULL_TREE
	   && (IS_AGGR_TYPE (non_reference (from))
	       || IS_AGGR_TYPE (non_reference (to)))
	   && (flags & LOOKUP_NO_CONVERSION) == 0)
    {
      cand = build_user_type_conversion_1
	(to, expr, LOOKUP_ONLYCONVERTING);
      if (cand)
	conv = cand->second_conv;
      if ((! conv || ICS_BAD_FLAG (conv))
	  && TREE_CODE (to) == REFERENCE_TYPE
	  && (flags & LOOKUP_NO_TEMP_BIND) == 0)
	{
	  cand = build_user_type_conversion_1
	    (TYPE_MAIN_VARIANT (TREE_TYPE (to)), expr, LOOKUP_ONLYCONVERTING);
	  if (cand)
	    {
	      if (!CP_TYPE_CONST_NON_VOLATILE_P (TREE_TYPE (to)))
		ICS_BAD_FLAG (cand->second_conv) = 1;
	      if (!conv || (ICS_BAD_FLAG (conv)
			    > ICS_BAD_FLAG (cand->second_conv)))
		conv = build_conv (REF_BIND, to, cand->second_conv);
	    }
	}
    }

  return conv;
}

/* Add a new entry to the list of candidates.  Used by the add_*_candidate
   functions.  */

static struct z_candidate *
add_candidate (candidates, fn, convs, viable)
     struct z_candidate *candidates;
     tree fn, convs;
     int viable;
{
  /* FIXME: This is a memory leak.  Presumably, we should use
     ggc_alloc instead.  */
  struct z_candidate *cand
    = (struct z_candidate *) expralloc (sizeof (struct z_candidate));

  cand->fn = fn;
  cand->convs = convs;
  cand->second_conv = NULL_TREE;
  cand->viable = viable;
  cand->basetype_path = NULL_TREE;
  cand->template = NULL_TREE;
  cand->warnings = NULL_TREE;
  cand->next = candidates;

  return cand;
}

/* Create an overload candidate for the function or method FN called with
   the argument list ARGLIST and add it to CANDIDATES.  FLAGS is passed on
   to implicit_conversion.

   CTYPE, if non-NULL, is the type we want to pretend this function
   comes from for purposes of overload resolution.  */

static struct z_candidate *
add_function_candidate (candidates, fn, ctype, arglist, flags)
     struct z_candidate *candidates;
     tree fn, ctype, arglist;
     int flags;
{
  tree parmlist = TYPE_ARG_TYPES (TREE_TYPE (fn));
  int i, len;
  tree convs;
  tree parmnode, argnode;
  int viable = 1;

  /* The `this' and `in_chrg' arguments to constructors are not considered
     in overload resolution.  */
  if (DECL_CONSTRUCTOR_P (fn))
    {
      parmlist = TREE_CHAIN (parmlist);
      arglist = TREE_CHAIN (arglist);
      if (TYPE_USES_VIRTUAL_BASECLASSES (DECL_CONTEXT (fn)))
	{
	  parmlist = TREE_CHAIN (parmlist);
	  arglist = TREE_CHAIN (arglist);
	}
    }

  len = list_length (arglist);
  convs = make_tree_vec (len);

  /* 13.3.2 - Viable functions [over.match.viable]
     First, to be a viable function, a candidate function shall have enough
     parameters to agree in number with the arguments in the list.

     We need to check this first; otherwise, checking the ICSes might cause
     us to produce an ill-formed template instantiation.  */

  parmnode = parmlist;
  for (i = 0; i < len; ++i)
    {
      if (parmnode == NULL_TREE || parmnode == void_list_node)
	break;
      parmnode = TREE_CHAIN (parmnode);
    }

  if (i < len && parmnode)
    viable = 0;

  /* Make sure there are default args for the rest of the parms.  */
  else for (; parmnode && parmnode != void_list_node;
	    parmnode = TREE_CHAIN (parmnode))
    if (! TREE_PURPOSE (parmnode))
      {
	viable = 0;
	break;
      }

  if (! viable)
    goto out;

  /* Second, for F to be a viable function, there shall exist for each
     argument an implicit conversion sequence that converts that argument
     to the corresponding parameter of F.  */

  parmnode = parmlist;
  argnode = arglist;

  for (i = 0; i < len; ++i)
    {
      tree arg = TREE_VALUE (argnode);
      tree argtype = lvalue_type (arg);
      tree t;
      int is_this;

      if (parmnode == void_list_node)
	break;

      is_this = (i == 0 && DECL_NONSTATIC_MEMBER_FUNCTION_P (fn)
		 && ! DECL_CONSTRUCTOR_P (fn));

      if (parmnode)
	{
	  tree parmtype = TREE_VALUE (parmnode);

	  /* The type of the implicit object parameter ('this') for
	     overload resolution is not always the same as for the
	     function itself; conversion functions are considered to
	     be members of the class being converted, and functions
	     introduced by a using-declaration are considered to be
	     members of the class that uses them.

	     Since build_over_call ignores the ICS for the `this'
	     parameter, we can just change the parm type.  */
	  if (ctype && is_this)
	    {
	      parmtype
		= build_qualified_type (ctype,
					TYPE_QUALS (TREE_TYPE (parmtype)));
	      parmtype = build_pointer_type (parmtype);
	    }

	  t = implicit_conversion (parmtype, argtype, arg, flags);
	}
      else
	{
	  t = build1 (IDENTITY_CONV, argtype, arg);
	  ICS_ELLIPSIS_FLAG (t) = 1;
	}

      if (t && is_this)
	ICS_THIS_FLAG (t) = 1;

      TREE_VEC_ELT (convs, i) = t;
      if (! t)
	{
	  viable = 0;
	  break;
	}

      if (ICS_BAD_FLAG (t))
	viable = -1;

      if (parmnode)
	parmnode = TREE_CHAIN (parmnode);
      argnode = TREE_CHAIN (argnode);
    }

 out:
  return add_candidate (candidates, fn, convs, viable);
}

/* Create an overload candidate for the conversion function FN which will
   be invoked for expression OBJ, producing a pointer-to-function which
   will in turn be called with the argument list ARGLIST, and add it to
   CANDIDATES.  FLAGS is passed on to implicit_conversion.

   Actually, we don't really care about FN; we care about the type it
   converts to.  There may be multiple conversion functions that will
   convert to that type, and we rely on build_user_type_conversion_1 to
   choose the best one; so when we create our candidate, we record the type
   instead of the function.  */

static struct z_candidate *
add_conv_candidate (candidates, fn, obj, arglist)
     struct z_candidate *candidates;
     tree fn, obj, arglist;
{
  tree totype = TREE_TYPE (TREE_TYPE (fn));
  tree parmlist = TYPE_ARG_TYPES (TREE_TYPE (totype));
  int i, len = list_length (arglist) + 1;
  tree convs = make_tree_vec (len);
  tree parmnode = parmlist;
  tree argnode = arglist;
  int viable = 1;
  int flags = LOOKUP_NORMAL;

  /* Don't bother looking up the same type twice.  */
  if (candidates && candidates->fn == totype)
    return candidates;

  for (i = 0; i < len; ++i)
    {
      tree arg = i == 0 ? obj : TREE_VALUE (argnode);
      tree argtype = lvalue_type (arg);
      tree t;

      if (i == 0)
	t = implicit_conversion (totype, argtype, arg, flags);
      else if (parmnode == void_list_node)
	break;
      else if (parmnode)
	t = implicit_conversion (TREE_VALUE (parmnode), argtype, arg, flags);
      else
	{
	  t = build1 (IDENTITY_CONV, argtype, arg);
	  ICS_ELLIPSIS_FLAG (t) = 1;
	}

      TREE_VEC_ELT (convs, i) = t;
      if (! t)
	break;

      if (ICS_BAD_FLAG (t))
	viable = -1;

      if (i == 0)
	continue;

      if (parmnode)
	parmnode = TREE_CHAIN (parmnode);
      argnode = TREE_CHAIN (argnode);
    }

  if (i < len)
    viable = 0;

  for (; parmnode && parmnode != void_list_node;
       parmnode = TREE_CHAIN (parmnode))
    if (! TREE_PURPOSE (parmnode))
      {
	viable = 0;
	break;
      }

  return add_candidate (candidates, totype, convs, viable);
}

static struct z_candidate *
build_builtin_candidate (candidates, fnname, type1, type2,
			 args, argtypes, flags)
     struct z_candidate *candidates;
     tree fnname, type1, type2, *args, *argtypes;
     int flags;

{
  tree t, convs;
  int viable = 1, i;
  tree types[2];

  types[0] = type1;
  types[1] = type2;

  convs = make_tree_vec (args[2] ? 3 : (args[1] ? 2 : 1));

  for (i = 0; i < 2; ++i)
    {
      if (! args[i])
	break;

      t = implicit_conversion (types[i], argtypes[i], args[i], flags);
      if (! t)
	{
	  viable = 0;
	  /* We need something for printing the candidate.  */
	  t = build1 (IDENTITY_CONV, types[i], NULL_TREE);
	}
      else if (ICS_BAD_FLAG (t))
	viable = 0;
      TREE_VEC_ELT (convs, i) = t;
    }

  /* For COND_EXPR we rearranged the arguments; undo that now.  */
  if (args[2])
    {
      TREE_VEC_ELT (convs, 2) = TREE_VEC_ELT (convs, 1);
      TREE_VEC_ELT (convs, 1) = TREE_VEC_ELT (convs, 0);
      t = implicit_conversion (boolean_type_node, argtypes[2], args[2], flags);
      if (t)
	TREE_VEC_ELT (convs, 0) = t;
      else
	viable = 0;
    }      

  return add_candidate (candidates, fnname, convs, viable);
}

static int
is_complete (t)
     tree t;
{
  return TYPE_SIZE (complete_type (t)) != NULL_TREE;
}

/* Returns non-zero if TYPE is a promoted arithmetic type.  */

static int
promoted_arithmetic_type_p (type)
     tree type;
{
  /* [over.built]

     In this section, the term promoted integral type is used to refer
     to those integral types which are preserved by integral promotion
     (including e.g.  int and long but excluding e.g.  char).
     Similarly, the term promoted arithmetic type refers to promoted
     integral types plus floating types.  */
  return ((INTEGRAL_TYPE_P (type)
	   && same_type_p (type_promotes_to (type), type))
	  || TREE_CODE (type) == REAL_TYPE);
}

/* Create any builtin operator overload candidates for the operator in
   question given the converted operand types TYPE1 and TYPE2.  The other
   args are passed through from add_builtin_candidates to
   build_builtin_candidate.  */

static struct z_candidate *
add_builtin_candidate (candidates, code, code2, fnname, type1, type2,
		       args, argtypes, flags)
     struct z_candidate *candidates;
     enum tree_code code, code2;
     tree fnname, type1, type2, *args, *argtypes;
     int flags;
{
  switch (code)
    {
    case POSTINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      args[1] = integer_zero_node;
      type2 = integer_type_node;
      break;
    default:
      break;
    }

  switch (code)
    {

/* 4 For every pair T, VQ), where T is an arithmetic or  enumeration  type,
     and  VQ  is  either  volatile or empty, there exist candidate operator
     functions of the form
	     VQ T&   operator++(VQ T&);
	     T       operator++(VQ T&, int);
   5 For every pair T, VQ), where T is an enumeration type or an arithmetic
     type  other than bool, and VQ is either volatile or empty, there exist
     candidate operator functions of the form
	     VQ T&   operator--(VQ T&);
	     T       operator--(VQ T&, int);
   6 For every pair T, VQ), where T is  a  cv-qualified  or  cv-unqualified
     complete  object type, and VQ is either volatile or empty, there exist
     candidate operator functions of the form
	     T*VQ&   operator++(T*VQ&);
	     T*VQ&   operator--(T*VQ&);
	     T*      operator++(T*VQ&, int);
	     T*      operator--(T*VQ&, int);  */

    case POSTDECREMENT_EXPR:
    case PREDECREMENT_EXPR:
      if (TREE_CODE (type1) == BOOLEAN_TYPE)
	return candidates;
    case POSTINCREMENT_EXPR:
    case PREINCREMENT_EXPR:
      if ((ARITHMETIC_TYPE_P (type1) && TREE_CODE (type1) != ENUMERAL_TYPE)
	  || TYPE_PTROB_P (type1))
	{
	  type1 = build_reference_type (type1);
	  break;
	}
      return candidates;

/* 7 For every cv-qualified or cv-unqualified complete object type T, there
     exist candidate operator functions of the form

	     T&      operator*(T*);

   8 For every function type T, there exist candidate operator functions of
     the form
	     T&      operator*(T*);  */

    case INDIRECT_REF:
      if (TREE_CODE (type1) == POINTER_TYPE
	  && (TYPE_PTROB_P (type1)
	      || TREE_CODE (TREE_TYPE (type1)) == FUNCTION_TYPE))
	break;
      return candidates;

/* 9 For every type T, there exist candidate operator functions of the form
	     T*      operator+(T*);

   10For  every  promoted arithmetic type T, there exist candidate operator
     functions of the form
	     T       operator+(T);
	     T       operator-(T);  */

    case CONVERT_EXPR: /* unary + */
      if (TREE_CODE (type1) == POINTER_TYPE
	  && TREE_CODE (TREE_TYPE (type1)) != OFFSET_TYPE)
	break;
    case NEGATE_EXPR:
      if (ARITHMETIC_TYPE_P (type1))
	break;
      return candidates;

/* 11For every promoted integral type T,  there  exist  candidate  operator
     functions of the form
	     T       operator~(T);  */

    case BIT_NOT_EXPR:
      if (INTEGRAL_TYPE_P (type1))
	break;
      return candidates;

/* 12For every quintuple C1, C2, T, CV1, CV2), where C2 is a class type, C1
     is the same type as C2 or is a derived class of C2, T  is  a  complete
     object type or a function type, and CV1 and CV2 are cv-qualifier-seqs,
     there exist candidate operator functions of the form
	     CV12 T& operator->*(CV1 C1*, CV2 T C2::*);
     where CV12 is the union of CV1 and CV2.  */

    case MEMBER_REF:
      if (TREE_CODE (type1) == POINTER_TYPE
	  && (TYPE_PTRMEMFUNC_P (type2) || TYPE_PTRMEM_P (type2)))
	{
	  tree c1 = TREE_TYPE (type1);
	  tree c2 = (TYPE_PTRMEMFUNC_P (type2)
		     ? TYPE_METHOD_BASETYPE (TREE_TYPE (TYPE_PTRMEMFUNC_FN_TYPE (type2)))
		     : TYPE_OFFSET_BASETYPE (TREE_TYPE (type2)));

	  if (IS_AGGR_TYPE (c1) && DERIVED_FROM_P (c2, c1)
	      && (TYPE_PTRMEMFUNC_P (type2)
		  || is_complete (TREE_TYPE (TREE_TYPE (type2)))))
	    break;
	}
      return candidates;

/* 13For every pair of promoted arithmetic types L and R, there exist  can-
     didate operator functions of the form
	     LR      operator*(L, R);
	     LR      operator/(L, R);
	     LR      operator+(L, R);
	     LR      operator-(L, R);
	     bool    operator<(L, R);
	     bool    operator>(L, R);
	     bool    operator<=(L, R);
	     bool    operator>=(L, R);
	     bool    operator==(L, R);
	     bool    operator!=(L, R);
     where  LR  is  the  result of the usual arithmetic conversions between
     types L and R.

   14For every pair of types T and I, where T  is  a  cv-qualified  or  cv-
     unqualified  complete  object  type and I is a promoted integral type,
     there exist candidate operator functions of the form
	     T*      operator+(T*, I);
	     T&      operator[](T*, I);
	     T*      operator-(T*, I);
	     T*      operator+(I, T*);
	     T&      operator[](I, T*);

   15For every T, where T is a pointer to complete object type, there exist
     candidate operator functions of the form112)
	     ptrdiff_t operator-(T, T);

   16For  every pointer type T, there exist candidate operator functions of
     the form
	     bool    operator<(T, T);
	     bool    operator>(T, T);
	     bool    operator<=(T, T);
	     bool    operator>=(T, T);
	     bool    operator==(T, T);
	     bool    operator!=(T, T);

   17For every pointer to member type T,  there  exist  candidate  operator
     functions of the form
	     bool    operator==(T, T);
	     bool    operator!=(T, T);  */

    case MINUS_EXPR:
      if (TYPE_PTROB_P (type1) && TYPE_PTROB_P (type2))
	break;
      if (TYPE_PTROB_P (type1) && INTEGRAL_TYPE_P (type2))
	{
	  type2 = ptrdiff_type_node;
	  break;
	}
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
      if (ARITHMETIC_TYPE_P (type1) && ARITHMETIC_TYPE_P (type2))
	break;
      return candidates;

    case EQ_EXPR:
    case NE_EXPR:
      if ((TYPE_PTRMEMFUNC_P (type1) && TYPE_PTRMEMFUNC_P (type2))
	  || (TYPE_PTRMEM_P (type1) && TYPE_PTRMEM_P (type2)))
	break;
      if ((TYPE_PTRMEMFUNC_P (type1) || TYPE_PTRMEM_P (type1))
	  && null_ptr_cst_p (args[1]))
	{
	  type2 = type1;
	  break;
	}
      if ((TYPE_PTRMEMFUNC_P (type2) || TYPE_PTRMEM_P (type2))
	  && null_ptr_cst_p (args[0]))
	{
	  type1 = type2;
	  break;
	}
    case LT_EXPR:
    case GT_EXPR:
    case LE_EXPR:
    case GE_EXPR:
    case MAX_EXPR:
    case MIN_EXPR:
      if ((ARITHMETIC_TYPE_P (type1) && ARITHMETIC_TYPE_P (type2))
	  || (TYPE_PTR_P (type1) && TYPE_PTR_P (type2)))
	break;
      if (TYPE_PTR_P (type1) && null_ptr_cst_p (args[1]))
	{
	  type2 = type1;
	  break;
	}
      if (null_ptr_cst_p (args[0]) && TYPE_PTR_P (type2))
	{
	  type1 = type2;
	  break;
	}
      return candidates;

    case PLUS_EXPR:
      if (ARITHMETIC_TYPE_P (type1) && ARITHMETIC_TYPE_P (type2))
	break;
    case ARRAY_REF:
      if (INTEGRAL_TYPE_P (type1) && TYPE_PTROB_P (type2))
	{
	  type1 = ptrdiff_type_node;
	  break;
	}
      if (TYPE_PTROB_P (type1) && INTEGRAL_TYPE_P (type2))
	{
	  type2 = ptrdiff_type_node;
	  break;
	}
      return candidates;

/* 18For  every pair of promoted integral types L and R, there exist candi-
     date operator functions of the form
	     LR      operator%(L, R);
	     LR      operator&(L, R);
	     LR      operator^(L, R);
	     LR      operator|(L, R);
	     L       operator<<(L, R);
	     L       operator>>(L, R);
     where LR is the result of the  usual  arithmetic  conversions  between
     types L and R.  */

    case TRUNC_MOD_EXPR:
    case BIT_AND_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
      if (INTEGRAL_TYPE_P (type1) && INTEGRAL_TYPE_P (type2))
	break;
      return candidates;

/* 19For  every  triple  L, VQ, R), where L is an arithmetic or enumeration
     type, VQ is either volatile or empty, and R is a  promoted  arithmetic
     type, there exist candidate operator functions of the form
	     VQ L&   operator=(VQ L&, R);
	     VQ L&   operator*=(VQ L&, R);
	     VQ L&   operator/=(VQ L&, R);
	     VQ L&   operator+=(VQ L&, R);
	     VQ L&   operator-=(VQ L&, R);

   20For  every  pair T, VQ), where T is any type and VQ is either volatile
     or empty, there exist candidate operator functions of the form
	     T*VQ&   operator=(T*VQ&, T*);

   21For every pair T, VQ), where T is a pointer to member type and  VQ  is
     either  volatile or empty, there exist candidate operator functions of
     the form
	     VQ T&   operator=(VQ T&, T);

   22For every triple  T,  VQ,  I),  where  T  is  a  cv-qualified  or  cv-
     unqualified  complete object type, VQ is either volatile or empty, and
     I is a promoted integral type, there exist  candidate  operator  func-
     tions of the form
	     T*VQ&   operator+=(T*VQ&, I);
	     T*VQ&   operator-=(T*VQ&, I);

   23For  every  triple  L,  VQ,  R), where L is an integral or enumeration
     type, VQ is either volatile or empty, and R  is  a  promoted  integral
     type, there exist candidate operator functions of the form

	     VQ L&   operator%=(VQ L&, R);
	     VQ L&   operator<<=(VQ L&, R);
	     VQ L&   operator>>=(VQ L&, R);
	     VQ L&   operator&=(VQ L&, R);
	     VQ L&   operator^=(VQ L&, R);
	     VQ L&   operator|=(VQ L&, R);  */

    case MODIFY_EXPR:
      switch (code2)
	{
	case PLUS_EXPR:
	case MINUS_EXPR:
	  if (TYPE_PTROB_P (type1) && INTEGRAL_TYPE_P (type2))
	    {
	      type2 = ptrdiff_type_node;
	      break;
	    }
	case MULT_EXPR:
	case TRUNC_DIV_EXPR:
	  if (ARITHMETIC_TYPE_P (type1) && ARITHMETIC_TYPE_P (type2))
	    break;
	  return candidates;

	case TRUNC_MOD_EXPR:
	case BIT_AND_EXPR:
	case BIT_IOR_EXPR:
	case BIT_XOR_EXPR:
	case LSHIFT_EXPR:
	case RSHIFT_EXPR:
	  if (INTEGRAL_TYPE_P (type1) && INTEGRAL_TYPE_P (type2))
	    break;
	  return candidates;

	case NOP_EXPR:
	  if (ARITHMETIC_TYPE_P (type1) && ARITHMETIC_TYPE_P (type2))
	    break;
	  if ((TYPE_PTRMEMFUNC_P (type1) && TYPE_PTRMEMFUNC_P (type2))
	      || (TYPE_PTR_P (type1) && TYPE_PTR_P (type2))
	      || (TYPE_PTRMEM_P (type1) && TYPE_PTRMEM_P (type2))
	      || ((TYPE_PTRMEMFUNC_P (type1)
		   || TREE_CODE (type1) == POINTER_TYPE)
		  && null_ptr_cst_p (args[1])))
	    {
	      type2 = type1;
	      break;
	    }
	  return candidates;

	default:
	  my_friendly_abort (367);
	}
      type1 = build_reference_type (type1);
      break;

    case COND_EXPR:
      /* [over.builtin]

	 For every pair of promoted arithmetic types L and R, there
	 exist candidate operator functions of the form 

	 LR operator?(bool, L, R); 

	 where LR is the result of the usual arithmetic conversions
	 between types L and R.

	 For every type T, where T is a pointer or pointer-to-member
	 type, there exist candidate operator functions of the form T
	 operator?(bool, T, T);  */

      if (promoted_arithmetic_type_p (type1)
	  && promoted_arithmetic_type_p (type2))
	/* That's OK.  */
	break;

      /* Otherwise, the types should be pointers.  */
      if (!(TREE_CODE (type1) == POINTER_TYPE
	    || TYPE_PTRMEM_P (type1)
	    || TYPE_PTRMEMFUNC_P (type1))
	  || !(TREE_CODE (type2) == POINTER_TYPE
	       || TYPE_PTRMEM_P (type2)
	       || TYPE_PTRMEMFUNC_P (type2)))
	return candidates;
      
      /* We don't check that the two types are the same; the logic
	 below will actually create two candidates; one in which both
	 parameter types are TYPE1, and one in which both parameter
	 types are TYPE2.  */
	break;

      /* These arguments do not make for a legal overloaded operator.  */
      return candidates;

    default:
      my_friendly_abort (367);
    }

  /* If we're dealing with two pointer types, we need candidates
     for both of them.  */
  if (type2 && !same_type_p (type1, type2)
      && TREE_CODE (type1) == TREE_CODE (type2)
      && (TREE_CODE (type1) == REFERENCE_TYPE
	  || (TREE_CODE (type1) == POINTER_TYPE
	      && TYPE_PTRMEM_P (type1) == TYPE_PTRMEM_P (type2))
	  || TYPE_PTRMEMFUNC_P (type1)
	  || IS_AGGR_TYPE (type1)))
    {
      candidates = build_builtin_candidate
	(candidates, fnname, type1, type1, args, argtypes, flags);
      return build_builtin_candidate
	(candidates, fnname, type2, type2, args, argtypes, flags);
    }

  return build_builtin_candidate
    (candidates, fnname, type1, type2, args, argtypes, flags);
}

tree
type_decays_to (type)
     tree type;
{
  if (TREE_CODE (type) == ARRAY_TYPE)
    return build_pointer_type (TREE_TYPE (type));
  if (TREE_CODE (type) == FUNCTION_TYPE)
    return build_pointer_type (type);
  return type;
}

/* There are three conditions of builtin candidates:

   1) bool-taking candidates.  These are the same regardless of the input.
   2) pointer-pair taking candidates.  These are generated for each type
      one of the input types converts to.
   3) arithmetic candidates.  According to the standard, we should generate
      all of these, but I'm trying not to... */

static struct z_candidate *
add_builtin_candidates (candidates, code, code2, fnname, args, flags)
     struct z_candidate *candidates;
     enum tree_code code, code2;
     tree fnname, *args;
     int flags;
{
  int ref1, i;
  tree type, argtypes[3];
  /* TYPES[i] is the set of possible builtin-operator parameter types
     we will consider for the Ith argument.  These are represented as
     a TREE_LIST; the TREE_VALUE of each node is the potential
     parameter type.  */
  tree types[2];

  for (i = 0; i < 3; ++i)
    {
      if (args[i])
	argtypes[i]  = lvalue_type (args[i]);
      else
	argtypes[i] = NULL_TREE;
    }

  switch (code)
    {
/* 4 For every pair T, VQ), where T is an arithmetic or  enumeration  type,
     and  VQ  is  either  volatile or empty, there exist candidate operator
     functions of the form
		 VQ T&   operator++(VQ T&);  */

    case POSTINCREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case MODIFY_EXPR:
      ref1 = 1;
      break;

/* 24There also exist candidate operator functions of the form
	     bool    operator!(bool);
	     bool    operator&&(bool, bool);
	     bool    operator||(bool, bool);  */

    case TRUTH_NOT_EXPR:
      return build_builtin_candidate
	(candidates, fnname, boolean_type_node,
	 NULL_TREE, args, argtypes, flags);

    case TRUTH_ORIF_EXPR:
    case TRUTH_ANDIF_EXPR:
      return build_builtin_candidate
	(candidates, fnname, boolean_type_node,
	 boolean_type_node, args, argtypes, flags);

    case ADDR_EXPR:
    case COMPOUND_EXPR:
    case COMPONENT_REF:
      return candidates;

    default:
      ref1 = 0;
    }

  types[0] = types[1] = NULL_TREE;

  for (i = 0; i < 2; ++i)
    {
      if (! args[i])
	;
      else if (IS_AGGR_TYPE (argtypes[i]))
	{
	  tree convs;

	  if (i == 0 && code == MODIFY_EXPR && code2 == NOP_EXPR)
	    return candidates;

	  convs = lookup_conversions (argtypes[i]);

	  if (code == COND_EXPR)
	    {
	      if (real_lvalue_p (args[i]))
		types[i] = tree_cons
		  (NULL_TREE, build_reference_type (argtypes[i]), types[i]);

	      types[i] = tree_cons
		(NULL_TREE, TYPE_MAIN_VARIANT (argtypes[i]), types[i]);
	    }

	  else if (! convs)
	    return candidates;

	  for (; convs; convs = TREE_CHAIN (convs))
	    {
	      type = TREE_TYPE (TREE_TYPE (OVL_CURRENT (TREE_VALUE (convs))));

	      if (i == 0 && ref1
		  && (TREE_CODE (type) != REFERENCE_TYPE
		      || CP_TYPE_CONST_P (TREE_TYPE (type))))
		continue;

	      if (code == COND_EXPR && TREE_CODE (type) == REFERENCE_TYPE)
		types[i] = tree_cons (NULL_TREE, type, types[i]);

	      type = non_reference (type);
	      if (i != 0 || ! ref1)
		{
		  type = TYPE_MAIN_VARIANT (type_decays_to (type));
		  if (code == COND_EXPR && TREE_CODE (type) == ENUMERAL_TYPE)
		    types[i] = tree_cons (NULL_TREE, type, types[i]);
		  if (INTEGRAL_TYPE_P (type))
		    type = type_promotes_to (type);
		}

	      if (! value_member (type, types[i]))
		types[i] = tree_cons (NULL_TREE, type, types[i]);
	    }
	}
      else
	{
	  if (code == COND_EXPR && real_lvalue_p (args[i]))
	    types[i] = tree_cons
	      (NULL_TREE, build_reference_type (argtypes[i]), types[i]);
	  type = non_reference (argtypes[i]);
	  if (i != 0 || ! ref1)
	    {
	      type = TYPE_MAIN_VARIANT (type_decays_to (type));
	      if (code == COND_EXPR && TREE_CODE (type) == ENUMERAL_TYPE)
		types[i] = tree_cons (NULL_TREE, type, types[i]);
	      if (INTEGRAL_TYPE_P (type))
		type = type_promotes_to (type);
	    }
	  types[i] = tree_cons (NULL_TREE, type, types[i]);
	}
    }

  /* Run through the possible parameter types of both arguments,
     creating candidates with those parameter types.  */
  for (; types[0]; types[0] = TREE_CHAIN (types[0]))
    {
      if (types[1])
	for (type = types[1]; type; type = TREE_CHAIN (type))
	  candidates = add_builtin_candidate
	    (candidates, code, code2, fnname, TREE_VALUE (types[0]),
	     TREE_VALUE (type), args, argtypes, flags);
      else
	candidates = add_builtin_candidate
	  (candidates, code, code2, fnname, TREE_VALUE (types[0]),
	   NULL_TREE, args, argtypes, flags);
    }

  return candidates;
}


/* If TMPL can be successfully instantiated as indicated by
   EXPLICIT_TARGS and ARGLIST, adds the instantiation to CANDIDATES.

   TMPL is the template.  EXPLICIT_TARGS are any explicit template
   arguments.  ARGLIST is the arguments provided at the call-site.
   The RETURN_TYPE is the desired type for conversion operators.  If
   OBJ is NULL_TREE, FLAGS and CTYPE are as for add_function_candidate.
   If an OBJ is supplied, FLAGS and CTYPE are ignored, and OBJ is as for
   add_conv_candidate.  */

static struct z_candidate*
add_template_candidate_real (candidates, tmpl, ctype, explicit_targs,
			     arglist, return_type, flags,
			     obj, strict)
     struct z_candidate *candidates;
     tree tmpl, ctype, explicit_targs, arglist, return_type;
     int flags;
     tree obj;
     unification_kind_t strict;
{
  int ntparms = DECL_NTPARMS (tmpl);
  tree targs = make_tree_vec (ntparms);
  struct z_candidate *cand;
  int i;
  tree fn;

  i = fn_type_unification (tmpl, explicit_targs, targs, arglist,
			   return_type, strict);

  if (i != 0)
    return candidates;

  fn = instantiate_template (tmpl, targs);
  if (fn == error_mark_node)
    return candidates;

  if (obj != NULL_TREE)
    /* Aha, this is a conversion function.  */
    cand = add_conv_candidate (candidates, fn, obj, arglist);
  else
    cand = add_function_candidate (candidates, fn, ctype,
				   arglist, flags);
  if (DECL_TI_TEMPLATE (fn) != tmpl)
    /* This situation can occur if a member template of a template
       class is specialized.  Then, instantiate_template might return
       an instantiation of the specialization, in which case the
       DECL_TI_TEMPLATE field will point at the original
       specialization.  For example:

	 template <class T> struct S { template <class U> void f(U);
				       template <> void f(int) {}; };
	 S<double> sd;
	 sd.f(3);

       Here, TMPL will be template <class U> S<double>::f(U).
       And, instantiate template will give us the specialization
       template <> S<double>::f(int).  But, the DECL_TI_TEMPLATE field
       for this will point at template <class T> template <> S<T>::f(int),
       so that we can find the definition.  For the purposes of
       overload resolution, however, we want the original TMPL.  */
    cand->template = tree_cons (tmpl, targs, NULL_TREE);
  else
    cand->template = DECL_TEMPLATE_INFO (fn);

  return cand;
}


static struct z_candidate *
add_template_candidate (candidates, tmpl, ctype, explicit_targs, 
			arglist, return_type, flags, strict)
     struct z_candidate *candidates;
     tree tmpl, ctype, explicit_targs, arglist, return_type;
     int flags;
     unification_kind_t strict;
{
  return 
    add_template_candidate_real (candidates, tmpl, ctype,
				 explicit_targs, arglist, return_type, flags,
				 NULL_TREE, strict);
}


static struct z_candidate *
add_template_conv_candidate (candidates, tmpl, obj, arglist, return_type)
     struct z_candidate *candidates;
     tree tmpl, obj, arglist, return_type;
{
  return 
    add_template_candidate_real (candidates, tmpl, NULL_TREE, NULL_TREE,
				 arglist, return_type, 0, obj, DEDUCE_CONV);
}


static int
any_viable (cands)
     struct z_candidate *cands;
{
  for (; cands; cands = cands->next)
    if (pedantic ? cands->viable == 1 : cands->viable)
      return 1;
  return 0;
}

static struct z_candidate *
splice_viable (cands)
     struct z_candidate *cands;
{
  struct z_candidate **p = &cands;

  for (; *p; )
    {
      if (pedantic ? (*p)->viable == 1 : (*p)->viable)
	p = &((*p)->next);
      else
	*p = (*p)->next;
    }

  return cands;
}

static tree
build_this (obj)
     tree obj;
{
  /* Fix this to work on non-lvalues.  */
  return build_unary_op (ADDR_EXPR, obj, 0);
}

static void
print_z_candidates (candidates)
     struct z_candidate *candidates;
{
  const char *str = "candidates are:";
  for (; candidates; candidates = candidates->next)
    {
      if (TREE_CODE (candidates->fn) == IDENTIFIER_NODE)
	{
	  if (candidates->fn == ansi_opname [COND_EXPR])
	    cp_error ("%s %D(%T, %T, %T) <builtin>", str, candidates->fn,
		      TREE_TYPE (TREE_VEC_ELT (candidates->convs, 0)),
		      TREE_TYPE (TREE_VEC_ELT (candidates->convs, 1)),
		      TREE_TYPE (TREE_VEC_ELT (candidates->convs, 2)));
	  else if (TREE_VEC_LENGTH (candidates->convs) == 2)
	    cp_error ("%s %D(%T, %T) <builtin>", str, candidates->fn,
		      TREE_TYPE (TREE_VEC_ELT (candidates->convs, 0)),
		      TREE_TYPE (TREE_VEC_ELT (candidates->convs, 1)));
	  else
	    cp_error ("%s %D(%T) <builtin>", str, candidates->fn,
		      TREE_TYPE (TREE_VEC_ELT (candidates->convs, 0)));
	}
      else if (TYPE_P (candidates->fn))
	cp_error ("%s %T <conversion>", str, candidates->fn);
      else
	cp_error_at ("%s %+#D%s", str, candidates->fn,
		     candidates->viable == -1 ? " <near match>" : "");
      str = "               "; 
    }
}

/* Returns the best overload candidate to perform the requested
   conversion.  This function is used for three the overloading situations
   described in [over.match.copy], [over.match.conv], and [over.match.ref].
   If TOTYPE is a REFERENCE_TYPE, we're trying to find an lvalue binding as
   per [dcl.init.ref], so we ignore temporary bindings.  */

static struct z_candidate *
build_user_type_conversion_1 (totype, expr, flags)
     tree totype, expr;
     int flags;
{
  struct z_candidate *candidates, *cand;
  tree fromtype = TREE_TYPE (expr);
  tree ctors = NULL_TREE, convs = NULL_TREE, *p;
  tree args = NULL_TREE;
  tree templates = NULL_TREE;

  if (IS_AGGR_TYPE (totype))
    ctors = lookup_fnfields (TYPE_BINFO (totype), ctor_identifier, 0);
  if (IS_AGGR_TYPE (fromtype)
      && (! IS_AGGR_TYPE (totype) || ! DERIVED_FROM_P (totype, fromtype)))
    convs = lookup_conversions (fromtype);

  candidates = 0;
  flags |= LOOKUP_NO_CONVERSION;

  if (ctors)
    {
      tree t = build_int_2 (0, 0);
      TREE_TYPE (t) = build_pointer_type (totype);
      args = build_tree_list (NULL_TREE, expr);
      if (TYPE_USES_VIRTUAL_BASECLASSES (totype))
	args = tree_cons (NULL_TREE, integer_one_node, args);
      args = tree_cons (NULL_TREE, t, args);

      ctors = TREE_VALUE (ctors);
    }
  for (; ctors; ctors = OVL_NEXT (ctors))
    {
      tree ctor = OVL_CURRENT (ctors);
      if (DECL_NONCONVERTING_P (ctor))
	continue;

      if (TREE_CODE (ctor) == TEMPLATE_DECL) 
	{
	  templates = tree_cons (NULL_TREE, ctor, templates);
	  candidates = 
	    add_template_candidate (candidates, ctor, totype,
				    NULL_TREE, args, NULL_TREE, flags,
				    DEDUCE_CALL);
	} 
      else 
	candidates = add_function_candidate (candidates, ctor, totype,
					     args, flags); 

      if (candidates) 
	{
	  candidates->second_conv = build1 (IDENTITY_CONV, totype, NULL_TREE);
	  candidates->basetype_path = TYPE_BINFO (totype);
	} 
    }

  if (convs)
    args = build_tree_list (NULL_TREE, build_this (expr));

  for (; convs; convs = TREE_CHAIN (convs))
    {
      tree fns = TREE_VALUE (convs);
      int convflags = LOOKUP_NO_CONVERSION;
      tree ics;

      /* If we are called to convert to a reference type, we are trying to
	 find an lvalue binding, so don't even consider temporaries.  If
	 we don't find an lvalue binding, the caller will try again to
	 look for a temporary binding.  */
      if (TREE_CODE (totype) == REFERENCE_TYPE)
	convflags |= LOOKUP_NO_TEMP_BIND;

      if (TREE_CODE (OVL_CURRENT (fns)) != TEMPLATE_DECL)
	ics = implicit_conversion
	  (totype, TREE_TYPE (TREE_TYPE (OVL_CURRENT (fns))), 0, convflags);
      else
	/* We can't compute this yet.  */
	ics = error_mark_node;

      if (TREE_CODE (totype) == REFERENCE_TYPE && ics && ICS_BAD_FLAG (ics))
	/* ignore the near match.  */;
      else if (ics)
	for (; fns; fns = OVL_NEXT (fns))
	  {
	    tree fn = OVL_CURRENT (fns);
	    struct z_candidate *old_candidates = candidates;

	    /* [over.match.funcs] For conversion functions, the function is
	       considered to be a member of the class of the implicit object
	       argument for the purpose of defining the type of the implicit
	       object parameter.

	       So we pass fromtype as CTYPE to add_*_candidate.  */

	    if (TREE_CODE (fn) == TEMPLATE_DECL)
	      {
		templates = tree_cons (NULL_TREE, fn, templates);
		candidates = 
		  add_template_candidate (candidates, fn, fromtype, NULL_TREE,
					  args, totype, flags,
					  DEDUCE_CONV);
	      } 
	    else 
	      candidates = add_function_candidate (candidates, fn, fromtype,
						   args, flags); 

	    if (candidates != old_candidates)
	      {
		if (TREE_CODE (fn) == TEMPLATE_DECL)
		  ics = implicit_conversion
		    (totype, TREE_TYPE (TREE_TYPE (candidates->fn)),
		     0, convflags);

		candidates->second_conv = ics;
		candidates->basetype_path = TYPE_BINFO (fromtype);

		if (ics == NULL_TREE)
		  candidates->viable = 0;
		else if (candidates->viable == 1 && ICS_BAD_FLAG (ics))
		  candidates->viable = -1;
	      }
	  }
    }

  if (! any_viable (candidates))
    {
#if 0
      if (flags & LOOKUP_COMPLAIN)
	{
	  if (candidates && ! candidates->next)
	    /* say why this one won't work or try to be loose */;
	  else
	    cp_error ("no viable candidates");
	}
#endif

      return 0;
    }

  candidates = splice_viable (candidates);
  cand = tourney (candidates);

  if (cand == 0)
    {
      if (flags & LOOKUP_COMPLAIN)
	{
	  cp_error ("conversion from `%T' to `%T' is ambiguous",
		    fromtype, totype);
	  print_z_candidates (candidates);
	}

      cand = candidates;	/* any one will do */
      cand->second_conv = build1 (AMBIG_CONV, totype, expr);
      ICS_USER_FLAG (cand->second_conv) = 1;
      ICS_BAD_FLAG (cand->second_conv) = 1;

      return cand;
    }

  for (p = &(cand->second_conv); TREE_CODE (*p) != IDENTITY_CONV; )
    p = &(TREE_OPERAND (*p, 0));

  /* Pedantically, normal function declarations are never considered
     to refer to template instantiations, so we only do this with
     -fguiding-decls.  */ 
  if (flag_guiding_decls && templates && ! cand->template 
      && !DECL_INITIAL (cand->fn) 
      && TREE_CODE (TREE_TYPE (cand->fn)) != METHOD_TYPE)
    add_maybe_template (cand->fn, templates);

  *p = build
    (USER_CONV,
     (DECL_CONSTRUCTOR_P (cand->fn)
      ? totype : non_reference (TREE_TYPE (TREE_TYPE (cand->fn)))),
     expr, build_expr_ptr_wrapper (cand));
  ICS_USER_FLAG (cand->second_conv) = 1;
  if (cand->viable == -1)
    ICS_BAD_FLAG (cand->second_conv) = 1;

  return cand;
}

tree
build_user_type_conversion (totype, expr, flags)
     tree totype, expr;
     int flags;
{
  struct z_candidate *cand
    = build_user_type_conversion_1 (totype, expr, flags);

  if (cand)
    {
      if (TREE_CODE (cand->second_conv) == AMBIG_CONV)
	return error_mark_node;
      return convert_from_reference (convert_like (cand->second_conv, expr));
    }
  return NULL_TREE;
}

/* Do any initial processing on the arguments to a function call.  */

static tree
resolve_args (args)
     tree args;
{
  tree t;
  for (t = args; t; t = TREE_CHAIN (t))
    {
      if (TREE_VALUE (t) == error_mark_node)
	return error_mark_node;
      else if (TREE_CODE (TREE_TYPE (TREE_VALUE (t))) == VOID_TYPE)
	{
	  error ("invalid use of void expression");
	  return error_mark_node;
	}
      else if (TREE_CODE (TREE_VALUE (t)) == OFFSET_REF)
	TREE_VALUE (t) = resolve_offset_ref (TREE_VALUE (t));
    }
  return args;
}
      
tree
build_new_function_call (fn, args)
     tree fn, args;
{
  struct z_candidate *candidates = 0, *cand;
  tree explicit_targs = NULL_TREE;
  int template_only = 0;

  if (TREE_CODE (fn) == TEMPLATE_ID_EXPR)
    {
      explicit_targs = TREE_OPERAND (fn, 1);
      fn = TREE_OPERAND (fn, 0);
      template_only = 1;
    }

  if (really_overloaded_fn (fn))
    {
      tree t1;
      tree templates = NULL_TREE;

      args = resolve_args (args);

      if (args == error_mark_node)
	return error_mark_node;

      for (t1 = fn; t1; t1 = OVL_CHAIN (t1))
	{
	  tree t = OVL_FUNCTION (t1);

	  if (TREE_CODE (t) == TEMPLATE_DECL)
	    {
	      templates = tree_cons (NULL_TREE, t, templates);
	      candidates = add_template_candidate
		(candidates, t, NULL_TREE, explicit_targs, args, NULL_TREE,
		 LOOKUP_NORMAL, DEDUCE_CALL);  
	    }
	  else if (! template_only)
	    candidates = add_function_candidate
	      (candidates, t, NULL_TREE, args, LOOKUP_NORMAL);
	}

      if (! any_viable (candidates))
	{
	  if (candidates && ! candidates->next)
	    return build_function_call (candidates->fn, args);
	  cp_error ("no matching function for call to `%D (%A)'",
		    DECL_NAME (OVL_FUNCTION (fn)), args);
	  if (candidates)
	    print_z_candidates (candidates);
	  return error_mark_node;
	}
      candidates = splice_viable (candidates);
      cand = tourney (candidates);

      if (cand == 0)
	{
	  cp_error ("call of overloaded `%D (%A)' is ambiguous",
		    DECL_NAME (OVL_FUNCTION (fn)), args);
	  print_z_candidates (candidates);
	  return error_mark_node;
	}

      /* Pedantically, normal function declarations are never considered
	 to refer to template instantiations, so we only do this with
	 -fguiding-decls.  */
      if (flag_guiding_decls && templates && ! cand->template 
	  && ! DECL_INITIAL (cand->fn))
	add_maybe_template (cand->fn, templates);

      return build_over_call (cand, args, LOOKUP_NORMAL);
    }

  /* This is not really overloaded. */
  fn = OVL_CURRENT (fn);

  return build_function_call (fn, args);
}

static tree
build_object_call (obj, args)
     tree obj, args;
{
  struct z_candidate *candidates = 0, *cand;
  tree fns, convs, mem_args = NULL_TREE;
  tree type = TREE_TYPE (obj);

  if (TYPE_PTRMEMFUNC_P (type))
    {
      /* It's no good looking for an overloaded operator() on a
	 pointer-to-member-function.  */
      cp_error ("pointer-to-member function %E cannot be called without an object; consider using .* or ->*", obj);
      return error_mark_node;
    }

  fns = lookup_fnfields (TYPE_BINFO (type), ansi_opname [CALL_EXPR], 1);
  if (fns == error_mark_node)
    return error_mark_node;

  args = resolve_args (args);

  if (args == error_mark_node)
    return error_mark_node;

  if (fns)
    {
      tree base = BINFO_TYPE (TREE_PURPOSE (fns));
      mem_args = tree_cons (NULL_TREE, build_this (obj), args);

      for (fns = TREE_VALUE (fns); fns; fns = OVL_NEXT (fns))
	{
	  tree fn = OVL_CURRENT (fns);
	  if (TREE_CODE (fn) == TEMPLATE_DECL)
	    {
	      candidates 
		= add_template_candidate (candidates, fn, base, NULL_TREE,
					  mem_args, NULL_TREE, 
					  LOOKUP_NORMAL, DEDUCE_CALL);
	    }
	  else
	    candidates = add_function_candidate
	      (candidates, fn, base, mem_args, LOOKUP_NORMAL);

	  if (candidates)
	    candidates->basetype_path = TYPE_BINFO (type);
	}
    }

  convs = lookup_conversions (type);

  for (; convs; convs = TREE_CHAIN (convs))
    {
      tree fns = TREE_VALUE (convs);
      tree totype = TREE_TYPE (TREE_TYPE (OVL_CURRENT (fns)));

      if ((TREE_CODE (totype) == POINTER_TYPE
	   || TREE_CODE (totype) == REFERENCE_TYPE)
	  && TREE_CODE (TREE_TYPE (totype)) == FUNCTION_TYPE)
	for (; fns; fns = OVL_NEXT (fns))
	  {
	    tree fn = OVL_CURRENT (fns);
	    if (TREE_CODE (fn) == TEMPLATE_DECL) 
	      {
		candidates = add_template_conv_candidate (candidates,
							  fn,
							  obj,
							  args,
							  totype);
	      }
	    else
	      candidates = add_conv_candidate (candidates, fn, obj, args);
	  }
    }

  if (! any_viable (candidates))
    {
      cp_error ("no match for call to `(%T) (%A)'", TREE_TYPE (obj), args);
      print_z_candidates (candidates);
      return error_mark_node;
    }

  candidates = splice_viable (candidates);
  cand = tourney (candidates);

  if (cand == 0)
    {
      cp_error ("call of `(%T) (%A)' is ambiguous", TREE_TYPE (obj), args);
      print_z_candidates (candidates);
      return error_mark_node;
    }

  /* Since cand->fn will be a type, not a function, for a conversion
     function, we must be careful not to unconditionally look at
     DECL_NAME here.  */
  if (TREE_CODE (cand->fn) == FUNCTION_DECL
      && DECL_NAME (cand->fn) == ansi_opname [CALL_EXPR])
    return build_over_call (cand, mem_args, LOOKUP_NORMAL);

  obj = convert_like (TREE_VEC_ELT (cand->convs, 0), obj);

  /* FIXME */
  return build_function_call (obj, args);
}

static void
op_error (code, code2, arg1, arg2, arg3, problem)
     enum tree_code code, code2;
     tree arg1, arg2, arg3;
     const char *problem;
{
  const char * opname
    = (code == MODIFY_EXPR ? assignop_tab [code2] : opname_tab [code]);

  switch (code)
    {
    case COND_EXPR:
      cp_error ("%s for `%T ? %T : %T'", problem,
		error_type (arg1), error_type (arg2), error_type (arg3));
      break;
    case POSTINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      cp_error ("%s for `%T%s'", problem, error_type (arg1), opname);
      break;
    case ARRAY_REF:
      cp_error ("%s for `%T[%T]'", problem,
		error_type (arg1), error_type (arg2));
      break;
    default:
      if (arg2)
	cp_error ("%s for `%T %s %T'", problem,
		  error_type (arg1), opname, error_type (arg2));
      else
	cp_error ("%s for `%s%T'", problem, opname, error_type (arg1));
    }
}

/* Return the implicit conversion sequence that could be used to
   convert E1 to E2 in [expr.cond].  */

static tree
conditional_conversion (e1, e2)
     tree e1;
     tree e2;
{
  tree t1 = non_reference (TREE_TYPE (e1));
  tree t2 = non_reference (TREE_TYPE (e2));
  tree conv;

  /* [expr.cond]

     If E2 is an lvalue: E1 can be converted to match E2 if E1 can be
     implicitly converted (clause _conv_) to the type "reference to
     T2", subject to the constraint that in the conversion the
     reference must bind directly (_dcl.init.ref_) to E1.  */
  if (real_lvalue_p (e2))
    {
      conv = implicit_conversion (build_reference_type (t2), 
				  t1,
				  e1,
				  LOOKUP_NO_TEMP_BIND);
      if (conv)
	return conv;
    }

  /* [expr.cond]

     If E1 and E2 have class type, and the underlying class types are
     the same or one is a base class of the other: E1 can be converted
     to match E2 if the class of T2 is the same type as, or a base
     class of, the class of T1, and the cv-qualification of T2 is the
     same cv-qualification as, or a greater cv-qualification than, the
     cv-qualification of T1.  If the conversion is applied, E1 is
     changed to an rvalue of type T2 that still refers to the original
     source class object (or the appropriate subobject thereof).  */
  if (CLASS_TYPE_P (t1) && CLASS_TYPE_P (t2)
      && same_or_base_type_p (TYPE_MAIN_VARIANT (t2), 
			      TYPE_MAIN_VARIANT (t1)))
    {
      if (at_least_as_qualified_p (t2, t1))
	{
	  conv = build1 (IDENTITY_CONV, t1, e1);
	  if (!same_type_p (TYPE_MAIN_VARIANT (t1), 
			    TYPE_MAIN_VARIANT (t2)))
	    conv = build_conv (BASE_CONV, t2, conv);
	  return conv;
	}
      else
	return NULL_TREE;
    }

  /* [expr.cond]

     E1 can be converted to match E2 if E1 can be implicitly converted
     to the type that expression E2 would have if E2 were converted to
     an rvalue (or the type it has, if E2 is an rvalue).  */
  return implicit_conversion (t2, t1, e1, LOOKUP_NORMAL);
}

/* Implement [expr.cond].  ARG1, ARG2, and ARG3 are the three
   arguments to the conditional expression.  By the time this function
   is called, any suitable candidate functions are included in
   CANDIDATES.  */

tree
build_conditional_expr (arg1, arg2, arg3)
     tree arg1;
     tree arg2;
     tree arg3;
{
  tree arg2_type;
  tree arg3_type;
  tree result;
  tree result_type = NULL_TREE;
  int lvalue_p = 1;
  struct z_candidate *candidates = 0;
  struct z_candidate *cand;

  /* As a G++ extension, the second argument to the conditional can be
     omitted.  (So that `a ? : c' is roughly equivalent to `a ? a :
     c'.)  If the second operand is omitted, make sure it is
     calculated only once.  */
  if (!arg2)
    {
      if (pedantic)
	pedwarn ("ISO C++ forbids omitting the middle term of a ?: expression");
      arg1 = arg2 = save_expr (arg1);
    }

  /* [expr.cond]
  
     The first expr ession is implicitly converted to bool (clause
     _conv_).  */
  arg1 = cp_convert (boolean_type_node, arg1);

  /* If something has already gone wrong, just pass that fact up the
     tree.  */
  if (arg1 == error_mark_node 
      || arg2 == error_mark_node 
      || arg3 == error_mark_node 
      || TREE_TYPE (arg1) == error_mark_node
      || TREE_TYPE (arg2) == error_mark_node
      || TREE_TYPE (arg3) == error_mark_node)
    return error_mark_node;

  /* Convert from reference types to ordinary types; no expressions
     really have reference type in C++.  */
  arg2 = convert_from_reference (arg2);
  arg3 = convert_from_reference (arg3);
     
  /* [expr.cond]

     If either the second or the third operand has type (possibly
     cv-qualified) void, then the lvalue-to-rvalue (_conv.lval_),
     array-to-pointer (_conv.array_), and function-to-pointer
     (_conv.func_) standard conversions are performed on the second
     and third operands.  */
  arg2_type = TREE_TYPE (arg2);
  arg3_type = TREE_TYPE (arg3);
  if (same_type_p (TYPE_MAIN_VARIANT (arg2_type), void_type_node)
      || same_type_p (TYPE_MAIN_VARIANT (arg3_type), void_type_node))
    {
      int arg2_void_p;
      int arg3_void_p;

      /* Do the conversions.  We don't these for `void' type arguments
	 since it can't have any effect and since decay_conversion
	 does not handle that case gracefully.  */
      if (!same_type_p (TYPE_MAIN_VARIANT (arg2_type), void_type_node))
	arg2 = decay_conversion (arg2);
      if (!same_type_p (TYPE_MAIN_VARIANT (arg3_type), void_type_node))
	arg3 = decay_conversion (arg3);
      arg2_type = TREE_TYPE (arg2);
      arg3_type = TREE_TYPE (arg3);

      arg2_void_p = same_type_p (TYPE_MAIN_VARIANT (arg2_type),
				 void_type_node);
      arg3_void_p = same_type_p (TYPE_MAIN_VARIANT (arg3_type),
				 void_type_node);

      /* [expr.cond]

	 One of the following shall hold:

	 --The second or the third operand (but not both) is a
	   throw-expression (_except.throw_); the result is of the
	   type of the other and is an rvalue.

	 --Both the second and the third operands have type void; the
	   result is of type void and is an rvalue.   */
      if ((TREE_CODE (arg2) == THROW_EXPR)
	  ^ (TREE_CODE (arg3) == THROW_EXPR))
	result_type = ((TREE_CODE (arg2) == THROW_EXPR) 
		       ? arg3_type : arg2_type);
      else if (arg2_void_p && arg3_void_p)
	result_type = void_type_node;
      else
	{
	  cp_error ("`%E' has type `void' and is not a throw-expression",
		    arg2_void_p ? arg2 : arg3);
	  return error_mark_node;
	}

      lvalue_p = 0;
      goto valid_operands;
    }
  /* [expr.cond]

     Otherwise, if the second and third operand have different types,
     and either has (possibly cv-qualified) class type, an attempt is
     made to convert each of those operands to the type of the other.  */
  else if (!same_type_p (arg2_type, arg3_type)
	   && (CLASS_TYPE_P (arg2_type) || CLASS_TYPE_P (arg3_type)))
    {
      tree conv2 = conditional_conversion (arg2, arg3);
      tree conv3 = conditional_conversion (arg3, arg2);
      
      /* [expr.cond]

	 If both can be converted, or one can be converted but the
	 conversion is ambiguous, the program is ill-formed.  If
	 neither can be converted, the operands are left unchanged and
	 further checking is performed as described below.  If exactly
	 one conversion is possible, that conversion is applied to the
	 chosen operand and the converted operand is used in place of
	 the original operand for the remainder of this section.  */
      if ((conv2 && !ICS_BAD_FLAG (conv2) 
	   && conv3 && !ICS_BAD_FLAG (conv3))
	  || (conv2 && TREE_CODE (conv2) == AMBIG_CONV)
	  || (conv3 && TREE_CODE (conv3) == AMBIG_CONV))
	{
	  cp_error ("operands to ?: have different types");
	  return error_mark_node;
	}
      else if (conv2 && !ICS_BAD_FLAG (conv2))
	{
	  arg2 = convert_like (conv2, arg2);
	  arg2 = convert_from_reference (arg2);
	  /* That may not quite have done the trick.  If the two types
	     are cv-qualified variants of one another, we will have
	     just used an IDENTITY_CONV.  (There's no conversion from
	     an lvalue of one class type to an lvalue of another type,
	     even a cv-qualified variant, and we don't want to lose
	     lvalue-ness here.)  So, we manually add a NOP_EXPR here
	     if necessary.  */
	  if (!same_type_p (TREE_TYPE (arg2), arg3_type))
	    arg2 = build1 (NOP_EXPR, arg3_type, arg2);
	  arg2_type = TREE_TYPE (arg2);
	}
      else if (conv3 && !ICS_BAD_FLAG (conv3))
	{
	  arg3 = convert_like (conv3, arg3);
	  arg3 = convert_from_reference (arg3);
	  if (!same_type_p (TREE_TYPE (arg3), arg2_type))
	    arg3 = build1 (NOP_EXPR, arg2_type, arg3);
	  arg3_type = TREE_TYPE (arg3);
	}
    }

  /* [expr.cond]

     If the second and third operands are lvalues and have the same
     type, the result is of that type and is an lvalue.  */
  if (real_lvalue_p (arg2) && real_lvalue_p (arg3) && 
      same_type_p (arg2_type, arg3_type))
    {
      result_type = arg2_type;
      goto valid_operands;
    }

  /* [expr.cond]

     Otherwise, the result is an rvalue.  If the second and third
     operand do not have the same type, and either has (possibly
     cv-qualified) class type, overload resolution is used to
     determine the conversions (if any) to be applied to the operands
     (_over.match.oper_, _over.built_).  */
  lvalue_p = 0;
  if (!same_type_p (arg2_type, arg3_type)
      && (CLASS_TYPE_P (arg2_type) || CLASS_TYPE_P (arg3_type)))
    {
      tree args[3];
      tree conv;

      /* Rearrange the arguments so that add_builtin_candidate only has
	 to know about two args.  In build_builtin_candidates, the
	 arguments are unscrambled.  */
      args[0] = arg2;
      args[1] = arg3;
      args[2] = arg1;
      candidates = add_builtin_candidates (candidates, 
					   COND_EXPR, 
					   NOP_EXPR,
					   ansi_opname[COND_EXPR],
					   args,
					   LOOKUP_NORMAL);

      /* [expr.cond]

	 If the overload resolution fails, the program is
	 ill-formed.  */
      if (!any_viable (candidates))
	{
	  op_error (COND_EXPR, NOP_EXPR, arg1, arg2, arg3, "no match");
	  print_z_candidates (candidates);
	  return error_mark_node;
	}
      candidates = splice_viable (candidates);
      cand = tourney (candidates);
      if (!cand)
	{
	  op_error (COND_EXPR, NOP_EXPR, arg1, arg2, arg3, "no match");
	  print_z_candidates (candidates);
	  return error_mark_node;
	}

      /* [expr.cond]

	 Otherwise, the conversions thus determined are applied, and
	 the converted operands are used in place of the original
	 operands for the remainder of this section.  */
      conv = TREE_VEC_ELT (cand->convs, 0);
      arg1 = convert_like (conv, arg1);
      conv = TREE_VEC_ELT (cand->convs, 1);
      arg2 = convert_like (conv, arg2);
      conv = TREE_VEC_ELT (cand->convs, 2);
      arg3 = convert_like (conv, arg3);
    }

  /* [expr.cond]

     Lvalue-to-rvalue (_conv.lval_), array-to-pointer (_conv.array_),
     and function-to-pointer (_conv.func_) standard conversions are
     performed on the second and third operands.

     We need to force the lvalue-to-rvalue conversion here for class types,
     so we get TARGET_EXPRs; trying to deal with a COND_EXPR of class rvalues
     that isn't wrapped with a TARGET_EXPR plays havoc with exception
     regions.  */

  if (IS_AGGR_TYPE (TREE_TYPE (arg2)) && real_lvalue_p (arg2))
    arg2 = build_user_type_conversion (TREE_TYPE (arg2), arg2, LOOKUP_NORMAL);
  else
    arg2 = decay_conversion (arg2);
  arg2_type = TREE_TYPE (arg2);

  if (IS_AGGR_TYPE (TREE_TYPE (arg3)) && real_lvalue_p (arg3))
    arg3 = build_user_type_conversion (TREE_TYPE (arg3), arg3, LOOKUP_NORMAL);
  else
    arg3 = decay_conversion (arg3);
  arg3_type = TREE_TYPE (arg3);

  /* [expr.cond]
     
     After those conversions, one of the following shall hold:

     --The second and third operands have the same type; the result  is  of
       that type.  */
  if (same_type_p (arg2_type, arg3_type))
    result_type = arg2_type;
  /* [expr.cond]

     --The second and third operands have arithmetic or enumeration
       type; the usual arithmetic conversions are performed to bring
       them to a common type, and the result is of that type.  */
  else if ((ARITHMETIC_TYPE_P (arg2_type) 
	    || TREE_CODE (arg2_type) == ENUMERAL_TYPE)
	   && (ARITHMETIC_TYPE_P (arg3_type)
	       || TREE_CODE (arg3_type) == ENUMERAL_TYPE))
    {
      /* In this case, there is always a common type.  */
      result_type = type_after_usual_arithmetic_conversions (arg2_type, 
							     arg3_type);
      
      if (TREE_CODE (arg2_type) == ENUMERAL_TYPE
          && TREE_CODE (arg3_type) == ENUMERAL_TYPE)
         cp_warning ("enumeral mismatch in conditional expression: `%T' vs `%T'",
                   arg2_type, arg3_type);
      else if (extra_warnings
               && ((TREE_CODE (arg2_type) == ENUMERAL_TYPE
                    && !same_type_p (arg3_type, type_promotes_to (arg2_type)))
                   || (TREE_CODE (arg3_type) == ENUMERAL_TYPE
                       && !same_type_p (arg2_type, type_promotes_to (arg3_type)))))
        cp_warning ("enumeral and non-enumeral type in conditional expression");
      
      arg2 = perform_implicit_conversion (result_type, arg2);
      arg3 = perform_implicit_conversion (result_type, arg3);
    }
  /* [expr.cond]

     --The second and third operands have pointer type, or one has
       pointer type and the other is a null pointer constant; pointer
       conversions (_conv.ptr_) and qualification conversions
       (_conv.qual_) are performed to bring them to their composite
       pointer type (_expr.rel_).  The result is of the composite
       pointer type.

     --The second and third operands have pointer to member type, or
       one has pointer to member type and the other is a null pointer
       constant; pointer to member conversions (_conv.mem_) and
       qualification conversions (_conv.qual_) are performed to bring
       them to a common type, whose cv-qualification shall match the
       cv-qualification of either the second or the third operand.
       The result is of the common type.   */
  else if ((null_ptr_cst_p (arg2) 
	    && (TYPE_PTR_P (arg3_type) || TYPE_PTRMEM_P (arg3_type)
		|| TYPE_PTRMEMFUNC_P (arg3_type)))
	   || (null_ptr_cst_p (arg3) 
	       && (TYPE_PTR_P (arg2_type) || TYPE_PTRMEM_P (arg2_type)
		|| TYPE_PTRMEMFUNC_P (arg2_type)))
	   || (TYPE_PTR_P (arg2_type) && TYPE_PTR_P (arg3_type))
	   || (TYPE_PTRMEM_P (arg2_type) && TYPE_PTRMEM_P (arg3_type))
	   || (TYPE_PTRMEMFUNC_P (arg2_type) 
	       && TYPE_PTRMEMFUNC_P (arg3_type)))
    {
      result_type = composite_pointer_type (arg2_type, arg3_type, arg2,
					    arg3, "conditional expression");
      arg2 = perform_implicit_conversion (result_type, arg2);
      arg3 = perform_implicit_conversion (result_type, arg3);
    }

  if (!result_type)
    {
      cp_error ("operands to ?: have different types");
      return error_mark_node;
    }

 valid_operands:
  result = fold (build (COND_EXPR, result_type, arg1, arg2, arg3));
  /* Expand both sides into the same slot, hopefully the target of the
     ?: expression.  We used to check for TARGET_EXPRs here, but now we
     sometimes wrap them in NOP_EXPRs so the test would fail.  */
  if (!lvalue_p && IS_AGGR_TYPE (result_type))
    result = build_target_expr_with_type (result, result_type);
  
  /* If this expression is an rvalue, but might be mistaken for an
     lvalue, we must add a NON_LVALUE_EXPR.  */
  if (!lvalue_p && real_lvalue_p (result))
    result = build1 (NON_LVALUE_EXPR, result_type, result);

  return result;
}

tree
build_new_op (code, flags, arg1, arg2, arg3)
     enum tree_code code;
     int flags;
     tree arg1, arg2, arg3;
{
  struct z_candidate *candidates = 0, *cand;
  tree fns, mem_arglist = NULL_TREE, arglist, fnname;
  enum tree_code code2 = NOP_EXPR;
  tree templates = NULL_TREE;
  tree conv;

  if (arg1 == error_mark_node
      || arg2 == error_mark_node
      || arg3 == error_mark_node)
    return error_mark_node;

  /* This can happen if a template takes all non-type parameters, e.g.
     undeclared_template<1, 5, 72>a;  */
  if (code == LT_EXPR && TREE_CODE (arg1) == TEMPLATE_DECL)
    {
      cp_error ("`%D' must be declared before use", arg1);
      return error_mark_node;
    }

  if (code == MODIFY_EXPR)
    {
      code2 = TREE_CODE (arg3);
      arg3 = NULL_TREE;
      fnname = ansi_assopname[code2];
    }
  else
    fnname = ansi_opname[code];

  switch (code)
    {
    case NEW_EXPR:
    case VEC_NEW_EXPR:
    case VEC_DELETE_EXPR:
    case DELETE_EXPR:
      /* Use build_op_new_call and build_op_delete_call instead. */
      my_friendly_abort (981018);

    case CALL_EXPR:
      return build_object_call (arg1, arg2);

    default:
      break;
    }

  /* The comma operator can have void args.  */
  if (TREE_CODE (arg1) == OFFSET_REF)
    arg1 = resolve_offset_ref (arg1);
  if (arg2 && TREE_CODE (arg2) == OFFSET_REF)
    arg2 = resolve_offset_ref (arg2);
  if (arg3 && TREE_CODE (arg3) == OFFSET_REF)
    arg3 = resolve_offset_ref (arg3);

  if (code == COND_EXPR)
    {
      if (arg2 == NULL_TREE
	  || TREE_CODE (TREE_TYPE (arg2)) == VOID_TYPE
	  || TREE_CODE (TREE_TYPE (arg3)) == VOID_TYPE
	  || (! IS_OVERLOAD_TYPE (TREE_TYPE (arg2))
	      && ! IS_OVERLOAD_TYPE (TREE_TYPE (arg3))))
	goto builtin;
    }
  else if (! IS_OVERLOAD_TYPE (TREE_TYPE (arg1))
	   && (! arg2 || ! IS_OVERLOAD_TYPE (TREE_TYPE (arg2))))
    goto builtin;

  if (code == POSTINCREMENT_EXPR || code == POSTDECREMENT_EXPR)
    arg2 = integer_zero_node;

  if (arg2 && arg3)
    arglist = tree_cons (NULL_TREE, arg1, tree_cons
		      (NULL_TREE, arg2, build_tree_list (NULL_TREE, arg3)));
  else if (arg2)
    arglist = tree_cons (NULL_TREE, arg1, build_tree_list (NULL_TREE, arg2));
  else
    arglist = build_tree_list (NULL_TREE, arg1);

  fns = lookup_function_nonclass (fnname, arglist);

  if (fns && TREE_CODE (fns) == TREE_LIST)
    fns = TREE_VALUE (fns);
  for (; fns; fns = OVL_NEXT (fns))
    {
      tree fn = OVL_CURRENT (fns);
      if (TREE_CODE (fn) == TEMPLATE_DECL)
	{
	  templates = tree_cons (NULL_TREE, fn, templates);
	  candidates 
	    = add_template_candidate (candidates, fn, NULL_TREE, NULL_TREE,
				      arglist, TREE_TYPE (fnname),
				      flags, DEDUCE_CALL); 
	}
      else
	candidates = add_function_candidate (candidates, fn, NULL_TREE,
					     arglist, flags);
    }

  if (IS_AGGR_TYPE (TREE_TYPE (arg1)))
    {
      fns = lookup_fnfields (TYPE_BINFO (TREE_TYPE (arg1)), fnname, 1);
      if (fns == error_mark_node)
	return fns;
    }
  else
    fns = NULL_TREE;

  if (fns)
    {
      tree basetype = BINFO_TYPE (TREE_PURPOSE (fns));
      mem_arglist = tree_cons (NULL_TREE, build_this (arg1), TREE_CHAIN (arglist));
      for (fns = TREE_VALUE (fns); fns; fns = OVL_NEXT (fns))
	{
	  tree fn = OVL_CURRENT (fns);
	  tree this_arglist;

	  if (TREE_CODE (TREE_TYPE (fn)) == METHOD_TYPE)
	    this_arglist = mem_arglist;
	  else
	    this_arglist = arglist;

	  if (TREE_CODE (fn) == TEMPLATE_DECL)
	    {
	      /* A member template. */
	      templates = tree_cons (NULL_TREE, fn, templates);
	      candidates 
		= add_template_candidate (candidates, fn, basetype, NULL_TREE,
					  this_arglist,  TREE_TYPE (fnname),
					  flags, DEDUCE_CALL); 
	    }
	  else
	    candidates = add_function_candidate
	      (candidates, fn, basetype, this_arglist, flags);

	  if (candidates)
	    candidates->basetype_path = TYPE_BINFO (TREE_TYPE (arg1));
	}
    }

  {
    tree args[3];

    /* Rearrange the arguments for ?: so that add_builtin_candidate only has
       to know about two args; a builtin candidate will always have a first
       parameter of type bool.  We'll handle that in
       build_builtin_candidate.  */
    if (code == COND_EXPR)
      {
	args[0] = arg2;
	args[1] = arg3;
	args[2] = arg1;
      }
    else
      {
	args[0] = arg1;
	args[1] = arg2;
	args[2] = NULL_TREE;
      }

    candidates = add_builtin_candidates
      (candidates, code, code2, fnname, args, flags);
  }

  if (! any_viable (candidates))
    {
      switch (code)
	{
	case POSTINCREMENT_EXPR:
	case POSTDECREMENT_EXPR:
	  /* Look for an `operator++ (int)'.  If they didn't have
	     one, then we fall back to the old way of doing things.  */
	  if (flags & LOOKUP_COMPLAIN)
	    cp_pedwarn ("no `%D (int)' declared for postfix `%s', trying prefix operator instead",
			fnname, opname_tab [code]);
	  if (code == POSTINCREMENT_EXPR)
	    code = PREINCREMENT_EXPR;
	  else
	    code = PREDECREMENT_EXPR;	
	  return build_new_op (code, flags, arg1, NULL_TREE, NULL_TREE);
	  
	  /* The caller will deal with these.  */
	case ADDR_EXPR:
	case COMPOUND_EXPR:
	case COMPONENT_REF:
	  return NULL_TREE;

	default:
	  break;
	}
      if (flags & LOOKUP_COMPLAIN)
	{
	  op_error (code, code2, arg1, arg2, arg3, "no match");
	  print_z_candidates (candidates);
	}
      return error_mark_node;
    }
  candidates = splice_viable (candidates);
  cand = tourney (candidates);

  if (cand == 0)
    {
      if (flags & LOOKUP_COMPLAIN)
	{
	  op_error (code, code2, arg1, arg2, arg3, "ambiguous overload");
	  print_z_candidates (candidates);
	}
      return error_mark_node;
    }

  if (TREE_CODE (cand->fn) == FUNCTION_DECL)
    {
      extern int warn_synth;
      if (warn_synth
	  && fnname == ansi_opname[MODIFY_EXPR]
	  && DECL_ARTIFICIAL (cand->fn)
	  && candidates->next
	  && ! candidates->next->next)
	{
	  cp_warning ("using synthesized `%#D' for copy assignment",
		      cand->fn);
	  cp_warning_at ("  where cfront would use `%#D'",
			 cand == candidates
			 ? candidates->next->fn
			 : candidates->fn);
	}

      /* Pedantically, normal function declarations are never considered
	 to refer to template instantiations, so we only do this with
	 -fguiding-decls.  */ 
      if (flag_guiding_decls && templates && ! cand->template 
	  && ! DECL_INITIAL (cand->fn)
	  && TREE_CODE (TREE_TYPE (cand->fn)) != METHOD_TYPE)
	add_maybe_template (cand->fn, templates);

      return build_over_call
	(cand,
	 TREE_CODE (TREE_TYPE (cand->fn)) == METHOD_TYPE
	 ? mem_arglist : arglist,
	 LOOKUP_NORMAL);
    }

  /* Check for comparison of different enum types.  */
  switch (code)
    {
    case GT_EXPR:
    case LT_EXPR:
    case GE_EXPR:
    case LE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
      if (TREE_CODE (TREE_TYPE (arg1)) == ENUMERAL_TYPE 
	  && TREE_CODE (TREE_TYPE (arg2)) == ENUMERAL_TYPE 
	  && (TYPE_MAIN_VARIANT (TREE_TYPE (arg1))
	      != TYPE_MAIN_VARIANT (TREE_TYPE (arg2))))
	{
	  cp_warning ("comparison between `%#T' and `%#T'", 
		      TREE_TYPE (arg1), TREE_TYPE (arg2));
	}
      break;
    default:
      break;
    }

  /* We need to strip any leading REF_BIND so that bitfields don't cause
     errors.  This should not remove any important conversions, because
     builtins don't apply to class objects directly.  */
  conv = TREE_VEC_ELT (cand->convs, 0);
  if (TREE_CODE (conv) == REF_BIND)
    conv = TREE_OPERAND (conv, 0);
  arg1 = convert_like (conv, arg1);
  if (arg2)
    {
      conv = TREE_VEC_ELT (cand->convs, 1);
      if (TREE_CODE (conv) == REF_BIND)
        conv = TREE_OPERAND (conv, 0);
      arg2 = convert_like (conv, arg2);
    }
  if (arg3)
    {
      conv = TREE_VEC_ELT (cand->convs, 2);
      if (TREE_CODE (conv) == REF_BIND)
        conv = TREE_OPERAND (conv, 0);
      arg3 = convert_like (conv, arg3);
    }

builtin:
  switch (code)
    {
    case MODIFY_EXPR:
      return build_modify_expr (arg1, code2, arg2);

    case INDIRECT_REF:
      return build_indirect_ref (arg1, "unary *");

    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case GT_EXPR:
    case LT_EXPR:
    case GE_EXPR:
    case LE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case MAX_EXPR:
    case MIN_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case TRUNC_MOD_EXPR:
    case BIT_AND_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
      return build_binary_op (code, arg1, arg2);

    case CONVERT_EXPR:
    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
    case TRUTH_NOT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
      return build_unary_op (code, arg1, candidates != 0);

    case ARRAY_REF:
      return build_array_ref (arg1, arg2);

    case COND_EXPR:
      return build_conditional_expr (arg1, arg2, arg3);

    case MEMBER_REF:
      return build_m_component_ref
	(build_indirect_ref (arg1, NULL_PTR), arg2);

      /* The caller will deal with these.  */
    case ADDR_EXPR:
    case COMPONENT_REF:
    case COMPOUND_EXPR:
      return NULL_TREE;

    default:
      my_friendly_abort (367);
      return NULL_TREE;
    }
}

/* Build a call to operator delete.  This has to be handled very specially,
   because the restrictions on what signatures match are different from all
   other call instances.  For a normal delete, only a delete taking (void *)
   or (void *, size_t) is accepted.  For a placement delete, only an exact
   match with the placement new is accepted.

   CODE is either DELETE_EXPR or VEC_DELETE_EXPR.
   ADDR is the pointer to be deleted.  For placement delete, it is also
     used to determine what the corresponding new looked like.
   SIZE is the size of the memory block to be deleted.
   FLAGS are the usual overloading flags.
   PLACEMENT is the corresponding placement new call, or 0.  */

tree
build_op_delete_call (code, addr, size, flags, placement)
     enum tree_code code;
     tree addr, size, placement;
     int flags;
{
  tree fn, fns, fnname, fntype, argtypes, args, type;

  if (addr == error_mark_node)
    return error_mark_node;

  type = TREE_TYPE (TREE_TYPE (addr));
  fnname = ansi_opname[code];

  if (IS_AGGR_TYPE (type) && ! (flags & LOOKUP_GLOBAL))
    /* In [class.free]

       If the result of the lookup is ambiguous or inaccessible, or if
       the lookup selects a placement deallocation function, the
       program is ill-formed.
  
       Therefore, we ask lookup_fnfields to complain ambout ambiguity.  */
    {
      fns = lookup_fnfields (TYPE_BINFO (type), fnname, 1);
      if (fns == error_mark_node)
	return error_mark_node;
    }
  else
    fns = NULL_TREE;

  if (fns == NULL_TREE)
    fns = lookup_name_nonclass (fnname);

  if (placement)
    {
      /* placement is a CALL_EXPR around an ADDR_EXPR around a function.  */

      /* Extract the function.  */
      argtypes = TREE_OPERAND (TREE_OPERAND (placement, 0), 0);
      /* Then the second parm type.  */
      argtypes = TREE_CHAIN (TYPE_ARG_TYPES (TREE_TYPE (argtypes)));

      /* Also the second argument.  */
      args = TREE_CHAIN (TREE_OPERAND (placement, 1));
    }
  else
    {
      /* First try it without the size argument.  */
      argtypes = void_list_node;
      args = NULL_TREE;
    }

  argtypes = tree_cons (NULL_TREE, ptr_type_node, argtypes);
  fntype = build_function_type (void_type_node, argtypes);

  /* Strip const and volatile from addr.  */
  if (type != TYPE_MAIN_VARIANT (type))
    addr = cp_convert (build_pointer_type (TYPE_MAIN_VARIANT (type)), addr);

  fn = instantiate_type (fntype, fns, 2);

  if (fn != error_mark_node)
    {
      if (TREE_CODE (fns) == TREE_LIST)
	/* Member functions.  */
	enforce_access (type, fn);
      return build_function_call (fn, tree_cons (NULL_TREE, addr, args));
    }

  /* If we are doing placement delete we do nothing if we don't find a
     matching op delete.  */
  if (placement)
    return NULL_TREE;

  /* Normal delete; now try to find a match including the size argument.  */
  argtypes = tree_cons (NULL_TREE, ptr_type_node,
			tree_cons (NULL_TREE, sizetype, void_list_node));
  fntype = build_function_type (void_type_node, argtypes);

  fn = instantiate_type (fntype, fns, 2);

  if (fn != error_mark_node)
    {
      if (BASELINK_P (fns))
	/* Member functions.  */
	enforce_access (type, fn);
      return build_function_call
	(fn, tree_cons (NULL_TREE, addr,
			build_tree_list (NULL_TREE, size)));
    }

  /* finish_function passes LOOKUP_SPECULATIVELY if we're in a
     destructor, in which case the error should be deferred
     until someone actually tries to delete one of these.  */
  if (flags & LOOKUP_SPECULATIVELY)
    return NULL_TREE;

  cp_error ("no suitable `operator delete' for `%T'", type);
  return error_mark_node;
}

/* If the current scope isn't allowed to access DECL along
   BASETYPE_PATH, give an error.  The most derived class in
   BASETYPE_PATH is the one used to qualify DECL.  */

int
enforce_access (basetype_path, decl)
     tree basetype_path;
     tree decl;
{
  int accessible;

  accessible = accessible_p (basetype_path, decl);
  if (!accessible)
    {
      if (TREE_PRIVATE (decl))
	cp_error_at ("`%+#D' is private", decl);
      else if (TREE_PROTECTED (decl))
	cp_error_at ("`%+#D' is protected", decl);
      else
	cp_error_at ("`%+#D' is inaccessible", decl);
      cp_error ("within this context");
      return 0;
    }

  return 1;
}

/* Perform the conversions in CONVS on the expression EXPR.  */

static tree
convert_like (convs, expr)
     tree convs, expr;
{
  if (ICS_BAD_FLAG (convs)
      && TREE_CODE (convs) != USER_CONV
      && TREE_CODE (convs) != AMBIG_CONV
      && TREE_CODE (convs) != REF_BIND)
    {
      tree t = convs; 
      for (; t; t = TREE_OPERAND (t, 0))
	{
	  if (TREE_CODE (t) == USER_CONV)
	    {
	      expr = convert_like (t, expr);
	      break;
	    }
	  else if (TREE_CODE (t) == AMBIG_CONV)
	    return convert_like (t, expr);
	  else if (TREE_CODE (t) == IDENTITY_CONV)
	    break;
	}
      return convert_for_initialization
	(NULL_TREE, TREE_TYPE (convs), expr, LOOKUP_NORMAL,
	 "conversion", NULL_TREE, 0);
    }

  switch (TREE_CODE (convs))
    {
    case USER_CONV:
      {
	struct z_candidate *cand
	  = WRAPPER_PTR (TREE_OPERAND (convs, 1));
	tree fn = cand->fn;
	tree args;

	if (DECL_CONSTRUCTOR_P (fn))
	  {
	    tree t = build_int_2 (0, 0);
	    TREE_TYPE (t) = build_pointer_type (DECL_CONTEXT (fn));

	    args = build_tree_list (NULL_TREE, expr);
	    if (TYPE_USES_VIRTUAL_BASECLASSES (DECL_CONTEXT (fn)))
	      args = tree_cons (NULL_TREE, integer_one_node, args);
	    args = tree_cons (NULL_TREE, t, args);
	  }
	else
	  args = build_this (expr);
	expr = build_over_call (cand, args, LOOKUP_NORMAL);

	/* If this is a constructor or a function returning an aggr type,
	   we need to build up a TARGET_EXPR.  */
	if (DECL_CONSTRUCTOR_P (fn))
	  expr = build_cplus_new (TREE_TYPE (convs), expr);

	return expr;
      }
    case IDENTITY_CONV:
      if (type_unknown_p (expr))
	expr = instantiate_type (TREE_TYPE (convs), expr, 1);
      return expr;
    case AMBIG_CONV:
      /* Call build_user_type_conversion again for the error.  */
      return build_user_type_conversion
	(TREE_TYPE (convs), TREE_OPERAND (convs, 0), LOOKUP_NORMAL);

    default:
      break;
    };

  expr = convert_like (TREE_OPERAND (convs, 0), expr);
  if (expr == error_mark_node)
    return error_mark_node;

  /* Convert a constant variable to its underlying value, unless we
     are about to bind it to a reference, in which case we need to
     leave it as an lvalue.  */
  if (TREE_READONLY_DECL_P (expr) && TREE_CODE (convs) != REF_BIND)
    expr = decl_constant_value (expr);

  switch (TREE_CODE (convs))
    {
    case RVALUE_CONV:
      if (! IS_AGGR_TYPE (TREE_TYPE (convs)))
	return expr;
      /* else fall through */
    case BASE_CONV:
      if (TREE_CODE (convs) == BASE_CONV && !NEED_TEMPORARY_P (convs))
	{
	  /* We are going to bind a reference directly to a base-class
	     subobject of EXPR.  */
	  tree base_ptr = build_pointer_type (TREE_TYPE (convs));

	  /* Build an expression for `*((base*) &expr)'.  */
	  expr = build_unary_op (ADDR_EXPR, expr, 0);
	  expr = perform_implicit_conversion (base_ptr, expr);
	  expr = build_indirect_ref (expr, "implicit conversion");
	  return expr;
	}

      {
	tree cvt_expr = build_user_type_conversion
	  (TREE_TYPE (convs), expr, LOOKUP_NORMAL);
	if (!cvt_expr) 
	  {
	    /* This can occur if, for example, the EXPR has incomplete
	       type.  We can't check for that before attempting the
	       conversion because the type might be an incomplete
	       array type, which is OK if some constructor for the
	       destination type takes a pointer argument.  */
	    if (TYPE_SIZE (TREE_TYPE (expr)) == 0)
	      {
		if (same_type_p (TREE_TYPE (expr), TREE_TYPE (convs)))
		  incomplete_type_error (expr, TREE_TYPE (expr));
		else
		  cp_error ("could not convert `%E' (with incomplete type `%T') to `%T'",
			    expr, TREE_TYPE (expr), TREE_TYPE (convs));
	      }
	    else
	      cp_error ("could not convert `%E' to `%T'",
			expr, TREE_TYPE (convs));
	    return error_mark_node;
	  }
	return cvt_expr;
      }

    case REF_BIND:
      {
	tree ref_type = TREE_TYPE (convs);

	/* If necessary, create a temporary.  */
	if (NEED_TEMPORARY_P (convs))
	  {
	    tree type = TREE_TYPE (TREE_OPERAND (convs, 0));
	    expr = build_target_expr_with_type (expr, type);
	  }

	/* Take the address of the thing to which we will bind the
	   reference.  */
	expr = build_unary_op (ADDR_EXPR, expr, 1);
	if (expr == error_mark_node)
	  return error_mark_node;

	/* Convert it to a pointer to the type referred to by the
	   reference.  This will adjust the pointer if a derived to
	   base conversion is being performed.  */
	expr = cp_convert (build_pointer_type (TREE_TYPE (ref_type)), 
			   expr);
	/* Convert the pointer to the desired reference type.  */
	expr = build1 (NOP_EXPR, ref_type, expr);

	return expr;
      }

    case LVALUE_CONV:
      return decay_conversion (expr);

    case QUAL_CONV:
      /* Warn about deprecated conversion if appropriate.  */
      string_conv_p (TREE_TYPE (convs), expr, 1);
      break;
      
    default:
      break;
    }
  return ocp_convert (TREE_TYPE (convs), expr, CONV_IMPLICIT,
		      LOOKUP_NORMAL|LOOKUP_NO_CONVERSION);
}

/* ARG is being passed to a varargs function.  Perform any conversions
   required.  Array/function to pointer decay must have already happened.
   Return the converted value.  */

tree
convert_arg_to_ellipsis (arg)
     tree arg;
{
  if (TREE_CODE (TREE_TYPE (arg)) == REAL_TYPE
      && (TYPE_PRECISION (TREE_TYPE (arg))
	  < TYPE_PRECISION (double_type_node)))
    /* Convert `float' to `double'.  */
    arg = cp_convert (double_type_node, arg);
  else
    /* Convert `short' and `char' to full-size `int'.  */
    arg = default_conversion (arg);

  arg = require_complete_type (arg);
  
  if (arg != error_mark_node && ! pod_type_p (TREE_TYPE (arg)))
    {
      /* Undefined behaviour [expr.call] 5.2.2/7.  */
      cp_warning ("cannot pass objects of non-POD type `%#T' through `...'",
		  TREE_TYPE (arg));
    }

  return arg;
}

/* va_arg (EXPR, TYPE) is a builtin. Make sure it is not abused.  */

tree
build_x_va_arg (expr, type)
     tree expr;
     tree type;
{
  type = complete_type_or_else (type, NULL_TREE);

  if (expr == error_mark_node || !type)
    return error_mark_node;
  
  if (! pod_type_p (type))
    {
      /* Undefined behaviour [expr.call] 5.2.2/7.  */
      cp_warning ("cannot receive objects of non-POD type `%#T' through `...'",
		  type);
    }
  
  return build_va_arg (expr, type);
}

/* TYPE has been given to va_arg. Apply the default conversions which would
   have happened when passed via ellipsis. Return the promoted type, or
   NULL_TREE, if there is no change.  */

tree
convert_type_from_ellipsis (type)
     tree type;
{
  tree promote;
  
  if (TREE_CODE (type) == ARRAY_TYPE)
    promote = build_pointer_type (TREE_TYPE (type));
  else if (TREE_CODE (type) == FUNCTION_TYPE)
    promote = build_pointer_type (type);
  else
    promote = type_promotes_to (type);
  
  return same_type_p (type, promote) ? NULL_TREE : promote;
}

/* ARG is a default argument expression being passed to a parameter of
   the indicated TYPE, which is a parameter to FN.  Do any required
   conversions.  Return the converted value.  */

tree
convert_default_arg (type, arg, fn)
     tree type;
     tree arg;
     tree fn;
{
  if (fn && DECL_TEMPLATE_INFO (fn))
    arg = tsubst_default_argument (fn, type, arg);

  arg = break_out_target_exprs (arg);

  if (TREE_CODE (arg) == CONSTRUCTOR)
    {
      arg = digest_init (type, arg, 0);
      arg = convert_for_initialization (0, type, arg, LOOKUP_NORMAL,
					"default argument", 0, 0);
    }
  else
    {
      /* This could get clobbered by the following call.  */
      if (TREE_HAS_CONSTRUCTOR (arg))
	arg = copy_node (arg);

      arg = convert_for_initialization (0, type, arg, LOOKUP_NORMAL,
					"default argument", 0, 0);
      if (PROMOTE_PROTOTYPES
	  && (TREE_CODE (type) == INTEGER_TYPE
	      || TREE_CODE (type) == ENUMERAL_TYPE)
	  && (TYPE_PRECISION (type) < TYPE_PRECISION (integer_type_node)))
	arg = default_conversion (arg);
    }

  return arg;
}

static tree
build_over_call (cand, args, flags)
     struct z_candidate *cand;
     tree args;
     int flags;
{
  tree fn = cand->fn;
  tree convs = cand->convs;
  tree converted_args = NULL_TREE;
  tree parm = TYPE_ARG_TYPES (TREE_TYPE (fn));
  tree conv, arg, val;
  int i = 0;
  int is_method = 0;

  /* Give any warnings we noticed during overload resolution.  */
  if (cand->warnings)
    for (val = cand->warnings; val; val = TREE_CHAIN (val))
      joust (cand, WRAPPER_PTR (TREE_VALUE (val)), 1);

  if (DECL_FUNCTION_MEMBER_P (fn))
    enforce_access (cand->basetype_path, fn);

  if (args && TREE_CODE (args) != TREE_LIST)
    args = build_tree_list (NULL_TREE, args);
  arg = args;

  /* The implicit parameters to a constructor are not considered by overload
     resolution, and must be of the proper type.  */
  if (DECL_CONSTRUCTOR_P (fn))
    {
      converted_args = tree_cons (NULL_TREE, TREE_VALUE (arg), converted_args);
      arg = TREE_CHAIN (arg);
      parm = TREE_CHAIN (parm);
      if (TYPE_USES_VIRTUAL_BASECLASSES (DECL_CONTEXT (fn)))
	{
	  converted_args = tree_cons
	    (NULL_TREE, TREE_VALUE (arg), converted_args);
	  arg = TREE_CHAIN (arg);
	  parm = TREE_CHAIN (parm);
	}
    }      
  /* Bypass access control for 'this' parameter.  */
  else if (TREE_CODE (TREE_TYPE (fn)) == METHOD_TYPE)
    {
      tree parmtype = TREE_VALUE (parm);
      tree argtype = TREE_TYPE (TREE_VALUE (arg));
      tree t;
      if (ICS_BAD_FLAG (TREE_VEC_ELT (convs, i)))
	cp_pedwarn ("passing `%T' as `this' argument of `%#D' discards qualifiers",
		    TREE_TYPE (argtype), fn);

      /* [class.mfct.nonstatic]: If a nonstatic member function of a class
	 X is called for an object that is not of type X, or of a type
	 derived from X, the behavior is undefined.

         So we can assume that anything passed as 'this' is non-null, and
	 optimize accordingly.  */
      my_friendly_assert (TREE_CODE (parmtype) == POINTER_TYPE, 19990811);
      t = convert_pointer_to_real (TREE_TYPE (parmtype), TREE_VALUE (arg));
      converted_args = tree_cons (NULL_TREE, t, converted_args);
      parm = TREE_CHAIN (parm);
      arg = TREE_CHAIN (arg);
      ++i;
      is_method = 1;
    }

  for (; arg && parm;
       parm = TREE_CHAIN (parm), arg = TREE_CHAIN (arg), ++i)
    {
      tree type = TREE_VALUE (parm);

      conv = TREE_VEC_ELT (convs, i);
      if (ICS_BAD_FLAG (conv))
	{
	  tree t = conv;
	  val = TREE_VALUE (arg);

	  for (; t; t = TREE_OPERAND (t, 0))
	    {
	      if (TREE_CODE (t) == USER_CONV
		  || TREE_CODE (t) == AMBIG_CONV)
		{
		  val = convert_like (t, val);
		  break;
		}
	      else if (TREE_CODE (t) == IDENTITY_CONV)
		break;
	    }
	  val = convert_for_initialization
	    (NULL_TREE, type, val, LOOKUP_NORMAL,
	     "argument passing", fn, i - is_method);
	}
      else
	{
	  /* Issue warnings about peculiar, but legal, uses of NULL.  */
	  if (ARITHMETIC_TYPE_P (TREE_VALUE (parm))
	      && TREE_VALUE (arg) == null_node)
	    cp_warning ("converting NULL to non-pointer type");
	    
	  val = convert_like (conv, TREE_VALUE (arg));
	}

      if (PROMOTE_PROTOTYPES
	  && (TREE_CODE (type) == INTEGER_TYPE
	      || TREE_CODE (type) == ENUMERAL_TYPE)
	  && (TYPE_PRECISION (type) < TYPE_PRECISION (integer_type_node)))
	val = default_conversion (val);
      converted_args = tree_cons (NULL_TREE, val, converted_args);
    }

  /* Default arguments */
  for (; parm && parm != void_list_node; parm = TREE_CHAIN (parm))
    converted_args 
      = tree_cons (NULL_TREE, 
		   convert_default_arg (TREE_VALUE (parm), 
					TREE_PURPOSE (parm),
					fn),
		   converted_args);

  /* Ellipsis */
  for (; arg; arg = TREE_CHAIN (arg))
    converted_args 
      = tree_cons (NULL_TREE,
		   convert_arg_to_ellipsis (TREE_VALUE (arg)),
		   converted_args);

  converted_args = nreverse (converted_args);

  if (warn_format && (DECL_NAME (fn) || DECL_ASSEMBLER_NAME (fn)))
    check_function_format (DECL_NAME (fn), DECL_ASSEMBLER_NAME (fn),
			   converted_args); 

  /* Avoid actually calling copy constructors and copy assignment operators,
     if possible.  */

  if (! flag_elide_constructors)
    /* Do things the hard way.  */;
  else if (DECL_CONSTRUCTOR_P (fn)
	   && TREE_VEC_LENGTH (convs) == 1
	   && copy_args_p (fn))
    {
      tree targ;
      arg = TREE_CHAIN (converted_args);
      if (TYPE_USES_VIRTUAL_BASECLASSES (DECL_CONTEXT (fn)))
	arg = TREE_CHAIN (arg);
      arg = TREE_VALUE (arg);

      /* Pull out the real argument, disregarding const-correctness.  */
      targ = arg;
      while (TREE_CODE (targ) == NOP_EXPR
	     || TREE_CODE (targ) == NON_LVALUE_EXPR
	     || TREE_CODE (targ) == CONVERT_EXPR)
	targ = TREE_OPERAND (targ, 0);
      if (TREE_CODE (targ) == ADDR_EXPR)
	{
	  targ = TREE_OPERAND (targ, 0);
	  if (!same_type_p (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (arg))),
			    TYPE_MAIN_VARIANT (TREE_TYPE (targ))))
	    targ = NULL_TREE;
	}
      else
	targ = NULL_TREE;

      if (targ)
	arg = targ;
      else
	arg = build_indirect_ref (arg, 0);

      /* [class.copy]: the copy constructor is implicitly defined even if
	 the implementation elided its use.  */
      if (TYPE_HAS_COMPLEX_INIT_REF (DECL_CONTEXT (fn)))
	mark_used (fn);

      /* If we're creating a temp and we already have one, don't create a
         new one.  If we're not creating a temp but we get one, use
         INIT_EXPR to collapse the temp into our target.  Otherwise, if the
         ctor is trivial, do a bitwise copy with a simple TARGET_EXPR for a
         temp or an INIT_EXPR otherwise.  */
      if (integer_zerop (TREE_VALUE (args)))
	{
	  if (! real_lvalue_p (arg))
	    return arg;
	  else if (TYPE_HAS_TRIVIAL_INIT_REF (DECL_CONTEXT (fn)))
	    return build_target_expr_with_type (arg, DECL_CONTEXT (fn));
	}
      else if (! real_lvalue_p (arg)
	       || TYPE_HAS_TRIVIAL_INIT_REF (DECL_CONTEXT (fn)))
	{
	  tree address;
	  tree to = stabilize_reference
	    (build_indirect_ref (TREE_VALUE (args), 0));

	  /* If we're initializing an empty class, then we actually
	     have to use a MODIFY_EXPR rather than an INIT_EXPR.  The
	     reason is that the dummy padding member in the target may
	     not actually be allocated if TO is a base class
	     subobject.  Since we've set TYPE_NONCOPIED_PARTS on the
	     padding, a MODIFY_EXPR will preserve its value, which is
	     the right thing to do if it's not really padding at all.
	  
	     It's not safe to just throw away the ARG if we're looking
	     at an empty class because the ARG might contain a
	     TARGET_EXPR which wants to be bound to TO.  If it is not,
	     expand_expr will assign a dummy slot for the TARGET_EXPR,
	     and we will call a destructor for it, which is wrong,
	     because we will also destroy TO, but will never have
	     constructed it.  */
	  val = build (is_empty_class (DECL_CONTEXT (fn))
		       ? MODIFY_EXPR : INIT_EXPR, 
		       DECL_CONTEXT (fn), to, arg);
	  address = build_unary_op (ADDR_EXPR, val, 0);
	  /* Avoid a warning about this expression, if the address is
	     never used.  */
	  TREE_USED (address) = 1;
	  return address;
	}
    }
  else if (DECL_NAME (fn) == ansi_opname[MODIFY_EXPR]
	   && copy_args_p (fn)
	   && TYPE_HAS_TRIVIAL_ASSIGN_REF (DECL_CONTEXT (fn)))
    {
      tree to = stabilize_reference
	(build_indirect_ref (TREE_VALUE (converted_args), 0));

      arg = build_indirect_ref (TREE_VALUE (TREE_CHAIN (converted_args)), 0);

      val = build (MODIFY_EXPR, TREE_TYPE (to), to, arg);
      return val;
    }

  mark_used (fn);

  if (DECL_VINDEX (fn) && (flags & LOOKUP_NONVIRTUAL) == 0)
    {
      tree t, *p = &TREE_VALUE (converted_args);
      tree binfo = get_binfo
	(DECL_VIRTUAL_CONTEXT (fn), TREE_TYPE (TREE_TYPE (*p)), 0);
      *p = convert_pointer_to_real (binfo, *p);
      if (TREE_SIDE_EFFECTS (*p))
	*p = save_expr (*p);
      t = build_pointer_type (TREE_TYPE (fn));
      fn = build_vfn_ref (p, build_indirect_ref (*p, 0), DECL_VINDEX (fn));
      TREE_TYPE (fn) = t;
    }
  else if (DECL_INLINE (fn))
    fn = inline_conversion (fn);
  else
    fn = build_addr_func (fn);

  /* Recognize certain built-in functions so we can make tree-codes
     other than CALL_EXPR.  We do this when it enables fold-const.c
     to do something useful.  */

  if (TREE_CODE (fn) == ADDR_EXPR
      && TREE_CODE (TREE_OPERAND (fn, 0)) == FUNCTION_DECL
      && DECL_BUILT_IN (TREE_OPERAND (fn, 0)))
    {
      tree exp;
      exp = expand_tree_builtin (TREE_OPERAND (fn, 0), args, converted_args);
      if (exp)
	return exp;
    }

  fn = build_call (fn, TREE_TYPE (TREE_TYPE (TREE_TYPE (fn))), converted_args);
  if (TREE_CODE (TREE_TYPE (fn)) == VOID_TYPE)
    return fn;
  fn = require_complete_type (fn);
  if (IS_AGGR_TYPE (TREE_TYPE (fn)))
    fn = build_cplus_new (TREE_TYPE (fn), fn);
  return convert_from_reference (fn);
}

static tree
build_new_method_call (instance, name, args, basetype_path, flags)
     tree instance, name, args, basetype_path;
     int flags;
{
  struct z_candidate *candidates = 0, *cand;
  tree explicit_targs = NULL_TREE;
  tree basetype, mem_args = NULL_TREE, fns, instance_ptr;
  tree pretty_name;
  tree user_args = args;
  tree templates = NULL_TREE;
  int template_only = 0;

  if (TREE_CODE (name) == TEMPLATE_ID_EXPR)
    {
      explicit_targs = TREE_OPERAND (name, 1);
      name = TREE_OPERAND (name, 0);
      if (TREE_CODE_CLASS (TREE_CODE (name)) == 'd')
	name = DECL_NAME (name);
      else
	{
	  if (TREE_CODE (name) == COMPONENT_REF)
	    name = TREE_OPERAND (name, 1);
	  if (TREE_CODE (name) == OVERLOAD)
	    name = DECL_NAME (OVL_CURRENT (name));
	}

      template_only = 1;
    }

  /* If there is an extra argument for controlling virtual bases,
     remove it for error reporting.  */
  if (flags & LOOKUP_HAS_IN_CHARGE)
    user_args = TREE_CHAIN (args);

  args = resolve_args (args);

  if (args == error_mark_node)
    return error_mark_node;

  if (instance == NULL_TREE)
    basetype = BINFO_TYPE (basetype_path);
  else
    {
      if (TREE_CODE (instance) == OFFSET_REF)
	instance = resolve_offset_ref (instance);
      if (TREE_CODE (TREE_TYPE (instance)) == REFERENCE_TYPE)
	instance = convert_from_reference (instance);
      basetype = TYPE_MAIN_VARIANT (TREE_TYPE (instance));

      /* XXX this should be handled before we get here.  */
      if (! IS_AGGR_TYPE (basetype))
	{
	  if ((flags & LOOKUP_COMPLAIN) && basetype != error_mark_node)
	    cp_error ("request for member `%D' in `%E', which is of non-aggregate type `%T'",
		      name, instance, basetype);

	  return error_mark_node;
	}
    }

  if (basetype_path == NULL_TREE)
    basetype_path = TYPE_BINFO (basetype);

  if (instance)
    {
      instance_ptr = build_this (instance);

      if (! template_only)
	{
	  /* XXX this should be handled before we get here.  */
	  fns = build_field_call (basetype_path, instance_ptr, name, args);
	  if (fns)
	    return fns;
	}
    }
  else
    {
      instance_ptr = build_int_2 (0, 0);
      TREE_TYPE (instance_ptr) = build_pointer_type (basetype);
    }

  pretty_name
    = (name == ctor_identifier ? constructor_name (basetype) : name);

  fns = lookup_fnfields (basetype_path, name, 1);

  if (fns == error_mark_node)
    return error_mark_node;
  if (fns)
    {
      tree base = BINFO_TYPE (TREE_PURPOSE (fns));
      tree fn = TREE_VALUE (fns);
      if (name == ctor_identifier && TYPE_USES_VIRTUAL_BASECLASSES (basetype)
	  && ! (flags & LOOKUP_HAS_IN_CHARGE))
	{
	  flags |= LOOKUP_HAS_IN_CHARGE;
	  args = tree_cons (NULL_TREE, integer_one_node, args);
	}
      mem_args = tree_cons (NULL_TREE, instance_ptr, args);
      for (; fn; fn = OVL_NEXT (fn))
	{
	  tree t = OVL_CURRENT (fn);
	  tree this_arglist;

	  /* We can end up here for copy-init of same or base class.  */
	  if (name == ctor_identifier
	      && (flags & LOOKUP_ONLYCONVERTING)
	      && DECL_NONCONVERTING_P (t))
	    continue;
	  if (TREE_CODE (TREE_TYPE (t)) == METHOD_TYPE)
	    this_arglist = mem_args;
	  else
	    this_arglist = args;

	  if (TREE_CODE (t) == TEMPLATE_DECL)
	    {
	      /* A member template. */
	      templates = tree_cons (NULL_TREE, t, templates);
	      candidates = 
		add_template_candidate (candidates, t, base, explicit_targs,
					this_arglist,
					TREE_TYPE (name), flags, DEDUCE_CALL); 
	    }
	  else if (! template_only)
	    candidates = add_function_candidate (candidates, t, base,
						 this_arglist, flags);

	  if (candidates)
	    candidates->basetype_path = basetype_path;
	}
    }

  if (! any_viable (candidates))
    {
      /* XXX will LOOKUP_SPECULATIVELY be needed when this is done?  */
      if (flags & LOOKUP_SPECULATIVELY)
	return NULL_TREE;
      if (TYPE_SIZE (basetype) == 0)
	incomplete_type_error (instance_ptr, basetype);
      else
	cp_error ("no matching function for call to `%T::%D (%A)%V'",
		  basetype, pretty_name, user_args,
		  TREE_TYPE (TREE_TYPE (instance_ptr)));
      print_z_candidates (candidates);
      return error_mark_node;
    }
  candidates = splice_viable (candidates);
  cand = tourney (candidates);

  if (cand == 0)
    {
      cp_error ("call of overloaded `%D(%A)' is ambiguous", pretty_name,
		user_args);
      print_z_candidates (candidates);
      return error_mark_node;
    }

  if (DECL_PURE_VIRTUAL_P (cand->fn)
      && instance == current_class_ref
      && (DECL_CONSTRUCTOR_P (current_function_decl)
	  || DECL_DESTRUCTOR_P (current_function_decl))
      && ! (flags & LOOKUP_NONVIRTUAL)
      && value_member (cand->fn, CLASSTYPE_PURE_VIRTUALS (basetype)))
    cp_error ((DECL_CONSTRUCTOR_P (current_function_decl) ? 
	       "abstract virtual `%#D' called from constructor"
	       : "abstract virtual `%#D' called from destructor"),
	      cand->fn);
  if (TREE_CODE (TREE_TYPE (cand->fn)) == METHOD_TYPE
      && is_dummy_object (instance_ptr))
    {
      cp_error ("cannot call member function `%D' without object", cand->fn);
      return error_mark_node;
    }

  if (DECL_VINDEX (cand->fn) && ! (flags & LOOKUP_NONVIRTUAL)
      && ((instance == current_class_ref && (dtor_label || ctor_label))
	  || resolves_to_fixed_type_p (instance, 0)))
    flags |= LOOKUP_NONVIRTUAL;

  /* Pedantically, normal function declarations are never considered
     to refer to template instantiations, so we only do this with
     -fguiding-decls.  */ 
  if (flag_guiding_decls && templates && ! cand->template 
      && ! DECL_INITIAL (cand->fn))
    add_maybe_template (cand->fn, templates);

  return build_over_call
    (cand,
     TREE_CODE (TREE_TYPE (cand->fn)) == METHOD_TYPE ? mem_args : args,
     flags);
}

/* Returns non-zero iff standard conversion sequence ICS1 is a proper
   subsequence of ICS2.  */

static int
is_subseq (ics1, ics2)
     tree ics1, ics2;
{
  /* We can assume that a conversion of the same code
     between the same types indicates a subsequence since we only get
     here if the types we are converting from are the same.  */

  while (TREE_CODE (ics1) == RVALUE_CONV
	 || TREE_CODE (ics1) == LVALUE_CONV)
    ics1 = TREE_OPERAND (ics1, 0);

  while (1)
    {
      while (TREE_CODE (ics2) == RVALUE_CONV
	  || TREE_CODE (ics2) == LVALUE_CONV)
	ics2 = TREE_OPERAND (ics2, 0);

      if (TREE_CODE (ics2) == USER_CONV
	  || TREE_CODE (ics2) == AMBIG_CONV
	  || TREE_CODE (ics2) == IDENTITY_CONV)
	/* At this point, ICS1 cannot be a proper subsequence of
	   ICS2.  We can get a USER_CONV when we are comparing the
	   second standard conversion sequence of two user conversion
	   sequences.  */
	return 0;

      ics2 = TREE_OPERAND (ics2, 0);

      if (TREE_CODE (ics2) == TREE_CODE (ics1)
	  && same_type_p (TREE_TYPE (ics2), TREE_TYPE (ics1))
	  && same_type_p (TREE_TYPE (TREE_OPERAND (ics2, 0)),
			     TREE_TYPE (TREE_OPERAND (ics1, 0))))
	return 1;
    }
}

/* Returns non-zero iff DERIVED is derived from BASE.  The inputs may
   be any _TYPE nodes.  */

int
is_properly_derived_from (derived, base)
     tree derived;
     tree base;
{
  if (!IS_AGGR_TYPE_CODE (TREE_CODE (derived))
      || !IS_AGGR_TYPE_CODE (TREE_CODE (base)))
    return 0;

  /* We only allow proper derivation here.  The DERIVED_FROM_P macro
     considers every class derived from itself.  */
  return (!same_type_p (TYPE_MAIN_VARIANT (derived),
			TYPE_MAIN_VARIANT (base))
	  && DERIVED_FROM_P (base, derived));
}

/* We build the ICS for an implicit object parameter as a pointer
   conversion sequence.  However, such a sequence should be compared
   as if it were a reference conversion sequence.  If ICS is the
   implicit conversion sequence for an implicit object parameter,
   modify it accordingly.  */

static void
maybe_handle_implicit_object (ics)
     tree* ics;
{
  if (ICS_THIS_FLAG (*ics))
    {
      /* [over.match.funcs]
	 
	 For non-static member functions, the type of the
	 implicit object parameter is "reference to cv X"
	 where X is the class of which the function is a
	 member and cv is the cv-qualification on the member
	 function declaration.  */
      tree t = *ics;
      tree reference_type;

      /* The `this' parameter is a pointer to a class type.  Make the
	 implict conversion talk about a reference to that same class
	 type.  */
      reference_type = TREE_TYPE (TREE_TYPE (*ics));
      reference_type = build_reference_type (reference_type);

      if (TREE_CODE (t) == QUAL_CONV)
	t = TREE_OPERAND (t, 0);
      if (TREE_CODE (t) == PTR_CONV)
	t = TREE_OPERAND (t, 0);
      t = build1 (IDENTITY_CONV, TREE_TYPE (TREE_TYPE (t)), NULL_TREE);
      t = direct_reference_binding (reference_type, t); 
      *ics = t;
    }
}

/* If ICS is a REF_BIND, modify it appropriately, set TARGET_TYPE
   to the type the reference originally referred to, and return 1.
   Otherwise, return 0.  */

static int
maybe_handle_ref_bind (ics, target_type)
     tree* ics;
     tree* target_type;
{
  if (TREE_CODE (*ics) == REF_BIND)
    {
      *target_type = TREE_TYPE (TREE_TYPE (*ics));
      *ics = TREE_OPERAND (*ics, 0);
      return 1;
    }
  
  return 0;
}

/* Compare two implicit conversion sequences according to the rules set out in
   [over.ics.rank].  Return values:

      1: ics1 is better than ics2
     -1: ics2 is better than ics1
      0: ics1 and ics2 are indistinguishable */

static int
compare_ics (ics1, ics2)
     tree ics1, ics2;
{
  tree from_type1;
  tree from_type2;
  tree to_type1;
  tree to_type2;
  tree deref_from_type1 = NULL_TREE;
  tree deref_from_type2 = NULL_TREE;
  tree deref_to_type1 = NULL_TREE;
  tree deref_to_type2 = NULL_TREE;

  /* REF_BINDING is non-zero if the result of the conversion sequence
     is a reference type.   In that case TARGET_TYPE is the
     type referred to by the reference.  */
  int ref_binding1;
  int ref_binding2;
  tree target_type1;
  tree target_type2;

  /* Handle implicit object parameters.  */
  maybe_handle_implicit_object (&ics1);
  maybe_handle_implicit_object (&ics2);

  /* Handle reference parameters.  */
  ref_binding1 = maybe_handle_ref_bind (&ics1, &target_type1);
  ref_binding2 = maybe_handle_ref_bind (&ics2, &target_type2);

  /* [over.ics.rank]

     When  comparing  the  basic forms of implicit conversion sequences (as
     defined in _over.best.ics_)

     --a standard conversion sequence (_over.ics.scs_) is a better
       conversion sequence than a user-defined conversion sequence
       or an ellipsis conversion sequence, and
     
     --a user-defined conversion sequence (_over.ics.user_) is a
       better conversion sequence than an ellipsis conversion sequence
       (_over.ics.ellipsis_).  */
  if (ICS_RANK (ics1) > ICS_RANK (ics2))
    return -1;
  else if (ICS_RANK (ics1) < ICS_RANK (ics2))
    return 1;

  if (ICS_RANK (ics1) == BAD_RANK)
    {
      /* Both ICS are bad.  We try to make a decision based on what
	 would have happenned if they'd been good.  */
      if (ICS_USER_FLAG (ics1) > ICS_USER_FLAG (ics2)
	  || ICS_STD_RANK (ics1) > ICS_STD_RANK (ics2))
	return -1;
      else if (ICS_USER_FLAG (ics1) < ICS_USER_FLAG (ics2)
	       || ICS_STD_RANK (ics1) < ICS_STD_RANK (ics2))
	return 1;

      /* We couldn't make up our minds; try to figure it out below.  */
    }

  if (ICS_ELLIPSIS_FLAG (ics1))
    /* Both conversions are ellipsis conversions.  */
    return 0;

  /* User-defined  conversion sequence U1 is a better conversion sequence
     than another user-defined conversion sequence U2 if they contain the
     same user-defined conversion operator or constructor and if the sec-
     ond standard conversion sequence of U1 is  better  than  the  second
     standard conversion sequence of U2.  */

  if (ICS_USER_FLAG (ics1))
    {
      tree t1, t2;

      for (t1 = ics1; TREE_CODE (t1) != USER_CONV; t1 = TREE_OPERAND (t1, 0))
	if (TREE_CODE (t1) == AMBIG_CONV)
	  return 0;
      for (t2 = ics2; TREE_CODE (t2) != USER_CONV; t2 = TREE_OPERAND (t2, 0))
	if (TREE_CODE (t2) == AMBIG_CONV)
	  return 0;

      if (USER_CONV_FN (t1) != USER_CONV_FN (t2))
	return 0;

      /* We can just fall through here, after setting up
	 FROM_TYPE1 and FROM_TYPE2.  */
      from_type1 = TREE_TYPE (t1);
      from_type2 = TREE_TYPE (t2);
    }
  else
    {
      /* We're dealing with two standard conversion sequences. 

	 [over.ics.rank]
	 
	 Standard conversion sequence S1 is a better conversion
	 sequence than standard conversion sequence S2 if
     
	 --S1 is a proper subsequence of S2 (comparing the conversion
	   sequences in the canonical form defined by _over.ics.scs_,
	   excluding any Lvalue Transformation; the identity
	   conversion sequence is considered to be a subsequence of
	   any non-identity conversion sequence */
      
      from_type1 = ics1;
      while (TREE_CODE (from_type1) != IDENTITY_CONV)
	from_type1 = TREE_OPERAND (from_type1, 0);
      from_type1 = TREE_TYPE (from_type1);
      
      from_type2 = ics2;
      while (TREE_CODE (from_type2) != IDENTITY_CONV)
	from_type2 = TREE_OPERAND (from_type2, 0);
      from_type2 = TREE_TYPE (from_type2);
    }

  if (same_type_p (from_type1, from_type2))
    {
      if (is_subseq (ics1, ics2))
	return 1;
      if (is_subseq (ics2, ics1))
	return -1;
    }
  /* Otherwise, one sequence cannot be a subsequence of the other; they
     don't start with the same type.  This can happen when comparing the
     second standard conversion sequence in two user-defined conversion
     sequences.  */

  /* [over.ics.rank]

     Or, if not that,

     --the rank of S1 is better than the rank of S2 (by the rules
       defined below):

    Standard conversion sequences are ordered by their ranks: an Exact
    Match is a better conversion than a Promotion, which is a better
    conversion than a Conversion.

    Two conversion sequences with the same rank are indistinguishable
    unless one of the following rules applies:

    --A conversion that is not a conversion of a pointer, or pointer
      to member, to bool is better than another conversion that is such
      a conversion.  

    The ICS_STD_RANK automatically handles the pointer-to-bool rule,
    so that we do not have to check it explicitly.  */
  if (ICS_STD_RANK (ics1) < ICS_STD_RANK (ics2))
    return 1;
  else if (ICS_STD_RANK (ics2) < ICS_STD_RANK (ics1))
    return -1;

  to_type1 = TREE_TYPE (ics1);
  to_type2 = TREE_TYPE (ics2);

  if (TYPE_PTR_P (from_type1)
      && TYPE_PTR_P (from_type2)
      && TYPE_PTR_P (to_type1)
      && TYPE_PTR_P (to_type2))
    {
      deref_from_type1 = TREE_TYPE (from_type1);
      deref_from_type2 = TREE_TYPE (from_type2);
      deref_to_type1 = TREE_TYPE (to_type1);
      deref_to_type2 = TREE_TYPE (to_type2);
    }
  /* The rules for pointers to members A::* are just like the rules
     for pointers A*, except opposite: if B is derived from A then
     A::* converts to B::*, not vice versa.  For that reason, we
     switch the from_ and to_ variables here.  */
  else if (TYPE_PTRMEM_P (from_type1)
	   && TYPE_PTRMEM_P (from_type2)
	   && TYPE_PTRMEM_P (to_type1)
	   && TYPE_PTRMEM_P (to_type2))
    {
      deref_to_type1 = TYPE_OFFSET_BASETYPE (TREE_TYPE (from_type1));
      deref_to_type2 = TYPE_OFFSET_BASETYPE (TREE_TYPE (from_type2));
      deref_from_type1 = TYPE_OFFSET_BASETYPE (TREE_TYPE (to_type1));
      deref_from_type2 = TYPE_OFFSET_BASETYPE (TREE_TYPE (to_type2));
    }
  else if (TYPE_PTRMEMFUNC_P (from_type1)
	   && TYPE_PTRMEMFUNC_P (from_type2)
	   && TYPE_PTRMEMFUNC_P (to_type1)
	   && TYPE_PTRMEMFUNC_P (to_type2))
    {
      deref_to_type1 = TYPE_PTRMEMFUNC_OBJECT_TYPE (from_type1);
      deref_to_type2 = TYPE_PTRMEMFUNC_OBJECT_TYPE (from_type2);
      deref_from_type1 = TYPE_PTRMEMFUNC_OBJECT_TYPE (to_type1);
      deref_from_type2 = TYPE_PTRMEMFUNC_OBJECT_TYPE (to_type2);
    }

  if (deref_from_type1 != NULL_TREE
      && IS_AGGR_TYPE_CODE (TREE_CODE (deref_from_type1))
      && IS_AGGR_TYPE_CODE (TREE_CODE (deref_from_type2)))
    {
      /* This was one of the pointer or pointer-like conversions.  

	 [over.ics.rank]
	 
	 --If class B is derived directly or indirectly from class A,
	   conversion of B* to A* is better than conversion of B* to
	   void*, and conversion of A* to void* is better than
	   conversion of B* to void*.  */
      if (TREE_CODE (deref_to_type1) == VOID_TYPE
	  && TREE_CODE (deref_to_type2) == VOID_TYPE)
	{
	  if (is_properly_derived_from (deref_from_type1,
					deref_from_type2))
	    return -1;
	  else if (is_properly_derived_from (deref_from_type2,
					     deref_from_type1))
	    return 1;
	}
      else if (TREE_CODE (deref_to_type1) == VOID_TYPE
	       || TREE_CODE (deref_to_type2) == VOID_TYPE)
	{
	  if (same_type_p (deref_from_type1, deref_from_type2))
	    {
	      if (TREE_CODE (deref_to_type2) == VOID_TYPE)
		{
		  if (is_properly_derived_from (deref_from_type1,
						deref_to_type1))
		    return 1;
		}
	      /* We know that DEREF_TO_TYPE1 is `void' here.  */
	      else if (is_properly_derived_from (deref_from_type1,
						 deref_to_type2))
		return -1;
	    }
	}
      else if (IS_AGGR_TYPE_CODE (TREE_CODE (deref_to_type1))
	       && IS_AGGR_TYPE_CODE (TREE_CODE (deref_to_type2)))
	{
	  /* [over.ics.rank]

	     --If class B is derived directly or indirectly from class A
	       and class C is derived directly or indirectly from B,
	     
	     --conversion of C* to B* is better than conversion of C* to
	       A*, 
	     
	     --conversion of B* to A* is better than conversion of C* to
	       A*  */
	  if (same_type_p (deref_from_type1, deref_from_type2))
	    {
	      if (is_properly_derived_from (deref_to_type1,
					    deref_to_type2))
		return 1;
	      else if (is_properly_derived_from (deref_to_type2,
						 deref_to_type1))
		return -1;
	    }
	  else if (same_type_p (deref_to_type1, deref_to_type2))
	    {
	      if (is_properly_derived_from (deref_from_type2,
					    deref_from_type1))
		return 1;
	      else if (is_properly_derived_from (deref_from_type1,
						 deref_from_type2))
		return -1;
	    }
	}
    }
  else if (IS_AGGR_TYPE_CODE (TREE_CODE (from_type1))
	   && same_type_p (from_type1, from_type2))
    {
      /* [over.ics.rank]
	 
	 --binding of an expression of type C to a reference of type
	   B& is better than binding an expression of type C to a
	   reference of type A&

	 --conversion of C to B is better than conversion of C to A,  */
      if (is_properly_derived_from (from_type1, to_type1)
	  && is_properly_derived_from (from_type1, to_type2))
	{
	  if (is_properly_derived_from (to_type1, to_type2))
	    return 1;
	  else if (is_properly_derived_from (to_type2, to_type1))
	    return -1;
	}
    }
  else if (IS_AGGR_TYPE_CODE (TREE_CODE (to_type1))
	   && same_type_p (to_type1, to_type2))
    {
      /* [over.ics.rank]

	 --binding of an expression of type B to a reference of type
	   A& is better than binding an expression of type C to a
	   reference of type A&, 

	 --onversion of B to A is better than conversion of C to A  */
      if (is_properly_derived_from (from_type1, to_type1)
	  && is_properly_derived_from (from_type2, to_type1))
	{
	  if (is_properly_derived_from (from_type2, from_type1))
	    return 1;
	  else if (is_properly_derived_from (from_type1, from_type2))
	    return -1;
	}
    }

  /* [over.ics.rank]

     --S1 and S2 differ only in their qualification conversion and  yield
       similar  types  T1 and T2 (_conv.qual_), respectively, and the cv-
       qualification signature of type T1 is a proper subset of  the  cv-
       qualification signature of type T2  */
  if (TREE_CODE (ics1) == QUAL_CONV 
      && TREE_CODE (ics2) == QUAL_CONV
      && same_type_p (from_type1, from_type2))
    return comp_cv_qual_signature (to_type1, to_type2);

  /* [over.ics.rank]
     
     --S1 and S2 are reference bindings (_dcl.init.ref_), and the
     types to which the references refer are the same type except for
     top-level cv-qualifiers, and the type to which the reference
     initialized by S2 refers is more cv-qualified than the type to
     which the reference initialized by S1 refers */
      
  if (ref_binding1 && ref_binding2
      && same_type_p (TYPE_MAIN_VARIANT (to_type1),
		      TYPE_MAIN_VARIANT (to_type2)))
    return comp_cv_qualification (target_type2, target_type1);

  /* Neither conversion sequence is better than the other.  */
  return 0;
}

/* The source type for this standard conversion sequence.  */

static tree
source_type (t)
     tree t;
{
  for (;; t = TREE_OPERAND (t, 0))
    {
      if (TREE_CODE (t) == USER_CONV
	  || TREE_CODE (t) == AMBIG_CONV
	  || TREE_CODE (t) == IDENTITY_CONV)
	return TREE_TYPE (t);
    }
  my_friendly_abort (1823);
}

/* Note a warning about preferring WINNER to LOSER.  We do this by storing
   a pointer to LOSER and re-running joust to produce the warning if WINNER
   is actually used.  */

static void
add_warning (winner, loser)
     struct z_candidate *winner, *loser;
{
  winner->warnings = tree_cons (NULL_PTR,
				build_expr_ptr_wrapper (loser),
				winner->warnings);
}

/* Returns true iff functions are equivalent. Equivalent functions are
   not identical only if one is a function-local extern function.  */

static inline int
equal_functions (fn1, fn2)
     tree fn1;
     tree fn2;
{
  if (DECL_LOCAL_FUNCTION_P (fn1) || DECL_LOCAL_FUNCTION_P (fn2))
    return decls_match (fn1, fn2);
  return fn1 == fn2;
}

/* Compare two candidates for overloading as described in
   [over.match.best].  Return values:

      1: cand1 is better than cand2
     -1: cand2 is better than cand1
      0: cand1 and cand2 are indistinguishable */

static int
joust (cand1, cand2, warn)
     struct z_candidate *cand1, *cand2;
     int warn;
{
  int winner = 0;
  int i, off1 = 0, off2 = 0, len;

  /* Candidates that involve bad conversions are always worse than those
     that don't.  */
  if (cand1->viable > cand2->viable)
    return 1;
  if (cand1->viable < cand2->viable)
    return -1;

  /* If we have two pseudo-candidates for conversions to the same type,
     arbitrarily pick one.  */
  if (TYPE_P (cand1->fn) && cand1->fn == cand2->fn)
    return 1;

  /* a viable function F1
     is defined to be a better function than another viable function F2  if
     for  all arguments i, ICSi(F1) is not a worse conversion sequence than
     ICSi(F2), and then */

  /* for some argument j, ICSj(F1) is a better conversion  sequence  than
     ICSj(F2) */

  /* For comparing static and non-static member functions, we ignore
     the implicit object parameter of the non-static function.  The
     standard says to pretend that the static function has an object
     parm, but that won't work with operator overloading.  */
  len = TREE_VEC_LENGTH (cand1->convs);
  if (len != TREE_VEC_LENGTH (cand2->convs))
    {
      if (DECL_STATIC_FUNCTION_P (cand1->fn)
	  && ! DECL_STATIC_FUNCTION_P (cand2->fn))
	off2 = 1;
      else if (! DECL_STATIC_FUNCTION_P (cand1->fn)
	       && DECL_STATIC_FUNCTION_P (cand2->fn))
	{
	  off1 = 1;
	  --len;
	}
      else
	my_friendly_abort (42);
    }

  for (i = 0; i < len; ++i)
    {
      tree t1 = TREE_VEC_ELT (cand1->convs, i+off1);
      tree t2 = TREE_VEC_ELT (cand2->convs, i+off2);
      int comp = compare_ics (t1, t2);

      if (comp != 0)
	{
	  if (warn_sign_promo
	      && ICS_RANK (t1) + ICS_RANK (t2) == STD_RANK + PROMO_RANK
	      && TREE_CODE (t1) == STD_CONV
	      && TREE_CODE (t2) == STD_CONV
	      && TREE_CODE (TREE_TYPE (t1)) == INTEGER_TYPE
	      && TREE_CODE (TREE_TYPE (t2)) == INTEGER_TYPE
	      && (TYPE_PRECISION (TREE_TYPE (t1))
		  == TYPE_PRECISION (TREE_TYPE (t2)))
	      && (TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (t1, 0)))
		  || (TREE_CODE (TREE_TYPE (TREE_OPERAND (t1, 0)))
		      == ENUMERAL_TYPE)))
	    {
	      tree type = TREE_TYPE (TREE_OPERAND (t1, 0));
	      tree type1, type2;
	      struct z_candidate *w, *l;
	      if (comp > 0)
		type1 = TREE_TYPE (t1), type2 = TREE_TYPE (t2),
		  w = cand1, l = cand2;
	      else
		type1 = TREE_TYPE (t2), type2 = TREE_TYPE (t1),
		  w = cand2, l = cand1;

	      if (warn)
		{
		  cp_warning ("passing `%T' chooses `%T' over `%T'",
			      type, type1, type2);
		  cp_warning ("  in call to `%D'", w->fn);
		}
	      else
		add_warning (w, l);
	    }

	  if (winner && comp != winner)
	    {
	      winner = 0;
	      goto tweak;
	    }
	  winner = comp;
	}
    }

  /* warn about confusing overload resolution for user-defined conversions,
     either between a constructor and a conversion op, or between two
     conversion ops.  */
  if (winner && cand1->second_conv
      && ((DECL_CONSTRUCTOR_P (cand1->fn)
	   != DECL_CONSTRUCTOR_P (cand2->fn))
	  /* Don't warn if the two conv ops convert to the same type...  */
	  || (! DECL_CONSTRUCTOR_P (cand1->fn)
	      && ! same_type_p (TREE_TYPE (TREE_TYPE (cand1->fn)),
				TREE_TYPE (TREE_TYPE (cand2->fn))))))
    {
      int comp = compare_ics (cand1->second_conv, cand2->second_conv);
      if (comp != winner)
	{
	  struct z_candidate *w, *l;
	  if (winner == 1)
	    w = cand1, l = cand2;
	  else
	    w = cand2, l = cand1;
	  if (warn)
	    {
	      tree source = source_type (TREE_VEC_ELT (w->convs, 0));
	      if (! DECL_CONSTRUCTOR_P (w->fn))
		source = TREE_TYPE (source);
	      cp_warning ("choosing `%D' over `%D'", w->fn, l->fn);
	      cp_warning ("  for conversion from `%T' to `%T'",
			  source, TREE_TYPE (w->second_conv));
	      cp_warning ("  because conversion sequence for the argument is better");
	    }
	  else
	    add_warning (w, l);
	}
    }

  if (winner)
    return winner;

  /* or, if not that,
     F1 is a non-template function and F2 is a template function */

  if (! cand1->template && cand2->template)
    return 1;
  else if (cand1->template && ! cand2->template)
    return -1;
  else if (cand1->template && cand2->template)
    winner = more_specialized
      (TI_TEMPLATE (cand1->template), TI_TEMPLATE (cand2->template),
       NULL_TREE);

  /* or, if not that,
     the  context  is  an  initialization by user-defined conversion (see
     _dcl.init_  and  _over.match.user_)  and  the  standard   conversion
     sequence  from  the return type of F1 to the destination type (i.e.,
     the type of the entity being initialized)  is  a  better  conversion
     sequence  than the standard conversion sequence from the return type
     of F2 to the destination type.  */

  if (! winner && cand1->second_conv)
    winner = compare_ics (cand1->second_conv, cand2->second_conv);

  /* If the built-in candidates are the same, arbitrarily pick one.  */
  if (! winner && cand1->fn == cand2->fn
      && TREE_CODE (cand1->fn) == IDENTIFIER_NODE)
    {
      for (i = 0; i < len; ++i)
	if (!same_type_p (TREE_TYPE (TREE_VEC_ELT (cand1->convs, i)),
			  TREE_TYPE (TREE_VEC_ELT (cand2->convs, i))))
	  break;
      if (i == TREE_VEC_LENGTH (cand1->convs))
	return 1;

      /* Kludge around broken overloading rules whereby
	 Integer a, b; test ? a : b; is ambiguous, since there's a builtin
	 that takes references and another that takes values.  */
      if (cand1->fn == ansi_opname[COND_EXPR])
	{
	  tree c1 = TREE_VEC_ELT (cand1->convs, 1);
	  tree c2 = TREE_VEC_ELT (cand2->convs, 1);
	  tree t1 = strip_top_quals (non_reference (TREE_TYPE (c1)));
	  tree t2 = strip_top_quals (non_reference (TREE_TYPE (c2)));

	  if (same_type_p (t1, t2))
	    {
	      if (TREE_CODE (c1) == REF_BIND && TREE_CODE (c2) != REF_BIND)
		return 1;
	      if (TREE_CODE (c1) != REF_BIND && TREE_CODE (c2) == REF_BIND)
		return -1;
	    }
	}
    }

  /* If the two functions are the same (this can happen with declarations
     in multiple scopes and arg-dependent lookup), arbitrarily choose one.  */
  if (DECL_P (cand1->fn) && DECL_P (cand2->fn)
      && equal_functions (cand1->fn, cand2->fn))
    return 1;

tweak:

  /* Extension: If the worst conversion for one candidate is worse than the
     worst conversion for the other, take the first.  */
  if (! winner && ! pedantic)
    {
      int rank1 = IDENTITY_RANK, rank2 = IDENTITY_RANK;

      for (i = 0; i < len; ++i)
	{
	  if (ICS_RANK (TREE_VEC_ELT (cand1->convs, i+off1)) > rank1)
	    rank1 = ICS_RANK (TREE_VEC_ELT (cand1->convs, i+off1));
	  if (ICS_RANK (TREE_VEC_ELT (cand2->convs, i+off2)) > rank2)
	    rank2 = ICS_RANK (TREE_VEC_ELT (cand2->convs, i+off2));
	}

      if (rank1 < rank2)
	return 1;
      if (rank1 > rank2)
	return -1;
    }

  return winner;
}

/* Given a list of candidates for overloading, find the best one, if any.
   This algorithm has a worst case of O(2n) (winner is last), and a best
   case of O(n/2) (totally ambiguous); much better than a sorting
   algorithm.  */

static struct z_candidate *
tourney (candidates)
     struct z_candidate *candidates;
{
  struct z_candidate *champ = candidates, *challenger;
  int fate;
  int champ_compared_to_predecessor = 0;

  /* Walk through the list once, comparing each current champ to the next
     candidate, knocking out a candidate or two with each comparison.  */

  for (challenger = champ->next; challenger; )
    {
      fate = joust (champ, challenger, 0);
      if (fate == 1)
	challenger = challenger->next;
      else
	{
	  if (fate == 0)
	    {
	      champ = challenger->next;
	      if (champ == 0)
		return 0;
	      champ_compared_to_predecessor = 0;
	    }
	  else
	    {
	      champ = challenger;
	      champ_compared_to_predecessor = 1;
	    }

	  challenger = champ->next;
	}
    }

  /* Make sure the champ is better than all the candidates it hasn't yet
     been compared to.  */

  for (challenger = candidates; 
       challenger != champ 
	 && !(champ_compared_to_predecessor && challenger->next == champ);
       challenger = challenger->next)
    {
      fate = joust (champ, challenger, 0);
      if (fate != 1)
	return 0;
    }

  return champ;
}

/* Returns non-zero if things of type FROM can be converted to TO.  */

int
can_convert (to, from)
     tree to, from;
{
  return can_convert_arg (to, from, NULL_TREE);
}

/* Returns non-zero if ARG (of type FROM) can be converted to TO.  */

int
can_convert_arg (to, from, arg)
     tree to, from, arg;
{
  tree t = implicit_conversion (to, from, arg, LOOKUP_NORMAL);
  return (t && ! ICS_BAD_FLAG (t));
}

/* Convert EXPR to TYPE.  Return the converted expression.  */

tree
perform_implicit_conversion (type, expr)
     tree type;
     tree expr;
{
  tree conv;
  
  if (expr == error_mark_node)
    return error_mark_node;
  conv = implicit_conversion (type, TREE_TYPE (expr), expr,
			      LOOKUP_NORMAL);
  if (!conv || ICS_BAD_FLAG (conv))
    {
      cp_error ("could not convert `%E' to `%T'", expr, type);
      return error_mark_node;
    }

  return convert_like (conv, expr);
}

/* Convert EXPR to the indicated reference TYPE, in a way suitable for
   initializing a variable of that TYPE.  Return the converted
   expression.  */

tree
initialize_reference (type, expr)
     tree type;
     tree expr;
{
  tree conv;

  conv = reference_binding (type, TREE_TYPE (expr), expr, LOOKUP_NORMAL);
  if (!conv || ICS_BAD_FLAG (conv))
    {
      cp_error ("could not convert `%E' to `%T'", expr, type);
      return error_mark_node;
    }

  return convert_like (conv, expr);
}
