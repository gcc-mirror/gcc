/* Functions related to invoking methods and overloaded functions.
   Copyright (C) 1987-2014 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com) and
   modified by Brendan Kehoe (brendan@cygnus.com).

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


/* High-level class interface.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "stor-layout.h"
#include "trans-mem.h"
#include "stringpool.h"
#include "cp-tree.h"
#include "flags.h"
#include "toplev.h"
#include "diagnostic-core.h"
#include "intl.h"
#include "target.h"
#include "convert.h"
#include "langhooks.h"
#include "c-family/c-objc.h"
#include "timevar.h"
#include "cgraph.h"
#include "wide-int.h"

/* The various kinds of conversion.  */

typedef enum conversion_kind {
  ck_identity,
  ck_lvalue,
  ck_qual,
  ck_std,
  ck_ptr,
  ck_pmem,
  ck_base,
  ck_ref_bind,
  ck_user,
  ck_ambig,
  ck_list,
  ck_aggr,
  ck_rvalue
} conversion_kind;

/* The rank of the conversion.  Order of the enumerals matters; better
   conversions should come earlier in the list.  */

typedef enum conversion_rank {
  cr_identity,
  cr_exact,
  cr_promotion,
  cr_std,
  cr_pbool,
  cr_user,
  cr_ellipsis,
  cr_bad
} conversion_rank;

/* An implicit conversion sequence, in the sense of [over.best.ics].
   The first conversion to be performed is at the end of the chain.
   That conversion is always a cr_identity conversion.  */

typedef struct conversion conversion;
struct conversion {
  /* The kind of conversion represented by this step.  */
  conversion_kind kind;
  /* The rank of this conversion.  */
  conversion_rank rank;
  BOOL_BITFIELD user_conv_p : 1;
  BOOL_BITFIELD ellipsis_p : 1;
  BOOL_BITFIELD this_p : 1;
  /* True if this conversion would be permitted with a bending of
     language standards, e.g. disregarding pointer qualifiers or
     converting integers to pointers.  */
  BOOL_BITFIELD bad_p : 1;
  /* If KIND is ck_ref_bind ck_base_conv, true to indicate that a
     temporary should be created to hold the result of the
     conversion.  */
  BOOL_BITFIELD need_temporary_p : 1;
  /* If KIND is ck_ptr or ck_pmem, true to indicate that a conversion
     from a pointer-to-derived to pointer-to-base is being performed.  */
  BOOL_BITFIELD base_p : 1;
  /* If KIND is ck_ref_bind, true when either an lvalue reference is
     being bound to an lvalue expression or an rvalue reference is
     being bound to an rvalue expression.  If KIND is ck_rvalue,
     true when we should treat an lvalue as an rvalue (12.8p33).  If
     KIND is ck_base, always false.  */
  BOOL_BITFIELD rvaluedness_matches_p: 1;
  BOOL_BITFIELD check_narrowing: 1;
  /* The type of the expression resulting from the conversion.  */
  tree type;
  union {
    /* The next conversion in the chain.  Since the conversions are
       arranged from outermost to innermost, the NEXT conversion will
       actually be performed before this conversion.  This variant is
       used only when KIND is neither ck_identity, ck_ambig nor
       ck_list.  Please use the next_conversion function instead
       of using this field directly.  */
    conversion *next;
    /* The expression at the beginning of the conversion chain.  This
       variant is used only if KIND is ck_identity or ck_ambig.  */
    tree expr;
    /* The array of conversions for an initializer_list, so this
       variant is used only when KIN D is ck_list.  */
    conversion **list;
  } u;
  /* The function candidate corresponding to this conversion
     sequence.  This field is only used if KIND is ck_user.  */
  struct z_candidate *cand;
};

#define CONVERSION_RANK(NODE)			\
  ((NODE)->bad_p ? cr_bad			\
   : (NODE)->ellipsis_p ? cr_ellipsis		\
   : (NODE)->user_conv_p ? cr_user		\
   : (NODE)->rank)

#define BAD_CONVERSION_RANK(NODE)		\
  ((NODE)->ellipsis_p ? cr_ellipsis		\
   : (NODE)->user_conv_p ? cr_user		\
   : (NODE)->rank)

static struct obstack conversion_obstack;
static bool conversion_obstack_initialized;
struct rejection_reason;

static struct z_candidate * tourney (struct z_candidate *, tsubst_flags_t);
static int equal_functions (tree, tree);
static int joust (struct z_candidate *, struct z_candidate *, bool,
		  tsubst_flags_t);
static int compare_ics (conversion *, conversion *);
static tree build_over_call (struct z_candidate *, int, tsubst_flags_t);
static tree build_java_interface_fn_ref (tree, tree);
#define convert_like(CONV, EXPR, COMPLAIN)			\
  convert_like_real ((CONV), (EXPR), NULL_TREE, 0, 0,		\
		     /*issue_conversion_warnings=*/true,	\
		     /*c_cast_p=*/false, (COMPLAIN))
#define convert_like_with_context(CONV, EXPR, FN, ARGNO, COMPLAIN )	\
  convert_like_real ((CONV), (EXPR), (FN), (ARGNO), 0,			\
		     /*issue_conversion_warnings=*/true,		\
		     /*c_cast_p=*/false, (COMPLAIN))
static tree convert_like_real (conversion *, tree, tree, int, int, bool,
			       bool, tsubst_flags_t);
static void op_error (location_t, enum tree_code, enum tree_code, tree,
		      tree, tree, bool);
static struct z_candidate *build_user_type_conversion_1 (tree, tree, int,
							 tsubst_flags_t);
static void print_z_candidate (location_t, const char *, struct z_candidate *);
static void print_z_candidates (location_t, struct z_candidate *);
static tree build_this (tree);
static struct z_candidate *splice_viable (struct z_candidate *, bool, bool *);
static bool any_strictly_viable (struct z_candidate *);
static struct z_candidate *add_template_candidate
	(struct z_candidate **, tree, tree, tree, tree, const vec<tree, va_gc> *,
	 tree, tree, tree, int, unification_kind_t, tsubst_flags_t);
static struct z_candidate *add_template_candidate_real
	(struct z_candidate **, tree, tree, tree, tree, const vec<tree, va_gc> *,
	 tree, tree, tree, int, tree, unification_kind_t, tsubst_flags_t);
static struct z_candidate *add_template_conv_candidate
	(struct z_candidate **, tree, tree, tree, const vec<tree, va_gc> *,
	 tree, tree, tree, tsubst_flags_t);
static void add_builtin_candidates
	(struct z_candidate **, enum tree_code, enum tree_code,
	 tree, tree *, int, tsubst_flags_t);
static void add_builtin_candidate
	(struct z_candidate **, enum tree_code, enum tree_code,
	 tree, tree, tree, tree *, tree *, int, tsubst_flags_t);
static bool is_complete (tree);
static void build_builtin_candidate
	(struct z_candidate **, tree, tree, tree, tree *, tree *,
	 int, tsubst_flags_t);
static struct z_candidate *add_conv_candidate
	(struct z_candidate **, tree, tree, tree, const vec<tree, va_gc> *, tree,
	 tree, tsubst_flags_t);
static struct z_candidate *add_function_candidate
	(struct z_candidate **, tree, tree, tree, const vec<tree, va_gc> *, tree,
	 tree, int, tsubst_flags_t);
static conversion *implicit_conversion (tree, tree, tree, bool, int,
					tsubst_flags_t);
static conversion *standard_conversion (tree, tree, tree, bool, int);
static conversion *reference_binding (tree, tree, tree, bool, int,
				      tsubst_flags_t);
static conversion *build_conv (conversion_kind, tree, conversion *);
static conversion *build_list_conv (tree, tree, int, tsubst_flags_t);
static conversion *next_conversion (conversion *);
static bool is_subseq (conversion *, conversion *);
static conversion *maybe_handle_ref_bind (conversion **);
static void maybe_handle_implicit_object (conversion **);
static struct z_candidate *add_candidate
	(struct z_candidate **, tree, tree, const vec<tree, va_gc> *, size_t,
	 conversion **, tree, tree, int, struct rejection_reason *, int);
static tree source_type (conversion *);
static void add_warning (struct z_candidate *, struct z_candidate *);
static bool reference_compatible_p (tree, tree);
static conversion *direct_reference_binding (tree, conversion *);
static bool promoted_arithmetic_type_p (tree);
static conversion *conditional_conversion (tree, tree, tsubst_flags_t);
static char *name_as_c_string (tree, tree, bool *);
static tree prep_operand (tree);
static void add_candidates (tree, tree, const vec<tree, va_gc> *, tree, tree,
			    bool, tree, tree, int, struct z_candidate **,
			    tsubst_flags_t);
static conversion *merge_conversion_sequences (conversion *, conversion *);
static tree build_temp (tree, tree, int, diagnostic_t *, tsubst_flags_t);

/* Returns nonzero iff the destructor name specified in NAME matches BASETYPE.
   NAME can take many forms...  */

bool
check_dtor_name (tree basetype, tree name)
{
  /* Just accept something we've already complained about.  */
  if (name == error_mark_node)
    return true;

  if (TREE_CODE (name) == TYPE_DECL)
    name = TREE_TYPE (name);
  else if (TYPE_P (name))
    /* OK */;
  else if (identifier_p (name))
    {
      if ((MAYBE_CLASS_TYPE_P (basetype)
	   && name == constructor_name (basetype))
	  || (TREE_CODE (basetype) == ENUMERAL_TYPE
	      && name == TYPE_IDENTIFIER (basetype)))
	return true;
      else
	name = get_type_value (name);
    }
  else
    {
      /* In the case of:

	 template <class T> struct S { ~S(); };
	 int i;
	 i.~S();

	 NAME will be a class template.  */
      gcc_assert (DECL_CLASS_TEMPLATE_P (name));
      return false;
    }

  if (!name || name == error_mark_node)
    return false;
  return same_type_p (TYPE_MAIN_VARIANT (basetype), TYPE_MAIN_VARIANT (name));
}

/* We want the address of a function or method.  We avoid creating a
   pointer-to-member function.  */

tree
build_addr_func (tree function, tsubst_flags_t complain)
{
  tree type = TREE_TYPE (function);

  /* We have to do these by hand to avoid real pointer to member
     functions.  */
  if (TREE_CODE (type) == METHOD_TYPE)
    {
      if (TREE_CODE (function) == OFFSET_REF)
	{
	  tree object = build_address (TREE_OPERAND (function, 0));
	  return get_member_function_from_ptrfunc (&object,
						   TREE_OPERAND (function, 1),
						   complain);
	}
      function = build_address (function);
    }
  else
    function = decay_conversion (function, complain);

  return function;
}

/* Build a CALL_EXPR, we can handle FUNCTION_TYPEs, METHOD_TYPEs, or
   POINTER_TYPE to those.  Note, pointer to member function types
   (TYPE_PTRMEMFUNC_P) must be handled by our callers.  There are
   two variants.  build_call_a is the primitive taking an array of
   arguments, while build_call_n is a wrapper that handles varargs.  */

tree
build_call_n (tree function, int n, ...)
{
  if (n == 0)
    return build_call_a (function, 0, NULL);
  else
    {
      tree *argarray = XALLOCAVEC (tree, n);
      va_list ap;
      int i;

      va_start (ap, n);
      for (i = 0; i < n; i++)
	argarray[i] = va_arg (ap, tree);
      va_end (ap);
      return build_call_a (function, n, argarray);
    }
}

/* Update various flags in cfun and the call itself based on what is being
   called.  Split out of build_call_a so that bot_manip can use it too.  */

void
set_flags_from_callee (tree call)
{
  int nothrow;
  tree decl = get_callee_fndecl (call);

  /* We check both the decl and the type; a function may be known not to
     throw without being declared throw().  */
  nothrow = ((decl && TREE_NOTHROW (decl))
	     || TYPE_NOTHROW_P (TREE_TYPE (TREE_TYPE (CALL_EXPR_FN (call)))));

  if (!nothrow && at_function_scope_p () && cfun && cp_function_chain)
    cp_function_chain->can_throw = 1;

  if (decl && TREE_THIS_VOLATILE (decl) && cfun && cp_function_chain)
    current_function_returns_abnormally = 1;

  TREE_NOTHROW (call) = nothrow;
}

tree
build_call_a (tree function, int n, tree *argarray)
{
  tree decl;
  tree result_type;
  tree fntype;
  int i;

  function = build_addr_func (function, tf_warning_or_error);

  gcc_assert (TYPE_PTR_P (TREE_TYPE (function)));
  fntype = TREE_TYPE (TREE_TYPE (function));
  gcc_assert (TREE_CODE (fntype) == FUNCTION_TYPE
	      || TREE_CODE (fntype) == METHOD_TYPE);
  result_type = TREE_TYPE (fntype);
  /* An rvalue has no cv-qualifiers.  */
  if (SCALAR_TYPE_P (result_type) || VOID_TYPE_P (result_type))
    result_type = cv_unqualified (result_type);

  function = build_call_array_loc (input_location,
				   result_type, function, n, argarray);
  set_flags_from_callee (function);

  decl = get_callee_fndecl (function);

  if (decl && !TREE_USED (decl))
    {
      /* We invoke build_call directly for several library
	 functions.  These may have been declared normally if
	 we're building libgcc, so we can't just check
	 DECL_ARTIFICIAL.  */
      gcc_assert (DECL_ARTIFICIAL (decl)
		  || !strncmp (IDENTIFIER_POINTER (DECL_NAME (decl)),
			       "__", 2));
      mark_used (decl);
    }

  if (decl && TREE_DEPRECATED (decl))
    warn_deprecated_use (decl, NULL_TREE);
  require_complete_eh_spec_types (fntype, decl);

  TREE_HAS_CONSTRUCTOR (function) = (decl && DECL_CONSTRUCTOR_P (decl));

  /* Don't pass empty class objects by value.  This is useful
     for tags in STL, which are used to control overload resolution.
     We don't need to handle other cases of copying empty classes.  */
  if (! decl || ! DECL_BUILT_IN (decl))
    for (i = 0; i < n; i++)
      {
	tree arg = CALL_EXPR_ARG (function, i);
	if (is_empty_class (TREE_TYPE (arg))
	    && ! TREE_ADDRESSABLE (TREE_TYPE (arg)))
	  {
	    tree t = build0 (EMPTY_CLASS_EXPR, TREE_TYPE (arg));
	    arg = build2 (COMPOUND_EXPR, TREE_TYPE (t), arg, t);
	    CALL_EXPR_ARG (function, i) = arg;
	  }
      }

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
   information to get protected accesses correct.

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

/* New overloading code.  */

typedef struct z_candidate z_candidate;

typedef struct candidate_warning candidate_warning;
struct candidate_warning {
  z_candidate *loser;
  candidate_warning *next;
};

/* Information for providing diagnostics about why overloading failed.  */

enum rejection_reason_code {
  rr_none,
  rr_arity,
  rr_explicit_conversion,
  rr_template_conversion,
  rr_arg_conversion,
  rr_bad_arg_conversion,
  rr_template_unification,
  rr_invalid_copy
};

struct conversion_info {
  /* The index of the argument, 0-based.  */
  int n_arg;
  /* The actual argument or its type.  */
  tree from;
  /* The type of the parameter.  */
  tree to_type;
};
  
struct rejection_reason {
  enum rejection_reason_code code;
  union {
    /* Information about an arity mismatch.  */
    struct {
      /* The expected number of arguments.  */
      int expected;
      /* The actual number of arguments in the call.  */
      int actual;
      /* Whether the call was a varargs call.  */
      bool call_varargs_p;
    } arity;
    /* Information about an argument conversion mismatch.  */
    struct conversion_info conversion;
    /* Same, but for bad argument conversions.  */
    struct conversion_info bad_conversion;
    /* Information about template unification failures.  These are the
       parameters passed to fn_type_unification.  */
    struct {
      tree tmpl;
      tree explicit_targs;
      int num_targs;
      const tree *args;
      unsigned int nargs;
      tree return_type;
      unification_kind_t strict;
      int flags;
    } template_unification;
    /* Information about template instantiation failures.  These are the
       parameters passed to instantiate_template.  */
    struct {
      tree tmpl;
      tree targs;
    } template_instantiation;
  } u;
};

struct z_candidate {
  /* The FUNCTION_DECL that will be called if this candidate is
     selected by overload resolution.  */
  tree fn;
  /* If not NULL_TREE, the first argument to use when calling this
     function.  */
  tree first_arg;
  /* The rest of the arguments to use when calling this function.  If
     there are no further arguments this may be NULL or it may be an
     empty vector.  */
  const vec<tree, va_gc> *args;
  /* The implicit conversion sequences for each of the arguments to
     FN.  */
  conversion **convs;
  /* The number of implicit conversion sequences.  */
  size_t num_convs;
  /* If FN is a user-defined conversion, the standard conversion
     sequence from the type returned by FN to the desired destination
     type.  */
  conversion *second_conv;
  struct rejection_reason *reason;
  /* If FN is a member function, the binfo indicating the path used to
     qualify the name of FN at the call site.  This path is used to
     determine whether or not FN is accessible if it is selected by
     overload resolution.  The DECL_CONTEXT of FN will always be a
     (possibly improper) base of this binfo.  */
  tree access_path;
  /* If FN is a non-static member function, the binfo indicating the
     subobject to which the `this' pointer should be converted if FN
     is selected by overload resolution.  The type pointed to by
     the `this' pointer must correspond to the most derived class
     indicated by the CONVERSION_PATH.  */
  tree conversion_path;
  tree template_decl;
  tree explicit_targs;
  candidate_warning *warnings;
  z_candidate *next;
  int viable;

  /* The flags active in add_candidate.  */
  int flags;
};

/* Returns true iff T is a null pointer constant in the sense of
   [conv.ptr].  */

bool
null_ptr_cst_p (tree t)
{
  /* [conv.ptr]

     A null pointer constant is an integral constant expression
     (_expr.const_) rvalue of integer type that evaluates to zero or
     an rvalue of type std::nullptr_t. */
  if (NULLPTR_TYPE_P (TREE_TYPE (t)))
    return true;
  if (CP_INTEGRAL_TYPE_P (TREE_TYPE (t)))
    {
      /* Core issue 903 says only literal 0 is a null pointer constant.  */
      if (cxx_dialect < cxx11)
	t = maybe_constant_value (fold_non_dependent_expr_sfinae (t, tf_none));
      STRIP_NOPS (t);
      if (integer_zerop (t) && !TREE_OVERFLOW (t))
	return true;
    }
  return false;
}

/* Returns true iff T is a null member pointer value (4.11).  */

bool
null_member_pointer_value_p (tree t)
{
  tree type = TREE_TYPE (t);
  if (!type)
    return false;
  else if (TYPE_PTRMEMFUNC_P (type))
    return (TREE_CODE (t) == CONSTRUCTOR
	    && integer_zerop (CONSTRUCTOR_ELT (t, 0)->value));
  else if (TYPE_PTRDATAMEM_P (type))
    return integer_all_onesp (t);
  else
    return false;
}

/* Returns nonzero if PARMLIST consists of only default parms,
   ellipsis, and/or undeduced parameter packs.  */

bool
sufficient_parms_p (const_tree parmlist)
{
  for (; parmlist && parmlist != void_list_node;
       parmlist = TREE_CHAIN (parmlist))
    if (!TREE_PURPOSE (parmlist)
	&& !PACK_EXPANSION_P (TREE_VALUE (parmlist)))
      return false;
  return true;
}

/* Allocate N bytes of memory from the conversion obstack.  The memory
   is zeroed before being returned.  */

static void *
conversion_obstack_alloc (size_t n)
{
  void *p;
  if (!conversion_obstack_initialized)
    {
      gcc_obstack_init (&conversion_obstack);
      conversion_obstack_initialized = true;
    }
  p = obstack_alloc (&conversion_obstack, n);
  memset (p, 0, n);
  return p;
}

/* Allocate rejection reasons.  */

static struct rejection_reason *
alloc_rejection (enum rejection_reason_code code)
{
  struct rejection_reason *p;
  p = (struct rejection_reason *) conversion_obstack_alloc (sizeof *p);
  p->code = code;
  return p;
}

static struct rejection_reason *
arity_rejection (tree first_arg, int expected, int actual)
{
  struct rejection_reason *r = alloc_rejection (rr_arity);
  int adjust = first_arg != NULL_TREE;
  r->u.arity.expected = expected - adjust;
  r->u.arity.actual = actual - adjust;
  return r;
}

static struct rejection_reason *
arg_conversion_rejection (tree first_arg, int n_arg, tree from, tree to)
{
  struct rejection_reason *r = alloc_rejection (rr_arg_conversion);
  int adjust = first_arg != NULL_TREE;
  r->u.conversion.n_arg = n_arg - adjust;
  r->u.conversion.from = from;
  r->u.conversion.to_type = to;
  return r;
}

static struct rejection_reason *
bad_arg_conversion_rejection (tree first_arg, int n_arg, tree from, tree to)
{
  struct rejection_reason *r = alloc_rejection (rr_bad_arg_conversion);
  int adjust = first_arg != NULL_TREE;
  r->u.bad_conversion.n_arg = n_arg - adjust;
  r->u.bad_conversion.from = from;
  r->u.bad_conversion.to_type = to;
  return r;
}

static struct rejection_reason *
explicit_conversion_rejection (tree from, tree to)
{
  struct rejection_reason *r = alloc_rejection (rr_explicit_conversion);
  r->u.conversion.n_arg = 0;
  r->u.conversion.from = from;
  r->u.conversion.to_type = to;
  return r;
}

static struct rejection_reason *
template_conversion_rejection (tree from, tree to)
{
  struct rejection_reason *r = alloc_rejection (rr_template_conversion);
  r->u.conversion.n_arg = 0;
  r->u.conversion.from = from;
  r->u.conversion.to_type = to;
  return r;
}

static struct rejection_reason *
template_unification_rejection (tree tmpl, tree explicit_targs, tree targs,
				const tree *args, unsigned int nargs,
				tree return_type, unification_kind_t strict,
				int flags)
{
  size_t args_n_bytes = sizeof (*args) * nargs;
  tree *args1 = (tree *) conversion_obstack_alloc (args_n_bytes);
  struct rejection_reason *r = alloc_rejection (rr_template_unification);
  r->u.template_unification.tmpl = tmpl;
  r->u.template_unification.explicit_targs = explicit_targs;
  r->u.template_unification.num_targs = TREE_VEC_LENGTH (targs);
  /* Copy args to our own storage.  */
  memcpy (args1, args, args_n_bytes);
  r->u.template_unification.args = args1;
  r->u.template_unification.nargs = nargs;
  r->u.template_unification.return_type = return_type;
  r->u.template_unification.strict = strict;
  r->u.template_unification.flags = flags;
  return r;
}

static struct rejection_reason *
template_unification_error_rejection (void)
{
  return alloc_rejection (rr_template_unification);
}

static struct rejection_reason *
invalid_copy_with_fn_template_rejection (void)
{
  struct rejection_reason *r = alloc_rejection (rr_invalid_copy);
  return r;
}

/* Dynamically allocate a conversion.  */

static conversion *
alloc_conversion (conversion_kind kind)
{
  conversion *c;
  c = (conversion *) conversion_obstack_alloc (sizeof (conversion));
  c->kind = kind;
  return c;
}

#ifdef ENABLE_CHECKING

/* Make sure that all memory on the conversion obstack has been
   freed.  */

void
validate_conversion_obstack (void)
{
  if (conversion_obstack_initialized)
    gcc_assert ((obstack_next_free (&conversion_obstack)
		 == obstack_base (&conversion_obstack)));
}

#endif /* ENABLE_CHECKING */

/* Dynamically allocate an array of N conversions.  */

static conversion **
alloc_conversions (size_t n)
{
  return (conversion **) conversion_obstack_alloc (n * sizeof (conversion *));
}

static conversion *
build_conv (conversion_kind code, tree type, conversion *from)
{
  conversion *t;
  conversion_rank rank = CONVERSION_RANK (from);

  /* Note that the caller is responsible for filling in t->cand for
     user-defined conversions.  */
  t = alloc_conversion (code);
  t->type = type;
  t->u.next = from;

  switch (code)
    {
    case ck_ptr:
    case ck_pmem:
    case ck_base:
    case ck_std:
      if (rank < cr_std)
	rank = cr_std;
      break;

    case ck_qual:
      if (rank < cr_exact)
	rank = cr_exact;
      break;

    default:
      break;
    }
  t->rank = rank;
  t->user_conv_p = (code == ck_user || from->user_conv_p);
  t->bad_p = from->bad_p;
  t->base_p = false;
  return t;
}

/* Represent a conversion from CTOR, a braced-init-list, to TYPE, a
   specialization of std::initializer_list<T>, if such a conversion is
   possible.  */

static conversion *
build_list_conv (tree type, tree ctor, int flags, tsubst_flags_t complain)
{
  tree elttype = TREE_VEC_ELT (CLASSTYPE_TI_ARGS (type), 0);
  unsigned len = CONSTRUCTOR_NELTS (ctor);
  conversion **subconvs = alloc_conversions (len);
  conversion *t;
  unsigned i;
  tree val;

  /* Within a list-initialization we can have more user-defined
     conversions.  */
  flags &= ~LOOKUP_NO_CONVERSION;
  /* But no narrowing conversions.  */
  flags |= LOOKUP_NO_NARROWING;

  FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (ctor), i, val)
    {
      conversion *sub
	= implicit_conversion (elttype, TREE_TYPE (val), val,
			       false, flags, complain);
      if (sub == NULL)
	return NULL;

      subconvs[i] = sub;
    }

  t = alloc_conversion (ck_list);
  t->type = type;
  t->u.list = subconvs;
  t->rank = cr_exact;

  for (i = 0; i < len; ++i)
    {
      conversion *sub = subconvs[i];
      if (sub->rank > t->rank)
	t->rank = sub->rank;
      if (sub->user_conv_p)
	t->user_conv_p = true;
      if (sub->bad_p)
	t->bad_p = true;
    }

  return t;
}

/* Return the next conversion of the conversion chain (if applicable),
   or NULL otherwise.  Please use this function instead of directly
   accessing fields of struct conversion.  */

static conversion *
next_conversion (conversion *conv)
{
  if (conv == NULL
      || conv->kind == ck_identity
      || conv->kind == ck_ambig
      || conv->kind == ck_list)
    return NULL;
  return conv->u.next;
}

/* Subroutine of build_aggr_conv: check whether CTOR, a braced-init-list,
   is a valid aggregate initializer for array type ATYPE.  */

static bool
can_convert_array (tree atype, tree ctor, int flags, tsubst_flags_t complain)
{
  unsigned i;
  tree elttype = TREE_TYPE (atype);
  for (i = 0; i < CONSTRUCTOR_NELTS (ctor); ++i)
    {
      tree val = CONSTRUCTOR_ELT (ctor, i)->value;
      bool ok;
      if (TREE_CODE (elttype) == ARRAY_TYPE
	  && TREE_CODE (val) == CONSTRUCTOR)
	ok = can_convert_array (elttype, val, flags, complain);
      else
	ok = can_convert_arg (elttype, TREE_TYPE (val), val, flags,
			      complain);
      if (!ok)
	return false;
    }
  return true;
}

/* Represent a conversion from CTOR, a braced-init-list, to TYPE, an
   aggregate class, if such a conversion is possible.  */

static conversion *
build_aggr_conv (tree type, tree ctor, int flags, tsubst_flags_t complain)
{
  unsigned HOST_WIDE_INT i = 0;
  conversion *c;
  tree field = next_initializable_field (TYPE_FIELDS (type));
  tree empty_ctor = NULL_TREE;

  ctor = reshape_init (type, ctor, tf_none);
  if (ctor == error_mark_node)
    return NULL;

  /* The conversions within the init-list aren't affected by the enclosing
     context; they're always simple copy-initialization.  */
  flags = LOOKUP_IMPLICIT|LOOKUP_NO_NARROWING;

  for (; field; field = next_initializable_field (DECL_CHAIN (field)))
    {
      tree ftype = TREE_TYPE (field);
      tree val;
      bool ok;

      if (i < CONSTRUCTOR_NELTS (ctor))
	val = CONSTRUCTOR_ELT (ctor, i)->value;
      else if (TREE_CODE (ftype) == REFERENCE_TYPE)
	/* Value-initialization of reference is ill-formed.  */
	return NULL;
      else
	{
	  if (empty_ctor == NULL_TREE)
	    empty_ctor = build_constructor (init_list_type_node, NULL);
	  val = empty_ctor;
	}
      ++i;

      if (TREE_CODE (ftype) == ARRAY_TYPE
	  && TREE_CODE (val) == CONSTRUCTOR)
	ok = can_convert_array (ftype, val, flags, complain);
      else
	ok = can_convert_arg (ftype, TREE_TYPE (val), val, flags,
			      complain);

      if (!ok)
	return NULL;

      if (TREE_CODE (type) == UNION_TYPE)
	break;
    }

  if (i < CONSTRUCTOR_NELTS (ctor))
    return NULL;

  c = alloc_conversion (ck_aggr);
  c->type = type;
  c->rank = cr_exact;
  c->user_conv_p = true;
  c->check_narrowing = true;
  c->u.next = NULL;
  return c;
}

/* Represent a conversion from CTOR, a braced-init-list, to TYPE, an
   array type, if such a conversion is possible.  */

static conversion *
build_array_conv (tree type, tree ctor, int flags, tsubst_flags_t complain)
{
  conversion *c;
  unsigned HOST_WIDE_INT len = CONSTRUCTOR_NELTS (ctor);
  tree elttype = TREE_TYPE (type);
  unsigned i;
  tree val;
  bool bad = false;
  bool user = false;
  enum conversion_rank rank = cr_exact;

  /* We might need to propagate the size from the element to the array.  */
  complete_type (type);

  if (TYPE_DOMAIN (type)
      && !variably_modified_type_p (TYPE_DOMAIN (type), NULL_TREE))
    {
      unsigned HOST_WIDE_INT alen = tree_to_uhwi (array_type_nelts_top (type));
      if (alen < len)
	return NULL;
    }

  flags = LOOKUP_IMPLICIT|LOOKUP_NO_NARROWING;

  FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (ctor), i, val)
    {
      conversion *sub
	= implicit_conversion (elttype, TREE_TYPE (val), val,
			       false, flags, complain);
      if (sub == NULL)
	return NULL;

      if (sub->rank > rank)
	rank = sub->rank;
      if (sub->user_conv_p)
	user = true;
      if (sub->bad_p)
	bad = true;
    }

  c = alloc_conversion (ck_aggr);
  c->type = type;
  c->rank = rank;
  c->user_conv_p = user;
  c->bad_p = bad;
  c->u.next = NULL;
  return c;
}

/* Represent a conversion from CTOR, a braced-init-list, to TYPE, a
   complex type, if such a conversion is possible.  */

static conversion *
build_complex_conv (tree type, tree ctor, int flags,
		    tsubst_flags_t complain)
{
  conversion *c;
  unsigned HOST_WIDE_INT len = CONSTRUCTOR_NELTS (ctor);
  tree elttype = TREE_TYPE (type);
  unsigned i;
  tree val;
  bool bad = false;
  bool user = false;
  enum conversion_rank rank = cr_exact;

  if (len != 2)
    return NULL;

  flags = LOOKUP_IMPLICIT|LOOKUP_NO_NARROWING;

  FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (ctor), i, val)
    {
      conversion *sub
	= implicit_conversion (elttype, TREE_TYPE (val), val,
			       false, flags, complain);
      if (sub == NULL)
	return NULL;

      if (sub->rank > rank)
	rank = sub->rank;
      if (sub->user_conv_p)
	user = true;
      if (sub->bad_p)
	bad = true;
    }

  c = alloc_conversion (ck_aggr);
  c->type = type;
  c->rank = rank;
  c->user_conv_p = user;
  c->bad_p = bad;
  c->u.next = NULL;
  return c;
}

/* Build a representation of the identity conversion from EXPR to
   itself.  The TYPE should match the type of EXPR, if EXPR is non-NULL.  */

static conversion *
build_identity_conv (tree type, tree expr)
{
  conversion *c;

  c = alloc_conversion (ck_identity);
  c->type = type;
  c->u.expr = expr;

  return c;
}

/* Converting from EXPR to TYPE was ambiguous in the sense that there
   were multiple user-defined conversions to accomplish the job.
   Build a conversion that indicates that ambiguity.  */

static conversion *
build_ambiguous_conv (tree type, tree expr)
{
  conversion *c;

  c = alloc_conversion (ck_ambig);
  c->type = type;
  c->u.expr = expr;

  return c;
}

tree
strip_top_quals (tree t)
{
  if (TREE_CODE (t) == ARRAY_TYPE)
    return t;
  return cp_build_qualified_type (t, 0);
}

/* Returns the standard conversion path (see [conv]) from type FROM to type
   TO, if any.  For proper handling of null pointer constants, you must
   also pass the expression EXPR to convert from.  If C_CAST_P is true,
   this conversion is coming from a C-style cast.  */

static conversion *
standard_conversion (tree to, tree from, tree expr, bool c_cast_p,
		     int flags)
{
  enum tree_code fcode, tcode;
  conversion *conv;
  bool fromref = false;
  tree qualified_to;

  to = non_reference (to);
  if (TREE_CODE (from) == REFERENCE_TYPE)
    {
      fromref = true;
      from = TREE_TYPE (from);
    }
  qualified_to = to;
  to = strip_top_quals (to);
  from = strip_top_quals (from);

  if ((TYPE_PTRFN_P (to) || TYPE_PTRMEMFUNC_P (to))
      && expr && type_unknown_p (expr))
    {
      tsubst_flags_t tflags = tf_conv;
      expr = instantiate_type (to, expr, tflags);
      if (expr == error_mark_node)
	return NULL;
      from = TREE_TYPE (expr);
    }

  fcode = TREE_CODE (from);
  tcode = TREE_CODE (to);

  conv = build_identity_conv (from, expr);
  if (fcode == FUNCTION_TYPE || fcode == ARRAY_TYPE)
    {
      from = type_decays_to (from);
      fcode = TREE_CODE (from);
      conv = build_conv (ck_lvalue, from, conv);
    }
  else if (fromref || (expr && lvalue_p (expr)))
    {
      if (expr)
	{
	  tree bitfield_type;
	  bitfield_type = is_bitfield_expr_with_lowered_type (expr);
	  if (bitfield_type)
	    {
	      from = strip_top_quals (bitfield_type);
	      fcode = TREE_CODE (from);
	    }
	}
      conv = build_conv (ck_rvalue, from, conv);
      if (flags & LOOKUP_PREFER_RVALUE)
	conv->rvaluedness_matches_p = true;
    }

   /* Allow conversion between `__complex__' data types.  */
  if (tcode == COMPLEX_TYPE && fcode == COMPLEX_TYPE)
    {
      /* The standard conversion sequence to convert FROM to TO is
	 the standard conversion sequence to perform componentwise
	 conversion.  */
      conversion *part_conv = standard_conversion
	(TREE_TYPE (to), TREE_TYPE (from), NULL_TREE, c_cast_p, flags);

      if (part_conv)
	{
	  conv = build_conv (part_conv->kind, to, conv);
	  conv->rank = part_conv->rank;
	}
      else
	conv = NULL;

      return conv;
    }

  if (same_type_p (from, to))
    {
      if (CLASS_TYPE_P (to) && conv->kind == ck_rvalue)
	conv->type = qualified_to;
      return conv;
    }

  /* [conv.ptr]
     A null pointer constant can be converted to a pointer type; ... A
     null pointer constant of integral type can be converted to an
     rvalue of type std::nullptr_t. */
  if ((tcode == POINTER_TYPE || TYPE_PTRMEM_P (to)
       || NULLPTR_TYPE_P (to))
      && expr && null_ptr_cst_p (expr))
    conv = build_conv (ck_std, to, conv);
  else if ((tcode == INTEGER_TYPE && fcode == POINTER_TYPE)
	   || (tcode == POINTER_TYPE && fcode == INTEGER_TYPE))
    {
      /* For backwards brain damage compatibility, allow interconversion of
	 pointers and integers with a pedwarn.  */
      conv = build_conv (ck_std, to, conv);
      conv->bad_p = true;
    }
  else if (UNSCOPED_ENUM_P (to) && fcode == INTEGER_TYPE)
    {
      /* For backwards brain damage compatibility, allow interconversion of
	 enums and integers with a pedwarn.  */
      conv = build_conv (ck_std, to, conv);
      conv->bad_p = true;
    }
  else if ((tcode == POINTER_TYPE && fcode == POINTER_TYPE)
	   || (TYPE_PTRDATAMEM_P (to) && TYPE_PTRDATAMEM_P (from)))
    {
      tree to_pointee;
      tree from_pointee;

      if (tcode == POINTER_TYPE
	  && same_type_ignoring_top_level_qualifiers_p (TREE_TYPE (from),
							TREE_TYPE (to)))
	;
      else if (VOID_TYPE_P (TREE_TYPE (to))
	       && !TYPE_PTRDATAMEM_P (from)
	       && TREE_CODE (TREE_TYPE (from)) != FUNCTION_TYPE)
	{
	  tree nfrom = TREE_TYPE (from);
	  /* Don't try to apply restrict to void.  */
	  int quals = cp_type_quals (nfrom) & ~TYPE_QUAL_RESTRICT;
	  from = build_pointer_type
	    (cp_build_qualified_type (void_type_node, quals));
	  conv = build_conv (ck_ptr, from, conv);
	}
      else if (TYPE_PTRDATAMEM_P (from))
	{
	  tree fbase = TYPE_PTRMEM_CLASS_TYPE (from);
	  tree tbase = TYPE_PTRMEM_CLASS_TYPE (to);

	  if (DERIVED_FROM_P (fbase, tbase)
	      && (same_type_ignoring_top_level_qualifiers_p
		  (TYPE_PTRMEM_POINTED_TO_TYPE (from),
		   TYPE_PTRMEM_POINTED_TO_TYPE (to))))
	    {
	      from = build_ptrmem_type (tbase,
					TYPE_PTRMEM_POINTED_TO_TYPE (from));
	      conv = build_conv (ck_pmem, from, conv);
	    }
	  else if (!same_type_p (fbase, tbase))
	    return NULL;
	}
      else if (CLASS_TYPE_P (TREE_TYPE (from))
	       && CLASS_TYPE_P (TREE_TYPE (to))
	       /* [conv.ptr]

		  An rvalue of type "pointer to cv D," where D is a
		  class type, can be converted to an rvalue of type
		  "pointer to cv B," where B is a base class (clause
		  _class.derived_) of D.  If B is an inaccessible
		  (clause _class.access_) or ambiguous
		  (_class.member.lookup_) base class of D, a program
		  that necessitates this conversion is ill-formed.
		  Therefore, we use DERIVED_FROM_P, and do not check
		  access or uniqueness.  */
	       && DERIVED_FROM_P (TREE_TYPE (to), TREE_TYPE (from)))
	{
	  from =
	    cp_build_qualified_type (TREE_TYPE (to),
				     cp_type_quals (TREE_TYPE (from)));
	  from = build_pointer_type (from);
	  conv = build_conv (ck_ptr, from, conv);
	  conv->base_p = true;
	}

      if (tcode == POINTER_TYPE)
	{
	  to_pointee = TREE_TYPE (to);
	  from_pointee = TREE_TYPE (from);
	}
      else
	{
	  to_pointee = TYPE_PTRMEM_POINTED_TO_TYPE (to);
	  from_pointee = TYPE_PTRMEM_POINTED_TO_TYPE (from);
	}

      if (same_type_p (from, to))
	/* OK */;
      else if (c_cast_p && comp_ptr_ttypes_const (to, from))
	/* In a C-style cast, we ignore CV-qualification because we
	   are allowed to perform a static_cast followed by a
	   const_cast.  */
	conv = build_conv (ck_qual, to, conv);
      else if (!c_cast_p && comp_ptr_ttypes (to_pointee, from_pointee))
	conv = build_conv (ck_qual, to, conv);
      else if (expr && string_conv_p (to, expr, 0))
	/* converting from string constant to char *.  */
	conv = build_conv (ck_qual, to, conv);
      /* Allow conversions among compatible ObjC pointer types (base
	 conversions have been already handled above).  */
      else if (c_dialect_objc ()
	       && objc_compare_types (to, from, -4, NULL_TREE))
	conv = build_conv (ck_ptr, to, conv);
      else if (ptr_reasonably_similar (to_pointee, from_pointee))
	{
	  conv = build_conv (ck_ptr, to, conv);
	  conv->bad_p = true;
	}
      else
	return NULL;

      from = to;
    }
  else if (TYPE_PTRMEMFUNC_P (to) && TYPE_PTRMEMFUNC_P (from))
    {
      tree fromfn = TREE_TYPE (TYPE_PTRMEMFUNC_FN_TYPE (from));
      tree tofn = TREE_TYPE (TYPE_PTRMEMFUNC_FN_TYPE (to));
      tree fbase = class_of_this_parm (fromfn);
      tree tbase = class_of_this_parm (tofn);

      if (!DERIVED_FROM_P (fbase, tbase)
	  || !same_type_p (static_fn_type (fromfn),
			   static_fn_type (tofn)))
	return NULL;

      from = build_memfn_type (fromfn,
                               tbase,
                               cp_type_quals (tbase),
                               type_memfn_rqual (tofn));
      from = build_ptrmemfunc_type (build_pointer_type (from));
      conv = build_conv (ck_pmem, from, conv);
      conv->base_p = true;
    }
  else if (tcode == BOOLEAN_TYPE)
    {
      /* [conv.bool]

	  A prvalue of arithmetic, unscoped enumeration, pointer, or pointer
	  to member type can be converted to a prvalue of type bool. ...
	  For direct-initialization (8.5 [dcl.init]), a prvalue of type
	  std::nullptr_t can be converted to a prvalue of type bool;  */
      if (ARITHMETIC_TYPE_P (from)
	  || UNSCOPED_ENUM_P (from)
	  || fcode == POINTER_TYPE
	  || TYPE_PTRMEM_P (from)
	  || NULLPTR_TYPE_P (from))
	{
	  conv = build_conv (ck_std, to, conv);
	  if (fcode == POINTER_TYPE
	      || TYPE_PTRDATAMEM_P (from)
	      || (TYPE_PTRMEMFUNC_P (from)
		  && conv->rank < cr_pbool)
	      || NULLPTR_TYPE_P (from))
	    conv->rank = cr_pbool;
	  if (NULLPTR_TYPE_P (from) && (flags & LOOKUP_ONLYCONVERTING))
	    conv->bad_p = true;
	  return conv;
	}

      return NULL;
    }
  /* We don't check for ENUMERAL_TYPE here because there are no standard
     conversions to enum type.  */
  /* As an extension, allow conversion to complex type.  */
  else if (ARITHMETIC_TYPE_P (to))
    {
      if (! (INTEGRAL_CODE_P (fcode)
	     || (fcode == REAL_TYPE && !(flags & LOOKUP_NO_NON_INTEGRAL)))
          || SCOPED_ENUM_P (from))
	return NULL;
      conv = build_conv (ck_std, to, conv);

      /* Give this a better rank if it's a promotion.  */
      if (same_type_p (to, type_promotes_to (from))
	  && next_conversion (conv)->rank <= cr_promotion)
	conv->rank = cr_promotion;
    }
  else if (fcode == VECTOR_TYPE && tcode == VECTOR_TYPE
	   && vector_types_convertible_p (from, to, false))
    return build_conv (ck_std, to, conv);
  else if (MAYBE_CLASS_TYPE_P (to) && MAYBE_CLASS_TYPE_P (from)
	   && is_properly_derived_from (from, to))
    {
      if (conv->kind == ck_rvalue)
	conv = next_conversion (conv);
      conv = build_conv (ck_base, to, conv);
      /* The derived-to-base conversion indicates the initialization
	 of a parameter with base type from an object of a derived
	 type.  A temporary object is created to hold the result of
	 the conversion unless we're binding directly to a reference.  */
      conv->need_temporary_p = !(flags & LOOKUP_NO_TEMP_BIND);
    }
  else
    return NULL;

  if (flags & LOOKUP_NO_NARROWING)
    conv->check_narrowing = true;

  return conv;
}

/* Returns nonzero if T1 is reference-related to T2.  */

bool
reference_related_p (tree t1, tree t2)
{
  if (t1 == error_mark_node || t2 == error_mark_node)
    return false;

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

/* Returns nonzero if T1 is reference-compatible with T2.  */

static bool
reference_compatible_p (tree t1, tree t2)
{
  /* [dcl.init.ref]

     "cv1 T1" is reference compatible with "cv2 T2" if T1 is
     reference-related to T2 and cv1 is the same cv-qualification as,
     or greater cv-qualification than, cv2.  */
  return (reference_related_p (t1, t2)
	  && at_least_as_qualified_p (t1, t2));
}

/* A reference of the indicated TYPE is being bound directly to the
   expression represented by the implicit conversion sequence CONV.
   Return a conversion sequence for this binding.  */

static conversion *
direct_reference_binding (tree type, conversion *conv)
{
  tree t;

  gcc_assert (TREE_CODE (type) == REFERENCE_TYPE);
  gcc_assert (TREE_CODE (conv->type) != REFERENCE_TYPE);

  t = TREE_TYPE (type);

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
  if (!same_type_ignoring_top_level_qualifiers_p (t, conv->type))
    {
      /* Represent the derived-to-base conversion.  */
      conv = build_conv (ck_base, t, conv);
      /* We will actually be binding to the base-class subobject in
	 the derived class, so we mark this conversion appropriately.
	 That way, convert_like knows not to generate a temporary.  */
      conv->need_temporary_p = false;
    }
  return build_conv (ck_ref_bind, type, conv);
}

/* Returns the conversion path from type FROM to reference type TO for
   purposes of reference binding.  For lvalue binding, either pass a
   reference type to FROM or an lvalue expression to EXPR.  If the
   reference will be bound to a temporary, NEED_TEMPORARY_P is set for
   the conversion returned.  If C_CAST_P is true, this
   conversion is coming from a C-style cast.  */

static conversion *
reference_binding (tree rto, tree rfrom, tree expr, bool c_cast_p, int flags,
		   tsubst_flags_t complain)
{
  conversion *conv = NULL;
  tree to = TREE_TYPE (rto);
  tree from = rfrom;
  tree tfrom;
  bool related_p;
  bool compatible_p;
  cp_lvalue_kind gl_kind;
  bool is_lvalue;

  if (TREE_CODE (to) == FUNCTION_TYPE && expr && type_unknown_p (expr))
    {
      expr = instantiate_type (to, expr, tf_none);
      if (expr == error_mark_node)
	return NULL;
      from = TREE_TYPE (expr);
    }

  if (expr && BRACE_ENCLOSED_INITIALIZER_P (expr))
    {
      maybe_warn_cpp0x (CPP0X_INITIALIZER_LISTS);
      /* DR 1288: Otherwise, if the initializer list has a single element
	 of type E and ... [T's] referenced type is reference-related to E,
	 the object or reference is initialized from that element... */
      if (CONSTRUCTOR_NELTS (expr) == 1)
	{
	  tree elt = CONSTRUCTOR_ELT (expr, 0)->value;
	  if (error_operand_p (elt))
	    return NULL;
	  tree etype = TREE_TYPE (elt);
	  if (reference_related_p (to, etype))
	    {
	      expr = elt;
	      from = etype;
	      goto skip;
	    }
	}
      /* Otherwise, if T is a reference type, a prvalue temporary of the
	 type referenced by T is copy-list-initialized or
	 direct-list-initialized, depending on the kind of initialization
	 for the reference, and the reference is bound to that temporary. */
      conv = implicit_conversion (to, from, expr, c_cast_p,
				  flags|LOOKUP_NO_TEMP_BIND, complain);
    skip:;
    }

  if (TREE_CODE (from) == REFERENCE_TYPE)
    {
      from = TREE_TYPE (from);
      if (!TYPE_REF_IS_RVALUE (rfrom)
	  || TREE_CODE (from) == FUNCTION_TYPE)
	gl_kind = clk_ordinary;
      else
	gl_kind = clk_rvalueref;
    }
  else if (expr)
    {
      gl_kind = lvalue_kind (expr);
      if (gl_kind & clk_class)
	/* A class prvalue is not a glvalue.  */
	gl_kind = clk_none;
    }
  else
    gl_kind = clk_none;
  is_lvalue = gl_kind && !(gl_kind & clk_rvalueref);

  tfrom = from;
  if ((gl_kind & clk_bitfield) != 0)
    tfrom = unlowered_expr_type (expr);

  /* Figure out whether or not the types are reference-related and
     reference compatible.  We have do do this after stripping
     references from FROM.  */
  related_p = reference_related_p (to, tfrom);
  /* If this is a C cast, first convert to an appropriately qualified
     type, so that we can later do a const_cast to the desired type.  */
  if (related_p && c_cast_p
      && !at_least_as_qualified_p (to, tfrom))
    to = cp_build_qualified_type (to, cp_type_quals (tfrom));
  compatible_p = reference_compatible_p (to, tfrom);

  /* Directly bind reference when target expression's type is compatible with
     the reference and expression is an lvalue. In DR391, the wording in
     [8.5.3/5 dcl.init.ref] is changed to also require direct bindings for
     const and rvalue references to rvalues of compatible class type.
     We should also do direct bindings for non-class xvalues.  */
  if (related_p
      && (gl_kind
	  || (!(flags & LOOKUP_NO_TEMP_BIND)
	      && (CLASS_TYPE_P (from)
		  || TREE_CODE (from) == ARRAY_TYPE))))
    {
      /* [dcl.init.ref]

	 If the initializer expression

	 -- is an lvalue (but not an lvalue for a bit-field), and "cv1 T1"
	    is reference-compatible with "cv2 T2,"

	 the reference is bound directly to the initializer expression
	 lvalue.

	 [...]
	 If the initializer expression is an rvalue, with T2 a class type,
	 and "cv1 T1" is reference-compatible with "cv2 T2", the reference
	 is bound to the object represented by the rvalue or to a sub-object
	 within that object.  */

      conv = build_identity_conv (tfrom, expr);
      conv = direct_reference_binding (rto, conv);

      if (flags & LOOKUP_PREFER_RVALUE)
	/* The top-level caller requested that we pretend that the lvalue
	   be treated as an rvalue.  */
	conv->rvaluedness_matches_p = TYPE_REF_IS_RVALUE (rto);
      else if (TREE_CODE (rfrom) == REFERENCE_TYPE)
	/* Handle rvalue reference to function properly.  */
	conv->rvaluedness_matches_p
	  = (TYPE_REF_IS_RVALUE (rto) == TYPE_REF_IS_RVALUE (rfrom));
      else
	conv->rvaluedness_matches_p 
          = (TYPE_REF_IS_RVALUE (rto) == !is_lvalue);

      if ((gl_kind & clk_bitfield) != 0
	  || ((gl_kind & clk_packed) != 0 && !TYPE_PACKED (to)))
	/* For the purposes of overload resolution, we ignore the fact
	   this expression is a bitfield or packed field. (In particular,
	   [over.ics.ref] says specifically that a function with a
	   non-const reference parameter is viable even if the
	   argument is a bitfield.)

	   However, when we actually call the function we must create
	   a temporary to which to bind the reference.  If the
	   reference is volatile, or isn't const, then we cannot make
	   a temporary, so we just issue an error when the conversion
	   actually occurs.  */
	conv->need_temporary_p = true;

      /* Don't allow binding of lvalues (other than function lvalues) to
	 rvalue references.  */
      if (is_lvalue && TYPE_REF_IS_RVALUE (rto)
	  && TREE_CODE (to) != FUNCTION_TYPE
          && !(flags & LOOKUP_PREFER_RVALUE))
	conv->bad_p = true;

      /* Nor the reverse.  */
      if (!is_lvalue && !TYPE_REF_IS_RVALUE (rto)
	  && (!CP_TYPE_CONST_NON_VOLATILE_P (to)
	      || (flags & LOOKUP_NO_RVAL_BIND))
	  && TREE_CODE (to) != FUNCTION_TYPE)
	conv->bad_p = true;

      if (!compatible_p)
	conv->bad_p = true;

      return conv;
    }
  /* [class.conv.fct] A conversion function is never used to convert a
     (possibly cv-qualified) object to the (possibly cv-qualified) same
     object type (or a reference to it), to a (possibly cv-qualified) base
     class of that type (or a reference to it).... */
  else if (CLASS_TYPE_P (from) && !related_p
	   && !(flags & LOOKUP_NO_CONVERSION))
    {
      /* [dcl.init.ref]

	 If the initializer expression

	 -- has a class type (i.e., T2 is a class type) can be
	    implicitly converted to an lvalue of type "cv3 T3," where
	    "cv1 T1" is reference-compatible with "cv3 T3".  (this
	    conversion is selected by enumerating the applicable
	    conversion functions (_over.match.ref_) and choosing the
	    best one through overload resolution.  (_over.match_).

	the reference is bound to the lvalue result of the conversion
	in the second case.  */
      z_candidate *cand = build_user_type_conversion_1 (rto, expr, flags,
							complain);
      if (cand)
	return cand->second_conv;
    }

  /* From this point on, we conceptually need temporaries, even if we
     elide them.  Only the cases above are "direct bindings".  */
  if (flags & LOOKUP_NO_TEMP_BIND)
    return NULL;

  /* [over.ics.rank]

     When a parameter of reference type is not bound directly to an
     argument expression, the conversion sequence is the one required
     to convert the argument expression to the underlying type of the
     reference according to _over.best.ics_.  Conceptually, this
     conversion sequence corresponds to copy-initializing a temporary
     of the underlying type with the argument expression.  Any
     difference in top-level cv-qualification is subsumed by the
     initialization itself and does not constitute a conversion.  */

  /* We're generating a temporary now, but don't bind any more in the
     conversion (specifically, don't slice the temporary returned by a
     conversion operator).  */
  flags |= LOOKUP_NO_TEMP_BIND;

  /* Core issue 899: When [copy-]initializing a temporary to be bound
     to the first parameter of a copy constructor (12.8) called with
     a single argument in the context of direct-initialization,
     explicit conversion functions are also considered.

     So don't set LOOKUP_ONLYCONVERTING in that case.  */
  if (!(flags & LOOKUP_COPY_PARM))
    flags |= LOOKUP_ONLYCONVERTING;

  if (!conv)
    conv = implicit_conversion (to, from, expr, c_cast_p,
				flags, complain);
  if (!conv)
    return NULL;

  if (conv->user_conv_p)
    {
      /* If initializing the temporary used a conversion function,
	 recalculate the second conversion sequence.  */
      for (conversion *t = conv; t; t = next_conversion (t))
	if (t->kind == ck_user
	    && DECL_CONV_FN_P (t->cand->fn))
	  {
	    tree ftype = TREE_TYPE (TREE_TYPE (t->cand->fn));
	    int sflags = (flags|LOOKUP_NO_CONVERSION)&~LOOKUP_NO_TEMP_BIND;
	    conversion *new_second
	      = reference_binding (rto, ftype, NULL_TREE, c_cast_p,
				   sflags, complain);
	    if (!new_second)
	      return NULL;
	    return merge_conversion_sequences (t, new_second);
	  }
    }

  conv = build_conv (ck_ref_bind, rto, conv);
  /* This reference binding, unlike those above, requires the
     creation of a temporary.  */
  conv->need_temporary_p = true;
  conv->rvaluedness_matches_p = TYPE_REF_IS_RVALUE (rto);

  /* [dcl.init.ref]

     Otherwise, the reference shall be an lvalue reference to a
     non-volatile const type, or the reference shall be an rvalue
     reference.  */
  if (!CP_TYPE_CONST_NON_VOLATILE_P (to) && !TYPE_REF_IS_RVALUE (rto))
    conv->bad_p = true;

  /* [dcl.init.ref]

     Otherwise, a temporary of type "cv1 T1" is created and
     initialized from the initializer expression using the rules for a
     non-reference copy initialization.  If T1 is reference-related to
     T2, cv1 must be the same cv-qualification as, or greater
     cv-qualification than, cv2; otherwise, the program is ill-formed.  */
  if (related_p && !at_least_as_qualified_p (to, from))
    conv->bad_p = true;

  return conv;
}

/* Returns the implicit conversion sequence (see [over.ics]) from type
   FROM to type TO.  The optional expression EXPR may affect the
   conversion.  FLAGS are the usual overloading flags.  If C_CAST_P is
   true, this conversion is coming from a C-style cast.  */

static conversion *
implicit_conversion (tree to, tree from, tree expr, bool c_cast_p,
		     int flags, tsubst_flags_t complain)
{
  conversion *conv;

  if (from == error_mark_node || to == error_mark_node
      || expr == error_mark_node)
    return NULL;

  /* Other flags only apply to the primary function in overload
     resolution, or after we've chosen one.  */
  flags &= (LOOKUP_ONLYCONVERTING|LOOKUP_NO_CONVERSION|LOOKUP_COPY_PARM
	    |LOOKUP_NO_TEMP_BIND|LOOKUP_NO_RVAL_BIND|LOOKUP_PREFER_RVALUE
	    |LOOKUP_NO_NARROWING|LOOKUP_PROTECT|LOOKUP_NO_NON_INTEGRAL);

  /* FIXME: actually we don't want warnings either, but we can't just
     have 'complain &= ~(tf_warning|tf_error)' because it would cause
     the regression of, eg, g++.old-deja/g++.benjamin/16077.C.
     We really ought not to issue that warning until we've committed
     to that conversion.  */
  complain &= ~tf_error;

  if (TREE_CODE (to) == REFERENCE_TYPE)
    conv = reference_binding (to, from, expr, c_cast_p, flags, complain);
  else
    conv = standard_conversion (to, from, expr, c_cast_p, flags);

  if (conv)
    return conv;

  if (expr && BRACE_ENCLOSED_INITIALIZER_P (expr))
    {
      if (is_std_init_list (to))
	return build_list_conv (to, expr, flags, complain);

      /* As an extension, allow list-initialization of _Complex.  */
      if (TREE_CODE (to) == COMPLEX_TYPE)
	{
	  conv = build_complex_conv (to, expr, flags, complain);
	  if (conv)
	    return conv;
	}

      /* Allow conversion from an initializer-list with one element to a
	 scalar type.  */
      if (SCALAR_TYPE_P (to))
	{
	  int nelts = CONSTRUCTOR_NELTS (expr);
	  tree elt;

	  if (nelts == 0)
	    elt = build_value_init (to, tf_none);
	  else if (nelts == 1)
	    elt = CONSTRUCTOR_ELT (expr, 0)->value;
	  else
	    elt = error_mark_node;

	  conv = implicit_conversion (to, TREE_TYPE (elt), elt,
				      c_cast_p, flags, complain);
	  if (conv)
	    {
	      conv->check_narrowing = true;
	      if (BRACE_ENCLOSED_INITIALIZER_P (elt))
		/* Too many levels of braces, i.e. '{{1}}'.  */
		conv->bad_p = true;
	      return conv;
	    }
	}
      else if (TREE_CODE (to) == ARRAY_TYPE)
	return build_array_conv (to, expr, flags, complain);
    }

  if (expr != NULL_TREE
      && (MAYBE_CLASS_TYPE_P (from)
	  || MAYBE_CLASS_TYPE_P (to))
      && (flags & LOOKUP_NO_CONVERSION) == 0)
    {
      struct z_candidate *cand;

      if (CLASS_TYPE_P (to)
	  && BRACE_ENCLOSED_INITIALIZER_P (expr)
	  && !CLASSTYPE_NON_AGGREGATE (complete_type (to)))
	return build_aggr_conv (to, expr, flags, complain);

      cand = build_user_type_conversion_1 (to, expr, flags, complain);
      if (cand)
	conv = cand->second_conv;

      /* We used to try to bind a reference to a temporary here, but that
	 is now handled after the recursive call to this function at the end
	 of reference_binding.  */
      return conv;
    }

  return NULL;
}

/* Add a new entry to the list of candidates.  Used by the add_*_candidate
   functions.  ARGS will not be changed until a single candidate is
   selected.  */

static struct z_candidate *
add_candidate (struct z_candidate **candidates,
	       tree fn, tree first_arg, const vec<tree, va_gc> *args,
	       size_t num_convs, conversion **convs,
	       tree access_path, tree conversion_path,
	       int viable, struct rejection_reason *reason,
	       int flags)
{
  struct z_candidate *cand = (struct z_candidate *)
    conversion_obstack_alloc (sizeof (struct z_candidate));

  cand->fn = fn;
  cand->first_arg = first_arg;
  cand->args = args;
  cand->convs = convs;
  cand->num_convs = num_convs;
  cand->access_path = access_path;
  cand->conversion_path = conversion_path;
  cand->viable = viable;
  cand->reason = reason;
  cand->next = *candidates;
  cand->flags = flags;
  *candidates = cand;

  return cand;
}

/* Return the number of remaining arguments in the parameter list
   beginning with ARG.  */

static int
remaining_arguments (tree arg)
{
  int n;

  for (n = 0; arg != NULL_TREE && arg != void_list_node;
       arg = TREE_CHAIN (arg))
    n++;

  return n;
}

/* Create an overload candidate for the function or method FN called
   with the argument list FIRST_ARG/ARGS and add it to CANDIDATES.
   FLAGS is passed on to implicit_conversion.

   This does not change ARGS.

   CTYPE, if non-NULL, is the type we want to pretend this function
   comes from for purposes of overload resolution.  */

static struct z_candidate *
add_function_candidate (struct z_candidate **candidates,
			tree fn, tree ctype, tree first_arg,
			const vec<tree, va_gc> *args, tree access_path,
			tree conversion_path, int flags,
			tsubst_flags_t complain)
{
  tree parmlist = TYPE_ARG_TYPES (TREE_TYPE (fn));
  int i, len;
  conversion **convs;
  tree parmnode;
  tree orig_first_arg = first_arg;
  int skip;
  int viable = 1;
  struct rejection_reason *reason = NULL;

  /* At this point we should not see any functions which haven't been
     explicitly declared, except for friend functions which will have
     been found using argument dependent lookup.  */
  gcc_assert (!DECL_ANTICIPATED (fn) || DECL_HIDDEN_FRIEND_P (fn));

  /* The `this', `in_chrg' and VTT arguments to constructors are not
     considered in overload resolution.  */
  if (DECL_CONSTRUCTOR_P (fn))
    {
      parmlist = skip_artificial_parms_for (fn, parmlist);
      skip = num_artificial_parms_for (fn);
      if (skip > 0 && first_arg != NULL_TREE)
	{
	  --skip;
	  first_arg = NULL_TREE;
	}
    }
  else
    skip = 0;

  len = vec_safe_length (args) - skip + (first_arg != NULL_TREE ? 1 : 0);
  convs = alloc_conversions (len);

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

  if ((i < len && parmnode)
      || !sufficient_parms_p (parmnode))
    {
      int remaining = remaining_arguments (parmnode);
      viable = 0;
      reason = arity_rejection (first_arg, i + remaining, len);
    }
  /* When looking for a function from a subobject from an implicit
     copy/move constructor/operator=, don't consider anything that takes (a
     reference to) an unrelated type.  See c++/44909 and core 1092.  */
  else if (parmlist && (flags & LOOKUP_DEFAULTED))
    {
      if (DECL_CONSTRUCTOR_P (fn))
	i = 1;
      else if (DECL_ASSIGNMENT_OPERATOR_P (fn)
	       && DECL_OVERLOADED_OPERATOR_P (fn) == NOP_EXPR)
	i = 2;
      else
	i = 0;
      if (i && len == i)
	{
	  parmnode = chain_index (i-1, parmlist);
	  if (!reference_related_p (non_reference (TREE_VALUE (parmnode)),
				    ctype))
	    viable = 0;
	}

      /* This only applies at the top level.  */
      flags &= ~LOOKUP_DEFAULTED;
    }

  if (! viable)
    goto out;

  /* Second, for F to be a viable function, there shall exist for each
     argument an implicit conversion sequence that converts that argument
     to the corresponding parameter of F.  */

  parmnode = parmlist;

  for (i = 0; i < len; ++i)
    {
      tree argtype, to_type;
      tree arg;
      conversion *t;
      int is_this;

      if (parmnode == void_list_node)
	break;

      if (i == 0 && first_arg != NULL_TREE)
	arg = first_arg;
      else
	arg = CONST_CAST_TREE (
		(*args)[i + skip - (first_arg != NULL_TREE ? 1 : 0)]);
      argtype = lvalue_type (arg);

      is_this = (i == 0 && DECL_NONSTATIC_MEMBER_FUNCTION_P (fn)
		 && ! DECL_CONSTRUCTOR_P (fn));

      if (parmnode)
	{
	  tree parmtype = TREE_VALUE (parmnode);
	  int lflags = flags;

	  parmnode = TREE_CHAIN (parmnode);

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
	      parmtype = cp_build_qualified_type
		(ctype, cp_type_quals (TREE_TYPE (parmtype)));
	      if (FUNCTION_REF_QUALIFIED (TREE_TYPE (fn)))
		{
		  /* If the function has a ref-qualifier, the implicit
		     object parameter has reference type.  */
		  bool rv = FUNCTION_RVALUE_QUALIFIED (TREE_TYPE (fn));
		  parmtype = cp_build_reference_type (parmtype, rv);
		}
	      else
		{
		  parmtype = build_pointer_type (parmtype);
		  arg = build_this (arg);
		  argtype = lvalue_type (arg);
		}
	    }

	  /* Core issue 899: When [copy-]initializing a temporary to be bound
	     to the first parameter of a copy constructor (12.8) called with
	     a single argument in the context of direct-initialization,
	     explicit conversion functions are also considered.

	     So set LOOKUP_COPY_PARM to let reference_binding know that
	     it's being called in that context.  We generalize the above
	     to handle move constructors and template constructors as well;
	     the standardese should soon be updated similarly.  */
	  if (ctype && i == 0 && (len-skip == 1)
	      && DECL_CONSTRUCTOR_P (fn)
	      && parmtype != error_mark_node
	      && (same_type_ignoring_top_level_qualifiers_p
		  (non_reference (parmtype), ctype)))
	    {
	      if (!(flags & LOOKUP_ONLYCONVERTING))
		lflags |= LOOKUP_COPY_PARM;
	      /* We allow user-defined conversions within init-lists, but
		 don't list-initialize the copy parm, as that would mean
		 using two levels of braces for the same type.  */
	      if ((flags & LOOKUP_LIST_INIT_CTOR)
		  && BRACE_ENCLOSED_INITIALIZER_P (arg))
		lflags |= LOOKUP_NO_CONVERSION;
	    }
	  else
	    lflags |= LOOKUP_ONLYCONVERTING;

	  t = implicit_conversion (parmtype, argtype, arg,
				   /*c_cast_p=*/false, lflags, complain);
	  to_type = parmtype;
	}
      else
	{
	  t = build_identity_conv (argtype, arg);
	  t->ellipsis_p = true;
	  to_type = argtype;
	}

      if (t && is_this)
	t->this_p = true;

      convs[i] = t;
      if (! t)
	{
	  viable = 0;
	  reason = arg_conversion_rejection (first_arg, i, argtype, to_type);
	  break;
	}

      if (t->bad_p)
	{
	  viable = -1;
	  reason = bad_arg_conversion_rejection (first_arg, i, arg, to_type);
	}
    }

 out:
  return add_candidate (candidates, fn, orig_first_arg, args, len, convs,
			access_path, conversion_path, viable, reason, flags);
}

/* Create an overload candidate for the conversion function FN which will
   be invoked for expression OBJ, producing a pointer-to-function which
   will in turn be called with the argument list FIRST_ARG/ARGLIST,
   and add it to CANDIDATES.  This does not change ARGLIST.  FLAGS is
   passed on to implicit_conversion.

   Actually, we don't really care about FN; we care about the type it
   converts to.  There may be multiple conversion functions that will
   convert to that type, and we rely on build_user_type_conversion_1 to
   choose the best one; so when we create our candidate, we record the type
   instead of the function.  */

static struct z_candidate *
add_conv_candidate (struct z_candidate **candidates, tree fn, tree obj,
		    tree first_arg, const vec<tree, va_gc> *arglist,
		    tree access_path, tree conversion_path,
		    tsubst_flags_t complain)
{
  tree totype = TREE_TYPE (TREE_TYPE (fn));
  int i, len, viable, flags;
  tree parmlist, parmnode;
  conversion **convs;
  struct rejection_reason *reason;

  for (parmlist = totype; TREE_CODE (parmlist) != FUNCTION_TYPE; )
    parmlist = TREE_TYPE (parmlist);
  parmlist = TYPE_ARG_TYPES (parmlist);

  len = vec_safe_length (arglist) + (first_arg != NULL_TREE ? 1 : 0) + 1;
  convs = alloc_conversions (len);
  parmnode = parmlist;
  viable = 1;
  flags = LOOKUP_IMPLICIT;
  reason = NULL;

  /* Don't bother looking up the same type twice.  */
  if (*candidates && (*candidates)->fn == totype)
    return NULL;

  for (i = 0; i < len; ++i)
    {
      tree arg, argtype, convert_type = NULL_TREE;
      conversion *t;

      if (i == 0)
	arg = obj;
      else if (i == 1 && first_arg != NULL_TREE)
	arg = first_arg;
      else
	arg = (*arglist)[i - (first_arg != NULL_TREE ? 1 : 0) - 1];
      argtype = lvalue_type (arg);

      if (i == 0)
	{
	  t = implicit_conversion (totype, argtype, arg, /*c_cast_p=*/false,
				   flags, complain);
	  convert_type = totype;
	}
      else if (parmnode == void_list_node)
	break;
      else if (parmnode)
	{
	  t = implicit_conversion (TREE_VALUE (parmnode), argtype, arg,
				   /*c_cast_p=*/false, flags, complain);
	  convert_type = TREE_VALUE (parmnode);
	}
      else
	{
	  t = build_identity_conv (argtype, arg);
	  t->ellipsis_p = true;
	  convert_type = argtype;
	}

      convs[i] = t;
      if (! t)
	break;

      if (t->bad_p)
	{
	  viable = -1;
	  reason = bad_arg_conversion_rejection (NULL_TREE, i, arg, convert_type);
	}

      if (i == 0)
	continue;

      if (parmnode)
	parmnode = TREE_CHAIN (parmnode);
    }

  if (i < len
      || ! sufficient_parms_p (parmnode))
    {
      int remaining = remaining_arguments (parmnode);
      viable = 0;
      reason = arity_rejection (NULL_TREE, i + remaining, len);
    }

  return add_candidate (candidates, totype, first_arg, arglist, len, convs,
			access_path, conversion_path, viable, reason, flags);
}

static void
build_builtin_candidate (struct z_candidate **candidates, tree fnname,
			 tree type1, tree type2, tree *args, tree *argtypes,
			 int flags, tsubst_flags_t complain)
{
  conversion *t;
  conversion **convs;
  size_t num_convs;
  int viable = 1, i;
  tree types[2];
  struct rejection_reason *reason = NULL;

  types[0] = type1;
  types[1] = type2;

  num_convs =  args[2] ? 3 : (args[1] ? 2 : 1);
  convs = alloc_conversions (num_convs);

  /* TRUTH_*_EXPR do "contextual conversion to bool", which means explicit
     conversion ops are allowed.  We handle that here by just checking for
     boolean_type_node because other operators don't ask for it.  COND_EXPR
     also does contextual conversion to bool for the first operand, but we
     handle that in build_conditional_expr, and type1 here is operand 2.  */
  if (type1 != boolean_type_node)
    flags |= LOOKUP_ONLYCONVERTING;

  for (i = 0; i < 2; ++i)
    {
      if (! args[i])
	break;

      t = implicit_conversion (types[i], argtypes[i], args[i],
			       /*c_cast_p=*/false, flags, complain);
      if (! t)
	{
	  viable = 0;
	  /* We need something for printing the candidate.  */
	  t = build_identity_conv (types[i], NULL_TREE);
	  reason = arg_conversion_rejection (NULL_TREE, i, argtypes[i],
					     types[i]);
	}
      else if (t->bad_p)
	{
	  viable = 0;
	  reason = bad_arg_conversion_rejection (NULL_TREE, i, args[i],
						 types[i]);
	}
      convs[i] = t;
    }

  /* For COND_EXPR we rearranged the arguments; undo that now.  */
  if (args[2])
    {
      convs[2] = convs[1];
      convs[1] = convs[0];
      t = implicit_conversion (boolean_type_node, argtypes[2], args[2],
			       /*c_cast_p=*/false, flags,
			       complain);
      if (t)
	convs[0] = t;
      else
	{
	  viable = 0;
	  reason = arg_conversion_rejection (NULL_TREE, 0, argtypes[2],
					     boolean_type_node);
	}
    }

  add_candidate (candidates, fnname, /*first_arg=*/NULL_TREE, /*args=*/NULL,
		 num_convs, convs,
		 /*access_path=*/NULL_TREE,
		 /*conversion_path=*/NULL_TREE,
		 viable, reason, flags);
}

static bool
is_complete (tree t)
{
  return COMPLETE_TYPE_P (complete_type (t));
}

/* Returns nonzero if TYPE is a promoted arithmetic type.  */

static bool
promoted_arithmetic_type_p (tree type)
{
  /* [over.built]

     In this section, the term promoted integral type is used to refer
     to those integral types which are preserved by integral promotion
     (including e.g.  int and long but excluding e.g.  char).
     Similarly, the term promoted arithmetic type refers to promoted
     integral types plus floating types.  */
  return ((CP_INTEGRAL_TYPE_P (type)
	   && same_type_p (type_promotes_to (type), type))
	  || TREE_CODE (type) == REAL_TYPE);
}

/* Create any builtin operator overload candidates for the operator in
   question given the converted operand types TYPE1 and TYPE2.  The other
   args are passed through from add_builtin_candidates to
   build_builtin_candidate.

   TYPE1 and TYPE2 may not be permissible, and we must filter them.
   If CODE is requires candidates operands of the same type of the kind
   of which TYPE1 and TYPE2 are, we add both candidates
   CODE (TYPE1, TYPE1) and CODE (TYPE2, TYPE2).  */

static void
add_builtin_candidate (struct z_candidate **candidates, enum tree_code code,
		       enum tree_code code2, tree fnname, tree type1,
		       tree type2, tree *args, tree *argtypes, int flags,
		       tsubst_flags_t complain)
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
	return;
    case POSTINCREMENT_EXPR:
    case PREINCREMENT_EXPR:
      if (ARITHMETIC_TYPE_P (type1) || TYPE_PTROB_P (type1))
	{
	  type1 = build_reference_type (type1);
	  break;
	}
      return;

/* 7 For every cv-qualified or cv-unqualified object type T, there
     exist candidate operator functions of the form

	     T&      operator*(T*);

   8 For every function type T, there exist candidate operator functions of
     the form
	     T&      operator*(T*);  */

    case INDIRECT_REF:
      if (TYPE_PTR_P (type1)
	  && (TYPE_PTROB_P (type1)
	      || TREE_CODE (TREE_TYPE (type1)) == FUNCTION_TYPE))
	break;
      return;

/* 9 For every type T, there exist candidate operator functions of the form
	     T*      operator+(T*);

   10For  every  promoted arithmetic type T, there exist candidate operator
     functions of the form
	     T       operator+(T);
	     T       operator-(T);  */

    case UNARY_PLUS_EXPR: /* unary + */
      if (TYPE_PTR_P (type1))
	break;
    case NEGATE_EXPR:
      if (ARITHMETIC_TYPE_P (type1))
	break;
      return;

/* 11For every promoted integral type T,  there  exist  candidate  operator
     functions of the form
	     T       operator~(T);  */

    case BIT_NOT_EXPR:
      if (INTEGRAL_OR_UNSCOPED_ENUMERATION_TYPE_P (type1))
	break;
      return;

/* 12For every quintuple C1, C2, T, CV1, CV2), where C2 is a class type, C1
     is the same type as C2 or is a derived class of C2, T  is  a  complete
     object type or a function type, and CV1 and CV2 are cv-qualifier-seqs,
     there exist candidate operator functions of the form
	     CV12 T& operator->*(CV1 C1*, CV2 T C2::*);
     where CV12 is the union of CV1 and CV2.  */

    case MEMBER_REF:
      if (TYPE_PTR_P (type1) && TYPE_PTRMEM_P (type2))
	{
	  tree c1 = TREE_TYPE (type1);
	  tree c2 = TYPE_PTRMEM_CLASS_TYPE (type2);

	  if (MAYBE_CLASS_TYPE_P (c1) && DERIVED_FROM_P (c2, c1)
	      && (TYPE_PTRMEMFUNC_P (type2)
		  || is_complete (TYPE_PTRMEM_POINTED_TO_TYPE (type2))))
	    break;
	}
      return;

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

   16For every pointer or enumeration type T, there exist candidate operator
     functions of the form
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
      if (TYPE_PTROB_P (type1)
	  && INTEGRAL_OR_UNSCOPED_ENUMERATION_TYPE_P (type2))
	{
	  type2 = ptrdiff_type_node;
	  break;
	}
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
      if (ARITHMETIC_TYPE_P (type1) && ARITHMETIC_TYPE_P (type2))
	break;
      return;

    case EQ_EXPR:
    case NE_EXPR:
      if ((TYPE_PTRMEMFUNC_P (type1) && TYPE_PTRMEMFUNC_P (type2))
	  || (TYPE_PTRDATAMEM_P (type1) && TYPE_PTRDATAMEM_P (type2)))
	break;
      if (TYPE_PTRMEM_P (type1) && null_ptr_cst_p (args[1]))
	{
	  type2 = type1;
	  break;
	}
      if (TYPE_PTRMEM_P (type2) && null_ptr_cst_p (args[0]))
	{
	  type1 = type2;
	  break;
	}
      /* Fall through.  */
    case LT_EXPR:
    case GT_EXPR:
    case LE_EXPR:
    case GE_EXPR:
    case MAX_EXPR:
    case MIN_EXPR:
      if (ARITHMETIC_TYPE_P (type1) && ARITHMETIC_TYPE_P (type2))
	break;
      if (TYPE_PTR_P (type1) && TYPE_PTR_P (type2))
	break;
      if (TREE_CODE (type1) == ENUMERAL_TYPE 
	  && TREE_CODE (type2) == ENUMERAL_TYPE)
	break;
      if (TYPE_PTR_P (type1) 
	  && null_ptr_cst_p (args[1]))
	{
	  type2 = type1;
	  break;
	}
      if (null_ptr_cst_p (args[0]) 
	  && TYPE_PTR_P (type2))
	{
	  type1 = type2;
	  break;
	}
      return;

    case PLUS_EXPR:
      if (ARITHMETIC_TYPE_P (type1) && ARITHMETIC_TYPE_P (type2))
	break;
    case ARRAY_REF:
      if (INTEGRAL_OR_UNSCOPED_ENUMERATION_TYPE_P (type1) && TYPE_PTROB_P (type2))
	{
	  type1 = ptrdiff_type_node;
	  break;
	}
      if (TYPE_PTROB_P (type1) && INTEGRAL_OR_UNSCOPED_ENUMERATION_TYPE_P (type2))
	{
	  type2 = ptrdiff_type_node;
	  break;
	}
      return;

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
      if (INTEGRAL_OR_UNSCOPED_ENUMERATION_TYPE_P (type1) && INTEGRAL_OR_UNSCOPED_ENUMERATION_TYPE_P (type2))
	break;
      return;

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
	  if (TYPE_PTROB_P (type1) && INTEGRAL_OR_UNSCOPED_ENUMERATION_TYPE_P (type2))
	    {
	      type2 = ptrdiff_type_node;
	      break;
	    }
	case MULT_EXPR:
	case TRUNC_DIV_EXPR:
	  if (ARITHMETIC_TYPE_P (type1) && ARITHMETIC_TYPE_P (type2))
	    break;
	  return;

	case TRUNC_MOD_EXPR:
	case BIT_AND_EXPR:
	case BIT_IOR_EXPR:
	case BIT_XOR_EXPR:
	case LSHIFT_EXPR:
	case RSHIFT_EXPR:
	  if (INTEGRAL_OR_UNSCOPED_ENUMERATION_TYPE_P (type1) && INTEGRAL_OR_UNSCOPED_ENUMERATION_TYPE_P (type2))
	    break;
	  return;

	case NOP_EXPR:
	  if (ARITHMETIC_TYPE_P (type1) && ARITHMETIC_TYPE_P (type2))
	    break;
	  if ((TYPE_PTRMEMFUNC_P (type1) && TYPE_PTRMEMFUNC_P (type2))
	      || (TYPE_PTR_P (type1) && TYPE_PTR_P (type2))
	      || (TYPE_PTRDATAMEM_P (type1) && TYPE_PTRDATAMEM_P (type2))
	      || ((TYPE_PTRMEMFUNC_P (type1)
		   || TYPE_PTR_P (type1))
		  && null_ptr_cst_p (args[1])))
	    {
	      type2 = type1;
	      break;
	    }
	  return;

	default:
	  gcc_unreachable ();
	}
      type1 = build_reference_type (type1);
      break;

    case COND_EXPR:
      /* [over.built]

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
      if (!TYPE_PTR_OR_PTRMEM_P (type1) || !TYPE_PTR_OR_PTRMEM_P (type2))
	return;

      /* We don't check that the two types are the same; the logic
	 below will actually create two candidates; one in which both
	 parameter types are TYPE1, and one in which both parameter
	 types are TYPE2.  */
      break;

    case REALPART_EXPR:
    case IMAGPART_EXPR:
      if (ARITHMETIC_TYPE_P (type1))
	break;
      return;
 
    default:
      gcc_unreachable ();
    }

  /* Make sure we don't create builtin candidates with dependent types.  */
  bool u1 = uses_template_parms (type1);
  bool u2 = type2 ? uses_template_parms (type2) : false;
  if (u1 || u2)
    {
      /* Try to recover if one of the types is non-dependent.  But if
	 there's only one type, there's nothing we can do.  */
      if (!type2)
	return;
      /* And we lose if both are dependent.  */
      if (u1 && u2)
	return;
      /* Or if they have different forms.  */
      if (TREE_CODE (type1) != TREE_CODE (type2))
	return;

      if (u1 && !u2)
	type1 = type2;
      else if (u2 && !u1)
	type2 = type1;
    }

  /* If we're dealing with two pointer types or two enumeral types,
     we need candidates for both of them.  */
  if (type2 && !same_type_p (type1, type2)
      && TREE_CODE (type1) == TREE_CODE (type2)
      && (TREE_CODE (type1) == REFERENCE_TYPE
	  || (TYPE_PTR_P (type1) && TYPE_PTR_P (type2))
	  || (TYPE_PTRDATAMEM_P (type1) && TYPE_PTRDATAMEM_P (type2))
	  || TYPE_PTRMEMFUNC_P (type1)
	  || MAYBE_CLASS_TYPE_P (type1)
	  || TREE_CODE (type1) == ENUMERAL_TYPE))
    {
      if (TYPE_PTR_OR_PTRMEM_P (type1))
	{
	  tree cptype = composite_pointer_type (type1, type2,
						error_mark_node,
						error_mark_node,
						CPO_CONVERSION,
						tf_none);
	  if (cptype != error_mark_node)
	    {
	      build_builtin_candidate
		(candidates, fnname, cptype, cptype, args, argtypes,
		 flags, complain);
	      return;
	    }
	}

      build_builtin_candidate
	(candidates, fnname, type1, type1, args, argtypes, flags, complain);
      build_builtin_candidate
	(candidates, fnname, type2, type2, args, argtypes, flags, complain);
      return;
    }

  build_builtin_candidate
    (candidates, fnname, type1, type2, args, argtypes, flags, complain);
}

tree
type_decays_to (tree type)
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
      all of these, but I'm trying not to...

   Here we generate a superset of the possible candidates for this particular
   case.  That is a subset of the full set the standard defines, plus some
   other cases which the standard disallows. add_builtin_candidate will
   filter out the invalid set.  */

static void
add_builtin_candidates (struct z_candidate **candidates, enum tree_code code,
			enum tree_code code2, tree fnname, tree *args,
			int flags, tsubst_flags_t complain)
{
  int ref1, i;
  int enum_p = 0;
  tree type, argtypes[3], t;
  /* TYPES[i] is the set of possible builtin-operator parameter types
     we will consider for the Ith argument.  */
  vec<tree, va_gc> *types[2];
  unsigned ix;

  for (i = 0; i < 3; ++i)
    {
      if (args[i])
	argtypes[i] = unlowered_expr_type (args[i]);
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
      build_builtin_candidate
	(candidates, fnname, boolean_type_node,
	 NULL_TREE, args, argtypes, flags, complain);
      return;

    case TRUTH_ORIF_EXPR:
    case TRUTH_ANDIF_EXPR:
      build_builtin_candidate
	(candidates, fnname, boolean_type_node,
	 boolean_type_node, args, argtypes, flags, complain);
      return;

    case ADDR_EXPR:
    case COMPOUND_EXPR:
    case COMPONENT_REF:
      return;

    case COND_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
      enum_p = 1;
      /* Fall through.  */

    default:
      ref1 = 0;
    }

  types[0] = make_tree_vector ();
  types[1] = make_tree_vector ();

  for (i = 0; i < 2; ++i)
    {
      if (! args[i])
	;
      else if (MAYBE_CLASS_TYPE_P (argtypes[i]))
	{
	  tree convs;

	  if (i == 0 && code == MODIFY_EXPR && code2 == NOP_EXPR)
	    return;

	  convs = lookup_conversions (argtypes[i]);

	  if (code == COND_EXPR)
	    {
	      if (real_lvalue_p (args[i]))
		vec_safe_push (types[i], build_reference_type (argtypes[i]));

	      vec_safe_push (types[i], TYPE_MAIN_VARIANT (argtypes[i]));
	    }

	  else if (! convs)
	    return;

	  for (; convs; convs = TREE_CHAIN (convs))
	    {
	      type = TREE_TYPE (convs);

	      if (i == 0 && ref1
		  && (TREE_CODE (type) != REFERENCE_TYPE
		      || CP_TYPE_CONST_P (TREE_TYPE (type))))
		continue;

	      if (code == COND_EXPR && TREE_CODE (type) == REFERENCE_TYPE)
		vec_safe_push (types[i], type);

	      type = non_reference (type);
	      if (i != 0 || ! ref1)
		{
		  type = cv_unqualified (type_decays_to (type));
		  if (enum_p && TREE_CODE (type) == ENUMERAL_TYPE)
		    vec_safe_push (types[i], type);
		  if (INTEGRAL_OR_UNSCOPED_ENUMERATION_TYPE_P (type))
		    type = type_promotes_to (type);
		}

	      if (! vec_member (type, types[i]))
		vec_safe_push (types[i], type);
	    }
	}
      else
	{
	  if (code == COND_EXPR && real_lvalue_p (args[i]))
	    vec_safe_push (types[i], build_reference_type (argtypes[i]));
	  type = non_reference (argtypes[i]);
	  if (i != 0 || ! ref1)
	    {
	      type = cv_unqualified (type_decays_to (type));
	      if (enum_p && UNSCOPED_ENUM_P (type))
		vec_safe_push (types[i], type);
	      if (INTEGRAL_OR_UNSCOPED_ENUMERATION_TYPE_P (type))
		type = type_promotes_to (type);
	    }
	  vec_safe_push (types[i], type);
	}
    }

  /* Run through the possible parameter types of both arguments,
     creating candidates with those parameter types.  */
  FOR_EACH_VEC_ELT_REVERSE (*(types[0]), ix, t)
    {
      unsigned jx;
      tree u;

      if (!types[1]->is_empty ())
	FOR_EACH_VEC_ELT_REVERSE (*(types[1]), jx, u)
	  add_builtin_candidate
	    (candidates, code, code2, fnname, t,
	     u, args, argtypes, flags, complain);
      else
	add_builtin_candidate
	  (candidates, code, code2, fnname, t,
	   NULL_TREE, args, argtypes, flags, complain);
    }

  release_tree_vector (types[0]);
  release_tree_vector (types[1]);
}


/* If TMPL can be successfully instantiated as indicated by
   EXPLICIT_TARGS and ARGLIST, adds the instantiation to CANDIDATES.

   TMPL is the template.  EXPLICIT_TARGS are any explicit template
   arguments.  ARGLIST is the arguments provided at the call-site.
   This does not change ARGLIST.  The RETURN_TYPE is the desired type
   for conversion operators.  If OBJ is NULL_TREE, FLAGS and CTYPE are
   as for add_function_candidate.  If an OBJ is supplied, FLAGS and
   CTYPE are ignored, and OBJ is as for add_conv_candidate.  */

static struct z_candidate*
add_template_candidate_real (struct z_candidate **candidates, tree tmpl,
			     tree ctype, tree explicit_targs, tree first_arg,
			     const vec<tree, va_gc> *arglist, tree return_type,
			     tree access_path, tree conversion_path,
			     int flags, tree obj, unification_kind_t strict,
			     tsubst_flags_t complain)
{
  int ntparms = DECL_NTPARMS (tmpl);
  tree targs = make_tree_vec (ntparms);
  unsigned int len = vec_safe_length (arglist);
  unsigned int nargs = (first_arg == NULL_TREE ? 0 : 1) + len;
  unsigned int skip_without_in_chrg = 0;
  tree first_arg_without_in_chrg = first_arg;
  tree *args_without_in_chrg;
  unsigned int nargs_without_in_chrg;
  unsigned int ia, ix;
  tree arg;
  struct z_candidate *cand;
  tree fn;
  struct rejection_reason *reason = NULL;
  int errs;

  /* We don't do deduction on the in-charge parameter, the VTT
     parameter or 'this'.  */
  if (DECL_NONSTATIC_MEMBER_FUNCTION_P (tmpl))
    {
      if (first_arg_without_in_chrg != NULL_TREE)
	first_arg_without_in_chrg = NULL_TREE;
      else
	++skip_without_in_chrg;
    }

  if ((DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P (tmpl)
       || DECL_BASE_CONSTRUCTOR_P (tmpl))
      && CLASSTYPE_VBASECLASSES (DECL_CONTEXT (tmpl)))
    {
      if (first_arg_without_in_chrg != NULL_TREE)
	first_arg_without_in_chrg = NULL_TREE;
      else
	++skip_without_in_chrg;
    }

  if (len < skip_without_in_chrg)
    return NULL;

  nargs_without_in_chrg = ((first_arg_without_in_chrg != NULL_TREE ? 1 : 0)
			   + (len - skip_without_in_chrg));
  args_without_in_chrg = XALLOCAVEC (tree, nargs_without_in_chrg);
  ia = 0;
  if (first_arg_without_in_chrg != NULL_TREE)
    {
      args_without_in_chrg[ia] = first_arg_without_in_chrg;
      ++ia;
    }
  for (ix = skip_without_in_chrg;
       vec_safe_iterate (arglist, ix, &arg);
       ++ix)
    {
      args_without_in_chrg[ia] = arg;
      ++ia;
    }
  gcc_assert (ia == nargs_without_in_chrg);

  errs = errorcount+sorrycount;
  fn = fn_type_unification (tmpl, explicit_targs, targs,
			    args_without_in_chrg,
			    nargs_without_in_chrg,
			    return_type, strict, flags, false,
			    complain & tf_decltype);

  if (fn == error_mark_node)
    {
      /* Don't repeat unification later if it already resulted in errors.  */
      if (errorcount+sorrycount == errs)
	reason = template_unification_rejection (tmpl, explicit_targs,
						 targs, args_without_in_chrg,
						 nargs_without_in_chrg,
						 return_type, strict, flags);
      else
	reason = template_unification_error_rejection ();
      goto fail;
    }

  /* In [class.copy]:

       A member function template is never instantiated to perform the
       copy of a class object to an object of its class type.

     It's a little unclear what this means; the standard explicitly
     does allow a template to be used to copy a class.  For example,
     in:

       struct A {
	 A(A&);
	 template <class T> A(const T&);
       };
       const A f ();
       void g () { A a (f ()); }

     the member template will be used to make the copy.  The section
     quoted above appears in the paragraph that forbids constructors
     whose only parameter is (a possibly cv-qualified variant of) the
     class type, and a logical interpretation is that the intent was
     to forbid the instantiation of member templates which would then
     have that form.  */
  if (DECL_CONSTRUCTOR_P (fn) && nargs == 2)
    {
      tree arg_types = FUNCTION_FIRST_USER_PARMTYPE (fn);
      if (arg_types && same_type_p (TYPE_MAIN_VARIANT (TREE_VALUE (arg_types)),
				    ctype))
	{
	  reason = invalid_copy_with_fn_template_rejection ();
	  goto fail;
	}
    }

  if (obj != NULL_TREE)
    /* Aha, this is a conversion function.  */
    cand = add_conv_candidate (candidates, fn, obj, first_arg, arglist,
			       access_path, conversion_path, complain);
  else
    cand = add_function_candidate (candidates, fn, ctype,
				   first_arg, arglist, access_path,
				   conversion_path, flags, complain);
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
    cand->template_decl = build_template_info (tmpl, targs);
  else
    cand->template_decl = DECL_TEMPLATE_INFO (fn);
  cand->explicit_targs = explicit_targs;

  return cand;
 fail:
  return add_candidate (candidates, tmpl, first_arg, arglist, nargs, NULL,
			access_path, conversion_path, 0, reason, flags);
}


static struct z_candidate *
add_template_candidate (struct z_candidate **candidates, tree tmpl, tree ctype,
			tree explicit_targs, tree first_arg,
			const vec<tree, va_gc> *arglist, tree return_type,
			tree access_path, tree conversion_path, int flags,
			unification_kind_t strict, tsubst_flags_t complain)
{
  return
    add_template_candidate_real (candidates, tmpl, ctype,
				 explicit_targs, first_arg, arglist,
				 return_type, access_path, conversion_path,
				 flags, NULL_TREE, strict, complain);
}


static struct z_candidate *
add_template_conv_candidate (struct z_candidate **candidates, tree tmpl,
			     tree obj, tree first_arg,
			     const vec<tree, va_gc> *arglist,
			     tree return_type, tree access_path,
			     tree conversion_path, tsubst_flags_t complain)
{
  return
    add_template_candidate_real (candidates, tmpl, NULL_TREE, NULL_TREE,
				 first_arg, arglist, return_type, access_path,
				 conversion_path, 0, obj, DEDUCE_CONV,
				 complain);
}

/* The CANDS are the set of candidates that were considered for
   overload resolution.  Return the set of viable candidates, or CANDS
   if none are viable.  If any of the candidates were viable, set
   *ANY_VIABLE_P to true.  STRICT_P is true if a candidate should be
   considered viable only if it is strictly viable.  */

static struct z_candidate*
splice_viable (struct z_candidate *cands,
	       bool strict_p,
	       bool *any_viable_p)
{
  struct z_candidate *viable;
  struct z_candidate **last_viable;
  struct z_candidate **cand;
  bool found_strictly_viable = false;

  /* Be strict inside templates, since build_over_call won't actually
     do the conversions to get pedwarns.  */
  if (processing_template_decl)
    strict_p = true;

  viable = NULL;
  last_viable = &viable;
  *any_viable_p = false;

  cand = &cands;
  while (*cand)
    {
      struct z_candidate *c = *cand;
      if (!strict_p
	  && (c->viable == 1 || TREE_CODE (c->fn) == TEMPLATE_DECL))
	{
	  /* Be strict in the presence of a viable candidate.  Also if
	     there are template candidates, so that we get deduction errors
	     for them instead of silently preferring a bad conversion.  */
	  strict_p = true;
	  if (viable && !found_strictly_viable)
	    {
	      /* Put any spliced near matches back onto the main list so
		 that we see them if there is no strict match.  */
	      *any_viable_p = false;
	      *last_viable = cands;
	      cands = viable;
	      viable = NULL;
	      last_viable = &viable;
	    }
	}

      if (strict_p ? c->viable == 1 : c->viable)
	{
	  *last_viable = c;
	  *cand = c->next;
	  c->next = NULL;
	  last_viable = &c->next;
	  *any_viable_p = true;
	  if (c->viable == 1)
	    found_strictly_viable = true;
	}
      else
	cand = &c->next;
    }

  return viable ? viable : cands;
}

static bool
any_strictly_viable (struct z_candidate *cands)
{
  for (; cands; cands = cands->next)
    if (cands->viable == 1)
      return true;
  return false;
}

/* OBJ is being used in an expression like "OBJ.f (...)".  In other
   words, it is about to become the "this" pointer for a member
   function call.  Take the address of the object.  */

static tree
build_this (tree obj)
{
  /* In a template, we are only concerned about the type of the
     expression, so we can take a shortcut.  */
  if (processing_template_decl)
    return build_address (obj);

  return cp_build_addr_expr (obj, tf_warning_or_error);
}

/* Returns true iff functions are equivalent. Equivalent functions are
   not '==' only if one is a function-local extern function or if
   both are extern "C".  */

static inline int
equal_functions (tree fn1, tree fn2)
{
  if (TREE_CODE (fn1) != TREE_CODE (fn2))
    return 0;
  if (TREE_CODE (fn1) == TEMPLATE_DECL)
    return fn1 == fn2;
  if (DECL_LOCAL_FUNCTION_P (fn1) || DECL_LOCAL_FUNCTION_P (fn2)
      || DECL_EXTERN_C_FUNCTION_P (fn1))
    return decls_match (fn1, fn2);
  return fn1 == fn2;
}

/* Print information about a candidate being rejected due to INFO.  */

static void
print_conversion_rejection (location_t loc, struct conversion_info *info)
{
  tree from = info->from;
  if (!TYPE_P (from))
    from = lvalue_type (from);
  if (info->n_arg == -1)
    {
      /* Conversion of implicit `this' argument failed.  */
      if (!TYPE_P (info->from))
	/* A bad conversion for 'this' must be discarding cv-quals.  */
	inform (loc, "  passing %qT as %<this%> "
		"argument discards qualifiers",
		from);
      else
	inform (loc, "  no known conversion for implicit "
		"%<this%> parameter from %qT to %qT",
		from, info->to_type);
    }
  else if (!TYPE_P (info->from))
    {
      if (info->n_arg >= 0)
	inform (loc, "  conversion of argument %d would be ill-formed:",
		info->n_arg + 1);
      perform_implicit_conversion (info->to_type, info->from,
				   tf_warning_or_error);
    }
  else if (info->n_arg == -2)
    /* Conversion of conversion function return value failed.  */
    inform (loc, "  no known conversion from %qT to %qT",
	    from, info->to_type);
  else
    inform (loc, "  no known conversion for argument %d from %qT to %qT",
	    info->n_arg + 1, from, info->to_type);
}

/* Print information about a candidate with WANT parameters and we found
   HAVE.  */

static void
print_arity_information (location_t loc, unsigned int have, unsigned int want)
{
  inform_n (loc, want,
	    "  candidate expects %d argument, %d provided",
	    "  candidate expects %d arguments, %d provided",
	    want, have);
}

/* Print information about one overload candidate CANDIDATE.  MSGSTR
   is the text to print before the candidate itself.

   NOTE: Unlike most diagnostic functions in GCC, MSGSTR is expected
   to have been run through gettext by the caller.  This wart makes
   life simpler in print_z_candidates and for the translators.  */

static void
print_z_candidate (location_t loc, const char *msgstr,
		   struct z_candidate *candidate)
{
  const char *msg = (msgstr == NULL
		     ? ""
		     : ACONCAT ((msgstr, " ", NULL)));
  location_t cloc = location_of (candidate->fn);

  if (identifier_p (candidate->fn))
    {
      cloc = loc;
      if (candidate->num_convs == 3)
	inform (cloc, "%s%D(%T, %T, %T) <built-in>", msg, candidate->fn,
		candidate->convs[0]->type,
		candidate->convs[1]->type,
		candidate->convs[2]->type);
      else if (candidate->num_convs == 2)
	inform (cloc, "%s%D(%T, %T) <built-in>", msg, candidate->fn,
		candidate->convs[0]->type,
		candidate->convs[1]->type);
      else
	inform (cloc, "%s%D(%T) <built-in>", msg, candidate->fn,
		candidate->convs[0]->type);
    }
  else if (TYPE_P (candidate->fn))
    inform (cloc, "%s%T <conversion>", msg, candidate->fn);
  else if (candidate->viable == -1)
    inform (cloc, "%s%#D <near match>", msg, candidate->fn);
  else if (DECL_DELETED_FN (candidate->fn))
    inform (cloc, "%s%#D <deleted>", msg, candidate->fn);
  else
    inform (cloc, "%s%#D", msg, candidate->fn);
  /* Give the user some information about why this candidate failed.  */
  if (candidate->reason != NULL)
    {
      struct rejection_reason *r = candidate->reason;

      switch (r->code)
	{
	case rr_arity:
	  print_arity_information (cloc, r->u.arity.actual,
				   r->u.arity.expected);
	  break;
	case rr_arg_conversion:
	  print_conversion_rejection (cloc, &r->u.conversion);
	  break;
	case rr_bad_arg_conversion:
	  print_conversion_rejection (cloc, &r->u.bad_conversion);
	  break;
	case rr_explicit_conversion:
	  inform (cloc, "  return type %qT of explicit conversion function "
		  "cannot be converted to %qT with a qualification "
		  "conversion", r->u.conversion.from,
		  r->u.conversion.to_type);
	  break;
	case rr_template_conversion:
	  inform (cloc, "  conversion from return type %qT of template "
		  "conversion function specialization to %qT is not an "
		  "exact match", r->u.conversion.from,
		  r->u.conversion.to_type);
	  break;
	case rr_template_unification:
	  /* We use template_unification_error_rejection if unification caused
	     actual non-SFINAE errors, in which case we don't need to repeat
	     them here.  */
	  if (r->u.template_unification.tmpl == NULL_TREE)
	    {
	      inform (cloc, "  substitution of deduced template arguments "
		      "resulted in errors seen above");
	      break;
	    }
	  /* Re-run template unification with diagnostics.  */
	  inform (cloc, "  template argument deduction/substitution failed:");
	  fn_type_unification (r->u.template_unification.tmpl,
			       r->u.template_unification.explicit_targs,
			       (make_tree_vec
				(r->u.template_unification.num_targs)),
			       r->u.template_unification.args,
			       r->u.template_unification.nargs,
			       r->u.template_unification.return_type,
			       r->u.template_unification.strict,
			       r->u.template_unification.flags,
			       true, false);
	  break;
	case rr_invalid_copy:
	  inform (cloc,
		  "  a constructor taking a single argument of its own "
		  "class type is invalid");
	  break;
	case rr_none:
	default:
	  /* This candidate didn't have any issues or we failed to
	     handle a particular code.  Either way...  */
	  gcc_unreachable ();
	}
    }
}

static void
print_z_candidates (location_t loc, struct z_candidate *candidates)
{
  struct z_candidate *cand1;
  struct z_candidate **cand2;
  int n_candidates;

  if (!candidates)
    return;

  /* Remove non-viable deleted candidates.  */
  cand1 = candidates;
  for (cand2 = &cand1; *cand2; )
    {
      if (TREE_CODE ((*cand2)->fn) == FUNCTION_DECL
	  && !(*cand2)->viable
	  && DECL_DELETED_FN ((*cand2)->fn))
	*cand2 = (*cand2)->next;
      else
	cand2 = &(*cand2)->next;
    }
  /* ...if there are any non-deleted ones.  */
  if (cand1)
    candidates = cand1;

  /* There may be duplicates in the set of candidates.  We put off
     checking this condition as long as possible, since we have no way
     to eliminate duplicates from a set of functions in less than n^2
     time.  Now we are about to emit an error message, so it is more
     permissible to go slowly.  */
  for (cand1 = candidates; cand1; cand1 = cand1->next)
    {
      tree fn = cand1->fn;
      /* Skip builtin candidates and conversion functions.  */
      if (!DECL_P (fn))
	continue;
      cand2 = &cand1->next;
      while (*cand2)
	{
	  if (DECL_P ((*cand2)->fn)
	      && equal_functions (fn, (*cand2)->fn))
	    *cand2 = (*cand2)->next;
	  else
	    cand2 = &(*cand2)->next;
	}
    }

  for (n_candidates = 0, cand1 = candidates; cand1; cand1 = cand1->next)
    n_candidates++;

  for (; candidates; candidates = candidates->next)
    print_z_candidate (loc, "candidate:", candidates);
}

/* USER_SEQ is a user-defined conversion sequence, beginning with a
   USER_CONV.  STD_SEQ is the standard conversion sequence applied to
   the result of the conversion function to convert it to the final
   desired type.  Merge the two sequences into a single sequence,
   and return the merged sequence.  */

static conversion *
merge_conversion_sequences (conversion *user_seq, conversion *std_seq)
{
  conversion **t;
  bool bad = user_seq->bad_p;

  gcc_assert (user_seq->kind == ck_user);

  /* Find the end of the second conversion sequence.  */
  for (t = &std_seq; (*t)->kind != ck_identity; t = &((*t)->u.next))
    {
      /* The entire sequence is a user-conversion sequence.  */
      (*t)->user_conv_p = true;
      if (bad)
	(*t)->bad_p = true;
    }

  /* Replace the identity conversion with the user conversion
     sequence.  */
  *t = user_seq;

  return std_seq;
}

/* Handle overload resolution for initializing an object of class type from
   an initializer list.  First we look for a suitable constructor that
   takes a std::initializer_list; if we don't find one, we then look for a
   non-list constructor.

   Parameters are as for add_candidates, except that the arguments are in
   the form of a CONSTRUCTOR (the initializer list) rather than a vector, and
   the RETURN_TYPE parameter is replaced by TOTYPE, the desired type.  */

static void
add_list_candidates (tree fns, tree first_arg,
		     tree init_list, tree totype,
		     tree explicit_targs, bool template_only,
		     tree conversion_path, tree access_path,
		     int flags,
		     struct z_candidate **candidates,
		     tsubst_flags_t complain)
{
  vec<tree, va_gc> *args;

  gcc_assert (*candidates == NULL);

  /* We're looking for a ctor for list-initialization.  */
  flags |= LOOKUP_LIST_INIT_CTOR;
  /* And we don't allow narrowing conversions.  We also use this flag to
     avoid the copy constructor call for copy-list-initialization.  */
  flags |= LOOKUP_NO_NARROWING;

  /* Always use the default constructor if the list is empty (DR 990).  */
  if (CONSTRUCTOR_NELTS (init_list) == 0
      && TYPE_HAS_DEFAULT_CONSTRUCTOR (totype))
    ;
  /* If the class has a list ctor, try passing the list as a single
     argument first, but only consider list ctors.  */
  else if (TYPE_HAS_LIST_CTOR (totype))
    {
      flags |= LOOKUP_LIST_ONLY;
      args = make_tree_vector_single (init_list);
      add_candidates (fns, first_arg, args, NULL_TREE,
		      explicit_targs, template_only, conversion_path,
		      access_path, flags, candidates, complain);
      if (any_strictly_viable (*candidates))
	return;
    }

  args = ctor_to_vec (init_list);

  /* We aren't looking for list-ctors anymore.  */
  flags &= ~LOOKUP_LIST_ONLY;
  /* We allow more user-defined conversions within an init-list.  */
  flags &= ~LOOKUP_NO_CONVERSION;

  add_candidates (fns, first_arg, args, NULL_TREE,
		  explicit_targs, template_only, conversion_path,
		  access_path, flags, candidates, complain);
}

/* Returns the best overload candidate to perform the requested
   conversion.  This function is used for three the overloading situations
   described in [over.match.copy], [over.match.conv], and [over.match.ref].
   If TOTYPE is a REFERENCE_TYPE, we're trying to find a direct binding as
   per [dcl.init.ref], so we ignore temporary bindings.  */

static struct z_candidate *
build_user_type_conversion_1 (tree totype, tree expr, int flags,
			      tsubst_flags_t complain)
{
  struct z_candidate *candidates, *cand;
  tree fromtype;
  tree ctors = NULL_TREE;
  tree conv_fns = NULL_TREE;
  conversion *conv = NULL;
  tree first_arg = NULL_TREE;
  vec<tree, va_gc> *args = NULL;
  bool any_viable_p;
  int convflags;

  if (!expr)
    return NULL;

  fromtype = TREE_TYPE (expr);

  /* We represent conversion within a hierarchy using RVALUE_CONV and
     BASE_CONV, as specified by [over.best.ics]; these become plain
     constructor calls, as specified in [dcl.init].  */
  gcc_assert (!MAYBE_CLASS_TYPE_P (fromtype) || !MAYBE_CLASS_TYPE_P (totype)
	      || !DERIVED_FROM_P (totype, fromtype));

  if (MAYBE_CLASS_TYPE_P (totype))
    /* Use lookup_fnfields_slot instead of lookup_fnfields to avoid
       creating a garbage BASELINK; constructors can't be inherited.  */
    ctors = lookup_fnfields_slot (totype, complete_ctor_identifier);

  if (MAYBE_CLASS_TYPE_P (fromtype))
    {
      tree to_nonref = non_reference (totype);
      if (same_type_ignoring_top_level_qualifiers_p (to_nonref, fromtype) ||
	  (CLASS_TYPE_P (to_nonref) && CLASS_TYPE_P (fromtype)
	   && DERIVED_FROM_P (to_nonref, fromtype)))
	{
	  /* [class.conv.fct] A conversion function is never used to
	     convert a (possibly cv-qualified) object to the (possibly
	     cv-qualified) same object type (or a reference to it), to a
	     (possibly cv-qualified) base class of that type (or a
	     reference to it)...  */
	}
      else
	conv_fns = lookup_conversions (fromtype);
    }

  candidates = 0;
  flags |= LOOKUP_NO_CONVERSION;
  if (BRACE_ENCLOSED_INITIALIZER_P (expr))
    flags |= LOOKUP_NO_NARROWING;

  /* It's OK to bind a temporary for converting constructor arguments, but
     not in converting the return value of a conversion operator.  */
  convflags = ((flags & LOOKUP_NO_TEMP_BIND) | LOOKUP_NO_CONVERSION);
  flags &= ~LOOKUP_NO_TEMP_BIND;

  if (ctors)
    {
      int ctorflags = flags;

      first_arg = build_dummy_object (totype);

      /* We should never try to call the abstract or base constructor
	 from here.  */
      gcc_assert (!DECL_HAS_IN_CHARGE_PARM_P (OVL_CURRENT (ctors))
		  && !DECL_HAS_VTT_PARM_P (OVL_CURRENT (ctors)));

      if (BRACE_ENCLOSED_INITIALIZER_P (expr))
	{
	  /* List-initialization.  */
	  add_list_candidates (ctors, first_arg, expr, totype, NULL_TREE,
			       false, TYPE_BINFO (totype), TYPE_BINFO (totype),
			       ctorflags, &candidates, complain);
	}
      else
	{
	  args = make_tree_vector_single (expr);
	  add_candidates (ctors, first_arg, args, NULL_TREE, NULL_TREE, false,
			  TYPE_BINFO (totype), TYPE_BINFO (totype),
			  ctorflags, &candidates, complain);
	}

      for (cand = candidates; cand; cand = cand->next)
	{
	  cand->second_conv = build_identity_conv (totype, NULL_TREE);

	  /* If totype isn't a reference, and LOOKUP_NO_TEMP_BIND isn't
	     set, then this is copy-initialization.  In that case, "The
	     result of the call is then used to direct-initialize the
	     object that is the destination of the copy-initialization."
	     [dcl.init]

	     We represent this in the conversion sequence with an
	     rvalue conversion, which means a constructor call.  */
	  if (TREE_CODE (totype) != REFERENCE_TYPE
	      && !(convflags & LOOKUP_NO_TEMP_BIND))
	    cand->second_conv
	      = build_conv (ck_rvalue, totype, cand->second_conv);
	}
    }

  if (conv_fns)
    first_arg = expr;

  for (; conv_fns; conv_fns = TREE_CHAIN (conv_fns))
    {
      tree conversion_path = TREE_PURPOSE (conv_fns);
      struct z_candidate *old_candidates;

      /* If we are called to convert to a reference type, we are trying to
	 find a direct binding, so don't even consider temporaries.  If
	 we don't find a direct binding, the caller will try again to
	 look for a temporary binding.  */
      if (TREE_CODE (totype) == REFERENCE_TYPE)
	convflags |= LOOKUP_NO_TEMP_BIND;

      old_candidates = candidates;
      add_candidates (TREE_VALUE (conv_fns), first_arg, NULL, totype,
		      NULL_TREE, false,
		      conversion_path, TYPE_BINFO (fromtype),
		      flags, &candidates, complain);

      for (cand = candidates; cand != old_candidates; cand = cand->next)
	{
	  tree rettype = TREE_TYPE (TREE_TYPE (cand->fn));
	  conversion *ics
	    = implicit_conversion (totype,
				   rettype,
				   0,
				   /*c_cast_p=*/false, convflags,
				   complain);

	  /* If LOOKUP_NO_TEMP_BIND isn't set, then this is
	     copy-initialization.  In that case, "The result of the
	     call is then used to direct-initialize the object that is
	     the destination of the copy-initialization."  [dcl.init]

	     We represent this in the conversion sequence with an
	     rvalue conversion, which means a constructor call.  But
	     don't add a second rvalue conversion if there's already
	     one there.  Which there really shouldn't be, but it's
	     harmless since we'd add it here anyway. */
	  if (ics && MAYBE_CLASS_TYPE_P (totype) && ics->kind != ck_rvalue
	      && !(convflags & LOOKUP_NO_TEMP_BIND))
	    ics = build_conv (ck_rvalue, totype, ics);

	  cand->second_conv = ics;

	  if (!ics)
	    {
	      cand->viable = 0;
	      cand->reason = arg_conversion_rejection (NULL_TREE, -2,
						       rettype, totype);
	    }
	  else if (DECL_NONCONVERTING_P (cand->fn)
		   && ics->rank > cr_exact)
	    {
	      /* 13.3.1.5: For direct-initialization, those explicit
		 conversion functions that are not hidden within S and
		 yield type T or a type that can be converted to type T
		 with a qualification conversion (4.4) are also candidate
		 functions.  */
	      /* 13.3.1.6 doesn't have a parallel restriction, but it should;
		 I've raised this issue with the committee. --jason 9/2011 */
	      cand->viable = -1;
	      cand->reason = explicit_conversion_rejection (rettype, totype);
	    }
	  else if (cand->viable == 1 && ics->bad_p)
	    {
	      cand->viable = -1;
	      cand->reason
		= bad_arg_conversion_rejection (NULL_TREE, -2,
						rettype, totype);
	    }
	  else if (primary_template_instantiation_p (cand->fn)
		   && ics->rank > cr_exact)
	    {
	      /* 13.3.3.1.2: If the user-defined conversion is specified by
		 a specialization of a conversion function template, the
		 second standard conversion sequence shall have exact match
		 rank.  */
	      cand->viable = -1;
	      cand->reason = template_conversion_rejection (rettype, totype);
	    }
	}
    }

  candidates = splice_viable (candidates, false, &any_viable_p);
  if (!any_viable_p)
    {
      if (args)
	release_tree_vector (args);
      return NULL;
    }

  cand = tourney (candidates, complain);
  if (cand == 0)
    {
      if (complain & tf_error)
	{
	  error ("conversion from %qT to %qT is ambiguous",
		 fromtype, totype);
	  print_z_candidates (location_of (expr), candidates);
	}

      cand = candidates;	/* any one will do */
      cand->second_conv = build_ambiguous_conv (totype, expr);
      cand->second_conv->user_conv_p = true;
      if (!any_strictly_viable (candidates))
	cand->second_conv->bad_p = true;
      /* If there are viable candidates, don't set ICS_BAD_FLAG; an
	 ambiguous conversion is no worse than another user-defined
	 conversion.  */

      return cand;
    }

  tree convtype;
  if (!DECL_CONSTRUCTOR_P (cand->fn))
    convtype = non_reference (TREE_TYPE (TREE_TYPE (cand->fn)));
  else if (cand->second_conv->kind == ck_rvalue)
    /* DR 5: [in the first step of copy-initialization]...if the function
       is a constructor, the call initializes a temporary of the
       cv-unqualified version of the destination type. */
    convtype = cv_unqualified (totype);
  else
    convtype = totype;
  /* Build the user conversion sequence.  */
  conv = build_conv
    (ck_user,
     convtype,
     build_identity_conv (TREE_TYPE (expr), expr));
  conv->cand = cand;
  if (cand->viable == -1)
    conv->bad_p = true;

  /* Remember that this was a list-initialization.  */
  if (flags & LOOKUP_NO_NARROWING)
    conv->check_narrowing = true;

  /* Combine it with the second conversion sequence.  */
  cand->second_conv = merge_conversion_sequences (conv,
						  cand->second_conv);

  return cand;
}

/* Wrapper for above. */

tree
build_user_type_conversion (tree totype, tree expr, int flags,
			    tsubst_flags_t complain)
{
  struct z_candidate *cand;
  tree ret;

  bool subtime = timevar_cond_start (TV_OVERLOAD);
  cand = build_user_type_conversion_1 (totype, expr, flags, complain);

  if (cand)
    {
      if (cand->second_conv->kind == ck_ambig)
	ret = error_mark_node;
      else
        {
          expr = convert_like (cand->second_conv, expr, complain);
          ret = convert_from_reference (expr);
        }
    }
  else
    ret = NULL_TREE;

  timevar_cond_stop (TV_OVERLOAD, subtime);
  return ret;
}

/* Subroutine of convert_nontype_argument.

   EXPR is an argument for a template non-type parameter of integral or
   enumeration type.  Do any necessary conversions (that are permitted for
   non-type arguments) to convert it to the parameter type.

   If conversion is successful, returns the converted expression;
   otherwise, returns error_mark_node.  */

tree
build_integral_nontype_arg_conv (tree type, tree expr, tsubst_flags_t complain)
{
  conversion *conv;
  void *p;
  tree t;
  location_t loc = EXPR_LOC_OR_LOC (expr, input_location);

  if (error_operand_p (expr))
    return error_mark_node;

  gcc_assert (INTEGRAL_OR_ENUMERATION_TYPE_P (type));

  /* Get the high-water mark for the CONVERSION_OBSTACK.  */
  p = conversion_obstack_alloc (0);

  conv = implicit_conversion (type, TREE_TYPE (expr), expr,
			      /*c_cast_p=*/false,
			      LOOKUP_IMPLICIT, complain);

  /* for a non-type template-parameter of integral or
     enumeration type, integral promotions (4.5) and integral
     conversions (4.7) are applied.  */
  /* It should be sufficient to check the outermost conversion step, since
     there are no qualification conversions to integer type.  */
  if (conv)
    switch (conv->kind)
      {
	/* A conversion function is OK.  If it isn't constexpr, we'll
	   complain later that the argument isn't constant.  */
      case ck_user:
	/* The lvalue-to-rvalue conversion is OK.  */
      case ck_rvalue:
      case ck_identity:
	break;

      case ck_std:
	t = next_conversion (conv)->type;
	if (INTEGRAL_OR_ENUMERATION_TYPE_P (t))
	  break;

	if (complain & tf_error)
	  error_at (loc, "conversion from %qT to %qT not considered for "
		    "non-type template argument", t, type);
	/* and fall through.  */

      default:
	conv = NULL;
	break;
      }

  if (conv)
    expr = convert_like (conv, expr, complain);
  else
    expr = error_mark_node;

  /* Free all the conversions we allocated.  */
  obstack_free (&conversion_obstack, p);

  return expr;
}

/* Do any initial processing on the arguments to a function call.  */

static vec<tree, va_gc> *
resolve_args (vec<tree, va_gc> *args, tsubst_flags_t complain)
{
  unsigned int ix;
  tree arg;

  FOR_EACH_VEC_SAFE_ELT (args, ix, arg)
    {
      if (error_operand_p (arg))
	return NULL;
      else if (VOID_TYPE_P (TREE_TYPE (arg)))
	{
	  if (complain & tf_error)
	    error ("invalid use of void expression");
	  return NULL;
	}
      else if (invalid_nonstatic_memfn_p (arg, complain))
	return NULL;
    }
  return args;
}

/* Perform overload resolution on FN, which is called with the ARGS.

   Return the candidate function selected by overload resolution, or
   NULL if the event that overload resolution failed.  In the case
   that overload resolution fails, *CANDIDATES will be the set of
   candidates considered, and ANY_VIABLE_P will be set to true or
   false to indicate whether or not any of the candidates were
   viable.

   The ARGS should already have gone through RESOLVE_ARGS before this
   function is called.  */

static struct z_candidate *
perform_overload_resolution (tree fn,
			     const vec<tree, va_gc> *args,
			     struct z_candidate **candidates,
			     bool *any_viable_p, tsubst_flags_t complain)
{
  struct z_candidate *cand;
  tree explicit_targs;
  int template_only;

  bool subtime = timevar_cond_start (TV_OVERLOAD);

  explicit_targs = NULL_TREE;
  template_only = 0;

  *candidates = NULL;
  *any_viable_p = true;

  /* Check FN.  */
  gcc_assert (TREE_CODE (fn) == FUNCTION_DECL
	      || TREE_CODE (fn) == TEMPLATE_DECL
	      || TREE_CODE (fn) == OVERLOAD
	      || TREE_CODE (fn) == TEMPLATE_ID_EXPR);

  if (TREE_CODE (fn) == TEMPLATE_ID_EXPR)
    {
      explicit_targs = TREE_OPERAND (fn, 1);
      fn = TREE_OPERAND (fn, 0);
      template_only = 1;
    }

  /* Add the various candidate functions.  */
  add_candidates (fn, NULL_TREE, args, NULL_TREE,
		  explicit_targs, template_only,
		  /*conversion_path=*/NULL_TREE,
		  /*access_path=*/NULL_TREE,
		  LOOKUP_NORMAL,
		  candidates, complain);

  *candidates = splice_viable (*candidates, false, any_viable_p);
  if (*any_viable_p)
    cand = tourney (*candidates, complain);
  else
    cand = NULL;

  timevar_cond_stop (TV_OVERLOAD, subtime);
  return cand;
}

/* Print an error message about being unable to build a call to FN with
   ARGS.  ANY_VIABLE_P indicates whether any candidate functions could
   be located; CANDIDATES is a possibly empty list of such
   functions.  */

static void
print_error_for_call_failure (tree fn, vec<tree, va_gc> *args,
			      struct z_candidate *candidates)
{
  tree name = DECL_NAME (OVL_CURRENT (fn));
  location_t loc = location_of (name);

  if (!any_strictly_viable (candidates))
    error_at (loc, "no matching function for call to %<%D(%A)%>",
	      name, build_tree_list_vec (args));
  else
    error_at (loc, "call of overloaded %<%D(%A)%> is ambiguous",
	      name, build_tree_list_vec (args));
  if (candidates)
    print_z_candidates (loc, candidates);
}

/* Return an expression for a call to FN (a namespace-scope function,
   or a static member function) with the ARGS.  This may change
   ARGS.  */

tree
build_new_function_call (tree fn, vec<tree, va_gc> **args, bool koenig_p, 
			 tsubst_flags_t complain)
{
  struct z_candidate *candidates, *cand;
  bool any_viable_p;
  void *p;
  tree result;

  if (args != NULL && *args != NULL)
    {
      *args = resolve_args (*args, complain);
      if (*args == NULL)
	return error_mark_node;
    }

  if (flag_tm)
    tm_malloc_replacement (fn);

  /* If this function was found without using argument dependent
     lookup, then we want to ignore any undeclared friend
     functions.  */
  if (!koenig_p)
    {
      tree orig_fn = fn;

      fn = remove_hidden_names (fn);
      if (!fn)
	{
	  if (complain & tf_error)
	    print_error_for_call_failure (orig_fn, *args, NULL);
	  return error_mark_node;
	}
    }

  /* Get the high-water mark for the CONVERSION_OBSTACK.  */
  p = conversion_obstack_alloc (0);

  cand = perform_overload_resolution (fn, *args, &candidates, &any_viable_p,
				      complain);

  if (!cand)
    {
      if (complain & tf_error)
	{
	  if (!any_viable_p && candidates && ! candidates->next
	      && (TREE_CODE (candidates->fn) == FUNCTION_DECL))
	    return cp_build_function_call_vec (candidates->fn, args, complain);
	  if (TREE_CODE (fn) == TEMPLATE_ID_EXPR)
	    fn = TREE_OPERAND (fn, 0);
	  print_error_for_call_failure (fn, *args, candidates);
	}
      result = error_mark_node;
    }
  else
    {
      int flags = LOOKUP_NORMAL;
      /* If fn is template_id_expr, the call has explicit template arguments
         (e.g. func<int>(5)), communicate this info to build_over_call
         through flags so that later we can use it to decide whether to warn
         about peculiar null pointer conversion.  */
      if (TREE_CODE (fn) == TEMPLATE_ID_EXPR)
        flags |= LOOKUP_EXPLICIT_TMPL_ARGS;
      result = build_over_call (cand, flags, complain);
    }

  /* Free all the conversions we allocated.  */
  obstack_free (&conversion_obstack, p);

  return result;
}

/* Build a call to a global operator new.  FNNAME is the name of the
   operator (either "operator new" or "operator new[]") and ARGS are
   the arguments provided.  This may change ARGS.  *SIZE points to the
   total number of bytes required by the allocation, and is updated if
   that is changed here.  *COOKIE_SIZE is non-NULL if a cookie should
   be used.  If this function determines that no cookie should be
   used, after all, *COOKIE_SIZE is set to NULL_TREE.  If SIZE_CHECK
   is not NULL_TREE, it is evaluated before calculating the final
   array size, and if it fails, the array size is replaced with
   (size_t)-1 (usually triggering a std::bad_alloc exception).  If FN
   is non-NULL, it will be set, upon return, to the allocation
   function called.  */

tree
build_operator_new_call (tree fnname, vec<tree, va_gc> **args,
			 tree *size, tree *cookie_size, tree size_check,
			 tree *fn, tsubst_flags_t complain)
{
  tree original_size = *size;
  tree fns;
  struct z_candidate *candidates;
  struct z_candidate *cand;
  bool any_viable_p;

  if (fn)
    *fn = NULL_TREE;
  /* Set to (size_t)-1 if the size check fails.  */
  if (size_check != NULL_TREE)
    {
      tree errval = TYPE_MAX_VALUE (sizetype);
      if (cxx_dialect >= cxx11 && flag_exceptions)
	errval = throw_bad_array_new_length ();
      *size = fold_build3 (COND_EXPR, sizetype, size_check,
			   original_size, errval);
    }
  vec_safe_insert (*args, 0, *size);
  *args = resolve_args (*args, complain);
  if (*args == NULL)
    return error_mark_node;

  /* Based on:

       [expr.new]

       If this lookup fails to find the name, or if the allocated type
       is not a class type, the allocation function's name is looked
       up in the global scope.

     we disregard block-scope declarations of "operator new".  */
  fns = lookup_function_nonclass (fnname, *args, /*block_p=*/false);

  /* Figure out what function is being called.  */
  cand = perform_overload_resolution (fns, *args, &candidates, &any_viable_p,
				      complain);

  /* If no suitable function could be found, issue an error message
     and give up.  */
  if (!cand)
    {
      if (complain & tf_error)
	print_error_for_call_failure (fns, *args, candidates);
      return error_mark_node;
    }

   /* If a cookie is required, add some extra space.  Whether
      or not a cookie is required cannot be determined until
      after we know which function was called.  */
   if (*cookie_size)
     {
       bool use_cookie = true;
       tree arg_types;

       arg_types = TYPE_ARG_TYPES (TREE_TYPE (cand->fn));
       /* Skip the size_t parameter.  */
       arg_types = TREE_CHAIN (arg_types);
       /* Check the remaining parameters (if any).  */
       if (arg_types
	   && TREE_CHAIN (arg_types) == void_list_node
	   && same_type_p (TREE_VALUE (arg_types),
			   ptr_type_node))
	 use_cookie = false;
       /* If we need a cookie, adjust the number of bytes allocated.  */
       if (use_cookie)
	 {
	   /* Update the total size.  */
	   *size = size_binop (PLUS_EXPR, original_size, *cookie_size);
	   /* Set to (size_t)-1 if the size check fails.  */
	   gcc_assert (size_check != NULL_TREE);
	   *size = fold_build3 (COND_EXPR, sizetype, size_check,
				*size, TYPE_MAX_VALUE (sizetype));
	   /* Update the argument list to reflect the adjusted size.  */
	   (**args)[0] = *size;
	 }
       else
	 *cookie_size = NULL_TREE;
     }

   /* Tell our caller which function we decided to call.  */
   if (fn)
     *fn = cand->fn;

   /* Build the CALL_EXPR.  */
   return build_over_call (cand, LOOKUP_NORMAL, complain);
}

/* Build a new call to operator().  This may change ARGS.  */

static tree
build_op_call_1 (tree obj, vec<tree, va_gc> **args, tsubst_flags_t complain)
{
  struct z_candidate *candidates = 0, *cand;
  tree fns, convs, first_mem_arg = NULL_TREE;
  tree type = TREE_TYPE (obj);
  bool any_viable_p;
  tree result = NULL_TREE;
  void *p;

  if (error_operand_p (obj))
    return error_mark_node;

  obj = prep_operand (obj);

  if (TYPE_PTRMEMFUNC_P (type))
    {
      if (complain & tf_error)
        /* It's no good looking for an overloaded operator() on a
           pointer-to-member-function.  */
        error ("pointer-to-member function %E cannot be called without an object; consider using .* or ->*", obj);
      return error_mark_node;
    }

  if (TYPE_BINFO (type))
    {
      fns = lookup_fnfields (TYPE_BINFO (type), ansi_opname (CALL_EXPR), 1);
      if (fns == error_mark_node)
	return error_mark_node;
    }
  else
    fns = NULL_TREE;

  if (args != NULL && *args != NULL)
    {
      *args = resolve_args (*args, complain);
      if (*args == NULL)
	return error_mark_node;
    }

  /* Get the high-water mark for the CONVERSION_OBSTACK.  */
  p = conversion_obstack_alloc (0);

  if (fns)
    {
      first_mem_arg = obj;

      add_candidates (BASELINK_FUNCTIONS (fns),
		      first_mem_arg, *args, NULL_TREE,
		      NULL_TREE, false,
		      BASELINK_BINFO (fns), BASELINK_ACCESS_BINFO (fns),
		      LOOKUP_NORMAL, &candidates, complain);
    }

  convs = lookup_conversions (type);

  for (; convs; convs = TREE_CHAIN (convs))
    {
      tree fns = TREE_VALUE (convs);
      tree totype = TREE_TYPE (convs);

      if (TYPE_PTRFN_P (totype)
	  || TYPE_REFFN_P (totype)
	  || (TREE_CODE (totype) == REFERENCE_TYPE
	      && TYPE_PTRFN_P (TREE_TYPE (totype))))
	for (; fns; fns = OVL_NEXT (fns))
	  {
	    tree fn = OVL_CURRENT (fns);

	    if (DECL_NONCONVERTING_P (fn))
	      continue;

	    if (TREE_CODE (fn) == TEMPLATE_DECL)
	      add_template_conv_candidate
		(&candidates, fn, obj, NULL_TREE, *args, totype,
		 /*access_path=*/NULL_TREE,
		 /*conversion_path=*/NULL_TREE, complain);
	    else
	      add_conv_candidate (&candidates, fn, obj, NULL_TREE,
				  *args, /*conversion_path=*/NULL_TREE,
				  /*access_path=*/NULL_TREE, complain);
	  }
    }

  /* Be strict here because if we choose a bad conversion candidate, the
     errors we get won't mention the call context.  */
  candidates = splice_viable (candidates, true, &any_viable_p);
  if (!any_viable_p)
    {
      if (complain & tf_error)
        {
          error ("no match for call to %<(%T) (%A)%>", TREE_TYPE (obj),
		 build_tree_list_vec (*args));
          print_z_candidates (location_of (TREE_TYPE (obj)), candidates);
        }
      result = error_mark_node;
    }
  else
    {
      cand = tourney (candidates, complain);
      if (cand == 0)
	{
          if (complain & tf_error)
            {
              error ("call of %<(%T) (%A)%> is ambiguous", 
                     TREE_TYPE (obj), build_tree_list_vec (*args));
              print_z_candidates (location_of (TREE_TYPE (obj)), candidates);
            }
	  result = error_mark_node;
	}
      /* Since cand->fn will be a type, not a function, for a conversion
	 function, we must be careful not to unconditionally look at
	 DECL_NAME here.  */
      else if (TREE_CODE (cand->fn) == FUNCTION_DECL
	       && DECL_OVERLOADED_OPERATOR_P (cand->fn) == CALL_EXPR)
	result = build_over_call (cand, LOOKUP_NORMAL, complain);
      else
	{
	  obj = convert_like_with_context (cand->convs[0], obj, cand->fn, -1,
					   complain);
	  obj = convert_from_reference (obj);
	  result = cp_build_function_call_vec (obj, args, complain);
	}
    }

  /* Free all the conversions we allocated.  */
  obstack_free (&conversion_obstack, p);

  return result;
}

/* Wrapper for above.  */

tree
build_op_call (tree obj, vec<tree, va_gc> **args, tsubst_flags_t complain)
{
  tree ret;
  bool subtime = timevar_cond_start (TV_OVERLOAD);
  ret = build_op_call_1 (obj, args, complain);
  timevar_cond_stop (TV_OVERLOAD, subtime);
  return ret;
}

/* Called by op_error to prepare format strings suitable for the error
   function.  It concatenates a prefix (controlled by MATCH), ERRMSG,
   and a suffix (controlled by NTYPES).  */

static const char *
op_error_string (const char *errmsg, int ntypes, bool match)
{
  const char *msg;

  const char *msgp = concat (match ? G_("ambiguous overload for ")
			           : G_("no match for "), errmsg, NULL);

  if (ntypes == 3)
    msg = concat (msgp, G_(" (operand types are %qT, %qT, and %qT)"), NULL);
  else if (ntypes == 2)
    msg = concat (msgp, G_(" (operand types are %qT and %qT)"), NULL);
  else
    msg = concat (msgp, G_(" (operand type is %qT)"), NULL);

  return msg;
}

static void
op_error (location_t loc, enum tree_code code, enum tree_code code2,
	  tree arg1, tree arg2, tree arg3, bool match)
{
  const char *opname;

  if (code == MODIFY_EXPR)
    opname = assignment_operator_name_info[code2].name;
  else
    opname = operator_name_info[code].name;

  switch (code)
    {
    case COND_EXPR:
      if (flag_diagnostics_show_caret)
	error_at (loc, op_error_string (G_("ternary %<operator?:%>"),
					3, match),
		  TREE_TYPE (arg1), TREE_TYPE (arg2), TREE_TYPE (arg3));
      else
	error_at (loc, op_error_string (G_("ternary %<operator?:%> "
					   "in %<%E ? %E : %E%>"), 3, match),
		  arg1, arg2, arg3,
		  TREE_TYPE (arg1), TREE_TYPE (arg2), TREE_TYPE (arg3));
      break;

    case POSTINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      if (flag_diagnostics_show_caret)
	error_at (loc, op_error_string (G_("%<operator%s%>"), 1, match),
		  opname, TREE_TYPE (arg1));
      else
	error_at (loc, op_error_string (G_("%<operator%s%> in %<%E%s%>"),
					1, match),
		  opname, arg1, opname, TREE_TYPE (arg1));
      break;

    case ARRAY_REF:
      if (flag_diagnostics_show_caret)
	error_at (loc, op_error_string (G_("%<operator[]%>"), 2, match),
		  TREE_TYPE (arg1), TREE_TYPE (arg2));
      else
	error_at (loc, op_error_string (G_("%<operator[]%> in %<%E[%E]%>"),
					2, match),
		  arg1, arg2, TREE_TYPE (arg1), TREE_TYPE (arg2));
      break;

    case REALPART_EXPR:
    case IMAGPART_EXPR:
      if (flag_diagnostics_show_caret)
	error_at (loc, op_error_string (G_("%qs"), 1, match),
		  opname, TREE_TYPE (arg1));
      else
	error_at (loc, op_error_string (G_("%qs in %<%s %E%>"), 1, match),
		  opname, opname, arg1, TREE_TYPE (arg1));
      break;

    default:
      if (arg2)
	if (flag_diagnostics_show_caret)
	  error_at (loc, op_error_string (G_("%<operator%s%>"), 2, match),
		    opname, TREE_TYPE (arg1), TREE_TYPE (arg2));
	else
	  error_at (loc, op_error_string (G_("%<operator%s%> in %<%E %s %E%>"),
					  2, match),
		    opname, arg1, opname, arg2,
		    TREE_TYPE (arg1), TREE_TYPE (arg2));
      else
	if (flag_diagnostics_show_caret)
	  error_at (loc, op_error_string (G_("%<operator%s%>"), 1, match),
		    opname, TREE_TYPE (arg1));
	else
	  error_at (loc, op_error_string (G_("%<operator%s%> in %<%s%E%>"),
					  1, match),
		    opname, opname, arg1, TREE_TYPE (arg1));
      break;
    }
}

/* Return the implicit conversion sequence that could be used to
   convert E1 to E2 in [expr.cond].  */

static conversion *
conditional_conversion (tree e1, tree e2, tsubst_flags_t complain)
{
  tree t1 = non_reference (TREE_TYPE (e1));
  tree t2 = non_reference (TREE_TYPE (e2));
  conversion *conv;
  bool good_base;

  /* [expr.cond]

     If E2 is an lvalue: E1 can be converted to match E2 if E1 can be
     implicitly converted (clause _conv_) to the type "lvalue reference to
     T2", subject to the constraint that in the conversion the
     reference must bind directly (_dcl.init.ref_) to an lvalue.

     If E2 is an xvalue: E1 can be converted to match E2 if E1 can be
     implicitly converted to the type "rvalue reference to T2", subject to
     the constraint that the reference must bind directly.  */
  if (lvalue_or_rvalue_with_address_p (e2))
    {
      tree rtype = cp_build_reference_type (t2, !real_lvalue_p (e2));
      conv = implicit_conversion (rtype,
				  t1,
				  e1,
				  /*c_cast_p=*/false,
				  LOOKUP_NO_TEMP_BIND|LOOKUP_NO_RVAL_BIND
				  |LOOKUP_ONLYCONVERTING,
				  complain);
      if (conv && !conv->bad_p)
	return conv;
    }

  /* If E2 is a prvalue or if neither of the conversions above can be done
     and at least one of the operands has (possibly cv-qualified) class
     type: */
  if (!CLASS_TYPE_P (t1) && !CLASS_TYPE_P (t2))
    return NULL;

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
      && ((good_base = DERIVED_FROM_P (t2, t1)) || DERIVED_FROM_P (t1, t2)))
    {
      if (good_base && at_least_as_qualified_p (t2, t1))
	{
	  conv = build_identity_conv (t1, e1);
	  if (!same_type_p (TYPE_MAIN_VARIANT (t1),
			    TYPE_MAIN_VARIANT (t2)))
	    conv = build_conv (ck_base, t2, conv);
	  else
	    conv = build_conv (ck_rvalue, t2, conv);
	  return conv;
	}
      else
	return NULL;
    }
  else
    /* [expr.cond]

       Otherwise: E1 can be converted to match E2 if E1 can be implicitly
       converted to the type that expression E2 would have if E2 were
       converted to an rvalue (or the type it has, if E2 is an rvalue).  */
    return implicit_conversion (t2, t1, e1, /*c_cast_p=*/false,
				LOOKUP_IMPLICIT, complain);
}

/* Implement [expr.cond].  ARG1, ARG2, and ARG3 are the three
   arguments to the conditional expression.  */

static tree
build_conditional_expr_1 (location_t loc, tree arg1, tree arg2, tree arg3,
                          tsubst_flags_t complain)
{
  tree arg2_type;
  tree arg3_type;
  tree result = NULL_TREE;
  tree result_type = NULL_TREE;
  bool lvalue_p = true;
  struct z_candidate *candidates = 0;
  struct z_candidate *cand;
  void *p;
  tree orig_arg2, orig_arg3;

  /* As a G++ extension, the second argument to the conditional can be
     omitted.  (So that `a ? : c' is roughly equivalent to `a ? a :
     c'.)  If the second operand is omitted, make sure it is
     calculated only once.  */
  if (!arg2)
    {
      if (complain & tf_error)
	pedwarn (loc, OPT_Wpedantic, 
		 "ISO C++ forbids omitting the middle term of a ?: expression");

      /* Make sure that lvalues remain lvalues.  See g++.oliva/ext1.C.  */
      if (real_lvalue_p (arg1))
	arg2 = arg1 = stabilize_reference (arg1);
      else
	arg2 = arg1 = save_expr (arg1);
    }

  /* If something has already gone wrong, just pass that fact up the
     tree.  */
  if (error_operand_p (arg1)
      || error_operand_p (arg2)
      || error_operand_p (arg3))
    return error_mark_node;

  orig_arg2 = arg2;
  orig_arg3 = arg3;

  if (VECTOR_INTEGER_TYPE_P (TREE_TYPE (arg1)))
    {
      arg1 = force_rvalue (arg1, complain);
      arg2 = force_rvalue (arg2, complain);
      arg3 = force_rvalue (arg3, complain);

      /* force_rvalue can return error_mark on valid arguments.  */
      if (error_operand_p (arg1)
	  || error_operand_p (arg2)
	  || error_operand_p (arg3))
	return error_mark_node;

      tree arg1_type = TREE_TYPE (arg1);
      arg2_type = TREE_TYPE (arg2);
      arg3_type = TREE_TYPE (arg3);

      if (TREE_CODE (arg2_type) != VECTOR_TYPE
	  && TREE_CODE (arg3_type) != VECTOR_TYPE)
	{
	  /* Rely on the error messages of the scalar version.  */
	  tree scal = build_conditional_expr_1 (loc, integer_one_node,
						orig_arg2, orig_arg3, complain);
	  if (scal == error_mark_node)
	    return error_mark_node;
	  tree stype = TREE_TYPE (scal);
	  tree ctype = TREE_TYPE (arg1_type);
	  if (TYPE_SIZE (stype) != TYPE_SIZE (ctype)
	      || (!INTEGRAL_TYPE_P (stype) && !SCALAR_FLOAT_TYPE_P (stype)))
	    {
	      if (complain & tf_error)
		error_at (loc, "inferred scalar type %qT is not an integer or "
			  "floating point type of the same size as %qT", stype,
			  COMPARISON_CLASS_P (arg1)
			  ? TREE_TYPE (TREE_TYPE (TREE_OPERAND (arg1, 0)))
			  : ctype);
	      return error_mark_node;
	    }

	  tree vtype = build_opaque_vector_type (stype,
			 TYPE_VECTOR_SUBPARTS (arg1_type));
	  /* We could pass complain & tf_warning to unsafe_conversion_p,
	     but the warnings (like Wsign-conversion) have already been
	     given by the scalar build_conditional_expr_1. We still check
	     unsafe_conversion_p to forbid truncating long long -> float.  */
	  if (unsafe_conversion_p (loc, stype, arg2, false))
	    {
	      if (complain & tf_error)
		error_at (loc, "conversion of scalar %qT to vector %qT "
			       "involves truncation", arg2_type, vtype);
	      return error_mark_node;
	    }
	  if (unsafe_conversion_p (loc, stype, arg3, false))
	    {
	      if (complain & tf_error)
		error_at (loc, "conversion of scalar %qT to vector %qT "
			       "involves truncation", arg3_type, vtype);
	      return error_mark_node;
	    }

	  arg2 = cp_convert (stype, arg2, complain);
	  arg2 = save_expr (arg2);
	  arg2 = build_vector_from_val (vtype, arg2);
	  arg2_type = vtype;
	  arg3 = cp_convert (stype, arg3, complain);
	  arg3 = save_expr (arg3);
	  arg3 = build_vector_from_val (vtype, arg3);
	  arg3_type = vtype;
	}

      if ((TREE_CODE (arg2_type) == VECTOR_TYPE)
	  != (TREE_CODE (arg3_type) == VECTOR_TYPE))
	{
	  enum stv_conv convert_flag =
	    scalar_to_vector (loc, VEC_COND_EXPR, arg2, arg3,
			      complain & tf_error);

	  switch (convert_flag)
	    {
	      case stv_error:
		return error_mark_node;
	      case stv_firstarg:
		{
		  arg2 = save_expr (arg2);
		  arg2 = convert (TREE_TYPE (arg3_type), arg2);
		  arg2 = build_vector_from_val (arg3_type, arg2);
		  arg2_type = TREE_TYPE (arg2);
		  break;
		}
	      case stv_secondarg:
		{
		  arg3 = save_expr (arg3);
		  arg3 = convert (TREE_TYPE (arg2_type), arg3);
		  arg3 = build_vector_from_val (arg2_type, arg3);
		  arg3_type = TREE_TYPE (arg3);
		  break;
		}
	      default:
		break;
	    }
	}

      if (!same_type_p (arg2_type, arg3_type)
	  || TYPE_VECTOR_SUBPARTS (arg1_type)
	     != TYPE_VECTOR_SUBPARTS (arg2_type)
	  || TYPE_SIZE (arg1_type) != TYPE_SIZE (arg2_type))
	{
	  if (complain & tf_error)
	    error_at (loc,
		      "incompatible vector types in conditional expression: "
		      "%qT, %qT and %qT", TREE_TYPE (arg1),
		      TREE_TYPE (orig_arg2), TREE_TYPE (orig_arg3));
	  return error_mark_node;
	}

      if (!COMPARISON_CLASS_P (arg1))
	arg1 = cp_build_binary_op (loc, NE_EXPR, arg1,
				   build_zero_cst (arg1_type), complain);
      return fold_build3 (VEC_COND_EXPR, arg2_type, arg1, arg2, arg3);
    }

  /* [expr.cond]

     The first expression is implicitly converted to bool (clause
     _conv_).  */
  arg1 = perform_implicit_conversion_flags (boolean_type_node, arg1, complain,
					    LOOKUP_NORMAL);
  if (error_operand_p (arg1))
    return error_mark_node;

  /* [expr.cond]

     If either the second or the third operand has type (possibly
     cv-qualified) void, then the lvalue-to-rvalue (_conv.lval_),
     array-to-pointer (_conv.array_), and function-to-pointer
     (_conv.func_) standard conversions are performed on the second
     and third operands.  */
  arg2_type = unlowered_expr_type (arg2);
  arg3_type = unlowered_expr_type (arg3);
  if (VOID_TYPE_P (arg2_type) || VOID_TYPE_P (arg3_type))
    {
      /* Do the conversions.  We don't these for `void' type arguments
	 since it can't have any effect and since decay_conversion
	 does not handle that case gracefully.  */
      if (!VOID_TYPE_P (arg2_type))
	arg2 = decay_conversion (arg2, complain);
      if (!VOID_TYPE_P (arg3_type))
	arg3 = decay_conversion (arg3, complain);
      arg2_type = TREE_TYPE (arg2);
      arg3_type = TREE_TYPE (arg3);

      /* [expr.cond]

	 One of the following shall hold:

	 --The second or the third operand (but not both) is a
	   throw-expression (_except.throw_); the result is of the
	   type of the other and is an rvalue.

	 --Both the second and the third operands have type void; the
	   result is of type void and is an rvalue.

	 We must avoid calling force_rvalue for expressions of type
	 "void" because it will complain that their value is being
	 used.  */
      if (TREE_CODE (arg2) == THROW_EXPR
	  && TREE_CODE (arg3) != THROW_EXPR)
	{
	  if (!VOID_TYPE_P (arg3_type))
	    {
	      arg3 = force_rvalue (arg3, complain);
	      if (arg3 == error_mark_node)
		return error_mark_node;
	    }
	  arg3_type = TREE_TYPE (arg3);
	  result_type = arg3_type;
	}
      else if (TREE_CODE (arg2) != THROW_EXPR
	       && TREE_CODE (arg3) == THROW_EXPR)
	{
	  if (!VOID_TYPE_P (arg2_type))
	    {
	      arg2 = force_rvalue (arg2, complain);
	      if (arg2 == error_mark_node)
		return error_mark_node;
	    }
	  arg2_type = TREE_TYPE (arg2);
	  result_type = arg2_type;
	}
      else if (VOID_TYPE_P (arg2_type) && VOID_TYPE_P (arg3_type))
	result_type = void_type_node;
      else
	{
          if (complain & tf_error)
            {
              if (VOID_TYPE_P (arg2_type))
                error_at (EXPR_LOC_OR_LOC (arg3, loc),
			  "second operand to the conditional operator "
			  "is of type %<void%>, but the third operand is "
			  "neither a throw-expression nor of type %<void%>");
              else
                error_at (EXPR_LOC_OR_LOC (arg2, loc),
			  "third operand to the conditional operator "
			  "is of type %<void%>, but the second operand is "
			  "neither a throw-expression nor of type %<void%>");
            }
	  return error_mark_node;
	}

      lvalue_p = false;
      goto valid_operands;
    }
  /* [expr.cond]

     Otherwise, if the second and third operand have different types,
     and either has (possibly cv-qualified) class type, or if both are
     glvalues of the same value category and the same type except for
     cv-qualification, an attempt is made to convert each of those operands
     to the type of the other.  */
  else if (!same_type_p (arg2_type, arg3_type)
	    && (CLASS_TYPE_P (arg2_type) || CLASS_TYPE_P (arg3_type)
		|| (same_type_ignoring_top_level_qualifiers_p (arg2_type,
							       arg3_type)
		    && lvalue_or_rvalue_with_address_p (arg2)
		    && lvalue_or_rvalue_with_address_p (arg3)
		    && real_lvalue_p (arg2) == real_lvalue_p (arg3))))
    {
      conversion *conv2;
      conversion *conv3;

      /* Get the high-water mark for the CONVERSION_OBSTACK.  */
      p = conversion_obstack_alloc (0);

      conv2 = conditional_conversion (arg2, arg3, complain);
      conv3 = conditional_conversion (arg3, arg2, complain);

      /* [expr.cond]

	 If both can be converted, or one can be converted but the
	 conversion is ambiguous, the program is ill-formed.  If
	 neither can be converted, the operands are left unchanged and
	 further checking is performed as described below.  If exactly
	 one conversion is possible, that conversion is applied to the
	 chosen operand and the converted operand is used in place of
	 the original operand for the remainder of this section.  */
      if ((conv2 && !conv2->bad_p
	   && conv3 && !conv3->bad_p)
	  || (conv2 && conv2->kind == ck_ambig)
	  || (conv3 && conv3->kind == ck_ambig))
	{
	  if (complain & tf_error)
	    {
	      error_at (loc, "operands to ?: have different types %qT and %qT",
			arg2_type, arg3_type);
	      if (conv2 && !conv2->bad_p && conv3 && !conv3->bad_p)
		inform (loc, "  and each type can be converted to the other");
	      else if (conv2 && conv2->kind == ck_ambig)
		convert_like (conv2, arg2, complain);
	      else
		convert_like (conv3, arg3, complain);
	    }
	  result = error_mark_node;
	}
      else if (conv2 && !conv2->bad_p)
	{
	  arg2 = convert_like (conv2, arg2, complain);
	  arg2 = convert_from_reference (arg2);
	  arg2_type = TREE_TYPE (arg2);
	  /* Even if CONV2 is a valid conversion, the result of the
	     conversion may be invalid.  For example, if ARG3 has type
	     "volatile X", and X does not have a copy constructor
	     accepting a "volatile X&", then even if ARG2 can be
	     converted to X, the conversion will fail.  */
	  if (error_operand_p (arg2))
	    result = error_mark_node;
	}
      else if (conv3 && !conv3->bad_p)
	{
	  arg3 = convert_like (conv3, arg3, complain);
	  arg3 = convert_from_reference (arg3);
	  arg3_type = TREE_TYPE (arg3);
	  if (error_operand_p (arg3))
	    result = error_mark_node;
	}

      /* Free all the conversions we allocated.  */
      obstack_free (&conversion_obstack, p);

      if (result)
	return result;

      /* If, after the conversion, both operands have class type,
	 treat the cv-qualification of both operands as if it were the
	 union of the cv-qualification of the operands.

	 The standard is not clear about what to do in this
	 circumstance.  For example, if the first operand has type
	 "const X" and the second operand has a user-defined
	 conversion to "volatile X", what is the type of the second
	 operand after this step?  Making it be "const X" (matching
	 the first operand) seems wrong, as that discards the
	 qualification without actually performing a copy.  Leaving it
	 as "volatile X" seems wrong as that will result in the
	 conditional expression failing altogether, even though,
	 according to this step, the one operand could be converted to
	 the type of the other.  */
      if (((conv2 && !conv2->bad_p)
	   || (conv3 && !conv3->bad_p))
	  && CLASS_TYPE_P (arg2_type)
	  && cp_type_quals (arg2_type) != cp_type_quals (arg3_type))
	arg2_type = arg3_type =
	  cp_build_qualified_type (arg2_type,
				   cp_type_quals (arg2_type)
				   | cp_type_quals (arg3_type));
    }

  /* [expr.cond]

     If the second and third operands are glvalues of the same value
     category and have the same type, the result is of that type and
     value category.  */
  if (((real_lvalue_p (arg2) && real_lvalue_p (arg3))
       || (xvalue_p (arg2) && xvalue_p (arg3)))
      && same_type_p (arg2_type, arg3_type))
    {
      result_type = arg2_type;
      arg2 = mark_lvalue_use (arg2);
      arg3 = mark_lvalue_use (arg3);
      goto valid_operands;
    }

  /* [expr.cond]

     Otherwise, the result is an rvalue.  If the second and third
     operand do not have the same type, and either has (possibly
     cv-qualified) class type, overload resolution is used to
     determine the conversions (if any) to be applied to the operands
     (_over.match.oper_, _over.built_).  */
  lvalue_p = false;
  if (!same_type_p (arg2_type, arg3_type)
      && (CLASS_TYPE_P (arg2_type) || CLASS_TYPE_P (arg3_type)))
    {
      tree args[3];
      conversion *conv;
      bool any_viable_p;

      /* Rearrange the arguments so that add_builtin_candidate only has
	 to know about two args.  In build_builtin_candidate, the
	 arguments are unscrambled.  */
      args[0] = arg2;
      args[1] = arg3;
      args[2] = arg1;
      add_builtin_candidates (&candidates,
			      COND_EXPR,
			      NOP_EXPR,
			      ansi_opname (COND_EXPR),
			      args,
			      LOOKUP_NORMAL, complain);

      /* [expr.cond]

	 If the overload resolution fails, the program is
	 ill-formed.  */
      candidates = splice_viable (candidates, false, &any_viable_p);
      if (!any_viable_p)
	{
          if (complain & tf_error)
	    error_at (loc, "operands to ?: have different types %qT and %qT",
		      arg2_type, arg3_type);
	  return error_mark_node;
	}
      cand = tourney (candidates, complain);
      if (!cand)
	{
          if (complain & tf_error)
            {
              op_error (loc, COND_EXPR, NOP_EXPR, arg1, arg2, arg3, FALSE);
              print_z_candidates (loc, candidates);
            }
	  return error_mark_node;
	}

      /* [expr.cond]

	 Otherwise, the conversions thus determined are applied, and
	 the converted operands are used in place of the original
	 operands for the remainder of this section.  */
      conv = cand->convs[0];
      arg1 = convert_like (conv, arg1, complain);
      conv = cand->convs[1];
      arg2 = convert_like (conv, arg2, complain);
      arg2_type = TREE_TYPE (arg2);
      conv = cand->convs[2];
      arg3 = convert_like (conv, arg3, complain);
      arg3_type = TREE_TYPE (arg3);
    }

  /* [expr.cond]

     Lvalue-to-rvalue (_conv.lval_), array-to-pointer (_conv.array_),
     and function-to-pointer (_conv.func_) standard conversions are
     performed on the second and third operands.

     We need to force the lvalue-to-rvalue conversion here for class types,
     so we get TARGET_EXPRs; trying to deal with a COND_EXPR of class rvalues
     that isn't wrapped with a TARGET_EXPR plays havoc with exception
     regions.  */

  arg2 = force_rvalue (arg2, complain);
  if (!CLASS_TYPE_P (arg2_type))
    arg2_type = TREE_TYPE (arg2);

  arg3 = force_rvalue (arg3, complain);
  if (!CLASS_TYPE_P (arg3_type))
    arg3_type = TREE_TYPE (arg3);

  if (arg2 == error_mark_node || arg3 == error_mark_node)
    return error_mark_node;

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
	    || UNSCOPED_ENUM_P (arg2_type))
	   && (ARITHMETIC_TYPE_P (arg3_type)
	       || UNSCOPED_ENUM_P (arg3_type)))
    {
      /* In this case, there is always a common type.  */
      result_type = type_after_usual_arithmetic_conversions (arg2_type,
							     arg3_type);
      if (complain & tf_warning)
	do_warn_double_promotion (result_type, arg2_type, arg3_type,
				  "implicit conversion from %qT to %qT to "
				  "match other result of conditional",
				  loc);

      if (TREE_CODE (arg2_type) == ENUMERAL_TYPE
	  && TREE_CODE (arg3_type) == ENUMERAL_TYPE)
        {
	  if (TREE_CODE (orig_arg2) == CONST_DECL
	      && TREE_CODE (orig_arg3) == CONST_DECL
	      && DECL_CONTEXT (orig_arg2) == DECL_CONTEXT (orig_arg3))
	    /* Two enumerators from the same enumeration can have different
	       types when the enumeration is still being defined.  */;
          else if (complain & tf_warning)
            warning_at (loc, OPT_Wenum_compare, "enumeral mismatch in "
			"conditional expression: %qT vs %qT",
			arg2_type, arg3_type);
        }
      else if (extra_warnings
	       && ((TREE_CODE (arg2_type) == ENUMERAL_TYPE
		    && !same_type_p (arg3_type, type_promotes_to (arg2_type)))
		   || (TREE_CODE (arg3_type) == ENUMERAL_TYPE
		       && !same_type_p (arg2_type,
					type_promotes_to (arg3_type)))))
        {
          if (complain & tf_warning)
            warning_at (loc, 0, "enumeral and non-enumeral type in "
			"conditional expression");
        }

      arg2 = perform_implicit_conversion (result_type, arg2, complain);
      arg3 = perform_implicit_conversion (result_type, arg3, complain);
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
       The result is of the common type.  */
  else if ((null_ptr_cst_p (arg2)
	    && TYPE_PTR_OR_PTRMEM_P (arg3_type))
	   || (null_ptr_cst_p (arg3)
	       && TYPE_PTR_OR_PTRMEM_P (arg2_type))
	   || (TYPE_PTR_P (arg2_type) && TYPE_PTR_P (arg3_type))
	   || (TYPE_PTRDATAMEM_P (arg2_type) && TYPE_PTRDATAMEM_P (arg3_type))
	   || (TYPE_PTRMEMFUNC_P (arg2_type) && TYPE_PTRMEMFUNC_P (arg3_type)))
    {
      result_type = composite_pointer_type (arg2_type, arg3_type, arg2,
					    arg3, CPO_CONDITIONAL_EXPR,
					    complain);
      if (result_type == error_mark_node)
	return error_mark_node;
      arg2 = perform_implicit_conversion (result_type, arg2, complain);
      arg3 = perform_implicit_conversion (result_type, arg3, complain);
    }

  if (!result_type)
    {
      if (complain & tf_error)
        error_at (loc, "operands to ?: have different types %qT and %qT",
		  arg2_type, arg3_type);
      return error_mark_node;
    }

  if (arg2 == error_mark_node || arg3 == error_mark_node)
    return error_mark_node;

 valid_operands:
  result = build3 (COND_EXPR, result_type, arg1, arg2, arg3);
  if (!cp_unevaluated_operand)
    /* Avoid folding within decltype (c++/42013) and noexcept.  */
    result = fold_if_not_in_template (result);

  /* We can't use result_type below, as fold might have returned a
     throw_expr.  */

  if (!lvalue_p)
    {
      /* Expand both sides into the same slot, hopefully the target of
	 the ?: expression.  We used to check for TARGET_EXPRs here,
	 but now we sometimes wrap them in NOP_EXPRs so the test would
	 fail.  */
      if (CLASS_TYPE_P (TREE_TYPE (result)))
	result = get_target_expr_sfinae (result, complain);
      /* If this expression is an rvalue, but might be mistaken for an
	 lvalue, we must add a NON_LVALUE_EXPR.  */
      result = rvalue (result);
    }
  else
    result = force_paren_expr (result);

  return result;
}

/* Wrapper for above.  */

tree
build_conditional_expr (location_t loc, tree arg1, tree arg2, tree arg3,
                        tsubst_flags_t complain)
{
  tree ret;
  bool subtime = timevar_cond_start (TV_OVERLOAD);
  ret = build_conditional_expr_1 (loc, arg1, arg2, arg3, complain);
  timevar_cond_stop (TV_OVERLOAD, subtime);
  return ret;
}

/* OPERAND is an operand to an expression.  Perform necessary steps
   required before using it.  If OPERAND is NULL_TREE, NULL_TREE is
   returned.  */

static tree
prep_operand (tree operand)
{
  if (operand)
    {
      if (CLASS_TYPE_P (TREE_TYPE (operand))
	  && CLASSTYPE_TEMPLATE_INSTANTIATION (TREE_TYPE (operand)))
	/* Make sure the template type is instantiated now.  */
	instantiate_class_template (TYPE_MAIN_VARIANT (TREE_TYPE (operand)));
    }

  return operand;
}

/* Add each of the viable functions in FNS (a FUNCTION_DECL or
   OVERLOAD) to the CANDIDATES, returning an updated list of
   CANDIDATES.  The ARGS are the arguments provided to the call;
   if FIRST_ARG is non-null it is the implicit object argument,
   otherwise the first element of ARGS is used if needed.  The
   EXPLICIT_TARGS are explicit template arguments provided.
   TEMPLATE_ONLY is true if only template functions should be
   considered.  CONVERSION_PATH, ACCESS_PATH, and FLAGS are as for
   add_function_candidate.  */

static void
add_candidates (tree fns, tree first_arg, const vec<tree, va_gc> *args,
		tree return_type,
		tree explicit_targs, bool template_only,
		tree conversion_path, tree access_path,
		int flags,
		struct z_candidate **candidates,
		tsubst_flags_t complain)
{
  tree ctype;
  const vec<tree, va_gc> *non_static_args;
  bool check_list_ctor;
  bool check_converting;
  unification_kind_t strict;
  tree fn;

  if (!fns)
    return;

  /* Precalculate special handling of constructors and conversion ops.  */
  fn = OVL_CURRENT (fns);
  if (DECL_CONV_FN_P (fn))
    {
      check_list_ctor = false;
      check_converting = !!(flags & LOOKUP_ONLYCONVERTING);
      if (flags & LOOKUP_NO_CONVERSION)
	/* We're doing return_type(x).  */
	strict = DEDUCE_CONV;
      else
	/* We're doing x.operator return_type().  */
	strict = DEDUCE_EXACT;
      /* [over.match.funcs] For conversion functions, the function
	 is considered to be a member of the class of the implicit
	 object argument for the purpose of defining the type of
	 the implicit object parameter.  */
      ctype = TYPE_MAIN_VARIANT (TREE_TYPE (first_arg));
    }
  else
    {
      if (DECL_CONSTRUCTOR_P (fn))
	{
	  check_list_ctor = !!(flags & LOOKUP_LIST_ONLY);
	  /* For list-initialization we consider explicit constructors
	     and complain if one is chosen.  */
	  check_converting
	    = ((flags & (LOOKUP_ONLYCONVERTING|LOOKUP_LIST_INIT_CTOR))
	       == LOOKUP_ONLYCONVERTING);
	}
      else
	{
	  check_list_ctor = false;
	  check_converting = false;
	}
      strict = DEDUCE_CALL;
      ctype = conversion_path ? BINFO_TYPE (conversion_path) : NULL_TREE;
    }

  if (first_arg)
    non_static_args = args;
  else
    /* Delay creating the implicit this parameter until it is needed.  */
    non_static_args = NULL;

  for (; fns; fns = OVL_NEXT (fns))
    {
      tree fn_first_arg;
      const vec<tree, va_gc> *fn_args;

      fn = OVL_CURRENT (fns);

      if (check_converting && DECL_NONCONVERTING_P (fn))
	continue;
      if (check_list_ctor && !is_list_ctor (fn))
	continue;

      /* Figure out which set of arguments to use.  */
      if (DECL_NONSTATIC_MEMBER_FUNCTION_P (fn))
	{
	  /* If this function is a non-static member and we didn't get an
	     implicit object argument, move it out of args.  */
	  if (first_arg == NULL_TREE)
	    {
	      unsigned int ix;
	      tree arg;
	      vec<tree, va_gc> *tempvec;
	      vec_alloc (tempvec, args->length () - 1);
	      for (ix = 1; args->iterate (ix, &arg); ++ix)
		tempvec->quick_push (arg);
	      non_static_args = tempvec;
	      first_arg = (*args)[0];
	    }

	  fn_first_arg = first_arg;
	  fn_args = non_static_args;
	}
      else
	{
	  /* Otherwise, just use the list of arguments provided.  */
	  fn_first_arg = NULL_TREE;
	  fn_args = args;
	}

      if (TREE_CODE (fn) == TEMPLATE_DECL)
	add_template_candidate (candidates,
				fn,
				ctype,
				explicit_targs,
				fn_first_arg, 
				fn_args,
				return_type,
				access_path,
				conversion_path,
				flags,
				strict,
				complain);
      else if (!template_only)
	add_function_candidate (candidates,
				fn,
				ctype,
				fn_first_arg,
				fn_args,
				access_path,
				conversion_path,
				flags,
				complain);
    }
}

static tree
build_new_op_1 (location_t loc, enum tree_code code, int flags, tree arg1,
		tree arg2, tree arg3, tree *overload, tsubst_flags_t complain)
{
  struct z_candidate *candidates = 0, *cand;
  vec<tree, va_gc> *arglist;
  tree fnname;
  tree args[3];
  tree result = NULL_TREE;
  bool result_valid_p = false;
  enum tree_code code2 = NOP_EXPR;
  enum tree_code code_orig_arg1 = ERROR_MARK;
  enum tree_code code_orig_arg2 = ERROR_MARK;
  conversion *conv;
  void *p;
  bool strict_p;
  bool any_viable_p;

  if (error_operand_p (arg1)
      || error_operand_p (arg2)
      || error_operand_p (arg3))
    return error_mark_node;

  if (code == MODIFY_EXPR)
    {
      code2 = TREE_CODE (arg3);
      arg3 = NULL_TREE;
      fnname = ansi_assopname (code2);
    }
  else
    fnname = ansi_opname (code);

  arg1 = prep_operand (arg1);

  switch (code)
    {
    case NEW_EXPR:
    case VEC_NEW_EXPR:
    case VEC_DELETE_EXPR:
    case DELETE_EXPR:
      /* Use build_op_new_call and build_op_delete_call instead.  */
      gcc_unreachable ();

    case CALL_EXPR:
      /* Use build_op_call instead.  */
      gcc_unreachable ();

    case TRUTH_ORIF_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
      /* These are saved for the sake of warn_logical_operator.  */
      code_orig_arg1 = TREE_CODE (arg1);
      code_orig_arg2 = TREE_CODE (arg2);

    default:
      break;
    }

  arg2 = prep_operand (arg2);
  arg3 = prep_operand (arg3);

  if (code == COND_EXPR)
    /* Use build_conditional_expr instead.  */
    gcc_unreachable ();
  else if (! OVERLOAD_TYPE_P (TREE_TYPE (arg1))
	   && (! arg2 || ! OVERLOAD_TYPE_P (TREE_TYPE (arg2))))
    goto builtin;

  if (code == POSTINCREMENT_EXPR || code == POSTDECREMENT_EXPR)
    arg2 = integer_zero_node;

  vec_alloc (arglist, 3);
  arglist->quick_push (arg1);
  if (arg2 != NULL_TREE)
    arglist->quick_push (arg2);
  if (arg3 != NULL_TREE)
    arglist->quick_push (arg3);

  /* Get the high-water mark for the CONVERSION_OBSTACK.  */
  p = conversion_obstack_alloc (0);

  /* Add namespace-scope operators to the list of functions to
     consider.  */
  add_candidates (lookup_function_nonclass (fnname, arglist, /*block_p=*/true),
		  NULL_TREE, arglist, NULL_TREE,
		  NULL_TREE, false, NULL_TREE, NULL_TREE,
		  flags, &candidates, complain);

  args[0] = arg1;
  args[1] = arg2;
  args[2] = NULL_TREE;

  /* Add class-member operators to the candidate set.  */
  if (CLASS_TYPE_P (TREE_TYPE (arg1)))
    {
      tree fns;

      fns = lookup_fnfields (TREE_TYPE (arg1), fnname, 1);
      if (fns == error_mark_node)
	{
	  result = error_mark_node;
	  goto user_defined_result_ready;
	}
      if (fns)
	add_candidates (BASELINK_FUNCTIONS (fns),
			NULL_TREE, arglist, NULL_TREE,
			NULL_TREE, false,
			BASELINK_BINFO (fns),
			BASELINK_ACCESS_BINFO (fns),
			flags, &candidates, complain);
    }
  /* Per 13.3.1.2/3, 2nd bullet, if no operand has a class type, then
     only non-member functions that have type T1 or reference to
     cv-qualified-opt T1 for the first argument, if the first argument
     has an enumeration type, or T2 or reference to cv-qualified-opt
     T2 for the second argument, if the the second argument has an
     enumeration type.  Filter out those that don't match.  */
  else if (! arg2 || ! CLASS_TYPE_P (TREE_TYPE (arg2)))
    {
      struct z_candidate **candp, **next;

      for (candp = &candidates; *candp; candp = next)
	{
	  tree parmlist, parmtype;
	  int i, nargs = (arg2 ? 2 : 1);

	  cand = *candp;
	  next = &cand->next;

	  parmlist = TYPE_ARG_TYPES (TREE_TYPE (cand->fn));

	  for (i = 0; i < nargs; ++i)
	    {
	      parmtype = TREE_VALUE (parmlist);

	      if (TREE_CODE (parmtype) == REFERENCE_TYPE)
		parmtype = TREE_TYPE (parmtype);
	      if (TREE_CODE (TREE_TYPE (args[i])) == ENUMERAL_TYPE
		  && (same_type_ignoring_top_level_qualifiers_p
		      (TREE_TYPE (args[i]), parmtype)))
		break;

	      parmlist = TREE_CHAIN (parmlist);
	    }

	  /* No argument has an appropriate type, so remove this
	     candidate function from the list.  */
	  if (i == nargs)
	    {
	      *candp = cand->next;
	      next = candp;
	    }
	}
    }

  add_builtin_candidates (&candidates, code, code2, fnname, args,
			  flags, complain);

  switch (code)
    {
    case COMPOUND_EXPR:
    case ADDR_EXPR:
      /* For these, the built-in candidates set is empty
	 [over.match.oper]/3.  We don't want non-strict matches
	 because exact matches are always possible with built-in
	 operators.  The built-in candidate set for COMPONENT_REF
	 would be empty too, but since there are no such built-in
	 operators, we accept non-strict matches for them.  */
      strict_p = true;
      break;

    default:
      strict_p = false;
      break;
    }

  candidates = splice_viable (candidates, strict_p, &any_viable_p);
  if (!any_viable_p)
    {
      switch (code)
	{
	case POSTINCREMENT_EXPR:
	case POSTDECREMENT_EXPR:
	  /* Don't try anything fancy if we're not allowed to produce
	     errors.  */
	  if (!(complain & tf_error))
	    return error_mark_node;

	  /* Look for an `operator++ (int)'. Pre-1985 C++ didn't
	     distinguish between prefix and postfix ++ and
	     operator++() was used for both, so we allow this with
	     -fpermissive.  */
	  else
	    {
	      const char *msg = (flag_permissive) 
		? G_("no %<%D(int)%> declared for postfix %qs,"
		     " trying prefix operator instead")
		: G_("no %<%D(int)%> declared for postfix %qs");
	      permerror (loc, msg, fnname, operator_name_info[code].name);
	    }

	  if (!flag_permissive)
	    return error_mark_node;

	  if (code == POSTINCREMENT_EXPR)
	    code = PREINCREMENT_EXPR;
	  else
	    code = PREDECREMENT_EXPR;
	  result = build_new_op_1 (loc, code, flags, arg1, NULL_TREE,
				   NULL_TREE, overload, complain);
	  break;

	  /* The caller will deal with these.  */
	case ADDR_EXPR:
	case COMPOUND_EXPR:
	case COMPONENT_REF:
	  result = NULL_TREE;
	  result_valid_p = true;
	  break;

	default:
	  if (complain & tf_error)
	    {
		/* If one of the arguments of the operator represents
		   an invalid use of member function pointer, try to report
		   a meaningful error ...  */
		if (invalid_nonstatic_memfn_p (arg1, tf_error)
		    || invalid_nonstatic_memfn_p (arg2, tf_error)
		    || invalid_nonstatic_memfn_p (arg3, tf_error))
		  /* We displayed the error message.  */;
		else
		  {
		    /* ... Otherwise, report the more generic
		       "no matching operator found" error */
		    op_error (loc, code, code2, arg1, arg2, arg3, FALSE);
		    print_z_candidates (loc, candidates);
		  }
	    }
	  result = error_mark_node;
	  break;
	}
    }
  else
    {
      cand = tourney (candidates, complain);
      if (cand == 0)
	{
	  if (complain & tf_error)
	    {
	      op_error (loc, code, code2, arg1, arg2, arg3, TRUE);
	      print_z_candidates (loc, candidates);
	    }
	  result = error_mark_node;
	}
      else if (TREE_CODE (cand->fn) == FUNCTION_DECL)
	{
	  if (overload)
	    *overload = cand->fn;

	  if (resolve_args (arglist, complain) == NULL)
	    result = error_mark_node;
	  else
	    result = build_over_call (cand, LOOKUP_NORMAL, complain);
	}
      else
	{
	  /* Give any warnings we noticed during overload resolution.  */
	  if (cand->warnings && (complain & tf_warning))
	    {
	      struct candidate_warning *w;
	      for (w = cand->warnings; w; w = w->next)
		joust (cand, w->loser, 1, complain);
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
		      != TYPE_MAIN_VARIANT (TREE_TYPE (arg2)))
		  && (complain & tf_warning))
		{
		  warning (OPT_Wenum_compare,
			   "comparison between %q#T and %q#T",
			   TREE_TYPE (arg1), TREE_TYPE (arg2));
		}
	      break;
	    default:
	      break;
	    }

	  /* We need to strip any leading REF_BIND so that bitfields
	     don't cause errors.  This should not remove any important
	     conversions, because builtins don't apply to class
	     objects directly.  */
	  conv = cand->convs[0];
	  if (conv->kind == ck_ref_bind)
	    conv = next_conversion (conv);
	  arg1 = convert_like (conv, arg1, complain);

	  if (arg2)
	    {
	      conv = cand->convs[1];
	      if (conv->kind == ck_ref_bind)
		conv = next_conversion (conv);
	      else
		arg2 = decay_conversion (arg2, complain);

	      /* We need to call warn_logical_operator before
		 converting arg2 to a boolean_type, but after
		 decaying an enumerator to its value.  */
	      if (complain & tf_warning)
		warn_logical_operator (loc, code, boolean_type_node,
				       code_orig_arg1, arg1,
				       code_orig_arg2, arg2);

	      arg2 = convert_like (conv, arg2, complain);
	    }
	  if (arg3)
	    {
	      conv = cand->convs[2];
	      if (conv->kind == ck_ref_bind)
		conv = next_conversion (conv);
	      arg3 = convert_like (conv, arg3, complain);
	    }

	}
    }

 user_defined_result_ready:

  /* Free all the conversions we allocated.  */
  obstack_free (&conversion_obstack, p);

  if (result || result_valid_p)
    return result;

 builtin:
  switch (code)
    {
    case MODIFY_EXPR:
      return cp_build_modify_expr (arg1, code2, arg2, complain);

    case INDIRECT_REF:
      return cp_build_indirect_ref (arg1, RO_UNARY_STAR, complain);

    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
      warn_logical_operator (loc, code, boolean_type_node,
			     code_orig_arg1, arg1, code_orig_arg2, arg2);
      /* Fall through.  */
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
      return cp_build_binary_op (loc, code, arg1, arg2, complain);

    case UNARY_PLUS_EXPR:
    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
    case TRUTH_NOT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case ABS_EXPR:
      return cp_build_unary_op (code, arg1, candidates != 0, complain);

    case ARRAY_REF:
      return cp_build_array_ref (input_location, arg1, arg2, complain);

    case MEMBER_REF:
      return build_m_component_ref (cp_build_indirect_ref (arg1, RO_ARROW_STAR, 
                                                           complain), 
                                    arg2, complain);

      /* The caller will deal with these.  */
    case ADDR_EXPR:
    case COMPONENT_REF:
    case COMPOUND_EXPR:
      return NULL_TREE;

    default:
      gcc_unreachable ();
    }
  return NULL_TREE;
}

/* Wrapper for above.  */

tree
build_new_op (location_t loc, enum tree_code code, int flags,
	      tree arg1, tree arg2, tree arg3,
	      tree *overload, tsubst_flags_t complain)
{
  tree ret;
  bool subtime = timevar_cond_start (TV_OVERLOAD);
  ret = build_new_op_1 (loc, code, flags, arg1, arg2, arg3,
			overload, complain);
  timevar_cond_stop (TV_OVERLOAD, subtime);
  return ret;
}

/* Returns true iff T, an element of an OVERLOAD chain, is a usual
   deallocation function (3.7.4.2 [basic.stc.dynamic.deallocation]).  */

static bool
non_placement_deallocation_fn_p (tree t)
{
  /* A template instance is never a usual deallocation function,
     regardless of its signature.  */
  if (TREE_CODE (t) == TEMPLATE_DECL
      || primary_template_instantiation_p (t))
    return false;

  /* If a class T has a member deallocation function named operator delete
     with exactly one parameter, then that function is a usual
     (non-placement) deallocation function. If class T does not declare
     such an operator delete but does declare a member deallocation
     function named operator delete with exactly two parameters, the second
     of which has type std::size_t (18.2), then this function is a usual
     deallocation function.  */
  t = FUNCTION_ARG_CHAIN (t);
  if (t == void_list_node
      || (t && same_type_p (TREE_VALUE (t), size_type_node)
	  && TREE_CHAIN (t) == void_list_node))
    return true;
  return false;
}

/* Build a call to operator delete.  This has to be handled very specially,
   because the restrictions on what signatures match are different from all
   other call instances.  For a normal delete, only a delete taking (void *)
   or (void *, size_t) is accepted.  For a placement delete, only an exact
   match with the placement new is accepted.

   CODE is either DELETE_EXPR or VEC_DELETE_EXPR.
   ADDR is the pointer to be deleted.
   SIZE is the size of the memory block to be deleted.
   GLOBAL_P is true if the delete-expression should not consider
   class-specific delete operators.
   PLACEMENT is the corresponding placement new call, or NULL_TREE.

   If this call to "operator delete" is being generated as part to
   deallocate memory allocated via a new-expression (as per [expr.new]
   which requires that if the initialization throws an exception then
   we call a deallocation function), then ALLOC_FN is the allocation
   function.  */

tree
build_op_delete_call (enum tree_code code, tree addr, tree size,
		      bool global_p, tree placement,
		      tree alloc_fn, tsubst_flags_t complain)
{
  tree fn = NULL_TREE;
  tree fns, fnname, type, t;

  if (addr == error_mark_node)
    return error_mark_node;

  type = strip_array_types (TREE_TYPE (TREE_TYPE (addr)));

  fnname = ansi_opname (code);

  if (CLASS_TYPE_P (type)
      && COMPLETE_TYPE_P (complete_type (type))
      && !global_p)
    /* In [class.free]

       If the result of the lookup is ambiguous or inaccessible, or if
       the lookup selects a placement deallocation function, the
       program is ill-formed.

       Therefore, we ask lookup_fnfields to complain about ambiguity.  */
    {
      fns = lookup_fnfields (TYPE_BINFO (type), fnname, 1);
      if (fns == error_mark_node)
	return error_mark_node;
    }
  else
    fns = NULL_TREE;

  if (fns == NULL_TREE)
    fns = lookup_name_nonclass (fnname);

  /* Strip const and volatile from addr.  */
  addr = cp_convert (ptr_type_node, addr, complain);

  if (placement)
    {
      /* "A declaration of a placement deallocation function matches the
	 declaration of a placement allocation function if it has the same
	 number of parameters and, after parameter transformations (8.3.5),
	 all parameter types except the first are identical."

	 So we build up the function type we want and ask instantiate_type
	 to get it for us.  */
      t = FUNCTION_ARG_CHAIN (alloc_fn);
      t = tree_cons (NULL_TREE, ptr_type_node, t);
      t = build_function_type (void_type_node, t);

      fn = instantiate_type (t, fns, tf_none);
      if (fn == error_mark_node)
	return NULL_TREE;

      if (BASELINK_P (fn))
	fn = BASELINK_FUNCTIONS (fn);

      /* "If the lookup finds the two-parameter form of a usual deallocation
	 function (3.7.4.2) and that function, considered as a placement
	 deallocation function, would have been selected as a match for the
	 allocation function, the program is ill-formed."  */
      if (non_placement_deallocation_fn_p (fn))
	{
	  /* But if the class has an operator delete (void *), then that is
	     the usual deallocation function, so we shouldn't complain
	     about using the operator delete (void *, size_t).  */
	  for (t = BASELINK_P (fns) ? BASELINK_FUNCTIONS (fns) : fns;
	       t; t = OVL_NEXT (t))
	    {
	      tree elt = OVL_CURRENT (t);
	      if (non_placement_deallocation_fn_p (elt)
		  && FUNCTION_ARG_CHAIN (elt) == void_list_node)
		goto ok;
	    }
	  if (complain & tf_error)
	    {
	      permerror (0, "non-placement deallocation function %q+D", fn);
	      permerror (input_location, "selected for placement delete");
	    }
	  else
	    return error_mark_node;
	ok:;
	}
    }
  else
    /* "Any non-placement deallocation function matches a non-placement
       allocation function. If the lookup finds a single matching
       deallocation function, that function will be called; otherwise, no
       deallocation function will be called."  */
    for (t = BASELINK_P (fns) ? BASELINK_FUNCTIONS (fns) : fns;
	 t; t = OVL_NEXT (t))
      {
	tree elt = OVL_CURRENT (t);
	if (non_placement_deallocation_fn_p (elt))
	  {
	    fn = elt;
	    /* "If a class T has a member deallocation function named
	       operator delete with exactly one parameter, then that
	       function is a usual (non-placement) deallocation
	       function. If class T does not declare such an operator
	       delete but does declare a member deallocation function named
	       operator delete with exactly two parameters, the second of
	       which has type std::size_t (18.2), then this function is a
	       usual deallocation function."

	       So (void*) beats (void*, size_t).  */
	    if (FUNCTION_ARG_CHAIN (fn) == void_list_node)
	      break;
	  }
      }

  /* If we have a matching function, call it.  */
  if (fn)
    {
      gcc_assert (TREE_CODE (fn) == FUNCTION_DECL);

      /* If the FN is a member function, make sure that it is
	 accessible.  */
      if (BASELINK_P (fns))
	perform_or_defer_access_check (BASELINK_BINFO (fns), fn, fn,
				       complain);

      /* Core issue 901: It's ok to new a type with deleted delete.  */
      if (DECL_DELETED_FN (fn) && alloc_fn)
	return NULL_TREE;

      if (placement)
	{
	  /* The placement args might not be suitable for overload
	     resolution at this point, so build the call directly.  */
	  int nargs = call_expr_nargs (placement);
	  tree *argarray = XALLOCAVEC (tree, nargs);
	  int i;
	  argarray[0] = addr;
	  for (i = 1; i < nargs; i++)
	    argarray[i] = CALL_EXPR_ARG (placement, i);
	  mark_used (fn);
	  return build_cxx_call (fn, nargs, argarray, complain);
	}
      else
	{
	  tree ret;
	  vec<tree, va_gc> *args = make_tree_vector ();
	  args->quick_push (addr);
	  if (FUNCTION_ARG_CHAIN (fn) != void_list_node)
	    args->quick_push (size);
	  ret = cp_build_function_call_vec (fn, &args, complain);
	  release_tree_vector (args);
	  return ret;
	}
    }

  /* [expr.new]

     If no unambiguous matching deallocation function can be found,
     propagating the exception does not cause the object's memory to
     be freed.  */
  if (alloc_fn)
    {
      if ((complain & tf_warning)
	  && !placement)
	warning (0, "no corresponding deallocation function for %qD",
		 alloc_fn);
      return NULL_TREE;
    }

  if (complain & tf_error)
    error ("no suitable %<operator %s%> for %qT",
	   operator_name_info[(int)code].name, type);
  return error_mark_node;
}

/* If the current scope isn't allowed to access DECL along
   BASETYPE_PATH, give an error.  The most derived class in
   BASETYPE_PATH is the one used to qualify DECL. DIAG_DECL is
   the declaration to use in the error diagnostic.  */

bool
enforce_access (tree basetype_path, tree decl, tree diag_decl,
		tsubst_flags_t complain)
{
  gcc_assert (TREE_CODE (basetype_path) == TREE_BINFO);

  if (!accessible_p (basetype_path, decl, true))
    {
      if (complain & tf_error)
	{
	  if (TREE_PRIVATE (decl))
	    error ("%q+#D is private", diag_decl);
	  else if (TREE_PROTECTED (decl))
	    error ("%q+#D is protected", diag_decl);
	  else
	    error ("%q+#D is inaccessible", diag_decl);
	  error ("within this context");
	}
      return false;
    }

  return true;
}

/* Initialize a temporary of type TYPE with EXPR.  The FLAGS are a
   bitwise or of LOOKUP_* values.  If any errors are warnings are
   generated, set *DIAGNOSTIC_FN to "error" or "warning",
   respectively.  If no diagnostics are generated, set *DIAGNOSTIC_FN
   to NULL.  */

static tree
build_temp (tree expr, tree type, int flags,
	    diagnostic_t *diagnostic_kind, tsubst_flags_t complain)
{
  int savew, savee;
  vec<tree, va_gc> *args;

  savew = warningcount + werrorcount, savee = errorcount;
  args = make_tree_vector_single (expr);
  expr = build_special_member_call (NULL_TREE, complete_ctor_identifier,
				    &args, type, flags, complain);
  release_tree_vector (args);
  if (warningcount + werrorcount > savew)
    *diagnostic_kind = DK_WARNING;
  else if (errorcount > savee)
    *diagnostic_kind = DK_ERROR;
  else
    *diagnostic_kind = DK_UNSPECIFIED;
  return expr;
}

/* Perform warnings about peculiar, but valid, conversions from/to NULL.
   EXPR is implicitly converted to type TOTYPE.
   FN and ARGNUM are used for diagnostics.  */

static void
conversion_null_warnings (tree totype, tree expr, tree fn, int argnum)
{
  /* Issue warnings about peculiar, but valid, uses of NULL.  */
  if (expr == null_node && TREE_CODE (totype) != BOOLEAN_TYPE
      && ARITHMETIC_TYPE_P (totype))
    {
      source_location loc =
	expansion_point_location_if_in_system_header (input_location);

      if (fn)
	warning_at (loc, OPT_Wconversion_null,
		    "passing NULL to non-pointer argument %P of %qD",
		    argnum, fn);
      else
	warning_at (loc, OPT_Wconversion_null,
		    "converting to non-pointer type %qT from NULL", totype);
    }

  /* Issue warnings if "false" is converted to a NULL pointer */
  else if (TREE_CODE (TREE_TYPE (expr)) == BOOLEAN_TYPE
	   && TYPE_PTR_P (totype))
    {
      if (fn)
	warning_at (input_location, OPT_Wconversion_null,
		    "converting %<false%> to pointer type for argument %P "
		    "of %qD", argnum, fn);
      else
	warning_at (input_location, OPT_Wconversion_null,
		    "converting %<false%> to pointer type %qT", totype);
    }
}

/* We gave a diagnostic during a conversion.  If this was in the second
   standard conversion sequence of a user-defined conversion sequence, say
   which user-defined conversion.  */

static void
maybe_print_user_conv_context (conversion *convs)
{
  if (convs->user_conv_p)
    for (conversion *t = convs; t; t = next_conversion (t))
      if (t->kind == ck_user)
	{
	  print_z_candidate (0, "  after user-defined conversion:",
			     t->cand);
	  break;
	}
}

/* Perform the conversions in CONVS on the expression EXPR.  FN and
   ARGNUM are used for diagnostics.  ARGNUM is zero based, -1
   indicates the `this' argument of a method.  INNER is nonzero when
   being called to continue a conversion chain. It is negative when a
   reference binding will be applied, positive otherwise.  If
   ISSUE_CONVERSION_WARNINGS is true, warnings about suspicious
   conversions will be emitted if appropriate.  If C_CAST_P is true,
   this conversion is coming from a C-style cast; in that case,
   conversions to inaccessible bases are permitted.  */

static tree
convert_like_real (conversion *convs, tree expr, tree fn, int argnum,
		   int inner, bool issue_conversion_warnings,
		   bool c_cast_p, tsubst_flags_t complain)
{
  tree totype = convs->type;
  diagnostic_t diag_kind;
  int flags;
  location_t loc = EXPR_LOC_OR_LOC (expr, input_location);

  if (convs->bad_p && !(complain & tf_error))
    return error_mark_node;

  if (convs->bad_p
      && convs->kind != ck_user
      && convs->kind != ck_list
      && convs->kind != ck_ambig
      && (convs->kind != ck_ref_bind
	  || (convs->user_conv_p && next_conversion (convs)->bad_p))
      && (convs->kind != ck_rvalue
	  || SCALAR_TYPE_P (totype))
      && convs->kind != ck_base)
    {
      bool complained = false;
      conversion *t = convs;

      /* Give a helpful error if this is bad because of excess braces.  */
      if (BRACE_ENCLOSED_INITIALIZER_P (expr)
	  && SCALAR_TYPE_P (totype)
	  && CONSTRUCTOR_NELTS (expr) > 0
	  && BRACE_ENCLOSED_INITIALIZER_P (CONSTRUCTOR_ELT (expr, 0)->value))
	{
	  complained = permerror (loc, "too many braces around initializer "
				  "for %qT", totype);
	  while (BRACE_ENCLOSED_INITIALIZER_P (expr)
		 && CONSTRUCTOR_NELTS (expr) == 1)
	    expr = CONSTRUCTOR_ELT (expr, 0)->value;
	}

      /* Give a helpful error if this is bad because a conversion to bool
	 from std::nullptr_t requires direct-initialization.  */
      if (NULLPTR_TYPE_P (TREE_TYPE (expr))
	  && TREE_CODE (totype) == BOOLEAN_TYPE)
	complained = permerror (loc, "converting to %qT from %qT requires "
				"direct-initialization",
				totype, TREE_TYPE (expr));

      for (; t ; t = next_conversion (t))
	{
	  if (t->kind == ck_user && t->cand->reason)
	    {
	      permerror (loc, "invalid user-defined conversion "
			 "from %qT to %qT", TREE_TYPE (expr), totype);
	      print_z_candidate (loc, "candidate is:", t->cand);
	      expr = convert_like_real (t, expr, fn, argnum, 1,
					/*issue_conversion_warnings=*/false,
					/*c_cast_p=*/false,
					complain);
	      if (convs->kind == ck_ref_bind)
		expr = convert_to_reference (totype, expr, CONV_IMPLICIT,
					     LOOKUP_NORMAL, NULL_TREE,
					     complain);
	      else
		expr = cp_convert (totype, expr, complain);
	      if (fn)
		inform (DECL_SOURCE_LOCATION (fn),
			"  initializing argument %P of %qD", argnum, fn);
	      return expr;
	    }
	  else if (t->kind == ck_user || !t->bad_p)
	    {
	      expr = convert_like_real (t, expr, fn, argnum, 1,
					/*issue_conversion_warnings=*/false,
					/*c_cast_p=*/false,
					complain);
	      break;
	    }
	  else if (t->kind == ck_ambig)
	    return convert_like_real (t, expr, fn, argnum, 1,
				      /*issue_conversion_warnings=*/false,
				      /*c_cast_p=*/false,
				      complain);
	  else if (t->kind == ck_identity)
	    break;
	}
      if (!complained)
	complained = permerror (loc, "invalid conversion from %qT to %qT",
				TREE_TYPE (expr), totype);
      if (complained && fn)
	inform (DECL_SOURCE_LOCATION (fn),
		"  initializing argument %P of %qD", argnum, fn);

      return cp_convert (totype, expr, complain);
    }

  if (issue_conversion_warnings && (complain & tf_warning))
    conversion_null_warnings (totype, expr, fn, argnum);

  switch (convs->kind)
    {
    case ck_user:
      {
	struct z_candidate *cand = convs->cand;
	tree convfn = cand->fn;
	unsigned i;

	/* When converting from an init list we consider explicit
	   constructors, but actually trying to call one is an error.  */
	if (DECL_NONCONVERTING_P (convfn) && DECL_CONSTRUCTOR_P (convfn)
	    /* Unless this is for direct-list-initialization.  */
	    && !DIRECT_LIST_INIT_P (expr))
	  {
	    if (!(complain & tf_error))
	      return error_mark_node;
	    error ("converting to %qT from initializer list would use "
		   "explicit constructor %qD", totype, convfn);
	  }

	/* If we're initializing from {}, it's value-initialization.  */
	if (BRACE_ENCLOSED_INITIALIZER_P (expr)
	    && CONSTRUCTOR_NELTS (expr) == 0
	    && TYPE_HAS_DEFAULT_CONSTRUCTOR (totype))
	  {
	    bool direct = CONSTRUCTOR_IS_DIRECT_INIT (expr);
	    expr = build_value_init (totype, complain);
	    expr = get_target_expr_sfinae (expr, complain);
	    if (expr != error_mark_node)
	      {
		TARGET_EXPR_LIST_INIT_P (expr) = true;
		TARGET_EXPR_DIRECT_INIT_P (expr) = direct;
	      }
	    return expr;
	  }

	expr = mark_rvalue_use (expr);

	/* Set user_conv_p on the argument conversions, so rvalue/base
	   handling knows not to allow any more UDCs.  */
	for (i = 0; i < cand->num_convs; ++i)
	  cand->convs[i]->user_conv_p = true;

	expr = build_over_call (cand, LOOKUP_NORMAL, complain);

	/* If this is a constructor or a function returning an aggr type,
	   we need to build up a TARGET_EXPR.  */
	if (DECL_CONSTRUCTOR_P (convfn))
	  {
	    expr = build_cplus_new (totype, expr, complain);

	    /* Remember that this was list-initialization.  */
	    if (convs->check_narrowing && expr != error_mark_node)
	      TARGET_EXPR_LIST_INIT_P (expr) = true;
	  }

	return expr;
      }
    case ck_identity:
      expr = mark_rvalue_use (expr);
      if (BRACE_ENCLOSED_INITIALIZER_P (expr))
	{
	  int nelts = CONSTRUCTOR_NELTS (expr);
	  if (nelts == 0)
	    expr = build_value_init (totype, complain);
	  else if (nelts == 1)
	    expr = CONSTRUCTOR_ELT (expr, 0)->value;
	  else
	    gcc_unreachable ();
	}

      if (type_unknown_p (expr))
	expr = instantiate_type (totype, expr, complain);
      /* Convert a constant to its underlying value, unless we are
	 about to bind it to a reference, in which case we need to
	 leave it as an lvalue.  */
      if (inner >= 0)
        {   
          expr = decl_constant_value_safe (expr);
          if (expr == null_node && INTEGRAL_OR_UNSCOPED_ENUMERATION_TYPE_P (totype))
            /* If __null has been converted to an integer type, we do not
               want to warn about uses of EXPR as an integer, rather than
               as a pointer.  */
            expr = build_int_cst (totype, 0);
        }
      return expr;
    case ck_ambig:
      /* We leave bad_p off ck_ambig because overload resolution considers
	 it valid, it just fails when we try to perform it.  So we need to
         check complain here, too.  */
      if (complain & tf_error)
	{
	  /* Call build_user_type_conversion again for the error.  */
	  build_user_type_conversion (totype, convs->u.expr, LOOKUP_NORMAL,
				      complain);
	  if (fn)
	    inform (input_location, "  initializing argument %P of %q+D",
		    argnum, fn);
	}
      return error_mark_node;

    case ck_list:
      {
	/* Conversion to std::initializer_list<T>.  */
	tree elttype = TREE_VEC_ELT (CLASSTYPE_TI_ARGS (totype), 0);
	tree new_ctor = build_constructor (init_list_type_node, NULL);
	unsigned len = CONSTRUCTOR_NELTS (expr);
	tree array, val, field;
	vec<constructor_elt, va_gc> *vec = NULL;
	unsigned ix;

	/* Convert all the elements.  */
	FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (expr), ix, val)
	  {
	    tree sub = convert_like_real (convs->u.list[ix], val, fn, argnum,
					  1, false, false, complain);
	    if (sub == error_mark_node)
	      return sub;
	    if (!BRACE_ENCLOSED_INITIALIZER_P (val))
	      check_narrowing (TREE_TYPE (sub), val);
	    CONSTRUCTOR_APPEND_ELT (CONSTRUCTOR_ELTS (new_ctor), NULL_TREE, sub);
	    if (!TREE_CONSTANT (sub))
	      TREE_CONSTANT (new_ctor) = false;
	  }
	/* Build up the array.  */
	elttype = cp_build_qualified_type
	  (elttype, cp_type_quals (elttype) | TYPE_QUAL_CONST);
	array = build_array_of_n_type (elttype, len);
	array = finish_compound_literal (array, new_ctor, complain);
	/* Take the address explicitly rather than via decay_conversion
	   to avoid the error about taking the address of a temporary.  */
	array = cp_build_addr_expr (array, complain);
	array = cp_convert (build_pointer_type (elttype), array, complain);
	if (array == error_mark_node)
	  return error_mark_node;

	/* Build up the initializer_list object.  */
	totype = complete_type (totype);
	field = next_initializable_field (TYPE_FIELDS (totype));
	CONSTRUCTOR_APPEND_ELT (vec, field, array);
	field = next_initializable_field (DECL_CHAIN (field));
	CONSTRUCTOR_APPEND_ELT (vec, field, size_int (len));
	new_ctor = build_constructor (totype, vec);
	return get_target_expr_sfinae (new_ctor, complain);
      }

    case ck_aggr:
      if (TREE_CODE (totype) == COMPLEX_TYPE)
	{
	  tree real = CONSTRUCTOR_ELT (expr, 0)->value;
	  tree imag = CONSTRUCTOR_ELT (expr, 1)->value;
	  real = perform_implicit_conversion (TREE_TYPE (totype),
					      real, complain);
	  imag = perform_implicit_conversion (TREE_TYPE (totype),
					      imag, complain);
	  expr = build2 (COMPLEX_EXPR, totype, real, imag);
	  return fold_if_not_in_template (expr);
	}
      expr = reshape_init (totype, expr, complain);
      expr = get_target_expr_sfinae (digest_init (totype, expr, complain),
				     complain);
      if (expr != error_mark_node)
	TARGET_EXPR_LIST_INIT_P (expr) = true;
      return expr;

    default:
      break;
    };

  expr = convert_like_real (next_conversion (convs), expr, fn, argnum,
			    convs->kind == ck_ref_bind ? -1 : 1,
			    convs->kind == ck_ref_bind ? issue_conversion_warnings : false, 
			    c_cast_p,
			    complain);
  if (expr == error_mark_node)
    return error_mark_node;

  switch (convs->kind)
    {
    case ck_rvalue:
      expr = decay_conversion (expr, complain);
      if (expr == error_mark_node)
	return error_mark_node;

      if (! MAYBE_CLASS_TYPE_P (totype))
	return expr;
      /* Else fall through.  */
    case ck_base:
      if (convs->kind == ck_base && !convs->need_temporary_p)
	{
	  /* We are going to bind a reference directly to a base-class
	     subobject of EXPR.  */
	  /* Build an expression for `*((base*) &expr)'.  */
	  expr = cp_build_addr_expr (expr, complain);
	  expr = convert_to_base (expr, build_pointer_type (totype),
				  !c_cast_p, /*nonnull=*/true, complain);
	  expr = cp_build_indirect_ref (expr, RO_IMPLICIT_CONVERSION, complain);
	  return expr;
	}

      /* Copy-initialization where the cv-unqualified version of the source
	 type is the same class as, or a derived class of, the class of the
	 destination [is treated as direct-initialization].  [dcl.init] */
      flags = LOOKUP_NORMAL|LOOKUP_ONLYCONVERTING;
      if (convs->user_conv_p)
	/* This conversion is being done in the context of a user-defined
	   conversion (i.e. the second step of copy-initialization), so
	   don't allow any more.  */
	flags |= LOOKUP_NO_CONVERSION;
      if (convs->rvaluedness_matches_p)
	flags |= LOOKUP_PREFER_RVALUE;
      if (TREE_CODE (expr) == TARGET_EXPR
	  && TARGET_EXPR_LIST_INIT_P (expr))
	/* Copy-list-initialization doesn't actually involve a copy.  */
	return expr;
      expr = build_temp (expr, totype, flags, &diag_kind, complain);
      if (diag_kind && complain)
	{
	  maybe_print_user_conv_context (convs);
	  if (fn)
	    inform (DECL_SOURCE_LOCATION (fn),
		    "  initializing argument %P of %qD", argnum, fn);
	}

      return build_cplus_new (totype, expr, complain);

    case ck_ref_bind:
      {
	tree ref_type = totype;

	if (convs->bad_p && !next_conversion (convs)->bad_p)
	  {
	    tree extype = TREE_TYPE (expr);
	    if (TYPE_REF_IS_RVALUE (ref_type)
		&& real_lvalue_p (expr))
	      error_at (loc, "cannot bind %qT lvalue to %qT",
			extype, totype);
	    else if (!TYPE_REF_IS_RVALUE (ref_type) && !real_lvalue_p (expr)
		     && !CP_TYPE_CONST_NON_VOLATILE_P (TREE_TYPE (ref_type)))
	      error_at (loc, "invalid initialization of non-const reference of "
			"type %qT from an rvalue of type %qT", totype, extype);
	    else if (!reference_compatible_p (TREE_TYPE (totype), extype))
	      error_at (loc, "binding %qT to reference of type %qT "
			"discards qualifiers", extype, totype);
	    else
	      gcc_unreachable ();
	    maybe_print_user_conv_context (convs);
	    if (fn)
	      inform (input_location,
		      "  initializing argument %P of %q+D", argnum, fn);
	    return error_mark_node;
	  }

	/* If necessary, create a temporary. 

           VA_ARG_EXPR and CONSTRUCTOR expressions are special cases
           that need temporaries, even when their types are reference
           compatible with the type of reference being bound, so the
           upcoming call to cp_build_addr_expr doesn't fail.  */
	if (convs->need_temporary_p
	    || TREE_CODE (expr) == CONSTRUCTOR
	    || TREE_CODE (expr) == VA_ARG_EXPR)
	  {
	    /* Otherwise, a temporary of type "cv1 T1" is created and
	       initialized from the initializer expression using the rules
	       for a non-reference copy-initialization (8.5).  */

	    tree type = TREE_TYPE (ref_type);
	    cp_lvalue_kind lvalue = real_lvalue_p (expr);

	    gcc_assert (same_type_ignoring_top_level_qualifiers_p
			(type, next_conversion (convs)->type));
	    if (!CP_TYPE_CONST_NON_VOLATILE_P (type)
		&& !TYPE_REF_IS_RVALUE (ref_type))
	      {
		/* If the reference is volatile or non-const, we
		   cannot create a temporary.  */
		if (lvalue & clk_bitfield)
		  error_at (loc, "cannot bind bitfield %qE to %qT",
			    expr, ref_type);
		else if (lvalue & clk_packed)
		  error_at (loc, "cannot bind packed field %qE to %qT",
			    expr, ref_type);
		else
		  error_at (loc, "cannot bind rvalue %qE to %qT",
			    expr, ref_type);
		return error_mark_node;
	      }
	    /* If the source is a packed field, and we must use a copy
	       constructor, then building the target expr will require
	       binding the field to the reference parameter to the
	       copy constructor, and we'll end up with an infinite
	       loop.  If we can use a bitwise copy, then we'll be
	       OK.  */
	    if ((lvalue & clk_packed)
		&& CLASS_TYPE_P (type)
		&& type_has_nontrivial_copy_init (type))
	      {
		error_at (loc, "cannot bind packed field %qE to %qT",
			  expr, ref_type);
		return error_mark_node;
	      }
	    if (lvalue & clk_bitfield)
	      {
		expr = convert_bitfield_to_declared_type (expr);
		expr = fold_convert (type, expr);
	      }
	    expr = build_target_expr_with_type (expr, type, complain);
	  }

	/* Take the address of the thing to which we will bind the
	   reference.  */
	expr = cp_build_addr_expr (expr, complain);
	if (expr == error_mark_node)
	  return error_mark_node;

	/* Convert it to a pointer to the type referred to by the
	   reference.  This will adjust the pointer if a derived to
	   base conversion is being performed.  */
	expr = cp_convert (build_pointer_type (TREE_TYPE (ref_type)),
			   expr, complain);
	/* Convert the pointer to the desired reference type.  */
	return build_nop (ref_type, expr);
      }

    case ck_lvalue:
      return decay_conversion (expr, complain);

    case ck_qual:
      /* Warn about deprecated conversion if appropriate.  */
      string_conv_p (totype, expr, 1);
      break;

    case ck_ptr:
      if (convs->base_p)
	expr = convert_to_base (expr, totype, !c_cast_p,
				/*nonnull=*/false, complain);
      return build_nop (totype, expr);

    case ck_pmem:
      return convert_ptrmem (totype, expr, /*allow_inverse_p=*/false,
			     c_cast_p, complain);

    default:
      break;
    }

  if (convs->check_narrowing)
    check_narrowing (totype, expr);

  if (issue_conversion_warnings)
    expr = cp_convert_and_check (totype, expr, complain);
  else
    expr = cp_convert (totype, expr, complain);

  return expr;
}

/* ARG is being passed to a varargs function.  Perform any conversions
   required.  Return the converted value.  */

tree
convert_arg_to_ellipsis (tree arg, tsubst_flags_t complain)
{
  tree arg_type;
  location_t loc = EXPR_LOC_OR_LOC (arg, input_location);

  /* [expr.call]

     The lvalue-to-rvalue, array-to-pointer, and function-to-pointer
     standard conversions are performed.  */
  arg = decay_conversion (arg, complain);
  arg_type = TREE_TYPE (arg);
  /* [expr.call]

     If the argument has integral or enumeration type that is subject
     to the integral promotions (_conv.prom_), or a floating point
     type that is subject to the floating point promotion
     (_conv.fpprom_), the value of the argument is converted to the
     promoted type before the call.  */
  if (TREE_CODE (arg_type) == REAL_TYPE
      && (TYPE_PRECISION (arg_type)
	  < TYPE_PRECISION (double_type_node))
      && !DECIMAL_FLOAT_MODE_P (TYPE_MODE (arg_type)))
    {
      if ((complain & tf_warning)
	  && warn_double_promotion && !c_inhibit_evaluation_warnings)
	warning_at (loc, OPT_Wdouble_promotion,
		    "implicit conversion from %qT to %qT when passing "
		    "argument to function",
		    arg_type, double_type_node);
      arg = convert_to_real (double_type_node, arg);
    }
  else if (NULLPTR_TYPE_P (arg_type))
    arg = null_pointer_node;
  else if (INTEGRAL_OR_ENUMERATION_TYPE_P (arg_type))
    {
      if (SCOPED_ENUM_P (arg_type))
	{
	  tree prom = cp_convert (ENUM_UNDERLYING_TYPE (arg_type), arg,
				  complain);
	  prom = cp_perform_integral_promotions (prom, complain);
	  if (abi_version_crosses (6)
	      && TYPE_MODE (TREE_TYPE (prom)) != TYPE_MODE (arg_type)
	      && (complain & tf_warning))
	    warning_at (loc, OPT_Wabi, "scoped enum %qT passed through ... as "
			"%qT before -fabi-version=6, %qT after", arg_type,
			TREE_TYPE (prom), ENUM_UNDERLYING_TYPE (arg_type));
	  if (!abi_version_at_least (6))
	    arg = prom;
	}
      else
	arg = cp_perform_integral_promotions (arg, complain);
    }

  arg = require_complete_type_sfinae (arg, complain);
  arg_type = TREE_TYPE (arg);

  if (arg != error_mark_node
      /* In a template (or ill-formed code), we can have an incomplete type
	 even after require_complete_type_sfinae, in which case we don't know
	 whether it has trivial copy or not.  */
      && COMPLETE_TYPE_P (arg_type))
    {
      /* Build up a real lvalue-to-rvalue conversion in case the
	 copy constructor is trivial but not callable.  */
      if (!cp_unevaluated_operand && CLASS_TYPE_P (arg_type))
	force_rvalue (arg, complain);

      /* [expr.call] 5.2.2/7:
	 Passing a potentially-evaluated argument of class type (Clause 9)
	 with a non-trivial copy constructor or a non-trivial destructor
	 with no corresponding parameter is conditionally-supported, with
	 implementation-defined semantics.

	 We used to just warn here and do a bitwise copy, but now
	 cp_expr_size will abort if we try to do that.

	 If the call appears in the context of a sizeof expression,
	 it is not potentially-evaluated.  */
      if (cp_unevaluated_operand == 0
	  && (type_has_nontrivial_copy_init (arg_type)
	      || TYPE_HAS_NONTRIVIAL_DESTRUCTOR (arg_type)))
	{
	  if (complain & tf_error)
	    error_at (loc, "cannot pass objects of non-trivially-copyable "
		      "type %q#T through %<...%>", arg_type);
	  return error_mark_node;
	}
    }

  return arg;
}

/* va_arg (EXPR, TYPE) is a builtin. Make sure it is not abused.  */

tree
build_x_va_arg (source_location loc, tree expr, tree type)
{
  if (processing_template_decl)
    return build_min (VA_ARG_EXPR, type, expr);

  type = complete_type_or_else (type, NULL_TREE);

  if (expr == error_mark_node || !type)
    return error_mark_node;

  expr = mark_lvalue_use (expr);

  if (type_has_nontrivial_copy_init (type)
      || TYPE_HAS_NONTRIVIAL_DESTRUCTOR (type)
      || TREE_CODE (type) == REFERENCE_TYPE)
    {
      /* Remove reference types so we don't ICE later on.  */
      tree type1 = non_reference (type);
      /* conditionally-supported behavior [expr.call] 5.2.2/7.  */
      error ("cannot receive objects of non-trivially-copyable type %q#T "
	     "through %<...%>; ", type);
      expr = convert (build_pointer_type (type1), null_node);
      expr = cp_build_indirect_ref (expr, RO_NULL, tf_warning_or_error);
      return expr;
    }

  return build_va_arg (loc, expr, type);
}

/* TYPE has been given to va_arg.  Apply the default conversions which
   would have happened when passed via ellipsis.  Return the promoted
   type, or the passed type if there is no change.  */

tree
cxx_type_promotes_to (tree type)
{
  tree promote;

  /* Perform the array-to-pointer and function-to-pointer
     conversions.  */
  type = type_decays_to (type);

  promote = type_promotes_to (type);
  if (same_type_p (type, promote))
    promote = type;

  return promote;
}

/* ARG is a default argument expression being passed to a parameter of
   the indicated TYPE, which is a parameter to FN.  PARMNUM is the
   zero-based argument number.  Do any required conversions.  Return
   the converted value.  */

static GTY(()) vec<tree, va_gc> *default_arg_context;
void
push_defarg_context (tree fn)
{ vec_safe_push (default_arg_context, fn); }

void
pop_defarg_context (void)
{ default_arg_context->pop (); }

tree
convert_default_arg (tree type, tree arg, tree fn, int parmnum,
		     tsubst_flags_t complain)
{
  int i;
  tree t;

  /* See through clones.  */
  fn = DECL_ORIGIN (fn);

  /* Detect recursion.  */
  FOR_EACH_VEC_SAFE_ELT (default_arg_context, i, t)
    if (t == fn)
      {
	if (complain & tf_error)
	  error ("recursive evaluation of default argument for %q#D", fn);
	return error_mark_node;
      }

  /* If the ARG is an unparsed default argument expression, the
     conversion cannot be performed.  */
  if (TREE_CODE (arg) == DEFAULT_ARG)
    {
      if (complain & tf_error)
	error ("call to %qD uses the default argument for parameter %P, which "
	       "is not yet defined", fn, parmnum);
      return error_mark_node;
    }

  push_defarg_context (fn);

  if (fn && DECL_TEMPLATE_INFO (fn))
    arg = tsubst_default_argument (fn, type, arg, complain);

  /* Due to:

       [dcl.fct.default]

       The names in the expression are bound, and the semantic
       constraints are checked, at the point where the default
       expressions appears.

     we must not perform access checks here.  */
  push_deferring_access_checks (dk_no_check);
  /* We must make a copy of ARG, in case subsequent processing
     alters any part of it.  */
  arg = break_out_target_exprs (arg);
  arg = convert_for_initialization (0, type, arg, LOOKUP_IMPLICIT,
				    ICR_DEFAULT_ARGUMENT, fn, parmnum,
				    complain);
  arg = convert_for_arg_passing (type, arg, complain);
  pop_deferring_access_checks();

  pop_defarg_context ();

  return arg;
}

/* Returns the type which will really be used for passing an argument of
   type TYPE.  */

tree
type_passed_as (tree type)
{
  /* Pass classes with copy ctors by invisible reference.  */
  if (TREE_ADDRESSABLE (type))
    {
      type = build_reference_type (type);
      /* There are no other pointers to this temporary.  */
      type = cp_build_qualified_type (type, TYPE_QUAL_RESTRICT);
    }
  else if (targetm.calls.promote_prototypes (type)
	   && INTEGRAL_TYPE_P (type)
	   && COMPLETE_TYPE_P (type)
	   && tree_int_cst_lt (TYPE_SIZE (type), TYPE_SIZE (integer_type_node)))
    type = integer_type_node;

  return type;
}

/* Actually perform the appropriate conversion.  */

tree
convert_for_arg_passing (tree type, tree val, tsubst_flags_t complain)
{
  tree bitfield_type;

  /* If VAL is a bitfield, then -- since it has already been converted
     to TYPE -- it cannot have a precision greater than TYPE.  

     If it has a smaller precision, we must widen it here.  For
     example, passing "int f:3;" to a function expecting an "int" will
     not result in any conversion before this point.

     If the precision is the same we must not risk widening.  For
     example, the COMPONENT_REF for a 32-bit "long long" bitfield will
     often have type "int", even though the C++ type for the field is
     "long long".  If the value is being passed to a function
     expecting an "int", then no conversions will be required.  But,
     if we call convert_bitfield_to_declared_type, the bitfield will
     be converted to "long long".  */
  bitfield_type = is_bitfield_expr_with_lowered_type (val);
  if (bitfield_type 
      && TYPE_PRECISION (TREE_TYPE (val)) < TYPE_PRECISION (type))
    val = convert_to_integer (TYPE_MAIN_VARIANT (bitfield_type), val);

  if (val == error_mark_node)
    ;
  /* Pass classes with copy ctors by invisible reference.  */
  else if (TREE_ADDRESSABLE (type))
    val = build1 (ADDR_EXPR, build_reference_type (type), val);
  else if (targetm.calls.promote_prototypes (type)
	   && INTEGRAL_TYPE_P (type)
	   && COMPLETE_TYPE_P (type)
	   && tree_int_cst_lt (TYPE_SIZE (type), TYPE_SIZE (integer_type_node)))
    val = cp_perform_integral_promotions (val, complain);
  if ((complain & tf_warning)
      && warn_suggest_attribute_format)
    {
      tree rhstype = TREE_TYPE (val);
      const enum tree_code coder = TREE_CODE (rhstype);
      const enum tree_code codel = TREE_CODE (type);
      if ((codel == POINTER_TYPE || codel == REFERENCE_TYPE)
	  && coder == codel
	  && check_missing_format_attribute (type, rhstype))
	warning (OPT_Wsuggest_attribute_format,
		 "argument of function call might be a candidate for a format attribute");
    }
  return val;
}

/* Returns true iff FN is a function with magic varargs, i.e. ones for
   which no conversions at all should be done.  This is true for some
   builtins which don't act like normal functions.  */

bool
magic_varargs_p (tree fn)
{
  if (flag_cilkplus && is_cilkplus_reduce_builtin (fn) != BUILT_IN_NONE)
    return true;

  if (DECL_BUILT_IN (fn))
    switch (DECL_FUNCTION_CODE (fn))
      {
      case BUILT_IN_CLASSIFY_TYPE:
      case BUILT_IN_CONSTANT_P:
      case BUILT_IN_NEXT_ARG:
      case BUILT_IN_VA_START:
	return true;

      default:;
	return lookup_attribute ("type generic",
				 TYPE_ATTRIBUTES (TREE_TYPE (fn))) != 0;
      }

  return false;
}

/* Returns the decl of the dispatcher function if FN is a function version.  */

tree
get_function_version_dispatcher (tree fn)
{
  tree dispatcher_decl = NULL;

  gcc_assert (TREE_CODE (fn) == FUNCTION_DECL
	      && DECL_FUNCTION_VERSIONED (fn));

  gcc_assert (targetm.get_function_versions_dispatcher);
  dispatcher_decl = targetm.get_function_versions_dispatcher (fn);

  if (dispatcher_decl == NULL)
    {
      error_at (input_location, "use of multiversioned function "
				"without a default");
      return NULL;
    }

  retrofit_lang_decl (dispatcher_decl);
  gcc_assert (dispatcher_decl != NULL);
  return dispatcher_decl;
}

/* fn is a function version dispatcher that is marked used. Mark all the 
   semantically identical function versions it will dispatch as used.  */

void
mark_versions_used (tree fn)
{
  struct cgraph_node *node;
  struct cgraph_function_version_info *node_v;
  struct cgraph_function_version_info *it_v;

  gcc_assert (TREE_CODE (fn) == FUNCTION_DECL);

  node = cgraph_get_node (fn);
  if (node == NULL)
    return;

  gcc_assert (node->dispatcher_function);

  node_v = get_cgraph_node_version (node);
  if (node_v == NULL)
    return;

  /* All semantically identical versions are chained.  Traverse and mark each
     one of them as used.  */
  it_v = node_v->next;
  while (it_v != NULL)
    {
      mark_used (it_v->this_node->decl);
      it_v = it_v->next;
    }
}

/* Subroutine of the various build_*_call functions.  Overload resolution
   has chosen a winning candidate CAND; build up a CALL_EXPR accordingly.
   ARGS is a TREE_LIST of the unconverted arguments to the call.  FLAGS is a
   bitmask of various LOOKUP_* flags which apply to the call itself.  */

static tree
build_over_call (struct z_candidate *cand, int flags, tsubst_flags_t complain)
{
  tree fn = cand->fn;
  const vec<tree, va_gc> *args = cand->args;
  tree first_arg = cand->first_arg;
  conversion **convs = cand->convs;
  conversion *conv;
  tree parm = TYPE_ARG_TYPES (TREE_TYPE (fn));
  int parmlen;
  tree val;
  int i = 0;
  int j = 0;
  unsigned int arg_index = 0;
  int is_method = 0;
  int nargs;
  tree *argarray;
  bool already_used = false;

  /* In a template, there is no need to perform all of the work that
     is normally done.  We are only interested in the type of the call
     expression, i.e., the return type of the function.  Any semantic
     errors will be deferred until the template is instantiated.  */
  if (processing_template_decl)
    {
      tree expr, addr;
      tree return_type;
      const tree *argarray;
      unsigned int nargs;

      return_type = TREE_TYPE (TREE_TYPE (fn));
      nargs = vec_safe_length (args);
      if (first_arg == NULL_TREE)
	argarray = args->address ();
      else
	{
	  tree *alcarray;
	  unsigned int ix;
	  tree arg;

	  ++nargs;
	  alcarray = XALLOCAVEC (tree, nargs);
	  alcarray[0] = first_arg;
	  FOR_EACH_VEC_SAFE_ELT (args, ix, arg)
	    alcarray[ix + 1] = arg;
	  argarray = alcarray;
	}

      addr = build_addr_func (fn, complain);
      if (addr == error_mark_node)
	return error_mark_node;
      expr = build_call_array_loc (input_location, return_type,
				   addr, nargs, argarray);
      if (TREE_THIS_VOLATILE (fn) && cfun)
	current_function_returns_abnormally = 1;
      return convert_from_reference (expr);
    }

  /* Give any warnings we noticed during overload resolution.  */
  if (cand->warnings && (complain & tf_warning))
    {
      struct candidate_warning *w;
      for (w = cand->warnings; w; w = w->next)
	joust (cand, w->loser, 1, complain);
    }

  /* Make =delete work with SFINAE.  */
  if (DECL_DELETED_FN (fn) && !(complain & tf_error))
    return error_mark_node;

  if (DECL_FUNCTION_MEMBER_P (fn))
    {
      tree access_fn;
      /* If FN is a template function, two cases must be considered.
	 For example:

	   struct A {
	     protected:
	       template <class T> void f();
	   };
	   template <class T> struct B {
	     protected:
	       void g();
	   };
	   struct C : A, B<int> {
	     using A::f;	// #1
	     using B<int>::g;	// #2
	   };

	 In case #1 where `A::f' is a member template, DECL_ACCESS is
	 recorded in the primary template but not in its specialization.
	 We check access of FN using its primary template.

	 In case #2, where `B<int>::g' has a DECL_TEMPLATE_INFO simply
	 because it is a member of class template B, DECL_ACCESS is
	 recorded in the specialization `B<int>::g'.  We cannot use its
	 primary template because `B<T>::g' and `B<int>::g' may have
	 different access.  */
      if (DECL_TEMPLATE_INFO (fn)
	  && DECL_MEMBER_TEMPLATE_P (DECL_TI_TEMPLATE (fn)))
	access_fn = DECL_TI_TEMPLATE (fn);
      else
	access_fn = fn;
      if (!perform_or_defer_access_check (cand->access_path, access_fn,
					  fn, complain))
	return error_mark_node;
    }

  /* If we're checking for implicit delete, don't bother with argument
     conversions.  */
  if (flags & LOOKUP_SPECULATIVE)
    {
      if (DECL_DELETED_FN (fn))
	{
	  if (complain & tf_error)
	    mark_used (fn);
	  return error_mark_node;
	}
      if (cand->viable == 1)
	return fn;
      else if (!(complain & tf_error))
	/* Reject bad conversions now.  */
	return error_mark_node;
      /* else continue to get conversion error.  */
    }

  /* N3276 magic doesn't apply to nested calls.  */
  int decltype_flag = (complain & tf_decltype);
  complain &= ~tf_decltype;

  /* Find maximum size of vector to hold converted arguments.  */
  parmlen = list_length (parm);
  nargs = vec_safe_length (args) + (first_arg != NULL_TREE ? 1 : 0);
  if (parmlen > nargs)
    nargs = parmlen;
  argarray = XALLOCAVEC (tree, nargs);

  /* The implicit parameters to a constructor are not considered by overload
     resolution, and must be of the proper type.  */
  if (DECL_CONSTRUCTOR_P (fn))
    {
      tree object_arg;
      if (first_arg != NULL_TREE)
	{
	  object_arg = first_arg;
	  first_arg = NULL_TREE;
	}
      else
	{
	  object_arg = (*args)[arg_index];
	  ++arg_index;
	}
      argarray[j++] = build_this (object_arg);
      parm = TREE_CHAIN (parm);
      /* We should never try to call the abstract constructor.  */
      gcc_assert (!DECL_HAS_IN_CHARGE_PARM_P (fn));

      if (DECL_HAS_VTT_PARM_P (fn))
	{
	  argarray[j++] = (*args)[arg_index];
	  ++arg_index;
	  parm = TREE_CHAIN (parm);
	}
    }
  /* Bypass access control for 'this' parameter.  */
  else if (TREE_CODE (TREE_TYPE (fn)) == METHOD_TYPE)
    {
      tree parmtype = TREE_VALUE (parm);
      tree arg = build_this (first_arg != NULL_TREE
			     ? first_arg
			     : (*args)[arg_index]);
      tree argtype = TREE_TYPE (arg);
      tree converted_arg;
      tree base_binfo;

      if (convs[i]->bad_p)
	{
	  if (complain & tf_error)
	    {
	      if (permerror (input_location, "passing %qT as %<this%> "
			     "argument discards qualifiers",
			     TREE_TYPE (argtype)))
		inform (DECL_SOURCE_LOCATION (fn), "  in call to %qD", fn);
	    }
	  else
	    return error_mark_node;
	}

      /* See if the function member or the whole class type is declared
	 final and the call can be devirtualized.  */
      if (DECL_FINAL_P (fn)
	  || CLASSTYPE_FINAL (TYPE_METHOD_BASETYPE (TREE_TYPE (fn))))
	flags |= LOOKUP_NONVIRTUAL;

      /* [class.mfct.nonstatic]: If a nonstatic member function of a class
	 X is called for an object that is not of type X, or of a type
	 derived from X, the behavior is undefined.

	 So we can assume that anything passed as 'this' is non-null, and
	 optimize accordingly.  */
      gcc_assert (TYPE_PTR_P (parmtype));
      /* Convert to the base in which the function was declared.  */
      gcc_assert (cand->conversion_path != NULL_TREE);
      converted_arg = build_base_path (PLUS_EXPR,
				       arg,
				       cand->conversion_path,
				       1, complain);
      /* Check that the base class is accessible.  */
      if (!accessible_base_p (TREE_TYPE (argtype),
			      BINFO_TYPE (cand->conversion_path), true))
	{
	  if (complain & tf_error)
	    error ("%qT is not an accessible base of %qT",
		   BINFO_TYPE (cand->conversion_path),
		   TREE_TYPE (argtype));
	  else
	    return error_mark_node;
	}
      /* If fn was found by a using declaration, the conversion path
	 will be to the derived class, not the base declaring fn. We
	 must convert from derived to base.  */
      base_binfo = lookup_base (TREE_TYPE (TREE_TYPE (converted_arg)),
				TREE_TYPE (parmtype), ba_unique,
				NULL, complain);
      converted_arg = build_base_path (PLUS_EXPR, converted_arg,
				       base_binfo, 1, complain);

      argarray[j++] = converted_arg;
      parm = TREE_CHAIN (parm);
      if (first_arg != NULL_TREE)
	first_arg = NULL_TREE;
      else
	++arg_index;
      ++i;
      is_method = 1;
    }

  gcc_assert (first_arg == NULL_TREE);
  for (; arg_index < vec_safe_length (args) && parm;
       parm = TREE_CHAIN (parm), ++arg_index, ++i)
    {
      tree type = TREE_VALUE (parm);
      tree arg = (*args)[arg_index];
      bool conversion_warning = true;

      conv = convs[i];

      /* If the argument is NULL and used to (implicitly) instantiate a
         template function (and bind one of the template arguments to
         the type of 'long int'), we don't want to warn about passing NULL
         to non-pointer argument.
         For example, if we have this template function:

           template<typename T> void func(T x) {}

         we want to warn (when -Wconversion is enabled) in this case:

           void foo() {
             func<int>(NULL);
           }

         but not in this case:

           void foo() {
             func(NULL);
           }
      */
      if (arg == null_node
          && DECL_TEMPLATE_INFO (fn)
          && cand->template_decl
          && !(flags & LOOKUP_EXPLICIT_TMPL_ARGS))
        conversion_warning = false;

      /* Warn about initializer_list deduction that isn't currently in the
	 working draft.  */
      if (cxx_dialect > cxx98
	  && flag_deduce_init_list
	  && cand->template_decl
	  && is_std_init_list (non_reference (type))
	  && BRACE_ENCLOSED_INITIALIZER_P (arg))
	{
	  tree tmpl = TI_TEMPLATE (cand->template_decl);
	  tree realparm = chain_index (j, DECL_ARGUMENTS (cand->fn));
	  tree patparm = get_pattern_parm (realparm, tmpl);
	  tree pattype = TREE_TYPE (patparm);
	  if (PACK_EXPANSION_P (pattype))
	    pattype = PACK_EXPANSION_PATTERN (pattype);
	  pattype = non_reference (pattype);

	  if (TREE_CODE (pattype) == TEMPLATE_TYPE_PARM
	      && (cand->explicit_targs == NULL_TREE
		  || (TREE_VEC_LENGTH (cand->explicit_targs)
		      <= TEMPLATE_TYPE_IDX (pattype))))
	    {
	      pedwarn (input_location, 0, "deducing %qT as %qT",
		       non_reference (TREE_TYPE (patparm)),
		       non_reference (type));
	      pedwarn (input_location, 0, "  in call to %q+D", cand->fn);
	      pedwarn (input_location, 0,
		       "  (you can disable this with -fno-deduce-init-list)");
	    }
	}
      val = convert_like_with_context (conv, arg, fn, i - is_method,
				       conversion_warning
				       ? complain
				       : complain & (~tf_warning));

      val = convert_for_arg_passing (type, val, complain);
	
      if (val == error_mark_node)
        return error_mark_node;
      else
        argarray[j++] = val;
    }

  /* Default arguments */
  for (; parm && parm != void_list_node; parm = TREE_CHAIN (parm), i++)
    {
      if (TREE_VALUE (parm) == error_mark_node)
	return error_mark_node;
      argarray[j++] = convert_default_arg (TREE_VALUE (parm),
					   TREE_PURPOSE (parm),
					   fn, i - is_method,
					   complain);
    }

  /* Ellipsis */
  for (; arg_index < vec_safe_length (args); ++arg_index)
    {
      tree a = (*args)[arg_index];
      if (magic_varargs_p (fn))
	/* Do no conversions for magic varargs.  */
	a = mark_type_use (a);
      else
	a = convert_arg_to_ellipsis (a, complain);
      argarray[j++] = a;
    }

  gcc_assert (j <= nargs);
  nargs = j;

  check_function_arguments (TREE_TYPE (fn), nargs, argarray);

  /* Avoid actually calling copy constructors and copy assignment operators,
     if possible.  */

  if (! flag_elide_constructors)
    /* Do things the hard way.  */;
  else if (cand->num_convs == 1 
           && (DECL_COPY_CONSTRUCTOR_P (fn) 
               || DECL_MOVE_CONSTRUCTOR_P (fn)))
    {
      tree targ;
      tree arg = argarray[num_artificial_parms_for (fn)];
      tree fa;
      bool trivial = trivial_fn_p (fn);

      /* Pull out the real argument, disregarding const-correctness.  */
      targ = arg;
      while (CONVERT_EXPR_P (targ)
	     || TREE_CODE (targ) == NON_LVALUE_EXPR)
	targ = TREE_OPERAND (targ, 0);
      if (TREE_CODE (targ) == ADDR_EXPR)
	{
	  targ = TREE_OPERAND (targ, 0);
	  if (!same_type_ignoring_top_level_qualifiers_p
	      (TREE_TYPE (TREE_TYPE (arg)), TREE_TYPE (targ)))
	    targ = NULL_TREE;
	}
      else
	targ = NULL_TREE;

      if (targ)
	arg = targ;
      else
	arg = cp_build_indirect_ref (arg, RO_NULL, complain);

      /* [class.copy]: the copy constructor is implicitly defined even if
	 the implementation elided its use.  */
      if (!trivial || DECL_DELETED_FN (fn))
	{
	  mark_used (fn);
	  already_used = true;
	}

      /* If we're creating a temp and we already have one, don't create a
	 new one.  If we're not creating a temp but we get one, use
	 INIT_EXPR to collapse the temp into our target.  Otherwise, if the
	 ctor is trivial, do a bitwise copy with a simple TARGET_EXPR for a
	 temp or an INIT_EXPR otherwise.  */
      fa = argarray[0];
      if (is_dummy_object (fa))
	{
	  if (TREE_CODE (arg) == TARGET_EXPR)
	    return arg;
	  else if (trivial)
	    return force_target_expr (DECL_CONTEXT (fn), arg, complain);
	}
      else if (TREE_CODE (arg) == TARGET_EXPR || trivial)
	{
	  tree to = stabilize_reference (cp_build_indirect_ref (fa, RO_NULL,
								complain));

	  val = build2 (INIT_EXPR, DECL_CONTEXT (fn), to, arg);
	  return val;
	}
    }
  else if (DECL_OVERLOADED_OPERATOR_P (fn) == NOP_EXPR
	   && trivial_fn_p (fn)
	   && !DECL_DELETED_FN (fn))
    {
      tree to = stabilize_reference
	(cp_build_indirect_ref (argarray[0], RO_NULL, complain));
      tree type = TREE_TYPE (to);
      tree as_base = CLASSTYPE_AS_BASE (type);
      tree arg = argarray[1];

      if (is_really_empty_class (type))
	{
	  /* Avoid copying empty classes.  */
	  val = build2 (COMPOUND_EXPR, void_type_node, to, arg);
	  TREE_NO_WARNING (val) = 1;
	  val = build2 (COMPOUND_EXPR, type, val, to);
	  TREE_NO_WARNING (val) = 1;
	}
      else if (tree_int_cst_equal (TYPE_SIZE (type), TYPE_SIZE (as_base)))
	{
	  arg = cp_build_indirect_ref (arg, RO_NULL, complain);
	  val = build2 (MODIFY_EXPR, TREE_TYPE (to), to, arg);
	}
      else
	{
	  /* We must only copy the non-tail padding parts.  */
	  tree arg0, arg2, t;
	  tree array_type, alias_set;

	  arg2 = TYPE_SIZE_UNIT (as_base);
	  arg0 = cp_build_addr_expr (to, complain);

	  array_type = build_array_type (char_type_node,
					 build_index_type
					   (size_binop (MINUS_EXPR,
							arg2, size_int (1))));
	  alias_set = build_int_cst (build_pointer_type (type), 0);
	  t = build2 (MODIFY_EXPR, void_type_node,
		      build2 (MEM_REF, array_type, arg0, alias_set),
		      build2 (MEM_REF, array_type, arg, alias_set));
	  val = build2 (COMPOUND_EXPR, TREE_TYPE (to), t, to);
          TREE_NO_WARNING (val) = 1;
	}

      return val;
    }
  else if (DECL_DESTRUCTOR_P (fn)
	   && trivial_fn_p (fn)
	   && !DECL_DELETED_FN (fn))
    return fold_convert (void_type_node, argarray[0]);
  /* FIXME handle trivial default constructor, too.  */

  /* For calls to a multi-versioned function, overload resolution
     returns the function with the highest target priority, that is,
     the version that will checked for dispatching first.  If this
     version is inlinable, a direct call to this version can be made
     otherwise the call should go through the dispatcher.  */

  if (DECL_FUNCTION_VERSIONED (fn)
      && (current_function_decl == NULL
	  || !targetm.target_option.can_inline_p (current_function_decl, fn)))
    {
      fn = get_function_version_dispatcher (fn);
      if (fn == NULL)
	return NULL;
      if (!already_used)
	mark_versions_used (fn);
    }

  if (!already_used
      && !mark_used (fn))
    return error_mark_node;

  if (DECL_VINDEX (fn) && (flags & LOOKUP_NONVIRTUAL) == 0
      /* Don't mess with virtual lookup in fold_non_dependent_expr; virtual
	 functions can't be constexpr.  */
      && !in_template_function ())
    {
      tree t;
      tree binfo = lookup_base (TREE_TYPE (TREE_TYPE (argarray[0])),
				DECL_CONTEXT (fn),
				ba_any, NULL, complain);
      gcc_assert (binfo && binfo != error_mark_node);

      /* Warn about deprecated virtual functions now, since we're about
	 to throw away the decl.  */
      if (TREE_DEPRECATED (fn))
	warn_deprecated_use (fn, NULL_TREE);

      argarray[0] = build_base_path (PLUS_EXPR, argarray[0], binfo, 1,
				     complain);
      if (TREE_SIDE_EFFECTS (argarray[0]))
	argarray[0] = save_expr (argarray[0]);
      t = build_pointer_type (TREE_TYPE (fn));
      if (DECL_CONTEXT (fn) && TYPE_JAVA_INTERFACE (DECL_CONTEXT (fn)))
	fn = build_java_interface_fn_ref (fn, argarray[0]);
      else
	fn = build_vfn_ref (argarray[0], DECL_VINDEX (fn));
      TREE_TYPE (fn) = t;
    }
  else
    {
      fn = build_addr_func (fn, complain);
      if (fn == error_mark_node)
	return error_mark_node;
    }

  tree call = build_cxx_call (fn, nargs, argarray, complain|decltype_flag);
  if (TREE_CODE (call) == CALL_EXPR
      && (cand->flags & LOOKUP_LIST_INIT_CTOR))
    CALL_EXPR_LIST_INIT_P (call) = true;
  return call;
}

/* Build and return a call to FN, using NARGS arguments in ARGARRAY.
   This function performs no overload resolution, conversion, or other
   high-level operations.  */

tree
build_cxx_call (tree fn, int nargs, tree *argarray,
		tsubst_flags_t complain)
{
  tree fndecl;
  int optimize_sav;

  /* Remember roughly where this call is.  */
  location_t loc = EXPR_LOC_OR_LOC (fn, input_location);
  fn = build_call_a (fn, nargs, argarray);
  SET_EXPR_LOCATION (fn, loc);

  fndecl = get_callee_fndecl (fn);

  /* Check that arguments to builtin functions match the expectations.  */
  if (fndecl
      && DECL_BUILT_IN (fndecl)
      && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
      && !check_builtin_function_arguments (fndecl, nargs, argarray))
    return error_mark_node;

    /* If it is a built-in array notation function, then the return type of
     the function is the element type of the array passed in as array 
     notation (i.e. the first parameter of the function).  */
  if (flag_cilkplus && TREE_CODE (fn) == CALL_EXPR) 
    {
      enum built_in_function bif = 
	is_cilkplus_reduce_builtin (CALL_EXPR_FN (fn));
      if (bif == BUILT_IN_CILKPLUS_SEC_REDUCE_ADD
	  || bif == BUILT_IN_CILKPLUS_SEC_REDUCE_MUL
	  || bif == BUILT_IN_CILKPLUS_SEC_REDUCE_MAX
	  || bif == BUILT_IN_CILKPLUS_SEC_REDUCE_MIN
	  || bif == BUILT_IN_CILKPLUS_SEC_REDUCE
	  || bif == BUILT_IN_CILKPLUS_SEC_REDUCE_MUTATING)
	{ 
	  /* for bif == BUILT_IN_CILKPLUS_SEC_REDUCE_ALL_ZERO or
	     BUILT_IN_CILKPLUS_SEC_REDUCE_ANY_ZERO or
	     BUILT_IN_CILKPLUS_SEC_REDUCE_ANY_NONZERO or 
	     BUILT_IN_CILKPLUS_SEC_REDUCE_ALL_NONZERO or
	     BUILT_IN_CILKPLUS_SEC_REDUCE_MIN_IND or
             BUILT_IN_CILKPLUS_SEC_REDUCE_MAX_IND
	     The pre-defined return-type is the correct one.  */
	  tree array_ntn = CALL_EXPR_ARG (fn, 0); 
	  TREE_TYPE (fn) = TREE_TYPE (array_ntn); 
	  return fn;
	}
    }

  /* Some built-in function calls will be evaluated at compile-time in
     fold ().  Set optimize to 1 when folding __builtin_constant_p inside
     a constexpr function so that fold_builtin_1 doesn't fold it to 0.  */
  optimize_sav = optimize;
  if (!optimize && fndecl && DECL_IS_BUILTIN_CONSTANT_P (fndecl)
      && current_function_decl
      && DECL_DECLARED_CONSTEXPR_P (current_function_decl))
    optimize = 1;
  fn = fold_if_not_in_template (fn);
  optimize = optimize_sav;

  if (VOID_TYPE_P (TREE_TYPE (fn)))
    return fn;

  /* 5.2.2/11: If a function call is a prvalue of object type: if the
     function call is either the operand of a decltype-specifier or the
     right operand of a comma operator that is the operand of a
     decltype-specifier, a temporary object is not introduced for the
     prvalue. The type of the prvalue may be incomplete.  */
  if (!(complain & tf_decltype))
    {
      fn = require_complete_type_sfinae (fn, complain);
      if (fn == error_mark_node)
	return error_mark_node;

      if (MAYBE_CLASS_TYPE_P (TREE_TYPE (fn)))
	fn = build_cplus_new (TREE_TYPE (fn), fn, complain);
    }
  return convert_from_reference (fn);
}

static GTY(()) tree java_iface_lookup_fn;

/* Make an expression which yields the address of the Java interface
   method FN.  This is achieved by generating a call to libjava's
   _Jv_LookupInterfaceMethodIdx().  */

static tree
build_java_interface_fn_ref (tree fn, tree instance)
{
  tree lookup_fn, method, idx;
  tree klass_ref, iface, iface_ref;
  int i;

  if (!java_iface_lookup_fn)
    {
      tree ftype = build_function_type_list (ptr_type_node,
					     ptr_type_node, ptr_type_node,
					     java_int_type_node, NULL_TREE);
      java_iface_lookup_fn
	= add_builtin_function ("_Jv_LookupInterfaceMethodIdx", ftype,
				0, NOT_BUILT_IN, NULL, NULL_TREE);
    }

  /* Look up the pointer to the runtime java.lang.Class object for `instance'.
     This is the first entry in the vtable.  */
  klass_ref = build_vtbl_ref (cp_build_indirect_ref (instance, RO_NULL, 
                                                     tf_warning_or_error),
			      integer_zero_node);

  /* Get the java.lang.Class pointer for the interface being called.  */
  iface = DECL_CONTEXT (fn);
  iface_ref = lookup_field (iface, get_identifier ("class$"), 0, false);
  if (!iface_ref || !VAR_P (iface_ref)
      || DECL_CONTEXT (iface_ref) != iface)
    {
      error ("could not find class$ field in java interface type %qT",
		iface);
      return error_mark_node;
    }
  iface_ref = build_address (iface_ref);
  iface_ref = convert (build_pointer_type (iface), iface_ref);

  /* Determine the itable index of FN.  */
  i = 1;
  for (method = TYPE_METHODS (iface); method; method = DECL_CHAIN (method))
    {
      if (!DECL_VIRTUAL_P (method))
	continue;
      if (fn == method)
	break;
      i++;
    }
  idx = build_int_cst (NULL_TREE, i);

  lookup_fn = build1 (ADDR_EXPR,
		      build_pointer_type (TREE_TYPE (java_iface_lookup_fn)),
		      java_iface_lookup_fn);
  return build_call_nary (ptr_type_node, lookup_fn,
			  3, klass_ref, iface_ref, idx);
}

/* Returns the value to use for the in-charge parameter when making a
   call to a function with the indicated NAME.

   FIXME:Can't we find a neater way to do this mapping?  */

tree
in_charge_arg_for_name (tree name)
{
 if (name == base_ctor_identifier
      || name == base_dtor_identifier)
    return integer_zero_node;
  else if (name == complete_ctor_identifier)
    return integer_one_node;
  else if (name == complete_dtor_identifier)
    return integer_two_node;
  else if (name == deleting_dtor_identifier)
    return integer_three_node;

  /* This function should only be called with one of the names listed
     above.  */
  gcc_unreachable ();
  return NULL_TREE;
}

/* Build a call to a constructor, destructor, or an assignment
   operator for INSTANCE, an expression with class type.  NAME
   indicates the special member function to call; *ARGS are the
   arguments.  ARGS may be NULL.  This may change ARGS.  BINFO
   indicates the base of INSTANCE that is to be passed as the `this'
   parameter to the member function called.

   FLAGS are the LOOKUP_* flags to use when processing the call.

   If NAME indicates a complete object constructor, INSTANCE may be
   NULL_TREE.  In this case, the caller will call build_cplus_new to
   store the newly constructed object into a VAR_DECL.  */

tree
build_special_member_call (tree instance, tree name, vec<tree, va_gc> **args,
			   tree binfo, int flags, tsubst_flags_t complain)
{
  tree fns;
  /* The type of the subobject to be constructed or destroyed.  */
  tree class_type;
  vec<tree, va_gc> *allocated = NULL;
  tree ret;

  gcc_assert (name == complete_ctor_identifier
	      || name == base_ctor_identifier
	      || name == complete_dtor_identifier
	      || name == base_dtor_identifier
	      || name == deleting_dtor_identifier
	      || name == ansi_assopname (NOP_EXPR));
  if (TYPE_P (binfo))
    {
      /* Resolve the name.  */
      if (!complete_type_or_maybe_complain (binfo, NULL_TREE, complain))
	return error_mark_node;

      binfo = TYPE_BINFO (binfo);
    }

  gcc_assert (binfo != NULL_TREE);

  class_type = BINFO_TYPE (binfo);

  /* Handle the special case where INSTANCE is NULL_TREE.  */
  if (name == complete_ctor_identifier && !instance)
    instance = build_dummy_object (class_type);
  else
    {
      if (name == complete_dtor_identifier
	  || name == base_dtor_identifier
	  || name == deleting_dtor_identifier)
	gcc_assert (args == NULL || vec_safe_is_empty (*args));

      /* Convert to the base class, if necessary.  */
      if (!same_type_ignoring_top_level_qualifiers_p
	  (TREE_TYPE (instance), BINFO_TYPE (binfo)))
	{
	  if (name != ansi_assopname (NOP_EXPR))
	    /* For constructors and destructors, either the base is
	       non-virtual, or it is virtual but we are doing the
	       conversion from a constructor or destructor for the
	       complete object.  In either case, we can convert
	       statically.  */
	    instance = convert_to_base_statically (instance, binfo);
	  else
	    /* However, for assignment operators, we must convert
	       dynamically if the base is virtual.  */
	    instance = build_base_path (PLUS_EXPR, instance,
					binfo, /*nonnull=*/1, complain);
	}
    }

  gcc_assert (instance != NULL_TREE);

  fns = lookup_fnfields (binfo, name, 1);

  /* When making a call to a constructor or destructor for a subobject
     that uses virtual base classes, pass down a pointer to a VTT for
     the subobject.  */
  if ((name == base_ctor_identifier
       || name == base_dtor_identifier)
      && CLASSTYPE_VBASECLASSES (class_type))
    {
      tree vtt;
      tree sub_vtt;

      /* If the current function is a complete object constructor
	 or destructor, then we fetch the VTT directly.
	 Otherwise, we look it up using the VTT we were given.  */
      vtt = DECL_CHAIN (CLASSTYPE_VTABLES (current_class_type));
      vtt = decay_conversion (vtt, complain);
      if (vtt == error_mark_node)
	return error_mark_node;
      vtt = build3 (COND_EXPR, TREE_TYPE (vtt),
		    build2 (EQ_EXPR, boolean_type_node,
			    current_in_charge_parm, integer_zero_node),
		    current_vtt_parm,
		    vtt);
      if (BINFO_SUBVTT_INDEX (binfo))
	sub_vtt = fold_build_pointer_plus (vtt, BINFO_SUBVTT_INDEX (binfo));
      else
	sub_vtt = vtt;

      if (args == NULL)
	{
	  allocated = make_tree_vector ();
	  args = &allocated;
	}

      vec_safe_insert (*args, 0, sub_vtt);
    }

  ret = build_new_method_call (instance, fns, args,
			       TYPE_BINFO (BINFO_TYPE (binfo)),
			       flags, /*fn=*/NULL,
			       complain);

  if (allocated != NULL)
    release_tree_vector (allocated);

  if ((complain & tf_error)
      && (flags & LOOKUP_DELEGATING_CONS)
      && name == complete_ctor_identifier 
      && TREE_CODE (ret) == CALL_EXPR
      && (DECL_ABSTRACT_ORIGIN (TREE_OPERAND (CALL_EXPR_FN (ret), 0))
	  == current_function_decl))
    error ("constructor delegates to itself");

  return ret;
}

/* Return the NAME, as a C string.  The NAME indicates a function that
   is a member of TYPE.  *FREE_P is set to true if the caller must
   free the memory returned.

   Rather than go through all of this, we should simply set the names
   of constructors and destructors appropriately, and dispense with
   ctor_identifier, dtor_identifier, etc.  */

static char *
name_as_c_string (tree name, tree type, bool *free_p)
{
  char *pretty_name;

  /* Assume that we will not allocate memory.  */
  *free_p = false;
  /* Constructors and destructors are special.  */
  if (IDENTIFIER_CTOR_OR_DTOR_P (name))
    {
      pretty_name
	= CONST_CAST (char *, identifier_to_locale (IDENTIFIER_POINTER (constructor_name (type))));
      /* For a destructor, add the '~'.  */
      if (name == complete_dtor_identifier
	  || name == base_dtor_identifier
	  || name == deleting_dtor_identifier)
	{
	  pretty_name = concat ("~", pretty_name, NULL);
	  /* Remember that we need to free the memory allocated.  */
	  *free_p = true;
	}
    }
  else if (IDENTIFIER_TYPENAME_P (name))
    {
      pretty_name = concat ("operator ",
			    type_as_string_translate (TREE_TYPE (name),
						      TFF_PLAIN_IDENTIFIER),
			    NULL);
      /* Remember that we need to free the memory allocated.  */
      *free_p = true;
    }
  else
    pretty_name = CONST_CAST (char *, identifier_to_locale (IDENTIFIER_POINTER (name)));

  return pretty_name;
}

/* Build a call to "INSTANCE.FN (ARGS)".  If FN_P is non-NULL, it will
   be set, upon return, to the function called.  ARGS may be NULL.
   This may change ARGS.  */

static tree
build_new_method_call_1 (tree instance, tree fns, vec<tree, va_gc> **args,
		         tree conversion_path, int flags,
		         tree *fn_p, tsubst_flags_t complain)
{
  struct z_candidate *candidates = 0, *cand;
  tree explicit_targs = NULL_TREE;
  tree basetype = NULL_TREE;
  tree access_binfo, binfo;
  tree optype;
  tree first_mem_arg = NULL_TREE;
  tree name;
  bool skip_first_for_error;
  vec<tree, va_gc> *user_args;
  tree call;
  tree fn;
  int template_only = 0;
  bool any_viable_p;
  tree orig_instance;
  tree orig_fns;
  vec<tree, va_gc> *orig_args = NULL;
  void *p;

  gcc_assert (instance != NULL_TREE);

  /* We don't know what function we're going to call, yet.  */
  if (fn_p)
    *fn_p = NULL_TREE;

  if (error_operand_p (instance)
      || !fns || error_operand_p (fns))
    return error_mark_node;

  if (!BASELINK_P (fns))
    {
      if (complain & tf_error)
	error ("call to non-function %qD", fns);
      return error_mark_node;
    }

  orig_instance = instance;
  orig_fns = fns;

  /* Dismantle the baselink to collect all the information we need.  */
  if (!conversion_path)
    conversion_path = BASELINK_BINFO (fns);
  access_binfo = BASELINK_ACCESS_BINFO (fns);
  binfo = BASELINK_BINFO (fns);
  optype = BASELINK_OPTYPE (fns);
  fns = BASELINK_FUNCTIONS (fns);
  if (TREE_CODE (fns) == TEMPLATE_ID_EXPR)
    {
      explicit_targs = TREE_OPERAND (fns, 1);
      fns = TREE_OPERAND (fns, 0);
      template_only = 1;
    }
  gcc_assert (TREE_CODE (fns) == FUNCTION_DECL
	      || TREE_CODE (fns) == TEMPLATE_DECL
	      || TREE_CODE (fns) == OVERLOAD);
  fn = get_first_fn (fns);
  name = DECL_NAME (fn);

  basetype = TYPE_MAIN_VARIANT (TREE_TYPE (instance));
  gcc_assert (CLASS_TYPE_P (basetype));

  if (processing_template_decl)
    {
      orig_args = args == NULL ? NULL : make_tree_vector_copy (*args);
      instance = build_non_dependent_expr (instance);
      if (args != NULL)
	make_args_non_dependent (*args);
    }

  user_args = args == NULL ? NULL : *args;
  /* Under DR 147 A::A() is an invalid constructor call,
     not a functional cast.  */
  if (DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P (fn))
    {
      if (! (complain & tf_error))
	return error_mark_node;

      if (permerror (input_location,
		     "cannot call constructor %<%T::%D%> directly",
		     basetype, name))
	inform (input_location, "for a function-style cast, remove the "
		"redundant %<::%D%>", name);
      call = build_functional_cast (basetype, build_tree_list_vec (user_args),
				    complain);
      return call;
    }

  /* Figure out whether to skip the first argument for the error
     message we will display to users if an error occurs.  We don't
     want to display any compiler-generated arguments.  The "this"
     pointer hasn't been added yet.  However, we must remove the VTT
     pointer if this is a call to a base-class constructor or
     destructor.  */
  skip_first_for_error = false;
  if (IDENTIFIER_CTOR_OR_DTOR_P (name))
    {
      /* Callers should explicitly indicate whether they want to construct
	 the complete object or just the part without virtual bases.  */
      gcc_assert (name != ctor_identifier);
      /* Similarly for destructors.  */
      gcc_assert (name != dtor_identifier);
      /* Remove the VTT pointer, if present.  */
      if ((name == base_ctor_identifier || name == base_dtor_identifier)
	  && CLASSTYPE_VBASECLASSES (basetype))
	skip_first_for_error = true;
    }

  /* Process the argument list.  */
  if (args != NULL && *args != NULL)
    {
      *args = resolve_args (*args, complain);
      if (*args == NULL)
	return error_mark_node;
    }

  /* Consider the object argument to be used even if we end up selecting a
     static member function.  */
  instance = mark_type_use (instance);

  /* It's OK to call destructors and constructors on cv-qualified objects.
     Therefore, convert the INSTANCE to the unqualified type, if
     necessary.  */
  if (DECL_DESTRUCTOR_P (fn)
      || DECL_CONSTRUCTOR_P (fn))
    {
      if (!same_type_p (basetype, TREE_TYPE (instance)))
	{
	  instance = build_this (instance);
	  instance = build_nop (build_pointer_type (basetype), instance);
	  instance = build_fold_indirect_ref (instance);
	}
    }
  if (DECL_DESTRUCTOR_P (fn))
    name = complete_dtor_identifier;

  /* For the overload resolution we need to find the actual `this`
     that would be captured if the call turns out to be to a
     non-static member function.  Do not actually capture it at this
     point.  */
  first_mem_arg = maybe_resolve_dummy (instance, false);

  /* Get the high-water mark for the CONVERSION_OBSTACK.  */
  p = conversion_obstack_alloc (0);

  /* If CONSTRUCTOR_IS_DIRECT_INIT is set, this was a T{ } form
     initializer, not T({ }).  */
  if (DECL_CONSTRUCTOR_P (fn) && args != NULL && !vec_safe_is_empty (*args)
      && DIRECT_LIST_INIT_P ((**args)[0]))
    {
      tree init_list = (**args)[0];
      tree init = NULL_TREE;

      gcc_assert ((*args)->length () == 1
		  && !(flags & LOOKUP_ONLYCONVERTING));

      /* If the initializer list has no elements and T is a class type with
	 a default constructor, the object is value-initialized.  Handle
	 this here so we don't need to handle it wherever we use
	 build_special_member_call.  */
      if (CONSTRUCTOR_NELTS (init_list) == 0
	  && TYPE_HAS_DEFAULT_CONSTRUCTOR (basetype)
	  /* For a user-provided default constructor, use the normal
	     mechanisms so that protected access works.  */
	  && !type_has_user_provided_default_constructor (basetype)
	  && !processing_template_decl)
	init = build_value_init (basetype, complain);

      /* If BASETYPE is an aggregate, we need to do aggregate
	 initialization.  */
      else if (CP_AGGREGATE_TYPE_P (basetype))
	init = digest_init (basetype, init_list, complain);

      if (init)
	{
	  if (is_dummy_object (instance))
	    return get_target_expr_sfinae (init, complain);
	  init = build2 (INIT_EXPR, TREE_TYPE (instance), instance, init);
	  TREE_SIDE_EFFECTS (init) = true;
	  return init;
	}

      /* Otherwise go ahead with overload resolution.  */
      add_list_candidates (fns, first_mem_arg, init_list,
			   basetype, explicit_targs, template_only,
			   conversion_path, access_binfo, flags,
			   &candidates, complain);
    }
  else
    {
      add_candidates (fns, first_mem_arg, user_args, optype,
		      explicit_targs, template_only, conversion_path,
		      access_binfo, flags, &candidates, complain);
    }
  any_viable_p = false;
  candidates = splice_viable (candidates, false, &any_viable_p);

  if (!any_viable_p)
    {
      if (complain & tf_error)
	{
	  if (!COMPLETE_OR_OPEN_TYPE_P (basetype))
	    cxx_incomplete_type_error (instance, basetype);
	  else if (optype)
	    error ("no matching function for call to %<%T::operator %T(%A)%#V%>",
		   basetype, optype, build_tree_list_vec (user_args),
		   TREE_TYPE (instance));
	  else
	    {
	      char *pretty_name;
	      bool free_p;
	      tree arglist;

	      pretty_name = name_as_c_string (name, basetype, &free_p);
	      arglist = build_tree_list_vec (user_args);
	      if (skip_first_for_error)
		arglist = TREE_CHAIN (arglist);
	      error ("no matching function for call to %<%T::%s(%A)%#V%>",
		     basetype, pretty_name, arglist,
		     TREE_TYPE (instance));
	      if (free_p)
		free (pretty_name);
	    }
	  print_z_candidates (location_of (name), candidates);
	}
      call = error_mark_node;
    }
  else
    {
      cand = tourney (candidates, complain);
      if (cand == 0)
	{
	  char *pretty_name;
	  bool free_p;
	  tree arglist;

	  if (complain & tf_error)
	    {
	      pretty_name = name_as_c_string (name, basetype, &free_p);
	      arglist = build_tree_list_vec (user_args);
	      if (skip_first_for_error)
		arglist = TREE_CHAIN (arglist);
	      if (!any_strictly_viable (candidates))
		error ("no matching function for call to %<%s(%A)%>",
		       pretty_name, arglist);
	      else
		error ("call of overloaded %<%s(%A)%> is ambiguous",
		       pretty_name, arglist);
	      print_z_candidates (location_of (name), candidates);
	      if (free_p)
		free (pretty_name);
	    }
	  call = error_mark_node;
	}
      else
	{
	  fn = cand->fn;
	  call = NULL_TREE;

	  if (!(flags & LOOKUP_NONVIRTUAL)
	      && DECL_PURE_VIRTUAL_P (fn)
	      && instance == current_class_ref
	      && (complain & tf_warning))
	    {
	      /* This is not an error, it is runtime undefined
		 behavior.  */
	      if (!current_function_decl)
		warning (0, "pure virtual %q#D called from "
			 "non-static data member initializer", fn);
	      else if (DECL_CONSTRUCTOR_P (current_function_decl)
		       || DECL_DESTRUCTOR_P (current_function_decl))
		warning (0, (DECL_CONSTRUCTOR_P (current_function_decl)
			     ? "pure virtual %q#D called from constructor"
			     : "pure virtual %q#D called from destructor"),
			 fn);
	    }

	  if (TREE_CODE (TREE_TYPE (fn)) == METHOD_TYPE
	      && !DECL_CONSTRUCTOR_P (fn)
	      && is_dummy_object (instance))
	    {
	      instance = maybe_resolve_dummy (instance, true);
	      if (instance == error_mark_node)
		call = error_mark_node;
	      else if (!is_dummy_object (instance))
		{
		  /* We captured 'this' in the current lambda now that
		     we know we really need it.  */
		  cand->first_arg = instance;
		}
	      else
		{
		  if (complain & tf_error)
		    error ("cannot call member function %qD without object",
			   fn);
		  call = error_mark_node;
		}
	    }

	  if (call != error_mark_node)
	    {
	      /* Optimize away vtable lookup if we know that this
		 function can't be overridden.  We need to check if
		 the context and the type where we found fn are the same,
		 actually FN might be defined in a different class
		 type because of a using-declaration. In this case, we
		 do not want to perform a non-virtual call.  */
	      if (DECL_VINDEX (fn) && ! (flags & LOOKUP_NONVIRTUAL)
		  && same_type_ignoring_top_level_qualifiers_p
		  (DECL_CONTEXT (fn), BINFO_TYPE (binfo))
		  && resolves_to_fixed_type_p (instance, 0))
		flags |= LOOKUP_NONVIRTUAL;
              if (explicit_targs)
                flags |= LOOKUP_EXPLICIT_TMPL_ARGS;
	      /* Now we know what function is being called.  */
	      if (fn_p)
		*fn_p = fn;
	      /* Build the actual CALL_EXPR.  */
	      call = build_over_call (cand, flags, complain);
	      /* In an expression of the form `a->f()' where `f' turns
		 out to be a static member function, `a' is
		 none-the-less evaluated.  */
	      if (TREE_CODE (TREE_TYPE (fn)) != METHOD_TYPE
		  && !is_dummy_object (instance)
		  && TREE_SIDE_EFFECTS (instance))
		call = build2 (COMPOUND_EXPR, TREE_TYPE (call),
			       instance, call);
	      else if (call != error_mark_node
		       && DECL_DESTRUCTOR_P (cand->fn)
		       && !VOID_TYPE_P (TREE_TYPE (call)))
		/* An explicit call of the form "x->~X()" has type
		   "void".  However, on platforms where destructors
		   return "this" (i.e., those where
		   targetm.cxx.cdtor_returns_this is true), such calls
		   will appear to have a return value of pointer type
		   to the low-level call machinery.  We do not want to
		   change the low-level machinery, since we want to be
		   able to optimize "delete f()" on such platforms as
		   "operator delete(~X(f()))" (rather than generating
		   "t = f(), ~X(t), operator delete (t)").  */
		call = build_nop (void_type_node, call);
	    }
	}
    }

  if (processing_template_decl && call != error_mark_node)
    {
      bool cast_to_void = false;

      if (TREE_CODE (call) == COMPOUND_EXPR)
	call = TREE_OPERAND (call, 1);
      else if (TREE_CODE (call) == NOP_EXPR)
	{
	  cast_to_void = true;
	  call = TREE_OPERAND (call, 0);
	}
      if (INDIRECT_REF_P (call))
	call = TREE_OPERAND (call, 0);
      call = (build_min_non_dep_call_vec
	      (call,
	       build_min (COMPONENT_REF, TREE_TYPE (CALL_EXPR_FN (call)),
			  orig_instance, orig_fns, NULL_TREE),
	       orig_args));
      SET_EXPR_LOCATION (call, input_location);
      call = convert_from_reference (call);
      if (cast_to_void)
	call = build_nop (void_type_node, call);
    }

 /* Free all the conversions we allocated.  */
  obstack_free (&conversion_obstack, p);

  if (orig_args != NULL)
    release_tree_vector (orig_args);

  return call;
}

/* Wrapper for above.  */

tree
build_new_method_call (tree instance, tree fns, vec<tree, va_gc> **args,
		       tree conversion_path, int flags,
		       tree *fn_p, tsubst_flags_t complain)
{
  tree ret;
  bool subtime = timevar_cond_start (TV_OVERLOAD);
  ret = build_new_method_call_1 (instance, fns, args, conversion_path, flags,
                                 fn_p, complain);
  timevar_cond_stop (TV_OVERLOAD, subtime);
  return ret;
}

/* Returns true iff standard conversion sequence ICS1 is a proper
   subsequence of ICS2.  */

static bool
is_subseq (conversion *ics1, conversion *ics2)
{
  /* We can assume that a conversion of the same code
     between the same types indicates a subsequence since we only get
     here if the types we are converting from are the same.  */

  while (ics1->kind == ck_rvalue
	 || ics1->kind == ck_lvalue)
    ics1 = next_conversion (ics1);

  while (1)
    {
      while (ics2->kind == ck_rvalue
	     || ics2->kind == ck_lvalue)
	ics2 = next_conversion (ics2);

      if (ics2->kind == ck_user
	  || ics2->kind == ck_ambig
	  || ics2->kind == ck_aggr
	  || ics2->kind == ck_list
	  || ics2->kind == ck_identity)
	/* At this point, ICS1 cannot be a proper subsequence of
	   ICS2.  We can get a USER_CONV when we are comparing the
	   second standard conversion sequence of two user conversion
	   sequences.  */
	return false;

      ics2 = next_conversion (ics2);

      if (ics2->kind == ics1->kind
	  && same_type_p (ics2->type, ics1->type)
	  && same_type_p (next_conversion (ics2)->type,
			  next_conversion (ics1)->type))
	return true;
    }
}

/* Returns nonzero iff DERIVED is derived from BASE.  The inputs may
   be any _TYPE nodes.  */

bool
is_properly_derived_from (tree derived, tree base)
{
  if (!CLASS_TYPE_P (derived) || !CLASS_TYPE_P (base))
    return false;

  /* We only allow proper derivation here.  The DERIVED_FROM_P macro
     considers every class derived from itself.  */
  return (!same_type_ignoring_top_level_qualifiers_p (derived, base)
	  && DERIVED_FROM_P (base, derived));
}

/* We build the ICS for an implicit object parameter as a pointer
   conversion sequence.  However, such a sequence should be compared
   as if it were a reference conversion sequence.  If ICS is the
   implicit conversion sequence for an implicit object parameter,
   modify it accordingly.  */

static void
maybe_handle_implicit_object (conversion **ics)
{
  if ((*ics)->this_p)
    {
      /* [over.match.funcs]

	 For non-static member functions, the type of the
	 implicit object parameter is "reference to cv X"
	 where X is the class of which the function is a
	 member and cv is the cv-qualification on the member
	 function declaration.  */
      conversion *t = *ics;
      tree reference_type;

      /* The `this' parameter is a pointer to a class type.  Make the
	 implicit conversion talk about a reference to that same class
	 type.  */
      reference_type = TREE_TYPE (t->type);
      reference_type = build_reference_type (reference_type);

      if (t->kind == ck_qual)
	t = next_conversion (t);
      if (t->kind == ck_ptr)
	t = next_conversion (t);
      t = build_identity_conv (TREE_TYPE (t->type), NULL_TREE);
      t = direct_reference_binding (reference_type, t);
      t->this_p = 1;
      t->rvaluedness_matches_p = 0;
      *ics = t;
    }
}

/* If *ICS is a REF_BIND set *ICS to the remainder of the conversion,
   and return the initial reference binding conversion. Otherwise,
   leave *ICS unchanged and return NULL.  */

static conversion *
maybe_handle_ref_bind (conversion **ics)
{
  if ((*ics)->kind == ck_ref_bind)
    {
      conversion *old_ics = *ics;
      *ics = next_conversion (old_ics);
      (*ics)->user_conv_p = old_ics->user_conv_p;
      return old_ics;
    }

  return NULL;
}

/* Compare two implicit conversion sequences according to the rules set out in
   [over.ics.rank].  Return values:

      1: ics1 is better than ics2
     -1: ics2 is better than ics1
      0: ics1 and ics2 are indistinguishable */

static int
compare_ics (conversion *ics1, conversion *ics2)
{
  tree from_type1;
  tree from_type2;
  tree to_type1;
  tree to_type2;
  tree deref_from_type1 = NULL_TREE;
  tree deref_from_type2 = NULL_TREE;
  tree deref_to_type1 = NULL_TREE;
  tree deref_to_type2 = NULL_TREE;
  conversion_rank rank1, rank2;

  /* REF_BINDING is nonzero if the result of the conversion sequence
     is a reference type.   In that case REF_CONV is the reference
     binding conversion. */
  conversion *ref_conv1;
  conversion *ref_conv2;

  /* Compare badness before stripping the reference conversion.  */
  if (ics1->bad_p > ics2->bad_p)
    return -1;
  else if (ics1->bad_p < ics2->bad_p)
    return 1;

  /* Handle implicit object parameters.  */
  maybe_handle_implicit_object (&ics1);
  maybe_handle_implicit_object (&ics2);

  /* Handle reference parameters.  */
  ref_conv1 = maybe_handle_ref_bind (&ics1);
  ref_conv2 = maybe_handle_ref_bind (&ics2);

  /* List-initialization sequence L1 is a better conversion sequence than
     list-initialization sequence L2 if L1 converts to
     std::initializer_list<X> for some X and L2 does not.  */
  if (ics1->kind == ck_list && ics2->kind != ck_list)
    return 1;
  if (ics2->kind == ck_list && ics1->kind != ck_list)
    return -1;

  /* [over.ics.rank]

     When  comparing  the  basic forms of implicit conversion sequences (as
     defined in _over.best.ics_)

     --a standard conversion sequence (_over.ics.scs_) is a better
       conversion sequence than a user-defined conversion sequence
       or an ellipsis conversion sequence, and

     --a user-defined conversion sequence (_over.ics.user_) is a
       better conversion sequence than an ellipsis conversion sequence
       (_over.ics.ellipsis_).  */
  /* Use BAD_CONVERSION_RANK because we already checked for a badness
     mismatch.  If both ICS are bad, we try to make a decision based on
     what would have happened if they'd been good.  This is not an
     extension, we'll still give an error when we build up the call; this
     just helps us give a more helpful error message.  */
  rank1 = BAD_CONVERSION_RANK (ics1);
  rank2 = BAD_CONVERSION_RANK (ics2);

  if (rank1 > rank2)
    return -1;
  else if (rank1 < rank2)
    return 1;

  if (ics1->ellipsis_p)
    /* Both conversions are ellipsis conversions.  */
    return 0;

  /* User-defined  conversion sequence U1 is a better conversion sequence
     than another user-defined conversion sequence U2 if they contain the
     same user-defined conversion operator or constructor and if the sec-
     ond standard conversion sequence of U1 is  better  than  the  second
     standard conversion sequence of U2.  */

  /* Handle list-conversion with the same code even though it isn't always
     ranked as a user-defined conversion and it doesn't have a second
     standard conversion sequence; it will still have the desired effect.
     Specifically, we need to do the reference binding comparison at the
     end of this function.  */

  if (ics1->user_conv_p || ics1->kind == ck_list || ics1->kind == ck_aggr)
    {
      conversion *t1;
      conversion *t2;

      for (t1 = ics1; t1->kind != ck_user; t1 = next_conversion (t1))
	if (t1->kind == ck_ambig || t1->kind == ck_aggr
	    || t1->kind == ck_list)
	  break;
      for (t2 = ics2; t2->kind != ck_user; t2 = next_conversion (t2))
	if (t2->kind == ck_ambig || t2->kind == ck_aggr
	    || t2->kind == ck_list)
	  break;

      if (t1->kind != t2->kind)
	return 0;
      else if (t1->kind == ck_user)
	{
	  if (t1->cand->fn != t2->cand->fn)
	    return 0;
	}
      else
	{
	  /* For ambiguous or aggregate conversions, use the target type as
	     a proxy for the conversion function.  */
	  if (!same_type_ignoring_top_level_qualifiers_p (t1->type, t2->type))
	    return 0;
	}

      /* We can just fall through here, after setting up
	 FROM_TYPE1 and FROM_TYPE2.  */
      from_type1 = t1->type;
      from_type2 = t2->type;
    }
  else
    {
      conversion *t1;
      conversion *t2;

      /* We're dealing with two standard conversion sequences.

	 [over.ics.rank]

	 Standard conversion sequence S1 is a better conversion
	 sequence than standard conversion sequence S2 if

	 --S1 is a proper subsequence of S2 (comparing the conversion
	   sequences in the canonical form defined by _over.ics.scs_,
	   excluding any Lvalue Transformation; the identity
	   conversion sequence is considered to be a subsequence of
	   any non-identity conversion sequence */

      t1 = ics1;
      while (t1->kind != ck_identity)
	t1 = next_conversion (t1);
      from_type1 = t1->type;

      t2 = ics2;
      while (t2->kind != ck_identity)
	t2 = next_conversion (t2);
      from_type2 = t2->type;
    }

  /* One sequence can only be a subsequence of the other if they start with
     the same type.  They can start with different types when comparing the
     second standard conversion sequence in two user-defined conversion
     sequences.  */
  if (same_type_p (from_type1, from_type2))
    {
      if (is_subseq (ics1, ics2))
	return 1;
      if (is_subseq (ics2, ics1))
	return -1;
    }

  /* [over.ics.rank]

     Or, if not that,

     --the rank of S1 is better than the rank of S2 (by the rules
       defined below):

    Standard conversion sequences are ordered by their ranks: an Exact
    Match is a better conversion than a Promotion, which is a better
    conversion than a Conversion.

    Two conversion sequences with the same rank are indistinguishable
    unless one of the following rules applies:

    --A conversion that does not a convert a pointer, pointer to member,
      or std::nullptr_t to bool is better than one that does.

    The ICS_STD_RANK automatically handles the pointer-to-bool rule,
    so that we do not have to check it explicitly.  */
  if (ics1->rank < ics2->rank)
    return 1;
  else if (ics2->rank < ics1->rank)
    return -1;

  to_type1 = ics1->type;
  to_type2 = ics2->type;

  /* A conversion from scalar arithmetic type to complex is worse than a
     conversion between scalar arithmetic types.  */
  if (same_type_p (from_type1, from_type2)
      && ARITHMETIC_TYPE_P (from_type1)
      && ARITHMETIC_TYPE_P (to_type1)
      && ARITHMETIC_TYPE_P (to_type2)
      && ((TREE_CODE (to_type1) == COMPLEX_TYPE)
	  != (TREE_CODE (to_type2) == COMPLEX_TYPE)))
    {
      if (TREE_CODE (to_type1) == COMPLEX_TYPE)
	return -1;
      else
	return 1;
    }

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
  else if ((TYPE_PTRDATAMEM_P (from_type1) && TYPE_PTRDATAMEM_P (from_type2)
	    && TYPE_PTRDATAMEM_P (to_type1) && TYPE_PTRDATAMEM_P (to_type2))
	   || (TYPE_PTRMEMFUNC_P (from_type1)
	       && TYPE_PTRMEMFUNC_P (from_type2)
	       && TYPE_PTRMEMFUNC_P (to_type1)
	       && TYPE_PTRMEMFUNC_P (to_type2)))
    {
      deref_to_type1 = TYPE_PTRMEM_CLASS_TYPE (from_type1);
      deref_to_type2 = TYPE_PTRMEM_CLASS_TYPE (from_type2);
      deref_from_type1 = TYPE_PTRMEM_CLASS_TYPE (to_type1);
      deref_from_type2 = TYPE_PTRMEM_CLASS_TYPE (to_type2);
    }

  if (deref_from_type1 != NULL_TREE
      && RECORD_OR_UNION_CODE_P (TREE_CODE (deref_from_type1))
      && RECORD_OR_UNION_CODE_P (TREE_CODE (deref_from_type2)))
    {
      /* This was one of the pointer or pointer-like conversions.

	 [over.ics.rank]

	 --If class B is derived directly or indirectly from class A,
	   conversion of B* to A* is better than conversion of B* to
	   void*, and conversion of A* to void* is better than
	   conversion of B* to void*.  */
      if (VOID_TYPE_P (deref_to_type1)
	  && VOID_TYPE_P (deref_to_type2))
	{
	  if (is_properly_derived_from (deref_from_type1,
					deref_from_type2))
	    return -1;
	  else if (is_properly_derived_from (deref_from_type2,
					     deref_from_type1))
	    return 1;
	}
      else if (VOID_TYPE_P (deref_to_type1)
	       || VOID_TYPE_P (deref_to_type2))
	{
	  if (same_type_p (deref_from_type1, deref_from_type2))
	    {
	      if (VOID_TYPE_P (deref_to_type2))
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
      else if (RECORD_OR_UNION_CODE_P (TREE_CODE (deref_to_type1))
	       && RECORD_OR_UNION_CODE_P (TREE_CODE (deref_to_type2)))
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
  else if (CLASS_TYPE_P (non_reference (from_type1))
	   && same_type_p (from_type1, from_type2))
    {
      tree from = non_reference (from_type1);

      /* [over.ics.rank]

	 --binding of an expression of type C to a reference of type
	   B& is better than binding an expression of type C to a
	   reference of type A&

	 --conversion of C to B is better than conversion of C to A,  */
      if (is_properly_derived_from (from, to_type1)
	  && is_properly_derived_from (from, to_type2))
	{
	  if (is_properly_derived_from (to_type1, to_type2))
	    return 1;
	  else if (is_properly_derived_from (to_type2, to_type1))
	    return -1;
	}
    }
  else if (CLASS_TYPE_P (non_reference (to_type1))
	   && same_type_p (to_type1, to_type2))
    {
      tree to = non_reference (to_type1);

      /* [over.ics.rank]

	 --binding of an expression of type B to a reference of type
	   A& is better than binding an expression of type C to a
	   reference of type A&,

	 --conversion of B to A is better than conversion of C to A  */
      if (is_properly_derived_from (from_type1, to)
	  && is_properly_derived_from (from_type2, to))
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
  if (ics1->kind == ck_qual
      && ics2->kind == ck_qual
      && same_type_p (from_type1, from_type2))
    {
      int result = comp_cv_qual_signature (to_type1, to_type2);
      if (result != 0)
	return result;
    }

  /* [over.ics.rank]

     --S1 and S2 are reference bindings (_dcl.init.ref_) and neither refers
     to an implicit object parameter, and either S1 binds an lvalue reference
     to an lvalue and S2 binds an rvalue reference or S1 binds an rvalue
     reference to an rvalue and S2 binds an lvalue reference
     (C++0x draft standard, 13.3.3.2)

     --S1 and S2 are reference bindings (_dcl.init.ref_), and the
     types to which the references refer are the same type except for
     top-level cv-qualifiers, and the type to which the reference
     initialized by S2 refers is more cv-qualified than the type to
     which the reference initialized by S1 refers.

     DR 1328 [over.match.best]: the context is an initialization by
     conversion function for direct reference binding (13.3.1.6) of a
     reference to function type, the return type of F1 is the same kind of
     reference (i.e. lvalue or rvalue) as the reference being initialized,
     and the return type of F2 is not.  */

  if (ref_conv1 && ref_conv2)
    {
      if (!ref_conv1->this_p && !ref_conv2->this_p
	  && (ref_conv1->rvaluedness_matches_p
	      != ref_conv2->rvaluedness_matches_p)
	  && (same_type_p (ref_conv1->type, ref_conv2->type)
	      || (TYPE_REF_IS_RVALUE (ref_conv1->type)
		  != TYPE_REF_IS_RVALUE (ref_conv2->type))))
	{
	  if (ref_conv1->bad_p
	      && !same_type_p (TREE_TYPE (ref_conv1->type),
			       TREE_TYPE (ref_conv2->type)))
	    /* Don't prefer a bad conversion that drops cv-quals to a bad
	       conversion with the wrong rvalueness.  */
	    return 0;
	  return (ref_conv1->rvaluedness_matches_p
		  - ref_conv2->rvaluedness_matches_p);
	}

      if (same_type_ignoring_top_level_qualifiers_p (to_type1, to_type2))
	{
	  int q1 = cp_type_quals (TREE_TYPE (ref_conv1->type));
	  int q2 = cp_type_quals (TREE_TYPE (ref_conv2->type));
	  if (ref_conv1->bad_p)
	    {
	      /* Prefer the one that drops fewer cv-quals.  */
	      tree ftype = next_conversion (ref_conv1)->type;
	      int fquals = cp_type_quals (ftype);
	      q1 ^= fquals;
	      q2 ^= fquals;
	    }
	  return comp_cv_qualification (q2, q1);
	}
    }

  /* Neither conversion sequence is better than the other.  */
  return 0;
}

/* The source type for this standard conversion sequence.  */

static tree
source_type (conversion *t)
{
  for (;; t = next_conversion (t))
    {
      if (t->kind == ck_user
	  || t->kind == ck_ambig
	  || t->kind == ck_identity)
	return t->type;
    }
  gcc_unreachable ();
}

/* Note a warning about preferring WINNER to LOSER.  We do this by storing
   a pointer to LOSER and re-running joust to produce the warning if WINNER
   is actually used.  */

static void
add_warning (struct z_candidate *winner, struct z_candidate *loser)
{
  candidate_warning *cw = (candidate_warning *)
    conversion_obstack_alloc (sizeof (candidate_warning));
  cw->loser = loser;
  cw->next = winner->warnings;
  winner->warnings = cw;
}

/* Compare two candidates for overloading as described in
   [over.match.best].  Return values:

      1: cand1 is better than cand2
     -1: cand2 is better than cand1
      0: cand1 and cand2 are indistinguishable */

static int
joust (struct z_candidate *cand1, struct z_candidate *cand2, bool warn,
       tsubst_flags_t complain)
{
  int winner = 0;
  int off1 = 0, off2 = 0;
  size_t i;
  size_t len;

  /* Candidates that involve bad conversions are always worse than those
     that don't.  */
  if (cand1->viable > cand2->viable)
    return 1;
  if (cand1->viable < cand2->viable)
    return -1;

  /* If we have two pseudo-candidates for conversions to the same type,
     or two candidates for the same function, arbitrarily pick one.  */
  if (cand1->fn == cand2->fn
      && (IS_TYPE_OR_DECL_P (cand1->fn)))
    return 1;

  /* Prefer a non-deleted function over an implicitly deleted move
     constructor or assignment operator.  This differs slightly from the
     wording for issue 1402 (which says the move op is ignored by overload
     resolution), but this way produces better error messages.  */
  if (TREE_CODE (cand1->fn) == FUNCTION_DECL
      && TREE_CODE (cand2->fn) == FUNCTION_DECL
      && DECL_DELETED_FN (cand1->fn) != DECL_DELETED_FN (cand2->fn))
    {
      if (DECL_DELETED_FN (cand1->fn) && DECL_DEFAULTED_FN (cand1->fn)
	  && move_fn_p (cand1->fn))
	return -1;
      if (DECL_DELETED_FN (cand2->fn) && DECL_DEFAULTED_FN (cand2->fn)
	  && move_fn_p (cand2->fn))
	return 1;
    }

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
  len = cand1->num_convs;
  if (len != cand2->num_convs)
    {
      int static_1 = DECL_STATIC_FUNCTION_P (cand1->fn);
      int static_2 = DECL_STATIC_FUNCTION_P (cand2->fn);

      if (DECL_CONSTRUCTOR_P (cand1->fn)
	  && is_list_ctor (cand1->fn) != is_list_ctor (cand2->fn))
	/* We're comparing a near-match list constructor and a near-match
	   non-list constructor.  Just treat them as unordered.  */
	return 0;

      gcc_assert (static_1 != static_2);

      if (static_1)
	off2 = 1;
      else
	{
	  off1 = 1;
	  --len;
	}
    }

  for (i = 0; i < len; ++i)
    {
      conversion *t1 = cand1->convs[i + off1];
      conversion *t2 = cand2->convs[i + off2];
      int comp = compare_ics (t1, t2);

      if (comp != 0)
	{
	  if ((complain & tf_warning)
	      && warn_sign_promo
	      && (CONVERSION_RANK (t1) + CONVERSION_RANK (t2)
		  == cr_std + cr_promotion)
	      && t1->kind == ck_std
	      && t2->kind == ck_std
	      && TREE_CODE (t1->type) == INTEGER_TYPE
	      && TREE_CODE (t2->type) == INTEGER_TYPE
	      && (TYPE_PRECISION (t1->type)
		  == TYPE_PRECISION (t2->type))
	      && (TYPE_UNSIGNED (next_conversion (t1)->type)
		  || (TREE_CODE (next_conversion (t1)->type)
		      == ENUMERAL_TYPE)))
	    {
	      tree type = next_conversion (t1)->type;
	      tree type1, type2;
	      struct z_candidate *w, *l;
	      if (comp > 0)
		type1 = t1->type, type2 = t2->type,
		  w = cand1, l = cand2;
	      else
		type1 = t2->type, type2 = t1->type,
		  w = cand2, l = cand1;

	      if (warn)
		{
		  warning (OPT_Wsign_promo, "passing %qT chooses %qT over %qT",
			   type, type1, type2);
		  warning (OPT_Wsign_promo, "  in call to %qD", w->fn);
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
  if ((complain & tf_warning)
      && winner && warn_conversion && cand1->second_conv
      && (!DECL_CONSTRUCTOR_P (cand1->fn) || !DECL_CONSTRUCTOR_P (cand2->fn))
      && winner != compare_ics (cand1->second_conv, cand2->second_conv))
    {
      struct z_candidate *w, *l;
      bool give_warning = false;

      if (winner == 1)
	w = cand1, l = cand2;
      else
	w = cand2, l = cand1;

      /* We don't want to complain about `X::operator T1 ()'
	 beating `X::operator T2 () const', when T2 is a no less
	 cv-qualified version of T1.  */
      if (DECL_CONTEXT (w->fn) == DECL_CONTEXT (l->fn)
	  && !DECL_CONSTRUCTOR_P (w->fn) && !DECL_CONSTRUCTOR_P (l->fn))
	{
	  tree t = TREE_TYPE (TREE_TYPE (l->fn));
	  tree f = TREE_TYPE (TREE_TYPE (w->fn));

	  if (TREE_CODE (t) == TREE_CODE (f) && POINTER_TYPE_P (t))
	    {
	      t = TREE_TYPE (t);
	      f = TREE_TYPE (f);
	    }
	  if (!comp_ptr_ttypes (t, f))
	    give_warning = true;
	}
      else
	give_warning = true;

      if (!give_warning)
	/*NOP*/;
      else if (warn)
	{
	  tree source = source_type (w->convs[0]);
	  if (! DECL_CONSTRUCTOR_P (w->fn))
	    source = TREE_TYPE (source);
	  if (warning (OPT_Wconversion, "choosing %qD over %qD", w->fn, l->fn)
	      && warning (OPT_Wconversion, "  for conversion from %qT to %qT",
			  source, w->second_conv->type)) 
	    {
	      inform (input_location, "  because conversion sequence for the argument is better");
	    }
	}
      else
	add_warning (w, l);
    }

  if (winner)
    return winner;

  /* DR 495 moved this tiebreaker above the template ones.  */
  /* or, if not that,
     the  context  is  an  initialization by user-defined conversion (see
     _dcl.init_  and  _over.match.user_)  and  the  standard   conversion
     sequence  from  the return type of F1 to the destination type (i.e.,
     the type of the entity being initialized)  is  a  better  conversion
     sequence  than the standard conversion sequence from the return type
     of F2 to the destination type.  */

  if (cand1->second_conv)
    {
      winner = compare_ics (cand1->second_conv, cand2->second_conv);
      if (winner)
	return winner;
    }

  /* or, if not that,
     F1 is a non-template function and F2 is a template function
     specialization.  */

  if (!cand1->template_decl && cand2->template_decl)
    return 1;
  else if (cand1->template_decl && !cand2->template_decl)
    return -1;

  /* or, if not that,
     F1 and F2 are template functions and the function template for F1 is
     more specialized than the template for F2 according to the partial
     ordering rules.  */

  if (cand1->template_decl && cand2->template_decl)
    {
      winner = more_specialized_fn
	(TI_TEMPLATE (cand1->template_decl),
	 TI_TEMPLATE (cand2->template_decl),
	 /* [temp.func.order]: The presence of unused ellipsis and default
	    arguments has no effect on the partial ordering of function
	    templates.   add_function_candidate() will not have
	    counted the "this" argument for constructors.  */
	 cand1->num_convs + DECL_CONSTRUCTOR_P (cand1->fn));
      if (winner)
	return winner;
    }

  /* Check whether we can discard a builtin candidate, either because we
     have two identical ones or matching builtin and non-builtin candidates.

     (Pedantically in the latter case the builtin which matched the user
     function should not be added to the overload set, but we spot it here.

     [over.match.oper]
     ... the builtin candidates include ...
     - do not have the same parameter type list as any non-template
       non-member candidate.  */

  if (identifier_p (cand1->fn) || identifier_p (cand2->fn))
    {
      for (i = 0; i < len; ++i)
	if (!same_type_p (cand1->convs[i]->type,
			  cand2->convs[i]->type))
	  break;
      if (i == cand1->num_convs)
	{
	  if (cand1->fn == cand2->fn)
	    /* Two built-in candidates; arbitrarily pick one.  */
	    return 1;
	  else if (identifier_p (cand1->fn))
	    /* cand1 is built-in; prefer cand2.  */
	    return -1;
	  else
	    /* cand2 is built-in; prefer cand1.  */
	    return 1;
	}
    }

  /* For candidates of a multi-versioned function,  make the version with
     the highest priority win.  This version will be checked for dispatching
     first.  If this version can be inlined into the caller, the front-end
     will simply make a direct call to this function.  */

  if (TREE_CODE (cand1->fn) == FUNCTION_DECL
      && DECL_FUNCTION_VERSIONED (cand1->fn)
      && TREE_CODE (cand2->fn) == FUNCTION_DECL
      && DECL_FUNCTION_VERSIONED (cand2->fn))
    {
      tree f1 = TREE_TYPE (cand1->fn);
      tree f2 = TREE_TYPE (cand2->fn);
      tree p1 = TYPE_ARG_TYPES (f1);
      tree p2 = TYPE_ARG_TYPES (f2);
     
      /* Check if cand1->fn and cand2->fn are versions of the same function.  It
         is possible that cand1->fn and cand2->fn are function versions but of
         different functions.  Check types to see if they are versions of the same
         function.  */
      if (compparms (p1, p2)
	  && same_type_p (TREE_TYPE (f1), TREE_TYPE (f2)))
	{
	  /* Always make the version with the higher priority, more
	     specialized, win.  */
	  gcc_assert (targetm.compare_version_priority);
	  if (targetm.compare_version_priority (cand1->fn, cand2->fn) >= 0)
	    return 1;
	  else
	    return -1;
	}
    }

  /* If the two function declarations represent the same function (this can
     happen with declarations in multiple scopes and arg-dependent lookup),
     arbitrarily choose one.  But first make sure the default args we're
     using match.  */
  if (DECL_P (cand1->fn) && DECL_P (cand2->fn)
      && equal_functions (cand1->fn, cand2->fn))
    {
      tree parms1 = TYPE_ARG_TYPES (TREE_TYPE (cand1->fn));
      tree parms2 = TYPE_ARG_TYPES (TREE_TYPE (cand2->fn));

      gcc_assert (!DECL_CONSTRUCTOR_P (cand1->fn));

      for (i = 0; i < len; ++i)
	{
	  /* Don't crash if the fn is variadic.  */
	  if (!parms1)
	    break;
	  parms1 = TREE_CHAIN (parms1);
	  parms2 = TREE_CHAIN (parms2);
	}

      if (off1)
	parms1 = TREE_CHAIN (parms1);
      else if (off2)
	parms2 = TREE_CHAIN (parms2);

      for (; parms1; ++i)
	{
	  if (!cp_tree_equal (TREE_PURPOSE (parms1),
			      TREE_PURPOSE (parms2)))
	    {
	      if (warn)
		{
		  if (complain & tf_error)
		    {
		      if (permerror (input_location,
				     "default argument mismatch in "
				     "overload resolution"))
			{
			  inform (input_location,
				  " candidate 1: %q+#F", cand1->fn);
			  inform (input_location,
				  " candidate 2: %q+#F", cand2->fn);
			}
		    }
		  else
		    return 0;
		}
	      else
		add_warning (cand1, cand2);
	      break;
	    }
	  parms1 = TREE_CHAIN (parms1);
	  parms2 = TREE_CHAIN (parms2);
	}

      return 1;
    }

tweak:

  /* Extension: If the worst conversion for one candidate is worse than the
     worst conversion for the other, take the first.  */
  if (!pedantic && (complain & tf_warning_or_error))
    {
      conversion_rank rank1 = cr_identity, rank2 = cr_identity;
      struct z_candidate *w = 0, *l = 0;

      for (i = 0; i < len; ++i)
	{
	  if (CONVERSION_RANK (cand1->convs[i+off1]) > rank1)
	    rank1 = CONVERSION_RANK (cand1->convs[i+off1]);
	  if (CONVERSION_RANK (cand2->convs[i + off2]) > rank2)
	    rank2 = CONVERSION_RANK (cand2->convs[i + off2]);
	}
      if (rank1 < rank2)
	winner = 1, w = cand1, l = cand2;
      if (rank1 > rank2)
	winner = -1, w = cand2, l = cand1;
      if (winner)
	{
	  /* Don't choose a deleted function over ambiguity.  */
	  if (DECL_P (w->fn) && DECL_DELETED_FN (w->fn))
	    return 0;
	  if (warn)
	    {
	      pedwarn (input_location, 0,
	      "ISO C++ says that these are ambiguous, even "
	      "though the worst conversion for the first is better than "
	      "the worst conversion for the second:");
	      print_z_candidate (input_location, _("candidate 1:"), w);
	      print_z_candidate (input_location, _("candidate 2:"), l);
	    }
	  else
	    add_warning (w, l);
	  return winner;
	}
    }

  gcc_assert (!winner);
  return 0;
}

/* Given a list of candidates for overloading, find the best one, if any.
   This algorithm has a worst case of O(2n) (winner is last), and a best
   case of O(n/2) (totally ambiguous); much better than a sorting
   algorithm.  */

static struct z_candidate *
tourney (struct z_candidate *candidates, tsubst_flags_t complain)
{
  struct z_candidate *champ = candidates, *challenger;
  int fate;
  int champ_compared_to_predecessor = 0;

  /* Walk through the list once, comparing each current champ to the next
     candidate, knocking out a candidate or two with each comparison.  */

  for (challenger = champ->next; challenger; )
    {
      fate = joust (champ, challenger, 0, complain);
      if (fate == 1)
	challenger = challenger->next;
      else
	{
	  if (fate == 0)
	    {
	      champ = challenger->next;
	      if (champ == 0)
		return NULL;
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
      fate = joust (champ, challenger, 0, complain);
      if (fate != 1)
	return NULL;
    }

  return champ;
}

/* Returns nonzero if things of type FROM can be converted to TO.  */

bool
can_convert (tree to, tree from, tsubst_flags_t complain)
{
  tree arg = NULL_TREE;
  /* implicit_conversion only considers user-defined conversions
     if it has an expression for the call argument list.  */
  if (CLASS_TYPE_P (from) || CLASS_TYPE_P (to))
    arg = build1 (CAST_EXPR, from, NULL_TREE);
  return can_convert_arg (to, from, arg, LOOKUP_IMPLICIT, complain);
}

/* Returns nonzero if things of type FROM can be converted to TO with a
   standard conversion.  */

bool
can_convert_standard (tree to, tree from, tsubst_flags_t complain)
{
  return can_convert_arg (to, from, NULL_TREE, LOOKUP_IMPLICIT, complain);
}

/* Returns nonzero if ARG (of type FROM) can be converted to TO.  */

bool
can_convert_arg (tree to, tree from, tree arg, int flags,
		 tsubst_flags_t complain)
{
  conversion *t;
  void *p;
  bool ok_p;

  /* Get the high-water mark for the CONVERSION_OBSTACK.  */
  p = conversion_obstack_alloc (0);
  /* We want to discard any access checks done for this test,
     as we might not be in the appropriate access context and
     we'll do the check again when we actually perform the
     conversion.  */
  push_deferring_access_checks (dk_deferred);

  t  = implicit_conversion (to, from, arg, /*c_cast_p=*/false,
			    flags, complain);
  ok_p = (t && !t->bad_p);

  /* Discard the access checks now.  */
  pop_deferring_access_checks ();
  /* Free all the conversions we allocated.  */
  obstack_free (&conversion_obstack, p);

  return ok_p;
}

/* Like can_convert_arg, but allows dubious conversions as well.  */

bool
can_convert_arg_bad (tree to, tree from, tree arg, int flags,
		     tsubst_flags_t complain)
{
  conversion *t;
  void *p;

  /* Get the high-water mark for the CONVERSION_OBSTACK.  */
  p = conversion_obstack_alloc (0);
  /* Try to perform the conversion.  */
  t  = implicit_conversion (to, from, arg, /*c_cast_p=*/false,
			    flags, complain);
  /* Free all the conversions we allocated.  */
  obstack_free (&conversion_obstack, p);

  return t != NULL;
}

/* Convert EXPR to TYPE.  Return the converted expression.

   Note that we allow bad conversions here because by the time we get to
   this point we are committed to doing the conversion.  If we end up
   doing a bad conversion, convert_like will complain.  */

tree
perform_implicit_conversion_flags (tree type, tree expr,
				   tsubst_flags_t complain, int flags)
{
  conversion *conv;
  void *p;
  location_t loc = EXPR_LOC_OR_LOC (expr, input_location);

  if (error_operand_p (expr))
    return error_mark_node;

  /* Get the high-water mark for the CONVERSION_OBSTACK.  */
  p = conversion_obstack_alloc (0);

  conv = implicit_conversion (type, TREE_TYPE (expr), expr,
			      /*c_cast_p=*/false,
			      flags, complain);

  if (!conv)
    {
      if (complain & tf_error)
	{
	  /* If expr has unknown type, then it is an overloaded function.
	     Call instantiate_type to get good error messages.  */
	  if (TREE_TYPE (expr) == unknown_type_node)
	    instantiate_type (type, expr, complain);
	  else if (invalid_nonstatic_memfn_p (expr, complain))
	    /* We gave an error.  */;
	  else
	    error_at (loc, "could not convert %qE from %qT to %qT", expr,
		      TREE_TYPE (expr), type);
	}
      expr = error_mark_node;
    }
  else if (processing_template_decl && conv->kind != ck_identity)
    {
      /* In a template, we are only concerned about determining the
	 type of non-dependent expressions, so we do not have to
	 perform the actual conversion.  But for initializers, we
	 need to be able to perform it at instantiation
	 (or fold_non_dependent_expr) time.  */
      expr = build1 (IMPLICIT_CONV_EXPR, type, expr);
      if (!(flags & LOOKUP_ONLYCONVERTING))
	IMPLICIT_CONV_EXPR_DIRECT_INIT (expr) = true;
    }
  else
    expr = convert_like (conv, expr, complain);

  /* Free all the conversions we allocated.  */
  obstack_free (&conversion_obstack, p);

  return expr;
}

tree
perform_implicit_conversion (tree type, tree expr, tsubst_flags_t complain)
{
  return perform_implicit_conversion_flags (type, expr, complain,
					    LOOKUP_IMPLICIT);
}

/* Convert EXPR to TYPE (as a direct-initialization) if that is
   permitted.  If the conversion is valid, the converted expression is
   returned.  Otherwise, NULL_TREE is returned, except in the case
   that TYPE is a class type; in that case, an error is issued.  If
   C_CAST_P is true, then this direct-initialization is taking
   place as part of a static_cast being attempted as part of a C-style
   cast.  */

tree
perform_direct_initialization_if_possible (tree type,
					   tree expr,
					   bool c_cast_p,
                                           tsubst_flags_t complain)
{
  conversion *conv;
  void *p;

  if (type == error_mark_node || error_operand_p (expr))
    return error_mark_node;
  /* [dcl.init]

     If the destination type is a (possibly cv-qualified) class type:

     -- If the initialization is direct-initialization ...,
     constructors are considered. ... If no constructor applies, or
     the overload resolution is ambiguous, the initialization is
     ill-formed.  */
  if (CLASS_TYPE_P (type))
    {
      vec<tree, va_gc> *args = make_tree_vector_single (expr);
      expr = build_special_member_call (NULL_TREE, complete_ctor_identifier,
					&args, type, LOOKUP_NORMAL, complain);
      release_tree_vector (args);
      return build_cplus_new (type, expr, complain);
    }

  /* Get the high-water mark for the CONVERSION_OBSTACK.  */
  p = conversion_obstack_alloc (0);

  conv = implicit_conversion (type, TREE_TYPE (expr), expr,
			      c_cast_p,
			      LOOKUP_NORMAL, complain);
  if (!conv || conv->bad_p)
    expr = NULL_TREE;
  else
    expr = convert_like_real (conv, expr, NULL_TREE, 0, 0,
			      /*issue_conversion_warnings=*/false,
			      c_cast_p,
			      complain);

  /* Free all the conversions we allocated.  */
  obstack_free (&conversion_obstack, p);

  return expr;
}

/* When initializing a reference that lasts longer than a full-expression,
   this special rule applies:

     [class.temporary]

     The temporary to which the reference is bound or the temporary
     that is the complete object to which the reference is bound
     persists for the lifetime of the reference.

     The temporaries created during the evaluation of the expression
     initializing the reference, except the temporary to which the
     reference is bound, are destroyed at the end of the
     full-expression in which they are created.

   In that case, we store the converted expression into a new
   VAR_DECL in a new scope.

   However, we want to be careful not to create temporaries when
   they are not required.  For example, given:

     struct B {};
     struct D : public B {};
     D f();
     const B& b = f();

   there is no need to copy the return value from "f"; we can just
   extend its lifetime.  Similarly, given:

     struct S {};
     struct T { operator S(); };
     T t;
     const S& s = t;

  we can extend the lifetime of the return value of the conversion
  operator.

  The next several functions are involved in this lifetime extension.  */

/* DECL is a VAR_DECL or FIELD_DECL whose type is a REFERENCE_TYPE.  The
   reference is being bound to a temporary.  Create and return a new
   VAR_DECL with the indicated TYPE; this variable will store the value to
   which the reference is bound.  */

tree
make_temporary_var_for_ref_to_temp (tree decl, tree type)
{
  tree var;

  /* Create the variable.  */
  var = create_temporary_var (type);

  /* Register the variable.  */
  if (VAR_P (decl)
      && (TREE_STATIC (decl) || DECL_THREAD_LOCAL_P (decl)))
    {
      /* Namespace-scope or local static; give it a mangled name.  */
      /* FIXME share comdat with decl?  */
      tree name;

      TREE_STATIC (var) = TREE_STATIC (decl);
      set_decl_tls_model (var, DECL_TLS_MODEL (decl));
      name = mangle_ref_init_variable (decl);
      DECL_NAME (var) = name;
      SET_DECL_ASSEMBLER_NAME (var, name);
      var = pushdecl_top_level (var);
    }
  else
    /* Create a new cleanup level if necessary.  */
    maybe_push_cleanup_level (type);

  return var;
}

/* EXPR is the initializer for a variable DECL of reference or
   std::initializer_list type.  Create, push and return a new VAR_DECL
   for the initializer so that it will live as long as DECL.  Any
   cleanup for the new variable is returned through CLEANUP, and the
   code to initialize the new variable is returned through INITP.  */

static tree
set_up_extended_ref_temp (tree decl, tree expr, vec<tree, va_gc> **cleanups,
			  tree *initp)
{
  tree init;
  tree type;
  tree var;

  /* Create the temporary variable.  */
  type = TREE_TYPE (expr);
  var = make_temporary_var_for_ref_to_temp (decl, type);
  layout_decl (var, 0);
  /* If the rvalue is the result of a function call it will be
     a TARGET_EXPR.  If it is some other construct (such as a
     member access expression where the underlying object is
     itself the result of a function call), turn it into a
     TARGET_EXPR here.  It is important that EXPR be a
     TARGET_EXPR below since otherwise the INIT_EXPR will
     attempt to make a bitwise copy of EXPR to initialize
     VAR.  */
  if (TREE_CODE (expr) != TARGET_EXPR)
    expr = get_target_expr (expr);

  if (TREE_CODE (decl) == FIELD_DECL
      && extra_warnings && !TREE_NO_WARNING (decl))
    {
      warning (OPT_Wextra, "a temporary bound to %qD only persists "
	       "until the constructor exits", decl);
      TREE_NO_WARNING (decl) = true;
    }

  /* Recursively extend temps in this initializer.  */
  TARGET_EXPR_INITIAL (expr)
    = extend_ref_init_temps (decl, TARGET_EXPR_INITIAL (expr), cleanups);

  /* Any reference temp has a non-trivial initializer.  */
  DECL_NONTRIVIALLY_INITIALIZED_P (var) = true;

  /* If the initializer is constant, put it in DECL_INITIAL so we get
     static initialization and use in constant expressions.  */
  init = maybe_constant_init (expr);
  if (TREE_CONSTANT (init))
    {
      if (literal_type_p (type) && CP_TYPE_CONST_NON_VOLATILE_P (type))
	{
	  /* 5.19 says that a constant expression can include an
	     lvalue-rvalue conversion applied to "a glvalue of literal type
	     that refers to a non-volatile temporary object initialized
	     with a constant expression".  Rather than try to communicate
	     that this VAR_DECL is a temporary, just mark it constexpr.

	     Currently this is only useful for initializer_list temporaries,
	     since reference vars can't appear in constant expressions.  */
	  DECL_DECLARED_CONSTEXPR_P (var) = true;
	  DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (var) = true;
	  TREE_CONSTANT (var) = true;
	}
      DECL_INITIAL (var) = init;
      init = NULL_TREE;
    }
  else
    /* Create the INIT_EXPR that will initialize the temporary
       variable.  */
    init = build2 (INIT_EXPR, type, var, expr);
  if (at_function_scope_p ())
    {
      add_decl_expr (var);

      if (TREE_STATIC (var))
	init = add_stmt_to_compound (init, register_dtor_fn (var));
      else
	{
	  tree cleanup = cxx_maybe_build_cleanup (var, tf_warning_or_error);
	  if (cleanup)
	    vec_safe_push (*cleanups, cleanup);
	}

      /* We must be careful to destroy the temporary only
	 after its initialization has taken place.  If the
	 initialization throws an exception, then the
	 destructor should not be run.  We cannot simply
	 transform INIT into something like:

	 (INIT, ({ CLEANUP_STMT; }))

	 because emit_local_var always treats the
	 initializer as a full-expression.  Thus, the
	 destructor would run too early; it would run at the
	 end of initializing the reference variable, rather
	 than at the end of the block enclosing the
	 reference variable.

	 The solution is to pass back a cleanup expression
	 which the caller is responsible for attaching to
	 the statement tree.  */
    }
  else
    {
      rest_of_decl_compilation (var, /*toplev=*/1, at_eof);
      if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (type))
	{
	  if (DECL_THREAD_LOCAL_P (var))
	    tls_aggregates = tree_cons (NULL_TREE, var,
					tls_aggregates);
	  else
	    static_aggregates = tree_cons (NULL_TREE, var,
					   static_aggregates);
	}
      else
	/* Check whether the dtor is callable.  */
	cxx_maybe_build_cleanup (var, tf_warning_or_error);
    }

  *initp = init;
  return var;
}

/* Convert EXPR to the indicated reference TYPE, in a way suitable for
   initializing a variable of that TYPE.  */

tree
initialize_reference (tree type, tree expr,
		      int flags, tsubst_flags_t complain)
{
  conversion *conv;
  void *p;
  location_t loc = EXPR_LOC_OR_LOC (expr, input_location);

  if (type == error_mark_node || error_operand_p (expr))
    return error_mark_node;

  /* Get the high-water mark for the CONVERSION_OBSTACK.  */
  p = conversion_obstack_alloc (0);

  conv = reference_binding (type, TREE_TYPE (expr), expr, /*c_cast_p=*/false,
			    flags, complain);
  if (!conv || conv->bad_p)
    {
      if (complain & tf_error)
	{
	  if (conv)
	    convert_like (conv, expr, complain);
	  else if (!CP_TYPE_CONST_P (TREE_TYPE (type))
		   && !TYPE_REF_IS_RVALUE (type)
		   && !real_lvalue_p (expr))
	    error_at (loc, "invalid initialization of non-const reference of "
		      "type %qT from an rvalue of type %qT",
		      type, TREE_TYPE (expr));
	  else
	    error_at (loc, "invalid initialization of reference of type "
		      "%qT from expression of type %qT", type,
		      TREE_TYPE (expr));
	}
      return error_mark_node;
    }

  if (conv->kind == ck_ref_bind)
    /* Perform the conversion.  */
    expr = convert_like (conv, expr, complain);
  else if (conv->kind == ck_ambig)
    /* We gave an error in build_user_type_conversion_1.  */
    expr = error_mark_node;
  else
    gcc_unreachable ();

  /* Free all the conversions we allocated.  */
  obstack_free (&conversion_obstack, p);

  return expr;
}

/* Subroutine of extend_ref_init_temps.  Possibly extend one initializer,
   which is bound either to a reference or a std::initializer_list.  */

static tree
extend_ref_init_temps_1 (tree decl, tree init, vec<tree, va_gc> **cleanups)
{
  tree sub = init;
  tree *p;
  STRIP_NOPS (sub);
  if (TREE_CODE (sub) == COMPOUND_EXPR)
    {
      TREE_OPERAND (sub, 1)
        = extend_ref_init_temps_1 (decl, TREE_OPERAND (sub, 1), cleanups);
      return init;
    }
  if (TREE_CODE (sub) != ADDR_EXPR)
    return init;
  /* Deal with binding to a subobject.  */
  for (p = &TREE_OPERAND (sub, 0); TREE_CODE (*p) == COMPONENT_REF; )
    p = &TREE_OPERAND (*p, 0);
  if (TREE_CODE (*p) == TARGET_EXPR)
    {
      tree subinit = NULL_TREE;
      *p = set_up_extended_ref_temp (decl, *p, cleanups, &subinit);
      if (subinit)
	init = build2 (COMPOUND_EXPR, TREE_TYPE (init), subinit, init);
      recompute_tree_invariant_for_addr_expr (sub);
    }
  return init;
}

/* INIT is part of the initializer for DECL.  If there are any
   reference or initializer lists being initialized, extend their
   lifetime to match that of DECL.  */

tree
extend_ref_init_temps (tree decl, tree init, vec<tree, va_gc> **cleanups)
{
  tree type = TREE_TYPE (init);
  if (processing_template_decl)
    return init;
  if (TREE_CODE (type) == REFERENCE_TYPE)
    init = extend_ref_init_temps_1 (decl, init, cleanups);
  else if (is_std_init_list (type))
    {
      /* The temporary array underlying a std::initializer_list
	 is handled like a reference temporary.  */
      tree ctor = init;
      if (TREE_CODE (ctor) == TARGET_EXPR)
	ctor = TARGET_EXPR_INITIAL (ctor);
      if (TREE_CODE (ctor) == CONSTRUCTOR)
	{
	  tree array = CONSTRUCTOR_ELT (ctor, 0)->value;
	  array = extend_ref_init_temps_1 (decl, array, cleanups);
	  CONSTRUCTOR_ELT (ctor, 0)->value = array;
	}
    }
  else if (TREE_CODE (init) == CONSTRUCTOR)
    {
      unsigned i;
      constructor_elt *p;
      vec<constructor_elt, va_gc> *elts = CONSTRUCTOR_ELTS (init);
      FOR_EACH_VEC_SAFE_ELT (elts, i, p)
	p->value = extend_ref_init_temps (decl, p->value, cleanups);
    }

  return init;
}

/* Returns true iff an initializer for TYPE could contain temporaries that
   need to be extended because they are bound to references or
   std::initializer_list.  */

bool
type_has_extended_temps (tree type)
{
  type = strip_array_types (type);
  if (TREE_CODE (type) == REFERENCE_TYPE)
    return true;
  if (CLASS_TYPE_P (type))
    {
      if (is_std_init_list (type))
	return true;
      for (tree f = next_initializable_field (TYPE_FIELDS (type));
	   f; f = next_initializable_field (DECL_CHAIN (f)))
	if (type_has_extended_temps (TREE_TYPE (f)))
	  return true;
    }
  return false;
}

/* Returns true iff TYPE is some variant of std::initializer_list.  */

bool
is_std_init_list (tree type)
{
  /* Look through typedefs.  */
  if (!TYPE_P (type))
    return false;
  if (cxx_dialect == cxx98)
    return false;
  type = TYPE_MAIN_VARIANT (type);
  return (CLASS_TYPE_P (type)
	  && CP_TYPE_CONTEXT (type) == std_node
	  && CLASSTYPE_TEMPLATE_INFO (type)
	  && strcmp (TYPE_NAME_STRING (type), "initializer_list") == 0);
}

/* Returns true iff DECL is a list constructor: i.e. a constructor which
   will accept an argument list of a single std::initializer_list<T>.  */

bool
is_list_ctor (tree decl)
{
  tree args = FUNCTION_FIRST_USER_PARMTYPE (decl);
  tree arg;

  if (!args || args == void_list_node)
    return false;

  arg = non_reference (TREE_VALUE (args));
  if (!is_std_init_list (arg))
    return false;

  args = TREE_CHAIN (args);

  if (args && args != void_list_node && !TREE_PURPOSE (args))
    /* There are more non-defaulted parms.  */
    return false;

  return true;
}

#include "gt-cp-call.h"
