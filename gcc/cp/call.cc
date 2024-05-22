/* Functions related to invoking -*- C++ -*- methods and overloaded functions.
   Copyright (C) 1987-2024 Free Software Foundation, Inc.
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
#include "target.h"
#include "cp-tree.h"
#include "timevar.h"
#include "stringpool.h"
#include "cgraph.h"
#include "stor-layout.h"
#include "trans-mem.h"
#include "flags.h"
#include "toplev.h"
#include "intl.h"
#include "convert.h"
#include "langhooks.h"
#include "c-family/c-objc.h"
#include "internal-fn.h"
#include "stringpool.h"
#include "attribs.h"
#include "decl.h"
#include "c-family/c-type-mismatch.h"
#include "tristate.h"

/* The various kinds of conversion.  */

enum conversion_kind {
  ck_identity,
  ck_lvalue,
  ck_fnptr,
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
  ck_rvalue,
  /* When LOOKUP_SHORTCUT_BAD_CONVS is set, we may return a conversion of
     this kind whenever we know the true conversion is either bad or outright
     invalid, but we don't want to attempt to compute the bad conversion (for
     sake of avoiding unnecessary instantiation).  bad_p should always be set
     for these.  */
  ck_deferred_bad,
};

/* The rank of the conversion.  Order of the enumerals matters; better
   conversions should come earlier in the list.  */

enum conversion_rank {
  cr_identity,
  cr_exact,
  cr_promotion,
  cr_std,
  cr_pbool,
  cr_user,
  cr_ellipsis,
  cr_bad
};

/* An implicit conversion sequence, in the sense of [over.best.ics].
   The first conversion to be performed is at the end of the chain.
   That conversion is always a cr_identity conversion.  */

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
  /* If KIND is ck_ref_bind or ck_base, true to indicate that a
     temporary should be created to hold the result of the
     conversion.  If KIND is ck_ambig or ck_user, true means force
     copy-initialization.  */
  BOOL_BITFIELD need_temporary_p : 1;
  /* If KIND is ck_ptr or ck_pmem, true to indicate that a conversion
     from a pointer-to-derived to pointer-to-base is being performed.  */
  BOOL_BITFIELD base_p : 1;
  /* If KIND is ck_ref_bind, true when either an lvalue reference is
     being bound to an lvalue expression or an rvalue reference is
     being bound to an rvalue expression.  If KIND is ck_rvalue or ck_base,
     true when we are treating an lvalue as an rvalue (12.8p33).  If
     ck_identity, we will be binding a reference directly or decaying to
     a pointer.  */
  BOOL_BITFIELD rvaluedness_matches_p: 1;
  BOOL_BITFIELD check_narrowing: 1;
  /* Whether check_narrowing should only check TREE_CONSTANTs; used
     in build_converted_constant_expr.  */
  BOOL_BITFIELD check_narrowing_const_only: 1;
  /* True if this conversion is taking place in a copy-initialization context
     and we should only consider converting constructors.  Only set in
     ck_base and ck_rvalue.  */
  BOOL_BITFIELD copy_init_p : 1;
  /* The type of the expression resulting from the conversion.  */
  tree type;
  union {
    /* The next conversion in the chain.  Since the conversions are
       arranged from outermost to innermost, the NEXT conversion will
       actually be performed before this conversion.  This variant is
       used only when KIND is neither ck_identity, ck_aggr, ck_ambig nor
       ck_list.  Please use the next_conversion function instead
       of using this field directly.  */
    conversion *next;
    /* The expression at the beginning of the conversion chain.  This
       variant is used only if KIND is ck_identity, ck_aggr, or ck_ambig.
       You can use conv_get_original_expr to get this expression.  */
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
static void maybe_warn_class_memaccess (location_t, tree,
					const vec<tree, va_gc> *);
static tree build_over_call (struct z_candidate *, int, tsubst_flags_t);
static tree convert_like (conversion *, tree, tsubst_flags_t);
static tree convert_like_with_context (conversion *, tree, tree, int,
				       tsubst_flags_t);
static void op_error (const op_location_t &, enum tree_code, enum tree_code,
		      tree, tree, tree, bool);
static struct z_candidate *build_user_type_conversion_1 (tree, tree, int,
							 tsubst_flags_t);
static void print_z_candidate (location_t, const char *, struct z_candidate *);
static void print_z_candidates (location_t, struct z_candidate *,
				tristate = tristate::unknown ());
static tree build_this (tree);
static struct z_candidate *splice_viable (struct z_candidate *, bool, bool *);
static bool any_strictly_viable (struct z_candidate *);
static struct z_candidate *add_template_candidate
	(struct z_candidate **, tree, tree, tree, tree, const vec<tree, va_gc> *,
	 tree, tree, tree, int, unification_kind_t, bool, tsubst_flags_t);
static struct z_candidate *add_template_candidate_real
	(struct z_candidate **, tree, tree, tree, tree, const vec<tree, va_gc> *,
	 tree, tree, tree, int, tree, unification_kind_t, bool, tsubst_flags_t);
static bool is_complete (tree);
static struct z_candidate *add_conv_candidate
	(struct z_candidate **, tree, tree, const vec<tree, va_gc> *, tree,
	 tree, tsubst_flags_t);
static struct z_candidate *add_function_candidate
	(struct z_candidate **, tree, tree, tree, const vec<tree, va_gc> *, tree,
	 tree, int, conversion**, bool, tsubst_flags_t);
static conversion *implicit_conversion (tree, tree, tree, bool, int,
					tsubst_flags_t);
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
static conversion *build_identity_conv (tree, tree);
static inline bool conv_binds_to_array_of_unknown_bound (conversion *);
static bool conv_is_prvalue (conversion *);
static tree prevent_lifetime_extension (tree);

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
	   || TREE_CODE (basetype) == ENUMERAL_TYPE)
	  && name == constructor_name (basetype))
	return true;

      /* Otherwise lookup the name, it could be an unrelated typedef
	 of the correct type.  */
      name = lookup_name (name, LOOK_want::TYPE);
      if (!name)
	return false;
      name = TREE_TYPE (name);
      if (name == error_mark_node)
	return false;
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
  else if (TREE_CODE (function) == FUNCTION_DECL
	   && DECL_IMMEDIATE_FUNCTION_P (function))
    function = build_address (function);
  else
    function = decay_conversion (function, complain, /*reject_builtin=*/false);

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
  /* Handle both CALL_EXPRs and AGGR_INIT_EXPRs.  */
  tree decl = cp_get_callee_fndecl_nofold (call);

  /* We check both the decl and the type; a function may be known not to
     throw without being declared throw().  */
  bool nothrow = decl && TREE_NOTHROW (decl);
  tree callee = cp_get_callee (call);
  if (callee)
    nothrow |= TYPE_NOTHROW_P (TREE_TYPE (TREE_TYPE (callee)));
  else if (TREE_CODE (call) == CALL_EXPR
	   && internal_fn_flags (CALL_EXPR_IFN (call)) & ECF_NOTHROW)
    nothrow = true;

  if (cfun && cp_function_chain && !cp_unevaluated_operand)
    {
      if (!nothrow && at_function_scope_p ())
	cp_function_chain->can_throw = 1;

      if (decl && TREE_THIS_VOLATILE (decl))
	current_function_returns_abnormally = 1;
    }

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
  gcc_assert (FUNC_OR_METHOD_TYPE_P (fntype));
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

  require_complete_eh_spec_types (fntype, decl);

  TREE_HAS_CONSTRUCTOR (function) = (decl && DECL_CONSTRUCTOR_P (decl));

  /* Don't pass empty class objects by value.  This is useful
     for tags in STL, which are used to control overload resolution.
     We don't need to handle other cases of copying empty classes.  */
  if (!decl || !fndecl_built_in_p (decl))
    for (i = 0; i < n; i++)
      {
	tree arg = CALL_EXPR_ARG (function, i);
	if (is_empty_class (TREE_TYPE (arg))
	    && simple_empty_class_p (TREE_TYPE (arg), arg, INIT_EXPR))
	  {
	    while (TREE_CODE (arg) == TARGET_EXPR)
	      /* We're disconnecting the initializer from its target,
		 don't create a temporary.  */
	      arg = TARGET_EXPR_INITIAL (arg);
	    tree t = build0 (EMPTY_CLASS_EXPR, TREE_TYPE (arg));
	    arg = build2 (COMPOUND_EXPR, TREE_TYPE (t), arg, t);
	    CALL_EXPR_ARG (function, i) = arg;
	  }
      }

  return function;
}

/* New overloading code.  */

struct z_candidate;

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
  rr_invalid_copy,
  rr_inherited_ctor,
  rr_constraint_failure,
  rr_ignored,
};

struct conversion_info {
  /* The index of the argument, 0-based.  */
  int n_arg;
  /* The actual argument or its type.  */
  tree from;
  /* The type of the parameter.  */
  tree to_type;
  /* The location of the argument.  */
  location_t loc;
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
      /* Whether EXPECTED should be treated as a lower bound.  */
      bool least_p;
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

  bool rewritten () const { return (flags & LOOKUP_REWRITTEN); }
  bool reversed () const { return (flags & LOOKUP_REVERSED); }
};

/* Returns true iff T is a null pointer constant in the sense of
   [conv.ptr].  */

bool
null_ptr_cst_p (tree t)
{
  tree type = TREE_TYPE (t);

  /* [conv.ptr]

     A null pointer constant is an integer literal ([lex.icon]) with value
     zero or a prvalue of type std::nullptr_t.  */
  if (NULLPTR_TYPE_P (type))
    return true;

  if (cxx_dialect >= cxx11)
    {
      STRIP_ANY_LOCATION_WRAPPER (t);

      /* Core issue 903 says only literal 0 is a null pointer constant.  */
      if (TREE_CODE (t) == INTEGER_CST
	  && !TREE_OVERFLOW (t)
	  && TREE_CODE (type) == INTEGER_TYPE
	  && integer_zerop (t)
	  && !char_type_p (type))
	return true;
    }
  else if (CP_INTEGRAL_TYPE_P (type))
    {
      t = fold_non_dependent_expr (t, tf_none);
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
	    && CONSTRUCTOR_NELTS (t)
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

/* RAII class to discard anything added to conversion_obstack.  */

struct conversion_obstack_sentinel
{
  void *p;
  conversion_obstack_sentinel (): p (conversion_obstack_alloc (0)) {}
  ~conversion_obstack_sentinel () { obstack_free (&conversion_obstack, p); }
};

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
arity_rejection (tree first_arg, int expected, int actual, bool least_p = false)
{
  struct rejection_reason *r = alloc_rejection (rr_arity);
  int adjust = first_arg != NULL_TREE;
  r->u.arity.expected = expected - adjust;
  r->u.arity.actual = actual - adjust;
  r->u.arity.least_p = least_p;
  return r;
}

static struct rejection_reason *
arg_conversion_rejection (tree first_arg, int n_arg, tree from, tree to,
			  location_t loc)
{
  struct rejection_reason *r = alloc_rejection (rr_arg_conversion);
  int adjust = first_arg != NULL_TREE;
  r->u.conversion.n_arg = n_arg - adjust;
  r->u.conversion.from = from;
  r->u.conversion.to_type = to;
  r->u.conversion.loc = loc;
  return r;
}

static struct rejection_reason *
bad_arg_conversion_rejection (tree first_arg, int n_arg, tree from, tree to,
			      location_t loc)
{
  struct rejection_reason *r = alloc_rejection (rr_bad_arg_conversion);
  int adjust = first_arg != NULL_TREE;
  r->u.bad_conversion.n_arg = n_arg - adjust;
  r->u.bad_conversion.from = from;
  r->u.bad_conversion.to_type = to;
  r->u.bad_conversion.loc = loc;
  return r;
}

static struct rejection_reason *
explicit_conversion_rejection (tree from, tree to)
{
  struct rejection_reason *r = alloc_rejection (rr_explicit_conversion);
  r->u.conversion.n_arg = 0;
  r->u.conversion.from = from;
  r->u.conversion.to_type = to;
  r->u.conversion.loc = UNKNOWN_LOCATION;
  return r;
}

static struct rejection_reason *
template_conversion_rejection (tree from, tree to)
{
  struct rejection_reason *r = alloc_rejection (rr_template_conversion);
  r->u.conversion.n_arg = 0;
  r->u.conversion.from = from;
  r->u.conversion.to_type = to;
  r->u.conversion.loc = UNKNOWN_LOCATION;
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

static struct rejection_reason *
inherited_ctor_rejection (void)
{
  struct rejection_reason *r = alloc_rejection (rr_inherited_ctor);
  return r;
}

/* Build a constraint failure record.  */

static struct rejection_reason *
constraint_failure (void)
{
  struct rejection_reason *r = alloc_rejection (rr_constraint_failure);
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

/* Make sure that all memory on the conversion obstack has been
   freed.  */

void
validate_conversion_obstack (void)
{
  if (conversion_obstack_initialized)
    gcc_assert ((obstack_next_free (&conversion_obstack)
		 == obstack_base (&conversion_obstack)));
}

/* Dynamically allocate an array of N conversions.  */

static conversion **
alloc_conversions (size_t n)
{
  return (conversion **) conversion_obstack_alloc (n * sizeof (conversion *));
}

/* True iff the active member of conversion::u for code CODE is NEXT.  */

static inline bool
has_next (conversion_kind code)
{
  return !(code == ck_identity
	   || code == ck_ambig
	   || code == ck_list
	   || code == ck_aggr
	   || code == ck_deferred_bad);
}

static conversion *
build_conv (conversion_kind code, tree type, conversion *from)
{
  conversion *t;
  conversion_rank rank = CONVERSION_RANK (from);

  /* Only call this function for conversions that use u.next.  */
  gcc_assert (from == NULL || has_next (code));

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
    case ck_fnptr:
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

  /* Can't make an array of these types.  */
  if (TYPE_REF_P (elttype)
      || TREE_CODE (elttype) == FUNCTION_TYPE
      || VOID_TYPE_P (elttype))
    return NULL;

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
      || !has_next (conv->kind))
    return NULL;
  return conv->u.next;
}

/* Strip to the first ck_user, ck_ambig, ck_list, ck_aggr or ck_identity
   encountered.  */

static conversion *
strip_standard_conversion (conversion *conv)
{
  while (conv
	 && conv->kind != ck_user
	 && has_next (conv->kind))
    conv = next_conversion (conv);
  return conv;
}

/* Subroutine of build_aggr_conv: check whether FROM is a valid aggregate
   initializer for array type ATYPE.  */

static bool
can_convert_array (tree atype, tree from, int flags, tsubst_flags_t complain)
{
  tree elttype = TREE_TYPE (atype);
  unsigned i;

  if (TREE_CODE (from) == CONSTRUCTOR)
    {
      for (i = 0; i < CONSTRUCTOR_NELTS (from); ++i)
	{
	  tree val = CONSTRUCTOR_ELT (from, i)->value;
	  bool ok;
	  if (TREE_CODE (elttype) == ARRAY_TYPE)
	    ok = can_convert_array (elttype, val, flags, complain);
	  else
	    ok = can_convert_arg (elttype, TREE_TYPE (val), val, flags,
				  complain);
	  if (!ok)
	    return false;
	}
      return true;
    }

  if (char_type_p (TYPE_MAIN_VARIANT (elttype))
      && TREE_CODE (tree_strip_any_location_wrapper (from)) == STRING_CST)
    return array_string_literal_compatible_p (atype, from);

  /* No other valid way to aggregate initialize an array.  */
  return false;
}

/* Helper for build_aggr_conv.  Return true if FIELD is in PSET, or if
   FIELD has ANON_AGGR_TYPE_P and any initializable field in there recursively
   is in PSET.  */

static bool
field_in_pset (hash_set<tree, true> &pset, tree field)
{
  if (pset.contains (field))
    return true;
  if (ANON_AGGR_TYPE_P (TREE_TYPE (field)))
    for (field = TYPE_FIELDS (TREE_TYPE (field));
	 field; field = DECL_CHAIN (field))
      {
	field = next_aggregate_field (field);
	if (field == NULL_TREE)
	  break;
	if (field_in_pset (pset, field))
	  return true;
      }
  return false;
}

/* Represent a conversion from CTOR, a braced-init-list, to TYPE, an
   aggregate class, if such a conversion is possible.  */

static conversion *
build_aggr_conv (tree type, tree ctor, int flags, tsubst_flags_t complain)
{
  unsigned HOST_WIDE_INT i = 0;
  conversion *c;
  tree field = next_aggregate_field (TYPE_FIELDS (type));
  tree empty_ctor = NULL_TREE;
  hash_set<tree, true> pset;

  /* We already called reshape_init in implicit_conversion, but it might not
     have done anything in the case of parenthesized aggr init.  */

  /* The conversions within the init-list aren't affected by the enclosing
     context; they're always simple copy-initialization.  */
  flags = LOOKUP_IMPLICIT|LOOKUP_NO_NARROWING;

  /* For designated initializers, verify that each initializer is convertible
     to corresponding TREE_TYPE (ce->index) and mark those FIELD_DECLs as
     visited.  In the following loop then ignore already visited
     FIELD_DECLs.  */
  tree idx, val;
  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (ctor), i, idx, val)
    {
      if (!idx)
	break;

      gcc_checking_assert (TREE_CODE (idx) == FIELD_DECL);

      tree ftype = TREE_TYPE (idx);
      bool ok;

      if (TREE_CODE (ftype) == ARRAY_TYPE)
	ok = can_convert_array (ftype, val, flags, complain);
      else
	ok = can_convert_arg (ftype, TREE_TYPE (val), val, flags,
			      complain);

      if (!ok)
	return NULL;

      /* For unions, there should be just one initializer.  */
      if (TREE_CODE (type) == UNION_TYPE)
	{
	  field = NULL_TREE;
	  i = 1;
	  break;
	}
      pset.add (idx);
    }

  for (; field; field = next_aggregate_field (DECL_CHAIN (field)))
    {
      tree ftype = TREE_TYPE (field);
      bool ok;

      if (!pset.is_empty () && field_in_pset (pset, field))
	continue;
      if (i < CONSTRUCTOR_NELTS (ctor))
	{
	  constructor_elt *ce = CONSTRUCTOR_ELT (ctor, i);
	  gcc_checking_assert (!ce->index);
	  val = ce->value;
	  ++i;
	}
      else if (DECL_INITIAL (field))
	val = get_nsdmi (field, /*ctor*/false, complain);
      else if (TYPE_REF_P (ftype))
	/* Value-initialization of reference is ill-formed.  */
	return NULL;
      else
	{
	  if (empty_ctor == NULL_TREE)
	    empty_ctor = build_constructor (init_list_type_node, NULL);
	  val = empty_ctor;
	}

      if (TREE_CODE (ftype) == ARRAY_TYPE)
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
  c->u.expr = ctor;
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

  for (auto &e: CONSTRUCTOR_ELTS (ctor))
    {
      conversion *sub
	= implicit_conversion (elttype, TREE_TYPE (e.value), e.value,
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
  c->u.expr = ctor;
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
  bool bad = false;
  bool user = false;
  enum conversion_rank rank = cr_exact;

  if (len != 2)
    return NULL;

  flags = LOOKUP_IMPLICIT|LOOKUP_NO_NARROWING;

  for (auto &e: CONSTRUCTOR_ELTS (ctor))
    {
      conversion *sub
	= implicit_conversion (elttype, TREE_TYPE (e.value), e.value,
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
  c->u.expr = ctor;
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
		     int flags, tsubst_flags_t complain)
{
  enum tree_code fcode, tcode;
  conversion *conv;
  bool fromref = false;
  tree qualified_to;

  to = non_reference (to);
  if (TYPE_REF_P (from))
    {
      fromref = true;
      from = TREE_TYPE (from);
    }
  qualified_to = to;
  to = strip_top_quals (to);
  from = strip_top_quals (from);

  if (expr && type_unknown_p (expr))
    {
      if (TYPE_PTRFN_P (to) || TYPE_PTRMEMFUNC_P (to))
	{
	  tsubst_flags_t tflags = tf_conv;
	  expr = instantiate_type (to, expr, tflags);
	  if (expr == error_mark_node)
	    return NULL;
	  from = TREE_TYPE (expr);
	}
      else if (TREE_CODE (to) == BOOLEAN_TYPE)
	{
	  /* Necessary for eg, TEMPLATE_ID_EXPRs (c++/50961).  */
	  expr = resolve_nondeduced_context (expr, complain);
	  from = TREE_TYPE (expr);
	}
    }

  fcode = TREE_CODE (from);
  tcode = TREE_CODE (to);

  conv = build_identity_conv (from, expr);
  if (fcode == FUNCTION_TYPE || fcode == ARRAY_TYPE)
    {
      from = type_decays_to (from);
      fcode = TREE_CODE (from);
      /* Tell convert_like that we're using the address.  */
      conv->rvaluedness_matches_p = true;
      conv = build_conv (ck_lvalue, from, conv);
    }
  /* Wrapping a ck_rvalue around a class prvalue (as a result of using
     obvalue_p) seems odd, since it's already a prvalue, but that's how we
     express the copy constructor call required by copy-initialization.  */
  else if (fromref || (expr && obvalue_p (expr)))
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
      /* If we're performing copy-initialization, remember to skip
	 explicit constructors.  */
      if (flags & LOOKUP_ONLYCONVERTING)
	conv->copy_init_p = true;
    }

   /* Allow conversion between `__complex__' data types.  */
  if (tcode == COMPLEX_TYPE && fcode == COMPLEX_TYPE)
    {
      /* The standard conversion sequence to convert FROM to TO is
	 the standard conversion sequence to perform componentwise
	 conversion.  */
      conversion *part_conv = standard_conversion
	(TREE_TYPE (to), TREE_TYPE (from), NULL_TREE, c_cast_p, flags,
	 complain);

      if (!part_conv)
	conv = NULL;
      else if (part_conv->kind == ck_identity)
	/* Leave conv alone.  */;
      else
	{
	  conv = build_conv (part_conv->kind, to, conv);
	  conv->rank = part_conv->rank;
	}

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
      && ((expr && null_ptr_cst_p (expr))
	  || NULLPTR_TYPE_P (from)))
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

      if (tcode == POINTER_TYPE)
	{
	  to_pointee = TREE_TYPE (to);
	  from_pointee = TREE_TYPE (from);

	  /* Since this is the target of a pointer, it can't have function
	     qualifiers, so any TYPE_QUALS must be for attributes const or
	     noreturn.  Strip them.  */
	  if (TREE_CODE (to_pointee) == FUNCTION_TYPE
	      && TYPE_QUALS (to_pointee))
	    to_pointee = build_qualified_type (to_pointee, TYPE_UNQUALIFIED);
	  if (TREE_CODE (from_pointee) == FUNCTION_TYPE
	      && TYPE_QUALS (from_pointee))
	    from_pointee = build_qualified_type (from_pointee, TYPE_UNQUALIFIED);
	}
      else
	{
	  to_pointee = TYPE_PTRMEM_POINTED_TO_TYPE (to);
	  from_pointee = TYPE_PTRMEM_POINTED_TO_TYPE (from);
	}

      if (tcode == POINTER_TYPE
	  && same_type_ignoring_top_level_qualifiers_p (from_pointee,
							to_pointee))
	;
      else if (VOID_TYPE_P (to_pointee)
	       && !TYPE_PTRDATAMEM_P (from)
	       && TREE_CODE (from_pointee) != FUNCTION_TYPE)
	{
	  tree nfrom = TREE_TYPE (from);
	  /* Don't try to apply restrict to void.  */
	  int quals = cp_type_quals (nfrom) & ~TYPE_QUAL_RESTRICT;
	  from_pointee = cp_build_qualified_type (void_type_node, quals);
	  from = build_pointer_type (from_pointee);
	  conv = build_conv (ck_ptr, from, conv);
	}
      else if (TYPE_PTRDATAMEM_P (from))
	{
	  tree fbase = TYPE_PTRMEM_CLASS_TYPE (from);
	  tree tbase = TYPE_PTRMEM_CLASS_TYPE (to);

	  if (same_type_p (fbase, tbase))
	    /* No base conversion needed.  */;
	  else if (DERIVED_FROM_P (fbase, tbase)
		   && (same_type_ignoring_top_level_qualifiers_p
		       (from_pointee, to_pointee)))
	    {
	      from = build_ptrmem_type (tbase, from_pointee);
	      conv = build_conv (ck_pmem, from, conv);
	    }
	  else
	    return NULL;
	}
      else if (CLASS_TYPE_P (from_pointee)
	       && CLASS_TYPE_P (to_pointee)
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
	       && DERIVED_FROM_P (to_pointee, from_pointee))
	{
	  from_pointee
	    = cp_build_qualified_type (to_pointee,
				       cp_type_quals (from_pointee));
	  from = build_pointer_type (from_pointee);
	  conv = build_conv (ck_ptr, from, conv);
	  conv->base_p = true;
	}

      if (same_type_p (from, to))
	/* OK */;
      else if (c_cast_p && comp_ptr_ttypes_const (to, from, bounds_either))
	/* In a C-style cast, we ignore CV-qualification because we
	   are allowed to perform a static_cast followed by a
	   const_cast.  */
	conv = build_conv (ck_qual, to, conv);
      else if (!c_cast_p && comp_ptr_ttypes (to_pointee, from_pointee))
	conv = build_conv (ck_qual, to, conv);
      else if (expr && string_conv_p (to, expr, 0))
	/* converting from string constant to char *.  */
	conv = build_conv (ck_qual, to, conv);
      else if (fnptr_conv_p (to, from))
	conv = build_conv (ck_fnptr, to, conv);
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

      /* If FBASE and TBASE are equivalent but incomplete, DERIVED_FROM_P
	 yields false.  But a pointer to member of incomplete class is OK.  */
      if (!same_type_p (fbase, tbase) && !DERIVED_FROM_P (fbase, tbase))
	return NULL;

      tree fstat = static_fn_type (fromfn);
      tree tstat = static_fn_type (tofn);
      if (same_type_p (tstat, fstat)
	  || fnptr_conv_p (tstat, fstat))
	/* OK */;
      else
	return NULL;

      if (!same_type_p (fbase, tbase))
	{
	  from = build_memfn_type (fstat,
				   tbase,
				   cp_type_quals (tbase),
				   type_memfn_rqual (tofn));
	  from = build_ptrmemfunc_type (build_pointer_type (from));
	  conv = build_conv (ck_pmem, from, conv);
	  conv->base_p = true;
	}
      if (fnptr_conv_p (tstat, fstat))
	conv = build_conv (ck_fnptr, to, conv);
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
	  if (flags & LOOKUP_NO_NARROWING)
	    conv->check_narrowing = true;
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

      /* If we're parsing an enum with no fixed underlying type, we're
	 dealing with an incomplete type, which renders the conversion
	 ill-formed.  */
      if (!COMPLETE_TYPE_P (from))
	return NULL;

      conv = build_conv (ck_std, to, conv);

      tree underlying_type = NULL_TREE;
      if (TREE_CODE (from) == ENUMERAL_TYPE
	  && ENUM_FIXED_UNDERLYING_TYPE_P (from))
	underlying_type = ENUM_UNDERLYING_TYPE (from);

      /* Give this a better rank if it's a promotion.

	 To handle CWG 1601, also bump the rank if we are converting
	 an enumeration with a fixed underlying type to the underlying
	 type.  */
      if ((same_type_p (to, type_promotes_to (from))
	   || (underlying_type && same_type_p (to, underlying_type)))
	  && next_conversion (conv)->rank <= cr_promotion)
	conv->rank = cr_promotion;

      /* A prvalue of floating-point type can be converted to a prvalue of
	 another floating-point type with a greater or equal conversion
	 rank ([conv.rank]).  A prvalue of standard floating-point type can
	 be converted to a prvalue of another standard floating-point type.
	 For backwards compatibility with handling __float128 and other
	 non-standard floating point types, allow all implicit floating
	 point conversions if neither type is extended floating-point
	 type and if at least one of them is, fail if they have unordered
	 conversion rank or from has higher conversion rank.  */
      if (fcode == REAL_TYPE
	  && tcode == REAL_TYPE
	  && (extended_float_type_p (from)
	      || extended_float_type_p (to))
	  && cp_compare_floating_point_conversion_ranks (from, to) >= 2)
	conv->bad_p = true;
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
      /* If we're performing copy-initialization, remember to skip
	 explicit constructors.  */
      if (flags & LOOKUP_ONLYCONVERTING)
	conv->copy_init_p = true;
    }
  else
    return NULL;

  if (flags & LOOKUP_NO_NARROWING)
    conv->check_narrowing = true;

  return conv;
}

/* Returns nonzero if T1 is reference-related to T2.

   This is considered when a reference to T1 is initialized by a T2.  */

bool
reference_related_p (tree t1, tree t2)
{
  if (t1 == error_mark_node || t2 == error_mark_node)
    return false;

  t1 = TYPE_MAIN_VARIANT (t1);
  t2 = TYPE_MAIN_VARIANT (t2);

  /* [dcl.init.ref]

     Given types "cv1 T1" and "cv2 T2," "cv1 T1" is reference-related
     to "cv2 T2" if T1 is similar to T2, or T1 is a base class of T2.  */
  return (similar_type_p (t1, t2)
	  || (CLASS_TYPE_P (t1) && CLASS_TYPE_P (t2)
	      && DERIVED_FROM_P (t1, t2)));
}

/* Returns nonzero if T1 is reference-compatible with T2.  */

bool
reference_compatible_p (tree t1, tree t2)
{
  /* [dcl.init.ref]

     "cv1 T1" is reference compatible with "cv2 T2" if
     a prvalue of type "pointer to cv2 T2" can be converted to the type
     "pointer to cv1 T1" via a standard conversion sequence.  */
  tree ptype1 = build_pointer_type (t1);
  tree ptype2 = build_pointer_type (t2);
  conversion *conv = standard_conversion (ptype1, ptype2, NULL_TREE,
					  /*c_cast_p=*/false, 0, tf_none);
  if (!conv || conv->bad_p)
    return false;
  return true;
}

/* Return true if converting FROM to TO would involve a qualification
   conversion.  */

static bool
involves_qualification_conversion_p (tree to, tree from)
{
  /* If we're not convering a pointer to another one, we won't get
     a qualification conversion.  */
  if (!((TYPE_PTR_P (to) && TYPE_PTR_P (from))
	|| (TYPE_PTRDATAMEM_P (to) && TYPE_PTRDATAMEM_P (from))))
    return false;

  conversion *conv = standard_conversion (to, from, NULL_TREE,
					  /*c_cast_p=*/false, 0, tf_none);
  for (conversion *t = conv; t; t = next_conversion (t))
    if (t->kind == ck_qual)
      return true;

  return false;
}

/* A reference of the indicated TYPE is being bound directly to the
   expression represented by the implicit conversion sequence CONV.
   Return a conversion sequence for this binding.  */

static conversion *
direct_reference_binding (tree type, conversion *conv)
{
  tree t;

  gcc_assert (TYPE_REF_P (type));
  gcc_assert (!TYPE_REF_P (conv->type));

  t = TREE_TYPE (type);

  if (conv->kind == ck_identity)
    /* Mark the identity conv as to not decay to rvalue.  */
    conv->rvaluedness_matches_p = true;

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
  if (is_properly_derived_from (conv->type, t))
    {
      /* Represent the derived-to-base conversion.  */
      conv = build_conv (ck_base, t, conv);
      /* We will actually be binding to the base-class subobject in
	 the derived class, so we mark this conversion appropriately.
	 That way, convert_like knows not to generate a temporary.  */
      conv->need_temporary_p = false;
    }
  else if (involves_qualification_conversion_p (t, conv->type))
    /* Represent the qualification conversion.  After DR 2352
       #1 and #2 were indistinguishable conversion sequences:

	 void f(int*); // #1
	 void f(const int* const &); // #2
	 void g(int* p) { f(p); }

       because the types "int *" and "const int *const" are
       reference-related and we were binding both directly and they
       had the same rank.  To break it up, we add a ck_qual under the
       ck_ref_bind so that conversion sequence ranking chooses #1.

       We strip_top_quals here which is also what standard_conversion
       does.  Failure to do so would confuse comp_cv_qual_signature
       into thinking that in

	 void f(const int * const &); // #1
	 void f(const int *); // #2
	 int *x;
	 f(x);

       #2 is a better match than #1 even though they're ambiguous (97296).  */
    conv = build_conv (ck_qual, strip_top_quals (t), conv);

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
  conversion *bad_direct_conv = nullptr;
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

  bool copy_list_init = false;
  bool single_list_conv = false;
  if (expr && BRACE_ENCLOSED_INITIALIZER_P (expr))
    {
      maybe_warn_cpp0x (CPP0X_INITIALIZER_LISTS);
      /* DR 1288: Otherwise, if the initializer list has a single element
	 of type E and ... [T's] referenced type is reference-related to E,
	 the object or reference is initialized from that element...

	 ??? With P0388R4, we should bind 't' directly to U{}:
	   using U = A[2];
	   A (&&t)[] = {U{}};
	 because A[] and A[2] are reference-related.  But we don't do it
	 because grok_reference_init has deduced the array size (to 1), and
	 A[1] and A[2] aren't reference-related.  */
      if (CONSTRUCTOR_NELTS (expr) == 1
	  && !CONSTRUCTOR_IS_DESIGNATED_INIT (expr))
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
	  else if (CLASS_TYPE_P (etype) && TYPE_HAS_CONVERSION (etype))
	    /* CWG1996: jason's proposed drafting adds "or initializing T from E
	       would bind directly".  We check that in the direct binding with
	       conversion code below.  */
	    single_list_conv = true;
	}
      /* Otherwise, if T is a reference type, a prvalue temporary of the type
	 referenced by T is copy-list-initialized, and the reference is bound
	 to that temporary. */
      copy_list_init = true;
    skip:;
    }

  if (TYPE_REF_P (from))
    {
      from = TREE_TYPE (from);
      if (!TYPE_REF_IS_RVALUE (rfrom)
	  || TREE_CODE (from) == FUNCTION_TYPE)
	gl_kind = clk_ordinary;
      else
	gl_kind = clk_rvalueref;
    }
  else if (expr)
    gl_kind = lvalue_kind (expr);
  else if (CLASS_TYPE_P (from)
	   || TREE_CODE (from) == ARRAY_TYPE)
    gl_kind = clk_class;
  else
    gl_kind = clk_none;

  /* Don't allow a class prvalue when LOOKUP_NO_TEMP_BIND.  */
  if ((flags & LOOKUP_NO_TEMP_BIND)
      && (gl_kind & clk_class))
    gl_kind = clk_none;

  /* Same mask as real_lvalue_p.  */
  is_lvalue = gl_kind && !(gl_kind & (clk_rvalueref|clk_class));

  tfrom = from;
  if ((gl_kind & clk_bitfield) != 0)
    tfrom = unlowered_expr_type (expr);

  /* Figure out whether or not the types are reference-related and
     reference compatible.  We have to do this after stripping
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
  if ((related_p || compatible_p) && gl_kind)
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

      if (TYPE_REF_P (rfrom))
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
	  && TREE_CODE (to) != FUNCTION_TYPE)
	conv->bad_p = true;

      /* Nor the reverse.  */
      if (!is_lvalue && !TYPE_REF_IS_RVALUE (rto)
	  /* Unless it's really a C++20 lvalue being treated as an xvalue.
	     But in C++23, such an expression is just an xvalue, not a special
	     lvalue, so the binding is once again ill-formed.  */
	  && !(cxx_dialect <= cxx20
	       && (gl_kind & clk_implicit_rval))
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
  else if (!related_p
	   && !(flags & LOOKUP_NO_CONVERSION)
	   && (CLASS_TYPE_P (from) || single_list_conv))
    {
      tree rexpr = expr;
      if (single_list_conv)
	rexpr = CONSTRUCTOR_ELT (expr, 0)->value;

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
      z_candidate *cand = build_user_type_conversion_1 (rto, rexpr, flags,
							complain);
      if (cand)
	{
	  if (!cand->second_conv->bad_p)
	    return cand->second_conv;

	  /* Direct reference binding wasn't successful and yielded a bad
	     conversion.  Proceed with trying to go through a temporary
	     instead, and if that also fails then we'll return this bad
	     conversion rather than no conversion for sake of better
	     diagnostics.  */
	  bad_direct_conv = cand->second_conv;
	}
    }

  /* From this point on, we conceptually need temporaries, even if we
     elide them.  Only the cases above are "direct bindings".  */
  if (flags & LOOKUP_NO_TEMP_BIND)
    return bad_direct_conv ? bad_direct_conv : nullptr;

  /* [over.ics.rank]

     When a parameter of reference type is not bound directly to an
     argument expression, the conversion sequence is the one required
     to convert the argument expression to the underlying type of the
     reference according to _over.best.ics_.  Conceptually, this
     conversion sequence corresponds to copy-initializing a temporary
     of the underlying type with the argument expression.  Any
     difference in top-level cv-qualification is subsumed by the
     initialization itself and does not constitute a conversion.  */

  bool maybe_valid_p = true;

  /* [dcl.init.ref]

     Otherwise, the reference shall be an lvalue reference to a
     non-volatile const type, or the reference shall be an rvalue
     reference.  */
  if (!CP_TYPE_CONST_NON_VOLATILE_P (to) && !TYPE_REF_IS_RVALUE (rto))
    maybe_valid_p = false;

  /* [dcl.init.ref]

     Otherwise, a temporary of type "cv1 T1" is created and
     initialized from the initializer expression using the rules for a
     non-reference copy initialization.  If T1 is reference-related to
     T2, cv1 must be the same cv-qualification as, or greater
     cv-qualification than, cv2; otherwise, the program is ill-formed.  */
  if (related_p && !at_least_as_qualified_p (to, from))
    maybe_valid_p = false;

  /* We try below to treat an invalid reference binding as a bad conversion
     to improve diagnostics, but doing so may cause otherwise unnecessary
     instantiations that can lead to a hard error.  So during the first pass
     of overload resolution wherein we shortcut bad conversions, instead just
     produce a special conversion indicating a second pass is necessary if
     there's no strictly viable candidate.  */
  if (!maybe_valid_p && (flags & LOOKUP_SHORTCUT_BAD_CONVS))
    {
      if (bad_direct_conv)
	return bad_direct_conv;

      conv = alloc_conversion (ck_deferred_bad);
      conv->bad_p = true;
      return conv;
    }

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
    return bad_direct_conv ? bad_direct_conv : nullptr;

  if (conv->user_conv_p)
    {
      if (copy_list_init)
	/* Remember this was copy-list-initialization.  */
	conv->need_temporary_p = true;

      /* If initializing the temporary used a conversion function,
	 recalculate the second conversion sequence.  */
      for (conversion *t = conv; t; t = next_conversion (t))
	if (t->kind == ck_user
	    && c_cast_p && !maybe_valid_p)
	  {
	    if (complain & tf_warning)
	      warning (OPT_Wcast_user_defined,
		       "casting %qT to %qT does not use %qD",
		       from, rto, t->cand->fn);
	    /* Don't let recalculation try to make this valid.  */
	    break;
	  }
	else if (t->kind == ck_user
		 && DECL_CONV_FN_P (t->cand->fn))
	  {
	    tree ftype = TREE_TYPE (TREE_TYPE (t->cand->fn));
	    /* A prvalue of non-class type is cv-unqualified.  */
	    if (!TYPE_REF_P (ftype) && !CLASS_TYPE_P (ftype))
	      ftype = cv_unqualified (ftype);
	    int sflags = (flags|LOOKUP_NO_CONVERSION)&~LOOKUP_NO_TEMP_BIND;
	    conversion *new_second
	      = reference_binding (rto, ftype, NULL_TREE, c_cast_p,
				   sflags, complain);
	    if (!new_second)
	      return bad_direct_conv ? bad_direct_conv : nullptr;
	    conv = merge_conversion_sequences (t, new_second);
	    gcc_assert (maybe_valid_p || conv->bad_p);
	    return conv;
	  }
    }

  conv = build_conv (ck_ref_bind, rto, conv);
  /* This reference binding, unlike those above, requires the
     creation of a temporary.  */
  conv->need_temporary_p = true;
  conv->rvaluedness_matches_p = TYPE_REF_IS_RVALUE (rto);
  conv->bad_p |= !maybe_valid_p;

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
	    |LOOKUP_NO_TEMP_BIND|LOOKUP_NO_RVAL_BIND|LOOKUP_NO_NARROWING
	    |LOOKUP_PROTECT|LOOKUP_NO_NON_INTEGRAL|LOOKUP_SHORTCUT_BAD_CONVS);

  /* FIXME: actually we don't want warnings either, but we can't just
     have 'complain &= ~(tf_warning|tf_error)' because it would cause
     the regression of, eg, g++.old-deja/g++.benjamin/16077.C.
     We really ought not to issue that warning until we've committed
     to that conversion.  */
  complain &= ~tf_error;

  /* Call reshape_init early to remove redundant braces.  */
  if (expr && BRACE_ENCLOSED_INITIALIZER_P (expr) && CLASS_TYPE_P (to))
    {
      to = complete_type (to);
      if (!COMPLETE_TYPE_P (to))
	return nullptr;
      if (!CLASSTYPE_NON_AGGREGATE (to))
	{
	  expr = reshape_init (to, expr, complain);
	  if (expr == error_mark_node)
	    return nullptr;
	  from = TREE_TYPE (expr);
	}
    }

  if (TYPE_REF_P (to))
    conv = reference_binding (to, from, expr, c_cast_p, flags, complain);
  else
    conv = standard_conversion (to, from, expr, c_cast_p, flags, complain);

  if (conv)
    return conv;

  if (expr && BRACE_ENCLOSED_INITIALIZER_P (expr))
    {
      if (is_std_init_list (to) && !CONSTRUCTOR_IS_DESIGNATED_INIT (expr))
	return build_list_conv (to, expr, flags, complain);

      /* As an extension, allow list-initialization of _Complex.  */
      if (TREE_CODE (to) == COMPLEX_TYPE
	  && !CONSTRUCTOR_IS_DESIGNATED_INIT (expr))
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
	  else if (nelts == 1 && !CONSTRUCTOR_IS_DESIGNATED_INIT (expr))
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
	{
	  if (BRACE_ENCLOSED_INITIALIZER_P (expr)
	      && CONSTRUCTOR_NELTS (expr) == 1
	      && !CONSTRUCTOR_IS_DESIGNATED_INIT (expr)
	      && !is_list_ctor (cand->fn))
	    {
	      /* "If C is not an initializer-list constructor and the
		 initializer list has a single element of type cv U, where U is
		 X or a class derived from X, the implicit conversion sequence
		 has Exact Match rank if U is X, or Conversion rank if U is
		 derived from X."  */
	      tree elt = CONSTRUCTOR_ELT (expr, 0)->value;
	      tree elttype = TREE_TYPE (elt);
	      if (reference_related_p (to, elttype))
		return implicit_conversion (to, elttype, elt,
					    c_cast_p, flags, complain);
	    }
	  conv = cand->second_conv;
	}

      /* We used to try to bind a reference to a temporary here, but that
	 is now handled after the recursive call to this function at the end
	 of reference_binding.  */
      return conv;
    }

  return NULL;
}

/* Like implicit_conversion, but return NULL if the conversion is bad.

   This is not static so that check_non_deducible_conversion can call it within
   add_template_candidate_real as part of overload resolution; it should not be
   called outside of overload resolution.  */

conversion *
good_conversion (tree to, tree from, tree expr,
		 int flags, tsubst_flags_t complain)
{
  conversion *c = implicit_conversion (to, from, expr, /*cast*/false,
				       flags, complain);
  if (c && c->bad_p)
    c = NULL;
  return c;
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

  if (convs && cand->reversed ())
    /* Swap the conversions for comparison in joust; we'll swap them back
       before build_over_call.  */
    std::swap (convs[0], convs[1]);

  return cand;
}

/* FN is a function from the overload set that we outright didn't even
   consider (for some reason); add it to the list as an non-viable "ignored"
   candidate.  */

static z_candidate *
add_ignored_candidate (z_candidate **candidates, tree fn)
{
  /* No need to dynamically allocate these.  */
  static const rejection_reason reason_ignored = { rr_ignored, {} };

  struct z_candidate *cand = (struct z_candidate *)
    conversion_obstack_alloc (sizeof (struct z_candidate));

  cand->fn = fn;
  cand->reason = const_cast<rejection_reason *> (&reason_ignored);
  cand->next = *candidates;
  *candidates = cand;

  return cand;
}

/* True iff CAND is a candidate added by add_ignored_candidate.  */

static bool
ignored_candidate_p (const z_candidate *cand)
{
  return cand->reason && cand->reason->code == rr_ignored;
}

/* Return the number of remaining arguments in the parameter list
   beginning with ARG.  */

int
remaining_arguments (tree arg)
{
  int n;

  for (n = 0; arg != NULL_TREE && arg != void_list_node;
       arg = TREE_CHAIN (arg))
    n++;

  return n;
}

/* [over.match.copy]: When initializing a temporary object (12.2) to be bound
   to the first parameter of a constructor where the parameter is of type
   "reference to possibly cv-qualified T" and the constructor is called with a
   single argument in the context of direct-initialization of an object of type
   "cv2 T", explicit conversion functions are also considered.

   So set LOOKUP_COPY_PARM to let reference_binding know that
   it's being called in that context.  */

int
conv_flags (int i, int nargs, tree fn, tree arg, int flags)
{
  int lflags = flags;
  tree t;
  if (i == 0 && nargs == 1 && DECL_CONSTRUCTOR_P (fn)
      && (t = FUNCTION_FIRST_USER_PARMTYPE (fn))
      && (same_type_ignoring_top_level_qualifiers_p
	  (non_reference (TREE_VALUE (t)), DECL_CONTEXT (fn))))
    {
      if (!(flags & LOOKUP_ONLYCONVERTING))
	lflags |= LOOKUP_COPY_PARM;
      if ((flags & LOOKUP_LIST_INIT_CTOR)
	  && BRACE_ENCLOSED_INITIALIZER_P (arg))
	lflags |= LOOKUP_NO_CONVERSION;
    }
  else
    lflags |= LOOKUP_ONLYCONVERTING;

  return lflags;
}

/* Build an appropriate 'this' conversion for the method FN and class
   type CTYPE from the value ARG (having type ARGTYPE) to the type PARMTYPE.
   This function modifies PARMTYPE, ARGTYPE and ARG.  */

static conversion *
build_this_conversion (tree fn, tree ctype,
		       tree& parmtype, tree& argtype, tree& arg,
		       int flags, tsubst_flags_t complain)
{
  gcc_assert (DECL_IOBJ_MEMBER_FUNCTION_P (fn)
	      && !DECL_CONSTRUCTOR_P (fn));

  /* The type of the implicit object parameter ('this') for
     overload resolution is not always the same as for the
     function itself; conversion functions are considered to
     be members of the class being converted, and functions
     introduced by a using-declaration are considered to be
     members of the class that uses them.

     Since build_over_call ignores the ICS for the `this'
     parameter, we can just change the parm type.  */
  parmtype = cp_build_qualified_type (ctype,
				      cp_type_quals (TREE_TYPE (parmtype)));
  bool this_p = true;
  if (FUNCTION_REF_QUALIFIED (TREE_TYPE (fn)))
    {
      /* If the function has a ref-qualifier, the implicit
	 object parameter has reference type.  */
      bool rv = FUNCTION_RVALUE_QUALIFIED (TREE_TYPE (fn));
      parmtype = cp_build_reference_type (parmtype, rv);
      /* The special handling of 'this' conversions in compare_ics
	 does not apply if there is a ref-qualifier.  */
      this_p = false;
    }
  else
    {
      parmtype = build_pointer_type (parmtype);
      /* We don't use build_this here because we don't want to
	 capture the object argument until we've chosen a
	 non-static member function.  */
      arg = build_address (arg);
      argtype = lvalue_type (arg);
    }
  flags |= LOOKUP_ONLYCONVERTING;
  conversion *t = implicit_conversion (parmtype, argtype, arg,
				       /*c_cast_p=*/false, flags, complain);
  t->this_p = this_p;
  return t;
}

/* Create an overload candidate for the function or method FN called
   with the argument list FIRST_ARG/ARGS and add it to CANDIDATES.
   FLAGS is passed on to implicit_conversion.

   This does not change ARGS.

   CTYPE, if non-NULL, is the type we want to pretend this function
   comes from for purposes of overload resolution.

   SHORTCUT_BAD_CONVS controls how we handle "bad" argument conversions.
   If true, we stop computing conversions upon seeing the first bad
   conversion.  This is used by add_candidates to avoid computing
   more conversions than necessary in the presence of a strictly viable
   candidate, while preserving the defacto behavior of overload resolution
   when it turns out there are only non-strictly viable candidates.  */

static struct z_candidate *
add_function_candidate (struct z_candidate **candidates,
			tree fn, tree ctype, tree first_arg,
			const vec<tree, va_gc> *args, tree access_path,
			tree conversion_path, int flags,
			conversion **convs,
			bool shortcut_bad_convs,
			tsubst_flags_t complain)
{
  tree parmlist = TYPE_ARG_TYPES (TREE_TYPE (fn));
  int i, len;
  tree parmnode;
  tree orig_first_arg = first_arg;
  int skip;
  int viable = 1;
  struct rejection_reason *reason = NULL;

  /* The `this', `in_chrg' and VTT arguments to constructors are not
     considered in overload resolution.  */
  if (DECL_CONSTRUCTOR_P (fn))
    {
      if (ctor_omit_inherited_parms (fn))
	/* Bring back parameters omitted from an inherited ctor.  */
	parmlist = FUNCTION_FIRST_USER_PARMTYPE (DECL_ORIGIN (fn));
      else
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
  if (!convs)
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

  /* An inherited constructor (12.6.3 [class.inhctor.init]) that has a first
     parameter of type "reference to cv C" (including such a constructor
     instantiated from a template) is excluded from the set of candidate
     functions when used to construct an object of type D with an argument list
     containing a single argument if C is reference-related to D.  */
  if (viable && len == 1 && parmlist && DECL_CONSTRUCTOR_P (fn)
      && flag_new_inheriting_ctors
      && DECL_INHERITED_CTOR (fn))
    {
      tree ptype = non_reference (TREE_VALUE (parmlist));
      tree dtype = DECL_CONTEXT (fn);
      tree btype = DECL_INHERITED_CTOR_BASE (fn);
      if (reference_related_p (ptype, dtype)
	  && reference_related_p (btype, ptype))
	{
	  viable = false;
	  reason = inherited_ctor_rejection ();
	}
    }

  /* Second, for a function to be viable, its constraints must be
     satisfied. */
  if (flag_concepts && viable && !constraints_satisfied_p (fn))
    {
      reason = constraint_failure ();
      viable = false;
    }

  /* When looking for a function from a subobject from an implicit
     copy/move constructor/operator=, don't consider anything that takes (a
     reference to) an unrelated type.  See c++/44909 and core 1092.  */
  if (viable && parmlist && (flags & LOOKUP_DEFAULTED))
    {
      if (DECL_CONSTRUCTOR_P (fn))
	i = 1;
      else if (DECL_ASSIGNMENT_OPERATOR_P (fn)
	       && DECL_OVERLOADED_OPERATOR_IS (fn, NOP_EXPR))
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

  if (shortcut_bad_convs)
    flags |= LOOKUP_SHORTCUT_BAD_CONVS;
  else
    flags &= ~LOOKUP_SHORTCUT_BAD_CONVS;

  /* Third, for F to be a viable function, there shall exist for each
     argument an implicit conversion sequence that converts that argument
     to the corresponding parameter of F.  */

  parmnode = parmlist;

  for (i = 0; i < len; ++i)
    {
      tree argtype, to_type;
      tree arg;

      if (parmnode == void_list_node)
	break;

      if (convs[i])
	{
	  /* Already set during deduction.  */
	  parmnode = TREE_CHAIN (parmnode);
	  continue;
	}

      if (i == 0 && first_arg != NULL_TREE)
	arg = first_arg;
      else
	arg = CONST_CAST_TREE (
		(*args)[i + skip - (first_arg != NULL_TREE ? 1 : 0)]);
      argtype = lvalue_type (arg);

      conversion *t;
      if (parmnode)
	{
	  tree parmtype = TREE_VALUE (parmnode);
	  if (i == 0
	      && DECL_IOBJ_MEMBER_FUNCTION_P (fn)
	      && !DECL_CONSTRUCTOR_P (fn))
	    t = build_this_conversion (fn, ctype, parmtype, argtype, arg,
				       flags, complain);
	  else
	    {
	      int lflags = conv_flags (i, len-skip, fn, arg, flags);
	      t = implicit_conversion (parmtype, argtype, arg,
				       /*c_cast_p=*/false, lflags, complain);
	    }
	  to_type = parmtype;
	  parmnode = TREE_CHAIN (parmnode);
	}
      else
	{
	  t = build_identity_conv (argtype, arg);
	  t->ellipsis_p = true;
	  to_type = argtype;
	}

      convs[i] = t;
      if (! t)
	{
	  viable = 0;
	  reason = arg_conversion_rejection (first_arg, i, argtype, to_type,
					     EXPR_LOCATION (arg));
	  break;
	}

      if (t->bad_p)
	{
	  viable = -1;
	  reason = bad_arg_conversion_rejection (first_arg, i, arg, to_type,
						 EXPR_LOCATION (arg));
	  if (shortcut_bad_convs)
	    break;
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
		    const vec<tree, va_gc> *arglist,
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

  len = vec_safe_length (arglist) + 1;
  convs = alloc_conversions (len);
  parmnode = parmlist;
  viable = 1;
  flags = LOOKUP_IMPLICIT;
  reason = NULL;

  /* Don't bother looking up the same type twice.  */
  if (*candidates && (*candidates)->fn == totype)
    return NULL;

  if (!constraints_satisfied_p (fn))
    {
      reason = constraint_failure ();
      viable = 0;
      return add_candidate (candidates, fn, obj, arglist, len, convs,
			    access_path, conversion_path, viable, reason, flags);
    }

  for (i = 0; i < len; ++i)
    {
      tree arg, argtype, convert_type = NULL_TREE;
      conversion *t;

      if (i == 0)
	arg = obj;
      else
	arg = (*arglist)[i - 1];
      argtype = lvalue_type (arg);

      if (i == 0)
	{
	  t = build_identity_conv (argtype, NULL_TREE);
	  t = build_conv (ck_user, totype, t);
	  /* Leave the 'cand' field null; we'll figure out the conversion in
	     convert_like if this candidate is chosen.  */
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
	  reason = bad_arg_conversion_rejection (NULL_TREE, i, arg, convert_type,
						 EXPR_LOCATION (arg));
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

  return add_candidate (candidates, totype, obj, arglist, len, convs,
			access_path, conversion_path, viable, reason, flags);
}

static void
build_builtin_candidate (struct z_candidate **candidates, tree fnname,
			 tree type1, tree type2, const vec<tree,va_gc> &args,
			 tree *argtypes, int flags, tsubst_flags_t complain)
{
  conversion *t;
  conversion **convs;
  size_t num_convs;
  int viable = 1;
  tree types[2];
  struct rejection_reason *reason = NULL;

  types[0] = type1;
  types[1] = type2;

  num_convs = args.length ();
  convs = alloc_conversions (num_convs);

  /* TRUTH_*_EXPR do "contextual conversion to bool", which means explicit
     conversion ops are allowed.  We handle that here by just checking for
     boolean_type_node because other operators don't ask for it.  COND_EXPR
     also does contextual conversion to bool for the first operand, but we
     handle that in build_conditional_expr, and type1 here is operand 2.  */
  if (type1 != boolean_type_node)
    flags |= LOOKUP_ONLYCONVERTING;

  for (unsigned i = 0; i < 2 && i < num_convs; ++i)
    {
      t = implicit_conversion (types[i], argtypes[i], args[i],
			       /*c_cast_p=*/false, flags, complain);
      if (! t)
	{
	  viable = 0;
	  /* We need something for printing the candidate.  */
	  t = build_identity_conv (types[i], NULL_TREE);
	  reason = arg_conversion_rejection (NULL_TREE, i, argtypes[i],
					     types[i], EXPR_LOCATION (args[i]));
	}
      else if (t->bad_p)
	{
	  viable = 0;
	  reason = bad_arg_conversion_rejection (NULL_TREE, i, args[i],
						 types[i],
						 EXPR_LOCATION (args[i]));
	}
      convs[i] = t;
    }

  /* For COND_EXPR we rearranged the arguments; undo that now.  */
  if (num_convs == 3)
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
					     boolean_type_node,
					     EXPR_LOCATION (args[2]));
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
	  || SCALAR_FLOAT_TYPE_P (type));
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
		       tree type2, vec<tree,va_gc> &args, tree *argtypes,
		       int flags, tsubst_flags_t complain)
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

/* 4 For every pair (T, VQ), where T is an arithmetic type other than bool,
     and  VQ  is  either  volatile or empty, there exist candidate operator
     functions of the form
	     VQ T&   operator++(VQ T&);
	     T       operator++(VQ T&, int);
   5 For every pair (T, VQ), where T is an arithmetic type other than bool,
     and VQ is either volatile or empty, there exist candidate operator
     functions of the form
	     VQ T&   operator--(VQ T&);
	     T       operator--(VQ T&, int);
   6 For every pair (T, VQ), where T is a cv-qualified or cv-unqualified object
     type, and VQ is either volatile or empty, there exist candidate operator
     functions of the form
	     T*VQ&   operator++(T*VQ&);
	     T*VQ&   operator--(T*VQ&);
	     T*      operator++(T*VQ&, int);
	     T*      operator--(T*VQ&, int);  */

    case POSTDECREMENT_EXPR:
    case PREDECREMENT_EXPR:
      if (TREE_CODE (type1) == BOOLEAN_TYPE)
	return;
      /* FALLTHRU */
    case POSTINCREMENT_EXPR:
    case PREINCREMENT_EXPR:
      /* P0002R1, Remove deprecated operator++(bool) added "other than bool"
	 to p4.  */
      if (TREE_CODE (type1) == BOOLEAN_TYPE && cxx_dialect >= cxx17)
	return;
      if (ARITHMETIC_TYPE_P (type1) || TYPE_PTROB_P (type1))
	{
	  type1 = build_reference_type (type1);
	  break;
	}
      return;

/* 7 For every cv-qualified or cv-unqualified object type T, there
     exist candidate operator functions of the form

	     T&      operator*(T*);


   8 For every function type T that does not have cv-qualifiers or
     a ref-qualifier, there exist candidate operator functions of the form
	     T&      operator*(T*);  */

    case INDIRECT_REF:
      if (TYPE_PTR_P (type1)
	  && (TYPE_PTROB_P (type1)
	      || TREE_CODE (TREE_TYPE (type1)) == FUNCTION_TYPE))
	break;
      return;

/* 9 For every type T, there exist candidate operator functions of the form
	     T*      operator+(T*);

   10 For every floating-point or promoted integral type T, there exist
      candidate operator functions of the form
	     T       operator+(T);
	     T       operator-(T);  */

    case UNARY_PLUS_EXPR: /* unary + */
      if (TYPE_PTR_P (type1))
	break;
      /* FALLTHRU */
    case NEGATE_EXPR:
      if (ARITHMETIC_TYPE_P (type1))
	break;
      return;

/* 11 For every promoted integral type T,  there  exist  candidate  operator
      functions of the form
	     T       operator~(T);  */

    case BIT_NOT_EXPR:
      if (INTEGRAL_OR_UNSCOPED_ENUMERATION_TYPE_P (type1))
	break;
      return;

/* 12 For every quintuple (C1, C2, T, CV1, CV2), where C2 is a class type, C1
     is the same type as C2 or is a derived class of C2, and T is an object
     type or a function type there exist candidate operator functions of the
     form
	     CV12 T& operator->*(CV1 C1*, CV2 T C2::*);
     where CV12 is the union of CV1 and CV2.  */

    case MEMBER_REF:
      if (TYPE_PTR_P (type1) && TYPE_PTRMEM_P (type2))
	{
	  tree c1 = TREE_TYPE (type1);
	  tree c2 = TYPE_PTRMEM_CLASS_TYPE (type2);

	  if (CLASS_TYPE_P (c1) && DERIVED_FROM_P (c2, c1)
	      && (TYPE_PTRMEMFUNC_P (type2)
		  || is_complete (TYPE_PTRMEM_POINTED_TO_TYPE (type2))))
	    break;
	}
      return;

/* 13 For every pair of types L and R, where each of L and R is a floating-point
      or promoted integral type, there exist candidate operator functions of the
      form
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

   14 For every integral type T there exists a candidate operator function of
      the form

       std::strong_ordering operator<=>(T, T);

   15 For every pair of floating-point types L and R, there exists a candidate
      operator function of the form

       std::partial_ordering operator<=>(L, R);

   16 For every cv-qualified or cv-unqualified object type T there exist
      candidate operator functions of the form
	     T*      operator+(T*, std::ptrdiff_t);
	     T&      operator[](T*, std::ptrdiff_t);
	     T*      operator-(T*, std::ptrdiff_t);
	     T*      operator+(std::ptrdiff_t, T*);
	     T&      operator[](std::ptrdiff_t, T*);

   17 For every T, where T is a pointer to object type, there exist candidate
      operator functions of the form
	     std::ptrdiff_t operator-(T, T);

   18 For every T, where T is an enumeration type or a pointer type, there
      exist candidate operator functions of the form
	     bool    operator<(T, T);
	     bool    operator>(T, T);
	     bool    operator<=(T, T);
	     bool    operator>=(T, T);
	     bool    operator==(T, T);
	     bool    operator!=(T, T);
	     R       operator<=>(T, T);

      where R is the result type specified in [expr.spaceship].

   19 For every T, where T is a pointer-to-member type or std::nullptr_t,
      there exist candidate operator functions of the form
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
      /* FALLTHRU */
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
      if (ARITHMETIC_TYPE_P (type1) && ARITHMETIC_TYPE_P (type2))
	break;
      return;

      /* This isn't exactly what's specified above for operator<=>, but it's
	 close enough.  In particular, we don't care about the return type
	 specified above; it doesn't participate in overload resolution and it
	 doesn't affect the semantics of the built-in operator.  */
    case SPACESHIP_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
      if ((TYPE_PTRMEMFUNC_P (type1) && TYPE_PTRMEMFUNC_P (type2))
	  || (TYPE_PTRDATAMEM_P (type1) && TYPE_PTRDATAMEM_P (type2)))
	break;
      if (NULLPTR_TYPE_P (type1) && NULLPTR_TYPE_P (type2))
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
      /* FALLTHRU */
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
	  /* FALLTHRU */
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
      && (TYPE_REF_P (type1)
	  || (TYPE_PTR_P (type1) && TYPE_PTR_P (type2))
	  || (TYPE_PTRDATAMEM_P (type1) && TYPE_PTRDATAMEM_P (type2))
	  || TYPE_PTRMEMFUNC_P (type1)
	  || MAYBE_CLASS_TYPE_P (type1)
	  || TREE_CODE (type1) == ENUMERAL_TYPE))
    {
      if (TYPE_PTR_OR_PTRMEM_P (type1))
	{
	  tree cptype = composite_pointer_type (input_location,
						type1, type2,
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
			enum tree_code code2, tree fnname,
			vec<tree, va_gc> *argv,
			int flags, tsubst_flags_t complain)
{
  int ref1;
  int enum_p = 0;
  tree type, argtypes[3], t;
  /* TYPES[i] is the set of possible builtin-operator parameter types
     we will consider for the Ith argument.  */
  vec<tree, va_gc> *types[2];
  unsigned ix;
  vec<tree, va_gc> &args = *argv;
  unsigned len = args.length ();

  for (unsigned i = 0; i < len; ++i)
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
    case CO_AWAIT_EXPR:
      return;

    case COND_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case SPACESHIP_EXPR:
      enum_p = 1;
      /* Fall through.  */

    default:
      ref1 = 0;
    }

  types[0] = make_tree_vector ();
  types[1] = make_tree_vector ();

  if (len == 3)
    len = 2;
  for (unsigned i = 0; i < len; ++i)
    {
      if (MAYBE_CLASS_TYPE_P (argtypes[i]))
	{
	  tree convs;

	  if (i == 0 && code == MODIFY_EXPR && code2 == NOP_EXPR)
	    return;

	  convs = lookup_conversions (argtypes[i]);

	  if (code == COND_EXPR)
	    {
	      if (lvalue_p (args[i]))
		vec_safe_push (types[i], build_reference_type (argtypes[i]));

	      vec_safe_push (types[i], TYPE_MAIN_VARIANT (argtypes[i]));
	    }

	  else if (! convs)
	    return;

	  for (; convs; convs = TREE_CHAIN (convs))
	    {
	      type = TREE_TYPE (convs);

	      if (i == 0 && ref1
		  && (!TYPE_REF_P (type)
		      || CP_TYPE_CONST_P (TREE_TYPE (type))))
		continue;

	      if (code == COND_EXPR && TYPE_REF_P (type))
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
	  if (code == COND_EXPR && lvalue_p (args[i]))
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
   CTYPE are ignored, and OBJ is as for add_conv_candidate.

   SHORTCUT_BAD_CONVS is as in add_function_candidate.  */

static struct z_candidate*
add_template_candidate_real (struct z_candidate **candidates, tree tmpl,
			     tree ctype, tree explicit_targs, tree first_arg,
			     const vec<tree, va_gc> *arglist, tree return_type,
			     tree access_path, tree conversion_path,
			     int flags, tree obj, unification_kind_t strict,
			     bool shortcut_bad_convs, tsubst_flags_t complain)
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
  conversion **convs = NULL;

  /* We don't do deduction on the in-charge parameter, the VTT
     parameter or 'this'.  */
  if (DECL_IOBJ_MEMBER_FUNCTION_P (tmpl))
    {
      if (first_arg_without_in_chrg != NULL_TREE)
	first_arg_without_in_chrg = NULL_TREE;
      else if (return_type && strict == DEDUCE_CALL)
	/* We're deducing for a call to the result of a template conversion
	   function, so the args don't contain 'this'; leave them alone.  */;
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
    return add_ignored_candidate (candidates, tmpl);

  if (DECL_CONSTRUCTOR_P (tmpl) && nargs == 2
      && same_type_ignoring_top_level_qualifiers_p (TREE_TYPE (first_arg),
						    TREE_TYPE ((*arglist)[0])))
    {
      /* 12.8/6 says, "A declaration of a constructor for a class X is
	 ill-formed if its first parameter is of type (optionally cv-qualified)
	 X and either there are no other parameters or else all other
	 parameters have default arguments. A member function template is never
	 instantiated to produce such a constructor signature."

	 So if we're trying to copy an object of the containing class, don't
	 consider a template constructor that has a first parameter type that
	 is just a template parameter, as we would deduce a signature that we
	 would then reject in the code below.  */
      if (tree firstparm = FUNCTION_FIRST_USER_PARMTYPE (tmpl))
	{
	  firstparm = TREE_VALUE (firstparm);
	  if (PACK_EXPANSION_P (firstparm))
	    firstparm = PACK_EXPANSION_PATTERN (firstparm);
	  if (TREE_CODE (firstparm) == TEMPLATE_TYPE_PARM)
	    {
	      gcc_assert (!explicit_targs);
	      reason = invalid_copy_with_fn_template_rejection ();
	      goto fail;
	    }
	}
    }

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

  if (!obj)
    {
      /* Check that there's no obvious arity mismatch before proceeding with
	 deduction.  This avoids substituting explicit template arguments
	 into the template or e.g. derived-to-base parm/arg unification
	 (which could result in an error outside the immediate context) when
	 the resulting candidate would be unviable anyway.  */
      int min_arity = 0, max_arity = 0;
      tree parms = TYPE_ARG_TYPES (TREE_TYPE (tmpl));
      parms = skip_artificial_parms_for (tmpl, parms);
      for (; parms != void_list_node; parms = TREE_CHAIN (parms))
	{
	  if (!parms || PACK_EXPANSION_P (TREE_VALUE (parms)))
	    {
	      max_arity = -1;
	      break;
	    }
	  if (TREE_PURPOSE (parms))
	    /* A parameter with a default argument.  */
	    ++max_arity;
	  else
	    ++min_arity, ++max_arity;
	}
      if (ia < (unsigned)min_arity)
	{
	  /* Too few arguments.  */
	  reason = arity_rejection (NULL_TREE, min_arity, ia,
				    /*least_p=*/(max_arity == -1));
	  goto fail;
	}
      else if (max_arity != -1 && ia > (unsigned)max_arity)
	{
	  /* Too many arguments.  */
	  reason = arity_rejection (NULL_TREE, max_arity, ia);
	  goto fail;
	}

      convs = alloc_conversions (nargs);

      if (shortcut_bad_convs
	  && DECL_IOBJ_MEMBER_FUNCTION_P (tmpl)
	  && !DECL_CONSTRUCTOR_P (tmpl))
	{
	  /* Check the 'this' conversion before proceeding with deduction.
	     This is effectively an extension of the DR 1391 resolution
	     that we perform in check_non_deducible_conversions, though it's
	     convenient to do this extra check here instead of there.  */
	  tree parmtype = TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (tmpl)));
	  tree argtype = lvalue_type (first_arg);
	  tree arg = first_arg;
	  conversion *t = build_this_conversion (tmpl, ctype,
						 parmtype, argtype, arg,
						 flags, complain);
	  convs[0] = t;
	  if (t->bad_p)
	    {
	      reason = bad_arg_conversion_rejection (first_arg, 0,
						     arg, parmtype,
						     EXPR_LOCATION (arg));
	      goto fail;
	    }
	}
    }

  errs = errorcount+sorrycount;
  fn = fn_type_unification (tmpl, explicit_targs, targs,
			    args_without_in_chrg,
			    nargs_without_in_chrg,
			    return_type, strict, flags, convs,
			    false, complain & tf_decltype);

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

  /* Now the explicit specifier might have been deduced; check if this
     declaration is explicit.  If it is and we're ignoring non-converting
     constructors, don't add this function to the set of candidates.  */
  if (((flags & (LOOKUP_ONLYCONVERTING|LOOKUP_LIST_INIT_CTOR))
       == LOOKUP_ONLYCONVERTING)
      && DECL_NONCONVERTING_P (fn))
    return add_ignored_candidate (candidates, fn);

  if (DECL_CONSTRUCTOR_P (fn) && nargs == 2)
    {
      tree arg_types = FUNCTION_FIRST_USER_PARMTYPE (fn);
      if (arg_types && same_type_p (TYPE_MAIN_VARIANT (TREE_VALUE (arg_types)),
				    ctype))
	{
	  /* We're trying to produce a constructor with a prohibited signature,
	     as discussed above; handle here any cases we didn't catch then,
	     such as X(X<T>).  */
	  reason = invalid_copy_with_fn_template_rejection ();
	  goto fail;
	}
    }

  if (obj != NULL_TREE)
    /* Aha, this is a conversion function.  */
    cand = add_conv_candidate (candidates, fn, obj, arglist,
			       access_path, conversion_path, complain);
  else
    cand = add_function_candidate (candidates, fn, ctype,
				   first_arg, arglist, access_path,
				   conversion_path, flags, convs,
				   shortcut_bad_convs, complain);
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
  int viable = (reason->code == rr_bad_arg_conversion ? -1 : 0);
  return add_candidate (candidates, tmpl, first_arg, arglist, nargs, convs,
			access_path, conversion_path, viable, reason, flags);
}


static struct z_candidate *
add_template_candidate (struct z_candidate **candidates, tree tmpl, tree ctype,
			tree explicit_targs, tree first_arg,
			const vec<tree, va_gc> *arglist, tree return_type,
			tree access_path, tree conversion_path, int flags,
			unification_kind_t strict, bool shortcut_bad_convs,
			tsubst_flags_t complain)
{
  return
    add_template_candidate_real (candidates, tmpl, ctype,
				 explicit_targs, first_arg, arglist,
				 return_type, access_path, conversion_path,
				 flags, NULL_TREE, strict, shortcut_bad_convs,
				 complain);
}

/* Create an overload candidate for the conversion function template TMPL,
   returning RETURN_TYPE, which will be invoked for expression OBJ to produce a
   pointer-to-function which will in turn be called with the argument list
   ARGLIST, and add it to CANDIDATES.  This does not change ARGLIST.  FLAGS is
   passed on to implicit_conversion.  */

static struct z_candidate *
add_template_conv_candidate (struct z_candidate **candidates, tree tmpl,
			     tree obj,
			     const vec<tree, va_gc> *arglist,
			     tree return_type, tree access_path,
			     tree conversion_path, tsubst_flags_t complain)
{
  return
    add_template_candidate_real (candidates, tmpl, NULL_TREE, NULL_TREE,
				 NULL_TREE, arglist, return_type, access_path,
				 conversion_path, 0, obj, DEDUCE_CALL,
				 /*shortcut_bad_convs=*/false, complain);
}

/* The CANDS are the set of candidates that were considered for
   overload resolution.  Sort CANDS so that the strictly viable
   candidates appear first, followed by non-strictly viable candidates,
   followed by non-viable candidates.  Returns the first candidate
   in this sorted list.  If any of the candidates were viable, set
   *ANY_VIABLE_P to true.  STRICT_P is true if a candidate should be
   considered viable only if it is strictly viable when setting
   *ANY_VIABLE_P.  */

static struct z_candidate*
splice_viable (struct z_candidate *cands,
	       bool strict_p,
	       bool *any_viable_p)
{
  z_candidate *strictly_viable = nullptr;
  z_candidate **strictly_viable_tail = &strictly_viable;

  z_candidate *non_strictly_viable = nullptr;
  z_candidate **non_strictly_viable_tail = &non_strictly_viable;

  z_candidate *non_viable = nullptr;
  z_candidate **non_viable_tail = &non_viable;

  z_candidate *non_viable_ignored = nullptr;
  z_candidate **non_viable_ignored_tail = &non_viable_ignored;

  /* Be strict inside templates, since build_over_call won't actually
     do the conversions to get pedwarns.  */
  if (processing_template_decl)
    strict_p = true;

  for (z_candidate *cand = cands; cand; cand = cand->next)
    {
      if (!strict_p
	  && (cand->viable == 1 || TREE_CODE (cand->fn) == TEMPLATE_DECL))
	/* Be strict in the presence of a viable candidate.  Also if
	   there are template candidates, so that we get deduction errors
	   for them instead of silently preferring a bad conversion.  */
	strict_p = true;

      /* Move this candidate to the appropriate list according to
	 its viability.  */
      auto& tail = (cand->viable == 1 ? strictly_viable_tail
		    : cand->viable == -1 ? non_strictly_viable_tail
		    : ignored_candidate_p (cand) ? non_viable_ignored_tail
		    : non_viable_tail);
      *tail = cand;
      tail = &cand->next;
    }

  *any_viable_p = (strictly_viable != nullptr
		   || (!strict_p && non_strictly_viable != nullptr));

  /* Combine the lists.  */
  *non_viable_ignored_tail = nullptr;
  *non_viable_tail = non_viable_ignored;
  *non_strictly_viable_tail = non_viable;
  *strictly_viable_tail = non_strictly_viable;

  return strictly_viable;
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
  if (DECL_LOCAL_DECL_P (fn1) || DECL_LOCAL_DECL_P (fn2)
      || DECL_EXTERN_C_FUNCTION_P (fn1))
    return decls_match (fn1, fn2);
  return fn1 == fn2;
}

/* Print information about a candidate FN being rejected due to INFO.  */

static void
print_conversion_rejection (location_t loc, struct conversion_info *info,
			    tree fn)
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
		"%<this%> parameter from %qH to %qI",
		from, info->to_type);
    }
  else if (!TYPE_P (info->from))
    {
      if (info->n_arg >= 0)
	inform (loc, "  conversion of argument %d would be ill-formed:",
		info->n_arg + 1);
      iloc_sentinel ils = loc;
      perform_implicit_conversion (info->to_type, info->from,
				   tf_warning_or_error);
    }
  else if (info->n_arg == -2)
    /* Conversion of conversion function return value failed.  */
    inform (loc, "  no known conversion from %qH to %qI",
	    from, info->to_type);
  else
    {
      if (TREE_CODE (fn) == FUNCTION_DECL)
	loc = get_fndecl_argument_location (fn, info->n_arg);
      inform (loc, "  no known conversion for argument %d from %qH to %qI",
	      info->n_arg + 1, from, info->to_type);
    }
}

/* Print information about a candidate with WANT parameters and we found
   HAVE.  */

static void
print_arity_information (location_t loc, unsigned int have, unsigned int want,
			 bool least_p)
{
  if (least_p)
    inform_n (loc, want,
	      "  candidate expects at least %d argument, %d provided",
	      "  candidate expects at least %d arguments, %d provided",
	      want, have);
  else
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
		     : ACONCAT ((_(msgstr), " ", NULL)));
  tree fn = candidate->fn;
  if (flag_new_inheriting_ctors)
    fn = strip_inheriting_ctors (fn);
  location_t cloc = location_of (fn);

  if (identifier_p (fn))
    {
      cloc = loc;
      if (candidate->num_convs == 3)
	inform (cloc, "%s%<%D(%T, %T, %T)%> (built-in)", msg, fn,
		candidate->convs[0]->type,
		candidate->convs[1]->type,
		candidate->convs[2]->type);
      else if (candidate->num_convs == 2)
	inform (cloc, "%s%<%D(%T, %T)%> (built-in)", msg, fn,
		candidate->convs[0]->type,
		candidate->convs[1]->type);
      else
	inform (cloc, "%s%<%D(%T)%> (built-in)", msg, fn,
		candidate->convs[0]->type);
    }
  else if (TYPE_P (fn))
    inform (cloc, "%s%qT (conversion)", msg, fn);
  else if (candidate->viable == -1)
    inform (cloc, "%s%#qD (near match)", msg, fn);
  else if (ignored_candidate_p (candidate))
    inform (cloc, "%s%#qD (ignored)", msg, fn);
  else if (DECL_DELETED_FN (fn))
    inform (cloc, "%s%#qD (deleted)", msg, fn);
  else if (candidate->reversed ())
    inform (cloc, "%s%#qD (reversed)", msg, fn);
  else if (candidate->rewritten ())
    inform (cloc, "%s%#qD (rewritten)", msg, fn);
  else
    inform (cloc, "%s%#qD", msg, fn);
  if (fn != candidate->fn)
    {
      cloc = location_of (candidate->fn);
      inform (cloc, "  inherited here");
    }
  /* Give the user some information about why this candidate failed.  */
  if (candidate->reason != NULL)
    {
      struct rejection_reason *r = candidate->reason;

      switch (r->code)
	{
	case rr_arity:
	  print_arity_information (cloc, r->u.arity.actual,
				   r->u.arity.expected,
				   r->u.arity.least_p);
	  break;
	case rr_arg_conversion:
	  print_conversion_rejection (cloc, &r->u.conversion, fn);
	  break;
	case rr_bad_arg_conversion:
	  print_conversion_rejection (cloc, &r->u.bad_conversion, fn);
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
			       NULL, true, false);
	  break;
	case rr_invalid_copy:
	  inform (cloc,
		  "  a constructor taking a single argument of its own "
		  "class type is invalid");
	  break;
	case rr_constraint_failure:
	  diagnose_constraints (cloc, fn, NULL_TREE);
	  break;
	case rr_inherited_ctor:
	  inform (cloc, "  an inherited constructor is not a candidate for "
		  "initialization from an expression of the same or derived "
		  "type");
	  break;
	case rr_ignored:
	  break;
	case rr_none:
	default:
	  /* This candidate didn't have any issues or we failed to
	     handle a particular code.  Either way...  */
	  gcc_unreachable ();
	}
    }
}

/* Print information about each overload candidate in CANDIDATES,
   which is assumed to have gone through splice_viable and tourney
   (if splice_viable succeeded).  */

static void
print_z_candidates (location_t loc, struct z_candidate *candidates,
		    tristate only_viable_p /* = tristate::unknown () */)
{
  struct z_candidate *cand1;
  struct z_candidate **cand2;

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

  /* Unless otherwise specified, if there's a (strictly) viable candidate
     then we assume we're being called as part of diagnosing ambiguity, in
     which case we want to print only viable candidates since non-viable
     candidates couldn't have contributed to the ambiguity.  */
  if (only_viable_p.is_unknown ())
    only_viable_p = candidates->viable == 1;

  for (; candidates; candidates = candidates->next)
    {
      if (only_viable_p.is_true () && candidates->viable != 1)
	break;
      if (ignored_candidate_p (candidates) && !flag_diagnostics_all_candidates)
	{
	  inform (loc, "some candidates omitted; "
		       "use %<-fdiagnostics-all-candidates%> to display them");
	  break;
	}
      print_z_candidate (loc, N_("candidate:"), candidates);
    }
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

  if ((*t)->rvaluedness_matches_p)
    /* We're binding a reference directly to the result of the conversion.
       build_user_type_conversion_1 stripped the REFERENCE_TYPE from the return
       type, but we want it back.  */
    user_seq->type = TREE_TYPE (TREE_TYPE (user_seq->cand->fn));

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
		     const vec<tree, va_gc> *args, tree totype,
		     tree explicit_targs, bool template_only,
		     tree conversion_path, tree access_path,
		     int flags,
		     struct z_candidate **candidates,
		     tsubst_flags_t complain)
{
  gcc_assert (*candidates == NULL);

  /* We're looking for a ctor for list-initialization.  */
  flags |= LOOKUP_LIST_INIT_CTOR;
  /* And we don't allow narrowing conversions.  We also use this flag to
     avoid the copy constructor call for copy-list-initialization.  */
  flags |= LOOKUP_NO_NARROWING;

  unsigned nart = num_artificial_parms_for (OVL_FIRST (fns)) - 1;
  tree init_list = (*args)[nart];

  /* Always use the default constructor if the list is empty (DR 990).  */
  if (CONSTRUCTOR_NELTS (init_list) == 0
      && TYPE_HAS_DEFAULT_CONSTRUCTOR (totype))
    ;
  else if (CONSTRUCTOR_IS_DESIGNATED_INIT (init_list)
	   && !CP_AGGREGATE_TYPE_P (totype))
    {
      if (complain & tf_error)
	error ("designated initializers cannot be used with a "
	       "non-aggregate type %qT", totype);
      return;
    }
  /* If the class has a list ctor, try passing the list as a single
     argument first, but only consider list ctors.  */
  else if (TYPE_HAS_LIST_CTOR (totype))
    {
      flags |= LOOKUP_LIST_ONLY;
      add_candidates (fns, first_arg, args, NULL_TREE,
		      explicit_targs, template_only, conversion_path,
		      access_path, flags, candidates, complain);
      if (any_strictly_viable (*candidates))
	return;
    }

  /* Expand the CONSTRUCTOR into a new argument vec.  */
  vec<tree, va_gc> *new_args;
  vec_alloc (new_args, nart + CONSTRUCTOR_NELTS (init_list));
  for (unsigned i = 0; i < nart; ++i)
    new_args->quick_push ((*args)[i]);
  for (unsigned i = 0; i < CONSTRUCTOR_NELTS (init_list); ++i)
    new_args->quick_push (CONSTRUCTOR_ELT (init_list, i)->value);

  /* We aren't looking for list-ctors anymore.  */
  flags &= ~LOOKUP_LIST_ONLY;
  /* We allow more user-defined conversions within an init-list.  */
  flags &= ~LOOKUP_NO_CONVERSION;

  add_candidates (fns, first_arg, new_args, NULL_TREE,
		  explicit_targs, template_only, conversion_path,
		  access_path, flags, candidates, complain);
}

/* Given C(std::initializer_list<A>), return A.  */

static tree
list_ctor_element_type (tree fn)
{
  gcc_checking_assert (is_list_ctor (fn));

  tree parm = FUNCTION_FIRST_USER_PARMTYPE (fn);
  parm = non_reference (TREE_VALUE (parm));
  return TREE_VEC_ELT (CLASSTYPE_TI_ARGS (parm), 0);
}

/* If EXPR is a braced-init-list where the elements all decay to the same type,
   return that type.  */

static tree
braced_init_element_type (tree expr)
{
  if (TREE_CODE (expr) == CONSTRUCTOR
      && TREE_CODE (TREE_TYPE (expr)) == ARRAY_TYPE)
    return TREE_TYPE (TREE_TYPE (expr));
  if (!BRACE_ENCLOSED_INITIALIZER_P (expr))
    return NULL_TREE;

  tree elttype = NULL_TREE;
  for (constructor_elt &e: CONSTRUCTOR_ELTS (expr))
    {
      tree type = TREE_TYPE (e.value);
      type = type_decays_to (type);
      if (!elttype)
	elttype = type;
      else if (!same_type_p (type, elttype))
	return NULL_TREE;
    }
  return elttype;
}

/* True iff EXPR contains any temporaries with non-trivial destruction.

   ??? Also ignore classes with non-trivial but no-op destruction other than
   std::allocator?  */

static bool
has_non_trivial_temporaries (tree expr)
{
  auto_vec<tree*> temps;
  cp_walk_tree_without_duplicates (&expr, find_temps_r, &temps);
  for (tree *p : temps)
    {
      tree t = TREE_TYPE (*p);
      if (!TYPE_HAS_TRIVIAL_DESTRUCTOR (t)
	  && !is_std_allocator (t))
	return true;
    }
  return false;
}

/* We're initializing an array of ELTTYPE from INIT.  If it seems useful,
   return INIT as an array (of its own type) so the caller can initialize the
   target array in a loop.  */

static tree
maybe_init_list_as_array (tree elttype, tree init)
{
  /* Only do this if the array can go in rodata but not once converted.  */
  if (!TYPE_NON_AGGREGATE_CLASS (elttype))
    return NULL_TREE;
  tree init_elttype = braced_init_element_type (init);
  if (!init_elttype || !SCALAR_TYPE_P (init_elttype) || !TREE_CONSTANT (init))
    return NULL_TREE;

  /* Check with a stub expression to weed out special cases, and check whether
     we call the same function for direct-init as copy-list-init.  */
  conversion_obstack_sentinel cos;
  tree arg = build_stub_object (init_elttype);
  conversion *c = implicit_conversion (elttype, init_elttype, arg, false,
				       LOOKUP_NORMAL, tf_none);
  if (c && c->kind == ck_rvalue)
    c = next_conversion (c);
  if (!c || c->kind != ck_user)
    return NULL_TREE;

  tree first = CONSTRUCTOR_ELT (init, 0)->value;
  conversion *fc = implicit_conversion (elttype, init_elttype, first, false,
					LOOKUP_IMPLICIT|LOOKUP_NO_NARROWING,
					tf_none);
  if (fc && fc->kind == ck_rvalue)
    fc = next_conversion (fc);
  if (!fc || fc->kind != ck_user || fc->cand->fn != c->cand->fn)
    return NULL_TREE;
  first = convert_like (fc, first, tf_none);
  if (first == error_mark_node)
    /* Let the normal code give the error.  */
    return NULL_TREE;

  /* Don't do this if the conversion would be constant.  */
  first = maybe_constant_init (first);
  if (TREE_CONSTANT (first))
    return NULL_TREE;

  /* We can't do this if the conversion creates temporaries that need
     to live until the whole array is initialized.  */
  if (has_non_trivial_temporaries (first))
    return NULL_TREE;

  /* We can't do this if copying from the initializer_list would be
     ill-formed.  */
  tree copy_argtypes = make_tree_vec (1);
  TREE_VEC_ELT (copy_argtypes, 0)
    = cp_build_qualified_type (elttype, TYPE_QUAL_CONST);
  if (!is_xible (INIT_EXPR, elttype, copy_argtypes))
    return NULL_TREE;

  init_elttype = cp_build_qualified_type (init_elttype, TYPE_QUAL_CONST);
  tree arr = build_array_of_n_type (init_elttype, CONSTRUCTOR_NELTS (init));
  arr = finish_compound_literal (arr, init, tf_none);
  DECL_MERGEABLE (TARGET_EXPR_SLOT (arr)) = true;
  return arr;
}

/* If we were going to call e.g. vector(initializer_list<string>) starting
   with a list of string-literals (which is inefficient, see PR105838),
   instead build an array of const char* and pass it to the range constructor.
   But only do this for standard library types, where we can assume the
   transformation makes sense.

   Really the container classes should have initializer_list<U> constructors to
   get the same effect more simply; this is working around that lack.  */

static tree
maybe_init_list_as_range (tree fn, tree expr)
{
  if (!processing_template_decl
      && BRACE_ENCLOSED_INITIALIZER_P (expr)
      && is_list_ctor (fn)
      && decl_in_std_namespace_p (fn))
    {
      tree to = list_ctor_element_type (fn);
      if (tree init = maybe_init_list_as_array (to, expr))
	{
	  tree begin = decay_conversion (TARGET_EXPR_SLOT (init), tf_none);
	  tree nelts = array_type_nelts_top (TREE_TYPE (init));
	  tree end = cp_build_binary_op (input_location, PLUS_EXPR, begin,
					 nelts, tf_none);
	  begin = cp_build_compound_expr (init, begin, tf_none);
	  return build_constructor_va (init_list_type_node, 2,
				       NULL_TREE, begin, NULL_TREE, end);
	}
    }

  return NULL_TREE;
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

  if (CLASS_TYPE_P (totype))
    /* Use lookup_fnfields_slot instead of lookup_fnfields to avoid
       creating a garbage BASELINK; constructors can't be inherited.  */
    ctors = get_class_binding (totype, complete_ctor_identifier);

  tree to_nonref = non_reference (totype);
  if (MAYBE_CLASS_TYPE_P (fromtype))
    {
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
  /* Prevent add_candidates from treating a non-strictly viable candidate
     as unviable.  */
  complain |= tf_conv;

  /* It's OK to bind a temporary for converting constructor arguments, but
     not in converting the return value of a conversion operator.  */
  convflags = ((flags & LOOKUP_NO_TEMP_BIND) | LOOKUP_NO_CONVERSION
	       | (flags & LOOKUP_NO_NARROWING));
  flags &= ~LOOKUP_NO_TEMP_BIND;

  if (ctors)
    {
      int ctorflags = flags;

      first_arg = build_dummy_object (totype);

      /* We should never try to call the abstract or base constructor
	 from here.  */
      gcc_assert (!DECL_HAS_IN_CHARGE_PARM_P (OVL_FIRST (ctors))
		  && !DECL_HAS_VTT_PARM_P (OVL_FIRST (ctors)));

      args = make_tree_vector_single (expr);
      if (BRACE_ENCLOSED_INITIALIZER_P (expr))
	{
	  /* List-initialization.  */
	  add_list_candidates (ctors, first_arg, args, totype, NULL_TREE,
			       false, TYPE_BINFO (totype), TYPE_BINFO (totype),
			       ctorflags, &candidates, complain);
	}
      else
	{
	  add_candidates (ctors, first_arg, args, NULL_TREE, NULL_TREE, false,
			  TYPE_BINFO (totype), TYPE_BINFO (totype),
			  ctorflags, &candidates, complain);
	}

      for (cand = candidates; cand; cand = cand->next)
	{
	  cand->second_conv = build_identity_conv (totype, NULL_TREE);

	  /* If totype isn't a reference, and LOOKUP_ONLYCONVERTING is
	     set, then this is copy-initialization.  In that case, "The
	     result of the call is then used to direct-initialize the
	     object that is the destination of the copy-initialization."
	     [dcl.init]

	     We represent this in the conversion sequence with an
	     rvalue conversion, which means a constructor call.  */
	  if (!TYPE_REF_P (totype)
	      && cxx_dialect < cxx17
	      && (flags & LOOKUP_ONLYCONVERTING)
	      && !(convflags & LOOKUP_NO_TEMP_BIND))
	    cand->second_conv
	      = build_conv (ck_rvalue, totype, cand->second_conv);
	}
    }

  if (conv_fns)
    {
      if (BRACE_ENCLOSED_INITIALIZER_P (expr))
	first_arg = CONSTRUCTOR_ELT (expr, 0)->value;
      else
	first_arg = expr;
    }

  for (; conv_fns; conv_fns = TREE_CHAIN (conv_fns))
    {
      tree conversion_path = TREE_PURPOSE (conv_fns);
      struct z_candidate *old_candidates;

      /* If LOOKUP_NO_CONVERSION, don't consider a conversion function that
	 would need an addional user-defined conversion, i.e. if the return
	 type differs in class-ness from the desired type.  So we avoid
	 considering operator bool when calling a copy constructor.

	 This optimization avoids the failure in PR97600, and is allowed by
	 [temp.inst]/9: "If the function selected by overload resolution can be
	 determined without instantiating a class template definition, it is
	 unspecified whether that instantiation actually takes place."	*/
      tree convtype = non_reference (TREE_TYPE (conv_fns));
      if ((flags & LOOKUP_NO_CONVERSION)
	  && !WILDCARD_TYPE_P (convtype)
	  && (CLASS_TYPE_P (to_nonref)
	      != CLASS_TYPE_P (convtype)))
	continue;

      /* If we are called to convert to a reference type, we are trying to
	 find a direct binding, so don't even consider temporaries.  If
	 we don't find a direct binding, the caller will try again to
	 look for a temporary binding.  */
      if (TYPE_REF_P (totype))
	convflags |= LOOKUP_NO_TEMP_BIND;

      old_candidates = candidates;
      add_candidates (TREE_VALUE (conv_fns), first_arg, NULL, totype,
		      NULL_TREE, false,
		      conversion_path, TYPE_BINFO (fromtype),
		      flags, &candidates, complain);

      for (cand = candidates; cand != old_candidates; cand = cand->next)
	{
	  if (cand->viable == 0)
	    /* Already rejected, don't change to -1.  */
	    continue;

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
						       rettype, totype,
						       EXPR_LOCATION (expr));
	    }
	  else if (TYPE_REF_P (totype) && !ics->rvaluedness_matches_p
		   /* Limit this to non-templates for now (PR90546).  */
		   && !cand->template_decl
		   && TREE_CODE (TREE_TYPE (totype)) != FUNCTION_TYPE)
	    {
	      /* If we are called to convert to a reference type, we are trying
		 to find a direct binding per [over.match.ref], so rvaluedness
		 must match for non-functions.  */
	      cand->viable = 0;
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
						rettype, totype,
						EXPR_LOCATION (expr));
	    }
	  else if (primary_template_specialization_p (cand->fn)
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
  if (cand == NULL)
    {
      if (complain & tf_error)
	{
	  auto_diagnostic_group d;
	  error_at (cp_expr_loc_or_input_loc (expr),
		    "conversion from %qH to %qI is ambiguous",
		    fromtype, totype);
	  print_z_candidates (location_of (expr), candidates);
	}

      cand = candidates;	/* any one will do */
      cand->second_conv = build_ambiguous_conv (totype, expr);
      cand->second_conv->user_conv_p = true;
      if (!any_strictly_viable (candidates))
	cand->second_conv->bad_p = true;
      if (flags & LOOKUP_ONLYCONVERTING)
	cand->second_conv->need_temporary_p = true;
      /* If there are viable candidates, don't set ICS_BAD_FLAG; an
	 ambiguous conversion is no worse than another user-defined
	 conversion.  */

      return cand;
    }

  /* Maybe pass { } as iterators instead of an initializer_list.  */
  if (tree iters = maybe_init_list_as_range (cand->fn, expr))
    if (z_candidate *cand2
	= build_user_type_conversion_1 (totype, iters, flags, tf_none))
      if (cand2->viable == 1 && !is_list_ctor (cand2->fn))
	{
	  cand = cand2;
	  expr = iters;
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

  auto_cond_timevar tv (TV_OVERLOAD);

  conversion_obstack_sentinel cos;

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

  return ret;
}

/* Give a helpful diagnostic when implicit_conversion fails.  */

static void
implicit_conversion_error (location_t loc, tree type, tree expr)
{
  tsubst_flags_t complain = tf_warning_or_error;

  /* If expr has unknown type, then it is an overloaded function.
     Call instantiate_type to get good error messages.  */
  if (TREE_TYPE (expr) == unknown_type_node)
    instantiate_type (type, expr, complain);
  else if (invalid_nonstatic_memfn_p (loc, expr, complain))
    /* We gave an error.  */;
  else if (BRACE_ENCLOSED_INITIALIZER_P (expr)
	   && CONSTRUCTOR_IS_DESIGNATED_INIT (expr)
	   && !CP_AGGREGATE_TYPE_P (type))
    error_at (loc, "designated initializers cannot be used with a "
	      "non-aggregate type %qT", type);
  else
    {
      range_label_for_type_mismatch label (TREE_TYPE (expr), type);
      gcc_rich_location rich_loc (loc, &label);
      error_at (&rich_loc, "could not convert %qE from %qH to %qI",
		expr, TREE_TYPE (expr), type);
    }
}

/* Worker for build_converted_constant_expr.  */

static tree
build_converted_constant_expr_internal (tree type, tree expr,
					int flags, tsubst_flags_t complain)
{
  conversion *conv;
  tree t;
  location_t loc = cp_expr_loc_or_input_loc (expr);

  if (error_operand_p (expr))
    return error_mark_node;

  conversion_obstack_sentinel cos;

  conv = implicit_conversion (type, TREE_TYPE (expr), expr,
			      /*c_cast_p=*/false, flags, complain);

  /* A converted constant expression of type T is an expression, implicitly
     converted to type T, where the converted expression is a constant
     expression and the implicit conversion sequence contains only

       * user-defined conversions,
       * lvalue-to-rvalue conversions (7.1),
       * array-to-pointer conversions (7.2),
       * function-to-pointer conversions (7.3),
       * qualification conversions (7.5),
       * integral promotions (7.6),
       * integral conversions (7.8) other than narrowing conversions (11.6.4),
       * null pointer conversions (7.11) from std::nullptr_t,
       * null member pointer conversions (7.12) from std::nullptr_t, and
       * function pointer conversions (7.13),

     and where the reference binding (if any) binds directly.  */

  for (conversion *c = conv;
       c && c->kind != ck_identity;
       c = next_conversion (c))
    {
      switch (c->kind)
	{
	  /* A conversion function is OK.  If it isn't constexpr, we'll
	     complain later that the argument isn't constant.  */
	case ck_user:
	  /* List-initialization is OK.  */
	case ck_aggr:
	  /* The lvalue-to-rvalue conversion is OK.  */
	case ck_rvalue:
	  /* Array-to-pointer and function-to-pointer.  */
	case ck_lvalue:
	  /* Function pointer conversions.  */
	case ck_fnptr:
	  /* Qualification conversions.  */
	case ck_qual:
	  break;

	case ck_ref_bind:
	  if (c->need_temporary_p)
	    {
	      if (complain & tf_error)
		error_at (loc, "initializing %qH with %qI in converted "
			  "constant expression does not bind directly",
			  type, next_conversion (c)->type);
	      conv = NULL;
	    }
	  break;

	case ck_base:
	case ck_pmem:
	case ck_ptr:
	case ck_std:
	  t = next_conversion (c)->type;
	  if (INTEGRAL_OR_ENUMERATION_TYPE_P (t)
	      && INTEGRAL_OR_ENUMERATION_TYPE_P (type))
	    /* Integral promotion or conversion.  */
	    break;
	  if (NULLPTR_TYPE_P (t))
	    /* Conversion from nullptr to pointer or pointer-to-member.  */
	    break;

	  if (complain & tf_error)
	    error_at (loc, "conversion from %qH to %qI in a "
		      "converted constant expression", t, type);
	  /* fall through.  */

	default:
	  conv = NULL;
	  break;
	}
    }

  /* Avoid confusing convert_nontype_argument by introducing
     a redundant conversion to the same reference type.  */
  if (conv && conv->kind == ck_ref_bind
      && REFERENCE_REF_P (expr))
    {
      tree ref = TREE_OPERAND (expr, 0);
      if (same_type_p (type, TREE_TYPE (ref)))
	return ref;
    }

  if (conv)
    {
      /* Don't copy a class in a template.  */
      if (CLASS_TYPE_P (type) && conv->kind == ck_rvalue
	  && processing_template_decl)
	conv = next_conversion (conv);

      /* Issuing conversion warnings for value-dependent expressions is
	 likely too noisy.  */
      warning_sentinel w (warn_conversion);
      conv->check_narrowing = true;
      conv->check_narrowing_const_only = true;
      expr = convert_like (conv, expr, complain);
    }
  else
    {
      if (complain & tf_error)
	implicit_conversion_error (loc, type, expr);
      expr = error_mark_node;
    }

  return expr;
}

/* Subroutine of convert_nontype_argument.

   EXPR is an expression used in a context that requires a converted
   constant-expression, such as a template non-type parameter.  Do any
   necessary conversions (that are permitted for converted
   constant-expressions) to convert it to the desired type.

   This function doesn't consider explicit conversion functions.  If
   you mean to use "a contextually converted constant expression of type
   bool", use build_converted_constant_bool_expr.

   If conversion is successful, returns the converted expression;
   otherwise, returns error_mark_node.  */

tree
build_converted_constant_expr (tree type, tree expr, tsubst_flags_t complain)
{
  return build_converted_constant_expr_internal (type, expr, LOOKUP_IMPLICIT,
						 complain);
}

/* Used to create "a contextually converted constant expression of type
   bool".  This differs from build_converted_constant_expr in that it
   also considers explicit conversion functions.  */

tree
build_converted_constant_bool_expr (tree expr, tsubst_flags_t complain)
{
  return build_converted_constant_expr_internal (boolean_type_node, expr,
						 LOOKUP_NORMAL, complain);
}

/* Do any initial processing on the arguments to a function call.  */

vec<tree, va_gc> *
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
	    error_at (cp_expr_loc_or_input_loc (arg),
		      "invalid use of void expression");
	  return NULL;
	}
      else if (invalid_nonstatic_memfn_p (EXPR_LOCATION (arg), arg, complain))
	return NULL;

      /* Force auto deduction now.  Omit tf_warning to avoid redundant
	 deprecated warning on deprecated-14.C.  */
      if (!mark_single_function (arg, complain & ~tf_warning))
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

  auto_cond_timevar tv (TV_OVERLOAD);

  explicit_targs = NULL_TREE;
  template_only = 0;

  *candidates = NULL;
  *any_viable_p = true;

  /* Check FN.  */
  gcc_assert (OVL_P (fn) || TREE_CODE (fn) == TEMPLATE_ID_EXPR);

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

  return cand;
}

/* Print an error message about being unable to build a call to FN with
   ARGS.  ANY_VIABLE_P indicates whether any candidate functions could
   be located; CANDIDATES is a possibly empty list of such
   functions.  */

static void
print_error_for_call_failure (tree fn, const vec<tree, va_gc> *args,
			      struct z_candidate *candidates)
{
  tree targs = NULL_TREE;
  if (TREE_CODE (fn) == TEMPLATE_ID_EXPR)
    {
      targs = TREE_OPERAND (fn, 1);
      fn = TREE_OPERAND (fn, 0);
    }
  tree name = OVL_NAME (fn);
  location_t loc = location_of (name);
  if (targs)
    name = lookup_template_function (name, targs);

  auto_diagnostic_group d;
  if (!any_strictly_viable (candidates))
    error_at (loc, "no matching function for call to %<%D(%A)%>",
	      name, build_tree_list_vec (args));
  else
    error_at (loc, "call of overloaded %<%D(%A)%> is ambiguous",
	      name, build_tree_list_vec (args));
  if (candidates)
    print_z_candidates (loc, candidates);
}

/* Perform overload resolution on the set of deduction guides DGUIDES
   using ARGS.  Returns the selected deduction guide, or error_mark_node
   if overload resolution fails.  */

tree
perform_dguide_overload_resolution (tree dguides, const vec<tree, va_gc> *args,
				    tsubst_flags_t complain)
{
  z_candidate *candidates;
  bool any_viable_p;
  tree result;

  gcc_assert (deduction_guide_p (OVL_FIRST (dguides)));

  conversion_obstack_sentinel cos;

  z_candidate *cand = perform_overload_resolution (dguides, args, &candidates,
						   &any_viable_p, complain);
  if (!cand)
    {
      if (complain & tf_error)
	print_error_for_call_failure (dguides, args, candidates);
      result = error_mark_node;
    }
  else
    result = cand->fn;

  return result;
}

/* Return an expression for a call to FN (a namespace-scope function,
   or a static member function) with the ARGS.  This may change
   ARGS.  */

tree
build_new_function_call (tree fn, vec<tree, va_gc> **args,
			 tsubst_flags_t complain)
{
  struct z_candidate *candidates, *cand;
  bool any_viable_p;
  tree result;

  if (args != NULL && *args != NULL)
    {
      *args = resolve_args (*args, complain);
      if (*args == NULL)
	return error_mark_node;
    }

  if (flag_tm)
    tm_malloc_replacement (fn);

  conversion_obstack_sentinel cos;

  cand = perform_overload_resolution (fn, *args, &candidates, &any_viable_p,
				      complain);

  if (!cand)
    {
      if (complain & tf_error)
	{
	  // If there is a single (non-viable) function candidate,
	  // let the error be diagnosed by cp_build_function_call_vec.
	  if (!any_viable_p && candidates && ! candidates->next
	      && TREE_CODE (candidates->fn) == FUNCTION_DECL
	      /* A template-id callee consisting of a single (ignored)
		 non-template candidate needs to be diagnosed the
		 ordinary way.  */
	      && (TREE_CODE (fn) != TEMPLATE_ID_EXPR
		  || candidates->template_decl))
	    return cp_build_function_call_vec (candidates->fn, args, complain);

	  // Otherwise, emit notes for non-viable candidates.
	  print_error_for_call_failure (fn, *args, candidates);
	}
      result = error_mark_node;
    }
  else
    {
      result = build_over_call (cand, LOOKUP_NORMAL, complain);
    }

  if (flag_coroutines
      && result
      && TREE_CODE (result) == CALL_EXPR
      && DECL_BUILT_IN_CLASS (TREE_OPERAND (CALL_EXPR_FN (result), 0))
	  == BUILT_IN_NORMAL)
   result = coro_validate_builtin_call (result);

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
			 tree *size, tree *cookie_size,
			 tree align_arg, tree size_check,
			 tree *fn, tsubst_flags_t complain)
{
  tree original_size = *size;
  tree fns;
  struct z_candidate *candidates;
  struct z_candidate *cand = NULL;
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

  conversion_obstack_sentinel cos;

  /* Based on:

       [expr.new]

       If this lookup fails to find the name, or if the allocated type
       is not a class type, the allocation function's name is looked
       up in the global scope.

     we disregard block-scope declarations of "operator new".  */
  fns = lookup_qualified_name (global_namespace, fnname);

  if (align_arg)
    {
      vec<tree, va_gc>* align_args
	= vec_copy_and_insert (*args, align_arg, 1);
      cand = perform_overload_resolution (fns, align_args, &candidates,
					  &any_viable_p, tf_none);
      if (cand)
	*args = align_args;
      /* If no aligned allocation function matches, try again without the
	 alignment.  */
    }

  /* Figure out what function is being called.  */
  if (!cand)
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
	   if (size_check)
	     {
	       /* Set to (size_t)-1 if the size check fails.  */
	       gcc_assert (size_check != NULL_TREE);
	       *size = fold_build3 (COND_EXPR, sizetype, size_check,
				    *size, TYPE_MAX_VALUE (sizetype));
	    }
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
   tree ret = build_over_call (cand, LOOKUP_NORMAL, complain);

   /* Set this flag for all callers of this function.  In addition to
      new-expressions, this is called for allocating coroutine state; treat
      that as an implicit new-expression.  */
   tree call = extract_call_expr (ret);
   if (TREE_CODE (call) == CALL_EXPR)
     CALL_FROM_NEW_OR_DELETE_P (call) = 1;

   return ret;
}

/* Evaluate side-effects from OBJ before evaluating call
   to FN in RESULT expression.
   This is for expressions of the form `obj->fn(...)'
   where `fn' turns out to be a static member function and
   `obj' needs to be evaluated.  `fn' could be also static operator[]
   or static operator(), in which cases the source expression
   would be `obj[...]' or `obj(...)'.  */

tree
keep_unused_object_arg (tree result, tree obj, tree fn)
{
  if (result == NULL_TREE
      || result == error_mark_node
      || DECL_OBJECT_MEMBER_FUNCTION_P (fn)
      || !TREE_SIDE_EFFECTS (obj))
    return result;

  /* But avoid the implicit lvalue-rvalue conversion when `obj' is
     volatile.  */
  tree a = obj;
  if (TREE_THIS_VOLATILE (a))
    a = build_this (a);
  if (TREE_SIDE_EFFECTS (a))
    return cp_build_compound_expr (a, result, tf_error);
  return result;
}

/* Build a new call to operator().  This may change ARGS.  */

tree
build_op_call (tree obj, vec<tree, va_gc> **args, tsubst_flags_t complain)
{
  struct z_candidate *candidates = 0, *cand;
  tree fns, convs, first_mem_arg = NULL_TREE;
  bool any_viable_p;
  tree result = NULL_TREE;

  auto_cond_timevar tv (TV_OVERLOAD);

  obj = mark_lvalue_use (obj);

  if (error_operand_p (obj))
    return error_mark_node;

  tree type = TREE_TYPE (obj);

  obj = prep_operand (obj);

  if (TYPE_PTRMEMFUNC_P (type))
    {
      if (complain & tf_error)
        /* It's no good looking for an overloaded operator() on a
           pointer-to-member-function.  */
	error ("pointer-to-member function %qE cannot be called without "
	       "an object; consider using %<.*%> or %<->*%>", obj);
      return error_mark_node;
    }

  if (TYPE_BINFO (type))
    {
      fns = lookup_fnfields (TYPE_BINFO (type), call_op_identifier, 1, complain);
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

  conversion_obstack_sentinel cos;

  if (fns)
    {
      first_mem_arg = obj;

      add_candidates (BASELINK_FUNCTIONS (fns),
		      first_mem_arg, *args, NULL_TREE,
		      NULL_TREE, false,
		      BASELINK_BINFO (fns), BASELINK_ACCESS_BINFO (fns),
		      LOOKUP_NORMAL, &candidates, complain);
    }

  bool any_call_ops = candidates != nullptr;

  convs = lookup_conversions (type);

  for (; convs; convs = TREE_CHAIN (convs))
    {
      tree totype = TREE_TYPE (convs);

      if (TYPE_PTRFN_P (totype)
	  || TYPE_REFFN_P (totype)
	  || (TYPE_REF_P (totype)
	      && TYPE_PTRFN_P (TREE_TYPE (totype))))
	for (tree fn : ovl_range (TREE_VALUE (convs)))
	  {
	    if (DECL_NONCONVERTING_P (fn))
	      continue;

	    if (TREE_CODE (fn) == TEMPLATE_DECL)
	      {
		/* Making this work broke PR 71117 and 85118, so until the
		   committee resolves core issue 2189, let's disable this
		   candidate if there are any call operators.  */
		if (any_call_ops)
		  continue;

		add_template_conv_candidate
		  (&candidates, fn, obj, *args, totype,
		   /*access_path=*/NULL_TREE,
		   /*conversion_path=*/NULL_TREE, complain);
	      }
	    else
	      add_conv_candidate (&candidates, fn, obj,
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
          auto_diagnostic_group d;
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
              auto_diagnostic_group d;
              error ("call of %<(%T) (%A)%> is ambiguous", 
                     TREE_TYPE (obj), build_tree_list_vec (*args));
              print_z_candidates (location_of (TREE_TYPE (obj)), candidates);
            }
	  result = error_mark_node;
	}
      else if (TREE_CODE (cand->fn) == FUNCTION_DECL
	       && DECL_OVERLOADED_OPERATOR_P (cand->fn)
	       && DECL_OVERLOADED_OPERATOR_IS (cand->fn, CALL_EXPR))
	{
	  result = build_over_call (cand, LOOKUP_NORMAL, complain);
	  /* In an expression of the form `a()' where cand->fn
	     which is operator() turns out to be a static member function,
	     `a' is none-the-less evaluated.  */
	  result = keep_unused_object_arg (result, obj, cand->fn);
	}
      else
	{
	  if (TREE_CODE (cand->fn) == FUNCTION_DECL)
	    obj = convert_like_with_context (cand->convs[0], obj, cand->fn,
					     -1, complain);
	  else
	    {
	      gcc_checking_assert (TYPE_P (cand->fn));
	      obj = convert_like (cand->convs[0], obj, complain);
	    }
	  obj = convert_from_reference (obj);
	  result = cp_build_function_call_vec (obj, args, complain);
	}
    }

  return result;
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
op_error (const op_location_t &loc,
	  enum tree_code code, enum tree_code code2,
	  tree arg1, tree arg2, tree arg3, bool match)
{
  bool assop = code == MODIFY_EXPR;
  const char *opname = OVL_OP_INFO (assop, assop ? code2 : code)->name;

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

    case CO_AWAIT_EXPR:
      if (flag_diagnostics_show_caret)
	error_at (loc, op_error_string (G_("%<operator %s%>"), 1, match),
		  opname, TREE_TYPE (arg1));
      else
	error_at (loc, op_error_string (G_("%<operator %s%> in %<%s%E%>"),
					  1, match),
		   opname, opname, arg1, TREE_TYPE (arg1));
      break;

    default:
      if (arg2)
	if (flag_diagnostics_show_caret)
	  {
	    binary_op_rich_location richloc (loc, arg1, arg2, true);
	    error_at (&richloc,
		      op_error_string (G_("%<operator%s%>"), 2, match),
		      opname, TREE_TYPE (arg1), TREE_TYPE (arg2));
	  }
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
  if (glvalue_p (e2))
    {
      tree rtype = cp_build_reference_type (t2, !lvalue_p (e2));
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

tree
build_conditional_expr (const op_location_t &loc,
			tree arg1, tree arg2, tree arg3,
			tsubst_flags_t complain)
{
  tree arg2_type;
  tree arg3_type;
  tree result = NULL_TREE;
  tree result_type = NULL_TREE;
  tree semantic_result_type = NULL_TREE;
  bool is_glvalue = true;
  struct z_candidate *candidates = 0;
  struct z_candidate *cand;
  tree orig_arg2, orig_arg3;

  auto_cond_timevar tv (TV_OVERLOAD);

  /* As a G++ extension, the second argument to the conditional can be
     omitted.  (So that `a ? : c' is roughly equivalent to `a ? a :
     c'.)  If the second operand is omitted, make sure it is
     calculated only once.  */
  if (!arg2)
    {
      if (complain & tf_error)
	pedwarn (loc, OPT_Wpedantic,
		 "ISO C++ forbids omitting the middle term of "
		 "a %<?:%> expression");

      if ((complain & tf_warning) && !truth_value_p (TREE_CODE (arg1)))
	warn_for_omitted_condop (loc, arg1);

      /* Make sure that lvalues remain lvalues.  See g++.oliva/ext1.C.  */
      if (glvalue_p (arg1))
	{
	  arg1 = cp_stabilize_reference (arg1);
	  arg2 = arg1 = prevent_lifetime_extension (arg1);
	}
      else if (TREE_CODE (arg1) == TARGET_EXPR)
	/* arg1 can't be a prvalue result of the conditional
	   expression, since it needs to be materialized for the
	   conversion to bool, so treat it as an xvalue in arg2.  */
	arg2 = move (TARGET_EXPR_SLOT (arg1));
      else if (TREE_CODE (arg1) == EXCESS_PRECISION_EXPR)
	arg2 = arg1 = build1 (EXCESS_PRECISION_EXPR, TREE_TYPE (arg1),
			      cp_save_expr (TREE_OPERAND (arg1, 0)));
      else
	arg2 = arg1 = cp_save_expr (arg1);
    }

  /* If something has already gone wrong, just pass that fact up the
     tree.  */
  if (error_operand_p (arg1)
      || error_operand_p (arg2)
      || error_operand_p (arg3))
    return error_mark_node;

  conversion_obstack_sentinel cos;

  orig_arg2 = arg2;
  orig_arg3 = arg3;

  if (gnu_vector_type_p (TREE_TYPE (arg1))
      && VECTOR_INTEGER_TYPE_P (TREE_TYPE (arg1)))
    {
      tree arg1_type = TREE_TYPE (arg1);

      /* If arg1 is another cond_expr choosing between -1 and 0,
	 then we can use its comparison.  It may help to avoid
	 additional comparison, produce more accurate diagnostics
	 and enables folding.  */
      if (TREE_CODE (arg1) == VEC_COND_EXPR
	  && integer_minus_onep (TREE_OPERAND (arg1, 1))
	  && integer_zerop (TREE_OPERAND (arg1, 2)))
	arg1 = TREE_OPERAND (arg1, 0);

      arg1 = force_rvalue (arg1, complain);
      arg2 = force_rvalue (arg2, complain);
      arg3 = force_rvalue (arg3, complain);

      /* force_rvalue can return error_mark on valid arguments.  */
      if (error_operand_p (arg1)
	  || error_operand_p (arg2)
	  || error_operand_p (arg3))
	return error_mark_node;

      arg2_type = TREE_TYPE (arg2);
      arg3_type = TREE_TYPE (arg3);

      if (!VECTOR_TYPE_P (arg2_type)
	  && !VECTOR_TYPE_P (arg3_type))
	{
	  /* Rely on the error messages of the scalar version.  */
	  tree scal = build_conditional_expr (loc, integer_one_node,
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
			  "floating-point type of the same size as %qT", stype,
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
	  if (unsafe_conversion_p (stype, arg2, NULL_TREE, false))
	    {
	      if (complain & tf_error)
		error_at (loc, "conversion of scalar %qH to vector %qI "
			       "involves truncation", arg2_type, vtype);
	      return error_mark_node;
	    }
	  if (unsafe_conversion_p (stype, arg3, NULL_TREE, false))
	    {
	      if (complain & tf_error)
		error_at (loc, "conversion of scalar %qH to vector %qI "
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

      if ((gnu_vector_type_p (arg2_type) && !VECTOR_TYPE_P (arg3_type))
	  || (gnu_vector_type_p (arg3_type) && !VECTOR_TYPE_P (arg2_type)))
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

      if (!gnu_vector_type_p (arg2_type)
	  || !gnu_vector_type_p (arg3_type)
	  || !same_type_p (arg2_type, arg3_type)
	  || maybe_ne (TYPE_VECTOR_SUBPARTS (arg1_type),
		       TYPE_VECTOR_SUBPARTS (arg2_type))
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
	{
	  tree cmp_type = truth_type_for (arg1_type);
	  arg1 = build2 (NE_EXPR, cmp_type, arg1, build_zero_cst (arg1_type));
	}
      return build3_loc (loc, VEC_COND_EXPR, arg2_type, arg1, arg2, arg3);
    }

  /* [expr.cond]

     The first expression is implicitly converted to bool (clause
     _conv_).  */
  arg1 = perform_implicit_conversion_flags (boolean_type_node, arg1, complain,
					    LOOKUP_NORMAL);
  if (error_operand_p (arg1))
    return error_mark_node;

  arg2_type = unlowered_expr_type (arg2);
  arg3_type = unlowered_expr_type (arg3);

  if ((TREE_CODE (arg2) == EXCESS_PRECISION_EXPR
       || TREE_CODE (arg3) == EXCESS_PRECISION_EXPR)
      && (TREE_CODE (arg2_type) == INTEGER_TYPE
	  || SCALAR_FLOAT_TYPE_P (arg2_type)
	  || TREE_CODE (arg2_type) == COMPLEX_TYPE)
      && (TREE_CODE (arg3_type) == INTEGER_TYPE
	  || SCALAR_FLOAT_TYPE_P (arg3_type)
	  || TREE_CODE (arg3_type) == COMPLEX_TYPE))
    {
      semantic_result_type
	= type_after_usual_arithmetic_conversions (arg2_type, arg3_type);
      if (semantic_result_type == error_mark_node)
	{
	  tree t1 = arg2_type;
	  tree t2 = arg3_type;
	  if (TREE_CODE (t1) == COMPLEX_TYPE)
	    t1 = TREE_TYPE (t1);
	  if (TREE_CODE (t2) == COMPLEX_TYPE)
	    t2 = TREE_TYPE (t2);
	  gcc_checking_assert (SCALAR_FLOAT_TYPE_P (t1)
			       && SCALAR_FLOAT_TYPE_P (t2)
			       && (extended_float_type_p (t1)
				   || extended_float_type_p (t2))
			       && cp_compare_floating_point_conversion_ranks
				    (t1, t2) == 3);
	  if (complain & tf_error)
	    error_at (loc, "operands to %<?:%> of types %qT and %qT "
			   "have unordered conversion rank",
		      arg2_type, arg3_type);
	  return error_mark_node;
	}
      if (TREE_CODE (arg2) == EXCESS_PRECISION_EXPR)
	{
	  arg2 = TREE_OPERAND (arg2, 0);
	  arg2_type = TREE_TYPE (arg2);
	}
      if (TREE_CODE (arg3) == EXCESS_PRECISION_EXPR)
	{
	  arg3 = TREE_OPERAND (arg3, 0);
	  arg3_type = TREE_TYPE (arg3);
	}
    }

  /* [expr.cond]

     If either the second or the third operand has type (possibly
     cv-qualified) void, then the lvalue-to-rvalue (_conv.lval_),
     array-to-pointer (_conv.array_), and function-to-pointer
     (_conv.func_) standard conversions are performed on the second
     and third operands.  */
  if (VOID_TYPE_P (arg2_type) || VOID_TYPE_P (arg3_type))
    {
      /* 'void' won't help in resolving an overloaded expression on the
	 other side, so require it to resolve by itself.  */
      if (arg2_type == unknown_type_node)
	{
	  arg2 = resolve_nondeduced_context_or_error (arg2, complain);
	  arg2_type = TREE_TYPE (arg2);
	}
      if (arg3_type == unknown_type_node)
	{
	  arg3 = resolve_nondeduced_context_or_error (arg3, complain);
	  arg3_type = TREE_TYPE (arg3);
	}

      /* [expr.cond]

	 One of the following shall hold:

	 --The second or the third operand (but not both) is a
	   throw-expression (_except.throw_); the result is of the type
	   and value category of the other.

	 --Both the second and the third operands have type void; the
	   result is of type void and is a prvalue.  */
      if (TREE_CODE (arg2) == THROW_EXPR
	  && TREE_CODE (arg3) != THROW_EXPR)
	{
	  result_type = arg3_type;
	  is_glvalue = glvalue_p (arg3);
	}
      else if (TREE_CODE (arg2) != THROW_EXPR
	       && TREE_CODE (arg3) == THROW_EXPR)
	{
	  result_type = arg2_type;
	  is_glvalue = glvalue_p (arg2);
	}
      else if (VOID_TYPE_P (arg2_type) && VOID_TYPE_P (arg3_type))
	{
	  result_type = void_type_node;
	  is_glvalue = false;
	}
      else
	{
          if (complain & tf_error)
            {
              if (VOID_TYPE_P (arg2_type))
                error_at (cp_expr_loc_or_loc (arg3, loc),
			  "second operand to the conditional operator "
			  "is of type %<void%>, but the third operand is "
			  "neither a throw-expression nor of type %<void%>");
              else
                error_at (cp_expr_loc_or_loc (arg2, loc),
			  "third operand to the conditional operator "
			  "is of type %<void%>, but the second operand is "
			  "neither a throw-expression nor of type %<void%>");
            }
	  return error_mark_node;
	}

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
		    && glvalue_p (arg2) && glvalue_p (arg3)
		    && lvalue_p (arg2) == lvalue_p (arg3))))
    {
      conversion *conv2;
      conversion *conv3;
      bool converted = false;

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
	      error_at (loc, "operands to %<?:%> have different types "
			"%qT and %qT",
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
	  converted = true;
	}
      else if (conv3 && !conv3->bad_p)
	{
	  arg3 = convert_like (conv3, arg3, complain);
	  arg3 = convert_from_reference (arg3);
	  arg3_type = TREE_TYPE (arg3);
	  if (error_operand_p (arg3))
	    result = error_mark_node;
	  converted = true;
	}

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
      if (converted
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
  if (((lvalue_p (arg2) && lvalue_p (arg3))
       || (xvalue_p (arg2) && xvalue_p (arg3)))
      && same_type_p (arg2_type, arg3_type))
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
  is_glvalue = false;
  if (!same_type_p (arg2_type, arg3_type)
      && (CLASS_TYPE_P (arg2_type) || CLASS_TYPE_P (arg3_type)))
    {
      releasing_vec args;
      conversion *conv;
      bool any_viable_p;

      /* Rearrange the arguments so that add_builtin_candidate only has
	 to know about two args.  In build_builtin_candidate, the
	 arguments are unscrambled.  */
      args->quick_push (arg2);
      args->quick_push (arg3);
      args->quick_push (arg1);
      add_builtin_candidates (&candidates,
			      COND_EXPR,
			      NOP_EXPR,
			      ovl_op_identifier (false, COND_EXPR),
			      args,
			      LOOKUP_NORMAL, complain);

      /* [expr.cond]

	 If the overload resolution fails, the program is
	 ill-formed.  */
      candidates = splice_viable (candidates, false, &any_viable_p);
      if (!any_viable_p)
	{
          if (complain & tf_error)
	    error_at (loc, "operands to %<?:%> have different types %qT and %qT",
		      arg2_type, arg3_type);
	  return error_mark_node;
	}
      cand = tourney (candidates, complain);
      if (!cand)
	{
          if (complain & tf_error)
            {
              auto_diagnostic_group d;
	      op_error (loc, COND_EXPR, NOP_EXPR, arg1, arg2, arg3, false);
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
      /* A conditional expression between a floating-point
	 type and an integer type should convert the integer type to
	 the evaluation format of the floating-point type, with
	 possible excess precision.  */
      tree eptype2 = arg2_type;
      tree eptype3 = arg3_type;
      tree eptype;
      if (ANY_INTEGRAL_TYPE_P (arg2_type)
	  && (eptype = excess_precision_type (arg3_type)) != NULL_TREE)
	{
	  eptype3 = eptype;
	  if (!semantic_result_type)
	    semantic_result_type
	      = type_after_usual_arithmetic_conversions (arg2_type, arg3_type);
	}
      else if (ANY_INTEGRAL_TYPE_P (arg3_type)
	       && (eptype = excess_precision_type (arg2_type)) != NULL_TREE)
	{
	  eptype2 = eptype;
	  if (!semantic_result_type)
	    semantic_result_type
	      = type_after_usual_arithmetic_conversions (arg2_type, arg3_type);
	}
      result_type = type_after_usual_arithmetic_conversions (eptype2,
							     eptype3);
      if (result_type == error_mark_node)
	{
	  tree t1 = eptype2;
	  tree t2 = eptype3;
	  if (TREE_CODE (t1) == COMPLEX_TYPE)
	    t1 = TREE_TYPE (t1);
	  if (TREE_CODE (t2) == COMPLEX_TYPE)
	    t2 = TREE_TYPE (t2);
	  gcc_checking_assert (SCALAR_FLOAT_TYPE_P (t1)
			       && SCALAR_FLOAT_TYPE_P (t2)
			       && (extended_float_type_p (t1)
				   || extended_float_type_p (t2))
			       && cp_compare_floating_point_conversion_ranks
				    (t1, t2) == 3);
	  if (complain & tf_error)
	    error_at (loc, "operands to %<?:%> of types %qT and %qT "
			   "have unordered conversion rank",
		      eptype2, eptype3);
	  return error_mark_node;
	}
      if (semantic_result_type == error_mark_node)
	{
	  tree t1 = arg2_type;
	  tree t2 = arg3_type;
	  if (TREE_CODE (t1) == COMPLEX_TYPE)
	    t1 = TREE_TYPE (t1);
	  if (TREE_CODE (t2) == COMPLEX_TYPE)
	    t2 = TREE_TYPE (t2);
	  gcc_checking_assert (SCALAR_FLOAT_TYPE_P (t1)
			       && SCALAR_FLOAT_TYPE_P (t2)
			       && (extended_float_type_p (t1)
				   || extended_float_type_p (t2))
			       && cp_compare_floating_point_conversion_ranks
				    (t1, t2) == 3);
	  if (complain & tf_error)
	    error_at (loc, "operands to %<?:%> of types %qT and %qT "
			   "have unordered conversion rank",
		      arg2_type, arg3_type);
	  return error_mark_node;
	}

      if (complain & tf_warning)
	do_warn_double_promotion (result_type, arg2_type, arg3_type,
				  "implicit conversion from %qH to %qI to "
				  "match other result of conditional",
				  loc);

      if (TREE_CODE (arg2_type) == ENUMERAL_TYPE
	  && TREE_CODE (arg3_type) == ENUMERAL_TYPE)
        {
	  tree stripped_orig_arg2 = tree_strip_any_location_wrapper (orig_arg2);
	  tree stripped_orig_arg3 = tree_strip_any_location_wrapper (orig_arg3);
	  if (TREE_CODE (stripped_orig_arg2) == CONST_DECL
	      && TREE_CODE (stripped_orig_arg3) == CONST_DECL
	      && (DECL_CONTEXT (stripped_orig_arg2)
		  == DECL_CONTEXT (stripped_orig_arg3)))
	    /* Two enumerators from the same enumeration can have different
	       types when the enumeration is still being defined.  */;
	  else if (complain & (cxx_dialect >= cxx26
			       ? tf_warning_or_error : tf_warning))
	    emit_diagnostic (cxx_dialect >= cxx26 ? DK_PEDWARN : DK_WARNING,
			     loc, OPT_Wenum_compare, "enumerated mismatch "
			     "in conditional expression: %qT vs %qT",
			     arg2_type, arg3_type);
	  else if (cxx_dialect >= cxx26)
	    return error_mark_node;
        }
      else if ((((complain & (cxx_dialect >= cxx26
			      ? tf_warning_or_error : tf_warning))
		 && warn_deprecated_enum_float_conv)
		|| (cxx_dialect >= cxx26
		    && (complain & tf_warning_or_error) == 0))
	       && ((TREE_CODE (arg2_type) == ENUMERAL_TYPE
		    && SCALAR_FLOAT_TYPE_P (arg3_type))
		   || (SCALAR_FLOAT_TYPE_P (arg2_type)
		       && TREE_CODE (arg3_type) == ENUMERAL_TYPE)))
	{
	  if (cxx_dialect >= cxx26 && (complain & tf_warning_or_error) == 0)
	    return error_mark_node;
	  if (cxx_dialect >= cxx26 && TREE_CODE (arg2_type) == ENUMERAL_TYPE)
	    pedwarn (loc, OPT_Wdeprecated_enum_float_conversion,
		     "conditional expression between enumeration type "
		     "%qT and floating-point type %qT", arg2_type, arg3_type);
	  else if (cxx_dialect >= cxx26)
	    pedwarn (loc, OPT_Wdeprecated_enum_float_conversion,
		     "conditional expression between floating-point type "
		     "%qT and enumeration type %qT", arg2_type, arg3_type);
	  else if (TREE_CODE (arg2_type) == ENUMERAL_TYPE)
	    warning_at (loc, OPT_Wdeprecated_enum_float_conversion,
			"conditional expression between enumeration type "
			"%qT and floating-point type %qT is deprecated",
			arg2_type, arg3_type);
	  else
	    warning_at (loc, OPT_Wdeprecated_enum_float_conversion,
			"conditional expression between floating-point "
			"type %qT and enumeration type %qT is deprecated",
			arg2_type, arg3_type);
	}
      else if ((extra_warnings || warn_enum_conversion)
	       && ((TREE_CODE (arg2_type) == ENUMERAL_TYPE
		    && !same_type_p (arg3_type, type_promotes_to (arg2_type)))
		   || (TREE_CODE (arg3_type) == ENUMERAL_TYPE
		       && !same_type_p (arg2_type,
					type_promotes_to (arg3_type)))))
	{
	  if (complain & tf_warning)
	    {
	      enum opt_code opt = (warn_enum_conversion
				   ? OPT_Wenum_conversion
				   : OPT_Wextra);
	      warning_at (loc, opt, "enumerated and "
			  "non-enumerated type in conditional expression");
	    }
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
      result_type = composite_pointer_type (loc,
					    arg2_type, arg3_type, arg2,
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
	error_at (loc, "operands to %<?:%> have different types %qT and %qT",
		  arg2_type, arg3_type);
      return error_mark_node;
    }

  if (arg2 == error_mark_node || arg3 == error_mark_node)
    return error_mark_node;

 valid_operands:
  if (processing_template_decl && is_glvalue)
    {
      /* Let lvalue_kind know this was a glvalue.  */
      tree arg = (result_type == arg2_type ? arg2 : arg3);
      result_type = cp_build_reference_type (result_type, xvalue_p (arg));
    }

  result = build3_loc (loc, COND_EXPR, result_type, arg1, arg2, arg3);

  /* If the ARG2 and ARG3 are the same and don't have side-effects,
     warn here, because the COND_EXPR will be turned into ARG2.  */
  if (warn_duplicated_branches
      && (complain & tf_warning)
      && (arg2 == arg3 || operand_equal_p (arg2, arg3,
					   OEP_ADDRESS_OF_SAME_FIELD)))
    warning_at (EXPR_LOCATION (result), OPT_Wduplicated_branches,
		"this condition has identical branches");

  /* We can't use result_type below, as fold might have returned a
     throw_expr.  */

  if (!is_glvalue)
    {
      /* Expand both sides into the same slot, hopefully the target of
	 the ?: expression.  We used to check for TARGET_EXPRs here,
	 but now we sometimes wrap them in NOP_EXPRs so the test would
	 fail.  */
      if (CLASS_TYPE_P (TREE_TYPE (result)))
	{
	  result = get_target_expr (result, complain);
	  /* Tell gimplify_modify_expr_rhs not to strip this in
	     assignment context: we want both arms to initialize
	     the same temporary.  */
	  TARGET_EXPR_NO_ELIDE (result) = true;
	}
      /* If this expression is an rvalue, but might be mistaken for an
	 lvalue, we must add a NON_LVALUE_EXPR.  */
      result = rvalue (result);
      if (semantic_result_type)
	result = build1 (EXCESS_PRECISION_EXPR, semantic_result_type,
			 result);
    }
  else
    {
      result = force_paren_expr (result);
      gcc_assert (semantic_result_type == NULL_TREE);
    }

  return result;
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

/* True iff CONV represents a conversion sequence which no other can be better
   than under [over.ics.rank]: in other words, a "conversion" to the exact same
   type (including binding to a reference to the same type).  This is stronger
   than the standard's "identity" category, which also includes reference
   bindings that add cv-qualifiers or change rvalueness.  */

static bool
perfect_conversion_p (conversion *conv)
{
  if (CONVERSION_RANK (conv) != cr_identity)
    return false;
  if (conv->kind == ck_ref_bind)
    {
      if (!conv->rvaluedness_matches_p)
	return false;
      if (!same_type_p (TREE_TYPE (conv->type),
			next_conversion (conv)->type))
	return false;
    }
  if (conv->check_narrowing)
    /* Brace elision is imperfect.  */
    return false;
  return true;
}

/* True if CAND represents a perfect match, i.e. all perfect conversions, so no
   other candidate can be a better match.  Since the template/non-template
   tiebreaker comes immediately after the conversion comparison in
   [over.match.best], a perfect non-template candidate is better than all
   templates.  */

static bool
perfect_candidate_p (z_candidate *cand)
{
  if (cand->viable < 1)
    return false;
  /* CWG1402 makes an implicitly deleted move op worse than other
     candidates.  */
  if (DECL_DELETED_FN (cand->fn) && DECL_DEFAULTED_FN (cand->fn)
      && move_fn_p (cand->fn))
    return false;
  int len = cand->num_convs;
  for (int i = 0; i < len; ++i)
    if (!perfect_conversion_p (cand->convs[i]))
      return false;
  if (conversion *conv = cand->second_conv)
    if (!perfect_conversion_p (conv))
      return false;
  return true;
}

/* True iff one of CAND's argument conversions is missing.  */

static bool
missing_conversion_p (const z_candidate *cand)
{
  for (unsigned i = 0; i < cand->num_convs; ++i)
    {
      conversion *conv = cand->convs[i];
      if (!conv)
	return true;
      if (conv->kind == ck_deferred_bad)
	{
	  /* We don't know whether this conversion is outright invalid or
	     just bad, so conservatively assume it's missing.  */
	  gcc_checking_assert (conv->bad_p);
	  return true;
	}
    }
  return false;
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
  bool check_list_ctor = false;
  bool check_converting = false;
  unification_kind_t strict;
  tree ne_fns = NULL_TREE;

  if (!fns)
    return;

  /* Precalculate special handling of constructors and conversion ops.  */
  tree fn = OVL_FIRST (fns);
  if (DECL_CONV_FN_P (fn))
    {
      check_list_ctor = false;
      check_converting = (flags & LOOKUP_ONLYCONVERTING) != 0;
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
	  check_list_ctor = (flags & LOOKUP_LIST_ONLY) != 0;
	  /* For list-initialization we consider explicit constructors
	     and complain if one is chosen.  */
	  check_converting
	    = ((flags & (LOOKUP_ONLYCONVERTING|LOOKUP_LIST_INIT_CTOR))
	       == LOOKUP_ONLYCONVERTING);
	}
      strict = DEDUCE_CALL;
      ctype = conversion_path ? BINFO_TYPE (conversion_path) : NULL_TREE;
    }

  /* P2468: Check if operator== is a rewrite target with first operand
     (*args)[0]; for now just do the lookups.  */
  if ((flags & (LOOKUP_REWRITTEN | LOOKUP_REVERSED))
      && DECL_OVERLOADED_OPERATOR_IS (fn, EQ_EXPR))
    {
      tree ne_name = ovl_op_identifier (false, NE_EXPR);
      if (DECL_CLASS_SCOPE_P (fn))
	{
	  ne_fns = lookup_fnfields (TREE_TYPE ((*args)[0]), ne_name,
				    1, tf_none);
	  if (ne_fns == error_mark_node || ne_fns == NULL_TREE)
	    ne_fns = NULL_TREE;
	  else
	    ne_fns = BASELINK_FUNCTIONS (ne_fns);
	}
      else
	{
	  tree context = decl_namespace_context (fn);
	  ne_fns = lookup_qualified_name (context, ne_name, LOOK_want::NORMAL,
					  /*complain*/false);
	  if (ne_fns == error_mark_node
	      || !is_overloaded_fn (ne_fns))
	    ne_fns = NULL_TREE;
	}
    }

  if (first_arg)
    non_static_args = args;
  else
    /* Delay creating the implicit this parameter until it is needed.  */
    non_static_args = NULL;

  bool seen_strictly_viable = any_strictly_viable (*candidates);
  /* If there's a non-template perfect match, we don't need to consider
     templates.  So check non-templates first.  This optimization is only
     really needed for the defaulted copy constructor of tuple and the like
     (96926), but it seems like we might as well enable it more generally.  */
  bool seen_perfect = false;
  enum { templates, non_templates, either } which = either;
  if (template_only)
    which = templates;
  else /*if (flags & LOOKUP_DEFAULTED)*/
    which = non_templates;

  /* Template candidates that we'll potentially ignore if the
     perfect candidate optimization succeeds.  */
  z_candidate *ignored_template_cands = nullptr;

  /* During overload resolution, we first consider each function under the
     assumption that we'll eventually find a strictly viable candidate.
     This allows us to circumvent our defacto behavior when checking
     argument conversions and shortcut consideration of the candidate
     upon encountering the first bad conversion.  If this assumption
     turns out to be false, and all candidates end up being non-strictly
     viable, then we reconsider such candidates under the defacto behavior.
     This trick is important for pruning member function overloads according
     to their const/ref-qualifiers (since all 'this' conversions are at
     worst bad) without breaking -fpermissive.  */
  z_candidate *bad_cands = nullptr;
  bool shortcut_bad_convs = true;

 again:
  for (tree fn : lkp_range (fns))
    {
      if (which == templates && TREE_CODE (fn) != TEMPLATE_DECL)
	{
	  if (template_only)
	    add_ignored_candidate (candidates, fn);
	  continue;
	}
      if (which == non_templates && TREE_CODE (fn) == TEMPLATE_DECL)
	{
	  add_ignored_candidate (&ignored_template_cands, fn);
	  continue;
	}
      if ((check_converting && DECL_NONCONVERTING_P (fn))
	  || (check_list_ctor && !is_list_ctor (fn)))
	{
	  add_ignored_candidate (candidates, fn);
	  continue;
	}

      tree fn_first_arg = NULL_TREE;
      const vec<tree, va_gc> *fn_args = args;

      if (DECL_OBJECT_MEMBER_FUNCTION_P (fn))
	{
	  /* Figure out where the object arg comes from.  If this
	     function is a non-static member and we didn't get an
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

      /* Don't bother reversing an operator with two identical parameters.  */
      else if (vec_safe_length (args) == 2 && (flags & LOOKUP_REVERSED))
	{
	  tree parmlist = TYPE_ARG_TYPES (TREE_TYPE (fn));
	  if (same_type_p (TREE_VALUE (parmlist),
			   TREE_VALUE (TREE_CHAIN (parmlist))))
	    continue;
	}

      /* When considering reversed operator==, if there's a corresponding
	 operator!= in the same scope, it's not a rewrite target.  */
      if (ne_fns)
	{
	  bool found = false;
	  for (lkp_iterator ne (ne_fns); !found && ne; ++ne)
	    if (0 && !ne.using_p ()
		&& DECL_NAMESPACE_SCOPE_P (fn)
		&& DECL_CONTEXT (*ne) != DECL_CONTEXT (fn))
	      /* ??? This kludge excludes inline namespace members for the H
		 test in spaceship-eq15.C, but I don't see why we would want
		 that behavior.  Asked Core 2022-11-04.  Disabling for now.  */;
	    else if (fns_correspond (fn, *ne))
	      {
		found = true;
		break;
	      }
	  if (found)
	    continue;
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
				shortcut_bad_convs,
				complain);
      else
	{
	  add_function_candidate (candidates,
				  fn,
				  ctype,
				  fn_first_arg,
				  fn_args,
				  access_path,
				  conversion_path,
				  flags,
				  NULL,
				  shortcut_bad_convs,
				  complain);
	  if (perfect_candidate_p (*candidates))
	    seen_perfect = true;
	}

      z_candidate *cand = *candidates;
      if (cand->viable == 1)
	seen_strictly_viable = true;

      if (cand->viable == -1
	  && shortcut_bad_convs
	  && missing_conversion_p (cand))
	{
	  /* This candidate has been tentatively marked non-strictly viable,
	     and we didn't compute all argument conversions for it (having
	     stopped at the first bad conversion).  Move it to BAD_CANDS to
	     to fully reconsider later if we don't find any strictly viable
	     candidates.  */
	  if (complain & (tf_error | tf_conv))
	    {
	      *candidates = cand->next;
	      cand->next = bad_cands;
	      bad_cands = cand;
	    }
	  else
	    /* But if we're in a SFINAE context, just mark this candidate as
	       unviable outright and avoid potentially reconsidering it.
	       This is safe to do because in a SFINAE context, performing a bad
	       conversion is always an error (even with -fpermissive), so a
	       non-strictly viable candidate is effectively unviable anyway.  */
	    cand->viable = 0;
	}
    }
  if (which == non_templates && !seen_perfect)
    {
      which = templates;
      ignored_template_cands = nullptr;
      goto again;
    }
  else if (which == templates
	   && !seen_strictly_viable
	   && shortcut_bad_convs
	   && bad_cands)
    {
      /* None of the candidates are strictly viable, so consider again those
	 functions in BAD_CANDS, this time without shortcutting bad conversions
	 so that all their argument conversions are computed.  */
      which = either;
      fns = NULL_TREE;
      for (z_candidate *cand = bad_cands; cand; cand = cand->next)
	{
	  tree fn = cand->fn;
	  if (tree ti = cand->template_decl)
	    fn = TI_TEMPLATE (ti);
	  fns = ovl_make (fn, fns);
	}
      shortcut_bad_convs = false;
      bad_cands = nullptr;
      goto again;
    }

  if (complain & tf_error)
    {
      /* Remember any omitted candidates; we may want to print all candidates
	 as part of overload resolution failure diagnostics.  */
      for (z_candidate *omitted_cands : { ignored_template_cands, bad_cands })
	{
	  z_candidate **omitted_cands_tail = &omitted_cands;
	  while (*omitted_cands_tail)
	    omitted_cands_tail = &(*omitted_cands_tail)->next;
	  *omitted_cands_tail = *candidates;
	  *candidates = omitted_cands;
	}
    }
}

/* Returns 1 if P0145R2 says that the LHS of operator CODE is evaluated first,
   -1 if the RHS is evaluated first, or 0 if the order is unspecified.  */

static int
op_is_ordered (tree_code code)
{
  switch (code)
    {
      // 5. b @= a
    case MODIFY_EXPR:
      return (flag_strong_eval_order > 1 ? -1 : 0);

      // 6. a[b]
    case ARRAY_REF:
      return (flag_strong_eval_order > 1 ? 1 : 0);

      // 1. a.b
      // Not overloadable (yet).
      // 2. a->b
      // Only one argument.
      // 3. a->*b
    case MEMBER_REF:
      // 7. a << b
    case LSHIFT_EXPR:
      // 8. a >> b
    case RSHIFT_EXPR:
      // a && b
      // Predates P0145R3.
    case TRUTH_ANDIF_EXPR:
      // a || b
      // Predates P0145R3.
    case TRUTH_ORIF_EXPR:
      // a , b
      // Predates P0145R3.
    case COMPOUND_EXPR:
      return (flag_strong_eval_order ? 1 : 0);

    default:
      return 0;
    }
}

/* Subroutine of build_new_op: Add to CANDIDATES all candidates for the
   operator indicated by CODE/CODE2.  This function calls itself recursively to
   handle C++20 rewritten comparison operator candidates.  Returns NULL_TREE
   upon success, and error_mark_node if something went wrong that prevented
   us from performing overload resolution (e.g. ambiguous member name lookup).

   LOOKUPS, if non-NULL, is the set of pertinent namespace-scope operator
   overloads to consider.  This parameter is used when instantiating a
   dependent operator expression and has the same structure as
   DEPENDENT_OPERATOR_TYPE_SAVED_LOOKUPS.  */

static tree
add_operator_candidates (z_candidate **candidates,
			 tree_code code, tree_code code2,
			 vec<tree, va_gc> *arglist, tree lookups,
			 int flags, tsubst_flags_t complain)
{
  z_candidate *start_candidates = *candidates;
  bool ismodop = code2 != ERROR_MARK;
  tree fnname = ovl_op_identifier (ismodop, ismodop ? code2 : code);

  /* LOOKUP_REWRITTEN is set when we're looking for the == or <=> operator to
     rewrite from, and also when we're looking for the e.g. < operator to use
     on the result of <=>.  In the latter case, we don't want the flag set in
     the candidate, we just want to suppress looking for rewrites.  */
  bool rewritten = (flags & LOOKUP_REWRITTEN);
  if (rewritten && code != EQ_EXPR && code != SPACESHIP_EXPR)
    flags &= ~LOOKUP_REWRITTEN;

  bool memonly = false;
  switch (code)
    {
      /* =, ->, [], () must be non-static member functions.  */
    case MODIFY_EXPR:
      if (code2 != NOP_EXPR)
	break;
      /* FALLTHRU */
    case COMPONENT_REF:
    case ARRAY_REF:
      memonly = true;
      break;

    default:
      break;
    }

  /* Add namespace-scope operators to the list of functions to
     consider.  */
  if (!memonly)
    {
      tree fns;
      if (!lookups)
	fns = lookup_name (fnname, LOOK_where::BLOCK_NAMESPACE);
      /* If LOOKUPS is non-NULL, then we're instantiating a dependent operator
	 expression, and LOOKUPS is the result of stage 1 name lookup.  */
      else if (tree found = purpose_member (fnname, lookups))
	fns = TREE_VALUE (found);
      else
	fns = NULL_TREE;
      fns = lookup_arg_dependent (fnname, fns, arglist);
      add_candidates (fns, NULL_TREE, arglist, NULL_TREE,
		      NULL_TREE, false, NULL_TREE, NULL_TREE,
		      flags, candidates, complain);
    }

  /* Add class-member operators to the candidate set.  */
  tree arg1_type = TREE_TYPE ((*arglist)[0]);
  unsigned nargs = arglist->length () > 1 ? 2 : 1;
  tree arg2_type = nargs > 1 ? TREE_TYPE ((*arglist)[1]) : NULL_TREE;
  if (CLASS_TYPE_P (arg1_type))
    {
      tree fns = lookup_fnfields (arg1_type, fnname, 1, complain);
      if (fns == error_mark_node)
	return error_mark_node;
      if (fns)
	{
	  if (code == ARRAY_REF)
	    {
	      vec<tree,va_gc> *restlist = make_tree_vector ();
	      for (unsigned i = 1; i < nargs; ++i)
		vec_safe_push (restlist, (*arglist)[i]);
	      z_candidate *save_cand = *candidates;
	      add_candidates (BASELINK_FUNCTIONS (fns),
			      (*arglist)[0], restlist, NULL_TREE,
			      NULL_TREE, false,
			      BASELINK_BINFO (fns),
			      BASELINK_ACCESS_BINFO (fns),
			      flags, candidates, complain);
	      /* Release the vec if we didn't add a candidate that uses it.  */
	      for (z_candidate *c = *candidates; c != save_cand; c = c->next)
		if (c->args == restlist)
		  {
		    restlist = NULL;
		    break;
		  }
	      release_tree_vector (restlist);
	    }
	  else
	    add_candidates (BASELINK_FUNCTIONS (fns),
			    NULL_TREE, arglist, NULL_TREE,
			    NULL_TREE, false,
			    BASELINK_BINFO (fns),
			    BASELINK_ACCESS_BINFO (fns),
			    flags, candidates, complain);
	}
    }
  /* Per [over.match.oper]3.2, if no operand has a class type, then
     only non-member functions that have type T1 or reference to
     cv-qualified-opt T1 for the first argument, if the first argument
     has an enumeration type, or T2 or reference to cv-qualified-opt
     T2 for the second argument, if the second argument has an
     enumeration type.  Filter out those that don't match.  */
  else if (! arg2_type || ! CLASS_TYPE_P (arg2_type))
    {
      struct z_candidate **candp, **next;

      for (candp = candidates; *candp != start_candidates; candp = next)
	{
	  unsigned i;
	  z_candidate *cand = *candp;
	  next = &cand->next;

	  tree parmlist = TYPE_ARG_TYPES (TREE_TYPE (cand->fn));

	  for (i = 0; i < nargs; ++i)
	    {
	      tree parmtype = TREE_VALUE (parmlist);
	      tree argtype = unlowered_expr_type ((*arglist)[i]);

	      if (TYPE_REF_P (parmtype))
		parmtype = TREE_TYPE (parmtype);
	      if (TREE_CODE (argtype) == ENUMERAL_TYPE
		  && (same_type_ignoring_top_level_qualifiers_p
		      (argtype, parmtype)))
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

  if (!rewritten)
    {
      /* The standard says to rewrite built-in candidates, too,
	 but there's no point.  */
      add_builtin_candidates (candidates, code, code2, fnname, arglist,
			      flags, complain);

      /* Maybe add C++20 rewritten comparison candidates.  */
      tree_code rewrite_code = ERROR_MARK;
      if (cxx_dialect >= cxx20
	  && nargs == 2
	  && (OVERLOAD_TYPE_P (arg1_type) || OVERLOAD_TYPE_P (arg2_type)))
	switch (code)
	  {
	  case LT_EXPR:
	  case LE_EXPR:
	  case GT_EXPR:
	  case GE_EXPR:
	  case SPACESHIP_EXPR:
	    rewrite_code = SPACESHIP_EXPR;
	    break;

	  case NE_EXPR:
	  case EQ_EXPR:
	    rewrite_code = EQ_EXPR;
	    break;

	  default:;
	  }

      if (rewrite_code)
	{
	  tree r;
	  flags |= LOOKUP_REWRITTEN;
	  if (rewrite_code != code)
	    {
	      /* Add rewritten candidates in same order.  */
	      r = add_operator_candidates (candidates, rewrite_code, ERROR_MARK,
					   arglist, lookups, flags, complain);
	      if (r == error_mark_node)
		return error_mark_node;
	    }

	  z_candidate *save_cand = *candidates;

	  /* Add rewritten candidates in reverse order.  */
	  flags |= LOOKUP_REVERSED;
	  vec<tree,va_gc> *revlist = make_tree_vector ();
	  revlist->quick_push ((*arglist)[1]);
	  revlist->quick_push ((*arglist)[0]);
	  r = add_operator_candidates (candidates, rewrite_code, ERROR_MARK,
				       revlist, lookups, flags, complain);
	  if (r == error_mark_node)
	    return error_mark_node;

	  /* Release the vec if we didn't add a candidate that uses it.  */
	  for (z_candidate *c = *candidates; c != save_cand; c = c->next)
	    if (c->args == revlist)
	      {
		revlist = NULL;
		break;
	      }
	  release_tree_vector (revlist);
	}
    }

  return NULL_TREE;
}

tree
build_new_op (const op_location_t &loc, enum tree_code code, int flags,
	      tree arg1, tree arg2, tree arg3, tree lookups,
	      tree *overload, tsubst_flags_t complain)
{
  struct z_candidate *candidates = 0, *cand;
  releasing_vec arglist;
  tree result = NULL_TREE;
  bool result_valid_p = false;
  enum tree_code code2 = ERROR_MARK;
  enum tree_code code_orig_arg1 = ERROR_MARK;
  enum tree_code code_orig_arg2 = ERROR_MARK;
  bool strict_p;
  bool any_viable_p;

  auto_cond_timevar tv (TV_OVERLOAD);

  if (error_operand_p (arg1)
      || error_operand_p (arg2)
      || error_operand_p (arg3))
    return error_mark_node;

  conversion_obstack_sentinel cos;

  bool ismodop = code == MODIFY_EXPR;
  if (ismodop)
    {
      code2 = TREE_CODE (arg3);
      arg3 = NULL_TREE;
    }

  tree arg1_type = unlowered_expr_type (arg1);
  tree arg2_type = arg2 ? unlowered_expr_type (arg2) : NULL_TREE;

  arg1 = prep_operand (arg1);

  switch (code)
    {
    case NEW_EXPR:
    case VEC_NEW_EXPR:
    case VEC_DELETE_EXPR:
    case DELETE_EXPR:
      /* Use build_operator_new_call and build_op_delete_call instead.  */
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
      break;
    case GT_EXPR:
    case LT_EXPR:
    case GE_EXPR:
    case LE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
      /* These are saved for the sake of maybe_warn_bool_compare.  */
      code_orig_arg1 = TREE_CODE (arg1_type);
      code_orig_arg2 = TREE_CODE (arg2_type);
      break;

    default:
      break;
    }

  arg2 = prep_operand (arg2);
  arg3 = prep_operand (arg3);

  if (code == COND_EXPR)
    /* Use build_conditional_expr instead.  */
    gcc_unreachable ();
  else if (! OVERLOAD_TYPE_P (arg1_type)
	   && (! arg2 || ! OVERLOAD_TYPE_P (arg2_type)))
    goto builtin;

  if (code == POSTINCREMENT_EXPR || code == POSTDECREMENT_EXPR)
    {
      arg2 = integer_zero_node;
      arg2_type = integer_type_node;
    }

  arglist->quick_push (arg1);
  if (arg2 != NULL_TREE)
    arglist->quick_push (arg2);
  if (arg3 != NULL_TREE)
    arglist->quick_push (arg3);

  result = add_operator_candidates (&candidates, code, code2, arglist,
				    lookups, flags, complain);
  if (result == error_mark_node)
    return error_mark_node;

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
	      tree fnname = ovl_op_identifier (ismodop, ismodop ? code2 : code);
	      const char *msg = (flag_permissive) 
		? G_("no %<%D(int)%> declared for postfix %qs,"
		     " trying prefix operator instead")
		: G_("no %<%D(int)%> declared for postfix %qs");
	      permerror (loc, msg, fnname, OVL_OP_INFO (false, code)->name);
	    }

	  if (!flag_permissive)
	    return error_mark_node;

	  if (code == POSTINCREMENT_EXPR)
	    code = PREINCREMENT_EXPR;
	  else
	    code = PREDECREMENT_EXPR;
	  result = build_new_op (loc, code, flags, arg1, NULL_TREE,
				 NULL_TREE, lookups, overload, complain);
	  break;

	  /* The caller will deal with these.  */
	case ADDR_EXPR:
	case COMPOUND_EXPR:
	case COMPONENT_REF:
	case CO_AWAIT_EXPR:
	  result = NULL_TREE;
	  result_valid_p = true;
	  break;

	default:
	  if (complain & tf_error)
	    {
		/* If one of the arguments of the operator represents
		   an invalid use of member function pointer, try to report
		   a meaningful error ...  */
	      if (invalid_nonstatic_memfn_p (loc, arg1, tf_error)
		    || invalid_nonstatic_memfn_p (loc, arg2, tf_error)
		    || invalid_nonstatic_memfn_p (loc, arg3, tf_error))
		  /* We displayed the error message.  */;
		else
		  {
		    /* ... Otherwise, report the more generic
		       "no matching operator found" error */
		    auto_diagnostic_group d;
		    op_error (loc, code, code2, arg1, arg2, arg3, false);
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
	      auto_diagnostic_group d;
	      op_error (loc, code, code2, arg1, arg2, arg3, true);
	      print_z_candidates (loc, candidates);
	    }
	  result = error_mark_node;
	  if (overload)
	    *overload = error_mark_node;
	}
      else if (TREE_CODE (cand->fn) == FUNCTION_DECL)
	{
	  if (overload)
	    *overload = cand->fn;

	  if (resolve_args (arglist, complain) == NULL)
	    result = error_mark_node;
	  else
	    {
	      tsubst_flags_t ocomplain = complain;
	      if (cand->rewritten ())
		/* We'll wrap this call in another one.  */
		ocomplain &= ~tf_decltype;
	      if (cand->reversed ())
		{
		  /* We swapped these in add_candidate, swap them back now.  */
		  std::swap (cand->convs[0], cand->convs[1]);
		  if (cand->fn == current_function_decl)
		    warning_at (loc, 0, "in C++20 this comparison calls the "
				"current function recursively with reversed "
				"arguments");
		}
	      result = build_over_call (cand, LOOKUP_NORMAL, ocomplain);
	    }

	  if (trivial_fn_p (cand->fn) || DECL_IMMEDIATE_FUNCTION_P (cand->fn))
	    /* There won't be a CALL_EXPR.  */;
	  else if (result && result != error_mark_node)
	    {
	      tree call = extract_call_expr (result);
	      CALL_EXPR_OPERATOR_SYNTAX (call) = true;

	      /* Specify evaluation order as per P0145R2.  */
	      CALL_EXPR_ORDERED_ARGS (call) = false;
	      switch (op_is_ordered (code))
		{
		case -1:
		  CALL_EXPR_REVERSE_ARGS (call) = true;
		  break;

		case 1:
		  CALL_EXPR_ORDERED_ARGS (call) = true;
		  break;

		default:
		  break;
		}
	    }

	  /* If this was a C++20 rewritten comparison, adjust the result.  */
	  if (cand->rewritten ())
	    {
	      /* FIXME build_min_non_dep_op_overload can't handle rewrites.  */
	      if (overload)
		*overload = NULL_TREE;
	      switch (code)
		{
		case EQ_EXPR:
		  gcc_checking_assert (cand->reversed ());
		  gcc_fallthrough ();
		case NE_EXPR:
		  if (result == error_mark_node)
		    ;
		  /* If a rewritten operator== candidate is selected by
		     overload resolution for an operator @, its return type
		     shall be cv bool.... */
		  else if (TREE_CODE (TREE_TYPE (result)) != BOOLEAN_TYPE)
		    {
		      if (complain & tf_error)
			{
			  auto_diagnostic_group d;
			  error_at (loc, "return type of %qD is not %qs",
				    cand->fn, "bool");
			  inform (loc, "used as rewritten candidate for "
				  "comparison of %qT and %qT",
				  arg1_type, arg2_type);
			}
		      result = error_mark_node;
		    }
		  else if (code == NE_EXPR)
		    /* !(y == x) or !(x == y)  */
		    result = build1_loc (loc, TRUTH_NOT_EXPR,
					 boolean_type_node, result);
		  break;

		  /* If a rewritten operator<=> candidate is selected by
		     overload resolution for an operator @, x @ y is
		     interpreted as 0 @ (y <=> x) if the selected candidate is
		     a synthesized candidate with reversed order of parameters,
		     or (x <=> y) @ 0 otherwise, using the selected rewritten
		     operator<=> candidate.  */
		case SPACESHIP_EXPR:
		  if (!cand->reversed ())
		    /* We're in the build_new_op call below for an outer
		       reversed call; we don't need to do anything more.  */
		    break;
		  gcc_fallthrough ();
		case LT_EXPR:
		case LE_EXPR:
		case GT_EXPR:
		case GE_EXPR:
		  {
		    tree lhs = result;
		    tree rhs = integer_zero_node;
		    if (cand->reversed ())
		      std::swap (lhs, rhs);
		    warning_sentinel ws (warn_zero_as_null_pointer_constant);
		    result = build_new_op (loc, code,
					   LOOKUP_NORMAL|LOOKUP_REWRITTEN,
					   lhs, rhs, NULL_TREE, lookups,
					   NULL, complain);
		  }
		  break;

		default:
		  gcc_unreachable ();
		}
	    }

	  /* In an expression of the form `a[]' where cand->fn
	     which is operator[] turns out to be a static member function,
	     `a' is none-the-less evaluated.  */
	  if (code == ARRAY_REF)
	    result = keep_unused_object_arg (result, arg1, cand->fn);
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
	      if (TREE_CODE (arg1_type) == ENUMERAL_TYPE
		  && TREE_CODE (arg2_type) == ENUMERAL_TYPE
		  && (TYPE_MAIN_VARIANT (arg1_type)
		      != TYPE_MAIN_VARIANT (arg2_type)))
		{
		  if (cxx_dialect >= cxx26
		      && (complain & tf_warning_or_error) == 0)
		    result = error_mark_node;
		  else if (cxx_dialect >= cxx26 || (complain & tf_warning))
		    emit_diagnostic (cxx_dialect >= cxx26
				     ? DK_PEDWARN : DK_WARNING,
				     loc, OPT_Wenum_compare,
				     "comparison between %q#T and %q#T",
				     arg1_type, arg2_type);
		}
	      break;
	    default:
	      break;
	    }

	  /* "If a built-in candidate is selected by overload resolution, the
	     operands of class type are converted to the types of the
	     corresponding parameters of the selected operation function,
	     except that the second standard conversion sequence of a
	     user-defined conversion sequence (12.3.3.1.2) is not applied."  */
	  conversion *conv = cand->convs[0];
	  if (conv->user_conv_p)
	    {
	      conv = strip_standard_conversion (conv);
	      arg1 = convert_like (conv, arg1, complain);
	    }

	  if (arg2)
	    {
	      conv = cand->convs[1];
	      if (conv->user_conv_p)
		{
		  conv = strip_standard_conversion (conv);
		  arg2 = convert_like (conv, arg2, complain);
		}
	    }

	  if (arg3)
	    {
	      conv = cand->convs[2];
	      if (conv->user_conv_p)
		{
		  conv = strip_standard_conversion (conv);
		  arg3 = convert_like (conv, arg3, complain);
		}
	    }
	}
    }

  if (result || result_valid_p)
    return result;

 builtin:
  switch (code)
    {
    case MODIFY_EXPR:
      return cp_build_modify_expr (loc, arg1, code2, arg2, complain);

    case INDIRECT_REF:
      return cp_build_indirect_ref (loc, arg1, RO_UNARY_STAR, complain);

    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
      if ((complain & tf_warning) && !processing_template_decl)
	warn_logical_operator (loc, code, boolean_type_node,
			       code_orig_arg1, arg1,
			       code_orig_arg2, arg2);
      /* Fall through.  */
    case GT_EXPR:
    case LT_EXPR:
    case GE_EXPR:
    case LE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
      if ((complain & tf_warning)
	  && ((code_orig_arg1 == BOOLEAN_TYPE)
	      ^ (code_orig_arg2 == BOOLEAN_TYPE)))
	maybe_warn_bool_compare (loc, code, arg1, arg2);
      if (complain & tf_warning && warn_tautological_compare)
	warn_tautological_cmp (loc, code, arg1, arg2);
      /* Fall through.  */
    case SPACESHIP_EXPR:
    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
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
    case CO_AWAIT_EXPR:
      return cp_build_unary_op (code, arg1, false, complain);

    case ARRAY_REF:
      return cp_build_array_ref (input_location, arg1, arg2, complain);

    case MEMBER_REF:
      return build_m_component_ref (cp_build_indirect_ref (loc, arg1,
							   RO_ARROW_STAR,
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

/* Build a new call to operator[].  This may change ARGS.  */

tree
build_op_subscript (const op_location_t &loc, tree obj,
		    vec<tree, va_gc> **args, tree *overload,
		    tsubst_flags_t complain)
{
  struct z_candidate *candidates = 0, *cand;
  tree fns, first_mem_arg = NULL_TREE;
  bool any_viable_p;
  tree result = NULL_TREE;

  auto_cond_timevar tv (TV_OVERLOAD);

  obj = mark_lvalue_use (obj);

  if (error_operand_p (obj))
    return error_mark_node;

  tree type = TREE_TYPE (obj);

  obj = prep_operand (obj);

  if (TYPE_BINFO (type))
    {
      fns = lookup_fnfields (TYPE_BINFO (type), ovl_op_identifier (ARRAY_REF),
			     1, complain);
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

  conversion_obstack_sentinel cos;

  if (fns)
    {
      first_mem_arg = obj;

      add_candidates (BASELINK_FUNCTIONS (fns),
		      first_mem_arg, *args, NULL_TREE,
		      NULL_TREE, false,
		      BASELINK_BINFO (fns), BASELINK_ACCESS_BINFO (fns),
		      LOOKUP_NORMAL, &candidates, complain);
    }

  /* Be strict here because if we choose a bad conversion candidate, the
     errors we get won't mention the call context.  */
  candidates = splice_viable (candidates, true, &any_viable_p);
  if (!any_viable_p)
    {
      if (complain & tf_error)
	{
	  auto_diagnostic_group d;
	  error ("no match for call to %<%T::operator[] (%A)%>",
		 TREE_TYPE (obj), build_tree_list_vec (*args));
	  print_z_candidates (loc, candidates);
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
	      auto_diagnostic_group d;
	      error ("call of %<%T::operator[] (%A)%> is ambiguous",
		     TREE_TYPE (obj), build_tree_list_vec (*args));
	      print_z_candidates (loc, candidates);
	    }
	  result = error_mark_node;
	}
      else if (TREE_CODE (cand->fn) == FUNCTION_DECL
	       && DECL_OVERLOADED_OPERATOR_P (cand->fn)
	       && DECL_OVERLOADED_OPERATOR_IS (cand->fn, ARRAY_REF))
	{
	  if (overload)
	    *overload = cand->fn;
	  result = build_over_call (cand, LOOKUP_NORMAL, complain);
	  if (trivial_fn_p (cand->fn) || DECL_IMMEDIATE_FUNCTION_P (cand->fn))
	    /* There won't be a CALL_EXPR.  */;
	  else if (result && result != error_mark_node)
	    {
	      tree call = extract_call_expr (result);
	      CALL_EXPR_OPERATOR_SYNTAX (call) = true;

	      /* Specify evaluation order as per P0145R2.  */
	      CALL_EXPR_ORDERED_ARGS (call) = op_is_ordered (ARRAY_REF) == 1;
	    }

	  /* In an expression of the form `a[]' where cand->fn
	     which is operator[] turns out to be a static member function,
	     `a' is none-the-less evaluated.  */
	  result = keep_unused_object_arg (result, obj, cand->fn);
	}
      else
	gcc_unreachable ();
    }

  return result;
}

/* CALL was returned by some call-building function; extract the actual
   CALL_EXPR from any bits that have been tacked on, e.g. by
   convert_from_reference.  */

tree
extract_call_expr (tree call)
{
  while (TREE_CODE (call) == COMPOUND_EXPR)
    call = TREE_OPERAND (call, 1);
  if (REFERENCE_REF_P (call))
    call = TREE_OPERAND (call, 0);
  if (TREE_CODE (call) == TARGET_EXPR)
    call = TARGET_EXPR_INITIAL (call);
  if (cxx_dialect >= cxx20)
    switch (TREE_CODE (call))
      {
	/* C++20 rewritten comparison operators.  */
      case TRUTH_NOT_EXPR:
	call = TREE_OPERAND (call, 0);
	break;
      case LT_EXPR:
      case LE_EXPR:
      case GT_EXPR:
      case GE_EXPR:
      case SPACESHIP_EXPR:
	{
	  tree op0 = TREE_OPERAND (call, 0);
	  if (integer_zerop (op0))
	    call = TREE_OPERAND (call, 1);
	  else
	    call = op0;
	}
	break;
      default:;
      }

  if (TREE_CODE (call) != CALL_EXPR
      && TREE_CODE (call) != AGGR_INIT_EXPR
      && call != error_mark_node)
    return NULL_TREE;
  return call;
}

/* Returns true if FN has two parameters, of which the second has type
   size_t.  */

static bool
second_parm_is_size_t (tree fn)
{
  tree t = FUNCTION_ARG_CHAIN (fn);
  if (!t || !same_type_p (TREE_VALUE (t), size_type_node))
    return false;
  t = TREE_CHAIN (t);
  if (t == void_list_node)
    return true;
  return false;
}

/* True if T, an allocation function, has std::align_val_t as its second
   argument.  */

bool
aligned_allocation_fn_p (tree t)
{
  if (!aligned_new_threshold)
    return false;

  tree a = FUNCTION_ARG_CHAIN (t);
  return (a && same_type_p (TREE_VALUE (a), align_type_node));
}

/* True if T is std::destroying_delete_t.  */

static bool
std_destroying_delete_t_p (tree t)
{
  return (TYPE_CONTEXT (t) == std_node
	  && id_equal (TYPE_IDENTIFIER (t), "destroying_delete_t"));
}

/* A deallocation function with at least two parameters whose second parameter
   type is of type std::destroying_delete_t is a destroying operator delete. A
   destroying operator delete shall be a class member function named operator
   delete. [ Note: Array deletion cannot use a destroying operator
   delete. --end note ] */

tree
destroying_delete_p (tree t)
{
  tree a = TYPE_ARG_TYPES (TREE_TYPE (t));
  if (!a || !TREE_CHAIN (a))
    return NULL_TREE;
  tree type = TREE_VALUE (TREE_CHAIN (a));
  return std_destroying_delete_t_p (type) ? type : NULL_TREE;
}

struct dealloc_info
{
  bool sized;
  bool aligned;
  tree destroying;
};

/* Returns true iff T, an element of an OVERLOAD chain, is a usual deallocation
   function (3.7.4.2 [basic.stc.dynamic.deallocation]).  If so, and DI is
   non-null, also set *DI. */

static bool
usual_deallocation_fn_p (tree t, dealloc_info *di)
{
  if (di) *di = dealloc_info();

  /* A template instance is never a usual deallocation function,
     regardless of its signature.  */
  if (TREE_CODE (t) == TEMPLATE_DECL
      || primary_template_specialization_p (t))
    return false;

  /* A usual deallocation function is a deallocation function whose parameters
     after the first are
     - optionally, a parameter of type std::destroying_delete_t, then
     - optionally, a parameter of type std::size_t, then
     - optionally, a parameter of type std::align_val_t.  */
  bool global = DECL_NAMESPACE_SCOPE_P (t);
  tree chain = FUNCTION_ARG_CHAIN (t);
  if (chain && destroying_delete_p (t))
    {
      if (di) di->destroying = TREE_VALUE (chain);
      chain = TREE_CHAIN (chain);
    }
  if (chain
      && (!global || flag_sized_deallocation)
      && same_type_p (TREE_VALUE (chain), size_type_node))
    {
      if (di) di->sized = true;
      chain = TREE_CHAIN (chain);
    }
  if (chain && aligned_new_threshold
      && same_type_p (TREE_VALUE (chain), align_type_node))
    {
      if (di) di->aligned = true;
      chain = TREE_CHAIN (chain);
    }
  return (chain == void_list_node);
}

/* Just return whether FN is a usual deallocation function.  */

bool
usual_deallocation_fn_p (tree fn)
{
  return usual_deallocation_fn_p (fn, NULL);
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
  dealloc_info di_fn = { };

  if (addr == error_mark_node)
    return error_mark_node;

  type = strip_array_types (TREE_TYPE (TREE_TYPE (addr)));

  fnname = ovl_op_identifier (false, code);

  if (CLASS_TYPE_P (type)
      && COMPLETE_TYPE_P (complete_type (type))
      && !global_p)
    /* In [class.free]

       If the result of the lookup is ambiguous or inaccessible, or if
       the lookup selects a placement deallocation function, the
       program is ill-formed.

       Therefore, we ask lookup_fnfields to complain about ambiguity.  */
    {
      fns = lookup_fnfields (TYPE_BINFO (type), fnname, 1, complain);
      if (fns == error_mark_node)
	return error_mark_node;
    }
  else
    fns = NULL_TREE;

  if (fns == NULL_TREE)
    fns = lookup_name (fnname, LOOK_where::BLOCK_NAMESPACE);

  /* Strip const and volatile from addr.  */
  tree oaddr = addr;
  addr = cp_convert (ptr_type_node, addr, complain);

  tree excluded_destroying = NULL_TREE;

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

      fn = MAYBE_BASELINK_FUNCTIONS (fn);

      /* "If the lookup finds the two-parameter form of a usual deallocation
	 function (3.7.4.2) and that function, considered as a placement
	 deallocation function, would have been selected as a match for the
	 allocation function, the program is ill-formed."  */
      if (second_parm_is_size_t (fn))
	{
	  const char *const msg1
	    = G_("exception cleanup for this placement new selects "
		 "non-placement %<operator delete%>");
	  const char *const msg2
	    = G_("%qD is a usual (non-placement) deallocation "
		 "function in C++14 (or with %<-fsized-deallocation%>)");

	  /* But if the class has an operator delete (void *), then that is
	     the usual deallocation function, so we shouldn't complain
	     about using the operator delete (void *, size_t).  */
	  if (DECL_CLASS_SCOPE_P (fn))
	    for (tree elt : lkp_range (MAYBE_BASELINK_FUNCTIONS (fns)))
	      {
		if (usual_deallocation_fn_p (elt)
		    && FUNCTION_ARG_CHAIN (elt) == void_list_node)
		  goto ok;
	      }
	  /* Before C++14 a two-parameter global deallocation function is
	     always a placement deallocation function, but warn if
	     -Wc++14-compat.  */
	  else if (!flag_sized_deallocation)
	    {
	      if (complain & tf_warning)
		{
		  auto_diagnostic_group d;
		  if (warning (OPT_Wc__14_compat, msg1))
		    inform (DECL_SOURCE_LOCATION (fn), msg2, fn);
		}
	      goto ok;
	    }

	  if (complain & tf_warning_or_error)
	    {
	      auto_diagnostic_group d;
	      if (permerror (input_location, msg1))
		{
		  /* Only mention C++14 for namespace-scope delete.  */
		  if (DECL_NAMESPACE_SCOPE_P (fn))
		    inform (DECL_SOURCE_LOCATION (fn), msg2, fn);
		  else
		    inform (DECL_SOURCE_LOCATION (fn),
			    "%qD is a usual (non-placement) deallocation "
			    "function", fn);
		}
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
    for (tree elt : lkp_range (MAYBE_BASELINK_FUNCTIONS (fns)))
      {
	dealloc_info di_elt;
	if (usual_deallocation_fn_p (elt, &di_elt))
	  {
	    /* If we're called for an EH cleanup in a new-expression, we can't
	       use a destroying delete; the exception was thrown before the
	       object was constructed.  */
	    if (alloc_fn && di_elt.destroying)
	      {
		excluded_destroying = elt;
		continue;
	      }

	    if (!fn)
	      {
		fn = elt;
		di_fn = di_elt;
		continue;
	      }

	    /* -- If any of the deallocation functions is a destroying
	       operator delete, all deallocation functions that are not
	       destroying operator deletes are eliminated from further
	       consideration.  */
	    if (di_elt.destroying != di_fn.destroying)
	      {
		if (di_elt.destroying)
		  {
		    fn = elt;
		    di_fn = di_elt;
		  }
		continue;
	      }

	    /* -- If the type has new-extended alignment, a function with a
	       parameter of type std::align_val_t is preferred; otherwise a
	       function without such a parameter is preferred. If exactly one
	       preferred function is found, that function is selected and the
	       selection process terminates. If more than one preferred
	       function is found, all non-preferred functions are eliminated
	       from further consideration.  */
	    if (aligned_new_threshold)
	      {
		bool want_align = type_has_new_extended_alignment (type);
		if (di_elt.aligned != di_fn.aligned)
		  {
		    if (want_align == di_elt.aligned)
		      {
			fn = elt;
			di_fn = di_elt;
		      }
		    continue;
		  }
	      }

	    /* -- If the deallocation functions have class scope, the one
	       without a parameter of type std::size_t is selected.  */
	    bool want_size;
	    if (DECL_CLASS_SCOPE_P (fn))
	      want_size = false;

	    /* -- If the type is complete and if, for the second alternative
	       (delete array) only, the operand is a pointer to a class type
	       with a non-trivial destructor or a (possibly multi-dimensional)
	       array thereof, the function with a parameter of type std::size_t
	       is selected.

	       -- Otherwise, it is unspecified whether a deallocation function
	       with a parameter of type std::size_t is selected.  */
	    else
	      {
		want_size = COMPLETE_TYPE_P (type);
		if (code == VEC_DELETE_EXPR
		    && !TYPE_VEC_NEW_USES_COOKIE (type))
		  /* We need a cookie to determine the array size.  */
		  want_size = false;
	      }
	    gcc_assert (di_fn.sized != di_elt.sized);
	    if (want_size == di_elt.sized)
	      {
		fn = elt;
		di_fn = di_elt;
	      }
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

      tree ret;
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
	  if (!mark_used (fn, complain) && !(complain & tf_error))
	    return error_mark_node;
	  ret = build_cxx_call (fn, nargs, argarray, complain);
	}
      else
	{
	  tree destroying = di_fn.destroying;
	  if (destroying)
	    {
	      /* Strip const and volatile from addr but retain the type of the
		 object.  */
	      tree rtype = TREE_TYPE (TREE_TYPE (oaddr));
	      rtype = cv_unqualified (rtype);
	      rtype = TYPE_POINTER_TO (rtype);
	      addr = cp_convert (rtype, oaddr, complain);
	      destroying = build_functional_cast (input_location,
						  destroying, NULL_TREE,
						  complain);
	    }

	  releasing_vec args;
	  args->quick_push (addr);
	  if (destroying)
	    args->quick_push (destroying);
	  if (di_fn.sized)
	    args->quick_push (size);
	  if (di_fn.aligned)
	    {
	      tree al = build_int_cst (align_type_node, TYPE_ALIGN_UNIT (type));
	      args->quick_push (al);
	    }
	  ret = cp_build_function_call_vec (fn, &args, complain);
	}

      /* Set this flag for all callers of this function.  In addition to
	 delete-expressions, this is called for deallocating coroutine state;
	 treat that as an implicit delete-expression.  This is also called for
	 the delete if the constructor throws in a new-expression, and for a
	 deleting destructor (which implements a delete-expression).  */
      /* But leave this flag off for destroying delete to avoid wrong
	 assumptions in the optimizers.  */
      tree call = extract_call_expr (ret);
      if (TREE_CODE (call) == CALL_EXPR && !destroying_delete_p (fn))
	CALL_FROM_NEW_OR_DELETE_P (call) = 1;

      return ret;
    }

  /* If there's only a destroying delete that we can't use because the
     object isn't constructed yet, and we used global new, use global
     delete as well.  */
  if (excluded_destroying
      && DECL_NAMESPACE_SCOPE_P (alloc_fn))
    return build_op_delete_call (code, addr, size, true, placement,
				 alloc_fn, complain);

  /* [expr.new]

     If no unambiguous matching deallocation function can be found,
     propagating the exception does not cause the object's memory to
     be freed.  */
  if (alloc_fn)
    {
      if ((complain & tf_warning)
	  && !placement)
	{
	  bool w = warning (0,
			    "no corresponding deallocation function for %qD",
			    alloc_fn);
	  if (w && excluded_destroying)
	    inform (DECL_SOURCE_LOCATION (excluded_destroying), "destroying "
		    "delete %qD cannot be used to release the allocated memory"
		    " if the initialization throws because the object is not "
		    "constructed yet", excluded_destroying);
	}
      return NULL_TREE;
    }

  if (complain & tf_error)
    error ("no suitable %<operator %s%> for %qT",
	   OVL_OP_INFO (false, code)->name, type);
  return error_mark_node;
}

/* Issue diagnostics about a disallowed access of DECL, using DIAG_DECL
   in the diagnostics.

   If ISSUE_ERROR is true, then issue an error about the access, followed
   by a note showing the declaration.  Otherwise, just show the note.

   DIAG_DECL and DIAG_LOCATION will almost always be the same.
   DIAG_LOCATION is just another DECL.  NO_ACCESS_REASON is an optional
   parameter used to specify why DECL wasn't accessible (e.g. ak_private
   would be because DECL was private).  If not using NO_ACCESS_REASON,
   then it must be ak_none, and the access failure reason will be
   figured out by looking at the protection of DECL.  */

void
complain_about_access (tree decl, tree diag_decl, tree diag_location,
		       bool issue_error, access_kind no_access_reason)
{
  /* If we have not already figured out why DECL is inaccessible...  */
  if (no_access_reason == ak_none)
    {
      /* Examine the access of DECL to find out why.  */
      if (TREE_PRIVATE (decl))
	no_access_reason = ak_private;
      else if (TREE_PROTECTED (decl))
	no_access_reason = ak_protected;
    }

  /* Now generate an error message depending on calculated access.  */
  if (no_access_reason == ak_private)
    {
      if (issue_error)
	error ("%q#D is private within this context", diag_decl);
      inform (DECL_SOURCE_LOCATION (diag_location), "declared private here");
    }
  else if (no_access_reason == ak_protected)
    {
      if (issue_error)
	error ("%q#D is protected within this context", diag_decl);
      inform (DECL_SOURCE_LOCATION (diag_location), "declared protected here");
    }
  /* Couldn't figure out why DECL is inaccesible, so just say it's
     inaccessible.  */
  else
    {
      if (issue_error)
	error ("%q#D is inaccessible within this context", diag_decl);
      inform (DECL_SOURCE_LOCATION (diag_decl), "declared here");
    }
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

  *diagnostic_kind = DK_UNSPECIFIED;

  /* If the source is a packed field, calling the copy constructor will require
     binding the field to the reference parameter to the copy constructor, and
     we'll end up with an infinite loop.  If we can use a bitwise copy, then
     do that now.  */
  if ((lvalue_kind (expr) & clk_packed)
      && CLASS_TYPE_P (TREE_TYPE (expr))
      && !type_has_nontrivial_copy_init (TREE_TYPE (expr)))
    return get_target_expr (expr, complain);

  /* In decltype, we might have decided not to wrap this call in a TARGET_EXPR.
     But it turns out to be a subexpression, so perform temporary
     materialization now.  */
  if (TREE_CODE (expr) == CALL_EXPR
      && CLASS_TYPE_P (type)
      && same_type_ignoring_top_level_qualifiers_p (type, TREE_TYPE (expr)))
    expr = build_cplus_new (type, expr, complain);

  savew = warningcount + werrorcount, savee = errorcount;
  releasing_vec args (make_tree_vector_single (expr));
  expr = build_special_member_call (NULL_TREE, complete_ctor_identifier,
				    &args, type, flags, complain);
  if (warningcount + werrorcount > savew)
    *diagnostic_kind = DK_WARNING;
  else if (errorcount > savee)
    *diagnostic_kind = DK_ERROR;
  return expr;
}

/* Get any location for EXPR, falling back to input_location.

   If the result is in a system header and is the virtual location for
   a token coming from the expansion of a macro, unwind it to the
   location of the expansion point of the macro (e.g. to avoid the
   diagnostic being suppressed for expansions of NULL where "NULL" is
   in a system header).  */

static location_t
get_location_for_expr_unwinding_for_system_header (tree expr)
{
  location_t loc = EXPR_LOC_OR_LOC (expr, input_location);
  loc = expansion_point_location_if_in_system_header (loc);
  return loc;
}

/* Perform warnings about peculiar, but valid, conversions from/to NULL.
   Also handle a subset of zero as null warnings.
   EXPR is implicitly converted to type TOTYPE.
   FN and ARGNUM are used for diagnostics.  */

static void
conversion_null_warnings (tree totype, tree expr, tree fn, int argnum)
{
  /* Issue warnings about peculiar, but valid, uses of NULL.  */
  if (TREE_CODE (totype) != BOOLEAN_TYPE
      && ARITHMETIC_TYPE_P (totype)
      && null_node_p (expr))
    {
      location_t loc = get_location_for_expr_unwinding_for_system_header (expr);
      if (fn)
	{
	  auto_diagnostic_group d;
	  if (warning_at (loc, OPT_Wconversion_null,
			  "passing NULL to non-pointer argument %P of %qD",
			  argnum, fn))
	    inform (get_fndecl_argument_location (fn, argnum),
		    "  declared here");
	}
      else
	warning_at (loc, OPT_Wconversion_null,
		    "converting to non-pointer type %qT from NULL", totype);
    }

  /* Issue warnings if "false" is converted to a NULL pointer */
  else if (TREE_CODE (TREE_TYPE (expr)) == BOOLEAN_TYPE
	   && TYPE_PTR_P (totype))
    {
      location_t loc = get_location_for_expr_unwinding_for_system_header (expr);
      if (fn)
	{
	  auto_diagnostic_group d;
	  if (warning_at (loc, OPT_Wconversion_null,
			  "converting %<false%> to pointer type for argument "
			  "%P of %qD", argnum, fn))
	    inform (get_fndecl_argument_location (fn, argnum),
		    "  declared here");
	}
      else
	warning_at (loc, OPT_Wconversion_null,
		    "converting %<false%> to pointer type %qT", totype);
    }
  /* Handle zero as null pointer warnings for cases other
     than EQ_EXPR and NE_EXPR */
  else if ((TYPE_PTR_OR_PTRMEM_P (totype) || NULLPTR_TYPE_P (totype))
	   && null_ptr_cst_p (expr))
    {
      location_t loc = get_location_for_expr_unwinding_for_system_header (expr);
      maybe_warn_zero_as_null_pointer_constant (expr, loc);
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
	  print_z_candidate (0, N_("  after user-defined conversion:"),
			     t->cand);
	  break;
	}
}

/* Locate the parameter with the given index within FNDECL.
   ARGNUM is zero based, -1 indicates the `this' argument of a method.
   Return the location of the FNDECL itself if there are problems.  */

location_t
get_fndecl_argument_location (tree fndecl, int argnum)
{
  /* The locations of implicitly-declared functions are likely to be
     more meaningful than those of their parameters.  */
  if (DECL_ARTIFICIAL (fndecl))
    return DECL_SOURCE_LOCATION (fndecl);

  int i;
  tree param;

  /* Locate param by index within DECL_ARGUMENTS (fndecl).  */
  for (i = 0, param = FUNCTION_FIRST_USER_PARM (fndecl);
       i < argnum && param;
       i++, param = TREE_CHAIN (param))
    ;

  /* If something went wrong (e.g. if we have a builtin and thus no arguments),
     return the location of FNDECL.  */
  if (param == NULL)
    return DECL_SOURCE_LOCATION (fndecl);

  return DECL_SOURCE_LOCATION (param);
}

/* If FNDECL is non-NULL, issue a note highlighting ARGNUM
   within its declaration (or the fndecl itself if something went
   wrong).  */

void
maybe_inform_about_fndecl_for_bogus_argument_init (tree fn, int argnum)
{
  if (fn)
    inform (get_fndecl_argument_location (fn, argnum),
	    "  initializing argument %P of %qD", argnum, fn);
}

/* Maybe warn about C++20 Conversions to arrays of unknown bound.  C is
   the conversion, EXPR is the expression we're converting.  */

static void
maybe_warn_array_conv (location_t loc, conversion *c, tree expr)
{
  if (cxx_dialect >= cxx20)
    return;

  tree type = TREE_TYPE (expr);
  type = strip_pointer_operator (type);

  if (TREE_CODE (type) != ARRAY_TYPE
      || TYPE_DOMAIN (type) == NULL_TREE)
    return;

  if (pedantic && conv_binds_to_array_of_unknown_bound (c))
    pedwarn (loc, OPT_Wc__20_extensions,
	     "conversions to arrays of unknown bound "
	     "are only available with %<-std=c++20%> or %<-std=gnu++20%>");
}

/* We call this recursively in convert_like_internal.  */
static tree convert_like (conversion *, tree, tree, int, bool, bool, bool,
			  tsubst_flags_t);

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
convert_like_internal (conversion *convs, tree expr, tree fn, int argnum,
		       bool issue_conversion_warnings, bool c_cast_p,
		       bool nested_p, tsubst_flags_t complain)
{
  tree totype = convs->type;
  diagnostic_t diag_kind;
  int flags;
  location_t loc = cp_expr_loc_or_input_loc (expr);

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
      int complained = 0;
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
	complained = permerror (loc, "converting to %qH from %qI requires "
				"direct-initialization",
				totype, TREE_TYPE (expr));

      if (SCALAR_FLOAT_TYPE_P (TREE_TYPE (expr))
	  && SCALAR_FLOAT_TYPE_P (totype)
	  && (extended_float_type_p (TREE_TYPE (expr))
	      || extended_float_type_p (totype)))
	switch (cp_compare_floating_point_conversion_ranks (TREE_TYPE (expr),
							    totype))
	  {
	  case 2:
	    if (pedwarn (loc, OPT_Wnarrowing, "ISO C++ does not allow "
			 "converting to %qH from %qI with greater "
			 "conversion rank", totype, TREE_TYPE (expr)))
	      complained = 1;
	    else if (!complained)
	      complained = -1;
	    break;
	  case 3:
	    if (pedwarn (loc, OPT_Wnarrowing, "ISO C++ does not allow "
			 "converting to %qH from %qI with unordered "
			 "conversion rank", totype, TREE_TYPE (expr)))
	      complained = 1;
	    else if (!complained)
	      complained = -1;
	    break;
	  default:
	    break;
	  }

      for (; t ; t = next_conversion (t))
	{
	  if (t->kind == ck_user && t->cand->reason)
	    {
	      auto_diagnostic_group d;
	      complained = permerror (loc, "invalid user-defined conversion "
				      "from %qH to %qI", TREE_TYPE (expr),
				      totype);
	      if (complained)
		print_z_candidate (loc, N_("candidate is:"), t->cand);
	      expr = convert_like (t, expr, fn, argnum,
				   /*issue_conversion_warnings=*/false,
				   /*c_cast_p=*/false, /*nested_p=*/true,
				   complain);
	    }
	  else if (t->kind == ck_user || !t->bad_p)
	    {
	      expr = convert_like (t, expr, fn, argnum,
				   /*issue_conversion_warnings=*/false,
				   /*c_cast_p=*/false, /*nested_p=*/true,
				   complain);
	      if (t->bad_p)
		complained = 1;
	      break;
	    }
	  else if (t->kind == ck_ambig)
	    return convert_like (t, expr, fn, argnum,
				 /*issue_conversion_warnings=*/false,
				 /*c_cast_p=*/false, /*nested_p=*/true,
				 complain);
	  else if (t->kind == ck_identity)
	    break;
	}
      if (!complained && expr != error_mark_node)
	{
	  range_label_for_type_mismatch label (TREE_TYPE (expr), totype);
	  gcc_rich_location richloc (loc, &label);
	  complained = permerror (&richloc,
				  "invalid conversion from %qH to %qI",
				  TREE_TYPE (expr), totype);
	}
      if (convs->kind == ck_ref_bind)
	expr = convert_to_reference (totype, expr, CONV_IMPLICIT,
				     LOOKUP_NORMAL, NULL_TREE,
				     complain);
      else
	expr = cp_convert (totype, expr, complain);
      if (complained == 1)
	maybe_inform_about_fndecl_for_bogus_argument_init (fn, argnum);
      return expr;
    }

  if (issue_conversion_warnings && (complain & tf_warning))
    conversion_null_warnings (totype, expr, fn, argnum);

  switch (convs->kind)
    {
    case ck_user:
      {
	struct z_candidate *cand = convs->cand;

	if (cand == NULL)
	  /* We chose the surrogate function from add_conv_candidate, now we
	     actually need to build the conversion.  */
	  cand = build_user_type_conversion_1 (totype, expr,
					       LOOKUP_NO_CONVERSION, complain);

	tree convfn = cand->fn;

	/* When converting from an init list we consider explicit
	   constructors, but actually trying to call one is an error.  */
	if (DECL_NONCONVERTING_P (convfn) && DECL_CONSTRUCTOR_P (convfn)
	    && BRACE_ENCLOSED_INITIALIZER_P (expr)
	    /* Unless this is for direct-list-initialization.  */
	    && (!CONSTRUCTOR_IS_DIRECT_INIT (expr) || convs->need_temporary_p)
	    /* And in C++98 a default constructor can't be explicit.  */
	    && cxx_dialect >= cxx11)
	  {
	    if (!(complain & tf_error))
	      return error_mark_node;
	    location_t loc = location_of (expr);
	    if (CONSTRUCTOR_NELTS (expr) == 0
		&& FUNCTION_FIRST_USER_PARMTYPE (convfn) != void_list_node)
	      {
		auto_diagnostic_group d;
		if (pedwarn (loc, 0, "converting to %qT from initializer list "
			     "would use explicit constructor %qD",
			     totype, convfn))
		  {
		    inform (DECL_SOURCE_LOCATION (convfn), "%qD declared here",
			    convfn);
		    inform (loc, "in C++11 and above a default constructor "
			    "can be explicit");
		  }
	      }
	    else
	      {
		auto_diagnostic_group d;
		error ("converting to %qT from initializer list would use "
		       "explicit constructor %qD", totype, convfn);
		inform (DECL_SOURCE_LOCATION (convfn), "%qD declared here",
			convfn);
	      }
	  }

	/* If we're initializing from {}, it's value-initialization.  */
	if (BRACE_ENCLOSED_INITIALIZER_P (expr)
	    && CONSTRUCTOR_NELTS (expr) == 0
	    && TYPE_HAS_DEFAULT_CONSTRUCTOR (totype)
	    && !processing_template_decl)
	  {
	    if (abstract_virtuals_error (NULL_TREE, totype, complain))
	      return error_mark_node;
	    expr = build_value_init (totype, complain);
	    expr = get_target_expr (expr, complain);
	    if (expr != error_mark_node)
	      TARGET_EXPR_LIST_INIT_P (expr) = true;
	    return expr;
	  }

	/* We don't know here whether EXPR is being used as an lvalue or
	   rvalue, but we know it's read.  */
	mark_exp_read (expr);

	/* Pass LOOKUP_NO_CONVERSION so rvalue/base handling knows not to allow
	   any more UDCs.  */
	expr = build_over_call (cand, LOOKUP_NORMAL|LOOKUP_NO_CONVERSION,
				complain);

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
      expr = mark_use (expr, /*rvalue_p=*/!convs->rvaluedness_matches_p,
		       /*read_p=*/true, UNKNOWN_LOCATION,
		       /*reject_builtin=*/true);

      if (type_unknown_p (expr))
	expr = instantiate_type (totype, expr, complain);
      if (!nested_p && TREE_CODE (expr) == EXCESS_PRECISION_EXPR)
	expr = cp_convert (totype, TREE_OPERAND (expr, 0), complain);
      if (expr == null_node
	  && INTEGRAL_OR_UNSCOPED_ENUMERATION_TYPE_P (totype))
	/* If __null has been converted to an integer type, we do not want to
	   continue to warn about uses of EXPR as an integer, rather than as a
	   pointer.  */
	expr = build_int_cst (totype, 0);
      return expr;
    case ck_ambig:
      /* We leave bad_p off ck_ambig because overload resolution considers
	 it valid, it just fails when we try to perform it.  So we need to
         check complain here, too.  */
      if (complain & tf_error)
	{
	  /* Call build_user_type_conversion again for the error.  */
	  int flags = (convs->need_temporary_p
		       ? LOOKUP_IMPLICIT : LOOKUP_NORMAL);
	  build_user_type_conversion (totype, convs->u.expr, flags, complain);
	  gcc_assert (seen_error ());
	  maybe_inform_about_fndecl_for_bogus_argument_init (fn, argnum);
	}
      return error_mark_node;

    case ck_list:
      {
	/* Conversion to std::initializer_list<T>.  */
	tree elttype = TREE_VEC_ELT (CLASSTYPE_TI_ARGS (totype), 0);
	unsigned len = CONSTRUCTOR_NELTS (expr);
	tree array;

	if (tree init = maybe_init_list_as_array (elttype, expr))
	  {
	    elttype = cp_build_qualified_type
	      (elttype, cp_type_quals (elttype) | TYPE_QUAL_CONST);
	    array = build_array_of_n_type (elttype, len);
	    array = build_vec_init_expr (array, init, complain);
	    array = get_target_expr (array);
	    array = cp_build_addr_expr (array, complain);
	  }
	else if (len)
	  {
	    tree val; unsigned ix;

	    tree new_ctor = build_constructor (init_list_type_node, NULL);

	    /* Convert all the elements.  */
	    FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (expr), ix, val)
	      {
		tree sub = convert_like (convs->u.list[ix], val, fn,
					 argnum, false, false,
					 /*nested_p=*/true, complain);
		if (sub == error_mark_node)
		  return sub;
		if (!BRACE_ENCLOSED_INITIALIZER_P (val)
		    && !check_narrowing (TREE_TYPE (sub), val, complain))
		  return error_mark_node;
		CONSTRUCTOR_APPEND_ELT (CONSTRUCTOR_ELTS (new_ctor),
					NULL_TREE, sub);
		if (!TREE_CONSTANT (sub))
		  TREE_CONSTANT (new_ctor) = false;
	      }
	    /* Build up the array.  */
	    elttype = cp_build_qualified_type
	      (elttype, cp_type_quals (elttype) | TYPE_QUAL_CONST);
	    array = build_array_of_n_type (elttype, len);
	    array = finish_compound_literal (array, new_ctor, complain);
	    /* This is dubious now, should be blessed by P2752.  */
	    DECL_MERGEABLE (TARGET_EXPR_SLOT (array)) = true;
	    array = cp_build_addr_expr (array, complain);
	  }
	else
	  array = nullptr_node;

	array = cp_convert (build_pointer_type (elttype), array, complain);
	if (array == error_mark_node)
	  return error_mark_node;

	/* Build up the initializer_list object.  Note: fail gracefully
	   if the object cannot be completed because, for example, no
	   definition is provided (c++/80956).  */
	totype = complete_type_or_maybe_complain (totype, NULL_TREE, complain);
	if (!totype)
	  return error_mark_node;
	tree field = next_aggregate_field (TYPE_FIELDS (totype));
	vec<constructor_elt, va_gc> *vec = NULL;
	CONSTRUCTOR_APPEND_ELT (vec, field, array);
	field = next_aggregate_field (DECL_CHAIN (field));
	CONSTRUCTOR_APPEND_ELT (vec, field, size_int (len));
	tree new_ctor = build_constructor (totype, vec);
	return get_target_expr (new_ctor, complain);
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
	  return expr;
	}
      expr = reshape_init (totype, expr, complain);
      expr = get_target_expr (digest_init (totype, expr, complain),
				     complain);
      if (expr != error_mark_node)
	TARGET_EXPR_LIST_INIT_P (expr) = true;
      return expr;

    default:
      break;
    };

  conversion *nc = next_conversion (convs);
  if (convs->kind == ck_ref_bind && nc->kind == ck_qual
      && !convs->need_temporary_p)
    /* direct_reference_binding might have inserted a ck_qual under
       this ck_ref_bind for the benefit of conversion sequence ranking.
       Don't actually perform that conversion.  */
    nc = next_conversion (nc);

  expr = convert_like (nc, expr, fn, argnum,
		       convs->kind == ck_ref_bind
		       ? issue_conversion_warnings : false,
		       c_cast_p, /*nested_p=*/true, complain & ~tf_no_cleanup);
  if (expr == error_mark_node)
    return error_mark_node;

  switch (convs->kind)
    {
    case ck_rvalue:
      expr = decay_conversion (expr, complain);
      if (expr == error_mark_node)
	{
	  if (complain & tf_error)
	    {
	      auto_diagnostic_group d;
	      maybe_print_user_conv_context (convs);
	      maybe_inform_about_fndecl_for_bogus_argument_init (fn, argnum);
	    }
	  return error_mark_node;
	}

      if (! MAYBE_CLASS_TYPE_P (totype))
	return expr;

      /* Don't introduce copies when passing arguments along to the inherited
	 constructor.  */
      if (current_function_decl
	  && flag_new_inheriting_ctors
	  && DECL_INHERITED_CTOR (current_function_decl))
	return expr;

      if (TREE_CODE (expr) == TARGET_EXPR
	  && TARGET_EXPR_LIST_INIT_P (expr))
	/* Copy-list-initialization doesn't actually involve a copy.  */
	return expr;

      /* Fall through.  */
    case ck_base:
      if (convs->kind == ck_base && !convs->need_temporary_p)
	{
	  /* We are going to bind a reference directly to a base-class
	     subobject of EXPR.  */
	  /* Build an expression for `*((base*) &expr)'.  */
	  expr = convert_to_base (expr, totype,
				  !c_cast_p, /*nonnull=*/true, complain);
	  return expr;
	}

      /* Copy-initialization where the cv-unqualified version of the source
	 type is the same class as, or a derived class of, the class of the
	 destination [is treated as direct-initialization].  [dcl.init] */
      flags = LOOKUP_NORMAL;
      /* This conversion is being done in the context of a user-defined
	 conversion (i.e. the second step of copy-initialization), so
	 don't allow any more.  */
      if (convs->user_conv_p)
	flags |= LOOKUP_NO_CONVERSION;
      /* We might be performing a conversion of the argument
	 to the user-defined conversion, i.e., not a conversion of the
	 result of the user-defined conversion.  In which case we skip
	 explicit constructors.  */
      if (convs->copy_init_p)
	flags |= LOOKUP_ONLYCONVERTING;
      expr = build_temp (expr, totype, flags, &diag_kind, complain);
      if (diag_kind && complain)
	{
	  auto_diagnostic_group d;
	  maybe_print_user_conv_context (convs);
	  maybe_inform_about_fndecl_for_bogus_argument_init (fn, argnum);
	}

      return build_cplus_new (totype, expr, complain);

    case ck_ref_bind:
      {
	tree ref_type = totype;

	if (convs->bad_p && !next_conversion (convs)->bad_p)
	  {
	    tree extype = TREE_TYPE (expr);
	    auto_diagnostic_group d;
	    if (TYPE_REF_IS_RVALUE (ref_type)
		&& lvalue_p (expr))
	      error_at (loc, "cannot bind rvalue reference of type %qH to "
                        "lvalue of type %qI", totype, extype);
	    else if (!TYPE_REF_IS_RVALUE (ref_type) && !lvalue_p (expr)
		     && !CP_TYPE_CONST_NON_VOLATILE_P (TREE_TYPE (ref_type)))
	      {
		conversion *next = next_conversion (convs);
		if (next->kind == ck_std)
		  {
		    next = next_conversion (next);
		    error_at (loc, "cannot bind non-const lvalue reference of "
			      "type %qH to a value of type %qI",
			      totype, next->type);
		  }
		else if (!CP_TYPE_CONST_P (TREE_TYPE (ref_type)))
		  error_at (loc, "cannot bind non-const lvalue reference of "
			    "type %qH to an rvalue of type %qI", totype, extype);
		else // extype is volatile
		  error_at (loc, "cannot bind lvalue reference of type "
			    "%qH to an rvalue of type %qI", totype,
			    extype);
	      }
	    else if (!reference_compatible_p (TREE_TYPE (totype), extype))
	      {
		/* If we're converting from T[] to T[N], don't talk
		   about discarding qualifiers.  (Converting from T[N] to
		   T[] is allowed by P0388R4.)  */
		if (TREE_CODE (extype) == ARRAY_TYPE
		    && TYPE_DOMAIN (extype) == NULL_TREE
		    && TREE_CODE (TREE_TYPE (totype)) == ARRAY_TYPE
		    && TYPE_DOMAIN (TREE_TYPE (totype)) != NULL_TREE)
		  error_at (loc, "cannot bind reference of type %qH to %qI "
			    "due to different array bounds", totype, extype);
		else
		  error_at (loc, "binding reference of type %qH to %qI "
			    "discards qualifiers", totype, extype);
	      }
	    else
	      gcc_unreachable ();
	    maybe_print_user_conv_context (convs);
	    maybe_inform_about_fndecl_for_bogus_argument_init (fn, argnum);

	    return error_mark_node;
	  }
	else if (complain & tf_warning)
	  maybe_warn_array_conv (loc, convs, expr);

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
	    cp_lvalue_kind lvalue = lvalue_kind (expr);

	    gcc_assert (similar_type_p (type, next_conversion (convs)->type));
	    if (!CP_TYPE_CONST_NON_VOLATILE_P (type)
		&& !TYPE_REF_IS_RVALUE (ref_type))
	      {
		/* If the reference is volatile or non-const, we
		   cannot create a temporary.  */
		if (complain & tf_error)
		  {
		    if (lvalue & clk_bitfield)
		      error_at (loc, "cannot bind bit-field %qE to %qT",
				expr, ref_type);
		    else if (lvalue & clk_packed)
		      error_at (loc, "cannot bind packed field %qE to %qT",
				expr, ref_type);
		    else
		      error_at (loc, "cannot bind rvalue %qE to %qT",
				expr, ref_type);
		  }
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

	    /* Creating &TARGET_EXPR<> in a template would break when
	       tsubsting the expression, so use an IMPLICIT_CONV_EXPR
	       instead.  This can happen even when there's no class
	       involved, e.g., when converting an integer to a reference
	       type.  */
	    if (processing_template_decl)
	      return build1 (IMPLICIT_CONV_EXPR, totype, expr);
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

    case ck_fnptr:
      /* ??? Should the address of a transaction-safe pointer point to the TM
        clone, and this conversion look up the primary function?  */
      return build_nop (totype, expr);

    case ck_qual:
      /* Warn about deprecated conversion if appropriate.  */
      if (complain & tf_warning)
	{
	  string_conv_p (totype, expr, 1);
	  maybe_warn_array_conv (loc, convs, expr);
	}
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

  if (convs->check_narrowing
      && !check_narrowing (totype, expr, complain,
			   convs->check_narrowing_const_only))
    return error_mark_node;

  warning_sentinel w (warn_zero_as_null_pointer_constant);
  if (issue_conversion_warnings)
    expr = cp_convert_and_check (totype, expr, complain);
  else
    {
      if (TREE_CODE (expr) == EXCESS_PRECISION_EXPR)
	expr = TREE_OPERAND (expr, 0);
      expr = cp_convert (totype, expr, complain);
    }

  return expr;
}

/* Return true if converting FROM to TO is unsafe in a template.  */

static bool
conv_unsafe_in_template_p (tree to, tree from)
{
  /* Converting classes involves TARGET_EXPR.  */
  if (CLASS_TYPE_P (to) || CLASS_TYPE_P (from))
    return true;

  /* Converting real to integer produces FIX_TRUNC_EXPR which tsubst
     doesn't handle.  */
  if (SCALAR_FLOAT_TYPE_P (from) && INTEGRAL_OR_ENUMERATION_TYPE_P (to))
    return true;

  /* Converting integer to real isn't a trivial conversion, either.  */
  if (INTEGRAL_OR_ENUMERATION_TYPE_P (from) && SCALAR_FLOAT_TYPE_P (to))
    return true;

  return false;
}

/* Wrapper for convert_like_internal that handles creating
   IMPLICIT_CONV_EXPR.  */

static tree
convert_like (conversion *convs, tree expr, tree fn, int argnum,
	      bool issue_conversion_warnings, bool c_cast_p, bool nested_p,
	      tsubst_flags_t complain)
{
  /* Creating &TARGET_EXPR<> in a template breaks when substituting,
     and creating a CALL_EXPR in a template breaks in finish_call_expr
     so use an IMPLICIT_CONV_EXPR for this conversion.  We would have
     created such codes e.g. when calling a user-defined conversion
     function.  */
  tree conv_expr = NULL_TREE;
  if (processing_template_decl
      && convs->kind != ck_identity
      && conv_unsafe_in_template_p (convs->type, TREE_TYPE (expr)))
    {
      conv_expr = build1 (IMPLICIT_CONV_EXPR, convs->type, expr);
      if (convs->kind != ck_ref_bind)
	conv_expr = convert_from_reference (conv_expr);
      if (!convs->bad_p)
	return conv_expr;
      /* Do the normal processing to give the bad_p errors.  But we still
	 need to return the IMPLICIT_CONV_EXPR, unless we're returning
	 error_mark_node.  */
    }
  expr = convert_like_internal (convs, expr, fn, argnum,
				issue_conversion_warnings, c_cast_p,
				nested_p, complain);
  if (expr == error_mark_node)
    return error_mark_node;
  return conv_expr ? conv_expr : expr;
}

/* Convenience wrapper for convert_like.  */

static inline tree
convert_like (conversion *convs, tree expr, tsubst_flags_t complain)
{
  return convert_like (convs, expr, NULL_TREE, 0,
		       /*issue_conversion_warnings=*/true,
		       /*c_cast_p=*/false, /*nested_p=*/false, complain);
}

/* Convenience wrapper for convert_like.  */

static inline tree
convert_like_with_context (conversion *convs, tree expr, tree fn, int argnum,
			   tsubst_flags_t complain)
{
  return convert_like (convs, expr, fn, argnum,
		       /*issue_conversion_warnings=*/true,
		       /*c_cast_p=*/false, /*nested_p=*/false, complain);
}

/* ARG is being passed to a varargs function.  Perform any conversions
   required.  Return the converted value.  */

tree
convert_arg_to_ellipsis (tree arg, tsubst_flags_t complain)
{
  tree arg_type = TREE_TYPE (arg);
  location_t loc = cp_expr_loc_or_input_loc (arg);

  /* [expr.call]

     If the argument has integral or enumeration type that is subject
     to the integral promotions (_conv.prom_), or a floating-point
     type that is subject to the floating-point promotion
     (_conv.fpprom_), the value of the argument is converted to the
     promoted type before the call.  */
  if (SCALAR_FLOAT_TYPE_P (arg_type)
      && (TYPE_PRECISION (arg_type)
	  < TYPE_PRECISION (double_type_node))
      && !DECIMAL_FLOAT_MODE_P (TYPE_MODE (arg_type))
      && !extended_float_type_p (arg_type))
    {
      if ((complain & tf_warning)
	  && warn_double_promotion && !c_inhibit_evaluation_warnings)
	warning_at (loc, OPT_Wdouble_promotion,
		    "implicit conversion from %qH to %qI when passing "
		    "argument to function",
		    arg_type, double_type_node);
      if (TREE_CODE (arg) == EXCESS_PRECISION_EXPR)
	arg = TREE_OPERAND (arg, 0);
      arg = mark_rvalue_use (arg);
      arg = convert_to_real_nofold (double_type_node, arg);
    }
  else if (NULLPTR_TYPE_P (arg_type))
    {
      arg = mark_rvalue_use (arg);
      if (TREE_SIDE_EFFECTS (arg))
	{
	  warning_sentinel w(warn_unused_result);
	  arg = cp_build_compound_expr (arg, null_pointer_node, complain);
	}
      else
	arg = null_pointer_node;
    }
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
	    warning_at (loc, OPT_Wabi, "scoped enum %qT passed through %<...%>"
			" as %qT before %<-fabi-version=6%>, %qT after",
			arg_type,
			TREE_TYPE (prom), ENUM_UNDERLYING_TYPE (arg_type));
	  if (!abi_version_at_least (6))
	    arg = prom;
	}
      else
	arg = cp_perform_integral_promotions (arg, complain);
    }
  else
    /* [expr.call]

       The lvalue-to-rvalue, array-to-pointer, and function-to-pointer
       standard conversions are performed.  */
    arg = decay_conversion (arg, complain);

  arg = require_complete_type (arg, complain);
  arg_type = TREE_TYPE (arg);

  if (arg != error_mark_node
      /* In a template (or ill-formed code), we can have an incomplete type
	 even after require_complete_type, in which case we don't know
	 whether it has trivial copy or not.  */
      && COMPLETE_TYPE_P (arg_type)
      && !cp_unevaluated_operand)
    {
      /* [expr.call] 5.2.2/7:
	 Passing a potentially-evaluated argument of class type (Clause 9)
	 with a non-trivial copy constructor or a non-trivial destructor
	 with no corresponding parameter is conditionally-supported, with
	 implementation-defined semantics.

	 We support it as pass-by-invisible-reference, just like a normal
	 value parameter.

	 If the call appears in the context of a sizeof expression,
	 it is not potentially-evaluated.  */
      if (type_has_nontrivial_copy_init (arg_type)
	  || TYPE_HAS_NONTRIVIAL_DESTRUCTOR (arg_type))
	{
	  arg = force_rvalue (arg, complain);
	  if (complain & tf_warning)
	    warning (OPT_Wconditionally_supported,
		     "passing objects of non-trivially-copyable "
		     "type %q#T through %<...%> is conditionally supported",
		     arg_type);
	  return build1 (ADDR_EXPR, build_reference_type (arg_type), arg);
	}
      /* Build up a real lvalue-to-rvalue conversion in case the
	 copy constructor is trivial but not callable.  */
      else if (CLASS_TYPE_P (arg_type))
	force_rvalue (arg, complain);

    }

  return arg;
}

/* va_arg (EXPR, TYPE) is a builtin. Make sure it is not abused.  */

tree
build_x_va_arg (location_t loc, tree expr, tree type)
{
  if (processing_template_decl)
    {
      tree r = build_min (VA_ARG_EXPR, type, expr);
      SET_EXPR_LOCATION (r, loc);
      return r;
    }

  type = complete_type_or_else (type, NULL_TREE);

  if (expr == error_mark_node || !type)
    return error_mark_node;

  expr = mark_lvalue_use (expr);

  if (TYPE_REF_P (type))
    {
      error ("cannot receive reference type %qT through %<...%>", type);
      return error_mark_node;
    }

  if (type_has_nontrivial_copy_init (type)
      || TYPE_HAS_NONTRIVIAL_DESTRUCTOR (type))
    {
      /* conditionally-supported behavior [expr.call] 5.2.2/7.  Let's treat
	 it as pass by invisible reference.  */
      warning_at (loc, OPT_Wconditionally_supported,
		 "receiving objects of non-trivially-copyable type %q#T "
		 "through %<...%> is conditionally-supported", type);

      tree ref = cp_build_reference_type (type, false);
      expr = build_va_arg (loc, expr, ref);
      return convert_from_reference (expr);
    }

  tree ret = build_va_arg (loc, expr, type);
  if (CLASS_TYPE_P (type))
    /* Wrap the VA_ARG_EXPR in a TARGET_EXPR now so other code doesn't need to
       know how to handle it.  */
    ret = get_target_expr (ret);
  return ret;
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
  /* And inheriting ctors.  */
  if (flag_new_inheriting_ctors)
    fn = strip_inheriting_ctors (fn);

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
  if (TREE_CODE (arg) == DEFERRED_PARSE)
    {
      if (complain & tf_error)
	error ("call to %qD uses the default argument for parameter %P, which "
	       "is not yet defined", fn, parmnum);
      return error_mark_node;
    }

  push_defarg_context (fn);

  if (fn && DECL_TEMPLATE_INFO (fn))
    arg = tsubst_default_argument (fn, parmnum, type, arg, complain);

  /* Due to:

       [dcl.fct.default]

       The names in the expression are bound, and the semantic
       constraints are checked, at the point where the default
       expressions appears.

     we must not perform access checks here.  */
  push_deferring_access_checks (dk_no_check);
  /* We must make a copy of ARG, in case subsequent processing
     alters any part of it.  */
  arg = break_out_target_exprs (arg, /*clear location*/true);

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
    type = build_reference_type (type);
  else if (targetm.calls.promote_prototypes (NULL_TREE)
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
    val = convert_to_integer_nofold (TYPE_MAIN_VARIANT (bitfield_type), val);

  if (val == error_mark_node)
    ;
  /* Pass classes with copy ctors by invisible reference.  */
  else if (TREE_ADDRESSABLE (type))
    val = build1 (ADDR_EXPR, build_reference_type (type), val);
  else if (targetm.calls.promote_prototypes (NULL_TREE)
	   && INTEGRAL_TYPE_P (type)
	   && COMPLETE_TYPE_P (type)
	   && tree_int_cst_lt (TYPE_SIZE (type), TYPE_SIZE (integer_type_node)))
    val = cp_perform_integral_promotions (val, complain);
  if (complain & tf_warning)
    {
      if (warn_suggest_attribute_format)
	{
	  tree rhstype = TREE_TYPE (val);
	  const enum tree_code coder = TREE_CODE (rhstype);
	  const enum tree_code codel = TREE_CODE (type);
	  if ((codel == POINTER_TYPE || codel == REFERENCE_TYPE)
	      && coder == codel
	      && check_missing_format_attribute (type, rhstype))
	    warning (OPT_Wsuggest_attribute_format,
		     "argument of function call might be a candidate "
		     "for a format attribute");
	}
      maybe_warn_parm_abi (type, cp_expr_loc_or_input_loc (val));
    }

  if (complain & tf_warning)
    warn_for_address_of_packed_member (type, val);

  /* gimplify_arg elides TARGET_EXPRs that initialize a function argument.  */
  if (SIMPLE_TARGET_EXPR_P (val))
    set_target_expr_eliding (val);

  return val;
}

/* Returns non-zero iff FN is a function with magic varargs, i.e. ones for
   which just decay_conversion or no conversions at all should be done.
   This is true for some builtins which don't act like normal functions.
   Return 2 if just decay_conversion and removal of excess precision should
   be done, 1 if just decay_conversion.  Return 3 for special treatment of
   the 3rd argument for __builtin_*_overflow_p.  Return 4 for special
   treatment of the 1st argument for
   __builtin_{clz,ctz,clrsb,ffs,parity,popcount}g.  */

int
magic_varargs_p (tree fn)
{
  if (DECL_BUILT_IN_CLASS (fn) == BUILT_IN_NORMAL)
    switch (DECL_FUNCTION_CODE (fn))
      {
      case BUILT_IN_CLASSIFY_TYPE:
      case BUILT_IN_CONSTANT_P:
      case BUILT_IN_NEXT_ARG:
      case BUILT_IN_VA_START:
	return 1;

      case BUILT_IN_ADD_OVERFLOW_P:
      case BUILT_IN_SUB_OVERFLOW_P:
      case BUILT_IN_MUL_OVERFLOW_P:
	return 3;

      case BUILT_IN_ISFINITE:
      case BUILT_IN_ISINF:
      case BUILT_IN_ISINF_SIGN:
      case BUILT_IN_ISNAN:
      case BUILT_IN_ISNORMAL:
      case BUILT_IN_FPCLASSIFY:
	return 2;

      case BUILT_IN_CLZG:
      case BUILT_IN_CTZG:
      case BUILT_IN_CLRSBG:
      case BUILT_IN_FFSG:
      case BUILT_IN_PARITYG:
      case BUILT_IN_POPCOUNTG:
	return 4;

      default:
	return lookup_attribute ("type generic",
				 TYPE_ATTRIBUTES (TREE_TYPE (fn))) != 0;
      }

  return 0;
}

/* Returns the decl of the dispatcher function if FN is a function version.  */

tree
get_function_version_dispatcher (tree fn)
{
  tree dispatcher_decl = NULL;

  if (DECL_LOCAL_DECL_P (fn))
    fn = DECL_LOCAL_DECL_ALIAS (fn);

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

  node = cgraph_node::get (fn);
  if (node == NULL)
    return;

  gcc_assert (node->dispatcher_function);

  node_v = node->function_version ();
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

/* Build a call to "the copy constructor" for the type of A, even if it
   wouldn't be selected by normal overload resolution.  Used for
   diagnostics.  */

static tree
call_copy_ctor (tree a, tsubst_flags_t complain)
{
  tree ctype = TYPE_MAIN_VARIANT (TREE_TYPE (a));
  tree binfo = TYPE_BINFO (ctype);
  tree copy = get_copy_ctor (ctype, complain);
  copy = build_baselink (binfo, binfo, copy, NULL_TREE);
  tree ob = build_dummy_object (ctype);
  releasing_vec args (make_tree_vector_single (a));
  tree r = build_new_method_call (ob, copy, &args, NULL_TREE,
				  LOOKUP_NORMAL, NULL, complain);
  return r;
}

/* Return the base constructor corresponding to COMPLETE_CTOR or NULL_TREE.  */

static tree
base_ctor_for (tree complete_ctor)
{
  tree clone;
  FOR_EACH_CLONE (clone, DECL_CLONED_FUNCTION (complete_ctor))
    if (DECL_BASE_CONSTRUCTOR_P (clone))
      return clone;
  return NULL_TREE;
}

/* Try to make EXP suitable to be used as the initializer for a base subobject,
   and return whether we were successful.  EXP must have already been cleared
   by unsafe_copy_elision_p{,_opt}.  */

static bool
make_base_init_ok (tree exp)
{
  if (TREE_CODE (exp) == TARGET_EXPR)
    exp = TARGET_EXPR_INITIAL (exp);
  while (TREE_CODE (exp) == COMPOUND_EXPR)
    exp = TREE_OPERAND (exp, 1);
  if (TREE_CODE (exp) == COND_EXPR)
    {
      bool ret = make_base_init_ok (TREE_OPERAND (exp, 2));
      if (tree op1 = TREE_OPERAND (exp, 1))
	{
	  bool r1 = make_base_init_ok (op1);
	  /* If unsafe_copy_elision_p was false, the arms should match.  */
	  gcc_assert (r1 == ret);
	}
      return ret;
    }
  if (TREE_CODE (exp) != AGGR_INIT_EXPR)
    /* A trivial copy is OK.  */
    return true;
  if (!AGGR_INIT_VIA_CTOR_P (exp))
    /* unsafe_copy_elision_p_opt must have said this is OK.  */
    return true;
  tree fn = cp_get_callee_fndecl_nofold (exp);
  if (DECL_BASE_CONSTRUCTOR_P (fn))
    return true;
  gcc_assert (DECL_COMPLETE_CONSTRUCTOR_P (fn));
  fn = base_ctor_for (fn);
  if (!fn || DECL_HAS_VTT_PARM_P (fn))
    /* The base constructor has more parameters, so we can't just change the
       call target.  It would be possible to splice in the appropriate
       arguments, but probably not worth the complexity.  */
    return false;
  mark_used (fn);
  AGGR_INIT_EXPR_FN (exp) = build_address (fn);
  return true;
}

/* Return 2 if T refers to a base, 1 if a potentially-overlapping field,
   neither of which can be used for return by invisible reference.  We avoid
   doing C++17 mandatory copy elision for either of these cases.

   This returns non-zero even if the type of T has no tail padding that other
   data could be allocated into, because that depends on the particular ABI.
   unsafe_copy_elision_p_opt does consider whether there is padding.  */

int
unsafe_return_slot_p (tree t)
{
  /* Check empty bases separately, they don't have fields.  */
  if (is_empty_base_ref (t))
    return 2;

  /* A delegating constructor might be used to initialize a base.  */
  if (current_function_decl
      && DECL_CONSTRUCTOR_P (current_function_decl)
      && (t == current_class_ref
	  || tree_strip_nop_conversions (t) == current_class_ptr))
    return 2;

  STRIP_NOPS (t);
  if (TREE_CODE (t) == ADDR_EXPR)
    t = TREE_OPERAND (t, 0);
  if (TREE_CODE (t) == COMPONENT_REF)
    t = TREE_OPERAND (t, 1);
  if (TREE_CODE (t) != FIELD_DECL)
    return false;
  if (!CLASS_TYPE_P (TREE_TYPE (t)))
    /* The middle-end will do the right thing for scalar types.  */
    return false;
  if (DECL_FIELD_IS_BASE (t))
    return 2;
  if (lookup_attribute ("no_unique_address", DECL_ATTRIBUTES (t)))
    return 1;
  return 0;
}

/* True IFF EXP is a prvalue that represents return by invisible reference.  */

static bool
init_by_return_slot_p (tree exp)
{
  /* Copy elision only happens with a TARGET_EXPR.  */
  if (TREE_CODE (exp) != TARGET_EXPR)
    return false;
  tree init = TARGET_EXPR_INITIAL (exp);
  /* build_compound_expr pushes COMPOUND_EXPR inside TARGET_EXPR.  */
  while (TREE_CODE (init) == COMPOUND_EXPR)
    init = TREE_OPERAND (init, 1);
  if (TREE_CODE (init) == COND_EXPR)
    {
      /* We'll end up copying from each of the arms of the COND_EXPR directly
	 into the target, so look at them.  */
      if (tree op = TREE_OPERAND (init, 1))
	if (init_by_return_slot_p (op))
	  return true;
      return init_by_return_slot_p (TREE_OPERAND (init, 2));
    }
  return (TREE_CODE (init) == AGGR_INIT_EXPR
	  && !AGGR_INIT_VIA_CTOR_P (init));
}

/* We can't elide a copy from a function returning by value to a
   potentially-overlapping subobject, as the callee might clobber tail padding.
   Return true iff this could be that case.

   Places that use this function (or _opt) to decide to elide a copy should
   probably use make_safe_copy_elision instead.  */

bool
unsafe_copy_elision_p (tree target, tree exp)
{
  return unsafe_return_slot_p (target) && init_by_return_slot_p (exp);
}

/* As above, but for optimization allow more cases that are actually safe.  */

static bool
unsafe_copy_elision_p_opt (tree target, tree exp)
{
  tree type = TYPE_MAIN_VARIANT (TREE_TYPE (exp));
  /* It's safe to elide the copy for a class with no tail padding.  */
  if (!is_empty_class (type)
      && tree_int_cst_equal (TYPE_SIZE (type), CLASSTYPE_SIZE (type)))
    return false;
  return unsafe_copy_elision_p (target, exp);
}

/* Try to make EXP suitable to be used as the initializer for TARGET,
   and return whether we were successful.  */

bool
make_safe_copy_elision (tree target, tree exp)
{
  int uns = unsafe_return_slot_p (target);
  if (!uns)
    return true;
  if (init_by_return_slot_p (exp))
    return false;
  if (uns == 1)
    return true;
  return make_base_init_ok (exp);
}

/* True IFF the result of the conversion C is a prvalue.  */

static bool
conv_is_prvalue (conversion *c)
{
  if (c->kind == ck_rvalue)
    return true;
  if (c->kind == ck_base && c->need_temporary_p)
    return true;
  if (c->kind == ck_user && !TYPE_REF_P (c->type))
    return true;
  if (c->kind == ck_identity && c->u.expr
      && TREE_CODE (c->u.expr) == TARGET_EXPR)
    return true;

  return false;
}

/* True iff C is a conversion that binds a reference to a prvalue.  */

static bool
conv_binds_ref_to_prvalue (conversion *c)
{
  if (c->kind != ck_ref_bind)
    return false;
  if (c->need_temporary_p)
    return true;

  return conv_is_prvalue (next_conversion (c));
}

/* True iff EXPR represents a (subobject of a) temporary.  */

static bool
expr_represents_temporary_p (tree expr)
{
  while (handled_component_p (expr))
    expr = TREE_OPERAND (expr, 0);
  return TREE_CODE (expr) == TARGET_EXPR;
}

/* True iff C is a conversion that binds a reference to a temporary.
   This is a superset of conv_binds_ref_to_prvalue: here we're also
   interested in xvalues.  */

static bool
conv_binds_ref_to_temporary (conversion *c)
{
  if (conv_binds_ref_to_prvalue (c))
    return true;
  if (c->kind != ck_ref_bind)
    return false;
  c = next_conversion (c);
  /* This is the case for
       struct Base {};
       struct Derived : Base {};
       const Base& b(Derived{});
     where we bind 'b' to the Base subobject of a temporary object of type
     Derived.  The subobject is an xvalue; the whole object is a prvalue.

     The ck_base doesn't have to be present for cases like X{}.m.  */
  if (c->kind == ck_base)
    c = next_conversion (c);
  if (c->kind == ck_identity && c->u.expr
      && expr_represents_temporary_p (c->u.expr))
    return true;
  return false;
}

/* Return tristate::TS_TRUE if converting EXPR to a reference type TYPE binds
   the reference to a temporary.  Return tristate::TS_FALSE if converting
   EXPR to a reference type TYPE doesn't bind the reference to a temporary.  If
   the conversion is invalid or bad, return tristate::TS_UNKNOWN.  DIRECT_INIT_P
   says whether the conversion should be done in direct- or copy-initialization
   context.  */

tristate
ref_conv_binds_to_temporary (tree type, tree expr, bool direct_init_p/*=false*/)
{
  gcc_assert (TYPE_REF_P (type));

  conversion_obstack_sentinel cos;

  const int flags = direct_init_p ? LOOKUP_NORMAL : LOOKUP_IMPLICIT;
  conversion *conv = implicit_conversion (type, TREE_TYPE (expr), expr,
					  /*c_cast_p=*/false, flags, tf_none);
  tristate ret (tristate::TS_UNKNOWN);
  if (conv && !conv->bad_p)
    ret = tristate (conv_binds_ref_to_temporary (conv));

  return ret;
}

/* Call the trivial destructor for INSTANCE, which can be either an lvalue of
   class type or a pointer to class type.  If NO_PTR_DEREF is true and
   INSTANCE has pointer type, clobber the pointer rather than what it points
   to.  */

tree
build_trivial_dtor_call (tree instance, bool no_ptr_deref)
{
  gcc_assert (!is_dummy_object (instance));

  if (!flag_lifetime_dse)
    {
    no_clobber:
      return fold_convert (void_type_node, instance);
    }

  if (INDIRECT_TYPE_P (TREE_TYPE (instance))
      && (!no_ptr_deref || TYPE_REF_P (TREE_TYPE (instance))))
    {
      if (VOID_TYPE_P (TREE_TYPE (TREE_TYPE (instance))))
	goto no_clobber;
      instance = cp_build_fold_indirect_ref (instance);
    }

  /* A trivial destructor should still clobber the object.  */
  tree clobber = build_clobber (TREE_TYPE (instance), CLOBBER_OBJECT_END);
  return build2 (MODIFY_EXPR, void_type_node,
		 instance, clobber);
}

/* Return true if in an immediate function context, or an unevaluated operand,
   or a default argument/member initializer, or a subexpression of an immediate
   invocation.  */

bool
in_immediate_context ()
{
  return (cp_unevaluated_operand != 0
	  || (current_function_decl != NULL_TREE
	      && DECL_IMMEDIATE_FUNCTION_P (current_function_decl))
	  /* DR 2631: default args and DMI aren't immediately evaluated.
	     Return true here so immediate_invocation_p returns false.  */
	  || current_binding_level->kind == sk_function_parms
	  || current_binding_level->kind == sk_template_parms
	  || parsing_nsdmi ()
	  || in_consteval_if_p);
}

/* Return true if a call to FN with number of arguments NARGS
   is an immediate invocation.  */

bool
immediate_invocation_p (tree fn)
{
  return (TREE_CODE (fn) == FUNCTION_DECL
	  && DECL_IMMEDIATE_FUNCTION_P (fn)
	  && !in_immediate_context ());
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
  tree parm = TYPE_ARG_TYPES (TREE_TYPE (fn));
  int parmlen;
  tree val;
  int nargs;
  tree *argarray;
  bool already_used = false;

  /* In a template, there is no need to perform all of the work that
     is normally done.  We are only interested in the type of the call
     expression, i.e., the return type of the function.  Any semantic
     errors will be deferred until the template is instantiated.  */
  if (processing_template_decl)
    {
      if (undeduced_auto_decl (fn))
	mark_used (fn, complain);
      else
	/* Otherwise set TREE_USED for the benefit of -Wunused-function.
	   See PR80598.  */
	TREE_USED (fn) = 1;

      tree return_type = TREE_TYPE (TREE_TYPE (fn));
      tree callee;
      if (first_arg == NULL_TREE)
	{
	  callee = build_addr_func (fn, complain);
	  if (callee == error_mark_node)
	    return error_mark_node;
	}
      else
	{
	  callee = build_baselink (cand->conversion_path, cand->access_path,
				   fn, NULL_TREE);
	  callee = build_min (COMPONENT_REF, TREE_TYPE (fn),
			      first_arg, callee, NULL_TREE);
	}

      tree expr = build_call_vec (return_type, callee, args);
      SET_EXPR_LOCATION (expr, input_location);
      if (TREE_THIS_VOLATILE (fn) && cfun)
	current_function_returns_abnormally = 1;
      if (immediate_invocation_p (fn))
	{
	  tree obj_arg = NULL_TREE, exprimm = expr;
	  if (DECL_CONSTRUCTOR_P (fn))
	    obj_arg = first_arg;
	  if (obj_arg
	      && is_dummy_object (obj_arg)
	      && !type_dependent_expression_p (obj_arg))
	    {
	      exprimm = build_cplus_new (DECL_CONTEXT (fn), expr, complain);
	      obj_arg = NULL_TREE;
	    }
	  /* Look through *(const T *)&obj.  */
	  else if (obj_arg && INDIRECT_REF_P (obj_arg))
	    {
	      tree addr = TREE_OPERAND (obj_arg, 0);
	      STRIP_NOPS (addr);
	      if (TREE_CODE (addr) == ADDR_EXPR)
		{
		  tree typeo = TREE_TYPE (obj_arg);
		  tree typei = TREE_TYPE (TREE_OPERAND (addr, 0));
		  if (same_type_ignoring_top_level_qualifiers_p (typeo, typei))
		    obj_arg = TREE_OPERAND (addr, 0);
		}
	    }
	  fold_non_dependent_expr (exprimm, complain,
				   /*manifestly_const_eval=*/true,
				   obj_arg);
	}
      return convert_from_reference (expr);
    }

  /* Give any warnings we noticed during overload resolution.  */
  if (cand->warnings && (complain & tf_warning))
    {
      struct candidate_warning *w;
      for (w = cand->warnings; w; w = w->next)
	joust (cand, w->loser, 1, complain);
    }

  /* Core issue 2327: P0135 doesn't say how to handle the case where the
     argument to the copy constructor ends up being a prvalue after
     conversion.  Let's do the normal processing, but pretend we aren't
     actually using the copy constructor.  */
  bool force_elide = false;
  if (cxx_dialect >= cxx17
      && cand->num_convs == 1
      && DECL_COMPLETE_CONSTRUCTOR_P (fn)
      && (DECL_COPY_CONSTRUCTOR_P (fn)
	  || DECL_MOVE_CONSTRUCTOR_P (fn))
      && !unsafe_return_slot_p (first_arg)
      && conv_binds_ref_to_prvalue (convs[0]))
    {
      force_elide = true;
      goto not_really_used;
    }

  /* OK, we're actually calling this inherited constructor; set its deletedness
     appropriately.  We can get away with doing this here because calling is
     the only way to refer to a constructor.  */
  if (DECL_INHERITED_CTOR (fn)
      && !deduce_inheriting_ctor (fn))
    {
      if (complain & tf_error)
	mark_used (fn);
      return error_mark_node;
    }

  /* Make =delete work with SFINAE.  */
  if (DECL_DELETED_FN (fn))
    {
      if (complain & tf_error)
	{
	  mark_used (fn);
	  if (cand->next)
	    {
	      if (flag_diagnostics_all_candidates)
		print_z_candidates (input_location, cand, /*only_viable_p=*/false);
	      else
		inform (input_location,
			"use %<-fdiagnostics-all-candidates%> to display "
			"considered candidates");
	    }
	}
      return error_mark_node;
    }

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
      if (cand->viable == 1)
	return fn;
      else if (!(complain & tf_error))
	/* Reject bad conversions now.  */
	return error_mark_node;
      /* else continue to get conversion error.  */
    }

 not_really_used:

  /* N3276 magic doesn't apply to nested calls.  */
  tsubst_flags_t decltype_flag = (complain & tf_decltype);
  complain &= ~tf_decltype;
  /* No-Cleanup doesn't apply to nested calls either.  */
  tsubst_flags_t no_cleanup_complain = complain;
  complain &= ~tf_no_cleanup;

  /* Find maximum size of vector to hold converted arguments.  */
  parmlen = list_length (parm);
  nargs = vec_safe_length (args) + (first_arg != NULL_TREE ? 1 : 0);
  if (parmlen > nargs)
    nargs = parmlen;
  argarray = XALLOCAVEC (tree, nargs);

  in_consteval_if_p_temp_override icip;
  /* If the call is immediate function invocation, make sure
     taking address of immediate functions is allowed in its arguments.  */
  if (immediate_invocation_p (STRIP_TEMPLATE (fn)))
    in_consteval_if_p = true;

  int argarray_size = 0;
  unsigned int arg_index = 0;
  int conv_index = 0;
  int param_index = 0;

  auto consume_object_arg = [&arg_index, &first_arg, args]()
    {
      if (!first_arg)
	return (*args)[arg_index++];
      tree object_arg = first_arg;
      first_arg = NULL_TREE;
      return object_arg;
    };

  /* The implicit parameters to a constructor are not considered by overload
     resolution, and must be of the proper type.  */
  if (DECL_CONSTRUCTOR_P (fn))
    {
      tree object_arg = consume_object_arg ();
      argarray[argarray_size++] = build_this (object_arg);
      parm = TREE_CHAIN (parm);
      /* We should never try to call the abstract constructor.  */
      gcc_assert (!DECL_HAS_IN_CHARGE_PARM_P (fn));

      if (DECL_HAS_VTT_PARM_P (fn))
	{
	  argarray[argarray_size++] = (*args)[arg_index];
	  ++arg_index;
	  parm = TREE_CHAIN (parm);
	}
    }
  /* Bypass access control for 'this' parameter.  */
  else if (DECL_IOBJ_MEMBER_FUNCTION_P (fn))
    {
      tree arg = build_this (consume_object_arg ());
      tree argtype = TREE_TYPE (arg);

      if (arg == error_mark_node)
	return error_mark_node;
      if (convs[conv_index++]->bad_p)
	{
	  if (complain & tf_error)
	    {
	      auto_diagnostic_group d;
	      if (permerror (input_location, "passing %qT as %<this%> "
			     "argument discards qualifiers",
			     TREE_TYPE (argtype)))
		inform (DECL_SOURCE_LOCATION (fn), "  in call to %qD", fn);
	    }
	  else
	    return error_mark_node;
	}

      /* The class where FN is defined.  */
      tree ctx = DECL_CONTEXT (fn);

      /* See if the function member or the whole class type is declared
	 final and the call can be devirtualized.  */
      if (DECL_FINAL_P (fn) || CLASSTYPE_FINAL (ctx))
	flags |= LOOKUP_NONVIRTUAL;

      /* [class.mfct.non-static]: If a non-static member function of a class
	 X is called for an object that is not of type X, or of a type
	 derived from X, the behavior is undefined.

	 So we can assume that anything passed as 'this' is non-null, and
	 optimize accordingly.  */
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
	 must convert to the base.  */
      tree base_binfo = cand->conversion_path;
      if (BINFO_TYPE (base_binfo) != ctx)
	{
	  base_binfo = lookup_base (base_binfo, ctx, ba_unique, NULL, complain);
	  if (base_binfo == error_mark_node)
	    return error_mark_node;
	}

      /* If we know the dynamic type of the object, look up the final overrider
	 in the BINFO.  */
      if (DECL_VINDEX (fn) && (flags & LOOKUP_NONVIRTUAL) == 0
	  && resolves_to_fixed_type_p (arg))
	{
	  tree ov = lookup_vfn_in_binfo (DECL_VINDEX (fn), base_binfo);

	  /* And unwind base_binfo to match.  If we don't find the type we're
	     looking for in BINFO_INHERITANCE_CHAIN, we're looking at diamond
	     inheritance; for now do a normal virtual call in that case.  */
	  tree octx = DECL_CONTEXT (ov);
	  tree obinfo = base_binfo;
	  while (obinfo && !SAME_BINFO_TYPE_P (BINFO_TYPE (obinfo), octx))
	    obinfo = BINFO_INHERITANCE_CHAIN (obinfo);
	  if (obinfo)
	    {
	      fn = ov;
	      base_binfo = obinfo;
	      flags |= LOOKUP_NONVIRTUAL;
	    }
	}

      tree converted_arg = build_base_path (PLUS_EXPR, arg,
					    base_binfo, 1, complain);

      argarray[argarray_size++] = converted_arg;
      parm = TREE_CHAIN (parm);
    }

  auto handle_arg = [fn, flags](tree type,
				tree arg,
				int const param_index,
				conversion *conv,
				tsubst_flags_t const arg_complain)
    {
      /* Set user_conv_p on the argument conversions, so rvalue/base handling
	 knows not to allow any more UDCs.  This needs to happen after we
	 process cand->warnings.  */
      if (flags & LOOKUP_NO_CONVERSION)
	conv->user_conv_p = true;

      if (arg_complain & tf_warning)
	maybe_warn_pessimizing_move (arg, type, /*return_p=*/false);

      tree val = convert_like_with_context (conv, arg, fn,
					    param_index, arg_complain);
      val = convert_for_arg_passing (type, val, arg_complain);
      return val;
    };

  if (DECL_XOBJ_MEMBER_FUNCTION_P (fn))
    {
      gcc_assert (cand->num_convs > 0);
      tree object_arg = consume_object_arg ();
      val = handle_arg (TREE_VALUE (parm),
			object_arg,
			param_index++,
			convs[conv_index++],
			complain);

      if (val == error_mark_node)
	return error_mark_node;
      else
	argarray[argarray_size++] = val;
      parm = TREE_CHAIN (parm);
    }

  gcc_assert (first_arg == NULL_TREE);
  for (; arg_index < vec_safe_length (args) && parm;
       parm = TREE_CHAIN (parm), ++arg_index, ++param_index, ++conv_index)
    {
      tree current_arg = (*args)[arg_index];

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
      bool const conversion_warning = !(null_node_p (current_arg)
					&& DECL_TEMPLATE_INFO (fn)
					&& cand->template_decl
					&& !cand->explicit_targs);

      tsubst_flags_t const arg_complain
	= conversion_warning ? complain : complain & ~tf_warning;

      val = handle_arg (TREE_VALUE (parm),
			current_arg,
			param_index,
			convs[conv_index],
			arg_complain);

      if (val == error_mark_node)
	return error_mark_node;
      else
	argarray[argarray_size++] = val;
    }

  /* Default arguments */
  for (; parm && parm != void_list_node;
       parm = TREE_CHAIN (parm), param_index++)
    {
      if (TREE_VALUE (parm) == error_mark_node)
	return error_mark_node;
      val = convert_default_arg (TREE_VALUE (parm),
				 TREE_PURPOSE (parm),
				 fn, param_index,
				 complain);
      if (val == error_mark_node)
	return error_mark_node;
      argarray[argarray_size++] = val;
    }

  /* Ellipsis */
  int magic = magic_varargs_p (fn);
  for (; arg_index < vec_safe_length (args); ++arg_index)
    {
      tree a = (*args)[arg_index];
      if ((magic == 3 && arg_index == 2) || (magic == 4 && arg_index == 0))
	{
	  /* Do no conversions for certain magic varargs.  */
	  a = mark_type_use (a);
	  if (TREE_CODE (a) == FUNCTION_DECL && reject_gcc_builtin (a))
	    return error_mark_node;
	}
      else if (magic != 0)
	{
	  /* Don't truncate excess precision to the semantic type.  */
	  if (magic == 1 && TREE_CODE (a) == EXCESS_PRECISION_EXPR)
	    a = TREE_OPERAND (a, 0);
	  /* For other magic varargs only do decay_conversion.  */
	  a = decay_conversion (a, complain);
	}
      else if (DECL_CONSTRUCTOR_P (fn)
	       && same_type_ignoring_top_level_qualifiers_p (DECL_CONTEXT (fn),
							     TREE_TYPE (a)))
	{
	  /* Avoid infinite recursion trying to call A(...).  */
	  if (complain & tf_error)
	    /* Try to call the actual copy constructor for a good error.  */
	    call_copy_ctor (a, complain);
	  return error_mark_node;
	}
      else
	a = convert_arg_to_ellipsis (a, complain);
      if (a == error_mark_node)
	return error_mark_node;
      argarray[argarray_size++] = a;
    }

  gcc_assert (argarray_size <= nargs);
  nargs = argarray_size;
  icip.reset ();

  /* Avoid performing argument transformation if warnings are disabled.
     When tf_warning is set and at least one of the warnings is active
     the check_function_arguments function might warn about something.  */

  bool warned_p = false;
  if ((complain & tf_warning)
      && (warn_nonnull
	  || warn_format
	  || warn_suggest_attribute_format
	  || warn_restrict))
    {
      tree *fargs = (!nargs ? argarray
			    : (tree *) alloca (nargs * sizeof (tree)));
      for (int j = 0; j < nargs; j++)
	{
	  /* For -Wformat undo the implicit passing by hidden reference
	     done by convert_arg_to_ellipsis.  */
	  if (TREE_CODE (argarray[j]) == ADDR_EXPR
	      && TYPE_REF_P (TREE_TYPE (argarray[j])))
	    fargs[j] = TREE_OPERAND (argarray[j], 0);
	  else
	    fargs[j] = argarray[j];
	}

      warned_p = check_function_arguments (input_location, fn, TREE_TYPE (fn),
					   nargs, fargs, NULL);
    }

  if (DECL_INHERITED_CTOR (fn))
    {
      /* Check for passing ellipsis arguments to an inherited constructor.  We
	 could handle this by open-coding the inherited constructor rather than
	 defining it, but let's not bother now.  */
      if (!cp_unevaluated_operand
	  && cand->num_convs
	  && cand->convs[cand->num_convs-1]->ellipsis_p)
	{
	  if (complain & tf_error)
	    {
	      sorry ("passing arguments to ellipsis of inherited constructor "
		     "%qD", cand->fn);
	      inform (DECL_SOURCE_LOCATION (cand->fn), "declared here");
	    }
	  return error_mark_node;
	}

      /* A base constructor inheriting from a virtual base doesn't get the
	 inherited arguments, just this and __vtt.  */
      if (ctor_omit_inherited_parms (fn))
	nargs = 2;
    }

  /* Avoid actually calling copy constructors and copy assignment operators,
     if possible.  */

  if (!force_elide
      && (!flag_elide_constructors
	  /* It's unsafe to elide the operation when handling
	     a noexcept-expression, it may evaluate to the wrong
	     value (c++/53025, c++/96090).  */
	  || cp_noexcept_operand != 0))
    /* Do things the hard way.  */;
  else if (cand->num_convs == 1
	   && (DECL_COPY_CONSTRUCTOR_P (fn)
	       || DECL_MOVE_CONSTRUCTOR_P (fn)))
    {
      tree targ;
      tree arg = argarray[num_artificial_parms_for (fn)];
      tree fa = argarray[0];
      bool trivial = trivial_fn_p (fn);

      /* Pull out the real argument, disregarding const-correctness.  */
      targ = arg;
      /* Strip the reference binding for the constructor parameter.  */
      if (CONVERT_EXPR_P (targ)
	  && TYPE_REF_P (TREE_TYPE (targ)))
	targ = TREE_OPERAND (targ, 0);
      /* But don't strip any other reference bindings; binding a temporary to a
	 reference prevents copy elision.  */
      while ((CONVERT_EXPR_P (targ)
	      && !TYPE_REF_P (TREE_TYPE (targ)))
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
	arg = cp_build_fold_indirect_ref (arg);

      /* In C++17 we shouldn't be copying a TARGET_EXPR except into a
	 potentially-overlapping subobject.  */
      if (CHECKING_P && cxx_dialect >= cxx17)
	gcc_assert (TREE_CODE (arg) != TARGET_EXPR
		    || force_elide
		    /* It's from binding the ref parm to a packed field. */
		    || convs[0]->need_temporary_p
		    || seen_error ()
		    /* See unsafe_copy_elision_p.  */
		    || unsafe_return_slot_p (fa));

      bool unsafe = unsafe_copy_elision_p_opt (fa, arg);
      bool eliding_temp = (TREE_CODE (arg) == TARGET_EXPR && !unsafe);

      /* [class.copy]: the copy constructor is implicitly defined even if the
	 implementation elided its use.  But don't warn about deprecation when
	 eliding a temporary, as then no copy is actually performed.  */
      warning_sentinel s (warn_deprecated_copy, eliding_temp);
      if (force_elide)
	/* The language says this isn't called.  */;
      else if (!trivial)
	{
	  if (!mark_used (fn, complain) && !(complain & tf_error))
	    return error_mark_node;
	  already_used = true;
	}
      else
	cp_handle_deprecated_or_unavailable (fn, complain);

      if (eliding_temp && DECL_BASE_CONSTRUCTOR_P (fn)
	  && !make_base_init_ok (arg))
	unsafe = true;

      /* If we're creating a temp and we already have one, don't create a
	 new one.  If we're not creating a temp but we get one, use
	 INIT_EXPR to collapse the temp into our target.  Otherwise, if the
	 ctor is trivial, do a bitwise copy with a simple TARGET_EXPR for a
	 temp or an INIT_EXPR otherwise.  */
      if (is_dummy_object (fa))
	{
	  if (TREE_CODE (arg) == TARGET_EXPR)
	    return arg;
	  else if (trivial)
	    return force_target_expr (DECL_CONTEXT (fn), arg, complain);
	}
      else if ((trivial || TREE_CODE (arg) == TARGET_EXPR)
	       && !unsafe)
	{
	  tree to = cp_build_fold_indirect_ref (fa);
	  val = cp_build_init_expr (to, arg);
	  return val;
	}
    }
  else if (DECL_ASSIGNMENT_OPERATOR_P (fn)
	   && DECL_OVERLOADED_OPERATOR_IS (fn, NOP_EXPR)
	   && trivial_fn_p (fn))
    {
      tree to = cp_build_fold_indirect_ref (argarray[0]);
      tree type = TREE_TYPE (to);
      tree as_base = CLASSTYPE_AS_BASE (type);
      tree arg = argarray[1];
      location_t loc = cp_expr_loc_or_input_loc (arg);

      if (is_really_empty_class (type, /*ignore_vptr*/true))
	{
	  /* Avoid copying empty classes, but ensure op= returns an lvalue even
	     if the object argument isn't one. This isn't needed in other cases
	     since MODIFY_EXPR is always considered an lvalue.  */
	  to = cp_build_addr_expr (to, tf_none);
	  to = cp_build_indirect_ref (input_location, to, RO_ARROW, complain);
	  val = build2 (COMPOUND_EXPR, type, arg, to);
	  suppress_warning (val, OPT_Wunused);
	}
      else if (tree_int_cst_equal (TYPE_SIZE (type), TYPE_SIZE (as_base)))
	{
	  if (is_std_init_list (type)
	      && conv_binds_ref_to_prvalue (convs[1]))
	    warning_at (loc, OPT_Winit_list_lifetime,
			"assignment from temporary %<initializer_list%> does "
			"not extend the lifetime of the underlying array");
	  arg = cp_build_fold_indirect_ref (arg);
	  val = build2 (MODIFY_EXPR, TREE_TYPE (to), to, arg);
	}
      else
	{
	  /* We must only copy the non-tail padding parts.  */
	  tree arg0, arg2, t;
	  tree array_type, alias_set;

	  arg2 = TYPE_SIZE_UNIT (as_base);
	  to = cp_stabilize_reference (to);
	  arg0 = cp_build_addr_expr (to, complain);

	  array_type = build_array_type (unsigned_char_type_node,
					 build_index_type
					   (size_binop (MINUS_EXPR,
							arg2, size_int (1))));
	  alias_set = build_int_cst (build_pointer_type (type), 0);
	  t = build2 (MODIFY_EXPR, void_type_node,
		      build2 (MEM_REF, array_type, arg0, alias_set),
		      build2 (MEM_REF, array_type, arg, alias_set));
	  val = build2 (COMPOUND_EXPR, TREE_TYPE (to), t, to);
          suppress_warning (val, OPT_Wunused);
	}

      cp_handle_deprecated_or_unavailable (fn, complain);

      return val;
    }
  else if (trivial_fn_p (fn))
    {
      if (DECL_DESTRUCTOR_P (fn))
	return build_trivial_dtor_call (argarray[0]);
      else if (default_ctor_p (fn))
	{
	  if (is_dummy_object (argarray[0]))
	    return force_target_expr (DECL_CONTEXT (fn), void_node,
				      no_cleanup_complain);
	  else
	    return cp_build_fold_indirect_ref (argarray[0]);
	}
    }

  gcc_assert (!force_elide);

  if (!already_used
      && !mark_used (fn, complain))
    return error_mark_node;

  /* Warn if the built-in writes to an object of a non-trivial type.  */
  if (warn_class_memaccess
      && vec_safe_length (args) >= 2
      && DECL_BUILT_IN_CLASS (fn) == BUILT_IN_NORMAL)
    maybe_warn_class_memaccess (input_location, fn, args);

  if (DECL_VINDEX (fn) && (flags & LOOKUP_NONVIRTUAL) == 0)
    {
      tree t;
      tree binfo = lookup_base (TREE_TYPE (TREE_TYPE (argarray[0])),
				DECL_CONTEXT (fn),
				ba_any, NULL, complain);
      gcc_assert (binfo && binfo != error_mark_node);

      argarray[0] = build_base_path (PLUS_EXPR, argarray[0], binfo, 1,
				     complain);
      if (TREE_SIDE_EFFECTS (argarray[0]))
	argarray[0] = save_expr (argarray[0]);
      t = build_pointer_type (TREE_TYPE (fn));
      fn = build_vfn_ref (argarray[0], DECL_VINDEX (fn));
      TREE_TYPE (fn) = t;
    }
  else
    {
      /* If FN is marked deprecated or unavailable, then we've already
	 issued a diagnostic from mark_used above, so avoid redundantly
	 issuing another one from build_addr_func.  */
      auto w = make_temp_override (deprecated_state,
				   UNAVAILABLE_DEPRECATED_SUPPRESS);

      fn = build_addr_func (fn, complain);
      if (fn == error_mark_node)
	return error_mark_node;

      /* We're actually invoking the function.  (Immediate functions get an
	 & when invoking it even though the user didn't use &.)  */
      ADDR_EXPR_DENOTES_CALL_P (fn) = true;
    }

  tree call = build_cxx_call (fn, nargs, argarray, complain|decltype_flag);
  if (call == error_mark_node)
    return call;
  if (cand->flags & LOOKUP_LIST_INIT_CTOR)
    {
      tree c = extract_call_expr (call);
      /* build_new_op will clear this when appropriate.  */
      CALL_EXPR_ORDERED_ARGS (c) = true;
    }
  if (warned_p)
    {
      tree c = extract_call_expr (call);
      if (TREE_CODE (c) == CALL_EXPR)
	suppress_warning (c /* Suppress all warnings.  */);
    }

  return call;
}

namespace
{

/* Return the DECL of the first non-static subobject of class TYPE
   that satisfies the predicate PRED or null if none can be found.  */

template <class Predicate>
tree
first_non_static_field (tree type, Predicate pred)
{
  if (!type || !CLASS_TYPE_P (type))
    return NULL_TREE;

  for (tree field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
    {
      if (TREE_CODE (field) != FIELD_DECL)
	continue;
      if (TREE_STATIC (field))
	continue;
      if (pred (field))
	return field;
    }

  int i = 0;

  for (tree base_binfo, binfo = TYPE_BINFO (type);
       BINFO_BASE_ITERATE (binfo, i, base_binfo); i++)
    {
      tree base = TREE_TYPE (base_binfo);
      if (pred (base))
	return base;
      if (tree field = first_non_static_field (base, pred))
	return field;
    }

  return NULL_TREE;
}

struct NonPublicField
{
  bool operator() (const_tree t) const
  {
    return DECL_P (t) && (TREE_PRIVATE (t) || TREE_PROTECTED (t));
  }
};

/* Return the DECL of the first non-public subobject of class TYPE
   or null if none can be found.  */

static inline tree
first_non_public_field (tree type)
{
  return first_non_static_field (type, NonPublicField ());
}

struct NonTrivialField
{
  bool operator() (const_tree t) const
  {
    return !trivial_type_p (DECL_P (t) ? TREE_TYPE (t) : t);
  }
};

/* Return the DECL of the first non-trivial subobject of class TYPE
   or null if none can be found.  */

static inline tree
first_non_trivial_field (tree type)
{
  return first_non_static_field (type, NonTrivialField ());
}

}   /* unnamed namespace */

/* Return true if all copy and move assignment operator overloads for
   class TYPE are trivial and at least one of them is not deleted and,
   when ACCESS is set, accessible.  Return false otherwise.  Set
   HASASSIGN to true when the TYPE has a (not necessarily trivial)
   copy or move assignment.  */

static bool
has_trivial_copy_assign_p (tree type, bool access, bool *hasassign)
{
  tree fns = get_class_binding (type, assign_op_identifier);
  bool all_trivial = true;

  /* Iterate over overloads of the assignment operator, checking
     accessible copy assignments for triviality.  */

  for (tree f : ovl_range (fns))
    {
      /* Skip operators that aren't copy assignments.  */
      if (!copy_fn_p (f))
	continue;

      bool accessible = (!access || !(TREE_PRIVATE (f) || TREE_PROTECTED (f))
			 || accessible_p (TYPE_BINFO (type), f, true));

      /* Skip template assignment operators and deleted functions.  */
      if (TREE_CODE (f) != FUNCTION_DECL || DECL_DELETED_FN (f))
	continue;

      if (accessible)
	*hasassign = true;

      if (!accessible || !trivial_fn_p (f))
	all_trivial = false;

      /* Break early when both properties have been determined.  */
      if (*hasassign && !all_trivial)
	break;
    }

  /* Return true if they're all trivial and one of the expressions
     TYPE() = TYPE() or TYPE() = (TYPE&)() is valid.  */
  tree ref = cp_build_reference_type (type, false);
  return (all_trivial
	  && (is_trivially_xible (MODIFY_EXPR, type, type)
	      || is_trivially_xible (MODIFY_EXPR, type, ref)));
}

/* Return true if all copy and move ctor overloads for class TYPE are
   trivial and at least one of them is not deleted and, when ACCESS is
   set, accessible.  Return false otherwise.  Set each element of HASCTOR[]
   to true when the TYPE has a (not necessarily trivial) default and copy
   (or move) ctor, respectively.  */

static bool
has_trivial_copy_p (tree type, bool access, bool hasctor[2])
{
  tree fns = get_class_binding (type, complete_ctor_identifier);
  bool all_trivial = true;

  for (tree f : ovl_range (fns))
    {
      /* Skip template constructors.  */
      if (TREE_CODE (f) != FUNCTION_DECL)
	continue;

      bool cpy_or_move_ctor_p = copy_fn_p (f);

      /* Skip ctors other than default, copy, and move.  */
      if (!cpy_or_move_ctor_p && !default_ctor_p (f))
	continue;

      if (DECL_DELETED_FN (f))
	continue;

      bool accessible = (!access || !(TREE_PRIVATE (f) || TREE_PROTECTED (f))
			 || accessible_p (TYPE_BINFO (type), f, true));

      if (accessible)
	hasctor[cpy_or_move_ctor_p] = true;

      if (cpy_or_move_ctor_p && (!accessible || !trivial_fn_p (f)))
	all_trivial = false;

      /* Break early when both properties have been determined.  */
      if (hasctor[0] && hasctor[1] && !all_trivial)
	break;
    }

  return all_trivial;
}

/* Issue a warning on a call to the built-in function FNDECL if it is
   a raw memory write whose destination is not an object of (something
   like) trivial or standard layout type with a non-deleted assignment
   and copy ctor.  Detects const correctness violations, corrupting
   references, virtual table pointers, and bypassing non-trivial
   assignments.  */

static void
maybe_warn_class_memaccess (location_t loc, tree fndecl,
			    const vec<tree, va_gc> *args)
{
  /* Except for bcopy where it's second, the destination pointer is
     the first argument for all functions handled here.  Compute
     the index of the destination and source arguments.  */
  unsigned dstidx = DECL_FUNCTION_CODE (fndecl) == BUILT_IN_BCOPY;
  unsigned srcidx = !dstidx;

  tree dest = (*args)[dstidx];
  if (!TREE_TYPE (dest)
      || (TREE_CODE (TREE_TYPE (dest)) != ARRAY_TYPE
	  && !INDIRECT_TYPE_P (TREE_TYPE (dest))))
    return;

  tree srctype = NULL_TREE;

  /* Determine the type of the pointed-to object and whether it's
     a complete class type.  */
  tree desttype = TREE_TYPE (TREE_TYPE (dest));

  if (!desttype || !COMPLETE_TYPE_P (desttype) || !CLASS_TYPE_P (desttype))
    return;

  /* Check to see if the raw memory call is made by a non-static member
     function with THIS as the destination argument for the destination
     type.  If so, and if the class has no non-trivial bases or members,
     be more permissive.  */
  if (current_function_decl
      && DECL_OBJECT_MEMBER_FUNCTION_P (current_function_decl)
      && is_object_parameter (tree_strip_nop_conversions (dest)))
    {
      tree ctx = DECL_CONTEXT (current_function_decl);
      bool special = same_type_ignoring_top_level_qualifiers_p (ctx, desttype);
      tree binfo = TYPE_BINFO (ctx);

      if (special
	  && !BINFO_VTABLE (binfo)
	  && !first_non_trivial_field (desttype))
	return;
    }

  /* True if the class is trivial.  */
  bool trivial = trivial_type_p (desttype);

  /* Set to true if DESTYPE has an accessible copy assignment.  */
  bool hasassign = false;
  /* True if all of the class' overloaded copy assignment operators
     are all trivial (and not deleted) and at least one of them is
     accessible.  */
  bool trivassign = has_trivial_copy_assign_p (desttype, true, &hasassign);

  /* Set to true if DESTTYPE has an accessible default and copy ctor,
     respectively.  */
  bool hasctors[2] = { false, false };

  /* True if all of the class' overloaded copy constructors are all
     trivial (and not deleted) and at least one of them is accessible.  */
  bool trivcopy = has_trivial_copy_p (desttype, true, hasctors);

  /* Set FLD to the first private/protected member of the class.  */
  tree fld = trivial ? first_non_public_field (desttype) : NULL_TREE;

  /* The warning format string.  */
  const char *warnfmt = NULL;
  /* A suggested alternative to offer instead of the raw memory call.
     Empty string when none can be come up with.  */
  const char *suggest = "";
  bool warned = false;

  switch (DECL_FUNCTION_CODE (fndecl))
    {
    case BUILT_IN_MEMSET:
      if (!integer_zerop (maybe_constant_value ((*args)[1])))
	{
	  /* Diagnose setting non-copy-assignable or non-trivial types,
	     or types with a private member, to (potentially) non-zero
	     bytes.  Since the value of the bytes being written is unknown,
	     suggest using assignment instead (if one exists).  Also warn
	     for writes into objects for which zero-initialization doesn't
	     mean all bits clear (pointer-to-member data, where null is all
	     bits set).  Since the value being written is (most likely)
	     non-zero, simply suggest assignment (but not copy assignment).  */
	  suggest = "; use assignment instead";
	  if (!trivassign)
	    warnfmt = G_("%qD writing to an object of type %#qT with "
			 "no trivial copy-assignment");
	  else if (!trivial)
	    warnfmt = G_("%qD writing to an object of non-trivial type %#qT%s");
	  else if (fld)
	    {
	      const char *access = TREE_PRIVATE (fld) ? "private" : "protected";
	      warned = warning_at (loc, OPT_Wclass_memaccess,
				   "%qD writing to an object of type %#qT with "
				   "%qs member %qD",
				   fndecl, desttype, access, fld);
	    }
	  else if (!zero_init_p (desttype))
	    warnfmt = G_("%qD writing to an object of type %#qT containing "
			 "a pointer to data member%s");

	  break;
	}
      /* Fall through.  */

    case BUILT_IN_BZERO:
      /* Similarly to the above, diagnose clearing non-trivial or non-
	 standard layout objects, or objects of types with no assignmenmt.
	 Since the value being written is known to be zero, suggest either
	 copy assignment, copy ctor, or default ctor as an alternative,
	 depending on what's available.  */

      if (hasassign && hasctors[0])
	suggest = G_("; use assignment or value-initialization instead");
      else if (hasassign)
	suggest = G_("; use assignment instead");
      else if (hasctors[0])
	suggest = G_("; use value-initialization instead");

      if (!trivassign)
	warnfmt = G_("%qD clearing an object of type %#qT with "
		     "no trivial copy-assignment%s");
      else if (!trivial)
	warnfmt =  G_("%qD clearing an object of non-trivial type %#qT%s");
      else if (!zero_init_p (desttype))
	warnfmt = G_("%qD clearing an object of type %#qT containing "
		     "a pointer-to-member%s");
      break;

    case BUILT_IN_BCOPY:
    case BUILT_IN_MEMCPY:
    case BUILT_IN_MEMMOVE:
    case BUILT_IN_MEMPCPY:
      /* Determine the type of the source object.  */
      srctype = TREE_TYPE ((*args)[srcidx]);
      if (!srctype || !INDIRECT_TYPE_P (srctype))
	srctype = void_type_node;
      else
	srctype = TREE_TYPE (srctype);

      /* Since it's impossible to determine wheter the byte copy is
	 being used in place of assignment to an existing object or
	 as a substitute for initialization, assume it's the former.
	 Determine the best alternative to use instead depending on
	 what's not deleted.  */
      if (hasassign && hasctors[1])
	suggest = G_("; use copy-assignment or copy-initialization instead");
      else if (hasassign)
	suggest = G_("; use copy-assignment instead");
      else if (hasctors[1])
	suggest = G_("; use copy-initialization instead");

      if (!trivassign)
	warnfmt = G_("%qD writing to an object of type %#qT with no trivial "
		     "copy-assignment%s");
      else if (!trivially_copyable_p (desttype))
	warnfmt = G_("%qD writing to an object of non-trivially copyable "
		     "type %#qT%s");
      else if (!trivcopy)
	warnfmt = G_("%qD writing to an object with a deleted copy constructor");

      else if (!trivial
	       && !VOID_TYPE_P (srctype)
	       && !is_byte_access_type (srctype)
	       && !same_type_ignoring_top_level_qualifiers_p (desttype,
							      srctype))
	{
	  /* Warn when copying into a non-trivial object from an object
	     of a different type other than void or char.  */
	  warned = warning_at (loc, OPT_Wclass_memaccess,
			       "%qD copying an object of non-trivial type "
			       "%#qT from an array of %#qT",
			       fndecl, desttype, srctype);
	}
      else if (fld
	       && !VOID_TYPE_P (srctype)
	       && !is_byte_access_type (srctype)
	       && !same_type_ignoring_top_level_qualifiers_p (desttype,
							      srctype))
	{
	  const char *access = TREE_PRIVATE (fld) ? "private" : "protected";
	  warned = warning_at (loc, OPT_Wclass_memaccess,
			       "%qD copying an object of type %#qT with "
			       "%qs member %qD from an array of %#qT; use "
			       "assignment or copy-initialization instead",
			       fndecl, desttype, access, fld, srctype);
	}
      else if (!trivial && vec_safe_length (args) > 2)
	{
	  tree sz = maybe_constant_value ((*args)[2]);
	  if (!tree_fits_uhwi_p (sz))
	    break;

	  /* Finally, warn on partial copies.  */
	  unsigned HOST_WIDE_INT typesize
	    = tree_to_uhwi (TYPE_SIZE_UNIT (desttype));
	  if (typesize == 0)
	    break;
	  if (unsigned HOST_WIDE_INT partial = tree_to_uhwi (sz) % typesize)
	    warned = warning_at (loc, OPT_Wclass_memaccess,
				 (typesize - partial > 1
				  ? G_("%qD writing to an object of "
				       "a non-trivial type %#qT leaves %wu "
				       "bytes unchanged")
				  : G_("%qD writing to an object of "
				       "a non-trivial type %#qT leaves %wu "
				       "byte unchanged")),
				 fndecl, desttype, typesize - partial);
	}
      break;

    case BUILT_IN_REALLOC:

      if (!trivially_copyable_p (desttype))
	warnfmt = G_("%qD moving an object of non-trivially copyable type "
		     "%#qT; use %<new%> and %<delete%> instead");
      else if (!trivcopy)
	warnfmt = G_("%qD moving an object of type %#qT with deleted copy "
		     "constructor; use %<new%> and %<delete%> instead");
      else if (!get_dtor (desttype, tf_none))
	warnfmt = G_("%qD moving an object of type %#qT with deleted "
		     "destructor");
      else if (!trivial)
	{
	  tree sz = maybe_constant_value ((*args)[1]);
	  if (TREE_CODE (sz) == INTEGER_CST
	      && tree_int_cst_lt (sz, TYPE_SIZE_UNIT (desttype)))
	    /* Finally, warn on reallocation into insufficient space.  */
	    warned = warning_at (loc, OPT_Wclass_memaccess,
				 "%qD moving an object of non-trivial type "
				 "%#qT and size %E into a region of size %E",
				 fndecl, desttype, TYPE_SIZE_UNIT (desttype),
				 sz);
	}
      break;

    default:
      return;
    }

  if (warnfmt)
    {
      if (suggest)
	warned = warning_at (loc, OPT_Wclass_memaccess,
			     warnfmt, fndecl, desttype, suggest);
      else
	warned = warning_at (loc, OPT_Wclass_memaccess,
			     warnfmt, fndecl, desttype);
    }

  if (warned)
    inform (location_of (desttype), "%#qT declared here", desttype);
}

/* Build and return a call to FN, using NARGS arguments in ARGARRAY.
   If FN is the result of resolving an overloaded target built-in,
   ORIG_FNDECL is the original function decl, otherwise it is null.
   This function performs no overload resolution, conversion, or other
   high-level operations.  */

tree
build_cxx_call (tree fn, int nargs, tree *argarray,
		tsubst_flags_t complain, tree orig_fndecl)
{
  tree fndecl;

  /* Remember roughly where this call is.  */
  location_t loc = cp_expr_loc_or_input_loc (fn);
  fn = build_call_a (fn, nargs, argarray);
  SET_EXPR_LOCATION (fn, loc);

  fndecl = get_callee_fndecl (fn);
  if (!orig_fndecl)
    orig_fndecl = fndecl;

  /* Check that arguments to builtin functions match the expectations.  */
  if (fndecl
      && !processing_template_decl
      && fndecl_built_in_p (fndecl))
    {
      int i;

      /* We need to take care that values to BUILT_IN_NORMAL
         are reduced.  */
      for (i = 0; i < nargs; i++)
	argarray[i] = maybe_constant_value (argarray[i]);

      if (!check_builtin_function_arguments (EXPR_LOCATION (fn), vNULL, fndecl,
					     orig_fndecl, nargs, argarray))
	return error_mark_node;
      else if (fndecl_built_in_p (fndecl, BUILT_IN_CLEAR_PADDING))
	{
	  tree arg0 = argarray[0];
	  STRIP_NOPS (arg0);
	  if (TREE_CODE (arg0) == ADDR_EXPR
	      && DECL_P (TREE_OPERAND (arg0, 0))
	      && same_type_ignoring_top_level_qualifiers_p
			(TREE_TYPE (TREE_TYPE (argarray[0])),
			 TREE_TYPE (TREE_TYPE (arg0))))
	    /* For __builtin_clear_padding (&var) we know the type
	       is for a complete object, so there is no risk in clearing
	       padding that is reused in some derived class member.  */;
	  else if (!trivially_copyable_p (TREE_TYPE (TREE_TYPE (argarray[0]))))
	    {
	      error_at (EXPR_LOC_OR_LOC (argarray[0], input_location),
			"argument %u in call to function %qE "
			"has pointer to a non-trivially-copyable type (%qT)",
			1, fndecl, TREE_TYPE (argarray[0]));
	      return error_mark_node;
	    }
	}
    }

  if (VOID_TYPE_P (TREE_TYPE (fn)))
    return fn;

  /* 5.2.2/11: If a function call is a prvalue of object type: if the
     function call is either the operand of a decltype-specifier or the
     right operand of a comma operator that is the operand of a
     decltype-specifier, a temporary object is not introduced for the
     prvalue. The type of the prvalue may be incomplete.  */
  if (!(complain & tf_decltype))
    {
      fn = require_complete_type (fn, complain);
      if (fn == error_mark_node)
	return error_mark_node;

      if (MAYBE_CLASS_TYPE_P (TREE_TYPE (fn)))
	{
	  fn = build_cplus_new (TREE_TYPE (fn), fn, complain);
	  maybe_warn_parm_abi (TREE_TYPE (fn), loc);
	}
    }
  return convert_from_reference (fn);
}

/* Returns the value to use for the in-charge parameter when making a
   call to a function with the indicated NAME.

   FIXME:Can't we find a neater way to do this mapping?  */

tree
in_charge_arg_for_name (tree name)
{
  if (IDENTIFIER_CTOR_P (name))
    {
      if (name == complete_ctor_identifier)
	return integer_one_node;
      gcc_checking_assert (name == base_ctor_identifier);
    }
  else
    {
      if (name == complete_dtor_identifier)
	return integer_two_node;
      else if (name == deleting_dtor_identifier)
	/* The deleting dtor should now be handled by
	   build_delete_destructor_body.  */
	gcc_unreachable ();
      gcc_checking_assert (name == base_dtor_identifier);
    }

  return integer_zero_node;
}

/* We've built up a constructor call RET.  Complain if it delegates to the
   constructor we're currently compiling.  */

static void
check_self_delegation (tree ret)
{
  if (TREE_CODE (ret) == TARGET_EXPR)
    ret = TARGET_EXPR_INITIAL (ret);
  tree fn = cp_get_callee_fndecl_nofold (ret);
  if (fn && DECL_ABSTRACT_ORIGIN (fn) == current_function_decl)
    error ("constructor delegates to itself");
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

  gcc_assert (IDENTIFIER_CDTOR_P (name) || name == assign_op_identifier);

  if (error_operand_p (instance))
    return error_mark_node;

  if (IDENTIFIER_DTOR_P (name))
    {
      gcc_assert (args == NULL || vec_safe_is_empty (*args));
      if (!type_build_dtor_call (TREE_TYPE (instance)))
	/* Shortcut to avoid lazy destructor declaration.  */
	return build_trivial_dtor_call (instance);
    }

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
      /* Convert to the base class, if necessary.  */
      if (!same_type_ignoring_top_level_qualifiers_p
	  (TREE_TYPE (instance), BINFO_TYPE (binfo)))
	{
	  if (IDENTIFIER_CDTOR_P (name))
	    /* For constructors and destructors, either the base is
	       non-virtual, or it is virtual but we are doing the
	       conversion from a constructor or destructor for the
	       complete object.  In either case, we can convert
	       statically.  */
	    instance = convert_to_base_statically (instance, binfo);
	  else
	    {
	      /* However, for assignment operators, we must convert
		 dynamically if the base is virtual.  */
	      gcc_checking_assert (name == assign_op_identifier);
	      instance = build_base_path (PLUS_EXPR, instance,
					  binfo, /*nonnull=*/1, complain);
	    }
	}
    }

  gcc_assert (instance != NULL_TREE);

  /* In C++17, "If the initializer expression is a prvalue and the
     cv-unqualified version of the source type is the same class as the class
     of the destination, the initializer expression is used to initialize the
     destination object."  Handle that here to avoid doing overload
     resolution.  */
  if (cxx_dialect >= cxx17
      && args && vec_safe_length (*args) == 1
      && !unsafe_return_slot_p (instance))
    {
      tree arg = (**args)[0];

      if (BRACE_ENCLOSED_INITIALIZER_P (arg)
	  && !TYPE_HAS_LIST_CTOR (class_type)
	  && !CONSTRUCTOR_IS_DESIGNATED_INIT (arg)
	  && CONSTRUCTOR_NELTS (arg) == 1)
	arg = CONSTRUCTOR_ELT (arg, 0)->value;

      if ((TREE_CODE (arg) == TARGET_EXPR
	   || TREE_CODE (arg) == CONSTRUCTOR)
	  && (same_type_ignoring_top_level_qualifiers_p
	      (class_type, TREE_TYPE (arg))))
	{
	  if (is_dummy_object (instance))
	    return arg;
	  else if (TREE_CODE (arg) == TARGET_EXPR)
	    TARGET_EXPR_DIRECT_INIT_P (arg) = true;

	  if ((complain & tf_error)
	      && (flags & LOOKUP_DELEGATING_CONS))
	    check_self_delegation (arg);
	  /* Avoid change of behavior on Wunused-var-2.C.  */
	  instance = mark_lvalue_use (instance);
	  return cp_build_init_expr (instance, arg);
	}
    }

  fns = lookup_fnfields (binfo, name, 1, complain);

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
      vtt = build_if_in_charge (vtt, current_vtt_parm);
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
      && name == complete_ctor_identifier)
    check_self_delegation (ret);

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
  const char *pretty_name;

  /* Assume that we will not allocate memory.  */
  *free_p = false;
  /* Constructors and destructors are special.  */
  if (IDENTIFIER_CDTOR_P (name))
    {
      pretty_name
	= identifier_to_locale (IDENTIFIER_POINTER (constructor_name (type)));
      /* For a destructor, add the '~'.  */
      if (IDENTIFIER_DTOR_P (name))
	{
	  pretty_name = concat ("~", pretty_name, NULL);
	  /* Remember that we need to free the memory allocated.  */
	  *free_p = true;
	}
    }
  else if (IDENTIFIER_CONV_OP_P (name))
    {
      pretty_name = concat ("operator ",
			    type_as_string_translate (TREE_TYPE (name),
						      TFF_PLAIN_IDENTIFIER),
			    NULL);
      /* Remember that we need to free the memory allocated.  */
      *free_p = true;
    }
  else
    pretty_name = identifier_to_locale (IDENTIFIER_POINTER (name));

  return CONST_CAST (char *, pretty_name);
}

/* If CANDIDATES contains exactly one candidate, return it, otherwise
   return NULL.  */

static z_candidate *
single_z_candidate (z_candidate *candidates)
{
  if (candidates == NULL)
    return NULL;

  if (candidates->next)
    return NULL;

  return candidates;
}

/* If CANDIDATE is invalid due to a bad argument type, return the
   pertinent conversion_info.

   Otherwise, return NULL.  */

static const conversion_info *
maybe_get_bad_conversion_for_unmatched_call (const z_candidate *candidate)
{
  /* Must be an rr_arg_conversion or rr_bad_arg_conversion.  */
  rejection_reason *r = candidate->reason;

  if (r == NULL)
    return NULL;

  switch (r->code)
    {
    default:
      return NULL;

    case rr_arg_conversion:
      return &r->u.conversion;

    case rr_bad_arg_conversion:
      return &r->u.bad_conversion;
    }
}

/* Issue an error and note complaining about a bad argument type at a
   callsite with a single candidate FNDECL.

   ARG_LOC is the location of the argument (or UNKNOWN_LOCATION, in which
   case input_location is used).
   FROM_TYPE is the type of the actual argument; TO_TYPE is the type of
   the formal parameter.  */

void
complain_about_bad_argument (location_t arg_loc,
			     tree from_type, tree to_type,
			     tree fndecl, int parmnum)
{
  auto_diagnostic_group d;
  range_label_for_type_mismatch rhs_label (from_type, to_type);
  range_label *label = &rhs_label;
  if (arg_loc == UNKNOWN_LOCATION)
    {
      arg_loc = input_location;
      label = NULL;
    }
  gcc_rich_location richloc (arg_loc, label);
  error_at (&richloc,
	    "cannot convert %qH to %qI",
	    from_type, to_type);
  maybe_inform_about_fndecl_for_bogus_argument_init (fndecl,
						     parmnum);
}

/* Subroutine of build_new_method_call_1, for where there are no viable
   candidates for the call.  */

static void
complain_about_no_candidates_for_method_call (tree instance,
					      z_candidate *candidates,
					      tree explicit_targs,
					      tree basetype,
					      tree optype, tree name,
					      bool skip_first_for_error,
					      vec<tree, va_gc> *user_args)
{
  auto_diagnostic_group d;
  if (!COMPLETE_OR_OPEN_TYPE_P (basetype))
    cxx_incomplete_type_error (instance, basetype);
  else if (optype)
    error ("no matching function for call to %<%T::operator %T(%A)%#V%>",
	   basetype, optype, build_tree_list_vec (user_args),
	   TREE_TYPE (instance));
  else
    {
      /* Special-case for when there's a single candidate that's failing
	 due to a bad argument type.  */
      if (z_candidate *candidate = single_z_candidate (candidates))
	  if (const conversion_info *conv
		= maybe_get_bad_conversion_for_unmatched_call (candidate))
	    {
	      tree from_type = conv->from;
	      if (!TYPE_P (conv->from))
		from_type = lvalue_type (conv->from);
	      complain_about_bad_argument (conv->loc,
					   from_type, conv->to_type,
					   candidate->fn, conv->n_arg);
	      return;
	    }

      tree arglist = build_tree_list_vec (user_args);
      tree errname = name;
      bool twiddle = false;
      if (IDENTIFIER_CDTOR_P (errname))
	{
	  twiddle = IDENTIFIER_DTOR_P (errname);
	  errname = constructor_name (basetype);
	}
      if (explicit_targs)
	errname = lookup_template_function (errname, explicit_targs);
      if (skip_first_for_error)
	arglist = TREE_CHAIN (arglist);
      error ("no matching function for call to %<%T::%s%E(%A)%#V%>",
	     basetype, &"~"[!twiddle], errname, arglist,
	     TREE_TYPE (instance));
    }
  print_z_candidates (location_of (name), candidates);
}

/* Build a call to "INSTANCE.FN (ARGS)".  If FN_P is non-NULL, it will
   be set, upon return, to the function called.  ARGS may be NULL.
   This may change ARGS.  */

tree
build_new_method_call (tree instance, tree fns, vec<tree, va_gc> **args,
		       tree conversion_path, int flags,
		       tree *fn_p, tsubst_flags_t complain)
{
  struct z_candidate *candidates = 0, *cand;
  tree explicit_targs = NULL_TREE;
  tree basetype = NULL_TREE;
  tree access_binfo;
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

  auto_cond_timevar tv (TV_OVERLOAD);

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
  optype = BASELINK_OPTYPE (fns);
  fns = BASELINK_FUNCTIONS (fns);
  if (TREE_CODE (fns) == TEMPLATE_ID_EXPR)
    {
      explicit_targs = TREE_OPERAND (fns, 1);
      fns = TREE_OPERAND (fns, 0);
      template_only = 1;
    }
  gcc_assert (OVL_P (fns));
  fn = OVL_FIRST (fns);
  name = DECL_NAME (fn);

  basetype = TYPE_MAIN_VARIANT (TREE_TYPE (instance));
  gcc_assert (CLASS_TYPE_P (basetype));

  user_args = args == NULL ? NULL : *args;
  /* Under DR 147 A::A() is an invalid constructor call,
     not a functional cast.  */
  if (DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P (fn))
    {
      if (! (complain & tf_error))
	return error_mark_node;

      basetype = DECL_CONTEXT (fn);
      name = constructor_name (basetype);
      auto_diagnostic_group d;
      if (permerror (input_location,
		     "cannot call constructor %<%T::%D%> directly",
		     basetype, name))
	inform (input_location, "for a function-style cast, remove the "
		"redundant %<::%D%>", name);
      call = build_functional_cast (input_location, basetype,
				    build_tree_list_vec (user_args),
				    complain);
      return call;
    }

  if (processing_template_decl)
    orig_args = args == NULL ? NULL : make_tree_vector_copy (*args);

  /* Process the argument list.  */
  if (args != NULL && *args != NULL)
    {
      *args = resolve_args (*args, complain);
      if (*args == NULL)
	return error_mark_node;
      user_args = *args;
    }

  /* Consider the object argument to be used even if we end up selecting a
     static member function.  */
  instance = mark_type_use (instance);

  /* Figure out whether to skip the first argument for the error
     message we will display to users if an error occurs.  We don't
     want to display any compiler-generated arguments.  The "this"
     pointer hasn't been added yet.  However, we must remove the VTT
     pointer if this is a call to a base-class constructor or
     destructor.  */
  skip_first_for_error = false;
  if (IDENTIFIER_CDTOR_P (name))
    {
      /* Callers should explicitly indicate whether they want to ctor
	 the complete object or just the part without virtual bases.  */
      gcc_assert (name != ctor_identifier);

      /* Remove the VTT pointer, if present.  */
      if ((name == base_ctor_identifier || name == base_dtor_identifier)
	  && CLASSTYPE_VBASECLASSES (basetype))
	skip_first_for_error = true;

      /* It's OK to call destructors and constructors on cv-qualified
	 objects.  Therefore, convert the INSTANCE to the unqualified
	 type, if necessary.  */
      if (!same_type_p (basetype, TREE_TYPE (instance)))
	{
	  instance = build_this (instance);
	  instance = build_nop (build_pointer_type (basetype), instance);
	  instance = build_fold_indirect_ref (instance);
	}
    }
  else
    gcc_assert (!DECL_DESTRUCTOR_P (fn) && !DECL_CONSTRUCTOR_P (fn));

  /* For the overload resolution we need to find the actual `this`
     that would be captured if the call turns out to be to a
     non-static member function.  Do not actually capture it at this
     point.  */
  if (DECL_CONSTRUCTOR_P (fn))
    /* Constructors don't use the enclosing 'this'.  */
    first_mem_arg = instance;
  else
    first_mem_arg = maybe_resolve_dummy (instance, false);

  conversion_obstack_sentinel cos;

  /* The number of arguments artificial parms in ARGS; we subtract one because
     there's no 'this' in ARGS.  */
  unsigned skip = num_artificial_parms_for (fn) - 1;

  /* If CONSTRUCTOR_IS_DIRECT_INIT is set, this was a T{ } form
     initializer, not T({ }).  */
  if (DECL_CONSTRUCTOR_P (fn)
      && vec_safe_length (user_args) > skip
      && DIRECT_LIST_INIT_P ((*user_args)[skip]))
    {
      tree init_list = (*user_args)[skip];
      tree init = NULL_TREE;

      gcc_assert (user_args->length () == skip + 1
		  && !(flags & LOOKUP_ONLYCONVERTING));

      /* If the initializer list has no elements and T is a class type with
	 a default constructor, the object is value-initialized.  Handle
	 this here so we don't need to handle it wherever we use
	 build_special_member_call.  */
      if (CONSTRUCTOR_NELTS (init_list) == 0
	  && TYPE_HAS_DEFAULT_CONSTRUCTOR (basetype)
	  /* For a user-provided default constructor, use the normal
	     mechanisms so that protected access works.  */
	  && type_has_non_user_provided_default_constructor (basetype)
	  && !processing_template_decl)
	init = build_value_init (basetype, complain);

      /* If BASETYPE is an aggregate, we need to do aggregate
	 initialization.  */
      else if (CP_AGGREGATE_TYPE_P (basetype))
	{
	  init = reshape_init (basetype, init_list, complain);
	  init = digest_init (basetype, init, complain);
	}

      if (init)
	{
	  if (is_dummy_object (instance))
	    return get_target_expr (init, complain);
	  return cp_build_init_expr (instance, init);
	}

      /* Otherwise go ahead with overload resolution.  */
      add_list_candidates (fns, first_mem_arg, user_args,
			   basetype, explicit_targs, template_only,
			   conversion_path, access_binfo, flags,
			   &candidates, complain);
    }
  else
    add_candidates (fns, first_mem_arg, user_args, optype,
		    explicit_targs, template_only, conversion_path,
		    access_binfo, flags, &candidates, complain);

  any_viable_p = false;
  candidates = splice_viable (candidates, false, &any_viable_p);

  if (!any_viable_p)
    {
      /* [dcl.init], 17.6.2.2:

	 Otherwise, if no constructor is viable, the destination type is
	 a (possibly cv-qualified) aggregate class A, and the initializer
	 is a parenthesized expression-list, the object is initialized as
	 follows...

	 We achieve this by building up a CONSTRUCTOR, as for list-init,
	 and setting CONSTRUCTOR_IS_PAREN_INIT to distinguish between
	 the two.  */
      if (DECL_CONSTRUCTOR_P (fn)
	  && !(flags & LOOKUP_ONLYCONVERTING)
	  && cxx_dialect >= cxx20
	  && CP_AGGREGATE_TYPE_P (basetype)
	  && !vec_safe_is_empty (user_args))
	{
	  /* Create a CONSTRUCTOR from ARGS, e.g. {1, 2} from <1, 2>.  */
	  tree ctor = build_constructor_from_vec (init_list_type_node,
						  user_args);
	  CONSTRUCTOR_IS_DIRECT_INIT (ctor) = true;
	  CONSTRUCTOR_IS_PAREN_INIT (ctor) = true;
	  if (is_dummy_object (instance))
	    return ctor;
	  else
	    {
	      ctor = digest_init (basetype, ctor, complain);
	      if (ctor == error_mark_node)
		return error_mark_node;
	      return cp_build_init_expr (instance, ctor);
	    }
	}
      if (complain & tf_error)
	complain_about_no_candidates_for_method_call (instance, candidates,
						      explicit_targs, basetype,
						      optype, name,
						      skip_first_for_error,
						      user_args);
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
	      auto_diagnostic_group d;
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
	  if (fn_p)
	    *fn_p = error_mark_node;
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
			     ? G_("pure virtual %q#D called from constructor")
			     : G_("pure virtual %q#D called from destructor")),
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
	      else if (current_class_ptr && any_dependent_bases_p ())
		/* We can't tell until instantiation time whether we can use
		   *this as the implicit object argument.  */;
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
	      /* Now we know what function is being called.  */
	      if (fn_p)
		*fn_p = fn;
	      /* Build the actual CALL_EXPR.  */
	      call = build_over_call (cand, flags, complain);

	      /* Suppress warnings for if (my_struct.operator= (x)) where
		 my_struct is implicitly converted to bool. */
	      if (TREE_CODE (call) == MODIFY_EXPR)
		suppress_warning (call, OPT_Wparentheses);

	      /* In an expression of the form `a->f()' where `f' turns
		 out to be a static member function, `a' is
		 none-the-less evaluated.  */
	      if (!is_dummy_object (instance))
		call = keep_unused_object_arg (call, instance, fn);
	      if (call != error_mark_node
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

      /* Prune all but the selected function from the original overload
	 set so that we can avoid some duplicate work at instantiation time.  */
      if (really_overloaded_fn (fns))
	{
	  if (DECL_TEMPLATE_INFO (fn)
	      && DECL_MEMBER_TEMPLATE_P (DECL_TI_TEMPLATE (fn)))
	    {
	      /* Use the selected template, not the specialization, so that
		 this looks like an actual lookup result for sake of
		 filter_memfn_lookup.  */

	      if (OVL_SINGLE_P (fns))
		/* If the original overload set consists of a single function
		   template, this isn't beneficial.  */
		goto skip_prune;

	      fn = ovl_make (DECL_TI_TEMPLATE (fn));
	      if (template_only)
		fn = lookup_template_function (fn, explicit_targs);
	    }
	  orig_fns = copy_node (orig_fns);
	  BASELINK_FUNCTIONS (orig_fns) = fn;
	  BASELINK_FUNCTIONS_MAYBE_INCOMPLETE_P (orig_fns) = true;
	}

skip_prune:
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

  if (orig_args != NULL)
    release_tree_vector (orig_args);

  return call;
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
	  || !has_next (ics2->kind))
	/* At this point, ICS1 cannot be a proper subsequence of
	   ICS2.  We can get a USER_CONV when we are comparing the
	   second standard conversion sequence of two user conversion
	   sequences.  */
	return false;

      ics2 = next_conversion (ics2);

      while (ics2->kind == ck_rvalue
	     || ics2->kind == ck_lvalue)
	ics2 = next_conversion (ics2);

      if (ics2->kind == ics1->kind
	  && same_type_p (ics2->type, ics1->type)
	  && (ics1->kind == ck_identity
	      || same_type_p (next_conversion (ics2)->type,
			      next_conversion (ics1)->type)))
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

/* Get the expression at the beginning of the conversion chain C.  */

static tree
conv_get_original_expr (conversion *c)
{
  for (; c; c = next_conversion (c))
    if (c->kind == ck_identity || c->kind == ck_ambig || c->kind == ck_aggr)
      return c->u.expr;
  return NULL_TREE;
}

/* Return a tree representing the number of elements initialized by the
   list-initialization C.  The caller must check that C converts to an
   array type.  */

static tree
nelts_initialized_by_list_init (conversion *c)
{
  /* If the array we're converting to has a dimension, we'll use that.  */
  if (TYPE_DOMAIN (c->type))
    return array_type_nelts_top (c->type);
  else
    {
      /* Otherwise, we look at how many elements the constructor we're
	 initializing from has.  */
      tree ctor = conv_get_original_expr (c);
      return size_int (CONSTRUCTOR_NELTS (ctor));
    }
}

/* True iff C is a conversion that binds a reference or a pointer to
   an array of unknown bound.  */

static inline bool
conv_binds_to_array_of_unknown_bound (conversion *c)
{
  /* ck_ref_bind won't have the reference stripped.  */
  tree type = non_reference (c->type);
  /* ck_qual won't have the pointer stripped.  */
  type = strip_pointer_operator (type);
  return (TREE_CODE (type) == ARRAY_TYPE
	  && TYPE_DOMAIN (type) == NULL_TREE);
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

  if (ics1->user_conv_p || ics1->kind == ck_list
      || ics1->kind == ck_aggr || ics2->kind == ck_aggr)
    {
      conversion *t1 = strip_standard_conversion (ics1);
      conversion *t2 = strip_standard_conversion (ics2);

      if (!t1 || !t2 || t1->kind != t2->kind)
	return 0;
      else if (t1->kind == ck_user)
	{
	  tree f1 = t1->cand ? t1->cand->fn : t1->type;
	  tree f2 = t2->cand ? t2->cand->fn : t2->type;
	  if (f1 != f2)
	    return 0;
	}
      /* List-initialization sequence L1 is a better conversion sequence than
	 list-initialization sequence L2 if

	 -- L1 and L2 convert to arrays of the same element type, and either
	 the number of elements n1 initialized by L1 is less than the number
	 of elements n2 initialized by L2, or n1=n2 and L2 converts to an array
	 of unknown bound and L1 does not.  (Added in CWG 1307 and extended by
	 P0388R4.)  */
      else if (t1->kind == ck_aggr
	       && TREE_CODE (t1->type) == ARRAY_TYPE
	       && TREE_CODE (t2->type) == ARRAY_TYPE
	       && same_type_p (TREE_TYPE (t1->type), TREE_TYPE (t2->type)))
	{
	  tree n1 = nelts_initialized_by_list_init (t1);
	  tree n2 = nelts_initialized_by_list_init (t2);
	  if (tree_int_cst_lt (n1, n2))
	    return 1;
	  else if (tree_int_cst_lt (n2, n1))
	    return -1;
	  /* The n1 == n2 case.  */
	  bool c1 = conv_binds_to_array_of_unknown_bound (t1);
	  bool c2 = conv_binds_to_array_of_unknown_bound (t2);
	  if (c1 && !c2)
	    return -1;
	  else if (!c1 && c2)
	    return 1;
	  else
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

  {
    /* A conversion in either direction between floating-point type FP1 and
       floating-point type FP2 is better than a conversion in the same
       direction between FP1 and arithmetic type T3 if
       - the floating-point conversion rank of FP1 is equal to the rank of
	 FP2, and
       - T3 is not a floating-point type, or T3 is a floating-point type
	 whose rank is not equal to the rank of FP1, or the floating-point
	 conversion subrank of FP2 is greater than the subrank of T3.  */
    tree fp1 = from_type1;
    tree fp2 = to_type1;
    tree fp3 = from_type2;
    tree t3 = to_type2;
    int ret = 1;
    if (TYPE_MAIN_VARIANT (fp2) == TYPE_MAIN_VARIANT (t3))
      {
	std::swap (fp1, fp2);
	std::swap (fp3, t3);
      }
    if (TYPE_MAIN_VARIANT (fp1) == TYPE_MAIN_VARIANT (fp3)
	&& SCALAR_FLOAT_TYPE_P (fp1)
	/* Only apply this rule if at least one of the 3 types is
	   extended floating-point type, otherwise keep them as
	   before for compatibility reasons with types like __float128.
	   float, double and long double alone have different conversion
	   ranks and so when just those 3 types are involved, this
	   rule doesn't trigger.  */
	&& (extended_float_type_p (fp1)
	    || (SCALAR_FLOAT_TYPE_P (fp2) && extended_float_type_p (fp2))
	    || (SCALAR_FLOAT_TYPE_P (t3) && extended_float_type_p (t3))))
      {
	if (TREE_CODE (fp2) != REAL_TYPE)
	  {
	    ret = -ret;
	    std::swap (fp2, t3);
	  }
	if (SCALAR_FLOAT_TYPE_P (fp2))
	  {
	    /* cp_compare_floating_point_conversion_ranks returns -1, 0 or 1
	       if the conversion rank is equal (-1 or 1 if the subrank is
	       different).  */
	    if (IN_RANGE (cp_compare_floating_point_conversion_ranks (fp1,
								      fp2),
			  -1, 1))
	      {
		/* Conversion ranks of FP1 and FP2 are equal.  */
		if (TREE_CODE (t3) != REAL_TYPE
		    || !IN_RANGE (cp_compare_floating_point_conversion_ranks
								(fp1, t3),
				  -1, 1))
		  /* FP1 <-> FP2 conversion is better.  */
		  return ret;
		int c = cp_compare_floating_point_conversion_ranks (fp2, t3);
		gcc_assert (IN_RANGE (c, -1, 1));
		if (c == 1)
		  /* Conversion subrank of FP2 is greater than subrank of T3.
		     FP1 <-> FP2 conversion is better.  */
		  return ret;
		else if (c == -1)
		  /* Conversion subrank of FP2 is less than subrank of T3.
		     FP1 <-> T3 conversion is better.  */
		  return -ret;
	      }
	    else if (SCALAR_FLOAT_TYPE_P (t3)
		     && IN_RANGE (cp_compare_floating_point_conversion_ranks
								(fp1, t3),
				  -1, 1))
	      /* Conversion ranks of FP1 and FP2 are not equal, conversion
		 ranks of FP1 and T3 are equal.
		 FP1 <-> T3 conversion is better.  */
	      return -ret;
	  }
      }
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
     to an implicit object parameter of a non-static member function
     declared without a ref-qualifier, and either S1 binds an lvalue
     reference to an lvalue and S2 binds an rvalue reference or S1 binds an
     rvalue reference to an rvalue and S2 binds an lvalue reference (C++0x
     draft standard, 13.3.3.2)

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
	  /* Per P0388R4:

	    void f (int(&)[]),     // (1)
		 f (int(&)[1]),    // (2)
		 f (int*);	   // (3)

	    (2) is better than (1), but (3) should be equal to (1) and to
	    (2).  For that reason we don't use ck_qual for (1) which would
	    give it the cr_exact rank while (3) remains ck_identity.
	    Therefore we compare (1) and (2) here.  For (1) we'll have

	      ck_ref_bind <- ck_identity
		int[] &	       int[1]

	    so to handle this we must look at ref_conv.  */
	  bool c1 = conv_binds_to_array_of_unknown_bound (ref_conv1);
	  bool c2 = conv_binds_to_array_of_unknown_bound (ref_conv2);
	  if (c1 && !c2)
	    return -1;
	  else if (!c1 && c2)
	    return 1;

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

  /* [over.ics.rank]

     Per CWG 1601:
     -- A conversion that promotes an enumeration whose underlying type
     is fixed to its underlying type is better than one that promotes to
     the promoted underlying type, if the two are different.  */
  if (ics1->rank == cr_promotion
      && ics2->rank == cr_promotion
      && UNSCOPED_ENUM_P (from_type1)
      && ENUM_FIXED_UNDERLYING_TYPE_P (from_type1)
      && same_type_p (from_type1, from_type2))
    {
      tree utype = ENUM_UNDERLYING_TYPE (from_type1);
      tree prom = type_promotes_to (from_type1);
      if (!same_type_p (utype, prom))
	{
	  if (same_type_p (to_type1, utype)
	      && same_type_p (to_type2, prom))
	    return 1;
	  else if (same_type_p (to_type2, utype)
		   && same_type_p (to_type1, prom))
	    return -1;
	}
    }

  /* Neither conversion sequence is better than the other.  */
  return 0;
}

/* The source type for this standard conversion sequence.  */

static tree
source_type (conversion *t)
{
  return strip_standard_conversion (t)->type;
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

/* CAND is a constructor candidate in joust in C++17 and up.  If it copies a
   prvalue returned from a conversion function, return true.  Otherwise, return
   false.  */

static bool
joust_maybe_elide_copy (z_candidate *cand)
{
  tree fn = cand->fn;
  if (!DECL_COPY_CONSTRUCTOR_P (fn) && !DECL_MOVE_CONSTRUCTOR_P (fn))
    return false;
  conversion *conv = cand->convs[0];
  if (conv->kind == ck_ambig)
    return false;
  gcc_checking_assert (conv->kind == ck_ref_bind);
  conv = next_conversion (conv);
  if (conv->kind == ck_user && !TYPE_REF_P (conv->type))
    {
      gcc_checking_assert (same_type_ignoring_top_level_qualifiers_p
			   (conv->type, DECL_CONTEXT (fn)));
      z_candidate *uc = conv->cand;
      if (DECL_CONV_FN_P (uc->fn))
	return true;
    }
  return false;
}

/* Return the class that CAND's implicit object parameter refers to.  */

static tree
class_of_implicit_object (z_candidate *cand)
{
  if (!DECL_IOBJ_MEMBER_FUNCTION_P (cand->fn))
    return NULL_TREE;

  /* "For conversion functions that are implicit object member functions,
     the function is considered to be a member of the class of the implied
     object argument for the purpose of defining the type of the implicit
     object parameter."  */
  if (DECL_CONV_FN_P (cand->fn))
    return TYPE_MAIN_VARIANT (TREE_TYPE (cand->first_arg));

  /* "For non-conversion functions that are implicit object member
     functions nominated by a using-declaration in a derived class, the
     function is considered to be a member of the derived class for the
     purpose of defining the type of the implicit object parameter."

     That derived class is reflected in the conversion_path binfo.  */
  return BINFO_TYPE (cand->conversion_path);
}

/* True if candidates C1 and C2 have corresponding object parameters per
   [basic.scope.scope].  */

static bool
object_parms_correspond (z_candidate *c1, tree fn1, z_candidate *c2, tree fn2)
{
  tree context = class_of_implicit_object (c1);
  tree ctx2 = class_of_implicit_object (c2);
  if (!ctx2)
    /* Leave context as is. */;
  else if (!context)
    context = ctx2;
  else if (context != ctx2)
    /* This can't happen for normal function calls, since it means finding
       functions in multiple bases which would fail with an ambiguous lookup,
       but it can occur with reversed operators.  */
    return false;

  return object_parms_correspond (fn1, fn2, context);
}

/* Return whether the first parameter of C1 matches the second parameter
   of C2.  */

static bool
reversed_match (z_candidate *c1, z_candidate *c2)
{
  tree fn1 = c1->fn;
  tree parms2 = TYPE_ARG_TYPES (TREE_TYPE (c2->fn));
  tree parm2 = TREE_VALUE (TREE_CHAIN (parms2));
  if (DECL_IOBJ_MEMBER_FUNCTION_P (fn1))
    {
      tree ctx = class_of_implicit_object (c1);
      return iobj_parm_corresponds_to (fn1, parm2, ctx);
    }
  else
    {
      tree parms1 = TYPE_ARG_TYPES (TREE_TYPE (fn1));
      tree parm1 = TREE_VALUE (parms1);
      return same_type_p (parm1, parm2);
    }
}

/* True if the defining declarations of the two candidates have equivalent
   parameters.  MATCH_KIND controls whether we're trying to compare the
   original declarations (for a warning) or the actual candidates.  */

enum class pmatch { original, current };

static bool
cand_parms_match (z_candidate *c1, z_candidate *c2, pmatch match_kind)
{
  tree fn1 = c1->fn;
  tree fn2 = c2->fn;
  bool reversed = (match_kind == pmatch::current
		   && c1->reversed () != c2->reversed ());
  if (fn1 == fn2 && !reversed)
    return true;
  if (identifier_p (fn1) || identifier_p (fn2))
    return false;
  if (match_kind == pmatch::original)
    {
      /* We don't look at c1->template_decl because that's only set for
	 primary templates, not e.g. non-template member functions of
	 class templates.  */
      tree t1 = most_general_template (fn1);
      tree t2 = most_general_template (fn2);
      if (t1 || t2)
	{
	  if (!t1 || !t2)
	    return false;
	  if (t1 == t2)
	    return true;
	  fn1 = DECL_TEMPLATE_RESULT (t1);
	  fn2 = DECL_TEMPLATE_RESULT (t2);
	}
    }

  else if (reversed)
    return (reversed_match (c1, c2)
	    && reversed_match (c2, c1));

  tree parms1 = TYPE_ARG_TYPES (TREE_TYPE (fn1));
  tree parms2 = TYPE_ARG_TYPES (TREE_TYPE (fn2));

  if (!(DECL_FUNCTION_MEMBER_P (fn1)
	&& DECL_FUNCTION_MEMBER_P (fn2)))
    /* Early escape.  */;

  /* CWG2789 is not adequate, it should specify corresponding object
     parameters, not same typed object parameters.  */
  else if (!object_parms_correspond (c1, fn1, c2, fn2))
    return false;
  else
    {
      /* We just compared the object parameters, if they don't correspond
	 we already returned false.  */
      auto skip_parms = [] (tree fn, tree parms)
	{
	  if (DECL_XOBJ_MEMBER_FUNCTION_P (fn))
	    return TREE_CHAIN (parms);
	  else
	    return skip_artificial_parms_for (fn, parms);
	};
      parms1 = skip_parms (fn1, parms1);
      parms2 = skip_parms (fn2, parms2);
    }
  return compparms (parms1, parms2);
}

/* True iff FN is a copy or move constructor or assignment operator.  */

static bool
sfk_copy_or_move (tree fn)
{
  if (TREE_CODE (fn) != FUNCTION_DECL)
    return false;
  special_function_kind sfk = special_function_p (fn);
  return sfk >= sfk_copy_constructor && sfk <= sfk_move_assignment;
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
      && cand1->reversed () == cand2->reversed ()
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
      int static_1 = (TREE_CODE (cand1->fn) == FUNCTION_DECL
		      && DECL_STATIC_FUNCTION_P (cand1->fn));
      int static_2 = (TREE_CODE (cand2->fn) == FUNCTION_DECL
		      && DECL_STATIC_FUNCTION_P (cand2->fn));

      if (TREE_CODE (cand1->fn) == FUNCTION_DECL
	  && TREE_CODE (cand2->fn) == FUNCTION_DECL
	  && DECL_CONSTRUCTOR_P (cand1->fn)
	  && is_list_ctor (cand1->fn) != is_list_ctor (cand2->fn))
	/* We're comparing a near-match list constructor and a near-match
	   non-list constructor.  Just treat them as unordered.  */
	return 0;

      gcc_assert (static_1 != static_2);

      if (static_1)
	{
	  /* C++23 [over.best.ics.general] says:
	     When the parameter is the implicit object parameter of a static
	     member function, the implicit conversion sequence is a standard
	     conversion sequence that is neither better nor worse than any
	     other standard conversion sequence.  */
	  if (CONVERSION_RANK (cand2->convs[0]) >= cr_user)
	    winner = 1;
	  off2 = 1;
	}
      else
	{
	  if (CONVERSION_RANK (cand1->convs[0]) >= cr_user)
	    winner = -1;
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
	      /* Ambiguity between normal and reversed comparison operators
		 with the same parameter types.  P2468 decided not to go with
		 this approach to resolving the ambiguity, so pedwarn.  */
	      if ((complain & tf_warning_or_error)
		  && (cand1->reversed () != cand2->reversed ())
		  && cand_parms_match (cand1, cand2, pmatch::original))
		{
		  struct z_candidate *w, *l;
		  if (cand2->reversed ())
		    winner = 1, w = cand1, l = cand2;
		  else
		    winner = -1, w = cand2, l = cand1;
		  if (warn)
		    {
		      auto_diagnostic_group d;
		      if (pedwarn (input_location, 0,
				   "C++20 says that these are ambiguous, "
				   "even though the second is reversed:"))
			{
			  print_z_candidate (input_location,
					     N_("candidate 1:"), w);
			  print_z_candidate (input_location,
					     N_("candidate 2:"), l);
			  if (w->fn == l->fn
			      && DECL_IOBJ_MEMBER_FUNCTION_P (w->fn)
			      && (type_memfn_quals (TREE_TYPE (w->fn))
				  & TYPE_QUAL_CONST) == 0)
			    {
			      /* Suggest adding const to
				 struct A { bool operator==(const A&); }; */
			      tree parmtype
				= FUNCTION_FIRST_USER_PARMTYPE (w->fn);
			      parmtype = TREE_VALUE (parmtype);
			      if (TYPE_REF_P (parmtype)
				  && TYPE_READONLY (TREE_TYPE (parmtype))
				  && (same_type_ignoring_top_level_qualifiers_p
				      (TREE_TYPE (parmtype),
				       DECL_CONTEXT (w->fn))))
				inform (DECL_SOURCE_LOCATION (w->fn),
					"try making the operator a %<const%> "
					"member function");
			    }
			}
		    }
		  else
		    add_warning (w, l);
		  return winner;
		}

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
      /* In C++17, the constructor might have been elided, which means that
	 an originally null ->second_conv could become non-null.  */
      && winner && warn_conversion && cand1->second_conv && cand2->second_conv
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

	  if (TREE_CODE (t) == TREE_CODE (f) && INDIRECT_TYPE_P (t))
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
	  if (INDIRECT_TYPE_P (source))
	    source = TREE_TYPE (source);
	  auto_diagnostic_group d;
	  if (warning (OPT_Wconversion, "choosing %qD over %qD", w->fn, l->fn)
	      && warning (OPT_Wconversion, "  for conversion from %qH to %qI",
			  source, w->second_conv->type)) 
	    {
	      inform (input_location, "  because conversion sequence "
		      "for the argument is better");
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

  /* CWG2735 (PR109247): A copy/move ctor/op= for which its operand uses an
     explicit conversion (due to list-initialization) is worse.  */
  {
    z_candidate *sp = nullptr;
    if (sfk_copy_or_move (cand1->fn))
      sp = cand1;
    if (sfk_copy_or_move (cand2->fn))
      sp = sp ? nullptr : cand2;
    if (sp)
      {
	conversion *conv = sp->convs[!DECL_CONSTRUCTOR_P (sp->fn)];
	if (conv->user_conv_p)
	  for (; conv; conv = next_conversion (conv))
	    if (conv->kind == ck_user
		&& DECL_P (conv->cand->fn)
		&& DECL_NONCONVERTING_P (conv->cand->fn))
	      return (sp == cand1) ? -1 : 1;
      }
  }

  /* DR2327: C++17 copy elision in [over.match.ctor] (direct-init) context.
     The standard currently says that only constructors are candidates, but if
     one copies a prvalue returned by a conversion function we prefer that.

     Clang does something similar, as discussed at
     http://lists.isocpp.org/core/2017/10/3166.php
     http://lists.isocpp.org/core/2019/03/5721.php  */
  if (len == 1 && cxx_dialect >= cxx17
      && DECL_P (cand1->fn)
      && DECL_COMPLETE_CONSTRUCTOR_P (cand1->fn)
      && !(cand1->flags & LOOKUP_ONLYCONVERTING))
    {
      bool elided1 = joust_maybe_elide_copy (cand1);
      bool elided2 = joust_maybe_elide_copy (cand2);
      winner = elided1 - elided2;
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

  /* Concepts: F1 and F2 are non-template functions with the same
     parameter-type-lists, and F1 is more constrained than F2 according to the
     partial ordering of constraints described in 13.5.4.  */

  if (flag_concepts && DECL_P (cand1->fn) && DECL_P (cand2->fn)
      && !cand1->template_decl && !cand2->template_decl
      && cand_parms_match (cand1, cand2, pmatch::current))
    {
      winner = more_constrained (cand1->fn, cand2->fn);
      if (winner)
	return winner;
    }

  /* F2 is a rewritten candidate (12.4.1.2) and F1 is not, or F1 and F2 are
     rewritten candidates, and F2 is a synthesized candidate with reversed
     order of parameters and F1 is not.  */
  if (cand1->rewritten ())
    {
      if (!cand2->rewritten ())
	return -1;
      if (!cand1->reversed () && cand2->reversed ())
	return 1;
      if (cand1->reversed () && !cand2->reversed ())
	return -1;
    }
  else if (cand2->rewritten ())
    return 1;

  /* F1 is generated from a deduction-guide (13.3.1.8) and F2 is not */
  if (deduction_guide_p (cand1->fn))
    {
      gcc_assert (deduction_guide_p (cand2->fn));
      /* We distinguish between candidates from an explicit deduction guide and
	 candidates built from a constructor based on DECL_ARTIFICIAL.  */
      int art1 = DECL_ARTIFICIAL (cand1->fn);
      int art2 = DECL_ARTIFICIAL (cand2->fn);
      if (art1 != art2)
	return art2 - art1;

      if (art1)
	{
	  /* Prefer the special copy guide over a declared copy/move
	     constructor.  */
	  if (copy_guide_p (cand1->fn))
	    return 1;
	  if (copy_guide_p (cand2->fn))
	    return -1;

	  /* Prefer a candidate generated from a non-template constructor.  */
	  int tg1 = template_guide_p (cand1->fn);
	  int tg2 = template_guide_p (cand2->fn);
	  if (tg1 != tg2)
	    return tg2 - tg1;
	}
    }

  /* F1 is a member of a class D, F2 is a member of a base class B of D, and
     for all arguments the corresponding parameters of F1 and F2 have the same
     type (CWG 2273/2277). */
  if (DECL_P (cand1->fn) && DECL_CLASS_SCOPE_P (cand1->fn)
      && !DECL_CONV_FN_P (cand1->fn)
      && DECL_P (cand2->fn) && DECL_CLASS_SCOPE_P (cand2->fn)
      && !DECL_CONV_FN_P (cand2->fn))
    {
      tree base1 = DECL_CONTEXT (strip_inheriting_ctors (cand1->fn));
      tree base2 = DECL_CONTEXT (strip_inheriting_ctors (cand2->fn));

      bool used1 = false;
      bool used2 = false;
      if (base1 == base2)
	/* No difference.  */;
      else if (DERIVED_FROM_P (base1, base2))
	used1 = true;
      else if (DERIVED_FROM_P (base2, base1))
	used2 = true;

      if (int diff = used2 - used1)
	{
	  for (i = 0; i < len; ++i)
	    {
	      conversion *t1 = cand1->convs[i + off1];
	      conversion *t2 = cand2->convs[i + off2];
	      if (!same_type_p (t1->type, t2->type))
		break;
	    }
	  if (i == len)
	    return diff;
	}
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
		      auto_diagnostic_group d;
		      if (permerror (input_location,
				     "default argument mismatch in "
				     "overload resolution"))
			{
			  inform (DECL_SOURCE_LOCATION (cand1->fn),
				  " candidate 1: %q#F", cand1->fn);
			  inform (DECL_SOURCE_LOCATION (cand2->fn),
				  " candidate 2: %q#F", cand2->fn);
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

  /* Extension: If the worst conversion for one candidate is better than the
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
	      auto_diagnostic_group d;
	      if (pedwarn (input_location, 0,
			   "ISO C++ says that these are ambiguous, even "
			   "though the worst conversion for the first is "
			   "better than the worst conversion for the second:"))
		{
		  print_z_candidate (input_location, N_("candidate 1:"), w);
		  print_z_candidate (input_location, N_("candidate 2:"), l);
		}
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
   algorithm.  The candidates list is assumed to be sorted according
   to viability (via splice_viable).  */

static struct z_candidate *
tourney (struct z_candidate *candidates, tsubst_flags_t complain)
{
  struct z_candidate **champ = &candidates, **challenger;
  int fate;
  struct z_candidate *previous_worse_champ = nullptr;

  /* Walk through the list once, comparing each current champ to the next
     candidate, knocking out a candidate or two with each comparison.  */

  for (challenger = &candidates->next; *challenger && (*challenger)->viable; )
    {
      fate = joust (*champ, *challenger, 0, complain);
      if (fate == 1)
	challenger = &(*challenger)->next;
      else if (fate == -1)
	{
	  previous_worse_champ = *champ;
	  champ = challenger;
	  challenger = &(*challenger)->next;
	}
      else
	{
	  previous_worse_champ = nullptr;
	  champ = &(*challenger)->next;
	  if (!*champ || !(*champ)->viable)
	    {
	      champ = nullptr;
	      break;
	    }
	  challenger = &(*champ)->next;
	}
    }

  /* Make sure the champ is better than all the candidates it hasn't yet
     been compared to.  */

  if (champ)
    for (challenger = &candidates;
	 challenger != champ;
	 challenger = &(*challenger)->next)
      {
	if (*challenger == previous_worse_champ)
	  /* We already know this candidate is worse than the champ.  */
	  continue;
	fate = joust (*champ, *challenger, 0, complain);
	if (fate != 1)
	  {
	    champ = nullptr;
	    break;
	  }
      }

  if (!champ)
    return nullptr;

  /* Move the champ to the front of the candidate list.  */

  if (champ != &candidates)
    {
      z_candidate *saved_champ = *champ;
      *champ = saved_champ->next;
      saved_champ->next = candidates;
      candidates = saved_champ;
    }

  return candidates;
}

/* Returns nonzero if things of type FROM can be converted to TO.  */

bool
can_convert (tree to, tree from, tsubst_flags_t complain)
{
  tree arg = NULL_TREE;
  /* implicit_conversion only considers user-defined conversions
     if it has an expression for the call argument list.  */
  if (CLASS_TYPE_P (from) || CLASS_TYPE_P (to))
    arg = build_stub_object (from);
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
  bool ok_p;

  conversion_obstack_sentinel cos;
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

  return ok_p;
}

/* Like can_convert_arg, but allows dubious conversions as well.  */

bool
can_convert_arg_bad (tree to, tree from, tree arg, int flags,
		     tsubst_flags_t complain)
{
  conversion *t;

  conversion_obstack_sentinel cos;
  /* Try to perform the conversion.  */
  t  = implicit_conversion (to, from, arg, /*c_cast_p=*/false,
			    flags, complain);

  return t != NULL;
}

/* Return an IMPLICIT_CONV_EXPR from EXPR to TYPE with bits set from overload
   resolution FLAGS.  */

tree
build_implicit_conv_flags (tree type, tree expr, int flags)
{
  /* In a template, we are only concerned about determining the
     type of non-dependent expressions, so we do not have to
     perform the actual conversion.  But for initializers, we
     need to be able to perform it at instantiation
     (or instantiate_non_dependent_expr) time.  */
  expr = build1 (IMPLICIT_CONV_EXPR, type, expr);
  if (!(flags & LOOKUP_ONLYCONVERTING))
    IMPLICIT_CONV_EXPR_DIRECT_INIT (expr) = true;
  if (flags & LOOKUP_NO_NARROWING)
    IMPLICIT_CONV_EXPR_BRACED_INIT (expr) = true;
  return expr;
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
  location_t loc = cp_expr_loc_or_input_loc (expr);

  if (TYPE_REF_P (type))
    expr = mark_lvalue_use (expr);
  else
    expr = mark_rvalue_use (expr);

  if (error_operand_p (expr))
    return error_mark_node;

  conversion_obstack_sentinel cos;

  conv = implicit_conversion (type, TREE_TYPE (expr), expr,
			      /*c_cast_p=*/false,
			      flags, complain);

  if (!conv)
    {
      if (complain & tf_error)
	implicit_conversion_error (loc, type, expr);
      expr = error_mark_node;
    }
  else if (processing_template_decl && conv->kind != ck_identity)
    expr = build_implicit_conv_flags (type, expr, flags);
  else
    {
      /* Give a conversion call the same location as expr.  */
      iloc_sentinel il (loc);
      expr = convert_like (conv, expr, complain);
    }

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

  if (type == error_mark_node || error_operand_p (expr))
    return error_mark_node;
  /* [dcl.init]

     If the destination type is a (possibly cv-qualified) class type:

     -- If the initialization is direct-initialization ...,
     constructors are considered.

       -- If overload resolution is successful, the selected constructor
       is called to initialize the object, with the initializer expression
       or expression-list as its argument(s).

       -- Otherwise, if no constructor is viable, the destination type is
       a (possibly cv-qualified) aggregate class A, and the initializer is
       a parenthesized expression-list, the object is initialized as
       follows...  */
  if (CLASS_TYPE_P (type))
    {
      releasing_vec args (make_tree_vector_single (expr));
      expr = build_special_member_call (NULL_TREE, complete_ctor_identifier,
					&args, type, LOOKUP_NORMAL, complain);
      return build_cplus_new (type, expr, complain);
    }

  conversion_obstack_sentinel cos;

  conv = implicit_conversion (type, TREE_TYPE (expr), expr,
			      c_cast_p,
			      LOOKUP_NORMAL, complain);
  if (!conv || conv->bad_p)
    expr = NULL_TREE;
  else if (processing_template_decl && conv->kind != ck_identity)
    {
      /* In a template, we are only concerned about determining the
	 type of non-dependent expressions, so we do not have to
	 perform the actual conversion.  But for initializers, we
	 need to be able to perform it at instantiation
	 (or instantiate_non_dependent_expr) time.  */
      expr = build1 (IMPLICIT_CONV_EXPR, type, expr);
      IMPLICIT_CONV_EXPR_DIRECT_INIT (expr) = true;
    }
  else
    expr = convert_like (conv, expr, NULL_TREE, 0,
			 /*issue_conversion_warnings=*/false,
			 c_cast_p, /*nested_p=*/false, complain);

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
  tree var = create_temporary_var (type);

  /* Register the variable.  */
  if (VAR_P (decl)
      && (TREE_STATIC (decl) || CP_DECL_THREAD_LOCAL_P (decl)))
    {
      /* Namespace-scope or local static; give it a mangled name.  */

      /* If an initializer is visible to multiple translation units, those
	 translation units must agree on the addresses of the
	 temporaries. Therefore the temporaries must be given a consistent name
	 and vague linkage. The mangled name of a temporary is the name of the
	 non-temporary object in whose initializer they appear, prefixed with
	 GR and suffixed with a sequence number mangled using the usual rules
	 for a seq-id. Temporaries are numbered with a pre-order, depth-first,
	 left-to-right walk of the complete initializer.  */
      copy_linkage (var, decl);

      tree name = mangle_ref_init_variable (decl);
      DECL_NAME (var) = name;
      SET_DECL_ASSEMBLER_NAME (var, name);

      /* Set the context to make the variable mergeable in modules.  */
      DECL_CONTEXT (var) = current_scope ();
    }
  else
    /* Create a new cleanup level if necessary.  */
    maybe_push_cleanup_level (type);

  return pushdecl (var);
}

/* EXPR is the initializer for a variable DECL of reference or
   std::initializer_list type.  Create, push and return a new VAR_DECL
   for the initializer so that it will live as long as DECL.  Any
   cleanup for the new variable is returned through CLEANUP, and the
   code to initialize the new variable is returned through INITP.  */

static tree
set_up_extended_ref_temp (tree decl, tree expr, vec<tree, va_gc> **cleanups,
			  tree *initp, tree *cond_guard)
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
  else
    {
      if (TREE_ADDRESSABLE (expr))
	TREE_ADDRESSABLE (var) = 1;
      if (DECL_MERGEABLE (TARGET_EXPR_SLOT (expr)))
	DECL_MERGEABLE (var) = true;
    }

  if (TREE_CODE (decl) == FIELD_DECL
      && extra_warnings && !warning_suppressed_p (decl))
    {
      warning (OPT_Wextra, "a temporary bound to %qD only persists "
	       "until the constructor exits", decl);
      suppress_warning (decl);
    }

  /* Recursively extend temps in this initializer.  */
  TARGET_EXPR_INITIAL (expr)
    = extend_ref_init_temps (decl, TARGET_EXPR_INITIAL (expr), cleanups,
			     cond_guard);

  /* Any reference temp has a non-trivial initializer.  */
  DECL_NONTRIVIALLY_INITIALIZED_P (var) = true;

  /* If the initializer is constant, put it in DECL_INITIAL so we get
     static initialization and use in constant expressions.  */
  init = maybe_constant_init (expr, var, /*manifestly_const_eval=*/true);
  /* As in store_init_value.  */
  init = cp_fully_fold (init);
  if (TREE_CONSTANT (init))
    {
      if (literal_type_p (type) && CP_TYPE_CONST_NON_VOLATILE_P (type))
	{
	  /* 5.19 says that a constant expression can include an
	     lvalue-rvalue conversion applied to "a glvalue of literal type
	     that refers to a non-volatile temporary object initialized
	     with a constant expression".  Rather than try to communicate
	     that this VAR_DECL is a temporary, just mark it constexpr.  */
	  DECL_DECLARED_CONSTEXPR_P (var) = true;
	  DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (var) = true;
	  TREE_CONSTANT (var) = true;
	  TREE_READONLY (var) = true;
	}
      DECL_INITIAL (var) = init;
      init = NULL_TREE;
    }
  else
    /* Create the INIT_EXPR that will initialize the temporary
       variable.  */
    init = split_nonconstant_init (var, expr);
  if (at_function_scope_p ())
    {
      add_decl_expr (var);

      if (TREE_STATIC (var))
	init = add_stmt_to_compound (init, register_dtor_fn (var));
      else
	{
	  tree cleanup = cxx_maybe_build_cleanup (var, tf_warning_or_error);
	  if (cleanup)
	    {
	      if (cond_guard && cleanup != error_mark_node)
		{
		  if (*cond_guard == NULL_TREE)
		    {
		      *cond_guard = build_local_temp (boolean_type_node);
		      add_decl_expr (*cond_guard);
		      tree set = cp_build_modify_expr (UNKNOWN_LOCATION,
						       *cond_guard, NOP_EXPR,
						       boolean_false_node,
						       tf_warning_or_error);
		      finish_expr_stmt (set);
		    }
		  cleanup = build3 (COND_EXPR, void_type_node,
				    *cond_guard, cleanup, NULL_TREE);
		}
	      vec_safe_push (*cleanups, cleanup);
	    }
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
	  if (CP_DECL_THREAD_LOCAL_P (var))
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
  /* Avoid -Wunused-variable warning (c++/38958).  */
  if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (type)
      && VAR_P (decl))
    TREE_USED (decl) = DECL_READ_P (decl) = true;

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
  location_t loc = cp_expr_loc_or_input_loc (expr);

  if (type == error_mark_node || error_operand_p (expr))
    return error_mark_node;

  conversion_obstack_sentinel cos;

  conv = reference_binding (type, TREE_TYPE (expr), expr, /*c_cast_p=*/false,
			    flags, complain);
  /* If this conversion failed, we're in C++20, and we have something like
     A& a(b) where A is an aggregate, try again, this time as A& a{b}.  */
  if ((!conv || conv->bad_p)
      && (flags & LOOKUP_AGGREGATE_PAREN_INIT))
    {
      tree e = build_constructor_single (init_list_type_node, NULL_TREE, expr);
      CONSTRUCTOR_IS_DIRECT_INIT (e) = true;
      CONSTRUCTOR_IS_PAREN_INIT (e) = true;
      conversion *c = reference_binding (type, TREE_TYPE (e), e,
					 /*c_cast_p=*/false, flags, complain);
      /* If this worked, use it.  */
      if (c && !c->bad_p)
	expr = e, conv = c;
    }
  if (!conv || conv->bad_p)
    {
      if (complain & tf_error)
	{
	  if (conv)
	    convert_like (conv, expr, complain);
	  else if (!CP_TYPE_CONST_P (TREE_TYPE (type))
		   && !TYPE_REF_IS_RVALUE (type)
		   && !lvalue_p (expr))
	    error_at (loc, "invalid initialization of non-const reference of "
		      "type %qH from an rvalue of type %qI",
		      type, TREE_TYPE (expr));
	  else
	    error_at (loc, "invalid initialization of reference of type "
		      "%qH from expression of type %qI", type,
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

  return expr;
}

/* Return true if T is std::pair<const T&, const T&>.  */

static bool
std_pair_ref_ref_p (tree t)
{
  /* First, check if we have std::pair.  */
  if (!NON_UNION_CLASS_TYPE_P (t)
      || !CLASSTYPE_TEMPLATE_INSTANTIATION (t))
    return false;
  tree tdecl = TYPE_NAME (TYPE_MAIN_VARIANT (t));
  if (!decl_in_std_namespace_p (tdecl))
    return false;
  tree name = DECL_NAME (tdecl);
  if (!name || !id_equal (name, "pair"))
    return false;

  /* Now see if the template arguments are both const T&.  */
  tree args = CLASSTYPE_TI_ARGS (t);
  if (TREE_VEC_LENGTH (args) != 2)
    return false;
  for (int i = 0; i < 2; i++)
    if (!TYPE_REF_OBJ_P (TREE_VEC_ELT (args, i))
	|| !CP_TYPE_CONST_P (TREE_TYPE (TREE_VEC_ELT (args, i))))
      return false;

  return true;
}

/* Return true if a class T has a reference member.  */

static bool
class_has_reference_member_p (tree t)
{
  for (tree fields = TYPE_FIELDS (t);
       fields;
       fields = DECL_CHAIN (fields))
    if (TREE_CODE (fields) == FIELD_DECL
	&& !DECL_ARTIFICIAL (fields)
	&& TYPE_REF_P (TREE_TYPE (fields)))
      return true;
  return false;
}

/* A wrapper for the above suitable as a callback for dfs_walk_once.  */

static tree
class_has_reference_member_p_r (tree binfo, void *)
{
  return (class_has_reference_member_p (BINFO_TYPE (binfo))
	  ? integer_one_node : NULL_TREE);
}


/* Return true if T (either a class or a function) has been marked as
   not-dangling.  */

static bool
no_dangling_p (tree t)
{
  t = lookup_attribute ("no_dangling", TYPE_ATTRIBUTES (t));
  if (!t)
    return false;

  t = TREE_VALUE (t);
  if (!t)
    return true;

  t = build_converted_constant_bool_expr (TREE_VALUE (t), tf_warning_or_error);
  t = cxx_constant_value (t);
  return t == boolean_true_node;
}

/* Return true if a class CTYPE is either std::reference_wrapper or
   std::ref_view, or a reference wrapper class.  We consider a class
   a reference wrapper class if it has a reference member.  We no
   longer check that it has a constructor taking the same reference type
   since that approach still generated too many false positives.  */

static bool
reference_like_class_p (tree ctype)
{
  if (!CLASS_TYPE_P (ctype))
    return false;

  if (no_dangling_p (ctype))
    return true;

  /* Also accept a std::pair<const T&, const T&>.  */
  if (std_pair_ref_ref_p (ctype))
    return true;

  tree tdecl = TYPE_NAME (TYPE_MAIN_VARIANT (ctype));
  if (decl_in_std_namespace_p (tdecl))
    {
      tree name = DECL_NAME (tdecl);
      if (name
	  && (id_equal (name, "reference_wrapper")
	      || id_equal (name, "span")
	      || id_equal (name, "ref_view")))
	return true;
    }

  /* Avoid warning if CTYPE looks like std::span: it has a T* member and
     a trivial destructor.  For example,

      template<typename T>
      struct Span {
	T* data_;
	std::size len_;
      };

     is considered std::span-like.  */
  if (NON_UNION_CLASS_TYPE_P (ctype) && TYPE_HAS_TRIVIAL_DESTRUCTOR (ctype))
    for (tree field = next_aggregate_field (TYPE_FIELDS (ctype));
	 field; field = next_aggregate_field (DECL_CHAIN (field)))
      if (TYPE_PTR_P (TREE_TYPE (field)))
	return true;

  /* Some classes, such as std::tuple, have the reference member in its
     (non-direct) base class.  */
  if (dfs_walk_once (TYPE_BINFO (ctype), class_has_reference_member_p_r,
		     nullptr, nullptr))
    return true;

  return false;
}

/* Helper for maybe_warn_dangling_reference to find a problematic CALL_EXPR
   that initializes the LHS (and at least one of its arguments represents
   a temporary, as outlined in maybe_warn_dangling_reference), or NULL_TREE
   if none found.  For instance:

     const S& s = S().self(); // S::self (&TARGET_EXPR <...>)
     const int& r = (42, f(1)); // f(1)
     const int& t = b ? f(1) : f(2); // f(1)
     const int& u = b ? f(1) : f(g); // f(1)
     const int& v = b ? f(g) : f(2); // f(2)
     const int& w = b ? f(g) : f(g); // NULL_TREE
     const int& y = (f(1), 42); // NULL_TREE
     const int& z = f(f(1)); // f(f(1))

   EXPR is the initializer.  If ARG_P is true, we're processing an argument
   to a function; the point is to distinguish between, for example,

     Ref::inner (&TARGET_EXPR <D.2839, F::foo (fm)>)

   where we shouldn't warn, and

     Ref::inner (&TARGET_EXPR <D.2908, F::foo (&TARGET_EXPR <...>)>)

   where we should warn (Ref is a reference_like_class_p so we see through
   it.  */

static tree
do_warn_dangling_reference (tree expr, bool arg_p)
{
  STRIP_NOPS (expr);

  if (arg_p && expr_represents_temporary_p (expr))
    {
      /* An attempt to reduce the number of -Wdangling-reference
	 false positives concerning reference wrappers (c++/107532).
	 When we encounter a reference_like_class_p, we don't warn
	 just yet; instead, we keep recursing to see if there were
	 any temporaries behind the reference-wrapper class.  */
      tree e = expr;
      while (handled_component_p (e))
	e = TREE_OPERAND (e, 0);
      tree type = TREE_TYPE (e);
      /* If the temporary represents a lambda, we don't really know
	 what's going on here.  */
      if (!reference_like_class_p (type) && !LAMBDA_TYPE_P (type))
	return expr;
    }

  switch (TREE_CODE (expr))
    {
    case CALL_EXPR:
      {
	tree fndecl = cp_get_callee_fndecl_nofold (expr);
	if (!fndecl
	    || warning_suppressed_p (fndecl, OPT_Wdangling_reference)
	    || !warning_enabled_at (DECL_SOURCE_LOCATION (fndecl),
				    OPT_Wdangling_reference)
	    /* Don't emit a false positive for:
		std::vector<int> v = ...;
		std::vector<int>::const_iterator it = v.begin();
		const int &r = *it++;
	       because R refers to one of the int elements of V, not to
	       a temporary object.  Member operator* may return a reference
	       but probably not to one of its arguments.  */
	    || (DECL_OBJECT_MEMBER_FUNCTION_P (fndecl)
		&& DECL_OVERLOADED_OPERATOR_P (fndecl)
		&& DECL_OVERLOADED_OPERATOR_IS (fndecl, INDIRECT_REF))
	    || no_dangling_p (TREE_TYPE (fndecl)))
	  return NULL_TREE;

	tree rettype = TREE_TYPE (TREE_TYPE (fndecl));
	/* If the function doesn't return a reference, don't warn.  This
	   can be e.g.
	     const int& z = std::min({1, 2, 3, 4, 5, 6, 7});
	   which doesn't dangle: std::min here returns an int.

	   If the function returns a std::pair<const T&, const T&>, we
	   warn, to detect e.g.
	     std::pair<const int&, const int&> v = std::minmax(1, 2);
	   which also creates a dangling reference, because std::minmax
	   returns std::pair<const T&, const T&>(b, a).  */
	if (!(TYPE_REF_OBJ_P (rettype) || reference_like_class_p (rettype)))
	  return NULL_TREE;

	/* Here we're looking to see if any of the arguments is a temporary
	   initializing a reference parameter.  */
	for (int i = 0; i < call_expr_nargs (expr); ++i)
	  {
	    tree arg = CALL_EXPR_ARG (expr, i);
	    /* Check that this argument initializes a reference, except for
	       the argument initializing the object of a member function.  */
	    if (!DECL_IOBJ_MEMBER_FUNCTION_P (fndecl)
		&& !TYPE_REF_P (TREE_TYPE (arg)))
	      continue;
	    STRIP_NOPS (arg);
	    if (TREE_CODE (arg) == ADDR_EXPR)
	      arg = TREE_OPERAND (arg, 0);
	    /* Recurse to see if the argument is a temporary.  It could also
	       be another call taking a temporary and returning it and
	       initializing this reference parameter.  */
	    if (do_warn_dangling_reference (arg, /*arg_p=*/true))
	      return expr;
	  /* Don't warn about member functions like:
	      std::any a(...);
	      S& s = a.emplace<S>({0}, 0);
	     which construct a new object and return a reference to it, but
	     we still want to detect:
	       struct S { const S& self () { return *this; } };
	       const S& s = S().self();
	     where 's' dangles.  If we've gotten here, the object this function
	     is invoked on is not a temporary.  */
	    if (DECL_OBJECT_MEMBER_FUNCTION_P (fndecl))
	      break;
	  }
	return NULL_TREE;
      }
    case COMPOUND_EXPR:
      return do_warn_dangling_reference (TREE_OPERAND (expr, 1), arg_p);
    case COND_EXPR:
      if (tree t = do_warn_dangling_reference (TREE_OPERAND (expr, 1), arg_p))
	return t;
      return do_warn_dangling_reference (TREE_OPERAND (expr, 2), arg_p);
    case PAREN_EXPR:
      return do_warn_dangling_reference (TREE_OPERAND (expr, 0), arg_p);
    case TARGET_EXPR:
      return do_warn_dangling_reference (TARGET_EXPR_INITIAL (expr), arg_p);
    default:
      return NULL_TREE;
    }
}

/* Implement -Wdangling-reference, to detect cases like

     int n = 1;
     const int& r = std::max(n - 1, n + 1); // r is dangling

   This creates temporaries from the arguments, returns a reference to
   one of the temporaries, but both temporaries are destroyed at the end
   of the full expression.

   This works by checking if a reference is initialized with a function
   that returns a reference, and at least one parameter of the function
   is a reference that is bound to a temporary.  It assumes that such a
   function actually returns one of its arguments.

   DECL is the reference being initialized, INIT is the initializer.  */

static void
maybe_warn_dangling_reference (const_tree decl, tree init)
{
  if (!warn_dangling_reference)
    return;
  tree type = TREE_TYPE (decl);
  /* Only warn if what we're initializing has type T&& or const T&, or
     std::pair<const T&, const T&>.  (A non-const lvalue reference can't
     bind to a temporary.)  */
  if (!((TYPE_REF_OBJ_P (type)
	 && (TYPE_REF_IS_RVALUE (type)
	     || CP_TYPE_CONST_P (TREE_TYPE (type))))
	|| std_pair_ref_ref_p (type)))
    return;
  /* Don't suppress the diagnostic just because the call comes from
     a system header.  If the DECL is not in a system header, or if
     -Wsystem-headers was provided, warn.  */
  auto wsh
    = make_temp_override (global_dc->m_warn_system_headers,
			  (!in_system_header_at (DECL_SOURCE_LOCATION (decl))
			   || global_dc->m_warn_system_headers));
  if (tree call = do_warn_dangling_reference (init, /*arg_p=*/false))
    {
      auto_diagnostic_group d;
      if (warning_at (DECL_SOURCE_LOCATION (decl), OPT_Wdangling_reference,
		      "possibly dangling reference to a temporary"))
	inform (EXPR_LOCATION (call), "the temporary was destroyed at "
		"the end of the full expression %qE", call);
    }
}

/* If *P is an xvalue expression, prevent temporary lifetime extension if it
   gets used to initialize a reference.  */

static tree
prevent_lifetime_extension (tree t)
{
  tree *p = &t;
  while (TREE_CODE (*p) == COMPOUND_EXPR)
    p = &TREE_OPERAND (*p, 1);
  while (handled_component_p (*p))
    p = &TREE_OPERAND (*p, 0);
  /* Change a TARGET_EXPR from prvalue to xvalue.  */
  if (TREE_CODE (*p) == TARGET_EXPR)
    *p = build2 (COMPOUND_EXPR, TREE_TYPE (*p), *p,
		 move (TARGET_EXPR_SLOT (*p)));
  return t;
}

/* Subroutine of extend_ref_init_temps.  Possibly extend one initializer,
   which is bound either to a reference or a std::initializer_list.  */

static tree
extend_ref_init_temps_1 (tree decl, tree init, vec<tree, va_gc> **cleanups,
			 tree *cond_guard)
{
  /* CWG1299 (C++20): The temporary object to which the reference is bound or
     the temporary object that is the complete object of a subobject to which
     the reference is bound persists for the lifetime of the reference if the
     glvalue to which the reference is bound was obtained through one of the
     following:
     - a temporary materialization conversion ([conv.rval]),
     - ( expression ), where expression is one of these expressions,
     - subscripting ([expr.sub]) of an array operand, where that operand is one
       of these expressions,
     - a class member access ([expr.ref]) using the . operator where the left
       operand is one of these expressions and the right operand designates a
       non-static data member of non-reference type,
     - a pointer-to-member operation ([expr.mptr.oper]) using the .* operator
       where the left operand is one of these expressions and the right operand
       is a pointer to data member of non-reference type,
     - a const_cast ([expr.const.cast]), static_cast ([expr.static.cast]),
       dynamic_cast ([expr.dynamic.cast]), or reinterpret_cast
       ([expr.reinterpret.cast]) converting, without a user-defined conversion,
       a glvalue operand that is one of these expressions to a glvalue that
       refers to the object designated by the operand, or to its complete
       object or a subobject thereof,
     - a conditional expression ([expr.cond]) that is a glvalue where the
       second or third operand is one of these expressions, or
     - a comma expression ([expr.comma]) that is a glvalue where the right
       operand is one of these expressions.  */

  /* FIXME several cases are still handled wrong (101572, 81420).  */

  tree sub = init;
  tree *p;
  STRIP_NOPS (sub);
  if (TREE_CODE (sub) == COMPOUND_EXPR)
    {
      TREE_OPERAND (sub, 1)
	= extend_ref_init_temps_1 (decl, TREE_OPERAND (sub, 1), cleanups,
				   cond_guard);
      return init;
    }
  if (TREE_CODE (sub) == POINTER_PLUS_EXPR
      && TYPE_PTRDATAMEM_P (TREE_TYPE (tree_strip_nop_conversions
				       (TREE_OPERAND (sub, 1)))))
    {
      /* A pointer-to-member operation.  */
      TREE_OPERAND (sub, 0)
	= extend_ref_init_temps_1 (decl, TREE_OPERAND (sub, 0), cleanups,
				   cond_guard);
      return init;
    }
  if (TREE_CODE (sub) == COND_EXPR)
    {
      tree cur_cond_guard = NULL_TREE;
      if (TREE_OPERAND (sub, 1))
	TREE_OPERAND (sub, 1)
	  = extend_ref_init_temps_1 (decl, TREE_OPERAND (sub, 1), cleanups,
				     &cur_cond_guard);
      if (cur_cond_guard)
	{
	  tree set = cp_build_modify_expr (UNKNOWN_LOCATION, cur_cond_guard,
					   NOP_EXPR, boolean_true_node,
					   tf_warning_or_error);
	  TREE_OPERAND (sub, 1)
	    = cp_build_compound_expr (set, TREE_OPERAND (sub, 1),
				      tf_warning_or_error);
	}
      cur_cond_guard = NULL_TREE;
      if (TREE_OPERAND (sub, 2))
	TREE_OPERAND (sub, 2)
	  = extend_ref_init_temps_1 (decl, TREE_OPERAND (sub, 2), cleanups,
				     &cur_cond_guard);
      if (cur_cond_guard)
	{
	  tree set = cp_build_modify_expr (UNKNOWN_LOCATION, cur_cond_guard,
					   NOP_EXPR, boolean_true_node,
					   tf_warning_or_error);
	  TREE_OPERAND (sub, 2)
	    = cp_build_compound_expr (set, TREE_OPERAND (sub, 2),
				      tf_warning_or_error);
	}
      return init;
    }
  if (TREE_CODE (sub) != ADDR_EXPR)
    return init;
  /* Deal with binding to a subobject.  */
  for (p = &TREE_OPERAND (sub, 0);
       TREE_CODE (*p) == COMPONENT_REF || TREE_CODE (*p) == ARRAY_REF; )
    p = &TREE_OPERAND (*p, 0);
  if (TREE_CODE (*p) == TARGET_EXPR)
    {
      tree subinit = NULL_TREE;
      *p = set_up_extended_ref_temp (decl, *p, cleanups, &subinit, cond_guard);
      recompute_tree_invariant_for_addr_expr (sub);
      if (init != sub)
	init = fold_convert (TREE_TYPE (init), sub);
      if (subinit)
	init = build2 (COMPOUND_EXPR, TREE_TYPE (init), subinit, init);
    }
  return init;
}

/* INIT is part of the initializer for DECL.  If there are any
   reference or initializer lists being initialized, extend their
   lifetime to match that of DECL.  */

tree
extend_ref_init_temps (tree decl, tree init, vec<tree, va_gc> **cleanups,
		       tree *cond_guard)
{
  tree type = TREE_TYPE (init);
  if (processing_template_decl)
    return init;

  maybe_warn_dangling_reference (decl, init);

  if (TYPE_REF_P (type))
    init = extend_ref_init_temps_1 (decl, init, cleanups, cond_guard);
  else
    {
      tree ctor = init;
      if (TREE_CODE (ctor) == TARGET_EXPR)
	ctor = TARGET_EXPR_INITIAL (ctor);
      if (TREE_CODE (ctor) == CONSTRUCTOR)
	{
	  /* [dcl.init] When initializing an aggregate from a parenthesized list
	     of values... a temporary object bound to a reference does not have
	     its lifetime extended.  */
	  if (CONSTRUCTOR_IS_PAREN_INIT (ctor))
	    return init;

	  if (is_std_init_list (type))
	    {
	      /* The temporary array underlying a std::initializer_list
		 is handled like a reference temporary.  */
	      tree array = CONSTRUCTOR_ELT (ctor, 0)->value;
	      array = extend_ref_init_temps_1 (decl, array, cleanups,
					       cond_guard);
	      CONSTRUCTOR_ELT (ctor, 0)->value = array;
	    }
	  else
	    {
	      unsigned i;
	      constructor_elt *p;
	      vec<constructor_elt, va_gc> *elts = CONSTRUCTOR_ELTS (ctor);
	      FOR_EACH_VEC_SAFE_ELT (elts, i, p)
		p->value = extend_ref_init_temps (decl, p->value, cleanups,
						  cond_guard);
	    }
	  recompute_constructor_flags (ctor);
	  if (decl_maybe_constant_var_p (decl) && TREE_CONSTANT (ctor))
	    DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (decl) = true;
	}
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
  if (TYPE_REF_P (type))
    return true;
  if (CLASS_TYPE_P (type))
    {
      if (is_std_init_list (type))
	return true;
      for (tree f = next_aggregate_field (TYPE_FIELDS (type));
	   f; f = next_aggregate_field (DECL_CHAIN (f)))
	if (type_has_extended_temps (TREE_TYPE (f)))
	  return true;
    }
  return false;
}

/* Returns true iff TYPE is some variant of std::initializer_list.  */

bool
is_std_init_list (tree type)
{
  if (!TYPE_P (type))
    return false;
  if (cxx_dialect == cxx98)
    return false;
  /* Look through typedefs.  */
  type = TYPE_MAIN_VARIANT (type);
  return (CLASS_TYPE_P (type)
	  && CP_TYPE_CONTEXT (type) == std_node
	  && init_list_identifier == DECL_NAME (TYPE_NAME (type)));
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

/* We know that can_convert_arg_bad already said "no" when trying to convert
   FROM to TO with ARG and FLAGS.  Try to figure out if it was because
   an explicit conversion function was skipped when looking for a way to
   perform the conversion.  At this point we've already printed an error.  */

void
maybe_show_nonconverting_candidate (tree to, tree from, tree arg, int flags)
{
  if (!(flags & LOOKUP_ONLYCONVERTING))
    return;

  conversion_obstack_sentinel cos;
  conversion *c = implicit_conversion (to, from, arg, /*c_cast_p=*/false,
				       flags & ~LOOKUP_ONLYCONVERTING, tf_none);
  if (c && !c->bad_p && c->user_conv_p)
    /* Ay, the conversion would have worked in direct-init context.  */
    for (; c; c = next_conversion (c))
      if (c->kind == ck_user
	  && DECL_P (c->cand->fn)
	  && DECL_NONCONVERTING_P (c->cand->fn))
	inform (DECL_SOURCE_LOCATION (c->cand->fn), "explicit conversion "
		"function was not considered");
}

/* We're converting EXPR to TYPE.  If that conversion involves a conversion
   function and we're binding EXPR to a reference parameter of that function,
   return true.  */

bool
conv_binds_to_reference_parm_p (tree type, tree expr)
{
  conversion_obstack_sentinel cos;
  conversion *c = implicit_conversion (type, TREE_TYPE (expr), expr,
				       /*c_cast_p=*/false, LOOKUP_NORMAL,
				       tf_none);
  if (c && !c->bad_p && c->user_conv_p)
    for (; c; c = next_conversion (c))
      if (c->kind == ck_user)
	for (z_candidate *cand = c->cand; cand; cand = cand->next)
	  if (cand->viable == 1)
	    for (size_t i = 0; i < cand->num_convs; ++i)
	      if (cand->convs[i]->kind == ck_ref_bind
		  && conv_get_original_expr (cand->convs[i]) == expr)
		return true;

  return false;
}

#include "gt-cp-call.h"
