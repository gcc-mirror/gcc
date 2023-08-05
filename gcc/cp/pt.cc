/* Handle parameterized types (templates) for GNU -*- C++ -*-.
   Copyright (C) 1992-2023 Free Software Foundation, Inc.
   Written by Ken Raeburn (raeburn@cygnus.com) while at Watchmaker Computing.
   Rewritten by Jason Merrill (jason@cygnus.com).

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

/* Known bugs or deficiencies include:

     all methods must be provided in header files; can't use a source
     file that contains only the method templates and "just win".

     Fixed by: C++20 modules.  */

#include "config.h"
#define INCLUDE_ALGORITHM // for std::equal
#include "system.h"
#include "coretypes.h"
#include "cp-tree.h"
#include "timevar.h"
#include "stringpool.h"
#include "varasm.h"
#include "attribs.h"
#include "stor-layout.h"
#include "intl.h"
#include "c-family/c-objc.h"
#include "cp-objcp-common.h"
#include "toplev.h"
#include "tree-iterator.h"
#include "type-utils.h"
#include "gimplify.h"
#include "gcc-rich-location.h"
#include "selftest.h"
#include "target.h"

/* The type of functions taking a tree, and some additional data, and
   returning an int.  */
typedef int (*tree_fn_t) (tree, void*);

/* The PENDING_TEMPLATES is a list of templates whose instantiations
   have been deferred, either because their definitions were not yet
   available, or because we were putting off doing the work.  */
struct GTY ((chain_next ("%h.next"))) pending_template
{
  struct pending_template *next;
  struct tinst_level *tinst;
};

static GTY(()) struct pending_template *pending_templates;
static GTY(()) struct pending_template *last_pending_template;

int processing_template_parmlist;
static int template_header_count;

static vec<int> inline_parm_levels;

static GTY(()) struct tinst_level *current_tinst_level;

static GTY(()) vec<tree, va_gc> *saved_access_scope;

/* Live only within one (recursive) call to tsubst_expr.  We use
   this to pass the statement expression node from the STMT_EXPR
   to the EXPR_STMT that is its result.  */
static tree cur_stmt_expr;

// -------------------------------------------------------------------------- //
// Local Specialization Stack
//
// Implementation of the RAII helper for creating new local
// specializations.
local_specialization_stack::local_specialization_stack (lss_policy policy)
  : saved (local_specializations)
{
  if (policy == lss_nop)
    ;
  else if (policy == lss_blank || !saved)
    local_specializations = new hash_map<tree, tree>;
  else
    local_specializations = new hash_map<tree, tree>(*saved);
}

local_specialization_stack::~local_specialization_stack ()
{
  if (local_specializations != saved)
    {
      delete local_specializations;
      local_specializations = saved;
    }
}

/* True if we've recursed into fn_type_unification too many times.  */
static bool excessive_deduction_depth;

struct spec_hasher : ggc_ptr_hash<spec_entry>
{
  static hashval_t hash (tree, tree);
  static hashval_t hash (spec_entry *);
  static bool equal (spec_entry *, spec_entry *);
};

/* The general template is not in these tables.  */
typedef hash_table<spec_hasher> spec_hash_table;
static GTY (()) spec_hash_table *decl_specializations;
static GTY (()) spec_hash_table *type_specializations;

/* Contains canonical template parameter types. The vector is indexed by
   the TEMPLATE_TYPE_IDX of the template parameter. Each element is a
   TREE_LIST, whose TREE_VALUEs contain the canonical template
   parameters of various types and levels.  */
static GTY(()) vec<tree, va_gc> *canonical_template_parms;

#define UNIFY_ALLOW_NONE 0
#define UNIFY_ALLOW_MORE_CV_QUAL 1
#define UNIFY_ALLOW_LESS_CV_QUAL 2
#define UNIFY_ALLOW_DERIVED 4
#define UNIFY_ALLOW_INTEGER 8
#define UNIFY_ALLOW_OUTER_LEVEL 16
#define UNIFY_ALLOW_OUTER_MORE_CV_QUAL 32
#define UNIFY_ALLOW_OUTER_LESS_CV_QUAL 64

enum template_base_result {
  tbr_incomplete_type,
  tbr_ambiguous_baseclass,
  tbr_success
};

static bool resolve_overloaded_unification (tree, tree, tree, tree,
					    unification_kind_t, int,
					    bool);
static int try_one_overload (tree, tree, tree, tree, tree,
			     unification_kind_t, int, bool, bool);
static int unify (tree, tree, tree, tree, int, bool);
static void add_pending_template (tree);
static tree reopen_tinst_level (struct tinst_level *);
static tree tsubst_initializer_list (tree, tree);
static tree get_partial_spec_bindings (tree, tree, tree);
static void tsubst_enum	(tree, tree, tree);
static bool check_instantiated_args (tree, tree, tsubst_flags_t);
static int check_non_deducible_conversion (tree, tree, unification_kind_t, int,
					   struct conversion **, bool);
static int maybe_adjust_types_for_deduction (tree, unification_kind_t,
					     tree*, tree*, tree);
static int type_unification_real (tree, tree, tree, const tree *,
				  unsigned int, int, unification_kind_t,
				  vec<deferred_access_check, va_gc> **,
				  bool);
static void note_template_header (int);
static tree convert_nontype_argument_function (tree, tree, tsubst_flags_t);
static tree convert_nontype_argument (tree, tree, tsubst_flags_t);
static tree convert_template_argument (tree, tree, tree,
				       tsubst_flags_t, int, tree);
static tree for_each_template_parm (tree, tree_fn_t, void*,
				    hash_set<tree> *, bool, tree_fn_t = NULL);
static tree expand_template_argument_pack (tree);
static tree build_template_parm_index (int, int, int, tree, tree);
static bool inline_needs_template_parms (tree, bool);
static void push_inline_template_parms_recursive (tree, int);
static tree reduce_template_parm_level (tree, tree, int, tree, tsubst_flags_t);
static int mark_template_parm (tree, void *);
static int template_parm_this_level_p (tree, void *);
static tree tsubst_friend_function (tree, tree);
static tree tsubst_friend_class (tree, tree);
static int can_complete_type_without_circularity (tree);
static tree get_bindings (tree, tree, tree, bool);
static int template_decl_level (tree);
static int check_cv_quals_for_unify (int, tree, tree);
static int unify_pack_expansion (tree, tree, tree,
				 tree, unification_kind_t, bool, bool);
static tree copy_template_args (tree);
static tree tsubst_template_parms (tree, tree, tsubst_flags_t);
static void tsubst_each_template_parm_constraints (tree, tree, tsubst_flags_t);
static tree tsubst_aggr_type (tree, tree, tsubst_flags_t, tree, int);
static tree tsubst_aggr_type_1 (tree, tree, tsubst_flags_t, tree, int);
static tree tsubst_arg_types (tree, tree, tree, tsubst_flags_t, tree);
static tree tsubst_function_type (tree, tree, tsubst_flags_t, tree);
static bool check_specialization_scope (void);
static tree process_partial_specialization (tree);
static enum template_base_result get_template_base (tree, tree, tree, tree,
						    bool , tree *);
static tree try_class_unification (tree, tree, tree, tree, bool);
static bool class_nttp_const_wrapper_p (tree t);
static int coerce_template_template_parms (tree, tree, tsubst_flags_t,
					   tree, tree);
static bool template_template_parm_bindings_ok_p (tree, tree);
static void tsubst_default_arguments (tree, tsubst_flags_t);
static tree for_each_template_parm_r (tree *, int *, void *);
static tree copy_default_args_to_explicit_spec_1 (tree, tree);
static void copy_default_args_to_explicit_spec (tree);
static bool invalid_nontype_parm_type_p (tree, tsubst_flags_t);
static bool dependent_template_arg_p (tree);
static bool dependent_type_p_r (tree);
static tree tsubst_copy	(tree, tree, tsubst_flags_t, tree);
static tree tsubst_decl (tree, tree, tsubst_flags_t, bool = true);
static tree tsubst_scope (tree, tree, tsubst_flags_t, tree);
static void perform_instantiation_time_access_checks (tree, tree);
static tree listify (tree);
static tree listify_autos (tree, tree);
static tree tsubst_template_parm (tree, tree, tsubst_flags_t);
static tree instantiate_alias_template (tree, tree, tsubst_flags_t);
static bool complex_alias_template_p (const_tree tmpl);
static tree get_underlying_template (tree);
static tree tsubst_attributes (tree, tree, tsubst_flags_t, tree);
static tree canonicalize_expr_argument (tree, tsubst_flags_t);
static tree make_argument_pack (tree);
static tree enclosing_instantiation_of (tree tctx);
static void instantiate_body (tree pattern, tree args, tree d, bool nested);
static tree maybe_dependent_member_ref (tree, tree, tsubst_flags_t, tree);
static void mark_template_arguments_used (tree, tree);
static bool uses_outer_template_parms (tree);

/* Make the current scope suitable for access checking when we are
   processing T.  T can be FUNCTION_DECL for instantiated function
   template, VAR_DECL for static member variable, or TYPE_DECL for
   for a class or alias template (needed by instantiate_decl).  */

void
push_access_scope (tree t)
{
  gcc_assert (VAR_OR_FUNCTION_DECL_P (t)
	      || TREE_CODE (t) == TYPE_DECL);

  if (DECL_FRIEND_CONTEXT (t))
    push_nested_class (DECL_FRIEND_CONTEXT (t));
  else if (DECL_IMPLICIT_TYPEDEF_P (t)
	   && CLASS_TYPE_P (TREE_TYPE (t)))
    push_nested_class (TREE_TYPE (t));
  else if (DECL_CLASS_SCOPE_P (t))
    push_nested_class (DECL_CONTEXT (t));
  else if (deduction_guide_p (t) && DECL_ARTIFICIAL (t))
    /* An artificial deduction guide should have the same access as
       the constructor.  */
    push_nested_class (TREE_TYPE (TREE_TYPE (t)));
  else
    push_to_top_level ();

  if (TREE_CODE (t) == FUNCTION_DECL)
    {
      vec_safe_push (saved_access_scope, current_function_decl);
      current_function_decl = t;
    }
}

/* Restore the scope set up by push_access_scope.  T is the node we
   are processing.  */

void
pop_access_scope (tree t)
{
  if (TREE_CODE (t) == FUNCTION_DECL)
    current_function_decl = saved_access_scope->pop();

  if (DECL_FRIEND_CONTEXT (t)
      || (DECL_IMPLICIT_TYPEDEF_P (t)
	  && CLASS_TYPE_P (TREE_TYPE (t)))
      || DECL_CLASS_SCOPE_P (t)
      || (deduction_guide_p (t) && DECL_ARTIFICIAL (t)))
    pop_nested_class ();
  else
    pop_from_top_level ();
}

/* Do any processing required when DECL (a member template
   declaration) is finished.  Returns the TEMPLATE_DECL corresponding
   to DECL, unless it is a specialization, in which case the DECL
   itself is returned.  */

tree
finish_member_template_decl (tree decl)
{
  if (decl == error_mark_node)
    return error_mark_node;

  gcc_assert (DECL_P (decl));

  if (TREE_CODE (decl) == TYPE_DECL)
    {
      tree type;

      type = TREE_TYPE (decl);
      if (type == error_mark_node)
	return error_mark_node;
      if (MAYBE_CLASS_TYPE_P (type)
	  && CLASSTYPE_TEMPLATE_INFO (type)
	  && !CLASSTYPE_TEMPLATE_SPECIALIZATION (type))
	{
	  tree tmpl = CLASSTYPE_TI_TEMPLATE (type);
	  check_member_template (tmpl);
	  return tmpl;
	}
      return NULL_TREE;
    }
  else if (TREE_CODE (decl) == FIELD_DECL)
    error_at (DECL_SOURCE_LOCATION (decl),
	      "data member %qD cannot be a member template", decl);
  else if (DECL_TEMPLATE_INFO (decl))
    {
      if (!DECL_TEMPLATE_SPECIALIZATION (decl))
	{
	  check_member_template (DECL_TI_TEMPLATE (decl));
	  return DECL_TI_TEMPLATE (decl);
	}
      else
	return decl;
    }
  else
    error_at (DECL_SOURCE_LOCATION (decl),
	      "invalid member template declaration %qD", decl);

  return error_mark_node;
}

/* Create a template info node.  */

tree
build_template_info (tree template_decl, tree template_args)
{
  tree result = make_node (TEMPLATE_INFO);
  TI_TEMPLATE (result) = template_decl;
  TI_ARGS (result) = template_args;
  return result;
}

/* Return the template info node corresponding to T, whatever T is.  */

tree
get_template_info (const_tree t)
{
  tree tinfo = NULL_TREE;

  if (!t || t == error_mark_node)
    return NULL;

  if (TREE_CODE (t) == NAMESPACE_DECL
      || TREE_CODE (t) == PARM_DECL)
    return NULL;

  if (DECL_P (t) && DECL_LANG_SPECIFIC (t))
    tinfo = DECL_TEMPLATE_INFO (t);

  if (!tinfo && DECL_IMPLICIT_TYPEDEF_P (t))
    t = TREE_TYPE (t);

  if (OVERLOAD_TYPE_P (t))
    tinfo = TYPE_TEMPLATE_INFO (t);
  else if (TREE_CODE (t) == BOUND_TEMPLATE_TEMPLATE_PARM)
    tinfo = TEMPLATE_TEMPLATE_PARM_TEMPLATE_INFO (t);

  return tinfo;
}

/* Returns the template nesting level of the indicated class TYPE.

   For example, in:
     template <class T>
     struct A
     {
       template <class U>
       struct B {};
     };

   A<T>::B<U> has depth two, while A<T> has depth one.
   Both A<T>::B<int> and A<int>::B<U> have depth one, if
   they are instantiations, not specializations.

   This function is guaranteed to return 0 if passed NULL_TREE so
   that, for example, `template_class_depth (current_class_type)' is
   always safe.  */

int
template_class_depth (tree type)
{
  int depth;

  for (depth = 0; type && TREE_CODE (type) != NAMESPACE_DECL; )
    {
      tree tinfo = get_template_info (type);

      if (tinfo
	  && TREE_CODE (TI_TEMPLATE (tinfo)) == TEMPLATE_DECL
	  && PRIMARY_TEMPLATE_P (TI_TEMPLATE (tinfo))
	  && uses_template_parms (INNERMOST_TEMPLATE_ARGS (TI_ARGS (tinfo))))
	++depth;

      if (DECL_P (type))
	{
	  if (tree fctx = DECL_FRIEND_CONTEXT (type))
	    type = fctx;
	  else
	    type = CP_DECL_CONTEXT (type);
	}
      else if (LAMBDA_TYPE_P (type) && LAMBDA_TYPE_EXTRA_SCOPE (type))
	type = LAMBDA_TYPE_EXTRA_SCOPE (type);
      else
	type = CP_TYPE_CONTEXT (type);
    }

  return depth;
}

/* Return TRUE if NODE instantiates a template that has arguments of
   its own, be it directly a primary template or indirectly through a
   partial specializations.  */
static bool
instantiates_primary_template_p (tree node)
{
  tree tinfo = get_template_info (node);
  if (!tinfo)
    return false;

  tree tmpl = TI_TEMPLATE (tinfo);
  if (PRIMARY_TEMPLATE_P (tmpl))
    return true;

  if (!DECL_TEMPLATE_SPECIALIZATION (tmpl))
    return false;

  /* So now we know we have a specialization, but it could be a full
     or a partial specialization.  To tell which, compare the depth of
     its template arguments with those of its context.  */

  tree ctxt = DECL_CONTEXT (tmpl);
  tree ctinfo = get_template_info (ctxt);
  if (!ctinfo)
    return true;

  return (TMPL_ARGS_DEPTH (TI_ARGS (tinfo))
	  > TMPL_ARGS_DEPTH (TI_ARGS (ctinfo)));
}

/* Subroutine of maybe_begin_member_template_processing.
   Returns true if processing DECL needs us to push template parms.  */

static bool
inline_needs_template_parms (tree decl, bool nsdmi)
{
  if (!decl || (!nsdmi && ! DECL_TEMPLATE_INFO (decl)))
    return false;

  return (TMPL_PARMS_DEPTH (DECL_TEMPLATE_PARMS (most_general_template (decl)))
	  > (current_template_depth + DECL_TEMPLATE_SPECIALIZATION (decl)));
}

/* Subroutine of maybe_begin_member_template_processing.
   Push the template parms in PARMS, starting from LEVELS steps into the
   chain, and ending at the beginning, since template parms are listed
   innermost first.  */

static void
push_inline_template_parms_recursive (tree parmlist, int levels)
{
  tree parms = TREE_VALUE (parmlist);
  int i;

  if (levels > 1)
    push_inline_template_parms_recursive (TREE_CHAIN (parmlist), levels - 1);

  ++processing_template_decl;
  current_template_parms
    = tree_cons (size_int (current_template_depth + 1),
		 parms, current_template_parms);
  TEMPLATE_PARMS_CONSTRAINTS (current_template_parms)
    = TEMPLATE_PARMS_CONSTRAINTS (parmlist);
  TEMPLATE_PARMS_FOR_INLINE (current_template_parms) = 1;

  begin_scope (TREE_VEC_LENGTH (parms) ? sk_template_parms : sk_template_spec,
	       NULL);
  for (i = 0; i < TREE_VEC_LENGTH (parms); ++i)
    {
      tree parm = TREE_VALUE (TREE_VEC_ELT (parms, i));

      if (error_operand_p (parm))
	continue;

      gcc_assert (DECL_P (parm));

      switch (TREE_CODE (parm))
	{
	case TYPE_DECL:
	case TEMPLATE_DECL:
	  pushdecl (parm);
	  break;

	case PARM_DECL:
	  /* Push the CONST_DECL.  */
	  pushdecl (TEMPLATE_PARM_DECL (DECL_INITIAL (parm)));
	  break;

	default:
	  gcc_unreachable ();
	}
    }
}

/* Restore the template parameter context for a member template, a
   friend template defined in a class definition, or a non-template
   member of template class.  */

void
maybe_begin_member_template_processing (tree decl)
{
  tree parms;
  int levels = 0;
  bool nsdmi = TREE_CODE (decl) == FIELD_DECL;

  if (nsdmi)
    {
      tree ctx = DECL_CONTEXT (decl);
      decl = (CLASSTYPE_TEMPLATE_INFO (ctx)
	      /* Disregard full specializations (c++/60999).  */
	      && uses_template_parms (ctx)
	      ? CLASSTYPE_TI_TEMPLATE (ctx) : NULL_TREE);
    }

  if (inline_needs_template_parms (decl, nsdmi))
    {
      parms = DECL_TEMPLATE_PARMS (most_general_template (decl));
      levels = TMPL_PARMS_DEPTH (parms) - current_template_depth;

      if (DECL_TEMPLATE_SPECIALIZATION (decl))
	{
	  --levels;
	  parms = TREE_CHAIN (parms);
	}

      push_inline_template_parms_recursive (parms, levels);
    }

  /* Remember how many levels of template parameters we pushed so that
     we can pop them later.  */
  inline_parm_levels.safe_push (levels);
}

/* Undo the effects of maybe_begin_member_template_processing.  */

void
maybe_end_member_template_processing (void)
{
  int i;
  int last;

  if (inline_parm_levels.length () == 0)
    return;

  last = inline_parm_levels.pop ();
  for (i = 0; i < last; ++i)
    {
      --processing_template_decl;
      current_template_parms = TREE_CHAIN (current_template_parms);
      poplevel (0, 0, 0);
    }
}

/* Return a new template argument vector which contains all of ARGS,
   but has as its innermost set of arguments the EXTRA_ARGS.  */

tree
add_to_template_args (tree args, tree extra_args)
{
  tree new_args;
  int extra_depth;
  int i;
  int j;

  if (args == NULL_TREE || extra_args == error_mark_node)
    return extra_args;

  extra_depth = TMPL_ARGS_DEPTH (extra_args);
  new_args = make_tree_vec (TMPL_ARGS_DEPTH (args) + extra_depth);

  for (i = 1; i <= TMPL_ARGS_DEPTH (args); ++i)
    SET_TMPL_ARGS_LEVEL (new_args, i, TMPL_ARGS_LEVEL (args, i));

  for (j = 1; j <= extra_depth; ++j, ++i)
    SET_TMPL_ARGS_LEVEL (new_args, i, TMPL_ARGS_LEVEL (extra_args, j));

  return new_args;
}

/* Like add_to_template_args, but only the outermost ARGS are added to
   the EXTRA_ARGS.  In particular, all but TMPL_ARGS_DEPTH
   (EXTRA_ARGS) levels are added.  This function is used to combine
   the template arguments from a partial instantiation with the
   template arguments used to attain the full instantiation from the
   partial instantiation.

   If ARGS is a TEMPLATE_DECL, use its parameters as args.  */

tree
add_outermost_template_args (tree args, tree extra_args)
{
  tree new_args;

  if (!args)
    return extra_args;
  if (TREE_CODE (args) == TEMPLATE_DECL)
    {
      tree ti = get_template_info (DECL_TEMPLATE_RESULT (args));
      args = TI_ARGS (ti);
    }

  /* If there are more levels of EXTRA_ARGS than there are ARGS,
     something very fishy is going on.  */
  gcc_assert (TMPL_ARGS_DEPTH (args) >= TMPL_ARGS_DEPTH (extra_args));

  /* If *all* the new arguments will be the EXTRA_ARGS, just return
     them.  */
  if (TMPL_ARGS_DEPTH (args) == TMPL_ARGS_DEPTH (extra_args))
    return extra_args;

  /* For the moment, we make ARGS look like it contains fewer levels.  */
  TREE_VEC_LENGTH (args) -= TMPL_ARGS_DEPTH (extra_args);

  new_args = add_to_template_args (args, extra_args);

  /* Now, we restore ARGS to its full dimensions.  */
  TREE_VEC_LENGTH (args) += TMPL_ARGS_DEPTH (extra_args);

  return new_args;
}

/* Return the N levels of innermost template arguments from the ARGS.  */

tree
get_innermost_template_args (tree args, int n)
{
  tree new_args;
  int extra_levels;
  int i;

  gcc_assert (n >= 0);

  /* If N is 1, just return the innermost set of template arguments.  */
  if (n == 1)
    return TMPL_ARGS_LEVEL (args, TMPL_ARGS_DEPTH (args));

  /* If we're not removing anything, just return the arguments we were
     given.  */
  extra_levels = TMPL_ARGS_DEPTH (args) - n;
  gcc_assert (extra_levels >= 0);
  if (extra_levels == 0)
    return args;

  /* Make a new set of arguments, not containing the outer arguments.  */
  new_args = make_tree_vec (n);
  for (i = 1; i <= n; ++i)
    SET_TMPL_ARGS_LEVEL (new_args, i,
			 TMPL_ARGS_LEVEL (args, i + extra_levels));

  return new_args;
}

/* The inverse of get_innermost_template_args: Return all but the innermost
   EXTRA_LEVELS levels of template arguments from the ARGS.  */

static tree
strip_innermost_template_args (tree args, int extra_levels)
{
  tree new_args;
  int n = TMPL_ARGS_DEPTH (args) - extra_levels;
  int i;

  gcc_assert (n >= 0);

  /* If N is 1, just return the outermost set of template arguments.  */
  if (n == 1)
    return TMPL_ARGS_LEVEL (args, 1);

  /* If we're not removing anything, just return the arguments we were
     given.  */
  gcc_assert (extra_levels >= 0);
  if (extra_levels == 0)
    return args;

  /* Make a new set of arguments, not containing the inner arguments.  */
  new_args = make_tree_vec (n);
  for (i = 1; i <= n; ++i)
    SET_TMPL_ARGS_LEVEL (new_args, i,
			 TMPL_ARGS_LEVEL (args, i));

  return new_args;
}

/* We've got a template header coming up; push to a new level for storing
   the parms.  */

void
begin_template_parm_list (void)
{
  /* We use a non-tag-transparent scope here, which causes pushtag to
     put tags in this scope, rather than in the enclosing class or
     namespace scope.  This is the right thing, since we want
     TEMPLATE_DECLS, and not TYPE_DECLS for template classes.  For a
     global template class, push_template_decl handles putting the
     TEMPLATE_DECL into top-level scope.  For a nested template class,
     e.g.:

       template <class T> struct S1 {
	 template <class T> struct S2 {};
       };

     pushtag contains special code to insert the TEMPLATE_DECL for S2
     at the right scope.  */
  begin_scope (sk_template_parms, NULL);
  ++processing_template_decl;
  ++processing_template_parmlist;
  note_template_header (0);

  /* Add a dummy parameter level while we process the parameter list.  */
  current_template_parms
    = tree_cons (size_int (current_template_depth + 1),
		 make_tree_vec (0),
		 current_template_parms);
}

/* This routine is called when a specialization is declared.  If it is
   invalid to declare a specialization here, an error is reported and
   false is returned, otherwise this routine will return true.  */

static bool
check_specialization_scope (void)
{
  tree scope = current_scope ();

  /* [temp.expl.spec]

     An explicit specialization shall be declared in the namespace of
     which the template is a member, or, for member templates, in the
     namespace of which the enclosing class or enclosing class
     template is a member.  An explicit specialization of a member
     function, member class or static data member of a class template
     shall be declared in the namespace of which the class template
     is a member.  */
  if (scope && TREE_CODE (scope) != NAMESPACE_DECL)
    {
      error ("explicit specialization in non-namespace scope %qD", scope);
      return false;
    }

  /* [temp.expl.spec]

     In an explicit specialization declaration for a member of a class
     template or a member template that appears in namespace scope,
     the member template and some of its enclosing class templates may
     remain unspecialized, except that the declaration shall not
     explicitly specialize a class member template if its enclosing
     class templates are not explicitly specialized as well.  */
  if (current_template_parms)
    {
      error ("enclosing class templates are not explicitly specialized");
      return false;
    }

  return true;
}

/* We've just seen template <>.  */

bool
begin_specialization (void)
{
  begin_scope (sk_template_spec, NULL);
  note_template_header (1);
  return check_specialization_scope ();
}

/* Called at then end of processing a declaration preceded by
   template<>.  */

void
end_specialization (void)
{
  finish_scope ();
  reset_specialization ();
}

/* Any template <>'s that we have seen thus far are not referring to a
   function specialization.  */

void
reset_specialization (void)
{
  processing_specialization = 0;
  template_header_count = 0;
}

/* We've just seen a template header.  If SPECIALIZATION is nonzero,
   it was of the form template <>.  */

static void
note_template_header (int specialization)
{
  processing_specialization = specialization;
  template_header_count++;
}

/* We're beginning an explicit instantiation.  */

void
begin_explicit_instantiation (void)
{
  gcc_assert (!processing_explicit_instantiation);
  processing_explicit_instantiation = true;
}


void
end_explicit_instantiation (void)
{
  gcc_assert (processing_explicit_instantiation);
  processing_explicit_instantiation = false;
}

/* An explicit specialization or partial specialization of TMPL is being
   declared.  Check that the namespace in which the specialization is
   occurring is permissible.  Returns false iff it is invalid to
   specialize TMPL in the current namespace.  */

static bool
check_specialization_namespace (tree tmpl)
{
  tree tpl_ns = decl_namespace_context (tmpl);

  /* [tmpl.expl.spec]

     An explicit specialization shall be declared in a namespace enclosing the
     specialized template. An explicit specialization whose declarator-id is
     not qualified shall be declared in the nearest enclosing namespace of the
     template, or, if the namespace is inline (7.3.1), any namespace from its
     enclosing namespace set.  */
  if (current_scope() != DECL_CONTEXT (tmpl)
      && !at_namespace_scope_p ())
    {
      error ("specialization of %qD must appear at namespace scope", tmpl);
      return false;
    }

  if (is_nested_namespace (current_namespace, tpl_ns, cxx_dialect < cxx11))
    /* Same or enclosing namespace.  */
    return true;
  else
    {
      auto_diagnostic_group d;
      if (permerror (input_location,
		     "specialization of %qD in different namespace", tmpl))
	inform (DECL_SOURCE_LOCATION (tmpl),
		"  from definition of %q#D", tmpl);
      return false;
    }
}

/* SPEC is an explicit instantiation.  Check that it is valid to
   perform this explicit instantiation in the current namespace.  */

static void
check_explicit_instantiation_namespace (tree spec)
{
  tree ns;

  /* DR 275: An explicit instantiation shall appear in an enclosing
     namespace of its template.  */
  ns = decl_namespace_context (spec);
  if (!is_nested_namespace (current_namespace, ns))
    permerror (input_location, "explicit instantiation of %qD in namespace %qD "
	       "(which does not enclose namespace %qD)",
	       spec, current_namespace, ns);
}

/* Returns true if TYPE is a new partial specialization that needs to be
   set up.  This may also modify TYPE to point to the correct (new or
   existing) constrained partial specialization.  */

static bool
maybe_new_partial_specialization (tree& type)
{
  /* An implicit instantiation of an incomplete type implies
     the definition of a new class template.

	template<typename T>
	  struct S;

	template<typename T>
	  struct S<T*>;

     Here, S<T*> is an implicit instantiation of S whose type
     is incomplete.  */
  if (CLASSTYPE_IMPLICIT_INSTANTIATION (type) && !COMPLETE_TYPE_P (type))
    return true;

  /* It can also be the case that TYPE is a completed specialization.
     Continuing the previous example, suppose we also declare:

	template<typename T>
	  requires Integral<T>
	    struct S<T*>;

     Here, S<T*> refers to the specialization S<T*> defined
     above. However, we need to differentiate definitions because
     we intend to define a new partial specialization. In this case,
     we rely on the fact that the constraints are different for
     this declaration than that above.

     Note that we also get here for injected class names and
     late-parsed template definitions. We must ensure that we
     do not create new type declarations for those cases.  */
  if (flag_concepts && CLASSTYPE_TEMPLATE_SPECIALIZATION (type))
    {
      tree tmpl = CLASSTYPE_TI_TEMPLATE (type);
      tree args = CLASSTYPE_TI_ARGS (type);

      /* If there are no template parameters, this cannot be a new
	 partial template specialization?  */
      if (!current_template_parms)
	return false;

      /* The injected-class-name is not a new partial specialization.  */
      if (DECL_SELF_REFERENCE_P (TYPE_NAME (type)))
	return false;

      /* If the constraints are not the same as those of the primary
	 then, we can probably create a new specialization.  */
      tree type_constr = current_template_constraints ();

      if (type == TREE_TYPE (tmpl))
	{
	  tree main_constr = get_constraints (tmpl);
	  if (equivalent_constraints (type_constr, main_constr))
	    return false;
	}

      /* Also, if there's a pre-existing specialization with matching
	 constraints, then this also isn't new.  */
      tree specs = DECL_TEMPLATE_SPECIALIZATIONS (tmpl);
      while (specs)
        {
          tree spec_tmpl = TREE_VALUE (specs);
          tree spec_args = TREE_PURPOSE (specs);
          tree spec_constr = get_constraints (spec_tmpl);
          if (comp_template_args (args, spec_args)
	      && equivalent_constraints (type_constr, spec_constr))
	    {
	      type = TREE_TYPE (spec_tmpl);
	      return false;
	    }
          specs = TREE_CHAIN (specs);
        }

      /* Create a new type node (and corresponding type decl)
	 for the newly declared specialization.  */
      tree t = make_class_type (TREE_CODE (type));
      CLASSTYPE_DECLARED_CLASS (t) = CLASSTYPE_DECLARED_CLASS (type);
      SET_TYPE_TEMPLATE_INFO (t, build_template_info (tmpl, args));

      /* We only need a separate type node for storing the definition of this
	 partial specialization; uses of S<T*> are unconstrained, so all are
	 equivalent.  So keep TYPE_CANONICAL the same.  */
      TYPE_CANONICAL (t) = TYPE_CANONICAL (type);

      /* Build the corresponding type decl.  */
      tree d = create_implicit_typedef (DECL_NAME (tmpl), t);
      DECL_CONTEXT (d) = TYPE_CONTEXT (t);
      DECL_SOURCE_LOCATION (d) = input_location;
      TREE_PRIVATE (d) = (current_access_specifier == access_private_node);
      TREE_PROTECTED (d) = (current_access_specifier == access_protected_node);

      set_instantiating_module (d);
      DECL_MODULE_EXPORT_P (d) = DECL_MODULE_EXPORT_P (tmpl);

      type = t;
      return true;
    }

  return false;
}

/* The TYPE is being declared.  If it is a template type, that means it
   is a partial specialization.  Do appropriate error-checking.  */

tree
maybe_process_partial_specialization (tree type)
{
  tree context;

  if (type == error_mark_node)
    return error_mark_node;

  /* A lambda that appears in specialization context is not itself a
     specialization.  */
  if (CLASS_TYPE_P (type) && CLASSTYPE_LAMBDA_EXPR (type))
    return type;

  /* An injected-class-name is not a specialization.  */
  if (DECL_SELF_REFERENCE_P (TYPE_NAME (type)))
    return type;

  if (TREE_CODE (type) == BOUND_TEMPLATE_TEMPLATE_PARM)
    {
      error ("name of class shadows template template parameter %qD",
	     TYPE_NAME (type));
      return error_mark_node;
    }

  context = TYPE_CONTEXT (type);

  if (TYPE_ALIAS_P (type))
    {
      tree tinfo = TYPE_ALIAS_TEMPLATE_INFO (type);

      if (tinfo && DECL_ALIAS_TEMPLATE_P (TI_TEMPLATE (tinfo)))
	error ("specialization of alias template %qD",
	       TI_TEMPLATE (tinfo));
      else
	error ("explicit specialization of non-template %qT", type);
      return error_mark_node;
    }
  else if (CLASS_TYPE_P (type) && CLASSTYPE_USE_TEMPLATE (type))
    {
      /* This is for ordinary explicit specialization and partial
	 specialization of a template class such as:

	   template <> class C<int>;

	 or:

	   template <class T> class C<T*>;

	 Make sure that `C<int>' and `C<T*>' are implicit instantiations.  */

      if (maybe_new_partial_specialization (type))
	{
	  if (!check_specialization_namespace (CLASSTYPE_TI_TEMPLATE (type))
	      && !at_namespace_scope_p ())
	    return error_mark_node;
	  SET_CLASSTYPE_TEMPLATE_SPECIALIZATION (type);
	  DECL_SOURCE_LOCATION (TYPE_MAIN_DECL (type)) = input_location;
	  if (processing_template_decl)
	    {
	      tree decl = push_template_decl (TYPE_MAIN_DECL (type));
	      if (decl == error_mark_node)
		return error_mark_node;
	      return TREE_TYPE (decl);
	    }
	}
      else if (CLASSTYPE_TEMPLATE_INSTANTIATION (type))
	error ("specialization of %qT after instantiation", type);
      else if (errorcount && !processing_specialization
	        && CLASSTYPE_TEMPLATE_SPECIALIZATION (type)
	       && !uses_template_parms (CLASSTYPE_TI_ARGS (type)))
	/* Trying to define a specialization either without a template<> header
	   or in an inappropriate place.  We've already given an error, so just
	   bail now so we don't actually define the specialization.  */
	return error_mark_node;
    }
  else if (CLASS_TYPE_P (type)
	   && !CLASSTYPE_USE_TEMPLATE (type)
	   && CLASSTYPE_TEMPLATE_INFO (type)
	   && context && CLASS_TYPE_P (context)
	   && CLASSTYPE_TEMPLATE_INFO (context))
    {
      /* This is for an explicit specialization of member class
	 template according to [temp.expl.spec/18]:

	   template <> template <class U> class C<int>::D;

	 The context `C<int>' must be an implicit instantiation.
	 Otherwise this is just a member class template declared
	 earlier like:

	   template <> class C<int> { template <class U> class D; };
	   template <> template <class U> class C<int>::D;

	 In the first case, `C<int>::D' is a specialization of `C<T>::D'
	 while in the second case, `C<int>::D' is a primary template
	 and `C<T>::D' may not exist.  */

      if (CLASSTYPE_IMPLICIT_INSTANTIATION (context)
	  && !COMPLETE_TYPE_P (type))
	{
	  tree t;
	  tree tmpl = CLASSTYPE_TI_TEMPLATE (type);

	  if (current_namespace
	      != decl_namespace_context (tmpl))
	    {
	      if (permerror (input_location,
			     "specialization of %qD in different namespace",
			     type))
		inform (DECL_SOURCE_LOCATION (tmpl),
			"from definition of %q#D", tmpl);
	    }

	  /* Check for invalid specialization after instantiation:

	       template <> template <> class C<int>::D<int>;
	       template <> template <class U> class C<int>::D;  */

	  for (t = DECL_TEMPLATE_INSTANTIATIONS (tmpl);
	       t; t = TREE_CHAIN (t))
	    {
	      tree inst = TREE_VALUE (t);
	      if (CLASSTYPE_TEMPLATE_SPECIALIZATION (inst)
		  || !COMPLETE_OR_OPEN_TYPE_P (inst))
		{
		  /* We already have a full specialization of this partial
		     instantiation, or a full specialization has been
		     looked up but not instantiated.  Reassign it to the
		     new member specialization template.  */
		  spec_entry elt;
		  spec_entry *entry;

		  elt.tmpl = most_general_template (tmpl);
		  elt.args = CLASSTYPE_TI_ARGS (inst);
		  elt.spec = inst;

		  type_specializations->remove_elt (&elt);

		  elt.tmpl = tmpl;
		  CLASSTYPE_TI_ARGS (inst)
		    = elt.args = INNERMOST_TEMPLATE_ARGS (elt.args);

		  spec_entry **slot
		    = type_specializations->find_slot (&elt, INSERT);
		  entry = ggc_alloc<spec_entry> ();
		  *entry = elt;
		  *slot = entry;
		}
	      else
		/* But if we've had an implicit instantiation, that's a
		   problem ([temp.expl.spec]/6).  */
		error ("specialization %qT after instantiation %qT",
		       type, inst);
	    }

	  /* Mark TYPE as a specialization.  And as a result, we only
	     have one level of template argument for the innermost
	     class template.  */
	  SET_CLASSTYPE_TEMPLATE_SPECIALIZATION (type);
	  DECL_SOURCE_LOCATION (TYPE_MAIN_DECL (type)) = input_location;
	  CLASSTYPE_TI_ARGS (type)
	    = INNERMOST_TEMPLATE_ARGS (CLASSTYPE_TI_ARGS (type));
	}
    }
  else if (processing_specialization)
    {
       /* Someday C++0x may allow for enum template specialization.  */
      if (cxx_dialect > cxx98 && TREE_CODE (type) == ENUMERAL_TYPE
	  && CLASS_TYPE_P (context) && CLASSTYPE_USE_TEMPLATE (context))
	pedwarn (input_location, OPT_Wpedantic, "template specialization "
		 "of %qD not allowed by ISO C++", type);
      else
	{
	  error ("explicit specialization of non-template %qT", type);
	  return error_mark_node;
	}
    }

  return type;
}

/* Make sure ARGS doesn't use any inappropriate typedefs; we should have
   gone through coerce_template_parms by now.  */

static void
verify_unstripped_args_1 (tree inner)
{
  for (int i = 0; i < TREE_VEC_LENGTH (inner); ++i)
    {
      tree arg = TREE_VEC_ELT (inner, i);
      if (TREE_CODE (arg) == TEMPLATE_DECL)
	/* OK */;
      else if (TYPE_P (arg))
	gcc_assert (strip_typedefs (arg, NULL) == arg);
      else if (ARGUMENT_PACK_P (arg))
	verify_unstripped_args_1 (ARGUMENT_PACK_ARGS (arg));
      else if (strip_typedefs (TREE_TYPE (arg), NULL) != TREE_TYPE (arg))
	/* Allow typedefs on the type of a non-type argument, since a
	   parameter can have them.  */;
      else
	gcc_assert (strip_typedefs_expr (arg, NULL) == arg);
    }
}

static void
verify_unstripped_args (tree args)
{
  ++processing_template_decl;
  if (!any_dependent_template_arguments_p (args))
    verify_unstripped_args_1 (INNERMOST_TEMPLATE_ARGS (args));
  --processing_template_decl;
}

/* Retrieve the specialization (in the sense of [temp.spec] - a
   specialization is either an instantiation or an explicit
   specialization) of TMPL for the given template ARGS.  If there is
   no such specialization, return NULL_TREE.  The ARGS are a vector of
   arguments, or a vector of vectors of arguments, in the case of
   templates with more than one level of parameters.

   If TMPL is a type template and CLASS_SPECIALIZATIONS_P is true,
   then we search for a partial specialization matching ARGS.  This
   parameter is ignored if TMPL is not a class template.

   We can also look up a FIELD_DECL, if it is a lambda capture pack; the
   result is a NONTYPE_ARGUMENT_PACK.  */

static tree
retrieve_specialization (tree tmpl, tree args, hashval_t hash)
{
  if (tmpl == NULL_TREE)
    return NULL_TREE;

  if (args == error_mark_node)
    return NULL_TREE;

  gcc_assert (TREE_CODE (tmpl) == TEMPLATE_DECL
	      || TREE_CODE (tmpl) == FIELD_DECL);

  /* There should be as many levels of arguments as there are
     levels of parameters.  */
  gcc_assert (TMPL_ARGS_DEPTH (args)
	      == (TREE_CODE (tmpl) == TEMPLATE_DECL
		  ? TMPL_PARMS_DEPTH (DECL_TEMPLATE_PARMS (tmpl))
		  : template_class_depth (DECL_CONTEXT (tmpl))));

  if (flag_checking)
    verify_unstripped_args (args);

  /* Lambda functions in templates aren't instantiated normally, but through
     tsubst_lambda_expr.  */
  if (lambda_fn_in_template_p (tmpl))
    return NULL_TREE;

  spec_entry elt;
  elt.tmpl = tmpl;
  elt.args = args;
  elt.spec = NULL_TREE;

  spec_hash_table *specializations;
  if (DECL_CLASS_TEMPLATE_P (tmpl))
    specializations = type_specializations;
  else
    specializations = decl_specializations;

  if (hash == 0)
    hash = spec_hasher::hash (&elt);
  if (spec_entry *found = specializations->find_with_hash (&elt, hash))
    return found->spec;

  return NULL_TREE;
}

/* Like retrieve_specialization, but for local declarations.  */

tree
retrieve_local_specialization (tree tmpl)
{
  if (local_specializations == NULL)
    return NULL_TREE;

  tree *slot = local_specializations->get (tmpl);
  return slot ? *slot : NULL_TREE;
}

/* Returns nonzero iff DECL is a specialization of TMPL.  */

int
is_specialization_of (tree decl, tree tmpl)
{
  tree t;

  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      for (t = decl;
	   t != NULL_TREE;
	   t = DECL_TEMPLATE_INFO (t) ? DECL_TI_TEMPLATE (t) : NULL_TREE)
	if (t == tmpl)
	  return 1;
    }
  else
    {
      gcc_assert (TREE_CODE (decl) == TYPE_DECL);

      for (t = TREE_TYPE (decl);
	   t != NULL_TREE;
	   t = CLASSTYPE_USE_TEMPLATE (t)
	     ? TREE_TYPE (CLASSTYPE_TI_TEMPLATE (t)) : NULL_TREE)
	if (same_type_ignoring_top_level_qualifiers_p (t, TREE_TYPE (tmpl)))
	  return 1;
    }

  return 0;
}

/* Returns nonzero iff DECL is a specialization of friend declaration
   FRIEND_DECL according to [temp.friend].  */

bool
is_specialization_of_friend (tree decl, tree friend_decl)
{
  bool need_template = true;
  int template_depth;

  gcc_assert (TREE_CODE (decl) == FUNCTION_DECL
	      || TREE_CODE (decl) == TYPE_DECL);

  /* For [temp.friend/6] when FRIEND_DECL is an ordinary member function
     of a template class, we want to check if DECL is a specialization
     if this.  */
  if (TREE_CODE (friend_decl) == FUNCTION_DECL
      && DECL_CLASS_SCOPE_P (friend_decl)
      && DECL_TEMPLATE_INFO (friend_decl)
      && !DECL_USE_TEMPLATE (friend_decl))
    {
      /* We want a TEMPLATE_DECL for `is_specialization_of'.  */
      friend_decl = DECL_TI_TEMPLATE (friend_decl);
      need_template = false;
    }
  else if (TREE_CODE (friend_decl) == TEMPLATE_DECL
	   && !PRIMARY_TEMPLATE_P (friend_decl))
    need_template = false;

  /* There is nothing to do if this is not a template friend.  */
  if (TREE_CODE (friend_decl) != TEMPLATE_DECL)
    return false;

  if (is_specialization_of (decl, friend_decl))
    return true;

  /* [temp.friend/6]
     A member of a class template may be declared to be a friend of a
     non-template class.  In this case, the corresponding member of
     every specialization of the class template is a friend of the
     class granting friendship.

     For example, given a template friend declaration

       template <class T> friend void A<T>::f();

     the member function below is considered a friend

       template <> struct A<int> {
	 void f();
       };

     For this type of template friend, TEMPLATE_DEPTH below will be
     nonzero.  To determine if DECL is a friend of FRIEND, we first
     check if the enclosing class is a specialization of another.  */

  template_depth = template_class_depth (CP_DECL_CONTEXT (friend_decl));
  if (template_depth
      && DECL_CLASS_SCOPE_P (decl)
      && is_specialization_of (TYPE_NAME (DECL_CONTEXT (decl)),
			       CLASSTYPE_TI_TEMPLATE (DECL_CONTEXT (friend_decl))))
    {
      /* Next, we check the members themselves.  In order to handle
	 a few tricky cases, such as when FRIEND_DECL's are

	   template <class T> friend void A<T>::g(T t);
	   template <class T> template <T t> friend void A<T>::h();

	 and DECL's are

	   void A<int>::g(int);
	   template <int> void A<int>::h();

	 we need to figure out ARGS, the template arguments from
	 the context of DECL.  This is required for template substitution
	 of `T' in the function parameter of `g' and template parameter
	 of `h' in the above examples.  Here ARGS corresponds to `int'.  */

      tree context = DECL_CONTEXT (decl);
      tree args = NULL_TREE;
      int current_depth = 0;

      while (current_depth < template_depth)
	{
	  if (CLASSTYPE_TEMPLATE_INFO (context))
	    {
	      if (current_depth == 0)
		args = TYPE_TI_ARGS (context);
	      else
		args = add_to_template_args (TYPE_TI_ARGS (context), args);
	      current_depth++;
	    }
	  context = TYPE_CONTEXT (context);
	}

      if (TREE_CODE (decl) == FUNCTION_DECL)
	{
	  bool is_template;
	  tree friend_type;
	  tree decl_type;
	  tree friend_args_type;
	  tree decl_args_type;

	  /* Make sure that both DECL and FRIEND_DECL are templates or
	     non-templates.  */
	  is_template = DECL_TEMPLATE_INFO (decl)
			&& PRIMARY_TEMPLATE_P (DECL_TI_TEMPLATE (decl));
	  if (need_template ^ is_template)
	    return false;
	  else if (is_template)
	    {
	      /* If both are templates, check template parameter list.  */
	      tree friend_parms
		= tsubst_template_parms (DECL_TEMPLATE_PARMS (friend_decl),
					 args, tf_none);
	      if (!comp_template_parms
		     (DECL_TEMPLATE_PARMS (DECL_TI_TEMPLATE (decl)),
		      friend_parms))
		return false;

	      decl_type = TREE_TYPE (DECL_TI_TEMPLATE (decl));
	    }
	  else
	    decl_type = TREE_TYPE (decl);

	  friend_type = tsubst_function_type (TREE_TYPE (friend_decl), args,
					      tf_none, NULL_TREE);
	  if (friend_type == error_mark_node)
	    return false;

	  /* Check if return types match.  */
	  if (!same_type_p (TREE_TYPE (decl_type), TREE_TYPE (friend_type)))
	    return false;

	  /* Check if function parameter types match, ignoring the
	     `this' parameter.  */
	  friend_args_type = TYPE_ARG_TYPES (friend_type);
	  decl_args_type = TYPE_ARG_TYPES (decl_type);
	  if (DECL_NONSTATIC_MEMBER_FUNCTION_P (friend_decl))
	    friend_args_type = TREE_CHAIN (friend_args_type);
	  if (DECL_NONSTATIC_MEMBER_FUNCTION_P (decl))
	    decl_args_type = TREE_CHAIN (decl_args_type);

	  return compparms (decl_args_type, friend_args_type);
	}
      else
	{
	  /* DECL is a TYPE_DECL */
	  bool is_template;
	  tree decl_type = TREE_TYPE (decl);

	  /* Make sure that both DECL and FRIEND_DECL are templates or
	     non-templates.  */
	  is_template
	    = CLASSTYPE_TEMPLATE_INFO (decl_type)
	      && PRIMARY_TEMPLATE_P (CLASSTYPE_TI_TEMPLATE (decl_type));

	  if (need_template ^ is_template)
	    return false;
	  else if (is_template)
	    {
	      tree friend_parms;
	      /* If both are templates, check the name of the two
		 TEMPLATE_DECL's first because is_friend didn't.  */
	      if (DECL_NAME (CLASSTYPE_TI_TEMPLATE (decl_type))
		  != DECL_NAME (friend_decl))
		return false;

	      /* Now check template parameter list.  */
	      friend_parms
		= tsubst_template_parms (DECL_TEMPLATE_PARMS (friend_decl),
					 args, tf_none);
	      return comp_template_parms
		(DECL_TEMPLATE_PARMS (CLASSTYPE_TI_TEMPLATE (decl_type)),
		 friend_parms);
	    }
	  else
	    return (DECL_NAME (decl)
		    == DECL_NAME (friend_decl));
	}
    }
  return false;
}

/* Register the specialization SPEC as a specialization of TMPL with
   the indicated ARGS.  IS_FRIEND indicates whether the specialization
   is actually just a friend declaration.  ATTRLIST is the list of
   attributes that the specialization is declared with or NULL when
   it isn't.  Returns SPEC, or an equivalent prior declaration, if
   available.

   We also store instantiations of field packs in the hash table, even
   though they are not themselves templates, to make lookup easier.  */

static tree
register_specialization (tree spec, tree tmpl, tree args, bool is_friend,
			 hashval_t hash)
{
  tree fn;

  gcc_assert ((TREE_CODE (tmpl) == TEMPLATE_DECL && DECL_P (spec))
	      || (TREE_CODE (tmpl) == FIELD_DECL
		  && TREE_CODE (spec) == NONTYPE_ARGUMENT_PACK));

  if (TREE_CODE (spec) == FUNCTION_DECL
      && uses_template_parms (DECL_TI_ARGS (spec)))
    /* This is the FUNCTION_DECL for a partial instantiation.  Don't
       register it; we want the corresponding TEMPLATE_DECL instead.
       We use `uses_template_parms (DECL_TI_ARGS (spec))' rather than
       the more obvious `uses_template_parms (spec)' to avoid problems
       with default function arguments.  In particular, given
       something like this:

	  template <class T> void f(T t1, T t = T())

       the default argument expression is not substituted for in an
       instantiation unless and until it is actually needed.  */
    return spec;

  spec_entry elt;
  elt.tmpl = tmpl;
  elt.args = args;
  elt.spec = spec;

  if (hash == 0)
    hash = spec_hasher::hash (&elt);

  spec_entry **slot = decl_specializations->find_slot_with_hash (&elt, hash, INSERT);
  if (*slot)
    fn = (*slot)->spec;
  else
    fn = NULL_TREE;

  /* We can sometimes try to re-register a specialization that we've
     already got.  In particular, regenerate_decl_from_template calls
     duplicate_decls which will update the specialization list.  But,
     we'll still get called again here anyhow.  It's more convenient
     to simply allow this than to try to prevent it.  */
  if (fn == spec)
    return spec;
  else if (fn && DECL_TEMPLATE_SPECIALIZATION (spec))
    {
      if (DECL_TEMPLATE_INSTANTIATION (fn))
	{
	  if (DECL_ODR_USED (fn)
	      || DECL_EXPLICIT_INSTANTIATION (fn))
	    {
	      error ("specialization of %qD after instantiation",
		     fn);
	      return error_mark_node;
	    }
	  else
	    {
	      tree clone;
	      /* This situation should occur only if the first
		 specialization is an implicit instantiation, the
		 second is an explicit specialization, and the
		 implicit instantiation has not yet been used.  That
		 situation can occur if we have implicitly
		 instantiated a member function and then specialized
		 it later.

		 We can also wind up here if a friend declaration that
		 looked like an instantiation turns out to be a
		 specialization:

		   template <class T> void foo(T);
		   class S { friend void foo<>(int) };
		   template <> void foo(int);

		 We transform the existing DECL in place so that any
		 pointers to it become pointers to the updated
		 declaration.

		 If there was a definition for the template, but not
		 for the specialization, we want this to look as if
		 there were no definition, and vice versa.  */
	      DECL_INITIAL (fn) = NULL_TREE;
	      duplicate_decls (spec, fn, /*hiding=*/is_friend);

	      /* The call to duplicate_decls will have applied
		 [temp.expl.spec]:

		   An explicit specialization of a function template
		   is inline only if it is explicitly declared to be,
		   and independently of whether its function template
		   is.

		to the primary function; now copy the inline bits to
		the various clones.  */
	      FOR_EACH_CLONE (clone, fn)
		{
		  DECL_DECLARED_INLINE_P (clone)
		    = DECL_DECLARED_INLINE_P (fn);
		  DECL_SOURCE_LOCATION (clone)
		    = DECL_SOURCE_LOCATION (fn);
		  DECL_DELETED_FN (clone)
		    = DECL_DELETED_FN (fn);
		}
	      check_specialization_namespace (tmpl);

	      return fn;
	    }
	}
      else if (DECL_TEMPLATE_SPECIALIZATION (fn))
	{
	  tree dd = duplicate_decls (spec, fn, /*hiding=*/is_friend);
	  if (dd == error_mark_node)
	    /* We've already complained in duplicate_decls.  */
	    return error_mark_node;

	  if (dd == NULL_TREE && DECL_INITIAL (spec))
	    /* Dup decl failed, but this is a new definition. Set the
	       line number so any errors match this new
	       definition.  */
	    DECL_SOURCE_LOCATION (fn) = DECL_SOURCE_LOCATION (spec);

	  return fn;
	}
    }
  else if (fn)
    return duplicate_decls (spec, fn, /*hiding=*/is_friend);

  /* A specialization must be declared in the same namespace as the
     template it is specializing.  */
  if (DECL_P (spec) && DECL_TEMPLATE_SPECIALIZATION (spec)
      && !check_specialization_namespace (tmpl))
    DECL_CONTEXT (spec) = DECL_CONTEXT (tmpl);

  spec_entry *entry = ggc_alloc<spec_entry> ();
  gcc_assert (tmpl && args && spec);
  *entry = elt;
  *slot = entry;
  if ((TREE_CODE (spec) == FUNCTION_DECL && DECL_NAMESPACE_SCOPE_P (spec)
       && PRIMARY_TEMPLATE_P (tmpl)
       && DECL_SAVED_TREE (DECL_TEMPLATE_RESULT (tmpl)) == NULL_TREE)
      || variable_template_p (tmpl))
    /* If TMPL is a forward declaration of a template function, keep a list
       of all specializations in case we need to reassign them to a friend
       template later in tsubst_friend_function.

       Also keep a list of all variable template instantiations so that
       process_partial_specialization can check whether a later partial
       specialization would have used it.  */
    DECL_TEMPLATE_INSTANTIATIONS (tmpl)
      = tree_cons (args, spec, DECL_TEMPLATE_INSTANTIATIONS (tmpl));

  return spec;
}

/* Restricts tree and type comparisons.  */
int comparing_specializations;
int comparing_dependent_aliases;

/* Returns true iff two spec_entry nodes are equivalent.  */

bool
spec_hasher::equal (spec_entry *e1, spec_entry *e2)
{
  int equal;

  ++comparing_specializations;
  ++comparing_dependent_aliases;
  ++processing_template_decl;
  equal = (e1->tmpl == e2->tmpl
	   && comp_template_args (e1->args, e2->args));
  if (equal && flag_concepts
      /* tmpl could be a FIELD_DECL for a capture pack.  */
      && TREE_CODE (e1->tmpl) == TEMPLATE_DECL
      && VAR_P (DECL_TEMPLATE_RESULT (e1->tmpl))
      && uses_template_parms (e1->args))
    {
      /* Partial specializations of a variable template can be distinguished by
	 constraints.  */
      tree c1 = e1->spec ? get_constraints (e1->spec) : NULL_TREE;
      tree c2 = e2->spec ? get_constraints (e2->spec) : NULL_TREE;
      equal = equivalent_constraints (c1, c2);
    }
  --processing_template_decl;
  --comparing_dependent_aliases;
  --comparing_specializations;

  return equal;
}

/* Returns a hash for a template TMPL and template arguments ARGS.  */

static hashval_t
hash_tmpl_and_args (tree tmpl, tree args)
{
  hashval_t val = iterative_hash_object (DECL_UID (tmpl), 0);
  return iterative_hash_template_arg (args, val);
}

hashval_t
spec_hasher::hash (tree tmpl, tree args)
{
  ++comparing_specializations;
  hashval_t val = hash_tmpl_and_args (tmpl, args);
  --comparing_specializations;
  return val;
}

/* Returns a hash for a spec_entry node based on the TMPL and ARGS members,
   ignoring SPEC.  */

hashval_t
spec_hasher::hash (spec_entry *e)
{
  return spec_hasher::hash (e->tmpl, e->args);
}

/* Recursively calculate a hash value for a template argument ARG, for use
   in the hash tables of template specializations.   We must be
   careful to (at least) skip the same entities template_args_equal
   does.  */

hashval_t
iterative_hash_template_arg (tree arg, hashval_t val)
{
  if (arg == NULL_TREE)
    return iterative_hash_object (arg, val);

  if (!TYPE_P (arg))
    /* Strip nop-like things, but not the same as STRIP_NOPS.  */
    while (CONVERT_EXPR_P (arg)
	   || TREE_CODE (arg) == NON_LVALUE_EXPR
	   || class_nttp_const_wrapper_p (arg))
      arg = TREE_OPERAND (arg, 0);

  enum tree_code code = TREE_CODE (arg);

  val = iterative_hash_object (code, val);

  switch (code)
    {
    case ARGUMENT_PACK_SELECT:
      /* Getting here with an ARGUMENT_PACK_SELECT means we're probably
	 preserving it in a hash table, which is bad because it will change
	 meaning when gen_elem_of_pack_expansion_instantiation changes the
	 ARGUMENT_PACK_SELECT_INDEX.  */
      gcc_unreachable ();

    case ERROR_MARK:
      return val;

    case IDENTIFIER_NODE:
      return iterative_hash_object (IDENTIFIER_HASH_VALUE (arg), val);

    case TREE_VEC:
      for (tree elt : tree_vec_range (arg))
	val = iterative_hash_template_arg (elt, val);
      return val;

    case TYPE_PACK_EXPANSION:
    case EXPR_PACK_EXPANSION:
      val = iterative_hash_template_arg (PACK_EXPANSION_PATTERN (arg), val);
      return iterative_hash_template_arg (PACK_EXPANSION_EXTRA_ARGS (arg), val);

    case TYPE_ARGUMENT_PACK:
    case NONTYPE_ARGUMENT_PACK:
      return iterative_hash_template_arg (ARGUMENT_PACK_ARGS (arg), val);

    case TREE_LIST:
      for (; arg; arg = TREE_CHAIN (arg))
	val = iterative_hash_template_arg (TREE_VALUE (arg), val);
      return val;

    case OVERLOAD:
      for (lkp_iterator iter (arg); iter; ++iter)
	val = iterative_hash_template_arg (*iter, val);
      return val;

    case CONSTRUCTOR:
      {
	iterative_hash_template_arg (TREE_TYPE (arg), val);
	for (auto &e: CONSTRUCTOR_ELTS (arg))
	  {
	    val = iterative_hash_template_arg (e.index, val);
	    val = iterative_hash_template_arg (e.value, val);
	  }
	return val;
      }

    case PARM_DECL:
      if (!DECL_ARTIFICIAL (arg))
	{
	  val = iterative_hash_object (DECL_PARM_INDEX (arg), val);
	  val = iterative_hash_object (DECL_PARM_LEVEL (arg), val);
	}
      return iterative_hash_template_arg (TREE_TYPE (arg), val);

    case TARGET_EXPR:
      return iterative_hash_template_arg (TARGET_EXPR_INITIAL (arg), val);

    case PTRMEM_CST:
      val = iterative_hash_template_arg (PTRMEM_CST_CLASS (arg), val);
      return iterative_hash_template_arg (PTRMEM_CST_MEMBER (arg), val);

    case TEMPLATE_PARM_INDEX:
      val = iterative_hash_template_arg
	(TREE_TYPE (TEMPLATE_PARM_DECL (arg)), val);
      val = iterative_hash_object (TEMPLATE_PARM_LEVEL (arg), val);
      return iterative_hash_object (TEMPLATE_PARM_IDX (arg), val);

    case TRAIT_EXPR:
      val = iterative_hash_object (TRAIT_EXPR_KIND (arg), val);
      val = iterative_hash_template_arg (TRAIT_EXPR_TYPE1 (arg), val);
      return iterative_hash_template_arg (TRAIT_EXPR_TYPE2 (arg), val);

    case BASELINK:
      val = iterative_hash_template_arg (BINFO_TYPE (BASELINK_BINFO (arg)),
					 val);
      return iterative_hash_template_arg (DECL_NAME (get_first_fn (arg)),
					  val);

    case MODOP_EXPR:
      val = iterative_hash_template_arg (TREE_OPERAND (arg, 0), val);
      code = TREE_CODE (TREE_OPERAND (arg, 1));
      val = iterative_hash_object (code, val);
      return iterative_hash_template_arg (TREE_OPERAND (arg, 2), val);

    case LAMBDA_EXPR:
      /* [temp.over.link] Two lambda-expressions are never considered
	 equivalent.

         So just hash the closure type.  */
      return iterative_hash_template_arg (TREE_TYPE (arg), val);

    case CAST_EXPR:
    case IMPLICIT_CONV_EXPR:
    case STATIC_CAST_EXPR:
    case REINTERPRET_CAST_EXPR:
    case CONST_CAST_EXPR:
    case DYNAMIC_CAST_EXPR:
    case NEW_EXPR:
      val = iterative_hash_template_arg (TREE_TYPE (arg), val);
      /* Now hash operands as usual.  */
      break;

    case CALL_EXPR:
      {
	tree fn = CALL_EXPR_FN (arg);
	if (tree name = call_expr_dependent_name (arg))
	  {
	    if (TREE_CODE (fn) == TEMPLATE_ID_EXPR)
	      val = iterative_hash_template_arg (TREE_OPERAND (fn, 1), val);
	    fn = name;
	  }
	val = iterative_hash_template_arg (fn, val);
	call_expr_arg_iterator ai;
	for (tree x = first_call_expr_arg (arg, &ai); x;
	     x = next_call_expr_arg (&ai))
	  val = iterative_hash_template_arg (x, val);
	return val;
      }

    default:
      break;
    }

  char tclass = TREE_CODE_CLASS (code);
  switch (tclass)
    {
    case tcc_type:
      if (tree ats = alias_template_specialization_p (arg, nt_transparent))
	{
	  // We want an alias specialization that survived strip_typedefs
	  // to hash differently from its TYPE_CANONICAL, to avoid hash
	  // collisions that compare as different in template_args_equal.
	  // These could be dependent specializations that strip_typedefs
	  // left alone, or untouched specializations because
	  // coerce_template_parms returns the unconverted template
	  // arguments if it sees incomplete argument packs.
	  tree ti = TYPE_ALIAS_TEMPLATE_INFO (ats);
	  return hash_tmpl_and_args (TI_TEMPLATE (ti), TI_ARGS (ti));
	}

      switch (code)
	{
	case  DECLTYPE_TYPE:
	  val = iterative_hash_template_arg (DECLTYPE_TYPE_EXPR (arg), val);
	  break;

	case TYPENAME_TYPE:
	  if (comparing_specializations)
	    {
	      /* Hash the components that are relevant to TYPENAME_TYPE
		 equivalence as determined by structural_comptypes.  We
		 can only coherently do this when comparing_specializations
		 is set, because otherwise structural_comptypes tries
		 resolving TYPENAME_TYPE via the current instantiation.  */
	      tree context = TYPE_MAIN_VARIANT (TYPE_CONTEXT (arg));
	      tree fullname = TYPENAME_TYPE_FULLNAME (arg);
	      val = iterative_hash_template_arg (context, val);
	      val = iterative_hash_template_arg (fullname, val);
	    }
	  break;

	default:
	  if (tree canonical = TYPE_CANONICAL (arg))
	    val = iterative_hash_object (TYPE_HASH (canonical), val);
	  break;
	}

      return val;

    case tcc_declaration:
    case tcc_constant:
      return iterative_hash_expr (arg, val);

    default:
      gcc_assert (IS_EXPR_CODE_CLASS (tclass));
      for (int i = 0, n = cp_tree_operand_length (arg); i < n; ++i)
	val = iterative_hash_template_arg (TREE_OPERAND (arg, i), val);
      return val;
    }
}

/* Unregister the specialization SPEC as a specialization of TMPL.
   Replace it with NEW_SPEC, if NEW_SPEC is non-NULL.  Returns true
   if the SPEC was listed as a specialization of TMPL.

   Note that SPEC has been ggc_freed, so we can't look inside it.  */

bool
reregister_specialization (tree spec, tree tinfo, tree new_spec)
{
  spec_entry *entry;
  spec_entry elt;

  elt.tmpl = most_general_template (TI_TEMPLATE (tinfo));
  elt.args = TI_ARGS (tinfo);
  elt.spec = NULL_TREE;

  entry = decl_specializations->find (&elt);
  if (entry != NULL)
    {
      gcc_assert (entry->spec == spec || entry->spec == new_spec);
      gcc_assert (new_spec != NULL_TREE);
      entry->spec = new_spec;
      return 1;
    }

  return 0;
}

/* Like register_specialization, but for local declarations.  We are
   registering SPEC, an instantiation of TMPL.  */

void
register_local_specialization (tree spec, tree tmpl)
{
  gcc_assert (tmpl != spec);
  local_specializations->put (tmpl, spec);
}

/* Registers T as a specialization of itself.  This is used to preserve
   the references to already-parsed parameters when instantiating
   postconditions.  */

void
register_local_identity (tree t)
{
  local_specializations->put (t, t);
}

/* TYPE is a class type.  Returns true if TYPE is an explicitly
   specialized class.  */

bool
explicit_class_specialization_p (tree type)
{
  if (!CLASSTYPE_TEMPLATE_SPECIALIZATION (type))
    return false;
  return !uses_template_parms (CLASSTYPE_TI_ARGS (type));
}

/* Print the list of functions at FNS, going through all the overloads
   for each element of the list.  Alternatively, FNS cannot be a
   TREE_LIST, in which case it will be printed together with all the
   overloads.

   MORE and *STR should respectively be FALSE and NULL when the function
   is called from the outside.  They are used internally on recursive
   calls.  print_candidates manages the two parameters and leaves NULL
   in *STR when it ends.  */

static void
print_candidates_1 (tree fns, char **str, bool more = false)
{
  if (TREE_CODE (fns) == TREE_LIST)
    for (; fns; fns = TREE_CHAIN (fns))
      print_candidates_1 (TREE_VALUE (fns), str, more || TREE_CHAIN (fns));
  else
    for (lkp_iterator iter (fns); iter;)
      {
	tree cand = *iter;
	++iter;

	const char *pfx = *str;
	if (!pfx)
	  {
	    if (more || iter)
	      pfx = _("candidates are:");
	    else
	      pfx = _("candidate is:");
	    *str = get_spaces (pfx);
	  }
	inform (DECL_SOURCE_LOCATION (cand), "%s %#qD", pfx, cand);
      }
}

/* Print the list of candidate FNS in an error message.  FNS can also
   be a TREE_LIST of non-functions in the case of an ambiguous lookup.  */

void
print_candidates (tree fns)
{
  char *str = NULL;
  print_candidates_1 (fns, &str);
  free (str);
}

/* Get a (possibly) constrained template declaration for the
   purpose of ordering candidates.  */
static tree
get_template_for_ordering (tree list)
{
  gcc_assert (TREE_CODE (list) == TREE_LIST);
  tree f = TREE_VALUE (list);
  if (tree ti = DECL_TEMPLATE_INFO (f))
    return TI_TEMPLATE (ti);
  return f;
}

/* Among candidates having the same signature, return the
   most constrained or NULL_TREE if there is no best candidate.
   If the signatures of candidates vary (e.g., template
   specialization vs. member function), then there can be no
   most constrained.

   Note that we don't compare constraints on the functions
   themselves, but rather those of their templates. */
static tree
most_constrained_function (tree candidates)
{
  // Try to find the best candidate in a first pass.
  tree champ = candidates;
  for (tree c = TREE_CHAIN (champ); c; c = TREE_CHAIN (c))
    {
      int winner = more_constrained (get_template_for_ordering (champ),
                                     get_template_for_ordering (c));
      if (winner == -1)
        champ = c; // The candidate is more constrained
      else if (winner == 0)
        return NULL_TREE; // Neither is more constrained
    }

  // Verify that the champ is better than previous candidates.
  for (tree c = candidates; c != champ; c = TREE_CHAIN (c)) {
    if (!more_constrained (get_template_for_ordering (champ),
                           get_template_for_ordering (c)))
      return NULL_TREE;
  }

  return champ;
}


/* Returns the template (one of the functions given by TEMPLATE_ID)
   which can be specialized to match the indicated DECL with the
   explicit template args given in TEMPLATE_ID.  The DECL may be
   NULL_TREE if none is available.  In that case, the functions in
   TEMPLATE_ID are non-members.

   If NEED_MEMBER_TEMPLATE is nonzero the function is known to be a
   specialization of a member template.

   The TEMPLATE_COUNT is the number of references to qualifying
   template classes that appeared in the name of the function. See
   check_explicit_specialization for a more accurate description.

   TSK indicates what kind of template declaration (if any) is being
   declared.  TSK_TEMPLATE indicates that the declaration given by
   DECL, though a FUNCTION_DECL, has template parameters, and is
   therefore a template function.

   The template args (those explicitly specified and those deduced)
   are output in a newly created vector *TARGS_OUT.

   If it is impossible to determine the result, an error message is
   issued.  The error_mark_node is returned to indicate failure.  */

static tree
determine_specialization (tree template_id,
			  tree decl,
			  tree* targs_out,
			  int need_member_template,
			  int template_count,
			  tmpl_spec_kind tsk)
{
  tree fns;
  tree targs;
  tree explicit_targs;
  tree candidates = NULL_TREE;

  /* A TREE_LIST of templates of which DECL may be a specialization.
     The TREE_VALUE of each node is a TEMPLATE_DECL.  The
     corresponding TREE_PURPOSE is the set of template arguments that,
     when used to instantiate the template, would produce a function
     with the signature of DECL.  */
  tree templates = NULL_TREE;
  int header_count;
  cp_binding_level *b;

  *targs_out = NULL_TREE;

  if (template_id == error_mark_node || decl == error_mark_node)
    return error_mark_node;

  /* We shouldn't be specializing a member template of an
     unspecialized class template; we already gave an error in
     check_specialization_scope, now avoid crashing.  */
  if (!VAR_P (decl)
      && template_count && DECL_CLASS_SCOPE_P (decl)
      && template_class_depth (DECL_CONTEXT (decl)) > 0)
    {
      gcc_assert (errorcount);
      return error_mark_node;
    }

  fns = TREE_OPERAND (template_id, 0);
  explicit_targs = TREE_OPERAND (template_id, 1);

  if (fns == error_mark_node)
    return error_mark_node;

  /* Check for baselinks.  */
  if (BASELINK_P (fns))
    fns = BASELINK_FUNCTIONS (fns);

  if (TREE_CODE (decl) == FUNCTION_DECL && !is_overloaded_fn (fns))
    {
      error_at (DECL_SOURCE_LOCATION (decl),
		"%qD is not a function template", fns);
      return error_mark_node;
    }
  else if (VAR_P (decl) && !variable_template_p (fns))
    {
      error ("%qD is not a variable template", fns);
      return error_mark_node;
    }

  /* Count the number of template headers specified for this
     specialization.  */
  header_count = 0;
  for (b = current_binding_level;
       b->kind == sk_template_parms;
       b = b->level_chain)
    ++header_count;

  tree orig_fns = fns;
  bool header_mismatch = false;

  if (variable_template_p (fns))
    {
      tree parms = INNERMOST_TEMPLATE_PARMS (DECL_TEMPLATE_PARMS (fns));
      targs = coerce_template_parms (parms, explicit_targs, fns,
				     tf_warning_or_error);
      if (targs != error_mark_node
	  && constraints_satisfied_p (fns, targs))
        templates = tree_cons (targs, fns, templates);
    }
  else for (lkp_iterator iter (fns); iter; ++iter)
    {
      tree fn = *iter;

      if (TREE_CODE (fn) == TEMPLATE_DECL)
	{
	  tree decl_arg_types;
	  tree fn_arg_types;

	  /* In case of explicit specialization, we need to check if
	     the number of template headers appearing in the specialization
	     is correct. This is usually done in check_explicit_specialization,
	     but the check done there cannot be exhaustive when specializing
	     member functions. Consider the following code:

	     template <> void A<int>::f(int);
	     template <> template <> void A<int>::f(int);

	     Assuming that A<int> is not itself an explicit specialization
	     already, the first line specializes "f" which is a non-template
	     member function, whilst the second line specializes "f" which
	     is a template member function. So both lines are syntactically
	     correct, and check_explicit_specialization does not reject
	     them.

	     Here, we can do better, as we are matching the specialization
	     against the declarations. We count the number of template
	     headers, and we check if they match TEMPLATE_COUNT + 1
	     (TEMPLATE_COUNT is the number of qualifying template classes,
	     plus there must be another header for the member template
	     itself).

	     Notice that if header_count is zero, this is not a
	     specialization but rather a template instantiation, so there
	     is no check we can perform here.  */
	  if (header_count && header_count != template_count + 1)
	    {
	      header_mismatch = true;
	      continue;
	    }

	  /* Check that the number of template arguments at the
	     innermost level for DECL is the same as for FN.  */
	  if (current_binding_level->kind == sk_template_parms
	      && !current_binding_level->explicit_spec_p
	      && (TREE_VEC_LENGTH (DECL_INNERMOST_TEMPLATE_PARMS (fn))
		  != TREE_VEC_LENGTH (INNERMOST_TEMPLATE_PARMS
				      (current_template_parms))))
	    continue;

	  /* DECL might be a specialization of FN.  */
	  decl_arg_types = TYPE_ARG_TYPES (TREE_TYPE (decl));
	  fn_arg_types = TYPE_ARG_TYPES (TREE_TYPE (fn));

	  /* For a non-static member function, we need to make sure
	     that the const qualification is the same.  Since
	     get_bindings does not try to merge the "this" parameter,
	     we must do the comparison explicitly.  */
	  if (DECL_NONSTATIC_MEMBER_FUNCTION_P (fn))
	    {
	      if (!same_type_p (TREE_VALUE (fn_arg_types),
				TREE_VALUE (decl_arg_types)))
		continue;

	      /* And the ref-qualification.  */
	      if (type_memfn_rqual (TREE_TYPE (decl))
		  != type_memfn_rqual (TREE_TYPE (fn)))
		continue;
	    }

	  /* Skip the "this" parameter and, for constructors of
	     classes with virtual bases, the VTT parameter.  A
	     full specialization of a constructor will have a VTT
	     parameter, but a template never will.  */
	  decl_arg_types
	    = skip_artificial_parms_for (decl, decl_arg_types);
	  fn_arg_types
	    = skip_artificial_parms_for (fn, fn_arg_types);

	  /* Function templates cannot be specializations; there are
	     no partial specializations of functions.  Therefore, if
	     the type of DECL does not match FN, there is no
	     match.

             Note that it should never be the case that we have both
             candidates added here, and for regular member functions
             below. */
	  if (tsk == tsk_template)
	    {
	      if (!comp_template_parms (DECL_TEMPLATE_PARMS (fn),
					current_template_parms))
		continue;
	      if (!same_type_p (TREE_TYPE (TREE_TYPE (decl)),
				TREE_TYPE (TREE_TYPE (fn))))
		continue;
	      if (!compparms (fn_arg_types, decl_arg_types))
		continue;

	      tree freq = get_constraints (fn);
	      tree dreq = get_constraints (decl);
	      if (!freq != !dreq)
		continue;
	      if (freq)
		{
		  /* C++20 CA104: Substitute directly into the
		     constraint-expression.  */
		  tree fargs = DECL_TI_ARGS (fn);
		  tsubst_flags_t complain = tf_none;
		  freq = tsubst_constraint_info (freq, fargs, complain, fn);
		  if (!cp_tree_equal (freq, dreq))
		    continue;
		}

	      candidates = tree_cons (NULL_TREE, fn, candidates);
	      continue;
	    }

	  /* See whether this function might be a specialization of this
	     template.  Suppress access control because we might be trying
	     to make this specialization a friend, and we have already done
	     access control for the declaration of the specialization.  */
	  push_deferring_access_checks (dk_no_check);
	  targs = get_bindings (fn, decl, explicit_targs, /*check_ret=*/true);
	  pop_deferring_access_checks ();

	  if (!targs)
	    /* We cannot deduce template arguments that when used to
	       specialize TMPL will produce DECL.  */
	    continue;

	  if (uses_template_parms (targs))
	    /* We deduced something involving 'auto', which isn't a valid
	       template argument.  */
	    continue;

	  /* Save this template, and the arguments deduced.  */
	  templates = tree_cons (targs, fn, templates);
	}
      else if (need_member_template)
	/* FN is an ordinary member function, and we need a
	   specialization of a member template.  */
	;
      else if (TREE_CODE (fn) != FUNCTION_DECL)
	/* We can get IDENTIFIER_NODEs here in certain erroneous
	   cases.  */
	;
      else if (!DECL_FUNCTION_MEMBER_P (fn))
	/* This is just an ordinary non-member function.  Nothing can
	   be a specialization of that.  */
	;
      else if (DECL_ARTIFICIAL (fn))
	/* Cannot specialize functions that are created implicitly.  */
	;
      else
	{
	  tree decl_arg_types;

	  /* This is an ordinary member function.  However, since
	     we're here, we can assume its enclosing class is a
	     template class.  For example,

	       template <typename T> struct S { void f(); };
	       template <> void S<int>::f() {}

	     Here, S<int>::f is a non-template, but S<int> is a
	     template class.  If FN has the same type as DECL, we
	     might be in business.  */

	  if (!DECL_TEMPLATE_INFO (fn))
	    /* Its enclosing class is an explicit specialization
	       of a template class.  This is not a candidate.  */
	    continue;

	  if (!same_type_p (TREE_TYPE (TREE_TYPE (decl)),
			    TREE_TYPE (TREE_TYPE (fn))))
	    /* The return types differ.  */
	    continue;

	  /* Adjust the type of DECL in case FN is a static member.  */
	  decl_arg_types = TYPE_ARG_TYPES (TREE_TYPE (decl));
	  if (DECL_STATIC_FUNCTION_P (fn)
	      && DECL_NONSTATIC_MEMBER_FUNCTION_P (decl))
	    decl_arg_types = TREE_CHAIN (decl_arg_types);

	  if (!compparms (TYPE_ARG_TYPES (TREE_TYPE (fn)),
			 decl_arg_types))
            continue;

	  if (DECL_NONSTATIC_MEMBER_FUNCTION_P (fn)
	      && (type_memfn_rqual (TREE_TYPE (decl))
		  != type_memfn_rqual (TREE_TYPE (fn))))
	    continue;

          // If the deduced arguments do not satisfy the constraints,
          // this is not a candidate.
          if (flag_concepts && !constraints_satisfied_p (fn))
            continue;

          // Add the candidate.
          candidates = tree_cons (NULL_TREE, fn, candidates);
	}
    }

  if (templates && TREE_CHAIN (templates))
    {
      /* We have:

	   [temp.expl.spec]

	   It is possible for a specialization with a given function
	   signature to be instantiated from more than one function
	   template.  In such cases, explicit specification of the
	   template arguments must be used to uniquely identify the
	   function template specialization being specialized.

	 Note that here, there's no suggestion that we're supposed to
	 determine which of the candidate templates is most
	 specialized.  However, we, also have:

	   [temp.func.order]

	   Partial ordering of overloaded function template
	   declarations is used in the following contexts to select
	   the function template to which a function template
	   specialization refers:

	   -- when an explicit specialization refers to a function
	      template.

	 So, we do use the partial ordering rules, at least for now.
	 This extension can only serve to make invalid programs valid,
	 so it's safe.  And, there is strong anecdotal evidence that
	 the committee intended the partial ordering rules to apply;
	 the EDG front end has that behavior, and John Spicer claims
	 that the committee simply forgot to delete the wording in
	 [temp.expl.spec].  */
      tree tmpl = most_specialized_instantiation (templates);
      if (tmpl != error_mark_node)
	{
	  templates = tmpl;
	  TREE_CHAIN (templates) = NULL_TREE;
	}
    }

  // Concepts allows multiple declarations of member functions
  // with the same signature. Like above, we need to rely on
  // on the partial ordering of those candidates to determine which
  // is the best.
  if (flag_concepts && candidates && TREE_CHAIN (candidates))
    {
      if (tree cand = most_constrained_function (candidates))
        {
          candidates = cand;
          TREE_CHAIN (cand) = NULL_TREE;
        }
    }

  if (templates == NULL_TREE && candidates == NULL_TREE)
    {
      error ("template-id %qD for %q+D does not match any template "
	     "declaration", template_id, decl);
      if (header_mismatch)
	inform (DECL_SOURCE_LOCATION (decl),
		"saw %d %<template<>%>, need %d for "
		"specializing a member function template",
		header_count, template_count + 1);
      print_candidates (orig_fns);
      return error_mark_node;
    }
  else if ((templates && TREE_CHAIN (templates))
	   || (candidates && TREE_CHAIN (candidates))
	   || (templates && candidates))
    {
      error ("ambiguous template specialization %qD for %q+D",
	     template_id, decl);
      candidates = chainon (candidates, templates);
      print_candidates (candidates);
      return error_mark_node;
    }

  /* We have one, and exactly one, match.  */
  if (candidates)
    {
      tree fn = TREE_VALUE (candidates);
      *targs_out = copy_node (DECL_TI_ARGS (fn));

      /* Propagate the candidate's constraints to the declaration.  */
      if (tsk != tsk_template)
	set_constraints (decl, get_constraints (fn));

      /* DECL is a re-declaration or partial instantiation of a template
	 function.  */
      if (TREE_CODE (fn) == TEMPLATE_DECL)
	return fn;
      /* It was a specialization of an ordinary member function in a
	 template class.  */
      return DECL_TI_TEMPLATE (fn);
    }

  /* It was a specialization of a template.  */
  tree tmpl = TREE_VALUE (templates);
  *targs_out = add_outermost_template_args (tmpl, TREE_PURPOSE (templates));

  /* Propagate the template's constraints to the declaration.  */
  if (tsk != tsk_template)
    set_constraints (decl, get_constraints (tmpl));

  return tmpl;
}

/* Returns a chain of parameter types, exactly like the SPEC_TYPES,
   but with the default argument values filled in from those in the
   TMPL_TYPES.  */

static tree
copy_default_args_to_explicit_spec_1 (tree spec_types,
				      tree tmpl_types)
{
  tree new_spec_types;

  if (!spec_types)
    return NULL_TREE;

  if (spec_types == void_list_node)
    return void_list_node;

  /* Substitute into the rest of the list.  */
  new_spec_types =
    copy_default_args_to_explicit_spec_1 (TREE_CHAIN (spec_types),
					  TREE_CHAIN (tmpl_types));

  /* Add the default argument for this parameter.  */
  return hash_tree_cons (TREE_PURPOSE (tmpl_types),
			 TREE_VALUE (spec_types),
			 new_spec_types);
}

/* DECL is an explicit specialization.  Replicate default arguments
   from the template it specializes.  (That way, code like:

     template <class T> void f(T = 3);
     template <> void f(double);
     void g () { f (); }

   works, as required.)  An alternative approach would be to look up
   the correct default arguments at the call-site, but this approach
   is consistent with how implicit instantiations are handled.  */

static void
copy_default_args_to_explicit_spec (tree decl)
{
  tree tmpl;
  tree spec_types;
  tree tmpl_types;
  tree new_spec_types;
  tree old_type;
  tree new_type;
  tree t;
  tree object_type = NULL_TREE;
  tree in_charge = NULL_TREE;
  tree vtt = NULL_TREE;

  /* See if there's anything we need to do.  */
  tmpl = DECL_TI_TEMPLATE (decl);
  tmpl_types = TYPE_ARG_TYPES (TREE_TYPE (DECL_TEMPLATE_RESULT (tmpl)));
  for (t = tmpl_types; t; t = TREE_CHAIN (t))
    if (TREE_PURPOSE (t))
      break;
  if (!t)
    return;

  old_type = TREE_TYPE (decl);
  spec_types = TYPE_ARG_TYPES (old_type);

  if (DECL_NONSTATIC_MEMBER_FUNCTION_P (decl))
    {
      /* Remove the this pointer, but remember the object's type for
	 CV quals.  */
      object_type = TREE_TYPE (TREE_VALUE (spec_types));
      spec_types = TREE_CHAIN (spec_types);
      tmpl_types = TREE_CHAIN (tmpl_types);

      if (DECL_HAS_IN_CHARGE_PARM_P (decl))
	{
	  /* DECL may contain more parameters than TMPL due to the extra
	     in-charge parameter in constructors and destructors.  */
	  in_charge = spec_types;
	  spec_types = TREE_CHAIN (spec_types);
	}
      if (DECL_HAS_VTT_PARM_P (decl))
	{
	  vtt = spec_types;
	  spec_types = TREE_CHAIN (spec_types);
	}
    }

  /* Compute the merged default arguments.  */
  new_spec_types =
    copy_default_args_to_explicit_spec_1 (spec_types, tmpl_types);

  /* Compute the new FUNCTION_TYPE.  */
  if (object_type)
    {
      if (vtt)
	new_spec_types = hash_tree_cons (TREE_PURPOSE (vtt),
					 TREE_VALUE (vtt),
					 new_spec_types);

      if (in_charge)
	/* Put the in-charge parameter back.  */
	new_spec_types = hash_tree_cons (TREE_PURPOSE (in_charge),
					 TREE_VALUE (in_charge),
					 new_spec_types);

      new_type = build_method_type_directly (object_type,
					     TREE_TYPE (old_type),
					     new_spec_types);
    }
  else
    new_type = build_function_type (TREE_TYPE (old_type),
				    new_spec_types);
  new_type = cp_build_type_attribute_variant (new_type,
					      TYPE_ATTRIBUTES (old_type));
  new_type = cxx_copy_lang_qualifiers (new_type, old_type);

  TREE_TYPE (decl) = new_type;
}

/* Return the number of template headers we expect to see for a definition
   or specialization of CTYPE or one of its non-template members.  */

int
num_template_headers_for_class (tree ctype)
{
  int num_templates = 0;

  while (ctype && CLASS_TYPE_P (ctype))
    {
      /* You're supposed to have one `template <...>' for every
	 template class, but you don't need one for a full
	 specialization.  For example:

	 template <class T> struct S{};
	 template <> struct S<int> { void f(); };
	 void S<int>::f () {}

	 is correct; there shouldn't be a `template <>' for the
	 definition of `S<int>::f'.  */
      if (!CLASSTYPE_TEMPLATE_INFO (ctype))
	/* If CTYPE does not have template information of any
	   kind,  then it is not a template, nor is it nested
	   within a template.  */
	break;
      if (explicit_class_specialization_p (ctype))
	break;
      if (PRIMARY_TEMPLATE_P (CLASSTYPE_TI_TEMPLATE (ctype)))
	++num_templates;

      ctype = TYPE_CONTEXT (ctype);
    }

  return num_templates;
}

/* Do a simple sanity check on the template headers that precede the
   variable declaration DECL.  */

void
check_template_variable (tree decl)
{
  tree ctx = CP_DECL_CONTEXT (decl);
  int wanted = num_template_headers_for_class (ctx);
  if (DECL_LANG_SPECIFIC (decl) && DECL_TEMPLATE_INFO (decl)
      && PRIMARY_TEMPLATE_P (DECL_TI_TEMPLATE (decl)))
    {
      if (cxx_dialect < cxx14)
        pedwarn (DECL_SOURCE_LOCATION (decl), OPT_Wc__14_extensions,
		 "variable templates only available with "
		 "%<-std=c++14%> or %<-std=gnu++14%>");

      // Namespace-scope variable templates should have a template header.
      ++wanted;
    }
  if (template_header_count > wanted)
    {
      auto_diagnostic_group d;
      bool warned = pedwarn (DECL_SOURCE_LOCATION (decl), 0,
			     "too many template headers for %qD "
	                     "(should be %d)",
			     decl, wanted);
      if (warned && CLASS_TYPE_P (ctx)
	  && CLASSTYPE_TEMPLATE_SPECIALIZATION (ctx))
	inform (DECL_SOURCE_LOCATION (decl),
		"members of an explicitly specialized class are defined "
		"without a template header");
    }
}

/* An explicit specialization whose declarator-id or class-head-name is not
   qualified shall be declared in the nearest enclosing namespace of the
   template, or, if the namespace is inline (7.3.1), any namespace from its
   enclosing namespace set.

   If the name declared in the explicit instantiation is an unqualified name,
   the explicit instantiation shall appear in the namespace where its template
   is declared or, if that namespace is inline (7.3.1), any namespace from its
   enclosing namespace set.  */

void
check_unqualified_spec_or_inst (tree t, location_t loc)
{
  tree tmpl = most_general_template (t);
  if (DECL_NAMESPACE_SCOPE_P (tmpl)
      && !is_nested_namespace (current_namespace,
			       CP_DECL_CONTEXT (tmpl), true))
    {
      if (processing_specialization)
	permerror (loc, "explicit specialization of %qD outside its "
		   "namespace must use a nested-name-specifier", tmpl);
      else if (processing_explicit_instantiation
	       && cxx_dialect >= cxx11)
	/* This was allowed in C++98, so only pedwarn.  */
	pedwarn (loc, OPT_Wpedantic, "explicit instantiation of %qD "
		 "outside its namespace must use a nested-name-"
		 "specifier", tmpl);
    }
}

/* Warn for a template specialization SPEC that is missing some of a set
   of function or type attributes that the template TEMPL is declared with.
   ATTRLIST is a list of additional attributes that SPEC should be taken
   to ultimately be declared with.  */

static void
warn_spec_missing_attributes (tree tmpl, tree spec, tree attrlist)
{
  if (DECL_FUNCTION_TEMPLATE_P (tmpl))
    tmpl = DECL_TEMPLATE_RESULT (tmpl);

  /* Avoid warning if the difference between the primary and
     the specialization is not in one of the attributes below.  */
  const char* const blacklist[] = {
    "alloc_align", "alloc_size", "assume_aligned", "format",
    "format_arg", "malloc", "nonnull", NULL
  };

  /* Put together a list of the black listed attributes that the primary
     template is declared with that the specialization is not, in case
     it's not apparent from the most recent declaration of the primary.  */
  pretty_printer str;
  unsigned nattrs = decls_mismatched_attributes (tmpl, spec, attrlist,
						 blacklist, &str);

  if (!nattrs)
    return;

  auto_diagnostic_group d;
  if (warning_at (DECL_SOURCE_LOCATION (spec), OPT_Wmissing_attributes,
		  "explicit specialization %q#D may be missing attributes",
		  spec))
    inform (DECL_SOURCE_LOCATION (tmpl),
	    nattrs > 1
	    ? G_("missing primary template attributes %s")
	    : G_("missing primary template attribute %s"),
	    pp_formatted_text (&str));
}

/* Check to see if the function just declared, as indicated in
   DECLARATOR, and in DECL, is a specialization of a function
   template.  We may also discover that the declaration is an explicit
   instantiation at this point.

   Returns DECL, or an equivalent declaration that should be used
   instead if all goes well.  Issues an error message if something is
   amiss.  Returns error_mark_node if the error is not easily
   recoverable.

   FLAGS is a bitmask consisting of the following flags:

   2: The function has a definition.
   4: The function is a friend.

   The TEMPLATE_COUNT is the number of references to qualifying
   template classes that appeared in the name of the function.  For
   example, in

     template <class T> struct S { void f(); };
     void S<int>::f();

   the TEMPLATE_COUNT would be 1.  However, explicitly specialized
   classes are not counted in the TEMPLATE_COUNT, so that in

     template <class T> struct S {};
     template <> struct S<int> { void f(); }
     template <> void S<int>::f();

   the TEMPLATE_COUNT would be 0.  (Note that this declaration is
   invalid; there should be no template <>.)

   If the function is a specialization, it is marked as such via
   DECL_TEMPLATE_SPECIALIZATION.  Furthermore, its DECL_TEMPLATE_INFO
   is set up correctly, and it is added to the list of specializations
   for that template.  */

tree
check_explicit_specialization (tree declarator,
			       tree decl,
			       int template_count,
			       int flags,
			       tree attrlist)
{
  int have_def = flags & 2;
  int is_friend = flags & 4;
  bool is_concept = flags & 8;
  int specialization = 0;
  int explicit_instantiation = 0;
  int member_specialization = 0;
  tree ctype = DECL_CLASS_CONTEXT (decl);
  tree dname = DECL_NAME (decl);
  tmpl_spec_kind tsk;

  if (is_friend)
    {
      if (!processing_specialization)
	tsk = tsk_none;
      else
	tsk = tsk_excessive_parms;
    }
  else
    tsk = current_tmpl_spec_kind (template_count);

  switch (tsk)
    {
    case tsk_none:
      if (processing_specialization && !VAR_P (decl))
	{
	  specialization = 1;
	  SET_DECL_TEMPLATE_SPECIALIZATION (decl);
	}
      else if (TREE_CODE (declarator) == TEMPLATE_ID_EXPR
	       || (DECL_LANG_SPECIFIC (decl)
		   && DECL_IMPLICIT_INSTANTIATION (decl)))
	{
	  if (is_friend)
	    /* This could be something like:

	       template <class T> void f(T);
	       class S { friend void f<>(int); }  */
	    specialization = 1;
	  else
	    {
	      /* This case handles bogus declarations like template <>
		 template <class T> void f<int>(); */

	      error_at (cp_expr_loc_or_input_loc (declarator),
			"template-id %qE in declaration of primary template",
			declarator);
	      return decl;
	    }
	}
      break;

    case tsk_invalid_member_spec:
      /* The error has already been reported in
	 check_specialization_scope.  */
      return error_mark_node;

    case tsk_invalid_expl_inst:
      error ("template parameter list used in explicit instantiation");

      /* Fall through.  */

    case tsk_expl_inst:
      if (have_def)
	error ("definition provided for explicit instantiation");

      explicit_instantiation = 1;
      break;

    case tsk_excessive_parms:
    case tsk_insufficient_parms:
      if (tsk == tsk_excessive_parms)
	error ("too many template parameter lists in declaration of %qD",
	       decl);
      else if (template_header_count)
	error("too few template parameter lists in declaration of %qD", decl);
      else
	error("explicit specialization of %qD must be introduced by "
	      "%<template <>%>", decl);

      /* Fall through.  */
    case tsk_expl_spec:
      if (is_concept)
        error ("explicit specialization declared %<concept%>");

      if (VAR_P (decl) && TREE_CODE (declarator) != TEMPLATE_ID_EXPR)
	/* In cases like template<> constexpr bool v = true;
	   We'll give an error in check_template_variable.  */
	break;

      SET_DECL_TEMPLATE_SPECIALIZATION (decl);
      if (ctype)
	member_specialization = 1;
      else
	specialization = 1;
      break;

    case tsk_template:
      if (TREE_CODE (declarator) == TEMPLATE_ID_EXPR)
	{
	  /* This case handles bogus declarations like template <>
	     template <class T> void f<int>(); */

	  if (!uses_template_parms (TREE_OPERAND (declarator, 1)))
	    error_at (cp_expr_loc_or_input_loc (declarator),
		      "template-id %qE in declaration of primary template",
		      declarator);
	  else if (variable_template_p (TREE_OPERAND (declarator, 0)))
	    {
	      /* Partial specialization of variable template.  */
	      SET_DECL_TEMPLATE_SPECIALIZATION (decl);
	      specialization = 1;
	      goto ok;
	    }
	  else if (cxx_dialect < cxx14)
	    error_at (cp_expr_loc_or_input_loc (declarator),
		      "non-type partial specialization %qE "
		      "is not allowed", declarator);
	  else
	    error_at (cp_expr_loc_or_input_loc (declarator),
		      "non-class, non-variable partial specialization %qE "
		      "is not allowed", declarator);
	  return decl;
	ok:;
	}

      if (ctype && CLASSTYPE_TEMPLATE_INSTANTIATION (ctype))
	/* This is a specialization of a member template, without
	   specialization the containing class.  Something like:

	     template <class T> struct S {
	       template <class U> void f (U);
	     };
	     template <> template <class U> void S<int>::f(U) {}

	   That's a specialization -- but of the entire template.  */
	specialization = 1;
      break;

    default:
      gcc_unreachable ();
    }

  if ((specialization || member_specialization)
      /* This doesn't apply to variable templates.  */
      && FUNC_OR_METHOD_TYPE_P (TREE_TYPE (decl)))
    {
      tree t = TYPE_ARG_TYPES (TREE_TYPE (decl));
      for (; t; t = TREE_CHAIN (t))
	if (TREE_PURPOSE (t))
	  {
	    permerror (input_location,
		       "default argument specified in explicit specialization");
	    break;
	  }
    }

  if (specialization || member_specialization || explicit_instantiation)
    {
      tree tmpl = NULL_TREE;
      tree targs = NULL_TREE;
      bool was_template_id = (TREE_CODE (declarator) == TEMPLATE_ID_EXPR);
      bool found_hidden = false;

      /* Make sure that the declarator is a TEMPLATE_ID_EXPR.  */
      if (!was_template_id)
	{
	  tree fns;

	  gcc_assert (identifier_p (declarator));
	  if (ctype)
	    fns = dname;
	  else
	    {
	      /* If there is no class context, the explicit instantiation
		 must be at namespace scope.  */
	      gcc_assert (DECL_NAMESPACE_SCOPE_P (decl));

	      /* Find the namespace binding, using the declaration
		 context.  */
	      fns = lookup_qualified_name (CP_DECL_CONTEXT (decl), dname,
					   LOOK_want::NORMAL, true);
	      if (fns == error_mark_node)
		{
		  /* If lookup fails, look for a friend declaration so we can
		     give a better diagnostic.  */
		  fns = (lookup_qualified_name
			 (CP_DECL_CONTEXT (decl), dname,
			  LOOK_want::NORMAL | LOOK_want::HIDDEN_FRIEND,
			  /*complain*/true));
		  found_hidden = true;
		}

	      if (fns == error_mark_node || !is_overloaded_fn (fns))
		{
		  error ("%qD is not a template function", dname);
		  fns = error_mark_node;
		}
	    }

	  declarator = lookup_template_function (fns, NULL_TREE);
	}

      if (declarator == error_mark_node)
	return error_mark_node;

      if (ctype != NULL_TREE && TYPE_BEING_DEFINED (ctype))
	{
	  if (!explicit_instantiation)
	    /* A specialization in class scope.  This is invalid,
	       but the error will already have been flagged by
	       check_specialization_scope.  */
	    return error_mark_node;
	  else
	    {
	      /* It's not valid to write an explicit instantiation in
		 class scope, e.g.:

		   class C { template void f(); }

		   This case is caught by the parser.  However, on
		   something like:

		   template class C { void f(); };

		   (which is invalid) we can get here.  The error will be
		   issued later.  */
	      ;
	    }

	  return decl;
	}
      else if (ctype != NULL_TREE
	       && (identifier_p (TREE_OPERAND (declarator, 0))))
	{
	  // We'll match variable templates in start_decl.
	  if (VAR_P (decl))
	    return decl;

	  /* Find the list of functions in ctype that have the same
	     name as the declared function.  */
	  tree name = TREE_OPERAND (declarator, 0);

	  if (constructor_name_p (name, ctype))
	    {
	      if (DECL_CONSTRUCTOR_P (decl)
		  ? !TYPE_HAS_USER_CONSTRUCTOR (ctype)
		  : !CLASSTYPE_DESTRUCTOR (ctype))
		{
		  /* From [temp.expl.spec]:

		     If such an explicit specialization for the member
		     of a class template names an implicitly-declared
		     special member function (clause _special_), the
		     program is ill-formed.

		     Similar language is found in [temp.explicit].  */
		  error ("specialization of implicitly-declared special member function");
		  return error_mark_node;
		}

	      name = DECL_NAME (decl);
	    }

	  /* For a type-conversion operator, We might be looking for
	     `operator int' which will be a specialization of
	     `operator T'.  Grab all the conversion operators, and
	     then select from them.  */
	  tree fns = get_class_binding (ctype, IDENTIFIER_CONV_OP_P (name)
					? conv_op_identifier : name);

	  if (fns == NULL_TREE)
	    {
	      error ("no member function %qD declared in %qT", name, ctype);
	      return error_mark_node;
	    }
	  else
	    TREE_OPERAND (declarator, 0) = fns;
	}

      /* Figure out what exactly is being specialized at this point.
	 Note that for an explicit instantiation, even one for a
	 member function, we cannot tell a priori whether the
	 instantiation is for a member template, or just a member
	 function of a template class.  Even if a member template is
	 being instantiated, the member template arguments may be
	 elided if they can be deduced from the rest of the
	 declaration.  */
      tmpl = determine_specialization (declarator, decl,
				       &targs,
				       member_specialization,
				       template_count,
				       tsk);

      if (!tmpl || tmpl == error_mark_node)
	/* We couldn't figure out what this declaration was
	   specializing.  */
	return error_mark_node;
      else
	{
	  if (found_hidden && TREE_CODE (decl) == FUNCTION_DECL)
	    {
	      auto_diagnostic_group d;
	      if (pedwarn (DECL_SOURCE_LOCATION (decl), 0,
			   "friend declaration %qD is not visible to "
			   "explicit specialization", tmpl))
		inform (DECL_SOURCE_LOCATION (tmpl),
			"friend declaration here");
	    }

	  if (!ctype && !is_friend
	      && CP_DECL_CONTEXT (decl) == current_namespace)
	    check_unqualified_spec_or_inst (tmpl, DECL_SOURCE_LOCATION (decl));

	  tree gen_tmpl = most_general_template (tmpl);

	  if (explicit_instantiation)
	    {
	      /* We don't set DECL_EXPLICIT_INSTANTIATION here; that
		 is done by do_decl_instantiation later.  */

	      int arg_depth = TMPL_ARGS_DEPTH (targs);
	      int parm_depth = TMPL_PARMS_DEPTH (DECL_TEMPLATE_PARMS (tmpl));

	      if (arg_depth > parm_depth)
		{
		  /* If TMPL is not the most general template (for
		     example, if TMPL is a friend template that is
		     injected into namespace scope), then there will
		     be too many levels of TARGS.  Remove some of them
		     here.  */
		  int i;
		  tree new_targs;

		  new_targs = make_tree_vec (parm_depth);
		  for (i = arg_depth - parm_depth; i < arg_depth; ++i)
		    TREE_VEC_ELT (new_targs, i - (arg_depth - parm_depth))
		      = TREE_VEC_ELT (targs, i);
		  targs = new_targs;
		}

	      return instantiate_template (tmpl, targs, tf_error);
	    }

	  /* If we thought that the DECL was a member function, but it
	     turns out to be specializing a static member function,
	     make DECL a static member function as well.  */
	  if (DECL_FUNCTION_TEMPLATE_P (tmpl)
	      && DECL_STATIC_FUNCTION_P (tmpl)
	      && DECL_NONSTATIC_MEMBER_FUNCTION_P (decl))
	    revert_static_member_fn (decl);

	  /* If this is a specialization of a member template of a
	     template class, we want to return the TEMPLATE_DECL, not
	     the specialization of it.  */
	  if (tsk == tsk_template && !was_template_id)
	    {
	      tree result = DECL_TEMPLATE_RESULT (tmpl);
	      SET_DECL_TEMPLATE_SPECIALIZATION (tmpl);
	      DECL_INITIAL (result) = NULL_TREE;
	      if (have_def)
		{
		  tree parm;
		  DECL_SOURCE_LOCATION (tmpl) = DECL_SOURCE_LOCATION (decl);
		  DECL_SOURCE_LOCATION (result)
		    = DECL_SOURCE_LOCATION (decl);
		  /* We want to use the argument list specified in the
		     definition, not in the original declaration.  */
		  DECL_ARGUMENTS (result) = DECL_ARGUMENTS (decl);
		  for (parm = DECL_ARGUMENTS (result); parm;
		       parm = DECL_CHAIN (parm))
		    DECL_CONTEXT (parm) = result;
		}
	      decl = register_specialization (tmpl, gen_tmpl, targs,
					      is_friend, 0);
	      remove_contract_attributes (result);
	      return decl;
	    }

	  /* Set up the DECL_TEMPLATE_INFO for DECL.  */
	  DECL_TEMPLATE_INFO (decl) = build_template_info (tmpl, targs);

	  if (was_template_id)
	    TINFO_USED_TEMPLATE_ID (DECL_TEMPLATE_INFO (decl)) = true;

	  /* Inherit default function arguments from the template
	     DECL is specializing.  */
	  if (DECL_FUNCTION_TEMPLATE_P (tmpl))
	    copy_default_args_to_explicit_spec (decl);

	  /* This specialization has the same protection as the
	     template it specializes.  */
	  TREE_PRIVATE (decl) = TREE_PRIVATE (gen_tmpl);
	  TREE_PROTECTED (decl) = TREE_PROTECTED (gen_tmpl);

          /* 7.1.1-1 [dcl.stc]

             A storage-class-specifier shall not be specified in an
             explicit specialization...

             The parser rejects these, so unless action is taken here,
             explicit function specializations will always appear with
             global linkage.

             The action recommended by the C++ CWG in response to C++
             defect report 605 is to make the storage class and linkage
             of the explicit specialization match the templated function:

             http://www.open-std.org/jtc1/sc22/wg21/docs/cwg_active.html#605
           */
          if (tsk == tsk_expl_spec && DECL_FUNCTION_TEMPLATE_P (gen_tmpl))
            {
              tree tmpl_func = DECL_TEMPLATE_RESULT (gen_tmpl);
              gcc_assert (TREE_CODE (tmpl_func) == FUNCTION_DECL);

              /* A concept cannot be specialized.  */
              if (DECL_DECLARED_CONCEPT_P (tmpl_func))
                {
                  error ("explicit specialization of function concept %qD",
                         gen_tmpl);
                  return error_mark_node;
                }

              /* This specialization has the same linkage and visibility as
                 the function template it specializes.  */
              TREE_PUBLIC (decl) = TREE_PUBLIC (tmpl_func);
	      if (! TREE_PUBLIC (decl))
		{
		  DECL_INTERFACE_KNOWN (decl) = 1;
		  DECL_NOT_REALLY_EXTERN (decl) = 1;
		}
              DECL_THIS_STATIC (decl) = DECL_THIS_STATIC (tmpl_func);
              if (DECL_VISIBILITY_SPECIFIED (tmpl_func))
                {
                  DECL_VISIBILITY_SPECIFIED (decl) = 1;
                  DECL_VISIBILITY (decl) = DECL_VISIBILITY (tmpl_func);
                }
            }

	  /* If DECL is a friend declaration, declared using an
	     unqualified name, the namespace associated with DECL may
	     have been set incorrectly.  For example, in:

	       template <typename T> void f(T);
	       namespace N {
		 struct S { friend void f<int>(int); }
	       }

	     we will have set the DECL_CONTEXT for the friend
	     declaration to N, rather than to the global namespace.  */
	  if (DECL_NAMESPACE_SCOPE_P (decl))
	    DECL_CONTEXT (decl) = DECL_CONTEXT (tmpl);

	  if (is_friend && !have_def)
	    /* This is not really a declaration of a specialization.
	       It's just the name of an instantiation.  But, it's not
	       a request for an instantiation, either.  */
	    SET_DECL_IMPLICIT_INSTANTIATION (decl);
	  else if (TREE_CODE (decl) == FUNCTION_DECL)
	    /* A specialization is not necessarily COMDAT.  */
	    DECL_COMDAT (decl) = (TREE_PUBLIC (decl)
				  && DECL_DECLARED_INLINE_P (decl));
	  else if (VAR_P (decl))
	    DECL_COMDAT (decl) = false;

	  /* If this is a full specialization, register it so that we can find
	     it again.  Partial specializations will be registered in
	     process_partial_specialization.  */
	  if (!processing_template_decl)
	    {
	      warn_spec_missing_attributes (gen_tmpl, decl, attrlist);

	      decl = register_specialization (decl, gen_tmpl, targs,
					      is_friend, 0);
	    }

	  /* If this is a specialization, splice any contracts that may have
	     been inherited from the template, removing them.  */
	  if (decl != error_mark_node && DECL_TEMPLATE_SPECIALIZATION (decl))
	    remove_contract_attributes (decl);

	  /* A 'structor should already have clones.  */
	  gcc_assert (decl == error_mark_node
		      || variable_template_p (tmpl)
		      || !(DECL_CONSTRUCTOR_P (decl)
			   || DECL_DESTRUCTOR_P (decl))
		      || DECL_CLONED_FUNCTION_P (DECL_CHAIN (decl)));
	}
    }

  return decl;
}

/* Returns 1 iff PARMS1 and PARMS2 are identical sets of template
   parameters.  These are represented in the same format used for
   DECL_TEMPLATE_PARMS.  */

int
comp_template_parms (const_tree parms1, const_tree parms2)
{
  const_tree p1;
  const_tree p2;

  if (parms1 == parms2)
    return 1;

  for (p1 = parms1, p2 = parms2;
       p1 != NULL_TREE && p2 != NULL_TREE;
       p1 = TREE_CHAIN (p1), p2 = TREE_CHAIN (p2))
    {
      tree t1 = TREE_VALUE (p1);
      tree t2 = TREE_VALUE (p2);
      int i;

      gcc_assert (TREE_CODE (t1) == TREE_VEC);
      gcc_assert (TREE_CODE (t2) == TREE_VEC);

      if (TREE_VEC_LENGTH (t1) != TREE_VEC_LENGTH (t2))
	return 0;

      for (i = 0; i < TREE_VEC_LENGTH (t2); ++i)
	{
          tree parm1 = TREE_VALUE (TREE_VEC_ELT (t1, i));
          tree parm2 = TREE_VALUE (TREE_VEC_ELT (t2, i));

          /* If either of the template parameters are invalid, assume
             they match for the sake of error recovery. */
          if (error_operand_p (parm1) || error_operand_p (parm2))
            return 1;

	  if (TREE_CODE (parm1) != TREE_CODE (parm2))
	    return 0;

	  if (TREE_CODE (parm1) == TEMPLATE_TYPE_PARM
              && (TEMPLATE_TYPE_PARAMETER_PACK (parm1)
                  == TEMPLATE_TYPE_PARAMETER_PACK (parm2)))
	    continue;
	  else if (!same_type_p (TREE_TYPE (parm1), TREE_TYPE (parm2)))
	    return 0;
	}
    }

  if ((p1 != NULL_TREE) != (p2 != NULL_TREE))
    /* One set of parameters has more parameters lists than the
       other.  */
    return 0;

  return 1;
}

/* Returns true if two template parameters are declared with
   equivalent constraints.  */

static bool
template_parameter_constraints_equivalent_p (const_tree parm1, const_tree parm2)
{
  tree req1 = TREE_TYPE (parm1);
  tree req2 = TREE_TYPE (parm2);
  if (!req1 != !req2)
    return false;
  if (req1)
    return cp_tree_equal (req1, req2);
  return true;
}

/* Returns true when two template parameters are equivalent.  */

static bool
template_parameters_equivalent_p (const_tree parm1, const_tree parm2)
{
  tree decl1 = TREE_VALUE (parm1);
  tree decl2 = TREE_VALUE (parm2);

  /* If either of the template parameters are invalid, assume
     they match for the sake of error recovery. */
  if (error_operand_p (decl1) || error_operand_p (decl2))
    return true;

  /* ... they declare parameters of the same kind.  */
  if (TREE_CODE (decl1) != TREE_CODE (decl2))
    return false;

  /* ... one parameter was introduced by a parameter declaration, then
     both are. This case arises as a result of eagerly rewriting declarations
     during parsing.  */
  if (DECL_VIRTUAL_P (decl1) != DECL_VIRTUAL_P (decl2))
    return false;

  /* ... if either declares a pack, they both do.  */
  if (template_parameter_pack_p (decl1) != template_parameter_pack_p (decl2))
    return false;

  if (TREE_CODE (decl1) == PARM_DECL)
    {
      /* ... if they declare non-type parameters, the types are equivalent.  */
      if (!same_type_p (TREE_TYPE (decl1), TREE_TYPE (decl2)))
	return false;
    }
  else if (TREE_CODE (decl2) == TEMPLATE_DECL)
    {
      /* ... if they declare template template parameters, their template
	 parameter lists are equivalent.  */
      if (!template_heads_equivalent_p (decl1, decl2))
	return false;
    }

  /* ... if they are declared with a qualified-concept name, they both
     are, and those names are equivalent.  */
  return template_parameter_constraints_equivalent_p (parm1, parm2);
}

/* Returns true if two template parameters lists are equivalent.
   Two template parameter lists are equivalent if they have the
   same length and their corresponding parameters are equivalent.

   PARMS1 and PARMS2 are TREE_LISTs containing TREE_VECs: the
   data structure returned by DECL_TEMPLATE_PARMS.

   This is generally the same implementation as comp_template_parms
   except that it also the concept names and arguments used to
   introduce parameters.  */

static bool
template_parameter_lists_equivalent_p (const_tree parms1, const_tree parms2)
{
  if (parms1 == parms2)
    return true;

  const_tree p1 = parms1;
  const_tree p2 = parms2;
  while (p1 != NULL_TREE && p2 != NULL_TREE)
    {
      tree list1 = TREE_VALUE (p1);
      tree list2 = TREE_VALUE (p2);

      if (TREE_VEC_LENGTH (list1) != TREE_VEC_LENGTH (list2))
	return 0;

      for (int i = 0; i < TREE_VEC_LENGTH (list2); ++i)
	{
	  tree parm1 = TREE_VEC_ELT (list1, i);
	  tree parm2 = TREE_VEC_ELT (list2, i);
	  if (!template_parameters_equivalent_p (parm1, parm2))
	    return false;
	}

      p1 = TREE_CHAIN (p1);
      p2 = TREE_CHAIN (p2);
    }

  if ((p1 != NULL_TREE) != (p2 != NULL_TREE))
    return false;

  return true;
}

/* Return true if the requires-clause of the template parameter lists are
   equivalent and false otherwise.  */
static bool
template_requirements_equivalent_p (const_tree parms1, const_tree parms2)
{
  tree req1 = TEMPLATE_PARMS_CONSTRAINTS (parms1);
  tree req2 = TEMPLATE_PARMS_CONSTRAINTS (parms2);
  if ((req1 != NULL_TREE) != (req2 != NULL_TREE))
    return false;
  if (!cp_tree_equal (req1, req2))
    return false;
  return true;
}

/* Returns true if two template heads are equivalent. 17.6.6.1p6:
   Two template heads are equivalent if their template parameter
   lists are equivalent and their requires clauses are equivalent.

   In pre-C++20, this is equivalent to calling comp_template_parms
   for the template parameters of TMPL1 and TMPL2.  */

bool
template_heads_equivalent_p (const_tree tmpl1, const_tree tmpl2)
{
  tree parms1 = DECL_TEMPLATE_PARMS (tmpl1);
  tree parms2 = DECL_TEMPLATE_PARMS (tmpl2);

  /* Don't change the matching rules for pre-C++20.  */
  if (cxx_dialect < cxx20)
    return comp_template_parms (parms1, parms2);

  /* ... have the same number of template parameters, and their
     corresponding parameters are equivalent.  */
  if (!template_parameter_lists_equivalent_p (parms1, parms2))
    return false;

  /* ... if either has a requires-clause, they both do and their
     corresponding constraint-expressions are equivalent.  */
  return template_requirements_equivalent_p (parms1, parms2);
}

/* Determine whether PARM is a parameter pack.  */

bool
template_parameter_pack_p (const_tree parm)
{
  /* Determine if we have a non-type template parameter pack.  */
  if (TREE_CODE (parm) == PARM_DECL)
    return (DECL_TEMPLATE_PARM_P (parm)
            && TEMPLATE_PARM_PARAMETER_PACK (DECL_INITIAL (parm)));
  if (TREE_CODE (parm) == TEMPLATE_PARM_INDEX)
    return TEMPLATE_PARM_PARAMETER_PACK (parm);

  /* If this is a list of template parameters, we could get a
     TYPE_DECL or a TEMPLATE_DECL.  */
  if (TREE_CODE (parm) == TYPE_DECL || TREE_CODE (parm) == TEMPLATE_DECL)
    parm = TREE_TYPE (parm);

  /* Otherwise it must be a type template parameter.  */
  return ((TREE_CODE (parm) == TEMPLATE_TYPE_PARM
	   || TREE_CODE (parm) == TEMPLATE_TEMPLATE_PARM)
	  && TEMPLATE_TYPE_PARAMETER_PACK (parm));
}

/* Determine if T is a function parameter pack.  */

bool
function_parameter_pack_p (const_tree t)
{
  if (t && TREE_CODE (t) == PARM_DECL)
    return DECL_PACK_P (t);
  return false;
}

/* Return the function template declaration of PRIMARY_FUNC_TMPL_INST.
   PRIMARY_FUNC_TMPL_INST is a primary function template instantiation.  */

tree
get_function_template_decl (const_tree primary_func_tmpl_inst)
{
  if (! primary_func_tmpl_inst
      || TREE_CODE (primary_func_tmpl_inst) != FUNCTION_DECL
      || ! primary_template_specialization_p (primary_func_tmpl_inst))
    return NULL;

  return DECL_TEMPLATE_RESULT (DECL_TI_TEMPLATE (primary_func_tmpl_inst));
}

/* Return true iff the function parameter PARAM_DECL was expanded
   from the function parameter pack PACK.  */

bool
function_parameter_expanded_from_pack_p (tree param_decl, tree pack)
{
  if (DECL_ARTIFICIAL (param_decl)
      || !function_parameter_pack_p (pack))
    return false;

  /* The parameter pack and its pack arguments have the same
     DECL_PARM_INDEX.  */
  return DECL_PARM_INDEX (pack) == DECL_PARM_INDEX (param_decl);
}

/* Determine whether ARGS describes a variadic template args list,
   i.e., one that is terminated by a template argument pack.  */

static bool
template_args_variadic_p (tree args)
{
  int nargs;
  tree last_parm;

  if (args == NULL_TREE)
    return false;

  args = INNERMOST_TEMPLATE_ARGS (args);
  nargs = TREE_VEC_LENGTH (args);

  if (nargs == 0)
    return false;

  last_parm = TREE_VEC_ELT (args, nargs - 1);

  return ARGUMENT_PACK_P (last_parm);
}

/* Generate a new name for the parameter pack name NAME (an
   IDENTIFIER_NODE) that incorporates its */

static tree
make_ith_pack_parameter_name (tree name, int i)
{
  /* Munge the name to include the parameter index.  */
#define NUMBUF_LEN 128
  char numbuf[NUMBUF_LEN];
  char* newname;
  int newname_len;

  if (name == NULL_TREE)
    return name;
  snprintf (numbuf, NUMBUF_LEN, "%i", i);
  newname_len = IDENTIFIER_LENGTH (name)
	        + strlen (numbuf) + 2;
  newname = (char*)alloca (newname_len);
  snprintf (newname, newname_len,
	    "%s#%i", IDENTIFIER_POINTER (name), i);
  return get_identifier (newname);
}

/* Return true if T is a primary function, class or alias template
   specialization, not including the template pattern.  */

bool
primary_template_specialization_p (const_tree t)
{
  if (!t)
    return false;

  if (VAR_OR_FUNCTION_DECL_P (t))
    return (DECL_LANG_SPECIFIC (t)
	    && DECL_USE_TEMPLATE (t)
	    && DECL_TEMPLATE_INFO (t)
	    && PRIMARY_TEMPLATE_P (DECL_TI_TEMPLATE (t)));
  else if (CLASS_TYPE_P (t) && !TYPE_DECL_ALIAS_P (TYPE_NAME (t)))
    return (CLASSTYPE_TEMPLATE_INFO (t)
	    && CLASSTYPE_USE_TEMPLATE (t)
	    && PRIMARY_TEMPLATE_P (CLASSTYPE_TI_TEMPLATE (t)));
  else if (alias_template_specialization_p (t, nt_transparent))
    return true;
  return false;
}

/* Return true if PARM is a template template parameter.  */

bool
template_template_parameter_p (const_tree parm)
{
  return DECL_TEMPLATE_TEMPLATE_PARM_P (parm);
}

/* Return true iff PARM is a DECL representing a type template
   parameter.  */

bool
template_type_parameter_p (const_tree parm)
{
  return (parm
	  && (TREE_CODE (parm) == TYPE_DECL
	      || TREE_CODE (parm) == TEMPLATE_DECL)
	  && DECL_TEMPLATE_PARM_P (parm));
}

/* Return the template parameters of T if T is a
   primary template instantiation, NULL otherwise.  */

tree
get_primary_template_innermost_parameters (const_tree t)
{
  tree parms = NULL, template_info = NULL;

  if ((template_info = get_template_info (t))
      && primary_template_specialization_p (t))
    parms = INNERMOST_TEMPLATE_PARMS
	(DECL_TEMPLATE_PARMS (TI_TEMPLATE (template_info)));

  return parms;
}

/* Returns the template arguments of T if T is a template instantiation,
   NULL otherwise.  */

tree
get_template_innermost_arguments (const_tree t)
{
  tree args = NULL, template_info = NULL;

  if ((template_info = get_template_info (t))
      && TI_ARGS (template_info))
    args = INNERMOST_TEMPLATE_ARGS (TI_ARGS (template_info));

  return args;
}

/* Return the argument pack elements of T if T is a template argument pack,
   NULL otherwise.  */

tree
get_template_argument_pack_elems (const_tree t)
{
  if (TREE_CODE (t) != TYPE_ARGUMENT_PACK
      && TREE_CODE (t) != NONTYPE_ARGUMENT_PACK)
    return NULL;

  return ARGUMENT_PACK_ARGS (t);
}

/* In an ARGUMENT_PACK_SELECT, the actual underlying argument that the
   ARGUMENT_PACK_SELECT represents. */

static tree
argument_pack_select_arg (tree t)
{
  tree args = ARGUMENT_PACK_ARGS (ARGUMENT_PACK_SELECT_FROM_PACK (t));
  tree arg = TREE_VEC_ELT (args, ARGUMENT_PACK_SELECT_INDEX (t));

  /* If the selected argument is an expansion E, that most likely means we were
     called from gen_elem_of_pack_expansion_instantiation during the
     substituting of an argument pack (of which the Ith element is a pack
     expansion, where I is ARGUMENT_PACK_SELECT_INDEX) into a pack expansion.
     In this case, the Ith element resulting from this substituting is going to
     be a pack expansion, which pattern is the pattern of E.  Let's return the
     pattern of E, and gen_elem_of_pack_expansion_instantiation will build the
     resulting pack expansion from it.  */
  if (PACK_EXPANSION_P (arg))
    {
      /* Make sure we aren't throwing away arg info.  */
      gcc_assert (!PACK_EXPANSION_EXTRA_ARGS (arg));
      arg = PACK_EXPANSION_PATTERN (arg);
    }

  return arg;
}

/* Return a modification of ARGS that's suitable for preserving inside a hash
   table.  In particular, this replaces each ARGUMENT_PACK_SELECT with its
   underlying argument.  ARGS is copied (upon modification) iff COW_P.  */

static tree
preserve_args (tree args, bool cow_p = true)
{
  if (!args)
    return NULL_TREE;

  for (int i = 0, len = TREE_VEC_LENGTH (args); i < len; ++i)
    {
      tree t = TREE_VEC_ELT (args, i);
      tree r;
      if (!t)
	r = NULL_TREE;
      else if (TREE_CODE (t) == ARGUMENT_PACK_SELECT)
	r = argument_pack_select_arg (t);
      else if (TREE_CODE (t) == TREE_VEC)
	r = preserve_args (t, cow_p);
      else
	r = t;
      if (r != t)
	{
	  if (cow_p)
	    {
	      args = copy_template_args (args);
	      cow_p = false;
	    }
	  TREE_VEC_ELT (args, i) = r;
	}
    }

  return args;
}

/* True iff FN is a function representing a built-in variadic parameter
   pack.  */

bool
builtin_pack_fn_p (tree fn)
{
  if (!fn
      || TREE_CODE (fn) != FUNCTION_DECL
      || !DECL_IS_UNDECLARED_BUILTIN (fn))
    return false;

  if (id_equal (DECL_NAME (fn), "__integer_pack"))
    return true;

  return false;
}

/* True iff CALL is a call to a function representing a built-in variadic
   parameter pack.  */

static bool
builtin_pack_call_p (tree call)
{
  if (TREE_CODE (call) != CALL_EXPR)
    return false;
  return builtin_pack_fn_p (CALL_EXPR_FN (call));
}

/* Return a TREE_VEC for the expansion of __integer_pack(HI).  */

static tree
expand_integer_pack (tree call, tree args, tsubst_flags_t complain,
		     tree in_decl)
{
  tree ohi = CALL_EXPR_ARG (call, 0);
  tree hi = tsubst_copy_and_build (ohi, args, complain, in_decl);

  if (instantiation_dependent_expression_p (hi))
    {
      if (hi != ohi)
	{
	  call = copy_node (call);
	  CALL_EXPR_ARG (call, 0) = hi;
	}
      tree ex = make_pack_expansion (call, complain);
      tree vec = make_tree_vec (1);
      TREE_VEC_ELT (vec, 0) = ex;
      return vec;
    }
  else
    {
      hi = instantiate_non_dependent_expr (hi, complain);
      hi = cxx_constant_value (hi, complain);
      int len = valid_constant_size_p (hi) ? tree_to_shwi (hi) : -1;

      /* Calculate the largest value of len that won't make the size of the vec
	 overflow an int.  The compiler will exceed resource limits long before
	 this, but it seems a decent place to diagnose.  */
      int max = ((INT_MAX - sizeof (tree_vec)) / sizeof (tree)) + 1;

      if (len < 0 || len > max)
	{
	  if ((complain & tf_error)
	      && hi != error_mark_node)
	    error ("argument to %<__integer_pack%> must be between 0 and %d",
		   max);
	  return error_mark_node;
	}

      tree vec = make_tree_vec (len);

      for (int i = 0; i < len; ++i)
	TREE_VEC_ELT (vec, i) = size_int (i);

      return vec;
    }
}

/* Return a TREE_VEC for the expansion of built-in template parameter pack
   CALL.  */

static tree
expand_builtin_pack_call (tree call, tree args, tsubst_flags_t complain,
			  tree in_decl)
{
  if (!builtin_pack_call_p (call))
    return NULL_TREE;

  tree fn = CALL_EXPR_FN (call);

  if (id_equal (DECL_NAME (fn), "__integer_pack"))
    return expand_integer_pack (call, args, complain, in_decl);

  return NULL_TREE;
}

/* Return true if the tree T has the extra args mechanism for
   avoiding partial instantiation.  */

static bool
has_extra_args_mechanism_p (const_tree t)
{
  return (PACK_EXPANSION_P (t) /* PACK_EXPANSION_EXTRA_ARGS  */
	  || TREE_CODE (t) == REQUIRES_EXPR /* REQUIRES_EXPR_EXTRA_ARGS  */
	  || (TREE_CODE (t) == IF_STMT
	      && IF_STMT_CONSTEXPR_P (t))); /* IF_STMT_EXTRA_ARGS  */
}

/* Structure used to track the progress of find_parameter_packs_r.  */
struct find_parameter_pack_data
{
  /* TREE_LIST that will contain all of the parameter packs found by
     the traversal.  */
  tree* parameter_packs;

  /* Set of AST nodes that have been visited by the traversal.  */
  hash_set<tree> *visited;

  /* True iff we're making a type pack expansion.  */
  bool type_pack_expansion_p;

  /* True iff we found a subtree that has the extra args mechanism.  */
  bool found_extra_args_tree_p = false;
};

/* Identifies all of the argument packs that occur in a template
   argument and appends them to the TREE_LIST inside DATA, which is a
   find_parameter_pack_data structure. This is a subroutine of
   make_pack_expansion and uses_parameter_packs.  */
static tree
find_parameter_packs_r (tree *tp, int *walk_subtrees, void* data)
{
  tree t = *tp;
  struct find_parameter_pack_data* ppd =
    (struct find_parameter_pack_data*)data;
  bool parameter_pack_p = false;

#define WALK_SUBTREE(NODE)				\
  cp_walk_tree (&(NODE), &find_parameter_packs_r,	\
		ppd, ppd->visited)			\

  /* Don't look through typedefs; we are interested in whether a
     parameter pack is actually written in the expression/type we're
     looking at, not the target type.  */
  if (TYPE_P (t) && typedef_variant_p (t))
    {
      /* But do look at arguments for an alias template.  */
      if (tree tinfo = TYPE_ALIAS_TEMPLATE_INFO (t))
	cp_walk_tree (&TI_ARGS (tinfo),
		      &find_parameter_packs_r,
		      ppd, ppd->visited);
      *walk_subtrees = 0;
      return NULL_TREE;
    }

  /* Identify whether this is a parameter pack or not.  */
  switch (TREE_CODE (t))
    {
    case TEMPLATE_PARM_INDEX:
      if (TEMPLATE_PARM_PARAMETER_PACK (t))
        parameter_pack_p = true;
      break;

    case TEMPLATE_TYPE_PARM:
      t = TYPE_MAIN_VARIANT (t);
      /* FALLTHRU */
    case TEMPLATE_TEMPLATE_PARM:
      /* If the placeholder appears in the decl-specifier-seq of a function
	 parameter pack (14.6.3), or the type-specifier-seq of a type-id that
	 is a pack expansion, the invented template parameter is a template
	 parameter pack.  */
      if (ppd->type_pack_expansion_p && is_auto (t))
	TEMPLATE_TYPE_PARAMETER_PACK (t) = true;
      if (TEMPLATE_TYPE_PARAMETER_PACK (t))
        parameter_pack_p = true;
      break;

    case FIELD_DECL:
    case PARM_DECL:
      if (DECL_PACK_P (t))
        {
          /* We don't want to walk into the type of a PARM_DECL,
             because we don't want to see the type parameter pack.  */
          *walk_subtrees = 0;
	  parameter_pack_p = true;
        }
      break;

    case VAR_DECL:
      if (DECL_PACK_P (t))
        {
          /* We don't want to walk into the type of a variadic capture proxy,
             because we don't want to see the type parameter pack.  */
          *walk_subtrees = 0;
	  parameter_pack_p = true;
        }
      else if (variable_template_specialization_p (t))
	{
	  cp_walk_tree (&DECL_TI_ARGS (t),
			find_parameter_packs_r,
			ppd, ppd->visited);
	  *walk_subtrees = 0;
	}
      break;

    case CALL_EXPR:
      if (builtin_pack_call_p (t))
	parameter_pack_p = true;
      break;

    case BASES:
      parameter_pack_p = true;
      break;
    default:
      /* Not a parameter pack.  */
      break;
    }

  if (parameter_pack_p)
    {
      /* Add this parameter pack to the list.  */
      *ppd->parameter_packs = tree_cons (NULL_TREE, t, *ppd->parameter_packs);
    }

  if (has_extra_args_mechanism_p (t) && !PACK_EXPANSION_P (t))
    ppd->found_extra_args_tree_p = true;

  if (TYPE_P (t))
    cp_walk_tree (&TYPE_CONTEXT (t),
		  &find_parameter_packs_r, ppd, ppd->visited);

  /* This switch statement will return immediately if we don't find a
     parameter pack.  ??? Should some of these be in cp_walk_subtrees?  */
  switch (TREE_CODE (t))
    {
    case BOUND_TEMPLATE_TEMPLATE_PARM:
      /* Check the template itself.  */
      cp_walk_tree (&TREE_TYPE (TYPE_TI_TEMPLATE (t)),
		    &find_parameter_packs_r, ppd, ppd->visited);
      return NULL_TREE;

    case DECL_EXPR:
      {
	tree decl = DECL_EXPR_DECL (t);
	/* Ignore the declaration of a capture proxy for a parameter pack.  */
	if (is_capture_proxy (decl))
	  *walk_subtrees = 0;
	if (is_typedef_decl (decl))
	  /* Since we stop at typedefs above, we need to look through them at
	     the point of the DECL_EXPR.  */
	  cp_walk_tree (&DECL_ORIGINAL_TYPE (decl),
			&find_parameter_packs_r, ppd, ppd->visited);
	return NULL_TREE;
      }

    case TEMPLATE_DECL:
      if (!DECL_TEMPLATE_TEMPLATE_PARM_P (t))
	return NULL_TREE;
      cp_walk_tree (&TREE_TYPE (t),
		    &find_parameter_packs_r, ppd, ppd->visited);
      return NULL_TREE;

    case TYPE_PACK_EXPANSION:
    case EXPR_PACK_EXPANSION:
      *walk_subtrees = 0;
      return NULL_TREE;

    case INTEGER_TYPE:
      cp_walk_tree (&TYPE_MAX_VALUE (t), &find_parameter_packs_r,
		    ppd, ppd->visited);
      *walk_subtrees = 0;
      return NULL_TREE;

    case IDENTIFIER_NODE:
      cp_walk_tree (&TREE_TYPE (t), &find_parameter_packs_r, ppd,
		    ppd->visited);
      *walk_subtrees = 0;
      return NULL_TREE;

    case LAMBDA_EXPR:
      {
	/* Since we defer implicit capture, look in the parms and body.  */
	tree fn = lambda_function (t);
	cp_walk_tree (&TREE_TYPE (fn), &find_parameter_packs_r, ppd,
		      ppd->visited);
	cp_walk_tree (&DECL_SAVED_TREE (fn), &find_parameter_packs_r, ppd,
		      ppd->visited);
	return NULL_TREE;
      }

    case DECLTYPE_TYPE:
      {
	/* When traversing a DECLTYPE_TYPE_EXPR, we need to set
	   type_pack_expansion_p to false so that any placeholders
	   within the expression don't get marked as parameter packs.  */
	bool type_pack_expansion_p = ppd->type_pack_expansion_p;
	ppd->type_pack_expansion_p = false;
	cp_walk_tree (&DECLTYPE_TYPE_EXPR (t), &find_parameter_packs_r,
		      ppd, ppd->visited);
	ppd->type_pack_expansion_p = type_pack_expansion_p;
	*walk_subtrees = 0;
	return NULL_TREE;
      }

    case IF_STMT:
      cp_walk_tree (&IF_COND (t), &find_parameter_packs_r,
		    ppd, ppd->visited);
      cp_walk_tree (&THEN_CLAUSE (t), &find_parameter_packs_r,
		    ppd, ppd->visited);
      cp_walk_tree (&ELSE_CLAUSE (t), &find_parameter_packs_r,
		    ppd, ppd->visited);
      /* Don't walk into IF_STMT_EXTRA_ARGS.  */
      *walk_subtrees = 0;
      return NULL_TREE;

    case TAG_DEFN:
      t = TREE_TYPE (t);
      if (CLASS_TYPE_P (t))
	{
	  /* Local class, need to look through the whole definition.
	     TYPE_BINFO might be unset for a partial instantiation.  */
	  if (TYPE_BINFO (t))
	    for (tree bb : BINFO_BASE_BINFOS (TYPE_BINFO (t)))
	      cp_walk_tree (&BINFO_TYPE (bb), &find_parameter_packs_r,
			    ppd, ppd->visited);
	}
      else
	/* Enum, look at the values.  */
	for (tree l = TYPE_VALUES (t); l; l = TREE_CHAIN (l))
	  cp_walk_tree (&DECL_INITIAL (TREE_VALUE (l)),
			&find_parameter_packs_r,
			ppd, ppd->visited);
      return NULL_TREE;

    case FUNCTION_TYPE:
    case METHOD_TYPE:
      WALK_SUBTREE (TYPE_RAISES_EXCEPTIONS (t));
      break;

    default:
      return NULL_TREE;
    }

#undef WALK_SUBTREE

  return NULL_TREE;
}

/* Determines if the expression or type T uses any parameter packs.  */
tree
uses_parameter_packs (tree t)
{
  tree parameter_packs = NULL_TREE;
  struct find_parameter_pack_data ppd;
  ppd.parameter_packs = &parameter_packs;
  ppd.visited = new hash_set<tree>;
  ppd.type_pack_expansion_p = false;
  cp_walk_tree (&t, &find_parameter_packs_r, &ppd, ppd.visited);
  delete ppd.visited;
  return parameter_packs;
}

/* Turn ARG, which may be an expression, type, or a TREE_LIST
   representation a base-class initializer into a parameter pack
   expansion. If all goes well, the resulting node will be an
   EXPR_PACK_EXPANSION, TYPE_PACK_EXPANSION, or TREE_LIST,
   respectively.  */
tree
make_pack_expansion (tree arg, tsubst_flags_t complain)
{
  tree result;
  tree parameter_packs = NULL_TREE;
  bool for_types = false;
  struct find_parameter_pack_data ppd;

  if (!arg || arg == error_mark_node)
    return arg;

  if (TREE_CODE (arg) == TREE_LIST && TREE_PURPOSE (arg))
    {
      /* A TREE_LIST with a non-null TREE_PURPOSE is for a base
         class initializer.  In this case, the TREE_PURPOSE will be a
         _TYPE node (representing the base class expansion we're
         initializing) and the TREE_VALUE will be a TREE_LIST
         containing the initialization arguments. 

         The resulting expansion looks somewhat different from most
         expansions. Rather than returning just one _EXPANSION, we
         return a TREE_LIST whose TREE_PURPOSE is a
         TYPE_PACK_EXPANSION containing the bases that will be
         initialized.  The TREE_VALUE will be identical to the
         original TREE_VALUE, which is a list of arguments that will
         be passed to each base.  We do not introduce any new pack
         expansion nodes into the TREE_VALUE (although it is possible
         that some already exist), because the TREE_PURPOSE and
         TREE_VALUE all need to be expanded together with the same
         _EXPANSION node.  Note that the TYPE_PACK_EXPANSION in the
         resulting TREE_PURPOSE will mention the parameter packs in
         both the bases and the arguments to the bases.  */
      tree purpose;
      tree value;
      tree parameter_packs = NULL_TREE;

      /* Determine which parameter packs will be used by the base
         class expansion.  */
      ppd.visited = new hash_set<tree>;
      ppd.parameter_packs = &parameter_packs;
      ppd.type_pack_expansion_p = false;
      gcc_assert (TYPE_P (TREE_PURPOSE (arg)));
      cp_walk_tree (&TREE_PURPOSE (arg), &find_parameter_packs_r,
                    &ppd, ppd.visited);

      if (parameter_packs == NULL_TREE)
        {
	  if (complain & tf_error)
	    error ("base initializer expansion %qT contains no parameter packs",
		   arg);
          delete ppd.visited;
          return error_mark_node;
        }

      if (TREE_VALUE (arg) != void_type_node)
        {
          /* Collect the sets of parameter packs used in each of the
             initialization arguments.  */
          for (value = TREE_VALUE (arg); value; value = TREE_CHAIN (value))
            {
              /* Determine which parameter packs will be expanded in this
                 argument.  */
              cp_walk_tree (&TREE_VALUE (value), &find_parameter_packs_r, 
                            &ppd, ppd.visited);
            }
        }

      delete ppd.visited;

      /* Create the pack expansion type for the base type.  */
      purpose = cxx_make_type (TYPE_PACK_EXPANSION);
      PACK_EXPANSION_PATTERN (purpose) = TREE_PURPOSE (arg);
      PACK_EXPANSION_PARAMETER_PACKS (purpose) = parameter_packs;
      PACK_EXPANSION_LOCAL_P (purpose) = at_function_scope_p ();

      /* Just use structural equality for these TYPE_PACK_EXPANSIONS;
	 they will rarely be compared to anything.  */
      SET_TYPE_STRUCTURAL_EQUALITY (purpose);

      return tree_cons (purpose, TREE_VALUE (arg), NULL_TREE);
    }

  if (TYPE_P (arg) || TREE_CODE (arg) == TEMPLATE_DECL)
    for_types = true;

  /* Build the PACK_EXPANSION_* node.  */
  result = for_types
     ? cxx_make_type (TYPE_PACK_EXPANSION)
     : make_node (EXPR_PACK_EXPANSION);
  PACK_EXPANSION_PATTERN (result) = arg;
  if (TREE_CODE (result) == EXPR_PACK_EXPANSION)
    {
      /* Propagate type and const-expression information.  */
      TREE_TYPE (result) = TREE_TYPE (arg);
      TREE_CONSTANT (result) = TREE_CONSTANT (arg);
      /* Mark this read now, since the expansion might be length 0.  */
      mark_exp_read (arg);
    }
  else
    /* Just use structural equality for these TYPE_PACK_EXPANSIONS;
       they will rarely be compared to anything.  */
    SET_TYPE_STRUCTURAL_EQUALITY (result);

  /* Determine which parameter packs will be expanded.  */
  ppd.parameter_packs = &parameter_packs;
  ppd.visited = new hash_set<tree>;
  ppd.type_pack_expansion_p = TYPE_P (arg);
  cp_walk_tree (&arg, &find_parameter_packs_r, &ppd, ppd.visited);
  delete ppd.visited;

  /* Make sure we found some parameter packs.  */
  if (parameter_packs == NULL_TREE)
    {
      if (complain & tf_error)
	{
	  if (TYPE_P (arg))
	    error ("expansion pattern %qT contains no parameter packs", arg);
	  else
	    error ("expansion pattern %qE contains no parameter packs", arg);
	}
      return error_mark_node;
    }
  PACK_EXPANSION_PARAMETER_PACKS (result) = parameter_packs;

  PACK_EXPANSION_LOCAL_P (result) = at_function_scope_p ();
  if (ppd.found_extra_args_tree_p)
    /* If the pattern of this pack expansion contains a subtree that has
       the extra args mechanism for avoiding partial instantiation, then
       force this pack expansion to also use extra args.  Otherwise
       partial instantiation of this pack expansion may not lower the
       level of some parameter packs within the pattern, which would
       confuse tsubst_pack_expansion later (PR101764).  */
    PACK_EXPANSION_FORCE_EXTRA_ARGS_P (result) = true;

  return result;
}

/* Checks T for any "bare" parameter packs, which have not yet been
   expanded, and issues an error if any are found. This operation can
   only be done on full expressions or types (e.g., an expression
   statement, "if" condition, etc.), because we could have expressions like:

     foo(f(g(h(args)))...)

   where "args" is a parameter pack. check_for_bare_parameter_packs
   should not be called for the subexpressions args, h(args),
   g(h(args)), or f(g(h(args))), because we would produce erroneous
   error messages.

   Returns TRUE and emits an error if there were bare parameter packs,
   returns FALSE otherwise.  */
bool
check_for_bare_parameter_packs (tree t, location_t loc /* = UNKNOWN_LOCATION */)
{
  tree parameter_packs = NULL_TREE;
  struct find_parameter_pack_data ppd;

  if (!processing_template_decl || !t || t == error_mark_node)
    return false;

  if (TREE_CODE (t) == TYPE_DECL)
    t = TREE_TYPE (t);

  ppd.parameter_packs = &parameter_packs;
  ppd.visited = new hash_set<tree>;
  ppd.type_pack_expansion_p = false;
  cp_walk_tree (&t, &find_parameter_packs_r, &ppd, ppd.visited);
  delete ppd.visited;

  if (!parameter_packs)
    return false;

  if (loc == UNKNOWN_LOCATION)
    loc = cp_expr_loc_or_input_loc (t);

  /* It's OK for a lambda to have an unexpanded parameter pack from the
     containing context, but do complain about unexpanded capture packs.  */
  tree lam = current_lambda_expr ();
  if (lam)
    lam = TREE_TYPE (lam);

  if (lam && lam != current_class_type)
    {
      /* We're in a lambda, but it isn't the innermost class.
	 This should work, but currently doesn't.  */
      sorry_at (loc, "unexpanded parameter pack in local class in lambda");
      return true;
    }

  if (lam && CLASSTYPE_TEMPLATE_INFO (lam))
    for (; parameter_packs;
	 parameter_packs = TREE_CHAIN (parameter_packs))
      {
	tree pack = TREE_VALUE (parameter_packs);
	if (is_capture_proxy (pack)
	    || (TREE_CODE (pack) == PARM_DECL
		&& DECL_CONTEXT (DECL_CONTEXT (pack)) == lam))
	  break;
      }

  if (parameter_packs)
    {
      error_at (loc, "parameter packs not expanded with %<...%>:");
      while (parameter_packs)
        {
          tree pack = TREE_VALUE (parameter_packs);
          tree name = NULL_TREE;

          if (TREE_CODE (pack) == TEMPLATE_TYPE_PARM
              || TREE_CODE (pack) == TEMPLATE_TEMPLATE_PARM)
            name = TYPE_NAME (pack);
          else if (TREE_CODE (pack) == TEMPLATE_PARM_INDEX)
            name = DECL_NAME (TEMPLATE_PARM_DECL (pack));
	  else if (TREE_CODE (pack) == CALL_EXPR)
	    name = DECL_NAME (CALL_EXPR_FN (pack));
          else
            name = DECL_NAME (pack);

	  if (name)
	    inform (loc, "        %qD", name);
	  else
	    inform (loc, "        %s", "<anonymous>");

          parameter_packs = TREE_CHAIN (parameter_packs);
        }

      return true;
    }

  return false;
}

/* Expand any parameter packs that occur in the template arguments in
   ARGS.  */
tree
expand_template_argument_pack (tree args)
{
  if (args == error_mark_node)
    return error_mark_node;

  tree result_args = NULL_TREE;
  int in_arg, out_arg = 0, nargs = args ? TREE_VEC_LENGTH (args) : 0;
  int num_result_args = -1;
  int non_default_args_count = -1;

  /* First, determine if we need to expand anything, and the number of
     slots we'll need.  */
  for (in_arg = 0; in_arg < nargs; ++in_arg)
    {
      tree arg = TREE_VEC_ELT (args, in_arg);
      if (arg == NULL_TREE)
	return args;
      if (ARGUMENT_PACK_P (arg))
        {
          int num_packed = TREE_VEC_LENGTH (ARGUMENT_PACK_ARGS (arg));
          if (num_result_args < 0)
            num_result_args = in_arg + num_packed;
          else
            num_result_args += num_packed;
        }
      else
        {
          if (num_result_args >= 0)
            num_result_args++;
        }
    }

  /* If no expansion is necessary, we're done.  */
  if (num_result_args < 0)
    return args;

  /* Expand arguments.  */
  result_args = make_tree_vec (num_result_args);
  if (NON_DEFAULT_TEMPLATE_ARGS_COUNT (args))
    non_default_args_count =
      GET_NON_DEFAULT_TEMPLATE_ARGS_COUNT (args);
  for (in_arg = 0; in_arg < nargs; ++in_arg)
    {
      tree arg = TREE_VEC_ELT (args, in_arg);
      if (ARGUMENT_PACK_P (arg))
        {
          tree packed = ARGUMENT_PACK_ARGS (arg);
          int i, num_packed = TREE_VEC_LENGTH (packed);
          for (i = 0; i < num_packed; ++i, ++out_arg)
            TREE_VEC_ELT (result_args, out_arg) = TREE_VEC_ELT(packed, i);
	  if (non_default_args_count > 0)
	    non_default_args_count += num_packed - 1;
        }
      else
        {
          TREE_VEC_ELT (result_args, out_arg) = arg;
          ++out_arg;
        }
    }
  if (non_default_args_count >= 0)
    SET_NON_DEFAULT_TEMPLATE_ARGS_COUNT (result_args, non_default_args_count);
  return result_args;
}

/* Checks if DECL shadows a template parameter.

   [temp.local]: A template-parameter shall not be redeclared within its
   scope (including nested scopes).

   Emits an error and returns TRUE if the DECL shadows a parameter,
   returns FALSE otherwise.  */

bool
check_template_shadow (tree decl)
{
  tree olddecl;

  /* If we're not in a template, we can't possibly shadow a template
     parameter.  */
  if (!current_template_parms)
    return true;

  /* Figure out what we're shadowing.  */
  decl = OVL_FIRST (decl);
  olddecl = innermost_non_namespace_value (DECL_NAME (decl));

  /* If there's no previous binding for this name, we're not shadowing
     anything, let alone a template parameter.  */
  if (!olddecl)
    return true;

  /* If we're not shadowing a template parameter, we're done.  Note
     that OLDDECL might be an OVERLOAD (or perhaps even an
     ERROR_MARK), so we can't just blithely assume it to be a _DECL
     node.  */
  if (!DECL_P (olddecl) || !DECL_TEMPLATE_PARM_P (olddecl))
    return true;

  /* We check for decl != olddecl to avoid bogus errors for using a
     name inside a class.  We check TPFI to avoid duplicate errors for
     inline member templates.  */
  if (decl == olddecl
      || (DECL_TEMPLATE_PARM_P (decl)
	  && TEMPLATE_PARMS_FOR_INLINE (current_template_parms)))
    return true;

  /* Don't complain about the injected class name, as we've already
     complained about the class itself.  */
  if (DECL_SELF_REFERENCE_P (decl))
    return false;

  if (DECL_TEMPLATE_PARM_P (decl))
    error ("declaration of template parameter %q+D shadows "
	   "template parameter", decl);
  else
    error ("declaration of %q+#D shadows template parameter", decl);
  inform (DECL_SOURCE_LOCATION (olddecl),
	  "template parameter %qD declared here", olddecl);
  return false;
}

/* Return a new TEMPLATE_PARM_INDEX with the indicated INDEX, LEVEL,
   ORIG_LEVEL, DECL, and TYPE.  */

static tree
build_template_parm_index (int index,
			   int level,
			   int orig_level,
			   tree decl,
			   tree type)
{
  tree t = make_node (TEMPLATE_PARM_INDEX);
  TEMPLATE_PARM_IDX (t) = index;
  TEMPLATE_PARM_LEVEL (t) = level;
  TEMPLATE_PARM_ORIG_LEVEL (t) = orig_level;
  TEMPLATE_PARM_DECL (t) = decl;
  TREE_TYPE (t) = type;
  TREE_CONSTANT (t) = TREE_CONSTANT (decl);
  TREE_READONLY (t) = TREE_READONLY (decl);

  return t;
}

struct ctp_hasher : ggc_ptr_hash<tree_node>
{
  static hashval_t hash (tree t)
  {
    ++comparing_specializations;
    tree_code code = TREE_CODE (t);
    hashval_t val = iterative_hash_object (code, 0);
    val = iterative_hash_object (TEMPLATE_TYPE_LEVEL (t), val);
    val = iterative_hash_object (TEMPLATE_TYPE_IDX (t), val);
    if (TREE_CODE (t) == BOUND_TEMPLATE_TEMPLATE_PARM)
      val = iterative_hash_template_arg (TYPE_TI_ARGS (t), val);
    --comparing_specializations;
    return val;
  }

  static bool equal (tree t, tree u)
  {
    ++comparing_specializations;
    bool eq = comptypes (t, u, COMPARE_STRUCTURAL);
    --comparing_specializations;
    return eq;
  }
};

static GTY (()) hash_table<ctp_hasher> *ctp_table;

/* Find the canonical type parameter for the given template type
   parameter.  Returns the canonical type parameter, which may be TYPE
   if no such parameter existed.  */

tree
canonical_type_parameter (tree type)
{
  if (ctp_table == NULL)
    ctp_table = hash_table<ctp_hasher>::create_ggc (61);

  tree& slot = *ctp_table->find_slot (type, INSERT);
  if (slot == NULL_TREE)
    slot = type;
  return slot;
}

/* Return a TEMPLATE_PARM_INDEX, similar to INDEX, but whose
   TEMPLATE_PARM_LEVEL has been decreased by LEVELS.  If such a
   TEMPLATE_PARM_INDEX already exists, it is returned; otherwise, a
   new one is created.  */

static tree
reduce_template_parm_level (tree index, tree type, int levels, tree args,
			    tsubst_flags_t complain)
{
  if (TEMPLATE_PARM_DESCENDANTS (index) == NULL_TREE
      || (TEMPLATE_PARM_LEVEL (TEMPLATE_PARM_DESCENDANTS (index))
	  != TEMPLATE_PARM_LEVEL (index) - levels)
      || !same_type_p (type, TREE_TYPE (TEMPLATE_PARM_DESCENDANTS (index))))
    {
      tree orig_decl = TEMPLATE_PARM_DECL (index);

      tree decl = build_decl (DECL_SOURCE_LOCATION (orig_decl),
			      TREE_CODE (orig_decl), DECL_NAME (orig_decl),
			      type);
      TREE_CONSTANT (decl) = TREE_CONSTANT (orig_decl);
      TREE_READONLY (decl) = TREE_READONLY (orig_decl);
      DECL_VIRTUAL_P (decl) = DECL_VIRTUAL_P (orig_decl);
      DECL_ARTIFICIAL (decl) = 1;
      SET_DECL_TEMPLATE_PARM_P (decl);

      tree tpi = build_template_parm_index (TEMPLATE_PARM_IDX (index),
					    TEMPLATE_PARM_LEVEL (index) - levels,
					    TEMPLATE_PARM_ORIG_LEVEL (index),
					    decl, type);
      TEMPLATE_PARM_DESCENDANTS (index) = tpi;
      TEMPLATE_PARM_PARAMETER_PACK (tpi)
	= TEMPLATE_PARM_PARAMETER_PACK (index);

      /* Template template parameters need this.  */
      tree inner = decl;
      if (TREE_CODE (decl) == TEMPLATE_DECL)
	{
	  inner = build_lang_decl_loc (DECL_SOURCE_LOCATION (decl),
				       TYPE_DECL, DECL_NAME (decl), type);
	  DECL_TEMPLATE_RESULT (decl) = inner;
	  DECL_ARTIFICIAL (inner) = true;
	  tree parms = tsubst_template_parms (DECL_TEMPLATE_PARMS (orig_decl),
					      args, complain);
	  DECL_TEMPLATE_PARMS (decl) = parms;
	  tree orig_inner = DECL_TEMPLATE_RESULT (orig_decl);
	  DECL_TEMPLATE_INFO (inner)
	    = build_template_info (DECL_TI_TEMPLATE (orig_inner),
				   template_parms_to_args (parms));
	}

      /* Attach the TPI to the decl.  */
      if (TREE_CODE (inner) == TYPE_DECL)
	TEMPLATE_TYPE_PARM_INDEX (type) = tpi;
      else
	DECL_INITIAL (decl) = tpi;
    }

  return TEMPLATE_PARM_DESCENDANTS (index);
}

/* Process information from new template parameter PARM and append it
   to the LIST being built.  This new parameter is a non-type
   parameter iff IS_NON_TYPE is true. This new parameter is a
   parameter pack iff IS_PARAMETER_PACK is true.  The location of PARM
   is in PARM_LOC.  */

tree
process_template_parm (tree list, location_t parm_loc, tree parm,
		       bool is_non_type, bool is_parameter_pack)
{
  gcc_assert (TREE_CODE (parm) == TREE_LIST);
  tree prev = NULL_TREE;
  int idx = 0;

  if (list)
    {
      prev = tree_last (list);

      tree p = TREE_VALUE (prev);
      if (TREE_CODE (p) == TYPE_DECL || TREE_CODE (p) == TEMPLATE_DECL)
	idx = TEMPLATE_TYPE_IDX (TREE_TYPE (p));
      else if (TREE_CODE (p) == PARM_DECL)
	idx = TEMPLATE_PARM_IDX (DECL_INITIAL (p));

      ++idx;
    }

  tree decl = NULL_TREE;
  tree defval = TREE_PURPOSE (parm);
  tree constr = TREE_TYPE (parm);

  if (is_non_type)
    {
      parm = TREE_VALUE (parm);

      SET_DECL_TEMPLATE_PARM_P (parm);

      if (TREE_TYPE (parm) != error_mark_node)
	{
	  /* [temp.param]

	     The top-level cv-qualifiers on the template-parameter are
	     ignored when determining its type.  */
	  TREE_TYPE (parm) = TYPE_MAIN_VARIANT (TREE_TYPE (parm));
	  if (invalid_nontype_parm_type_p (TREE_TYPE (parm), 1))
	    TREE_TYPE (parm) = error_mark_node;
	  else if (uses_parameter_packs (TREE_TYPE (parm))
		   && !is_parameter_pack
		   /* If we're in a nested template parameter list, the template
		      template parameter could be a parameter pack.  */
		   && processing_template_parmlist == 1)
	    {
	      /* This template parameter is not a parameter pack, but it
		 should be. Complain about "bare" parameter packs.  */
	      check_for_bare_parameter_packs (TREE_TYPE (parm));

	      /* Recover by calling this a parameter pack.  */
	      is_parameter_pack = true;
	    }
	}

      /* A template parameter is not modifiable.  */
      TREE_CONSTANT (parm) = 1;
      TREE_READONLY (parm) = 1;
      decl = build_decl (parm_loc,
			 CONST_DECL, DECL_NAME (parm), TREE_TYPE (parm));
      TREE_CONSTANT (decl) = 1;
      TREE_READONLY (decl) = 1;
      DECL_INITIAL (parm) = DECL_INITIAL (decl)
	= build_template_parm_index (idx, current_template_depth,
				     current_template_depth,
				     decl, TREE_TYPE (parm));

      TEMPLATE_PARM_PARAMETER_PACK (DECL_INITIAL (parm))
	= is_parameter_pack;
    }
  else
    {
      tree t;
      parm = TREE_VALUE (TREE_VALUE (parm));

      if (parm && TREE_CODE (parm) == TEMPLATE_DECL)
	{
	  t = cxx_make_type (TEMPLATE_TEMPLATE_PARM);
	  /* This is for distinguishing between real templates and template
	     template parameters */
	  TREE_TYPE (parm) = t;

	  /* any_template_parm_r expects to be able to get the targs of a
	     DECL_TEMPLATE_RESULT.  */
	  tree result = DECL_TEMPLATE_RESULT (parm);
	  TREE_TYPE (result) = t;
	  tree args = template_parms_to_args (DECL_TEMPLATE_PARMS (parm));
	  tree tinfo = build_template_info (parm, args);
	  retrofit_lang_decl (result);
	  DECL_TEMPLATE_INFO (result) = tinfo;

	  decl = parm;
	}
      else
	{
	  t = cxx_make_type (TEMPLATE_TYPE_PARM);
	  /* parm is either IDENTIFIER_NODE or NULL_TREE.  */
	  decl = build_decl (parm_loc,
			     TYPE_DECL, parm, t);
	}

      TYPE_NAME (t) = decl;
      TYPE_STUB_DECL (t) = decl;
      parm = decl;
      TEMPLATE_TYPE_PARM_INDEX (t)
	= build_template_parm_index (idx, current_template_depth,
				     current_template_depth,
				     decl, TREE_TYPE (parm));
      TEMPLATE_TYPE_PARAMETER_PACK (t) = is_parameter_pack;
      TYPE_CANONICAL (t) = canonical_type_parameter (t);
    }
  DECL_ARTIFICIAL (decl) = 1;
  SET_DECL_TEMPLATE_PARM_P (decl);

  if (TREE_CODE (parm) == TEMPLATE_DECL
      && !uses_outer_template_parms (parm))
    TEMPLATE_TEMPLATE_PARM_SIMPLE_P (TREE_TYPE (parm)) = true;

  /* Build requirements for the type/template parameter.
     This must be done after SET_DECL_TEMPLATE_PARM_P or
     process_template_parm could fail. */
  tree reqs = finish_shorthand_constraint (parm, constr);

  decl = pushdecl (decl);
  if (!is_non_type)
    parm = decl;

  /* Build the parameter node linking the parameter declaration,
     its default argument (if any), and its constraints (if any). */
  parm = build_tree_list (defval, parm);
  TEMPLATE_PARM_CONSTRAINTS (parm) = reqs;

  if (prev)
    TREE_CHAIN (prev) = parm;
  else
    list = parm;

  return list;
}

/* The end of a template parameter list has been reached.  Process the
   tree list into a parameter vector, converting each parameter into a more
   useful form.	 Type parameters are saved as IDENTIFIER_NODEs, and others
   as PARM_DECLs.  */

tree
end_template_parm_list (tree parms)
{
  tree saved_parmlist = make_tree_vec (list_length (parms));

  /* Pop the dummy parameter level and add the real one.  We do not
     morph the dummy parameter in place, as it might have been
     captured by a (nested) template-template-parm.  */
  current_template_parms = TREE_CHAIN (current_template_parms);

  current_template_parms
    = tree_cons (size_int (current_template_depth + 1),
		 saved_parmlist, current_template_parms);

  for (unsigned ix = 0; parms; ix++)
    {
      tree parm = parms;
      parms = TREE_CHAIN (parms);
      TREE_CHAIN (parm) = NULL_TREE;

      TREE_VEC_ELT (saved_parmlist, ix) = parm;
    }

  --processing_template_parmlist;

  return saved_parmlist;
}

// Explicitly indicate the end of the template parameter list. We assume
// that the current template parameters have been constructed and/or
// managed explicitly, as when creating new template template parameters
// from a shorthand constraint.
void
end_template_parm_list ()
{
  --processing_template_parmlist;
}

/* end_template_decl is called after a template declaration is seen.  */

void
end_template_decl (void)
{
  reset_specialization ();

  if (! processing_template_decl)
    return;

  /* This matches the pushlevel in begin_template_parm_list.  */
  finish_scope ();

  --processing_template_decl;
  current_template_parms = TREE_CHAIN (current_template_parms);
}

/* Takes a TEMPLATE_PARM_P or DECL_TEMPLATE_PARM_P node or a TREE_LIST
   thereof, and converts it into an argument suitable to be passed to
   the type substitution functions.  Note that if the TREE_LIST contains
   an error_mark node, the returned argument is error_mark_node.  */

tree
template_parm_to_arg (tree t)
{
  if (!t)
    return NULL_TREE;

  if (TREE_CODE (t) == TREE_LIST)
    t = TREE_VALUE (t);

  if (error_operand_p (t))
    return error_mark_node;

  if (DECL_P (t) && DECL_TEMPLATE_PARM_P (t))
    {
      if (TREE_CODE (t) == TYPE_DECL
	  || TREE_CODE (t) == TEMPLATE_DECL)
	t = TREE_TYPE (t);
      else
	t = DECL_INITIAL (t);
    }

  gcc_assert (TEMPLATE_PARM_P (t));

  if (TREE_CODE (t) == TEMPLATE_TYPE_PARM
      || TREE_CODE (t) == TEMPLATE_TEMPLATE_PARM)
    {
      if (TEMPLATE_TYPE_PARAMETER_PACK (t))
	{
	  /* Turn this argument into a TYPE_ARGUMENT_PACK
	     with a single element, which expands T.  */
	  tree vec = make_tree_vec (1);
	  if (CHECKING_P)
	    SET_NON_DEFAULT_TEMPLATE_ARGS_COUNT (vec, TREE_VEC_LENGTH (vec));

	  TREE_VEC_ELT (vec, 0) = make_pack_expansion (t);

	  t = cxx_make_type (TYPE_ARGUMENT_PACK);
	  ARGUMENT_PACK_ARGS (t) = vec;
	}
    }
  else
    {
      if (TEMPLATE_PARM_PARAMETER_PACK (t))
	{
	  /* Turn this argument into a NONTYPE_ARGUMENT_PACK
	     with a single element, which expands T.  */
	  tree vec = make_tree_vec (1);
	  if (CHECKING_P)
	    SET_NON_DEFAULT_TEMPLATE_ARGS_COUNT (vec, TREE_VEC_LENGTH (vec));

	  t = convert_from_reference (t);
	  TREE_VEC_ELT (vec, 0) = make_pack_expansion (t);

	  t  = make_node (NONTYPE_ARGUMENT_PACK);
	  ARGUMENT_PACK_ARGS (t) = vec;
	}
      else
	t = convert_from_reference (t);
    }
  return t;
}

/* If T looks like a generic template argument produced by template_parm_to_arg,
   return the corresponding template parameter, otherwise return NULL_TREE.  */

static tree
template_arg_to_parm (tree t)
{
  if (t == NULL_TREE)
    return NULL_TREE;

  if (ARGUMENT_PACK_P (t))
    {
      tree args = ARGUMENT_PACK_ARGS (t);
      if (TREE_VEC_LENGTH (args) == 1
	  && PACK_EXPANSION_P (TREE_VEC_ELT (args, 0)))
	t = PACK_EXPANSION_PATTERN (TREE_VEC_ELT (args, 0));
    }

  if (REFERENCE_REF_P (t))
    t = TREE_OPERAND (t, 0);

  if (TEMPLATE_PARM_P (t))
    return t;
  else
    return NULL_TREE;
}

/* Given a single level of template parameters (a TREE_VEC), return it
   as a set of template arguments.  */

tree
template_parms_level_to_args (tree parms)
{
  parms = copy_node (parms);
  TREE_TYPE (parms) = NULL_TREE;
  for (tree& parm : tree_vec_range (parms))
    parm = template_parm_to_arg (parm);

  if (CHECKING_P)
    SET_NON_DEFAULT_TEMPLATE_ARGS_COUNT (parms, TREE_VEC_LENGTH (parms));

  return parms;
}

/* Given a set of template parameters, return them as a set of template
   arguments.  The template parameters are represented as a TREE_VEC, in
   the form documented in cp-tree.h for template arguments.  */

tree
template_parms_to_args (tree parms)
{
  tree header;
  tree args = NULL_TREE;
  int length = TMPL_PARMS_DEPTH (parms);
  int l = length;

  /* If there is only one level of template parameters, we do not
     create a TREE_VEC of TREE_VECs.  Instead, we return a single
     TREE_VEC containing the arguments.  */
  if (length > 1)
    args = make_tree_vec (length);

  for (header = parms; header; header = TREE_CHAIN (header))
    {
      tree a = template_parms_level_to_args (TREE_VALUE (header));

      if (length > 1)
	TREE_VEC_ELT (args, --l) = a;
      else
	args = a;
    }

  return args;
}

/* Within the declaration of a template, return the currently active
   template parameters as an argument TREE_VEC.  */

static tree
current_template_args (void)
{
  return template_parms_to_args (current_template_parms);
}

/* Return the fully generic arguments for of TMPL, i.e. what
   current_template_args would be while parsing it.  */

tree
generic_targs_for (tree tmpl)
{
  if (tmpl == NULL_TREE)
    return NULL_TREE;
  if (DECL_TEMPLATE_TEMPLATE_PARM_P (tmpl)
      || DECL_TEMPLATE_SPECIALIZATION (tmpl))
    /* DECL_TEMPLATE_RESULT doesn't have the arguments we want.  For a template
       template parameter, it has no TEMPLATE_INFO; for a partial
       specialization, it has the arguments for the primary template, and we
       want the arguments for the partial specialization.  */;
  else if (tree result = DECL_TEMPLATE_RESULT (tmpl))
    if (tree ti = get_template_info (result))
      return TI_ARGS (ti);
  return template_parms_to_args (DECL_TEMPLATE_PARMS (tmpl));
}

/* Return the template arguments corresponding to the template parameters of
   TMPL's enclosing scope.  When TMPL is a member of a partial specialization,
   this returns the arguments for the partial specialization as opposed to those
   for the primary template, which is the main difference between this function
   and simply using e.g. the TYPE_TI_ARGS of TMPL's DECL_CONTEXT.  */

tree
outer_template_args (tree tmpl)
{
  tree ti = get_template_info (DECL_TEMPLATE_RESULT (tmpl));
  if (!ti)
    return NULL_TREE;
  tree args = TI_ARGS (ti);
  if (!PRIMARY_TEMPLATE_P (tmpl))
    return args;
  if (TMPL_ARGS_DEPTH (args) == 1)
    return NULL_TREE;
  return strip_innermost_template_args (args, 1);
}

/* Update the declared TYPE by doing any lookups which were thought to be
   dependent, but are not now that we know the SCOPE of the declarator.  */

tree
maybe_update_decl_type (tree orig_type, tree scope)
{
  tree type = orig_type;

  if (type == NULL_TREE)
    return type;

  if (TREE_CODE (orig_type) == TYPE_DECL)
    type = TREE_TYPE (type);

  if (scope && TYPE_P (scope) && dependent_type_p (scope)
      && dependent_type_p (type)
      /* Don't bother building up the args in this case.  */
      && TREE_CODE (type) != TEMPLATE_TYPE_PARM)
    {
      /* tsubst in the args corresponding to the template parameters,
	 including auto if present.  Most things will be unchanged, but
	 make_typename_type and tsubst_qualified_id will resolve
	 TYPENAME_TYPEs and SCOPE_REFs that were previously dependent.  */
      tree args = current_template_args ();
      tree auto_node = type_uses_auto (type);
      tree pushed;
      if (auto_node)
	{
	  tree auto_vec = make_tree_vec (1);
	  TREE_VEC_ELT (auto_vec, 0) = auto_node;
	  args = add_to_template_args (args, auto_vec);
	}
      pushed = push_scope (scope);
      type = tsubst (type, args, tf_warning_or_error, NULL_TREE);
      if (pushed)
	pop_scope (scope);
    }

  if (type == error_mark_node)
    return orig_type;

  if (TREE_CODE (orig_type) == TYPE_DECL)
    {
      if (same_type_p (type, TREE_TYPE (orig_type)))
	type = orig_type;
      else
	type = TYPE_NAME (type);
    }
  return type;
}

/* Return a TEMPLATE_DECL corresponding to DECL, using the indicated
   template PARMS and constraints, CONSTR.  If MEMBER_TEMPLATE_P is true,
   the new  template is a member template. */

static tree
build_template_decl (tree decl, tree parms, bool member_template_p)
{
  gcc_checking_assert (TREE_CODE (decl) != TEMPLATE_DECL);

  tree tmpl = build_lang_decl (TEMPLATE_DECL, DECL_NAME (decl), NULL_TREE);
  SET_DECL_LANGUAGE (tmpl, DECL_LANGUAGE (decl));
  DECL_TEMPLATE_PARMS (tmpl) = parms;
  DECL_TEMPLATE_RESULT (tmpl) = decl;
  DECL_CONTEXT (tmpl) = DECL_CONTEXT (decl);
  TREE_TYPE (tmpl) = TREE_TYPE (decl);
  DECL_SOURCE_LOCATION (tmpl) = DECL_SOURCE_LOCATION (decl);
  DECL_MEMBER_TEMPLATE_P (tmpl) = member_template_p;

  /* Propagate module information from the decl.  */
  DECL_MODULE_EXPORT_P (tmpl) = DECL_MODULE_EXPORT_P (decl);

  return tmpl;
}

struct template_parm_data
{
  /* The level of the template parameters we are currently
     processing.  */
  int level;

  /* The index of the specialization argument we are currently
     processing.  */
  int current_arg;

  /* An array whose size is the number of template parameters.  The
     elements are nonzero if the parameter has been used in any one
     of the arguments processed so far.  */
  int* parms;

  /* An array whose size is the number of template arguments.  The
     elements are nonzero if the argument makes use of template
     parameters of this level.  */
  int* arg_uses_template_parms;
};

/* Subroutine of push_template_decl used to see if each template
   parameter in a partial specialization is used in the explicit
   argument list.  If T is of the LEVEL given in DATA (which is
   treated as a template_parm_data*), then DATA->PARMS is marked
   appropriately.  */

static int
mark_template_parm (tree t, void* data)
{
  int level;
  int idx;
  struct template_parm_data* tpd = (struct template_parm_data*) data;

  template_parm_level_and_index (t, &level, &idx);

  if (level == tpd->level)
    {
      tpd->parms[idx] = 1;
      tpd->arg_uses_template_parms[tpd->current_arg] = 1;
    }

  /* In C++17 the type of a non-type argument is a deduced context.  */
  if (cxx_dialect >= cxx17
      && TREE_CODE (t) == TEMPLATE_PARM_INDEX)
    for_each_template_parm (TREE_TYPE (t),
			    &mark_template_parm,
			    data,
			    NULL,
			    /*include_nondeduced_p=*/false);

  /* Return zero so that for_each_template_parm will continue the
     traversal of the tree; we want to mark *every* template parm.  */
  return 0;
}

/* Process the partial specialization DECL.  */

static tree
process_partial_specialization (tree decl)
{
  tree type = TREE_TYPE (decl);
  tree tinfo = get_template_info (decl);
  tree maintmpl = TI_TEMPLATE (tinfo);
  tree specargs = TI_ARGS (tinfo);
  tree inner_args = INNERMOST_TEMPLATE_ARGS (specargs);
  tree main_inner_parms = DECL_INNERMOST_TEMPLATE_PARMS (maintmpl);
  tree inner_parms;
  tree inst;
  int nargs = TREE_VEC_LENGTH (inner_args);
  int ntparms;
  int  i;
  bool did_error_intro = false;
  struct template_parm_data tpd;
  struct template_parm_data tpd2;

  gcc_assert (current_template_parms);

  /* A concept cannot be specialized.  */
  if (flag_concepts && variable_concept_p (maintmpl))
    {
      error ("specialization of variable concept %q#D", maintmpl);
      return error_mark_node;
    }

  inner_parms = INNERMOST_TEMPLATE_PARMS (current_template_parms);
  ntparms = TREE_VEC_LENGTH (inner_parms);

  /* We check that each of the template parameters given in the
     partial specialization is used in the argument list to the
     specialization.  For example:

       template <class T> struct S;
       template <class T> struct S<T*>;

     The second declaration is OK because `T*' uses the template
     parameter T, whereas

       template <class T> struct S<int>;

     is no good.  Even trickier is:

       template <class T>
       struct S1
       {
	  template <class U>
	  struct S2;
	  template <class U>
	  struct S2<T>;
       };

     The S2<T> declaration is actually invalid; it is a
     full-specialization.  Of course,

	  template <class U>
	  struct S2<T (*)(U)>;

     or some such would have been OK.  */
  tpd.level = TMPL_PARMS_DEPTH (current_template_parms);
  tpd.parms = XALLOCAVEC (int, ntparms);
  memset (tpd.parms, 0, sizeof (int) * ntparms);

  tpd.arg_uses_template_parms = XALLOCAVEC (int, nargs);
  memset (tpd.arg_uses_template_parms, 0, sizeof (int) * nargs);
  for (i = 0; i < nargs; ++i)
    {
      tpd.current_arg = i;
      for_each_template_parm (TREE_VEC_ELT (inner_args, i),
			      &mark_template_parm,
			      &tpd,
			      NULL,
			      /*include_nondeduced_p=*/false);
    }
  for (i = 0; i < ntparms; ++i)
    if (tpd.parms[i] == 0)
      {
	/* One of the template parms was not used in a deduced context in the
	   specialization.  */
	if (!did_error_intro)
	  {
	    error ("template parameters not deducible in "
		   "partial specialization:");
	    did_error_intro = true;
	  }

	inform (input_location, "        %qD",
		TREE_VALUE (TREE_VEC_ELT (inner_parms, i)));
      }

  if (did_error_intro)
    return error_mark_node;

  /* [temp.class.spec]

     The argument list of the specialization shall not be identical to
     the implicit argument list of the primary template.  */
  tree main_args
    = TI_ARGS (get_template_info (DECL_TEMPLATE_RESULT (maintmpl)));
  if (comp_template_args (inner_args, INNERMOST_TEMPLATE_ARGS (main_args))
      && (!flag_concepts
	  || !strictly_subsumes (current_template_constraints (), maintmpl)))
    {
      if (!flag_concepts)
        error ("partial specialization %q+D does not specialize "
	       "any template arguments; to define the primary template, "
	       "remove the template argument list", decl);
      else
        error ("partial specialization %q+D does not specialize any "
	       "template arguments and is not more constrained than "
	       "the primary template; to define the primary template, "
	       "remove the template argument list", decl);
      inform (DECL_SOURCE_LOCATION (maintmpl), "primary template here");
    }

  /* A partial specialization that replaces multiple parameters of the
     primary template with a pack expansion is less specialized for those
     parameters.  */
  if (nargs < DECL_NTPARMS (maintmpl))
    {
      error ("partial specialization is not more specialized than the "
	     "primary template because it replaces multiple parameters "
	     "with a pack expansion");
      inform (DECL_SOURCE_LOCATION (maintmpl), "primary template here");
      /* Avoid crash in process_partial_specialization.  */
      return decl;
    }

  else if (nargs > DECL_NTPARMS (maintmpl))
    {
      error ("too many arguments for partial specialization %qT", type);
      inform (DECL_SOURCE_LOCATION (maintmpl), "primary template here");
      /* Avoid crash below.  */
      return decl;
    }

  /* If we aren't in a dependent class, we can actually try deduction.  */
  else if (tpd.level == 1
	   /* FIXME we should be able to handle a partial specialization of a
	      partial instantiation, but currently we can't (c++/41727).  */
	   && TMPL_ARGS_DEPTH (specargs) == 1
	   && !get_partial_spec_bindings (maintmpl, maintmpl, specargs))
    {
      auto_diagnostic_group d;
      if (pedwarn (input_location, 0,
		   "partial specialization %qD is not more specialized than",
		   decl))
	inform (DECL_SOURCE_LOCATION (maintmpl), "primary template %qD",
		maintmpl);
    }

  /* [temp.spec.partial]

     The type of a template parameter corresponding to a specialized
     non-type argument shall not be dependent on a parameter of the
     specialization.

     Also, we verify that pack expansions only occur at the
     end of the argument list.  */
  tpd2.parms = 0;
  for (i = 0; i < nargs; ++i)
    {
      tree parm = TREE_VALUE (TREE_VEC_ELT (main_inner_parms, i));
      tree arg = TREE_VEC_ELT (inner_args, i);
      tree packed_args = NULL_TREE;
      int j, len = 1;

      if (ARGUMENT_PACK_P (arg))
        {
          /* Extract the arguments from the argument pack. We'll be
             iterating over these in the following loop.  */
          packed_args = ARGUMENT_PACK_ARGS (arg);
          len = TREE_VEC_LENGTH (packed_args);
        }

      for (j = 0; j < len; j++)
        {
          if (packed_args)
            /* Get the Jth argument in the parameter pack.  */
            arg = TREE_VEC_ELT (packed_args, j);

          if (PACK_EXPANSION_P (arg))
            {
              /* Pack expansions must come at the end of the
                 argument list.  */
              if ((packed_args && j < len - 1)
                  || (!packed_args && i < nargs - 1))
                {
                  if (TREE_CODE (arg) == EXPR_PACK_EXPANSION)
                    error ("parameter pack argument %qE must be at the "
			   "end of the template argument list", arg);
                  else
                    error ("parameter pack argument %qT must be at the "
			   "end of the template argument list", arg);
                }
            }

          if (TREE_CODE (arg) == EXPR_PACK_EXPANSION)
            /* We only care about the pattern.  */
            arg = PACK_EXPANSION_PATTERN (arg);

          if (/* These first two lines are the `non-type' bit.  */
              !TYPE_P (arg)
              && TREE_CODE (arg) != TEMPLATE_DECL
              /* This next two lines are the `argument expression is not just a
                 simple identifier' condition and also the `specialized
                 non-type argument' bit.  */
              && TREE_CODE (arg) != TEMPLATE_PARM_INDEX
	      && !((REFERENCE_REF_P (arg)
		    || TREE_CODE (arg) == VIEW_CONVERT_EXPR)
		   && TREE_CODE (TREE_OPERAND (arg, 0)) == TEMPLATE_PARM_INDEX))
            {
	      /* Look at the corresponding template parameter,
		 marking which template parameters its type depends
		 upon.  */
	      tree type = TREE_TYPE (parm);

	      if (!tpd2.parms)
		{
		  /* We haven't yet initialized TPD2.  Do so now.  */
		  tpd2.arg_uses_template_parms = XALLOCAVEC (int, nargs);
		  /* The number of parameters here is the number in the
		     main template, which, as checked in the assertion
		     above, is NARGS.  */
		  tpd2.parms = XALLOCAVEC (int, nargs);
		  tpd2.level =
		    TMPL_PARMS_DEPTH (DECL_TEMPLATE_PARMS (maintmpl));
		}

	      /* Mark the template parameters.  But this time, we're
		 looking for the template parameters of the main
		 template, not in the specialization.  */
	      tpd2.current_arg = i;
	      tpd2.arg_uses_template_parms[i] = 0;
	      memset (tpd2.parms, 0, sizeof (int) * nargs);
	      for_each_template_parm (type,
				      &mark_template_parm,
				      &tpd2,
				      NULL,
				      /*include_nondeduced_p=*/false);

	      if (tpd2.arg_uses_template_parms [i])
		{
		  /* The type depended on some template parameters.
		     If they are fully specialized in the
		     specialization, that's OK.  */
		  int j;
		  int count = 0;
		  for (j = 0; j < nargs; ++j)
		    if (tpd2.parms[j] != 0
			&& tpd.arg_uses_template_parms [j])
		      ++count;
		  if (count != 0)
		    error_n (input_location, count,
			     "type %qT of template argument %qE depends "
			     "on a template parameter",
			     "type %qT of template argument %qE depends "
			     "on template parameters",
			     type,
			     arg);
		}
            }
        }
    }

  /* We should only get here once.  */
  if (TREE_CODE (decl) == TYPE_DECL)
    gcc_assert (!COMPLETE_TYPE_P (type));

  // Build the template decl.
  tree tmpl = build_template_decl (decl, current_template_parms,
				   DECL_MEMBER_TEMPLATE_P (maintmpl));
  SET_DECL_TEMPLATE_SPECIALIZATION (tmpl);
  DECL_TEMPLATE_INFO (tmpl) = build_template_info (maintmpl, specargs);
  DECL_PRIMARY_TEMPLATE (tmpl) = maintmpl;

  /* Give template template parms a DECL_CONTEXT of the template
     for which they are a parameter.  */
  for (i = 0; i < ntparms; ++i)
    {
      tree parm = TREE_VALUE (TREE_VEC_ELT (inner_parms, i));
      if (TREE_CODE (parm) == TEMPLATE_DECL)
	DECL_CONTEXT (parm) = tmpl;
    }

  if (VAR_P (decl))
    /* We didn't register this in check_explicit_specialization so we could
       wait until the constraints were set.  */
    decl = register_specialization (decl, maintmpl, specargs, false, 0);
  else
    associate_classtype_constraints (type);

  DECL_TEMPLATE_SPECIALIZATIONS (maintmpl)
    = tree_cons (specargs, tmpl,
                 DECL_TEMPLATE_SPECIALIZATIONS (maintmpl));
  TREE_TYPE (DECL_TEMPLATE_SPECIALIZATIONS (maintmpl)) = type;
  /* Link the DECL_TEMPLATE_RESULT back to the partial TEMPLATE_DECL.  */
  gcc_checking_assert (!TI_PARTIAL_INFO (tinfo));
  TI_PARTIAL_INFO (tinfo) = build_template_info (tmpl, NULL_TREE);

  for (inst = DECL_TEMPLATE_INSTANTIATIONS (maintmpl); inst;
       inst = TREE_CHAIN (inst))
    {
      tree instance = TREE_VALUE (inst);
      if (TYPE_P (instance)
	  ? (COMPLETE_TYPE_P (instance)
	     && CLASSTYPE_IMPLICIT_INSTANTIATION (instance))
	  : DECL_TEMPLATE_INSTANTIATION (instance))
	{
	  tree partial_ti = most_specialized_partial_spec (instance, tf_none,
							   /*rechecking=*/true);
	  tree inst_decl = (DECL_P (instance)
			    ? instance : TYPE_NAME (instance));
	  if (!partial_ti)
	    /* OK */;
	  else if (partial_ti == error_mark_node)
	    permerror (input_location,
		       "declaration of %qD ambiguates earlier template "
		       "instantiation for %qD", decl, inst_decl);
	  else if (TI_TEMPLATE (partial_ti) == tmpl)
	    permerror (input_location,
		       "partial specialization of %qD after instantiation "
		       "of %qD", decl, inst_decl);
	}
    }

  return decl;
}

/* PARM is a template parameter of some form; return the corresponding
   TEMPLATE_PARM_INDEX.  */

static tree
get_template_parm_index (tree parm)
{
  if (TREE_CODE (parm) == PARM_DECL
      || TREE_CODE (parm) == CONST_DECL)
    parm = DECL_INITIAL (parm);
  else if (TREE_CODE (parm) == TYPE_DECL
	   || TREE_CODE (parm) == TEMPLATE_DECL)
    parm = TREE_TYPE (parm);
  if (TREE_CODE (parm) == TEMPLATE_TYPE_PARM
      || TREE_CODE (parm) == BOUND_TEMPLATE_TEMPLATE_PARM
      || TREE_CODE (parm) == TEMPLATE_TEMPLATE_PARM)
    parm = TEMPLATE_TYPE_PARM_INDEX (parm);
  gcc_assert (TREE_CODE (parm) == TEMPLATE_PARM_INDEX);
  return parm;
}

/* Subroutine of fixed_parameter_pack_p below.  Look for any template
   parameter packs used by the template parameter PARM.  */

static void
fixed_parameter_pack_p_1 (tree parm, struct find_parameter_pack_data *ppd)
{
  /* A type parm can't refer to another parm.  */
  if (TREE_CODE (parm) == TYPE_DECL || parm == error_mark_node)
    return;
  else if (TREE_CODE (parm) == PARM_DECL)
    {
      cp_walk_tree (&TREE_TYPE (parm), &find_parameter_packs_r,
		    ppd, ppd->visited);
      return;
    }

  gcc_assert (TREE_CODE (parm) == TEMPLATE_DECL);

  tree vec = INNERMOST_TEMPLATE_PARMS (DECL_TEMPLATE_PARMS (parm));
  for (int i = 0; i < TREE_VEC_LENGTH (vec); ++i)
    {
      tree p = TREE_VALUE (TREE_VEC_ELT (vec, i));
      if (template_parameter_pack_p (p))
	/* Any packs in the type are expanded by this parameter.  */;
      else
	fixed_parameter_pack_p_1 (p, ppd);
    }
}

/* PARM is a template parameter pack.  Return any parameter packs used in
   its type or the type of any of its template parameters.  If there are
   any such packs, it will be instantiated into a fixed template parameter
   list by partial instantiation rather than be fully deduced.  */

tree
fixed_parameter_pack_p (tree parm)
{
  /* This can only be true in a member template.  */
  if (TEMPLATE_PARM_ORIG_LEVEL (get_template_parm_index (parm)) < 2)
    return NULL_TREE;
  /* This can only be true for a parameter pack.  */
  if (!template_parameter_pack_p (parm))
    return NULL_TREE;
  /* A type parm can't refer to another parm.  */
  if (TREE_CODE (parm) == TYPE_DECL)
    return NULL_TREE;

  tree parameter_packs = NULL_TREE;
  struct find_parameter_pack_data ppd;
  ppd.parameter_packs = &parameter_packs;
  ppd.visited = new hash_set<tree>;
  ppd.type_pack_expansion_p = false;

  fixed_parameter_pack_p_1 (parm, &ppd);

  delete ppd.visited;
  return parameter_packs;
}

/* Check that a template declaration's use of default arguments and
   parameter packs is not invalid.  Here, PARMS are the template
   parameters.  IS_PRIMARY is true if DECL is the thing declared by
   a primary template.  IS_PARTIAL is true if DECL is a partial
   specialization.

   IS_FRIEND_DECL is nonzero if DECL is either a non-defining friend
   function template declaration or a friend class template
   declaration.  In the function case, 1 indicates a declaration, 2
   indicates a redeclaration.  When IS_FRIEND_DECL=2, no errors are
   emitted for extraneous default arguments.

   Returns TRUE if there were no errors found, FALSE otherwise. */

bool
check_default_tmpl_args (tree decl, tree parms, bool is_primary,
                         bool is_partial, int is_friend_decl)
{
  const char *msg;
  int last_level_to_check;
  tree parm_level;
  bool no_errors = true;

  /* [temp.param]

     A default template-argument shall not be specified in a
     function template declaration or a function template definition, nor
     in the template-parameter-list of the definition of a member of a
     class template.  */

  if (TREE_CODE (CP_DECL_CONTEXT (decl)) == FUNCTION_DECL
      || (TREE_CODE (decl) == FUNCTION_DECL && DECL_LOCAL_DECL_P (decl)))
    /* You can't have a function template declaration in a local
       scope, nor you can you define a member of a class template in a
       local scope.  */
    return true;

  if ((TREE_CODE (decl) == TYPE_DECL
       && TREE_TYPE (decl)
       && LAMBDA_TYPE_P (TREE_TYPE (decl)))
      || (TREE_CODE (decl) == FUNCTION_DECL
	  && LAMBDA_FUNCTION_P (decl)))
    /* A lambda doesn't have an explicit declaration; don't complain
       about the parms of the enclosing class.  */
    return true;

  if (current_class_type
      && !TYPE_BEING_DEFINED (current_class_type)
      && DECL_LANG_SPECIFIC (decl)
      && DECL_DECLARES_FUNCTION_P (decl)
      /* If this is either a friend defined in the scope of the class
	 or a member function.  */
      && (DECL_FUNCTION_MEMBER_P (decl)
	  ? same_type_p (DECL_CONTEXT (decl), current_class_type)
	  : DECL_FRIEND_CONTEXT (decl)
	  ? same_type_p (DECL_FRIEND_CONTEXT (decl), current_class_type)
	  : false)
      /* And, if it was a member function, it really was defined in
	 the scope of the class.  */
      && (!DECL_FUNCTION_MEMBER_P (decl)
	  || DECL_INITIALIZED_IN_CLASS_P (decl)))
    /* We already checked these parameters when the template was
       declared, so there's no need to do it again now.  This function
       was defined in class scope, but we're processing its body now
       that the class is complete.  */
    return true;

  /* Core issue 226 (C++0x only): the following only applies to class
     templates.  */
  if (is_primary
      && ((cxx_dialect == cxx98) || TREE_CODE (decl) != FUNCTION_DECL))
    {
      /* [temp.param]

         If a template-parameter has a default template-argument, all
         subsequent template-parameters shall have a default
         template-argument supplied.  */
      for (parm_level = parms; parm_level; parm_level = TREE_CHAIN (parm_level))
        {
          tree inner_parms = TREE_VALUE (parm_level);
          int ntparms = TREE_VEC_LENGTH (inner_parms);
          int seen_def_arg_p = 0;
          int i;

          for (i = 0; i < ntparms; ++i)
            {
              tree parm = TREE_VEC_ELT (inner_parms, i);

              if (parm == error_mark_node)
                continue;

              if (TREE_PURPOSE (parm))
                seen_def_arg_p = 1;
              else if (seen_def_arg_p
		       && !template_parameter_pack_p (TREE_VALUE (parm)))
                {
                  error ("no default argument for %qD", TREE_VALUE (parm));
                  /* For better subsequent error-recovery, we indicate that
                     there should have been a default argument.  */
                  TREE_PURPOSE (parm) = error_mark_node;
                  no_errors = false;
                }
	      else if (!is_partial
		       && !is_friend_decl
		       /* Don't complain about an enclosing partial
			  specialization.  */
		       && parm_level == parms
		       && (TREE_CODE (decl) == TYPE_DECL || VAR_P (decl))
		       && i < ntparms - 1
		       && template_parameter_pack_p (TREE_VALUE (parm))
		       /* A fixed parameter pack will be partially
			  instantiated into a fixed length list.  */
		       && !fixed_parameter_pack_p (TREE_VALUE (parm)))
		{
		  /* A primary class template, primary variable template
		     (DR 2032), or alias template can only have one
		     parameter pack, at the end of the template
		     parameter list.  */

		  error ("parameter pack %q+D must be at the end of the"
			 " template parameter list", TREE_VALUE (parm));

		  TREE_VALUE (TREE_VEC_ELT (inner_parms, i))
		    = error_mark_node;
		  no_errors = false;
		}
            }
        }
    }

  if (((cxx_dialect == cxx98) && TREE_CODE (decl) != TYPE_DECL)
      || is_partial
      || !is_primary
      || is_friend_decl)
    /* For an ordinary class template, default template arguments are
       allowed at the innermost level, e.g.:
	 template <class T = int>
	 struct S {};
       but, in a partial specialization, they're not allowed even
       there, as we have in [temp.class.spec]:

	 The template parameter list of a specialization shall not
	 contain default template argument values.

       So, for a partial specialization, or for a function template
       (in C++98/C++03), we look at all of them.  */
    ;
  else
    /* But, for a primary class template that is not a partial
       specialization we look at all template parameters except the
       innermost ones.  */
    parms = TREE_CHAIN (parms);

  /* Figure out what error message to issue.  */
  if (is_friend_decl == 2)
    msg = G_("default template arguments may not be used in function template "
	     "friend re-declaration");
  else if (is_friend_decl)
    msg = G_("default template arguments may not be used in template "
	     "friend declarations");
  else if (TREE_CODE (decl) == FUNCTION_DECL && (cxx_dialect == cxx98))
    msg = G_("default template arguments may not be used in function templates "
	     "without %<-std=c++11%> or %<-std=gnu++11%>");
  else if (is_partial)
    msg = G_("default template arguments may not be used in "
	     "partial specializations");
  else if (current_class_type && CLASSTYPE_IS_TEMPLATE (current_class_type))
    msg = G_("default argument for template parameter for class enclosing %qD");
  else
    /* Per [temp.param]/9, "A default template-argument shall not be
       specified in the template-parameter-lists of the definition of
       a member of a class template that appears outside of the member's
       class.", thus if we aren't handling a member of a class template
       there is no need to examine the parameters.  */
    return true;

  if (current_class_type && TYPE_BEING_DEFINED (current_class_type))
    /* If we're inside a class definition, there's no need to
       examine the parameters to the class itself.  On the one
       hand, they will be checked when the class is defined, and,
       on the other, default arguments are valid in things like:
	 template <class T = double>
	 struct S { template <class U> void f(U); };
       Here the default argument for `S' has no bearing on the
       declaration of `f'.  */
    last_level_to_check = template_class_depth (current_class_type) + 1;
  else
    /* Check everything.  */
    last_level_to_check = 0;

  for (parm_level = parms;
       parm_level && TMPL_PARMS_DEPTH (parm_level) >= last_level_to_check;
       parm_level = TREE_CHAIN (parm_level))
    {
      tree inner_parms = TREE_VALUE (parm_level);
      int i;
      int ntparms;

      ntparms = TREE_VEC_LENGTH (inner_parms);
      for (i = 0; i < ntparms; ++i)
        {
          if (TREE_VEC_ELT (inner_parms, i) == error_mark_node)
            continue;

	  if (TREE_PURPOSE (TREE_VEC_ELT (inner_parms, i)))
	    {
	      if (msg)
	        {
                  no_errors = false;
                  if (is_friend_decl == 2)
                    return no_errors;

		  error (msg, decl);
		  msg = 0;
	        }

	      /* Clear out the default argument so that we are not
	         confused later.  */
	      TREE_PURPOSE (TREE_VEC_ELT (inner_parms, i)) = NULL_TREE;
	    }
        }

      /* At this point, if we're still interested in issuing messages,
	 they must apply to classes surrounding the object declared.  */
      if (msg)
	msg = G_("default argument for template parameter for class "
		 "enclosing %qD");
    }

  return no_errors;
}

/* Worker for push_template_decl_real, called via
   for_each_template_parm.  DATA is really an int, indicating the
   level of the parameters we are interested in.  If T is a template
   parameter of that level, return nonzero.  */

static int
template_parm_this_level_p (tree t, void* data)
{
  int this_level = *(int *)data;
  int level;

  if (TREE_CODE (t) == TEMPLATE_PARM_INDEX)
    level = TEMPLATE_PARM_LEVEL (t);
  else
    level = TEMPLATE_TYPE_LEVEL (t);
  return level == this_level;
}

/* Worker for uses_outer_template_parms, called via for_each_template_parm.
   DATA is really an int, indicating the innermost outer level of parameters.
   If T is a template parameter of that level or further out, return
   nonzero.  */

static int
template_parm_outer_level (tree t, void *data)
{
  int this_level = *(int *)data;
  int level;

  if (TREE_CODE (t) == TEMPLATE_PARM_INDEX)
    level = TEMPLATE_PARM_LEVEL (t);
  else
    level = TEMPLATE_TYPE_LEVEL (t);
  return level <= this_level;
}

/* Creates a TEMPLATE_DECL for the indicated DECL using the template
   parameters given by current_template_args, or reuses a
   previously existing one, if appropriate.  Returns the DECL, or an
   equivalent one, if it is replaced via a call to duplicate_decls.

   If IS_FRIEND is true, DECL is a friend declaration.  */

tree
push_template_decl (tree decl, bool is_friend)
{
  if (decl == error_mark_node || !current_template_parms)
    return error_mark_node;

  /* See if this is a partial specialization.  */
  bool is_partial = ((DECL_IMPLICIT_TYPEDEF_P (decl)
		      && TREE_CODE (TREE_TYPE (decl)) != ENUMERAL_TYPE
		      && CLASSTYPE_TEMPLATE_SPECIALIZATION (TREE_TYPE (decl)))
		     || (VAR_P (decl)
			 && DECL_LANG_SPECIFIC (decl)
			 && DECL_TEMPLATE_SPECIALIZATION (decl)
			 && TINFO_USED_TEMPLATE_ID (DECL_TEMPLATE_INFO (decl))));

  /* No surprising friend functions.  */
  gcc_checking_assert (is_friend
		       || !(TREE_CODE (decl) == FUNCTION_DECL
			    && DECL_UNIQUE_FRIEND_P (decl)));

  tree ctx;
  if (is_friend)
    /* For a friend, we want the context of the friend, not
       the type of which it is a friend.  */
    ctx = CP_DECL_CONTEXT (decl);
  else if (CP_DECL_CONTEXT (decl)
	   && TREE_CODE (CP_DECL_CONTEXT (decl)) != NAMESPACE_DECL)
    /* In the case of a virtual function, we want the class in which
       it is defined.  */
    ctx = CP_DECL_CONTEXT (decl);
  else
    /* Otherwise, if we're currently defining some class, the DECL
       is assumed to be a member of the class.  */
    ctx = current_scope ();

  if (ctx && TREE_CODE (ctx) == NAMESPACE_DECL)
    ctx = NULL_TREE;

  if (!DECL_CONTEXT (decl))
    DECL_CONTEXT (decl) = FROB_CONTEXT (current_namespace);

  /* See if this is a primary template.  */
  bool is_primary = false;
  if (is_friend && ctx
      && uses_template_parms_level (ctx, current_template_depth))
    /* A friend template that specifies a class context, i.e.
         template <typename T> friend void A<T>::f();
       is not primary.  */
    ;
  else if (TREE_CODE (decl) == TYPE_DECL && LAMBDA_TYPE_P (TREE_TYPE (decl)))
    /* Lambdas are not primary.  */
    ;
  else
    is_primary = template_parm_scope_p ();

  /* True if the template is a member template, in the sense of
     [temp.mem].  */
  bool member_template_p = false;

  if (is_primary)
    {
      warning (OPT_Wtemplates, "template %qD declared", decl);

      if (DECL_CLASS_SCOPE_P (decl))
	member_template_p = true;

      if (TREE_CODE (decl) == TYPE_DECL
	  && IDENTIFIER_ANON_P (DECL_NAME (decl)))
	{
	  error ("template class without a name");
	  return error_mark_node;
	}
      else if (TREE_CODE (decl) == FUNCTION_DECL)
	{
	  if (member_template_p)
	    {
	      if (DECL_OVERRIDE_P (decl) || DECL_FINAL_P (decl))
		error ("member template %qD may not have virt-specifiers", decl);
	    }
	  if (DECL_DESTRUCTOR_P (decl))
	    {
	      /* [temp.mem]

		 A destructor shall not be a member template.  */
	      error_at (DECL_SOURCE_LOCATION (decl),
			"destructor %qD declared as member template", decl);
	      return error_mark_node;
	    }
	  if (IDENTIFIER_NEWDEL_OP_P (DECL_NAME (decl))
	      && (!prototype_p (TREE_TYPE (decl))
		  || TYPE_ARG_TYPES (TREE_TYPE (decl)) == void_list_node
		  || !TREE_CHAIN (TYPE_ARG_TYPES (TREE_TYPE (decl)))
		  || (TREE_CHAIN (TYPE_ARG_TYPES (TREE_TYPE (decl)))
		      == void_list_node)))
	    {
	      /* [basic.stc.dynamic.allocation]

		 An allocation function can be a function
		 template. ... Template allocation functions shall
		 have two or more parameters.  */
	      error ("invalid template declaration of %qD", decl);
	      return error_mark_node;
	    }
	}
      else if (DECL_IMPLICIT_TYPEDEF_P (decl)
	       && CLASS_TYPE_P (TREE_TYPE (decl)))
	/* Class template.  */;
      else if (TREE_CODE (decl) == TYPE_DECL
	       && TYPE_DECL_ALIAS_P (decl))
	/* alias-declaration */
	gcc_assert (!DECL_ARTIFICIAL (decl));
      else if (VAR_P (decl))
	/* C++14 variable template. */;
      else if (TREE_CODE (decl) == CONCEPT_DECL)
	/* C++20 concept definitions.  */;
      else
	{
	  error ("template declaration of %q#D", decl);
	  return error_mark_node;
	}
    }

  bool local_p = (!DECL_IMPLICIT_TYPEDEF_P (decl)
		  && ((ctx && TREE_CODE (ctx) == FUNCTION_DECL)
		      || (VAR_OR_FUNCTION_DECL_P (decl)
			  && DECL_LOCAL_DECL_P (decl))));

  /* Check to see that the rules regarding the use of default
     arguments are not being violated.  We check args for a friend
     functions when we know whether it's a definition, introducing
     declaration or re-declaration.  */
  if (!local_p && (!is_friend || TREE_CODE (decl) != FUNCTION_DECL))
    check_default_tmpl_args (decl, current_template_parms,
			     is_primary, is_partial, is_friend);

  /* Ensure that there are no parameter packs in the type of this
     declaration that have not been expanded.  */
  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      /* Check each of the arguments individually to see if there are
         any bare parameter packs.  */
      tree type = TREE_TYPE (decl);
      tree arg = DECL_ARGUMENTS (decl);
      tree argtype = TYPE_ARG_TYPES (type);

      while (arg && argtype)
        {
          if (!DECL_PACK_P (arg)
              && check_for_bare_parameter_packs (TREE_TYPE (arg)))
            {
            /* This is a PARM_DECL that contains unexpanded parameter
               packs. We have already complained about this in the
               check_for_bare_parameter_packs call, so just replace
               these types with ERROR_MARK_NODE.  */
              TREE_TYPE (arg) = error_mark_node;
              TREE_VALUE (argtype) = error_mark_node;
            }

          arg = DECL_CHAIN (arg);
          argtype = TREE_CHAIN (argtype);
        }

      /* Check for bare parameter packs in the return type and the
         exception specifiers.  */
      if (check_for_bare_parameter_packs (TREE_TYPE (type)))
	/* Errors were already issued, set return type to int
	   as the frontend doesn't expect error_mark_node as
	   the return type.  */
	TREE_TYPE (type) = integer_type_node;
      if (check_for_bare_parameter_packs (TYPE_RAISES_EXCEPTIONS (type)))
	TYPE_RAISES_EXCEPTIONS (type) = NULL_TREE;
    }
  else
    {
      if (check_for_bare_parameter_packs (is_typedef_decl (decl)
					  ? DECL_ORIGINAL_TYPE (decl)
					  : TREE_TYPE (decl)))
	{
	  TREE_TYPE (decl) = error_mark_node;
	  return error_mark_node;
	}

      if (is_partial && VAR_P (decl)
	  && check_for_bare_parameter_packs (DECL_TI_ARGS (decl)))
	return error_mark_node;
    }

  if (is_partial)
    return process_partial_specialization (decl);

  tree args = current_template_args ();
  tree tmpl = NULL_TREE;
  bool new_template_p = false;
  if (local_p)
    {
      /* Does not get a template head.  */
      tmpl = NULL_TREE;
      gcc_checking_assert (!is_primary);
    }
  else if (!ctx
	   || TREE_CODE (ctx) == FUNCTION_DECL
	   || (CLASS_TYPE_P (ctx) && TYPE_BEING_DEFINED (ctx))
	   || (TREE_CODE (decl) == TYPE_DECL && LAMBDA_TYPE_P (TREE_TYPE (decl)))
	   || (is_friend && !(DECL_LANG_SPECIFIC (decl)
			      && DECL_TEMPLATE_INFO (decl))))
    {
      if (DECL_LANG_SPECIFIC (decl)
	  && DECL_TEMPLATE_INFO (decl)
	  && DECL_TI_TEMPLATE (decl))
	tmpl = DECL_TI_TEMPLATE (decl);
      /* If DECL is a TYPE_DECL for a class-template, then there won't
	 be DECL_LANG_SPECIFIC.  The information equivalent to
	 DECL_TEMPLATE_INFO is found in TYPE_TEMPLATE_INFO instead.  */
      else if (DECL_IMPLICIT_TYPEDEF_P (decl)
	       && TYPE_TEMPLATE_INFO (TREE_TYPE (decl))
	       && TYPE_TI_TEMPLATE (TREE_TYPE (decl)))
	{
	  /* Since a template declaration already existed for this
	     class-type, we must be redeclaring it here.  Make sure
	     that the redeclaration is valid.  */
	  redeclare_class_template (TREE_TYPE (decl),
				    current_template_parms,
				    current_template_constraints ());
	  /* We don't need to create a new TEMPLATE_DECL; just use the
	     one we already had.  */
	  tmpl = TYPE_TI_TEMPLATE (TREE_TYPE (decl));
	}
      else
	{
	  tmpl = build_template_decl (decl, current_template_parms,
				      member_template_p);
	  new_template_p = true;

	  if (DECL_LANG_SPECIFIC (decl)
	      && DECL_TEMPLATE_SPECIALIZATION (decl))
	    {
	      /* A specialization of a member template of a template
		 class.  */
	      SET_DECL_TEMPLATE_SPECIALIZATION (tmpl);
	      DECL_TEMPLATE_INFO (tmpl) = DECL_TEMPLATE_INFO (decl);
	      DECL_TEMPLATE_INFO (decl) = NULL_TREE;
	    }
	}
    }
  else
    {
      tree a, t, current, parms;
      int i;
      tree tinfo = get_template_info (decl);

      if (!tinfo)
	{
	  error ("template definition of non-template %q#D", decl);
	  return error_mark_node;
	}

      tmpl = TI_TEMPLATE (tinfo);

      if (DECL_FUNCTION_TEMPLATE_P (tmpl)
	  && DECL_TEMPLATE_INFO (decl) && DECL_TI_ARGS (decl)
	  && DECL_TEMPLATE_SPECIALIZATION (decl)
	  && DECL_MEMBER_TEMPLATE_P (tmpl))
	{
	  /* The declaration is a specialization of a member
	     template, declared outside the class.  Therefore, the
	     innermost template arguments will be NULL, so we
	     replace them with the arguments determined by the
	     earlier call to check_explicit_specialization.  */
	  args = DECL_TI_ARGS (decl);

	  tree new_tmpl
	    = build_template_decl (decl, current_template_parms,
				   member_template_p);
	  DECL_TI_TEMPLATE (decl) = new_tmpl;
	  SET_DECL_TEMPLATE_SPECIALIZATION (new_tmpl);
	  DECL_TEMPLATE_INFO (new_tmpl)
	    = build_template_info (tmpl, args);

	  register_specialization (new_tmpl,
				   most_general_template (tmpl),
				   args,
				   is_friend, 0);
	  return decl;
	}

      /* Make sure the template headers we got make sense.  */

      parms = DECL_TEMPLATE_PARMS (tmpl);
      i = TMPL_PARMS_DEPTH (parms);
      if (TMPL_ARGS_DEPTH (args) != i)
	{
	  error ("expected %d levels of template parms for %q#D, got %d",
		 i, decl, TMPL_ARGS_DEPTH (args));
	  DECL_INTERFACE_KNOWN (decl) = 1;
	  return error_mark_node;
	}
      else
	for (current = decl; i > 0; --i, parms = TREE_CHAIN (parms))
	  {
	    a = TMPL_ARGS_LEVEL (args, i);
	    t = INNERMOST_TEMPLATE_PARMS (parms);

	    if (TREE_VEC_LENGTH (t) != TREE_VEC_LENGTH (a))
	      {
		if (current == decl)
		  error ("got %d template parameters for %q#D",
			 TREE_VEC_LENGTH (a), decl);
		else
		  error ("got %d template parameters for %q#T",
			 TREE_VEC_LENGTH (a), current);
		error ("  but %d required", TREE_VEC_LENGTH (t));
		/* Avoid crash in import_export_decl.  */
		DECL_INTERFACE_KNOWN (decl) = 1;
		return error_mark_node;
	      }

	    if (current == decl)
	      current = ctx;
	    else if (current == NULL_TREE)
	      /* Can happen in erroneous input.  */
	      break;
	    else
	      current = get_containing_scope (current);
	  }

      /* Check that the parms are used in the appropriate qualifying scopes
	 in the declarator.  */
      if (!comp_template_args
	  (TI_ARGS (tinfo),
	   TI_ARGS (get_template_info (DECL_TEMPLATE_RESULT (tmpl)))))
	{
	  error ("template arguments to %qD do not match original "
		 "template %qD", decl, DECL_TEMPLATE_RESULT (tmpl));
	  if (!uses_template_parms (TI_ARGS (tinfo)))
	    inform (input_location, "use %<template<>%> for"
		    " an explicit specialization");
	  /* Avoid crash in import_export_decl.  */
	  DECL_INTERFACE_KNOWN (decl) = 1;
	  return error_mark_node;
	}

      /* Check that the constraints for each enclosing template scope are
	 consistent with the original declarations.  */
      if (flag_concepts)
	{
	  tree decl_parms = DECL_TEMPLATE_PARMS (tmpl);
	  tree scope_parms = current_template_parms;
	  if (PRIMARY_TEMPLATE_P (tmpl))
	    {
	      decl_parms = TREE_CHAIN (decl_parms);
	      scope_parms = TREE_CHAIN (scope_parms);
	    }
	  while (decl_parms)
	    {
	      if (!template_requirements_equivalent_p (decl_parms, scope_parms))
		{
		  error ("redeclaration of %qD with different constraints",
			 TPARMS_PRIMARY_TEMPLATE (TREE_VALUE (decl_parms)));
		  break;
		}
	      decl_parms = TREE_CHAIN (decl_parms);
	      scope_parms = TREE_CHAIN (scope_parms);
	    }
	}
    }

  gcc_checking_assert (!tmpl || DECL_TEMPLATE_RESULT (tmpl) == decl);

  if (new_template_p)
    {
      /* Push template declarations for global functions and types.
	 Note that we do not try to push a global template friend
	 declared in a template class; such a thing may well depend on
	 the template parameters of the class and we'll push it when
	 instantiating the befriending class.  */
      if (!ctx
	  && !(is_friend && template_class_depth (current_class_type) > 0))
	{
	  tree pushed = pushdecl_namespace_level (tmpl, /*hiding=*/is_friend);
	  if (pushed == error_mark_node)
	    return error_mark_node;

	  /* pushdecl may have found an existing template.  */
	  if (pushed != tmpl)
	    {
	      decl = DECL_TEMPLATE_RESULT (pushed);
	      tmpl = NULL_TREE;
	    }
	}
      else if (is_friend)
	{
	  /* Record this decl as belonging to the current class.  It's
	     not chained onto anything else.  */
	  DECL_UNINSTANTIATED_TEMPLATE_FRIEND_P (tmpl) = true;
	  gcc_checking_assert (!DECL_CHAIN (tmpl));
	  DECL_CHAIN (tmpl) = current_scope ();
	}
    }
  else if (tmpl)
    /* The type may have been completed, or (erroneously) changed.  */
    TREE_TYPE (tmpl) = TREE_TYPE (decl);

  if (tmpl)
    {
      if (is_primary)
	{
	  tree parms = DECL_TEMPLATE_PARMS (tmpl);

	  DECL_PRIMARY_TEMPLATE (tmpl) = tmpl;

	  /* Give template template parms a DECL_CONTEXT of the template
	     for which they are a parameter.  */
	  parms = INNERMOST_TEMPLATE_PARMS (parms);
	  for (int i = TREE_VEC_LENGTH (parms) - 1; i >= 0; --i)
	    {
	      tree parm = TREE_VALUE (TREE_VEC_ELT (parms, i));
	      if (TREE_CODE (parm) == TEMPLATE_DECL)
		DECL_CONTEXT (parm) = tmpl;
	    }

	  if (TREE_CODE (decl) == TYPE_DECL
	      && TYPE_DECL_ALIAS_P (decl))
	    {
	      if (tree constr
		  = TEMPLATE_PARMS_CONSTRAINTS (DECL_TEMPLATE_PARMS (tmpl)))
		{
		  /* ??? Why don't we do this here for all templates?  */
		  constr = build_constraints (constr, NULL_TREE);
		  set_constraints (decl, constr);
		}
	      if (complex_alias_template_p (tmpl))
		TEMPLATE_DECL_COMPLEX_ALIAS_P (tmpl) = true;
	    }
	}

      /* The DECL_TI_ARGS of DECL contains full set of arguments
	 referring wback to its most general template.  If TMPL is a
	 specialization, ARGS may only have the innermost set of
	 arguments.  Add the missing argument levels if necessary.  */
      if (DECL_TEMPLATE_INFO (tmpl))
	args = add_outermost_template_args (DECL_TI_ARGS (tmpl), args);

      tree info = build_template_info (tmpl, args);

      if (DECL_IMPLICIT_TYPEDEF_P (decl))
	SET_TYPE_TEMPLATE_INFO (TREE_TYPE (tmpl), info);
      else
	{
	  retrofit_lang_decl (decl);
	  DECL_TEMPLATE_INFO (decl) = info;
	}
    }

  if (flag_implicit_templates
      && !is_friend
      && TREE_PUBLIC (decl)
      && VAR_OR_FUNCTION_DECL_P (decl))
    /* Set DECL_COMDAT on template instantiations; if we force
       them to be emitted by explicit instantiation,
       mark_needed will tell cgraph to do the right thing.  */
    DECL_COMDAT (decl) = true;

  gcc_checking_assert (!tmpl || DECL_TEMPLATE_RESULT (tmpl) == decl);

  return decl;
}

/* FN is an inheriting constructor that inherits from the constructor
   template INHERITED; turn FN into a constructor template with a matching
   template header.  */

tree
add_inherited_template_parms (tree fn, tree inherited)
{
  tree inner_parms
    = INNERMOST_TEMPLATE_PARMS (DECL_TEMPLATE_PARMS (inherited));
  inner_parms = copy_node (inner_parms);
  tree parms
    = tree_cons (size_int (current_template_depth + 1),
		 inner_parms, current_template_parms);
  tree tmpl = build_template_decl (fn, parms, /*member*/true);
  tree args = template_parms_to_args (parms);
  DECL_TEMPLATE_INFO (fn) = build_template_info (tmpl, args);
  DECL_ARTIFICIAL (tmpl) = true;
  DECL_PRIMARY_TEMPLATE (tmpl) = tmpl;
  return tmpl;
}

/* Called when a class template TYPE is redeclared with the indicated
   template PARMS, e.g.:

     template <class T> struct S;
     template <class T> struct S {};  */

bool
redeclare_class_template (tree type, tree parms, tree cons)
{
  tree tmpl;
  tree tmpl_parms;
  int i;

  if (!TYPE_TEMPLATE_INFO (type))
    {
      error ("%qT is not a template type", type);
      return false;
    }

  tmpl = TYPE_TI_TEMPLATE (type);
  if (!PRIMARY_TEMPLATE_P (tmpl))
    /* The type is nested in some template class.  Nothing to worry
       about here; there are no new template parameters for the nested
       type.  */
    return true;

  if (!parms)
    {
      error ("template specifiers not specified in declaration of %qD",
	     tmpl);
      return false;
    }

  parms = INNERMOST_TEMPLATE_PARMS (parms);
  tmpl_parms = DECL_INNERMOST_TEMPLATE_PARMS (tmpl);

  if (TREE_VEC_LENGTH (parms) != TREE_VEC_LENGTH (tmpl_parms))
    {
      error_n (input_location, TREE_VEC_LENGTH (parms),
               "redeclared with %d template parameter",
               "redeclared with %d template parameters",
               TREE_VEC_LENGTH (parms));
      inform_n (DECL_SOURCE_LOCATION (tmpl), TREE_VEC_LENGTH (tmpl_parms),
                "previous declaration %qD used %d template parameter",
                "previous declaration %qD used %d template parameters",
                tmpl, TREE_VEC_LENGTH (tmpl_parms));
      return false;
    }

  for (i = 0; i < TREE_VEC_LENGTH (tmpl_parms); ++i)
    {
      tree tmpl_parm;
      tree parm;

      if (TREE_VEC_ELT (tmpl_parms, i) == error_mark_node
          || TREE_VEC_ELT (parms, i) == error_mark_node)
        continue;

      tmpl_parm = TREE_VALUE (TREE_VEC_ELT (tmpl_parms, i));
      if (error_operand_p (tmpl_parm))
	return false;

      parm = TREE_VALUE (TREE_VEC_ELT (parms, i));

      /* TMPL_PARM and PARM can be either TYPE_DECL, PARM_DECL, or
	 TEMPLATE_DECL.  */
      if (TREE_CODE (tmpl_parm) != TREE_CODE (parm)
	  || (TREE_CODE (tmpl_parm) != TYPE_DECL
	      && !same_type_p (TREE_TYPE (tmpl_parm), TREE_TYPE (parm)))
	  || (TREE_CODE (tmpl_parm) != PARM_DECL
	      && (TEMPLATE_TYPE_PARAMETER_PACK (TREE_TYPE (tmpl_parm))
		  != TEMPLATE_TYPE_PARAMETER_PACK (TREE_TYPE (parm))))
	  || (TREE_CODE (tmpl_parm) == PARM_DECL
	      && (TEMPLATE_PARM_PARAMETER_PACK (DECL_INITIAL (tmpl_parm))
		  != TEMPLATE_PARM_PARAMETER_PACK (DECL_INITIAL (parm)))))
	{
	  auto_diagnostic_group d;
	  error ("template parameter %q+#D", tmpl_parm);
	  if (DECL_P (parm))
	    inform (DECL_SOURCE_LOCATION (parm), "redeclared here as %q#D", parm);
	  else
	    inform (input_location, "redeclared here");
	  return false;
	}

      /* The parameters can be declared to introduce different
	 constraints.  */
      tree p1 = TREE_VEC_ELT (tmpl_parms, i);
      tree p2 = TREE_VEC_ELT (parms, i);
      if (!template_parameter_constraints_equivalent_p (p1, p2))
	{
	  auto_diagnostic_group d;
	  error ("declaration of template parameter %q+#D with different "
		 "constraints", parm);
	  inform (DECL_SOURCE_LOCATION (tmpl_parm),
		  "original declaration appeared here");
	  return false;
	}

      /* Give each template template parm in this redeclaration a
	 DECL_CONTEXT of the template for which they are a parameter.  */
      if (TREE_CODE (parm) == TEMPLATE_DECL)
	{
	  gcc_checking_assert (DECL_CONTEXT (parm) == NULL_TREE
			       || DECL_CONTEXT (parm) == tmpl);
	  DECL_CONTEXT (parm) = tmpl;
	}
    }

  if (!merge_default_template_args (parms, tmpl_parms, /*class_p=*/true))
    return false;

  tree ci = get_constraints (tmpl);
  tree req1 = ci ? CI_TEMPLATE_REQS (ci) : NULL_TREE;
  tree req2 = cons ? CI_TEMPLATE_REQS (cons) : NULL_TREE;

  /* Two classes with different constraints declare different entities.  */
  if (!cp_tree_equal (req1, req2))
    {
      auto_diagnostic_group d;
      error_at (input_location, "redeclaration of %q#D with different "
                                "constraints", tmpl);
      inform (DECL_SOURCE_LOCATION (tmpl),
              "original declaration appeared here");
      return false;
    }

    return true;
}

/* The actual substitution part of instantiate_non_dependent_expr,
   to be used when the caller has already checked
    !instantiation_dependent_uneval_expression_p (expr)
   and cleared processing_template_decl.  */

tree
instantiate_non_dependent_expr_internal (tree expr, tsubst_flags_t complain)
{
  return tsubst_copy_and_build (expr,
				/*args=*/NULL_TREE,
				complain,
				/*in_decl=*/NULL_TREE);
}

/* Instantiate the non-dependent expression EXPR.  */

tree
instantiate_non_dependent_expr (tree expr,
				tsubst_flags_t complain /* = tf_error */)
{
  if (expr == NULL_TREE)
    return NULL_TREE;

  if (processing_template_decl)
    {
      /* The caller should have checked this already.  */
      gcc_checking_assert (!instantiation_dependent_uneval_expression_p (expr));
      processing_template_decl_sentinel s;
      expr = instantiate_non_dependent_expr_internal (expr, complain);
    }
  return expr;
}

/* Like instantiate_non_dependent_expr, but return NULL_TREE if the
   expression is dependent or non-constant.  */

tree
instantiate_non_dependent_or_null (tree expr)
{
  if (expr == NULL_TREE)
    return NULL_TREE;
  if (processing_template_decl)
    {
      if (!is_nondependent_constant_expression (expr))
	expr = NULL_TREE;
      else
	{
	  processing_template_decl_sentinel s;
	  expr = instantiate_non_dependent_expr_internal (expr, tf_error);
	}
    }
  return expr;
}

/* True iff T is a specialization of a variable template.  */

bool
variable_template_specialization_p (tree t)
{
  if (!VAR_P (t) || !DECL_LANG_SPECIFIC (t) || !DECL_TEMPLATE_INFO (t))
    return false;
  tree tmpl = DECL_TI_TEMPLATE (t);
  return variable_template_p (tmpl);
}

/* Return TRUE iff T is a type alias, a TEMPLATE_DECL for an alias
   template declaration, or a TYPE_DECL for an alias declaration.  */

bool
alias_type_or_template_p (tree t)
{
  if (t == NULL_TREE)
    return false;
  return ((TREE_CODE (t) == TYPE_DECL && TYPE_DECL_ALIAS_P (t))
	  || (TYPE_P (t)
	      && TYPE_NAME (t)
	      && TYPE_DECL_ALIAS_P (TYPE_NAME (t)))
	  || DECL_ALIAS_TEMPLATE_P (t));
}

/* If T is a specialization of an alias template, return it; otherwise return
   NULL_TREE.  If TRANSPARENT_TYPEDEFS is true, look through other aliases.  */

tree
alias_template_specialization_p (const_tree t,
				 bool transparent_typedefs)
{
  if (!TYPE_P (t))
    return NULL_TREE;

  /* It's an alias template specialization if it's an alias and its
     TYPE_NAME is a specialization of a primary template.  */
  if (typedef_variant_p (t))
    {
      if (tree tinfo = TYPE_ALIAS_TEMPLATE_INFO (t))
	if (PRIMARY_TEMPLATE_P (TI_TEMPLATE (tinfo)))
	  return CONST_CAST_TREE (t);
      if (transparent_typedefs)
	return alias_template_specialization_p (DECL_ORIGINAL_TYPE
						(TYPE_NAME (t)),
						transparent_typedefs);
    }

  return NULL_TREE;
}

/* Data structure for complex_alias_template_*.  */

struct uses_all_template_parms_data
{
  int level;
  bool *seen;
};

/* walk_tree callback for complex_alias_template_p.  */

static tree
complex_alias_template_r (tree *tp, int *walk_subtrees, void *data_)
{
  tree t = *tp;
  auto &data = *(struct uses_all_template_parms_data*)data_;

  switch (TREE_CODE (t))
    {
    case TEMPLATE_TYPE_PARM:
    case TEMPLATE_PARM_INDEX:
    case TEMPLATE_TEMPLATE_PARM:
    case BOUND_TEMPLATE_TEMPLATE_PARM:
      {
	tree idx = get_template_parm_index (t);
	if (TEMPLATE_PARM_LEVEL (idx) == data.level)
	  data.seen[TEMPLATE_PARM_IDX (idx)] = true;
      }

    default:;
    }

  if (!PACK_EXPANSION_P (t))
    return 0;

  /* An alias template with a pack expansion that expands a pack from the
     enclosing class needs to be considered complex, to avoid confusion with
     the same pack being used as an argument to the alias's own template
     parameter (91966).  */
  for (tree pack = PACK_EXPANSION_PARAMETER_PACKS (t); pack;
       pack = TREE_CHAIN (pack))
    {
      tree parm_pack = TREE_VALUE (pack);
      if (!TEMPLATE_PARM_P (parm_pack))
	continue;
      int idx, level;
      template_parm_level_and_index (parm_pack, &level, &idx);
      if (level < data.level)
	return t;

      /* Consider the expanded packs to be used outside the expansion...  */
      data.seen[idx] = true;
    }

  /* ...but don't walk into the pattern.  Consider PR104008:

     template <typename T, typename... Ts>
     using IsOneOf = disjunction<is_same<T, Ts>...>;

     where IsOneOf seemingly uses all of its template parameters in its
     expansion (and does not expand a pack from the enclosing class), so the
     alias was not marked as complex.  However, if it is used like
     "IsOneOf<T>", the empty pack for Ts means that T no longer appears in the
     expansion.  So only Ts is considered used by the pack expansion.  */
  *walk_subtrees = false;

  return 0;
}

/* An alias template is complex from a SFINAE perspective if a template-id
   using that alias can be ill-formed when the expansion is not, as with
   the void_t template.

   Returns 1 if always complex, 0 if not complex, -1 if complex iff any of the
   template arguments are empty packs.  */

static bool
complex_alias_template_p (const_tree tmpl)
{
  /* A renaming alias isn't complex.  */
  if (get_underlying_template (CONST_CAST_TREE (tmpl)) != tmpl)
    return false;

  /* Any other constrained alias is complex.  */
  if (get_constraints (tmpl))
    return true;

  struct uses_all_template_parms_data data;
  tree pat = DECL_ORIGINAL_TYPE (DECL_TEMPLATE_RESULT (tmpl));
  tree parms = DECL_TEMPLATE_PARMS (tmpl);
  data.level = TMPL_PARMS_DEPTH (parms);
  int len = TREE_VEC_LENGTH (INNERMOST_TEMPLATE_PARMS (parms));
  data.seen = XALLOCAVEC (bool, len);
  for (int i = 0; i < len; ++i)
    data.seen[i] = false;

  if (cp_walk_tree_without_duplicates (&pat, complex_alias_template_r, &data))
    return true;
  for (int i = 0; i < len; ++i)
    if (!data.seen[i])
      return true;
  return false;
}

/* If T is a specialization of a complex alias template with dependent
   template-arguments, return it; otherwise return NULL_TREE.  If T is a
   typedef to such a specialization, return the specialization.  */

tree
dependent_alias_template_spec_p (const_tree t, bool transparent_typedefs)
{
  if (t == error_mark_node)
    return NULL_TREE;
  gcc_assert (TYPE_P (t));

  if (!typedef_variant_p (t))
    return NULL_TREE;

  tree tinfo = TYPE_ALIAS_TEMPLATE_INFO (t);
  if (tinfo
      && TEMPLATE_DECL_COMPLEX_ALIAS_P (TI_TEMPLATE (tinfo))
      && (any_dependent_template_arguments_p
	  (INNERMOST_TEMPLATE_ARGS (TI_ARGS (tinfo)))))
    return CONST_CAST_TREE (t);

  if (transparent_typedefs)
    {
      tree utype = DECL_ORIGINAL_TYPE (TYPE_NAME (t));
      return dependent_alias_template_spec_p (utype, transparent_typedefs);
    }

  return NULL_TREE;
}

/* Return the number of innermost template parameters in TMPL.  */

static int
num_innermost_template_parms (const_tree tmpl)
{
  tree parms = INNERMOST_TEMPLATE_PARMS (DECL_TEMPLATE_PARMS (tmpl));
  return TREE_VEC_LENGTH (parms);
}

/* Return either TMPL or another template that it is equivalent to under DR
   1286: An alias that just changes the name of a template is equivalent to
   the other template.  */

static tree
get_underlying_template (tree tmpl)
{
  gcc_assert (TREE_CODE (tmpl) == TEMPLATE_DECL);
  while (DECL_ALIAS_TEMPLATE_P (tmpl))
    {
      /* Determine if the alias is equivalent to an underlying template.  */
      tree orig_type = DECL_ORIGINAL_TYPE (DECL_TEMPLATE_RESULT (tmpl));
      /* The underlying type may have been ill-formed. Don't proceed.  */
      if (!orig_type)
	break;
      tree tinfo = TYPE_TEMPLATE_INFO_MAYBE_ALIAS (orig_type);
      if (!tinfo)
	break;

      tree underlying = TI_TEMPLATE (tinfo);
      if (!PRIMARY_TEMPLATE_P (underlying)
	  || (num_innermost_template_parms (tmpl)
	      != num_innermost_template_parms (underlying)))
	break;

      /* Does the alias add cv-quals?  */
      if (TYPE_QUALS (TREE_TYPE (underlying)) != TYPE_QUALS (TREE_TYPE (tmpl)))
	break;

      tree alias_args = INNERMOST_TEMPLATE_ARGS (generic_targs_for (tmpl));
      if (!comp_template_args (TI_ARGS (tinfo), alias_args))
	break;

      /* Are any default template arguments equivalent?  */
      tree aparms = INNERMOST_TEMPLATE_PARMS (DECL_TEMPLATE_PARMS (tmpl));
      tree uparms = INNERMOST_TEMPLATE_PARMS (DECL_TEMPLATE_PARMS (underlying));
      const int nparms = TREE_VEC_LENGTH (aparms);
      for (int i = 0; i < nparms; ++i)
	{
	  tree adefarg = TREE_PURPOSE (TREE_VEC_ELT (aparms, i));
	  tree udefarg = TREE_PURPOSE (TREE_VEC_ELT (uparms, i));
	  if (!template_args_equal (adefarg, udefarg))
	    goto top_break;
	}

      /* If TMPL adds or changes any constraints, it isn't equivalent.  I think
	 it's appropriate to treat a less-constrained alias as equivalent.  */
      if (!at_least_as_constrained (underlying, tmpl))
	break;

      /* Alias is equivalent.  Strip it and repeat.  */
      tmpl = underlying;
    }
  top_break:;

  return tmpl;
}

/* Subroutine of convert_nontype_argument. Converts EXPR to TYPE, which
   must be a reference-to-function or a pointer-to-function type, as specified
   in [temp.arg.nontype]: disambiguate EXPR if it is an overload set,
   and check that the resulting function has external linkage.  */

static tree
convert_nontype_argument_function (tree type, tree expr,
				   tsubst_flags_t complain)
{
  tree fns = expr;
  tree fn, fn_no_ptr;
  linkage_kind linkage;

  fn = instantiate_type (type, fns, tf_none);
  if (fn == error_mark_node)
    return error_mark_node;

  if (value_dependent_expression_p (fn))
    goto accept;

  fn_no_ptr = fn;
  if (REFERENCE_REF_P (fn_no_ptr))
    fn_no_ptr = TREE_OPERAND (fn_no_ptr, 0);
  fn_no_ptr = strip_fnptr_conv (fn_no_ptr);
  if (TREE_CODE (fn_no_ptr) == ADDR_EXPR)
    fn_no_ptr = TREE_OPERAND (fn_no_ptr, 0);
  if (BASELINK_P (fn_no_ptr))
    fn_no_ptr = BASELINK_FUNCTIONS (fn_no_ptr);

  /* [temp.arg.nontype]/1

     A template-argument for a non-type, non-template template-parameter
     shall be one of:
     [...]
     -- the address of an object or function with external [C++11: or
        internal] linkage.  */

  STRIP_ANY_LOCATION_WRAPPER (fn_no_ptr);
  if (TREE_CODE (fn_no_ptr) != FUNCTION_DECL)
    {
      if (complain & tf_error)
	{
	  location_t loc = cp_expr_loc_or_input_loc (expr);
	  error_at (loc, "%qE is not a valid template argument for type %qT",
		    expr, type);
	  if (TYPE_PTR_P (type))
	    inform (loc, "it must be the address of a function "
		    "with external linkage");
	  else
	    inform (loc, "it must be the name of a function with "
		    "external linkage");
	}
      return NULL_TREE;
    }

  linkage = decl_linkage (fn_no_ptr);
  if ((cxx_dialect < cxx11 && linkage != lk_external)
      || (cxx_dialect < cxx17 && linkage == lk_none))
    {
      if (complain & tf_error)
	{
	  location_t loc = cp_expr_loc_or_input_loc (expr);
	  if (cxx_dialect >= cxx11)
	    error_at (loc, "%qE is not a valid template argument for type "
		      "%qT because %qD has no linkage",
		      expr, type, fn_no_ptr);
	  else
	    error_at (loc, "%qE is not a valid template argument for type "
		      "%qT because %qD does not have external linkage",
		      expr, type, fn_no_ptr);
	}
      return NULL_TREE;
    }

 accept:
  if (TYPE_REF_P (type))
    {
      if (REFERENCE_REF_P (fn))
	fn = TREE_OPERAND (fn, 0);
      else
	fn = build_address (fn);
    }
  if (!same_type_ignoring_top_level_qualifiers_p (type, TREE_TYPE (fn)))
    fn = build_nop (type, fn);

  return fn;
}

/* Subroutine of convert_nontype_argument.
   Check if EXPR of type TYPE is a valid pointer-to-member constant.
   Emit an error otherwise.  */

static bool
check_valid_ptrmem_cst_expr (tree type, tree expr,
			     tsubst_flags_t complain)
{
  tree orig_expr = expr;
  STRIP_NOPS (expr);
  if (null_ptr_cst_p (expr))
    return true;
  if (TREE_CODE (expr) == PTRMEM_CST
      && same_type_p (TYPE_PTRMEM_CLASS_TYPE (type),
		      PTRMEM_CST_CLASS (expr)))
    return true;
  if (cxx_dialect >= cxx11 && null_member_pointer_value_p (expr))
    return true;
  if (processing_template_decl
      && TREE_CODE (expr) == ADDR_EXPR
      && TREE_CODE (TREE_OPERAND (expr, 0)) == OFFSET_REF)
    return true;
  if (complain & tf_error)
    {
      location_t loc = cp_expr_loc_or_input_loc (orig_expr);
      error_at (loc, "%qE is not a valid template argument for type %qT",
		orig_expr, type);
      if (TREE_CODE (expr) != PTRMEM_CST)
	inform (loc, "it must be a pointer-to-member of the form %<&X::Y%>");
      else
	inform (loc, "because it is a member of %qT", PTRMEM_CST_CLASS (expr));
    }
  return false;
}

/* Returns TRUE iff the address of OP is value-dependent.

   14.6.2.4 [temp.dep.temp]:
   A non-integral non-type template-argument is dependent if its type is
   dependent or it has either of the following forms
     qualified-id
     & qualified-id
   and contains a nested-name-specifier which specifies a class-name that
   names a dependent type.

   We generalize this to just say that the address of a member of a
   dependent class is value-dependent; the above doesn't cover the
   address of a static data member named with an unqualified-id.  */

static bool
has_value_dependent_address (tree op)
{
  STRIP_ANY_LOCATION_WRAPPER (op);

  /* We could use get_inner_reference here, but there's no need;
     this is only relevant for template non-type arguments, which
     can only be expressed as &id-expression.  */
  if (DECL_P (op))
    {
      tree ctx = CP_DECL_CONTEXT (op);

      if (TYPE_P (ctx) && dependent_type_p (ctx))
	return true;

      if (VAR_P (op)
	  && TREE_STATIC (op)
	  && TREE_CODE (ctx) == FUNCTION_DECL
	  && type_dependent_expression_p (ctx))
	return true;
    }

  return false;
}

/* The next set of functions are used for providing helpful explanatory
   diagnostics for failed overload resolution.  Their messages should be
   indented by two spaces for consistency with the messages in
   call.cc  */

static int
unify_success (bool /*explain_p*/)
{
  return 0;
}

/* Other failure functions should call this one, to provide a single function
   for setting a breakpoint on.  */

static int
unify_invalid (bool /*explain_p*/)
{
  return 1;
}

static int
unify_parameter_deduction_failure (bool explain_p, tree parm)
{
  if (explain_p)
    inform (input_location,
	    "  couldn%'t deduce template parameter %qD", parm);
  return unify_invalid (explain_p);
}

static int
unify_cv_qual_mismatch (bool explain_p, tree parm, tree arg)
{
  if (explain_p)
    inform (input_location,
	    "  types %qT and %qT have incompatible cv-qualifiers",
	    parm, arg);
  return unify_invalid (explain_p);
}

static int
unify_type_mismatch (bool explain_p, tree parm, tree arg)
{
  if (explain_p)
    inform (input_location, "  mismatched types %qT and %qT", parm, arg);
  return unify_invalid (explain_p);
}

static int
unify_parameter_pack_mismatch (bool explain_p, tree parm, tree arg)
{
  if (explain_p)
    inform (input_location,
	    "  template parameter %qD is not a parameter pack, but "
	    "argument %qD is",
	    parm, arg);
  return unify_invalid (explain_p);
}

static int
unify_ptrmem_cst_mismatch (bool explain_p, tree parm, tree arg)
{
  if (explain_p)
    inform (input_location,
	    "  template argument %qE does not match "
	    "pointer-to-member constant %qE",
	    arg, parm);
  return unify_invalid (explain_p);
}

static int
unify_expression_unequal (bool explain_p, tree parm, tree arg)
{
  if (explain_p)
    inform (input_location, "  %qE is not equivalent to %qE", parm, arg);
  return unify_invalid (explain_p);
}

static int
unify_parameter_pack_inconsistent (bool explain_p, tree old_arg, tree new_arg)
{
  if (explain_p)
    inform (input_location,
	    "  inconsistent parameter pack deduction with %qT and %qT",
	    old_arg, new_arg);
  return unify_invalid (explain_p);
}

static int
unify_inconsistency (bool explain_p, tree parm, tree first, tree second)
{
  if (explain_p)
    {
      if (TYPE_P (parm))
	inform (input_location,
		"  deduced conflicting types for parameter %qT (%qT and %qT)",
		parm, first, second);
      else
	inform (input_location,
		"  deduced conflicting values for non-type parameter "
		"%qE (%qE and %qE)", parm, first, second);
    }
  return unify_invalid (explain_p);
}

static int
unify_vla_arg (bool explain_p, tree arg)
{
  if (explain_p)
    inform (input_location,
	    "  variable-sized array type %qT is not "
	    "a valid template argument",
	    arg);
  return unify_invalid (explain_p);
}

static int
unify_method_type_error (bool explain_p, tree arg)
{
  if (explain_p)
    inform (input_location,
	    "  member function type %qT is not a valid template argument",
	    arg);
  return unify_invalid (explain_p);
}

static int
unify_arity (bool explain_p, int have, int wanted, bool least_p = false)
{
  if (explain_p)
    {
      if (least_p)
	inform_n (input_location, wanted,
		  "  candidate expects at least %d argument, %d provided",
		  "  candidate expects at least %d arguments, %d provided",
		  wanted, have);
      else
	inform_n (input_location, wanted,
		  "  candidate expects %d argument, %d provided",
		  "  candidate expects %d arguments, %d provided",
		  wanted, have);
    }
  return unify_invalid (explain_p);
}

static int
unify_too_many_arguments (bool explain_p, int have, int wanted)
{
  return unify_arity (explain_p, have, wanted);
}

static int
unify_too_few_arguments (bool explain_p, int have, int wanted,
			 bool least_p = false)
{
  return unify_arity (explain_p, have, wanted, least_p);
}

static int
unify_arg_conversion (bool explain_p, tree to_type,
		      tree from_type, tree arg)
{
  if (explain_p)
    inform (cp_expr_loc_or_input_loc (arg),
	    "  cannot convert %qE (type %qT) to type %qT",
	    arg, from_type, to_type);
  return unify_invalid (explain_p);
}

static int
unify_no_common_base (bool explain_p, enum template_base_result r,
		      tree parm, tree arg)
{
  if (explain_p)
    switch (r)
      {
      case tbr_ambiguous_baseclass:
	inform (input_location, "  %qT is an ambiguous base class of %qT",
		parm, arg);
	break;
      default:
	inform (input_location, "  %qT is not derived from %qT", arg, parm);
	break;
      }
  return unify_invalid (explain_p);
}

static int
unify_inconsistent_template_template_parameters (bool explain_p)
{
  if (explain_p)
    inform (input_location,
	    "  template parameters of a template template argument are "
	    "inconsistent with other deduced template arguments");
  return unify_invalid (explain_p);
}

static int
unify_template_deduction_failure (bool explain_p, tree parm, tree arg)
{
  if (explain_p)
    inform (input_location,
	    "  cannot deduce a template for %qT from non-template type %qT",
	    parm, arg);
  return unify_invalid (explain_p);
}

static int
unify_template_argument_mismatch (bool explain_p, tree parm, tree arg)
{
  if (explain_p)
    inform (input_location,
	    "  template argument %qE does not match %qE", arg, parm);
  return unify_invalid (explain_p);
}

/* Subroutine of convert_nontype_argument, to check whether EXPR, as an
   argument for TYPE, points to an unsuitable object.

   Also adjust the type of the index in C++20 array subobject references.  */

static bool
invalid_tparm_referent_p (tree type, tree expr, tsubst_flags_t complain)
{
  switch (TREE_CODE (expr))
    {
    CASE_CONVERT:
      return invalid_tparm_referent_p (type, TREE_OPERAND (expr, 0),
				       complain);

    case TARGET_EXPR:
      return invalid_tparm_referent_p (type, TARGET_EXPR_INITIAL (expr),
				       complain);

    case CONSTRUCTOR:
      {
	for (auto &e: CONSTRUCTOR_ELTS (expr))
	  if (invalid_tparm_referent_p (TREE_TYPE (e.value), e.value, complain))
	    return true;
      }
      break;

    case ADDR_EXPR:
      {
	tree decl = TREE_OPERAND (expr, 0);

	if (cxx_dialect >= cxx20)
	  while (TREE_CODE (decl) == COMPONENT_REF
		 || TREE_CODE (decl) == ARRAY_REF)
	    {
	      tree &op = TREE_OPERAND (decl, 1);
	      if (TREE_CODE (decl) == ARRAY_REF
		  && TREE_CODE (op) == INTEGER_CST)
		/* Canonicalize array offsets to ptrdiff_t; how they were
		   written doesn't matter for subobject identity.  */
		op = fold_convert (ptrdiff_type_node, op);
	      decl = TREE_OPERAND (decl, 0);
	    }

	if (!VAR_OR_FUNCTION_DECL_P (decl))
	  {
	    if (complain & tf_error)
	      error_at (cp_expr_loc_or_input_loc (expr),
			"%qE is not a valid template argument of type %qT "
			"because %qE is not a variable or function",
			expr, type, decl);
	    return true;
	  }
	else if (cxx_dialect < cxx11 && !DECL_EXTERNAL_LINKAGE_P (decl))
	  {
	    if (complain & tf_error)
	      error_at (cp_expr_loc_or_input_loc (expr),
			"%qE is not a valid template argument of type %qT "
			"in C++98 because %qD does not have external linkage",
			expr, type, decl);
	    return true;
	  }
	else if ((cxx_dialect >= cxx11 && cxx_dialect < cxx17)
		 && decl_linkage (decl) == lk_none)
	  {
	    if (complain & tf_error)
	      error_at (cp_expr_loc_or_input_loc (expr),
			"%qE is not a valid template argument of type %qT "
			"because %qD has no linkage", expr, type, decl);
	    return true;
	  }
	/* C++17: For a non-type template-parameter of reference or pointer
	   type, the value of the constant expression shall not refer to (or
	   for a pointer type, shall not be the address of):
	   * a subobject (4.5),
	   * a temporary object (15.2),
	   * a string literal (5.13.5),
	   * the result of a typeid expression (8.2.8), or
	   * a predefined __func__ variable (11.4.1).  */
	else if (VAR_P (decl) && DECL_ARTIFICIAL (decl))
	  {
	    if (complain & tf_error)
	      error ("the address of %qD is not a valid template argument",
		     decl);
	    return true;
	  }
	else if (cxx_dialect < cxx20
		 && !(same_type_ignoring_top_level_qualifiers_p
		      (strip_array_types (TREE_TYPE (type)),
		       strip_array_types (TREE_TYPE (decl)))))
	  {
	    if (complain & tf_error)
	      error ("the address of the %qT subobject of %qD is not a "
		     "valid template argument", TREE_TYPE (type), decl);
	    return true;
	  }
	else if (!TREE_STATIC (decl) && !DECL_EXTERNAL (decl))
	  {
	    if (complain & tf_error)
	      error ("the address of %qD is not a valid template argument "
		     "because it does not have static storage duration",
		     decl);
	    return true;
	  }
      }
      break;

    default:
      if (!INDIRECT_TYPE_P (type))
	/* We're only concerned about pointers and references here.  */;
      else if (cxx_dialect >= cxx11 && integer_zerop (expr))
	/* Null pointer values are OK in C++11.  */;
      else
	{
	  if (VAR_P (expr))
	    {
	      if (complain & tf_error)
		error ("%qD is not a valid template argument "
		       "because %qD is a variable, not the address of "
		       "a variable", expr, expr);
	      return true;
	    }
	  else
	    {
	      if (complain & tf_error)
		error ("%qE is not a valid template argument for %qT "
		       "because it is not the address of a variable",
		       expr, type);
	      return true;
	    }
	}
    }
  return false;

}

/* Return a VAR_DECL for the C++20 template parameter object corresponding to
   template argument EXPR.  */

static tree
create_template_parm_object (tree expr, tsubst_flags_t complain)
{
  if (TREE_CODE (expr) == TARGET_EXPR)
    expr = TARGET_EXPR_INITIAL (expr);

  if (!TREE_CONSTANT (expr))
    {
      if ((complain & tf_error)
	  && require_rvalue_constant_expression (expr))
	cxx_constant_value (expr);
      return error_mark_node;
    }
  if (invalid_tparm_referent_p (TREE_TYPE (expr), expr, complain))
    return error_mark_node;

  /* This is no longer a compound literal.  */
  gcc_assert (!TREE_HAS_CONSTRUCTOR (expr));

  return get_template_parm_object (expr, mangle_template_parm_object (expr));
}

/* The template arguments corresponding to template parameter objects of types
   that contain pointers to members.  */

static GTY(()) hash_map<tree, tree> *tparm_obj_values;

/* Find or build an nttp object for (already-validated) EXPR with name
   NAME.  */

tree
get_template_parm_object (tree expr, tree name)
{
  tree decl = get_global_binding (name);
  if (decl)
    return decl;

  tree type = cp_build_qualified_type (TREE_TYPE (expr), TYPE_QUAL_CONST);
  decl = create_temporary_var (type);
  DECL_NTTP_OBJECT_P (decl) = true;
  DECL_CONTEXT (decl) = NULL_TREE;
  TREE_STATIC (decl) = true;
  DECL_DECLARED_CONSTEXPR_P (decl) = true;
  TREE_READONLY (decl) = true;
  DECL_NAME (decl) = name;
  SET_DECL_ASSEMBLER_NAME (decl, name);
  comdat_linkage (decl);

  if (!zero_init_p (type))
    {
      /* If EXPR contains any PTRMEM_CST, they will get clobbered by
	 lower_var_init before we're done mangling.  So store the original
	 value elsewhere.  */
      tree copy = unshare_constructor (expr);
      hash_map_safe_put<hm_ggc> (tparm_obj_values, decl, copy);
    }

  pushdecl_top_level_and_finish (decl, expr);

  return decl;
}

/* Return the actual template argument corresponding to template parameter
   object VAR.  */

tree
tparm_object_argument (tree var)
{
  if (zero_init_p (TREE_TYPE (var)))
    return DECL_INITIAL (var);
  return *(tparm_obj_values->get (var));
}

/* Attempt to convert the non-type template parameter EXPR to the
   indicated TYPE.  If the conversion is successful, return the
   converted value.  If the conversion is unsuccessful, return
   NULL_TREE if we issued an error message, or error_mark_node if we
   did not.  We issue error messages for out-and-out bad template
   parameters, but not simply because the conversion failed, since we
   might be just trying to do argument deduction.  Both TYPE and EXPR
   must be non-dependent.

   The conversion follows the special rules described in
   [temp.arg.nontype], and it is much more strict than an implicit
   conversion.

   This function is called twice for each template argument (see
   lookup_template_class for a more accurate description of this
   problem). This means that we need to handle expressions which
   are not valid in a C++ source, but can be created from the
   first call (for instance, casts to perform conversions). These
   hacks can go away after we fix the double coercion problem.  */

static tree
convert_nontype_argument (tree type, tree expr, tsubst_flags_t complain)
{
  tree expr_type;
  location_t loc = cp_expr_loc_or_input_loc (expr);

  /* Detect immediately string literals as invalid non-type argument.
     This special-case is not needed for correctness (we would easily
     catch this later), but only to provide better diagnostic for this
     common user mistake. As suggested by DR 100, we do not mention
     linkage issues in the diagnostic as this is not the point.  */
  if (TREE_CODE (expr) == STRING_CST && !CLASS_TYPE_P (type))
    {
      if (complain & tf_error)
	error ("%qE is not a valid template argument for type %qT "
	       "because string literals can never be used in this context",
	       expr, type);
      return NULL_TREE;
    }

  /* Add the ADDR_EXPR now for the benefit of
     value_dependent_expression_p.  */
  if (TYPE_PTROBV_P (type)
      && TREE_CODE (TREE_TYPE (expr)) == ARRAY_TYPE)
    {
      expr = decay_conversion (expr, complain);
      if (expr == error_mark_node)
	return error_mark_node;
    }

  /* If we are in a template, EXPR may be non-dependent, but still
     have a syntactic, rather than semantic, form.  For example, EXPR
     might be a SCOPE_REF, rather than the VAR_DECL to which the
     SCOPE_REF refers.  Preserving the qualifying scope is necessary
     so that access checking can be performed when the template is
     instantiated -- but here we need the resolved form so that we can
     convert the argument.  */
  bool non_dep = false;
  if (TYPE_REF_OBJ_P (type)
      && has_value_dependent_address (expr))
    /* If we want the address and it's value-dependent, don't fold.  */;
  else if (processing_template_decl
	   && !instantiation_dependent_expression_p (expr))
    non_dep = true;
  if (error_operand_p (expr))
    return error_mark_node;
  expr_type = TREE_TYPE (expr);

  /* If the argument is non-dependent, perform any conversions in
     non-dependent context as well.  */
  processing_template_decl_sentinel s (non_dep);
  if (non_dep)
    expr = instantiate_non_dependent_expr_internal (expr, complain);

  bool val_dep_p = value_dependent_expression_p (expr);
  if (val_dep_p)
    expr = canonicalize_expr_argument (expr, complain);
  else
    STRIP_ANY_LOCATION_WRAPPER (expr);

  /* 14.3.2/5: The null pointer{,-to-member} conversion is applied
     to a non-type argument of "nullptr".  */
  if (NULLPTR_TYPE_P (expr_type) && TYPE_PTR_OR_PTRMEM_P (type))
    expr = fold_simple (convert (type, expr));

  /* In C++11, integral or enumeration non-type template arguments can be
     arbitrary constant expressions.  Pointer and pointer to
     member arguments can be general constant expressions that evaluate
     to a null value, but otherwise still need to be of a specific form.  */
  if (cxx_dialect >= cxx11)
    {
      if (TREE_CODE (expr) == PTRMEM_CST && TYPE_PTRMEM_P (type))
	/* A PTRMEM_CST is already constant, and a valid template
	   argument for a parameter of pointer to member type, we just want
	   to leave it in that form rather than lower it to a
	   CONSTRUCTOR.  */;
      else if (INTEGRAL_OR_ENUMERATION_TYPE_P (type)
	       || cxx_dialect >= cxx17)
	{
	  /* C++17: A template-argument for a non-type template-parameter shall
	     be a converted constant expression (8.20) of the type of the
	     template-parameter.  */
	  expr = build_converted_constant_expr (type, expr, complain);
	  if (expr == error_mark_node)
	    /* Make sure we return NULL_TREE only if we have really issued
	       an error, as described above.  */
	    return (complain & tf_error) ? NULL_TREE : error_mark_node;
	  else if (TREE_CODE (expr) == IMPLICIT_CONV_EXPR)
	    {
	      IMPLICIT_CONV_EXPR_NONTYPE_ARG (expr) = true;
	      return expr;
	    }
	  expr = maybe_constant_value (expr, NULL_TREE, mce_true);
	  expr = convert_from_reference (expr);
	  /* EXPR may have become value-dependent.  */
	  val_dep_p = value_dependent_expression_p (expr);
	}
      else if (TYPE_PTR_OR_PTRMEM_P (type))
	{
	  tree folded = maybe_constant_value (expr, NULL_TREE, mce_true);
	  if (TYPE_PTR_P (type) ? integer_zerop (folded)
	      : null_member_pointer_value_p (folded))
	    expr = folded;
	}
    }

  if (TYPE_REF_P (type))
    expr = mark_lvalue_use (expr);
  else
    expr = mark_rvalue_use (expr);

  /* HACK: Due to double coercion, we can get a
     NOP_EXPR<REFERENCE_TYPE>(ADDR_EXPR<POINTER_TYPE> (arg)) here,
     which is the tree that we built on the first call (see
     below when coercing to reference to object or to reference to
     function). We just strip everything and get to the arg.
     See g++.old-deja/g++.oliva/template4.C and g++.dg/template/nontype9.C
     for examples.  */
  if (TYPE_REF_OBJ_P (type) || TYPE_REFFN_P (type))
    {
      /* Check this before we strip *& to avoid redundancy.  */
      if (!mark_single_function (expr, complain))
	return error_mark_node;

      tree probe_type, probe = expr;
      if (REFERENCE_REF_P (probe))
	probe = TREE_OPERAND (probe, 0);
      probe_type = TREE_TYPE (probe);
      if (TREE_CODE (probe) == NOP_EXPR)
	{
	  /* ??? Maybe we could use convert_from_reference here, but we
	     would need to relax its constraints because the NOP_EXPR
	     could actually change the type to something more cv-qualified,
	     and this is not folded by convert_from_reference.  */
	  tree addr = TREE_OPERAND (probe, 0);
	  if (TYPE_REF_P (probe_type)
	      && TREE_CODE (addr) == ADDR_EXPR
	      && TYPE_PTR_P (TREE_TYPE (addr))
	      && (same_type_ignoring_top_level_qualifiers_p
		  (TREE_TYPE (probe_type),
		   TREE_TYPE (TREE_TYPE (addr)))))
	    {
	      expr = TREE_OPERAND (addr, 0);
	      expr_type = TREE_TYPE (probe_type);
	    }
	}
    }

  /* [temp.arg.nontype]/5, bullet 1

     For a non-type template-parameter of integral or enumeration type,
     integral promotions (_conv.prom_) and integral conversions
     (_conv.integral_) are applied.  */
  if (INTEGRAL_OR_ENUMERATION_TYPE_P (type)
      || SCALAR_FLOAT_TYPE_P (type))
    {
      if (cxx_dialect < cxx11)
	{
	  tree t = build_converted_constant_expr (type, expr, complain);
	  t = maybe_constant_value (t);
	  if (t != error_mark_node)
	    expr = t;
	}

      if (!same_type_ignoring_top_level_qualifiers_p (type, TREE_TYPE (expr)))
	return error_mark_node;

      /* Notice that there are constant expressions like '4 % 0' which
	 do not fold into integer constants.  */
      if (!CONSTANT_CLASS_P (expr) && !val_dep_p)
	{
	  if (complain & tf_error)
	    {
	      int errs = errorcount, warns = warningcount + werrorcount;
	      if (!require_potential_constant_expression (expr))
		expr = error_mark_node;
	      else
		expr = cxx_constant_value (expr);
	      if (errorcount > errs || warningcount + werrorcount > warns)
		inform (loc, "in template argument for type %qT", type);
	      if (expr == error_mark_node)
		return NULL_TREE;
	      /* else cxx_constant_value complained but gave us
		 a real constant, so go ahead.  */
	      if (!CONSTANT_CLASS_P (expr))
		{
		  /* Some assemble time constant expressions like
		     (intptr_t)&&lab1 - (intptr_t)&&lab2 or
		     4 + (intptr_t)&&var satisfy reduced_constant_expression_p
		     as we can emit them into .rodata initializers of
		     variables, yet they can't fold into an INTEGER_CST at
		     compile time.  Refuse them here.  */
		  gcc_checking_assert (reduced_constant_expression_p (expr));
		  error_at (loc, "template argument %qE for type %qT not "
				 "a compile-time constant", expr, type);
		  return NULL_TREE;
		}
	    }
	  else
	    return NULL_TREE;
	}

      /* Avoid typedef problems.  */
      if (TREE_TYPE (expr) != type)
	expr = fold_convert (type, expr);
    }
  /* [temp.arg.nontype]/5, bullet 2

     For a non-type template-parameter of type pointer to object,
     qualification conversions (_conv.qual_) and the array-to-pointer
     conversion (_conv.array_) are applied.  */
  else if (TYPE_PTROBV_P (type))
    {
      tree decayed = expr;

      /* Look through any NOP_EXPRs around an ADDR_EXPR, whether they come from
	 decay_conversion or an explicit cast.  If it's a problematic cast,
	 we'll complain about it below.  */
      if (TREE_CODE (expr) == NOP_EXPR)
	{
	  tree probe = expr;
	  STRIP_NOPS (probe);
	  if (TREE_CODE (probe) == ADDR_EXPR
	      && TYPE_PTR_P (TREE_TYPE (probe)))
	    {
	      expr = probe;
	      expr_type = TREE_TYPE (expr);
	    }
	}

      /* [temp.arg.nontype]/1  (TC1 version, DR 49):

	 A template-argument for a non-type, non-template template-parameter
	 shall be one of: [...]

	 -- the name of a non-type template-parameter;
	 -- the address of an object or function with external linkage, [...]
	    expressed as "& id-expression" where the & is optional if the name
	    refers to a function or array, or if the corresponding
	    template-parameter is a reference.

	Here, we do not care about functions, as they are invalid anyway
	for a parameter of type pointer-to-object.  */

      if (val_dep_p)
	/* Non-type template parameters are OK.  */
	;
      else if (cxx_dialect >= cxx11 && integer_zerop (expr))
	/* Null pointer values are OK in C++11.  */;
      else if (TREE_CODE (expr) != ADDR_EXPR
	       && !INDIRECT_TYPE_P (expr_type))
	/* Other values, like integer constants, might be valid
	   non-type arguments of some other type.  */
	return error_mark_node;
      else if (invalid_tparm_referent_p (type, expr, complain))
	return NULL_TREE;

      expr = decayed;

      expr = perform_qualification_conversions (type, expr);
      if (expr == error_mark_node)
	return error_mark_node;
    }
  /* [temp.arg.nontype]/5, bullet 3

     For a non-type template-parameter of type reference to object, no
     conversions apply. The type referred to by the reference may be more
     cv-qualified than the (otherwise identical) type of the
     template-argument. The template-parameter is bound directly to the
     template-argument, which must be an lvalue.  */
  else if (TYPE_REF_OBJ_P (type))
    {
      if (!same_type_ignoring_top_level_qualifiers_p (TREE_TYPE (type),
						      expr_type))
	return error_mark_node;

      if (!at_least_as_qualified_p (TREE_TYPE (type), expr_type))
	{
	  if (complain & tf_error)
	    error ("%qE is not a valid template argument for type %qT "
		   "because of conflicts in cv-qualification", expr, type);
	  return NULL_TREE;
	}

      if (!lvalue_p (expr))
	{
	  if (complain & tf_error)
	    error ("%qE is not a valid template argument for type %qT "
		   "because it is not an lvalue", expr, type);
	  return NULL_TREE;
	}

      /* [temp.arg.nontype]/1

	 A template-argument for a non-type, non-template template-parameter
	 shall be one of: [...]

	 -- the address of an object or function with external linkage.  */
      if (INDIRECT_REF_P (expr)
	  && TYPE_REF_OBJ_P (TREE_TYPE (TREE_OPERAND (expr, 0))))
	{
	  expr = TREE_OPERAND (expr, 0);
	  if (DECL_P (expr))
	    {
	      if (complain & tf_error)
		error ("%q#D is not a valid template argument for type %qT "
		       "because a reference variable does not have a constant "
		       "address", expr, type);
	      return NULL_TREE;
	    }
	}

      if (TYPE_REF_OBJ_P (TREE_TYPE (expr)) && val_dep_p)
	/* OK, dependent reference.  We don't want to ask whether a DECL is
	   itself value-dependent, since what we want here is its address.  */;
      else
	{
	  expr = build_address (expr);

	  if (invalid_tparm_referent_p (type, expr, complain))
	    return NULL_TREE;
	}

      if (!same_type_p (type, TREE_TYPE (expr)))
	expr = build_nop (type, expr);
    }
  /* [temp.arg.nontype]/5, bullet 4

     For a non-type template-parameter of type pointer to function, only
     the function-to-pointer conversion (_conv.func_) is applied. If the
     template-argument represents a set of overloaded functions (or a
     pointer to such), the matching function is selected from the set
     (_over.over_).  */
  else if (TYPE_PTRFN_P (type))
    {
      /* If the argument is a template-id, we might not have enough
	 context information to decay the pointer.  */
      if (!type_unknown_p (expr_type))
	{
	  expr = decay_conversion (expr, complain);
	  if (expr == error_mark_node)
	    return error_mark_node;
	}

      if (cxx_dialect >= cxx11 && integer_zerop (expr))
	/* Null pointer values are OK in C++11.  */
	return perform_qualification_conversions (type, expr);

      expr = convert_nontype_argument_function (type, expr, complain);
      if (!expr || expr == error_mark_node)
	return expr;
    }
  /* [temp.arg.nontype]/5, bullet 5

     For a non-type template-parameter of type reference to function, no
     conversions apply. If the template-argument represents a set of
     overloaded functions, the matching function is selected from the set
     (_over.over_).  */
  else if (TYPE_REFFN_P (type))
    {
      if (TREE_CODE (expr) == ADDR_EXPR)
	{
	  if (complain & tf_error)
	    {
	      error ("%qE is not a valid template argument for type %qT "
		     "because it is a pointer", expr, type);
	      inform (input_location, "try using %qE instead",
		      TREE_OPERAND (expr, 0));
	    }
	  return NULL_TREE;
	}

      expr = convert_nontype_argument_function (type, expr, complain);
      if (!expr || expr == error_mark_node)
	return expr;
    }
  /* [temp.arg.nontype]/5, bullet 6

     For a non-type template-parameter of type pointer to member function,
     no conversions apply. If the template-argument represents a set of
     overloaded member functions, the matching member function is selected
     from the set (_over.over_).  */
  else if (TYPE_PTRMEMFUNC_P (type))
    {
      expr = instantiate_type (type, expr, tf_none);
      if (expr == error_mark_node)
	return error_mark_node;

      /* [temp.arg.nontype] bullet 1 says the pointer to member
         expression must be a pointer-to-member constant.  */
      if (!val_dep_p
	  && !check_valid_ptrmem_cst_expr (type, expr, complain))
	return NULL_TREE;

      /* Repeated conversion can't deal with a conversion that turns PTRMEM_CST
	 into a CONSTRUCTOR, so build up a new PTRMEM_CST instead.  */
      if (fnptr_conv_p (type, TREE_TYPE (expr)))
	expr = make_ptrmem_cst (type, PTRMEM_CST_MEMBER (expr));
    }
  /* [temp.arg.nontype]/5, bullet 7

     For a non-type template-parameter of type pointer to data member,
     qualification conversions (_conv.qual_) are applied.  */
  else if (TYPE_PTRDATAMEM_P (type))
    {
      /* [temp.arg.nontype] bullet 1 says the pointer to member
         expression must be a pointer-to-member constant.  */
      if (!val_dep_p
	  && !check_valid_ptrmem_cst_expr (type, expr, complain))
	return NULL_TREE;

      expr = perform_qualification_conversions (type, expr);
      if (expr == error_mark_node)
	return expr;
    }
  else if (NULLPTR_TYPE_P (type))
    {
      if (!NULLPTR_TYPE_P (TREE_TYPE (expr)))
	{
	  if (complain & tf_error)
	    error ("%qE is not a valid template argument for type %qT "
		   "because it is of type %qT", expr, type, TREE_TYPE (expr));
	  return NULL_TREE;
	}
      return expr;
    }
  else if (CLASS_TYPE_P (type))
    {
      /* Replace the argument with a reference to the corresponding template
	 parameter object.  */
      if (!val_dep_p)
	expr = create_template_parm_object (expr, complain);
      if (expr == error_mark_node)
	return NULL_TREE;
    }
  /* A template non-type parameter must be one of the above.  */
  else
    gcc_unreachable ();

  /* Sanity check: did we actually convert the argument to the
     right type?  */
  gcc_assert (same_type_ignoring_top_level_qualifiers_p
	      (type, TREE_TYPE (expr)));
  return convert_from_reference (expr);
}

/* Subroutine of coerce_template_template_parms, which returns 1 if
   PARM_PARM and ARG_PARM match using the rule for the template
   parameters of template template parameters. Both PARM and ARG are
   template parameters; the rest of the arguments are the same as for
   coerce_template_template_parms.
 */
static int
coerce_template_template_parm (tree parm,
                              tree arg,
                              tsubst_flags_t complain,
                              tree in_decl,
                              tree outer_args)
{
  if (arg == NULL_TREE || error_operand_p (arg)
      || parm == NULL_TREE || error_operand_p (parm))
    return 0;

  if (TREE_CODE (arg) != TREE_CODE (parm))
    return 0;

  switch (TREE_CODE (parm))
    {
    case TEMPLATE_DECL:
      /* We encounter instantiations of templates like
	 template <template <template <class> class> class TT>
	 class C;  */
      {
	if (!coerce_template_template_parms
	    (parm, arg, complain, in_decl, outer_args))
	  return 0;
      }
      /* Fall through.  */

    case TYPE_DECL:
      if (TEMPLATE_TYPE_PARAMETER_PACK (TREE_TYPE (arg))
	  && !TEMPLATE_TYPE_PARAMETER_PACK (TREE_TYPE (parm)))
	/* Argument is a parameter pack but parameter is not.  */
	return 0;
      break;

    case PARM_DECL:
      /* The tsubst call is used to handle cases such as

           template <int> class C {};
	   template <class T, template <T> class TT> class D {};
	   D<int, C> d;

	 i.e. the parameter list of TT depends on earlier parameters.  */
      if (!uses_template_parms (TREE_TYPE (arg)))
	{
	  tree t = tsubst (TREE_TYPE (parm), outer_args, complain, in_decl);
	  if (!uses_template_parms (t)
	      && !same_type_p (t, TREE_TYPE (arg)))
	    return 0;
	}

      if (TEMPLATE_PARM_PARAMETER_PACK (DECL_INITIAL (arg))
	  && !TEMPLATE_PARM_PARAMETER_PACK (DECL_INITIAL (parm)))
	/* Argument is a parameter pack but parameter is not.  */
	return 0;

      break;

    default:
      gcc_unreachable ();
    }

  return 1;
}

/* Coerce template argument list ARGLIST for use with template
   template-parameter TEMPL.  */

static tree
coerce_template_args_for_ttp (tree templ, tree arglist,
			      tsubst_flags_t complain)
{
  /* Consider an example where a template template parameter declared as

     template <class T, class U = std::allocator<T> > class TT

     The template parameter level of T and U are one level larger than
     of TT.  To proper process the default argument of U, say when an
     instantiation `TT<int>' is seen, we need to build the full
     arguments containing {int} as the innermost level.  Outer levels,
     available when not appearing as default template argument, can be
     obtained from the arguments of the enclosing template.

     Suppose that TT is later substituted with std::vector.  The above
     instantiation is `TT<int, std::allocator<T> >' with TT at
     level 1, and T at level 2, while the template arguments at level 1
     becomes {std::vector} and the inner level 2 is {int}.  */

  tree outer = DECL_CONTEXT (templ);
  if (outer)
    outer = generic_targs_for (outer);
  else if (current_template_parms)
    {
      /* This is an argument of the current template, so we haven't set
	 DECL_CONTEXT yet.  We can also get here when level-lowering a
	 bound ttp. */
      tree relevant_template_parms;

      /* Parameter levels that are greater than the level of the given
	 template template parm are irrelevant.  */
      relevant_template_parms = current_template_parms;
      while (TMPL_PARMS_DEPTH (relevant_template_parms)
	     != TEMPLATE_TYPE_LEVEL (TREE_TYPE (templ)))
	relevant_template_parms = TREE_CHAIN (relevant_template_parms);

      outer = template_parms_to_args (relevant_template_parms);
    }

  if (outer)
    arglist = add_to_template_args (outer, arglist);

  tree parmlist = DECL_INNERMOST_TEMPLATE_PARMS (templ);
  return coerce_template_parms (parmlist, arglist, templ, complain);
}

/* A cache of template template parameters with match-all default
   arguments.  */
static GTY((deletable)) hash_map<tree,tree> *defaulted_ttp_cache;

/* T is a bound template template-parameter.  Copy its arguments into default
   arguments of the template template-parameter's template parameters.  */

static tree
add_defaults_to_ttp (tree otmpl)
{
  if (tree *c = hash_map_safe_get (defaulted_ttp_cache, otmpl))
    return *c;

  tree ntmpl = copy_node (otmpl);

  tree ntype = copy_node (TREE_TYPE (otmpl));
  TYPE_STUB_DECL (ntype) = TYPE_NAME (ntype) = ntmpl;
  TYPE_MAIN_VARIANT (ntype) = ntype;
  TYPE_POINTER_TO (ntype) = TYPE_REFERENCE_TO (ntype) = NULL_TREE;
  TYPE_NAME (ntype) = ntmpl;
  SET_TYPE_STRUCTURAL_EQUALITY (ntype);

  tree idx = TEMPLATE_TYPE_PARM_INDEX (ntype)
    = copy_node (TEMPLATE_TYPE_PARM_INDEX (ntype));
  TEMPLATE_PARM_DECL (idx) = ntmpl;
  TREE_TYPE (ntmpl) = TREE_TYPE (idx) = ntype;

  tree oparms = DECL_TEMPLATE_PARMS (otmpl);
  tree parms = DECL_TEMPLATE_PARMS (ntmpl) = copy_node (oparms);
  TREE_CHAIN (parms) = TREE_CHAIN (oparms);
  tree vec = TREE_VALUE (parms) = copy_node (TREE_VALUE (parms));
  for (int i = 0; i < TREE_VEC_LENGTH (vec); ++i)
    {
      tree o = TREE_VEC_ELT (vec, i);
      if (!template_parameter_pack_p (TREE_VALUE (o)))
	{
	  tree n = TREE_VEC_ELT (vec, i) = copy_node (o);
	  TREE_PURPOSE (n) = any_targ_node;
	}
    }

  tree oresult = DECL_TEMPLATE_RESULT (otmpl);
  tree gen_otmpl = DECL_TI_TEMPLATE (oresult);
  tree gen_ntmpl;
  if (gen_otmpl == otmpl)
    gen_ntmpl = ntmpl;
  else
    gen_ntmpl = add_defaults_to_ttp (gen_otmpl);

  tree nresult = copy_decl (oresult);
  DECL_TEMPLATE_INFO (nresult)
    = build_template_info (gen_ntmpl, TI_ARGS (DECL_TEMPLATE_INFO (oresult)));
  DECL_TEMPLATE_RESULT (ntmpl) = nresult;

  hash_map_safe_put<hm_ggc> (defaulted_ttp_cache, otmpl, ntmpl);
  return ntmpl;
}

/* ARG is a bound potential template template-argument, and PARGS is a list
   of arguments for the corresponding template template-parameter.  Adjust
   PARGS as appropriate for application to ARG's template, and if ARG is a
   BOUND_TEMPLATE_TEMPLATE_PARM, possibly adjust it to add default template
   arguments to the template template parameter.  */

static tree
coerce_ttp_args_for_tta (tree& arg, tree pargs, tsubst_flags_t complain)
{
  ++processing_template_decl;
  tree arg_tmpl = TYPE_TI_TEMPLATE (arg);
  if (DECL_TEMPLATE_TEMPLATE_PARM_P (arg_tmpl))
    {
      /* When comparing two template template-parameters in partial ordering,
	 rewrite the one currently being used as an argument to have default
	 arguments for all parameters.  */
      arg_tmpl = add_defaults_to_ttp (arg_tmpl);
      pargs = coerce_template_args_for_ttp (arg_tmpl, pargs, complain);
      if (pargs != error_mark_node)
	arg = bind_template_template_parm (TREE_TYPE (arg_tmpl),
					   TYPE_TI_ARGS (arg));
    }
  else
    {
      tree aparms
	= INNERMOST_TEMPLATE_PARMS (DECL_TEMPLATE_PARMS (arg_tmpl));
      pargs = coerce_template_parms (aparms, pargs, arg_tmpl, complain);
    }
  --processing_template_decl;
  return pargs;
}

/* Subroutine of unify for the case when PARM is a
   BOUND_TEMPLATE_TEMPLATE_PARM.  */

static int
unify_bound_ttp_args (tree tparms, tree targs, tree parm, tree& arg,
		      bool explain_p)
{
  tree parmvec = TYPE_TI_ARGS (parm);
  tree argvec = INNERMOST_TEMPLATE_ARGS (TYPE_TI_ARGS (arg));

  /* The template template parm might be variadic and the argument
     not, so flatten both argument lists.  */
  parmvec = expand_template_argument_pack (parmvec);
  argvec = expand_template_argument_pack (argvec);

  if (flag_new_ttp)
    {
      /* In keeping with P0522R0, adjust P's template arguments
	 to apply to A's template; then flatten it again.  */
      tree nparmvec = coerce_ttp_args_for_tta (arg, parmvec, tf_none);
      nparmvec = expand_template_argument_pack (nparmvec);

      if (unify (tparms, targs, nparmvec, argvec,
		 UNIFY_ALLOW_NONE, explain_p))
	return 1;

      /* If the P0522 adjustment eliminated a pack expansion, deduce
	 empty packs.  */
      if (flag_new_ttp
	  && TREE_VEC_LENGTH (nparmvec) < TREE_VEC_LENGTH (parmvec)
	  && unify_pack_expansion (tparms, targs, parmvec, argvec,
				   DEDUCE_EXACT, /*sub*/true, explain_p))
	return 1;
    }
  else
    {
      /* Deduce arguments T, i from TT<T> or TT<i>.
	 We check each element of PARMVEC and ARGVEC individually
	 rather than the whole TREE_VEC since they can have
	 different number of elements, which is allowed under N2555.  */

      int len = TREE_VEC_LENGTH (parmvec);

      /* Check if the parameters end in a pack, making them
	 variadic.  */
      int parm_variadic_p = 0;
      if (len > 0
	  && PACK_EXPANSION_P (TREE_VEC_ELT (parmvec, len - 1)))
	parm_variadic_p = 1;

      for (int i = 0; i < len - parm_variadic_p; ++i)
	/* If the template argument list of P contains a pack
	   expansion that is not the last template argument, the
	   entire template argument list is a non-deduced
	   context.  */
	if (PACK_EXPANSION_P (TREE_VEC_ELT (parmvec, i)))
	  return unify_success (explain_p);

      if (TREE_VEC_LENGTH (argvec) < len - parm_variadic_p)
	return unify_too_few_arguments (explain_p,
					TREE_VEC_LENGTH (argvec), len);

      for (int i = 0; i < len - parm_variadic_p; ++i)
	if (unify (tparms, targs,
		   TREE_VEC_ELT (parmvec, i),
		   TREE_VEC_ELT (argvec, i),
		   UNIFY_ALLOW_NONE, explain_p))
	  return 1;

      if (parm_variadic_p
	  && unify_pack_expansion (tparms, targs,
				   parmvec, argvec,
				   DEDUCE_EXACT,
				   /*subr=*/true, explain_p))
	return 1;
    }

  return 0;
}

/* Return 1 if PARM_TMPL and ARG_TMPL match using rule for
   template template parameters.

   Consider the example:
     template <class T> class A;
     template<template <class U> class TT> class B;

   For B<A>, PARM_TMPL is TT, while ARG_TMPL is A,
   and OUTER_ARGS contains A.  */

static int
coerce_template_template_parms (tree parm_tmpl,
				tree arg_tmpl,
				tsubst_flags_t complain,
				tree in_decl,
				tree outer_args)
{
  int nparms, nargs, i;
  tree parm, arg;
  int variadic_p = 0;

  tree parm_parms = DECL_INNERMOST_TEMPLATE_PARMS (parm_tmpl);
  tree arg_parms = DECL_INNERMOST_TEMPLATE_PARMS (arg_tmpl);
  tree gen_arg_tmpl = most_general_template (arg_tmpl);
  tree gen_arg_parms = DECL_INNERMOST_TEMPLATE_PARMS (gen_arg_tmpl);

  nparms = TREE_VEC_LENGTH (parm_parms);
  nargs = TREE_VEC_LENGTH (arg_parms);

  if (flag_new_ttp)
    {
      /* P0522R0: A template template-parameter P is at least as specialized as
	 a template template-argument A if, given the following rewrite to two
	 function templates, the function template corresponding to P is at
	 least as specialized as the function template corresponding to A
	 according to the partial ordering rules for function templates
	 ([temp.func.order]). Given an invented class template X with the
	 template parameter list of A (including default arguments):

	 * Each of the two function templates has the same template parameters,
	 respectively, as P or A.

	 * Each function template has a single function parameter whose type is
	 a specialization of X with template arguments corresponding to the
	 template parameters from the respective function template where, for
	 each template parameter PP in the template parameter list of the
	 function template, a corresponding template argument AA is formed. If
	 PP declares a parameter pack, then AA is the pack expansion
	 PP... ([temp.variadic]); otherwise, AA is the id-expression PP.

	 If the rewrite produces an invalid type, then P is not at least as
	 specialized as A.  */

      /* So coerce P's args to apply to A's parms, and then deduce between A's
	 args and the converted args.  If that succeeds, A is at least as
	 specialized as P, so they match.*/
      processing_template_decl_sentinel ptds (/*reset*/false);
      ++processing_template_decl;

      tree pargs = template_parms_level_to_args (parm_parms);

      /* PARM and ARG might be at different template depths, and we want to
	 pass the right additional levels of args when coercing PARGS to
	 ARG_PARMS in case we need to do any substitution into non-type
	 template parameter types.

	 OUTER_ARGS are not the right outer levels in this case, as they are
	 the args we're building up for PARM, and for the coercion we want the
	 args for ARG.  If DECL_CONTEXT isn't set for a template template
	 parameter, we can assume that it's in the current scope.  */
      tree ctx = DECL_CONTEXT (arg_tmpl);
      if (!ctx && DECL_TEMPLATE_TEMPLATE_PARM_P (arg_tmpl))
	ctx = current_scope ();
      tree scope_args = NULL_TREE;
      if (tree tinfo = get_template_info (ctx))
	scope_args = TI_ARGS (tinfo);
      if (DECL_TEMPLATE_TEMPLATE_PARM_P (arg_tmpl))
	{
	  int level = TEMPLATE_TYPE_LEVEL (TREE_TYPE (gen_arg_tmpl));
	  int scope_depth = TMPL_ARGS_DEPTH (scope_args);
	  tree full_pargs = make_tree_vec (level + 1);

	/* Only use as many levels from the scope as needed
	   (excluding the level of ARG).  */
	  for (int i = 0; i < level - 1; ++i)
	    if (i < scope_depth)
	      TREE_VEC_ELT (full_pargs, i) = TMPL_ARGS_LEVEL (scope_args, i + 1);
	    else
	      TREE_VEC_ELT (full_pargs, i) = make_tree_vec (0);

	  /* Add the arguments that appear at the levels of ARG.  */
	  tree adjacent = DECL_TI_ARGS (DECL_TEMPLATE_RESULT (arg_tmpl));
	  adjacent = TMPL_ARGS_LEVEL (adjacent, TMPL_ARGS_DEPTH (adjacent) - 1);
	  TREE_VEC_ELT (full_pargs, level - 1) = adjacent;

	  TREE_VEC_ELT (full_pargs, level) = pargs;
	  pargs = full_pargs;
	}
      else
	pargs = add_to_template_args (scope_args, pargs);

      pargs = coerce_template_parms (gen_arg_parms, pargs,
				     NULL_TREE, tf_none);
      if (pargs != error_mark_node)
	{
	  tree targs = make_tree_vec (nargs);
	  tree aargs = template_parms_level_to_args (arg_parms);
	  if (!unify (arg_parms, targs, aargs, pargs, UNIFY_ALLOW_NONE,
		      /*explain*/false))
	    return 1;
	}
    }

  /* Determine whether we have a parameter pack at the end of the
     template template parameter's template parameter list.  */
  if (TREE_VEC_ELT (parm_parms, nparms - 1) != error_mark_node)
    {
      parm = TREE_VALUE (TREE_VEC_ELT (parm_parms, nparms - 1));

      if (error_operand_p (parm))
	return 0;

      switch (TREE_CODE (parm))
        {
        case TEMPLATE_DECL:
        case TYPE_DECL:
          if (TEMPLATE_TYPE_PARAMETER_PACK (TREE_TYPE (parm)))
            variadic_p = 1;
          break;

        case PARM_DECL:
          if (TEMPLATE_PARM_PARAMETER_PACK (DECL_INITIAL (parm)))
            variadic_p = 1;
          break;

        default:
          gcc_unreachable ();
        }
    }

  if (nargs != nparms
      && !(variadic_p && nargs >= nparms - 1))
    return 0;

  /* Check all of the template parameters except the parameter pack at
     the end (if any).  */
  for (i = 0; i < nparms - variadic_p; ++i)
    {
      if (TREE_VEC_ELT (parm_parms, i) == error_mark_node
          || TREE_VEC_ELT (arg_parms, i) == error_mark_node)
        continue;

      parm = TREE_VALUE (TREE_VEC_ELT (parm_parms, i));
      arg = TREE_VALUE (TREE_VEC_ELT (arg_parms, i));

      if (!coerce_template_template_parm (parm, arg, complain, in_decl,
                                          outer_args))
	return 0;

    }

  if (variadic_p)
    {
      /* Check each of the template parameters in the template
	 argument against the template parameter pack at the end of
	 the template template parameter.  */
      if (TREE_VEC_ELT (parm_parms, i) == error_mark_node)
	return 0;

      parm = TREE_VALUE (TREE_VEC_ELT (parm_parms, i));

      for (; i < nargs; ++i)
        {
          if (TREE_VEC_ELT (arg_parms, i) == error_mark_node)
            continue;

          arg = TREE_VALUE (TREE_VEC_ELT (arg_parms, i));

          if (!coerce_template_template_parm (parm, arg, complain, in_decl,
                                              outer_args))
            return 0;
        }
    }

  return 1;
}

/* Verifies that the deduced template arguments (in TARGS) for the
   template template parameters (in TPARMS) represent valid bindings,
   by comparing the template parameter list of each template argument
   to the template parameter list of its corresponding template
   template parameter, in accordance with DR150. This
   routine can only be called after all template arguments have been
   deduced. It will return TRUE if all of the template template
   parameter bindings are okay, FALSE otherwise.  */
bool
template_template_parm_bindings_ok_p (tree tparms, tree targs)
{
  int i, ntparms = TREE_VEC_LENGTH (tparms);
  bool ret = true;

  /* We're dealing with template parms in this process.  */
  ++processing_template_decl;

  targs = INNERMOST_TEMPLATE_ARGS (targs);

  for (i = 0; i < ntparms; ++i)
    {
      tree tparm = TREE_VALUE (TREE_VEC_ELT (tparms, i));
      tree targ = TREE_VEC_ELT (targs, i);

      if (TREE_CODE (tparm) == TEMPLATE_DECL && targ)
	{
	  tree packed_args = NULL_TREE;
	  int idx, len = 1;

	  if (ARGUMENT_PACK_P (targ))
	    {
	      /* Look inside the argument pack.  */
	      packed_args = ARGUMENT_PACK_ARGS (targ);
	      len = TREE_VEC_LENGTH (packed_args);
	    }

	  for (idx = 0; idx < len; ++idx)
	    {
	      if (packed_args)
		/* Extract the next argument from the argument
		   pack.  */
		targ = TREE_VEC_ELT (packed_args, idx);

	      if (PACK_EXPANSION_P (targ))
		/* Look at the pattern of the pack expansion.  */
		targ = PACK_EXPANSION_PATTERN (targ);

	      /* Extract the template parameters from the template
		 argument.  */
	      if (TREE_CODE (targ) == TEMPLATE_TEMPLATE_PARM)
		targ = TYPE_NAME (targ);

	      /* Verify that we can coerce the template template
		 parameters from the template argument to the template
		 parameter.  This requires an exact match.  */
	      if (TREE_CODE (targ) == TEMPLATE_DECL
		  && !coerce_template_template_parms
		       (tparm,
			targ,
			tf_none,
			tparm,
			targs))
		{
		  ret = false;
		  goto out;
		}
	    }
	}
    }

 out:

  --processing_template_decl;
  return ret;
}

/* Since type attributes aren't mangled, we need to strip them from
   template type arguments.  */

tree
canonicalize_type_argument (tree arg, tsubst_flags_t complain)
{
  if (!arg || arg == error_mark_node || arg == TYPE_CANONICAL (arg))
    return arg;
  bool removed_attributes = false;
  tree canon = strip_typedefs (arg, &removed_attributes);
  if (removed_attributes
      && (complain & tf_warning))
    warning (OPT_Wignored_attributes,
	     "ignoring attributes on template argument %qT", arg);
  return canon;
}

/* And from inside dependent non-type arguments like sizeof(Type).  */

static tree
canonicalize_expr_argument (tree arg, tsubst_flags_t complain)
{
  if (!arg || arg == error_mark_node)
    return arg;
  bool removed_attributes = false;
  tree canon = strip_typedefs_expr (arg, &removed_attributes);
  if (removed_attributes
      && (complain & tf_warning))
    warning (OPT_Wignored_attributes,
	     "ignoring attributes in template argument %qE", arg);
  return canon;
}

/* A template declaration can be substituted for a constrained
   template template parameter only when the argument is no more
   constrained than the parameter.  */

static bool
is_compatible_template_arg (tree parm, tree arg)
{
  tree parm_cons = get_constraints (parm);

  /* For now, allow constrained template template arguments
     and unconstrained template template parameters.  */
  if (parm_cons == NULL_TREE)
    return true;

  /* If the template parameter is constrained, we need to rewrite its
     constraints in terms of the ARG's template parameters. This ensures
     that all of the template parameter types will have the same depth.

     Note that this is only valid when coerce_template_template_parm is
     true for the innermost template parameters of PARM and ARG. In other
     words, because coercion is successful, this conversion will be valid.  */
  tree new_args = NULL_TREE;
  if (parm_cons)
    {
      tree aparms = DECL_INNERMOST_TEMPLATE_PARMS (arg);
      new_args = template_parms_level_to_args (aparms);
      ++processing_template_decl;
      parm_cons = tsubst_constraint_info (parm_cons, new_args,
					  tf_none, NULL_TREE);
      --processing_template_decl;
      if (parm_cons == error_mark_node)
        return false;
    }

  return weakly_subsumes (parm_cons, arg);
}

// Convert a placeholder argument into a binding to the original
// parameter. The original parameter is saved as the TREE_TYPE of
// ARG.
static inline tree
convert_wildcard_argument (tree parm, tree arg)
{
  TREE_TYPE (arg) = parm;
  return arg;
}

/* We can't fully resolve ARG given as a non-type template argument to TYPE,
   because one of them is dependent.  But we need to represent the
   conversion for the benefit of cp_tree_equal.  */

static tree
maybe_convert_nontype_argument (tree type, tree arg)
{
  /* Auto parms get no conversion.  */
  if (type_uses_auto (type))
    return arg;
  /* We don't need or want to add this conversion now if we're going to use the
     argument for deduction.  */
  if (value_dependent_expression_p (arg))
    return arg;

  type = cv_unqualified (type);
  tree argtype = TREE_TYPE (arg);
  if (same_type_p (type, argtype))
    return arg;

  arg = build1 (IMPLICIT_CONV_EXPR, type, arg);
  IMPLICIT_CONV_EXPR_NONTYPE_ARG (arg) = true;
  return arg;
}

/* Convert the indicated template ARG as necessary to match the
   indicated template PARM.  Returns the converted ARG, or
   error_mark_node if the conversion was unsuccessful.  Error and
   warning messages are issued under control of COMPLAIN.  This
   conversion is for the Ith parameter in the parameter list.  ARGS is
   the full set of template arguments deduced so far.  */

static tree
convert_template_argument (tree parm,
			   tree arg,
			   tree args,
			   tsubst_flags_t complain,
			   int i,
			   tree in_decl)
{
  tree orig_arg;
  tree val;
  int is_type, requires_type, is_tmpl_type, requires_tmpl_type;

  if (parm == error_mark_node || error_operand_p (arg))
    return error_mark_node;

  /* Trivially convert placeholders. */
  if (TREE_CODE (arg) == WILDCARD_DECL)
    return convert_wildcard_argument (parm, arg);

  if (arg == any_targ_node)
    return arg;

  if (TREE_CODE (arg) == TREE_LIST
      && TREE_CODE (TREE_VALUE (arg)) == OFFSET_REF)
    {
      /* The template argument was the name of some
	 member function.  That's usually
	 invalid, but static members are OK.  In any
	 case, grab the underlying fields/functions
	 and issue an error later if required.  */
      TREE_TYPE (arg) = unknown_type_node;
    }

  orig_arg = arg;

  requires_tmpl_type = TREE_CODE (parm) == TEMPLATE_DECL;
  requires_type = (TREE_CODE (parm) == TYPE_DECL
		   || requires_tmpl_type);

  /* When determining whether an argument pack expansion is a template,
     look at the pattern.  */
  if (PACK_EXPANSION_P (arg))
    arg = PACK_EXPANSION_PATTERN (arg);

  /* Deal with an injected-class-name used as a template template arg.  */
  if (requires_tmpl_type && CLASS_TYPE_P (arg))
    {
      tree t = maybe_get_template_decl_from_type_decl (TYPE_NAME (arg));
      if (TREE_CODE (t) == TEMPLATE_DECL)
	{
	  if (cxx_dialect >= cxx11)
	    /* OK under DR 1004.  */;
	  else if (complain & tf_warning_or_error)
	    pedwarn (input_location, OPT_Wpedantic, "injected-class-name %qD"
		     " used as template template argument", TYPE_NAME (arg));
	  else if (flag_pedantic_errors)
	    t = arg;

	  arg = t;
	}
    }

  is_tmpl_type =
    ((TREE_CODE (arg) == TEMPLATE_DECL
      && TREE_CODE (DECL_TEMPLATE_RESULT (arg)) == TYPE_DECL)
     || (requires_tmpl_type && TREE_CODE (arg) == TYPE_ARGUMENT_PACK)
     || TREE_CODE (arg) == TEMPLATE_TEMPLATE_PARM
     || TREE_CODE (arg) == UNBOUND_CLASS_TEMPLATE);

  if (is_tmpl_type
      && (TREE_CODE (arg) == TEMPLATE_TEMPLATE_PARM
	  || TREE_CODE (arg) == UNBOUND_CLASS_TEMPLATE))
    arg = TYPE_STUB_DECL (arg);

  is_type = TYPE_P (arg) || is_tmpl_type;

  if (requires_type && ! is_type && TREE_CODE (arg) == SCOPE_REF
      && TREE_CODE (TREE_OPERAND (arg, 0)) == TEMPLATE_TYPE_PARM)
    {
      if (TREE_CODE (TREE_OPERAND (arg, 1)) == BIT_NOT_EXPR)
	{
	  if (complain & tf_error)
	    error ("invalid use of destructor %qE as a type", orig_arg);
	  return error_mark_node;
	}

      permerror (input_location,
		 "to refer to a type member of a template parameter, "
		 "use %<typename %E%>", orig_arg);

      orig_arg = make_typename_type (TREE_OPERAND (arg, 0),
				     TREE_OPERAND (arg, 1),
				     typename_type,
				     complain);
      arg = orig_arg;
      is_type = 1;
    }
  if (is_type != requires_type)
    {
      if (in_decl)
	{
	  if (complain & tf_error)
	    {
	      error ("type/value mismatch at argument %d in template "
		     "parameter list for %qD",
		     i + 1, in_decl);
	      if (is_type)
		{
		  /* The template argument is a type, but we're expecting
		     an expression.  */
		  inform (input_location,
			  "  expected a constant of type %qT, got %qT",
			  TREE_TYPE (parm),
			  (DECL_P (arg) ? DECL_NAME (arg) : orig_arg));
		  /* [temp.arg]/2: "In a template-argument, an ambiguity
		     between a type-id and an expression is resolved to a
		     type-id, regardless of the form of the corresponding
		     template-parameter."  So give the user a clue.  */
		  if (TREE_CODE (arg) == FUNCTION_TYPE)
		    inform (input_location, "  ambiguous template argument "
			    "for non-type template parameter is treated as "
			    "function type");
		}
	      else if (requires_tmpl_type)
		inform (input_location,
			"  expected a class template, got %qE", orig_arg);
	      else
		inform (input_location,
			"  expected a type, got %qE", orig_arg);
	    }
	}
      return error_mark_node;
    }
  if (is_tmpl_type ^ requires_tmpl_type)
    {
      if (in_decl && (complain & tf_error))
	{
	  error ("type/value mismatch at argument %d in template "
		 "parameter list for %qD",
		 i + 1, in_decl);
	  if (is_tmpl_type)
	    inform (input_location,
		    "  expected a type, got %qT", DECL_NAME (arg));
	  else
	    inform (input_location,
		    "  expected a class template, got %qT", orig_arg);
	}
      return error_mark_node;
    }

  if (template_parameter_pack_p (parm) && ARGUMENT_PACK_P (orig_arg))
    /* We already did the appropriate conversion when packing args.  */
    val = orig_arg;
  else if (is_type)
    {
      if (requires_tmpl_type)
	{
	  if (TREE_CODE (TREE_TYPE (arg)) == UNBOUND_CLASS_TEMPLATE)
	    /* The number of argument required is not known yet.
	       Just accept it for now.  */
	    val = orig_arg;
	  else
	    {
	      /* Strip alias templates that are equivalent to another
		 template.  */
	      arg = get_underlying_template (arg);

	      if (coerce_template_template_parms (parm, arg,
						  complain, in_decl,
						  args))
		{
		  val = arg;

		  /* TEMPLATE_TEMPLATE_PARM node is preferred over
		     TEMPLATE_DECL.  */
		  if (val != error_mark_node)
                    {
                      if (DECL_TEMPLATE_TEMPLATE_PARM_P (val))
                        val = TREE_TYPE (val);
		      if (TREE_CODE (orig_arg) == TYPE_PACK_EXPANSION)
			val = make_pack_expansion (val, complain);
                    }
		}
	      else
		{
		  if (in_decl && (complain & tf_error))
		    {
		      error ("type/value mismatch at argument %d in "
			     "template parameter list for %qD",
			     i + 1, in_decl);
		      inform (input_location,
			      "  expected a template of type %qD, got %qT",
			      parm, orig_arg);
		    }

		  val = error_mark_node;
		}

              // Check that the constraints are compatible before allowing the
              // substitution.
              if (val != error_mark_node)
                if (!is_compatible_template_arg (parm, arg))
                  {
		    if (in_decl && (complain & tf_error))
                      {
                        error ("constraint mismatch at argument %d in "
                               "template parameter list for %qD",
                               i + 1, in_decl);
                        inform (input_location, "  expected %qD but got %qD",
                                parm, arg);
                      }
		    val = error_mark_node;
                  }
	    }
	}
      else
	val = orig_arg;
      /* We only form one instance of each template specialization.
	 Therefore, if we use a non-canonical variant (i.e., a
	 typedef), any future messages referring to the type will use
	 the typedef, which is confusing if those future uses do not
	 themselves also use the typedef.  */
      if (TYPE_P (val))
	val = canonicalize_type_argument (val, complain);
    }
  else
    {
      tree t = TREE_TYPE (parm);

      if (TEMPLATE_PARM_LEVEL (get_template_parm_index (parm))
	  > TMPL_ARGS_DEPTH (args))
	/* We don't have enough levels of args to do any substitution.  This
	   can happen in the context of -fnew-ttp-matching.  */;
      else if (tree a = type_uses_auto (t))
	{
	  t = do_auto_deduction (t, arg, a, complain, adc_unify, args,
				 LOOKUP_IMPLICIT, /*tmpl=*/in_decl);
	  if (t == error_mark_node)
	    return error_mark_node;
	}
      else
	t = tsubst (t, args, complain, in_decl);

      /* Perform array-to-pointer and function-to-pointer conversion
	 as per [temp.param]/10.  */
      t = type_decays_to (t);

      if (invalid_nontype_parm_type_p (t, complain))
	return error_mark_node;

      /* Drop top-level cv-qualifiers on the substituted/deduced type of
	 this non-type template parameter, as per [temp.param]/6.  */
      t = cv_unqualified (t);

      if (t != TREE_TYPE (parm))
	t = canonicalize_type_argument (t, complain);

      if (!type_dependent_expression_p (orig_arg)
	  && !uses_template_parms (t))
	/* We used to call digest_init here.  However, digest_init
	   will report errors, which we don't want when complain
	   is zero.  More importantly, digest_init will try too
	   hard to convert things: for example, `0' should not be
	   converted to pointer type at this point according to
	   the standard.  Accepting this is not merely an
	   extension, since deciding whether or not these
	   conversions can occur is part of determining which
	   function template to call, or whether a given explicit
	   argument specification is valid.  */
	val = convert_nontype_argument (t, orig_arg, complain);
      else
	{
	  val = canonicalize_expr_argument (orig_arg, complain);
	  val = maybe_convert_nontype_argument (t, val);
	}


      if (val == NULL_TREE)
	val = error_mark_node;
      else if (val == error_mark_node && (complain & tf_error))
	error_at (cp_expr_loc_or_input_loc (orig_arg),
		  "could not convert template argument %qE from %qT to %qT",
		  orig_arg, TREE_TYPE (orig_arg), t);

      if (INDIRECT_REF_P (val))
        {
          /* Reject template arguments that are references to built-in
             functions with no library fallbacks.  */
          const_tree inner = TREE_OPERAND (val, 0);
	  const_tree innertype = TREE_TYPE (inner);
	  if (innertype
	      && TYPE_REF_P (innertype)
	      && TREE_CODE (TREE_TYPE (innertype)) == FUNCTION_TYPE
	      && TREE_OPERAND_LENGTH (inner) > 0
              && reject_gcc_builtin (TREE_OPERAND (inner, 0)))
              return error_mark_node;
        }

      if (TREE_CODE (val) == SCOPE_REF)
	{
	  /* Strip typedefs from the SCOPE_REF.  */
	  tree type = canonicalize_type_argument (TREE_TYPE (val), complain);
	  tree scope = canonicalize_type_argument (TREE_OPERAND (val, 0),
						   complain);
	  val = build_qualified_name (type, scope, TREE_OPERAND (val, 1),
				      QUALIFIED_NAME_IS_TEMPLATE (val));
	}
    }

  return val;
}

/* Coerces the remaining template arguments in INNER_ARGS (from
   ARG_IDX to the end) into the parameter pack at PARM_IDX in PARMS.
   Returns the coerced argument pack. PARM_IDX is the position of this
   parameter in the template parameter list. ARGS is the original
   template argument list.  */
static tree
coerce_template_parameter_pack (tree parms,
                                int parm_idx,
                                tree args,
                                tree inner_args,
                                int arg_idx,
                                tree new_args,
                                int* lost,
                                tree in_decl,
                                tsubst_flags_t complain)
{
  tree parm = TREE_VEC_ELT (parms, parm_idx);
  int nargs = inner_args ? NUM_TMPL_ARGS (inner_args) : 0;
  tree packed_args;
  tree argument_pack;
  tree packed_parms = NULL_TREE;

  if (arg_idx > nargs)
    arg_idx = nargs;

  if (tree packs = fixed_parameter_pack_p (TREE_VALUE (parm)))
    {
      /* When the template parameter is a non-type template parameter pack
         or template template parameter pack whose type or template
         parameters use parameter packs, we know exactly how many arguments
         we are looking for.  Build a vector of the instantiated decls for
         these template parameters in PACKED_PARMS.  */
      /* We can't use make_pack_expansion here because it would interpret a
	 _DECL as a use rather than a declaration.  */
      tree decl = TREE_VALUE (parm);
      tree exp = cxx_make_type (TYPE_PACK_EXPANSION);
      PACK_EXPANSION_PATTERN (exp) = decl;
      PACK_EXPANSION_PARAMETER_PACKS (exp) = packs;
      SET_TYPE_STRUCTURAL_EQUALITY (exp);

      TREE_VEC_LENGTH (args)--;
      packed_parms = tsubst_pack_expansion (exp, args, complain, decl);
      TREE_VEC_LENGTH (args)++;

      if (packed_parms == error_mark_node)
        return error_mark_node;

      /* If we're doing a partial instantiation of a member template,
         verify that all of the types used for the non-type
         template parameter pack are, in fact, valid for non-type
         template parameters.  */
      if (arg_idx < nargs
          && PACK_EXPANSION_P (TREE_VEC_ELT (inner_args, arg_idx)))
        {
          int j, len = TREE_VEC_LENGTH (packed_parms);
          for (j = 0; j < len; ++j)
            {
              tree t = TREE_VEC_ELT (packed_parms, j);
              if (TREE_CODE (t) == PARM_DECL
		  && invalid_nontype_parm_type_p (TREE_TYPE (t), complain))
                return error_mark_node;
            }
	  /* We don't know how many args we have yet, just
	     use the unconverted ones for now.  */
	  return NULL_TREE;
        }

      packed_args = make_tree_vec (TREE_VEC_LENGTH (packed_parms));
    }
  /* Check if we have a placeholder pack, which indicates we're
     in the context of a introduction list.  In that case we want
     to match this pack to the single placeholder.  */
  else if (arg_idx < nargs
           && TREE_CODE (TREE_VEC_ELT (inner_args, arg_idx)) == WILDCARD_DECL
           && WILDCARD_PACK_P (TREE_VEC_ELT (inner_args, arg_idx)))
    {
      nargs = arg_idx + 1;
      packed_args = make_tree_vec (1);
    }
  else
    packed_args = make_tree_vec (nargs - arg_idx);

  /* Convert the remaining arguments, which will be a part of the
     parameter pack "parm".  */
  int first_pack_arg = arg_idx;
  for (; arg_idx < nargs; ++arg_idx)
    {
      tree arg = TREE_VEC_ELT (inner_args, arg_idx);
      tree actual_parm = TREE_VALUE (parm);
      int pack_idx = arg_idx - first_pack_arg;

      if (packed_parms)
        {
	  /* Once we've packed as many args as we have types, stop.  */
	  if (pack_idx >= TREE_VEC_LENGTH (packed_parms))
	    break;
	  else if (PACK_EXPANSION_P (arg))
	    /* We don't know how many args we have yet, just
	       use the unconverted ones for now.  */
	    return NULL_TREE;
	  else
	    actual_parm = TREE_VEC_ELT (packed_parms, pack_idx);
        }

      if (arg == error_mark_node)
	{
	  if (complain & tf_error)
	    error ("template argument %d is invalid", arg_idx + 1);
	}
      else
	arg = convert_template_argument (actual_parm,
					 arg, new_args, complain, parm_idx,
					 in_decl);
      if (arg == error_mark_node)
        (*lost)++;
      TREE_VEC_ELT (packed_args, pack_idx) = arg;
    }

  if (arg_idx - first_pack_arg < TREE_VEC_LENGTH (packed_args)
      && TREE_VEC_LENGTH (packed_args) > 0)
    {
      if (complain & tf_error)
	error ("wrong number of template arguments (%d, should be %d)",
	       arg_idx - first_pack_arg, TREE_VEC_LENGTH (packed_args));
      return error_mark_node;
    }

  if (TREE_CODE (TREE_VALUE (parm)) == TYPE_DECL
      || TREE_CODE (TREE_VALUE (parm)) == TEMPLATE_DECL)
    argument_pack = cxx_make_type (TYPE_ARGUMENT_PACK);
  else
    {
      argument_pack = make_node (NONTYPE_ARGUMENT_PACK);
      TREE_CONSTANT (argument_pack) = 1;
    }

  ARGUMENT_PACK_ARGS (argument_pack) = packed_args;
  if (CHECKING_P)
    SET_NON_DEFAULT_TEMPLATE_ARGS_COUNT (packed_args,
					 TREE_VEC_LENGTH (packed_args));
  return argument_pack;
}

/* Returns the number of pack expansions in the template argument vector
   ARGS.  */

static int
pack_expansion_args_count (tree args)
{
  int i;
  int count = 0;
  if (args)
    for (i = 0; i < TREE_VEC_LENGTH (args); ++i)
      {
	tree elt = TREE_VEC_ELT (args, i);
	if (elt && PACK_EXPANSION_P (elt))
	  ++count;
      }
  return count;
}

/* Convert all template arguments to their appropriate types, and
   return a vector containing the innermost resulting template
   arguments.  If any error occurs, return error_mark_node. Error and
   warning messages are issued under control of COMPLAIN.

   If PARMS represents all template parameters levels, this function
   returns a vector of vectors representing all the resulting argument
   levels.  Note that in this case, only the innermost arguments are
   coerced because the outermost ones are supposed to have been coerced
   already.  Otherwise, if PARMS represents only (the innermost) vector
   of parameters, this function returns a vector containing just the
   innermost resulting arguments.

   If REQUIRE_ALL_ARGS is false, argument deduction will be performed
   for arguments not specified in ARGS.  If REQUIRE_ALL_ARGS is true,
   arguments not specified in ARGS must have default arguments which
   we'll use to fill in ARGS.  */

tree
coerce_template_parms (tree parms,
		       tree args,
		       tree in_decl,
		       tsubst_flags_t complain,
		       bool require_all_args /* = true */)
{
  int nparms, nargs, parm_idx, arg_idx, lost = 0;
  tree orig_inner_args;
  tree inner_args;

  /* When used as a boolean value, indicates whether this is a
     variadic template parameter list. Since it's an int, we can also
     subtract it from nparms to get the number of non-variadic
     parameters.  */
  int variadic_p = 0;
  int variadic_args_p = 0;
  int post_variadic_parms = 0;

  /* Adjustment to nparms for fixed parameter packs.  */
  int fixed_pack_adjust = 0;
  int fixed_packs = 0;
  int missing = 0;

  /* Likewise for parameters with default arguments.  */
  int default_p = 0;

  if (args == error_mark_node)
    return error_mark_node;

  bool return_full_args = false;
  if (TREE_CODE (parms) == TREE_LIST)
    {
      if (TMPL_PARMS_DEPTH (parms) > 1)
	{
	  gcc_assert (TMPL_PARMS_DEPTH (parms) == TMPL_ARGS_DEPTH (args));
	  return_full_args = true;
	}
      parms = INNERMOST_TEMPLATE_PARMS (parms);
    }

  nparms = TREE_VEC_LENGTH (parms);

  /* Determine if there are any parameter packs or default arguments.  */
  for (parm_idx = 0; parm_idx < nparms; ++parm_idx)
    {
      tree parm = TREE_VEC_ELT (parms, parm_idx);
      if (variadic_p)
	++post_variadic_parms;
      if (template_parameter_pack_p (TREE_VALUE (parm)))
	++variadic_p;
      if (TREE_PURPOSE (parm))
	++default_p;
    }

  inner_args = orig_inner_args = INNERMOST_TEMPLATE_ARGS (args);
  /* If there are no parameters that follow a parameter pack, we need to
     expand any argument packs so that we can deduce a parameter pack from
     some non-packed args followed by an argument pack, as in variadic85.C.
     If there are such parameters, we need to leave argument packs intact
     so the arguments are assigned properly.  This can happen when dealing
     with a nested class inside a partial specialization of a class
     template, as in variadic92.C, or when deducing a template parameter pack
     from a sub-declarator, as in variadic114.C.  */
  if (!post_variadic_parms)
    inner_args = expand_template_argument_pack (inner_args);

  /* Count any pack expansion args.  */
  variadic_args_p = pack_expansion_args_count (inner_args);

  nargs = inner_args ? NUM_TMPL_ARGS (inner_args) : 0;
  if ((nargs - variadic_args_p > nparms && !variadic_p)
      || (nargs < nparms - variadic_p
	  && require_all_args
	  && !variadic_args_p
	  && (TREE_VEC_ELT (parms, nargs) != error_mark_node
	      && !TREE_PURPOSE (TREE_VEC_ELT (parms, nargs)))))
    {
    bad_nargs:
      if (complain & tf_error)
	{
          if (variadic_p || default_p)
            {
              nparms -= variadic_p + default_p;
	      error ("wrong number of template arguments "
		     "(%d, should be at least %d)", nargs, nparms);
            }
	  else
	     error ("wrong number of template arguments "
		    "(%d, should be %d)", nargs, nparms);

	  if (in_decl)
	    inform (DECL_SOURCE_LOCATION (in_decl),
		    "provided for %qD", in_decl);
	}

      return error_mark_node;
    }
  /* We can't pass a pack expansion to a non-pack parameter of an alias
     template (DR 1430).  */
  else if (in_decl
	   && (DECL_ALIAS_TEMPLATE_P (in_decl)
	       || concept_definition_p (in_decl))
	   && variadic_args_p
	   && nargs - variadic_args_p < nparms - variadic_p)
    {
      if (complain & tf_error)
	{
	  for (int i = 0; i < TREE_VEC_LENGTH (inner_args); ++i)
	    {
	      tree arg = TREE_VEC_ELT (inner_args, i);
	      tree parm = TREE_VALUE (TREE_VEC_ELT (parms, i));

	      if (PACK_EXPANSION_P (arg)
		  && !template_parameter_pack_p (parm))
		{
		  if (DECL_ALIAS_TEMPLATE_P (in_decl))
		    error_at (location_of (arg),
			      "pack expansion argument for non-pack parameter "
			      "%qD of alias template %qD", parm, in_decl);
		  else
		    error_at (location_of (arg),
			      "pack expansion argument for non-pack parameter "
			      "%qD of concept %qD", parm, in_decl);
		  inform (DECL_SOURCE_LOCATION (parm), "declared here");
		  goto found;
		}
	    }
	  gcc_unreachable ();
	found:;
	}
      return error_mark_node;
    }

  /* We need to evaluate the template arguments, even though this
     template-id may be nested within a "sizeof".  */
  cp_evaluated ev;

  tree new_args = add_outermost_template_args (args, make_tree_vec (nparms));
  tree& new_inner_args = TMPL_ARGS_LEVEL (new_args, TMPL_ARGS_DEPTH (new_args));
  int pack_adjust = 0;
  for (parm_idx = 0, arg_idx = 0; parm_idx < nparms; parm_idx++, arg_idx++)
    {
      tree arg;
      tree parm;

      /* Get the Ith template parameter.  */
      parm = TREE_VEC_ELT (parms, parm_idx);

      if (parm == error_mark_node)
	{
	  TREE_VEC_ELT (new_inner_args, arg_idx) = error_mark_node;
	  continue;
	}

      /* Calculate the next argument.  */
      if (arg_idx < nargs)
	arg = TREE_VEC_ELT (inner_args, arg_idx);
      else
	arg = NULL_TREE;

      if (template_parameter_pack_p (TREE_VALUE (parm))
	  && (arg || require_all_args || !(complain & tf_partial))
	  && !(arg && ARGUMENT_PACK_P (arg)))
        {
	  /* Some arguments will be placed in the
	     template parameter pack PARM.  */
	  arg = coerce_template_parameter_pack (parms, parm_idx, args,
						inner_args, arg_idx,
						new_args, &lost,
						in_decl, complain);

	  if (arg == NULL_TREE)
	    {
	      /* We don't know how many args we have yet, just use the
		 unconverted (and still packed) ones for now.  */
	      new_inner_args = orig_inner_args;
	      arg_idx = nargs;
	      break;
	    }

          TREE_VEC_ELT (new_inner_args, parm_idx) = arg;

          /* Store this argument.  */
          if (arg == error_mark_node)
	    {
	      lost++;
	      /* We are done with all of the arguments.  */
	      arg_idx = nargs;
	      break;
	    }
	  else
	    {
	      pack_adjust = TREE_VEC_LENGTH (ARGUMENT_PACK_ARGS (arg)) - 1;
	      arg_idx += pack_adjust;
	      if (fixed_parameter_pack_p (TREE_VALUE (parm)))
		{
		  ++fixed_packs;
		  fixed_pack_adjust += pack_adjust;
		}
	    }

          continue;
        }
      else if (arg)
	{
          if (PACK_EXPANSION_P (arg))
            {
	      /* "If every valid specialization of a variadic template
		 requires an empty template parameter pack, the template is
		 ill-formed, no diagnostic required."  So check that the
		 pattern works with this parameter.  */
	      tree pattern = PACK_EXPANSION_PATTERN (arg);
	      tree conv = convert_template_argument (TREE_VALUE (parm),
						     pattern, new_args,
						     complain, parm_idx,
						     in_decl);
	      if (conv == error_mark_node)
		{
		  if (complain & tf_error)
		    inform (input_location, "so any instantiation with a "
			    "non-empty parameter pack would be ill-formed");
		  ++lost;
		}
	      else if (TYPE_P (conv) && !TYPE_P (pattern))
		/* Recover from missing typename.  */
		TREE_VEC_ELT (inner_args, arg_idx)
		  = make_pack_expansion (conv, complain);

              /* We don't know how many args we have yet, just
                 use the unconverted ones for now.  */
              new_inner_args = inner_args;
	      arg_idx = nargs;
              break;
            }
        }
      else if (require_all_args)
	{
	  /* There must be a default arg in this case.  */
	  arg = tsubst_template_arg (TREE_PURPOSE (parm), new_args,
				     complain, in_decl);
	  /* The position of the first default template argument,
	     is also the number of non-defaulted arguments in NEW_INNER_ARGS.
	     Record that.  */
	  if (!NON_DEFAULT_TEMPLATE_ARGS_COUNT (new_inner_args))
	    SET_NON_DEFAULT_TEMPLATE_ARGS_COUNT (new_inner_args,
						 arg_idx - pack_adjust);
	}
      else
	break;

      if (arg == error_mark_node)
	{
	  if (complain & tf_error)
	    error ("template argument %d is invalid", arg_idx + 1);
	}
      else if (!arg)
	{
	  /* This can occur if there was an error in the template
	     parameter list itself (which we would already have
	     reported) that we are trying to recover from, e.g., a class
	     template with a parameter list such as
	     template<typename..., typename> (cpp0x/variadic150.C).  */
	  ++lost;

	  /* This can also happen with a fixed parameter pack (71834).  */
	  if (arg_idx >= nargs)
	    ++missing;
	}
      else
	arg = convert_template_argument (TREE_VALUE (parm),
					 arg, new_args, complain,
                                         parm_idx, in_decl);

      if (arg == error_mark_node)
	lost++;

      TREE_VEC_ELT (new_inner_args, arg_idx - pack_adjust) = arg;
    }

  if (missing || arg_idx < nargs - variadic_args_p)
    {
      /* If we had fixed parameter packs, we didn't know how many arguments we
	 actually needed earlier; now we do.  */
      nparms += fixed_pack_adjust;
      variadic_p -= fixed_packs;
      goto bad_nargs;
    }

  if (arg_idx < nargs)
    {
      /* We had some pack expansion arguments that will only work if the packs
	 are empty, but wait until instantiation time to complain.
	 See variadic-ttp3.C.  */

      /* Except that we can't provide empty packs to alias templates or
         concepts when there are no corresponding parameters. Basically,
         we can get here with this:

             template<typename T> concept C = true;

             template<typename... Args>
	       requires C<Args...>
             void f();

         When parsing C<Args...>, we try to form a concept check of
         C<?, Args...>. Without the extra check for substituting an empty
         pack past the last parameter, we can accept the check as valid.

         FIXME: This may be valid for alias templates (but I doubt it).

         FIXME: The error could be better also.   */
      if (in_decl && concept_definition_p (in_decl))
	{
	  if (complain & tf_error)
	    error_at (location_of (TREE_VEC_ELT (args, arg_idx)),
		      "too many arguments");
	  return error_mark_node;
	}

      int len = nparms + (nargs - arg_idx);
      tree args = make_tree_vec (len);
      int i = 0;
      for (; i < nparms; ++i)
	TREE_VEC_ELT (args, i) = TREE_VEC_ELT (new_inner_args, i);
      for (; i < len; ++i, ++arg_idx)
	TREE_VEC_ELT (args, i) = TREE_VEC_ELT (inner_args,
					       arg_idx - pack_adjust);
      new_inner_args = args;
    }

  if (lost)
    {
      gcc_assert (!(complain & tf_error) || seen_error ());
      return error_mark_node;
    }

  if (CHECKING_P && !NON_DEFAULT_TEMPLATE_ARGS_COUNT (new_inner_args))
    SET_NON_DEFAULT_TEMPLATE_ARGS_COUNT (new_inner_args,
					 TREE_VEC_LENGTH (new_inner_args));

  return return_full_args ? new_args : new_inner_args;
}

/* Returns true if T is a wrapper to make a C++20 template parameter
   object const.  */

static bool
class_nttp_const_wrapper_p (tree t)
{
  if (cxx_dialect < cxx20)
    return false;
  return (TREE_CODE (t) == VIEW_CONVERT_EXPR
	  && CP_TYPE_CONST_P (TREE_TYPE (t))
	  && TREE_CODE (TREE_OPERAND (t, 0)) == TEMPLATE_PARM_INDEX);
}

/* Returns 1 if template args OT and NT are equivalent.  */

int
template_args_equal (tree ot, tree nt, bool partial_order /* = false */)
{
  if (nt == ot)
    return 1;
  if (nt == NULL_TREE || ot == NULL_TREE)
    return false;
  if (nt == any_targ_node || ot == any_targ_node)
    return true;

  if (class_nttp_const_wrapper_p (nt))
    nt = TREE_OPERAND (nt, 0);
  if (class_nttp_const_wrapper_p (ot))
    ot = TREE_OPERAND (ot, 0);

  /* DR 1558: Don't treat an alias template specialization with dependent
     arguments as equivalent to its underlying type when used as a template
     argument; we need them to be distinct so that we substitute into the
     specialization arguments at instantiation time.  And aliases can't be
     equivalent without being ==, so we don't need to look any deeper.

     During partial ordering, however, we need to treat them normally so we can
     order uses of the same alias with different cv-qualification (79960).  */
  auto cso = make_temp_override (comparing_dependent_aliases);
  if (!partial_order)
    ++comparing_dependent_aliases;

  if (TREE_CODE (nt) == TREE_VEC || TREE_CODE (ot) == TREE_VEC)
    /* For member templates */
    return TREE_CODE (ot) == TREE_CODE (nt) && comp_template_args (ot, nt);
  else if (PACK_EXPANSION_P (ot) || PACK_EXPANSION_P (nt))
    return (PACK_EXPANSION_P (ot) && PACK_EXPANSION_P (nt)
	    && template_args_equal (PACK_EXPANSION_PATTERN (ot),
				    PACK_EXPANSION_PATTERN (nt))
	    && template_args_equal (PACK_EXPANSION_EXTRA_ARGS (ot),
				    PACK_EXPANSION_EXTRA_ARGS (nt)));
  else if (ARGUMENT_PACK_P (ot) || ARGUMENT_PACK_P (nt))
    return cp_tree_equal (ot, nt);
  else if (TREE_CODE (ot) == ARGUMENT_PACK_SELECT)
    gcc_unreachable ();
  else if (TYPE_P (nt) || TYPE_P (ot))
    {
      if (!(TYPE_P (nt) && TYPE_P (ot)))
	return false;
      return same_type_p (ot, nt);
    }
  else
    {
      /* Try to treat a template non-type argument that has been converted
	 to the parameter type as equivalent to one that hasn't yet.  */
      for (enum tree_code code1 = TREE_CODE (ot);
	   CONVERT_EXPR_CODE_P (code1)
	     || code1 == NON_LVALUE_EXPR;
	   code1 = TREE_CODE (ot))
	ot = TREE_OPERAND (ot, 0);

      for (enum tree_code code2 = TREE_CODE (nt);
	   CONVERT_EXPR_CODE_P (code2)
	     || code2 == NON_LVALUE_EXPR;
	   code2 = TREE_CODE (nt))
	nt = TREE_OPERAND (nt, 0);

      return cp_tree_equal (ot, nt);
    }
}

/* Returns true iff the OLDARGS and NEWARGS are in fact identical sets of
   template arguments.  Returns false otherwise, and updates OLDARG_PTR and
   NEWARG_PTR with the offending arguments if they are non-NULL.  */

bool
comp_template_args (tree oldargs, tree newargs,
		    tree *oldarg_ptr /* = NULL */, tree *newarg_ptr /* = NULL */,
		    bool partial_order /* = false */)
{
  if (oldargs == newargs)
    return true;

  if (!oldargs || !newargs)
    return false;

  if (TREE_VEC_LENGTH (oldargs) != TREE_VEC_LENGTH (newargs))
    return false;

  for (int i = 0; i < TREE_VEC_LENGTH (oldargs); ++i)
    {
      tree nt = TREE_VEC_ELT (newargs, i);
      tree ot = TREE_VEC_ELT (oldargs, i);

      if (! template_args_equal (ot, nt, partial_order))
	{
	  if (oldarg_ptr != NULL)
	    *oldarg_ptr = ot;
	  if (newarg_ptr != NULL)
	    *newarg_ptr = nt;
	  return false;
	}
    }
  return true;
}

inline bool
comp_template_args_porder (tree oargs, tree nargs)
{
  return comp_template_args (oargs, nargs, NULL, NULL, true);
}

/* Implement a freelist interface for objects of type T.

   Head is a separate object, rather than a regular member, so that we
   can define it as a GTY deletable pointer, which is highly
   desirable.  A data member could be declared that way, but then the
   containing object would implicitly get GTY((user)), which would
   prevent us from instantiating freelists as global objects.
   Although this way we can create freelist global objects, they're
   such thin wrappers that instantiating temporaries at every use
   loses nothing and saves permanent storage for the freelist object.

   Member functions next, anew, poison and reinit have default
   implementations that work for most of the types we're interested
   in, but if they don't work for some type, they should be explicitly
   specialized.  See the comments before them for requirements, and
   the example specializations for the tree_list_freelist.  */
template <typename T>
class freelist
{
  /* Return the next object in a chain.  We could just do type
     punning, but if we access the object with its underlying type, we
     avoid strict-aliasing trouble.  This needs only work between
     poison and reinit.  */
  static T *&next (T *obj) { return obj->next; }

  /* Return a newly allocated, uninitialized or minimally-initialized
     object of type T.  Any initialization performed by anew should
     either remain across the life of the object and the execution of
     poison, or be redone by reinit.  */
  static T *anew () { return ggc_alloc<T> (); }

  /* Optionally scribble all over the bits holding the object, so that
     they become (mostly?) uninitialized memory.  This is called while
     preparing to make the object part of the free list.  */
  static void poison (T *obj) {
    T *p ATTRIBUTE_UNUSED = obj;
    T **q ATTRIBUTE_UNUSED = &next (obj);

#ifdef ENABLE_GC_CHECKING
    /* Poison the data, to indicate the data is garbage.  */
    VALGRIND_DISCARD (VALGRIND_MAKE_MEM_UNDEFINED (p, sizeof (*p)));
    memset (p, 0xa5, sizeof (*p));
#endif
    /* Let valgrind know the object is free.  */
    VALGRIND_DISCARD (VALGRIND_MAKE_MEM_NOACCESS (p, sizeof (*p)));

    /* Let valgrind know the next portion of the object is available,
       but uninitialized.  */
    VALGRIND_DISCARD (VALGRIND_MAKE_MEM_UNDEFINED (q, sizeof (*q)));
  }

  /* Bring an object that underwent at least one lifecycle after anew
     and before the most recent free and poison, back to a usable
     state, reinitializing whatever is needed for it to be
     functionally equivalent to an object just allocated and returned
     by anew.  This may poison or clear the next field, used by
     freelist housekeeping after poison was called.  */
  static void reinit (T *obj) {
    T **q ATTRIBUTE_UNUSED = &next (obj);

#ifdef ENABLE_GC_CHECKING
    memset (q, 0xa5, sizeof (*q));
#endif
    /* Let valgrind know the entire object is available, but
       uninitialized.  */
    VALGRIND_DISCARD (VALGRIND_MAKE_MEM_UNDEFINED (obj, sizeof (*obj)));
  }

  /* Reference a GTY-deletable pointer that points to the first object
     in the free list proper.  */
  T *&head;
public:
  /* Construct a freelist object chaining objects off of HEAD.  */
  freelist (T *&head) : head(head) {}

  /* Add OBJ to the free object list.  The former head becomes OBJ's
     successor.  */
  void free (T *obj)
  {
    poison (obj);
    next (obj) = head;
    head = obj;
  }

  /* Take an object from the free list, if one is available, or
     allocate a new one.  Objects taken from the free list should be
     regarded as filled with garbage, except for bits that are
     configured to be preserved across free and alloc.  */
  T *alloc ()
  {
    if (head)
      {
	T *obj = head;
	head = next (head);
	reinit (obj);
	return obj;
      }
    else
      return anew ();
  }
};

/* Explicitly specialize the interfaces for freelist<tree_node>: we
   want to allocate a TREE_LIST using the usual interface, and ensure
   TREE_CHAIN remains functional.  Alas, we have to duplicate a bit of
   build_tree_list logic in reinit, so this could go out of sync.  */
template <>
inline tree &
freelist<tree_node>::next (tree obj)
{
  return TREE_CHAIN (obj);
}
template <>
inline tree
freelist<tree_node>::anew ()
{
  return build_tree_list (NULL, NULL);
}
template <>
inline void
freelist<tree_node>::poison (tree obj ATTRIBUTE_UNUSED)
{
  int size ATTRIBUTE_UNUSED = sizeof (tree_list);
  tree p ATTRIBUTE_UNUSED = obj;
  tree_base *b ATTRIBUTE_UNUSED = &obj->base;
  tree *q ATTRIBUTE_UNUSED = &next (obj);

#ifdef ENABLE_GC_CHECKING
  gcc_checking_assert (TREE_CODE (obj) == TREE_LIST);

  /* Poison the data, to indicate the data is garbage.  */
  VALGRIND_DISCARD (VALGRIND_MAKE_MEM_UNDEFINED (p, size));
  memset (p, 0xa5, size);
#endif
  /* Let valgrind know the object is free.  */
  VALGRIND_DISCARD (VALGRIND_MAKE_MEM_NOACCESS (p, size));
  /* But we still want to use the TREE_CODE and TREE_CHAIN parts.  */
  VALGRIND_DISCARD (VALGRIND_MAKE_MEM_DEFINED (b, sizeof (*b)));
  VALGRIND_DISCARD (VALGRIND_MAKE_MEM_UNDEFINED (q, sizeof (*q)));

#ifdef ENABLE_GC_CHECKING
  VALGRIND_DISCARD (VALGRIND_MAKE_MEM_UNDEFINED (b, sizeof (*b)));
  /* Keep TREE_CHAIN functional.  */
  TREE_SET_CODE (obj, TREE_LIST);
#else
  VALGRIND_DISCARD (VALGRIND_MAKE_MEM_DEFINED (b, sizeof (*b)));
#endif
}
template <>
inline void
freelist<tree_node>::reinit (tree obj ATTRIBUTE_UNUSED)
{
  tree_base *b ATTRIBUTE_UNUSED = &obj->base;

#ifdef ENABLE_GC_CHECKING
  gcc_checking_assert (TREE_CODE (obj) == TREE_LIST);
  VALGRIND_DISCARD (VALGRIND_MAKE_MEM_UNDEFINED (obj, sizeof (tree_list)));
  memset (obj, 0, sizeof (tree_list));
#endif

  /* Let valgrind know the entire object is available, but
     uninitialized.  */
  VALGRIND_DISCARD (VALGRIND_MAKE_MEM_UNDEFINED (obj, sizeof (tree_list)));

#ifdef ENABLE_GC_CHECKING
  TREE_SET_CODE (obj, TREE_LIST);
#else
  VALGRIND_DISCARD (VALGRIND_MAKE_MEM_DEFINED (b, sizeof (*b)));
#endif
}

/* Point to the first object in the TREE_LIST freelist.  */
static GTY((deletable)) tree tree_list_freelist_head;
/* Return the/an actual TREE_LIST freelist.  */
static inline freelist<tree_node>
tree_list_freelist ()
{
  return tree_list_freelist_head;
}

/* Point to the first object in the tinst_level freelist.  */
static GTY((deletable)) tinst_level *tinst_level_freelist_head;
/* Return the/an actual tinst_level freelist.  */
static inline freelist<tinst_level>
tinst_level_freelist ()
{
  return tinst_level_freelist_head;
}

/* Point to the first object in the pending_template freelist.  */
static GTY((deletable)) pending_template *pending_template_freelist_head;
/* Return the/an actual pending_template freelist.  */
static inline freelist<pending_template>
pending_template_freelist ()
{
  return pending_template_freelist_head;
}

/* Build the TREE_LIST object out of a split list, store it
   permanently, and return it.  */
tree
tinst_level::to_list ()
{
  gcc_assert (split_list_p ());
  tree ret = tree_list_freelist ().alloc ();
  TREE_PURPOSE (ret) = tldcl;
  TREE_VALUE (ret) = targs;
  tldcl = ret;
  targs = NULL;
  gcc_assert (tree_list_p ());
  return ret;
}

const unsigned short tinst_level::refcount_infinity;

/* Increment OBJ's refcount unless it is already infinite.  */
static tinst_level *
inc_refcount_use (tinst_level *obj)
{
  if (obj && obj->refcount != tinst_level::refcount_infinity)
    ++obj->refcount;
  return obj;
}

/* Release storage for OBJ and node, if it's a TREE_LIST.  */
void
tinst_level::free (tinst_level *obj)
{
  if (obj->tree_list_p ())
    tree_list_freelist ().free (obj->get_node ());
  tinst_level_freelist ().free (obj);
}

/* Decrement OBJ's refcount if not infinite.  If it reaches zero, release
   OBJ's DECL and OBJ, and start over with the tinst_level object that
   used to be referenced by OBJ's NEXT.  */
static void
dec_refcount_use (tinst_level *obj)
{
  while (obj
	 && obj->refcount != tinst_level::refcount_infinity
	 && !--obj->refcount)
    {
      tinst_level *next = obj->next;
      tinst_level::free (obj);
      obj = next;
    }
}

/* Modify PTR so that it points to OBJ, adjusting the refcounts of OBJ
   and of the former PTR.  Omitting the second argument is equivalent
   to passing (T*)NULL; this is allowed because passing the
   zero-valued integral constant NULL confuses type deduction and/or
   overload resolution.  */
template <typename T>
static void
set_refcount_ptr (T *& ptr, T *obj = NULL)
{
  T *save = ptr;
  ptr = inc_refcount_use (obj);
  dec_refcount_use (save);
}

static void
add_pending_template (tree d)
{
  tree ti = (TYPE_P (d)
	     ? CLASSTYPE_TEMPLATE_INFO (d)
	     : DECL_TEMPLATE_INFO (d));
  struct pending_template *pt;
  int level;

  if (TI_PENDING_TEMPLATE_FLAG (ti))
    return;

  /* We are called both from instantiate_decl, where we've already had a
     tinst_level pushed, and instantiate_template, where we haven't.
     Compensate.  */
  gcc_assert (TREE_CODE (d) != TREE_LIST);
  level = !current_tinst_level
    || current_tinst_level->maybe_get_node () != d;

  if (level)
    push_tinst_level (d);

  pt = pending_template_freelist ().alloc ();
  pt->next = NULL;
  pt->tinst = NULL;
  set_refcount_ptr (pt->tinst, current_tinst_level);
  if (last_pending_template)
    last_pending_template->next = pt;
  else
    pending_templates = pt;

  last_pending_template = pt;

  TI_PENDING_TEMPLATE_FLAG (ti) = 1;

  if (level)
    pop_tinst_level ();
}


/* Return a TEMPLATE_ID_EXPR corresponding to the indicated FNS and
   ARGLIST.  Valid choices for FNS are given in the cp-tree.def
   documentation for TEMPLATE_ID_EXPR.  */

tree
lookup_template_function (tree fns, tree arglist)
{
  if (fns == error_mark_node || arglist == error_mark_node)
    return error_mark_node;

  gcc_assert (!arglist || TREE_CODE (arglist) == TREE_VEC);

  if (!is_overloaded_fn (fns) && !identifier_p (fns))
    {
      error ("%q#D is not a function template", fns);
      return error_mark_node;
    }

  if (BASELINK_P (fns))
    {
      fns = copy_node (fns);
      BASELINK_FUNCTIONS (fns) = build2 (TEMPLATE_ID_EXPR,
					 unknown_type_node,
					 BASELINK_FUNCTIONS (fns),
					 arglist);
      return fns;
    }

  return build2 (TEMPLATE_ID_EXPR, unknown_type_node, fns, arglist);
}

/* Within the scope of a template class S<T>, the name S gets bound
   (in build_self_reference) to a TYPE_DECL for the class, not a
   TEMPLATE_DECL.  If DECL is a TYPE_DECL for current_class_type,
   or one of its enclosing classes, and that type is a template,
   return the associated TEMPLATE_DECL.  Otherwise, the original
   DECL is returned.

   Also handle the case when DECL is a TREE_LIST of ambiguous
   injected-class-names from different bases.  */

tree
maybe_get_template_decl_from_type_decl (tree decl)
{
  if (decl == NULL_TREE)
    return decl;

  /* DR 176: A lookup that finds an injected-class-name (10.2
     [class.member.lookup]) can result in an ambiguity in certain cases
     (for example, if it is found in more than one base class). If all of
     the injected-class-names that are found refer to specializations of
     the same class template, and if the name is followed by a
     template-argument-list, the reference refers to the class template
     itself and not a specialization thereof, and is not ambiguous.  */
  if (TREE_CODE (decl) == TREE_LIST)
    {
      tree t, tmpl = NULL_TREE;
      for (t = decl; t; t = TREE_CHAIN (t))
	{
	  tree elt = maybe_get_template_decl_from_type_decl (TREE_VALUE (t));
	  if (!tmpl)
	    tmpl = elt;
	  else if (tmpl != elt)
	    break;
	}
      if (tmpl && t == NULL_TREE)
	return tmpl;
      else
	return decl;
    }

  return (decl != NULL_TREE
	  && DECL_SELF_REFERENCE_P (decl)
	  && CLASSTYPE_TEMPLATE_INFO (TREE_TYPE (decl)))
    ? CLASSTYPE_TI_TEMPLATE (TREE_TYPE (decl)) : decl;
}

/* Given an IDENTIFIER_NODE (or type TEMPLATE_DECL) and a chain of
   parameters, find the desired type.

   D1 is the PTYPENAME terminal, and ARGLIST is the list of arguments.

   IN_DECL, if non-NULL, is the template declaration we are trying to
   instantiate.

   If ENTERING_SCOPE is nonzero, we are about to enter the scope of
   the class we are looking up.

   Issue error and warning messages under control of COMPLAIN.

   If the template class is really a local class in a template
   function, then the FUNCTION_CONTEXT is the function in which it is
   being instantiated.

   ??? Note that this function is currently called *twice* for each
   template-id: the first time from the parser, while creating the
   incomplete type (finish_template_type), and the second type during the
   real instantiation (instantiate_template_class). This is surely something
   that we want to avoid. It also causes some problems with argument
   coercion (see convert_nontype_argument for more information on this).  */

tree
lookup_template_class (tree d1, tree arglist, tree in_decl, tree context,
		       int entering_scope, tsubst_flags_t complain)
{
  auto_timevar tv (TV_TEMPLATE_INST);

  tree templ = NULL_TREE, parmlist;
  tree t;
  spec_entry **slot;
  spec_entry *entry;
  spec_entry elt;
  hashval_t hash;

  if (identifier_p (d1))
    {
      tree value = innermost_non_namespace_value (d1);
      if (value && DECL_TEMPLATE_TEMPLATE_PARM_P (value))
	templ = value;
      else
	{
	  if (context)
	    push_decl_namespace (context);
	  templ = lookup_name (d1);
	  templ = maybe_get_template_decl_from_type_decl (templ);
	  if (context)
	    pop_decl_namespace ();
	}
    }
  else if (TREE_CODE (d1) == TYPE_DECL && MAYBE_CLASS_TYPE_P (TREE_TYPE (d1)))
    {
      tree type = TREE_TYPE (d1);

      /* If we are declaring a constructor, say A<T>::A<T>, we will get
	 an implicit typename for the second A.  Deal with it.  */
      if (TREE_CODE (type) == TYPENAME_TYPE && TREE_TYPE (type))
	type = TREE_TYPE (type);

      if (CLASSTYPE_TEMPLATE_INFO (type))
	{
	  templ = CLASSTYPE_TI_TEMPLATE (type);
	  d1 = DECL_NAME (templ);
	}
    }
  else if (TREE_CODE (d1) == ENUMERAL_TYPE
	   || (TYPE_P (d1) && MAYBE_CLASS_TYPE_P (d1)))
    {
      templ = TYPE_TI_TEMPLATE (d1);
      d1 = DECL_NAME (templ);
    }
  else if (DECL_TYPE_TEMPLATE_P (d1))
    {
      templ = d1;
      d1 = DECL_NAME (templ);
    }
  else if (DECL_TEMPLATE_TEMPLATE_PARM_P (d1))
    {
      templ = d1;
      d1 = DECL_NAME (templ);
    }

  /* Issue an error message if we didn't find a template.  */
  if (! templ)
    {
      if (complain & tf_error)
	error ("%qT is not a template", d1);
      return error_mark_node;
    }

  if (TREE_CODE (templ) != TEMPLATE_DECL
	 /* Make sure it's a user visible template, if it was named by
	    the user.  */
      || ((complain & tf_user) && !DECL_TEMPLATE_PARM_P (templ)
	  && !PRIMARY_TEMPLATE_P (templ)))
    {
      if (complain & tf_error)
	{
	  error ("non-template type %qT used as a template", d1);
	  if (in_decl)
	    error ("for template declaration %q+D", in_decl);
	}
      return error_mark_node;
    }

  complain &= ~tf_user;

  /* An alias that just changes the name of a template is equivalent to the
     other template, so if any of the arguments are pack expansions, strip
     the alias to avoid problems with a pack expansion passed to a non-pack
     alias template parameter (DR 1430).  */
  if (pack_expansion_args_count (INNERMOST_TEMPLATE_ARGS (arglist)))
    templ = get_underlying_template (templ);

  if (DECL_TEMPLATE_TEMPLATE_PARM_P (templ))
    {
      tree parm;
      tree arglist2 = coerce_template_args_for_ttp (templ, arglist, complain);
      if (arglist2 == error_mark_node
	  || (!uses_template_parms (arglist2)
	      && check_instantiated_args (templ, arglist2, complain)))
	return error_mark_node;

      parm = bind_template_template_parm (TREE_TYPE (templ), arglist2);
      return parm;
    }
  else
    {
      tree template_type = TREE_TYPE (templ);
      tree gen_tmpl;
      tree type_decl;
      tree found = NULL_TREE;
      int arg_depth;
      int parm_depth;
      int is_dependent_type;
      int use_partial_inst_tmpl = false;

      if (template_type == error_mark_node)
	/* An error occurred while building the template TEMPL, and a
	   diagnostic has most certainly been emitted for that
	   already.  Let's propagate that error.  */
	return error_mark_node;

      gen_tmpl = most_general_template (templ);
      if (modules_p ())
	lazy_load_pendings (gen_tmpl);

      parmlist = DECL_TEMPLATE_PARMS (gen_tmpl);
      parm_depth = TMPL_PARMS_DEPTH (parmlist);
      arg_depth = TMPL_ARGS_DEPTH (arglist);

      if (arg_depth == 1 && parm_depth > 1)
	{
	  /* We've been given an incomplete set of template arguments.
	     For example, given:

	       template <class T> struct S1 {
		 template <class U> struct S2 {};
		 template <class U> struct S2<U*> {};
		};

	     we will be called with an ARGLIST of `U*', but the
	     TEMPLATE will be `template <class T> template
	     <class U> struct S1<T>::S2'.  We must fill in the missing
	     arguments.  */
	  tree ti = TYPE_TEMPLATE_INFO_MAYBE_ALIAS (TREE_TYPE (templ));
	  arglist = add_outermost_template_args (TI_ARGS (ti), arglist);
	  arg_depth = TMPL_ARGS_DEPTH (arglist);
	}

      /* Now we should have enough arguments.  */
      gcc_assert (parm_depth == arg_depth);

      /* From here on, we're only interested in the most general
	 template.  */

      /* Shortcut looking up the current class scope again.  */
      for (tree cur = current_nonlambda_class_type ();
	   cur != NULL_TREE;
	   cur = get_containing_scope (cur))
	{
	  if (!CLASS_TYPE_P (cur))
	    continue;

	  tree ti = CLASSTYPE_TEMPLATE_INFO (cur);
	  if (!ti || arg_depth > TMPL_ARGS_DEPTH (TI_ARGS (ti)))
	    break;

	  if (gen_tmpl == most_general_template (TI_TEMPLATE (ti))
	      && comp_template_args (arglist, TI_ARGS (ti)))
	    return cur;
	}

      /* Calculate the BOUND_ARGS.  These will be the args that are
	 actually tsubst'd into the definition to create the
	 instantiation.  */
      if (PRIMARY_TEMPLATE_P (gen_tmpl))
	arglist = coerce_template_parms (parmlist, arglist, gen_tmpl, complain);

      if (arglist == error_mark_node)
	/* We were unable to bind the arguments.  */
	return error_mark_node;

      /* In the scope of a template class, explicit references to the
	 template class refer to the type of the template, not any
	 instantiation of it.  For example, in:

	   template <class T> class C { void f(C<T>); }

	 the `C<T>' is just the same as `C'.  Outside of the
	 class, however, such a reference is an instantiation.  */
      if (entering_scope
	  || !PRIMARY_TEMPLATE_P (gen_tmpl)
	  || currently_open_class (template_type))
	{
	  tree tinfo = TYPE_TEMPLATE_INFO (template_type);

	  if (tinfo && comp_template_args (TI_ARGS (tinfo), arglist))
	    return template_type;
	}

      /* If we already have this specialization, return it.  */
      elt.tmpl = gen_tmpl;
      elt.args = arglist;
      elt.spec = NULL_TREE;
      hash = spec_hasher::hash (&elt);
      entry = type_specializations->find_with_hash (&elt, hash);

      if (entry)
	return entry->spec;

      /* If the template's constraints are not satisfied,
         then we cannot form a valid type.

         Note that the check is deferred until after the hash
         lookup. This prevents redundant checks on previously
         instantiated specializations. */
      if (flag_concepts
	  && !DECL_ALIAS_TEMPLATE_P (gen_tmpl)
	  && !constraints_satisfied_p (gen_tmpl, arglist))
        {
          if (complain & tf_error)
            {
	      auto_diagnostic_group d;
              error ("template constraint failure for %qD", gen_tmpl);
              diagnose_constraints (input_location, gen_tmpl, arglist);
            }
          return error_mark_node;
        }

      is_dependent_type = uses_template_parms (arglist);

      /* If the deduced arguments are invalid, then the binding
	 failed.  */
      if (!is_dependent_type
	  && check_instantiated_args (gen_tmpl,
				      INNERMOST_TEMPLATE_ARGS (arglist),
				      complain))
	return error_mark_node;

      if (!is_dependent_type
	  && !PRIMARY_TEMPLATE_P (gen_tmpl)
	  && !LAMBDA_TYPE_P (TREE_TYPE (gen_tmpl))
	  && TREE_CODE (CP_DECL_CONTEXT (gen_tmpl)) == NAMESPACE_DECL)
	/* This occurs when the user has tried to define a tagged type
	   in a scope that forbids it.  We emitted an error during the
	   parse.  We didn't complete the bail out then, so here we
	   are.  */
	return error_mark_node;

      context = DECL_CONTEXT (gen_tmpl);
      if (context && TYPE_P (context))
	{
	  if (!uses_template_parms (DECL_CONTEXT (templ)))
	    /* If the context of the partially instantiated template is
	       already non-dependent, then we might as well use it.  */
	    context = DECL_CONTEXT (templ);
	  else
	    {
	      context = tsubst_aggr_type (context, arglist,
					  complain, in_decl, true);
	      /* Try completing the enclosing context if it's not already so.  */
	      if (context != error_mark_node
		  && !COMPLETE_TYPE_P (context))
		{
		  context = complete_type (context);
		  if (COMPLETE_TYPE_P (context))
		    {
		      /* Completion could have caused us to register the desired
			 specialization already, so check the table again.  */
		      entry = type_specializations->find_with_hash (&elt, hash);
		      if (entry)
			return entry->spec;
		    }
		}
	    }
	}
      else
	context = tsubst (context, arglist, complain, in_decl);

      if (context == error_mark_node)
	return error_mark_node;

      if (!context)
	context = global_namespace;

      /* Create the type.  */
      if (DECL_ALIAS_TEMPLATE_P (gen_tmpl))
	{
	  /* The user referred to a specialization of an alias
	    template represented by GEN_TMPL.

	    [temp.alias]/2 says:

	        When a template-id refers to the specialization of an
		alias template, it is equivalent to the associated
		type obtained by substitution of its
		template-arguments for the template-parameters in the
		type-id of the alias template.  */

	  t = tsubst (TREE_TYPE (gen_tmpl), arglist, complain, in_decl);
	  /* Note that the call above (by indirectly calling
	     register_specialization in tsubst_decl) registers the
	     TYPE_DECL representing the specialization of the alias
	     template.  So next time someone substitutes ARGLIST for
	     the template parms into the alias template (GEN_TMPL),
	     she'll get that TYPE_DECL back.  */

	  if (t == error_mark_node)
	    return t;
	}
      else if (TREE_CODE (template_type) == ENUMERAL_TYPE)
	{
	  if (!is_dependent_type)
	    {
	      set_current_access_from_decl (TYPE_NAME (template_type));
	      t = start_enum (TYPE_IDENTIFIER (template_type), NULL_TREE,
			      tsubst (ENUM_UNDERLYING_TYPE (template_type),
				      arglist, complain, in_decl),
			      tsubst_attributes (TYPE_ATTRIBUTES (template_type),
						 arglist, complain, in_decl),
			      SCOPED_ENUM_P (template_type), NULL);

	      if (t == error_mark_node)
		return t;
	    }
	  else
            {
              /* We don't want to call start_enum for this type, since
                 the values for the enumeration constants may involve
                 template parameters.  And, no one should be interested
                 in the enumeration constants for such a type.  */
              t = cxx_make_type (ENUMERAL_TYPE);
              SET_SCOPED_ENUM_P (t, SCOPED_ENUM_P (template_type));
            }
          SET_OPAQUE_ENUM_P (t, OPAQUE_ENUM_P (template_type));
	  ENUM_FIXED_UNDERLYING_TYPE_P (t)
	    = ENUM_FIXED_UNDERLYING_TYPE_P (template_type);
	}
      else if (CLASS_TYPE_P (template_type))
	{
	  /* Lambda closures are regenerated in tsubst_lambda_expr, not
	     instantiated here.  */
	  gcc_assert (!LAMBDA_TYPE_P (template_type));

	  t = make_class_type (TREE_CODE (template_type));
	  CLASSTYPE_DECLARED_CLASS (t)
	    = CLASSTYPE_DECLARED_CLASS (template_type);
	  SET_CLASSTYPE_IMPLICIT_INSTANTIATION (t);

	  /* A local class.  Make sure the decl gets registered properly.  */
	  if (context == current_function_decl)
	    if (pushtag (DECL_NAME (gen_tmpl), t)
		== error_mark_node)
	      return error_mark_node;

	  if (comp_template_args (CLASSTYPE_TI_ARGS (template_type), arglist))
	    /* This instantiation is another name for the primary
	       template type. Set the TYPE_CANONICAL field
	       appropriately. */
	    TYPE_CANONICAL (t) = template_type;
	  else if (any_template_arguments_need_structural_equality_p (arglist))
	    SET_TYPE_STRUCTURAL_EQUALITY (t);
	}
      else
	gcc_unreachable ();

      /* If we called start_enum or pushtag above, this information
	 will already be set up.  */
      type_decl = TYPE_NAME (t);
      if (!type_decl)
	{
	  TYPE_CONTEXT (t) = FROB_CONTEXT (context);

	  type_decl = create_implicit_typedef (DECL_NAME (gen_tmpl), t);
	  DECL_CONTEXT (type_decl) = TYPE_CONTEXT (t);
	  DECL_SOURCE_LOCATION (type_decl)
	    = DECL_SOURCE_LOCATION (TYPE_STUB_DECL (template_type));
	}

      set_instantiating_module (type_decl);
      /* Although GEN_TMPL is the TEMPLATE_DECL, it has the same value
	 of export flag.  We want to propagate this because it might
	 be a friend declaration that pushes a new hidden binding.  */
      DECL_MODULE_EXPORT_P (type_decl) = DECL_MODULE_EXPORT_P (gen_tmpl);

      if (CLASS_TYPE_P (template_type))
	{
	  TREE_PRIVATE (type_decl)
	    = TREE_PRIVATE (TYPE_MAIN_DECL (template_type));
	  TREE_PROTECTED (type_decl)
	    = TREE_PROTECTED (TYPE_MAIN_DECL (template_type));
	  if (CLASSTYPE_VISIBILITY_SPECIFIED (template_type))
	    {
	      DECL_VISIBILITY_SPECIFIED (type_decl) = 1;
	      DECL_VISIBILITY (type_decl) = CLASSTYPE_VISIBILITY (template_type);
	    }
	}

      if (OVERLOAD_TYPE_P (t)
	  && !DECL_ALIAS_TEMPLATE_P (gen_tmpl))
	{
	  static const char *tags[] = {"abi_tag", "may_alias"};

	  for (unsigned ix = 0; ix != 2; ix++)
	    {
	      tree attributes
		= lookup_attribute (tags[ix], TYPE_ATTRIBUTES (template_type));

	      if (attributes)
		TYPE_ATTRIBUTES (t)
		  = tree_cons (TREE_PURPOSE (attributes),
			       TREE_VALUE (attributes),
			       TYPE_ATTRIBUTES (t));
	    }
	}

      /* Let's consider the explicit specialization of a member
         of a class template specialization that is implicitly instantiated,
	 e.g.:
	     template<class T>
	     struct S
	     {
	       template<class U> struct M {}; //#0
	     };

	     template<>
	     template<>
	     struct S<int>::M<char> //#1
	     {
	       int i;
	     };
	[temp.expl.spec]/4 says this is valid.

	In this case, when we write:
	S<int>::M<char> m;

	M is instantiated from the CLASSTYPE_TI_TEMPLATE of #1, not from
	the one of #0.

	When we encounter #1, we want to store the partial instantiation
	of M (template<class T> S<int>::M<T>) in its CLASSTYPE_TI_TEMPLATE.

	For all cases other than this "explicit specialization of member of a
	class template", we just want to store the most general template into
	the CLASSTYPE_TI_TEMPLATE of M.

	This case of "explicit specialization of member of a class template"
	only happens when:
	1/ the enclosing class is an instantiation of, and therefore not
	the same as, the context of the most general template, and
	2/ we aren't looking at the partial instantiation itself, i.e.
	the innermost arguments are not the same as the innermost parms of
	the most general template.

	So it's only when 1/ and 2/ happens that we want to use the partial
	instantiation of the member template in lieu of its most general
	template.  */

      if (PRIMARY_TEMPLATE_P (gen_tmpl)
	  && TMPL_ARGS_HAVE_MULTIPLE_LEVELS (arglist)
	  /* the enclosing class must be an instantiation...  */
	  && CLASS_TYPE_P (context)
	  && !same_type_p (context, DECL_CONTEXT (gen_tmpl)))
	{
	  TREE_VEC_LENGTH (arglist)--;
	  ++processing_template_decl;
	  tree tinfo = TYPE_TEMPLATE_INFO_MAYBE_ALIAS (TREE_TYPE (gen_tmpl));
	  tree partial_inst_args =
	    tsubst (INNERMOST_TEMPLATE_ARGS (TI_ARGS (tinfo)),
		    arglist, complain, NULL_TREE);
	  --processing_template_decl;
	  TREE_VEC_LENGTH (arglist)++;
	  if (partial_inst_args == error_mark_node)
	    return error_mark_node;
	  use_partial_inst_tmpl =
	    /*...and we must not be looking at the partial instantiation
	     itself. */
	    !comp_template_args (INNERMOST_TEMPLATE_ARGS (arglist),
				 partial_inst_args);
	}

      if (!use_partial_inst_tmpl)
	/* This case is easy; there are no member templates involved.  */
	found = gen_tmpl;
      else
	{
	  /* This is a full instantiation of a member template.  Find
	     the partial instantiation of which this is an instance.  */

	  /* Temporarily reduce by one the number of levels in the ARGLIST
	     so as to avoid comparing the last set of arguments.  */
	  TREE_VEC_LENGTH (arglist)--;
	  /* We don't use COMPLAIN in the following call because this isn't
	     the immediate context of deduction.  For instance, tf_partial
	     could be set here as we might be at the beginning of template
	     argument deduction when any explicitly specified template
	     arguments are substituted into the function type.  tf_partial
	     could lead into trouble because we wouldn't find the partial
	     instantiation that might have been created outside tf_partial
	     context, because the levels of template parameters wouldn't
	     match, because in a tf_partial context, tsubst doesn't reduce
	     TEMPLATE_PARM_LEVEL.  */
	  found = tsubst (gen_tmpl, arglist, tf_none, NULL_TREE);
	  TREE_VEC_LENGTH (arglist)++;
	  /* FOUND is either a proper class type, or an alias
	     template specialization.  In the later case, it's a
	     TYPE_DECL, resulting from the substituting of arguments
	     for parameters in the TYPE_DECL of the alias template
	     done earlier.  So be careful while getting the template
	     of FOUND.  */
	  found = (TREE_CODE (found) == TEMPLATE_DECL
		   ? found
		   : (TREE_CODE (found) == TYPE_DECL
		      ? DECL_TI_TEMPLATE (found)
		      : CLASSTYPE_TI_TEMPLATE (found)));

	  if (DECL_CLASS_TEMPLATE_P (found)
	      && CLASSTYPE_TEMPLATE_SPECIALIZATION (TREE_TYPE (found)))
	    {
	      /* If this partial instantiation is specialized, we want to
		 use it for hash table lookup.  */
	      elt.tmpl = found;
	      elt.args = arglist = INNERMOST_TEMPLATE_ARGS (arglist);
	      hash = spec_hasher::hash (&elt);
	    }
	}

      /* Build template info for the new specialization.  */
      SET_TYPE_TEMPLATE_INFO (t, build_template_info (found, arglist));

      elt.spec = t;
      slot = type_specializations->find_slot_with_hash (&elt, hash, INSERT);
      gcc_checking_assert (*slot == NULL);
      entry = ggc_alloc<spec_entry> ();
      *entry = elt;
      *slot = entry;

      /* Note this use of the partial instantiation so we can check it
	 later in maybe_process_partial_specialization.  */
      DECL_TEMPLATE_INSTANTIATIONS (found)
	= tree_cons (arglist, t,
		     DECL_TEMPLATE_INSTANTIATIONS (found));

      if (TREE_CODE (template_type) == ENUMERAL_TYPE
	  && !uses_template_parms (current_nonlambda_scope ())
	  && !DECL_ALIAS_TEMPLATE_P (gen_tmpl))
	/* Now that the type has been registered on the instantiations
	   list, we set up the enumerators.  Because the enumeration
	   constants may involve the enumeration type itself, we make
	   sure to register the type first, and then create the
	   constants.  That way, doing tsubst_expr for the enumeration
	   constants won't result in recursive calls here; we'll find
	   the instantiation and exit above.  */
	tsubst_enum (template_type, t, arglist);

      if (CLASS_TYPE_P (template_type) && is_dependent_type)
	/* If the type makes use of template parameters, the
	   code that generates debugging information will crash.  */
	DECL_IGNORED_P (TYPE_MAIN_DECL (t)) = 1;

      /* Possibly limit visibility based on template args.  */
      TREE_PUBLIC (type_decl) = 1;
      determine_visibility (type_decl);

      inherit_targ_abi_tags (t);

      return t;
    }
}

/* Return a TEMPLATE_ID_EXPR for the given variable template and ARGLIST.  */

tree
lookup_template_variable (tree templ, tree arglist, tsubst_flags_t complain)
{
  if (flag_concepts && variable_concept_p (templ))
    return build_concept_check (templ, arglist, tf_none);

  tree gen_templ = most_general_template (templ);
  tree parms = DECL_INNERMOST_TEMPLATE_PARMS (gen_templ);
  arglist = add_outermost_template_args (templ, arglist);
  arglist = coerce_template_parms (parms, arglist, templ, complain);
  if (arglist == error_mark_node)
    return error_mark_node;

  /* The type of the expression is NULL_TREE since the template-id could refer
     to an explicit or partial specialization. */
  return build2 (TEMPLATE_ID_EXPR, NULL_TREE, templ, arglist);
}

/* Instantiate a variable declaration from a TEMPLATE_ID_EXPR if it's
   not dependent.  */

tree
finish_template_variable (tree var, tsubst_flags_t complain)
{
  tree templ = TREE_OPERAND (var, 0);
  tree arglist = TREE_OPERAND (var, 1);

  /* If the template or arguments are dependent, then we
     can't resolve the TEMPLATE_ID_EXPR yet.  */
  if (TMPL_PARMS_DEPTH (DECL_TEMPLATE_PARMS (templ)) != 1
      || any_dependent_template_arguments_p (arglist))
    return var;

  if (flag_concepts && !constraints_satisfied_p (templ, arglist))
    {
      if (complain & tf_error)
	{
	  auto_diagnostic_group d;
	  error ("use of invalid variable template %qE", var);
	  diagnose_constraints (location_of (var), templ, arglist);
	}
      return error_mark_node;
    }

  return instantiate_template (templ, arglist, complain);
}

/* Construct a TEMPLATE_ID_EXPR for the given variable template TEMPL having
   TARGS template args, and instantiate it if it's not dependent.  */

tree
lookup_and_finish_template_variable (tree templ, tree targs,
				     tsubst_flags_t complain)
{
  tree var = lookup_template_variable (templ, targs, complain);
  if (var == error_mark_node)
    return error_mark_node;
  var = finish_template_variable (var, complain);
  mark_used (var);
  return var;
}

/* If the set of template parameters PARMS contains a template parameter
   at the given LEVEL and INDEX, then return this parameter.  Otherwise
   return NULL_TREE.  */

static tree
corresponding_template_parameter_list (tree parms, int level, int index)
{
  while (TMPL_PARMS_DEPTH (parms) > level)
    parms = TREE_CHAIN (parms);

  if (TMPL_PARMS_DEPTH (parms) != level
      || TREE_VEC_LENGTH (TREE_VALUE (parms)) <= index)
    return NULL_TREE;

  return TREE_VEC_ELT (TREE_VALUE (parms), index);
}

/* Return the TREE_LIST for the template parameter from PARMS that positionally
   corresponds to the template parameter PARM, or else return NULL_TREE.  */

static tree
corresponding_template_parameter_list (tree parms, tree parm)
{
  int level, index;
  template_parm_level_and_index (parm, &level, &index);
  return corresponding_template_parameter_list (parms, level, index);
}

/* As above, but pull out the actual parameter.  */

static tree
corresponding_template_parameter (tree parms, tree parm)
{
  tree list = corresponding_template_parameter_list (parms, parm);
  if (!list)
    return NULL_TREE;

  tree t = TREE_VALUE (list);
  /* As in template_parm_to_arg.  */
  if (TREE_CODE (t) == TYPE_DECL || TREE_CODE (t) == TEMPLATE_DECL)
    t = TREE_TYPE (t);
  else
    t = DECL_INITIAL (t);

  gcc_assert (TEMPLATE_PARM_P (t));
  return t;
}

struct pair_fn_data
{
  tree_fn_t fn;
  tree_fn_t any_fn;
  void *data;
  /* True when we should also visit template parameters that occur in
     non-deduced contexts.  */
  bool include_nondeduced_p;
  hash_set<tree> *visited;
};

/* Called from for_each_template_parm via walk_tree.  */

static tree
for_each_template_parm_r (tree *tp, int *walk_subtrees, void *d)
{
  tree t = *tp;
  struct pair_fn_data *pfd = (struct pair_fn_data *) d;
  tree_fn_t fn = pfd->fn;
  void *data = pfd->data;
  tree result = NULL_TREE;

#define WALK_SUBTREE(NODE)						\
  do									\
    {									\
      result = for_each_template_parm (NODE, fn, data, pfd->visited,	\
				       pfd->include_nondeduced_p,	\
				       pfd->any_fn);			\
      if (result) goto out;						\
    }									\
  while (0)

  if (pfd->any_fn && (*pfd->any_fn)(t, data))
    return t;

  if (TYPE_P (t)
      && (pfd->include_nondeduced_p || TREE_CODE (t) != TYPENAME_TYPE))
    WALK_SUBTREE (TYPE_CONTEXT (t));

  switch (TREE_CODE (t))
    {
    case RECORD_TYPE:
      if (TYPE_PTRMEMFUNC_P (t))
	break;
      /* Fall through.  */

    case UNION_TYPE:
    case ENUMERAL_TYPE:
      if (!TYPE_TEMPLATE_INFO (t))
	*walk_subtrees = 0;
      else
	WALK_SUBTREE (TYPE_TI_ARGS (t));
      break;

    case INTEGER_TYPE:
      WALK_SUBTREE (TYPE_MIN_VALUE (t));
      WALK_SUBTREE (TYPE_MAX_VALUE (t));
      break;

    case METHOD_TYPE:
      /* Since we're not going to walk subtrees, we have to do this
	 explicitly here.  */
      WALK_SUBTREE (TYPE_METHOD_BASETYPE (t));
      /* Fall through.  */

    case FUNCTION_TYPE:
      /* Check the return type.  */
      WALK_SUBTREE (TREE_TYPE (t));

      /* Check the parameter types.  Since default arguments are not
	 instantiated until they are needed, the TYPE_ARG_TYPES may
	 contain expressions that involve template parameters.  But,
	 no-one should be looking at them yet.  And, once they're
	 instantiated, they don't contain template parameters, so
	 there's no point in looking at them then, either.  */
      {
	tree parm;

	for (parm = TYPE_ARG_TYPES (t); parm; parm = TREE_CHAIN (parm))
	  WALK_SUBTREE (TREE_VALUE (parm));

	/* Since we've already handled the TYPE_ARG_TYPES, we don't
	   want walk_tree walking into them itself.  */
	*walk_subtrees = 0;
      }

      if (flag_noexcept_type)
	{
	  tree spec = TYPE_RAISES_EXCEPTIONS (t);
	  if (spec)
	    WALK_SUBTREE (TREE_PURPOSE (spec));
	}
      break;

    case TYPEOF_TYPE:
    case DECLTYPE_TYPE:
      if (pfd->include_nondeduced_p
	  && for_each_template_parm (TYPE_VALUES_RAW (t), fn, data,
				     pfd->visited,
				     pfd->include_nondeduced_p,
				     pfd->any_fn))
	return error_mark_node;
      *walk_subtrees = false;
      break;

    case TRAIT_TYPE:
      if (pfd->include_nondeduced_p)
	{
	  WALK_SUBTREE (TRAIT_TYPE_TYPE1 (t));
	  WALK_SUBTREE (TRAIT_TYPE_TYPE2 (t));
	}
      *walk_subtrees = false;
      break;

    case FUNCTION_DECL:
    case VAR_DECL:
      if (DECL_LANG_SPECIFIC (t) && DECL_TEMPLATE_INFO (t))
	WALK_SUBTREE (DECL_TI_ARGS (t));
      break;

    case PARM_DECL:
      WALK_SUBTREE (TREE_TYPE (t));
      break;

    case CONST_DECL:
      if (DECL_TEMPLATE_PARM_P (t))
	WALK_SUBTREE (DECL_INITIAL (t));
      if (DECL_CONTEXT (t)
	  && pfd->include_nondeduced_p)
	WALK_SUBTREE (DECL_CONTEXT (t));
      break;

    case BOUND_TEMPLATE_TEMPLATE_PARM:
      /* Record template parameters such as `T' inside `TT<T>'.  */
      WALK_SUBTREE (TYPE_TI_ARGS (t));
      /* Fall through.  */

    case TEMPLATE_TEMPLATE_PARM:
    case TEMPLATE_TYPE_PARM:
    case TEMPLATE_PARM_INDEX:
      if (fn && (*fn)(t, data))
	return t;
      else if (!fn)
	return t;
      break;

    case TEMPLATE_DECL:
      /* A template template parameter is encountered.  */
      if (DECL_TEMPLATE_TEMPLATE_PARM_P (t))
	WALK_SUBTREE (TREE_TYPE (t));

      /* Already substituted template template parameter */
      *walk_subtrees = 0;
      break;

    case TYPENAME_TYPE:
      /* A template-id in a TYPENAME_TYPE might be a deduced context after
	 partial instantiation.  */
      WALK_SUBTREE (TYPENAME_TYPE_FULLNAME (t));
      *walk_subtrees = 0;
      break;

    case INDIRECT_REF:
    case COMPONENT_REF:
      /* If there's no type, then this thing must be some expression
	 involving template parameters.  */
      if (!fn && !TREE_TYPE (t))
	return error_mark_node;
      break;

    case CONSTRUCTOR:
    case TRAIT_EXPR:
    case PLUS_EXPR:
    case MULT_EXPR:
    case SCOPE_REF:
      /* These are non-deduced contexts.  */
      if (!pfd->include_nondeduced_p)
	*walk_subtrees = 0;
      break;

    case MODOP_EXPR:
    case CAST_EXPR:
    case IMPLICIT_CONV_EXPR:
    case REINTERPRET_CAST_EXPR:
    case CONST_CAST_EXPR:
    case STATIC_CAST_EXPR:
    case DYNAMIC_CAST_EXPR:
    case ARROW_EXPR:
    case DOTSTAR_EXPR:
    case TYPEID_EXPR:
    case PSEUDO_DTOR_EXPR:
      if (!fn)
	return error_mark_node;
      break;

    default:
      break;
    }

  #undef WALK_SUBTREE

  /* We didn't find any template parameters we liked.  */
 out:
  return result;
}

/* For each TEMPLATE_TYPE_PARM, TEMPLATE_TEMPLATE_PARM,
   BOUND_TEMPLATE_TEMPLATE_PARM or TEMPLATE_PARM_INDEX in T,
   call FN with the parameter and the DATA.
   If FN returns nonzero, the iteration is terminated, and
   for_each_template_parm returns 1.  Otherwise, the iteration
   continues.  If FN never returns a nonzero value, the value
   returned by for_each_template_parm is 0.  If FN is NULL, it is
   considered to be the function which always returns 1.

   If INCLUDE_NONDEDUCED_P, then this routine will also visit template
   parameters that occur in non-deduced contexts.  When false, only
   visits those template parameters that can be deduced.  */

static tree
for_each_template_parm (tree t, tree_fn_t fn, void* data,
			hash_set<tree> *visited,
			bool include_nondeduced_p,
			tree_fn_t any_fn)
{
  struct pair_fn_data pfd;
  tree result;

  /* Set up.  */
  pfd.fn = fn;
  pfd.any_fn = any_fn;
  pfd.data = data;
  pfd.include_nondeduced_p = include_nondeduced_p;

  /* Walk the tree.  (Conceptually, we would like to walk without
     duplicates, but for_each_template_parm_r recursively calls
     for_each_template_parm, so we would need to reorganize a fair
     bit to use walk_tree_without_duplicates, so we keep our own
     visited list.)  */
  if (visited)
    pfd.visited = visited;
  else
    pfd.visited = new hash_set<tree>;
  result = cp_walk_tree (&t,
		         for_each_template_parm_r,
		         &pfd,
		         pfd.visited);

  /* Clean up.  */
  if (!visited)
    {
      delete pfd.visited;
      pfd.visited = 0;
    }

  return result;
}

struct find_template_parameter_info
{
  explicit find_template_parameter_info (tree ctx_parms)
    : ctx_parms (ctx_parms),
      max_depth (TMPL_PARMS_DEPTH (ctx_parms))
  {}

  hash_set<tree> visited;
  hash_set<tree> parms;
  tree parm_list = NULL_TREE;
  tree *parm_list_tail = &parm_list;
  tree ctx_parms;
  int max_depth;

  tree find_in (tree);
  tree find_in_recursive (tree);
  bool found (tree);
  unsigned num_found () { return parms.elements (); }
};

/* Appends the declaration of T to the list in DATA.  */

static int
keep_template_parm (tree t, void* data)
{
  find_template_parameter_info *ftpi = (find_template_parameter_info*)data;

  /* Template parameters declared within the expression are not part of
     the parameter mapping. For example, in this concept:

       template<typename T>
       concept C = requires { <expr> } -> same_as<int>;

     the return specifier same_as<int> declares a new decltype parameter
     that must not be part of the parameter mapping. The same is true
     for generic lambda parameters, lambda template parameters, etc.  */
  int level;
  int index;
  template_parm_level_and_index (t, &level, &index);
  if (level == 0 || level > ftpi->max_depth)
    return 0;

  if (TREE_CODE (t) == BOUND_TEMPLATE_TEMPLATE_PARM)
    /* We want the underlying TEMPLATE_TEMPLATE_PARM, not the
       BOUND_TEMPLATE_TEMPLATE_PARM itself.  */
    t = TREE_TYPE (TEMPLATE_TEMPLATE_PARM_TEMPLATE_DECL (t));

  /* This template parameter might be an argument to a cached dependent
     specalization that was formed earlier inside some other template, in
     which case the parameter is not among the ones that are in-scope.
     Look in CTX_PARMS to find the corresponding in-scope template
     parameter, and use it instead.  */
  if (tree in_scope = corresponding_template_parameter (ftpi->ctx_parms, t))
    t = in_scope;

  /* Arguments like const T yield parameters like const T. This means that
     a template-id like X<T, const T> would yield two distinct parameters:
     T and const T. Adjust types to their unqualified versions.  */
  if (TYPE_P (t))
    t = TYPE_MAIN_VARIANT (t);
  if (!ftpi->parms.add (t))
    {
      /* Append T to PARM_LIST.  */
      tree node = build_tree_list (NULL_TREE, t);
      *ftpi->parm_list_tail = node;
      ftpi->parm_list_tail = &TREE_CHAIN (node);
    }

  /* Verify the parameter we found has a valid index.  */
  if (flag_checking)
    {
      tree parms = ftpi->ctx_parms;
      while (TMPL_PARMS_DEPTH (parms) > level)
	parms = TREE_CHAIN (parms);
      if (int len = TREE_VEC_LENGTH (TREE_VALUE (parms)))
	gcc_assert (index < len);
    }

  return 0;
}

/* Ensure that we recursively examine certain terms that are not normally
   visited in for_each_template_parm_r.  */

static int
any_template_parm_r (tree t, void *data)
{
  find_template_parameter_info *ftpi = (find_template_parameter_info*)data;

#define WALK_SUBTREE(NODE)						\
  do									\
    {									\
      for_each_template_parm (NODE, keep_template_parm, data,		\
			      &ftpi->visited, true,			\
			      any_template_parm_r);			\
    }									\
  while (0)

  /* A mention of a member alias/typedef is a use of all of its template
     arguments, including those from the enclosing class, so we don't use
     alias_template_specialization_p here.  */
  if (TYPE_P (t) && typedef_variant_p (t))
    if (tree tinfo = TYPE_ALIAS_TEMPLATE_INFO (t))
      WALK_SUBTREE (TI_ARGS (tinfo));

  switch (TREE_CODE (t))
    {
    case TEMPLATE_TYPE_PARM:
      /* Type constraints of a placeholder type may contain parameters.  */
      if (is_auto (t))
	if (tree constr = PLACEHOLDER_TYPE_CONSTRAINTS (t))
	  WALK_SUBTREE (constr);
      break;

    case TEMPLATE_ID_EXPR:
      /* Search through references to variable templates.  */
      WALK_SUBTREE (TREE_OPERAND (t, 0));
      WALK_SUBTREE (TREE_OPERAND (t, 1));
      break;

    case TEMPLATE_PARM_INDEX:
      WALK_SUBTREE (TREE_TYPE (t));
      break;

    case TEMPLATE_DECL:
      /* If T is a member template that shares template parameters with
	 ctx_parms, we need to mark all those parameters for mapping.
	 To that end, it should suffice to just walk the DECL_CONTEXT of
	 the template (assuming the template is not overly general).  */
      WALK_SUBTREE (DECL_CONTEXT (t));
      break;

    case LAMBDA_EXPR:
      {
	/* Look in the parms and body.  */
	tree fn = lambda_function (t);
	WALK_SUBTREE (TREE_TYPE (fn));
	WALK_SUBTREE (DECL_SAVED_TREE (fn));
      }
      break;

    case IDENTIFIER_NODE:
      if (IDENTIFIER_CONV_OP_P (t))
	/* The conversion-type-id of a conversion operator may be dependent.  */
	WALK_SUBTREE (TREE_TYPE (t));
      break;

    case CONVERT_EXPR:
      if (is_dummy_object (t))
	WALK_SUBTREE (TREE_TYPE (t));
      break;

    default:
      break;
    }

  /* Keep walking.  */
  return 0;
}

/* Look through T for template parameters.  */

tree
find_template_parameter_info::find_in (tree t)
{
  return for_each_template_parm (t, keep_template_parm, this, &visited,
				 /*include_nondeduced*/true,
				 any_template_parm_r);
}

/* As above, but also recursively look into the default arguments of template
   parameters we found.  Used for alias CTAD.  */

tree
find_template_parameter_info::find_in_recursive (tree t)
{
  if (tree r = find_in (t))
    return r;
  /* Since newly found parms are added to the end of the list, we
     can just walk it until we reach the end.  */
  for (tree pl = parm_list; pl; pl = TREE_CHAIN (pl))
    {
      tree parm = TREE_VALUE (pl);
      tree list = corresponding_template_parameter_list (ctx_parms, parm);
      if (tree r = find_in (TREE_PURPOSE (list)))
	return r;
    }
  return NULL_TREE;
}

/* True if PARM was found by a previous call to find_in.  PARM can be a
   TREE_LIST, a DECL_TEMPLATE_PARM_P, or a TEMPLATE_PARM_P.  */

bool
find_template_parameter_info::found (tree parm)
{
  if (TREE_CODE (parm) == TREE_LIST)
    parm = TREE_VALUE (parm);
  if (TREE_CODE (parm) == TYPE_DECL)
    parm = TREE_TYPE (parm);
  else
    parm = DECL_INITIAL (parm);
  gcc_checking_assert (TEMPLATE_PARM_P (parm));
  return parms.contains (parm);
}

/* Returns a list of unique template parameters found within T, where CTX_PARMS
   are the template parameters in scope.  */

tree
find_template_parameters (tree t, tree ctx_parms)
{
  if (!ctx_parms)
    return NULL_TREE;

  find_template_parameter_info ftpi (ctx_parms);
  ftpi.find_in (t);
  return ftpi.parm_list;
}

/* Returns true if T depends on any template parameter.  */

bool
uses_template_parms (tree t)
{
  if (t == NULL_TREE || t == error_mark_node)
    return false;

  /* Namespaces can't depend on any template parameters.  */
  if (TREE_CODE (t) == NAMESPACE_DECL)
    return false;

  processing_template_decl_sentinel ptds (/*reset*/false);
  ++processing_template_decl;

  if (TYPE_P (t))
    return dependent_type_p (t);
  else if (TREE_CODE (t) == TREE_VEC)
    return any_dependent_template_arguments_p (t);
  else if (TREE_CODE (t) == TREE_LIST)
    return (uses_template_parms (TREE_VALUE (t))
	    || uses_template_parms (TREE_CHAIN (t)));
  else if (TREE_CODE (t) == TYPE_DECL)
    return dependent_type_p (TREE_TYPE (t));
  else
    return instantiation_dependent_expression_p (t);
}

/* Returns true if T depends on any template parameter with level LEVEL.  */

bool
uses_template_parms_level (tree t, int level)
{
  return for_each_template_parm (t, template_parm_this_level_p, &level, NULL,
				 /*include_nondeduced_p=*/true);
}

/* Returns true if the signature of DECL depends on any template parameter from
   its enclosing class.  */

static bool
uses_outer_template_parms (tree decl)
{
  int depth;
  if (DECL_TEMPLATE_TEMPLATE_PARM_P (decl))
    depth = TEMPLATE_TYPE_LEVEL (TREE_TYPE (decl)) - 1;
  else
    depth = template_class_depth (CP_DECL_CONTEXT (decl));
  if (depth == 0)
    return false;
  if (for_each_template_parm (TREE_TYPE (decl), template_parm_outer_level,
			      &depth, NULL, /*include_nondeduced_p=*/true))
    return true;
  if (PRIMARY_TEMPLATE_P (decl)
      || DECL_TEMPLATE_TEMPLATE_PARM_P (decl))
    {
      tree parms = INNERMOST_TEMPLATE_PARMS (DECL_TEMPLATE_PARMS (decl));
      for (int i = TREE_VEC_LENGTH (parms) - 1; i >= 0; --i)
	{
	  tree parm = TREE_VALUE (TREE_VEC_ELT (parms, i));
	  tree defarg = TREE_PURPOSE (TREE_VEC_ELT (parms, i));
	  if (TREE_CODE (parm) == PARM_DECL
	      && for_each_template_parm (TREE_TYPE (parm),
					 template_parm_outer_level,
					 &depth, NULL, /*nondeduced*/true))
	    return true;
	  if (TREE_CODE (parm) == TEMPLATE_DECL
	      && uses_outer_template_parms (parm))
	    return true;
	  if (defarg
	      && for_each_template_parm (defarg, template_parm_outer_level,
					 &depth, NULL, /*nondeduced*/true))
	    return true;
	}
    }
  if (uses_outer_template_parms_in_constraints (decl))
    return true;
  return false;
}

/* Returns true if the constraints of DECL depend on any template parameters
   from its enclosing scope.  */

bool
uses_outer_template_parms_in_constraints (tree decl)
{
  tree ci = get_constraints (decl);
  if (ci)
    ci = CI_ASSOCIATED_CONSTRAINTS (ci);
  if (!ci)
    return false;
  int depth = template_class_depth (CP_DECL_CONTEXT (decl));
  if (depth == 0)
    return false;
  return for_each_template_parm (ci, template_parm_outer_level,
				 &depth, NULL, /*nondeduced*/true);
}

/* Returns TRUE iff INST is an instantiation we don't need to do in an
   ill-formed translation unit, i.e. a variable or function that isn't
   usable in a constant expression.  */

static inline bool
neglectable_inst_p (tree d)
{
  return (d && DECL_P (d)
	  && !undeduced_auto_decl (d)
	  && !(TREE_CODE (d) == FUNCTION_DECL
	       ? FNDECL_MANIFESTLY_CONST_EVALUATED (d)
	       : decl_maybe_constant_var_p (d)));
}

/* Returns TRUE iff we should refuse to instantiate DECL because it's
   neglectable and instantiated from within an erroneous instantiation.  */

static bool
limit_bad_template_recursion (tree decl)
{
  struct tinst_level *lev = current_tinst_level;
  int errs = errorcount + sorrycount;
  if (errs == 0 || !neglectable_inst_p (decl))
    return false;

  /* Avoid instantiating members of an ill-formed class.  */
  bool refuse
    = (DECL_CLASS_SCOPE_P (decl)
       && CLASSTYPE_ERRONEOUS (DECL_CONTEXT (decl)));

  if (!refuse)
    {
      for (; lev; lev = lev->next)
	if (neglectable_inst_p (lev->maybe_get_node ()))
	  break;
      refuse = (lev && errs > lev->errors);
    }

  if (refuse)
    {
      /* Don't warn about it not being defined.  */
      suppress_warning (decl, OPT_Wunused);
      tree clone;
      FOR_EACH_CLONE (clone, decl)
	suppress_warning (clone, OPT_Wunused);
    }
  return refuse;
}

static int tinst_depth;
extern int max_tinst_depth;
int depth_reached;

static GTY(()) struct tinst_level *last_error_tinst_level;

/* We're starting to instantiate D; record the template instantiation context
   at LOC for diagnostics and to restore it later.  */

bool
push_tinst_level_loc (tree tldcl, tree targs, location_t loc)
{
  struct tinst_level *new_level;

  if (tinst_depth >= max_tinst_depth)
    {
      /* Tell error.cc not to try to instantiate any templates.  */
      at_eof = 2;
      fatal_error (input_location,
		   "template instantiation depth exceeds maximum of %d"
		   " (use %<-ftemplate-depth=%> to increase the maximum)",
                   max_tinst_depth);
      return false;
    }

  /* If the current instantiation caused problems, don't let it instantiate
     anything else.  Do allow deduction substitution and decls usable in
     constant expressions.  */
  if (!targs && limit_bad_template_recursion (tldcl))
    {
      /* Avoid no_linkage_errors and unused function (and all other)
	 warnings for this decl.  */
      suppress_warning (tldcl);
      return false;
    }

  /* When not -quiet, dump template instantiations other than functions, since
     announce_function will take care of those.  */
  if (!quiet_flag && !targs
      && TREE_CODE (tldcl) != TREE_LIST
      && TREE_CODE (tldcl) != FUNCTION_DECL)
    fprintf (stderr, " %s", decl_as_string (tldcl, TFF_DECL_SPECIFIERS));

  new_level = tinst_level_freelist ().alloc ();
  new_level->tldcl = tldcl;
  new_level->targs = targs;
  new_level->locus = loc;
  new_level->errors = errorcount + sorrycount;
  new_level->next = NULL;
  new_level->refcount = 0;
  new_level->path = new_level->visible = nullptr;
  set_refcount_ptr (new_level->next, current_tinst_level);
  set_refcount_ptr (current_tinst_level, new_level);

  ++tinst_depth;
  if (GATHER_STATISTICS && (tinst_depth > depth_reached))
    depth_reached = tinst_depth;

  return true;
}

/* We're starting substitution of TMPL<ARGS>; record the template
   substitution context for diagnostics and to restore it later.  */

bool
push_tinst_level (tree tmpl, tree args)
{
  return push_tinst_level_loc (tmpl, args, input_location);
}

/* We're starting to instantiate D; record INPUT_LOCATION and the
   template instantiation context for diagnostics and to restore it
   later.  */

bool
push_tinst_level (tree d)
{
  return push_tinst_level_loc (d, input_location);
}

/* Likewise, but record LOC as the program location.  */

bool
push_tinst_level_loc (tree d, location_t loc)
{
  gcc_assert (TREE_CODE (d) != TREE_LIST);
  return push_tinst_level_loc (d, NULL, loc);
}

/* We're done instantiating this template; return to the instantiation
   context.  */

void
pop_tinst_level (void)
{
  /* Restore the filename and line number stashed away when we started
     this instantiation.  */
  input_location = current_tinst_level->locus;
  set_refcount_ptr (current_tinst_level, current_tinst_level->next);
  --tinst_depth;
}

/* We're instantiating a deferred template; restore the template
   instantiation context in which the instantiation was requested, which
   is one step out from LEVEL.  Return the corresponding DECL or TYPE.  */

static tree
reopen_tinst_level (struct tinst_level *level)
{
  struct tinst_level *t;

  tinst_depth = 0;
  for (t = level; t; t = t->next)
    ++tinst_depth;

  set_refcount_ptr (current_tinst_level, level);
  pop_tinst_level ();
  if (current_tinst_level)
    current_tinst_level->errors = errorcount+sorrycount;
  return level->maybe_get_node ();
}

/* Returns the TINST_LEVEL which gives the original instantiation
   context.  */

struct tinst_level *
outermost_tinst_level (void)
{
  struct tinst_level *level = current_tinst_level;
  if (level)
    while (level->next)
      level = level->next;
  return level;
}

/* True iff T is a friend function declaration that is not itself a template
   and is not defined in a class template.  */

bool
non_templated_friend_p (tree t)
{
  if (t && TREE_CODE (t) == FUNCTION_DECL
      && DECL_UNIQUE_FRIEND_P (t))
    {
      tree ti = DECL_TEMPLATE_INFO (t);
      if (!ti)
	return true;
      /* DECL_FRIEND_CONTEXT is set for a friend defined in class.  */
      if (DECL_FRIEND_CONTEXT (t))
	return false;
      /* Non-templated friends in a class template are still represented with a
	 TEMPLATE_DECL; check that its primary template is the befriending
	 class.  Note that DECL_PRIMARY_TEMPLATE is null for
	 template <class T> friend A<T>::f(); */
      tree tmpl = TI_TEMPLATE (ti);
      tree primary = DECL_PRIMARY_TEMPLATE (tmpl);
      return (primary && primary != tmpl);
    }
  else
    return false;
}

/* DECL is a friend FUNCTION_DECL or TEMPLATE_DECL.  ARGS is the
   vector of template arguments, as for tsubst.

   Returns an appropriate tsubst'd friend declaration.  */

static tree
tsubst_friend_function (tree decl, tree args)
{
  tree new_friend;

  if (TREE_CODE (decl) == FUNCTION_DECL
      && DECL_TEMPLATE_INSTANTIATION (decl)
      && TREE_CODE (DECL_TI_TEMPLATE (decl)) != TEMPLATE_DECL)
    /* This was a friend declared with an explicit template
       argument list, e.g.:

       friend void f<>(T);

       to indicate that f was a template instantiation, not a new
       function declaration.  Now, we have to figure out what
       instantiation of what template.  */
    {
      tree template_id, arglist, fns;
      tree new_args;
      tree tmpl;
      tree ns = decl_namespace_context (TYPE_MAIN_DECL (current_class_type));

      /* Friend functions are looked up in the containing namespace scope.
	 We must enter that scope, to avoid finding member functions of the
	 current class with same name.  */
      push_nested_namespace (ns);
      fns = tsubst_expr (DECL_TI_TEMPLATE (decl), args,
			 tf_warning_or_error, NULL_TREE);
      pop_nested_namespace (ns);
      arglist = tsubst (DECL_TI_ARGS (decl), args,
			tf_warning_or_error, NULL_TREE);
      template_id = lookup_template_function (fns, arglist);

      new_friend = tsubst (decl, args, tf_warning_or_error, NULL_TREE);
      tmpl = determine_specialization (template_id, new_friend,
				       &new_args,
				       /*need_member_template=*/0,
				       TREE_VEC_LENGTH (args),
				       tsk_none);
      return instantiate_template (tmpl, new_args, tf_error);
    }

  new_friend = tsubst (decl, args, tf_warning_or_error, NULL_TREE);
  if (new_friend == error_mark_node)
    return error_mark_node;

  /* The NEW_FRIEND will look like an instantiation, to the
     compiler, but is not an instantiation from the point of view of
     the language.  For example, we might have had:

     template <class T> struct S {
       template <class U> friend void f(T, U);
     };

     Then, in S<int>, template <class U> void f(int, U) is not an
     instantiation of anything.  */

  DECL_USE_TEMPLATE (new_friend) = 0;
  if (TREE_CODE (new_friend) == TEMPLATE_DECL)
    {
      DECL_UNINSTANTIATED_TEMPLATE_FRIEND_P (new_friend) = false;
      DECL_USE_TEMPLATE (DECL_TEMPLATE_RESULT (new_friend)) = 0;
      DECL_SAVED_TREE (DECL_TEMPLATE_RESULT (new_friend))
	= DECL_SAVED_TREE (DECL_TEMPLATE_RESULT (decl));

      /* Substitute TEMPLATE_PARMS_CONSTRAINTS so that parameter levels will
	 match in decls_match.  */
      tree parms = DECL_TEMPLATE_PARMS (new_friend);
      tree treqs = TEMPLATE_PARMS_CONSTRAINTS (parms);
      treqs = maybe_substitute_reqs_for (treqs, new_friend);
      if (treqs != TEMPLATE_PARMS_CONSTRAINTS (parms))
	{
	  TEMPLATE_PARMS_CONSTRAINTS (parms) = treqs;
	  /* As well as each TEMPLATE_PARM_CONSTRAINTS.  */
	  tsubst_each_template_parm_constraints (parms, args,
						 tf_warning_or_error);
	}
    }

  /* The mangled name for the NEW_FRIEND is incorrect.  The function
     is not a template instantiation and should not be mangled like
     one.  Therefore, we forget the mangling here; we'll recompute it
     later if we need it.  */
  if (TREE_CODE (new_friend) != TEMPLATE_DECL)
    {
      SET_DECL_RTL (new_friend, NULL);
      SET_DECL_ASSEMBLER_NAME (new_friend, NULL_TREE);
    }

  if (DECL_NAMESPACE_SCOPE_P (new_friend))
    {
      tree old_decl;
      tree ns;

      /* We must save some information from NEW_FRIEND before calling
	 duplicate decls since that function will free NEW_FRIEND if
	 possible.  */
      tree new_friend_template_info = DECL_TEMPLATE_INFO (new_friend);
      tree new_friend_result_template_info = NULL_TREE;
      bool new_friend_is_defn =
	(new_friend_template_info
	 && (DECL_INITIAL (DECL_TEMPLATE_RESULT
			   (template_for_substitution (new_friend)))
	     != NULL_TREE));
      tree not_tmpl = new_friend;

      if (TREE_CODE (new_friend) == TEMPLATE_DECL)
	{
	  /* This declaration is a `primary' template.  */
	  DECL_PRIMARY_TEMPLATE (new_friend) = new_friend;

	  not_tmpl = DECL_TEMPLATE_RESULT (new_friend);
	  new_friend_result_template_info = DECL_TEMPLATE_INFO (not_tmpl);
	}
      else if (!constraints_satisfied_p (new_friend))
	/* Only define a constrained hidden friend when satisfied.  */
	return error_mark_node;

      /* Inside pushdecl_namespace_level, we will push into the
	 current namespace. However, the friend function should go
	 into the namespace of the template.  */
      ns = decl_namespace_context (new_friend);
      push_nested_namespace (ns);
      old_decl = pushdecl_namespace_level (new_friend, /*hiding=*/true);
      pop_nested_namespace (ns);

      if (old_decl == error_mark_node)
	return error_mark_node;

      if (old_decl != new_friend)
	{
	  /* This new friend declaration matched an existing
	     declaration.  For example, given:

	       template <class T> void f(T);
	       template <class U> class C {
		 template <class T> friend void f(T) {}
	       };

	     the friend declaration actually provides the definition
	     of `f', once C has been instantiated for some type.  So,
	     old_decl will be the out-of-class template declaration,
	     while new_friend is the in-class definition.

	     But, if `f' was called before this point, the
	     instantiation of `f' will have DECL_TI_ARGS corresponding
	     to `T' but not to `U', references to which might appear
	     in the definition of `f'.  Previously, the most general
	     template for an instantiation of `f' was the out-of-class
	     version; now it is the in-class version.  Therefore, we
	     run through all specialization of `f', adding to their
	     DECL_TI_ARGS appropriately.  In particular, they need a
	     new set of outer arguments, corresponding to the
	     arguments for this class instantiation.

	     The same situation can arise with something like this:

	       friend void f(int);
	       template <class T> class C {
		 friend void f(T) {}
	       };

	     when `C<int>' is instantiated.  Now, `f(int)' is defined
	     in the class.  */

	  if (!new_friend_is_defn)
	    /* On the other hand, if the in-class declaration does
	       *not* provide a definition, then we don't want to alter
	       existing definitions.  We can just leave everything
	       alone.  */
	    ;
	  else
	    {
	      tree new_template = TI_TEMPLATE (new_friend_template_info);
	      tree new_args = TI_ARGS (new_friend_template_info);

	      /* Overwrite whatever template info was there before, if
		 any, with the new template information pertaining to
		 the declaration.  */
	      DECL_TEMPLATE_INFO (old_decl) = new_friend_template_info;

	      if (TREE_CODE (old_decl) != TEMPLATE_DECL)
		{
		  /* We should have called reregister_specialization in
		     duplicate_decls.  */
		  gcc_assert (retrieve_specialization (new_template,
						       new_args, 0)
			      == old_decl);

		  /* Instantiate it if the global has already been used.  */
		  if (DECL_ODR_USED (old_decl))
		    instantiate_decl (old_decl, /*defer_ok=*/true,
				      /*expl_inst_class_mem_p=*/false);
		}
	      else
		{
		  tree t;

		  /* Indicate that the old function template is a partial
		     instantiation.  */
		  DECL_TEMPLATE_INFO (DECL_TEMPLATE_RESULT (old_decl))
		    = new_friend_result_template_info;

		  gcc_assert (new_template
			      == most_general_template (new_template));
		  gcc_assert (new_template != old_decl);

		  /* Reassign any specializations already in the hash table
		     to the new more general template, and add the
		     additional template args.  */
		  for (t = DECL_TEMPLATE_INSTANTIATIONS (old_decl);
		       t != NULL_TREE;
		       t = TREE_CHAIN (t))
		    {
		      tree spec = TREE_VALUE (t);
		      spec_entry elt;

		      elt.tmpl = old_decl;
		      elt.args = DECL_TI_ARGS (spec);
		      elt.spec = NULL_TREE;

		      decl_specializations->remove_elt (&elt);

		      DECL_TI_ARGS (spec)
			= add_outermost_template_args (new_args,
						       DECL_TI_ARGS (spec));

		      register_specialization
			(spec, new_template, DECL_TI_ARGS (spec), true, 0);

		    }
		  DECL_TEMPLATE_INSTANTIATIONS (old_decl) = NULL_TREE;
		}
	    }

	  /* The information from NEW_FRIEND has been merged into OLD_DECL
	     by duplicate_decls.  */
	  new_friend = old_decl;
	}

      /* We've just introduced a namespace-scope function in the purview
	 without necessarily having opened the enclosing namespace, so
	 make sure the namespace is in the purview now too.  */
      if (modules_p ()
	  && DECL_MODULE_PURVIEW_P (STRIP_TEMPLATE (new_friend))
	  && TREE_CODE (DECL_CONTEXT (new_friend)) == NAMESPACE_DECL)
	DECL_MODULE_PURVIEW_P (DECL_CONTEXT (new_friend)) = true;
    }
  else
    {
      tree context = DECL_CONTEXT (new_friend);
      bool dependent_p;

      /* In the code
	   template <class T> class C {
	     template <class U> friend void C1<U>::f (); // case 1
	     friend void C2<T>::f ();			 // case 2
	   };
	 we only need to make sure CONTEXT is a complete type for
	 case 2.  To distinguish between the two cases, we note that
	 CONTEXT of case 1 remains dependent type after tsubst while
	 this isn't true for case 2.  */
      ++processing_template_decl;
      dependent_p = dependent_type_p (context);
      --processing_template_decl;

      if (!dependent_p
	  && !complete_type_or_else (context, NULL_TREE))
	return error_mark_node;

      if (COMPLETE_TYPE_P (context))
	{
	  tree fn = new_friend;
	  /* do_friend adds the TEMPLATE_DECL for any member friend
	     template even if it isn't a member template, i.e.
	       template <class T> friend A<T>::f();
	     Look through it in that case.  */
	  if (TREE_CODE (fn) == TEMPLATE_DECL
	      && !PRIMARY_TEMPLATE_P (fn))
	    fn = DECL_TEMPLATE_RESULT (fn);
	  /* Check to see that the declaration is really present, and,
	     possibly obtain an improved declaration.  */
	  fn = check_classfn (context, fn, NULL_TREE);

	  if (fn)
	    new_friend = fn;
	}
    }

  return new_friend;
}

/* FRIEND_TMPL is a friend TEMPLATE_DECL.  ARGS is the vector of
   template arguments, as for tsubst.

   Returns an appropriate tsubst'd friend type or error_mark_node on
   failure.  */

static tree
tsubst_friend_class (tree friend_tmpl, tree args)
{
  tree tmpl;

  if (DECL_TEMPLATE_TEMPLATE_PARM_P (friend_tmpl))
    {
      tmpl = tsubst (TREE_TYPE (friend_tmpl), args, tf_none, NULL_TREE);
      return TREE_TYPE (tmpl);
    }

  tree context = CP_DECL_CONTEXT (friend_tmpl);
  if (TREE_CODE (context) == NAMESPACE_DECL)
    push_nested_namespace (context);
  else
    {
      context = tsubst (context, args, tf_error, NULL_TREE);
      push_nested_class (context);
    }

  tmpl = lookup_name (DECL_NAME (friend_tmpl), LOOK_where::CLASS_NAMESPACE,
		      LOOK_want::NORMAL | LOOK_want::HIDDEN_FRIEND);

  if (tmpl && DECL_CLASS_TEMPLATE_P (tmpl))
    {
      /* The friend template has already been declared.  Just
	 check to see that the declarations match, and install any new
	 default parameters.  We must tsubst the default parameters,
	 of course.  We only need the innermost template parameters
	 because that is all that redeclare_class_template will look
	 at.  */
      if (TMPL_PARMS_DEPTH (DECL_TEMPLATE_PARMS (friend_tmpl))
	  > TMPL_ARGS_DEPTH (args))
	{
	  tree parms = tsubst_template_parms (DECL_TEMPLATE_PARMS (friend_tmpl),
					      args, tf_warning_or_error);
	  tsubst_each_template_parm_constraints (parms, args,
						 tf_warning_or_error);
          location_t saved_input_location = input_location;
          input_location = DECL_SOURCE_LOCATION (friend_tmpl);
	  tree cons = get_constraints (friend_tmpl);
	  ++processing_template_decl;
	  cons = tsubst_constraint_info (cons, args, tf_warning_or_error,
					 DECL_FRIEND_CONTEXT (friend_tmpl));
	  --processing_template_decl;
          redeclare_class_template (TREE_TYPE (tmpl), parms, cons);
          input_location = saved_input_location;
	}
    }
  else
    {
      /* The friend template has not already been declared.  In this
	 case, the instantiation of the template class will cause the
	 injection of this template into the namespace scope.  */
      tmpl = tsubst (friend_tmpl, args, tf_warning_or_error, NULL_TREE);

      if (tmpl != error_mark_node)
	{
	  /* The new TMPL is not an instantiation of anything, so we
	     forget its origins.  We don't reset CLASSTYPE_TI_TEMPLATE
	     for the new type because that is supposed to be the
	     corresponding template decl, i.e., TMPL.  */
	  DECL_USE_TEMPLATE (tmpl) = 0;
	  DECL_TEMPLATE_INFO (tmpl) = NULL_TREE;
	  CLASSTYPE_USE_TEMPLATE (TREE_TYPE (tmpl)) = 0;
	  CLASSTYPE_TI_ARGS (TREE_TYPE (tmpl))
	    = INNERMOST_TEMPLATE_ARGS (CLASSTYPE_TI_ARGS (TREE_TYPE (tmpl)));

	  /* Substitute into and set the constraints on the new declaration.  */
	  if (tree ci = get_constraints (friend_tmpl))
	    {
	      ++processing_template_decl;
	      ci = tsubst_constraint_info (ci, args, tf_warning_or_error,
					   DECL_FRIEND_CONTEXT (friend_tmpl));
	      --processing_template_decl;
	      set_constraints (tmpl, ci);
	      tsubst_each_template_parm_constraints (DECL_TEMPLATE_PARMS (tmpl),
						     args, tf_warning_or_error);
	    }

	  /* Inject this template into the enclosing namspace scope.  */
	  tmpl = pushdecl_namespace_level (tmpl, /*hiding=*/true);
	}
    }

  if (TREE_CODE (context) == NAMESPACE_DECL)
    pop_nested_namespace (context);
  else
    pop_nested_class ();

  return TREE_TYPE (tmpl);
}

/* Returns zero if TYPE cannot be completed later due to circularity.
   Otherwise returns one.  */

static int
can_complete_type_without_circularity (tree type)
{
  if (type == NULL_TREE || type == error_mark_node)
    return 0;
  else if (COMPLETE_TYPE_P (type))
    return 1;
  else if (TREE_CODE (type) == ARRAY_TYPE)
    return can_complete_type_without_circularity (TREE_TYPE (type));
  else if (CLASS_TYPE_P (type)
	   && TYPE_BEING_DEFINED (TYPE_MAIN_VARIANT (type)))
    return 0;
  else
    return 1;
}

static tree tsubst_omp_clauses (tree, enum c_omp_region_type, tree,
				tsubst_flags_t, tree);

/* Instantiate the contract statement.  */

static tree
tsubst_contract (tree decl, tree t, tree args, tsubst_flags_t complain,
		 tree in_decl)
{
  tree type = decl ? TREE_TYPE (TREE_TYPE (decl)) : NULL_TREE;
  bool auto_p  = type_uses_auto (type);

  tree r = copy_node (t);

  /* Rebuild the result variable.  */
  if (type && POSTCONDITION_P (t) && POSTCONDITION_IDENTIFIER (t))
    {
      tree oldvar = POSTCONDITION_IDENTIFIER (t);

      tree newvar = copy_node (oldvar);
      TREE_TYPE (newvar) = type;
      DECL_CONTEXT (newvar) = decl;
      POSTCONDITION_IDENTIFIER (r) = newvar;

      /* Make sure the postcondition is valid.  */
      location_t loc = DECL_SOURCE_LOCATION (oldvar);
      if (!auto_p)
	if (!check_postcondition_result (decl, type, loc))
	  return invalidate_contract (r);

      /* Make the variable available for lookup.  */
      register_local_specialization (newvar, oldvar);
    }

  /* Instantiate the condition.  If the return type is undeduced, process
     the expression as if inside a template to avoid spurious type errors.  */
  if (auto_p)
    ++processing_template_decl;
  ++processing_contract_condition;
  CONTRACT_CONDITION (r)
      = tsubst_expr (CONTRACT_CONDITION (t), args, complain, in_decl);
  --processing_contract_condition;
  if (auto_p)
    --processing_template_decl;

  /* And the comment.  */
  CONTRACT_COMMENT (r)
      = tsubst_expr (CONTRACT_COMMENT (r), args, complain, in_decl);

  return r;
}

/* Update T by instantiating its contract attribute.  */

static void
tsubst_contract_attribute (tree decl, tree t, tree args,
			   tsubst_flags_t complain, tree in_decl)
{
  /* For non-specializations, adjust the current declaration to the most general
     version of in_decl. Because we defer the instantiation of contracts as long
     as possible, they are still written in terms of the parameters (and return
     type) of the most general template.  */
  tree tmpl = DECL_TI_TEMPLATE (in_decl);
  if (!DECL_TEMPLATE_SPECIALIZATION (tmpl))
    in_decl = DECL_TEMPLATE_RESULT (most_general_template (in_decl));
  local_specialization_stack specs (lss_copy);
  register_parameter_specializations (in_decl, decl);

  /* Get the contract to be instantiated.  */
  tree contract = CONTRACT_STATEMENT (t);

  /* Use the complete set of template arguments for instantiation. The
     contract may not have been instantiated and still refer to outer levels
     of template parameters.  */
  args = DECL_TI_ARGS (decl);

  /* For member functions, make this available for semantic analysis.  */
  tree save_ccp = current_class_ptr;
  tree save_ccr = current_class_ref;
  if (DECL_NONSTATIC_MEMBER_FUNCTION_P (decl))
    {
      tree arg_types = TYPE_ARG_TYPES (TREE_TYPE (decl));
      tree this_type = TREE_TYPE (TREE_VALUE (arg_types));
      inject_this_parameter (this_type, cp_type_quals (this_type));
    }

  contract = tsubst_contract (decl, contract, args, complain, in_decl);

  current_class_ptr = save_ccp;
  current_class_ref = save_ccr;

  /* Rebuild the attribute.  */
  TREE_VALUE (t) = build_tree_list (NULL_TREE, contract);
}

/* Rebuild the attribute list for DECL, substituting into contracts
   as needed.  */

void
tsubst_contract_attributes (tree decl, tree args, tsubst_flags_t complain, tree in_decl)
{
  tree list = copy_list (DECL_ATTRIBUTES (decl));
  for (tree attr = list; attr; attr = CONTRACT_CHAIN (attr))
    {
      if (cxx_contract_attribute_p (attr))
	tsubst_contract_attribute (decl, attr, args, complain, in_decl);
    }
  DECL_ATTRIBUTES (decl) = list;
}

/* Instantiate a single dependent attribute T (a TREE_LIST), and return either
   T or a new TREE_LIST, possibly a chain in the case of a pack expansion.  */

static tree
tsubst_attribute (tree t, tree *decl_p, tree args,
		  tsubst_flags_t complain, tree in_decl)
{
  gcc_assert (ATTR_IS_DEPENDENT (t));

  /* Note that contract attributes are never substituted from this function.
     Their instantiation is triggered by regenerate_from_template_decl when
     we instantiate the body of the function.  */

  tree val = TREE_VALUE (t);
  if (val == NULL_TREE)
    /* Nothing to do.  */;
  else if ((flag_openmp || flag_openmp_simd)
	   && is_attribute_p ("omp declare simd",
			      get_attribute_name (t)))
    {
      tree clauses = TREE_VALUE (val);
      clauses = tsubst_omp_clauses (clauses, C_ORT_OMP_DECLARE_SIMD, args,
				    complain, in_decl);
      c_omp_declare_simd_clauses_to_decls (*decl_p, clauses);
      clauses = finish_omp_clauses (clauses, C_ORT_OMP_DECLARE_SIMD);
      tree parms = DECL_ARGUMENTS (*decl_p);
      clauses
	= c_omp_declare_simd_clauses_to_numbers (parms, clauses);
      if (clauses)
	val = build_tree_list (NULL_TREE, clauses);
      else
	val = NULL_TREE;
    }
  else if (flag_openmp
	   && is_attribute_p ("omp declare variant base",
			      get_attribute_name (t)))
    {
      ++cp_unevaluated_operand;
      tree varid = tsubst_expr (TREE_PURPOSE (val), args, complain, in_decl);
      --cp_unevaluated_operand;
      tree chain = TREE_CHAIN (val);
      location_t match_loc = cp_expr_loc_or_input_loc (TREE_PURPOSE (chain));
      tree ctx = copy_list (TREE_VALUE (val));
      tree simd = get_identifier ("simd");
      tree score = get_identifier (" score");
      tree condition = get_identifier ("condition");
      for (tree t1 = ctx; t1; t1 = TREE_CHAIN (t1))
	{
	  const char *set = IDENTIFIER_POINTER (TREE_PURPOSE (t1));
	  TREE_VALUE (t1) = copy_list (TREE_VALUE (t1));
	  for (tree t2 = TREE_VALUE (t1); t2; t2 = TREE_CHAIN (t2))
	    {
	      if (TREE_PURPOSE (t2) == simd && set[0] == 'c')
		{
		  tree clauses = TREE_VALUE (t2);
		  clauses = tsubst_omp_clauses (clauses,
						C_ORT_OMP_DECLARE_SIMD, args,
						complain, in_decl);
		  c_omp_declare_simd_clauses_to_decls (*decl_p, clauses);
		  clauses = finish_omp_clauses (clauses, C_ORT_OMP_DECLARE_SIMD);
		  TREE_VALUE (t2) = clauses;
		}
	      else
		{
		  TREE_VALUE (t2) = copy_list (TREE_VALUE (t2));
		  for (tree t3 = TREE_VALUE (t2); t3; t3 = TREE_CHAIN (t3))
		    if (TREE_VALUE (t3))
		      {
			bool allow_string
			  = ((TREE_PURPOSE (t2) != condition || set[0] != 'u')
			     && TREE_PURPOSE (t3) != score);
			tree v = TREE_VALUE (t3);
			if (TREE_CODE (v) == STRING_CST && allow_string)
			  continue;
			v = tsubst_expr (v, args, complain, in_decl);
			v = fold_non_dependent_expr (v);
			if (!INTEGRAL_TYPE_P (TREE_TYPE (v))
			    || (TREE_PURPOSE (t3) == score
				? TREE_CODE (v) != INTEGER_CST
				: !tree_fits_shwi_p (v)))
			  {
			    location_t loc
			      = cp_expr_loc_or_loc (TREE_VALUE (t3),
						    match_loc);
			    if (TREE_PURPOSE (t3) == score)
			      error_at (loc, "score argument must be "
					     "constant integer expression");
			    else if (allow_string)
			      error_at (loc, "property must be constant "
					     "integer expression or string "
					     "literal");
			    else
			      error_at (loc, "property must be constant "
					     "integer expression");
			    return NULL_TREE;
			  }
			else if (TREE_PURPOSE (t3) == score
				 && tree_int_cst_sgn (v) < 0)
			  {
			    location_t loc
			      = cp_expr_loc_or_loc (TREE_VALUE (t3),
						    match_loc);
			    error_at (loc, "score argument must be "
					   "non-negative");
			    return NULL_TREE;
			  }
			TREE_VALUE (t3) = v;
		      }
		}
	    }
	}
      val = tree_cons (varid, ctx, chain);
    }
  /* If the first attribute argument is an identifier, don't
     pass it through tsubst.  Attributes like mode, format,
     cleanup and several target specific attributes expect it
     unmodified.  */
  else if (attribute_takes_identifier_p (get_attribute_name (t)))
    {
      tree chain
	= tsubst_expr (TREE_CHAIN (val), args, complain, in_decl);
      if (chain != TREE_CHAIN (val))
	val = tree_cons (NULL_TREE, TREE_VALUE (val), chain);
    }
  else if (PACK_EXPANSION_P (val))
    {
      /* An attribute pack expansion.  */
      tree purp = TREE_PURPOSE (t);
      tree pack = tsubst_pack_expansion (val, args, complain, in_decl);
      if (pack == error_mark_node)
	return error_mark_node;
      int len = TREE_VEC_LENGTH (pack);
      tree list = NULL_TREE;
      tree *q = &list;
      for (int i = 0; i < len; ++i)
	{
	  tree elt = TREE_VEC_ELT (pack, i);
	  *q = build_tree_list (purp, elt);
	  q = &TREE_CHAIN (*q);
	}
      return list;
    }
  else
    val = tsubst_expr (val, args, complain, in_decl);

  if (val == error_mark_node)
    return error_mark_node;
  if (val != TREE_VALUE (t))
    return build_tree_list (TREE_PURPOSE (t), val);
  return t;
}

/* Instantiate any dependent attributes in ATTRIBUTES, returning either it
   unchanged or a new TREE_LIST chain.  */

static tree
tsubst_attributes (tree attributes, tree args,
		   tsubst_flags_t complain, tree in_decl)
{
  tree last_dep = NULL_TREE;

  for (tree t = attributes; t; t = TREE_CHAIN (t))
    if (ATTR_IS_DEPENDENT (t))
      {
	last_dep = t;
	attributes = copy_list (attributes);
	break;
      }

  if (last_dep)
    for (tree *p = &attributes; *p; )
      {
	tree t = *p;
	if (ATTR_IS_DEPENDENT (t))
	  {
	    tree subst = tsubst_attribute (t, NULL, args, complain, in_decl);
	    if (subst != t)
	      {
		*p = subst;
		while (*p)
		  p = &TREE_CHAIN (*p);
		*p = TREE_CHAIN (t);
		continue;
	      }
	  }
	p = &TREE_CHAIN (*p);
      }

  return attributes;
}

/* Apply any attributes which had to be deferred until instantiation
   time.  DECL_P, ATTRIBUTES and ATTR_FLAGS are as cplus_decl_attributes;
   ARGS, COMPLAIN, IN_DECL are as tsubst.  Returns true normally,
   false on error.  */

static bool
apply_late_template_attributes (tree *decl_p, tree attributes, int attr_flags,
				tree args, tsubst_flags_t complain, tree in_decl)
{
  tree t;
  tree *p;

  if (attributes == NULL_TREE)
    return true;

  if (DECL_P (*decl_p))
    {
      if (TREE_TYPE (*decl_p) == error_mark_node)
	return false;
      p = &DECL_ATTRIBUTES (*decl_p);
      /* DECL_ATTRIBUTES comes from copy_node in tsubst_decl, and is identical
         to our attributes parameter.  */
      gcc_assert (*p == attributes);
    }
  else
    {
      p = &TYPE_ATTRIBUTES (*decl_p);
      /* TYPE_ATTRIBUTES was set up (with abi_tag and may_alias) in
	 lookup_template_class_1, and should be preserved.  */
      gcc_assert (*p != attributes);
      while (*p)
	p = &TREE_CHAIN (*p);
    }

  /* save_template_attributes puts the dependent attributes at the beginning of
     the list; find the non-dependent ones.  */
  for (t = attributes; t; t = TREE_CHAIN (t))
    if (!ATTR_IS_DEPENDENT (t))
      break;
  tree nondep = t;

  /* Apply any non-dependent attributes.  */
  *p = nondep;

  if (nondep == attributes)
    return true;

  /* And then any dependent ones.  */
  tree late_attrs = NULL_TREE;
  tree *q = &late_attrs;
  for (t = attributes; t != nondep; t = TREE_CHAIN (t))
    {
      *q = tsubst_attribute (t, decl_p, args, complain, in_decl);
      if (*q == error_mark_node)
	return false;
      if (*q == t)
	{
	  *q = copy_node (t);
	  TREE_CHAIN (*q) = NULL_TREE;
	}
      while (*q)
	q = &TREE_CHAIN (*q);
    }

  /* cplus_decl_attributes can add some attributes implicitly.  For templates,
     those attributes should have been added already when those templates were
     parsed, and shouldn't be added based on from which context they are
     first time instantiated.  */
  auto o1 = make_temp_override (current_optimize_pragma, NULL_TREE);
  auto o2 = make_temp_override (optimization_current_node,
				optimization_default_node);
  auto o3 = make_temp_override (current_target_pragma, NULL_TREE);
  auto o4 = make_temp_override (scope_chain->omp_declare_target_attribute,
				NULL);
  auto o5 = make_temp_override (scope_chain->omp_begin_assumes, NULL);

  cplus_decl_attributes (decl_p, late_attrs, attr_flags);

  return true;
}

/* The template TMPL is being instantiated with the template arguments TARGS.
   Perform the access checks that we deferred when parsing the template.  */

static void
perform_instantiation_time_access_checks (tree tmpl, tree targs)
{
  unsigned i;
  deferred_access_check *chk;

  if (!CLASS_TYPE_P (tmpl) && TREE_CODE (tmpl) != FUNCTION_DECL)
    return;

  if (vec<deferred_access_check, va_gc> *access_checks
      = TI_DEFERRED_ACCESS_CHECKS (get_template_info (tmpl)))
    FOR_EACH_VEC_ELT (*access_checks, i, chk)
      {
	tree decl = chk->decl;
	tree diag_decl = chk->diag_decl;
	tree type_scope = TREE_TYPE (chk->binfo);

	if (uses_template_parms (type_scope))
	  type_scope = tsubst (type_scope, targs, tf_error, NULL_TREE);

	/* Make access check error messages point to the location
	   of the use of the typedef.  */
	iloc_sentinel ils (chk->loc);
	perform_or_defer_access_check (TYPE_BINFO (type_scope),
				       decl, diag_decl, tf_warning_or_error);
      }
}

tree
instantiate_class_template (tree type)
{
  auto_timevar tv (TV_TEMPLATE_INST);

  tree templ, args, pattern, t, member;
  tree typedecl;
  tree pbinfo;
  tree base_list;
  unsigned int saved_maximum_field_alignment;
  tree fn_context;

  if (type == error_mark_node)
    return error_mark_node;

  if (COMPLETE_OR_OPEN_TYPE_P (type)
      || (uses_template_parms (type)
	  && !TYPE_FUNCTION_SCOPE_P (type)))
    return type;

  /* Figure out which template is being instantiated.  */
  templ = most_general_template (CLASSTYPE_TI_TEMPLATE (type));
  gcc_assert (TREE_CODE (templ) == TEMPLATE_DECL);

  /* Mark the type as in the process of being defined.  */
  TYPE_BEING_DEFINED (type) = 1;

  /* We may be in the middle of deferred access check.  Disable
     it now.  */
  deferring_access_check_sentinel acs (dk_no_deferred);

  /* Determine what specialization of the original template to
     instantiate.  */
  t = most_specialized_partial_spec (type, tf_warning_or_error);
  if (t == error_mark_node)
    return error_mark_node;
  else if (t)
    {
      /* This TYPE is actually an instantiation of a partial
	 specialization.  We replace the innermost set of ARGS with
	 the arguments appropriate for substitution.  For example,
	 given:

	   template <class T> struct S {};
	   template <class T> struct S<T*> {};

	 and supposing that we are instantiating S<int*>, ARGS will
	 presently be {int*} -- but we need {int}.  */
      pattern = TREE_TYPE (TI_TEMPLATE (t));
      args = TI_ARGS (t);
    }
  else
    {
      pattern = TREE_TYPE (templ);
      args = CLASSTYPE_TI_ARGS (type);
    }

  /* If the template we're instantiating is incomplete, then clearly
     there's nothing we can do.  */
  if (!COMPLETE_TYPE_P (pattern))
    {
      /* We can try again later.  */
      TYPE_BEING_DEFINED (type) = 0;
      return type;
    }

  /* If we've recursively instantiated too many templates, stop.  */
  if (! push_tinst_level (type))
    return type;

  int saved_unevaluated_operand = cp_unevaluated_operand;
  int saved_inhibit_evaluation_warnings = c_inhibit_evaluation_warnings;

  fn_context = decl_function_context (TYPE_MAIN_DECL (type));
  /* Also avoid push_to_top_level for a lambda in an NSDMI.  */
  if (!fn_context && LAMBDA_TYPE_P (type) && TYPE_CLASS_SCOPE_P (type))
    fn_context = error_mark_node;
  if (!fn_context)
    push_to_top_level ();
  else
    {
      cp_unevaluated_operand = 0;
      c_inhibit_evaluation_warnings = 0;
    }

  mark_template_arguments_used (templ, CLASSTYPE_TI_ARGS (type));

  /* Use #pragma pack from the template context.  */
  saved_maximum_field_alignment = maximum_field_alignment;
  maximum_field_alignment = TYPE_PRECISION (pattern);

  SET_CLASSTYPE_INTERFACE_UNKNOWN (type);

  /* Set the input location to the most specialized template definition.
     This is needed if tsubsting causes an error.  */
  typedecl = TYPE_MAIN_DECL (pattern);
  input_location = DECL_SOURCE_LOCATION (TYPE_NAME (type)) =
    DECL_SOURCE_LOCATION (typedecl);

  set_instantiating_module (TYPE_NAME (type));

  TYPE_PACKED (type) = TYPE_PACKED (pattern);
  SET_TYPE_ALIGN (type, TYPE_ALIGN (pattern));
  TYPE_USER_ALIGN (type) = TYPE_USER_ALIGN (pattern);
  CLASSTYPE_NON_AGGREGATE (type) = CLASSTYPE_NON_AGGREGATE (pattern);
  if (ANON_AGGR_TYPE_P (pattern))
    SET_ANON_AGGR_TYPE_P (type);
  if (CLASSTYPE_VISIBILITY_SPECIFIED (pattern))
    {
      CLASSTYPE_VISIBILITY_SPECIFIED (type) = 1;
      CLASSTYPE_VISIBILITY (type) = CLASSTYPE_VISIBILITY (pattern);
      /* Adjust visibility for template arguments.  */
      determine_visibility (TYPE_MAIN_DECL (type));
    }
  if (CLASS_TYPE_P (type))
    CLASSTYPE_FINAL (type) = CLASSTYPE_FINAL (pattern);

  pbinfo = TYPE_BINFO (pattern);

  /* We should never instantiate a nested class before its enclosing
     class; we need to look up the nested class by name before we can
     instantiate it, and that lookup should instantiate the enclosing
     class.  */
  gcc_assert (!DECL_CLASS_SCOPE_P (TYPE_MAIN_DECL (pattern))
	      || COMPLETE_OR_OPEN_TYPE_P (TYPE_CONTEXT (type)));

  base_list = NULL_TREE;
  /* Defer access checking while we substitute into the types named in
     the base-clause.  */
  push_deferring_access_checks (dk_deferred);
  if (BINFO_N_BASE_BINFOS (pbinfo))
    {
      tree pbase_binfo;
      int i;

      /* Substitute into each of the bases to determine the actual
	 basetypes.  */
      for (i = 0; BINFO_BASE_ITERATE (pbinfo, i, pbase_binfo); i++)
	{
	  tree base;
	  tree access = BINFO_BASE_ACCESS (pbinfo, i);
          tree expanded_bases = NULL_TREE;
          int idx, len = 1;

          if (PACK_EXPANSION_P (BINFO_TYPE (pbase_binfo)))
            {
              expanded_bases = 
		tsubst_pack_expansion (BINFO_TYPE (pbase_binfo),
				       args, tf_error, NULL_TREE);
              if (expanded_bases == error_mark_node)
                continue;

              len = TREE_VEC_LENGTH (expanded_bases);
            }

          for (idx = 0; idx < len; idx++)
            {
              if (expanded_bases)
                /* Extract the already-expanded base class.  */
                base = TREE_VEC_ELT (expanded_bases, idx);
              else
                /* Substitute to figure out the base class.  */
                base = tsubst (BINFO_TYPE (pbase_binfo), args, tf_error, 
                               NULL_TREE);

              if (base == error_mark_node)
                continue;

              base_list = tree_cons (access, base, base_list);
              if (BINFO_VIRTUAL_P (pbase_binfo))
                TREE_TYPE (base_list) = integer_type_node;
            }
	}

      /* The list is now in reverse order; correct that.  */
      base_list = nreverse (base_list);
    }
  /* Now call xref_basetypes to set up all the base-class
     information.  */
  xref_basetypes (type, base_list);

  apply_late_template_attributes (&type, TYPE_ATTRIBUTES (pattern),
				  (int) ATTR_FLAG_TYPE_IN_PLACE,
				  args, tf_error, NULL_TREE);
  fixup_attribute_variants (type);

  /* Now that our base classes are set up, enter the scope of the
     class, so that name lookups into base classes, etc. will work
     correctly.  This is precisely analogous to what we do in
     begin_class_definition when defining an ordinary non-template
     class, except we also need to push the enclosing classes.  */
  push_nested_class (type);

  /* Now check accessibility of the types named in its base-clause,
     relative to the scope of the class.  */
  pop_to_parent_deferring_access_checks ();

  /* A vector to hold members marked with attribute used. */
  auto_vec<tree> used;

  /* Now members are processed in the order of declaration.  */
  for (member = CLASSTYPE_DECL_LIST (pattern);
       member; member = TREE_CHAIN (member))
    {
      tree t = TREE_VALUE (member);

      if (TREE_PURPOSE (member))
	{
	  if (TYPE_P (t))
	    {
	      if (LAMBDA_TYPE_P (t))
		/* A closure type for a lambda in an NSDMI or default argument.
		   Ignore it; it will be regenerated when needed.  */
		continue;

	      /* If the member is a class template, we've
		 already substituted its type.  */
	      if (CLASS_TYPE_P (t)
		  && CLASSTYPE_IS_TEMPLATE (t))
		continue;

	      tree newtag = tsubst (t, args, tf_error, NULL_TREE);
	      if (newtag == error_mark_node)
		continue;

	      if (TREE_CODE (newtag) != ENUMERAL_TYPE)
		{
		  tree name = TYPE_IDENTIFIER (t);

		  /* Now, install the tag.  We don't use pushtag
		     because that does too much work -- creating an
		     implicit typedef, which we've already done.  */
		  set_identifier_type_value (name, TYPE_NAME (newtag));
		  maybe_add_class_template_decl_list (type, newtag, false);
		  TREE_PUBLIC (TYPE_NAME (newtag)) = true;
		  determine_visibility (TYPE_NAME (newtag));
		}
	    }
	  else if (DECL_DECLARES_FUNCTION_P (t))
	    {
	      tree r;

	      if (TREE_CODE (t) == TEMPLATE_DECL)
		++processing_template_decl;
	      r = tsubst (t, args, tf_error, NULL_TREE);
	      if (TREE_CODE (t) == TEMPLATE_DECL)
		--processing_template_decl;

	      set_current_access_from_decl (r);
	      finish_member_declaration (r);
	      /* Instantiate members marked with attribute used.  */
	      if (r != error_mark_node && DECL_PRESERVE_P (r))
		used.safe_push (r);
	      if (TREE_CODE (r) == FUNCTION_DECL
		  && DECL_OMP_DECLARE_REDUCTION_P (r))
		cp_check_omp_declare_reduction (r);
	    }
	  else if ((DECL_CLASS_TEMPLATE_P (t) || DECL_IMPLICIT_TYPEDEF_P (t))
		   && LAMBDA_TYPE_P (TREE_TYPE (t)))
	    /* A closure type for a lambda in an NSDMI or default argument.
	       Ignore it; it will be regenerated when needed.  */;
	  else
	    {
	      /* Build new TYPE_FIELDS.  */
              if (TREE_CODE (t) == STATIC_ASSERT)
		tsubst_expr (t, args, tf_warning_or_error, NULL_TREE);
	      else if (TREE_CODE (t) != CONST_DECL)
		{
		  tree r;
		  tree vec = NULL_TREE;
		  int len = 1;

		  gcc_checking_assert (TREE_CODE (t) != CONST_DECL);
		  /* The file and line for this declaration, to
		     assist in error message reporting.  Since we
		     called push_tinst_level above, we don't need to
		     restore these.  */
		  input_location = DECL_SOURCE_LOCATION (t);

		  if (TREE_CODE (t) == TEMPLATE_DECL)
		    ++processing_template_decl;
		  r = tsubst (t, args, tf_warning_or_error, NULL_TREE);
		  if (TREE_CODE (t) == TEMPLATE_DECL)
		    --processing_template_decl;

		  if (TREE_CODE (r) == TREE_VEC)
		    {
		      /* A capture pack became multiple fields.  */
		      vec = r;
		      len = TREE_VEC_LENGTH (vec);
		    }

		  for (int i = 0; i < len; ++i)
		    {
		      if (vec)
			r = TREE_VEC_ELT (vec, i);
		      if (VAR_P (r))
			{
			  /* In [temp.inst]:

			     [t]he initialization (and any associated
			     side-effects) of a static data member does
			     not occur unless the static data member is
			     itself used in a way that requires the
			     definition of the static data member to
			     exist.

			     Therefore, we do not substitute into the
			     initialized for the static data member here.  */
			  finish_static_data_member_decl
			    (r,
			     /*init=*/NULL_TREE,
			     /*init_const_expr_p=*/false,
			     /*asmspec_tree=*/NULL_TREE,
			     /*flags=*/0);
			  /* Instantiate members marked with attribute used. */
			  if (r != error_mark_node && DECL_PRESERVE_P (r))
			    used.safe_push (r);
			}
		      else if (TREE_CODE (r) == FIELD_DECL)
			{
			  /* Determine whether R has a valid type and can be
			     completed later.  If R is invalid, then its type
			     is replaced by error_mark_node.  */
			  tree rtype = TREE_TYPE (r);
			  if (can_complete_type_without_circularity (rtype))
			    complete_type (rtype);

			  if (!complete_or_array_type_p (rtype))
			    {
			      /* If R's type couldn't be completed and
				 it isn't a flexible array member (whose
				 type is incomplete by definition) give
				 an error.  */
			      cxx_incomplete_type_error (r, rtype);
			      TREE_TYPE (r) = error_mark_node;
			    }
			  else if (TREE_CODE (rtype) == ARRAY_TYPE
				   && TYPE_DOMAIN (rtype) == NULL_TREE
				   && (TREE_CODE (type) == UNION_TYPE
				       || TREE_CODE (type) == QUAL_UNION_TYPE))
			    {
			      error ("flexible array member %qD in union", r);
			      TREE_TYPE (r) = error_mark_node;
			    }
			  else if (!verify_type_context (input_location,
							 TCTX_FIELD, rtype))
			    TREE_TYPE (r) = error_mark_node;
			}

		      /* If it is a TYPE_DECL for a class-scoped
			 ENUMERAL_TYPE, such a thing will already have
			 been added to the field list by tsubst_enum
			 in finish_member_declaration case above.  */
		      if (!(TREE_CODE (r) == TYPE_DECL
			    && TREE_CODE (TREE_TYPE (r)) == ENUMERAL_TYPE
			    && DECL_ARTIFICIAL (r)))
			{
			  set_current_access_from_decl (r);
			  finish_member_declaration (r);
			}
		    }
		}
	    }
	}
      else
	{
	  if (TYPE_P (t) || DECL_CLASS_TEMPLATE_P (t)
	      || DECL_TEMPLATE_TEMPLATE_PARM_P (t))
	    {
	      /* Build new CLASSTYPE_FRIEND_CLASSES.  */

	      tree friend_type = t;
	      if (TREE_CODE (friend_type) == TEMPLATE_DECL)
		{
		  /* template <class T> friend class C;  */
		  friend_type = tsubst_friend_class (friend_type, args);
		}
	      else if (TREE_CODE (friend_type) == UNBOUND_CLASS_TEMPLATE)
		{
		  /* template <class T> friend class C::D;  */
		  friend_type = tsubst (friend_type, args,
					tf_warning_or_error, NULL_TREE);
		  if (TREE_CODE (friend_type) == TEMPLATE_DECL)
		    friend_type = TREE_TYPE (friend_type);
		}
	      else if (TREE_CODE (friend_type) == TYPENAME_TYPE
		       || TREE_CODE (friend_type) == TEMPLATE_TYPE_PARM)
		{
		  /* This could be either

		       friend class T::C;

		     when dependent_type_p is false or

		       template <class U> friend class T::C;

		     otherwise.  */
		  /* Bump processing_template_decl in case this is something like
		     template <class T> friend struct A<T>::B.  */
		  ++processing_template_decl;
		  friend_type = tsubst (friend_type, args,
					tf_warning_or_error, NULL_TREE);
		  --processing_template_decl;
		}
	      else if (uses_template_parms (friend_type))
		/* friend class C<T>;  */
		friend_type = tsubst (friend_type, args,
				      tf_warning_or_error, NULL_TREE);
	      
	      /* Otherwise it's

		   friend class C;

		 where C is already declared or

		   friend class C<int>;

		 We don't have to do anything in these cases.  */

	      if (friend_type != error_mark_node)
		make_friend_class (type, friend_type, /*complain=*/false);
	    }
	  else
	    {
	      /* Build new DECL_FRIENDLIST.  */
	      tree r;

	      /* The file and line for this declaration, to
		 assist in error message reporting.  Since we
		 called push_tinst_level above, we don't need to
		 restore these.  */
	      input_location = DECL_SOURCE_LOCATION (t);

	      if (TREE_CODE (t) == TEMPLATE_DECL)
		{
		  ++processing_template_decl;
		  push_deferring_access_checks (dk_no_check);
		}

	      r = tsubst_friend_function (t, args);
	      add_friend (type, r, /*complain=*/false);
	      if (TREE_CODE (t) == TEMPLATE_DECL)
		{
		  pop_deferring_access_checks ();
		  --processing_template_decl;
		}
	    }
	}
    }

  if (fn_context)
    {
      /* Restore these before substituting into the lambda capture
	 initializers.  */
      cp_unevaluated_operand = saved_unevaluated_operand;
      c_inhibit_evaluation_warnings = saved_inhibit_evaluation_warnings;
    }

  /* Set the file and line number information to whatever is given for
     the class itself.  This puts error messages involving generated
     implicit functions at a predictable point, and the same point
     that would be used for non-template classes.  */
  input_location = DECL_SOURCE_LOCATION (typedecl);

  unreverse_member_declarations (type);
  finish_struct_1 (type);
  TYPE_BEING_DEFINED (type) = 0;

  /* Remember if instantiating this class ran into errors, so we can avoid
     instantiating member functions in limit_bad_template_recursion.  We set
     this flag even if the problem was in another instantiation triggered by
     this one, as that will likely also cause trouble for member functions.  */
  if (errorcount + sorrycount > current_tinst_level->errors)
    CLASSTYPE_ERRONEOUS (type) = true;

  /* We don't instantiate default arguments for member functions.  14.7.1:

     The implicit instantiation of a class template specialization causes
     the implicit instantiation of the declarations, but not of the
     definitions or default arguments, of the class member functions,
     member classes, static data members and member templates....  */

  perform_instantiation_time_access_checks (pattern, args);
  perform_deferred_access_checks (tf_warning_or_error);

  /* Now that we've gone through all the members, instantiate those
     marked with attribute used.  We must do this in the context of
     the class -- not the context we pushed from, as that might be
     inside a template and change the behaviour of mark_used.  */
  for (tree x : used)
    mark_used (x);

  pop_nested_class ();
  maximum_field_alignment = saved_maximum_field_alignment;
  if (!fn_context)
    pop_from_top_level ();
  pop_tinst_level ();

  /* The vtable for a template class can be emitted in any translation
     unit in which the class is instantiated.  When there is no key
     method, however, finish_struct_1 will already have added TYPE to
     the keyed_classes.  */
  if (TYPE_CONTAINS_VPTR_P (type) && CLASSTYPE_KEY_METHOD (type))
    vec_safe_push (keyed_classes, type);

  return type;
}

tree
tsubst_template_arg (tree t, tree args, tsubst_flags_t complain, tree in_decl)
{
  tree r;

  if (!t)
    r = t;
  else if (TYPE_P (t))
    r = tsubst (t, args, complain, in_decl);
  else
    {
      if (!(complain & tf_warning))
	++c_inhibit_evaluation_warnings;
      r = tsubst_expr (t, args, complain, in_decl);
      if (!(complain & tf_warning))
	--c_inhibit_evaluation_warnings;
    }

  return r;
}

/* Given a function parameter pack TMPL_PARM and some function parameters
   instantiated from it at *SPEC_P, return a NONTYPE_ARGUMENT_PACK of them
   and set *SPEC_P to point at the next point in the list.  */

tree
extract_fnparm_pack (tree tmpl_parm, tree *spec_p)
{
  /* Collect all of the extra "packed" parameters into an
     argument pack.  */
  tree argpack;
  tree spec_parm = *spec_p;
  int len;

  for (len = 0; spec_parm; ++len, spec_parm = TREE_CHAIN (spec_parm))
    if (tmpl_parm
	&& !function_parameter_expanded_from_pack_p (spec_parm, tmpl_parm))
      break;

  spec_parm = *spec_p;
  if (len == 1 && DECL_PACK_P (spec_parm))
    {
      /* The instantiation is still a parameter pack; don't wrap it in a
	 NONTYPE_ARGUMENT_PACK.  */
      argpack = spec_parm;
      spec_parm = DECL_CHAIN (spec_parm);
    }
  else
    {
      /* Fill in PARMVEC with all of the parameters.  */
      tree parmvec = make_tree_vec (len);
      argpack = make_node (NONTYPE_ARGUMENT_PACK);
      for (int i = 0; i < len; i++)
	{
	  tree elt = spec_parm;
	  if (DECL_PACK_P (elt))
	    elt = make_pack_expansion (elt);
	  TREE_VEC_ELT (parmvec, i) = elt;
	  spec_parm = DECL_CHAIN (spec_parm);
	}

      /* Build the argument packs.  */
      ARGUMENT_PACK_ARGS (argpack) = parmvec;
    }
  *spec_p = spec_parm;

  return argpack;
}

/* Give a chain SPEC_PARM of PARM_DECLs, pack them into a
   NONTYPE_ARGUMENT_PACK.  */

static tree
make_fnparm_pack (tree spec_parm)
{
  return extract_fnparm_pack (NULL_TREE, &spec_parm);
}

/* Return 1 if the Ith element of the argument pack ARG_PACK is a
   pack expansion with no extra args, 2 if it has extra args, or 0
   if it is not a pack expansion.  */

static int
argument_pack_element_is_expansion_p (tree arg_pack, int i)
{
  if (TREE_CODE (arg_pack) == ARGUMENT_PACK_SELECT)
    /* We're being called before this happens in tsubst_pack_expansion.  */
    arg_pack = ARGUMENT_PACK_SELECT_FROM_PACK (arg_pack);
  tree vec = ARGUMENT_PACK_ARGS (arg_pack);
  if (i >= TREE_VEC_LENGTH (vec))
    return 0;
  tree elt = TREE_VEC_ELT (vec, i);
  if (DECL_P (elt))
    /* A decl pack is itself an expansion.  */
    elt = TREE_TYPE (elt);
  if (!PACK_EXPANSION_P (elt))
    return 0;
  if (PACK_EXPANSION_EXTRA_ARGS (elt))
    return 2;
  return 1;
}


/* Creates and return an ARGUMENT_PACK_SELECT tree node.  */

static tree
make_argument_pack_select (tree arg_pack, unsigned index)
{
  tree aps = make_node (ARGUMENT_PACK_SELECT);

  ARGUMENT_PACK_SELECT_FROM_PACK (aps) = arg_pack;
  ARGUMENT_PACK_SELECT_INDEX (aps) = index;

  return aps;
}

/*  This is a subroutine of tsubst_pack_expansion.

    It returns TRUE if we need to use the PACK_EXPANSION_EXTRA_ARGS
    mechanism to store the (non complete list of) arguments of the
    substitution and return a non substituted pack expansion, in order
    to wait for when we have enough arguments to really perform the
    substitution.  */

static bool
use_pack_expansion_extra_args_p (tree t,
				 tree parm_packs,
				 int arg_pack_len,
				 bool has_empty_arg)
{
  if (has_empty_arg
      && PACK_EXPANSION_FORCE_EXTRA_ARGS_P (t))
    return true;

  /* If one pack has an expansion and another pack has a normal
     argument or if one pack has an empty argument and an another
     one hasn't then tsubst_pack_expansion cannot perform the
     substitution and need to fall back on the
     PACK_EXPANSION_EXTRA mechanism.  */
  if (parm_packs == NULL_TREE)
    return false;
  else if (has_empty_arg)
    {
      /* If all the actual packs are pack expansions, we can still
	 subsitute directly.  */
      for (tree p = parm_packs; p; p = TREE_CHAIN (p))
	{
	  tree a = TREE_VALUE (p);
	  if (TREE_CODE (a) == ARGUMENT_PACK_SELECT)
	    a = ARGUMENT_PACK_SELECT_FROM_PACK (a);
	  a = ARGUMENT_PACK_ARGS (a);
	  if (TREE_VEC_LENGTH (a) == 1)
	    a = TREE_VEC_ELT (a, 0);
	  if (PACK_EXPANSION_P (a))
	    continue;
	  return true;
	}
      return false;
    }

  for (int i = 0 ; i < arg_pack_len; ++i)
    {
      bool has_expansion_arg = false;
      bool has_non_expansion_arg = false;
      for (tree parm_pack = parm_packs;
	   parm_pack;
	   parm_pack = TREE_CHAIN (parm_pack))
	{
	  tree arg = TREE_VALUE (parm_pack);

	  int exp = argument_pack_element_is_expansion_p (arg, i);
	  if (exp == 2)
	    /* We can't substitute a pack expansion with extra args into
	       our pattern.  */
	    return true;
	  else if (exp)
	    has_expansion_arg = true;
	  else
	    has_non_expansion_arg = true;
	}

      if (has_expansion_arg && has_non_expansion_arg)
	{
	  gcc_checking_assert (false);
	  return true;
	}
    }
  return false;
}

/* [temp.variadic]/6 says that:

       The instantiation of a pack expansion [...]
       produces a list E1,E2, ..., En, where N is the number of elements
       in the pack expansion parameters.

   This subroutine of tsubst_pack_expansion produces one of these Ei.

   PATTERN is the pattern of the pack expansion.  PARM_PACKS is a
   TREE_LIST in which each TREE_PURPOSE is a parameter pack of
   PATTERN, and each TREE_VALUE is its corresponding argument pack.
   INDEX is the index 'i' of the element Ei to produce.  ARGS,
   COMPLAIN, and IN_DECL are the same parameters as for the
   tsubst_pack_expansion function.

   The function returns the resulting Ei upon successful completion,
   or error_mark_node.

   Note that this function possibly modifies the ARGS parameter, so
   it's the responsibility of the caller to restore it.  */

static tree
gen_elem_of_pack_expansion_instantiation (tree pattern,
					  tree parm_packs,
					  unsigned index,
					  tree args /* This parm gets
						       modified.  */,
					  tsubst_flags_t complain,
					  tree in_decl)
{
  tree t;
  bool ith_elem_is_expansion = false;

  /* For each parameter pack, change the substitution of the parameter
     pack to the ith argument in its argument pack, then expand the
     pattern.  */
  for (tree pack = parm_packs; pack; pack = TREE_CHAIN (pack))
    {
      tree parm = TREE_PURPOSE (pack);
      tree arg_pack = TREE_VALUE (pack);
      tree aps;			/* instance of ARGUMENT_PACK_SELECT.  */

      ith_elem_is_expansion |=
	argument_pack_element_is_expansion_p (arg_pack, index);

      /* Select the Ith argument from the pack.  */
      if (TREE_CODE (parm) == PARM_DECL
	  || VAR_P (parm)
	  || TREE_CODE (parm) == FIELD_DECL)
	{
	  if (index == 0)
	    {
	      aps = make_argument_pack_select (arg_pack, index);
	      if (!mark_used (parm, complain) && !(complain & tf_error))
		return error_mark_node;
	      register_local_specialization (aps, parm);
	    }
	  else
	    aps = retrieve_local_specialization (parm);
	}
      else
	{
	  int idx, level;
	  template_parm_level_and_index (parm, &level, &idx);

	  if (index == 0)
	    {
	      aps = make_argument_pack_select (arg_pack, index);
	      /* Update the corresponding argument.  */
	      TMPL_ARG (args, level, idx) = aps;
	    }
	  else
	    /* Re-use the ARGUMENT_PACK_SELECT.  */
	    aps = TMPL_ARG (args, level, idx);
	}
      ARGUMENT_PACK_SELECT_INDEX (aps) = index;
    }

  /* Substitute into the PATTERN with the (possibly altered)
     arguments.  */
  if (pattern == in_decl)
    /* Expanding a fixed parameter pack from
       coerce_template_parameter_pack.  */
    t = tsubst_decl (pattern, args, complain);
  else if (pattern == error_mark_node)
    t = error_mark_node;
  else if (!TYPE_P (pattern))
    t = tsubst_expr (pattern, args, complain, in_decl);
  else
    {
      t = tsubst (pattern, args, complain, in_decl);
      if (is_auto (t) && !ith_elem_is_expansion)
	/* When expanding the fake auto... pack expansion from add_capture, we
	   need to mark that the expansion is no longer a pack.  */
	TEMPLATE_TYPE_PARAMETER_PACK (t) = false;
    }

  /*  If the Ith argument pack element is a pack expansion, then
      the Ith element resulting from the substituting is going to
      be a pack expansion as well.  */
  if (ith_elem_is_expansion)
    t = make_pack_expansion (t, complain);

  return t;
}

/* When the unexpanded parameter pack in a fold expression expands to an empty
   sequence, the value of the expression is as follows; the program is
   ill-formed if the operator is not listed in this table.

   &&	true
   ||	false
   ,	void()  */

tree
expand_empty_fold (tree t, tsubst_flags_t complain)
{
  tree_code code = (tree_code)TREE_INT_CST_LOW (TREE_OPERAND (t, 0));
  if (!FOLD_EXPR_MODIFY_P (t))
    switch (code)
      {
      case TRUTH_ANDIF_EXPR:
	return boolean_true_node;
      case TRUTH_ORIF_EXPR:
	return boolean_false_node;
      case COMPOUND_EXPR:
	return void_node;
      default:
	break;
      }

  if (complain & tf_error)
    error_at (location_of (t),
	      "fold of empty expansion over %O", code);
  return error_mark_node;
}

/* Given a fold-expression T and a current LEFT and RIGHT operand,
   form an expression that combines the two terms using the
   operator of T. */

static tree
fold_expression (tree t, tree left, tree right, tsubst_flags_t complain)
{
  tree_code code = FOLD_EXPR_OP (t);

  tree lookups = templated_operator_saved_lookups (t);

  // Handle compound assignment operators.
  if (FOLD_EXPR_MODIFY_P (t))
    return build_x_modify_expr (input_location, left, code, right,
				lookups, complain);

  warning_sentinel s(warn_parentheses);
  switch (code)
    {
    case COMPOUND_EXPR:
      return build_x_compound_expr (input_location, left, right,
				    lookups, complain);
    default:
      return build_x_binary_op (input_location, code,
                                left, TREE_CODE (left),
                                right, TREE_CODE (right),
				lookups, /*overload=*/NULL,
                                complain);
    }
}

/* Substitute ARGS into the pack of a fold expression T. */

static inline tree
tsubst_fold_expr_pack (tree t, tree args, tsubst_flags_t complain, tree in_decl)
{
  return tsubst_pack_expansion (FOLD_EXPR_PACK (t), args, complain, in_decl);
}

/* Substitute ARGS into the pack of a fold expression T. */

static inline tree
tsubst_fold_expr_init (tree t, tree args, tsubst_flags_t complain, tree in_decl)
{
  return tsubst_expr (FOLD_EXPR_INIT (t), args, complain, in_decl);
}

/* Expand a PACK of arguments into a grouped as left fold.
   Given a pack containing elements A0, A1, ..., An and an
   operator @, this builds the expression:

      ((A0 @ A1) @ A2) ... @ An

   Note that PACK must not be empty.

   The operator is defined by the original fold expression T. */

static tree
expand_left_fold (tree t, tree pack, tsubst_flags_t complain)
{
  tree left = TREE_VEC_ELT (pack, 0);
  for (int i = 1; i < TREE_VEC_LENGTH (pack); ++i)
    {
      tree right = TREE_VEC_ELT (pack, i);
      left = fold_expression (t, left, right, complain);
    }
  return left;
}

/* Substitute into a unary left fold expression. */

static tree
tsubst_unary_left_fold (tree t, tree args, tsubst_flags_t complain,
                        tree in_decl)
{
  tree pack = tsubst_fold_expr_pack (t, args, complain, in_decl);
  if (pack == error_mark_node)
    return error_mark_node;
  if (PACK_EXPANSION_P (pack))
    {
      tree r = copy_node (t);
      FOLD_EXPR_PACK (r) = pack;
      return r;
    }
  if (TREE_VEC_LENGTH (pack) == 0)
    return expand_empty_fold (t, complain);
  else
    return expand_left_fold (t, pack, complain);
}

/* Substitute into a binary left fold expression.

   Do ths by building a single (non-empty) vector of argumnts and
   building the expression from those elements. */

static tree
tsubst_binary_left_fold (tree t, tree args, tsubst_flags_t complain,
                         tree in_decl)
{
  tree pack = tsubst_fold_expr_pack (t, args, complain, in_decl);
  if (pack == error_mark_node)
    return error_mark_node;
  tree init = tsubst_fold_expr_init (t, args, complain, in_decl);
  if (init == error_mark_node)
    return error_mark_node;

  if (PACK_EXPANSION_P (pack))
    {
      tree r = copy_node (t);
      FOLD_EXPR_PACK (r) = pack;
      FOLD_EXPR_INIT (r) = init;
      return r;
    }

  tree vec = make_tree_vec (TREE_VEC_LENGTH (pack) + 1);
  TREE_VEC_ELT (vec, 0) = init;
  for (int i = 0; i < TREE_VEC_LENGTH (pack); ++i)
    TREE_VEC_ELT (vec, i + 1) = TREE_VEC_ELT (pack, i);

  return expand_left_fold (t, vec, complain);
}

/* Expand a PACK of arguments into a grouped as right fold.
   Given a pack containing elementns A0, A1, ..., and an
   operator @, this builds the expression:

      A0@ ... (An-2 @ (An-1 @ An))

   Note that PACK must not be empty.

   The operator is defined by the original fold expression T. */

tree
expand_right_fold (tree t, tree pack, tsubst_flags_t complain)
{
  // Build the expression.
  int n = TREE_VEC_LENGTH (pack);
  tree right = TREE_VEC_ELT (pack, n - 1);
  for (--n; n != 0; --n)
    {
      tree left = TREE_VEC_ELT (pack, n - 1);
      right = fold_expression (t, left, right, complain);
    }
  return right;
}

/* Substitute into a unary right fold expression. */

static tree
tsubst_unary_right_fold (tree t, tree args, tsubst_flags_t complain,
                         tree in_decl)
{
  tree pack = tsubst_fold_expr_pack (t, args, complain, in_decl);
  if (pack == error_mark_node)
    return error_mark_node;
  if (PACK_EXPANSION_P (pack))
    {
      tree r = copy_node (t);
      FOLD_EXPR_PACK (r) = pack;
      return r;
    }
  if (TREE_VEC_LENGTH (pack) == 0)
    return expand_empty_fold (t, complain);
  else
    return expand_right_fold (t, pack, complain);
}

/* Substitute into a binary right fold expression.

   Do ths by building a single (non-empty) vector of arguments and
   building the expression from those elements. */

static tree
tsubst_binary_right_fold (tree t, tree args, tsubst_flags_t complain,
                         tree in_decl)
{
  tree pack = tsubst_fold_expr_pack (t, args, complain, in_decl);
  if (pack == error_mark_node)
    return error_mark_node;
  tree init = tsubst_fold_expr_init (t, args, complain, in_decl);
  if (init == error_mark_node)
    return error_mark_node;

  if (PACK_EXPANSION_P (pack))
    {
      tree r = copy_node (t);
      FOLD_EXPR_PACK (r) = pack;
      FOLD_EXPR_INIT (r) = init;
      return r;
    }

  int n = TREE_VEC_LENGTH (pack);
  tree vec = make_tree_vec (n + 1);
  for (int i = 0; i < n; ++i)
    TREE_VEC_ELT (vec, i) = TREE_VEC_ELT (pack, i);
  TREE_VEC_ELT (vec, n) = init;

  return expand_right_fold (t, vec, complain);
}

/* Walk through the pattern of a pack expansion, adding everything in
   local_specializations to a list.  */

class el_data
{
public:
  /* Set of variables declared within the pattern.  */
  hash_set<tree> internal;
  /* Set of AST nodes that have been visited by the traversal.  */
  hash_set<tree> visited;
  /* List of local_specializations used within the pattern.  */
  tree extra;
  tsubst_flags_t complain;
  /* True iff we don't want to walk into unevaluated contexts.  */
  bool skip_unevaluated_operands = false;
  /* The unevaluated contexts that we avoided walking.  */
  auto_vec<tree> skipped_trees;

  el_data (tsubst_flags_t c)
    : extra (NULL_TREE), complain (c) {}
};
static tree
extract_locals_r (tree *tp, int *walk_subtrees, void *data_)
{
  el_data &data = *reinterpret_cast<el_data*>(data_);
  tree *extra = &data.extra;
  tsubst_flags_t complain = data.complain;

  if (data.skip_unevaluated_operands
      && unevaluated_p (TREE_CODE (*tp)))
    {
      data.skipped_trees.safe_push (*tp);
      *walk_subtrees = 0;
      return NULL_TREE;
    }

  if (TYPE_P (*tp) && typedef_variant_p (*tp))
    /* Remember local typedefs (85214).  */
    tp = &TYPE_NAME (*tp);

  if (TREE_CODE (*tp) == DECL_EXPR)
    {
      tree decl = DECL_EXPR_DECL (*tp);
      data.internal.add (decl);
      if (VAR_P (decl)
	  && DECL_DECOMPOSITION_P (decl)
	  && TREE_TYPE (decl) != error_mark_node)
	{
	  gcc_assert (DECL_NAME (decl) == NULL_TREE);
	  for (tree decl2 = DECL_CHAIN (decl);
	       decl2
	       && VAR_P (decl2)
	       && DECL_DECOMPOSITION_P (decl2)
	       && DECL_NAME (decl2)
	       && TREE_TYPE (decl2) != error_mark_node;
	       decl2 = DECL_CHAIN (decl2))
	    {
	      gcc_assert (DECL_DECOMP_BASE (decl2) == decl);
	      data.internal.add (decl2);
	    }
	}
    }
  else if (TREE_CODE (*tp) == LAMBDA_EXPR)
    {
      /* Since we defer implicit capture, look in the parms and body.  */
      tree fn = lambda_function (*tp);
      cp_walk_tree (&TREE_TYPE (fn), &extract_locals_r, &data,
		    &data.visited);
      cp_walk_tree (&DECL_SAVED_TREE (fn), &extract_locals_r, &data,
		    &data.visited);
    }
  else if (tree spec = retrieve_local_specialization (*tp))
    {
      if (data.internal.contains (*tp))
	/* Don't mess with variables declared within the pattern.  */
	return NULL_TREE;
      if (TREE_CODE (spec) == NONTYPE_ARGUMENT_PACK)
	{
	  /* Maybe pull out the PARM_DECL for a partial instantiation.  */
	  tree args = ARGUMENT_PACK_ARGS (spec);
	  if (TREE_VEC_LENGTH (args) == 1)
	    {
	      tree elt = TREE_VEC_ELT (args, 0);
	      if (PACK_EXPANSION_P (elt))
		elt = PACK_EXPANSION_PATTERN (elt);
	      if (DECL_PACK_P (elt))
		spec = elt;
	    }
	  if (TREE_CODE (spec) == NONTYPE_ARGUMENT_PACK)
	    {
	      /* Handle lambda capture here, since we aren't doing any
		 substitution now, and so tsubst_copy won't call
		 process_outer_var_ref.  */
	      tree args = ARGUMENT_PACK_ARGS (spec);
	      int len = TREE_VEC_LENGTH (args);
	      for (int i = 0; i < len; ++i)
		{
		  tree arg = TREE_VEC_ELT (args, i);
		  tree carg = arg;
		  if (outer_automatic_var_p (arg))
		    carg = process_outer_var_ref (arg, complain);
		  if (carg != arg)
		    {
		      /* Make a new NONTYPE_ARGUMENT_PACK of the capture
			 proxies.  */
		      if (i == 0)
			{
			  spec = copy_node (spec);
			  args = copy_node (args);
			  ARGUMENT_PACK_ARGS (spec) = args;
			  register_local_specialization (spec, *tp);
			}
		      TREE_VEC_ELT (args, i) = carg;
		    }
		}
	    }
	}
      if (outer_automatic_var_p (spec))
	spec = process_outer_var_ref (spec, complain);
      *extra = tree_cons (*tp, spec, *extra);
    }
  return NULL_TREE;
}
static tree
extract_local_specs (tree pattern, tsubst_flags_t complain)
{
  el_data data (complain);
  /* Walk the pattern twice, ignoring unevaluated operands the first time
     around, so that if a local specialization appears in both an evaluated
     and unevaluated context we prefer to process it in the evaluated context
     (since e.g. process_outer_var_ref is a no-op inside an unevaluated
     context).  */
  data.skip_unevaluated_operands = true;
  cp_walk_tree (&pattern, extract_locals_r, &data, &data.visited);
  /* Now walk the unevaluated contexts we skipped the first time around.  */
  data.skip_unevaluated_operands = false;
  for (tree t : data.skipped_trees)
    {
      data.visited.remove (t);
      cp_walk_tree (&t, extract_locals_r, &data, &data.visited);
    }
  return data.extra;
}

/* Extract any uses of local_specializations from PATTERN and add them to ARGS
   for use in PACK_EXPANSION_EXTRA_ARGS.  */

tree
build_extra_args (tree pattern, tree args, tsubst_flags_t complain)
{
  /* Make a copy of the extra arguments so that they won't get changed
     out from under us.  */
  tree extra = preserve_args (copy_template_args (args), /*cow_p=*/false);
  if (local_specializations)
    if (tree locals = extract_local_specs (pattern, complain))
      extra = tree_cons (NULL_TREE, extra, locals);
  return extra;
}

/* Apply any local specializations from PACK_EXPANSION_EXTRA_ARGS and add the
   normal template args to ARGS.  */

tree
add_extra_args (tree extra, tree args, tsubst_flags_t complain, tree in_decl)
{
  if (extra && TREE_CODE (extra) == TREE_LIST)
    {
      for (tree elt = TREE_CHAIN (extra); elt; elt = TREE_CHAIN (elt))
	{
	  /* The partial instantiation involved local declarations collected in
	     extract_local_specs; map from the general template to our local
	     context.  */
	  tree gen = TREE_PURPOSE (elt);
	  tree inst = TREE_VALUE (elt);
	  if (DECL_P (inst))
	    if (tree local = retrieve_local_specialization (inst))
	      inst = local;
	  /* else inst is already a full instantiation of the pack.  */
	  register_local_specialization (inst, gen);
	}
      gcc_assert (!TREE_PURPOSE (extra));
      extra = TREE_VALUE (extra);
    }
  if (uses_template_parms (extra))
    {
      /* This can happen after dependent substitution into a
	 requires-expr or a lambda that uses constexpr if.  */
      extra = tsubst_template_args (extra, args, complain, in_decl);
      args = add_outermost_template_args (args, extra);
    }
  else
    args = add_to_template_args (extra, args);
  return args;
}

/* Substitute ARGS into T, which is an pack expansion
   (i.e. TYPE_PACK_EXPANSION or EXPR_PACK_EXPANSION). Returns a
   TREE_VEC with the substituted arguments, a PACK_EXPANSION_* node
   (if only a partial substitution could be performed) or
   ERROR_MARK_NODE if there was an error.  */
tree
tsubst_pack_expansion (tree t, tree args, tsubst_flags_t complain,
		       tree in_decl)
{
  tree pattern;
  tree pack, packs = NULL_TREE;
  bool unsubstituted_packs = false;
  int i, len = -1;
  tree result;
  bool need_local_specializations = false;
  int levels;

  gcc_assert (PACK_EXPANSION_P (t));
  pattern = PACK_EXPANSION_PATTERN (t);

  /* Add in any args remembered from an earlier partial instantiation.  */
  args = add_extra_args (PACK_EXPANSION_EXTRA_ARGS (t), args, complain, in_decl);

  levels = TMPL_ARGS_DEPTH (args);

  /* Determine the argument packs that will instantiate the parameter
     packs used in the expansion expression. While we're at it,
     compute the number of arguments to be expanded and make sure it
     is consistent.  */
  for (pack = PACK_EXPANSION_PARAMETER_PACKS (t); pack;
       pack = TREE_CHAIN (pack))
    {
      tree parm_pack = TREE_VALUE (pack);
      tree arg_pack = NULL_TREE;
      tree orig_arg = NULL_TREE;
      int level = 0;

      if (TREE_CODE (parm_pack) == BASES)
	{
	  gcc_assert (parm_pack == pattern);
	  if (BASES_DIRECT (parm_pack))
	    return calculate_direct_bases (tsubst_expr (BASES_TYPE (parm_pack),
							args, complain,
							in_decl),
					   complain);
	  else
	    return calculate_bases (tsubst_expr (BASES_TYPE (parm_pack),
						 args, complain, in_decl),
				    complain);
	}
      else if (builtin_pack_call_p (parm_pack))
	{
	  if (parm_pack != pattern)
	    {
	      if (complain & tf_error)
		sorry ("%qE is not the entire pattern of the pack expansion",
		       parm_pack);
	      return error_mark_node;
	    }
	  return expand_builtin_pack_call (parm_pack, args,
					   complain, in_decl);
	}
      else if (TREE_CODE (parm_pack) == PARM_DECL)
	{
	  /* We know we have correct local_specializations if this
	     expansion is at function scope, or if we're dealing with a
	     local parameter in a requires expression; for the latter,
	     tsubst_requires_expr set it up appropriately.  */
	  if (PACK_EXPANSION_LOCAL_P (t) || CONSTRAINT_VAR_P (parm_pack))
	    arg_pack = retrieve_local_specialization (parm_pack);
	  else
	    /* We can't rely on local_specializations for a parameter
	       name used later in a function declaration (such as in a
	       late-specified return type).  Even if it exists, it might
	       have the wrong value for a recursive call.  */
	    need_local_specializations = true;

	  if (!arg_pack)
	    {
	      /* This parameter pack was used in an unevaluated context.  Just
		 make a dummy decl, since it's only used for its type.  */
	      ++cp_unevaluated_operand;
	      arg_pack = tsubst_decl (parm_pack, args, complain);
	      --cp_unevaluated_operand;
	      if (arg_pack && DECL_PACK_P (arg_pack))
		/* Partial instantiation of the parm_pack, we can't build
		   up an argument pack yet.  */
		arg_pack = NULL_TREE;
	      else
		arg_pack = make_fnparm_pack (arg_pack);
	    }
	  else if (DECL_PACK_P (arg_pack))
	    /* This argument pack isn't fully instantiated yet.  */
	    arg_pack = NULL_TREE;
	}
      else if (is_capture_proxy (parm_pack))
	{
	  arg_pack = retrieve_local_specialization (parm_pack);
	  if (DECL_PACK_P (arg_pack))
	    arg_pack = NULL_TREE;
	}
      else
        {
	  int idx;
          template_parm_level_and_index (parm_pack, &level, &idx);
          if (level <= levels)
            arg_pack = TMPL_ARG (args, level, idx);

	  if (arg_pack && TREE_CODE (arg_pack) == TEMPLATE_TYPE_PARM
	      && TEMPLATE_TYPE_PARAMETER_PACK (arg_pack))
	    arg_pack = NULL_TREE;
        }

      orig_arg = arg_pack;
      if (arg_pack && TREE_CODE (arg_pack) == ARGUMENT_PACK_SELECT)
	arg_pack = ARGUMENT_PACK_SELECT_FROM_PACK (arg_pack);

      if (arg_pack && !ARGUMENT_PACK_P (arg_pack))
	/* This can only happen if we forget to expand an argument
	   pack somewhere else. Just return an error, silently.  */
	{
	  result = make_tree_vec (1);
	  TREE_VEC_ELT (result, 0) = error_mark_node;
	  return result;
	}

      if (arg_pack)
        {
          int my_len = 
            TREE_VEC_LENGTH (ARGUMENT_PACK_ARGS (arg_pack));

	  /* Don't bother trying to do a partial substitution with
	     incomplete packs; we'll try again after deduction.  */
          if (ARGUMENT_PACK_INCOMPLETE_P (arg_pack))
            return t;

          if (len < 0)
	    len = my_len;
	  else if (len != my_len)
            {
	      if (!(complain & tf_error))
		/* Fail quietly.  */;
              else if (TREE_CODE (t) == TYPE_PACK_EXPANSION)
                error ("mismatched argument pack lengths while expanding %qT",
                       pattern);
              else
                error ("mismatched argument pack lengths while expanding %qE",
                       pattern);
              return error_mark_node;
            }

          /* Keep track of the parameter packs and their corresponding
             argument packs.  */
          packs = tree_cons (parm_pack, arg_pack, packs);
          TREE_TYPE (packs) = orig_arg;
        }
      else
	{
	  /* We can't substitute for this parameter pack.  We use a flag as
	     well as the missing_level counter because function parameter
	     packs don't have a level.  */
	  gcc_assert (processing_template_decl || is_auto (parm_pack));
	  unsubstituted_packs = true;
	}
    }

  /* If the expansion is just T..., return the matching argument pack, unless
     we need to call convert_from_reference on all the elements.  This is an
     important optimization; see c++/68422.  */
  if (!unsubstituted_packs
      && TREE_PURPOSE (packs) == pattern)
    {
      tree args = ARGUMENT_PACK_ARGS (TREE_VALUE (packs));

      /* If the argument pack is a single pack expansion, pull it out.  */
      if (TREE_VEC_LENGTH (args) == 1
	  && pack_expansion_args_count (args))
	return TREE_VEC_ELT (args, 0);

      /* Types need no adjustment, nor does sizeof..., and if we still have
	 some pack expansion args we won't do anything yet.  */
      if (TREE_CODE (t) == TYPE_PACK_EXPANSION
	  || PACK_EXPANSION_SIZEOF_P (t)
	  || pack_expansion_args_count (args))
	return args;
      /* Also optimize expression pack expansions if we can tell that the
	 elements won't have reference type.  */
      tree type = TREE_TYPE (pattern);
      if (type && !TYPE_REF_P (type)
	  && !PACK_EXPANSION_P (type)
	  && !WILDCARD_TYPE_P (type))
	return args;
      /* Otherwise use the normal path so we get convert_from_reference.  */
    }

  /* We cannot expand this expansion expression, because we don't have
     all of the argument packs we need.  */
  if (use_pack_expansion_extra_args_p (t, packs, len, unsubstituted_packs))
    {
      /* We got some full packs, but we can't substitute them in until we
	 have values for all the packs.  So remember these until then.  */

      t = make_pack_expansion (pattern, complain);
      PACK_EXPANSION_EXTRA_ARGS (t)
	= build_extra_args (pattern, args, complain);
      return t;
    }

  /* If NEED_LOCAL_SPECIALIZATIONS then we're in a late-specified return
     type, so create our own local specializations map; the current map is
     either NULL or (in the case of recursive unification) might have
     bindings that we don't want to use or alter.  */
  local_specialization_stack lss (need_local_specializations
				  ? lss_blank : lss_nop);

  if (unsubstituted_packs)
    {
      /* There were no real arguments, we're just replacing a parameter
	 pack with another version of itself. Substitute into the
	 pattern and return a PACK_EXPANSION_*. The caller will need to
	 deal with that.  */
      if (TREE_CODE (t) == EXPR_PACK_EXPANSION)
	result = tsubst_expr (pattern, args, complain, in_decl);
      else
	result = tsubst (pattern, args, complain, in_decl);
      result = make_pack_expansion (result, complain);
      PACK_EXPANSION_LOCAL_P (result) = PACK_EXPANSION_LOCAL_P (t);
      PACK_EXPANSION_SIZEOF_P (result) = PACK_EXPANSION_SIZEOF_P (t);
      if (PACK_EXPANSION_AUTO_P (t))
	{
	  /* This is a fake auto... pack expansion created in add_capture with
	     _PACKS that don't appear in the pattern.  Copy one over.  */
	  packs = PACK_EXPANSION_PARAMETER_PACKS (t);
	  pack = retrieve_local_specialization (TREE_VALUE (packs));
	  gcc_checking_assert (DECL_PACK_P (pack));
	  PACK_EXPANSION_PARAMETER_PACKS (result)
	    = build_tree_list (NULL_TREE, pack);
	  PACK_EXPANSION_AUTO_P (result) = true;
	}
      return result;
    }

  gcc_assert (len >= 0);

  /* For each argument in each argument pack, substitute into the
     pattern.  */
  result = make_tree_vec (len);
  tree elem_args = copy_template_args (args);
  for (i = 0; i < len; ++i)
    {
      t = gen_elem_of_pack_expansion_instantiation (pattern, packs,
						    i,
						    elem_args, complain,
						    in_decl);
      TREE_VEC_ELT (result, i) = t;
      if (t == error_mark_node)
	{
	  result = error_mark_node;
	  break;
	}
    }

  /* Update ARGS to restore the substitution from parameter packs to
     their argument packs.  */
  for (pack = packs; pack; pack = TREE_CHAIN (pack))
    {
      tree parm = TREE_PURPOSE (pack);

      if (TREE_CODE (parm) == PARM_DECL
	  || VAR_P (parm)
	  || TREE_CODE (parm) == FIELD_DECL)
        register_local_specialization (TREE_TYPE (pack), parm);
      else
        {
          int idx, level;

	  if (TREE_VALUE (pack) == NULL_TREE)
	    continue;

          template_parm_level_and_index (parm, &level, &idx);

          /* Update the corresponding argument.  */
          if (TMPL_ARGS_HAVE_MULTIPLE_LEVELS (args))
            TREE_VEC_ELT (TREE_VEC_ELT (args, level -1 ), idx) =
              TREE_TYPE (pack);
          else
            TREE_VEC_ELT (args, idx) = TREE_TYPE (pack);
        }
    }

  /* If the dependent pack arguments were such that we end up with only a
     single pack expansion again, there's no need to keep it in a TREE_VEC.  */
  if (len == 1 && TREE_CODE (result) == TREE_VEC
      && PACK_EXPANSION_P (TREE_VEC_ELT (result, 0)))
    return TREE_VEC_ELT (result, 0);

  return result;
}

/* Make an argument pack out of the TREE_VEC VEC.  */

static tree
make_argument_pack (tree vec)
{
  tree pack;

  if (TYPE_P (TREE_VEC_ELT (vec, 0)))
    pack = cxx_make_type (TYPE_ARGUMENT_PACK);
  else
    {
      pack = make_node (NONTYPE_ARGUMENT_PACK);
      TREE_CONSTANT (pack) = 1;
    }
  ARGUMENT_PACK_ARGS (pack) = vec;
  return pack;
}

/* Return an exact copy of template args T that can be modified
   independently.  */

static tree
copy_template_args (tree t)
{
  if (t == error_mark_node)
    return t;

  int len = TREE_VEC_LENGTH (t);
  tree new_vec = make_tree_vec (len);

  for (int i = 0; i < len; ++i)
    {
      tree elt = TREE_VEC_ELT (t, i);
      if (elt && TREE_CODE (elt) == TREE_VEC)
	elt = copy_template_args (elt);
      TREE_VEC_ELT (new_vec, i) = elt;
    }

  NON_DEFAULT_TEMPLATE_ARGS_COUNT (new_vec)
    = NON_DEFAULT_TEMPLATE_ARGS_COUNT (t);

  return new_vec;
}

/* Substitute ARGS into the *_ARGUMENT_PACK orig_arg.  */

tree
tsubst_argument_pack (tree orig_arg, tree args, tsubst_flags_t complain,
		      tree in_decl)
{
  /* This flag is used only during deduction, and we don't expect to
     substitute such ARGUMENT_PACKs.  */
  gcc_assert (!ARGUMENT_PACK_INCOMPLETE_P (orig_arg));

  /* Substitute into each of the arguments.  */
  tree pack_args = tsubst_template_args (ARGUMENT_PACK_ARGS (orig_arg),
					 args, complain, in_decl);
  if (pack_args == error_mark_node)
    return error_mark_node;

  if (pack_args == ARGUMENT_PACK_ARGS (orig_arg))
    return orig_arg;

  /* If we're substituting into a generic ARGUMENT_PACK for a variadic
     template parameter, we might be able to avoid allocating a new
     ARGUMENT_PACK and reuse the corresponding ARGUMENT_PACK from ARGS
     if the substituted result is identical to it.  */
  if (tree parm = template_arg_to_parm (orig_arg))
    {
      int level, index;
      template_parm_level_and_index (parm, &level, &index);
      if (TMPL_ARGS_DEPTH (args) >= level)
	if (tree arg = TMPL_ARG (args, level, index))
	  if (TREE_CODE (arg) == TREE_CODE (orig_arg)
	      && ARGUMENT_PACK_ARGS (arg) == pack_args)
	    {
	      gcc_assert (!ARGUMENT_PACK_INCOMPLETE_P (arg));
	      return arg;
	    }
    }

  tree new_arg;
  if (TYPE_P (orig_arg))
    {
      new_arg = cxx_make_type (TREE_CODE (orig_arg));
      SET_TYPE_STRUCTURAL_EQUALITY (new_arg);
    }
  else
    {
      new_arg = make_node (TREE_CODE (orig_arg));
      TREE_CONSTANT (new_arg) = TREE_CONSTANT (orig_arg);
    }
  ARGUMENT_PACK_ARGS (new_arg) = pack_args;
  return new_arg;
}

/* Substitute ARGS into the vector or list of template arguments T.  */

tree
tsubst_template_args (tree t, tree args, tsubst_flags_t complain, tree in_decl)
{
  if (t == error_mark_node)
    return error_mark_node;

  /* In "sizeof(X<I>)" we need to evaluate "I".  */
  cp_evaluated ev;

  const int len = TREE_VEC_LENGTH (t);
  tree *elts = XALLOCAVEC (tree, len);
  int expanded_len_adjust = 0;

  /* True iff the substituted result is identical to T.  */
  bool const_subst_p = true;

  for (int i = 0; i < len; i++)
    {
      tree orig_arg = TREE_VEC_ELT (t, i);
      tree new_arg;

      if (!orig_arg)
	new_arg = NULL_TREE;
      else if (TREE_CODE (orig_arg) == TREE_VEC)
	new_arg = tsubst_template_args (orig_arg, args, complain, in_decl);
      else if (PACK_EXPANSION_P (orig_arg))
        {
          /* Substitute into an expansion expression.  */
          new_arg = tsubst_pack_expansion (orig_arg, args, complain, in_decl);

          if (TREE_CODE (new_arg) == TREE_VEC)
            /* Add to the expanded length adjustment the number of
               expanded arguments. We subtract one from this
               measurement, because the argument pack expression
               itself is already counted as 1 in
               LEN. EXPANDED_LEN_ADJUST can actually be negative, if
               the argument pack is empty.  */
            expanded_len_adjust += TREE_VEC_LENGTH (new_arg) - 1;
        }
      else if (ARGUMENT_PACK_P (orig_arg))
	new_arg = tsubst_argument_pack (orig_arg, args, complain, in_decl);
      else
	new_arg = tsubst_template_arg (orig_arg, args, complain, in_decl);

      if (new_arg == error_mark_node)
	return error_mark_node;

      elts[i] = new_arg;
      if (new_arg != orig_arg)
	const_subst_p = false;
    }

  if (const_subst_p)
    return t;

  tree maybe_reuse = NULL_TREE;

  /* If ARGS and T are both multi-level, the substituted result may be
     identical to ARGS.  */
  if (TMPL_ARGS_HAVE_MULTIPLE_LEVELS (t)
      && TMPL_ARGS_HAVE_MULTIPLE_LEVELS (args)
      && TMPL_ARGS_DEPTH (t) == TMPL_ARGS_DEPTH (args))
    maybe_reuse = args;
  /* If T appears to be a vector of generic template arguments, the
     substituted result may be identical to the corresponding level
     from ARGS.  */
  else if (tree parm = template_arg_to_parm (TREE_VEC_ELT (t, 0)))
    {
      int level, index;
      template_parm_level_and_index (parm, &level, &index);
      if (index == 0 && TMPL_ARGS_DEPTH (args) >= level)
	maybe_reuse = TMPL_ARGS_LEVEL (args, level);
    }

  /* If the substituted result is identical to MAYBE_REUSE, return
     it and avoid allocating a new TREE_VEC, as an optimization.  */
  if (maybe_reuse != NULL_TREE
      && TREE_VEC_LENGTH (maybe_reuse) == len
      && std::equal (elts, elts+len, TREE_VEC_BEGIN (maybe_reuse)))
    return maybe_reuse;

  /* If T consists of only a pack expansion for which substitution yielded
     a TREE_VEC of the expanded elements, then reuse that TREE_VEC instead
     of effectively making a copy.  */
  if (len == 1
      && PACK_EXPANSION_P (TREE_VEC_ELT (t, 0))
      && TREE_CODE (elts[0]) == TREE_VEC)
    return elts[0];

  /* Make space for the expanded arguments coming from template
     argument packs.  */
  tree r = make_tree_vec (len + expanded_len_adjust);
  /* T can contain TREE_VECs. That happens if T contains the
     arguments for a member template.
     In that case each TREE_VEC in T represents a level of template
     arguments, and T won't carry any non defaulted argument count.
     It will rather be the nested TREE_VECs that will carry one.
     In other words, T carries a non defaulted argument count only
     if it doesn't contain any nested TREE_VEC.  */
  if (NON_DEFAULT_TEMPLATE_ARGS_COUNT (t))
    {
      int count = GET_NON_DEFAULT_TEMPLATE_ARGS_COUNT (t);
      count += expanded_len_adjust;
      SET_NON_DEFAULT_TEMPLATE_ARGS_COUNT (r, count);
    }

  int out = 0;
  for (int i = 0; i < len; i++)
    {
      tree orig_arg = TREE_VEC_ELT (t, i);
      if (orig_arg
	  && PACK_EXPANSION_P (orig_arg)
          && TREE_CODE (elts[i]) == TREE_VEC)
        {
          /* Now expand the template argument pack "in place".  */
	  for (int idx = 0; idx < TREE_VEC_LENGTH (elts[i]); idx++, out++)
	    TREE_VEC_ELT (r, out) = TREE_VEC_ELT (elts[i], idx);
        }
      else
        {
	  TREE_VEC_ELT (r, out) = elts[i];
          out++;
        }
    }
  gcc_assert (out == TREE_VEC_LENGTH (r));

  return r;
}

/* Substitute ARGS into one level PARMS of template parameters.  */

static tree
tsubst_template_parms_level (tree parms, tree args, tsubst_flags_t complain)
{
  if (parms == error_mark_node)
    return error_mark_node;

  tree new_vec = make_tree_vec (TREE_VEC_LENGTH (parms));

  for (int i = 0; i < TREE_VEC_LENGTH (new_vec); ++i)
    {
      tree tuple = TREE_VEC_ELT (parms, i);

      if (tuple == error_mark_node)
	continue;

      TREE_VEC_ELT (new_vec, i) =
	tsubst_template_parm (tuple, args, complain);
    }

  return new_vec;
}

/* Return the result of substituting ARGS into the template parameters
   given by PARMS.  If there are m levels of ARGS and m + n levels of
   PARMS, then the result will contain n levels of PARMS.  For
   example, if PARMS is `template <class T> template <class U>
   template <T*, U, class V>' and ARGS is {{int}, {double}} then the
   result will be `template <int*, double, class V>'.  */

static tree
tsubst_template_parms (tree parms, tree args, tsubst_flags_t complain)
{
  tree r = NULL_TREE;
  tree* new_parms;

  /* When substituting into a template, we must set
     PROCESSING_TEMPLATE_DECL as the template parameters may be
     dependent if they are based on one-another, and the dependency
     predicates are short-circuit outside of templates.  */
  ++processing_template_decl;

  for (new_parms = &r;
       parms && TMPL_PARMS_DEPTH (parms) > TMPL_ARGS_DEPTH (args);
       new_parms = &(TREE_CHAIN (*new_parms)),
	 parms = TREE_CHAIN (parms))
    {
      tree new_vec = tsubst_template_parms_level (TREE_VALUE (parms),
						  args, complain);
      *new_parms =
	tree_cons (size_int (TMPL_PARMS_DEPTH (parms)
			     - TMPL_ARGS_DEPTH (args)),
		   new_vec, NULL_TREE);
      TEMPLATE_PARMS_CONSTRAINTS (*new_parms)
	= TEMPLATE_PARMS_CONSTRAINTS (parms);
    }

  --processing_template_decl;

  return r;
}

/* Return the result of substituting ARGS into one template parameter
   given by T. T Must be a TREE_LIST which TREE_VALUE is the template
   parameter and which TREE_PURPOSE is the default argument of the
   template parameter.  */

static tree
tsubst_template_parm (tree t, tree args, tsubst_flags_t complain)
{
  tree default_value, parm_decl;

  if (args == NULL_TREE
      || t == NULL_TREE
      || t == error_mark_node)
    return t;

  gcc_assert (TREE_CODE (t) == TREE_LIST);

  default_value = TREE_PURPOSE (t);
  parm_decl = TREE_VALUE (t);

  parm_decl = tsubst (parm_decl, args, complain, NULL_TREE);
  if (TREE_CODE (parm_decl) == PARM_DECL
      && invalid_nontype_parm_type_p (TREE_TYPE (parm_decl), complain))
    parm_decl = error_mark_node;
  default_value = tsubst_template_arg (default_value, args,
				       complain, NULL_TREE);

  tree r = build_tree_list (default_value, parm_decl);
  TEMPLATE_PARM_CONSTRAINTS (r) = TEMPLATE_PARM_CONSTRAINTS (t);
  return r;
}

/* Substitute in-place the TEMPLATE_PARM_CONSTRAINTS of each template
   parameter in PARMS for sake of declaration matching.  */

static void
tsubst_each_template_parm_constraints (tree parms, tree args,
				       tsubst_flags_t complain)
{
  ++processing_template_decl;
  for (; parms; parms = TREE_CHAIN (parms))
    {
      tree level = TREE_VALUE (parms);
      for (tree parm : tree_vec_range (level))
	TEMPLATE_PARM_CONSTRAINTS (parm)
	  = tsubst_constraint (TEMPLATE_PARM_CONSTRAINTS (parm), args,
			       complain, NULL_TREE);
    }
  --processing_template_decl;
}

/* Substitute the ARGS into the indicated aggregate (or enumeration)
   type T.  If T is not an aggregate or enumeration type, it is
   handled as if by tsubst.  IN_DECL is as for tsubst.  If
   ENTERING_SCOPE is nonzero, T is the context for a template which
   we are presently tsubst'ing.  Return the substituted value.  */

static tree
tsubst_aggr_type (tree t,
		  tree args,
		  tsubst_flags_t complain,
		  tree in_decl,
		  int entering_scope)
{
  if (t == NULL_TREE)
    return NULL_TREE;

  /* Handle typedefs via tsubst so that they get consistently reused.  */
  if (typedef_variant_p (t))
    {
      t = tsubst (t, args, complain, in_decl);
      if (t == error_mark_node)
	return error_mark_node;

      /* The effect of entering_scope is that for a dependent specialization
	 A<T>, lookup_template_class prefers to return A's primary template
	 type instead of the implicit instantiation.  So when entering_scope,
	 we mirror this behavior by inspecting TYPE_CANONICAL appropriately,
	 taking advantage of the fact that lookup_template_class links the two
	 types by setting TYPE_CANONICAL of the latter to the former.  */
      if (entering_scope
	  && CLASS_TYPE_P (t)
	  && dependent_type_p (t)
	  && TYPE_CANONICAL (t) == TREE_TYPE (TYPE_TI_TEMPLATE (t)))
	t = TYPE_CANONICAL (t);

      return t;
    }

  switch (TREE_CODE (t))
    {
      case RECORD_TYPE:
      case ENUMERAL_TYPE:
      case UNION_TYPE:
	return tsubst_aggr_type_1 (t, args, complain, in_decl, entering_scope);

      default:
	return tsubst (t, args, complain, in_decl);
    }
}

/* The part of tsubst_aggr_type that's shared with the RECORD_, UNION_
   and ENUMERAL_TYPE cases of tsubst.  */

static tree
tsubst_aggr_type_1 (tree t,
		    tree args,
		    tsubst_flags_t complain,
		    tree in_decl,
		    int entering_scope)
{
  if (TYPE_TEMPLATE_INFO (t) && uses_template_parms (t))
    {
      complain &= ~tf_qualifying_scope;

      /* Figure out what arguments are appropriate for the
	 type we are trying to find.  For example, given:

	   template <class T> struct S;
	   template <class T, class U> void f(T, U) { S<U> su; }

	 and supposing that we are instantiating f<int, double>,
	 then our ARGS will be {int, double}, but, when looking up
	 S we only want {double}.  */
      tree argvec = tsubst_template_args (TYPE_TI_ARGS (t), args,
					  complain, in_decl);
      if (argvec == error_mark_node)
	return error_mark_node;

      tree r = lookup_template_class (t, argvec, in_decl, NULL_TREE,
				      entering_scope, complain);
      return cp_build_qualified_type (r, cp_type_quals (t), complain);
    }
  else
    /* This is not a template type, so there's nothing to do.  */
    return t;
}

/* Map from a FUNCTION_DECL to a vec of default argument instantiations,
   indexed in reverse order of the parameters.  */

static GTY((cache)) hash_table<tree_vec_map_cache_hasher> *defarg_inst;

/* Return a reference to the vec* of defarg insts for FN.  */

static vec<tree,va_gc> *&
defarg_insts_for (tree fn)
{
  if (!defarg_inst)
    defarg_inst = hash_table<tree_vec_map_cache_hasher>::create_ggc (13);
  tree_vec_map in = { { fn }, nullptr };
  tree_vec_map **slot
    = defarg_inst->find_slot_with_hash (&in, DECL_UID (fn), INSERT);
  if (!*slot)
    {
      *slot = ggc_alloc<tree_vec_map> ();
      **slot = in;
    }
  return (*slot)->to;
}

/* Substitute into the default argument ARG (a default argument for
   FN), which has the indicated TYPE.  */

tree
tsubst_default_argument (tree fn, int parmnum, tree type, tree arg,
			 tsubst_flags_t complain)
{
  int errs = errorcount + sorrycount;

  /* This can happen in invalid code.  */
  if (TREE_CODE (arg) == DEFERRED_PARSE)
    return arg;

  /* Shortcut {}.  */
  if (BRACE_ENCLOSED_INITIALIZER_P (arg)
      && CONSTRUCTOR_NELTS (arg) == 0)
    return arg;

  tree parm = FUNCTION_FIRST_USER_PARM (fn);
  parm = chain_index (parmnum, parm);
  tree parmtype = TREE_TYPE (parm);
  if (DECL_BY_REFERENCE (parm))
    parmtype = TREE_TYPE (parmtype);
  if (parmtype == error_mark_node)
    return error_mark_node;

  gcc_assert (same_type_ignoring_top_level_qualifiers_p (type, parmtype));

  /* Remember the location of the pointer to the vec rather than the location
     of the particular element, in case the vec grows in tsubst_expr.  */
  vec<tree,va_gc> *&defs = defarg_insts_for (fn);
  /* Index in reverse order to avoid allocating space for initial parameters
     that don't have default arguments.  */
  unsigned ridx = list_length (parm);
  if (vec_safe_length (defs) < ridx)
    vec_safe_grow_cleared (defs, ridx);
  else if (tree inst = (*defs)[ridx - 1])
    return inst;

  /* This default argument came from a template.  Instantiate the
     default argument here, not in tsubst.  In the case of
     something like:

       template <class T>
       struct S {
	 static T t();
	 void f(T = t());
       };

     we must be careful to do name lookup in the scope of S<T>,
     rather than in the current class.  */
  push_to_top_level ();
  push_access_scope (fn);
  push_deferring_access_checks (dk_no_deferred);
  /* So in_immediate_context knows this is a default argument.  */
  begin_scope (sk_function_parms, fn);
  start_lambda_scope (parm);

  /* The default argument expression may cause implicitly defined
     member functions to be synthesized, which will result in garbage
     collection.  We must treat this situation as if we were within
     the body of function so as to avoid collecting live data on the
     stack.  */
  ++function_depth;
  arg = tsubst_expr (arg, DECL_TI_ARGS (fn), complain, NULL_TREE);
  --function_depth;

  finish_lambda_scope ();

  /* Make sure the default argument is reasonable.  */
  arg = check_default_argument (type, arg, complain);

  if (errorcount+sorrycount > errs
      && (complain & tf_warning_or_error))
    inform (input_location,
	    "  when instantiating default argument for call to %qD", fn);

  leave_scope ();
  pop_deferring_access_checks ();
  pop_access_scope (fn);
  pop_from_top_level ();

  if (arg != error_mark_node && !cp_unevaluated_operand)
    (*defs)[ridx - 1] = arg;

  return arg;
}

/* Substitute into all the default arguments for FN.  */

static void
tsubst_default_arguments (tree fn, tsubst_flags_t complain)
{
  tree arg;
  tree tmpl_args;

  tmpl_args = DECL_TI_ARGS (fn);

  /* If this function is not yet instantiated, we certainly don't need
     its default arguments.  */
  if (uses_template_parms (tmpl_args))
    return;
  /* Don't do this again for clones.  */
  if (DECL_CLONED_FUNCTION_P (fn))
    return;

  int i = 0;
  for (arg = TYPE_ARG_TYPES (TREE_TYPE (fn));
       arg;
       arg = TREE_CHAIN (arg), ++i)
    if (TREE_PURPOSE (arg))
      TREE_PURPOSE (arg) = tsubst_default_argument (fn, i,
						    TREE_VALUE (arg),
						    TREE_PURPOSE (arg),
						    complain);
}

/* Hash table mapping a FUNCTION_DECL to its dependent explicit-specifier.  */
static GTY((cache)) decl_tree_cache_map *explicit_specifier_map;

/* Store a pair to EXPLICIT_SPECIFIER_MAP.  */

void
store_explicit_specifier (tree v, tree t)
{
  if (!explicit_specifier_map)
    explicit_specifier_map = decl_tree_cache_map::create_ggc (37);
  DECL_HAS_DEPENDENT_EXPLICIT_SPEC_P (v) = true;
  explicit_specifier_map->put (v, t);
}

/* Lookup an element in EXPLICIT_SPECIFIER_MAP.  */

tree
lookup_explicit_specifier (tree v)
{
  return *explicit_specifier_map->get (v);
}

/* Given T, a FUNCTION_TYPE or METHOD_TYPE, construct and return a corresponding
   FUNCTION_TYPE or METHOD_TYPE whose return type is RETURN_TYPE, argument types
   are ARG_TYPES, and exception specification is RAISES, and otherwise is
   identical to T.  */

static tree
rebuild_function_or_method_type (tree t, tree return_type, tree arg_types,
				 tree raises, tsubst_flags_t complain)
{
  gcc_assert (FUNC_OR_METHOD_TYPE_P (t));

  tree new_type;
  if (TREE_CODE (t) == FUNCTION_TYPE)
    {
      new_type = build_function_type (return_type, arg_types);
      new_type = apply_memfn_quals (new_type, type_memfn_quals (t));
    }
  else
    {
      tree r = TREE_TYPE (TREE_VALUE (arg_types));
      /* Don't pick up extra function qualifiers from the basetype.  */
      r = cp_build_qualified_type (r, type_memfn_quals (t), complain);
      if (! MAYBE_CLASS_TYPE_P (r))
	{
	  /* [temp.deduct]

	     Type deduction may fail for any of the following
	     reasons:

	     -- Attempting to create "pointer to member of T" when T
	     is not a class type.  */
	  if (complain & tf_error)
	    error ("creating pointer to member function of non-class type %qT",
		   r);
	  return error_mark_node;
	}

      new_type = build_method_type_directly (r, return_type,
					     TREE_CHAIN (arg_types));
    }
  new_type = cp_build_type_attribute_variant (new_type, TYPE_ATTRIBUTES (t));

  cp_ref_qualifier rqual = type_memfn_rqual (t);
  bool late_return_type_p = TYPE_HAS_LATE_RETURN_TYPE (t);
  return build_cp_fntype_variant (new_type, rqual, raises, late_return_type_p);
}

/* Check if the function type of DECL, a FUNCTION_DECL, agrees with the type of
   each of its formal parameters.  If there is a disagreement then rebuild
   DECL's function type according to its formal parameter types, as part of a
   resolution for Core issues 1001/1322.  */

static void
maybe_rebuild_function_decl_type (tree decl)
{
  bool function_type_needs_rebuilding = false;
  if (tree parm_list = FUNCTION_FIRST_USER_PARM (decl))
    {
      tree parm_type_list = FUNCTION_FIRST_USER_PARMTYPE (decl);
      while (parm_type_list && parm_type_list != void_list_node)
	{
	  tree parm_type = TREE_VALUE (parm_type_list);
	  tree formal_parm_type_unqual = strip_top_quals (TREE_TYPE (parm_list));
	  if (!same_type_p (parm_type, formal_parm_type_unqual))
	    {
	      function_type_needs_rebuilding = true;
	      break;
	    }

	  parm_list = DECL_CHAIN (parm_list);
	  parm_type_list = TREE_CHAIN (parm_type_list);
	}
    }

  if (!function_type_needs_rebuilding)
    return;

  const tree fntype = TREE_TYPE (decl);
  tree parm_list = DECL_ARGUMENTS (decl);
  tree old_parm_type_list = TYPE_ARG_TYPES (fntype);
  tree new_parm_type_list = NULL_TREE;
  tree *q = &new_parm_type_list;
  for (int skip = num_artificial_parms_for (decl); skip > 0; skip--)
    {
      *q = copy_node (old_parm_type_list);
      parm_list = DECL_CHAIN (parm_list);
      old_parm_type_list = TREE_CHAIN (old_parm_type_list);
      q = &TREE_CHAIN (*q);
    }
  while (old_parm_type_list && old_parm_type_list != void_list_node)
    {
      *q = copy_node (old_parm_type_list);
      tree *new_parm_type = &TREE_VALUE (*q);
      tree formal_parm_type_unqual = strip_top_quals (TREE_TYPE (parm_list));
      if (!same_type_p (*new_parm_type, formal_parm_type_unqual))
	*new_parm_type = formal_parm_type_unqual;

      parm_list = DECL_CHAIN (parm_list);
      old_parm_type_list = TREE_CHAIN (old_parm_type_list);
      q = &TREE_CHAIN (*q);
    }
  if (old_parm_type_list == void_list_node)
    *q = void_list_node;

  TREE_TYPE (decl)
    = rebuild_function_or_method_type (fntype,
				       TREE_TYPE (fntype), new_parm_type_list,
				       TYPE_RAISES_EXCEPTIONS (fntype), tf_none);
}

/* Subroutine of tsubst_decl for the case when T is a FUNCTION_DECL.  */

static tree
tsubst_function_decl (tree t, tree args, tsubst_flags_t complain,
		      tree lambda_fntype, bool use_spec_table = true)
{
  tree gen_tmpl = NULL_TREE, argvec = NULL_TREE;
  hashval_t hash = 0;
  tree in_decl = t;

  /* Nobody should be tsubst'ing into non-template functions.  */
  gcc_assert (DECL_TEMPLATE_INFO (t) != NULL_TREE
	      || DECL_LOCAL_DECL_P (t));

  if (DECL_LOCAL_DECL_P (t))
    {
      if (tree spec = retrieve_local_specialization (t))
	return spec;
    }
  else if (TREE_CODE (DECL_TI_TEMPLATE (t)) == TEMPLATE_DECL)
    {
      /* If T is not dependent, just return it.  */
      if (!uses_template_parms (DECL_TI_ARGS (t))
	  && !LAMBDA_FUNCTION_P (t))
	return t;

      /* A non-templated friend doesn't get DECL_TEMPLATE_INFO.  */
      if (non_templated_friend_p (t))
	goto friend_case;

      /* Calculate the most general template of which R is a
	 specialization.  */
      gen_tmpl = most_general_template (DECL_TI_TEMPLATE (t));

      /* We're substituting a lambda function under tsubst_lambda_expr but not
	 directly from it; find the matching function we're already inside.
	 But don't do this if T is a generic lambda with a single level of
	 template parms, as in that case we're doing a normal instantiation. */
      if (LAMBDA_FUNCTION_P (t) && !lambda_fntype
	  && (!generic_lambda_fn_p (t)
	      || TMPL_PARMS_DEPTH (DECL_TEMPLATE_PARMS (gen_tmpl)) > 1))
	return enclosing_instantiation_of (t);

      /* Calculate the complete set of arguments used to
	 specialize R.  */
      if (use_spec_table)
	{
	  argvec = tsubst_template_args (DECL_TI_ARGS
					 (DECL_TEMPLATE_RESULT
					  (DECL_TI_TEMPLATE (t))),
					 args, complain, in_decl);
	  if (argvec == error_mark_node)
	    return error_mark_node;

	  /* Check to see if we already have this specialization.  */
	  if (!lambda_fntype)
	    {
	      hash = spec_hasher::hash (gen_tmpl, argvec);
	      if (tree spec = retrieve_specialization (gen_tmpl, argvec, hash))
		/* The spec for these args might be a partial instantiation of the
		   template, but here what we want is the FUNCTION_DECL.  */
		return STRIP_TEMPLATE (spec);
	    }
	}
      else
	argvec = args;
    }
  else
    {
      /* This special case arises when we have something like this:

	 template <class T> struct S {
	   friend void f<int>(int, double);
	 };

	 Here, the DECL_TI_TEMPLATE for the friend declaration
	 will be an IDENTIFIER_NODE.  We are being called from
	 tsubst_friend_function, and we want only to create a
	 new decl (R) with appropriate types so that we can call
	 determine_specialization.  */
    friend_case:
      gen_tmpl = NULL_TREE;
      argvec = NULL_TREE;
    }

  tree closure = (lambda_fntype ? TYPE_METHOD_BASETYPE (lambda_fntype)
		  : NULL_TREE);
  tree ctx = closure ? closure : DECL_CONTEXT (t);
  bool member = ctx && TYPE_P (ctx);

  /* If this is a static lambda, remove the 'this' pointer added in
     tsubst_lambda_expr now that we know the closure type.  */
  if (lambda_fntype && DECL_STATIC_FUNCTION_P (t))
    lambda_fntype = static_fn_type (lambda_fntype);

  if (member && !closure)
    ctx = tsubst_aggr_type (ctx, args,
			    complain, t, /*entering_scope=*/1);

  tree type = (lambda_fntype ? lambda_fntype
	       : tsubst (TREE_TYPE (t), args,
			 complain | tf_fndecl_type, in_decl));
  if (type == error_mark_node)
    return error_mark_node;

  /* If we hit excessive deduction depth, the type is bogus even if
     it isn't error_mark_node, so don't build a decl.  */
  if (excessive_deduction_depth)
    return error_mark_node;

  /* We do NOT check for matching decls pushed separately at this
     point, as they may not represent instantiations of this
     template, and in any case are considered separate under the
     discrete model.  */
  tree r = copy_decl (t);
  DECL_USE_TEMPLATE (r) = 0;
  TREE_TYPE (r) = type;
  /* Clear out the mangled name and RTL for the instantiation.  */
  SET_DECL_ASSEMBLER_NAME (r, NULL_TREE);
  SET_DECL_RTL (r, NULL);
  /* Leave DECL_INITIAL set on deleted instantiations.  */
  if (!DECL_DELETED_FN (r))
    DECL_INITIAL (r) = NULL_TREE;
  DECL_CONTEXT (r) = ctx;
  set_instantiating_module (r);

  /* Handle explicit(dependent-expr).  */
  if (DECL_HAS_DEPENDENT_EXPLICIT_SPEC_P (t))
    {
      tree spec = lookup_explicit_specifier (t);
      spec = tsubst_copy_and_build (spec, args, complain, in_decl);
      spec = build_explicit_specifier (spec, complain);
      if (spec == error_mark_node)
	return error_mark_node;
      if (instantiation_dependent_expression_p (spec))
	store_explicit_specifier (r, spec);
      else
	{
	  DECL_NONCONVERTING_P (r) = (spec == boolean_true_node);
	  DECL_HAS_DEPENDENT_EXPLICIT_SPEC_P (r) = false;
	}
    }

  /* OpenMP UDRs have the only argument a reference to the declared
     type.  We want to diagnose if the declared type is a reference,
     which is invalid, but as references to references are usually
     quietly merged, diagnose it here.  */
  if (DECL_OMP_DECLARE_REDUCTION_P (t))
    {
      tree argtype
	= TREE_TYPE (TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (t))));
      argtype = tsubst (argtype, args, complain, in_decl);
      if (TYPE_REF_P (argtype))
	error_at (DECL_SOURCE_LOCATION (t),
		  "reference type %qT in "
		  "%<#pragma omp declare reduction%>", argtype);
      if (strchr (IDENTIFIER_POINTER (DECL_NAME (t)), '~') == NULL)
	DECL_NAME (r) = omp_reduction_id (ERROR_MARK, DECL_NAME (t),
					  argtype);
    }

  if (member && DECL_CONV_FN_P (r))
    /* Type-conversion operator.  Reconstruct the name, in
       case it's the name of one of the template's parameters.  */
    DECL_NAME (r) = make_conv_op_name (TREE_TYPE (type));

  tree parms = DECL_ARGUMENTS (t);
  if (closure && !DECL_STATIC_FUNCTION_P (t))
    parms = DECL_CHAIN (parms);
  parms = tsubst (parms, args, complain, t);
  for (tree parm = parms; parm; parm = DECL_CHAIN (parm))
    DECL_CONTEXT (parm) = r;
  if (closure && !DECL_STATIC_FUNCTION_P (t))
    {
      tree tparm = build_this_parm (r, closure, type_memfn_quals (type));
      DECL_NAME (tparm) = closure_identifier;
      DECL_CHAIN (tparm) = parms;
      parms = tparm;
    }
  DECL_ARGUMENTS (r) = parms;
  DECL_RESULT (r) = NULL_TREE;

  maybe_rebuild_function_decl_type (r);

  TREE_STATIC (r) = 0;
  TREE_PUBLIC (r) = TREE_PUBLIC (t);
  DECL_EXTERNAL (r) = 1;
  /* If this is an instantiation of a function with internal
     linkage, we already know what object file linkage will be
     assigned to the instantiation.  */
  DECL_INTERFACE_KNOWN (r) = !TREE_PUBLIC (r);
  DECL_DEFER_OUTPUT (r) = 0;
  DECL_CHAIN (r) = NULL_TREE;
  DECL_PENDING_INLINE_INFO (r) = 0;
  DECL_PENDING_INLINE_P (r) = 0;
  DECL_SAVED_TREE (r) = NULL_TREE;
  DECL_STRUCT_FUNCTION (r) = NULL;
  TREE_USED (r) = 0;
  /* We'll re-clone as appropriate in instantiate_template.  */
  DECL_CLONED_FUNCTION (r) = NULL_TREE;

  /* If we aren't complaining now, return on error before we register
     the specialization so that we'll complain eventually.  */
  if ((complain & tf_error) == 0
      && IDENTIFIER_ANY_OP_P (DECL_NAME (r))
      && !grok_op_properties (r, /*complain=*/false))
    return error_mark_node;

  /* Associate the constraints directly with the instantiation. We
     don't substitute through the constraints; that's only done when
     they are checked.  */
  if (tree ci = get_constraints (t))
    set_constraints (r, ci);

  if (DECL_FRIEND_CONTEXT (t))
    SET_DECL_FRIEND_CONTEXT (r,
			     tsubst (DECL_FRIEND_CONTEXT (t),
				     args, complain, in_decl));

  if (!apply_late_template_attributes (&r, DECL_ATTRIBUTES (r), 0,
				       args, complain, in_decl))
    return error_mark_node;

  /* Set up the DECL_TEMPLATE_INFO for R.  There's no need to do
     this in the special friend case mentioned above where
     GEN_TMPL is NULL.  */
  if (gen_tmpl && !closure)
    {
      DECL_TEMPLATE_INFO (r)
	= build_template_info (gen_tmpl, argvec);
      SET_DECL_IMPLICIT_INSTANTIATION (r);

      if (use_spec_table)
	{
	  tree new_r
	    = register_specialization (r, gen_tmpl, argvec, false, hash);
	  if (new_r != r)
	    /* We instantiated this while substituting into
	       the type earlier (template/friend54.C).  */
	    return new_r;
	}

      /* We're not supposed to instantiate default arguments
	 until they are called, for a template.  But, for a
	 declaration like:

	 template <class T> void f ()
	 { extern void g(int i = T()); }

	 we should do the substitution when the template is
	 instantiated.  We handle the member function case in
	 instantiate_class_template since the default arguments
	 might refer to other members of the class.  */
      if (!member
	  && !PRIMARY_TEMPLATE_P (gen_tmpl)
	  && !uses_template_parms (argvec))
	tsubst_default_arguments (r, complain);
    }
  else if (DECL_LOCAL_DECL_P (r))
    {
      if (!cp_unevaluated_operand)
	register_local_specialization (r, t);
    }
  else
    DECL_TEMPLATE_INFO (r) = NULL_TREE;

  /* Copy the list of befriending classes.  */
  for (tree *friends = &DECL_BEFRIENDING_CLASSES (r);
       *friends;
       friends = &TREE_CHAIN (*friends))
    {
      *friends = copy_node (*friends);
      TREE_VALUE (*friends)
	= tsubst (TREE_VALUE (*friends), args, complain, in_decl);
    }

  if (DECL_CONSTRUCTOR_P (r) || DECL_DESTRUCTOR_P (r))
    {
      maybe_retrofit_in_chrg (r);
      if (DECL_CONSTRUCTOR_P (r) && !grok_ctor_properties (ctx, r))
	return error_mark_node;
      /* If this is an instantiation of a member template, clone it.
	 If it isn't, that'll be handled by
	 clone_constructors_and_destructors.  */
      if (gen_tmpl && PRIMARY_TEMPLATE_P (gen_tmpl))
	clone_cdtor (r, /*update_methods=*/false);
    }
  else if ((complain & tf_error) != 0
	   && IDENTIFIER_ANY_OP_P (DECL_NAME (r))
	   && !grok_op_properties (r, /*complain=*/true))
    return error_mark_node;

  /* Possibly limit visibility based on template args.  */
  DECL_VISIBILITY (r) = VISIBILITY_DEFAULT;
  if (DECL_VISIBILITY_SPECIFIED (t))
    {
      DECL_VISIBILITY_SPECIFIED (r) = 0;
      DECL_ATTRIBUTES (r)
	= remove_attribute ("visibility", DECL_ATTRIBUTES (r));
    }
  determine_visibility (r);
  if (DECL_DEFAULTED_OUTSIDE_CLASS_P (r)
      && !processing_template_decl)
    defaulted_late_check (r);

  if (flag_openmp)
    if (tree attr = lookup_attribute ("omp declare variant base",
				      DECL_ATTRIBUTES (r)))
      omp_declare_variant_finalize (r, attr);

  return r;
}

/* Subroutine of tsubst_decl for the case when T is a TEMPLATE_DECL.  */

static tree
tsubst_template_decl (tree t, tree args, tsubst_flags_t complain,
		      tree lambda_fntype, tree lambda_tparms)
{
  /* We can get here when processing a member function template,
     member class template, or template template parameter.  */
  tree decl = DECL_TEMPLATE_RESULT (t);
  tree in_decl = t;
  tree spec;
  tree tmpl_args;
  tree full_args;
  tree r;
  hashval_t hash = 0;

  if (DECL_TEMPLATE_TEMPLATE_PARM_P (t))
    {
      /* Template template parameter is treated here.  */
      tree new_type = tsubst (TREE_TYPE (t), args, complain, in_decl);
      if (new_type == error_mark_node)
	r = error_mark_node;
      /* If we get a real template back, return it.  This can happen in
	 the context of most_specialized_partial_spec.  */
      else if (TREE_CODE (new_type) == TEMPLATE_DECL)
	r = new_type;
      else
	/* The new TEMPLATE_DECL was built in
	   reduce_template_parm_level.  */
	r = TEMPLATE_TEMPLATE_PARM_TEMPLATE_DECL (new_type);
      return r;
    }

  if (!lambda_fntype)
    {
      /* We might already have an instance of this template.
	 The ARGS are for the surrounding class type, so the
	 full args contain the tsubst'd args for the context,
	 plus the innermost args from the template decl.  */
      tmpl_args = DECL_CLASS_TEMPLATE_P (t)
	? CLASSTYPE_TI_ARGS (TREE_TYPE (t))
	: DECL_TI_ARGS (DECL_TEMPLATE_RESULT (t));
      /* Because this is a template, the arguments will still be
	 dependent, even after substitution.  If
	 PROCESSING_TEMPLATE_DECL is not set, the dependency
	 predicates will short-circuit.  */
      ++processing_template_decl;
      full_args = tsubst_template_args (tmpl_args, args,
					complain, in_decl);
      --processing_template_decl;
      if (full_args == error_mark_node)
	return error_mark_node;

      /* If this is a default template template argument,
	 tsubst might not have changed anything.  */
      if (full_args == tmpl_args)
	return t;

      hash = spec_hasher::hash (t, full_args);
      spec = retrieve_specialization (t, full_args, hash);
      if (spec != NULL_TREE)
	{
	  if (TYPE_P (spec))
	    /* Type partial instantiations are stored as the type by
	       lookup_template_class_1, not here as the template.  */
	    spec = CLASSTYPE_TI_TEMPLATE (spec);
	  return spec;
	}
    }

  /* Make a new template decl.  It will be similar to the
     original, but will record the current template arguments.
     We also create a new function declaration, which is just
     like the old one, but points to this new template, rather
     than the old one.  */
  r = copy_decl (t);
  gcc_assert (DECL_LANG_SPECIFIC (r) != 0);
  DECL_CHAIN (r) = NULL_TREE;

  // Build new template info linking to the original template decl.
  if (!lambda_fntype)
    {
      DECL_TEMPLATE_INFO (r) = build_template_info (t, args);
      SET_DECL_IMPLICIT_INSTANTIATION (r);
    }
  else
    DECL_TEMPLATE_INFO (r) = NULL_TREE;

  /* The template parameters for this new template are all the
     template parameters for the old template, except the
     outermost level of parameters.  */
  auto tparm_guard = make_temp_override (current_template_parms);
  DECL_TEMPLATE_PARMS (r)
    = current_template_parms
    = (lambda_tparms
       ? lambda_tparms
       : tsubst_template_parms (DECL_TEMPLATE_PARMS (t), args,
				complain));

  bool class_p = false;
  tree inner = decl;
  ++processing_template_decl;
  if (TREE_CODE (inner) == FUNCTION_DECL)
    inner = tsubst_function_decl (inner, args, complain, lambda_fntype);
  else
    {
      if (TREE_CODE (inner) == TYPE_DECL && !TYPE_DECL_ALIAS_P (inner))
	{
	  class_p = true;
	  inner = TREE_TYPE (inner);
	}
      if (class_p)
	inner = tsubst_aggr_type (inner, args, complain,
				  in_decl, /*entering*/1);
      else
	inner = tsubst (inner, args, complain, in_decl);
    }
  --processing_template_decl;
  if (inner == error_mark_node)
    return error_mark_node;

  if (class_p)
    {
      /* For a partial specialization, we need to keep pointing to
	 the primary template.  */
      if (!DECL_TEMPLATE_SPECIALIZATION (t))
	{
	  CLASSTYPE_TI_TEMPLATE (inner) = r;
	  CLASSTYPE_USE_TEMPLATE (inner) = 0;
	}

      DECL_TI_ARGS (r) = CLASSTYPE_TI_ARGS (inner);
      inner = TYPE_MAIN_DECL (inner);
    }
  else if (lambda_fntype)
    {
      tree args = template_parms_to_args (DECL_TEMPLATE_PARMS (r));
      DECL_TEMPLATE_INFO (inner) = build_template_info (r, args);
    }
  else
    {
      DECL_TI_TEMPLATE (inner) = r;
      DECL_TI_ARGS (r) = DECL_TI_ARGS (inner);
    }

  DECL_TEMPLATE_RESULT (r) = inner;
  TREE_TYPE (r) = TREE_TYPE (inner);
  DECL_CONTEXT (r) = DECL_CONTEXT (inner);

  if (modules_p ())
    {
      /* Propagate module information from the decl.  */
      DECL_MODULE_EXPORT_P (r) = DECL_MODULE_EXPORT_P (inner);
      if (DECL_LANG_SPECIFIC (inner))
	/* If this is a constrained template, the above tsubst of
	   inner can find the unconstrained template, which may have
	   come from an import.  This is ok, because we don't
	   register this instantiation (see below).  */
	gcc_checking_assert (!DECL_MODULE_IMPORT_P (inner)
			     || (TEMPLATE_PARMS_CONSTRAINTS
				 (DECL_TEMPLATE_PARMS (t))));
    }

  DECL_TEMPLATE_INSTANTIATIONS (r) = NULL_TREE;
  DECL_TEMPLATE_SPECIALIZATIONS (r) = NULL_TREE;

  if (PRIMARY_TEMPLATE_P (t))
    DECL_PRIMARY_TEMPLATE (r) = r;

  if (TREE_CODE (decl) == FUNCTION_DECL && !lambda_fntype)
    /* Record this non-type partial instantiation.  */
    register_specialization (r, t,
			     DECL_TI_ARGS (DECL_TEMPLATE_RESULT (r)),
			     false, hash);

  return r;
}

/* True if FN is the op() for a lambda in an uninstantiated template.  */

bool
lambda_fn_in_template_p (tree fn)
{
  if (!fn || !LAMBDA_FUNCTION_P (fn))
    return false;
  tree closure = DECL_CONTEXT (fn);
  return CLASSTYPE_TEMPLATE_INFO (closure) != NULL_TREE;
}

/* True if FN is the substitution (via tsubst_lambda_expr) of a function for
   which the above is true.  */

bool
regenerated_lambda_fn_p (tree fn)
{
  if (!fn || !LAMBDA_FUNCTION_P (fn))
    return false;
  tree closure = DECL_CONTEXT (fn);
  tree lam = CLASSTYPE_LAMBDA_EXPR (closure);
  return LAMBDA_EXPR_REGEN_INFO (lam) != NULL_TREE;
}

/* Return the LAMBDA_EXPR from which T was ultimately regenerated.
   If T is not a regenerated LAMBDA_EXPR, return T.  */

tree
most_general_lambda (tree t)
{
  while (tree ti = LAMBDA_EXPR_REGEN_INFO (t))
    t = TI_TEMPLATE (ti);
  return t;
}

/* Return the set of template arguments used to regenerate the lambda T
   from its most general lambda.  */

tree
lambda_regenerating_args (tree t)
{
  if (LAMBDA_FUNCTION_P (t))
    t = CLASSTYPE_LAMBDA_EXPR (DECL_CONTEXT (t));
  gcc_assert (TREE_CODE (t) == LAMBDA_EXPR);
  if (tree ti = LAMBDA_EXPR_REGEN_INFO (t))
    return TI_ARGS (ti);
  else
    return NULL_TREE;
}

/* We're instantiating a variable from template function TCTX.  Return the
   corresponding current enclosing scope.  We can match them up using
   DECL_SOURCE_LOCATION because lambdas only ever have one source location, and
   the DECL_SOURCE_LOCATION for a function instantiation is updated to match
   the template definition in regenerate_decl_from_template.  */

static tree
enclosing_instantiation_of (tree tctx)
{
  tree fn = current_function_decl;

  /* We shouldn't ever need to do this for other artificial functions.  */
  gcc_assert (!DECL_ARTIFICIAL (tctx) || LAMBDA_FUNCTION_P (tctx));

  for (; fn; fn = decl_function_context (fn))
    if (DECL_SOURCE_LOCATION (fn) == DECL_SOURCE_LOCATION (tctx))
      return fn;
  gcc_unreachable ();
}

/* Substitute the ARGS into the T, which is a _DECL.  Return the
   result of the substitution.  Issue error and warning messages under
   control of COMPLAIN.  The flag USE_SPEC_TABLE controls if we look up
   and insert into the specializations table or if we can assume it's
   the caller's responsibility; this is used by instantiate_template
   to avoid doing some redundant work.  */

static tree
tsubst_decl (tree t, tree args, tsubst_flags_t complain,
	     bool use_spec_table /* = true */)
{
#define RETURN(EXP) do { r = (EXP); goto out; } while(0)
  location_t saved_loc;
  tree r = NULL_TREE;
  tree in_decl = t;
  hashval_t hash = 0;

  if (t == error_mark_node)
    return error_mark_node;

  /* Set the filename and linenumber to improve error-reporting.  */
  saved_loc = input_location;
  input_location = DECL_SOURCE_LOCATION (t);

  switch (TREE_CODE (t))
    {
    case TEMPLATE_DECL:
      r = tsubst_template_decl (t, args, complain,
				/*lambda_fntype=*/NULL_TREE,
				/*lambda_tparms=*/NULL_TREE);
      break;

    case FUNCTION_DECL:
      r = tsubst_function_decl (t, args, complain, /*lambda*/NULL_TREE,
				use_spec_table);
      break;

    case PARM_DECL:
      {
	tree type = NULL_TREE;
        int i, len = 1;
        tree expanded_types = NULL_TREE;
        tree prev_r = NULL_TREE;
        tree first_r = NULL_TREE;

        if (DECL_PACK_P (t))
          {
            /* If there is a local specialization that isn't a
               parameter pack, it means that we're doing a "simple"
               substitution from inside tsubst_pack_expansion. Just
               return the local specialization (which will be a single
               parm).  */
            tree spec = retrieve_local_specialization (t);
            if (spec 
                && TREE_CODE (spec) == PARM_DECL
                && TREE_CODE (TREE_TYPE (spec)) != TYPE_PACK_EXPANSION)
              RETURN (spec);

            /* Expand the TYPE_PACK_EXPANSION that provides the types for
               the parameters in this function parameter pack.  */
            expanded_types = tsubst_pack_expansion (TREE_TYPE (t), args,
						    complain, in_decl);
            if (TREE_CODE (expanded_types) == TREE_VEC)
              {
                len = TREE_VEC_LENGTH (expanded_types);

                /* Zero-length parameter packs are boring. Just substitute
                   into the chain.  */
		if (len == 0 && !cp_unevaluated_operand)
                  RETURN (tsubst (TREE_CHAIN (t), args, complain,
				  TREE_CHAIN (t)));
              }
            else
              {
                /* All we did was update the type. Make a note of that.  */
                type = expanded_types;
                expanded_types = NULL_TREE;
              }
          }

        /* Loop through all of the parameters we'll build. When T is
           a function parameter pack, LEN is the number of expanded
           types in EXPANDED_TYPES; otherwise, LEN is 1.  */
        r = NULL_TREE;
        for (i = 0; i < len; ++i)
          {
            prev_r = r;
            r = copy_node (t);
            if (DECL_TEMPLATE_PARM_P (t))
              SET_DECL_TEMPLATE_PARM_P (r);

            if (expanded_types)
              /* We're on the Ith parameter of the function parameter
                 pack.  */
              {
                /* Get the Ith type.  */
                type = TREE_VEC_ELT (expanded_types, i);

		/* Rename the parameter to include the index.  */
		DECL_NAME (r)
		  = make_ith_pack_parameter_name (DECL_NAME (r), i);
              }
            else if (!type)
              /* We're dealing with a normal parameter.  */
              type = tsubst (TREE_TYPE (t), args, complain, in_decl);

            type = type_decays_to (type);
            TREE_TYPE (r) = type;
            cp_apply_type_quals_to_decl (cp_type_quals (type), r);

            if (DECL_INITIAL (r))
              {
                if (TREE_CODE (DECL_INITIAL (r)) != TEMPLATE_PARM_INDEX)
                  DECL_INITIAL (r) = TREE_TYPE (r);
                else
                  DECL_INITIAL (r) = tsubst (DECL_INITIAL (r), args,
                                             complain, in_decl);
              }

            DECL_CONTEXT (r) = NULL_TREE;

            if (!DECL_TEMPLATE_PARM_P (r))
              DECL_ARG_TYPE (r) = type_passed_as (type);

	    if (!apply_late_template_attributes (&r, DECL_ATTRIBUTES (r), 0,
						 args, complain, in_decl))
	      return error_mark_node;

            /* Keep track of the first new parameter we
               generate. That's what will be returned to the
               caller.  */
            if (!first_r)
              first_r = r;

            /* Build a proper chain of parameters when substituting
               into a function parameter pack.  */
            if (prev_r)
              DECL_CHAIN (prev_r) = r;
          }

	/* If cp_unevaluated_operand is set, we're just looking for a
	   single dummy parameter, so don't keep going.  */
	if (DECL_CHAIN (t) && !cp_unevaluated_operand)
	  DECL_CHAIN (r) = tsubst (DECL_CHAIN (t), args,
				   complain, DECL_CHAIN (t));

        /* FIRST_R contains the start of the chain we've built.  */
        r = first_r;
      }
      break;

    case FIELD_DECL:
      {
	tree type = NULL_TREE;
	tree vec = NULL_TREE;
	tree expanded_types = NULL_TREE;
	int len = 1;

	if (PACK_EXPANSION_P (TREE_TYPE (t)))
	  {
	    /* This field is a lambda capture pack.  Return a TREE_VEC of
	       the expanded fields to instantiate_class_template_1.  */
            expanded_types = tsubst_pack_expansion (TREE_TYPE (t), args,
						    complain, in_decl);
            if (TREE_CODE (expanded_types) == TREE_VEC)
              {
                len = TREE_VEC_LENGTH (expanded_types);
		vec = make_tree_vec (len);
              }
            else
              {
                /* All we did was update the type. Make a note of that.  */
                type = expanded_types;
                expanded_types = NULL_TREE;
              }
	  }

	for (int i = 0; i < len; ++i)
	  {
	    r = copy_decl (t);
	    if (expanded_types)
	      {
		type = TREE_VEC_ELT (expanded_types, i);
		DECL_NAME (r)
		  = make_ith_pack_parameter_name (DECL_NAME (r), i);
	      }
            else if (!type)
              type = tsubst (TREE_TYPE (t), args, complain, in_decl);

	    if (type == error_mark_node)
	      RETURN (error_mark_node);
	    TREE_TYPE (r) = type;
	    cp_apply_type_quals_to_decl (cp_type_quals (type), r);

	    if (DECL_C_BIT_FIELD (r))
	      /* For bit-fields, DECL_BIT_FIELD_REPRESENTATIVE gives the
		 number of bits.  */
	      DECL_BIT_FIELD_REPRESENTATIVE (r)
		= tsubst_expr (DECL_BIT_FIELD_REPRESENTATIVE (t), args,
			       complain, in_decl);
	    if (DECL_INITIAL (t))
	      {
		/* Set up DECL_TEMPLATE_INFO so that we can get at the
		   NSDMI in perform_member_init.  Still set DECL_INITIAL
		   so that we know there is one.  */
		DECL_INITIAL (r) = void_node;
		gcc_assert (DECL_LANG_SPECIFIC (r) == NULL);
		retrofit_lang_decl (r);
		DECL_TEMPLATE_INFO (r) = build_template_info (t, args);
	      }
	    /* We don't have to set DECL_CONTEXT here; it is set by
	       finish_member_declaration.  */
	    DECL_CHAIN (r) = NULL_TREE;

	    if (!apply_late_template_attributes (&r, DECL_ATTRIBUTES (r), 0,
						 args, complain, in_decl))
	      return error_mark_node;

	    if (vec)
	      TREE_VEC_ELT (vec, i) = r;
	  }

	if (vec)
	  r = vec;
      }
      break;

    case USING_DECL:
      /* We reach here only for member using decls.  We also need to check
	 uses_template_parms because DECL_DEPENDENT_P is not set for a
	 using-declaration that designates a member of the current
	 instantiation (c++/53549).  */
      if (DECL_DEPENDENT_P (t)
	  || uses_template_parms (USING_DECL_SCOPE (t)))
	{
	  /* True iff this using-decl was written as a pack expansion
	     (and a pack appeared in its scope or name).  If a pack
	     appeared in both, we expand the packs separately and
	     manually merge them.  */
	  bool variadic_p = false;

	  tree scope = USING_DECL_SCOPE (t);
	  if (PACK_EXPANSION_P (scope))
	    {
	      scope = tsubst_pack_expansion (scope, args,
					     complain | tf_qualifying_scope,
					     in_decl);
	      variadic_p = true;
	    }
	  else
	    scope = tsubst_scope (scope, args, complain, in_decl);

	  tree name = DECL_NAME (t);
	  if (IDENTIFIER_CONV_OP_P (name)
	      && PACK_EXPANSION_P (TREE_TYPE (name)))
	    {
	      name = tsubst_pack_expansion (TREE_TYPE (name), args,
					    complain, in_decl);
	      if (name == error_mark_node)
		{
		  r = error_mark_node;
		  break;
		}
	      for (tree& elt : tree_vec_range (name))
		elt = make_conv_op_name (elt);
	      variadic_p = true;
	    }
	  else
	    name = tsubst_copy (name, args, complain, in_decl);

	  int len;
	  if (!variadic_p)
	    len = 1;
	  else if (TREE_CODE (scope) == TREE_VEC
		   && TREE_CODE (name) == TREE_VEC)
	    {
	      if (TREE_VEC_LENGTH (scope) != TREE_VEC_LENGTH (name))
		{
		  error ("mismatched argument pack lengths (%d vs %d)",
			 TREE_VEC_LENGTH (scope), TREE_VEC_LENGTH (name));
		  r = error_mark_node;
		  break;
		}
	      len = TREE_VEC_LENGTH (scope);
	    }
	  else if (TREE_CODE (scope) == TREE_VEC)
	    len = TREE_VEC_LENGTH (scope);
	  else /* TREE_CODE (name) == TREE_VEC  */
	    len = TREE_VEC_LENGTH (name);

	  r = make_tree_vec (len);
	  for (int i = 0; i < len; ++i)
	    {
	      tree escope = (TREE_CODE (scope) == TREE_VEC
			     ? TREE_VEC_ELT (scope, i)
			     : scope);
	      tree ename = (TREE_CODE (name) == TREE_VEC
			    ? TREE_VEC_ELT (name, i)
			    : name);
	      tree elt = do_class_using_decl (escope, ename);
	      if (!elt)
		{
		  r = error_mark_node;
		  break;
		}
	      TREE_PROTECTED (elt) = TREE_PROTECTED (t);
	      TREE_PRIVATE (elt) = TREE_PRIVATE (t);
	      TREE_VEC_ELT (r, i) = elt;
	    }

	  if (!variadic_p && r != error_mark_node)
	    r = TREE_VEC_ELT (r, 0);
	}
      else
	{
	  r = copy_node (t);
	  DECL_CHAIN (r) = NULL_TREE;
	}
      break;

    case TYPE_DECL:
    case VAR_DECL:
      {
	tree argvec = NULL_TREE;
	tree gen_tmpl = NULL_TREE;
	tree tmpl = NULL_TREE;
	tree type = NULL_TREE;

	if (TREE_TYPE (t) == error_mark_node)
	  RETURN (error_mark_node);

	if (TREE_CODE (t) == TYPE_DECL
	    && t == TYPE_MAIN_DECL (TREE_TYPE (t)))
	  {
	    /* If this is the canonical decl, we don't have to
	       mess with instantiations, and often we can't (for
	       typename, template type parms and such).  Note that
	       TYPE_NAME is not correct for the above test if
	       we've copied the type for a typedef.  */
	    type = tsubst (TREE_TYPE (t), args, complain, in_decl);
	    if (type == error_mark_node)
	      RETURN (error_mark_node);
	    r = TYPE_NAME (type);
	    break;
	  }

	/* Check to see if we already have the specialization we
	   need.  */
	tree spec = NULL_TREE;
	bool local_p = false;
	tree ctx = DECL_CONTEXT (t);
	if (!(VAR_P (t) && DECL_LOCAL_DECL_P (t))
	    && (DECL_CLASS_SCOPE_P (t) || DECL_NAMESPACE_SCOPE_P (t)))
	  {
	    local_p = false;
	    if (DECL_CLASS_SCOPE_P (t))
	      {
		ctx = tsubst_aggr_type (ctx, args,
					complain,
					in_decl, /*entering_scope=*/1);
		if (DECL_SELF_REFERENCE_P (t))
		  /* The context and type of an injected-class-name are
		     the same, so we don't need to substitute both.  */
		  type = ctx;
		/* If CTX is unchanged, then T is in fact the
		   specialization we want.  That situation occurs when
		   referencing a static data member within in its own
		   class.  We can use pointer equality, rather than
		   same_type_p, because DECL_CONTEXT is always
		   canonical...  */
		if (ctx == DECL_CONTEXT (t)
		    /* ... unless T is a member template; in which
		       case our caller can be willing to create a
		       specialization of that template represented
		       by T.  */
		    && !(DECL_TI_TEMPLATE (t)
			 && DECL_MEMBER_TEMPLATE_P (DECL_TI_TEMPLATE (t))))
		  spec = t;
	      }

	    if (!spec)
	      {
		tmpl = DECL_TI_TEMPLATE (t);
		if (use_spec_table)
		  {
		    argvec = tsubst (DECL_TI_ARGS (t), args, complain, in_decl);
		    if (argvec == error_mark_node)
		      RETURN (error_mark_node);
		    gen_tmpl = most_general_template (tmpl);
		    hash = spec_hasher::hash (gen_tmpl, argvec);
		    spec = retrieve_specialization (gen_tmpl, argvec, hash);
		  }
		else
		  argvec = args;
	      }
	  }
	else
	  {
	    if (!(VAR_P (t) && DECL_LOCAL_DECL_P (t)))
	      /* Subsequent calls to pushdecl will fill this in.  */
	      ctx = NULL_TREE;
	    /* A local variable.  */
	    local_p = true;
	    /* Unless this is a reference to a static variable from an
	       enclosing function, in which case we need to fill it in now.  */
	    if (TREE_STATIC (t))
	      {
		tree fn = enclosing_instantiation_of (DECL_CONTEXT (t));
		if (fn != current_function_decl)
		  ctx = fn;
	      }
	    spec = retrieve_local_specialization (t);
	  }
	/* If we already have the specialization we need, there is
	   nothing more to do.  */
	if (spec)
	  {
	    r = spec;
	    break;
	  }

	/* Create a new node for the specialization we need.  */
	if (type == NULL_TREE)
	  {
	    if (is_typedef_decl (t))
	      type = DECL_ORIGINAL_TYPE (t);
	    else
	      type = TREE_TYPE (t);
	    if (VAR_P (t)
		&& VAR_HAD_UNKNOWN_BOUND (t)
		&& type != error_mark_node)
	      type = strip_array_domain (type);
	    tsubst_flags_t tcomplain = complain;
	    if (VAR_P (t))
	      tcomplain |= tf_tst_ok;
	    type = tsubst (type, args, tcomplain, in_decl);
	    /* Substituting the type might have recursively instantiated this
	       same alias (c++/86171).  */
	    if (use_spec_table && gen_tmpl && DECL_ALIAS_TEMPLATE_P (gen_tmpl)
		&& (spec = retrieve_specialization (gen_tmpl, argvec, hash)))
	      {
		r = spec;
		break;
	      }
	  }
	if (type == error_mark_node && !(complain & tf_error))
	  RETURN (error_mark_node);
	r = copy_decl (t);
	if (VAR_P (r))
	  {
	    DECL_INITIALIZED_P (r) = 0;
	    DECL_TEMPLATE_INSTANTIATED (r) = 0;
	    if (TREE_CODE (type) == FUNCTION_TYPE)
	      {
		/* It may seem that this case cannot occur, since:

		   typedef void f();
		   void g() { f x; }

		   declares a function, not a variable.  However:

		   typedef void f();
		   template <typename T> void g() { T t; }
		   template void g<f>();

		   is an attempt to declare a variable with function
		   type.  */
		error ("variable %qD has function type",
		       /* R is not yet sufficiently initialized, so we
			  just use its name.  */
		       DECL_NAME (r));
		RETURN (error_mark_node);
	      }
	    type = complete_type (type);
	    /* Wait until cp_finish_decl to set this again, to handle
	       circular dependency (template/instantiate6.C). */
	    DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (r) = 0;
	    type = check_var_type (DECL_NAME (r), type,
				   DECL_SOURCE_LOCATION (r));
	    if (DECL_HAS_VALUE_EXPR_P (t))
	      {
		tree ve = DECL_VALUE_EXPR (t);
		/* If the DECL_VALUE_EXPR is converted to the declared type,
		   preserve the identity so that gimplify_type_sizes works.  */
		bool nop = (TREE_CODE (ve) == NOP_EXPR);
		if (nop)
		  ve = TREE_OPERAND (ve, 0);
		ve = tsubst_expr (ve, args, complain, in_decl);
		if (REFERENCE_REF_P (ve))
		  {
		    gcc_assert (TYPE_REF_P (type));
		    ve = TREE_OPERAND (ve, 0);
		  }
		if (nop)
		  ve = build_nop (type, ve);
		else if (DECL_LANG_SPECIFIC (t)
			 && DECL_OMP_PRIVATIZED_MEMBER (t)
			 && TREE_CODE (ve) == COMPONENT_REF
			 && TREE_CODE (TREE_OPERAND (ve, 1)) == FIELD_DECL
			 && DECL_BIT_FIELD_TYPE (TREE_OPERAND (ve, 1)) == type)
		  type = TREE_TYPE (ve);
		else
		  gcc_checking_assert (TYPE_MAIN_VARIANT (TREE_TYPE (ve))
				       == TYPE_MAIN_VARIANT (type));
		SET_DECL_VALUE_EXPR (r, ve);
	      }
	    if (CP_DECL_THREAD_LOCAL_P (r)
		&& !processing_template_decl)
	      set_decl_tls_model (r, decl_default_tls_model (r));
	  }
	else if (DECL_SELF_REFERENCE_P (t))
	  SET_DECL_SELF_REFERENCE_P (r);
	TREE_TYPE (r) = type;
	cp_apply_type_quals_to_decl (cp_type_quals (type), r);
	DECL_CONTEXT (r) = ctx;
	/* Clear out the mangled name and RTL for the instantiation.  */
	SET_DECL_ASSEMBLER_NAME (r, NULL_TREE);
	if (CODE_CONTAINS_STRUCT (TREE_CODE (t), TS_DECL_WRTL))
	  SET_DECL_RTL (r, NULL);
	set_instantiating_module (r);

	/* The initializer must not be expanded until it is required;
	   see [temp.inst].  */
	DECL_INITIAL (r) = NULL_TREE;
	DECL_SIZE (r) = DECL_SIZE_UNIT (r) = 0;
	if (VAR_P (r))
	  {
	    if (DECL_LANG_SPECIFIC (r))
	      SET_DECL_DEPENDENT_INIT_P (r, false);

	    SET_DECL_MODE (r, VOIDmode);

	    /* Possibly limit visibility based on template args.  */
	    DECL_VISIBILITY (r) = VISIBILITY_DEFAULT;
	    if (DECL_VISIBILITY_SPECIFIED (t))
	      {
		DECL_VISIBILITY_SPECIFIED (r) = 0;
		DECL_ATTRIBUTES (r)
		  = remove_attribute ("visibility", DECL_ATTRIBUTES (r));
	      }
	    determine_visibility (r);
	  }

	if (!local_p)
	  {
	    /* A static data member declaration is always marked
	       external when it is declared in-class, even if an
	       initializer is present.  We mimic the non-template
	       processing here.  */
	    DECL_EXTERNAL (r) = 1;
	    if (DECL_NAMESPACE_SCOPE_P (t))
	      DECL_NOT_REALLY_EXTERN (r) = 1;

	    DECL_TEMPLATE_INFO (r) = build_template_info (tmpl, argvec);
	    SET_DECL_IMPLICIT_INSTANTIATION (r);
	    if (use_spec_table)
	      register_specialization (r, gen_tmpl, argvec, false, hash);
	  }
	else
	  {
	    if (DECL_LANG_SPECIFIC (r))
	      DECL_TEMPLATE_INFO (r) = NULL_TREE;
	    if (!cp_unevaluated_operand)
	      register_local_specialization (r, t);
	  }

	DECL_CHAIN (r) = NULL_TREE;

	if (!apply_late_template_attributes (&r, DECL_ATTRIBUTES (r),
					     /*flags=*/0,
					     args, complain, in_decl))
	  return error_mark_node;

	/* Preserve a typedef that names a type.  */
	if (is_typedef_decl (r) && type != error_mark_node)
	  {
	    DECL_ORIGINAL_TYPE (r) = NULL_TREE;
	    set_underlying_type (r);

	    /* common_handle_aligned_attribute doesn't apply the alignment
	       to DECL_ORIGINAL_TYPE.  */
	    if (TYPE_USER_ALIGN (TREE_TYPE (t)))
	      TREE_TYPE (r) = build_aligned_type (TREE_TYPE (r),
						  TYPE_ALIGN (TREE_TYPE (t)));
	  }

	layout_decl (r, 0);
      }
      break;

    default:
      gcc_unreachable ();
    }
#undef RETURN

 out:
  /* Restore the file and line information.  */
  input_location = saved_loc;

  return r;
}

/* Substitute into the complete parameter type list PARMS.  */

tree
tsubst_function_parms (tree parms,
		       tree args,
		       tsubst_flags_t complain,
		       tree in_decl)
{
  return tsubst_arg_types (parms, args, NULL_TREE, complain, in_decl);
}

/* Substitute into the ARG_TYPES of a function type.
   If END is a TREE_CHAIN, leave it and any following types
   un-substituted.  */

static tree
tsubst_arg_types (tree arg_types,
		  tree args,
		  tree end,
		  tsubst_flags_t complain,
		  tree in_decl)
{
  tree type = NULL_TREE;
  int len = 1;
  tree expanded_args = NULL_TREE;

  if (!arg_types || arg_types == void_list_node || arg_types == end)
    return arg_types;

  if (PACK_EXPANSION_P (TREE_VALUE (arg_types)))
    {
      /* For a pack expansion, perform substitution on the
         entire expression. Later on, we'll handle the arguments
         one-by-one.  */
      expanded_args = tsubst_pack_expansion (TREE_VALUE (arg_types),
                                            args, complain, in_decl);

      if (TREE_CODE (expanded_args) == TREE_VEC)
        /* So that we'll spin through the parameters, one by one.  */
	len = TREE_VEC_LENGTH (expanded_args);
      else
        {
          /* We only partially substituted into the parameter
             pack. Our type is TYPE_PACK_EXPANSION.  */
          type = expanded_args;
          expanded_args = NULL_TREE;
        }
    }
  else
    type = tsubst (TREE_VALUE (arg_types), args, complain, in_decl);

  /* Check if a substituted type is erroneous before substituting into
     the rest of the chain.  */
  for (int i = 0; i < len; i++)
    {
      if (expanded_args)
	type = TREE_VEC_ELT (expanded_args, i);

      if (type == error_mark_node)
	return error_mark_node;
      if (VOID_TYPE_P (type))
	{
	  if (complain & tf_error)
	    {
	      error ("invalid parameter type %qT", type);
	      if (in_decl)
		error ("in declaration %q+D", in_decl);
	    }
	  return error_mark_node;
	}
    }

  /* We do not substitute into default arguments here.  The standard
     mandates that they be instantiated only when needed, which is
     done in build_over_call.  */
  tree default_arg = TREE_PURPOSE (arg_types);

  /* Except that we do substitute default arguments under tsubst_lambda_expr,
     since the new op() won't have any associated template arguments for us
     to refer to later.  */
  if (lambda_fn_in_template_p (in_decl)
      || (in_decl && TREE_CODE (in_decl) == FUNCTION_DECL
	  && DECL_LOCAL_DECL_P (in_decl)))
    default_arg = tsubst_copy_and_build (default_arg, args, complain, in_decl);

  tree remaining_arg_types = tsubst_arg_types (TREE_CHAIN (arg_types),
					       args, end, complain, in_decl);
  if (remaining_arg_types == error_mark_node)
    return error_mark_node;

  for (int i = len-1; i >= 0; i--)
    {
      if (expanded_args)
	type = TREE_VEC_ELT (expanded_args, i);

      /* Do array-to-pointer, function-to-pointer conversion, and ignore
	 top-level qualifiers as required.  */
      type = cv_unqualified (type_decays_to (type));

      if (default_arg && TREE_CODE (default_arg) == DEFERRED_PARSE)
	{
	  /* We've instantiated a template before its default arguments
	     have been parsed.  This can happen for a nested template
	     class, and is not an error unless we require the default
	     argument in a call of this function.  */
	  remaining_arg_types
	    = tree_cons (default_arg, type, remaining_arg_types);
	  vec_safe_push (DEFPARSE_INSTANTIATIONS (default_arg),
			 remaining_arg_types);
	}
      else
	remaining_arg_types
	  = hash_tree_cons (default_arg, type, remaining_arg_types);
    }

  return remaining_arg_types;
}

/* Substitute into a FUNCTION_TYPE or METHOD_TYPE.  This routine does
   *not* handle the exception-specification for FNTYPE, because the
   initial substitution of explicitly provided template parameters
   during argument deduction forbids substitution into the
   exception-specification:

     [temp.deduct]

     All references in the function type of the function template to  the
     corresponding template parameters are replaced by the specified tem-
     plate argument values.  If a substitution in a template parameter or
     in  the function type of the function template results in an invalid
     type, type deduction fails.  [Note: The equivalent  substitution  in
     exception specifications is done only when the function is instanti-
     ated, at which point a program is  ill-formed  if  the  substitution
     results in an invalid type.]  */

static tree
tsubst_function_type (tree t,
		      tree args,
		      tsubst_flags_t complain,
		      tree in_decl)
{
  tree return_type;
  tree arg_types = NULL_TREE;

  /* The TYPE_CONTEXT is not used for function/method types.  */
  gcc_assert (TYPE_CONTEXT (t) == NULL_TREE);

  /* DR 1227: Mixing immediate and non-immediate contexts in deduction
     failure.  */
  bool late_return_type_p = TYPE_HAS_LATE_RETURN_TYPE (t);

  if (late_return_type_p)
    {
      /* Substitute the argument types.  */
      arg_types = tsubst_arg_types (TYPE_ARG_TYPES (t), args, NULL_TREE,
				    complain, in_decl);
      if (arg_types == error_mark_node)
	return error_mark_node;

      tree save_ccp = current_class_ptr;
      tree save_ccr = current_class_ref;
      tree this_type = (TREE_CODE (t) == METHOD_TYPE
			? TREE_TYPE (TREE_VALUE (arg_types)) : NULL_TREE);
      bool do_inject = this_type && CLASS_TYPE_P (this_type);
      if (do_inject)
	{
	  /* DR 1207: 'this' is in scope in the trailing return type.  */
	  inject_this_parameter (this_type, cp_type_quals (this_type));
	}

      /* Substitute the return type.  */
      return_type = tsubst (TREE_TYPE (t), args, complain, in_decl);

      if (do_inject)
	{
	  current_class_ptr = save_ccp;
	  current_class_ref = save_ccr;
	}
    }
  else
    /* Substitute the return type.  */
    return_type = tsubst (TREE_TYPE (t), args, complain, in_decl);

  if (return_type == error_mark_node)
    return error_mark_node;
  /* DR 486 clarifies that creation of a function type with an
     invalid return type is a deduction failure.  */
  if (TREE_CODE (return_type) == ARRAY_TYPE
      || TREE_CODE (return_type) == FUNCTION_TYPE)
    {
      if (complain & tf_error)
	{
	  if (TREE_CODE (return_type) == ARRAY_TYPE)
	    error ("function returning an array");
	  else
	    error ("function returning a function");
	}
      return error_mark_node;
    }

  if (!late_return_type_p)
    {
      /* Substitute the argument types.  */
      arg_types = tsubst_arg_types (TYPE_ARG_TYPES (t), args, NULL_TREE,
				    complain, in_decl);
      if (arg_types == error_mark_node)
	return error_mark_node;
    }

  /* Construct a new type node and return it.  */
  return rebuild_function_or_method_type (t, return_type, arg_types,
					  /*raises=*/NULL_TREE, complain);
}

/* FNTYPE is a FUNCTION_TYPE or METHOD_TYPE.  Substitute the template
   ARGS into that specification, and return the substituted
   specification.  If there is no specification, return NULL_TREE.  */

static tree
tsubst_exception_specification (tree fntype,
				tree args,
				tsubst_flags_t complain,
				tree in_decl,
				bool defer_ok)
{
  tree specs;
  tree new_specs;

  specs = TYPE_RAISES_EXCEPTIONS (fntype);
  new_specs = NULL_TREE;
  if (specs && TREE_PURPOSE (specs))
    {
      /* A noexcept-specifier.  */
      tree expr = TREE_PURPOSE (specs);
      if (TREE_CODE (expr) == INTEGER_CST)
	new_specs = expr;
      else if (defer_ok)
	{
	  /* Defer instantiation of noexcept-specifiers to avoid
	     excessive instantiations (c++/49107).  */
	  new_specs = make_node (DEFERRED_NOEXCEPT);
	  if (DEFERRED_NOEXCEPT_SPEC_P (specs))
	    {
	      /* We already partially instantiated this member template,
		 so combine the new args with the old.  */
	      DEFERRED_NOEXCEPT_PATTERN (new_specs)
		= DEFERRED_NOEXCEPT_PATTERN (expr);
	      DEFERRED_NOEXCEPT_ARGS (new_specs)
		= add_to_template_args (DEFERRED_NOEXCEPT_ARGS (expr), args);
	    }
	  else
	    {
	      DEFERRED_NOEXCEPT_PATTERN (new_specs) = expr;
	      DEFERRED_NOEXCEPT_ARGS (new_specs) = args;
	    }
	}
      else
	{
	  if (DEFERRED_NOEXCEPT_SPEC_P (specs))
	    {
	      args = add_to_template_args (DEFERRED_NOEXCEPT_ARGS (expr),
					   args);
	      expr = DEFERRED_NOEXCEPT_PATTERN (expr);
	    }
	  new_specs = tsubst_copy_and_build (expr, args, complain, in_decl);
	}
      new_specs = build_noexcept_spec (new_specs, complain);
      /* We've instantiated a template before a noexcept-specifier
	 contained therein has been parsed.  This can happen for
	 a nested template class:

	  struct S {
	    template<typename> struct B { B() noexcept(...); };
	    struct A : B<int> { ... use B() ... };
	  };

	 where completing B<int> will trigger instantiating the
	 noexcept, even though we only parse it at the end of S.  */
      if (UNPARSED_NOEXCEPT_SPEC_P (specs))
	{
	  gcc_checking_assert (defer_ok);
	  vec_safe_push (DEFPARSE_INSTANTIATIONS (expr), new_specs);
	}
    }
  else if (specs)
    {
      if (! TREE_VALUE (specs))
	new_specs = specs;
      else
	while (specs)
	  {
	    tree spec;
            int i, len = 1;
            tree expanded_specs = NULL_TREE;

            if (PACK_EXPANSION_P (TREE_VALUE (specs)))
              {
                /* Expand the pack expansion type.  */
                expanded_specs = tsubst_pack_expansion (TREE_VALUE (specs),
                                                       args, complain,
                                                       in_decl);

		if (expanded_specs == error_mark_node)
		  return error_mark_node;
		else if (TREE_CODE (expanded_specs) == TREE_VEC)
		  len = TREE_VEC_LENGTH (expanded_specs);
		else
		  {
		    /* We're substituting into a member template, so
		       we got a TYPE_PACK_EXPANSION back.  Add that
		       expansion and move on.  */
		    gcc_assert (TREE_CODE (expanded_specs)
				== TYPE_PACK_EXPANSION);
		    new_specs = add_exception_specifier (new_specs,
							 expanded_specs,
							 complain);
		    specs = TREE_CHAIN (specs);
		    continue;
		  }
              }

            for (i = 0; i < len; ++i)
              {
                if (expanded_specs)
                  spec = TREE_VEC_ELT (expanded_specs, i);
                else
                  spec = tsubst (TREE_VALUE (specs), args, complain, in_decl);
                if (spec == error_mark_node)
                  return spec;
                new_specs = add_exception_specifier (new_specs, spec, 
                                                     complain);
              }

            specs = TREE_CHAIN (specs);
	  }
    }
  return new_specs;
}

/* Substitute through a TREE_LIST of types or expressions, handling pack
   expansions.  */

tree
tsubst_tree_list (tree t, tree args, tsubst_flags_t complain, tree in_decl)
{
  if (t == void_list_node)
    return t;

  tree purpose = TREE_PURPOSE (t);
  tree purposevec = NULL_TREE;
  if (!purpose)
    ;
  else if (PACK_EXPANSION_P (purpose))
    {
      purpose = tsubst_pack_expansion (purpose, args, complain, in_decl);
      if (TREE_CODE (purpose) == TREE_VEC)
	purposevec = purpose;
    }
  else if (TYPE_P (purpose))
    purpose = tsubst (purpose, args, complain, in_decl);
  else
    purpose = tsubst_copy_and_build (purpose, args, complain, in_decl);
  if (purpose == error_mark_node || purposevec == error_mark_node)
    return error_mark_node;

  tree value = TREE_VALUE (t);
  tree valuevec = NULL_TREE;
  if (!value)
    ;
  else if (PACK_EXPANSION_P (value))
    {
      value = tsubst_pack_expansion (value, args, complain, in_decl);
      if (TREE_CODE (value) == TREE_VEC)
	valuevec = value;
    }
  else if (TYPE_P (value))
    value = tsubst (value, args, complain, in_decl);
  else
    value = tsubst_copy_and_build (value, args, complain, in_decl);
  if (value == error_mark_node || valuevec == error_mark_node)
    return error_mark_node;

  tree chain = TREE_CHAIN (t);
  if (!chain)
    ;
  else if (TREE_CODE (chain) == TREE_LIST)
    chain = tsubst_tree_list (chain, args, complain, in_decl);
  else if (TYPE_P (chain))
    chain = tsubst (chain, args, complain, in_decl);
  else
    chain = tsubst_copy_and_build (chain, args, complain, in_decl);
  if (chain == error_mark_node)
    return error_mark_node;

  if (purpose == TREE_PURPOSE (t)
      && value == TREE_VALUE (t)
      && chain == TREE_CHAIN (t))
    return t;

  int len;
  /* Determine the number of arguments.  */
  if (purposevec)
    {
      len = TREE_VEC_LENGTH (purposevec);
      gcc_assert (!valuevec || len == TREE_VEC_LENGTH (valuevec));
    }
  else if (valuevec)
    len = TREE_VEC_LENGTH (valuevec);
  else
    len = 1;

  for (int i = len; i-- > 0; )
    {
      if (purposevec)
	purpose = TREE_VEC_ELT (purposevec, i);
      if (valuevec)
	value = TREE_VEC_ELT (valuevec, i);

      if (value && TYPE_P (value))
	chain = hash_tree_cons (purpose, value, chain);
      else
	chain = tree_cons (purpose, value, chain);
    }

  return chain;
}

/* Take the tree structure T and replace template parameters used
   therein with the argument vector ARGS.  IN_DECL is an associated
   decl for diagnostics.  If an error occurs, returns ERROR_MARK_NODE.
   Issue error and warning messages under control of COMPLAIN.  Note
   that we must be relatively non-tolerant of extensions here, in
   order to preserve conformance; if we allow substitutions that
   should not be allowed, we may allow argument deductions that should
   not succeed, and therefore report ambiguous overload situations
   where there are none.  In theory, we could allow the substitution,
   but indicate that it should have failed, and allow our caller to
   make sure that the right thing happens, but we don't try to do this
   yet.

   This function is used for dealing with types, decls and the like;
   for expressions, use tsubst_expr or tsubst_copy.  */

tree
tsubst (tree t, tree args, tsubst_flags_t complain, tree in_decl)
{
  enum tree_code code;
  tree type, r = NULL_TREE;

  if (t == NULL_TREE || t == error_mark_node
      || t == integer_type_node
      || t == void_type_node
      || t == char_type_node
      || t == unknown_type_node
      || TREE_CODE (t) == NAMESPACE_DECL
      || TREE_CODE (t) == TRANSLATION_UNIT_DECL)
    return t;

  tsubst_flags_t tst_ok_flag = (complain & tf_tst_ok);
  complain &= ~tf_tst_ok;

  tsubst_flags_t qualifying_scope_flag = (complain & tf_qualifying_scope);
  complain &= ~tf_qualifying_scope;

  if (DECL_P (t))
    return tsubst_decl (t, args, complain);

  if (args == NULL_TREE)
    return t;

  code = TREE_CODE (t);

  gcc_assert (code != IDENTIFIER_NODE);
  type = TREE_TYPE (t);

  gcc_assert (type != unknown_type_node);

  if (tree d = maybe_dependent_member_ref (t, args, complain, in_decl))
    return d;

  /* Reuse typedefs.  We need to do this to handle dependent attributes,
     such as attribute aligned.  */
  if (TYPE_P (t)
      && typedef_variant_p (t))
    {
      tree decl = TYPE_NAME (t);

      if (alias_template_specialization_p (t, nt_opaque))
	{
	  /* DECL represents an alias template and we want to
	     instantiate it.  */
	  tree tmpl = most_general_template (DECL_TI_TEMPLATE (decl));
	  tree gen_args = tsubst (DECL_TI_ARGS (decl), args, complain, in_decl);
	  r = instantiate_alias_template (tmpl, gen_args, complain);
	}
      else if (DECL_CLASS_SCOPE_P (decl)
	       && CLASSTYPE_TEMPLATE_INFO (DECL_CONTEXT (decl))
	       && uses_template_parms (DECL_CONTEXT (decl)))
	{
	  tree tmpl = most_general_template (DECL_TI_TEMPLATE (decl));
	  tree gen_args = tsubst (DECL_TI_ARGS (decl), args, complain, in_decl);
	  r = retrieve_specialization (tmpl, gen_args, 0);
	}
      else if (DECL_FUNCTION_SCOPE_P (decl)
	       && DECL_TEMPLATE_INFO (DECL_CONTEXT (decl))
	       && uses_template_parms (DECL_TI_ARGS (DECL_CONTEXT (decl))))
	r = retrieve_local_specialization (decl);
      else
	/* The typedef is from a non-template context.  */
	return t;

      if (r)
	{
	  r = TREE_TYPE (r);
	  r = cp_build_qualified_type
	    (r, cp_type_quals (t) | cp_type_quals (r),
	     complain | tf_ignore_bad_quals);
	  return r;
	}
      else
	{
	  /* We don't have an instantiation yet, so drop the typedef.  */
	  int quals = cp_type_quals (t);
	  t = DECL_ORIGINAL_TYPE (decl);
	  t = cp_build_qualified_type (t, quals,
				       complain | tf_ignore_bad_quals);
	}
    }

  bool fndecl_type = (complain & tf_fndecl_type);
  complain &= ~tf_fndecl_type;

  if (type
      && code != TYPENAME_TYPE
      && code != TEMPLATE_TYPE_PARM
      && code != TEMPLATE_PARM_INDEX
      && code != IDENTIFIER_NODE
      && code != FUNCTION_TYPE
      && code != METHOD_TYPE)
    type = tsubst (type, args, complain, in_decl);
  if (type == error_mark_node)
    return error_mark_node;

  switch (code)
    {
    case RECORD_TYPE:
      if (TYPE_PTRMEMFUNC_P (t))
	return tsubst (TYPE_PTRMEMFUNC_FN_TYPE (t), args, complain, in_decl);
      /* Fall through.  */
    case UNION_TYPE:
    case ENUMERAL_TYPE:
      return tsubst_aggr_type_1 (t, args, complain, in_decl,
				 /*entering_scope=*/0);

    case ERROR_MARK:
    case IDENTIFIER_NODE:
    case VOID_TYPE:
    case OPAQUE_TYPE:
    case REAL_TYPE:
    case COMPLEX_TYPE:
    case VECTOR_TYPE:
    case BOOLEAN_TYPE:
    case NULLPTR_TYPE:
    case LANG_TYPE:
      return t;

    case INTEGER_TYPE:
      if (t == integer_type_node)
	return t;

      if (TREE_CODE (TYPE_MIN_VALUE (t)) == INTEGER_CST
          && TREE_CODE (TYPE_MAX_VALUE (t)) == INTEGER_CST)
        return t;

      {
	tree max, omax = TREE_OPERAND (TYPE_MAX_VALUE (t), 0);

	max = tsubst_expr (omax, args, complain, in_decl);

	/* Fix up type of the magic NOP_EXPR with TREE_SIDE_EFFECTS if
	   needed.  */
	if (TREE_CODE (max) == NOP_EXPR
	    && TREE_SIDE_EFFECTS (omax)
	    && !TREE_TYPE (max))
	  TREE_TYPE (max) = TREE_TYPE (TREE_OPERAND (max, 0));

	/* If we're in a partial instantiation, preserve the magic NOP_EXPR
	   with TREE_SIDE_EFFECTS that indicates this is not an integral
	   constant expression.  */
	if (processing_template_decl
	    && TREE_SIDE_EFFECTS (omax) && TREE_CODE (omax) == NOP_EXPR)
	  {
	    gcc_assert (TREE_CODE (max) == NOP_EXPR);
	    TREE_SIDE_EFFECTS (max) = 1;
	  }

	return compute_array_index_type (NULL_TREE, max, complain);
      }

    case TEMPLATE_TYPE_PARM:
      if (template_placeholder_p (t))
	{
	  tree tmpl = CLASS_PLACEHOLDER_TEMPLATE (t);
	  tmpl = tsubst_copy (tmpl, args, complain, in_decl);
	  if (TREE_CODE (tmpl) == TEMPLATE_TEMPLATE_PARM)
	    tmpl = TEMPLATE_TEMPLATE_PARM_TEMPLATE_DECL (tmpl);

	  if (tmpl != CLASS_PLACEHOLDER_TEMPLATE (t))
	    return make_template_placeholder (tmpl);
	  else
	    return t;
	}
      /* Fall through.  */
    case TEMPLATE_TEMPLATE_PARM:
    case BOUND_TEMPLATE_TEMPLATE_PARM:
    case TEMPLATE_PARM_INDEX:
      {
	int idx;
	int level;
	int levels;
	tree arg = NULL_TREE;

	r = NULL_TREE;

	gcc_assert (TREE_VEC_LENGTH (args) > 0);
	template_parm_level_and_index (t, &level, &idx);

	levels = TMPL_ARGS_DEPTH (args);
	if (level <= levels
	    && TREE_VEC_LENGTH (TMPL_ARGS_LEVEL (args, level)) > 0)
	  {
	    arg = TMPL_ARG (args, level, idx);

	    /* See through ARGUMENT_PACK_SELECT arguments. */
	    if (arg && TREE_CODE (arg) == ARGUMENT_PACK_SELECT)
	      arg = argument_pack_select_arg (arg);
	  }

	if (arg == error_mark_node)
	  return error_mark_node;
	else if (arg != NULL_TREE)
	  {
	    if (ARGUMENT_PACK_P (arg))
	      /* If ARG is an argument pack, we don't actually want to
		 perform a substitution here, because substitutions
		 for argument packs are only done
		 element-by-element. We can get to this point when
		 substituting the type of a non-type template
		 parameter pack, when that type actually contains
		 template parameter packs from an outer template, e.g.,

	         template<typename... Types> struct A {
		   template<Types... Values> struct B { };
                 };  */
	      return t;

	    if (code == TEMPLATE_TYPE_PARM)
	      {
		int quals;

		/* When building concept checks for the purpose of
		   deducing placeholders, we can end up with wildcards
		   where types are expected. Adjust this to the deduced
		   value.  */
		if (TREE_CODE (arg) == WILDCARD_DECL)
		  arg = TREE_TYPE (TREE_TYPE (arg));

		gcc_assert (TYPE_P (arg));

		quals = cp_type_quals (arg) | cp_type_quals (t);

		return cp_build_qualified_type
		  (arg, quals, complain | tf_ignore_bad_quals);
	      }
	    else if (code == BOUND_TEMPLATE_TEMPLATE_PARM)
	      {
		/* We are processing a type constructed from a
		   template template parameter.  */
		tree argvec = tsubst (TYPE_TI_ARGS (t),
				      args, complain, in_decl);
		if (argvec == error_mark_node)
		  return error_mark_node;

		gcc_assert (TREE_CODE (arg) == TEMPLATE_TEMPLATE_PARM
			    || TREE_CODE (arg) == TEMPLATE_DECL
			    || TREE_CODE (arg) == UNBOUND_CLASS_TEMPLATE);

		if (TREE_CODE (arg) == UNBOUND_CLASS_TEMPLATE)
		  /* Consider this code:

			template <template <class> class Template>
			struct Internal {
			template <class Arg> using Bind = Template<Arg>;
			};

			template <template <class> class Template, class Arg>
			using Instantiate = Template<Arg>; //#0

			template <template <class> class Template,
                                  class Argument>
			using Bind =
			  Instantiate<Internal<Template>::template Bind,
				      Argument>; //#1

		     When #1 is parsed, the
		     BOUND_TEMPLATE_TEMPLATE_PARM representing the
		     parameter `Template' in #0 matches the
		     UNBOUND_CLASS_TEMPLATE representing the argument
		     `Internal<Template>::template Bind'; We then want
		     to assemble the type `Bind<Argument>' that can't
		     be fully created right now, because
		     `Internal<Template>' not being complete, the Bind
		     template cannot be looked up in that context.  So
		     we need to "store" `Bind<Argument>' for later
		     when the context of Bind becomes complete.  Let's
		     store that in a TYPENAME_TYPE.  */
		  return make_typename_type (TYPE_CONTEXT (arg),
					     build_nt (TEMPLATE_ID_EXPR,
						       TYPE_IDENTIFIER (arg),
						       argvec),
					     typename_type,
					     complain);

		/* We can get a TEMPLATE_TEMPLATE_PARM here when we
		   are resolving nested-types in the signature of a
		   member function templates.  Otherwise ARG is a
		   TEMPLATE_DECL and is the real template to be
		   instantiated.  */
		if (TREE_CODE (arg) == TEMPLATE_TEMPLATE_PARM)
		  arg = TYPE_NAME (arg);

		r = lookup_template_class (arg,
					   argvec, in_decl,
					   DECL_CONTEXT (arg),
					    /*entering_scope=*/0,
					   complain);
		return cp_build_qualified_type
		  (r, cp_type_quals (t) | cp_type_quals (r), complain);
	      }
	    else if (code == TEMPLATE_TEMPLATE_PARM)
	      return arg;
	    else
	      /* TEMPLATE_PARM_INDEX.  */
	      return convert_from_reference (unshare_expr (arg));
	  }

	if (level == 1)
	  /* This can happen during the attempted tsubst'ing in
	     unify.  This means that we don't yet have any information
	     about the template parameter in question.  */
	  return t;

	/* Early in template argument deduction substitution, we don't
	   want to reduce the level of 'auto', or it will be confused
	   with a normal template parm in subsequent deduction.
	   Similarly, don't reduce the level of template parameters to
	   avoid mismatches when deducing their types.  */
	if (complain & tf_partial)
	  return t;

	/* If we get here, we must have been looking at a parm for a
	   more deeply nested template.  Make a new version of this
	   template parameter, but with a lower level.  */
	int quals;
	switch (code)
	  {
	  case TEMPLATE_TYPE_PARM:
	  case TEMPLATE_TEMPLATE_PARM:
	    quals = cp_type_quals (t);
	    if (quals)
	      {
		gcc_checking_assert (code == TEMPLATE_TYPE_PARM);
		t = TYPE_MAIN_VARIANT (t);
	      }

	    if (tree d = TEMPLATE_TYPE_DESCENDANTS (t))
	      if (TEMPLATE_PARM_LEVEL (d) == TEMPLATE_TYPE_LEVEL (t) - levels
		  && (code == TEMPLATE_TYPE_PARM
		      || TEMPLATE_TEMPLATE_PARM_SIMPLE_P (t)))
		/* Cache lowering a type parameter or a simple template
		   template parameter.  */
		r = TREE_TYPE (d);

	    if (!r)
	      {
		r = copy_type (t);
		TEMPLATE_TYPE_PARM_INDEX (r)
		  = reduce_template_parm_level (TEMPLATE_TYPE_PARM_INDEX (t),
						r, levels, args, complain);
		TYPE_STUB_DECL (r) = TYPE_NAME (r) = TEMPLATE_TYPE_DECL (r);
		TYPE_MAIN_VARIANT (r) = r;
		TYPE_POINTER_TO (r) = NULL_TREE;
		TYPE_REFERENCE_TO (r) = NULL_TREE;

		if (code == TEMPLATE_TYPE_PARM)
		  if (tree ci = PLACEHOLDER_TYPE_CONSTRAINTS_INFO (t))
		    /* Propagate constraints on placeholders since they are
		       only instantiated during satisfaction.  */
		    PLACEHOLDER_TYPE_CONSTRAINTS_INFO (r) = ci;

		if (TYPE_STRUCTURAL_EQUALITY_P (t))
		  SET_TYPE_STRUCTURAL_EQUALITY (r);
		else
		  TYPE_CANONICAL (r) = canonical_type_parameter (r);
	      }

	    if (quals)
	      r = cp_build_qualified_type (r, quals,
					   complain | tf_ignore_bad_quals);
	    break;

	  case BOUND_TEMPLATE_TEMPLATE_PARM:
	    {
	      tree tinfo = TYPE_TEMPLATE_INFO (t);
	      /* We might need to substitute into the types of non-type
		 template parameters.  This also lowers the level of
		 the ttp appropriately.  */
	      tree tmpl = tsubst (TI_TEMPLATE (tinfo), args,
				  complain, in_decl);
	      if (tmpl == error_mark_node)
		return error_mark_node;
	      tree argvec = tsubst (TI_ARGS (tinfo), args,
				    complain, in_decl);
	      if (argvec == error_mark_node)
		return error_mark_node;
	      r = lookup_template_class (tmpl, argvec, in_decl, NULL_TREE,
					 /*entering_scope=*/false, complain);
	      r = cp_build_qualified_type (r, cp_type_quals (t), complain);
	      break;
	    }

	  case TEMPLATE_PARM_INDEX:
	    /* OK, now substitute the type of the non-type parameter.  We
	       couldn't do it earlier because it might be an auto parameter,
	       and we wouldn't need to if we had an argument.  */
	    type = tsubst (type, args, complain, in_decl);
	    if (type == error_mark_node)
	      return error_mark_node;
	    r = reduce_template_parm_level (t, type, levels, args, complain);
	    break;

	  default:
	    gcc_unreachable ();
	  }

	return r;
      }

    case TREE_LIST:
      return tsubst_tree_list (t, args, complain, in_decl);

    case TREE_BINFO:
      /* We should never be tsubsting a binfo.  */
      gcc_unreachable ();

    case TREE_VEC:
      /* A vector of template arguments.  */
      gcc_assert (!type);
      return tsubst_template_args (t, args, complain, in_decl);

    case POINTER_TYPE:
    case REFERENCE_TYPE:
      {
	if (type == TREE_TYPE (t) && TREE_CODE (type) != METHOD_TYPE)
	  return t;

	/* [temp.deduct]

	   Type deduction may fail for any of the following
	   reasons:

	   -- Attempting to create a pointer to reference type.
	   -- Attempting to create a reference to a reference type or
	      a reference to void.

	  Core issue 106 says that creating a reference to a reference
	  during instantiation is no longer a cause for failure. We
	  only enforce this check in strict C++98 mode.  */
	if ((TYPE_REF_P (type)
	     && (((cxx_dialect == cxx98) && flag_iso) || code != REFERENCE_TYPE))
	    || (code == REFERENCE_TYPE && VOID_TYPE_P (type)))
	  {
	    static location_t last_loc;

	    /* We keep track of the last time we issued this error
	       message to avoid spewing a ton of messages during a
	       single bad template instantiation.  */
	    if (complain & tf_error
		&& last_loc != input_location)
	      {
		if (VOID_TYPE_P (type))
		  error ("forming reference to void");
               else if (code == POINTER_TYPE)
                 error ("forming pointer to reference type %qT", type);
               else
		  error ("forming reference to reference type %qT", type);
		last_loc = input_location;
	      }

	    return error_mark_node;
	  }
	else if (TREE_CODE (type) == FUNCTION_TYPE
		 && (type_memfn_quals (type) != TYPE_UNQUALIFIED
		     || type_memfn_rqual (type) != REF_QUAL_NONE))
	  {
	    if (complain & tf_error)
	      {
		if (code == POINTER_TYPE)
		  error ("forming pointer to qualified function type %qT",
			 type);
		else
		  error ("forming reference to qualified function type %qT",
			 type);
	      }
	    return error_mark_node;
	  }
	else if (code == POINTER_TYPE)
	  {
	    r = build_pointer_type (type);
	    if (TREE_CODE (type) == METHOD_TYPE)
	      r = build_ptrmemfunc_type (r);
	  }
	else if (TYPE_REF_P (type))
	  /* In C++0x, during template argument substitution, when there is an
	     attempt to create a reference to a reference type, reference
	     collapsing is applied as described in [14.3.1/4 temp.arg.type]:

	     "If a template-argument for a template-parameter T names a type
	     that is a reference to a type A, an attempt to create the type
	     'lvalue reference to cv T' creates the type 'lvalue reference to
	     A,' while an attempt to create the type type rvalue reference to
	     cv T' creates the type T"
	  */
	  r = cp_build_reference_type
	      (TREE_TYPE (type),
	       TYPE_REF_IS_RVALUE (t) && TYPE_REF_IS_RVALUE (type));
	else
	  r = cp_build_reference_type (type, TYPE_REF_IS_RVALUE (t));
	r = cp_build_qualified_type (r, cp_type_quals (t), complain);

	if (r != error_mark_node)
	  /* Will this ever be needed for TYPE_..._TO values?  */
	  layout_type (r);

	return r;
      }
    case OFFSET_TYPE:
      {
	r = tsubst (TYPE_OFFSET_BASETYPE (t), args, complain, in_decl);
	if (r == error_mark_node || !MAYBE_CLASS_TYPE_P (r))
	  {
	    /* [temp.deduct]

	       Type deduction may fail for any of the following
	       reasons:

	       -- Attempting to create "pointer to member of T" when T
		  is not a class type.  */
	    if (complain & tf_error)
	      error ("creating pointer to member of non-class type %qT", r);
	    return error_mark_node;
	  }
	if (TYPE_REF_P (type))
	  {
	    if (complain & tf_error)
	      error ("creating pointer to member reference type %qT", type);
	    return error_mark_node;
	  }
	if (VOID_TYPE_P (type))
	  {
	    if (complain & tf_error)
	      error ("creating pointer to member of type void");
	    return error_mark_node;
	  }
	gcc_assert (TREE_CODE (type) != METHOD_TYPE);
	if (TREE_CODE (type) == FUNCTION_TYPE)
	  {
	    /* The type of the implicit object parameter gets its
	       cv-qualifiers from the FUNCTION_TYPE. */
	    tree memptr;
	    tree method_type
	      = build_memfn_type (type, r, type_memfn_quals (type),
				  type_memfn_rqual (type));
	    memptr = build_ptrmemfunc_type (build_pointer_type (method_type));
	    return cp_build_qualified_type (memptr, cp_type_quals (t),
					    complain);
	  }
	else
	  return cp_build_qualified_type (build_ptrmem_type (r, type),
					  cp_type_quals (t),
					  complain);
      }
    case FUNCTION_TYPE:
    case METHOD_TYPE:
      {
	tree fntype;
	tree specs;
	fntype = tsubst_function_type (t, args, complain, in_decl);
	if (fntype == error_mark_node)
	  return error_mark_node;

	/* Substitute the exception specification.  */
	specs = tsubst_exception_specification (t, args, complain, in_decl,
						/*defer_ok*/fndecl_type);
	if (specs == error_mark_node)
	  return error_mark_node;
	if (specs)
	  fntype = build_exception_variant (fntype, specs);
	return fntype;
      }
    case ARRAY_TYPE:
      {
	tree domain = tsubst (TYPE_DOMAIN (t), args, complain, in_decl);
	if (domain == error_mark_node)
	  return error_mark_node;

	/* As an optimization, we avoid regenerating the array type if
	   it will obviously be the same as T.  */
	if (type == TREE_TYPE (t) && domain == TYPE_DOMAIN (t))
	  return t;

	/* These checks should match the ones in create_array_type_for_decl.

	   [temp.deduct]

	   The deduction may fail for any of the following reasons:

	   -- Attempting to create an array with an element type that
	      is void, a function type, or a reference type, or [DR337]
	      an abstract class type.  */
	if (VOID_TYPE_P (type)
	    || TREE_CODE (type) == FUNCTION_TYPE
	    || (TREE_CODE (type) == ARRAY_TYPE
		&& TYPE_DOMAIN (type) == NULL_TREE)
	    || TYPE_REF_P (type))
	  {
	    if (complain & tf_error)
	      error ("creating array of %qT", type);
	    return error_mark_node;
	  }

	if (!verify_type_context (input_location, TCTX_ARRAY_ELEMENT, type,
				  !(complain & tf_error)))
	  return error_mark_node;

	r = build_cplus_array_type (type, domain);

	if (!valid_array_size_p (input_location, r, in_decl,
				 (complain & tf_error)))
	  return error_mark_node;

	if (TYPE_USER_ALIGN (t))
	  {
	    SET_TYPE_ALIGN (r, TYPE_ALIGN (t));
	    TYPE_USER_ALIGN (r) = 1;
	  }

	return r;
      }

    case TYPENAME_TYPE:
      {
	tree ctx = TYPE_CONTEXT (t);
	if (TREE_CODE (ctx) == TYPE_PACK_EXPANSION)
	  {
	    ctx = tsubst_pack_expansion (ctx, args,
					 complain | tf_qualifying_scope,
					 in_decl);
	    if (ctx == error_mark_node
		|| TREE_VEC_LENGTH (ctx) > 1)
	      return error_mark_node;
	    if (TREE_VEC_LENGTH (ctx) == 0)
	      {
		if (complain & tf_error)
		  error ("%qD is instantiated for an empty pack",
			 TYPENAME_TYPE_FULLNAME (t));
		return error_mark_node;
	      }
	    ctx = TREE_VEC_ELT (ctx, 0);
	  }
	else
	  ctx = tsubst_aggr_type (ctx, args,
				  complain | tf_qualifying_scope,
				  in_decl, /*entering_scope=*/1);
	if (ctx == error_mark_node)
	  return error_mark_node;

	tree f = tsubst_copy (TYPENAME_TYPE_FULLNAME (t), args,
			      complain, in_decl);
	if (f == error_mark_node)
	  return error_mark_node;

	if (!MAYBE_CLASS_TYPE_P (ctx))
	  {
	    if (complain & tf_error)
	      error ("%qT is not a class, struct, or union type", ctx);
	    return error_mark_node;
	  }
	else if (!uses_template_parms (ctx) && !TYPE_BEING_DEFINED (ctx))
	  {
	    /* Normally, make_typename_type does not require that the CTX
	       have complete type in order to allow things like:

		 template <class T> struct S { typename S<T>::X Y; };

	       But, such constructs have already been resolved by this
	       point, so here CTX really should have complete type, unless
	       it's a partial instantiation.  */
	    if (!complete_type_or_maybe_complain (ctx, NULL_TREE, complain))
	      return error_mark_node;
	  }

	/* FIXME: TYPENAME_IS_CLASS_P conflates 'class' vs 'struct' vs 'union'
	   tags.  TYPENAME_TYPE should probably remember the exact tag that
	   was written.  */
	enum tag_types tag_type
	  = TYPENAME_IS_CLASS_P (t) ? class_type
	  : TYPENAME_IS_ENUM_P (t) ? enum_type
	  : typename_type;
	tsubst_flags_t tcomplain = complain | tf_keep_type_decl;
	tcomplain |= tst_ok_flag | qualifying_scope_flag;
	f = make_typename_type (ctx, f, tag_type, tcomplain);
	if (f == error_mark_node)
	  return f;
	if (TREE_CODE (f) == TYPE_DECL)
	  {
	    complain |= tf_ignore_bad_quals;
	    f = TREE_TYPE (f);
	  }

	if (TREE_CODE (f) != TYPENAME_TYPE)
	  {
	    if (TYPENAME_IS_ENUM_P (t) && TREE_CODE (f) != ENUMERAL_TYPE)
	      {
		if (complain & tf_error)
		  error ("%qT resolves to %qT, which is not an enumeration type",
			 t, f);
		else
		  return error_mark_node;
	      }
	    else if (TYPENAME_IS_CLASS_P (t) && !CLASS_TYPE_P (f))
	      {
		if (complain & tf_error)
		  error ("%qT resolves to %qT, which is not a class type",
			 t, f);
		else
		  return error_mark_node;
	      }
	  }

	return cp_build_qualified_type
	  (f, cp_type_quals (f) | cp_type_quals (t), complain);
      }

    case UNBOUND_CLASS_TEMPLATE:
      {
	tree ctx = tsubst_aggr_type (TYPE_CONTEXT (t), args, complain,
				     in_decl, /*entering_scope=*/1);
	tree name = TYPE_IDENTIFIER (t);
	tree parm_list = DECL_TEMPLATE_PARMS (TYPE_NAME (t));

	if (ctx == error_mark_node || name == error_mark_node)
	  return error_mark_node;

	if (parm_list)
	  parm_list = tsubst_template_parms (parm_list, args, complain);
	return make_unbound_class_template (ctx, name, parm_list, complain);
      }

    case TYPEOF_TYPE:
      {
	tree type;

	++cp_unevaluated_operand;
	++c_inhibit_evaluation_warnings;

	type = tsubst_expr (TYPEOF_TYPE_EXPR (t), args, complain, in_decl);

	--cp_unevaluated_operand;
	--c_inhibit_evaluation_warnings;

	type = finish_typeof (type);
	return cp_build_qualified_type (type,
					cp_type_quals (t)
					| cp_type_quals (type),
					complain);
      }

    case DECLTYPE_TYPE:
      {
	tree type;

	++cp_unevaluated_operand;
	++c_inhibit_evaluation_warnings;

	type = tsubst_copy_and_build (DECLTYPE_TYPE_EXPR (t), args,
				      complain|tf_decltype, in_decl);

	--cp_unevaluated_operand;
	--c_inhibit_evaluation_warnings;

	if (DECLTYPE_FOR_LAMBDA_CAPTURE (t))
	  type = lambda_capture_field_type (type,
					    false /*explicit_init*/,
					    DECLTYPE_FOR_REF_CAPTURE (t));
	else if (DECLTYPE_FOR_LAMBDA_PROXY (t))
	  type = lambda_proxy_type (type);
	else
	  {
	    bool id = DECLTYPE_TYPE_ID_EXPR_OR_MEMBER_ACCESS_P (t);
	    if (id && TREE_CODE (DECLTYPE_TYPE_EXPR (t)) == BIT_NOT_EXPR
		&& EXPR_P (type))
	      /* In a template ~id could be either a complement expression
		 or an unqualified-id naming a destructor; if instantiating
		 it produces an expression, it's not an id-expression or
		 member access.  */
	      id = false;
	    type = finish_decltype_type (type, id, complain);
	  }
	return cp_build_qualified_type (type,
					cp_type_quals (t)
					| cp_type_quals (type),
					complain | tf_ignore_bad_quals);
      }

    case TRAIT_TYPE:
      {
	tree type1 = TRAIT_TYPE_TYPE1 (t);
	if (TYPE_P (type1))
	  type1 = tsubst (type1, args, complain, in_decl);
	else
	  type1 = tsubst_copy_and_build (type1, args, complain, in_decl);
	tree type2 = tsubst (TRAIT_TYPE_TYPE2 (t), args, complain, in_decl);
	type = finish_trait_type (TRAIT_TYPE_KIND (t), type1, type2, complain);
	return cp_build_qualified_type (type,
					cp_type_quals (t) | cp_type_quals (type),
					complain | tf_ignore_bad_quals);
      }

    case TYPE_ARGUMENT_PACK:
    case NONTYPE_ARGUMENT_PACK:
      return tsubst_argument_pack (t, args, complain, in_decl);

    case VOID_CST:
    case INTEGER_CST:
    case REAL_CST:
    case STRING_CST:
    case PLUS_EXPR:
    case MINUS_EXPR:
    case NEGATE_EXPR:
    case NOP_EXPR:
    case INDIRECT_REF:
    case ADDR_EXPR:
    case CALL_EXPR:
    case ARRAY_REF:
    case SCOPE_REF:
      /* We should use one of the expression tsubsts for these codes.  */
      gcc_unreachable ();

    default:
      sorry ("use of %qs in template", get_tree_code_name (code));
      return error_mark_node;
    }
}

/* Convenience wrapper over tsubst for substituting into the LHS
   of the :: scope resolution operator.  */

static tree
tsubst_scope (tree t, tree args, tsubst_flags_t complain, tree in_decl)
{
  gcc_checking_assert (TYPE_P (t));
  return tsubst (t, args, complain | tf_qualifying_scope, in_decl);
}

/* OLDFNS is a lookup set of member functions from some class template, and
   NEWFNS is a lookup set of member functions from NEWTYPE, a specialization
   of that class template.  Return the subset of NEWFNS which are
   specializations of a function from OLDFNS.  */

static tree
filter_memfn_lookup (tree oldfns, tree newfns, tree newtype)
{
  /* Record all member functions from the old lookup set OLDFNS into
     VISIBLE_SET.  */
  hash_set<tree> visible_set;
  bool seen_dep_using = false;
  for (tree fn : lkp_range (oldfns))
    {
      if (TREE_CODE (fn) == USING_DECL)
	{
	  /* Imprecisely handle dependent using-decl by keeping all members
	     in the new lookup set that are defined in a base class, i.e.
	     members that could plausibly have been introduced by this
	     dependent using-decl.
	     FIXME: Track which members are introduced by a dependent
	     using-decl precisely, perhaps by performing another lookup
	     from the substituted USING_DECL_SCOPE.  */
	  gcc_checking_assert (DECL_DEPENDENT_P (fn));
	  seen_dep_using = true;
	}
      else
	visible_set.add (fn);
    }

  /* Returns true iff (a less specialized version of) FN appeared in
     the old lookup set OLDFNS.  */
  auto visible_p = [newtype, seen_dep_using, &visible_set] (tree fn) {
    if (DECL_CONTEXT (fn) != newtype)
      /* FN is a member function from a base class, introduced via a
	 using-decl; if it might have been introduced by a dependent
	 using-decl then just conservatively keep it, otherwise look
	 in the old lookup set for FN exactly.  */
      return seen_dep_using || visible_set.contains (fn);
    else if (TREE_CODE (fn) == TEMPLATE_DECL)
      /* FN is a member function template from the current class;
	 look in the old lookup set for the TEMPLATE_DECL from which
	 it was specialized.  */
      return visible_set.contains (DECL_TI_TEMPLATE (fn));
    else
      /* FN is a non-template member function from the current class;
	 look in the old lookup set for the FUNCTION_DECL from which
	 it was specialized.  */
      return visible_set.contains (DECL_TEMPLATE_RESULT
				   (DECL_TI_TEMPLATE (fn)));
  };

  bool lookup_changed_p = false;
  for (tree fn : lkp_range (newfns))
    if (!visible_p (fn))
      {
	lookup_changed_p = true;
	break;
      }
  if (!lookup_changed_p)
    return newfns;

  /* Filter out from NEWFNS the member functions that weren't
     previously visible according to OLDFNS.  */
  tree filtered_fns = NULL_TREE;
  unsigned filtered_size = 0;
  for (tree fn : lkp_range (newfns))
    if (visible_p (fn))
      {
	filtered_fns = lookup_add (fn, filtered_fns);
	filtered_size++;
      }
  gcc_checking_assert (seen_dep_using
		       ? filtered_size >= visible_set.elements ()
		       : filtered_size == visible_set.elements ());

  return filtered_fns;
}

/* tsubst a BASELINK.  OBJECT_TYPE, if non-NULL, is the type of the
   expression on the left-hand side of the "." or "->" operator.  We
   only do the lookup if we had a dependent BASELINK.  Otherwise we
   adjust it onto the instantiated heirarchy.  */

static tree
tsubst_baselink (tree baselink, tree object_type,
		 tree args, tsubst_flags_t complain, tree in_decl)
{
  bool qualified_p = BASELINK_QUALIFIED_P (baselink);
  tree qualifying_scope = BINFO_TYPE (BASELINK_ACCESS_BINFO (baselink));
  qualifying_scope = tsubst (qualifying_scope, args, complain, in_decl);

  tree optype = BASELINK_OPTYPE (baselink);
  optype = tsubst (optype, args, complain, in_decl);

  tree template_args = NULL_TREE;
  bool template_id_p = false;
  tree fns = BASELINK_FUNCTIONS (baselink);
  if (TREE_CODE (fns) == TEMPLATE_ID_EXPR)
    {
      template_id_p = true;
      template_args = TREE_OPERAND (fns, 1);
      fns = TREE_OPERAND (fns, 0);
      if (template_args)
	template_args = tsubst_template_args (template_args, args,
					      complain, in_decl);
    }

  tree binfo_type = BINFO_TYPE (BASELINK_BINFO (baselink));
  binfo_type = tsubst (binfo_type, args, complain, in_decl);
  bool dependent_p = (binfo_type != BINFO_TYPE (BASELINK_BINFO (baselink))
		      || optype != BASELINK_OPTYPE (baselink));

  if (dependent_p)
    {
      tree name = OVL_NAME (fns);
      if (IDENTIFIER_CONV_OP_P (name))
	name = make_conv_op_name (optype);

      /* See maybe_dependent_member_ref.  */
      if ((complain & tf_dguide) && dependent_scope_p (qualifying_scope))
	{
	  if (template_id_p)
	    name = build2 (TEMPLATE_ID_EXPR, unknown_type_node, name,
			   template_args);
	  return build_qualified_name (NULL_TREE, qualifying_scope, name,
				       /* ::template */false);
	}

      if (name == complete_dtor_identifier)
	/* Treat as-if non-dependent below.  */
	dependent_p = false;

      bool maybe_incomplete = BASELINK_FUNCTIONS_MAYBE_INCOMPLETE_P (baselink);
      baselink = lookup_fnfields (qualifying_scope, name, /*protect=*/1,
				  complain);
      if (maybe_incomplete)
	{
	  /* Filter out from the new lookup set those functions which didn't
	     appear in the original lookup set (in a less specialized form).
	     This is needed to preserve the consistency of member lookup
	     performed in an incomplete-class context, within which
	     later-declared members ought to remain invisible.  */
	  BASELINK_FUNCTIONS (baselink)
	    = filter_memfn_lookup (fns, BASELINK_FUNCTIONS (baselink),
				   binfo_type);
	  BASELINK_FUNCTIONS_MAYBE_INCOMPLETE_P (baselink) = true;
	}

      if (!baselink)
	{
	  if ((complain & tf_error)
	      && constructor_name_p (name, qualifying_scope))
	    error ("cannot call constructor %<%T::%D%> directly",
		   qualifying_scope, name);
	  return error_mark_node;
	}

      fns = BASELINK_FUNCTIONS (baselink);
    }
  else
    {
      /* We're going to overwrite pieces below, make a duplicate.  */
      baselink = copy_node (baselink);

      if (qualifying_scope != BINFO_TYPE (BASELINK_ACCESS_BINFO (baselink)))
	{
	  /* The decl we found was from non-dependent scope, but we still need
	     to update the binfos for the instantiated qualifying_scope.  */
	  BASELINK_ACCESS_BINFO (baselink) = TYPE_BINFO (qualifying_scope);
	  BASELINK_BINFO (baselink) = lookup_base (qualifying_scope, binfo_type,
						   ba_unique, nullptr, complain);
	}
    }

  /* If lookup found a single function, mark it as used at this point.
     (If lookup found multiple functions the one selected later by
     overload resolution will be marked as used at that point.)  */
  if (!template_id_p && !really_overloaded_fn (fns))
    {
      tree fn = OVL_FIRST (fns);
      bool ok = mark_used (fn, complain);
      if (!ok && !(complain & tf_error))
	return error_mark_node;
      if (ok && BASELINK_P (baselink))
	/* We might have instantiated an auto function.  */
	TREE_TYPE (baselink) = TREE_TYPE (fn);
    }

  if (BASELINK_P (baselink))
    {
      /* Add back the template arguments, if present.  */
      if (template_id_p)
	BASELINK_FUNCTIONS (baselink)
	  = build2 (TEMPLATE_ID_EXPR, unknown_type_node, fns, template_args);

      /* Update the conversion operator type.  */
      BASELINK_OPTYPE (baselink) = optype;
    }

  if (!object_type)
    object_type = current_class_type;

  if (qualified_p || !dependent_p)
    {
      baselink = adjust_result_of_qualified_name_lookup (baselink,
							 qualifying_scope,
							 object_type);
      if (!qualified_p)
	/* We need to call adjust_result_of_qualified_name_lookup in case the
	   destructor names a base class, but we unset BASELINK_QUALIFIED_P
	   so that we still get virtual function binding.  */
	BASELINK_QUALIFIED_P (baselink) = false;
    }

  return baselink;
}

/* Like tsubst_expr for a SCOPE_REF, given by QUALIFIED_ID.  DONE is
   true if the qualified-id will be a postfix-expression in-and-of
   itself; false if more of the postfix-expression follows the
   QUALIFIED_ID.  ADDRESS_P is true if the qualified-id is the operand
   of "&".  */

static tree
tsubst_qualified_id (tree qualified_id, tree args,
		     tsubst_flags_t complain, tree in_decl,
		     bool done, bool address_p)
{
  tree expr;
  tree scope;
  tree name;
  bool is_template;
  tree template_args;
  location_t loc = EXPR_LOCATION (qualified_id);

  gcc_assert (TREE_CODE (qualified_id) == SCOPE_REF);

  /* Figure out what name to look up.  */
  name = TREE_OPERAND (qualified_id, 1);
  if (TREE_CODE (name) == TEMPLATE_ID_EXPR)
    {
      is_template = true;
      template_args = TREE_OPERAND (name, 1);
      if (template_args)
	template_args = tsubst_template_args (template_args, args,
					      complain, in_decl);
      if (template_args == error_mark_node)
	return error_mark_node;
      name = TREE_OPERAND (name, 0);
    }
  else
    {
      is_template = false;
      template_args = NULL_TREE;
    }

  /* Substitute into the qualifying scope.  When there are no ARGS, we
     are just trying to simplify a non-dependent expression.  In that
     case the qualifying scope may be dependent, and, in any case,
     substituting will not help.  */
  scope = TREE_OPERAND (qualified_id, 0);
  if (args)
    {
      scope = tsubst_scope (scope, args, complain, in_decl);
      expr = tsubst_copy (name, args, complain, in_decl);
    }
  else
    expr = name;

  if (dependent_scope_p (scope))
    {
      if (TREE_CODE (expr) == SCOPE_REF)
	/* We built one in tsubst_baselink.  */
	gcc_checking_assert (same_type_p (scope, TREE_OPERAND (expr, 0)));
      else
	{
	  if (is_template)
	    expr = build_min_nt_loc (loc, TEMPLATE_ID_EXPR, expr,
				     template_args);
	  expr = build_qualified_name (NULL_TREE, scope, expr,
				       QUALIFIED_NAME_IS_TEMPLATE
				       (qualified_id));
	}
      REF_PARENTHESIZED_P (expr) = REF_PARENTHESIZED_P (qualified_id);
      return expr;
    }

  if (!BASELINK_P (name) && !DECL_P (expr))
    {
      if (TREE_CODE (expr) == BIT_NOT_EXPR)
	{
	  /* A BIT_NOT_EXPR is used to represent a destructor.  */
	  if (!check_dtor_name (scope, TREE_OPERAND (expr, 0)))
	    {
	      error ("qualifying type %qT does not match destructor name ~%qT",
		     scope, TREE_OPERAND (expr, 0));
	      expr = error_mark_node;
	    }
	  else
	    expr = lookup_qualified_name (scope, complete_dtor_identifier,
					  LOOK_want::NORMAL, false);
	}
      else
	expr = lookup_qualified_name (scope, expr, LOOK_want::NORMAL, false);
      if (TREE_CODE (TREE_CODE (expr) == TEMPLATE_DECL
		     ? DECL_TEMPLATE_RESULT (expr) : expr) == TYPE_DECL)
	{
	  if (complain & tf_error)
	    {
	      error ("dependent-name %qE is parsed as a non-type, but "
		     "instantiation yields a type", qualified_id);
	      inform (input_location, "say %<typename %E%> if a type is meant", qualified_id);
	    }
	  return error_mark_node;
	}
    }

  if (DECL_P (expr))
    {
      if (!check_accessibility_of_qualified_id (expr, /*object_type=*/NULL_TREE,
						scope, complain))
	return error_mark_node;
      /* Remember that there was a reference to this entity.  */
      if (!mark_used (expr, complain) && !(complain & tf_error))
	return error_mark_node;
    }

  if (expr == error_mark_node || TREE_CODE (expr) == TREE_LIST)
    {
      if (complain & tf_error)
	qualified_name_lookup_error (scope,
				     TREE_OPERAND (qualified_id, 1),
				     expr, input_location);
      return error_mark_node;
    }

  if (is_template)
    {
      /* We may be repeating a check already done during parsing, but
	 if it was well-formed and passed then, it will pass again
	 now, and if it didn't, we wouldn't have got here.  The case
	 we want to catch is when we couldn't tell then, and can now,
	 namely when templ prior to substitution was an
	 identifier.  */
      if (flag_concepts && check_auto_in_tmpl_args (expr, template_args))
	return error_mark_node;

      if (variable_template_p (expr))
	expr = lookup_and_finish_template_variable (expr, template_args,
						    complain);
      else
	expr = lookup_template_function (expr, template_args);
    }

  if (expr == error_mark_node && complain & tf_error)
    qualified_name_lookup_error (scope, TREE_OPERAND (qualified_id, 1),
				 expr, input_location);
  else if (TYPE_P (scope))
    {
      expr = (adjust_result_of_qualified_name_lookup
	      (expr, scope, current_nonlambda_class_type ()));
      expr = (finish_qualified_id_expr
	      (scope, expr, done, address_p && PTRMEM_OK_P (qualified_id),
	       QUALIFIED_NAME_IS_TEMPLATE (qualified_id),
	       /*template_arg_p=*/false, complain));
    }

  /* Expressions do not generally have reference type.  */
  if (TREE_CODE (expr) != SCOPE_REF
      /* However, if we're about to form a pointer-to-member, we just
	 want the referenced member referenced.  */
      && TREE_CODE (expr) != OFFSET_REF)
    expr = convert_from_reference (expr);

  if (REF_PARENTHESIZED_P (qualified_id))
    expr = force_paren_expr (expr);

  expr = maybe_wrap_with_location (expr, loc);

  return expr;
}

/* tsubst the initializer for a VAR_DECL.  INIT is the unsubstituted
   initializer, DECL is the substituted VAR_DECL.  Other arguments are as
   for tsubst.  */

static tree
tsubst_init (tree init, tree decl, tree args,
	     tsubst_flags_t complain, tree in_decl)
{
  if (!init)
    return NULL_TREE;

  init = tsubst_expr (init, args, complain, in_decl);

  tree type = TREE_TYPE (decl);

  if (!init && type != error_mark_node)
    {
      if (tree auto_node = type_uses_auto (type))
	{
	  if (!CLASS_PLACEHOLDER_TEMPLATE (auto_node))
	    {
	      if (complain & tf_error)
		error ("initializer for %q#D expands to an empty list "
		       "of expressions", decl);
	      return error_mark_node;
	    }
	}
      else if (!dependent_type_p (type))
	{
	  /* If we had an initializer but it
	     instantiated to nothing,
	     value-initialize the object.  This will
	     only occur when the initializer was a
	     pack expansion where the parameter packs
	     used in that expansion were of length
	     zero.  */
	  init = build_value_init (type, complain);
	  if (TREE_CODE (init) == AGGR_INIT_EXPR)
	    init = get_target_expr (init, complain);
	  if (TREE_CODE (init) == TARGET_EXPR)
	    TARGET_EXPR_DIRECT_INIT_P (init) = true;
	}
    }

  return init;
}

/* If T is a reference to a dependent member of the current instantiation C and
   we are trying to refer to that member in a partial instantiation of C,
   return a SCOPE_REF; otherwise, return NULL_TREE.

   This can happen when forming a C++17 deduction guide, as in PR96199.  */

static tree
maybe_dependent_member_ref (tree t, tree args, tsubst_flags_t complain,
			    tree in_decl)
{
  if (!(complain & tf_dguide))
    return NULL_TREE;

  tree decl = (t && TYPE_P (t)) ? TYPE_NAME (t) : t;
  if (!decl || !DECL_P (decl))
    return NULL_TREE;

  tree ctx = context_for_name_lookup (decl);
  if (!CLASS_TYPE_P (ctx))
    return NULL_TREE;

  ctx = tsubst (ctx, args, complain, in_decl);
  if (!dependent_scope_p (ctx))
    return NULL_TREE;

  if (TYPE_P (t))
    {
      if (typedef_variant_p (t))
	t = strip_typedefs (t);
      tree decl = TYPE_NAME (t);
      if (decl)
	decl = maybe_dependent_member_ref (decl, args, complain, in_decl);
      if (!decl)
	return NULL_TREE;
      return cp_build_qualified_type (TREE_TYPE (decl), cp_type_quals (t),
				      complain);
    }

  tree name = DECL_NAME (t);
  tree fullname = name;
  if (instantiates_primary_template_p (t))
    {
      tree tinfo = get_template_info (t);
      name = DECL_NAME (TI_TEMPLATE (tinfo));
      tree targs = INNERMOST_TEMPLATE_ARGS (TI_ARGS (tinfo));
      targs = tsubst_template_args (targs, args, complain, in_decl);
      fullname = build_nt (TEMPLATE_ID_EXPR, name, targs);
    }

  if (TREE_CODE (t) == TYPE_DECL)
    {
      if (TREE_CODE (TREE_TYPE (t)) == TYPENAME_TYPE
	  && TYPE_NAME (TREE_TYPE (t)) == t)
	/* The TYPE_DECL for a typename has DECL_CONTEXT of the typename
	   scope, but it doesn't need to be rewritten again.  */
	return NULL_TREE;
      tree type = build_typename_type (ctx, name, fullname, typename_type);
      return TYPE_NAME (type);
    }
  else if (DECL_TYPE_TEMPLATE_P (t))
    return make_unbound_class_template (ctx, name,
					NULL_TREE, complain);
  else
    return build_qualified_name (NULL_TREE, ctx, fullname,
				 TREE_CODE (t) == TEMPLATE_DECL);
}

/* Like tsubst, but deals with expressions.  This function just replaces
   template parms; to finish processing the resultant expression, use
   tsubst_copy_and_build or tsubst_expr.  */

static tree
tsubst_copy (tree t, tree args, tsubst_flags_t complain, tree in_decl)
{
  enum tree_code code;
  tree r;

  if (t == NULL_TREE || t == error_mark_node || args == NULL_TREE)
    return t;

  if (TYPE_P (t))
    return tsubst (t, args, complain, in_decl);

  if (tree d = maybe_dependent_member_ref (t, args, complain, in_decl))
    return d;

  code = TREE_CODE (t);

  switch (code)
    {
    case PARM_DECL:
      r = retrieve_local_specialization (t);

      if (r == NULL_TREE)
	{
	  /* We get here for a use of 'this' in an NSDMI.  */
	  if (DECL_NAME (t) == this_identifier && current_class_ptr)
	    return current_class_ptr;

	  /* This can happen for a parameter name used later in a function
	     declaration (such as in a late-specified return type).  Just
	     make a dummy decl, since it's only used for its type.  */
	  gcc_assert (cp_unevaluated_operand);
	  r = tsubst_decl (t, args, complain);
	  /* Give it the template pattern as its context; its true context
	     hasn't been instantiated yet and this is good enough for
	     mangling.  */
	  DECL_CONTEXT (r) = DECL_CONTEXT (t);
	}

      if (TREE_CODE (r) == ARGUMENT_PACK_SELECT)
	r = argument_pack_select_arg (r);
      if (!mark_used (r, complain) && !(complain & tf_error))
	return error_mark_node;
      return r;

    case CONST_DECL:
      {
	tree enum_type;
	tree v;

	if (DECL_TEMPLATE_PARM_P (t))
	  return tsubst_copy (DECL_INITIAL (t), args, complain, in_decl);
	if (!uses_template_parms (DECL_CONTEXT (t)))
	  return t;

	/* Unfortunately, we cannot just call lookup_name here.
	   Consider:

	     template <int I> int f() {
	     enum E { a = I };
	     struct S { void g() { E e = a; } };
	     };

	   When we instantiate f<7>::S::g(), say, lookup_name is not
	   clever enough to find f<7>::a.  */
	enum_type
	  = tsubst_aggr_type (DECL_CONTEXT (t), args, complain, in_decl,
			      /*entering_scope=*/0);

	for (v = TYPE_VALUES (enum_type);
	     v != NULL_TREE;
	     v = TREE_CHAIN (v))
	  if (TREE_PURPOSE (v) == DECL_NAME (t))
	    return TREE_VALUE (v);

	  /* We didn't find the name.  That should never happen; if
	     name-lookup found it during preliminary parsing, we
	     should find it again here during instantiation.  */
	gcc_unreachable ();
      }
      return t;

    case FIELD_DECL:
      if (DECL_CONTEXT (t))
	{
	  tree ctx;

	  ctx = tsubst_aggr_type (DECL_CONTEXT (t), args, complain, in_decl,
				  /*entering_scope=*/1);
	  if (ctx != DECL_CONTEXT (t))
	    {
	      tree r = lookup_field (ctx, DECL_NAME (t), 0, false);
	      if (!r)
		{
		  if (complain & tf_error)
		    error ("using invalid field %qD", t);
		  return error_mark_node;
		}
	      return r;
	    }
	}

      return t;

    case VAR_DECL:
    case FUNCTION_DECL:
      if (DECL_LANG_SPECIFIC (t) && DECL_TEMPLATE_INFO (t))
	r = tsubst (t, args, complain, in_decl);
      else if (DECL_LOCAL_DECL_P (t))
	{
	  /* Local specialization will usually have been created when
	     we instantiated the DECL_EXPR_DECL. */
	  r = retrieve_local_specialization (t);
	  if (!r)
	    {
	      /* We're in a generic lambda referencing a local extern
		 from an outer block-scope of a non-template.  */
	      gcc_checking_assert (LAMBDA_FUNCTION_P (current_function_decl));
	      r = t;
	    }
	}
      else if (local_variable_p (t)
	       && uses_template_parms (DECL_CONTEXT (t)))
	{
	  r = retrieve_local_specialization (t);
	  if (r == NULL_TREE)
	    {
	      /* First try name lookup to find the instantiation.  */
	      r = lookup_name (DECL_NAME (t));
	      if (r)
		{
		  if (!VAR_P (r))
		    {
		      /* During error-recovery we may find a non-variable,
			 even an OVERLOAD: just bail out and avoid ICEs and
			 duplicate diagnostics (c++/62207).  */
		      gcc_assert (seen_error ());
		      return error_mark_node;
		    }
		  if (!is_capture_proxy (r))
		    {
		      /* Make sure the one we found is the one we want.  */
		      tree ctx = enclosing_instantiation_of (DECL_CONTEXT (t));
		      if (ctx != DECL_CONTEXT (r))
			r = NULL_TREE;
		    }
		}

	      if (r)
		/* OK */;
	      else
		{
		  /* This can happen for a variable used in a
		     late-specified return type of a local lambda, or for a
		     local static or constant.  Building a new VAR_DECL
		     should be OK in all those cases.  */
		  r = tsubst_decl (t, args, complain);
		  if (local_specializations)
		    /* Avoid infinite recursion (79640).  */
		    register_local_specialization (r, t);
		  if (decl_maybe_constant_var_p (r))
		    {
		      /* We can't call cp_finish_decl, so handle the
			 initializer by hand.  */
		      tree init = tsubst_init (DECL_INITIAL (t), r, args,
					       complain, in_decl);
		      if (!processing_template_decl)
			init = maybe_constant_init (init);
		      if (processing_template_decl
			  ? potential_constant_expression (init)
			  : reduced_constant_expression_p (init))
			DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (r)
			  = TREE_CONSTANT (r) = true;
		      DECL_INITIAL (r) = init;
		      if (tree auto_node = type_uses_auto (TREE_TYPE (r)))
			TREE_TYPE (r)
			  = do_auto_deduction (TREE_TYPE (r), init, auto_node,
					       complain, adc_variable_type);
		    }
		  gcc_assert (cp_unevaluated_operand
			      || processing_contract_condition
			      || TREE_STATIC (r)
			      || decl_constant_var_p (r)
			      || seen_error ());
		  if (!processing_template_decl
		      && !TREE_STATIC (r))
		    r = process_outer_var_ref (r, complain);
		}
	      /* Remember this for subsequent uses.  */
	      if (local_specializations)
		register_local_specialization (r, t);
	    }
	  if (TREE_CODE (r) == ARGUMENT_PACK_SELECT)
	    r = argument_pack_select_arg (r);
	}
      else
	r = t;
      if (!mark_used (r, complain))
	return error_mark_node;
      return r;

    case NAMESPACE_DECL:
      return t;

    case OVERLOAD:
      return t;

    case BASELINK:
      return tsubst_baselink (t, current_nonlambda_class_type (),
			      args, complain, in_decl);

    case TEMPLATE_DECL:
      if (DECL_TEMPLATE_TEMPLATE_PARM_P (t))
	return tsubst (TREE_TYPE (DECL_TEMPLATE_RESULT (t)),
		       args, complain, in_decl);
      else if (DECL_FUNCTION_TEMPLATE_P (t) && DECL_MEMBER_TEMPLATE_P (t))
	return tsubst (t, args, complain, in_decl);
      else if (DECL_CLASS_SCOPE_P (t)
	       && uses_template_parms (DECL_CONTEXT (t)))
	{
	  /* Template template argument like the following example need
	     special treatment:

	       template <template <class> class TT> struct C {};
	       template <class T> struct D {
		 template <class U> struct E {};
		 C<E> c;				// #1
	       };
	       D<int> d;				// #2

	     We are processing the template argument `E' in #1 for
	     the template instantiation #2.  Originally, `E' is a
	     TEMPLATE_DECL with `D<T>' as its DECL_CONTEXT.  Now we
	     have to substitute this with one having context `D<int>'.  */

	  tree context = tsubst_aggr_type (DECL_CONTEXT (t), args, complain,
					   in_decl, /*entering_scope=*/true);
	  return lookup_field (context, DECL_NAME(t), 0, false);
	}
      else
	/* Ordinary template template argument.  */
	return t;

    case NON_LVALUE_EXPR:
    case VIEW_CONVERT_EXPR:
	{
	  /* Handle location wrappers by substituting the wrapped node
	     first, *then* reusing the resulting type.  Doing the type
	     first ensures that we handle template parameters and
	     parameter pack expansions.  */
	  if (location_wrapper_p (t))
	    {
	      tree op0 = tsubst_copy (TREE_OPERAND (t, 0), args,
				      complain, in_decl);
	      return maybe_wrap_with_location (op0, EXPR_LOCATION (t));
	    }
	  tree op = TREE_OPERAND (t, 0);
	  /* force_paren_expr can also create a VIEW_CONVERT_EXPR.  */
	  if (code == VIEW_CONVERT_EXPR && REF_PARENTHESIZED_P (t))
	    {
	      op = tsubst_copy (op, args, complain, in_decl);
	      op = build1 (code, TREE_TYPE (op), op);
	      REF_PARENTHESIZED_P (op) = true;
	      return op;
	    }
	  /* We shouldn't see any other uses of these in templates
	     (tsubst_copy_and_build handles C++20 tparm object wrappers).  */
	  gcc_unreachable ();
	}

    case CAST_EXPR:
    case REINTERPRET_CAST_EXPR:
    case CONST_CAST_EXPR:
    case STATIC_CAST_EXPR:
    case DYNAMIC_CAST_EXPR:
    case IMPLICIT_CONV_EXPR:
    CASE_CONVERT:
      {
	tsubst_flags_t tcomplain = complain;
	if (code == CAST_EXPR)
	  tcomplain |= tf_tst_ok;
	tree type = tsubst (TREE_TYPE (t), args, tcomplain, in_decl);
	tree op0 = tsubst_copy (TREE_OPERAND (t, 0), args, complain, in_decl);
	return build1 (code, type, op0);
      }

    case BIT_CAST_EXPR:
      {
	tree type = tsubst (TREE_TYPE (t), args, complain, in_decl);
	tree op0 = tsubst_copy (TREE_OPERAND (t, 0), args, complain, in_decl);
	r = build_min (BIT_CAST_EXPR, type, op0);
	SET_EXPR_LOCATION (r, EXPR_LOCATION (t));
	return r;
      }

    case SIZEOF_EXPR:
      if (PACK_EXPANSION_P (TREE_OPERAND (t, 0))
	  || ARGUMENT_PACK_P (TREE_OPERAND (t, 0)))
        {
          tree expanded, op = TREE_OPERAND (t, 0);
	  int len = 0;

	  if (SIZEOF_EXPR_TYPE_P (t))
	    op = TREE_TYPE (op);

	  ++cp_unevaluated_operand;
	  ++c_inhibit_evaluation_warnings;
	  /* We only want to compute the number of arguments.  */
	  if (PACK_EXPANSION_P (op))
	    expanded = tsubst_pack_expansion (op, args, complain, in_decl);
	  else
	    expanded = tsubst_template_args (ARGUMENT_PACK_ARGS (op),
					     args, complain, in_decl);
	  --cp_unevaluated_operand;
	  --c_inhibit_evaluation_warnings;

	  if (TREE_CODE (expanded) == TREE_VEC)
	    {
	      len = TREE_VEC_LENGTH (expanded);
	      /* Set TREE_USED for the benefit of -Wunused.  */
	      for (int i = 0; i < len; i++)
		if (DECL_P (TREE_VEC_ELT (expanded, i)))
		  TREE_USED (TREE_VEC_ELT (expanded, i)) = true;
	    }

	  if (expanded == error_mark_node)
	    return error_mark_node;
	  else if (PACK_EXPANSION_P (expanded)
		   || (TREE_CODE (expanded) == TREE_VEC
		       && pack_expansion_args_count (expanded)))

	    {
	      if (PACK_EXPANSION_P (expanded))
		/* OK.  */;
	      else if (TREE_VEC_LENGTH (expanded) == 1)
		expanded = TREE_VEC_ELT (expanded, 0);
	      else
		expanded = make_argument_pack (expanded);

	      if (TYPE_P (expanded))
		return cxx_sizeof_or_alignof_type (input_location,
						   expanded, SIZEOF_EXPR,
						   false,
						   complain & tf_error);
	      else
		return cxx_sizeof_or_alignof_expr (input_location,
						   expanded, SIZEOF_EXPR,
						   false,
                                                   complain & tf_error);
	    }
	  else
	    return build_int_cst (size_type_node, len);
        }
      if (SIZEOF_EXPR_TYPE_P (t))
	{
	  r = tsubst (TREE_TYPE (TREE_OPERAND (t, 0)),
		      args, complain, in_decl);
	  r = build1 (NOP_EXPR, r, error_mark_node);
	  r = build1 (SIZEOF_EXPR,
		      tsubst (TREE_TYPE (t), args, complain, in_decl), r);
	  SIZEOF_EXPR_TYPE_P (r) = 1;
	  return r;
	}
      /* Fall through */

    case INDIRECT_REF:
    case NEGATE_EXPR:
    case TRUTH_NOT_EXPR:
    case BIT_NOT_EXPR:
    case ADDR_EXPR:
    case UNARY_PLUS_EXPR:      /* Unary + */
    case ALIGNOF_EXPR:
    case AT_ENCODE_EXPR:
    case ARROW_EXPR:
    case THROW_EXPR:
    case TYPEID_EXPR:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case PAREN_EXPR:
      {
	tree type = tsubst (TREE_TYPE (t), args, complain, in_decl);
	tree op0 = tsubst_copy (TREE_OPERAND (t, 0), args, complain, in_decl);
	r = build1_loc (EXPR_LOCATION (t), code, type, op0);
	if (code == ALIGNOF_EXPR)
	  ALIGNOF_EXPR_STD_P (r) = ALIGNOF_EXPR_STD_P (t);
	/* For addresses of immediate functions ensure we have EXPR_LOCATION
	   set for possible later diagnostics.  */
	if (code == ADDR_EXPR
	    && EXPR_LOCATION (r) == UNKNOWN_LOCATION
	    && TREE_CODE (op0) == FUNCTION_DECL
	    && DECL_IMMEDIATE_FUNCTION_P (op0))
	  SET_EXPR_LOCATION (r, input_location);
	return r;
      }

    case EXCESS_PRECISION_EXPR:
      {
	tree type = tsubst (TREE_TYPE (t), args, complain, in_decl);
	tree op0 = tsubst_copy (TREE_OPERAND (t, 0), args, complain, in_decl);
	if (TREE_CODE (op0) == EXCESS_PRECISION_EXPR)
	  {
	    gcc_checking_assert (same_type_p (type, TREE_TYPE (op0)));
	    return op0;
	  }
	return build1_loc (EXPR_LOCATION (t), code, type, op0);
      }

    case COMPONENT_REF:
      {
	tree object;
	tree name;

	object = tsubst_copy (TREE_OPERAND (t, 0), args, complain, in_decl);
	name = TREE_OPERAND (t, 1);
	if (TREE_CODE (name) == BIT_NOT_EXPR)
	  {
	    name = tsubst_copy (TREE_OPERAND (name, 0), args,
				complain, in_decl);
	    name = build1 (BIT_NOT_EXPR, NULL_TREE, name);
	  }
	else if (TREE_CODE (name) == SCOPE_REF
		 && TREE_CODE (TREE_OPERAND (name, 1)) == BIT_NOT_EXPR)
	  {
	    tree base = tsubst_copy (TREE_OPERAND (name, 0), args,
				     complain, in_decl);
	    name = TREE_OPERAND (name, 1);
	    name = tsubst_copy (TREE_OPERAND (name, 0), args,
				complain, in_decl);
	    name = build1 (BIT_NOT_EXPR, NULL_TREE, name);
	    name = build_qualified_name (/*type=*/NULL_TREE,
					 base, name,
					 /*template_p=*/false);
	  }
	else if (BASELINK_P (name))
	  name = tsubst_baselink (name,
				  non_reference (TREE_TYPE (object)),
				  args, complain,
				  in_decl);
	else
	  name = tsubst_copy (name, args, complain, in_decl);
	return build_nt (COMPONENT_REF, object, name, NULL_TREE);
      }

    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case EXACT_DIV_EXPR:
    case BIT_AND_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case TRUNC_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case RSHIFT_EXPR:
    case LSHIFT_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case MAX_EXPR:
    case MIN_EXPR:
    case LE_EXPR:
    case GE_EXPR:
    case LT_EXPR:
    case GT_EXPR:
    case COMPOUND_EXPR:
    case DOTSTAR_EXPR:
    case MEMBER_REF:
    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
      {
	tree op0 = tsubst_copy (TREE_OPERAND (t, 0), args, complain, in_decl);
	tree op1 = tsubst_copy (TREE_OPERAND (t, 1), args, complain, in_decl);
	return build_nt (code, op0, op1);
      }

    case SCOPE_REF:
      {
	tree op0 = tsubst_scope (TREE_OPERAND (t, 0), args, complain, in_decl);
	tree op1 = tsubst_copy (TREE_OPERAND (t, 1), args, complain, in_decl);
	return build_qualified_name (/*type=*/NULL_TREE, op0, op1,
				     QUALIFIED_NAME_IS_TEMPLATE (t));
      }

    case ARRAY_REF:
      {
	tree op0 = tsubst_copy (TREE_OPERAND (t, 0), args, complain, in_decl);
	tree op1 = tsubst_copy (TREE_OPERAND (t, 1), args, complain, in_decl);
	return build_nt (ARRAY_REF, op0, op1, NULL_TREE, NULL_TREE);
      }

    case CALL_EXPR:
      {
	int n = VL_EXP_OPERAND_LENGTH (t);
	tree result = build_vl_exp (CALL_EXPR, n);
	int i;
	for (i = 0; i < n; i++)
	  TREE_OPERAND (t, i) = tsubst_copy (TREE_OPERAND (t, i), args,
					     complain, in_decl);
	return result;
      }

    case COND_EXPR:
    case MODOP_EXPR:
    case PSEUDO_DTOR_EXPR:
    case VEC_PERM_EXPR:
      {
	tree op0 = tsubst_copy (TREE_OPERAND (t, 0), args, complain, in_decl);
	tree op1 = tsubst_copy (TREE_OPERAND (t, 1), args, complain, in_decl);
	tree op2 = tsubst_copy (TREE_OPERAND (t, 2), args, complain, in_decl);
	r = build_nt (code, op0, op1, op2);
	copy_warning (r, t);
	return r;
      }

    case NEW_EXPR:
      {
	tree op0 = tsubst_copy (TREE_OPERAND (t, 0), args, complain, in_decl);
	tree op1 = tsubst_copy (TREE_OPERAND (t, 1), args, complain, in_decl);
	tree op2 = tsubst_copy (TREE_OPERAND (t, 2), args, complain, in_decl);
	r = build_nt (code, op0, op1, op2);
	NEW_EXPR_USE_GLOBAL (r) = NEW_EXPR_USE_GLOBAL (t);
	return r;
      }

    case DELETE_EXPR:
      {
	tree op0 = tsubst_copy (TREE_OPERAND (t, 0), args, complain, in_decl);
	tree op1 = tsubst_copy (TREE_OPERAND (t, 1), args, complain, in_decl);
	r = build_nt (code, op0, op1);
	DELETE_EXPR_USE_GLOBAL (r) = DELETE_EXPR_USE_GLOBAL (t);
	DELETE_EXPR_USE_VEC (r) = DELETE_EXPR_USE_VEC (t);
	return r;
      }

    case TEMPLATE_ID_EXPR:
      {
	/* Substituted template arguments */
	tree tmpl = TREE_OPERAND (t, 0);
	tree targs = TREE_OPERAND (t, 1);

	tmpl = tsubst_copy (tmpl, args, complain, in_decl);
	if (targs)
	  targs = tsubst_template_args (targs, args, complain, in_decl);

	if (variable_template_p (tmpl))
	  return lookup_template_variable (tmpl, targs, complain);
	else
	  return lookup_template_function (tmpl, targs);
      }

    case TREE_LIST:
      {
	tree purpose, value, chain;

	if (t == void_list_node)
	  return t;

	purpose = TREE_PURPOSE (t);
	if (purpose)
	  purpose = tsubst_copy (purpose, args, complain, in_decl);
	value = TREE_VALUE (t);
	if (value)
	  value = tsubst_copy (value, args, complain, in_decl);
	chain = TREE_CHAIN (t);
	if (chain && chain != void_type_node)
	  chain = tsubst_copy (chain, args, complain, in_decl);
	if (purpose == TREE_PURPOSE (t)
	    && value == TREE_VALUE (t)
	    && chain == TREE_CHAIN (t))
	  return t;
	return tree_cons (purpose, value, chain);
      }

    case TEMPLATE_PARM_INDEX:
    case TYPE_DECL:
      return tsubst (t, args, complain, in_decl);

    case USING_DECL:
      t = DECL_NAME (t);
      /* Fall through.  */
    case IDENTIFIER_NODE:
      if (IDENTIFIER_CONV_OP_P (t))
	{
	  tree new_type = tsubst (TREE_TYPE (t), args, complain, in_decl);
	  return make_conv_op_name (new_type);
	}
      else
	return t;

    case CONSTRUCTOR:
      /* This is handled by tsubst_copy_and_build.  */
      gcc_unreachable ();

    case VA_ARG_EXPR:
      {
	tree op0 = tsubst_copy (TREE_OPERAND (t, 0), args, complain, in_decl);
	tree type = tsubst (TREE_TYPE (t), args, complain, in_decl);
	return build_x_va_arg (EXPR_LOCATION (t), op0, type);
      }

    case CLEANUP_POINT_EXPR:
      /* We shouldn't have built any of these during initial template
	 generation.  Instead, they should be built during instantiation
	 in response to the saved STMT_IS_FULL_EXPR_P setting.  */
      gcc_unreachable ();

    case OFFSET_REF:
      {
	tree type = tsubst (TREE_TYPE (t), args, complain, in_decl);
	tree op0 = tsubst_copy (TREE_OPERAND (t, 0), args, complain, in_decl);
	tree op1 = tsubst_copy (TREE_OPERAND (t, 1), args, complain, in_decl);
	r = build2 (code, type, op0, op1);
	PTRMEM_OK_P (r) = PTRMEM_OK_P (t);
	if (!mark_used (TREE_OPERAND (r, 1), complain)
	    && !(complain & tf_error))
	  return error_mark_node;
	return r;
      }

    case EXPR_PACK_EXPANSION:
      error ("invalid use of pack expansion expression");
      return error_mark_node;

    case NONTYPE_ARGUMENT_PACK:
      error ("use %<...%> to expand argument pack");
      return error_mark_node;

    case VOID_CST:
      gcc_checking_assert (t == void_node && VOID_TYPE_P (TREE_TYPE (t)));
      return t;

    case INTEGER_CST:
    case REAL_CST:
    case COMPLEX_CST:
    case VECTOR_CST:
      {
	/* Instantiate any typedefs in the type.  */
	tree type = tsubst (TREE_TYPE (t), args, complain, in_decl);
	r = fold_convert (type, t);
	gcc_assert (TREE_CODE (r) == code);
	return r;
      }

    case STRING_CST:
      {
	tree type = tsubst (TREE_TYPE (t), args, complain, in_decl);
	r = t;
	if (type != TREE_TYPE (t))
	  {
	    r = copy_node (t);
	    TREE_TYPE (r) = type;
	  }
	return r;
      }

    case PTRMEM_CST:
      /* These can sometimes show up in a partial instantiation, but never
	 involve template parms.  */
      gcc_assert (!uses_template_parms (t));
      return t;

    case UNARY_LEFT_FOLD_EXPR:
      return tsubst_unary_left_fold (t, args, complain, in_decl);
    case UNARY_RIGHT_FOLD_EXPR:
      return tsubst_unary_right_fold (t, args, complain, in_decl);
    case BINARY_LEFT_FOLD_EXPR:
      return tsubst_binary_left_fold (t, args, complain, in_decl);
    case BINARY_RIGHT_FOLD_EXPR:
      return tsubst_binary_right_fold (t, args, complain, in_decl);
    case PREDICT_EXPR:
      return t;

    case DEBUG_BEGIN_STMT:
      /* ??? There's no point in copying it for now, but maybe some
	 day it will contain more information, such as a pointer back
	 to the containing function, inlined copy or so.  */
      return t;

    case CO_AWAIT_EXPR:
      return tsubst_expr (t, args, complain, in_decl);

    default:
      /* We shouldn't get here, but keep going if !flag_checking.  */
      if (flag_checking)
	gcc_unreachable ();
      return t;
    }
}

/* Helper function for tsubst_omp_clauses, used for instantiation of
   OMP_CLAUSE_DECL of clauses.  */

static tree
tsubst_omp_clause_decl (tree decl, tree args, tsubst_flags_t complain,
			tree in_decl, tree *iterator_cache)
{
  if (decl == NULL_TREE || decl == ridpointers[RID_OMP_ALL_MEMORY])
    return decl;

  /* Handle OpenMP iterators.  */
  if (TREE_CODE (decl) == TREE_LIST
      && TREE_PURPOSE (decl)
      && TREE_CODE (TREE_PURPOSE (decl)) == TREE_VEC)
    {
      tree ret;
      if (iterator_cache[0] == TREE_PURPOSE (decl))
	ret = iterator_cache[1];
      else
	{
	  tree *tp = &ret;
	  begin_scope (sk_omp, NULL);
	  for (tree it = TREE_PURPOSE (decl); it; it = TREE_CHAIN (it))
	    {
	      *tp = copy_node (it);
	      TREE_VEC_ELT (*tp, 0)
		= tsubst_decl (TREE_VEC_ELT (it, 0), args, complain);
	      DECL_CONTEXT (TREE_VEC_ELT (*tp, 0)) = current_function_decl;
	      pushdecl (TREE_VEC_ELT (*tp, 0));
	      TREE_VEC_ELT (*tp, 1)
		= tsubst_expr (TREE_VEC_ELT (it, 1), args, complain, in_decl);
	      TREE_VEC_ELT (*tp, 2)
		= tsubst_expr (TREE_VEC_ELT (it, 2), args, complain, in_decl);
	      TREE_VEC_ELT (*tp, 3)
		= tsubst_expr (TREE_VEC_ELT (it, 3), args, complain, in_decl);
	      TREE_CHAIN (*tp) = NULL_TREE;
	      tp = &TREE_CHAIN (*tp);
	    }
	  TREE_VEC_ELT (ret, 5) = poplevel (1, 1, 0);
	  iterator_cache[0] = TREE_PURPOSE (decl);
	  iterator_cache[1] = ret;
	}
      return build_tree_list (ret, tsubst_omp_clause_decl (TREE_VALUE (decl),
							   args, complain,
							   in_decl, NULL));
    }

  /* Handle an OpenMP array section represented as a TREE_LIST (or
     OMP_CLAUSE_DOACROSS_KIND).  An OMP_CLAUSE_DOACROSS (with a depend
     kind of OMP_CLAUSE_DOACROSS_SINK) can also be represented as a
     TREE_LIST.  We can handle it exactly the same as an array section
     (purpose, value, and a chain), even though the nomenclature
     (low_bound, length, etc) is different.  */
  if (TREE_CODE (decl) == TREE_LIST)
    {
      tree low_bound
	= tsubst_expr (TREE_PURPOSE (decl), args, complain, in_decl);
      tree length = tsubst_expr (TREE_VALUE (decl), args, complain, in_decl);
      tree chain = tsubst_omp_clause_decl (TREE_CHAIN (decl), args, complain,
					   in_decl, NULL);
      if (TREE_PURPOSE (decl) == low_bound
	  && TREE_VALUE (decl) == length
	  && TREE_CHAIN (decl) == chain)
	return decl;
      tree ret = tree_cons (low_bound, length, chain);
      OMP_CLAUSE_DOACROSS_SINK_NEGATIVE (ret)
	= OMP_CLAUSE_DOACROSS_SINK_NEGATIVE (decl);
      return ret;
    }
  tree ret = tsubst_expr (decl, args, complain, in_decl);
  /* Undo convert_from_reference tsubst_expr could have called.  */
  if (decl
      && REFERENCE_REF_P (ret)
      && !REFERENCE_REF_P (decl))
    ret = TREE_OPERAND (ret, 0);
  return ret;
}

/* Like tsubst_copy, but specifically for OpenMP clauses.  */

static tree
tsubst_omp_clauses (tree clauses, enum c_omp_region_type ort,
		    tree args, tsubst_flags_t complain, tree in_decl)
{
  tree new_clauses = NULL_TREE, nc, oc;
  tree linear_no_step = NULL_TREE;
  tree iterator_cache[2] = { NULL_TREE, NULL_TREE };

  for (oc = clauses; oc ; oc = OMP_CLAUSE_CHAIN (oc))
    {
      nc = copy_node (oc);
      OMP_CLAUSE_CHAIN (nc) = new_clauses;
      new_clauses = nc;

      switch (OMP_CLAUSE_CODE (nc))
	{
	case OMP_CLAUSE_LASTPRIVATE:
	  if (OMP_CLAUSE_LASTPRIVATE_STMT (oc))
	    {
	      OMP_CLAUSE_LASTPRIVATE_STMT (nc) = push_stmt_list ();
	      tsubst_expr (OMP_CLAUSE_LASTPRIVATE_STMT (oc), args,
			   complain, in_decl);
	      OMP_CLAUSE_LASTPRIVATE_STMT (nc)
		= pop_stmt_list (OMP_CLAUSE_LASTPRIVATE_STMT (nc));
	    }
	  /* FALLTHRU */
	case OMP_CLAUSE_PRIVATE:
	case OMP_CLAUSE_SHARED:
	case OMP_CLAUSE_FIRSTPRIVATE:
	case OMP_CLAUSE_COPYIN:
	case OMP_CLAUSE_COPYPRIVATE:
	case OMP_CLAUSE_UNIFORM:
	case OMP_CLAUSE_DEPEND:
	case OMP_CLAUSE_DOACROSS:
	case OMP_CLAUSE_AFFINITY:
	case OMP_CLAUSE_FROM:
	case OMP_CLAUSE_TO:
	case OMP_CLAUSE_MAP:
	case OMP_CLAUSE__CACHE_:
	case OMP_CLAUSE_NONTEMPORAL:
	case OMP_CLAUSE_USE_DEVICE_PTR:
	case OMP_CLAUSE_USE_DEVICE_ADDR:
	case OMP_CLAUSE_IS_DEVICE_PTR:
	case OMP_CLAUSE_HAS_DEVICE_ADDR:
	case OMP_CLAUSE_INCLUSIVE:
	case OMP_CLAUSE_EXCLUSIVE:
	  OMP_CLAUSE_DECL (nc)
	    = tsubst_omp_clause_decl (OMP_CLAUSE_DECL (oc), args, complain,
				      in_decl, iterator_cache);
	  break;
	case OMP_CLAUSE_NUM_TEAMS:
	  if (OMP_CLAUSE_NUM_TEAMS_LOWER_EXPR (oc))
	    OMP_CLAUSE_NUM_TEAMS_LOWER_EXPR (nc)
	      = tsubst_expr (OMP_CLAUSE_NUM_TEAMS_LOWER_EXPR (oc), args,
			     complain, in_decl);
	  /* FALLTHRU */
	case OMP_CLAUSE_TILE:
	case OMP_CLAUSE_IF:
	case OMP_CLAUSE_NUM_THREADS:
	case OMP_CLAUSE_SCHEDULE:
	case OMP_CLAUSE_COLLAPSE:
	case OMP_CLAUSE_FINAL:
	case OMP_CLAUSE_DEVICE:
	case OMP_CLAUSE_DIST_SCHEDULE:
	case OMP_CLAUSE_THREAD_LIMIT:
	case OMP_CLAUSE_SAFELEN:
	case OMP_CLAUSE_SIMDLEN:
	case OMP_CLAUSE_NUM_TASKS:
	case OMP_CLAUSE_GRAINSIZE:
	case OMP_CLAUSE_PRIORITY:
	case OMP_CLAUSE_ORDERED:
	case OMP_CLAUSE_HINT:
	case OMP_CLAUSE_FILTER:
	case OMP_CLAUSE_NUM_GANGS:
	case OMP_CLAUSE_NUM_WORKERS:
	case OMP_CLAUSE_VECTOR_LENGTH:
	case OMP_CLAUSE_WORKER:
	case OMP_CLAUSE_VECTOR:
	case OMP_CLAUSE_ASYNC:
	case OMP_CLAUSE_WAIT:
	case OMP_CLAUSE_DETACH:
	  OMP_CLAUSE_OPERAND (nc, 0)
	    = tsubst_expr (OMP_CLAUSE_OPERAND (oc, 0), args, complain, in_decl);
	  break;
	case OMP_CLAUSE_REDUCTION:
	case OMP_CLAUSE_IN_REDUCTION:
	case OMP_CLAUSE_TASK_REDUCTION:
	  if (OMP_CLAUSE_REDUCTION_PLACEHOLDER (oc))
	    {
	      tree placeholder = OMP_CLAUSE_REDUCTION_PLACEHOLDER (oc);
	      if (TREE_CODE (placeholder) == SCOPE_REF)
		{
		  tree scope = tsubst (TREE_OPERAND (placeholder, 0), args,
				       complain, in_decl);
		  OMP_CLAUSE_REDUCTION_PLACEHOLDER (nc)
		    = build_qualified_name (NULL_TREE, scope,
					    TREE_OPERAND (placeholder, 1),
					    false);
		}
	      else
		gcc_assert (identifier_p (placeholder));
	    }
	  OMP_CLAUSE_DECL (nc)
	    = tsubst_omp_clause_decl (OMP_CLAUSE_DECL (oc), args, complain,
				      in_decl, NULL);
	  break;
	case OMP_CLAUSE_GANG:
	case OMP_CLAUSE_ALIGNED:
	  OMP_CLAUSE_DECL (nc)
	    = tsubst_omp_clause_decl (OMP_CLAUSE_DECL (oc), args, complain,
				      in_decl, NULL);
	  OMP_CLAUSE_OPERAND (nc, 1)
	    = tsubst_expr (OMP_CLAUSE_OPERAND (oc, 1), args, complain, in_decl);
	  break;
	case OMP_CLAUSE_ALLOCATE:
	  OMP_CLAUSE_DECL (nc)
	    = tsubst_omp_clause_decl (OMP_CLAUSE_DECL (oc), args, complain,
				      in_decl, NULL);
	  OMP_CLAUSE_OPERAND (nc, 1)
	    = tsubst_expr (OMP_CLAUSE_OPERAND (oc, 1), args, complain, in_decl);
	  OMP_CLAUSE_OPERAND (nc, 2)
	    = tsubst_expr (OMP_CLAUSE_OPERAND (oc, 2), args, complain, in_decl);
	  break;
	case OMP_CLAUSE_LINEAR:
	  OMP_CLAUSE_DECL (nc)
	    = tsubst_omp_clause_decl (OMP_CLAUSE_DECL (oc), args, complain,
				      in_decl, NULL);
	  if (OMP_CLAUSE_LINEAR_STEP (oc) == NULL_TREE)
	    {
	      gcc_assert (!linear_no_step);
	      linear_no_step = nc;
	    }
	  else if (OMP_CLAUSE_LINEAR_VARIABLE_STRIDE (oc))
	    OMP_CLAUSE_LINEAR_STEP (nc)
	      = tsubst_omp_clause_decl (OMP_CLAUSE_LINEAR_STEP (oc), args,
					complain, in_decl, NULL);
	  else
	    OMP_CLAUSE_LINEAR_STEP (nc)
	      = tsubst_expr (OMP_CLAUSE_LINEAR_STEP (oc), args,
			     complain, in_decl);
	  break;
	case OMP_CLAUSE_NOWAIT:
	case OMP_CLAUSE_DEFAULT:
	case OMP_CLAUSE_UNTIED:
	case OMP_CLAUSE_MERGEABLE:
	case OMP_CLAUSE_INBRANCH:
	case OMP_CLAUSE_NOTINBRANCH:
	case OMP_CLAUSE_PROC_BIND:
	case OMP_CLAUSE_FOR:
	case OMP_CLAUSE_PARALLEL:
	case OMP_CLAUSE_SECTIONS:
	case OMP_CLAUSE_TASKGROUP:
	case OMP_CLAUSE_NOGROUP:
	case OMP_CLAUSE_THREADS:
	case OMP_CLAUSE_SIMD:
	case OMP_CLAUSE_DEFAULTMAP:
	case OMP_CLAUSE_ORDER:
	case OMP_CLAUSE_BIND:
	case OMP_CLAUSE_INDEPENDENT:
	case OMP_CLAUSE_AUTO:
	case OMP_CLAUSE_SEQ:
	case OMP_CLAUSE_IF_PRESENT:
	case OMP_CLAUSE_FINALIZE:
	case OMP_CLAUSE_NOHOST:
	  break;
	default:
	  gcc_unreachable ();
	}
      if ((ort & C_ORT_OMP_DECLARE_SIMD) == C_ORT_OMP)
	switch (OMP_CLAUSE_CODE (nc))
	  {
	  case OMP_CLAUSE_SHARED:
	  case OMP_CLAUSE_PRIVATE:
	  case OMP_CLAUSE_FIRSTPRIVATE:
	  case OMP_CLAUSE_LASTPRIVATE:
	  case OMP_CLAUSE_COPYPRIVATE:
	  case OMP_CLAUSE_LINEAR:
	  case OMP_CLAUSE_REDUCTION:
	  case OMP_CLAUSE_IN_REDUCTION:
	  case OMP_CLAUSE_TASK_REDUCTION:
	  case OMP_CLAUSE_USE_DEVICE_PTR:
	  case OMP_CLAUSE_USE_DEVICE_ADDR:
	  case OMP_CLAUSE_IS_DEVICE_PTR:
	  case OMP_CLAUSE_HAS_DEVICE_ADDR:
	  case OMP_CLAUSE_INCLUSIVE:
	  case OMP_CLAUSE_EXCLUSIVE:
	  case OMP_CLAUSE_ALLOCATE:
	    /* tsubst_expr on SCOPE_REF results in returning
	       finish_non_static_data_member result.  Undo that here.  */
	    if (TREE_CODE (OMP_CLAUSE_DECL (oc)) == SCOPE_REF
		&& (TREE_CODE (TREE_OPERAND (OMP_CLAUSE_DECL (oc), 1))
		    == IDENTIFIER_NODE))
	      {
		tree t = OMP_CLAUSE_DECL (nc);
		tree v = t;
		while (v)
		  switch (TREE_CODE (v))
		    {
		    case COMPONENT_REF:
		    case MEM_REF:
		    case INDIRECT_REF:
		    CASE_CONVERT:
		    case POINTER_PLUS_EXPR:
		      v = TREE_OPERAND (v, 0);
		      continue;
		    case PARM_DECL:
		      if (DECL_CONTEXT (v) == current_function_decl
			  && DECL_ARTIFICIAL (v)
			  && DECL_NAME (v) == this_identifier)
			OMP_CLAUSE_DECL (nc) = TREE_OPERAND (t, 1);
		      /* FALLTHRU */
		    default:
		      v = NULL_TREE;
		      break;
		    }
	      }
	    else if (VAR_P (OMP_CLAUSE_DECL (oc))
		     && DECL_HAS_VALUE_EXPR_P (OMP_CLAUSE_DECL (oc))
		     && DECL_ARTIFICIAL (OMP_CLAUSE_DECL (oc))
		     && DECL_LANG_SPECIFIC (OMP_CLAUSE_DECL (oc))
		     && DECL_OMP_PRIVATIZED_MEMBER (OMP_CLAUSE_DECL (oc)))
	      {
		tree decl = OMP_CLAUSE_DECL (nc);
		if (VAR_P (decl))
		  {
		    retrofit_lang_decl (decl);
		    DECL_OMP_PRIVATIZED_MEMBER (decl) = 1;
		  }
	      }
	    break;
	  default:
	    break;
	  }
    }

  new_clauses = nreverse (new_clauses);
  if (ort != C_ORT_OMP_DECLARE_SIMD)
    {
      new_clauses = finish_omp_clauses (new_clauses, ort);
      if (linear_no_step)
	for (nc = new_clauses; nc; nc = OMP_CLAUSE_CHAIN (nc))
	  if (nc == linear_no_step)
	    {
	      OMP_CLAUSE_LINEAR_STEP (nc) = NULL_TREE;
	      break;
	    }
    }
  return new_clauses;
}

/* Like tsubst_copy_and_build, but unshare TREE_LIST nodes.  */

static tree
tsubst_copy_asm_operands (tree t, tree args, tsubst_flags_t complain,
			  tree in_decl)
{
#define RECUR(t) tsubst_copy_asm_operands (t, args, complain, in_decl)

  tree purpose, value, chain;

  if (t == NULL)
    return t;

  if (TREE_CODE (t) != TREE_LIST)
    return tsubst_copy_and_build (t, args, complain, in_decl);

  if (t == void_list_node)
    return t;

  purpose = TREE_PURPOSE (t);
  if (purpose)
    purpose = RECUR (purpose);
  value = TREE_VALUE (t);
  if (value)
    {
      if (TREE_CODE (value) != LABEL_DECL)
	value = RECUR (value);
      else
	{
	  value = lookup_label (DECL_NAME (value));
	  gcc_assert (TREE_CODE (value) == LABEL_DECL);
	  TREE_USED (value) = 1;
	}
    }
  chain = TREE_CHAIN (t);
  if (chain && chain != void_type_node)
    chain = RECUR (chain);
  return tree_cons (purpose, value, chain);
#undef RECUR
}

/* Used to temporarily communicate the list of #pragma omp parallel
   clauses to #pragma omp for instantiation if they are combined
   together.  */

static tree *omp_parallel_combined_clauses;

static tree tsubst_decomp_names (tree, tree, tree, tsubst_flags_t, tree,
				 tree *, unsigned int *);

/* Substitute one OMP_FOR iterator.  */

static bool
tsubst_omp_for_iterator (tree t, int i, tree declv, tree &orig_declv,
			 tree initv, tree condv, tree incrv, tree *clauses,
			 tree args, tsubst_flags_t complain, tree in_decl)
{
#define RECUR(NODE)				\
  tsubst_expr ((NODE), args, complain, in_decl)
  tree decl, init, cond = NULL_TREE, incr = NULL_TREE;
  bool ret = false;

  init = TREE_VEC_ELT (OMP_FOR_INIT (t), i);
  gcc_assert (TREE_CODE (init) == MODIFY_EXPR);

  decl = TREE_OPERAND (init, 0);
  init = TREE_OPERAND (init, 1);
  tree decl_expr = NULL_TREE;
  bool range_for = TREE_VEC_ELT (OMP_FOR_COND (t), i) == global_namespace;
  if (range_for)
    {
      bool decomp = false;
      if (decl != error_mark_node && DECL_HAS_VALUE_EXPR_P (decl))
	{
	  tree v = DECL_VALUE_EXPR (decl);
	  if (TREE_CODE (v) == ARRAY_REF
	      && VAR_P (TREE_OPERAND (v, 0))
	      && DECL_DECOMPOSITION_P (TREE_OPERAND (v, 0)))
	    {
	      tree decomp_first = NULL_TREE;
	      unsigned decomp_cnt = 0;
	      tree d = tsubst_decl (TREE_OPERAND (v, 0), args, complain);
	      maybe_push_decl (d);
	      d = tsubst_decomp_names (d, TREE_OPERAND (v, 0), args, complain,
				       in_decl, &decomp_first, &decomp_cnt);
	      decomp = true;
	      if (d == error_mark_node)
		decl = error_mark_node;
	      else
		for (unsigned int i = 0; i < decomp_cnt; i++)
		  {
		    if (!DECL_HAS_VALUE_EXPR_P (decomp_first))
		      {
			tree v = build_nt (ARRAY_REF, d,
					   size_int (decomp_cnt - i - 1),
					   NULL_TREE, NULL_TREE);
			SET_DECL_VALUE_EXPR (decomp_first, v);
			DECL_HAS_VALUE_EXPR_P (decomp_first) = 1;
		      }
		    fit_decomposition_lang_decl (decomp_first, d);
		    decomp_first = DECL_CHAIN (decomp_first);
		  }
	    }
	}
      decl = tsubst_decl (decl, args, complain);
      if (!decomp)
	maybe_push_decl (decl);
    }
  else if (init && TREE_CODE (init) == DECL_EXPR)
    {
      /* We need to jump through some hoops to handle declarations in the
	 init-statement, since we might need to handle auto deduction,
	 but we need to keep control of initialization.  */
      decl_expr = init;
      init = DECL_INITIAL (DECL_EXPR_DECL (init));
      decl = tsubst_decl (decl, args, complain);
    }
  else
    {
      if (TREE_CODE (decl) == SCOPE_REF)
	{
	  decl = RECUR (decl);
	  if (TREE_CODE (decl) == COMPONENT_REF)
	    {
	      tree v = decl;
	      while (v)
		switch (TREE_CODE (v))
		  {
		  case COMPONENT_REF:
		  case MEM_REF:
		  case INDIRECT_REF:
		  CASE_CONVERT:
		  case POINTER_PLUS_EXPR:
		    v = TREE_OPERAND (v, 0);
		    continue;
		  case PARM_DECL:
		    if (DECL_CONTEXT (v) == current_function_decl
			&& DECL_ARTIFICIAL (v)
			&& DECL_NAME (v) == this_identifier)
		      {
			decl = TREE_OPERAND (decl, 1);
			decl = omp_privatize_field (decl, false);
		      }
		    /* FALLTHRU */
		  default:
		    v = NULL_TREE;
		    break;
		  }
	    }
	}
      else
	decl = RECUR (decl);
    }
  if (init && TREE_CODE (init) == TREE_VEC)
    {
      init = copy_node (init);
      TREE_VEC_ELT (init, 0)
	= tsubst_decl (TREE_VEC_ELT (init, 0), args, complain);
      TREE_VEC_ELT (init, 1) = RECUR (TREE_VEC_ELT (init, 1));
      TREE_VEC_ELT (init, 2) = RECUR (TREE_VEC_ELT (init, 2));
    }
  else
    init = RECUR (init);

  if (orig_declv && OMP_FOR_ORIG_DECLS (t))
    {
      tree o = TREE_VEC_ELT (OMP_FOR_ORIG_DECLS (t), i);
      if (TREE_CODE (o) == TREE_LIST)
	TREE_VEC_ELT (orig_declv, i)
	  = tree_cons (RECUR (TREE_PURPOSE (o)),
		       RECUR (TREE_VALUE (o)),
		       NULL_TREE);
      else
	TREE_VEC_ELT (orig_declv, i) = RECUR (o);
    }

  if (range_for)
    {
      tree this_pre_body = NULL_TREE;
      tree orig_init = NULL_TREE;
      tree orig_decl = NULL_TREE;
      cp_convert_omp_range_for (this_pre_body, NULL, decl, orig_decl, init,
				orig_init, cond, incr);
      if (orig_decl)
	{
	  if (orig_declv == NULL_TREE)
	    orig_declv = copy_node (declv);
	  TREE_VEC_ELT (orig_declv, i) = orig_decl;
	  ret = true;
	}
      else if (orig_declv)
	TREE_VEC_ELT (orig_declv, i) = decl;
    }

  tree auto_node = type_uses_auto (TREE_TYPE (decl));
  if (!range_for && auto_node && init)
    TREE_TYPE (decl)
      = do_auto_deduction (TREE_TYPE (decl), init, auto_node, complain);

  gcc_assert (!type_dependent_expression_p (decl));

  if (!CLASS_TYPE_P (TREE_TYPE (decl)) || range_for)
    {
      if (decl_expr)
	{
	  /* Declare the variable, but don't let that initialize it.  */
	  tree init_sav = DECL_INITIAL (DECL_EXPR_DECL (decl_expr));
	  DECL_INITIAL (DECL_EXPR_DECL (decl_expr)) = NULL_TREE;
	  RECUR (decl_expr);
	  DECL_INITIAL (DECL_EXPR_DECL (decl_expr)) = init_sav;
	}

      if (!range_for)
	{
	  cond = TREE_VEC_ELT (OMP_FOR_COND (t), i);
	  if (COMPARISON_CLASS_P (cond)
	      && TREE_CODE (TREE_OPERAND (cond, 1)) == TREE_VEC)
	    {
	      tree lhs = RECUR (TREE_OPERAND (cond, 0));
	      tree rhs = copy_node (TREE_OPERAND (cond, 1));
	      TREE_VEC_ELT (rhs, 0)
		= tsubst_decl (TREE_VEC_ELT (rhs, 0), args, complain);
	      TREE_VEC_ELT (rhs, 1) = RECUR (TREE_VEC_ELT (rhs, 1));
	      TREE_VEC_ELT (rhs, 2) = RECUR (TREE_VEC_ELT (rhs, 2));
	      cond = build2 (TREE_CODE (cond), TREE_TYPE (cond),
			     lhs, rhs);	      
	    }
	  else
	    cond = RECUR (cond);
	  incr = TREE_VEC_ELT (OMP_FOR_INCR (t), i);
	  if (TREE_CODE (incr) == MODIFY_EXPR)
	    {
	      tree lhs = RECUR (TREE_OPERAND (incr, 0));
	      tree rhs = RECUR (TREE_OPERAND (incr, 1));
	      incr = build_x_modify_expr (EXPR_LOCATION (incr), lhs,
					  NOP_EXPR, rhs, NULL_TREE, complain);
	    }
	  else
	    incr = RECUR (incr);
	  if (orig_declv && !OMP_FOR_ORIG_DECLS (t))
	    TREE_VEC_ELT (orig_declv, i) = decl;
	}
      TREE_VEC_ELT (declv, i) = decl;
      TREE_VEC_ELT (initv, i) = init;
      TREE_VEC_ELT (condv, i) = cond;
      TREE_VEC_ELT (incrv, i) = incr;
      return ret;
    }

  if (decl_expr)
    {
      /* Declare and initialize the variable.  */
      RECUR (decl_expr);
      init = NULL_TREE;
    }
  else if (init)
    {
      tree *pc;
      int j;
      for (j = ((omp_parallel_combined_clauses == NULL
		|| TREE_CODE (t) == OMP_LOOP) ? 1 : 0); j < 2; j++)
	{
	  for (pc = j ? clauses : omp_parallel_combined_clauses; *pc; )
	    {
	      if (OMP_CLAUSE_CODE (*pc) == OMP_CLAUSE_PRIVATE
		  && OMP_CLAUSE_DECL (*pc) == decl)
		break;
	      else if (OMP_CLAUSE_CODE (*pc) == OMP_CLAUSE_LASTPRIVATE
		       && OMP_CLAUSE_DECL (*pc) == decl)
		{
		  if (j)
		    break;
		  /* Move lastprivate (decl) clause to OMP_FOR_CLAUSES.  */
		  tree c = *pc;
		  *pc = OMP_CLAUSE_CHAIN (c);
		  OMP_CLAUSE_CHAIN (c) = *clauses;
		  *clauses = c;
		}
	      else if (OMP_CLAUSE_CODE (*pc) == OMP_CLAUSE_FIRSTPRIVATE
		       && OMP_CLAUSE_DECL (*pc) == decl)
		{
		  error ("iteration variable %qD should not be firstprivate",
			 decl);
		  *pc = OMP_CLAUSE_CHAIN (*pc);
		}
	      else if (OMP_CLAUSE_CODE (*pc) == OMP_CLAUSE_REDUCTION
		       && OMP_CLAUSE_DECL (*pc) == decl)
		{
		  error ("iteration variable %qD should not be reduction",
			 decl);
		  *pc = OMP_CLAUSE_CHAIN (*pc);
		}
	      else
		pc = &OMP_CLAUSE_CHAIN (*pc);
	    }
	  if (*pc)
	    break;
	}
      if (*pc == NULL_TREE)
	{
	  tree c = build_omp_clause (input_location,
				     TREE_CODE (t) == OMP_LOOP
				     ? OMP_CLAUSE_LASTPRIVATE
				     : OMP_CLAUSE_PRIVATE);
	  OMP_CLAUSE_DECL (c) = decl;
	  c = finish_omp_clauses (c, C_ORT_OMP);
	  if (c)
	    {
	      OMP_CLAUSE_CHAIN (c) = *clauses;
	      *clauses = c;
	    }
	}
    }
  cond = TREE_VEC_ELT (OMP_FOR_COND (t), i);
  if (COMPARISON_CLASS_P (cond))
    {
      tree op0 = RECUR (TREE_OPERAND (cond, 0));
      tree op1 = RECUR (TREE_OPERAND (cond, 1));
      cond = build2 (TREE_CODE (cond), boolean_type_node, op0, op1);
    }
  else
    cond = RECUR (cond);
  incr = TREE_VEC_ELT (OMP_FOR_INCR (t), i);
  switch (TREE_CODE (incr))
    {
    case PREINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      incr = build2 (TREE_CODE (incr), TREE_TYPE (decl),
		     RECUR (TREE_OPERAND (incr, 0)), NULL_TREE);
      break;
    case MODIFY_EXPR:
      if (TREE_CODE (TREE_OPERAND (incr, 1)) == PLUS_EXPR
	  || TREE_CODE (TREE_OPERAND (incr, 1)) == MINUS_EXPR)
	{
	  tree rhs = TREE_OPERAND (incr, 1);
	  tree lhs = RECUR (TREE_OPERAND (incr, 0));
	  tree rhs0 = RECUR (TREE_OPERAND (rhs, 0));
	  tree rhs1 = RECUR (TREE_OPERAND (rhs, 1));
	  incr = build2 (MODIFY_EXPR, TREE_TYPE (decl), lhs,
			 build2 (TREE_CODE (rhs), TREE_TYPE (decl),
				 rhs0, rhs1));
	}
      else
	incr = RECUR (incr);
      break;
    case MODOP_EXPR:
      if (TREE_CODE (TREE_OPERAND (incr, 1)) == PLUS_EXPR
	  || TREE_CODE (TREE_OPERAND (incr, 1)) == MINUS_EXPR)
	{
	  tree lhs = RECUR (TREE_OPERAND (incr, 0));
	  incr = build2 (MODIFY_EXPR, TREE_TYPE (decl), lhs,
			 build2 (TREE_CODE (TREE_OPERAND (incr, 1)),
				 TREE_TYPE (decl), lhs,
				 RECUR (TREE_OPERAND (incr, 2))));
	}
      else if (TREE_CODE (TREE_OPERAND (incr, 1)) == NOP_EXPR
	       && (TREE_CODE (TREE_OPERAND (incr, 2)) == PLUS_EXPR
		   || (TREE_CODE (TREE_OPERAND (incr, 2)) == MINUS_EXPR)))
	{
	  tree rhs = TREE_OPERAND (incr, 2);
	  tree lhs = RECUR (TREE_OPERAND (incr, 0));
	  tree rhs0 = RECUR (TREE_OPERAND (rhs, 0));
	  tree rhs1 = RECUR (TREE_OPERAND (rhs, 1));
	  incr = build2 (MODIFY_EXPR, TREE_TYPE (decl), lhs,
			 build2 (TREE_CODE (rhs), TREE_TYPE (decl),
				 rhs0, rhs1));
	}
      else
	incr = RECUR (incr);
      break;
    default:
      incr = RECUR (incr);
      break;
    }

  if (orig_declv && !OMP_FOR_ORIG_DECLS (t))
    TREE_VEC_ELT (orig_declv, i) = decl;
  TREE_VEC_ELT (declv, i) = decl;
  TREE_VEC_ELT (initv, i) = init;
  TREE_VEC_ELT (condv, i) = cond;
  TREE_VEC_ELT (incrv, i) = incr;
  return false;
#undef RECUR
}

/* Helper function of tsubst_expr, find OMP_TEAMS inside
   of OMP_TARGET's body.  */

static tree
tsubst_find_omp_teams (tree *tp, int *walk_subtrees, void *)
{
  *walk_subtrees = 0;
  switch (TREE_CODE (*tp))
    {
    case OMP_TEAMS:
      return *tp;
    case BIND_EXPR:
    case STATEMENT_LIST:
      *walk_subtrees = 1;
      break;
    default:
      break;
    }
  return NULL_TREE;
}

/* Helper function for tsubst_expr.  For decomposition declaration
   artificial base DECL, which is tsubsted PATTERN_DECL, tsubst
   also the corresponding decls representing the identifiers
   of the decomposition declaration.  Return DECL if successful
   or error_mark_node otherwise, set *FIRST to the first decl
   in the list chained through DECL_CHAIN and *CNT to the number
   of such decls.  */

static tree
tsubst_decomp_names (tree decl, tree pattern_decl, tree args,
		     tsubst_flags_t complain, tree in_decl, tree *first,
		     unsigned int *cnt)
{
  tree decl2, decl3, prev = decl;
  *cnt = 0;
  gcc_assert (DECL_NAME (decl) == NULL_TREE);
  for (decl2 = DECL_CHAIN (pattern_decl);
       decl2
       && VAR_P (decl2)
       && DECL_DECOMPOSITION_P (decl2)
       && DECL_NAME (decl2);
       decl2 = DECL_CHAIN (decl2))
    {
      if (TREE_TYPE (decl2) == error_mark_node && *cnt == 0)
	{
	  gcc_assert (errorcount);
	  return error_mark_node;
	}
      (*cnt)++;
      gcc_assert (DECL_DECOMP_BASE (decl2) == pattern_decl);
      gcc_assert (DECL_HAS_VALUE_EXPR_P (decl2));
      tree v = DECL_VALUE_EXPR (decl2);
      DECL_HAS_VALUE_EXPR_P (decl2) = 0;
      SET_DECL_VALUE_EXPR (decl2, NULL_TREE);
      decl3 = tsubst (decl2, args, complain, in_decl);
      SET_DECL_VALUE_EXPR (decl2, v);
      DECL_HAS_VALUE_EXPR_P (decl2) = 1;
      if (VAR_P (decl3))
	DECL_TEMPLATE_INSTANTIATED (decl3) = 1;
      else
	{
	  gcc_assert (errorcount);
	  decl = error_mark_node;
	  continue;
	}
      maybe_push_decl (decl3);
      if (error_operand_p (decl3))
	decl = error_mark_node;
      else if (decl != error_mark_node
	       && DECL_CHAIN (decl3) != prev
	       && decl != prev)
	{
	  gcc_assert (errorcount);
	  decl = error_mark_node;
	}
      else
	prev = decl3;
    }
  *first = prev;
  return decl;
}

/* Return the proper local_specialization for init-capture pack DECL.  */

static tree
lookup_init_capture_pack (tree decl)
{
  /* We handle normal pack captures by forwarding to the specialization of the
     captured parameter.  We can't do that for pack init-captures; we need them
     to have their own local_specialization.  We created the individual
     VAR_DECLs (if any) under build_capture_proxy, and we need to collect them
     when we process the DECL_EXPR for the pack init-capture in the template.
     So, how do we find them?  We don't know the capture proxy pack when
     building the individual resulting proxies, and we don't know the
     individual proxies when instantiating the pack.  What we have in common is
     the FIELD_DECL.

     So...when we instantiate the FIELD_DECL, we stick the result in
     local_specializations.  Then at the DECL_EXPR we look up that result, see
     how many elements it has, synthesize the names, and look them up.  */

  tree cname = DECL_NAME (decl);
  tree val = DECL_VALUE_EXPR (decl);
  tree field = TREE_OPERAND (val, 1);
  gcc_assert (TREE_CODE (field) == FIELD_DECL);
  tree fpack = retrieve_local_specialization (field);
  if (fpack == error_mark_node)
    return error_mark_node;

  int len = 1;
  tree vec = NULL_TREE;
  tree r = NULL_TREE;
  if (TREE_CODE (fpack) == TREE_VEC)
    {
      len = TREE_VEC_LENGTH (fpack);
      vec = make_tree_vec (len);
      r = make_node (NONTYPE_ARGUMENT_PACK);
      ARGUMENT_PACK_ARGS (r) = vec;
    }
  for (int i = 0; i < len; ++i)
    {
      tree ename = vec ? make_ith_pack_parameter_name (cname, i) : cname;
      tree elt = lookup_name (ename);
      if (vec)
	TREE_VEC_ELT (vec, i) = elt;
      else
	r = elt;
    }
  return r;
}

/* T is an operand of a template tree being substituted.  Return whether
   T is dependent such that we should suppress some warnings that would
   make sense if the substituted expression were written directly, like
     template <int I> bool f() { return I == 2; }
   We don't want to warn when instantiating f that comparing two constants
   always has the same value.

   This is a more limited concept of dependence than instantiation-dependent;
   here we don't care whether substitution could fail.  */

static bool
dependent_operand_p (tree t)
{
  while (TREE_CODE (t) == IMPLICIT_CONV_EXPR)
    t = TREE_OPERAND (t, 0);
  ++processing_template_decl;
  bool r = (potential_constant_expression (t)
	    ? value_dependent_expression_p (t)
	    : type_dependent_expression_p (t));
  --processing_template_decl;
  return r;
}

/* Like tsubst_copy for expressions, etc. but also does semantic
   processing.  */

tree
tsubst_expr (tree t, tree args, tsubst_flags_t complain, tree in_decl)
{
#define RETURN(EXP) do { r = (EXP); goto out; } while(0)
#define RECUR(NODE)				\
  tsubst_expr ((NODE), args, complain, in_decl)

  tree stmt, tmp;
  tree r;
  location_t loc;

  if (t == NULL_TREE || t == error_mark_node)
    return t;

  loc = input_location;
  if (location_t eloc = cp_expr_location (t))
    input_location = eloc;
  if (STATEMENT_CODE_P (TREE_CODE (t)))
    current_stmt_tree ()->stmts_are_full_exprs_p = STMT_IS_FULL_EXPR_P (t);

  switch (TREE_CODE (t))
    {
    case STATEMENT_LIST:
      {
	for (tree stmt : tsi_range (t))
	  RECUR (stmt);
	break;
      }

    case CTOR_INITIALIZER:
      finish_mem_initializers (tsubst_initializer_list
			       (TREE_OPERAND (t, 0), args));
      break;

    case RETURN_EXPR:
      finish_return_stmt (RECUR (TREE_OPERAND (t, 0)));
      break;

    case CO_RETURN_EXPR:
      finish_co_return_stmt (input_location, RECUR (TREE_OPERAND (t, 0)));
      break;

    case CO_YIELD_EXPR:
      stmt = finish_co_yield_expr (input_location,
				   RECUR (TREE_OPERAND (t, 0)));
      RETURN (stmt);

    case CO_AWAIT_EXPR:
      stmt = finish_co_await_expr (input_location,
				   RECUR (TREE_OPERAND (t, 0)));
      RETURN (stmt);

    case EXPR_STMT:
      tmp = RECUR (EXPR_STMT_EXPR (t));
      if (EXPR_STMT_STMT_EXPR_RESULT (t))
	finish_stmt_expr_expr (tmp, cur_stmt_expr);
      else
	finish_expr_stmt (tmp);
      break;

    case USING_STMT:
      finish_using_directive (USING_STMT_NAMESPACE (t), /*attribs=*/NULL_TREE);
      break;

    case PRECONDITION_STMT:
    case POSTCONDITION_STMT:
      gcc_unreachable ();

    case ASSERTION_STMT:
      {
	r = tsubst_contract (NULL_TREE, t, args, complain, in_decl);
	if (r != error_mark_node)
	  add_stmt (r);
	RETURN (r);
      }
      break;

    case DECL_EXPR:
      {
	tree decl, pattern_decl;
	tree init;

	pattern_decl = decl = DECL_EXPR_DECL (t);
	if (TREE_CODE (decl) == LABEL_DECL)
	  finish_label_decl (DECL_NAME (decl));
	else if (TREE_CODE (decl) == USING_DECL)
	  {
	    tree scope = USING_DECL_SCOPE (decl);
	    if (DECL_DEPENDENT_P (decl))
	      {
		scope = tsubst (scope, args, complain, in_decl);
		if (!MAYBE_CLASS_TYPE_P (scope)
		    && TREE_CODE (scope) != ENUMERAL_TYPE)
		  {
		    if (complain & tf_error)
		      error_at (DECL_SOURCE_LOCATION (decl), "%qT is not a "
				"class, namespace, or enumeration", scope);
		    return error_mark_node;
		  }
		finish_nonmember_using_decl (scope, DECL_NAME (decl));
	      }
	    else
	      {
		/* This is a non-dependent using-decl, and we'll have
		   used the names it found during template parsing.  We do
		   not want to do the lookup again, because we might not
		   find the things we found then.  */
		gcc_checking_assert (scope == tsubst (scope, args,
						      complain, in_decl));
		/* We still need to push the bindings so that we can look up
		   this name later.  */
		push_using_decl_bindings (DECL_NAME (decl),
					  USING_DECL_DECLS (decl));
	      }
	  }
	else if (is_capture_proxy (decl)
		 && !DECL_TEMPLATE_INSTANTIATION (current_function_decl))
	  {
	    /* We're in tsubst_lambda_expr, we've already inserted a new
	       capture proxy, so look it up and register it.  */
	    tree inst;
	    if (!DECL_PACK_P (decl))
	      {
		inst = lookup_name (DECL_NAME (decl), LOOK_where::BLOCK,
				    LOOK_want::HIDDEN_LAMBDA);
		gcc_assert (inst != decl && is_capture_proxy (inst));
	      }
	    else if (is_normal_capture_proxy (decl))
	      {
		inst = (retrieve_local_specialization
			(DECL_CAPTURED_VARIABLE (decl)));
		gcc_assert (TREE_CODE (inst) == NONTYPE_ARGUMENT_PACK
			    || DECL_PACK_P (inst));
	      }
	    else
	      inst = lookup_init_capture_pack (decl);

	    register_local_specialization (inst, decl);
	    break;
	  }
	else if (DECL_PRETTY_FUNCTION_P (decl))
	  decl = make_fname_decl (DECL_SOURCE_LOCATION (decl),
				  DECL_NAME (decl),
				  true/*DECL_PRETTY_FUNCTION_P (decl)*/);
	else if (DECL_IMPLICIT_TYPEDEF_P (decl)
		 && LAMBDA_TYPE_P (TREE_TYPE (decl)))
	  /* Don't copy the old closure; we'll create a new one in
	     tsubst_lambda_expr.  */
	  break;
	else
	  {
	    init = DECL_INITIAL (decl);
	    decl = tsubst (decl, args, complain, in_decl);
	    if (decl != error_mark_node)
	      {
		/* By marking the declaration as instantiated, we avoid
		   trying to instantiate it.  Since instantiate_decl can't
		   handle local variables, and since we've already done
		   all that needs to be done, that's the right thing to
		   do.  */
		if (VAR_P (decl))
		  DECL_TEMPLATE_INSTANTIATED (decl) = 1;
		if (VAR_P (decl) && !DECL_NAME (decl)
		    && ANON_AGGR_TYPE_P (TREE_TYPE (decl)))
		  /* Anonymous aggregates are a special case.  */
		  finish_anon_union (decl);
		else if (is_capture_proxy (DECL_EXPR_DECL (t)))
		  {
		    DECL_CONTEXT (decl) = current_function_decl;
		    if (DECL_NAME (decl) == this_identifier)
		      {
			tree lam = DECL_CONTEXT (current_function_decl);
			lam = CLASSTYPE_LAMBDA_EXPR (lam);
			LAMBDA_EXPR_THIS_CAPTURE (lam) = decl;
		      }
		    insert_capture_proxy (decl);
		  }
		else if (DECL_IMPLICIT_TYPEDEF_P (t))
		  /* We already did a pushtag.  */;
		else if (VAR_OR_FUNCTION_DECL_P (decl)
			 && DECL_LOCAL_DECL_P (decl))
		  {
		    if (TREE_CODE (DECL_CONTEXT (decl)) == FUNCTION_DECL)
		      DECL_CONTEXT (decl) = NULL_TREE;
		    decl = pushdecl (decl);
		    if (TREE_CODE (decl) == FUNCTION_DECL
			&& DECL_OMP_DECLARE_REDUCTION_P (decl)
			&& cp_check_omp_declare_reduction (decl))
		      instantiate_body (pattern_decl, args, decl, true);
		  }
		else
		  {
		    bool const_init = false;
		    unsigned int cnt = 0;
		    tree first = NULL_TREE, ndecl = error_mark_node;
		    tree asmspec_tree = NULL_TREE;
		    maybe_push_decl (decl);

		    if (VAR_P (decl)
			&& DECL_LANG_SPECIFIC (decl)
			&& DECL_OMP_PRIVATIZED_MEMBER (decl))
		      break;

		    if (VAR_P (decl)
			&& DECL_DECOMPOSITION_P (decl)
			&& TREE_TYPE (pattern_decl) != error_mark_node)
		      ndecl = tsubst_decomp_names (decl, pattern_decl, args,
						   complain, in_decl, &first,
						   &cnt);

		    init = tsubst_init (init, decl, args, complain, in_decl);

		    if (VAR_P (decl))
		      const_init = (DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P
				    (pattern_decl));

		    if (ndecl != error_mark_node)
		      cp_maybe_mangle_decomp (ndecl, first, cnt);

		    /* In a non-template function, VLA type declarations are
		       handled in grokdeclarator; for templates, handle them
		       now.  */
		    predeclare_vla (decl);

		    if (VAR_P (decl) && DECL_HARD_REGISTER (pattern_decl))
		      {
			tree id = DECL_ASSEMBLER_NAME (pattern_decl);
			const char *asmspec = IDENTIFIER_POINTER (id);
			gcc_assert (asmspec[0] == '*');
			asmspec_tree
			  = build_string (IDENTIFIER_LENGTH (id) - 1,
					  asmspec + 1);
			TREE_TYPE (asmspec_tree) = char_array_type_node;
		      }

		    cp_finish_decl (decl, init, const_init, asmspec_tree, 0);

		    if (ndecl != error_mark_node)
		      cp_finish_decomp (ndecl, first, cnt);
		  }
	      }
	  }

	break;
      }

    case FOR_STMT:
      stmt = begin_for_stmt (NULL_TREE, NULL_TREE);
      RECUR (FOR_INIT_STMT (t));
      finish_init_stmt (stmt);
      tmp = RECUR (FOR_COND (t));
      finish_for_cond (tmp, stmt, false, 0, false);
      tmp = RECUR (FOR_EXPR (t));
      finish_for_expr (tmp, stmt);
      {
	bool prev = note_iteration_stmt_body_start ();
	RECUR (FOR_BODY (t));
	note_iteration_stmt_body_end (prev);
      }
      finish_for_stmt (stmt);
      break;

    case RANGE_FOR_STMT:
      {
	/* Construct another range_for, if this is not a final
	   substitution (for inside a generic lambda of a
	   template).  Otherwise convert to a regular for.  */
        tree decl, expr;
        stmt = (processing_template_decl
		? begin_range_for_stmt (NULL_TREE, NULL_TREE)
		: begin_for_stmt (NULL_TREE, NULL_TREE));
	RECUR (RANGE_FOR_INIT_STMT (t));
        decl = RANGE_FOR_DECL (t);
        decl = tsubst (decl, args, complain, in_decl);
        maybe_push_decl (decl);
        expr = RECUR (RANGE_FOR_EXPR (t));

	tree decomp_first = NULL_TREE;
	unsigned decomp_cnt = 0;
	if (VAR_P (decl) && DECL_DECOMPOSITION_P (decl))
	  decl = tsubst_decomp_names (decl, RANGE_FOR_DECL (t), args,
				      complain, in_decl,
				      &decomp_first, &decomp_cnt);

	if (processing_template_decl)
	  {
	    RANGE_FOR_IVDEP (stmt) = RANGE_FOR_IVDEP (t);
	    RANGE_FOR_UNROLL (stmt) = RANGE_FOR_UNROLL (t);
	    RANGE_FOR_NOVECTOR (stmt) = RANGE_FOR_NOVECTOR (t);
	    finish_range_for_decl (stmt, decl, expr);
	    if (decomp_first && decl != error_mark_node)
	      cp_finish_decomp (decl, decomp_first, decomp_cnt);
	  }
	else
	  {
	    unsigned short unroll = (RANGE_FOR_UNROLL (t)
				     ? tree_to_uhwi (RANGE_FOR_UNROLL (t)) : 0);
	    stmt = cp_convert_range_for (stmt, decl, expr,
					 decomp_first, decomp_cnt,
					 RANGE_FOR_IVDEP (t), unroll,
					 RANGE_FOR_NOVECTOR (t));
	  }

	bool prev = note_iteration_stmt_body_start ();
        RECUR (RANGE_FOR_BODY (t));
	note_iteration_stmt_body_end (prev);
        finish_for_stmt (stmt);
      }
      break;

    case WHILE_STMT:
      stmt = begin_while_stmt ();
      tmp = RECUR (WHILE_COND (t));
      finish_while_stmt_cond (tmp, stmt, false, 0, false);
      {
	bool prev = note_iteration_stmt_body_start ();
	RECUR (WHILE_BODY (t));
	note_iteration_stmt_body_end (prev);
      }
      finish_while_stmt (stmt);
      break;

    case DO_STMT:
      stmt = begin_do_stmt ();
      {
	bool prev = note_iteration_stmt_body_start ();
	RECUR (DO_BODY (t));
	note_iteration_stmt_body_end (prev);
      }
      finish_do_body (stmt);
      tmp = RECUR (DO_COND (t));
      finish_do_stmt (tmp, stmt, false, 0, false);
      break;

    case IF_STMT:
      stmt = begin_if_stmt ();
      IF_STMT_CONSTEXPR_P (stmt) = IF_STMT_CONSTEXPR_P (t);
      IF_STMT_CONSTEVAL_P (stmt) = IF_STMT_CONSTEVAL_P (t);
      if (IF_STMT_CONSTEXPR_P (t))
	args = add_extra_args (IF_STMT_EXTRA_ARGS (t), args, complain, in_decl);
      {
	tree cond = IF_COND (t);
	bool was_dep = dependent_operand_p (cond);
	cond = RECUR (cond);
	warning_sentinel s1(warn_address, was_dep);
	tmp = finish_if_stmt_cond (cond, stmt);
      }
      if (IF_STMT_CONSTEXPR_P (t)
	  && instantiation_dependent_expression_p (tmp))
	{
	  /* We're partially instantiating a generic lambda, but the condition
	     of the constexpr if is still dependent.  Don't substitute into the
	     branches now, just remember the template arguments.  */
	  do_poplevel (IF_SCOPE (stmt));
	  IF_COND (stmt) = IF_COND (t);
	  THEN_CLAUSE (stmt) = THEN_CLAUSE (t);
	  ELSE_CLAUSE (stmt) = ELSE_CLAUSE (t);
	  IF_STMT_EXTRA_ARGS (stmt) = build_extra_args (t, args, complain);
	  add_stmt (stmt);
	  break;
	}
      if (IF_STMT_CONSTEXPR_P (t) && integer_zerop (tmp))
	/* Don't instantiate the THEN_CLAUSE. */;
      else if (IF_STMT_CONSTEVAL_P (t))
	{
	  bool save_in_consteval_if_p = in_consteval_if_p;
	  in_consteval_if_p = true;
	  RECUR (THEN_CLAUSE (t));
	  in_consteval_if_p = save_in_consteval_if_p;
	}
      else
	{
	  tree folded = fold_non_dependent_expr (tmp, complain);
	  bool inhibit = integer_zerop (folded);
	  if (inhibit)
	    ++c_inhibit_evaluation_warnings;
	  RECUR (THEN_CLAUSE (t));
	  if (inhibit)
	    --c_inhibit_evaluation_warnings;
	}
      finish_then_clause (stmt);

      if (IF_STMT_CONSTEXPR_P (t) && integer_nonzerop (tmp))
	/* Don't instantiate the ELSE_CLAUSE. */;
      else if (ELSE_CLAUSE (t))
	{
	  tree folded = fold_non_dependent_expr (tmp, complain);
	  bool inhibit = integer_nonzerop (folded);
	  begin_else_clause (stmt);
	  if (inhibit)
	    ++c_inhibit_evaluation_warnings;
	  RECUR (ELSE_CLAUSE (t));
	  if (inhibit)
	    --c_inhibit_evaluation_warnings;
	  finish_else_clause (stmt);
	}

      finish_if_stmt (stmt);
      break;

    case BIND_EXPR:
      if (BIND_EXPR_BODY_BLOCK (t))
	stmt = begin_function_body ();
      else
	stmt = begin_compound_stmt (BIND_EXPR_TRY_BLOCK (t)
				    ? BCS_TRY_BLOCK : 0);

      RECUR (BIND_EXPR_BODY (t));

      if (BIND_EXPR_BODY_BLOCK (t))
	finish_function_body (stmt);
      else
	finish_compound_stmt (stmt);
      break;

    case BREAK_STMT:
      finish_break_stmt ();
      break;

    case CONTINUE_STMT:
      finish_continue_stmt ();
      break;

    case SWITCH_STMT:
      stmt = begin_switch_stmt ();
      tmp = RECUR (SWITCH_STMT_COND (t));
      finish_switch_cond (tmp, stmt);
      RECUR (SWITCH_STMT_BODY (t));
      finish_switch_stmt (stmt);
      break;

    case CASE_LABEL_EXPR:
      {
	tree decl = CASE_LABEL (t);
	tree low = RECUR (CASE_LOW (t));
	tree high = RECUR (CASE_HIGH (t));
	tree l = finish_case_label (EXPR_LOCATION (t), low, high);
	if (l && TREE_CODE (l) == CASE_LABEL_EXPR)
	  {
	    tree label = CASE_LABEL (l);
	    FALLTHROUGH_LABEL_P (label) = FALLTHROUGH_LABEL_P (decl);
	    if (DECL_ATTRIBUTES (decl) != NULL_TREE)
	      cplus_decl_attributes (&label, DECL_ATTRIBUTES (decl), 0);
	  }
      }
      break;

    case LABEL_EXPR:
      {
	tree decl = LABEL_EXPR_LABEL (t);
	tree label;

	label = finish_label_stmt (DECL_NAME (decl));
	if (TREE_CODE (label) == LABEL_DECL)
	  FALLTHROUGH_LABEL_P (label) = FALLTHROUGH_LABEL_P (decl);
	if (DECL_ATTRIBUTES (decl) != NULL_TREE)
	  cplus_decl_attributes (&label, DECL_ATTRIBUTES (decl), 0);
      }
      break;

    case GOTO_EXPR:
      tmp = GOTO_DESTINATION (t);
      if (TREE_CODE (tmp) != LABEL_DECL)
	/* Computed goto's must be tsubst'd into.  On the other hand,
	   non-computed gotos must not be; the identifier in question
	   will have no binding.  */
	tmp = RECUR (tmp);
      else
	tmp = DECL_NAME (tmp);
      finish_goto_stmt (tmp);
      break;

    case ASM_EXPR:
      {
	tree string = RECUR (ASM_STRING (t));
	tree outputs = tsubst_copy_asm_operands (ASM_OUTPUTS (t), args,
						 complain, in_decl);
	tree inputs = tsubst_copy_asm_operands (ASM_INPUTS (t), args,
						complain, in_decl);
	tree clobbers = tsubst_copy_asm_operands (ASM_CLOBBERS (t), args,
	 					  complain, in_decl);
	tree labels = tsubst_copy_asm_operands (ASM_LABELS (t), args,
						complain, in_decl);
	tmp = finish_asm_stmt (EXPR_LOCATION (t), ASM_VOLATILE_P (t), string,
			       outputs, inputs, clobbers, labels,
			       ASM_INLINE_P (t));
	tree asm_expr = tmp;
	if (TREE_CODE (asm_expr) == CLEANUP_POINT_EXPR)
	  asm_expr = TREE_OPERAND (asm_expr, 0);
	ASM_INPUT_P (asm_expr) = ASM_INPUT_P (t);
      }
      break;

    case TRY_BLOCK:
      if (CLEANUP_P (t))
	{
	  stmt = begin_try_block ();
	  RECUR (TRY_STMTS (t));
	  finish_cleanup_try_block (stmt);
	  finish_cleanup (RECUR (TRY_HANDLERS (t)), stmt);
	}
      else
	{
	  tree compound_stmt = NULL_TREE;

	  if (FN_TRY_BLOCK_P (t))
	    stmt = begin_function_try_block (&compound_stmt);
	  else
	    stmt = begin_try_block ();

	  RECUR (TRY_STMTS (t));

	  if (FN_TRY_BLOCK_P (t))
	    finish_function_try_block (stmt);
	  else
	    finish_try_block (stmt);

	  RECUR (TRY_HANDLERS (t));
	  if (FN_TRY_BLOCK_P (t))
	    finish_function_handler_sequence (stmt, compound_stmt);
	  else
	    finish_handler_sequence (stmt);
	}
      break;

    case HANDLER:
      {
	tree decl = HANDLER_PARMS (t);

	if (decl)
	  {
	    decl = tsubst (decl, args, complain, in_decl);
	    /* Prevent instantiate_decl from trying to instantiate
	       this variable.  We've already done all that needs to be
	       done.  */
	    if (decl != error_mark_node)
	      DECL_TEMPLATE_INSTANTIATED (decl) = 1;
	  }
	stmt = begin_handler ();
	finish_handler_parms (decl, stmt);
	RECUR (HANDLER_BODY (t));
	finish_handler (stmt);
      }
      break;

    case TAG_DEFN:
      tmp = tsubst (TREE_TYPE (t), args, complain, NULL_TREE);
      if (CLASS_TYPE_P (tmp))
	{
	  /* Local classes are not independent templates; they are
	     instantiated along with their containing function.  And this
	     way we don't have to deal with pushing out of one local class
	     to instantiate a member of another local class.  */
	  /* Closures are handled by the LAMBDA_EXPR.  */
	  gcc_assert (!LAMBDA_TYPE_P (TREE_TYPE (t)));
	  complete_type (tmp);
	  if (dependent_type_p (tmp))
	    {
	      /* This is a partial instantiation, try again when full.  */
	      add_stmt (build_min (TAG_DEFN, tmp));
	      break;
	    }
	  tree save_ccp = current_class_ptr;
	  tree save_ccr = current_class_ref;
	  for (tree fld = TYPE_FIELDS (tmp); fld; fld = DECL_CHAIN (fld))
	    if ((VAR_P (fld)
		 || (TREE_CODE (fld) == FUNCTION_DECL
		     && !DECL_ARTIFICIAL (fld)))
		&& DECL_TEMPLATE_INSTANTIATION (fld))
	      instantiate_decl (fld, /*defer_ok=*/false,
				/*expl_inst_class=*/false);
	    else if (TREE_CODE (fld) == FIELD_DECL)
	      maybe_instantiate_nsdmi_init (fld, tf_warning_or_error);
	  current_class_ptr = save_ccp;
	  current_class_ref = save_ccr;
	}
      break;

    case STATIC_ASSERT:
      {
	tree condition;

	++c_inhibit_evaluation_warnings;
	condition = tsubst_expr (STATIC_ASSERT_CONDITION (t), args,
				 complain, in_decl);
	--c_inhibit_evaluation_warnings;

        finish_static_assert (condition,
                              STATIC_ASSERT_MESSAGE (t),
                              STATIC_ASSERT_SOURCE_LOCATION (t),
			      /*member_p=*/false, /*show_expr_p=*/true);
      }
      break;

    case OACC_KERNELS:
    case OACC_PARALLEL:
    case OACC_SERIAL:
      tmp = tsubst_omp_clauses (OMP_CLAUSES (t), C_ORT_ACC, args, complain,
				in_decl);
      stmt = begin_omp_parallel ();
      RECUR (OMP_BODY (t));
      finish_omp_construct (TREE_CODE (t), stmt, tmp);
      break;

    case OMP_PARALLEL:
      r = push_omp_privatization_clauses (OMP_PARALLEL_COMBINED (t));
      tmp = tsubst_omp_clauses (OMP_PARALLEL_CLAUSES (t), C_ORT_OMP, args,
				complain, in_decl);
      if (OMP_PARALLEL_COMBINED (t))
	omp_parallel_combined_clauses = &tmp;
      stmt = begin_omp_parallel ();
      RECUR (OMP_PARALLEL_BODY (t));
      gcc_assert (omp_parallel_combined_clauses == NULL);
      OMP_PARALLEL_COMBINED (finish_omp_parallel (tmp, stmt))
	= OMP_PARALLEL_COMBINED (t);
      pop_omp_privatization_clauses (r);
      break;

    case OMP_TASK:
      if (OMP_TASK_BODY (t) == NULL_TREE)
	{
	  tmp = tsubst_omp_clauses (OMP_TASK_CLAUSES (t), C_ORT_OMP, args,
				    complain, in_decl);
	  t = copy_node (t);
	  OMP_TASK_CLAUSES (t) = tmp;
	  add_stmt (t);
	  break;
	}
      r = push_omp_privatization_clauses (false);
      tmp = tsubst_omp_clauses (OMP_TASK_CLAUSES (t), C_ORT_OMP, args,
				complain, in_decl);
      stmt = begin_omp_task ();
      RECUR (OMP_TASK_BODY (t));
      finish_omp_task (tmp, stmt);
      pop_omp_privatization_clauses (r);
      break;

    case OMP_FOR:
    case OMP_LOOP:
    case OMP_SIMD:
    case OMP_DISTRIBUTE:
    case OMP_TASKLOOP:
    case OACC_LOOP:
      {
	tree clauses, body, pre_body;
	tree declv = NULL_TREE, initv = NULL_TREE, condv = NULL_TREE;
	tree orig_declv = NULL_TREE;
	tree incrv = NULL_TREE;
	enum c_omp_region_type ort = C_ORT_OMP;
	bool any_range_for = false;
	int i;

	if (TREE_CODE (t) == OACC_LOOP)
	  ort = C_ORT_ACC;

	r = push_omp_privatization_clauses (OMP_FOR_INIT (t) == NULL_TREE);
	clauses = tsubst_omp_clauses (OMP_FOR_CLAUSES (t), ort, args, complain,
				      in_decl);
	if (OMP_FOR_INIT (t) != NULL_TREE)
	  {
	    declv = make_tree_vec (TREE_VEC_LENGTH (OMP_FOR_INIT (t)));
	    if (OMP_FOR_ORIG_DECLS (t))
	      orig_declv = make_tree_vec (TREE_VEC_LENGTH (OMP_FOR_INIT (t)));
	    initv = make_tree_vec (TREE_VEC_LENGTH (OMP_FOR_INIT (t)));
	    condv = make_tree_vec (TREE_VEC_LENGTH (OMP_FOR_INIT (t)));
	    incrv = make_tree_vec (TREE_VEC_LENGTH (OMP_FOR_INIT (t)));
	  }

	keep_next_level (true);
	stmt = begin_omp_structured_block ();

	pre_body = push_stmt_list ();
	RECUR (OMP_FOR_PRE_BODY (t));
	pre_body = pop_stmt_list (pre_body);

	if (OMP_FOR_INIT (t) != NULL_TREE)
	  for (i = 0; i < TREE_VEC_LENGTH (OMP_FOR_INIT (t)); i++)
	    any_range_for
	      |= tsubst_omp_for_iterator (t, i, declv, orig_declv, initv,
					  condv, incrv, &clauses, args,
					  complain, in_decl);
	omp_parallel_combined_clauses = NULL;

	if (any_range_for)
	  {
	    gcc_assert (orig_declv);
	    body = begin_omp_structured_block ();
	    for (i = 0; i < TREE_VEC_LENGTH (OMP_FOR_INIT (t)); i++)
	      if (TREE_VEC_ELT (orig_declv, i) != TREE_VEC_ELT (declv, i)
		  && TREE_CODE (TREE_VEC_ELT (orig_declv, i)) == TREE_LIST
		  && TREE_CHAIN (TREE_VEC_ELT (orig_declv, i)))
		cp_finish_omp_range_for (TREE_VEC_ELT (orig_declv, i),
					 TREE_VEC_ELT (declv, i));
	  }
	else
	  body = push_stmt_list ();
	RECUR (OMP_FOR_BODY (t));
	if (any_range_for)
	  body = finish_omp_structured_block (body);
	else
	  body = pop_stmt_list (body);

	if (OMP_FOR_INIT (t) != NULL_TREE)
	  t = finish_omp_for (EXPR_LOCATION (t), TREE_CODE (t), declv,
			      orig_declv, initv, condv, incrv, body, pre_body,
			      NULL, clauses);
	else
	  {
	    t = make_node (TREE_CODE (t));
	    TREE_TYPE (t) = void_type_node;
	    OMP_FOR_BODY (t) = body;
	    OMP_FOR_PRE_BODY (t) = pre_body;
	    OMP_FOR_CLAUSES (t) = clauses;
	    SET_EXPR_LOCATION (t, EXPR_LOCATION (t));
	    add_stmt (t);
	  }

	add_stmt (finish_omp_for_block (finish_omp_structured_block (stmt),
					t));
	pop_omp_privatization_clauses (r);
      }
      break;

    case OMP_SECTIONS:
    case OMP_MASKED:
      omp_parallel_combined_clauses = NULL;
      /* FALLTHRU */
    case OMP_SINGLE:
    case OMP_SCOPE:
    case OMP_TEAMS:
    case OMP_CRITICAL:
    case OMP_TASKGROUP:
    case OMP_SCAN:
      r = push_omp_privatization_clauses (TREE_CODE (t) == OMP_TEAMS
					  && OMP_TEAMS_COMBINED (t));
      tmp = tsubst_omp_clauses (OMP_CLAUSES (t), C_ORT_OMP, args, complain,
				in_decl);
      if (TREE_CODE (t) == OMP_TEAMS)
	{
	  keep_next_level (true);
	  stmt = begin_omp_structured_block ();
	  RECUR (OMP_BODY (t));
	  stmt = finish_omp_structured_block (stmt);
	}
      else
	{
	  stmt = push_stmt_list ();
	  RECUR (OMP_BODY (t));
	  stmt = pop_stmt_list (stmt);
	}

      if (TREE_CODE (t) == OMP_CRITICAL
	  && tmp != NULL_TREE
	  && integer_nonzerop (OMP_CLAUSE_HINT_EXPR (tmp)))
	{
	  error_at (OMP_CLAUSE_LOCATION (tmp),
		    "%<#pragma omp critical%> with %<hint%> clause requires "
		    "a name, except when %<omp_sync_hint_none%> is used");
	  RETURN (error_mark_node);
	}
      t = copy_node (t);
      OMP_BODY (t) = stmt;
      OMP_CLAUSES (t) = tmp;
      add_stmt (t);
      pop_omp_privatization_clauses (r);
      break;

    case OMP_DEPOBJ:
      r = RECUR (OMP_DEPOBJ_DEPOBJ (t));
      if (OMP_DEPOBJ_CLAUSES (t) && OMP_DEPOBJ_CLAUSES (t) != error_mark_node)
	{
	  enum omp_clause_depend_kind kind = OMP_CLAUSE_DEPEND_INVALID;
	  if (TREE_CODE (OMP_DEPOBJ_CLAUSES (t)) == OMP_CLAUSE)
	    {
	      tmp = tsubst_omp_clauses (OMP_DEPOBJ_CLAUSES (t), C_ORT_OMP,
					args, complain, in_decl);
	      if (tmp == NULL_TREE)
		tmp = error_mark_node;
	    }
	  else
	    {
	      kind = (enum omp_clause_depend_kind)
		     tree_to_uhwi (OMP_DEPOBJ_CLAUSES (t));
	      tmp = NULL_TREE;
	    }
	  finish_omp_depobj (EXPR_LOCATION (t), r, kind, tmp);
	}
      else
	finish_omp_depobj (EXPR_LOCATION (t), r,
			   OMP_CLAUSE_DEPEND_INVALID,
			   OMP_DEPOBJ_CLAUSES (t));
      break;

    case OACC_DATA:
    case OMP_TARGET_DATA:
    case OMP_TARGET:
      tmp = tsubst_omp_clauses (OMP_CLAUSES (t),
				TREE_CODE (t) == OACC_DATA
				? C_ORT_ACC
				: TREE_CODE (t) == OMP_TARGET
				? C_ORT_OMP_TARGET : C_ORT_OMP,
				args, complain, in_decl);
      keep_next_level (true);
      stmt = begin_omp_structured_block ();

      RECUR (OMP_BODY (t));
      stmt = finish_omp_structured_block (stmt);

      t = copy_node (t);
      OMP_BODY (t) = stmt;
      OMP_CLAUSES (t) = tmp;

      if (TREE_CODE (t) == OMP_TARGET)
	finish_omp_target_clauses (EXPR_LOCATION (t), OMP_BODY (t),
				   &OMP_CLAUSES (t));

      if (TREE_CODE (t) == OMP_TARGET && OMP_TARGET_COMBINED (t))
	{
	  tree teams = cp_walk_tree (&stmt, tsubst_find_omp_teams, NULL, NULL);
	  if (teams)
	    /* For combined target teams, ensure the num_teams and
	       thread_limit clause expressions are evaluated on the host,
	       before entering the target construct.  */
	    for (tree c = OMP_TEAMS_CLAUSES (teams);
		 c; c = OMP_CLAUSE_CHAIN (c))
	      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_NUM_TEAMS
		  || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_THREAD_LIMIT)
		for (int i = 0;
		     i <= (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_NUM_TEAMS); ++i)
		  if (OMP_CLAUSE_OPERAND (c, i)
		      && TREE_CODE (OMP_CLAUSE_OPERAND (c, i)) != INTEGER_CST)
		    {
		      tree expr = OMP_CLAUSE_OPERAND (c, i);
		      expr = force_target_expr (TREE_TYPE (expr), expr,
						tf_none);
		      if (expr == error_mark_node)
			continue;
		      tmp = TARGET_EXPR_SLOT (expr);
		      add_stmt (expr);
		      OMP_CLAUSE_OPERAND (c, i) = expr;
		      tree tc = build_omp_clause (OMP_CLAUSE_LOCATION (c),
						  OMP_CLAUSE_FIRSTPRIVATE);
		      OMP_CLAUSE_DECL (tc) = tmp;
		      OMP_CLAUSE_CHAIN (tc) = OMP_TARGET_CLAUSES (t);
		      OMP_TARGET_CLAUSES (t) = tc;
		    }
	}
      add_stmt (t);
      break;

    case OACC_DECLARE:
      t = copy_node (t);
      tmp = tsubst_omp_clauses (OACC_DECLARE_CLAUSES (t), C_ORT_ACC, args,
				complain, in_decl);
      OACC_DECLARE_CLAUSES (t) = tmp;
      add_stmt (t);
      break;

    case OMP_TARGET_UPDATE:
    case OMP_TARGET_ENTER_DATA:
    case OMP_TARGET_EXIT_DATA:
      tmp = tsubst_omp_clauses (OMP_STANDALONE_CLAUSES (t), C_ORT_OMP, args,
				complain, in_decl);
      t = copy_node (t);
      OMP_STANDALONE_CLAUSES (t) = tmp;
      add_stmt (t);
      break;

    case OACC_CACHE:
    case OACC_ENTER_DATA:
    case OACC_EXIT_DATA:
    case OACC_UPDATE:
      tmp = tsubst_omp_clauses (OMP_STANDALONE_CLAUSES (t), C_ORT_ACC, args,
				complain, in_decl);
      t = copy_node (t);
      OMP_STANDALONE_CLAUSES (t) = tmp;
      add_stmt (t);
      break;

    case OMP_ORDERED:
      tmp = tsubst_omp_clauses (OMP_ORDERED_CLAUSES (t), C_ORT_OMP, args,
				complain, in_decl);
      if (OMP_BODY (t))
	{
	  stmt = push_stmt_list ();
	  RECUR (OMP_BODY (t));
	  stmt = pop_stmt_list (stmt);
	}
      else
	stmt = NULL_TREE;

      t = copy_node (t);
      OMP_BODY (t) = stmt;
      OMP_ORDERED_CLAUSES (t) = tmp;
      add_stmt (t);
      break;

    case OMP_MASTER:
      omp_parallel_combined_clauses = NULL;
      /* FALLTHRU */
    case OMP_SECTION:
      stmt = push_stmt_list ();
      RECUR (OMP_BODY (t));
      stmt = pop_stmt_list (stmt);

      t = copy_node (t);
      OMP_BODY (t) = stmt;
      add_stmt (t);
      break;

    case OMP_ATOMIC:
      gcc_assert (OMP_ATOMIC_DEPENDENT_P (t));
      tmp = NULL_TREE;
      if (TREE_CODE (TREE_OPERAND (t, 0)) == OMP_CLAUSE)
	tmp = tsubst_omp_clauses (TREE_OPERAND (t, 0), C_ORT_OMP, args,
				  complain, in_decl);
      if (TREE_CODE (TREE_OPERAND (t, 1)) != MODIFY_EXPR)
	{
	  tree op1 = TREE_OPERAND (t, 1);
	  tree rhs1 = NULL_TREE;
	  tree r = NULL_TREE;
	  tree lhs, rhs;
	  if (TREE_CODE (op1) == COMPOUND_EXPR)
	    {
	      rhs1 = RECUR (TREE_OPERAND (op1, 0));
	      op1 = TREE_OPERAND (op1, 1);
	    }
	  if (TREE_CODE (op1) == COND_EXPR)
	    {
	      gcc_assert (rhs1 == NULL_TREE);
	      tree c = TREE_OPERAND (op1, 0);
	      if (TREE_CODE (c) == MODIFY_EXPR)
		{
		  r = RECUR (TREE_OPERAND (c, 0));
		  c = TREE_OPERAND (c, 1);
		}
	      gcc_assert (TREE_CODE (c) == EQ_EXPR);
	      rhs = RECUR (TREE_OPERAND (c, 1));
	      lhs = RECUR (TREE_OPERAND (op1, 2));
	      rhs1 = RECUR (TREE_OPERAND (op1, 1));
	    }
	  else
	    {
	      lhs = RECUR (TREE_OPERAND (op1, 0));
	      rhs = RECUR (TREE_OPERAND (op1, 1));
	    }
	  finish_omp_atomic (EXPR_LOCATION (t), OMP_ATOMIC, TREE_CODE (op1),
			     lhs, rhs, NULL_TREE, NULL_TREE, rhs1, r,
			     tmp, OMP_ATOMIC_MEMORY_ORDER (t),
			     OMP_ATOMIC_WEAK (t));
	}
      else
	{
	  tree op1 = TREE_OPERAND (t, 1);
	  tree v = NULL_TREE, lhs, rhs = NULL_TREE, lhs1 = NULL_TREE;
	  tree rhs1 = NULL_TREE, r = NULL_TREE;
	  enum tree_code code = TREE_CODE (TREE_OPERAND (op1, 1));
	  enum tree_code opcode = NOP_EXPR;
	  if (code == OMP_ATOMIC_READ)
	    {
	      v = RECUR (TREE_OPERAND (op1, 0));
	      lhs = RECUR (TREE_OPERAND (TREE_OPERAND (op1, 1), 0));
	    }
	  else if (code == OMP_ATOMIC_CAPTURE_OLD
		   || code == OMP_ATOMIC_CAPTURE_NEW)
	    {
	      tree op11 = TREE_OPERAND (TREE_OPERAND (op1, 1), 1);
	      v = RECUR (TREE_OPERAND (op1, 0));
	      lhs1 = RECUR (TREE_OPERAND (TREE_OPERAND (op1, 1), 0));
	      if (TREE_CODE (op11) == COMPOUND_EXPR)
		{
		  rhs1 = RECUR (TREE_OPERAND (op11, 0));
		  op11 = TREE_OPERAND (op11, 1);
		}
	      if (TREE_CODE (op11) == COND_EXPR)
		{
		  gcc_assert (rhs1 == NULL_TREE);
		  tree c = TREE_OPERAND (op11, 0);
		  if (TREE_CODE (c) == MODIFY_EXPR)
		    {
		      r = RECUR (TREE_OPERAND (c, 0));
		      c = TREE_OPERAND (c, 1);
		    }
		  gcc_assert (TREE_CODE (c) == EQ_EXPR);
		  rhs = RECUR (TREE_OPERAND (c, 1));
		  lhs = RECUR (TREE_OPERAND (op11, 2));
		  rhs1 = RECUR (TREE_OPERAND (op11, 1));
		}
	      else
		{
		  lhs = RECUR (TREE_OPERAND (op11, 0));
		  rhs = RECUR (TREE_OPERAND (op11, 1));
		}
	      opcode = TREE_CODE (op11);
	      if (opcode == MODIFY_EXPR)
		opcode = NOP_EXPR;
	    }
	  else
	    {
	      code = OMP_ATOMIC;
	      lhs = RECUR (TREE_OPERAND (op1, 0));
	      rhs = RECUR (TREE_OPERAND (op1, 1));
	    }
	  finish_omp_atomic (EXPR_LOCATION (t), code, opcode, lhs, rhs, v,
			     lhs1, rhs1, r, tmp,
			     OMP_ATOMIC_MEMORY_ORDER (t), OMP_ATOMIC_WEAK (t));
	}
      break;

    case TRANSACTION_EXPR:
      {
	int flags = 0;
	flags |= (TRANSACTION_EXPR_OUTER (t) ? TM_STMT_ATTR_OUTER : 0);
	flags |= (TRANSACTION_EXPR_RELAXED (t) ? TM_STMT_ATTR_RELAXED : 0);

        if (TRANSACTION_EXPR_IS_STMT (t))
          {
	    tree body = TRANSACTION_EXPR_BODY (t);
	    tree noex = NULL_TREE;
	    if (TREE_CODE (body) == MUST_NOT_THROW_EXPR)
	      {
		noex = MUST_NOT_THROW_COND (body);
		if (noex == NULL_TREE)
		  noex = boolean_true_node;
		body = TREE_OPERAND (body, 0);
	      }
            stmt = begin_transaction_stmt (input_location, NULL, flags);
            RECUR (body);
            finish_transaction_stmt (stmt, NULL, flags, RECUR (noex));
          }
        else
          {
            stmt = build_transaction_expr (EXPR_LOCATION (t),
					   RECUR (TRANSACTION_EXPR_BODY (t)),
					   flags, NULL_TREE);
            RETURN (stmt);
          }
      }
      break;

    case MUST_NOT_THROW_EXPR:
      {
	tree op0 = RECUR (TREE_OPERAND (t, 0));
	tree cond = RECUR (MUST_NOT_THROW_COND (t));
	RETURN (build_must_not_throw_expr (op0, cond));
      }

    case EXPR_PACK_EXPANSION:
      error ("invalid use of pack expansion expression");
      RETURN (error_mark_node);

    case NONTYPE_ARGUMENT_PACK:
      error ("use %<...%> to expand argument pack");
      RETURN (error_mark_node);

    case COMPOUND_EXPR:
      tmp = RECUR (TREE_OPERAND (t, 0));
      if (tmp == NULL_TREE)
	/* If the first operand was a statement, we're done with it.  */
	RETURN (RECUR (TREE_OPERAND (t, 1)));
      RETURN (build_x_compound_expr (EXPR_LOCATION (t), tmp,
				    RECUR (TREE_OPERAND (t, 1)),
				    templated_operator_saved_lookups (t),
				    complain));

    case ANNOTATE_EXPR:
      tmp = RECUR (TREE_OPERAND (t, 0));
      RETURN (build3_loc (EXPR_LOCATION (t), ANNOTATE_EXPR,
			  TREE_TYPE (tmp), tmp,
			  RECUR (TREE_OPERAND (t, 1)),
			  RECUR (TREE_OPERAND (t, 2))));

    case PREDICT_EXPR:
      RETURN (add_stmt (copy_node (t)));

    default:
      gcc_assert (!STATEMENT_CODE_P (TREE_CODE (t)));

      RETURN (tsubst_copy_and_build (t, args, complain, in_decl));
    }

  RETURN (NULL_TREE);
 out:
  input_location = loc;
  return r;
#undef RECUR
#undef RETURN
}

/* Instantiate the special body of the artificial DECL_OMP_DECLARE_REDUCTION
   function.  For description of the body see comment above
   cp_parser_omp_declare_reduction_exprs.  */

static void
tsubst_omp_udr (tree t, tree args, tsubst_flags_t complain, tree in_decl)
{
  if (t == NULL_TREE || t == error_mark_node)
    return;

  gcc_assert (TREE_CODE (t) == STATEMENT_LIST && current_function_decl);

  tree_stmt_iterator tsi;
  int i;
  tree stmts[7];
  memset (stmts, 0, sizeof stmts);
  for (i = 0, tsi = tsi_start (t);
       i < 7 && !tsi_end_p (tsi);
       i++, tsi_next (&tsi))
    stmts[i] = tsi_stmt (tsi);
  gcc_assert (tsi_end_p (tsi));

  if (i >= 3)
    {
      gcc_assert (TREE_CODE (stmts[0]) == DECL_EXPR
		  && TREE_CODE (stmts[1]) == DECL_EXPR);
      tree omp_out = tsubst (DECL_EXPR_DECL (stmts[0]),
			     args, complain, in_decl);
      tree omp_in = tsubst (DECL_EXPR_DECL (stmts[1]),
			    args, complain, in_decl);
      /* tsubsting a local var_decl leaves DECL_CONTEXT null, as we
	 expect to be pushing it.  */
      DECL_CONTEXT (omp_out) = current_function_decl;
      DECL_CONTEXT (omp_in) = current_function_decl;
      keep_next_level (true);
      tree block = begin_omp_structured_block ();
      tsubst_expr (stmts[2], args, complain, in_decl);
      block = finish_omp_structured_block (block);
      block = maybe_cleanup_point_expr_void (block);
      add_decl_expr (omp_out);
      copy_warning (omp_out, DECL_EXPR_DECL (stmts[0]));
      add_decl_expr (omp_in);
      finish_expr_stmt (block);
    }
  if (i >= 6)
    {
      gcc_assert (TREE_CODE (stmts[3]) == DECL_EXPR
		  && TREE_CODE (stmts[4]) == DECL_EXPR);
      tree omp_priv = tsubst (DECL_EXPR_DECL (stmts[3]),
			      args, complain, in_decl);
      tree omp_orig = tsubst (DECL_EXPR_DECL (stmts[4]),
			      args, complain, in_decl);
      DECL_CONTEXT (omp_priv) = current_function_decl;
      DECL_CONTEXT (omp_orig) = current_function_decl;
      keep_next_level (true);
      tree block = begin_omp_structured_block ();
      tsubst_expr (stmts[5], args, complain, in_decl);
      block = finish_omp_structured_block (block);
      block = maybe_cleanup_point_expr_void (block);
      cp_walk_tree (&block, cp_remove_omp_priv_cleanup_stmt, omp_priv, NULL);
      add_decl_expr (omp_priv);
      add_decl_expr (omp_orig);
      finish_expr_stmt (block);
      if (i == 7)
	add_decl_expr (omp_orig);
    }
}

/* T is a postfix-expression that is not being used in a function
   call.  Return the substituted version of T.  */

static tree
tsubst_non_call_postfix_expression (tree t, tree args,
				    tsubst_flags_t complain,
				    tree in_decl)
{
  if (TREE_CODE (t) == SCOPE_REF)
    t = tsubst_qualified_id (t, args, complain, in_decl,
			     /*done=*/false, /*address_p=*/false);
  else
    t = tsubst_copy_and_build (t, args, complain, in_decl);

  return t;
}

/* Subroutine of tsubst_lambda_expr: add the FIELD/INIT capture pair to the
   LAMBDA_EXPR_CAPTURE_LIST passed in LIST.  Do deduction for a previously
   dependent init-capture.  EXPLICIT_P is true if the original list had
   explicit captures.  */

static void
prepend_one_capture (tree field, tree init, tree &list, bool explicit_p,
		     tsubst_flags_t complain)
{
  if (tree auto_node = type_uses_auto (TREE_TYPE (field)))
    {
      tree type = NULL_TREE;
      if (!init)
	{
	  if (complain & tf_error)
	    error ("empty initializer in lambda init-capture");
	  init = error_mark_node;
	}
      else if (TREE_CODE (init) == TREE_LIST)
	init = build_x_compound_expr_from_list (init, ELK_INIT, complain);
      if (!type)
	type = do_auto_deduction (TREE_TYPE (field), init, auto_node, complain);
      TREE_TYPE (field) = type;
      cp_apply_type_quals_to_decl (cp_type_quals (type), field);
    }
  list = tree_cons (field, init, list);
  LAMBDA_CAPTURE_EXPLICIT_P (list) = explicit_p;
}

/* T is a LAMBDA_EXPR.  Generate a new LAMBDA_EXPR for the current
   instantiation context.  Instantiating a pack expansion containing a lambda
   might result in multiple lambdas all based on the same lambda in the
   template.  */

tree
tsubst_lambda_expr (tree t, tree args, tsubst_flags_t complain, tree in_decl)
{
  tree oldfn = lambda_function (t);
  in_decl = oldfn;

  tree r = build_lambda_expr ();

  LAMBDA_EXPR_LOCATION (r)
    = LAMBDA_EXPR_LOCATION (t);
  LAMBDA_EXPR_DEFAULT_CAPTURE_MODE (r)
    = LAMBDA_EXPR_DEFAULT_CAPTURE_MODE (t);
  LAMBDA_EXPR_MUTABLE_P (r) = LAMBDA_EXPR_MUTABLE_P (t);
  if (tree ti = LAMBDA_EXPR_REGEN_INFO (t))
    LAMBDA_EXPR_REGEN_INFO (r)
      = build_template_info (t, add_to_template_args (TI_ARGS (ti),
						      preserve_args (args)));
  else
    LAMBDA_EXPR_REGEN_INFO (r)
      = build_template_info (t, preserve_args (args));

  gcc_assert (LAMBDA_EXPR_THIS_CAPTURE (t) == NULL_TREE
	      && LAMBDA_EXPR_PENDING_PROXIES (t) == NULL);

  vec<tree,va_gc>* field_packs = NULL;

  for (tree cap = LAMBDA_EXPR_CAPTURE_LIST (t); cap;
       cap = TREE_CHAIN (cap))
    {
      tree ofield = TREE_PURPOSE (cap);
      tree init = TREE_VALUE (cap);
      if (PACK_EXPANSION_P (init))
	init = tsubst_pack_expansion (init, args, complain, in_decl);
      else
	init = tsubst_copy_and_build (init, args, complain, in_decl);

      if (init == error_mark_node)
	return error_mark_node;

      if (init && TREE_CODE (init) == TREE_LIST)
	init = build_x_compound_expr_from_list (init, ELK_INIT, complain);

      if (!processing_template_decl
	  && init && TREE_CODE (init) != TREE_VEC
	  && variably_modified_type_p (TREE_TYPE (init), NULL_TREE))
	{
	  /* For a VLA, simply tsubsting the field type won't work, we need to
	     go through add_capture again.  XXX do we want to do this for all
	     captures?  */
	  tree name = (get_identifier
		       (IDENTIFIER_POINTER (DECL_NAME (ofield)) + 2));
	  tree ftype = TREE_TYPE (ofield);
	  bool by_ref = (TYPE_REF_P (ftype)
			 || (TREE_CODE (ftype) == DECLTYPE_TYPE
			     && DECLTYPE_FOR_REF_CAPTURE (ftype)));
	  add_capture (r, name, init, by_ref, !DECL_NORMAL_CAPTURE_P (ofield));
	  continue;
	}

      if (PACK_EXPANSION_P (ofield))
	ofield = PACK_EXPANSION_PATTERN (ofield);
      tree field = tsubst_decl (ofield, args, complain);

      if (DECL_PACK_P (ofield) && !DECL_NORMAL_CAPTURE_P (ofield))
	{
	  /* Remember these for when we've pushed local_specializations.  */
	  vec_safe_push (field_packs, ofield);
	  vec_safe_push (field_packs, field);
	}

      if (field == error_mark_node)
	return error_mark_node;

      if (TREE_CODE (field) == TREE_VEC)
	{
	  int len = TREE_VEC_LENGTH (field);
	  gcc_assert (TREE_CODE (init) == TREE_VEC
		      && TREE_VEC_LENGTH (init) == len);
	  for (int i = 0; i < len; ++i)
	    prepend_one_capture (TREE_VEC_ELT (field, i),
				 TREE_VEC_ELT (init, i),
				 LAMBDA_EXPR_CAPTURE_LIST (r),
				 LAMBDA_CAPTURE_EXPLICIT_P (cap),
				 complain);
	}
      else
	{
	  prepend_one_capture (field, init, LAMBDA_EXPR_CAPTURE_LIST (r),
			       LAMBDA_CAPTURE_EXPLICIT_P (cap), complain);

	  if (id_equal (DECL_NAME (field), "__this"))
	    LAMBDA_EXPR_THIS_CAPTURE (r) = field;
	}
    }

  tree type = begin_lambda_type (r);
  if (type == error_mark_node)
    return error_mark_node;

  if (LAMBDA_EXPR_EXTRA_SCOPE (t))
    record_lambda_scope (r);
  else if (TYPE_NAMESPACE_SCOPE_P (TREE_TYPE (t)))
    /* If we're pushed into another scope (PR105652), fix it.  */
    TYPE_CONTEXT (type) = DECL_CONTEXT (TYPE_NAME (type))
      = TYPE_CONTEXT (TREE_TYPE (t));
  record_lambda_scope_discriminator (r);

  /* Do this again now that LAMBDA_EXPR_EXTRA_SCOPE is set.  */
  determine_visibility (TYPE_NAME (type));

  register_capture_members (LAMBDA_EXPR_CAPTURE_LIST (r));

  tree oldtmpl = (generic_lambda_fn_p (oldfn)
		  ? DECL_TI_TEMPLATE (oldfn)
		  : NULL_TREE);

  tree tparms = NULL_TREE;
  if (oldtmpl)
    tparms = tsubst_template_parms (DECL_TEMPLATE_PARMS (oldtmpl), args, complain);

  tree fntype = static_fn_type (oldfn);

  tree saved_ctp = current_template_parms;
  if (oldtmpl)
    {
      ++processing_template_decl;
      current_template_parms = tparms;
    }
  fntype = tsubst (fntype, args, complain, in_decl);
  if (oldtmpl)
    {
      current_template_parms = saved_ctp;
      --processing_template_decl;
    }

  if (fntype == error_mark_node)
    r = error_mark_node;
  else
    {
      /* The body of a lambda-expression is not a subexpression of the
	 enclosing expression.  Parms are to have DECL_CHAIN tsubsted,
	 which would be skipped if cp_unevaluated_operand.  */
      cp_evaluated ev;

      /* Fix the type of 'this'.  */
      fntype = build_memfn_type (fntype, type,
				 type_memfn_quals (fntype),
				 type_memfn_rqual (fntype));
      tree inst = (oldtmpl
		   ? tsubst_template_decl (oldtmpl, args, complain,
					   fntype, tparms)
		   : tsubst_function_decl (oldfn, args, complain, fntype));
      if (inst == error_mark_node)
	{
	  r = error_mark_node;
	  goto out;
	}
      finish_member_declaration (inst);
      record_lambda_scope_sig_discriminator (r, inst);

      tree fn = oldtmpl ? DECL_TEMPLATE_RESULT (inst) : inst;

      /* Let finish_function set this.  */
      DECL_DECLARED_CONSTEXPR_P (fn) = false;

      bool nested = cfun;
      if (nested)
	push_function_context ();
      else
	/* Still increment function_depth so that we don't GC in the
	   middle of an expression.  */
	++function_depth;

      local_specialization_stack s (lss_copy);

      bool save_in_consteval_if_p = in_consteval_if_p;
      in_consteval_if_p = false;

      tree body = start_lambda_function (fn, r);

      /* Now record them for lookup_init_capture_pack.  */
      int fplen = vec_safe_length (field_packs);
      for (int i = 0; i < fplen; )
	{
	  tree pack = (*field_packs)[i++];
	  tree inst = (*field_packs)[i++];
	  register_local_specialization (inst, pack);
	}
      release_tree_vector (field_packs);

      register_parameter_specializations (oldfn, fn);

      if (oldtmpl)
	{
	  /* We might not partially instantiate some parts of the function, so
	     copy these flags from the original template.  */
	  language_function *ol = DECL_STRUCT_FUNCTION (oldfn)->language;
	  current_function_returns_value = ol->returns_value;
	  current_function_returns_null = ol->returns_null;
	  current_function_returns_abnormally = ol->returns_abnormally;
	  current_function_infinite_loop = ol->infinite_loop;
	}

      /* [temp.deduct] A lambda-expression appearing in a function type or a
	 template parameter is not considered part of the immediate context for
	 the purposes of template argument deduction. */
      complain = tf_warning_or_error;

      tree saved = DECL_SAVED_TREE (oldfn);
      if (TREE_CODE (saved) == BIND_EXPR && BIND_EXPR_BODY_BLOCK (saved))
	/* We already have a body block from start_lambda_function, we don't
	   need another to confuse NRV (91217).  */
	saved = BIND_EXPR_BODY (saved);

      tsubst_expr (saved, args, complain, r);

      finish_lambda_function (body);

      in_consteval_if_p = save_in_consteval_if_p;

      if (nested)
	pop_function_context ();
      else
	--function_depth;

      /* The capture list was built up in reverse order; fix that now.  */
      LAMBDA_EXPR_CAPTURE_LIST (r)
	= nreverse (LAMBDA_EXPR_CAPTURE_LIST (r));

      LAMBDA_EXPR_THIS_CAPTURE (r) = NULL_TREE;

      maybe_add_lambda_conv_op (type);
    }

out:
  finish_struct (type, /*attr*/NULL_TREE);

  insert_pending_capture_proxies ();

  return r;
}

/* Subroutine of maybe_fold_fn_template_args.  */

static bool
fold_targs_r (tree targs, tsubst_flags_t complain)
{
  int len = TREE_VEC_LENGTH (targs);
  for (int i = 0; i < len; ++i)
    {
      tree &elt = TREE_VEC_ELT (targs, i);
      if (!elt || TYPE_P (elt)
	  || TREE_CODE (elt) == TEMPLATE_DECL)
	continue;
      if (TREE_CODE (elt) == NONTYPE_ARGUMENT_PACK)
	{
	  if (!fold_targs_r (ARGUMENT_PACK_ARGS (elt), complain))
	    return false;
	}
      else if (/* We can only safely preevaluate scalar prvalues.  */
	       SCALAR_TYPE_P (TREE_TYPE (elt))
	       && !glvalue_p (elt)
	       && !TREE_CONSTANT (elt))
	{
	  elt = cxx_constant_value (elt, complain);
	  if (elt == error_mark_node)
	    return false;
	}
    }

  return true;
}

/* Try to do constant evaluation of any explicit template arguments in FN
   before overload resolution, to get any errors only once.  Return true iff
   we didn't have any problems folding.  */

static bool
maybe_fold_fn_template_args (tree fn, tsubst_flags_t complain)
{
  if (processing_template_decl || fn == NULL_TREE)
    return true;
  if (fn == error_mark_node)
    return false;
  if (TREE_CODE (fn) == OFFSET_REF
      || TREE_CODE (fn) == COMPONENT_REF)
    fn = TREE_OPERAND (fn, 1);
  if (BASELINK_P (fn))
    fn = BASELINK_FUNCTIONS (fn);
  if (TREE_CODE (fn) != TEMPLATE_ID_EXPR)
    return true;
  tree targs = TREE_OPERAND (fn, 1);
  if (targs == NULL_TREE)
    return true;
  if (targs == error_mark_node)
    return false;
  return fold_targs_r (targs, complain);
}

/* Helper function for tsubst_copy_and_build CALL_EXPR and ARRAY_REF
   handling.  */

static void
tsubst_copy_and_build_call_args (tree t, tree args, tsubst_flags_t complain,
				 tree in_decl, releasing_vec &call_args)
{
  unsigned int nargs = call_expr_nargs (t);
  for (unsigned int i = 0; i < nargs; ++i)
    {
      tree arg = CALL_EXPR_ARG (t, i);

      if (!PACK_EXPANSION_P (arg))
	vec_safe_push (call_args,
		       tsubst_copy_and_build (arg, args, complain, in_decl));
      else
	{
	  /* Expand the pack expansion and push each entry onto CALL_ARGS.  */
	  arg = tsubst_pack_expansion (arg, args, complain, in_decl);
	  if (TREE_CODE (arg) == TREE_VEC)
	    {
	      unsigned int len, j;

	      len = TREE_VEC_LENGTH (arg);
	      for (j = 0; j < len; ++j)
		{
		  tree value = TREE_VEC_ELT (arg, j);
		  if (value != NULL_TREE)
		    value = convert_from_reference (value);
		  vec_safe_push (call_args, value);
		}
	    }
	  else
	    /* A partial substitution.  Add one entry.  */
	    vec_safe_push (call_args, arg);
	}
    }
}

/* Like tsubst but deals with expressions and performs semantic
   analysis.  */

tree
tsubst_copy_and_build (tree t,
		       tree args,
		       tsubst_flags_t complain,
		       tree in_decl)
{
#define RETURN(EXP) do { retval = (EXP); goto out; } while(0)
#define RECUR(NODE)						\
  tsubst_copy_and_build (NODE, args, complain, in_decl)

  tree retval, op1;
  location_t save_loc;

  if (t == NULL_TREE || t == error_mark_node)
    return t;

  save_loc = input_location;
  if (location_t eloc = cp_expr_location (t))
    input_location = eloc;

  /* N3276 decltype magic only applies to calls at the top level or on the
     right side of a comma.  */
  tsubst_flags_t decltype_flag = (complain & tf_decltype);
  complain &= ~tf_decltype;

  switch (TREE_CODE (t))
    {
    case USING_DECL:
      t = DECL_NAME (t);
      /* Fall through.  */
    case IDENTIFIER_NODE:
      {
	tree decl;
	cp_id_kind idk;
	const char *error_msg;

	if (IDENTIFIER_CONV_OP_P (t))
	  {
	    tree new_type = tsubst (TREE_TYPE (t), args, complain, in_decl);
	    t = make_conv_op_name (new_type);
	  }

	/* Look up the name.  */
	decl = lookup_name (t);

	/* By convention, expressions use ERROR_MARK_NODE to indicate
	   failure, not NULL_TREE.  */
	if (decl == NULL_TREE)
	  decl = error_mark_node;

	decl = finish_id_expression (t, decl, NULL_TREE,
				     &idk,
				     /*i_c_e_p=*/false,
				     /*allow_i_c_e_p=*/true,
				     /*non_i_c_e_p=*/nullptr,
				     /*template_p=*/false,
				     /*done=*/true,
				     /*address_p=*/false,
				     /*template_arg_p=*/false,
				     &error_msg,
				     input_location);
	if (error_msg)
	  error (error_msg);
	if (identifier_p (decl))
	  {
	    if (complain & tf_error)
	      unqualified_name_lookup_error (decl);
	    decl = error_mark_node;
	  }
	RETURN (decl);
      }

    case TEMPLATE_ID_EXPR:
      {
	tree object;
	tree templ = tsubst_copy_and_build (TREE_OPERAND (t, 0), args,
					    complain, in_decl);
	tree targs = TREE_OPERAND (t, 1);

	if (targs)
	  targs = tsubst_template_args (targs, args, complain, in_decl);
	if (targs == error_mark_node)
	  RETURN (error_mark_node);

	if (TREE_CODE (templ) == SCOPE_REF)
	  {
	    tree name = TREE_OPERAND (templ, 1);
	    tree tid = lookup_template_function (name, targs);
	    TREE_OPERAND (templ, 1) = tid;
	    RETURN (templ);
	  }

	if (concept_definition_p (templ))
	  {
	    tree check = build_concept_check (templ, targs, complain);
	    if (check == error_mark_node)
	      RETURN (error_mark_node);

	    tree id = unpack_concept_check (check);

	    /* If we built a function concept check, return the underlying
	       template-id. So we can evaluate it as a function call.  */
	    if (function_concept_p (TREE_OPERAND (id, 0)))
	      RETURN (id);

	    RETURN (check);
	  }

	if (variable_template_p (templ))
	  {
	    tree r = lookup_and_finish_template_variable (templ, targs,
							  complain);
	    r = convert_from_reference (r);
	    r = maybe_wrap_with_location (r, EXPR_LOCATION (t));
	    RETURN (r);
	  }

	if (TREE_CODE (templ) == COMPONENT_REF)
	  {
	    object = TREE_OPERAND (templ, 0);
	    templ = TREE_OPERAND (templ, 1);
	  }
	else
	  object = NULL_TREE;

	tree tid = lookup_template_function (templ, targs);
	protected_set_expr_location (tid, EXPR_LOCATION (t));

	if (object)
	  RETURN (build3 (COMPONENT_REF, TREE_TYPE (tid),
			 object, tid, NULL_TREE));
	else if (identifier_p (templ))
	  {
	    /* C++20 P0846: we can encounter an IDENTIFIER_NODE here when
	       name lookup found nothing when parsing the template name.  */
	    gcc_assert (cxx_dialect >= cxx20 || seen_error ());
	    RETURN (tid);
	  }
	else
	  RETURN (baselink_for_fns (tid));
      }

    case INDIRECT_REF:
      {
	tree r = RECUR (TREE_OPERAND (t, 0));

	if (REFERENCE_REF_P (t))
	  {
	    /* A type conversion to reference type will be enclosed in
	       such an indirect ref, but the substitution of the cast
	       will have also added such an indirect ref.  */
	    r = convert_from_reference (r);
	  }
	else
	  r = build_x_indirect_ref (input_location, r, RO_UNARY_STAR,
				    templated_operator_saved_lookups (t),
				    complain|decltype_flag);

	if (REF_PARENTHESIZED_P (t))
	  r = force_paren_expr (r);

	RETURN (r);
      }

    case NOP_EXPR:
      {
	tree type = tsubst (TREE_TYPE (t), args, complain, in_decl);
	tree op0 = RECUR (TREE_OPERAND (t, 0));
	RETURN (build_nop (type, op0));
      }

    case IMPLICIT_CONV_EXPR:
      {
	tree type = tsubst (TREE_TYPE (t), args, complain, in_decl);
	tree expr = RECUR (TREE_OPERAND (t, 0));
	if (dependent_type_p (type) || type_dependent_expression_p (expr))
	  {
	    retval = copy_node (t);
	    TREE_TYPE (retval) = type;
	    TREE_OPERAND (retval, 0) = expr;
	    RETURN (retval);
	  }
	if (IMPLICIT_CONV_EXPR_NONTYPE_ARG (t))
	  /* We'll pass this to convert_nontype_argument again, we don't need
	     to actually perform any conversion here.  */
	  RETURN (expr);
	int flags = LOOKUP_IMPLICIT;
	if (IMPLICIT_CONV_EXPR_DIRECT_INIT (t))
	  flags = LOOKUP_NORMAL;
	if (IMPLICIT_CONV_EXPR_BRACED_INIT (t))
	  flags |= LOOKUP_NO_NARROWING;
	RETURN (perform_implicit_conversion_flags (type, expr, complain,
						  flags));
      }

    case CONVERT_EXPR:
      {
	tree type = tsubst (TREE_TYPE (t), args, complain, in_decl);
	tree op0 = RECUR (TREE_OPERAND (t, 0));
	if (op0 == error_mark_node)
	  RETURN (error_mark_node);
	RETURN (build1 (CONVERT_EXPR, type, op0));
      }

    case CAST_EXPR:
    case REINTERPRET_CAST_EXPR:
    case CONST_CAST_EXPR:
    case DYNAMIC_CAST_EXPR:
    case STATIC_CAST_EXPR:
      {
	tree type;
	tree op, r = NULL_TREE;

	tsubst_flags_t tcomplain = complain;
	if (TREE_CODE (t) == CAST_EXPR)
	  tcomplain |= tf_tst_ok;
	type = tsubst (TREE_TYPE (t), args, tcomplain, in_decl);

	op = RECUR (TREE_OPERAND (t, 0));

	warning_sentinel s(warn_useless_cast);
	warning_sentinel s2(warn_ignored_qualifiers);
	warning_sentinel s3(warn_int_in_bool_context);
	switch (TREE_CODE (t))
	  {
	  case CAST_EXPR:
	    r = build_functional_cast (input_location, type, op, complain);
	    break;
	  case REINTERPRET_CAST_EXPR:
	    r = build_reinterpret_cast (input_location, type, op, complain);
	    break;
	  case CONST_CAST_EXPR:
	    r = build_const_cast (input_location, type, op, complain);
	    break;
	  case DYNAMIC_CAST_EXPR:
	    r = build_dynamic_cast (input_location, type, op, complain);
	    break;
	  case STATIC_CAST_EXPR:
	    r = build_static_cast (input_location, type, op, complain);
	    if (IMPLICIT_RVALUE_P (t))
	      set_implicit_rvalue_p (r);
	    break;
	  default:
	    gcc_unreachable ();
	  }

	RETURN (r);
      }

    case BIT_CAST_EXPR:
      {
	tree type = tsubst (TREE_TYPE (t), args, complain, in_decl);
	tree op0 = RECUR (TREE_OPERAND (t, 0));
	RETURN (cp_build_bit_cast (EXPR_LOCATION (t), type, op0, complain));
      }

    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
      op1 = tsubst_non_call_postfix_expression (TREE_OPERAND (t, 0),
						args, complain, in_decl);
      RETURN (build_x_unary_op (input_location, TREE_CODE (t), op1,
				templated_operator_saved_lookups (t),
				complain|decltype_flag));

    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
    case ABS_EXPR:
    case TRUTH_NOT_EXPR:
    case UNARY_PLUS_EXPR:  /* Unary + */
    case REALPART_EXPR:
    case IMAGPART_EXPR:
      RETURN (build_x_unary_op (input_location, TREE_CODE (t),
				RECUR (TREE_OPERAND (t, 0)),
				templated_operator_saved_lookups (t),
				complain|decltype_flag));

    case EXCESS_PRECISION_EXPR:
      {
	tree type = tsubst (TREE_TYPE (t), args, complain, in_decl);
	tree op0 = RECUR (TREE_OPERAND (t, 0));
	if (TREE_CODE (op0) == EXCESS_PRECISION_EXPR)
	  RETURN (op0);
	RETURN (build1_loc (EXPR_LOCATION (t), EXCESS_PRECISION_EXPR,
			    type, op0));
      }

    case FIX_TRUNC_EXPR:
      /* convert_like should have created an IMPLICIT_CONV_EXPR.  */
      gcc_unreachable ();

    case ADDR_EXPR:
      op1 = TREE_OPERAND (t, 0);
      if (TREE_CODE (op1) == LABEL_DECL)
	RETURN (finish_label_address_expr (DECL_NAME (op1),
					  EXPR_LOCATION (op1)));
      if (TREE_CODE (op1) == SCOPE_REF)
	op1 = tsubst_qualified_id (op1, args, complain, in_decl,
				   /*done=*/true, /*address_p=*/true);
      else
	op1 = tsubst_non_call_postfix_expression (op1, args, complain,
						  in_decl);
      RETURN (build_x_unary_op (input_location, ADDR_EXPR, op1,
				templated_operator_saved_lookups (t),
				complain|decltype_flag));

    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case EXACT_DIV_EXPR:
    case BIT_AND_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case TRUNC_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case RSHIFT_EXPR:
    case LSHIFT_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case MAX_EXPR:
    case MIN_EXPR:
    case LE_EXPR:
    case GE_EXPR:
    case LT_EXPR:
    case GT_EXPR:
    case SPACESHIP_EXPR:
    case MEMBER_REF:
    case DOTSTAR_EXPR:
      {
	/* If either OP0 or OP1 was value- or type-dependent, suppress
	   warnings that depend on the range of the types involved.  */
	tree op0 = TREE_OPERAND (t, 0);
	tree op1 = TREE_OPERAND (t, 1);
	const bool was_dep = (dependent_operand_p (op0)
			      || dependent_operand_p (op1));
	op0 = RECUR (op0);
	op1 = RECUR (op1);

	warning_sentinel s1(warn_type_limits, was_dep);
	warning_sentinel s2(warn_div_by_zero, was_dep);
	warning_sentinel s3(warn_logical_op, was_dep);
	warning_sentinel s4(warn_tautological_compare, was_dep);
	warning_sentinel s5(warn_address, was_dep);

	tree r = build_x_binary_op
	  (input_location, TREE_CODE (t),
	   op0,
	   (warning_suppressed_p (TREE_OPERAND (t, 0))
	    ? ERROR_MARK
	    : TREE_CODE (TREE_OPERAND (t, 0))),
	   op1,
	   (warning_suppressed_p (TREE_OPERAND (t, 1))
	    ? ERROR_MARK
	    : TREE_CODE (TREE_OPERAND (t, 1))),
	   templated_operator_saved_lookups (t),
	   /*overload=*/NULL,
	   complain|decltype_flag);
	if (EXPR_P (r))
	  copy_warning (r, t);

	RETURN (r);
      }

    case POINTER_PLUS_EXPR:
      {
	tree op0 = RECUR (TREE_OPERAND (t, 0));
	if (op0 == error_mark_node)
	  RETURN (error_mark_node);
	tree op1 = RECUR (TREE_OPERAND (t, 1));
	if (op1 == error_mark_node)
	  RETURN (error_mark_node);
	RETURN (fold_build_pointer_plus (op0, op1));
      }

    case SCOPE_REF:
      RETURN (tsubst_qualified_id (t, args, complain, in_decl, /*done=*/true,
				  /*address_p=*/false));

    case BASELINK:
      RETURN (tsubst_baselink (t, current_nonlambda_class_type (),
			       args, complain, in_decl));

    case ARRAY_REF:
      op1 = tsubst_non_call_postfix_expression (TREE_OPERAND (t, 0),
						args, complain, in_decl);
      if (TREE_CODE (TREE_OPERAND (t, 1)) == CALL_EXPR
	  && (CALL_EXPR_FN (TREE_OPERAND (t, 1))
	      == ovl_op_identifier (ARRAY_REF)))
	{
	  tree c = TREE_OPERAND (t, 1);
	  releasing_vec index_exp_list;
	  tsubst_copy_and_build_call_args (c, args, complain, in_decl,
					   index_exp_list);

	  tree r;
	  if (vec_safe_length (index_exp_list) == 1
	      && !PACK_EXPANSION_P (index_exp_list[0]))
	    r = grok_array_decl (EXPR_LOCATION (t), op1,
				 index_exp_list[0], NULL,
				 complain | decltype_flag);
	  else
	    r = grok_array_decl (EXPR_LOCATION (t), op1,
				 NULL_TREE, &index_exp_list,
				 complain | decltype_flag);
	  RETURN (r);
	}
      RETURN (build_x_array_ref (EXPR_LOCATION (t), op1,
				 RECUR (TREE_OPERAND (t, 1)),
				 complain|decltype_flag));

    case SIZEOF_EXPR:
      if (PACK_EXPANSION_P (TREE_OPERAND (t, 0))
	  || ARGUMENT_PACK_P (TREE_OPERAND (t, 0)))
	RETURN (tsubst_copy (t, args, complain, in_decl));
      /* Fall through */

    case ALIGNOF_EXPR:
      {
	tree r;

	op1 = TREE_OPERAND (t, 0);
	if (TREE_CODE (t) == SIZEOF_EXPR && SIZEOF_EXPR_TYPE_P (t))
	  op1 = TREE_TYPE (op1);
	bool std_alignof = (TREE_CODE (t) == ALIGNOF_EXPR
			    && ALIGNOF_EXPR_STD_P (t));
        if (!args)
	  {
	    /* When there are no ARGS, we are trying to evaluate a
	       non-dependent expression from the parser.  Trying to do
	       the substitutions may not work.  */
	    if (!TYPE_P (op1))
	      op1 = TREE_TYPE (op1);
	  }
	else
	  {
	    ++cp_unevaluated_operand;
	    ++c_inhibit_evaluation_warnings;
	    if (TYPE_P (op1))
	      op1 = tsubst (op1, args, complain, in_decl);
	    else
	      op1 = tsubst_copy_and_build (op1, args, complain, in_decl);
	    --cp_unevaluated_operand;
	    --c_inhibit_evaluation_warnings;
	  }
        if (TYPE_P (op1))
	  r = cxx_sizeof_or_alignof_type (input_location,
					  op1, TREE_CODE (t), std_alignof,
					  complain & tf_error);
	else
	  r = cxx_sizeof_or_alignof_expr (input_location,
					  op1, TREE_CODE (t), std_alignof,
					  complain & tf_error);
	if (TREE_CODE (t) == SIZEOF_EXPR && r != error_mark_node)
	  {
	    if (TREE_CODE (r) != SIZEOF_EXPR || TYPE_P (op1))
	      {
		if (!processing_template_decl && TYPE_P (op1))
		  {
		    r = build_min (SIZEOF_EXPR, size_type_node,
				   build1 (NOP_EXPR, op1, error_mark_node));
		    SIZEOF_EXPR_TYPE_P (r) = 1;
		  }
		else
		  r = build_min (SIZEOF_EXPR, size_type_node, op1);
		TREE_SIDE_EFFECTS (r) = 0;
		TREE_READONLY (r) = 1;
	      }
	    SET_EXPR_LOCATION (r, EXPR_LOCATION (t));
	  }
	RETURN (r);
      }

    case AT_ENCODE_EXPR:
      {
	op1 = TREE_OPERAND (t, 0);
	++cp_unevaluated_operand;
	++c_inhibit_evaluation_warnings;
	op1 = tsubst_copy_and_build (op1, args, complain, in_decl);
	--cp_unevaluated_operand;
	--c_inhibit_evaluation_warnings;
	RETURN (objc_build_encode_expr (op1));
      }

    case NOEXCEPT_EXPR:
      op1 = TREE_OPERAND (t, 0);
      ++cp_unevaluated_operand;
      ++c_inhibit_evaluation_warnings;
      ++cp_noexcept_operand;
      op1 = tsubst_copy_and_build (op1, args, complain, in_decl);
      --cp_unevaluated_operand;
      --c_inhibit_evaluation_warnings;
      --cp_noexcept_operand;
      RETURN (finish_noexcept_expr (op1, complain));

    case MODOP_EXPR:
      {
	warning_sentinel s(warn_div_by_zero);
	tree lhs = RECUR (TREE_OPERAND (t, 0));
	tree rhs = RECUR (TREE_OPERAND (t, 2));

	tree r = build_x_modify_expr
	  (EXPR_LOCATION (t), lhs, TREE_CODE (TREE_OPERAND (t, 1)), rhs,
	   templated_operator_saved_lookups (t),
	   complain|decltype_flag);
	/* TREE_NO_WARNING must be set if either the expression was
	   parenthesized or it uses an operator such as >>= rather
	   than plain assignment.  In the former case, it was already
	   set and must be copied.  In the latter case,
	   build_x_modify_expr sets it and it must not be reset
	   here.  */
	if (warning_suppressed_p (t, OPT_Wparentheses))
	  suppress_warning (r, OPT_Wparentheses);

	RETURN (r);
      }

    case ARROW_EXPR:
      op1 = tsubst_non_call_postfix_expression (TREE_OPERAND (t, 0),
						args, complain, in_decl);
      /* Remember that there was a reference to this entity.  */
      if (DECL_P (op1)
	  && !mark_used (op1, complain) && !(complain & tf_error))
	RETURN (error_mark_node);
      RETURN (build_x_arrow (input_location, op1, complain));

    case NEW_EXPR:
      {
	tree placement = RECUR (TREE_OPERAND (t, 0));
	tree init = RECUR (TREE_OPERAND (t, 3));
	vec<tree, va_gc> *placement_vec;
	vec<tree, va_gc> *init_vec;
	tree ret;
	location_t loc = EXPR_LOCATION (t);

	if (placement == NULL_TREE)
	  placement_vec = NULL;
	else if (placement == error_mark_node)
	  RETURN (error_mark_node);
	else
	  {
	    placement_vec = make_tree_vector ();
	    for (; placement != NULL_TREE; placement = TREE_CHAIN (placement))
	      vec_safe_push (placement_vec, TREE_VALUE (placement));
	  }

	/* If there was an initializer in the original tree, but it
	   instantiated to an empty list, then we should pass a
	   non-NULL empty vector to tell build_new that it was an
	   empty initializer() rather than no initializer.  This can
	   only happen when the initializer is a pack expansion whose
	   parameter packs are of length zero.  */
	if (init == NULL_TREE && TREE_OPERAND (t, 3) == NULL_TREE)
	  init_vec = NULL;
	else if (init == error_mark_node)
	  RETURN (error_mark_node);
	else
	  {
	    init_vec = make_tree_vector ();
	    if (init == void_node)
	      gcc_assert (init_vec != NULL);
	    else
	      {
		for (; init != NULL_TREE; init = TREE_CHAIN (init))
		  vec_safe_push (init_vec, TREE_VALUE (init));
	      }
	  }

	/* Avoid passing an enclosing decl to valid_array_size_p.  */
	in_decl = NULL_TREE;

	tree op1 = tsubst (TREE_OPERAND (t, 1), args, complain, in_decl);
	tree op2 = RECUR (TREE_OPERAND (t, 2));
	ret = build_new (loc, &placement_vec, op1, op2,
			 &init_vec, NEW_EXPR_USE_GLOBAL (t),
			 complain);

	if (placement_vec != NULL)
	  release_tree_vector (placement_vec);
	if (init_vec != NULL)
	  release_tree_vector (init_vec);

	RETURN (ret);
      }

    case DELETE_EXPR:
      {
	tree op0 = RECUR (TREE_OPERAND (t, 0));
	tree op1 = RECUR (TREE_OPERAND (t, 1));
	RETURN (delete_sanity (input_location, op0, op1,
			       DELETE_EXPR_USE_VEC (t),
			       DELETE_EXPR_USE_GLOBAL (t),
			       complain));
      }

    case COMPOUND_EXPR:
      {
	tree op0 = tsubst_copy_and_build (TREE_OPERAND (t, 0), args,
					  complain & ~tf_decltype, in_decl);
	RETURN (build_x_compound_expr (EXPR_LOCATION (t),
				       op0,
				       RECUR (TREE_OPERAND (t, 1)),
				       templated_operator_saved_lookups (t),
				       complain|decltype_flag));
      }

    case CALL_EXPR:
      {
	tree function;
	unsigned int nargs;
	bool qualified_p;
	bool koenig_p;
	tree ret;

	function = CALL_EXPR_FN (t);
	/* Internal function with no arguments.  */
	if (function == NULL_TREE && call_expr_nargs (t) == 0)
	  RETURN (t);

	/* When we parsed the expression, we determined whether or
	   not Koenig lookup should be performed.  */
	koenig_p = KOENIG_LOOKUP_P (t);
	if (function == NULL_TREE)
	  {
	    koenig_p = false;
	    qualified_p = false;
	  }
	else if (TREE_CODE (function) == SCOPE_REF)
	  {
	    qualified_p = true;
	    function = tsubst_qualified_id (function, args, complain, in_decl,
					    /*done=*/false,
					    /*address_p=*/false);
	  }
	else if (koenig_p
		 && (identifier_p (function)
		     || (TREE_CODE (function) == TEMPLATE_ID_EXPR
			 && identifier_p (TREE_OPERAND (function, 0)))))
	  {
	    /* Do nothing; calling tsubst_copy_and_build on an identifier
	       would incorrectly perform unqualified lookup again.

	       Note that we can also have an IDENTIFIER_NODE if the earlier
	       unqualified lookup found a dependent local extern declaration
	       (as per finish_call_expr); in that case koenig_p will be false
	       and we do want to do the lookup again to find the substituted
	       declaration.  */
	    qualified_p = false;

	    if (TREE_CODE (function) == TEMPLATE_ID_EXPR)
	      /* Use tsubst_copy to substitute through the template arguments
		 of the template-id without performing unqualified lookup of
		 the template name.  */
	      function = tsubst_copy (function, args, complain, in_decl);
	  }
	else
	  {
	    if (TREE_CODE (function) == COMPONENT_REF)
	      {
		tree op = TREE_OPERAND (function, 1);

		qualified_p = (TREE_CODE (op) == SCOPE_REF
			       || (BASELINK_P (op)
				   && BASELINK_QUALIFIED_P (op)));
	      }
	    else
	      qualified_p = false;

	    if (TREE_CODE (function) == ADDR_EXPR
		&& TREE_CODE (TREE_OPERAND (function, 0)) == FUNCTION_DECL)
	      /* Avoid error about taking the address of a constructor.  */
	      function = TREE_OPERAND (function, 0);

	    tsubst_flags_t subcomplain = complain;
	    if (koenig_p && TREE_CODE (function) == FUNCTION_DECL)
	      /* When KOENIG_P, we don't want to mark_used the callee before
		 augmenting the overload set via ADL, so during this initial
		 substitution we disable mark_used by setting tf_conv (68942).  */
	      subcomplain |= tf_conv;
	    function = tsubst_copy_and_build (function, args, subcomplain, in_decl);

	    if (BASELINK_P (function))
	      qualified_p = true;
	  }

	nargs = call_expr_nargs (t);
	releasing_vec call_args;
	tsubst_copy_and_build_call_args (t, args, complain, in_decl,
					 call_args);

	/* Stripped-down processing for a call in a thunk.  Specifically, in
	   the thunk template for a generic lambda.  */
	if (call_from_lambda_thunk_p (t))
	  {
	    /* Now that we've expanded any packs, the number of call args
	       might be different.  */
	    unsigned int cargs = call_args->length ();
	    tree thisarg = NULL_TREE;
	    if (TREE_CODE (function) == COMPONENT_REF)
	      {
		thisarg = TREE_OPERAND (function, 0);
		if (TREE_CODE (thisarg) == INDIRECT_REF)
		  thisarg = TREE_OPERAND (thisarg, 0);
		function = TREE_OPERAND (function, 1);
		if (TREE_CODE (function) == BASELINK)
		  function = BASELINK_FUNCTIONS (function);
	      }
	    /* We aren't going to do normal overload resolution, so force the
	       template-id to resolve.  */
	    function = resolve_nondeduced_context (function, complain);
	    for (unsigned i = 0; i < cargs; ++i)
	      {
		/* In a thunk, pass through args directly, without any
		   conversions.  */
		tree arg = (*call_args)[i];
		while (TREE_CODE (arg) != PARM_DECL)
		  arg = TREE_OPERAND (arg, 0);
		(*call_args)[i] = arg;
	      }
	    if (thisarg)
	      {
		/* If there are no other args, just push 'this'.  */
		if (cargs == 0)
		  vec_safe_push (call_args, thisarg);
		else
		  {
		    /* Otherwise, shift the other args over to make room.  */
		    tree last = (*call_args)[cargs - 1];
		    vec_safe_push (call_args, last);
		    for (int i = cargs - 1; i > 0; --i)
		      (*call_args)[i] = (*call_args)[i - 1];
		    (*call_args)[0] = thisarg;
		  }
	      }
	    ret = build_call_a (function, call_args->length (),
				call_args->address ());
	    /* The thunk location is not interesting.  */
	    SET_EXPR_LOCATION (ret, UNKNOWN_LOCATION);
	    CALL_FROM_THUNK_P (ret) = true;
	    if (CLASS_TYPE_P (TREE_TYPE (ret)))
	      CALL_EXPR_RETURN_SLOT_OPT (ret) = true;

	    RETURN (ret);
	  }

	/* We do not perform argument-dependent lookup if normal
	   lookup finds a non-function, in accordance with the
	   resolution of DR 218.  */
	if (koenig_p
	    && ((is_overloaded_fn (function)
		 /* If lookup found a member function, the Koenig lookup is
		    not appropriate, even if an unqualified-name was used
		    to denote the function.  */
		 && !DECL_FUNCTION_MEMBER_P (get_first_fn (function)))
		|| identifier_p (function)
		/* C++20 P0846: Lookup found nothing.  */
		|| (TREE_CODE (function) == TEMPLATE_ID_EXPR
		    && identifier_p (TREE_OPERAND (function, 0))))
	    /* Only do this when substitution turns a dependent call
	       into a non-dependent call.  */
	    && type_dependent_expression_p_push (t)
	    && !any_type_dependent_arguments_p (call_args))
	  function = perform_koenig_lookup (function, call_args, tf_none);

	if (function != NULL_TREE
	    && (identifier_p (function)
		|| (TREE_CODE (function) == TEMPLATE_ID_EXPR
		    && identifier_p (TREE_OPERAND (function, 0))
		    && !any_dependent_template_arguments_p (TREE_OPERAND
							    (function, 1))))
	    && !any_type_dependent_arguments_p (call_args))
	  {
	    bool template_id_p = (TREE_CODE (function) == TEMPLATE_ID_EXPR);
	    if (template_id_p)
	      function = TREE_OPERAND (function, 0);
	    if (koenig_p && (complain & tf_warning_or_error))
	      {
		/* For backwards compatibility and good diagnostics, try
		   the unqualified lookup again if we aren't in SFINAE
		   context.  */
		tree unq = tsubst_copy_and_build (function, args,
						  complain, in_decl);
		if (unq == error_mark_node)
		  RETURN (error_mark_node);

		if (unq != function)
		  {
		    char const *const msg
		      = G_("%qD was not declared in this scope, "
			   "and no declarations were found by "
			   "argument-dependent lookup at the point "
			   "of instantiation");

		    bool in_lambda = (current_class_type
				      && LAMBDA_TYPE_P (current_class_type));
		    /* In a lambda fn, we have to be careful to not
		       introduce new this captures.  Legacy code can't
		       be using lambdas anyway, so it's ok to be
		       stricter.  Be strict with C++20 template-id ADL too.
		       And be strict if we're already failing anyway.  */
		    bool strict = in_lambda || template_id_p || seen_error();
		    bool diag = true;
		    if (strict)
		      error_at (cp_expr_loc_or_input_loc (t),
				msg, function);
		    else
		      diag = permerror (cp_expr_loc_or_input_loc (t),
					msg, function);
		    if (diag)
		      {
			tree fn = unq;

			if (INDIRECT_REF_P (fn))
			  fn = TREE_OPERAND (fn, 0);
			if (is_overloaded_fn (fn))
			  fn = get_first_fn (fn);

			if (!DECL_P (fn))
			  /* Can't say anything more.  */;
			else if (DECL_CLASS_SCOPE_P (fn))
			  {
			    location_t loc = cp_expr_loc_or_input_loc (t);
			    inform (loc,
				    "declarations in dependent base %qT are "
				    "not found by unqualified lookup",
				    DECL_CLASS_CONTEXT (fn));
			    if (current_class_ptr)
			      inform (loc,
				      "use %<this->%D%> instead", function);
			    else
			      inform (loc,
				      "use %<%T::%D%> instead",
				      current_class_name, function);
			  }
			else
			  inform (DECL_SOURCE_LOCATION (fn),
				  "%qD declared here, later in the "
				  "translation unit", fn);
			if (strict)
			  RETURN (error_mark_node);
		      }

		    function = unq;
		  }
	      }
	    if (identifier_p (function))
	      {
		if (complain & tf_error)
		  unqualified_name_lookup_error (function);
		RETURN (error_mark_node);
	      }
	  }

	/* Remember that there was a reference to this entity.  */
	if (function != NULL_TREE
	    && DECL_P (function)
	    && !mark_used (function, complain) && !(complain & tf_error))
	  RETURN (error_mark_node);

	if (!maybe_fold_fn_template_args (function, complain))
	  return error_mark_node;

	/* Put back tf_decltype for the actual call.  */
	complain |= decltype_flag;

	if (function == NULL_TREE)
	  switch (CALL_EXPR_IFN (t))
	    {
	    case IFN_LAUNDER:
	      gcc_assert (nargs == 1);
	      if (vec_safe_length (call_args) != 1)
		{
		  error_at (cp_expr_loc_or_input_loc (t),
			    "wrong number of arguments to "
			    "%<__builtin_launder%>");
		  ret = error_mark_node;
		}
	      else
		ret = finish_builtin_launder (cp_expr_loc_or_input_loc (t),
					      (*call_args)[0], complain);
	      break;

	    case IFN_VEC_CONVERT:
	      gcc_assert (nargs == 1);
	      if (vec_safe_length (call_args) != 1)
		{
		  error_at (cp_expr_loc_or_input_loc (t),
			    "wrong number of arguments to "
			    "%<__builtin_convertvector%>");
		  ret = error_mark_node;
		  break;
		}
	      ret = cp_build_vec_convert ((*call_args)[0], input_location,
					  tsubst (TREE_TYPE (t), args,
						  complain, in_decl),
					  complain);
	      if (TREE_CODE (ret) == VIEW_CONVERT_EXPR)
		RETURN (ret);
	      break;

	    case IFN_SHUFFLEVECTOR:
	      {
		ret = build_x_shufflevector (input_location, call_args,
					     complain);
		if (ret != error_mark_node)
		  RETURN (ret);
		break;
	      }

	    case IFN_ASSUME:
	      gcc_assert (nargs == 1);
	      if (vec_safe_length (call_args) != 1)
		{
		  error_at (cp_expr_loc_or_input_loc (t),
			    "wrong number of arguments to "
			    "%<assume%> attribute");
		  ret = error_mark_node;
		}
	      else
		{
		  tree &arg = (*call_args)[0];
		  if (!type_dependent_expression_p (arg))
		    arg = contextual_conv_bool (arg, tf_warning_or_error);
		  if (error_operand_p (arg))
		    {
		      ret = error_mark_node;
		      break;
		    }
		  ret = build_assume_call (EXPR_LOCATION (t), arg);
		  RETURN (ret);
		}
	      break;

	    default:
	      /* Unsupported internal function with arguments.  */
	      gcc_unreachable ();
	    }
	else if (TREE_CODE (function) == OFFSET_REF
		 || TREE_CODE (function) == DOTSTAR_EXPR
		 || TREE_CODE (function) == MEMBER_REF)
	  ret = build_offset_ref_call_from_tree (function, &call_args,
						 complain);
	else if (TREE_CODE (function) == COMPONENT_REF)
	  {
	    tree instance = TREE_OPERAND (function, 0);
	    tree fn = TREE_OPERAND (function, 1);

	    if (processing_template_decl
		&& (type_dependent_expression_p (instance)
		    || (!BASELINK_P (fn)
			&& TREE_CODE (fn) != FIELD_DECL)
		    || type_dependent_expression_p (fn)
		    || any_type_dependent_arguments_p (call_args)))
	      ret = build_min_nt_call_vec (function, call_args);
	    else if (!BASELINK_P (fn))
	      ret = finish_call_expr (function, &call_args,
				       /*disallow_virtual=*/false,
				       /*koenig_p=*/false,
				       complain);
	    else
	      ret = (build_new_method_call
		      (instance, fn,
		       &call_args, NULL_TREE,
		       qualified_p ? LOOKUP_NONVIRTUAL : LOOKUP_NORMAL,
		       /*fn_p=*/NULL,
		       complain));
	  }
	else if (concept_check_p (function))
	  {
	    /* FUNCTION is a template-id referring to a concept definition.  */
	    tree id = unpack_concept_check (function);
	    tree tmpl = TREE_OPERAND (id, 0);
	    tree args = TREE_OPERAND (id, 1);

	    /* Calls to standard and variable concepts should have been
	       previously diagnosed.  */
	    gcc_assert (function_concept_p (tmpl));

	    /* Ensure the result is wrapped as a call expression.  */
	    ret = build_concept_check (tmpl, args, tf_warning_or_error);
	  }
	else
	  ret = finish_call_expr (function, &call_args,
				  /*disallow_virtual=*/qualified_p,
				  koenig_p,
				  complain);

	if (ret != error_mark_node)
	  {
	    bool op = CALL_EXPR_OPERATOR_SYNTAX (t);
	    bool ord = CALL_EXPR_ORDERED_ARGS (t);
	    bool rev = CALL_EXPR_REVERSE_ARGS (t);
	    if (op || ord || rev)
	      if (tree call = extract_call_expr (ret))
		{
		  CALL_EXPR_OPERATOR_SYNTAX (call) = op;
		  CALL_EXPR_ORDERED_ARGS (call) = ord;
		  CALL_EXPR_REVERSE_ARGS (call) = rev;
		}
	    if (warning_suppressed_p (t, OPT_Wpessimizing_move))
	      /* This also suppresses -Wredundant-move.  */
	      suppress_warning (ret, OPT_Wpessimizing_move);
	  }

	RETURN (ret);
      }

    case COND_EXPR:
      {
	tree cond = RECUR (TREE_OPERAND (t, 0));
	cond = mark_rvalue_use (cond);
	tree folded_cond = fold_non_dependent_expr (cond, complain);
	tree exp1, exp2;

	if (TREE_CODE (folded_cond) == INTEGER_CST)
	  {
	    if (integer_zerop (folded_cond))
	      {
		++c_inhibit_evaluation_warnings;
		exp1 = RECUR (TREE_OPERAND (t, 1));
		--c_inhibit_evaluation_warnings;
		exp2 = RECUR (TREE_OPERAND (t, 2));
	      }
	    else
	      {
		exp1 = RECUR (TREE_OPERAND (t, 1));
		++c_inhibit_evaluation_warnings;
		exp2 = RECUR (TREE_OPERAND (t, 2));
		--c_inhibit_evaluation_warnings;
	      }
	    cond = folded_cond;
	  }
	else
	  {
	    exp1 = RECUR (TREE_OPERAND (t, 1));
	    exp2 = RECUR (TREE_OPERAND (t, 2));
	  }

	warning_sentinel s(warn_duplicated_branches);
	RETURN (build_x_conditional_expr (EXPR_LOCATION (t),
					 cond, exp1, exp2, complain));
      }

    case PSEUDO_DTOR_EXPR:
      {
	tree op0 = RECUR (TREE_OPERAND (t, 0));
	tree op1 = RECUR (TREE_OPERAND (t, 1));
	tree op2 = tsubst (TREE_OPERAND (t, 2), args, complain, in_decl);
	RETURN (finish_pseudo_destructor_expr (op0, op1, op2,
					       input_location));
      }

    case TREE_LIST:
      RETURN (tsubst_tree_list (t, args, complain, in_decl));

    case COMPONENT_REF:
      {
	tree object;
	tree object_type;
	tree member;
	tree r;

	object = tsubst_non_call_postfix_expression (TREE_OPERAND (t, 0),
						     args, complain, in_decl);
	/* Remember that there was a reference to this entity.  */
	if (DECL_P (object)
	    && !mark_used (object, complain) && !(complain & tf_error))
	  RETURN (error_mark_node);
	object_type = TREE_TYPE (object);

	member = TREE_OPERAND (t, 1);
	if (BASELINK_P (member))
	  member = tsubst_baselink (member,
				    non_reference (TREE_TYPE (object)),
				    args, complain, in_decl);
	else
	  member = tsubst_copy (member, args, complain, in_decl);
	if (member == error_mark_node)
	  RETURN (error_mark_node);

	if (object_type && TYPE_PTRMEMFUNC_P (object_type)
	    && TREE_CODE (member) == FIELD_DECL)
	  {
	    r = build_ptrmemfunc_access_expr (object, DECL_NAME (member));
	    RETURN (r);
	  }
	else if (TREE_CODE (member) == FIELD_DECL)
	  {
	    r = finish_non_static_data_member (member, object, NULL_TREE,
					       complain);
	    if (TREE_CODE (r) == COMPONENT_REF)
	      REF_PARENTHESIZED_P (r) = REF_PARENTHESIZED_P (t);
	    RETURN (r);
	  }
	else if (type_dependent_expression_p (object))
	  /* We can't do much here.  */;
	else if (!CLASS_TYPE_P (object_type))
	  {
	    if (scalarish_type_p (object_type))
	      {
		tree s = NULL_TREE;
		tree dtor = member;

		if (TREE_CODE (dtor) == SCOPE_REF)
		  {
		    s = TREE_OPERAND (dtor, 0);
		    dtor = TREE_OPERAND (dtor, 1);
		  }
		if (TREE_CODE (dtor) == BIT_NOT_EXPR)
		  {
		    dtor = TREE_OPERAND (dtor, 0);
		    if (TYPE_P (dtor))
		      RETURN (finish_pseudo_destructor_expr
			      (object, s, dtor, input_location));
		  }
	      }
	  }
	else if (TREE_CODE (member) == SCOPE_REF
		 && TREE_CODE (TREE_OPERAND (member, 1)) == TEMPLATE_ID_EXPR)
	  {
	    /* Lookup the template functions now that we know what the
	       scope is.  */
	    tree scope = TREE_OPERAND (member, 0);
	    tree tmpl = TREE_OPERAND (TREE_OPERAND (member, 1), 0);
	    tree args = TREE_OPERAND (TREE_OPERAND (member, 1), 1);
	    member = lookup_qualified_name (scope, tmpl, LOOK_want::NORMAL,
					    /*complain=*/false);
	    if (BASELINK_P (member))
	      {
		BASELINK_FUNCTIONS (member)
		  = build_nt (TEMPLATE_ID_EXPR, BASELINK_FUNCTIONS (member),
			      args);
		member = (adjust_result_of_qualified_name_lookup
			  (member, BINFO_TYPE (BASELINK_BINFO (member)),
			   object_type));
	      }
	    else
	      {
		qualified_name_lookup_error (scope, tmpl, member,
					     input_location);
		RETURN (error_mark_node);
	      }
	  }
	else if (TREE_CODE (member) == SCOPE_REF
		 && !CLASS_TYPE_P (TREE_OPERAND (member, 0))
		 && TREE_CODE (TREE_OPERAND (member, 0)) != NAMESPACE_DECL)
	  {
	    if (complain & tf_error)
	      {
		if (TYPE_P (TREE_OPERAND (member, 0)))
		  error ("%qT is not a class or namespace",
			 TREE_OPERAND (member, 0));
		else
		  error ("%qD is not a class or namespace",
			 TREE_OPERAND (member, 0));
	      }
	    RETURN (error_mark_node);
	  }

	r = finish_class_member_access_expr (object, member,
					     /*template_p=*/false,
					     complain);
	if (REF_PARENTHESIZED_P (t))
	  r = force_paren_expr (r);
	RETURN (r);
      }

    case THROW_EXPR:
      RETURN (build_throw
       (input_location, RECUR (TREE_OPERAND (t, 0))));

    case CONSTRUCTOR:
      {
	vec<constructor_elt, va_gc> *n;
	constructor_elt *ce;
	unsigned HOST_WIDE_INT idx;
	bool process_index_p;
        int newlen;
        bool need_copy_p = false;
	tree r;

	tsubst_flags_t tcomplain = complain;
	if (COMPOUND_LITERAL_P (t))
	  tcomplain |= tf_tst_ok;
	tree type = tsubst (TREE_TYPE (t), args, tcomplain, in_decl);
	if (type == error_mark_node)
	  RETURN (error_mark_node);

	/* We do not want to process the index of aggregate
	   initializers as they are identifier nodes which will be
	   looked up by digest_init.  */
	process_index_p = !(type && MAYBE_CLASS_TYPE_P (type));

	if (null_member_pointer_value_p (t))
	  {
	    gcc_assert (same_type_p (type, TREE_TYPE (t)));
	    RETURN (t);
	  }

	n = vec_safe_copy (CONSTRUCTOR_ELTS (t));
        newlen = vec_safe_length (n);
	FOR_EACH_VEC_SAFE_ELT (n, idx, ce)
	  {
	    if (ce->index && process_index_p
		/* An identifier index is looked up in the type
		   being initialized, not the current scope.  */
		&& TREE_CODE (ce->index) != IDENTIFIER_NODE)
	      ce->index = RECUR (ce->index);

            if (PACK_EXPANSION_P (ce->value))
              {
                /* Substitute into the pack expansion.  */
                ce->value = tsubst_pack_expansion (ce->value, args, complain,
                                                  in_decl);

		if (ce->value == error_mark_node
		    || PACK_EXPANSION_P (ce->value))
		  ;
		else if (TREE_VEC_LENGTH (ce->value) == 1)
                  /* Just move the argument into place.  */
                  ce->value = TREE_VEC_ELT (ce->value, 0);
                else
                  {
                    /* Update the length of the final CONSTRUCTOR
                       arguments vector, and note that we will need to
                       copy.*/
                    newlen = newlen + TREE_VEC_LENGTH (ce->value) - 1;
                    need_copy_p = true;
                  }
              }
            else
              ce->value = RECUR (ce->value);
	  }

        if (need_copy_p)
          {
            vec<constructor_elt, va_gc> *old_n = n;

            vec_alloc (n, newlen);
            FOR_EACH_VEC_ELT (*old_n, idx, ce)
              {
                if (TREE_CODE (ce->value) == TREE_VEC)
                  {
                    int i, len = TREE_VEC_LENGTH (ce->value);
                    for (i = 0; i < len; ++i)
                      CONSTRUCTOR_APPEND_ELT (n, 0,
                                              TREE_VEC_ELT (ce->value, i));
                  }
                else
                  CONSTRUCTOR_APPEND_ELT (n, 0, ce->value);
              }
          }

	r = build_constructor (init_list_type_node, n);
	CONSTRUCTOR_IS_DIRECT_INIT (r) = CONSTRUCTOR_IS_DIRECT_INIT (t);
	CONSTRUCTOR_IS_DESIGNATED_INIT (r)
	  = CONSTRUCTOR_IS_DESIGNATED_INIT (t);

	if (TREE_HAS_CONSTRUCTOR (t))
	  {
	    fcl_t cl = fcl_functional;
	    if (CONSTRUCTOR_C99_COMPOUND_LITERAL (t))
	      cl = fcl_c99;
	    RETURN (finish_compound_literal (type, r, complain, cl));
	  }

	TREE_TYPE (r) = type;
	RETURN (r);
      }

    case TYPEID_EXPR:
      {
	tree operand_0 = TREE_OPERAND (t, 0);
	if (TYPE_P (operand_0))
	  {
	    operand_0 = tsubst (operand_0, args, complain, in_decl);
	    RETURN (get_typeid (operand_0, complain));
	  }
	else
	  {
	    operand_0 = RECUR (operand_0);
	    RETURN (build_typeid (operand_0, complain));
	  }
      }

    case VAR_DECL:
      if (!args)
	RETURN (t);
      /* Fall through */

    case PARM_DECL:
      {
	tree r = tsubst_copy (t, args, complain, in_decl);
	/* ??? We're doing a subset of finish_id_expression here.  */
	if (tree wrap = maybe_get_tls_wrapper_call (r))
	  /* Replace an evaluated use of the thread_local variable with
	     a call to its wrapper.  */
	  r = wrap;
	else if (outer_automatic_var_p (r))
	  r = process_outer_var_ref (r, complain);

	if (!TYPE_REF_P (TREE_TYPE (t)))
	  /* If the original type was a reference, we'll be wrapped in
	     the appropriate INDIRECT_REF.  */
	  r = convert_from_reference (r);
	RETURN (r);
      }

    case VA_ARG_EXPR:
      {
	tree op0 = RECUR (TREE_OPERAND (t, 0));
	tree type = tsubst (TREE_TYPE (t), args, complain, in_decl);
	RETURN (build_x_va_arg (EXPR_LOCATION (t), op0, type));
      }

    case OFFSETOF_EXPR:
      {
	tree object_ptr
	  = tsubst_copy_and_build (TREE_OPERAND (t, 1), args,
				   complain, in_decl);
	RETURN (finish_offsetof (object_ptr,
				 RECUR (TREE_OPERAND (t, 0)),
				 EXPR_LOCATION (t)));
      }

    case ADDRESSOF_EXPR:
      RETURN (cp_build_addressof (EXPR_LOCATION (t),
				  RECUR (TREE_OPERAND (t, 0)), complain));

    case TRAIT_EXPR:
      {
	tree type1 = tsubst_copy (TRAIT_EXPR_TYPE1 (t), args,
				  complain, in_decl);
	tree type2 = tsubst (TRAIT_EXPR_TYPE2 (t), args,
			     complain, in_decl);
	RETURN (finish_trait_expr (TRAIT_EXPR_LOCATION (t),
				   TRAIT_EXPR_KIND (t), type1, type2));
      }

    case STMT_EXPR:
      {
	tree old_stmt_expr = cur_stmt_expr;
	tree stmt_expr = begin_stmt_expr ();

	cur_stmt_expr = stmt_expr;
	tsubst_expr (STMT_EXPR_STMT (t), args, complain, in_decl);
	stmt_expr = finish_stmt_expr (stmt_expr, false);
	cur_stmt_expr = old_stmt_expr;

	/* If the resulting list of expression statement is empty,
	   fold it further into void_node.  */
	if (empty_expr_stmt_p (stmt_expr))
	  stmt_expr = void_node;

	RETURN (stmt_expr);
      }

    case LAMBDA_EXPR:
      {
	if (complain & tf_partial)
	  {
	    /* We don't have a full set of template arguments yet; don't touch
	       the lambda at all.  */
	    gcc_assert (processing_template_decl);
	    return t;
	  }
	tree r = tsubst_lambda_expr (t, args, complain, in_decl);

	RETURN (build_lambda_object (r));
      }

    case TRANSACTION_EXPR:
      RETURN (tsubst_expr (t, args, complain, in_decl));

    case PAREN_EXPR:
      if (REF_PARENTHESIZED_P (t))
	RETURN (finish_parenthesized_expr (RECUR (TREE_OPERAND (t, 0))));
      else
	/* Recreate the PAREN_EXPR from __builtin_assoc_barrier.  */
	{
	  tree op0 = RECUR (TREE_OPERAND (t, 0));
	  RETURN (build1_loc (input_location, PAREN_EXPR,
			      TREE_TYPE (op0), op0));
	}

    case VEC_PERM_EXPR:
      {
	tree op0 = RECUR (TREE_OPERAND (t, 0));
	tree op1 = RECUR (TREE_OPERAND (t, 1));
	tree op2 = RECUR (TREE_OPERAND (t, 2));
	RETURN (build_x_vec_perm_expr (input_location, op0, op1, op2,
				       complain));
      }

    case REQUIRES_EXPR:
      {
	tree r = tsubst_requires_expr (t, args, tf_none, in_decl);
	RETURN (r);
      }

    case RANGE_EXPR:
      /* No need to substitute further, a RANGE_EXPR will always be built
	 with constant operands.  */
      RETURN (t);

    case NON_LVALUE_EXPR:
    case VIEW_CONVERT_EXPR:
      {
	tree op = RECUR (TREE_OPERAND (t, 0));

	if (location_wrapper_p (t))
	  /* We need to do this here as well as in tsubst_copy so we get the
	     other tsubst_copy_and_build semantics for a PARM_DECL operand.  */
	  RETURN (maybe_wrap_with_location (op, EXPR_LOCATION (t)));

	gcc_checking_assert (TREE_CODE (t) == VIEW_CONVERT_EXPR);
	if (REF_PARENTHESIZED_P (t))
	  /* force_paren_expr can also create a VIEW_CONVERT_EXPR.  */
	  RETURN (finish_parenthesized_expr (op));

	/* Otherwise, we're dealing with a wrapper to make a C++20 template
	   parameter object const.  */
	if (TREE_TYPE (op) == NULL_TREE
	    || !CP_TYPE_CONST_P (TREE_TYPE (op)))
	  {
	    /* The template argument is not const, presumably because
	       it is still dependent, and so not the const template parm
	       object.  */
	    tree type = tsubst (TREE_TYPE (t), args, complain, in_decl);
	    if (TREE_CODE (op) == CONSTRUCTOR
		|| TREE_CODE (op) == IMPLICIT_CONV_EXPR)
	      {
		/* Don't add a wrapper to these.  */
		op = copy_node (op);
		TREE_TYPE (op) = type;
	      }
	    else
	      /* Do add a wrapper otherwise (in particular, if op is
		 another TEMPLATE_PARM_INDEX).  */
	      op = build1 (VIEW_CONVERT_EXPR, type, op);
	  }
	RETURN (op);
      }

    default:
      /* Handle Objective-C++ constructs, if appropriate.  */
      {
	tree subst
	  = objcp_tsubst_copy_and_build (t, args, complain, in_decl);
	if (subst)
	  RETURN (subst);
      }
      RETURN (tsubst_copy (t, args, complain, in_decl));
    }

#undef RECUR
#undef RETURN
 out:
  input_location = save_loc;
  return retval;
}

/* Verify that the instantiated ARGS are valid. For type arguments,
   make sure that the type's linkage is ok. For non-type arguments,
   make sure they are constants if they are integral or enumerations.
   Emit an error under control of COMPLAIN, and return TRUE on error.  */

static bool
check_instantiated_arg (tree tmpl, tree t, tsubst_flags_t complain)
{
  if (dependent_template_arg_p (t))
    return false;
  if (ARGUMENT_PACK_P (t))
    {
      tree vec = ARGUMENT_PACK_ARGS (t);
      int len = TREE_VEC_LENGTH (vec);
      bool result = false;
      int i;

      for (i = 0; i < len; ++i)
	if (check_instantiated_arg (tmpl, TREE_VEC_ELT (vec, i), complain))
	  result = true;
      return result;
    }
  else if (TYPE_P (t))
    {
      /* [basic.link]: A name with no linkage (notably, the name
	 of a class or enumeration declared in a local scope)
	 shall not be used to declare an entity with linkage.
	 This implies that names with no linkage cannot be used as
	 template arguments

	 DR 757 relaxes this restriction for C++0x.  */
      tree nt = (cxx_dialect > cxx98 ? NULL_TREE
		 : no_linkage_check (t, /*relaxed_p=*/false));

      if (nt)
	{
	  /* DR 488 makes use of a type with no linkage cause
	     type deduction to fail.  */
	  if (complain & tf_error)
	    {
	      if (TYPE_UNNAMED_P (nt))
		error ("%qT is/uses unnamed type", t);
	      else
		error ("template argument for %qD uses local type %qT",
		       tmpl, t);
	    }
	  return true;
	}
      /* In order to avoid all sorts of complications, we do not
	 allow variably-modified types as template arguments.  */
      else if (variably_modified_type_p (t, NULL_TREE))
	{
	  if (complain & tf_error)
	    error ("%qT is a variably modified type", t);
	  return true;
	}
    }
  /* Class template and alias template arguments should be OK.  */
  else if (DECL_TYPE_TEMPLATE_P (t))
    ;
  /* A non-type argument of integral or enumerated type must be a
     constant.  */
  else if (TREE_TYPE (t)
	   && INTEGRAL_OR_ENUMERATION_TYPE_P (TREE_TYPE (t))
	   && !REFERENCE_REF_P (t)
	   && !TREE_CONSTANT (t))
    {
      if (complain & tf_error)
	error ("integral expression %qE is not constant", t);
      return true;
    }
  return false;
}

static bool
check_instantiated_args (tree tmpl, tree args, tsubst_flags_t complain)
{
  int ix, len = DECL_NTPARMS (tmpl);
  bool result = false;

  for (ix = 0; ix != len; ix++)
    {
      if (check_instantiated_arg (tmpl, TREE_VEC_ELT (args, ix), complain))
	result = true;
    }
  if (result && (complain & tf_error))
    error ("  trying to instantiate %qD", tmpl);
  return result;
}

/* Call mark_used on each entity within the non-type template arguments in
   ARGS for an instantiation of TMPL, to ensure that each such entity is
   considered odr-used (and therefore marked for instantiation) regardless of
   whether the specialization was first formed in a template context (which
   inhibits mark_used).

   This function assumes push_to_top_level has been called beforehand.  */

static void
mark_template_arguments_used (tree tmpl, tree args)
{
  /* It suffices to do this only when instantiating a primary template.  */
  if (TREE_CODE (tmpl) != TEMPLATE_DECL || !PRIMARY_TEMPLATE_P (tmpl))
    return;

  /* We already marked outer arguments when specializing the context.  */
  args = INNERMOST_TEMPLATE_ARGS (args);

  for (tree arg : tree_vec_range (args))
    {
      /* A (pointer/reference to) function or variable NTTP argument.  */
      if (TREE_CODE (arg) == ADDR_EXPR
	  || TREE_CODE (arg) == INDIRECT_REF)
	{
	  while (TREE_CODE (arg) == ADDR_EXPR
		 || REFERENCE_REF_P (arg)
		 || CONVERT_EXPR_P (arg))
	    arg = TREE_OPERAND (arg, 0);
	  if (VAR_OR_FUNCTION_DECL_P (arg))
	    {
	      /* Pass tf_none to avoid duplicate diagnostics: if this call
		 fails then an earlier call to mark_used for this argument
		 must have also failed and emitted a diagnostic.  */
	      bool ok = mark_used (arg, tf_none);
	      gcc_checking_assert (ok || seen_error ());
	    }
	}
      /* A class NTTP argument.  */
      else if (VAR_P (arg)
	       && DECL_NTTP_OBJECT_P (arg))
	{
	  auto mark_used_r = [](tree *tp, int *, void *) {
	    if (VAR_OR_FUNCTION_DECL_P (*tp))
	      {
		bool ok = mark_used (*tp, tf_none);
		gcc_checking_assert (ok || seen_error ());
	      }
	    return NULL_TREE;
	  };
	  cp_walk_tree_without_duplicates (&DECL_INITIAL (arg),
					   mark_used_r, nullptr);
	}
    }
}

/* We're out of SFINAE context now, so generate diagnostics for the access
   errors we saw earlier when instantiating D from TMPL and ARGS.  */

static void
recheck_decl_substitution (tree d, tree tmpl, tree args)
{
  tree pattern = DECL_TEMPLATE_RESULT (tmpl);
  tree type = TREE_TYPE (pattern);
  location_t loc = input_location;

  push_access_scope (d);
  push_deferring_access_checks (dk_no_deferred);
  input_location = DECL_SOURCE_LOCATION (pattern);
  tsubst (type, args, tf_warning_or_error, d);
  input_location = loc;
  pop_deferring_access_checks ();
  pop_access_scope (d);
}

/* Instantiate the indicated variable, function, or alias template TMPL with
   the template arguments in TARG_PTR.  */

tree
instantiate_template (tree tmpl, tree orig_args, tsubst_flags_t complain)
{
  auto_timevar tv (TV_TEMPLATE_INST);

  tree targ_ptr = orig_args;
  tree fndecl;
  tree gen_tmpl;
  bool access_ok = true;

  if (tmpl == error_mark_node)
    return error_mark_node;

  /* The other flags are not relevant anymore here, especially tf_partial
     shouldn't be set.  For instance, we may be called while doing a partial
     substitution of a template variable, but the type of the variable
     template may be auto, in which case we will call do_auto_deduction
     in mark_used (which clears tf_partial) and the auto must be properly
     reduced at that time for the deduction to work.  */
  complain &= tf_warning_or_error;

  gcc_assert (TREE_CODE (tmpl) == TEMPLATE_DECL);

  if (modules_p ())
    lazy_load_pendings (tmpl);

  /* If this function is a clone, handle it specially.  */
  if (DECL_CLONED_FUNCTION_P (tmpl))
    {
      tree spec;
      tree clone;

      /* Use DECL_ABSTRACT_ORIGIN because only FUNCTION_DECLs have
	 DECL_CLONED_FUNCTION.  */
      spec = instantiate_template (DECL_ABSTRACT_ORIGIN (tmpl),
				   targ_ptr, complain);
      if (spec == error_mark_node)
	return error_mark_node;

      /* Look for the clone.  */
      FOR_EACH_CLONE (clone, spec)
	if (DECL_NAME (clone) == DECL_NAME (tmpl))
	  return clone;
      /* We should always have found the clone by now.  */
      gcc_unreachable ();
      return NULL_TREE;
    }

  if (targ_ptr == error_mark_node)
    return error_mark_node;

  /* Check to see if we already have this specialization.  */
  gen_tmpl = most_general_template (tmpl);
  if (TMPL_ARGS_DEPTH (targ_ptr)
      < TMPL_PARMS_DEPTH (DECL_TEMPLATE_PARMS (gen_tmpl)))
    /* targ_ptr only has the innermost template args, so add the outer ones
       from tmpl, which could be either a partial instantiation or gen_tmpl (in
       the case of a non-dependent call within a template definition).  */
    targ_ptr = (add_outermost_template_args
		(DECL_TI_ARGS (DECL_TEMPLATE_RESULT (tmpl)),
		 targ_ptr));

  hashval_t hash = spec_hasher::hash (gen_tmpl, targ_ptr);
  tree spec = retrieve_specialization (gen_tmpl, targ_ptr, hash);

  gcc_checking_assert (tmpl == gen_tmpl
		       || ((fndecl
			    = retrieve_specialization (tmpl, orig_args, 0))
			   == spec)
		       || fndecl == NULL_TREE);

  if (spec != NULL_TREE)
    {
      if (FNDECL_HAS_ACCESS_ERRORS (spec))
	{
	  if (complain & tf_error)
	    recheck_decl_substitution (spec, gen_tmpl, targ_ptr);
	  return error_mark_node;
	}
      return spec;
    }

  if (check_instantiated_args (gen_tmpl, INNERMOST_TEMPLATE_ARGS (targ_ptr),
			       complain))
    return error_mark_node;

  /* We are building a FUNCTION_DECL, during which the access of its
     parameters and return types have to be checked.  However this
     FUNCTION_DECL which is the desired context for access checking
     is not built yet.  We solve this chicken-and-egg problem by
     deferring all checks until we have the FUNCTION_DECL.  */
  push_deferring_access_checks (dk_deferred);

  /* Instantiation of the function happens in the context of the function
     template, not the context of the overload resolution we're doing.  */
  push_to_top_level ();
  /* If there are dependent arguments, e.g. because we're doing partial
     ordering, make sure processing_template_decl stays set.  */
  if (uses_template_parms (targ_ptr))
    ++processing_template_decl;
  if (DECL_CLASS_SCOPE_P (gen_tmpl))
    {
      tree ctx;
      if (!uses_template_parms (DECL_CONTEXT (tmpl)))
	/* If the context of the partially instantiated template is
	   already non-dependent, then we might as well use it.  */
	ctx = DECL_CONTEXT (tmpl);
      else
	ctx = tsubst_aggr_type (DECL_CONTEXT (gen_tmpl), targ_ptr,
				complain, gen_tmpl, true);
      push_nested_class (ctx);
    }

  tree pattern = DECL_TEMPLATE_RESULT (gen_tmpl);

  tree partial_ti = NULL_TREE;
  fndecl = NULL_TREE;
  if (VAR_P (pattern))
    {
      /* We need to determine if we're using a partial or explicit
	 specialization now, because the type of the variable could be
	 different.  */
      tree tid = build2 (TEMPLATE_ID_EXPR, NULL_TREE, tmpl, targ_ptr);
      partial_ti = most_specialized_partial_spec (tid, complain);
      if (partial_ti == error_mark_node)
	pattern = error_mark_node;
      else if (partial_ti)
	{
	  tree partial_tmpl = TI_TEMPLATE (partial_ti);
	  tree partial_args = TI_ARGS (partial_ti);
	  tree partial_pat = DECL_TEMPLATE_RESULT (partial_tmpl);
	  fndecl = tsubst_decl (partial_pat, partial_args, complain,
				/*use_spec_table=*/false);
	}
    }

  /* Substitute template parameters to obtain the specialization.  */
  if (fndecl == NULL_TREE)
    fndecl = tsubst_decl (pattern, targ_ptr, complain, /*use_spec_table=*/false);
  if (DECL_CLASS_SCOPE_P (gen_tmpl))
    pop_nested_class ();
  pop_from_top_level ();

  if (fndecl == error_mark_node)
    {
      pop_deferring_access_checks ();
      return error_mark_node;
    }

  /* The DECL_TI_TEMPLATE should always be the immediate parent
     template, not the most general template.  */
  DECL_TI_TEMPLATE (fndecl) = tmpl;
  DECL_TI_ARGS (fndecl) = targ_ptr;
  if (VAR_P (pattern))
    /* Now that we we've formed this variable template specialization,
       remember the result of most_specialized_partial_spec for it.  */
    TI_PARTIAL_INFO (DECL_TEMPLATE_INFO (fndecl)) = partial_ti;

  fndecl = register_specialization (fndecl, gen_tmpl, targ_ptr, false, hash);
  if (fndecl == error_mark_node)
    return error_mark_node;

  set_instantiating_module (fndecl);

  /* Now we know the specialization, compute access previously
     deferred.  Do no access control for inheriting constructors,
     as we already checked access for the inherited constructor.  */
  if (!(flag_new_inheriting_ctors
	&& DECL_INHERITED_CTOR (fndecl)))
    {
      push_access_scope (fndecl);
      if (!perform_deferred_access_checks (complain))
	access_ok = false;
      pop_access_scope (fndecl);
    }
  pop_deferring_access_checks ();

  /* If we've just instantiated the main entry point for a function,
     instantiate all the alternate entry points as well.  We do this
     by cloning the instantiation of the main entry point, not by
     instantiating the template clones.  */
  if (tree chain = DECL_CHAIN (gen_tmpl))
    if (DECL_P (chain) && DECL_CLONED_FUNCTION_P (chain))
      clone_cdtor (fndecl, /*update_methods=*/false);

  if (!access_ok)
    {
      if (!(complain & tf_error))
	{
	  /* Remember to reinstantiate when we're out of SFINAE so the user
	     can see the errors.  */
	  FNDECL_HAS_ACCESS_ERRORS (fndecl) = true;
	}
      return error_mark_node;
    }

  return fndecl;
}

/* Instantiate the alias template TMPL with ARGS.  Also push a template
   instantiation level, which instantiate_template doesn't do because
   functions and variables have sufficient context established by the
   callers.  */

static tree
instantiate_alias_template (tree tmpl, tree args, tsubst_flags_t complain)
{
  if (tmpl == error_mark_node || args == error_mark_node)
    return error_mark_node;

  args = coerce_template_parms (DECL_TEMPLATE_PARMS (tmpl),
				args, tmpl, complain);
  if (args == error_mark_node)
    return error_mark_node;

  /* FIXME check for satisfaction in check_instantiated_args.  */
  if (!constraints_satisfied_p (tmpl, args))
    {
      if (complain & tf_error)
	{
	  auto_diagnostic_group d;
	  error ("template constraint failure for %qD", tmpl);
	  diagnose_constraints (input_location, tmpl, args);
	}
      return error_mark_node;
    }

  if (!push_tinst_level (tmpl, args))
    return error_mark_node;
  tree r = instantiate_template (tmpl, args, complain);
  pop_tinst_level ();

  if (tree d = dependent_alias_template_spec_p (TREE_TYPE (r), nt_opaque))
    {
      /* An alias template specialization can be dependent
	 even if its underlying type is not.  */
      TYPE_DEPENDENT_P (d) = true;
      TYPE_DEPENDENT_P_VALID (d) = true;
      /* Sometimes a dependent alias spec is equivalent to its expansion,
	 sometimes not.  So always use structural_comptypes.  */
      SET_TYPE_STRUCTURAL_EQUALITY (d);
    }

  return r;
}

/* PARM is a template parameter pack for FN.  Returns true iff
   PARM is used in a deducible way in the argument list of FN.  */

static bool
pack_deducible_p (tree parm, tree fn)
{
  tree t = FUNCTION_FIRST_USER_PARMTYPE (fn);
  for (; t; t = TREE_CHAIN (t))
    {
      tree type = TREE_VALUE (t);
      tree packs;
      if (!PACK_EXPANSION_P (type))
	continue;
      for (packs = PACK_EXPANSION_PARAMETER_PACKS (type);
	   packs; packs = TREE_CHAIN (packs))
	if (template_args_equal (TREE_VALUE (packs), parm))
	  {
	    /* The template parameter pack is used in a function parameter
	       pack.  If this is the end of the parameter list, the
	       template parameter pack is deducible.  */
	    if (TREE_CHAIN (t) == void_list_node)
	      return true;
	    else
	      /* Otherwise, not.  Well, it could be deduced from
		 a non-pack parameter, but doing so would end up with
		 a deduction mismatch, so don't bother.  */
	      return false;
	  }
    }
  /* The template parameter pack isn't used in any function parameter
     packs, but it might be used deeper, e.g. tuple<Args...>.  */
  return true;
}

/* Subroutine of fn_type_unification: check non-dependent parms for
   convertibility.  */

static int
check_non_deducible_conversions (tree parms, const tree *args, unsigned nargs,
				 tree fn, unification_kind_t strict, int flags,
				 struct conversion **convs, bool explain_p)
{
  /* Non-constructor methods need to leave a conversion for 'this', which
     isn't included in nargs here.  */
  unsigned offset = (DECL_NONSTATIC_MEMBER_FUNCTION_P (fn)
		     && !DECL_CONSTRUCTOR_P (fn));

  for (unsigned ia = 0;
       parms && parms != void_list_node && ia < nargs; )
    {
      tree parm = TREE_VALUE (parms);

      if (TREE_CODE (parm) == TYPE_PACK_EXPANSION
	  && (!TREE_CHAIN (parms)
	      || TREE_CHAIN (parms) == void_list_node))
	/* For a function parameter pack that occurs at the end of the
	   parameter-declaration-list, the type A of each remaining
	   argument of the call is compared with the type P of the
	   declarator-id of the function parameter pack.  */
	break;

      parms = TREE_CHAIN (parms);

      if (TREE_CODE (parm) == TYPE_PACK_EXPANSION)
	/* For a function parameter pack that does not occur at the
	   end of the parameter-declaration-list, the type of the
	   parameter pack is a non-deduced context.  */
	continue;

      if (!uses_template_parms (parm))
	{
	  tree arg = args[ia];
	  conversion **conv_p = convs ? &convs[ia+offset] : NULL;
	  int lflags = conv_flags (ia, nargs, fn, arg, flags);

	  if (check_non_deducible_conversion (parm, arg, strict, lflags,
					      conv_p, explain_p))
	    return 1;
	}

      ++ia;
    }

  return 0;
}

/* The FN is a TEMPLATE_DECL for a function.  ARGS is an array with
   NARGS elements of the arguments that are being used when calling
   it.  TARGS is a vector into which the deduced template arguments
   are placed.

   Returns either a FUNCTION_DECL for the matching specialization of FN or
   NULL_TREE if no suitable specialization can be found.  If EXPLAIN_P is
   true, diagnostics will be printed to explain why it failed.

   If FN is a conversion operator, or we are trying to produce a specific
   specialization, RETURN_TYPE is the return type desired.

   The EXPLICIT_TARGS are explicit template arguments provided via a
   template-id.

   The parameter STRICT is one of:

   DEDUCE_CALL:
     We are deducing arguments for a function call, as in
     [temp.deduct.call].  If RETURN_TYPE is non-null, we are
     deducing arguments for a call to the result of a conversion
     function template, as in [over.call.object].

   DEDUCE_CONV:
     We are deducing arguments for a conversion function, as in
     [temp.deduct.conv].

   DEDUCE_EXACT:
     We are deducing arguments when doing an explicit instantiation
     as in [temp.explicit], when determining an explicit specialization
     as in [temp.expl.spec], or when taking the address of a function
     template, as in [temp.deduct.funcaddr].  */

tree
fn_type_unification (tree fn,
		     tree explicit_targs,
		     tree targs,
		     const tree *args,
		     unsigned int nargs,
		     tree return_type,
		     unification_kind_t strict,
		     int flags,
		     struct conversion **convs,
		     bool explain_p,
		     bool decltype_p)
{
  tree parms;
  tree fntype;
  tree decl = NULL_TREE;
  tsubst_flags_t complain = (explain_p ? tf_warning_or_error : tf_none);
  bool ok;
  static int deduction_depth;
  /* type_unification_real will pass back any access checks from default
     template argument substitution.  */
  vec<deferred_access_check, va_gc> *checks = NULL;
  /* We don't have all the template args yet.  */
  bool incomplete = true;

  tree orig_fn = fn;
  if (flag_new_inheriting_ctors)
    fn = strip_inheriting_ctors (fn);

  tree tparms = DECL_INNERMOST_TEMPLATE_PARMS (fn);
  tree r = error_mark_node;

  tree full_targs = targs;
  if (TMPL_ARGS_DEPTH (targs)
      < TMPL_PARMS_DEPTH (DECL_TEMPLATE_PARMS (fn)))
    full_targs = (add_outermost_template_args
		  (DECL_TI_ARGS (DECL_TEMPLATE_RESULT (fn)),
		   targs));

  if (decltype_p)
    complain |= tf_decltype;

  /* In C++0x, it's possible to have a function template whose type depends
     on itself recursively.  This is most obvious with decltype, but can also
     occur with enumeration scope (c++/48969).  So we need to catch infinite
     recursion and reject the substitution at deduction time; this function
     will return error_mark_node for any repeated substitution.

     This also catches excessive recursion such as when f<N> depends on
     f<N-1> across all integers, and returns error_mark_node for all the
     substitutions back up to the initial one.

     This is, of course, not reentrant.  */
  if (excessive_deduction_depth)
    return error_mark_node;
  ++deduction_depth;

  gcc_assert (TREE_CODE (fn) == TEMPLATE_DECL);

  fntype = TREE_TYPE (fn);
  if (explicit_targs)
    {
      /* [temp.deduct]

	 The specified template arguments must match the template
	 parameters in kind (i.e., type, nontype, template), and there
	 must not be more arguments than there are parameters;
	 otherwise type deduction fails.

	 Nontype arguments must match the types of the corresponding
	 nontype template parameters, or must be convertible to the
	 types of the corresponding nontype parameters as specified in
	 _temp.arg.nontype_, otherwise type deduction fails.

	 All references in the function type of the function template
	 to the corresponding template parameters are replaced by the
	 specified template argument values.  If a substitution in a
	 template parameter or in the function type of the function
	 template results in an invalid type, type deduction fails.  */
      int i, len = TREE_VEC_LENGTH (tparms);
      location_t loc = input_location;
      incomplete = false;

      if (explicit_targs == error_mark_node)
	goto fail;

      if (TMPL_ARGS_DEPTH (explicit_targs)
	  < TMPL_ARGS_DEPTH (full_targs))
	explicit_targs = add_outermost_template_args (full_targs,
						      explicit_targs);

      /* Adjust any explicit template arguments before entering the
	 substitution context.  */
      explicit_targs
	= (coerce_template_parms (tparms, explicit_targs, fn,
				  complain|tf_partial,
				  /*require_all_args=*/false));
      if (explicit_targs == error_mark_node)
	goto fail;

      /* Substitute the explicit args into the function type.  This is
	 necessary so that, for instance, explicitly declared function
	 arguments can match null pointed constants.  If we were given
	 an incomplete set of explicit args, we must not do semantic
	 processing during substitution as we could create partial
	 instantiations.  */
      for (i = 0; i < len; i++)
        {
          tree parm = TREE_VALUE (TREE_VEC_ELT (tparms, i));
          bool parameter_pack = false;
	  tree targ = TREE_VEC_ELT (explicit_targs, i);

          /* Dig out the actual parm.  */
          if (TREE_CODE (parm) == TYPE_DECL
              || TREE_CODE (parm) == TEMPLATE_DECL)
            {
              parm = TREE_TYPE (parm);
              parameter_pack = TEMPLATE_TYPE_PARAMETER_PACK (parm);
            }
          else if (TREE_CODE (parm) == PARM_DECL)
            {
              parm = DECL_INITIAL (parm);
              parameter_pack = TEMPLATE_PARM_PARAMETER_PACK (parm);
            }

	  if (targ == NULL_TREE)
	    /* No explicit argument for this template parameter.  */
	    incomplete = true;
	  else if (parameter_pack && pack_deducible_p (parm, fn))
            {
              /* Mark the argument pack as "incomplete". We could
                 still deduce more arguments during unification.
	         We remove this mark in type_unification_real.  */
	      ARGUMENT_PACK_INCOMPLETE_P(targ) = 1;
	      ARGUMENT_PACK_EXPLICIT_ARGS (targ)
		= ARGUMENT_PACK_ARGS (targ);

              /* We have some incomplete argument packs.  */
              incomplete = true;
            }
        }

      if (incomplete)
	{
	  if (!push_tinst_level (fn, explicit_targs))
	    {
	      excessive_deduction_depth = true;
	      goto fail;
	    }
	  ++processing_template_decl;
	  input_location = DECL_SOURCE_LOCATION (fn);
	  /* Ignore any access checks; we'll see them again in
	     instantiate_template and they might have the wrong
	     access path at this point.  */
	  push_deferring_access_checks (dk_deferred);
	  tsubst_flags_t ecomplain = complain | tf_partial | tf_fndecl_type;
	  fntype = tsubst (TREE_TYPE (fn), explicit_targs, ecomplain, NULL_TREE);
	  pop_deferring_access_checks ();
	  input_location = loc;
	  --processing_template_decl;
	  pop_tinst_level ();

	  if (fntype == error_mark_node)
	    goto fail;
	}

      /* Place the explicitly specified arguments in TARGS.  */
      explicit_targs = INNERMOST_TEMPLATE_ARGS (explicit_targs);
      for (i = NUM_TMPL_ARGS (explicit_targs); i--;)
	TREE_VEC_ELT (targs, i) = TREE_VEC_ELT (explicit_targs, i);
      if (!incomplete && CHECKING_P
	  && !NON_DEFAULT_TEMPLATE_ARGS_COUNT (targs))
	SET_NON_DEFAULT_TEMPLATE_ARGS_COUNT
	  (targs, NUM_TMPL_ARGS (explicit_targs));
    }

  if (return_type && strict != DEDUCE_CALL)
    {
      tree *new_args = XALLOCAVEC (tree, nargs + 1);
      new_args[0] = return_type;
      memcpy (new_args + 1, args, nargs * sizeof (tree));
      args = new_args;
      ++nargs;
    }

  if (!incomplete)
    goto deduced;

  /* Never do unification on the 'this' parameter.  */
  parms = skip_artificial_parms_for (fn, TYPE_ARG_TYPES (fntype));

  if (return_type && strict == DEDUCE_CALL)
    {
      /* We're deducing for a call to the result of a template conversion
         function.  The parms we really want are in return_type.  */
      if (INDIRECT_TYPE_P (return_type))
	return_type = TREE_TYPE (return_type);
      parms = TYPE_ARG_TYPES (return_type);
    }
  else if (return_type)
    {
      parms = tree_cons (NULL_TREE, TREE_TYPE (fntype), parms);
    }

  /* We allow incomplete unification without an error message here
     because the standard doesn't seem to explicitly prohibit it.  Our
     callers must be ready to deal with unification failures in any
     event.  */

  /* If we aren't explaining yet, push tinst context so we can see where
     any errors (e.g. from class instantiations triggered by instantiation
     of default template arguments) come from.  If we are explaining, this
     context is redundant.  */
  if (!explain_p && !push_tinst_level (fn, targs))
    {
      excessive_deduction_depth = true;
      goto fail;
    }

  ok = !type_unification_real (DECL_INNERMOST_TEMPLATE_PARMS (fn),
			       full_targs, parms, args, nargs, /*subr=*/0,
			       strict, &checks, explain_p);
  if (!explain_p)
    pop_tinst_level ();
  if (!ok)
    goto fail;

  /* Now that we have bindings for all of the template arguments,
     ensure that the arguments deduced for the template template
     parameters have compatible template parameter lists.  We cannot
     check this property before we have deduced all template
     arguments, because the template parameter types of a template
     template parameter might depend on prior template parameters
     deduced after the template template parameter.  The following
     ill-formed example illustrates this issue:

       template<typename T, template<T> class C> void f(C<5>, T);

       template<int N> struct X {};

       void g() {
         f(X<5>(), 5l); // error: template argument deduction fails
       }

     The template parameter list of 'C' depends on the template type
     parameter 'T', but 'C' is deduced to 'X' before 'T' is deduced to
     'long'.  Thus, we can't check that 'C' cannot bind to 'X' at the
     time that we deduce 'C'.  */
  if (!template_template_parm_bindings_ok_p
           (DECL_INNERMOST_TEMPLATE_PARMS (fn), targs))
    {
      unify_inconsistent_template_template_parameters (explain_p);
      goto fail;
    }

 deduced:

  /* CWG2369: Check satisfaction before non-deducible conversions.  */
  if (!constraints_satisfied_p (fn, targs))
    {
      if (explain_p)
	diagnose_constraints (DECL_SOURCE_LOCATION (fn), fn, targs);
      goto fail;
    }

  /* DR 1391: All parameters have args, now check non-dependent parms for
     convertibility.  We don't do this if all args were explicitly specified,
     as the standard says that we substitute explicit args immediately.  */
  if (incomplete
      && check_non_deducible_conversions (parms, args, nargs, fn, strict, flags,
					  convs, explain_p))
    goto fail;

  /* All is well so far.  Now, check:

     [temp.deduct]

     When all template arguments have been deduced, all uses of
     template parameters in nondeduced contexts are replaced with
     the corresponding deduced argument values.  If the
     substitution results in an invalid type, as described above,
     type deduction fails.  */
  if (!push_tinst_level (fn, targs))
    {
      excessive_deduction_depth = true;
      goto fail;
    }

  /* Also collect access checks from the instantiation.  */
  reopen_deferring_access_checks (checks);

  decl = instantiate_template (fn, targs, complain);

  checks = get_deferred_access_checks ();
  pop_deferring_access_checks ();

  pop_tinst_level ();

  if (decl == error_mark_node)
    goto fail;

  /* Now perform any access checks encountered during substitution.  */
  push_access_scope (decl);
  ok = perform_access_checks (checks, complain);
  pop_access_scope (decl);
  if (!ok)
    goto fail;

  /* If we're looking for an exact match, check that what we got
     is indeed an exact match.  It might not be if some template
     parameters are used in non-deduced contexts.  But don't check
     for an exact match if we have dependent template arguments;
     in that case we're doing partial ordering, and we already know
     that we have two candidates that will provide the actual type.  */
  if (strict == DEDUCE_EXACT && !any_dependent_template_arguments_p (targs))
    {
      tree substed = TREE_TYPE (decl);
      unsigned int i;

      tree sarg
	= skip_artificial_parms_for (decl, TYPE_ARG_TYPES (substed));
      if (return_type)
	sarg = tree_cons (NULL_TREE, TREE_TYPE (substed), sarg);
      for (i = 0; i < nargs && sarg; ++i, sarg = TREE_CHAIN (sarg))
	if (!same_type_p (args[i], TREE_VALUE (sarg)))
	  {
	    unify_type_mismatch (explain_p, args[i],
				 TREE_VALUE (sarg));
	    goto fail;
	  }
      if ((i < nargs || sarg)
	  /* add_candidates uses DEDUCE_EXACT for x.operator foo(), but args
	     doesn't contain the trailing void, and conv fns are always ().  */
	  && !DECL_CONV_FN_P (decl))
	{
	  unsigned nsargs = i + list_length (sarg);
	  unify_arity (explain_p, nargs, nsargs);
	  goto fail;
	}
    }

  /* After doing deduction with the inherited constructor, actually return an
     instantiation of the inheriting constructor.  */
  if (orig_fn != fn)
    decl = instantiate_template (orig_fn, targs, complain);

  r = decl;

 fail:
  --deduction_depth;
  if (excessive_deduction_depth)
    {
      if (deduction_depth == 0)
	/* Reset once we're all the way out.  */
	excessive_deduction_depth = false;
    }

  return r;
}

/* Returns true iff PARM is a forwarding reference in the context of
   template argument deduction for TMPL.  */

static bool
forwarding_reference_p (tree parm, tree tmpl)
{
  /* [temp.deduct.call], "A forwarding reference is an rvalue reference to a
     cv-unqualified template parameter ..."  */
  if (TYPE_REF_P (parm)
      && TYPE_REF_IS_RVALUE (parm)
      && TREE_CODE (TREE_TYPE (parm)) == TEMPLATE_TYPE_PARM
      && cp_type_quals (TREE_TYPE (parm)) == TYPE_UNQUALIFIED)
    {
      parm = TREE_TYPE (parm);
      /* [temp.deduct.call], "... that does not represent a template parameter
	 of a class template (during class template argument deduction)."  */
      if (tmpl
	  && deduction_guide_p (tmpl)
	  && DECL_ARTIFICIAL (tmpl))
	{
	  /* Since the template parameters of a synthesized guide consist of
	     the template parameters of the class template followed by those of
	     the constructor (if any), we can tell if PARM represents a template
	     parameter of the class template by comparing its index with the
	     arity of the class template.  */
	  tree ctmpl = CLASSTYPE_TI_TEMPLATE (TREE_TYPE (TREE_TYPE (tmpl)));
	  if (TEMPLATE_TYPE_IDX (parm)
	      < TREE_VEC_LENGTH (DECL_INNERMOST_TEMPLATE_PARMS (ctmpl)))
	    return false;
	}
      return true;
    }
  return false;
}

/* Adjust types before performing type deduction, as described in
   [temp.deduct.call] and [temp.deduct.conv].  The rules in these two
   sections are symmetric.  PARM is the type of a function parameter
   or the return type of the conversion function.  ARG is the type of
   the argument passed to the call, or the type of the value
   initialized with the result of the conversion function.
   ARG_EXPR is the original argument expression, which may be null.  */

static int
maybe_adjust_types_for_deduction (tree tparms,
				  unification_kind_t strict,
				  tree* parm,
				  tree* arg,
				  tree arg_expr)
{
  int result = 0;

  switch (strict)
    {
    case DEDUCE_CALL:
      break;

    case DEDUCE_CONV:
      /* [temp.deduct.conv] First remove a reference type on parm.
	 DRs 322 & 976 affected this.  */
      if (TYPE_REF_P (*parm))
	*parm = TREE_TYPE (*parm);

      /* Swap PARM and ARG throughout the remainder of this
	 function; the handling is precisely symmetric since PARM
	 will initialize ARG rather than vice versa.  */
      std::swap (parm, arg);

      break;

    case DEDUCE_EXACT:
      /* Core issue #873: Do the DR606 thing (see below) for these cases,
	 too, but here handle it by stripping the reference from PARM
	 rather than by adding it to ARG.  */
      if (forwarding_reference_p (*parm, TPARMS_PRIMARY_TEMPLATE (tparms))
	  && TYPE_REF_P (*arg)
	  && !TYPE_REF_IS_RVALUE (*arg))
	*parm = TREE_TYPE (*parm);
      /* Nothing else to do in this case.  */
      return 0;

    default:
      gcc_unreachable ();
    }

  if (!TYPE_REF_P (*parm))
    {
      /* [temp.deduct.call]

	 If P is not a reference type:

	 --If A is an array type, the pointer type produced by the
	 array-to-pointer standard conversion (_conv.array_) is
	 used in place of A for type deduction; otherwise,

	 --If A is a function type, the pointer type produced by
	 the function-to-pointer standard conversion
	 (_conv.func_) is used in place of A for type deduction;
	 otherwise,

	 --If A is a cv-qualified type, the top level
	 cv-qualifiers of A's type are ignored for type
	 deduction.  */
      if (TREE_CODE (*arg) == ARRAY_TYPE)
	*arg = build_pointer_type (TREE_TYPE (*arg));
      else if (TREE_CODE (*arg) == FUNCTION_TYPE)
	*arg = build_pointer_type (*arg);
      else
	*arg = TYPE_MAIN_VARIANT (*arg);
    }

  /* [temp.deduct.call], "If P is a forwarding reference and the argument is
     an lvalue, the type 'lvalue reference to A' is used in place of A for
     type deduction."  */
  if (forwarding_reference_p (*parm, TPARMS_PRIMARY_TEMPLATE (tparms))
      && (arg_expr ? lvalue_p (arg_expr)
	  /* try_one_overload doesn't provide an arg_expr, but
	     functions are always lvalues.  */
	  : TREE_CODE (*arg) == FUNCTION_TYPE))
    *arg = build_reference_type (*arg);

  /* [temp.deduct.call]

     If P is a cv-qualified type, the top level cv-qualifiers
     of P's type are ignored for type deduction.  If P is a
     reference type, the type referred to by P is used for
     type deduction.  */
  *parm = TYPE_MAIN_VARIANT (*parm);
  if (TYPE_REF_P (*parm))
    {
      *parm = TREE_TYPE (*parm);
      result |= UNIFY_ALLOW_OUTER_MORE_CV_QUAL;
    }

  return result;
}

/* Subroutine of fn_type_unification.  PARM is a function parameter of a
   template which doesn't contain any deducible template parameters; check if
   ARG is a suitable match for it.  STRICT, FLAGS and EXPLAIN_P are as in
   unify_one_argument.  */

static int
check_non_deducible_conversion (tree parm, tree arg, unification_kind_t strict,
				int flags, struct conversion **conv_p,
				bool explain_p)
{
  tree type;

  if (!TYPE_P (arg))
    type = TREE_TYPE (arg);
  else
    type = arg;

  if (same_type_p (parm, type))
    return unify_success (explain_p);

  tsubst_flags_t complain = (explain_p ? tf_warning_or_error : tf_none);
  if (strict == DEDUCE_CONV)
    {
      if (can_convert_arg (type, parm, NULL_TREE, flags, complain))
	return unify_success (explain_p);
    }
  else if (strict == DEDUCE_CALL)
    {
      bool ok = false;
      tree conv_arg = TYPE_P (arg) ? NULL_TREE : arg;
      if (conv_p)
	/* Avoid recalculating this in add_function_candidate.  */
	ok = (*conv_p
	      = good_conversion (parm, type, conv_arg, flags, complain));
      else
	ok = can_convert_arg (parm, type, conv_arg, flags, complain);
      if (ok)
	return unify_success (explain_p);
    }

  if (strict == DEDUCE_EXACT)
    return unify_type_mismatch (explain_p, parm, arg);
  else
    return unify_arg_conversion (explain_p, parm, type, arg);
}

static bool uses_deducible_template_parms (tree type);

/* Returns true iff the expression EXPR is one from which a template
   argument can be deduced.  In other words, if it's an undecorated
   use of a template non-type parameter.  */

static bool
deducible_expression (tree expr)
{
  /* Strip implicit conversions and implicit INDIRECT_REFs.  */
  while (CONVERT_EXPR_P (expr)
	 || TREE_CODE (expr) == VIEW_CONVERT_EXPR
	 || REFERENCE_REF_P (expr))
    expr = TREE_OPERAND (expr, 0);
  return (TREE_CODE (expr) == TEMPLATE_PARM_INDEX);
}

/* Returns true iff the array domain DOMAIN uses a template parameter in a
   deducible way; that is, if it has a max value of <PARM> - 1.  */

static bool
deducible_array_bound (tree domain)
{
  if (domain == NULL_TREE)
    return false;

  tree max = TYPE_MAX_VALUE (domain);
  if (TREE_CODE (max) != MINUS_EXPR)
    return false;

  return deducible_expression (TREE_OPERAND (max, 0));
}

/* Returns true iff the template arguments ARGS use a template parameter
   in a deducible way.  */

static bool
deducible_template_args (tree args)
{
  for (tree elt : tree_vec_range (args))
    {
      bool deducible;
      if (ARGUMENT_PACK_P (elt))
	deducible = deducible_template_args (ARGUMENT_PACK_ARGS (elt));
      else
	{
	  if (PACK_EXPANSION_P (elt))
	    elt = PACK_EXPANSION_PATTERN (elt);
	  if (TREE_CODE (elt) == TEMPLATE_TEMPLATE_PARM)
	    deducible = true;
	  else if (TYPE_P (elt))
	    deducible = uses_deducible_template_parms (elt);
	  else
	    deducible = deducible_expression (elt);
	}
      if (deducible)
	return true;
    }
  return false;
}

/* Returns true iff TYPE contains any deducible references to template
   parameters, as per 14.8.2.5.  */

static bool
uses_deducible_template_parms (tree type)
{
  if (PACK_EXPANSION_P (type))
    type = PACK_EXPANSION_PATTERN (type);

  /* T
     cv-list T
     TT<T>
     TT<i>
     TT<> */
  if (TREE_CODE (type) == TEMPLATE_TYPE_PARM
      || TREE_CODE (type) == BOUND_TEMPLATE_TEMPLATE_PARM)
    return true;

  /* T*
     T&
     T&&  */
  if (INDIRECT_TYPE_P (type))
    return uses_deducible_template_parms (TREE_TYPE (type));

  /* T[integer-constant ]
     type [i]  */
  if (TREE_CODE (type) == ARRAY_TYPE)
    return (uses_deducible_template_parms (TREE_TYPE (type))
	    || deducible_array_bound (TYPE_DOMAIN (type)));

  /* T type ::*
     type T::*
     T T::*
     T (type ::*)()
     type (T::*)()
     type (type ::*)(T)
     type (T::*)(T)
     T (type ::*)(T)
     T (T::*)()
     T (T::*)(T) */
  if (TYPE_PTRMEM_P (type))
    return (uses_deducible_template_parms (TYPE_PTRMEM_CLASS_TYPE (type))
	    || (uses_deducible_template_parms
		(TYPE_PTRMEM_POINTED_TO_TYPE (type))));

  /* template-name <T> (where template-name refers to a class template)
     template-name <i> (where template-name refers to a class template) */
  if (CLASS_TYPE_P (type)
      && CLASSTYPE_TEMPLATE_INFO (type)
      && PRIMARY_TEMPLATE_P (CLASSTYPE_TI_TEMPLATE (type)))
    return deducible_template_args (INNERMOST_TEMPLATE_ARGS
				    (CLASSTYPE_TI_ARGS (type)));

  /* type (T)
     T()
     T(T)  */
  if (FUNC_OR_METHOD_TYPE_P (type))
    {
      if (uses_deducible_template_parms (TREE_TYPE (type)))
	return true;
      tree parm = TYPE_ARG_TYPES (type);
      if (TREE_CODE (type) == METHOD_TYPE)
	parm = TREE_CHAIN (parm);
      for (; parm; parm = TREE_CHAIN (parm))
	if (uses_deducible_template_parms (TREE_VALUE (parm)))
	  return true;
      if (flag_noexcept_type
	  && TYPE_RAISES_EXCEPTIONS (type)
	  && TREE_PURPOSE (TYPE_RAISES_EXCEPTIONS (type))
	  && deducible_expression (TREE_PURPOSE (TYPE_RAISES_EXCEPTIONS (type))))
	return true;
    }

  return false;
}

/* Subroutine of type_unification_real and unify_pack_expansion to
   handle unification of a single P/A pair.  Parameters are as
   for those functions.  */

static int
unify_one_argument (tree tparms, tree targs, tree parm, tree arg,
		    int subr, unification_kind_t strict,
		    bool explain_p)
{
  tree arg_expr = NULL_TREE;
  int arg_strict;

  if (arg == error_mark_node || parm == error_mark_node)
    return unify_invalid (explain_p);
  if (arg == unknown_type_node)
    /* We can't deduce anything from this, but we might get all the
       template args from other function args.  */
    return unify_success (explain_p);

  /* Implicit conversions (Clause 4) will be performed on a function
     argument to convert it to the type of the corresponding function
     parameter if the parameter type contains no template-parameters that
     participate in template argument deduction.  */
  if (strict != DEDUCE_EXACT
      && TYPE_P (parm) && !uses_deducible_template_parms (parm))
    /* For function parameters with no deducible template parameters,
       just return.  We'll check non-dependent conversions later.  */
    return unify_success (explain_p);

  switch (strict)
    {
    case DEDUCE_CALL:
      arg_strict = (UNIFY_ALLOW_OUTER_LEVEL
		    | UNIFY_ALLOW_MORE_CV_QUAL
		    | UNIFY_ALLOW_DERIVED);
      break;

    case DEDUCE_CONV:
      arg_strict = UNIFY_ALLOW_LESS_CV_QUAL;
      break;

    case DEDUCE_EXACT:
      arg_strict = UNIFY_ALLOW_NONE;
      break;

    default:
      gcc_unreachable ();
    }

  /* We only do these transformations if this is the top-level
     parameter_type_list in a call or declaration matching; in other
     situations (nested function declarators, template argument lists) we
     won't be comparing a type to an expression, and we don't do any type
     adjustments.  */
  if (!subr)
    {
      if (!TYPE_P (arg))
	{
	  gcc_assert (TREE_TYPE (arg) != NULL_TREE);
	  if (type_unknown_p (arg))
	    {
	      /* [temp.deduct.type] A template-argument can be
		 deduced from a pointer to function or pointer
		 to member function argument if the set of
		 overloaded functions does not contain function
		 templates and at most one of a set of
		 overloaded functions provides a unique
		 match.  */
	      resolve_overloaded_unification (tparms, targs, parm,
					      arg, strict,
					      arg_strict, explain_p);
	      /* If a unique match was not found, this is a
	         non-deduced context, so we still succeed. */
	      return unify_success (explain_p);
	    }

	  arg_expr = arg;
	  arg = unlowered_expr_type (arg);
	  if (arg == error_mark_node)
	    return unify_invalid (explain_p);
	}

      arg_strict |= maybe_adjust_types_for_deduction (tparms, strict,
						      &parm, &arg, arg_expr);
    }
  else
    if ((TYPE_P (parm) || TREE_CODE (parm) == TEMPLATE_DECL)
	!= (TYPE_P (arg) || TREE_CODE (arg) == TEMPLATE_DECL))
      return unify_template_argument_mismatch (explain_p, parm, arg);

  /* For deduction from an init-list we need the actual list.  */
  if (arg_expr && BRACE_ENCLOSED_INITIALIZER_P (arg_expr))
    arg = arg_expr;
  return unify (tparms, targs, parm, arg, arg_strict, explain_p);
}

/* for_each_template_parm callback that always returns 0.  */

static int
zero_r (tree, void *)
{
  return 0;
}

/* for_each_template_parm any_fn callback to handle deduction of a template
   type argument from the type of an array bound.  */

static int
array_deduction_r (tree t, void *data)
{
  tree_pair_p d = (tree_pair_p)data;
  tree &tparms = d->purpose;
  tree &targs = d->value;

  if (TREE_CODE (t) == ARRAY_TYPE)
    if (tree dom = TYPE_DOMAIN (t))
      if (tree max = TYPE_MAX_VALUE (dom))
	{
	  if (TREE_CODE (max) == MINUS_EXPR)
	    max = TREE_OPERAND (max, 0);
	  if (TREE_CODE (max) == TEMPLATE_PARM_INDEX)
	    unify (tparms, targs, TREE_TYPE (max), size_type_node,
		   UNIFY_ALLOW_NONE, /*explain*/false);
	}

  /* Keep walking.  */
  return 0;
}

/* Try to deduce any not-yet-deduced template type arguments from the type of
   an array bound.  This is handled separately from unify because 14.8.2.5 says
   "The type of a type parameter is only deduced from an array bound if it is
   not otherwise deduced."  */

static void
try_array_deduction (tree tparms, tree targs, tree parm)
{
  tree_pair_s data = { tparms, targs };
  hash_set<tree> visited;
  for_each_template_parm (parm, zero_r, &data, &visited,
			  /*nondeduced*/false, array_deduction_r);
}

/* Most parms like fn_type_unification.

   If SUBR is 1, we're being called recursively (to unify the
   arguments of a function or method parameter of a function
   template).

   CHECKS is a pointer to a vector of access checks encountered while
   substituting default template arguments.  */

static int
type_unification_real (tree tparms,
		       tree full_targs,
		       tree xparms,
		       const tree *xargs,
		       unsigned int xnargs,
		       int subr,
		       unification_kind_t strict,
		       vec<deferred_access_check, va_gc> **checks,
		       bool explain_p)
{
  tree parm, arg;
  int i;
  int ntparms = TREE_VEC_LENGTH (tparms);
  int saw_undeduced = 0;
  tree parms;
  const tree *args;
  unsigned int nargs;
  unsigned int ia;

  gcc_assert (TREE_CODE (tparms) == TREE_VEC);
  gcc_assert (xparms == NULL_TREE || TREE_CODE (xparms) == TREE_LIST);
  gcc_assert (ntparms > 0);

  tree targs = INNERMOST_TEMPLATE_ARGS (full_targs);

  /* Reset the number of non-defaulted template arguments contained
     in TARGS.  */
  NON_DEFAULT_TEMPLATE_ARGS_COUNT (targs) = NULL_TREE;

 again:
  parms = xparms;
  args = xargs;
  nargs = xnargs;

  /* Only fn_type_unification cares about terminal void.  */
  if (nargs && args[nargs-1] == void_type_node)
    --nargs;

  ia = 0;
  while (parms && parms != void_list_node
	 && ia < nargs)
    {
      parm = TREE_VALUE (parms);

      if (TREE_CODE (parm) == TYPE_PACK_EXPANSION
	  && (!TREE_CHAIN (parms) || TREE_CHAIN (parms) == void_list_node))
	/* For a function parameter pack that occurs at the end of the
	   parameter-declaration-list, the type A of each remaining
	   argument of the call is compared with the type P of the
	   declarator-id of the function parameter pack.  */
	break;

      parms = TREE_CHAIN (parms);

      if (TREE_CODE (parm) == TYPE_PACK_EXPANSION)
	/* For a function parameter pack that does not occur at the
	   end of the parameter-declaration-list, the type of the
	   parameter pack is a non-deduced context.  */
	continue;

      arg = args[ia];
      ++ia;

      if (unify_one_argument (tparms, full_targs, parm, arg, subr, strict,
			      explain_p))
	return 1;
    }

  if (parms
      && parms != void_list_node
      && TREE_CODE (TREE_VALUE (parms)) == TYPE_PACK_EXPANSION)
    {
      /* Unify the remaining arguments with the pack expansion type.  */
      tree argvec;
      tree parmvec = make_tree_vec (1);

      /* Allocate a TREE_VEC and copy in all of the arguments */
      argvec = make_tree_vec (nargs - ia);
      for (i = 0; ia < nargs; ++ia, ++i)
	TREE_VEC_ELT (argvec, i) = args[ia];

      /* Copy the parameter into parmvec.  */
      TREE_VEC_ELT (parmvec, 0) = TREE_VALUE (parms);
      if (unify_pack_expansion (tparms, full_targs, parmvec, argvec, strict,
                                /*subr=*/subr, explain_p))
        return 1;

      /* Advance to the end of the list of parameters.  */
      parms = TREE_CHAIN (parms);
    }

  /* Fail if we've reached the end of the parm list, and more args
     are present, and the parm list isn't variadic.  */
  if (ia < nargs && parms == void_list_node)
    return unify_too_many_arguments (explain_p, nargs, ia);
  /* Fail if parms are left and they don't have default values and
     they aren't all deduced as empty packs (c++/57397).  This is
     consistent with sufficient_parms_p.  */
  if (parms && parms != void_list_node
      && TREE_PURPOSE (parms) == NULL_TREE)
    {
      unsigned int count = nargs;
      tree p = parms;
      bool type_pack_p;
      do
	{
	  type_pack_p = TREE_CODE (TREE_VALUE (p)) == TYPE_PACK_EXPANSION;
	  if (!type_pack_p)
	    count++;
	  p = TREE_CHAIN (p);
	}
      while (p && p != void_list_node);
      if (count != nargs)
	return unify_too_few_arguments (explain_p, ia, count,
					type_pack_p);
    }

  if (!subr)
    {
      tsubst_flags_t complain = (explain_p
				 ? tf_warning_or_error
				 : tf_none);
      bool tried_array_deduction = (cxx_dialect < cxx17);

      for (i = 0; i < ntparms; i++)
	{
	  tree targ = TREE_VEC_ELT (targs, i);
	  tree tparm = TREE_VEC_ELT (tparms, i);

	  /* Clear the "incomplete" flags on all argument packs now so that
	     substituting them into later default arguments works.  */
	  if (targ && ARGUMENT_PACK_P (targ))
            {
              ARGUMENT_PACK_INCOMPLETE_P (targ) = 0;
              ARGUMENT_PACK_EXPLICIT_ARGS (targ) = NULL_TREE;
            }

	  if (targ || tparm == error_mark_node)
	    continue;
	  tparm = TREE_VALUE (tparm);

	  if (TREE_CODE (tparm) == TYPE_DECL
	      && !tried_array_deduction)
	    {
	      try_array_deduction (tparms, targs, xparms);
	      tried_array_deduction = true;
	      if (TREE_VEC_ELT (targs, i))
		continue;
	    }

	  /* If this is an undeduced nontype parameter that depends on
	     a type parameter, try another pass; its type may have been
	     deduced from a later argument than the one from which
	     this parameter can be deduced.  */
	  if (TREE_CODE (tparm) == PARM_DECL
	      && !is_auto (TREE_TYPE (tparm))
	      && uses_template_parms (TREE_TYPE (tparm))
	      && saw_undeduced < 2)
	    {
	      saw_undeduced = 1;
	      continue;
	    }

	  /* Core issue #226 (C++0x) [temp.deduct]:

	     If a template argument has not been deduced, its
	     default template argument, if any, is used.

	     When we are in C++98 mode, TREE_PURPOSE will either
	     be NULL_TREE or ERROR_MARK_NODE, so we do not need
	     to explicitly check cxx_dialect here.  */
	  if (TREE_PURPOSE (TREE_VEC_ELT (tparms, i)))
	    /* OK, there is a default argument.  Wait until after the
	       conversion check to do substitution.  */
	    continue;

	  /* If the type parameter is a parameter pack, then it will
	     be deduced to an empty parameter pack.  */
	  if (template_parameter_pack_p (tparm))
	    {
	      tree arg;

	      if (TREE_CODE (tparm) == PARM_DECL)
		{
		  arg = make_node (NONTYPE_ARGUMENT_PACK);
		  TREE_CONSTANT (arg) = 1;
		}
	      else
		arg = cxx_make_type (TYPE_ARGUMENT_PACK);

	      ARGUMENT_PACK_ARGS (arg) = make_tree_vec (0);

	      TREE_VEC_ELT (targs, i) = arg;
	      continue;
	    }

	  return unify_parameter_deduction_failure (explain_p, tparm);
	}

      /* During partial ordering, we deduce dependent template args.  */
      bool any_dependent_targs = false;

      /* Now substitute into the default template arguments.  */
      for (i = 0; i < ntparms; i++)
	{
	  tree targ = TREE_VEC_ELT (targs, i);
	  tree tparm = TREE_VEC_ELT (tparms, i);

	  if (targ)
	    {
	      if (!any_dependent_targs && dependent_template_arg_p (targ))
		any_dependent_targs = true;
	      continue;
	    }
	  if (tparm == error_mark_node)
	    continue;

	  tree parm = TREE_VALUE (tparm);
	  tree arg = TREE_PURPOSE (tparm);
	  reopen_deferring_access_checks (*checks);
	  location_t save_loc = input_location;
	  if (DECL_P (parm))
	    input_location = DECL_SOURCE_LOCATION (parm);

	  if (saw_undeduced == 1
	      && TREE_CODE (parm) == PARM_DECL
	      && !is_auto (TREE_TYPE (parm))
	      && uses_template_parms (TREE_TYPE (parm)))
	    {
	      /* The type of this non-type parameter depends on undeduced
		 parameters.  Don't try to use its default argument yet,
		 since we might deduce an argument for it on the next pass,
		 but do check whether the arguments we already have cause
		 substitution failure, so that that happens before we try
		 later default arguments (78489).  */
	      ++processing_template_decl;
	      tree type = tsubst (TREE_TYPE (parm), full_targs, complain,
				  NULL_TREE);
	      --processing_template_decl;
	      if (type == error_mark_node)
		arg = error_mark_node;
	      else
		arg = NULL_TREE;
	    }
	  else
	    {
	      /* Even if the call is happening in template context, getting
		 here means it's non-dependent, and a default argument is
		 considered a separate definition under [temp.decls], so we can
		 do this substitution without processing_template_decl.  This
		 is important if the default argument contains something that
		 might be instantiation-dependent like access (87480).  */
	      processing_template_decl_sentinel s (!any_dependent_targs);
	      tree substed = NULL_TREE;
	      if (saw_undeduced == 1 && !any_dependent_targs)
		{
		  /* First instatiate in template context, in case we still
		     depend on undeduced template parameters.  */
		  ++processing_template_decl;
		  substed = tsubst_template_arg (arg, full_targs, complain,
						 NULL_TREE);
		  --processing_template_decl;
		  if (substed != error_mark_node
		      && !uses_template_parms (substed))
		    /* We replaced all the tparms, substitute again out of
		       template context.  */
		    substed = NULL_TREE;
		}
	      if (!substed)
		substed = tsubst_template_arg (arg, full_targs, complain,
					       NULL_TREE);

	      if (!uses_template_parms (substed))
		arg = convert_template_argument (parm, substed, full_targs,
						 complain, i, NULL_TREE);
	      else if (saw_undeduced == 1)
		arg = NULL_TREE;
	      else if (!any_dependent_targs)
		arg = error_mark_node;
	    }

	  input_location = save_loc;
	  *checks = get_deferred_access_checks ();
	  pop_deferring_access_checks ();

	  if (arg == error_mark_node)
	    return 1;
	  else if (arg)
	    {
	      TREE_VEC_ELT (targs, i) = arg;
	      /* The position of the first default template argument,
		 is also the number of non-defaulted arguments in TARGS.
		 Record that.  */
	      if (!NON_DEFAULT_TEMPLATE_ARGS_COUNT (targs))
		SET_NON_DEFAULT_TEMPLATE_ARGS_COUNT (targs, i);
	    }
	}

      if (saw_undeduced++ == 1)
	goto again;
    }

  if (CHECKING_P && !NON_DEFAULT_TEMPLATE_ARGS_COUNT (targs))
    SET_NON_DEFAULT_TEMPLATE_ARGS_COUNT (targs, TREE_VEC_LENGTH (targs));

  return unify_success (explain_p);
}

/* Subroutine of type_unification_real.  Args are like the variables
   at the call site.  ARG is an overloaded function (or template-id);
   we try deducing template args from each of the overloads, and if
   only one succeeds, we go with that.  Modifies TARGS and returns
   true on success.  */

static bool
resolve_overloaded_unification (tree tparms,
				tree targs,
				tree parm,
				tree arg,
				unification_kind_t strict,
				int sub_strict,
			        bool explain_p)
{
  tree tempargs = copy_node (targs);
  int good = 0;
  tree goodfn = NULL_TREE;
  bool addr_p;

  if (TREE_CODE (arg) == ADDR_EXPR)
    {
      arg = TREE_OPERAND (arg, 0);
      addr_p = true;
    }
  else
    addr_p = false;

  if (TREE_CODE (arg) == COMPONENT_REF)
    /* Handle `&x' where `x' is some static or non-static member
       function name.  */
    arg = TREE_OPERAND (arg, 1);

  if (TREE_CODE (arg) == OFFSET_REF)
    arg = TREE_OPERAND (arg, 1);

  /* Strip baselink information.  */
  if (BASELINK_P (arg))
    arg = BASELINK_FUNCTIONS (arg);

  if (TREE_CODE (arg) == TEMPLATE_ID_EXPR)
    {
      /* If we got some explicit template args, we need to plug them into
	 the affected templates before we try to unify, in case the
	 explicit args will completely resolve the templates in question.  */

      int ok = 0;
      tree expl_subargs = TREE_OPERAND (arg, 1);
      arg = TREE_OPERAND (arg, 0);

      for (lkp_iterator iter (arg); iter; ++iter)
	{
	  tree fn = *iter;
	  tree subargs, elem;

	  if (TREE_CODE (fn) != TEMPLATE_DECL)
	    continue;

	  subargs = coerce_template_parms (DECL_INNERMOST_TEMPLATE_PARMS (fn),
					   expl_subargs, NULL_TREE, tf_none);
	  if (subargs != error_mark_node
	      && !any_dependent_template_arguments_p (subargs))
	    {
	      fn = instantiate_template (fn, subargs, tf_none);
	      if (!constraints_satisfied_p (fn))
		continue;
	      if (undeduced_auto_decl (fn))
		{
		  /* Instantiate the function to deduce its return type.  */
		  ++function_depth;
		  instantiate_decl (fn, /*defer*/false, /*class*/false);
		  --function_depth;
		}

	      if (flag_noexcept_type)
		maybe_instantiate_noexcept (fn, tf_none);

	      elem = TREE_TYPE (fn);
	      if (try_one_overload (tparms, targs, tempargs, parm,
				    elem, strict, sub_strict, addr_p, explain_p)
		  && (!goodfn || !same_type_p (goodfn, elem)))
		{
		  goodfn = elem;
		  ++good;
		}
	    }
	  else if (subargs)
	    ++ok;
	}
      /* If no templates (or more than one) are fully resolved by the
	 explicit arguments, this template-id is a non-deduced context; it
	 could still be OK if we deduce all template arguments for the
	 enclosing call through other arguments.  */
      if (good != 1)
	good = ok;
    }
  else if (!OVL_P (arg))
    /* If ARG is, for example, "(0, &f)" then its type will be unknown
       -- but the deduction does not succeed because the expression is
       not just the function on its own.  */
    return false;
  else
    for (lkp_iterator iter (arg); iter; ++iter)
      {
	tree fn = *iter;
	if (try_one_overload (tparms, targs, tempargs, parm, TREE_TYPE (fn),
			      strict, sub_strict, addr_p, explain_p)
	    && (!goodfn || !decls_match (goodfn, fn)))
	  {
	    goodfn = fn;
	    ++good;
	  }
      }

  /* [temp.deduct.type] A template-argument can be deduced from a pointer
     to function or pointer to member function argument if the set of
     overloaded functions does not contain function templates and at most
     one of a set of overloaded functions provides a unique match.

     So if we found multiple possibilities, we return success but don't
     deduce anything.  */

  if (good == 1)
    {
      int i = TREE_VEC_LENGTH (targs);
      for (; i--; )
	if (TREE_VEC_ELT (tempargs, i))
	  {
	    tree old = TREE_VEC_ELT (targs, i);
	    tree new_ = TREE_VEC_ELT (tempargs, i);
	    if (new_ && old && ARGUMENT_PACK_P (old)
		&& ARGUMENT_PACK_EXPLICIT_ARGS (old))
	      /* Don't forget explicit template arguments in a pack.  */
	      ARGUMENT_PACK_EXPLICIT_ARGS (new_)
		= ARGUMENT_PACK_EXPLICIT_ARGS (old);
	    TREE_VEC_ELT (targs, i) = new_;
	  }
    }
  if (good)
    return true;

  return false;
}

/* Core DR 115: In contexts where deduction is done and fails, or in
   contexts where deduction is not done, if a template argument list is
   specified and it, along with any default template arguments, identifies
   a single function template specialization, then the template-id is an
   lvalue for the function template specialization.  */

tree
resolve_nondeduced_context (tree orig_expr, tsubst_flags_t complain)
{
  tree expr, offset, baselink;
  bool addr;

  if (!type_unknown_p (orig_expr))
    return orig_expr;

  expr = orig_expr;
  addr = false;
  offset = NULL_TREE;
  baselink = NULL_TREE;

  if (TREE_CODE (expr) == ADDR_EXPR)
    {
      expr = TREE_OPERAND (expr, 0);
      addr = true;
    }
  if (TREE_CODE (expr) == OFFSET_REF)
    {
      offset = expr;
      expr = TREE_OPERAND (expr, 1);
    }
  if (BASELINK_P (expr))
    {
      baselink = expr;
      expr = BASELINK_FUNCTIONS (expr);
    }

  if (TREE_CODE (expr) == TEMPLATE_ID_EXPR)
    {
      int good = 0;
      tree goodfn = NULL_TREE;

      /* If we got some explicit template args, we need to plug them into
	 the affected templates before we try to unify, in case the
	 explicit args will completely resolve the templates in question.  */

      tree expl_subargs = TREE_OPERAND (expr, 1);
      tree arg = TREE_OPERAND (expr, 0);
      tree badfn = NULL_TREE;
      tree badargs = NULL_TREE;

      for (lkp_iterator iter (arg); iter; ++iter)
	{
	  tree fn = *iter;
	  tree subargs, elem;

	  if (TREE_CODE (fn) != TEMPLATE_DECL)
	    continue;

	  subargs = coerce_template_parms (DECL_INNERMOST_TEMPLATE_PARMS (fn),
					   expl_subargs, NULL_TREE, tf_none);
	  if (subargs != error_mark_node
	      && !any_dependent_template_arguments_p (subargs))
	    {
	      elem = instantiate_template (fn, subargs, tf_none);
	      if (elem == error_mark_node)
		{
		  badfn = fn;
		  badargs = subargs;
		}
	      else if (elem && (!goodfn || !decls_match (goodfn, elem))
		       && constraints_satisfied_p (elem))
		{
		  goodfn = elem;
		  ++good;
		}
	    }
	}
      if (good == 1)
	{
	  mark_used (goodfn);
	  expr = goodfn;
	  if (baselink)
	    expr = build_baselink (BASELINK_BINFO (baselink),
				   BASELINK_ACCESS_BINFO (baselink),
				   expr, BASELINK_OPTYPE (baselink));
	  if (offset)
	    {
	      tree base
		= TYPE_MAIN_VARIANT (TREE_TYPE (TREE_OPERAND (offset, 0)));
	      expr = build_offset_ref (base, expr, addr, complain);
	    }
	  if (addr)
	    expr = cp_build_addr_expr (expr, complain);
	  return expr;
	}
      else if (good == 0 && badargs && (complain & tf_error))
	/* There were no good options and at least one bad one, so let the
	   user know what the problem is.  */
	instantiate_template (badfn, badargs, complain);
    }
  return orig_expr;
}

/* As above, but error out if the expression remains overloaded.  */

tree
resolve_nondeduced_context_or_error (tree exp, tsubst_flags_t complain)
{
  exp = resolve_nondeduced_context (exp, complain);
  if (type_unknown_p (exp))
    {
      if (complain & tf_error)
	cxx_incomplete_type_error (exp, TREE_TYPE (exp));
      return error_mark_node;
    }
  return exp;
}

/* Subroutine of resolve_overloaded_unification; does deduction for a single
   overload.  Fills TARGS with any deduced arguments, or error_mark_node if
   different overloads deduce different arguments for a given parm.
   ADDR_P is true if the expression for which deduction is being
   performed was of the form "& fn" rather than simply "fn".

   Returns 1 on success.  */

static int
try_one_overload (tree tparms,
		  tree orig_targs,
		  tree targs,
		  tree parm,
		  tree arg,
		  unification_kind_t strict,
		  int sub_strict,
		  bool addr_p,
		  bool explain_p)
{
  int nargs;
  tree tempargs;
  int i;

  if (arg == error_mark_node)
    return 0;

  /* [temp.deduct.type] A template-argument can be deduced from a pointer
     to function or pointer to member function argument if the set of
     overloaded functions does not contain function templates and at most
     one of a set of overloaded functions provides a unique match.

     So if this is a template, just return success.  */

  if (uses_template_parms (arg))
    return 1;

  if (TREE_CODE (arg) == METHOD_TYPE)
    arg = build_ptrmemfunc_type (build_pointer_type (arg));
  else if (addr_p)
    arg = build_pointer_type (arg);

  sub_strict |= maybe_adjust_types_for_deduction (tparms, strict,
						  &parm, &arg, NULL_TREE);

  /* We don't copy orig_targs for this because if we have already deduced
     some template args from previous args, unify would complain when we
     try to deduce a template parameter for the same argument, even though
     there isn't really a conflict.  */
  nargs = TREE_VEC_LENGTH (targs);
  tempargs = make_tree_vec (nargs);

  if (unify (tparms, tempargs, parm, arg, sub_strict, explain_p))
    return 0;

  /* First make sure we didn't deduce anything that conflicts with
     explicitly specified args.  */
  for (i = nargs; i--; )
    {
      tree elt = TREE_VEC_ELT (tempargs, i);
      tree oldelt = TREE_VEC_ELT (orig_targs, i);

      if (!elt)
	/*NOP*/;
      else if (uses_template_parms (elt))
	/* Since we're unifying against ourselves, we will fill in
	   template args used in the function parm list with our own
	   template parms.  Discard them.  */
	TREE_VEC_ELT (tempargs, i) = NULL_TREE;
      else if (oldelt && ARGUMENT_PACK_P (oldelt))
	{
	  /* Check that the argument at each index of the deduced argument pack
	     is equivalent to the corresponding explicitly specified argument.
	     We may have deduced more arguments than were explicitly specified,
	     and that's OK.  */

	  /* We used to assert ARGUMENT_PACK_INCOMPLETE_P (oldelt) here, but
	     that's wrong if we deduce the same argument pack from multiple
	     function arguments: it's only incomplete the first time.  */

	  tree explicit_pack = ARGUMENT_PACK_ARGS (oldelt);
	  tree deduced_pack = ARGUMENT_PACK_ARGS (elt);

	  if (TREE_VEC_LENGTH (deduced_pack)
	      < TREE_VEC_LENGTH (explicit_pack))
	    return 0;

	  for (int j = 0; j < TREE_VEC_LENGTH (explicit_pack); j++)
	    if (!template_args_equal (TREE_VEC_ELT (explicit_pack, j),
				      TREE_VEC_ELT (deduced_pack, j)))
	      return 0;
	}
      else if (oldelt && !template_args_equal (oldelt, elt))
	return 0;
    }

  for (i = nargs; i--; )
    {
      tree elt = TREE_VEC_ELT (tempargs, i);

      if (elt)
	TREE_VEC_ELT (targs, i) = elt;
    }

  return 1;
}

/* PARM is a template class (perhaps with unbound template
   parameters).  ARG is a fully instantiated type.  If ARG can be
   bound to PARM, return ARG, otherwise return NULL_TREE.  TPARMS and
   TARGS are as for unify.  */

static tree
try_class_unification (tree tparms, tree targs, tree parm, tree arg,
		       bool explain_p)
{
  if (!CLASSTYPE_SPECIALIZATION_OF_PRIMARY_TEMPLATE_P (arg))
    return NULL_TREE;
  else if (TREE_CODE (parm) == BOUND_TEMPLATE_TEMPLATE_PARM)
    /* Matches anything.  */;
  else if (most_general_template (CLASSTYPE_TI_TEMPLATE (arg))
	   != most_general_template (CLASSTYPE_TI_TEMPLATE (parm)))
    return NULL_TREE;

  /* We need to make a new template argument vector for the call to
     unify.  If we used TARGS, we'd clutter it up with the result of
     the attempted unification, even if this class didn't work out.
     We also don't want to commit ourselves to all the unifications
     we've already done, since unification is supposed to be done on
     an argument-by-argument basis.  In other words, consider the
     following pathological case:

       template <int I, int J, int K>
       struct S {};

       template <int I, int J>
       struct S<I, J, 2> : public S<I, I, I>, S<J, J, J> {};

       template <int I, int J, int K>
       void f(S<I, J, K>, S<I, I, I>);

       void g() {
	 S<0, 0, 0> s0;
	 S<0, 1, 2> s2;

	 f(s0, s2);
       }

     Now, by the time we consider the unification involving `s2', we
     already know that we must have `f<0, 0, 0>'.  But, even though
     `S<0, 1, 2>' is derived from `S<0, 0, 0>', the code is invalid
     because there are two ways to unify base classes of S<0, 1, 2>
     with S<I, I, I>.  If we kept the already deduced knowledge, we
     would reject the possibility I=1.  */
  targs = copy_template_args (targs);
  for (tree& targ : tree_vec_range (INNERMOST_TEMPLATE_ARGS (targs)))
    targ = NULL_TREE;

  int err;
  if (TREE_CODE (parm) == BOUND_TEMPLATE_TEMPLATE_PARM)
    err = unify_bound_ttp_args (tparms, targs, parm, arg, explain_p);
  else
    err = unify (tparms, targs, CLASSTYPE_TI_ARGS (parm),
		 CLASSTYPE_TI_ARGS (arg), UNIFY_ALLOW_NONE, explain_p);

  return err ? NULL_TREE : arg;
}

/* Given a template type PARM and a class type ARG, find the unique
   base type in ARG that is an instance of PARM.  We do not examine
   ARG itself; only its base-classes.  If there is not exactly one
   appropriate base class, return NULL_TREE.  PARM may be the type of
   a partial specialization, as well as a plain template type.  Used
   by unify.  */

static enum template_base_result
get_template_base (tree tparms, tree targs, tree parm, tree arg,
		   bool explain_p, tree *result)
{
  tree rval = NULL_TREE;
  tree binfo;

  gcc_assert (RECORD_OR_UNION_CODE_P (TREE_CODE (arg)));

  binfo = TYPE_BINFO (complete_type (arg));
  if (!binfo)
    {
      /* The type could not be completed.  */
      *result = NULL_TREE;
      return tbr_incomplete_type;
    }

  /* Walk in inheritance graph order.  The search order is not
     important, and this avoids multiple walks of virtual bases.  */
  for (binfo = TREE_CHAIN (binfo); binfo; binfo = TREE_CHAIN (binfo))
    {
      tree r = try_class_unification (tparms, targs, parm,
				      BINFO_TYPE (binfo), explain_p);

      if (r)
	{
	  /* If there is more than one satisfactory baseclass, then:

	       [temp.deduct.call]

	      If they yield more than one possible deduced A, the type
	      deduction fails.

	     applies.  */
	  if (rval && !same_type_p (r, rval))
	    {
	      /* [temp.deduct.call]/4.3: If there is a class C that is a
		 (direct or indirect) base class of D and derived (directly or
		 indirectly) from a class B and that would be a valid deduced
		 A, the deduced A cannot be B or pointer to B, respectively. */
	      if (DERIVED_FROM_P (r, rval))
		/* Ignore r.  */
		continue;
	      else if (DERIVED_FROM_P (rval, r))
		/* Ignore rval.  */;
	      else
		{
		  *result = NULL_TREE;
		  return tbr_ambiguous_baseclass;
		}
	    }

	  rval = r;
	}
    }

  *result = rval;
  return tbr_success;
}

/* Returns the level of DECL, which declares a template parameter.  */

static int
template_decl_level (tree decl)
{
  switch (TREE_CODE (decl))
    {
    case TYPE_DECL:
    case TEMPLATE_DECL:
      return TEMPLATE_TYPE_LEVEL (TREE_TYPE (decl));

    case PARM_DECL:
      return TEMPLATE_PARM_LEVEL (DECL_INITIAL (decl));

    default:
      gcc_unreachable ();
    }
  return 0;
}

/* Decide whether ARG can be unified with PARM, considering only the
   cv-qualifiers of each type, given STRICT as documented for unify.
   Returns nonzero iff the unification is OK on that basis.  */

static int
check_cv_quals_for_unify (int strict, tree arg, tree parm)
{
  int arg_quals = cp_type_quals (arg);
  int parm_quals = cp_type_quals (parm);

  if (TREE_CODE (parm) == TEMPLATE_TYPE_PARM
      && !(strict & UNIFY_ALLOW_OUTER_MORE_CV_QUAL))
    {
      /*  Although a CVR qualifier is ignored when being applied to a
	  substituted template parameter ([8.3.2]/1 for example), that
	  does not allow us to unify "const T" with "int&" because both
	  types are not of the form "cv-list T" [14.8.2.5 temp.deduct.type].
	  It is ok when we're allowing additional CV qualifiers
	  at the outer level [14.8.2.1]/3,1st bullet.  */
      if ((TYPE_REF_P (arg)
	   || FUNC_OR_METHOD_TYPE_P (arg))
	  && (parm_quals & (TYPE_QUAL_CONST | TYPE_QUAL_VOLATILE)))
	return 0;

      if ((!INDIRECT_TYPE_P (arg) && TREE_CODE (arg) != TEMPLATE_TYPE_PARM)
	  && (parm_quals & TYPE_QUAL_RESTRICT))
	return 0;
    }

  if (!(strict & (UNIFY_ALLOW_MORE_CV_QUAL | UNIFY_ALLOW_OUTER_MORE_CV_QUAL))
      && (arg_quals & parm_quals) != parm_quals)
    return 0;

  if (!(strict & (UNIFY_ALLOW_LESS_CV_QUAL | UNIFY_ALLOW_OUTER_LESS_CV_QUAL))
      && (parm_quals & arg_quals) != arg_quals)
    return 0;

  return 1;
}

/* Determines the LEVEL and INDEX for the template parameter PARM.  */
void
template_parm_level_and_index (tree parm, int* level, int* index)
{
  if (TREE_CODE (parm) == TEMPLATE_TYPE_PARM
      || TREE_CODE (parm) == TEMPLATE_TEMPLATE_PARM
      || TREE_CODE (parm) == BOUND_TEMPLATE_TEMPLATE_PARM)
    {
      *index = TEMPLATE_TYPE_IDX (parm);
      *level = TEMPLATE_TYPE_LEVEL (parm);
    }
  else
    {
      *index = TEMPLATE_PARM_IDX (parm);
      *level = TEMPLATE_PARM_LEVEL (parm);
    }
}

#define RECUR_AND_CHECK_FAILURE(TP, TA, P, A, S, EP)			\
  do {									\
    if (unify (TP, TA, P, A, S, EP))					\
      return 1;								\
  } while (0)

/* Unifies the remaining arguments in PACKED_ARGS with the pack
   expansion at the end of PACKED_PARMS. Returns 0 if the type
   deduction succeeds, 1 otherwise. STRICT is the same as in
   fn_type_unification. CALL_ARGS_P is true iff PACKED_ARGS is actually a
   function call argument list. We'll need to adjust the arguments to make them
   types. SUBR tells us if this is from a recursive call to
   type_unification_real, or for comparing two template argument
   lists. */

static int
unify_pack_expansion (tree tparms, tree targs, tree packed_parms,
                      tree packed_args, unification_kind_t strict,
                      bool subr, bool explain_p)
{
  tree parm
    = TREE_VEC_ELT (packed_parms, TREE_VEC_LENGTH (packed_parms) - 1);
  tree pattern = PACK_EXPANSION_PATTERN (parm);
  tree pack, packs = NULL_TREE;
  int i, start = TREE_VEC_LENGTH (packed_parms) - 1;

  /* Add in any args remembered from an earlier partial instantiation.  */
  targs = add_to_template_args (PACK_EXPANSION_EXTRA_ARGS (parm), targs);
  int levels = TMPL_ARGS_DEPTH (targs);

  packed_args = expand_template_argument_pack (packed_args);

  int len = TREE_VEC_LENGTH (packed_args);

  /* Determine the parameter packs we will be deducing from the
     pattern, and record their current deductions.  */
  for (pack = PACK_EXPANSION_PARAMETER_PACKS (parm);
       pack; pack = TREE_CHAIN (pack))
    {
      tree parm_pack = TREE_VALUE (pack);
      int idx, level;

      /* Only template parameter packs can be deduced, not e.g. function
	 parameter packs or __bases or __integer_pack.  */
      if (!TEMPLATE_PARM_P (parm_pack))
	continue;

      /* Determine the index and level of this parameter pack.  */
      template_parm_level_and_index (parm_pack, &level, &idx);
      if (level > levels)
	continue;

      /* Keep track of the parameter packs and their corresponding
         argument packs.  */
      packs = tree_cons (parm_pack, TMPL_ARG (targs, level, idx), packs);
      TREE_TYPE (packs) = make_tree_vec (len - start);
    }

  /* Loop through all of the arguments that have not yet been
     unified and unify each with the pattern.  */
  for (i = start; i < len; i++)
    {
      tree parm;
      bool any_explicit = false;
      tree arg = TREE_VEC_ELT (packed_args, i);

      /* For each parameter pack, set its TMPL_ARG to either NULL_TREE
	 or the element of its argument pack at the current index if
	 this argument was explicitly specified.  */
      for (pack = packs; pack; pack = TREE_CHAIN (pack))
        {
          int idx, level;
          tree arg, pargs;
          template_parm_level_and_index (TREE_PURPOSE (pack), &level, &idx);

          arg = NULL_TREE;
          if (TREE_VALUE (pack)
              && (pargs = ARGUMENT_PACK_EXPLICIT_ARGS (TREE_VALUE (pack)))
              && (i - start < TREE_VEC_LENGTH (pargs)))
            {
              any_explicit = true;
              arg = TREE_VEC_ELT (pargs, i - start);
            }
          TMPL_ARG (targs, level, idx) = arg;
        }

      /* If we had explicit template arguments, substitute them into the
	 pattern before deduction.  */
      if (any_explicit)
	{
	  /* Some arguments might still be unspecified or dependent.  */
	  bool dependent;
	  ++processing_template_decl;
	  dependent = any_dependent_template_arguments_p (targs);
	  if (!dependent)
	    --processing_template_decl;
	  parm = tsubst (pattern, targs,
			 explain_p ? tf_warning_or_error : tf_none,
			 NULL_TREE);
	  if (dependent)
	    --processing_template_decl;
	  if (parm == error_mark_node)
	    return 1;
	}
      else
	parm = pattern;

      /* Unify the pattern with the current argument.  */
      if (unify_one_argument (tparms, targs, parm, arg, subr, strict,
			      explain_p))
	return 1;

      /* For each parameter pack, collect the deduced value.  */
      for (pack = packs; pack; pack = TREE_CHAIN (pack))
        {
          int idx, level;
          template_parm_level_and_index (TREE_PURPOSE (pack), &level, &idx);

          TREE_VEC_ELT (TREE_TYPE (pack), i - start) = 
            TMPL_ARG (targs, level, idx);
        }
    }

  /* Verify that the results of unification with the parameter packs
     produce results consistent with what we've seen before, and make
     the deduced argument packs available.  */
  for (pack = packs; pack; pack = TREE_CHAIN (pack))
    {
      tree old_pack = TREE_VALUE (pack);
      tree new_args = TREE_TYPE (pack);
      int i, len = TREE_VEC_LENGTH (new_args);
      int idx, level;
      bool nondeduced_p = false;

      /* By default keep the original deduced argument pack.
	 If necessary, more specific code is going to update the
	 resulting deduced argument later down in this function.  */
      template_parm_level_and_index (TREE_PURPOSE (pack), &level, &idx);
      TMPL_ARG (targs, level, idx) = old_pack;

      /* If NEW_ARGS contains any NULL_TREE entries, we didn't
	 actually deduce anything.  */
      for (i = 0; i < len && !nondeduced_p; ++i)
	if (TREE_VEC_ELT (new_args, i) == NULL_TREE)
	  nondeduced_p = true;
      if (nondeduced_p)
	continue;

      if (old_pack && ARGUMENT_PACK_INCOMPLETE_P (old_pack))
        {
          /* If we had fewer function args than explicit template args,
             just use the explicits.  */
          tree explicit_args = ARGUMENT_PACK_EXPLICIT_ARGS (old_pack);
          int explicit_len = TREE_VEC_LENGTH (explicit_args);
          if (len < explicit_len)
            new_args = explicit_args;
        }

      if (!old_pack)
        {
          tree result;
          /* Build the deduced *_ARGUMENT_PACK.  */
          if (TREE_CODE (TREE_PURPOSE (pack)) == TEMPLATE_PARM_INDEX)
            {
              result = make_node (NONTYPE_ARGUMENT_PACK);
              TREE_CONSTANT (result) = 1;
            }
          else
	    result = cxx_make_type (TYPE_ARGUMENT_PACK);

	  ARGUMENT_PACK_ARGS (result) = new_args;

          /* Note the deduced argument packs for this parameter
             pack.  */
          TMPL_ARG (targs, level, idx) = result;
        }
      else if (ARGUMENT_PACK_INCOMPLETE_P (old_pack)
               && (ARGUMENT_PACK_ARGS (old_pack) 
                   == ARGUMENT_PACK_EXPLICIT_ARGS (old_pack)))
        {
          /* We only had the explicitly-provided arguments before, but
             now we have a complete set of arguments.  */
          tree explicit_args = ARGUMENT_PACK_EXPLICIT_ARGS (old_pack);

	  ARGUMENT_PACK_ARGS (old_pack) = new_args;
          ARGUMENT_PACK_INCOMPLETE_P (old_pack) = 1;
          ARGUMENT_PACK_EXPLICIT_ARGS (old_pack) = explicit_args;
        }
      else
	{
	  tree bad_old_arg = NULL_TREE, bad_new_arg = NULL_TREE;
	  tree old_args = ARGUMENT_PACK_ARGS (old_pack);
	  temp_override<int> ovl (TREE_VEC_LENGTH (old_args));
	  /* During template argument deduction for the aggregate deduction
	     candidate, the number of elements in a trailing parameter pack
	     is only deduced from the number of remaining function
	     arguments if it is not otherwise deduced.  */
	  if (cxx_dialect >= cxx20
	      && TREE_VEC_LENGTH (new_args) < TREE_VEC_LENGTH (old_args)
	      /* FIXME This isn't set properly for partial instantiations.  */
	      && TPARMS_PRIMARY_TEMPLATE (tparms)
	      && builtin_guide_p (TPARMS_PRIMARY_TEMPLATE (tparms)))
	    TREE_VEC_LENGTH (old_args) = TREE_VEC_LENGTH (new_args);
	  if (!comp_template_args (old_args, new_args,
				   &bad_old_arg, &bad_new_arg))
	    /* Inconsistent unification of this parameter pack.  */
	    return unify_parameter_pack_inconsistent (explain_p,
						      bad_old_arg,
						      bad_new_arg);
	}
    }

  return unify_success (explain_p);
}

/* Handle unification of the domain of an array.  PARM_DOM and ARG_DOM are
   INTEGER_TYPEs representing the TYPE_DOMAIN of ARRAY_TYPEs.  The other
   parameters and return value are as for unify.  */

static int
unify_array_domain (tree tparms, tree targs,
		    tree parm_dom, tree arg_dom,
		    bool explain_p)
{
  tree parm_max;
  tree arg_max;
  bool parm_cst;
  bool arg_cst;

  /* Our representation of array types uses "N - 1" as the
     TYPE_MAX_VALUE for an array with "N" elements, if "N" is
     not an integer constant.  We cannot unify arbitrarily
     complex expressions, so we eliminate the MINUS_EXPRs
     here.  */
  parm_max = TYPE_MAX_VALUE (parm_dom);
  parm_cst = TREE_CODE (parm_max) == INTEGER_CST;
  if (!parm_cst)
    {
      gcc_assert (TREE_CODE (parm_max) == MINUS_EXPR);
      parm_max = TREE_OPERAND (parm_max, 0);
    }
  arg_max = TYPE_MAX_VALUE (arg_dom);
  arg_cst = TREE_CODE (arg_max) == INTEGER_CST;
  if (!arg_cst)
    {
      /* The ARG_MAX may not be a simple MINUS_EXPR, if we are
	 trying to unify the type of a variable with the type
	 of a template parameter.  For example:

	   template <unsigned int N>
	   void f (char (&) [N]);
	   int g();
	   void h(int i) {
	     char a[g(i)];
	     f(a);
	   }

	 Here, the type of the ARG will be "int [g(i)]", and
	 may be a SAVE_EXPR, etc.  */
      if (TREE_CODE (arg_max) != MINUS_EXPR)
	return unify_vla_arg (explain_p, arg_dom);
      arg_max = TREE_OPERAND (arg_max, 0);
    }

  /* If only one of the bounds used a MINUS_EXPR, compensate
     by adding one to the other bound.  */
  if (parm_cst && !arg_cst)
    parm_max = fold_build2_loc (input_location, PLUS_EXPR,
				integer_type_node,
				parm_max,
				integer_one_node);
  else if (arg_cst && !parm_cst)
    arg_max = fold_build2_loc (input_location, PLUS_EXPR,
			       integer_type_node,
			       arg_max,
			       integer_one_node);

  return unify (tparms, targs, parm_max, arg_max,
		UNIFY_ALLOW_INTEGER, explain_p);
}

/* Returns whether T, a P or A in unify, is a type, template or expression.  */

enum pa_kind_t { pa_type, pa_tmpl, pa_expr };

static pa_kind_t
pa_kind (tree t)
{
  if (PACK_EXPANSION_P (t))
    t = PACK_EXPANSION_PATTERN (t);
  if (TREE_CODE (t) == TEMPLATE_TEMPLATE_PARM
      || TREE_CODE (t) == UNBOUND_CLASS_TEMPLATE
      || DECL_TYPE_TEMPLATE_P (t))
    return pa_tmpl;
  else if (TYPE_P (t))
    return pa_type;
  else
    return pa_expr;
}

/* Deduce the value of template parameters.  TPARMS is the (innermost)
   set of template parameters to a template.  TARGS is the bindings
   for those template parameters, as determined thus far; TARGS may
   include template arguments for outer levels of template parameters
   as well.  PARM is a parameter to a template function, or a
   subcomponent of that parameter; ARG is the corresponding argument.
   This function attempts to match PARM with ARG in a manner
   consistent with the existing assignments in TARGS.  If more values
   are deduced, then TARGS is updated.

   Returns 0 if the type deduction succeeds, 1 otherwise.  The
   parameter STRICT is a bitwise or of the following flags:

     UNIFY_ALLOW_NONE:
       Require an exact match between PARM and ARG.
     UNIFY_ALLOW_MORE_CV_QUAL:
       Allow the deduced ARG to be more cv-qualified (by qualification
       conversion) than ARG.
     UNIFY_ALLOW_LESS_CV_QUAL:
       Allow the deduced ARG to be less cv-qualified than ARG.
     UNIFY_ALLOW_DERIVED:
       Allow the deduced ARG to be a template base class of ARG,
       or a pointer to a template base class of the type pointed to by
       ARG.
     UNIFY_ALLOW_INTEGER:
       Allow any integral type to be deduced.  See the TEMPLATE_PARM_INDEX
       case for more information.
     UNIFY_ALLOW_OUTER_LEVEL:
       This is the outermost level of a deduction. Used to determine validity
       of qualification conversions. A valid qualification conversion must
       have const qualified pointers leading up to the inner type which
       requires additional CV quals, except at the outer level, where const
       is not required [conv.qual]. It would be normal to set this flag in
       addition to setting UNIFY_ALLOW_MORE_CV_QUAL.
     UNIFY_ALLOW_OUTER_MORE_CV_QUAL:
       This is the outermost level of a deduction, and PARM can be more CV
       qualified at this point.
     UNIFY_ALLOW_OUTER_LESS_CV_QUAL:
       This is the outermost level of a deduction, and PARM can be less CV
       qualified at this point.  */

static int
unify (tree tparms, tree targs, tree parm, tree arg, int strict,
       bool explain_p)
{
  int idx;
  tree targ;
  tree tparm;
  int strict_in = strict;
  tsubst_flags_t complain = (explain_p
			     ? tf_warning_or_error
			     : tf_none);

  /* I don't think this will do the right thing with respect to types.
     But the only case I've seen it in so far has been array bounds, where
     signedness is the only information lost, and I think that will be
     okay.  VIEW_CONVERT_EXPR can appear with class NTTP, thanks to
     finish_id_expression_1, and are also OK.  */
  while (CONVERT_EXPR_P (parm) || TREE_CODE (parm) == VIEW_CONVERT_EXPR)
    parm = TREE_OPERAND (parm, 0);

  if (arg == error_mark_node)
    return unify_invalid (explain_p);
  if (arg == unknown_type_node
      || arg == init_list_type_node)
    /* We can't deduce anything from this, but we might get all the
       template args from other function args.  */
    return unify_success (explain_p);

  if (parm == any_targ_node || arg == any_targ_node)
    return unify_success (explain_p);

  /* If PARM uses template parameters, then we can't bail out here,
     even if ARG == PARM, since we won't record unifications for the
     template parameters.  We might need them if we're trying to
     figure out which of two things is more specialized.  */
  if (arg == parm && !uses_template_parms (parm))
    return unify_success (explain_p);

  /* Handle init lists early, so the rest of the function can assume
     we're dealing with a type. */
  if (BRACE_ENCLOSED_INITIALIZER_P (arg))
    {
      tree elttype;
      tree orig_parm = parm;

      if (!is_std_init_list (parm)
	  && TREE_CODE (parm) != ARRAY_TYPE)
	/* We can only deduce from an initializer list argument if the
	   parameter is std::initializer_list or an array; otherwise this
	   is a non-deduced context. */
	return unify_success (explain_p);

      if (TREE_CODE (parm) == ARRAY_TYPE)
	elttype = TREE_TYPE (parm);
      else
	{
	  elttype = TREE_VEC_ELT (CLASSTYPE_TI_ARGS (parm), 0);
	  /* Deduction is defined in terms of a single type, so just punt
	     on the (bizarre) std::initializer_list<T...>.  */
	  if (PACK_EXPANSION_P (elttype))
	    return unify_success (explain_p);
	}

      if (strict != DEDUCE_EXACT
	  && TYPE_P (elttype)
	  && !uses_deducible_template_parms (elttype))
	/* If ELTTYPE has no deducible template parms, skip deduction from
	   the list elements.  */;
      else
	for (auto &e: CONSTRUCTOR_ELTS (arg))
	  {
	    tree elt = e.value;
	    int elt_strict = strict;

	    if (elt == error_mark_node)
	      return unify_invalid (explain_p);

	    if (!BRACE_ENCLOSED_INITIALIZER_P (elt))
	      {
		tree type = TREE_TYPE (elt);
		if (type == error_mark_node)
		  return unify_invalid (explain_p);
		/* It should only be possible to get here for a call.  */
		gcc_assert (elt_strict & UNIFY_ALLOW_OUTER_LEVEL);
		elt_strict |= maybe_adjust_types_for_deduction
		  (tparms, DEDUCE_CALL, &elttype, &type, elt);
		elt = type;
	      }

	  RECUR_AND_CHECK_FAILURE (tparms, targs, elttype, elt, elt_strict,
				   explain_p);
	}

      if (TREE_CODE (parm) == ARRAY_TYPE
	  && deducible_array_bound (TYPE_DOMAIN (parm)))
	{
	  /* Also deduce from the length of the initializer list.  */
	  tree max = size_int (CONSTRUCTOR_NELTS (arg));
	  tree idx = compute_array_index_type (NULL_TREE, max, tf_none);
	  if (idx == error_mark_node)
	    return unify_invalid (explain_p);
	  return unify_array_domain (tparms, targs, TYPE_DOMAIN (parm),
				     idx, explain_p);
	}

      /* If the std::initializer_list<T> deduction worked, replace the
	 deduced A with std::initializer_list<A>.  */
      if (orig_parm != parm)
	{
	  idx = TEMPLATE_TYPE_IDX (orig_parm);
	  targ = TREE_VEC_ELT (INNERMOST_TEMPLATE_ARGS (targs), idx);
	  targ = listify (targ);
	  TREE_VEC_ELT (INNERMOST_TEMPLATE_ARGS (targs), idx) = targ;
	}
      return unify_success (explain_p);
    }

  /* If parm and arg aren't the same kind of thing (template, type, or
     expression), fail early.  */
  if (pa_kind (parm) != pa_kind (arg))
    return unify_invalid (explain_p);

  /* Immediately reject some pairs that won't unify because of
     cv-qualification mismatches.  */
  if (TREE_CODE (arg) == TREE_CODE (parm)
      && TYPE_P (arg)
      /* It is the elements of the array which hold the cv quals of an array
	 type, and the elements might be template type parms. We'll check
	 when we recurse.  */
      && TREE_CODE (arg) != ARRAY_TYPE
      /* We check the cv-qualifiers when unifying with template type
	 parameters below.  We want to allow ARG `const T' to unify with
	 PARM `T' for example, when computing which of two templates
	 is more specialized, for example.  */
      && TREE_CODE (arg) != TEMPLATE_TYPE_PARM
      && !check_cv_quals_for_unify (strict_in, arg, parm))
    return unify_cv_qual_mismatch (explain_p, parm, arg);

  if (!(strict & UNIFY_ALLOW_OUTER_LEVEL)
      && TYPE_P (parm) && !CP_TYPE_CONST_P (parm)
      && !FUNC_OR_METHOD_TYPE_P (parm))
    strict &= ~UNIFY_ALLOW_MORE_CV_QUAL;
  /* PMFs recurse at the same level, so don't strip this yet.  */
  if (!TYPE_PTRMEMFUNC_P (parm))
    strict &= ~UNIFY_ALLOW_OUTER_LEVEL;
  strict &= ~UNIFY_ALLOW_DERIVED;
  strict &= ~UNIFY_ALLOW_OUTER_MORE_CV_QUAL;
  strict &= ~UNIFY_ALLOW_OUTER_LESS_CV_QUAL;

  switch (TREE_CODE (parm))
    {
    case TYPENAME_TYPE:
    case SCOPE_REF:
    case UNBOUND_CLASS_TEMPLATE:
      /* In a type which contains a nested-name-specifier, template
	 argument values cannot be deduced for template parameters used
	 within the nested-name-specifier.  */
      return unify_success (explain_p);

    case TEMPLATE_TYPE_PARM:
    case TEMPLATE_TEMPLATE_PARM:
    case BOUND_TEMPLATE_TEMPLATE_PARM:
      tparm = TREE_VALUE (TREE_VEC_ELT (tparms, 0));
      if (error_operand_p (tparm))
	return unify_invalid (explain_p);

      if (TEMPLATE_TYPE_LEVEL (parm)
	  != template_decl_level (tparm))
	/* The PARM is not one we're trying to unify.  Just check
	   to see if it matches ARG.  */
	{
	  if (TREE_CODE (arg) == TREE_CODE (parm)
	      && (is_auto (parm) ? is_auto (arg)
		  : same_type_p (parm, arg)))
	    return unify_success (explain_p);
	  else
	    return unify_type_mismatch (explain_p, parm, arg);
	}
      idx = TEMPLATE_TYPE_IDX (parm);
      targ = TREE_VEC_ELT (INNERMOST_TEMPLATE_ARGS (targs), idx);
      tparm = TREE_VALUE (TREE_VEC_ELT (tparms, idx));
      if (error_operand_p (tparm))
	return unify_invalid (explain_p);

      /* Check for mixed types and values.  */
      if ((TREE_CODE (parm) == TEMPLATE_TYPE_PARM
	   && TREE_CODE (tparm) != TYPE_DECL)
	  || (TREE_CODE (parm) == TEMPLATE_TEMPLATE_PARM
	      && TREE_CODE (tparm) != TEMPLATE_DECL))
	gcc_unreachable ();

      if (TREE_CODE (parm) == BOUND_TEMPLATE_TEMPLATE_PARM)
	{
	  if ((strict_in & UNIFY_ALLOW_DERIVED)
	      && CLASS_TYPE_P (arg))
	    {
	      /* First try to match ARG directly.  */
	      tree t = try_class_unification (tparms, targs, parm, arg,
					      explain_p);
	      if (!t)
		{
		  /* Otherwise, look for a suitable base of ARG, as below.  */
		  enum template_base_result r;
		  r = get_template_base (tparms, targs, parm, arg,
					 explain_p, &t);
		  if (!t)
		    return unify_no_common_base (explain_p, r, parm, arg);
		  arg = t;
		}
	    }
	  /* ARG must be constructed from a template class or a template
	     template parameter.  */
	  else if (TREE_CODE (arg) != BOUND_TEMPLATE_TEMPLATE_PARM
		   && !CLASSTYPE_SPECIALIZATION_OF_PRIMARY_TEMPLATE_P (arg))
	    return unify_template_deduction_failure (explain_p, parm, arg);

	  /* Deduce arguments T, i from TT<T> or TT<i>.  */
	  if (unify_bound_ttp_args (tparms, targs, parm, arg, explain_p))
	    return 1;

	  arg = TYPE_TI_TEMPLATE (arg);
	  if (DECL_TEMPLATE_TEMPLATE_PARM_P (arg))
	    /* If the template is a template template parameter, use the
	       TEMPLATE_TEMPLATE_PARM for matching.  */
	    arg = TREE_TYPE (arg);

	  /* Fall through to deduce template name.  */
	}

      if (TREE_CODE (parm) == TEMPLATE_TEMPLATE_PARM
	  || TREE_CODE (parm) == BOUND_TEMPLATE_TEMPLATE_PARM)
	{
	  /* Deduce template name TT from TT, TT<>, TT<T> and TT<i>.  */

	  /* Simple cases: Value already set, does match or doesn't.  */
	  if (targ != NULL_TREE && template_args_equal (targ, arg))
	    return unify_success (explain_p);
	  else if (targ)
	    return unify_inconsistency (explain_p, parm, targ, arg);
	}
      else
	{
	  /* If PARM is `const T' and ARG is only `int', we don't have
	     a match unless we are allowing additional qualification.
	     If ARG is `const int' and PARM is just `T' that's OK;
	     that binds `const int' to `T'.  */
	  if (!check_cv_quals_for_unify (strict_in | UNIFY_ALLOW_LESS_CV_QUAL,
					 arg, parm))
	    return unify_cv_qual_mismatch (explain_p, parm, arg);

	  /* Consider the case where ARG is `const volatile int' and
	     PARM is `const T'.  Then, T should be `volatile int'.  */
	  arg = cp_build_qualified_type
	    (arg, cp_type_quals (arg) & ~cp_type_quals (parm), tf_none);
	  if (arg == error_mark_node)
	    return unify_invalid (explain_p);

	  /* Simple cases: Value already set, does match or doesn't.  */
	  if (targ != NULL_TREE && same_type_p (targ, arg))
	    return unify_success (explain_p);
	  else if (targ)
	    return unify_inconsistency (explain_p, parm, targ, arg);

	  /* Make sure that ARG is not a variable-sized array.  (Note
	     that were talking about variable-sized arrays (like
	     `int[n]'), rather than arrays of unknown size (like
	     `int[]').)  We'll get very confused by such a type since
	     the bound of the array is not constant, and therefore
	     not mangleable.  Besides, such types are not allowed in
	     ISO C++, so we can do as we please here.  We do allow
	     them for 'auto' deduction, since that isn't ABI-exposed.  */
	  if (!is_auto (parm) && variably_modified_type_p (arg, NULL_TREE))
	    return unify_vla_arg (explain_p, arg);

	  /* Strip typedefs as in convert_template_argument.  */
	  arg = canonicalize_type_argument (arg, tf_none);
	}

      /* If ARG is a parameter pack or an expansion, we cannot unify
	 against it unless PARM is also a parameter pack.  */
      if ((template_parameter_pack_p (arg) || PACK_EXPANSION_P (arg))
	  && !template_parameter_pack_p (parm))
	return unify_parameter_pack_mismatch (explain_p, parm, arg);

      /* If the argument deduction results is a METHOD_TYPE,
         then there is a problem.
         METHOD_TYPE doesn't map to any real C++ type the result of
	 the deduction cannot be of that type.  */
      if (TREE_CODE (arg) == METHOD_TYPE)
	return unify_method_type_error (explain_p, arg);

      TREE_VEC_ELT (INNERMOST_TEMPLATE_ARGS (targs), idx) = arg;
      return unify_success (explain_p);

    case TEMPLATE_PARM_INDEX:
      tparm = TREE_VALUE (TREE_VEC_ELT (tparms, 0));
      if (error_operand_p (tparm))
	return unify_invalid (explain_p);

      if (TEMPLATE_PARM_LEVEL (parm)
	  != template_decl_level (tparm))
	{
	  /* The PARM is not one we're trying to unify.  Just check
	     to see if it matches ARG.  */
	  int result = !(TREE_CODE (arg) == TREE_CODE (parm)
			 && cp_tree_equal (parm, arg));
	  if (result)
	    unify_expression_unequal (explain_p, parm, arg);
	  return result;
	}

      idx = TEMPLATE_PARM_IDX (parm);
      targ = TREE_VEC_ELT (INNERMOST_TEMPLATE_ARGS (targs), idx);

      if (targ)
	{
	  if ((strict & UNIFY_ALLOW_INTEGER)
	      && TREE_TYPE (targ) && TREE_TYPE (arg)
	      && CP_INTEGRAL_TYPE_P (TREE_TYPE (targ)))
	    /* We're deducing from an array bound, the type doesn't matter.
	       This conversion should match the one below.  */
	    arg = fold (build_nop (TREE_TYPE (targ), arg));
	  int x = !cp_tree_equal (targ, arg);
	  if (x)
	    unify_inconsistency (explain_p, parm, targ, arg);
	  return x;
	}

      /* [temp.deduct.type] If, in the declaration of a function template
	 with a non-type template-parameter, the non-type
	 template-parameter is used in an expression in the function
	 parameter-list and, if the corresponding template-argument is
	 deduced, the template-argument type shall match the type of the
	 template-parameter exactly, except that a template-argument
	 deduced from an array bound may be of any integral type.
	 The non-type parameter might use already deduced type parameters.  */
      tparm = TREE_TYPE (parm);
      if (TEMPLATE_PARM_LEVEL (parm) > TMPL_ARGS_DEPTH (targs))
	/* We don't have enough levels of args to do any substitution.  This
	   can happen in the context of -fnew-ttp-matching.  */;
      else
	{
	  ++processing_template_decl;
	  tparm = tsubst (tparm, targs, tf_none, NULL_TREE);
	  --processing_template_decl;

	  if (tree a = type_uses_auto (tparm))
	    {
	      tparm = do_auto_deduction (tparm, arg, a,
					 complain, adc_unify, targs,
					 LOOKUP_NORMAL,
					 TPARMS_PRIMARY_TEMPLATE (tparms));
	      if (tparm == error_mark_node)
		return 1;
	    }
	}

      if (!TREE_TYPE (arg)
	  || TREE_CODE (TREE_TYPE (arg)) == DEPENDENT_OPERATOR_TYPE)
	/* Template-parameter dependent expression.  Just accept it for now.
	   It will later be processed in convert_template_argument.  */
	;
      else if (same_type_ignoring_top_level_qualifiers_p
	       (non_reference (TREE_TYPE (arg)),
		non_reference (tparm)))
	/* OK.  Ignore top-level quals here because a class-type template
	   parameter object is const.  */;
      else if ((strict & UNIFY_ALLOW_INTEGER)
	       && CP_INTEGRAL_TYPE_P (tparm))
	/* Convert the ARG to the type of PARM; the deduced non-type
	   template argument must exactly match the types of the
	   corresponding parameter.  This conversion should match the
	   one above.  */
	arg = fold (build_nop (tparm, arg));
      else if (uses_template_parms (tparm))
	{
	  /* We haven't deduced the type of this parameter yet.  */
	  if (cxx_dialect >= cxx17
	      /* We deduce from array bounds in try_array_deduction.  */
	      && !(strict & UNIFY_ALLOW_INTEGER)
	      && TEMPLATE_PARM_LEVEL (parm) <= TMPL_ARGS_DEPTH (targs))
	    {
	      /* Deduce it from the non-type argument.  As above, ignore
		 top-level quals here too.  */
	      tree atype = cv_unqualified (TREE_TYPE (arg));
	      RECUR_AND_CHECK_FAILURE (tparms, targs,
				       tparm, atype,
				       UNIFY_ALLOW_NONE, explain_p);
	      /* Now check whether the type of this parameter is still
		 dependent, and give up if so.  */
	      ++processing_template_decl;
	      tparm = tsubst (TREE_TYPE (parm), targs, tf_none, NULL_TREE);
	      --processing_template_decl;
	      if (uses_template_parms (tparm))
		return unify_success (explain_p);
	    }
	  else
	    /* Try again later.  */
	    return unify_success (explain_p);
	}
      else
	return unify_type_mismatch (explain_p, tparm, TREE_TYPE (arg));

      /* If ARG is a parameter pack or an expansion, we cannot unify
	 against it unless PARM is also a parameter pack.  */
      if ((template_parameter_pack_p (arg) || PACK_EXPANSION_P (arg))
	  && !TEMPLATE_PARM_PARAMETER_PACK (parm))
	return unify_parameter_pack_mismatch (explain_p, parm, arg);

      {
	bool removed_attr = false;
	arg = strip_typedefs_expr (arg, &removed_attr);
      }
      TREE_VEC_ELT (INNERMOST_TEMPLATE_ARGS (targs), idx) = arg;
      return unify_success (explain_p);

    case PTRMEM_CST:
     {
	/* A pointer-to-member constant can be unified only with
	 another constant.  */
      if (TREE_CODE (arg) != PTRMEM_CST)
	return unify_ptrmem_cst_mismatch (explain_p, parm, arg);

      /* Just unify the class member. It would be useless (and possibly
	 wrong, depending on the strict flags) to unify also
	 PTRMEM_CST_CLASS, because we want to be sure that both parm and
	 arg refer to the same variable, even if through different
	 classes. For instance:

	 struct A { int x; };
	 struct B : A { };

	 Unification of &A::x and &B::x must succeed.  */
      return unify (tparms, targs, PTRMEM_CST_MEMBER (parm),
		    PTRMEM_CST_MEMBER (arg), strict, explain_p);
     }

    case POINTER_TYPE:
      {
	if (!TYPE_PTR_P (arg))
	  return unify_type_mismatch (explain_p, parm, arg);

	/* [temp.deduct.call]

	   A can be another pointer or pointer to member type that can
	   be converted to the deduced A via a qualification
	   conversion (_conv.qual_).

	   We pass down STRICT here rather than UNIFY_ALLOW_NONE.
	   This will allow for additional cv-qualification of the
	   pointed-to types if appropriate.  */

	if (TREE_CODE (TREE_TYPE (arg)) == RECORD_TYPE)
	  /* The derived-to-base conversion only persists through one
	     level of pointers.  */
	  strict |= (strict_in & UNIFY_ALLOW_DERIVED);

	return unify (tparms, targs, TREE_TYPE (parm),
		      TREE_TYPE (arg), strict, explain_p);
      }

    case REFERENCE_TYPE:
      if (!TYPE_REF_P (arg))
	return unify_type_mismatch (explain_p, parm, arg);
      return unify (tparms, targs, TREE_TYPE (parm), TREE_TYPE (arg),
		    strict & UNIFY_ALLOW_MORE_CV_QUAL, explain_p);

    case ARRAY_TYPE:
      if (TREE_CODE (arg) != ARRAY_TYPE)
	return unify_type_mismatch (explain_p, parm, arg);
      if ((TYPE_DOMAIN (parm) == NULL_TREE)
	  != (TYPE_DOMAIN (arg) == NULL_TREE))
	return unify_type_mismatch (explain_p, parm, arg);
      RECUR_AND_CHECK_FAILURE (tparms, targs, TREE_TYPE (parm), TREE_TYPE (arg),
			       strict & UNIFY_ALLOW_MORE_CV_QUAL, explain_p);
      if (TYPE_DOMAIN (parm) != NULL_TREE)
	return unify_array_domain (tparms, targs, TYPE_DOMAIN (parm),
				   TYPE_DOMAIN (arg), explain_p);
      return unify_success (explain_p);

    case REAL_TYPE:
    case COMPLEX_TYPE:
    case VECTOR_TYPE:
    case INTEGER_TYPE:
    case BOOLEAN_TYPE:
    case ENUMERAL_TYPE:
    case VOID_TYPE:
    case OPAQUE_TYPE:
    case NULLPTR_TYPE:
      if (TREE_CODE (arg) != TREE_CODE (parm))
	return unify_type_mismatch (explain_p, parm, arg);

      /* We have already checked cv-qualification at the top of the
	 function.  */
      if (!same_type_ignoring_top_level_qualifiers_p (arg, parm))
	return unify_type_mismatch (explain_p, parm, arg);

      /* As far as unification is concerned, this wins.	 Later checks
	 will invalidate it if necessary.  */
      return unify_success (explain_p);

      /* Types INTEGER_CST and MINUS_EXPR can come from array bounds.  */
      /* Type INTEGER_CST can come from ordinary constant template args.  */
    case INTEGER_CST:
    case REAL_CST:
      while (CONVERT_EXPR_P (arg))
	arg = TREE_OPERAND (arg, 0);

      if (TREE_CODE (arg) != TREE_CODE (parm))
	return unify_template_argument_mismatch (explain_p, parm, arg);
      return (simple_cst_equal (parm, arg)
	      ? unify_success (explain_p)
	      : unify_template_argument_mismatch (explain_p, parm, arg));

    case TREE_VEC:
      {
	int i, len, argslen;
	int parm_variadic_p = 0;

	if (TREE_CODE (arg) != TREE_VEC)
	  return unify_template_argument_mismatch (explain_p, parm, arg);

	len = TREE_VEC_LENGTH (parm);
	argslen = TREE_VEC_LENGTH (arg);

	/* Check for pack expansions in the parameters.  */
	for (i = 0; i < len; ++i)
	  {
	    if (PACK_EXPANSION_P (TREE_VEC_ELT (parm, i)))
	      {
		if (i == len - 1)
		  /* We can unify against something with a trailing
		     parameter pack.  */
		  parm_variadic_p = 1;
		else
		  /* [temp.deduct.type]/9: If the template argument list of
		     P contains a pack expansion that is not the last
		     template argument, the entire template argument list
		     is a non-deduced context.  */
		  return unify_success (explain_p);
	      }
	  }

        /* If we don't have enough arguments to satisfy the parameters
           (not counting the pack expression at the end), or we have
           too many arguments for a parameter list that doesn't end in
           a pack expression, we can't unify.  */
	if (parm_variadic_p
	    ? argslen < len - parm_variadic_p
	    : argslen != len)
	  return unify_arity (explain_p, TREE_VEC_LENGTH (arg), len);

	/* Unify all of the parameters that precede the (optional)
	   pack expression.  */
	for (i = 0; i < len - parm_variadic_p; ++i)
	  {
	    RECUR_AND_CHECK_FAILURE (tparms, targs,
				     TREE_VEC_ELT (parm, i),
				     TREE_VEC_ELT (arg, i),
				     UNIFY_ALLOW_NONE, explain_p);
	  }
	if (parm_variadic_p)
	  return unify_pack_expansion (tparms, targs, parm, arg,
				       DEDUCE_EXACT,
				       /*subr=*/true, explain_p);
	return unify_success (explain_p);
      }

    case RECORD_TYPE:
    case UNION_TYPE:
      if (TREE_CODE (arg) != TREE_CODE (parm))
	return unify_type_mismatch (explain_p, parm, arg);

      if (TYPE_PTRMEMFUNC_P (parm))
	{
	  if (!TYPE_PTRMEMFUNC_P (arg))
	    return unify_type_mismatch (explain_p, parm, arg);

	  return unify (tparms, targs,
			TYPE_PTRMEMFUNC_FN_TYPE (parm),
			TYPE_PTRMEMFUNC_FN_TYPE (arg),
			strict, explain_p);
	}
      else if (TYPE_PTRMEMFUNC_P (arg))
	return unify_type_mismatch (explain_p, parm, arg);

      if (CLASSTYPE_TEMPLATE_INFO (parm))
	{
	  tree t = NULL_TREE;

	  if (strict_in & UNIFY_ALLOW_DERIVED)
	    {
	      /* First, we try to unify the PARM and ARG directly.  */
	      t = try_class_unification (tparms, targs,
					 parm, arg, explain_p);

	      if (!t)
		{
		  /* Fallback to the special case allowed in
		     [temp.deduct.call]:

		       If P is a class, and P has the form
		       template-id, then A can be a derived class of
		       the deduced A.  Likewise, if P is a pointer to
		       a class of the form template-id, A can be a
		       pointer to a derived class pointed to by the
		       deduced A.  */
		  enum template_base_result r;
		  r = get_template_base (tparms, targs, parm, arg,
					 explain_p, &t);

		  if (!t)
		    {
		      /* Don't give the derived diagnostic if we're
			 already dealing with the same template.  */
		      bool same_template
			= (CLASSTYPE_TEMPLATE_INFO (arg)
			   && (CLASSTYPE_TI_TEMPLATE (parm)
			       == CLASSTYPE_TI_TEMPLATE (arg)));
		      return unify_no_common_base (explain_p && !same_template,
						   r, parm, arg);
		    }
		}
	    }
	  else if (CLASSTYPE_TEMPLATE_INFO (arg)
		   && (CLASSTYPE_TI_TEMPLATE (parm)
		       == CLASSTYPE_TI_TEMPLATE (arg)))
	    /* Perhaps PARM is something like S<U> and ARG is S<int>.
	       Then, we should unify `int' and `U'.  */
	    t = arg;
	  else
	    /* There's no chance of unification succeeding.  */
	    return unify_type_mismatch (explain_p, parm, arg);

	  return unify (tparms, targs, CLASSTYPE_TI_ARGS (parm),
			CLASSTYPE_TI_ARGS (t), UNIFY_ALLOW_NONE, explain_p);
	}
      else if (!same_type_ignoring_top_level_qualifiers_p (parm, arg))
	return unify_type_mismatch (explain_p, parm, arg);
      return unify_success (explain_p);

    case METHOD_TYPE:
    case FUNCTION_TYPE:
      {
	unsigned int nargs;
	tree *args;
	tree a;
	unsigned int i;

	if (TREE_CODE (arg) != TREE_CODE (parm))
	  return unify_type_mismatch (explain_p, parm, arg);

	/* CV qualifications for methods can never be deduced, they must
	   match exactly.  We need to check them explicitly here,
	   because type_unification_real treats them as any other
	   cv-qualified parameter.  */
	if (TREE_CODE (parm) == METHOD_TYPE
	    && (!check_cv_quals_for_unify
		(UNIFY_ALLOW_NONE,
		 class_of_this_parm (arg),
		 class_of_this_parm (parm))))
	  return unify_cv_qual_mismatch (explain_p, parm, arg);
	if (TREE_CODE (arg) == FUNCTION_TYPE
	    && type_memfn_quals (parm) != type_memfn_quals (arg))
	  return unify_cv_qual_mismatch (explain_p, parm, arg);
	if (type_memfn_rqual (parm) != type_memfn_rqual (arg))
	  return unify_type_mismatch (explain_p, parm, arg);

	RECUR_AND_CHECK_FAILURE (tparms, targs, TREE_TYPE (parm),
				 TREE_TYPE (arg), UNIFY_ALLOW_NONE, explain_p);

	nargs = list_length (TYPE_ARG_TYPES (arg));
	args = XALLOCAVEC (tree, nargs);
	for (a = TYPE_ARG_TYPES (arg), i = 0;
	     a != NULL_TREE && a != void_list_node;
	     a = TREE_CHAIN (a), ++i)
	  args[i] = TREE_VALUE (a);
	nargs = i;

	if (type_unification_real (tparms, targs, TYPE_ARG_TYPES (parm),
				   args, nargs, 1, DEDUCE_EXACT,
				   NULL, explain_p))
	  return 1;

	if (flag_noexcept_type)
	  {
	    tree pspec = TYPE_RAISES_EXCEPTIONS (parm);
	    tree aspec = canonical_eh_spec (TYPE_RAISES_EXCEPTIONS (arg));
	    if (pspec == NULL_TREE) pspec = noexcept_false_spec;
	    if (aspec == NULL_TREE) aspec = noexcept_false_spec;
	    if (TREE_PURPOSE (pspec) && TREE_PURPOSE (aspec)
		&& uses_template_parms (TREE_PURPOSE (pspec)))
	      RECUR_AND_CHECK_FAILURE (tparms, targs, TREE_PURPOSE (pspec),
				       TREE_PURPOSE (aspec),
				       UNIFY_ALLOW_NONE, explain_p);
	    else
	      {
		bool pn = nothrow_spec_p (pspec);
		bool an = nothrow_spec_p (aspec);
		/* Here "less cv-qual" means the deduced arg (i.e. parm) has
		   /more/ noexcept, since function pointer conversions are the
		   reverse of qualification conversions.  */
		if (an == pn
		    || (an < pn && (strict & UNIFY_ALLOW_LESS_CV_QUAL))
		    || (an > pn && (strict & UNIFY_ALLOW_MORE_CV_QUAL)))
		  /* OK.  */;
		else
		  return unify_type_mismatch (explain_p, parm, arg);
	      }
	  }
	if (flag_tm)
	  {
	    /* As for noexcept.  */
	    bool pn = tx_safe_fn_type_p (parm);
	    bool an = tx_safe_fn_type_p (arg);
	    if (an == pn
		|| (an < pn && (strict & UNIFY_ALLOW_LESS_CV_QUAL))
		|| (an > pn && (strict & UNIFY_ALLOW_MORE_CV_QUAL)))
	      /* OK.  */;
	    else
	      return unify_type_mismatch (explain_p, parm, arg);
	  }

	return 0;
      }

    case OFFSET_TYPE:
      /* Unify a pointer to member with a pointer to member function, which
	 deduces the type of the member as a function type. */
      if (TYPE_PTRMEMFUNC_P (arg))
	{
	  /* Check top-level cv qualifiers */
	  if (!check_cv_quals_for_unify (UNIFY_ALLOW_NONE, arg, parm))
	    return unify_cv_qual_mismatch (explain_p, parm, arg);

	  RECUR_AND_CHECK_FAILURE (tparms, targs, TYPE_OFFSET_BASETYPE (parm),
				   TYPE_PTRMEMFUNC_OBJECT_TYPE (arg),
				   UNIFY_ALLOW_NONE, explain_p);

	  /* Determine the type of the function we are unifying against. */
	  tree fntype = static_fn_type (arg);

	  return unify (tparms, targs, TREE_TYPE (parm), fntype, strict, explain_p);
	}

      if (TREE_CODE (arg) != OFFSET_TYPE)
	return unify_type_mismatch (explain_p, parm, arg);
      RECUR_AND_CHECK_FAILURE (tparms, targs, TYPE_OFFSET_BASETYPE (parm),
			       TYPE_OFFSET_BASETYPE (arg),
			       UNIFY_ALLOW_NONE, explain_p);
      return unify (tparms, targs, TREE_TYPE (parm), TREE_TYPE (arg),
		    strict, explain_p);

    case CONST_DECL:
      if (DECL_TEMPLATE_PARM_P (parm))
	return unify (tparms, targs, DECL_INITIAL (parm), arg, strict, explain_p);
      if (arg != scalar_constant_value (parm))
	return unify_template_argument_mismatch (explain_p, parm, arg);
      return unify_success (explain_p);

    case FIELD_DECL:
    case TEMPLATE_DECL:
      /* Matched cases are handled by the ARG == PARM test above.  */
      return unify_template_argument_mismatch (explain_p, parm, arg);

    case VAR_DECL:
      /* We might get a variable as a non-type template argument in parm if the
	 corresponding parameter is type-dependent.  Make any necessary
	 adjustments based on whether arg is a reference.  */
      if (CONSTANT_CLASS_P (arg))
	parm = fold_non_dependent_expr (parm, complain);
      else if (REFERENCE_REF_P (arg))
	{
	  tree sub = TREE_OPERAND (arg, 0);
	  STRIP_NOPS (sub);
	  if (TREE_CODE (sub) == ADDR_EXPR)
	    arg = TREE_OPERAND (sub, 0);
	}
      /* Now use the normal expression code to check whether they match.  */
      goto expr;

    case TYPE_ARGUMENT_PACK:
    case NONTYPE_ARGUMENT_PACK:
      return unify (tparms, targs, ARGUMENT_PACK_ARGS (parm),
		    ARGUMENT_PACK_ARGS (arg), strict, explain_p);

    case TYPEOF_TYPE:
    case DECLTYPE_TYPE:
    case TRAIT_TYPE:
      /* Cannot deduce anything from TYPEOF_TYPE, DECLTYPE_TYPE,
	 or TRAIT_TYPE nodes.  */
      return unify_success (explain_p);

    case ERROR_MARK:
      /* Unification fails if we hit an error node.  */
      return unify_invalid (explain_p);

    case INDIRECT_REF:
      if (REFERENCE_REF_P (parm))
	{
	  bool pexp = PACK_EXPANSION_P (arg);
	  if (pexp)
	    arg = PACK_EXPANSION_PATTERN (arg);
	  if (REFERENCE_REF_P (arg))
	    arg = TREE_OPERAND (arg, 0);
	  if (pexp)
	    arg = make_pack_expansion (arg, complain);
	  return unify (tparms, targs, TREE_OPERAND (parm, 0), arg,
			strict, explain_p);
	}
      /* FALLTHRU */

    default:
      /* An unresolved overload is a nondeduced context.  */
      if (is_overloaded_fn (parm) || type_unknown_p (parm))
	return unify_success (explain_p);
      gcc_assert (EXPR_P (parm)
		  || TREE_CODE (parm) == CONSTRUCTOR
		  || TREE_CODE (parm) == TRAIT_EXPR);
    expr:
      /* We must be looking at an expression.  This can happen with
	 something like:

	   template <int I>
	   void foo(S<I>, S<I + 2>);

	 or

	   template<typename T>
	   void foo(A<T, T{}>);

	 This is a "non-deduced context":

	   [deduct.type]

	   The non-deduced contexts are:

	   --A non-type template argument or an array bound in which
	     a subexpression references a template parameter.

	 In these cases, we assume deduction succeeded, but don't
	 actually infer any unifications.  */

      if (!uses_template_parms (parm)
	  && !template_args_equal (parm, arg))
	return unify_expression_unequal (explain_p, parm, arg);
      else
	return unify_success (explain_p);
    }
}
#undef RECUR_AND_CHECK_FAILURE

/* Note that DECL can be defined in this translation unit, if
   required.  */

static void
mark_definable (tree decl)
{
  tree clone;
  DECL_NOT_REALLY_EXTERN (decl) = 1;
  FOR_EACH_CLONE (clone, decl)
    DECL_NOT_REALLY_EXTERN (clone) = 1;
}

/* Called if RESULT is explicitly instantiated, or is a member of an
   explicitly instantiated class.  */

void
mark_decl_instantiated (tree result, int extern_p)
{
  SET_DECL_EXPLICIT_INSTANTIATION (result);

  /* If this entity has already been written out, it's too late to
     make any modifications.  */
  if (TREE_ASM_WRITTEN (result))
    return;

  /* consteval functions are never emitted.  */
  if (TREE_CODE (result) == FUNCTION_DECL
      && DECL_IMMEDIATE_FUNCTION_P (result))
    return;

  /* For anonymous namespace we don't need to do anything.  */
  if (decl_internal_context_p (result))
    {
      gcc_assert (!TREE_PUBLIC (result));
      return;
    }

  if (TREE_CODE (result) != FUNCTION_DECL)
    /* The TREE_PUBLIC flag for function declarations will have been
       set correctly by tsubst.  */
    TREE_PUBLIC (result) = 1;

  if (extern_p)
    {
      DECL_EXTERNAL (result) = 1;
      DECL_NOT_REALLY_EXTERN (result) = 0;
    }
  else
    {
      mark_definable (result);
      mark_needed (result);
      /* Always make artificials weak.  */
      if (DECL_ARTIFICIAL (result) && flag_weak)
	comdat_linkage (result);
      /* For WIN32 we also want to put explicit instantiations in
	 linkonce sections.  */
      else if (TREE_PUBLIC (result))
	maybe_make_one_only (result);
      if (TREE_CODE (result) == FUNCTION_DECL
	  && DECL_TEMPLATE_INSTANTIATED (result))
	/* If the function has already been instantiated, clear DECL_EXTERNAL,
	   since start_preparsed_function wouldn't have if we had an earlier
	   extern explicit instantiation.  */
	DECL_EXTERNAL (result) = 0;
    }

  /* If EXTERN_P, then this function will not be emitted -- unless
     followed by an explicit instantiation, at which point its linkage
     will be adjusted.  If !EXTERN_P, then this function will be
     emitted here.  In neither circumstance do we want
     import_export_decl to adjust the linkage.  */
  DECL_INTERFACE_KNOWN (result) = 1;
}

/* Subroutine of more_specialized_fn: check whether TARGS is missing any
   important template arguments.  If any are missing, we check whether
   they're important by using error_mark_node for substituting into any
   args that were used for partial ordering (the ones between ARGS and END)
   and seeing if it bubbles up.  */

static bool
check_undeduced_parms (tree targs, tree args, tree end)
{
  bool found = false;
  for (tree& targ : tree_vec_range (targs))
    if (targ == NULL_TREE)
      {
	found = true;
	targ = error_mark_node;
      }
  if (found)
    {
      tree substed = tsubst_arg_types (args, targs, end, tf_none, NULL_TREE);
      if (substed == error_mark_node)
	return true;
    }
  return false;
}

/* Given two function templates PAT1 and PAT2, return:

   1 if PAT1 is more specialized than PAT2 as described in [temp.func.order].
   -1 if PAT2 is more specialized than PAT1.
   0 if neither is more specialized.

   LEN indicates the number of parameters we should consider
   (defaulted parameters should not be considered).

   The 1998 std underspecified function template partial ordering, and
   DR214 addresses the issue.  We take pairs of arguments, one from
   each of the templates, and deduce them against each other.  One of
   the templates will be more specialized if all the *other*
   template's arguments deduce against its arguments and at least one
   of its arguments *does* *not* deduce against the other template's
   corresponding argument.  Deduction is done as for class templates.
   The arguments used in deduction have reference and top level cv
   qualifiers removed.  Iff both arguments were originally reference
   types *and* deduction succeeds in both directions, an lvalue reference
   wins against an rvalue reference and otherwise the template
   with the more cv-qualified argument wins for that pairing (if
   neither is more cv-qualified, they both are equal).  Unlike regular
   deduction, after all the arguments have been deduced in this way,
   we do *not* verify the deduced template argument values can be
   substituted into non-deduced contexts.

   The logic can be a bit confusing here, because we look at deduce1 and
   targs1 to see if pat2 is at least as specialized, and vice versa; if we
   can find template arguments for pat1 to make arg1 look like arg2, that
   means that arg2 is at least as specialized as arg1.  */

int
more_specialized_fn (tree pat1, tree pat2, int len)
{
  tree decl1 = DECL_TEMPLATE_RESULT (pat1);
  tree decl2 = DECL_TEMPLATE_RESULT (pat2);
  tree targs1 = make_tree_vec (DECL_NTPARMS (pat1));
  tree targs2 = make_tree_vec (DECL_NTPARMS (pat2));
  tree tparms1 = DECL_INNERMOST_TEMPLATE_PARMS (pat1);
  tree tparms2 = DECL_INNERMOST_TEMPLATE_PARMS (pat2);
  tree args1 = TYPE_ARG_TYPES (TREE_TYPE (decl1));
  tree args2 = TYPE_ARG_TYPES (TREE_TYPE (decl2));
  tree origs1, origs2;
  bool lose1 = false;
  bool lose2 = false;

  /* Remove the this parameter from non-static member functions.  If
     one is a non-static member function and the other is not a static
     member function, remove the first parameter from that function
     also.  This situation occurs for operator functions where we
     locate both a member function (with this pointer) and non-member
     operator (with explicit first operand).  */
  if (DECL_NONSTATIC_MEMBER_FUNCTION_P (decl1))
    {
      len--; /* LEN is the number of significant arguments for DECL1 */
      args1 = TREE_CHAIN (args1);
      if (!DECL_STATIC_FUNCTION_P (decl2))
	args2 = TREE_CHAIN (args2);
    }
  else if (DECL_NONSTATIC_MEMBER_FUNCTION_P (decl2))
    {
      args2 = TREE_CHAIN (args2);
      if (!DECL_STATIC_FUNCTION_P (decl1))
	{
	  len--;
	  args1 = TREE_CHAIN (args1);
	}
    }

  /* If only one is a conversion operator, they are unordered.  */
  if (DECL_CONV_FN_P (decl1) != DECL_CONV_FN_P (decl2))
    return 0;

  /* Consider the return type for a conversion function */
  if (DECL_CONV_FN_P (decl1))
    {
      args1 = tree_cons (NULL_TREE, TREE_TYPE (TREE_TYPE (decl1)), args1);
      args2 = tree_cons (NULL_TREE, TREE_TYPE (TREE_TYPE (decl2)), args2);
      len++;
    }

  processing_template_decl++;

  origs1 = args1;
  origs2 = args2;

  while (len--
	 /* Stop when an ellipsis is seen.  */
	 && args1 != NULL_TREE && args2 != NULL_TREE)
    {
      tree arg1 = TREE_VALUE (args1);
      tree arg2 = TREE_VALUE (args2);
      int deduce1, deduce2;
      int quals1 = -1;
      int quals2 = -1;
      int ref1 = 0;
      int ref2 = 0;

      if (TREE_CODE (arg1) == TYPE_PACK_EXPANSION
          && TREE_CODE (arg2) == TYPE_PACK_EXPANSION)
        {
          /* When both arguments are pack expansions, we need only
             unify the patterns themselves.  */
          arg1 = PACK_EXPANSION_PATTERN (arg1);
          arg2 = PACK_EXPANSION_PATTERN (arg2);

          /* This is the last comparison we need to do.  */
          len = 0;
        }

      if (TYPE_REF_P (arg1))
	{
	  ref1 = TYPE_REF_IS_RVALUE (arg1) + 1;
	  arg1 = TREE_TYPE (arg1);
	  quals1 = cp_type_quals (arg1);
	}

      if (TYPE_REF_P (arg2))
	{
	  ref2 = TYPE_REF_IS_RVALUE (arg2) + 1;
	  arg2 = TREE_TYPE (arg2);
	  quals2 = cp_type_quals (arg2);
	}

      arg1 = TYPE_MAIN_VARIANT (arg1);
      arg2 = TYPE_MAIN_VARIANT (arg2);

      if (TREE_CODE (arg1) == TYPE_PACK_EXPANSION)
        {
          int i, len2 = remaining_arguments (args2);
          tree parmvec = make_tree_vec (1);
          tree argvec = make_tree_vec (len2);
          tree ta = args2;

          /* Setup the parameter vector, which contains only ARG1.  */
          TREE_VEC_ELT (parmvec, 0) = arg1;

          /* Setup the argument vector, which contains the remaining
             arguments.  */
          for (i = 0; i < len2; i++, ta = TREE_CHAIN (ta))
            TREE_VEC_ELT (argvec, i) = TREE_VALUE (ta);

          deduce1 = (unify_pack_expansion (tparms1, targs1, parmvec,
					   argvec, DEDUCE_EXACT,
					   /*subr=*/true, /*explain_p=*/false)
		     == 0);

          /* We cannot deduce in the other direction, because ARG1 is
             a pack expansion but ARG2 is not.  */
          deduce2 = 0;
        }
      else if (TREE_CODE (arg2) == TYPE_PACK_EXPANSION)
        {
          int i, len1 = remaining_arguments (args1);
          tree parmvec = make_tree_vec (1);
          tree argvec = make_tree_vec (len1);
          tree ta = args1;

          /* Setup the parameter vector, which contains only ARG1.  */
          TREE_VEC_ELT (parmvec, 0) = arg2;

          /* Setup the argument vector, which contains the remaining
             arguments.  */
          for (i = 0; i < len1; i++, ta = TREE_CHAIN (ta))
            TREE_VEC_ELT (argvec, i) = TREE_VALUE (ta);

          deduce2 = (unify_pack_expansion (tparms2, targs2, parmvec,
					   argvec, DEDUCE_EXACT,
					   /*subr=*/true, /*explain_p=*/false)
		     == 0);

          /* We cannot deduce in the other direction, because ARG2 is
             a pack expansion but ARG1 is not.*/
          deduce1 = 0;
        }

      else
        {
          /* The normal case, where neither argument is a pack
             expansion.  */
          deduce1 = (unify (tparms1, targs1, arg1, arg2,
			    UNIFY_ALLOW_NONE, /*explain_p=*/false)
		     == 0);
          deduce2 = (unify (tparms2, targs2, arg2, arg1,
			    UNIFY_ALLOW_NONE, /*explain_p=*/false)
		     == 0);
        }

      /* If we couldn't deduce arguments for tparms1 to make arg1 match
	 arg2, then arg2 is not as specialized as arg1.  */
      if (!deduce1)
	lose2 = true;
      if (!deduce2)
	lose1 = true;

      /* "If, for a given type, deduction succeeds in both directions
	 (i.e., the types are identical after the transformations above)
	 and both P and A were reference types (before being replaced with
	 the type referred to above):
	 - if the type from the argument template was an lvalue reference and
	 the type from the parameter template was not, the argument type is
	 considered to be more specialized than the other; otherwise,
	 - if the type from the argument template is more cv-qualified
	 than the type from the parameter template (as described above),
	 the argument type is considered to be more specialized than the other;
	 otherwise,
	 - neither type is more specialized than the other."  */

      if (deduce1 && deduce2)
	{
	  if (ref1 && ref2 && ref1 != ref2)
	    {
	      if (ref1 > ref2)
		lose1 = true;
	      else
		lose2 = true;
	    }
	  else if (quals1 != quals2 && quals1 >= 0 && quals2 >= 0)
	    {
	      if ((quals1 & quals2) == quals2)
		lose2 = true;
	      if ((quals1 & quals2) == quals1)
		lose1 = true;
	    }
	}

      if (lose1 && lose2)
	/* We've failed to deduce something in either direction.
	   These must be unordered.  */
	break;

      if (TREE_CODE (arg1) == TYPE_PACK_EXPANSION
          || TREE_CODE (arg2) == TYPE_PACK_EXPANSION)
        /* We have already processed all of the arguments in our
           handing of the pack expansion type.  */
        len = 0;

      args1 = TREE_CHAIN (args1);
      args2 = TREE_CHAIN (args2);
    }

  /* "In most cases, all template parameters must have values in order for
     deduction to succeed, but for partial ordering purposes a template
     parameter may remain without a value provided it is not used in the
     types being used for partial ordering."

     Thus, if we are missing any of the targs1 we need to substitute into
     origs1, then pat2 is not as specialized as pat1.  This can happen when
     there is a nondeduced context.  */
  if (!lose2 && check_undeduced_parms (targs1, origs1, args1))
    lose2 = true;
  if (!lose1 && check_undeduced_parms (targs2, origs2, args2))
    lose1 = true;

  processing_template_decl--;

  /* If both deductions succeed, the partial ordering selects the more
     constrained template.  */
  /* P2113: If the corresponding template-parameters of the
     template-parameter-lists are not equivalent ([temp.over.link]) or if
     the function parameters that positionally correspond between the two
     templates are not of the same type, neither template is more
     specialized than the other.  */
  if (!lose1 && !lose2
      && comp_template_parms (DECL_TEMPLATE_PARMS (pat1),
			      DECL_TEMPLATE_PARMS (pat2))
      && compparms (origs1, origs2))
    {
      int winner = more_constrained (decl1, decl2);
      if (winner > 0)
	lose2 = true;
      else if (winner < 0)
	lose1 = true;
    }

  /* All things being equal, if the next argument is a pack expansion
     for one function but not for the other, prefer the
     non-variadic function.  FIXME this is bogus; see c++/41958.  */
  if (lose1 == lose2
      && args1 && TREE_VALUE (args1)
      && args2 && TREE_VALUE (args2))
    {
      lose1 = TREE_CODE (TREE_VALUE (args1)) == TYPE_PACK_EXPANSION;
      lose2 = TREE_CODE (TREE_VALUE (args2)) == TYPE_PACK_EXPANSION;
    }

  if (lose1 == lose2)
    return 0;
  else if (!lose1)
    return 1;
  else
    return -1;
}

/* Determine which of two partial specializations of TMPL is more
   specialized.

   PAT1 is a TREE_LIST whose TREE_VALUE is the TEMPLATE_DECL corresponding
   to the first partial specialization.  The TREE_PURPOSE is the
   innermost set of template parameters for the partial
   specialization.  PAT2 is similar, but for the second template.

   Return 1 if the first partial specialization is more specialized;
   -1 if the second is more specialized; 0 if neither is more
   specialized.

   See [temp.class.order] for information about determining which of
   two templates is more specialized.  */

static int
more_specialized_partial_spec (tree tmpl, tree pat1, tree pat2)
{
  tree targs;
  int winner = 0;
  bool any_deductions = false;

  tree tmpl1 = TREE_VALUE (pat1);
  tree tmpl2 = TREE_VALUE (pat2);
  tree specargs1 = TI_ARGS (get_template_info (DECL_TEMPLATE_RESULT (tmpl1)));
  tree specargs2 = TI_ARGS (get_template_info (DECL_TEMPLATE_RESULT (tmpl2)));

  /* Just like what happens for functions, if we are ordering between
     different template specializations, we may encounter dependent
     types in the arguments, and we need our dependency check functions
     to behave correctly.  */
  ++processing_template_decl;
  targs = get_partial_spec_bindings (tmpl, tmpl1, specargs2);
  if (targs)
    {
      --winner;
      any_deductions = true;
    }

  targs = get_partial_spec_bindings (tmpl, tmpl2, specargs1);
  if (targs)
    {
      ++winner;
      any_deductions = true;
    }
  --processing_template_decl;

  /* If both deductions succeed, the partial ordering selects the more
     constrained template.  */
  if (!winner && any_deductions)
    winner = more_constrained (tmpl1, tmpl2);

  /* In the case of a tie where at least one of the templates
     has a parameter pack at the end, the template with the most
     non-packed parameters wins.  */
  if (winner == 0
      && any_deductions
      && (template_args_variadic_p (TREE_PURPOSE (pat1))
          || template_args_variadic_p (TREE_PURPOSE (pat2))))
    {
      tree args1 = INNERMOST_TEMPLATE_ARGS (TREE_PURPOSE (pat1));
      tree args2 = INNERMOST_TEMPLATE_ARGS (TREE_PURPOSE (pat2));
      int len1 = TREE_VEC_LENGTH (args1);
      int len2 = TREE_VEC_LENGTH (args2);

      /* We don't count the pack expansion at the end.  */
      if (template_args_variadic_p (TREE_PURPOSE (pat1)))
        --len1;
      if (template_args_variadic_p (TREE_PURPOSE (pat2)))
        --len2;

      if (len1 > len2)
        return 1;
      else if (len1 < len2)
        return -1;
    }

  return winner;
}

/* Return the template arguments that will produce the function signature
   DECL from the function template FN, with the explicit template
   arguments EXPLICIT_ARGS.  If CHECK_RETTYPE is true, the return type must
   also match.  Return NULL_TREE if no satisfactory arguments could be
   found.  */

static tree
get_bindings (tree fn, tree decl, tree explicit_args, bool check_rettype)
{
  int ntparms = DECL_NTPARMS (fn);
  tree targs = make_tree_vec (ntparms);
  tree decl_type = TREE_TYPE (decl);
  tree decl_arg_types;
  tree *args;
  unsigned int nargs, ix;
  tree arg;

  gcc_assert (decl != DECL_TEMPLATE_RESULT (fn));

  /* Never do unification on the 'this' parameter.  */
  decl_arg_types = skip_artificial_parms_for (decl,
					      TYPE_ARG_TYPES (decl_type));

  nargs = list_length (decl_arg_types);
  args = XALLOCAVEC (tree, nargs);
  for (arg = decl_arg_types, ix = 0;
       arg != NULL_TREE;
       arg = TREE_CHAIN (arg), ++ix)
    args[ix] = TREE_VALUE (arg);

  if (fn_type_unification (fn, explicit_args, targs,
			   args, ix,
			   (check_rettype || DECL_CONV_FN_P (fn)
			    ? TREE_TYPE (decl_type) : NULL_TREE),
			   DEDUCE_EXACT, LOOKUP_NORMAL, NULL,
			   /*explain_p=*/false,
			   /*decltype*/false)
      == error_mark_node)
    return NULL_TREE;

  return targs;
}

/* Return the innermost template arguments that, when applied to a partial
   specialization SPEC_TMPL of TMPL, yield the ARGS.

   For example, suppose we have:

     template <class T, class U> struct S {};
     template <class T> struct S<T*, int> {};

   Then, suppose we want to get `S<double*, int>'.  SPEC_TMPL will be the
   partial specialization and the ARGS will be {double*, int}.  The resulting
   vector will be {double}, indicating that `T' is bound to `double'.  */

static tree
get_partial_spec_bindings (tree tmpl, tree spec_tmpl, tree args)
{
  tree tparms = DECL_INNERMOST_TEMPLATE_PARMS (spec_tmpl);
  tree spec_args
    = TI_ARGS (get_template_info (DECL_TEMPLATE_RESULT (spec_tmpl)));
  int i, ntparms = TREE_VEC_LENGTH (tparms);
  tree deduced_args;
  tree innermost_deduced_args;

  innermost_deduced_args = make_tree_vec (ntparms);
  if (TMPL_ARGS_HAVE_MULTIPLE_LEVELS (args))
    {
      deduced_args = copy_node (args);
      SET_TMPL_ARGS_LEVEL (deduced_args,
			   TMPL_ARGS_DEPTH (deduced_args),
			   innermost_deduced_args);
    }
  else
    deduced_args = innermost_deduced_args;

  bool tried_array_deduction = (cxx_dialect < cxx17);
 again:
  if (unify (tparms, deduced_args,
	     INNERMOST_TEMPLATE_ARGS (spec_args),
	     INNERMOST_TEMPLATE_ARGS (args),
	     UNIFY_ALLOW_NONE, /*explain_p=*/false))
    return NULL_TREE;

  for (i =  0; i < ntparms; ++i)
    if (! TREE_VEC_ELT (innermost_deduced_args, i))
      {
	if (!tried_array_deduction)
	  {
	    try_array_deduction (tparms, innermost_deduced_args,
				 INNERMOST_TEMPLATE_ARGS (spec_args));
	    tried_array_deduction = true;
	    if (TREE_VEC_ELT (innermost_deduced_args, i))
	      goto again;
	  }
	return NULL_TREE;
      }

  if (!push_tinst_level (spec_tmpl, deduced_args))
    {
      excessive_deduction_depth = true;
      return NULL_TREE;
    }

  /* Verify that nondeduced template arguments agree with the type
     obtained from argument deduction.

     For example:

       struct A { typedef int X; };
       template <class T, class U> struct C {};
       template <class T> struct C<T, typename T::X> {};

     Then with the instantiation `C<A, int>', we can deduce that
     `T' is `A' but unify () does not check whether `typename T::X'
     is `int'.  */
  spec_args = tsubst (spec_args, deduced_args, tf_none, NULL_TREE);

  if (spec_args != error_mark_node)
    spec_args = coerce_template_parms (DECL_INNERMOST_TEMPLATE_PARMS (tmpl),
				       INNERMOST_TEMPLATE_ARGS (spec_args),
				       tmpl, tf_none, false);

  pop_tinst_level ();

  if (spec_args == error_mark_node
      /* We only need to check the innermost arguments; the other
	 arguments will always agree.  */
      || !comp_template_args_porder (INNERMOST_TEMPLATE_ARGS (spec_args),
				     INNERMOST_TEMPLATE_ARGS (args)))
    return NULL_TREE;

  /* Now that we have bindings for all of the template arguments,
     ensure that the arguments deduced for the template template
     parameters have compatible template parameter lists.  See the use
     of template_template_parm_bindings_ok_p in fn_type_unification
     for more information.  */
  if (!template_template_parm_bindings_ok_p (tparms, deduced_args))
    return NULL_TREE;

  return deduced_args;
}

// Compare two function templates T1 and T2 by deducing bindings
// from one against the other. If both deductions succeed, compare
// constraints to see which is more constrained.
static int
more_specialized_inst (tree t1, tree t2)
{
  int fate = 0;
  int count = 0;

  if (get_bindings (t1, DECL_TEMPLATE_RESULT (t2), NULL_TREE, true))
    {
      --fate;
      ++count;
    }

  if (get_bindings (t2, DECL_TEMPLATE_RESULT (t1), NULL_TREE, true))
    {
      ++fate;
      ++count;
    }

  // If both deductions succeed, then one may be more constrained.
  if (count == 2 && fate == 0)
    fate = more_constrained (t1, t2);

  return fate;
}

/* TEMPLATES is a TREE_LIST.  Each TREE_VALUE is a TEMPLATE_DECL.
   Return the TREE_LIST node with the most specialized template, if
   any.  If there is no most specialized template, the error_mark_node
   is returned.

   Note that this function does not look at, or modify, the
   TREE_PURPOSE or TREE_TYPE of any of the nodes.  Since the node
   returned is one of the elements of INSTANTIATIONS, callers may
   store information in the TREE_PURPOSE or TREE_TYPE of the nodes,
   and retrieve it from the value returned.  */

tree
most_specialized_instantiation (tree templates)
{
  tree fn, champ;

  ++processing_template_decl;

  champ = templates;
  for (fn = TREE_CHAIN (templates); fn; fn = TREE_CHAIN (fn))
    {
      gcc_assert (TREE_VALUE (champ) != TREE_VALUE (fn));
      int fate = more_specialized_inst (TREE_VALUE (champ), TREE_VALUE (fn));
      if (fate == -1)
	champ = fn;
      else if (!fate)
	{
	  /* Equally specialized, move to next function.  If there
	     is no next function, nothing's most specialized.  */
	  fn = TREE_CHAIN (fn);
	  champ = fn;
	  if (!fn)
	    break;
	}
    }

  if (champ)
    /* Now verify that champ is better than everything earlier in the
       instantiation list.  */
    for (fn = templates; fn != champ; fn = TREE_CHAIN (fn)) {
      if (more_specialized_inst (TREE_VALUE (champ), TREE_VALUE (fn)) != 1)
      {
        champ = NULL_TREE;
        break;
      }
    }

  processing_template_decl--;

  if (!champ)
    return error_mark_node;

  return champ;
}

/* If DECL is a specialization of some template, return the most
   general such template.  Otherwise, returns NULL_TREE.

   For example, given:

     template <class T> struct S { template <class U> void f(U); };

   if TMPL is `template <class U> void S<int>::f(U)' this will return
   the full template.  This function will not trace past partial
   specializations, however.  For example, given in addition:

     template <class T> struct S<T*> { template <class U> void f(U); };

   if TMPL is `template <class U> void S<int*>::f(U)' this will return
   `template <class T> template <class U> S<T*>::f(U)'.  */

tree
most_general_template (tree decl)
{
  if (TREE_CODE (decl) != TEMPLATE_DECL)
    {
      if (tree tinfo = get_template_info (decl))
	decl = TI_TEMPLATE (tinfo);
      /* The TI_TEMPLATE can be an IDENTIFIER_NODE for a
	 template friend, or a FIELD_DECL for a capture pack.  */
      if (TREE_CODE (decl) != TEMPLATE_DECL)
	return NULL_TREE;
    }

  if (DECL_TEMPLATE_TEMPLATE_PARM_P (decl))
    return DECL_TI_TEMPLATE (DECL_TEMPLATE_RESULT (decl));

  /* Look for more and more general templates.  */
  while (DECL_LANG_SPECIFIC (decl) && DECL_TEMPLATE_INFO (decl))
    {
      /* The DECL_TI_TEMPLATE can be an IDENTIFIER_NODE in some cases.
	 (See cp-tree.h for details.)  */
      if (TREE_CODE (DECL_TI_TEMPLATE (decl)) != TEMPLATE_DECL)
	break;

      if (CLASS_TYPE_P (TREE_TYPE (decl))
	  && !TYPE_DECL_ALIAS_P (TYPE_NAME (TREE_TYPE (decl)))
	  && CLASSTYPE_TEMPLATE_SPECIALIZATION (TREE_TYPE (decl)))
	break;

      /* Stop if we run into an explicitly specialized class template.  */
      if (!DECL_NAMESPACE_SCOPE_P (decl)
	  && DECL_CONTEXT (decl)
	  && CLASSTYPE_TEMPLATE_SPECIALIZATION (DECL_CONTEXT (decl)))
	break;

      decl = DECL_TI_TEMPLATE (decl);
    }

  return decl;
}

/* Return the most specialized of the template partial specializations
   which can produce TARGET, a specialization of some class or variable
   template.  The value returned is a TEMPLATE_INFO; the TI_TEMPLATE is a
   TEMPLATE_DECL node corresponding to the partial specialization, while
   the TI_ARGS is the set of template arguments that must be substituted
   into the template pattern in order to generate TARGET.  The result is
   cached in the TI_PARTIAL_INFO of the corresponding TEMPLATE_INFO unless
   RECHECKING is true.

   If the choice of partial specialization is ambiguous, a diagnostic
   is issued, and the error_mark_node is returned.  If there are no
   partial specializations matching TARGET, then NULL_TREE is
   returned, indicating that the primary template should be used.  */

tree
most_specialized_partial_spec (tree target, tsubst_flags_t complain,
			       bool rechecking /* = false */)
{
  tree tinfo = NULL_TREE;
  tree tmpl, args, decl;
  if (TYPE_P (target))
    {
      tinfo = CLASSTYPE_TEMPLATE_INFO (target);
      tmpl = TI_TEMPLATE (tinfo);
      args = TI_ARGS (tinfo);
      decl = TYPE_NAME (target);
    }
  else if (TREE_CODE (target) == TEMPLATE_ID_EXPR)
    {
      tmpl = TREE_OPERAND (target, 0);
      args = TREE_OPERAND (target, 1);
      decl = DECL_TEMPLATE_RESULT (tmpl);
    }
  else if (VAR_P (target))
    {
      tinfo = DECL_TEMPLATE_INFO (target);
      tmpl = TI_TEMPLATE (tinfo);
      args = TI_ARGS (tinfo);
      decl = target;
    }
  else
    gcc_unreachable ();

  if (!PRIMARY_TEMPLATE_P (tmpl))
    return NULL_TREE;

  if (!rechecking
      && tinfo
      && (VAR_P (target) || COMPLETE_TYPE_P (target)))
    return TI_PARTIAL_INFO (tinfo);

  tree main_tmpl = most_general_template (tmpl);
  tree specs = DECL_TEMPLATE_SPECIALIZATIONS (main_tmpl);
  if (!specs)
    /* There are no partial specializations of this template.  */
    return NULL_TREE;

  push_access_scope_guard pas (decl);
  deferring_access_check_sentinel acs (dk_no_deferred);

  /* For determining which partial specialization to use, only the
     innermost args are interesting.  */
  tree outer_args = NULL_TREE;
  if (TMPL_ARGS_HAVE_MULTIPLE_LEVELS (args))
    {
      outer_args = strip_innermost_template_args (args, 1);
      args = INNERMOST_TEMPLATE_ARGS (args);
    }

  /* The caller hasn't called push_to_top_level yet, but we need
     get_partial_spec_bindings to be done in non-template context so that we'll
     fully resolve everything.  */
  processing_template_decl_sentinel ptds;

  tree list = NULL_TREE;
  for (tree t = specs; t; t = TREE_CHAIN (t))
    {
      const tree ospec_tmpl = TREE_VALUE (t);

      tree spec_tmpl;
      if (outer_args)
	{
	  /* Substitute in the template args from the enclosing class.  */
	  ++processing_template_decl;
	  spec_tmpl = tsubst (ospec_tmpl, outer_args, tf_none, NULL_TREE);
	  --processing_template_decl;
	  if (spec_tmpl == error_mark_node)
	    return error_mark_node;
	}
      else
	spec_tmpl = ospec_tmpl;

      tree spec_args = get_partial_spec_bindings (tmpl, spec_tmpl, args);
      if (spec_args)
	{
	  if (outer_args)
	    spec_args = add_to_template_args (outer_args, spec_args);

	  /* Keep the candidate only if its constraints are satisfied.  */
	  if (constraints_satisfied_p (ospec_tmpl, spec_args))
	    list = tree_cons (spec_args, ospec_tmpl, list);
	}
    }

  if (! list)
    return NULL_TREE;

  tree champ = list;
  bool ambiguous_p = false;
  for (tree t = TREE_CHAIN (list); t; t = TREE_CHAIN (t))
    {
      int fate = more_specialized_partial_spec (tmpl, champ, t);
      if (fate == 1)
	;
      else
	{
	  if (fate == 0)
	    {
	      t = TREE_CHAIN (t);
	      if (! t)
		{
		  ambiguous_p = true;
		  break;
		}
	    }
	  champ = t;
	}
    }

  if (!ambiguous_p)
    for (tree t = list; t && t != champ; t = TREE_CHAIN (t))
      {
	int fate = more_specialized_partial_spec (tmpl, champ, t);
	if (fate != 1)
	  {
	    ambiguous_p = true;
	    break;
	  }
      }

  if (ambiguous_p)
    {
      const char *str;
      char *spaces = NULL;
      if (!(complain & tf_error))
	return error_mark_node;
      if (TYPE_P (target))
	error ("ambiguous template instantiation for %q#T", target);
      else
	error ("ambiguous template instantiation for %q#D", target);
      str = ngettext ("candidate is:", "candidates are:", list_length (list));
      for (tree t = list; t; t = TREE_CHAIN (t))
        {
	  tree subst = build_tree_list (TREE_VALUE (t), TREE_PURPOSE (t));
          inform (DECL_SOURCE_LOCATION (TREE_VALUE (t)),
		  "%s %#qS", spaces ? spaces : str, subst);
          spaces = spaces ? spaces : get_spaces (str);
        }
      free (spaces);
      return error_mark_node;
    }

  tree result = build_template_info (TREE_VALUE (champ), TREE_PURPOSE (champ));
  if (!rechecking && tinfo)
    TI_PARTIAL_INFO (tinfo) = result;
  return result;
}

/* Explicitly instantiate DECL.  */

void
do_decl_instantiation (tree decl, tree storage)
{
  tree result = NULL_TREE;
  int extern_p = 0;

  if (!decl || decl == error_mark_node)
    /* An error occurred, for which grokdeclarator has already issued
       an appropriate message.  */
    return;
  else if (! DECL_LANG_SPECIFIC (decl))
    {
      error ("explicit instantiation of non-template %q#D", decl);
      return;
    }
  else if (DECL_DECLARED_CONCEPT_P (decl))
    {
      if (VAR_P (decl))
	error ("explicit instantiation of variable concept %q#D", decl);
      else
	error ("explicit instantiation of function concept %q#D", decl);
      return;
    }

  bool var_templ = (DECL_TEMPLATE_INFO (decl)
                    && variable_template_p (DECL_TI_TEMPLATE (decl)));

  if (VAR_P (decl) && !var_templ)
    {
      /* There is an asymmetry here in the way VAR_DECLs and
	 FUNCTION_DECLs are handled by grokdeclarator.  In the case of
	 the latter, the DECL we get back will be marked as a
	 template instantiation, and the appropriate
	 DECL_TEMPLATE_INFO will be set up.  This does not happen for
	 VAR_DECLs so we do the lookup here.  Probably, grokdeclarator
	 should handle VAR_DECLs as it currently handles
	 FUNCTION_DECLs.  */
      if (!DECL_CLASS_SCOPE_P (decl))
	{
	  error ("%qD is not a static data member of a class template", decl);
	  return;
	}
      result = lookup_field (DECL_CONTEXT (decl), DECL_NAME (decl), 0, false);
      if (!result || !VAR_P (result))
	{
	  error ("no matching template for %qD found", decl);
	  return;
	}
      if (!same_type_p (TREE_TYPE (result), TREE_TYPE (decl)))
	{
	  error ("type %qT for explicit instantiation %qD does not match "
		 "declared type %qT", TREE_TYPE (result), decl,
		 TREE_TYPE (decl));
	  return;
	}
    }
  else if (TREE_CODE (decl) != FUNCTION_DECL && !var_templ)
    {
      error ("explicit instantiation of %q#D", decl);
      return;
    }
  else
    result = decl;

  /* Check for various error cases.  Note that if the explicit
     instantiation is valid the RESULT will currently be marked as an
     *implicit* instantiation; DECL_EXPLICIT_INSTANTIATION is not set
     until we get here.  */

  if (DECL_TEMPLATE_SPECIALIZATION (result))
    {
      /* DR 259 [temp.spec].

	 Both an explicit instantiation and a declaration of an explicit
	 specialization shall not appear in a program unless the explicit
	 instantiation follows a declaration of the explicit specialization.

	 For a given set of template parameters, if an explicit
	 instantiation of a template appears after a declaration of an
	 explicit specialization for that template, the explicit
	 instantiation has no effect.  */
      return;
    }
  else if (DECL_EXPLICIT_INSTANTIATION (result))
    {
      /* [temp.spec]

	 No program shall explicitly instantiate any template more
	 than once.

	 We check DECL_NOT_REALLY_EXTERN so as not to complain when
	 the first instantiation was `extern' and the second is not,
	 and EXTERN_P for the opposite case.  */
      if (DECL_NOT_REALLY_EXTERN (result) && !extern_p)
	permerror (input_location, "duplicate explicit instantiation of %q#D", result);
      /* If an "extern" explicit instantiation follows an ordinary
	 explicit instantiation, the template is instantiated.  */
      if (extern_p)
	return;
    }
  else if (!DECL_IMPLICIT_INSTANTIATION (result))
    {
      error ("no matching template for %qD found", result);
      return;
    }
  else if (!DECL_TEMPLATE_INFO (result))
    {
      permerror (input_location, "explicit instantiation of non-template %q#D", result);
      return;
    }

  if (storage == NULL_TREE)
    ;
  else if (storage == ridpointers[(int) RID_EXTERN])
    {
      if (cxx_dialect == cxx98)
	pedwarn (input_location, OPT_Wpedantic,
		 "ISO C++ 1998 forbids the use of %<extern%> on explicit "
		 "instantiations");
      extern_p = 1;
    }
  else
    error ("storage class %qD applied to template instantiation", storage);

  check_explicit_instantiation_namespace (result);
  mark_decl_instantiated (result, extern_p);
  if (! extern_p)
    instantiate_decl (result, /*defer_ok=*/true,
		      /*expl_inst_class_mem_p=*/false);
}

static void
mark_class_instantiated (tree t, int extern_p)
{
  SET_CLASSTYPE_EXPLICIT_INSTANTIATION (t);
  SET_CLASSTYPE_INTERFACE_KNOWN (t);
  CLASSTYPE_INTERFACE_ONLY (t) = extern_p;
  TYPE_DECL_SUPPRESS_DEBUG (TYPE_NAME (t)) = extern_p;
  if (! extern_p)
    {
      CLASSTYPE_DEBUG_REQUESTED (t) = 1;
      rest_of_type_compilation (t, 1);
    }
}

/* Perform an explicit instantiation of template class T.  STORAGE, if
   non-null, is the RID for extern, inline or static.  COMPLAIN is
   nonzero if this is called from the parser, zero if called recursively,
   since the standard is unclear (as detailed below).  */

void
do_type_instantiation (tree t, tree storage, tsubst_flags_t complain)
{
  if (!(CLASS_TYPE_P (t) && CLASSTYPE_TEMPLATE_INFO (t)))
    {
      if (tree ti = TYPE_TEMPLATE_INFO (t))
	error ("explicit instantiation of non-class template %qD",
	       TI_TEMPLATE (ti));
      else
	error ("explicit instantiation of non-template type %qT", t);
      return;
    }

  complete_type (t);

  if (!COMPLETE_TYPE_P (t))
    {
      if (complain & tf_error)
	error ("explicit instantiation of %q#T before definition of template",
	       t);
      return;
    }

  /* At most one of these will be true.  */
  bool extern_p = false;
  bool nomem_p = false;
  bool static_p = false;

  if (storage != NULL_TREE)
    {
      if (storage == ridpointers[(int) RID_EXTERN])
	{
	  if (cxx_dialect == cxx98)
	    pedwarn (input_location, OPT_Wpedantic,
		     "ISO C++ 1998 forbids the use of %<extern%> on "
		     "explicit instantiations");
	}
      else
	pedwarn (input_location, OPT_Wpedantic,
		 "ISO C++ forbids the use of %qE"
		 " on explicit instantiations", storage);

      if (storage == ridpointers[(int) RID_INLINE])
	nomem_p = true;
      else if (storage == ridpointers[(int) RID_EXTERN])
	extern_p = true;
      else if (storage == ridpointers[(int) RID_STATIC])
	static_p = true;
      else
	error ("storage class %qD applied to template instantiation",
	       storage);
    }

  if (CLASSTYPE_TEMPLATE_SPECIALIZATION (t))
    /* DR 259 [temp.spec].

       Both an explicit instantiation and a declaration of an explicit
       specialization shall not appear in a program unless the
       explicit instantiation follows a declaration of the explicit
       specialization.

       For a given set of template parameters, if an explicit
       instantiation of a template appears after a declaration of an
       explicit specialization for that template, the explicit
       instantiation has no effect.  */
    return;

  if (CLASSTYPE_EXPLICIT_INSTANTIATION (t) && !CLASSTYPE_INTERFACE_ONLY (t))
    {
      /* We've already instantiated the template.  */

      /* [temp.spec]

	 No program shall explicitly instantiate any template more
	 than once.

	 If EXTERN_P then this is ok.  */
      if (!extern_p && (complain & tf_error))
	permerror (input_location,
		   "duplicate explicit instantiation of %q#T", t);

      return;
    }

  check_explicit_instantiation_namespace (TYPE_NAME (t));
  mark_class_instantiated (t, extern_p);

  if (nomem_p)
    return;

  /* In contrast to implicit instantiation, where only the
     declarations, and not the definitions, of members are
     instantiated, we have here:

	 [temp.explicit]

	 An explicit instantiation that names a class template
	 specialization is also an explicit instantiation of the same
	 kind (declaration or definition) of each of its members (not
	 including members inherited from base classes and members
	 that are templates) that has not been previously explicitly
	 specialized in the translation unit containing the explicit
	 instantiation, provided that the associated constraints, if
	 any, of that member are satisfied by the template arguments
	 of the explicit instantiation.  */
  for (tree fld = TYPE_FIELDS (t); fld; fld = DECL_CHAIN (fld))
    if ((VAR_P (fld)
	 || (TREE_CODE (fld) == FUNCTION_DECL
	     && !static_p
	     && user_provided_p (fld)))
	&& DECL_TEMPLATE_INSTANTIATION (fld)
	&& constraints_satisfied_p (fld))
      {
	mark_decl_instantiated (fld, extern_p);
	if (! extern_p)
	  instantiate_decl (fld, /*defer_ok=*/true,
			    /*expl_inst_class_mem_p=*/true);
      }
    else if (DECL_IMPLICIT_TYPEDEF_P (fld))
      {
	tree type = TREE_TYPE (fld);

	if (CLASS_TYPE_P (type) && CLASSTYPE_TEMPLATE_INFO (type)
	    && !uses_template_parms (CLASSTYPE_TI_ARGS (type)))
	  do_type_instantiation (type, storage, 0);
      }
}

/* Given a function DECL, which is a specialization of TMPL, modify
   DECL to be a re-instantiation of TMPL with the same template
   arguments.  TMPL should be the template into which tsubst'ing
   should occur for DECL, not the most general template.

   One reason for doing this is a scenario like this:

     template <class T>
     void f(const T&, int i);

     void g() { f(3, 7); }

     template <class T>
     void f(const T& t, const int i) { }

   Note that when the template is first instantiated, with
   instantiate_template, the resulting DECL will have no name for the
   first parameter, and the wrong type for the second.  So, when we go
   to instantiate the DECL, we regenerate it.  */

static void
regenerate_decl_from_template (tree decl, tree tmpl, tree args)
{
  /* The arguments used to instantiate DECL, from the most general
     template.  */
  tree code_pattern = DECL_TEMPLATE_RESULT (tmpl);

  /* Make sure that we can see identifiers, and compute access correctly.  */
  push_access_scope (decl);

  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      tree specs;
      int args_depth;
      int parms_depth;

      /* Don't bother with this for unique friends that can't be redeclared and
	 might change type if regenerated (PR69836).  */
      if (DECL_UNIQUE_FRIEND_P (decl))
	goto done;

      /* Use the source location of the definition.  */
      DECL_SOURCE_LOCATION (decl) = DECL_SOURCE_LOCATION (tmpl);

      args_depth = TMPL_ARGS_DEPTH (args);
      parms_depth = TMPL_PARMS_DEPTH (DECL_TEMPLATE_PARMS (tmpl));
      if (args_depth > parms_depth)
	args = get_innermost_template_args (args, parms_depth);

      /* Instantiate a dynamic exception-specification.  noexcept will be
	 handled below.  */
      if (tree raises = TYPE_RAISES_EXCEPTIONS (TREE_TYPE (code_pattern)))
	if (TREE_VALUE (raises))
	  {
	    specs = tsubst_exception_specification (TREE_TYPE (code_pattern),
						    args, tf_error, NULL_TREE,
						    /*defer_ok*/false);
	    if (specs && specs != error_mark_node)
	      TREE_TYPE (decl) = build_exception_variant (TREE_TYPE (decl),
							  specs);
	  }

      /* Merge parameter declarations.  */
      if (tree pattern_parm
	  = skip_artificial_parms_for (code_pattern,
				       DECL_ARGUMENTS (code_pattern)))
	{
	  tree *p = &DECL_ARGUMENTS (decl);
	  for (int skip = num_artificial_parms_for (decl); skip; --skip)
	    p = &DECL_CHAIN (*p);
	  *p = tsubst_decl (pattern_parm, args, tf_error);
	  for (tree t = *p; t; t = DECL_CHAIN (t))
	    DECL_CONTEXT (t) = decl;
	}

      if (DECL_CONTRACTS (decl))
	{
	  /* If we're regenerating a specialization, the contracts will have
	     been copied from the most general template. Replace those with
	     the ones from the actual specialization.  */
	  tree tmpl = DECL_TI_TEMPLATE (decl);
	  if (DECL_TEMPLATE_SPECIALIZATION (tmpl))
	    {
	      remove_contract_attributes (decl);
	      copy_contract_attributes (decl, code_pattern);
	    }

	  tsubst_contract_attributes (decl, args, tf_warning_or_error, code_pattern);
	}

      /* Merge additional specifiers from the CODE_PATTERN.  */
      if (DECL_DECLARED_INLINE_P (code_pattern)
	  && !DECL_DECLARED_INLINE_P (decl))
	DECL_DECLARED_INLINE_P (decl) = 1;

      maybe_instantiate_noexcept (decl, tf_error);
    }
  else if (VAR_P (decl))
    {
      start_lambda_scope (decl);
      DECL_INITIAL (decl) =
	tsubst_init (DECL_INITIAL (code_pattern), decl, args,
		     tf_error, DECL_TI_TEMPLATE (decl));
      finish_lambda_scope ();
      if (VAR_HAD_UNKNOWN_BOUND (decl))
	TREE_TYPE (decl) = tsubst (TREE_TYPE (code_pattern), args,
				   tf_error, DECL_TI_TEMPLATE (decl));
    }
  else
    gcc_unreachable ();

 done:
  pop_access_scope (decl);
}

/* Return the TEMPLATE_DECL into which DECL_TI_ARGS(DECL) should be
   substituted to get DECL.  */

tree
template_for_substitution (tree decl)
{
  tree tmpl = DECL_TI_TEMPLATE (decl);

  /* Set TMPL to the template whose DECL_TEMPLATE_RESULT is the pattern
     for the instantiation.  This is not always the most general
     template.  Consider, for example:

	template <class T>
	struct S { template <class U> void f();
		   template <> void f<int>(); };

     and an instantiation of S<double>::f<int>.  We want TD to be the
     specialization S<T>::f<int>, not the more general S<T>::f<U>.  */
  while (/* An instantiation cannot have a definition, so we need a
	    more general template.  */
	 DECL_TEMPLATE_INSTANTIATION (tmpl)
	   /* We must also deal with friend templates.  Given:

		template <class T> struct S {
		  template <class U> friend void f() {};
		};

	      S<int>::f<U> say, is not an instantiation of S<T>::f<U>,
	      so far as the language is concerned, but that's still
	      where we get the pattern for the instantiation from.  On
	      other hand, if the definition comes outside the class, say:

		template <class T> struct S {
		  template <class U> friend void f();
		};
		template <class U> friend void f() {}

	      we don't need to look any further.  That's what the check for
	      DECL_INITIAL is for.  */
	  || (TREE_CODE (decl) == FUNCTION_DECL
	      && DECL_FRIEND_PSEUDO_TEMPLATE_INSTANTIATION (tmpl)
	      && !DECL_INITIAL (DECL_TEMPLATE_RESULT (tmpl))))
    {
      /* The present template, TD, should not be a definition.  If it
	 were a definition, we should be using it!  Note that we
	 cannot restructure the loop to just keep going until we find
	 a template with a definition, since that might go too far if
	 a specialization was declared, but not defined.  */

      /* Fetch the more general template.  */
      tmpl = DECL_TI_TEMPLATE (tmpl);
    }

  return tmpl;
}

/* Returns true if we need to instantiate this template instance even if we
   know we aren't going to emit it.  */

bool
always_instantiate_p (tree decl)
{
  /* We always instantiate inline functions so that we can inline them.  An
     explicit instantiation declaration prohibits implicit instantiation of
     non-inline functions.  With high levels of optimization, we would
     normally inline non-inline functions -- but we're not allowed to do
     that for "extern template" functions.  Therefore, we check
     DECL_DECLARED_INLINE_P, rather than possibly_inlined_p.  */
  return ((TREE_CODE (decl) == FUNCTION_DECL
	   && (DECL_DECLARED_INLINE_P (decl)
	       || type_uses_auto (TREE_TYPE (TREE_TYPE (decl)))))
	  /* And we need to instantiate static data members so that
	     their initializers are available in integral constant
	     expressions.  */
	  || (VAR_P (decl)
	      && decl_maybe_constant_var_p (decl)));
}

/* If FN has a noexcept-specifier that hasn't been instantiated yet,
   instantiate it now, modifying TREE_TYPE (fn).  Returns false on
   error, true otherwise.  */

bool
maybe_instantiate_noexcept (tree fn, tsubst_flags_t complain)
{
  if (fn == error_mark_node)
    return false;

  /* Don't instantiate a noexcept-specification from template context.  */
  if (processing_template_decl
      && (!flag_noexcept_type || type_dependent_expression_p (fn)))
    return true;

  tree fntype = TREE_TYPE (fn);
  tree spec = TYPE_RAISES_EXCEPTIONS (fntype);

  if ((!spec || UNEVALUATED_NOEXCEPT_SPEC_P (spec))
      && DECL_MAYBE_DELETED (fn))
    {
      if (fn == current_function_decl)
	/* We're in start_preparsed_function, keep going.  */
	return true;

      ++function_depth;
      maybe_synthesize_method (fn);
      --function_depth;
      return !DECL_DELETED_FN (fn);
    }

  if (!spec || !TREE_PURPOSE (spec))
    return true;

  tree noex = TREE_PURPOSE (spec);
  if (TREE_CODE (noex) != DEFERRED_NOEXCEPT
      && TREE_CODE (noex) != DEFERRED_PARSE)
    return true;

  tree orig_fn = NULL_TREE;
  /* For a member friend template we can get a TEMPLATE_DECL.  Let's use
     its FUNCTION_DECL for the rest of this function -- push_access_scope
     doesn't accept TEMPLATE_DECLs.  */
  if (DECL_FUNCTION_TEMPLATE_P (fn))
    {
      orig_fn = fn;
      fn = DECL_TEMPLATE_RESULT (fn);
    }

  if (DECL_CLONED_FUNCTION_P (fn))
    {
      tree prime = DECL_CLONED_FUNCTION (fn);
      if (!maybe_instantiate_noexcept (prime, complain))
	return false;
      spec = TYPE_RAISES_EXCEPTIONS (TREE_TYPE (prime));
    }
  else if (TREE_CODE (noex) == DEFERRED_NOEXCEPT)
    {
      static hash_set<tree>* fns = new hash_set<tree>;
      bool added = false;
      if (DEFERRED_NOEXCEPT_PATTERN (noex) == NULL_TREE)
	{
	  spec = get_defaulted_eh_spec (fn, complain);
	  if (spec == error_mark_node)
	    /* This might have failed because of an unparsed DMI, so
	       let's try again later.  */
	    return false;
	}
      else if (!(added = !fns->add (fn)))
	{
	  /* If hash_set::add returns true, the element was already there.  */
	  location_t loc = cp_expr_loc_or_loc (DEFERRED_NOEXCEPT_PATTERN (noex),
					    DECL_SOURCE_LOCATION (fn));
	  error_at (loc,
		    "exception specification of %qD depends on itself",
		    fn);
	  spec = noexcept_false_spec;
	}
      else if (push_tinst_level (fn))
	{
	  push_to_top_level ();
	  push_access_scope (fn);
	  push_deferring_access_checks (dk_no_deferred);
	  input_location = DECL_SOURCE_LOCATION (fn);

	  if (DECL_NONSTATIC_MEMBER_FUNCTION_P (fn)
	      && !DECL_LOCAL_DECL_P (fn))
	    {
	      /* If needed, set current_class_ptr for the benefit of
		 tsubst_copy/PARM_DECL.  */
	      tree this_parm = DECL_ARGUMENTS (fn);
	      current_class_ptr = NULL_TREE;
	      current_class_ref = cp_build_fold_indirect_ref (this_parm);
	      current_class_ptr = this_parm;
	    }

	  /* If this function is represented by a TEMPLATE_DECL, then
	     the deferred noexcept-specification might still contain
	     dependent types, even after substitution.  And we need the
	     dependency check functions to work in build_noexcept_spec.  */
	  if (orig_fn)
	    ++processing_template_decl;

	  /* Do deferred instantiation of the noexcept-specifier.  */
	  noex = tsubst_copy_and_build (DEFERRED_NOEXCEPT_PATTERN (noex),
					DEFERRED_NOEXCEPT_ARGS (noex),
					tf_warning_or_error, fn);

	  /* Build up the noexcept-specification.  */
	  spec = build_noexcept_spec (noex, tf_warning_or_error);

	  if (orig_fn)
	    --processing_template_decl;

	  pop_deferring_access_checks ();
	  pop_access_scope (fn);
	  pop_tinst_level ();
	  pop_from_top_level ();
	}
      else
	spec = noexcept_false_spec;

      if (added)
	fns->remove (fn);
    }

  if (spec == error_mark_node)
    {
      /* This failed with a hard error, so let's go with false.  */
      gcc_assert (seen_error ());
      spec = noexcept_false_spec;
    }

  TREE_TYPE (fn) = build_exception_variant (fntype, spec);
  if (orig_fn)
    TREE_TYPE (orig_fn) = TREE_TYPE (fn);

  return true;
}

/* We're starting to process the function INST, an instantiation of PATTERN;
   add their parameters to local_specializations.  */

void
register_parameter_specializations (tree pattern, tree inst)
{
  tree tmpl_parm = DECL_ARGUMENTS (pattern);
  tree spec_parm = DECL_ARGUMENTS (inst);
  if (DECL_NONSTATIC_MEMBER_FUNCTION_P (inst))
    {
      register_local_specialization (spec_parm, tmpl_parm);
      spec_parm = skip_artificial_parms_for (inst, spec_parm);
      tmpl_parm = skip_artificial_parms_for (pattern, tmpl_parm);
    }
  for (; tmpl_parm; tmpl_parm = DECL_CHAIN (tmpl_parm))
    {
      if (!DECL_PACK_P (tmpl_parm))
	{
	  register_local_specialization (spec_parm, tmpl_parm);
	  spec_parm = DECL_CHAIN (spec_parm);
	}
      else
	{
	  /* Register the (value) argument pack as a specialization of
	     TMPL_PARM, then move on.  */
	  tree argpack = extract_fnparm_pack (tmpl_parm, &spec_parm);
	  register_local_specialization (argpack, tmpl_parm);
	}
    }
  gcc_assert (!spec_parm);
}

/* Instantiate the body of D using PATTERN with ARGS.  We have
   already determined PATTERN is the correct template to use.
   NESTED_P is true if this is a nested function, in which case
   PATTERN will be a FUNCTION_DECL not a TEMPLATE_DECL.  */

static void
instantiate_body (tree pattern, tree args, tree d, bool nested_p)
{
  tree td = NULL_TREE;
  tree code_pattern = pattern;

  if (!nested_p)
    {
      td = pattern;
      code_pattern = DECL_TEMPLATE_RESULT (td);
    }
  else
    /* Only OMP reductions are nested.  */
    gcc_checking_assert (DECL_OMP_DECLARE_REDUCTION_P (code_pattern));

  vec<tree> omp_privatization_save;
  if (current_function_decl)
    save_omp_privatization_clauses (omp_privatization_save);

  bool push_to_top = maybe_push_to_top_level (d);

  mark_template_arguments_used (pattern, args);

  if (VAR_P (d))
    {
      /* The variable might be a lambda's extra scope, and that
	 lambda's visibility depends on D's.  */
      maybe_commonize_var (d);
      determine_visibility (d);
    }

  /* Mark D as instantiated so that recursive calls to
     instantiate_decl do not try to instantiate it again.  */
  DECL_TEMPLATE_INSTANTIATED (d) = 1;

  if (td)
    /* Regenerate the declaration in case the template has been modified
       by a subsequent redeclaration.  */
    regenerate_decl_from_template (d, td, args);

  /* We already set the file and line above.  Reset them now in case
     they changed as a result of calling regenerate_decl_from_template.  */
  input_location = DECL_SOURCE_LOCATION (d);

  if (VAR_P (d))
    {
      /* Clear out DECL_RTL; whatever was there before may not be right
	 since we've reset the type of the declaration.  */
      SET_DECL_RTL (d, NULL);
      DECL_IN_AGGR_P (d) = 0;

      /* The initializer is placed in DECL_INITIAL by
	 regenerate_decl_from_template so we don't need to
	 push/pop_access_scope again here.  Pull it out so that
	 cp_finish_decl can process it.  */
      bool const_init = false;
      tree init = DECL_INITIAL (d);
      DECL_INITIAL (d) = NULL_TREE;
      DECL_INITIALIZED_P (d) = 0;

      /* Clear DECL_EXTERNAL so that cp_finish_decl will process the
	 initializer.  That function will defer actual emission until
	 we have a chance to determine linkage.  */
      DECL_EXTERNAL (d) = 0;

      /* Enter the scope of D so that access-checking works correctly.  */
      bool enter_context = DECL_CLASS_SCOPE_P (d);
      if (enter_context)
        push_nested_class (DECL_CONTEXT (d));

      const_init = DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (code_pattern);
      cp_finish_decl (d, init, const_init, NULL_TREE, 0);

      if (enter_context)
        pop_nested_class ();
    }
  else if (TREE_CODE (d) == FUNCTION_DECL && DECL_DEFAULTED_FN (code_pattern))
    synthesize_method (d);
  else if (TREE_CODE (d) == FUNCTION_DECL)
    {
      /* Set up the list of local specializations.  */
      local_specialization_stack lss (push_to_top ? lss_blank : lss_copy);
      tree block = NULL_TREE;

      /* Set up context.  */
      if (nested_p)
	block = push_stmt_list ();
      else
	{
	  start_preparsed_function (d, NULL_TREE, SF_PRE_PARSED);

	  perform_instantiation_time_access_checks (code_pattern, args);
	}

      /* Create substitution entries for the parameters.  */
      register_parameter_specializations (code_pattern, d);

      /* Substitute into the body of the function.  */
      if (DECL_OMP_DECLARE_REDUCTION_P (code_pattern))
	tsubst_omp_udr (DECL_SAVED_TREE (code_pattern), args,
			tf_warning_or_error, d);
      else
	{
	  tsubst_expr (DECL_SAVED_TREE (code_pattern), args,
		       tf_warning_or_error, DECL_TI_TEMPLATE (d));

	  /* Set the current input_location to the end of the function
	     so that finish_function knows where we are.  */
	  input_location
	    = DECL_STRUCT_FUNCTION (code_pattern)->function_end_locus;

	  /* Remember if we saw an infinite loop in the template.  */
	  current_function_infinite_loop
	    = DECL_STRUCT_FUNCTION (code_pattern)->language->infinite_loop;
	}

      /* Finish the function.  */
      if (nested_p)
	DECL_SAVED_TREE (d) = pop_stmt_list (block);
      else
	{
	  d = finish_function (/*inline_p=*/false);
	  expand_or_defer_fn (d);
	}

      if (DECL_OMP_DECLARE_REDUCTION_P (code_pattern))
	cp_check_omp_declare_reduction (d);
    }

  /* We're not deferring instantiation any more.  */
  if (!nested_p)
    TI_PENDING_TEMPLATE_FLAG (DECL_TEMPLATE_INFO (d)) = 0;

  maybe_pop_from_top_level (push_to_top);

  if (current_function_decl)
    restore_omp_privatization_clauses (omp_privatization_save);
}

/* Produce the definition of D, a _DECL generated from a template.  If
   DEFER_OK is true, then we don't have to actually do the
   instantiation now; we just have to do it sometime.  Normally it is
   an error if this is an explicit instantiation but D is undefined.
   EXPL_INST_CLASS_MEM_P is true iff D is a member of an explicitly
   instantiated class template.  */

tree
instantiate_decl (tree d, bool defer_ok, bool expl_inst_class_mem_p)
{
  tree tmpl = DECL_TI_TEMPLATE (d);
  tree gen_args;
  tree args;
  tree td;
  tree code_pattern;
  tree spec;
  tree gen_tmpl;
  bool pattern_defined;
  location_t saved_loc = input_location;
  int saved_unevaluated_operand = cp_unevaluated_operand;
  int saved_inhibit_evaluation_warnings = c_inhibit_evaluation_warnings;
  bool external_p;
  bool deleted_p;

  /* This function should only be used to instantiate templates for
     functions and static member variables.  */
  gcc_assert (VAR_OR_FUNCTION_DECL_P (d));

  /* A concept is never instantiated. */
  gcc_assert (!DECL_DECLARED_CONCEPT_P (d));

  gcc_checking_assert (!DECL_FUNCTION_SCOPE_P (d));

  if (modules_p ())
    /* We may have a pending instantiation of D itself.  */
    lazy_load_pendings (d);

  /* Variables are never deferred; if instantiation is required, they
     are instantiated right away.  That allows for better code in the
     case that an expression refers to the value of the variable --
     if the variable has a constant value the referring expression can
     take advantage of that fact.  */
  if (VAR_P (d))
    defer_ok = false;

  /* Don't instantiate cloned functions.  Instead, instantiate the
     functions they cloned.  */
  if (TREE_CODE (d) == FUNCTION_DECL && DECL_CLONED_FUNCTION_P (d))
    d = DECL_CLONED_FUNCTION (d);

  if (DECL_TEMPLATE_INSTANTIATED (d)
      || TREE_TYPE (d) == error_mark_node
      || (TREE_CODE (d) == FUNCTION_DECL
	  && DECL_DEFAULTED_FN (d) && DECL_INITIAL (d))
      || DECL_TEMPLATE_SPECIALIZATION (d))
    /* D has already been instantiated or explicitly specialized, so
       there's nothing for us to do here.

       It might seem reasonable to check whether or not D is an explicit
       instantiation, and, if so, stop here.  But when an explicit
       instantiation is deferred until the end of the compilation,
       DECL_EXPLICIT_INSTANTIATION is set, even though we still need to do
       the instantiation.  */
    return d;

  /* Check to see whether we know that this template will be
     instantiated in some other file, as with "extern template"
     extension.  */
  external_p = (DECL_INTERFACE_KNOWN (d) && DECL_REALLY_EXTERN (d));

  /* In general, we do not instantiate such templates.  */
  if (external_p && !always_instantiate_p (d))
    return d;

  gen_tmpl = most_general_template (tmpl);
  gen_args = DECL_TI_ARGS (d);

  /* We should already have the extra args.  */
  gcc_checking_assert (tmpl == gen_tmpl
		       || (TMPL_PARMS_DEPTH (DECL_TEMPLATE_PARMS (gen_tmpl))
			   == TMPL_ARGS_DEPTH (gen_args)));
  /* And what's in the hash table should match D.  */
  gcc_checking_assert ((spec = retrieve_specialization (gen_tmpl, gen_args, 0))
		       == d
		       || spec == NULL_TREE);

  /* This needs to happen before any tsubsting.  */
  if (! push_tinst_level (d))
    return d;

  auto_timevar tv (TV_TEMPLATE_INST);

  /* Set TD to the template whose DECL_TEMPLATE_RESULT is the pattern
     for the instantiation.  */
  td = template_for_substitution (d);
  args = gen_args;

  if (variable_template_specialization_p (d))
    {
      /* Look up an explicit specialization, if any.  */
      tree partial_ti = most_specialized_partial_spec (d, tf_warning_or_error);
      if (partial_ti && partial_ti != error_mark_node)
	{
	  td = TI_TEMPLATE (partial_ti);
	  args = TI_ARGS (partial_ti);
	}
    }

  code_pattern = DECL_TEMPLATE_RESULT (td);

  /* We should never be trying to instantiate a member of a class
     template or partial specialization.  */
  gcc_assert (d != code_pattern);

  if ((DECL_NAMESPACE_SCOPE_P (d) && !DECL_INITIALIZED_IN_CLASS_P (d))
      || DECL_TEMPLATE_SPECIALIZATION (td))
    /* In the case of a friend template whose definition is provided
       outside the class, we may have too many arguments.  Drop the
       ones we don't need.  The same is true for specializations.  */
    args = get_innermost_template_args
      (args, TMPL_PARMS_DEPTH (DECL_TEMPLATE_PARMS (td)));

  if (TREE_CODE (d) == FUNCTION_DECL)
    {
      deleted_p = DECL_DELETED_FN (code_pattern);
      pattern_defined = ((DECL_SAVED_TREE (code_pattern) != NULL_TREE
			  && DECL_INITIAL (code_pattern) != error_mark_node)
			 || DECL_DEFAULTED_FN (code_pattern)
			 || deleted_p);
    }
  else
    {
      deleted_p = false;
      if (DECL_CLASS_SCOPE_P (code_pattern))
	pattern_defined = ! DECL_IN_AGGR_P (code_pattern);
      else
	pattern_defined = ! DECL_EXTERNAL (code_pattern);
    }

  /* We may be in the middle of deferred access check.  Disable it now.  */
  push_deferring_access_checks (dk_no_deferred);

  /* Unless an explicit instantiation directive has already determined
     the linkage of D, remember that a definition is available for
     this entity.  */
  if (pattern_defined
      && !DECL_INTERFACE_KNOWN (d)
      && !DECL_NOT_REALLY_EXTERN (d))
    mark_definable (d);

  DECL_SOURCE_LOCATION (td) = DECL_SOURCE_LOCATION (code_pattern);
  DECL_SOURCE_LOCATION (d) = DECL_SOURCE_LOCATION (code_pattern);
  input_location = DECL_SOURCE_LOCATION (d);

  /* If D is a member of an explicitly instantiated class template,
     and no definition is available, treat it like an implicit
     instantiation.  */
  if (!pattern_defined && expl_inst_class_mem_p
      && DECL_EXPLICIT_INSTANTIATION (d))
    {
      /* Leave linkage flags alone on instantiations with anonymous
	 visibility.  */
      if (TREE_PUBLIC (d))
	{
	  DECL_NOT_REALLY_EXTERN (d) = 0;
	  DECL_INTERFACE_KNOWN (d) = 0;
	}
      SET_DECL_IMPLICIT_INSTANTIATION (d);
    }

  /* Defer all other templates, unless we have been explicitly
     forbidden from doing so.  */
  if (/* If there is no definition, we cannot instantiate the
	 template.  */
      ! pattern_defined
      /* If it's OK to postpone instantiation, do so.  */
      || defer_ok
      /* If this is a static data member that will be defined
	 elsewhere, we don't want to instantiate the entire data
	 member, but we do want to instantiate the initializer so that
	 we can substitute that elsewhere.  */
      || (external_p && VAR_P (d))
      /* Handle here a deleted function too, avoid generating
	 its body (c++/61080).  */
      || deleted_p)
    {
      /* The definition of the static data member is now required so
	 we must substitute the initializer.  */
      if (VAR_P (d)
	  && !DECL_INITIAL (d)
	  && DECL_INITIAL (code_pattern))
	{
	  tree ns;
	  tree init;
	  bool const_init = false;
	  bool enter_context = DECL_CLASS_SCOPE_P (d);

	  ns = decl_namespace_context (d);
	  push_nested_namespace (ns);
	  if (enter_context)
	    push_nested_class (DECL_CONTEXT (d));
	  init = tsubst_expr (DECL_INITIAL (code_pattern),
			      args,
			      tf_warning_or_error, NULL_TREE);
	  /* If instantiating the initializer involved instantiating this
	     again, don't call cp_finish_decl twice.  */
	  if (!DECL_INITIAL (d))
	    {
	      /* Make sure the initializer is still constant, in case of
		 circular dependency (template/instantiate6.C). */
	      const_init
		= DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (code_pattern);
	      cp_finish_decl (d, init, /*init_const_expr_p=*/const_init,
			      /*asmspec_tree=*/NULL_TREE, 0);
	    }
	  if (enter_context)
	    pop_nested_class ();
	  pop_nested_namespace (ns);
	}

      /* We restore the source position here because it's used by
	 add_pending_template.  */
      input_location = saved_loc;

      if (at_eof && !pattern_defined
	  && DECL_EXPLICIT_INSTANTIATION (d)
	  && DECL_NOT_REALLY_EXTERN (d))
	/* [temp.explicit]

	   The definition of a non-exported function template, a
	   non-exported member function template, or a non-exported
	   member function or static data member of a class template
	   shall be present in every translation unit in which it is
	   explicitly instantiated.  */
	permerror (input_location,  "explicit instantiation of %qD "
		   "but no definition available", d);

      /* If we're in unevaluated context, we just wanted to get the
	 constant value; this isn't an odr use, so don't queue
	 a full instantiation.  */
      if (!cp_unevaluated_operand
	  /* ??? Historically, we have instantiated inline functions, even
	     when marked as "extern template".  */
	  && !(external_p && VAR_P (d)))
	add_pending_template (d);
    }
  else
    {
      set_instantiating_module (d);
      if (variable_template_p (gen_tmpl))
	note_variable_template_instantiation (d);
      instantiate_body (td, args, d, false);
    }

  pop_deferring_access_checks ();
  pop_tinst_level ();
  input_location = saved_loc;
  cp_unevaluated_operand = saved_unevaluated_operand;
  c_inhibit_evaluation_warnings = saved_inhibit_evaluation_warnings;

  return d;
}

/* Run through the list of templates that we wish we could
   instantiate, and instantiate any we can.  RETRIES is the
   number of times we retry pending template instantiation.  */

void
instantiate_pending_templates (int retries)
{
  int reconsider;
  location_t saved_loc = input_location;

  /* Instantiating templates may trigger vtable generation.  This in turn
     may require further template instantiations.  We place a limit here
     to avoid infinite loop.  */
  if (pending_templates && retries >= max_tinst_depth)
    {
      tree decl = pending_templates->tinst->maybe_get_node ();

      fatal_error (input_location,
		   "template instantiation depth exceeds maximum of %d"
		   " instantiating %q+D, possibly from virtual table generation"
		   " (use %<-ftemplate-depth=%> to increase the maximum)",
		   max_tinst_depth, decl);
      if (TREE_CODE (decl) == FUNCTION_DECL)
	/* Pretend that we defined it.  */
	DECL_INITIAL (decl) = error_mark_node;
      return;
    }

  do
    {
      struct pending_template **t = &pending_templates;
      struct pending_template *last = NULL;
      reconsider = 0;
      while (*t)
	{
	  tree instantiation = reopen_tinst_level ((*t)->tinst);
	  bool complete = false;

	  if (TYPE_P (instantiation))
	    {
	      if (!COMPLETE_TYPE_P (instantiation))
		{
		  instantiate_class_template (instantiation);
		  if (CLASSTYPE_TEMPLATE_INSTANTIATION (instantiation))
		    for (tree fld = TYPE_FIELDS (instantiation);
			 fld; fld = TREE_CHAIN (fld))
		      if ((VAR_P (fld)
			   || (TREE_CODE (fld) == FUNCTION_DECL
			       && !DECL_ARTIFICIAL (fld)))
			  && DECL_TEMPLATE_INSTANTIATION (fld))
			instantiate_decl (fld,
					  /*defer_ok=*/false,
					  /*expl_inst_class_mem_p=*/false);

		  if (COMPLETE_TYPE_P (instantiation))
		    reconsider = 1;
		}

	      complete = COMPLETE_TYPE_P (instantiation);
	    }
	  else
	    {
	      if (!DECL_TEMPLATE_SPECIALIZATION (instantiation)
		  && !DECL_TEMPLATE_INSTANTIATED (instantiation))
		{
		  instantiation
		    = instantiate_decl (instantiation,
					/*defer_ok=*/false,
					/*expl_inst_class_mem_p=*/false);
		  if (DECL_TEMPLATE_INSTANTIATED (instantiation))
		    reconsider = 1;
		}

	      complete = (DECL_TEMPLATE_SPECIALIZATION (instantiation)
			  || DECL_TEMPLATE_INSTANTIATED (instantiation));
	    }

	  if (complete)
	    {
	      /* If INSTANTIATION has been instantiated, then we don't
		 need to consider it again in the future.  */
	      struct pending_template *drop = *t;
	      *t = (*t)->next;
	      set_refcount_ptr (drop->tinst);
	      pending_template_freelist ().free (drop);
	    }
	  else
	    {
	      last = *t;
	      t = &(*t)->next;
	    }
	  tinst_depth = 0;
	  set_refcount_ptr (current_tinst_level);
	}
      last_pending_template = last;
    }
  while (reconsider);

  input_location = saved_loc;
}

/* Substitute ARGVEC into T, which is a list of initializers for
   either base class or a non-static data member.  The TREE_PURPOSEs
   are DECLs, and the TREE_VALUEs are the initializer values.  Used by
   instantiate_decl.  */

static tree
tsubst_initializer_list (tree t, tree argvec)
{
  tree inits = NULL_TREE;
  tree target_ctor = error_mark_node;

  for (; t; t = TREE_CHAIN (t))
    {
      tree decl;
      tree init;
      tree expanded_bases = NULL_TREE;
      tree expanded_arguments = NULL_TREE;
      int i, len = 1;

      if (TREE_CODE (TREE_PURPOSE (t)) == TYPE_PACK_EXPANSION)
        {
          tree expr;
          tree arg;

          /* Expand the base class expansion type into separate base
             classes.  */
          expanded_bases = tsubst_pack_expansion (TREE_PURPOSE (t), argvec,
                                                 tf_warning_or_error,
                                                 NULL_TREE);
          if (expanded_bases == error_mark_node)
            continue;

          /* We'll be building separate TREE_LISTs of arguments for
             each base.  */
          len = TREE_VEC_LENGTH (expanded_bases);
          expanded_arguments = make_tree_vec (len);
          for (i = 0; i < len; i++)
            TREE_VEC_ELT (expanded_arguments, i) = NULL_TREE;

          /* Build a dummy EXPR_PACK_EXPANSION that will be used to
             expand each argument in the TREE_VALUE of t.  */
          expr = make_node (EXPR_PACK_EXPANSION);
	  PACK_EXPANSION_LOCAL_P (expr) = true;
          PACK_EXPANSION_PARAMETER_PACKS (expr) =
            PACK_EXPANSION_PARAMETER_PACKS (TREE_PURPOSE (t));

	  if (TREE_VALUE (t) == void_type_node)
	    /* VOID_TYPE_NODE is used to indicate
	       value-initialization.  */
	    {
	      for (i = 0; i < len; i++)
		TREE_VEC_ELT (expanded_arguments, i) = void_type_node;
	    }
	  else
	    {
	      /* Substitute parameter packs into each argument in the
		 TREE_LIST.  */
	      in_base_initializer = 1;
	      for (arg = TREE_VALUE (t); arg; arg = TREE_CHAIN (arg))
		{
		  tree expanded_exprs;

		  /* Expand the argument.  */
		  tree value;
		  if (TREE_CODE (TREE_VALUE (arg)) == EXPR_PACK_EXPANSION)
		    value = TREE_VALUE (arg);
		  else
		    {
		      value = expr;
		      PACK_EXPANSION_PATTERN (value) = TREE_VALUE (arg);
		    }
		  expanded_exprs
		    = tsubst_pack_expansion (value, argvec,
					     tf_warning_or_error,
					     NULL_TREE);
		  if (expanded_exprs == error_mark_node)
		    continue;

		  /* Prepend each of the expanded expressions to the
		     corresponding TREE_LIST in EXPANDED_ARGUMENTS.  */
		  for (i = 0; i < len; i++)
		    if (TREE_CODE (TREE_VALUE (arg)) == EXPR_PACK_EXPANSION)
		      for (int j = 0; j < TREE_VEC_LENGTH (expanded_exprs); j++)
			TREE_VEC_ELT (expanded_arguments, i)
			  = tree_cons (NULL_TREE,
				       TREE_VEC_ELT (expanded_exprs, j),
				       TREE_VEC_ELT (expanded_arguments, i));
		    else
		      TREE_VEC_ELT (expanded_arguments, i)
			= tree_cons (NULL_TREE,
				     TREE_VEC_ELT (expanded_exprs, i),
				     TREE_VEC_ELT (expanded_arguments, i));
		}
	      in_base_initializer = 0;

	      /* Reverse all of the TREE_LISTs in EXPANDED_ARGUMENTS,
		 since we built them backwards.  */
	      for (i = 0; i < len; i++)
		{
		  TREE_VEC_ELT (expanded_arguments, i) =
		    nreverse (TREE_VEC_ELT (expanded_arguments, i));
		}
	    }
        }

      for (i = 0; i < len; ++i)
        {
          if (expanded_bases)
            {
              decl = TREE_VEC_ELT (expanded_bases, i);
              decl = expand_member_init (decl);
              init = TREE_VEC_ELT (expanded_arguments, i);
            }
          else
            {
	      tree tmp;
              decl = tsubst_copy (TREE_PURPOSE (t), argvec, 
                                  tf_warning_or_error, NULL_TREE);

              decl = expand_member_init (decl);
              if (decl && !DECL_P (decl))
                in_base_initializer = 1;

	      init = TREE_VALUE (t);
	      tmp = init;
	      if (init != void_type_node)
		init = tsubst_expr (init, argvec,
				    tf_warning_or_error, NULL_TREE);
	      if (init == NULL_TREE && tmp != NULL_TREE)
		/* If we had an initializer but it instantiated to nothing,
		   value-initialize the object.  This will only occur when
		   the initializer was a pack expansion where the parameter
		   packs used in that expansion were of length zero.  */
		init = void_type_node;
              in_base_initializer = 0;
            }

	  if (target_ctor != error_mark_node
	      && init != error_mark_node)
	    {
	      error ("mem-initializer for %qD follows constructor delegation",
		     decl);
	      return inits;
	    }
	  /* Look for a target constructor. */
	  if (init != error_mark_node
	      && decl && CLASS_TYPE_P (decl)
	      && same_type_p (decl, current_class_type))
	    {
	      maybe_warn_cpp0x (CPP0X_DELEGATING_CTORS);
	      if (inits)
		{
		  error ("constructor delegation follows mem-initializer for %qD",
			 TREE_PURPOSE (inits));
		  continue;
		}
	      target_ctor = init;
	    }

          if (decl)
            {
              init = build_tree_list (decl, init);
	      /* Carry over the dummy TREE_TYPE node containing the source
		 location.  */
	      TREE_TYPE (init) = TREE_TYPE (t);
              TREE_CHAIN (init) = inits;
              inits = init;
            }
        }
    }
  return inits;
}

/* Instantiate an enumerated type.  TAG is the template type, NEWTAG
   is the instantiation (which should have been created with
   start_enum) and ARGS are the template arguments to use.  */

static void
tsubst_enum (tree tag, tree newtag, tree args)
{
  tree e;

  if (SCOPED_ENUM_P (newtag))
    begin_scope (sk_scoped_enum, newtag);

  for (e = TYPE_VALUES (tag); e; e = TREE_CHAIN (e))
    {
      tree value;
      tree decl = TREE_VALUE (e);

      /* Note that in a template enum, the TREE_VALUE is the
	 CONST_DECL, not the corresponding INTEGER_CST.  */
      value = tsubst_expr (DECL_INITIAL (decl),
			   args, tf_warning_or_error, NULL_TREE);

      /* Give this enumeration constant the correct access.  */
      set_current_access_from_decl (decl);

      /* Actually build the enumerator itself.  Here we're assuming that
	 enumerators can't have dependent attributes.  */
      tree newdecl = build_enumerator (DECL_NAME (decl), value, newtag,
				       DECL_ATTRIBUTES (decl),
				       DECL_SOURCE_LOCATION (decl));
      /* Attribute deprecated without an argument isn't sticky: it'll
	 melt into a tree flag, so we need to propagate the flag here,
	 since we just created a new enumerator.  */
      TREE_DEPRECATED (newdecl) = TREE_DEPRECATED (decl);
      TREE_UNAVAILABLE (newdecl) = TREE_UNAVAILABLE (decl);
    }

  if (SCOPED_ENUM_P (newtag))
    finish_scope ();

  finish_enum_value_list (newtag);
  finish_enum (newtag);

  DECL_SOURCE_LOCATION (TYPE_NAME (newtag))
    = DECL_SOURCE_LOCATION (TYPE_NAME (tag));
  TREE_DEPRECATED (newtag) = TREE_DEPRECATED (tag);
  TREE_UNAVAILABLE (newtag) = TREE_UNAVAILABLE (tag);
}

/* DECL is a FUNCTION_DECL that is a template specialization.  Return
   its type -- but without substituting the innermost set of template
   arguments.  So, innermost set of template parameters will appear in
   the type.  */

tree
get_mostly_instantiated_function_type (tree decl)
{
  /* For a function, DECL_TI_TEMPLATE is partially instantiated.  */
  return TREE_TYPE (DECL_TI_TEMPLATE (decl));
}

/* Return truthvalue if we're processing a template different from
   the last one involved in diagnostics.  */
bool
problematic_instantiation_changed (void)
{
  return current_tinst_level != last_error_tinst_level;
}

/* Remember current template involved in diagnostics.  */
void
record_last_problematic_instantiation (void)
{
  set_refcount_ptr (last_error_tinst_level, current_tinst_level);
}

struct tinst_level *
current_instantiation (void)
{
  return current_tinst_level;
}

/* Return TRUE if current_function_decl is being instantiated, false
   otherwise.  */

bool
instantiating_current_function_p (void)
{
  return (current_instantiation ()
	  && (current_instantiation ()->maybe_get_node ()
	      == current_function_decl));
}

/* [temp.param] Check that template non-type parm TYPE is of an allowable
   type.  Return false for ok, true for disallowed.  Issue error and
   inform messages under control of COMPLAIN.  */

static bool
invalid_nontype_parm_type_p (tree type, tsubst_flags_t complain)
{
  if (INTEGRAL_OR_ENUMERATION_TYPE_P (type))
    return false;
  else if (TYPE_PTR_P (type))
    return false;
  else if (TYPE_REF_P (type)
	   && !TYPE_REF_IS_RVALUE (type))
    return false;
  else if (TYPE_PTRMEM_P (type))
    return false;
  else if (TREE_CODE (type) == TEMPLATE_TYPE_PARM)
    {
      if (CLASS_PLACEHOLDER_TEMPLATE (type) && cxx_dialect < cxx20)
	{
	  if (complain & tf_error)
	    error ("non-type template parameters of deduced class type only "
		   "available with %<-std=c++20%> or %<-std=gnu++20%>");
	  return true;
	}
      return false;
    }
  else if (TREE_CODE (type) == NULLPTR_TYPE)
    return false;
  else if (TREE_CODE (type) == BOUND_TEMPLATE_TEMPLATE_PARM
	   && cxx_dialect < cxx11)
    /* Fall through; before C++11 alias templates, a bound ttp
       always instantiates into a class type.  */;
  else if (WILDCARD_TYPE_P (type))
    /* Any other wildcard type not already handled above is allowed.  */
    return false;
  else if (TREE_CODE (type) == COMPLEX_TYPE)
    /* Fall through.  */;
  else if (VOID_TYPE_P (type))
    /* Fall through.  */;
  else if (cxx_dialect >= cxx20)
    {
      if (dependent_type_p (type))
	return false;
      if (!complete_type_or_maybe_complain (type, NULL_TREE, complain))
	return true;
      if (structural_type_p (type))
	return false;
      if (complain & tf_error)
	{
	  auto_diagnostic_group d;
	  error ("%qT is not a valid type for a template non-type "
		 "parameter because it is not structural", type);
	  structural_type_p (type, true);
	}
      return true;
    }
  else if (CLASS_TYPE_P (type))
    {
      if (complain & tf_error)
	error ("non-type template parameters of class type only available "
	       "with %<-std=c++20%> or %<-std=gnu++20%>");
      return true;
    }

  if (complain & tf_error)
    {
      if (type == error_mark_node)
	inform (input_location, "invalid template non-type parameter");
      else
	error ("%q#T is not a valid type for a template non-type parameter",
	       type);
    }
  return true;
}

/* Returns true iff the noexcept-specifier for TYPE is value-dependent.  */

static bool
value_dependent_noexcept_spec_p (tree type)
{
  if (tree spec = TYPE_RAISES_EXCEPTIONS (type))
    if (tree noex = TREE_PURPOSE (spec))
      /* Treat DEFERRED_NOEXCEPT as non-dependent, since it doesn't
	 affect overload resolution and treating it as dependent breaks
	 things.  Same for an unparsed noexcept expression.  */
      if (TREE_CODE (noex) != DEFERRED_NOEXCEPT
	  && TREE_CODE (noex) != DEFERRED_PARSE
	  && value_dependent_expression_p (noex))
	return true;

  return false;
}

/* Returns TRUE if TYPE is dependent, in the sense of [temp.dep.type].
   Assumes that TYPE really is a type, and not the ERROR_MARK_NODE.*/

static bool
dependent_type_p_r (tree type)
{
  tree scope;

  /* [temp.dep.type]

     A type is dependent if it is:

     -- a template parameter. Template template parameters are types
	for us (since TYPE_P holds true for them) so we handle
	them here.  */
  if (TREE_CODE (type) == TEMPLATE_TYPE_PARM
      || TREE_CODE (type) == TEMPLATE_TEMPLATE_PARM)
    return true;
  /* -- a qualified-id with a nested-name-specifier which contains a
	class-name that names a dependent type or whose unqualified-id
	names a dependent type.  */
  if (TREE_CODE (type) == TYPENAME_TYPE)
    return true;

  /* An alias template specialization can be dependent even if the
     resulting type is not.  */
  if (dependent_alias_template_spec_p (type, nt_transparent))
    return true;

  /* -- a cv-qualified type where the cv-unqualified type is
	dependent.
     No code is necessary for this bullet; the code below handles
     cv-qualified types, and we don't want to strip aliases with
     TYPE_MAIN_VARIANT because of DR 1558.  */
  /* -- a compound type constructed from any dependent type.  */
  if (TYPE_PTRMEM_P (type))
    return (dependent_type_p (TYPE_PTRMEM_CLASS_TYPE (type))
	    || dependent_type_p (TYPE_PTRMEM_POINTED_TO_TYPE
					   (type)));
  else if (INDIRECT_TYPE_P (type))
    return dependent_type_p (TREE_TYPE (type));
  else if (FUNC_OR_METHOD_TYPE_P (type))
    {
      tree arg_type;

      if (dependent_type_p (TREE_TYPE (type)))
	return true;
      for (arg_type = TYPE_ARG_TYPES (type);
	   arg_type;
	   arg_type = TREE_CHAIN (arg_type))
	if (dependent_type_p (TREE_VALUE (arg_type)))
	  return true;
      if (cxx_dialect >= cxx17
	  && value_dependent_noexcept_spec_p (type))
	/* A value-dependent noexcept-specifier makes the type dependent.  */
	return true;
      return false;
    }
  /* -- an array type constructed from any dependent type or whose
	size is specified by a constant expression that is
	value-dependent.

        We checked for type- and value-dependence of the bounds in
        compute_array_index_type, so TYPE_DEPENDENT_P is already set.  */
  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      if (TYPE_DOMAIN (type)
	  && dependent_type_p (TYPE_DOMAIN (type)))
	return true;
      return dependent_type_p (TREE_TYPE (type));
    }

  /* -- a template-id in which either the template name is a template
     parameter ...  */
  if (TREE_CODE (type) == BOUND_TEMPLATE_TEMPLATE_PARM)
    return true;
  /* ... or any of the template arguments is a dependent type or
	an expression that is type-dependent or value-dependent.  */
  else if (CLASS_TYPE_P (type) && CLASSTYPE_TEMPLATE_INFO (type)
	   && (any_dependent_template_arguments_p
	       (INNERMOST_TEMPLATE_ARGS (CLASSTYPE_TI_ARGS (type)))))
    return true;

  /* All TYPEOF_TYPEs, DECLTYPE_TYPEs, and TRAIT_TYPEs are
     dependent; if the argument of the `typeof' expression is not
     type-dependent, then it should already been have resolved.  */
  if (TREE_CODE (type) == TYPEOF_TYPE
      || TREE_CODE (type) == DECLTYPE_TYPE
      || TREE_CODE (type) == TRAIT_TYPE)
    return true;

  /* A template argument pack is dependent if any of its packed
     arguments are.  */
  if (TREE_CODE (type) == TYPE_ARGUMENT_PACK)
    {
      tree args = ARGUMENT_PACK_ARGS (type);
      for (tree arg : tree_vec_range (args))
	if (dependent_template_arg_p (arg))
	  return true;
    }

  /* All TYPE_PACK_EXPANSIONs are dependent, because parameter packs must
     be template parameters.  */
  if (TREE_CODE (type) == TYPE_PACK_EXPANSION)
    return true;

  if (TREE_CODE (type) == DEPENDENT_OPERATOR_TYPE)
    return true;

  if (any_dependent_type_attributes_p (TYPE_ATTRIBUTES (type)))
    return true;

  /* The standard does not specifically mention types that are local
     to template functions or local classes, but they should be
     considered dependent too.  For example:

       template <int I> void f() {
	 enum E { a = I };
	 S<sizeof (E)> s;
       }

     The size of `E' cannot be known until the value of `I' has been
     determined.  Therefore, `E' must be considered dependent.  */
  scope = TYPE_CONTEXT (type);
  if (scope && TYPE_P (scope))
    return dependent_type_p (scope);
  /* Don't use type_dependent_expression_p here, as it can lead
     to infinite recursion trying to determine whether a lambda
     nested in a lambda is dependent (c++/47687).  */
  else if (scope && TREE_CODE (scope) == FUNCTION_DECL
	   && DECL_LANG_SPECIFIC (scope)
	   && DECL_TEMPLATE_INFO (scope)
	   && (any_dependent_template_arguments_p
	       (INNERMOST_TEMPLATE_ARGS (DECL_TI_ARGS (scope)))))
    return true;

  /* Other types are non-dependent.  */
  return false;
}

/* Returns TRUE if TYPE is dependent, in the sense of
   [temp.dep.type].  Note that a NULL type is considered dependent.  */

bool
dependent_type_p (tree type)
{
  /* If there are no template parameters in scope, then there can't be
     any dependent types.  */
  if (!processing_template_decl)
    {
      /* If we are not processing a template, then nobody should be
	 providing us with a dependent type.  */
      gcc_assert (type);
      gcc_assert (TREE_CODE (type) != TEMPLATE_TYPE_PARM || is_auto (type));
      return false;
    }

  /* If the type is NULL, we have not computed a type for the entity
     in question; in that case, the type is dependent.  */
  if (!type)
    return true;

  /* Erroneous types can be considered non-dependent.  */
  if (type == error_mark_node)
    return false;

  /* If we have not already computed the appropriate value for TYPE,
     do so now.  */
  if (!TYPE_DEPENDENT_P_VALID (type))
    {
      TYPE_DEPENDENT_P (type) = dependent_type_p_r (type);
      TYPE_DEPENDENT_P_VALID (type) = 1;
    }

  return TYPE_DEPENDENT_P (type);
}

/* Returns TRUE if SCOPE is a dependent scope, in which we can't do any
   lookup.  In other words, a dependent type that is not the current
   instantiation.  */

bool
dependent_scope_p (tree scope)
{
  return (scope && TYPE_P (scope) && dependent_type_p (scope)
	  && !currently_open_class (scope));
}

/* True if we might find more declarations in SCOPE during instantiation than
   we can when parsing the template.  */

bool
dependentish_scope_p (tree scope)
{
  return dependent_scope_p (scope) || any_dependent_bases_p (scope);
}

/* T is a SCOPE_REF.  Return whether it represents a non-static member of
   an unknown base of 'this' (and is therefore instantiation-dependent).  */

static bool
unknown_base_ref_p (tree t)
{
  if (!current_class_ptr)
    return false;

  tree mem = TREE_OPERAND (t, 1);
  if (shared_member_p (mem))
    return false;

  tree cur = current_nonlambda_class_type ();
  if (!any_dependent_bases_p (cur))
    return false;

  tree ctx = TREE_OPERAND (t, 0);
  if (DERIVED_FROM_P (ctx, cur))
    return false;

  return true;
}

/* T is a SCOPE_REF; return whether we need to consider it
    instantiation-dependent so that we can check access at instantiation
    time even though we know which member it resolves to.  */

static bool
instantiation_dependent_scope_ref_p (tree t)
{
  if (DECL_P (TREE_OPERAND (t, 1))
      && CLASS_TYPE_P (TREE_OPERAND (t, 0))
      && !dependent_scope_p (TREE_OPERAND (t, 0))
      && !unknown_base_ref_p (t)
      && accessible_in_template_p (TREE_OPERAND (t, 0),
				   TREE_OPERAND (t, 1)))
    return false;
  else
    return true;
}

/* Returns TRUE if the EXPRESSION is value-dependent, in the sense of
   [temp.dep.constexpr].  EXPRESSION is already known to be a constant
   expression.  */

/* Note that this predicate is not appropriate for general expressions;
   only constant expressions (that satisfy potential_constant_expression)
   can be tested for value dependence.  */

bool
value_dependent_expression_p (tree expression)
{
  if (!processing_template_decl || expression == NULL_TREE)
    return false;

  /* A type-dependent expression is also value-dependent.  */
  if (type_dependent_expression_p (expression))
    return true;

  switch (TREE_CODE (expression))
    {
    case BASELINK:
      /* A dependent member function of the current instantiation.  */
      return dependent_type_p (BINFO_TYPE (BASELINK_BINFO (expression)));

    case FUNCTION_DECL:
      /* A dependent member function of the current instantiation.  */
      if (DECL_CLASS_SCOPE_P (expression)
	  && dependent_type_p (DECL_CONTEXT (expression)))
	return true;
      break;

    case IDENTIFIER_NODE:
      /* A name that has not been looked up -- must be dependent.  */
      return true;

    case TEMPLATE_PARM_INDEX:
      /* A non-type template parm.  */
      return true;

    case CONST_DECL:
      /* A non-type template parm.  */
      if (DECL_TEMPLATE_PARM_P (expression))
	return true;
      return value_dependent_expression_p (DECL_INITIAL (expression));

    case VAR_DECL:
       /* A constant with literal type and is initialized
	  with an expression that is value-dependent.  */
      if (DECL_DEPENDENT_INIT_P (expression))
	return true;
      if (DECL_HAS_VALUE_EXPR_P (expression))
	{
	  tree value_expr = DECL_VALUE_EXPR (expression);
	  if (value_dependent_expression_p (value_expr)
	      /* __PRETTY_FUNCTION__ inside a template function is dependent
		 on the name of the function.  */
	      || (DECL_PRETTY_FUNCTION_P (expression)
		  /* It might be used in a template, but not a template
		     function, in which case its DECL_VALUE_EXPR will be
		     "top level".  */
		  && value_expr == error_mark_node))
	    return true;
	}
      else if (TYPE_REF_P (TREE_TYPE (expression)))
	/* FIXME cp_finish_decl doesn't fold reference initializers.  */
	return true;
      /* We have a constexpr variable and we're processing a template.  When
	 there's lifetime extension involved (for which finish_compound_literal
	 used to create a temporary), we'll not be able to evaluate the
	 variable until instantiating, so pretend it's value-dependent.  */
      else if (DECL_DECLARED_CONSTEXPR_P (expression)
	       && !TREE_CONSTANT (expression))
	return true;
      return false;

    case DYNAMIC_CAST_EXPR:
    case STATIC_CAST_EXPR:
    case CONST_CAST_EXPR:
    case REINTERPRET_CAST_EXPR:
    case CAST_EXPR:
    case IMPLICIT_CONV_EXPR:
      /* These expressions are value-dependent if the type to which
	 the cast occurs is dependent or the expression being casted
	 is value-dependent.  */
      {
	tree type = TREE_TYPE (expression);

	if (dependent_type_p (type))
	  return true;

	/* A functional cast has a list of operands.  */
	expression = TREE_OPERAND (expression, 0);
	if (!expression)
	  {
	    /* If there are no operands, it must be an expression such
	       as "int()". This should not happen for aggregate types
	       because it would form non-constant expressions.  */
	    gcc_assert (cxx_dialect >= cxx11
			|| INTEGRAL_OR_ENUMERATION_TYPE_P (type));

	    return false;
	  }

	if (TREE_CODE (expression) == TREE_LIST)
	  return any_value_dependent_elements_p (expression);

	if (TREE_CODE (type) == REFERENCE_TYPE
	    && has_value_dependent_address (expression))
	  return true;

	return value_dependent_expression_p (expression);
      }

    case SIZEOF_EXPR:
      if (SIZEOF_EXPR_TYPE_P (expression))
	return dependent_type_p (TREE_TYPE (TREE_OPERAND (expression, 0)));
      /* FALLTHRU */
    case ALIGNOF_EXPR:
    case TYPEID_EXPR:
      /* A `sizeof' expression is value-dependent if the operand is
	 type-dependent or is a pack expansion.  */
      expression = TREE_OPERAND (expression, 0);
      if (PACK_EXPANSION_P (expression))
        return true;
      else if (TYPE_P (expression))
	return dependent_type_p (expression);
      return instantiation_dependent_uneval_expression_p (expression);

    case AT_ENCODE_EXPR:
      /* An 'encode' expression is value-dependent if the operand is
	 type-dependent.  */
      expression = TREE_OPERAND (expression, 0);
      return dependent_type_p (expression);

    case NOEXCEPT_EXPR:
      expression = TREE_OPERAND (expression, 0);
      return instantiation_dependent_uneval_expression_p (expression);

    case SCOPE_REF:
      /* All instantiation-dependent expressions should also be considered
	 value-dependent.  */
      return instantiation_dependent_scope_ref_p (expression);

    case COMPONENT_REF:
      return (value_dependent_expression_p (TREE_OPERAND (expression, 0))
	      || value_dependent_expression_p (TREE_OPERAND (expression, 1)));

    case NONTYPE_ARGUMENT_PACK:
      /* A NONTYPE_ARGUMENT_PACK is value-dependent if any packed argument
         is value-dependent.  */
      for (tree arg : tree_vec_range (ARGUMENT_PACK_ARGS (expression)))
	if (value_dependent_expression_p (arg))
	  return true;
      return false;

    case TRAIT_EXPR:
      {
	if (dependent_type_p (TRAIT_EXPR_TYPE1 (expression)))
	  return true;

	tree type2 = TRAIT_EXPR_TYPE2 (expression);
	if (!type2)
	  return false;

	if (TREE_CODE (type2) != TREE_VEC)
	  return dependent_type_p (type2);

	for (tree arg : tree_vec_range (type2))
	  if (dependent_type_p (arg))
	    return true;

	return false;
      }

    case MODOP_EXPR:
      return ((value_dependent_expression_p (TREE_OPERAND (expression, 0)))
	      || (value_dependent_expression_p (TREE_OPERAND (expression, 2))));

    case ARRAY_REF:
      return ((value_dependent_expression_p (TREE_OPERAND (expression, 0)))
	      || (value_dependent_expression_p (TREE_OPERAND (expression, 1))));

    case ADDR_EXPR:
      {
	tree op = TREE_OPERAND (expression, 0);
	return (value_dependent_expression_p (op)
		|| has_value_dependent_address (op));
      }

    case REQUIRES_EXPR:
      /* Treat all requires-expressions as value-dependent so
         we don't try to fold them.  */
      return true;

    case TYPE_REQ:
      return dependent_type_p (TREE_OPERAND (expression, 0));

    case CALL_EXPR:
      {
	if (value_dependent_expression_p (CALL_EXPR_FN (expression)))
	  return true;
	tree fn = get_callee_fndecl (expression);
	int i, nargs;
	nargs = call_expr_nargs (expression);
	for (i = 0; i < nargs; ++i)
	  {
	    tree op = CALL_EXPR_ARG (expression, i);
	    /* In a call to a constexpr member function, look through the
	       implicit ADDR_EXPR on the object argument so that it doesn't
	       cause the call to be considered value-dependent.  We also
	       look through it in potential_constant_expression.  */
	    if (i == 0 && fn && DECL_DECLARED_CONSTEXPR_P (fn)
		&& DECL_NONSTATIC_MEMBER_FUNCTION_P (fn)
		&& TREE_CODE (op) == ADDR_EXPR)
	      op = TREE_OPERAND (op, 0);
	    if (value_dependent_expression_p (op))
	      return true;
	  }
	return false;
      }

    case TEMPLATE_ID_EXPR:
      return concept_definition_p (TREE_OPERAND (expression, 0))
	&& any_dependent_template_arguments_p (TREE_OPERAND (expression, 1));

    case CONSTRUCTOR:
      {
	unsigned ix;
	tree val;
	if (dependent_type_p (TREE_TYPE (expression)))
	  return true;
	FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (expression), ix, val)
	  if (value_dependent_expression_p (val))
	    return true;
	return false;
      }

    case STMT_EXPR:
      /* Treat a GNU statement expression as dependent to avoid crashing
	 under instantiate_non_dependent_expr; it can't be constant.  */
      return true;

    case NEW_EXPR:
    case VEC_NEW_EXPR:
      /* The second operand is a type, which type_dependent_expression_p
	 (and therefore value_dependent_expression_p) doesn't want to see.  */
      return (value_dependent_expression_p (TREE_OPERAND (expression, 0))
	      || value_dependent_expression_p (TREE_OPERAND (expression, 2))
	      || value_dependent_expression_p (TREE_OPERAND (expression, 3)));

    default:
      /* A constant expression is value-dependent if any subexpression is
	 value-dependent.  */
      switch (TREE_CODE_CLASS (TREE_CODE (expression)))
	{
	case tcc_reference:
	case tcc_unary:
	case tcc_comparison:
	case tcc_binary:
	case tcc_expression:
	case tcc_vl_exp:
	  {
	    int i, len = cp_tree_operand_length (expression);

	    for (i = 0; i < len; i++)
	      {
		tree t = TREE_OPERAND (expression, i);

		/* In some cases, some of the operands may be missing.
		   (For example, in the case of PREDECREMENT_EXPR, the
		   amount to increment by may be missing.)  That doesn't
		   make the expression dependent.  */
		if (t && value_dependent_expression_p (t))
		  return true;
	      }
	  }
	  break;
	default:
	  break;
	}
      break;
    }

  /* The expression is not value-dependent.  */
  return false;
}

/* Returns TRUE if the EXPRESSION is type-dependent, in the sense of
   [temp.dep.expr].  Note that an expression with no type is
   considered dependent.  Other parts of the compiler arrange for an
   expression with type-dependent subexpressions to have no type, so
   this function doesn't have to be fully recursive.  */

bool
type_dependent_expression_p (tree expression)
{
  if (!processing_template_decl)
    return false;

  if (expression == NULL_TREE || expression == error_mark_node)
    return false;

  gcc_checking_assert (!TYPE_P (expression));

  STRIP_ANY_LOCATION_WRAPPER (expression);

  /* An unresolved name is always dependent.  */
  if (identifier_p (expression)
      || TREE_CODE (expression) == USING_DECL
      || TREE_CODE (expression) == WILDCARD_DECL)
    return true;

  /* A lambda-expression in template context is dependent.  dependent_type_p is
     true for a lambda in the scope of a class or function template, but that
     doesn't cover all template contexts, like a default template argument.  */
  if (TREE_CODE (expression) == LAMBDA_EXPR)
    return true;

  /* A fold expression is type-dependent. */
  if (TREE_CODE (expression) == UNARY_LEFT_FOLD_EXPR
      || TREE_CODE (expression) == UNARY_RIGHT_FOLD_EXPR
      || TREE_CODE (expression) == BINARY_LEFT_FOLD_EXPR
      || TREE_CODE (expression) == BINARY_RIGHT_FOLD_EXPR)
    return true;

  /* Some expression forms are never type-dependent.  */
  if (TREE_CODE (expression) == SIZEOF_EXPR
      || TREE_CODE (expression) == ALIGNOF_EXPR
      || TREE_CODE (expression) == AT_ENCODE_EXPR
      || TREE_CODE (expression) == NOEXCEPT_EXPR
      || TREE_CODE (expression) == TRAIT_EXPR
      || TREE_CODE (expression) == TYPEID_EXPR
      || TREE_CODE (expression) == DELETE_EXPR
      || TREE_CODE (expression) == VEC_DELETE_EXPR
      || TREE_CODE (expression) == THROW_EXPR
      || TREE_CODE (expression) == REQUIRES_EXPR)
    return false;

  /* The types of these expressions depends only on the type to which
     the cast occurs.  */
  if (TREE_CODE (expression) == DYNAMIC_CAST_EXPR
      || TREE_CODE (expression) == STATIC_CAST_EXPR
      || TREE_CODE (expression) == CONST_CAST_EXPR
      || TREE_CODE (expression) == REINTERPRET_CAST_EXPR
      || TREE_CODE (expression) == IMPLICIT_CONV_EXPR
      || TREE_CODE (expression) == CAST_EXPR)
    return dependent_type_p (TREE_TYPE (expression));

  /* The types of these expressions depends only on the type created
     by the expression.  */
  if (TREE_CODE (expression) == NEW_EXPR
      || TREE_CODE (expression) == VEC_NEW_EXPR)
    {
      /* For NEW_EXPR tree nodes created inside a template, either
	 the object type itself or a TREE_LIST may appear as the
	 operand 1.  */
      tree type = TREE_OPERAND (expression, 1);
      if (TREE_CODE (type) == TREE_LIST)
	/* This is an array type.  We need to check array dimensions
	   as well.  */
	return dependent_type_p (TREE_VALUE (TREE_PURPOSE (type)))
	       || value_dependent_expression_p
		    (TREE_OPERAND (TREE_VALUE (type), 1));
      /* Array type whose dimension has to be deduced.  */
      else if (TREE_CODE (type) == ARRAY_TYPE
	       && TREE_OPERAND (expression, 2) == NULL_TREE)
	return true;
      else
	return dependent_type_p (type);
    }

  if (TREE_CODE (expression) == SCOPE_REF)
    {
      tree scope = TREE_OPERAND (expression, 0);
      tree name = TREE_OPERAND (expression, 1);

      /* 14.6.2.2 [temp.dep.expr]: An id-expression is type-dependent if it
	 contains an identifier associated by name lookup with one or more
	 declarations declared with a dependent type, or...a
	 nested-name-specifier or qualified-id that names a member of an
	 unknown specialization.  */
      return (type_dependent_expression_p (name)
	      || dependent_scope_p (scope));
    }

  if (TREE_CODE (expression) == TEMPLATE_DECL
      && !DECL_TEMPLATE_TEMPLATE_PARM_P (expression))
    return uses_outer_template_parms (expression);

  if (TREE_CODE (expression) == STMT_EXPR)
    expression = stmt_expr_value_expr (expression);

  if (BRACE_ENCLOSED_INITIALIZER_P (expression))
    {
      for (auto &elt : CONSTRUCTOR_ELTS (expression))
	if (type_dependent_expression_p (elt.value))
	  return true;
      return false;
    }

  /* A static data member of the current instantiation with incomplete
     array type is type-dependent, as the definition and specializations
     can have different bounds.  */
  if (VAR_P (expression)
      && DECL_CLASS_SCOPE_P (expression)
      && dependent_type_p (DECL_CONTEXT (expression))
      && VAR_HAD_UNKNOWN_BOUND (expression))
    return true;

  /* An array of unknown bound depending on a variadic parameter, eg:

     template<typename... Args>
       void foo (Args... args)
       {
         int arr[] = { args... };
       }

     template<int... vals>
       void bar ()
       {
         int arr[] = { vals... };
       }

     If the array has no length and has an initializer, it must be that
     we couldn't determine its length in cp_complete_array_type because
     it is dependent.  */
  if (((VAR_P (expression) && DECL_INITIAL (expression))
       || COMPOUND_LITERAL_P (expression))
      && TREE_TYPE (expression) != NULL_TREE
      && TREE_CODE (TREE_TYPE (expression)) == ARRAY_TYPE
      && !TYPE_DOMAIN (TREE_TYPE (expression)))
   return true;

  /* Pull a FUNCTION_DECL out of a BASELINK if we can.  */
  if (BASELINK_P (expression))
    {
      if (BASELINK_OPTYPE (expression)
	  && dependent_type_p (BASELINK_OPTYPE (expression)))
	return true;
      expression = BASELINK_FUNCTIONS (expression);
    }

  /* A function or variable template-id is type-dependent if it has any
     dependent template arguments.  */
  if (VAR_OR_FUNCTION_DECL_P (expression)
      && DECL_LANG_SPECIFIC (expression)
      && DECL_TEMPLATE_INFO (expression))
    {
      /* Consider the innermost template arguments, since those are the ones
	 that come from the template-id; the template arguments for the
	 enclosing class do not make it type-dependent unless they are used in
	 the type of the decl.  */
      if (instantiates_primary_template_p (expression)
	  && (any_dependent_template_arguments_p
	      (INNERMOST_TEMPLATE_ARGS (DECL_TI_ARGS (expression)))))
	return true;
    }

  /* Otherwise, if the function decl isn't from a dependent scope, it can't be
     type-dependent.  Checking this is important for functions with auto return
     type, which looks like a dependent type.  */
  if (TREE_CODE (expression) == FUNCTION_DECL
      && !(DECL_CLASS_SCOPE_P (expression)
	   && dependent_type_p (DECL_CONTEXT (expression)))
      && !(DECL_LANG_SPECIFIC (expression)
	   && DECL_UNIQUE_FRIEND_P (expression)
	   && (!DECL_FRIEND_CONTEXT (expression)
	       || dependent_type_p (DECL_FRIEND_CONTEXT (expression))))
      && !DECL_LOCAL_DECL_P (expression))
    {
      gcc_assert (!dependent_type_p (TREE_TYPE (expression))
		  || undeduced_auto_decl (expression));
      return false;
    }

  /* Otherwise, its constraints could still depend on outer template parameters
     from its (dependent) scope.  */
  if (TREE_CODE (expression) == FUNCTION_DECL
      /* As an optimization, check this cheaper sufficient condition first.
	 (At this point we've established that we're looking at a member of
	 a dependent class, so it makes sense to start treating say undeduced
	 auto as dependent.)  */
      && !dependent_type_p (TREE_TYPE (expression))
      && uses_outer_template_parms_in_constraints (expression))
    return true;

  /* Always dependent, on the number of arguments if nothing else.  */
  if (TREE_CODE (expression) == EXPR_PACK_EXPANSION)
    return true;

  if (TREE_TYPE (expression) == unknown_type_node)
    {
      if (TREE_CODE (expression) == ADDR_EXPR)
	return type_dependent_expression_p (TREE_OPERAND (expression, 0));
      if (TREE_CODE (expression) == COMPONENT_REF
	  || TREE_CODE (expression) == OFFSET_REF)
	{
	  if (type_dependent_expression_p (TREE_OPERAND (expression, 0)))
	    return true;
	  expression = TREE_OPERAND (expression, 1);
	  if (identifier_p (expression))
	    return false;
	}
      /* SCOPE_REF with non-null TREE_TYPE is always non-dependent.  */
      if (TREE_CODE (expression) == SCOPE_REF)
	return false;

      /* CO_AWAIT/YIELD_EXPR with unknown type is always dependent.  */
      if (TREE_CODE (expression) == CO_AWAIT_EXPR
	  || TREE_CODE (expression) == CO_YIELD_EXPR)
	return true;

      if (BASELINK_P (expression))
	{
	  if (BASELINK_OPTYPE (expression)
	      && dependent_type_p (BASELINK_OPTYPE (expression)))
	    return true;
	  expression = BASELINK_FUNCTIONS (expression);
	}

      if (TREE_CODE (expression) == TEMPLATE_ID_EXPR)
	{
	  if (any_dependent_template_arguments_p
	      (TREE_OPERAND (expression, 1)))
	    return true;
	  expression = TREE_OPERAND (expression, 0);
	  if (identifier_p (expression))
	    return true;
	}

      gcc_assert (OVL_P (expression));

      for (lkp_iterator iter (expression); iter; ++iter)
	if (type_dependent_expression_p (*iter))
	  return true;

      return false;
    }

  /* The type of a non-type template parm declared with a placeholder type
     depends on the corresponding template argument, even though
     placeholders are not normally considered dependent.  */
  if (TREE_CODE (expression) == TEMPLATE_PARM_INDEX
      && is_auto (TREE_TYPE (expression)))
    return true;

  gcc_assert (TREE_CODE (expression) != TYPE_DECL);

  /* Dependent type attributes might not have made it from the decl to
     the type yet.  */
  if (DECL_P (expression)
      && any_dependent_type_attributes_p (DECL_ATTRIBUTES (expression)))
    return true;

  return (dependent_type_p (TREE_TYPE (expression)));
}

/* [temp.dep.expr]/5: A class member access expression (5.2.5) is
   type-dependent if the expression refers to a member of the current
   instantiation and the type of the referenced member is dependent, or the
   class member access expression refers to a member of an unknown
   specialization.

   This function returns true if the OBJECT in such a class member access
   expression is of an unknown specialization.  */

bool
type_dependent_object_expression_p (tree object)
{
  /* An IDENTIFIER_NODE can sometimes have a TREE_TYPE, but it's still
     dependent.  */
  if (TREE_CODE (object) == IDENTIFIER_NODE)
    return true;
  tree scope = TREE_TYPE (object);
  return (!scope || dependent_scope_p (scope));
}

/* walk_tree callback function for instantiation_dependent_expression_p,
   below.  Returns non-zero if a dependent subexpression is found.  */

static tree
instantiation_dependent_r (tree *tp, int *walk_subtrees,
			   void * /*data*/)
{
  if (TYPE_P (*tp))
    {
      /* We don't have to worry about decltype currently because decltype
	 of an instantiation-dependent expr is a dependent type.  This
	 might change depending on the resolution of DR 1172.  */
      *walk_subtrees = false;
      return NULL_TREE;
    }
  enum tree_code code = TREE_CODE (*tp);
  switch (code)
    {
      /* Don't treat an argument list as dependent just because it has no
	 TREE_TYPE.  */
    case TREE_LIST:
    case TREE_VEC:
    case NONTYPE_ARGUMENT_PACK:
      return NULL_TREE;

    case TEMPLATE_PARM_INDEX:
      if (dependent_type_p (TREE_TYPE (*tp)))
	return *tp;
      if (TEMPLATE_PARM_PARAMETER_PACK (*tp))
	return *tp;
      /* We'll check value-dependence separately.  */
      return NULL_TREE;

      /* Handle expressions with type operands.  */
    case SIZEOF_EXPR:
    case ALIGNOF_EXPR:
    case TYPEID_EXPR:
    case AT_ENCODE_EXPR:
      {
	tree op = TREE_OPERAND (*tp, 0);
	if (code == SIZEOF_EXPR && SIZEOF_EXPR_TYPE_P (*tp))
	  op = TREE_TYPE (op);
	if (TYPE_P (op))
	  {
	    if (dependent_type_p (op))
	      return *tp;
	    else
	      {
		*walk_subtrees = false;
		return NULL_TREE;
	      }
	  }
	break;
      }

    case COMPONENT_REF:
      if (identifier_p (TREE_OPERAND (*tp, 1)))
	/* In a template, finish_class_member_access_expr creates a
	   COMPONENT_REF with an IDENTIFIER_NODE for op1 even if it isn't
	   type-dependent, so that we can check access control at
	   instantiation time (PR 42277).  See also Core issue 1273.  */
	return *tp;
      break;

    case SCOPE_REF:
      if (instantiation_dependent_scope_ref_p (*tp))
	return *tp;
      else
	break;

      /* Treat statement-expressions as dependent.  */
    case BIND_EXPR:
      return *tp;

      /* Treat requires-expressions as dependent. */
    case REQUIRES_EXPR:
      return *tp;

    case CONSTRUCTOR:
      if (CONSTRUCTOR_IS_DEPENDENT (*tp))
	return *tp;
      break;

    case TEMPLATE_DECL:
    case FUNCTION_DECL:
      /* Before C++17, a noexcept-specifier isn't part of the function type
	 so it doesn't affect type dependence, but we still want to consider it
	 for instantiation dependence.  */
      if (cxx_dialect < cxx17
	  && DECL_DECLARES_FUNCTION_P (*tp)
	  && value_dependent_noexcept_spec_p (TREE_TYPE (*tp)))
	return *tp;
      break;

    default:
      break;
    }

  if (type_dependent_expression_p (*tp))
    return *tp;
  else
    return NULL_TREE;
}

/* Returns TRUE if the EXPRESSION is instantiation-dependent, in the
   sense defined by the ABI:

   "An expression is instantiation-dependent if it is type-dependent
   or value-dependent, or it has a subexpression that is type-dependent
   or value-dependent."

   Except don't actually check value-dependence for unevaluated expressions,
   because in sizeof(i) we don't care about the value of i.  Checking
   type-dependence will in turn check value-dependence of array bounds/template
   arguments as needed.  */

bool
instantiation_dependent_uneval_expression_p (tree expression)
{
  tree result;

  if (!processing_template_decl)
    return false;

  if (expression == error_mark_node)
    return false;

  result = cp_walk_tree_without_duplicates (&expression,
					    instantiation_dependent_r, NULL);
  return result != NULL_TREE;
}

/* As above, but also check value-dependence of the expression as a whole.  */

bool
instantiation_dependent_expression_p (tree expression)
{
  return (instantiation_dependent_uneval_expression_p (expression)
	  || (processing_template_decl
	      && potential_constant_expression (expression)
	      && value_dependent_expression_p (expression)));
}

/* Like type_dependent_expression_p, but it also works while not processing
   a template definition, i.e. during substitution or mangling.  */

bool
type_dependent_expression_p_push (tree expr)
{
  bool b;
  ++processing_template_decl;
  b = type_dependent_expression_p (expr);
  --processing_template_decl;
  return b;
}

/* Returns TRUE if ARGS contains a type-dependent expression.  */

bool
any_type_dependent_arguments_p (const vec<tree, va_gc> *args)
{
  if (!processing_template_decl || !args)
    return false;

  for (tree arg : *args)
    if (type_dependent_expression_p (arg))
      return true;

  return false;
}

/* Returns TRUE if LIST (a TREE_LIST whose TREE_VALUEs are
   expressions) contains any type-dependent expressions.  */

bool
any_type_dependent_elements_p (const_tree list)
{
  for (; list; list = TREE_CHAIN (list))
    if (type_dependent_expression_p (TREE_VALUE (list)))
      return true;

  return false;
}

/* Returns TRUE if LIST (a TREE_LIST whose TREE_VALUEs are
   expressions) contains any value-dependent expressions.  */

bool
any_value_dependent_elements_p (const_tree list)
{
  for (; list; list = TREE_CHAIN (list))
    if (value_dependent_expression_p (TREE_VALUE (list)))
      return true;

  return false;
}

/* Returns TRUE if the ARG (a template argument) is dependent.  */

bool
dependent_template_arg_p (tree arg)
{
  if (!processing_template_decl)
    return false;

  /* Assume a template argument that was wrongly written by the user
     is dependent. This is consistent with what
     any_dependent_template_arguments_p [that calls this function]
     does.  */
  if (!arg || arg == error_mark_node)
    return true;

  if (TREE_CODE (arg) == ARGUMENT_PACK_SELECT)
    arg = argument_pack_select_arg (arg);

  if (TREE_CODE (arg) == TEMPLATE_TEMPLATE_PARM)
    return true;
  if (TREE_CODE (arg) == TEMPLATE_DECL)
    {
      if (DECL_TEMPLATE_PARM_P (arg))
	return true;
      /* A member template of a dependent class is not necessarily
	 type-dependent, but it is a dependent template argument because it
	 will be a member of an unknown specialization to that template.  */
      tree scope = CP_DECL_CONTEXT (arg);
      return TYPE_P (scope) && dependent_type_p (scope);
    }
  else if (ARGUMENT_PACK_P (arg))
    {
      tree args = ARGUMENT_PACK_ARGS (arg);
      for (tree arg : tree_vec_range (args))
	if (dependent_template_arg_p (arg))
	  return true;
      return false;
    }
  else if (TYPE_P (arg))
    return dependent_type_p (arg);
  else
    return value_dependent_expression_p (arg);
}

/* Identify any expressions that use function parms.  */

static tree
find_parm_usage_r (tree *tp, int *walk_subtrees, void*)
{
  tree t = *tp;
  if (TREE_CODE (t) == PARM_DECL)
    {
      *walk_subtrees = 0;
      return t;
    }
  return NULL_TREE;
}

/* Returns true if a type specialization formed using the template
   arguments ARGS needs to use structural equality.  */

bool
any_template_arguments_need_structural_equality_p (tree args)
{
  int i;
  int j;

  if (!args)
    return false;
  if (args == error_mark_node)
    return true;

  for (i = 0; i < TMPL_ARGS_DEPTH (args); ++i)
    {
      tree level = TMPL_ARGS_LEVEL (args, i + 1);
      for (j = 0; j < TREE_VEC_LENGTH (level); ++j)
	{
	  tree arg = TREE_VEC_ELT (level, j);
	  tree packed_args = NULL_TREE;
	  int k, len = 1;

	  if (ARGUMENT_PACK_P (arg))
	    {
	      /* Look inside the argument pack.  */
	      packed_args = ARGUMENT_PACK_ARGS (arg);
	      len = TREE_VEC_LENGTH (packed_args);
	    }

	  for (k = 0; k < len; ++k)
	    {
	      if (packed_args)
		arg = TREE_VEC_ELT (packed_args, k);

	      if (error_operand_p (arg))
		return true;
	      else if (TREE_CODE (arg) == TEMPLATE_DECL)
		continue;
	      else if (arg == any_targ_node)
		/* An any_targ_node argument (added by add_defaults_to_ttp)
		   makes the corresponding specialization not canonicalizable,
		   since template_args_equal always return true for it.  We
		   may see this when called from bind_template_template_parm.  */
		return true;
	      /* Checking current_function_decl because this structural
		 comparison is only necessary for redeclaration.  */
	      else if (!current_function_decl
		       && dependent_template_arg_p (arg)
		       && (cp_walk_tree_without_duplicates
			   (&arg, find_parm_usage_r, NULL)))
		/* The identity of a class template specialization that uses
		   a function parameter depends on the identity of the function.
		   And if this specialization appeared in the trailing return
		   type thereof, we don't know the identity of the function
		   (e.g. if it's a redeclaration or a new function) until we
		   form its signature and go through duplicate_decls.  Thus
		   it's unsafe to decide on a canonical type now (which depends
		   on the DECL_CONTEXT of the function parameter, which can get
		   mutated after the fact by duplicate_decls), so just require
		   structural equality in this case (PR52830).  */
		return true;
	    }
	}
    }

  return false;
}

/* Returns true if ARGS (a collection of template arguments) contains
   any dependent arguments.  */

bool
any_dependent_template_arguments_p (const_tree args)
{
  if (args == error_mark_node)
    return true;
  if (!processing_template_decl || !args)
    return false;

  for (int i = 0, depth = TMPL_ARGS_DEPTH (args); i < depth; ++i)
    {
      const_tree level = TMPL_ARGS_LEVEL (args, i + 1);
      for (tree arg : tree_vec_range (CONST_CAST_TREE (level)))
	if (dependent_template_arg_p (arg))
	  return true;
    }

  return false;
}

/* Returns true if ARGS contains any errors.  */

bool
any_erroneous_template_args_p (const_tree args)
{
  int i;
  int j;

  if (args == error_mark_node)
    return true;

  if (args && TREE_CODE (args) != TREE_VEC)
    {
      if (tree ti = get_template_info (args))
	args = TI_ARGS (ti);
      else
	args = NULL_TREE;
    }

  if (!args)
    return false;

  for (i = 0; i < TMPL_ARGS_DEPTH (args); ++i)
    {
      const_tree level = TMPL_ARGS_LEVEL (args, i + 1);
      for (j = 0; j < TREE_VEC_LENGTH (level); ++j)
	if (error_operand_p (TREE_VEC_ELT (level, j)))
	  return true;
    }

  return false;
}

/* Returns TRUE if the template TMPL is type-dependent.  */

bool
dependent_template_p (tree tmpl)
{
  if (TREE_CODE (tmpl) == OVERLOAD)
    {
      for (lkp_iterator iter (tmpl); iter; ++iter)
	if (dependent_template_p (*iter))
	  return true;
      return false;
    }

  /* Template template parameters are dependent.  */
  if (DECL_TEMPLATE_TEMPLATE_PARM_P (tmpl)
      || TREE_CODE (tmpl) == TEMPLATE_TEMPLATE_PARM)
    return true;
  /* So are names that have not been looked up.  */
  if (TREE_CODE (tmpl) == SCOPE_REF || identifier_p (tmpl))
    return true;
  return false;
}

/* Returns TRUE if the specialization TMPL<ARGS> is dependent.  */

bool
dependent_template_id_p (tree tmpl, tree args)
{
  return (dependent_template_p (tmpl)
	  || any_dependent_template_arguments_p (args));
}

/* Returns TRUE if OMP_FOR with DECLV, INITV, CONDV and INCRV vectors
   are dependent.  */

bool
dependent_omp_for_p (tree declv, tree initv, tree condv, tree incrv)
{
  int i;

  if (!processing_template_decl)
    return false;

  for (i = 0; i < TREE_VEC_LENGTH (declv); i++)
    {
      tree decl = TREE_VEC_ELT (declv, i);
      tree init = TREE_VEC_ELT (initv, i);
      tree cond = TREE_VEC_ELT (condv, i);
      tree incr = TREE_VEC_ELT (incrv, i);

      if (type_dependent_expression_p (decl)
	  || TREE_CODE (decl) == SCOPE_REF)
	return true;

      if (init && type_dependent_expression_p (init))
	return true;

      if (cond == global_namespace)
	return true;

      if (type_dependent_expression_p (cond))
	return true;

      if (COMPARISON_CLASS_P (cond)
	  && (type_dependent_expression_p (TREE_OPERAND (cond, 0))
	      || type_dependent_expression_p (TREE_OPERAND (cond, 1))))
	return true;

      if (TREE_CODE (incr) == MODOP_EXPR)
	{
	  if (type_dependent_expression_p (TREE_OPERAND (incr, 0))
	      || type_dependent_expression_p (TREE_OPERAND (incr, 2)))
	    return true;
	}
      else if (type_dependent_expression_p (incr))
	return true;
      else if (TREE_CODE (incr) == MODIFY_EXPR)
	{
	  if (type_dependent_expression_p (TREE_OPERAND (incr, 0)))
	    return true;
	  else if (BINARY_CLASS_P (TREE_OPERAND (incr, 1)))
	    {
	      tree t = TREE_OPERAND (incr, 1);
	      if (type_dependent_expression_p (TREE_OPERAND (t, 0))
		  || type_dependent_expression_p (TREE_OPERAND (t, 1)))
		return true;

	      /* If this loop has a class iterator with != comparison
		 with increment other than i++/++i/i--/--i, make sure the
		 increment is constant.  */
	      if (CLASS_TYPE_P (TREE_TYPE (decl))
		  && TREE_CODE (cond) == NE_EXPR)
		{
		  if (TREE_OPERAND (t, 0) == decl)
		    t = TREE_OPERAND (t, 1);
		  else
		    t = TREE_OPERAND (t, 0);
		  if (TREE_CODE (t) != INTEGER_CST)
		    return true;
		}
	    }
	}
    }

  return false;
}

/* TYPE is a TYPENAME_TYPE.  Returns the ordinary TYPE to which the
   TYPENAME_TYPE corresponds.  Returns the original TYPENAME_TYPE if
   no such TYPE can be found.  Note that this function peers inside
   uninstantiated templates and therefore should be used only in
   extremely limited situations.  ONLY_CURRENT_P restricts this
   peering to the currently open classes hierarchy (which is required
   when comparing types).  */

tree
resolve_typename_type (tree type, bool only_current_p)
{
  tree scope;
  tree name;
  tree decl;
  int quals;
  tree pushed_scope;
  tree result;

  gcc_assert (TREE_CODE (type) == TYPENAME_TYPE);

  scope = TYPE_CONTEXT (type);
  /* We shouldn't have built a TYPENAME_TYPE with a non-dependent scope.  */
  gcc_checking_assert (uses_template_parms (scope));

  /* Usually the non-qualified identifier of a TYPENAME_TYPE is
     TYPE_IDENTIFIER (type). But when 'type' is a typedef variant of a
     TYPENAME_TYPE node, then TYPE_NAME (type) is set to the TYPE_DECL
     representing the typedef. In that case TYPE_IDENTIFIER (type) is
     not the non-qualified identifier of the TYPENAME_TYPE anymore.
     So by getting the TYPE_IDENTIFIER of the _main declaration_ of
     the TYPENAME_TYPE instead, we avoid messing up with a possible
     typedef variant case.  */
  name = TYPE_IDENTIFIER (TYPE_MAIN_VARIANT (type));

  /* If the SCOPE is itself a TYPENAME_TYPE, then we need to resolve
     it first before we can figure out what NAME refers to.  */
  if (TREE_CODE (scope) == TYPENAME_TYPE)
    {
      if (TYPENAME_IS_RESOLVING_P (scope))
	/* Given a class template A with a dependent base with nested type C,
	   typedef typename A::C::C C will land us here, as trying to resolve
	   the initial A::C leads to the local C typedef, which leads back to
	   A::C::C.  So we break the recursion now.  */
	return type;
      else
	scope = resolve_typename_type (scope, only_current_p);
    }
  /* If we don't know what SCOPE refers to, then we cannot resolve the
     TYPENAME_TYPE.  */
  if (!CLASS_TYPE_P (scope))
    return type;
  /* If this is a typedef, we don't want to look inside (c++/11987).  */
  if (typedef_variant_p (type))
    return type;
  /* If SCOPE isn't the template itself, it will not have a valid
     TYPE_FIELDS list.  */
  if (same_type_p (scope, CLASSTYPE_PRIMARY_TEMPLATE_TYPE (scope)))
    /* scope is either the template itself or a compatible instantiation
       like X<T>, so look up the name in the original template.  */
    scope = CLASSTYPE_PRIMARY_TEMPLATE_TYPE (scope);
  /* If scope has no fields, it can't be a current instantiation.  Check this
     before currently_open_class to avoid infinite recursion (71515).  */
  if (!TYPE_FIELDS (scope))
    return type;
  /* If the SCOPE is not the current instantiation, there's no reason
     to look inside it.  */
  if (only_current_p && !currently_open_class (scope))
    return type;
  /* Enter the SCOPE so that name lookup will be resolved as if we
     were in the class definition.  In particular, SCOPE will no
     longer be considered a dependent type.  */
  pushed_scope = push_scope (scope);
  /* Look up the declaration.  */
  decl = lookup_member (scope, name, /*protect=*/0, /*want_type=*/true,
			tf_warning_or_error);

  result = NULL_TREE;

  /* For a TYPENAME_TYPE like "typename X::template Y<T>", we want to
     find a TEMPLATE_DECL.  Otherwise, we want to find a TYPE_DECL.  */
  tree fullname = TYPENAME_TYPE_FULLNAME (type);
  if (!decl)
    /*nop*/;
  else if (identifier_p (fullname)
	   && TREE_CODE (decl) == TYPE_DECL)
    {
      result = TREE_TYPE (decl);
      if (result == error_mark_node)
	result = NULL_TREE;
    }
  else if (TREE_CODE (fullname) == TEMPLATE_ID_EXPR
	   && DECL_CLASS_TEMPLATE_P (decl))
    {
      /* Obtain the template and the arguments.  */
      tree tmpl = TREE_OPERAND (fullname, 0);
      if (TREE_CODE (tmpl) == IDENTIFIER_NODE)
	{
	  /* We get here with a plain identifier because a previous tentative
	     parse of the nested-name-specifier as part of a ptr-operator saw
	     ::template X<A>.  The use of ::template is necessary in a
	     ptr-operator, but wrong in a declarator-id.

	     [temp.names]: In a qualified-id of a declarator-id, the keyword
	     template shall not appear at the top level.  */
	  pedwarn (cp_expr_loc_or_input_loc (fullname), OPT_Wpedantic,
		   "keyword %<template%> not allowed in declarator-id");
	  tmpl = decl;
	}
      tree args = TREE_OPERAND (fullname, 1);
      /* Instantiate the template.  */
      result = lookup_template_class (tmpl, args, NULL_TREE, NULL_TREE,
				      /*entering_scope=*/true,
				      tf_error | tf_user);
      if (result == error_mark_node)
	result = NULL_TREE;
    }

  /* Leave the SCOPE.  */
  if (pushed_scope)
    pop_scope (pushed_scope);

  /* If we failed to resolve it, return the original typename.  */
  if (!result)
    return type;

  /* If lookup found a typename type, resolve that too.  */
  if (TREE_CODE (result) == TYPENAME_TYPE && !TYPENAME_IS_RESOLVING_P (result))
    {
      /* Ill-formed programs can cause infinite recursion here, so we
	 must catch that.  */
      TYPENAME_IS_RESOLVING_P (result) = 1;
      result = resolve_typename_type (result, only_current_p);
      TYPENAME_IS_RESOLVING_P (result) = 0;
    }

  /* Qualify the resulting type.  */
  quals = cp_type_quals (type);
  if (quals)
    result = cp_build_qualified_type (result, cp_type_quals (result) | quals);

  return result;
}

/* EXPR is an expression which is not type-dependent.  Return a proxy
   for EXPR that can be used to compute the types of larger
   expressions containing EXPR.  */

tree
build_non_dependent_expr (tree expr)
{
  tree orig_expr = expr;
  tree inner_expr;

  /* When checking, try to get a constant value for all non-dependent
     expressions in order to expose bugs in *_dependent_expression_p
     and constexpr.  This can affect code generation, see PR70704, so
     only do this for -fchecking=2.  */
  if (flag_checking > 1
      && cxx_dialect >= cxx11
      /* Don't do this during nsdmi parsing as it can lead to
	 unexpected recursive instantiations.  */
      && !parsing_nsdmi ()
      /* Don't do this during concept processing either and for
         the same reason.  */
      && !processing_constraint_expression_p ())
    fold_non_dependent_expr (expr, tf_none);

  STRIP_ANY_LOCATION_WRAPPER (expr);

  /* Preserve OVERLOADs; the functions must be available to resolve
     types.  */
  inner_expr = expr;
  if (TREE_CODE (inner_expr) == STMT_EXPR)
    inner_expr = stmt_expr_value_expr (inner_expr);
  if (TREE_CODE (inner_expr) == ADDR_EXPR)
    inner_expr = TREE_OPERAND (inner_expr, 0);
  if (TREE_CODE (inner_expr) == COMPONENT_REF)
    inner_expr = TREE_OPERAND (inner_expr, 1);
  if (is_overloaded_fn (inner_expr)
      || TREE_CODE (inner_expr) == OFFSET_REF)
    return orig_expr;
  /* There is no need to return a proxy for a variable, parameter
     or enumerator.  */
  if (VAR_P (expr) || TREE_CODE (expr) == PARM_DECL
      || TREE_CODE (expr) == CONST_DECL)
    return orig_expr;
  /* Preserve string constants; conversions from string constants to
     "char *" are allowed, even though normally a "const char *"
     cannot be used to initialize a "char *".  */
  if (TREE_CODE (expr) == STRING_CST)
    return orig_expr;
  /* Preserve void and arithmetic constants, as an optimization -- there is no
     reason to create a new node.  */
  if (TREE_CODE (expr) == VOID_CST
      || TREE_CODE (expr) == INTEGER_CST
      || TREE_CODE (expr) == REAL_CST)
    return orig_expr;
  /* Preserve THROW_EXPRs -- all throw-expressions have type "void".
     There is at least one place where we want to know that a
     particular expression is a throw-expression: when checking a ?:
     expression, there are special rules if the second or third
     argument is a throw-expression.  */
  if (TREE_CODE (expr) == THROW_EXPR)
    return orig_expr;

  /* Don't wrap an initializer list, we need to be able to look inside.  */
  if (BRACE_ENCLOSED_INITIALIZER_P (expr))
    return orig_expr;

  /* Don't wrap a dummy object, we need to be able to test for it.  */
  if (is_dummy_object (expr))
    return orig_expr;

  if (TREE_CODE (expr) == COND_EXPR)
    return build3 (COND_EXPR,
		   TREE_TYPE (expr),
		   build_non_dependent_expr (TREE_OPERAND (expr, 0)),
		   (TREE_OPERAND (expr, 1)
		    ? build_non_dependent_expr (TREE_OPERAND (expr, 1))
		    : build_non_dependent_expr (TREE_OPERAND (expr, 0))),
		   build_non_dependent_expr (TREE_OPERAND (expr, 2)));
  if (TREE_CODE (expr) == COMPOUND_EXPR)
    return build2 (COMPOUND_EXPR,
		   TREE_TYPE (expr),
		   TREE_OPERAND (expr, 0),
		   build_non_dependent_expr (TREE_OPERAND (expr, 1)));

  /* If the type is unknown, it can't really be non-dependent */
  gcc_assert (TREE_TYPE (expr) != unknown_type_node);

  /* Otherwise, build a NON_DEPENDENT_EXPR.  */
  return build1_loc (EXPR_LOCATION (orig_expr), NON_DEPENDENT_EXPR,
		     TREE_TYPE (expr), expr);
}

/* ARGS is a vector of expressions as arguments to a function call.
   Replace the arguments with equivalent non-dependent expressions.
   This modifies ARGS in place.  */

void
make_args_non_dependent (vec<tree, va_gc> *args)
{
  unsigned int ix;
  tree arg;

  FOR_EACH_VEC_SAFE_ELT (args, ix, arg)
    {
      tree newarg = build_non_dependent_expr (arg);
      if (newarg != arg)
	(*args)[ix] = newarg;
    }
}

/* Returns a type which represents 'auto' or 'decltype(auto)'.  We use a
   TEMPLATE_TYPE_PARM with a level one deeper than the actual template parms,
   by default.  If set_canonical is true, we set TYPE_CANONICAL on it.  */

static tree
make_auto_1 (tree name, bool set_canonical, int level = -1)
{
  if (level == -1)
    level = current_template_depth + 1;
  tree au = cxx_make_type (TEMPLATE_TYPE_PARM);
  TYPE_NAME (au) = build_decl (input_location, TYPE_DECL, name, au);
  TYPE_STUB_DECL (au) = TYPE_NAME (au);
  TEMPLATE_TYPE_PARM_INDEX (au) = build_template_parm_index
    (0, level, level, TYPE_NAME (au), NULL_TREE);
  if (set_canonical)
    TYPE_CANONICAL (au) = canonical_type_parameter (au);
  DECL_ARTIFICIAL (TYPE_NAME (au)) = 1;
  SET_DECL_TEMPLATE_PARM_P (TYPE_NAME (au));
  if (name == decltype_auto_identifier)
    AUTO_IS_DECLTYPE (au) = true;

  return au;
}

tree
make_decltype_auto (void)
{
  return make_auto_1 (decltype_auto_identifier, true);
}

tree
make_auto (void)
{
  return make_auto_1 (auto_identifier, true);
}

/* Return a C++17 deduction placeholder for class template TMPL.
   There are represented as an 'auto' with the special level 0 and
   CLASS_PLACEHOLDER_TEMPLATE set.  */

tree
make_template_placeholder (tree tmpl)
{
  tree t = make_auto_1 (auto_identifier, false, /*level=*/0);
  CLASS_PLACEHOLDER_TEMPLATE (t) = tmpl;
  /* Our canonical type depends on the placeholder.  */
  TYPE_CANONICAL (t) = canonical_type_parameter (t);
  return t;
}

/* True iff T is a C++17 class template deduction placeholder.  */

bool
template_placeholder_p (tree t)
{
  return is_auto (t) && CLASS_PLACEHOLDER_TEMPLATE (t);
}

/* Make a "constrained auto" type-specifier. This is an auto or
  decltype(auto) type with constraints that must be associated after
  deduction.  The constraint is formed from the given concept CON
  and its optional sequence of template arguments ARGS.

  TYPE must be the result of make_auto_type or make_decltype_auto_type. */

static tree
make_constrained_placeholder_type (tree type, tree con, tree args)
{
  /* Build the constraint. */
  tree tmpl = DECL_TI_TEMPLATE (con);
  tree expr = tmpl;
  if (TREE_CODE (con) == FUNCTION_DECL)
    expr = ovl_make (tmpl);
  ++processing_template_decl;
  expr = build_concept_check (expr, type, args, tf_warning_or_error);
  --processing_template_decl;

  PLACEHOLDER_TYPE_CONSTRAINTS_INFO (type)
    = build_tree_list (current_template_parms, expr);

  /* Our canonical type depends on the constraint.  */
  TYPE_CANONICAL (type) = canonical_type_parameter (type);

  /* Attach the constraint to the type declaration. */
  return TYPE_NAME (type);
}

/* Make a "constrained auto" type-specifier.  */

tree
make_constrained_auto (tree con, tree args)
{
  tree type = make_auto_1 (auto_identifier, false);
  return make_constrained_placeholder_type (type, con, args);
}

/* Make a "constrained decltype(auto)" type-specifier.  */

tree
make_constrained_decltype_auto (tree con, tree args)
{
  tree type = make_auto_1 (decltype_auto_identifier, false);
  return make_constrained_placeholder_type (type, con, args);
}

/* Returns true if the placeholder type constraint T has any dependent
   (explicit) template arguments.  */

static bool
placeholder_type_constraint_dependent_p (tree t)
{
  tree id = unpack_concept_check (t);
  tree args = TREE_OPERAND (id, 1);
  tree first = TREE_VEC_ELT (args, 0);
  if (ARGUMENT_PACK_P (first))
    {
      args = expand_template_argument_pack (args);
      first = TREE_VEC_ELT (args, 0);
    }
  gcc_checking_assert (TREE_CODE (first) == WILDCARD_DECL
		       || is_auto (first));
  for (int i = 1; i < TREE_VEC_LENGTH (args); ++i)
    if (dependent_template_arg_p (TREE_VEC_ELT (args, i)))
      return true;
  return false;
}

/* Build and return a concept definition. Like other templates, the
   CONCEPT_DECL node is wrapped by a TEMPLATE_DECL.  This returns the
   the TEMPLATE_DECL. */

tree
finish_concept_definition (cp_expr id, tree init, tree attrs)
{
  gcc_assert (identifier_p (id));
  gcc_assert (processing_template_decl);

  location_t loc = id.get_location();

  /* A concept-definition shall not have associated constraints.  */
  if (TEMPLATE_PARMS_CONSTRAINTS (current_template_parms))
    {
      error_at (loc, "a concept cannot be constrained");
      TEMPLATE_PARMS_CONSTRAINTS (current_template_parms) = NULL_TREE;
    }

  /* A concept-definition shall appear in namespace scope.  Templates
     aren't allowed in block scope, so we only need to check for class
     scope.  */
  if (TYPE_P (current_scope()) || !DECL_NAMESPACE_SCOPE_P (current_scope ()))
    {
      error_at (loc, "concept %qE not in namespace scope", *id);
      return error_mark_node;
    }

  if (current_template_depth > 1)
    {
      error_at (loc, "concept %qE has multiple template parameter lists", *id);
      return error_mark_node;
    }

  /* Initially build the concept declaration; its type is bool.  */
  tree decl = build_lang_decl_loc (loc, CONCEPT_DECL, *id, boolean_type_node);
  DECL_CONTEXT (decl) = current_scope ();
  DECL_INITIAL (decl) = init;

  if (attrs)
    cplus_decl_attributes (&decl, attrs, 0);

  set_originating_module (decl, false);

  /* Push the enclosing template.  */
  return push_template_decl (decl);
}

/* Given type ARG, return std::initializer_list<ARG>.  */

static tree
listify (tree arg)
{
  tree std_init_list = lookup_qualified_name (std_node, init_list_identifier);

  if (std_init_list == error_mark_node
      || !DECL_CLASS_TEMPLATE_P (std_init_list))
    {
      gcc_rich_location richloc (input_location);
      maybe_add_include_fixit (&richloc, "<initializer_list>", false);
      error_at (&richloc,
		"deducing from brace-enclosed initializer list"
		" requires %<#include <initializer_list>%>");

      return error_mark_node;
    }
  tree argvec = make_tree_vec (1);
  TREE_VEC_ELT (argvec, 0) = arg;

  return lookup_template_class (std_init_list, argvec, NULL_TREE,
				NULL_TREE, 0, tf_warning_or_error);
}

/* Replace auto in TYPE with std::initializer_list<auto>.  */

static tree
listify_autos (tree type, tree auto_node)
{
  tree init_auto = listify (strip_top_quals (auto_node));
  tree argvec = make_tree_vec (1);
  TREE_VEC_ELT (argvec, 0) = init_auto;
  if (processing_template_decl)
    argvec = add_to_template_args (current_template_args (), argvec);
  return tsubst (type, argvec, tf_warning_or_error, NULL_TREE);
}

/* Hash traits for hashing possibly constrained 'auto'
   TEMPLATE_TYPE_PARMs for use by do_auto_deduction.  */

struct auto_hash : default_hash_traits<tree>
{
  static inline hashval_t hash (tree);
  static inline bool equal (tree, tree);
};

/* Hash the 'auto' T.  */

inline hashval_t
auto_hash::hash (tree t)
{
  if (tree c = NON_ERROR (PLACEHOLDER_TYPE_CONSTRAINTS (t)))
    /* Matching constrained-type-specifiers denote the same template
       parameter, so hash the constraint.  */
    return hash_placeholder_constraint (c);
  else
    /* But unconstrained autos are all separate, so just hash the pointer.  */
    return iterative_hash_object (t, 0);
}

/* Compare two 'auto's.  */

inline bool
auto_hash::equal (tree t1, tree t2)
{
  if (t1 == t2)
    return true;

  tree c1 = PLACEHOLDER_TYPE_CONSTRAINTS (t1);
  tree c2 = PLACEHOLDER_TYPE_CONSTRAINTS (t2);

  /* Two unconstrained autos are distinct.  */
  if (!c1 || !c2)
    return false;

  return equivalent_placeholder_constraints (c1, c2);
}

/* for_each_template_parm callback for extract_autos: if t is a (possibly
   constrained) auto, add it to the vector.  */

static int
extract_autos_r (tree t, void *data)
{
  hash_table<auto_hash> &hash = *(hash_table<auto_hash>*)data;
  if (is_auto (t) && !template_placeholder_p (t))
    {
      /* All the autos were built with index 0; fix that up now.  */
      tree *p = hash.find_slot (t, INSERT);
      int idx;
      if (*p)
	/* If this is a repeated constrained-type-specifier, use the index we
	   chose before.  */
	idx = TEMPLATE_TYPE_IDX (*p);
      else
	{
	  /* Otherwise this is new, so use the current count.  */
	  *p = t;
	  idx = hash.elements () - 1;
	}
      if (idx != TEMPLATE_TYPE_IDX (t))
	{
	  gcc_checking_assert (TEMPLATE_TYPE_IDX (t) == 0);
	  gcc_checking_assert (TYPE_CANONICAL (t) != t);
	  TEMPLATE_TYPE_IDX (t) = idx;
	  TYPE_CANONICAL (t) = canonical_type_parameter (t);
	}
    }

  /* Always keep walking.  */
  return 0;
}

/* Return a TREE_VEC of the 'auto's used in type under the Concepts TS, which
   says they can appear anywhere in the type.  */

static tree
extract_autos (tree type)
{
  hash_set<tree> visited;
  hash_table<auto_hash> hash (2);

  for_each_template_parm (type, extract_autos_r, &hash, &visited, true);

  tree tree_vec = make_tree_vec (hash.elements());
  for (tree elt : hash)
    {
      unsigned i = TEMPLATE_PARM_IDX (TEMPLATE_TYPE_PARM_INDEX (elt));
      TREE_VEC_ELT (tree_vec, i)
	= build_tree_list (NULL_TREE, TYPE_NAME (elt));
    }

  return tree_vec;
}

/* The stem for deduction guide names.  */
const char *const dguide_base = "__dguide_";

/* Return the name for a deduction guide for class template TMPL.  */

tree
dguide_name (tree tmpl)
{
  tree type = (TYPE_P (tmpl) ? tmpl : TREE_TYPE (tmpl));
  tree tname = TYPE_IDENTIFIER (type);
  char *buf = (char *) alloca (1 + strlen (dguide_base)
			       + IDENTIFIER_LENGTH (tname));
  memcpy (buf, dguide_base, strlen (dguide_base));
  memcpy (buf + strlen (dguide_base), IDENTIFIER_POINTER (tname),
	  IDENTIFIER_LENGTH (tname) + 1);
  tree dname = get_identifier (buf);
  TREE_TYPE (dname) = type;
  return dname;
}

/* True if NAME is the name of a deduction guide.  */

bool
dguide_name_p (tree name)
{
  return (TREE_CODE (name) == IDENTIFIER_NODE
	  && TREE_TYPE (name)
	  && startswith (IDENTIFIER_POINTER (name), dguide_base));
}

/* True if FN is a deduction guide.  */

bool
deduction_guide_p (const_tree fn)
{
  if (DECL_P (fn))
    if (tree name = DECL_NAME (fn))
      return dguide_name_p (name);
  return false;
}

/* True if FN is the copy deduction guide, i.e. A(A)->A.  */

bool
copy_guide_p (const_tree fn)
{
  gcc_assert (deduction_guide_p (fn));
  if (!DECL_ARTIFICIAL (fn))
    return false;
  tree parms = FUNCTION_FIRST_USER_PARMTYPE (DECL_TI_TEMPLATE (fn));
  return (TREE_CHAIN (parms) == void_list_node
	  && same_type_p (TREE_VALUE (parms), TREE_TYPE (DECL_NAME (fn))));
}

/* True if FN is a guide generated from a constructor template.  */

bool
template_guide_p (const_tree fn)
{
  gcc_assert (deduction_guide_p (fn));
  if (!DECL_ARTIFICIAL (fn))
    return false;
  tree tmpl = DECL_TI_TEMPLATE (fn);
  if (tree org = DECL_ABSTRACT_ORIGIN (tmpl))
    return PRIMARY_TEMPLATE_P (org);
  return false;
}

/* True if FN is an aggregate initialization guide or the copy deduction
   guide.  */

bool
builtin_guide_p (const_tree fn)
{
  if (!deduction_guide_p (fn))
    return false;
  if (!DECL_ARTIFICIAL (fn))
    /* Explicitly declared.  */
    return false;
  if (DECL_ABSTRACT_ORIGIN (fn))
    /* Derived from a constructor.  */
    return false;
  return true;
}

/* OLDDECL is a _DECL for a template parameter.  Return a similar parameter at
   LEVEL:INDEX, using tsubst_args and complain for substitution into non-type
   template parameter types.  Note that the handling of template template
   parameters relies on current_template_parms being set appropriately for the
   new template.  */

static tree
rewrite_template_parm (tree olddecl, unsigned index, unsigned level,
		       tree tsubst_args, tsubst_flags_t complain)
{
  if (olddecl == error_mark_node)
    return error_mark_node;

  tree oldidx = get_template_parm_index (olddecl);

  tree newtype;
  if (TREE_CODE (olddecl) == TYPE_DECL
      || TREE_CODE (olddecl) == TEMPLATE_DECL)
    {
      tree oldtype = TREE_TYPE (olddecl);
      newtype = cxx_make_type (TREE_CODE (oldtype));
      TYPE_MAIN_VARIANT (newtype) = newtype;
    }
  else
    {
      newtype = TREE_TYPE (olddecl);
      if (type_uses_auto (newtype))
	{
	  // Substitute once to fix references to other template parameters.
	  newtype = tsubst (newtype, tsubst_args,
			    complain|tf_partial, NULL_TREE);
	  // Now substitute again to reduce the level of the auto.
	  newtype = tsubst (newtype, current_template_args (),
			    complain, NULL_TREE);
	}
      else
	newtype = tsubst (newtype, tsubst_args,
			  complain, NULL_TREE);
    }

  tree newdecl
    = build_decl (DECL_SOURCE_LOCATION (olddecl), TREE_CODE (olddecl),
		  DECL_NAME (olddecl), newtype);
  SET_DECL_TEMPLATE_PARM_P (newdecl);

  tree newidx;
  if (TREE_CODE (olddecl) == TYPE_DECL
      || TREE_CODE (olddecl) == TEMPLATE_DECL)
    {
      newidx = TEMPLATE_TYPE_PARM_INDEX (newtype)
	= build_template_parm_index (index, level, level,
				     newdecl, newtype);
      TEMPLATE_PARM_PARAMETER_PACK (newidx)
	= TEMPLATE_PARM_PARAMETER_PACK (oldidx);
      TYPE_STUB_DECL (newtype) = TYPE_NAME (newtype) = newdecl;

      if (TREE_CODE (olddecl) == TEMPLATE_DECL)
	{
	  tree newresult
	    = build_lang_decl_loc (DECL_SOURCE_LOCATION (olddecl), TYPE_DECL,
				   DECL_NAME (olddecl), newtype);
	  DECL_ARTIFICIAL (newresult) = true;
	  DECL_TEMPLATE_RESULT (newdecl) = newresult;
	  // First create a copy (ttargs) of tsubst_args with an
	  // additional level for the template template parameter's own
	  // template parameters (ttparms).
	  tree ttparms = (INNERMOST_TEMPLATE_PARMS
			  (DECL_TEMPLATE_PARMS (olddecl)));
	  const int depth = TMPL_ARGS_DEPTH (tsubst_args);
	  tree ttargs = make_tree_vec (depth + 1);
	  for (int i = 0; i < depth; ++i)
	    TREE_VEC_ELT (ttargs, i) = TMPL_ARGS_LEVEL (tsubst_args, i + 1);
	  TREE_VEC_ELT (ttargs, depth)
	    = template_parms_level_to_args (ttparms);
	  // Substitute ttargs into ttparms to fix references to
	  // other template parameters.
	  ttparms = tsubst_template_parms_level (ttparms, ttargs,
						 complain|tf_partial);
	  // Now substitute again with args based on tparms, to reduce
	  // the level of the ttparms.
	  ttargs = current_template_args ();
	  ttparms = tsubst_template_parms_level (ttparms, ttargs,
						 complain);
	  // Finally, tack the adjusted parms onto tparms.
	  ttparms = tree_cons (size_int (level + 1), ttparms,
			       copy_node (current_template_parms));
	  // As with all template template parms, the parameter list captured
	  // by this template template parm that corresponds to its own level
	  // should be empty.  This avoids infinite recursion when structurally
	  // comparing two such rewritten template template parms (PR102479).
	  gcc_assert (!TREE_VEC_LENGTH
		      (TREE_VALUE (TREE_CHAIN (DECL_TEMPLATE_PARMS (olddecl)))));
	  gcc_assert (TMPL_PARMS_DEPTH (TREE_CHAIN (ttparms)) == level);
	  TREE_VALUE (TREE_CHAIN (ttparms)) = make_tree_vec (0);
	  // All done.
	  DECL_TEMPLATE_PARMS (newdecl) = ttparms;
	  DECL_TEMPLATE_INFO (newresult)
	    = build_template_info (newdecl, template_parms_to_args (ttparms));
	}

      if (TYPE_STRUCTURAL_EQUALITY_P (TREE_TYPE (olddecl)))
	SET_TYPE_STRUCTURAL_EQUALITY (newtype);
      else
	TYPE_CANONICAL (newtype) = canonical_type_parameter (newtype);
    }
  else
    {
      tree oldconst = TEMPLATE_PARM_DECL (oldidx);
      tree newconst
	= build_decl (DECL_SOURCE_LOCATION (oldconst),
		      TREE_CODE (oldconst),
		      DECL_NAME (oldconst), newtype);
      TREE_CONSTANT (newconst) = TREE_CONSTANT (newdecl)
	= TREE_READONLY (newconst) = TREE_READONLY (newdecl) = true;
      SET_DECL_TEMPLATE_PARM_P (newconst);
      newidx = build_template_parm_index (index, level, level,
					  newconst, newtype);
      TEMPLATE_PARM_PARAMETER_PACK (newidx)
	= TEMPLATE_PARM_PARAMETER_PACK (oldidx);
      DECL_INITIAL (newdecl) = DECL_INITIAL (newconst) = newidx;
    }

  return newdecl;
}

/* As rewrite_template_parm, but for the whole TREE_LIST representing a
   template parameter.  */

static tree
rewrite_tparm_list (tree oldelt, unsigned index, unsigned level,
		    tree targs, unsigned targs_index, tsubst_flags_t complain)
{
  tree olddecl = TREE_VALUE (oldelt);
  tree newdecl = rewrite_template_parm (olddecl, index, level,
					targs, complain);
  if (newdecl == error_mark_node)
    return error_mark_node;
  tree newdef = tsubst_template_arg (TREE_PURPOSE (oldelt),
				     targs, complain, NULL_TREE);
  tree list = build_tree_list (newdef, newdecl);
  TEMPLATE_PARM_CONSTRAINTS (list)
    = tsubst_constraint_info (TEMPLATE_PARM_CONSTRAINTS (oldelt),
			      targs, complain, NULL_TREE);
  int depth = TMPL_ARGS_DEPTH (targs);
  TMPL_ARG (targs, depth, targs_index) = template_parm_to_arg (list);
  return list;
}

/* Returns a C++17 class deduction guide template based on the constructor
   CTOR.  As a special case, CTOR can be a RECORD_TYPE for an implicit default
   guide, REFERENCE_TYPE for an implicit copy/move guide, or TREE_LIST for an
   aggregate initialization guide.  OUTER_ARGS are the template arguments
   for the enclosing scope of the class.  */

static tree
build_deduction_guide (tree type, tree ctor, tree outer_args, tsubst_flags_t complain)
{
  tree tparms, targs, fparms, fargs, ci;
  bool memtmpl = false;
  bool explicit_p;
  location_t loc;
  tree fn_tmpl = NULL_TREE;

  if (outer_args)
    {
      ++processing_template_decl;
      type = tsubst (type, outer_args, complain, CLASSTYPE_TI_TEMPLATE (type));
      --processing_template_decl;
    }

  if (!DECL_DECLARES_FUNCTION_P (ctor))
    {
      if (TYPE_P (ctor))
	{
	  bool copy_p = TYPE_REF_P (ctor);
	  if (copy_p)
	    fparms = tree_cons (NULL_TREE, type, void_list_node);
	  else
	    fparms = void_list_node;
	}
      else if (TREE_CODE (ctor) == TREE_LIST)
	fparms = ctor;
      else
	gcc_unreachable ();

      tree ctmpl = CLASSTYPE_TI_TEMPLATE (type);
      tparms = DECL_TEMPLATE_PARMS (ctmpl);
      targs = INNERMOST_TEMPLATE_ARGS (CLASSTYPE_TI_ARGS (type));
      ci = NULL_TREE;
      fargs = NULL_TREE;
      loc = DECL_SOURCE_LOCATION (ctmpl);
      explicit_p = false;
    }
  else
    {
      ++processing_template_decl;
      bool ok = true;

      complain |= tf_dguide;

      fn_tmpl
	= (TREE_CODE (ctor) == TEMPLATE_DECL ? ctor
	   : DECL_TI_TEMPLATE (ctor));
      if (outer_args)
	fn_tmpl = tsubst (fn_tmpl, outer_args, complain, ctor);
      ctor = DECL_TEMPLATE_RESULT (fn_tmpl);

      tparms = DECL_TEMPLATE_PARMS (fn_tmpl);
      /* If type is a member class template, DECL_TI_ARGS (ctor) will have
	 fully specialized args for the enclosing class.  Strip those off, as
	 the deduction guide won't have those template parameters.  */
      targs = get_innermost_template_args (DECL_TI_ARGS (ctor),
						TMPL_PARMS_DEPTH (tparms));
      /* Discard the 'this' parameter.  */
      fparms = FUNCTION_ARG_CHAIN (ctor);
      fargs = TREE_CHAIN (DECL_ARGUMENTS (ctor));
      ci = get_constraints (ctor);
      loc = DECL_SOURCE_LOCATION (ctor);
      explicit_p = DECL_NONCONVERTING_P (ctor);

      if (PRIMARY_TEMPLATE_P (fn_tmpl))
	{
	  memtmpl = true;

	  /* For a member template constructor, we need to flatten the two
	     template parameter lists into one, and then adjust the function
	     signature accordingly.  This gets...complicated.  */
	  tree save_parms = current_template_parms;

	  /* For a member template we should have two levels of parms/args, one
	     for the class and one for the constructor.  We stripped
	     specialized args for further enclosing classes above.  */
	  const int depth = 2;
	  gcc_assert (TMPL_ARGS_DEPTH (targs) == depth);

	  /* Template args for translating references to the two-level template
	     parameters into references to the one-level template parameters we
	     are creating.  */
	  tree tsubst_args = copy_node (targs);
	  TMPL_ARGS_LEVEL (tsubst_args, depth)
	    = copy_node (TMPL_ARGS_LEVEL (tsubst_args, depth));

	  /* Template parms for the constructor template.  */
	  tree ftparms = TREE_VALUE (tparms);
	  unsigned flen = TREE_VEC_LENGTH (ftparms);
	  /* Template parms for the class template.  */
	  tparms = TREE_CHAIN (tparms);
	  tree ctparms = TREE_VALUE (tparms);
	  unsigned clen = TREE_VEC_LENGTH (ctparms);
	  /* Template parms for the deduction guide start as a copy of the
	     template parms for the class.  We set current_template_parms for
	     lookup_template_class_1.  */
	  current_template_parms = tparms = copy_node (tparms);
	  tree new_vec = TREE_VALUE (tparms) = make_tree_vec (flen + clen);
	  for (unsigned i = 0; i < clen; ++i)
	    TREE_VEC_ELT (new_vec, i) = TREE_VEC_ELT (ctparms, i);

	  /* Now we need to rewrite the constructor parms to append them to the
	     class parms.  */
	  for (unsigned i = 0; i < flen; ++i)
	    {
	      unsigned index = i + clen;
	      unsigned level = 1;
	      tree oldelt = TREE_VEC_ELT (ftparms, i);
	      tree newelt
		= rewrite_tparm_list (oldelt, index, level,
				      tsubst_args, i, complain);
	      if (newelt == error_mark_node)
		ok = false;
	      TREE_VEC_ELT (new_vec, index) = newelt;
	    }

	  /* Now we have a final set of template parms to substitute into the
	     function signature.  */
	  targs = template_parms_to_args (tparms);
	  fparms = tsubst_arg_types (fparms, tsubst_args, NULL_TREE,
				     complain, ctor);
	  if (fparms == error_mark_node)
	    ok = false;
	  if (ci)
	    {
	      if (outer_args)
		/* FIXME: We'd like to avoid substituting outer template
		   arguments into the constraint ahead of time, but the
		   construction of tsubst_args assumes that outer arguments
		   are already substituted in.  */
		ci = tsubst_constraint_info (ci, outer_args, complain, ctor);
	      ci = tsubst_constraint_info (ci, tsubst_args, complain, ctor);
	    }

	  /* Parms are to have DECL_CHAIN tsubsted, which would be skipped if
	     cp_unevaluated_operand.  */
	  cp_evaluated ev;
	  fargs = tsubst (fargs, tsubst_args, complain, ctor);
	  current_template_parms = save_parms;
	}
      else
	{
	  /* Substitute in the same arguments to rewrite class members into
	     references to members of an unknown specialization.  */
	  cp_evaluated ev;
	  fparms = tsubst_arg_types (fparms, targs, NULL_TREE, complain, ctor);
	  fargs = tsubst (fargs, targs, complain, ctor);
	  if (ci)
	    {
	      if (outer_args)
		/* FIXME: As above.  */
		ci = tsubst_constraint_info (ci, outer_args, complain, ctor);
	      ci = tsubst_constraint_info (ci, targs, complain, ctor);
	    }
	}

      --processing_template_decl;
      if (!ok)
	return error_mark_node;
    }

  if (!memtmpl)
    {
      /* Copy the parms so we can set DECL_PRIMARY_TEMPLATE.  */
      tparms = copy_node (tparms);
      INNERMOST_TEMPLATE_PARMS (tparms)
	= copy_node (INNERMOST_TEMPLATE_PARMS (tparms));
    }

  tree fntype = build_function_type (type, fparms);
  tree ded_fn = build_lang_decl_loc (loc,
				     FUNCTION_DECL,
				     dguide_name (type), fntype);
  DECL_ARGUMENTS (ded_fn) = fargs;
  DECL_ARTIFICIAL (ded_fn) = true;
  DECL_NONCONVERTING_P (ded_fn) = explicit_p;
  tree ded_tmpl = build_template_decl (ded_fn, tparms, /*member*/false);
  DECL_ARTIFICIAL (ded_tmpl) = true;
  DECL_TEMPLATE_INFO (ded_fn) = build_template_info (ded_tmpl, targs);
  DECL_PRIMARY_TEMPLATE (ded_tmpl) = ded_tmpl;
  if (DECL_P (ctor))
    DECL_ABSTRACT_ORIGIN (ded_tmpl) = fn_tmpl;
  if (ci)
    set_constraints (ded_tmpl, ci);

  return ded_tmpl;
}

/* Add to LIST the member types for the reshaped initializer CTOR.  */

static tree
collect_ctor_idx_types (tree ctor, tree list, tree elt = NULL_TREE)
{
  vec<constructor_elt, va_gc> *v = CONSTRUCTOR_ELTS (ctor);
  tree idx, val; unsigned i;
  FOR_EACH_CONSTRUCTOR_ELT (v, i, idx, val)
    {
      tree ftype = elt ? elt : TREE_TYPE (idx);
      if (BRACE_ENCLOSED_INITIALIZER_P (val)
	  && CONSTRUCTOR_BRACES_ELIDED_P (val))
	{
	  tree subelt = NULL_TREE;
	  if (TREE_CODE (ftype) == ARRAY_TYPE)
	    subelt = TREE_TYPE (ftype);
	  list = collect_ctor_idx_types (val, list, subelt);
	  continue;
	}
      tree arg = NULL_TREE;
      if (i == v->length() - 1
	  && PACK_EXPANSION_P (ftype))
	/* Give the trailing pack expansion parameter a default argument to
	   match aggregate initialization behavior, even if we deduce the
	   length of the pack separately to more than we have initializers. */
	arg = build_constructor (init_list_type_node, NULL);
      /* if ei is of array type and xi is a braced-init-list or string literal,
	 Ti is an rvalue reference to the declared type of ei */
      STRIP_ANY_LOCATION_WRAPPER (val);
      if (TREE_CODE (ftype) == ARRAY_TYPE
	  && (BRACE_ENCLOSED_INITIALIZER_P (val)
	      || TREE_CODE (val) == STRING_CST))
	{
	  if (TREE_CODE (val) == STRING_CST)
	    ftype = cp_build_qualified_type
	      (ftype, cp_type_quals (ftype) | TYPE_QUAL_CONST);
	  ftype = (cp_build_reference_type
		   (ftype, BRACE_ENCLOSED_INITIALIZER_P (val)));
	}
      list = tree_cons (arg, ftype, list);
    }

  return list;
}

/* Return whether ETYPE is, or is derived from, a specialization of TMPL.  */

static bool
is_spec_or_derived (tree etype, tree tmpl)
{
  if (!etype || !CLASS_TYPE_P (etype))
    return false;

  etype = cv_unqualified (etype);
  tree type = TREE_TYPE (tmpl);
  tree tparms = (INNERMOST_TEMPLATE_PARMS
		 (DECL_TEMPLATE_PARMS (tmpl)));
  tree targs = make_tree_vec (TREE_VEC_LENGTH (tparms));
  int err = unify (tparms, targs, type, etype,
		   UNIFY_ALLOW_DERIVED, /*explain*/false);
  ggc_free (targs);
  return !err;
}

static tree alias_ctad_tweaks (tree, tree);

/* Return a C++20 aggregate deduction candidate for TYPE initialized from
   INIT.  */

static tree
maybe_aggr_guide (tree tmpl, tree init, vec<tree,va_gc> *args)
{
  if (cxx_dialect < cxx20)
    return NULL_TREE;

  if (init == NULL_TREE)
    return NULL_TREE;

  if (DECL_ALIAS_TEMPLATE_P (tmpl))
    {
      tree under = DECL_ORIGINAL_TYPE (DECL_TEMPLATE_RESULT (tmpl));
      tree tinfo = get_template_info (under);
      if (tree guide = maybe_aggr_guide (TI_TEMPLATE (tinfo), init, args))
	return alias_ctad_tweaks (tmpl, guide);
      return NULL_TREE;
    }

  /* We might be creating a guide for a class member template, e.g.,

       template<typename U> struct A {
	 template<typename T> struct B { T t; };
       };

     At this point, A will have been instantiated.  Below, we need to
     use both A<U>::B<T> (TEMPLATE_TYPE) and A<int>::B<T> (TYPE) types.  */
  const bool member_template_p
    = (DECL_TEMPLATE_INFO (tmpl)
       && DECL_MEMBER_TEMPLATE_P (DECL_TI_TEMPLATE (tmpl)));
  tree type = TREE_TYPE (tmpl);
  tree template_type = (member_template_p
			? TREE_TYPE (DECL_TI_TEMPLATE (tmpl))
			: type);
  if (!CP_AGGREGATE_TYPE_P (template_type))
    return NULL_TREE;

  /* No aggregate candidate for copy-initialization.  */
  if (args->length() == 1)
    {
      tree val = (*args)[0];
      if (is_spec_or_derived (TREE_TYPE (val), tmpl))
	return NULL_TREE;
    }

  /* If we encounter a problem, we just won't add the candidate.  */
  tsubst_flags_t complain = tf_none;

  tree parms = NULL_TREE;
  if (BRACE_ENCLOSED_INITIALIZER_P (init))
    {
      init = reshape_init (template_type, init, complain);
      if (init == error_mark_node)
	return NULL_TREE;
      parms = collect_ctor_idx_types (init, parms);
      /* If we're creating a deduction guide for a member class template,
	 we've used the original template pattern type for the reshape_init
	 above; this is done because we want PARMS to be a template parameter
	 type, something that can be deduced when used as a function template
	 parameter.  At this point the outer class template has already been
	 partially instantiated (we deferred the deduction until the enclosing
	 scope is non-dependent).  Therefore we have to partially instantiate
	 PARMS, so that its template level is properly reduced and we don't get
	 mismatches when deducing types using the guide with PARMS.  */
      if (member_template_p)
	{
	  ++processing_template_decl;
	  parms = tsubst (parms, DECL_TI_ARGS (tmpl), complain, init);
	  --processing_template_decl;
	}
    }
  else if (TREE_CODE (init) == TREE_LIST)
    {
      int len = list_length (init);
      for (tree field = TYPE_FIELDS (type);
	   len;
	   --len, field = DECL_CHAIN (field))
	{
	  field = next_aggregate_field (field);
	  if (!field)
	    return NULL_TREE;
	  tree ftype = finish_decltype_type (field, true, complain);
	  parms = tree_cons (NULL_TREE, ftype, parms);
	}
    }
  else
    /* Aggregate initialization doesn't apply to an initializer expression.  */
    return NULL_TREE;

  if (parms)
    {
      tree last = parms;
      parms = nreverse (parms);
      TREE_CHAIN (last) = void_list_node;
      tree guide = build_deduction_guide (type, parms, NULL_TREE, complain);
      return guide;
    }

  return NULL_TREE;
}

/* UGUIDES are the deduction guides for the underlying template of alias
   template TMPL; adjust them to be deduction guides for TMPL.  */

static tree
alias_ctad_tweaks (tree tmpl, tree uguides)
{
  /* [over.match.class.deduct]: When resolving a placeholder for a deduced
     class type (9.2.8.2) where the template-name names an alias template A,
     the defining-type-id of A must be of the form

     typename(opt) nested-name-specifier(opt) template(opt) simple-template-id

     as specified in 9.2.8.2. The guides of A are the set of functions or
     function templates formed as follows. For each function or function
     template f in the guides of the template named by the simple-template-id
     of the defining-type-id, the template arguments of the return type of f
     are deduced from the defining-type-id of A according to the process in
     13.10.2.5 with the exception that deduction does not fail if not all
     template arguments are deduced. Let g denote the result of substituting
     these deductions into f. If substitution succeeds, form a function or
     function template f' with the following properties and add it to the set
     of guides of A:

     * The function type of f' is the function type of g.

     * If f is a function template, f' is a function template whose template
     parameter list consists of all the template parameters of A (including
     their default template arguments) that appear in the above deductions or
     (recursively) in their default template arguments, followed by the
     template parameters of f that were not deduced (including their default
     template arguments), otherwise f' is not a function template.

     * The associated constraints (13.5.2) are the conjunction of the
     associated constraints of g and a constraint that is satisfied if and only
     if the arguments of A are deducible (see below) from the return type.

     * If f is a copy deduction candidate (12.4.1.8), then f' is considered to
     be so as well.

     * If f was generated from a deduction-guide (12.4.1.8), then f' is
     considered to be so as well.

     * The explicit-specifier of f' is the explicit-specifier of g (if
     any).  */

  tsubst_flags_t complain = tf_warning_or_error;
  tree atype = TREE_TYPE (tmpl);
  tree aguides = NULL_TREE;
  tree fullatparms = DECL_TEMPLATE_PARMS (tmpl);
  tree atparms = INNERMOST_TEMPLATE_PARMS (fullatparms);
  unsigned natparms = TREE_VEC_LENGTH (atparms);
  tree utype = DECL_ORIGINAL_TYPE (DECL_TEMPLATE_RESULT (tmpl));
  for (ovl_iterator iter (uguides); iter; ++iter)
    {
      tree f = *iter;
      tree in_decl = f;
      location_t loc = DECL_SOURCE_LOCATION (f);
      tree ret = TREE_TYPE (TREE_TYPE (f));
      tree fprime = f;
      if (TREE_CODE (f) == TEMPLATE_DECL)
	{
	  processing_template_decl_sentinel ptds (/*reset*/false);
	  ++processing_template_decl;

	  /* Deduce template arguments for f from the type-id of A.  */
	  tree ftparms = INNERMOST_TEMPLATE_PARMS (DECL_TEMPLATE_PARMS (f));
	  unsigned len = TREE_VEC_LENGTH (ftparms);
	  tree targs = make_tree_vec (len);
	  int err = unify (ftparms, targs, ret, utype, UNIFY_ALLOW_NONE, false);
	  if (err)
	    /* CWG2664: Discard any deductions, still build the guide.  */
	    for (unsigned i = 0; i < len; ++i)
	      TREE_VEC_ELT (targs, i) = NULL_TREE;

	  /* The number of parms for f' is the number of parms of A used in
	     the deduced arguments plus non-deduced parms of f.  */
	  unsigned ndlen = 0;
	  unsigned j;
	  for (unsigned i = 0; i < len; ++i)
	    if (TREE_VEC_ELT (targs, i) == NULL_TREE)
	      ++ndlen;
	  find_template_parameter_info ftpi (fullatparms);
	  ftpi.find_in_recursive (targs);
	  unsigned nusedatparms = ftpi.num_found ();
	  unsigned nfparms = nusedatparms + ndlen;
	  tree gtparms = make_tree_vec (nfparms);

	  /* Set current_template_parms as in build_deduction_guide.  */
	  auto ctp = make_temp_override (current_template_parms);
	  current_template_parms = copy_node (DECL_TEMPLATE_PARMS (tmpl));
	  TREE_VALUE (current_template_parms) = gtparms;

	  j = 0;
	  unsigned level = 1;

	  /* First copy over the used parms of A.  */
	  tree atargs = make_tree_vec (natparms);
	  for (unsigned i = 0; i < natparms; ++i)
	    {
	      tree elt = TREE_VEC_ELT (atparms, i);
	      if (ftpi.found (elt))
		{
		  unsigned index = j++;
		  tree nelt = rewrite_tparm_list (elt, index, level,
						  atargs, i, complain);
		  TREE_VEC_ELT (gtparms, index) = nelt;
		}
	    }
	  gcc_checking_assert (j == nusedatparms);

	  /* Adjust the deduced template args for f to refer to the A parms
	     with their new indexes.  */
	  if (nusedatparms && nusedatparms != natparms)
	    targs = tsubst_template_args (targs, atargs, complain, in_decl);

	  /* Now rewrite the non-deduced parms of f.  */
	  for (unsigned i = 0; ndlen && i < len; ++i)
	    if (TREE_VEC_ELT (targs, i) == NULL_TREE)
	      {
		--ndlen;
		unsigned index = j++;
		tree oldlist = TREE_VEC_ELT (ftparms, i);
		tree list = rewrite_tparm_list (oldlist, index, level,
						targs, i, complain);
		TREE_VEC_ELT (gtparms, index) = list;
	      }
	  gtparms = build_tree_list (size_one_node, gtparms);

	  /* Substitute the deduced arguments plus the rewritten template
	     parameters into f to get g.  This covers the type, copyness,
	     guideness, and explicit-specifier.  */
	  tree g;
	    {
	      /* Parms are to have DECL_CHAIN tsubsted, which would be skipped
		 if cp_unevaluated_operand.  */
	      cp_evaluated ev;
	      g = tsubst_decl (DECL_TEMPLATE_RESULT (f), targs, complain);
	    }
	  if (g == error_mark_node)
	    continue;
	  if (nfparms == 0)
	    {
	      /* The targs are all non-dependent, so g isn't a template.  */
	      fprime = g;
	      ret = TREE_TYPE (TREE_TYPE (fprime));
	      goto non_template;
	    }
	  DECL_USE_TEMPLATE (g) = 0;
	  fprime = build_template_decl (g, gtparms, false);
	  DECL_TEMPLATE_RESULT (fprime) = g;
	  TREE_TYPE (fprime) = TREE_TYPE (g);
	  tree gtargs = template_parms_to_args (gtparms);
	  DECL_TEMPLATE_INFO (g) = build_template_info (fprime, gtargs);
	  DECL_PRIMARY_TEMPLATE (fprime) = fprime;

	  /* Substitute the associated constraints.  */
	  tree ci = get_constraints (f);
	  if (ci)
	    ci = tsubst_constraint_info (ci, targs, complain, in_decl);
	  if (ci == error_mark_node)
	    continue;

	  /* Add a constraint that the return type matches the instantiation of
	     A with the same template arguments.  */
	  ret = TREE_TYPE (TREE_TYPE (fprime));
	  if (!same_type_p (atype, ret)
	      /* FIXME this should mean they don't compare as equivalent.  */
	      || dependent_alias_template_spec_p (atype, nt_opaque))
	    {
	      tree same = finish_trait_expr (loc, CPTK_IS_DEDUCIBLE, tmpl, ret);
	      ci = append_constraint (ci, same);
	    }

	  if (ci)
	    {
	      remove_constraints (fprime);
	      set_constraints (fprime, ci);
	    }
	}
      else
	{
	  /* For a non-template deduction guide, if the arguments of A aren't
	     deducible from the return type, don't add the candidate.  */
	non_template:
	  if (!type_targs_deducible_from (tmpl, ret))
	    continue;
	}

      aguides = lookup_add (fprime, aguides);
    }

  return aguides;
}

/* True iff template arguments for TMPL can be deduced from TYPE.
   Used to implement CPTK_IS_DEDUCIBLE for alias CTAD according to
   [over.match.class.deduct].

   This check is specified in terms of partial specialization, so the behavior
   should be parallel to that of get_partial_spec_bindings.  */

bool
type_targs_deducible_from (tree tmpl, tree type)
{
  tree tparms = DECL_INNERMOST_TEMPLATE_PARMS (tmpl);
  int len = TREE_VEC_LENGTH (tparms);
  tree targs = make_tree_vec (len);
  bool tried_array_deduction = (cxx_dialect < cxx17);

  /* If tmpl is a class template, this is trivial: it's deducible if TYPE is a
     specialization of TMPL.  */
  if (DECL_CLASS_TEMPLATE_P (tmpl))
    return (CLASS_TYPE_P (type)
	    && CLASSTYPE_TEMPLATE_INFO (type)
	    && CLASSTYPE_TI_TEMPLATE (type) == tmpl);

  /* Otherwise it's an alias template.  */
 again:
  if (unify (tparms, targs, TREE_TYPE (tmpl), type,
	     UNIFY_ALLOW_NONE, false))
    return false;

  /* We don't fail on an undeduced targ the second time through (like
     get_partial_spec_bindings) because we're going to try defaults.  */
  for (int i =  0; i < len; ++i)
    if (! TREE_VEC_ELT (targs, i))
      {
	tree tparm = TREE_VEC_ELT (tparms, i);
	tparm = TREE_VALUE (tparm);

	if (!tried_array_deduction
	    && TREE_CODE (tparm) == TYPE_DECL)
	  {
	    try_array_deduction (tparms, targs, TREE_TYPE (tmpl));
	    tried_array_deduction = true;
	    if (TREE_VEC_ELT (targs, i))
	      goto again;
	  }
	/* If the type parameter is a parameter pack, then it will be deduced
	   to an empty parameter pack.  This is another case that doesn't model
	   well as partial specialization.  */
	if (template_parameter_pack_p (tparm))
	  {
	    tree arg;
	    if (TREE_CODE (tparm) == PARM_DECL)
	      {
		arg = make_node (NONTYPE_ARGUMENT_PACK);
		TREE_CONSTANT (arg) = 1;
	      }
	    else
	      arg = cxx_make_type (TYPE_ARGUMENT_PACK);
	    ARGUMENT_PACK_ARGS (arg) = make_tree_vec (0);
	    TREE_VEC_ELT (targs, i) = arg;
	  }
      }

  /* Maybe add in default template args.  This seems like a flaw in the
     specification in terms of partial specialization, since it says the
     partial specialization has the the template parameter list of A, but a
     partial specialization can't have default targs.  */
  targs = coerce_template_parms (tparms, targs, tmpl, tf_none);
  if (targs == error_mark_node)
    return false;

  /* I believe we don't need the template_template_parm_bindings_ok_p call
     because coerce_template_parms did coerce_template_template_parms.  */

  return constraints_satisfied_p (tmpl, targs);
}

/* Return artificial deduction guides built from the constructors of class
   template TMPL.  */

static tree
ctor_deduction_guides_for (tree tmpl, tsubst_flags_t complain)
{
  tree outer_args = outer_template_args (tmpl);
  tree type = TREE_TYPE (most_general_template (tmpl));

  tree cands = NULL_TREE;

  for (ovl_iterator iter (CLASSTYPE_CONSTRUCTORS (type)); iter; ++iter)
    {
      /* Skip inherited constructors.  */
      if (iter.using_p ())
	continue;

      tree guide = build_deduction_guide (type, *iter, outer_args, complain);
      cands = lookup_add (guide, cands);
    }

  /* Add implicit default constructor deduction guide.  */
  if (!TYPE_HAS_USER_CONSTRUCTOR (type))
    {
      tree guide = build_deduction_guide (type, type, outer_args,
					  complain);
      cands = lookup_add (guide, cands);
    }

  /* Add copy guide.  */
  {
    tree gtype = build_reference_type (type);
    tree guide = build_deduction_guide (type, gtype, outer_args,
					complain);
    cands = lookup_add (guide, cands);
  }

  return cands;
}

static GTY((deletable)) hash_map<tree, tree_pair_p> *dguide_cache;

/* Return the non-aggregate deduction guides for deducible template TMPL.  The
   aggregate candidate is added separately because it depends on the
   initializer.  Set ANY_DGUIDES_P if we find a non-implicit deduction
   guide.  */

static tree
deduction_guides_for (tree tmpl, bool &any_dguides_p, tsubst_flags_t complain)
{
  tree guides = NULL_TREE;
  if (DECL_ALIAS_TEMPLATE_P (tmpl))
    {
      tree under = DECL_ORIGINAL_TYPE (DECL_TEMPLATE_RESULT (tmpl));
      tree tinfo = get_template_info (under);
      guides = deduction_guides_for (TI_TEMPLATE (tinfo), any_dguides_p,
				     complain);
    }
  else
    {
      guides = lookup_qualified_name (CP_DECL_CONTEXT (tmpl),
				      dguide_name (tmpl),
				      LOOK_want::NORMAL, /*complain*/false);
      if (guides == error_mark_node)
	guides = NULL_TREE;
      else
	any_dguides_p = true;
    }

  /* Cache the deduction guides for a template.  We also remember the result of
     lookup, and rebuild everything if it changes; should be very rare.  */
  tree_pair_p cache = NULL;
  if (tree_pair_p &r
      = hash_map_safe_get_or_insert<hm_ggc> (dguide_cache, tmpl))
    {
      cache = r;
      if (cache->purpose == guides)
	return cache->value;
    }
  else
    {
      r = cache = ggc_cleared_alloc<tree_pair_s> ();
      cache->purpose = guides;
    }

  tree cands = NULL_TREE;
  if (DECL_ALIAS_TEMPLATE_P (tmpl))
    cands = alias_ctad_tweaks (tmpl, guides);
  else
    {
      cands = ctor_deduction_guides_for (tmpl, complain);
      for (ovl_iterator it (guides); it; ++it)
	cands = lookup_add (*it, cands);
    }

  cache->value = cands;
  return cands;
}

/* Return whether TMPL is a (class template argument-) deducible template.  */

bool
ctad_template_p (tree tmpl)
{
  /* A deducible template is either a class template or is an alias template
     whose defining-type-id is of the form

      typename(opt) nested-name-specifier(opt) template(opt) simple-template-id

     where the nested-name-specifier (if any) is non-dependent and the
     template-name of the simple-template-id names a deducible template.  */

  if (DECL_CLASS_TEMPLATE_P (tmpl)
      || DECL_TEMPLATE_TEMPLATE_PARM_P (tmpl))
    return true;
  if (!DECL_ALIAS_TEMPLATE_P (tmpl))
    return false;
  tree orig = DECL_ORIGINAL_TYPE (DECL_TEMPLATE_RESULT (tmpl));
  if (tree tinfo = get_template_info (orig))
    return ctad_template_p (TI_TEMPLATE (tinfo));
  return false;
}

/* Deduce template arguments for the class template placeholder PTYPE for
   template TMPL based on the initializer INIT, and return the resulting
   type.  */

static tree
do_class_deduction (tree ptype, tree tmpl, tree init,
		    int flags, tsubst_flags_t complain)
{
  /* We should have handled this in the caller.  */
  if (DECL_TEMPLATE_TEMPLATE_PARM_P (tmpl))
    return ptype;

  /* If the class was erroneous, don't try to deduce, because that
     can generate a lot of diagnostic.  */
  if (TREE_TYPE (tmpl)
      && TYPE_LANG_SPECIFIC (TREE_TYPE (tmpl))
      && CLASSTYPE_ERRONEOUS (TREE_TYPE (tmpl)))
    return ptype;

  /* Wait until the enclosing scope is non-dependent.  */
  if (DECL_CLASS_SCOPE_P (tmpl)
      && dependent_type_p (DECL_CONTEXT (tmpl)))
    return ptype;

  /* Initializing one placeholder from another.  */
  if (init
      && (TREE_CODE (init) == TEMPLATE_PARM_INDEX
	  || (TREE_CODE (init) == EXPR_PACK_EXPANSION
	      && (TREE_CODE (PACK_EXPANSION_PATTERN (init))
		  == TEMPLATE_PARM_INDEX)))
      && is_auto (TREE_TYPE (init))
      && CLASS_PLACEHOLDER_TEMPLATE (TREE_TYPE (init)) == tmpl)
    return cp_build_qualified_type (TREE_TYPE (init), cp_type_quals (ptype));

  if (!ctad_template_p (tmpl))
    {
      if (complain & tf_error)
	error ("non-deducible template %qT used without template arguments", tmpl);
      return error_mark_node;
    }
  else if (cxx_dialect < cxx20 && DECL_ALIAS_TEMPLATE_P (tmpl))
    {
      if (complain & tf_error)
	{
	  /* Be permissive with equivalent alias templates.  */
	  tree u = get_underlying_template (tmpl);
	  diagnostic_t dk = (u == tmpl) ? DK_ERROR : DK_PEDWARN;
	  bool complained
	    = emit_diagnostic (dk, input_location, 0,
			       "alias template deduction only available "
			       "with %<-std=c++20%> or %<-std=gnu++20%>");
	  if (u == tmpl)
	    return error_mark_node;
	  else if (complained)
	    {
	      inform (input_location, "use %qD directly instead", u);
	      tmpl = u;
	    }
	}
      else
	return error_mark_node;
    }

  /* Wait until the initializer is non-dependent.  */
  if (type_dependent_expression_p (init))
    return ptype;

  /* Don't bother with the alias rules for an equivalent template.  */
  tmpl = get_underlying_template (tmpl);

  tree type = TREE_TYPE (tmpl);

  bool try_list_cand = false;
  bool list_init_p = false;

  releasing_vec rv_args = NULL;
  vec<tree,va_gc> *&args = *&rv_args;
  if (init == NULL_TREE)
    args = make_tree_vector ();
  else if (BRACE_ENCLOSED_INITIALIZER_P (init))
    {
      list_init_p = true;
      try_list_cand = true;
      if (CONSTRUCTOR_NELTS (init) == 1
	  && !CONSTRUCTOR_IS_DESIGNATED_INIT (init))
	{
	  /* As an exception, the first phase in 16.3.1.7 (considering the
	     initializer list as a single argument) is omitted if the
	     initializer list consists of a single expression of type cv U,
	     where U is a specialization of C or a class derived from a
	     specialization of C.  */
	  tree elt = CONSTRUCTOR_ELT (init, 0)->value;
	  if (is_spec_or_derived (TREE_TYPE (elt), tmpl))
	    try_list_cand = false;
	}
      if (try_list_cand || is_std_init_list (type))
	args = make_tree_vector_single (init);
      else
	args = make_tree_vector_from_ctor (init);
    }
  else if (TREE_CODE (init) == TREE_LIST)
    args = make_tree_vector_from_list (init);
  else
    args = make_tree_vector_single (init);

  /* Do this now to avoid problems with erroneous args later on.  */
  args = resolve_args (args, complain);
  if (args == NULL)
    return error_mark_node;

  bool any_dguides_p = false;
  tree cands = deduction_guides_for (tmpl, any_dguides_p, complain);
  if (cands == error_mark_node)
    return error_mark_node;

  /* Prune explicit deduction guides in copy-initialization context (but
     not copy-list-initialization).  */
  bool elided = false;
  if (!list_init_p && (flags & LOOKUP_ONLYCONVERTING))
    {
      for (lkp_iterator iter (cands); !elided && iter; ++iter)
	if (DECL_NONCONVERTING_P (STRIP_TEMPLATE (*iter)))
	  elided = true;

      if (elided)
	{
	  /* Found a nonconverting guide, prune the candidates.  */
	  tree pruned = NULL_TREE;
	  for (lkp_iterator iter (cands); iter; ++iter)
	    if (!DECL_NONCONVERTING_P (STRIP_TEMPLATE (*iter)))
	      pruned = lookup_add (*iter, pruned);

	  cands = pruned;
	}
    }

  if (!any_dguides_p)
    if (tree guide = maybe_aggr_guide (tmpl, init, args))
      cands = lookup_add (guide, cands);

  tree fndecl = error_mark_node;

  /* If this is list-initialization and the class has a list guide, first
     try deducing from the list as a single argument, as [over.match.list].  */
  if (try_list_cand)
    {
      tree list_cands = NULL_TREE;
      for (tree dg : lkp_range (cands))
	if (is_list_ctor (dg))
	  list_cands = lookup_add (dg, list_cands);
      if (list_cands)
	fndecl = perform_dguide_overload_resolution (list_cands, args, tf_none);
      if (fndecl == error_mark_node)
	{
	  /* That didn't work, now try treating the list as a sequence of
	     arguments.  */
	  release_tree_vector (args);
	  args = make_tree_vector_from_ctor (init);
	  args = resolve_args (args, complain);
	  if (args == NULL)
	    return error_mark_node;
	}
    }

  if (elided && !cands)
    {
      error ("cannot deduce template arguments for copy-initialization"
	     " of %qT, as it has no non-explicit deduction guides or "
	     "user-declared constructors", type);
      return error_mark_node;
    }
  else if (!cands && fndecl == error_mark_node)
    {
      error ("cannot deduce template arguments of %qT, as it has no viable "
	     "deduction guides", type);
      return error_mark_node;
    }

  if (fndecl == error_mark_node)
    fndecl = perform_dguide_overload_resolution (cands, args, tf_none);

  if (fndecl == error_mark_node)
    {
      if (complain & tf_warning_or_error)
	{
	  error ("class template argument deduction failed:");
	  perform_dguide_overload_resolution (cands, args, complain);
	  if (elided)
	    inform (input_location, "explicit deduction guides not considered "
		    "for copy-initialization");
	}
      return error_mark_node;
    }
  /* [over.match.list]/1: In copy-list-initialization, if an explicit
     constructor is chosen, the initialization is ill-formed.  */
  else if (flags & LOOKUP_ONLYCONVERTING)
    {
      if (DECL_NONCONVERTING_P (fndecl))
	{
	  if (complain & tf_warning_or_error)
	    {
	      // TODO: Pass down location from cp_finish_decl.
	      error ("class template argument deduction for %qT failed: "
		     "explicit deduction guide selected in "
		     "copy-list-initialization", type);
	      inform (DECL_SOURCE_LOCATION (fndecl),
		      "explicit deduction guide declared here");

	    }
	  return error_mark_node;
	}
    }

  /* If CTAD succeeded but the type doesn't have any explicit deduction
     guides, this deduction might not be what the user intended.  */
  if (fndecl != error_mark_node && !any_dguides_p && (complain & tf_warning))
    {
      if ((!DECL_IN_SYSTEM_HEADER (fndecl)
	   || global_dc->dc_warn_system_headers)
	  && warning (OPT_Wctad_maybe_unsupported,
		      "%qT may not intend to support class template argument "
		      "deduction", type))
	inform (input_location, "add a deduction guide to suppress this "
		"warning");
    }

  return cp_build_qualified_type (TREE_TYPE (TREE_TYPE (fndecl)),
				  cp_type_quals (ptype));
}

/* Return true if INIT is an unparenthesized id-expression or an
   unparenthesized class member access.  Used for the argument of
   decltype(auto).  */

bool
unparenthesized_id_or_class_member_access_p (tree init)
{
  STRIP_ANY_LOCATION_WRAPPER (init);

  /* We need to be able to tell '(r)' and 'r' apart (when it's of
     reference type).  Only the latter is an id-expression.  */
  if (REFERENCE_REF_P (init)
      && !REF_PARENTHESIZED_P (init))
    init = TREE_OPERAND (init, 0);
  return (DECL_P (init)
	  || ((TREE_CODE (init) == COMPONENT_REF
	       || TREE_CODE (init) == SCOPE_REF)
	      && !REF_PARENTHESIZED_P (init)));
}

/* Replace occurrences of 'auto' in TYPE with the appropriate type deduced
   from INIT.  AUTO_NODE is the TEMPLATE_TYPE_PARM used for 'auto' in TYPE.
   The CONTEXT determines the context in which auto deduction is performed
   and is used to control error diagnostics.  FLAGS are the LOOKUP_* flags.

   OUTER_TARGS is used during template argument deduction (context == adc_unify)
   to properly substitute the result.  It's also used in the adc_unify and
   adc_requirement contexts to communicate the necessary template arguments
   to satisfaction.  OUTER_TARGS is ignored in other contexts.

   Additionally for adc_unify contexts TMPL is the template for which TYPE
   is a template parameter type.

   For partial-concept-ids, extra args from OUTER_TARGS, TMPL and the current
   scope may be appended to the list of deduced template arguments prior to
   determining constraint satisfaction as appropriate.  */

tree
do_auto_deduction (tree type, tree init, tree auto_node,
		   tsubst_flags_t complain /* = tf_warning_or_error */,
		   auto_deduction_context context /* = adc_unspecified */,
		   tree outer_targs /* = NULL_TREE */,
		   int flags /* = LOOKUP_NORMAL */,
		   tree tmpl /* = NULL_TREE */)
{
  if (type == error_mark_node || init == error_mark_node)
    return error_mark_node;

  if (init && type_dependent_expression_p (init)
      && context != adc_unify)
    /* Defining a subset of type-dependent expressions that we can deduce
       from ahead of time isn't worth the trouble.  */
    return type;

  /* Similarly, we can't deduce from another undeduced decl.  */
  if (init && undeduced_auto_decl (init))
    return type;

  /* We may be doing a partial substitution, but we still want to replace
     auto_node.  */
  complain &= ~tf_partial;

  if (init && BRACE_ENCLOSED_INITIALIZER_P (init))
    {
      /* We don't recurse here because we can't deduce from a nested
	 initializer_list.  */
      if (CONSTRUCTOR_ELTS (init))
	for (constructor_elt &elt : CONSTRUCTOR_ELTS (init))
	  elt.value = resolve_nondeduced_context (elt.value, complain);
    }
  else if (init)
    init = resolve_nondeduced_context (init, complain);

  /* In C++23, we must deduce the type to int&& for code like
       decltype(auto) f(int&& x) { return (x); }
     or
       auto&& f(int x) { return x; }
     so we use treat_lvalue_as_rvalue_p.  But don't do it for
       decltype(auto) f(int x) { return x; }
     where we should deduce 'int' rather than 'int&&'; transmogrifying
     INIT to an rvalue would break that.  */
  tree r;
  if (cxx_dialect >= cxx23
      && context == adc_return_type
      && (!AUTO_IS_DECLTYPE (auto_node)
	  || !unparenthesized_id_or_class_member_access_p (init))
      && (r = treat_lvalue_as_rvalue_p (maybe_undo_parenthesized_ref (init),
					/*return*/true)))
    init = r;

  if (tree ctmpl = CLASS_PLACEHOLDER_TEMPLATE (auto_node))
    /* C++17 class template argument deduction.  */
    return do_class_deduction (type, ctmpl, init, flags, complain);

  if (init == NULL_TREE || TREE_TYPE (init) == NULL_TREE)
    /* Nothing we can do with this, even in deduction context.  */
    return type;

  location_t loc = cp_expr_loc_or_input_loc (init);

  /* [dcl.spec.auto]: Obtain P from T by replacing the occurrences of auto
     with either a new invented type template parameter U or, if the
     initializer is a braced-init-list (8.5.4), with
     std::initializer_list<U>.  */
  if (BRACE_ENCLOSED_INITIALIZER_P (init))
    {
      if (!DIRECT_LIST_INIT_P (init))
	type = listify_autos (type, auto_node);
      else if (CONSTRUCTOR_NELTS (init) == 1)
	init = CONSTRUCTOR_ELT (init, 0)->value;
      else
	{
          if (complain & tf_warning_or_error)
            {
	      if (permerror (loc, "direct-list-initialization of "
			     "%<auto%> requires exactly one element"))
		inform (loc,
		        "for deduction to %<std::initializer_list%>, use copy-"
		        "list-initialization (i.e. add %<=%> before the %<{%>)");
            }
	  type = listify_autos (type, auto_node);
	}
    }

  if (type == error_mark_node || init == error_mark_node)
    return error_mark_node;

  tree targs;
  if (context == adc_decomp_type
      && auto_node == type
      && TREE_CODE (TREE_TYPE (init)) == ARRAY_TYPE)
    {
      /* [dcl.struct.bind]/1 - if decomposition declaration has no ref-qualifiers
	 and initializer has array type, deduce cv-qualified array type.  */
      targs = make_tree_vec (1);
      TREE_VEC_ELT (targs, 0) = TREE_TYPE (init);
    }
  else if (AUTO_IS_DECLTYPE (auto_node))
    {
      const bool id = unparenthesized_id_or_class_member_access_p (init);
      tree deduced = finish_decltype_type (init, id, complain);
      deduced = canonicalize_type_argument (deduced, complain);
      if (deduced == error_mark_node)
	return error_mark_node;
      targs = make_tree_vec (1);
      TREE_VEC_ELT (targs, 0) = deduced;
    }
  else
    {
      if (error_operand_p (init))
	return error_mark_node;

      tree parms = build_tree_list (NULL_TREE, type);
      tree tparms;

      if (flag_concepts_ts)
	tparms = extract_autos (type);
      else
	{
	  tparms = make_tree_vec (1);
	  TREE_VEC_ELT (tparms, 0)
	    = build_tree_list (NULL_TREE, TYPE_NAME (auto_node));
	}

      targs = make_tree_vec (TREE_VEC_LENGTH (tparms));
      int val = type_unification_real (tparms, targs, parms, &init, 1, 0,
				       DEDUCE_CALL,
				       NULL, /*explain_p=*/false);
      if (val > 0)
	{
	  if (processing_template_decl)
	    /* Try again at instantiation time.  */
	    return type;
	  if (type && type != error_mark_node
	      && (complain & tf_error))
	    /* If type is error_mark_node a diagnostic must have been
	       emitted by now.  Also, having a mention to '<type error>'
	       in the diagnostic is not really useful to the user.  */
	    {
	      if (cfun
		  && FNDECL_USED_AUTO (current_function_decl)
		  && (auto_node
		      == DECL_SAVED_AUTO_RETURN_TYPE (current_function_decl))
		  && LAMBDA_FUNCTION_P (current_function_decl))
		error_at (loc, "unable to deduce lambda return type from %qE",
			  init);
	      else
		error_at (loc, "unable to deduce %qT from %qE", type, init);
	      type_unification_real (tparms, targs, parms, &init, 1, 0,
				     DEDUCE_CALL,
				     NULL, /*explain_p=*/true);
	    }
	  return error_mark_node;
	}
    }

  /* Check any placeholder constraints against the deduced type. */
  if (processing_template_decl && context == adc_unify)
    /* Constraints will be checked after deduction.  */;
  else if (tree constr = NON_ERROR (PLACEHOLDER_TYPE_CONSTRAINTS (auto_node)))
    {
      if (processing_template_decl)
	{
	  gcc_checking_assert (context == adc_variable_type
			       || context == adc_return_type
			       || context == adc_decomp_type);
	  gcc_checking_assert (!type_dependent_expression_p (init));
	  /* If the constraint is dependent, we need to wait until
	     instantiation time to resolve the placeholder.  */
	  if (placeholder_type_constraint_dependent_p (constr))
	    return type;
	}

      if (context == adc_return_type
	  || context == adc_variable_type
	  || context == adc_decomp_type)
	if (tree fn = current_function_decl)
	  if (DECL_TEMPLATE_INFO (fn) || LAMBDA_FUNCTION_P (fn))
	    {
	      outer_targs = DECL_TEMPLATE_INFO (fn)
		? DECL_TI_ARGS (fn) : NULL_TREE;
	      if (LAMBDA_FUNCTION_P (fn))
		{
		  /* As in satisfy_declaration_constraints.  */
		  tree regen_args = lambda_regenerating_args (fn);
		  if (outer_targs)
		    outer_targs = add_to_template_args (regen_args, outer_targs);
		  else
		    outer_targs = regen_args;
		}
	    }

      tree full_targs = outer_targs;
      if (context == adc_unify && tmpl)
	full_targs = add_outermost_template_args (tmpl, full_targs);
      full_targs = add_to_template_args (full_targs, targs);

      /* HACK: Compensate for callers not always communicating all levels of
	 outer template arguments by filling in the outermost missing levels
	 with dummy levels before checking satisfaction.  We'll still crash
	 if the constraint depends on a template argument belonging to one of
	 these missing levels, but this hack otherwise allows us to handle a
	 large subset of possible constraints (including all non-dependent
	 constraints).  */
      if (int missing_levels = (TEMPLATE_TYPE_ORIG_LEVEL (auto_node)
				- TMPL_ARGS_DEPTH (full_targs)))
	{
	  tree dummy_levels = make_tree_vec (missing_levels);
	  for (int i = 0; i < missing_levels; ++i)
	    TREE_VEC_ELT (dummy_levels, i) = make_tree_vec (0);
	  full_targs = add_to_template_args (dummy_levels, full_targs);
	}

      if (!constraints_satisfied_p (auto_node, full_targs))
	{
	  if (complain & tf_warning_or_error)
	    {
	      auto_diagnostic_group d;
	      switch (context)
		{
		case adc_unspecified:
		case adc_unify:
		  error_at (loc, "placeholder constraints not satisfied");
		  break;
		case adc_variable_type:
		case adc_decomp_type:
		  error_at (loc, "deduced initializer does not satisfy "
			    "placeholder constraints");
		  break;
		case adc_return_type:
		  error_at (loc, "deduced return type does not satisfy "
			    "placeholder constraints");
		  break;
		case adc_requirement:
		  error_at (loc, "deduced expression type does not satisfy "
			    "placeholder constraints");
		  break;
		}
	      diagnose_constraints (loc, auto_node, full_targs);
	    }
	  return error_mark_node;
	}
    }

  if (TEMPLATE_TYPE_LEVEL (auto_node) == 1)
    /* The outer template arguments are already substituted into type
       (but we still may have used them for constraint checking above).  */;
  else if (context == adc_unify)
    targs = add_to_template_args (outer_targs, targs);
  else if (processing_template_decl)
    targs = add_to_template_args (current_template_args (), targs);
  return tsubst (type, targs, complain, NULL_TREE);
}

/* Substitutes LATE_RETURN_TYPE for 'auto' in TYPE and returns the
   result.  */

tree
splice_late_return_type (tree type, tree late_return_type)
{
  if (late_return_type)
    {
      gcc_assert (is_auto (type) || seen_error ());
      return late_return_type;
    }

  if (tree auto_node = find_type_usage (type, is_auto))
    if (TEMPLATE_TYPE_LEVEL (auto_node) <= current_template_depth)
      {
	/* In an abbreviated function template we didn't know we were dealing
	   with a function template when we saw the auto return type, so rebuild
	   the return type using an auto with the correct level.  */
	tree new_auto = make_auto_1 (TYPE_IDENTIFIER (auto_node), false);
	tree auto_vec = make_tree_vec (1);
	TREE_VEC_ELT (auto_vec, 0) = new_auto;
	tree targs = add_outermost_template_args (current_template_args (),
						  auto_vec);
	/* Also rebuild the constraint info in terms of the new auto.  */
	if (tree ci = PLACEHOLDER_TYPE_CONSTRAINTS_INFO (auto_node))
	  PLACEHOLDER_TYPE_CONSTRAINTS_INFO (new_auto)
	    = build_tree_list (current_template_parms,
			       tsubst_constraint (TREE_VALUE (ci), targs,
						  tf_none, NULL_TREE));
	TYPE_CANONICAL (new_auto) = canonical_type_parameter (new_auto);
	return tsubst (type, targs, tf_none, NULL_TREE);
      }
  return type;
}

/* Returns true iff TYPE is a TEMPLATE_TYPE_PARM representing 'auto' or
   'decltype(auto)' or a deduced class template.  */

bool
is_auto (const_tree type)
{
  if (TREE_CODE (type) == TEMPLATE_TYPE_PARM
      && (TYPE_IDENTIFIER (type) == auto_identifier
	  || TYPE_IDENTIFIER (type) == decltype_auto_identifier))
    return true;
  else
    return false;
}

/* for_each_template_parm callback for type_uses_auto.  */

int
is_auto_r (tree tp, void */*data*/)
{
  return is_auto (tp);
}

/* Returns the TEMPLATE_TYPE_PARM in TYPE representing `auto' iff TYPE contains
   a use of `auto'.  Returns NULL_TREE otherwise.  */

tree
type_uses_auto (tree type)
{
  if (type == NULL_TREE)
    return NULL_TREE;

  /* For parameter packs, check the contents of the pack.  */
  if (PACK_EXPANSION_P (type))
    type = PACK_EXPANSION_PATTERN (type);

  if (flag_concepts_ts)
    {
      /* The Concepts TS allows multiple autos in one type-specifier; just
	 return the first one we find, do_auto_deduction will collect all of
	 them.  */
      if (uses_template_parms (type))
	return for_each_template_parm (type, is_auto_r, /*data*/NULL,
				       /*visited*/NULL, /*nondeduced*/false);
      else
	return NULL_TREE;
    }
  else
    return find_type_usage (type, is_auto);
}

/* Report ill-formed occurrences of auto types in ARGUMENTS.  If
   concepts are enabled, auto is acceptable in template arguments, but
   only when TEMPL identifies a template class.  Return TRUE if any
   such errors were reported.  */

bool
check_auto_in_tmpl_args (tree tmpl, tree args)
{
  if (!flag_concepts_ts)
    /* Only the concepts TS allows 'auto' as a type-id; it'd otherwise
       have already been rejected by the parser more generally.  */
    return false;

  /* If there were previous errors, nevermind.  */
  if (!args || TREE_CODE (args) != TREE_VEC)
    return false;

  /* If TMPL is an identifier, we're parsing and we can't tell yet
     whether TMPL is supposed to be a type, a function or a variable.
     We'll only be able to tell during template substitution, so we
     expect to be called again then.  If concepts are enabled and we
     know we have a type, we're ok.  */
  if (identifier_p (tmpl)
      || (DECL_P (tmpl)
	  &&  (DECL_TYPE_TEMPLATE_P (tmpl)
	       || DECL_TEMPLATE_TEMPLATE_PARM_P (tmpl))))
    return false;

  /* Quickly search for any occurrences of auto; usually there won't
     be any, and then we'll avoid allocating the vector.  */
  if (!type_uses_auto (args))
    return false;

  bool errors = false;

  tree vec = extract_autos (args);
  for (int i = 0; i < TREE_VEC_LENGTH (vec); i++)
    {
      tree xauto = TREE_VALUE (TREE_VEC_ELT (vec, i));
      error_at (DECL_SOURCE_LOCATION (xauto),
		"invalid use of %qT in template argument", xauto);
      errors = true;
    }

  return errors;
}

/* Recursively walk over && expressions searching for EXPR. Return a reference
   to that expression.  */

static tree *find_template_requirement (tree *t, tree key)
{
  if (*t == key)
    return t;
  if (TREE_CODE (*t) == TRUTH_ANDIF_EXPR)
    {
      if (tree *p = find_template_requirement (&TREE_OPERAND (*t, 0), key))
	return p;
      if (tree *p = find_template_requirement (&TREE_OPERAND (*t, 1), key))
	return p;
    }
  return 0;
}

/* Convert the generic type parameters in PARM that match the types given in the
   range [START_IDX, END_IDX) from the current_template_parms into generic type
   packs.  */

tree
convert_generic_types_to_packs (tree parm, int start_idx, int end_idx)
{
  tree current = current_template_parms;
  int depth = TMPL_PARMS_DEPTH (current);
  current = INNERMOST_TEMPLATE_PARMS (current);
  tree replacement = make_tree_vec (TREE_VEC_LENGTH (current));

  for (int i = 0; i < start_idx; ++i)
    TREE_VEC_ELT (replacement, i)
      = TREE_TYPE (TREE_VALUE (TREE_VEC_ELT (current, i)));

  for (int i = start_idx; i < end_idx; ++i)
    {
      /* Create a distinct parameter pack type from the current parm and add it
	 to the replacement args to tsubst below into the generic function
	 parameter.  */
      tree node = TREE_VEC_ELT (current, i);
      tree o = TREE_TYPE (TREE_VALUE (node));
      tree t = copy_type (o);
      TEMPLATE_TYPE_PARM_INDEX (t)
	= reduce_template_parm_level (TEMPLATE_TYPE_PARM_INDEX (o),
				      t, 0, 0, tf_none);
      TREE_TYPE (TEMPLATE_TYPE_DECL (t)) = t;
      TYPE_STUB_DECL (t) = TYPE_NAME (t) = TEMPLATE_TYPE_DECL (t);
      TYPE_MAIN_VARIANT (t) = t;
      TEMPLATE_TYPE_PARAMETER_PACK (t) = true;
      TYPE_CANONICAL (t) = canonical_type_parameter (t);
      TREE_VEC_ELT (replacement, i) = t;

      /* Replace the current template parameter with new pack.  */
      TREE_VALUE (node) = TREE_CHAIN (t);

      /* Surgically adjust the associated constraint of adjusted parameter
         and it's corresponding contribution to the current template
         requirements.  */
      if (tree constr = TEMPLATE_PARM_CONSTRAINTS (node))
	{
	  tree id = unpack_concept_check (constr);
	  TREE_VEC_ELT (TREE_OPERAND (id, 1), 0) = t;
	  tree fold = finish_left_unary_fold_expr (constr, TRUTH_ANDIF_EXPR);
	  TEMPLATE_PARM_CONSTRAINTS (node) = fold;

	  /* If there was a constraint, we also need to replace that in
	     the template requirements, which we've already built.  */
	  tree *reqs = &TEMPLATE_PARMS_CONSTRAINTS (current_template_parms);
	  reqs = find_template_requirement (reqs, constr);
	  *reqs = fold;
	}
    }

  for (int i = end_idx, e = TREE_VEC_LENGTH (current); i < e; ++i)
    TREE_VEC_ELT (replacement, i)
      = TREE_TYPE (TREE_VALUE (TREE_VEC_ELT (current, i)));

  /* If there are more levels then build up the replacement with the outer
     template parms.  */
  if (depth > 1)
    replacement = add_to_template_args (template_parms_to_args
					(TREE_CHAIN (current_template_parms)),
					replacement);

  return tsubst (parm, replacement, tf_none, NULL_TREE);
}

/* __integer_pack(N) in a pack expansion expands to a sequence of numbers from
   0..N-1.  */

void
declare_integer_pack (void)
{
  tree ipfn = push_library_fn (get_identifier ("__integer_pack"),
			       build_function_type_list (integer_type_node,
							 integer_type_node,
							 NULL_TREE),
			       NULL_TREE, ECF_CONST);
  DECL_DECLARED_CONSTEXPR_P (ipfn) = true;
  set_decl_built_in_function (ipfn, BUILT_IN_FRONTEND,
			      CP_BUILT_IN_INTEGER_PACK);
}

/* Walk the decl or type specialization table calling FN on each
   entry.  */

void
walk_specializations (bool decls_p,
		      void (*fn) (bool decls_p, spec_entry *entry, void *data),
		      void *data)
{
  spec_hash_table *table = decls_p ? decl_specializations
    : type_specializations;
  spec_hash_table::iterator end (table->end ());
  for (spec_hash_table::iterator iter (table->begin ()); iter != end; ++iter)
    fn (decls_p, *iter, data);
}

/* Lookup the specialization of *ELT, in the decl or type
   specialization table.  Return the SPEC that's already there, or
   NULL if nothing.  */

tree
match_mergeable_specialization (bool decl_p, spec_entry *elt)
{
  hash_table<spec_hasher> *specializations
    = decl_p ? decl_specializations : type_specializations;
  hashval_t hash = spec_hasher::hash (elt);
  auto *slot = specializations->find_slot_with_hash (elt, hash, NO_INSERT);

  if (slot)
    return (*slot)->spec;

  return NULL_TREE;
}

/* Return flags encoding whether SPEC is on the instantiation and/or
   specialization lists of TMPL.  */

unsigned
get_mergeable_specialization_flags (tree tmpl, tree decl)
{
  unsigned flags = 0;

  for (tree inst = DECL_TEMPLATE_INSTANTIATIONS (tmpl);
       inst; inst = TREE_CHAIN (inst))
    if (TREE_VALUE (inst) == decl)
      {
	flags |= 1;
	break;
      }

  if (CLASS_TYPE_P (TREE_TYPE (decl))
      && CLASSTYPE_TEMPLATE_INFO (TREE_TYPE (decl))
      && CLASSTYPE_USE_TEMPLATE (TREE_TYPE (decl)) == 2)
    /* Only need to search if DECL is a partial specialization.  */
    for (tree part = DECL_TEMPLATE_SPECIALIZATIONS (tmpl);
	 part; part = TREE_CHAIN (part))
      if (TREE_VALUE (part) == decl)
	{
	  flags |= 2;
	  break;
	}

  return flags;
}

/* Add a new specialization described by SPEC.  DECL is the
   maybe-template decl and FLAGS is as returned from
   get_mergeable_specialization_flags.  */

void
add_mergeable_specialization (bool decl_p, bool alias_p, spec_entry *elt,
			      tree decl, unsigned flags)
{
  hashval_t hash = spec_hasher::hash (elt);
  if (decl_p)
    {
      auto *slot = decl_specializations->find_slot_with_hash (elt, hash, INSERT);

      gcc_checking_assert (!*slot);
      auto entry = ggc_alloc<spec_entry> ();
      *entry = *elt;
      *slot = entry;

      if (alias_p)
	{
	  elt->spec = TREE_TYPE (elt->spec);
	  gcc_checking_assert (elt->spec);
	}
    }

  if (!decl_p || alias_p)
    {
      auto *slot = type_specializations->find_slot_with_hash (elt, hash, INSERT);

      /* We don't distinguish different constrained partial type
	 specializations, so there could be duplicates.  Everything else
	 must be new.   */
      if (!(flags & 2 && *slot))
	{
	  gcc_checking_assert (!*slot);

	  auto entry = ggc_alloc<spec_entry> ();
	  *entry = *elt;
	  *slot = entry;
	}
    }

  if (flags & 1)
    DECL_TEMPLATE_INSTANTIATIONS (elt->tmpl)
      = tree_cons (elt->args, decl, DECL_TEMPLATE_INSTANTIATIONS (elt->tmpl));

  if (flags & 2)
    {
      /* A partial specialization.  */
      tree cons = tree_cons (elt->args, decl,
			     DECL_TEMPLATE_SPECIALIZATIONS (elt->tmpl));
      TREE_TYPE (cons) = decl_p ? TREE_TYPE (elt->spec) : elt->spec;
      DECL_TEMPLATE_SPECIALIZATIONS (elt->tmpl) = cons;
    }
}

/* Set up the hash tables for template instantiations.  */

void
init_template_processing (void)
{
  decl_specializations = hash_table<spec_hasher>::create_ggc (37);
  type_specializations = hash_table<spec_hasher>::create_ggc (37);

  if (cxx_dialect >= cxx11)
    declare_integer_pack ();
}

/* Print stats about the template hash tables for -fstats.  */

void
print_template_statistics (void)
{
  fprintf (stderr, "decl_specializations: size %ld, %ld elements, "
	   "%f collisions\n", (long) decl_specializations->size (),
	   (long) decl_specializations->elements (),
	   decl_specializations->collisions ());
  fprintf (stderr, "type_specializations: size %ld, %ld elements, "
	   "%f collisions\n", (long) type_specializations->size (),
	   (long) type_specializations->elements (),
	   type_specializations->collisions ());
}

#if CHECKING_P

namespace selftest {

/* Verify that build_non_dependent_expr () works, for various expressions,
   and that location wrappers don't affect the results.  */

static void
test_build_non_dependent_expr ()
{
  location_t loc = BUILTINS_LOCATION;

  /* Verify constants, without and with location wrappers.  */
  tree int_cst = build_int_cst (integer_type_node, 42);
  ASSERT_EQ (int_cst, build_non_dependent_expr (int_cst));

  tree wrapped_int_cst = maybe_wrap_with_location (int_cst, loc);
  ASSERT_TRUE (location_wrapper_p (wrapped_int_cst));
  ASSERT_EQ (wrapped_int_cst, build_non_dependent_expr (wrapped_int_cst));

  tree string_lit = build_string (4, "foo");
  TREE_TYPE (string_lit) = char_array_type_node;
  string_lit = fix_string_type (string_lit);
  ASSERT_EQ (string_lit, build_non_dependent_expr (string_lit));

  tree wrapped_string_lit = maybe_wrap_with_location (string_lit, loc);
  ASSERT_TRUE (location_wrapper_p (wrapped_string_lit));
  ASSERT_EQ (wrapped_string_lit,
	     build_non_dependent_expr (wrapped_string_lit));
}

/* Verify that type_dependent_expression_p () works correctly, even
   in the presence of location wrapper nodes.  */

static void
test_type_dependent_expression_p ()
{
  location_t loc = BUILTINS_LOCATION;

  tree name = get_identifier ("foo");

  /* If no templates are involved, nothing is type-dependent.  */
  gcc_assert (!processing_template_decl);
  ASSERT_FALSE (type_dependent_expression_p (name));

  ++processing_template_decl;

  /* Within a template, an unresolved name is always type-dependent.  */
  ASSERT_TRUE (type_dependent_expression_p (name));

  /* Ensure it copes with NULL_TREE and errors.  */
  ASSERT_FALSE (type_dependent_expression_p (NULL_TREE));
  ASSERT_FALSE (type_dependent_expression_p (error_mark_node));

  /* A USING_DECL in a template should be type-dependent, even if wrapped
     with a location wrapper (PR c++/83799).  */
  tree using_decl = build_lang_decl (USING_DECL, name, NULL_TREE);
  TREE_TYPE (using_decl) = integer_type_node;
  ASSERT_TRUE (type_dependent_expression_p (using_decl));
  tree wrapped_using_decl = maybe_wrap_with_location (using_decl, loc);
  ASSERT_TRUE (location_wrapper_p (wrapped_using_decl));
  ASSERT_TRUE (type_dependent_expression_p (wrapped_using_decl));

  --processing_template_decl;
}

/* Run all of the selftests within this file.  */

void
cp_pt_cc_tests ()
{
  test_build_non_dependent_expr ();
  test_type_dependent_expression_p ();
}

} // namespace selftest

#endif /* #if CHECKING_P */

#include "gt-cp-pt.h"
