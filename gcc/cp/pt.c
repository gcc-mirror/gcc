/* Handle parameterized types (templates) for GNU -*- C++ -*-.
   Copyright (C) 1992-2014 Free Software Foundation, Inc.
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
     file that contains only the method templates and "just win".  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "stringpool.h"
#include "varasm.h"
#include "attribs.h"
#include "stor-layout.h"
#include "intl.h"
#include "pointer-set.h"
#include "flags.h"
#include "cp-tree.h"
#include "c-family/c-common.h"
#include "c-family/c-objc.h"
#include "cp-objcp-common.h"
#include "tree-inline.h"
#include "decl.h"
#include "toplev.h"
#include "timevar.h"
#include "tree-iterator.h"
#include "type-utils.h"
#include "gimplify.h"

/* The type of functions taking a tree, and some additional data, and
   returning an int.  */
typedef int (*tree_fn_t) (tree, void*);

/* The PENDING_TEMPLATES is a TREE_LIST of templates whose
   instantiations have been deferred, either because their definitions
   were not yet available, or because we were putting off doing the work.  */
struct GTY ((chain_next ("%h.next"))) pending_template {
  struct pending_template *next;
  struct tinst_level *tinst;
};

static GTY(()) struct pending_template *pending_templates;
static GTY(()) struct pending_template *last_pending_template;

int processing_template_parmlist;
static int template_header_count;

static GTY(()) tree saved_trees;
static vec<int> inline_parm_levels;

static GTY(()) struct tinst_level *current_tinst_level;

static GTY(()) tree saved_access_scope;

/* Live only within one (recursive) call to tsubst_expr.  We use
   this to pass the statement expression node from the STMT_EXPR
   to the EXPR_STMT that is its result.  */
static tree cur_stmt_expr;

/* True if we've recursed into fn_type_unification too many times.  */
static bool excessive_deduction_depth;

typedef struct GTY(()) spec_entry
{
  tree tmpl;
  tree args;
  tree spec;
} spec_entry;

static GTY ((param_is (spec_entry)))
  htab_t decl_specializations;

static GTY ((param_is (spec_entry)))
  htab_t type_specializations;

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

static void push_access_scope (tree);
static void pop_access_scope (tree);
static bool resolve_overloaded_unification (tree, tree, tree, tree,
					    unification_kind_t, int,
					    bool);
static int try_one_overload (tree, tree, tree, tree, tree,
			     unification_kind_t, int, bool, bool);
static int unify (tree, tree, tree, tree, int, bool);
static void add_pending_template (tree);
static tree reopen_tinst_level (struct tinst_level *);
static tree tsubst_initializer_list (tree, tree);
static tree get_class_bindings (tree, tree, tree, tree);
static tree coerce_template_parms (tree, tree, tree, tsubst_flags_t,
				   bool, bool);
static tree coerce_innermost_template_parms (tree, tree, tree, tsubst_flags_t,
					      bool, bool);
static void tsubst_enum	(tree, tree, tree);
static tree add_to_template_args (tree, tree);
static tree add_outermost_template_args (tree, tree);
static bool check_instantiated_args (tree, tree, tsubst_flags_t);
static int maybe_adjust_types_for_deduction (unification_kind_t, tree*, tree*,
					     tree);
static int type_unification_real (tree, tree, tree, const tree *,
				  unsigned int, int, unification_kind_t, int,
				  vec<deferred_access_check, va_gc> **,
				  bool);
static void note_template_header (int);
static tree convert_nontype_argument_function (tree, tree);
static tree convert_nontype_argument (tree, tree, tsubst_flags_t);
static tree convert_template_argument (tree, tree, tree,
				       tsubst_flags_t, int, tree);
static int for_each_template_parm (tree, tree_fn_t, void*,
				   struct pointer_set_t*, bool);
static tree expand_template_argument_pack (tree);
static tree build_template_parm_index (int, int, int, tree, tree);
static bool inline_needs_template_parms (tree, bool);
static void push_inline_template_parms_recursive (tree, int);
static tree retrieve_local_specialization (tree);
static void register_local_specialization (tree, tree);
static hashval_t hash_specialization (const void *p);
static tree reduce_template_parm_level (tree, tree, int, tree, tsubst_flags_t);
static int mark_template_parm (tree, void *);
static int template_parm_this_level_p (tree, void *);
static tree tsubst_friend_function (tree, tree);
static tree tsubst_friend_class (tree, tree);
static int can_complete_type_without_circularity (tree);
static tree get_bindings (tree, tree, tree, bool);
static int template_decl_level (tree);
static int check_cv_quals_for_unify (int, tree, tree);
static void template_parm_level_and_index (tree, int*, int*);
static int unify_pack_expansion (tree, tree, tree,
				 tree, unification_kind_t, bool, bool);
static tree tsubst_template_arg (tree, tree, tsubst_flags_t, tree);
static tree tsubst_template_args (tree, tree, tsubst_flags_t, tree);
static tree tsubst_template_parms (tree, tree, tsubst_flags_t);
static void regenerate_decl_from_template (tree, tree);
static tree most_specialized_class (tree, tsubst_flags_t);
static tree tsubst_aggr_type (tree, tree, tsubst_flags_t, tree, int);
static tree tsubst_arg_types (tree, tree, tree, tsubst_flags_t, tree);
static tree tsubst_function_type (tree, tree, tsubst_flags_t, tree);
static bool check_specialization_scope (void);
static tree process_partial_specialization (tree);
static void set_current_access_from_decl (tree);
static enum template_base_result get_template_base (tree, tree, tree, tree,
						    bool , tree *);
static tree try_class_unification (tree, tree, tree, tree, bool);
static int coerce_template_template_parms (tree, tree, tsubst_flags_t,
					   tree, tree);
static bool template_template_parm_bindings_ok_p (tree, tree);
static int template_args_equal (tree, tree);
static void tsubst_default_arguments (tree, tsubst_flags_t);
static tree for_each_template_parm_r (tree *, int *, void *);
static tree copy_default_args_to_explicit_spec_1 (tree, tree);
static void copy_default_args_to_explicit_spec (tree);
static int invalid_nontype_parm_type_p (tree, tsubst_flags_t);
static bool dependent_template_arg_p (tree);
static bool any_template_arguments_need_structural_equality_p (tree);
static bool dependent_type_p_r (tree);
static tree tsubst_expr	(tree, tree, tsubst_flags_t, tree, bool);
static tree tsubst_copy	(tree, tree, tsubst_flags_t, tree);
static tree tsubst_pack_expansion (tree, tree, tsubst_flags_t, tree);
static tree tsubst_decl (tree, tree, tsubst_flags_t);
static void perform_typedefs_access_check (tree tmpl, tree targs);
static void append_type_to_template_for_access_check_1 (tree, tree, tree,
							location_t);
static tree listify (tree);
static tree listify_autos (tree, tree);
static tree template_parm_to_arg (tree t);
static tree current_template_args (void);
static tree tsubst_template_parm (tree, tree, tsubst_flags_t);
static tree instantiate_alias_template (tree, tree, tsubst_flags_t);

/* Make the current scope suitable for access checking when we are
   processing T.  T can be FUNCTION_DECL for instantiated function
   template, VAR_DECL for static member variable, or TYPE_DECL for
   alias template (needed by instantiate_decl).  */

static void
push_access_scope (tree t)
{
  gcc_assert (VAR_OR_FUNCTION_DECL_P (t)
	      || TREE_CODE (t) == TYPE_DECL);

  if (DECL_FRIEND_CONTEXT (t))
    push_nested_class (DECL_FRIEND_CONTEXT (t));
  else if (DECL_CLASS_SCOPE_P (t))
    push_nested_class (DECL_CONTEXT (t));
  else
    push_to_top_level ();

  if (TREE_CODE (t) == FUNCTION_DECL)
    {
      saved_access_scope = tree_cons
	(NULL_TREE, current_function_decl, saved_access_scope);
      current_function_decl = t;
    }
}

/* Restore the scope set up by push_access_scope.  T is the node we
   are processing.  */

static void
pop_access_scope (tree t)
{
  if (TREE_CODE (t) == FUNCTION_DECL)
    {
      current_function_decl = TREE_VALUE (saved_access_scope);
      saved_access_scope = TREE_CHAIN (saved_access_scope);
    }

  if (DECL_FRIEND_CONTEXT (t) || DECL_CLASS_SCOPE_P (t))
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
    error ("data member %qD cannot be a member template", decl);
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
    error ("invalid member template declaration %qD", decl);

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

  for (depth = 0;
       type && TREE_CODE (type) != NAMESPACE_DECL;
       type = (TREE_CODE (type) == FUNCTION_DECL)
	 ? CP_DECL_CONTEXT (type) : CP_TYPE_CONTEXT (type))
    {
      tree tinfo = get_template_info (type);

      if (tinfo && PRIMARY_TEMPLATE_P (TI_TEMPLATE (tinfo))
	  && uses_template_parms (INNERMOST_TEMPLATE_ARGS (TI_ARGS (tinfo))))
	++depth;
    }

  return depth;
}

/* Subroutine of maybe_begin_member_template_processing.
   Returns true if processing DECL needs us to push template parms.  */

static bool
inline_needs_template_parms (tree decl, bool nsdmi)
{
  if (!decl || (!nsdmi && ! DECL_TEMPLATE_INFO (decl)))
    return false;

  return (TMPL_PARMS_DEPTH (DECL_TEMPLATE_PARMS (most_general_template (decl)))
	  > (processing_template_decl + DECL_TEMPLATE_SPECIALIZATION (decl)));
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
    = tree_cons (size_int (processing_template_decl),
		 parms, current_template_parms);
  TEMPLATE_PARMS_FOR_INLINE (current_template_parms) = 1;

  begin_scope (TREE_VEC_LENGTH (parms) ? sk_template_parms : sk_template_spec,
	       NULL);
  for (i = 0; i < TREE_VEC_LENGTH (parms); ++i)
    {
      tree parm = TREE_VALUE (TREE_VEC_ELT (parms, i));

      if (parm == error_mark_node)
	continue;

      gcc_assert (DECL_P (parm));

      switch (TREE_CODE (parm))
	{
	case TYPE_DECL:
	case TEMPLATE_DECL:
	  pushdecl (parm);
	  break;

	case PARM_DECL:
	  {
	    /* Make a CONST_DECL as is done in process_template_parm.
	       It is ugly that we recreate this here; the original
	       version built in process_template_parm is no longer
	       available.  */
	    tree decl = build_decl (DECL_SOURCE_LOCATION (parm),
				    CONST_DECL, DECL_NAME (parm),
				    TREE_TYPE (parm));
	    DECL_ARTIFICIAL (decl) = 1;
	    TREE_CONSTANT (decl) = 1;
	    TREE_READONLY (decl) = 1;
	    DECL_INITIAL (decl) = DECL_INITIAL (parm);
	    SET_DECL_TEMPLATE_PARM_P (decl);
	    pushdecl (decl);
	  }
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
    decl = (CLASSTYPE_TEMPLATE_INFO (DECL_CONTEXT (decl))
	    ? CLASSTYPE_TI_TEMPLATE (DECL_CONTEXT (decl))
	    : NULL_TREE);

  if (inline_needs_template_parms (decl, nsdmi))
    {
      parms = DECL_TEMPLATE_PARMS (most_general_template (decl));
      levels = TMPL_PARMS_DEPTH (parms) - processing_template_decl;

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

static tree
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
   partial instantiation.  */

static tree
add_outermost_template_args (tree args, tree extra_args)
{
  tree new_args;

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

     pushtag contains special code to call pushdecl_with_scope on the
     TEMPLATE_DECL for S2.  */
  begin_scope (sk_template_parms, NULL);
  ++processing_template_decl;
  ++processing_template_parmlist;
  note_template_header (0);
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

     An explicit specialization shall be declared in the namespace of
     which the template is a member, or, for member templates, in the
     namespace of which the enclosing class or enclosing class
     template is a member.  An explicit specialization of a member
     function, member class or static data member of a class template
     shall be declared in the namespace of which the class template is
     a member.  */
  if (current_scope() != DECL_CONTEXT (tmpl)
      && !at_namespace_scope_p ())
    {
      error ("specialization of %qD must appear at namespace scope", tmpl);
      return false;
    }
  if (is_associated_namespace (current_namespace, tpl_ns))
    /* Same or super-using namespace.  */
    return true;
  else
    {
      permerror (input_location, "specialization of %qD in different namespace", tmpl);
      permerror (input_location, "  from definition of %q+#D", tmpl);
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
  if (!is_ancestor (current_namespace, ns))
    permerror (input_location, "explicit instantiation of %qD in namespace %qD "
	       "(which does not enclose namespace %qD)",
	       spec, current_namespace, ns);
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

  if (TREE_CODE (type) == BOUND_TEMPLATE_TEMPLATE_PARM)
    {
      error ("name of class shadows template template parameter %qD",
	     TYPE_NAME (type));
      return error_mark_node;
    }

  context = TYPE_CONTEXT (type);

  if (TYPE_ALIAS_P (type))
    {
      if (TYPE_TEMPLATE_INFO (type)
	  && DECL_ALIAS_TEMPLATE_P (TYPE_TI_TEMPLATE (type)))
	error ("specialization of alias template %qD",
	       TYPE_TI_TEMPLATE (type));
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

      if (CLASSTYPE_IMPLICIT_INSTANTIATION (type)
	  && !COMPLETE_TYPE_P (type))
	{
	  if (!check_specialization_namespace (CLASSTYPE_TI_TEMPLATE (type))
	      && !at_namespace_scope_p ())
	    return error_mark_node;
	  SET_CLASSTYPE_TEMPLATE_SPECIALIZATION (type);
	  DECL_SOURCE_LOCATION (TYPE_MAIN_DECL (type)) = input_location;
	  if (processing_template_decl)
	    {
	      if (push_template_decl (TYPE_MAIN_DECL (type))
		  == error_mark_node)
		return error_mark_node;
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
	      permerror (input_location, "specializing %q#T in different namespace", type);
	      permerror (input_location, "  from definition of %q+#D", tmpl);
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
		  void **slot;

		  elt.tmpl = most_general_template (tmpl);
		  elt.args = CLASSTYPE_TI_ARGS (inst);
		  elt.spec = inst;

		  htab_remove_elt (type_specializations, &elt);

		  elt.tmpl = tmpl;
		  elt.args = INNERMOST_TEMPLATE_ARGS (elt.args);

		  slot = htab_find_slot (type_specializations, &elt, INSERT);
		  entry = ggc_alloc_spec_entry ();
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

/* Returns nonzero if we can optimize the retrieval of specializations
   for TMPL, a TEMPLATE_DECL.  In particular, for such a template, we
   do not use DECL_TEMPLATE_SPECIALIZATIONS at all.  */

static inline bool
optimize_specialization_lookup_p (tree tmpl)
{
  return (DECL_FUNCTION_TEMPLATE_P (tmpl)
	  && DECL_CLASS_SCOPE_P (tmpl)
	  /* DECL_CLASS_SCOPE_P holds of T::f even if T is a template
	     parameter.  */
	  && CLASS_TYPE_P (DECL_CONTEXT (tmpl))
	  /* The optimized lookup depends on the fact that the
	     template arguments for the member function template apply
	     purely to the containing class, which is not true if the
	     containing class is an explicit or partial
	     specialization.  */
	  && !CLASSTYPE_TEMPLATE_SPECIALIZATION (DECL_CONTEXT (tmpl))
	  && !DECL_MEMBER_TEMPLATE_P (tmpl)
	  && !DECL_CONV_FN_P (tmpl)
	  /* It is possible to have a template that is not a member
	     template and is not a member of a template class:

	     template <typename T>
	     struct S { friend A::f(); };

	     Here, the friend function is a template, but the context does
	     not have template information.  The optimized lookup relies
	     on having ARGS be the template arguments for both the class
	     and the function template.  */
	  && !DECL_FRIEND_P (DECL_TEMPLATE_RESULT (tmpl)));
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

  if (optimize_specialization_lookup_p (tmpl))
    {
      tree class_template;
      tree class_specialization;
      vec<tree, va_gc> *methods;
      tree fns;
      int idx;

      /* The template arguments actually apply to the containing
	 class.  Find the class specialization with those
	 arguments.  */
      class_template = CLASSTYPE_TI_TEMPLATE (DECL_CONTEXT (tmpl));
      class_specialization
	= retrieve_specialization (class_template, args, 0);
      if (!class_specialization)
	return NULL_TREE;
      /* Now, find the appropriate entry in the CLASSTYPE_METHOD_VEC
	 for the specialization.  */
      idx = class_method_index_for_fn (class_specialization, tmpl);
      if (idx == -1)
	return NULL_TREE;
      /* Iterate through the methods with the indicated name, looking
	 for the one that has an instance of TMPL.  */
      methods = CLASSTYPE_METHOD_VEC (class_specialization);
      for (fns = (*methods)[idx]; fns; fns = OVL_NEXT (fns))
	{
	  tree fn = OVL_CURRENT (fns);
	  if (DECL_TEMPLATE_INFO (fn) && DECL_TI_TEMPLATE (fn) == tmpl
	      /* using-declarations can add base methods to the method vec,
		 and we don't want those here.  */
	      && DECL_CONTEXT (fn) == class_specialization)
	    return fn;
	}
      return NULL_TREE;
    }
  else
    {
      spec_entry *found;
      spec_entry elt;
      htab_t specializations;

      elt.tmpl = tmpl;
      elt.args = args;
      elt.spec = NULL_TREE;

      if (DECL_CLASS_TEMPLATE_P (tmpl))
	specializations = type_specializations;
      else
	specializations = decl_specializations;

      if (hash == 0)
	hash = hash_specialization (&elt);
      found = (spec_entry *) htab_find_with_hash (specializations, &elt, hash);
      if (found)
	return found->spec;
    }

  return NULL_TREE;
}

/* Like retrieve_specialization, but for local declarations.  */

static tree
retrieve_local_specialization (tree tmpl)
{
  void **slot;

  if (local_specializations == NULL)
    return NULL_TREE;

  slot = pointer_map_contains (local_specializations, tmpl);
  return slot ? (tree) *slot : NULL_TREE;
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
   is actually just a friend declaration.  Returns SPEC, or an
   equivalent prior declaration, if available.

   We also store instantiations of field packs in the hash table, even
   though they are not themselves templates, to make lookup easier.  */

static tree
register_specialization (tree spec, tree tmpl, tree args, bool is_friend,
			 hashval_t hash)
{
  tree fn;
  void **slot = NULL;
  spec_entry elt;

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

  if (optimize_specialization_lookup_p (tmpl))
    /* We don't put these specializations in the hash table, but we might
       want to give an error about a mismatch.  */
    fn = retrieve_specialization (tmpl, args, 0);
  else
    {
      elt.tmpl = tmpl;
      elt.args = args;
      elt.spec = spec;

      if (hash == 0)
	hash = hash_specialization (&elt);

      slot =
	htab_find_slot_with_hash (decl_specializations, &elt, hash, INSERT);
      if (*slot)
	fn = ((spec_entry *) *slot)->spec;
      else
	fn = NULL_TREE;
    }

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
	      duplicate_decls (spec, fn, is_friend);
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
	  if (!duplicate_decls (spec, fn, is_friend) && DECL_INITIAL (spec))
	    /* Dup decl failed, but this is a new definition. Set the
	       line number so any errors match this new
	       definition.  */
	    DECL_SOURCE_LOCATION (fn) = DECL_SOURCE_LOCATION (spec);

	  return fn;
	}
    }
  else if (fn)
    return duplicate_decls (spec, fn, is_friend);

  /* A specialization must be declared in the same namespace as the
     template it is specializing.  */
  if (DECL_P (spec) && DECL_TEMPLATE_SPECIALIZATION (spec)
      && !check_specialization_namespace (tmpl))
    DECL_CONTEXT (spec) = DECL_CONTEXT (tmpl);

  if (slot != NULL /* !optimize_specialization_lookup_p (tmpl) */)
    {
      spec_entry *entry = ggc_alloc_spec_entry ();
      gcc_assert (tmpl && args && spec);
      *entry = elt;
      *slot = entry;
      if (TREE_CODE (spec) == FUNCTION_DECL && DECL_NAMESPACE_SCOPE_P (spec)
	  && PRIMARY_TEMPLATE_P (tmpl)
	  && DECL_SAVED_TREE (DECL_TEMPLATE_RESULT (tmpl)) == NULL_TREE)
	/* TMPL is a forward declaration of a template function; keep a list
	   of all specializations in case we need to reassign them to a friend
	   template later in tsubst_friend_function.  */
	DECL_TEMPLATE_INSTANTIATIONS (tmpl)
	  = tree_cons (args, spec, DECL_TEMPLATE_INSTANTIATIONS (tmpl));
    }

  return spec;
}

/* Returns true iff two spec_entry nodes are equivalent.  Only compares the
   TMPL and ARGS members, ignores SPEC.  */

int comparing_specializations;

static int
eq_specializations (const void *p1, const void *p2)
{
  const spec_entry *e1 = (const spec_entry *)p1;
  const spec_entry *e2 = (const spec_entry *)p2;
  int equal;

  ++comparing_specializations;
  equal = (e1->tmpl == e2->tmpl
	   && comp_template_args (e1->args, e2->args));
  --comparing_specializations;

  return equal;
}

/* Returns a hash for a template TMPL and template arguments ARGS.  */

static hashval_t
hash_tmpl_and_args (tree tmpl, tree args)
{
  hashval_t val = DECL_UID (tmpl);
  return iterative_hash_template_arg (args, val);
}

/* Returns a hash for a spec_entry node based on the TMPL and ARGS members,
   ignoring SPEC.  */

static hashval_t
hash_specialization (const void *p)
{
  const spec_entry *e = (const spec_entry *)p;
  return hash_tmpl_and_args (e->tmpl, e->args);
}

/* Recursively calculate a hash value for a template argument ARG, for use
   in the hash tables of template specializations.  */

hashval_t
iterative_hash_template_arg (tree arg, hashval_t val)
{
  unsigned HOST_WIDE_INT i;
  enum tree_code code;
  char tclass;

  if (arg == NULL_TREE)
    return iterative_hash_object (arg, val);

  if (!TYPE_P (arg))
    STRIP_NOPS (arg);

  if (TREE_CODE (arg) == ARGUMENT_PACK_SELECT)
    /* We can get one of these when re-hashing a previous entry in the middle
       of substituting into a pack expansion.  Just look through it.  */
    arg = ARGUMENT_PACK_SELECT_FROM_PACK (arg);

  code = TREE_CODE (arg);
  tclass = TREE_CODE_CLASS (code);

  val = iterative_hash_object (code, val);

  switch (code)
    {
    case ERROR_MARK:
      return val;

    case IDENTIFIER_NODE:
      return iterative_hash_object (IDENTIFIER_HASH_VALUE (arg), val);

    case TREE_VEC:
      {
	int i, len = TREE_VEC_LENGTH (arg);
	for (i = 0; i < len; ++i)
	  val = iterative_hash_template_arg (TREE_VEC_ELT (arg, i), val);
	return val;
      }

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
      for (; arg; arg = OVL_NEXT (arg))
	val = iterative_hash_template_arg (OVL_CURRENT (arg), val);
      return val;

    case CONSTRUCTOR:
      {
	tree field, value;
	FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (arg), i, field, value)
	  {
	    val = iterative_hash_template_arg (field, val);
	    val = iterative_hash_template_arg (value, val);
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
      /* A lambda can't appear in a template arg, but don't crash on
	 erroneous input.  */
      gcc_assert (seen_error ());
      return val;

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

    default:
      break;
    }

  switch (tclass)
    {
    case tcc_type:
      if (TYPE_CANONICAL (arg))
	return iterative_hash_object (TYPE_HASH (TYPE_CANONICAL (arg)),
				      val);
      else if (TREE_CODE (arg) == DECLTYPE_TYPE)
	return iterative_hash_template_arg (DECLTYPE_TYPE_EXPR (arg), val);
      /* Otherwise just compare the types during lookup.  */
      return val;

    case tcc_declaration:
    case tcc_constant:
      return iterative_hash_expr (arg, val);

    default:
      gcc_assert (IS_EXPR_CODE_CLASS (tclass));
      {
	unsigned n = cp_tree_operand_length (arg);
	for (i = 0; i < n; ++i)
	  val = iterative_hash_template_arg (TREE_OPERAND (arg, i), val);
	return val;
      }
    }
  gcc_unreachable ();
  return 0;
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

  entry = (spec_entry *) htab_find (decl_specializations, &elt);
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

static void
register_local_specialization (tree spec, tree tmpl)
{
  void **slot;

  slot = pointer_map_insert (local_specializations, tmpl);
  *slot = spec;
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
   for each element of the list.  Alternatively, FNS can not be a
   TREE_LIST, in which case it will be printed together with all the
   overloads.

   MORE and *STR should respectively be FALSE and NULL when the function
   is called from the outside.  They are used internally on recursive
   calls.  print_candidates manages the two parameters and leaves NULL
   in *STR when it ends.  */

static void
print_candidates_1 (tree fns, bool more, const char **str)
{
  tree fn, fn2;
  char *spaces = NULL;

  for (fn = fns; fn; fn = OVL_NEXT (fn))
    if (TREE_CODE (fn) == TREE_LIST)
      {
        for (fn2 = fn; fn2 != NULL_TREE; fn2 = TREE_CHAIN (fn2))
          print_candidates_1 (TREE_VALUE (fn2),
                              TREE_CHAIN (fn2) || more, str);
      }
    else
      {
	tree cand = OVL_CURRENT (fn);
        if (!*str)
          {
            /* Pick the prefix string.  */
            if (!more && !OVL_NEXT (fns))
              {
                inform (DECL_SOURCE_LOCATION (cand),
			"candidate is: %#D", cand);
                continue;
              }

            *str = _("candidates are:");
            spaces = get_spaces (*str);
          }
        inform (DECL_SOURCE_LOCATION (cand), "%s %#D", *str, cand);
        *str = spaces ? spaces : *str;
      }

  if (!more)
    {
      free (spaces);
      *str = NULL;
    }
}

/* Print the list of candidate FNS in an error message.  FNS can also
   be a TREE_LIST of non-functions in the case of an ambiguous lookup.  */

void
print_candidates (tree fns)
{
  const char *str = NULL;
  print_candidates_1 (fns, false, &str);
  gcc_assert (str == NULL);
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
  if (template_count && DECL_CLASS_SCOPE_P (decl)
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

  if (!is_overloaded_fn (fns))
    {
      error ("%qD is not a function template", fns);
      return error_mark_node;
    }

  /* Count the number of template headers specified for this
     specialization.  */
  header_count = 0;
  for (b = current_binding_level;
       b->kind == sk_template_parms;
       b = b->level_chain)
    ++header_count;

  for (; fns; fns = OVL_NEXT (fns))
    {
      tree fn = OVL_CURRENT (fns);

      if (TREE_CODE (fn) == TEMPLATE_DECL)
	{
	  tree decl_arg_types;
	  tree fn_arg_types;
	  tree insttype;

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
	    continue;

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
	  if (DECL_NONSTATIC_MEMBER_FUNCTION_P (fn)
	      && !same_type_p (TREE_VALUE (fn_arg_types),
			       TREE_VALUE (decl_arg_types)))
	    continue;

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
	     match.  */
	  if (tsk == tsk_template)
	    {
	      if (compparms (fn_arg_types, decl_arg_types))
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

	  /* Make sure that the deduced arguments actually work.  */
	  insttype = tsubst (TREE_TYPE (fn), targs, tf_none, NULL_TREE);
	  if (insttype == error_mark_node)
	    continue;
	  fn_arg_types
	    = skip_artificial_parms_for (fn, TYPE_ARG_TYPES (insttype));
	  if (!compparms (fn_arg_types, decl_arg_types))
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

	  if (compparms (TYPE_ARG_TYPES (TREE_TYPE (fn)),
			 decl_arg_types))
	    /* They match!  */
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

  if (templates == NULL_TREE && candidates == NULL_TREE)
    {
      error ("template-id %qD for %q+D does not match any template "
	     "declaration", template_id, decl);
      if (header_count && header_count != template_count + 1)
	inform (input_location, "saw %d %<template<>%>, need %d for "
		"specializing a member function template",
		header_count, template_count + 1);
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
      /* DECL is a re-declaration or partial instantiation of a template
	 function.  */
      if (TREE_CODE (fn) == TEMPLATE_DECL)
	return fn;
      /* It was a specialization of an ordinary member function in a
	 template class.  */
      return DECL_TI_TEMPLATE (fn);
    }

  /* It was a specialization of a template.  */
  targs = DECL_TI_ARGS (DECL_TEMPLATE_RESULT (TREE_VALUE (templates)));
  if (TMPL_ARGS_HAVE_MULTIPLE_LEVELS (targs))
    {
      *targs_out = copy_node (targs);
      SET_TMPL_ARGS_LEVEL (*targs_out,
			   TMPL_ARGS_DEPTH (*targs_out),
			   TREE_PURPOSE (templates));
    }
  else
    *targs_out = TREE_PURPOSE (templates);
  return TREE_VALUE (templates);
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
  new_type = build_exception_variant (new_type,
				      TYPE_RAISES_EXCEPTIONS (old_type));
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
  if (!TYPE_P (ctx) || !CLASSTYPE_TEMPLATE_INFO (ctx))
    permerror (DECL_SOURCE_LOCATION (decl),
	       "%qD is not a static data member of a class template", decl);
  else if (template_header_count > wanted)
    {
      pedwarn (DECL_SOURCE_LOCATION (decl), 0,
	       "too many template headers for %D (should be %d)",
	       decl, wanted);
      if (CLASSTYPE_TEMPLATE_SPECIALIZATION (ctx))
	inform (DECL_SOURCE_LOCATION (decl),
		"members of an explicitly specialized class are defined "
		"without a template header");
    }
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
			       int flags)
{
  int have_def = flags & 2;
  int is_friend = flags & 4;
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
      if (processing_specialization)
	{
	  specialization = 1;
	  SET_DECL_TEMPLATE_SPECIALIZATION (decl);
	}
      else if (TREE_CODE (declarator) == TEMPLATE_ID_EXPR)
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

	      error ("template-id %qD in declaration of primary template",
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

	  if (uses_template_parms (declarator))
	    error ("function template partial specialization %qD "
		   "is not allowed", declarator);
	  else
	    error ("template-id %qD in declaration of primary template",
		   declarator);
	  return decl;
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

  if (specialization || member_specialization)
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

      /* Make sure that the declarator is a TEMPLATE_ID_EXPR.  */
      if (TREE_CODE (declarator) != TEMPLATE_ID_EXPR)
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
					   false, true);
	      if (fns == error_mark_node || !is_overloaded_fn (fns))
		{
		  error ("%qD is not a template function", dname);
		  fns = error_mark_node;
		}
	      else
		{
		  tree fn = OVL_CURRENT (fns);
		  if (!is_associated_namespace (CP_DECL_CONTEXT (decl),
						CP_DECL_CONTEXT (fn)))
		    error ("%qD is not declared in %qD",
			   decl, current_namespace);
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
	  /* Find the list of functions in ctype that have the same
	     name as the declared function.  */
	  tree name = TREE_OPERAND (declarator, 0);
	  tree fns = NULL_TREE;
	  int idx;

	  if (constructor_name_p (name, ctype))
	    {
	      int is_constructor = DECL_CONSTRUCTOR_P (decl);

	      if (is_constructor ? !TYPE_HAS_USER_CONSTRUCTOR (ctype)
		  : !CLASSTYPE_DESTRUCTORS (ctype))
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

	      name = is_constructor ? ctor_identifier : dtor_identifier;
	    }

	  if (!DECL_CONV_FN_P (decl))
	    {
	      idx = lookup_fnfields_1 (ctype, name);
	      if (idx >= 0)
		fns = (*CLASSTYPE_METHOD_VEC (ctype))[idx];
	    }
	  else
	    {
	      vec<tree, va_gc> *methods;
	      tree ovl;

	      /* For a type-conversion operator, we cannot do a
		 name-based lookup.  We might be looking for `operator
		 int' which will be a specialization of `operator T'.
		 So, we find *all* the conversion operators, and then
		 select from them.  */
	      fns = NULL_TREE;

	      methods = CLASSTYPE_METHOD_VEC (ctype);
	      if (methods)
		for (idx = CLASSTYPE_FIRST_CONVERSION_SLOT;
		     methods->iterate (idx, &ovl);
		     ++idx)
		  {
		    if (!DECL_CONV_FN_P (OVL_CURRENT (ovl)))
		      /* There are no more conversion functions.  */
		      break;

		    /* Glue all these conversion functions together
		       with those we already have.  */
		    for (; ovl; ovl = OVL_NEXT (ovl))
		      fns = ovl_cons (OVL_CURRENT (ovl), fns);
		  }
	    }

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
	 member function, we cannot tell apriori whether the
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
	  if (DECL_STATIC_FUNCTION_P (tmpl)
	      && DECL_NONSTATIC_MEMBER_FUNCTION_P (decl))
	    revert_static_member_fn (decl);

	  /* If this is a specialization of a member template of a
	     template class, we want to return the TEMPLATE_DECL, not
	     the specialization of it.  */
	  if (tsk == tsk_template)
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
	      return register_specialization (tmpl, gen_tmpl, targs,
					      is_friend, 0);
	    }

	  /* Set up the DECL_TEMPLATE_INFO for DECL.  */
	  DECL_TEMPLATE_INFO (decl) = build_template_info (tmpl, targs);

	  /* Inherit default function arguments from the template
	     DECL is specializing.  */
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

	  /* Register this specialization so that we can find it
	     again.  */
	  decl = register_specialization (decl, gen_tmpl, targs, is_friend, 0);

	  /* A 'structor should already have clones.  */
	  gcc_assert (decl == error_mark_node
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
          if (parm1 == error_mark_node || parm2 == error_mark_node)
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
      || ! primary_template_instantiation_p (primary_func_tmpl_inst))
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
   instantiation.  */

bool
primary_template_instantiation_p (const_tree t)
{
  if (!t)
    return false;

  if (TREE_CODE (t) == FUNCTION_DECL)
    return DECL_LANG_SPECIFIC (t)
	   && DECL_TEMPLATE_INSTANTIATION (t)
	   && PRIMARY_TEMPLATE_P (DECL_TI_TEMPLATE (t));
  else if (CLASS_TYPE_P (t) && !TYPE_DECL_ALIAS_P (TYPE_NAME (t)))
    return CLASSTYPE_TEMPLATE_INSTANTIATION (t)
	   && PRIMARY_TEMPLATE_P (CLASSTYPE_TI_TEMPLATE (t));
  else if (alias_template_specialization_p (t))
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
      && primary_template_instantiation_p (t))
    parms = INNERMOST_TEMPLATE_PARMS
	(DECL_TEMPLATE_PARMS (TI_TEMPLATE (template_info)));

  return parms;
}

/* Return the template parameters of the LEVELth level from the full list
   of template parameters PARMS.  */

tree
get_template_parms_at_level (tree parms, int level)
{
  tree p;
  if (!parms
      || TREE_CODE (parms) != TREE_LIST
      || level > TMPL_PARMS_DEPTH (parms))
    return NULL_TREE;

  for (p = parms; p; p = TREE_CHAIN (p))
    if (TMPL_PARMS_DEPTH (p) == level)
      return p;

  return NULL_TREE;
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

/* Structure used to track the progress of find_parameter_packs_r.  */
struct find_parameter_pack_data 
{
  /* TREE_LIST that will contain all of the parameter packs found by
     the traversal.  */
  tree* parameter_packs;

  /* Set of AST nodes that have been visited by the traversal.  */
  struct pointer_set_t *visited;
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

  /* Handle type aliases/typedefs.  */
  if (TYPE_ALIAS_P (t))
    {
      if (TYPE_TEMPLATE_INFO (t))
	cp_walk_tree (&TYPE_TI_ARGS (t),
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
    case TEMPLATE_TEMPLATE_PARM:
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

      /* Look through a lambda capture proxy to the field pack.  */
    case VAR_DECL:
      if (DECL_HAS_VALUE_EXPR_P (t))
	{
	  tree v = DECL_VALUE_EXPR (t);
	  cp_walk_tree (&v,
			&find_parameter_packs_r,
			ppd, ppd->visited);
	  *walk_subtrees = 0;
	}
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

  if (TYPE_P (t))
    cp_walk_tree (&TYPE_CONTEXT (t), 
		  &find_parameter_packs_r, ppd, ppd->visited);

  /* This switch statement will return immediately if we don't find a
     parameter pack.  */
  switch (TREE_CODE (t)) 
    {
    case TEMPLATE_PARM_INDEX:
      return NULL_TREE;

    case BOUND_TEMPLATE_TEMPLATE_PARM:
      /* Check the template itself.  */
      cp_walk_tree (&TREE_TYPE (TYPE_TI_TEMPLATE (t)), 
		    &find_parameter_packs_r, ppd, ppd->visited);
      /* Check the template arguments.  */
      cp_walk_tree (&TYPE_TI_ARGS (t), &find_parameter_packs_r, ppd, 
		    ppd->visited);
      *walk_subtrees = 0;
      return NULL_TREE;

    case TEMPLATE_TYPE_PARM:
    case TEMPLATE_TEMPLATE_PARM:
      return NULL_TREE;

    case PARM_DECL:
      return NULL_TREE;

    case RECORD_TYPE:
      if (TYPE_PTRMEMFUNC_P (t))
	return NULL_TREE;
      /* Fall through.  */

    case UNION_TYPE:
    case ENUMERAL_TYPE:
      if (TYPE_TEMPLATE_INFO (t))
	cp_walk_tree (&TYPE_TI_ARGS (t),
		      &find_parameter_packs_r, ppd, ppd->visited);

      *walk_subtrees = 0;
      return NULL_TREE;

    case CONSTRUCTOR:
    case TEMPLATE_DECL:
      cp_walk_tree (&TREE_TYPE (t),
		    &find_parameter_packs_r, ppd, ppd->visited);
      return NULL_TREE;
 
    case TYPENAME_TYPE:
      cp_walk_tree (&TYPENAME_TYPE_FULLNAME (t), &find_parameter_packs_r,
                   ppd, ppd->visited);
      *walk_subtrees = 0;
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

    default:
      return NULL_TREE;
    }

  return NULL_TREE;
}

/* Determines if the expression or type T uses any parameter packs.  */
bool
uses_parameter_packs (tree t)
{
  tree parameter_packs = NULL_TREE;
  struct find_parameter_pack_data ppd;
  ppd.parameter_packs = &parameter_packs;
  ppd.visited = pointer_set_create ();
  cp_walk_tree (&t, &find_parameter_packs_r, &ppd, ppd.visited);
  pointer_set_destroy (ppd.visited);
  return parameter_packs != NULL_TREE;
}

/* Turn ARG, which may be an expression, type, or a TREE_LIST
   representation a base-class initializer into a parameter pack
   expansion. If all goes well, the resulting node will be an
   EXPR_PACK_EXPANSION, TYPE_PACK_EXPANSION, or TREE_LIST,
   respectively.  */
tree 
make_pack_expansion (tree arg)
{
  tree result;
  tree parameter_packs = NULL_TREE;
  bool for_types = false;
  struct find_parameter_pack_data ppd;

  if (!arg || arg == error_mark_node)
    return arg;

  if (TREE_CODE (arg) == TREE_LIST)
    {
      /* The only time we will see a TREE_LIST here is for a base
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
      ppd.visited = pointer_set_create ();
      ppd.parameter_packs = &parameter_packs;
      cp_walk_tree (&TREE_PURPOSE (arg), &find_parameter_packs_r, 
                    &ppd, ppd.visited);

      if (parameter_packs == NULL_TREE)
        {
          error ("base initializer expansion %<%T%> contains no parameter packs", arg);
          pointer_set_destroy (ppd.visited);
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

      pointer_set_destroy (ppd.visited);

      /* Create the pack expansion type for the base type.  */
      purpose = cxx_make_type (TYPE_PACK_EXPANSION);
      SET_PACK_EXPANSION_PATTERN (purpose, TREE_PURPOSE (arg));
      PACK_EXPANSION_PARAMETER_PACKS (purpose) = parameter_packs;

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
  SET_PACK_EXPANSION_PATTERN (result, arg);
  if (TREE_CODE (result) == EXPR_PACK_EXPANSION)
    {
      /* Propagate type and const-expression information.  */
      TREE_TYPE (result) = TREE_TYPE (arg);
      TREE_CONSTANT (result) = TREE_CONSTANT (arg);
    }
  else
    /* Just use structural equality for these TYPE_PACK_EXPANSIONS;
       they will rarely be compared to anything.  */
    SET_TYPE_STRUCTURAL_EQUALITY (result);

  /* Determine which parameter packs will be expanded.  */
  ppd.parameter_packs = &parameter_packs;
  ppd.visited = pointer_set_create ();
  cp_walk_tree (&arg, &find_parameter_packs_r, &ppd, ppd.visited);
  pointer_set_destroy (ppd.visited);

  /* Make sure we found some parameter packs.  */
  if (parameter_packs == NULL_TREE)
    {
      if (TYPE_P (arg))
        error ("expansion pattern %<%T%> contains no argument packs", arg);
      else
        error ("expansion pattern %<%E%> contains no argument packs", arg);
      return error_mark_node;
    }
  PACK_EXPANSION_PARAMETER_PACKS (result) = parameter_packs;

  PACK_EXPANSION_LOCAL_P (result) = at_function_scope_p ();

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
check_for_bare_parameter_packs (tree t)
{
  tree parameter_packs = NULL_TREE;
  struct find_parameter_pack_data ppd;

  if (!processing_template_decl || !t || t == error_mark_node)
    return false;

  if (TREE_CODE (t) == TYPE_DECL)
    t = TREE_TYPE (t);

  ppd.parameter_packs = &parameter_packs;
  ppd.visited = pointer_set_create ();
  cp_walk_tree (&t, &find_parameter_packs_r, &ppd, ppd.visited);
  pointer_set_destroy (ppd.visited);

  if (parameter_packs) 
    {
      error ("parameter packs not expanded with %<...%>:");
      while (parameter_packs)
        {
          tree pack = TREE_VALUE (parameter_packs);
          tree name = NULL_TREE;

          if (TREE_CODE (pack) == TEMPLATE_TYPE_PARM
              || TREE_CODE (pack) == TEMPLATE_TEMPLATE_PARM)
            name = TYPE_NAME (pack);
          else if (TREE_CODE (pack) == TEMPLATE_PARM_INDEX)
            name = DECL_NAME (TEMPLATE_PARM_DECL (pack));
          else
            name = DECL_NAME (pack);

	  if (name)
	    inform (input_location, "        %qD", name);
	  else
	    inform (input_location, "        <anonymous>");

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
  if (TREE_CODE (decl) == OVERLOAD)
    decl = OVL_CURRENT (decl);
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

  error ("declaration of %q+#D", decl);
  error (" shadows template parm %q+#D", olddecl);
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

/* Find the canonical type parameter for the given template type
   parameter.  Returns the canonical type parameter, which may be TYPE
   if no such parameter existed.  */

static tree
canonical_type_parameter (tree type)
{
  tree list;
  int idx = TEMPLATE_TYPE_IDX (type);
  if (!canonical_template_parms)
    vec_alloc (canonical_template_parms, idx+1);

  while (canonical_template_parms->length () <= (unsigned)idx)
    vec_safe_push (canonical_template_parms, NULL_TREE);

  list = (*canonical_template_parms)[idx];
  while (list && !comptypes (type, TREE_VALUE (list), COMPARE_STRUCTURAL))
    list = TREE_CHAIN (list);

  if (list)
    return TREE_VALUE (list);
  else
    {
      (*canonical_template_parms)[idx]
		= tree_cons (NULL_TREE, type,
			     (*canonical_template_parms)[idx]);
      return type;
    }
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
      tree decl, t;

      decl = build_decl (DECL_SOURCE_LOCATION (orig_decl),
			 TREE_CODE (orig_decl), DECL_NAME (orig_decl), type);
      TREE_CONSTANT (decl) = TREE_CONSTANT (orig_decl);
      TREE_READONLY (decl) = TREE_READONLY (orig_decl);
      DECL_ARTIFICIAL (decl) = 1;
      SET_DECL_TEMPLATE_PARM_P (decl);

      t = build_template_parm_index (TEMPLATE_PARM_IDX (index),
				     TEMPLATE_PARM_LEVEL (index) - levels,
				     TEMPLATE_PARM_ORIG_LEVEL (index),
				     decl, type);
      TEMPLATE_PARM_DESCENDANTS (index) = t;
      TEMPLATE_PARM_PARAMETER_PACK (t) 
	= TEMPLATE_PARM_PARAMETER_PACK (index);

	/* Template template parameters need this.  */
      if (TREE_CODE (decl) == TEMPLATE_DECL)
	DECL_TEMPLATE_PARMS (decl) = tsubst_template_parms
	  (DECL_TEMPLATE_PARMS (TEMPLATE_PARM_DECL (index)),
	   args, complain);
    }

  return TEMPLATE_PARM_DESCENDANTS (index);
}

/* Process information from new template parameter PARM and append it
   to the LIST being built.  This new parameter is a non-type
   parameter iff IS_NON_TYPE is true. This new parameter is a
   parameter pack iff IS_PARAMETER_PACK is true.  The location of PARM
   is in PARM_LOC. NUM_TEMPLATE_PARMS is the size of the template
   parameter list PARM belongs to. This is used used to create a
   proper canonical type for the type of PARM that is to be created,
   iff PARM is a type.  If the size is not known, this parameter shall
   be set to 0.  */

tree
process_template_parm (tree list, location_t parm_loc, tree parm,
		       bool is_non_type, bool is_parameter_pack)
{
  tree decl = 0;
  tree defval;
  tree err_parm_list;
  int idx = 0;

  gcc_assert (TREE_CODE (parm) == TREE_LIST);
  defval = TREE_PURPOSE (parm);

  if (list)
    {
      tree p = tree_last (list);

      if (p && TREE_VALUE (p) != error_mark_node)
        {
          p = TREE_VALUE (p);
          if (TREE_CODE (p) == TYPE_DECL || TREE_CODE (p) == TEMPLATE_DECL)
            idx = TEMPLATE_TYPE_IDX (TREE_TYPE (p));
          else
            idx = TEMPLATE_PARM_IDX (DECL_INITIAL (p));
        }

      ++idx;
    }
  else
    idx = 0;

  if (is_non_type)
    {
      parm = TREE_VALUE (parm);

      SET_DECL_TEMPLATE_PARM_P (parm);

      if (TREE_TYPE (parm) == error_mark_node)
        {
          err_parm_list = build_tree_list (defval, parm);
          TREE_VALUE (err_parm_list) = error_mark_node;
	   return chainon (list, err_parm_list);
        }
      else
      {
	/* [temp.param]

	   The top-level cv-qualifiers on the template-parameter are
	   ignored when determining its type.  */
	TREE_TYPE (parm) = TYPE_MAIN_VARIANT (TREE_TYPE (parm));
	if (invalid_nontype_parm_type_p (TREE_TYPE (parm), 1))
          {
            err_parm_list = build_tree_list (defval, parm);
            TREE_VALUE (err_parm_list) = error_mark_node;
	     return chainon (list, err_parm_list);
          }

        if (uses_parameter_packs (TREE_TYPE (parm)) && !is_parameter_pack
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
	= build_template_parm_index (idx, processing_template_decl,
				     processing_template_decl,
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
	  TREE_TYPE (DECL_TEMPLATE_RESULT (parm)) = t;
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
	= build_template_parm_index (idx, processing_template_decl,
				     processing_template_decl,
				     decl, TREE_TYPE (parm));
      TEMPLATE_TYPE_PARAMETER_PACK (t) = is_parameter_pack;
      TYPE_CANONICAL (t) = canonical_type_parameter (t);
    }
  DECL_ARTIFICIAL (decl) = 1;
  SET_DECL_TEMPLATE_PARM_P (decl);
  pushdecl (decl);
  parm = build_tree_list (defval, parm);
  return chainon (list, parm);
}

/* The end of a template parameter list has been reached.  Process the
   tree list into a parameter vector, converting each parameter into a more
   useful form.	 Type parameters are saved as IDENTIFIER_NODEs, and others
   as PARM_DECLs.  */

tree
end_template_parm_list (tree parms)
{
  int nparms;
  tree parm, next;
  tree saved_parmlist = make_tree_vec (list_length (parms));

  current_template_parms
    = tree_cons (size_int (processing_template_decl),
		 saved_parmlist, current_template_parms);

  for (parm = parms, nparms = 0; parm; parm = next, nparms++)
    {
      next = TREE_CHAIN (parm);
      TREE_VEC_ELT (saved_parmlist, nparms) = parm;
      TREE_CHAIN (parm) = NULL_TREE;
    }

  --processing_template_parmlist;

  return saved_parmlist;
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

/* Takes a TREE_LIST representing a template parameter and convert it
   into an argument suitable to be passed to the type substitution
   functions.  Note that If the TREE_LIST contains an error_mark
   node, the returned argument is error_mark_node.  */

static tree
template_parm_to_arg (tree t)
{

  if (t == NULL_TREE
      || TREE_CODE (t) != TREE_LIST)
    return t;

  if (error_operand_p (TREE_VALUE (t)))
    return error_mark_node;

  t = TREE_VALUE (t);

  if (TREE_CODE (t) == TYPE_DECL
      || TREE_CODE (t) == TEMPLATE_DECL)
    {
      t = TREE_TYPE (t);

      if (TEMPLATE_TYPE_PARAMETER_PACK (t))
	{
	  /* Turn this argument into a TYPE_ARGUMENT_PACK
	     with a single element, which expands T.  */
	  tree vec = make_tree_vec (1);
#ifdef ENABLE_CHECKING
	  SET_NON_DEFAULT_TEMPLATE_ARGS_COUNT
	    (vec, TREE_VEC_LENGTH (vec));
#endif
	  TREE_VEC_ELT (vec, 0) = make_pack_expansion (t);

	  t = cxx_make_type (TYPE_ARGUMENT_PACK);
	  SET_ARGUMENT_PACK_ARGS (t, vec);
	}
    }
  else
    {
      t = DECL_INITIAL (t);

      if (TEMPLATE_PARM_PARAMETER_PACK (t))
	{
	  /* Turn this argument into a NONTYPE_ARGUMENT_PACK
	     with a single element, which expands T.  */
	  tree vec = make_tree_vec (1);
	  tree type = TREE_TYPE (TEMPLATE_PARM_DECL (t));
#ifdef ENABLE_CHECKING
	  SET_NON_DEFAULT_TEMPLATE_ARGS_COUNT
	    (vec, TREE_VEC_LENGTH (vec));
#endif
	  t = convert_from_reference (t);
	  TREE_VEC_ELT (vec, 0) = make_pack_expansion (t);

	  t  = make_node (NONTYPE_ARGUMENT_PACK);
	  SET_ARGUMENT_PACK_ARGS (t, vec);
	  TREE_TYPE (t) = type;
	}
      else
	t = convert_from_reference (t);
    }
  return t;
}

/* Given a set of template parameters, return them as a set of template
   arguments.  The template parameters are represented as a TREE_VEC, in
   the form documented in cp-tree.h for template arguments.  */

static tree
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
      tree a = copy_node (TREE_VALUE (header));
      int i;

      TREE_TYPE (a) = NULL_TREE;
      for (i = TREE_VEC_LENGTH (a) - 1; i >= 0; --i)
	TREE_VEC_ELT (a, i) = template_parm_to_arg (TREE_VEC_ELT (a, i));

#ifdef ENABLE_CHECKING
      SET_NON_DEFAULT_TEMPLATE_ARGS_COUNT (a, TREE_VEC_LENGTH (a));
#endif

      if (length > 1)
	TREE_VEC_ELT (args, --l) = a;
      else
	args = a;
    }

    if (length > 1 && TREE_VEC_ELT (args, 0) == NULL_TREE)
      /* This can happen for template parms of a template template
	 parameter, e.g:

	 template<template<class T, class U> class TT> struct S;

	 Consider the level of the parms of TT; T and U both have
	 level 2; TT has no template parm of level 1. So in this case
	 the first element of full_template_args is NULL_TREE. If we
	 leave it like this TMPL_ARGS_DEPTH on args returns 1 instead
	 of 2. This will make tsubst wrongly consider that T and U
	 have level 1. Instead, let's create a dummy vector as the
	 first element of full_template_args so that TMPL_ARGS_DEPTH
	 returns the correct depth for args.  */
      TREE_VEC_ELT (args, 0) = make_tree_vec (1);
  return args;
}

/* Within the declaration of a template, return the currently active
   template parameters as an argument TREE_VEC.  */

static tree
current_template_args (void)
{
  return template_parms_to_args (current_template_parms);
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
   template PARMS.  If MEMBER_TEMPLATE_P is true, the new template is
   a member template.  Used by push_template_decl below.  */

static tree
build_template_decl (tree decl, tree parms, bool member_template_p)
{
  tree tmpl = build_lang_decl (TEMPLATE_DECL, DECL_NAME (decl), NULL_TREE);
  DECL_TEMPLATE_PARMS (tmpl) = parms;
  DECL_CONTEXT (tmpl) = DECL_CONTEXT (decl);
  DECL_SOURCE_LOCATION (tmpl) = DECL_SOURCE_LOCATION (decl);
  DECL_MEMBER_TEMPLATE_P (tmpl) = member_template_p;

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

  if (TREE_CODE (t) == TEMPLATE_PARM_INDEX)
    {
      level = TEMPLATE_PARM_LEVEL (t);
      idx = TEMPLATE_PARM_IDX (t);
    }
  else
    {
      level = TEMPLATE_TYPE_LEVEL (t);
      idx = TEMPLATE_TYPE_IDX (t);
    }

  if (level == tpd->level)
    {
      tpd->parms[idx] = 1;
      tpd->arg_uses_template_parms[tpd->current_arg] = 1;
    }

  /* Return zero so that for_each_template_parm will continue the
     traversal of the tree; we want to mark *every* template parm.  */
  return 0;
}

/* Process the partial specialization DECL.  */

static tree
process_partial_specialization (tree decl)
{
  tree type = TREE_TYPE (decl);
  tree maintmpl = CLASSTYPE_TI_TEMPLATE (type);
  tree specargs = CLASSTYPE_TI_ARGS (type);
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
	/* One of the template parms was not used in the
	   specialization.  */
	if (!did_error_intro)
	  {
	    error ("template parameters not used in partial specialization:");
	    did_error_intro = true;
	  }

	error ("        %qD", TREE_VALUE (TREE_VEC_ELT (inner_parms, i)));
      }

  if (did_error_intro)
    return error_mark_node;

  /* [temp.class.spec]

     The argument list of the specialization shall not be identical to
     the implicit argument list of the primary template.  */
  if (comp_template_args
      (inner_args,
       INNERMOST_TEMPLATE_ARGS (CLASSTYPE_TI_ARGS (TREE_TYPE
						   (maintmpl)))))
    error ("partial specialization %qT does not specialize any template arguments", type);

  /* A partial specialization that replaces multiple parameters of the
     primary template with a pack expansion is less specialized for those
     parameters.  */
  if (nargs < DECL_NTPARMS (maintmpl))
    {
      error ("partial specialization is not more specialized than the "
	     "primary template because it replaces multiple parameters "
	     "with a pack expansion");
      inform (DECL_SOURCE_LOCATION (maintmpl), "primary template here");
      return decl;
    }

  /* [temp.class.spec]

     A partially specialized non-type argument expression shall not
     involve template parameters of the partial specialization except
     when the argument expression is a simple identifier.

     The type of a template parameter corresponding to a specialized
     non-type argument shall not be dependent on a parameter of the
     specialization. 

     Also, we verify that pack expansions only occur at the
     end of the argument list.  */
  gcc_assert (nargs == DECL_NTPARMS (maintmpl));
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
	      && !(REFERENCE_REF_P (arg)
		   && TREE_CODE (TREE_OPERAND (arg, 0)) == TEMPLATE_PARM_INDEX))
            {
              if ((!packed_args && tpd.arg_uses_template_parms[i])
                  || (packed_args && uses_template_parms (arg)))
                error ("template argument %qE involves template parameter(s)",
                       arg);
              else 
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
    }

  /* We should only get here once.  */
  gcc_assert (!COMPLETE_TYPE_P (type));

  tree tmpl = build_template_decl (decl, current_template_parms,
				   DECL_MEMBER_TEMPLATE_P (maintmpl));
  TREE_TYPE (tmpl) = type;
  DECL_TEMPLATE_RESULT (tmpl) = decl;
  SET_DECL_TEMPLATE_SPECIALIZATION (tmpl);
  DECL_TEMPLATE_INFO (tmpl) = build_template_info (maintmpl, specargs);
  DECL_PRIMARY_TEMPLATE (tmpl) = maintmpl;

  DECL_TEMPLATE_SPECIALIZATIONS (maintmpl)
    = tree_cons (specargs, tmpl,
                 DECL_TEMPLATE_SPECIALIZATIONS (maintmpl));
  TREE_TYPE (DECL_TEMPLATE_SPECIALIZATIONS (maintmpl)) = type;

  for (inst = DECL_TEMPLATE_INSTANTIATIONS (maintmpl); inst;
       inst = TREE_CHAIN (inst))
    {
      tree inst_type = TREE_VALUE (inst);
      if (COMPLETE_TYPE_P (inst_type)
	  && CLASSTYPE_IMPLICIT_INSTANTIATION (inst_type))
	{
	  tree spec = most_specialized_class (inst_type, tf_none);
	  if (spec && TREE_TYPE (spec) == type)
	    permerror (input_location,
		       "partial specialization of %qT after instantiation "
		       "of %qT", type, inst_type);
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
  if (TREE_CODE (parm) == TYPE_DECL)
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
    fixed_parameter_pack_p_1 (TREE_VALUE (TREE_VEC_ELT (vec, i)), ppd);
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
  ppd.visited = pointer_set_create ();

  fixed_parameter_pack_p_1 (parm, &ppd);

  pointer_set_destroy (ppd.visited);
  return parameter_packs;
}

/* Check that a template declaration's use of default arguments and
   parameter packs is not invalid.  Here, PARMS are the template
   parameters.  IS_PRIMARY is true if DECL is the thing declared by
   a primary template.  IS_PARTIAL is true if DECL is a partial
   specialization.

   IS_FRIEND_DECL is nonzero if DECL is a friend function template
   declaration (but not a definition); 1 indicates a declaration, 2
   indicates a redeclaration. When IS_FRIEND_DECL=2, no errors are
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

  if (TREE_CODE (CP_DECL_CONTEXT (decl)) == FUNCTION_DECL)
    /* You can't have a function template declaration in a local
       scope, nor you can you define a member of a class template in a
       local scope.  */
    return true;

  if (TREE_CODE (decl) == TYPE_DECL
      && TREE_TYPE (decl)
      && LAMBDA_TYPE_P (TREE_TYPE (decl)))
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
		       && TREE_CODE (decl) == TYPE_DECL
		       && i < ntparms - 1
		       && template_parameter_pack_p (TREE_VALUE (parm))
		       /* A fixed parameter pack will be partially
			  instantiated into a fixed length list.  */
		       && !fixed_parameter_pack_p (TREE_VALUE (parm)))
		{
		  /* A primary class template can only have one
		     parameter pack, at the end of the template
		     parameter list.  */

		  if (TREE_CODE (TREE_VALUE (parm)) == PARM_DECL)
		    error ("parameter pack %qE must be at the end of the"
			   " template parameter list", TREE_VALUE (parm));
		  else
		    error ("parameter pack %qT must be at the end of the"
			   " template parameter list", 
			   TREE_TYPE (TREE_VALUE (parm)));

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
    msg = G_("default template arguments may not be used in function template "
	     "friend declarations");
  else if (TREE_CODE (decl) == FUNCTION_DECL && (cxx_dialect == cxx98))
    msg = G_("default template arguments may not be used in function templates "
	     "without -std=c++11 or -std=gnu++11");
  else if (is_partial)
    msg = G_("default template arguments may not be used in "
	     "partial specializations");
  else
    msg = G_("default argument for template parameter for class enclosing %qD");

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

/* Creates a TEMPLATE_DECL for the indicated DECL using the template
   parameters given by current_template_args, or reuses a
   previously existing one, if appropriate.  Returns the DECL, or an
   equivalent one, if it is replaced via a call to duplicate_decls.

   If IS_FRIEND is true, DECL is a friend declaration.  */

tree
push_template_decl_real (tree decl, bool is_friend)
{
  tree tmpl;
  tree args;
  tree info;
  tree ctx;
  bool is_primary;
  bool is_partial;
  int new_template_p = 0;
  /* True if the template is a member template, in the sense of
     [temp.mem].  */
  bool member_template_p = false;

  if (decl == error_mark_node || !current_template_parms)
    return error_mark_node;

  /* See if this is a partial specialization.  */
  is_partial = (DECL_IMPLICIT_TYPEDEF_P (decl)
		&& TREE_CODE (TREE_TYPE (decl)) != ENUMERAL_TYPE
		&& CLASSTYPE_TEMPLATE_SPECIALIZATION (TREE_TYPE (decl)));

  if (TREE_CODE (decl) == FUNCTION_DECL && DECL_FRIEND_P (decl))
    is_friend = true;

  if (is_friend)
    /* For a friend, we want the context of the friend function, not
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
  if (is_friend && ctx
      && uses_template_parms_level (ctx, processing_template_decl))
    /* A friend template that specifies a class context, i.e.
         template <typename T> friend void A<T>::f();
       is not primary.  */
    is_primary = false;
  else
    is_primary = template_parm_scope_p ();

  if (is_primary)
    {
      if (DECL_CLASS_SCOPE_P (decl))
	member_template_p = true;
      if (TREE_CODE (decl) == TYPE_DECL
	  && ANON_AGGRNAME_P (DECL_NAME (decl)))
	{
	  error ("template class without a name");
	  return error_mark_node;
	}
      else if (TREE_CODE (decl) == FUNCTION_DECL)
	{
	  if (DECL_DESTRUCTOR_P (decl))
	    {
	      /* [temp.mem]

		 A destructor shall not be a member template.  */
	      error ("destructor %qD declared as member template", decl);
	      return error_mark_node;
	    }
	  if (NEW_DELETE_OPNAME_P (DECL_NAME (decl))
	      && (!prototype_p (TREE_TYPE (decl))
		  || TYPE_ARG_TYPES (TREE_TYPE (decl)) == void_list_node
		  || !TREE_CHAIN (TYPE_ARG_TYPES (TREE_TYPE (decl)))
		  || (TREE_CHAIN (TYPE_ARG_TYPES ((TREE_TYPE (decl))))
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
	/* OK */;
      else if (TREE_CODE (decl) == TYPE_DECL
	       && TYPE_DECL_ALIAS_P (decl))
	/* alias-declaration */
	gcc_assert (!DECL_ARTIFICIAL (decl));
      else
	{
	  error ("template declaration of %q#D", decl);
	  return error_mark_node;
	}
    }

  /* Check to see that the rules regarding the use of default
     arguments are not being violated.  */
  check_default_tmpl_args (decl, current_template_parms,
			   is_primary, is_partial, /*is_friend_decl=*/0);

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
  else if (check_for_bare_parameter_packs ((TREE_CODE (decl) == TYPE_DECL
					    && TYPE_DECL_ALIAS_P (decl))
					   ? DECL_ORIGINAL_TYPE (decl)
					   : TREE_TYPE (decl)))
    {
      TREE_TYPE (decl) = error_mark_node;
      return error_mark_node;
    }

  if (is_partial)
    return process_partial_specialization (decl);

  args = current_template_args ();

  if (!ctx
      || TREE_CODE (ctx) == FUNCTION_DECL
      || (CLASS_TYPE_P (ctx) && TYPE_BEING_DEFINED (ctx))
      || (TREE_CODE (decl) == TYPE_DECL
	  && LAMBDA_TYPE_P (TREE_TYPE (decl)))
      || (is_friend && !DECL_TEMPLATE_INFO (decl)))
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
				    current_template_parms);
	  /* We don't need to create a new TEMPLATE_DECL; just use the
	     one we already had.  */
	  tmpl = TYPE_TI_TEMPLATE (TREE_TYPE (decl));
	}
      else
	{
	  tmpl = build_template_decl (decl, current_template_parms,
				      member_template_p);
	  new_template_p = 1;

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
	  tree new_tmpl;

	  /* The declaration is a specialization of a member
	     template, declared outside the class.  Therefore, the
	     innermost template arguments will be NULL, so we
	     replace them with the arguments determined by the
	     earlier call to check_explicit_specialization.  */
	  args = DECL_TI_ARGS (decl);

	  new_tmpl
	    = build_template_decl (decl, current_template_parms,
				   member_template_p);
	  DECL_TEMPLATE_RESULT (new_tmpl) = decl;
	  TREE_TYPE (new_tmpl) = TREE_TYPE (decl);
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
	  error ("\
template arguments to %qD do not match original template %qD",
		 decl, DECL_TEMPLATE_RESULT (tmpl));
	  if (!uses_template_parms (TI_ARGS (tinfo)))
	    inform (input_location, "use template<> for an explicit specialization");
	  /* Avoid crash in import_export_decl.  */
	  DECL_INTERFACE_KNOWN (decl) = 1;
	  return error_mark_node;
	}
    }

  DECL_TEMPLATE_RESULT (tmpl) = decl;
  TREE_TYPE (tmpl) = TREE_TYPE (decl);

  /* Push template declarations for global functions and types.  Note
     that we do not try to push a global template friend declared in a
     template class; such a thing may well depend on the template
     parameters of the class.  */
  if (new_template_p && !ctx
      && !(is_friend && template_class_depth (current_class_type) > 0))
    {
      tmpl = pushdecl_namespace_level (tmpl, is_friend);
      if (tmpl == error_mark_node)
	return error_mark_node;

      /* Hide template friend classes that haven't been declared yet.  */
      if (is_friend && TREE_CODE (decl) == TYPE_DECL)
	{
	  DECL_ANTICIPATED (tmpl) = 1;
	  DECL_FRIEND_P (tmpl) = 1;
	}
    }

  if (is_primary)
    {
      tree parms = DECL_TEMPLATE_PARMS (tmpl);
      int i;

      DECL_PRIMARY_TEMPLATE (tmpl) = tmpl;
      if (DECL_CONV_FN_P (tmpl))
	{
	  int depth = TMPL_PARMS_DEPTH (parms);

	  /* It is a conversion operator. See if the type converted to
	     depends on innermost template operands.  */

	  if (uses_template_parms_level (TREE_TYPE (TREE_TYPE (tmpl)),
					 depth))
	    DECL_TEMPLATE_CONV_FN_P (tmpl) = 1;
	}

      /* Give template template parms a DECL_CONTEXT of the template
	 for which they are a parameter.  */
      parms = INNERMOST_TEMPLATE_PARMS (parms);
      for (i = TREE_VEC_LENGTH (parms) - 1; i >= 0; --i)
	{
	  tree parm = TREE_VALUE (TREE_VEC_ELT (parms, i));
	  if (TREE_CODE (parm) == TEMPLATE_DECL)
	    DECL_CONTEXT (parm) = tmpl;
	}
    }

  /* The DECL_TI_ARGS of DECL contains full set of arguments referring
     back to its most general template.  If TMPL is a specialization,
     ARGS may only have the innermost set of arguments.  Add the missing
     argument levels if necessary.  */
  if (DECL_TEMPLATE_INFO (tmpl))
    args = add_outermost_template_args (DECL_TI_ARGS (tmpl), args);

  info = build_template_info (tmpl, args);

  if (DECL_IMPLICIT_TYPEDEF_P (decl))
    SET_TYPE_TEMPLATE_INFO (TREE_TYPE (tmpl), info);
  else
    {
      if (is_primary && !DECL_LANG_SPECIFIC (decl))
	retrofit_lang_decl (decl);
      if (DECL_LANG_SPECIFIC (decl))
	DECL_TEMPLATE_INFO (decl) = info;
    }

  return DECL_TEMPLATE_RESULT (tmpl);
}

tree
push_template_decl (tree decl)
{
  return push_template_decl_real (decl, false);
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
    = tree_cons (size_int (processing_template_decl + 1),
		 inner_parms, current_template_parms);
  tree tmpl = build_template_decl (fn, parms, /*member*/true);
  tree args = template_parms_to_args (parms);
  DECL_TEMPLATE_INFO (fn) = build_template_info (tmpl, args);
  TREE_TYPE (tmpl) = TREE_TYPE (fn);
  DECL_TEMPLATE_RESULT (tmpl) = fn;
  DECL_ARTIFICIAL (tmpl) = true;
  DECL_PRIMARY_TEMPLATE (tmpl) = tmpl;
  return tmpl;
}

/* Called when a class template TYPE is redeclared with the indicated
   template PARMS, e.g.:

     template <class T> struct S;
     template <class T> struct S {};  */

bool
redeclare_class_template (tree type, tree parms)
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
      inform_n (input_location, TREE_VEC_LENGTH (tmpl_parms),
                "previous declaration %q+D used %d template parameter",
                "previous declaration %q+D used %d template parameters",
                tmpl, TREE_VEC_LENGTH (tmpl_parms));
      return false;
    }

  for (i = 0; i < TREE_VEC_LENGTH (tmpl_parms); ++i)
    {
      tree tmpl_parm;
      tree parm;
      tree tmpl_default;
      tree parm_default;

      if (TREE_VEC_ELT (tmpl_parms, i) == error_mark_node
          || TREE_VEC_ELT (parms, i) == error_mark_node)
        continue;

      tmpl_parm = TREE_VALUE (TREE_VEC_ELT (tmpl_parms, i));
      if (tmpl_parm == error_mark_node)
	return false;

      parm = TREE_VALUE (TREE_VEC_ELT (parms, i));
      tmpl_default = TREE_PURPOSE (TREE_VEC_ELT (tmpl_parms, i));
      parm_default = TREE_PURPOSE (TREE_VEC_ELT (parms, i));

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
	  error ("template parameter %q+#D", tmpl_parm);
	  error ("redeclared here as %q#D", parm);
	  return false;
	}

      if (tmpl_default != NULL_TREE && parm_default != NULL_TREE)
	{
	  /* We have in [temp.param]:

	     A template-parameter may not be given default arguments
	     by two different declarations in the same scope.  */
	  error_at (input_location, "redefinition of default argument for %q#D", parm);
	  inform (DECL_SOURCE_LOCATION (tmpl_parm),
		  "original definition appeared here");
	  return false;
	}

      if (parm_default != NULL_TREE)
	/* Update the previous template parameters (which are the ones
	   that will really count) with the new default value.  */
	TREE_PURPOSE (TREE_VEC_ELT (tmpl_parms, i)) = parm_default;
      else if (tmpl_default != NULL_TREE)
	/* Update the new parameters, too; they'll be used as the
	   parameters for any members.  */
	TREE_PURPOSE (TREE_VEC_ELT (parms, i)) = tmpl_default;
    }

    return true;
}

/* Simplify EXPR if it is a non-dependent expression.  Returns the
   (possibly simplified) expression.  */

tree
fold_non_dependent_expr_sfinae (tree expr, tsubst_flags_t complain)
{
  if (expr == NULL_TREE)
    return NULL_TREE;

  /* If we're in a template, but EXPR isn't value dependent, simplify
     it.  We're supposed to treat:

       template <typename T> void f(T[1 + 1]);
       template <typename T> void f(T[2]);

     as two declarations of the same function, for example.  */
  if (processing_template_decl
      && !instantiation_dependent_expression_p (expr)
      && potential_constant_expression (expr))
    {
      HOST_WIDE_INT saved_processing_template_decl;

      saved_processing_template_decl = processing_template_decl;
      processing_template_decl = 0;
      expr = tsubst_copy_and_build (expr,
				    /*args=*/NULL_TREE,
				    complain,
				    /*in_decl=*/NULL_TREE,
				    /*function_p=*/false,
				    /*integral_constant_expression_p=*/true);
      processing_template_decl = saved_processing_template_decl;
    }
  return expr;
}

tree
fold_non_dependent_expr (tree expr)
{
  return fold_non_dependent_expr_sfinae (expr, tf_error);
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

/* Return TRUE iff is a specialization of an alias template.  */

bool
alias_template_specialization_p (const_tree t)
{
  if (t == NULL_TREE)
    return false;
  
  return (TYPE_P (t)
	  && TYPE_TEMPLATE_INFO (t)
	  && PRIMARY_TEMPLATE_P (TYPE_TI_TEMPLATE (t))
	  && DECL_ALIAS_TEMPLATE_P (TYPE_TI_TEMPLATE (t)));
}

/* Return the number of innermost template parameters in TMPL.  */

static int
num_innermost_template_parms (tree tmpl)
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
      tree result = DECL_ORIGINAL_TYPE (DECL_TEMPLATE_RESULT (tmpl));
      if (TYPE_TEMPLATE_INFO (result))
	{
	  tree sub = TYPE_TI_TEMPLATE (result);
	  if (PRIMARY_TEMPLATE_P (sub)
	      && (num_innermost_template_parms (tmpl)
		  == num_innermost_template_parms (sub)))
	    {
	      tree alias_args = INNERMOST_TEMPLATE_ARGS
		(template_parms_to_args (DECL_TEMPLATE_PARMS (tmpl)));
	      if (!comp_template_args (TYPE_TI_ARGS (result), alias_args))
		break;
	      /* The alias type is equivalent to the pattern of the
		 underlying template, so strip the alias.  */
	      tmpl = sub;
	      continue;
	    }
	}
      break;
    }
  return tmpl;
}

/* Subroutine of convert_nontype_argument. Converts EXPR to TYPE, which
   must be a function or a pointer-to-function type, as specified
   in [temp.arg.nontype]: disambiguate EXPR if it is an overload set,
   and check that the resulting function has external linkage.  */

static tree
convert_nontype_argument_function (tree type, tree expr)
{
  tree fns = expr;
  tree fn, fn_no_ptr;
  linkage_kind linkage;

  fn = instantiate_type (type, fns, tf_none);
  if (fn == error_mark_node)
    return error_mark_node;

  fn_no_ptr = fn;
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

  if (TREE_CODE (fn_no_ptr) != FUNCTION_DECL)
    {
      error ("%qE is not a valid template argument for type %qT", expr, type);
      if (TYPE_PTR_P (type))
	error ("it must be the address of a function with external linkage");
      else
	error ("it must be the name of a function with external linkage");
      return NULL_TREE;
    }

  linkage = decl_linkage (fn_no_ptr);
  if (cxx_dialect >= cxx11 ? linkage == lk_none : linkage != lk_external)
    {
      if (cxx_dialect >= cxx11)
	error ("%qE is not a valid template argument for type %qT "
	       "because %qD has no linkage",
	       expr, type, fn_no_ptr);
      else
	error ("%qE is not a valid template argument for type %qT "
	       "because %qD does not have external linkage",
	       expr, type, fn_no_ptr);
      return NULL_TREE;
    }

  return fn;
}

/* Subroutine of convert_nontype_argument.
   Check if EXPR of type TYPE is a valid pointer-to-member constant.
   Emit an error otherwise.  */

static bool
check_valid_ptrmem_cst_expr (tree type, tree expr,
			     tsubst_flags_t complain)
{
  STRIP_NOPS (expr);
  if (expr && (null_ptr_cst_p (expr) || TREE_CODE (expr) == PTRMEM_CST))
    return true;
  if (cxx_dialect >= cxx11 && null_member_pointer_value_p (expr))
    return true;
  if (complain & tf_error)
    {
      error ("%qE is not a valid template argument for type %qT",
	     expr, type);
      error ("it must be a pointer-to-member of the form %<&X::Y%>");
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
  /* We could use get_inner_reference here, but there's no need;
     this is only relevant for template non-type arguments, which
     can only be expressed as &id-expression.  */
  if (DECL_P (op))
    {
      tree ctx = CP_DECL_CONTEXT (op);
      if (TYPE_P (ctx) && dependent_type_p (ctx))
	return true;
    }

  return false;
}

/* The next set of functions are used for providing helpful explanatory
   diagnostics for failed overload resolution.  Their messages should be
   indented by two spaces for consistency with the messages in
   call.c  */

static int
unify_success (bool /*explain_p*/)
{
  return 0;
}

static int
unify_parameter_deduction_failure (bool explain_p, tree parm)
{
  if (explain_p)
    inform (input_location,
	    "  couldn't deduce template parameter %qD", parm);
  return 1;
}

static int
unify_invalid (bool /*explain_p*/)
{
  return 1;
}

static int
unify_cv_qual_mismatch (bool explain_p, tree parm, tree arg)
{
  if (explain_p)
    inform (input_location,
	    "  types %qT and %qT have incompatible cv-qualifiers",
	    parm, arg);
  return 1;
}

static int
unify_type_mismatch (bool explain_p, tree parm, tree arg)
{
  if (explain_p)
    inform (input_location, "  mismatched types %qT and %qT", parm, arg);
  return 1;
}

static int
unify_parameter_pack_mismatch (bool explain_p, tree parm, tree arg)
{
  if (explain_p)
    inform (input_location,
	    "  template parameter %qD is not a parameter pack, but "
	    "argument %qD is",
	    parm, arg);
  return 1;
}

static int
unify_ptrmem_cst_mismatch (bool explain_p, tree parm, tree arg)
{
  if (explain_p)
    inform (input_location,
	    "  template argument %qE does not match "
	    "pointer-to-member constant %qE",
	    arg, parm);
  return 1;
}

static int
unify_expression_unequal (bool explain_p, tree parm, tree arg)
{
  if (explain_p)
    inform (input_location, "  %qE is not equivalent to %qE", parm, arg);
  return 1;
}

static int
unify_parameter_pack_inconsistent (bool explain_p, tree old_arg, tree new_arg)
{
  if (explain_p)
    inform (input_location,
	    "  inconsistent parameter pack deduction with %qT and %qT",
	    old_arg, new_arg);
  return 1;
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
  return 1;
}

static int
unify_vla_arg (bool explain_p, tree arg)
{
  if (explain_p)
    inform (input_location,
	    "  variable-sized array type %qT is not "
	    "a valid template argument",
	    arg);
  return 1;
}

static int
unify_method_type_error (bool explain_p, tree arg)
{
  if (explain_p)
    inform (input_location,
	    "  member function type %qT is not a valid template argument",
	    arg);
  return 1;
}

static int
unify_arity (bool explain_p, int have, int wanted)
{
  if (explain_p)
    inform_n (input_location, wanted,
	      "  candidate expects %d argument, %d provided",
	      "  candidate expects %d arguments, %d provided",
	      wanted, have);
  return 1;
}

static int
unify_too_many_arguments (bool explain_p, int have, int wanted)
{
  return unify_arity (explain_p, have, wanted);
}

static int
unify_too_few_arguments (bool explain_p, int have, int wanted)
{
  return unify_arity (explain_p, have, wanted);
}

static int
unify_arg_conversion (bool explain_p, tree to_type,
		      tree from_type, tree arg)
{
  if (explain_p)
    inform (EXPR_LOC_OR_LOC (arg, input_location),
	    "  cannot convert %qE (type %qT) to type %qT",
	    arg, from_type, to_type);
  return 1;
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
  return 1;
}

static int
unify_inconsistent_template_template_parameters (bool explain_p)
{
  if (explain_p)
    inform (input_location,
	    "  template parameters of a template template argument are "
	    "inconsistent with other deduced template arguments");
  return 1;
}

static int
unify_template_deduction_failure (bool explain_p, tree parm, tree arg)
{
  if (explain_p)
    inform (input_location,
	    "  can't deduce a template for %qT from non-template type %qT",
	    parm, arg);
  return 1;
}

static int
unify_template_argument_mismatch (bool explain_p, tree parm, tree arg)
{
  if (explain_p)
    inform (input_location,
	    "  template argument %qE does not match %qD", arg, parm);
  return 1;
}

static int
unify_overload_resolution_failure (bool explain_p, tree arg)
{
  if (explain_p)
    inform (input_location,
	    "  could not resolve address from overloaded function %qE",
	    arg);
  return 1;
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

  /* Detect immediately string literals as invalid non-type argument.
     This special-case is not needed for correctness (we would easily
     catch this later), but only to provide better diagnostic for this
     common user mistake. As suggested by DR 100, we do not mention
     linkage issues in the diagnostic as this is not the point.  */
  /* FIXME we're making this OK.  */
  if (TREE_CODE (expr) == STRING_CST)
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
  if (TYPE_REF_OBJ_P (type)
      && has_value_dependent_address (expr))
    /* If we want the address and it's value-dependent, don't fold.  */;
  else if (!type_unknown_p (expr))
    expr = fold_non_dependent_expr_sfinae (expr, complain);
  if (error_operand_p (expr))
    return error_mark_node;
  expr_type = TREE_TYPE (expr);
  if (TREE_CODE (type) == REFERENCE_TYPE)
    expr = mark_lvalue_use (expr);
  else
    expr = mark_rvalue_use (expr);

  /* 14.3.2/5: The null pointer{,-to-member} conversion is applied
     to a non-type argument of "nullptr".  */
  if (expr == nullptr_node && TYPE_PTR_OR_PTRMEM_P (type))
    expr = convert (type, expr);

  /* In C++11, integral or enumeration non-type template arguments can be
     arbitrary constant expressions.  Pointer and pointer to
     member arguments can be general constant expressions that evaluate
     to a null value, but otherwise still need to be of a specific form.  */
  if (cxx_dialect >= cxx11)
    {
      if (TREE_CODE (expr) == PTRMEM_CST)
	/* A PTRMEM_CST is already constant, and a valid template
	   argument for a parameter of pointer to member type, we just want
	   to leave it in that form rather than lower it to a
	   CONSTRUCTOR.  */;
      else if (INTEGRAL_OR_ENUMERATION_TYPE_P (type))
	expr = maybe_constant_value (expr);
      else if (TYPE_PTR_OR_PTRMEM_P (type))
	{
	  tree folded = maybe_constant_value (expr);
	  if (TYPE_PTR_P (type) ? integer_zerop (folded)
	      : null_member_pointer_value_p (folded))
	    expr = folded;
	}
    }

  /* HACK: Due to double coercion, we can get a
     NOP_EXPR<REFERENCE_TYPE>(ADDR_EXPR<POINTER_TYPE> (arg)) here,
     which is the tree that we built on the first call (see
     below when coercing to reference to object or to reference to
     function). We just strip everything and get to the arg.
     See g++.old-deja/g++.oliva/template4.C and g++.dg/template/nontype9.C
     for examples.  */
  if (TYPE_REF_OBJ_P (type) || TYPE_REFFN_P (type))
    {
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
	  if (TREE_CODE (probe_type) == REFERENCE_TYPE
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

  /* We could also generate a NOP_EXPR(ADDR_EXPR()) when the
     parameter is a pointer to object, through decay and
     qualification conversion. Let's strip everything.  */
  else if (TREE_CODE (expr) == NOP_EXPR && TYPE_PTROBV_P (type))
    {
      tree probe = expr;
      STRIP_NOPS (probe);
      if (TREE_CODE (probe) == ADDR_EXPR
	  && TYPE_PTR_P (TREE_TYPE (probe)))
	{
	  /* Skip the ADDR_EXPR only if it is part of the decay for
	     an array. Otherwise, it is part of the original argument
	     in the source code.  */
	  if (TREE_CODE (TREE_TYPE (TREE_OPERAND (probe, 0))) == ARRAY_TYPE)
	    probe = TREE_OPERAND (probe, 0);
	  expr = probe;
	  expr_type = TREE_TYPE (expr);
	}
    }

  /* [temp.arg.nontype]/5, bullet 1

     For a non-type template-parameter of integral or enumeration type,
     integral promotions (_conv.prom_) and integral conversions
     (_conv.integral_) are applied.  */
  if (INTEGRAL_OR_ENUMERATION_TYPE_P (type))
    {
      tree t = build_integral_nontype_arg_conv (type, expr, complain);
      t = maybe_constant_value (t);
      if (t != error_mark_node)
	expr = t;

      if (!same_type_ignoring_top_level_qualifiers_p (type, TREE_TYPE (expr)))
	return error_mark_node;

      /* Notice that there are constant expressions like '4 % 0' which
	 do not fold into integer constants.  */
      if (TREE_CODE (expr) != INTEGER_CST)
	{
	  if (complain & tf_error)
	    {
	      int errs = errorcount, warns = warningcount + werrorcount;
	      if (processing_template_decl
		  && !require_potential_constant_expression (expr))
		return NULL_TREE;
	      expr = cxx_constant_value (expr);
	      if (errorcount > errs || warningcount + werrorcount > warns)
		inform (EXPR_LOC_OR_LOC (expr, input_location),
			"in template argument for type %qT ", type);
	      if (expr == error_mark_node)
		return NULL_TREE;
	      /* else cxx_constant_value complained but gave us
		 a real constant, so go ahead.  */
	      gcc_assert (TREE_CODE (expr) == INTEGER_CST);
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

      if (DECL_P (expr) && DECL_TEMPLATE_PARM_P (expr))
	/* Non-type template parameters are OK.  */
	;
      else if (cxx_dialect >= cxx11 && integer_zerop (expr))
	/* Null pointer values are OK in C++11.  */;
      else if (TREE_CODE (expr) != ADDR_EXPR
	       && TREE_CODE (expr_type) != ARRAY_TYPE)
	{
	  if (VAR_P (expr))
	    {
	      error ("%qD is not a valid template argument "
		     "because %qD is a variable, not the address of "
		     "a variable",
		     expr, expr);
	      return NULL_TREE;
	    }
	  if (POINTER_TYPE_P (expr_type))
	    {
	      error ("%qE is not a valid template argument for %qT "
		     "because it is not the address of a variable",
		     expr, type);
	      return NULL_TREE;
	    }
	  /* Other values, like integer constants, might be valid
	     non-type arguments of some other type.  */
	  return error_mark_node;
	}
      else
	{
	  tree decl;

	  decl = ((TREE_CODE (expr) == ADDR_EXPR)
		  ? TREE_OPERAND (expr, 0) : expr);
	  if (!VAR_P (decl))
	    {
	      error ("%qE is not a valid template argument of type %qT "
		     "because %qE is not a variable",
		     expr, type, decl);
	      return NULL_TREE;
	    }
	  else if (cxx_dialect < cxx11 && !DECL_EXTERNAL_LINKAGE_P (decl))
	    {
	      error ("%qE is not a valid template argument of type %qT "
		     "because %qD does not have external linkage",
		     expr, type, decl);
	      return NULL_TREE;
	    }
	  else if (cxx_dialect >= cxx11 && decl_linkage (decl) == lk_none)
	    {
	      error ("%qE is not a valid template argument of type %qT "
		     "because %qD has no linkage",
		     expr, type, decl);
	      return NULL_TREE;
	    }
	}

      expr = decay_conversion (expr, complain);
      if (expr == error_mark_node)
	return error_mark_node;

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
	  error ("%qE is not a valid template argument for type %qT "
		 "because of conflicts in cv-qualification", expr, type);
	  return NULL_TREE;
	}

      if (!real_lvalue_p (expr))
	{
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
	      error ("%q#D is not a valid template argument for type %qT "
		     "because a reference variable does not have a constant "
		     "address", expr, type);
	      return NULL_TREE;
	    }
	}

      if (!DECL_P (expr))
	{
	  error ("%qE is not a valid template argument for type %qT "
		 "because it is not an object with external linkage",
		 expr, type);
	  return NULL_TREE;
	}

      if (!DECL_EXTERNAL_LINKAGE_P (expr))
	{
	  error ("%qE is not a valid template argument for type %qT "
		 "because object %qD has not external linkage",
		 expr, type, expr);
	  return NULL_TREE;
	}

      expr = build_nop (type, build_address (expr));
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

      expr = convert_nontype_argument_function (type, expr);
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
	  error ("%qE is not a valid template argument for type %qT "
		 "because it is a pointer", expr, type);
	  inform (input_location, "try using %qE instead", TREE_OPERAND (expr, 0));
	  return NULL_TREE;
	}

      expr = convert_nontype_argument_function (type, expr);
      if (!expr || expr == error_mark_node)
	return expr;

      expr = build_nop (type, build_address (expr));
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
      if (!check_valid_ptrmem_cst_expr (type, expr, complain))
	return error_mark_node;

      /* There is no way to disable standard conversions in
	 resolve_address_of_overloaded_function (called by
	 instantiate_type). It is possible that the call succeeded by
	 converting &B::I to &D::I (where B is a base of D), so we need
	 to reject this conversion here.

	 Actually, even if there was a way to disable standard conversions,
	 it would still be better to reject them here so that we can
	 provide a superior diagnostic.  */
      if (!same_type_p (TREE_TYPE (expr), type))
	{
	  error ("%qE is not a valid template argument for type %qT "
		 "because it is of type %qT", expr, type,
		 TREE_TYPE (expr));
	  /* If we are just one standard conversion off, explain.  */
	  if (can_convert_standard (type, TREE_TYPE (expr), complain))
	    inform (input_location,
		    "standard conversions are not allowed in this context");
	  return NULL_TREE;
	}
    }
  /* [temp.arg.nontype]/5, bullet 7

     For a non-type template-parameter of type pointer to data member,
     qualification conversions (_conv.qual_) are applied.  */
  else if (TYPE_PTRDATAMEM_P (type))
    {
      /* [temp.arg.nontype] bullet 1 says the pointer to member
         expression must be a pointer-to-member constant.  */
      if (!check_valid_ptrmem_cst_expr (type, expr, complain))
	return error_mark_node;

      expr = perform_qualification_conversions (type, expr);
      if (expr == error_mark_node)
	return expr;
    }
  else if (NULLPTR_TYPE_P (type))
    {
      if (expr != nullptr_node)
	{
	  error ("%qE is not a valid template argument for type %qT "
		 "because it is of type %qT", expr, type, TREE_TYPE (expr));
	  return NULL_TREE;
	}
      return expr;
    }
  /* A template non-type parameter must be one of the above.  */
  else
    gcc_unreachable ();

  /* Sanity check: did we actually convert the argument to the
     right type?  */
  gcc_assert (same_type_ignoring_top_level_qualifiers_p
	      (type, TREE_TYPE (expr)));
  return expr;
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
  if (arg == NULL_TREE || arg == error_mark_node
      || parm == NULL_TREE || parm == error_mark_node)
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
	tree parmparm = DECL_INNERMOST_TEMPLATE_PARMS (parm);
	tree argparm = DECL_INNERMOST_TEMPLATE_PARMS (arg);
	
	if (!coerce_template_template_parms
	    (parmparm, argparm, complain, in_decl, outer_args))
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
      if (!uses_template_parms (TREE_TYPE (arg))
	  && !same_type_p
	        (tsubst (TREE_TYPE (parm), outer_args, complain, in_decl),
		 TREE_TYPE (arg)))
	return 0;
      
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


/* Return 1 if PARM_PARMS and ARG_PARMS matches using rule for
   template template parameters.  Both PARM_PARMS and ARG_PARMS are
   vectors of TREE_LIST nodes containing TYPE_DECL, TEMPLATE_DECL
   or PARM_DECL.

   Consider the example:
     template <class T> class A;
     template<template <class U> class TT> class B;

   For B<A>, PARM_PARMS are the parameters to TT, while ARG_PARMS are
   the parameters to A, and OUTER_ARGS contains A.  */

static int
coerce_template_template_parms (tree parm_parms,
				tree arg_parms,
				tsubst_flags_t complain,
				tree in_decl,
				tree outer_args)
{
  int nparms, nargs, i;
  tree parm, arg;
  int variadic_p = 0;

  gcc_assert (TREE_CODE (parm_parms) == TREE_VEC);
  gcc_assert (TREE_CODE (arg_parms) == TREE_VEC);

  nparms = TREE_VEC_LENGTH (parm_parms);
  nargs = TREE_VEC_LENGTH (arg_parms);

  /* Determine whether we have a parameter pack at the end of the
     template template parameter's template parameter list.  */
  if (TREE_VEC_ELT (parm_parms, nparms - 1) != error_mark_node)
    {
      parm = TREE_VALUE (TREE_VEC_ELT (parm_parms, nparms - 1));
      
      if (parm == error_mark_node)
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
	      tree targ_parms = NULL_TREE;

	      if (packed_args)
		/* Extract the next argument from the argument
		   pack.  */
		targ = TREE_VEC_ELT (packed_args, idx);

	      if (PACK_EXPANSION_P (targ))
		/* Look at the pattern of the pack expansion.  */
		targ = PACK_EXPANSION_PATTERN (targ);

	      /* Extract the template parameters from the template
		 argument.  */
	      if (TREE_CODE (targ) == TEMPLATE_DECL)
		targ_parms = DECL_INNERMOST_TEMPLATE_PARMS (targ);
	      else if (TREE_CODE (targ) == TEMPLATE_TEMPLATE_PARM)
		targ_parms = DECL_INNERMOST_TEMPLATE_PARMS (TYPE_NAME (targ));

	      /* Verify that we can coerce the template template
		 parameters from the template argument to the template
		 parameter.  This requires an exact match.  */
	      if (targ_parms
		  && !coerce_template_template_parms
		       (DECL_INNERMOST_TEMPLATE_PARMS (tparm),
			targ_parms,
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

static tree
canonicalize_type_argument (tree arg, tsubst_flags_t complain)
{
  tree mv;
  if (!arg || arg == error_mark_node || arg == TYPE_CANONICAL (arg))
    return arg;
  mv = TYPE_MAIN_VARIANT (arg);
  arg = strip_typedefs (arg);
  if (TYPE_ALIGN (arg) != TYPE_ALIGN (mv)
      || TYPE_ATTRIBUTES (arg) != TYPE_ATTRIBUTES (mv))
    {
      if (complain & tf_warning)
	warning (0, "ignoring attributes on template argument %qT", arg);
      arg = build_aligned_type (arg, TYPE_ALIGN (mv));
      arg = cp_build_type_attribute_variant (arg, TYPE_ATTRIBUTES (mv));
    }
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

  if (TREE_CODE (arg) == TREE_LIST
      && TREE_CODE (TREE_VALUE (arg)) == OFFSET_REF)
    {
      /* The template argument was the name of some
	 member function.  That's usually
	 invalid, but static members are OK.  In any
	 case, grab the underlying fields/functions
	 and issue an error later if required.  */
      orig_arg = TREE_VALUE (arg);
      TREE_TYPE (arg) = unknown_type_node;
    }

  orig_arg = arg;

  requires_tmpl_type = TREE_CODE (parm) == TEMPLATE_DECL;
  requires_type = (TREE_CODE (parm) == TYPE_DECL
		   || requires_tmpl_type);

  /* When determining whether an argument pack expansion is a template,
     look at the pattern.  */
  if (TREE_CODE (arg) == TYPE_PACK_EXPANSION)
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
		error ("  expected a constant of type %qT, got %qT",
		       TREE_TYPE (parm),
		       (DECL_P (arg) ? DECL_NAME (arg) : orig_arg));
	      else if (requires_tmpl_type)
		error ("  expected a class template, got %qE", orig_arg);
	      else
		error ("  expected a type, got %qE", orig_arg);
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
	    error ("  expected a type, got %qT", DECL_NAME (arg));
	  else
	    error ("  expected a class template, got %qT", orig_arg);
	}
      return error_mark_node;
    }

  if (is_type)
    {
      if (requires_tmpl_type)
	{
	  if (template_parameter_pack_p (parm) && ARGUMENT_PACK_P (orig_arg))
	    val = orig_arg;
	  else if (TREE_CODE (TREE_TYPE (arg)) == UNBOUND_CLASS_TEMPLATE)
	    /* The number of argument required is not known yet.
	       Just accept it for now.  */
	    val = TREE_TYPE (arg);
	  else
	    {
	      tree parmparm = DECL_INNERMOST_TEMPLATE_PARMS (parm);
	      tree argparm;

	      /* Strip alias templates that are equivalent to another
		 template.  */
	      arg = get_underlying_template (arg);
              argparm = DECL_INNERMOST_TEMPLATE_PARMS (arg);

	      if (coerce_template_template_parms (parmparm, argparm,
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
			val = make_pack_expansion (val);
                    }
		}
	      else
		{
		  if (in_decl && (complain & tf_error))
		    {
		      error ("type/value mismatch at argument %d in "
			     "template parameter list for %qD",
			     i + 1, in_decl);
		      error ("  expected a template of type %qD, got %qT",
			     parm, orig_arg);
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
      tree t = tsubst (TREE_TYPE (parm), args, complain, in_decl);

      if (invalid_nontype_parm_type_p (t, complain))
	return error_mark_node;

      if (template_parameter_pack_p (parm) && ARGUMENT_PACK_P (orig_arg))
	{
	  if (same_type_p (t, TREE_TYPE (orig_arg)))
	    val = orig_arg;
	  else
	    {
	      /* Not sure if this is reachable, but it doesn't hurt
		 to be robust.  */
	      error ("type mismatch in nontype parameter pack");
	      val = error_mark_node;
	    }
	}
      else if (!dependent_template_arg_p (orig_arg)
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
	val = strip_typedefs_expr (orig_arg);

      if (val == NULL_TREE)
	val = error_mark_node;
      else if (val == error_mark_node && (complain & tf_error))
	error ("could not convert template argument %qE to %qT",  orig_arg, t);

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
      SET_PACK_EXPANSION_PATTERN (exp, decl);
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
              tree t = TREE_TYPE (TREE_VEC_ELT (packed_parms, j));
              if (invalid_nontype_parm_type_p (t, complain))
                return error_mark_node;
            }
        }

      packed_args = make_tree_vec (TREE_VEC_LENGTH (packed_parms));
    }
  else
    packed_args = make_tree_vec (nargs - arg_idx);

  /* Convert the remaining arguments, which will be a part of the
     parameter pack "parm".  */
  for (; arg_idx < nargs; ++arg_idx)
    {
      tree arg = TREE_VEC_ELT (inner_args, arg_idx);
      tree actual_parm = TREE_VALUE (parm);
      int pack_idx = arg_idx - parm_idx;

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

  if (arg_idx - parm_idx < TREE_VEC_LENGTH (packed_args)
      && TREE_VEC_LENGTH (packed_args) > 0)
    {
      error ("wrong number of template arguments (%d, should be %d)",
	     arg_idx - parm_idx, TREE_VEC_LENGTH (packed_args));
      return error_mark_node;
    }

  if (TREE_CODE (TREE_VALUE (parm)) == TYPE_DECL
      || TREE_CODE (TREE_VALUE (parm)) == TEMPLATE_DECL)
    argument_pack = cxx_make_type (TYPE_ARGUMENT_PACK);
  else
    {
      argument_pack = make_node (NONTYPE_ARGUMENT_PACK);
      TREE_TYPE (argument_pack) 
        = tsubst (TREE_TYPE (TREE_VALUE (parm)), new_args, complain, in_decl);
      TREE_CONSTANT (argument_pack) = 1;
    }

  SET_ARGUMENT_PACK_ARGS (argument_pack, packed_args);
#ifdef ENABLE_CHECKING
  SET_NON_DEFAULT_TEMPLATE_ARGS_COUNT (packed_args,
				       TREE_VEC_LENGTH (packed_args));
#endif
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

   If REQUIRE_ALL_ARGS is false, argument deduction will be performed
   for arguments not specified in ARGS.  Otherwise, if
   USE_DEFAULT_ARGS is true, default arguments will be used to fill in
   unspecified arguments.  If REQUIRE_ALL_ARGS is true, but
   USE_DEFAULT_ARGS is false, then all arguments must be specified in
   ARGS.  */

static tree
coerce_template_parms (tree parms,
		       tree args,
		       tree in_decl,
		       tsubst_flags_t complain,
		       bool require_all_args,
		       bool use_default_args)
{
  int nparms, nargs, parm_idx, arg_idx, lost = 0;
  tree orig_inner_args;
  tree inner_args;
  tree new_args;
  tree new_inner_args;
  int saved_unevaluated_operand;
  int saved_inhibit_evaluation_warnings;

  /* When used as a boolean value, indicates whether this is a
     variadic template parameter list. Since it's an int, we can also
     subtract it from nparms to get the number of non-variadic
     parameters.  */
  int variadic_p = 0;
  int variadic_args_p = 0;
  int post_variadic_parms = 0;

  if (args == error_mark_node)
    return error_mark_node;

  nparms = TREE_VEC_LENGTH (parms);

  /* Determine if there are any parameter packs.  */
  for (parm_idx = 0; parm_idx < nparms; ++parm_idx)
    {
      tree tparm = TREE_VALUE (TREE_VEC_ELT (parms, parm_idx));
      if (variadic_p)
	++post_variadic_parms;
      if (template_parameter_pack_p (tparm))
	++variadic_p;
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
  if ((nargs > nparms && !variadic_p)
      || (nargs < nparms - variadic_p
	  && require_all_args
	  && !variadic_args_p
	  && (!use_default_args
	      || (TREE_VEC_ELT (parms, nargs) != error_mark_node
                  && !TREE_PURPOSE (TREE_VEC_ELT (parms, nargs))))))
    {
      if (complain & tf_error)
	{
          if (variadic_p)
            {
              nparms -= variadic_p;
	      error ("wrong number of template arguments "
		     "(%d, should be %d or more)", nargs, nparms);
            }
	  else
	     error ("wrong number of template arguments "
		    "(%d, should be %d)", nargs, nparms);

	  if (in_decl)
	    error ("provided for %q+D", in_decl);
	}

      return error_mark_node;
    }
  /* We can't pass a pack expansion to a non-pack parameter of an alias
     template (DR 1430).  */
  else if (in_decl && DECL_ALIAS_TEMPLATE_P (in_decl)
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
		  error ("pack expansion argument for non-pack parameter "
			 "%qD of alias template %qD", parm, in_decl);
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
  saved_unevaluated_operand = cp_unevaluated_operand;
  cp_unevaluated_operand = 0;
  saved_inhibit_evaluation_warnings = c_inhibit_evaluation_warnings;
  c_inhibit_evaluation_warnings = 0;
  new_inner_args = make_tree_vec (nparms);
  new_args = add_outermost_template_args (args, new_inner_args);
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
	    }
	  else
	    {
	      pack_adjust = TREE_VEC_LENGTH (ARGUMENT_PACK_ARGS (arg)) - 1;
	      arg_idx += pack_adjust;
	    }
          
          continue;
        }
      else if (arg)
	{
          if (PACK_EXPANSION_P (arg))
            {
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
        /* This only occurs if there was an error in the template
           parameter list itself (which we would already have
           reported) that we are trying to recover from, e.g., a class
           template with a parameter list such as
           template<typename..., typename>.  */
	++lost;
      else
	arg = convert_template_argument (TREE_VALUE (parm),
					 arg, new_args, complain, 
                                         parm_idx, in_decl);

      if (arg == error_mark_node)
	lost++;
      TREE_VEC_ELT (new_inner_args, arg_idx - pack_adjust) = arg;
    }
  cp_unevaluated_operand = saved_unevaluated_operand;
  c_inhibit_evaluation_warnings = saved_inhibit_evaluation_warnings;

  if (variadic_p && arg_idx < nargs)
    {
      if (complain & tf_error)
	{
	  error ("wrong number of template arguments "
		 "(%d, should be %d)", nargs, arg_idx);
	  if (in_decl)
	    error ("provided for %q+D", in_decl);
	}
      return error_mark_node;
    }

  if (lost)
    return error_mark_node;

#ifdef ENABLE_CHECKING
  if (!NON_DEFAULT_TEMPLATE_ARGS_COUNT (new_inner_args))
    SET_NON_DEFAULT_TEMPLATE_ARGS_COUNT (new_inner_args,
					 TREE_VEC_LENGTH (new_inner_args));
#endif

  return new_inner_args;
}

/* Like coerce_template_parms.  If PARMS represents all template
   parameters levels, this function returns a vector of vectors
   representing all the resulting argument levels.  Note that in this
   case, only the innermost arguments are coerced because the
   outermost ones are supposed to have been coerced already.

   Otherwise, if PARMS represents only (the innermost) vector of
   parameters, this function returns a vector containing just the
   innermost resulting arguments.  */

static tree
coerce_innermost_template_parms (tree parms,
				  tree args,
				  tree in_decl,
				  tsubst_flags_t complain,
				  bool require_all_args,
				  bool use_default_args)
{
  int parms_depth = TMPL_PARMS_DEPTH (parms);
  int args_depth = TMPL_ARGS_DEPTH (args);
  tree coerced_args;

  if (parms_depth > 1)
    {
      coerced_args = make_tree_vec (parms_depth);
      tree level;
      int cur_depth;

      for (level = parms, cur_depth = parms_depth;
	   parms_depth > 0 && level != NULL_TREE;
	   level = TREE_CHAIN (level), --cur_depth)
	{
	  tree l;
	  if (cur_depth == args_depth)
	    l = coerce_template_parms (TREE_VALUE (level),
				       args, in_decl, complain,
				       require_all_args,
				       use_default_args);
	  else
	    l = TMPL_ARGS_LEVEL (args, cur_depth);

	  if (l == error_mark_node)
	    return error_mark_node;

	  SET_TMPL_ARGS_LEVEL (coerced_args, cur_depth, l);
	}
    }
  else
    coerced_args = coerce_template_parms (INNERMOST_TEMPLATE_PARMS (parms),
					  args, in_decl, complain,
					  require_all_args,
					  use_default_args);
  return coerced_args;
}

/* Returns 1 if template args OT and NT are equivalent.  */

static int
template_args_equal (tree ot, tree nt)
{
  if (nt == ot)
    return 1;
  if (nt == NULL_TREE || ot == NULL_TREE)
    return false;

  if (TREE_CODE (nt) == TREE_VEC)
    /* For member templates */
    return TREE_CODE (ot) == TREE_VEC && comp_template_args (ot, nt);
  else if (PACK_EXPANSION_P (ot))
    return (PACK_EXPANSION_P (nt)
	    && template_args_equal (PACK_EXPANSION_PATTERN (ot),
				    PACK_EXPANSION_PATTERN (nt))
	    && template_args_equal (PACK_EXPANSION_EXTRA_ARGS (ot),
				    PACK_EXPANSION_EXTRA_ARGS (nt)));
  else if (ARGUMENT_PACK_P (ot))
    {
      int i, len;
      tree opack, npack;

      if (!ARGUMENT_PACK_P (nt))
	return 0;

      opack = ARGUMENT_PACK_ARGS (ot);
      npack = ARGUMENT_PACK_ARGS (nt);
      len = TREE_VEC_LENGTH (opack);
      if (TREE_VEC_LENGTH (npack) != len)
	return 0;
      for (i = 0; i < len; ++i)
	if (!template_args_equal (TREE_VEC_ELT (opack, i),
				  TREE_VEC_ELT (npack, i)))
	  return 0;
      return 1;
    }
  else if (ot && TREE_CODE (ot) == ARGUMENT_PACK_SELECT)
    {
      /* We get here probably because we are in the middle of substituting
         into the pattern of a pack expansion. In that case the
	 ARGUMENT_PACK_SELECT temporarily replaces the pack argument we are
	 interested in. So we want to use the initial pack argument for
	 the comparison.  */
      ot = ARGUMENT_PACK_SELECT_FROM_PACK (ot);
      if (nt && TREE_CODE (nt) == ARGUMENT_PACK_SELECT)
	nt = ARGUMENT_PACK_SELECT_FROM_PACK (nt);
      return template_args_equal (ot, nt);
    }
  else if (TYPE_P (nt))
    return TYPE_P (ot) && same_type_p (ot, nt);
  else if (TREE_CODE (ot) == TREE_VEC || TYPE_P (ot))
    return 0;
  else
    return cp_tree_equal (ot, nt);
}

/* Returns 1 iff the OLDARGS and NEWARGS are in fact identical sets of
   template arguments.  Returns 0 otherwise, and updates OLDARG_PTR and
   NEWARG_PTR with the offending arguments if they are non-NULL.  */

static int
comp_template_args_with_info (tree oldargs, tree newargs,
			      tree *oldarg_ptr, tree *newarg_ptr)
{
  int i;

  if (oldargs == newargs)
    return 1;

  if (!oldargs || !newargs)
    return 0;

  if (TREE_VEC_LENGTH (oldargs) != TREE_VEC_LENGTH (newargs))
    return 0;

  for (i = 0; i < TREE_VEC_LENGTH (oldargs); ++i)
    {
      tree nt = TREE_VEC_ELT (newargs, i);
      tree ot = TREE_VEC_ELT (oldargs, i);

      if (! template_args_equal (ot, nt))
	{
	  if (oldarg_ptr != NULL)
	    *oldarg_ptr = ot;
	  if (newarg_ptr != NULL)
	    *newarg_ptr = nt;
	  return 0;
	}
    }
  return 1;
}

/* Returns 1 iff the OLDARGS and NEWARGS are in fact identical sets
   of template arguments.  Returns 0 otherwise.  */

int
comp_template_args (tree oldargs, tree newargs)
{
  return comp_template_args_with_info (oldargs, newargs, NULL, NULL);
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
  level = !current_tinst_level || current_tinst_level->decl != d;

  if (level)
    push_tinst_level (d);

  pt = ggc_alloc_pending_template ();
  pt->next = NULL;
  pt->tinst = current_tinst_level;
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
  tree type;

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
      BASELINK_FUNCTIONS (fns) = build2 (TEMPLATE_ID_EXPR,
					 unknown_type_node,
					 BASELINK_FUNCTIONS (fns),
					 arglist);
      return fns;
    }

  type = TREE_TYPE (fns);
  if (TREE_CODE (fns) == OVERLOAD || !type)
    type = unknown_type_node;

  return build2 (TEMPLATE_ID_EXPR, type, fns, arglist);
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

static tree
lookup_template_class_1 (tree d1, tree arglist, tree in_decl, tree context,
			 int entering_scope, tsubst_flags_t complain)
{
  tree templ = NULL_TREE, parmlist;
  tree t;
  void **slot;
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
      if (templ)
	context = DECL_CONTEXT (templ);
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
  else if (TREE_CODE (d1) == TEMPLATE_DECL
           && DECL_TEMPLATE_RESULT (d1)
	   && TREE_CODE (DECL_TEMPLATE_RESULT (d1)) == TYPE_DECL)
    {
      templ = d1;
      d1 = DECL_NAME (templ);
      context = DECL_CONTEXT (templ);
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
      /* Create a new TEMPLATE_DECL and TEMPLATE_TEMPLATE_PARM node to store
	 template arguments */

      tree parm;
      tree arglist2;
      tree outer;

      parmlist = DECL_INNERMOST_TEMPLATE_PARMS (templ);

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

      outer = DECL_CONTEXT (templ);
      if (outer)
	outer = TI_ARGS (get_template_info (DECL_TEMPLATE_RESULT (outer)));
      else if (current_template_parms)
	/* This is an argument of the current template, so we haven't set
	   DECL_CONTEXT yet.  */
	outer = current_template_args ();

      if (outer)
	arglist = add_to_template_args (outer, arglist);

      arglist2 = coerce_template_parms (parmlist, arglist, templ,
					complain,
					/*require_all_args=*/true,
					/*use_default_args=*/true);
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
	  arglist
	    = add_outermost_template_args (TYPE_TI_ARGS (TREE_TYPE (templ)),
					   arglist);
	  arg_depth = TMPL_ARGS_DEPTH (arglist);
	}

      /* Now we should have enough arguments.  */
      gcc_assert (parm_depth == arg_depth);

      /* From here on, we're only interested in the most general
	 template.  */

      /* Calculate the BOUND_ARGS.  These will be the args that are
	 actually tsubst'd into the definition to create the
	 instantiation.  */
      if (parm_depth > 1)
	{
	  /* We have multiple levels of arguments to coerce, at once.  */
	  int i;
	  int saved_depth = TMPL_ARGS_DEPTH (arglist);

	  tree bound_args = make_tree_vec (parm_depth);

	  for (i = saved_depth,
		 t = DECL_TEMPLATE_PARMS (gen_tmpl);
	       i > 0 && t != NULL_TREE;
	       --i, t = TREE_CHAIN (t))
	    {
	      tree a;
	      if (i == saved_depth)
		a = coerce_template_parms (TREE_VALUE (t),
					   arglist, gen_tmpl,
					   complain,
					   /*require_all_args=*/true,
					   /*use_default_args=*/true);
	      else
		/* Outer levels should have already been coerced.  */
		a = TMPL_ARGS_LEVEL (arglist, i);

	      /* Don't process further if one of the levels fails.  */
	      if (a == error_mark_node)
		{
		  /* Restore the ARGLIST to its full size.  */
		  TREE_VEC_LENGTH (arglist) = saved_depth;
		  return error_mark_node;
		}

	      SET_TMPL_ARGS_LEVEL (bound_args, i, a);

	      /* We temporarily reduce the length of the ARGLIST so
		 that coerce_template_parms will see only the arguments
		 corresponding to the template parameters it is
		 examining.  */
	      TREE_VEC_LENGTH (arglist)--;
	    }

	  /* Restore the ARGLIST to its full size.  */
	  TREE_VEC_LENGTH (arglist) = saved_depth;

	  arglist = bound_args;
	}
      else
	arglist
	  = coerce_template_parms (INNERMOST_TEMPLATE_PARMS (parmlist),
				   INNERMOST_TEMPLATE_ARGS (arglist),
				   gen_tmpl,
				   complain,
				   /*require_all_args=*/true,
				   /*use_default_args=*/true);

      if (arglist == error_mark_node)
	/* We were unable to bind the arguments.  */
	return error_mark_node;

      /* In the scope of a template class, explicit references to the
	 template class refer to the type of the template, not any
	 instantiation of it.  For example, in:

	   template <class T> class C { void f(C<T>); }

	 the `C<T>' is just the same as `C'.  Outside of the
	 class, however, such a reference is an instantiation.  */
      if ((entering_scope
	   || !PRIMARY_TEMPLATE_P (gen_tmpl)
	   || currently_open_class (template_type))
	  /* comp_template_args is expensive, check it last.  */
	  && comp_template_args (TYPE_TI_ARGS (template_type),
				 arglist))
	return template_type;

      /* If we already have this specialization, return it.  */
      elt.tmpl = gen_tmpl;
      elt.args = arglist;
      hash = hash_specialization (&elt);
      entry = (spec_entry *) htab_find_with_hash (type_specializations,
						  &elt, hash);

      if (entry)
	return entry->spec;

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
	{
	  found = xref_tag_from_type (TREE_TYPE (gen_tmpl),
				      DECL_NAME (gen_tmpl),
				      /*tag_scope=*/ts_global);
	  return found;
	}

      context = tsubst (DECL_CONTEXT (gen_tmpl), arglist,
			complain, in_decl);
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
	  t = make_class_type (TREE_CODE (template_type));
	  CLASSTYPE_DECLARED_CLASS (t)
	    = CLASSTYPE_DECLARED_CLASS (template_type);
	  SET_CLASSTYPE_IMPLICIT_INSTANTIATION (t);
	  TYPE_FOR_JAVA (t) = TYPE_FOR_JAVA (template_type);

	  /* A local class.  Make sure the decl gets registered properly.  */
	  if (context == current_function_decl)
	    pushtag (DECL_NAME (gen_tmpl), t, /*tag_scope=*/ts_current);

	  if (comp_template_args (CLASSTYPE_TI_ARGS (template_type), arglist))
	    /* This instantiation is another name for the primary
	       template type. Set the TYPE_CANONICAL field
	       appropriately. */
	    TYPE_CANONICAL (t) = template_type;
	  else if (any_template_arguments_need_structural_equality_p (arglist))
	    /* Some of the template arguments require structural
	       equality testing, so this template class requires
	       structural equality testing. */
	    SET_TYPE_STRUCTURAL_EQUALITY (t);
	}
      else
	gcc_unreachable ();

      /* If we called start_enum or pushtag above, this information
	 will already be set up.  */
      if (!TYPE_NAME (t))
	{
	  TYPE_CONTEXT (t) = FROB_CONTEXT (context);

	  type_decl = create_implicit_typedef (DECL_NAME (gen_tmpl), t);
	  DECL_CONTEXT (type_decl) = TYPE_CONTEXT (t);
	  DECL_SOURCE_LOCATION (type_decl)
	    = DECL_SOURCE_LOCATION (TYPE_STUB_DECL (template_type));
	}
      else
	type_decl = TYPE_NAME (t);

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
	  tree partial_inst_args;
	  TREE_VEC_LENGTH (arglist)--;
	  ++processing_template_decl;
	  partial_inst_args =
	    tsubst (INNERMOST_TEMPLATE_ARGS
			(TYPE_TI_ARGS (TREE_TYPE (gen_tmpl))),
		    arglist, complain, NULL_TREE);
	  --processing_template_decl;
	  TREE_VEC_LENGTH (arglist)++;
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
	  found = tsubst (gen_tmpl, arglist, complain, NULL_TREE);
	  TREE_VEC_LENGTH (arglist)++;
	  /* FOUND is either a proper class type, or an alias
	     template specialization.  In the later case, it's a
	     TYPE_DECL, resulting from the substituting of arguments
	     for parameters in the TYPE_DECL of the alias template
	     done earlier.  So be careful while getting the template
	     of FOUND.  */
	  found = TREE_CODE (found) == TYPE_DECL
	    ? TYPE_TI_TEMPLATE (TREE_TYPE (found))
	    : CLASSTYPE_TI_TEMPLATE (found);
	}

      SET_TYPE_TEMPLATE_INFO (t, build_template_info (found, arglist));

      elt.spec = t;
      slot = htab_find_slot_with_hash (type_specializations,
				       &elt, hash, INSERT);
      entry = ggc_alloc_spec_entry ();
      *entry = elt;
      *slot = entry;

      /* Note this use of the partial instantiation so we can check it
	 later in maybe_process_partial_specialization.  */
      DECL_TEMPLATE_INSTANTIATIONS (found)
	= tree_cons (arglist, t,
		     DECL_TEMPLATE_INSTANTIATIONS (found));

      if (TREE_CODE (template_type) == ENUMERAL_TYPE && !is_dependent_type
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

      return t;
    }
}

/* Wrapper for lookup_template_class_1.  */

tree
lookup_template_class (tree d1, tree arglist, tree in_decl, tree context,
                       int entering_scope, tsubst_flags_t complain)
{
  tree ret;
  timevar_push (TV_TEMPLATE_INST);
  ret = lookup_template_class_1 (d1, arglist, in_decl, context,
                                 entering_scope, complain);
  timevar_pop (TV_TEMPLATE_INST);
  return ret;
}

struct pair_fn_data
{
  tree_fn_t fn;
  void *data;
  /* True when we should also visit template parameters that occur in
     non-deduced contexts.  */
  bool include_nondeduced_p;
  struct pointer_set_t *visited;
};

/* Called from for_each_template_parm via walk_tree.  */

static tree
for_each_template_parm_r (tree *tp, int *walk_subtrees, void *d)
{
  tree t = *tp;
  struct pair_fn_data *pfd = (struct pair_fn_data *) d;
  tree_fn_t fn = pfd->fn;
  void *data = pfd->data;

  if (TYPE_P (t)
      && (pfd->include_nondeduced_p || TREE_CODE (t) != TYPENAME_TYPE)
      && for_each_template_parm (TYPE_CONTEXT (t), fn, data, pfd->visited,
				 pfd->include_nondeduced_p))
    return error_mark_node;

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
      else if (for_each_template_parm (TYPE_TI_ARGS (t),
				       fn, data, pfd->visited, 
				       pfd->include_nondeduced_p))
	return error_mark_node;
      break;

    case INTEGER_TYPE:
      if (for_each_template_parm (TYPE_MIN_VALUE (t),
				  fn, data, pfd->visited, 
				  pfd->include_nondeduced_p)
	  || for_each_template_parm (TYPE_MAX_VALUE (t),
				     fn, data, pfd->visited,
				     pfd->include_nondeduced_p))
	return error_mark_node;
      break;

    case METHOD_TYPE:
      /* Since we're not going to walk subtrees, we have to do this
	 explicitly here.  */
      if (for_each_template_parm (TYPE_METHOD_BASETYPE (t), fn, data,
				  pfd->visited, pfd->include_nondeduced_p))
	return error_mark_node;
      /* Fall through.  */

    case FUNCTION_TYPE:
      /* Check the return type.  */
      if (for_each_template_parm (TREE_TYPE (t), fn, data, pfd->visited,
				  pfd->include_nondeduced_p))
	return error_mark_node;

      /* Check the parameter types.  Since default arguments are not
	 instantiated until they are needed, the TYPE_ARG_TYPES may
	 contain expressions that involve template parameters.  But,
	 no-one should be looking at them yet.  And, once they're
	 instantiated, they don't contain template parameters, so
	 there's no point in looking at them then, either.  */
      {
	tree parm;

	for (parm = TYPE_ARG_TYPES (t); parm; parm = TREE_CHAIN (parm))
	  if (for_each_template_parm (TREE_VALUE (parm), fn, data,
				      pfd->visited, pfd->include_nondeduced_p))
	    return error_mark_node;

	/* Since we've already handled the TYPE_ARG_TYPES, we don't
	   want walk_tree walking into them itself.  */
	*walk_subtrees = 0;
      }
      break;

    case TYPEOF_TYPE:
    case UNDERLYING_TYPE:
      if (pfd->include_nondeduced_p
	  && for_each_template_parm (TYPE_FIELDS (t), fn, data,
				     pfd->visited, 
				     pfd->include_nondeduced_p))
	return error_mark_node;
      break;

    case FUNCTION_DECL:
    case VAR_DECL:
      if (DECL_LANG_SPECIFIC (t) && DECL_TEMPLATE_INFO (t)
	  && for_each_template_parm (DECL_TI_ARGS (t), fn, data,
				     pfd->visited, pfd->include_nondeduced_p))
	return error_mark_node;
      /* Fall through.  */

    case PARM_DECL:
    case CONST_DECL:
      if (TREE_CODE (t) == CONST_DECL && DECL_TEMPLATE_PARM_P (t)
	  && for_each_template_parm (DECL_INITIAL (t), fn, data,
				     pfd->visited, pfd->include_nondeduced_p))
	return error_mark_node;
      if (DECL_CONTEXT (t)
	  && pfd->include_nondeduced_p
	  && for_each_template_parm (DECL_CONTEXT (t), fn, data,
				     pfd->visited, pfd->include_nondeduced_p))
	return error_mark_node;
      break;

    case BOUND_TEMPLATE_TEMPLATE_PARM:
      /* Record template parameters such as `T' inside `TT<T>'.  */
      if (for_each_template_parm (TYPE_TI_ARGS (t), fn, data, pfd->visited,
				  pfd->include_nondeduced_p))
	return error_mark_node;
      /* Fall through.  */

    case TEMPLATE_TEMPLATE_PARM:
    case TEMPLATE_TYPE_PARM:
    case TEMPLATE_PARM_INDEX:
      if (fn && (*fn)(t, data))
	return error_mark_node;
      else if (!fn)
	return error_mark_node;
      break;

    case TEMPLATE_DECL:
      /* A template template parameter is encountered.  */
      if (DECL_TEMPLATE_TEMPLATE_PARM_P (t)
	  && for_each_template_parm (TREE_TYPE (t), fn, data, pfd->visited,
				     pfd->include_nondeduced_p))
	return error_mark_node;

      /* Already substituted template template parameter */
      *walk_subtrees = 0;
      break;

    case TYPENAME_TYPE:
      if (!fn
	  || for_each_template_parm (TYPENAME_TYPE_FULLNAME (t), fn,
				     data, pfd->visited, 
				     pfd->include_nondeduced_p))
	return error_mark_node;
      break;

    case CONSTRUCTOR:
      if (TREE_TYPE (t) && TYPE_PTRMEMFUNC_P (TREE_TYPE (t))
	  && pfd->include_nondeduced_p
	  && for_each_template_parm (TYPE_PTRMEMFUNC_FN_TYPE
				     (TREE_TYPE (t)), fn, data,
				     pfd->visited, pfd->include_nondeduced_p))
	return error_mark_node;
      break;

    case INDIRECT_REF:
    case COMPONENT_REF:
      /* If there's no type, then this thing must be some expression
	 involving template parameters.  */
      if (!fn && !TREE_TYPE (t))
	return error_mark_node;
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

  /* We didn't find any template parameters we liked.  */
  return NULL_TREE;
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

static int
for_each_template_parm (tree t, tree_fn_t fn, void* data,
			struct pointer_set_t *visited,
			bool include_nondeduced_p)
{
  struct pair_fn_data pfd;
  int result;

  /* Set up.  */
  pfd.fn = fn;
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
    pfd.visited = pointer_set_create ();
  result = cp_walk_tree (&t,
		         for_each_template_parm_r,
		         &pfd,
		         pfd.visited) != NULL_TREE;

  /* Clean up.  */
  if (!visited)
    {
      pointer_set_destroy (pfd.visited);
      pfd.visited = 0;
    }

  return result;
}

/* Returns true if T depends on any template parameter.  */

int
uses_template_parms (tree t)
{
  bool dependent_p;
  int saved_processing_template_decl;

  saved_processing_template_decl = processing_template_decl;
  if (!saved_processing_template_decl)
    processing_template_decl = 1;
  if (TYPE_P (t))
    dependent_p = dependent_type_p (t);
  else if (TREE_CODE (t) == TREE_VEC)
    dependent_p = any_dependent_template_arguments_p (t);
  else if (TREE_CODE (t) == TREE_LIST)
    dependent_p = (uses_template_parms (TREE_VALUE (t))
		   || uses_template_parms (TREE_CHAIN (t)));
  else if (TREE_CODE (t) == TYPE_DECL)
    dependent_p = dependent_type_p (TREE_TYPE (t));
  else if (DECL_P (t)
	   || EXPR_P (t)
	   || TREE_CODE (t) == TEMPLATE_PARM_INDEX
	   || TREE_CODE (t) == OVERLOAD
	   || BASELINK_P (t)
	   || identifier_p (t)
	   || TREE_CODE (t) == TRAIT_EXPR
	   || TREE_CODE (t) == CONSTRUCTOR
	   || CONSTANT_CLASS_P (t))
    dependent_p = (type_dependent_expression_p (t)
		   || value_dependent_expression_p (t));
  else
    {
      gcc_assert (t == error_mark_node);
      dependent_p = false;
    }

  processing_template_decl = saved_processing_template_decl;

  return dependent_p;
}

/* Returns true iff current_function_decl is an incompletely instantiated
   template.  Useful instead of processing_template_decl because the latter
   is set to 0 during fold_non_dependent_expr.  */

bool
in_template_function (void)
{
  tree fn = current_function_decl;
  bool ret;
  ++processing_template_decl;
  ret = (fn && DECL_LANG_SPECIFIC (fn)
	 && DECL_TEMPLATE_INFO (fn)
	 && any_dependent_template_arguments_p (DECL_TI_ARGS (fn)));
  --processing_template_decl;
  return ret;
}

/* Returns true if T depends on any template parameter with level LEVEL.  */

int
uses_template_parms_level (tree t, int level)
{
  return for_each_template_parm (t, template_parm_this_level_p, &level, NULL,
				 /*include_nondeduced_p=*/true);
}

/* Returns TRUE iff INST is an instantiation we don't need to do in an
   ill-formed translation unit, i.e. a variable or function that isn't
   usable in a constant expression.  */

static inline bool
neglectable_inst_p (tree d)
{
  return (DECL_P (d)
	  && !(TREE_CODE (d) == FUNCTION_DECL ? DECL_DECLARED_CONSTEXPR_P (d)
	       : decl_maybe_constant_var_p (d)));
}

/* Returns TRUE iff we should refuse to instantiate DECL because it's
   neglectable and instantiated from within an erroneous instantiation.  */

static bool
limit_bad_template_recursion (tree decl)
{
  struct tinst_level *lev = current_tinst_level;
  int errs = errorcount + sorrycount;
  if (lev == NULL || errs == 0 || !neglectable_inst_p (decl))
    return false;

  for (; lev; lev = lev->next)
    if (neglectable_inst_p (lev->decl))
      break;

  return (lev && errs > lev->errors);
}

static int tinst_depth;
extern int max_tinst_depth;
int depth_reached;

static GTY(()) struct tinst_level *last_error_tinst_level;

/* We're starting to instantiate D; record the template instantiation context
   for diagnostics and to restore it later.  */

int
push_tinst_level (tree d)
{
  struct tinst_level *new_level;

  if (tinst_depth >= max_tinst_depth)
    {
      last_error_tinst_level = current_tinst_level;
      if (TREE_CODE (d) == TREE_LIST)
	error ("template instantiation depth exceeds maximum of %d (use "
	       "-ftemplate-depth= to increase the maximum) substituting %qS",
	       max_tinst_depth, d);
      else
	error ("template instantiation depth exceeds maximum of %d (use "
	       "-ftemplate-depth= to increase the maximum) instantiating %qD",
	       max_tinst_depth, d);

      print_instantiation_context ();

      return 0;
    }

  /* If the current instantiation caused problems, don't let it instantiate
     anything else.  Do allow deduction substitution and decls usable in
     constant expressions.  */
  if (limit_bad_template_recursion (d))
    return 0;

  new_level = ggc_alloc_tinst_level ();
  new_level->decl = d;
  new_level->locus = input_location;
  new_level->errors = errorcount+sorrycount;
  new_level->in_system_header_p = in_system_header_at (input_location);
  new_level->next = current_tinst_level;
  current_tinst_level = new_level;

  ++tinst_depth;
  if (GATHER_STATISTICS && (tinst_depth > depth_reached))
    depth_reached = tinst_depth;

  return 1;
}

/* We're done instantiating this template; return to the instantiation
   context.  */

void
pop_tinst_level (void)
{
  /* Restore the filename and line number stashed away when we started
     this instantiation.  */
  input_location = current_tinst_level->locus;
  current_tinst_level = current_tinst_level->next;
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

  current_tinst_level = level;
  pop_tinst_level ();
  if (current_tinst_level)
    current_tinst_level->errors = errorcount+sorrycount;
  return level->decl;
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
			 tf_warning_or_error, NULL_TREE,
			 /*integral_constant_expression_p=*/false);
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

  /* The NEW_FRIEND will look like an instantiation, to the
     compiler, but is not an instantiation from the point of view of
     the language.  For example, we might have had:

     template <class T> struct S {
       template <class U> friend void f(T, U);
     };

     Then, in S<int>, template <class U> void f(int, U) is not an
     instantiation of anything.  */
  if (new_friend == error_mark_node)
    return error_mark_node;

  DECL_USE_TEMPLATE (new_friend) = 0;
  if (TREE_CODE (decl) == TEMPLATE_DECL)
    {
      DECL_USE_TEMPLATE (DECL_TEMPLATE_RESULT (new_friend)) = 0;
      DECL_SAVED_TREE (DECL_TEMPLATE_RESULT (new_friend))
	= DECL_SAVED_TREE (DECL_TEMPLATE_RESULT (decl));
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
      tree new_friend_template_info;
      tree new_friend_result_template_info;
      tree ns;
      int  new_friend_is_defn;

      /* We must save some information from NEW_FRIEND before calling
	 duplicate decls since that function will free NEW_FRIEND if
	 possible.  */
      new_friend_template_info = DECL_TEMPLATE_INFO (new_friend);
      new_friend_is_defn =
	    (DECL_INITIAL (DECL_TEMPLATE_RESULT
			   (template_for_substitution (new_friend)))
	     != NULL_TREE);
      if (TREE_CODE (new_friend) == TEMPLATE_DECL)
	{
	  /* This declaration is a `primary' template.  */
	  DECL_PRIMARY_TEMPLATE (new_friend) = new_friend;

	  new_friend_result_template_info
	    = DECL_TEMPLATE_INFO (DECL_TEMPLATE_RESULT (new_friend));
	}
      else
	new_friend_result_template_info = NULL_TREE;

      /* Make the init_value nonzero so pushdecl knows this is a defn.  */
      if (new_friend_is_defn)
	DECL_INITIAL (new_friend) = error_mark_node;

      /* Inside pushdecl_namespace_level, we will push into the
	 current namespace. However, the friend function should go
	 into the namespace of the template.  */
      ns = decl_namespace_context (new_friend);
      push_nested_namespace (ns);
      old_decl = pushdecl_namespace_level (new_friend, /*is_friend=*/true);
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

		      htab_remove_elt (decl_specializations, &elt);

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
  tree friend_type;
  tree tmpl;
  tree context;

  if (DECL_TEMPLATE_TEMPLATE_PARM_P (friend_tmpl))
    {
      tree t = tsubst (TREE_TYPE (friend_tmpl), args, tf_none, NULL_TREE);
      return TREE_TYPE (t);
    }

  context = CP_DECL_CONTEXT (friend_tmpl);

  if (context != global_namespace)
    {
      if (TREE_CODE (context) == NAMESPACE_DECL)
	push_nested_namespace (context);
      else
	push_nested_class (tsubst (context, args, tf_none, NULL_TREE));
    }

  /* Look for a class template declaration.  We look for hidden names
     because two friend declarations of the same template are the
     same.  For example, in:

       struct A { 
         template <typename> friend class F;
       };
       template <typename> struct B { 
         template <typename> friend class F;
       };

     both F templates are the same.  */
  tmpl = lookup_name_real (DECL_NAME (friend_tmpl), 0, 0,
			   /*block_p=*/true, 0, LOOKUP_HIDDEN);

  /* But, if we don't find one, it might be because we're in a
     situation like this:

       template <class T>
       struct S {
	 template <class U>
	 friend struct S;
       };

     Here, in the scope of (say) S<int>, `S' is bound to a TYPE_DECL
     for `S<int>', not the TEMPLATE_DECL.  */
  if (!tmpl || !DECL_CLASS_TEMPLATE_P (tmpl))
    {
      tmpl = lookup_name_prefer_type (DECL_NAME (friend_tmpl), 1);
      tmpl = maybe_get_template_decl_from_type_decl (tmpl);
    }

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
	  tree parms;
          location_t saved_input_location;
	  parms = tsubst_template_parms (DECL_TEMPLATE_PARMS (friend_tmpl),
					 args, tf_warning_or_error);

          saved_input_location = input_location;
          input_location = DECL_SOURCE_LOCATION (friend_tmpl);
	  redeclare_class_template (TREE_TYPE (tmpl), parms);
          input_location = saved_input_location;
          
	}

      friend_type = TREE_TYPE (tmpl);
    }
  else
    {
      /* The friend template has not already been declared.  In this
	 case, the instantiation of the template class will cause the
	 injection of this template into the global scope.  */
      tmpl = tsubst (friend_tmpl, args, tf_warning_or_error, NULL_TREE);
      if (tmpl == error_mark_node)
	return error_mark_node;

      /* The new TMPL is not an instantiation of anything, so we
	 forget its origins.  We don't reset CLASSTYPE_TI_TEMPLATE for
	 the new type because that is supposed to be the corresponding
	 template decl, i.e., TMPL.  */
      DECL_USE_TEMPLATE (tmpl) = 0;
      DECL_TEMPLATE_INFO (tmpl) = NULL_TREE;
      CLASSTYPE_USE_TEMPLATE (TREE_TYPE (tmpl)) = 0;
      CLASSTYPE_TI_ARGS (TREE_TYPE (tmpl))
	= INNERMOST_TEMPLATE_ARGS (CLASSTYPE_TI_ARGS (TREE_TYPE (tmpl)));

      /* Inject this template into the global scope.  */
      friend_type = TREE_TYPE (pushdecl_top_level_maybe_friend (tmpl, true));
    }

  if (context != global_namespace)
    {
      if (TREE_CODE (context) == NAMESPACE_DECL)
	pop_nested_namespace (context);
      else
	pop_nested_class ();
    }

  return friend_type;
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
  else if (TREE_CODE (type) == ARRAY_TYPE && TYPE_DOMAIN (type))
    return can_complete_type_without_circularity (TREE_TYPE (type));
  else if (CLASS_TYPE_P (type)
	   && TYPE_BEING_DEFINED (TYPE_MAIN_VARIANT (type)))
    return 0;
  else
    return 1;
}

static tree tsubst_omp_clauses (tree, bool, tree, tsubst_flags_t, tree);

/* Apply any attributes which had to be deferred until instantiation
   time.  DECL_P, ATTRIBUTES and ATTR_FLAGS are as cplus_decl_attributes;
   ARGS, COMPLAIN, IN_DECL are as tsubst.  */

static void
apply_late_template_attributes (tree *decl_p, tree attributes, int attr_flags,
				tree args, tsubst_flags_t complain, tree in_decl)
{
  tree last_dep = NULL_TREE;
  tree t;
  tree *p;

  for (t = attributes; t; t = TREE_CHAIN (t))
    if (ATTR_IS_DEPENDENT (t))
      {
	last_dep = t;
	attributes = copy_list (attributes);
	break;
      }

  if (DECL_P (*decl_p))
    {
      if (TREE_TYPE (*decl_p) == error_mark_node)
	return;
      p = &DECL_ATTRIBUTES (*decl_p);
    }
  else
    p = &TYPE_ATTRIBUTES (*decl_p);

  if (last_dep)
    {
      tree late_attrs = NULL_TREE;
      tree *q = &late_attrs;

      for (*p = attributes; *p; )
	{
	  t = *p;
	  if (ATTR_IS_DEPENDENT (t))
	    {
	      *p = TREE_CHAIN (t);
	      TREE_CHAIN (t) = NULL_TREE;
	      if ((flag_openmp || flag_cilkplus)
		  && is_attribute_p ("omp declare simd",
				     get_attribute_name (t))
		  && TREE_VALUE (t))
		{
		  tree clauses = TREE_VALUE (TREE_VALUE (t));
		  clauses = tsubst_omp_clauses (clauses, true, args,
						complain, in_decl);
		  c_omp_declare_simd_clauses_to_decls (*decl_p, clauses);
		  clauses = finish_omp_clauses (clauses);
		  tree parms = DECL_ARGUMENTS (*decl_p);
		  clauses
		    = c_omp_declare_simd_clauses_to_numbers (parms, clauses);
		  if (clauses)
		    TREE_VALUE (TREE_VALUE (t)) = clauses;
		  else
		    TREE_VALUE (t) = NULL_TREE;
		}
	      /* If the first attribute argument is an identifier, don't
		 pass it through tsubst.  Attributes like mode, format,
		 cleanup and several target specific attributes expect it
		 unmodified.  */
	      else if (attribute_takes_identifier_p (get_attribute_name (t))
		       && TREE_VALUE (t))
		{
		  tree chain
		    = tsubst_expr (TREE_CHAIN (TREE_VALUE (t)), args, complain,
				   in_decl,
				   /*integral_constant_expression_p=*/false);
		  if (chain != TREE_CHAIN (TREE_VALUE (t)))
		    TREE_VALUE (t)
		      = tree_cons (NULL_TREE, TREE_VALUE (TREE_VALUE (t)),
				   chain);
		}
	      else
		TREE_VALUE (t)
		  = tsubst_expr (TREE_VALUE (t), args, complain, in_decl,
				 /*integral_constant_expression_p=*/false);
	      *q = t;
	      q = &TREE_CHAIN (t);
	    }
	  else
	    p = &TREE_CHAIN (t);
	}

      cplus_decl_attributes (decl_p, late_attrs, attr_flags);
    }
}

/* Perform (or defer) access check for typedefs that were referenced
   from within the template TMPL code.
   This is a subroutine of instantiate_decl and instantiate_class_template.
   TMPL is the template to consider and TARGS is the list of arguments of
   that template.  */

static void
perform_typedefs_access_check (tree tmpl, tree targs)
{
  location_t saved_location;
  unsigned i;
  qualified_typedef_usage_t *iter;

  if (!tmpl
      || (!CLASS_TYPE_P (tmpl)
	  && TREE_CODE (tmpl) != FUNCTION_DECL))
    return;

  saved_location = input_location;
  FOR_EACH_VEC_SAFE_ELT (get_types_needing_access_check (tmpl), i, iter)
    {
      tree type_decl = iter->typedef_decl;
      tree type_scope = iter->context;

      if (!type_decl || !type_scope || !CLASS_TYPE_P (type_scope))
	continue;

      if (uses_template_parms (type_decl))
	type_decl = tsubst (type_decl, targs, tf_error, NULL_TREE);
      if (uses_template_parms (type_scope))
	type_scope = tsubst (type_scope, targs, tf_error, NULL_TREE);

      /* Make access check error messages point to the location
         of the use of the typedef.  */
      input_location = iter->locus;
      perform_or_defer_access_check (TYPE_BINFO (type_scope),
				     type_decl, type_decl,
				     tf_warning_or_error);
    }
    input_location = saved_location;
}

static tree
instantiate_class_template_1 (tree type)
{
  tree templ, args, pattern, t, member;
  tree typedecl;
  tree pbinfo;
  tree base_list;
  unsigned int saved_maximum_field_alignment;
  tree fn_context;

  if (type == error_mark_node)
    return error_mark_node;

  if (COMPLETE_OR_OPEN_TYPE_P (type)
      || uses_template_parms (type))
    return type;

  /* Figure out which template is being instantiated.  */
  templ = most_general_template (CLASSTYPE_TI_TEMPLATE (type));
  gcc_assert (TREE_CODE (templ) == TEMPLATE_DECL);

  /* Determine what specialization of the original template to
     instantiate.  */
  t = most_specialized_class (type, tf_warning_or_error);
  if (t == error_mark_node)
    {
      TYPE_BEING_DEFINED (type) = 1;
      return error_mark_node;
    }
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
      pattern = TREE_TYPE (t);
      args = TREE_PURPOSE (t);
    }
  else
    {
      pattern = TREE_TYPE (templ);
      args = CLASSTYPE_TI_ARGS (type);
    }

  /* If the template we're instantiating is incomplete, then clearly
     there's nothing we can do.  */
  if (!COMPLETE_TYPE_P (pattern))
    return type;

  /* If we've recursively instantiated too many templates, stop.  */
  if (! push_tinst_level (type))
    return type;

  /* Now we're really doing the instantiation.  Mark the type as in
     the process of being defined.  */
  TYPE_BEING_DEFINED (type) = 1;

  /* We may be in the middle of deferred access check.  Disable
     it now.  */
  push_deferring_access_checks (dk_no_deferred);

  fn_context = decl_function_context (TYPE_MAIN_DECL (type));
  if (!fn_context)
    push_to_top_level ();
  /* Use #pragma pack from the template context.  */
  saved_maximum_field_alignment = maximum_field_alignment;
  maximum_field_alignment = TYPE_PRECISION (pattern);

  SET_CLASSTYPE_INTERFACE_UNKNOWN (type);

  /* Set the input location to the most specialized template definition.
     This is needed if tsubsting causes an error.  */
  typedecl = TYPE_MAIN_DECL (pattern);
  input_location = DECL_SOURCE_LOCATION (TYPE_NAME (type)) =
    DECL_SOURCE_LOCATION (typedecl);

  TYPE_PACKED (type) = TYPE_PACKED (pattern);
  TYPE_ALIGN (type) = TYPE_ALIGN (pattern);
  TYPE_USER_ALIGN (type) = TYPE_USER_ALIGN (pattern);
  TYPE_FOR_JAVA (type) = TYPE_FOR_JAVA (pattern); /* For libjava's JArray<T> */
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
  if (BINFO_N_BASE_BINFOS (pbinfo))
    {
      tree pbase_binfo;
      tree pushed_scope;
      int i;

      /* We must enter the scope containing the type, as that is where
	 the accessibility of types named in dependent bases are
	 looked up from.  */
      pushed_scope = push_scope (CP_TYPE_CONTEXT (type));

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

      if (pushed_scope)
	pop_scope (pushed_scope);
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

  /* Now members are processed in the order of declaration.  */
  for (member = CLASSTYPE_DECL_LIST (pattern);
       member; member = TREE_CHAIN (member))
    {
      tree t = TREE_VALUE (member);

      if (TREE_PURPOSE (member))
	{
	  if (TYPE_P (t))
	    {
	      /* Build new CLASSTYPE_NESTED_UTDS.  */

	      tree newtag;
	      bool class_template_p;

	      class_template_p = (TREE_CODE (t) != ENUMERAL_TYPE
				  && TYPE_LANG_SPECIFIC (t)
				  && CLASSTYPE_IS_TEMPLATE (t));
	      /* If the member is a class template, then -- even after
		 substitution -- there may be dependent types in the
		 template argument list for the class.  We increment
		 PROCESSING_TEMPLATE_DECL so that dependent_type_p, as
		 that function will assume that no types are dependent
		 when outside of a template.  */
	      if (class_template_p)
		++processing_template_decl;
	      newtag = tsubst (t, args, tf_error, NULL_TREE);
	      if (class_template_p)
		--processing_template_decl;
	      if (newtag == error_mark_node)
		continue;

	      if (TREE_CODE (newtag) != ENUMERAL_TYPE)
		{
		  tree name = TYPE_IDENTIFIER (t);

		  if (class_template_p)
		    /* Unfortunately, lookup_template_class sets
		       CLASSTYPE_IMPLICIT_INSTANTIATION for a partial
		       instantiation (i.e., for the type of a member
		       template class nested within a template class.)
		       This behavior is required for
		       maybe_process_partial_specialization to work
		       correctly, but is not accurate in this case;
		       the TAG is not an instantiation of anything.
		       (The corresponding TEMPLATE_DECL is an
		       instantiation, but the TYPE is not.) */
		    CLASSTYPE_USE_TEMPLATE (newtag) = 0;

		  /* Now, we call pushtag to put this NEWTAG into the scope of
		     TYPE.  We first set up the IDENTIFIER_TYPE_VALUE to avoid
		     pushtag calling push_template_decl.  We don't have to do
		     this for enums because it will already have been done in
		     tsubst_enum.  */
		  if (name)
		    SET_IDENTIFIER_TYPE_VALUE (name, newtag);
		  pushtag (name, newtag, /*tag_scope=*/ts_current);
		}
	    }
	  else if (DECL_DECLARES_FUNCTION_P (t))
	    {
	      /* Build new TYPE_METHODS.  */
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
		mark_used (r);
	      if (TREE_CODE (r) == FUNCTION_DECL
		  && DECL_OMP_DECLARE_REDUCTION_P (r))
		cp_check_omp_declare_reduction (r);
	    }
	  else
	    {
	      /* Build new TYPE_FIELDS.  */
              if (TREE_CODE (t) == STATIC_ASSERT)
                {
                  tree condition;
 
		  ++c_inhibit_evaluation_warnings;
		  condition =
		    tsubst_expr (STATIC_ASSERT_CONDITION (t), args, 
				 tf_warning_or_error, NULL_TREE,
				 /*integral_constant_expression_p=*/true);
		  --c_inhibit_evaluation_warnings;

                  finish_static_assert (condition,
                                        STATIC_ASSERT_MESSAGE (t), 
                                        STATIC_ASSERT_SOURCE_LOCATION (t),
                                        /*member_p=*/true);
                }
	      else if (TREE_CODE (t) != CONST_DECL)
		{
		  tree r;
		  tree vec = NULL_TREE;
		  int len = 1;

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
			    mark_used (r);
			}
		      else if (TREE_CODE (r) == FIELD_DECL)
			{
			  /* Determine whether R has a valid type and can be
			     completed later.  If R is invalid, then its type
			     is replaced by error_mark_node.  */
			  tree rtype = TREE_TYPE (r);
			  if (can_complete_type_without_circularity (rtype))
			    complete_type (rtype);

			  if (!COMPLETE_TYPE_P (rtype))
			    {
			      cxx_incomplete_type_error (r, rtype);
			      TREE_TYPE (r) = error_mark_node;
			    }
			}

		      /* If it is a TYPE_DECL for a class-scoped ENUMERAL_TYPE,
			 such a thing will already have been added to the field
			 list by tsubst_enum in finish_member_declaration in the
			 CLASSTYPE_NESTED_UTDS case above.  */
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
	      bool adjust_processing_template_decl = false;

	      if (TREE_CODE (friend_type) == TEMPLATE_DECL)
		{
		  /* template <class T> friend class C;  */
		  friend_type = tsubst_friend_class (friend_type, args);
		  adjust_processing_template_decl = true;
		}
	      else if (TREE_CODE (friend_type) == UNBOUND_CLASS_TEMPLATE)
		{
		  /* template <class T> friend class C::D;  */
		  friend_type = tsubst (friend_type, args,
					tf_warning_or_error, NULL_TREE);
		  if (TREE_CODE (friend_type) == TEMPLATE_DECL)
		    friend_type = TREE_TYPE (friend_type);
		  adjust_processing_template_decl = true;
		}
	      else if (TREE_CODE (friend_type) == TYPENAME_TYPE
		       || TREE_CODE (friend_type) == TEMPLATE_TYPE_PARM)
		{
		  /* This could be either

		       friend class T::C;

		     when dependent_type_p is false or

		       template <class U> friend class T::C;

		     otherwise.  */
		  friend_type = tsubst (friend_type, args,
					tf_warning_or_error, NULL_TREE);
		  /* Bump processing_template_decl for correct
		     dependent_type_p calculation.  */
		  ++processing_template_decl;
		  if (dependent_type_p (friend_type))
		    adjust_processing_template_decl = true;
		  --processing_template_decl;
		}
	      else if (!CLASSTYPE_USE_TEMPLATE (friend_type)
		       && hidden_name_p (TYPE_NAME (friend_type)))
		{
		  /* friend class C;

		     where C hasn't been declared yet.  Let's lookup name
		     from namespace scope directly, bypassing any name that
		     come from dependent base class.  */
		  tree ns = decl_namespace_context (TYPE_MAIN_DECL (friend_type));

		  /* The call to xref_tag_from_type does injection for friend
		     classes.  */
		  push_nested_namespace (ns);
		  friend_type =
		    xref_tag_from_type (friend_type, NULL_TREE,
					/*tag_scope=*/ts_current);
		  pop_nested_namespace (ns);
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

	      if (adjust_processing_template_decl)
		/* Trick make_friend_class into realizing that the friend
		   we're adding is a template, not an ordinary class.  It's
		   important that we use make_friend_class since it will
		   perform some error-checking and output cross-reference
		   information.  */
		++processing_template_decl;

	      if (friend_type != error_mark_node)
		make_friend_class (type, friend_type, /*complain=*/false);

	      if (adjust_processing_template_decl)
		--processing_template_decl;
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

  if (tree expr = CLASSTYPE_LAMBDA_EXPR (type))
    {
      tree decl = lambda_function (type);
      if (decl)
	{
	  if (!DECL_TEMPLATE_INFO (decl)
	      || DECL_TEMPLATE_RESULT (DECL_TI_TEMPLATE (decl)) != decl)
	    instantiate_decl (decl, false, false);

	  /* We need to instantiate the capture list from the template
	     after we've instantiated the closure members, but before we
	     consider adding the conversion op.  Also keep any captures
	     that may have been added during instantiation of the op().  */
	  tree tmpl_expr = CLASSTYPE_LAMBDA_EXPR (pattern);
	  tree tmpl_cap
	    = tsubst_copy_and_build (LAMBDA_EXPR_CAPTURE_LIST (tmpl_expr),
				     args, tf_warning_or_error, NULL_TREE,
				     false, false);

	  LAMBDA_EXPR_CAPTURE_LIST (expr)
	    = chainon (tmpl_cap, nreverse (LAMBDA_EXPR_CAPTURE_LIST (expr)));

	  maybe_add_lambda_conv_op (type);
	}
      else
	gcc_assert (errorcount);
    }

  /* Set the file and line number information to whatever is given for
     the class itself.  This puts error messages involving generated
     implicit functions at a predictable point, and the same point
     that would be used for non-template classes.  */
  input_location = DECL_SOURCE_LOCATION (typedecl);

  unreverse_member_declarations (type);
  finish_struct_1 (type);
  TYPE_BEING_DEFINED (type) = 0;

  /* We don't instantiate default arguments for member functions.  14.7.1:

     The implicit instantiation of a class template specialization causes
     the implicit instantiation of the declarations, but not of the
     definitions or default arguments, of the class member functions,
     member classes, static data members and member templates....  */

  /* Some typedefs referenced from within the template code need to be access
     checked at template instantiation time, i.e now. These types were
     added to the template at parsing time. Let's get those and perform
     the access checks then.  */
  perform_typedefs_access_check (pattern, args);
  perform_deferred_access_checks (tf_warning_or_error);
  pop_nested_class ();
  maximum_field_alignment = saved_maximum_field_alignment;
  if (!fn_context)
    pop_from_top_level ();
  pop_deferring_access_checks ();
  pop_tinst_level ();

  /* The vtable for a template class can be emitted in any translation
     unit in which the class is instantiated.  When there is no key
     method, however, finish_struct_1 will already have added TYPE to
     the keyed_classes list.  */
  if (TYPE_CONTAINS_VPTR_P (type) && CLASSTYPE_KEY_METHOD (type))
    keyed_classes = tree_cons (NULL_TREE, type, keyed_classes);

  return type;
}

/* Wrapper for instantiate_class_template_1.  */

tree
instantiate_class_template (tree type)
{
  tree ret;
  timevar_push (TV_TEMPLATE_INST);
  ret = instantiate_class_template_1 (type);
  timevar_pop (TV_TEMPLATE_INST);
  return ret;
}

static tree
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
      r = tsubst_expr (t, args, complain, in_decl,
		       /*integral_constant_expression_p=*/true);
      if (!(complain & tf_warning))
	--c_inhibit_evaluation_warnings;
    }
  return r;
}

/* Given a function parameter pack TMPL_PARM and some function parameters
   instantiated from it at *SPEC_P, return a NONTYPE_ARGUMENT_PACK of them
   and set *SPEC_P to point at the next point in the list.  */

static tree
extract_fnparm_pack (tree tmpl_parm, tree *spec_p)
{
  /* Collect all of the extra "packed" parameters into an
     argument pack.  */
  tree parmvec;
  tree parmtypevec;
  tree argpack = make_node (NONTYPE_ARGUMENT_PACK);
  tree argtypepack = cxx_make_type (TYPE_ARGUMENT_PACK);
  tree spec_parm = *spec_p;
  int i, len;

  for (len = 0; spec_parm; ++len, spec_parm = TREE_CHAIN (spec_parm))
    if (tmpl_parm
	&& !function_parameter_expanded_from_pack_p (spec_parm, tmpl_parm))
      break;

  /* Fill in PARMVEC and PARMTYPEVEC with all of the parameters.  */
  parmvec = make_tree_vec (len);
  parmtypevec = make_tree_vec (len);
  spec_parm = *spec_p;
  for (i = 0; i < len; i++, spec_parm = DECL_CHAIN (spec_parm))
    {
      TREE_VEC_ELT (parmvec, i) = spec_parm;
      TREE_VEC_ELT (parmtypevec, i) = TREE_TYPE (spec_parm);
    }

  /* Build the argument packs.  */
  SET_ARGUMENT_PACK_ARGS (argpack, parmvec);
  SET_ARGUMENT_PACK_ARGS (argtypepack, parmtypevec);
  TREE_TYPE (argpack) = argtypepack;
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

/* Return true iff the Ith element of the argument pack ARG_PACK is a
   pack expansion.  */

static bool
argument_pack_element_is_expansion_p (tree arg_pack, int i)
{
  tree vec = ARGUMENT_PACK_ARGS (arg_pack);
  if (i >= TREE_VEC_LENGTH (vec))
    return false;
  return PACK_EXPANSION_P (TREE_VEC_ELT (vec, i));
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
use_pack_expansion_extra_args_p (tree parm_packs,
				 int arg_pack_len,
				 bool has_empty_arg)
{
  /* If one pack has an expansion and another pack has a normal
     argument or if one pack has an empty argument and an another
     one hasn't then tsubst_pack_expansion cannot perform the
     substitution and need to fall back on the
     PACK_EXPANSION_EXTRA mechanism.  */
  if (parm_packs == NULL_TREE)
    return false;
  else if (has_empty_arg)
    return true;

  bool has_expansion_arg = false;
  for (int i = 0 ; i < arg_pack_len; ++i)
    {
      bool has_non_expansion_arg = false;
      for (tree parm_pack = parm_packs;
	   parm_pack;
	   parm_pack = TREE_CHAIN (parm_pack))
	{
	  tree arg = TREE_VALUE (parm_pack);

	  if (argument_pack_element_is_expansion_p (arg, i))
	    has_expansion_arg = true;
	  else
	    has_non_expansion_arg = true;
	}

      if (has_expansion_arg && has_non_expansion_arg)
	return true;
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
	  || TREE_CODE (parm) == FIELD_DECL)
	{
	  if (index == 0)
	    {
	      aps = make_argument_pack_select (arg_pack, index);
	      mark_used (parm);
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
  else if (!TYPE_P (pattern))
    t = tsubst_expr (pattern, args, complain, in_decl,
		     /*integral_constant_expression_p=*/false);
  else
    t = tsubst (pattern, args, complain, in_decl);

  /*  If the Ith argument pack element is a pack expansion, then
      the Ith element resulting from the substituting is going to
      be a pack expansion as well.  */
  if (ith_elem_is_expansion)
    t = make_pack_expansion (t);

  return t;
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
  struct pointer_map_t *saved_local_specializations = NULL;
  bool need_local_specializations = false;
  int levels;

  gcc_assert (PACK_EXPANSION_P (t));
  pattern = PACK_EXPANSION_PATTERN (t);

  /* Add in any args remembered from an earlier partial instantiation.  */
  args = add_to_template_args (PACK_EXPANSION_EXTRA_ARGS (t), args);

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
         if (BASES_DIRECT (parm_pack))
           return calculate_direct_bases (tsubst_expr (BASES_TYPE (parm_pack),
                                                        args, complain, in_decl, false));
         else
           return calculate_bases (tsubst_expr (BASES_TYPE (parm_pack),
                                                 args, complain, in_decl, false));
       }
      if (TREE_CODE (parm_pack) == PARM_DECL)
	{
	  if (PACK_EXPANSION_LOCAL_P (t))
	    arg_pack = retrieve_local_specialization (parm_pack);
	  else
	    {
	      /* We can't rely on local_specializations for a parameter
		 name used later in a function declaration (such as in a
		 late-specified return type).  Even if it exists, it might
		 have the wrong value for a recursive call.  Just make a
		 dummy decl, since it's only used for its type.  */
	      arg_pack = tsubst_decl (parm_pack, args, complain);
	      if (arg_pack && DECL_PACK_P (arg_pack))
		/* Partial instantiation of the parm_pack, we can't build
		   up an argument pack yet.  */
		arg_pack = NULL_TREE;
	      else
		arg_pack = make_fnparm_pack (arg_pack);
	      need_local_specializations = true;
	    }
	}
      else if (TREE_CODE (parm_pack) == FIELD_DECL)
	arg_pack = tsubst_copy (parm_pack, args, complain, in_decl);
      else
        {
	  int idx;
          template_parm_level_and_index (parm_pack, &level, &idx);

          if (level <= levels)
            arg_pack = TMPL_ARG (args, level, idx);
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
                error ("mismatched argument pack lengths while expanding "
                       "%<%T%>",
                       pattern);
              else
                error ("mismatched argument pack lengths while expanding "
                       "%<%E%>",
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
	  unsubstituted_packs = true;
	}
    }

  /* We cannot expand this expansion expression, because we don't have
     all of the argument packs we need.  */
  if (use_pack_expansion_extra_args_p (packs, len, unsubstituted_packs))
    {
      /* We got some full packs, but we can't substitute them in until we
	 have values for all the packs.  So remember these until then.  */

      t = make_pack_expansion (pattern);
      PACK_EXPANSION_EXTRA_ARGS (t) = args;
      return t;
    }
  else if (unsubstituted_packs)
    {
      /* There were no real arguments, we're just replacing a parameter
	 pack with another version of itself. Substitute into the
	 pattern and return a PACK_EXPANSION_*. The caller will need to
	 deal with that.  */
      if (TREE_CODE (t) == EXPR_PACK_EXPANSION)
	t = tsubst_expr (pattern, args, complain, in_decl,
			 /*integral_constant_expression_p=*/false);
      else
	t = tsubst (pattern, args, complain, in_decl);
      t = make_pack_expansion (t);
      return t;
    }

  gcc_assert (len >= 0);

  if (need_local_specializations)
    {
      /* We're in a late-specified return type, so create our own local
	 specializations map; the current map is either NULL or (in the
	 case of recursive unification) might have bindings that we don't
	 want to use or alter.  */
      saved_local_specializations = local_specializations;
      local_specializations = pointer_map_create ();
    }

  /* For each argument in each argument pack, substitute into the
     pattern.  */
  result = make_tree_vec (len);
  for (i = 0; i < len; ++i)
    {
      t = gen_elem_of_pack_expansion_instantiation (pattern, packs,
						    i,
						    args, complain,
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

  if (need_local_specializations)
    {
      pointer_map_destroy (local_specializations);
      local_specializations = saved_local_specializations;
    }
  
  return result;
}

/* Given PARM_DECL PARM, find the corresponding PARM_DECL in the template
   TMPL.  We do this using DECL_PARM_INDEX, which should work even with
   parameter packs; all parms generated from a function parameter pack will
   have the same DECL_PARM_INDEX.  */

tree
get_pattern_parm (tree parm, tree tmpl)
{
  tree pattern = DECL_TEMPLATE_RESULT (tmpl);
  tree patparm;

  if (DECL_ARTIFICIAL (parm))
    {
      for (patparm = DECL_ARGUMENTS (pattern);
	   patparm; patparm = DECL_CHAIN (patparm))
	if (DECL_ARTIFICIAL (patparm)
	    && DECL_NAME (parm) == DECL_NAME (patparm))
	  break;
    }
  else
    {
      patparm = FUNCTION_FIRST_USER_PARM (DECL_TEMPLATE_RESULT (tmpl));
      patparm = chain_index (DECL_PARM_INDEX (parm)-1, patparm);
      gcc_assert (DECL_PARM_INDEX (patparm)
		  == DECL_PARM_INDEX (parm));
    }

  return patparm;
}

/* Substitute ARGS into the vector or list of template arguments T.  */

static tree
tsubst_template_args (tree t, tree args, tsubst_flags_t complain, tree in_decl)
{
  tree orig_t = t;
  int len, need_new = 0, i, expanded_len_adjust = 0, out;
  tree *elts;

  if (t == error_mark_node)
    return error_mark_node;

  len = TREE_VEC_LENGTH (t);
  elts = XALLOCAVEC (tree, len);

  for (i = 0; i < len; i++)
    {
      tree orig_arg = TREE_VEC_ELT (t, i);
      tree new_arg;

      if (TREE_CODE (orig_arg) == TREE_VEC)
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
        {
          /* Substitute into each of the arguments.  */
          new_arg = TYPE_P (orig_arg)
            ? cxx_make_type (TREE_CODE (orig_arg))
            : make_node (TREE_CODE (orig_arg));
          
          SET_ARGUMENT_PACK_ARGS (
            new_arg,
            tsubst_template_args (ARGUMENT_PACK_ARGS (orig_arg),
                                  args, complain, in_decl));

          if (ARGUMENT_PACK_ARGS (new_arg) == error_mark_node)
            new_arg = error_mark_node;

          if (TREE_CODE (new_arg) == NONTYPE_ARGUMENT_PACK) {
            TREE_TYPE (new_arg) = tsubst (TREE_TYPE (orig_arg), args,
                                          complain, in_decl);
            TREE_CONSTANT (new_arg) = TREE_CONSTANT (orig_arg);

            if (TREE_TYPE (new_arg) == error_mark_node)
              new_arg = error_mark_node;
          }
        }
      else
	new_arg = tsubst_template_arg (orig_arg, args, complain, in_decl);

      if (new_arg == error_mark_node)
	return error_mark_node;

      elts[i] = new_arg;
      if (new_arg != orig_arg)
	need_new = 1;
    }

  if (!need_new)
    return t;

  /* Make space for the expanded arguments coming from template
     argument packs.  */
  t = make_tree_vec (len + expanded_len_adjust);
  /* ORIG_T can contain TREE_VECs. That happens if ORIG_T contains the
     arguments for a member template.
     In that case each TREE_VEC in ORIG_T represents a level of template
     arguments, and ORIG_T won't carry any non defaulted argument count.
     It will rather be the nested TREE_VECs that will carry one.
     In other words, ORIG_T carries a non defaulted argument count only
     if it doesn't contain any nested TREE_VEC.  */
  if (NON_DEFAULT_TEMPLATE_ARGS_COUNT (orig_t))
    {
      int count = GET_NON_DEFAULT_TEMPLATE_ARGS_COUNT (orig_t);
      count += expanded_len_adjust;
      SET_NON_DEFAULT_TEMPLATE_ARGS_COUNT (t, count);
    }
  for (i = 0, out = 0; i < len; i++)
    {
      if ((PACK_EXPANSION_P (TREE_VEC_ELT (orig_t, i))
           || ARGUMENT_PACK_P (TREE_VEC_ELT (orig_t, i)))
          && TREE_CODE (elts[i]) == TREE_VEC)
        {
          int idx;

          /* Now expand the template argument pack "in place".  */
          for (idx = 0; idx < TREE_VEC_LENGTH (elts[i]); idx++, out++)
            TREE_VEC_ELT (t, out) = TREE_VEC_ELT (elts[i], idx);
        }
      else
        {
          TREE_VEC_ELT (t, out) = elts[i];
          out++;
        }
    }

  return t;
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
      tree new_vec =
	make_tree_vec (TREE_VEC_LENGTH (TREE_VALUE (parms)));
      int i;

      for (i = 0; i < TREE_VEC_LENGTH (new_vec); ++i)
	{
          tree tuple;

          if (parms == error_mark_node)
            continue;

          tuple = TREE_VEC_ELT (TREE_VALUE (parms), i);

          if (tuple == error_mark_node)
            continue;

	  TREE_VEC_ELT (new_vec, i) =
	    tsubst_template_parm (tuple, args, complain);
	}

      *new_parms =
	tree_cons (size_int (TMPL_PARMS_DEPTH (parms)
			     - TMPL_ARGS_DEPTH (args)),
		   new_vec, NULL_TREE);
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

  return build_tree_list (default_value, parm_decl);
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

  switch (TREE_CODE (t))
    {
    case RECORD_TYPE:
      if (TYPE_PTRMEMFUNC_P (t))
	return tsubst (TYPE_PTRMEMFUNC_FN_TYPE (t), args, complain, in_decl);

      /* Else fall through.  */
    case ENUMERAL_TYPE:
    case UNION_TYPE:
      if (TYPE_TEMPLATE_INFO (t) && uses_template_parms (t))
	{
	  tree argvec;
	  tree context;
	  tree r;
	  int saved_unevaluated_operand;
	  int saved_inhibit_evaluation_warnings;

	  /* In "sizeof(X<I>)" we need to evaluate "I".  */
	  saved_unevaluated_operand = cp_unevaluated_operand;
	  cp_unevaluated_operand = 0;
	  saved_inhibit_evaluation_warnings = c_inhibit_evaluation_warnings;
	  c_inhibit_evaluation_warnings = 0;

	  /* First, determine the context for the type we are looking
	     up.  */
	  context = TYPE_CONTEXT (t);
	  if (context && TYPE_P (context))
	    {
	      context = tsubst_aggr_type (context, args, complain,
					  in_decl, /*entering_scope=*/1);
	      /* If context is a nested class inside a class template,
	         it may still need to be instantiated (c++/33959).  */
	      context = complete_type (context);
	    }

	  /* Then, figure out what arguments are appropriate for the
	     type we are trying to find.  For example, given:

	       template <class T> struct S;
	       template <class T, class U> void f(T, U) { S<U> su; }

	     and supposing that we are instantiating f<int, double>,
	     then our ARGS will be {int, double}, but, when looking up
	     S we only want {double}.  */
	  argvec = tsubst_template_args (TYPE_TI_ARGS (t), args,
					 complain, in_decl);
	  if (argvec == error_mark_node)
	    r = error_mark_node;
	  else
	    {
	      r = lookup_template_class (t, argvec, in_decl, context,
					 entering_scope, complain);
	      r = cp_build_qualified_type_real (r, cp_type_quals (t), complain);
	    }

	  cp_unevaluated_operand = saved_unevaluated_operand;
	  c_inhibit_evaluation_warnings = saved_inhibit_evaluation_warnings;

	  return r;
	}
      else
	/* This is not a template type, so there's nothing to do.  */
	return t;

    default:
      return tsubst (t, args, complain, in_decl);
    }
}

/* Substitute into the default argument ARG (a default argument for
   FN), which has the indicated TYPE.  */

tree
tsubst_default_argument (tree fn, tree type, tree arg, tsubst_flags_t complain)
{
  tree saved_class_ptr = NULL_TREE;
  tree saved_class_ref = NULL_TREE;
  int errs = errorcount + sorrycount;

  /* This can happen in invalid code.  */
  if (TREE_CODE (arg) == DEFAULT_ARG)
    return arg;

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
  push_access_scope (fn);
  /* The "this" pointer is not valid in a default argument.  */
  if (cfun)
    {
      saved_class_ptr = current_class_ptr;
      cp_function_chain->x_current_class_ptr = NULL_TREE;
      saved_class_ref = current_class_ref;
      cp_function_chain->x_current_class_ref = NULL_TREE;
    }

  push_deferring_access_checks(dk_no_deferred);
  /* The default argument expression may cause implicitly defined
     member functions to be synthesized, which will result in garbage
     collection.  We must treat this situation as if we were within
     the body of function so as to avoid collecting live data on the
     stack.  */
  ++function_depth;
  arg = tsubst_expr (arg, DECL_TI_ARGS (fn),
		     complain, NULL_TREE,
		     /*integral_constant_expression_p=*/false);
  --function_depth;
  pop_deferring_access_checks();

  /* Restore the "this" pointer.  */
  if (cfun)
    {
      cp_function_chain->x_current_class_ptr = saved_class_ptr;
      cp_function_chain->x_current_class_ref = saved_class_ref;
    }

  if (errorcount+sorrycount > errs
      && (complain & tf_warning_or_error))
    inform (input_location,
	    "  when instantiating default argument for call to %D", fn);

  /* Make sure the default argument is reasonable.  */
  arg = check_default_argument (type, arg, complain);

  pop_access_scope (fn);

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

  for (arg = TYPE_ARG_TYPES (TREE_TYPE (fn));
       arg;
       arg = TREE_CHAIN (arg))
    if (TREE_PURPOSE (arg))
      TREE_PURPOSE (arg) = tsubst_default_argument (fn,
						    TREE_VALUE (arg),
						    TREE_PURPOSE (arg),
						    complain);
}

/* Substitute the ARGS into the T, which is a _DECL.  Return the
   result of the substitution.  Issue error and warning messages under
   control of COMPLAIN.  */

static tree
tsubst_decl (tree t, tree args, tsubst_flags_t complain)
{
#define RETURN(EXP) do { r = (EXP); goto out; } while(0)
  location_t saved_loc;
  tree r = NULL_TREE;
  tree in_decl = t;
  hashval_t hash = 0;

  /* Set the filename and linenumber to improve error-reporting.  */
  saved_loc = input_location;
  input_location = DECL_SOURCE_LOCATION (t);

  switch (TREE_CODE (t))
    {
    case TEMPLATE_DECL:
      {
	/* We can get here when processing a member function template,
	   member class template, or template template parameter.  */
	tree decl = DECL_TEMPLATE_RESULT (t);
	tree spec;
	tree tmpl_args;
	tree full_args;

	if (DECL_TEMPLATE_TEMPLATE_PARM_P (t))
	  {
	    /* Template template parameter is treated here.  */
	    tree new_type = tsubst (TREE_TYPE (t), args, complain, in_decl);
	    if (new_type == error_mark_node)
	      RETURN (error_mark_node);
	    /* If we get a real template back, return it.  This can happen in
	       the context of most_specialized_class.  */
	    if (TREE_CODE (new_type) == TEMPLATE_DECL)
	      return new_type;

	    r = copy_decl (t);
	    DECL_CHAIN (r) = NULL_TREE;
	    TREE_TYPE (r) = new_type;
	    DECL_TEMPLATE_RESULT (r)
	      = build_decl (DECL_SOURCE_LOCATION (decl),
			    TYPE_DECL, DECL_NAME (decl), new_type);
	    DECL_TEMPLATE_PARMS (r)
	      = tsubst_template_parms (DECL_TEMPLATE_PARMS (t), args,
				       complain);
	    TYPE_NAME (new_type) = r;
	    break;
	  }

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
	  RETURN (error_mark_node);

	/* If this is a default template template argument,
	   tsubst might not have changed anything.  */
	if (full_args == tmpl_args)
	  RETURN (t);

	hash = hash_tmpl_and_args (t, full_args);
	spec = retrieve_specialization (t, full_args, hash);
	if (spec != NULL_TREE)
	  {
	    r = spec;
	    break;
	  }

	/* Make a new template decl.  It will be similar to the
	   original, but will record the current template arguments.
	   We also create a new function declaration, which is just
	   like the old one, but points to this new template, rather
	   than the old one.  */
	r = copy_decl (t);
	gcc_assert (DECL_LANG_SPECIFIC (r) != 0);
	DECL_CHAIN (r) = NULL_TREE;

	DECL_TEMPLATE_INFO (r) = build_template_info (t, args);

	if (TREE_CODE (decl) == TYPE_DECL
	    && !TYPE_DECL_ALIAS_P (decl))
	  {
	    tree new_type;
	    ++processing_template_decl;
	    new_type = tsubst (TREE_TYPE (t), args, complain, in_decl);
	    --processing_template_decl;
	    if (new_type == error_mark_node)
	      RETURN (error_mark_node);

	    TREE_TYPE (r) = new_type;
	    /* For a partial specialization, we need to keep pointing to
	       the primary template.  */
	    if (!DECL_TEMPLATE_SPECIALIZATION (t))
	      CLASSTYPE_TI_TEMPLATE (new_type) = r;
	    DECL_TEMPLATE_RESULT (r) = TYPE_MAIN_DECL (new_type);
	    DECL_TI_ARGS (r) = CLASSTYPE_TI_ARGS (new_type);
	    DECL_CONTEXT (r) = TYPE_CONTEXT (new_type);
	  }
	else
	  {
	    tree new_decl;
	    ++processing_template_decl;
	    new_decl = tsubst (decl, args, complain, in_decl);
	    --processing_template_decl;
	    if (new_decl == error_mark_node)
	      RETURN (error_mark_node);

	    DECL_TEMPLATE_RESULT (r) = new_decl;
	    DECL_TI_TEMPLATE (new_decl) = r;
	    TREE_TYPE (r) = TREE_TYPE (new_decl);
	    DECL_TI_ARGS (r) = DECL_TI_ARGS (new_decl);
	    DECL_CONTEXT (r) = DECL_CONTEXT (new_decl);
	  }

	SET_DECL_IMPLICIT_INSTANTIATION (r);
	DECL_TEMPLATE_INSTANTIATIONS (r) = NULL_TREE;
	DECL_TEMPLATE_SPECIALIZATIONS (r) = NULL_TREE;

	/* The template parameters for this new template are all the
	   template parameters for the old template, except the
	   outermost level of parameters.  */
	DECL_TEMPLATE_PARMS (r)
	  = tsubst_template_parms (DECL_TEMPLATE_PARMS (t), args,
				   complain);

	if (PRIMARY_TEMPLATE_P (t))
	  DECL_PRIMARY_TEMPLATE (r) = r;

	if (TREE_CODE (decl) != TYPE_DECL)
	  /* Record this non-type partial instantiation.  */
	  register_specialization (r, t,
				   DECL_TI_ARGS (DECL_TEMPLATE_RESULT (r)),
				   false, hash);
      }
      break;

    case FUNCTION_DECL:
      {
	tree ctx;
	tree argvec = NULL_TREE;
	tree *friends;
	tree gen_tmpl;
	tree type;
	int member;
	int args_depth;
	int parms_depth;

	/* Nobody should be tsubst'ing into non-template functions.  */
	gcc_assert (DECL_TEMPLATE_INFO (t) != NULL_TREE);

	if (TREE_CODE (DECL_TI_TEMPLATE (t)) == TEMPLATE_DECL)
	  {
	    tree spec;
	    bool dependent_p;

	    /* If T is not dependent, just return it.  We have to
	       increment PROCESSING_TEMPLATE_DECL because
	       value_dependent_expression_p assumes that nothing is
	       dependent when PROCESSING_TEMPLATE_DECL is zero.  */
	    ++processing_template_decl;
	    dependent_p = value_dependent_expression_p (t);
	    --processing_template_decl;
	    if (!dependent_p)
	      RETURN (t);

	    /* Calculate the most general template of which R is a
	       specialization, and the complete set of arguments used to
	       specialize R.  */
	    gen_tmpl = most_general_template (DECL_TI_TEMPLATE (t));
	    argvec = tsubst_template_args (DECL_TI_ARGS
                                          (DECL_TEMPLATE_RESULT
                                                 (DECL_TI_TEMPLATE (t))),
					   args, complain, in_decl);
	    if (argvec == error_mark_node)
	      RETURN (error_mark_node);

	    /* Check to see if we already have this specialization.  */
	    hash = hash_tmpl_and_args (gen_tmpl, argvec);
	    spec = retrieve_specialization (gen_tmpl, argvec, hash);

	    if (spec)
	      {
		r = spec;
		break;
	      }

	    /* We can see more levels of arguments than parameters if
	       there was a specialization of a member template, like
	       this:

		 template <class T> struct S { template <class U> void f(); }
		 template <> template <class U> void S<int>::f(U);

	       Here, we'll be substituting into the specialization,
	       because that's where we can find the code we actually
	       want to generate, but we'll have enough arguments for
	       the most general template.

	       We also deal with the peculiar case:

		 template <class T> struct S {
		   template <class U> friend void f();
		 };
		 template <class U> void f() {}
		 template S<int>;
		 template void f<double>();

	       Here, the ARGS for the instantiation of will be {int,
	       double}.  But, we only need as many ARGS as there are
	       levels of template parameters in CODE_PATTERN.  We are
	       careful not to get fooled into reducing the ARGS in
	       situations like:

		 template <class T> struct S { template <class U> void f(U); }
		 template <class T> template <> void S<T>::f(int) {}

	       which we can spot because the pattern will be a
	       specialization in this case.  */
	    args_depth = TMPL_ARGS_DEPTH (args);
	    parms_depth =
	      TMPL_PARMS_DEPTH (DECL_TEMPLATE_PARMS (DECL_TI_TEMPLATE (t)));
	    if (args_depth > parms_depth
		&& !DECL_TEMPLATE_SPECIALIZATION (t))
	      args = get_innermost_template_args (args, parms_depth);
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
	    gen_tmpl = NULL_TREE;
	  }

	if (DECL_CLASS_SCOPE_P (t))
	  {
	    if (DECL_NAME (t) == constructor_name (DECL_CONTEXT (t)))
	      member = 2;
	    else
	      member = 1;
	    ctx = tsubst_aggr_type (DECL_CONTEXT (t), args,
				    complain, t, /*entering_scope=*/1);
	  }
	else
	  {
	    member = 0;
	    ctx = DECL_CONTEXT (t);
	  }
	type = tsubst (TREE_TYPE (t), args, complain, in_decl);
	if (type == error_mark_node)
	  RETURN (error_mark_node);

	/* If we hit excessive deduction depth, the type is bogus even if
	   it isn't error_mark_node, so don't build a decl.  */
	if (excessive_deduction_depth)
	  RETURN (error_mark_node);

	/* We do NOT check for matching decls pushed separately at this
	   point, as they may not represent instantiations of this
	   template, and in any case are considered separate under the
	   discrete model.  */
	r = copy_decl (t);
	DECL_USE_TEMPLATE (r) = 0;
	TREE_TYPE (r) = type;
	/* Clear out the mangled name and RTL for the instantiation.  */
	SET_DECL_ASSEMBLER_NAME (r, NULL_TREE);
	SET_DECL_RTL (r, NULL);
	/* Leave DECL_INITIAL set on deleted instantiations.  */
	if (!DECL_DELETED_FN (r))
	  DECL_INITIAL (r) = NULL_TREE;
	DECL_CONTEXT (r) = ctx;

	/* OpenMP UDRs have the only argument a reference to the declared
	   type.  We want to diagnose if the declared type is a reference,
	   which is invalid, but as references to references are usually
	   quietly merged, diagnose it here.  */
	if (DECL_OMP_DECLARE_REDUCTION_P (t))
	  {
	    tree argtype
	      = TREE_TYPE (TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (t))));
	    argtype = tsubst (argtype, args, complain, in_decl);
	    if (TREE_CODE (argtype) == REFERENCE_TYPE)
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
	  DECL_NAME (r) = mangle_conv_op_name_for_type (TREE_TYPE (type));

	DECL_ARGUMENTS (r) = tsubst (DECL_ARGUMENTS (t), args,
				     complain, t);
	DECL_RESULT (r) = NULL_TREE;

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
	    && IDENTIFIER_OPNAME_P (DECL_NAME (r))
	    && !grok_op_properties (r, /*complain=*/false))
	  RETURN (error_mark_node);

	/* Set up the DECL_TEMPLATE_INFO for R.  There's no need to do
	   this in the special friend case mentioned above where
	   GEN_TMPL is NULL.  */
	if (gen_tmpl)
	  {
	    DECL_TEMPLATE_INFO (r)
	      = build_template_info (gen_tmpl, argvec);
	    SET_DECL_IMPLICIT_INSTANTIATION (r);

	    tree new_r
	      = register_specialization (r, gen_tmpl, argvec, false, hash);
	    if (new_r != r)
	      /* We instantiated this while substituting into
		 the type earlier (template/friend54.C).  */
	      RETURN (new_r);

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
	else
	  DECL_TEMPLATE_INFO (r) = NULL_TREE;

	/* Copy the list of befriending classes.  */
	for (friends = &DECL_BEFRIENDING_CLASSES (r);
	     *friends;
	     friends = &TREE_CHAIN (*friends))
	  {
	    *friends = copy_node (*friends);
	    TREE_VALUE (*friends) = tsubst (TREE_VALUE (*friends),
					    args, complain,
					    in_decl);
	  }

	if (DECL_CONSTRUCTOR_P (r) || DECL_DESTRUCTOR_P (r))
	  {
	    maybe_retrofit_in_chrg (r);
	    if (DECL_CONSTRUCTOR_P (r))
	      grok_ctor_properties (ctx, r);
	    if (DECL_INHERITED_CTOR_BASE (r))
	      deduce_inheriting_ctor (r);
	    /* If this is an instantiation of a member template, clone it.
	       If it isn't, that'll be handled by
	       clone_constructors_and_destructors.  */
	    if (PRIMARY_TEMPLATE_P (gen_tmpl))
	      clone_function_decl (r, /*update_method_vec_p=*/0);
	  }
	else if ((complain & tf_error) != 0
		 && IDENTIFIER_OPNAME_P (DECL_NAME (r))
		 && !grok_op_properties (r, /*complain=*/true))
	  RETURN (error_mark_node);

	if (DECL_FRIEND_P (t) && DECL_FRIEND_CONTEXT (t))
	  SET_DECL_FRIEND_CONTEXT (r,
				   tsubst (DECL_FRIEND_CONTEXT (t),
					    args, complain, in_decl));

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

	apply_late_template_attributes (&r, DECL_ATTRIBUTES (r), 0,
					args, complain, in_decl);
      }
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
                if (len == 0)
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

	    apply_late_template_attributes (&r, DECL_ATTRIBUTES (r), 0,
					    args, complain, in_decl);

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
	       the expanded fields to instantiate_class_template_1 and
	       store them in the specializations hash table as a
	       NONTYPE_ARGUMENT_PACK so that tsubst_copy can find them.  */
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
	      /* For bit-fields, DECL_INITIAL gives the number of bits.  For
		 non-bit-fields DECL_INITIAL is a non-static data member
		 initializer, which gets deferred instantiation.  */
	      DECL_INITIAL (r)
		= tsubst_expr (DECL_INITIAL (t), args,
			       complain, in_decl,
			       /*integral_constant_expression_p=*/true);
	    else if (DECL_INITIAL (t))
	      {
		/* Set up DECL_TEMPLATE_INFO so that we can get at the
		   NSDMI in perform_member_init.  Still set DECL_INITIAL
		   so that we know there is one.  */
		DECL_INITIAL (r) = void_zero_node;
		gcc_assert (DECL_LANG_SPECIFIC (r) == NULL);
		retrofit_lang_decl (r);
		DECL_TEMPLATE_INFO (r) = build_template_info (t, args);
	      }
	    /* We don't have to set DECL_CONTEXT here; it is set by
	       finish_member_declaration.  */
	    DECL_CHAIN (r) = NULL_TREE;

	    apply_late_template_attributes (&r, DECL_ATTRIBUTES (r), 0,
					    args, complain, in_decl);

	    if (vec)
	      TREE_VEC_ELT (vec, i) = r;
	  }

	if (vec)
	  {
	    r = vec;
	    tree pack = make_node (NONTYPE_ARGUMENT_PACK);
	    tree tpack = cxx_make_type (TYPE_ARGUMENT_PACK);
	    SET_ARGUMENT_PACK_ARGS (pack, vec);
	    SET_ARGUMENT_PACK_ARGS (tpack, expanded_types);
	    TREE_TYPE (pack) = tpack;
	    register_specialization (pack, t, args, false, 0);
	  }
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
	  tree inst_scope = tsubst_copy (USING_DECL_SCOPE (t), args,
					 complain, in_decl);
	  tree name = tsubst_copy (DECL_NAME (t), args, complain, in_decl);
	  r = do_class_using_decl (inst_scope, name);
	  if (!r)
	    r = error_mark_node;
	  else
	    {
	      TREE_PROTECTED (r) = TREE_PROTECTED (t);
	      TREE_PRIVATE (r) = TREE_PRIVATE (t);
	    }
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
	tree spec;
	tree tmpl = NULL_TREE;
	tree ctx;
	tree type = NULL_TREE;
	bool local_p;

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
	spec = NULL_TREE;
	if (DECL_CLASS_SCOPE_P (t) || DECL_NAMESPACE_SCOPE_P (t))
	  {
	    /* T is a static data member or namespace-scope entity.
	       We have to substitute into namespace-scope variables
	       (even though such entities are never templates) because
	       of cases like:
	       
	         template <class T> void f() { extern T t; }

	       where the entity referenced is not known until
	       instantiation time.  */
	    local_p = false;
	    ctx = DECL_CONTEXT (t);
	    if (DECL_CLASS_SCOPE_P (t))
	      {
		ctx = tsubst_aggr_type (ctx, args,
					complain,
					in_decl, /*entering_scope=*/1);
		/* If CTX is unchanged, then T is in fact the
		   specialization we want.  That situation occurs when
		   referencing a static data member within in its own
		   class.  We can use pointer equality, rather than
		   same_type_p, because DECL_CONTEXT is always
		   canonical...  */
		if (ctx == DECL_CONTEXT (t)
		    && (TREE_CODE (t) != TYPE_DECL
			/* ... unless T is a member template; in which
			   case our caller can be willing to create a
			   specialization of that template represented
			   by T.  */
			|| !(DECL_TI_TEMPLATE (t)
			     && DECL_MEMBER_TEMPLATE_P (DECL_TI_TEMPLATE (t)))))
		  spec = t;
	      }

	    if (!spec)
	      {
		tmpl = DECL_TI_TEMPLATE (t);
		gen_tmpl = most_general_template (tmpl);
		argvec = tsubst (DECL_TI_ARGS (t), args, complain, in_decl);
		if (argvec == error_mark_node)
		  RETURN (error_mark_node);
		hash = hash_tmpl_and_args (gen_tmpl, argvec);
		spec = retrieve_specialization (gen_tmpl, argvec, hash);
	      }
	  }
	else
	  {
	    /* A local variable.  */
	    local_p = true;
	    /* Subsequent calls to pushdecl will fill this in.  */
	    ctx = NULL_TREE;
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
	r = copy_decl (t);
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
	    type = tsubst (type, args, complain, in_decl);
	  }
	if (VAR_P (r))
	  {
	    /* Even if the original location is out of scope, the
	       newly substituted one is not.  */
	    DECL_DEAD_FOR_LOCAL (r) = 0;
	    DECL_INITIALIZED_P (r) = 0;
	    DECL_TEMPLATE_INSTANTIATED (r) = 0;
	    if (type == error_mark_node)
	      RETURN (error_mark_node);
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
	    type = check_var_type (DECL_NAME (r), type);

	    if (DECL_HAS_VALUE_EXPR_P (t))
	      {
		tree ve = DECL_VALUE_EXPR (t);
		ve = tsubst_expr (ve, args, complain, in_decl,
				  /*constant_expression_p=*/false);
		if (REFERENCE_REF_P (ve))
		  {
		    gcc_assert (TREE_CODE (type) == REFERENCE_TYPE);
		    ve = TREE_OPERAND (ve, 0);
		  }
		SET_DECL_VALUE_EXPR (r, ve);
	      }
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
	/* The initializer must not be expanded until it is required;
	   see [temp.inst].  */
	DECL_INITIAL (r) = NULL_TREE;
	if (CODE_CONTAINS_STRUCT (TREE_CODE (t), TS_DECL_WRTL))
	  SET_DECL_RTL (r, NULL);
	DECL_SIZE (r) = DECL_SIZE_UNIT (r) = 0;
	if (VAR_P (r))
	  {
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

	    register_specialization (r, gen_tmpl, argvec, false, hash);
	    DECL_TEMPLATE_INFO (r) = build_template_info (tmpl, argvec);
	    SET_DECL_IMPLICIT_INSTANTIATION (r);
	  }
	else if (!cp_unevaluated_operand)
	  register_local_specialization (r, t);

	DECL_CHAIN (r) = NULL_TREE;

	apply_late_template_attributes (&r, DECL_ATTRIBUTES (r),
					/*flags=*/0,
					args, complain, in_decl);

	/* Preserve a typedef that names a type.  */
	if (is_typedef_decl (r))
	  {
	    DECL_ORIGINAL_TYPE (r) = NULL_TREE;
	    set_underlying_type (r);
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
  tree remaining_arg_types;
  tree type = NULL_TREE;
  int i = 1;
  tree expanded_args = NULL_TREE;
  tree default_arg;

  if (!arg_types || arg_types == void_list_node || arg_types == end)
    return arg_types;

  remaining_arg_types = tsubst_arg_types (TREE_CHAIN (arg_types),
					  args, end, complain, in_decl);
  if (remaining_arg_types == error_mark_node)
    return error_mark_node;

  if (PACK_EXPANSION_P (TREE_VALUE (arg_types)))
    {
      /* For a pack expansion, perform substitution on the
         entire expression. Later on, we'll handle the arguments
         one-by-one.  */
      expanded_args = tsubst_pack_expansion (TREE_VALUE (arg_types),
                                            args, complain, in_decl);

      if (TREE_CODE (expanded_args) == TREE_VEC)
        /* So that we'll spin through the parameters, one by one.  */
        i = TREE_VEC_LENGTH (expanded_args);
      else
        {
          /* We only partially substituted into the parameter
             pack. Our type is TYPE_PACK_EXPANSION.  */
          type = expanded_args;
          expanded_args = NULL_TREE;
        }
    }

  while (i > 0) {
    --i;
    
    if (expanded_args)
      type = TREE_VEC_ELT (expanded_args, i);
    else if (!type)
      type = tsubst (TREE_VALUE (arg_types), args, complain, in_decl);

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
    /* DR 657. */
    if (abstract_virtuals_error_sfinae (ACU_PARM, type, complain))
      return error_mark_node;
    
    /* Do array-to-pointer, function-to-pointer conversion, and ignore
       top-level qualifiers as required.  */
    type = cv_unqualified (type_decays_to (type));

    /* We do not substitute into default arguments here.  The standard
       mandates that they be instantiated only when needed, which is
       done in build_over_call.  */
    default_arg = TREE_PURPOSE (arg_types);

    if (default_arg && TREE_CODE (default_arg) == DEFAULT_ARG)
      {
        /* We've instantiated a template before its default arguments
           have been parsed.  This can happen for a nested template
           class, and is not an error unless we require the default
           argument in a call of this function.  */
        remaining_arg_types = 
          tree_cons (default_arg, type, remaining_arg_types);
        vec_safe_push (DEFARG_INSTANTIATIONS(default_arg), remaining_arg_types);
      }
    else
      remaining_arg_types = 
        hash_tree_cons (default_arg, type, remaining_arg_types);
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
  tree arg_types;
  tree fntype;

  /* The TYPE_CONTEXT is not used for function/method types.  */
  gcc_assert (TYPE_CONTEXT (t) == NULL_TREE);

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
  /* And DR 657. */
  if (abstract_virtuals_error_sfinae (ACU_RETURN, return_type, complain))
    return error_mark_node;

  /* Substitute the argument types.  */
  arg_types = tsubst_arg_types (TYPE_ARG_TYPES (t), args, NULL_TREE,
				complain, in_decl);
  if (arg_types == error_mark_node)
    return error_mark_node;

  /* Construct a new type node and return it.  */
  if (TREE_CODE (t) == FUNCTION_TYPE)
    {
      fntype = build_function_type (return_type, arg_types);
      fntype = apply_memfn_quals (fntype,
				  type_memfn_quals (t),
				  type_memfn_rqual (t));
    }
  else
    {
      tree r = TREE_TYPE (TREE_VALUE (arg_types));
      /* Don't pick up extra function qualifiers from the basetype.  */
      r = cp_build_qualified_type_real (r, type_memfn_quals (t), complain);
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

      fntype = build_method_type_directly (r, return_type,
					   TREE_CHAIN (arg_types));
      fntype = build_ref_qualified_type (fntype, type_memfn_rqual (t));
    }
  fntype = cp_build_type_attribute_variant (fntype, TYPE_ATTRIBUTES (t));

  return fntype;
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
	new_specs = tsubst_copy_and_build
	  (expr, args, complain, in_decl, /*function_p=*/false,
	   /*integral_constant_expression_p=*/true);
      new_specs = build_noexcept_spec (new_specs, complain);
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

  if (DECL_P (t))
    return tsubst_decl (t, args, complain);

  if (args == NULL_TREE)
    return t;

  code = TREE_CODE (t);

  if (code == IDENTIFIER_NODE)
    type = IDENTIFIER_TYPE_VALUE (t);
  else
    type = TREE_TYPE (t);

  gcc_assert (type != unknown_type_node);

  /* Reuse typedefs.  We need to do this to handle dependent attributes,
     such as attribute aligned.  */
  if (TYPE_P (t)
      && typedef_variant_p (t))
    {
      tree decl = TYPE_NAME (t);

      if (alias_template_specialization_p (t))
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
	  r = cp_build_qualified_type_real
	    (r, cp_type_quals (t) | cp_type_quals (r),
	     complain | tf_ignore_bad_quals);
	  return r;
	}
      else
	{
	  /* We don't have an instantiation yet, so drop the typedef.  */
	  int quals = cp_type_quals (t);
	  t = DECL_ORIGINAL_TYPE (decl);
	  t = cp_build_qualified_type_real (t, quals,
					    complain | tf_ignore_bad_quals);
	}
    }

  if (type
      && code != TYPENAME_TYPE
      && code != TEMPLATE_TYPE_PARM
      && code != IDENTIFIER_NODE
      && code != FUNCTION_TYPE
      && code != METHOD_TYPE)
    type = tsubst (type, args, complain, in_decl);
  if (type == error_mark_node)
    return error_mark_node;

  switch (code)
    {
    case RECORD_TYPE:
    case UNION_TYPE:
    case ENUMERAL_TYPE:
      return tsubst_aggr_type (t, args, complain, in_decl,
			       /*entering_scope=*/0);

    case ERROR_MARK:
    case IDENTIFIER_NODE:
    case VOID_TYPE:
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

	max = tsubst_expr (omax, args, complain, in_decl,
			   /*integral_constant_expression_p=*/false);

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
	if (level <= levels)
	  {
	    arg = TMPL_ARG (args, level, idx);

	    if (arg && TREE_CODE (arg) == ARGUMENT_PACK_SELECT)
	      {
		/* See through ARGUMENT_PACK_SELECT arguments. */
		arg = ARGUMENT_PACK_SELECT_ARG (arg);
		/* If the selected argument is an expansion E, that most
		   likely means we were called from
		   gen_elem_of_pack_expansion_instantiation during the
		   substituting of pack an argument pack (which Ith
		   element is a pack expansion, where I is
		   ARGUMENT_PACK_SELECT_INDEX) into a pack expansion.
		   In this case, the Ith element resulting from this
		   substituting is going to be a pack expansion, which
		   pattern is the pattern of E.  Let's return the
		   pattern of E, and
		   gen_elem_of_pack_expansion_instantiation will
		   build the resulting pack expansion from it.  */
		if (PACK_EXPANSION_P (arg))
		  arg = PACK_EXPANSION_PATTERN (arg);
	      }
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
		gcc_assert (TYPE_P (arg));

		quals = cp_type_quals (arg) | cp_type_quals (t);
		  
		return cp_build_qualified_type_real
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
		return cp_build_qualified_type_real
		  (r, cp_type_quals (t) | cp_type_quals (r), complain);
	      }
	    else
	      /* TEMPLATE_TEMPLATE_PARM or TEMPLATE_PARM_INDEX.  */
	      return convert_from_reference (unshare_expr (arg));
	  }

	if (level == 1)
	  /* This can happen during the attempted tsubst'ing in
	     unify.  This means that we don't yet have any information
	     about the template parameter in question.  */
	  return t;

	/* Early in template argument deduction substitution, we don't
	   want to reduce the level of 'auto', or it will be confused
	   with a normal template parm in subsequent deduction.  */
	if (is_auto (t) && (complain & tf_partial))
	  return t;

	/* If we get here, we must have been looking at a parm for a
	   more deeply nested template.  Make a new version of this
	   template parameter, but with a lower level.  */
	switch (code)
	  {
	  case TEMPLATE_TYPE_PARM:
	  case TEMPLATE_TEMPLATE_PARM:
	  case BOUND_TEMPLATE_TEMPLATE_PARM:
	    if (cp_type_quals (t))
	      {
		r = tsubst (TYPE_MAIN_VARIANT (t), args, complain, in_decl);
		r = cp_build_qualified_type_real
		  (r, cp_type_quals (t),
		   complain | (code == TEMPLATE_TYPE_PARM
			       ? tf_ignore_bad_quals : 0));
	      }
	    else
	      {
		r = copy_type (t);
		TEMPLATE_TYPE_PARM_INDEX (r)
		  = reduce_template_parm_level (TEMPLATE_TYPE_PARM_INDEX (t),
						r, levels, args, complain);
		TYPE_STUB_DECL (r) = TYPE_NAME (r) = TEMPLATE_TYPE_DECL (r);
		TYPE_MAIN_VARIANT (r) = r;
		TYPE_POINTER_TO (r) = NULL_TREE;
		TYPE_REFERENCE_TO (r) = NULL_TREE;

		if (TREE_CODE (r) == TEMPLATE_TEMPLATE_PARM)
		  /* We have reduced the level of the template
		     template parameter, but not the levels of its
		     template parameters, so canonical_type_parameter
		     will not be able to find the canonical template
		     template parameter for this level. Thus, we
		     require structural equality checking to compare
		     TEMPLATE_TEMPLATE_PARMs. */
		  SET_TYPE_STRUCTURAL_EQUALITY (r);
		else if (TYPE_STRUCTURAL_EQUALITY_P (t))
		  SET_TYPE_STRUCTURAL_EQUALITY (r);
		else
		  TYPE_CANONICAL (r) = canonical_type_parameter (r);

		if (code == BOUND_TEMPLATE_TEMPLATE_PARM)
		  {
		    tree argvec = tsubst (TYPE_TI_ARGS (t), args,
					  complain, in_decl);
		    if (argvec == error_mark_node)
		      return error_mark_node;

		    TEMPLATE_TEMPLATE_PARM_TEMPLATE_INFO (r)
		      = build_template_info (TYPE_TI_TEMPLATE (t), argvec);
		  }
	      }
	    break;

	  case TEMPLATE_PARM_INDEX:
	    r = reduce_template_parm_level (t, type, levels, args, complain);
	    break;

	  default:
	    gcc_unreachable ();
	  }

	return r;
      }

    case TREE_LIST:
      {
	tree purpose, value, chain;

	if (t == void_list_node)
	  return t;

	purpose = TREE_PURPOSE (t);
	if (purpose)
	  {
	    purpose = tsubst (purpose, args, complain, in_decl);
	    if (purpose == error_mark_node)
	      return error_mark_node;
	  }
	value = TREE_VALUE (t);
	if (value)
	  {
	    value = tsubst (value, args, complain, in_decl);
	    if (value == error_mark_node)
	      return error_mark_node;
	  }
	chain = TREE_CHAIN (t);
	if (chain && chain != void_type_node)
	  {
	    chain = tsubst (chain, args, complain, in_decl);
	    if (chain == error_mark_node)
	      return error_mark_node;
	  }
	if (purpose == TREE_PURPOSE (t)
	    && value == TREE_VALUE (t)
	    && chain == TREE_CHAIN (t))
	  return t;
	return hash_tree_cons (purpose, value, chain);
      }

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
	if ((TREE_CODE (type) == REFERENCE_TYPE
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
	else if (TREE_CODE (type) == REFERENCE_TYPE)
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
	r = cp_build_qualified_type_real (r, cp_type_quals (t), complain);

	if (cxx_dialect >= cxx1y
	    && !(TREE_CODE (t) == REFERENCE_TYPE && REFERENCE_VLA_OK (t))
	    && array_of_runtime_bound_p (type)
	    && (flag_iso || warn_vla > 0))
	  {
	    if (complain & tf_warning_or_error)
	      pedwarn
		(input_location, OPT_Wvla,
		 code == REFERENCE_TYPE
		 ? G_("cannot declare reference to array of runtime bound")
		 : G_("cannot declare pointer to array of runtime bound"));
	    else
	      r = error_mark_node;
	  }

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
	if (TREE_CODE (type) == REFERENCE_TYPE)
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
	    return cp_build_qualified_type_real (memptr, cp_type_quals (t),
						 complain);
	  }
	else
	  return cp_build_qualified_type_real (build_ptrmem_type (r, type),
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
	specs = tsubst_exception_specification (t, args, complain,
						in_decl, /*defer_ok*/true);
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

	/* These checks should match the ones in grokdeclarator.

	   [temp.deduct]

	   The deduction may fail for any of the following reasons:

	   -- Attempting to create an array with an element type that
	      is void, a function type, or a reference type, or [DR337]
	      an abstract class type.  */
	if (VOID_TYPE_P (type)
	    || TREE_CODE (type) == FUNCTION_TYPE
	    || TREE_CODE (type) == REFERENCE_TYPE)
	  {
	    if (complain & tf_error)
	      error ("creating array of %qT", type);
	    return error_mark_node;
	  }

	if (abstract_virtuals_error_sfinae (ACU_ARRAY, type, complain))
	  return error_mark_node;

	r = build_cplus_array_type (type, domain);

	if (TYPE_USER_ALIGN (t))
	  {
	    TYPE_ALIGN (r) = TYPE_ALIGN (t);
	    TYPE_USER_ALIGN (r) = 1;
	  }

	return r;
      }

    case TYPENAME_TYPE:
      {
	tree ctx = tsubst_aggr_type (TYPE_CONTEXT (t), args, complain,
				     in_decl, /*entering_scope=*/1);
	tree f = tsubst_copy (TYPENAME_TYPE_FULLNAME (t), args,
			      complain, in_decl);

	if (ctx == error_mark_node || f == error_mark_node)
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
	    ctx = complete_type (ctx);
	    if (!COMPLETE_TYPE_P (ctx))
	      {
		if (complain & tf_error)
		  cxx_incomplete_type_error (NULL_TREE, ctx);
		return error_mark_node;
	      }
	  }

	f = make_typename_type (ctx, f, typename_type,
				complain | tf_keep_type_decl);
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
		  error ("%qT resolves to %qT, which is is not a class type",
			 t, f);
		else
		  return error_mark_node;
	      }
	  }

	return cp_build_qualified_type_real
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

	type = tsubst_expr (TYPEOF_TYPE_EXPR (t), args,
			    complain, in_decl,
			    /*integral_constant_expression_p=*/false);

	--cp_unevaluated_operand;
	--c_inhibit_evaluation_warnings;

	type = finish_typeof (type);
	return cp_build_qualified_type_real (type,
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
				      complain|tf_decltype, in_decl,
				      /*function_p*/false,
				      /*integral_constant_expression*/false);

	--cp_unevaluated_operand;
	--c_inhibit_evaluation_warnings;

	if (DECLTYPE_FOR_LAMBDA_CAPTURE (t))
	  type = lambda_capture_field_type (type,
					    DECLTYPE_FOR_INIT_CAPTURE (t));
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
	return cp_build_qualified_type_real (type,
					     cp_type_quals (t)
					     | cp_type_quals (type),
					     complain);
      }

    case UNDERLYING_TYPE:
      {
	tree type = tsubst (UNDERLYING_TYPE_TYPE (t), args,
			    complain, in_decl);
	return finish_underlying_type (type);
      }

    case TYPE_ARGUMENT_PACK:
    case NONTYPE_ARGUMENT_PACK:
      {
        tree r = TYPE_P (t) ? cxx_make_type (code) : make_node (code);
        tree packed_out = 
          tsubst_template_args (ARGUMENT_PACK_ARGS (t), 
                                args,
                                complain,
                                in_decl);
        SET_ARGUMENT_PACK_ARGS (r, packed_out);

        /* For template nontype argument packs, also substitute into
           the type.  */
        if (code == NONTYPE_ARGUMENT_PACK)
          TREE_TYPE (r) = tsubst (TREE_TYPE (t), args, complain, in_decl);

        return r;
      }
      break;

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

/* Like tsubst_expr for a BASELINK.  OBJECT_TYPE, if non-NULL, is the
   type of the expression on the left-hand side of the "." or "->"
   operator.  */

static tree
tsubst_baselink (tree baselink, tree object_type,
		 tree args, tsubst_flags_t complain, tree in_decl)
{
    tree name;
    tree qualifying_scope;
    tree fns;
    tree optype;
    tree template_args = 0;
    bool template_id_p = false;
    bool qualified = BASELINK_QUALIFIED_P (baselink);

    /* A baselink indicates a function from a base class.  Both the
       BASELINK_ACCESS_BINFO and the base class referenced may
       indicate bases of the template class, rather than the
       instantiated class.  In addition, lookups that were not
       ambiguous before may be ambiguous now.  Therefore, we perform
       the lookup again.  */
    qualifying_scope = BINFO_TYPE (BASELINK_ACCESS_BINFO (baselink));
    qualifying_scope = tsubst (qualifying_scope, args,
			       complain, in_decl);
    fns = BASELINK_FUNCTIONS (baselink);
    optype = tsubst (BASELINK_OPTYPE (baselink), args, complain, in_decl);
    if (TREE_CODE (fns) == TEMPLATE_ID_EXPR)
      {
	template_id_p = true;
	template_args = TREE_OPERAND (fns, 1);
	fns = TREE_OPERAND (fns, 0);
	if (template_args)
	  template_args = tsubst_template_args (template_args, args,
						complain, in_decl);
      }
    name = DECL_NAME (get_first_fn (fns));
    if (IDENTIFIER_TYPENAME_P (name))
      name = mangle_conv_op_name_for_type (optype);
    baselink = lookup_fnfields (qualifying_scope, name, /*protect=*/1);
    if (!baselink)
      return error_mark_node;

    /* If lookup found a single function, mark it as used at this
       point.  (If it lookup found multiple functions the one selected
       later by overload resolution will be marked as used at that
       point.)  */
    if (BASELINK_P (baselink))
      fns = BASELINK_FUNCTIONS (baselink);
    if (!template_id_p && !really_overloaded_fn (fns))
      mark_used (OVL_CURRENT (fns));

    /* Add back the template arguments, if present.  */
    if (BASELINK_P (baselink) && template_id_p)
      BASELINK_FUNCTIONS (baselink)
	= build_nt (TEMPLATE_ID_EXPR,
		    BASELINK_FUNCTIONS (baselink),
		    template_args);
    /* Update the conversion operator type.  */
    BASELINK_OPTYPE (baselink) = optype;

    if (!object_type)
      object_type = current_class_type;

    if (qualified)
      baselink = adjust_result_of_qualified_name_lookup (baselink,
							 qualifying_scope,
							 object_type);
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
  location_t loc = UNKNOWN_LOCATION;

  gcc_assert (TREE_CODE (qualified_id) == SCOPE_REF);

  /* Figure out what name to look up.  */
  name = TREE_OPERAND (qualified_id, 1);
  if (TREE_CODE (name) == TEMPLATE_ID_EXPR)
    {
      is_template = true;
      loc = EXPR_LOCATION (name);
      template_args = TREE_OPERAND (name, 1);
      if (template_args)
	template_args = tsubst_template_args (template_args, args,
					      complain, in_decl);
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
      scope = tsubst (scope, args, complain, in_decl);
      expr = tsubst_copy (name, args, complain, in_decl);
    }
  else
    expr = name;

  if (dependent_scope_p (scope))
    {
      if (is_template)
	expr = build_min_nt_loc (loc, TEMPLATE_ID_EXPR, expr, template_args);
      return build_qualified_name (NULL_TREE, scope, expr,
				   QUALIFIED_NAME_IS_TEMPLATE (qualified_id));
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
					  /*is_type_p=*/0, false);
	}
      else
	expr = lookup_qualified_name (scope, expr, /*is_type_p=*/0, false);
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
      check_accessibility_of_qualified_id (expr, /*object_type=*/NULL_TREE,
					   scope);
      /* Remember that there was a reference to this entity.  */
      mark_used (expr);
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
    expr = lookup_template_function (expr, template_args);

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

  return expr;
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

  code = TREE_CODE (t);

  switch (code)
    {
    case PARM_DECL:
      r = retrieve_local_specialization (t);

      if (r == NULL_TREE)
	{
	  /* We get here for a use of 'this' in an NSDMI.  */
	  if (DECL_NAME (t) == this_identifier
	      && at_function_scope_p ()
	      && DECL_CONSTRUCTOR_P (current_function_decl))
	    return current_class_ptr;

	  /* This can happen for a parameter name used later in a function
	     declaration (such as in a late-specified return type).  Just
	     make a dummy decl, since it's only used for its type.  */
	  gcc_assert (cp_unevaluated_operand != 0);
	  r = tsubst_decl (t, args, complain);
	  /* Give it the template pattern as its context; its true context
	     hasn't been instantiated yet and this is good enough for
	     mangling.  */
	  DECL_CONTEXT (r) = DECL_CONTEXT (t);
	}
      
      if (TREE_CODE (r) == ARGUMENT_PACK_SELECT)
	r = ARGUMENT_PACK_SELECT_ARG (r);
      mark_used (r);
      return r;

    case CONST_DECL:
      {
	tree enum_type;
	tree v;

	if (DECL_TEMPLATE_PARM_P (t))
	  return tsubst_copy (DECL_INITIAL (t), args, complain, in_decl);
	/* There is no need to substitute into namespace-scope
	   enumerators.  */
	if (DECL_NAMESPACE_SCOPE_P (t))
	  return t;
	/* If ARGS is NULL, then T is known to be non-dependent.  */
	if (args == NULL_TREE)
	  return integral_constant_value (t);

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
      if (PACK_EXPANSION_P (TREE_TYPE (t)))
	{
	  /* Check for a local specialization set up by
	     tsubst_pack_expansion.  */
	  if (tree r = retrieve_local_specialization (t))
	    {
	      if (TREE_CODE (r) == ARGUMENT_PACK_SELECT)
		r = ARGUMENT_PACK_SELECT_ARG (r);
	      return r;
	    }

	  /* When retrieving a capture pack from a generic lambda, remove the
	     lambda call op's own template argument list from ARGS.  Only the
	     template arguments active for the closure type should be used to
	     retrieve the pack specialization.  */
	  if (LAMBDA_FUNCTION_P (current_function_decl)
	      && (template_class_depth (DECL_CONTEXT (t))
		  != TMPL_ARGS_DEPTH (args)))
	    args = strip_innermost_template_args (args, 1);

	  /* Otherwise return the full NONTYPE_ARGUMENT_PACK that
	     tsubst_decl put in the hash table.  */
	  return retrieve_specialization (t, args, 0);
	}

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
      else if (local_variable_p (t))
	{
	  r = retrieve_local_specialization (t);
	  if (r == NULL_TREE)
	    {
	      if (DECL_ANON_UNION_VAR_P (t))
		{
		  /* Just use name lookup to find a member alias for an
		     anonymous union, but then add it to the hash table.  */
		  r = lookup_name (DECL_NAME (t));
		  gcc_assert (DECL_ANON_UNION_VAR_P (r));
		  register_local_specialization (r, t);
		}
	      else
		{
		  /* This can happen for a variable used in a late-specified
		     return type of a local lambda.  Just make a dummy decl
		     since it's only used for its type.  */
		  if (cp_unevaluated_operand)
		    return tsubst_decl (t, args, complain);
		  gcc_assert (errorcount || sorrycount);
		  return error_mark_node;
		}
	    }
	}
      else
	r = t;
      mark_used (r);
      return r;

    case NAMESPACE_DECL:
      return t;

    case OVERLOAD:
      /* An OVERLOAD will always be a non-dependent overload set; an
	 overload set from function scope will just be represented with an
	 IDENTIFIER_NODE, and from class scope with a BASELINK.  */
      gcc_assert (!uses_template_parms (t));
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

	  tree context = tsubst (DECL_CONTEXT (t), args, complain, in_decl);
	  return lookup_field (context, DECL_NAME(t), 0, false);
	}
      else
	/* Ordinary template template argument.  */
	return t;

    case CAST_EXPR:
    case REINTERPRET_CAST_EXPR:
    case CONST_CAST_EXPR:
    case STATIC_CAST_EXPR:
    case DYNAMIC_CAST_EXPR:
    case IMPLICIT_CONV_EXPR:
    case CONVERT_EXPR:
    case NOP_EXPR:
      return build1
	(code, tsubst (TREE_TYPE (t), args, complain, in_decl),
	 tsubst_copy (TREE_OPERAND (t, 0), args, complain, in_decl));

    case SIZEOF_EXPR:
      if (PACK_EXPANSION_P (TREE_OPERAND (t, 0)))
        {

          tree expanded, op = TREE_OPERAND (t, 0);
	  int len = 0;

	  if (SIZEOF_EXPR_TYPE_P (t))
	    op = TREE_TYPE (op);

	  ++cp_unevaluated_operand;
	  ++c_inhibit_evaluation_warnings;
	  /* We only want to compute the number of arguments.  */
	  expanded = tsubst_pack_expansion (op, args, complain, in_decl);
	  --cp_unevaluated_operand;
	  --c_inhibit_evaluation_warnings;

	  if (TREE_CODE (expanded) == TREE_VEC)
	    len = TREE_VEC_LENGTH (expanded);

	  if (expanded == error_mark_node)
	    return error_mark_node;
	  else if (PACK_EXPANSION_P (expanded)
		   || (TREE_CODE (expanded) == TREE_VEC
		       && len > 0
		       && PACK_EXPANSION_P (TREE_VEC_ELT (expanded, len-1))))
	    {
	      if (TREE_CODE (expanded) == TREE_VEC)
		expanded = TREE_VEC_ELT (expanded, len - 1);

	      if (TYPE_P (expanded))
		return cxx_sizeof_or_alignof_type (expanded, SIZEOF_EXPR, 
						   complain & tf_error);
	      else
		return cxx_sizeof_or_alignof_expr (expanded, SIZEOF_EXPR,
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
      return build1
	(code, tsubst (TREE_TYPE (t), args, complain, in_decl),
	 tsubst_copy (TREE_OPERAND (t, 0), args, complain, in_decl));

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
    case RROTATE_EXPR:
    case LROTATE_EXPR:
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
      return build_nt
	(code, tsubst_copy (TREE_OPERAND (t, 0), args, complain, in_decl),
	 tsubst_copy (TREE_OPERAND (t, 1), args, complain, in_decl));

    case SCOPE_REF:
      return build_qualified_name (/*type=*/NULL_TREE,
				   tsubst_copy (TREE_OPERAND (t, 0),
						args, complain, in_decl),
				   tsubst_copy (TREE_OPERAND (t, 1),
						args, complain, in_decl),
				   QUALIFIED_NAME_IS_TEMPLATE (t));

    case ARRAY_REF:
      return build_nt
	(ARRAY_REF,
	 tsubst_copy (TREE_OPERAND (t, 0), args, complain, in_decl),
	 tsubst_copy (TREE_OPERAND (t, 1), args, complain, in_decl),
	 NULL_TREE, NULL_TREE);

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
	r = build_nt
	  (code, tsubst_copy (TREE_OPERAND (t, 0), args, complain, in_decl),
	   tsubst_copy (TREE_OPERAND (t, 1), args, complain, in_decl),
	   tsubst_copy (TREE_OPERAND (t, 2), args, complain, in_decl));
	TREE_NO_WARNING (r) = TREE_NO_WARNING (t);
	return r;
      }

    case NEW_EXPR:
      {
	r = build_nt
	(code, tsubst_copy (TREE_OPERAND (t, 0), args, complain, in_decl),
	 tsubst_copy (TREE_OPERAND (t, 1), args, complain, in_decl),
	 tsubst_copy (TREE_OPERAND (t, 2), args, complain, in_decl));
	NEW_EXPR_USE_GLOBAL (r) = NEW_EXPR_USE_GLOBAL (t);
	return r;
      }

    case DELETE_EXPR:
      {
	r = build_nt
	(code, tsubst_copy (TREE_OPERAND (t, 0), args, complain, in_decl),
	 tsubst_copy (TREE_OPERAND (t, 1), args, complain, in_decl));
	DELETE_EXPR_USE_GLOBAL (r) = DELETE_EXPR_USE_GLOBAL (t);
	DELETE_EXPR_USE_VEC (r) = DELETE_EXPR_USE_VEC (t);
	return r;
      }

    case TEMPLATE_ID_EXPR:
      {
	/* Substituted template arguments */
	tree fn = TREE_OPERAND (t, 0);
	tree targs = TREE_OPERAND (t, 1);

	fn = tsubst_copy (fn, args, complain, in_decl);
	if (targs)
	  targs = tsubst_template_args (targs, args, complain, in_decl);

	return lookup_template_function (fn, targs);
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

    case RECORD_TYPE:
    case UNION_TYPE:
    case ENUMERAL_TYPE:
    case INTEGER_TYPE:
    case TEMPLATE_TYPE_PARM:
    case TEMPLATE_TEMPLATE_PARM:
    case BOUND_TEMPLATE_TEMPLATE_PARM:
    case TEMPLATE_PARM_INDEX:
    case POINTER_TYPE:
    case REFERENCE_TYPE:
    case OFFSET_TYPE:
    case FUNCTION_TYPE:
    case METHOD_TYPE:
    case ARRAY_TYPE:
    case TYPENAME_TYPE:
    case UNBOUND_CLASS_TEMPLATE:
    case TYPEOF_TYPE:
    case DECLTYPE_TYPE:
    case TYPE_DECL:
      return tsubst (t, args, complain, in_decl);

    case USING_DECL:
      t = DECL_NAME (t);
      /* Fall through.  */
    case IDENTIFIER_NODE:
      if (IDENTIFIER_TYPENAME_P (t))
	{
	  tree new_type = tsubst (TREE_TYPE (t), args, complain, in_decl);
	  return mangle_conv_op_name_for_type (new_type);
	}
      else
	return t;

    case CONSTRUCTOR:
      /* This is handled by tsubst_copy_and_build.  */
      gcc_unreachable ();

    case VA_ARG_EXPR:
      return build_x_va_arg (EXPR_LOCATION (t),
			     tsubst_copy (TREE_OPERAND (t, 0), args, complain,
					  in_decl),
			     tsubst (TREE_TYPE (t), args, complain, in_decl));

    case CLEANUP_POINT_EXPR:
      /* We shouldn't have built any of these during initial template
	 generation.  Instead, they should be built during instantiation
	 in response to the saved STMT_IS_FULL_EXPR_P setting.  */
      gcc_unreachable ();

    case OFFSET_REF:
      r = build2
	(code, tsubst (TREE_TYPE (t), args, complain, in_decl),
	 tsubst_copy (TREE_OPERAND (t, 0), args, complain, in_decl),
	 tsubst_copy (TREE_OPERAND (t, 1), args, complain, in_decl));
      PTRMEM_OK_P (r) = PTRMEM_OK_P (t);
      mark_used (TREE_OPERAND (r, 1));
      return r;

    case EXPR_PACK_EXPANSION:
      error ("invalid use of pack expansion expression");
      return error_mark_node;

    case NONTYPE_ARGUMENT_PACK:
      error ("use %<...%> to expand argument pack");
      return error_mark_node;

    case INTEGER_CST:
    case REAL_CST:
    case STRING_CST:
    case COMPLEX_CST:
      {
	/* Instantiate any typedefs in the type.  */
	tree type = tsubst (TREE_TYPE (t), args, complain, in_decl);
	r = fold_convert (type, t);
	gcc_assert (TREE_CODE (r) == code);
	return r;
      }

    case PTRMEM_CST:
      /* These can sometimes show up in a partial instantiation, but never
	 involve template parms.  */
      gcc_assert (!uses_template_parms (t));
      return t;

    default:
      /* We shouldn't get here, but keep going if !ENABLE_CHECKING.  */
      gcc_checking_assert (false);
      return t;
    }
}

/* Like tsubst_copy, but specifically for OpenMP clauses.  */

static tree
tsubst_omp_clauses (tree clauses, bool declare_simd,
		    tree args, tsubst_flags_t complain, tree in_decl)
{
  tree new_clauses = NULL, nc, oc;

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
	      tsubst_expr (OMP_CLAUSE_LASTPRIVATE_STMT (oc), args, complain,
			   in_decl, /*integral_constant_expression_p=*/false);
	      OMP_CLAUSE_LASTPRIVATE_STMT (nc)
		= pop_stmt_list (OMP_CLAUSE_LASTPRIVATE_STMT (nc));
	    }
	  /* FALLTHRU */
	case OMP_CLAUSE_PRIVATE:
	case OMP_CLAUSE_SHARED:
	case OMP_CLAUSE_FIRSTPRIVATE:
	case OMP_CLAUSE_COPYIN:
	case OMP_CLAUSE_COPYPRIVATE:
	case OMP_CLAUSE_IF:
	case OMP_CLAUSE_NUM_THREADS:
	case OMP_CLAUSE_SCHEDULE:
	case OMP_CLAUSE_COLLAPSE:
	case OMP_CLAUSE_FINAL:
	case OMP_CLAUSE_DEPEND:
	case OMP_CLAUSE_FROM:
	case OMP_CLAUSE_TO:
	case OMP_CLAUSE_UNIFORM:
	case OMP_CLAUSE_MAP:
	case OMP_CLAUSE_DEVICE:
	case OMP_CLAUSE_DIST_SCHEDULE:
	case OMP_CLAUSE_NUM_TEAMS:
	case OMP_CLAUSE_THREAD_LIMIT:
	case OMP_CLAUSE_SAFELEN:
	case OMP_CLAUSE_SIMDLEN:
	  OMP_CLAUSE_OPERAND (nc, 0)
	    = tsubst_expr (OMP_CLAUSE_OPERAND (oc, 0), args, complain, 
			   in_decl, /*integral_constant_expression_p=*/false);
	  break;
	case OMP_CLAUSE_REDUCTION:
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
	  OMP_CLAUSE_OPERAND (nc, 0)
	    = tsubst_expr (OMP_CLAUSE_OPERAND (oc, 0), args, complain,
			   in_decl, /*integral_constant_expression_p=*/false);
	  break;
	case OMP_CLAUSE_LINEAR:
	case OMP_CLAUSE_ALIGNED:
	  OMP_CLAUSE_OPERAND (nc, 0)
	    = tsubst_expr (OMP_CLAUSE_OPERAND (oc, 0), args, complain,
			   in_decl, /*integral_constant_expression_p=*/false);
	  OMP_CLAUSE_OPERAND (nc, 1)
	    = tsubst_expr (OMP_CLAUSE_OPERAND (oc, 1), args, complain,
			   in_decl, /*integral_constant_expression_p=*/false);
	  break;

	case OMP_CLAUSE_NOWAIT:
	case OMP_CLAUSE_ORDERED:
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
	  break;
	default:
	  gcc_unreachable ();
	}
    }

  new_clauses = nreverse (new_clauses);
  if (!declare_simd)
    new_clauses = finish_omp_clauses (new_clauses);
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
    return tsubst_copy_and_build (t, args, complain, in_decl,
				  /*function_p=*/false,
				  /*integral_constant_expression_p=*/false);

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

/* Substitute one OMP_FOR iterator.  */

static void
tsubst_omp_for_iterator (tree t, int i, tree declv, tree initv,
			 tree condv, tree incrv, tree *clauses,
			 tree args, tsubst_flags_t complain, tree in_decl,
			 bool integral_constant_expression_p)
{
#define RECUR(NODE)				\
  tsubst_expr ((NODE), args, complain, in_decl,	\
	       integral_constant_expression_p)
  tree decl, init, cond, incr;

  init = TREE_VEC_ELT (OMP_FOR_INIT (t), i);
  gcc_assert (TREE_CODE (init) == MODIFY_EXPR);
  decl = TREE_OPERAND (init, 0);
  init = TREE_OPERAND (init, 1);
  tree decl_expr = NULL_TREE;
  if (init && TREE_CODE (init) == DECL_EXPR)
    {
      /* We need to jump through some hoops to handle declarations in the
	 for-init-statement, since we might need to handle auto deduction,
	 but we need to keep control of initialization.  */
      decl_expr = init;
      init = DECL_INITIAL (DECL_EXPR_DECL (init));
      decl = tsubst_decl (decl, args, complain);
    }
  else
    decl = RECUR (decl);
  init = RECUR (init);

  tree auto_node = type_uses_auto (TREE_TYPE (decl));
  if (auto_node && init)
    TREE_TYPE (decl)
      = do_auto_deduction (TREE_TYPE (decl), init, auto_node);

  gcc_assert (!type_dependent_expression_p (decl));

  if (!CLASS_TYPE_P (TREE_TYPE (decl)))
    {
      if (decl_expr)
	{
	  /* Declare the variable, but don't let that initialize it.  */
	  tree init_sav = DECL_INITIAL (DECL_EXPR_DECL (decl_expr));
	  DECL_INITIAL (DECL_EXPR_DECL (decl_expr)) = NULL_TREE;
	  RECUR (decl_expr);
	  DECL_INITIAL (DECL_EXPR_DECL (decl_expr)) = init_sav;
	}

      cond = RECUR (TREE_VEC_ELT (OMP_FOR_COND (t), i));
      incr = TREE_VEC_ELT (OMP_FOR_INCR (t), i);
      if (TREE_CODE (incr) == MODIFY_EXPR)
	incr = build_x_modify_expr (EXPR_LOCATION (incr),
				    RECUR (TREE_OPERAND (incr, 0)), NOP_EXPR,
				    RECUR (TREE_OPERAND (incr, 1)),
				    complain);
      else
	incr = RECUR (incr);
      TREE_VEC_ELT (declv, i) = decl;
      TREE_VEC_ELT (initv, i) = init;
      TREE_VEC_ELT (condv, i) = cond;
      TREE_VEC_ELT (incrv, i) = incr;
      return;
    }

  if (decl_expr)
    {
      /* Declare and initialize the variable.  */
      RECUR (decl_expr);
      init = NULL_TREE;
    }
  else if (init)
    {
      tree c;
      for (c = *clauses; c ; c = OMP_CLAUSE_CHAIN (c))
	{
	  if ((OMP_CLAUSE_CODE (c) == OMP_CLAUSE_PRIVATE
	       || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LASTPRIVATE)
	      && OMP_CLAUSE_DECL (c) == decl)
	    break;
	  else if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_FIRSTPRIVATE
		   && OMP_CLAUSE_DECL (c) == decl)
	    error ("iteration variable %qD should not be firstprivate", decl);
	  else if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_REDUCTION
		   && OMP_CLAUSE_DECL (c) == decl)
	    error ("iteration variable %qD should not be reduction", decl);
	}
      if (c == NULL)
	{
	  c = build_omp_clause (input_location, OMP_CLAUSE_PRIVATE);
	  OMP_CLAUSE_DECL (c) = decl;
	  c = finish_omp_clauses (c);
	  if (c)
	    {
	      OMP_CLAUSE_CHAIN (c) = *clauses;
	      *clauses = c;
	    }
	}
    }
  cond = TREE_VEC_ELT (OMP_FOR_COND (t), i);
  if (COMPARISON_CLASS_P (cond))
    cond = build2 (TREE_CODE (cond), boolean_type_node,
		   RECUR (TREE_OPERAND (cond, 0)),
		   RECUR (TREE_OPERAND (cond, 1)));
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
	  incr = build2 (MODIFY_EXPR, TREE_TYPE (decl),
			 RECUR (TREE_OPERAND (incr, 0)),
			 build2 (TREE_CODE (rhs), TREE_TYPE (decl),
				 RECUR (TREE_OPERAND (rhs, 0)),
				 RECUR (TREE_OPERAND (rhs, 1))));
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
	  incr = build2 (MODIFY_EXPR, TREE_TYPE (decl),
			 RECUR (TREE_OPERAND (incr, 0)),
			 build2 (TREE_CODE (rhs), TREE_TYPE (decl),
				 RECUR (TREE_OPERAND (rhs, 0)),
				 RECUR (TREE_OPERAND (rhs, 1))));
	}
      else
	incr = RECUR (incr);
      break;
    default:
      incr = RECUR (incr);
      break;
    }

  TREE_VEC_ELT (declv, i) = decl;
  TREE_VEC_ELT (initv, i) = init;
  TREE_VEC_ELT (condv, i) = cond;
  TREE_VEC_ELT (incrv, i) = incr;
#undef RECUR
}

/* Like tsubst_copy for expressions, etc. but also does semantic
   processing.  */

static tree
tsubst_expr (tree t, tree args, tsubst_flags_t complain, tree in_decl,
	     bool integral_constant_expression_p)
{
#define RETURN(EXP) do { r = (EXP); goto out; } while(0)
#define RECUR(NODE)				\
  tsubst_expr ((NODE), args, complain, in_decl,	\
	       integral_constant_expression_p)

  tree stmt, tmp;
  tree r;
  location_t loc;

  if (t == NULL_TREE || t == error_mark_node)
    return t;

  loc = input_location;
  if (EXPR_HAS_LOCATION (t))
    input_location = EXPR_LOCATION (t);
  if (STATEMENT_CODE_P (TREE_CODE (t)))
    current_stmt_tree ()->stmts_are_full_exprs_p = STMT_IS_FULL_EXPR_P (t);

  switch (TREE_CODE (t))
    {
    case STATEMENT_LIST:
      {
	tree_stmt_iterator i;
	for (i = tsi_start (t); !tsi_end_p (i); tsi_next (&i))
	  RECUR (tsi_stmt (i));
	break;
      }

    case CTOR_INITIALIZER:
      finish_mem_initializers (tsubst_initializer_list
			       (TREE_OPERAND (t, 0), args));
      break;

    case RETURN_EXPR:
      finish_return_stmt (RECUR (TREE_OPERAND (t, 0)));
      break;

    case EXPR_STMT:
      tmp = RECUR (EXPR_STMT_EXPR (t));
      if (EXPR_STMT_STMT_EXPR_RESULT (t))
	finish_stmt_expr_expr (tmp, cur_stmt_expr);
      else
	finish_expr_stmt (tmp);
      break;

    case USING_STMT:
      do_using_directive (USING_STMT_NAMESPACE (t));
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
	    tree name = DECL_NAME (decl);
	    tree decl;

	    scope = tsubst (scope, args, complain, in_decl);
	    decl = lookup_qualified_name (scope, name,
					  /*is_type_p=*/false,
					  /*complain=*/false);
	    if (decl == error_mark_node || TREE_CODE (decl) == TREE_LIST)
	      qualified_name_lookup_error (scope, name, decl, input_location);
	    else
	      do_local_using_decl (decl, scope, name);
	  }
	else if (DECL_PACK_P (decl))
	  {
	    /* Don't build up decls for a variadic capture proxy, we'll
	       instantiate the elements directly as needed.  */
	    break;
	  }
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
		if (VAR_P (decl)
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
		else if (TREE_CODE (decl) == FUNCTION_DECL
			 && DECL_OMP_DECLARE_REDUCTION_P (decl)
			 && DECL_FUNCTION_SCOPE_P (pattern_decl))
		  {
		    DECL_CONTEXT (decl) = NULL_TREE;
		    pushdecl (decl);
		    DECL_CONTEXT (decl) = current_function_decl;
		    cp_check_omp_declare_reduction (decl);
		  }
		else
		  {
		    int const_init = false;
		    maybe_push_decl (decl);
		    if (VAR_P (decl)
			&& DECL_PRETTY_FUNCTION_P (decl))
		      {
			/* For __PRETTY_FUNCTION__ we have to adjust the
			   initializer.  */
			const char *const name
			  = cxx_printable_name (current_function_decl, 2);
			init = cp_fname_init (name, &TREE_TYPE (decl));
		      }
		    else
		      {
			tree t = RECUR (init);

			if (init && !t)
			  {
			    /* If we had an initializer but it
			       instantiated to nothing,
			       value-initialize the object.  This will
			       only occur when the initializer was a
			       pack expansion where the parameter packs
			       used in that expansion were of length
			       zero.  */
			    init = build_value_init (TREE_TYPE (decl),
						     complain);
			    if (TREE_CODE (init) == AGGR_INIT_EXPR)
			      init = get_target_expr_sfinae (init, complain);
			  }
			else
			  init = t;
		      }

		    if (VAR_P (decl))
		      const_init = (DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P
				    (pattern_decl));
		    cp_finish_decl (decl, init, const_init, NULL_TREE, 0);
		  }
	      }
	  }

	break;
      }

    case FOR_STMT:
      stmt = begin_for_stmt (NULL_TREE, NULL_TREE);
      RECUR (FOR_INIT_STMT (t));
      finish_for_init_stmt (stmt);
      tmp = RECUR (FOR_COND (t));
      finish_for_cond (tmp, stmt, false);
      tmp = RECUR (FOR_EXPR (t));
      finish_for_expr (tmp, stmt);
      RECUR (FOR_BODY (t));
      finish_for_stmt (stmt);
      break;

    case RANGE_FOR_STMT:
      {
        tree decl, expr;
        stmt = begin_for_stmt (NULL_TREE, NULL_TREE);
        decl = RANGE_FOR_DECL (t);
        decl = tsubst (decl, args, complain, in_decl);
        maybe_push_decl (decl);
        expr = RECUR (RANGE_FOR_EXPR (t));
        stmt = cp_convert_range_for (stmt, decl, expr, RANGE_FOR_IVDEP (t));
        RECUR (RANGE_FOR_BODY (t));
        finish_for_stmt (stmt);
      }
      break;

    case WHILE_STMT:
      stmt = begin_while_stmt ();
      tmp = RECUR (WHILE_COND (t));
      finish_while_stmt_cond (tmp, stmt, false);
      RECUR (WHILE_BODY (t));
      finish_while_stmt (stmt);
      break;

    case DO_STMT:
      stmt = begin_do_stmt ();
      RECUR (DO_BODY (t));
      finish_do_body (stmt);
      tmp = RECUR (DO_COND (t));
      finish_do_stmt (tmp, stmt, false);
      break;

    case IF_STMT:
      stmt = begin_if_stmt ();
      tmp = RECUR (IF_COND (t));
      finish_if_stmt_cond (tmp, stmt);
      RECUR (THEN_CLAUSE (t));
      finish_then_clause (stmt);

      if (ELSE_CLAUSE (t))
	{
	  begin_else_clause (stmt);
	  RECUR (ELSE_CLAUSE (t));
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
      finish_case_label (EXPR_LOCATION (t),
			 RECUR (CASE_LOW (t)),
			 RECUR (CASE_HIGH (t)));
      break;

    case LABEL_EXPR:
      {
	tree decl = LABEL_EXPR_LABEL (t);
	tree label;

	label = finish_label_stmt (DECL_NAME (decl));
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
      tmp = finish_asm_stmt
	(ASM_VOLATILE_P (t),
	 RECUR (ASM_STRING (t)),
	 tsubst_copy_asm_operands (ASM_OUTPUTS (t), args, complain, in_decl),
	 tsubst_copy_asm_operands (ASM_INPUTS (t), args, complain, in_decl),
	 tsubst_copy_asm_operands (ASM_CLOBBERS (t), args, complain, in_decl),
	 tsubst_copy_asm_operands (ASM_LABELS (t), args, complain, in_decl));
      {
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
	  tree fn;
	  /* Closures are handled by the LAMBDA_EXPR.  */
	  gcc_assert (!LAMBDA_TYPE_P (TREE_TYPE (t)));
	  complete_type (tmp);
	  for (fn = TYPE_METHODS (tmp); fn; fn = DECL_CHAIN (fn))
	    if (!DECL_ARTIFICIAL (fn))
	      instantiate_decl (fn, /*defer_ok*/0, /*expl_inst_class*/false);
	}
      break;

    case STATIC_ASSERT:
      {
	tree condition;

	++c_inhibit_evaluation_warnings;
        condition = 
          tsubst_expr (STATIC_ASSERT_CONDITION (t), 
                       args,
                       complain, in_decl,
                       /*integral_constant_expression_p=*/true);
	--c_inhibit_evaluation_warnings;

        finish_static_assert (condition,
                              STATIC_ASSERT_MESSAGE (t),
                              STATIC_ASSERT_SOURCE_LOCATION (t),
                              /*member_p=*/false);
      }
      break;

    case OMP_PARALLEL:
      tmp = tsubst_omp_clauses (OMP_PARALLEL_CLAUSES (t), false,
				args, complain, in_decl);
      stmt = begin_omp_parallel ();
      RECUR (OMP_PARALLEL_BODY (t));
      OMP_PARALLEL_COMBINED (finish_omp_parallel (tmp, stmt))
	= OMP_PARALLEL_COMBINED (t);
      break;

    case OMP_TASK:
      tmp = tsubst_omp_clauses (OMP_TASK_CLAUSES (t), false,
				args, complain, in_decl);
      stmt = begin_omp_task ();
      RECUR (OMP_TASK_BODY (t));
      finish_omp_task (tmp, stmt);
      break;

    case OMP_FOR:
    case OMP_SIMD:
    case CILK_SIMD:
    case OMP_DISTRIBUTE:
      {
	tree clauses, body, pre_body;
	tree declv = NULL_TREE, initv = NULL_TREE, condv = NULL_TREE;
	tree incrv = NULL_TREE;
	int i;

	clauses = tsubst_omp_clauses (OMP_FOR_CLAUSES (t), false,
				      args, complain, in_decl);
	if (OMP_FOR_INIT (t) != NULL_TREE)
	  {
	    declv = make_tree_vec (TREE_VEC_LENGTH (OMP_FOR_INIT (t)));
	    initv = make_tree_vec (TREE_VEC_LENGTH (OMP_FOR_INIT (t)));
	    condv = make_tree_vec (TREE_VEC_LENGTH (OMP_FOR_INIT (t)));
	    incrv = make_tree_vec (TREE_VEC_LENGTH (OMP_FOR_INIT (t)));
	  }

	stmt = begin_omp_structured_block ();

	pre_body = push_stmt_list ();
	RECUR (OMP_FOR_PRE_BODY (t));
	pre_body = pop_stmt_list (pre_body);

	if (OMP_FOR_INIT (t) != NULL_TREE)
	  for (i = 0; i < TREE_VEC_LENGTH (OMP_FOR_INIT (t)); i++)
	    tsubst_omp_for_iterator (t, i, declv, initv, condv, incrv,
				     &clauses, args, complain, in_decl,
				     integral_constant_expression_p);

	body = push_stmt_list ();
	RECUR (OMP_FOR_BODY (t));
	body = pop_stmt_list (body);

	if (OMP_FOR_INIT (t) != NULL_TREE)
	  t = finish_omp_for (EXPR_LOCATION (t), TREE_CODE (t), declv, initv,
			      condv, incrv, body, pre_body, clauses);
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

	add_stmt (finish_omp_structured_block (stmt));
      }
      break;

    case OMP_SECTIONS:
    case OMP_SINGLE:
    case OMP_TEAMS:
    case OMP_TARGET_DATA:
    case OMP_TARGET:
      tmp = tsubst_omp_clauses (OMP_CLAUSES (t), false,
				args, complain, in_decl);
      stmt = push_stmt_list ();
      RECUR (OMP_BODY (t));
      stmt = pop_stmt_list (stmt);

      t = copy_node (t);
      OMP_BODY (t) = stmt;
      OMP_CLAUSES (t) = tmp;
      add_stmt (t);
      break;

    case OMP_TARGET_UPDATE:
      tmp = tsubst_omp_clauses (OMP_TARGET_UPDATE_CLAUSES (t), false,
				args, complain, in_decl);
      t = copy_node (t);
      OMP_CLAUSES (t) = tmp;
      add_stmt (t);
      break;

    case OMP_SECTION:
    case OMP_CRITICAL:
    case OMP_MASTER:
    case OMP_TASKGROUP:
    case OMP_ORDERED:
      stmt = push_stmt_list ();
      RECUR (OMP_BODY (t));
      stmt = pop_stmt_list (stmt);

      t = copy_node (t);
      OMP_BODY (t) = stmt;
      add_stmt (t);
      break;

    case OMP_ATOMIC:
      gcc_assert (OMP_ATOMIC_DEPENDENT_P (t));
      if (TREE_CODE (TREE_OPERAND (t, 1)) != MODIFY_EXPR)
	{
	  tree op1 = TREE_OPERAND (t, 1);
	  tree rhs1 = NULL_TREE;
	  tree lhs, rhs;
	  if (TREE_CODE (op1) == COMPOUND_EXPR)
	    {
	      rhs1 = RECUR (TREE_OPERAND (op1, 0));
	      op1 = TREE_OPERAND (op1, 1);
	    }
	  lhs = RECUR (TREE_OPERAND (op1, 0));
	  rhs = RECUR (TREE_OPERAND (op1, 1));
	  finish_omp_atomic (OMP_ATOMIC, TREE_CODE (op1), lhs, rhs,
			     NULL_TREE, NULL_TREE, rhs1,
			     OMP_ATOMIC_SEQ_CST (t));
	}
      else
	{
	  tree op1 = TREE_OPERAND (t, 1);
	  tree v = NULL_TREE, lhs, rhs = NULL_TREE, lhs1 = NULL_TREE;
	  tree rhs1 = NULL_TREE;
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
	      lhs = RECUR (TREE_OPERAND (op11, 0));
	      rhs = RECUR (TREE_OPERAND (op11, 1));
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
	  finish_omp_atomic (code, opcode, lhs, rhs, v, lhs1, rhs1,
			     OMP_ATOMIC_SEQ_CST (t));
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
      RETURN (build_must_not_throw_expr (RECUR (TREE_OPERAND (t, 0)),
					RECUR (MUST_NOT_THROW_COND (t))));

    case EXPR_PACK_EXPANSION:
      error ("invalid use of pack expansion expression");
      RETURN (error_mark_node);

    case NONTYPE_ARGUMENT_PACK:
      error ("use %<...%> to expand argument pack");
      RETURN (error_mark_node);

    case CILK_SPAWN_STMT:
      cfun->calls_cilk_spawn = 1;
      RETURN (build_cilk_spawn (EXPR_LOCATION (t), RECUR (CILK_SPAWN_FN (t))));

    case CILK_SYNC_STMT:
      RETURN (build_cilk_sync ());

    case COMPOUND_EXPR:
      tmp = RECUR (TREE_OPERAND (t, 0));
      if (tmp == NULL_TREE)
	/* If the first operand was a statement, we're done with it.  */
	RETURN (RECUR (TREE_OPERAND (t, 1)));
      RETURN (build_x_compound_expr (EXPR_LOCATION (t), tmp,
				    RECUR (TREE_OPERAND (t, 1)),
				    complain));

    case ANNOTATE_EXPR:
      tmp = RECUR (TREE_OPERAND (t, 0));
      RETURN (build2_loc (EXPR_LOCATION (t), ANNOTATE_EXPR,
			  TREE_TYPE (tmp), tmp, RECUR (TREE_OPERAND (t, 1))));

    default:
      gcc_assert (!STATEMENT_CODE_P (TREE_CODE (t)));

      RETURN (tsubst_copy_and_build (t, args, complain, in_decl,
				    /*function_p=*/false,
				    integral_constant_expression_p));
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

  gcc_assert (TREE_CODE (t) == STATEMENT_LIST);

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
      DECL_CONTEXT (omp_out) = current_function_decl;
      DECL_CONTEXT (omp_in) = current_function_decl;
      keep_next_level (true);
      tree block = begin_omp_structured_block ();
      tsubst_expr (stmts[2], args, complain, in_decl, false);
      block = finish_omp_structured_block (block);
      block = maybe_cleanup_point_expr_void (block);
      add_decl_expr (omp_out);
      if (TREE_NO_WARNING (DECL_EXPR_DECL (stmts[0])))
	TREE_NO_WARNING (omp_out) = 1;
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
      tsubst_expr (stmts[5], args, complain, in_decl, false);
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
    t = tsubst_copy_and_build (t, args, complain, in_decl,
			       /*function_p=*/false,
			       /*integral_constant_expression_p=*/false);

  return t;
}

/* Sentinel to disable certain warnings during template substitution.  */

struct warning_sentinel {
  int &flag;
  int val;
  warning_sentinel(int& flag, bool suppress=true)
    : flag(flag), val(flag) { if (suppress) flag = 0; }
  ~warning_sentinel() { flag = val; }
};

/* Like tsubst but deals with expressions and performs semantic
   analysis.  FUNCTION_P is true if T is the "F" in "F (ARGS)".  */

tree
tsubst_copy_and_build (tree t,
		       tree args,
		       tsubst_flags_t complain,
		       tree in_decl,
		       bool function_p,
		       bool integral_constant_expression_p)
{
#define RETURN(EXP) do { retval = (EXP); goto out; } while(0)
#define RECUR(NODE)						\
  tsubst_copy_and_build (NODE, args, complain, in_decl, 	\
			 /*function_p=*/false,			\
			 integral_constant_expression_p)

  tree retval, op1;
  location_t loc;

  if (t == NULL_TREE || t == error_mark_node)
    return t;

  loc = input_location;
  if (EXPR_HAS_LOCATION (t))
    input_location = EXPR_LOCATION (t);

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
	bool non_integral_constant_expression_p;
	const char *error_msg;

	if (IDENTIFIER_TYPENAME_P (t))
	  {
	    tree new_type = tsubst (TREE_TYPE (t), args, complain, in_decl);
	    t = mangle_conv_op_name_for_type (new_type);
	  }

	/* Look up the name.  */
	decl = lookup_name (t);

	/* By convention, expressions use ERROR_MARK_NODE to indicate
	   failure, not NULL_TREE.  */
	if (decl == NULL_TREE)
	  decl = error_mark_node;

	decl = finish_id_expression (t, decl, NULL_TREE,
				     &idk,
				     integral_constant_expression_p,
          /*allow_non_integral_constant_expression_p=*/(cxx_dialect >= cxx11),
				     &non_integral_constant_expression_p,
				     /*template_p=*/false,
				     /*done=*/true,
				     /*address_p=*/false,
				     /*template_arg_p=*/false,
				     &error_msg,
				     input_location);
	if (error_msg)
	  error (error_msg);
	if (!function_p && identifier_p (decl))
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
	tree templ = RECUR (TREE_OPERAND (t, 0));
	tree targs = TREE_OPERAND (t, 1);

	if (targs)
	  targs = tsubst_template_args (targs, args, complain, in_decl);

	if (TREE_CODE (templ) == COMPONENT_REF)
	  {
	    object = TREE_OPERAND (templ, 0);
	    templ = TREE_OPERAND (templ, 1);
	  }
	else
	  object = NULL_TREE;
	templ = lookup_template_function (templ, targs);

	if (object)
	  RETURN (build3 (COMPONENT_REF, TREE_TYPE (templ),
			 object, templ, NULL_TREE));
	else
	  RETURN (baselink_for_fns (templ));
      }

    case INDIRECT_REF:
      {
	tree r = RECUR (TREE_OPERAND (t, 0));

	if (REFERENCE_REF_P (t))
	  {
	    /* A type conversion to reference type will be enclosed in
	       such an indirect ref, but the substitution of the cast
	       will have also added such an indirect ref.  */
	    if (TREE_CODE (TREE_TYPE (r)) == REFERENCE_TYPE)
	      r = convert_from_reference (r);
	  }
	else
	  r = build_x_indirect_ref (input_location, r, RO_UNARY_STAR,
				    complain|decltype_flag);
	RETURN (r);
      }

    case NOP_EXPR:
      RETURN (build_nop
	(tsubst (TREE_TYPE (t), args, complain, in_decl),
	 RECUR (TREE_OPERAND (t, 0))));

    case IMPLICIT_CONV_EXPR:
      {
	tree type = tsubst (TREE_TYPE (t), args, complain, in_decl);
	tree expr = RECUR (TREE_OPERAND (t, 0));
	int flags = LOOKUP_IMPLICIT;
	if (IMPLICIT_CONV_EXPR_DIRECT_INIT (t))
	  flags = LOOKUP_NORMAL;
	RETURN (perform_implicit_conversion_flags (type, expr, complain,
						  flags));
      }

    case CONVERT_EXPR:
      RETURN (build1
	(CONVERT_EXPR,
	 tsubst (TREE_TYPE (t), args, complain, in_decl),
	 RECUR (TREE_OPERAND (t, 0))));

    case CAST_EXPR:
    case REINTERPRET_CAST_EXPR:
    case CONST_CAST_EXPR:
    case DYNAMIC_CAST_EXPR:
    case STATIC_CAST_EXPR:
      {
	tree type;
	tree op, r = NULL_TREE;

	type = tsubst (TREE_TYPE (t), args, complain, in_decl);
	if (integral_constant_expression_p
	    && !cast_valid_in_integral_constant_expression_p (type))
	  {
            if (complain & tf_error)
              error ("a cast to a type other than an integral or "
                     "enumeration type cannot appear in a constant-expression");
	    RETURN (error_mark_node);
	  }

	op = RECUR (TREE_OPERAND (t, 0));

	warning_sentinel s(warn_useless_cast);
	switch (TREE_CODE (t))
	  {
	  case CAST_EXPR:
	    r = build_functional_cast (type, op, complain);
	    break;
	  case REINTERPRET_CAST_EXPR:
	    r = build_reinterpret_cast (type, op, complain);
	    break;
	  case CONST_CAST_EXPR:
	    r = build_const_cast (type, op, complain);
	    break;
	  case DYNAMIC_CAST_EXPR:
	    r = build_dynamic_cast (type, op, complain);
	    break;
	  case STATIC_CAST_EXPR:
	    r = build_static_cast (type, op, complain);
	    break;
	  default:
	    gcc_unreachable ();
	  }

	RETURN (r);
      }

    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
      op1 = tsubst_non_call_postfix_expression (TREE_OPERAND (t, 0),
						args, complain, in_decl);
      RETURN (build_x_unary_op (input_location, TREE_CODE (t), op1,
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
				complain|decltype_flag));

    case FIX_TRUNC_EXPR:
      RETURN (cp_build_unary_op (FIX_TRUNC_EXPR, RECUR (TREE_OPERAND (t, 0)),
				0, complain));

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
    case RROTATE_EXPR:
    case LROTATE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case MAX_EXPR:
    case MIN_EXPR:
    case LE_EXPR:
    case GE_EXPR:
    case LT_EXPR:
    case GT_EXPR:
    case MEMBER_REF:
    case DOTSTAR_EXPR:
      {
	warning_sentinel s1(warn_type_limits);
	warning_sentinel s2(warn_div_by_zero);
	tree r = build_x_binary_op
	  (input_location, TREE_CODE (t),
	   RECUR (TREE_OPERAND (t, 0)),
	   (TREE_NO_WARNING (TREE_OPERAND (t, 0))
	    ? ERROR_MARK
	    : TREE_CODE (TREE_OPERAND (t, 0))),
	   RECUR (TREE_OPERAND (t, 1)),
	   (TREE_NO_WARNING (TREE_OPERAND (t, 1))
	    ? ERROR_MARK
	    : TREE_CODE (TREE_OPERAND (t, 1))),
	   /*overload=*/NULL,
	   complain|decltype_flag);
	if (EXPR_P (r) && TREE_NO_WARNING (t))
	  TREE_NO_WARNING (r) = TREE_NO_WARNING (t);

	RETURN (r);
      }

    case POINTER_PLUS_EXPR:
      return fold_build_pointer_plus (RECUR (TREE_OPERAND (t, 0)),
				      RECUR (TREE_OPERAND (t, 1)));

    case SCOPE_REF:
      RETURN (tsubst_qualified_id (t, args, complain, in_decl, /*done=*/true,
				  /*address_p=*/false));
    case ARRAY_REF:
      op1 = tsubst_non_call_postfix_expression (TREE_OPERAND (t, 0),
						args, complain, in_decl);
      RETURN (build_x_array_ref (EXPR_LOCATION (t), op1,
				 RECUR (TREE_OPERAND (t, 1)),
				 complain|decltype_flag));

    case ARRAY_NOTATION_REF:
      {
	tree start_index, length, stride;
	op1 = tsubst_non_call_postfix_expression (ARRAY_NOTATION_ARRAY (t),
						  args, complain, in_decl);
	start_index = RECUR (ARRAY_NOTATION_START (t));
	length = RECUR (ARRAY_NOTATION_LENGTH (t));
	stride = RECUR (ARRAY_NOTATION_STRIDE (t));
	RETURN (build_array_notation_ref (EXPR_LOCATION (t), op1, start_index,
					  length, stride, TREE_TYPE (op1)));
      }
    case SIZEOF_EXPR:
      if (PACK_EXPANSION_P (TREE_OPERAND (t, 0)))
	RETURN (tsubst_copy (t, args, complain, in_decl));
      /* Fall through */
      
    case ALIGNOF_EXPR:
      {
	tree r;

	op1 = TREE_OPERAND (t, 0);
	if (TREE_CODE (t) == SIZEOF_EXPR && SIZEOF_EXPR_TYPE_P (t))
	  op1 = TREE_TYPE (op1);
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
	      op1 = tsubst_copy_and_build (op1, args, complain, in_decl,
					   /*function_p=*/false,
					   /*integral_constant_expression_p=*/
					   false);
	    --cp_unevaluated_operand;
	    --c_inhibit_evaluation_warnings;
	  }
        if (TYPE_P (op1))
	  r = cxx_sizeof_or_alignof_type (op1, TREE_CODE (t),
					  complain & tf_error);
	else
	  r = cxx_sizeof_or_alignof_expr (op1, TREE_CODE (t),
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
	op1 = tsubst_copy_and_build (op1, args, complain, in_decl,
				     /*function_p=*/false,
				     /*integral_constant_expression_p=*/false);
	--cp_unevaluated_operand;
	--c_inhibit_evaluation_warnings;
	RETURN (objc_build_encode_expr (op1));
      }

    case NOEXCEPT_EXPR:
      op1 = TREE_OPERAND (t, 0);
      ++cp_unevaluated_operand;
      ++c_inhibit_evaluation_warnings;
      op1 = tsubst_copy_and_build (op1, args, complain, in_decl,
				   /*function_p=*/false,
				   /*integral_constant_expression_p=*/false);
      --cp_unevaluated_operand;
      --c_inhibit_evaluation_warnings;
      RETURN (finish_noexcept_expr (op1, complain));

    case MODOP_EXPR:
      {
	warning_sentinel s(warn_div_by_zero);
	tree r = build_x_modify_expr
	  (EXPR_LOCATION (t),
	   RECUR (TREE_OPERAND (t, 0)),
	   TREE_CODE (TREE_OPERAND (t, 1)),
	   RECUR (TREE_OPERAND (t, 2)),
	   complain|decltype_flag);
	/* TREE_NO_WARNING must be set if either the expression was
	   parenthesized or it uses an operator such as >>= rather
	   than plain assignment.  In the former case, it was already
	   set and must be copied.  In the latter case,
	   build_x_modify_expr sets it and it must not be reset
	   here.  */
	if (TREE_NO_WARNING (t))
	  TREE_NO_WARNING (r) = TREE_NO_WARNING (t);

	RETURN (r);
      }

    case ARROW_EXPR:
      op1 = tsubst_non_call_postfix_expression (TREE_OPERAND (t, 0),
						args, complain, in_decl);
      /* Remember that there was a reference to this entity.  */
      if (DECL_P (op1))
	mark_used (op1);
      RETURN (build_x_arrow (input_location, op1, complain));

    case NEW_EXPR:
      {
	tree placement = RECUR (TREE_OPERAND (t, 0));
	tree init = RECUR (TREE_OPERAND (t, 3));
	vec<tree, va_gc> *placement_vec;
	vec<tree, va_gc> *init_vec;
	tree ret;

	if (placement == NULL_TREE)
	  placement_vec = NULL;
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
	else
	  {
	    init_vec = make_tree_vector ();
	    if (init == void_zero_node)
	      gcc_assert (init_vec != NULL);
	    else
	      {
		for (; init != NULL_TREE; init = TREE_CHAIN (init))
		  vec_safe_push (init_vec, TREE_VALUE (init));
	      }
	  }

	ret = build_new (&placement_vec,
			 tsubst (TREE_OPERAND (t, 1), args, complain, in_decl),
			 RECUR (TREE_OPERAND (t, 2)),
			 &init_vec,
			 NEW_EXPR_USE_GLOBAL (t),
			 complain);

	if (placement_vec != NULL)
	  release_tree_vector (placement_vec);
	if (init_vec != NULL)
	  release_tree_vector (init_vec);

	RETURN (ret);
      }

    case DELETE_EXPR:
     RETURN (delete_sanity
       (RECUR (TREE_OPERAND (t, 0)),
	RECUR (TREE_OPERAND (t, 1)),
	DELETE_EXPR_USE_VEC (t),
	DELETE_EXPR_USE_GLOBAL (t),
	complain));

    case COMPOUND_EXPR:
      {
	tree op0 = tsubst_copy_and_build (TREE_OPERAND (t, 0), args,
					  complain & ~tf_decltype, in_decl,
					  /*function_p=*/false,
					  integral_constant_expression_p);
	RETURN (build_x_compound_expr (EXPR_LOCATION (t),
				       op0,
				       RECUR (TREE_OPERAND (t, 1)),
				       complain|decltype_flag));
      }

    case CALL_EXPR:
      {
	tree function;
	vec<tree, va_gc> *call_args;
	unsigned int nargs, i;
	bool qualified_p;
	bool koenig_p;
	tree ret;

	function = CALL_EXPR_FN (t);
	/* When we parsed the expression,  we determined whether or
	   not Koenig lookup should be performed.  */
	koenig_p = KOENIG_LOOKUP_P (t);
	if (TREE_CODE (function) == SCOPE_REF)
	  {
	    qualified_p = true;
	    function = tsubst_qualified_id (function, args, complain, in_decl,
					    /*done=*/false,
					    /*address_p=*/false);
	  }
	else if (koenig_p && identifier_p (function))
	  {
	    /* Do nothing; calling tsubst_copy_and_build on an identifier
	       would incorrectly perform unqualified lookup again.

	       Note that we can also have an IDENTIFIER_NODE if the earlier
	       unqualified lookup found a member function; in that case
	       koenig_p will be false and we do want to do the lookup
	       again to find the instantiated member function.

	       FIXME but doing that causes c++/15272, so we need to stop
	       using IDENTIFIER_NODE in that situation.  */
	    qualified_p = false;
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

	    function = tsubst_copy_and_build (function, args, complain,
					      in_decl,
					      !qualified_p,
					      integral_constant_expression_p);

	    if (BASELINK_P (function))
	      qualified_p = true;
	  }

	nargs = call_expr_nargs (t);
	call_args = make_tree_vector ();
	for (i = 0; i < nargs; ++i)
	  {
	    tree arg = CALL_EXPR_ARG (t, i);

	    if (!PACK_EXPANSION_P (arg))
	      vec_safe_push (call_args, RECUR (CALL_EXPR_ARG (t, i)));
	    else
	      {
		/* Expand the pack expansion and push each entry onto
		   CALL_ARGS.  */
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
		  {
		    /* A partial substitution.  Add one entry.  */
		    vec_safe_push (call_args, arg);
		  }
	      }
	  }

	/* We do not perform argument-dependent lookup if normal
	   lookup finds a non-function, in accordance with the
	   expected resolution of DR 218.  */
	if (koenig_p
	    && ((is_overloaded_fn (function)
		 /* If lookup found a member function, the Koenig lookup is
		    not appropriate, even if an unqualified-name was used
		    to denote the function.  */
		 && !DECL_FUNCTION_MEMBER_P (get_first_fn (function)))
		|| identifier_p (function))
	    /* Only do this when substitution turns a dependent call
	       into a non-dependent call.  */
	    && type_dependent_expression_p_push (t)
	    && !any_type_dependent_arguments_p (call_args))
	  function = perform_koenig_lookup (function, call_args, tf_none);

	if (identifier_p (function)
	    && !any_type_dependent_arguments_p (call_args))
	  {
	    if (koenig_p && (complain & tf_warning_or_error))
	      {
		/* For backwards compatibility and good diagnostics, try
		   the unqualified lookup again if we aren't in SFINAE
		   context.  */
		tree unq = (tsubst_copy_and_build
			    (function, args, complain, in_decl, true,
			     integral_constant_expression_p));
		if (unq == error_mark_node)
		  RETURN (error_mark_node);

		if (unq != function)
		  {
		    tree fn = unq;
		    if (INDIRECT_REF_P (fn))
		      fn = TREE_OPERAND (fn, 0);
		    if (TREE_CODE (fn) == COMPONENT_REF)
		      fn = TREE_OPERAND (fn, 1);
		    if (is_overloaded_fn (fn))
		      fn = get_first_fn (fn);
		    if (permerror (EXPR_LOC_OR_LOC (t, input_location),
				   "%qD was not declared in this scope, "
				   "and no declarations were found by "
				   "argument-dependent lookup at the point "
				   "of instantiation", function))
		      {
			if (!DECL_P (fn))
			  /* Can't say anything more.  */;
			else if (DECL_CLASS_SCOPE_P (fn))
			  {
			    location_t loc = EXPR_LOC_OR_LOC (t,
							      input_location);
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
			  inform (0, "%q+D declared here, later in the "
				  "translation unit", fn);
		      }
		    function = unq;
		  }
	      }
	    if (identifier_p (function))
	      {
		if (complain & tf_error)
		  unqualified_name_lookup_error (function);
		release_tree_vector (call_args);
		RETURN (error_mark_node);
	      }
	  }

	/* Remember that there was a reference to this entity.  */
	if (DECL_P (function))
	  mark_used (function);

	/* Put back tf_decltype for the actual call.  */
	complain |= decltype_flag;

	if (TREE_CODE (function) == OFFSET_REF)
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
	      ret = build_nt_call_vec (function, call_args);
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
	else
	  ret = finish_call_expr (function, &call_args,
				  /*disallow_virtual=*/qualified_p,
				  koenig_p,
				  complain);

	release_tree_vector (call_args);

	RETURN (ret);
      }

    case COND_EXPR:
      {
	tree cond = RECUR (TREE_OPERAND (t, 0));
	tree exp1, exp2;

	if (TREE_CODE (cond) == INTEGER_CST)
	  {
	    if (integer_zerop (cond))
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
	  }
	else
	  {
	    exp1 = RECUR (TREE_OPERAND (t, 1));
	    exp2 = RECUR (TREE_OPERAND (t, 2));
	  }

	RETURN (build_x_conditional_expr (EXPR_LOCATION (t),
					 cond, exp1, exp2, complain));
      }

    case PSEUDO_DTOR_EXPR:
      RETURN (finish_pseudo_destructor_expr
	      (RECUR (TREE_OPERAND (t, 0)),
	       RECUR (TREE_OPERAND (t, 1)),
	       tsubst (TREE_OPERAND (t, 2), args, complain, in_decl),
	       input_location));

    case TREE_LIST:
      {
	tree purpose, value, chain;

	if (t == void_list_node)
	  RETURN (t);

        if ((TREE_PURPOSE (t) && PACK_EXPANSION_P (TREE_PURPOSE (t)))
            || (TREE_VALUE (t) && PACK_EXPANSION_P (TREE_VALUE (t))))
          {
            /* We have pack expansions, so expand those and
               create a new list out of it.  */
            tree purposevec = NULL_TREE;
            tree valuevec = NULL_TREE;
            tree chain;
            int i, len = -1;

            /* Expand the argument expressions.  */
            if (TREE_PURPOSE (t))
              purposevec = tsubst_pack_expansion (TREE_PURPOSE (t), args,
                                                 complain, in_decl);
            if (TREE_VALUE (t))
              valuevec = tsubst_pack_expansion (TREE_VALUE (t), args,
                                               complain, in_decl);

            /* Build the rest of the list.  */
            chain = TREE_CHAIN (t);
            if (chain && chain != void_type_node)
              chain = RECUR (chain);

            /* Determine the number of arguments.  */
            if (purposevec && TREE_CODE (purposevec) == TREE_VEC)
              {
                len = TREE_VEC_LENGTH (purposevec);
                gcc_assert (!valuevec || len == TREE_VEC_LENGTH (valuevec));
              }
            else if (TREE_CODE (valuevec) == TREE_VEC)
              len = TREE_VEC_LENGTH (valuevec);
            else
              {
                /* Since we only performed a partial substitution into
                   the argument pack, we only RETURN (a single list
                   node.  */
                if (purposevec == TREE_PURPOSE (t)
                    && valuevec == TREE_VALUE (t)
                    && chain == TREE_CHAIN (t))
                  RETURN (t);

                RETURN (tree_cons (purposevec, valuevec, chain));
              }
            
            /* Convert the argument vectors into a TREE_LIST */
            i = len;
            while (i > 0)
              {
                /* Grab the Ith values.  */
                i--;
                purpose = purposevec ? TREE_VEC_ELT (purposevec, i) 
		                     : NULL_TREE;
                value 
		  = valuevec ? convert_from_reference (TREE_VEC_ELT (valuevec, i)) 
                             : NULL_TREE;

                /* Build the list (backwards).  */
                chain = tree_cons (purpose, value, chain);
              }

            RETURN (chain);
          }

	purpose = TREE_PURPOSE (t);
	if (purpose)
	  purpose = RECUR (purpose);
	value = TREE_VALUE (t);
	if (value)
	  value = RECUR (value);
	chain = TREE_CHAIN (t);
	if (chain && chain != void_type_node)
	  chain = RECUR (chain);
	if (purpose == TREE_PURPOSE (t)
	    && value == TREE_VALUE (t)
	    && chain == TREE_CHAIN (t))
	  RETURN (t);
	RETURN (tree_cons (purpose, value, chain));
      }

    case COMPONENT_REF:
      {
	tree object;
	tree object_type;
	tree member;
	tree r;

	object = tsubst_non_call_postfix_expression (TREE_OPERAND (t, 0),
						     args, complain, in_decl);
	/* Remember that there was a reference to this entity.  */
	if (DECL_P (object))
	  mark_used (object);
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

	if (type_dependent_expression_p (object))
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
	    member = lookup_qualified_name (scope, tmpl,
					    /*is_type_p=*/false,
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
	else if (TREE_CODE (member) == FIELD_DECL)
	  {
	    r = finish_non_static_data_member (member, object, NULL_TREE);
	    if (TREE_CODE (r) == COMPONENT_REF)
	      REF_PARENTHESIZED_P (r) = REF_PARENTHESIZED_P (t);
	    RETURN (r);
	  }

	r = finish_class_member_access_expr (object, member,
					     /*template_p=*/false,
					     complain);
	if (TREE_CODE (r) == COMPONENT_REF)
	  REF_PARENTHESIZED_P (r) = REF_PARENTHESIZED_P (t);
	RETURN (r);
      }

    case THROW_EXPR:
      RETURN (build_throw
	(RECUR (TREE_OPERAND (t, 0))));

    case CONSTRUCTOR:
      {
	vec<constructor_elt, va_gc> *n;
	constructor_elt *ce;
	unsigned HOST_WIDE_INT idx;
	tree type = tsubst (TREE_TYPE (t), args, complain, in_decl);
	bool process_index_p;
        int newlen;
        bool need_copy_p = false;
	tree r;

	if (type == error_mark_node)
	  RETURN (error_mark_node);

	/* digest_init will do the wrong thing if we let it.  */
	if (type && TYPE_PTRMEMFUNC_P (type))
	  RETURN (t);

	/* We do not want to process the index of aggregate
	   initializers as they are identifier nodes which will be
	   looked up by digest_init.  */
	process_index_p = !(type && MAYBE_CLASS_TYPE_P (type));

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

	if (TREE_HAS_CONSTRUCTOR (t))
	  RETURN (finish_compound_literal (type, r, complain));

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
      else if (DECL_PACK_P (t))
	{
	  /* We don't build decls for an instantiation of a
	     variadic capture proxy, we instantiate the elements
	     when needed.  */
	  gcc_assert (DECL_HAS_VALUE_EXPR_P (t));
	  return RECUR (DECL_VALUE_EXPR (t));
	}
      /* Fall through */

    case PARM_DECL:
      {
	tree r = tsubst_copy (t, args, complain, in_decl);

	if (TREE_CODE (TREE_TYPE (t)) != REFERENCE_TYPE)
	  /* If the original type was a reference, we'll be wrapped in
	     the appropriate INDIRECT_REF.  */
	  r = convert_from_reference (r);
	RETURN (r);
      }

    case VA_ARG_EXPR:
      RETURN (build_x_va_arg (EXPR_LOCATION (t),
			     RECUR (TREE_OPERAND (t, 0)),
			     tsubst (TREE_TYPE (t), args, complain, in_decl)));

    case OFFSETOF_EXPR:
      RETURN (finish_offsetof (RECUR (TREE_OPERAND (t, 0))));

    case TRAIT_EXPR:
      {
	tree type1 = tsubst (TRAIT_EXPR_TYPE1 (t), args,
			     complain, in_decl);

	tree type2 = TRAIT_EXPR_TYPE2 (t);
	if (type2)
	  type2 = tsubst (type2, args, complain, in_decl);
	
	RETURN (finish_trait_expr (TRAIT_EXPR_KIND (t), type1, type2));
      }

    case STMT_EXPR:
      {
	tree old_stmt_expr = cur_stmt_expr;
	tree stmt_expr = begin_stmt_expr ();

	cur_stmt_expr = stmt_expr;
	tsubst_expr (STMT_EXPR_STMT (t), args, complain, in_decl,
		     integral_constant_expression_p);
	stmt_expr = finish_stmt_expr (stmt_expr, false);
	cur_stmt_expr = old_stmt_expr;

	/* If the resulting list of expression statement is empty,
	   fold it further into void_zero_node.  */
	if (empty_expr_stmt_p (stmt_expr))
	  stmt_expr = void_zero_node;

	RETURN (stmt_expr);
      }

    case LAMBDA_EXPR:
      {
	tree r = build_lambda_expr ();

	tree type = tsubst (LAMBDA_EXPR_CLOSURE (t), args, complain, NULL_TREE);
	LAMBDA_EXPR_CLOSURE (r) = type;
	CLASSTYPE_LAMBDA_EXPR (type) = r;

	LAMBDA_EXPR_LOCATION (r)
	  = LAMBDA_EXPR_LOCATION (t);
	LAMBDA_EXPR_DEFAULT_CAPTURE_MODE (r)
	  = LAMBDA_EXPR_DEFAULT_CAPTURE_MODE (t);
	LAMBDA_EXPR_MUTABLE_P (r) = LAMBDA_EXPR_MUTABLE_P (t);
	LAMBDA_EXPR_DISCRIMINATOR (r)
	  = (LAMBDA_EXPR_DISCRIMINATOR (t));
	/* For a function scope, we want to use tsubst so that we don't
	   complain about referring to an auto function before its return
	   type has been deduced.  Otherwise, we want to use tsubst_copy so
	   that we look up the existing field/parameter/variable rather
	   than build a new one.  */
	tree scope = LAMBDA_EXPR_EXTRA_SCOPE (t);
	if (scope && TREE_CODE (scope) == FUNCTION_DECL)
	  scope = tsubst (scope, args, complain, in_decl);
	else if (scope && TREE_CODE (scope) == PARM_DECL)
	  {
	    /* Look up the parameter we want directly, as tsubst_copy
	       doesn't do what we need.  */
	    tree fn = tsubst (DECL_CONTEXT (scope), args, complain, in_decl);
	    tree parm = FUNCTION_FIRST_USER_PARM (fn);
	    while (DECL_PARM_INDEX (parm) != DECL_PARM_INDEX (scope))
	      parm = DECL_CHAIN (parm);
	    scope = parm;
	    /* FIXME Work around the parm not having DECL_CONTEXT set.  */
	    if (DECL_CONTEXT (scope) == NULL_TREE)
	      DECL_CONTEXT (scope) = fn;
	  }
	else
	  scope = RECUR (scope);
	LAMBDA_EXPR_EXTRA_SCOPE (r) = scope;
	LAMBDA_EXPR_RETURN_TYPE (r)
	  = tsubst (LAMBDA_EXPR_RETURN_TYPE (t), args, complain, in_decl);

	gcc_assert (LAMBDA_EXPR_THIS_CAPTURE (t) == NULL_TREE
		    && LAMBDA_EXPR_PENDING_PROXIES (t) == NULL);

	/* Do this again now that LAMBDA_EXPR_EXTRA_SCOPE is set.  */
	determine_visibility (TYPE_NAME (type));
	/* Now that we know visibility, instantiate the type so we have a
	   declaration of the op() for later calls to lambda_function.  */
	complete_type (type);

	LAMBDA_EXPR_THIS_CAPTURE (r) = NULL_TREE;

	RETURN (build_lambda_object (r));
      }

    case TARGET_EXPR:
      /* We can get here for a constant initializer of non-dependent type.
         FIXME stop folding in cp_parser_initializer_clause.  */
      {
	tree r = get_target_expr_sfinae (RECUR (TARGET_EXPR_INITIAL (t)),
					 complain);
	RETURN (r);
      }

    case TRANSACTION_EXPR:
      RETURN (tsubst_expr(t, args, complain, in_decl,
	     integral_constant_expression_p));

    case PAREN_EXPR:
      RETURN (finish_parenthesized_expr (RECUR (TREE_OPERAND (t, 0))));

    case VEC_PERM_EXPR:
      RETURN (build_x_vec_perm_expr (input_location,
		RECUR (TREE_OPERAND (t, 0)),
		RECUR (TREE_OPERAND (t, 1)),
		RECUR (TREE_OPERAND (t, 2)),
		complain));

    default:
      /* Handle Objective-C++ constructs, if appropriate.  */
      {
	tree subst
	  = objcp_tsubst_copy_and_build (t, args, complain,
					 in_decl, /*function_p=*/false);
	if (subst)
	  RETURN (subst);
      }
      RETURN (tsubst_copy (t, args, complain, in_decl));
    }

#undef RECUR
#undef RETURN
 out:
  input_location = loc;
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
	      if (TYPE_ANONYMOUS_P (nt))
		error ("%qT is/uses anonymous type", t);
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

static tree
instantiate_template_1 (tree tmpl, tree orig_args, tsubst_flags_t complain)
{
  tree targ_ptr = orig_args;
  tree fndecl;
  tree gen_tmpl;
  tree spec;
  bool access_ok = true;

  if (tmpl == error_mark_node)
    return error_mark_node;

  gcc_assert (TREE_CODE (tmpl) == TEMPLATE_DECL);

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
  if (tmpl != gen_tmpl)
    /* The TMPL is a partial instantiation.  To get a full set of
       arguments we must add the arguments used to perform the
       partial instantiation.  */
    targ_ptr = add_outermost_template_args (DECL_TI_ARGS (tmpl),
					    targ_ptr);

  /* It would be nice to avoid hashing here and then again in tsubst_decl,
     but it doesn't seem to be on the hot path.  */
  spec = retrieve_specialization (gen_tmpl, targ_ptr, 0);

  gcc_assert (tmpl == gen_tmpl
	      || ((fndecl = retrieve_specialization (tmpl, orig_args, 0))
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
      tree ctx = tsubst (DECL_CONTEXT (gen_tmpl), targ_ptr,
			 complain, gen_tmpl);
      push_nested_class (ctx);
    }
  /* Substitute template parameters to obtain the specialization.  */
  fndecl = tsubst (DECL_TEMPLATE_RESULT (gen_tmpl),
		   targ_ptr, complain, gen_tmpl);
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

  /* Now we know the specialization, compute access previously
     deferred.  */
  push_access_scope (fndecl);
  if (!perform_deferred_access_checks (complain))
    access_ok = false;
  pop_access_scope (fndecl);
  pop_deferring_access_checks ();

  /* If we've just instantiated the main entry point for a function,
     instantiate all the alternate entry points as well.  We do this
     by cloning the instantiation of the main entry point, not by
     instantiating the template clones.  */
  if (DECL_CHAIN (gen_tmpl) && DECL_CLONED_FUNCTION_P (DECL_CHAIN (gen_tmpl)))
    clone_function_decl (fndecl, /*update_method_vec_p=*/0);

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

/* Wrapper for instantiate_template_1.  */

tree
instantiate_template (tree tmpl, tree orig_args, tsubst_flags_t complain)
{
  tree ret;
  timevar_push (TV_TEMPLATE_INST);
  ret = instantiate_template_1 (tmpl, orig_args,  complain);
  timevar_pop (TV_TEMPLATE_INST);
  return ret;
}

/* Instantiate the alias template TMPL with ARGS.  Also push a template
   instantiation level, which instantiate_template doesn't do because
   functions and variables have sufficient context established by the
   callers.  */

static tree
instantiate_alias_template (tree tmpl, tree args, tsubst_flags_t complain)
{
  struct pending_template *old_last_pend = last_pending_template;
  struct tinst_level *old_error_tinst = last_error_tinst_level;
  if (tmpl == error_mark_node || args == error_mark_node)
    return error_mark_node;
  tree tinst = build_tree_list (tmpl, args);
  if (!push_tinst_level (tinst))
    {
      ggc_free (tinst);
      return error_mark_node;
    }

  args =
    coerce_innermost_template_parms (DECL_TEMPLATE_PARMS (tmpl),
				     args, tmpl, complain,
				     /*require_all_args=*/true,
				     /*use_default_args=*/true);

  tree r = instantiate_template (tmpl, args, complain);
  pop_tinst_level ();
  /* We can't free this if a pending_template entry or last_error_tinst_level
     is pointing at it.  */
  if (last_pending_template == old_last_pend
      && last_error_tinst_level == old_error_tinst)
    ggc_free (tinst);

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
	if (TREE_VALUE (packs) == parm)
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
     [temp.deduct.call].

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
		     bool explain_p,
		     bool decltype_p)
{
  tree parms;
  tree fntype;
  tree decl = NULL_TREE;
  tsubst_flags_t complain = (explain_p ? tf_warning_or_error : tf_none);
  bool ok;
  static int deduction_depth;
  struct pending_template *old_last_pend = last_pending_template;
  struct tinst_level *old_error_tinst = last_error_tinst_level;
  tree tparms = DECL_INNERMOST_TEMPLATE_PARMS (fn);
  tree tinst;
  tree r = error_mark_node;

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
  tinst = build_tree_list (fn, NULL_TREE);
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
      bool incomplete = false;

      /* Adjust any explicit template arguments before entering the
	 substitution context.  */
      explicit_targs
	= (coerce_template_parms (tparms, explicit_targs, NULL_TREE,
				  complain,
				  /*require_all_args=*/false,
				  /*use_default_args=*/false));
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

	  if (!parameter_pack && targ == NULL_TREE)
	    /* No explicit argument for this template parameter.  */
	    incomplete = true;

          if (parameter_pack && pack_deducible_p (parm, fn))
            {
              /* Mark the argument pack as "incomplete". We could
                 still deduce more arguments during unification.
	         We remove this mark in type_unification_real.  */
              if (targ)
                {
                  ARGUMENT_PACK_INCOMPLETE_P(targ) = 1;
                  ARGUMENT_PACK_EXPLICIT_ARGS (targ) 
                    = ARGUMENT_PACK_ARGS (targ);
                }

              /* We have some incomplete argument packs.  */
              incomplete = true;
            }
        }

      TREE_VALUE (tinst) = explicit_targs;
      if (!push_tinst_level (tinst))
	{
	  excessive_deduction_depth = true;
	  goto fail;
	}
      processing_template_decl += incomplete;
      input_location = DECL_SOURCE_LOCATION (fn);
      /* Ignore any access checks; we'll see them again in
	 instantiate_template and they might have the wrong
	 access path at this point.  */
      push_deferring_access_checks (dk_deferred);
      fntype = tsubst (TREE_TYPE (fn), explicit_targs,
		       complain | tf_partial, NULL_TREE);
      pop_deferring_access_checks ();
      input_location = loc;
      processing_template_decl -= incomplete;
      pop_tinst_level ();

      if (fntype == error_mark_node)
	goto fail;

      /* Place the explicitly specified arguments in TARGS.  */
      for (i = NUM_TMPL_ARGS (explicit_targs); i--;)
	TREE_VEC_ELT (targs, i) = TREE_VEC_ELT (explicit_targs, i);
    }

  /* Never do unification on the 'this' parameter.  */
  parms = skip_artificial_parms_for (fn, TYPE_ARG_TYPES (fntype));

  if (return_type)
    {
      tree *new_args;

      parms = tree_cons (NULL_TREE, TREE_TYPE (fntype), parms);
      new_args = XALLOCAVEC (tree, nargs + 1);
      new_args[0] = return_type;
      memcpy (new_args + 1, args, nargs * sizeof (tree));
      args = new_args;
      ++nargs;
    }

  /* We allow incomplete unification without an error message here
     because the standard doesn't seem to explicitly prohibit it.  Our
     callers must be ready to deal with unification failures in any
     event.  */

  TREE_VALUE (tinst) = targs;
  /* If we aren't explaining yet, push tinst context so we can see where
     any errors (e.g. from class instantiations triggered by instantiation
     of default template arguments) come from.  If we are explaining, this
     context is redundant.  */
  if (!explain_p && !push_tinst_level (tinst))
    {
      excessive_deduction_depth = true;
      goto fail;
    }

  /* type_unification_real will pass back any access checks from default
     template argument substitution.  */
  vec<deferred_access_check, va_gc> *checks;
  checks = NULL;

  ok = !type_unification_real (DECL_INNERMOST_TEMPLATE_PARMS (fn),
			       targs, parms, args, nargs, /*subr=*/0,
			       strict, flags, &checks, explain_p);
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

  /* All is well so far.  Now, check:

     [temp.deduct]

     When all template arguments have been deduced, all uses of
     template parameters in nondeduced contexts are replaced with
     the corresponding deduced argument values.  If the
     substitution results in an invalid type, as described above,
     type deduction fails.  */
  TREE_VALUE (tinst) = targs;
  if (!push_tinst_level (tinst))
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
    }

  r = decl;

 fail:
  --deduction_depth;
  if (excessive_deduction_depth)
    {
      if (deduction_depth == 0)
	/* Reset once we're all the way out.  */
	excessive_deduction_depth = false;
    }

  /* We can't free this if a pending_template entry or last_error_tinst_level
     is pointing at it.  */
  if (last_pending_template == old_last_pend
      && last_error_tinst_level == old_error_tinst)
    ggc_free (tinst);

  return r;
}

/* Adjust types before performing type deduction, as described in
   [temp.deduct.call] and [temp.deduct.conv].  The rules in these two
   sections are symmetric.  PARM is the type of a function parameter
   or the return type of the conversion function.  ARG is the type of
   the argument passed to the call, or the type of the value
   initialized with the result of the conversion function.
   ARG_EXPR is the original argument expression, which may be null.  */

static int
maybe_adjust_types_for_deduction (unification_kind_t strict,
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
      {
	/* Swap PARM and ARG throughout the remainder of this
	   function; the handling is precisely symmetric since PARM
	   will initialize ARG rather than vice versa.  */
	tree* temp = parm;
	parm = arg;
	arg = temp;
	break;
      }

    case DEDUCE_EXACT:
      /* Core issue #873: Do the DR606 thing (see below) for these cases,
	 too, but here handle it by stripping the reference from PARM
	 rather than by adding it to ARG.  */
      if (TREE_CODE (*parm) == REFERENCE_TYPE
	  && TYPE_REF_IS_RVALUE (*parm)
	  && TREE_CODE (TREE_TYPE (*parm)) == TEMPLATE_TYPE_PARM
	  && cp_type_quals (TREE_TYPE (*parm)) == TYPE_UNQUALIFIED
	  && TREE_CODE (*arg) == REFERENCE_TYPE
	  && !TYPE_REF_IS_RVALUE (*arg))
	*parm = TREE_TYPE (*parm);
      /* Nothing else to do in this case.  */
      return 0;

    default:
      gcc_unreachable ();
    }

  if (TREE_CODE (*parm) != REFERENCE_TYPE)
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

  /* From C++0x [14.8.2.1/3 temp.deduct.call] (after DR606), "If P is
     of the form T&&, where T is a template parameter, and the argument
     is an lvalue, T is deduced as A& */
  if (TREE_CODE (*parm) == REFERENCE_TYPE
      && TYPE_REF_IS_RVALUE (*parm)
      && TREE_CODE (TREE_TYPE (*parm)) == TEMPLATE_TYPE_PARM
      && cp_type_quals (TREE_TYPE (*parm)) == TYPE_UNQUALIFIED
      && (arg_expr ? real_lvalue_p (arg_expr)
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
  if (TREE_CODE (*parm) == REFERENCE_TYPE)
    {
      *parm = TREE_TYPE (*parm);
      result |= UNIFY_ALLOW_OUTER_MORE_CV_QUAL;
    }

  /* DR 322. For conversion deduction, remove a reference type on parm
     too (which has been swapped into ARG).  */
  if (strict == DEDUCE_CONV && TREE_CODE (*arg) == REFERENCE_TYPE)
    *arg = TREE_TYPE (*arg);

  return result;
}

/* Subroutine of unify_one_argument.  PARM is a function parameter of a
   template which does contain any deducible template parameters; check if
   ARG is a suitable match for it.  STRICT, FLAGS and EXPLAIN_P are as in
   unify_one_argument.  */

static int
check_non_deducible_conversion (tree parm, tree arg, int strict,
				int flags, bool explain_p)
{
  tree type;

  if (!TYPE_P (arg))
    type = TREE_TYPE (arg);
  else
    type = arg;

  if (same_type_p (parm, type))
    return unify_success (explain_p);

  if (strict == DEDUCE_CONV)
    {
      if (can_convert_arg (type, parm, NULL_TREE, flags,
			   explain_p ? tf_warning_or_error : tf_none))
	return unify_success (explain_p);
    }
  else if (strict != DEDUCE_EXACT)
    {
      if (can_convert_arg (parm, type,
			   TYPE_P (arg) ? NULL_TREE : arg,
			   flags, explain_p ? tf_warning_or_error : tf_none))
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
  for (int i = 0; i < TREE_VEC_LENGTH (args); ++i)
    {
      bool deducible;
      tree elt = TREE_VEC_ELT (args, i);
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
  if (POINTER_TYPE_P (type))
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
  if (TREE_CODE (type) == FUNCTION_TYPE
      || TREE_CODE (type) == METHOD_TYPE)
    {
      if (uses_deducible_template_parms (TREE_TYPE (type)))
	return true;
      tree parm = TYPE_ARG_TYPES (type);
      if (TREE_CODE (type) == METHOD_TYPE)
	parm = TREE_CHAIN (parm);
      for (; parm; parm = TREE_CHAIN (parm))
	if (uses_deducible_template_parms (TREE_VALUE (parm)))
	  return true;
    }

  return false;
}

/* Subroutine of type_unification_real and unify_pack_expansion to
   handle unification of a single P/A pair.  Parameters are as
   for those functions.  */

static int
unify_one_argument (tree tparms, tree targs, tree parm, tree arg,
		    int subr, unification_kind_t strict, int flags,
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
  if (TYPE_P (parm) && !uses_template_parms (parm))
    /* For function parameters that contain no template-parameters at all,
       we have historically checked for convertibility in order to shortcut
       consideration of this candidate.  */
    return check_non_deducible_conversion (parm, arg, strict, flags,
					   explain_p);
  else if (strict == DEDUCE_CALL
	   && TYPE_P (parm) && !uses_deducible_template_parms (parm))
    /* For function parameters with only non-deducible template parameters,
       just return.  */
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

	      if (resolve_overloaded_unification
		  (tparms, targs, parm, arg, strict,
		   arg_strict, explain_p))
		return unify_success (explain_p);
	      return unify_overload_resolution_failure (explain_p, arg);
	    }

	  arg_expr = arg;
	  arg = unlowered_expr_type (arg);
	  if (arg == error_mark_node)
	    return unify_invalid (explain_p);
	}

      arg_strict |=
	maybe_adjust_types_for_deduction (strict, &parm, &arg, arg_expr);
    }
  else
    gcc_assert ((TYPE_P (parm) || TREE_CODE (parm) == TEMPLATE_DECL)
		== (TYPE_P (arg) || TREE_CODE (arg) == TEMPLATE_DECL));

  /* For deduction from an init-list we need the actual list.  */
  if (arg_expr && BRACE_ENCLOSED_INITIALIZER_P (arg_expr))
    arg = arg_expr;
  return unify (tparms, targs, parm, arg, arg_strict, explain_p);
}

/* Most parms like fn_type_unification.

   If SUBR is 1, we're being called recursively (to unify the
   arguments of a function or method parameter of a function
   template).

   CHECKS is a pointer to a vector of access checks encountered while
   substituting default template arguments.  */

static int
type_unification_real (tree tparms,
		       tree targs,
		       tree xparms,
		       const tree *xargs,
		       unsigned int xnargs,
		       int subr,
		       unification_kind_t strict,
		       int flags,
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

  /* Reset the number of non-defaulted template arguments contained
     in TARGS.  */
  NON_DEFAULT_TEMPLATE_ARGS_COUNT (targs) = NULL_TREE;

 again:
  parms = xparms;
  args = xargs;
  nargs = xnargs;

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

      if (unify_one_argument (tparms, targs, parm, arg, subr, strict,
			      flags, explain_p))
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
      if (unify_pack_expansion (tparms, targs, parmvec, argvec, strict,
                                /*subr=*/subr, explain_p))
        return 1;

      /* Advance to the end of the list of parameters.  */
      parms = TREE_CHAIN (parms);
    }

  /* Fail if we've reached the end of the parm list, and more args
     are present, and the parm list isn't variadic.  */
  if (ia < nargs && parms == void_list_node)
    return unify_too_many_arguments (explain_p, nargs, ia);
  /* Fail if parms are left and they don't have default values.  */
  if (parms && parms != void_list_node
      && TREE_PURPOSE (parms) == NULL_TREE)
    {
      unsigned int count = nargs;
      tree p = parms;
      while (p && p != void_list_node)
	{
	  count++;
	  p = TREE_CHAIN (p);
	}
      return unify_too_few_arguments (explain_p, ia, count);
    }

  if (!subr)
    {
      tsubst_flags_t complain = (explain_p
				 ? tf_warning_or_error
				 : tf_none);

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

	  /* If this is an undeduced nontype parameter that depends on
	     a type parameter, try another pass; its type may have been
	     deduced from a later argument than the one from which
	     this parameter can be deduced.  */
	  if (TREE_CODE (tparm) == PARM_DECL
	      && uses_template_parms (TREE_TYPE (tparm))
	      && !saw_undeduced++)
	    goto again;

	  /* Core issue #226 (C++0x) [temp.deduct]:

	     If a template argument has not been deduced, its
	     default template argument, if any, is used. 

	     When we are in C++98 mode, TREE_PURPOSE will either
	     be NULL_TREE or ERROR_MARK_NODE, so we do not need
	     to explicitly check cxx_dialect here.  */
	  if (TREE_PURPOSE (TREE_VEC_ELT (tparms, i)))
	    {
	      tree parm = TREE_VALUE (TREE_VEC_ELT (tparms, i));
	      tree arg = TREE_PURPOSE (TREE_VEC_ELT (tparms, i));
	      reopen_deferring_access_checks (*checks);
	      location_t save_loc = input_location;
	      if (DECL_P (parm))
		input_location = DECL_SOURCE_LOCATION (parm);
	      arg = tsubst_template_arg (arg, targs, complain, NULL_TREE);
	      arg = convert_template_argument (parm, arg, targs, complain,
					       i, NULL_TREE);
	      input_location = save_loc;
	      *checks = get_deferred_access_checks ();
	      pop_deferring_access_checks ();
	      if (arg == error_mark_node)
		return 1;
	      else
		{
		  TREE_VEC_ELT (targs, i) = arg;
		  /* The position of the first default template argument,
		     is also the number of non-defaulted arguments in TARGS.
		     Record that.  */
		  if (!NON_DEFAULT_TEMPLATE_ARGS_COUNT (targs))
		    SET_NON_DEFAULT_TEMPLATE_ARGS_COUNT (targs, i);
		  continue;
		}
	    }

	  /* If the type parameter is a parameter pack, then it will
	     be deduced to an empty parameter pack.  */
	  if (template_parameter_pack_p (tparm))
	    {
	      tree arg;

	      if (TREE_CODE (tparm) == TEMPLATE_PARM_INDEX)
		{
		  arg = make_node (NONTYPE_ARGUMENT_PACK);
		  TREE_TYPE (arg)  = TREE_TYPE (TEMPLATE_PARM_DECL (tparm));
		  TREE_CONSTANT (arg) = 1;
		}
	      else
		arg = cxx_make_type (TYPE_ARGUMENT_PACK);

	      SET_ARGUMENT_PACK_ARGS (arg, make_tree_vec (0));

	      TREE_VEC_ELT (targs, i) = arg;
	      continue;
	    }

	  return unify_parameter_deduction_failure (explain_p, tparm);
	}
    }
#ifdef ENABLE_CHECKING
  if (!NON_DEFAULT_TEMPLATE_ARGS_COUNT (targs))
    SET_NON_DEFAULT_TEMPLATE_ARGS_COUNT (targs, TREE_VEC_LENGTH (targs));
#endif

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

      for (; arg; arg = OVL_NEXT (arg))
	{
	  tree fn = OVL_CURRENT (arg);
	  tree subargs, elem;

	  if (TREE_CODE (fn) != TEMPLATE_DECL)
	    continue;

	  subargs = coerce_template_parms (DECL_INNERMOST_TEMPLATE_PARMS (fn),
					   expl_subargs, NULL_TREE, tf_none,
					   /*require_all_args=*/true,
					   /*use_default_args=*/true);
	  if (subargs != error_mark_node
	      && !any_dependent_template_arguments_p (subargs))
	    {
	      elem = TREE_TYPE (instantiate_template (fn, subargs, tf_none));
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
  else if (TREE_CODE (arg) != OVERLOAD
	   && TREE_CODE (arg) != FUNCTION_DECL)
    /* If ARG is, for example, "(0, &f)" then its type will be unknown
       -- but the deduction does not succeed because the expression is
       not just the function on its own.  */
    return false;
  else
    for (; arg; arg = OVL_NEXT (arg))
      if (try_one_overload (tparms, targs, tempargs, parm,
			    TREE_TYPE (OVL_CURRENT (arg)),
			    strict, sub_strict, addr_p, explain_p)
	  && (!goodfn || !decls_match (goodfn, OVL_CURRENT (arg))))
	{
	  goodfn = OVL_CURRENT (arg);
	  ++good;
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
	  TREE_VEC_ELT (targs, i) = TREE_VEC_ELT (tempargs, i);
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
resolve_nondeduced_context (tree orig_expr)
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

      for (; arg; arg = OVL_NEXT (arg))
	{
	  tree fn = OVL_CURRENT (arg);
	  tree subargs, elem;

	  if (TREE_CODE (fn) != TEMPLATE_DECL)
	    continue;

	  subargs = coerce_template_parms (DECL_INNERMOST_TEMPLATE_PARMS (fn),
					   expl_subargs, NULL_TREE, tf_none,
					   /*require_all_args=*/true,
					   /*use_default_args=*/true);
	  if (subargs != error_mark_node
	      && !any_dependent_template_arguments_p (subargs))
	    {
	      elem = instantiate_template (fn, subargs, tf_none);
	      if (elem == error_mark_node)
		{
		  badfn = fn;
		  badargs = subargs;
		}
	      else if (elem && (!goodfn || !decls_match (goodfn, elem)))
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
	      expr = build_offset_ref (base, expr, addr, tf_warning_or_error);
	    }
	  if (addr)
	    expr = cp_build_addr_expr (expr, tf_warning_or_error);
	  return expr;
	}
      else if (good == 0 && badargs)
	/* There were no good options and at least one bad one, so let the
	   user know what the problem is.  */
	instantiate_template (badfn, badargs, tf_warning_or_error);
    }
  return orig_expr;
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

  sub_strict |= maybe_adjust_types_for_deduction (strict, &parm, &arg, NULL);

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
  tree copy_of_targs;

  if (!CLASSTYPE_TEMPLATE_INFO (arg)
      || (most_general_template (CLASSTYPE_TI_TEMPLATE (arg))
	  != most_general_template (CLASSTYPE_TI_TEMPLATE (parm))))
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
  copy_of_targs = make_tree_vec (TREE_VEC_LENGTH (targs));

  /* If unification failed, we're done.  */
  if (unify (tparms, copy_of_targs, CLASSTYPE_TI_ARGS (parm),
	     CLASSTYPE_TI_ARGS (arg), UNIFY_ALLOW_NONE, explain_p))
    return NULL_TREE;

  return arg;
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
	      *result = NULL_TREE;
	      return tbr_ambiguous_baseclass;
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
      if ((TREE_CODE (arg) == REFERENCE_TYPE
	   || TREE_CODE (arg) == FUNCTION_TYPE
	   || TREE_CODE (arg) == METHOD_TYPE)
	  && (parm_quals & (TYPE_QUAL_CONST | TYPE_QUAL_VOLATILE)))
	return 0;

      if ((!POINTER_TYPE_P (arg) && TREE_CODE (arg) != TEMPLATE_TYPE_PARM)
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
  } while (0);

/* Unifies the remaining arguments in PACKED_ARGS with the pack
   expansion at the end of PACKED_PARMS. Returns 0 if the type
   deduction succeeds, 1 otherwise. STRICT is the same as in
   unify. CALL_ARGS_P is true iff PACKED_ARGS is actually a function
   call argument list. We'll need to adjust the arguments to make them
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

  packed_args = expand_template_argument_pack (packed_args);

  int len = TREE_VEC_LENGTH (packed_args);

  /* Determine the parameter packs we will be deducing from the
     pattern, and record their current deductions.  */
  for (pack = PACK_EXPANSION_PARAMETER_PACKS (parm); 
       pack; pack = TREE_CHAIN (pack))
    {
      tree parm_pack = TREE_VALUE (pack);
      int idx, level;

      /* Determine the index and level of this parameter pack.  */
      template_parm_level_and_index (parm_pack, &level, &idx);

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
			      LOOKUP_IMPLICIT, explain_p))
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
              TREE_TYPE (result) = 
                TREE_TYPE (TEMPLATE_PARM_DECL (TREE_PURPOSE (pack)));
              TREE_CONSTANT (result) = 1;
            }
          else
            result = cxx_make_type (TYPE_ARGUMENT_PACK);

          SET_ARGUMENT_PACK_ARGS (result, new_args);

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

          SET_ARGUMENT_PACK_ARGS (old_pack, new_args);
          ARGUMENT_PACK_INCOMPLETE_P (old_pack) = 1;
          ARGUMENT_PACK_EXPLICIT_ARGS (old_pack) = explicit_args;
        }
      else
	{
	  tree bad_old_arg = NULL_TREE, bad_new_arg = NULL_TREE;
	  tree old_args = ARGUMENT_PACK_ARGS (old_pack);

	  if (!comp_template_args_with_info (old_args, new_args,
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

  /* I don't think this will do the right thing with respect to types.
     But the only case I've seen it in so far has been array bounds, where
     signedness is the only information lost, and I think that will be
     okay.  */
  while (TREE_CODE (parm) == NOP_EXPR)
    parm = TREE_OPERAND (parm, 0);

  if (arg == error_mark_node)
    return unify_invalid (explain_p);
  if (arg == unknown_type_node
      || arg == init_list_type_node)
    /* We can't deduce anything from this, but we might get all the
       template args from other function args.  */
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
      tree elt, elttype;
      unsigned i;
      tree orig_parm = parm;

      /* Replace T with std::initializer_list<T> for deduction.  */
      if (TREE_CODE (parm) == TEMPLATE_TYPE_PARM
	  && flag_deduce_init_list)
	parm = listify (parm);

      if (!is_std_init_list (parm)
	  && TREE_CODE (parm) != ARRAY_TYPE)
	/* We can only deduce from an initializer list argument if the
	   parameter is std::initializer_list or an array; otherwise this
	   is a non-deduced context. */
	return unify_success (explain_p);

      if (TREE_CODE (parm) == ARRAY_TYPE)
	elttype = TREE_TYPE (parm);
      else
	elttype = TREE_VEC_ELT (CLASSTYPE_TI_ARGS (parm), 0);

      FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (arg), i, elt)
	{
	  int elt_strict = strict;

	  if (elt == error_mark_node)
	    return unify_invalid (explain_p);

	  if (!BRACE_ENCLOSED_INITIALIZER_P (elt))
	    {
	      tree type = TREE_TYPE (elt);
	      /* It should only be possible to get here for a call.  */
	      gcc_assert (elt_strict & UNIFY_ALLOW_OUTER_LEVEL);
	      elt_strict |= maybe_adjust_types_for_deduction
		(DEDUCE_CALL, &elttype, &type, elt);
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
      && TYPE_P (parm) && !CP_TYPE_CONST_P (parm))
    strict &= ~UNIFY_ALLOW_MORE_CV_QUAL;
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
      if (tparm == error_mark_node)
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
      if (tparm == error_mark_node)
	return unify_invalid (explain_p);

      /* Check for mixed types and values.  */
      if ((TREE_CODE (parm) == TEMPLATE_TYPE_PARM
	   && TREE_CODE (tparm) != TYPE_DECL)
	  || (TREE_CODE (parm) == TEMPLATE_TEMPLATE_PARM
	      && TREE_CODE (tparm) != TEMPLATE_DECL))
	gcc_unreachable ();

      if (TREE_CODE (parm) == BOUND_TEMPLATE_TEMPLATE_PARM)
	{
	  /* ARG must be constructed from a template class or a template
	     template parameter.  */
	  if (TREE_CODE (arg) != BOUND_TEMPLATE_TEMPLATE_PARM
	      && !CLASSTYPE_SPECIALIZATION_OF_PRIMARY_TEMPLATE_P (arg))
	    return unify_template_deduction_failure (explain_p, parm, arg);
	  {
	    tree parmvec = TYPE_TI_ARGS (parm);
	    /* An alias template name is never deduced.  */
	    if (TYPE_ALIAS_P (arg))
	      arg = strip_typedefs (arg);
	    tree argvec = INNERMOST_TEMPLATE_ARGS (TYPE_TI_ARGS (arg));
	    tree full_argvec = add_to_template_args (targs, argvec);
	    tree parm_parms 
              = DECL_INNERMOST_TEMPLATE_PARMS
	          (TEMPLATE_TEMPLATE_PARM_TEMPLATE_DECL (parm));
	    int i, len;
            int parm_variadic_p = 0;

	    /* The resolution to DR150 makes clear that default
	       arguments for an N-argument may not be used to bind T
	       to a template template parameter with fewer than N
	       parameters.  It is not safe to permit the binding of
	       default arguments as an extension, as that may change
	       the meaning of a conforming program.  Consider:

		  struct Dense { static const unsigned int dim = 1; };

		  template <template <typename> class View,
			    typename Block>
		  void operator+(float, View<Block> const&);

		  template <typename Block,
			    unsigned int Dim = Block::dim>
		  struct Lvalue_proxy { operator float() const; };

		  void
		  test_1d (void) {
		    Lvalue_proxy<Dense> p;
		    float b;
		    b + p;
		  }

	      Here, if Lvalue_proxy is permitted to bind to View, then
	      the global operator+ will be used; if they are not, the
	      Lvalue_proxy will be converted to float.  */
	    if (coerce_template_parms (parm_parms,
                                       full_argvec,
				       TYPE_TI_TEMPLATE (parm),
				       (explain_p
					? tf_warning_or_error
					: tf_none),
				       /*require_all_args=*/true,
				       /*use_default_args=*/false)
		== error_mark_node)
	      return 1;

	    /* Deduce arguments T, i from TT<T> or TT<i>.
	       We check each element of PARMVEC and ARGVEC individually
	       rather than the whole TREE_VEC since they can have
	       different number of elements.  */

            parmvec = expand_template_argument_pack (parmvec);
            argvec = expand_template_argument_pack (argvec);

            len = TREE_VEC_LENGTH (parmvec);

            /* Check if the parameters end in a pack, making them
               variadic.  */
            if (len > 0
                && PACK_EXPANSION_P (TREE_VEC_ELT (parmvec, len - 1)))
              parm_variadic_p = 1;
            
             for (i = 0; i < len - parm_variadic_p; ++i)
	       /* If the template argument list of P contains a pack
		  expansion that is not the last template argument, the
		  entire template argument list is a non-deduced
		  context.  */
	       if (PACK_EXPANSION_P (TREE_VEC_ELT (parmvec, i)))
		 return unify_success (explain_p);

            if (TREE_VEC_LENGTH (argvec) < len - parm_variadic_p)
              return unify_too_few_arguments (explain_p,
					      TREE_VEC_LENGTH (argvec), len);

             for (i = 0; i < len - parm_variadic_p; ++i)
	      {
		RECUR_AND_CHECK_FAILURE (tparms, targs,
					 TREE_VEC_ELT (parmvec, i),
					 TREE_VEC_ELT (argvec, i),
					 UNIFY_ALLOW_NONE, explain_p);
	      }

	    if (parm_variadic_p
		&& unify_pack_expansion (tparms, targs,
					 parmvec, argvec,
					 DEDUCE_EXACT,
					 /*subr=*/true, explain_p))
	      return 1;
	  }
	  arg = TYPE_TI_TEMPLATE (arg);

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
	  arg = cp_build_qualified_type_real
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
	 the deduction can not be of that type.  */
      if (TREE_CODE (arg) == METHOD_TYPE)
	return unify_method_type_error (explain_p, arg);

      TREE_VEC_ELT (INNERMOST_TEMPLATE_ARGS (targs), idx) = arg;
      return unify_success (explain_p);

    case TEMPLATE_PARM_INDEX:
      tparm = TREE_VALUE (TREE_VEC_ELT (tparms, 0));
      if (tparm == error_mark_node)
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
      tparm = tsubst (TREE_TYPE (parm), targs, 0, NULL_TREE);
      if (!TREE_TYPE (arg))
	/* Template-parameter dependent expression.  Just accept it for now.
	   It will later be processed in convert_template_argument.  */
	;
      else if (same_type_p (TREE_TYPE (arg), tparm))
	/* OK */;
      else if ((strict & UNIFY_ALLOW_INTEGER)
	       && CP_INTEGRAL_TYPE_P (tparm))
	/* Convert the ARG to the type of PARM; the deduced non-type
	   template argument must exactly match the types of the
	   corresponding parameter.  */
	arg = fold (build_nop (tparm, arg));
      else if (uses_template_parms (tparm))
	/* We haven't deduced the type of this parameter yet.  Try again
	   later.  */
	return unify_success (explain_p);
      else
	return unify_type_mismatch (explain_p, tparm, TREE_TYPE (arg));

      /* If ARG is a parameter pack or an expansion, we cannot unify
	 against it unless PARM is also a parameter pack.  */
      if ((template_parameter_pack_p (arg) || PACK_EXPANSION_P (arg))
	  && !TEMPLATE_PARM_PARAMETER_PACK (parm))
	return unify_parameter_pack_mismatch (explain_p, parm, arg);

      arg = strip_typedefs_expr (arg);
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
      if (TREE_CODE (arg) != REFERENCE_TYPE)
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
      while (TREE_CODE (arg) == NOP_EXPR)
	arg = TREE_OPERAND (arg, 0);

      if (TREE_CODE (arg) != INTEGER_CST)
	return unify_template_argument_mismatch (explain_p, parm, arg);
      return (tree_int_cst_equal (parm, arg)
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
		    return unify_no_common_base (explain_p, r, parm, arg);
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

	RECUR_AND_CHECK_FAILURE (tparms, targs, TREE_TYPE (parm),
				 TREE_TYPE (arg), UNIFY_ALLOW_NONE, explain_p);

	nargs = list_length (TYPE_ARG_TYPES (arg));
	args = XALLOCAVEC (tree, nargs);
	for (a = TYPE_ARG_TYPES (arg), i = 0;
	     a != NULL_TREE && a != void_list_node;
	     a = TREE_CHAIN (a), ++i)
	  args[i] = TREE_VALUE (a);
	nargs = i;

	return type_unification_real (tparms, targs, TYPE_ARG_TYPES (parm),
				      args, nargs, 1, DEDUCE_EXACT,
				      LOOKUP_NORMAL, NULL, explain_p);
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
      if (arg != integral_constant_value (parm))
	return unify_template_argument_mismatch (explain_p, parm, arg);
      return unify_success (explain_p);

    case FIELD_DECL:
    case TEMPLATE_DECL:
      /* Matched cases are handled by the ARG == PARM test above.  */
      return unify_template_argument_mismatch (explain_p, parm, arg);

    case VAR_DECL:
      /* A non-type template parameter that is a variable should be a
	 an integral constant, in which case, it whould have been
	 folded into its (constant) value. So we should not be getting
	 a variable here.  */
      gcc_unreachable ();

    case TYPE_ARGUMENT_PACK:
    case NONTYPE_ARGUMENT_PACK:
      return unify (tparms, targs, ARGUMENT_PACK_ARGS (parm),
		    ARGUMENT_PACK_ARGS (arg), strict, explain_p);

    case TYPEOF_TYPE:
    case DECLTYPE_TYPE:
    case UNDERLYING_TYPE:
      /* Cannot deduce anything from TYPEOF_TYPE, DECLTYPE_TYPE,
	 or UNDERLYING_TYPE nodes.  */
      return unify_success (explain_p);

    case ERROR_MARK:
      /* Unification fails if we hit an error node.  */
      return unify_invalid (explain_p);

    case INDIRECT_REF:
      if (REFERENCE_REF_P (parm))
	return unify (tparms, targs, TREE_OPERAND (parm, 0), arg,
		      strict, explain_p);
      /* FALLTHRU */

    default:
      /* An unresolved overload is a nondeduced context.  */
      if (is_overloaded_fn (parm) || type_unknown_p (parm))
	return unify_success (explain_p);
      gcc_assert (EXPR_P (parm));

      /* We must be looking at an expression.  This can happen with
	 something like:

	   template <int I>
	   void foo(S<I>, S<I + 2>);

	 This is a "nondeduced context":

	   [deduct.type]

	   The nondeduced contexts are:

	   --A type that is a template-id in which one or more of
	     the template-arguments is an expression that references
	     a template-parameter.

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

  /* For anonymous namespace we don't need to do anything.  */
  if (decl_anon_ns_mem_p (result))
    {
      gcc_assert (!TREE_PUBLIC (result));
      return;
    }

  if (TREE_CODE (result) != FUNCTION_DECL)
    /* The TREE_PUBLIC flag for function declarations will have been
       set correctly by tsubst.  */
    TREE_PUBLIC (result) = 1;

  /* This might have been set by an earlier implicit instantiation.  */
  DECL_COMDAT (result) = 0;

  if (extern_p)
    DECL_NOT_REALLY_EXTERN (result) = 0;
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
  int i;
  for (i = TREE_VEC_LENGTH (targs) - 1; i >= 0; --i)
    if (TREE_VEC_ELT (targs, i) == NULL_TREE)
      {
	found = true;
	TREE_VEC_ELT (targs, i) = error_mark_node;
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

      if (TREE_CODE (arg1) == REFERENCE_TYPE)
	{
	  ref1 = TYPE_REF_IS_RVALUE (arg1) + 1;
	  arg1 = TREE_TYPE (arg1);
	  quals1 = cp_type_quals (arg1);
	}

      if (TREE_CODE (arg2) == REFERENCE_TYPE)
	{
	  ref2 = TYPE_REF_IS_RVALUE (arg2) + 1;
	  arg2 = TREE_TYPE (arg2);
	  quals2 = cp_type_quals (arg2);
	}

      arg1 = TYPE_MAIN_VARIANT (arg1);
      arg2 = TYPE_MAIN_VARIANT (arg2);

      if (TREE_CODE (arg1) == TYPE_PACK_EXPANSION)
        {
          int i, len2 = list_length (args2);
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
          int i, len1 = list_length (args1);
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

   PAT1 is a TREE_LIST whose TREE_TYPE is the _TYPE node corresponding
   to the first partial specialization.  The TREE_VALUE is the
   innermost set of template parameters for the partial
   specialization.  PAT2 is similar, but for the second template.

   Return 1 if the first partial specialization is more specialized;
   -1 if the second is more specialized; 0 if neither is more
   specialized.

   See [temp.class.order] for information about determining which of
   two templates is more specialized.  */

static int
more_specialized_class (tree tmpl, tree pat1, tree pat2)
{
  tree targs;
  tree tmpl1, tmpl2;
  int winner = 0;
  bool any_deductions = false;

  tmpl1 = TREE_TYPE (pat1);
  tmpl2 = TREE_TYPE (pat2);

  /* Just like what happens for functions, if we are ordering between
     different class template specializations, we may encounter dependent
     types in the arguments, and we need our dependency check functions
     to behave correctly.  */
  ++processing_template_decl;
  targs = get_class_bindings (tmpl, TREE_VALUE (pat1),
			      CLASSTYPE_TI_ARGS (tmpl1),
			      CLASSTYPE_TI_ARGS (tmpl2));
  if (targs)
    {
      --winner;
      any_deductions = true;
    }

  targs = get_class_bindings (tmpl, TREE_VALUE (pat2),
			      CLASSTYPE_TI_ARGS (tmpl2),
			      CLASSTYPE_TI_ARGS (tmpl1));
  if (targs)
    {
      ++winner;
      any_deductions = true;
    }
  --processing_template_decl;

  /* In the case of a tie where at least one of the class templates
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
       arg != NULL_TREE && arg != void_list_node;
       arg = TREE_CHAIN (arg), ++ix)
    args[ix] = TREE_VALUE (arg);

  if (fn_type_unification (fn, explicit_args, targs,
			   args, ix,
			   (check_rettype || DECL_CONV_FN_P (fn)
			    ? TREE_TYPE (decl_type) : NULL_TREE),
			   DEDUCE_EXACT, LOOKUP_NORMAL, /*explain_p=*/false,
			   /*decltype*/false)
      == error_mark_node)
    return NULL_TREE;

  return targs;
}

/* Return the innermost template arguments that, when applied to a partial
   specialization of TMPL whose innermost template parameters are
   TPARMS, and whose specialization arguments are SPEC_ARGS, yield the
   ARGS.

   For example, suppose we have:

     template <class T, class U> struct S {};
     template <class T> struct S<T*, int> {};

   Then, suppose we want to get `S<double*, int>'.  The TPARMS will be
   {T}, the SPEC_ARGS will be {T*, int} and the ARGS will be {double*,
   int}.  The resulting vector will be {double}, indicating that `T'
   is bound to `double'.  */

static tree
get_class_bindings (tree tmpl, tree tparms, tree spec_args, tree args)
{
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

  if (unify (tparms, deduced_args,
	     INNERMOST_TEMPLATE_ARGS (spec_args),
	     INNERMOST_TEMPLATE_ARGS (args),
	     UNIFY_ALLOW_NONE, /*explain_p=*/false))
    return NULL_TREE;

  for (i =  0; i < ntparms; ++i)
    if (! TREE_VEC_ELT (innermost_deduced_args, i))
      return NULL_TREE;

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
  spec_args = coerce_template_parms (DECL_INNERMOST_TEMPLATE_PARMS (tmpl),
				     spec_args, tmpl,
				     tf_none, false, false);
  if (spec_args == error_mark_node
      /* We only need to check the innermost arguments; the other
	 arguments will always agree.  */
      || !comp_template_args (INNERMOST_TEMPLATE_ARGS (spec_args),
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
      int fate = 0;

      if (get_bindings (TREE_VALUE (champ),
			DECL_TEMPLATE_RESULT (TREE_VALUE (fn)),
			NULL_TREE, /*check_ret=*/true))
	fate--;

      if (get_bindings (TREE_VALUE (fn),
			DECL_TEMPLATE_RESULT (TREE_VALUE (champ)),
			NULL_TREE, /*check_ret=*/true))
	fate++;

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
    for (fn = templates; fn != champ; fn = TREE_CHAIN (fn))
      if (get_bindings (TREE_VALUE (champ),
			DECL_TEMPLATE_RESULT (TREE_VALUE (fn)),
			NULL_TREE, /*check_ret=*/true)
	  || !get_bindings (TREE_VALUE (fn),
			    DECL_TEMPLATE_RESULT (TREE_VALUE (champ)),
			    NULL_TREE, /*check_ret=*/true))
	{
	  champ = NULL_TREE;
	  break;
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
  /* If DECL is a FUNCTION_DECL, find the TEMPLATE_DECL of which it is
     an immediate specialization.  */
  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      if (DECL_TEMPLATE_INFO (decl)) {
	decl = DECL_TI_TEMPLATE (decl);

	/* The DECL_TI_TEMPLATE can be an IDENTIFIER_NODE for a
	   template friend.  */
	if (TREE_CODE (decl) != TEMPLATE_DECL)
	  return NULL_TREE;
      } else
	return NULL_TREE;
    }

  /* Look for more and more general templates.  */
  while (DECL_TEMPLATE_INFO (decl))
    {
      /* The DECL_TI_TEMPLATE can be an IDENTIFIER_NODE in some cases.
	 (See cp-tree.h for details.)  */
      if (TREE_CODE (DECL_TI_TEMPLATE (decl)) != TEMPLATE_DECL)
	break;

      if (CLASS_TYPE_P (TREE_TYPE (decl))
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

/* Return the most specialized of the class template partial
   specializations which can produce TYPE, a specialization of some class
   template.  The value returned is actually a TREE_LIST; the TREE_TYPE is
   a _TYPE node corresponding to the partial specialization, while the
   TREE_PURPOSE is the set of template arguments that must be
   substituted into the TREE_TYPE in order to generate TYPE.

   If the choice of partial specialization is ambiguous, a diagnostic
   is issued, and the error_mark_node is returned.  If there are no
   partial specializations matching TYPE, then NULL_TREE is
   returned, indicating that the primary template should be used.  */

static tree
most_specialized_class (tree type, tsubst_flags_t complain)
{
  tree list = NULL_TREE;
  tree t;
  tree champ;
  int fate;
  bool ambiguous_p;
  tree outer_args = NULL_TREE;

  tree tmpl = CLASSTYPE_TI_TEMPLATE (type);
  tree main_tmpl = most_general_template (tmpl);
  tree args = CLASSTYPE_TI_ARGS (type);

  /* For determining which partial specialization to use, only the
     innermost args are interesting.  */
  if (TMPL_ARGS_HAVE_MULTIPLE_LEVELS (args))
    {
      outer_args = strip_innermost_template_args (args, 1);
      args = INNERMOST_TEMPLATE_ARGS (args);
    }

  for (t = DECL_TEMPLATE_SPECIALIZATIONS (main_tmpl); t; t = TREE_CHAIN (t))
    {
      tree partial_spec_args;
      tree spec_args;
      tree spec_tmpl = TREE_VALUE (t);
      tree orig_parms = DECL_INNERMOST_TEMPLATE_PARMS (spec_tmpl);

      partial_spec_args = CLASSTYPE_TI_ARGS (TREE_TYPE (t));

      ++processing_template_decl;

      if (outer_args)
	{
	  /* Discard the outer levels of args, and then substitute in the
	     template args from the enclosing class.  */
	  partial_spec_args = INNERMOST_TEMPLATE_ARGS (partial_spec_args);
	  partial_spec_args = tsubst_template_args
	    (partial_spec_args, outer_args, tf_none, NULL_TREE);

	  /* And the same for the partial specialization TEMPLATE_DECL.  */
	  spec_tmpl = tsubst (spec_tmpl, outer_args, tf_none, NULL_TREE);
	}

      partial_spec_args =
	  coerce_template_parms (DECL_INNERMOST_TEMPLATE_PARMS (tmpl),
				 partial_spec_args,
				 tmpl, tf_none,
				 /*require_all_args=*/true,
				 /*use_default_args=*/true);

      --processing_template_decl;

      if (partial_spec_args == error_mark_node)
	return error_mark_node;
      if (spec_tmpl == error_mark_node)
	return error_mark_node;

      tree parms = DECL_INNERMOST_TEMPLATE_PARMS (spec_tmpl);
      spec_args = get_class_bindings (tmpl, parms,
				      partial_spec_args,
				      args);
      if (spec_args)
	{
	  if (outer_args)
	    spec_args = add_to_template_args (outer_args, spec_args);
	  list = tree_cons (spec_args, orig_parms, list);
	  TREE_TYPE (list) = TREE_TYPE (t);
	}
    }

  if (! list)
    return NULL_TREE;

  ambiguous_p = false;
  t = list;
  champ = t;
  t = TREE_CHAIN (t);
  for (; t; t = TREE_CHAIN (t))
    {
      fate = more_specialized_class (tmpl, champ, t);
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
    for (t = list; t && t != champ; t = TREE_CHAIN (t))
      {
	fate = more_specialized_class (tmpl, champ, t);
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
      error ("ambiguous class template instantiation for %q#T", type);
      str = ngettext ("candidate is:", "candidates are:", list_length (list));
      for (t = list; t; t = TREE_CHAIN (t))
        {
          error ("%s %+#T", spaces ? spaces : str, TREE_TYPE (t));
          spaces = spaces ? spaces : get_spaces (str);
        }
      free (spaces);
      return error_mark_node;
    }

  return champ;
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
  else if (VAR_P (decl))
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
  else if (TREE_CODE (decl) != FUNCTION_DECL)
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
      if (!in_system_header_at (input_location) && (cxx_dialect == cxx98))
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
    instantiate_decl (result, /*defer_ok=*/1,
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

/* Called from do_type_instantiation through binding_table_foreach to
   do recursive instantiation for the type bound in ENTRY.  */
static void
bt_instantiate_type_proc (binding_entry entry, void *data)
{
  tree storage = *(tree *) data;

  if (MAYBE_CLASS_TYPE_P (entry->type)
      && !uses_template_parms (CLASSTYPE_TI_ARGS (entry->type)))
    do_type_instantiation (TYPE_MAIN_DECL (entry->type), storage, 0);
}

/* Called from do_type_instantiation to instantiate a member
   (a member function or a static member variable) of an
   explicitly instantiated class template.  */
static void
instantiate_class_member (tree decl, int extern_p)
{
  mark_decl_instantiated (decl, extern_p);
  if (! extern_p)
    instantiate_decl (decl, /*defer_ok=*/1,
		      /*expl_inst_class_mem_p=*/true);
}

/* Perform an explicit instantiation of template class T.  STORAGE, if
   non-null, is the RID for extern, inline or static.  COMPLAIN is
   nonzero if this is called from the parser, zero if called recursively,
   since the standard is unclear (as detailed below).  */

void
do_type_instantiation (tree t, tree storage, tsubst_flags_t complain)
{
  int extern_p = 0;
  int nomem_p = 0;
  int static_p = 0;
  int previous_instantiation_extern_p = 0;

  if (TREE_CODE (t) == TYPE_DECL)
    t = TREE_TYPE (t);

  if (! CLASS_TYPE_P (t) || ! CLASSTYPE_TEMPLATE_INFO (t))
    {
      tree tmpl =
	(TYPE_TEMPLATE_INFO (t)) ? TYPE_TI_TEMPLATE (t) : NULL;
      if (tmpl)
	error ("explicit instantiation of non-class template %qD", tmpl);
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

  if (storage != NULL_TREE)
    {
      if (!in_system_header_at (input_location))
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
	}

      if (storage == ridpointers[(int) RID_INLINE])
	nomem_p = 1;
      else if (storage == ridpointers[(int) RID_EXTERN])
	extern_p = 1;
      else if (storage == ridpointers[(int) RID_STATIC])
	static_p = 1;
      else
	{
	  error ("storage class %qD applied to template instantiation",
		 storage);
	  extern_p = 0;
	}
    }

  if (CLASSTYPE_TEMPLATE_SPECIALIZATION (t))
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
  else if (CLASSTYPE_EXPLICIT_INSTANTIATION (t))
    {
      /* [temp.spec]

	 No program shall explicitly instantiate any template more
	 than once.

	 If PREVIOUS_INSTANTIATION_EXTERN_P, then the first explicit
	 instantiation was `extern'.  If EXTERN_P then the second is.
	 These cases are OK.  */
      previous_instantiation_extern_p = CLASSTYPE_INTERFACE_ONLY (t);

      if (!previous_instantiation_extern_p && !extern_p
	  && (complain & tf_error))
	permerror (input_location, "duplicate explicit instantiation of %q#T", t);

      /* If we've already instantiated the template, just return now.  */
      if (!CLASSTYPE_INTERFACE_ONLY (t))
	return;
    }

  check_explicit_instantiation_namespace (TYPE_NAME (t));
  mark_class_instantiated (t, extern_p);

  if (nomem_p)
    return;

  {
    tree tmp;

    /* In contrast to implicit instantiation, where only the
       declarations, and not the definitions, of members are
       instantiated, we have here:

	 [temp.explicit]

	 The explicit instantiation of a class template specialization
	 implies the instantiation of all of its members not
	 previously explicitly specialized in the translation unit
	 containing the explicit instantiation.

       Of course, we can't instantiate member template classes, since
       we don't have any arguments for them.  Note that the standard
       is unclear on whether the instantiation of the members are
       *explicit* instantiations or not.  However, the most natural
       interpretation is that it should be an explicit instantiation.  */

    if (! static_p)
      for (tmp = TYPE_METHODS (t); tmp; tmp = DECL_CHAIN (tmp))
	if (TREE_CODE (tmp) == FUNCTION_DECL
	    && DECL_TEMPLATE_INSTANTIATION (tmp))
	  instantiate_class_member (tmp, extern_p);

    for (tmp = TYPE_FIELDS (t); tmp; tmp = DECL_CHAIN (tmp))
      if (VAR_P (tmp) && DECL_TEMPLATE_INSTANTIATION (tmp))
	instantiate_class_member (tmp, extern_p);

    if (CLASSTYPE_NESTED_UTDS (t))
      binding_table_foreach (CLASSTYPE_NESTED_UTDS (t),
			     bt_instantiate_type_proc, &storage);
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
regenerate_decl_from_template (tree decl, tree tmpl)
{
  /* The arguments used to instantiate DECL, from the most general
     template.  */
  tree args;
  tree code_pattern;

  args = DECL_TI_ARGS (decl);
  code_pattern = DECL_TEMPLATE_RESULT (tmpl);

  /* Make sure that we can see identifiers, and compute access
     correctly.  */
  push_access_scope (decl);

  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      tree decl_parm;
      tree pattern_parm;
      tree specs;
      int args_depth;
      int parms_depth;

      args_depth = TMPL_ARGS_DEPTH (args);
      parms_depth = TMPL_PARMS_DEPTH (DECL_TEMPLATE_PARMS (tmpl));
      if (args_depth > parms_depth)
	args = get_innermost_template_args (args, parms_depth);

      specs = tsubst_exception_specification (TREE_TYPE (code_pattern),
					      args, tf_error, NULL_TREE,
					      /*defer_ok*/false);
      if (specs && specs != error_mark_node)
	TREE_TYPE (decl) = build_exception_variant (TREE_TYPE (decl),
						    specs);

      /* Merge parameter declarations.  */
      decl_parm = skip_artificial_parms_for (decl,
					     DECL_ARGUMENTS (decl));
      pattern_parm
	= skip_artificial_parms_for (code_pattern,
				     DECL_ARGUMENTS (code_pattern));
      while (decl_parm && !DECL_PACK_P (pattern_parm))
	{
	  tree parm_type;
	  tree attributes;
          
	  if (DECL_NAME (decl_parm) != DECL_NAME (pattern_parm))
	    DECL_NAME (decl_parm) = DECL_NAME (pattern_parm);
	  parm_type = tsubst (TREE_TYPE (pattern_parm), args, tf_error,
			      NULL_TREE);
	  parm_type = type_decays_to (parm_type);
	  if (!same_type_p (TREE_TYPE (decl_parm), parm_type))
	    TREE_TYPE (decl_parm) = parm_type;
	  attributes = DECL_ATTRIBUTES (pattern_parm);
	  if (DECL_ATTRIBUTES (decl_parm) != attributes)
	    {
	      DECL_ATTRIBUTES (decl_parm) = attributes;
	      cplus_decl_attributes (&decl_parm, attributes, /*flags=*/0);
	    }
	  decl_parm = DECL_CHAIN (decl_parm);
	  pattern_parm = DECL_CHAIN (pattern_parm);
	}
      /* Merge any parameters that match with the function parameter
         pack.  */
      if (pattern_parm && DECL_PACK_P (pattern_parm))
        {
          int i, len;
          tree expanded_types;
          /* Expand the TYPE_PACK_EXPANSION that provides the types for
             the parameters in this function parameter pack.  */
          expanded_types = tsubst_pack_expansion (TREE_TYPE (pattern_parm), 
                                                 args, tf_error, NULL_TREE);
          len = TREE_VEC_LENGTH (expanded_types);
          for (i = 0; i < len; i++)
            {
              tree parm_type;
              tree attributes;
          
              if (DECL_NAME (decl_parm) != DECL_NAME (pattern_parm))
                /* Rename the parameter to include the index.  */
                DECL_NAME (decl_parm) = 
                  make_ith_pack_parameter_name (DECL_NAME (pattern_parm), i);
              parm_type = TREE_VEC_ELT (expanded_types, i);
              parm_type = type_decays_to (parm_type);
              if (!same_type_p (TREE_TYPE (decl_parm), parm_type))
                TREE_TYPE (decl_parm) = parm_type;
              attributes = DECL_ATTRIBUTES (pattern_parm);
              if (DECL_ATTRIBUTES (decl_parm) != attributes)
                {
                  DECL_ATTRIBUTES (decl_parm) = attributes;
                  cplus_decl_attributes (&decl_parm, attributes, /*flags=*/0);
                }
              decl_parm = DECL_CHAIN (decl_parm);
            }
        }
      /* Merge additional specifiers from the CODE_PATTERN.  */
      if (DECL_DECLARED_INLINE_P (code_pattern)
	  && !DECL_DECLARED_INLINE_P (decl))
	DECL_DECLARED_INLINE_P (decl) = 1;
    }
  else if (VAR_P (decl))
    {
      DECL_INITIAL (decl) =
	tsubst_expr (DECL_INITIAL (code_pattern), args,
		     tf_error, DECL_TI_TEMPLATE (decl),
		     /*integral_constant_expression_p=*/false);
      if (VAR_HAD_UNKNOWN_BOUND (decl))
	TREE_TYPE (decl) = tsubst (TREE_TYPE (code_pattern), args,
				   tf_error, DECL_TI_TEMPLATE (decl));
    }
  else
    gcc_unreachable ();

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
      gcc_assert (!VAR_P (decl)
		  || DECL_IN_AGGR_P (DECL_TEMPLATE_RESULT (tmpl)));

      /* Fetch the more general template.  */
      tmpl = DECL_TI_TEMPLATE (tmpl);
    }

  return tmpl;
}

/* Returns true if we need to instantiate this template instance even if we
   know we aren't going to emit it..  */

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
   instantiate it now, modifying TREE_TYPE (fn).  */

void
maybe_instantiate_noexcept (tree fn)
{
  tree fntype, spec, noex, clone;

  /* Don't instantiate a noexcept-specification from template context.  */
  if (processing_template_decl)
    return;

  if (DECL_CLONED_FUNCTION_P (fn))
    fn = DECL_CLONED_FUNCTION (fn);
  fntype = TREE_TYPE (fn);
  spec = TYPE_RAISES_EXCEPTIONS (fntype);

  if (!DEFERRED_NOEXCEPT_SPEC_P (spec))
    return;

  noex = TREE_PURPOSE (spec);

  if (TREE_CODE (noex) == DEFERRED_NOEXCEPT)
    {
      if (push_tinst_level (fn))
	{
	  push_access_scope (fn);
	  push_deferring_access_checks (dk_no_deferred);
	  input_location = DECL_SOURCE_LOCATION (fn);
	  noex = tsubst_copy_and_build (DEFERRED_NOEXCEPT_PATTERN (noex),
					DEFERRED_NOEXCEPT_ARGS (noex),
					tf_warning_or_error, fn,
					/*function_p=*/false,
					/*integral_constant_expression_p=*/true);
	  pop_deferring_access_checks ();
	  pop_access_scope (fn);
	  pop_tinst_level ();
	  spec = build_noexcept_spec (noex, tf_warning_or_error);
	  if (spec == error_mark_node)
	    spec = noexcept_false_spec;
	}
      else
	spec = noexcept_false_spec;
    }
  else
    {
      /* This is an implicitly declared function, so NOEX is a list of
	 other functions to evaluate and merge.  */
      tree elt;
      spec = noexcept_true_spec;
      for (elt = noex; elt; elt = OVL_NEXT (elt))
	{
	  tree fn = OVL_CURRENT (elt);
	  tree subspec;
	  maybe_instantiate_noexcept (fn);
	  subspec = TYPE_RAISES_EXCEPTIONS (TREE_TYPE (fn));
	  spec = merge_exception_specifiers (spec, subspec, NULL_TREE);
	}
    }

  TREE_TYPE (fn) = build_exception_variant (fntype, spec);

  FOR_EACH_CLONE (clone, fn)
    {
      if (TREE_TYPE (clone) == fntype)
	TREE_TYPE (clone) = TREE_TYPE (fn);
      else
	TREE_TYPE (clone) = build_exception_variant (TREE_TYPE (clone), spec);
    }
}

/* Produce the definition of D, a _DECL generated from a template.  If
   DEFER_OK is nonzero, then we don't have to actually do the
   instantiation now; we just have to do it sometime.  Normally it is
   an error if this is an explicit instantiation but D is undefined.
   EXPL_INST_CLASS_MEM_P is true iff D is a member of an
   explicitly instantiated class template.  */

tree
instantiate_decl (tree d, int defer_ok,
		  bool expl_inst_class_mem_p)
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
  tree fn_context;
  bool nested;

  /* This function should only be used to instantiate templates for
     functions and static member variables.  */
  gcc_assert (VAR_OR_FUNCTION_DECL_P (d));

  /* Variables are never deferred; if instantiation is required, they
     are instantiated right away.  That allows for better code in the
     case that an expression refers to the value of the variable --
     if the variable has a constant value the referring expression can
     take advantage of that fact.  */
  if (VAR_P (d)
      || DECL_DECLARED_CONSTEXPR_P (d))
    defer_ok = 0;

  /* Don't instantiate cloned functions.  Instead, instantiate the
     functions they cloned.  */
  if (TREE_CODE (d) == FUNCTION_DECL && DECL_CLONED_FUNCTION_P (d))
    d = DECL_CLONED_FUNCTION (d);

  if (DECL_TEMPLATE_INSTANTIATED (d)
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

  if (tmpl != gen_tmpl)
    /* We should already have the extra args.  */
    gcc_assert (TMPL_PARMS_DEPTH (DECL_TEMPLATE_PARMS (gen_tmpl))
		== TMPL_ARGS_DEPTH (gen_args));
  /* And what's in the hash table should match D.  */
  gcc_assert ((spec = retrieve_specialization (gen_tmpl, gen_args, 0)) == d
	      || spec == NULL_TREE);

  /* This needs to happen before any tsubsting.  */
  if (! push_tinst_level (d))
    return d;

  timevar_push (TV_TEMPLATE_INST);

  /* Set TD to the template whose DECL_TEMPLATE_RESULT is the pattern
     for the instantiation.  */
  td = template_for_substitution (d);
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
      (gen_args, TMPL_PARMS_DEPTH  (DECL_TEMPLATE_PARMS (td)));
  else
    args = gen_args;

  if (TREE_CODE (d) == FUNCTION_DECL)
    pattern_defined = (DECL_SAVED_TREE (code_pattern) != NULL_TREE
		       || DECL_DEFAULTED_OUTSIDE_CLASS_P (code_pattern));
  else
    pattern_defined = ! DECL_IN_AGGR_P (code_pattern);

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

  if (TREE_CODE (d) == FUNCTION_DECL)
    maybe_instantiate_noexcept (d);

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
      || (external_p && VAR_P (d)))
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

	  ns = decl_namespace_context (d);
	  push_nested_namespace (ns);
	  push_nested_class (DECL_CONTEXT (d));
	  init = tsubst_expr (DECL_INITIAL (code_pattern),
			      args,
			      tf_warning_or_error, NULL_TREE,
			      /*integral_constant_expression_p=*/false);
	  /* Make sure the initializer is still constant, in case of
	     circular dependency (template/instantiate6.C). */
	  const_init
	    = DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (code_pattern);
	  cp_finish_decl (d, init, /*init_const_expr_p=*/const_init,
			  /*asmspec_tree=*/NULL_TREE,
			  LOOKUP_ONLYCONVERTING);
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
      if (cp_unevaluated_operand != 0)
	goto out;
      /* ??? Historically, we have instantiated inline functions, even
	 when marked as "extern template".  */
      if (!(external_p && VAR_P (d)))
	add_pending_template (d);
      goto out;
    }
  /* Tell the repository that D is available in this translation unit
     -- and see if it is supposed to be instantiated here.  */
  if (TREE_PUBLIC (d) && !DECL_REALLY_EXTERN (d) && !repo_emit_p (d))
    {
      /* In a PCH file, despite the fact that the repository hasn't
	 requested instantiation in the PCH it is still possible that
	 an instantiation will be required in a file that includes the
	 PCH.  */
      if (pch_file)
	add_pending_template (d);
      /* Instantiate inline functions so that the inliner can do its
	 job, even though we'll not be emitting a copy of this
	 function.  */
      if (!(TREE_CODE (d) == FUNCTION_DECL && possibly_inlined_p (d)))
	goto out;
    }

  fn_context = decl_function_context (d);
  nested = (current_function_decl != NULL_TREE);
  if (!fn_context)
    push_to_top_level ();
  else
    {
      if (nested)
	push_function_context ();
      cp_unevaluated_operand = 0;
      c_inhibit_evaluation_warnings = 0;
    }

  /* Mark D as instantiated so that recursive calls to
     instantiate_decl do not try to instantiate it again.  */
  DECL_TEMPLATE_INSTANTIATED (d) = 1;

  /* Regenerate the declaration in case the template has been modified
     by a subsequent redeclaration.  */
  regenerate_decl_from_template (d, td);

  /* We already set the file and line above.  Reset them now in case
     they changed as a result of calling regenerate_decl_from_template.  */
  input_location = DECL_SOURCE_LOCATION (d);

  if (VAR_P (d))
    {
      tree init;
      bool const_init = false;

      /* Clear out DECL_RTL; whatever was there before may not be right
	 since we've reset the type of the declaration.  */
      SET_DECL_RTL (d, NULL);
      DECL_IN_AGGR_P (d) = 0;

      /* The initializer is placed in DECL_INITIAL by
	 regenerate_decl_from_template so we don't need to
	 push/pop_access_scope again here.  Pull it out so that
	 cp_finish_decl can process it.  */
      init = DECL_INITIAL (d);
      DECL_INITIAL (d) = NULL_TREE;
      DECL_INITIALIZED_P (d) = 0;

      /* Clear DECL_EXTERNAL so that cp_finish_decl will process the
	 initializer.  That function will defer actual emission until
	 we have a chance to determine linkage.  */
      DECL_EXTERNAL (d) = 0;

      /* Enter the scope of D so that access-checking works correctly.  */
      push_nested_class (DECL_CONTEXT (d));
      const_init = DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (code_pattern);
      cp_finish_decl (d, init, const_init, NULL_TREE, 0);
      pop_nested_class ();
    }
  else if (TREE_CODE (d) == FUNCTION_DECL && DECL_DEFAULTED_FN (code_pattern))
    synthesize_method (d);
  else if (TREE_CODE (d) == FUNCTION_DECL)
    {
      struct pointer_map_t *saved_local_specializations;
      tree subst_decl;
      tree tmpl_parm;
      tree spec_parm;
      tree block = NULL_TREE;

      /* Save away the current list, in case we are instantiating one
	 template from within the body of another.  */
      saved_local_specializations = local_specializations;

      /* Set up the list of local specializations.  */
      local_specializations = pointer_map_create ();

      /* Set up context.  */
      if (DECL_OMP_DECLARE_REDUCTION_P (code_pattern)
	  && TREE_CODE (DECL_CONTEXT (code_pattern)) == FUNCTION_DECL)
	block = push_stmt_list ();
      else
	start_preparsed_function (d, NULL_TREE, SF_PRE_PARSED);

      /* Some typedefs referenced from within the template code need to be
	 access checked at template instantiation time, i.e now. These
	 types were added to the template at parsing time. Let's get those
	 and perform the access checks then.  */
      perform_typedefs_access_check (DECL_TEMPLATE_RESULT (gen_tmpl),
				     gen_args);

      /* Create substitution entries for the parameters.  */
      subst_decl = DECL_TEMPLATE_RESULT (template_for_substitution (d));
      tmpl_parm = DECL_ARGUMENTS (subst_decl);
      spec_parm = DECL_ARGUMENTS (d);
      if (DECL_NONSTATIC_MEMBER_FUNCTION_P (d))
	{
	  register_local_specialization (spec_parm, tmpl_parm);
	  spec_parm = skip_artificial_parms_for (d, spec_parm);
	  tmpl_parm = skip_artificial_parms_for (subst_decl, tmpl_parm);
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

      /* Substitute into the body of the function.  */
      if (DECL_OMP_DECLARE_REDUCTION_P (code_pattern))
	tsubst_omp_udr (DECL_SAVED_TREE (code_pattern), args,
			tf_warning_or_error, tmpl);
      else
	{
	  tsubst_expr (DECL_SAVED_TREE (code_pattern), args,
		       tf_warning_or_error, tmpl,
		       /*integral_constant_expression_p=*/false);

	  /* Set the current input_location to the end of the function
	     so that finish_function knows where we are.  */
	  input_location
	    = DECL_STRUCT_FUNCTION (code_pattern)->function_end_locus;

	  /* Remember if we saw an infinite loop in the template.  */
	  current_function_infinite_loop
	    = DECL_STRUCT_FUNCTION (code_pattern)->language->infinite_loop;
	}

      /* We don't need the local specializations any more.  */
      pointer_map_destroy (local_specializations);
      local_specializations = saved_local_specializations;

      /* Finish the function.  */
      if (DECL_OMP_DECLARE_REDUCTION_P (code_pattern)
	  && TREE_CODE (DECL_CONTEXT (code_pattern)) == FUNCTION_DECL)
	DECL_SAVED_TREE (d) = pop_stmt_list (block);
      else
	{
	  d = finish_function (0);
	  expand_or_defer_fn (d);
	}

      if (DECL_OMP_DECLARE_REDUCTION_P (code_pattern))
	cp_check_omp_declare_reduction (d);
    }

  /* We're not deferring instantiation any more.  */
  TI_PENDING_TEMPLATE_FLAG (DECL_TEMPLATE_INFO (d)) = 0;

  if (!fn_context)
    pop_from_top_level ();
  else if (nested)
    pop_function_context ();

out:
  input_location = saved_loc;
  cp_unevaluated_operand = saved_unevaluated_operand;
  c_inhibit_evaluation_warnings = saved_inhibit_evaluation_warnings;
  pop_deferring_access_checks ();
  pop_tinst_level ();

  timevar_pop (TV_TEMPLATE_INST);

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
      tree decl = pending_templates->tinst->decl;

      error ("template instantiation depth exceeds maximum of %d"
	     " instantiating %q+D, possibly from virtual table generation"
	     " (use -ftemplate-depth= to increase the maximum)",
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
	      tree fn;

	      if (!COMPLETE_TYPE_P (instantiation))
		{
		  instantiate_class_template (instantiation);
		  if (CLASSTYPE_TEMPLATE_INSTANTIATION (instantiation))
		    for (fn = TYPE_METHODS (instantiation);
			 fn;
			 fn = TREE_CHAIN (fn))
		      if (! DECL_ARTIFICIAL (fn))
			instantiate_decl (fn,
					  /*defer_ok=*/0,
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
					/*defer_ok=*/0,
					/*expl_inst_class_mem_p=*/false);
		  if (DECL_TEMPLATE_INSTANTIATED (instantiation))
		    reconsider = 1;
		}

	      complete = (DECL_TEMPLATE_SPECIALIZATION (instantiation)
			  || DECL_TEMPLATE_INSTANTIATED (instantiation));
	    }

	  if (complete)
	    /* If INSTANTIATION has been instantiated, then we don't
	       need to consider it again in the future.  */
	    *t = (*t)->next;
	  else
	    {
	      last = *t;
	      t = &(*t)->next;
	    }
	  tinst_depth = 0;
	  current_tinst_level = NULL;
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
		  SET_PACK_EXPANSION_PATTERN (expr, TREE_VALUE (arg));
		  expanded_exprs 
		    = tsubst_pack_expansion (expr, argvec,
					     tf_warning_or_error,
					     NULL_TREE);
		  if (expanded_exprs == error_mark_node)
		    continue;

		  /* Prepend each of the expanded expressions to the
		     corresponding TREE_LIST in EXPANDED_ARGUMENTS.  */
		  for (i = 0; i < len; i++)
		    {
		      TREE_VEC_ELT (expanded_arguments, i) = 
			tree_cons (NULL_TREE, 
				   TREE_VEC_ELT (expanded_exprs, i),
				   TREE_VEC_ELT (expanded_arguments, i));
		    }
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
				    tf_warning_or_error, NULL_TREE,
				    /*integral_constant_expression_p=*/false);
	      if (init == NULL_TREE && tmp != NULL_TREE)
		/* If we had an initializer but it instantiated to nothing,
		   value-initialize the object.  This will only occur when
		   the initializer was a pack expansion where the parameter
		   packs used in that expansion were of length zero.  */
		init = void_type_node;
              in_base_initializer = 0;
            }

          if (decl)
            {
              init = build_tree_list (decl, init);
              TREE_CHAIN (init) = inits;
              inits = init;
            }
        }
    }
  return inits;
}

/* Set CURRENT_ACCESS_SPECIFIER based on the protection of DECL.  */

static void
set_current_access_from_decl (tree decl)
{
  if (TREE_PRIVATE (decl))
    current_access_specifier = access_private_node;
  else if (TREE_PROTECTED (decl))
    current_access_specifier = access_protected_node;
  else
    current_access_specifier = access_public_node;
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
      tree decl;

      decl = TREE_VALUE (e);
      /* Note that in a template enum, the TREE_VALUE is the
	 CONST_DECL, not the corresponding INTEGER_CST.  */
      value = tsubst_expr (DECL_INITIAL (decl),
			   args, tf_warning_or_error, NULL_TREE,
			   /*integral_constant_expression_p=*/true);

      /* Give this enumeration constant the correct access.  */
      set_current_access_from_decl (decl);

      /* Actually build the enumerator itself.  */
      build_enumerator
	(DECL_NAME (decl), value, newtag, DECL_SOURCE_LOCATION (decl));
    }

  if (SCOPED_ENUM_P (newtag))
    finish_scope ();

  finish_enum_value_list (newtag);
  finish_enum (newtag);

  DECL_SOURCE_LOCATION (TYPE_NAME (newtag))
    = DECL_SOURCE_LOCATION (TYPE_NAME (tag));
}

/* DECL is a FUNCTION_DECL that is a template specialization.  Return
   its type -- but without substituting the innermost set of template
   arguments.  So, innermost set of template parameters will appear in
   the type.  */

tree
get_mostly_instantiated_function_type (tree decl)
{
  tree fn_type;
  tree tmpl;
  tree targs;
  tree tparms;
  int parm_depth;

  tmpl = most_general_template (DECL_TI_TEMPLATE (decl));
  targs = DECL_TI_ARGS (decl);
  tparms = DECL_TEMPLATE_PARMS (tmpl);
  parm_depth = TMPL_PARMS_DEPTH (tparms);

  /* There should be as many levels of arguments as there are levels
     of parameters.  */
  gcc_assert (parm_depth == TMPL_ARGS_DEPTH (targs));

  fn_type = TREE_TYPE (tmpl);

  if (parm_depth == 1)
    /* No substitution is necessary.  */
    ;
  else
    {
      int i;
      tree partial_args;

      /* Replace the innermost level of the TARGS with NULL_TREEs to
	 let tsubst know not to substitute for those parameters.  */
      partial_args = make_tree_vec (TREE_VEC_LENGTH (targs));
      for (i = 1; i < TMPL_ARGS_DEPTH (targs); ++i)
	SET_TMPL_ARGS_LEVEL (partial_args, i,
			     TMPL_ARGS_LEVEL (targs, i));
      SET_TMPL_ARGS_LEVEL (partial_args,
			   TMPL_ARGS_DEPTH (targs),
			   make_tree_vec (DECL_NTPARMS (tmpl)));

      /* Make sure that we can see identifiers, and compute access
	 correctly.  */
      push_access_scope (decl);

      ++processing_template_decl;
      /* Now, do the (partial) substitution to figure out the
	 appropriate function type.  */
      fn_type = tsubst (fn_type, partial_args, tf_error, NULL_TREE);
      --processing_template_decl;

      /* Substitute into the template parameters to obtain the real
	 innermost set of parameters.  This step is important if the
	 innermost set of template parameters contains value
	 parameters whose types depend on outer template parameters.  */
      TREE_VEC_LENGTH (partial_args)--;
      tparms = tsubst_template_parms (tparms, partial_args, tf_error);

      pop_access_scope (decl);
    }

  return fn_type;
}

/* Return truthvalue if we're processing a template different from
   the last one involved in diagnostics.  */
int
problematic_instantiation_changed (void)
{
  return current_tinst_level != last_error_tinst_level;
}

/* Remember current template involved in diagnostics.  */
void
record_last_problematic_instantiation (void)
{
  last_error_tinst_level = current_tinst_level;
}

struct tinst_level *
current_instantiation (void)
{
  return current_tinst_level;
}

/* [temp.param] Check that template non-type parm TYPE is of an allowable
   type. Return zero for ok, nonzero for disallowed. Issue error and
   warning messages under control of COMPLAIN.  */

static int
invalid_nontype_parm_type_p (tree type, tsubst_flags_t complain)
{
  if (INTEGRAL_OR_ENUMERATION_TYPE_P (type))
    return 0;
  else if (POINTER_TYPE_P (type))
    return 0;
  else if (TYPE_PTRMEM_P (type))
    return 0;
  else if (TREE_CODE (type) == TEMPLATE_TYPE_PARM)
    return 0;
  else if (TREE_CODE (type) == TYPENAME_TYPE)
    return 0;
  else if (TREE_CODE (type) == DECLTYPE_TYPE)
    return 0;
  else if (TREE_CODE (type) == NULLPTR_TYPE)
    return 0;

  if (complain & tf_error)
    {
      if (type == error_mark_node)
	inform (input_location, "invalid template non-type parameter");
      else
	error ("%q#T is not a valid type for a template non-type parameter",
	       type);
    }
  return 1;
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
  /* -- a cv-qualified type where the cv-unqualified type is
	dependent.  */
  type = TYPE_MAIN_VARIANT (type);
  /* -- a compound type constructed from any dependent type.  */
  if (TYPE_PTRMEM_P (type))
    return (dependent_type_p (TYPE_PTRMEM_CLASS_TYPE (type))
	    || dependent_type_p (TYPE_PTRMEM_POINTED_TO_TYPE
					   (type)));
  else if (TYPE_PTR_P (type)
	   || TREE_CODE (type) == REFERENCE_TYPE)
    return dependent_type_p (TREE_TYPE (type));
  else if (TREE_CODE (type) == FUNCTION_TYPE
	   || TREE_CODE (type) == METHOD_TYPE)
    {
      tree arg_type;

      if (dependent_type_p (TREE_TYPE (type)))
	return true;
      for (arg_type = TYPE_ARG_TYPES (type);
	   arg_type;
	   arg_type = TREE_CHAIN (arg_type))
	if (dependent_type_p (TREE_VALUE (arg_type)))
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

  /* All TYPEOF_TYPEs, DECLTYPE_TYPEs, and UNDERLYING_TYPEs are
     dependent; if the argument of the `typeof' expression is not
     type-dependent, then it should already been have resolved.  */
  if (TREE_CODE (type) == TYPEOF_TYPE
      || TREE_CODE (type) == DECLTYPE_TYPE
      || TREE_CODE (type) == UNDERLYING_TYPE)
    return true;

  /* A template argument pack is dependent if any of its packed
     arguments are.  */
  if (TREE_CODE (type) == TYPE_ARGUMENT_PACK)
    {
      tree args = ARGUMENT_PACK_ARGS (type);
      int i, len = TREE_VEC_LENGTH (args);
      for (i = 0; i < len; ++i)
        if (dependent_template_arg_p (TREE_VEC_ELT (args, i)))
          return true;
    }

  /* All TYPE_PACK_EXPANSIONs are dependent, because parameter packs must
     be template parameters.  */
  if (TREE_CODE (type) == TYPE_PACK_EXPANSION)
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

/* T is a SCOPE_REF; return whether we need to consider it
    instantiation-dependent so that we can check access at instantiation
    time even though we know which member it resolves to.  */

static bool
instantiation_dependent_scope_ref_p (tree t)
{
  if (DECL_P (TREE_OPERAND (t, 1))
      && CLASS_TYPE_P (TREE_OPERAND (t, 0))
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
  if (!processing_template_decl)
    return false;

  /* A name declared with a dependent type.  */
  if (DECL_P (expression) && type_dependent_expression_p (expression))
    return true;

  switch (TREE_CODE (expression))
    {
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
	  with an expression that is value-dependent.

          Note that a non-dependent parenthesized initializer will have
          already been replaced with its constant value, so if we see
          a TREE_LIST it must be dependent.  */
      if (DECL_INITIAL (expression)
	  && decl_constant_var_p (expression)
	  && (TREE_CODE (DECL_INITIAL (expression)) == TREE_LIST
	      || value_dependent_expression_p (DECL_INITIAL (expression))))
	return true;
      return false;

    case DYNAMIC_CAST_EXPR:
    case STATIC_CAST_EXPR:
    case CONST_CAST_EXPR:
    case REINTERPRET_CAST_EXPR:
    case CAST_EXPR:
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
      return instantiation_dependent_expression_p (expression);

    case AT_ENCODE_EXPR:
      /* An 'encode' expression is value-dependent if the operand is
	 type-dependent.  */
      expression = TREE_OPERAND (expression, 0);
      return dependent_type_p (expression);

    case NOEXCEPT_EXPR:
      expression = TREE_OPERAND (expression, 0);
      return instantiation_dependent_expression_p (expression);

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
      {
        tree values = ARGUMENT_PACK_ARGS (expression);
        int i, len = TREE_VEC_LENGTH (values);
        
        for (i = 0; i < len; ++i)
          if (value_dependent_expression_p (TREE_VEC_ELT (values, i)))
            return true;
        
        return false;
      }

    case TRAIT_EXPR:
      {
	tree type2 = TRAIT_EXPR_TYPE2 (expression);
	return (dependent_type_p (TRAIT_EXPR_TYPE1 (expression))
		|| (type2 ? dependent_type_p (type2) : false));
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

    case CALL_EXPR:
      {
	tree fn = get_callee_fndecl (expression);
	int i, nargs;
	if (!fn && value_dependent_expression_p (CALL_EXPR_FN (expression)))
	  return true;
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
      /* If a TEMPLATE_ID_EXPR involves a dependent name, it will be
	 type-dependent.  */
      return type_dependent_expression_p (expression);

    case CONSTRUCTOR:
      {
	unsigned ix;
	tree val;
	FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (expression), ix, val)
	  if (value_dependent_expression_p (val))
	    return true;
	return false;
      }

    case STMT_EXPR:
      /* Treat a GNU statement expression as dependent to avoid crashing
	 under fold_non_dependent_expr; it can't be constant.  */
      return true;

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

		/* In some cases, some of the operands may be missing.l
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

  /* An unresolved name is always dependent.  */
  if (identifier_p (expression) || TREE_CODE (expression) == USING_DECL)
    return true;

  /* Some expression forms are never type-dependent.  */
  if (TREE_CODE (expression) == PSEUDO_DTOR_EXPR
      || TREE_CODE (expression) == SIZEOF_EXPR
      || TREE_CODE (expression) == ALIGNOF_EXPR
      || TREE_CODE (expression) == AT_ENCODE_EXPR
      || TREE_CODE (expression) == NOEXCEPT_EXPR
      || TREE_CODE (expression) == TRAIT_EXPR
      || TREE_CODE (expression) == TYPEID_EXPR
      || TREE_CODE (expression) == DELETE_EXPR
      || TREE_CODE (expression) == VEC_DELETE_EXPR
      || TREE_CODE (expression) == THROW_EXPR)
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

  if (TREE_CODE (expression) == FUNCTION_DECL
      && DECL_LANG_SPECIFIC (expression)
      && DECL_TEMPLATE_INFO (expression)
      && (any_dependent_template_arguments_p
	  (INNERMOST_TEMPLATE_ARGS (DECL_TI_ARGS (expression)))))
    return true;

  if (TREE_CODE (expression) == TEMPLATE_DECL
      && !DECL_TEMPLATE_TEMPLATE_PARM_P (expression))
    return false;

  if (TREE_CODE (expression) == STMT_EXPR)
    expression = stmt_expr_value_expr (expression);

  if (BRACE_ENCLOSED_INITIALIZER_P (expression))
    {
      tree elt;
      unsigned i;

      FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (expression), i, elt)
	{
	  if (type_dependent_expression_p (elt))
	    return true;
	}
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
  if (VAR_P (expression)
      && TREE_CODE (TREE_TYPE (expression)) == ARRAY_TYPE
      && !TYPE_DOMAIN (TREE_TYPE (expression))
      && DECL_INITIAL (expression))
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

      /* Always dependent, on the number of arguments if nothing else.  */
      if (TREE_CODE (expression) == EXPR_PACK_EXPANSION)
	return true;

      if (BASELINK_P (expression))
	expression = BASELINK_FUNCTIONS (expression);

      if (TREE_CODE (expression) == TEMPLATE_ID_EXPR)
	{
	  if (any_dependent_template_arguments_p
	      (TREE_OPERAND (expression, 1)))
	    return true;
	  expression = TREE_OPERAND (expression, 0);
	}
      gcc_assert (TREE_CODE (expression) == OVERLOAD
		  || TREE_CODE (expression) == FUNCTION_DECL);

      while (expression)
	{
	  if (type_dependent_expression_p (OVL_CURRENT (expression)))
	    return true;
	  expression = OVL_NEXT (expression);
	}
      return false;
    }

  gcc_assert (TREE_CODE (expression) != TYPE_DECL);

  return (dependent_type_p (TREE_TYPE (expression)));
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
      return NULL_TREE;

    case VAR_DECL:
    case CONST_DECL:
      /* A constant with a dependent initializer is dependent.  */
      if (value_dependent_expression_p (*tp))
	return *tp;
      break;

    case TEMPLATE_PARM_INDEX:
      return *tp;

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

    case TRAIT_EXPR:
      if (dependent_type_p (TRAIT_EXPR_TYPE1 (*tp))
	  || (TRAIT_EXPR_TYPE2 (*tp)
	      && dependent_type_p (TRAIT_EXPR_TYPE2 (*tp))))
	return *tp;
      *walk_subtrees = false;
      return NULL_TREE;

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
   or value-dependent."  */

bool
instantiation_dependent_expression_p (tree expression)
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
  unsigned int i;
  tree arg;

  FOR_EACH_VEC_SAFE_ELT (args, i, arg)
    {
      if (type_dependent_expression_p (arg))
	return true;
    }
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
    arg = ARGUMENT_PACK_SELECT_ARG (arg);

  if (TREE_CODE (arg) == TEMPLATE_DECL
      || TREE_CODE (arg) == TEMPLATE_TEMPLATE_PARM)
    return dependent_template_p (arg);
  else if (ARGUMENT_PACK_P (arg))
    {
      tree args = ARGUMENT_PACK_ARGS (arg);
      int i, len = TREE_VEC_LENGTH (args);
      for (i = 0; i < len; ++i)
        {
          if (dependent_template_arg_p (TREE_VEC_ELT (args, i)))
            return true;
        }

      return false;
    }
  else if (TYPE_P (arg))
    return dependent_type_p (arg);
  else
    return (type_dependent_expression_p (arg)
	    || value_dependent_expression_p (arg));
}

/* Returns true if ARGS (a collection of template arguments) contains
   any types that require structural equality testing.  */

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
	      else if (TYPE_P (arg) && TYPE_STRUCTURAL_EQUALITY_P (arg))
		return true;
	      else if (!TYPE_P (arg) && TREE_TYPE (arg)
		       && TYPE_STRUCTURAL_EQUALITY_P (TREE_TYPE (arg)))
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
  int i;
  int j;

  if (!args)
    return false;
  if (args == error_mark_node)
    return true;

  for (i = 0; i < TMPL_ARGS_DEPTH (args); ++i)
    {
      const_tree level = TMPL_ARGS_LEVEL (args, i + 1);
      for (j = 0; j < TREE_VEC_LENGTH (level); ++j)
	if (dependent_template_arg_p (TREE_VEC_ELT (level, j)))
	  return true;
    }

  return false;
}

/* Returns TRUE if the template TMPL is dependent.  */

bool
dependent_template_p (tree tmpl)
{
  if (TREE_CODE (tmpl) == OVERLOAD)
    {
      while (tmpl)
	{
	  if (dependent_template_p (OVL_CURRENT (tmpl)))
	    return true;
	  tmpl = OVL_NEXT (tmpl);
	}
      return false;
    }

  /* Template template parameters are dependent.  */
  if (DECL_TEMPLATE_TEMPLATE_PARM_P (tmpl)
      || TREE_CODE (tmpl) == TEMPLATE_TEMPLATE_PARM)
    return true;
  /* So are names that have not been looked up.  */
  if (TREE_CODE (tmpl) == SCOPE_REF || identifier_p (tmpl))
    return true;
  /* So are member templates of dependent classes.  */
  if (TYPE_P (CP_DECL_CONTEXT (tmpl)))
    return dependent_type_p (DECL_CONTEXT (tmpl));
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
   is dependent.  */

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

      if (type_dependent_expression_p (decl))
	return true;

      if (init && type_dependent_expression_p (init))
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
  /* Usually the non-qualified identifier of a TYPENAME_TYPE is
     TYPE_IDENTIFIER (type). But when 'type' is a typedef variant of
     a TYPENAME_TYPE node, then TYPE_NAME (type) is set to the TYPE_DECL representing
     the typedef. In that case TYPE_IDENTIFIER (type) is not the non-qualified
     identifier  of the TYPENAME_TYPE anymore.
     So by getting the TYPE_IDENTIFIER of the _main declaration_ of the
     TYPENAME_TYPE instead, we avoid messing up with a possible
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
  if (TREE_CODE (scope) == TYPENAME_TYPE)
    return type;
  /* If the SCOPE is a template type parameter, we have no way of
     resolving the name.  */
  if (TREE_CODE (scope) == TEMPLATE_TYPE_PARM)
    return type;
  /* If the SCOPE is not the current instantiation, there's no reason
     to look inside it.  */
  if (only_current_p && !currently_open_class (scope))
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
  else
    /* scope is a partial instantiation, so we can't do the lookup or we
       will lose the template arguments.  */
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
  if (!decl)
    /*nop*/;
  else if (identifier_p (TYPENAME_TYPE_FULLNAME (type))
	   && TREE_CODE (decl) == TYPE_DECL)
    {
      result = TREE_TYPE (decl);
      if (result == error_mark_node)
	result = NULL_TREE;
    }
  else if (TREE_CODE (TYPENAME_TYPE_FULLNAME (type)) == TEMPLATE_ID_EXPR
	   && DECL_CLASS_TEMPLATE_P (decl))
    {
      tree tmpl;
      tree args;
      /* Obtain the template and the arguments.  */
      tmpl = TREE_OPERAND (TYPENAME_TYPE_FULLNAME (type), 0);
      args = TREE_OPERAND (TYPENAME_TYPE_FULLNAME (type), 1);
      /* Instantiate the template.  */
      result = lookup_template_class (tmpl, args, NULL_TREE, NULL_TREE,
				      /*entering_scope=*/0,
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
      TYPENAME_IS_RESOLVING_P (type) = 1;
      result = resolve_typename_type (result, only_current_p);
      TYPENAME_IS_RESOLVING_P (type) = 0;
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
  tree inner_expr;

#ifdef ENABLE_CHECKING
  /* Try to get a constant value for all non-dependent expressions in
      order to expose bugs in *_dependent_expression_p and constexpr.  */
  if (cxx_dialect >= cxx11)
    maybe_constant_value (fold_non_dependent_expr_sfinae (expr, tf_none));
#endif

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
    return expr;
  /* There is no need to return a proxy for a variable.  */
  if (VAR_P (expr))
    return expr;
  /* Preserve string constants; conversions from string constants to
     "char *" are allowed, even though normally a "const char *"
     cannot be used to initialize a "char *".  */
  if (TREE_CODE (expr) == STRING_CST)
    return expr;
  /* Preserve arithmetic constants, as an optimization -- there is no
     reason to create a new node.  */
  if (TREE_CODE (expr) == INTEGER_CST || TREE_CODE (expr) == REAL_CST)
    return expr;
  /* Preserve THROW_EXPRs -- all throw-expressions have type "void".
     There is at least one place where we want to know that a
     particular expression is a throw-expression: when checking a ?:
     expression, there are special rules if the second or third
     argument is a throw-expression.  */
  if (TREE_CODE (expr) == THROW_EXPR)
    return expr;

  /* Don't wrap an initializer list, we need to be able to look inside.  */
  if (BRACE_ENCLOSED_INITIALIZER_P (expr))
    return expr;

  /* Don't wrap a dummy object, we need to be able to test for it.  */
  if (is_dummy_object (expr))
    return expr;

  if (TREE_CODE (expr) == COND_EXPR)
    return build3 (COND_EXPR,
		   TREE_TYPE (expr),
		   TREE_OPERAND (expr, 0),
		   (TREE_OPERAND (expr, 1)
		    ? build_non_dependent_expr (TREE_OPERAND (expr, 1))
		    : build_non_dependent_expr (TREE_OPERAND (expr, 0))),
		   build_non_dependent_expr (TREE_OPERAND (expr, 2)));
  if (TREE_CODE (expr) == COMPOUND_EXPR
      && !COMPOUND_EXPR_OVERLOADED (expr))
    return build2 (COMPOUND_EXPR,
		   TREE_TYPE (expr),
		   TREE_OPERAND (expr, 0),
		   build_non_dependent_expr (TREE_OPERAND (expr, 1)));

  /* If the type is unknown, it can't really be non-dependent */
  gcc_assert (TREE_TYPE (expr) != unknown_type_node);

  /* Otherwise, build a NON_DEPENDENT_EXPR.  */
  return build1 (NON_DEPENDENT_EXPR, TREE_TYPE (expr), expr);
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
   TEMPLATE_TYPE_PARM with a level one deeper than the actual template
   parms.  */

static tree
make_auto_1 (tree name)
{
  tree au = cxx_make_type (TEMPLATE_TYPE_PARM);
  TYPE_NAME (au) = build_decl (input_location,
			       TYPE_DECL, name, au);
  TYPE_STUB_DECL (au) = TYPE_NAME (au);
  TEMPLATE_TYPE_PARM_INDEX (au) = build_template_parm_index
    (0, processing_template_decl + 1, processing_template_decl + 1,
     TYPE_NAME (au), NULL_TREE);
  TYPE_CANONICAL (au) = canonical_type_parameter (au);
  DECL_ARTIFICIAL (TYPE_NAME (au)) = 1;
  SET_DECL_TEMPLATE_PARM_P (TYPE_NAME (au));

  return au;
}

tree
make_decltype_auto (void)
{
  return make_auto_1 (get_identifier ("decltype(auto)"));
}

tree
make_auto (void)
{
  return make_auto_1 (get_identifier ("auto"));
}

/* Given type ARG, return std::initializer_list<ARG>.  */

static tree
listify (tree arg)
{
  tree std_init_list = namespace_binding
    (get_identifier ("initializer_list"), std_node);
  tree argvec;
  if (!std_init_list || !DECL_CLASS_TEMPLATE_P (std_init_list))
    {    
      error ("deducing from brace-enclosed initializer list requires "
	     "#include <initializer_list>");
      return error_mark_node;
    }
  argvec = make_tree_vec (1);
  TREE_VEC_ELT (argvec, 0) = arg;
  return lookup_template_class (std_init_list, argvec, NULL_TREE,
				NULL_TREE, 0, tf_warning_or_error);
}

/* Replace auto in TYPE with std::initializer_list<auto>.  */

static tree
listify_autos (tree type, tree auto_node)
{
  tree init_auto = listify (auto_node);
  tree argvec = make_tree_vec (1);
  TREE_VEC_ELT (argvec, 0) = init_auto;
  if (processing_template_decl)
    argvec = add_to_template_args (current_template_args (), argvec);
  return tsubst (type, argvec, tf_warning_or_error, NULL_TREE);
}

/* Replace occurrences of 'auto' in TYPE with the appropriate type deduced
   from INIT.  AUTO_NODE is the TEMPLATE_TYPE_PARM used for 'auto' in TYPE.  */

tree
do_auto_deduction (tree type, tree init, tree auto_node)
{
  tree targs;

  if (init == error_mark_node)
    return error_mark_node;

  if (type_dependent_expression_p (init))
    /* Defining a subset of type-dependent expressions that we can deduce
       from ahead of time isn't worth the trouble.  */
    return type;

  /* [dcl.spec.auto]: Obtain P from T by replacing the occurrences of auto
     with either a new invented type template parameter U or, if the
     initializer is a braced-init-list (8.5.4), with
     std::initializer_list<U>.  */
  if (BRACE_ENCLOSED_INITIALIZER_P (init))
    type = listify_autos (type, auto_node);

  init = resolve_nondeduced_context (init);

  targs = make_tree_vec (1);
  if (AUTO_IS_DECLTYPE (auto_node))
    {
      bool id = (DECL_P (init) || (TREE_CODE (init) == COMPONENT_REF
				   && !REF_PARENTHESIZED_P (init)));
      TREE_VEC_ELT (targs, 0)
	= finish_decltype_type (init, id, tf_warning_or_error);
      if (type != auto_node)
	{
	  error ("%qT as type rather than plain %<decltype(auto)%>", type);
	  return error_mark_node;
	}
    }
  else
    {
      tree parms = build_tree_list (NULL_TREE, type);
      tree tparms = make_tree_vec (1);
      int val;

      TREE_VEC_ELT (tparms, 0)
	= build_tree_list (NULL_TREE, TYPE_NAME (auto_node));
      val = type_unification_real (tparms, targs, parms, &init, 1, 0,
				   DEDUCE_CALL, LOOKUP_NORMAL,
				   NULL, /*explain_p=*/false);
      if (val > 0)
	{
	  if (processing_template_decl)
	    /* Try again at instantiation time.  */
	    return type;
	  if (type && type != error_mark_node)
	    /* If type is error_mark_node a diagnostic must have been
	       emitted by now.  Also, having a mention to '<type error>'
	       in the diagnostic is not really useful to the user.  */
	    {
	      if (cfun && auto_node == current_function_auto_return_pattern
		  && LAMBDA_FUNCTION_P (current_function_decl))
		error ("unable to deduce lambda return type from %qE", init);
	      else
		error ("unable to deduce %qT from %qE", type, init);
	    }
	  return error_mark_node;
	}
    }

  /* If the list of declarators contains more than one declarator, the type
     of each declared variable is determined as described above. If the
     type deduced for the template parameter U is not the same in each
     deduction, the program is ill-formed.  */
  if (TREE_TYPE (auto_node)
      && !same_type_p (TREE_TYPE (auto_node), TREE_VEC_ELT (targs, 0)))
    {
      if (cfun && auto_node == current_function_auto_return_pattern
	  && LAMBDA_FUNCTION_P (current_function_decl))
	error ("inconsistent types %qT and %qT deduced for "
	       "lambda return type", TREE_TYPE (auto_node),
	       TREE_VEC_ELT (targs, 0));
      else
	error ("inconsistent deduction for %qT: %qT and then %qT",
	       auto_node, TREE_TYPE (auto_node), TREE_VEC_ELT (targs, 0));
      return error_mark_node;
    }
  TREE_TYPE (auto_node) = TREE_VEC_ELT (targs, 0);

  if (processing_template_decl)
    targs = add_to_template_args (current_template_args (), targs);
  return tsubst (type, targs, tf_warning_or_error, NULL_TREE);
}

/* Substitutes LATE_RETURN_TYPE for 'auto' in TYPE and returns the
   result.  */

tree
splice_late_return_type (tree type, tree late_return_type)
{
  tree argvec;

  if (late_return_type == NULL_TREE)
    return type;
  argvec = make_tree_vec (1);
  TREE_VEC_ELT (argvec, 0) = late_return_type;
  if (processing_template_parmlist)
    /* For a late-specified return type in a template type-parameter, we
       need to add a dummy argument level for its parmlist.  */
    argvec = add_to_template_args
      (make_tree_vec (processing_template_parmlist), argvec);
  if (current_template_parms)
    argvec = add_to_template_args (current_template_args (), argvec);
  return tsubst (type, argvec, tf_warning_or_error, NULL_TREE);
}

/* Returns true iff TYPE is a TEMPLATE_TYPE_PARM representing 'auto' or
   'decltype(auto)'.  */

bool
is_auto (const_tree type)
{
  if (TREE_CODE (type) == TEMPLATE_TYPE_PARM
      && (TYPE_IDENTIFIER (type) == get_identifier ("auto")
	  || TYPE_IDENTIFIER (type) == get_identifier ("decltype(auto)")))
    return true;
  else
    return false;
}

/* Returns the TEMPLATE_TYPE_PARM in TYPE representing `auto' iff TYPE contains
   a use of `auto'.  Returns NULL_TREE otherwise.  */

tree
type_uses_auto (tree type)
{
  return find_type_usage (type, is_auto);
}

/* Returns true iff TYPE is a TEMPLATE_TYPE_PARM representing 'auto',
   'decltype(auto)' or a concept.  */

bool
is_auto_or_concept (const_tree type)
{
  return is_auto (type); // or concept
}

/* Returns the TEMPLATE_TYPE_PARM in TYPE representing a generic type (`auto' or
   a concept identifier) iff TYPE contains a use of a generic type.  Returns
   NULL_TREE otherwise.  */

tree
type_uses_auto_or_concept (tree type)
{
  return find_type_usage (type, is_auto_or_concept);
}


/* For a given template T, return the vector of typedefs referenced
   in T for which access check is needed at T instantiation time.
   T is either  a FUNCTION_DECL or a RECORD_TYPE.
   Those typedefs were added to T by the function
   append_type_to_template_for_access_check.  */

vec<qualified_typedef_usage_t, va_gc> *
get_types_needing_access_check (tree t)
{
  tree ti;
  vec<qualified_typedef_usage_t, va_gc> *result = NULL;

  if (!t || t == error_mark_node)
    return NULL;

  if (!(ti = get_template_info (t)))
    return NULL;

  if (CLASS_TYPE_P (t)
      || TREE_CODE (t) == FUNCTION_DECL)
    {
      if (!TI_TEMPLATE (ti))
	return NULL;

      result = TI_TYPEDEFS_NEEDING_ACCESS_CHECKING (ti);
    }

  return result;
}

/* Append the typedef TYPE_DECL used in template T to a list of typedefs
   tied to T. That list of typedefs will be access checked at
   T instantiation time.
   T is either a FUNCTION_DECL or a RECORD_TYPE.
   TYPE_DECL is a TYPE_DECL node representing a typedef.
   SCOPE is the scope through which TYPE_DECL is accessed.
   LOCATION is the location of the usage point of TYPE_DECL.

   This function is a subroutine of
   append_type_to_template_for_access_check.  */

static void
append_type_to_template_for_access_check_1 (tree t,
					    tree type_decl,
					    tree scope,
					    location_t location)
{
  qualified_typedef_usage_t typedef_usage;
  tree ti;

  if (!t || t == error_mark_node)
    return;

  gcc_assert ((TREE_CODE (t) == FUNCTION_DECL
	       || CLASS_TYPE_P (t))
	      && type_decl
	      && TREE_CODE (type_decl) == TYPE_DECL
	      && scope);

  if (!(ti = get_template_info (t)))
    return;

  gcc_assert (TI_TEMPLATE (ti));

  typedef_usage.typedef_decl = type_decl;
  typedef_usage.context = scope;
  typedef_usage.locus = location;

  vec_safe_push (TI_TYPEDEFS_NEEDING_ACCESS_CHECKING (ti), typedef_usage);
}

/* Append TYPE_DECL to the template TEMPL.
   TEMPL is either a class type, a FUNCTION_DECL or a a TEMPLATE_DECL.
   At TEMPL instanciation time, TYPE_DECL will be checked to see
   if it can be accessed through SCOPE.
   LOCATION is the location of the usage point of TYPE_DECL.

   e.g. consider the following code snippet:

     class C
     {
       typedef int myint;
     };

     template<class U> struct S
     {
       C::myint mi; // <-- usage point of the typedef C::myint
     };

     S<char> s;

   At S<char> instantiation time, we need to check the access of C::myint
   In other words, we need to check the access of the myint typedef through
   the C scope. For that purpose, this function will add the myint typedef
   and the scope C through which its being accessed to a list of typedefs
   tied to the template S. That list will be walked at template instantiation
   time and access check performed on each typedefs it contains.
   Note that this particular code snippet should yield an error because
   myint is private to C.  */

void
append_type_to_template_for_access_check (tree templ,
                                          tree type_decl,
					  tree scope,
					  location_t location)
{
  qualified_typedef_usage_t *iter;
  unsigned i;

  gcc_assert (type_decl && (TREE_CODE (type_decl) == TYPE_DECL));

  /* Make sure we don't append the type to the template twice.  */
  FOR_EACH_VEC_SAFE_ELT (get_types_needing_access_check (templ), i, iter)
    if (iter->typedef_decl == type_decl && scope == iter->context)
      return;

  append_type_to_template_for_access_check_1 (templ, type_decl,
					      scope, location);
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

      tree o = TREE_TYPE (TREE_VALUE
			  (TREE_VEC_ELT (current, i)));
      tree t = copy_type (o);
      TEMPLATE_TYPE_PARM_INDEX (t)
	= reduce_template_parm_level (TEMPLATE_TYPE_PARM_INDEX (o),
				      o, 0, 0, tf_none);
      TREE_TYPE (TEMPLATE_TYPE_DECL (t)) = t;
      TYPE_STUB_DECL (t) = TYPE_NAME (t) = TEMPLATE_TYPE_DECL (t);
      TYPE_MAIN_VARIANT (t) = t;
      TEMPLATE_TYPE_PARAMETER_PACK (t) = true;
      TYPE_CANONICAL (t) = canonical_type_parameter (t);
      TREE_VEC_ELT (replacement, i) = t;
      TREE_VALUE (TREE_VEC_ELT (current, i)) = TREE_CHAIN (t);
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


/* Set up the hash tables for template instantiations.  */

void
init_template_processing (void)
{
  decl_specializations = htab_create_ggc (37,
					  hash_specialization,
					  eq_specializations,
					  ggc_free);
  type_specializations = htab_create_ggc (37,
					  hash_specialization,
					  eq_specializations,
					  ggc_free);
}

/* Print stats about the template hash tables for -fstats.  */

void
print_template_statistics (void)
{
  fprintf (stderr, "decl_specializations: size %ld, %ld elements, "
	   "%f collisions\n", (long) htab_size (decl_specializations),
	   (long) htab_elements (decl_specializations),
	   htab_collisions (decl_specializations));
  fprintf (stderr, "type_specializations: size %ld, %ld elements, "
	   "%f collisions\n", (long) htab_size (type_specializations),
	   (long) htab_elements (type_specializations),
	   htab_collisions (type_specializations));
}

#include "gt-cp-pt.h"
