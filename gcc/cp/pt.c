/* Handle parameterized types (templates) for GNU C++.
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000
   Free Software Foundation, Inc.
   Written by Ken Raeburn (raeburn@cygnus.com) while at Watchmaker Computing.
   Rewritten by Jason Merrill (jason@cygnus.com).

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

/* Known bugs or deficiencies include:

     all methods must be provided in header files; can't use a source
     file that contains only the method templates and "just win".  */

#include "config.h"
#include "system.h"
#include "obstack.h"

#include "tree.h"
#include "flags.h"
#include "cp-tree.h"
#include "decl.h"
#include "parse.h"
#include "lex.h"
#include "output.h"
#include "defaults.h"
#include "except.h"
#include "toplev.h"
#include "rtl.h"
#include "defaults.h"
#include "ggc.h"

/* The type of functions taking a tree, and some additional data, and
   returning an int.  */
typedef int (*tree_fn_t) PARAMS ((tree, void*));

extern struct obstack permanent_obstack;

extern int lineno;
extern char *input_filename;

/* The PENDING_TEMPLATES is a TREE_LIST of templates whose
   instantiations have been deferred, either because their definitions
   were not yet available, or because we were putting off doing the
   work.  The TREE_PURPOSE of each entry is a SRCLOC indicating where
   the instantiate request occurred; the TREE_VALUE is a either a DECL
   (for a function or static data member), or a TYPE (for a class)
   indicating what we are hoping to instantiate.  */
static tree pending_templates;
static tree *template_tail = &pending_templates;

static tree maybe_templates;
static tree *maybe_template_tail = &maybe_templates;

int processing_template_parmlist;
static int template_header_count;

static tree saved_trees;
static varray_type inline_parm_levels;
static size_t inline_parm_levels_used;

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

#define UNIFY_ALLOW_NONE 0
#define UNIFY_ALLOW_MORE_CV_QUAL 1
#define UNIFY_ALLOW_LESS_CV_QUAL 2
#define UNIFY_ALLOW_DERIVED 4
#define UNIFY_ALLOW_INTEGER 8

#define GTB_VIA_VIRTUAL 1 /* The base class we are examining is
			     virtual, or a base class of a virtual
			     base.  */
#define GTB_IGNORE_TYPE 2 /* We don't need to try to unify the current
			     type with the desired type.  */

static int resolve_overloaded_unification PARAMS ((tree, tree, tree, tree,
						 unification_kind_t, int));
static int try_one_overload PARAMS ((tree, tree, tree, tree, tree,
				   unification_kind_t, int));
static int unify PARAMS ((tree, tree, tree, tree, int));
static void add_pending_template PARAMS ((tree));
static int push_tinst_level PARAMS ((tree));
static tree classtype_mangled_name PARAMS ((tree));
static char *mangle_class_name_for_template PARAMS ((char *, tree, tree));
static tree tsubst_expr_values PARAMS ((tree, tree));
static int list_eq PARAMS ((tree, tree));
static tree get_class_bindings PARAMS ((tree, tree, tree));
static tree coerce_template_parms PARAMS ((tree, tree, tree, int, int));
static void tsubst_enum	PARAMS ((tree, tree, tree));
static tree add_to_template_args PARAMS ((tree, tree));
static tree add_outermost_template_args PARAMS ((tree, tree));
static void maybe_adjust_types_for_deduction PARAMS ((unification_kind_t, tree*,
						    tree*)); 
static int  type_unification_real PARAMS ((tree, tree, tree, tree,
					 int, unification_kind_t, int));
static void note_template_header PARAMS ((int));
static tree maybe_fold_nontype_arg PARAMS ((tree));
static tree convert_nontype_argument PARAMS ((tree, tree));
static tree convert_template_argument PARAMS ((tree, tree, tree, int,
					      int , tree));
static tree get_bindings_overload PARAMS ((tree, tree, tree));
static int for_each_template_parm PARAMS ((tree, tree_fn_t, void*));
static tree build_template_parm_index PARAMS ((int, int, int, tree, tree));
static int inline_needs_template_parms PARAMS ((tree));
static void push_inline_template_parms_recursive PARAMS ((tree, int));
static tree retrieve_specialization PARAMS ((tree, tree));
static tree retrieve_local_specialization PARAMS ((tree, tree));
static tree register_specialization PARAMS ((tree, tree, tree));
static tree register_local_specialization PARAMS ((tree, tree, tree));
static int unregister_specialization PARAMS ((tree, tree));
static tree reduce_template_parm_level PARAMS ((tree, tree, int));
static tree build_template_decl PARAMS ((tree, tree));
static int mark_template_parm PARAMS ((tree, void *));
static tree tsubst_friend_function PARAMS ((tree, tree));
static tree tsubst_friend_class PARAMS ((tree, tree));
static tree get_bindings_real PARAMS ((tree, tree, tree, int));
static int template_decl_level PARAMS ((tree));
static tree maybe_get_template_decl_from_type_decl PARAMS ((tree));
static int check_cv_quals_for_unify PARAMS ((int, tree, tree));
static tree tsubst_template_arg_vector PARAMS ((tree, tree, int));
static tree tsubst_template_parms PARAMS ((tree, tree, int));
static void regenerate_decl_from_template PARAMS ((tree, tree));
static tree most_specialized PARAMS ((tree, tree, tree));
static tree most_specialized_class PARAMS ((tree, tree));
static void set_mangled_name_for_template_decl PARAMS ((tree));
static int template_class_depth_real PARAMS ((tree, int));
static tree tsubst_aggr_type PARAMS ((tree, tree, int, tree, int));
static tree tsubst_decl PARAMS ((tree, tree, tree, tree));
static tree tsubst_arg_types PARAMS ((tree, tree, int, tree));
static tree tsubst_function_type PARAMS ((tree, tree, int, tree));
static void check_specialization_scope PARAMS ((void));
static tree process_partial_specialization PARAMS ((tree));
static void set_current_access_from_decl PARAMS ((tree));
static void check_default_tmpl_args PARAMS ((tree, tree, int, int));
static tree tsubst_call_declarator_parms PARAMS ((tree, tree, int, tree));
static tree get_template_base_recursive PARAMS ((tree, tree,
					       tree, tree, tree, int)); 
static tree get_template_base PARAMS ((tree, tree, tree, tree));
static tree try_class_unification PARAMS ((tree, tree, tree, tree));
static int coerce_template_template_parms PARAMS ((tree, tree, int,
						 tree, tree));
static tree determine_specialization PARAMS ((tree, tree, tree *, int));
static int template_args_equal PARAMS ((tree, tree));
static void print_template_context PARAMS ((int));
static void tsubst_default_arguments PARAMS ((tree));
static tree for_each_template_parm_r PARAMS ((tree *, int *, void *));

/* Called once to initialize pt.c.  */

void
init_pt ()
{
  ggc_add_tree_root (&pending_templates, 1);
  ggc_add_tree_root (&maybe_templates, 1);
  ggc_add_tree_root (&saved_trees, 1);
}

/* Do any processing required when DECL (a member template declaration
   using TEMPLATE_PARAMETERS as its innermost parameter list) is
   finished.  Returns the TEMPLATE_DECL corresponding to DECL, unless
   it is a specialization, in which case the DECL itself is returned.  */

tree
finish_member_template_decl (decl)
  tree decl;
{
  if (decl == NULL_TREE || decl == void_type_node)
    return NULL_TREE;
  else if (decl == error_mark_node)
    /* By returning NULL_TREE, the parser will just ignore this
       declaration.  We have already issued the error.  */
    return NULL_TREE;
  else if (TREE_CODE (decl) == TREE_LIST)
    {
      /* Assume that the class is the only declspec.  */
      decl = TREE_VALUE (decl);
      if (IS_AGGR_TYPE (decl) && CLASSTYPE_TEMPLATE_INFO (decl)
	  && ! CLASSTYPE_PARTIAL_SPECIALIZATION (decl))
	{
	  tree tmpl = CLASSTYPE_TI_TEMPLATE (decl);
	  check_member_template (tmpl);
	  return tmpl;
	}
      return NULL_TREE;
    }
  else if (TREE_CODE (decl) == FIELD_DECL)
    cp_error ("data member `%D' cannot be a member template", decl);
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
    cp_error ("invalid member template declaration `%D'", decl);

  return error_mark_node;
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
   COUNT_SPECIALIZATIONS is 0 or if they are instantiations, not
   specializations.  

   This function is guaranteed to return 0 if passed NULL_TREE so
   that, for example, `template_class_depth (current_class_type)' is
   always safe.  */

static int 
template_class_depth_real (type, count_specializations)
     tree type;
     int count_specializations;
{
  int depth;

  for (depth = 0; 
       type && TREE_CODE (type) != NAMESPACE_DECL;
       type = (TREE_CODE (type) == FUNCTION_DECL) 
	 ? CP_DECL_CONTEXT (type) : TYPE_CONTEXT (type))
    {
      if (TREE_CODE (type) != FUNCTION_DECL)
	{
	  if (CLASSTYPE_TEMPLATE_INFO (type)
	      && PRIMARY_TEMPLATE_P (CLASSTYPE_TI_TEMPLATE (type))
	      && ((count_specializations
		   && CLASSTYPE_PARTIAL_SPECIALIZATION (type))
		  || uses_template_parms (CLASSTYPE_TI_ARGS (type))))
	    ++depth;
	}
      else 
	{
	  if (DECL_TEMPLATE_INFO (type)
	      && PRIMARY_TEMPLATE_P (DECL_TI_TEMPLATE (type))
	      && ((count_specializations
		   && DECL_TEMPLATE_SPECIALIZATION (type))
		  || uses_template_parms (DECL_TI_ARGS (type))))
	    ++depth;
	}
    }

  return depth;
}

/* Returns the template nesting level of the indicated class TYPE.
   Like template_class_depth_real, but instantiations do not count in
   the depth.  */

int 
template_class_depth (type)
     tree type;
{
  return template_class_depth_real (type, /*count_specializations=*/0);
}

/* Returns 1 if processing DECL as part of do_pending_inlines
   needs us to push template parms.  */

static int
inline_needs_template_parms (decl)
     tree decl;
{
  if (! DECL_TEMPLATE_INFO (decl))
    return 0;

  return (TMPL_PARMS_DEPTH (DECL_TEMPLATE_PARMS (most_general_template (decl)))
	  > (processing_template_decl + DECL_TEMPLATE_SPECIALIZATION (decl)));
}

/* Subroutine of maybe_begin_member_template_processing.
   Push the template parms in PARMS, starting from LEVELS steps into the
   chain, and ending at the beginning, since template parms are listed
   innermost first.  */

static void
push_inline_template_parms_recursive (parmlist, levels)
     tree parmlist;
     int levels;
{
  tree parms = TREE_VALUE (parmlist);
  int i;

  if (levels > 1)
    push_inline_template_parms_recursive (TREE_CHAIN (parmlist), levels - 1);

  ++processing_template_decl;
  current_template_parms
    = tree_cons (build_int_2 (0, processing_template_decl),
		 parms, current_template_parms);
  TEMPLATE_PARMS_FOR_INLINE (current_template_parms) = 1;

  pushlevel (0);
  for (i = 0; i < TREE_VEC_LENGTH (parms); ++i) 
    {
      tree parm = TREE_VALUE (TREE_VEC_ELT (parms, i));
      my_friendly_assert (DECL_P (parm), 0);

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
	    tree decl = build_decl (CONST_DECL, DECL_NAME (parm),
				    TREE_TYPE (parm));
	    SET_DECL_ARTIFICIAL (decl);
	    DECL_INITIAL (decl) = DECL_INITIAL (parm);
	    SET_DECL_TEMPLATE_PARM_P (decl);
	    pushdecl (decl);
	  }
	  break;

	default:
	  my_friendly_abort (0);
	}
    }
}

/* Restore the template parameter context for a member template or
   a friend template defined in a class definition.  */

void
maybe_begin_member_template_processing (decl)
     tree decl;
{
  tree parms;
  int levels = 0;

  if (inline_needs_template_parms (decl))
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
  if (!inline_parm_levels)
    VARRAY_INT_INIT (inline_parm_levels, 4, "inline_parm_levels");
  if (inline_parm_levels_used == inline_parm_levels->num_elements)
    VARRAY_GROW (inline_parm_levels, 2 * inline_parm_levels_used);
  VARRAY_INT (inline_parm_levels, inline_parm_levels_used) = levels;
  ++inline_parm_levels_used;
}

/* Undo the effects of begin_member_template_processing. */

void 
maybe_end_member_template_processing ()
{
  int i;

  if (!inline_parm_levels_used)
    return;

  --inline_parm_levels_used;
  for (i = 0; 
       i < VARRAY_INT (inline_parm_levels, inline_parm_levels_used);
       ++i) 
    {
      --processing_template_decl;
      current_template_parms = TREE_CHAIN (current_template_parms);
      poplevel (0, 0, 0);
    }
}

/* Returns non-zero iff T is a member template function.  We must be
   careful as in

     template <class T> class C { void f(); }

   Here, f is a template function, and a member, but not a member
   template.  This function does not concern itself with the origin of
   T, only its present state.  So if we have 

     template <class T> class C { template <class U> void f(U); }

   then neither C<int>::f<char> nor C<T>::f<double> is considered
   to be a member template.  But, `template <class U> void
   C<int>::f(U)' is considered a member template.  */

int
is_member_template (t)
     tree t;
{
  if (!DECL_FUNCTION_TEMPLATE_P (t))
    /* Anything that isn't a function or a template function is
       certainly not a member template.  */
    return 0;

  /* A local class can't have member templates.  */
  if (decl_function_context (t))
    return 0;

  return (DECL_FUNCTION_MEMBER_P (DECL_TEMPLATE_RESULT (t))
	  /* If there are more levels of template parameters than
	     there are template classes surrounding the declaration,
	     then we have a member template.  */
	  && (TMPL_PARMS_DEPTH (DECL_TEMPLATE_PARMS (t)) > 
	      template_class_depth (DECL_CONTEXT (t))));
}

#if 0 /* UNUSED */
/* Returns non-zero iff T is a member template class.  See
   is_member_template for a description of what precisely constitutes
   a member template.  */

int
is_member_template_class (t)
     tree t;
{
  if (!DECL_CLASS_TEMPLATE_P (t))
    /* Anything that isn't a class template, is certainly not a member
       template.  */
    return 0;

  if (!DECL_CLASS_SCOPE_P (t))
    /* Anything whose context isn't a class type is surely not a
       member template.  */
    return 0;

  /* If there are more levels of template parameters than there are
     template classes surrounding the declaration, then we have a
     member template.  */
  return  (TMPL_PARMS_DEPTH (DECL_TEMPLATE_PARMS (t)) > 
	   template_class_depth (DECL_CONTEXT (t)));
}
#endif

/* Return a new template argument vector which contains all of ARGS,
   but has as its innermost set of arguments the EXTRA_ARGS.  The
   resulting vector will be built on a temporary obstack, and so must
   be explicitly copied to the permanent obstack, if required.  */

static tree
add_to_template_args (args, extra_args)
     tree args;
     tree extra_args;
{
  tree new_args;
  int extra_depth;
  int i;
  int j;

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
add_outermost_template_args (args, extra_args)
     tree args;
     tree extra_args;
{
  tree new_args;

  /* If there are more levels of EXTRA_ARGS than there are ARGS,
     something very fishy is going on.  */
  my_friendly_assert (TMPL_ARGS_DEPTH (args) >= TMPL_ARGS_DEPTH (extra_args),
		      0);

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

/* We've got a template header coming up; push to a new level for storing
   the parms.  */

void
begin_template_parm_list ()
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
  begin_scope (sk_template_parms);
  ++processing_template_decl;
  ++processing_template_parmlist;
  note_template_header (0);
}

/* This routine is called when a specialization is declared.  If it is
   illegal to declare a specialization here, an error is reported.  */

static void
check_specialization_scope ()
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
    cp_error ("explicit specialization in non-namespace scope `%D'",
	      scope);

  /* [temp.expl.spec] 

     In an explicit specialization declaration for a member of a class
     template or a member template that appears in namespace scope,
     the member template and some of its enclosing class templates may
     remain unspecialized, except that the declaration shall not
     explicitly specialize a class member template if its enclosing
     class templates are not explicitly specialized as well.  */
  if (current_template_parms) 
    cp_error ("enclosing class templates are not explicitly specialized");
}

/* We've just seen template <>. */

void
begin_specialization ()
{
  begin_scope (sk_template_spec);
  note_template_header (1);
  check_specialization_scope ();
}

/* Called at then end of processing a declaration preceeded by
   template<>.  */

void 
end_specialization ()
{
  finish_scope ();
  reset_specialization ();
}

/* Any template <>'s that we have seen thus far are not referring to a
   function specialization. */

void
reset_specialization ()
{
  processing_specialization = 0;
  template_header_count = 0;
}

/* We've just seen a template header.  If SPECIALIZATION is non-zero,
   it was of the form template <>.  */

static void 
note_template_header (specialization)
     int specialization;
{
  processing_specialization = specialization;
  template_header_count++;
}

/* We're beginning an explicit instantiation.  */

void
begin_explicit_instantiation ()
{
  ++processing_explicit_instantiation;
}


void
end_explicit_instantiation ()
{
  my_friendly_assert(processing_explicit_instantiation > 0, 0);
  --processing_explicit_instantiation;
}

/* The TYPE is being declared.  If it is a template type, that means it
   is a partial specialization.  Do appropriate error-checking.  */

void 
maybe_process_partial_specialization (type)
     tree type;
{
  if (IS_AGGR_TYPE (type) && CLASSTYPE_USE_TEMPLATE (type))
    {
      if (CLASSTYPE_IMPLICIT_INSTANTIATION (type)
	  && !COMPLETE_TYPE_P (type))
	{
	  if (current_namespace
	      != decl_namespace_context (CLASSTYPE_TI_TEMPLATE (type)))
	    {
	      cp_pedwarn ("specializing `%#T' in different namespace", type);
	      cp_pedwarn_at ("  from definition of `%#D'",
			     CLASSTYPE_TI_TEMPLATE (type));
	    }
	  SET_CLASSTYPE_PARTIAL_SPECIALIZATION (type);
	  if (processing_template_decl)
	    push_template_decl (TYPE_MAIN_DECL (type));
	}
      else if (CLASSTYPE_TEMPLATE_INSTANTIATION (type))
	cp_error ("specialization of `%T' after instantiation", type);
    }
  else if (processing_specialization)
    cp_error ("explicit specialization of non-template `%T'", type);
}

/* Retrieve the specialization (in the sense of [temp.spec] - a
   specialization is either an instantiation or an explicit
   specialization) of TMPL for the given template ARGS.  If there is
   no such specialization, return NULL_TREE.  The ARGS are a vector of
   arguments, or a vector of vectors of arguments, in the case of
   templates with more than one level of parameters.  */
   
static tree
retrieve_specialization (tmpl, args)
     tree tmpl;
     tree args;
{
  tree s;

  my_friendly_assert (TREE_CODE (tmpl) == TEMPLATE_DECL, 0);

  /* There should be as many levels of arguments as there are
     levels of parameters.  */
  my_friendly_assert (TMPL_ARGS_DEPTH (args) 
		      == TMPL_PARMS_DEPTH (DECL_TEMPLATE_PARMS (tmpl)),
		      0);
		      
  for (s = DECL_TEMPLATE_SPECIALIZATIONS (tmpl);
       s != NULL_TREE;
       s = TREE_CHAIN (s))
    if (comp_template_args (TREE_PURPOSE (s), args))
      return TREE_VALUE (s);

  return NULL_TREE;
}

/* Like retrieve_speciailization, but for local declarations.  FN is
   the function in which we are looking for an instantiation.  */

static tree
retrieve_local_specialization (tmpl, fn)
     tree tmpl;
     tree fn;
{
  tree s = purpose_member (fn, DECL_TEMPLATE_SPECIALIZATIONS (tmpl));
  return s ? TREE_VALUE (s) : NULL_TREE;
}

/* Returns non-zero iff DECL is a specialization of TMPL.  */

int
is_specialization_of (decl, tmpl)
     tree decl;
     tree tmpl;
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
      my_friendly_assert (TREE_CODE (decl) == TYPE_DECL, 0);

      for (t = TREE_TYPE (decl);
	   t != NULL_TREE;
	   t = CLASSTYPE_USE_TEMPLATE (t)
	     ? TREE_TYPE (CLASSTYPE_TI_TEMPLATE (t)) : NULL_TREE)
	if (same_type_p (TYPE_MAIN_VARIANT (t), 
			 TYPE_MAIN_VARIANT (TREE_TYPE (tmpl))))
	  return 1;
    }  

  return 0;
}

/* Register the specialization SPEC as a specialization of TMPL with
   the indicated ARGS.  Returns SPEC, or an equivalent prior
   declaration, if available.  */

static tree
register_specialization (spec, tmpl, args)
     tree spec;
     tree tmpl;
     tree args;
{
  tree s;

  my_friendly_assert (TREE_CODE (tmpl) == TEMPLATE_DECL, 0);

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
    
  /* There should be as many levels of arguments as there are
     levels of parameters.  */
  my_friendly_assert (TMPL_ARGS_DEPTH (args) 
		      == TMPL_PARMS_DEPTH (DECL_TEMPLATE_PARMS (tmpl)),
		      0);

  for (s = DECL_TEMPLATE_SPECIALIZATIONS (tmpl);
       s != NULL_TREE;
       s = TREE_CHAIN (s))
    {
      tree fn = TREE_VALUE (s);

      /* We can sometimes try to re-register a specialization that we've
	 already got.  In particular, regenerate_decl_from_template
	 calls duplicate_decls which will update the specialization
	 list.  But, we'll still get called again here anyhow.  It's
	 more convenient to simply allow this than to try to prevent it.  */
      if (fn == spec)
	return spec;
      else if (comp_template_args (TREE_PURPOSE (s), args))
	{
	  if (DECL_TEMPLATE_SPECIALIZATION (spec))
	    {
	      if (DECL_TEMPLATE_INSTANTIATION (fn))
		{
		  if (TREE_USED (fn) 
		      || DECL_EXPLICIT_INSTANTIATION (fn))
		    {
		      cp_error ("specialization of %D after instantiation",
				fn);
		      return spec;
		    }
		  else
		    {
		      /* This situation should occur only if the first
			 specialization is an implicit instantiation,
			 the second is an explicit specialization, and
			 the implicit instantiation has not yet been
			 used.  That situation can occur if we have
			 implicitly instantiated a member function and
			 then specialized it later.

			 We can also wind up here if a friend
			 declaration that looked like an instantiation
			 turns out to be a specialization:

			   template <class T> void foo(T);
			   class S { friend void foo<>(int) };
			   template <> void foo(int);  

			 We transform the existing DECL in place so that
			 any pointers to it become pointers to the
			 updated declaration.  

			 If there was a definition for the template, but
			 not for the specialization, we want this to
			 look as if there is no definition, and vice
			 versa.  */
		      DECL_INITIAL (fn) = NULL_TREE;
		      duplicate_decls (spec, fn);

		      return fn;
		    }
		}
	      else if (DECL_TEMPLATE_SPECIALIZATION (fn))
		{
		  duplicate_decls (spec, fn);
		  return fn;
		}
	    }
	}
      }

  DECL_TEMPLATE_SPECIALIZATIONS (tmpl)
     = tree_cons (args, spec, DECL_TEMPLATE_SPECIALIZATIONS (tmpl));

  return spec;
}

/* Unregister the specialization SPEC as a specialization of TMPL.
   Returns nonzero if the SPEC was listed as a specialization of
   TMPL.  */

static int
unregister_specialization (spec, tmpl)
     tree spec;
     tree tmpl;
{
  tree* s;

  for (s = &DECL_TEMPLATE_SPECIALIZATIONS (tmpl);
       *s != NULL_TREE;
       s = &TREE_CHAIN (*s))
    if (TREE_VALUE (*s) == spec)
      {
	*s = TREE_CHAIN (*s);
	return 1;
      }

  return 0;
}

/* Like register_specialization, but for local declarations.  FN is
   the function in which we are registering SPEC, an instantiation of
   TMPL.  */

static tree
register_local_specialization (spec, tmpl, fn)
     tree spec;
     tree tmpl;
     tree fn;
{
  DECL_TEMPLATE_SPECIALIZATIONS (tmpl)
     = tree_cons (fn, spec, DECL_TEMPLATE_SPECIALIZATIONS (tmpl));

  return spec;
}

/* Print the list of candidate FNS in an error message.  */

void
print_candidates (fns)
     tree fns;
{
  tree fn;

  const char *str = "candidates are:";

  for (fn = fns; fn != NULL_TREE; fn = TREE_CHAIN (fn))
    {
      tree f;

      for (f = TREE_VALUE (fn); f; f = OVL_NEXT (f))
	cp_error_at ("%s %+#D", str, OVL_CURRENT (f));
      str = "               ";
    }
}

/* Returns the template (one of the functions given by TEMPLATE_ID)
   which can be specialized to match the indicated DECL with the
   explicit template args given in TEMPLATE_ID.  The DECL may be
   NULL_TREE if none is available.  In that case, the functions in
   TEMPLATE_ID are non-members.

   If NEED_MEMBER_TEMPLATE is non-zero the function is known to be a
   specialization of a member template.

   The template args (those explicitly specified and those deduced)
   are output in a newly created vector *TARGS_OUT.

   If it is impossible to determine the result, an error message is
   issued.  The error_mark_node is returned to indicate failure.  */

static tree
determine_specialization (template_id, decl, targs_out, 
			  need_member_template)
     tree template_id;
     tree decl;
     tree* targs_out;
     int need_member_template;
{
  tree fns;
  tree targs;
  tree explicit_targs;
  tree candidates = NULL_TREE;
  tree templates = NULL_TREE;

  *targs_out = NULL_TREE;

  if (template_id == error_mark_node)
    return error_mark_node;

  fns = TREE_OPERAND (template_id, 0);
  explicit_targs = TREE_OPERAND (template_id, 1);

  if (fns == error_mark_node)
    return error_mark_node;

  /* Check for baselinks. */
  if (BASELINK_P (fns))
    fns = TREE_VALUE (fns);

  if (!is_overloaded_fn (fns))
    {
      cp_error ("`%D' is not a function template", fns);
      return error_mark_node;
    }

  for (; fns; fns = OVL_NEXT (fns))
    {
      tree tmpl;

      tree fn = OVL_CURRENT (fns);

      if (TREE_CODE (fn) == TEMPLATE_DECL)
	/* DECL might be a specialization of FN.  */
	tmpl = fn;
      else if (need_member_template)
	/* FN is an ordinary member function, and we need a
	   specialization of a member template.  */
	continue;
      else if (TREE_CODE (fn) != FUNCTION_DECL)
	/* We can get IDENTIFIER_NODEs here in certain erroneous
	   cases.  */
	continue;
      else if (!DECL_FUNCTION_MEMBER_P (fn))
	/* This is just an ordinary non-member function.  Nothing can
	   be a specialization of that.  */
	continue;
      else
	{
	  tree decl_arg_types;

	  /* This is an ordinary member function.  However, since
	     we're here, we can assume it's enclosing class is a
	     template class.  For example,
	     
	       template <typename T> struct S { void f(); };
	       template <> void S<int>::f() {}

	     Here, S<int>::f is a non-template, but S<int> is a
	     template class.  If FN has the same type as DECL, we
	     might be in business.  */
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

	  continue;
	}

      /* See whether this function might be a specialization of this
	 template.  */
      targs = get_bindings (tmpl, decl, explicit_targs);

      if (!targs)
	/* We cannot deduce template arguments that when used to
	   specialize TMPL will produce DECL.  */
	continue;

      /* Save this template, and the arguments deduced.  */
      templates = tree_cons (targs, tmpl, templates);
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
	 This extension can only serve to make illegal programs legal,
	 so it's safe.  And, there is strong anecdotal evidence that
	 the committee intended the partial ordering rules to apply;
	 the EDG front-end has that behavior, and John Spicer claims
	 that the committee simply forgot to delete the wording in
	 [temp.expl.spec].  */
     tree tmpl = most_specialized (templates, decl, explicit_targs);
     if (tmpl && tmpl != error_mark_node)
       {
	 targs = get_bindings (tmpl, decl, explicit_targs);
	 templates = tree_cons (targs, tmpl, NULL_TREE);
       }
    }

  if (templates == NULL_TREE && candidates == NULL_TREE)
    {
      cp_error_at ("template-id `%D' for `%+D' does not match any template declaration",
		   template_id, decl);
      return error_mark_node;
    }
  else if ((templates && TREE_CHAIN (templates))
	   || (candidates && TREE_CHAIN (candidates))
	   || (templates && candidates))
    {
      cp_error_at ("ambiguous template specialization `%D' for `%+D'",
		   template_id, decl);
      chainon (candidates, templates);
      print_candidates (candidates);
      return error_mark_node;
    }

  /* We have one, and exactly one, match. */
  if (candidates)
    {
      /* It was a specialization of an ordinary member function in a
	 template class.  */
      *targs_out = copy_node (DECL_TI_ARGS (TREE_VALUE (candidates)));
      return DECL_TI_TEMPLATE (TREE_VALUE (candidates));
    }

  /* It was a specialization of a template.  */
  targs = DECL_TI_ARGS (DECL_RESULT (TREE_VALUE (templates)));
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
   illegal; there should be no template <>.)

   If the function is a specialization, it is marked as such via
   DECL_TEMPLATE_SPECIALIZATION.  Furthermore, its DECL_TEMPLATE_INFO
   is set up correctly, and it is added to the list of specializations 
   for that template.  */

tree
check_explicit_specialization (declarator, decl, template_count, flags)
     tree declarator;
     tree decl;
     int template_count;
     int flags;
{
  int have_def = flags & 2;
  int is_friend = flags & 4;
  int specialization = 0;
  int explicit_instantiation = 0;
  int member_specialization = 0;
  tree ctype = DECL_CLASS_CONTEXT (decl);
  tree dname = DECL_NAME (decl);
  tmpl_spec_kind tsk;

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

	      cp_error ("template-id `%D' in declaration of primary template",
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
      cp_error ("template parameter list used in explicit instantiation");

      /* Fall through.  */

    case tsk_expl_inst:
      if (have_def)
	cp_error ("definition provided for explicit instantiation");
      
      explicit_instantiation = 1;
      break;

    case tsk_excessive_parms:
      cp_error ("too many template parameter lists in declaration of `%D'", 
		decl);
      return error_mark_node;

      /* Fall through.  */
    case tsk_expl_spec:
      SET_DECL_TEMPLATE_SPECIALIZATION (decl);
      if (ctype)
	member_specialization = 1;
      else
	specialization = 1;
      break;
     
    case tsk_insufficient_parms:
      if (template_header_count)
	{
	  cp_error("too few template parameter lists in declaration of `%D'", 
		   decl);
	  return decl;
	}
      else if (ctype != NULL_TREE
	       && !TYPE_BEING_DEFINED (ctype)
	       && CLASSTYPE_TEMPLATE_INSTANTIATION (ctype)
	       && !is_friend)
	{
	  /* For backwards compatibility, we accept:

	       template <class T> struct S { void f(); };
	       void S<int>::f() {} // Missing template <>

	     That used to be legal C++.  */
	  if (pedantic)
	    cp_pedwarn
	      ("explicit specialization not preceded by `template <>'");
	  specialization = 1;
	  SET_DECL_TEMPLATE_SPECIALIZATION (decl);
	}
      break;

    case tsk_template:
      if (TREE_CODE (declarator) == TEMPLATE_ID_EXPR)
	{
	  /* This case handles bogus declarations like template <>
	     template <class T> void f<int>(); */

	  cp_error ("template-id `%D' in declaration of primary template",
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
      my_friendly_abort (20000309);
    }

  if (specialization || member_specialization)
    {
      tree t = TYPE_ARG_TYPES (TREE_TYPE (decl));
      for (; t; t = TREE_CHAIN (t))
	if (TREE_PURPOSE (t))
	  {
	    cp_pedwarn
	      ("default argument specified in explicit specialization");
	    break;
	  }
      if (current_lang_name == lang_name_c)
	cp_error ("template specialization with C linkage");
    }

  if (specialization || member_specialization || explicit_instantiation)
    {
      tree tmpl = NULL_TREE;
      tree targs = NULL_TREE;

      /* Make sure that the declarator is a TEMPLATE_ID_EXPR.  */
      if (TREE_CODE (declarator) != TEMPLATE_ID_EXPR)
	{
	  tree fns;

	  my_friendly_assert (TREE_CODE (declarator) == IDENTIFIER_NODE, 
			      0);
	  if (!ctype)
	    fns = IDENTIFIER_NAMESPACE_VALUE (dname);
	  else
	    fns = dname;

	  declarator = 
	    lookup_template_function (fns, NULL_TREE);
	}

      if (declarator == error_mark_node)
	return error_mark_node;

      if (ctype != NULL_TREE && TYPE_BEING_DEFINED (ctype))
	{
	  if (!explicit_instantiation)
	    /* A specialization in class scope.  This is illegal,
	       but the error will already have been flagged by
	       check_specialization_scope.  */
	    return error_mark_node;
	  else
	    {
	      /* It's not legal to write an explicit instantiation in
		 class scope, e.g.:

	           class C { template void f(); }

		   This case is caught by the parser.  However, on
		   something like:
	       
		   template class C { void f(); };

		   (which is illegal) we can get here.  The error will be
		   issued later.  */
	      ;
	    }

	  return decl;
	}
      else if (TREE_CODE (TREE_OPERAND (declarator, 0)) == LOOKUP_EXPR)
	{
	  /* A friend declaration.  We can't do much, because we don't
	   know what this resolves to, yet.  */
	  my_friendly_assert (is_friend != 0, 0);
	  my_friendly_assert (!explicit_instantiation, 0);
	  SET_DECL_IMPLICIT_INSTANTIATION (decl);
	  return decl;
	} 
      else if (ctype != NULL_TREE 
	       && (TREE_CODE (TREE_OPERAND (declarator, 0)) ==
		   IDENTIFIER_NODE))
	{
	  /* Find the list of functions in ctype that have the same
	     name as the declared function.  */
	  tree name = TREE_OPERAND (declarator, 0);
	  tree fns = NULL_TREE;
	  int idx;

	  if (name == constructor_name (ctype) 
	      || name == constructor_name_full (ctype))
	    {
	      int is_constructor = DECL_CONSTRUCTOR_P (decl);
	      
	      if (is_constructor ? !TYPE_HAS_CONSTRUCTOR (ctype)
		  : !TYPE_HAS_DESTRUCTOR (ctype))
		{
		  /* From [temp.expl.spec]:
		       
		     If such an explicit specialization for the member
		     of a class template names an implicitly-declared
		     special member function (clause _special_), the
		     program is ill-formed.  

		     Similar language is found in [temp.explicit].  */
		  cp_error ("specialization of implicitly-declared special member function");
		  return error_mark_node;
		}

	      name = is_constructor ? ctor_identifier : dtor_identifier;
	    }

	  if (!IDENTIFIER_TYPENAME_P (name))
	    {
	      idx = lookup_fnfields_1 (ctype, name);
	      if (idx >= 0)
		fns = TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (ctype), idx);
	    }
	  else
	    {
	      tree methods;

	      /* For a type-conversion operator, we cannot do a
		 name-based lookup.  We might be looking for `operator
		 int' which will be a specialization of `operator T'.
		 So, we find *all* the conversion operators, and then
		 select from them.  */
	      fns = NULL_TREE;

	      methods = CLASSTYPE_METHOD_VEC (ctype);
	      if (methods)
		for (idx = 2; idx < TREE_VEC_LENGTH (methods); ++idx) 
		  {
		    tree ovl = TREE_VEC_ELT (methods, idx);

		    if (!ovl || !DECL_CONV_FN_P (OVL_CURRENT (ovl)))
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
	      cp_error ("no member function `%D' declared in `%T'",
			name, ctype);
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
				       member_specialization);
	    
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
		     be too many levels fo TARGS.  Remove some of them
		     here.  */
		  int i;
		  tree new_targs;

		  new_targs = make_tree_vec (parm_depth);
		  for (i = arg_depth - parm_depth; i < arg_depth; ++i)
		    TREE_VEC_ELT (new_targs, i - (arg_depth - parm_depth))
		      = TREE_VEC_ELT (targs, i);
		  targs = new_targs;
		}
		  
	      return instantiate_template (tmpl, targs);
	    }

	  /* If this is both a template specialization, then it's a
	     specialization of a member template of a template class.
	     In that case we want to return the TEMPLATE_DECL, not the
	     specialization of it.  */
	  if (tsk == tsk_template)
	    {
	      SET_DECL_TEMPLATE_SPECIALIZATION (tmpl);
	      return tmpl;
	    }

	  /* If we though that the DECL was a member function, but it
	     turns out to be specializing a static member function,
	     make DECL a static member function as well.  */
	  if (DECL_STATIC_FUNCTION_P (tmpl)
	      && DECL_NONSTATIC_MEMBER_FUNCTION_P (decl))
	    {
	      revert_static_member_fn (decl);
	      last_function_parms = TREE_CHAIN (last_function_parms);
	    }

	  /* Set up the DECL_TEMPLATE_INFO for DECL.  */
	  DECL_TEMPLATE_INFO (decl) = tree_cons (tmpl, targs, NULL_TREE);

	  /* Mangle the function name appropriately.  Note that we do
	     not mangle specializations of non-template member
	     functions of template classes, e.g. with

	       template <class T> struct S { void f(); }

	     and given the specialization 

	       template <> void S<int>::f() {}

	     we do not mangle S<int>::f() here.  That's because it's
	     just an ordinary member function and doesn't need special
	     treatment.  We do this here so that the ordinary,
	     non-template, name-mangling algorithm will not be used
	     later.  */
	  if ((is_member_template (tmpl) || ctype == NULL_TREE)
	      && name_mangling_version >= 1)
	    set_mangled_name_for_template_decl (decl);

	  if (is_friend && !have_def)
	    /* This is not really a declaration of a specialization.
	       It's just the name of an instantiation.  But, it's not
	       a request for an instantiation, either.  */
	    SET_DECL_IMPLICIT_INSTANTIATION (decl);

	  /* Register this specialization so that we can find it
	     again.  */
	  decl = register_specialization (decl, gen_tmpl, targs);
	}
    }
  
  return decl;
}

/* TYPE is being declared.  Verify that the use of template headers
   and such is reasonable.  Issue error messages if not.  */

void
maybe_check_template_type (type)
     tree type;
{
  if (template_header_count)
    {
      /* We are in the scope of some `template <...>' header.  */

      int context_depth 
	= template_class_depth_real (TYPE_CONTEXT (type),
				     /*count_specializations=*/1);

      if (template_header_count <= context_depth)
	/* This is OK; the template headers are for the context.  We
	   are actually too lenient here; like
	   check_explicit_specialization we should consider the number
	   of template types included in the actual declaration.  For
	   example, 

	     template <class T> struct S {
	       template <class U> template <class V>
	       struct I {};
	     }; 

	   is illegal, but:

	     template <class T> struct S {
	       template <class U> struct I;
	     }; 

	     template <class T> template <class U.
	     struct S<T>::I {};

	   is not.  */
	; 
      else if (template_header_count > context_depth + 1)
	/* There are two many template parameter lists.  */
	cp_error ("too many template parameter lists in declaration of `%T'", type); 
    }
}

/* Returns 1 iff PARMS1 and PARMS2 are identical sets of template
   parameters.  These are represented in the same format used for
   DECL_TEMPLATE_PARMS.  */

int comp_template_parms (parms1, parms2)
     tree parms1;
     tree parms2;
{
  tree p1;
  tree p2;

  if (parms1 == parms2)
    return 1;

  for (p1 = parms1, p2 = parms2; 
       p1 != NULL_TREE && p2 != NULL_TREE;
       p1 = TREE_CHAIN (p1), p2 = TREE_CHAIN (p2))
    {
      tree t1 = TREE_VALUE (p1);
      tree t2 = TREE_VALUE (p2);
      int i;

      my_friendly_assert (TREE_CODE (t1) == TREE_VEC, 0);
      my_friendly_assert (TREE_CODE (t2) == TREE_VEC, 0);

      if (TREE_VEC_LENGTH (t1) != TREE_VEC_LENGTH (t2))
	return 0;

      for (i = 0; i < TREE_VEC_LENGTH (t2); ++i) 
	{
	  tree parm1 = TREE_VALUE (TREE_VEC_ELT (t1, i));
	  tree parm2 = TREE_VALUE (TREE_VEC_ELT (t2, i));

	  if (TREE_CODE (parm1) != TREE_CODE (parm2))
	    return 0;

	  if (TREE_CODE (parm1) == TEMPLATE_TYPE_PARM)
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

/* Complain if DECL shadows a template parameter.

   [temp.local]: A template-parameter shall not be redeclared within its
   scope (including nested scopes).  */

void
check_template_shadow (decl)
     tree decl;
{
  tree olddecl;

  /* If we're not in a template, we can't possibly shadow a template
     parameter.  */
  if (!current_template_parms)
    return;

  /* Figure out what we're shadowing.  */
  if (TREE_CODE (decl) == OVERLOAD)
    decl = OVL_CURRENT (decl);
  olddecl = IDENTIFIER_VALUE (DECL_NAME (decl));

  /* If there's no previous binding for this name, we're not shadowing
     anything, let alone a template parameter.  */
  if (!olddecl)
    return;

  /* If we're not shadowing a template parameter, we're done.  Note
     that OLDDECL might be an OVERLOAD (or perhaps even an
     ERROR_MARK), so we can't just blithely assume it to be a _DECL
     node.  */
  if (!DECL_P (olddecl) || !DECL_TEMPLATE_PARM_P (olddecl))
    return;

  /* We check for decl != olddecl to avoid bogus errors for using a
     name inside a class.  We check TPFI to avoid duplicate errors for
     inline member templates.  */
  if (decl == olddecl 
      || TEMPLATE_PARMS_FOR_INLINE (current_template_parms))
    return;

  cp_error_at ("declaration of `%#D'", decl);
  cp_error_at (" shadows template parm `%#D'", olddecl);
}

/* Return a new TEMPLATE_PARM_INDEX with the indicated INDEX, LEVEL,
   ORIG_LEVEL, DECL, and TYPE.  */

static tree
build_template_parm_index (index, level, orig_level, decl, type)
     int index;
     int level;
     int orig_level;
     tree decl;
     tree type;
{
  tree t = make_node (TEMPLATE_PARM_INDEX);
  TEMPLATE_PARM_IDX (t) = index;
  TEMPLATE_PARM_LEVEL (t) = level;
  TEMPLATE_PARM_ORIG_LEVEL (t) = orig_level;
  TEMPLATE_PARM_DECL (t) = decl;
  TREE_TYPE (t) = type;

  return t;
}

/* Return a TEMPLATE_PARM_INDEX, similar to INDEX, but whose
   TEMPLATE_PARM_LEVEL has been decreased by LEVELS.  If such a
   TEMPLATE_PARM_INDEX already exists, it is returned; otherwise, a
   new one is created.  */

static tree 
reduce_template_parm_level (index, type, levels)
     tree index;
     tree type;
     int levels;
{
  if (TEMPLATE_PARM_DESCENDANTS (index) == NULL_TREE
      || (TEMPLATE_PARM_LEVEL (TEMPLATE_PARM_DESCENDANTS (index))
	  != TEMPLATE_PARM_LEVEL (index) - levels))
    {
      tree decl 
	= build_decl (TREE_CODE (TEMPLATE_PARM_DECL (index)),
		      DECL_NAME (TEMPLATE_PARM_DECL (index)),
		      type);
      tree t
	= build_template_parm_index (TEMPLATE_PARM_IDX (index),
				     TEMPLATE_PARM_LEVEL (index) - levels,
				     TEMPLATE_PARM_ORIG_LEVEL (index),
				     decl, type);
      TEMPLATE_PARM_DESCENDANTS (index) = t;

      /* Template template parameters need this.  */
      DECL_TEMPLATE_PARMS (decl)
	= DECL_TEMPLATE_PARMS (TEMPLATE_PARM_DECL (index));
    }

  return TEMPLATE_PARM_DESCENDANTS (index);
}

/* Process information from new template parameter NEXT and append it to the
   LIST being built.  */

tree
process_template_parm (list, next)
     tree list, next;
{
  tree parm;
  tree decl = 0;
  tree defval;
  int is_type, idx;

  parm = next;
  my_friendly_assert (TREE_CODE (parm) == TREE_LIST, 259);
  defval = TREE_PURPOSE (parm);
  parm = TREE_VALUE (parm);
  is_type = TREE_PURPOSE (parm) == class_type_node;

  if (list)
    {
      tree p = TREE_VALUE (tree_last (list));

      if (TREE_CODE (p) == TYPE_DECL)
	idx = TEMPLATE_TYPE_IDX (TREE_TYPE (p));
      else if (TREE_CODE (p) == TEMPLATE_DECL)
	idx = TEMPLATE_TYPE_IDX (TREE_TYPE (DECL_TEMPLATE_RESULT (p)));
      else
	idx = TEMPLATE_PARM_IDX (DECL_INITIAL (p));
      ++idx;
    }
  else
    idx = 0;

  if (!is_type)
    {
      my_friendly_assert (TREE_CODE (TREE_PURPOSE (parm)) == TREE_LIST, 260);
      /* is a const-param */
      parm = grokdeclarator (TREE_VALUE (parm), TREE_PURPOSE (parm),
			     PARM, 0, NULL_TREE);

      /* [temp.param]

	 The top-level cv-qualifiers on the template-parameter are
	 ignored when determining its type.  */
      TREE_TYPE (parm) = TYPE_MAIN_VARIANT (TREE_TYPE (parm));

      /* A template parameter is not modifiable.  */
      TREE_READONLY (parm) = 1;
      if (IS_AGGR_TYPE (TREE_TYPE (parm))
	  && TREE_CODE (TREE_TYPE (parm)) != TEMPLATE_TYPE_PARM
	  && TREE_CODE (TREE_TYPE (parm)) != TYPENAME_TYPE)
	{
	  cp_error ("`%#T' is not a valid type for a template constant parameter",
		    TREE_TYPE (parm));
	  if (DECL_NAME (parm) == NULL_TREE)
	    error ("  a template type parameter must begin with `class' or `typename'");
	  TREE_TYPE (parm) = void_type_node;
	}
      else if (pedantic
	       && (TREE_CODE (TREE_TYPE (parm)) == REAL_TYPE
		   || TREE_CODE (TREE_TYPE (parm)) == COMPLEX_TYPE))
	cp_pedwarn ("`%T' is not a valid type for a template constant parameter",
		    TREE_TYPE (parm));
      decl = build_decl (CONST_DECL, DECL_NAME (parm), TREE_TYPE (parm));
      DECL_INITIAL (parm) = DECL_INITIAL (decl) 
	= build_template_parm_index (idx, processing_template_decl,
				     processing_template_decl,
				     decl, TREE_TYPE (parm));
    }
  else
    {
      tree t;
      parm = TREE_VALUE (parm);
      
      if (parm && TREE_CODE (parm) == TEMPLATE_DECL)
	{
	  t = make_aggr_type (TEMPLATE_TEMPLATE_PARM);
	  /* This is for distinguishing between real templates and template 
	     template parameters */
	  TREE_TYPE (parm) = t;
	  TREE_TYPE (DECL_TEMPLATE_RESULT (parm)) = t;
	  decl = parm;
	}
      else
	{
	  t = make_aggr_type (TEMPLATE_TYPE_PARM);
	  /* parm is either IDENTIFIER_NODE or NULL_TREE */
	  decl = build_decl (TYPE_DECL, parm, t);
	}
        
      TYPE_NAME (t) = decl;
      TYPE_STUB_DECL (t) = decl;
      parm = decl;
      TEMPLATE_TYPE_PARM_INDEX (t)
	= build_template_parm_index (idx, processing_template_decl, 
				     processing_template_decl,
				     decl, TREE_TYPE (parm));
    }
  SET_DECL_ARTIFICIAL (decl);
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
end_template_parm_list (parms)
     tree parms;
{
  int nparms;
  tree parm;
  tree saved_parmlist = make_tree_vec (list_length (parms));

  current_template_parms
    = tree_cons (build_int_2 (0, processing_template_decl),
		 saved_parmlist, current_template_parms);

  for (parm = parms, nparms = 0; 
       parm; 
       parm = TREE_CHAIN (parm), nparms++)
    TREE_VEC_ELT (saved_parmlist, nparms) = parm;

  --processing_template_parmlist;

  return saved_parmlist;
}

/* end_template_decl is called after a template declaration is seen.  */

void
end_template_decl ()
{
  reset_specialization ();

  if (! processing_template_decl)
    return;

  /* This matches the pushlevel in begin_template_parm_list.  */
  finish_scope ();

  --processing_template_decl;
  current_template_parms = TREE_CHAIN (current_template_parms);
}

/* Given a template argument vector containing the template PARMS.
   The innermost PARMS are given first.  */

tree
current_template_args ()
{
  tree header;
  tree args = NULL_TREE;
  int length = TMPL_PARMS_DEPTH (current_template_parms);
  int l = length;

  /* If there is only one level of template parameters, we do not
     create a TREE_VEC of TREE_VECs.  Instead, we return a single
     TREE_VEC containing the arguments.  */
  if (length > 1)
    args = make_tree_vec (length);

  for (header = current_template_parms; header; header = TREE_CHAIN (header))
    {
      tree a = copy_node (TREE_VALUE (header));
      int i;

      TREE_TYPE (a) = NULL_TREE;
      for (i = TREE_VEC_LENGTH (a) - 1; i >= 0; --i)
	{
	  tree t = TREE_VEC_ELT (a, i);

	  /* T will be a list if we are called from within a
	     begin/end_template_parm_list pair, but a vector directly
	     if within a begin/end_member_template_processing pair.  */
	  if (TREE_CODE (t) == TREE_LIST) 
	    {
	      t = TREE_VALUE (t);
	      
	      if (TREE_CODE (t) == TYPE_DECL 
		  || TREE_CODE (t) == TEMPLATE_DECL)
		t = TREE_TYPE (t);
	      else
		t = DECL_INITIAL (t);
	      TREE_VEC_ELT (a, i) = t;
	    }
	}

      if (length > 1)
	TREE_VEC_ELT (args, --l) = a;
      else
	args = a;
    }

  return args;
}

/* Return a TEMPLATE_DECL corresponding to DECL, using the indicated
   template PARMS.  Used by push_template_decl below.  */

static tree
build_template_decl (decl, parms)
     tree decl;
     tree parms;
{
  tree tmpl = build_lang_decl (TEMPLATE_DECL, DECL_NAME (decl), NULL_TREE);
  DECL_TEMPLATE_PARMS (tmpl) = parms;
  DECL_CONTEXT (tmpl) = DECL_CONTEXT (decl);
  if (DECL_LANG_SPECIFIC (decl))
    {
      DECL_VIRTUAL_CONTEXT (tmpl) = DECL_VIRTUAL_CONTEXT (decl);
      DECL_STATIC_FUNCTION_P (tmpl) = DECL_STATIC_FUNCTION_P (decl);
      DECL_CONSTRUCTOR_P (tmpl) = DECL_CONSTRUCTOR_P (decl);
      DECL_NONCONVERTING_P (tmpl) = DECL_NONCONVERTING_P (decl);
    }

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
     elements are non-zero if the parameter has been used in any one
     of the arguments processed so far.  */
  int* parms;

  /* An array whose size is the number of template arguments.  The
     elements are non-zero if the argument makes use of template
     parameters of this level.  */
  int* arg_uses_template_parms;
};

/* Subroutine of push_template_decl used to see if each template
   parameter in a partial specialization is used in the explicit
   argument list.  If T is of the LEVEL given in DATA (which is
   treated as a template_parm_data*), then DATA->PARMS is marked
   appropriately.  */

static int
mark_template_parm (t, data)
     tree t;
     void* data;
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
process_partial_specialization (decl)
     tree decl;
{
  tree type = TREE_TYPE (decl);
  tree maintmpl = CLASSTYPE_TI_TEMPLATE (type);
  tree specargs = CLASSTYPE_TI_ARGS (type);
  tree inner_args = innermost_args (specargs);
  tree inner_parms = INNERMOST_TEMPLATE_PARMS (current_template_parms);
  tree main_inner_parms = DECL_INNERMOST_TEMPLATE_PARMS (maintmpl);
  int nargs = TREE_VEC_LENGTH (inner_args);
  int ntparms = TREE_VEC_LENGTH (inner_parms);
  int  i;
  int did_error_intro = 0;
  struct template_parm_data tpd;
  struct template_parm_data tpd2;

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

     The S2<T> declaration is actually illegal; it is a
     full-specialization.  Of course, 

	  template <class U>
	  struct S2<T (*)(U)>;

     or some such would have been OK.  */
  tpd.level = TMPL_PARMS_DEPTH (current_template_parms);
  tpd.parms = alloca (sizeof (int) * ntparms);
  bzero ((PTR) tpd.parms, sizeof (int) * ntparms);

  tpd.arg_uses_template_parms = alloca (sizeof (int) * nargs);
  bzero ((PTR) tpd.arg_uses_template_parms, sizeof (int) * nargs);
  for (i = 0; i < nargs; ++i)
    {
      tpd.current_arg = i;
      for_each_template_parm (TREE_VEC_ELT (inner_args, i),
			      &mark_template_parm,
			      &tpd);
    }
  for (i = 0; i < ntparms; ++i)
    if (tpd.parms[i] == 0)
      {
	/* One of the template parms was not used in the
           specialization.  */
	if (!did_error_intro)
	  {
	    cp_error ("template parameters not used in partial specialization:");
	    did_error_intro = 1;
	  }

	cp_error ("        `%D'", 
		  TREE_VALUE (TREE_VEC_ELT (inner_parms, i)));
      }

  /* [temp.class.spec]

     The argument list of the specialization shall not be identical to
     the implicit argument list of the primary template.  */
  if (comp_template_args (inner_args, 
			  innermost_args (CLASSTYPE_TI_ARGS (TREE_TYPE
							     (maintmpl)))))
    cp_error ("partial specialization `%T' does not specialize any template arguments", type);

  /* [temp.class.spec]

     A partially specialized non-type argument expression shall not
     involve template parameters of the partial specialization except
     when the argument expression is a simple identifier.

     The type of a template parameter corresponding to a specialized
     non-type argument shall not be dependent on a parameter of the
     specialization.  */
  my_friendly_assert (nargs == DECL_NTPARMS (maintmpl), 0);
  tpd2.parms = 0;
  for (i = 0; i < nargs; ++i)
    {
      tree arg = TREE_VEC_ELT (inner_args, i);
      if (/* These first two lines are the `non-type' bit.  */
	  !TYPE_P (arg)
	  && TREE_CODE (arg) != TEMPLATE_DECL
	  /* This next line is the `argument expression is not just a
	     simple identifier' condition and also the `specialized
	     non-type argument' bit.  */
	  && TREE_CODE (arg) != TEMPLATE_PARM_INDEX)
	{
	  if (tpd.arg_uses_template_parms[i])
	    cp_error ("template argument `%E' involves template parameter(s)", arg);
	  else 
	    {
	      /* Look at the corresponding template parameter,
		 marking which template parameters its type depends
		 upon.  */
	      tree type = 
		TREE_TYPE (TREE_VALUE (TREE_VEC_ELT (main_inner_parms, 
						     i)));

	      if (!tpd2.parms)
		{
		  /* We haven't yet initialized TPD2.  Do so now.  */
		  tpd2.arg_uses_template_parms 
		    =  (int*) alloca (sizeof (int) * nargs);
		  /* The number of parameters here is the number in the
		     main template, which, as checked in the assertion
		     above, is NARGS.  */
		  tpd2.parms = (int*) alloca (sizeof (int) * nargs);
		  tpd2.level = 
		    TMPL_PARMS_DEPTH (DECL_TEMPLATE_PARMS (maintmpl));
		}

	      /* Mark the template parameters.  But this time, we're
		 looking for the template parameters of the main
		 template, not in the specialization.  */
	      tpd2.current_arg = i;
	      tpd2.arg_uses_template_parms[i] = 0;
	      bzero ((PTR) tpd2.parms, sizeof (int) * nargs);
	      for_each_template_parm (type,
				      &mark_template_parm,
				      &tpd2);
		  
	      if (tpd2.arg_uses_template_parms [i])
		{
		  /* The type depended on some template parameters.
		     If they are fully specialized in the
		     specialization, that's OK.  */
		  int j;
		  for (j = 0; j < nargs; ++j)
		    if (tpd2.parms[j] != 0
			&& tpd.arg_uses_template_parms [j])
		      {
			cp_error ("type `%T' of template argument `%E' depends on template parameter(s)", 
				  type,
				  arg);
			break;
		      }
		}
	    }
	}
    }

  if (retrieve_specialization (maintmpl, specargs))
    /* We've already got this specialization.  */
    return decl;

  DECL_TEMPLATE_SPECIALIZATIONS (maintmpl)
    = tree_cons (inner_args, inner_parms,
		 DECL_TEMPLATE_SPECIALIZATIONS (maintmpl));
  TREE_TYPE (DECL_TEMPLATE_SPECIALIZATIONS (maintmpl)) = type;
  return decl;
}

/* Check that a template declaration's use of default arguments is not
   invalid.  Here, PARMS are the template parameters.  IS_PRIMARY is
   non-zero if DECL is the thing declared by a primary template.
   IS_PARTIAL is non-zero if DECL is a partial specialization.  */

static void
check_default_tmpl_args (decl, parms, is_primary, is_partial)
     tree decl;
     tree parms;
     int is_primary;
     int is_partial;
{
  const char *msg;
  int last_level_to_check;
  tree parm_level;

  /* [temp.param] 

     A default template-argument shall not be specified in a
     function template declaration or a function template definition, nor
     in the template-parameter-list of the definition of a member of a
     class template.  */

  if (TREE_CODE (CP_DECL_CONTEXT (decl)) == FUNCTION_DECL)
    /* You can't have a function template declaration in a local
       scope, nor you can you define a member of a class template in a
       local scope.  */
    return;

  if (current_class_type
      && !TYPE_BEING_DEFINED (current_class_type)
      && DECL_LANG_SPECIFIC (decl)
      /* If this is either a friend defined in the scope of the class
	 or a member function.  */
      && ((DECL_CONTEXT (decl) 
	   && same_type_p (DECL_CONTEXT (decl), current_class_type))
	  || (DECL_FRIEND_CONTEXT (decl)
	      && same_type_p (DECL_FRIEND_CONTEXT (decl), 
			      current_class_type)))
      /* And, if it was a member function, it really was defined in
	 the scope of the class.  */
      && (!DECL_FUNCTION_MEMBER_P (decl) || DECL_DEFINED_IN_CLASS_P (decl)))
    /* We already checked these parameters when the template was
       declared, so there's no need to do it again now.  This function
       was defined in class scope, but we're processing it's body now
       that the class is complete.  */
    return;

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
	  if (TREE_PURPOSE (parm))
	    seen_def_arg_p = 1;
	  else if (seen_def_arg_p)
	    {
	      cp_error ("no default argument for `%D'", TREE_VALUE (parm));
	      /* For better subsequent error-recovery, we indicate that
		 there should have been a default argument.  */
	      TREE_PURPOSE (parm) = error_mark_node;
	    }
	}
    }

  if (TREE_CODE (decl) != TYPE_DECL || is_partial || !is_primary)
    /* For an ordinary class template, default template arguments are
       allowed at the innermost level, e.g.:
         template <class T = int>
	 struct S {};
       but, in a partial specialization, they're not allowed even
       there, as we have in [temp.class.spec]:
     
	 The template parameter list of a specialization shall not
	 contain default template argument values.  

       So, for a partial specialization, or for a function template,
       we look at all of them.  */
    ;
  else
    /* But, for a primary class template that is not a partial
       specialization we look at all template parameters except the
       innermost ones.  */
    parms = TREE_CHAIN (parms);

  /* Figure out what error message to issue.  */
  if (TREE_CODE (decl) == FUNCTION_DECL)
    msg = "default argument for template parameter in function template `%D'";
  else if (is_partial)
    msg = "default argument in partial specialization `%D'";
  else
    msg = "default argument for template parameter for class enclosing `%D'";

  if (current_class_type && TYPE_BEING_DEFINED (current_class_type))
    /* If we're inside a class definition, there's no need to
       examine the parameters to the class itself.  On the one
       hand, they will be checked when the class is defined, and,
       on the other, default arguments are legal in things like:
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
	if (TREE_PURPOSE (TREE_VEC_ELT (inner_parms, i)))
	  {
	    if (msg)
	      {
		cp_error (msg, decl);
		msg = 0;
	      }

	    /* Clear out the default argument so that we are not
	       confused later.  */
	    TREE_PURPOSE (TREE_VEC_ELT (inner_parms, i)) = NULL_TREE;
	  }

      /* At this point, if we're still interested in issuing messages,
	 they must apply to classes surrounding the object declared.  */
      if (msg)
	msg = "default argument for template parameter for class enclosing `%D'"; 
    }
}

/* Creates a TEMPLATE_DECL for the indicated DECL using the template
   parameters given by current_template_args, or reuses a
   previously existing one, if appropriate.  Returns the DECL, or an
   equivalent one, if it is replaced via a call to duplicate_decls.  

   If IS_FRIEND is non-zero, DECL is a friend declaration.  */

tree
push_template_decl_real (decl, is_friend)
     tree decl;
     int is_friend;
{
  tree tmpl;
  tree args;
  tree info;
  tree ctx;
  int primary;
  int is_partial;
  int new_template_p = 0;

  /* See if this is a partial specialization.  */
  is_partial = (DECL_IMPLICIT_TYPEDEF_P (decl)
		&& TREE_CODE (TREE_TYPE (decl)) != ENUMERAL_TYPE
		&& CLASSTYPE_PARTIAL_SPECIALIZATION (TREE_TYPE (decl)));

  is_friend |= (TREE_CODE (decl) == FUNCTION_DECL && DECL_FRIEND_P (decl));

  if (is_friend)
    /* For a friend, we want the context of the friend function, not
       the type of which it is a friend.  */
    ctx = DECL_CONTEXT (decl);
  else if (CP_DECL_CONTEXT (decl)
	   && TREE_CODE (CP_DECL_CONTEXT (decl)) != NAMESPACE_DECL)
    /* In the case of a virtual function, we want the class in which
       it is defined.  */
    ctx = CP_DECL_CONTEXT (decl);
  else
    /* Otherwise, if we're currently definining some class, the DECL
       is assumed to be a member of the class.  */
    ctx = current_scope ();

  if (ctx && TREE_CODE (ctx) == NAMESPACE_DECL)
    ctx = NULL_TREE;

  if (!DECL_CONTEXT (decl))
    DECL_CONTEXT (decl) = FROB_CONTEXT (current_namespace);

  /* See if this is a primary template.  */
  primary = template_parm_scope_p ();

  if (primary)
    {
      if (current_lang_name == lang_name_c)
	cp_error ("template with C linkage");
      else if (TREE_CODE (decl) == TYPE_DECL 
	       && ANON_AGGRNAME_P (DECL_NAME (decl))) 
	cp_error ("template class without a name");
      else if ((DECL_IMPLICIT_TYPEDEF_P (decl)
		&& CLASS_TYPE_P (TREE_TYPE (decl)))
	       || (TREE_CODE (decl) == VAR_DECL && ctx && CLASS_TYPE_P (ctx))
	       || TREE_CODE (decl) == FUNCTION_DECL)
	/* OK */;
      else
	cp_error ("template declaration of `%#D'", decl);
    }

  /* Check to see that the rules regarding the use of default
     arguments are not being violated.  */
  check_default_tmpl_args (decl, current_template_parms, 
			   primary, is_partial);

  if (is_partial)
    return process_partial_specialization (decl);

  args = current_template_args ();

  if (!ctx 
      || TREE_CODE (ctx) == FUNCTION_DECL
      || TYPE_BEING_DEFINED (ctx)
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
	     that the redeclaration is legal.  */
	  redeclare_class_template (TREE_TYPE (decl),
				    current_template_parms);
	  /* We don't need to create a new TEMPLATE_DECL; just use the
	     one we already had.  */
	  tmpl = TYPE_TI_TEMPLATE (TREE_TYPE (decl));
	}
      else
	{
	  tmpl = build_template_decl (decl, current_template_parms);
	  new_template_p = 1;

	  if (DECL_LANG_SPECIFIC (decl)
	      && DECL_TEMPLATE_SPECIALIZATION (decl))
	    {
	      /* A specialization of a member template of a template
		 class. */
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

      if (TREE_CODE (decl) == TYPE_DECL)
	{
	  if ((IS_AGGR_TYPE_CODE (TREE_CODE (TREE_TYPE (decl)))
	       || TREE_CODE (TREE_TYPE (decl)) == ENUMERAL_TYPE)
	      && TYPE_TEMPLATE_INFO (TREE_TYPE (decl))
	      && TYPE_TI_TEMPLATE (TREE_TYPE (decl)))
	    tmpl = TYPE_TI_TEMPLATE (TREE_TYPE (decl));
	  else
	    {
	      cp_error ("`%D' does not declare a template type", decl);
	      return decl;
	    }
	}
      else if (! DECL_TEMPLATE_INFO (decl))
	{
	  cp_error ("template definition of non-template `%#D'", decl);
	  return decl;
	}
      else
	tmpl = DECL_TI_TEMPLATE (decl);
      
      if (is_member_template (tmpl)
	  && DECL_FUNCTION_TEMPLATE_P (tmpl)
	  && DECL_TEMPLATE_INFO (decl) && DECL_TI_ARGS (decl) 
	  && DECL_TEMPLATE_SPECIALIZATION (decl))
	{
	  tree new_tmpl;

	  /* The declaration is a specialization of a member
	     template, declared outside the class.  Therefore, the
	     innermost template arguments will be NULL, so we
	     replace them with the arguments determined by the
	     earlier call to check_explicit_specialization.  */
	  args = DECL_TI_ARGS (decl);

	  new_tmpl 
	    = build_template_decl (decl, current_template_parms);
	  DECL_TEMPLATE_RESULT (new_tmpl) = decl;
	  TREE_TYPE (new_tmpl) = TREE_TYPE (decl);
	  DECL_TI_TEMPLATE (decl) = new_tmpl;
	  SET_DECL_TEMPLATE_SPECIALIZATION (new_tmpl);
	  DECL_TEMPLATE_INFO (new_tmpl) 
	    = tree_cons (tmpl, args, NULL_TREE);

	  register_specialization (new_tmpl, tmpl, args);
	  return decl;
	}

      /* Make sure the template headers we got make sense.  */

      parms = DECL_TEMPLATE_PARMS (tmpl);
      i = TMPL_PARMS_DEPTH (parms);
      if (TMPL_ARGS_DEPTH (args) != i)
	{
	  cp_error ("expected %d levels of template parms for `%#D', got %d",
		    i, decl, TMPL_ARGS_DEPTH (args));
	}
      else
	for (current = decl; i > 0; --i, parms = TREE_CHAIN (parms))
	  {
	    a = TMPL_ARGS_LEVEL (args, i);
	    t = INNERMOST_TEMPLATE_PARMS (parms);

	    if (TREE_VEC_LENGTH (t) != TREE_VEC_LENGTH (a))
	      {
		if (current == decl)
		  cp_error ("got %d template parameters for `%#D'",
			    TREE_VEC_LENGTH (a), decl);
		else
		  cp_error ("got %d template parameters for `%#T'",
			    TREE_VEC_LENGTH (a), current);
		cp_error ("  but %d required", TREE_VEC_LENGTH (t));
	      }

	    /* Perhaps we should also check that the parms are used in the
               appropriate qualifying scopes in the declarator?  */

	    if (current == decl)
	      current = ctx;
	    else
	      current = TYPE_CONTEXT (current);
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
    tmpl = pushdecl_namespace_level (tmpl);

  if (primary)
    DECL_PRIMARY_TEMPLATE (tmpl) = tmpl;

  info = tree_cons (tmpl, args, NULL_TREE);

  if (DECL_IMPLICIT_TYPEDEF_P (decl))
    {
      SET_TYPE_TEMPLATE_INFO (TREE_TYPE (tmpl), info);
      if ((!ctx || TREE_CODE (ctx) != FUNCTION_DECL)
	  && TREE_CODE (TREE_TYPE (decl)) != ENUMERAL_TYPE)
	DECL_NAME (decl) = classtype_mangled_name (TREE_TYPE (decl));
    }
  else if (DECL_LANG_SPECIFIC (decl))
    DECL_TEMPLATE_INFO (decl) = info;

  return DECL_TEMPLATE_RESULT (tmpl);
}

tree
push_template_decl (decl)
     tree decl;
{
  return push_template_decl_real (decl, 0);
}

/* Called when a class template TYPE is redeclared with the indicated
   template PARMS, e.g.:

     template <class T> struct S;
     template <class T> struct S {};  */

void 
redeclare_class_template (type, parms)
     tree type;
     tree parms;
{
  tree tmpl;
  tree tmpl_parms;
  int i;

  if (!TYPE_TEMPLATE_INFO (type))
    {
      cp_error ("`%T' is not a template type", type);
      return;
    }

  tmpl = TYPE_TI_TEMPLATE (type);
  if (!PRIMARY_TEMPLATE_P (tmpl))
    /* The type is nested in some template class.  Nothing to worry
       about here; there are no new template parameters for the nested
       type.  */
    return;

  parms = INNERMOST_TEMPLATE_PARMS (parms);
  tmpl_parms = DECL_INNERMOST_TEMPLATE_PARMS (tmpl);

  if (TREE_VEC_LENGTH (parms) != TREE_VEC_LENGTH (tmpl_parms))
    {
      cp_error_at ("previous declaration `%D'", tmpl);
      cp_error ("used %d template parameter%s instead of %d",
		TREE_VEC_LENGTH (tmpl_parms), 
		TREE_VEC_LENGTH (tmpl_parms) == 1 ? "" : "s",
		TREE_VEC_LENGTH (parms));
      return;
    }

  for (i = 0; i < TREE_VEC_LENGTH (tmpl_parms); ++i)
    {
      tree tmpl_parm = TREE_VALUE (TREE_VEC_ELT (tmpl_parms, i));
      tree parm = TREE_VALUE (TREE_VEC_ELT (parms, i));
      tree tmpl_default = TREE_PURPOSE (TREE_VEC_ELT (tmpl_parms, i));
      tree parm_default = TREE_PURPOSE (TREE_VEC_ELT (parms, i));

      if (TREE_CODE (tmpl_parm) != TREE_CODE (parm))
	{
	  cp_error_at ("template parameter `%#D'", tmpl_parm);
	  cp_error ("redeclared here as `%#D'", parm);
	  return;
	}

      if (tmpl_default != NULL_TREE && parm_default != NULL_TREE)
	{
	  /* We have in [temp.param]:

	     A template-parameter may not be given default arguments
	     by two different declarations in the same scope.  */
	  cp_error ("redefinition of default argument for `%#D'", parm);
	  cp_error_at ("  original definition appeared here", tmpl_parm);
	  return;
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
}

/* Attempt to convert the non-type template parameter EXPR to the
   indicated TYPE.  If the conversion is successful, return the
   converted value.  If the conversion is unsuccesful, return
   NULL_TREE if we issued an error message, or error_mark_node if we
   did not.  We issue error messages for out-and-out bad template
   parameters, but not simply because the conversion failed, since we
   might be just trying to do argument deduction.  By the time this
   function is called, neither TYPE nor EXPR may make use of template
   parameters.  */

static tree
convert_nontype_argument (type, expr)
     tree type;
     tree expr;
{
  tree expr_type = TREE_TYPE (expr);

  /* A template-argument for a non-type, non-template
     template-parameter shall be one of:

     --an integral constant-expression of integral or enumeration
     type; or
     
     --the name of a non-type template-parameter; or
     
     --the name of an object or function with external linkage,
     including function templates and function template-ids but
     excluding non-static class members, expressed as id-expression;
     or
     
     --the address of an object or function with external linkage,
     including function templates and function template-ids but
     excluding non-static class members, expressed as & id-expression
     where the & is optional if the name refers to a function or
     array; or
     
     --a pointer to member expressed as described in _expr.unary.op_.  */

  /* An integral constant-expression can include const variables or
     enumerators.  Simplify things by folding them to their values,
     unless we're about to bind the declaration to a reference
     parameter.  */
  if (INTEGRAL_TYPE_P (expr_type) && TREE_READONLY_DECL_P (expr)
      && TREE_CODE (type) != REFERENCE_TYPE)
    expr = decl_constant_value (expr);

  if (is_overloaded_fn (expr))
    /* OK for now.  We'll check that it has external linkage later.
       Check this first since if expr_type is the unknown_type_node
       we would otherwise complain below.  */
    ;
  else if (TYPE_PTRMEM_P (expr_type)
	   || TYPE_PTRMEMFUNC_P (expr_type))
    {
      if (TREE_CODE (expr) != PTRMEM_CST)
	goto bad_argument;
    }
  else if (TYPE_PTR_P (expr_type)
	   || TYPE_PTRMEM_P (expr_type)
	   || TREE_CODE (expr_type) == ARRAY_TYPE
	   || TREE_CODE (type) == REFERENCE_TYPE
	   /* If expr is the address of an overloaded function, we
	      will get the unknown_type_node at this point.  */
	   || expr_type == unknown_type_node)
    {
      tree referent;
      tree e = expr;
      STRIP_NOPS (e);

      if (TREE_CODE (type) == REFERENCE_TYPE
	  || TREE_CODE (expr_type) == ARRAY_TYPE)
	referent = e;
      else
	{
	  if (TREE_CODE (e) != ADDR_EXPR)
	    {
	    bad_argument:
	      cp_error ("`%E' is not a valid template argument", expr);
	      if (TYPE_PTR_P (expr_type))
		{
		  if (TREE_CODE (TREE_TYPE (expr_type)) == FUNCTION_TYPE)
		    cp_error ("it must be the address of a function with external linkage");
		  else
		    cp_error ("it must be the address of an object with external linkage");
		}
	      else if (TYPE_PTRMEM_P (expr_type)
		       || TYPE_PTRMEMFUNC_P (expr_type))
		cp_error ("it must be a pointer-to-member of the form `&X::Y'");

	      return NULL_TREE;
	    }

	  referent = TREE_OPERAND (e, 0);
	  STRIP_NOPS (referent);
	}

      if (TREE_CODE (referent) == STRING_CST)
	{
	  cp_error ("string literal %E is not a valid template argument because it is the address of an object with static linkage", 
		    referent);
	  return NULL_TREE;
	}

      if (is_overloaded_fn (referent))
	/* We'll check that it has external linkage later.  */
	;
      else if (TREE_CODE (referent) != VAR_DECL)
	goto bad_argument;
      else if (!TREE_PUBLIC (referent))
	{
	  cp_error ("address of non-extern `%E' cannot be used as template argument", referent); 
	  return error_mark_node;
	}
    }
  else if (INTEGRAL_TYPE_P (expr_type) 
	   || TYPE_PTRMEM_P (expr_type) 
	   || TYPE_PTRMEMFUNC_P (expr_type)
	   /* The next two are g++ extensions.  */
	   || TREE_CODE (expr_type) == REAL_TYPE
	   || TREE_CODE (expr_type) == COMPLEX_TYPE)
    {
      if (! TREE_CONSTANT (expr))
	{
	non_constant:
	  cp_error ("non-constant `%E' cannot be used as template argument",
		    expr);
	  return NULL_TREE;
	}
    }
  else 
    {
      cp_error ("object `%E' cannot be used as template argument", expr);
      return NULL_TREE;
    }

  switch (TREE_CODE (type))
    {
    case INTEGER_TYPE:
    case BOOLEAN_TYPE:
    case ENUMERAL_TYPE:
      /* For a non-type template-parameter of integral or enumeration
         type, integral promotions (_conv.prom_) and integral
         conversions (_conv.integral_) are applied. */
      if (!INTEGRAL_TYPE_P (expr_type))
	return error_mark_node;
      
      /* It's safe to call digest_init in this case; we know we're
	 just converting one integral constant expression to another.  */
      expr = digest_init (type, expr, (tree*) 0);

      if (TREE_CODE (expr) != INTEGER_CST)
	/* Curiously, some TREE_CONSTANT integral expressions do not
	   simplify to integer constants.  For example, `3 % 0',
	   remains a TRUNC_MOD_EXPR.  */
	goto non_constant;
      
      return expr;
	
    case REAL_TYPE:
    case COMPLEX_TYPE:
      /* These are g++ extensions.  */
      if (TREE_CODE (expr_type) != TREE_CODE (type))
	return error_mark_node;

      expr = digest_init (type, expr, (tree*) 0);
      
      if (TREE_CODE (expr) != REAL_CST)
	goto non_constant;

      return expr;

    case POINTER_TYPE:
      {
	tree type_pointed_to = TREE_TYPE (type);
 
	if (TYPE_PTRMEM_P (type))
	  {
	    tree e;

	    /* For a non-type template-parameter of type pointer to data
	       member, qualification conversions (_conv.qual_) are
	       applied.  */
	    e = perform_qualification_conversions (type, expr);
	    if (TREE_CODE (e) == NOP_EXPR)
	      /* The call to perform_qualification_conversions will
		 insert a NOP_EXPR over EXPR to do express conversion,
		 if necessary.  But, that will confuse us if we use
		 this (converted) template parameter to instantiate
		 another template; then the thing will not look like a
		 valid template argument.  So, just make a new
		 constant, of the appropriate type.  */
	      e = make_ptrmem_cst (type, PTRMEM_CST_MEMBER (expr));
	    return e;
	  }
	else if (TREE_CODE (type_pointed_to) == FUNCTION_TYPE)
	  { 
	    /* For a non-type template-parameter of type pointer to
	       function, only the function-to-pointer conversion
	       (_conv.func_) is applied.  If the template-argument
	       represents a set of overloaded functions (or a pointer to
	       such), the matching function is selected from the set
	       (_over.over_).  */
	    tree fns;
	    tree fn;

	    if (TREE_CODE (expr) == ADDR_EXPR)
	      fns = TREE_OPERAND (expr, 0);
	    else
	      fns = expr;

	    fn = instantiate_type (type_pointed_to, fns, 0);

	    if (fn == error_mark_node)
	      return error_mark_node;

	    if (!TREE_PUBLIC (fn))
	      {
		if (really_overloaded_fn (fns))
		  return error_mark_node;
		else
		  goto bad_argument;
	      }

	    expr = build_unary_op (ADDR_EXPR, fn, 0);

	    my_friendly_assert (same_type_p (type, TREE_TYPE (expr)), 
				0);
	    return expr;
	  }
	else 
	  {
	    /* For a non-type template-parameter of type pointer to
	       object, qualification conversions (_conv.qual_) and the
	       array-to-pointer conversion (_conv.array_) are applied.
	       [Note: In particular, neither the null pointer conversion
	       (_conv.ptr_) nor the derived-to-base conversion
	       (_conv.ptr_) are applied.  Although 0 is a valid
	       template-argument for a non-type template-parameter of
	       integral type, it is not a valid template-argument for a
	       non-type template-parameter of pointer type.]  
	    
	       The call to decay_conversion performs the
	       array-to-pointer conversion, if appropriate.  */
	    expr = decay_conversion (expr);

	    if (expr == error_mark_node)
	      return error_mark_node;
	    else
	      return perform_qualification_conversions (type, expr);
	  }
      }
      break;

    case REFERENCE_TYPE:
      {
	tree type_referred_to = TREE_TYPE (type);

	if (TREE_CODE (type_referred_to) == FUNCTION_TYPE)
	  {
	    /* For a non-type template-parameter of type reference to
	       function, no conversions apply.  If the
	       template-argument represents a set of overloaded
	       functions, the matching function is selected from the
	       set (_over.over_).  */
	    tree fns = expr;
	    tree fn;

	    fn = instantiate_type (type_referred_to, fns, 0);

	    if (fn == error_mark_node)
	      return error_mark_node;

	    if (!TREE_PUBLIC (fn))
	      {
		if (really_overloaded_fn (fns))
		  /* Don't issue an error here; we might get a different
		     function if the overloading had worked out
		     differently.  */
		  return error_mark_node;
		else
		  goto bad_argument;
	      }

	    my_friendly_assert (same_type_p (type_referred_to, 
					     TREE_TYPE (fn)),
				0);

	    return fn;
	  }
	else
	  {
	    /* For a non-type template-parameter of type reference to
	       object, no conversions apply.  The type referred to by the
	       reference may be more cv-qualified than the (otherwise
	       identical) type of the template-argument.  The
	       template-parameter is bound directly to the
	       template-argument, which must be an lvalue.  */
	    if ((TYPE_MAIN_VARIANT (expr_type)
		 != TYPE_MAIN_VARIANT (type_referred_to))
		|| !at_least_as_qualified_p (type_referred_to,
					     expr_type)
		|| !real_lvalue_p (expr))
	      return error_mark_node;
	    else
	      return expr;
	  }
      }
      break;

    case RECORD_TYPE:
      {
	if (!TYPE_PTRMEMFUNC_P (type))
	  /* This handles templates like
	       template<class T, T t> void f();
	     when T is substituted with any class.  The second template
	     parameter becomes invalid and the template candidate is
	     rejected.  */
	  return error_mark_node;

	/* For a non-type template-parameter of type pointer to member
	   function, no conversions apply.  If the template-argument
	   represents a set of overloaded member functions, the
	   matching member function is selected from the set
	   (_over.over_).  */

	if (!TYPE_PTRMEMFUNC_P (expr_type) && 
	    expr_type != unknown_type_node)
	  return error_mark_node;

	if (TREE_CODE (expr) == PTRMEM_CST)
	  {
	    /* A ptr-to-member constant.  */
	    if (!same_type_p (type, expr_type))
	      return error_mark_node;
	    else 
	      return expr;
	  }

	if (TREE_CODE (expr) != ADDR_EXPR)
	  return error_mark_node;

	expr = instantiate_type (type, expr, 0);
	
	if (expr == error_mark_node)
	  return error_mark_node;

	my_friendly_assert (same_type_p (type, TREE_TYPE (expr)),
			    0);
	return expr;
      }
      break;

    default:
      /* All non-type parameters must have one of these types.  */
      my_friendly_abort (0);
      break;
    }

  return error_mark_node;
}

/* Return 1 if PARM_PARMS and ARG_PARMS matches using rule for 
   template template parameters.  Both PARM_PARMS and ARG_PARMS are 
   vectors of TREE_LIST nodes containing TYPE_DECL, TEMPLATE_DECL 
   or PARM_DECL.
   
   ARG_PARMS may contain more parameters than PARM_PARMS.  If this is 
   the case, then extra parameters must have default arguments.

   Consider the example:
     template <class T, class Allocator = allocator> class vector;
     template<template <class U> class TT> class C;

   C<vector> is a valid instantiation.  PARM_PARMS for the above code 
   contains a TYPE_DECL (for U),  ARG_PARMS contains two TYPE_DECLs (for 
   T and Allocator) and OUTER_ARGS contains the argument that is used to 
   substitute the TT parameter.  */

static int
coerce_template_template_parms (parm_parms, arg_parms, complain, 
				in_decl, outer_args)
     tree parm_parms, arg_parms;
     int complain;
     tree in_decl, outer_args;
{
  int nparms, nargs, i;
  tree parm, arg;

  my_friendly_assert (TREE_CODE (parm_parms) == TREE_VEC, 0);
  my_friendly_assert (TREE_CODE (arg_parms) == TREE_VEC, 0);

  nparms = TREE_VEC_LENGTH (parm_parms);
  nargs = TREE_VEC_LENGTH (arg_parms);

  /* The rule here is opposite of coerce_template_parms.  */
  if (nargs < nparms
      || (nargs > nparms
	  && TREE_PURPOSE (TREE_VEC_ELT (arg_parms, nparms)) == NULL_TREE))
    return 0;

  for (i = 0; i < nparms; ++i)
    {
      parm = TREE_VALUE (TREE_VEC_ELT (parm_parms, i));
      arg = TREE_VALUE (TREE_VEC_ELT (arg_parms, i));

      if (arg == NULL_TREE || arg == error_mark_node
          || parm == NULL_TREE || parm == error_mark_node)
	return 0;

      if (TREE_CODE (arg) != TREE_CODE (parm))
        return 0;

      switch (TREE_CODE (parm))
	{
	case TYPE_DECL:
	  break;

	case TEMPLATE_DECL:
	  /* We encounter instantiations of templates like
	       template <template <template <class> class> class TT>
	       class C;  */
	  {
	    tree parmparm = DECL_INNERMOST_TEMPLATE_PARMS (parm);
	    tree argparm = DECL_INNERMOST_TEMPLATE_PARMS (arg);

	    if (!coerce_template_template_parms (parmparm, argparm, 
					         complain, in_decl,
						 outer_args))
	      return 0;
	  }
	  break;

	case PARM_DECL:
	  /* The tsubst call is used to handle cases such as
	       template <class T, template <T> class TT> class D;  
	     i.e. the parameter list of TT depends on earlier parameters.  */
	  if (!same_type_p (tsubst (TREE_TYPE (parm), outer_args, 
				    complain, in_decl),
			    TREE_TYPE (arg)))
	    return 0;
	  break;
	  
	default:
	  my_friendly_abort (0);
	}
    }
  return 1;
}

/* Convert the indicated template ARG as necessary to match the
   indicated template PARM.  Returns the converted ARG, or
   error_mark_node if the conversion was unsuccessful.  Error messages
   are issued if COMPLAIN is non-zero.  This conversion is for the Ith
   parameter in the parameter list.  ARGS is the full set of template
   arguments deduced so far.  */

static tree
convert_template_argument (parm, arg, args, complain, i, in_decl)
     tree parm;
     tree arg;
     tree args;
     int complain;
     int i;
     tree in_decl;
{
  tree val;
  tree inner_args;
  int is_type, requires_type, is_tmpl_type, requires_tmpl_type;
  
  inner_args = innermost_args (args);

  if (TREE_CODE (arg) == TREE_LIST 
      && TREE_TYPE (arg) != NULL_TREE
      && TREE_CODE (TREE_TYPE (arg)) == OFFSET_TYPE)
    {  
      /* The template argument was the name of some
	 member function.  That's usually
	 illegal, but static members are OK.  In any
	 case, grab the underlying fields/functions
	 and issue an error later if required.  */
      arg = TREE_VALUE (arg);
      TREE_TYPE (arg) = unknown_type_node;
    }

  requires_tmpl_type = TREE_CODE (parm) == TEMPLATE_DECL;
  requires_type = (TREE_CODE (parm) == TYPE_DECL
		   || requires_tmpl_type);

  /* Check if it is a class template.  If REQUIRES_TMPL_TYPE is true,
     we also accept implicitly created TYPE_DECL as a valid argument.
     This is necessary to handle the case where we pass a template name
     to a template template parameter in a scope where we've derived from
     in instantiation of that template, so the template name refers to that
     instantiation.  We really ought to handle this better.  */
  is_tmpl_type 
    = ((TREE_CODE (arg) == TEMPLATE_DECL
	&& TREE_CODE (DECL_TEMPLATE_RESULT (arg)) == TYPE_DECL)
       || (TREE_CODE (arg) == TEMPLATE_TEMPLATE_PARM
	   && !TEMPLATE_TEMPLATE_PARM_TEMPLATE_INFO (arg))
       || (TREE_CODE (arg) == RECORD_TYPE
	   && CLASSTYPE_TEMPLATE_INFO (arg)
	   && TREE_CODE (TYPE_NAME (arg)) == TYPE_DECL
	   && DECL_ARTIFICIAL (TYPE_NAME (arg))
	   && requires_tmpl_type
	   && is_base_of_enclosing_class (arg, current_class_type)));
  if (is_tmpl_type && TREE_CODE (arg) == TEMPLATE_TEMPLATE_PARM)
    arg = TYPE_STUB_DECL (arg);
  else if (is_tmpl_type && TREE_CODE (arg) == RECORD_TYPE)
    arg = CLASSTYPE_TI_TEMPLATE (arg);

  is_type = TYPE_P (arg) || is_tmpl_type;

  if (requires_type && ! is_type && TREE_CODE (arg) == SCOPE_REF
      && TREE_CODE (TREE_OPERAND (arg, 0)) == TEMPLATE_TYPE_PARM)
    {
      cp_pedwarn ("to refer to a type member of a template parameter, use `typename %E'", arg);
      
      arg = make_typename_type (TREE_OPERAND (arg, 0),
				TREE_OPERAND (arg, 1),
				complain);
      is_type = 1;
    }
  if (is_type != requires_type)
    {
      if (in_decl)
	{
	  if (complain)
	    {
	      cp_error ("type/value mismatch at argument %d in template parameter list for `%D'",
			i + 1, in_decl);
	      if (is_type)
		cp_error ("  expected a constant of type `%T', got `%T'",
			  TREE_TYPE (parm),
			  (is_tmpl_type ? DECL_NAME (arg) : arg));
	      else
		cp_error ("  expected a type, got `%E'", arg);
	    }
	}
      return error_mark_node;
    }
  if (is_tmpl_type ^ requires_tmpl_type)
    {
      if (in_decl && complain)
	{
	  cp_error ("type/value mismatch at argument %d in template parameter list for `%D'",
		    i + 1, in_decl);
	  if (is_tmpl_type)
	    cp_error ("  expected a type, got `%T'", DECL_NAME (arg));
	  else
	    cp_error ("  expected a class template, got `%T'", arg);
	}
      return error_mark_node;
    }
      
  if (is_type)
    {
      if (requires_tmpl_type)
	{
	  tree parmparm = DECL_INNERMOST_TEMPLATE_PARMS (parm);
	  tree argparm = DECL_INNERMOST_TEMPLATE_PARMS (arg);

	  if (coerce_template_template_parms (parmparm, argparm, complain,
					      in_decl, inner_args))
	    {
	      val = arg;
		  
	      /* TEMPLATE_TEMPLATE_PARM node is preferred over 
		 TEMPLATE_DECL.  */
	      if (val != error_mark_node 
		  && DECL_TEMPLATE_TEMPLATE_PARM_P (val))
		val = TREE_TYPE (val);
	    }
	  else
	    {
	      if (in_decl && complain)
		{
		  cp_error ("type/value mismatch at argument %d in template parameter list for `%D'",
			    i + 1, in_decl);
		  cp_error ("  expected a template of type `%D', got `%D'", parm, arg);
		}
		  
	      val = error_mark_node;
	    }
	}
      else
	{
	  val = groktypename (arg);
	  if (! processing_template_decl)
	    {
	      /* [basic.link]: A name with no linkage (notably, the
		 name of a class or enumeration declared in a local
		 scope) shall not be used to declare an entity with
		 linkage.  This implies that names with no linkage
		 cannot be used as template arguments.  */
	      tree t = no_linkage_check (val);
	      if (t)
		{
		  if (ANON_AGGRNAME_P (TYPE_IDENTIFIER (t)))
		    cp_pedwarn
		      ("template-argument `%T' uses anonymous type", val);
		  else
		    cp_error
		      ("template-argument `%T' uses local type `%T'",
		       val, t);
		  return error_mark_node;
		}
	    }
	}
    }
  else
    {
      tree t = tsubst (TREE_TYPE (parm), args, complain, in_decl);

      if (processing_template_decl)
	arg = maybe_fold_nontype_arg (arg);

      if (!uses_template_parms (arg) && !uses_template_parms (t))
	/* We used to call digest_init here.  However, digest_init
	   will report errors, which we don't want when complain
	   is zero.  More importantly, digest_init will try too
	   hard to convert things: for example, `0' should not be
	   converted to pointer type at this point according to
	   the standard.  Accepting this is not merely an
	   extension, since deciding whether or not these
	   conversions can occur is part of determining which
	   function template to call, or whether a given epxlicit
	   argument specification is legal.  */
	val = convert_nontype_argument (t, arg);
      else
	val = arg;

      if (val == NULL_TREE)
	val = error_mark_node;
      else if (val == error_mark_node && complain)
	cp_error ("could not convert template argument `%E' to `%T'", 
		  arg, t);
    }

  return val;
}

/* Convert all template arguments to their appropriate types, and
   return a vector containing the innermost resulting template
   arguments.  If any error occurs, return error_mark_node, and, if
   COMPLAIN is non-zero, issue an error message.  Some error messages
   are issued even if COMPLAIN is zero; for instance, if a template
   argument is composed from a local class.

   If REQUIRE_ALL_ARGUMENTS is non-zero, all arguments must be
   provided in ARGLIST, or else trailing parameters must have default
   values.  If REQUIRE_ALL_ARGUMENTS is zero, we will attempt argument
   deduction for any unspecified trailing arguments.  

   The resulting TREE_VEC is allocated on a temporary obstack, and
   must be explicitly copied if it will be permanent.  */
   
static tree
coerce_template_parms (parms, args, in_decl,
		       complain,
		       require_all_arguments)
     tree parms, args;
     tree in_decl;
     int complain;
     int require_all_arguments;
{
  int nparms, nargs, i, lost = 0;
  tree inner_args;
  tree new_args;
  tree new_inner_args;

  inner_args = innermost_args (args);
  nargs = NUM_TMPL_ARGS (inner_args);
  nparms = TREE_VEC_LENGTH (parms);

  if (nargs > nparms
      || (nargs < nparms
	  && require_all_arguments
	  && TREE_PURPOSE (TREE_VEC_ELT (parms, nargs)) == NULL_TREE))
    {
      if (complain) 
	{
	  cp_error ("wrong number of template arguments (%d, should be %d)",
		    nargs, nparms);
	  
	  if (in_decl)
	    cp_error_at ("provided for `%D'", in_decl);
	}

      return error_mark_node;
    }

  new_inner_args = make_tree_vec (nparms);
  new_args = add_outermost_template_args (args, new_inner_args);
  for (i = 0; i < nparms; i++)
    {
      tree arg;
      tree parm;

      /* Get the Ith template parameter.  */
      parm = TREE_VEC_ELT (parms, i);

      /* Calculate the Ith argument.  */
      if (inner_args && TREE_CODE (inner_args) == TREE_LIST)
	{
	  arg = TREE_VALUE (inner_args);
	  inner_args = TREE_CHAIN (inner_args);
	}
      else if (i < nargs)
	arg = TREE_VEC_ELT (inner_args, i);
      /* If no template argument was supplied, look for a default
	 value.  */
      else if (TREE_PURPOSE (parm) == NULL_TREE)
	{
	  /* There was no default value.  */
	  my_friendly_assert (!require_all_arguments, 0);
	  break;
	}
      else if (TREE_CODE (TREE_VALUE (parm)) == TYPE_DECL)
	arg = tsubst (TREE_PURPOSE (parm), new_args, complain, in_decl);
      else
	arg = tsubst_expr (TREE_PURPOSE (parm), new_args, complain,
			   in_decl);

      /* Now, convert the Ith argument, as necessary.  */
      if (arg == NULL_TREE)
	/* We're out of arguments.  */
	{
	  my_friendly_assert (!require_all_arguments, 0);
	  break;
	}
      else if (arg == error_mark_node)
	{
	  cp_error ("template argument %d is invalid", i + 1);
	  arg = error_mark_node;
	}
      else 
	arg = convert_template_argument (TREE_VALUE (parm), 
					 arg, new_args, complain, i,
					 in_decl); 
      
      if (arg == error_mark_node)
	lost++;
      TREE_VEC_ELT (new_inner_args, i) = arg;
    }

  if (lost)
    return error_mark_node;

  return new_inner_args;
}

/* Returns 1 if template args OT and NT are equivalent.  */

static int
template_args_equal (ot, nt)
     tree ot, nt;
{
  if (nt == ot)
    return 1;
  if (TREE_CODE (nt) != TREE_CODE (ot))
    return 0;
  if (TREE_CODE (nt) == TREE_VEC)
    /* For member templates */
    return comp_template_args (ot, nt);
  else if (TYPE_P (ot))
    return same_type_p (ot, nt);
  else
    return (cp_tree_equal (ot, nt) > 0);
}

/* Returns 1 iff the OLDARGS and NEWARGS are in fact identical sets
   of template arguments.  Returns 0 otherwise.  */

int
comp_template_args (oldargs, newargs)
     tree oldargs, newargs;
{
  int i;

  if (TREE_VEC_LENGTH (oldargs) != TREE_VEC_LENGTH (newargs))
    return 0;

  for (i = 0; i < TREE_VEC_LENGTH (oldargs); ++i)
    {
      tree nt = TREE_VEC_ELT (newargs, i);
      tree ot = TREE_VEC_ELT (oldargs, i);

      if (! template_args_equal (ot, nt))
	return 0;
    }
  return 1;
}

/* Given class template name and parameter list, produce a user-friendly name
   for the instantiation.  */

static char *
mangle_class_name_for_template (name, parms, arglist)
     char *name;
     tree parms, arglist;
{
  static struct obstack scratch_obstack;
  static char *scratch_firstobj;
  int i, nparms;

  if (!scratch_firstobj)
    gcc_obstack_init (&scratch_obstack);
  else
    obstack_free (&scratch_obstack, scratch_firstobj);
  scratch_firstobj = obstack_alloc (&scratch_obstack, 1);

#define ccat(c)	obstack_1grow (&scratch_obstack, (c));
#define cat(s)	obstack_grow (&scratch_obstack, (s), strlen (s))

  cat (name);
  ccat ('<');
  nparms = TREE_VEC_LENGTH (parms);
  arglist = innermost_args (arglist);
  my_friendly_assert (nparms == TREE_VEC_LENGTH (arglist), 268);
  for (i = 0; i < nparms; i++)
    {
      tree parm = TREE_VALUE (TREE_VEC_ELT (parms, i));
      tree arg = TREE_VEC_ELT (arglist, i);

      if (i)
	ccat (',');

      if (TREE_CODE (parm) == TYPE_DECL)
	{
	  cat (type_as_string (arg, TS_CHASE_TYPEDEFS));
	  continue;
	}
      else if (TREE_CODE (parm) == TEMPLATE_DECL)
	{
	  if (TREE_CODE (arg) == TEMPLATE_DECL)
	    {
	      /* Already substituted with real template.  Just output 
		 the template name here */
              tree context = DECL_CONTEXT (arg);
              if (context)
                {
                  /* The template may be defined in a namespace, or
                     may be a member template.  */
                  my_friendly_assert (TREE_CODE (context) == NAMESPACE_DECL
                                      || CLASS_TYPE_P (context), 
                                      980422);
		  cat(decl_as_string (DECL_CONTEXT (arg), 0));
		  cat("::");
		}
	      cat (IDENTIFIER_POINTER (DECL_NAME (arg)));
	    }
	  else
	    /* Output the parameter declaration */
	    cat (type_as_string (arg, TS_CHASE_TYPEDEFS));
	  continue;
	}
      else
	my_friendly_assert (TREE_CODE (parm) == PARM_DECL, 269);

      if (TREE_CODE (arg) == TREE_LIST)
	{
	  /* New list cell was built because old chain link was in
	     use.  */
	  my_friendly_assert (TREE_PURPOSE (arg) == NULL_TREE, 270);
	  arg = TREE_VALUE (arg);
	}
      /* No need to check arglist against parmlist here; we did that
	 in coerce_template_parms, called from lookup_template_class.  */
      cat (expr_as_string (arg, 0));
    }
  {
    char *bufp = obstack_next_free (&scratch_obstack);
    int offset = 0;
    while (bufp[offset - 1] == ' ')
      offset--;
    obstack_blank_fast (&scratch_obstack, offset);

    /* B<C<char> >, not B<C<char>> */
    if (bufp[offset - 1] == '>')
      ccat (' ');
  }
  ccat ('>');
  ccat ('\0');
  return (char *) obstack_base (&scratch_obstack);
}

static tree
classtype_mangled_name (t)
     tree t;
{
  if (CLASSTYPE_TEMPLATE_INFO (t)
      /* Specializations have already had their names set up in
	 lookup_template_class.  */
      && !CLASSTYPE_PARTIAL_SPECIALIZATION (t))
    {
      tree tmpl = most_general_template (CLASSTYPE_TI_TEMPLATE (t));

      /* For non-primary templates, the template parameters are
	 implicit from their surrounding context.  */
      if (PRIMARY_TEMPLATE_P (tmpl))
	{
	  tree name = DECL_NAME (tmpl);
	  char *mangled_name = mangle_class_name_for_template
	    (IDENTIFIER_POINTER (name), 
	     DECL_INNERMOST_TEMPLATE_PARMS (tmpl),
	     CLASSTYPE_TI_ARGS (t));
	  tree id = get_identifier (mangled_name);
	  IDENTIFIER_TEMPLATE (id) = name;
	  return id;
	}
    }

  return TYPE_IDENTIFIER (t);
}

static void
add_pending_template (d)
     tree d;
{
  tree ti = (TYPE_P (d)) ? CLASSTYPE_TEMPLATE_INFO (d) : DECL_TEMPLATE_INFO (d);

  if (TI_PENDING_TEMPLATE_FLAG (ti))
    return;

  *template_tail = tree_cons (build_srcloc_here (), d, NULL_TREE);
  template_tail = &TREE_CHAIN (*template_tail);
  TI_PENDING_TEMPLATE_FLAG (ti) = 1;
}


/* Return a TEMPLATE_ID_EXPR corresponding to the indicated FNS (which
   may be either a _DECL or an overloaded function or an
   IDENTIFIER_NODE), and ARGLIST.  */

tree
lookup_template_function (fns, arglist)
     tree fns, arglist;
{
  tree type;

  if (fns == NULL_TREE)
    {
      cp_error ("non-template used as template");
      return error_mark_node;
    }

  type = TREE_TYPE (fns);
  if (TREE_CODE (fns) == OVERLOAD || !type)
    type = unknown_type_node;

  if (processing_template_decl)
    return build_min (TEMPLATE_ID_EXPR, type, fns, arglist);  
  else
    return build (TEMPLATE_ID_EXPR, type, fns, arglist);
}

/* Within the scope of a template class S<T>, the name S gets bound
   (in build_self_reference) to a TYPE_DECL for the class, not a
   TEMPLATE_DECL.  If DECL is a TYPE_DECL for current_class_type,
   or one of its enclosing classes, and that type is a template,
   return the associated TEMPLATE_DECL.  Otherwise, the original
   DECL is returned.  */

static tree
maybe_get_template_decl_from_type_decl (decl)
     tree decl;
{
  return (decl != NULL_TREE
	  && TREE_CODE (decl) == TYPE_DECL 
	  && DECL_ARTIFICIAL (decl)
	  && CLASS_TYPE_P (TREE_TYPE (decl))
	  && CLASSTYPE_TEMPLATE_INFO (TREE_TYPE (decl))) 
    ? CLASSTYPE_TI_TEMPLATE (TREE_TYPE (decl)) : decl;
}

/* Given an IDENTIFIER_NODE (type TEMPLATE_DECL) and a chain of
   parameters, find the desired type.

   D1 is the PTYPENAME terminal, and ARGLIST is the list of arguments.
   (Actually ARGLIST may be either a TREE_LIST or a TREE_VEC.  It will
   be a TREE_LIST if called directly from the parser, and a TREE_VEC
   otherwise.)  Since ARGLIST is build on the temp_decl_obstack, we must
   copy it here to keep it from being reclaimed when the decl storage
   is reclaimed.

   IN_DECL, if non-NULL, is the template declaration we are trying to
   instantiate.  

   If ENTERING_SCOPE is non-zero, we are about to enter the scope of
   the class we are looking up.

   If the template class is really a local class in a template
   function, then the FUNCTION_CONTEXT is the function in which it is
   being instantiated.  */

tree
lookup_template_class (d1, arglist, in_decl, context, entering_scope)
     tree d1, arglist;
     tree in_decl;
     tree context;
     int entering_scope;
{
  tree template = NULL_TREE, parmlist;
  tree t;

  if (TREE_CODE (d1) == IDENTIFIER_NODE)
    {
      if (IDENTIFIER_VALUE (d1) 
	  && DECL_TEMPLATE_TEMPLATE_PARM_P (IDENTIFIER_VALUE (d1)))
	template = IDENTIFIER_VALUE (d1);
      else
	{
	  if (context)
	    push_decl_namespace (context);
	  template = lookup_name (d1, /*prefer_type=*/0);
	  template = maybe_get_template_decl_from_type_decl (template);
	  if (context)
	    pop_decl_namespace ();
	}
      if (template)
	context = DECL_CONTEXT (template);
    }
  else if (TREE_CODE (d1) == TYPE_DECL && IS_AGGR_TYPE (TREE_TYPE (d1)))
    {
      tree type = TREE_TYPE (d1);

      /* If we are declaring a constructor, say A<T>::A<T>, we will get
	 an implicit typename for the second A.  Deal with it.  */
      if (TREE_CODE (type) == TYPENAME_TYPE && TREE_TYPE (type))
	type = TREE_TYPE (type);
	
      if (CLASSTYPE_TEMPLATE_INFO (type))
	{
	  template = CLASSTYPE_TI_TEMPLATE (type);
	  d1 = DECL_NAME (template);
	}
    }
  else if (TREE_CODE (d1) == ENUMERAL_TYPE 
	   || (TYPE_P (d1) && IS_AGGR_TYPE (d1)))
    {
      template = TYPE_TI_TEMPLATE (d1);
      d1 = DECL_NAME (template);
    }
  else if (TREE_CODE (d1) == TEMPLATE_DECL
	   && TREE_CODE (DECL_RESULT (d1)) == TYPE_DECL)
    {
      template = d1;
      d1 = DECL_NAME (template);
      context = DECL_CONTEXT (template);
    }
  else
    my_friendly_abort (272);

  /* With something like `template <class T> class X class X { ... };'
     we could end up with D1 having nothing but an IDENTIFIER_VALUE.
     We don't want to do that, but we have to deal with the situation,
     so let's give them some syntax errors to chew on instead of a
     crash.  */
  if (! template)
    {
      cp_error ("`%T' is not a template", d1);
      return error_mark_node;
    }

  if (context == NULL_TREE)
    context = global_namespace;

  if (TREE_CODE (template) != TEMPLATE_DECL)
    {
      cp_error ("non-template type `%T' used as a template", d1);
      if (in_decl)
	cp_error_at ("for template declaration `%D'", in_decl);
      return error_mark_node;
    }

  if (DECL_TEMPLATE_TEMPLATE_PARM_P (template))
    {
      /* Create a new TEMPLATE_DECL and TEMPLATE_TEMPLATE_PARM node to store
         template arguments */

      tree parm = copy_template_template_parm (TREE_TYPE (template));
      tree template2 = TYPE_STUB_DECL (parm);
      tree arglist2;

      parmlist = DECL_INNERMOST_TEMPLATE_PARMS (template);

      arglist2 = coerce_template_parms (parmlist, arglist, template, 1, 1);
      if (arglist2 == error_mark_node)
	return error_mark_node;

      TEMPLATE_TEMPLATE_PARM_TEMPLATE_INFO (parm)
	= tree_cons (template2, arglist2, NULL_TREE);
      TYPE_SIZE (parm) = 0;
      return parm;
    }
  else 
    {
      tree template_type = TREE_TYPE (template);
      tree gen_tmpl;
      tree type_decl;
      tree found = NULL_TREE;
      int arg_depth;
      int parm_depth;
      int is_partial_instantiation;

      gen_tmpl = most_general_template (template);
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
	    = add_outermost_template_args (TYPE_TI_ARGS (TREE_TYPE (template)),
					   arglist);
	  arg_depth = TMPL_ARGS_DEPTH (arglist);
	}

      /* Now we should enough arguments.  */
      my_friendly_assert (parm_depth == arg_depth, 0);
      
      /* From here on, we're only interested in the most general
	 template.  */
      template = gen_tmpl;

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
		 t = DECL_TEMPLATE_PARMS (template); 
	       i > 0 && t != NULL_TREE;
	       --i, t = TREE_CHAIN (t))
	    {
	      tree a = coerce_template_parms (TREE_VALUE (t),
					      arglist, template, 1, 1);
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
				   innermost_args (arglist),
				   template, 1, 1);

      if (arglist == error_mark_node)
	/* We were unable to bind the arguments.  */
	return error_mark_node;

      /* In the scope of a template class, explicit references to the
	 template class refer to the type of the template, not any
	 instantiation of it.  For example, in:
	 
	   template <class T> class C { void f(C<T>); }

	 the `C<T>' is just the same as `C'.  Outside of the
	 class, however, such a reference is an instantiation.  */
      if (comp_template_args (TYPE_TI_ARGS (template_type),
			      arglist))
	{
	  found = template_type;
	  
	  if (!entering_scope && PRIMARY_TEMPLATE_P (template))
	    {
	      tree ctx;
	      
	      /* Note that we use DECL_CONTEXT, rather than
		 CP_DECL_CONTEXT, so that the termination test is
		 always just `ctx'.  We're not interested in namepace
		 scopes.  */
	      for (ctx = current_class_type; 
		   ctx; 
		   ctx = (TYPE_P (ctx)) ? TYPE_CONTEXT (ctx) : DECL_CONTEXT (ctx))
		if (same_type_p (ctx, template_type))
		  break;
	      
	      if (!ctx)
		/* We're not in the scope of the class, so the
		   TEMPLATE_TYPE is not the type we want after
		   all.  */
		found = NULL_TREE;
	    }
	}
      
      if (!found)
	{
	  for (found = DECL_TEMPLATE_INSTANTIATIONS (template);
	       found; found = TREE_CHAIN (found))
	    if (comp_template_args (TREE_PURPOSE (found), arglist))
	      break;

	  if (found)
	    found = TREE_VALUE (found);
	}

      if (found)
	return found;

      /* This type is a "partial instantiation" if any of the template
	 arguments still inolve template parameters.  Note that we set
	 IS_PARTIAL_INSTANTIATION for partial specializations as
	 well.  */
      is_partial_instantiation = uses_template_parms (arglist);

      if (!is_partial_instantiation 
	  && !PRIMARY_TEMPLATE_P (template)
	  && TREE_CODE (CP_DECL_CONTEXT (template)) == NAMESPACE_DECL)
	{
	  found = xref_tag_from_type (TREE_TYPE (template),
				      DECL_NAME (template),
				      /*globalize=*/1);
	  return found;
	}
      
      /* Create the type.  */
      if (TREE_CODE (template_type) == ENUMERAL_TYPE)
	{
	  if (!is_partial_instantiation)
	    t = start_enum (TYPE_IDENTIFIER (template_type));
	  else
	    /* We don't want to call start_enum for this type, since
	       the values for the enumeration constants may involve
	       template parameters.  And, no one should be interested
	       in the enumeration constants for such a type.  */
	    t = make_node (ENUMERAL_TYPE);
	}
      else
	{
	  t = make_aggr_type (TREE_CODE (template_type));
	  CLASSTYPE_DECLARED_CLASS (t) 
	    = CLASSTYPE_DECLARED_CLASS (template_type);
	  CLASSTYPE_GOT_SEMICOLON (t) = 1;
	  SET_CLASSTYPE_IMPLICIT_INSTANTIATION (t);
	  TYPE_FOR_JAVA (t) = TYPE_FOR_JAVA (template_type);

	  /* A local class.  Make sure the decl gets registered properly.  */
	  if (context == current_function_decl)
	    pushtag (DECL_NAME (template), t, 0);
	}

      /* If we called start_enum or pushtag above, this information
	 will already be set up.  */
      if (!TYPE_NAME (t))
	{
	  TYPE_CONTEXT (t) = FROB_CONTEXT (context);
	  
	  type_decl = create_implicit_typedef (DECL_NAME (template), t);
	  DECL_CONTEXT (type_decl) = TYPE_CONTEXT (t);
	  TYPE_STUB_DECL (t) = type_decl;
	  DECL_SOURCE_FILE (type_decl) 
	    = DECL_SOURCE_FILE (TYPE_STUB_DECL (template_type));
	  DECL_SOURCE_LINE (type_decl) 
	    = DECL_SOURCE_LINE (TYPE_STUB_DECL (template_type));
	}
      else
	type_decl = TYPE_NAME (t);

      /* Set up the template information.  We have to figure out which
	 template is the immediate parent if this is a full
	 instantiation.  */
      if (parm_depth == 1 || is_partial_instantiation
	  || !PRIMARY_TEMPLATE_P (template))
	/* This case is easy; there are no member templates involved.  */
	found = template;
      else
	{
	  /* This is a full instantiation of a member template.  There
	     should be some partial instantiation of which this is an
	     instance.  */

	  for (found = DECL_TEMPLATE_INSTANTIATIONS (template);
	       found; found = TREE_CHAIN (found))
	    {
	      int success;
	      tree tmpl = CLASSTYPE_TI_TEMPLATE (TREE_VALUE (found));

	      /* We only want partial instantiations, here, not
		 specializations or full instantiations.  */
	      if (CLASSTYPE_PARTIAL_SPECIALIZATION (TREE_VALUE (found))
		  || !uses_template_parms (TREE_VALUE (found)))
		continue;

	      /* Temporarily reduce by one the number of levels in the
		 ARGLIST and in FOUND so as to avoid comparing the
		 last set of arguments.  */
	      TREE_VEC_LENGTH (arglist)--;
	      TREE_VEC_LENGTH (TREE_PURPOSE (found)) --;

	      /* See if the arguments match.  If they do, then TMPL is
		 the partial instantiation we want.  */
	      success = comp_template_args (TREE_PURPOSE (found), arglist);

	      /* Restore the argument vectors to their full size.  */
	      TREE_VEC_LENGTH (arglist)++;
	      TREE_VEC_LENGTH (TREE_PURPOSE (found))++;

	      if (success)
		{
		  found = tmpl;
		  break;
		}
	    }

	  if (!found)
	    my_friendly_abort (0);
	}

      SET_TYPE_TEMPLATE_INFO (t,
			      tree_cons (found, arglist, NULL_TREE));  
      DECL_TEMPLATE_INSTANTIATIONS (template) 
	= tree_cons (arglist, t, 
		     DECL_TEMPLATE_INSTANTIATIONS (template));

      if (TREE_CODE (t) == ENUMERAL_TYPE 
	  && !is_partial_instantiation)
	/* Now that the type has been registered on the instantiations
	   list, we set up the enumerators.  Because the enumeration
	   constants may involve the enumeration type itself, we make
	   sure to register the type first, and then create the
	   constants.  That way, doing tsubst_expr for the enumeration
	   constants won't result in recursive calls here; we'll find
	   the instantiation and exit above.  */
	tsubst_enum (template_type, t, arglist);

      /* Reset the name of the type, now that CLASSTYPE_TEMPLATE_INFO
	 is set up.  */
      if (TREE_CODE (t) != ENUMERAL_TYPE)
	DECL_NAME (type_decl) = classtype_mangled_name (t);
      DECL_ASSEMBLER_NAME (type_decl) = DECL_NAME (type_decl);
      if (!is_partial_instantiation)
	{
	  DECL_ASSEMBLER_NAME (type_decl)
	    = get_identifier (build_overload_name (t, 1, 1));

	  /* For backwards compatibility; code that uses
	     -fexternal-templates expects looking up a template to
	     instantiate it.  I think DDD still relies on this.
	     (jason 8/20/1998) */
	  if (TREE_CODE (t) != ENUMERAL_TYPE
	      && flag_external_templates
	      && CLASSTYPE_INTERFACE_KNOWN (TREE_TYPE (template))
	      && ! CLASSTYPE_INTERFACE_ONLY (TREE_TYPE (template)))
	    add_pending_template (t);
	}
      else
	/* If the type makes use of template parameters, the
	   code that generates debugging information will crash.  */
	DECL_IGNORED_P (TYPE_STUB_DECL (t)) = 1;

      return t;
    }
}

struct pair_fn_data 
{
  tree_fn_t fn;
  void *data;
};

/* Called from for_each_template_parm via walk_tree.  */

static tree
for_each_template_parm_r (tp, walk_subtrees, d)
     tree *tp;
     int *walk_subtrees;
     void *d;
{
  tree t = *tp;
  struct pair_fn_data *pfd = (struct pair_fn_data *) d;
  tree_fn_t fn = pfd->fn;
  void *data = pfd->data;
  
  if (TYPE_P (t)
      && for_each_template_parm (TYPE_CONTEXT (t), fn, data))
    return error_mark_node;

  switch (TREE_CODE (t))
    {
    case RECORD_TYPE:
      if (TYPE_PTRMEMFUNC_FLAG (t))
	break;
      /* Fall through.  */

    case UNION_TYPE:
    case ENUMERAL_TYPE:
      if (!TYPE_TEMPLATE_INFO (t))
	*walk_subtrees = 0;
      else if (for_each_template_parm (TREE_VALUE (TYPE_TEMPLATE_INFO (t)),
				       fn, data))
	return error_mark_node;
      break;

    case METHOD_TYPE:
      /* Since we're not going to walk subtrees, we have to do this
	 explicitly here.  */
      if (for_each_template_parm (TYPE_METHOD_BASETYPE (t), fn, data))
	return error_mark_node;

    case FUNCTION_TYPE:
      /* Check the return type.  */
      if (for_each_template_parm (TREE_TYPE (t), fn, data))
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
	  if (for_each_template_parm (TREE_VALUE (parm), fn, data))
	    return error_mark_node;

	/* Since we've already handled the TYPE_ARG_TYPES, we don't
	   want walk_tree walking into them itself.  */
	*walk_subtrees = 0;
      }
      break;

    case FUNCTION_DECL:
    case VAR_DECL:
      if (DECL_LANG_SPECIFIC (t) && DECL_TEMPLATE_INFO (t)
	  && for_each_template_parm (DECL_TI_ARGS (t), fn, data))
	return error_mark_node;
      /* Fall through.  */

    case CONST_DECL:
    case PARM_DECL:
      if (DECL_CONTEXT (t) 
	  && for_each_template_parm (DECL_CONTEXT (t), fn, data))
	return error_mark_node;
      break;

    case TEMPLATE_TEMPLATE_PARM:
      /* Record template parameters such as `T' inside `TT<T>'.  */
      if (TEMPLATE_TEMPLATE_PARM_TEMPLATE_INFO (t)
	  && for_each_template_parm (TYPE_TI_ARGS (t), fn, data))
	return error_mark_node;
      /* Fall through.  */

    case TEMPLATE_TYPE_PARM:
    case TEMPLATE_PARM_INDEX:
      if (fn && (*fn)(t, data))
	return error_mark_node;
      else if (!fn)
	return error_mark_node;
      break;

    case TEMPLATE_DECL:
      /* A template template parameter is encountered */
      if (DECL_TEMPLATE_TEMPLATE_PARM_P (t)
	  && for_each_template_parm (TREE_TYPE (t), fn, data))
	return error_mark_node;

      /* Already substituted template template parameter */
      *walk_subtrees = 0;
      break;

    case TYPENAME_TYPE:
      if (!fn || for_each_template_parm (TYPENAME_TYPE_FULLNAME (t), fn, data))
	return error_mark_node;
      break;

    case CONSTRUCTOR:
      if (TREE_TYPE (t) && TYPE_PTRMEMFUNC_P (TREE_TYPE (t))
	  && for_each_template_parm (TYPE_PTRMEMFUNC_FN_TYPE
				     (TREE_TYPE (t)), fn, data))
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
    case REINTERPRET_CAST_EXPR:
    case CONST_CAST_EXPR:
    case STATIC_CAST_EXPR:
    case DYNAMIC_CAST_EXPR:
    case ARROW_EXPR:
    case DOTSTAR_EXPR:
    case TYPEID_EXPR:
    case LOOKUP_EXPR:
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

/* For each TEMPLATE_TYPE_PARM, TEMPLATE_TEMPLATE_PARM, or
   TEMPLATE_PARM_INDEX in T, call FN with the parameter and the DATA.
   If FN returns non-zero, the iteration is terminated, and
   for_each_template_parm returns 1.  Otherwise, the iteration
   continues.  If FN never returns a non-zero value, the value
   returned by for_each_template_parm is 0.  If FN is NULL, it is
   considered to be the function which always returns 1.  */

static int
for_each_template_parm (t, fn, data)
     tree t;
     tree_fn_t fn;
     void* data;
{
  struct pair_fn_data pfd;

  /* Set up.  */
  pfd.fn = fn;
  pfd.data = data;

  /* Walk the tree.  */
  return walk_tree (&t, for_each_template_parm_r, &pfd) != NULL_TREE;
}

int
uses_template_parms (t)
     tree t;
{
  return for_each_template_parm (t, 0, 0);
}

static struct tinst_level *current_tinst_level;
static struct tinst_level *free_tinst_level;
static int tinst_depth;
extern int max_tinst_depth;
#ifdef GATHER_STATISTICS
int depth_reached;
#endif
static int tinst_level_tick;
static int last_template_error_tick;

/* Print out all the template instantiations that we are currently
   working on.  If ERR, we are being called from cp_thing, so do
   the right thing for an error message.  */

static void
print_template_context (err)
     int err;
{
  struct tinst_level *p = current_tinst_level;
  int line = lineno;
  char *file = input_filename;

  if (err && p)
    {
      if (current_function_decl != p->decl
	  && current_function_decl != NULL_TREE)
	/* We can get here during the processing of some synthesized
	   method.  Then, p->decl will be the function that's causing
	   the synthesis.  */
	;
      else
	{
	  if (current_function_decl == p->decl)
	    /* Avoid redundancy with the the "In function" line.  */;
	  else 
	    fprintf (stderr, "%s: In instantiation of `%s':\n",
		     file, decl_as_string (p->decl, TS_DECL_TYPE | TS_FUNC_NORETURN));
	  
	  line = p->line;
	  file = p->file;
	  p = p->next;
	}
    }

  for (; p; p = p->next)
    {
      fprintf (stderr, "%s:%d:   instantiated from `%s'\n", file, line,
	       decl_as_string (p->decl, TS_DECL_TYPE | TS_FUNC_NORETURN));
      line = p->line;
      file = p->file;
    }
  fprintf (stderr, "%s:%d:   instantiated from here\n", file, line);
}

/* Called from cp_thing to print the template context for an error.  */

void
maybe_print_template_context ()
{
  if (last_template_error_tick == tinst_level_tick
      || current_tinst_level == 0)
    return;

  last_template_error_tick = tinst_level_tick;
  print_template_context (1);
}

static int
push_tinst_level (d)
     tree d;
{
  struct tinst_level *new;

  if (tinst_depth >= max_tinst_depth)
    {
      /* If the instantiation in question still has unbound template parms,
	 we don't really care if we can't instantiate it, so just return.
         This happens with base instantiation for implicit `typename'.  */
      if (uses_template_parms (d))
	return 0;

      last_template_error_tick = tinst_level_tick;
      cp_error ("template instantiation depth exceeds maximum of %d (use -ftemplate-depth-NN to increase the maximum) instantiating `%D'",
	     max_tinst_depth, d);

      print_template_context (0);

      return 0;
    }

  if (free_tinst_level)
    {
      new = free_tinst_level;
      free_tinst_level = new->next;
    }
  else
    new = (struct tinst_level *) xmalloc (sizeof (struct tinst_level));

  new->decl = d;
  new->line = lineno;
  new->file = input_filename;
  new->next = current_tinst_level;
  current_tinst_level = new;

  ++tinst_depth;
#ifdef GATHER_STATISTICS
  if (tinst_depth > depth_reached)
    depth_reached = tinst_depth;
#endif

  ++tinst_level_tick;
  return 1;
}

void
pop_tinst_level ()
{
  struct tinst_level *old = current_tinst_level;

  /* Restore the filename and line number stashed away when we started
     this instantiation.  */
  lineno = old->line;
  input_filename = old->file;
  extract_interface_info ();
  
  current_tinst_level = old->next;
  old->next = free_tinst_level;
  free_tinst_level = old;
  --tinst_depth;
  ++tinst_level_tick;
}

struct tinst_level *
tinst_for_decl ()
{
  struct tinst_level *p = current_tinst_level;

  if (p)
    for (; p->next ; p = p->next )
      ;
  return p;
}

/* DECL is a friend FUNCTION_DECL or TEMPLATE_DECL.  ARGS is the
   vector of template arguments, as for tsubst.

   Returns an appropriate tsbust'd friend declaration.  */

static tree
tsubst_friend_function (decl, args)
     tree decl;
     tree args;
{
  tree new_friend;
  int line = lineno;
  char *file = input_filename;

  lineno = DECL_SOURCE_LINE (decl);
  input_filename = DECL_SOURCE_FILE (decl);

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
      tree template_id;
      tree new_args;
      tree tmpl;

      template_id
	= lookup_template_function (tsubst_expr (DECL_TI_TEMPLATE (decl),
						 args, /*complain=*/1, 
						 NULL_TREE),
				    tsubst (DECL_TI_ARGS (decl),
					    args, /*complain=*/1, 
					    NULL_TREE));
      new_friend = tsubst (decl, args, /*complain=*/1, NULL_TREE);
      tmpl = determine_specialization (template_id, new_friend,
				       &new_args, 
				       /*need_member_template=*/0);
      new_friend = instantiate_template (tmpl, new_args);
      goto done;
    }

  new_friend = tsubst (decl, args, /*complain=*/1, NULL_TREE);
	
  /* The NEW_FRIEND will look like an instantiation, to the
     compiler, but is not an instantiation from the point of view of
     the language.  For example, we might have had:
     
     template <class T> struct S {
       template <class U> friend void f(T, U);
     };
     
     Then, in S<int>, template <class U> void f(int, U) is not an
     instantiation of anything.  */
  DECL_USE_TEMPLATE (new_friend) = 0;
  if (TREE_CODE (decl) == TEMPLATE_DECL)
    DECL_USE_TEMPLATE (DECL_TEMPLATE_RESULT (new_friend)) = 0;

  /* The mangled name for the NEW_FRIEND is incorrect.  The call to
     tsubst will have resulted in a call to
     set_mangled_name_for_template_decl.  But, the function is not a
     template instantiation and should not be mangled like one.
     Therefore, we remangle the function name.  We don't have to do
     this if the NEW_FRIEND is a template since
     set_mangled_name_for_template_decl doesn't do anything if the
     function declaration still uses template arguments.  */
  if (TREE_CODE (new_friend) != TEMPLATE_DECL)
    {
      set_mangled_name_for_decl (new_friend);
      DECL_RTL (new_friend) = 0;
      make_decl_rtl (new_friend, NULL_PTR, 1);
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
      if (TREE_CODE (new_friend) == TEMPLATE_DECL)
	{
	  /* This declaration is a `primary' template.  */
	  DECL_PRIMARY_TEMPLATE (new_friend) = new_friend;
	  
	  new_friend_is_defn 
	    = DECL_INITIAL (DECL_RESULT (new_friend)) != NULL_TREE;
	  new_friend_result_template_info
	    = DECL_TEMPLATE_INFO (DECL_RESULT (new_friend));
	}
      else
	{
	  new_friend_is_defn = DECL_INITIAL (new_friend) != NULL_TREE;
	  new_friend_result_template_info = NULL_TREE;
	}

      /* Inside pushdecl_namespace_level, we will push into the
	 current namespace. However, the friend function should go
	 into the namespace of the template. */
      ns = decl_namespace_context (new_friend);
      push_nested_namespace (ns);
      old_decl = pushdecl_namespace_level (new_friend);
      pop_nested_namespace (ns);

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
	      /* Overwrite whatever template info was there before, if
		 any, with the new template information pertaining to
		 the declaration.  */
	      DECL_TEMPLATE_INFO (old_decl) = new_friend_template_info;

	      if (TREE_CODE (old_decl) != TEMPLATE_DECL)
		/* duplicate_decls will take care of this case.  */
		;
	      else 
		{
		  tree t;
		  tree new_friend_args;

		  DECL_TEMPLATE_INFO (DECL_RESULT (old_decl)) 
		    = new_friend_result_template_info;
		    
		  new_friend_args = TI_ARGS (new_friend_template_info);
		  for (t = DECL_TEMPLATE_SPECIALIZATIONS (old_decl); 
		       t != NULL_TREE;
		       t = TREE_CHAIN (t))
		    {
		      tree spec = TREE_VALUE (t);
		  
		      DECL_TI_ARGS (spec) 
			= add_outermost_template_args (new_friend_args,
						       DECL_TI_ARGS (spec));
		    }

		  /* Now, since specializations are always supposed to
		     hang off of the most general template, we must move
		     them.  */
		  t = most_general_template (old_decl);
		  if (t != old_decl)
		    {
		      DECL_TEMPLATE_SPECIALIZATIONS (t)
			= chainon (DECL_TEMPLATE_SPECIALIZATIONS (t),
				   DECL_TEMPLATE_SPECIALIZATIONS (old_decl));
		      DECL_TEMPLATE_SPECIALIZATIONS (old_decl) = NULL_TREE;
		    }
		}
	    }

	  /* The information from NEW_FRIEND has been merged into OLD_DECL
	     by duplicate_decls.  */
	  new_friend = old_decl;
	}
    }
  else if (COMPLETE_TYPE_P (DECL_CONTEXT (new_friend)))
    {
      /* Check to see that the declaration is really present, and,
	 possibly obtain an improved declaration.  */
      tree fn = check_classfn (DECL_CONTEXT (new_friend),
			       new_friend);
      
      if (fn)
	new_friend = fn;
    }

 done:
  lineno = line;
  input_filename = file;
  return new_friend;
}

/* FRIEND_TMPL is a friend TEMPLATE_DECL.  ARGS is the vector of
   template arguments, as for tsubst.

   Returns an appropriate tsbust'd friend type.  */

static tree
tsubst_friend_class (friend_tmpl, args)
     tree friend_tmpl;
     tree args;
{
  tree friend_type;
  tree tmpl;

  /* First, we look for a class template.  */
  tmpl = lookup_name (DECL_NAME (friend_tmpl), /*prefer_type=*/0); 
  
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
      tmpl = lookup_name (DECL_NAME (friend_tmpl), /*prefer_type=*/1);
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
      tree parms 
	= tsubst_template_parms (DECL_TEMPLATE_PARMS (friend_tmpl),
				 args, /*complain=*/1);
      redeclare_class_template (TREE_TYPE (tmpl), parms);
      friend_type = TREE_TYPE (tmpl);
    }
  else
    {
      /* The friend template has not already been declared.  In this
	 case, the instantiation of the template class will cause the
	 injection of this template into the global scope.  */
      tmpl = tsubst (friend_tmpl, args, /*complain=*/1, NULL_TREE);

      /* The new TMPL is not an instantiation of anything, so we
 	 forget its origins.  We don't reset CLASSTYPE_TI_TEMPLATE for
	 the new type because that is supposed to be the corresponding
	 template decl, i.e., TMPL.  */
      DECL_USE_TEMPLATE (tmpl) = 0;
      DECL_TEMPLATE_INFO (tmpl) = NULL_TREE;
      CLASSTYPE_USE_TEMPLATE (TREE_TYPE (tmpl)) = 0;

      /* Inject this template into the global scope.  */
      friend_type = TREE_TYPE (pushdecl_top_level (tmpl));
    }

  return friend_type;
}

tree
instantiate_class_template (type)
     tree type;
{
  tree template, args, pattern, t;
  tree typedecl;

  if (type == error_mark_node)
    return error_mark_node;

  if (TYPE_BEING_DEFINED (type) || COMPLETE_TYPE_P (type))
    return type;

  /* Figure out which template is being instantiated.  */
  template = most_general_template (CLASSTYPE_TI_TEMPLATE (type));
  my_friendly_assert (TREE_CODE (template) == TEMPLATE_DECL, 279);

  /* Figure out which arguments are being used to do the
     instantiation.  */
  args = CLASSTYPE_TI_ARGS (type);
  PARTIAL_INSTANTIATION_P (type) = uses_template_parms (args);

  if (pedantic && PARTIAL_INSTANTIATION_P (type))
    /* If this is a partial instantiation, then we can't instantiate
       the type; there's no telling whether or not one of the
       template parameters might eventually be instantiated to some
       value that results in a specialization being used.  For
       example, consider:

         template <class T>
         struct S {};

         template <class U> 
         void f(S<U>);
	     
         template <> 
         struct S<int> {};

       Now, the `S<U>' in `f<int>' is the specialization, not an
       instantiation of the original template.  */
    return type;

  /* Determine what specialization of the original template to
     instantiate.  */
  if (PARTIAL_INSTANTIATION_P (type))
    /* There's no telling which specialization is appropriate at this
       point.  Since all peeking at the innards of this partial
       instantiation are extensions (like the "implicit typename"
       extension, which allows users to omit the keyword `typename' on
       names that are declared as types in template base classes), we
       are free to do what we please.

       Trying to figure out which partial instantiation to use can
       cause a crash.  (Some of the template arguments don't even have
       types.)  So, we just use the most general version.  */
    t = NULL_TREE;
  else
    {
      t = most_specialized_class (template, args);

      if (t == error_mark_node)
	{
	  const char *str = "candidates are:";
	  cp_error ("ambiguous class template instantiation for `%#T'", type);
	  for (t = DECL_TEMPLATE_SPECIALIZATIONS (template); t; 
	       t = TREE_CHAIN (t))
	    {
	      if (get_class_bindings (TREE_VALUE (t), TREE_PURPOSE (t),
				      args))
		{
		  cp_error_at ("%s %+#T", str, TREE_TYPE (t));
		  str = "               ";
		}
	    }
	  TYPE_BEING_DEFINED (type) = 1;
	  return error_mark_node;
	}
    }

  if (t)
    pattern = TREE_TYPE (t);
  else
    pattern = TREE_TYPE (template);

  /* If the template we're instantiating is incomplete, then clearly
     there's nothing we can do.  */
  if (!COMPLETE_TYPE_P (pattern))
    return type;

  /* If this is a partial instantiation, don't tsubst anything.  We will
     only use this type for implicit typename, so the actual contents don't
     matter.  All that matters is whether a particular name is a type.  */
  if (PARTIAL_INSTANTIATION_P (type))
    {
      /* The fields set here must be kept in sync with those cleared
	 in begin_class_definition.  */
      TYPE_BINFO_BASETYPES (type) = TYPE_BINFO_BASETYPES (pattern);
      TYPE_FIELDS (type) = TYPE_FIELDS (pattern);
      TYPE_METHODS (type) = TYPE_METHODS (pattern);
      CLASSTYPE_TAGS (type) = CLASSTYPE_TAGS (pattern);
      /* Pretend that the type is complete, so that we will look
	 inside it during name lookup and such.  */
      TYPE_SIZE (type) = integer_zero_node;
      return type;
    }

  /* If we've recursively instantiated too many templates, stop.  */
  if (! push_tinst_level (type))
    return type;

  /* Now we're really doing the instantiation.  Mark the type as in
     the process of being defined.  */
  TYPE_BEING_DEFINED (type) = 1;

  maybe_push_to_top_level (uses_template_parms (type));

  if (t)
    {
      /* This TYPE is actually a instantiation of of a partial
	 specialization.  We replace the innermost set of ARGS with
	 the arguments appropriate for substitution.  For example,
	 given:

	   template <class T> struct S {};
	   template <class T> struct S<T*> {};
	 
	 and supposing that we are instantiating S<int*>, ARGS will
	 present be {int*} but we need {int}.  */
      tree inner_args 
	= get_class_bindings (TREE_VALUE (t), TREE_PURPOSE (t),
			      args);

      /* If there were multiple levels in ARGS, replacing the
	 innermost level would alter CLASSTYPE_TI_ARGS, which we don't
	 want, so we make a copy first.  */
      if (TMPL_ARGS_HAVE_MULTIPLE_LEVELS (args))
	{
	  args = copy_node (args);
	  SET_TMPL_ARGS_LEVEL (args, TMPL_ARGS_DEPTH (args), inner_args);
	}
      else
	args = inner_args;
    }

  if (flag_external_templates)
    {
      if (flag_alt_external_templates)
	{
	  CLASSTYPE_INTERFACE_ONLY (type) = interface_only;
	  SET_CLASSTYPE_INTERFACE_UNKNOWN_X (type, interface_unknown);
	  CLASSTYPE_VTABLE_NEEDS_WRITING (type)
	    = (! CLASSTYPE_INTERFACE_ONLY (type)
	       && CLASSTYPE_INTERFACE_KNOWN (type));
	}
      else
	{
	  CLASSTYPE_INTERFACE_ONLY (type) = CLASSTYPE_INTERFACE_ONLY (pattern);
	  SET_CLASSTYPE_INTERFACE_UNKNOWN_X
	    (type, CLASSTYPE_INTERFACE_UNKNOWN (pattern));
	  CLASSTYPE_VTABLE_NEEDS_WRITING (type)
	    = (! CLASSTYPE_INTERFACE_ONLY (type)
	       && CLASSTYPE_INTERFACE_KNOWN (type));
	}
    }
  else
    {
      SET_CLASSTYPE_INTERFACE_UNKNOWN (type);
      CLASSTYPE_VTABLE_NEEDS_WRITING (type) = 1;
    }

  TYPE_HAS_CONSTRUCTOR (type) = TYPE_HAS_CONSTRUCTOR (pattern);
  TYPE_HAS_DESTRUCTOR (type) = TYPE_HAS_DESTRUCTOR (pattern);
  TYPE_OVERLOADS_CALL_EXPR (type) = TYPE_OVERLOADS_CALL_EXPR (pattern);
  TYPE_OVERLOADS_ARRAY_REF (type) = TYPE_OVERLOADS_ARRAY_REF (pattern);
  TYPE_OVERLOADS_ARROW (type) = TYPE_OVERLOADS_ARROW (pattern);
  TYPE_HAS_NEW_OPERATOR (type) = TYPE_HAS_NEW_OPERATOR (pattern);
  TYPE_HAS_ARRAY_NEW_OPERATOR (type) = TYPE_HAS_ARRAY_NEW_OPERATOR (pattern);
  TYPE_GETS_DELETE (type) = TYPE_GETS_DELETE (pattern);
  TYPE_VEC_DELETE_TAKES_SIZE (type) = TYPE_VEC_DELETE_TAKES_SIZE (pattern);
  TYPE_HAS_ASSIGN_REF (type) = TYPE_HAS_ASSIGN_REF (pattern);
  TYPE_HAS_CONST_ASSIGN_REF (type) = TYPE_HAS_CONST_ASSIGN_REF (pattern);
  TYPE_HAS_ABSTRACT_ASSIGN_REF (type) = TYPE_HAS_ABSTRACT_ASSIGN_REF (pattern);
  TYPE_HAS_INIT_REF (type) = TYPE_HAS_INIT_REF (pattern);
  TYPE_HAS_CONST_INIT_REF (type) = TYPE_HAS_CONST_INIT_REF (pattern);
  TYPE_HAS_DEFAULT_CONSTRUCTOR (type) = TYPE_HAS_DEFAULT_CONSTRUCTOR (pattern);
  TYPE_HAS_CONVERSION (type) = TYPE_HAS_CONVERSION (pattern);
  TYPE_BASE_CONVS_MAY_REQUIRE_CODE_P (type)
    = TYPE_BASE_CONVS_MAY_REQUIRE_CODE_P (pattern);
  TYPE_USES_MULTIPLE_INHERITANCE (type)
    = TYPE_USES_MULTIPLE_INHERITANCE (pattern);
  TYPE_USES_VIRTUAL_BASECLASSES (type)
    = TYPE_USES_VIRTUAL_BASECLASSES (pattern);
  TYPE_PACKED (type) = TYPE_PACKED (pattern);
  TYPE_ALIGN (type) = TYPE_ALIGN (pattern);
  TYPE_FOR_JAVA (type) = TYPE_FOR_JAVA (pattern); /* For libjava's JArray<T> */
  if (ANON_AGGR_TYPE_P (pattern))
    SET_ANON_AGGR_TYPE_P (type);

  if (TYPE_BINFO_BASETYPES (pattern))
    {
      tree base_list = NULL_TREE;
      tree pbases = TYPE_BINFO_BASETYPES (pattern);
      int i;

      /* Substitute into each of the bases to determine the actual
	 basetypes.  */
      for (i = 0; i < TREE_VEC_LENGTH (pbases); ++i)
	{
	  tree base;
	  tree access;
	  tree pbase;

	  pbase = TREE_VEC_ELT (pbases, i);

	  /* Substitue to figure out the base class.  */
	  base = tsubst (BINFO_TYPE (pbase), args, 
			 /*complain=*/1, NULL_TREE);
	  if (base == error_mark_node)
	    continue;

	  /* Calculate the correct access node.  */
	  if (TREE_VIA_VIRTUAL (pbase)) 
	    {
	      if (TREE_VIA_PUBLIC (pbase))
		access = access_public_virtual_node;
	      else if (TREE_VIA_PROTECTED (pbase))
		access = access_protected_virtual_node;
	      else 
		access = access_private_virtual_node;
	    }
	  else
	    {
	      if (TREE_VIA_PUBLIC (pbase))
		access = access_public_node;
	      else if (TREE_VIA_PROTECTED (pbase))
		access = access_protected_node;
	      else 
		access = access_private_node;
	    }

	  base_list = tree_cons (access, base, base_list);
	}

      /* The list is now in reverse order; correct that.  */
      base_list = nreverse (base_list);

      /* Now call xref_basetypes to set up all the base-class
	 information.  */
      xref_basetypes (TREE_CODE (pattern) == RECORD_TYPE
		      ? (CLASSTYPE_DECLARED_CLASS (pattern)
			 ? class_type_node : record_type_node)
		      : union_type_node,
		      DECL_NAME (TYPE_NAME (pattern)),
		      type,
		      base_list);
    }

  /* Now that our base classes are set up, enter the scope of the
     class, so that name lookups into base classes, etc. will work
     corectly.  This is precisely analagous to what we do in
     begin_class_definition when defining an ordinary non-template
     class.  */
  pushclass (type, 1);

  for (t = CLASSTYPE_TAGS (pattern); t; t = TREE_CHAIN (t))
    {
      tree tag = TREE_VALUE (t);
      tree name = TYPE_IDENTIFIER (tag);
      tree newtag;

      newtag = tsubst (tag, args, /*complain=*/1, NULL_TREE);
      if (TREE_CODE (newtag) != ENUMERAL_TYPE)
	{
	  if (TYPE_LANG_SPECIFIC (tag) && CLASSTYPE_IS_TEMPLATE (tag))
	    /* Unfortunately, lookup_template_class sets
	       CLASSTYPE_IMPLICIT_INSTANTIATION for a partial
	       instantiation (i.e., for the type of a member template
	       class nested within a template class.)  This behavior is
	       required for maybe_process_partial_specialization to work
	       correctly, but is not accurate in this case; the TAG is not
	       an instantiation of anything.  (The corresponding
	       TEMPLATE_DECL is an instantiation, but the TYPE is not.) */
	    CLASSTYPE_USE_TEMPLATE (newtag) = 0;

	  /* Now, we call pushtag to put this NEWTAG into the scope of
	     TYPE.  We first set up the IDENTIFIER_TYPE_VALUE to avoid
	     pushtag calling push_template_decl.  We don't have to do
	     this for enums because it will already have been done in
	     tsubst_enum.  */
	  if (name)
	    SET_IDENTIFIER_TYPE_VALUE (name, newtag);
	  pushtag (name, newtag, /*globalize=*/0);
	}
    }

  /* Don't replace enum constants here.  */
  for (t = TYPE_FIELDS (pattern); t; t = TREE_CHAIN (t))
    if (TREE_CODE (t) != CONST_DECL)
      {
	tree r;

	/* The the file and line for this declaration, to assist in
	   error message reporting.  Since we called push_tinst_level
	   above, we don't need to restore these.  */
	lineno = DECL_SOURCE_LINE (t);
	input_filename = DECL_SOURCE_FILE (t);

	r = tsubst (t, args, /*complain=*/1, NULL_TREE);
	if (TREE_CODE (r) == VAR_DECL)
	  {
	    tree init;

	    if (DECL_DEFINED_IN_CLASS_P (r))
	      init = tsubst_expr (DECL_INITIAL (t), args,
				  /*complain=*/1, NULL_TREE);
	    else
	      init = NULL_TREE;

	    finish_static_data_member_decl (r, init,
					    /*asmspec_tree=*/NULL_TREE, 
					    /*flags=*/0);

	    if (DECL_DEFINED_IN_CLASS_P (r))
	      check_static_variable_definition (r, TREE_TYPE (r));
	  }
	
	/* R will have a TREE_CHAIN if and only if it has already been
	   processed by finish_member_declaration.  This can happen
	   if, for example, it is a TYPE_DECL for a class-scoped
	   ENUMERAL_TYPE; such a thing will already have been added to
	   the field list by tsubst_enum above.  */
	if (!TREE_CHAIN (r))
	  {
	    set_current_access_from_decl (r);
	    finish_member_declaration (r);
	  }
      }

  /* Set up the list (TYPE_METHODS) and vector (CLASSTYPE_METHOD_VEC)
     for this instantiation.  */
  for (t = TYPE_METHODS (pattern); t; t = TREE_CHAIN (t))
    {
      tree r = tsubst (t, args, /*complain=*/1, NULL_TREE);
      set_current_access_from_decl (r);
      finish_member_declaration (r);
    }

  /* Construct the DECL_FRIENDLIST for the new class type.  */
  typedecl = TYPE_MAIN_DECL (type);
  for (t = DECL_FRIENDLIST (TYPE_MAIN_DECL (pattern));
       t != NULL_TREE;
       t = TREE_CHAIN (t))
    {
      tree friends;

      for (friends = TREE_VALUE (t);
	   friends != NULL_TREE;
	   friends = TREE_CHAIN (friends))
	if (TREE_PURPOSE (friends) == error_mark_node)
	  add_friend (type, 
		      tsubst_friend_function (TREE_VALUE (friends),
					      args));
	else
	  my_friendly_abort (20000216);
    }

  for (t = CLASSTYPE_FRIEND_CLASSES (pattern);
       t != NULL_TREE;
       t = TREE_CHAIN (t))
    {
      tree friend_type = TREE_VALUE (t);
      tree new_friend_type;

      if (TREE_CODE (friend_type) == TEMPLATE_DECL)
	new_friend_type = tsubst_friend_class (friend_type, args);
      else if (uses_template_parms (friend_type))
	new_friend_type = tsubst (friend_type, args, /*complain=*/1,
				  NULL_TREE);
      else 
	{
	  tree ns = decl_namespace_context (TYPE_MAIN_DECL (friend_type));

	  /* The call to xref_tag_from_type does injection for friend
	     classes.  */
	  push_nested_namespace (ns);
	  new_friend_type = 
	    xref_tag_from_type (friend_type, NULL_TREE, 1);
	  pop_nested_namespace (ns);
	}

      if (TREE_CODE (friend_type) == TEMPLATE_DECL)
	/* Trick make_friend_class into realizing that the friend
	   we're adding is a template, not an ordinary class.  It's
	   important that we use make_friend_class since it will
	   perform some error-checking and output cross-reference
	   information.  */
	++processing_template_decl;

      make_friend_class (type, new_friend_type);

      if (TREE_CODE (friend_type) == TEMPLATE_DECL)
	--processing_template_decl;
    }

  for (t = TYPE_FIELDS (type); t; t = TREE_CHAIN (t))
    if (TREE_CODE (t) == FIELD_DECL)
      {
	TREE_TYPE (t) = complete_type (TREE_TYPE (t));
	require_complete_type (t);
      }

  /* Set the file and line number information to whatever is given for
     the class itself.  This puts error messages involving generated
     implicit functions at a predictable point, and the same point
     that would be used for non-template classes.  */
  lineno = DECL_SOURCE_LINE (typedecl);
  input_filename = DECL_SOURCE_FILE (typedecl);

  unreverse_member_declarations (type);
  finish_struct_1 (type);
  CLASSTYPE_GOT_SEMICOLON (type) = 1;

  /* Clear this now so repo_template_used is happy.  */
  TYPE_BEING_DEFINED (type) = 0;
  repo_template_used (type);

  /* Now that the class is complete, instantiate default arguments for
     any member functions.  We don't do this earlier because the
     default arguments may reference members of the class.  */
  if (!PRIMARY_TEMPLATE_P (template))
    for (t = TYPE_METHODS (type); t; t = TREE_CHAIN (t))
      if (TREE_CODE (t) == FUNCTION_DECL 
	  /* Implicitly generated member functions will not have tmplate
	     information; they are not instantiations, but instead are
	     created "fresh" for each instantiation.  */
	  && DECL_TEMPLATE_INFO (t))
	tsubst_default_arguments (t);

  popclass ();
  pop_from_top_level ();
  pop_tinst_level ();

  return type;
}

static int
list_eq (t1, t2)
     tree t1, t2;
{
  if (t1 == NULL_TREE)
    return t2 == NULL_TREE;
  if (t2 == NULL_TREE)
    return 0;
  /* Don't care if one declares its arg const and the other doesn't -- the
     main variant of the arg type is all that matters.  */
  if (TYPE_MAIN_VARIANT (TREE_VALUE (t1))
      != TYPE_MAIN_VARIANT (TREE_VALUE (t2)))
    return 0;
  return list_eq (TREE_CHAIN (t1), TREE_CHAIN (t2));
}

/* If arg is a non-type template parameter that does not depend on template
   arguments, fold it like we weren't in the body of a template.  */

static tree
maybe_fold_nontype_arg (arg)
     tree arg;
{
  /* If we're not in a template, ARG is already as simple as it's going to
     get, and trying to reprocess the trees will break.  */
  if (! processing_template_decl)
    return arg;

  if (!TYPE_P (arg) && !uses_template_parms (arg))
    {
      /* Sometimes, one of the args was an expression involving a
	 template constant parameter, like N - 1.  Now that we've
	 tsubst'd, we might have something like 2 - 1.  This will
	 confuse lookup_template_class, so we do constant folding
	 here.  We have to unset processing_template_decl, to
	 fool build_expr_from_tree() into building an actual
	 tree.  */

      int saved_processing_template_decl = processing_template_decl; 
      processing_template_decl = 0;
      arg = fold (build_expr_from_tree (arg));
      processing_template_decl = saved_processing_template_decl; 
    }
  return arg;
}

/* Return the TREE_VEC with the arguments for the innermost template header,
   where ARGS is either that or the VEC of VECs for all the
   arguments.  */

tree
innermost_args (args)
     tree args;
{
  return TMPL_ARGS_LEVEL (args, TMPL_ARGS_DEPTH (args));
}

/* Substitute ARGS into the vector of template arguments T.  */

static tree
tsubst_template_arg_vector (t, args, complain)
     tree t;
     tree args;
     int complain;
{
  int len = TREE_VEC_LENGTH (t), need_new = 0, i;
  tree *elts = (tree *) alloca (len * sizeof (tree));
  
  bzero ((char *) elts, len * sizeof (tree));
  
  for (i = 0; i < len; i++)
    {
      if (TREE_VEC_ELT (t, i) != NULL_TREE
	  && TREE_CODE (TREE_VEC_ELT (t, i)) == TREE_VEC)
	elts[i] = tsubst_template_arg_vector (TREE_VEC_ELT (t, i),
					      args, complain);
      else
	elts[i] = maybe_fold_nontype_arg
	  (tsubst_expr (TREE_VEC_ELT (t, i), args, complain,
			NULL_TREE));
      
      if (elts[i] != TREE_VEC_ELT (t, i))
	need_new = 1;
    }
  
  if (!need_new)
    return t;
  
  t = make_tree_vec (len);
  for (i = 0; i < len; i++)
    TREE_VEC_ELT (t, i) = elts[i];
  
  return t;
}

/* Return the result of substituting ARGS into the template parameters
   given by PARMS.  If there are m levels of ARGS and m + n levels of
   PARMS, then the result will contain n levels of PARMS.  For
   example, if PARMS is `template <class T> template <class U>
   template <T*, U, class V>' and ARGS is {{int}, {double}} then the
   result will be `template <int*, double, class V>'.  */

static tree
tsubst_template_parms (parms, args, complain)
     tree parms;
     tree args;
     int complain;
{
  tree r = NULL_TREE;
  tree* new_parms;

  for (new_parms = &r;
       TMPL_PARMS_DEPTH (parms) > TMPL_ARGS_DEPTH (args);
       new_parms = &(TREE_CHAIN (*new_parms)),
	 parms = TREE_CHAIN (parms))
    {
      tree new_vec = 
	make_tree_vec (TREE_VEC_LENGTH (TREE_VALUE (parms)));
      int i;
      
      for (i = 0; i < TREE_VEC_LENGTH (new_vec); ++i)
	{
	  tree default_value =
	    TREE_PURPOSE (TREE_VEC_ELT (TREE_VALUE (parms), i));
	  tree parm_decl = 
	    TREE_VALUE (TREE_VEC_ELT (TREE_VALUE (parms), i));
	  
	  TREE_VEC_ELT (new_vec, i)
	    = build_tree_list (tsubst (default_value, args, complain,
				       NULL_TREE), 
			       tsubst (parm_decl, args, complain,
				       NULL_TREE));
	}
      
      *new_parms = 
	tree_cons (build_int_2 (0, (TMPL_PARMS_DEPTH (parms) 
				    - TMPL_ARGS_DEPTH (args))),
		   new_vec, NULL_TREE);
    }

  return r;
}

/* Substitute the ARGS into the indicated aggregate (or enumeration)
   type T.  If T is not an aggregate or enumeration type, it is
   handled as if by tsubst.  IN_DECL is as for tsubst.  If
   ENTERING_SCOPE is non-zero, T is the context for a template which
   we are presently tsubst'ing.  Return the subsituted value.  */

static tree
tsubst_aggr_type (t, args, complain, in_decl, entering_scope)
     tree t;
     tree args;
     int complain;
     tree in_decl;
     int entering_scope;
{
  if (t == NULL_TREE)
    return NULL_TREE;

  switch (TREE_CODE (t))
    {
    case RECORD_TYPE:
      if (TYPE_PTRMEMFUNC_P (t))
	{
	  tree r = build_ptrmemfunc_type
	    (tsubst (TYPE_PTRMEMFUNC_FN_TYPE (t), args, complain, in_decl));
	  return cp_build_qualified_type_real (r, TYPE_QUALS (t),
					       complain);
	}

      /* else fall through */
    case ENUMERAL_TYPE:
    case UNION_TYPE:
      if (TYPE_TEMPLATE_INFO (t))
	{
	  tree argvec;
	  tree context;
	  tree r;

	  /* First, determine the context for the type we are looking
	     up.  */
	  if (TYPE_CONTEXT (t) != NULL_TREE)
	    context = tsubst_aggr_type (TYPE_CONTEXT (t), args,
					complain,
					in_decl, /*entering_scope=*/1);
	  else
	    context = NULL_TREE;

	  /* Then, figure out what arguments are appropriate for the
	     type we are trying to find.  For example, given:

	       template <class T> struct S;
	       template <class T, class U> void f(T, U) { S<U> su; }

	     and supposing that we are instantiating f<int, double>,
	     then our ARGS will be {int, double}, but, when looking up
	     S we only want {double}.  */
	  argvec = tsubst_template_arg_vector (TYPE_TI_ARGS (t), args,
					       complain);

  	  r = lookup_template_class (t, argvec, in_decl, context,
				     entering_scope);

	  return cp_build_qualified_type_real (r, TYPE_QUALS (t),
					       complain);
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
tsubst_default_argument (fn, type, arg)
     tree fn;
     tree type;
     tree arg;
{
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
  if (DECL_CLASS_SCOPE_P (fn))
    pushclass (DECL_CONTEXT (fn), 2);

  arg = tsubst_expr (arg, DECL_TI_ARGS (fn), /*complain=*/1, NULL_TREE);
  
  if (DECL_CLASS_SCOPE_P (fn))
    popclass ();

  /* Make sure the default argument is reasonable.  */
  arg = check_default_argument (type, arg);

  return arg;
}

/* Substitute into all the default arguments for FN.  */

static void
tsubst_default_arguments (fn)
     tree fn;
{
  tree arg;
  tree tmpl_args;

  tmpl_args = DECL_TI_ARGS (fn);

  /* If this function is not yet instantiated, we certainly don't need
     its default arguments.  */
  if (uses_template_parms (tmpl_args))
    return;

  for (arg = TYPE_ARG_TYPES (TREE_TYPE (fn)); 
       arg; 
       arg = TREE_CHAIN (arg))
    if (TREE_PURPOSE (arg))
      TREE_PURPOSE (arg) = tsubst_default_argument (fn, 
						    TREE_VALUE (arg),
						    TREE_PURPOSE (arg));
}

/* Substitute the ARGS into the T, which is a _DECL.  TYPE is the
   (already computed) substitution of ARGS into TREE_TYPE (T), if
   appropriate.  Return the result of the substitution.  IN_DECL is as
   for tsubst.  */

static tree
tsubst_decl (t, args, type, in_decl)
     tree t;
     tree args;
     tree type;
     tree in_decl;
{
  int saved_lineno;
  char* saved_filename;
  tree r = NULL_TREE;

  /* Set the filename and linenumber to improve error-reporting.  */
  saved_lineno = lineno;
  saved_filename = input_filename;
  lineno = DECL_SOURCE_LINE (t);
  input_filename = DECL_SOURCE_FILE (t);

  switch (TREE_CODE (t))
    {
    case TEMPLATE_DECL:
      {
	/* We can get here when processing a member template function
	   of a template class.  */
	tree decl = DECL_TEMPLATE_RESULT (t);
	tree spec;
	int is_template_template_parm = DECL_TEMPLATE_TEMPLATE_PARM_P (t);

	if (!is_template_template_parm)
	  {
	    /* We might already have an instance of this template.
	       The ARGS are for the surrounding class type, so the
	       full args contain the tsubst'd args for the context,
	       plus the innermost args from the template decl.  */
	    tree tmpl_args = DECL_CLASS_TEMPLATE_P (t) 
	      ? CLASSTYPE_TI_ARGS (TREE_TYPE (t))
	      : DECL_TI_ARGS (DECL_RESULT (t));
	    tree full_args;
	    
	    full_args = tsubst_template_arg_vector (tmpl_args, args,
						    /*complain=*/1);

	    /* tsubst_template_arg_vector doesn't copy the vector if
	       nothing changed.  But, *something* should have
	       changed.  */
	    my_friendly_assert (full_args != tmpl_args, 0);

	    spec = retrieve_specialization (t, full_args);
	    if (spec != NULL_TREE)
	      {
		r = spec;
		break;
	      }
	  }

	/* Make a new template decl.  It will be similar to the
	   original, but will record the current template arguments. 
	   We also create a new function declaration, which is just
	   like the old one, but points to this new template, rather
	   than the old one.  */
	r = copy_node (t);
	copy_lang_decl (r);
	my_friendly_assert (DECL_LANG_SPECIFIC (r) != 0, 0);
	TREE_CHAIN (r) = NULL_TREE;

	if (is_template_template_parm)
	  {
	    tree new_decl = tsubst (decl, args, /*complain=*/1, in_decl);
	    DECL_RESULT (r) = new_decl;
	    TREE_TYPE (r) = TREE_TYPE (new_decl);
	    break;
	  }

	DECL_CONTEXT (r) 
	  = tsubst_aggr_type (DECL_CONTEXT (t), args, 
			      /*complain=*/1, in_decl, 
			      /*entering_scope=*/1); 
	DECL_VIRTUAL_CONTEXT (r) 
	  = tsubst_aggr_type (DECL_VIRTUAL_CONTEXT (t), args, 
			      /*complain=*/1, in_decl, 
			      /*entering_scope=*/1);
	DECL_TEMPLATE_INFO (r) = build_tree_list (t, args);

	if (TREE_CODE (decl) == TYPE_DECL)
	  {
	    tree new_type = tsubst (TREE_TYPE (t), args,
				    /*complain=*/1, in_decl);
	    TREE_TYPE (r) = new_type;
	    CLASSTYPE_TI_TEMPLATE (new_type) = r;
	    DECL_RESULT (r) = TYPE_MAIN_DECL (new_type);
	    DECL_TI_ARGS (r) = CLASSTYPE_TI_ARGS (new_type);
	  }
	else
	  {
	    tree new_decl = tsubst (decl, args, /*complain=*/1, in_decl);
	    DECL_RESULT (r) = new_decl;
	    DECL_TI_TEMPLATE (new_decl) = r;
	    TREE_TYPE (r) = TREE_TYPE (new_decl);
	    DECL_TI_ARGS (r) = DECL_TI_ARGS (new_decl);
	  }

	SET_DECL_IMPLICIT_INSTANTIATION (r);
	DECL_TEMPLATE_INSTANTIATIONS (r) = NULL_TREE;
	DECL_TEMPLATE_SPECIALIZATIONS (r) = NULL_TREE;

	/* The template parameters for this new template are all the
	   template parameters for the old template, except the
	   outermost level of parameters. */
	DECL_TEMPLATE_PARMS (r) 
	  = tsubst_template_parms (DECL_TEMPLATE_PARMS (t), args,
				   /*complain=*/1);

	if (PRIMARY_TEMPLATE_P (t))
	  DECL_PRIMARY_TEMPLATE (r) = r;

	/* We don't partially instantiate partial specializations.  */
	if (TREE_CODE (decl) == TYPE_DECL)
	  break;

	for (spec = DECL_TEMPLATE_SPECIALIZATIONS (t);
	     spec != NULL_TREE;
	     spec = TREE_CHAIN (spec))
	  {
	    /* It helps to consider example here.  Consider:

	       template <class T>
	       struct S {
	         template <class U>
		 void f(U u);

		 template <>
		 void f(T* t) {}
	       };
	       
	       Now, for example, we are instantiating S<int>::f(U u).  
	       We want to make a template:

	       template <class U>
	       void S<int>::f(U);

	       It will have a specialization, for the case U = int*, of
	       the form:

	       template <>
	       void S<int>::f<int*>(int*);

	       This specialization will be an instantiation of
	       the specialization given in the declaration of S, with
	       argument list int*.  */

	    tree fn = TREE_VALUE (spec);
	    tree spec_args;
	    tree new_fn;

	    if (!DECL_TEMPLATE_SPECIALIZATION (fn))
	      /* Instantiations are on the same list, but they're of
		 no concern to us.  */
	      continue;

	    if (TREE_CODE (fn) != TEMPLATE_DECL)
	      /* A full specialization.  There's no need to record
		 that here.  */
	      continue;

	    spec_args = tsubst (DECL_TI_ARGS (fn), args,
				/*complain=*/1, in_decl); 
	    new_fn = tsubst (DECL_RESULT (most_general_template (fn)), 
			     spec_args, /*complain=*/1, in_decl); 
	    DECL_TI_TEMPLATE (new_fn) = fn;
	    register_specialization (new_fn, r, 
				     innermost_args (spec_args));
	  }

	/* Record this partial instantiation.  */
	register_specialization (r, t, 
				 DECL_TI_ARGS (DECL_RESULT (r)));

      }
      break;

    case FUNCTION_DECL:
      {
	tree ctx;
	tree argvec = NULL_TREE;
	tree *friends;
	tree gen_tmpl;
	int member;
	int args_depth;
	int parms_depth;

	/* Nobody should be tsubst'ing into non-template functions.  */
	my_friendly_assert (DECL_TEMPLATE_INFO (t) != NULL_TREE, 0);

	if (TREE_CODE (DECL_TI_TEMPLATE (t)) == TEMPLATE_DECL)
	  {
	    tree spec;

	    /* Calculate the most general template of which R is a
	       specialization, and the complete set of arguments used to
	       specialize R.  */
	    gen_tmpl = most_general_template (DECL_TI_TEMPLATE (t));
	    argvec 
	      = tsubst_template_arg_vector (DECL_TI_ARGS 
					    (DECL_TEMPLATE_RESULT (gen_tmpl)),
					    args, /*complain=*/1); 

	    /* Check to see if we already have this specialization.  */
	    spec = retrieve_specialization (gen_tmpl, argvec);

	    if (spec)
	      {
		r = spec;
		break;
	      }

	    /* Here, we deal with the peculiar case:

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
	      {
		my_friendly_assert (DECL_FRIEND_P (t), 0);

		if (parms_depth > 1)
		  {
		    int i;

		    args = make_tree_vec (parms_depth);
		    for (i = 0; i < parms_depth; ++i)
		      TREE_VEC_ELT (args, i) = 
			TREE_VEC_ELT (args, i + (args_depth - parms_depth));
		  }
		else
		  args = TREE_VEC_ELT (args, args_depth - parms_depth);
	      }
	  }
	else
	  {
	    /* This special case arises when we have something like this:

	         template <class T> struct S { 
		   friend void f<int>(int, double); 
		 };

	       Here, the DECL_TI_TEMPLATE for the friend declaration
	       will be a LOOKUP_EXPR or an IDENTIFIER_NODE.  We are
	       being called from tsubst_friend_function, and we want
	       only to create a new decl (R) with appropriate types so
	       that we can call determine_specialization.  */
	    my_friendly_assert ((TREE_CODE (DECL_TI_TEMPLATE (t)) 
				 == LOOKUP_EXPR)
				|| (TREE_CODE (DECL_TI_TEMPLATE (t))
				    == IDENTIFIER_NODE), 0);
	    gen_tmpl = NULL_TREE;
	  }

	if (DECL_CLASS_SCOPE_P (t))
	  {
	    if (DECL_NAME (t) == constructor_name (DECL_CONTEXT (t)))
	      member = 2;
	    else
	      member = 1;
	    ctx = tsubst_aggr_type (DECL_CONTEXT (t), args, 
				    /*complain=*/1, t, 
				    /*entering_scope=*/1);
	  }
	else
	  {
	    member = 0;
	    ctx = DECL_CONTEXT (t);
	  }
	type = tsubst (type, args, /*complain=*/1, in_decl);
	if (type == error_mark_node)
	  return error_mark_node;

	/* We do NOT check for matching decls pushed separately at this
           point, as they may not represent instantiations of this
           template, and in any case are considered separate under the
           discrete model.  Instead, see add_maybe_template.  */

	r = copy_node (t);
	copy_lang_decl (r);
	DECL_USE_TEMPLATE (r) = 0;
	TREE_TYPE (r) = type;

	DECL_CONTEXT (r) = ctx;
	DECL_VIRTUAL_CONTEXT (r)
	  = tsubst_aggr_type (DECL_VIRTUAL_CONTEXT (t), args, 
			      /*complain=*/1, t,
			      /*entering_scope=*/1);

	if (member && IDENTIFIER_TYPENAME_P (DECL_NAME (r)))
	  /* Type-conversion operator.  Reconstruct the name, in
	     case it's the name of one of the template's parameters.  */
	  DECL_NAME (r) = build_typename_overload (TREE_TYPE (type));

	DECL_ARGUMENTS (r) = tsubst (DECL_ARGUMENTS (t), args,
				     /*complain=*/1, t);
	DECL_MAIN_VARIANT (r) = r;
	DECL_RESULT (r) = NULL_TREE;

	TREE_STATIC (r) = 0;
	TREE_PUBLIC (r) = TREE_PUBLIC (t);
	DECL_EXTERNAL (r) = 1;
	DECL_INTERFACE_KNOWN (r) = 0;
	DECL_DEFER_OUTPUT (r) = 0;
	TREE_CHAIN (r) = NULL_TREE;
	DECL_PENDING_INLINE_INFO (r) = 0;
	DECL_PENDING_INLINE_P (r) = 0;
	TREE_USED (r) = 0;

	/* Set up the DECL_TEMPLATE_INFO for R and compute its mangled
	   name.  There's no need to do this in the special friend
	   case mentioned above where GEN_TMPL is NULL.  */
	if (gen_tmpl)
	  {
	    DECL_TEMPLATE_INFO (r) 
	      = tree_cons (gen_tmpl, argvec, NULL_TREE);
	    SET_DECL_IMPLICIT_INSTANTIATION (r);
	    register_specialization (r, gen_tmpl, argvec);

	    /* Set the mangled name for R.  */
	    if (DECL_DESTRUCTOR_P (t))
	      DECL_ASSEMBLER_NAME (r) = build_destructor_name (ctx);
	    else 
	      {
		/* Instantiations of template functions must be mangled
		   specially, in order to conform to 14.5.5.1
		   [temp.over.link].  */
		tree tmpl = DECL_TI_TEMPLATE (t);
		
		/* TMPL will be NULL if this is a specialization of a
		   member function of a template class.  */
		if (name_mangling_version < 1
		    || tmpl == NULL_TREE
		    || (member && !is_member_template (tmpl)
			&& !DECL_TEMPLATE_INFO (tmpl)))
		  set_mangled_name_for_decl (r);
		else
		  set_mangled_name_for_template_decl (r);
	      }
	    
	    DECL_RTL (r) = 0;
	    make_decl_rtl (r, NULL_PTR, 1);
	    
	    /* Like grokfndecl.  If we don't do this, pushdecl will
	       mess up our TREE_CHAIN because it doesn't find a
	       previous decl.  Sigh.  */
	    if (member
		&& ! uses_template_parms (r)
		&& (IDENTIFIER_GLOBAL_VALUE (DECL_ASSEMBLER_NAME (r)) 
		    == NULL_TREE))
	      SET_IDENTIFIER_GLOBAL_VALUE (DECL_ASSEMBLER_NAME (r), r);

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
	      tsubst_default_arguments (r);
	  }

	/* Copy the list of befriending classes.  */
	for (friends = &DECL_BEFRIENDING_CLASSES (r);
	     *friends;
	     friends = &TREE_CHAIN (*friends)) 
	  {
	    *friends = copy_node (*friends);
	    TREE_VALUE (*friends) = tsubst (TREE_VALUE (*friends),
					    args, /*complain=*/1, 
					    in_decl);
	  }

	if (DECL_CONSTRUCTOR_P (r))
	  {
	    maybe_retrofit_in_chrg (r);
	    grok_ctor_properties (ctx, r);
	  }
	else if (DECL_OVERLOADED_OPERATOR_P (r))
	  grok_op_properties (r, DECL_VIRTUAL_P (r), DECL_FRIEND_P (r));
      }
      break;

    case PARM_DECL:
      {
	r = copy_node (t);
	TREE_TYPE (r) = type;
	c_apply_type_quals_to_decl (CP_TYPE_QUALS (type), r);

	if (TREE_CODE (DECL_INITIAL (r)) != TEMPLATE_PARM_INDEX)
	  DECL_INITIAL (r) = TREE_TYPE (r);
	else
	  DECL_INITIAL (r) = tsubst (DECL_INITIAL (r), args,
				     /*complain=*/1, in_decl);

	DECL_CONTEXT (r) = NULL_TREE;
	if (PROMOTE_PROTOTYPES
	    && (TREE_CODE (type) == INTEGER_TYPE
		|| TREE_CODE (type) == ENUMERAL_TYPE)
	    && TYPE_PRECISION (type) < TYPE_PRECISION (integer_type_node))
	  DECL_ARG_TYPE (r) = integer_type_node;
	if (TREE_CHAIN (t))
	  TREE_CHAIN (r) = tsubst (TREE_CHAIN (t), args,
				   /*complain=*/1, TREE_CHAIN (t));
      }
      break;

    case FIELD_DECL:
      {
	r = copy_node (t);
	copy_lang_decl (r);
	TREE_TYPE (r) = type;
	c_apply_type_quals_to_decl (CP_TYPE_QUALS (type), r);

	/* We don't have to set DECL_CONTEXT here; it is set by
	   finish_member_declaration.  */
	DECL_INITIAL (r) = tsubst_expr (DECL_INITIAL (t), args,
					/*complain=*/1, in_decl);
	TREE_CHAIN (r) = NULL_TREE;
	if (TREE_CODE (type) == VOID_TYPE) 
	  cp_error_at ("instantiation of `%D' as type void", r);
      }
      break;

    case USING_DECL:
      {
	r = copy_node (t);
	DECL_INITIAL (r)
	  = tsubst_copy (DECL_INITIAL (t), args, /*complain=*/1, in_decl);
	TREE_CHAIN (r) = NULL_TREE;
      }
      break;

    case TYPE_DECL:
      if (DECL_IMPLICIT_TYPEDEF_P (t))
	{
	  /* For an implicit typedef, we just want the implicit
	     typedef for the tsubst'd type.  We've already got the
	     tsubst'd type, as TYPE, so we just need it's associated
	     declaration.  */
	  r = TYPE_NAME (type);
	  break;
	}
      else if (!DECL_LANG_SPECIFIC (t))
	{
	  /* For a template type parameter, we don't have to do
	     anything special.  */
	  r = TYPE_NAME (type);
	  break;
	}

      /* Fall through.  */

    case VAR_DECL:
      {
	tree argvec;
	tree gen_tmpl;
	tree spec;
	tree tmpl;
	tree ctx;

	/* Nobody should be tsubst'ing into non-template variables.  */
	my_friendly_assert (DECL_LANG_SPECIFIC (t) 
			    && DECL_TEMPLATE_INFO (t) != NULL_TREE, 0);

	if (TYPE_P (CP_DECL_CONTEXT (t)))
	  ctx = tsubst_aggr_type (DECL_CONTEXT (t), args, 
				  /*complain=*/1,
				  in_decl, /*entering_scope=*/1);
	else
	  /* Subsequent calls to pushdecl will fill this in.  */
	  ctx = NULL_TREE;

	/* Check to see if we already have this specialization.  */
	tmpl = DECL_TI_TEMPLATE (t);
	gen_tmpl = most_general_template (tmpl);
	argvec = tsubst (DECL_TI_ARGS (t), args, /*complain=*/1, in_decl);
	if (ctx)
	  spec = retrieve_specialization (gen_tmpl, argvec);
	else
	  spec = retrieve_local_specialization (gen_tmpl,
						current_function_decl);

	if (spec)
	  {
	    r = spec;
	    break;
	  }

	/* This declaration is going to have to be around for a while,
	   so me make sure it is on a saveable obstack.  */
	r = copy_node (t);
	
	TREE_TYPE (r) = type;
	c_apply_type_quals_to_decl (CP_TYPE_QUALS (type), r);
	DECL_CONTEXT (r) = ctx;

	/* Don't try to expand the initializer until someone tries to use
	   this variable; otherwise we run into circular dependencies.  */
	DECL_INITIAL (r) = NULL_TREE;
	DECL_RTL (r) = 0;
	DECL_SIZE (r) = DECL_SIZE_UNIT (r) = 0;
	copy_lang_decl (r);

	/* For __PRETTY_FUNCTION__ we have to adjust the initializer.  */
	if (DECL_PRETTY_FUNCTION_P (r))
	  {
	    DECL_INITIAL (r) = tsubst (DECL_INITIAL (t),
				       args,
				       /*complain=*/1,
				       NULL_TREE);
	    TREE_TYPE (r) = TREE_TYPE (DECL_INITIAL (r));
	  }

	/* Even if the original location is out of scope, the newly
	   substituted one is not.  */
	if (TREE_CODE (r) == VAR_DECL)
	  DECL_DEAD_FOR_LOCAL (r) = 0;

	/* A static data member declaration is always marked external
	   when it is declared in-class, even if an initializer is
	   present.  We mimic the non-template processing here.  */
	if (ctx)
	  DECL_EXTERNAL (r) = 1;

	DECL_TEMPLATE_INFO (r) = tree_cons (tmpl, argvec, NULL_TREE);
	SET_DECL_IMPLICIT_INSTANTIATION (r);
	if (ctx)
	  register_specialization (r, gen_tmpl, argvec);
	else
	  register_local_specialization (r, gen_tmpl,
					 current_function_decl);

	TREE_CHAIN (r) = NULL_TREE;
	if (TREE_CODE (r) == VAR_DECL && TREE_CODE (type) == VOID_TYPE)
	  cp_error_at ("instantiation of `%D' as type void", r);
      }
      break;

    default:
      my_friendly_abort (0);
    } 

  /* Restore the file and line information.  */
  lineno = saved_lineno;
  input_filename = saved_filename;

  return r;
}

/* Substitue into the ARG_TYPES of a function type.  */

static tree
tsubst_arg_types (arg_types, args, complain, in_decl)
     tree arg_types;
     tree args;
     int complain;
     tree in_decl;
{
  tree remaining_arg_types;
  tree type;

  if (!arg_types || arg_types == void_list_node)
    return arg_types;
  
  remaining_arg_types = tsubst_arg_types (TREE_CHAIN (arg_types),
					  args, complain, in_decl);
  if (remaining_arg_types == error_mark_node)
    return error_mark_node;

  type = tsubst (TREE_VALUE (arg_types), args, complain, in_decl);
  if (type == error_mark_node)
    return error_mark_node;

  /* Do array-to-pointer, function-to-pointer conversion, and ignore
     top-level qualifiers as required.  */
  type = TYPE_MAIN_VARIANT (type_decays_to (type));

  /* Note that we do not substitute into default arguments here.  The
     standard mandates that they be instantiated only when needed,
     which is done in build_over_call.  */
  return hash_tree_cons (TREE_PURPOSE (arg_types), type,
			 remaining_arg_types);
			 
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
tsubst_function_type (t, args, complain, in_decl)
     tree t;
     tree args;
     int complain;
     tree in_decl;
{
  tree return_type;
  tree arg_types;
  tree fntype;

  /* The TYPE_CONTEXT is not used for function/method types.  */
  my_friendly_assert (TYPE_CONTEXT (t) == NULL_TREE, 0);

  /* Substitue the return type.  */
  return_type = tsubst (TREE_TYPE (t), args, complain, in_decl);
  if (return_type == error_mark_node)
    return error_mark_node;

  /* Substitue the argument types.  */
  arg_types = tsubst_arg_types (TYPE_ARG_TYPES (t), args,
				complain, in_decl); 
  if (arg_types == error_mark_node)
    return error_mark_node;
  
  /* Construct a new type node and return it.  */
  if (TREE_CODE (t) == FUNCTION_TYPE)
    fntype = build_function_type (return_type, arg_types);
  else
    {
      tree r = TREE_TYPE (TREE_VALUE (arg_types));
      if (! IS_AGGR_TYPE (r))
	{
	  /* [temp.deduct]
	     
	     Type deduction may fail for any of the following
	     reasons:
	     
	     -- Attempting to create "pointer to member of T" when T
	     is not a class type.  */
	  if (complain)
	    cp_error ("creating pointer to member function of non-class type `%T'",
		      r);
	  return error_mark_node;
	}
      
      fntype = build_cplus_method_type (r, return_type, TREE_CHAIN
					(arg_types));
    }
  fntype = build_qualified_type (fntype, TYPE_QUALS (t));
  fntype = build_type_attribute_variant (fntype, TYPE_ATTRIBUTES (t));
  
  return fntype;  
}

/* Substitute into the PARMS of a call-declarator.  */

static tree
tsubst_call_declarator_parms (parms, args, complain, in_decl)
     tree parms;
     tree args;
     int complain;
     tree in_decl;
{
  tree new_parms;
  tree type;
  tree defarg;

  if (!parms || parms == void_list_node)
    return parms;
  
  new_parms = tsubst_call_declarator_parms (TREE_CHAIN (parms),
					    args, complain, in_decl);

  /* Figure out the type of this parameter.  */
  type = tsubst (TREE_VALUE (parms), args, complain, in_decl);
  
  /* Figure out the default argument as well.  Note that we use
     tsubst_expr since the default argument is really an expression.  */
  defarg = tsubst_expr (TREE_PURPOSE (parms), args, complain, in_decl);

  /* Chain this parameter on to the front of those we have already
     processed.  We don't use hash_tree_cons because that function
     doesn't check TREE_PARMLIST.  */
  new_parms = tree_cons (defarg, type, new_parms);

  /* And note that these are parameters.  */
  TREE_PARMLIST (new_parms) = 1;
  
  return new_parms;
}

/* Take the tree structure T and replace template parameters used
   therein with the argument vector ARGS.  IN_DECL is an associated
   decl for diagnostics.  If an error occurs, returns ERROR_MARK_NODE.
   An appropriate error message is issued only if COMPLAIN is
   non-zero.  Note that we must be relatively non-tolerant of
   extensions here, in order to preserve conformance; if we allow
   substitutions that should not be allowed, we may allow argument
   deductions that should not succeed, and therefore report ambiguous
   overload situations where there are none.  In theory, we could
   allow the substitution, but indicate that it should have failed,
   and allow our caller to make sure that the right thing happens, but
   we don't try to do this yet.

   This function is used for dealing with types, decls and the like;
   for expressions, use tsubst_expr or tsubst_copy.  */

tree
tsubst (t, args, complain, in_decl)
     tree t, args;
     int complain;
     tree in_decl;
{
  tree type, r;

  if (t == NULL_TREE || t == error_mark_node
      || t == integer_type_node
      || t == void_type_node
      || t == char_type_node
      || TREE_CODE (t) == NAMESPACE_DECL)
    return t;

  if (TREE_CODE (t) == IDENTIFIER_NODE)
    type = IDENTIFIER_TYPE_VALUE (t);
  else
    type = TREE_TYPE (t);
  if (type == unknown_type_node)
    my_friendly_abort (42);

  if (type && TREE_CODE (t) != FUNCTION_DECL
      && TREE_CODE (t) != TYPENAME_TYPE
      && TREE_CODE (t) != TEMPLATE_DECL
      && TREE_CODE (t) != IDENTIFIER_NODE
      && TREE_CODE (t) != FUNCTION_TYPE
      && TREE_CODE (t) != METHOD_TYPE)
    type = tsubst (type, args, complain, in_decl);
  if (type == error_mark_node)
    return error_mark_node;

  if (DECL_P (t))
    return tsubst_decl (t, args, type, in_decl);

  switch (TREE_CODE (t))
    {
    case RECORD_TYPE:
    case UNION_TYPE:
    case ENUMERAL_TYPE:
      return tsubst_aggr_type (t, args, complain, in_decl,
			       /*entering_scope=*/0);

    case ERROR_MARK:
    case IDENTIFIER_NODE:
    case OP_IDENTIFIER:
    case VOID_TYPE:
    case REAL_TYPE:
    case COMPLEX_TYPE:
    case BOOLEAN_TYPE:
    case INTEGER_CST:
    case REAL_CST:
    case STRING_CST:
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
	if (max == error_mark_node)
	  return error_mark_node;

	/* See if we can reduce this expression to something simpler.  */
	max = maybe_fold_nontype_arg (max);
	if (!processing_template_decl && TREE_READONLY_DECL_P (max))
	  max = decl_constant_value (max);

	if (processing_template_decl 
	    /* When providing explicit arguments to a template
	       function, but leaving some arguments for subsequent
	       deduction, MAX may be template-dependent even if we're
	       not PROCESSING_TEMPLATE_DECL.  We still need to check for
	       template parms, though; MAX won't be an INTEGER_CST for
	       dynamic arrays, either.  */
	    || (TREE_CODE (max) != INTEGER_CST
		&& uses_template_parms (max)))
	  {
	    tree itype = make_node (INTEGER_TYPE);
	    TYPE_MIN_VALUE (itype) = size_zero_node;
	    TYPE_MAX_VALUE (itype) = build_min (MINUS_EXPR, sizetype, max,
						integer_one_node);
	    return itype;
	  }

	if (integer_zerop (omax))
	  {
	    /* Still allow an explicit array of size zero.  */
	    if (pedantic)
	      pedwarn ("creating array with size zero");
	  }
	else if (integer_zerop (max) 
		 || (TREE_CODE (max) == INTEGER_CST 
		     && INT_CST_LT (max, integer_zero_node)))
	  {
	    /* [temp.deduct]

	       Type deduction may fail for any of the following
	       reasons:  

		 Attempting to create an array with a size that is
		 zero or negative.  */
	    if (complain)
	      cp_error ("creating array with size zero (`%E')", max);

	    return error_mark_node;
	  }

	return compute_array_index_type (NULL_TREE, max);
      }

    case TEMPLATE_TYPE_PARM:
    case TEMPLATE_TEMPLATE_PARM:
    case TEMPLATE_PARM_INDEX:
      {
	int idx;
	int level;
	int levels;

	r = NULL_TREE;

	if (TREE_CODE (t) == TEMPLATE_TYPE_PARM
	    || TREE_CODE (t) == TEMPLATE_TEMPLATE_PARM)
	  {
	    idx = TEMPLATE_TYPE_IDX (t);
	    level = TEMPLATE_TYPE_LEVEL (t);
	  }
	else
	  {
	    idx = TEMPLATE_PARM_IDX (t);
	    level = TEMPLATE_PARM_LEVEL (t);
	  }

	if (TREE_VEC_LENGTH (args) > 0)
	  {
	    tree arg = NULL_TREE;

	    levels = TMPL_ARGS_DEPTH (args);
	    if (level <= levels)
	      arg = TMPL_ARG (args, level, idx);

	    if (arg == error_mark_node)
	      return error_mark_node;
	    else if (arg != NULL_TREE)
	      {
		if (TREE_CODE (t) == TEMPLATE_TYPE_PARM)
		  {
		    my_friendly_assert (TYPE_P (arg), 0);
		    return cp_build_qualified_type_real
		      (arg, CP_TYPE_QUALS (arg) | CP_TYPE_QUALS (t),
		       complain);
		  }
		else if (TREE_CODE (t) == TEMPLATE_TEMPLATE_PARM)
		  {
		    if (TEMPLATE_TEMPLATE_PARM_TEMPLATE_INFO (t))
		      {
			/* We are processing a type constructed from
			   a template template parameter */
			tree argvec = tsubst (TYPE_TI_ARGS (t),
					      args, complain, in_decl);
			if (argvec == error_mark_node)
			  return error_mark_node;
			
			/* We can get a TEMPLATE_TEMPLATE_PARM here when 
			   we are resolving nested-types in the signature of 
			   a member function templates.
			   Otherwise ARG is a TEMPLATE_DECL and is the real 
			   template to be instantiated.  */
			if (TREE_CODE (arg) == TEMPLATE_TEMPLATE_PARM)
			  arg = TYPE_NAME (arg);

			r = lookup_template_class (DECL_NAME (arg), 
						   argvec, in_decl, 
						   DECL_CONTEXT (arg),
						   /*entering_scope=*/0);
			return cp_build_qualified_type_real (r, 
							     TYPE_QUALS (t),
							     complain);
		      }
		    else
		      /* We are processing a template argument list.  */ 
		      return arg;
		  }
		else
		  return arg;
	      }
	  }
	else
	  my_friendly_abort (981018);

	if (level == 1)
	  /* This can happen during the attempted tsubst'ing in
	     unify.  This means that we don't yet have any information
	     about the template parameter in question.  */
	  return t;

	/* If we get here, we must have been looking at a parm for a
	   more deeply nested template.  Make a new version of this
	   template parameter, but with a lower level.  */
	switch (TREE_CODE (t))
	  {
	  case TEMPLATE_TYPE_PARM:
	  case TEMPLATE_TEMPLATE_PARM:
	    r = copy_node (t);
	    TEMPLATE_TYPE_PARM_INDEX (r)
	      = reduce_template_parm_level (TEMPLATE_TYPE_PARM_INDEX (t),
					    r, levels);
	    TYPE_STUB_DECL (r) = TYPE_NAME (r) = TEMPLATE_TYPE_DECL (r);
	    TYPE_MAIN_VARIANT (r) = r;
	    TYPE_POINTER_TO (r) = NULL_TREE;
	    TYPE_REFERENCE_TO (r) = NULL_TREE;

	    if (TREE_CODE (t) == TEMPLATE_TEMPLATE_PARM
		&& TEMPLATE_TEMPLATE_PARM_TEMPLATE_INFO (t))
	      {
		tree argvec = tsubst (TYPE_TI_ARGS (t), args,
				      complain, in_decl); 
		if (argvec == error_mark_node)
		  return error_mark_node;

		TEMPLATE_TEMPLATE_PARM_TEMPLATE_INFO (r)
		  = tree_cons (TYPE_NAME (t), argvec, NULL_TREE);
	      }
	    break;

	  case TEMPLATE_PARM_INDEX:
	    r = reduce_template_parm_level (t, type, levels);
	    break;
	   
	  default:
	    my_friendly_abort (0);
	  }

	return r;
      }

    case TREE_LIST:
      {
	tree purpose, value, chain, result;

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
	result = hash_tree_cons (purpose, value, chain);
	TREE_PARMLIST (result) = TREE_PARMLIST (t);
	return result;
      }
    case TREE_VEC:
      if (type != NULL_TREE)
	{
	  /* A binfo node.  We always need to make a copy, of the node
	     itself and of its BINFO_BASETYPES.  */

	  t = copy_node (t);

	  /* Make sure type isn't a typedef copy.  */
	  type = BINFO_TYPE (TYPE_BINFO (type));

	  TREE_TYPE (t) = complete_type (type);
	  if (IS_AGGR_TYPE (type))
	    {
	      BINFO_VTABLE (t) = TYPE_BINFO_VTABLE (type);
	      BINFO_VIRTUALS (t) = TYPE_BINFO_VIRTUALS (type);
	      if (TYPE_BINFO_BASETYPES (type) != NULL_TREE)
		BINFO_BASETYPES (t) = copy_node (TYPE_BINFO_BASETYPES (type));
	    }
	  return t;
	}

      /* Otherwise, a vector of template arguments.  */
      return tsubst_template_arg_vector (t, args, complain);

    case POINTER_TYPE:
    case REFERENCE_TYPE:
      {
	enum tree_code code;

	if (type == TREE_TYPE (t))
	  return t;

	code = TREE_CODE (t);


	/* [temp.deduct]
	   
	   Type deduction may fail for any of the following
	   reasons:  

	   -- Attempting to create a pointer to reference type.
	   -- Attempting to create a reference to a reference type or
	      a reference to void.  */
	if (TREE_CODE (type) == REFERENCE_TYPE
	    || (code == REFERENCE_TYPE && TREE_CODE (type) == VOID_TYPE))
	  {
	    static int   last_line = 0;
	    static char* last_file = 0;

	    /* We keep track of the last time we issued this error
	       message to avoid spewing a ton of messages during a
	       single bad template instantiation.  */
	    if (complain && (last_line != lineno ||
			     last_file != input_filename))
	      {
		if (TREE_CODE (type) == VOID_TYPE)
		  cp_error ("forming reference to void");
		else
		  cp_error ("forming %s to reference type `%T'",
			    (code == POINTER_TYPE) ? "pointer" : "reference",
			    type);
		last_line = lineno;
		last_file = input_filename;
	      }

	    return error_mark_node;
	  }
	else if (code == POINTER_TYPE)
	  r = build_pointer_type (type);
	else
	  r = build_reference_type (type);
	r = cp_build_qualified_type_real (r, TYPE_QUALS (t), complain);

	/* Will this ever be needed for TYPE_..._TO values?  */
	layout_type (r);
	return r;
      }
    case OFFSET_TYPE:
      {
	r = tsubst (TYPE_OFFSET_BASETYPE (t), args, complain, in_decl);
	if (r == error_mark_node || !IS_AGGR_TYPE (r))
	  {
	    /* [temp.deduct]

	       Type deduction may fail for any of the following
	       reasons:
	       
	       -- Attempting to create "pointer to member of T" when T
	          is not a class type.  */
	    if (complain)
	      cp_error ("creating pointer to member of non-class type `%T'", 
			r);
	    return error_mark_node;
	  }
	return build_offset_type (r, type);
      }
    case FUNCTION_TYPE:
    case METHOD_TYPE:
      {
	tree fntype;
	tree raises;

	fntype = tsubst_function_type (t, args, complain, in_decl);
	if (fntype == error_mark_node)
	  return error_mark_node;

	/* Substitue the exception specification. */
	raises = TYPE_RAISES_EXCEPTIONS (t);
	if (raises)
	  {
	    tree   list = NULL_TREE;
	    
	    if (! TREE_VALUE (raises))
	      list = raises;
	    else
	      for (; raises != NULL_TREE; raises = TREE_CHAIN (raises))
	        {
	          tree spec = TREE_VALUE (raises);
	          
	          spec = tsubst (spec, args, complain, in_decl);
	          if (spec == error_mark_node)
	            return spec;
	          list = add_exception_specifier (list, spec, complain);
	        }
	    fntype = build_exception_variant (fntype, list);
	  }
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
	      is void, a function type, or a reference type.  */
	if (TREE_CODE (type) == VOID_TYPE 
	    || TREE_CODE (type) == FUNCTION_TYPE
	    || TREE_CODE (type) == REFERENCE_TYPE)
	  {
	    if (complain)
	      cp_error ("creating array of `%T'", type);
	    return error_mark_node;
	  }

	r = build_cplus_array_type (type, domain);
	return r;
      }

    case PLUS_EXPR:
    case MINUS_EXPR:
      {
	tree e1 = tsubst (TREE_OPERAND (t, 0), args, complain,
			  in_decl);
	tree e2 = tsubst (TREE_OPERAND (t, 1), args, complain,
			  in_decl);

	if (e1 == error_mark_node || e2 == error_mark_node)
	  return error_mark_node;

	return fold (build (TREE_CODE (t), TREE_TYPE (t), e1, e2));
      }

    case NEGATE_EXPR:
    case NOP_EXPR:
      {
	tree e = tsubst (TREE_OPERAND (t, 0), args, complain,
			  in_decl);
	if (e == error_mark_node)
	  return error_mark_node;

	return fold (build (TREE_CODE (t), TREE_TYPE (t), e));
      }

    case TYPENAME_TYPE:
      {
	tree ctx = tsubst_aggr_type (TYPE_CONTEXT (t), args, complain,
				     in_decl, /*entering_scope=*/1);
	tree f = tsubst_copy (TYPENAME_TYPE_FULLNAME (t), args,
			      complain, in_decl); 

	if (ctx == error_mark_node || f == error_mark_node)
	  return error_mark_node;

	if (!IS_AGGR_TYPE (ctx))
	  {
	    if (complain)
	      cp_error ("`%T' is not a class, struct, or union type",
			ctx);
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
		if (complain)
		  incomplete_type_error (NULL_TREE, ctx);
		return error_mark_node;
	      }
	  }

	f = make_typename_type (ctx, f, complain);
	if (f == error_mark_node)
	  return f;
	return cp_build_qualified_type_real (f, 
					     CP_TYPE_QUALS (f) 
					     | CP_TYPE_QUALS (t),
					     complain);
      }

    case INDIRECT_REF:
      {
	tree e = tsubst (TREE_OPERAND (t, 0), args, complain,
			 in_decl);
	if (e == error_mark_node)
	  return error_mark_node;
	return make_pointer_declarator (type, e);
      }

    case ADDR_EXPR:
      {
	tree e = tsubst (TREE_OPERAND (t, 0), args, complain,
			 in_decl);
	if (e == error_mark_node)
	  return error_mark_node;
	return make_reference_declarator (type, e);
      }

    case ARRAY_REF:
      {
	tree e1 = tsubst (TREE_OPERAND (t, 0), args, complain,
			  in_decl);
	tree e2 = tsubst_expr (TREE_OPERAND (t, 1), args, complain,
			       in_decl);
	if (e1 == error_mark_node || e2 == error_mark_node)
	  return error_mark_node;

	return build_parse_node (ARRAY_REF, e1, e2, tsubst_expr);
      }

    case CALL_EXPR:
      {
	tree e1 = tsubst (TREE_OPERAND (t, 0), args, complain,
			  in_decl);
	tree e2 = (tsubst_call_declarator_parms
		   (CALL_DECLARATOR_PARMS (t), args, complain, in_decl));
	tree e3 = tsubst (CALL_DECLARATOR_EXCEPTION_SPEC (t), args,
			  complain, in_decl);

	if (e1 == error_mark_node || e2 == error_mark_node 
	    || e3 == error_mark_node)
	  return error_mark_node;

	return make_call_declarator (e1, e2, CALL_DECLARATOR_QUALS (t), e3);
      }

    case SCOPE_REF:
      {
	tree e1 = tsubst (TREE_OPERAND (t, 0), args, complain,
				  in_decl);
	tree e2 = tsubst (TREE_OPERAND (t, 1), args, complain, in_decl);
	if (e1 == error_mark_node || e2 == error_mark_node)
	  return error_mark_node;

	return build_parse_node (TREE_CODE (t), e1, e2);
      }

    case TYPEOF_TYPE:
      {
	tree e1 = tsubst_expr (TYPE_FIELDS (t), args, complain,
			       in_decl);
	if (e1 == error_mark_node)
	  return error_mark_node;

	return TREE_TYPE (e1); 
      }

    case FUNCTION_NAME:
      {
	const char *name;
	int len;
	tree type;
	tree str;

	/* This code should match declare_hidden_char_array in
	   c-common.c.  */
	name = (*decl_printable_name) (current_function_decl, 2);
	len = strlen (name) + 1;
	type =  build_array_type (char_type_node,
				  build_index_type (build_int_2 (len, 0)));
	str = build_string (len, name);
	TREE_TYPE (str) = type;
	return str;
      }

    default:
      sorry ("use of `%s' in template",
	     tree_code_name [(int) TREE_CODE (t)]);
      return error_mark_node;
    }
}

/* Like tsubst, but deals with expressions.  This function just replaces
   template parms; to finish processing the resultant expression, use
   tsubst_expr.  */

tree
tsubst_copy (t, args, complain, in_decl)
     tree t, args;
     int complain;
     tree in_decl;
{
  enum tree_code code;
  tree r;

  if (t == NULL_TREE || t == error_mark_node)
    return t;

  code = TREE_CODE (t);

  switch (code)
    {
    case PARM_DECL:
      return do_identifier (DECL_NAME (t), 0, NULL_TREE);

    case CONST_DECL:
      {
	tree enum_type;
	tree v;

	if (!DECL_CONTEXT (t))
	  /* This is a global enumeration constant.  */
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
	  = tsubst_aggr_type (TREE_TYPE (t), args, complain, in_decl, 
			      /*entering_scope=*/0);

	for (v = TYPE_VALUES (enum_type); 
	     v != NULL_TREE; 
	     v = TREE_CHAIN (v))
	  if (TREE_PURPOSE (v) == DECL_NAME (t))
	    return TREE_VALUE (v);

	  /* We didn't find the name.  That should never happen; if
	     name-lookup found it during preliminary parsing, we
	     should find it again here during instantiation.  */
	my_friendly_abort (0);
      }
      return t;

    case FIELD_DECL:
      if (DECL_CONTEXT (t))
	{
	  tree ctx;

	  ctx = tsubst_aggr_type (DECL_CONTEXT (t), args, complain, in_decl,
				  /*entering_scope=*/1);
	  if (ctx != DECL_CONTEXT (t))
	    return lookup_field (ctx, DECL_NAME (t), 0, 0);
	}
      return t;

    case VAR_DECL:
    case FUNCTION_DECL:
      if (DECL_LANG_SPECIFIC (t) && DECL_TEMPLATE_INFO (t))
	t = tsubst (t, args, complain, in_decl);
      mark_used (t);
      return t;

    case TEMPLATE_DECL:
      if (is_member_template (t))
	return tsubst (t, args, complain, in_decl);
      else
	return t;

    case LOOKUP_EXPR:
      {
	/* We must tsbust into a LOOKUP_EXPR in case the names to
	   which it refers is a conversion operator; in that case the
	   name will change.  We avoid making unnecessary copies,
	   however.  */
	
	tree id = tsubst_copy (TREE_OPERAND (t, 0), args, complain, in_decl);

	if (id != TREE_OPERAND (t, 0))
	  {
	    r = build_nt (LOOKUP_EXPR, id);
	    LOOKUP_EXPR_GLOBAL (r) = LOOKUP_EXPR_GLOBAL (t);
	    t = r;
	  }

	return t;
      }

    case CAST_EXPR:
    case REINTERPRET_CAST_EXPR:
    case CONST_CAST_EXPR:
    case STATIC_CAST_EXPR:
    case DYNAMIC_CAST_EXPR:
    case NOP_EXPR:
      return build1
	(code, tsubst (TREE_TYPE (t), args, complain, in_decl),
	 tsubst_copy (TREE_OPERAND (t, 0), args, complain, in_decl));

    case INDIRECT_REF:
    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case NEGATE_EXPR:
    case TRUTH_NOT_EXPR:
    case BIT_NOT_EXPR:
    case ADDR_EXPR:
    case CONVERT_EXPR:      /* Unary + */
    case SIZEOF_EXPR:
    case ALIGNOF_EXPR:
    case ARROW_EXPR:
    case THROW_EXPR:
    case TYPEID_EXPR:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
      return build1
	(code, tsubst (TREE_TYPE (t), args, complain, in_decl),
	 tsubst_copy (TREE_OPERAND (t, 0), args, complain, in_decl));

    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case EXACT_DIV_EXPR:
    case BIT_AND_EXPR:
    case BIT_ANDTC_EXPR:
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
    case COMPONENT_REF:
    case ARRAY_REF:
    case COMPOUND_EXPR:
    case SCOPE_REF:
    case DOTSTAR_EXPR:
    case MEMBER_REF:
      return build_nt
	(code, tsubst_copy (TREE_OPERAND (t, 0), args, complain, in_decl),
	 tsubst_copy (TREE_OPERAND (t, 1), args, complain, in_decl));

    case CALL_EXPR:
      {
	tree fn = TREE_OPERAND (t, 0);
	if (is_overloaded_fn (fn))
	  fn = tsubst_copy (get_first_fn (fn), args, complain, in_decl);
	else
	  /* Sometimes FN is a LOOKUP_EXPR.  */
	  fn = tsubst_copy (fn, args, complain, in_decl);
	return build_nt
	  (code, fn, tsubst_copy (TREE_OPERAND (t, 1), args, complain,
				  in_decl),
	   NULL_TREE);
      }

    case METHOD_CALL_EXPR:
      {
	tree name = TREE_OPERAND (t, 0);
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
	    name = build_nt (SCOPE_REF, base, name);
	  }
	else
	  name = tsubst_copy (TREE_OPERAND (t, 0), args, complain, in_decl);
	return build_nt
	  (code, name, tsubst_copy (TREE_OPERAND (t, 1), args,
				    complain, in_decl),
	   tsubst_copy (TREE_OPERAND (t, 2), args, complain, in_decl),
	   NULL_TREE);
      }

    case STMT_EXPR:
      /* This processing should really occur in tsubst_expr, However,
	 tsubst_expr does not recurse into expressions, since it
	 assumes that there aren't any statements inside them.
	 Instead, it simply calls build_expr_from_tree.  So, we need
	 to expand the STMT_EXPR here.  */
      if (!processing_template_decl)
	{
	  tree stmt_expr = begin_stmt_expr ();
	  tsubst_expr (STMT_EXPR_STMT (t), args,
		       complain, in_decl);
	  return finish_stmt_expr (stmt_expr);
	}
      
      return t;

    case COND_EXPR:
    case MODOP_EXPR:
    case PSEUDO_DTOR_EXPR:
      {
	r = build_nt
	  (code, tsubst_copy (TREE_OPERAND (t, 0), args, complain, in_decl),
	   tsubst_copy (TREE_OPERAND (t, 1), args, complain, in_decl),
	   tsubst_copy (TREE_OPERAND (t, 2), args, complain, in_decl));
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
	tree targs = tsubst_copy (TREE_OPERAND (t, 1), args, complain,
				  in_decl);

	if (targs && TREE_CODE (targs) == TREE_LIST)
	  {
	    tree chain;
	    for (chain = targs; chain; chain = TREE_CHAIN (chain))
	      TREE_VALUE (chain) = maybe_fold_nontype_arg (TREE_VALUE (chain));
	  }
	else if (targs)
	  {
	    int i;
	    for (i = 0; i < TREE_VEC_LENGTH (targs); ++i)
	      TREE_VEC_ELT (targs, i) 
		= maybe_fold_nontype_arg (TREE_VEC_ELT (targs, i));
	  }

	return lookup_template_function
	  (tsubst_copy (TREE_OPERAND (t, 0), args, complain, in_decl), targs);
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
    case TEMPLATE_PARM_INDEX:
    case POINTER_TYPE:
    case REFERENCE_TYPE:
    case OFFSET_TYPE:
    case FUNCTION_TYPE:
    case METHOD_TYPE:
    case ARRAY_TYPE:
    case TYPENAME_TYPE:
    case TYPE_DECL:
      return tsubst (t, args, complain, in_decl);

    case IDENTIFIER_NODE:
      if (IDENTIFIER_TYPENAME_P (t)
	  /* Make sure it's not just a variable named `__opr', for instance,
	     which can occur in some existing code.  */
	  && TREE_TYPE (t))
	return build_typename_overload
	  (tsubst (TREE_TYPE (t), args, complain, in_decl));
      else
	return t;

    case CONSTRUCTOR:
      {
	r = build
	  (CONSTRUCTOR, tsubst (TREE_TYPE (t), args, complain, in_decl), 
	   NULL_TREE, tsubst_copy (CONSTRUCTOR_ELTS (t), args,
				   complain, in_decl));
	TREE_HAS_CONSTRUCTOR (r) = TREE_HAS_CONSTRUCTOR (t);
	return r;
      }

    case VA_ARG_EXPR:
      return build_va_arg (tsubst_copy (TREE_OPERAND (t, 0), args, complain,
					in_decl),
			   tsubst (TREE_TYPE (t), args, complain, in_decl));

    case FUNCTION_NAME:
      return tsubst (t, args, complain, in_decl);

    default:
      return t;
    }
}

/* Like tsubst_copy, but also does semantic processing.  */

tree
tsubst_expr (t, args, complain, in_decl)
     tree t, args;
     int complain;
     tree in_decl;
{
  tree stmt;

  if (t == NULL_TREE || t == error_mark_node)
    return t;

  if (processing_template_decl)
    return tsubst_copy (t, args, complain, in_decl);

  switch (TREE_CODE (t))
    {
    case RETURN_INIT:
      prep_stmt (t);
      finish_named_return_value
	(TREE_OPERAND (t, 0),
	 tsubst_expr (TREE_OPERAND (t, 1), args, /*complain=*/1, in_decl));
      tsubst_expr (TREE_CHAIN (t), args, complain, in_decl);
      break;

    case CTOR_INITIALIZER:
      prep_stmt (t);
      current_member_init_list
	= tsubst_expr_values (TREE_OPERAND (t, 0), args);
      current_base_init_list
	= tsubst_expr_values (TREE_OPERAND (t, 1), args);
      setup_vtbl_ptr ();
      tsubst_expr (TREE_CHAIN (t), args, complain, in_decl);
      break;

    case RETURN_STMT:
      prep_stmt (t);
      finish_return_stmt (tsubst_expr (RETURN_EXPR (t),
				       args, complain, in_decl));
      break;

    case EXPR_STMT:
      prep_stmt (t);
      finish_expr_stmt (tsubst_expr (EXPR_STMT_EXPR (t),
				     args, complain, in_decl));
      break;

    case DECL_STMT:
      {
	tree decl;
	tree init;

	prep_stmt (t);
	decl = DECL_STMT_DECL (t);
	if (TREE_CODE (decl) == LABEL_DECL)
	  finish_label_decl (DECL_NAME (decl));
	else
	  {
	    init = DECL_INITIAL (decl);
	    decl = tsubst (decl, args, complain, in_decl);
	    init = tsubst_expr (init, args, complain, in_decl);
	    if (init)
	      DECL_INITIAL (decl) = error_mark_node;
	    /* By marking the declaration as instantiated, we avoid
	       trying to instantiate it.  Since instantiate_decl can't
	       handle local variables, and since we've already done
	       all that needs to be done, that's the right thing to
	       do.  */
	    if (TREE_CODE (decl) == VAR_DECL)
	      DECL_TEMPLATE_INSTANTIATED (decl) = 1;
	    maybe_push_decl (decl);
	    cp_finish_decl (decl, init, NULL_TREE, 0);
	  }
	return decl;
      }

    case FOR_STMT:
      {
	tree tmp;
	prep_stmt (t);

	stmt = begin_for_stmt ();
	for (tmp = FOR_INIT_STMT (t); tmp; tmp = TREE_CHAIN (tmp))
	  tsubst_expr (tmp, args, complain, in_decl);
	finish_for_init_stmt (stmt);
	finish_for_cond (tsubst_expr (FOR_COND (t), args,
				      complain, in_decl),
			 stmt);
	tmp = tsubst_expr (FOR_EXPR (t), args, complain, in_decl);
	finish_for_expr (tmp, stmt);
	tsubst_expr (FOR_BODY (t), args, complain, in_decl);
	finish_for_stmt (tmp, stmt);
      }
      break;

    case WHILE_STMT:
      {
	prep_stmt (t);
	stmt = begin_while_stmt ();
	finish_while_stmt_cond (tsubst_expr (WHILE_COND (t),
					     args, complain, in_decl),
				stmt);
	tsubst_expr (WHILE_BODY (t), args, complain, in_decl);
	finish_while_stmt (stmt);
      }
      break;

    case DO_STMT:
      {
	prep_stmt (t);
	stmt = begin_do_stmt ();
	tsubst_expr (DO_BODY (t), args, complain, in_decl);
	finish_do_body (stmt);
	finish_do_stmt (tsubst_expr (DO_COND (t), args,
				     complain, in_decl),
			stmt);
      }
      break;

    case IF_STMT:
      {
	tree tmp;

	prep_stmt (t);
	stmt = begin_if_stmt ();
	finish_if_stmt_cond (tsubst_expr (IF_COND (t),
					  args, complain, in_decl),
			     stmt);

	if (tmp = THEN_CLAUSE (t), tmp)
	  {
	    tsubst_expr (tmp, args, complain, in_decl);
	    finish_then_clause (stmt);
	  }

	if (tmp = ELSE_CLAUSE (t), tmp)
	  {
	    begin_else_clause ();
	    tsubst_expr (tmp, args, complain, in_decl);
	    finish_else_clause (stmt);
	  }

	finish_if_stmt ();
      }
      break;

    case COMPOUND_STMT:
      {
	tree substmt;

	prep_stmt (t);
	stmt = begin_compound_stmt (COMPOUND_STMT_NO_SCOPE (t));
	for (substmt = COMPOUND_BODY (t); 
	     substmt != NULL_TREE;
	     substmt = TREE_CHAIN (substmt))
	  tsubst_expr (substmt, args, complain, in_decl);
	return finish_compound_stmt (COMPOUND_STMT_NO_SCOPE (t), stmt);
      }
      break;

    case BREAK_STMT:
      prep_stmt (t);
      finish_break_stmt ();
      break;

    case CONTINUE_STMT:
      prep_stmt (t);
      finish_continue_stmt ();
      break;

    case SWITCH_STMT:
      {
	tree val;

	prep_stmt (t);
	stmt = begin_switch_stmt ();
	val = tsubst_expr (SWITCH_COND (t), args, complain, in_decl);
	finish_switch_cond (val, stmt);
	tsubst_expr (SWITCH_BODY (t), args, complain, in_decl);
	finish_switch_stmt (val, stmt);
      }
      break;

    case CASE_LABEL:
      prep_stmt (t);
      finish_case_label (tsubst_expr (CASE_LOW (t), args, complain, in_decl),
			 tsubst_expr (CASE_HIGH (t), args, complain, in_decl));
      break;

    case LABEL_STMT:
      lineno = STMT_LINENO (t);
      finish_label_stmt (DECL_NAME (LABEL_STMT_LABEL (t)));
      break;

    case GOTO_STMT:
      prep_stmt (t);
      t = GOTO_DESTINATION (t);
      if (TREE_CODE (t) != LABEL_DECL)
	/* Computed goto's must be tsubst'd into.  On the other hand,
	   non-computed gotos must not be; the identifier in question
	   will have no binding.  */
	t = tsubst_expr (t, args, complain, in_decl);
      else
	t = DECL_NAME (t);
      finish_goto_stmt (t);
      break;

    case ASM_STMT:
      prep_stmt (t);
      finish_asm_stmt (ASM_CV_QUAL (t),
		       tsubst_expr (ASM_STRING (t), args, complain, in_decl),
		       tsubst_expr (ASM_OUTPUTS (t), args, complain, in_decl),
		       tsubst_expr (ASM_INPUTS (t), args, complain, in_decl), 
		       tsubst_expr (ASM_CLOBBERS (t), args, complain,
				    in_decl));
      break;

    case TRY_BLOCK:
      prep_stmt (t);
      if (CLEANUP_P (t))
	{
	  stmt = begin_try_block ();
	  tsubst_expr (TRY_STMTS (t), args, complain, in_decl);
	  finish_cleanup_try_block (stmt);
	  finish_cleanup (tsubst_expr (TRY_HANDLERS (t), args,
				       complain, in_decl),
			  stmt);
	}
      else
	{
	  tree handler;

	  if (FN_TRY_BLOCK_P (t))
	    stmt = begin_function_try_block ();
	  else
	    stmt = begin_try_block ();

	  tsubst_expr (TRY_STMTS (t), args, complain, in_decl);

	  if (FN_TRY_BLOCK_P (t))
	    finish_function_try_block (stmt);
	  else
	    finish_try_block (stmt);

	  handler = TRY_HANDLERS (t);
	  for (; handler; handler = TREE_CHAIN (handler))
	    tsubst_expr (handler, args, complain, in_decl);
	  if (FN_TRY_BLOCK_P (t))
	    finish_function_handler_sequence (stmt);
	  else
	    finish_handler_sequence (stmt);
	}
      break;
      
    case HANDLER:
      {
	tree decl;
	tree blocks;

	prep_stmt (t);
	stmt = begin_handler ();
	if (HANDLER_PARMS (t))
	  {
	    decl = DECL_STMT_DECL (HANDLER_PARMS (t));
	    decl = tsubst (decl, args, complain, in_decl);
	    /* Prevent instantiate_decl from trying to instantiate
	       this variable.  We've already done all that needs to be
	       done.  */
	    DECL_TEMPLATE_INSTANTIATED (decl) = 1;
	  }
	else
	  decl = NULL_TREE;
	blocks = finish_handler_parms (decl, stmt);
	tsubst_expr (HANDLER_BODY (t), args, complain, in_decl);
	finish_handler (blocks, stmt);
      }
      break;

    case TAG_DEFN:
      prep_stmt (t);
      t = TREE_TYPE (t);
      tsubst (t, args, complain, NULL_TREE);
      break;

    default:
      return build_expr_from_tree (tsubst_copy (t, args, complain, in_decl));
    }
  return NULL_TREE;
}

/* Instantiate the indicated variable or function template TMPL with
   the template arguments in TARG_PTR.  */

tree
instantiate_template (tmpl, targ_ptr)
     tree tmpl, targ_ptr;
{
  tree fndecl;
  tree gen_tmpl;
  tree spec;
  int i, len;
  tree inner_args;

  if (tmpl == error_mark_node)
    return error_mark_node;

  my_friendly_assert (TREE_CODE (tmpl) == TEMPLATE_DECL, 283);

  /* Check to see if we already have this specialization.  */
  spec = retrieve_specialization (tmpl, targ_ptr);
  if (spec != NULL_TREE)
    return spec;

  if (DECL_TEMPLATE_INFO (tmpl) && !DECL_TEMPLATE_SPECIALIZATION (tmpl))
    {
      /* The TMPL is a partial instantiation.  To get a full set of
	 arguments we must add the arguments used to perform the
	 partial instantiation.  */
      targ_ptr = add_outermost_template_args (DECL_TI_ARGS (tmpl),
					      targ_ptr);
      gen_tmpl = most_general_template (tmpl);

      /* Check to see if we already have this specialization.  */
      spec = retrieve_specialization (gen_tmpl, targ_ptr);
      if (spec != NULL_TREE)
	return spec;
    }
  else
    gen_tmpl = tmpl;

  len = DECL_NTPARMS (gen_tmpl);
  inner_args = innermost_args (targ_ptr);
  i = len;
  while (i--)
    {
      tree t = TREE_VEC_ELT (inner_args, i);
      if (TYPE_P (t))
	{
	  tree nt = target_type (t);
	  if (IS_AGGR_TYPE (nt) && decl_function_context (TYPE_MAIN_DECL (nt)))
	    {
	      cp_error ("type `%T' composed from a local class is not a valid template-argument", t);
	      cp_error ("  trying to instantiate `%D'", gen_tmpl);
	      fndecl = error_mark_node;
	      goto out;
	    }
	}
    }

  /* substitute template parameters */
  fndecl = tsubst (DECL_RESULT (gen_tmpl), targ_ptr, /*complain=*/1, gen_tmpl);
  /* The DECL_TI_TEMPLATE should always be the immediate parent
     template, not the most general template.  */
  DECL_TI_TEMPLATE (fndecl) = tmpl;

  if (flag_external_templates)
    add_pending_template (fndecl);

 out:
  return fndecl;
}

/* Push the name of the class template into the scope of the instantiation.  */

void
overload_template_name (type)
     tree type;
{
  tree id = DECL_NAME (CLASSTYPE_TI_TEMPLATE (type));
  tree decl;

  if (IDENTIFIER_CLASS_VALUE (id)
      && TREE_TYPE (IDENTIFIER_CLASS_VALUE (id)) == type)
    return;

  decl = build_decl (TYPE_DECL, id, type);
  SET_DECL_ARTIFICIAL (decl);
  pushdecl_class_level (decl);
}

/* The FN is a TEMPLATE_DECL for a function.  The ARGS are the
   arguments that are being used when calling it.  TARGS is a vector
   into which the deduced template arguments are placed.  

   Return zero for success, 2 for an incomplete match that doesn't resolve
   all the types, and 1 for complete failure.  An error message will be
   printed only for an incomplete match.

   If FN is a conversion operator, RETURN_TYPE is the type desired as
   the result of the conversion operator.

   TPARMS is a vector of template parameters.

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
     We are deducing arguments when calculating the partial
     ordering between specializations of function or class
     templates, as in [temp.func.order] and [temp.class.order],
     when doing an explicit instantiation as in [temp.explicit],
     when determining an explicit specialization as in
     [temp.expl.spec], or when taking the address of a function
     template, as in [temp.deduct.funcaddr]. 

   The other arguments are as for type_unification.  */

int
fn_type_unification (fn, explicit_targs, targs, args, return_type,
		     strict)
     tree fn, explicit_targs, targs, args, return_type;
     unification_kind_t strict;
{
  tree parms;
  tree fntype;
  int result;

  my_friendly_assert (TREE_CODE (fn) == TEMPLATE_DECL, 0);
  
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
      int i;
      tree converted_args;

      converted_args
	= (coerce_template_parms (DECL_INNERMOST_TEMPLATE_PARMS (fn), 
				  explicit_targs, NULL_TREE, /*complain=*/0, 
				  /*require_all_arguments=*/0));
      if (converted_args == error_mark_node)
	return 1;

      fntype = tsubst (fntype, converted_args, /*complain=*/0, NULL_TREE);
      if (fntype == error_mark_node)
	return 1;

      /* Place the explicitly specified arguments in TARGS.  */
      for (i = 0; i < TREE_VEC_LENGTH (targs); i++)
	TREE_VEC_ELT (targs, i) = TREE_VEC_ELT (converted_args, i);
    }
     
  parms = TYPE_ARG_TYPES (fntype);

  if (DECL_CONV_FN_P (fn))
    {
      /* This is a template conversion operator.  Remove `this', since
         we could be comparing conversions from different classes.  */
      parms = TREE_CHAIN (parms);
      args = TREE_CHAIN (args);
      my_friendly_assert (return_type != NULL_TREE, 20000227);
    }
  
  if (return_type)
    {
      /* We've been given a return type to match, prepend it.  */
      parms = tree_cons (NULL_TREE, TREE_TYPE (fntype), parms);
      args = tree_cons (NULL_TREE, return_type, args);
    }

  /* We allow incomplete unification without an error message here
     because the standard doesn't seem to explicitly prohibit it.  Our
     callers must be ready to deal with unification failures in any
     event.  */
  result = type_unification_real (DECL_INNERMOST_TEMPLATE_PARMS (fn), 
				  targs, parms, args, /*subr=*/0,
				  strict, /*allow_incomplete*/1);

  if (result == 0) 
    /* All is well so far.  Now, check:
       
       [temp.deduct] 
       
       When all template arguments have been deduced, all uses of
       template parameters in nondeduced contexts are replaced with
       the corresponding deduced argument values.  If the
       substitution results in an invalid type, as described above,
       type deduction fails.  */
    if (tsubst (TREE_TYPE (fn), targs, /*complain=*/0, NULL_TREE)
	== error_mark_node)
      return 1;

  return result;
}

/* Adjust types before performing type deduction, as described in
   [temp.deduct.call] and [temp.deduct.conv].  The rules in these two
   sections are symmetric.  PARM is the type of a function parameter
   or the return type of the conversion function.  ARG is the type of
   the argument passed to the call, or the type of the value
   intialized with the result of the conversion function.  */

static void
maybe_adjust_types_for_deduction (strict, parm, arg)
     unification_kind_t strict;
     tree* parm;
     tree* arg;
{
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
      /* There is nothing to do in this case.  */
      return;

    default:
      my_friendly_abort (0);
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
  
  /* [temp.deduct.call]
     
     If P is a cv-qualified type, the top level cv-qualifiers
     of P's type are ignored for type deduction.  If P is a
     reference type, the type referred to by P is used for
     type deduction.  */
  *parm = TYPE_MAIN_VARIANT (*parm);
  if (TREE_CODE (*parm) == REFERENCE_TYPE)
    *parm = TREE_TYPE (*parm);
}

/* Like type_unfication.

   If SUBR is 1, we're being called recursively (to unify the
   arguments of a function or method parameter of a function
   template).  */

static int
type_unification_real (tparms, targs, parms, args, subr,
		       strict, allow_incomplete)
     tree tparms, targs, parms, args;
     int subr;
     unification_kind_t strict;
     int allow_incomplete;
{
  tree parm, arg;
  int i;
  int ntparms = TREE_VEC_LENGTH (tparms);
  int sub_strict;

  my_friendly_assert (TREE_CODE (tparms) == TREE_VEC, 289);
  my_friendly_assert (parms == NULL_TREE 
		      || TREE_CODE (parms) == TREE_LIST, 290);
  /* ARGS could be NULL (via a call from parse.y to
     build_x_function_call).  */
  if (args)
    my_friendly_assert (TREE_CODE (args) == TREE_LIST, 291);
  my_friendly_assert (ntparms > 0, 292);

  switch (strict)
    {
    case DEDUCE_CALL:
      sub_strict = UNIFY_ALLOW_MORE_CV_QUAL | UNIFY_ALLOW_DERIVED;
      break;
      
    case DEDUCE_CONV:
      sub_strict = UNIFY_ALLOW_LESS_CV_QUAL;
      break;

    case DEDUCE_EXACT:
      sub_strict = UNIFY_ALLOW_NONE;
      break;
      
    default:
      my_friendly_abort (0);
    }

  while (parms
	 && parms != void_list_node
	 && args
	 && args != void_list_node)
    {
      parm = TREE_VALUE (parms);
      parms = TREE_CHAIN (parms);
      arg = TREE_VALUE (args);
      args = TREE_CHAIN (args);

      if (arg == error_mark_node)
	return 1;
      if (arg == unknown_type_node)
	/* We can't deduce anything from this, but we might get all the
	   template args from other function args.  */
	continue;

      /* Conversions will be performed on a function argument that
	 corresponds with a function parameter that contains only
	 non-deducible template parameters and explicitly specified
	 template parameters.  */
      if (! uses_template_parms (parm))
	{
	  tree type;

	  if (!TYPE_P (arg))
	    type = TREE_TYPE (arg);
	  else
	    {
	      type = arg;
	      arg = NULL_TREE;
	    }

	  if (strict == DEDUCE_EXACT)
	    {
	      if (same_type_p (parm, type))
		continue;
	    }
	  else
	    /* It might work; we shouldn't check now, because we might
	       get into infinite recursion.  Overload resolution will
	       handle it.  */
	    continue;

	  return 1;
	}
	
      if (!TYPE_P (arg))
	{
	  my_friendly_assert (TREE_TYPE (arg) != NULL_TREE, 293);
	  if (type_unknown_p (arg))
	    {
	      /* [temp.deduct.type] A template-argument can be deduced from
		 a pointer to function or pointer to member function
		 argument if the set of overloaded functions does not
		 contain function templates and at most one of a set of
		 overloaded functions provides a unique match.  */

	      if (resolve_overloaded_unification
		  (tparms, targs, parm, arg, strict, sub_strict)
		  != 0)
		return 1;
	      continue;
	    }
	  arg = TREE_TYPE (arg);
	}

      if (!subr)
	maybe_adjust_types_for_deduction (strict, &parm, &arg);

      switch (unify (tparms, targs, parm, arg, sub_strict))
	{
	case 0:
	  break;
	case 1:
	  return 1;
	}
    }
  /* Fail if we've reached the end of the parm list, and more args
     are present, and the parm list isn't variadic.  */
  if (args && args != void_list_node && parms == void_list_node)
    return 1;
  /* Fail if parms are left and they don't have default values.	 */
  if (parms
      && parms != void_list_node
      && TREE_PURPOSE (parms) == NULL_TREE)
    return 1;
  if (!subr)
    for (i = 0; i < ntparms; i++)
      if (TREE_VEC_ELT (targs, i) == NULL_TREE)
	{
	  if (!allow_incomplete)
	    error ("incomplete type unification");
	  return 2;
	}
  return 0;
}

/* Subroutine of type_unification_real.  Args are like the variables at the
   call site.  ARG is an overloaded function (or template-id); we try
   deducing template args from each of the overloads, and if only one
   succeeds, we go with that.  Modifies TARGS and returns 0 on success.  */

static int
resolve_overloaded_unification (tparms, targs, parm, arg, strict,
				sub_strict)
     tree tparms, targs, parm, arg;
     unification_kind_t strict;
     int sub_strict;
{
  tree tempargs = copy_node (targs);
  int good = 0;

  if (TREE_CODE (arg) == ADDR_EXPR)
    arg = TREE_OPERAND (arg, 0);

  if (TREE_CODE (arg) == COMPONENT_REF)
    /* Handle `&x' where `x' is some static or non-static member
       function name.  */
    arg = TREE_OPERAND (arg, 1);

  if (TREE_CODE (arg) == OFFSET_REF)
    arg = TREE_OPERAND (arg, 1);

  /* Strip baselink information.  */
  while (TREE_CODE (arg) == TREE_LIST)
    arg = TREE_VALUE (arg);

  if (TREE_CODE (arg) == TEMPLATE_ID_EXPR)
    {
      /* If we got some explicit template args, we need to plug them into
	 the affected templates before we try to unify, in case the
	 explicit args will completely resolve the templates in question.  */

      tree expl_subargs = TREE_OPERAND (arg, 1);
      arg = TREE_OPERAND (arg, 0);

      for (; arg; arg = OVL_NEXT (arg))
	{
	  tree fn = OVL_CURRENT (arg);
	  tree subargs, elem;

	  if (TREE_CODE (fn) != TEMPLATE_DECL)
	    continue;

	  subargs = get_bindings_overload (fn, DECL_RESULT (fn), expl_subargs);
	  if (subargs)
	    {
	      elem = tsubst (TREE_TYPE (fn), subargs, /*complain=*/0,
			     NULL_TREE);
	      if (TREE_CODE (elem) == METHOD_TYPE)
		elem = build_ptrmemfunc_type (build_pointer_type (elem));
	      good += try_one_overload (tparms, targs, tempargs, parm, elem,
					strict, sub_strict);
	    }
	}
    }
  else if (TREE_CODE (arg) == OVERLOAD)
    {
      for (; arg; arg = OVL_NEXT (arg))
	{
	  tree type = TREE_TYPE (OVL_CURRENT (arg));
	  if (TREE_CODE (type) == METHOD_TYPE)
	    type = build_ptrmemfunc_type (build_pointer_type (type));
	  good += try_one_overload (tparms, targs, tempargs, parm,
				    type,
				    strict, sub_strict);
	}
    }
  else
    my_friendly_abort (981006);

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
    return 0;

  return 1;
}

/* Subroutine of resolve_overloaded_unification; does deduction for a single
   overload.  Fills TARGS with any deduced arguments, or error_mark_node if
   different overloads deduce different arguments for a given parm.
   Returns 1 on success.  */

static int
try_one_overload (tparms, orig_targs, targs, parm, arg, strict,
		  sub_strict)
     tree tparms, orig_targs, targs, parm, arg;
     unification_kind_t strict;
     int sub_strict;
{
  int nargs;
  tree tempargs;
  int i;

  /* [temp.deduct.type] A template-argument can be deduced from a pointer
     to function or pointer to member function argument if the set of
     overloaded functions does not contain function templates and at most
     one of a set of overloaded functions provides a unique match.

     So if this is a template, just return success.  */

  if (uses_template_parms (arg))
    return 1;

  maybe_adjust_types_for_deduction (strict, &parm, &arg);

  /* We don't copy orig_targs for this because if we have already deduced
     some template args from previous args, unify would complain when we
     try to deduce a template parameter for the same argument, even though
     there isn't really a conflict.  */
  nargs = TREE_VEC_LENGTH (targs);
  tempargs = make_tree_vec (nargs);

  if (unify (tparms, tempargs, parm, arg, sub_strict) != 0)
    return 0;

  /* First make sure we didn't deduce anything that conflicts with
     explicitly specified args.  */
  for (i = nargs; i--; )
    {
      tree elt = TREE_VEC_ELT (tempargs, i);
      tree oldelt = TREE_VEC_ELT (orig_targs, i);

      if (elt == NULL_TREE)
	continue;
      else if (uses_template_parms (elt))
	{
	  /* Since we're unifying against ourselves, we will fill in template
	     args used in the function parm list with our own template parms.
	     Discard them.  */
	  TREE_VEC_ELT (tempargs, i) = NULL_TREE;
	  continue;
	}
      else if (oldelt && ! template_args_equal (oldelt, elt))
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
try_class_unification (tparms, targs, parm, arg)
     tree tparms;
     tree targs;
     tree parm;
     tree arg;
{
  int i;
  tree copy_of_targs;

  if (!CLASSTYPE_TEMPLATE_INFO (arg)
      || CLASSTYPE_TI_TEMPLATE (arg) != CLASSTYPE_TI_TEMPLATE (parm))
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
     `S<0, 1, 2>' is derived from `S<0, 0, 0>', the code is not legal
     because there are two ways to unify base classes of S<0, 1, 2>
     with S<I, I, I>.  If we kept the already deduced knowledge, we
     would reject the possibility I=1.  */
  copy_of_targs = make_tree_vec (TREE_VEC_LENGTH (targs));
  i = unify (tparms, copy_of_targs, CLASSTYPE_TI_ARGS (parm),
	     CLASSTYPE_TI_ARGS (arg), UNIFY_ALLOW_NONE);
  
  /* If unification failed, we're done.  */
  if (i != 0)
    return NULL_TREE;
  else
    return arg;
}

/* Subroutine of get_template_base.  RVAL, if non-NULL, is a base we
   have alreay discovered to be satisfactory.  ARG_BINFO is the binfo
   for the base class of ARG that we are currently examining.  */

static tree
get_template_base_recursive (tparms, targs, parm,
			     arg_binfo, rval, flags)
     tree tparms;
     tree targs;
     tree arg_binfo;
     tree rval;
     tree parm;
     int flags;
{
  tree binfos;
  int i, n_baselinks;
  tree arg = BINFO_TYPE (arg_binfo);

  if (!(flags & GTB_IGNORE_TYPE))
    {
      tree r = try_class_unification (tparms, targs,
				      parm, arg);

      /* If there is more than one satisfactory baseclass, then:

	   [temp.deduct.call]

	   If they yield more than one possible deduced A, the type
	   deduction fails.

	   applies.  */
      if (r && rval && !same_type_p (r, rval))
	return error_mark_node;
      else if (r)
	rval = r;
    }

  binfos = BINFO_BASETYPES (arg_binfo);
  n_baselinks = binfos ? TREE_VEC_LENGTH (binfos) : 0;

  /* Process base types.  */
  for (i = 0; i < n_baselinks; i++)
    {
      tree base_binfo = TREE_VEC_ELT (binfos, i);
      int this_virtual;

      /* Skip this base, if we've already seen it.  */
      if (BINFO_MARKED (base_binfo))
	continue;

      this_virtual = 
	(flags & GTB_VIA_VIRTUAL) || TREE_VIA_VIRTUAL (base_binfo);
      
      /* When searching for a non-virtual, we cannot mark virtually
	 found binfos.  */
      if (! this_virtual)
	SET_BINFO_MARKED (base_binfo);
      
      rval = get_template_base_recursive (tparms, targs,
					  parm,
					  base_binfo, 
					  rval,
					  GTB_VIA_VIRTUAL * this_virtual);
      
      /* If we discovered more than one matching base class, we can
	 stop now.  */
      if (rval == error_mark_node)
	return error_mark_node;
    }

  return rval;
}

/* Given a template type PARM and a class type ARG, find the unique
   base type in ARG that is an instance of PARM.  We do not examine
   ARG itself; only its base-classes.  If there is no appropriate base
   class, return NULL_TREE.  If there is more than one, return
   error_mark_node.  PARM may be the type of a partial specialization,
   as well as a plain template type.  Used by unify.  */

static tree
get_template_base (tparms, targs, parm, arg)
     tree tparms;
     tree targs;
     tree parm;
     tree arg;
{
  tree rval;
  tree arg_binfo;

  my_friendly_assert (IS_AGGR_TYPE_CODE (TREE_CODE (arg)), 92);
  
  arg_binfo = TYPE_BINFO (complete_type (arg));
  rval = get_template_base_recursive (tparms, targs,
				      parm, arg_binfo, 
				      NULL_TREE,
				      GTB_IGNORE_TYPE);

  /* Since get_template_base_recursive marks the bases classes, we
     must unmark them here.  */
  dfs_walk (arg_binfo, dfs_unmark, markedp, 0);

  return rval;
}

/* Returns the level of DECL, which declares a template parameter.  */

static int
template_decl_level (decl)
     tree decl;
{
  switch (TREE_CODE (decl))
    {
    case TYPE_DECL:
    case TEMPLATE_DECL:
      return TEMPLATE_TYPE_LEVEL (TREE_TYPE (decl));

    case PARM_DECL:
      return TEMPLATE_PARM_LEVEL (DECL_INITIAL (decl));

    default:
      my_friendly_abort (0);
      return 0;
    }
}

/* Decide whether ARG can be unified with PARM, considering only the
   cv-qualifiers of each type, given STRICT as documented for unify.
   Returns non-zero iff the unification is OK on that basis.*/

static int
check_cv_quals_for_unify (strict, arg, parm)
     int strict;
     tree arg;
     tree parm;
{
  if (!(strict & UNIFY_ALLOW_MORE_CV_QUAL)
      && !at_least_as_qualified_p (arg, parm))
    return 0;

  if (!(strict & UNIFY_ALLOW_LESS_CV_QUAL)
      && !at_least_as_qualified_p (parm, arg))
    return 0;

  return 1;
}

/* Takes parameters as for type_unification.  Returns 0 if the
   type deduction suceeds, 1 otherwise.  The parameter STRICT is a
   bitwise or of the following flags:

     UNIFY_ALLOW_NONE:
       Require an exact match between PARM and ARG.
     UNIFY_ALLOW_MORE_CV_QUAL:
       Allow the deduced ARG to be more cv-qualified than ARG.
     UNIFY_ALLOW_LESS_CV_QUAL:
       Allow the deduced ARG to be less cv-qualified than ARG.
     UNIFY_ALLOW_DERIVED:
       Allow the deduced ARG to be a template base class of ARG,
       or a pointer to a template base class of the type pointed to by
       ARG.
     UNIFY_ALLOW_INTEGER:
       Allow any integral type to be deduced.  See the TEMPLATE_PARM_INDEX
       case for more information.  */

static int
unify (tparms, targs, parm, arg, strict)
     tree tparms, targs, parm, arg;
     int strict;
{
  int idx;
  tree targ;
  tree tparm;

  /* I don't think this will do the right thing with respect to types.
     But the only case I've seen it in so far has been array bounds, where
     signedness is the only information lost, and I think that will be
     okay.  */
  while (TREE_CODE (parm) == NOP_EXPR)
    parm = TREE_OPERAND (parm, 0);

  if (arg == error_mark_node)
    return 1;
  if (arg == unknown_type_node)
    /* We can't deduce anything from this, but we might get all the
       template args from other function args.  */
    return 0;

  /* If PARM uses template parameters, then we can't bail out here,
     even if ARG == PARM, since we won't record unifications for the
     template parameters.  We might need them if we're trying to
     figure out which of two things is more specialized.  */
  if (arg == parm && !uses_template_parms (parm))
    return 0;

  /* Immediately reject some pairs that won't unify because of
     cv-qualification mismatches.  */
  if (TREE_CODE (arg) == TREE_CODE (parm)
      && TYPE_P (arg)
      /* We check the cv-qualifiers when unifying with template type
	 parameters below.  We want to allow ARG `const T' to unify with
	 PARM `T' for example, when computing which of two templates
	 is more specialized, for example.  */
      && TREE_CODE (arg) != TEMPLATE_TYPE_PARM
      && !check_cv_quals_for_unify (strict, arg, parm))
    return 1;

  switch (TREE_CODE (parm))
    {
    case TYPENAME_TYPE:
      /* In a type which contains a nested-name-specifier, template
	 argument values cannot be deduced for template parameters used
	 within the nested-name-specifier.  */
      return 0;

    case TEMPLATE_TYPE_PARM:
    case TEMPLATE_TEMPLATE_PARM:
      tparm = TREE_VALUE (TREE_VEC_ELT (tparms, 0));

      if (TEMPLATE_TYPE_LEVEL (parm)
	  != template_decl_level (tparm))
	/* The PARM is not one we're trying to unify.  Just check
	   to see if it matches ARG.  */
	return (TREE_CODE (arg) == TREE_CODE (parm)
		&& same_type_p (parm, arg)) ? 0 : 1;
      idx = TEMPLATE_TYPE_IDX (parm);
      targ = TREE_VEC_ELT (targs, idx);
      tparm = TREE_VALUE (TREE_VEC_ELT (tparms, idx));

      /* Check for mixed types and values.  */
      if ((TREE_CODE (parm) == TEMPLATE_TYPE_PARM
	   && TREE_CODE (tparm) != TYPE_DECL)
	  || (TREE_CODE (parm) == TEMPLATE_TEMPLATE_PARM 
	      && TREE_CODE (tparm) != TEMPLATE_DECL))
	return 1;

      if (TREE_CODE (parm) == TEMPLATE_TEMPLATE_PARM)
	{
	  if (TEMPLATE_TEMPLATE_PARM_TEMPLATE_INFO (parm))
	    {
	      /* We arrive here when PARM does not involve template 
		 specialization.  */

	      /* ARG must be constructed from a template class.  */
	      if (TREE_CODE (arg) != RECORD_TYPE || !CLASSTYPE_TEMPLATE_INFO (arg))
		return 1;

	      {
		tree parmtmpl = TYPE_TI_TEMPLATE (parm);
		tree parmvec = TYPE_TI_ARGS (parm);
		tree argvec = CLASSTYPE_TI_ARGS (arg);
		tree argtmplvec
		  = DECL_INNERMOST_TEMPLATE_PARMS (CLASSTYPE_TI_TEMPLATE (arg));
		int i;

		/* The parameter and argument roles have to be switched here 
		   in order to handle default arguments properly.  For example, 
		   template<template <class> class TT> void f(TT<int>) 
		   should be able to accept vector<int> which comes from 
		   template <class T, class Allocator = allocator> 
		   class vector.  */

		if (coerce_template_parms (argtmplvec, parmvec, parmtmpl, 0, 1)
		    == error_mark_node)
		  return 1;
	  
		/* Deduce arguments T, i from TT<T> or TT<i>.  
		   We check each element of PARMVEC and ARGVEC individually
		   rather than the whole TREE_VEC since they can have
		   different number of elements.  */

		for (i = 0; i < TREE_VEC_LENGTH (parmvec); ++i)
		  {
		    tree t = TREE_VEC_ELT (parmvec, i);

		    if (unify (tparms, targs, t, 
			       TREE_VEC_ELT (argvec, i), 
			       UNIFY_ALLOW_NONE))
		      return 1;
		  }
	      }
	      arg = CLASSTYPE_TI_TEMPLATE (arg);
	    }
	}
      else
	{
	  /* If PARM is `const T' and ARG is only `int', we don't have
	     a match unless we are allowing additional qualification.
	     If ARG is `const int' and PARM is just `T' that's OK;
	     that binds `const int' to `T'.  */
	  if (!check_cv_quals_for_unify (strict | UNIFY_ALLOW_LESS_CV_QUAL, 
					 arg, parm))
	    return 1;

	  /* Consider the case where ARG is `const volatile int' and
	     PARM is `const T'.  Then, T should be `volatile int'.  */
	  arg = 
	    cp_build_qualified_type_real (arg,
					  CP_TYPE_QUALS (arg) 
					  & ~CP_TYPE_QUALS (parm),
					  /*complain=*/0);
	  if (arg == error_mark_node)
	    return 1;
	}

      /* Simple cases: Value already set, does match or doesn't.  */
      if (targ != NULL_TREE && same_type_p (targ, arg))
	return 0;
      else if (targ)
	return 1;

      /* Make sure that ARG is not a variable-sized array.  (Note that
	 were talking about variable-sized arrays (like `int[n]'),
	 rather than arrays of unknown size (like `int[]').)  We'll
	 get very confused by such a type since the bound of the array
	 will not be computable in an instantiation.  Besides, such
	 types are not allowed in ISO C++, so we can do as we please
	 here.  */
      if (TREE_CODE (arg) == ARRAY_TYPE 
	  && !uses_template_parms (arg)
	  && (TREE_CODE (TYPE_MAX_VALUE (TYPE_DOMAIN (arg)))
	      != INTEGER_CST))
	return 1;

      TREE_VEC_ELT (targs, idx) = arg;
      return 0;

    case TEMPLATE_PARM_INDEX:
      tparm = TREE_VALUE (TREE_VEC_ELT (tparms, 0));

      if (TEMPLATE_PARM_LEVEL (parm) 
	  != template_decl_level (tparm))
	/* The PARM is not one we're trying to unify.  Just check
	   to see if it matches ARG.  */
	return (TREE_CODE (arg) == TREE_CODE (parm)
		&& cp_tree_equal (parm, arg) > 0) ? 0 : 1;

      idx = TEMPLATE_PARM_IDX (parm);
      targ = TREE_VEC_ELT (targs, idx);

      if (targ)
	{
	  int i = (cp_tree_equal (targ, arg) > 0);
	  if (i == 1)
	    return 0;
	  else if (i == 0)
	    return 1;
	  else
	    my_friendly_abort (42);
	}

      /* [temp.deduct.type] If, in the declaration of a function template
	 with a non-type template-parameter, the non-type
	 template-parameter is used in an expression in the function
	 parameter-list and, if the corresponding template-argument is
	 deduced, the template-argument type shall match the type of the
	 template-parameter exactly, except that a template-argument
	 deduced from an array bound may be of any integral type.  */
      if (same_type_p (TREE_TYPE (arg), TREE_TYPE (parm)))
	/* OK */;
      else if ((strict & UNIFY_ALLOW_INTEGER)
	       && (TREE_CODE (TREE_TYPE (parm)) == INTEGER_TYPE
		   || TREE_CODE (TREE_TYPE (parm)) == BOOLEAN_TYPE))
	/* OK */;
      else
	return 1;

      TREE_VEC_ELT (targs, idx) = arg;
      return 0;

    case POINTER_TYPE:
      {
	int sub_strict;

	if (TREE_CODE (arg) != POINTER_TYPE)
	  return 1;
	
	/* [temp.deduct.call]

	   A can be another pointer or pointer to member type that can
	   be converted to the deduced A via a qualification
	   conversion (_conv.qual_).

	   We pass down STRICT here rather than UNIFY_ALLOW_NONE.
	   This will allow for additional cv-qualification of the
	   pointed-to types if appropriate.  In general, this is a bit
	   too generous; we are only supposed to allow qualification
	   conversions and this method will allow an ARG of char** and
	   a deduced ARG of const char**.  However, overload
	   resolution will subsequently invalidate the candidate, so
	   this is probably OK.  */
	sub_strict = strict;
	
	if (TREE_CODE (TREE_TYPE (arg)) != RECORD_TYPE)
	  /* The derived-to-base conversion only persists through one
	     level of pointers.  */
	  sub_strict &= ~UNIFY_ALLOW_DERIVED;

	return unify (tparms, targs, TREE_TYPE (parm), 
		      TREE_TYPE (arg), sub_strict);
      }

    case REFERENCE_TYPE:
      if (TREE_CODE (arg) != REFERENCE_TYPE)
	return 1;
      return unify (tparms, targs, TREE_TYPE (parm), TREE_TYPE (arg),
		    UNIFY_ALLOW_NONE);

    case ARRAY_TYPE:
      if (TREE_CODE (arg) != ARRAY_TYPE)
	return 1;
      if ((TYPE_DOMAIN (parm) == NULL_TREE)
	  != (TYPE_DOMAIN (arg) == NULL_TREE))
	return 1;
      if (TYPE_DOMAIN (parm) != NULL_TREE
	  && unify (tparms, targs, TYPE_DOMAIN (parm),
		    TYPE_DOMAIN (arg), UNIFY_ALLOW_NONE) != 0)
	return 1;
      return unify (tparms, targs, TREE_TYPE (parm), TREE_TYPE (arg),
		    UNIFY_ALLOW_NONE);

    case REAL_TYPE:
    case COMPLEX_TYPE:
    case INTEGER_TYPE:
    case BOOLEAN_TYPE:
    case VOID_TYPE:
      if (TREE_CODE (arg) != TREE_CODE (parm))
	return 1;

      if (TREE_CODE (parm) == INTEGER_TYPE
	  && TREE_CODE (TYPE_MAX_VALUE (parm)) != INTEGER_CST)
	{
	  if (TYPE_MIN_VALUE (parm) && TYPE_MIN_VALUE (arg)
	      && unify (tparms, targs, TYPE_MIN_VALUE (parm),
			TYPE_MIN_VALUE (arg), UNIFY_ALLOW_INTEGER))
	    return 1;
	  if (TYPE_MAX_VALUE (parm) && TYPE_MAX_VALUE (arg)
	      && unify (tparms, targs, TYPE_MAX_VALUE (parm),
			TYPE_MAX_VALUE (arg), UNIFY_ALLOW_INTEGER))
	    return 1;
	}
      /* We use the TYPE_MAIN_VARIANT since we have already
	 checked cv-qualification at the top of the
	 function.  */
      else if (!same_type_p (TYPE_MAIN_VARIANT (arg),
			     TYPE_MAIN_VARIANT (parm)))
	return 1;

      /* As far as unification is concerned, this wins.	 Later checks
	 will invalidate it if necessary.  */
      return 0;

      /* Types INTEGER_CST and MINUS_EXPR can come from array bounds.  */
      /* Type INTEGER_CST can come from ordinary constant template args.  */
    case INTEGER_CST:
      while (TREE_CODE (arg) == NOP_EXPR)
	arg = TREE_OPERAND (arg, 0);

      if (TREE_CODE (arg) != INTEGER_CST)
	return 1;
      return !tree_int_cst_equal (parm, arg);

    case TREE_VEC:
      {
	int i;
	if (TREE_CODE (arg) != TREE_VEC)
	  return 1;
	if (TREE_VEC_LENGTH (parm) != TREE_VEC_LENGTH (arg))
	  return 1;
	for (i = TREE_VEC_LENGTH (parm) - 1; i >= 0; i--)
	  if (unify (tparms, targs,
		     TREE_VEC_ELT (parm, i), TREE_VEC_ELT (arg, i),
		     UNIFY_ALLOW_NONE))
	    return 1;
	return 0;
      }

    case RECORD_TYPE:
    case UNION_TYPE:
      if (TREE_CODE (arg) != TREE_CODE (parm))
	return 1;
  
      if (TYPE_PTRMEMFUNC_P (parm))
	{
	  if (!TYPE_PTRMEMFUNC_P (arg))
	    return 1;

	  return unify (tparms, targs, 
			TYPE_PTRMEMFUNC_FN_TYPE (parm),
			TYPE_PTRMEMFUNC_FN_TYPE (arg),
			strict);
	}

      if (CLASSTYPE_TEMPLATE_INFO (parm))
	{
	  tree t = NULL_TREE;

	  if (strict & UNIFY_ALLOW_DERIVED)
	    {
	      /* First, we try to unify the PARM and ARG directly.  */
	      t = try_class_unification (tparms, targs,
					 parm, arg);

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
		  t = get_template_base (tparms, targs,
					 parm, arg);

		  if (! t || t == error_mark_node)
		    return 1;
		}
	    }
	  else if (CLASSTYPE_TEMPLATE_INFO (arg) 
		   && (CLASSTYPE_TI_TEMPLATE (parm) 
		       == CLASSTYPE_TI_TEMPLATE (arg)))
	    /* Perhaps PARM is something like S<U> and ARG is S<int>.
	       Then, we should unify `int' and `U'.  */
	    t = arg;
	  else
	    /* There's no chance of unication succeeding.  */
	    return 1;

	  return unify (tparms, targs, CLASSTYPE_TI_ARGS (parm),
			CLASSTYPE_TI_ARGS (t), UNIFY_ALLOW_NONE);
	}
      else if (!same_type_p (TYPE_MAIN_VARIANT (parm),
			     TYPE_MAIN_VARIANT (arg)))
	return 1;
      return 0;

    case METHOD_TYPE:
    case FUNCTION_TYPE:
      if (TREE_CODE (arg) != TREE_CODE (parm))
	return 1;

      if (unify (tparms, targs, TREE_TYPE (parm),
		 TREE_TYPE (arg), UNIFY_ALLOW_NONE))
	return 1;
      return type_unification_real (tparms, targs, TYPE_ARG_TYPES (parm),
				    TYPE_ARG_TYPES (arg), 1, 
				    DEDUCE_EXACT, 0);

    case OFFSET_TYPE:
      if (TREE_CODE (arg) != OFFSET_TYPE)
	return 1;
      if (unify (tparms, targs, TYPE_OFFSET_BASETYPE (parm),
		 TYPE_OFFSET_BASETYPE (arg), UNIFY_ALLOW_NONE))
	return 1;
      return unify (tparms, targs, TREE_TYPE (parm), TREE_TYPE (arg),
		    strict);

    case CONST_DECL:
      if (arg != decl_constant_value (parm)) 
	return 1;
      return 0;

    case TEMPLATE_DECL:
      /* Matched cases are handled by the ARG == PARM test above.  */
      return 1;

    case MINUS_EXPR:
      if (TREE_CODE (TREE_OPERAND (parm, 1)) == INTEGER_CST)
	{
	  /* We handle this case specially, since it comes up with
	     arrays.  In particular, something like:

	     template <int N> void f(int (&x)[N]);

	     Here, we are trying to unify the range type, which
	     looks like [0 ... (N - 1)].  */
	  tree t, t1, t2;
	  t1 = TREE_OPERAND (parm, 0);
	  t2 = TREE_OPERAND (parm, 1);

	  t = fold (build (PLUS_EXPR, integer_type_node, arg, t2));

	  return unify (tparms, targs, t1, t, strict);
	}
      /* else fall through */

    default:
      if (IS_EXPR_CODE_CLASS (TREE_CODE_CLASS (TREE_CODE (parm))))
	/* We're looking at an expression.  This can happen with
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
	return 0;
      else
	sorry ("use of `%s' in template type unification",
	       tree_code_name [(int) TREE_CODE (parm)]);

      return 1;
    }
}

/* Called if RESULT is explicitly instantiated, or is a member of an
   explicitly instantiated class, or if using -frepo and the
   instantiation of RESULT has been assigned to this file.  */

void
mark_decl_instantiated (result, extern_p)
     tree result;
     int extern_p;
{
  if (TREE_CODE (result) != FUNCTION_DECL)
    /* The TREE_PUBLIC flag for function declarations will have been
       set correctly by tsubst.  */
    TREE_PUBLIC (result) = 1;

  if (! extern_p)
    {
      DECL_INTERFACE_KNOWN (result) = 1;
      DECL_NOT_REALLY_EXTERN (result) = 1;

      /* Always make artificials weak.  */
      if (DECL_ARTIFICIAL (result) && flag_weak)
	comdat_linkage (result);
      /* For WIN32 we also want to put explicit instantiations in
	 linkonce sections.  */
      else if (TREE_PUBLIC (result))
	maybe_make_one_only (result);
    }
  else if (TREE_CODE (result) == FUNCTION_DECL)
    mark_inline_for_output (result);
}

/* Given two function templates PAT1 and PAT2, and explicit template
   arguments EXPLICIT_ARGS return:

   1 if PAT1 is more specialized than PAT2 as described in [temp.func.order].
   -1 if PAT2 is more specialized than PAT1.
   0 if neither is more specialized.  */
   
int
more_specialized (pat1, pat2, explicit_args)
     tree pat1, pat2, explicit_args;
{
  tree targs;
  int winner = 0;

  targs = get_bindings_overload (pat1, DECL_RESULT (pat2), explicit_args);
  if (targs)
    --winner;

  targs = get_bindings_overload (pat2, DECL_RESULT (pat1), explicit_args);
  if (targs)
    ++winner;

  return winner;
}

/* Given two class template specialization list nodes PAT1 and PAT2, return:

   1 if PAT1 is more specialized than PAT2 as described in [temp.class.order].
   -1 if PAT2 is more specialized than PAT1.
   0 if neither is more specialized.  */
   
int
more_specialized_class (pat1, pat2)
     tree pat1, pat2;
{
  tree targs;
  int winner = 0;

  targs = get_class_bindings (TREE_VALUE (pat1), TREE_PURPOSE (pat1),
			      TREE_PURPOSE (pat2));
  if (targs)
    --winner;

  targs = get_class_bindings (TREE_VALUE (pat2), TREE_PURPOSE (pat2),
			      TREE_PURPOSE (pat1));
  if (targs)
    ++winner;

  return winner;
}

/* Return the template arguments that will produce the function signature
   DECL from the function template FN, with the explicit template
   arguments EXPLICIT_ARGS.  If CHECK_RETTYPE is 1, the return type must
   also match.  Return NULL_TREE if no satisfactory arguments could be
   found.  */

static tree
get_bindings_real (fn, decl, explicit_args, check_rettype)
     tree fn, decl, explicit_args;
     int check_rettype;
{
  int ntparms = DECL_NTPARMS (fn);
  tree targs = make_tree_vec (ntparms);
  tree decl_type;
  tree decl_arg_types;
  int i;

  /* Substitute the explicit template arguments into the type of DECL.
     The call to fn_type_unification will handle substitution into the
     FN.  */
  decl_type = TREE_TYPE (decl);
  if (explicit_args && uses_template_parms (decl_type))
    {
      tree tmpl;
      tree converted_args;

      if (DECL_TEMPLATE_INFO (decl))
	tmpl = DECL_TI_TEMPLATE (decl);
      else
	/* We can get here for some illegal specializations.  */
	return NULL_TREE;

      converted_args
	= (coerce_template_parms (DECL_INNERMOST_TEMPLATE_PARMS (tmpl),
				  explicit_args, NULL_TREE,
				  /*complain=*/0, 
				  /*require_all_arguments=*/0));
      if (converted_args == error_mark_node)
	return NULL_TREE;
      
      decl_type = tsubst (decl_type, converted_args, /*complain=*/0, 
			  NULL_TREE); 
      if (decl_type == error_mark_node)
	return NULL_TREE;
    }

  /* If FN is a static member function, adjust the type of DECL
     appropriately.  */
  decl_arg_types = TYPE_ARG_TYPES (decl_type);
  if (DECL_STATIC_FUNCTION_P (fn) 
      && DECL_NONSTATIC_MEMBER_FUNCTION_P (decl))
    decl_arg_types = TREE_CHAIN (decl_arg_types);

  i = fn_type_unification (fn, explicit_args, targs, 
			   decl_arg_types,
			   (check_rettype || DECL_CONV_FN_P (fn)
	                    ? TREE_TYPE (decl_type) : NULL_TREE),
			   DEDUCE_EXACT);

  if (i != 0)
    return NULL_TREE;

  return targs;
}

/* For most uses, we want to check the return type.  */

tree 
get_bindings (fn, decl, explicit_args)
     tree fn, decl, explicit_args;
{
  return get_bindings_real (fn, decl, explicit_args, 1);
}

/* But for more_specialized, we only care about the parameter types.  */

static tree
get_bindings_overload (fn, decl, explicit_args)
     tree fn, decl, explicit_args;
{
  return get_bindings_real (fn, decl, explicit_args, 0);
}

/* Return the innermost template arguments that, when applied to a
   template specialization whose innermost template parameters are
   TPARMS, and whose specialization arguments are ARGS, yield the
   ARGS.  

   For example, suppose we have:

     template <class T, class U> struct S {};
     template <class T> struct S<T*, int> {};

   Then, suppose we want to get `S<double*, int>'.  The TPARMS will be
   {T}, the PARMS will be {T*, int} and the ARGS will be {double*,
   int}.  The resulting vector will be {double}, indicating that `T'
   is bound to `double'.  */

static tree
get_class_bindings (tparms, parms, args)
     tree tparms, parms, args;
{
  int i, ntparms = TREE_VEC_LENGTH (tparms);
  tree vec = make_tree_vec (ntparms);

  args = innermost_args (args);

  if (unify (tparms, vec, parms, args, UNIFY_ALLOW_NONE))
    return NULL_TREE;

  for (i =  0; i < ntparms; ++i)
    if (! TREE_VEC_ELT (vec, i))
      return NULL_TREE;

  return vec;
}

/* In INSTANTIATIONS is a list of <INSTANTIATION, TEMPLATE> pairs.
   Pick the most specialized template, and return the corresponding
   instantiation, or if there is no corresponding instantiation, the
   template itself.  EXPLICIT_ARGS is any template arguments explicity
   mentioned in a template-id.  If there is no most specialized
   tempalte, error_mark_node is returned.  If there are no templates
   at all, NULL_TREE is returned.  */

tree
most_specialized_instantiation (instantiations, explicit_args)
     tree instantiations;
     tree explicit_args;
{
  tree fn, champ;
  int fate;

  if (!instantiations)
    return NULL_TREE;

  champ = instantiations;
  for (fn = TREE_CHAIN (instantiations); fn; fn = TREE_CHAIN (fn))
    {
      fate = more_specialized (TREE_VALUE (champ), 
			       TREE_VALUE (fn), explicit_args);
      if (fate == 1)
	;
      else
	{
	  if (fate == 0)
	    {
	      fn = TREE_CHAIN (fn);
	      if (! fn)
		return error_mark_node;
	    }
	  champ = fn;
	}
    }

  for (fn = instantiations; fn && fn != champ; fn = TREE_CHAIN (fn))
    {
      fate = more_specialized (TREE_VALUE (champ), 
			       TREE_VALUE (fn), explicit_args);
      if (fate != 1)
	return error_mark_node;
    }

  return TREE_PURPOSE (champ) ? TREE_PURPOSE (champ) : TREE_VALUE (champ);
}

/* Return the most specialized of the list of templates in FNS that can
   produce an instantiation matching DECL, given the explicit template
   arguments EXPLICIT_ARGS.  */

static tree
most_specialized (fns, decl, explicit_args)
     tree fns, decl, explicit_args;
{
  tree candidates = NULL_TREE;
  tree fn, args;

  for (fn = fns; fn; fn = TREE_CHAIN (fn))
    {
      tree candidate = TREE_VALUE (fn);

      args = get_bindings (candidate, decl, explicit_args);
      if (args)
	candidates = tree_cons (NULL_TREE, candidate, candidates);
    }

  return most_specialized_instantiation (candidates, explicit_args);
}

/* If DECL is a specialization of some template, return the most
   general such template.  For example, given:

     template <class T> struct S { template <class U> void f(U); };

   if TMPL is `template <class U> void S<int>::f(U)' this will return
   the full template.  This function will not trace past partial
   specializations, however.  For example, given in addition:

     template <class T> struct S<T*> { template <class U> void f(U); };

   if TMPL is `template <class U> void S<int*>::f(U)' this will return
   `template <class T> template <class U> S<T*>::f(U)'.  */

tree
most_general_template (decl)
     tree decl;
{
  while (DECL_TEMPLATE_INFO (decl)
	 && !(TREE_CODE (decl) == TEMPLATE_DECL
	      && DECL_TEMPLATE_SPECIALIZATION (decl))
	 /* The DECL_TI_TEMPLATE can be a LOOKUP_EXPR or
	    IDENTIFIER_NODE in some cases.  (See cp-tree.h for
	    details.)  */
	 && TREE_CODE (DECL_TI_TEMPLATE (decl)) == TEMPLATE_DECL)
    decl = DECL_TI_TEMPLATE (decl);

  return decl;
}

/* Return the most specialized of the class template specializations
   of TMPL which can produce an instantiation matching ARGS, or
   error_mark_node if the choice is ambiguous.  */

static tree
most_specialized_class (tmpl, args)
     tree tmpl;
     tree args;
{
  tree list = NULL_TREE;
  tree t;
  tree champ;
  int fate;

  tmpl = most_general_template (tmpl);
  for (t = DECL_TEMPLATE_SPECIALIZATIONS (tmpl); t; t = TREE_CHAIN (t))
    {
      tree spec_args 
	= get_class_bindings (TREE_VALUE (t), TREE_PURPOSE (t), args);
      if (spec_args)
	{
	  list = decl_tree_cons (TREE_PURPOSE (t), TREE_VALUE (t), list);
	  TREE_TYPE (list) = TREE_TYPE (t);
	}
    }

  if (! list)
    return NULL_TREE;

  t = list;
  champ = t;
  t = TREE_CHAIN (t);
  for (; t; t = TREE_CHAIN (t))
    {
      fate = more_specialized_class (champ, t);
      if (fate == 1)
	;
      else
	{
	  if (fate == 0)
	    {
	      t = TREE_CHAIN (t);
	      if (! t)
		return error_mark_node;
	    }
	  champ = t;
	}
    }

  for (t = list; t && t != champ; t = TREE_CHAIN (t))
    {
      fate = more_specialized_class (champ, t);
      if (fate != 1)
	return error_mark_node;
    }

  return champ;
}

/* called from the parser.  */

void
do_decl_instantiation (declspecs, declarator, storage)
     tree declspecs, declarator, storage;
{
  tree decl = grokdeclarator (declarator, declspecs, NORMAL, 0, NULL_TREE);
  tree result = NULL_TREE;
  int extern_p = 0;

  if (!decl)
    /* An error ocurred, for which grokdeclarator has already issued
       an appropriate message.  */
    return;
  else if (! DECL_LANG_SPECIFIC (decl))
    {
      cp_error ("explicit instantiation of non-template `%#D'", decl);
      return;
    }
  else if (TREE_CODE (decl) == VAR_DECL)
    {
      /* There is an asymmetry here in the way VAR_DECLs and
	 FUNCTION_DECLs are handled by grokdeclarator.  In the case of
	 the latter, the DECL we get back will be marked as a
	 template instantiation, and the appropriate
	 DECL_TEMPLATE_INFO will be set up.  This does not happen for
	 VAR_DECLs so we do the lookup here.  Probably, grokdeclarator
	 should handle VAR_DECLs as it currently handles
	 FUNCTION_DECLs.  */
      result = lookup_field (DECL_CONTEXT (decl), DECL_NAME (decl), 0, 0);
      if (result && TREE_CODE (result) != VAR_DECL)
	{
	  cp_error ("no matching template for `%D' found", result);
	  return;
	}
    }
  else if (TREE_CODE (decl) != FUNCTION_DECL)
    {
      cp_error ("explicit instantiation of `%#D'", decl);
      return;
    }
  else
    result = decl;

  /* Check for various error cases.  Note that if the explicit
     instantiation is legal the RESULT will currently be marked as an
     *implicit* instantiation; DECL_EXPLICIT_INSTANTIATION is not set
     until we get here.  */

  if (DECL_TEMPLATE_SPECIALIZATION (result))
    {
      /* [temp.spec]

	 No program shall both explicitly instantiate and explicitly
	 specialize a template.  */
      cp_pedwarn ("explicit instantiation of `%#D' after", result);
      cp_pedwarn_at ("explicit specialization here", result);
      return;
    }
  else if (DECL_EXPLICIT_INSTANTIATION (result))
    {
      /* [temp.spec]

	 No program shall explicitly instantiate any template more
	 than once.  

	 We check DECL_INTERFACE_KNOWN so as not to complain when the first
	 instantiation was `extern' and the second is not, and EXTERN_P for
	 the opposite case.  If -frepo, chances are we already got marked
	 as an explicit instantion because of the repo file.  */
      if (DECL_INTERFACE_KNOWN (result) && !extern_p && !flag_use_repository)
	cp_pedwarn ("duplicate explicit instantiation of `%#D'", result);

      /* If we've already instantiated the template, just return now.  */
      if (DECL_INTERFACE_KNOWN (result))
	return;
    }
  else if (!DECL_IMPLICIT_INSTANTIATION (result))
    {
      cp_error ("no matching template for `%D' found", result);
      return;
    }
  else if (!DECL_TEMPLATE_INFO (result))
    {
      cp_pedwarn ("explicit instantiation of non-template `%#D'", result);
      return;
    }

  if (flag_external_templates)
    return;

  if (storage == NULL_TREE)
    ;
  else if (storage == ridpointers[(int) RID_EXTERN])
    {
      if (pedantic)
	cp_pedwarn ("ISO C++ forbids the use of `extern' on explicit instantiations");
      extern_p = 1;
    }
  else
    cp_error ("storage class `%D' applied to template instantiation",
	      storage);

  SET_DECL_EXPLICIT_INSTANTIATION (result);
  mark_decl_instantiated (result, extern_p);
  repo_template_instantiated (result, extern_p);
  if (! extern_p)
    instantiate_decl (result);
}

void
mark_class_instantiated (t, extern_p)
     tree t;
     int extern_p;
{
  SET_CLASSTYPE_EXPLICIT_INSTANTIATION (t);
  SET_CLASSTYPE_INTERFACE_KNOWN (t);
  CLASSTYPE_INTERFACE_ONLY (t) = extern_p;
  CLASSTYPE_VTABLE_NEEDS_WRITING (t) = ! extern_p;
  TYPE_DECL_SUPPRESS_DEBUG (TYPE_NAME (t)) = extern_p;
  if (! extern_p)
    {
      CLASSTYPE_DEBUG_REQUESTED (t) = 1;
      rest_of_type_compilation (t, 1);
    }
}     

void
do_type_instantiation (t, storage)
     tree t, storage;
{
  int extern_p = 0;
  int nomem_p = 0;
  int static_p = 0;

  if (TREE_CODE (t) == TYPE_DECL)
    t = TREE_TYPE (t);

  if (! CLASS_TYPE_P (t) || ! CLASSTYPE_TEMPLATE_INFO (t))
    {
      cp_error ("explicit instantiation of non-template type `%T'", t);
      return;
    }

  complete_type (t);

  /* With -fexternal-templates, explicit instantiations are treated the same
     as implicit ones.  */
  if (flag_external_templates)
    return;

  if (!COMPLETE_TYPE_P (t))
    {
      cp_error ("explicit instantiation of `%#T' before definition of template",
		t);
      return;
    }

  if (storage != NULL_TREE)
    {
      if (pedantic)
	cp_pedwarn("ISO C++ forbids the use of `%s' on explicit instantiations", 
		   IDENTIFIER_POINTER (storage));

      if (storage == ridpointers[(int) RID_INLINE])
	nomem_p = 1;
      else if (storage == ridpointers[(int) RID_EXTERN])
	extern_p = 1;
      else if (storage == ridpointers[(int) RID_STATIC])
	static_p = 1;
      else
	{
	  cp_error ("storage class `%D' applied to template instantiation",
		    storage);
	  extern_p = 0;
	}
    }

  if (CLASSTYPE_PARTIAL_SPECIALIZATION (t))
    {
      /* [temp.spec]

	 No program shall both explicitly instantiate and explicitly
	 specialize a template.  */
      cp_error ("explicit instantiation of `%#T' after", t);
      cp_error_at ("explicit specialization here", t);
      return;
    }
  else if (CLASSTYPE_EXPLICIT_INSTANTIATION (t))
    {
      /* [temp.spec]

	 No program shall explicitly instantiate any template more
	 than once.  

         If CLASSTYPE_INTERFACE_ONLY, then the first explicit instantiation
	 was `extern'.  If EXTERN_P then the second is.  If -frepo, chances
	 are we already got marked as an explicit instantion because of the
	 repo file.  All these cases are OK.  */
      if (!CLASSTYPE_INTERFACE_ONLY (t) && !extern_p && !flag_use_repository)
	cp_pedwarn ("duplicate explicit instantiation of `%#T'", t);
      
      /* If we've already instantiated the template, just return now.  */
      if (!CLASSTYPE_INTERFACE_ONLY (t))
	return;
    }

  mark_class_instantiated (t, extern_p);
  repo_template_instantiated (t, extern_p);

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
       is unclear on whether the instatiation of the members are
       *explicit* instantiations or not.  We choose to be generous,
       and not set DECL_EXPLICIT_INSTANTIATION.  Therefore, we allow
       the explicit instantiation of a class where some of the members
       have no definition in the current translation unit.  */

    if (! static_p)
      for (tmp = TYPE_METHODS (t); tmp; tmp = TREE_CHAIN (tmp))
	if (TREE_CODE (tmp) == FUNCTION_DECL
	    && DECL_TEMPLATE_INSTANTIATION (tmp))
	  {
	    mark_decl_instantiated (tmp, extern_p);
	    repo_template_instantiated (tmp, extern_p);
	    if (! extern_p)
	      instantiate_decl (tmp);
	  }

    for (tmp = TYPE_FIELDS (t); tmp; tmp = TREE_CHAIN (tmp))
      if (TREE_CODE (tmp) == VAR_DECL && DECL_TEMPLATE_INSTANTIATION (tmp))
	{
	  mark_decl_instantiated (tmp, extern_p);
	  repo_template_instantiated (tmp, extern_p);
	  if (! extern_p)
	    instantiate_decl (tmp);
	}

    for (tmp = CLASSTYPE_TAGS (t); tmp; tmp = TREE_CHAIN (tmp))
      if (IS_AGGR_TYPE (TREE_VALUE (tmp))
	  && !uses_template_parms (CLASSTYPE_TI_ARGS (TREE_VALUE (tmp))))
	do_type_instantiation (TYPE_MAIN_DECL (TREE_VALUE (tmp)), storage);
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
regenerate_decl_from_template (decl, tmpl)
     tree decl;
     tree tmpl;
{
  tree args;
  tree code_pattern;
  tree new_decl;
  tree gen_tmpl;
  int unregistered;

  args = DECL_TI_ARGS (decl);
  code_pattern = DECL_TEMPLATE_RESULT (tmpl);

  /* Unregister the specialization so that when we tsubst we will not
     just return DECL.  We don't have to unregister DECL from TMPL
     because if would only be registered there if it were a partial
     instantiation of a specialization, which it isn't: it's a full
     instantiation.  */
  gen_tmpl = most_general_template (tmpl);
  unregistered = unregister_specialization (decl, gen_tmpl);

  /* If the DECL was not unregistered then something peculiar is
     happening: we created a specialization but did not call
     register_specialization for it.  */
  my_friendly_assert (unregistered, 0);

  if (TREE_CODE (decl) == VAR_DECL)
    /* Make sure that we can see identifiers, and compute access
       correctly, for the class members used in the declaration of
       this static variable.  */
    pushclass (DECL_CONTEXT (decl), 2);

  /* Do the substitution to get the new declaration.  */
  new_decl = tsubst (code_pattern, args, /*complain=*/1, NULL_TREE);

  if (TREE_CODE (decl) == VAR_DECL)
    {
      /* Set up DECL_INITIAL, since tsubst doesn't.  */
      DECL_INITIAL (new_decl) = 
	tsubst_expr (DECL_INITIAL (code_pattern), args, 
		     /*complain=*/1, DECL_TI_TEMPLATE (decl));
      /* Pop the class context we pushed above.  */
      popclass ();
    }
  else if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      /* Convince duplicate_decls to use the DECL_ARGUMENTS from the
	 new decl.  */ 
      DECL_INITIAL (new_decl) = error_mark_node;
      /* And don't complain about a duplicate definition.  */
      DECL_INITIAL (decl) = NULL_TREE;
    }

  /* The immediate parent of the new template is still whatever it was
     before, even though tsubst sets DECL_TI_TEMPLATE up as the most
     general template.  We also reset the DECL_ASSEMBLER_NAME since
     tsubst always calculates the name as if the function in question
     were really a template instance, and sometimes, with friend
     functions, this is not so.  See tsubst_friend_function for
     details.  */
  DECL_TI_TEMPLATE (new_decl) = DECL_TI_TEMPLATE (decl);
  DECL_ASSEMBLER_NAME (new_decl) = DECL_ASSEMBLER_NAME (decl);
  DECL_RTL (new_decl) = DECL_RTL (decl);

  /* Call duplicate decls to merge the old and new declarations.  */
  duplicate_decls (new_decl, decl);

  /* Now, re-register the specialization.  */
  register_specialization (decl, gen_tmpl, args);
}

/* Produce the definition of D, a _DECL generated from a template.  */

tree
instantiate_decl (d)
     tree d;
{
  tree tmpl = DECL_TI_TEMPLATE (d);
  tree args = DECL_TI_ARGS (d);
  tree td;
  tree code_pattern;
  tree spec;
  tree gen_tmpl;
  int nested = in_function_p ();
  int pattern_defined;
  int line = lineno;
  char *file = input_filename;
  tree old_fn = current_function_decl;

  /* This function should only be used to instantiate templates for
     functions and static member variables.  */
  my_friendly_assert (TREE_CODE (d) == FUNCTION_DECL
		      || TREE_CODE (d) == VAR_DECL, 0);

  if (DECL_TEMPLATE_INSTANTIATED (d))
    /* D has already been instantiated.  It might seem reasonable to
       check whether or not D is an explict instantiation, and, if so,
       stop here.  But when an explicit instantiation is deferred
       until the end of the compilation, DECL_EXPLICIT_INSTANTIATION
       is set, even though we still need to do the instantiation.  */
    return d;

  /* If we already have a specialization of this declaration, then
     there's no reason to instantiate it.  Note that
     retrieve_specialization gives us both instantiations and
     specializations, so we must explicitly check
     DECL_TEMPLATE_SPECIALIZATION.  */
  gen_tmpl = most_general_template (tmpl);
  spec = retrieve_specialization (gen_tmpl, args);
  if (spec != NULL_TREE && DECL_TEMPLATE_SPECIALIZATION (spec))
    return spec;

  /* This needs to happen before any tsubsting.  */
  if (! push_tinst_level (d))
    return d;

  /* Set TD to the template whose DECL_TEMPLATE_RESULT is the pattern
     for the instantiation.  This is not always the most general
     template.  Consider, for example:

        template <class T>
	struct S { template <class U> void f();
	           template <> void f<int>(); };

     and an instantiation of S<double>::f<int>.  We want TD to be the
     specialization S<T>::f<int>, not the more general S<T>::f<U>.  */
  td = tmpl;
  for (td = tmpl;
       /* An instantiation cannot have a definition, so we need a
	  more general template.  */
       DECL_TEMPLATE_INSTANTIATION (td)
	 /* We must also deal with friend templates.  Given:

	      template <class T> struct S { 
		template <class U> friend void f() {};
	      };
	 
	    S<int>::f<U> say, is not an instantiation of S<T>::f<U>,
	    so far as the language is concerned, but that's still
	    where we get the pattern for the instantiation from.  On
	    ther hand, if the definition comes outside the class, say:

 	      template <class T> struct S { 
	        template <class U> friend void f();
              };
	      template <class U> friend void f() {}

	    we don't need to look any further.  That's what the check for
	    DECL_INITIAL is for.  */
	|| (TREE_CODE (d) == FUNCTION_DECL
	    && DECL_FRIEND_PSEUDO_TEMPLATE_INSTANTIATION (td)
	    && !DECL_INITIAL (DECL_TEMPLATE_RESULT (td)));
       )
    {
      /* The present template, TD, should not be a definition.  If it
	 were a definition, we should be using it!  Note that we
	 cannot restructure the loop to just keep going until we find
	 a template with a definition, since that might go too far if
	 a specialization was declared, but not defined.  */
      my_friendly_assert (!(TREE_CODE (d) == VAR_DECL
			    && !DECL_IN_AGGR_P (DECL_TEMPLATE_RESULT (td))), 
			  0); 
      
      /* Fetch the more general template.  */
      td = DECL_TI_TEMPLATE (td);
    }

  code_pattern = DECL_TEMPLATE_RESULT (td);

  if (TREE_CODE (d) == FUNCTION_DECL)
    pattern_defined = (DECL_SAVED_TREE (code_pattern) != NULL_TREE);
  else
    pattern_defined = ! DECL_IN_AGGR_P (code_pattern);

  push_to_top_level ();
  lineno = DECL_SOURCE_LINE (d);
  input_filename = DECL_SOURCE_FILE (d);

  if (pattern_defined)
    {
      repo_template_used (d);

      if (flag_external_templates && ! DECL_INTERFACE_KNOWN (d))
	{
	  if (flag_alt_external_templates)
	    {
	      if (interface_unknown)
		warn_if_unknown_interface (d);
	    }
	  else if (DECL_INTERFACE_KNOWN (code_pattern))
	    {
	      DECL_INTERFACE_KNOWN (d) = 1;
	      DECL_NOT_REALLY_EXTERN (d) = ! DECL_EXTERNAL (code_pattern);
	    }
	  else
	    warn_if_unknown_interface (code_pattern);
	}

      if (at_eof)
	import_export_decl (d);
    }

  /* Reject all external templates except inline functions.  */
  if (DECL_INTERFACE_KNOWN (d)
      && ! DECL_NOT_REALLY_EXTERN (d)
      && ! (TREE_CODE (d) == FUNCTION_DECL && DECL_INLINE (d)))
    goto out;

  if (TREE_CODE (d) == VAR_DECL 
      && TREE_READONLY (d)
      && DECL_INITIAL (d) == NULL_TREE
      && DECL_INITIAL (code_pattern) != NULL_TREE)
    /* We need to set up DECL_INITIAL regardless of pattern_defined if
       the variable is a static const initialized in the class body.  */;
  else if (pattern_defined && nested
	   && TREE_CODE (d) == FUNCTION_DECL && DECL_INLINE (d))
    /* An inline function used in another function; instantiate it now so
       we can inline it.  */;
  else if (! pattern_defined || ! at_eof)
    {
      /* Defer all other templates.  We restore the source position
         here because it's used by add_pending_template.  */
      lineno = line;
      input_filename = file;

      if (at_eof && !pattern_defined 
	  && DECL_EXPLICIT_INSTANTIATION (d))
	/* [temp.explicit]

	   The definition of a non-exported function template, a
	   non-exported member function template, or a non-exported
	   member function or static data member of a class template
	   shall be present in every translation unit in which it is
	   explicitly instantiated.  */
	cp_error ("explicit instantiation of `%D' but no definition available",
		  d);

      add_pending_template (d);
      goto out;
    }

  /* If this instantiation is COMDAT, we don't know whether or not we
     will really need to write it out.  If we can't be sure, mark it
     DECL_DEFER_OUTPUT.  NOTE: This increases memory consumption,
     since we keep some instantiations in memory rather than write
     them out immediately and forget them.  A better approach would be
     to wait until we know we need them to do the instantiation, but
     that would break templates with static locals, because we
     generate the functions to destroy statics before we determine
     which functions are needed.  A better solution would be to
     generate the ctor and dtor functions as we go.  */

  if (TREE_CODE (d) == FUNCTION_DECL
      && DECL_COMDAT (d)
      && ! DECL_NEEDED_P (d)
      /* If the function that caused us to be instantiated is needed, we
	 will be needed, too.  */
      && (! nested || (old_fn && ! DECL_NEEDED_P (old_fn))))
    DECL_DEFER_OUTPUT (d) = 1;

  /* We're now committed to instantiating this template.  Mark it as
     instantiated so that recursive calls to instantiate_decl do not
     try to instantiate it again.  */
  DECL_TEMPLATE_INSTANTIATED (d) = 1;

  /* Regenerate the declaration in case the template has been modified
     by a subsequent redeclaration.  */
  regenerate_decl_from_template (d, td);

  /* We already set the file and line above.  Reset them now in case
     they changed as a result of calling regenerate_decl_from_template.  */
  lineno = DECL_SOURCE_LINE (d);
  input_filename = DECL_SOURCE_FILE (d);

  if (TREE_CODE (d) == VAR_DECL)
    {
      DECL_IN_AGGR_P (d) = 0;
      if (DECL_INTERFACE_KNOWN (d))
	DECL_EXTERNAL (d) = ! DECL_NOT_REALLY_EXTERN (d);
      else
	{
	  DECL_EXTERNAL (d) = 1;
	  DECL_NOT_REALLY_EXTERN (d) = 1;
	}
      cp_finish_decl (d, DECL_INITIAL (d), NULL_TREE, 0);
    }
  else if (TREE_CODE (d) == FUNCTION_DECL)
    {
      /* Set up context.  */
      start_function (NULL_TREE, d, NULL_TREE, SF_PRE_PARSED);
      store_parm_decls ();

      /* We already set up __FUNCTION__, etc., so we don't want to do
	 it again now.  */
      current_function_name_declared = 1;

      /* Substitute into the body of the function.  */
      tsubst_expr (DECL_SAVED_TREE (code_pattern), args,
		   /*complain=*/1, tmpl);

      /* Finish the function.  */
      expand_body (finish_function (lineno, 0));
    }

  /* We're not deferring instantiation any more.  */
  TI_PENDING_TEMPLATE_FLAG (DECL_TEMPLATE_INFO (d)) = 0;

out:
  lineno = line;
  input_filename = file;

  pop_from_top_level ();
  pop_tinst_level ();

  return d;
}

/* Run through the list of templates that we wish we could
   instantiate, and instantiate any we can.  */

int
instantiate_pending_templates ()
{
  tree *t;
  int instantiated_something = 0;
  int reconsider;
  
  do 
    {
      reconsider = 0;

      t = &pending_templates;
      while (*t)
	{
	  tree srcloc = TREE_PURPOSE (*t);
	  tree instantiation = TREE_VALUE (*t);

	  input_filename = SRCLOC_FILE (srcloc);
	  lineno = SRCLOC_LINE (srcloc);

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
			instantiate_decl (fn);
		  if (COMPLETE_TYPE_P (instantiation))
		    {
		      instantiated_something = 1;
		      reconsider = 1;
		    }
		}

	      if (COMPLETE_TYPE_P (instantiation))
		/* If INSTANTIATION has been instantiated, then we don't
		   need to consider it again in the future.  */
		*t = TREE_CHAIN (*t);
	      else 
		t = &TREE_CHAIN (*t);
	    }
	  else
	    {
	      if (DECL_TEMPLATE_INSTANTIATION (instantiation)
		  && !DECL_TEMPLATE_INSTANTIATED (instantiation))
		{
		  instantiation = instantiate_decl (instantiation);
		  if (DECL_TEMPLATE_INSTANTIATED (instantiation))
		    {
		      instantiated_something = 1;
		      reconsider = 1;
		    }
		}

	      if (!DECL_TEMPLATE_INSTANTIATION (instantiation)
		  || DECL_TEMPLATE_INSTANTIATED (instantiation))
		/* If INSTANTIATION has been instantiated, then we don't
		   need to consider it again in the future.  */
		*t = TREE_CHAIN (*t);
	      else 
		t = &TREE_CHAIN (*t);
	    }
	}
      template_tail = t;

      /* Go through the things that are template instantiations if we are
	 using guiding declarations.  */
      t = &maybe_templates;
      while (*t)
	{
	  tree template;
	  tree fn;
	  tree args;

	  fn = TREE_VALUE (*t);

	  if (DECL_INITIAL (fn))
	    /* If the FN is already defined, then it was either already
	       instantiated or, even though guiding declarations were
	       allowed, a non-template definition was provided.  */
	    ;
	  else
	    {
	      template = TREE_PURPOSE (*t);
	      args = get_bindings (template, fn, NULL_TREE);
	      fn = instantiate_template (template, args);
	      instantiate_decl (fn);
	      reconsider = 1;
	    }
	
	  /* Remove this entry from the chain.  */
	  *t = TREE_CHAIN (*t);
	}
      maybe_template_tail = t;
    } 
  while (reconsider);

  return instantiated_something;
}

/* Substitute ARGVEC into T, which is a TREE_LIST.  In particular, it
   is an initializer list: the TREE_PURPOSEs are DECLs, and the
   TREE_VALUEs are initializer values.  Used by instantiate_decl.  */

static tree
tsubst_expr_values (t, argvec)
     tree t, argvec;
{
  tree first = NULL_TREE;
  tree *p = &first;

  for (; t; t = TREE_CHAIN (t))
    {
      tree pur = tsubst_copy (TREE_PURPOSE (t), argvec,
			      /*complain=*/1, NULL_TREE);
      tree val = tsubst_expr (TREE_VALUE (t), argvec, /*complain=*/1, 
			      NULL_TREE);
      *p = build_tree_list (pur, val);
      p = &TREE_CHAIN (*p);
    }
  return first;
}

/* D is an undefined function declaration in the presence of templates with
   the same name, listed in FNS.  If one of them can produce D as an
   instantiation, remember this so we can instantiate it at EOF if D has
   not been defined by that time.  */

void
add_maybe_template (d, fns)
     tree d, fns;
{
  tree t;

  if (DECL_MAYBE_TEMPLATE (d))
    return;

  t = most_specialized (fns, d, NULL_TREE);
  if (! t)
    return;
  if (t == error_mark_node)
    {
      cp_error ("ambiguous template instantiation for `%D'", d);
      return;
    }

  *maybe_template_tail = tree_cons (t, d, NULL_TREE);
  maybe_template_tail = &TREE_CHAIN (*maybe_template_tail);
  DECL_MAYBE_TEMPLATE (d) = 1;
}

/* Set CURRENT_ACCESS_SPECIFIER based on the protection of DECL.  */

static void
set_current_access_from_decl (decl)
     tree decl;
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
tsubst_enum (tag, newtag, args)
     tree tag;
     tree newtag;
     tree args;
{
  tree e;

  for (e = TYPE_VALUES (tag); e; e = TREE_CHAIN (e))
    {
      tree value;
      
      /* Note that in a template enum, the TREE_VALUE is the
	 CONST_DECL, not the corresponding INTEGER_CST.  */
      value = tsubst_expr (DECL_INITIAL (TREE_VALUE (e)), 
			   args, /*complain=*/1,
			   NULL_TREE);

      /* Give this enumeration constant the correct access.  */
      set_current_access_from_decl (TREE_VALUE (e));

      /* Actually build the enumerator itself.  */
      build_enumerator (TREE_PURPOSE (e), value, newtag); 
    }

  finish_enum (newtag);
}

/* Set the DECL_ASSEMBLER_NAME for DECL, which is a FUNCTION_DECL that
   is either an instantiation or specialization of a template
   function.  */

static void
set_mangled_name_for_template_decl (decl)
     tree decl;
{
  tree saved_namespace;
  tree context = NULL_TREE;
  tree fn_type;
  tree ret_type;
  tree parm_types;
  tree tparms;
  tree targs;
  tree tmpl;
  int parm_depth;

  my_friendly_assert (TREE_CODE (decl) == FUNCTION_DECL, 0);
  my_friendly_assert (DECL_TEMPLATE_INFO (decl) != NULL_TREE, 0);

  /* The names of template functions must be mangled so as to indicate
     what template is being specialized with what template arguments.
     For example, each of the following three functions must get
     different mangled names:

       void f(int);                  
       template <> void f<7>(int);
       template <> void f<8>(int);  */

  targs = DECL_TI_ARGS (decl);
  if (uses_template_parms (targs))
    /* This DECL is for a partial instantiation.  There's no need to
       mangle the name of such an entity.  */
    return;

  tmpl = most_general_template (DECL_TI_TEMPLATE (decl));
  tparms = DECL_TEMPLATE_PARMS (tmpl);
  parm_depth = TMPL_PARMS_DEPTH (tparms);

  /* There should be as many levels of arguments as there are levels
     of parameters.  */
  my_friendly_assert (parm_depth == TMPL_ARGS_DEPTH (targs), 0);

  /* We now compute the PARMS and RET_TYPE to give to
     build_decl_overload_real.  The PARMS and RET_TYPE are the
     parameter and return types of the template, after all but the
     innermost template arguments have been substituted, not the
     parameter and return types of the function DECL.  For example,
     given:

       template <class T> T f(T);

     both PARMS and RET_TYPE should be `T' even if DECL is `int f(int)'.  
     A more subtle example is:

       template <class T> struct S { template <class U> void f(T, U); }

     Here, if DECL is `void S<int>::f(int, double)', PARMS should be
     {int, U}.  Thus, the args that we want to subsitute into the
     return and parameter type for the function are those in TARGS,
     with the innermost level omitted.  */
  fn_type = TREE_TYPE (tmpl);
  if (DECL_STATIC_FUNCTION_P (decl))
    context = DECL_CONTEXT (decl);

  if (parm_depth == 1)
    /* No substitution is necessary.  */
    ;
  else
    {
      int i;
      tree partial_args;

      /* Replace the innermost level of the TARGS with NULL_TREEs to
	 let tsubst know not to subsitute for those parameters.  */
      partial_args = make_tree_vec (TREE_VEC_LENGTH (targs));
      for (i = 1; i < TMPL_ARGS_DEPTH (targs); ++i)
	SET_TMPL_ARGS_LEVEL (partial_args, i,
			     TMPL_ARGS_LEVEL (targs, i));
      SET_TMPL_ARGS_LEVEL (partial_args,
			   TMPL_ARGS_DEPTH (targs),
			   make_tree_vec (DECL_NTPARMS (tmpl)));

      /* Now, do the (partial) substitution to figure out the
	 appropriate function type.  */
      fn_type = tsubst (fn_type, partial_args, /*complain=*/1, NULL_TREE);
      if (DECL_STATIC_FUNCTION_P (decl))
	context = tsubst (context, partial_args, /*complain=*/1, NULL_TREE);

      /* Substitute into the template parameters to obtain the real
	 innermost set of parameters.  This step is important if the
	 innermost set of template parameters contains value
	 parameters whose types depend on outer template parameters.  */
      TREE_VEC_LENGTH (partial_args)--;
      tparms = tsubst_template_parms (tparms, partial_args, /*complain=*/1);
    }

  /* Now, get the innermost parameters and arguments, and figure out
     the parameter and return types.  */
  tparms = INNERMOST_TEMPLATE_PARMS (tparms);
  targs = innermost_args (targs);
  ret_type = TREE_TYPE (fn_type);
  parm_types = TYPE_ARG_TYPES (fn_type);

  /* For a static member function, we generate a fake `this' pointer,
     for the purposes of mangling.  This indicates of which class the
     function is a member.  Because of:

       [class.static] 

       There shall not be a static and a nonstatic member function
       with the same name and the same parameter types

     we don't have to worry that this will result in a clash with a
     non-static member function.  */
  if (DECL_STATIC_FUNCTION_P (decl))
    parm_types = hash_tree_chain (build_pointer_type (context), parm_types);

  /* There should be the same number of template parameters as
     template arguments.  */
  my_friendly_assert (TREE_VEC_LENGTH (tparms) == TREE_VEC_LENGTH (targs),
		      0);

  /* If the template is in a namespace, we need to put that into the
     mangled name. Unfortunately, build_decl_overload_real does not
     get the decl to mangle, so it relies on the current
     namespace. Therefore, we set that here temporarily. */
  my_friendly_assert (DECL_P (decl), 980702);
  saved_namespace = current_namespace;
  current_namespace = CP_DECL_CONTEXT (decl);  

  /* Actually set the DCL_ASSEMBLER_NAME.  */
  DECL_ASSEMBLER_NAME (decl)
    = build_decl_overload_real (DECL_NAME (decl), parm_types, ret_type,
				tparms, targs, 
				DECL_FUNCTION_MEMBER_P (decl) 
				+ DECL_CONSTRUCTOR_P (decl));

  /* Restore the previously active namespace.  */
  current_namespace = saved_namespace;
}
