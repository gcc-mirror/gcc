/* Handle parameterized types (templates) for GNU C++.
   Copyright (C) 1992, 93, 94, 95, 96, 1997 Free Software Foundation, Inc.
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
#include <stdio.h>
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

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

extern struct obstack permanent_obstack;

extern int lineno;
extern char *input_filename;
struct pending_inline *pending_template_expansions;

tree current_template_parms;
HOST_WIDE_INT processing_template_decl;

tree pending_templates;
static tree *template_tail = &pending_templates;

tree maybe_templates;
static tree *maybe_template_tail = &maybe_templates;

int minimal_parse_mode;

int processing_specialization;
static int template_header_count;

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

static int unify PROTO((tree, tree *, int, tree, tree, int *, int));
static void add_pending_template PROTO((tree));
static int push_tinst_level PROTO((tree));
static tree classtype_mangled_name PROTO((tree));
static char *mangle_class_name_for_template PROTO((char *, tree, tree));
static tree tsubst_expr_values PROTO((tree, tree));
static int comp_template_args PROTO((tree, tree));
static int list_eq PROTO((tree, tree));
static tree get_class_bindings PROTO((tree, tree, tree));
static tree coerce_template_parms PROTO((tree, tree, tree));
static tree tsubst_enum	PROTO((tree, tree, int, tree *));
static tree add_to_template_args PROTO((tree, tree));
static int  type_unification_real PROTO((tree, tree *, tree, tree, int*,
					 int, int, int));
static int processing_explicit_specialization PROTO((int));
static void note_template_header PROTO((int));

/* Restore the template parameter context. */

void 
begin_member_template_processing (decl)
     tree decl;
{
  tree parms;
  int i;

  parms = DECL_INNERMOST_TEMPLATE_PARMS (DECL_TI_TEMPLATE (decl));

  ++processing_template_decl;
  current_template_parms 
    = tree_cons (build_int_2 (0, processing_template_decl),
		 parms, current_template_parms);
  pushlevel (0);
  for (i = 0; i < TREE_VEC_LENGTH (parms); ++i) 
    {
      tree parm = TREE_VALUE (TREE_VEC_ELT (parms, i));
      my_friendly_assert (TREE_CODE_CLASS (TREE_CODE (parm)) == 'd', 0);
      
      switch (TREE_CODE (parm))
	{
	case TYPE_DECL:
	  pushdecl (parm);
	  break;

	case PARM_DECL:
	  {
	    /* Make a CONST_DECL as is done in process_template_parm. */
	    tree decl = build_decl (CONST_DECL, DECL_NAME (parm),
				    TREE_TYPE (parm));
	    DECL_INITIAL (decl) = DECL_INITIAL (parm);
	    pushdecl (decl);
	  }
	break;

	default:
	  my_friendly_abort (0);
	}
    }
}

/* Undo the effects of begin_member_template_processing. */

void 
end_member_template_processing ()
{
  if (! processing_template_decl)
    return;

  --processing_template_decl;
  current_template_parms = TREE_CHAIN (current_template_parms);
  poplevel (0, 0, 0);
}

/* Returns non-zero iff T is a member template function.  Works if T
   is either a FUNCTION_DECL or a TEMPLATE_DECL.  */

int
is_member_template (t)
     tree t;
{
  int r = 0;

  if (TREE_CODE (t) != FUNCTION_DECL
      && !DECL_FUNCTION_TEMPLATE_P (t))
    /* Anything that isn't a template or a template function is
       certainly not a member template.  */
    return 0;

  if ((DECL_FUNCTION_MEMBER_P (t) 
       && !DECL_TEMPLATE_SPECIALIZATION (t))
      || (TREE_CODE (t) == TEMPLATE_DECL && 
	  DECL_FUNCTION_MEMBER_P (DECL_TEMPLATE_RESULT (t))))
    {
      tree tmpl = NULL_TREE;

      if (DECL_FUNCTION_TEMPLATE_P (t))
	tmpl = t;
      else if (DECL_TEMPLATE_INFO (t) 
	       && DECL_FUNCTION_TEMPLATE_P (DECL_TI_TEMPLATE (t)))
	tmpl = DECL_TI_TEMPLATE (t);

      if (tmpl) 
	{
	  tree parms = DECL_TEMPLATE_PARMS (tmpl);
	  int parm_levels = list_length (parms);
	  int template_class_levels = 0;
	  tree ctx = DECL_CLASS_CONTEXT (t);

	  if (CLASSTYPE_TEMPLATE_INFO (ctx))
	    {
	      tree args;

	      /* Here, we should really count the number of levels
		 deep ctx is, making sure not to count any levels that
		 are just specializations.  Since there are no member
		 template classes yet, we don't have to do all that.  */

	      if (!CLASSTYPE_TEMPLATE_SPECIALIZATION (ctx))
		template_class_levels = 1;
	      else
		{
		  int i;

		  args = CLASSTYPE_TI_ARGS (ctx);

		  if (args == NULL_TREE)
		    template_class_levels = 1;
		  else 
		    for (i = 0; i < TREE_VEC_LENGTH (args); ++i)
		      if (uses_template_parms (TREE_VEC_ELT (args, i)))
			{
			  template_class_levels++;
			  break;
			}
		}
	    }

	  if (parm_levels > template_class_levels)
	    r = 1;
	}
    }

  return r;
}

/* Return a new template argument vector which contains all of ARGS,
   but has as its innermost set of arguments the EXTRA_ARGS.  */

static tree
add_to_template_args (args, extra_args)
     tree args;
     tree extra_args;
{
  tree new_args;

  if (TREE_CODE (TREE_VEC_ELT (args, 0)) != TREE_VEC)
    {
      new_args = make_tree_vec (2);
      TREE_VEC_ELT (new_args, 0) = args;
    }
  else 
    {
      int i;

      new_args = make_tree_vec (TREE_VEC_LENGTH (args) - 1);

      for (i = 0; i < TREE_VEC_LENGTH (args); ++i)
	TREE_VEC_ELT (new_args, i) = TREE_VEC_ELT (args, i);
    }
	  
  TREE_VEC_ELT (new_args, 
		TREE_VEC_LENGTH (new_args) - 1) = extra_args;

  return new_args;
}

/* We've got a template header coming up; push to a new level for storing
   the parms.  */

void
begin_template_parm_list ()
{
  pushlevel (0);
  declare_pseudo_global_level ();
  ++processing_template_decl;
  note_template_header (0);
}


/* We've just seen template <>. */

void
begin_specialization ()
{
  note_template_header (1);
}


/* Called at then end of processing a declaration preceded by
   template<>.  */

void 
end_specialization ()
{
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


/* Returns non-zero iff a declarator, in which the number of template
   types that appeared was TEMPLATE_COUNT, is an explicit
   specialization.  */

static int
processing_explicit_specialization (template_count)
     int template_count;
{
  /* A function declaration is an explicit specialization of a member
     template if all of the following conditions hold:
     
     o There was a template <...> preceding the declaration.
     o The last template <...> was in fact template <>.
     o The number of template <...>'s preceding the declaration, less
       the number of template classes with arguments specified used to
       qualify the function name, is 1.

     For example:

     template <> void S<int>::foo(); 
     template <class T> template <> void S<T>::foo();
     template <> struct S<int> { ... template <> void foo(); }

     The first of these is not a specialization of S<int>::foo() (it
     is instead a specialization of S<T>::foo), while the next two are
     specializations of member template functions.  */

  return processing_specialization 
    && template_header_count > template_count; 
}

/* Returns the template function specialized by TEMPLATE_ID, or
   NULL_TREE if there is none.

   The TEMPLATE_ID is a TEMPLATE_ID_EXPR.  The TYPE is
   the type it has been declared to have.  Return the TEMPLATE_DECL
   that is being specialized, and put the specialization arguments in
   *TARGS.  If no appropriate specialization can be found, NULL_TREE is
   returned, and *TARGS is assigned NULL_TREE.  If complain is
   non-zero, error messages are printed where appropriate.  */
   
tree
determine_explicit_specialization (template_id, type, targs_out,
				   need_member_template,
				   complain)
     tree template_id;
     tree type;
     tree* targs_out;
     int need_member_template;
     int complain;
{
  int i;
  int overloaded;
  tree fns;
  tree matching_fns = NULL_TREE;
  tree name = NULL_TREE;
  tree result;
  tree fn;

  my_friendly_assert (TREE_CODE (template_id) == TEMPLATE_ID_EXPR,
		      0); 
		      
  fns = TREE_OPERAND (template_id, 0);

  overloaded = fns != NULL_TREE && really_overloaded_fn (fns);

  for (fn = (fns != NULL_TREE) ? get_first_fn (fns) : NULL_TREE; 
       fn != NULL_TREE; 
       fn = overloaded ? DECL_CHAIN (fn) : NULL_TREE)
    {
      int dummy = 0;
      tree targs;

      if (name == NULL_TREE)
	name = DECL_NAME (fn);

      if (TREE_CODE (fn) != TEMPLATE_DECL
	  || (need_member_template && !is_member_template (fn)))
	continue;

      if (list_length (TREE_OPERAND (template_id, 1)) > DECL_NTPARMS (fn))
	continue;

      targs = make_scratch_vec (DECL_NTPARMS (fn));

      /* We allow incomplete unification here, because we are going to
	 check all the functions. */
      i = type_unification (DECL_INNERMOST_TEMPLATE_PARMS (fn),
			    &TREE_VEC_ELT (targs, 0),
			    type 
			    ? TYPE_ARG_TYPES (TREE_TYPE (fn)) : NULL_TREE, 
			    type ? TYPE_ARG_TYPES (type) : NULL_TREE,
			    TREE_OPERAND (template_id, 1),
			    &dummy, 1, 1);
      
      if (i == 0) 
	{
	  /* Unification was successful.  See if the return types
	     match. */
	  if (type != NULL_TREE)
	    {
	      tree tmpl_return_type = tsubst (TREE_TYPE (TREE_TYPE (fn)),
					      targs,
					      DECL_NTPARMS (fn),
					      NULL_TREE);
	      
	      if (tmpl_return_type != TREE_TYPE (type))
		{
		  /* Always complain about this.  With ambiguity, some
		     other context, might resolve things.  But, a
		     non-matching return type will always be a
		     problem.  */
		  cp_error ("Return type of explicit specialization of");
		  cp_error ("`%D' is `%T', but should be `%T'.", 
			    fn, TREE_TYPE (type), tmpl_return_type);
		  *targs_out = NULL_TREE;
		  return NULL_TREE;
		}
	    }

	  matching_fns = scratch_tree_cons (fn, targs, matching_fns);
	}
    }

  if (matching_fns == NULL_TREE)
    {
      if (complain)
	cp_error ("Specialization of `%s' does not match any template declaration.",
		  IDENTIFIER_POINTER (name));
      *targs_out = NULL_TREE;
      return NULL_TREE;
    }

  if (TREE_CHAIN (matching_fns) != NULL_TREE) 
    {
      if (complain)
	{
	  tree fn;
	  
	  cp_error ("Ambiguous explicit specialization.  Candidates are:");
	  for (fn = matching_fns; fn != NULL_TREE; fn = TREE_CHAIN (fn))
	    cp_error ("    %D", TREE_PURPOSE (fn));
	}

      *targs_out = NULL_TREE;
      return NULL_TREE;
    }

  /* We have one, and exactly one, match. */
  *targs_out = TREE_VALUE (matching_fns);
  return TREE_PURPOSE (matching_fns);
}

	
/* Check to see if the function just declared, as indicated in
   DECLARATOR, and in DECL, is a specialization.  Check that the
   specialization is OK.  If FLAGS == 1, we are being called by
   finish_struct_methods.  If FLAGS == 2, we are being called by
   grokfndecl, and the function has a definition, or is a friend.  If
   FLAGS == 3, this is a friend declaration.
   Returns 0 if the decl is not an explicit specialization or
   instantiation, 1 if it is an explicit specialization, and 2 if it
   is an explicit instantiation.  */

int
check_explicit_specialization (declarator, decl, template_count, flags)
     tree declarator;
     tree decl;
     int template_count;
     int flags;
{
  int finish_member = flags == 1;
  int have_def = flags == 2;
  int is_friend = flags == 3;

  if (processing_explicit_specialization (template_count)
      || finish_member
      || TREE_CODE (declarator) == TEMPLATE_ID_EXPR)
    {
      tree tmpl = NULL_TREE;
      tree dname = DECL_NAME (decl);
      tree ctype = DECL_CLASS_CONTEXT (decl);
      tree targs;

      /* We've come across a declarator that looks like: U f<T1,
	 T2, ...>(A1, A2, ..).  This is an explicit template
	 specialization.  Check that: 
	       
	 o The explicitly specified parameters together with those
	 that can be deduced by template argument deduction
	 uniquely determine a particular specialization.  
	       
	 See [temp.expl.spec].  */

      if (!finish_member
	  && TREE_CODE (declarator) == TEMPLATE_ID_EXPR
	  && !processing_explicit_specialization (template_count)
	  && !is_friend)
	{
	  if (!have_def)
	    /* This is not an explicit specialization.  It must be
	       an explicit instantiation.  */
	    return 2;
	  else if (pedantic)
	    pedwarn ("Explicit specialization not preceded by `template <>'");
	}

      if (TREE_CODE (declarator) != TEMPLATE_ID_EXPR)
	{
	  tree fns;

	  my_friendly_assert (TREE_CODE (declarator) == IDENTIFIER_NODE, 
			      0);
	  if (!ctype)
	    fns = IDENTIFIER_GLOBAL_VALUE (dname);
	  else
	    fns = dname;

	  declarator = lookup_template_function (fns, NULL_TREE);
	}

      if (TREE_CODE (TREE_OPERAND (declarator, 0)) == LOOKUP_EXPR) 
	{
	  /* A friend declaration.  We can't do much, because we don't
	   know what this resolves to, yet.  */
	  my_friendly_assert (is_friend != 0, 0);
	  SET_DECL_IMPLICIT_INSTANTIATION (decl);
	  return 1;
	} 

      if (ctype 
	  && TREE_CODE (TREE_OPERAND (declarator, 0)) == IDENTIFIER_NODE)
	{
	  tree fns;
 
	  if (TYPE_BEING_DEFINED (ctype) && !finish_member)
	    {
	      /* Since finish_struct_1 has not been called yet, we
		 can't call lookup_fnfields.  We note that this
		 template is a specialization, and proceed, letting
		 finish_struct_methods fix this up later.  */
	      SET_DECL_TEMPLATE_SPECIALIZATION (decl);
	      DECL_TEMPLATE_INFO (decl) 
		= perm_tree_cons (NULL_TREE, 
				  TREE_OPERAND (declarator, 1),
				  NULL_TREE);
	      return 1;
	    }

	  fns = lookup_fnfields (TYPE_BINFO (ctype), 
				 TREE_OPERAND (declarator, 0),
				 1);
	  
	  if (fns == NULL_TREE) 
	    {
	      cp_error ("No member template `%s' declared in `%T'",
			IDENTIFIER_POINTER (TREE_OPERAND (declarator,
							  0)),
			ctype);
	      return 1;
	    }
	  else
	    TREE_OPERAND (declarator, 0) = fns;
	}

      tmpl = 
	determine_explicit_specialization 
	(declarator, TREE_TYPE (decl), &targs, 
	 TREE_CODE (decl) == TEMPLATE_DECL, 1);
	    
      if (tmpl)
	{
	  /* Mangle the function name appropriately.  */
	  if (name_mangling_version >= 1)
	    {
	      tree arg_types = TYPE_ARG_TYPES (TREE_TYPE (tmpl));

	      if (ctype 
		  && TREE_CODE (TREE_TYPE (tmpl)) == FUNCTION_TYPE)
		arg_types = 
		  hash_tree_chain (build_pointer_type (ctype),
				   arg_types);

	      DECL_ASSEMBLER_NAME (decl) 
		= build_template_decl_overload 
		(DECL_NAME (decl), 
		 arg_types,
		 TREE_TYPE (TREE_TYPE (tmpl)),
		 DECL_INNERMOST_TEMPLATE_PARMS (tmpl),
		 targs, ctype != NULL_TREE);
	    }

	  if (is_friend && !have_def)
	    {
	      /* This is not really a declaration of a specialization.
		 It's just the name of an instantiation.  But, it's not
		 a request for an instantiation, either.  */
	      SET_DECL_IMPLICIT_INSTANTIATION (decl);
	      DECL_TEMPLATE_INFO (decl) 
		= perm_tree_cons (tmpl, targs, NULL_TREE);
	      return 1;
	    }

	  /* This function declaration is a template specialization.
	     Record that fact.  */
	  SET_DECL_TEMPLATE_SPECIALIZATION (decl);
	  DECL_TEMPLATE_SPECIALIZATIONS (tmpl) 
	    = perm_tree_cons (targs, decl, 
			      DECL_TEMPLATE_SPECIALIZATIONS
			      (tmpl));
	  /* If DECL_TI_TEMPLATE (decl), the decl is an
	     instantiation of a specialization of a member template.
	     (In other words, there was a member template, in a
	     class template.  That member template was specialized.
	     We then instantiated the class, so there is now an
	     instance of that specialization.)  

	     According to the CD2,

	     14.7.3.13 [tmpl.expl.spec]
	       
	     A specialization  of  a member function template or
	     member class template of a non-specialized class
	     template is itself a template.	        

	     So, we just leave the template info alone in this case.  */
	  if (!(DECL_TEMPLATE_INFO (decl) && DECL_TI_TEMPLATE (decl)))
	    DECL_TEMPLATE_INFO (decl)
	      = perm_tree_cons (tmpl, targs, NULL_TREE);
	  return 1;
	}
    }

  return 0;
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
      else
	idx = TEMPLATE_CONST_IDX (DECL_INITIAL (p));
      ++idx;
    }
  else
    idx = 0;

  if (!is_type)
    {
      tree tinfo = 0;
      my_friendly_assert (TREE_CODE (TREE_PURPOSE (parm)) == TREE_LIST, 260);
      /* is a const-param */
      parm = grokdeclarator (TREE_VALUE (parm), TREE_PURPOSE (parm),
			     PARM, 0, NULL_TREE);
      /* A template parameter is not modifiable.  */
      TREE_READONLY (parm) = 1;
      if (IS_AGGR_TYPE (TREE_TYPE (parm))
	  && TREE_CODE (TREE_TYPE (parm)) != TEMPLATE_TYPE_PARM)
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
      tinfo = make_node (TEMPLATE_CONST_PARM);
      my_friendly_assert (TREE_PERMANENT (tinfo), 260.5);
      if (TREE_PERMANENT (parm) == 0)
        {
	  parm = copy_node (parm);
	  TREE_PERMANENT (parm) = 1;
        }
      TREE_TYPE (tinfo) = TREE_TYPE (parm);
      decl = build_decl (CONST_DECL, DECL_NAME (parm), TREE_TYPE (parm));
      DECL_INITIAL (decl) = tinfo;
      DECL_INITIAL (parm) = tinfo;
      TEMPLATE_CONST_SET_INFO (tinfo, idx, processing_template_decl);
    }
  else
    {
      tree t = make_lang_type (TEMPLATE_TYPE_PARM);
      CLASSTYPE_GOT_SEMICOLON (t) = 1;
      decl = build_decl (TYPE_DECL, TREE_VALUE (parm), t);
      TYPE_NAME (t) = decl;
      TYPE_STUB_DECL (t) = decl;
      parm = decl;
      TEMPLATE_TYPE_SET_INFO (t, idx, processing_template_decl);
    }
  SET_DECL_ARTIFICIAL (decl);
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

  for (parm = parms, nparms = 0; parm; parm = TREE_CHAIN (parm), nparms++)
    TREE_VEC_ELT (saved_parmlist, nparms) = parm;

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
  poplevel (0, 0, 0);

  --processing_template_decl;
  current_template_parms = TREE_CHAIN (current_template_parms);
  (void) get_pending_sizes ();	/* Why? */
}

/* Generate a valid set of template args from current_template_parms.  */

tree
current_template_args ()
{
  tree header = current_template_parms;
  int length = list_length (header);
  tree args = make_tree_vec (length);
  int l = length;

  while (header)
    {
      tree a = copy_node (TREE_VALUE (header));
      int i = TREE_VEC_LENGTH (a);
      TREE_TYPE (a) = NULL_TREE;
      while (i--)
	{
	  tree t = TREE_VEC_ELT (a, i);

	  /* t will be a list if we are called from within a
	     begin/end_template_parm_list pair, but a vector directly
	     if within a begin/end_member_template_processing pair.  */
	  if (TREE_CODE (t) == TREE_LIST) 
	    {
	      t = TREE_VALUE (t);
	      
	      if (TREE_CODE (t) == TYPE_DECL)
		t = TREE_TYPE (t);
	      else
		t = DECL_INITIAL (t);
	    }

	  TREE_VEC_ELT (a, i) = t;
	}
      TREE_VEC_ELT (args, --l) = a;
      header = TREE_CHAIN (header);
    }

  return args;
}
  
void
push_template_decl (decl)
     tree decl;
{
  tree tmpl;
  tree args = NULL_TREE;
  tree info;
  tree ctx = DECL_CONTEXT (decl) ? DECL_CONTEXT (decl) : current_class_type;
  int primary = 0;

  /* Kludge! */
  if (TREE_CODE (decl) == FUNCTION_DECL && DECL_FRIEND_P (decl)
      && DECL_CLASS_CONTEXT (decl))
    ;
  /* Note that this template is a "primary template" */
  else if (! ctx || ! CLASSTYPE_TEMPLATE_INFO (ctx)
      /* || (processing_template_decl > CLASSTYPE_TEMPLATE_LEVEL (ctx)) */)
    primary = 1;

  /* Partial specialization.  */
  if (TREE_CODE (decl) == TYPE_DECL && DECL_ARTIFICIAL (decl)
      && CLASSTYPE_TEMPLATE_SPECIALIZATION (TREE_TYPE (decl)))
    {
      tree type = TREE_TYPE (decl);
      tree maintmpl = CLASSTYPE_TI_TEMPLATE (type);
      tree mainargs = CLASSTYPE_TI_ARGS (type);
      tree spec = DECL_TEMPLATE_SPECIALIZATIONS (maintmpl);

      for (; spec; spec = TREE_CHAIN (spec))
	{
	  /* purpose: args to main template
	     value: spec template */
	  if (comp_template_args (TREE_PURPOSE (spec), mainargs))
	    return;
	}

      DECL_TEMPLATE_SPECIALIZATIONS (maintmpl) = CLASSTYPE_TI_SPEC_INFO (type)
	= perm_tree_cons (mainargs, TREE_VALUE (current_template_parms),
			  DECL_TEMPLATE_SPECIALIZATIONS (maintmpl));
      TREE_TYPE (DECL_TEMPLATE_SPECIALIZATIONS (maintmpl)) = type;
      return;
    }

  args = current_template_args ();

  if (! ctx || TYPE_BEING_DEFINED (ctx))
    {
      tmpl = build_lang_decl (TEMPLATE_DECL, DECL_NAME (decl), NULL_TREE);
      DECL_TEMPLATE_PARMS (tmpl) = current_template_parms;
      DECL_CONTEXT (tmpl) = DECL_CONTEXT (decl);
      if (DECL_LANG_SPECIFIC (decl))
	{
	  DECL_CLASS_CONTEXT (tmpl) = DECL_CLASS_CONTEXT (decl);
	  DECL_STATIC_FUNCTION_P (tmpl) = 
	    DECL_STATIC_FUNCTION_P (decl);

	  if (DECL_TEMPLATE_SPECIALIZATION (decl))
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
      tree t;
      tree a;

      if (CLASSTYPE_TEMPLATE_INSTANTIATION (ctx))
	cp_error ("must specialize `%#T' before defining member `%#D'",
		  ctx, decl);
      if (TREE_CODE (decl) == TYPE_DECL && DECL_ARTIFICIAL (decl))
	tmpl = CLASSTYPE_TI_TEMPLATE (TREE_TYPE (decl));
      else if (! DECL_TEMPLATE_INFO (decl))
	{
	  cp_error ("template definition of non-template `%#D'", decl);
	  return;
	}
      else
	tmpl = DECL_TI_TEMPLATE (decl);
      
      if (is_member_template (tmpl))
	{
	  a = TREE_VEC_ELT (args, TREE_VEC_LENGTH (args) - 1);
	  t = DECL_INNERMOST_TEMPLATE_PARMS (DECL_TI_TEMPLATE (decl));
	  if (TREE_VEC_LENGTH (t) 
	      != TREE_VEC_LENGTH (a))
	    {
	      cp_error ("got %d template parameters for `%#D'",
			TREE_VEC_LENGTH (a), decl);
	      cp_error ("  but %d required", TREE_VEC_LENGTH (t));
	    }
	  if (TREE_VEC_LENGTH (args) > 1)
	    /* Get the template parameters for the enclosing template
	       class.  */ 
	    a = TREE_VEC_ELT (args, TREE_VEC_LENGTH (args) - 2);
	  else
	    a = NULL_TREE;
	}
      else 
	a = TREE_VEC_ELT (args, TREE_VEC_LENGTH (args) - 1);

      t = NULL_TREE;

      if (CLASSTYPE_TEMPLATE_SPECIALIZATION (ctx))
	{
	  /* When processing an inline member template of a
	     specialized class, there is no CLASSTYPE_TI_SPEC_INFO.  */
	  if (CLASSTYPE_TI_SPEC_INFO (ctx))
	    t = TREE_VALUE (CLASSTYPE_TI_SPEC_INFO (ctx));
	}
      else if (CLASSTYPE_TEMPLATE_INFO (ctx))
	t = DECL_INNERMOST_TEMPLATE_PARMS (CLASSTYPE_TI_TEMPLATE (ctx));

      /* There should be template arguments if and only if there is a
	 template class.  */
      my_friendly_assert((a != NULL_TREE) == (t != NULL_TREE), 0);

      if (t != NULL_TREE 
	  && TREE_VEC_LENGTH (t) != TREE_VEC_LENGTH (a))
	{
	  cp_error ("got %d template parameters for `%#D'",
		    TREE_VEC_LENGTH (a), decl);
	  cp_error ("  but `%#T' has %d", ctx, TREE_VEC_LENGTH (t));
	}
    }
  /* Get the innermost set of template arguments. */
  args = TREE_VEC_ELT (args, TREE_VEC_LENGTH (args) - 1);

  DECL_TEMPLATE_RESULT (tmpl) = decl;
  TREE_TYPE (tmpl) = TREE_TYPE (decl);

  if (! ctx)
    tmpl = pushdecl_top_level (tmpl);

  if (primary)
    TREE_TYPE (DECL_INNERMOST_TEMPLATE_PARMS (tmpl)) = tmpl;

  info = perm_tree_cons (tmpl, args, NULL_TREE);

  if (TREE_CODE (decl) == TYPE_DECL && DECL_ARTIFICIAL (decl))
    {
      CLASSTYPE_TEMPLATE_INFO (TREE_TYPE (tmpl)) = info;
      DECL_NAME (decl) = classtype_mangled_name (TREE_TYPE (decl));
    }
  else if (! DECL_LANG_SPECIFIC (decl))
    cp_error ("template declaration of `%#D'", decl);
  else
    DECL_TEMPLATE_INFO (decl) = info;
}

/* Convert all template arguments to their appropriate types, and return
   a vector containing the resulting values.  If any error occurs, return
   error_mark_node.  */

static tree
coerce_template_parms (parms, arglist, in_decl)
     tree parms, arglist;
     tree in_decl;
{
  int nparms, nargs, i, lost = 0;
  tree vec;

  if (arglist == NULL_TREE)
    nargs = 0;
  else if (TREE_CODE (arglist) == TREE_VEC)
    nargs = TREE_VEC_LENGTH (arglist);
  else
    nargs = list_length (arglist);

  nparms = TREE_VEC_LENGTH (parms);

  if (nargs > nparms
      || (nargs < nparms
	  && TREE_PURPOSE (TREE_VEC_ELT (parms, nargs)) == NULL_TREE))
    {
      error ("incorrect number of parameters (%d, should be %d)",
	     nargs, nparms);
      if (in_decl)
	cp_error_at ("in template expansion for decl `%D'", in_decl);
      return error_mark_node;
    }

  if (arglist && TREE_CODE (arglist) == TREE_VEC)
    vec = copy_node (arglist);
  else
    {
      vec = make_tree_vec (nparms);
      for (i = 0; i < nparms; i++)
	{
	  tree arg;

	  if (arglist)
	    {
	      arg = arglist;
	      arglist = TREE_CHAIN (arglist);

	      if (arg == error_mark_node)
		lost++;
	      else
		arg = TREE_VALUE (arg);
	    }
	  else if (TREE_CODE (TREE_VALUE (TREE_VEC_ELT (parms, i)))
		   == TYPE_DECL)
	    arg = tsubst (TREE_PURPOSE (TREE_VEC_ELT (parms, i)),
			  vec, i, in_decl);
	  else
	    arg = tsubst_expr (TREE_PURPOSE (TREE_VEC_ELT (parms, i)),
			       vec, i, in_decl);

	  TREE_VEC_ELT (vec, i) = arg;
	}
    }
  for (i = 0; i < nparms; i++)
    {
      tree arg = TREE_VEC_ELT (vec, i);
      tree parm = TREE_VALUE (TREE_VEC_ELT (parms, i));
      tree val = 0;
      int is_type, requires_type;

      is_type = TREE_CODE_CLASS (TREE_CODE (arg)) == 't';
      requires_type = TREE_CODE (parm) == TYPE_DECL;

      if (requires_type && ! is_type && TREE_CODE (arg) == SCOPE_REF
	  && TREE_CODE (TREE_OPERAND (arg, 0)) == TEMPLATE_TYPE_PARM)
	{
	  cp_pedwarn ("to refer to a type member of a template parameter,");
	  cp_pedwarn ("  use `typename %E'", arg);
	  arg = make_typename_type (TREE_OPERAND (arg, 0),
				    TREE_OPERAND (arg, 1));
	  is_type = 1;
	}
      if (is_type != requires_type)
	{
	  if (in_decl)
	    {
	      cp_error ("type/value mismatch at argument %d in template parameter list for `%D'",
			i, in_decl);
	      if (is_type)
		cp_error ("  expected a constant of type `%T', got `%T'",
			  TREE_TYPE (parm), arg);
	      else
		cp_error ("  expected a type, got `%E'", arg);
	    }
	  lost++;
	  TREE_VEC_ELT (vec, i) = error_mark_node;
	  continue;
	}
      if (is_type)
	{
	  val = groktypename (arg);
	  if (! processing_template_decl)
	    {
	      tree t = target_type (val);
	      if (TREE_CODE (t) != TYPENAME_TYPE 
		  && IS_AGGR_TYPE (t)
		  && decl_function_context (TYPE_MAIN_DECL (t)))
		{
		  cp_error ("type `%T' composed from a local class is not a valid template-argument", val);
		  return error_mark_node;
		}
	    }
	}
      else
	{
	  tree t = tsubst (TREE_TYPE (parm), vec,
			   TREE_VEC_LENGTH (vec), in_decl);
	  if (processing_template_decl)
	    val = arg;
	  else
	    val = digest_init (t, arg, (tree *) 0);

	  if (val == error_mark_node || processing_template_decl)
	    ;

	  /* 14.2: Other template-arguments must be constant-expressions,
	     addresses of objects or functions with external linkage, or of
	     static class members.  */
	  else if (IS_AGGR_TYPE (TREE_TYPE (val)))
	    {
	      cp_error ("object `%E' cannot be used as template argument", arg);
	      val = error_mark_node;
	    }
	  else if (!TREE_CONSTANT (val))
	    {
	      cp_error ("non-constant `%E' cannot be used as template argument",
			arg);
	      val = error_mark_node;
	    }
	  else if (POINTER_TYPE_P (TREE_TYPE (val))
		   && ! integer_zerop (val)
		   && TREE_CODE (TREE_TYPE (TREE_TYPE (val))) != OFFSET_TYPE
		   && TREE_CODE (TREE_TYPE (TREE_TYPE (val))) != METHOD_TYPE)
	    {
	      t = val;
	      STRIP_NOPS (t);
	      if (TREE_CODE (t) == ADDR_EXPR)
		{
		  tree a = TREE_OPERAND (t, 0);
		  STRIP_NOPS (a);
		  if (TREE_CODE (a) == STRING_CST)
		    {
		      cp_error ("string literal %E is not a valid template argument", a);
		      error ("because it is the address of an object with static linkage");
		      val = error_mark_node;
		    }
		  else if (TREE_CODE (a) != VAR_DECL
			   && TREE_CODE (a) != FUNCTION_DECL)
		    goto bad;
		  else if (! TREE_PUBLIC (a))
		    {
		      cp_error ("address of non-extern `%E' cannot be used as template argument", a);
		      val = error_mark_node;
		    }
		}
	      else
		{
		bad:
		  cp_error ("`%E' is not a valid template argument", t);
		  error ("it must be %s%s with external linkage",
			 TREE_CODE (TREE_TYPE (val)) == POINTER_TYPE
			 ? "a pointer to " : "",
			 TREE_CODE (TREE_TYPE (TREE_TYPE (val))) == FUNCTION_TYPE
			 ? "a function" : "an object");
		  val = error_mark_node;
		}
	    }
	}

      if (val == error_mark_node)
	lost++;

      TREE_VEC_ELT (vec, i) = val;
    }
  if (lost)
    return error_mark_node;
  return vec;
}

static int
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

      if (nt == ot)
	continue;
      if (TREE_CODE (nt) != TREE_CODE (ot))
	return 0;
      if (TREE_CODE (nt) == TREE_VEC)
        {
          /* For member templates */
	  if (comp_template_args (nt, ot))
	    continue;
        }
      else if (TREE_CODE_CLASS (TREE_CODE (ot)) == 't')
	{
	  if (comptypes (ot, nt, 1))
	    continue;
	}
      else if (cp_tree_equal (ot, nt) > 0)
	continue;
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

#if 0
#define buflen	sizeof(buf)
#define check	if (bufp >= buf+buflen-1) goto too_long
#define ccat(c) *bufp++=(c); check
#define advance	bufp+=strlen(bufp); check
#define cat(s)	strncpy(bufp, s, buf+buflen-bufp-1); advance
#else
#define check
#define ccat(c)	obstack_1grow (&scratch_obstack, (c));
#define advance
#define cat(s)	obstack_grow (&scratch_obstack, (s), strlen (s))
#endif

  cat (name);
  ccat ('<');
  nparms = TREE_VEC_LENGTH (parms);
  my_friendly_assert (nparms == TREE_VEC_LENGTH (arglist), 268);
  for (i = 0; i < nparms; i++)
    {
      tree parm = TREE_VALUE (TREE_VEC_ELT (parms, i));
      tree arg = TREE_VEC_ELT (arglist, i);

      if (i)
	ccat (',');

      if (TREE_CODE (parm) == TYPE_DECL)
	{
	  cat (type_as_string (arg, 0));
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

#if 0
 too_long:
#endif
  fatal ("out of (preallocated) string space creating template instantiation name");
  /* NOTREACHED */
  return NULL;
}

static tree
classtype_mangled_name (t)
     tree t;
{
  if (CLASSTYPE_TEMPLATE_INFO (t)
      && PRIMARY_TEMPLATE_P (CLASSTYPE_TI_TEMPLATE (t)))
    {
      tree name = DECL_NAME (CLASSTYPE_TI_TEMPLATE (t));
      char *mangled_name = mangle_class_name_for_template
	(IDENTIFIER_POINTER (name),
	 DECL_INNERMOST_TEMPLATE_PARMS (CLASSTYPE_TI_TEMPLATE (t)),
	 CLASSTYPE_TI_ARGS (t));
      tree id = get_identifier (mangled_name);
      IDENTIFIER_TEMPLATE (id) = name;
      return id;
    }
  else
    return TYPE_IDENTIFIER (t);
}

static void
add_pending_template (d)
     tree d;
{
  tree ti;

  if (TREE_CODE_CLASS (TREE_CODE (d)) == 't')
    ti = CLASSTYPE_TEMPLATE_INFO (d);
  else
    ti = DECL_TEMPLATE_INFO (d);

  if (TI_PENDING_TEMPLATE_FLAG (ti))
    return;

  *template_tail = perm_tree_cons
    (current_function_decl, d, NULL_TREE);
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
  if (fns == NULL_TREE)
    {
      cp_error ("non-template used as template");
      return error_mark_node;
    }

  if (arglist != NULL_TREE && !TREE_PERMANENT (arglist))
    {
      push_obstacks (&permanent_obstack, &permanent_obstack);
      arglist = copy_list (arglist);
      pop_obstacks ();
    }

  return build_min (TEMPLATE_ID_EXPR,
		    TREE_TYPE (fns) 
		    ? TREE_TYPE (fns) : unknown_type_node, 
		    fns, arglist);  
}


/* Given an IDENTIFIER_NODE (type TEMPLATE_DECL) and a chain of
   parameters, find the desired type.

   D1 is the PTYPENAME terminal, and ARGLIST is the list of arguments.
   Since ARGLIST is build on the decl_obstack, we must copy it here
   to keep it from being reclaimed when the decl storage is reclaimed.

   IN_DECL, if non-NULL, is the template declaration we are trying to
   instantiate.  */

tree
lookup_template_class (d1, arglist, in_decl)
     tree d1, arglist;
     tree in_decl;
{
  tree template, parmlist;
  char *mangled_name;
  tree id, t;

  if (TREE_CODE (d1) == IDENTIFIER_NODE)
    {
      template = IDENTIFIER_GLOBAL_VALUE (d1); /* XXX */
      if (! template)
	template = IDENTIFIER_CLASS_VALUE (d1);
    }
  else if (TREE_CODE (d1) == TYPE_DECL && IS_AGGR_TYPE (TREE_TYPE (d1)))
    {
      template = CLASSTYPE_TI_TEMPLATE (TREE_TYPE (d1));
      d1 = DECL_NAME (template);
    }
  else if (TREE_CODE_CLASS (TREE_CODE (d1)) == 't' && IS_AGGR_TYPE (d1))
    {
      template = CLASSTYPE_TI_TEMPLATE (d1);
      d1 = DECL_NAME (template);
    }
  else
    my_friendly_abort (272);

  /* With something like `template <class T> class X class X { ... };'
     we could end up with D1 having nothing but an IDENTIFIER_LOCAL_VALUE.
     We don't want to do that, but we have to deal with the situation, so
     let's give them some syntax errors to chew on instead of a crash.  */
  if (! template)
    return error_mark_node;
  if (TREE_CODE (template) != TEMPLATE_DECL)
    {
      cp_error ("non-template type `%T' used as a template", d1);
      if (in_decl)
	cp_error_at ("for template declaration `%D'", in_decl);
      return error_mark_node;
    }

  if (PRIMARY_TEMPLATE_P (template))
    {
      parmlist = DECL_INNERMOST_TEMPLATE_PARMS (template);

      arglist = coerce_template_parms (parmlist, arglist, template);
      if (arglist == error_mark_node)
	return error_mark_node;
      if (uses_template_parms (arglist))
	{
	  tree found;
	  if (comp_template_args
	      (CLASSTYPE_TI_ARGS (TREE_TYPE (template)), arglist))
	    found = TREE_TYPE (template);
	  else
	    {
	      for (found = DECL_TEMPLATE_INSTANTIATIONS (template);
		   found; found = TREE_CHAIN (found))
		{
		  if (TI_USES_TEMPLATE_PARMS (found)
		      && comp_template_args (TREE_PURPOSE (found), arglist))
		    break;
		}
	      if (found)
		found = TREE_VALUE (found);
	    }

	  if (found)
	    {
	      if (can_free (&permanent_obstack, arglist))
		obstack_free (&permanent_obstack, arglist);
	      return found;
	    }
	}

      mangled_name = mangle_class_name_for_template (IDENTIFIER_POINTER (d1),
						     parmlist, arglist);
      id = get_identifier (mangled_name);
      IDENTIFIER_TEMPLATE (id) = d1;

      maybe_push_to_top_level (uses_template_parms (arglist));
      t = xref_tag_from_type (TREE_TYPE (template), id, 1);
      pop_from_top_level ();
    }
  else
    {
      tree ctx = lookup_template_class (TYPE_CONTEXT (TREE_TYPE (template)),
					arglist, in_decl);
      id = d1;
      arglist = CLASSTYPE_TI_ARGS (ctx);

      if (TYPE_BEING_DEFINED (ctx) && ctx == current_class_type)
	{
	  int save_temp = processing_template_decl;
	  processing_template_decl = 0;
	  t = xref_tag_from_type (TREE_TYPE (template), id, 0);
	  processing_template_decl = save_temp;
	}
      else
	{
	  t = lookup_nested_type_by_name (ctx, id);
	  my_friendly_assert (t != NULL_TREE, 42);
	}
    }

  /* Seems to be wanted.  */
  CLASSTYPE_GOT_SEMICOLON (t) = 1;

  if (! CLASSTYPE_TEMPLATE_INFO (t))
    {
      arglist = copy_to_permanent (arglist);
      CLASSTYPE_TEMPLATE_INFO (t)
	= perm_tree_cons (template, arglist, NULL_TREE);
      DECL_TEMPLATE_INSTANTIATIONS (template) = perm_tree_cons
	(arglist, t, DECL_TEMPLATE_INSTANTIATIONS (template));
      TI_USES_TEMPLATE_PARMS (DECL_TEMPLATE_INSTANTIATIONS (template))
	= uses_template_parms (arglist);

      SET_CLASSTYPE_IMPLICIT_INSTANTIATION (t);

      /* We need to set this again after CLASSTYPE_TEMPLATE_INFO is set up.  */
      DECL_ASSEMBLER_NAME (TYPE_MAIN_DECL (t)) = id;
      /* if (! uses_template_parms (arglist)) */
	DECL_ASSEMBLER_NAME (TYPE_MAIN_DECL (t)) 
	  = get_identifier (build_overload_name (t, 1, 1));

      if (flag_external_templates && ! uses_template_parms (arglist)
	  && CLASSTYPE_INTERFACE_KNOWN (TREE_TYPE (template))
	  && ! CLASSTYPE_INTERFACE_ONLY (TREE_TYPE (template)))
	add_pending_template (t);
    }

  return t;
}

/* Should be defined in parse.h.  */
extern int yychar;

int
uses_template_parms (t)
     tree t;
{
  if (!t)
    return 0;
  switch (TREE_CODE (t))
    {
    case INDIRECT_REF:
    case COMPONENT_REF:
      /* We assume that the object must be instantiated in order to build
	 the COMPONENT_REF, so we test only whether the type of the
	 COMPONENT_REF uses template parms.  */
      return uses_template_parms (TREE_TYPE (t));

    case IDENTIFIER_NODE:
      if (!IDENTIFIER_TEMPLATE (t))
	return 0;
      my_friendly_abort (42);

      /* aggregates of tree nodes */
    case TREE_VEC:
      {
	int i = TREE_VEC_LENGTH (t);
	while (i--)
	  if (uses_template_parms (TREE_VEC_ELT (t, i)))
	    return 1;
	return 0;
      }
    case TREE_LIST:
      if (uses_template_parms (TREE_PURPOSE (t))
	  || uses_template_parms (TREE_VALUE (t)))
	return 1;
      return uses_template_parms (TREE_CHAIN (t));

      /* constructed type nodes */
    case POINTER_TYPE:
    case REFERENCE_TYPE:
      return uses_template_parms (TREE_TYPE (t));
    case RECORD_TYPE:
      if (TYPE_PTRMEMFUNC_FLAG (t))
	return uses_template_parms (TYPE_PTRMEMFUNC_FN_TYPE (t));
    case UNION_TYPE:
      if (! CLASSTYPE_TEMPLATE_INFO (t))
	return 0;
      return uses_template_parms (TREE_VALUE (CLASSTYPE_TEMPLATE_INFO (t)));
    case FUNCTION_TYPE:
      if (uses_template_parms (TYPE_ARG_TYPES (t)))
	return 1;
      return uses_template_parms (TREE_TYPE (t));
    case ARRAY_TYPE:
      if (uses_template_parms (TYPE_DOMAIN (t)))
	return 1;
      return uses_template_parms (TREE_TYPE (t));
    case OFFSET_TYPE:
      if (uses_template_parms (TYPE_OFFSET_BASETYPE (t)))
	return 1;
      return uses_template_parms (TREE_TYPE (t));
    case METHOD_TYPE:
      if (uses_template_parms (TYPE_METHOD_BASETYPE (t)))
	return 1;
      if (uses_template_parms (TYPE_ARG_TYPES (t)))
	return 1;
      return uses_template_parms (TREE_TYPE (t));

      /* decl nodes */
    case TYPE_DECL:
      return uses_template_parms (TREE_TYPE (t));

    case FUNCTION_DECL:
    case VAR_DECL:
      /* ??? What about FIELD_DECLs?  */
      if (DECL_LANG_SPECIFIC (t) && DECL_TEMPLATE_INFO (t)
	  && uses_template_parms (DECL_TI_ARGS (t)))
	return 1;
      /* fall through */
    case CONST_DECL:
    case PARM_DECL:
      if (uses_template_parms (TREE_TYPE (t)))
	return 1;
      if (DECL_CONTEXT (t) && uses_template_parms (DECL_CONTEXT (t)))
	return 1;
      return 0;

    case CALL_EXPR:
      return uses_template_parms (TREE_TYPE (t));
    case ADDR_EXPR:
      return uses_template_parms (TREE_OPERAND (t, 0));

      /* template parm nodes */
    case TEMPLATE_TYPE_PARM:
    case TEMPLATE_CONST_PARM:
      return 1;

      /* simple type nodes */
    case INTEGER_TYPE:
      if (uses_template_parms (TYPE_MIN_VALUE (t)))
	return 1;
      return uses_template_parms (TYPE_MAX_VALUE (t));

    case REAL_TYPE:
    case COMPLEX_TYPE:
    case VOID_TYPE:
    case BOOLEAN_TYPE:
      return 0;

    case ENUMERAL_TYPE:
      {
	tree v;

	for (v = TYPE_VALUES (t); v != NULL_TREE; v = TREE_CHAIN (v))
	  if (uses_template_parms (TREE_VALUE (v)))
	    return 1;
      }
      return 0;

      /* constants */
    case INTEGER_CST:
    case REAL_CST:
    case STRING_CST:
      return 0;

    case ERROR_MARK:
      /* Non-error_mark_node ERROR_MARKs are bad things.  */
      my_friendly_assert (t == error_mark_node, 274);
      /* NOTREACHED */
      return 0;

    case LOOKUP_EXPR:
    case TYPENAME_TYPE:
      return 1;

    case SCOPE_REF:
      return uses_template_parms (TREE_OPERAND (t, 0));

    case CONSTRUCTOR:
      if (TREE_TYPE (t) && TYPE_PTRMEMFUNC_P (TREE_TYPE (t)))
	return uses_template_parms (TYPE_PTRMEMFUNC_FN_TYPE (TREE_TYPE (t)));
      return uses_template_parms (TREE_OPERAND (t, 1));

    case MODOP_EXPR:
    case CAST_EXPR:
    case REINTERPRET_CAST_EXPR:
    case CONST_CAST_EXPR:
    case STATIC_CAST_EXPR:
    case DYNAMIC_CAST_EXPR:
    case SIZEOF_EXPR:
    case ARROW_EXPR:
    case DOTSTAR_EXPR:
    case TYPEID_EXPR:
      return 1;

    default:
      switch (TREE_CODE_CLASS (TREE_CODE (t)))
	{
	case '1':
	case '2':
	case 'e':
	case '<':
	  {
	    int i;
	    for (i = tree_code_length[(int) TREE_CODE (t)]; --i >= 0;)
	      if (uses_template_parms (TREE_OPERAND (t, i)))
		return 1;
	    return 0;
	  }
	default:
	  break;
	}
      sorry ("testing %s for template parms",
	     tree_code_name [(int) TREE_CODE (t)]);
      my_friendly_abort (82);
      /* NOTREACHED */
      return 0;
    }
}

static struct tinst_level *current_tinst_level = 0;
static struct tinst_level *free_tinst_level = 0;
static int tinst_depth = 0;
extern int max_tinst_depth;
#ifdef GATHER_STATISTICS
int depth_reached = 0;
#endif

static int
push_tinst_level (d)
     tree d;
{
  struct tinst_level *new;

  if (tinst_depth >= max_tinst_depth)
    {
      struct tinst_level *p = current_tinst_level;
      int line = lineno;
      char *file = input_filename;

      error ("template instantiation depth exceeds maximum of %d",
	     max_tinst_depth);
      error (" (use -ftemplate-depth-NN to increase the maximum)");
      cp_error ("  instantiating `%D'", d);

      for (; p; p = p->next)
	{
	  cp_error ("  instantiated from `%D'", p->decl);
	  lineno = p->line;
	  input_filename = p->file;
	}
      error ("  instantiated from here");

      lineno = line;
      input_filename = file;

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

  return 1;
}

void
pop_tinst_level ()
{
  struct tinst_level *old = current_tinst_level;

  current_tinst_level = old->next;
  old->next = free_tinst_level;
  free_tinst_level = old;
  --tinst_depth;
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

tree
instantiate_class_template (type)
     tree type;
{
  tree template, template_info, args, pattern, t, *field_chain;

  if (type == error_mark_node)
    return error_mark_node;

  template_info = CLASSTYPE_TEMPLATE_INFO (type);

  if (TYPE_BEING_DEFINED (type) || TYPE_SIZE (type))
    return type;

  template = TI_TEMPLATE (template_info);
  my_friendly_assert (TREE_CODE (template) == TEMPLATE_DECL, 279);
  args = TI_ARGS (template_info);

  t = most_specialized_class
    (DECL_TEMPLATE_SPECIALIZATIONS (template), args);

  if (t == error_mark_node)
    {
      char *str = "candidates are:";
      cp_error ("ambiguous class template instantiation for `%#T'", type);
      for (t = DECL_TEMPLATE_SPECIALIZATIONS (template); t; t = TREE_CHAIN (t))
	{
	  if (get_class_bindings (TREE_VALUE (t), TREE_PURPOSE (t), args))
	    {
	      cp_error_at ("%s %+#T", str, TREE_TYPE (t));
	      str = "               ";
	    }
	}
      TYPE_BEING_DEFINED (type) = 1;
      return error_mark_node;
    }
  else if (t)
    pattern = TREE_TYPE (t);
  else
    pattern = TREE_TYPE (template);

  if (TYPE_SIZE (pattern) == NULL_TREE)
    return type;

  if (t)
    args = get_class_bindings (TREE_VALUE (t), TREE_PURPOSE (t), args);

  TYPE_BEING_DEFINED (type) = 1;

  if (! push_tinst_level (type))
    return type;

  maybe_push_to_top_level (uses_template_parms (type));
  pushclass (type, 0);

  if (flag_external_templates)
    {
      if (flag_alt_external_templates)
	{
	  CLASSTYPE_INTERFACE_ONLY (type) = interface_only;
	  SET_CLASSTYPE_INTERFACE_UNKNOWN_X (type, interface_unknown);
	  CLASSTYPE_VTABLE_NEEDS_WRITING (type)
	    = ! CLASSTYPE_INTERFACE_ONLY (type)
	      && CLASSTYPE_INTERFACE_KNOWN (type);
	}
      else
	{
	  CLASSTYPE_INTERFACE_ONLY (type) = CLASSTYPE_INTERFACE_ONLY (pattern);
	  SET_CLASSTYPE_INTERFACE_UNKNOWN_X
	    (type, CLASSTYPE_INTERFACE_UNKNOWN (pattern));
	  CLASSTYPE_VTABLE_NEEDS_WRITING (type)
	    = ! CLASSTYPE_INTERFACE_ONLY (type)
	      && CLASSTYPE_INTERFACE_KNOWN (type);
	}
    }
  else
    {
      SET_CLASSTYPE_INTERFACE_UNKNOWN (type);
      CLASSTYPE_VTABLE_NEEDS_WRITING (type) = 1;
    }

  TYPE_HAS_CONSTRUCTOR (type) = TYPE_HAS_CONSTRUCTOR (pattern);
  TYPE_HAS_DESTRUCTOR (type) = TYPE_HAS_DESTRUCTOR (pattern);
  TYPE_HAS_ASSIGNMENT (type) = TYPE_HAS_ASSIGNMENT (pattern);
  TYPE_OVERLOADS_CALL_EXPR (type) = TYPE_OVERLOADS_CALL_EXPR (pattern);
  TYPE_OVERLOADS_ARRAY_REF (type) = TYPE_OVERLOADS_ARRAY_REF (pattern);
  TYPE_OVERLOADS_ARROW (type) = TYPE_OVERLOADS_ARROW (pattern);
  TYPE_GETS_NEW (type) = TYPE_GETS_NEW (pattern);
  TYPE_GETS_DELETE (type) = TYPE_GETS_DELETE (pattern);
  TYPE_VEC_DELETE_TAKES_SIZE (type) = TYPE_VEC_DELETE_TAKES_SIZE (pattern);
  TYPE_HAS_ASSIGN_REF (type) = TYPE_HAS_ASSIGN_REF (pattern);
  TYPE_HAS_CONST_ASSIGN_REF (type) = TYPE_HAS_CONST_ASSIGN_REF (pattern);
  TYPE_HAS_ABSTRACT_ASSIGN_REF (type) = TYPE_HAS_ABSTRACT_ASSIGN_REF (pattern);
  TYPE_HAS_INIT_REF (type) = TYPE_HAS_INIT_REF (pattern);
  TYPE_HAS_CONST_INIT_REF (type) = TYPE_HAS_CONST_INIT_REF (pattern);
  TYPE_GETS_INIT_AGGR (type) = TYPE_GETS_INIT_AGGR (pattern);
  TYPE_HAS_DEFAULT_CONSTRUCTOR (type) = TYPE_HAS_DEFAULT_CONSTRUCTOR (pattern);
  TYPE_HAS_CONVERSION (type) = TYPE_HAS_CONVERSION (pattern);
  TYPE_USES_COMPLEX_INHERITANCE (type)
    = TYPE_USES_COMPLEX_INHERITANCE (pattern);
  TYPE_USES_MULTIPLE_INHERITANCE (type)
    = TYPE_USES_MULTIPLE_INHERITANCE (pattern);
  TYPE_USES_VIRTUAL_BASECLASSES (type)
    = TYPE_USES_VIRTUAL_BASECLASSES (pattern);
  TYPE_PACKED (type) = TYPE_PACKED (pattern);
  TYPE_ALIGN (type) = TYPE_ALIGN (pattern);

  {
    tree binfo = TYPE_BINFO (type);
    tree pbases = TYPE_BINFO_BASETYPES (pattern);

    if (pbases)
      {
	tree bases;
	int i;
	int len = TREE_VEC_LENGTH (pbases);
	bases = make_tree_vec (len);
	for (i = 0; i < len; ++i)
	  {
	    tree elt;

	    TREE_VEC_ELT (bases, i) = elt
	      = tsubst (TREE_VEC_ELT (pbases, i), args,
			TREE_VEC_LENGTH (args), NULL_TREE);
	    BINFO_INHERITANCE_CHAIN (elt) = binfo;

	    if (! IS_AGGR_TYPE (TREE_TYPE (elt)))
	      cp_error
		("base type `%T' of `%T' fails to be a struct or class type",
		 TREE_TYPE (elt), type);
	    else if (! uses_template_parms (type)
		     && (TYPE_SIZE (complete_type (TREE_TYPE (elt)))
			 == NULL_TREE))
	      cp_error ("base class `%T' of `%T' has incomplete type",
			TREE_TYPE (elt), type);
	  }
	/* Don't initialize this until the vector is filled out, or
	   lookups will crash.  */
	BINFO_BASETYPES (binfo) = bases;
      }
  }

  CLASSTYPE_LOCAL_TYPEDECLS (type) = CLASSTYPE_LOCAL_TYPEDECLS (pattern);

  field_chain = &TYPE_FIELDS (type);

  for (t = CLASSTYPE_TAGS (pattern); t; t = TREE_CHAIN (t))
    {
      tree name = TREE_PURPOSE (t);
      tree tag = TREE_VALUE (t);

      /* These will add themselves to CLASSTYPE_TAGS for the new type.  */
      if (TREE_CODE (tag) == ENUMERAL_TYPE)
	{
	  tree e, newtag = tsubst_enum (tag, args, 
					TREE_VEC_LENGTH (args), field_chain);

	  while (*field_chain)
	    {
	      DECL_FIELD_CONTEXT (*field_chain) = type;
	      field_chain = &TREE_CHAIN (*field_chain);
	    }
	}
      else
	tsubst (tag, args,
		TREE_VEC_LENGTH (args), NULL_TREE);
    }

  /* Don't replace enum constants here.  */
  for (t = TYPE_FIELDS (pattern); t; t = TREE_CHAIN (t))
    if (TREE_CODE (t) != CONST_DECL)
      {
	tree r = tsubst (t, args,
			 TREE_VEC_LENGTH (args), NULL_TREE);
	if (TREE_CODE (r) == VAR_DECL)
	  {
	    if (! uses_template_parms (r))
	      pending_statics = perm_tree_cons (NULL_TREE, r, pending_statics);
	    /* Perhaps I should do more of grokfield here.  */
	    start_decl_1 (r);
	    DECL_IN_AGGR_P (r) = 1;
	    DECL_EXTERNAL (r) = 1;
	    cp_finish_decl (r, DECL_INITIAL (r), NULL_TREE, 0, 0);
	  }

	*field_chain = r;
	field_chain = &TREE_CHAIN (r);
      }

  TYPE_METHODS (type) = tsubst_chain (TYPE_METHODS (pattern), args);
  for (t = TYPE_METHODS (type); t; t = TREE_CHAIN (t))
    {
      if (DECL_CONSTRUCTOR_P (t))
	grok_ctor_properties (type, t);
      else if (IDENTIFIER_OPNAME_P (DECL_NAME (t)))
	grok_op_properties (t, DECL_VIRTUAL_P (t), 0);
    }

  DECL_FRIENDLIST (TYPE_MAIN_DECL (type))
    = tsubst (DECL_FRIENDLIST (TYPE_MAIN_DECL (pattern)),
	      args, TREE_VEC_LENGTH (args), NULL_TREE);

  {
    tree d = CLASSTYPE_FRIEND_CLASSES (type)
      = tsubst (CLASSTYPE_FRIEND_CLASSES (pattern), args,
		TREE_VEC_LENGTH (args), NULL_TREE);

    /* This does injection for friend classes.  */
    for (; d; d = TREE_CHAIN (d))
      TREE_VALUE (d) = xref_tag_from_type (TREE_VALUE (d), NULL_TREE, 1);

    d = tsubst (DECL_TEMPLATE_INJECT (template), args,
		TREE_VEC_LENGTH (args), NULL_TREE);

    for (; d; d = TREE_CHAIN (d))
      {
	tree t = TREE_VALUE (d);

	if (TREE_CODE (t) == TYPE_DECL)
	  /* Already injected.  */;
	else
	  pushdecl (t);
      }
  }

  if (! uses_template_parms (type))
    {
      tree tmp;
      for (tmp = TYPE_FIELDS (type); tmp; tmp = TREE_CHAIN (tmp))
	if (TREE_CODE (tmp) == FIELD_DECL)
	  {
	    TREE_TYPE (tmp) = complete_type (TREE_TYPE (tmp));
	    require_complete_type (tmp);
	  }

      type = finish_struct_1 (type, 0);
      CLASSTYPE_GOT_SEMICOLON (type) = 1;

      repo_template_used (type);
      if (at_eof && TYPE_BINFO_VTABLE (type) != NULL_TREE)
	finish_prevtable_vardecl (NULL, TYPE_BINFO_VTABLE (type));
    }
  else
    {
      TYPE_SIZE (type) = integer_zero_node;
      CLASSTYPE_METHOD_VEC (type)
	= finish_struct_methods (type, TYPE_METHODS (type), 1);
    }

  TYPE_BEING_DEFINED (type) = 0;
  popclass (0);

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

tree 
lookup_nested_type_by_name (ctype, name)
        tree ctype, name;
{
  tree t;

  complete_type (ctype);

  for (t = CLASSTYPE_TAGS (ctype); t; t = TREE_CHAIN (t))
    {
      if (name == TREE_PURPOSE (t)
	  /* this catches typedef enum { foo } bar; */
	  || name == TYPE_IDENTIFIER (TREE_VALUE (t)))
	return TREE_VALUE (t);
    }
  return NULL_TREE;
}

/* If arg is a non-type template parameter that does not depend on template
   arguments, fold it like we weren't in the body of a template.  */

static tree
maybe_fold_nontype_arg (arg)
     tree arg;
{
  if (TREE_CODE_CLASS (TREE_CODE (arg)) != 't'
      && !uses_template_parms (arg))
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

/* Take the tree structure T and replace template parameters used therein
   with the argument vector ARGS.  NARGS is the number of args; should
   be removed.  IN_DECL is an associated decl for diagnostics.

   tsubst is used for dealing with types, decls and the like; for
   expressions, use tsubst_expr or tsubst_copy.  */

tree
tsubst (t, args, nargs, in_decl)
     tree t, args;
     int nargs;
     tree in_decl;
{
  tree type;

  if (t == NULL_TREE || t == error_mark_node
      || t == integer_type_node
      || t == void_type_node
      || t == char_type_node)
    return t;

  type = TREE_TYPE (t);
  if (type == unknown_type_node)
    my_friendly_abort (42);
  if (type && TREE_CODE (t) != FUNCTION_DECL
      && TREE_CODE (t) != TYPENAME_TYPE)
    type = tsubst (type, args, nargs, in_decl);

  switch (TREE_CODE (t))
    {
    case RECORD_TYPE:
      if (TYPE_PTRMEMFUNC_P (t))
	{
	  tree r = build_ptrmemfunc_type
	    (tsubst (TYPE_PTRMEMFUNC_FN_TYPE (t), args, nargs, in_decl));
	  return cp_build_type_variant (r, TYPE_READONLY (t),
					TYPE_VOLATILE (t));
	}

      /* else fall through */
    case UNION_TYPE:
      if (uses_template_parms (t))
	{
	  tree argvec = tsubst (CLASSTYPE_TI_ARGS (t), args, nargs, in_decl);
	  tree r = lookup_template_class (t, argvec, in_decl);
	  return cp_build_type_variant (r, TYPE_READONLY (t),
					TYPE_VOLATILE (t));
	}

      /* else fall through */
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

    case ENUMERAL_TYPE:
      {
	tree ctx = tsubst (TYPE_CONTEXT (t), args, nargs, in_decl);
	if (ctx == NULL_TREE)
	  return t;
	else if (ctx == current_function_decl)
	  return lookup_name (TYPE_IDENTIFIER (t), 1);
	else
	  return lookup_nested_type_by_name (ctx, TYPE_IDENTIFIER (t));
      }

    case INTEGER_TYPE:
      if (t == integer_type_node)
	return t;

      if (TREE_CODE (TYPE_MIN_VALUE (t)) == INTEGER_CST
	  && TREE_CODE (TYPE_MAX_VALUE (t)) == INTEGER_CST)
	return t;

      {
	tree max = TREE_OPERAND (TYPE_MAX_VALUE (t), 0);
	max = tsubst_expr (max, args, nargs, in_decl);
	if (processing_template_decl)
	  {
	    tree itype = make_node (INTEGER_TYPE);
	    TYPE_MIN_VALUE (itype) = size_zero_node;
	    TYPE_MAX_VALUE (itype) = build_min (MINUS_EXPR, sizetype, max,
						integer_one_node);
	    return itype;
	  }

	max = fold (build_binary_op (MINUS_EXPR, max, integer_one_node, 1));
	return build_index_2_type (size_zero_node, max);
      }

    case TEMPLATE_TYPE_PARM:
    case TEMPLATE_CONST_PARM:
      {
	int idx;
	int level;

	if (TREE_CODE (t) == TEMPLATE_TYPE_PARM)
	  {
	    idx = TEMPLATE_TYPE_IDX (t);
	    level = TEMPLATE_TYPE_LEVEL (t);
	  }
	else
	  {
	    idx = TEMPLATE_CONST_IDX (t);
	    level = TEMPLATE_CONST_LEVEL (t);
	  }

	if (TREE_VEC_LENGTH (args) > 0) 
	  {
	    tree arg = NULL_TREE;

	    if (TREE_CODE (TREE_VEC_ELT (args, 0)) == TREE_VEC)
	      {
		if (TREE_VEC_LENGTH (args) >= level - 1)
		  arg = TREE_VEC_ELT
		    (TREE_VEC_ELT (args, level - 1), idx);
	      }
	    else if (level == 1)
	      arg = TREE_VEC_ELT (args, idx);

	    if (arg != NULL_TREE)
	      {
		if (TREE_CODE (t) == TEMPLATE_TYPE_PARM)
		  return cp_build_type_variant
		    (arg, TYPE_READONLY (arg) || TYPE_READONLY (t),
		     TYPE_VOLATILE (arg) || TYPE_VOLATILE (t));
		else
		  return arg;
	      }
	  }

	/* If we get here, we must have been looking at a parm for a
	   more deeply nested template.  */
	my_friendly_assert((TREE_CODE (t) == TEMPLATE_CONST_PARM 
			    && TEMPLATE_CONST_LEVEL (t) > 1) 
			   || (TREE_CODE (t) == TEMPLATE_TYPE_PARM
			       && TEMPLATE_TYPE_LEVEL (t) > 1),
			   0);
	return t;
      }

    case TEMPLATE_DECL:
      {
	/* We can get here when processing a member template function
	   of a template class.  */
	tree tmpl;
	tree decl = DECL_TEMPLATE_RESULT (t);
	tree new_decl;
	tree parms;
	tree spec;
	int i;

	/* We might already have an instance of this template. */
	tree instances = DECL_TEMPLATE_INSTANTIATIONS (t);
	tree ctx = tsubst (DECL_CLASS_CONTEXT (t), args, nargs, in_decl); 
	  
	for (; instances; instances = TREE_CHAIN (instances))
	  if (DECL_CLASS_CONTEXT (TREE_VALUE (instances)) == ctx)
	    return TREE_VALUE (instances);

	/* Make a new template decl.  It will be similar to the
	   original, but will record the current template arguments. 
	   We also create a new function declaration, which is just
	   like the old one, but points to this new template, rather
	   than the old one.  */
	tmpl = copy_node (t);
	copy_lang_decl (tmpl);
	my_friendly_assert (DECL_LANG_SPECIFIC (tmpl) != 0, 0);
	DECL_CHAIN (tmpl) = NULL_TREE;
	TREE_CHAIN (tmpl) = NULL_TREE;
	DECL_TEMPLATE_INFO (tmpl) = build_tree_list (t, args);
	new_decl = tsubst (decl, args, nargs, in_decl);
	DECL_RESULT (tmpl) = new_decl;
	DECL_TI_TEMPLATE (new_decl) = tmpl;
	TREE_TYPE (tmpl) = TREE_TYPE (new_decl);
	DECL_TEMPLATE_INSTANTIATIONS (tmpl) = NULL_TREE;
	SET_DECL_IMPLICIT_INSTANTIATION (tmpl);

	/* The template parameters for this new template are all the
	   template parameters for the old template, except the
	   outermost level of parameters. */
	DECL_TEMPLATE_PARMS (tmpl)
	  = copy_node (DECL_TEMPLATE_PARMS (tmpl));
	for (parms = DECL_TEMPLATE_PARMS (tmpl);
	     TREE_CHAIN (parms) != NULL_TREE;
	     parms = TREE_CHAIN (parms))
	  TREE_CHAIN (parms) = copy_node (TREE_CHAIN (parms));

	/* Record this partial instantiation. */
	DECL_TEMPLATE_INSTANTIATIONS (t)
	  = perm_tree_cons (NULL_TREE, tmpl,
			    DECL_TEMPLATE_INSTANTIATIONS (t));

	DECL_TEMPLATE_SPECIALIZATIONS (tmpl) = NULL_TREE;
	return tmpl;
      }

    case FUNCTION_DECL:
      {
	tree r = NULL_TREE;
	tree ctx;

	int member;

	if (DECL_CONTEXT (t) != NULL_TREE
	    && TREE_CODE_CLASS (TREE_CODE (DECL_CONTEXT (t))) == 't')
	  {
	    if (DECL_NAME (t) == constructor_name (DECL_CONTEXT (t)))
	      member = 2;
	    else
	      member = 1;
	    ctx = tsubst (DECL_CLASS_CONTEXT (t), args, nargs, t);
	    type = tsubst (type, args, nargs, in_decl);
	  }
	else
	  {
	    member = 0;
	    ctx = NULL_TREE;
	    type = tsubst (type, args, nargs, in_decl);
	  }

	/* Do we already have this instantiation?  */
	if (DECL_TEMPLATE_INFO (t) != NULL_TREE)
	  {
	    tree tmpl = DECL_TI_TEMPLATE (t);
	    tree decls = DECL_TEMPLATE_INSTANTIATIONS (tmpl);

	    for (; decls; decls = TREE_CHAIN (decls))
	      if (TREE_TYPE (TREE_VALUE (decls)) == type
		  && DECL_CLASS_CONTEXT (TREE_VALUE (decls)) == ctx
		  && comp_template_args (TREE_PURPOSE (decls), args))
		return TREE_VALUE (decls);
	  }

	/* We do NOT check for matching decls pushed separately at this
           point, as they may not represent instantiations of this
           template, and in any case are considered separate under the
           discrete model.  Instead, see add_maybe_template.  */

	r = copy_node (t);
	copy_lang_decl (r);
	TREE_TYPE (r) = type;

	DECL_CONTEXT (r)
	  = tsubst (DECL_CONTEXT (t), args, nargs, t);
	DECL_CLASS_CONTEXT (r) = ctx;

	if (member && !strncmp (OPERATOR_TYPENAME_FORMAT,
				IDENTIFIER_POINTER (DECL_NAME (r)),
				sizeof (OPERATOR_TYPENAME_FORMAT) - 1))
	  {
	    /* Type-conversion operator.  Reconstruct the name, in
	       case it's the name of one of the template's parameters.  */
	    DECL_NAME (r) = build_typename_overload (TREE_TYPE (type));
	  }

	if (DESTRUCTOR_NAME_P (DECL_ASSEMBLER_NAME (t)))
	  {
	    char *buf, *dbuf = build_overload_name (ctx, 1, 1);
	    int len = sizeof (DESTRUCTOR_DECL_PREFIX) - 1;
	    buf = (char *) alloca (strlen (dbuf)
				   + sizeof (DESTRUCTOR_DECL_PREFIX));
	    bcopy (DESTRUCTOR_DECL_PREFIX, buf, len);
	    buf[len] = '\0';
	    strcat (buf, dbuf);
	    DECL_ASSEMBLER_NAME (r) = get_identifier (buf);
	  }
	else 
	  {
	    /* Instantiations of template functions must be mangled
	       specially, in order to conform to 14.5.5.1
	       [temp.over.link].  We use in_decl below rather than
	       DECL_TI_TEMPLATE (r) because the latter is set to
	       NULL_TREE in instantiate_decl.  */
	    tree tmpl;
	    tree arg_types;

	    if (DECL_TEMPLATE_INFO (r))
	      tmpl = DECL_TI_TEMPLATE (r);
	    else
	      tmpl = in_decl;

	    /* tmpl will be NULL if this is a specialization of a
	       member template of a template class.  */
	    if (name_mangling_version < 1
		|| tmpl == NULL_TREE
		|| (member && !is_member_template (tmpl)
		    && !DECL_TEMPLATE_INFO (tmpl)))
	      {
		arg_types = TYPE_ARG_TYPES (type);
		if (member && TREE_CODE (type) == FUNCTION_TYPE)
		  arg_types = hash_tree_chain 
		    (build_pointer_type (DECL_CONTEXT (r)),
		     arg_types); 
		
		DECL_ASSEMBLER_NAME (r) 
		  = build_decl_overload (DECL_NAME (r), arg_types,
					 member);
	      }
	    else
	      {
		/* We pass the outermost template parameters to
		   build_template_decl_overload since the innermost
		   template parameters are still just template
		   parameters; there are no corresponding substitution
		   arguments.  */
		/* FIXME The messed up thing here is that we get here with
		   full args and only one level of parms.  This is necessary
		   because when we partially instantiate a member template,
		   even though there's really only one level of parms left
		   we re-use the parms from the original template, which
		   have level 2.  When this is fixed we can remove the
		   add_to_template_args from instantiate_template.  */
		tree tparms = DECL_TEMPLATE_PARMS (tmpl);

		while (tparms && TREE_CHAIN (tparms) != NULL_TREE)
		  tparms = TREE_CHAIN (tparms);

		my_friendly_assert (tparms != NULL_TREE
				    && TREE_CODE (tparms) == TREE_LIST,
				    0);
		tparms = TREE_VALUE (tparms);

		arg_types = TYPE_ARG_TYPES (TREE_TYPE (tmpl));
		if (member && TREE_CODE (type) == FUNCTION_TYPE)
		  arg_types = hash_tree_chain 
		    (build_pointer_type (DECL_CONTEXT (r)),
		     arg_types); 

		DECL_ASSEMBLER_NAME (r)
		  = build_template_decl_overload 
		  (DECL_NAME (r), arg_types, 
		   TREE_TYPE (TREE_TYPE (tmpl)),
		   tparms,
		   TREE_CODE (TREE_VEC_ELT (args, 0)) == TREE_VEC 
		   ? TREE_VEC_ELT (args, TREE_VEC_LENGTH (args) - 1) :
		   args, 
		   member);
	      }
	  }
	DECL_RTL (r) = 0;
	make_decl_rtl (r, NULL_PTR, 1);

	DECL_ARGUMENTS (r) = tsubst (DECL_ARGUMENTS (t), args, nargs, t);
	DECL_MAIN_VARIANT (r) = r;
	DECL_RESULT (r) = NULL_TREE;
	DECL_INITIAL (r) = NULL_TREE;

	TREE_STATIC (r) = 0;
	TREE_PUBLIC (r) = 1;
	DECL_EXTERNAL (r) = 1;
	DECL_INTERFACE_KNOWN (r) = 0;
	DECL_DEFER_OUTPUT (r) = 0;
	TREE_CHAIN (r) = NULL_TREE;
	DECL_CHAIN (r) = NULL_TREE;

	if (IDENTIFIER_OPNAME_P (DECL_NAME (r)))
	  grok_op_properties (r, DECL_VIRTUAL_P (r), DECL_FRIEND_P (r));

	/* Look for matching decls for the moment.  */
	if (! member && ! flag_ansi_overloading)
	  {
	    tree decls = lookup_name_nonclass (DECL_NAME (t));
	    tree d = NULL_TREE;
    
	    if (decls == NULL_TREE)
	      /* no match */;
	    else if (is_overloaded_fn (decls))
	      for (decls = get_first_fn (decls); decls;
		   decls = DECL_CHAIN (decls))
		{
		  if (TREE_CODE (decls) == FUNCTION_DECL
		      && TREE_TYPE (decls) == type)
		    {
		      d = decls;
		      break;
		    }
		}

	    if (d)
	      {
		int dcl_only = ! DECL_INITIAL (d);
		if (dcl_only)
		  DECL_INITIAL (r) = error_mark_node;
		duplicate_decls (r, d);
		r = d;
		if (dcl_only)
		  DECL_INITIAL (r) = 0;
	      }
	  }

	if (DECL_TEMPLATE_INFO (t) != NULL_TREE)
	  {
	    tree tmpl = DECL_TI_TEMPLATE (t);
	    tree *declsp = &DECL_TEMPLATE_INSTANTIATIONS (tmpl);
	    tree argvec = tsubst (DECL_TI_ARGS (t), args, nargs, in_decl);

	    if (DECL_TEMPLATE_INFO (tmpl) && DECL_TI_ARGS (tmpl))
	      argvec = add_to_template_args (DECL_TI_ARGS (tmpl), argvec);

	    DECL_TEMPLATE_INFO (r) = perm_tree_cons (tmpl, argvec, NULL_TREE);
	    *declsp = perm_tree_cons (argvec, r, *declsp);

	    /* If we have a preexisting version of this function, don't expand
	       the template version, use the other instead.  */
	    if (TREE_STATIC (r) || DECL_TEMPLATE_SPECIALIZATION (r))
	      SET_DECL_TEMPLATE_SPECIALIZATION (r);
	    else
	      SET_DECL_IMPLICIT_INSTANTIATION (r);

	    DECL_TEMPLATE_INSTANTIATIONS (tmpl)
	      = tree_cons (argvec, r, DECL_TEMPLATE_INSTANTIATIONS (tmpl));
	  }

	/* Like grokfndecl.  If we don't do this, pushdecl will mess up our
	   TREE_CHAIN because it doesn't find a previous decl.  Sigh.  */
	if (member
	    && IDENTIFIER_GLOBAL_VALUE (DECL_ASSEMBLER_NAME (r)) == NULL_TREE)
	  IDENTIFIER_GLOBAL_VALUE (DECL_ASSEMBLER_NAME (r)) = r;

	return r;
      }

    case PARM_DECL:
      {
	tree r = copy_node (t);
	TREE_TYPE (r) = type;
	DECL_INITIAL (r) = TREE_TYPE (r);
	DECL_CONTEXT (r) = NULL_TREE;
#ifdef PROMOTE_PROTOTYPES
	if ((TREE_CODE (type) == INTEGER_TYPE
	     || TREE_CODE (type) == ENUMERAL_TYPE)
	    && TYPE_PRECISION (type) < TYPE_PRECISION (integer_type_node))
	  DECL_ARG_TYPE (r) = integer_type_node;
#endif
	if (TREE_CHAIN (t))
	  TREE_CHAIN (r) = tsubst (TREE_CHAIN (t), args, nargs, TREE_CHAIN (t));
	return r;
      }

    case FIELD_DECL:
      {
	tree r = copy_node (t);
	TREE_TYPE (r) = type;
	copy_lang_decl (r);
#if 0
	DECL_FIELD_CONTEXT (r) = tsubst (DECL_FIELD_CONTEXT (t), args, nargs, in_decl);
#endif
	DECL_INITIAL (r) = tsubst_expr (DECL_INITIAL (t), args, nargs, in_decl);
	TREE_CHAIN (r) = NULL_TREE;
	return r;
      }

    case USING_DECL:
      {
	tree r = copy_node (t);
	DECL_INITIAL (r)
	  = tsubst_copy (DECL_INITIAL (t), args, nargs, in_decl);
	TREE_CHAIN (r) = NULL_TREE;
	return r;
      }

    case VAR_DECL:
      {
	tree r;
	tree ctx = tsubst_copy (DECL_CONTEXT (t), args, nargs, in_decl);

	/* Do we already have this instantiation?  */
	if (DECL_LANG_SPECIFIC (t) && DECL_TEMPLATE_INFO (t))
	  {
	    tree tmpl = DECL_TI_TEMPLATE (t);
	    tree decls = DECL_TEMPLATE_INSTANTIATIONS (tmpl);

	    for (; decls; decls = TREE_CHAIN (decls))
	      if (DECL_CONTEXT (TREE_VALUE (decls)) == ctx)
		return TREE_VALUE (decls);
	  }

	r = copy_node (t);
	TREE_TYPE (r) = type;
	DECL_CONTEXT (r) = ctx;
	if (TREE_STATIC (r))
	  DECL_ASSEMBLER_NAME (r)
	    = build_static_name (DECL_CONTEXT (r), DECL_NAME (r));

	/* Don't try to expand the initializer until someone tries to use
	   this variable; otherwise we run into circular dependencies.  */
	DECL_INITIAL (r) = NULL_TREE;

	DECL_RTL (r) = 0;
	DECL_SIZE (r) = 0;

	if (DECL_LANG_SPECIFIC (r))
	  {
	    copy_lang_decl (r);
	    DECL_CLASS_CONTEXT (r) = DECL_CONTEXT (r);
	  }

	if (DECL_LANG_SPECIFIC (t) && DECL_TEMPLATE_INFO (t))
	  {
	    tree tmpl = DECL_TI_TEMPLATE (t);
	    tree *declsp = &DECL_TEMPLATE_INSTANTIATIONS (tmpl);
	    tree argvec = tsubst (DECL_TI_ARGS (t), args, nargs, in_decl);

	    DECL_TEMPLATE_INFO (r) = perm_tree_cons (tmpl, argvec, NULL_TREE);
	    *declsp = perm_tree_cons (argvec, r, *declsp);
	    SET_DECL_IMPLICIT_INSTANTIATION (r);
	  }
	TREE_CHAIN (r) = NULL_TREE;
	return r;
      }

    case TYPE_DECL:
      if (t == TYPE_NAME (TREE_TYPE (t)))
	return TYPE_NAME (type);

      {
	tree r = copy_node (t);
	TREE_TYPE (r) = type;
	DECL_CONTEXT (r) = current_class_type;
	TREE_CHAIN (r) = NULL_TREE;
	return r;
      }	  

    case TREE_LIST:
      {
	tree purpose, value, chain, result;
	int via_public, via_virtual, via_protected;

	if (t == void_list_node)
	  return t;

	via_public = TREE_VIA_PUBLIC (t);
	via_protected = TREE_VIA_PROTECTED (t);
	via_virtual = TREE_VIA_VIRTUAL (t);

	purpose = TREE_PURPOSE (t);
	if (purpose)
	  purpose = tsubst (purpose, args, nargs, in_decl);
	value = TREE_VALUE (t);
	if (value)
	  value = tsubst (value, args, nargs, in_decl);
	chain = TREE_CHAIN (t);
	if (chain && chain != void_type_node)
	  chain = tsubst (chain, args, nargs, in_decl);
	if (purpose == TREE_PURPOSE (t)
	    && value == TREE_VALUE (t)
	    && chain == TREE_CHAIN (t))
	  return t;
	result = hash_tree_cons (via_public, via_virtual, via_protected,
				 purpose, value, chain);
	TREE_PARMLIST (result) = TREE_PARMLIST (t);
	return result;
      }
    case TREE_VEC:
      if (type != NULL_TREE)
	{
	  /* A binfo node.  */

	  t = copy_node (t);

	  if (type == TREE_TYPE (t))
	    return t;

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
      {
	int len = TREE_VEC_LENGTH (t), need_new = 0, i;
	tree *elts = (tree *) alloca (len * sizeof (tree));

	bzero ((char *) elts, len * sizeof (tree));

	for (i = 0; i < len; i++)
	  {
	    elts[i] = maybe_fold_nontype_arg
	      (tsubst_expr (TREE_VEC_ELT (t, i), args, nargs, in_decl));

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
    case POINTER_TYPE:
    case REFERENCE_TYPE:
      {
	tree r;
	enum tree_code code;
	if (type == TREE_TYPE (t))
	  return t;

	code = TREE_CODE (t);
	if (code == POINTER_TYPE)
	  r = build_pointer_type (type);
	else
	  r = build_reference_type (type);
	r = cp_build_type_variant (r, TYPE_READONLY (t), TYPE_VOLATILE (t));
	/* Will this ever be needed for TYPE_..._TO values?  */
	layout_type (r);
	return r;
      }
    case OFFSET_TYPE:
      return build_offset_type
	(tsubst (TYPE_OFFSET_BASETYPE (t), args, nargs, in_decl), type);
    case FUNCTION_TYPE:
    case METHOD_TYPE:
      {
	tree values = TYPE_ARG_TYPES (t);
	tree context = TYPE_CONTEXT (t);
	tree raises = TYPE_RAISES_EXCEPTIONS (t);
	tree fntype;

	/* Don't bother recursing if we know it won't change anything.	*/
	if (values != void_list_node)
	  {
	    /* This should probably be rewritten to use hash_tree_cons for
               the memory savings.  */
	    tree first = NULL_TREE;
	    tree last;

	    for (; values && values != void_list_node;
		 values = TREE_CHAIN (values))
	      {
		tree value = TYPE_MAIN_VARIANT (type_decays_to
		  (tsubst (TREE_VALUE (values), args, nargs, in_decl)));
		/* Don't instantiate default args unless they are used.
		   Handle it in build_over_call instead.  */
		tree purpose = TREE_PURPOSE (values);
		tree x = build_tree_list (purpose, value);

		if (first)
		  TREE_CHAIN (last) = x;
		else
		  first = x;
		last = x;
	      }

	    if (values == void_list_node)
	      TREE_CHAIN (last) = void_list_node;

	    values = first;
	  }
	if (context)
	  context = tsubst (context, args, nargs, in_decl);
	/* Could also optimize cases where return value and
	   values have common elements (e.g., T min(const &T, const T&).  */

	/* If the above parameters haven't changed, just return the type.  */
	if (type == TREE_TYPE (t)
	    && values == TYPE_VALUES (t)
	    && context == TYPE_CONTEXT (t))
	  return t;

	/* Construct a new type node and return it.  */
	if (TREE_CODE (t) == FUNCTION_TYPE
	    && context == NULL_TREE)
	  {
	    fntype = build_function_type (type, values);
	  }
	else if (context == NULL_TREE)
	  {
	    tree base = tsubst (TREE_TYPE (TREE_VALUE (TYPE_ARG_TYPES (t))),
				args, nargs, in_decl);
	    fntype = build_cplus_method_type (base, type,
					      TREE_CHAIN (values));
	  }
	else
	  {
	    fntype = make_node (TREE_CODE (t));
	    TREE_TYPE (fntype) = type;
	    TYPE_CONTEXT (fntype) = context;
	    TYPE_VALUES (fntype) = values;
	    TYPE_SIZE (fntype) = TYPE_SIZE (t);
	    TYPE_ALIGN (fntype) = TYPE_ALIGN (t);
	    TYPE_MODE (fntype) = TYPE_MODE (t);
	    if (TYPE_METHOD_BASETYPE (t))
	      TYPE_METHOD_BASETYPE (fntype) = tsubst (TYPE_METHOD_BASETYPE (t),
						      args, nargs, in_decl);
	    /* Need to generate hash value.  */
	    my_friendly_abort (84);
	  }
	fntype = build_type_variant (fntype,
				     TYPE_READONLY (t),
				     TYPE_VOLATILE (t));
	if (raises)
	  {
	    raises = tsubst (raises, args, nargs, in_decl);
	    fntype = build_exception_variant (fntype, raises);
	  }
	return fntype;
      }
    case ARRAY_TYPE:
      {
	tree domain = tsubst (TYPE_DOMAIN (t), args, nargs, in_decl);
	tree r;
	if (type == TREE_TYPE (t) && domain == TYPE_DOMAIN (t))
	  return t;
	r = build_cplus_array_type (type, domain);
	return r;
      }

    case PLUS_EXPR:
    case MINUS_EXPR:
      return fold (build (TREE_CODE (t), TREE_TYPE (t),
			  tsubst (TREE_OPERAND (t, 0), args, nargs, in_decl),
			  tsubst (TREE_OPERAND (t, 1), args, nargs, in_decl)));

    case NEGATE_EXPR:
    case NOP_EXPR:
      return fold (build1 (TREE_CODE (t), TREE_TYPE (t),
			   tsubst (TREE_OPERAND (t, 0), args, nargs, in_decl)));

    case TYPENAME_TYPE:
      {
	tree ctx = tsubst (TYPE_CONTEXT (t), args, nargs, in_decl);
	tree f = make_typename_type (ctx, TYPE_IDENTIFIER (t));
	return cp_build_type_variant
	  (f, TYPE_READONLY (f) || TYPE_READONLY (t),
	   TYPE_VOLATILE (f) || TYPE_VOLATILE (t));
      }

    case INDIRECT_REF:
      return make_pointer_declarator
	(type, tsubst (TREE_OPERAND (t, 0), args, nargs, in_decl));
      
    case ADDR_EXPR:
      return make_reference_declarator
	(type, tsubst (TREE_OPERAND (t, 0), args, nargs, in_decl));

    case ARRAY_REF:
      return build_parse_node
	(ARRAY_REF, tsubst (TREE_OPERAND (t, 0), args, nargs, in_decl),
	 tsubst_expr (TREE_OPERAND (t, 1), args, nargs, in_decl));

    case CALL_EXPR:
      return make_call_declarator
	(tsubst (TREE_OPERAND (t, 0), args, nargs, in_decl),
	 tsubst (TREE_OPERAND (t, 1), args, nargs, in_decl),
	 TREE_OPERAND (t, 2),
	 tsubst (TREE_TYPE (t), args, nargs, in_decl));

    case SCOPE_REF:
      return build_parse_node
	(TREE_CODE (t), tsubst (TREE_OPERAND (t, 0), args, nargs, in_decl),
	 tsubst (TREE_OPERAND (t, 1), args, nargs, in_decl));

    default:
      sorry ("use of `%s' in template",
	     tree_code_name [(int) TREE_CODE (t)]);
      return error_mark_node;
    }
}

void
do_pushlevel ()
{
  emit_line_note (input_filename, lineno);
  pushlevel (0);
  clear_last_expr ();
  push_momentary ();
  expand_start_bindings (0);
}  

tree
do_poplevel ()
{
  tree t;
  int saved_warn_unused;

  if (processing_template_decl)
    {
      saved_warn_unused = warn_unused;
      warn_unused = 0;
    }
  expand_end_bindings (getdecls (), kept_level_p (), 0);
  if (processing_template_decl)
    warn_unused = saved_warn_unused;
  t = poplevel (kept_level_p (), 1, 0);
  pop_momentary ();
  return t;
}

/* Like tsubst, but deals with expressions.  This function just replaces
   template parms; to finish processing the resultant expression, use
   tsubst_expr.  */

tree
tsubst_copy (t, args, nargs, in_decl)
     tree t, args;
     int nargs;
     tree in_decl;
{
  enum tree_code code;

  if (t == NULL_TREE || t == error_mark_node)
    return t;

  code = TREE_CODE (t);

  switch (code)
    {
    case PARM_DECL:
      return do_identifier (DECL_NAME (t), 0);

    case CONST_DECL:
    case FIELD_DECL:
      if (DECL_CONTEXT (t))
	{
	  tree ctx = tsubst (DECL_CONTEXT (t), args, nargs, in_decl);
	  if (ctx == current_function_decl)
	    return lookup_name (DECL_NAME (t), 0);
	  else if (ctx != DECL_CONTEXT (t))
	    return lookup_field (ctx, DECL_NAME (t), 0, 0);
	}
      return t;

    case VAR_DECL:
    case FUNCTION_DECL:
      if (DECL_LANG_SPECIFIC (t) && DECL_TEMPLATE_INFO (t))
	t = tsubst (t, args, nargs, in_decl);
      mark_used (t);
      return t;

    case TEMPLATE_DECL:
      if (is_member_template (t))
	return tsubst (t, args, nargs, in_decl);
      else
	return t;

#if 0
    case IDENTIFIER_NODE:
      return do_identifier (t, 0);
#endif
      
    case CAST_EXPR:
    case REINTERPRET_CAST_EXPR:
    case CONST_CAST_EXPR:
    case STATIC_CAST_EXPR:
    case DYNAMIC_CAST_EXPR:
      return build1
	(code, tsubst (TREE_TYPE (t), args, nargs, in_decl),
	 tsubst_copy (TREE_OPERAND (t, 0), args, nargs, in_decl));

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
    case ARROW_EXPR:
    case THROW_EXPR:
    case TYPEID_EXPR:
      return build1
	(code, NULL_TREE,
	 tsubst_copy (TREE_OPERAND (t, 0), args, nargs, in_decl));

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
	(code, tsubst_copy (TREE_OPERAND (t, 0), args, nargs, in_decl),
	 tsubst_copy (TREE_OPERAND (t, 1), args, nargs, in_decl));

    case CALL_EXPR:
      {
	tree fn = TREE_OPERAND (t, 0);
	if (really_overloaded_fn (fn))
	  fn = tsubst_copy (get_first_fn (fn), args, nargs, in_decl);
	else
	  fn = tsubst_copy (fn, args, nargs, in_decl);
	return build_nt
	  (code, fn, tsubst_copy (TREE_OPERAND (t, 1), args, nargs, in_decl),
	   NULL_TREE);
      }

    case METHOD_CALL_EXPR:
      {
	tree name = TREE_OPERAND (t, 0);
	if (TREE_CODE (name) == BIT_NOT_EXPR)
	  {
	    name = tsubst_copy (TREE_OPERAND (name, 0), args, nargs, in_decl);
	    name = build1 (BIT_NOT_EXPR, NULL_TREE, TYPE_MAIN_VARIANT (name));
	  }
	else if (TREE_CODE (name) == SCOPE_REF
		 && TREE_CODE (TREE_OPERAND (name, 1)) == BIT_NOT_EXPR)
	  {
	    tree base = tsubst_copy (TREE_OPERAND (name, 0), args, nargs, in_decl);
	    name = TREE_OPERAND (name, 1);
	    name = tsubst_copy (TREE_OPERAND (name, 0), args, nargs, in_decl);
	    name = build1 (BIT_NOT_EXPR, NULL_TREE, TYPE_MAIN_VARIANT (name));
	    name = build_nt (SCOPE_REF, base, name);
	  }
	else
	  name = tsubst_copy (TREE_OPERAND (t, 0), args, nargs, in_decl);
	return build_nt
	  (code, name, tsubst_copy (TREE_OPERAND (t, 1), args, nargs, in_decl),
	   tsubst_copy (TREE_OPERAND (t, 2), args, nargs, in_decl),
	   NULL_TREE);
      }

    case COND_EXPR:
    case MODOP_EXPR:
      return build_nt
	(code, tsubst_copy (TREE_OPERAND (t, 0), args, nargs, in_decl),
	 tsubst_copy (TREE_OPERAND (t, 1), args, nargs, in_decl),
	 tsubst_copy (TREE_OPERAND (t, 2), args, nargs, in_decl));

    case NEW_EXPR:
      {
	tree r = build_nt
	(code, tsubst_copy (TREE_OPERAND (t, 0), args, nargs, in_decl),
	 tsubst_copy (TREE_OPERAND (t, 1), args, nargs, in_decl),
	 tsubst_copy (TREE_OPERAND (t, 2), args, nargs, in_decl));
	NEW_EXPR_USE_GLOBAL (r) = NEW_EXPR_USE_GLOBAL (t);
	return r;
      }

    case DELETE_EXPR:
      {
	tree r = build_nt
	(code, tsubst_copy (TREE_OPERAND (t, 0), args, nargs, in_decl),
	 tsubst_copy (TREE_OPERAND (t, 1), args, nargs, in_decl));
	DELETE_EXPR_USE_GLOBAL (r) = DELETE_EXPR_USE_GLOBAL (t);
	DELETE_EXPR_USE_VEC (r) = DELETE_EXPR_USE_VEC (t);
	return r;
      }

    case TEMPLATE_ID_EXPR:
      {
        /* Substituted template arguments */
	tree targs = tsubst_copy (TREE_OPERAND (t, 1), args, nargs, in_decl);
	tree chain;
	for (chain = targs; chain; chain = TREE_CHAIN (chain))
	  TREE_VALUE (chain) = maybe_fold_nontype_arg (TREE_VALUE (chain));

	return lookup_template_function
	  (tsubst_copy (TREE_OPERAND (t, 0), args, nargs, in_decl), targs);
      }

    case TREE_LIST:
      {
	tree purpose, value, chain;

	if (t == void_list_node)
	  return t;

	purpose = TREE_PURPOSE (t);
	if (purpose)
	  purpose = tsubst_copy (purpose, args, nargs, in_decl);
	value = TREE_VALUE (t);
	if (value)
	  value = tsubst_copy (value, args, nargs, in_decl);
	chain = TREE_CHAIN (t);
	if (chain && chain != void_type_node)
	  chain = tsubst_copy (chain, args, nargs, in_decl);
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
    case TEMPLATE_CONST_PARM:
    case POINTER_TYPE:
    case REFERENCE_TYPE:
    case OFFSET_TYPE:
    case FUNCTION_TYPE:
    case METHOD_TYPE:
    case ARRAY_TYPE:
    case TYPENAME_TYPE:
      return tsubst (t, args, nargs, in_decl);

    case IDENTIFIER_NODE:
      if (IDENTIFIER_TYPENAME_P (t))
	return build_typename_overload
	  (tsubst (TREE_TYPE (t), args, nargs, in_decl));
      else
	return t;

    case CONSTRUCTOR:
      return build
	(CONSTRUCTOR, tsubst (TREE_TYPE (t), args, nargs, in_decl), NULL_TREE,
	 tsubst_copy (CONSTRUCTOR_ELTS (t), args, nargs, in_decl));

    default:
      return t;
    }
}

/* Like tsubst_copy, but also does semantic processing and RTL expansion.  */

tree
tsubst_expr (t, args, nargs, in_decl)
     tree t, args;
     int nargs;
     tree in_decl;
{
  if (t == NULL_TREE || t == error_mark_node)
    return t;

  if (processing_template_decl)
    return tsubst_copy (t, args, nargs, in_decl);

  switch (TREE_CODE (t))
    {
    case RETURN_STMT:
      lineno = TREE_COMPLEXITY (t);
      emit_line_note (input_filename, lineno);
      c_expand_return
	(tsubst_expr (TREE_OPERAND (t, 0), args, nargs, in_decl));
      finish_stmt ();
      break;

    case EXPR_STMT:
      lineno = TREE_COMPLEXITY (t);
      emit_line_note (input_filename, lineno);
      t = tsubst_expr (TREE_OPERAND (t, 0), args, nargs, in_decl);
      /* Do default conversion if safe and possibly important,
	 in case within ({...}).  */
      if ((TREE_CODE (TREE_TYPE (t)) == ARRAY_TYPE && lvalue_p (t))
	  || TREE_CODE (TREE_TYPE (t)) == FUNCTION_TYPE)
	t = default_conversion (t);
      cplus_expand_expr_stmt (t);
      clear_momentary ();
      finish_stmt ();
      break;

    case DECL_STMT:
      {
	int i = suspend_momentary ();
	tree dcl, init;

	lineno = TREE_COMPLEXITY (t);
	emit_line_note (input_filename, lineno);
	dcl = start_decl
	  (tsubst (TREE_OPERAND (t, 0), args, nargs, in_decl),
	   tsubst (TREE_OPERAND (t, 1), args, nargs, in_decl),
	   TREE_OPERAND (t, 2) != 0);
	init = tsubst_expr (TREE_OPERAND (t, 2), args, nargs, in_decl);
	cp_finish_decl
	  (dcl, init, NULL_TREE, 1, /*init ? LOOKUP_ONLYCONVERTING :*/ 0);
	resume_momentary (i);
	return dcl;
      }

    case FOR_STMT:
      {
	tree tmp;
	int init_scope = (flag_new_for_scope > 0 && TREE_OPERAND (t, 0)
			  && TREE_CODE (TREE_OPERAND (t, 0)) == DECL_STMT);
	int cond_scope = (TREE_OPERAND (t, 1)
			  && TREE_CODE (TREE_OPERAND (t, 1)) == DECL_STMT);

	lineno = TREE_COMPLEXITY (t);
	emit_line_note (input_filename, lineno);
	if (init_scope)
	  do_pushlevel ();
	for (tmp = TREE_OPERAND (t, 0); tmp; tmp = TREE_CHAIN (tmp))
	  tsubst_expr (tmp, args, nargs, in_decl);
	emit_nop ();
	emit_line_note (input_filename, lineno);
	expand_start_loop_continue_elsewhere (1); 

	if (cond_scope)
	  do_pushlevel ();
	tmp = tsubst_expr (TREE_OPERAND (t, 1), args, nargs, in_decl);
	emit_line_note (input_filename, lineno);
	if (tmp)
	  expand_exit_loop_if_false (0, condition_conversion (tmp));

	if (! cond_scope)
	  do_pushlevel ();
	tsubst_expr (TREE_OPERAND (t, 3), args, nargs, in_decl);
	do_poplevel ();

	emit_line_note (input_filename, lineno);
	expand_loop_continue_here ();
	tmp = tsubst_expr (TREE_OPERAND (t, 2), args, nargs, in_decl);
	if (tmp)
	  cplus_expand_expr_stmt (tmp);

	expand_end_loop ();
	if (init_scope)
	  do_poplevel ();
	finish_stmt ();
      }
      break;

    case WHILE_STMT:
      {
	tree cond;

	lineno = TREE_COMPLEXITY (t);
	emit_nop ();
	emit_line_note (input_filename, lineno);
	expand_start_loop (1); 

	cond = TREE_OPERAND (t, 0);
	if (TREE_CODE (cond) == DECL_STMT)
	  do_pushlevel ();
	cond = tsubst_expr (cond, args, nargs, in_decl);
	emit_line_note (input_filename, lineno);
	expand_exit_loop_if_false (0, condition_conversion (cond));

	if (TREE_CODE (TREE_OPERAND (t, 0)) != DECL_STMT)
	  do_pushlevel ();
	tsubst_expr (TREE_OPERAND (t, 1), args, nargs, in_decl);
	do_poplevel ();

	expand_end_loop ();
	finish_stmt ();
      }
      break;

    case DO_STMT:
      {
	tree cond;

	lineno = TREE_COMPLEXITY (t);
	emit_nop ();
	emit_line_note (input_filename, lineno);
	expand_start_loop_continue_elsewhere (1); 

	tsubst_expr (TREE_OPERAND (t, 0), args, nargs, in_decl);
	expand_loop_continue_here ();

	cond = tsubst_expr (TREE_OPERAND (t, 1), args, nargs, in_decl);
	emit_line_note (input_filename, lineno);
	expand_exit_loop_if_false (0, condition_conversion (cond));
	expand_end_loop ();

	clear_momentary ();
	finish_stmt ();
      }
      break;

    case IF_STMT:
      {
	tree tmp;
	int cond_scope = (TREE_CODE (TREE_OPERAND (t, 0)) == DECL_STMT);

	lineno = TREE_COMPLEXITY (t);
	if (cond_scope)
	  do_pushlevel ();
	tmp = tsubst_expr (TREE_OPERAND (t, 0), args, nargs, in_decl);
	emit_line_note (input_filename, lineno);
	expand_start_cond (condition_conversion (tmp), 0);
	
	if (tmp = TREE_OPERAND (t, 1), tmp)
	  tsubst_expr (tmp, args, nargs, in_decl);

	if (tmp = TREE_OPERAND (t, 2), tmp)
	  {
	    expand_start_else ();
	    tsubst_expr (tmp, args, nargs, in_decl);
	  }

	expand_end_cond ();

	if (cond_scope)
	  do_poplevel ();

	finish_stmt ();
      }
      break;

    case COMPOUND_STMT:
      {
	tree substmt = TREE_OPERAND (t, 0);

	lineno = TREE_COMPLEXITY (t);

	if (COMPOUND_STMT_NO_SCOPE (t) == 0)
	  do_pushlevel ();

	for (; substmt; substmt = TREE_CHAIN (substmt))
	  tsubst_expr (substmt, args, nargs, in_decl);

	if (COMPOUND_STMT_NO_SCOPE (t) == 0)
	  do_poplevel ();
      }
      break;

    case BREAK_STMT:
      lineno = TREE_COMPLEXITY (t);
      emit_line_note (input_filename, lineno);
      if (! expand_exit_something ())
	error ("break statement not within loop or switch");
      break;

    case CONTINUE_STMT:
      lineno = TREE_COMPLEXITY (t);
      emit_line_note (input_filename, lineno);
      if (! expand_continue_loop (0))
	error ("continue statement not within a loop");
      break;

    case SWITCH_STMT:
      {
	tree val, tmp;
	int cond_scope = (TREE_CODE (TREE_OPERAND (t, 0)) == DECL_STMT);

	lineno = TREE_COMPLEXITY (t);
	if (cond_scope)
	  do_pushlevel ();
	val = tsubst_expr (TREE_OPERAND (t, 0), args, nargs, in_decl);
	emit_line_note (input_filename, lineno);
	c_expand_start_case (val);
	push_switch ();
	
	if (tmp = TREE_OPERAND (t, 1), tmp)
	  tsubst_expr (tmp, args, nargs, in_decl);

	expand_end_case (val);
	pop_switch ();

	if (cond_scope)
	  do_poplevel ();

	finish_stmt ();
      }
      break;

    case CASE_LABEL:
      do_case (tsubst_expr (TREE_OPERAND (t, 0), args, nargs, in_decl),
	       tsubst_expr (TREE_OPERAND (t, 1), args, nargs, in_decl));
      break;

    case LABEL_DECL:
      t = define_label (DECL_SOURCE_FILE (t), DECL_SOURCE_LINE (t),
			DECL_NAME (t));
      if (t)
	expand_label (t);
      break;

    case GOTO_STMT:
      lineno = TREE_COMPLEXITY (t);
      emit_line_note (input_filename, lineno);
      if (TREE_CODE (TREE_OPERAND (t, 0)) == IDENTIFIER_NODE)
	{
	  tree decl = lookup_label (TREE_OPERAND (t, 0));
	  TREE_USED (decl) = 1;
	  expand_goto (decl);
	}
      else
	expand_computed_goto
	  (tsubst_expr (TREE_OPERAND (t, 0), args, nargs, in_decl));
      break;

    case TRY_BLOCK:
      lineno = TREE_COMPLEXITY (t);
      emit_line_note (input_filename, lineno);
      expand_start_try_stmts ();
      tsubst_expr (TREE_OPERAND (t, 0), args, nargs, in_decl);
      expand_start_all_catch ();
      {
	tree handler = TREE_OPERAND (t, 1);
	for (; handler; handler = TREE_CHAIN (handler))
	  tsubst_expr (handler, args, nargs, in_decl);
      }
      expand_end_all_catch ();
      break;

    case HANDLER:
      lineno = TREE_COMPLEXITY (t);
      do_pushlevel ();
      if (TREE_OPERAND (t, 0))
	{
	  tree d = TREE_OPERAND (t, 0);
	  expand_start_catch_block
	    (tsubst (TREE_OPERAND (d, 1), args, nargs, in_decl),
	     tsubst (TREE_OPERAND (d, 0), args, nargs, in_decl));
	}
      else
	expand_start_catch_block (NULL_TREE, NULL_TREE);
      tsubst_expr (TREE_OPERAND (t, 1), args, nargs, in_decl);
      expand_end_catch_block ();
      do_poplevel ();
      break;

    case TAG_DEFN:
      lineno = TREE_COMPLEXITY (t);
      t = TREE_TYPE (t);
      if (TREE_CODE (t) == ENUMERAL_TYPE)
	tsubst_enum (t, args, nargs, NULL);
      break;

    default:
      return build_expr_from_tree (tsubst_copy (t, args, nargs, in_decl));
    }
  return NULL_TREE;
}

tree
instantiate_template (tmpl, targ_ptr)
     tree tmpl, targ_ptr;
{
  tree fndecl;
  int i, len;
  struct obstack *old_fmp_obstack;
  extern struct obstack *function_maybepermanent_obstack;

  my_friendly_assert (TREE_CODE (tmpl) == TEMPLATE_DECL, 283);

  if (DECL_FUNCTION_TEMPLATE_P (tmpl))
    {
      tree specs;
      
      /* Check to see if there is a matching specialization. */
      for (specs = DECL_TEMPLATE_SPECIALIZATIONS (tmpl);
	   specs != NULL_TREE;
	   specs = TREE_CHAIN (specs))
	if (comp_template_args (TREE_PURPOSE (specs), targ_ptr))
	  return TREE_VALUE (specs);
    }

  push_obstacks (&permanent_obstack, &permanent_obstack);
  old_fmp_obstack = function_maybepermanent_obstack;
  function_maybepermanent_obstack = &permanent_obstack;

  len = DECL_NTPARMS (tmpl);

  i = len;
  while (i--)
    {
      tree t = TREE_VEC_ELT (targ_ptr, i);
      if (TREE_CODE_CLASS (TREE_CODE (t)) == 't')
	{
	  tree nt = target_type (t);
	  if (IS_AGGR_TYPE (nt) && decl_function_context (TYPE_MAIN_DECL (nt)))
	    {
	      cp_error ("type `%T' composed from a local class is not a valid template-argument", t);
	      cp_error ("  trying to instantiate `%D'", tmpl);
	      fndecl = error_mark_node;
	      goto out;
	    }
	}
      TREE_VEC_ELT (targ_ptr, i) = copy_to_permanent (t);
    }
  targ_ptr = copy_to_permanent (targ_ptr);

  if (DECL_TEMPLATE_INFO (tmpl) && DECL_TI_ARGS (tmpl))
    targ_ptr = add_to_template_args (DECL_TI_ARGS (tmpl), targ_ptr);

  /* substitute template parameters */
  fndecl = tsubst (DECL_RESULT (tmpl), targ_ptr, len, tmpl);

  if (flag_external_templates)
    add_pending_template (fndecl);

 out:
  function_maybepermanent_obstack = old_fmp_obstack;
  pop_obstacks ();

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

/* Like type_unification but designed specially to handle conversion
   operators.  */

int
fn_type_unification (fn, explicit_targs, targs, args, return_type, strict)
     tree fn, explicit_targs, targs, args, return_type;
     int strict;
{
  int i, dummy = 0;
  tree fn_arg_types = TYPE_ARG_TYPES (TREE_TYPE (fn));
  tree decl_arg_types = args;

  my_friendly_assert (TREE_CODE (fn) == TEMPLATE_DECL, 0);

  if (IDENTIFIER_TYPENAME_P (DECL_NAME (fn))) 
    {
      /* This is a template conversion operator.  Use the return types
         as well as the argument types.  */
      fn_arg_types = scratch_tree_cons (NULL_TREE, 
				TREE_TYPE (TREE_TYPE (fn)),
				fn_arg_types);
      decl_arg_types = scratch_tree_cons (NULL_TREE,
				  return_type,
				  decl_arg_types);
    }

  i = type_unification (DECL_INNERMOST_TEMPLATE_PARMS (fn), 
			&TREE_VEC_ELT (targs, 0), 
			fn_arg_types,
			decl_arg_types,
			explicit_targs,
			&dummy, strict, 0);

  return i;
}


/* Type unification.

   We have a function template signature with one or more references to
   template parameters, and a parameter list we wish to fit to this
   template.  If possible, produce a list of parameters for the template
   which will cause it to fit the supplied parameter list.

   Return zero for success, 2 for an incomplete match that doesn't resolve
   all the types, and 1 for complete failure.  An error message will be
   printed only for an incomplete match.

   TPARMS[NTPARMS] is an array of template parameter types;
   TARGS[NTPARMS] is the array of template parameter values.  PARMS is
   the function template's signature (using TEMPLATE_PARM_IDX nodes),
   and ARGS is the argument list we're trying to match against it.

   If SUBR is 1, we're being called recursively (to unify the arguments of
   a function or method parameter of a function template), so don't zero
   out targs and don't fail on an incomplete match.

   If STRICT is 1, the match must be exact (for casts of overloaded
   addresses, explicit instantiation, and more_specialized).  */

int
type_unification (tparms, targs, parms, args, targs_in, nsubsts,
		  strict, allow_incomplete)
     tree tparms, *targs, parms, args, targs_in;
     int *nsubsts, strict, allow_incomplete;
{
  int ntparms = TREE_VEC_LENGTH (tparms);
  tree t;
  int i;
  int r;

  bzero ((char *) targs, sizeof (tree) * ntparms);

  /* Insert any explicit template arguments.  They are encoded as the
     operands of NOP_EXPRs so that unify can tell that they are
     explicit arguments.  */
  for (i = 0, t = targs_in; t != NULL_TREE; t = TREE_CHAIN (t), ++i)
    targs[i] = build1 (NOP_EXPR, NULL_TREE, TREE_VALUE (t));

  r = type_unification_real (tparms, targs, parms, args, nsubsts, 0,
			     strict, allow_incomplete); 

  for (i = 0, t = targs_in; t != NULL_TREE; t = TREE_CHAIN (t), ++i)
    if (TREE_CODE (targs[i]) == NOP_EXPR)
      targs[i] = TREE_OPERAND (targs[i], 0);

  return r;
}


static int
type_unification_real (tparms, targs, parms, args, nsubsts, subr,
		       strict, allow_incomplete)
     tree tparms, *targs, parms, args;
     int *nsubsts, subr, strict, allow_incomplete;
{
  tree parm, arg;
  int i;
  int ntparms = TREE_VEC_LENGTH (tparms);

  my_friendly_assert (TREE_CODE (tparms) == TREE_VEC, 289);
  my_friendly_assert (parms == NULL_TREE 
		      || TREE_CODE (parms) == TREE_LIST, 290);
  /* ARGS could be NULL (via a call from parse.y to
     build_x_function_call).  */
  if (args)
    my_friendly_assert (TREE_CODE (args) == TREE_LIST, 291);
  my_friendly_assert (ntparms > 0, 292);

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
	return 1;

      /* Conversions will be performed on a function argument that
	 corresponds with a function parameter that contains only
	 non-deducible template parameters and explicitly specified
	 template parameters.  */
      if (! uses_template_parms (parm))
	{
	  tree type;

	  if (TREE_CODE_CLASS (TREE_CODE (arg)) != 't')
	    type = TREE_TYPE (arg);
	  else
	    {
	      type = arg;
	      arg = NULL_TREE;
	    }

	  if (strict)
	    {
	      if (comptypes (parm, type, 1))
		continue;
	    }
	  else if (arg)
	    {
	      if (can_convert_arg (parm, type, arg))
		continue;
	    }
	  else
	    {
	      if (can_convert (parm, type))
		continue;
	    }

	  return 1;
	}
	
#if 0
      if (TREE_CODE (arg) == VAR_DECL)
	arg = TREE_TYPE (arg);
      else if (TREE_CODE_CLASS (TREE_CODE (arg)) == 'e')
	arg = TREE_TYPE (arg);
#else
      if (TREE_CODE_CLASS (TREE_CODE (arg)) != 't')
	{
	  my_friendly_assert (TREE_TYPE (arg) != NULL_TREE, 293);
	  if (TREE_CODE (arg) == TREE_LIST
	      && TREE_TYPE (arg) == unknown_type_node
	      && TREE_CODE (TREE_VALUE (arg)) == TEMPLATE_DECL)
	    {
	      int nsubsts, ntparms;
	      tree *targs;

	      /* Have to back unify here */
	      arg = TREE_VALUE (arg);
	      nsubsts = 0;
	      ntparms = DECL_NTPARMS (arg);
	      targs = (tree *) alloca (sizeof (tree) * ntparms);
	      parm = expr_tree_cons (NULL_TREE, parm, NULL_TREE);
	      return 
		type_unification (DECL_INNERMOST_TEMPLATE_PARMS (arg), 
				  targs,
				  TYPE_ARG_TYPES (TREE_TYPE (arg)),
				  parm, NULL_TREE, &nsubsts, strict,
				  allow_incomplete); 
	    }
	  arg = TREE_TYPE (arg);
	}
#endif
      if (! subr && TREE_CODE (arg) == REFERENCE_TYPE)
	arg = TREE_TYPE (arg);

      if (! subr && TREE_CODE (parm) != REFERENCE_TYPE)
	{
	  if (TREE_CODE (arg) == FUNCTION_TYPE
	      || TREE_CODE (arg) == METHOD_TYPE)
	    arg = build_pointer_type (arg);
	  else if (TREE_CODE (arg) == ARRAY_TYPE)
	    arg = build_pointer_type (TREE_TYPE (arg));
	  else
	    arg = TYPE_MAIN_VARIANT (arg);
	}

      switch (unify (tparms, targs, ntparms, parm, arg, nsubsts, strict))
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
      if (!targs[i])
	{
	  if (!allow_incomplete)
	    error ("incomplete type unification");
	  return 2;
	}
  return 0;
}

/* Tail recursion is your friend.  */

static int
unify (tparms, targs, ntparms, parm, arg, nsubsts, strict)
     tree tparms, *targs, parm, arg;
     int *nsubsts, ntparms, strict;
{
  int idx;

  /* I don't think this will do the right thing with respect to types.
     But the only case I've seen it in so far has been array bounds, where
     signedness is the only information lost, and I think that will be
     okay.  */
  while (TREE_CODE (parm) == NOP_EXPR)
    parm = TREE_OPERAND (parm, 0);

  if (arg == error_mark_node)
    return 1;
  if (arg == unknown_type_node)
    return 1;
  if (arg == parm)
    return 0;

  switch (TREE_CODE (parm))
    {
    case TYPENAME_TYPE:
      /* In a type which contains a nested-name-specifier, template
	 argument values cannot be deduced for template parameters used
	 within the nested-name-specifier.  */
      return 0;

    case TEMPLATE_TYPE_PARM:
      (*nsubsts)++;
      idx = TEMPLATE_TYPE_IDX (parm);
      /* Check for mixed types and values.  */
      if (TREE_CODE (TREE_VALUE (TREE_VEC_ELT (tparms, idx))) != TYPE_DECL)
	return 1;

      if (!strict && targs[idx] != NULL_TREE && 
	  TREE_CODE (targs[idx]) == NOP_EXPR)
	/* An explicit template argument.  Don't even try to match
	   here; the overload resolution code will manage check to
	   see whether the call is legal.  */ 
	return 0;

      if (strict && (TYPE_READONLY (arg) < TYPE_READONLY (parm)
		     || TYPE_VOLATILE (arg) < TYPE_VOLATILE (parm)))
	return 1;
#if 0
      /* Template type parameters cannot contain cv-quals; i.e.
         template <class T> void f (T& a, T& b) will not generate
	 void f (const int& a, const int& b).  */
      if (TYPE_READONLY (arg) > TYPE_READONLY (parm)
	  || TYPE_VOLATILE (arg) > TYPE_VOLATILE (parm))
	return 1;
      arg = TYPE_MAIN_VARIANT (arg);
#else
      {
	int constp = TYPE_READONLY (arg) > TYPE_READONLY (parm);
	int volatilep = TYPE_VOLATILE (arg) > TYPE_VOLATILE (parm);
	arg = cp_build_type_variant (arg, constp, volatilep);
      }
#endif
      /* Simple cases: Value already set, does match or doesn't.  */
      if (targs[idx] == arg 
	  || (targs[idx] 
	      && TREE_CODE (targs[idx]) == NOP_EXPR 
	      && TREE_OPERAND (targs[idx], 0) == arg))
	return 0;
      else if (targs[idx])
	return 1;
      targs[idx] = arg;
      return 0;
    case TEMPLATE_CONST_PARM:
      (*nsubsts)++;
      idx = TEMPLATE_CONST_IDX (parm);
      if (targs[idx])
	{
	  int i = cp_tree_equal (targs[idx], arg);
	  if (i == 1)
	    return 0;
	  else if (i == 0)
	    return 1;
	  else
	    my_friendly_abort (42);
	}

      targs[idx] = copy_to_permanent (arg);
      return 0;

    case POINTER_TYPE:
      if (TREE_CODE (arg) == RECORD_TYPE && TYPE_PTRMEMFUNC_FLAG (arg))
	return unify (tparms, targs, ntparms, parm,
		      TYPE_PTRMEMFUNC_FN_TYPE (arg), nsubsts, strict);

      if (TREE_CODE (arg) != POINTER_TYPE)
	return 1;
      return unify (tparms, targs, ntparms, TREE_TYPE (parm), TREE_TYPE (arg),
		    nsubsts, strict);

    case REFERENCE_TYPE:
      if (TREE_CODE (arg) == REFERENCE_TYPE)
	arg = TREE_TYPE (arg);
      return unify (tparms, targs, ntparms, TREE_TYPE (parm), arg,
		    nsubsts, strict);

    case ARRAY_TYPE:
      if (TREE_CODE (arg) != ARRAY_TYPE)
	return 1;
      if (unify (tparms, targs, ntparms, TYPE_DOMAIN (parm), TYPE_DOMAIN (arg),
		 nsubsts, strict) != 0)
	return 1;
      return unify (tparms, targs, ntparms, TREE_TYPE (parm), TREE_TYPE (arg),
		    nsubsts, strict);

    case REAL_TYPE:
    case COMPLEX_TYPE:
    case INTEGER_TYPE:
    case BOOLEAN_TYPE:
    case VOID_TYPE:
      if (TREE_CODE (arg) != TREE_CODE (parm))
	return 1;

      if (TREE_CODE (parm) == INTEGER_TYPE)
	{
	  if (TYPE_MIN_VALUE (parm) && TYPE_MIN_VALUE (arg)
	      && unify (tparms, targs, ntparms, TYPE_MIN_VALUE (parm),
			TYPE_MIN_VALUE (arg), nsubsts, strict))
	    return 1;
	  if (TYPE_MAX_VALUE (parm) && TYPE_MAX_VALUE (arg)
	      && unify (tparms, targs, ntparms, TYPE_MAX_VALUE (parm),
			TYPE_MAX_VALUE (arg), nsubsts, strict))
	    return 1;
	}
      else if (TREE_CODE (parm) == REAL_TYPE
	       && TYPE_MAIN_VARIANT (arg) != TYPE_MAIN_VARIANT (parm))
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

    case MINUS_EXPR:
      {
	tree t1, t2;
	t1 = TREE_OPERAND (parm, 0);
	t2 = TREE_OPERAND (parm, 1);
	return unify (tparms, targs, ntparms, t1,
		      fold (build (PLUS_EXPR, integer_type_node, arg, t2)),
		      nsubsts, strict);
      }

    case TREE_VEC:
      {
	int i;
	if (TREE_CODE (arg) != TREE_VEC)
	  return 1;
	if (TREE_VEC_LENGTH (parm) != TREE_VEC_LENGTH (arg))
	  return 1;
	for (i = TREE_VEC_LENGTH (parm) - 1; i >= 0; i--)
	  if (unify (tparms, targs, ntparms,
		     TREE_VEC_ELT (parm, i), TREE_VEC_ELT (arg, i),
		     nsubsts, strict))
	    return 1;
	return 0;
      }

    case RECORD_TYPE:
      if (TYPE_PTRMEMFUNC_FLAG (parm))
	return unify (tparms, targs, ntparms, TYPE_PTRMEMFUNC_FN_TYPE (parm),
		      arg, nsubsts, strict);

      /* Allow trivial conversions.  */
      if (TREE_CODE (arg) != RECORD_TYPE
	  || TYPE_READONLY (parm) < TYPE_READONLY (arg)
	  || TYPE_VOLATILE (parm) < TYPE_VOLATILE (arg))
	return 1;

      if (CLASSTYPE_TEMPLATE_INFO (parm) && uses_template_parms (parm))
	{
	  tree t = NULL_TREE;
	  if (flag_ansi_overloading && ! strict)
	    t = get_template_base (CLASSTYPE_TI_TEMPLATE (parm), arg);
	  else if
	    (CLASSTYPE_TEMPLATE_INFO (arg)
	     && CLASSTYPE_TI_TEMPLATE (parm) == CLASSTYPE_TI_TEMPLATE (arg))
	    t = arg;
	  if (! t || t == error_mark_node)
	    return 1;

	  return unify (tparms, targs, ntparms, CLASSTYPE_TI_ARGS (parm),
			CLASSTYPE_TI_ARGS (t), nsubsts, strict);
	}
      else if (TYPE_MAIN_VARIANT (parm) != TYPE_MAIN_VARIANT (arg))
	return 1;
      return 0;

    case METHOD_TYPE:
      if (TREE_CODE (arg) != METHOD_TYPE)
	return 1;
      goto check_args;

    case FUNCTION_TYPE:
      if (TREE_CODE (arg) != FUNCTION_TYPE)
	return 1;
     check_args:
      if (unify (tparms, targs, ntparms, TREE_TYPE (parm),
		 TREE_TYPE (arg), nsubsts, strict))
	return 1;
      return type_unification_real (tparms, targs, TYPE_ARG_TYPES (parm),
				    TYPE_ARG_TYPES (arg), nsubsts, 1, 
				    strict, 0);

    case OFFSET_TYPE:
      if (TREE_CODE (arg) != OFFSET_TYPE)
	return 1;
      if (unify (tparms, targs, ntparms, TYPE_OFFSET_BASETYPE (parm),
		 TYPE_OFFSET_BASETYPE (arg), nsubsts, strict))
	return 1;
      return unify (tparms, targs, ntparms, TREE_TYPE (parm),
		    TREE_TYPE (arg), nsubsts, strict);

    case CONST_DECL:
      if (arg != decl_constant_value (parm))
	return 1;
      return 0;

    default:
      sorry ("use of `%s' in template type unification",
	     tree_code_name [(int) TREE_CODE (parm)]);
      return 1;
    }
}

void
mark_decl_instantiated (result, extern_p)
     tree result;
     int extern_p;
{
  if (DECL_TEMPLATE_INSTANTIATION (result))
    SET_DECL_EXPLICIT_INSTANTIATION (result);
  TREE_PUBLIC (result) = 1;

  if (! extern_p)
    {
      DECL_INTERFACE_KNOWN (result) = 1;
      DECL_NOT_REALLY_EXTERN (result) = 1;

      /* For WIN32 we also want to put explicit instantiations in
	 linkonce sections.  */
      if (supports_one_only () && ! SUPPORTS_WEAK)
	comdat_linkage (result);
    }
  else if (TREE_CODE (result) == FUNCTION_DECL)
    mark_inline_for_output (result);
}

/* Given two function templates PAT1 and PAT2, return:

   1 if PAT1 is more specialized than PAT2 as described in [temp.func.order].
   -1 if PAT2 is more specialized than PAT1.
   0 if neither is more specialized.  */
   
int
more_specialized (pat1, pat2)
     tree pat1, pat2;
{
  tree targs;
  int winner = 0;

  targs = get_bindings (pat1, pat2);
  if (targs)
    {
      --winner;
    }

  targs = get_bindings (pat2, pat1);
  if (targs)
    {
      ++winner;
    }

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

  targs = get_class_bindings
    (TREE_VALUE (pat1), TREE_PURPOSE (pat1), TREE_PURPOSE (pat2));
  if (targs)
    --winner;

  targs = get_class_bindings
    (TREE_VALUE (pat2), TREE_PURPOSE (pat2), TREE_PURPOSE (pat1));
  if (targs)
    ++winner;

  return winner;
}

/* Return the template arguments that will produce the function signature
   DECL from the function template FN.  */

tree 
get_bindings (fn, decl)
     tree fn, decl;
{
  int ntparms = DECL_NTPARMS (fn);
  tree targs = make_scratch_vec (ntparms);
  int i;

  i = fn_type_unification (fn, NULL_TREE, targs, 
			   TYPE_ARG_TYPES (TREE_TYPE (decl)), 
			   TREE_TYPE (TREE_TYPE (decl)),
			   1);

  if (i == 0)
    return targs;
  return 0;
}

static tree
get_class_bindings (tparms, parms, args)
     tree tparms, parms, args;
{
  int i, dummy, ntparms = TREE_VEC_LENGTH (tparms);
  tree vec = make_temp_vec (ntparms);

  for (i = 0; i < TREE_VEC_LENGTH (parms); ++i)
    {
      switch (unify (tparms, &TREE_VEC_ELT (vec, 0), ntparms,
		     TREE_VEC_ELT (parms, i), TREE_VEC_ELT (args, i),
		     &dummy, 1))
	{
	case 0:
	  break;
	case 1:
	  return NULL_TREE;
	}
    }

  for (i =  0; i < ntparms; ++i)
    if (! TREE_VEC_ELT (vec, i))
      return NULL_TREE;

  return vec;
}

/* Return the most specialized of the list of templates in FNS that can
   produce an instantiation matching DECL.  */

tree
most_specialized (fns, decl)
     tree fns, decl;
{
  tree fn, champ, args, *p;
  int fate;

  for (p = &fns; *p; )
    {
      args = get_bindings (TREE_VALUE (*p), decl);
      if (args)
	{
	  p = &TREE_CHAIN (*p);
	}
      else
	*p = TREE_CHAIN (*p);
    }

  if (! fns)
    return NULL_TREE;

  fn = fns;
  champ = TREE_VALUE (fn);
  fn = TREE_CHAIN (fn);
  for (; fn; fn = TREE_CHAIN (fn))
    {
      fate = more_specialized (champ, TREE_VALUE (fn));
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
	  champ = TREE_VALUE (fn);
	}
    }

  for (fn = fns; fn && TREE_VALUE (fn) != champ; fn = TREE_CHAIN (fn))
    {
      fate = more_specialized (champ, TREE_VALUE (fn));
      if (fate != 1)
	return error_mark_node;
    }

  return champ;
}

/* Return the most specialized of the class template specializations in
   SPECS that can produce an instantiation matching ARGS.  */

tree
most_specialized_class (specs, mainargs)
     tree specs, mainargs;
{
  tree list = NULL_TREE, t, args, champ;
  int fate;

  for (t = specs; t; t = TREE_CHAIN (t))
    {
      args = get_class_bindings (TREE_VALUE (t), TREE_PURPOSE (t), mainargs);
      if (args)
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
  tree name;
  tree fn;
  tree result = NULL_TREE;
  int extern_p = 0;
  tree templates = NULL_TREE;

  if (! DECL_LANG_SPECIFIC (decl))
    {
      cp_error ("explicit instantiation of non-template `%#D'", decl);
      return;
    }

  /* If we've already seen this template instance, use it.  */
  if (TREE_CODE (decl) == VAR_DECL)
    {
      result = lookup_field (DECL_CONTEXT (decl), DECL_NAME (decl), 0, 0);
      if (result && TREE_CODE (result) != VAR_DECL)
	result = NULL_TREE;
    }
  else if (TREE_CODE (decl) != FUNCTION_DECL)
    {
      cp_error ("explicit instantiation of `%#D'", decl);
      return;
    }
  else if (DECL_FUNCTION_MEMBER_P (decl))
    {
      if (DECL_TEMPLATE_INSTANTIATION (decl))
	result = decl;
      else if (name = DECL_ASSEMBLER_NAME (decl),
	       fn = IDENTIFIER_GLOBAL_VALUE (name),
	       fn && DECL_TEMPLATE_INSTANTIATION (fn))
	result = fn;
      else 
	{
	  /* Maybe this is an instantiation of a member template
	     function.  */
	  tree ctype = DECL_CONTEXT (decl);

	  name = DECL_NAME (decl);
	  fn = lookup_fnfields (TYPE_BINFO (ctype), name, 1);
	  if (fn)
	    fn = TREE_VALUE (fn);

	  for (; fn; fn = DECL_CHAIN (fn))
	    if (TREE_CODE (fn) == TEMPLATE_DECL)
	      templates = decl_tree_cons (NULL_TREE, fn, templates);
	}
    }
  else if (name = DECL_NAME (decl), fn = IDENTIFIER_GLOBAL_VALUE (name), fn)
    {
      for (fn = get_first_fn (fn); fn; fn = DECL_CHAIN (fn))
	if (TREE_CODE (fn) == TEMPLATE_DECL)
	  templates = decl_tree_cons (NULL_TREE, fn, templates);
    }

  if (templates && !result)
    {
      tree args;
      result = most_specialized (templates, decl);
      if (result == error_mark_node)
	{
	  char *str = "candidates are:";
	  cp_error ("ambiguous template instantiation for `%D' requested", decl);
	  for (fn = templates; fn; fn = TREE_CHAIN (fn))
	    {
	      cp_error_at ("%s %+#D", str, TREE_VALUE (fn));
	      str = "               ";
	    }
	  return;
	}
      else if (result)
	{
	  args = get_bindings (result, decl);
	  result = instantiate_template (result, args);
	}
    }

  if (! result)
    {
      cp_error ("no matching template for `%D' found", decl);
      return;
    }

  if (! DECL_TEMPLATE_INFO (result))
    {
      cp_pedwarn ("explicit instantiation of non-template `%#D'", result);
      return;
    }

  if (flag_external_templates)
    return;

  if (storage == NULL_TREE)
    ;
  else if (storage == ridpointers[(int) RID_EXTERN])
    extern_p = 1;
  else
    cp_error ("storage class `%D' applied to template instantiation",
	      storage);

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

  if (supports_one_only () && ! SUPPORTS_WEAK)
    /* For WIN32 we also want to put explicit instantiations in
       linkonce sections.  */;
  else
    {
      SET_CLASSTYPE_INTERFACE_KNOWN (t);
      CLASSTYPE_INTERFACE_ONLY (t) = extern_p;
    }

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

  if (! IS_AGGR_TYPE (t) || ! CLASSTYPE_TEMPLATE_INFO (t))
    {
      cp_error ("explicit instantiation of non-template type `%T'", t);
      return;
    }

  complete_type (t);

  /* With -fexternal-templates, explicit instantiations are treated the same
     as implicit ones.  */
  if (flag_external_templates)
    return;

  if (TYPE_SIZE (t) == NULL_TREE)
    {
      cp_error ("explicit instantiation of `%#T' before definition of template",
		t);
      return;
    }

  if (storage == NULL_TREE)
    /* OK */;
  else if (storage == ridpointers[(int) RID_INLINE])
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

  /* We've already instantiated this.  */
  if (CLASSTYPE_EXPLICIT_INSTANTIATION (t) && ! CLASSTYPE_INTERFACE_ONLY (t)
      && extern_p)
    return;

  if (! CLASSTYPE_TEMPLATE_SPECIALIZATION (t))
    {
      mark_class_instantiated (t, extern_p);
      repo_template_instantiated (t, extern_p);
    }

  if (nomem_p)
    return;

  {
    tree tmp;

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
      if (IS_AGGR_TYPE (TREE_VALUE (tmp)))
	do_type_instantiation (TYPE_MAIN_DECL (TREE_VALUE (tmp)), storage);
  }
}

tree
instantiate_decl (d)
     tree d;
{
  tree ti = DECL_TEMPLATE_INFO (d);
  tree tmpl = TI_TEMPLATE (ti);
  tree args = TI_ARGS (ti);
  tree td;
  tree decl_pattern, code_pattern;
  tree save_ti;
  int nested = in_function_p ();
  int d_defined;
  int pattern_defined;
  int line = lineno;
  char *file = input_filename;

  for (td = tmpl; DECL_TEMPLATE_INSTANTIATION (td); )
    td = DECL_TI_TEMPLATE (td);

  /* In the case of a member template, decl_pattern is the partially
     instantiated declaration (in the instantiated class), and code_pattern
     is the original template definition.  */
  decl_pattern = DECL_TEMPLATE_RESULT (tmpl);
  code_pattern = DECL_TEMPLATE_RESULT (td);

  if (TREE_CODE (d) == FUNCTION_DECL)
    {
      d_defined = (DECL_INITIAL (d) != NULL_TREE);
      pattern_defined = (DECL_INITIAL (code_pattern) != NULL_TREE);
    }
  else
    {
      d_defined = ! DECL_IN_AGGR_P (d);
      pattern_defined = ! DECL_IN_AGGR_P (code_pattern);
    }

  if (d_defined)
    return d;

  if (TREE_CODE (d) == FUNCTION_DECL) 
    {
      tree specs;

      /* Check to see if there is a matching specialization. */
      for (specs = DECL_TEMPLATE_SPECIALIZATIONS (tmpl);
	   specs != NULL_TREE;
	   specs = TREE_CHAIN (specs))
	if (comp_template_args (TREE_PURPOSE (specs), args))
	  return TREE_VALUE (specs);
    }

  /* This needs to happen before any tsubsting.  */
  if (! push_tinst_level (d))
    return d;

  push_to_top_level ();
  lineno = DECL_SOURCE_LINE (d);
  input_filename = DECL_SOURCE_FILE (d);

  /* We need to set up DECL_INITIAL regardless of pattern_defined if the
     variable is a static const initialized in the class body.  */
  if (TREE_CODE (d) == VAR_DECL
      && ! DECL_INITIAL (d) && DECL_INITIAL (code_pattern))
    {
      pushclass (DECL_CONTEXT (d), 2);
      DECL_INITIAL (d) = tsubst_expr (DECL_INITIAL (code_pattern), args,
				      TREE_VEC_LENGTH (args), tmpl);
      popclass (1);
    }

  /* import_export_decl has to happen after DECL_INITIAL is set up.  */
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

  /* Defer all templates except inline functions used in another function.  */
  if (! pattern_defined
      || (! (TREE_CODE (d) == FUNCTION_DECL && DECL_INLINE (d) && nested)
	  && ! at_eof))
    {
      add_pending_template (d);
      goto out;
    }

  lineno = DECL_SOURCE_LINE (d);
  input_filename = DECL_SOURCE_FILE (d);

  /* Trick tsubst into giving us a new decl in case the template changed.  */
  save_ti = DECL_TEMPLATE_INFO (decl_pattern);
  DECL_TEMPLATE_INFO (decl_pattern) = NULL_TREE;
  td = tsubst (decl_pattern, args, TREE_VEC_LENGTH (args), tmpl);
  SET_DECL_IMPLICIT_INSTANTIATION (td);
  DECL_TEMPLATE_INFO (decl_pattern) = save_ti;

  /* And set up DECL_INITIAL, since tsubst doesn't.  */
  if (TREE_CODE (td) == VAR_DECL)
    {
      pushclass (DECL_CONTEXT (d), 2);
      DECL_INITIAL (td) = tsubst_expr (DECL_INITIAL (code_pattern), args,
				       TREE_VEC_LENGTH (args), tmpl);
      popclass (1);
    }

  if (TREE_CODE (d) == FUNCTION_DECL)
    {
      /* Convince duplicate_decls to use the DECL_ARGUMENTS from the
	 new decl.  */ 
      DECL_INITIAL (td) = error_mark_node;

      if (DECL_TEMPLATE_SPECIALIZATION (td) && !DECL_TEMPLATE_INFO (td))
	/* Set up the information about what is being specialized. */
	DECL_TEMPLATE_INFO (td) = DECL_TEMPLATE_INFO (d);
    }
  duplicate_decls (td, d);
  if (TREE_CODE (d) == FUNCTION_DECL)
    DECL_INITIAL (td) = 0;

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
      cp_finish_decl (d, DECL_INITIAL (d), NULL_TREE, 0, 0);
    }
  else if (TREE_CODE (d) == FUNCTION_DECL)
    {
      tree t = DECL_SAVED_TREE (code_pattern);

      start_function (NULL_TREE, d, NULL_TREE, 1);
      store_parm_decls ();

      if (t && TREE_CODE (t) == RETURN_INIT)
	{
	  store_return_init
	    (TREE_OPERAND (t, 0),
	     tsubst_expr (TREE_OPERAND (t, 1), args,
			  TREE_VEC_LENGTH (args), tmpl));
	  t = TREE_CHAIN (t);
	}

      if (t && TREE_CODE (t) == CTOR_INITIALIZER)
	{
	  current_member_init_list
	    = tsubst_expr_values (TREE_OPERAND (t, 0), args);
	  current_base_init_list
	    = tsubst_expr_values (TREE_OPERAND (t, 1), args);
	  t = TREE_CHAIN (t);
	}

      setup_vtbl_ptr ();
      /* Always keep the BLOCK node associated with the outermost
	 pair of curly braces of a function.  These are needed
	 for correct operation of dwarfout.c.  */
      keep_next_level ();

      my_friendly_assert (TREE_CODE (t) == COMPOUND_STMT, 42);
      tsubst_expr (t, args, TREE_VEC_LENGTH (args), tmpl);

      finish_function (lineno, 0, nested);
    }

out:
  lineno = line;
  input_filename = file;

  pop_from_top_level ();
  pop_tinst_level ();

  return d;
}

tree
tsubst_chain (t, argvec)
     tree t, argvec;
{
  if (t)
    {
      tree first = tsubst (t, argvec,
			   TREE_VEC_LENGTH (argvec), NULL_TREE);
      tree last = first;

      for (t = TREE_CHAIN (t); t; t = TREE_CHAIN (t))
	{
	  tree x = tsubst (t, argvec, TREE_VEC_LENGTH (argvec), NULL_TREE);
	  TREE_CHAIN (last) = x;
	  last = x;
	}

      return first;
    }
  return NULL_TREE;
}

static tree
tsubst_expr_values (t, argvec)
     tree t, argvec;
{
  tree first = NULL_TREE;
  tree *p = &first;

  for (; t; t = TREE_CHAIN (t))
    {
      tree pur = tsubst_copy (TREE_PURPOSE (t), argvec,
			      TREE_VEC_LENGTH (argvec), NULL_TREE);
      tree val = tsubst_expr (TREE_VALUE (t), argvec,
			      TREE_VEC_LENGTH (argvec), NULL_TREE);
      *p = build_tree_list (pur, val);
      p = &TREE_CHAIN (*p);
    }
  return first;
}

tree last_tree;

void
add_tree (t)
     tree t;
{
  last_tree = TREE_CHAIN (last_tree) = t;
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

  t = most_specialized (fns, d);
  if (! t)
    return;
  if (t == error_mark_node)
    {
      cp_error ("ambiguous template instantiation for `%D'", d);
      return;
    }

  *maybe_template_tail = perm_tree_cons (t, d, NULL_TREE);
  maybe_template_tail = &TREE_CHAIN (*maybe_template_tail);
  DECL_MAYBE_TEMPLATE (d) = 1;
}

/* Instantiate an enumerated type.  Used by instantiate_class_template and
   tsubst_expr.  */

static tree
tsubst_enum (tag, args, nargs, field_chain)
     tree tag, args;
     int nargs;
     tree * field_chain;
{
  extern tree current_local_enum;
  tree prev_local_enum = current_local_enum;

  tree newtag = start_enum (TYPE_IDENTIFIER (tag));
  tree e, values = NULL_TREE;

  for (e = TYPE_VALUES (tag); e; e = TREE_CHAIN (e))
    {
      tree elt = build_enumerator (TREE_PURPOSE (e),
				   tsubst_expr (TREE_VALUE (e), args,
						nargs, NULL_TREE));
      TREE_CHAIN (elt) = values;
      values = elt;
    }

  finish_enum (newtag, values);

  if (NULL != field_chain)
    *field_chain = grok_enum_decls (newtag, NULL_TREE);

  current_local_enum = prev_local_enum;

  return newtag;
}
