/* Help friends in C++.
   Copyright (C) 1997, 1998, 1999, 2000, 2001 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "tree.h"
#include "rtl.h"
#include "expr.h"
#include "cp-tree.h"
#include "flags.h"
#include "output.h"
#include "toplev.h"

/* Friend data structures are described in cp-tree.h.  */

/* Returns nonzero if SUPPLICANT is a friend of TYPE.  */

int
is_friend (type, supplicant)
     tree type, supplicant;
{
  int declp;
  register tree list;
  tree context;

  if (supplicant == NULL_TREE || type == NULL_TREE)
    return 0;

  declp = DECL_P (supplicant);

  if (declp)
    /* It's a function decl.  */
    {
      tree list = DECL_FRIENDLIST (TYPE_MAIN_DECL (type));
      tree name = DECL_NAME (supplicant);

      for (; list ; list = TREE_CHAIN (list))
	{
	  if (name == FRIEND_NAME (list))
	    {
	      tree friends = FRIEND_DECLS (list);
	      for (; friends ; friends = TREE_CHAIN (friends))
		{
		  if (TREE_VALUE (friends) == NULL_TREE)
		    continue;

		  if (supplicant == TREE_VALUE (friends))
		    return 1;

		  /* We haven't completed the instantiation yet.  */
		  if (TREE_CODE (supplicant) == TEMPLATE_DECL)
		    return 1;

		  /* Temporarily, we are more lenient to deal with
		     nested friend functions, for which there can be
		     more than one FUNCTION_DECL, despite being the
		     same function.  When that's fixed, this bit can
		     go.  */
		  if (DECL_FUNCTION_MEMBER_P (supplicant)
		      && same_type_p (TREE_TYPE (supplicant),
				      TREE_TYPE (TREE_VALUE (friends))))
		    return 1;

		  if (TREE_CODE (TREE_VALUE (friends)) == TEMPLATE_DECL
		      && is_specialization_of (supplicant, 
					       TREE_VALUE (friends)))
		    return 1;
		}
	      break;
	    }
	}
    }
  else
    /* It's a type.  */
    {
      /* Nested classes are implicitly friends of their enclosing types, as
	 per core issue 45 (this is a change from the standard).  */
      for (context = supplicant;
	   context && TYPE_P (context);
	   context = TYPE_CONTEXT (context))
	if (type == context)
	  return 1;
      
      list = CLASSTYPE_FRIEND_CLASSES (TREE_TYPE (TYPE_MAIN_DECL (type)));
      for (; list ; list = TREE_CHAIN (list))
	{
	  tree t = TREE_VALUE (list);

	  if (TREE_CODE (t) == TEMPLATE_DECL ? 
	      is_specialization_of (TYPE_MAIN_DECL (supplicant), t) :
	      same_type_p (supplicant, t))
	    return 1;
	}
    }      

  if (declp && DECL_FUNCTION_MEMBER_P (supplicant))
    context = DECL_CONTEXT (supplicant);
  else if (! declp)
    /* Local classes have the same access as the enclosing function.  */
    context = decl_function_context (TYPE_MAIN_DECL (supplicant));
  else
    context = NULL_TREE;

  /* A namespace is not friend to anybody.  */
  if (context && TREE_CODE (context) == NAMESPACE_DECL)
    context = NULL_TREE;

  if (context)
    return is_friend (type, context);

  return 0;
}

/* Add a new friend to the friends of the aggregate type TYPE.
   DECL is the FUNCTION_DECL of the friend being added.  */

void
add_friend (type, decl)
     tree type, decl;
{
  tree typedecl;
  tree list;
  tree name;

  if (decl == error_mark_node)
    return;

  typedecl = TYPE_MAIN_DECL (type);
  list = DECL_FRIENDLIST (typedecl);
  name = DECL_NAME (decl);
  type = TREE_TYPE (typedecl);

  while (list)
    {
      if (name == FRIEND_NAME (list))
	{
	  tree friends = FRIEND_DECLS (list);
	  for (; friends ; friends = TREE_CHAIN (friends))
	    {
	      if (decl == TREE_VALUE (friends))
		{
		  warning ("`%D' is already a friend of class `%T'",
			      decl, type);
		  cp_warning_at ("previous friend declaration of `%D'",
				 TREE_VALUE (friends));
		  return;
		}
	    }

	  maybe_add_class_template_decl_list (type, decl, /*friend_p=*/1);

	  TREE_VALUE (list) = tree_cons (error_mark_node, decl,
					 TREE_VALUE (list));
	  return;
	}
      list = TREE_CHAIN (list);
    }

  maybe_add_class_template_decl_list (type, decl, /*friend_p=*/1);

  DECL_FRIENDLIST (typedecl)
    = tree_cons (DECL_NAME (decl), build_tree_list (error_mark_node, decl),
		 DECL_FRIENDLIST (typedecl));
  if (!uses_template_parms (type))
    DECL_BEFRIENDING_CLASSES (decl) 
      = tree_cons (NULL_TREE, type,
		   DECL_BEFRIENDING_CLASSES (decl));
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
  int is_template_friend;

  if (! IS_AGGR_TYPE (friend_type))
    {
      error ("invalid type `%T' declared `friend'", friend_type);
      return;
    }

  if (processing_template_decl > template_class_depth (type))
    /* If the TYPE is a template then it makes sense for it to be
       friends with itself; this means that each instantiation is
       friends with all other instantiations.  */
    {
      if (CLASS_TYPE_P (friend_type)
	  && CLASSTYPE_TEMPLATE_SPECIALIZATION (friend_type)
	  && uses_template_parms (friend_type))
	{
	  /* [temp.friend]
	     Friend declarations shall not declare partial
	     specializations.  */
	  error ("partial specialization `%T' declared `friend'",
		 friend_type);
	  return;
	}
  
      is_template_friend = 1;
    }
  else if (same_type_p (type, friend_type))
    {
      pedwarn ("class `%T' is implicitly friends with itself",
	          type);
      return;
    }
  else
    is_template_friend = 0;

  /* [temp.friend]

     A friend of a class or class template can be a function or
     class template, a specialization of a function template or
     class template, or an ordinary (nontemplate) function or
     class.  */
  if (!is_template_friend)
    ;/* ok */
  else if (TREE_CODE (friend_type) == TYPENAME_TYPE)
    {
      /* template <class T> friend typename S<T>::X; */
      error ("typename type `%#T' declared `friend'", friend_type);
      return;
    }
  else if (TREE_CODE (friend_type) == TEMPLATE_TYPE_PARM)
    {
      /* template <class T> friend class T; */
      error ("template parameter type `%T' declared `friend'", friend_type);
      return;
    }
  else if (!CLASSTYPE_TEMPLATE_INFO (friend_type))
    {
      /* template <class T> friend class A; where A is not a template */
      error ("`%#T' is not a template", friend_type);
      return;
    }

  if (is_template_friend)
    friend_type = CLASSTYPE_TI_TEMPLATE (friend_type);

  classes = CLASSTYPE_FRIEND_CLASSES (type);
  while (classes 
	 /* Stop if we find the same type on the list.  */
	 && !(TREE_CODE (TREE_VALUE (classes)) == TEMPLATE_DECL ?
	      friend_type == TREE_VALUE (classes) :
	      same_type_p (TREE_VALUE (classes), friend_type)))
    classes = TREE_CHAIN (classes);
  if (classes) 
    warning ("`%T' is already a friend of `%T'",
		TREE_VALUE (classes), type);
  else
    {
      maybe_add_class_template_decl_list (type, friend_type, /*friend_p=*/1);

      CLASSTYPE_FRIEND_CLASSES (type)
	= tree_cons (NULL_TREE, friend_type, CLASSTYPE_FRIEND_CLASSES (type));
      if (is_template_friend)
	friend_type = TREE_TYPE (friend_type);
      if (!uses_template_parms (type))
	CLASSTYPE_BEFRIENDING_CLASSES (friend_type)
	  = tree_cons (NULL_TREE, type, 
		       CLASSTYPE_BEFRIENDING_CLASSES (friend_type)); 
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
do_friend (ctype, declarator, decl, parmdecls, attrlist,
	   flags, quals, funcdef_flag)
     tree ctype, declarator, decl, parmdecls, attrlist;
     enum overload_flags flags;
     tree quals;
     int funcdef_flag;
{
  int is_friend_template = 0;

  /* Every decl that gets here is a friend of something.  */
  DECL_FRIEND_P (decl) = 1;

  if (TREE_CODE (declarator) == TEMPLATE_ID_EXPR)
    {
      declarator = TREE_OPERAND (declarator, 0);
      if (TREE_CODE (declarator) == LOOKUP_EXPR)
	declarator = TREE_OPERAND (declarator, 0);
      if (is_overloaded_fn (declarator))
	declarator = DECL_NAME (get_first_fn (declarator));
    }

  if (TREE_CODE (decl) != FUNCTION_DECL)
    abort ();

  is_friend_template = PROCESSING_REAL_TEMPLATE_DECL_P ();

  if (ctype)
    {
      tree cname = TYPE_NAME (ctype);
      if (TREE_CODE (cname) == TYPE_DECL)
	cname = DECL_NAME (cname);

      /* A method friend.  */
      if (flags == NO_SPECIAL && ctype && declarator == cname)
	DECL_CONSTRUCTOR_P (decl) = 1;

      /* This will set up DECL_ARGUMENTS for us.  */
      grokclassfn (ctype, decl, flags, quals);

      if (is_friend_template)
	decl = DECL_TI_TEMPLATE (push_template_decl (decl));
      else if (template_class_depth (current_class_type))
	decl = push_template_decl_real (decl, /*is_friend=*/1);

      /* We can't do lookup in a type that involves template
	 parameters.  Instead, we rely on tsubst_friend_function
	 to check the validity of the declaration later.  */
      if (processing_template_decl)
	add_friend (current_class_type, decl);
      /* A nested class may declare a member of an enclosing class
	 to be a friend, so we do lookup here even if CTYPE is in
	 the process of being defined.  */
      else if (COMPLETE_TYPE_P (ctype) || TYPE_BEING_DEFINED (ctype))
	{
	  decl = check_classfn (ctype, decl);

	  if (decl)
	    add_friend (current_class_type, decl);
	}
      else
	error ("member `%D' declared as friend before type `%T' defined",
		  decl, ctype);
    }
  /* A global friend.
     @@ or possibly a friend from a base class ?!?  */
  else if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      /* Friends must all go through the overload machinery,
	 even though they may not technically be overloaded.

	 Note that because classes all wind up being top-level
	 in their scope, their friend wind up in top-level scope as well.  */
      DECL_ARGUMENTS (decl) = parmdecls;
      if (funcdef_flag)
	SET_DECL_FRIEND_CONTEXT (decl, current_class_type);

      if (! DECL_USE_TEMPLATE (decl))
	{
	  /* We must check whether the decl refers to template
	     arguments before push_template_decl_real adds a
	     reference to the containing template class.  */
	  int warn = (warn_nontemplate_friend
		      && ! funcdef_flag && ! is_friend_template
		      && current_template_parms
		      && uses_template_parms (decl));

	  if (is_friend_template
	      || template_class_depth (current_class_type) != 0)
	    /* We can't call pushdecl for a template class, since in
	       general, such a declaration depends on template
	       parameters.  Instead, we call pushdecl when the class
	       is instantiated.  */
	    decl = push_template_decl_real (decl, /*is_friend=*/1); 
	  else if (current_function_decl)
	    /* This must be a local class, so pushdecl will be ok, and
	       insert an unqualified friend into the local scope
	       (rather than the containing namespace scope, which the
	       next choice will do).  */
	    decl = pushdecl (decl);
	  else
	    {
	      /* We can't use pushdecl, as we might be in a template
	         class specialization, and pushdecl will insert an
	         unqualified friend decl into the template parameter
	         scope, rather than the namespace containing it.  */
	      tree ns = decl_namespace_context (decl);
	      
	      push_nested_namespace (ns);
	      decl = pushdecl_namespace_level (decl);
	      pop_nested_namespace (ns);
	    }

	  if (warn)
	    {
	      static int explained;
	      warning ("friend declaration `%#D' declares a non-template function", decl);
	      if (! explained)
		{
		  warning ("(if this is not what you intended, make sure the function template has already been declared and add <> after the function name here) -Wno-non-template-friend disables this warning");
		  explained = 1;
		}
	    }
	}

      add_friend (current_class_type, 
		  is_friend_template ? DECL_TI_TEMPLATE (decl) : decl);
      DECL_FRIEND_P (decl) = 1;
    }

  /* Unfortunately, we have to handle attributes here.  Normally we would
     handle them in start_decl_1, but since this is a friend decl start_decl_1
     never gets to see it.  */

  /* Set attributes here so if duplicate decl, will have proper attributes.  */
  cplus_decl_attributes (&decl, attrlist, 0);

  return decl;
}
