/* Help friends in C++.
   Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation, Inc.

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
#include "cp-tree.h"
#include "flags.h"
#include "output.h"
#include "toplev.h"

/* Friend data structures are described in cp-tree.h.  */

/* Returns non-zero if SUPPLICANT is a friend of TYPE.  */

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

		  /* With -fguiding-decls we are more lenient about
		     friendship.  This is bogus in general since two
		     specializations of a template with non-type
		     template parameters may have the same type, but
		     be different.  

		     Temporarily, we are also more lenient to deal
		     with nested friend functions, for which there can
		     be more than one FUNCTION_DECL, despite being the
		     same function.  When that's fixed, the
		     FUNCTION_MEMBER_P bit can go.  */
		  if ((flag_guiding_decls 
		       || DECL_FUNCTION_MEMBER_P (supplicant))
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

  /* A namespace is not friend to anybody. */
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
		  cp_warning ("`%D' is already a friend of class `%T'",
			      decl, type);
		  cp_warning_at ("previous friend declaration of `%D'",
				 TREE_VALUE (friends));
		  return;
		}
	    }
	  TREE_VALUE (list) = tree_cons (error_mark_node, decl,
					 TREE_VALUE (list));
	  return;
	}
      list = TREE_CHAIN (list);
    }

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
      cp_error ("invalid type `%T' declared `friend'", friend_type);
      return;
    }

  if (CLASS_TYPE_P (friend_type)
      && CLASSTYPE_PARTIAL_SPECIALIZATION (friend_type)
      && uses_template_parms (friend_type))
    {
      /* [temp.friend]
	 
	 Friend declarations shall not declare partial
	 specializations.  */
      cp_error ("partial specialization `%T' declared `friend'",
		friend_type);
      return;
    }
  
  if (processing_template_decl > template_class_depth (type))
    /* If the TYPE is a template then it makes sense for it to be
       friends with itself; this means that each instantiation is
       friends with all other instantiations.  */
    is_template_friend = 1;
  else if (same_type_p (type, friend_type))
    {
      pedwarn ("class `%s' is implicitly friends with itself",
	       TYPE_NAME_STRING (type));
      return;
    }
  else
    is_template_friend = 0;

  if (is_template_friend 
      && (TREE_CODE (friend_type) == TYPENAME_TYPE
	  || TREE_CODE (friend_type) == TEMPLATE_TYPE_PARM))
    {
      /* [temp.friend]

	   A friend of a class or class template can be a function or
	   class template, a specialization of a function template or
	   class template, or an ordinary (nontemplate) function or
	   class. 
	   
	 But, we're looking at something like:

	   template <class T> friend typename S<T>::X;

	 or:

	   template <class T> friend class T;

	 which isn't any of these.  */
      if (TREE_CODE (friend_type) == TYPENAME_TYPE)
	cp_error ("typename type `%T' declared `friend'",
		  friend_type);
      else
	cp_error ("template parameter type `%T' declared `friend'",
		  friend_type);
      return;
    }

  GNU_xref_hier (type, friend_type, 0, 0, 1);

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
    cp_warning ("`%T' is already a friend of `%T'",
		TREE_VALUE (classes), type);
  else
    {
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
  tree prefix_attributes, attributes;

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
    my_friendly_abort (990513);

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
	cp_error ("member `%D' declared as friend before type `%T' defined",
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
	  /* We can call pushdecl here, because the TREE_CHAIN of this
	     FUNCTION_DECL is not needed for other purposes.  Don't do
	     this for a template instantiation.  However, we don't
	     call pushdecl() for a friend function of a template
	     class, since in general, such a declaration depends on
	     template parameters.  Instead, we call pushdecl when the
	     class is instantiated.  */
	  if (!is_friend_template
	      && template_class_depth (current_class_type) == 0)
	    decl = pushdecl (decl);
	  else 
	    decl = push_template_decl_real (decl, /*is_friend=*/1); 

	  if (warn_nontemplate_friend
	      && ! funcdef_flag && ! flag_guiding_decls && ! is_friend_template
	      && current_template_parms && uses_template_parms (decl))
	    {
	      static int explained;
	      cp_warning ("friend declaration `%#D'", decl);
	      warning ("  declares a non-template function");
	      if (! explained)
		{
		  warning ("  (if this is not what you intended, make sure the function template has already been declared and add <> after the function name here) -Wno-non-template-friend disables this warning.");
		  explained = 1;
		}
	    }
	}

      make_decl_rtl (decl, NULL_PTR, 1);
      add_friend (current_class_type, 
		  is_friend_template ? DECL_TI_TEMPLATE (decl) : decl);
      DECL_FRIEND_P (decl) = 1;
    }

  /* Unfortunately, we have to handle attributes here.  Normally we would
     handle them in start_decl_1, but since this is a friend decl start_decl_1
     never gets to see it.  */

  if (attrlist)
    {
      attributes = TREE_PURPOSE (attrlist);
      prefix_attributes = TREE_VALUE (attrlist);
    }
  else
    {
      attributes = NULL_TREE;
      prefix_attributes = NULL_TREE;
    } 

#ifdef SET_DEFAULT_DECL_ATTRIBUTES
  SET_DEFAULT_DECL_ATTRIBUTES (decl, attributes);
#endif
  
  /* Set attributes here so if duplicate decl, will have proper attributes.  */
  cplus_decl_attributes (decl, attributes, prefix_attributes);

  return decl;
}
