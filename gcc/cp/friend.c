/* Help friends in C++.
   Copyright (C) 1997 Free Software Foundation, Inc.

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
#include <stdio.h>
#include "tree.h"
#include "rtl.h"
#include "cp-tree.h"
#include "flags.h"
#include "output.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

static void add_friend PROTO((tree, tree));
static void add_friends PROTO((tree, tree, tree));

/* Friend data structures:

   Lists of friend functions come from TYPE_DECL nodes.  Since all
   aggregate types are automatically typedef'd, these nodes are guaranteed
   to exist.

   The TREE_PURPOSE of a friend list is the name of the friend,
   and its TREE_VALUE is another list.

   For each element of that list, either the TREE_VALUE or the TREE_PURPOSE
   will be filled in, but not both.  The TREE_VALUE of that list is an
   individual function which is a friend.  The TREE_PURPOSE of that list
   indicates a type in which all functions by that name are friends.

   Lists of friend classes come from _TYPE nodes.  Love that consistency
   thang.  */

int
is_friend (type, supplicant)
     tree type, supplicant;
{
  int declp;
  register tree list;

  if (supplicant == NULL_TREE || type == NULL_TREE)
    return 0;

  declp = (TREE_CODE_CLASS (TREE_CODE (supplicant)) == 'd');

  if (declp)
    /* It's a function decl.  */
    {
      tree list = DECL_FRIENDLIST (TYPE_MAIN_DECL (type));
      tree name = DECL_NAME (supplicant);
      tree ctype;

      if (DECL_FUNCTION_MEMBER_P (supplicant))
	ctype = DECL_CLASS_CONTEXT (supplicant);
      else
	ctype = NULL_TREE;

      for (; list ; list = TREE_CHAIN (list))
	{
	  if (name == TREE_PURPOSE (list))
	    {
	      tree friends = TREE_VALUE (list);
	      for (; friends ; friends = TREE_CHAIN (friends))
		{
		  if (ctype == TREE_PURPOSE (friends))
		    return 1;
		  if (comptypes (TREE_TYPE (supplicant),
				 TREE_TYPE (TREE_VALUE (friends)), 1))
		    return 1;
		}
	      break;
	    }
	}
    }
  else
    /* It's a type.  */
    {
      if (type == supplicant)
	return 1;
      
      list = CLASSTYPE_FRIEND_CLASSES (TREE_TYPE (TYPE_MAIN_DECL (type)));
      for (; list ; list = TREE_CHAIN (list))
	if (supplicant == TREE_VALUE (list))
	  return 1;
    }      

  {
    tree context;

    if (! declp)
      {
	/* Are we a nested or local class?  If so, we aren't friends
           with the CONTEXT.  */
	if (IS_AGGR_TYPE (supplicant))
	  context = NULL_TREE;
	else
	  context = DECL_CONTEXT (TYPE_MAIN_DECL (supplicant));
      }
    else if (DECL_FUNCTION_MEMBER_P (supplicant))
      context = DECL_CLASS_CONTEXT (supplicant);
    else
      context = NULL_TREE;

    if (context)
      return is_friend (type, context);
  }

  return 0;
}

/* Add a new friend to the friends of the aggregate type TYPE.
   DECL is the FUNCTION_DECL of the friend being added.  */

static void
add_friend (type, decl)
     tree type, decl;
{
  tree typedecl = TYPE_MAIN_DECL (type);
  tree list = DECL_FRIENDLIST (typedecl);
  tree name = DECL_NAME (decl);

  while (list)
    {
      if (name == TREE_PURPOSE (list))
	{
	  tree friends = TREE_VALUE (list);
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
  if (DECL_NAME (decl) == ansi_opname[(int) MODIFY_EXPR])
    {
      tree parmtypes = TYPE_ARG_TYPES (TREE_TYPE (decl));
      TYPE_HAS_ASSIGNMENT (TREE_TYPE (typedecl)) = 1;
      if (parmtypes && TREE_CHAIN (parmtypes))
	{
	  tree parmtype = TREE_VALUE (TREE_CHAIN (parmtypes));
	  if (TREE_CODE (parmtype) == REFERENCE_TYPE
	      && TREE_TYPE (parmtypes) == TREE_TYPE (typedecl))
	    TYPE_HAS_ASSIGN_REF (TREE_TYPE (typedecl)) = 1;
	}
    }
}

/* Declare that every member function NAME in FRIEND_TYPE
   (which may be NULL_TREE) is a friend of type TYPE.  */

static void
add_friends (type, name, friend_type)
     tree type, name, friend_type;
{
  tree typedecl = TYPE_MAIN_DECL (type);
  tree list = DECL_FRIENDLIST (typedecl);

  while (list)
    {
      if (name == TREE_PURPOSE (list))
	{
	  tree friends = TREE_VALUE (list);
	  while (friends && TREE_PURPOSE (friends) != friend_type)
	    friends = TREE_CHAIN (friends);
	  if (friends)
	    if (friend_type)
	      warning ("method `%s::%s' is already a friend of class",
		       TYPE_NAME_STRING (friend_type),
		       IDENTIFIER_POINTER (name));
	    else
	      warning ("function `%s' is already a friend of class `%s'",
		       IDENTIFIER_POINTER (name),
		       IDENTIFIER_POINTER (DECL_NAME (typedecl)));
	  else
	    TREE_VALUE (list) = tree_cons (friend_type, NULL_TREE,
					   TREE_VALUE (list));
	  return;
	}
      list = TREE_CHAIN (list);
    }
  DECL_FRIENDLIST (typedecl)
    = tree_cons (name,
		 build_tree_list (friend_type, NULL_TREE),
		 DECL_FRIENDLIST (typedecl));
  if (! strncmp (IDENTIFIER_POINTER (name),
		 IDENTIFIER_POINTER (ansi_opname[(int) MODIFY_EXPR]),
		 strlen (IDENTIFIER_POINTER (ansi_opname[(int) MODIFY_EXPR]))))
    {
      TYPE_HAS_ASSIGNMENT (TREE_TYPE (typedecl)) = 1;
      sorry ("declaring \"friend operator =\" will not find \"operator = (X&)\" if it exists");
    }
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

  if (IS_SIGNATURE (type))
    {
      error ("`friend' declaration in signature definition");
      return;
    }
  if (IS_SIGNATURE (friend_type))
    {
      error ("signature type `%s' declared `friend'",
	     IDENTIFIER_POINTER (TYPE_IDENTIFIER (friend_type)));
      return;
    }
  if (type == friend_type)
    {
      pedwarn ("class `%s' is implicitly friends with itself",
	       TYPE_NAME_STRING (type));
      return;
    }

  GNU_xref_hier (TYPE_NAME_STRING (type),
		 TYPE_NAME_STRING (friend_type), 0, 0, 1);

  classes = CLASSTYPE_FRIEND_CLASSES (type);
  while (classes && TREE_VALUE (classes) != friend_type)
    classes = TREE_CHAIN (classes);
  if (classes)
    warning ("class `%s' is already friends with class `%s'",
	     TYPE_NAME_STRING (TREE_VALUE (classes)), TYPE_NAME_STRING (type));
  else
    {
      CLASSTYPE_FRIEND_CLASSES (type)
	= tree_cons (NULL_TREE, friend_type, CLASSTYPE_FRIEND_CLASSES (type));
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
do_friend (ctype, declarator, decl, parmdecls, flags, quals, funcdef_flag)
     tree ctype, declarator, decl, parmdecls;
     enum overload_flags flags;
     tree quals;
     int funcdef_flag;
{
  /* Every decl that gets here is a friend of something.  */
  DECL_FRIEND_P (decl) = 1;

  if (ctype)
    {
      tree cname = TYPE_NAME (ctype);
      if (TREE_CODE (cname) == TYPE_DECL)
	cname = DECL_NAME (cname);

      /* A method friend.  */
      if (TREE_CODE (decl) == FUNCTION_DECL)
	{
	  if (flags == NO_SPECIAL && ctype && declarator == cname)
	    DECL_CONSTRUCTOR_P (decl) = 1;

	  /* This will set up DECL_ARGUMENTS for us.  */
	  grokclassfn (ctype, cname, decl, flags, quals);
	  if (TYPE_SIZE (ctype) != 0)
	    decl = check_classfn (ctype, decl);

	  if (TREE_TYPE (decl) != error_mark_node)
	    {
	      if (TYPE_SIZE (ctype))
		add_friend (current_class_type, decl);
	      else
		{
		  cp_error ("member `%D' declared as friend before type `%T' defined",
			    decl, ctype);
		}
	    }
	}
      else
	{
	  /* Possibly a bunch of method friends.  */

	  /* Get the class they belong to.  */
	  tree ctype = IDENTIFIER_TYPE_VALUE (cname);
	  tree fields = lookup_fnfields (TYPE_BINFO (ctype), declarator, 0);

	  if (fields)
	    add_friends (current_class_type, declarator, ctype);
	  else
	    error ("method `%s' is not a member of class `%s'",
		   IDENTIFIER_POINTER (declarator),
		   IDENTIFIER_POINTER (cname));
	  decl = void_type_node;
	}
    }
  else if (TREE_CODE (decl) == FUNCTION_DECL
	   && ((IDENTIFIER_LENGTH (declarator) == 4
		&& IDENTIFIER_POINTER (declarator)[0] == 'm'
		&& ! strcmp (IDENTIFIER_POINTER (declarator), "main"))
	       || (IDENTIFIER_LENGTH (declarator) > 10
		   && IDENTIFIER_POINTER (declarator)[0] == '_'
		   && IDENTIFIER_POINTER (declarator)[1] == '_'
		   && strncmp (IDENTIFIER_POINTER (declarator)+2,
			       "builtin_", 8) == 0)))
    {
      /* raw "main", and builtin functions never gets overloaded,
	 but they can become friends.  */
      add_friend (current_class_type, decl);
      DECL_FRIEND_P (decl) = 1;
      decl = void_type_node;
    }
  /* A global friend.
     @@ or possibly a friend from a base class ?!?  */
  else if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      /* Friends must all go through the overload machinery,
	 even though they may not technically be overloaded.

	 Note that because classes all wind up being top-level
	 in their scope, their friend wind up in top-level scope as well.  */
      DECL_ASSEMBLER_NAME (decl)
	= build_decl_overload (declarator, TYPE_ARG_TYPES (TREE_TYPE (decl)),
			       TREE_CODE (TREE_TYPE (decl)) == METHOD_TYPE);
      DECL_ARGUMENTS (decl) = parmdecls;
      if (funcdef_flag)
	DECL_CLASS_CONTEXT (decl) = current_class_type;

      if (! DECL_USE_TEMPLATE (decl))
	{
	  /* We can call pushdecl here, because the TREE_CHAIN of this
	     FUNCTION_DECL is not needed for other purposes.  Don't do this
	     for a template instantiation. */
	  decl = pushdecl (decl);

	  if (! funcdef_flag && ! flag_guiding_decls
	      && current_template_parms && uses_template_parms (decl))
	    {
	      static int explained;
	      cp_warning ("friend declaration `%#D'", decl);
	      warning ("  will not be treated as a template instantiation");
	      if (! explained)
		{
		  warning ("  unless you compile with -fguiding-decls");
		  warning ("  or add <> after the function name");
		  explained = 1;
		}
	    }
	}

      make_decl_rtl (decl, NULL_PTR, 1);
      add_friend (current_class_type, decl);

      DECL_FRIEND_P (decl) = 1;
    }
  else
    {
      /* @@ Should be able to ingest later definitions of this function
	 before use.  */
      tree decl = lookup_name_nonclass (declarator);
      if (decl == NULL_TREE)
	{
	  warning ("implicitly declaring `%s' as struct",
		   IDENTIFIER_POINTER (declarator));
	  decl = xref_tag (record_type_node, declarator, NULL_TREE, 1);
	  decl = TYPE_MAIN_DECL (decl);
	}

      /* Allow abbreviated declarations of overloaded functions,
	 but not if those functions are really class names.  */
      if (TREE_CODE (decl) == TREE_LIST && TREE_TYPE (TREE_PURPOSE (decl)))
	{
	  warning ("`friend %s' archaic, use `friend class %s' instead",
		   IDENTIFIER_POINTER (declarator),
		   IDENTIFIER_POINTER (declarator));
	  decl = TREE_TYPE (TREE_PURPOSE (decl));
	}

      if (TREE_CODE (decl) == TREE_LIST)
	add_friends (current_class_type, TREE_PURPOSE (decl), NULL_TREE);
      else
	make_friend_class (current_class_type, TREE_TYPE (decl));
      decl = void_type_node;
    }
  return decl;
}
