/* Routines for C++ support for GCC for a Symbian OS targeted SH backend.
   Copyright (C) 2004, 2005, 2007, 2009, 2010 Free Software Foundation, Inc.
   Contributed by RedHat.
   Most of this code is stolen from i386/winnt.c.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "output.h"
#include "flags.h"
#include "tree.h"
#include "expr.h"
#include "tm_p.h"
#include "cp/cp-tree.h"	/* We need access to the OVL_... macros.  */
#include "diagnostic-core.h"
#include "sh-symbian.h"


/* Return the type that we should use to determine if DECL is
   imported or exported.  */

tree
sh_symbian_associated_type (tree decl)
{
  tree t = NULL_TREE;

  if (TREE_CODE (TREE_TYPE (decl)) == METHOD_TYPE)
  /* Methods now inherit their dllimport/dllexport attributes correctly
     so there is no need to check their class.  In fact it is wrong to
     check their class since a method can remain unexported from an
     exported class.  */
    return t;

  /* Otherwise we can just take the DECL_CONTEXT as normal.  */
  if (DECL_CONTEXT (decl) && TYPE_P (DECL_CONTEXT (decl)))
    t = DECL_CONTEXT (decl);

  return t;
}


/* Return nonzero if DECL is a dllimport'd object.  */

bool
sh_symbian_is_dllimported (tree decl)
{
  tree imp;

  if (   TREE_CODE (decl) != VAR_DECL
      && TREE_CODE (decl) != FUNCTION_DECL)
    return false;

  imp = lookup_attribute ("dllimport", DECL_ATTRIBUTES (decl));
  if (imp)
    return true;

  /* Class members get the dllimport status of their class.  */
  imp = sh_symbian_associated_type (decl);
  if (! imp)
    return false;

  imp = lookup_attribute ("dllimport", TYPE_ATTRIBUTES (imp));
  if (!imp)
    return false;

  /* Don't mark defined functions as dllimport.  If the definition itself
     was marked with dllimport, then sh_symbian_handle_dll_attribute reports
     an error. This handles the case when the definition overrides an
     earlier declaration.  */
  if (TREE_CODE (decl) ==  FUNCTION_DECL
      && DECL_INITIAL (decl)
      && ! DECL_DECLARED_INLINE_P (decl))
    {
      /* Don't warn about artificial methods.  */
      if (!DECL_ARTIFICIAL (decl))
	warning (OPT_Wattributes, "function %q+D is defined after prior "
		 "declaration as dllimport: attribute ignored",
		 decl);
      return false;
    }

  /* We ignore the dllimport attribute for inline member functions.
     This differs from MSVC behavior which treats it like GNUC
     'extern inline' extension.   */
  else if (TREE_CODE (decl) == FUNCTION_DECL && DECL_DECLARED_INLINE_P (decl))
    {
      if (extra_warnings)
	warning (OPT_Wattributes, "inline function %q+D is declared as "
		 "dllimport: attribute ignored",
		 decl);
      return false;
    }

  /*  Don't allow definitions of static data members in dllimport
      class.  Just ignore the attribute for vtable data.  */
  else if (TREE_CODE (decl) == VAR_DECL
	   && TREE_STATIC (decl)
	   && TREE_PUBLIC (decl)
	   && !DECL_EXTERNAL (decl))
    {
      if (!DECL_VIRTUAL_P (decl))
	error ("definition of static data member %q+D of dllimport%'d class",
	       decl);
      return false;
    }

  /* Since we can't treat a pointer to a dllimport'd symbol as a
     constant address, we turn off the attribute on C++ virtual
     methods to allow creation of vtables using thunks.  Don't mark
     artificial methods either (in sh_symbian_associated_type, only
     COMDAT artificial method get import status from class context).  */
  else if (TREE_CODE (TREE_TYPE (decl)) == METHOD_TYPE
	   && (DECL_VIRTUAL_P (decl) || DECL_ARTIFICIAL (decl)))
    return false;

  return true;
}


/* This code implements a specification for exporting the vtable and rtti of
   classes that have members with the dllexport or dllexport attributes.
   This specification is defined here:

     http://www.armdevzone.com/EABI/exported_class.txt

   Basically it says that a class's vtable and rtti should be exported if
   the following rules apply:

   - If it has any non-inline non-pure virtual functions,
     at least one of these need to be declared dllimport
     OR any of the constructors is declared dllimport.

   AND

   - The class has an inline constructor/destructor and
     a key-function (placement of vtable uniquely defined) that
     is defined in this translation unit.

   The specification also says that for classes which will have their
   vtables and rtti exported that their base class(es) might also need a
   similar exporting if:

   - Every base class needs to have its vtable & rtti exported
     as well, if the following the conditions hold true:
     + The base class has a non-inline declared non-pure virtual function
     + The base class is polymorphic (has or inherits any virtual functions)
       or the base class has any virtual base classes.  */

/* Decide if a base class of a class should
   also have its vtable and rtti exported.  */

static void
sh_symbian_possibly_export_base_class (tree base_class)
{
  VEC(tree,gc) *method_vec;
  int len;

  if (! (TYPE_CONTAINS_VPTR_P (base_class)))
    return;

  method_vec = CLASSTYPE_METHOD_VEC (base_class);
  len = method_vec ? VEC_length (tree, method_vec) : 0;

  for (;len --;)
    {
      tree member = VEC_index (tree, method_vec, len);

      if (! member)
	continue;

      for (member = OVL_CURRENT (member); member; member = OVL_NEXT (member))
	{
	  if (TREE_CODE (member) != FUNCTION_DECL)
	    continue;

	  if (DECL_CONSTRUCTOR_P (member) || DECL_DESTRUCTOR_P (member))
	    continue;

	  if (! DECL_VIRTUAL_P (member))
	    continue;

	  if (DECL_PURE_VIRTUAL_P (member))
	    continue;

	  if (DECL_DECLARED_INLINE_P (member))
	    continue;

	  break;
	}

      if (member)
	break;
    }

  if (len < 0)
    return;

  /* FIXME: According to the spec this base class should be exported, but
     a) how do we do this ? and
     b) it does not appear to be necessary for compliance with the Symbian
        OS which so far is the only consumer of this code.  */
#if SYMBIAN_DEBUG
  print_node_brief (stderr, "", base_class, 0);
  fprintf (stderr, " EXPORTed [base class of exported class]\n");
#endif
}

/* Add the named attribute to the given node.  Copes with both DECLs and
   TYPEs.  Will only add the attribute if it is not already present.  */

static void
sh_symbian_add_attribute (tree node, const char *attr_name)
{
  tree attrs;
  tree attr;

  attrs = DECL_P (node) ? DECL_ATTRIBUTES (node) : TYPE_ATTRIBUTES (node);

  if (lookup_attribute (attr_name, attrs) != NULL_TREE)
    return;

  attr = get_identifier (attr_name);

  if (DECL_P (node))
    DECL_ATTRIBUTES (node) = tree_cons (attr, NULL_TREE, attrs);
  else
    TYPE_ATTRIBUTES (node) = tree_cons (attr, NULL_TREE, attrs);

#if SYMBIAN_DEBUG
  fprintf (stderr, "propagate %s attribute", attr_name);
  print_node_brief (stderr, " to", node, 0);
  fprintf (stderr, "\n");
#endif
}

/* Add the named attribute to a class and its vtable and rtti.  */

static void
sh_symbian_add_attribute_to_class_vtable_and_rtti (tree ctype, const char *attr_name)
{
  sh_symbian_add_attribute (ctype, attr_name);

  /* If the vtable exists then they need annotating as well.  */
  if (CLASSTYPE_VTABLES (ctype))
    /* XXX - Do we need to annotate any vtables other than the primary ?  */
    sh_symbian_add_attribute (CLASSTYPE_VTABLES (ctype), attr_name);

  /* If the rtti exists then it needs annotating as well.  */
  if (TYPE_MAIN_VARIANT (ctype)
      && CLASSTYPE_TYPEINFO_VAR (TYPE_MAIN_VARIANT (ctype)))
    sh_symbian_add_attribute (CLASSTYPE_TYPEINFO_VAR (TYPE_MAIN_VARIANT (ctype)),
			      attr_name);
}

/* Decide if a class needs to have an attribute because
   one of its member functions has the attribute.  */

static bool
sh_symbian_class_needs_attribute (tree ctype, const char *attribute_name)
{
  VEC(tree,gc) *method_vec;

  method_vec = CLASSTYPE_METHOD_VEC (ctype);

  /* If the key function has the attribute then the class needs it too.  */
  if (TYPE_POLYMORPHIC_P (ctype)
      && method_vec
      && tree_contains_struct [TREE_CODE (ctype), TS_DECL_COMMON] == 1
      && lookup_attribute (attribute_name,
			   DECL_ATTRIBUTES (VEC_index (tree, method_vec, 0))))
    return true;

  /* Check the class's member functions.  */
  if (TREE_CODE (ctype) == RECORD_TYPE)
    {
      unsigned int len;

      len = method_vec ? VEC_length (tree, method_vec) : 0;

      for (;len --;)
	{
	  tree member = VEC_index (tree, method_vec, len);

	  if (! member)
	    continue;

	  for (member = OVL_CURRENT (member);
	       member;
	       member = OVL_NEXT (member))
	    {
	      if (TREE_CODE (member) != FUNCTION_DECL)
		continue;

	      if (DECL_PURE_VIRTUAL_P (member))
		continue;

	      if (! DECL_VIRTUAL_P (member))
		continue;

	      if (lookup_attribute (attribute_name, DECL_ATTRIBUTES (member)))
		{
#if SYMBIAN_DEBUG
		  print_node_brief (stderr, "", ctype, 0);
		  fprintf (stderr, " inherits %s because", attribute_name);
		  print_node_brief (stderr, "", member, 0);
		  fprintf (stderr, " has it.\n");
#endif
		  return true;
		}
	    }
	}
    }

#if SYMBIAN_DEBUG
  print_node_brief (stderr, "", ctype, 0);
  fprintf (stderr, " does not inherit %s\n", attribute_name);
#endif
  return false;
}

/* Decide if a class needs its vtable and rtti exporting.  */

static bool
symbian_export_vtable_and_rtti_p (tree ctype)
{
  bool inline_ctor_dtor;
  bool dllimport_ctor_dtor;
  bool dllimport_member;
  tree binfo, base_binfo;
  VEC(tree,gc) *method_vec;
  tree key;
  int i;
  int len;

  /* Make sure that we are examining a class...  */
  if (TREE_CODE (ctype) != RECORD_TYPE)
    {
#if SYMBIAN_DEBUG
      print_node_brief (stderr, "", ctype, 0);
      fprintf (stderr, " does NOT need to be EXPORTed [not a class]\n");
#endif
      return false;
    }

  /* If the class does not have a key function it
     does not need to have its vtable exported.  */
  if ((key = CLASSTYPE_KEY_METHOD (ctype)) == NULL_TREE)
    {
#if SYMBIAN_DEBUG
      print_node_brief (stderr, "", ctype, 0);
      fprintf (stderr, " does NOT need to be EXPORTed [no key function]\n");
#endif
      return false;
    }

  /* If the key fn has not been defined
     then the class should not be exported.  */
  if (! TREE_ASM_WRITTEN (key))
    {
#if SYMBIAN_DEBUG
      print_node_brief (stderr, "", ctype, 0);
      fprintf (stderr, " does NOT need to be EXPORTed [key function not defined]\n");
#endif
      return false;
    }

  /* Check the class's member functions.  */
  inline_ctor_dtor = false;
  dllimport_ctor_dtor = false;
  dllimport_member = false;

  method_vec = CLASSTYPE_METHOD_VEC (ctype);
  len = method_vec ? VEC_length (tree, method_vec) : 0;

  for (;len --;)
    {
      tree member = VEC_index (tree, method_vec, len);

      if (! member)
	continue;

      for (member = OVL_CURRENT (member); member; member = OVL_NEXT (member))
	{
	  if (TREE_CODE (member) != FUNCTION_DECL)
	    continue;

	  if (DECL_CONSTRUCTOR_P (member) || DECL_DESTRUCTOR_P (member))
	    {
	      if (DECL_DECLARED_INLINE_P (member)
		  /* Ignore C++ backend created inline ctors/dtors.  */
		  && (   DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P (member)
		      || DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P (member)))
		inline_ctor_dtor = true;

	      if (lookup_attribute ("dllimport", DECL_ATTRIBUTES (member)))
		dllimport_ctor_dtor = true;
	    }
	  else
	    {
	      if (DECL_PURE_VIRTUAL_P (member))
		continue;

	      if (! DECL_VIRTUAL_P (member))
		continue;

	      if (DECL_DECLARED_INLINE_P (member))
		continue;

	      if (lookup_attribute ("dllimport", DECL_ATTRIBUTES (member)))
		dllimport_member = true;
	    }
	}
    }

  if (! dllimport_member && ! dllimport_ctor_dtor)
    {
#if SYMBIAN_DEBUG
      print_node_brief (stderr, "", ctype, 0);
      fprintf (stderr,
	       " does NOT need to be EXPORTed [no non-pure virtuals or ctors/dtors with dllimport]\n");
#endif
      return false;
    }

  if (! inline_ctor_dtor)
    {
#if SYMBIAN_DEBUG
      print_node_brief (stderr, "", ctype, 0);
      fprintf (stderr,
	       " does NOT need to be EXPORTed [no inline ctor/dtor]\n");
#endif
      return false;
    }

#if SYMBIAN_DEBUG
  print_node_brief (stderr, "", ctype, 0);
  fprintf (stderr, " DOES need to be EXPORTed\n");
#endif

  /* Now we must check and possibly export the base classes.  */
  for (i = 0, binfo = TYPE_BINFO (ctype);
       BINFO_BASE_ITERATE (binfo, i, base_binfo); i++)
    sh_symbian_possibly_export_base_class (BINFO_TYPE (base_binfo));

  return true;
}

/* Possibly override the decision to export class TYPE.  Upon entry
   IMPORT_EXPORT will contain 1 if the class is going to be exported,
   -1 if it is going to be imported and 0 otherwise.  This function
   should return the modified value and perform any other actions
   necessary to support the backend's targeted operating system.  */

int
sh_symbian_import_export_class (tree ctype, int import_export)
{
  const char *attr_name = NULL;

  /* If we are exporting the class but it does not have the dllexport
     attribute then we may need to add it.  Similarly imported classes
     may need the dllimport attribute.  */
  switch (import_export)
    {
    case  1: attr_name = "dllexport"; break;
    case -1: attr_name = "dllimport"; break;
    default: break;
    }

  if (attr_name
      && ! lookup_attribute (attr_name, TYPE_ATTRIBUTES (ctype)))
    {
      if (sh_symbian_class_needs_attribute (ctype, attr_name))
	sh_symbian_add_attribute_to_class_vtable_and_rtti (ctype, attr_name);

      /* Classes can be forced to export their
	 vtable and rtti under certain conditions.  */
      if (symbian_export_vtable_and_rtti_p (ctype))
	{
	  sh_symbian_add_attribute_to_class_vtable_and_rtti (ctype, "dllexport");

	  /* Make sure that the class and its vtable are exported.  */
	  import_export = 1;

	  if (CLASSTYPE_VTABLES (ctype))
	    DECL_EXTERNAL (CLASSTYPE_VTABLES (ctype)) = 1;

	  /* Check to make sure that if the class has a key method that
	     it is now on the list of keyed classes.  That way its vtable
	     will be emitted.  */
	  if (CLASSTYPE_KEY_METHOD (ctype))
	    {
	      tree class;

	      for (class = keyed_classes; class; class = TREE_CHAIN (class))
		if (class == ctype)
		  break;

	      if (class == NULL_TREE)
		{
#if SYMBIAN_DEBUG
		  print_node_brief (stderr, "Add node", ctype, 0);
		  fprintf (stderr, " to the keyed classes list\n");
#endif
		  keyed_classes = tree_cons (NULL_TREE, ctype, keyed_classes);
		}
	    }

	  /* Make sure that the typeinfo will be emitted as well.  */
	  if (CLASS_TYPE_P (ctype))
	    TYPE_NEEDS_CONSTRUCTING (TREE_TYPE (CLASSTYPE_TYPEINFO_VAR (TYPE_MAIN_VARIANT (ctype)))) = 1;
	}
    }

  return import_export;
}

/* Handle a "dllimport" or "dllexport" attribute;
   arguments as in struct attribute_spec.handler.  */

tree
sh_symbian_handle_dll_attribute (tree *pnode, tree name, tree args,
				 int flags, bool *no_add_attrs)
{
  tree thunk;
  tree node = *pnode;
  const char *attr = IDENTIFIER_POINTER (name);

  /* These attributes may apply to structure and union types being
     created, but otherwise should pass to the declaration involved.  */
  if (!DECL_P (node))
    {
      if (flags & ((int) ATTR_FLAG_DECL_NEXT
		   | (int) ATTR_FLAG_FUNCTION_NEXT
		   | (int) ATTR_FLAG_ARRAY_NEXT))
	{
	  warning (OPT_Wattributes, "%qs attribute ignored", attr);
	  *no_add_attrs = true;
	  return tree_cons (name, args, NULL_TREE);
	}

      if (TREE_CODE (node) != RECORD_TYPE && TREE_CODE (node) != UNION_TYPE)
	{
	  warning (OPT_Wattributes, "%qs attribute ignored", attr);
	  *no_add_attrs = true;
	}

      return NULL_TREE;
    }

  /* Report error on dllimport ambiguities
     seen now before they cause any damage.  */
  else if (is_attribute_p ("dllimport", name))
    {
      if (TREE_CODE (node) == VAR_DECL)
	{
	  if (DECL_INITIAL (node))
	    {
	      error ("variable %q+D definition is marked dllimport",
		     node);
	      *no_add_attrs = true;
	    }

	  /* `extern' needn't be specified with dllimport.
	     Specify `extern' now and hope for the best.  Sigh.  */
	  DECL_EXTERNAL (node) = 1;
	  /* Also, implicitly give dllimport'd variables declared within
	     a function global scope, unless declared static.  */
	  if (current_function_decl != NULL_TREE && ! TREE_STATIC (node))
  	    TREE_PUBLIC (node) = 1;
	}
    }

  /* If the node is an overloaded constructor or destructor, then we must
     make sure that the attribute is propagated along the overload chain,
     as it is these overloaded functions which will be emitted, rather than
     the user declared constructor itself.  */
  if (TREE_CODE (TREE_TYPE (node)) == METHOD_TYPE
      && (DECL_CONSTRUCTOR_P (node) || DECL_DESTRUCTOR_P (node)))
    {
      tree overload;

      for (overload = OVL_CHAIN (node); overload; overload = OVL_CHAIN (overload))
	{
	  tree node_args;
	  tree func_args;
	  tree function = OVL_CURRENT (overload);

	  if (! function
	      || ! DECL_P (function)
	      || (DECL_CONSTRUCTOR_P (node) && ! DECL_CONSTRUCTOR_P (function))
	      || (DECL_DESTRUCTOR_P (node)  && ! DECL_DESTRUCTOR_P (function)))
	    continue;

	  /* The arguments must match as well.  */
	  for (node_args = DECL_ARGUMENTS (node), func_args = DECL_ARGUMENTS (function);
	       node_args && func_args;
	       node_args = TREE_CHAIN (node_args), func_args = TREE_CHAIN (func_args))
	    if (TREE_TYPE (node_args) != TREE_TYPE (func_args))
	      break;

	  if (node_args || func_args)
	    {
	      /* We can ignore an extraneous __in_chrg arguments in the node.
		 GCC generated destructors, for example, will have this.  */
	      if ((node_args == NULL_TREE
		   || func_args != NULL_TREE)
		  && strcmp (IDENTIFIER_POINTER (DECL_NAME (node)), "__in_chrg") != 0)
		continue;
	    }

	  sh_symbian_add_attribute (function, attr);

	  /* Propagate the attribute to any function thunks as well.  */
	  for (thunk = DECL_THUNKS (function); thunk; thunk = DECL_CHAIN (thunk))
	    if (TREE_CODE (thunk) == FUNCTION_DECL)
	      sh_symbian_add_attribute (thunk, attr);
	}
    }

  if (TREE_CODE (node) == FUNCTION_DECL && DECL_VIRTUAL_P (node))
    {
      /* Propagate the attribute to any thunks of this function.  */
      for (thunk = DECL_THUNKS (node); thunk; thunk = DECL_CHAIN (thunk))
	if (TREE_CODE (thunk) == FUNCTION_DECL)
	  sh_symbian_add_attribute (thunk, attr);
    }

  /*  Report error if symbol is not accessible at global scope.  */
  if (!TREE_PUBLIC (node)
      && (   TREE_CODE (node) == VAR_DECL
	  || TREE_CODE (node) == FUNCTION_DECL))
    {
      error ("external linkage required for symbol %q+D because of %qE attribute",
	     node, name);
      *no_add_attrs = true;
    }

#if SYMBIAN_DEBUG
  print_node_brief (stderr, "mark node", node, 0);
  fprintf (stderr, " as %s\n", attr);
#endif

  return NULL_TREE;
}
