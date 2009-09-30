/* Routines for C compiler part of GCC for a Symbian OS targeted SH backend.
   Copyright (C) 2004, 2005, 2007, 2009 Free Software Foundation, Inc.
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
#include "toplev.h"
#include "sh-symbian.h"


/* Return the type that we should use to determine if DECL is
   imported or exported.  */

tree
sh_symbian_associated_type (tree decl)
{
  tree t = NULL_TREE;

  /* We can just take the DECL_CONTEXT as normal.  */
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
      warning (OPT_Wattributes, "function %q+D is defined after prior "
	       "declaration as dllimport: attribute ignored",
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
      error ("definition of static data member %q+D of dllimport'd class",
	     decl);
      return false;
    }

  return true;
}

/* Handle a "dllimport" or "dllexport" attribute;
   arguments as in struct attribute_spec.handler.  */

tree
sh_symbian_handle_dll_attribute (tree *pnode, tree name, tree args,
				 int flags, bool *no_add_attrs)
{
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

int
sh_symbian_import_export_class (tree ctype ATTRIBUTE_UNUSED, int import_export)
{
  return import_export;
}
