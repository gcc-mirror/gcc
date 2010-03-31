/* General Solaris system support.
   Copyright (C) 2004, 2005 , 2007, 2010 Free Software Foundation, Inc.
   Contributed by CodeSourcery, LLC.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "output.h"
#include "tm.h"
#include "rtl.h"
#include "tm_p.h"
#include "toplev.h"
#include "ggc.h"

tree solaris_pending_aligns, solaris_pending_inits, solaris_pending_finis;

/* Attach any pending attributes for DECL to the list in *ATTRIBUTES.
   Pending attributes come from #pragma or _Pragma, so this code is
   only useful in the C family front ends, but it is included in
   all languages to avoid changing the target machine initializer
   depending on the language.  */

void
solaris_insert_attributes (tree decl, tree *attributes)
{
  tree *x, next;

  if (solaris_pending_aligns != NULL && TREE_CODE (decl) == VAR_DECL)
    for (x = &solaris_pending_aligns; *x; x = &TREE_CHAIN (*x))
      {
	tree name = TREE_PURPOSE (*x);
	tree value = TREE_VALUE (*x);
	if (DECL_NAME (decl) == name)
	  {
	    if (lookup_attribute ("aligned", DECL_ATTRIBUTES (decl))
		|| lookup_attribute ("aligned", *attributes))
	      warning (0, "ignoring %<#pragma align%> for explicitly "
		       "aligned %q+D", decl);
	    else
	      *attributes = tree_cons (get_identifier ("aligned"), value,
				       *attributes);
	    next = TREE_CHAIN (*x);
	    ggc_free (*x);
	    *x = next;
	    break;
	  }
      }

  if (solaris_pending_inits != NULL && TREE_CODE (decl) == FUNCTION_DECL)
    for (x = &solaris_pending_inits; *x; x = &TREE_CHAIN (*x))
      {
	tree name = TREE_PURPOSE (*x);
	if (DECL_NAME (decl) == name)
	  {
	    *attributes = tree_cons (get_identifier ("init"), NULL,
				     *attributes);
	    TREE_USED (decl) = 1;
	    DECL_PRESERVE_P (decl) = 1;
	    next = TREE_CHAIN (*x);
	    ggc_free (*x);
	    *x = next;
	    break;
	  }
      }

  if (solaris_pending_finis != NULL && TREE_CODE (decl) == FUNCTION_DECL)
    for (x = &solaris_pending_finis; *x; x = &TREE_CHAIN (*x))
      {
	tree name = TREE_PURPOSE (*x);
	if (DECL_NAME (decl) == name)
	  {
	    *attributes = tree_cons (get_identifier ("fini"), NULL,
				     *attributes);
	    TREE_USED (decl) = 1;
	    DECL_PRESERVE_P (decl) = 1;
	    next = TREE_CHAIN (*x);
	    ggc_free (*x);
	    *x = next;
	    break;
	  }
      }
}

/* Output initializer or finalizer entries for DECL to FILE.  */

void
solaris_output_init_fini (FILE *file, tree decl)
{
  if (lookup_attribute ("init", DECL_ATTRIBUTES (decl)))
    {
      fprintf (file, "\t.pushsection\t\".init\"\n");
      ASM_OUTPUT_CALL (file, decl);
      fprintf (file, "\t.popsection\n");
    }

  if (lookup_attribute ("fini", DECL_ATTRIBUTES (decl)))
    {
      fprintf (file, "\t.pushsection\t\".fini\"\n");
      ASM_OUTPUT_CALL (file, decl);
      fprintf (file, "\t.popsection\n");
    }
}

/* Emit an assembler directive to set symbol for DECL visibility to
   the visibility type VIS, which must not be VISIBILITY_DEFAULT.  */

void
solaris_assemble_visibility (tree decl, int vis)
{
  /* Sun as uses .symbolic for STV_PROTECTED.  STV_INTERNAL is marked as
     `currently reserved', but the linker treats it like STV_HIDDEN.  Sun
     Studio 12.1 cc emits .hidden instead.

     There are 3 Sun extensions GCC doesn't yet know about: STV_EXPORTED,
     STV_SINGLETON, and STV_ELIMINATE.

     See Linker and Libraries Guide, Ch. 2, Link-Editor, Defining
     Additional Symbols with a mapfile,
     http://docs.sun.com/app/docs/doc/819-0690/gdzmc?a=view
     and Ch. 7, Object-File Format, Symbol Table Section,
     http://docs.sun.com/app/docs/doc/819-0690/chapter6-79797?a=view  */

  static const char * const visibility_types[] = {
    NULL, "symbolic", "hidden", "hidden"
  };

  const char *name, *type;

  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
  type = visibility_types[vis];

  /* .hidden dates back before Solaris 2.5, but .symbolic was only added in
     Solaris 9 12/02.  */
#ifdef HAVE_GAS_HIDDEN
  fprintf (asm_out_file, "\t.%s\t", type);
  assemble_name (asm_out_file, name);
  fprintf (asm_out_file, "\n");
#else
  warning (OPT_Wattributes, "visibility attribute not supported "
	   "in this configuration; ignored");
#endif
}
