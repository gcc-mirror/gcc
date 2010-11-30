/* Routines for GCC for a Symbian OS targeted SH backend, shared by
   both the C and C++ compilers.
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
#include "diagnostic-core.h"
#include "sh-symbian.h"

/* Return nonzero if SYMBOL is marked as being dllexport'd.  */

bool
sh_symbian_is_dllexported_name (const char *symbol)
{
  return strncmp (DLL_EXPORT_PREFIX, symbol,
		  strlen (DLL_EXPORT_PREFIX)) == 0;
}

/* Return nonzero if SYMBOL is marked as being dllimport'd.  */

static bool
sh_symbian_is_dllimported_name (const char *symbol)
{
  return strncmp (DLL_IMPORT_PREFIX, symbol,
		  strlen (DLL_IMPORT_PREFIX)) == 0;
}

/* Return nonzero if DECL is a dllexport'd object.  */

bool
sh_symbian_is_dllexported (tree decl)
{
  tree exp;

  if (   TREE_CODE (decl) != VAR_DECL
      && TREE_CODE (decl) != FUNCTION_DECL)
    return false;

  exp = lookup_attribute ("dllexport", DECL_ATTRIBUTES (decl));

  /* Class members get the dllexport status of their class.  */
  if (exp == NULL)
    {
      tree class = sh_symbian_associated_type (decl);

      if (class)
	exp = lookup_attribute ("dllexport", TYPE_ATTRIBUTES (class));
    }
#if SYMBIAN_DEBUG
  if (exp)
    {
      print_node_brief (stderr, "dllexport:", decl, 0);
      fprintf (stderr, "\n");
    }
  else
#if SYMBIAN_DEBUG < 2
    if (TREE_CODE (decl) != FUNCTION_DECL)
#endif
    {
      print_node_brief (stderr, "no dllexport:", decl, 0);
      fprintf (stderr, "\n");
    }
#endif
  return exp ? true : false;
}

/* Mark a DECL as being dllimport'd.  */

static void
sh_symbian_mark_dllimport (tree decl)
{
  const char *oldname;
  char *newname;
  tree idp;
  rtx rtlname;
  rtx newrtl;

  rtlname = XEXP (DECL_RTL (decl), 0);
  if (MEM_P (rtlname))
    rtlname = XEXP (rtlname, 0);
  gcc_assert (GET_CODE (rtlname) == SYMBOL_REF);
  oldname = XSTR (rtlname, 0);

  if (sh_symbian_is_dllexported_name (oldname))
    {
      error ("%qE declared as both exported to and imported from a DLL",
             DECL_NAME (decl));
    }
  else if (sh_symbian_is_dllimported_name (oldname))
    {
      /* Already done, but do a sanity check to prevent assembler errors.  */
      if (!DECL_EXTERNAL (decl) || !TREE_PUBLIC (decl))
	error ("failure in redeclaration of %q+D: dllimport%'d symbol lacks external linkage",
	       decl);
    }
  else
    {
      newname = (char *) alloca (strlen (DLL_IMPORT_PREFIX) + strlen (oldname) + 1);
      sprintf (newname, "%s%s", DLL_IMPORT_PREFIX, oldname);

      /* We pass newname through get_identifier to ensure it has a unique
	 address.  RTL processing can sometimes peek inside the symbol ref
	 and compare the string's addresses to see if two symbols are
	 identical.  */
      idp = get_identifier (newname);
      newrtl = gen_rtx_SYMBOL_REF (Pmode, IDENTIFIER_POINTER (idp));
      XEXP (DECL_RTL (decl), 0) = newrtl;
    }
}

/* Mark a DECL as being dllexport'd.
   Note that we override the previous setting (e.g.: dllimport).  */

static void
sh_symbian_mark_dllexport (tree decl)
{
  const char *oldname;
  char *newname;
  rtx rtlname;
  tree idp;

  rtlname = XEXP (DECL_RTL (decl), 0);
  if (MEM_P (rtlname))
    rtlname = XEXP (rtlname, 0);
  gcc_assert (GET_CODE (rtlname) == SYMBOL_REF);
  oldname = XSTR (rtlname, 0);

  if (sh_symbian_is_dllimported_name (oldname))
    {
     /* Remove DLL_IMPORT_PREFIX.
	Note - we do not issue a warning here.  In Symbian's environment it
	is legitimate for a prototype to be marked as dllimport and the
	corresponding definition to be marked as dllexport.  The prototypes
	are in headers used everywhere and the definition is in a translation
	unit which has included the header in order to ensure argument
	correctness.  */
      oldname += strlen (DLL_IMPORT_PREFIX);
      DECL_DLLIMPORT_P (decl) = 0;
    }
  else if (sh_symbian_is_dllexported_name (oldname))
    return; /* Already done.  */

  newname = (char *) alloca (strlen (DLL_EXPORT_PREFIX) + strlen (oldname) + 1);
  sprintf (newname, "%s%s", DLL_EXPORT_PREFIX, oldname);

  /* We pass newname through get_identifier to ensure it has a unique
     address.  RTL processing can sometimes peek inside the symbol ref
     and compare the string's addresses to see if two symbols are
     identical.  */
  idp = get_identifier (newname);

  XEXP (DECL_RTL (decl), 0) =
    gen_rtx_SYMBOL_REF (Pmode, IDENTIFIER_POINTER (idp));
}

void
sh_symbian_encode_section_info (tree decl, rtx rtl, int first)
{
  default_encode_section_info (decl, rtl, first);

  /* Mark the decl so we can tell from the rtl whether
     the object is dllexport'd or dllimport'd.  */
  if (sh_symbian_is_dllexported (decl))
    sh_symbian_mark_dllexport (decl);
  else if (sh_symbian_is_dllimported (decl))
    sh_symbian_mark_dllimport (decl);
  /* It might be that DECL has already been marked as dllimport, but a
     subsequent definition nullified that.  The attribute is gone but
     DECL_RTL still has (DLL_IMPORT_PREFIX) prefixed. We need to remove
     that. Ditto for the DECL_DLLIMPORT_P flag.  */
  else if (  (TREE_CODE (decl) == FUNCTION_DECL
	   || TREE_CODE (decl) == VAR_DECL)
	   && DECL_RTL (decl) != NULL_RTX
	   && MEM_P (DECL_RTL (decl))
	   && MEM_P (XEXP (DECL_RTL (decl), 0))
	   && GET_CODE (XEXP (XEXP (DECL_RTL (decl), 0), 0)) == SYMBOL_REF
	   && sh_symbian_is_dllimported_name (XSTR (XEXP (XEXP (DECL_RTL (decl), 0), 0), 0)))
    {
      const char * oldname = XSTR (XEXP (XEXP (DECL_RTL (decl), 0), 0), 0);
      /* Remove DLL_IMPORT_PREFIX.  */
      tree idp = get_identifier (oldname + strlen (DLL_IMPORT_PREFIX));
      rtx newrtl = gen_rtx_SYMBOL_REF (Pmode, IDENTIFIER_POINTER (idp));

      warning (0, "%s %q+D %s after being referenced with dllimport linkage",
	       TREE_CODE (decl) == VAR_DECL ? "variable" : "function",
	       decl, (DECL_INITIAL (decl) || !DECL_EXTERNAL (decl))
	       ? "defined locally" : "redeclared without dllimport attribute");

      XEXP (DECL_RTL (decl), 0) = newrtl;

      DECL_DLLIMPORT_P (decl) = 0;
    }
}

/* Return the length of a function name prefix
    that starts with the character 'c'.  */

static int
sh_symbian_get_strip_length (int c)
{
  /* XXX Assumes strlen (DLL_EXPORT_PREFIX) == strlen (DLL_IMPORT_PREFIX).  */
  return (c == SH_SYMBIAN_FLAG_CHAR[0]) ? strlen (DLL_EXPORT_PREFIX) : 0;
}

/* Return a pointer to a function's name with any
   and all prefix encodings stripped from it.  */

const char *
sh_symbian_strip_name_encoding (const char *name)
{
  int skip;

  while ((skip = sh_symbian_get_strip_length (*name)))
    name += skip;

  return name;
}

