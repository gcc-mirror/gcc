/* Subroutines for insn-output.c for Windows NT.
   Contributed by Douglas Rupp (drupp@cs.washington.edu)
   Copyright (C) 1995, 1997, 1998 Free Software Foundation, Inc.

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
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "output.h"
#include "tree.h"
#include "flags.h"

/* i386/PE specific attribute support.

   i386/PE has two new attributes:
   dllexport - for exporting a function/variable that will live in a dll
   dllimport - for importing a function/variable from a dll

   Microsoft allows multiple declspecs in one __declspec, separating
   them with spaces.  We do NOT support this.  Instead, use __declspec
   multiple times.
*/

/* Return nonzero if ATTR is a valid attribute for DECL.
   ATTRIBUTES are any existing attributes and ARGS are the arguments
   supplied with ATTR.  */

int
i386_pe_valid_decl_attribute_p (decl, attributes, attr, args)
     tree decl;
     tree attributes;
     tree attr;
     tree args;
{
  if (args == NULL_TREE)
    {
      if (is_attribute_p ("dllexport", attr))
	return 1;
      if (is_attribute_p ("dllimport", attr))
	return 1;
    }

  return i386_valid_decl_attribute_p (decl, attributes, attr, args);
}

/* Return nonzero if ATTR is a valid attribute for TYPE.
   ATTRIBUTES are any existing attributes and ARGS are the arguments
   supplied with ATTR.  */

int
i386_pe_valid_type_attribute_p (type, attributes, attr, args)
     tree type;
     tree attributes;
     tree attr;
     tree args;
{
  if (args == NULL_TREE
      && (TREE_CODE (type) == RECORD_TYPE || TREE_CODE (type) == UNION_TYPE))
    {
      if (is_attribute_p ("dllexport", attr))
	return 1;
      if (is_attribute_p ("dllimport", attr))
	return 1;
    }

  return i386_valid_type_attribute_p (type, attributes, attr, args);
}

/* Merge attributes in decls OLD and NEW.

   This handles the following situation:

   __declspec (dllimport) int foo;
   int foo;

   The second instance of `foo' nullifies the dllimport.  */

tree
i386_pe_merge_decl_attributes (old, new)
     tree old, new;
{
  tree a;
  int delete_dllimport_p;

  old = DECL_MACHINE_ATTRIBUTES (old);
  new = DECL_MACHINE_ATTRIBUTES (new);

  /* What we need to do here is remove from `old' dllimport if it doesn't
     appear in `new'.  dllimport behaves like extern: if a declaration is
     marked dllimport and a definition appears later, then the object
     is not dllimport'd.  */

  if (lookup_attribute ("dllimport", old) != NULL_TREE
      && lookup_attribute ("dllimport", new) == NULL_TREE)
    delete_dllimport_p = 1;
  else
    delete_dllimport_p = 0;

  a = merge_attributes (old, new);

  if (delete_dllimport_p)
    {
      tree prev,t;

      /* Scan the list for dllimport and delete it.  */
      for (prev = NULL_TREE, t = a; t; prev = t, t = TREE_CHAIN (t))
	{
	  if (is_attribute_p ("dllimport", TREE_PURPOSE (t)))
	    {
	      if (prev == NULL_TREE)
		a = TREE_CHAIN (a);
	      else
		TREE_CHAIN (prev) = TREE_CHAIN (t);
	      break;
	    }
	}
    }

  return a;
}

/* Return the type that we should use to determine if DECL is
   imported or exported.  */

static tree
associated_type (decl)
     tree decl;
{
  tree t = NULL_TREE;

  /* In the C++ frontend, DECL_CONTEXT for a method doesn't actually refer
     to the containing class.  So we look at the 'this' arg.  */
  if (TREE_CODE (TREE_TYPE (decl)) == METHOD_TYPE)
    {
      /* Artificial methods are not affected by the import/export status of
	 their class unless they are virtual.  */
      if (! DECL_ARTIFICIAL (decl) || DECL_VINDEX (decl))
	t = TREE_TYPE (TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (decl))));
    }
  else if (DECL_CONTEXT (decl)
	   && TREE_CODE_CLASS (TREE_CODE (DECL_CONTEXT (decl))) == 't')
    t = DECL_CONTEXT (decl);

  return t;
}

/* Return non-zero if DECL is a dllexport'd object.  */

int
i386_pe_dllexport_p (decl)
     tree decl;
{
  tree exp;

  if (TREE_CODE (decl) != VAR_DECL
      && TREE_CODE (decl) != FUNCTION_DECL)
    return 0;
  exp = lookup_attribute ("dllexport", DECL_MACHINE_ATTRIBUTES (decl));
  if (exp)
    return 1;

  /* Class members get the dllexport status of their class.  */
  if (associated_type (decl))
    {
      exp = lookup_attribute ("dllexport",
			      TYPE_ATTRIBUTES (associated_type (decl)));
      if (exp)
	return 1;
    }

  return 0;
}

/* Return non-zero if DECL is a dllimport'd object.  */

int
i386_pe_dllimport_p (decl)
     tree decl;
{
  tree imp;

  if (TREE_CODE (decl) == FUNCTION_DECL
      && TARGET_NOP_FUN_DLLIMPORT)
    return 0;

  if (TREE_CODE (decl) != VAR_DECL
      && TREE_CODE (decl) != FUNCTION_DECL)
    return 0;
  imp = lookup_attribute ("dllimport", DECL_MACHINE_ATTRIBUTES (decl));
  if (imp)
    return 1;

  /* Class members get the dllimport status of their class.  */
  if (associated_type (decl))
    {
      imp = lookup_attribute ("dllimport",
			      TYPE_ATTRIBUTES (associated_type (decl)));
      if (imp)
	return 1;
    }

  return 0;
}

/* Return non-zero if SYMBOL is marked as being dllexport'd.  */

int
i386_pe_dllexport_name_p (symbol)
     char *symbol;
{
  return symbol[0] == '@' && symbol[1] == 'e' && symbol[2] == '.';
}

/* Return non-zero if SYMBOL is marked as being dllimport'd.  */

int
i386_pe_dllimport_name_p (symbol)
     char *symbol;
{
  return symbol[0] == '@' && symbol[1] == 'i' && symbol[2] == '.';
}

/* Mark a DECL as being dllexport'd.
   Note that we override the previous setting (eg: dllimport).  */

void
i386_pe_mark_dllexport (decl)
     tree decl;
{
  char *oldname, *newname;
  rtx rtlname;
  tree idp;

  rtlname = XEXP (DECL_RTL (decl), 0);
  if (GET_CODE (rtlname) == SYMBOL_REF)
    oldname = XSTR (rtlname, 0);
  else if (GET_CODE (rtlname) == MEM
	   && GET_CODE (XEXP (rtlname, 0)) == SYMBOL_REF)
    oldname = XSTR (XEXP (rtlname, 0), 0);
  else
    abort ();
  if (i386_pe_dllimport_name_p (oldname))
    oldname += 9;
  else if (i386_pe_dllexport_name_p (oldname))
    return; /* already done */

  newname = alloca (strlen (oldname) + 4);
  sprintf (newname, "@e.%s", oldname);

  /* We pass newname through get_identifier to ensure it has a unique
     address.  RTL processing can sometimes peek inside the symbol ref
     and compare the string's addresses to see if two symbols are
     identical.  */
  idp = get_identifier (newname);

  XEXP (DECL_RTL (decl), 0) =
    gen_rtx (SYMBOL_REF, Pmode, IDENTIFIER_POINTER (idp));
}

/* Mark a DECL as being dllimport'd.  */

void
i386_pe_mark_dllimport (decl)
     tree decl;
{
  char *oldname, *newname;
  tree idp;
  rtx rtlname, newrtl;

  rtlname = XEXP (DECL_RTL (decl), 0);
  if (GET_CODE (rtlname) == SYMBOL_REF)
    oldname = XSTR (rtlname, 0);
  else if (GET_CODE (rtlname) == MEM
	   && GET_CODE (XEXP (rtlname, 0)) == SYMBOL_REF)
    oldname = XSTR (XEXP (rtlname, 0), 0);
  else
    abort ();
  if (i386_pe_dllexport_name_p (oldname))
    {
      error ("`%s' declared as both exported to and imported from a DLL.",
             IDENTIFIER_POINTER (DECL_NAME (decl)));
      return;
    }
  else if (i386_pe_dllimport_name_p (oldname))
    {
      /* Already done, but force correct linkage since the redeclaration 
         might have omitted explicit extern.  Sigh.  */
      if (TREE_CODE (decl) == VAR_DECL
	  /* ??? Is this test for vtables needed?  */
	  && !DECL_VIRTUAL_P (decl))
	{
	  DECL_EXTERNAL (decl) = 1;
	  TREE_PUBLIC (decl) = 1;
	}
      return;
    }

  /* ??? One can well ask why we're making these checks here,
     and that would be a good question.  */

  /* Imported variables can't be initialized. Note that C++ classes
     are marked initial, so we need to check.  */
  if (TREE_CODE (decl) == VAR_DECL
      && !DECL_VIRTUAL_P (decl)
      && (DECL_INITIAL (decl)
          && ! TYPE_NEEDS_CONSTRUCTING (TREE_TYPE (decl))))
    {
      error_with_decl (decl, "initialized variable `%s' is marked dllimport");
      return;
    }
  /* Nor can they be static.  */
  if (TREE_CODE (decl) == VAR_DECL
      /* ??? Is this test for vtables needed?  */
      && !DECL_VIRTUAL_P (decl)
      && 0 /*???*/)
    {
      error_with_decl (decl, "static variable `%s' is marked dllimport");
      return;
    }

  /* `extern' needn't be specified with dllimport.
     Specify `extern' now and hope for the best.  Sigh.  */
  if (TREE_CODE (decl) == VAR_DECL
      /* ??? Is this test for vtables needed?  */
      && !DECL_VIRTUAL_P (decl))
    {
      DECL_EXTERNAL (decl) = 1;
      TREE_PUBLIC (decl) = 1;
    }

  newname = alloca (strlen (oldname) + 11);
  sprintf (newname, "@i._imp__%s", oldname);

  /* We pass newname through get_identifier to ensure it has a unique
     address.  RTL processing can sometimes peek inside the symbol ref
     and compare the string's addresses to see if two symbols are
     identical.  */
  idp = get_identifier (newname);

  newrtl = gen_rtx (MEM, Pmode,
		    gen_rtx (SYMBOL_REF, Pmode,
			     IDENTIFIER_POINTER (idp)));
  XEXP (DECL_RTL (decl), 0) = newrtl;

  /* Can't treat a pointer to this as a constant address */
  DECL_NON_ADDR_CONST_P (decl) = 1;
}

/* Return string which is the former assembler name modified with a 
   suffix consisting of an atsign (@) followed by the number of bytes of 
   arguments */

char *
gen_stdcall_suffix (decl)
  tree decl;
{
  int total = 0;
  /* ??? This probably should use XSTR (XEXP (DECL_RTL (decl), 0), 0) instead
     of DECL_ASSEMBLER_NAME.  */
  char *asmname = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
  char *newsym;

  if (TYPE_ARG_TYPES (TREE_TYPE (decl)))
    if (TREE_VALUE (tree_last (TYPE_ARG_TYPES (TREE_TYPE (decl)))) 
        == void_type_node)
      {
	tree formal_type = TYPE_ARG_TYPES (TREE_TYPE (decl));

	while (TREE_VALUE (formal_type) != void_type_node)
	  {
	    int parm_size
	      = TREE_INT_CST_LOW (TYPE_SIZE (TREE_VALUE (formal_type)));
	    /* Must round up to include padding.  This is done the same
	       way as in store_one_arg.  */
	    parm_size = ((parm_size + PARM_BOUNDARY - 1)
			 / PARM_BOUNDARY * PARM_BOUNDARY);
	    total += parm_size;
	    formal_type = TREE_CHAIN (formal_type);
	  }
      }

  newsym = xmalloc (strlen (asmname) + 10);
  sprintf (newsym, "%s@%d", asmname, total/BITS_PER_UNIT);
  return IDENTIFIER_POINTER (get_identifier (newsym));
}

/* Cover function to implement ENCODE_SECTION_INFO.  */

void
i386_pe_encode_section_info (decl)
     tree decl;
{
  /* This bit is copied from i386.h.  */
  if (optimize > 0 && TREE_CONSTANT (decl)
      && (!flag_writable_strings || TREE_CODE (decl) != STRING_CST))
    {
      rtx rtl = (TREE_CODE_CLASS (TREE_CODE (decl)) != 'd'
                 ? TREE_CST_RTL (decl) : DECL_RTL (decl));
      SYMBOL_REF_FLAG (XEXP (rtl, 0)) = 1;
    }

  if (TREE_CODE (decl) == FUNCTION_DECL)
    if (lookup_attribute ("stdcall",
			  TYPE_ATTRIBUTES (TREE_TYPE (decl))))
      XEXP (DECL_RTL (decl), 0) = 
	gen_rtx (SYMBOL_REF, Pmode, gen_stdcall_suffix (decl));

  /* Mark the decl so we can tell from the rtl whether the object is
     dllexport'd or dllimport'd.  */

  if (i386_pe_dllexport_p (decl))
    i386_pe_mark_dllexport (decl);
  else if (i386_pe_dllimport_p (decl))
    i386_pe_mark_dllimport (decl);
  /* It might be that DECL has already been marked as dllimport, but a
     subsequent definition nullified that.  The attribute is gone but
     DECL_RTL still has @i._imp__foo.  We need to remove that. Ditto
     for the DECL_NON_ADDR_CONST_P flag.  */
  else if ((TREE_CODE (decl) == FUNCTION_DECL
	    || TREE_CODE (decl) == VAR_DECL)
	   && DECL_RTL (decl) != NULL_RTX
	   && GET_CODE (DECL_RTL (decl)) == MEM
	   && GET_CODE (XEXP (DECL_RTL (decl), 0)) == MEM
	   && GET_CODE (XEXP (XEXP (DECL_RTL (decl), 0), 0)) == SYMBOL_REF
	   && i386_pe_dllimport_name_p (XSTR (XEXP (XEXP (DECL_RTL (decl), 0), 0), 0)))
    {
      char *oldname = XSTR (XEXP (XEXP (DECL_RTL (decl), 0), 0), 0);
      tree idp = get_identifier (oldname + 9);
      rtx newrtl = gen_rtx (SYMBOL_REF, Pmode, IDENTIFIER_POINTER (idp));

      XEXP (DECL_RTL (decl), 0) = newrtl;

      DECL_NON_ADDR_CONST_P (decl) = 0;

      /* We previously set TREE_PUBLIC and DECL_EXTERNAL.
	 We leave these alone for now.  */
    }
}

/* Cover function for UNIQUE_SECTION.  */

void
i386_pe_unique_section (decl, reloc)
     tree decl;
     int reloc;
{
  int len;
  char *name,*string,*prefix;

  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
  /* Strip off any encoding in fnname.  */
  STRIP_NAME_ENCODING (name, name);

  /* The object is put in, for example, section .text$foo.
     The linker will then ultimately place them in .text
     (everything from the $ on is stripped). Don't put
     read-only data in .rdata section to avoid a PE linker 
     bug when .rdata$* grouped sections are used in code
     without a .rdata section.  */
  if (TREE_CODE (decl) == FUNCTION_DECL)
    prefix = ".text$";
  else if (DECL_READONLY_SECTION (decl, reloc))
#ifdef READONLY_DATA_SECTION
    prefix = ".rdata$";
#else
    prefix = ".text$";
#endif
  else
    prefix = ".data$";
  len = strlen (name) + strlen (prefix);
  string = alloca (len + 1);
  sprintf (string, "%s%s", prefix, name);

  DECL_SECTION_NAME (decl) = build_string (len, string);
}

/* The Microsoft linker requires that every function be marked as
   DT_FCN.  When using gas on cygwin, we must emit appropriate .type
   directives.  */

#include "gsyms.h"

/* Mark a function appropriately.  This should only be called for
   functions for which we are not emitting COFF debugging information.
   FILE is the assembler output file, NAME is the name of the
   function, and PUBLIC is non-zero if the function is globally
   visible.  */

void
i386_pe_declare_function_type (file, name, public)
     FILE *file;
     char *name;
     int public;
{
  fprintf (file, "\t.def\t");
  assemble_name (file, name);
  fprintf (file, ";\t.scl\t%d;\t.type\t%d;\t.endef\n",
	   public ? (int) C_EXT : (int) C_STAT,
	   (int) DT_FCN << N_BTSHFT);
}

/* Keep a list of external functions.  */

struct extern_list
{
  struct extern_list *next;
  char *name;
};

static struct extern_list *extern_head;

/* Assemble an external function reference.  We need to keep a list of
   these, so that we can output the function types at the end of the
   assembly.  We can't output the types now, because we might see a
   definition of the function later on and emit debugging information
   for it then.  */

void
i386_pe_record_external_function (name)
     char *name;
{
  struct extern_list *p;

  p = (struct extern_list *) permalloc (sizeof *p);
  p->next = extern_head;
  p->name = name;
  extern_head = p;
}

static struct extern_list *exports_head;

/* Assemble an export symbol entry.  We need to keep a list of
   these, so that we can output the export list at the end of the
   assembly.  We used to output these export symbols in each function,
   but that causes problems with GNU ld when the sections are 
   linkonce.  */

void
i386_pe_record_exported_symbol (name)
     char *name;
{
  struct extern_list *p;

  p = (struct extern_list *) permalloc (sizeof *p);
  p->next = exports_head;
  p->name = name;
  exports_head = p;
}

/* This is called at the end of assembly.  For each external function
   which has not been defined, we output a declaration now.  We also
   output the .drectve section.  */

void
i386_pe_asm_file_end (file)
     FILE *file;
{
  struct extern_list *p;

  for (p = extern_head; p != NULL; p = p->next)
    {
      tree decl;

      decl = get_identifier (p->name);

      /* Positively ensure only one declaration for any given symbol.  */
      if (! TREE_ASM_WRITTEN (decl) && TREE_SYMBOL_REFERENCED (decl))
	{
	  TREE_ASM_WRITTEN (decl) = 1;
	  i386_pe_declare_function_type (file, p->name, TREE_PUBLIC (decl));
	}
    }

  if (exports_head)
    drectve_section ();
  for (p = exports_head; p != NULL; p = p->next)
    {
      fprintf (file, "\t.ascii \" -export:%s\"\n",
               I386_PE_STRIP_ENCODING (p->name));
    }
}

