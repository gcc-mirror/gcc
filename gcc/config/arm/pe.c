/* Routines for GCC for ARM/pe.
   Copyright (C) 1995, 1996, 2000 Free Software Foundation, Inc.
   Contributed by Doug Evans (dje@cygnus.com).

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
#include "output.h"
#include "flags.h"
#include "tree.h"
#include "expr.h"
#include "toplev.h"
#include "arm-protos.h"

extern int current_function_anonymous_args;

/* ARM/PE specific attribute support.

   ARM/PE has three new attributes:
   naked - for interrupt functions
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
arm_pe_valid_machine_decl_attribute (decl, attributes, attr, args)
     tree decl;
     tree attributes ATTRIBUTE_UNUSED;
     tree attr;
     tree args;
{
  if (args != NULL_TREE)
    return 0;

  if (is_attribute_p ("dllexport", attr))
    return 1;
  if (is_attribute_p ("dllimport", attr))
    return 1;

  return arm_valid_machine_decl_attribute (decl, attr, args);
}

/* Merge attributes in decls OLD and NEW.

   This handles the following situation:

   __declspec (dllimport) int foo;
   int foo;

   The second instance of `foo' nullifies the dllimport.  */

tree
arm_pe_merge_machine_decl_attributes (old, new)
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

#if 0
/* Check a type that has a virtual table, and see if any virtual methods are
   marked for import or export, and if so, arrange for the vtable to
   be imported or exported.  */

static int
arm_check_vtable_importexport (type)
     tree type;
{
  tree methods = TYPE_METHODS (type);
  tree fndecl;

  if (TREE_CODE (methods) == FUNCTION_DECL)
    fndecl = methods;
  else if (TREE_VEC_ELT (methods, 0) != NULL_TREE)
    fndecl = TREE_VEC_ELT (methods, 0);
  else
    fndecl = TREE_VEC_ELT (methods, 1);

  while (fndecl)
    {
      if (DECL_VIRTUAL_P (fndecl) || DECL_VINDEX (fndecl) != NULL_TREE)
	{
	  tree exp = lookup_attribute ("dllimport",
				       DECL_MACHINE_ATTRIBUTES (fndecl));
	  if (exp == 0)
	    exp = lookup_attribute ("dllexport",
				    DECL_MACHINE_ATTRIBUTES (fndecl));
	  if (exp)
	    return 1;
	}

      fndecl = TREE_CHAIN (fndecl);
    }

  return 0;
}
#endif

/* Return non-zero if DECL is a dllexport'd object.  */

tree current_class_type; /* FIXME */

int
arm_dllexport_p (decl)
     tree decl;
{
  tree exp;

  if (TREE_CODE (decl) != VAR_DECL
      && TREE_CODE (decl) != FUNCTION_DECL)
    return 0;
  exp = lookup_attribute ("dllexport", DECL_MACHINE_ATTRIBUTES (decl));
  if (exp)
    return 1;

#if 0 /* This was a hack to get vtable's exported or imported since only one
	 copy of them is ever output.  Disabled pending better solution.  */
  /* For C++, the vtables might have to be marked.  */
  if (TREE_CODE (decl) == VAR_DECL && DECL_VIRTUAL_P (decl))
    {
      if (TREE_PUBLIC (decl)
	  && DECL_EXTERNAL (decl) == 0
	  && (DECL_CONTEXT (decl)
	      ? arm_check_vtable_importexport (DECL_CONTEXT (decl))
	      : current_class_type
	      ? arm_check_vtable_importexport (current_class_type)
	      : 0)
	  )
	return 1;
    }
#endif

  return 0;
}

/* Return non-zero if DECL is a dllimport'd object.  */

int
arm_dllimport_p (decl)
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

#if 0 /* This was a hack to get vtable's exported or imported since only one
	 copy of them is ever output.  Disabled pending better solution.  */
  /* For C++, the vtables might have to be marked.  */
  if (TREE_CODE (decl) == VAR_DECL && DECL_VIRTUAL_P (decl))
    {
      if (TREE_PUBLIC (decl)
	  && DECL_EXTERNAL (decl)
	  && (DECL_CONTEXT (decl)
	      ? arm_check_vtable_importexport (DECL_CONTEXT (decl))
	      : current_class_type
	      ? arm_check_vtable_importexport (current_class_type)
	      : 0)
	  )
	return 1;
    }
#endif

  return 0;
}

/* Return non-zero if SYMBOL is marked as being dllexport'd.  */

int
arm_dllexport_name_p (symbol)
     char * symbol;
{
  return symbol[0] == ARM_PE_FLAG_CHAR && symbol[1] == 'e' && symbol[2] == '.';
}

/* Return non-zero if SYMBOL is marked as being dllimport'd.  */

int
arm_dllimport_name_p (symbol)
     char * symbol;
{
  return symbol[0] == ARM_PE_FLAG_CHAR && symbol[1] == 'i' && symbol[2] == '.';
}

/* Mark a DECL as being dllexport'd.
   Note that we override the previous setting (eg: dllimport).  */

void
arm_mark_dllexport (decl)
     tree decl;
{
  char * oldname;
  char * newname;
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
  if (arm_dllimport_name_p (oldname))
    oldname += 9;
  else if (arm_dllexport_name_p (oldname))
    return; /* already done */

  newname = alloca (strlen (oldname) + 4);
  sprintf (newname, "%ce.%s", ARM_PE_FLAG_CHAR, oldname);

  /* We pass newname through get_identifier to ensure it has a unique
     address.  RTL processing can sometimes peek inside the symbol ref
     and compare the string's addresses to see if two symbols are
     identical.  */
  /* ??? At least I think that's why we do this.  */
  idp = get_identifier (newname);

  XEXP (DECL_RTL (decl), 0) =
    gen_rtx (SYMBOL_REF, Pmode, IDENTIFIER_POINTER (idp));
}

/* Mark a DECL as being dllimport'd.  */

void
arm_mark_dllimport (decl)
     tree decl;
{
  char * oldname;
  char * newname;
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
  
  if (arm_dllexport_name_p (oldname))
    abort (); /* this shouldn't happen */
  else if (arm_dllimport_name_p (oldname))
    return; /* already done */

  /* ??? One can well ask why we're making these checks here,
     and that would be a good question.  */

  /* Imported variables can't be initialized.  */
  if (TREE_CODE (decl) == VAR_DECL
      && !DECL_VIRTUAL_P (decl)
      && DECL_INITIAL (decl))
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
  sprintf (newname, "%ci.__imp_%s", ARM_PE_FLAG_CHAR, oldname);

  /* We pass newname through get_identifier to ensure it has a unique
     address.  RTL processing can sometimes peek inside the symbol ref
     and compare the string's addresses to see if two symbols are
     identical.  */
  /* ??? At least I think that's why we do this.  */
  idp = get_identifier (newname);

  newrtl = gen_rtx (MEM, Pmode,
		    gen_rtx (SYMBOL_REF, Pmode,
			     IDENTIFIER_POINTER (idp)));
  XEXP (DECL_RTL (decl), 0) = newrtl;
}

/* Cover function to implement ENCODE_SECTION_INFO.  */

void
arm_pe_encode_section_info (decl)
     tree decl;
{
  /* This bit is copied from arm.h.  */
  if (optimize > 0 && TREE_CONSTANT (decl)
      && (!flag_writable_strings || TREE_CODE (decl) != STRING_CST))
    {
      rtx rtl = (TREE_CODE_CLASS (TREE_CODE (decl)) != 'd'
                 ? TREE_CST_RTL (decl) : DECL_RTL (decl));
      SYMBOL_REF_FLAG (XEXP (rtl, 0)) = 1;
    }

  /* Mark the decl so we can tell from the rtl whether the object is
     dllexport'd or dllimport'd.  */
  if (arm_dllexport_p (decl))
    arm_mark_dllexport (decl);
  else if (arm_dllimport_p (decl))
    arm_mark_dllimport (decl);
  /* It might be that DECL has already been marked as dllimport, but a
     subsequent definition nullified that.  The attribute is gone but
     DECL_RTL still has @i.__imp_foo.  We need to remove that.  */
  else if ((TREE_CODE (decl) == FUNCTION_DECL
	    || TREE_CODE (decl) == VAR_DECL)
	   && DECL_RTL (decl) != NULL_RTX
	   && GET_CODE (DECL_RTL (decl)) == MEM
	   && GET_CODE (XEXP (DECL_RTL (decl), 0)) == MEM
	   && GET_CODE (XEXP (XEXP (DECL_RTL (decl), 0), 0)) == SYMBOL_REF
	   && arm_dllimport_name_p (XSTR (XEXP (XEXP (DECL_RTL (decl), 0), 0), 0)))
    {
      char *oldname = XSTR (XEXP (XEXP (DECL_RTL (decl), 0), 0), 0);
      tree idp = get_identifier (oldname + 9);
      rtx newrtl = gen_rtx (SYMBOL_REF, Pmode, IDENTIFIER_POINTER (idp));

      XEXP (DECL_RTL (decl), 0) = newrtl;

      /* We previously set TREE_PUBLIC and DECL_EXTERNAL.
	 ??? We leave these alone for now.  */
    }
}

/* Cover function for UNIQUE_SECTION.  */

void
arm_pe_unique_section (decl, reloc)
     tree decl;
     int reloc;
{
  int len;
  const char * name;
  char * string;
  char * prefix;

  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
  /* Strip off any encoding in fnname.  */
  STRIP_NAME_ENCODING (name, name);

  /* The object is put in, for example, section .text$foo.
     The linker will then ultimately place them in .text
     (everything from the $ on is stripped).  */
  if (TREE_CODE (decl) == FUNCTION_DECL)
    prefix = ".text$";
  else if (DECL_READONLY_SECTION (decl, reloc))
    prefix = ".rdata$";
  else
    prefix = ".data$";
  len = strlen (name) + strlen (prefix);
  string = alloca (len + 1);
  sprintf (string, "%s%s", prefix, name);

  DECL_SECTION_NAME (decl) = build_string (len, string);
}

/* This is to better conform to the ARM PCS.
   Richard Earnshaw hasn't put this into FSF sources yet so it's here.  */

int
arm_pe_return_in_memory (type)
     tree type;
{
  if (TREE_CODE (type) == RECORD_TYPE)
    {
      tree field;
      int num_fields = 0;

      /* For a record containing just a single element, we can be a little
	 less restrictive.  */
      for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	{
	  if (TREE_CODE (field) == FIELD_DECL && ! TREE_STATIC (field))
	    {
	      if ((AGGREGATE_TYPE_P (TREE_TYPE (field))
		   && RETURN_IN_MEMORY (TREE_TYPE (field)))
		  || FLOAT_TYPE_P (TREE_TYPE (field)))
		return 1;
	      num_fields++;
	    }
	}

      if (num_fields == 1)
	return 0;
	    
      /* For a struct, we can return in a register if every element was a
	 bit-field and it all fits in one word.  */
      for (field = TYPE_FIELDS (type); field;  field = TREE_CHAIN (field))
	{
	  if (TREE_CODE (field) == FIELD_DECL
	      && ! TREE_STATIC (field)
	      && (! DECL_BIT_FIELD_TYPE (field)
		  || (TREE_INT_CST_LOW (DECL_FIELD_BITPOS (field))
		      + TREE_INT_CST_LOW (DECL_SIZE (field))) > 32))
	    return 1;
	}
      return 0;
    }
  else if (TREE_CODE (type) == UNION_TYPE
	   || TREE_CODE (type) == QUAL_UNION_TYPE)
    {
      tree field;

      /* Unions can be returned in registers if every element is
	 integral, or can be returned in an integer register.  */
      for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	{
	  if (TREE_CODE (field) == FIELD_DECL
	      && ! TREE_STATIC (field)
	      && ((AGGREGATE_TYPE_P (TREE_TYPE (field))
		   && RETURN_IN_MEMORY (TREE_TYPE (field)))
		  || FLOAT_TYPE_P (TREE_TYPE (field))))
	    return 1;
	}
      return 0;
    }
  /* XXX Not sure what should be done for other aggregates, so put them in
     memory. */
  return 1;
}
