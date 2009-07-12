/* Subroutines for insn-output.c for Windows NT.
   Contributed by Douglas Rupp (drupp@cs.washington.edu)
   Copyright (C) 1995, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
   2005, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "output.h"
#include "tree.h"
#include "flags.h"
#include "tm_p.h"
#include "toplev.h"
#include "hashtab.h"
#include "langhooks.h"
#include "ggc.h"
#include "target.h"

/* i386/PE specific attribute support.

   i386/PE has two new attributes:
   dllexport - for exporting a function/variable that will live in a dll
   dllimport - for importing a function/variable from a dll

   Microsoft allows multiple declspecs in one __declspec, separating
   them with spaces.  We do NOT support this.  Instead, use __declspec
   multiple times.
*/

/* Handle a "shared" attribute;
   arguments as in struct attribute_spec.handler.  */
tree
ix86_handle_shared_attribute (tree *node, tree name,
			      tree args ATTRIBUTE_UNUSED,
			      int flags ATTRIBUTE_UNUSED, bool *no_add_attrs)
{
  if (TREE_CODE (*node) != VAR_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute only applies to variables",
	       name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "selectany" attribute;
   arguments as in struct attribute_spec.handler.  */
tree
ix86_handle_selectany_attribute (tree *node, tree name,
			         tree args ATTRIBUTE_UNUSED,
			         int flags ATTRIBUTE_UNUSED,
				 bool *no_add_attrs)
{
  /* The attribute applies only to objects that are initialized and have
     external linkage.  However, we may not know about initialization
     until the language frontend has processed the decl. We'll check for
     initialization later in encode_section_info.  */
  if (TREE_CODE (*node) != VAR_DECL || !TREE_PUBLIC (*node))
    {	
      error ("%qE attribute applies only to initialized variables"
       	     " with external linkage", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}


/* Return the type that we should use to determine if DECL is
   imported or exported.  */

static tree
associated_type (tree decl)
{
  return (DECL_CONTEXT (decl) && TYPE_P (DECL_CONTEXT (decl))
          ?  DECL_CONTEXT (decl) : NULL_TREE);
}

/* Return true if DECL should be a dllexport'd object.  */

static bool
i386_pe_determine_dllexport_p (tree decl)
{
  tree assoc;

  if (TREE_CODE (decl) != VAR_DECL && TREE_CODE (decl) != FUNCTION_DECL)
    return false;

  /* Don't export local clones of dllexports.  */
  if (!TREE_PUBLIC (decl))
    return false;

  if (lookup_attribute ("dllexport", DECL_ATTRIBUTES (decl)))
    return true;

  /* Also mark class members of exported classes with dllexport.  */
  assoc = associated_type (decl);
  if (assoc && lookup_attribute ("dllexport", TYPE_ATTRIBUTES (assoc)))
    return i386_pe_type_dllexport_p (decl);

  return false;
}

/* Return true if DECL should be a dllimport'd object.  */

static bool
i386_pe_determine_dllimport_p (tree decl)
{
  tree assoc;

  if (TREE_CODE (decl) != VAR_DECL && TREE_CODE (decl) != FUNCTION_DECL)
    return false;

  /* Lookup the attribute in addition to checking the DECL_DLLIMPORT_P flag.
     We may need to override an earlier decision.  */
  if (DECL_DLLIMPORT_P (decl))
    return true;

  /* The DECL_DLLIMPORT_P flag was set for decls in the class definition
     by  targetm.cxx.adjust_class_at_definition.  Check again to emit
     warnings if the class attribute has been overridden by an
     out-of-class definition.  */
  assoc = associated_type (decl);
  if (assoc && lookup_attribute ("dllimport", TYPE_ATTRIBUTES (assoc)))
    return i386_pe_type_dllimport_p (decl);

  return false;
}

/* Handle the -mno-fun-dllimport target switch.  */

bool
i386_pe_valid_dllimport_attribute_p (const_tree decl)
{
   if (TARGET_NOP_FUN_DLLIMPORT && TREE_CODE (decl) == FUNCTION_DECL)
     return false;
   return true;
}

/* Return string which is the function name, identified by ID, modified
   with a suffix consisting of an atsign (@) followed by the number of
   bytes of arguments.  If ID is NULL use the DECL_NAME as base. If
   FASTCALL is true, also add the FASTCALL_PREFIX.
   Return NULL if no change required.  */

static tree
gen_stdcall_or_fastcall_suffix (tree decl, tree id, bool fastcall)
{
  HOST_WIDE_INT total = 0;
  const char *old_str = IDENTIFIER_POINTER (id != NULL_TREE ? id : DECL_NAME (decl));
  char *new_str, *p;
  tree type = TREE_TYPE (decl);
  tree arg;
  function_args_iterator args_iter;

  gcc_assert (TREE_CODE (decl) == FUNCTION_DECL);  

  if (prototype_p (type))
    {
      /* This attribute is ignored for variadic functions.  */ 
      if (stdarg_p (type))
	return NULL_TREE;

      /* Quit if we hit an incomplete type.  Error is reported
	 by convert_arguments in c-typeck.c or cp/typeck.c.  */
      FOREACH_FUNCTION_ARGS(type, arg, args_iter)
	{
	  HOST_WIDE_INT parm_size;
	  HOST_WIDE_INT parm_boundary_bytes = PARM_BOUNDARY / BITS_PER_UNIT;

	  if (! COMPLETE_TYPE_P (arg))
	    break;

	  parm_size = int_size_in_bytes (arg);
	  if (parm_size < 0)
	    break;

	  /* Must round up to include padding.  This is done the same
	     way as in store_one_arg.  */
	  parm_size = ((parm_size + parm_boundary_bytes - 1)
		       / parm_boundary_bytes * parm_boundary_bytes);
	  total += parm_size;
	}
      }
  /* Assume max of 8 base 10 digits in the suffix.  */
  p = new_str = XALLOCAVEC (char, 1 + strlen (old_str) + 1 + 8 + 1);
  if (fastcall)
    *p++ = FASTCALL_PREFIX;
  sprintf (p, "%s@" HOST_WIDE_INT_PRINT_DEC, old_str, total);

  return get_identifier (new_str);
}

/* Maybe decorate and get a new identifier for the DECL of a stdcall or
   fastcall function. The original identifier is supplied in ID. */

static tree
i386_pe_maybe_mangle_decl_assembler_name (tree decl, tree id)
{
  tree new_id = NULL_TREE;

  if (TREE_CODE (decl) == FUNCTION_DECL)
    { 
      tree type_attributes = TYPE_ATTRIBUTES (TREE_TYPE (decl));
      if (lookup_attribute ("stdcall", type_attributes))
	new_id = gen_stdcall_or_fastcall_suffix (decl, id, false);
      else if (lookup_attribute ("fastcall", type_attributes))
	new_id = gen_stdcall_or_fastcall_suffix (decl, id, true);
    }

  return new_id;
}

/* This is used as a target hook to modify the DECL_ASSEMBLER_NAME
   in the language-independent default hook
   langhooks,c:lhd_set_decl_assembler_name ()
   and in cp/mangle,c:mangle_decl ().  */
tree
i386_pe_mangle_decl_assembler_name (tree decl, tree id)
{
  tree new_id = i386_pe_maybe_mangle_decl_assembler_name (decl, id);   

  return (new_id ? new_id : id);
}

void
i386_pe_encode_section_info (tree decl, rtx rtl, int first)
{
  rtx symbol;
  int flags;

  /* Do this last, due to our frobbing of DECL_DLLIMPORT_P above.  */
  default_encode_section_info (decl, rtl, first);

  /* Careful not to prod global register variables.  */
  if (!MEM_P (rtl))
    return;

  symbol = XEXP (rtl, 0);
  gcc_assert (GET_CODE (symbol) == SYMBOL_REF);

  switch (TREE_CODE (decl))
    {
    case FUNCTION_DECL:
      /* FIXME:  Imported stdcall names are not modified by the Ada frontend.
	 Check and decorate the RTL name now.  */
      if  (strcmp (lang_hooks.name, "GNU Ada") == 0)
	{
	  tree new_id;
	  tree old_id = DECL_ASSEMBLER_NAME (decl);
	  const char* asm_str = IDENTIFIER_POINTER (old_id);
	  /* Do not change the identifier if a verbatim asmspec
	     or if stdcall suffix already added. */
	  if (!(*asm_str == '*' || strchr (asm_str, '@'))
	      && (new_id = i386_pe_maybe_mangle_decl_assembler_name (decl,
								     old_id)))
	    XSTR (symbol, 0) = IDENTIFIER_POINTER (new_id);
	}
      break;

    case VAR_DECL:
      if (lookup_attribute ("selectany", DECL_ATTRIBUTES (decl)))
	{
	  if (DECL_INITIAL (decl)
	      /* If an object is initialized with a ctor, the static
		 initialization and destruction code for it is present in
		 each unit defining the object.  The code that calls the
		 ctor is protected by a link-once guard variable, so that
		 the object still has link-once semantics,  */
	      || TYPE_NEEDS_CONSTRUCTING (TREE_TYPE (decl)))
	    make_decl_one_only (decl, DECL_ASSEMBLER_NAME (decl));
	  else
	    error ("%q+D:'selectany' attribute applies only to "
		   "initialized objects", decl);
	}
      break;

    default:
      return;
    }

  /* Mark the decl so we can tell from the rtl whether the object is
     dllexport'd or dllimport'd.  tree.c: merge_dllimport_decl_attributes
     handles dllexport/dllimport override semantics.  */
  flags = (SYMBOL_REF_FLAGS (symbol) &
	   ~(SYMBOL_FLAG_DLLIMPORT | SYMBOL_FLAG_DLLEXPORT));
  if (i386_pe_determine_dllexport_p (decl))
    flags |= SYMBOL_FLAG_DLLEXPORT;
  else if (i386_pe_determine_dllimport_p (decl))
    {
      flags |= SYMBOL_FLAG_DLLIMPORT;
      /* If we went through the associated_type path, this won't already
	 be set.  Though, frankly, this seems wrong, and should be fixed
	 elsewhere.  */
      if (!DECL_DLLIMPORT_P (decl))
	{
	  DECL_DLLIMPORT_P (decl) = 1;
	  flags &= ~SYMBOL_FLAG_LOCAL;
	}
    }
  SYMBOL_REF_FLAGS (symbol) = flags;
}

bool
i386_pe_binds_local_p (const_tree exp)
{
  /* PE does not do dynamic binding.  Indeed, the only kind of
     non-local reference comes from a dllimport'd symbol.  */
  if ((TREE_CODE (exp) == VAR_DECL || TREE_CODE (exp) == FUNCTION_DECL)
      && DECL_DLLIMPORT_P (exp))
    return false;

  return true;
}

/* Also strip the fastcall prefix and stdcall suffix.  */

const char *
i386_pe_strip_name_encoding_full (const char *str)
{
  const char *p;
  const char *name = default_strip_name_encoding (str);

  /* Strip leading '@' on fastcall symbols.  */
  if (*name == '@')
    name++;

  /* Strip trailing "@n".  */
  p = strchr (name, '@');
  if (p)
    return ggc_alloc_string (name, p - name);

  return name;
}

void
i386_pe_unique_section (tree decl, int reloc)
{
  int len;
  const char *name, *prefix;
  char *string;

  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
  name = i386_pe_strip_name_encoding_full (name);

  /* The object is put in, for example, section .text$foo.
     The linker will then ultimately place them in .text
     (everything from the $ on is stripped). Don't put
     read-only data in .rdata section to avoid a PE linker
     bug when .rdata$* grouped sections are used in code
     without a .rdata section.  */
  if (TREE_CODE (decl) == FUNCTION_DECL)
    prefix = ".text$";
  else if (decl_readonly_section (decl, reloc))
    prefix = ".rdata$";
  else
    prefix = ".data$";
  len = strlen (name) + strlen (prefix);
  string = XALLOCAVEC (char, len + 1);
  sprintf (string, "%s%s", prefix, name);

  DECL_SECTION_NAME (decl) = build_string (len, string);
}

/* Select a set of attributes for section NAME based on the properties
   of DECL and whether or not RELOC indicates that DECL's initializer
   might contain runtime relocations.

   We make the section read-only and executable for a function decl,
   read-only for a const data decl, and writable for a non-const data decl.

   If the section has already been defined, to not allow it to have
   different attributes, as (1) this is ambiguous since we're not seeing
   all the declarations up front and (2) some assemblers (e.g. SVR4)
   do not recognize section redefinitions.  */
/* ??? This differs from the "standard" PE implementation in that we
   handle the SHARED variable attribute.  Should this be done for all
   PE targets?  */

#define SECTION_PE_SHARED	SECTION_MACH_DEP

unsigned int
i386_pe_section_type_flags (tree decl, const char *name, int reloc)
{
  static htab_t htab;
  unsigned int flags;
  unsigned int **slot;

  /* The names we put in the hashtable will always be the unique
     versions given to us by the stringtable, so we can just use
     their addresses as the keys.  */
  if (!htab)
    htab = htab_create (31, htab_hash_pointer, htab_eq_pointer, NULL);

  if (decl && TREE_CODE (decl) == FUNCTION_DECL)
    flags = SECTION_CODE;
  else if (decl && decl_readonly_section (decl, reloc))
    flags = 0;
  else if (current_function_decl
	   && cfun
	   && crtl->subsections.unlikely_text_section_name
	   && strcmp (name, crtl->subsections.unlikely_text_section_name) == 0)
    flags = SECTION_CODE;
  else if (!decl
	   && (!current_function_decl || !cfun)
	   && strcmp (name, UNLIKELY_EXECUTED_TEXT_SECTION_NAME) == 0)
    flags = SECTION_CODE;
  else
    {
      flags = SECTION_WRITE;

      if (decl && TREE_CODE (decl) == VAR_DECL
	  && lookup_attribute ("shared", DECL_ATTRIBUTES (decl)))
	flags |= SECTION_PE_SHARED;
    }

  if (decl && DECL_ONE_ONLY (decl))
    flags |= SECTION_LINKONCE;

  /* See if we already have an entry for this section.  */
  slot = (unsigned int **) htab_find_slot (htab, name, INSERT);
  if (!*slot)
    {
      *slot = (unsigned int *) xmalloc (sizeof (unsigned int));
      **slot = flags;
    }
  else
    {
      if (decl && **slot != flags)
	error ("%q+D causes a section type conflict", decl);
    }

  return flags;
}

void
i386_pe_asm_named_section (const char *name, unsigned int flags, 
			   tree decl)
{
  char flagchars[8], *f = flagchars;

  if ((flags & (SECTION_CODE | SECTION_WRITE)) == 0)
    /* readonly data */
    {
      *f++ ='d';  /* This is necessary for older versions of gas.  */
      *f++ ='r';
    }
  else	
    {
      if (flags & SECTION_CODE)
        *f++ = 'x';
      if (flags & SECTION_WRITE)
        *f++ = 'w';
      if (flags & SECTION_PE_SHARED)
        *f++ = 's';
    }

  *f = '\0';

  fprintf (asm_out_file, "\t.section\t%s,\"%s\"\n", name, flagchars);

  if (flags & SECTION_LINKONCE)
    {
      /* Functions may have been compiled at various levels of
	 optimization so we can't use `same_size' here.
	 Instead, have the linker pick one, without warning.
	 If 'selectany' attribute has been specified,  MS compiler
	 sets 'discard' characteristic, rather than telling linker
	 to warn of size or content mismatch, so do the same.  */ 
      bool discard = (flags & SECTION_CODE)
		      || lookup_attribute ("selectany",
					   DECL_ATTRIBUTES (decl));	 
      fprintf (asm_out_file, "\t.linkonce %s\n",
	       (discard  ? "discard" : "same_size"));
    }
}

void
i386_pe_asm_output_aligned_decl_common (FILE *stream, tree decl,
					const char *name, HOST_WIDE_INT size,
					HOST_WIDE_INT align ATTRIBUTE_UNUSED)
{
  HOST_WIDE_INT rounded;

  /* Compute as in assemble_noswitch_variable, since we don't have
     support for aligned common on older binutils.  We must also
     avoid emitting a common symbol of size zero, as this is the
     overloaded representation that indicates an undefined external
     symbol in the PE object file format.  */
  rounded = size ? size : 1;
  rounded += (BIGGEST_ALIGNMENT / BITS_PER_UNIT) - 1;
  rounded = (rounded / (BIGGEST_ALIGNMENT / BITS_PER_UNIT)
	     * (BIGGEST_ALIGNMENT / BITS_PER_UNIT));
  
  i386_pe_maybe_record_exported_symbol (decl, name, 1);

  fprintf (stream, "\t.comm\t");
  assemble_name (stream, name);
  if (use_pe_aligned_common)
    fprintf (stream, ", " HOST_WIDE_INT_PRINT_DEC ", %d\n",
	   size ? size : (HOST_WIDE_INT) 1,
	   exact_log2 (align) - exact_log2 (CHAR_BIT));
  else
    fprintf (stream, ", " HOST_WIDE_INT_PRINT_DEC "\t" ASM_COMMENT_START
	   " " HOST_WIDE_INT_PRINT_DEC "\n", rounded, size);
}

/* The Microsoft linker requires that every function be marked as
   DT_FCN.  When using gas on cygwin, we must emit appropriate .type
   directives.  */

#include "gsyms.h"

/* Mark a function appropriately.  This should only be called for
   functions for which we are not emitting COFF debugging information.
   FILE is the assembler output file, NAME is the name of the
   function, and PUB is nonzero if the function is globally
   visible.  */

void
i386_pe_declare_function_type (FILE *file, const char *name, int pub)
{
  fprintf (file, "\t.def\t");
  assemble_name (file, name);
  fprintf (file, ";\t.scl\t%d;\t.type\t%d;\t.endef\n",
	   pub ? (int) C_EXT : (int) C_STAT,
	   (int) DT_FCN << N_BTSHFT);
}

/* Keep a list of external functions.  */

struct GTY(()) extern_list
{
  struct extern_list *next;
  tree decl;
  const char *name;
};

static GTY(()) struct extern_list *extern_head;

/* Assemble an external function reference.  We need to keep a list of
   these, so that we can output the function types at the end of the
   assembly.  We can't output the types now, because we might see a
   definition of the function later on and emit debugging information
   for it then.  */

void
i386_pe_record_external_function (tree decl, const char *name)
{
  struct extern_list *p;

  p = (struct extern_list *) ggc_alloc (sizeof *p);
  p->next = extern_head;
  p->decl = decl;
  p->name = name;
  extern_head = p;
}

/* Keep a list of exported symbols.  */

struct GTY(()) export_list
{
  struct export_list *next;
  const char *name;
  int is_data;		/* used to type tag exported symbols.  */
};

static GTY(()) struct export_list *export_head;

/* Assemble an export symbol entry.  We need to keep a list of
   these, so that we can output the export list at the end of the
   assembly.  We used to output these export symbols in each function,
   but that causes problems with GNU ld when the sections are
   linkonce.  */

void
i386_pe_maybe_record_exported_symbol (tree decl, const char *name, int is_data)
{
  rtx symbol;
  struct export_list *p;

  symbol = XEXP (DECL_RTL (decl), 0);
  gcc_assert (GET_CODE (symbol) == SYMBOL_REF);
  if (!SYMBOL_REF_DLLEXPORT_P (symbol))
    return;

  gcc_assert (TREE_PUBLIC (decl));

  p = (struct export_list *) ggc_alloc (sizeof *p);
  p->next = export_head;
  p->name = name;
  p->is_data = is_data;
  export_head = p;
}

/* This is called at the end of assembly.  For each external function
   which has not been defined, we output a declaration now.  We also
   output the .drectve section.  */

void
i386_pe_file_end (void)
{
  struct extern_list *p;

  ix86_file_end ();

  for (p = extern_head; p != NULL; p = p->next)
    {
      tree decl;

      decl = p->decl;

      /* Positively ensure only one declaration for any given symbol.  */
      if (! TREE_ASM_WRITTEN (decl)
	  && TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl)))
	{
	  TREE_ASM_WRITTEN (decl) = 1;
	  i386_pe_declare_function_type (asm_out_file, p->name,
					 TREE_PUBLIC (decl));
	}
    }

  if (export_head)
    {
      struct export_list *q;
      drectve_section ();
      for (q = export_head; q != NULL; q = q->next)
	{
	  fprintf (asm_out_file, "\t.ascii \" -export:%s%s\"\n",
		   default_strip_name_encoding (q->name),
		   (q->is_data ? ",data" : ""));
	}
    }
}

#include "gt-winnt.h"
