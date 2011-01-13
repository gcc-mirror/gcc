/* Subroutines for insn-output.c for Windows NT.
   Contributed by Douglas Rupp (drupp@cs.washington.edu)
   Copyright (C) 1995, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
   2005, 2006, 2007, 2008, 2009, 2010 Free Software Foundation, Inc.

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
#include "diagnostic-core.h"
#include "hashtab.h"
#include "langhooks.h"
#include "ggc.h"
#include "target.h"
#include "except.h"
#include "lto-streamer.h"

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
  if (TREE_CODE (decl) != VAR_DECL && TREE_CODE (decl) != FUNCTION_DECL)
    return false;

  /* Don't export local clones of dllexports.  */
  if (!TREE_PUBLIC (decl))
    return false;

  if (lookup_attribute ("dllexport", DECL_ATTRIBUTES (decl)))
    return true;

  return false;
}

/* Return true if DECL should be a dllimport'd object.  */

static bool
i386_pe_determine_dllimport_p (tree decl)
{
  tree assoc;

  if (TREE_CODE (decl) != VAR_DECL && TREE_CODE (decl) != FUNCTION_DECL)
    return false;

  if (DECL_DLLIMPORT_P (decl))
    return true;

  /* The DECL_DLLIMPORT_P flag was set for decls in the class definition
     by  targetm.cxx.adjust_class_at_definition.  Check again to emit
     error message if the class attribute has been overridden by an
     out-of-class definition of static data.  */
  assoc = associated_type (decl);
  if (assoc && lookup_attribute ("dllimport", TYPE_ATTRIBUTES (assoc))
      && TREE_CODE (decl) == VAR_DECL
      && TREE_STATIC (decl) && TREE_PUBLIC (decl)
      && !DECL_EXTERNAL (decl)
      /* vtable's are linkonce constants, so defining a vtable is not
	 an error as long as we don't try to import it too.  */
      && !DECL_VIRTUAL_P (decl))
	error ("definition of static data member %q+D of "
	       "dllimport%'d class", decl);

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

/* Emit an assembler directive to set symbol for DECL visibility to
   the visibility type VIS, which must not be VISIBILITY_DEFAULT.
   As for PE there is no hidden support in gas, we just warn for
   user-specified visibility attributes.  */

void
i386_pe_assemble_visibility (tree decl,
			     int vis ATTRIBUTE_UNUSED)
{
  if (!decl
      || !lookup_attribute ("visibility", DECL_ATTRIBUTES (decl)))
    return;
  warning (OPT_Wattributes, "visibility attribute not supported "
	   "in this configuration; ignored");
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

/* This hook behaves the same as varasm.c/assemble_name(), but
   generates the name into memory rather than outputting it to
   a file stream.  */

tree
i386_pe_mangle_assembler_name (const char *name ATTRIBUTE_UNUSED)
{
  const char *skipped = name + (*name == '*' ? 1 : 0);
  const char *stripped = targetm.strip_name_encoding (skipped);
  if (*name != '*' && *user_label_prefix && *stripped != FASTCALL_PREFIX)
    stripped = ACONCAT ((user_label_prefix, stripped, NULL));
  return get_identifier (stripped);
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
    flags |= SYMBOL_FLAG_DLLIMPORT;
 
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

  /* Or a weak one, now that they are supported.  */
  if ((TREE_CODE (exp) == VAR_DECL || TREE_CODE (exp) == FUNCTION_DECL)
      && DECL_WEAK (exp))
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

  /* LTO sections need 1-byte alignment to avoid confusing the
     zlib decompression algorithm with trailing zero pad bytes.  */
  if (strncmp (name, LTO_SECTION_NAME_PREFIX,
			strlen (LTO_SECTION_NAME_PREFIX)) == 0)
    *f++ = '0';

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

/* Beware, DECL may be NULL if compile_file() is emitting the LTO marker.  */

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

  p = ggc_alloc_extern_list ();
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
   linkonce.  Beware, DECL may be NULL if compile_file() is emitting
   the LTO marker.  */

void
i386_pe_maybe_record_exported_symbol (tree decl, const char *name, int is_data)
{
  rtx symbol;
  struct export_list *p;

  if (!decl)
    return;

  symbol = XEXP (DECL_RTL (decl), 0);
  gcc_assert (GET_CODE (symbol) == SYMBOL_REF);
  if (!SYMBOL_REF_DLLEXPORT_P (symbol))
    return;

  gcc_assert (TREE_PUBLIC (decl));

  p = ggc_alloc_export_list ();
  p->next = export_head;
  p->name = name;
  p->is_data = is_data;
  export_head = p;
}

#ifdef CXX_WRAP_SPEC_LIST

/*  Hash table equality helper function.  */

static int
wrapper_strcmp (const void *x, const void *y)
{
  return !strcmp ((const char *) x, (const char *) y);
}

/* Search for a function named TARGET in the list of library wrappers
   we are using, returning a pointer to it if found or NULL if not.
   This function might be called on quite a few symbols, and we only
   have the list of names of wrapped functions available to us as a
   spec string, so first time round we lazily initialise a hash table
   to make things quicker.  */

static const char *
i386_find_on_wrapper_list (const char *target)
{
  static char first_time = 1;
  static htab_t wrappers;

  if (first_time)
    {
      /* Beware that this is not a complicated parser, it assumes
         that any sequence of non-whitespace beginning with an
	 underscore is one of the wrapped symbols.  For now that's
	 adequate to distinguish symbols from spec substitutions
	 and command-line options.  */
      static char wrapper_list_buffer[] = CXX_WRAP_SPEC_LIST;
      char *bufptr;
      /* Breaks up the char array into separated strings
         strings and enter them into the hash table.  */
      wrappers = htab_create_alloc (8, htab_hash_string, wrapper_strcmp,
	0, xcalloc, free);
      for (bufptr = wrapper_list_buffer; *bufptr; ++bufptr)
	{
	  char *found = NULL;
	  if (ISSPACE (*bufptr))
	    continue;
	  if (*bufptr == '_')
	    found = bufptr;
	  while (*bufptr && !ISSPACE (*bufptr))
	    ++bufptr;
	  if (*bufptr)
	    *bufptr = 0;
	  if (found)
	    *htab_find_slot (wrappers, found, INSERT) = found;
	}
      first_time = 0;
    }

  return (const char *) htab_find (wrappers, target);
}

#endif /* CXX_WRAP_SPEC_LIST */

/* This is called at the end of assembly.  For each external function
   which has not been defined, we output a declaration now.  We also
   output the .drectve section.  */

void
i386_pe_file_end (void)
{
  struct extern_list *p;

  for (p = extern_head; p != NULL; p = p->next)
    {
      tree decl;

      decl = p->decl;

      /* Positively ensure only one declaration for any given symbol.  */
      if (! TREE_ASM_WRITTEN (decl)
	  && TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl)))
	{
#ifdef CXX_WRAP_SPEC_LIST
	  /* To ensure the DLL that provides the corresponding real
	     functions is still loaded at runtime, we must reference
	     the real function so that an (unused) import is created.  */
	  const char *realsym = i386_find_on_wrapper_list (p->name);
	  if (realsym)
	    i386_pe_declare_function_type (asm_out_file,
		concat ("__real_", realsym, NULL), TREE_PUBLIC (decl));
#endif /* CXX_WRAP_SPEC_LIST */
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
	  fprintf (asm_out_file, "\t.ascii \" -export:\\\"%s\\\"%s\"\n",
		   default_strip_name_encoding (q->name),
		   (q->is_data ? ",data" : ""));
	}
    }
}


/* x64 Structured Exception Handling unwind info.  */

struct seh_frame_state
{
  /* SEH records saves relative to the "current" stack pointer, whether
     or not there's a frame pointer in place.  This tracks the current
     stack pointer offset from the CFA.  */
  HOST_WIDE_INT sp_offset;

  /* The CFA is located at CFA_REG + CFA_OFFSET.  */
  HOST_WIDE_INT cfa_offset;
  rtx cfa_reg;
};

/* Set up data structures beginning output for SEH.  */

void
i386_pe_seh_init (FILE *f)
{
  struct seh_frame_state *seh;

  if (!TARGET_SEH)
    return;
  if (cfun->is_thunk)
    return;

  /* We cannot support DRAP with SEH.  We turned off support for it by
     re-defining MAX_STACK_ALIGNMENT when SEH is enabled.  */
  gcc_assert (!stack_realign_drap);

  seh = XCNEW (struct seh_frame_state);
  cfun->machine->seh = seh;

  seh->sp_offset = INCOMING_FRAME_SP_OFFSET;
  seh->cfa_offset = INCOMING_FRAME_SP_OFFSET;
  seh->cfa_reg = stack_pointer_rtx;

  fputs ("\t.seh_proc\t", f);
  assemble_name (f, IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (cfun->decl)));
  fputc ('\n', f);
}

void
i386_pe_seh_end_prologue (FILE *f)
{
  struct seh_frame_state *seh;

  if (!TARGET_SEH)
    return;
  if (cfun->is_thunk)
    return;
  seh = cfun->machine->seh;

  /* Emit an assembler directive to set up the frame pointer.  Always do
     this last.  The documentation talks about doing this "before" any
     other code that uses offsets, but (experimentally) that's after we
     emit the codes in reverse order (handled by the assembler).  */
  if (seh->cfa_reg != stack_pointer_rtx)
    {
      HOST_WIDE_INT offset = seh->sp_offset - seh->cfa_offset;

      gcc_assert ((offset & 15) == 0);
      gcc_assert (IN_RANGE (offset, 0, 240));

      fputs ("\t.seh_setframe\t", f);
      print_reg (seh->cfa_reg, 0, f);
      fprintf (f, ", " HOST_WIDE_INT_PRINT_DEC "\n", offset);
    }

  XDELETE (seh);
  cfun->machine->seh = NULL;

  fputs ("\t.seh_endprologue\n", f);
}

static void
i386_pe_seh_fini (FILE *f)
{
  if (!TARGET_SEH)
    return;
  if (cfun->is_thunk)
    return;
  fputs ("\t.seh_endproc\n", f);
}

/* Emit an assembler directive to save REG via a PUSH.  */

static void
seh_emit_push (FILE *f, struct seh_frame_state *seh, rtx reg)
{
  unsigned int regno = REGNO (reg);

  gcc_checking_assert (GENERAL_REGNO_P (regno));

  seh->sp_offset += UNITS_PER_WORD;
  if (seh->cfa_reg == stack_pointer_rtx)
    seh->cfa_offset += UNITS_PER_WORD;

  fputs ("\t.seh_pushreg\t", f);
  print_reg (reg, 0, f);
  fputc ('\n', f);
}

/* Emit an assembler directive to save REG at CFA - CFA_OFFSET.  */

static void
seh_emit_save (FILE *f, struct seh_frame_state *seh,
	       rtx reg, HOST_WIDE_INT cfa_offset)
{
  unsigned int regno = REGNO (reg);
  HOST_WIDE_INT offset;

  /* Negative save offsets are of course not supported, since that
     would be a store below the stack pointer and thus clobberable.  */
  gcc_assert (seh->sp_offset >= cfa_offset);
  offset = seh->sp_offset - cfa_offset;

  fputs ((SSE_REGNO_P (regno) ? "\t.seh_savexmm\t"
	 : GENERAL_REGNO_P (regno) ?  "\t.seh_savereg\t"
	 : (gcc_unreachable (), "")), f);
  print_reg (reg, 0, f);
  fprintf (f, ", " HOST_WIDE_INT_PRINT_DEC "\n", offset);
}

/* Emit an assembler directive to adjust RSP by OFFSET.  */

static void
seh_emit_stackalloc (FILE *f, struct seh_frame_state *seh,
		     HOST_WIDE_INT offset)
{
  /* We're only concerned with prologue stack allocations, which all
     are subtractions from the stack pointer.  */
  gcc_assert (offset < 0);
  offset = -offset;

  if (seh->cfa_reg == stack_pointer_rtx)
    seh->cfa_offset += offset;
  seh->sp_offset += offset;

  fprintf (f, "\t.seh_stackalloc\t" HOST_WIDE_INT_PRINT_DEC "\n", offset);
}

/* Process REG_CFA_ADJUST_CFA for SEH.  */

static void
seh_cfa_adjust_cfa (FILE *f, struct seh_frame_state *seh, rtx pat)
{
  rtx dest, src;
  HOST_WIDE_INT reg_offset = 0;
  unsigned int dest_regno;

  dest = SET_DEST (pat);
  src = SET_SRC (pat);

  if (GET_CODE (src) == PLUS)
    {
      reg_offset = INTVAL (XEXP (src, 1));
      src = XEXP (src, 0);
    }
  else if (GET_CODE (src) == MINUS)
    {
      reg_offset = -INTVAL (XEXP (src, 1));
      src = XEXP (src, 0);
    }
  gcc_assert (src == stack_pointer_rtx);
  gcc_assert (seh->cfa_reg == stack_pointer_rtx);
  dest_regno = REGNO (dest);

  if (dest_regno == STACK_POINTER_REGNUM)
    seh_emit_stackalloc (f, seh, reg_offset);
  else if (dest_regno == HARD_FRAME_POINTER_REGNUM)
    {
      seh->cfa_reg = dest;
      seh->cfa_offset -= reg_offset;
    }
  else
    gcc_unreachable ();
}

/* Process REG_CFA_OFFSET for SEH.  */

static void
seh_cfa_offset (FILE *f, struct seh_frame_state *seh, rtx pat)
{
  rtx dest, src;
  HOST_WIDE_INT reg_offset;

  dest = SET_DEST (pat);
  src = SET_SRC (pat);

  gcc_assert (MEM_P (dest));
  dest = XEXP (dest, 0);
  if (REG_P (dest))
    reg_offset = 0;
  else
    {
      gcc_assert (GET_CODE (dest) == PLUS);
      reg_offset = INTVAL (XEXP (dest, 1));
      dest = XEXP (dest, 0);
    }
  gcc_assert (dest == seh->cfa_reg);

  seh_emit_save (f, seh, src, seh->cfa_offset - reg_offset);
}

/* Process a FRAME_RELATED_EXPR for SEH.  */

static void
seh_frame_related_expr (FILE *f, struct seh_frame_state *seh, rtx pat)
{
  rtx dest, src;
  HOST_WIDE_INT addend;

  /* See the full loop in dwarf2out_frame_debug_expr.  */
  if (GET_CODE (pat) == PARALLEL || GET_CODE (pat) == SEQUENCE)
    {
      int i, n = XVECLEN (pat, 0), pass, npass;

      npass = (GET_CODE (pat) == PARALLEL ? 2 : 1);
      for (pass = 0; pass < npass; ++pass)
	for (i = 0; i < n; ++i)
	  {
	    rtx ele = XVECEXP (pat, 0, i);

	    if (GET_CODE (ele) != SET)
	      continue;
	    dest = SET_DEST (ele);

	    /* Process each member of the PARALLEL independently.  The first
	       member is always processed; others only if they are marked.  */
	    if (i == 0 || RTX_FRAME_RELATED_P (ele))
	      {
		/* Evaluate all register saves in the first pass and all
		   register updates in the second pass.  */
		if ((MEM_P (dest) ^ pass) || npass == 1)
		  seh_frame_related_expr (f, seh, ele);
	      }
	  }
      return;
    }

  dest = SET_DEST (pat);
  src = SET_SRC (pat);

  switch (GET_CODE (dest))
    {
    case REG:
      switch (GET_CODE (src))
	{
	case REG:
	  /* REG = REG: This should be establishing a frame pointer.  */
	  gcc_assert (src == stack_pointer_rtx);
	  gcc_assert (dest == hard_frame_pointer_rtx);
	  seh_cfa_adjust_cfa (f, seh, pat);
	  break;

	case PLUS:
	  addend = INTVAL (XEXP (src, 1));
	  src = XEXP (src, 0);
	  if (dest == hard_frame_pointer_rtx)
	    seh_cfa_adjust_cfa (f, seh, pat);
	  else if (dest == stack_pointer_rtx)
	    {
	      gcc_assert (src == stack_pointer_rtx);
	      seh_emit_stackalloc (f, seh, addend);
	    }
	  else
	    gcc_unreachable ();
	  break;

	default:
	  gcc_unreachable ();
	}
      break;

    case MEM:
      /* A save of some kind.  */
      dest = XEXP (dest, 0);
      if (GET_CODE (dest) == PRE_DEC)
	{
	  gcc_checking_assert (GET_MODE (src) == Pmode);
	  gcc_checking_assert (REG_P (src));
	  seh_emit_push (f, seh, src);
	}
      else
	seh_cfa_offset (f, seh, pat);
      break;

    default:
      gcc_unreachable ();
    }
}

/* This function looks at a single insn and emits any SEH directives
   required for unwind of this insn.  */

void
i386_pe_seh_unwind_emit (FILE *asm_out_file, rtx insn)
{
  rtx note, pat;
  bool handled_one = false;
  struct seh_frame_state *seh;

  if (!TARGET_SEH)
    return;

  /* We free the SEH data once done with the prologue.  Ignore those
     RTX_FRAME_RELATED_P insns that are associated with the epilogue.  */
  seh = cfun->machine->seh;
  if (seh == NULL)
    return;

  if (NOTE_P (insn) || !RTX_FRAME_RELATED_P (insn))
    return;

  for (note = REG_NOTES (insn); note ; note = XEXP (note, 1))
    {
      pat = XEXP (note, 0);
      switch (REG_NOTE_KIND (note))
	{
	case REG_FRAME_RELATED_EXPR:
	  goto found;

	case REG_CFA_DEF_CFA:
	case REG_CFA_EXPRESSION:
	  /* Only emitted with DRAP, which we disable.  */
	  gcc_unreachable ();
	  break;

	case REG_CFA_REGISTER:
	  /* Only emitted in epilogues, which we skip.  */
	  gcc_unreachable ();

	case REG_CFA_ADJUST_CFA:
	  if (pat == NULL)
	    {
	      pat = PATTERN (insn);
	      if (GET_CODE (pat) == PARALLEL)
		pat = XVECEXP (pat, 0, 0);
	    }
	  seh_cfa_adjust_cfa (asm_out_file, seh, pat);
	  handled_one = true;
	  break;

	case REG_CFA_OFFSET:
	  if (pat == NULL)
	    pat = single_set (insn);
	  seh_cfa_offset (asm_out_file, seh, pat);
	  handled_one = true;
	  break;

	default:
	  break;
	}
    }
  if (handled_one)
    return;
  pat = PATTERN (insn);
 found:
  seh_frame_related_expr (asm_out_file, seh, pat);
}

void
i386_pe_start_function (FILE *f, const char *name, tree decl)
{
  i386_pe_maybe_record_exported_symbol (decl, name, 0);
  if (write_symbols != SDB_DEBUG)
    i386_pe_declare_function_type (f, name, TREE_PUBLIC (decl));
  /* In case section was altered by debugging output.  */
  if (decl != NULL_TREE)
    switch_to_section (function_section (decl));
  ASM_OUTPUT_FUNCTION_LABEL (f, name, decl);
}

void
i386_pe_end_function (FILE *f, const char *name ATTRIBUTE_UNUSED,
		      tree decl ATTRIBUTE_UNUSED)
{
  i386_pe_seh_fini (f);
}


#include "gt-winnt.h"
