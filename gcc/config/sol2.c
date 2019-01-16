/* General Solaris system support.
   Copyright (C) 2004-2019 Free Software Foundation, Inc.
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
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "tm_p.h"
#include "stringpool.h"
#include "attribs.h"
#include "diagnostic-core.h"
#include "varasm.h"
#include "output.h"

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
      fprintf (file, "\t.pushsection\t" SECTION_NAME_FORMAT "\n", ".init");
      ASM_OUTPUT_CALL (file, decl);
      fprintf (file, "\t.popsection\n");
    }

  if (lookup_attribute ("fini", DECL_ATTRIBUTES (decl)))
    {
      fprintf (file, "\t.pushsection\t" SECTION_NAME_FORMAT "\n", ".fini");
      ASM_OUTPUT_CALL (file, decl);
      fprintf (file, "\t.popsection\n");
    }
}

/* Emit an assembler directive to set symbol for DECL visibility to
   the visibility type VIS, which must not be VISIBILITY_DEFAULT.  */

void
solaris_assemble_visibility (tree decl, int vis ATTRIBUTE_UNUSED)
{
#ifdef HAVE_GAS_HIDDEN
  /* Sun as uses .symbolic for STV_PROTECTED.  STV_INTERNAL is marked as
     `currently reserved', but the linker treats it like STV_HIDDEN.  Sun
     Studio 12.1 cc emits .hidden instead.

     There are 3 Sun extensions GCC doesn't yet know about: STV_EXPORTED,
     STV_SINGLETON, and STV_ELIMINATE.

     See Linker and Libraries Guide, Ch. 2, Link-Editor, Defining
     Additional Symbols, and Ch. 7, Object-File Format, Symbol Table
     Section.  */

  static const char * const visibility_types[] = {
    NULL, "symbolic", "hidden", "hidden"
  };

  const char *name, *type;
  tree id = DECL_ASSEMBLER_NAME (decl);

  while (IDENTIFIER_TRANSPARENT_ALIAS (id))
    id = TREE_CHAIN (id);
  name = IDENTIFIER_POINTER (id);
  type = visibility_types[vis];

  fprintf (asm_out_file, "\t.%s\t", type);
  assemble_name (asm_out_file, name);
  fprintf (asm_out_file, "\n");
#else
  if (!DECL_ARTIFICIAL (decl))
    warning (OPT_Wattributes, "visibility attribute not supported "
			      "in this configuration; ignored");
#endif
}

/* Group section information entry stored in solaris_comdat_htab.  */

typedef struct comdat_entry
{
  const char *name;
  unsigned int flags;
  tree decl;
  const char *sig;
} comdat_entry;

/* Helpers for maintaining solaris_comdat_htab.  */

struct comdat_entry_hasher : nofree_ptr_hash <comdat_entry>
{
  static inline hashval_t hash (const comdat_entry *);
  static inline bool equal (const comdat_entry *, const comdat_entry *);
  static inline void remove (comdat_entry *);
};

inline hashval_t
comdat_entry_hasher::hash (const comdat_entry *entry)
{
  return htab_hash_string (entry->sig);
}

inline bool
comdat_entry_hasher::equal (const comdat_entry *entry1,
			    const comdat_entry *entry2)
{
  return strcmp (entry1->sig, entry2->sig) == 0;
}

/* Hash table of group signature symbols.  */

static hash_table<comdat_entry_hasher> *solaris_comdat_htab;

/* Output assembly to switch to COMDAT group section NAME with attributes
   FLAGS and group signature symbol DECL, using Sun as syntax.  */

void
solaris_elf_asm_comdat_section (const char *name, unsigned int flags, tree decl)
{
  const char *signature;
  char *section;
  comdat_entry entry, **slot;

  if (TREE_CODE (decl) == IDENTIFIER_NODE)
    signature = IDENTIFIER_POINTER (decl);
  else
    signature = IDENTIFIER_POINTER (DECL_COMDAT_GROUP (decl));

  /* Sun as requires group sections to be fragmented, i.e. to have names of
     the form <section>%<fragment>.  Strictly speaking this is only
     necessary to support cc -xF, but is enforced globally in violation of
     the ELF gABI.  We keep the section names generated by GCC (generally
     of the form .text.<signature>) and append %<signature> to pacify as,
     despite the redundancy.  */
  section = concat (name, "%", signature, NULL);

  /* Clear SECTION_LINKONCE flag so targetm.asm_out.named_section only
     emits this as a regular section.  Emit section before .group
     directive since Sun as treats undeclared sections as @progbits,
     which conflicts with .bss* sections which are @nobits.  */
  targetm.asm_out.named_section (section, flags & ~SECTION_LINKONCE, decl);
  
  /* Sun as separates declaration of a group section and of the group
     itself, using the .group directive and the #comdat flag.  */
  fprintf (asm_out_file, "\t.group\t%s," SECTION_NAME_FORMAT ",#comdat\n",
	   signature, section);

  /* Unlike GNU as, group signature symbols need to be defined explicitly
     for Sun as.  With a few exceptions, this is already the case.  To
     identify the missing ones without changing the affected frontents,
     remember the signature symbols and emit those not marked
     TREE_SYMBOL_REFERENCED in solaris_file_end.  */
  if (!solaris_comdat_htab)
    solaris_comdat_htab = new hash_table<comdat_entry_hasher> (37);

  entry.sig = signature;
  slot = solaris_comdat_htab->find_slot (&entry, INSERT);

  if (*slot == NULL)
    {
      *slot = XCNEW (comdat_entry);
      /* Remember fragmented section name.  */
      (*slot)->name = section;
      /* Emit as regular section, .group declaration has already been done.  */
      (*slot)->flags = flags & ~SECTION_LINKONCE;
      (*slot)->decl = decl;
      (*slot)->sig = signature;
    }
}

/* Define unreferenced COMDAT group signature symbol corresponding to SLOT.  */

int
solaris_define_comdat_signature (comdat_entry **slot,
				 void *aux ATTRIBUTE_UNUSED)
{
  comdat_entry *entry = *slot;
  tree decl = entry->decl;

  if (TREE_CODE (decl) != IDENTIFIER_NODE)
    decl = DECL_COMDAT_GROUP (decl);

  if (!TREE_SYMBOL_REFERENCED (decl))
    {
      /* Switch to group section, otherwise Sun as complains
	 `Group Id symbol defined outside of group'.  */
      switch_to_section (get_section (entry->name, entry->flags, entry->decl));

      ASM_OUTPUT_LABEL (asm_out_file, entry->sig);
    }

  /* Continue with scan.  */
  return 1;
}

/* Emit unreferenced COMDAT group signature symbols for Sun as.  */

void
solaris_file_end (void)
{
  if (!solaris_comdat_htab)
    return;

  solaris_comdat_htab->traverse <void *, solaris_define_comdat_signature>
    (NULL);
}

void
solaris_override_options (void)
{
  /* Older versions of Solaris ld cannot handle CIE version 3 in .eh_frame.
     Don't emit DWARF3/4 unless specifically selected if so.  */
  if (!HAVE_LD_EH_FRAME_CIEV3 && !global_options_set.x_dwarf_version)
    dwarf_version = 2;
}
