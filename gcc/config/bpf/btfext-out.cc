/* BPF Compile Once - Run Everywhere (CO-RE) support.
   Copyright (C) 2021-2024 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "memmodel.h"
#include "tm_p.h"
#include "output.h"
#include "dwarf2asm.h"
#include "ctfc.h"
#include "btf.h"
#include "rtl.h"
#include "tree-pretty-print.h"
#include "cgraph.h"

#include "btfext-out.h"

/* This file contains data structures and routines for construction and output
   of BPF Compile Once - Run Everywhere (BPF CO-RE) information.

   eBPF programs written in C usually include Linux kernel headers, so that
   they may interact with kernel data structures in a useful way. This
   intrudces two major portability issues:

   1. Kernel data structures regularly change, with fields added, moved or
      deleted between versions. An eBPF program cannot in general be expected
      to run on any systems which does not share an identical kernel version to
      the system on which it was compiled.

   2. Included kernel headers (and used data structures) may be internal, not
      exposed in an userspace API, and therefore target-specific. An eBPF
      program compiled on an x86_64 machine will include x86_64 kernel headers.
      The resulting program may not run well (or at all) in machines of
      another architecture.

   BPF CO-RE is designed to solve the first issue by leveraging the BPF loader
   to adjust references to kernel data structures made by the program as-needed
   according to versions of structures actually present on the host kernel.

   To achieve this, additional information is placed in a ".BTF.ext" section.
   This information tells the loader which references will require adjusting,
   and how to perform each necessary adjustment.

   For any access to a data structure which may require load-time adjustment,
   the following information is recorded (making up a CO-RE relocation record):
   - The BTF type ID of the outermost structure which is accessed.
   - An access string encoding the accessed member via a series of member and
     array indexes. These indexes are used to look up detailed BTF information
     about the member.
   - The offset of the appropriate instruction to patch in the BPF program.
   - An integer specifying what kind of relocation to perform.

   A CO-RE-capable BPF loader reads this information together with the BTF
   information of the program, compares it against BTF information of the host
   kernel, and determines the appropriate way to patch the specified
   instruction.

   Once all CO-RE relocations are resolved, the program is loaded and verified
   as usual. The process can be summarized with the following diagram:

              +------------+
              | C compiler |
              +-----+------+
                    | BPF + BTF + CO-RE relocations
                    v
              +------------+
         +--->| BPF loader |
         |    +-----+------+
         |          | BPF (adapted)
     BTF |          v
         |    +------------+
         +----+   Kernel   |
              +------------+

   Note that a single ELF object may contain multiple eBPF programs. As a
   result, a single .BTF.ext section can contain CO-RE relocations for multiple
   programs in distinct sections.  */

/* BTF.ext debug info section.  */
static GTY (()) section * btf_ext_info_section;

#ifndef BTF_EXT_INFO_SECTION_NAME
#define BTF_EXT_INFO_SECTION_NAME ".BTF.ext"
#endif
#define BTF_EXT_INFO_SECTION_FLAGS (SECTION_DEBUG)

#ifndef BTF_EXT_INFO_SECTION_LABEL
#define BTF_EXT_INFO_SECTION_LABEL "Lbtfext"
#endif

#define MAX_BTF_EXT_LABEL_BYTES 40
static char btf_ext_info_section_label[MAX_BTF_EXT_LABEL_BYTES];

/* A funcinfo record, in the .BTF.ext funcinfo section.  */
struct GTY ((chain_next ("%h.next"))) btf_ext_funcinfo
{
  uint32_t type;     /* Type ID of a BTF_KIND_FUNC type.  */
  const char *fnname;
  const char *label;

  struct btf_ext_funcinfo *next; /* Linked list to collect func_info elems.  */
};

/* A lineinfo record, in the .BTF.ext lineinfo section.  */
struct GTY ((chain_next ("%h.next"))) btf_ext_lineinfo
{
  uint32_t insn_off;      /* Offset of the instruction.  */
  uint32_t file_name_off; /* Offset of file name in BTF string table.  */
  uint32_t line_off;      /* Offset of source line in BTF string table.  */
  uint32_t line_col;      /* Line number (bits 31-11) and column (11-0).  */

  struct btf_ext_lineinfo *next; /* Linked list to collect line_info elems.  */
};

/* Internal representation of a BPF CO-RE relocation record.  */
struct GTY ((chain_next ("%h.next"))) btf_ext_core_reloc {
  ctf_dtdef_ref bpfcr_type;		/* BTF type involved in relocation.  */
  unsigned int  bpfcr_astr_off;		/* Offset of access string in .BTF
					   string table.  */
  rtx_code_label * bpfcr_insn_label;	/* RTX label attached to instruction
					   to patch.  */
  enum btf_core_reloc_kind bpfcr_kind;	/* Kind of relocation to perform.  */

  struct {
    const char *accessor_str;
    tree type;
  } info;

  struct btf_ext_core_reloc *next;
};

/* Main data structure to keep .BTF.ext section data.  */
struct GTY ((chain_next ("%h.next"))) btf_ext_info_sec {
  const char *sec_name;
  uint32_t   sec_name_off; /* offset to section name.  */

  struct {
    uint32_t num_info;
    struct btf_ext_funcinfo *head;
  } func_info;
  struct {
    uint32_t num_info;
    struct btf_ext_lineinfo *head;
  } line_info;
  struct {
    uint32_t num_info;
    struct btf_ext_core_reloc *head;
  } core_info;

  struct btf_ext_info_sec *next;
};

static GTY (()) struct btf_ext_info_sec *btf_ext = NULL;

/* Helper function to add a section structure to the linked list with entry
   point in info static variable.  */

static struct btf_ext_info_sec *
btfext_info_sec_find_or_add (const char *sec_name, bool add)
{
  struct btf_ext_info_sec **tmp = &btf_ext;

  while (*tmp != NULL)
    {
      if (strcmp ((*tmp)->sec_name, sec_name) == 0)
	return *tmp;
      tmp = &((*tmp)->next);
    }

  if (add == false)
    return NULL;

  struct btf_ext_info_sec *ret = ggc_cleared_alloc<struct btf_ext_info_sec> ();
  *tmp = ret;

  /* Set data for section info.  */
  ret->sec_name = sec_name;
  ret->sec_name_off = btf_ext_add_string (sec_name);

  return ret;
}

#define SEARCH_NODE_AND_RETURN(TYPE, FIELD, CONDITION) __extension__ ({ \
  TYPE **head = &(FIELD); \
  while (*head != NULL) \
    { \
      if (CONDITION) \
	return (*head); \
      head = &((*head)->next); \
    } \
  head; \
})

/* Function to create or find a funcinfo node in info.  */

static struct btf_ext_funcinfo *
bpf_create_or_find_funcinfo (const char *fnname, const char *sec_name,
			     btf_ext_info_sec **in_sec = NULL)
{
  struct btf_ext_info_sec *sec_elem =
    btfext_info_sec_find_or_add (sec_name, true);

  if (in_sec != NULL)
    *in_sec = sec_elem;

  struct btf_ext_funcinfo **head =
    SEARCH_NODE_AND_RETURN(struct btf_ext_funcinfo,
			   sec_elem->func_info.head,
			   strcmp ((*head)->fnname, fnname) == 0);

  *head = ggc_cleared_alloc<struct btf_ext_funcinfo> ();
  (*head)->fnname = fnname;
  (*head)->label = NULL;

  return *head;
}

/* Function to create a core_reloc node in info.  */

static struct btf_ext_core_reloc *
bpf_create_core_reloc (const char *sec_name,
		       struct btf_ext_info_sec **in_sec = NULL)
{
  struct btf_ext_info_sec *sec_elem =
    btfext_info_sec_find_or_add (sec_name, true);

  if (in_sec != NULL)
    *in_sec = sec_elem;

  struct btf_ext_core_reloc **head =
    SEARCH_NODE_AND_RETURN(struct btf_ext_core_reloc,
			   sec_elem->core_info.head,
			   false);

  *head = ggc_cleared_alloc<struct btf_ext_core_reloc> ();

  return *head;
}

/* String caching to avoid repeated strings added to BTF string table.  */
struct GTY((chain_next ("%h.next"))) string_cache {
  const char *str;
  unsigned int offset;
  struct string_cache *next;
};
static GTY(()) struct string_cache *btf_ext_strings = NULL;

unsigned int
btf_ext_add_string (const char *str)
{
  ctf_container_ref ctfc = ctf_get_tu_ctfc ();
  struct string_cache **tmp = &btf_ext_strings;
  while (*tmp != NULL)
    {
      if (strcmp ((*tmp)->str, str) == 0)
	return (*tmp)->offset;
      tmp = &((*tmp)->next);
    }

  *tmp = ggc_cleared_alloc<struct string_cache> ();
  (*tmp)->str = ggc_strdup (str);
  ctf_add_string (ctfc, (*tmp)->str, &((*tmp)->offset), CTF_AUX_STRTAB);

  return (*tmp)->offset;
}

/* Create a new BPF CO-RE relocation record, and add it to the appropriate
   CO-RE section.  */
void
bpf_core_reloc_add (const tree type, const char * section_name,
		    const char *accessor,
		    rtx_code_label *label,
		    enum btf_core_reloc_kind kind)
{
  struct btf_ext_info_sec *sec = NULL;
  struct btf_ext_core_reloc *bpfcr = bpf_create_core_reloc (section_name, &sec);

  ctf_container_ref ctfc = ctf_get_tu_ctfc ();
  ctf_dtdef_ref dtd = ctf_lookup_tree_type (ctfc, type);

  /* Buffer the access string in the auxiliary strtab.  */
  bpfcr->bpfcr_astr_off = 0;
  gcc_assert (accessor != NULL);
  bpfcr->bpfcr_astr_off = btf_ext_add_string (accessor);

  bpfcr->bpfcr_type = dtd;
  bpfcr->bpfcr_insn_label = label;
  bpfcr->bpfcr_kind = kind;

  bpfcr->info.accessor_str = accessor;
  bpfcr->info.type = type;

  sec->core_info.num_info += 1;
}

/* Return the 0-based index of the field NODE in its containing struct or union
   type.  */

int
bpf_core_get_sou_member_index (ctf_container_ref ctfc, const tree node)
{
  if (TREE_CODE (node) == FIELD_DECL)
    {
      const tree container = DECL_CONTEXT (node);

      /* Lookup the CTF type info for the containing type.  */
      dw_die_ref die = lookup_type_die (container);
      if (die == NULL)
        return -1;

      ctf_dtdef_ref dtd = ctf_dtd_lookup (ctfc, die);
      if (dtd == NULL)
        return -1;

      unsigned int kind = CTF_V2_INFO_KIND (dtd->dtd_data.ctti_info);
      if (kind != CTF_K_STRUCT && kind != CTF_K_UNION)
        return -1;

      tree field = TYPE_FIELDS (container);
      int i = 0;
      ctf_dmdef_t * dmd;
      for (dmd = dtd->dtd_u.dtu_members;
           dmd != NULL; dmd = (ctf_dmdef_t *) ctf_dmd_list_next (dmd))
        {
	  bool field_has_btf = (dmd->dmd_type
				&& dmd->dmd_type->dtd_type <= BTF_MAX_TYPE);

	  if (field == node)
	    return field_has_btf ? i : -1;

	  if (field_has_btf)
	    i++;

	  field = DECL_CHAIN (field);
        }
    }
  return -1;
}

/* Helper function to check if a particular named function exists as a
   BTF_KIND_FUNC type record.  */

static bool
btf_funcinfo_type_callback (ctf_dtdef_ref func, void *data)
{
  struct btf_ext_funcinfo *info = (struct btf_ext_funcinfo *) data;
  if (strcmp (func->dtd_name, info->fnname) == 0)
    {
      uint32_t type = func->dtd_type;
      info->type = type;
      return true;
    }
  return false;
}

/* Entry point function to add a func_info  in local data structures
   represented by info static variable.
   This function is used in bpf.cc.  */

struct btf_ext_funcinfo *
btf_add_func_info_for (tree decl, const char *label)
{
  const char *fnname = IDENTIFIER_POINTER (DECL_NAME (decl));
  const char *sec_name = decl_section_name (decl);

  /* Recover the original function name, which may have been mangled by
     optimizations.  */
  const char *cp_ptr = strstr (fnname, ".");
  if (cp_ptr != NULL)
    {
      char new_name[100];
      strcpy (new_name, fnname);
      int pos = cp_ptr - fnname;
      new_name[pos] = 0;
      fnname = ggc_strdup (new_name);
    }

  if (sec_name == NULL)
    sec_name = ".text";

  struct btf_ext_info_sec *sec = NULL;
  struct btf_ext_funcinfo *info =
    bpf_create_or_find_funcinfo (fnname, sec_name, &sec);

  info->label = label;
  return info;
}

/* This function traverses all func_info entries and verified they do have a
   BTF_KIND_FUNC type record associated.  If they do not it is marked as
   invalided by clearing the associated label.  */

static void
btf_validate_funcinfo (btf_ext_info_sec *sec)
{
  while (sec != NULL)
    {
      struct btf_ext_funcinfo *funcinfo = sec->func_info.head;
      while (funcinfo != NULL)
	{
	  bool found = traverse_btf_func_types (btf_funcinfo_type_callback,
						funcinfo);
	  if (found == true)
	    sec->func_info.num_info += 1;
	  else
	    funcinfo->label = NULL;

	  funcinfo = funcinfo->next;
	}
      sec = sec->next;
    }
}

/* Compute the section size in section for func_info, line_info and core_info
   regions of .BTF.ext.  */

static void
btf_ext_info_len (uint32_t *fi_len, uint32_t *li_len, uint32_t *cr_len)
{
  *fi_len = *li_len = *cr_len = 0;
  struct btf_ext_info_sec *tmp = btf_ext;
  if (tmp != NULL)
  while (tmp != NULL)
    {
      /* Size computation does 8 bytes per section entry plus num_info of the
       * respective structure size:
	  - 8 bytes for func_info,
	  - 16 bytes for both line_info and core_info.  */
      if (tmp->func_info.num_info > 0)
	*fi_len +=  8 + (8 * tmp->func_info.num_info);
      if (tmp->line_info.num_info > 0)
	*li_len +=  8 + (16 * tmp->line_info.num_info);
      if (tmp->core_info.num_info > 0)
	*cr_len +=  8 + (16 * tmp->core_info.num_info);
      tmp = tmp->next;
    }

  /* If there are entries within the regions, add 4 bytes to set the header of
     the respective sections that contains the size for each of the entry.  */
  *fi_len += *fi_len != 0 ? 4 : 0;
  *li_len += *li_len != 0 ? 4 : 0;
  *cr_len += *cr_len != 0 ? 4 : 0;
}

/* Compute and output the header of a .BTF.ext debug info section.  */

static void
output_btfext_header (void)
{
  switch_to_section (btf_ext_info_section);
  ASM_OUTPUT_LABEL (asm_out_file, btf_ext_info_section_label);

  dw2_asm_output_data (2, BTF_MAGIC, "btf_magic");
  dw2_asm_output_data (1, BTF_VERSION, "btfext_version");
  dw2_asm_output_data (1, 0, "btfext_flags");
  dw2_asm_output_data (4, sizeof (struct btf_ext_header), "btfext_hdr_len");

  btf_validate_funcinfo (btf_ext);

  uint32_t func_info_len = 0;
  uint32_t line_info_len = 0;
  uint32_t core_info_len = 0;
  btf_ext_info_len (&func_info_len, &line_info_len, &core_info_len);

  if (!TARGET_BPF_CORE)
    core_info_len = 0;

  uint32_t func_info_off = 0;
  uint32_t line_info_off = func_info_len;
  uint32_t core_info_off = line_info_off + line_info_len;

  dw2_asm_output_data (4, func_info_off, "func_info_offset");
  dw2_asm_output_data (4, func_info_len, "func_info_len");

  dw2_asm_output_data (4, line_info_off, "line_info_offset");
  dw2_asm_output_data (4, line_info_len, "line_info_len");

  dw2_asm_output_data (4, core_info_off, "core_relo_offset");
  dw2_asm_output_data (4, core_info_len, "core_relo_len");
}

/* Outputs func_info region on .BTF.ext.  */

static void
output_btfext_func_info (struct btf_ext_info_sec *sec)
{
  unsigned int str_aux_off = ctfc_get_strtab_len (ctf_get_tu_ctfc (),
						  CTF_STRTAB);
  bool executed = false;
  while (sec != NULL)
    {
      uint32_t count = 0;
      if (sec->func_info.num_info > 0)
	{
	  if (executed == false && (executed = true))
	    dw2_asm_output_data (4, 8, "FuncInfo entry size");
	  dw2_asm_output_data (4, sec->sec_name_off + str_aux_off,
			       "FuncInfo section string for %s",
			       sec->sec_name);
	  dw2_asm_output_data (4, sec->func_info.num_info, "Number of entries");

	  struct btf_ext_funcinfo *elem = sec->func_info.head;
	  while (elem != NULL)
	    {
	      if (elem->label != NULL)
		{
		  count += 1;
		  dw2_asm_output_offset (4, elem->label,
		       NULL, "label for function %s", elem->fnname);
		  dw2_asm_output_data (4, elem->type, "btf_type_id");
		}
	      elem = elem->next;
	    }
	}

      gcc_assert (count == sec->func_info.num_info);
      sec = sec->next;
    }
}

/* Output all CO-RE relocation sections.  */

static void
output_btfext_core_sections (void)
{
  struct btf_ext_info_sec *sec = btf_ext;
  unsigned int str_aux_off = ctfc_get_strtab_len (ctf_get_tu_ctfc (),
						  CTF_STRTAB);
  bool executed = false;
  while (sec != NULL)
    {
      uint32_t count = 0;
      if (sec->core_info.num_info > 0)
	{
	  if (executed == false && (executed = true))
	    dw2_asm_output_data (4, 16, "CoreInfo entry size");
	  dw2_asm_output_data (4, sec->sec_name_off + str_aux_off,
			       "CoreInfo section string for %s",
			       sec->sec_name);
	  dw2_asm_output_data (4, sec->core_info.num_info, "Number of entries");

	  struct btf_ext_core_reloc *bpfcr = sec->core_info.head;
	  while (bpfcr != NULL)
	    {
	      count += 1;
	      dw2_assemble_integer (4,
			gen_rtx_LABEL_REF (Pmode, bpfcr->bpfcr_insn_label));
	      fprintf (asm_out_file, "\t%s%s\n",
		       flag_debug_asm ? ASM_COMMENT_START : "",
		       (flag_debug_asm ? " bpfcr_insn" : ""));

	      /* Extract the pretty print for the type expression.  */
	      pretty_printer pp;
	      dump_generic_node (&pp, bpfcr->info.type, 0,
				 TDF_VOPS|TDF_MEMSYMS|TDF_SLIM,
				 false);
	      char *str = xstrdup (pp_formatted_text (&pp));

	      uint32_t type_id = bpfcr->bpfcr_type
		? bpfcr->bpfcr_type->dtd_type
		: BTF_VOID_TYPEID;
	      dw2_asm_output_data (4, type_id, "bpfcr_type (%s)", str);
	      dw2_asm_output_data (4, bpfcr->bpfcr_astr_off + str_aux_off,
				   "bpfcr_astr_off (\"%s\")",
				   bpfcr->info.accessor_str);
	      dw2_asm_output_data (4, bpfcr->bpfcr_kind, "bpfcr_kind");
	      bpfcr = bpfcr->next;
	    }
	}

      gcc_assert (count == sec->core_info.num_info);
      sec = sec->next;
    }
}

/* Initialize sections, labels, and data structures for BTF.ext output.  */

void
btf_ext_init (void)
{
  btf_ext_info_section = get_section (BTF_EXT_INFO_SECTION_NAME,
				      BTF_EXT_INFO_SECTION_FLAGS, NULL);

  ASM_GENERATE_INTERNAL_LABEL (btf_ext_info_section_label,
			       "Lbtfext", 0);
}

/* Output the entire .BTF.ext section.  */

void
btf_ext_output (void)
{
  output_btfext_header ();
  output_btfext_func_info (btf_ext);
  if (TARGET_BPF_CORE)
    output_btfext_core_sections ();

  /* Extra padding required by BPF code, in case all structures are empty.  */
  dw2_asm_output_data (4, 0, "Required padding by libbpf structs");
}

#include "gt-btfext-out.h"
