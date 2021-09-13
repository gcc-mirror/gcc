/* BPF Compile Once - Run Everywhere (CO-RE) support.
   Copyright (C) 2021 Free Software Foundation, Inc.

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

#include "coreout.h"

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

/* Internal representation of a BPF CO-RE relocation record.  */

typedef struct GTY (()) bpf_core_reloc {
  unsigned int bpfcr_type;		/* BTF type ID of container.  */
  unsigned int  bpfcr_astr_off;		/* Offset of access string in .BTF
					   string table.  */
  rtx_code_label * bpfcr_insn_label;	/* RTX label attached to instruction
					   to patch.  */
  enum btf_core_reloc_kind bpfcr_kind;	/* Kind of relocation to perform.  */
} bpf_core_reloc_t;

typedef bpf_core_reloc_t * bpf_core_reloc_ref;

/* Internal representation of a CO-RE relocation (sub)section of the
   .BTF.ext information. One such section is generated for each ELF section
   in the output object having relocations that a BPF loader must resolve.  */

typedef struct GTY (()) bpf_core_section {
  /* Name of ELF section to which these CO-RE relocations apply.  */
  const char * name;

  /* Offset of section name in .BTF string table.  */
  uint32_t name_offset;

  /* Relocations in the section.  */
  vec <bpf_core_reloc_ref, va_gc> * GTY (()) relocs;
} bpf_core_section_t;

typedef bpf_core_section_t * bpf_core_section_ref;

/* BTF.ext debug info section.  */

static GTY (()) section * btf_ext_info_section;

static int btf_ext_label_num;

#ifndef BTF_EXT_INFO_SECTION_NAME
#define BTF_EXT_INFO_SECTION_NAME ".BTF.ext"
#endif

#define BTF_EXT_INFO_SECTION_FLAGS (SECTION_DEBUG)

#define MAX_BTF_EXT_LABEL_BYTES 40

static char btf_ext_info_section_label[MAX_BTF_EXT_LABEL_BYTES];

#ifndef BTF_EXT_INFO_SECTION_LABEL
#define BTF_EXT_INFO_SECTION_LABEL "Lbtfext"
#endif

static GTY (()) vec<bpf_core_section_ref, va_gc> *bpf_core_sections;


/* Create a new BPF CO-RE relocation record, and add it to the appropriate
   CO-RE section.  */

void
bpf_core_reloc_add (const tree type, const char * section_name,
		    vec<unsigned int> *accessors, rtx_code_label *label)
{
  char buf[40];
  unsigned int i, n = 0;

  /* A valid CO-RE access must have at least one accessor.  */
  if (accessors->length () < 1)
    return;

  for (i = 0; i < accessors->length () - 1; i++)
    n += snprintf (buf + n, sizeof (buf) - n, "%u:", (*accessors)[i]);
  snprintf (buf + n, sizeof (buf) - n, "%u", (*accessors)[i]);

  bpf_core_reloc_ref bpfcr = ggc_cleared_alloc<bpf_core_reloc_t> ();
  ctf_container_ref ctfc = ctf_get_tu_ctfc ();

  /* Buffer the access string in the auxiliary strtab. Since the two string
     tables are concatenated, add the length of the first to the offset.  */
  size_t strtab_len = ctfc_get_strtab_len (ctfc, CTF_STRTAB);
  ctf_add_string (ctfc, buf, &(bpfcr->bpfcr_astr_off), CTF_AUX_STRTAB);
  bpfcr->bpfcr_astr_off += strtab_len;

  bpfcr->bpfcr_type = get_btf_id (ctf_lookup_tree_type (ctfc, type));
  bpfcr->bpfcr_insn_label = label;
  bpfcr->bpfcr_kind = BPF_RELO_FIELD_BYTE_OFFSET;

  /* Add the CO-RE reloc to the appropriate section.  */
  bpf_core_section_ref sec;
  FOR_EACH_VEC_ELT (*bpf_core_sections, i, sec)
    if (strcmp (sec->name, section_name) == 0)
      {
	vec_safe_push (sec->relocs, bpfcr);
	return;
      }

  /* If the CO-RE section does not yet exist, create it.  */
  sec = ggc_cleared_alloc<bpf_core_section_t> ();

  ctf_add_string (ctfc, section_name, &sec->name_offset, CTF_AUX_STRTAB);
  sec->name_offset += strtab_len;
  if (strcmp (section_name, ""))
    ctfc->ctfc_aux_strlen += strlen (section_name) + 1;

  sec->name = section_name;
  vec_alloc (sec->relocs, 1);
  vec_safe_push (sec->relocs, bpfcr);

  vec_safe_push (bpf_core_sections, sec);
}

/* Return the 0-based index of the field NODE in its containing struct or union
   type.  */

int
bpf_core_get_sou_member_index (ctf_container_ref ctfc, const tree node)
{
  if (TREE_CODE (node) == FIELD_DECL)
    {
      const tree container = DECL_CONTEXT (node);
      const char * name = IDENTIFIER_POINTER (DECL_NAME (node));

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

      int i = 0;
      ctf_dmdef_t * dmd;
      for (dmd = dtd->dtd_u.dtu_members;
           dmd != NULL; dmd = (ctf_dmdef_t *) ctf_dmd_list_next (dmd))
        {
          if (get_btf_id (dmd->dmd_type) > BTF_MAX_TYPE)
            continue;
          if (strcmp (dmd->dmd_name, name) == 0)
            return i;
          i++;
        }
    }
  return -1;
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

  uint32_t func_info_off = 0, func_info_len = 0;
  uint32_t line_info_off = 0, line_info_len = 0;
  uint32_t core_relo_off = 0, core_relo_len = 0;

  /* Header core_relo_len is the sum total length in bytes of all CO-RE
     relocation sections.  */
  size_t i;
  bpf_core_section_ref sec;
  core_relo_len += vec_safe_length (bpf_core_sections)
    * sizeof (struct btf_ext_section_header);

  FOR_EACH_VEC_ELT (*bpf_core_sections, i, sec)
    core_relo_len +=
      vec_safe_length (sec->relocs) * sizeof (struct btf_ext_reloc);

  dw2_asm_output_data (4, func_info_off, "func_info_offset");
  dw2_asm_output_data (4, func_info_len, "func_info_len");

  dw2_asm_output_data (4, line_info_off, "line_info_offset");
  dw2_asm_output_data (4, line_info_len, "line_info_len");

  dw2_asm_output_data (4, core_relo_off, "core_relo_offset");
  dw2_asm_output_data (4, core_relo_len, "core_relo_len");
}

/* Output a single CO-RE relocation record.  */

static void
output_asm_btfext_core_reloc (bpf_core_reloc_ref bpfcr)
{
  dw2_assemble_integer (4, gen_rtx_LABEL_REF (Pmode, bpfcr->bpfcr_insn_label));
  fprintf (asm_out_file, "\t%s bpfcr_insn\n", ASM_COMMENT_START);

  dw2_asm_output_data (4, bpfcr->bpfcr_type, "bpfcr_type");
  dw2_asm_output_data (4, bpfcr->bpfcr_astr_off, "bpfcr_astr_off");
  dw2_asm_output_data (4, bpfcr->bpfcr_kind, "bpfcr_kind");
}

/* Output all CO-RE relocation records for a section.  */

static void
output_btfext_core_relocs (bpf_core_section_ref sec)
{
  size_t i;
  bpf_core_reloc_ref bpfcr;
  FOR_EACH_VEC_ELT (*(sec->relocs), i, bpfcr)
    output_asm_btfext_core_reloc (bpfcr);
}

/* Output all CO-RE relocation sections.  */

static void
output_btfext_core_sections (void)
{
  size_t i;
  bpf_core_section_ref sec;
  FOR_EACH_VEC_ELT (*bpf_core_sections, i, sec)
    {
      /* BTF Ext section info. */
      dw2_asm_output_data (4, sizeof (struct btf_ext_reloc),
			   "btfext_secinfo_rec_size");

      /* Section name offset, refers to the offset of a string with the name of
	 the section to which these CORE relocations refer, e.g. '.text'.
	 The string is buffered in the BTF strings table.  */
      dw2_asm_output_data (4, sec->name_offset,  "btfext_secinfo_sec_name_off");
      dw2_asm_output_data (4, vec_safe_length (sec->relocs),
			   "btfext_secinfo_num_recs");

      output_btfext_core_relocs (sec);
    }
}

/* Initialize sections, labels, and data structures for BTF.ext output.  */

void
btf_ext_init (void)
{
  btf_ext_info_section = get_section (BTF_EXT_INFO_SECTION_NAME,
				      BTF_EXT_INFO_SECTION_FLAGS, NULL);

  ASM_GENERATE_INTERNAL_LABEL (btf_ext_info_section_label,
			       BTF_EXT_INFO_SECTION_LABEL,
			       btf_ext_label_num++);

  vec_alloc (bpf_core_sections, 1);
}

/* Output the entire .BTF.ext section.  */

void
btf_ext_output (void)
{
  output_btfext_header ();
  output_btfext_core_sections ();

  bpf_core_sections = NULL;
}

#include "gt-coreout.h"
