/* coreout.h - Declarations and definitions related to
   BPF Compile Once - Run Everywhere (CO-RE) support.
   Copyright (C) 2021-2023 Free Software Foundation, Inc.

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


#ifndef __COREOUT_H
#define __COREOUT_H

#include <stdint.h>

#ifdef	__cplusplus
extern "C"
{
#endif

/* .BTF.ext information.  */

struct btf_ext_section_header
{
  uint32_t sec_name_off;
  uint32_t num_records;
};

/* A funcinfo record, in the .BTF.ext funcinfo section.  */
struct btf_ext_funcinfo
{
  uint32_t insn_off; /* Offset of the first instruction of the function.  */
  uint32_t type;     /* Type ID of a BTF_KIND_FUNC type.  */
};

/* A lineinfo record, in the .BTF.ext lineinfo section.  */
struct btf_ext_lineinfo
{
  uint32_t insn_off;      /* Offset of the instruction.  */
  uint32_t file_name_off; /* Offset of file name in BTF string table.  */
  uint32_t line_off;      /* Offset of source line in BTF string table.  */
  uint32_t line_col;      /* Line number (bits 31-11) and column (11-0).  */
};

enum btf_core_reloc_kind
{
  BPF_RELO_FIELD_BYTE_OFFSET = 0,
  BPF_RELO_FIELD_BYTE_SIZE = 1,
  BPF_RELO_FIELD_EXISTS = 2,
  BPF_RELO_FIELD_SIGNED = 3,
  BPF_RELO_FIELD_LSHIFT_U64 = 4,
  BPF_RELO_FIELD_RSHIFT_U64 = 5,
  BPF_RELO_TYPE_ID_LOCAL = 6,
  BPF_RELO_TYPE_ID_TARGET = 7,
  BPF_RELO_TYPE_EXISTS = 8,
  BPF_RELO_TYPE_SIZE = 9,
  BPF_RELO_ENUMVAL_EXISTS = 10,
  BPF_RELO_ENUMVAL_VALUE = 11
};

struct btf_ext_reloc
{
  uint32_t insn_off;       /* Offset of instruction to be patched. A
			      section-relative label at compile time.  */
  uint32_t type_id;        /* Type ID of the outermost containing entity, e.g.
			      the containing structure.  */
  uint32_t access_str_off; /* Offset of CO-RE accessor string in .BTF strings
			      section.  */
  uint32_t kind;           /* An enum btf_core_reloc_kind. Note that it always
			      takes 32 bits.  */
};

struct btf_ext_header
{
  uint16_t magic;		/* Magic number (BTF_MAGIC).  */
  uint8_t  version;		/* Data format version (BTF_VERSION).  */
  uint8_t  flags;		/* Flags. Currently unused.  */
  uint32_t hdr_len;		/* Length of this header in bytes.  */

  /* Following offsets are relative to the end of this header, in bytes.
     Following lengths are in bytes.  */
  uint32_t func_info_off;	/* Offset of funcinfo section.  */
  uint32_t func_info_len;	/* Length of funcinfo section.  */
  uint32_t line_info_off;	/* Offset of lineinfo section.  */
  uint32_t line_info_len;	/* Length of lineinfo section.  */

  uint32_t core_relo_off;	/* Offset of CO-RE relocation section.  */
  uint32_t core_relo_len;	/* Length of CO-RE relocation section.  */
};

extern void btf_ext_init (void);
extern void btf_ext_output (void);

extern void bpf_core_reloc_add (const tree, const char *, vec<unsigned int> *,
				rtx_code_label *, enum btf_core_reloc_kind);
extern int bpf_core_get_sou_member_index (ctf_container_ref, const tree);

#ifdef	__cplusplus
}
#endif

#endif /* __COREOUT_H */
