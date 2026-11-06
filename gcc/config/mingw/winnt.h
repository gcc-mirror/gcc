/* Subroutines for targets on Windows.
Copyright (C) 2024-2026 Free Software Foundation, Inc.

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
http://www.gnu.org/licenses/.  */

#ifndef GCC_MINGW_WINNT_H
#define GCC_MINGW_WINNT_H

#ifndef USED_FOR_TARGET

/* Structured Exception Handling unwind info.  */

struct seh_frame_state
{
  /* SEH records offsets relative to the lowest address of the fixed stack
     allocation.  If there is no frame pointer, these offsets are from the
     stack pointer; if there is a frame pointer, these offsets are from the
     value of the stack pointer when the frame pointer was established, i.e.
     the frame pointer minus the offset in the .seh_setframe directive.

     We do not distinguish these two cases, i.e. we consider that the offsets
     are always relative to the "current" stack pointer.  This means that we
     need to perform the fixed stack allocation before establishing the frame
     pointer whenever there are registers to be saved, and this is guaranteed
     by the prologue provided that we force the frame pointer to point at or
     below the lowest used register save area, see ix86_compute_frame_layout.

     This tracks the current stack pointer offset from the CFA.  */
  HOST_WIDE_INT sp_offset;

  /* The CFA is located at CFA_REG + CFA_OFFSET.  */
  HOST_WIDE_INT cfa_offset;
  rtx cfa_reg;

  /* The offset wrt the CFA where register N has been saved.  */
  HOST_WIDE_INT reg_offset[FIRST_PSEUDO_REGISTER];

  /* True if we are past the end of the epilogue.  */
  bool after_prologue;

  /* True if we are in the cold section.  */
  bool in_cold_section;
};

extern tree aarch64_handle_ms_abi_attribute (tree *, tree, tree, int, bool *);
extern tree mingw_handle_selectany_attribute (tree *, tree, tree, int, bool *);

extern void mingw_pe_asm_named_section (const char *, unsigned int, tree);
extern void mingw_pe_asm_lto_start (void);
extern void mingw_pe_asm_lto_end (void);
extern void mingw_pe_assemble_visibility (tree decl, int vis);
extern void mingw_pe_declare_type (FILE *, const char *, bool, bool);
extern void mingw_pe_encode_section_info (tree, rtx, int);
extern void mingw_pe_end_cold_function (FILE *, const char *, tree);
extern void mingw_pe_end_function (FILE *, const char *, tree);
extern void mingw_pe_file_end (void);
extern void mingw_pe_maybe_record_exported_symbol (tree, const char *, int);
extern void mingw_pe_record_stub (const char *, bool);
extern unsigned int mingw_pe_section_type_flags (tree, const char *, int);
extern section *mingw_pe_select_section (tree, int, unsigned HOST_WIDE_INT);
extern void mingw_pe_unique_section (tree, int);
extern bool mingw_pe_valid_dllimport_attribute_p (const_tree);

extern void mingw_pe_seh_init (FILE *);
extern void mingw_pe_seh_init_sections (void);
extern void mingw_pe_seh_cold_init (FILE *, const char *);
extern void mingw_pe_seh_emit_except_personality (rtx);
extern void mingw_pe_seh_emit_stackalloc (FILE *, struct seh_frame_state *,
					  HOST_WIDE_INT);
extern void mingw_pe_seh_end_prologue (FILE *);

#endif /* not USED_FOR_TARGET.  */

#endif /* GCC_MINGW_WINNT_H.  */
