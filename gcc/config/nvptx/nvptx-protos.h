/* Prototypes for exported functions defined in nvptx.c.
   Copyright (C) 2014-2015 Free Software Foundation, Inc.
   Contributed by Bernd Schmidt <bernds@codesourcery.com>

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

#ifndef GCC_NVPTX_PROTOS_H
#define GCC_NVPTX_PROTOS_H

extern void nvptx_declare_function_name (FILE *, const char *, const_tree decl);
extern void nvptx_declare_object_name (FILE *file, const char *name,
				       const_tree decl);
extern void nvptx_record_needed_fndecl (tree decl);
extern void nvptx_function_end (FILE *);
extern void nvptx_output_skip (FILE *, unsigned HOST_WIDE_INT);
extern void nvptx_output_ascii (FILE *, const char *, unsigned HOST_WIDE_INT);
extern void nvptx_register_pragmas (void);
extern const char *nvptx_section_for_decl (const_tree);

#ifdef RTX_CODE
extern void nvptx_expand_call (rtx, rtx);
extern rtx nvptx_expand_compare (rtx);
extern const char *nvptx_ptx_type_from_mode (machine_mode, bool);
extern const char *nvptx_output_call_insn (rtx_insn *, rtx, rtx);
extern const char *nvptx_output_return (void);
extern machine_mode nvptx_underlying_object_mode (rtx);
extern const char *nvptx_section_from_addr_space (addr_space_t);
extern bool nvptx_hard_regno_mode_ok (int, machine_mode);
extern addr_space_t nvptx_addr_space_from_address (rtx);
extern rtx nvptx_maybe_convert_symbolic_operand (rtx);
#endif
#endif
