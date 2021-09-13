/* Prototypes for exported functions defined in nvptx.c.
   Copyright (C) 2014-2021 Free Software Foundation, Inc.
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

/* The kind of shuffe instruction.  */
enum nvptx_shuffle_kind
{
  SHUFFLE_UP,
  SHUFFLE_DOWN,
  SHUFFLE_BFLY,
  SHUFFLE_IDX,
  SHUFFLE_MAX
};

extern void nvptx_declare_function_name (FILE *, const char *, const_tree decl);
extern void nvptx_declare_object_name (FILE *file, const char *name,
				       const_tree decl);
extern void nvptx_output_aligned_decl (FILE *file, const char *name,
				       const_tree decl,
				       HOST_WIDE_INT size, unsigned align);
extern void nvptx_function_end (FILE *);
extern void nvptx_output_skip (FILE *, unsigned HOST_WIDE_INT);
extern void nvptx_output_ascii (FILE *, const char *, unsigned HOST_WIDE_INT);
extern void nvptx_cpu_cpp_builtins (void);
extern void nvptx_register_pragmas (void);
extern unsigned int nvptx_data_alignment (const_tree, unsigned int);

#ifdef RTX_CODE
extern void nvptx_expand_oacc_fork (unsigned);
extern void nvptx_expand_oacc_join (unsigned);
extern void nvptx_expand_call (rtx, rtx);
extern rtx nvptx_gen_shuffle (rtx, rtx, rtx, nvptx_shuffle_kind);
extern rtx nvptx_expand_compare (rtx);
extern const char *nvptx_ptx_type_from_mode (machine_mode, bool);
extern const char *nvptx_output_mov_insn (rtx, rtx);
extern const char *nvptx_output_call_insn (rtx_insn *, rtx, rtx);
extern const char *nvptx_output_return (void);
extern const char *nvptx_output_set_softstack (unsigned);
extern const char *nvptx_output_simt_enter (rtx, rtx, rtx);
extern const char *nvptx_output_simt_exit (rtx);
extern const char *nvptx_output_red_partition (rtx, rtx);
extern const char *nvptx_output_atomic_insn (const char *, rtx *, int, int);
#endif
#endif
