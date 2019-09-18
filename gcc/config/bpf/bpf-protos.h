/* Definition of eBPF target for GNU compiler.
   Copyright (C) 2019 Free Software Foundation, Inc.

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

#ifndef GCC_BPF_PROTOS_H
#define GCC_BPF_PROTOS_H

/* Routines implemented in bpf.c.  */

extern HOST_WIDE_INT bpf_initial_elimination_offset (int, int);
extern const char *bpf_output_call (rtx);
extern void bpf_target_macros (cpp_reader *);
extern void bpf_print_operand (FILE *, rtx, int);
extern void bpf_print_operand_address (FILE *, rtx);
extern void bpf_expand_prologue (void);
extern void bpf_expand_epilogue (void);

#endif /* ! GCC_BPF_PROTOS_H */
