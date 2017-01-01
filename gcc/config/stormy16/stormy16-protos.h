/* Prototypes for exported functions defined in xstormy16.c
   Copyright (C) 2000-2017 Free Software Foundation, Inc.
   Contributed by Red Hat, Inc.

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



extern struct xstormy16_stack_layout xstormy16_compute_stack_layout (void);
extern void xstormy16_expand_prologue (void);
extern void xstormy16_expand_epilogue (void);
extern int xstormy16_initial_elimination_offset (int, int);
extern int direct_return (void);
extern int xstormy16_interrupt_function_p (void);
extern int xstormy16_epilogue_uses (int);
extern void xstormy16_function_profiler (void);

#if defined (TREE_CODE)
extern void xstormy16_asm_output_aligned_common (FILE *, tree, const char *,
						 int, int, int);
#endif

#if defined (TREE_CODE) && defined (RTX_CODE)
extern void xstormy16_initialize_trampoline (rtx, rtx, rtx);
#endif

#ifdef RTX_CODE
extern void xstormy16_emit_cbranch (enum rtx_code, rtx, rtx, rtx);
extern char *xstormy16_output_cbranch_hi (rtx, const char *, int, rtx_insn *);
extern char *xstormy16_output_cbranch_si (rtx, const char *, int, rtx_insn *);

extern void xstormy16_expand_casesi (rtx, rtx, rtx, rtx, rtx);
extern void xstormy16_output_addr_vec (FILE *, rtx, rtx);
extern void xstormy16_expand_call (rtx, rtx, rtx);
extern void xstormy16_expand_iorqi3 (rtx *);
extern void xstormy16_expand_andqi3 (rtx *);
#endif

#if defined (HAVE_MACHINE_MODES) && defined (RTX_CODE)
extern void xstormy16_split_cbranch (machine_mode, rtx, rtx, rtx);
extern int  short_memory_operand (rtx, machine_mode);
extern int  nonimmediate_nonstack_operand (rtx, machine_mode);
extern enum reg_class xstormy16_secondary_reload_class 
 (enum reg_class, machine_mode, rtx);
extern void xstormy16_split_move (machine_mode, rtx, rtx);
extern void xstormy16_expand_move (machine_mode, rtx, rtx);
extern void xstormy16_expand_arith (machine_mode, enum rtx_code, 
				    rtx, rtx, rtx);
extern const char * xstormy16_output_shift (machine_mode, enum rtx_code, 
					    rtx, rtx, rtx);
extern int  xstormy16_below100_symbol (rtx, machine_mode);
extern int  xstormy16_splittable_below100_operand (rtx, machine_mode);
extern bool xstormy16_legitimate_address_p (machine_mode, rtx, bool);
#endif

