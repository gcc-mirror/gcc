/* Definitions of target machine for GNU compiler, for Xilinx MicroBlaze.
   Copyright (C) 2009-2014 Free Software Foundation, Inc.

   This file is part of GCC.

   Contributed by Michael Eager <eager@eagercon.com>.

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

#ifndef GCC_MICROBLAZE_PROTOS_H
#define GCC_MICROBLAZE_PROTOS_H

#ifdef RTX_CODE
extern int pic_address_needs_scratch (rtx);
extern void expand_block_move        (rtx *);
extern void microblaze_expand_prologue (void);
extern void microblaze_expand_epilogue (void);
extern void override_options (void);
extern int microblaze_expand_shift (rtx *);
extern bool microblaze_expand_move (enum machine_mode, rtx *);
extern bool microblaze_expand_block_move (rtx, rtx, rtx, rtx);
extern void microblaze_expand_divide (rtx *);
extern void microblaze_expand_conditional_branch (enum machine_mode, rtx *); 
extern void microblaze_expand_conditional_branch_sf (rtx *); 
extern int microblaze_can_use_return_insn (void);
extern void print_operand (FILE *, rtx, int);
extern void print_operand_address (FILE *, rtx);
extern void init_cumulative_args (CUMULATIVE_ARGS *,tree, rtx);
extern bool microblaze_legitimate_address_p (enum machine_mode, rtx, bool);
extern int microblaze_is_interrupt_variant (void);
extern rtx microblaze_return_addr (int, rtx);
extern int simple_memory_operand (rtx, enum machine_mode);
extern int double_memory_operand (rtx, enum machine_mode);

extern int microblaze_regno_ok_for_base_p (int, int);
extern HOST_WIDE_INT microblaze_initial_elimination_offset (int, int);
extern void microblaze_declare_object (FILE *, const char *, const char *,
   const char *, int);
extern void microblaze_asm_output_ident (const char *);
extern int microblaze_legitimate_pic_operand (rtx);
extern bool microblaze_tls_referenced_p (rtx);
extern int symbol_mentioned_p (rtx);
extern int label_mentioned_p (rtx);
extern bool microblaze_cannot_force_const_mem (enum machine_mode, rtx);
#endif  /* RTX_CODE */

/* Declare functions in microblaze-c.c.  */
extern void microblaze_cpp_define (struct cpp_reader *); 

#endif  /* GCC_MICROBLAZE_PROTOS_H */
