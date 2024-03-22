/* Prototypes of target machine functions, Lattice Mico32 architecture.
   Contributed by Jon Beniston <jon@beniston.com>

   Copyright (C) 2009-2024 Free Software Foundation, Inc.

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

extern int lm32_return_in_memory (tree type);
extern void lm32_declare_object (FILE *stream, char *name, char *init_string, 
                                 char *final_string, int size);
extern void lm32_expand_prologue (void);
extern void lm32_expand_epilogue (void);
extern void lm32_print_operand (FILE *file, rtx op, int letter);
extern void lm32_print_operand_address (FILE *file, rtx addr);
extern HOST_WIDE_INT lm32_compute_initial_elimination_offset (int from, 
                                                             int to);
extern int lm32_can_use_return (void);
extern rtx lm32_return_addr_rtx (int count, rtx frame);
extern int lm32_expand_block_move (rtx *);
extern int nonpic_symbol_mentioned_p (rtx);
extern rtx lm32_legitimize_pic_address (rtx, machine_mode, rtx);
extern void lm32_expand_scc (rtx operands[]);
extern void lm32_expand_conditional_branch (rtx operands[]);
extern bool lm32_move_ok (machine_mode, rtx operands[2]);
