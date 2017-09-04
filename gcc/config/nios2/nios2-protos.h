/* Subroutine declarations for Altera Nios II target support.
   Copyright (C) 2012-2017 Free Software Foundation, Inc.
   Contributed by Jonah Graham (jgraham@altera.com).
   Contributed by Mentor Graphics, Inc.

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

#ifndef GCC_NIOS2_PROTOS_H
#define GCC_NIOS2_PROTOS_H

extern int nios2_initial_elimination_offset (int, int);
extern int nios2_can_use_return_insn (void);
extern void nios2_expand_prologue (void);
extern void nios2_expand_epilogue (bool);
extern bool nios2_expand_return (void);
extern void nios2_function_profiler (FILE *, int);

#ifdef RTX_CODE
extern bool nios2_emit_move_sequence (rtx *, machine_mode);
extern void nios2_emit_expensive_div (rtx *, machine_mode);
extern void nios2_adjust_call_address (rtx *, rtx);

extern rtx nios2_get_return_address (int);
extern void nios2_set_return_address (rtx, rtx);

extern bool nios2_validate_compare (machine_mode, rtx *, rtx *, rtx *);
extern bool nios2_validate_fpu_compare (machine_mode, rtx *, rtx *, rtx *,
					bool);

extern bool nios2_fpu_insn_enabled (enum n2fpu_code);
extern const char * nios2_fpu_insn_asm (enum n2fpu_code);
extern const char * nios2_add_insn_asm (rtx_insn *, rtx *);

extern bool nios2_legitimate_pic_operand_p (rtx);
extern bool gprel_constant_p (rtx);
extern bool nios2_regno_ok_for_base_p (int, bool);
extern bool nios2_unspec_reloc_p (rtx);

extern int nios2_label_align (rtx);
extern bool nios2_cdx_narrow_form_p (rtx_insn *);

extern bool pop_operation_p (rtx);
extern bool ldstwm_operation_p (rtx, bool);
extern bool gen_ldstwm_peep (bool, int, rtx, rtx *);

extern void nios2_adjust_reg_alloc_order (void);

extern pad_direction nios2_block_reg_padding (machine_mode, tree, int);

#endif /* RTX_CODE */

#endif /* GCC_NIOS2_PROTOS_H */
