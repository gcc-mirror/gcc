/* Prototypes for exported functions of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2014 Free Software Foundation, Inc.
   Contributed by Andes Technology Corporation.

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


/* ------------------------------------------------------------------------ */

/* Defining Data Structures for Per-function Information.  */

extern void nds32_init_expanders (void);


/* Register Usage.  */

/* -- How Values Fit in Registers.  */

extern int nds32_hard_regno_nregs (int, enum machine_mode);
extern int nds32_hard_regno_mode_ok (int, enum machine_mode);


/* Register Classes.  */

extern enum reg_class nds32_regno_reg_class (int);


/* Stack Layout and Calling Conventions.  */

/* -- Basic Stack Layout.  */

extern rtx nds32_return_addr_rtx (int, rtx);

/* -- Eliminating Frame Pointer and Arg Pointer.  */

extern HOST_WIDE_INT nds32_initial_elimination_offset (unsigned int,
						       unsigned int);

/* -- Passing Arguments in Registers.  */

extern void nds32_init_cumulative_args (CUMULATIVE_ARGS *,
					tree, rtx, tree, int);

/* -- Function Entry and Exit.  */

extern void nds32_expand_prologue (void);
extern void nds32_expand_epilogue (void);
extern void nds32_expand_prologue_v3push (void);
extern void nds32_expand_epilogue_v3pop (void);

/* ------------------------------------------------------------------------ */

/* Auxiliary functions for auxiliary macros in nds32.h.  */

extern bool nds32_ls_333_p (rtx, rtx, rtx, enum machine_mode);

/* Auxiliary functions for expanding rtl used in nds32-multiple.md.  */

extern rtx nds32_expand_load_multiple (int, int, rtx, rtx);
extern rtx nds32_expand_store_multiple (int, int, rtx, rtx);
extern int nds32_expand_movmemqi (rtx, rtx, rtx, rtx);

/* Auxiliary functions for multiple load/store predicate checking.  */

extern bool nds32_valid_multiple_load_store (rtx, bool);

/* Auxiliary functions for stack operation predicate checking.  */

extern bool nds32_valid_stack_push_pop (rtx, bool);

/* Auxiliary functions for bit operation detection.  */

extern int nds32_can_use_bclr_p (int);
extern int nds32_can_use_bset_p (int);
extern int nds32_can_use_btgl_p (int);

extern int nds32_can_use_bitci_p (int);

/* Auxiliary function for 'Computing the Length of an Insn'.  */

extern int nds32_adjust_insn_length (rtx, int);

/* Auxiliary functions for FP_AS_GP detection.  */

extern bool nds32_symbol_load_store_p (rtx);
extern int nds32_fp_as_gp_check_available (void);

/* Auxiliary functions for jump table generation.  */

extern const char *nds32_output_casesi_pc_relative (rtx *);
extern const char *nds32_output_casesi (rtx *);

/* Auxiliary functions to identify 16 bit addresing mode.  */

extern enum nds32_16bit_address_type nds32_mem_format (rtx);

/* Auxiliary functions to output assembly code.  */

extern const char *nds32_output_16bit_store (rtx *, int);
extern const char *nds32_output_16bit_load (rtx *, int);
extern const char *nds32_output_32bit_store (rtx *, int);
extern const char *nds32_output_32bit_load (rtx *, int);
extern const char *nds32_output_32bit_load_s (rtx *, int);

/* Auxiliary functions to output stack push/pop instruction.  */

extern const char *nds32_output_stack_push (void);
extern const char *nds32_output_stack_pop (void);

/* Auxiliary functions to decide output alignment or not.  */

extern int nds32_target_alignment (rtx);

/* ------------------------------------------------------------------------ */
