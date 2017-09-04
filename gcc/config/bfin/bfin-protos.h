/* Prototypes for Blackfin functions used in the md file & elsewhere.
   Copyright (C) 2005-2017 Free Software Foundation, Inc.

   This file is part of GNU CC.

   GNU CC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GNU CC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* Function prototypes that cannot exist in bfin.h due to dependency
   complications.  */
#ifndef GCC_BFIN_PROTOS_H
#define GCC_BFIN_PROTOS_H

/* For the anomaly 05-00-0245 */
#define WA_SPECULATIVE_LOADS 0x00000001
#define ENABLE_WA_SPECULATIVE_LOADS \
  (bfin_workarounds & WA_SPECULATIVE_LOADS)

/* For the anomaly 05-00-0244 */
#define WA_SPECULATIVE_SYNCS 0x00000002
#define ENABLE_WA_SPECULATIVE_SYNCS \
  (bfin_workarounds & WA_SPECULATIVE_SYNCS)

/* For the anomaly 05-00-0371 */
#define WA_RETS 0x00000004
#define ENABLE_WA_RETS \
  (bfin_workarounds & WA_RETS)

/* For the anomaly 05-00-0426 */
#define WA_INDIRECT_CALLS 0x00000008
#define ENABLE_WA_INDIRECT_CALLS \
  ((bfin_workarounds & WA_INDIRECT_CALLS) && !TARGET_ICPLB)

#define WA_05000257 0x00000010
#define ENABLE_WA_05000257 \
  (bfin_workarounds & WA_05000257)

#define WA_05000283 0x00000020
#define ENABLE_WA_05000283 \
  (bfin_workarounds & WA_05000283)

#define WA_05000315 0x00000040
#define ENABLE_WA_05000315 \
  (bfin_workarounds & WA_05000315)

/* For the anomaly 05-00-0312 */
#define WA_LOAD_LCREGS 0x00000080
#define ENABLE_WA_LOAD_LCREGS \
  (bfin_workarounds & WA_LOAD_LCREGS)

#define WA_05000074 0x00000100
#define ENABLE_WA_05000074 \
  (bfin_workarounds & WA_05000074)

extern bool function_arg_regno_p (int);

extern const char *output_load_immediate (rtx *);
extern const char *output_casesi_internal (rtx *);
extern char *bfin_asm_long (void);
extern char *bfin_asm_short (void);
extern int log2constp (unsigned HOST_WIDE_INT);

extern void init_cumulative_args (CUMULATIVE_ARGS *, tree, rtx);	  
extern HOST_WIDE_INT bfin_initial_elimination_offset (int, int);

extern int effective_address_32bit_p (rtx, machine_mode);
extern int symbolic_reference_mentioned_p (rtx);
extern rtx bfin_gen_compare (rtx, machine_mode);
extern bool expand_move (rtx *, machine_mode);
extern void bfin_expand_call (rtx, rtx, rtx, rtx, int);
extern bool bfin_longcall_p (rtx, int);
extern bool bfin_dsp_memref_p (rtx);
extern bool bfin_expand_movmem (rtx, rtx, rtx, rtx);

extern enum reg_class secondary_input_reload_class (enum reg_class,
						    machine_mode,
						    rtx);
extern enum reg_class secondary_output_reload_class (enum reg_class,
						     machine_mode,
						     rtx);
extern char *section_asm_op_1 (SECT_ENUM_T);
extern char *section_asm_op (SECT_ENUM_T);
extern void print_operand (FILE *,  rtx, char);
extern void print_address_operand (FILE *, rtx);
extern void split_di (rtx [], int, rtx [], rtx []);
extern int split_load_immediate (rtx []);
extern void emit_pic_move (rtx *, machine_mode);
extern void asm_conditional_branch (rtx_insn *, rtx *, int, int);
extern rtx bfin_gen_compare (rtx, machine_mode);

extern unsigned bfin_local_alignment (tree, unsigned);
extern rtx bfin_va_arg (tree, tree);

extern void bfin_expand_prologue (void);
extern void bfin_expand_epilogue (int, int, bool);
extern int analyze_push_multiple_operation (rtx);
extern int analyze_pop_multiple_operation (rtx);
extern void output_push_multiple (rtx, rtx *);
extern void output_pop_multiple (rtx, rtx *);
extern int bfin_hard_regno_rename_ok (unsigned int, unsigned int);
extern rtx bfin_return_addr_rtx (int);
extern void bfin_hardware_loop (void);

#endif

