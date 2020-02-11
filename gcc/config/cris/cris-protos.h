/* Definitions for GCC.  Part of the machine description for CRIS.
   Copyright (C) 1998-2020 Free Software Foundation, Inc.
   Contributed by Axis Communications.

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

/* Prototypes for the CRIS port.  */

extern bool cris_simple_epilogue (void);
#ifdef RTX_CODE
extern const char *cris_op_str (rtx);
extern void cris_notice_update_cc (rtx, rtx_insn *);
extern bool cris_reload_address_legitimized (rtx, machine_mode, int, int, int);
extern int cris_side_effect_mode_ok (enum rtx_code, rtx *, int, int,
                                     int, int, int);
extern bool cris_cc0_user_requires_cmp (rtx_insn *);
extern rtx cris_return_addr_rtx (int, rtx);
extern rtx cris_split_movdx (rtx *);
extern int cris_legitimate_pic_operand (rtx);
extern enum cris_symbol_type cris_symbol_type_of (const_rtx);
extern bool cris_valid_pic_const (const_rtx, bool);
extern bool cris_legitimate_constant_p (machine_mode, rtx);
extern bool cris_constant_index_p (const_rtx);
extern bool cris_base_p (const_rtx, bool);
extern bool cris_base_or_autoincr_p (const_rtx, bool);
extern bool cris_bdap_index_p (const_rtx, bool);
extern void cris_reduce_compare (rtx *, rtx *, rtx *);
extern bool cris_biap_index_p (const_rtx, bool);
extern bool cris_legitimate_address_p (machine_mode, rtx, bool);
extern bool cris_store_multiple_op_p (rtx);
extern bool cris_movem_load_rest_p (rtx, int);
extern void cris_asm_output_symbol_ref (FILE *, rtx);
extern int cris_cfun_uses_pic_table (void);
extern void cris_asm_output_case_end (FILE *, int, rtx_insn *);
extern rtx cris_gen_movem_load (rtx, rtx, int);
extern rtx cris_emit_movem_store (rtx, rtx, int, bool);
extern void cris_expand_pic_call_address (rtx *, rtx *);
extern void cris_order_for_addsi3 (rtx *, int);
extern void cris_emit_trap_for_misalignment (rtx);
#endif /* RTX_CODE */
extern void cris_asm_output_label_ref (FILE *, char *);
extern void cris_asm_output_ident (const char *);
extern void cris_expand_prologue (void);
extern void cris_expand_epilogue (void);
extern void cris_expand_return (bool);
extern bool cris_return_address_on_stack_for_return (void);
extern bool cris_return_address_on_stack (void);
extern void cris_pragma_expand_mul (struct cpp_reader *);

/* Need one that returns an int; usable in expressions.  */
extern int cris_fatal (char *);

extern int cris_initial_elimination_offset (int, int);

extern void cris_init_expanders (void);
