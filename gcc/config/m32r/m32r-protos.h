/* Prototypes for m32r.c functions used in the md file & elsewhere.
   Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004
   Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* Function prototypes that cannot exist in v850.h due to dependency
   complications.  */
#define Mmode enum machine_mode

extern void   sbss_section (void);
extern void   sdata_section (void);
extern void   m32r_init (void);
extern void   m32r_init_expanders (void);
extern unsigned m32r_compute_frame_size (int);
extern void   m32r_expand_prologue (void);
extern void   m32r_finalize_pic (void);
extern int    direct_return (void);
extern void   m32r_load_pic_register (void);

#ifdef TREE_CODE
extern enum m32r_function_type m32r_compute_function_type (tree);
#endif /* TREE_CODE */

#ifdef RTX_CODE
extern int    easy_di_const (rtx);
extern int    easy_df_const (rtx);
extern rtx    gen_compare (enum rtx_code, rtx, rtx, int);
extern rtx    gen_split_move_double (rtx *);
extern int    m32r_address_code (rtx);
extern void   m32r_initialize_trampoline (rtx, rtx, rtx);
extern int    zero_and_one (rtx, rtx);
extern char * emit_cond_move (rtx *, rtx);
extern void   m32r_output_block_move (rtx, rtx *);
extern void   m32r_expand_block_move (rtx *);
extern void   m32r_print_operand (FILE *, rtx, int);
extern void   m32r_print_operand_address (FILE *, rtx);
extern int    m32r_not_same_reg (rtx, rtx);
extern int    m32r_hard_regno_rename_ok (unsigned int, unsigned int);
extern int    m32r_legitimate_pic_operand_p (rtx);
extern rtx    m32r_legitimize_pic_address (rtx, rtx);
extern rtx    m32r_return_addr (int);
extern rtx    m32r_function_symbol (const char *);

#ifdef HAVE_MACHINE_MODES
extern int    call_address_operand (rtx, Mmode);
extern int    call_operand (rtx, Mmode);
extern int    symbolic_operand (rtx, Mmode);
extern int    small_data_operand (rtx, Mmode);
extern int    addr24_operand (rtx, Mmode);
extern int    addr32_operand (rtx, Mmode);
extern int    call26_operand (rtx, Mmode);
extern int    seth_add3_operand (rtx, Mmode);
extern int    cmp_int16_operand (rtx, Mmode);
extern int    uint16_operand (rtx, Mmode);
extern int    reg_or_int16_operand (rtx, Mmode);
extern int    reg_or_uint16_operand (rtx, Mmode);
extern int    reg_or_cmp_int16_operand (rtx, Mmode);
extern int    two_insn_const_operand (rtx, Mmode);
extern int    move_src_operand (rtx, Mmode);
extern int    move_double_src_operand (rtx, Mmode);
extern int    move_dest_operand (rtx, Mmode);
extern int    eqne_comparison_operator (rtx, Mmode);
extern int    signed_comparison_operator (rtx, Mmode);
extern int    memreg_operand (rtx, Mmode);
extern int    small_insn_p (rtx, Mmode);
extern int    large_insn_p (rtx, Mmode);
extern int    conditional_move_operand (rtx, Mmode);
extern int    carry_compare_operand (rtx, Mmode);
extern int    m32r_block_immediate_operand (rtx, Mmode);
extern int    extend_operand (rtx, Mmode);
extern int    reg_or_eq_int16_operand (rtx, Mmode);
extern int    int8_operand (rtx, Mmode);
extern int    reg_or_zero_operand (rtx, Mmode);

#endif /* HAVE_MACHINE_MODES */

#endif /* RTX_CODE */

#undef  Mmode
