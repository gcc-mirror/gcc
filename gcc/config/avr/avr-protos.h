/* Prototypes for exported functions defined in avr.c
   
   Copyright (C) 2000, 2001 Free Software Foundation, Inc.
   Contributed by Denis Chertykov (denisc@overta.ru)

   This file is part of GNU CC.

   GNU CC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GNU CC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU CC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */


extern int    function_arg_regno_p              PARAMS ((int r));
extern void   asm_file_start                    PARAMS ((FILE *file));
extern void   asm_file_end                      PARAMS ((FILE *file));
extern void   avr_init_once                     PARAMS ((void));
extern void   avr_override_options              PARAMS ((void));
extern char * avr_change_section                PARAMS ((char *sect_name));
extern int    avr_ret_register                  PARAMS ((void));
extern enum reg_class class_likely_spilled_p    PARAMS ((int c));
extern enum reg_class avr_regno_reg_class       PARAMS ((int r));
extern enum reg_class avr_reg_class_from_letter PARAMS ((int c));
extern int    frame_pointer_required_p          PARAMS ((void));
extern void   asm_globalize_label         PARAMS ((FILE *file, const char *name));
extern void   order_regs_for_local_alloc  PARAMS ((void));
extern int    initial_elimination_offset  PARAMS ((int from, int to));
extern void   progmem_section             PARAMS ((void));
extern int    mask_one_bit_p              PARAMS ((HOST_WIDE_INT mask));
extern void   gas_output_limited_string PARAMS ((FILE *file, const char *str));
extern void   gas_output_ascii          PARAMS ((FILE *file, const char *str,
							 size_t length));
#ifdef TREE_CODE
extern void   asm_output_external          PARAMS ((FILE *file, tree decl,
						   char *name));
extern void   unique_section               PARAMS ((tree decl, int reloc));
extern void   encode_section_info          PARAMS ((tree decl));
extern int    avr_progmem_p                PARAMS ((tree decl));


#ifdef RTX_CODE /* inside TREE_CODE */
extern rtx    avr_function_value           PARAMS ((tree type, tree func));
extern void   init_cumulative_args         PARAMS ((CUMULATIVE_ARGS *cum,
						   tree fntype, rtx libname,
						   int indirect));
extern rtx    function_arg         PARAMS ((CUMULATIVE_ARGS *cum,
					   enum machine_mode mode,
					   tree type, int named));


#endif /* RTX_CODE inside TREE_CODE */

#ifdef HAVE_MACHINE_MODES /* inside TREE_CODE */
extern void   function_arg_advance PARAMS ((CUMULATIVE_ARGS *cum,
					   enum machine_mode mode, tree type,
					   int named));
#endif /* HAVE_MACHINE_MODES inside TREE_CODE*/
#endif /* TREE_CODE */

#ifdef RTX_CODE
extern void   asm_output_external_libcall PARAMS ((FILE *file, rtx symref));
extern int    legitimate_address_p    PARAMS ((enum machine_mode mode, rtx x,
					int strict));
extern void   machine_dependent_reorg PARAMS ((rtx first_insn));
extern int    compare_diff_p  PARAMS ((rtx insn));
extern const char * output_movqi    PARAMS ((rtx insn, rtx operands[], int *l));
extern const char * output_movhi    PARAMS ((rtx insn, rtx operands[], int *l));
extern const char * out_movqi_r_mr  PARAMS ((rtx insn, rtx op[], int *l));
extern const char * out_movqi_mr_r  PARAMS ((rtx insn, rtx op[], int *l));
extern const char * out_movhi_r_mr  PARAMS ((rtx insn, rtx op[], int *l));
extern const char * out_movhi_mr_r  PARAMS ((rtx insn, rtx op[], int *l));
extern const char * out_movsi_r_mr  PARAMS ((rtx insn, rtx op[], int *l));
extern const char * out_movsi_mr_r  PARAMS ((rtx insn, rtx op[], int *l));
extern const char * output_movsisf  PARAMS ((rtx insn, rtx operands[], int *l));
extern const char * out_tstsi       PARAMS ((rtx insn, int *l));
extern const char * out_tsthi       PARAMS ((rtx insn, int *l));
extern const char * ret_cond_branch PARAMS ((rtx x, int len, int reverse));

extern const char * ashlqi3_out PARAMS ((rtx insn, rtx operands[], int *len));
extern const char * ashlhi3_out PARAMS ((rtx insn, rtx operands[], int *len));
extern const char * ashlsi3_out PARAMS ((rtx insn, rtx operands[], int *len));

extern const char * ashrqi3_out PARAMS ((rtx insn, rtx operands[], int *len));
extern const char * ashrhi3_out PARAMS ((rtx insn, rtx operands[], int *len));
extern const char * ashrsi3_out PARAMS ((rtx insn, rtx operands[], int *len));

extern const char * lshrqi3_out PARAMS ((rtx insn, rtx operands[], int *len));
extern const char * lshrhi3_out PARAMS ((rtx insn, rtx operands[], int *len));
extern const char * lshrsi3_out PARAMS ((rtx insn, rtx operands[], int *len));

extern void avr_output_bld PARAMS ((rtx operands[], int bit_nr));
extern void avr_output_addr_vec_elt PARAMS ((FILE *stream, int value));

extern enum reg_class preferred_reload_class PARAMS ((rtx x,
						     enum reg_class class));
extern int    avr_address_cost       PARAMS ((rtx x));
extern int    extra_constraint       PARAMS ((rtx x, int c));
extern rtx    legitimize_address     PARAMS ((rtx x, rtx oldx,
					     enum machine_mode mode));
extern int    adjust_insn_length     PARAMS ((rtx insn, int len));
extern rtx    avr_libcall_value      PARAMS ((enum machine_mode mode));
extern const char * output_reload_inhi PARAMS ((rtx insn, rtx *operands,
						int *len));
extern const char * output_reload_insisf PARAMS ((rtx insn, rtx *operands,
						int *len));
extern int    default_rtx_costs      PARAMS ((rtx X, RTX_CODE code,
					     RTX_CODE outer_code));
extern enum reg_class secondary_input_reload_class PARAMS ((enum reg_class,
							   enum machine_mode,
							   rtx));
extern void   notice_update_cc       PARAMS ((rtx body, rtx insn));
extern void   print_operand          PARAMS ((FILE *file, rtx x, int code));
extern void   print_operand_address  PARAMS ((FILE *file, rtx addr));
extern int    reg_unused_after       PARAMS ((rtx insn, rtx reg));
extern int    _reg_unused_after      PARAMS ((rtx insn, rtx reg));
extern int    avr_jump_mode          PARAMS ((rtx x, rtx insn));
extern int    byte_immediate_operand PARAMS ((register rtx op,
					     enum machine_mode mode));
extern int    test_hard_reg_class    PARAMS ((enum reg_class class, rtx x));
extern int    jump_over_one_insn_p   PARAMS ((rtx insn, rtx dest));

extern int    avr_hard_regno_mode_ok PARAMS ((int regno,
					     enum machine_mode mode));
extern int    call_insn_operand      PARAMS ((rtx op, enum machine_mode mode));
extern void   final_prescan_insn     PARAMS ((rtx insn, rtx *operand,
					      int num_operands));
extern int    avr_simplify_comparision_p PARAMS ((enum machine_mode mode,
					      RTX_CODE operator, rtx x));
extern RTX_CODE avr_normalize_condition  PARAMS ((RTX_CODE condition));
extern int    compare_eq_p           PARAMS ((rtx insn));
extern void   out_shift_with_cnt     PARAMS ((const char *template, rtx insn,
					      rtx operands[], int *len,
					      int t_len));
extern int    const_int_pow2_p       PARAMS ((rtx x));
extern int    avr_peep2_scratch_safe PARAMS ((rtx reg_rtx));
#endif /* RTX_CODE */

#ifdef HAVE_MACHINE_MODES
extern int    class_max_nregs        PARAMS ((enum reg_class class,
					     enum machine_mode mode));
#endif /* HAVE_MACHINE_MODES */

#ifdef REAL_VALUE_TYPE

extern void   asm_output_float       PARAMS ((FILE *file, REAL_VALUE_TYPE n));

#endif
