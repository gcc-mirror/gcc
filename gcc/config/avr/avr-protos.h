/* Prototypes for exported functions defined in avr.c
   
   Copyright (C) 2000 Free Software Foundation, Inc.
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


extern void   avr_output_ascii                  PARAMS ((FILE *file,
							const char *p,
							int size));
extern int    function_arg_regno_p              PARAMS ((int r));
extern void   asm_file_start                    PARAMS ((FILE *file));
extern void   asm_file_end                      PARAMS ((FILE *file));
extern void   avr_init_once                     PARAMS ((void));
extern void   avr_override_options              PARAMS ((void));
extern char * avr_change_section                PARAMS ((char *sect_name));
extern int    avr_ret_register                  PARAMS((void));
extern enum reg_class class_likely_spilled_p    PARAMS ((int c));
extern enum reg_class avr_regno_reg_class       PARAMS ((int r));
extern enum reg_class avr_reg_class_from_letter PARAMS ((int c));
extern int    frame_pointer_required_p          PARAMS ((void));
extern void   asm_globalize_label               PARAMS ((FILE *file,
							const char *name));
extern void   order_regs_for_local_alloc        PARAMS ((void));
extern int    initial_elimination_offset        PARAMS ((int from, int to));
extern void   function_prologue                 PARAMS ((FILE *file, int size));
extern void   function_epilogue                 PARAMS ((FILE *file, int size));
extern void   progmem_section                   PARAMS ((void));
extern int    mask_one_bit_p                    PARAMS ((HOST_WIDE_INT mask));

#ifdef TREE_CODE
extern void   asm_output_external          PARAMS ((FILE *file, tree decl,
						   char *name));
extern void   unique_section               PARAMS ((tree decl, int reloc));
extern void   encode_section_info          PARAMS ((tree decl));
extern void   asm_output_section_name      PARAMS ((FILE *file, tree decl,
						   const char *name,
						   int reloc));
extern int    valid_machine_type_attribute PARAMS ((tree type, tree attributes,
						   tree identifier,
						   tree args));
extern int    valid_machine_decl_attribute PARAMS ((tree decl, tree attributes,
						   tree attr, tree args));

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
extern char * out_movqi_r_mr  PARAMS ((rtx insn, rtx op[], int *l));
extern char * out_movqi_mr_r  PARAMS ((rtx insn, rtx op[], int *l));
extern char * out_movhi_r_mr  PARAMS ((rtx insn, rtx op[], int *l));
extern char * out_movhi_mr_r  PARAMS ((rtx insn, rtx op[], int *l));
extern char * out_movsi_r_mr  PARAMS ((rtx insn, rtx op[], int *l));
extern char * out_movsi_mr_r  PARAMS ((rtx insn, rtx op[], int *l));
extern char * output_movsisf  PARAMS ((rtx insn, rtx operands[],
			      	      int which_alternative));
extern char * out_tstsi       PARAMS ((rtx insn, int *l));
extern char * out_tsthi       PARAMS ((rtx insn, int *l));
extern char * ret_cond_branch PARAMS ((RTX_CODE cond, int len));

extern char * ashlqi3_out     PARAMS ((rtx insn, rtx operands[], int *len));
extern char * ashlhi3_out     PARAMS ((rtx insn, rtx operands[], int *len));
extern char * ashlsi3_out     PARAMS ((rtx insn, rtx operands[], int *len));

extern char * ashrqi3_out     PARAMS ((rtx insn, rtx operands[], int *len));
extern char * ashrhi3_out     PARAMS ((rtx insn, rtx operands[], int *len));
extern char * ashrsi3_out     PARAMS ((rtx insn, rtx operands[], int *len));

extern char * lshrqi3_out     PARAMS ((rtx insn, rtx operands[], int *len));
extern char * lshrhi3_out     PARAMS ((rtx insn, rtx operands[], int *len));
extern char * lshrsi3_out     PARAMS ((rtx insn, rtx operands[], int *len));

extern enum reg_class preferred_reload_class PARAMS ((rtx x,
						     enum reg_class class));
extern int    avr_address_cost       PARAMS ((rtx x));
extern int    extra_constraint       PARAMS ((rtx x, char c));
extern rtx    legitimize_address     PARAMS ((rtx x, rtx oldx,
					     enum machine_mode mode));
extern int    adjust_insn_length     PARAMS ((rtx insn, int len));
extern rtx    avr_libcall_value      PARAMS ((enum machine_mode mode));
extern char * output_reload_inhi     PARAMS ((rtx insn, rtx *operands,
					     int which_alternative));
extern char * output_reload_insisf   PARAMS ((rtx insn, rtx *operands,
					     int which_alternative));
extern int    default_rtx_costs      PARAMS ((rtx X, RTX_CODE code,
					     RTX_CODE outer_code));
extern void   asm_output_char        PARAMS ((FILE *file, rtx value));
extern void   asm_output_short       PARAMS ((FILE *file, rtx value));
extern void   asm_output_byte        PARAMS ((FILE *file, char value));
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

#endif /* RTX_CODE */

#ifdef HAVE_MACHINE_MODES
extern int    class_max_nregs        PARAMS ((enum reg_class class,
					     enum machine_mode mode));
#endif /* HAVE_MACHINE_MODES */

#ifdef REAL_VALUE_TYPE

extern void   asm_output_float       PARAMS ((FILE *file, REAL_VALUE_TYPE n));

#endif 


