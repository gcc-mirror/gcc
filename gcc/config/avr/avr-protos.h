/* Prototypes for tm_p.h for AVR 8-bit microcontrollers.
   Copyright (C) 2000-2025 Free Software Foundation, Inc.
   Contributed by Denis Chertykov (chertykov@gmail.com)

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


extern bool avr_function_arg_regno_p (int r);
extern void avr_cpu_cpp_builtins (cpp_reader * pfile);
extern enum reg_class avr_regno_reg_class (int r);
extern void asm_globalize_label (FILE *file, const char *name);
extern void avr_adjust_reg_alloc_order (void);
extern int avr_initial_elimination_offset (int from, int to);
extern int avr_simple_epilogue (void);
extern bool avr_hard_regno_rename_ok (unsigned int, unsigned int);
extern rtx avr_return_addr_rtx (int count, rtx tem);
extern void avr_register_target_pragmas (void);
extern void avr_init_expanders (void);

#ifdef TREE_CODE
extern void avr_asm_output_aligned_decl_common (FILE*, tree, const char*, unsigned HOST_WIDE_INT, unsigned int, bool);
extern void avr_asm_asm_output_aligned_bss (FILE *, tree, const char *, unsigned HOST_WIDE_INT, int, void (*) (FILE *, tree, const char *, unsigned HOST_WIDE_INT, int));
extern void avr_declare_function_name (FILE *, const char *, tree);
extern void asm_output_external (FILE *file, tree decl, char *name);
extern int avr_progmem_p (tree decl, tree attributes);
extern bool avr_addr_space_supported_p (addr_space_t, location_t loc = UNKNOWN_LOCATION);

#ifdef RTX_CODE /* inside TREE_CODE */
extern void avr_init_cumulative_args (CUMULATIVE_ARGS*, tree, rtx, tree);
#endif /* RTX_CODE inside TREE_CODE */

#endif /* TREE_CODE */

#ifdef RTX_CODE
extern rtx avr_chunk (machine_mode mode, rtx x, int n);
extern rtx avr_byte (rtx x, int n);
extern rtx avr_word (rtx x, int n);
extern int8_t avr_int8 (rtx x, int n);
extern uint8_t avr_uint8 (rtx x, int n);
extern int16_t avr_int16 (rtx x, int n);
extern uint16_t avr_uint16 (rtx x, int n);
extern const char *output_movqi (rtx_insn *insn, rtx operands[], int *l);
extern const char *output_movhi (rtx_insn *insn, rtx operands[], int *l);
extern const char *output_movsisf (rtx_insn *insn, rtx operands[], int *l);
extern const char *avr_out_set_some (rtx_insn *, rtx*, int*);
extern const char *avr_out_tstsi (rtx_insn *, rtx*, int*);
extern const char *avr_out_tsthi (rtx_insn *, rtx*, int*);
extern const char *avr_out_tstpsi (rtx_insn *, rtx*, int*);
extern const char *avr_out_compare (rtx_insn *, rtx*, int*);
extern const char *avr_out_compare64 (rtx_insn *, rtx*, int*);
extern const char *avr_cond_branch (rtx_insn *, rtx *);
extern const char *avr_out_movpsi (rtx_insn *, rtx*, int*);
extern const char *avr_out_sign_extend (rtx_insn *, rtx*, int*);
extern const char *avr_out_insert_notbit (rtx_insn *, rtx*, int*);
extern const char *avr_out_insv (rtx_insn *, rtx*, int*);
extern const char *avr_out_extr (rtx_insn *, rtx*, int*);
extern const char *avr_out_extr_not (rtx_insn *, rtx*, int*);
extern const char *avr_out_plus_set_ZN (rtx*, int*);
extern const char *avr_out_plus_set_N (rtx*, int*);
extern const char *avr_out_op8_set_ZN (rtx_code, rtx*, int*);
extern int avr_len_op8_set_ZN (rtx_code, rtx*);
extern bool avr_op8_ZN_operator (rtx);
extern const char *avr_out_cmp_ext (rtx*, rtx_code, int*);
extern bool avr_set_some_operation (rtx);

extern const char *ashlqi3_out (rtx_insn *insn, rtx operands[], int *len);
extern const char *ashlhi3_out (rtx_insn *insn, rtx operands[], int *len);
extern const char *ashlsi3_out (rtx_insn *insn, rtx operands[], int *len);

extern const char *ashrqi3_out (rtx_insn *insn, rtx operands[], int *len);
extern const char *ashrhi3_out (rtx_insn *insn, rtx operands[], int *len);
extern const char *ashrsi3_out (rtx_insn *insn, rtx operands[], int *len);

extern const char *lshrqi3_out (rtx_insn *insn, rtx operands[], int *len);
extern const char *lshrhi3_out (rtx_insn *insn, rtx operands[], int *len);
extern const char *lshrsi3_out (rtx_insn *insn, rtx operands[], int *len);

extern const char *avr_out_ashlpsi3 (rtx_insn *, rtx*, int*);
extern const char *avr_out_ashrpsi3 (rtx_insn *, rtx*, int*);
extern const char *avr_out_lshrpsi3 (rtx_insn *, rtx*, int*);

extern bool avr_rotate_bytes (rtx operands[]);

extern const char* avr_out_fract (rtx_insn *, rtx[], bool, int*);
extern rtx avr_to_int_mode (rtx);

extern void avr_expand_prologue (void);
extern void avr_expand_epilogue (bool);
extern bool avr_emit_cpymemhi (rtx*);
extern void avr_emit_xior_with_shift (rtx_insn*, rtx*, int);
extern bool avr_epilogue_uses (int regno);

extern void avr_output_addr_vec (rtx_insn*, rtx);
extern const char *avr_out_sbxx_branch (rtx_insn *insn, rtx operands[]);
extern const char* avr_out_bitop (rtx, rtx*, int*);
extern const char* avr_out_plus (rtx, rtx*, int* =NULL, bool =true);
extern const char* avr_out_plus_ext (rtx_insn*, rtx*, int*);
extern const char* avr_out_add_msb (rtx_insn*, rtx*, rtx_code, int*);
extern const char* avr_out_round (rtx_insn *, rtx*, int* =NULL);
extern const char* avr_out_addto_sp (rtx*, int*);
extern const char* avr_out_xload (rtx_insn *, rtx*, int*);
extern const char* avr_out_fload (rtx_insn *, rtx*, int*);
extern const char* avr_out_cpymem (rtx_insn *, rtx*, int*);
extern const char* avr_out_insert_bits (rtx*, int*);
extern bool avr_popcount_each_byte (rtx, int, int);
extern bool avr_xor_noclobber_dconst (rtx, int);
extern bool avr_has_nibble_0xf (rtx);

extern bool extra_constraint_Q (rtx x);
extern int avr_adjust_insn_length (rtx_insn *insn, int len);
extern void output_reload_in_const (rtx *, rtx clobber, int *len, bool clear_p);
extern const char* output_reload_inhi (rtx*, rtx, int*);
extern const char* output_reload_insisf (rtx*, rtx, int*);
extern const char* avr_out_reload_inpsi (rtx*, rtx, int*);
extern const char* avr_out_lpm (rtx_insn *, rtx*, int*);
extern const char* avr_out_cmp_lsr (rtx_insn *, rtx*, int*);
extern void avr_maybe_cmp_lsr (rtx *);
extern bool reg_unused_after (rtx_insn *insn, rtx reg);
extern int avr_jump_mode (rtx x, rtx_insn *insn, int = 0);
extern bool test_hard_reg_class (enum reg_class rclass, rtx x);
extern bool jump_over_one_insn_p (rtx_insn *insn, rtx dest);

extern void avr_final_prescan_insn (rtx_insn *insn, rtx *operand,
				    int num_operands);
extern rtx_code avr_normalize_condition (rtx_code condition);
extern void out_shift_with_cnt (const char *templ, rtx_insn *insn,
				rtx operands[], int *len, int t_len);
extern enum reg_class avr_mode_code_base_reg_class (machine_mode, addr_space_t, rtx_code, rtx_code);
extern bool avr_regno_mode_code_ok_for_base_p (int, machine_mode, addr_space_t, rtx_code, rtx_code);
extern rtx avr_incoming_return_addr_rtx (void);
extern rtx avr_legitimize_reload_address (rtx*, machine_mode, int, int, int, int, rtx (*)(rtx,int));
extern bool avr_adiw_reg_p (rtx);
extern bool avr_mem_flash_p (rtx);
extern bool avr_mem_flashx_p (rtx);
extern bool avr_mem_memx_p (rtx);
extern bool avr_load_libgcc_p (rtx);
extern bool avr_xload_libgcc_p (machine_mode);
extern bool avr_fload_libgcc_p (machine_mode);
extern bool avr_load_libgcc_mem_p (rtx, addr_space_t, bool use_libgcc);
extern bool avr_load_libgcc_insn_p (rtx_insn *, addr_space_t, bool use_libgcc);
extern rtx avr_eval_addr_attrib (rtx x);

extern bool avr_float_lib_compare_returns_bool (machine_mode, rtx_code);

static inline unsigned
regmask (machine_mode mode, unsigned regno)
{
  return ((1u << GET_MODE_SIZE (as_a <fixed_size_mode> (mode))) - 1) << regno;
}

extern void avr_fix_inputs (rtx*, unsigned, unsigned);
extern bool avr_emit3_fix_outputs (rtx (*)(rtx,rtx,rtx), rtx*, unsigned, unsigned);

extern rtx lpm_reg_rtx;
extern rtx lpm_addr_reg_rtx;
extern rtx tmp_reg_rtx;
extern rtx zero_reg_rtx;
extern rtx all_regs_rtx[32];
extern rtx rampz_rtx;
extern rtx cc_reg_rtx;
extern rtx ccn_reg_rtx;
extern rtx cczn_reg_rtx;

extern int n_avr_fuse_add_executed;
extern bool avr_shift_is_3op ();
extern bool avr_split_shift_p (int n_bytes, int offset, rtx_code);
extern bool avr_split_shift (rtx xop[], rtx xscratch, rtx_code);
extern bool avr_split_ldst (rtx xop[]);

extern int avr_optimize_size_level ();

#endif /* RTX_CODE */

#ifdef REAL_VALUE_TYPE
extern void asm_output_float (FILE *file, REAL_VALUE_TYPE n);
#endif

extern bool avr_have_dimode;

/* From avr-passes.cc */

namespace gcc { class context; }
class rtl_opt_pass;

extern rtl_opt_pass *make_avr_pass_fuse_add (gcc::context *);
extern rtl_opt_pass *make_avr_pass_fuse_move (gcc::context *);
extern rtl_opt_pass *make_avr_pass_pre_proep (gcc::context *);
extern rtl_opt_pass *make_avr_pass_recompute_notes (gcc::context *);
extern rtl_opt_pass *make_avr_pass_casesi (gcc::context *);
extern rtl_opt_pass *make_avr_pass_ifelse (gcc::context *);
extern rtl_opt_pass *make_avr_pass_split_after_peephole2 (gcc::context *);
#ifdef RTX_CODE
extern bool avr_casei_sequence_check_operands (rtx *xop);
extern bool avr_split_fake_addressing_move (rtx_insn *insn, rtx *operands);
#endif /* RTX_CODE */

/* From avr-log.cc */

#ifdef GCC_DUMPFILE_H
#define avr_dump(...)							\
  do {									\
    if (dump_file)							\
      avr_vdump (dump_file, __FUNCTION__, __VA_ARGS__);			\
  } while (0)
#else
#define avr_dump(...) avr_vdump (NULL, __FUNCTION__, __VA_ARGS__)
#endif /* GCC_DUMPFILE_H */
#define avr_edump(...) avr_vdump (stderr, __FUNCTION__, __VA_ARGS__)
#define avr_fdump(FIL, ...) avr_vdump (FIL, __FUNCTION__, __VA_ARGS__)

extern int avr_vdump (FILE*, const char*, ...);
extern void avr_log_set_avr_log (void);

typedef struct
{
  unsigned address_cost :1;
  unsigned builtin :1;
  unsigned constraints :1;
  unsigned insn_addresses :1;
  unsigned legitimate_address_p :1;
  unsigned legitimize_address :1;
  unsigned legitimize_reload_address :1;
  unsigned progmem :1;
  unsigned rtx_costs :1;
} avr_log_t;

extern avr_log_t avr_log;
