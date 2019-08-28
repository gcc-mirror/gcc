#ifndef GCC_ARC64_PROTOS_H
#define GCC_ARC64_PROTOS_H

extern int arc64_epilogue_uses (int);
extern int arc64_eh_uses (int);
extern HOST_WIDE_INT arc64_initial_elimination_offset (unsigned, unsigned);
extern void arc64_init_expanders (void);
extern void arc64_cpu_cpp_builtins (cpp_reader *);

#ifdef RTX_CODE

extern rtx arc64_return_addr (int, rtx);
extern machine_mode arc64_select_cc_mode (enum rtx_code, rtx, rtx);
extern bool arc64_can_use_return_insn_p (void);
extern void arc64_expand_call (rtx, rtx, bool);
extern rtx arc64_gen_compare_reg (enum rtx_code, rtx, rtx);
extern bool arc64_prepare_move_operands (rtx, rtx, machine_mode);
extern void arc64_expand_prologue (void);
extern void arc64_expand_epilogue (bool);
extern bool arc64_limm_addr_p (rtx);
extern bool arc64_is_long_call_p (rtx);
extern bool arc64_legitimate_store_address_p (machine_mode, rtx);
extern bool arc64_short_access_p (rtx, machine_mode, bool);
extern rtx arc64_eh_return_handler_rtx (void);
extern int arc64_asm_preferred_eh_data_format (int, int);

extern bool arc64_check_mov_const (HOST_WIDE_INT);
extern bool arc64_split_mov_const (rtx *);
extern bool arc64_expand_cpymem (rtx *);

extern void arc64_expand_atomic_op (enum rtx_code, rtx, rtx, rtx, rtx, rtx);
extern void arc64_pre_atomic_barrier (enum memmodel);
extern void arc64_post_atomic_barrier (enum memmodel);
extern void arc64_expand_compare_and_swap (rtx []);
extern void arc64_split_compare_and_swap (rtx []);
extern bool arc64_allow_direct_access_p (rtx);
extern bool arc64_use_fp_regs (machine_mode);
extern bool arc64_fp_access_p (rtx, machine_mode);
extern void arc64_expand_casesi (rtx []);
extern bool arc64_split_double_move_p (rtx *, machine_mode);
extern void arc64_split_double_move (rtx *, machine_mode);
extern unsigned arc64_dbx_register_number (unsigned);
extern bool arc64_expand_fvect_shr (rtx *);
extern bool arc64_use_plt34_p (rtx);
extern int regno_clobbered_p (unsigned int, rtx_insn *, machine_mode, int);
extern void arc64_gen_unlikely_cbranch (enum rtx_code, machine_mode, rtx);
extern int accumulator_bypass_p (rtx_insn *, rtx_insn *);
extern int set_accumulator_p (rtx_insn *, rtx_insn *);
extern const char *arc64_output_return (void);
extern bool arc64_hard_regno_rename_ok (unsigned, unsigned);
extern void arc64_expand_vector_init (rtx, rtx);

#endif /* RTX_CODE */

#endif /* GCC_ARC64_PROTOS_H */
