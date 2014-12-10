/* Definitions of target machine for GNU compiler for Renesas / SuperH SH.
   Copyright (C) 1993-2014 Free Software Foundation, Inc.
   Contributed by Steve Chamberlain (sac@cygnus.com).
   Improved by Jim Wilson (wilson@cygnus.com).

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

#ifndef GCC_SH_PROTOS_H
#define GCC_SH_PROTOS_H

enum sh_function_kind {
  /* A function with normal C ABI  */
  FUNCTION_ORDINARY,
  /* A special function that guarantees that some otherwise call-clobbered
     registers are not clobbered.  These can't go through the SH5 resolver,
     because it only saves argument passing registers.  */
  SFUNC_GOT,
  /* A special function that should be linked statically.  These are typically
     smaller or not much larger than a PLT entry.
     Some also have a non-standard ABI which precludes dynamic linking.  */
  SFUNC_STATIC
};

/* Atomic model.  */
struct sh_atomic_model
{
  enum enum_type
  {
    none = 0,
    soft_gusa,
    hard_llcs,
    soft_tcb,
    soft_imask,

    num_models
  };

  /*  If strict is set, disallow mixing of different models, as it would
      happen on SH4A.  */
  bool strict;
  enum_type type;

  /* Name string as it was specified on the command line.  */
  const char* name;

  /* Name string as it is used in C/C++ defines.  */
  const char* cdef_name;

  /* GBR offset variable for TCB model.  */
  int tcb_gbr_offset;
};

extern const sh_atomic_model& selected_atomic_model (void);

/* Shortcuts to check the currently selected atomic model.  */
#define TARGET_ATOMIC_ANY \
  selected_atomic_model ().type != sh_atomic_model::none

#define TARGET_ATOMIC_STRICT \
  selected_atomic_model ().strict

#define TARGET_ATOMIC_SOFT_GUSA \
  selected_atomic_model ().type == sh_atomic_model::soft_gusa

#define TARGET_ATOMIC_HARD_LLCS \
  selected_atomic_model ().type == sh_atomic_model::hard_llcs

#define TARGET_ATOMIC_SOFT_TCB \
  selected_atomic_model ().type == sh_atomic_model::soft_tcb

#define TARGET_ATOMIC_SOFT_TCB_GBR_OFFSET_RTX \
  GEN_INT (selected_atomic_model ().tcb_gbr_offset)

#define TARGET_ATOMIC_SOFT_IMASK \
  selected_atomic_model ().type == sh_atomic_model::soft_imask

#ifdef RTX_CODE
extern rtx sh_fsca_sf2int (void);
extern rtx sh_fsca_int2sf (void);

/* Declare functions defined in sh.c and used in templates.  */

extern const char *output_branch (int, rtx_insn *, rtx *);
extern const char *output_ieee_ccmpeq (rtx_insn *, rtx *);
extern const char *output_branchy_insn (enum rtx_code, const char *,
					rtx_insn *, rtx *);
extern const char *output_movedouble (rtx, rtx[], machine_mode);
extern const char *output_movepcrel (rtx, rtx[], machine_mode);
extern const char *output_far_jump (rtx_insn *, rtx);

extern rtx sfunc_uses_reg (rtx_insn *);
extern int barrier_align (rtx_insn *);
extern int sh_loop_align (rtx_insn *);
extern bool fp_zero_operand (rtx);
extern bool fp_one_operand (rtx);
extern bool sh_legitimate_index_p (machine_mode, rtx, bool, bool);
extern bool sh_legitimize_reload_address (rtx *, machine_mode, int, int);
extern rtx legitimize_pic_address (rtx, machine_mode, rtx);
extern bool nonpic_symbol_mentioned_p (rtx);
extern void output_pic_addr_const (FILE *, rtx);
extern bool expand_block_move (rtx *);
extern void prepare_move_operands (rtx[], machine_mode mode);
extern bool sh_expand_cmpstr (rtx *);
extern bool sh_expand_cmpnstr (rtx *);
extern bool sh_expand_strlen  (rtx *);
extern void sh_expand_setmem (rtx *);
extern enum rtx_code prepare_cbranch_operands (rtx *, machine_mode mode,
					       enum rtx_code comparison);
extern void expand_cbranchsi4 (rtx *operands, enum rtx_code comparison, int);
extern bool expand_cbranchdi4 (rtx *operands, enum rtx_code comparison);
extern void sh_emit_scc_to_t (enum rtx_code, rtx, rtx);
extern rtx sh_emit_cheap_store_flag (machine_mode, enum rtx_code, rtx, rtx);
extern void sh_emit_compare_and_branch (rtx *, machine_mode);
extern void sh_emit_compare_and_set (rtx *, machine_mode);
extern bool sh_ashlsi_clobbers_t_reg_p (rtx);
extern bool sh_lshrsi_clobbers_t_reg_p (rtx);
extern void gen_shifty_op (int, rtx *);
extern void gen_shifty_hi_op (int, rtx *);
extern bool expand_ashiftrt (rtx *);
extern bool sh_dynamicalize_shift_p (rtx);
extern int shl_and_kind (rtx, rtx, int *);
extern int shl_and_length (rtx);
extern int shl_and_scr_length (rtx);
extern bool gen_shl_and (rtx, rtx, rtx, rtx);
extern int shl_sext_kind (rtx, rtx, int *);
extern int shl_sext_length (rtx);
extern bool gen_shl_sext (rtx, rtx, rtx, rtx);
extern rtx gen_datalabel_ref (rtx);
extern int regs_used (rtx, int);
extern void fixup_addr_diff_vecs (rtx_insn *);
extern int get_dest_uid (rtx, int);
extern void final_prescan_insn (rtx_insn *, rtx *, int);
extern enum tls_model tls_symbolic_operand (rtx, machine_mode);
extern bool system_reg_operand (rtx, machine_mode);
extern bool reg_unused_after (rtx, rtx_insn *);
extern int sh_insn_length_adjustment (rtx_insn *);
extern bool sh_can_redirect_branch (rtx_insn *, rtx_insn *);
extern void sh_expand_unop_v2sf (enum rtx_code, rtx, rtx);
extern void sh_expand_binop_v2sf (enum rtx_code, rtx, rtx, rtx);
extern bool sh_expand_t_scc (rtx *);
extern rtx sh_gen_truncate (machine_mode, rtx, int);
extern bool sh_vector_mode_supported_p (machine_mode);
extern bool sh_cfun_trap_exit_p (void);
extern rtx sh_find_equiv_gbr_addr (rtx_insn* cur_insn, rtx mem);
extern int sh_eval_treg_value (rtx op);
extern HOST_WIDE_INT sh_disp_addr_displacement (rtx mem_op);
extern int sh_max_mov_insn_displacement (machine_mode mode, bool consider_sh2a);

/* Result value of sh_find_set_of_reg.  */
struct set_of_reg
{
  /* The insn where sh_find_set_of_reg stopped looking.
     Can be NULL_RTX if the end of the insn list was reached.  */
  rtx_insn* insn;

  /* The set rtx of the specified reg if found, NULL_RTX otherwise.  */
  const_rtx set_rtx;

  /* The set source rtx of the specified reg if found, NULL_RTX otherwise.
     Usually, this is the most interesting return value.  */
  rtx set_src;
};

/* Given a reg rtx and a start insn, try to find the insn that sets the
   specified reg by using the specified insn stepping function, such as 
   'prev_nonnote_insn_bb'.  When the insn is found, try to extract the rtx
   of the reg set.  */
template <typename F> inline set_of_reg
sh_find_set_of_reg (rtx reg, rtx_insn* insn, F stepfunc)
{
  set_of_reg result;
  result.insn = insn;
  result.set_rtx = NULL_RTX;
  result.set_src = NULL_RTX;

  if (!REG_P (reg) || insn == NULL_RTX)
    return result;

  for (result.insn = stepfunc (insn); result.insn != NULL_RTX;
       result.insn = stepfunc (result.insn))
    {
      if (BARRIER_P (result.insn))
	return result;
      if (!NONJUMP_INSN_P (result.insn))
	continue;
      if (reg_set_p (reg, result.insn))
	{
	  result.set_rtx = set_of (reg, result.insn);

	  if (result.set_rtx == NULL_RTX || GET_CODE (result.set_rtx) != SET)
	    return result;

	  result.set_src = XEXP (result.set_rtx, 1);
	  return result;
	}
    }

  return result;
}

extern bool sh_is_logical_t_store_expr (rtx op, rtx_insn* insn);
extern rtx sh_try_omit_signzero_extend (rtx extended_op, rtx_insn* insn);
extern bool sh_split_movrt_negc_to_movt_xor (rtx_insn* curr_insn,
					     rtx operands[]);
#endif /* RTX_CODE */

extern void sh_cpu_cpp_builtins (cpp_reader* pfile);

extern const char *output_jump_label_table (void);
extern rtx get_t_reg_rtx (void);
extern int sh_media_register_for_return (void);
extern void sh_expand_prologue (void);
extern void sh_expand_epilogue (bool);
extern void sh_set_return_address (rtx, rtx);
extern int initial_elimination_offset (int, int);
extern bool sh_hard_regno_rename_ok (unsigned int, unsigned int);
extern bool sh_cfun_interrupt_handler_p (void);
extern bool sh_cfun_resbank_handler_p (void);
extern bool sh_attr_renesas_p (const_tree);
extern bool sh_cfun_attr_renesas_p (void);
extern bool sh_cannot_change_mode_class
	      (machine_mode, machine_mode, enum reg_class);
extern bool sh_small_register_classes_for_mode_p (machine_mode);
extern void sh_mark_label (rtx, int);
extern bool check_use_sfunc_addr (rtx_insn *, rtx);

#ifdef HARD_CONST
extern void fpscr_set_from_mem (int, HARD_REG_SET);
#endif

extern void sh_pr_interrupt (struct cpp_reader *);
extern void sh_pr_trapa (struct cpp_reader *);
extern void sh_pr_nosave_low_regs (struct cpp_reader *);
extern rtx function_symbol (rtx, const char *, enum sh_function_kind);
extern rtx sh_get_pr_initial_val (void);

extern void sh_init_cumulative_args (CUMULATIVE_ARGS *, tree, rtx, tree,
				     signed int, machine_mode);
extern rtx sh_dwarf_register_span (rtx);

extern rtx replace_n_hard_rtx (rtx, rtx *, int , int);
extern int shmedia_cleanup_truncate (rtx);

extern bool sh_contains_memref_p (rtx);
extern bool sh_loads_bankedreg_p (rtx);
extern rtx shmedia_prepare_call_address (rtx fnaddr, int is_sibcall);
extern int sh2a_get_function_vector_number (rtx);
extern bool sh2a_is_function_vector_call (rtx);
extern void sh_fix_range (const char *);
extern bool sh_hard_regno_mode_ok (unsigned int, machine_mode);
extern bool sh_can_use_simple_return_p (void);
#endif /* ! GCC_SH_PROTOS_H */
