/* Definitions of target machine for GNU compiler for Renesas / SuperH SH.
   Copyright (C) 1993-2018 Free Software Foundation, Inc.
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

#ifdef RTX_CODE
extern rtx sh_fsca_sf2int (void);
extern rtx sh_fsca_int2sf (void);

/* Declare functions defined in sh.c and used in templates.  */
extern bool sh_lra_p (void);

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
extern void expand_cbranchsi4 (rtx *operands, enum rtx_code comparison);
extern bool expand_cbranchdi4 (rtx *operands, enum rtx_code comparison);
extern void sh_emit_scc_to_t (enum rtx_code, rtx, rtx);
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
extern int regs_used (rtx, int);
extern void fixup_addr_diff_vecs (rtx_insn *);
extern int get_dest_uid (rtx_insn *, int);
extern void final_prescan_insn (rtx_insn *, rtx *, int);
extern enum tls_model tls_symbolic_operand (rtx, machine_mode);
extern bool system_reg_operand (rtx, machine_mode);
extern bool reg_unused_after (rtx, rtx_insn *);
extern int sh_insn_length_adjustment (rtx_insn *);
extern bool sh_expand_t_scc (rtx *);
extern rtx sh_gen_truncate (machine_mode, rtx, int);
extern bool sh_vector_mode_supported_p (machine_mode);
extern bool sh_cfun_trap_exit_p (void);
extern rtx sh_find_equiv_gbr_addr (rtx_insn* cur_insn, rtx mem);
extern int sh_eval_treg_value (rtx op);
extern HOST_WIDE_INT sh_disp_addr_displacement (rtx mem_op);
extern int sh_max_mov_insn_displacement (machine_mode mode, bool consider_sh2a);
extern bool sh_movsf_ie_ra_split_p (rtx, rtx, rtx);
extern void sh_expand_sym_label2reg (rtx, rtx, rtx, bool);

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

/* Given a reg rtx and a start insn, try to find the insn that sets
   the specified reg by using the specified insn stepping function,
   such as 'prev_nonnote_nondebug_insn_bb'.  When the insn is found,
   try to extract the rtx of the reg set.  */
template <typename F> inline set_of_reg
sh_find_set_of_reg (rtx reg, rtx_insn* insn, F stepfunc,
		    bool ignore_reg_reg_copies = false)
{
  set_of_reg result;
  result.insn = insn;
  result.set_rtx = NULL_RTX;
  result.set_src = NULL_RTX;

  if (!REG_P (reg) || insn == NULL_RTX)
    return result;

  for (rtx_insn* i = stepfunc (insn); i != NULL_RTX; i = stepfunc (i))
    {
      if (BARRIER_P (i))
	break;
      if (!INSN_P (i) || DEBUG_INSN_P (i))
	  continue;
      if (reg_set_p (reg, i))
	{
	  if (CALL_P (i))
	    break;

	  result.insn = i;
	  result.set_rtx = set_of (reg, i);

	  if (result.set_rtx == NULL_RTX || GET_CODE (result.set_rtx) != SET)
	    break;

	  result.set_src = XEXP (result.set_rtx, 1);

	  if (ignore_reg_reg_copies && REG_P (result.set_src))
	    {
	      reg = result.set_src;
	      continue;
	    }
	  if (ignore_reg_reg_copies && SUBREG_P (result.set_src)
	      && REG_P (SUBREG_REG (result.set_src)))
	    {
	      reg = SUBREG_REG (result.set_src);
	      continue;
	    }

	  break;
	}
    }

  /* If the searched reg is found inside a (mem (post_inc:SI (reg))), set_of
     will return NULL and set_rtx will be NULL.
     In this case report a 'not found'.  result.insn will always be non-null
     at this point, so no need to check it.  */
  if (result.set_src != NULL && result.set_rtx == NULL)
    result.set_src = NULL;

  return result;
}

/* Result value of sh_find_extending_set_of_reg.  */
struct sh_extending_set_of_reg : public set_of_reg
{
  /* The mode the set is extending from (QImode or HImode), or VOIDmode if
     this is not a zero/sign extending set.  */
  machine_mode from_mode;

  /* ZERO_EXTEND, SIGN_EXTEND or UNKNOWN.  */
  rtx_code ext_code;

  sh_extending_set_of_reg (rtx_insn* i)
  {
    insn = i;
    set_rtx = NULL;
    set_src = NULL;
    from_mode = VOIDmode;
    ext_code = UNKNOWN;
  }

  sh_extending_set_of_reg (const set_of_reg& rhs)
  {
    *((set_of_reg*)this) = rhs;
    from_mode = VOIDmode;
    ext_code = UNKNOWN;
  }

  /* Returns true if it's possible to use the source reg of the sign
     or zero extending set directly, bypassing the extension.  */
  bool can_use_as_unextended_reg (void) const;

  /* Returns the reg rtx of the sign or zero extending set source, that can
     be safely used at the specified insn in SImode.  */
  rtx use_as_unextended_reg (rtx_insn* use_at_insn) const;

  /* Returns the reg rtx of the sign or zero extending result, that can be
     safely used at the specified insn in SImode.  If the set source is an
     implicitly sign extending mem load, the mem load is converted into an
     explicitly sign extending mem load.  */
  rtx use_as_extended_reg (rtx_insn* use_at_insn) const;
};

extern sh_extending_set_of_reg sh_find_extending_set_of_reg (rtx reg,
							     rtx_insn* insn);

extern bool sh_is_logical_t_store_expr (rtx op, rtx_insn* insn);
extern rtx sh_try_omit_signzero_extend (rtx extended_op, rtx_insn* insn);
extern bool sh_split_movrt_negc_to_movt_xor (rtx_insn* curr_insn,
					     rtx operands[]);
extern void sh_split_tst_subregs (rtx_insn* curr_insn,
				  machine_mode subreg_mode, int subreg_offset,
				  rtx operands[]);

extern bool sh_is_nott_insn (const rtx_insn* i);
extern rtx sh_movt_set_dest (const rtx_insn* i);
extern rtx sh_movt_set_dest (const_rtx i);
extern rtx sh_movrt_set_dest (const rtx_insn* i);
extern rtx sh_movrt_set_dest (const_rtx i);

inline bool sh_is_movt_insn (const rtx_insn* i)
{
  return sh_movt_set_dest (i) != NULL;
}

inline bool sh_is_movrt_insn (const rtx_insn* i)
{
  return sh_movrt_set_dest (i) != NULL;
}

extern bool sh_insn_operands_modified_between_p (rtx_insn* operands_insn,
						 const rtx_insn* from,
						 const rtx_insn* to);

extern bool sh_reg_dead_or_unused_after_insn (const rtx_insn* i, int regno);
extern void sh_remove_reg_dead_or_unused_notes (rtx_insn* i, int regno);
extern rtx_insn* sh_check_add_incdec_notes (rtx_insn* i);
extern rtx sh_remove_overlapping_post_inc (rtx dst, rtx src);
extern rtx_insn* sh_peephole_emit_move_insn (rtx dst, rtx src);

extern bool sh_in_recog_treg_set_expr (void);
extern bool sh_recog_treg_set_expr (rtx op, machine_mode mode);

/* Result value of sh_split_treg_set_expr.  Contains the first insn emitted
   and the optional trailing nott insn.  */
class sh_treg_insns
{
public:
  sh_treg_insns (void) : m_first_insn (NULL), m_trailing_nott_insn (NULL) { }
  sh_treg_insns (rtx_insn* first_insn, rtx_insn* nott_insn)
  : m_first_insn (first_insn),
    m_trailing_nott_insn (nott_insn)
  { }

  bool was_treg_operand (void) const { return m_first_insn == NULL; }
  bool has_trailing_nott (void) const { return m_trailing_nott_insn != NULL; }
  rtx_insn* trailing_nott (void) const { return m_trailing_nott_insn; }
  rtx_insn* first_insn (void) const { return m_first_insn; }

  /* If there is a trailing nott, remove it from the emitted insns and
     return true.  Return false otherwise.  */
  bool
  remove_trailing_nott (void)
  {
    if (!has_trailing_nott ())
      return false;

    remove_insn (trailing_nott ());
    return true;
  }

private:
  rtx_insn* m_first_insn;
  rtx_insn* m_trailing_nott_insn;
};

extern sh_treg_insns sh_split_treg_set_expr (rtx x, rtx_insn* curr_insn);

enum
{
  /* An effective conditional branch distance of zero bytes is impossible.
     Hence we can use it to designate an unknown value.  */
  unknown_cbranch_distance = 0u,
  infinite_cbranch_distance = ~0u
};

unsigned int
sh_cbranch_distance (rtx_insn* cbranch_insn,
		     unsigned int max_dist = infinite_cbranch_distance);

#endif /* RTX_CODE */

extern void sh_cpu_cpp_builtins (cpp_reader* pfile);

extern const char *output_jump_label_table (void);
extern rtx get_t_reg_rtx (void);
extern void sh_expand_prologue (void);
extern void sh_expand_epilogue (bool);
extern void sh_set_return_address (rtx, rtx);
extern int initial_elimination_offset (int, int);
extern bool sh_hard_regno_rename_ok (unsigned int, unsigned int);
extern bool sh_cfun_interrupt_handler_p (void);
extern bool sh_cfun_resbank_handler_p (void);
extern bool sh_attr_renesas_p (const_tree);
extern bool sh_cfun_attr_renesas_p (void);
extern bool sh_small_register_classes_for_mode_p (machine_mode);
extern void sh_mark_label (rtx, int);
extern bool check_use_sfunc_addr (rtx_insn *, rtx);

#ifdef HARD_CONST
extern void fpscr_set_from_mem (int, HARD_REG_SET);
#endif

extern void sh_pr_interrupt (struct cpp_reader *);
extern void sh_pr_trapa (struct cpp_reader *);
extern void sh_pr_nosave_low_regs (struct cpp_reader *);

struct function_symbol_result
{
  function_symbol_result (void) : sym (NULL), lab (NULL) { }
  function_symbol_result (rtx s, rtx l) : sym (s), lab (l) { }

  rtx sym;
  rtx lab;
};

extern function_symbol_result function_symbol (rtx, const char *,
					       sh_function_kind);
extern rtx sh_get_fdpic_reg_initial_val (void);
extern rtx sh_get_pr_initial_val (void);

extern void sh_init_cumulative_args (CUMULATIVE_ARGS *, tree, rtx, tree,
				     signed int, machine_mode);
extern rtx sh_dwarf_register_span (rtx);

extern bool sh_contains_memref_p (rtx);
extern bool sh_loads_bankedreg_p (rtx);
extern int sh2a_get_function_vector_number (rtx);
extern bool sh2a_is_function_vector_call (rtx);
extern void sh_fix_range (const char *);
extern machine_mode sh_hard_regno_caller_save_mode (unsigned int, unsigned int,
						    machine_mode);
extern bool sh_can_use_simple_return_p (void);
extern rtx sh_load_function_descriptor (rtx);
#endif /* ! GCC_SH_PROTOS_H */
