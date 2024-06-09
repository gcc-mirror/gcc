/* Exported function prototypes from the Renesas RX backend.
   Copyright (C) 2008-2024 Free Software Foundation, Inc.
   Contributed by Red Hat.

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

#ifndef GCC_RX_PROTOS_H
#define GCC_RX_PROTOS_H

extern bool             rx_can_use_simple_return (void);
extern void		rx_expand_epilogue (bool);
extern void		rx_expand_prologue (void);
extern int		rx_initial_elimination_offset (int, int);

bool is_interrupt_func (const_tree decl);
bool is_fast_interrupt_func (const_tree decl);

/* rx_atomic_sequence is used to emit the header and footer
   of an atomic sequence.  It's supposed to be used in a scope.
   When constructed, it will emit the atomic sequence header insns.
   When destructred (goes out of scope), it will emit the
   corresponding atomic sequence footer insns.  */
class rx_atomic_sequence
{
public:
  rx_atomic_sequence (const_tree fun_decl);
  ~rx_atomic_sequence (void);

private:
  rx_atomic_sequence (void);
  rx_atomic_sequence (const rx_atomic_sequence&);
  rx_atomic_sequence& operator = (const rx_atomic_sequence&);

  rtx m_prev_psw_reg;
};

#ifdef RTX_CODE
extern int		rx_adjust_insn_length (rtx_insn *, int);
extern align_flags	rx_align_for_label (rtx_insn *, int);
extern void             rx_emit_stack_popm (rtx *, bool);
extern void             rx_emit_stack_pushm (rtx *);
extern char *		rx_gen_move_template (rtx *, bool);
extern bool		rx_is_legitimate_constant (machine_mode, rtx);
extern bool		rx_is_restricted_memory_address (rtx,
							 machine_mode);
extern bool		rx_match_ccmode (rtx, machine_mode);
extern rtx		rx_maybe_pidify_operand (rtx, int);
extern void		rx_notice_update_cc (rtx, rtx);
extern void		rx_split_cbranch (machine_mode, enum rtx_code,
					  rtx, rtx, rtx);
extern machine_mode	rx_select_cc_mode (enum rtx_code, rtx, rtx);

extern bool rx_reg_dead_or_unused_after_insn (const rtx_insn* i, int regno);
extern void rx_copy_reg_dead_or_unused_notes (rtx reg, const rtx_insn* src,
					      rtx_insn* dst);

extern bool rx_fuse_in_memory_bitop (rtx* operands, rtx_insn* curr_insn,
				     rtx (*gen_insn)(rtx, rtx));

/* Result value of rx_find_set_of_reg.  */
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

/* FIXME: Copy-pasta from SH.  Move to rtl.h.
   Given a reg rtx and a start insn, try to find the insn that sets
   the specified reg by using the specified insn stepping function,
   such as 'prev_nonnote_nondebug_insn_bb'.  When the insn is found,
   try to extract the rtx of the reg set.  */
template <typename F> inline set_of_reg
rx_find_set_of_reg (rtx reg, rtx_insn* insn, F stepfunc,
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

/* FIXME: Move to rtlh.h.  */
template <typename F> inline rtx_insn*
rx_find_use_of_reg (rtx reg, rtx_insn* insn, F stepfunc)
{
  if (!REG_P (reg) || insn == NULL_RTX)
    return NULL;

  for (rtx_insn* i = stepfunc (insn); i != NULL_RTX; i = stepfunc (i))
    {
      if (BARRIER_P (i))
	break;
      if (!INSN_P (i) || DEBUG_INSN_P (i))
	continue;
      if (reg_overlap_mentioned_p (reg, PATTERN (i))
	  || (CALL_P (i) && find_reg_fusage (i, USE, reg)))
	return i;
    }

  return NULL;
}

#endif

#endif /* GCC_RX_PROTOS_H */
