/* relax-opt pass of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2023 Free Software Foundation, Inc.
   Contributed by Andes Technology Corporation.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* ------------------------------------------------------------------------ */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "stringpool.h"
#include "attribs.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "optabs.h"		/* For GEN_FCN.  */
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "stor-layout.h"
#include "varasm.h"
#include "calls.h"
#include "output.h"
#include "explow.h"
#include "expr.h"
#include "tm-constrs.h"
#include "builtins.h"
#include "cpplib.h"
#include "insn-attr.h"
#include "cfgrtl.h"
#include "tree-pass.h"

using namespace nds32;

/* This is used to create unique relax hint id value.
   The initial value is 0.  */
static int relax_group_id = 0;

/* Group the following pattern as relax candidates:

   1. sethi	$ra, hi20(sym)
      ori	$ra, $ra, lo12(sym)
    ==>
      addi.gp	$ra, sym

   2. sethi	$ra, hi20(sym)
      lwi	$rb, [$ra + lo12(sym)]
    ==>
      lwi.gp	$rb, [(sym)]

   3. sethi	$ra, hi20(sym)
      ori	$ra, $ra, lo12(sym)
      lwi	$rb, [$ra]
      swi	$rc, [$ra]
    ==>
      lwi37	$rb, [(sym)]
      swi37	$rc, [(sym)] */

int
nds32_alloc_relax_group_id ()
{
  return relax_group_id++;
}

/* Return true if is load/store with REG addressing mode
   and memory mode is SImode.  */
static bool
nds32_reg_base_load_store_p (rtx_insn *insn)
{
  rtx mem_src = NULL_RTX;

  switch (get_attr_type (insn))
    {
    case TYPE_LOAD:
      mem_src = SET_SRC (PATTERN (insn));
      break;
    case TYPE_STORE:
      mem_src = SET_DEST (PATTERN (insn));
      break;
    default:
      break;
    }

  /* Find load/store insn with addressing mode is REG.  */
  if (mem_src != NULL_RTX)
    {
      if ((GET_CODE (mem_src) == ZERO_EXTEND)
	  || (GET_CODE (mem_src) == SIGN_EXTEND))
	mem_src = XEXP (mem_src, 0);

      if (GET_CODE (XEXP (mem_src, 0)) == REG)
	return true;
    }

  return false;
}

/* Return true if insn is a sp/fp base or sp/fp plus load-store instruction.  */

static bool
nds32_sp_base_or_plus_load_store_p (rtx_insn *insn)
{
  rtx mem_src = NULL_RTX;

  switch (get_attr_type (insn))
    {
    case TYPE_LOAD:
      mem_src = SET_SRC (PATTERN (insn));
      break;
    case TYPE_STORE:
      mem_src = SET_DEST (PATTERN (insn));
      break;
    default:
      break;
    }
  /* Find load/store insn with addressing mode is REG.  */
  if (mem_src != NULL_RTX)
    {
      if ((GET_CODE (mem_src) == ZERO_EXTEND)
	  || (GET_CODE (mem_src) == SIGN_EXTEND))
	mem_src = XEXP (mem_src, 0);

      if ((GET_CODE (XEXP (mem_src, 0)) == PLUS))
	mem_src = XEXP (mem_src, 0);

      if (REG_P (XEXP (mem_src, 0))
	  && ((frame_pointer_needed
	       && REGNO (XEXP (mem_src, 0)) == FP_REGNUM)
	      || REGNO (XEXP (mem_src, 0)) == SP_REGNUM))
	return true;
    }

  return false;
}

/* Return true if is load with [REG + REG/CONST_INT]  addressing mode.  */
static bool
nds32_plus_reg_load_store_p (rtx_insn *insn)
{
  rtx mem_src = NULL_RTX;

  switch (get_attr_type (insn))
    {
    case TYPE_LOAD:
      mem_src = SET_SRC (PATTERN (insn));
      break;
    case TYPE_STORE:
      mem_src = SET_DEST (PATTERN (insn));
      break;
    default:
      break;
    }

  /* Find load/store insn with addressing mode is [REG + REG/CONST].  */
  if (mem_src != NULL_RTX)
    {
      if ((GET_CODE (mem_src) == ZERO_EXTEND)
	  || (GET_CODE (mem_src) == SIGN_EXTEND))
	mem_src = XEXP (mem_src, 0);

      if ((GET_CODE (XEXP (mem_src, 0)) == PLUS))
	mem_src = XEXP (mem_src, 0);
      else
	return false;

      if (GET_CODE (XEXP (mem_src, 0)) == REG)
	return true;

    }

  return false;
}

/* Return true if x is const and the referance is ict symbol.  */
static bool
nds32_ict_const_p (rtx x)
{
  if (GET_CODE (x) == CONST)
    {
      x = XEXP (x, 0);
      return nds32_indirect_call_referenced_p (x);
    }
  return FALSE;
}

/* Group the following pattern as relax candidates:

   GOT:
      sethi	$ra, hi20(sym)
      ori	$ra, $ra, lo12(sym)
      lw	$rb, [$ra + $gp]

   GOTOFF, TLSLE:
      sethi	$ra, hi20(sym)
      ori	$ra, $ra, lo12(sym)
      LS	$rb, [$ra + $gp]

   GOTOFF, TLSLE:
      sethi	$ra, hi20(sym)
      ori	$ra, $ra, lo12(sym)
      add	$rb, $ra, $gp($tp)

   Initial GOT table:
      sethi	$gp,hi20(sym)
      ori	$gp, $gp, lo12(sym)
      add5.pc	$gp  */

static auto_vec<rtx_insn *, 32> nds32_group_infos;
/* Group the PIC and TLS relax candidate instructions for linker.  */
static bool
nds32_pic_tls_group (rtx_insn *def_insn,
		     enum nds32_relax_insn_type relax_type,
		     int sym_type)
{
  df_ref def_record;
  df_link *link;
  rtx_insn *use_insn = NULL;
  rtx pat, new_pat;
  def_record = DF_INSN_DEFS (def_insn);
  for (link = DF_REF_CHAIN (def_record); link; link = link->next)
    {
      if (!DF_REF_INSN_INFO (link->ref))
	continue;

      use_insn = DF_REF_INSN (link->ref);

      /* Skip if define insn and use insn not in the same basic block.  */
      if (!dominated_by_p (CDI_DOMINATORS,
			   BLOCK_FOR_INSN (use_insn),
			   BLOCK_FOR_INSN (def_insn)))
	return FALSE;

      /* Skip if use_insn not active insn.  */
      if (!active_insn_p (use_insn))
	return FALSE;

      switch (relax_type)
	{
	case RELAX_ORI:

	  /* GOTOFF, TLSLE:
	     sethi	$ra, hi20(sym)
	     ori	$ra, $ra, lo12(sym)
	     add	$rb, $ra, $gp($tp)  */
	  if ((sym_type == UNSPEC_TLSLE
	       || sym_type == UNSPEC_GOTOFF)
	      && (recog_memoized (use_insn) == CODE_FOR_addsi3))
	    {
	      pat = XEXP (PATTERN (use_insn), 1);
	      new_pat =
		gen_rtx_UNSPEC (SImode,
				gen_rtvec (2, XEXP (pat, 0), XEXP (pat, 1)),
				UNSPEC_ADD32);
	      validate_replace_rtx (pat, new_pat, use_insn);
	      nds32_group_infos.safe_push (use_insn);
	    }
	  else if (nds32_plus_reg_load_store_p (use_insn)
		   && !nds32_sp_base_or_plus_load_store_p (use_insn))
	    nds32_group_infos.safe_push (use_insn);
	  else
	    return FALSE;
	  break;

	default:
	  return FALSE;
	}
    }
  return TRUE;
}

static int
nds32_pic_tls_symbol_type (rtx x)
{
  x = XEXP (SET_SRC (PATTERN (x)), 1);

  if (GET_CODE (x) == CONST)
    {
      x = XEXP (x, 0);

      if (GET_CODE (x) == PLUS)
	x = XEXP (x, 0);

      return XINT (x, 1);
    }

  return XINT (x, 1);
}

/* Group the relax candidates with group id.  */
static void
nds32_group_insns (rtx_insn *sethi)
{
  df_ref def_record, use_record;
  df_link *link;
  rtx_insn *use_insn = NULL;
  rtx group_id;
  bool valid;

  def_record = DF_INSN_DEFS (sethi);

  for (link = DF_REF_CHAIN (def_record); link; link = link->next)
    {
      if (!DF_REF_INSN_INFO (link->ref))
	continue;

      use_insn = DF_REF_INSN (link->ref);

      /* Skip if define insn and use insn not in the same basic block.  */
      if (!dominated_by_p (CDI_DOMINATORS,
			   BLOCK_FOR_INSN (use_insn),
			   BLOCK_FOR_INSN (sethi)))
	return;

      /* Skip if the low-part used register is from different high-part
	 instructions.  */
      use_record = DF_INSN_USES (use_insn);
      if (DF_REF_CHAIN (use_record) && DF_REF_CHAIN (use_record)->next)
	return;

      /* Skip if use_insn not active insn.  */
      if (!active_insn_p (use_insn))
	return;

     /* Initial use_insn_type.  */
      if (!(recog_memoized (use_insn) == CODE_FOR_lo_sum
	    || nds32_symbol_load_store_p (use_insn)
	    || (nds32_reg_base_load_store_p (use_insn)
		&&!nds32_sp_base_or_plus_load_store_p (use_insn))))
	return;
    }

  group_id = GEN_INT (nds32_alloc_relax_group_id ());
  /* Insert .relax_* directive for sethi.  */
  emit_insn_before (gen_relax_group (group_id), sethi);

  /* Scan the use insns and insert the directive.  */
  for (link = DF_REF_CHAIN (def_record); link; link = link->next)
    {
      if (!DF_REF_INSN_INFO (link->ref))
	continue;

      use_insn = DF_REF_INSN (link->ref);

      /* Insert .relax_* directive.  */
      if (active_insn_p (use_insn))
	emit_insn_before (gen_relax_group (group_id), use_insn);

      /* Find ori ra, ra, unspec(symbol) instruction.  */
      if (use_insn != NULL
	  && recog_memoized (use_insn) == CODE_FOR_lo_sum
	  && !nds32_const_unspec_p (XEXP (SET_SRC (PATTERN (use_insn)), 1)))
	{
	  int sym_type = nds32_pic_tls_symbol_type (use_insn);
	  valid = nds32_pic_tls_group (use_insn, RELAX_ORI, sym_type);

	  /* Insert .relax_* directive.  */
	  while (!nds32_group_infos.is_empty ())
	    {
	      use_insn = nds32_group_infos.pop ();
	      if (valid)
		emit_insn_before (gen_relax_group (group_id), use_insn);
	    }
	}
    }
}

/* Convert relax group id in rtl.  */

static void
nds32_group_tls_insn (rtx insn)
{
  rtx pat = PATTERN (insn);
  rtx unspec_relax_group = XEXP (XVECEXP (pat, 0, 1), 0);
  int group_id = nds32_alloc_relax_group_id ();

  while (GET_CODE (pat) != SET && GET_CODE (pat) == PARALLEL)
    {
      pat = XVECEXP (pat, 0, 0);
    }

  if (GET_CODE (unspec_relax_group) == UNSPEC
      && XINT (unspec_relax_group, 1) == UNSPEC_VOLATILE_RELAX_GROUP)
    {
      XVECEXP (unspec_relax_group, 0, 0) = GEN_INT (group_id);
    }
}

static bool
nds32_float_reg_load_store_p (rtx_insn *insn)
{
  rtx pat = PATTERN (insn);

  if (get_attr_type (insn) == TYPE_FLOAD
      && GET_CODE (pat) == SET
      && (GET_MODE (XEXP (pat, 0)) == SFmode
	  || GET_MODE (XEXP (pat, 0)) == DFmode)
      && MEM_P (XEXP (pat, 1)))
    {
      rtx addr = XEXP (XEXP (pat, 1), 0);

      /* [$ra] */
      if (REG_P (addr))
	return true;
      /* [$ra + offset] */
      if (GET_CODE (addr) == PLUS
	  && REG_P (XEXP (addr, 0))
	  && CONST_INT_P (XEXP (addr, 1)))
	return true;
    }
  return false;
}


/* Group float load-store instructions:
   la $ra, symbol
   flsi $rt, [$ra + offset] */

static void
nds32_group_float_insns (rtx_insn *insn)
{
  df_ref def_record, use_record;
  df_link *link;
  rtx_insn *use_insn = NULL;
  rtx group_id;

  def_record = DF_INSN_DEFS (insn);

  for (link = DF_REF_CHAIN (def_record); link; link = link->next)
    {
      if (!DF_REF_INSN_INFO (link->ref))
	continue;

      use_insn = DF_REF_INSN (link->ref);

      /* Skip if define insn and use insn not in the same basic block.  */
      if (!dominated_by_p (CDI_DOMINATORS,
			   BLOCK_FOR_INSN (use_insn),
			   BLOCK_FOR_INSN (insn)))
	return;

      /* Skip if the low-part used register is from different high-part
	 instructions.  */
      use_record = DF_INSN_USES (use_insn);
      if (DF_REF_CHAIN (use_record) && DF_REF_CHAIN (use_record)->next)
	return;

      /* Skip if use_insn not active insn.  */
      if (!active_insn_p (use_insn))
	return;

      if (!nds32_float_reg_load_store_p (use_insn)
	  || find_post_update_rtx (use_insn) != -1)
	return;
    }

  group_id = GEN_INT (nds32_alloc_relax_group_id ());
  /* Insert .relax_* directive for insn.  */
  emit_insn_before (gen_relax_group (group_id), insn);

  /* Scan the use insns and insert the directive.  */
  for (link = DF_REF_CHAIN (def_record); link; link = link->next)
    {
      if (!DF_REF_INSN_INFO (link->ref))
	continue;

      use_insn = DF_REF_INSN (link->ref);

      /* Insert .relax_* directive.  */
	emit_insn_before (gen_relax_group (group_id), use_insn);
    }
}

/* Group the relax candidate instructions for linker.  */
static void
nds32_relax_group (void)
{
  rtx_insn *insn;

  compute_bb_for_insn ();

  df_chain_add_problem (DF_DU_CHAIN | DF_UD_CHAIN);
  df_insn_rescan_all ();
  df_analyze ();
  df_set_flags (DF_DEFER_INSN_RESCAN);
  calculate_dominance_info (CDI_DOMINATORS);

  insn = get_insns ();
  gcc_assert (NOTE_P (insn));

  for (insn = next_active_insn (insn); insn; insn = next_active_insn (insn))
    {
      if (NONJUMP_INSN_P (insn))
	{
	  /* Find sethi ra, symbol  instruction.  */
	  if (recog_memoized (insn) == CODE_FOR_sethi
	      && nds32_symbolic_operand (XEXP (SET_SRC (PATTERN (insn)), 0),
					 SImode)
	      && !nds32_ict_const_p (XEXP (SET_SRC (PATTERN (insn)), 0)))
	    nds32_group_insns (insn);
	  else if (recog_memoized (insn) == CODE_FOR_tls_ie)
	    nds32_group_tls_insn (insn);
	  else if (TARGET_FPU_SINGLE
		   && recog_memoized (insn) == CODE_FOR_move_addr
		   && !nds32_ict_const_p (XEXP (SET_SRC (PATTERN (insn)), 0)))
	    {
	      nds32_group_float_insns (insn);
	    }
	}
      else if (CALL_P (insn) && recog_memoized (insn) == CODE_FOR_tls_desc)
	{
	  nds32_group_tls_insn (insn);
	}
    }

  /* We must call df_finish_pass manually because it should be invoked before
     BB information is destroyed. Hence we cannot set the TODO_df_finish flag
     to the pass manager.  */
  df_insn_rescan_all ();
  df_finish_pass (false);
  free_dominance_info (CDI_DOMINATORS);
}

static unsigned int
nds32_relax_opt (void)
{
  if (TARGET_RELAX_HINT)
    nds32_relax_group ();
  return 1;
}

const pass_data pass_data_nds32_relax_opt =
{
  RTL_PASS,				/* type */
  "relax_opt",				/* name */
  OPTGROUP_NONE,			/* optinfo_flags */
  TV_MACH_DEP,				/* tv_id */
  0,					/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_df_finish,			/* todo_flags_finish */
};

class pass_nds32_relax_opt : public rtl_opt_pass
{
public:
  pass_nds32_relax_opt (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_nds32_relax_opt, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) { return TARGET_RELAX_HINT; }
  unsigned int execute (function *) { return nds32_relax_opt (); }
};

rtl_opt_pass *
make_pass_nds32_relax_opt (gcc::context *ctxt)
{
  return new pass_nds32_relax_opt (ctxt);
}
