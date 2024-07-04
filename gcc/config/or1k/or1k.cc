/* Target Code for OpenRISC
   Copyright (C) 2018-2024 Free Software Foundation, Inc.
   Contributed by Stafford Horne based on other ports.

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
#include "regs.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "diagnostic-core.h"
#include "output.h"
#include "stor-layout.h"
#include "varasm.h"
#include "calls.h"
#include "expr.h"
#include "builtins.h"
#include "optabs.h"
#include "explow.h"
#include "cfgrtl.h"
#include "alias.h"
#include "targhooks.h"
#include "case-cfn-macros.h"

/* These 4 are needed to allow using satisfies_constraint_J.  */
#include "insn-config.h"
#include "recog.h"
#include "tm_p.h"
#include "tm-constrs.h"

/* This file should be included last.  */
#include "target-def.h"

/* Per-function machine data.  */
struct GTY(()) machine_function
{
  /* Number of bytes saved on the stack for callee saved registers.  */
  HOST_WIDE_INT callee_saved_reg_size;

  /* Number of bytes saved on the stack for local variables.  */
  HOST_WIDE_INT local_vars_size;

  /* Number of bytes saved on the stack for outgoing/sub-function args.  */
  HOST_WIDE_INT args_size;

  /* The sum of sizes: locals vars, called saved regs, stack pointer
     and an optional frame pointer.
     Used in expand_prologue () and expand_epilogue ().  */
  HOST_WIDE_INT total_size;

  /* Remember where the set_got_placeholder is located.  */
  rtx_insn *set_got_insn;

  /* Remember where mcount args are stored so we can insert set_got_insn
     after.  */
  rtx_insn *set_mcount_arg_insn;
};

/* Zero initialization is OK for all current fields.  */

static struct machine_function *
or1k_init_machine_status (void)
{
  return ggc_cleared_alloc<machine_function> ();
}


/* Worker for TARGET_OPTION_OVERRIDE.
   We currently only use this to setup init_machine_status.  */

static void
or1k_option_override (void)
{
  /* Set the per-function-data initializer.  */
  init_machine_status = or1k_init_machine_status;
}

/* Returns true if REGNO must be saved for the current function.  */

static bool
callee_saved_regno_p (int regno)
{
  /* Check call-saved registers.  */
  if (!call_used_or_fixed_reg_p (regno) && df_regs_ever_live_p (regno))
    return true;

  switch (regno)
    {
    case HARD_FRAME_POINTER_REGNUM:
      return frame_pointer_needed;

    case LR_REGNUM:
      /* Always save LR if we are saving HFP, producing a walkable
	 stack chain with -fno-omit-frame-pointer.  */
      return (frame_pointer_needed
	      || !crtl->is_leaf
	      || crtl->uses_pic_offset_table
	      || df_regs_ever_live_p (regno));

    case HW_TO_GCC_REGNO (25):
    case HW_TO_GCC_REGNO (27):
    case HW_TO_GCC_REGNO (29):
    case HW_TO_GCC_REGNO (31):
      /* See EH_RETURN_DATA_REGNO.  */
      return crtl->calls_eh_return;

    default:
      return false;
    }
}

/* Worker for TARGET_COMPUTE_FRAME_LAYOUT.
   Compute and populate machine specific function attributes which are globally
   accessible via cfun->machine.  These include the sizes needed for
   stack stored local variables, callee saved registers and space for stack
   arguments which may be passed to a next function.  The values are used for
   the epilogue, prologue and eliminations.

   OpenRISC stack grows downwards and contains:

    ---- previous frame --------
    current func arg[n]
    current func arg[0]   <-- r2 [HFP,AP]
    ---- current stack frame ---  ^  ---\
    return address      r9        |     |
    old frame pointer   r2       (+)    |-- machine->total_size
    callee saved regs             |     | > machine->callee_saved_reg_size
    local variables               |     | > machine->local_vars_size       <-FP
    next function args    <-- r1 [SP]---/ > machine->args_size
    ----------------------------  |
				 (-)
	   (future)               |
				  V

   All of these contents are optional.  */

static void
or1k_compute_frame_layout (void)
{
  HOST_WIDE_INT local_vars_size, args_size, save_reg_size;

  local_vars_size = get_frame_size ();
  local_vars_size = ROUND_UP (local_vars_size, UNITS_PER_WORD);

  args_size = crtl->outgoing_args_size;
  args_size = ROUND_UP (args_size, UNITS_PER_WORD);

  save_reg_size = 0;
  for (int regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (callee_saved_regno_p (regno))
      save_reg_size += UNITS_PER_WORD;

  cfun->machine->local_vars_size = local_vars_size;
  cfun->machine->args_size = args_size;
  cfun->machine->callee_saved_reg_size = save_reg_size;
  cfun->machine->total_size = save_reg_size + local_vars_size + args_size;
}

/* Emit rtl to save register REGNO contents to stack memory at the given OFFSET
   from the current stack pointer.  */

static void
or1k_save_reg (int regno, HOST_WIDE_INT offset)
{
  rtx reg = gen_rtx_REG (Pmode, regno);
  rtx mem = gen_frame_mem (SImode, plus_constant (Pmode, stack_pointer_rtx,
						  offset));
  rtx insn = emit_move_insn (mem, reg);
  RTX_FRAME_RELATED_P (insn) = 1;
}

/* Emit rtl to restore register REGNO contents from stack memory at the given
   OFFSET from the current stack pointer.  */

static rtx
or1k_restore_reg (int regno, HOST_WIDE_INT offset, rtx cfa_restores)
{
  rtx reg = gen_rtx_REG (Pmode, regno);
  rtx mem = gen_frame_mem (SImode, plus_constant (Pmode, stack_pointer_rtx,
						  offset));
  emit_move_insn (reg, mem);
  return alloc_reg_note (REG_CFA_RESTORE, reg, cfa_restores);
}

/* Expand the "prologue" pattern.  */

void
or1k_expand_prologue (void)
{
  HOST_WIDE_INT sp_offset = -cfun->machine->total_size;
  HOST_WIDE_INT reg_offset, this_offset;
  rtx insn;

  if (flag_stack_usage_info)
    current_function_static_stack_size = -sp_offset;

  /* Early exit for frameless functions.  */
  if (sp_offset == 0)
    goto fini;

  /* Adjust the stack pointer.  For large stack offsets we will
     do this in multiple parts, before and after saving registers.  */
  reg_offset = (sp_offset + cfun->machine->local_vars_size
		+ cfun->machine->args_size);
  this_offset = MAX (sp_offset, -32764);
  reg_offset -= this_offset;
  sp_offset -= this_offset;

  insn = emit_insn (gen_frame_addsi3 (stack_pointer_rtx, stack_pointer_rtx,
				      GEN_INT (this_offset)));
  RTX_FRAME_RELATED_P (insn) = 1;

  /* Save callee-saved registers.  */
  for (int regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (regno != HARD_FRAME_POINTER_REGNUM
	&& regno != LR_REGNUM
	&& callee_saved_regno_p (regno))
      {
	or1k_save_reg (regno, reg_offset);
	reg_offset += UNITS_PER_WORD;
      }

  /* Save and update frame pointer.  */
  if (callee_saved_regno_p (HARD_FRAME_POINTER_REGNUM))
    {
      or1k_save_reg (HARD_FRAME_POINTER_REGNUM, reg_offset);
      if (frame_pointer_needed)
	{
	  insn = emit_insn (gen_addsi3 (hard_frame_pointer_rtx,
					stack_pointer_rtx,
					GEN_INT (-this_offset)));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
      reg_offset += UNITS_PER_WORD;
    }

  /* Save the link register.  */
  if (callee_saved_regno_p (LR_REGNUM))
    {
      or1k_save_reg (LR_REGNUM, reg_offset);
      reg_offset += UNITS_PER_WORD;
    }
  gcc_assert (reg_offset + this_offset == 0);

  /* Allocate the rest of the stack frame, if any.  */
  if (sp_offset != 0)
    {
      if (sp_offset < 2 * -32768)
	{
	  /* For very large offsets, we need a temporary register.  */
	  rtx tmp = gen_rtx_REG (Pmode, PE_TMP_REGNUM);
	  emit_move_insn (tmp, GEN_INT (sp_offset));
	  insn = emit_insn (gen_frame_addsi3 (stack_pointer_rtx,
					      stack_pointer_rtx, tmp));
	  if (!frame_pointer_needed)
	    {
	      RTX_FRAME_RELATED_P (insn) = 1;
	      add_reg_note (insn, REG_CFA_ADJUST_CFA,
			    gen_rtx_SET (stack_pointer_rtx,
					 plus_constant (Pmode,
							stack_pointer_rtx,
							sp_offset)));
	    }
	}
      else
	{
	  /* Otherwise, emit one or two sequential subtracts.  */
	  do
	    {
	      this_offset = MAX (sp_offset, -32768);
	      sp_offset -= this_offset;

	      insn = emit_insn (gen_frame_addsi3 (stack_pointer_rtx,
						  stack_pointer_rtx,
						  GEN_INT (this_offset)));
	      if (!frame_pointer_needed)
		RTX_FRAME_RELATED_P (insn) = 1;
	    }
	  while (sp_offset != 0);
	}
    }

 fini:
  /* Fix up, or remove, the insn that initialized the pic register.  */
  rtx_insn *set_got_insn = cfun->machine->set_got_insn;
  if (crtl->uses_pic_offset_table)
    {
      rtx reg = SET_DEST (PATTERN (set_got_insn));
      rtx_insn *insn = emit_insn_before (gen_set_got (reg), set_got_insn);
      RTX_FRAME_RELATED_P (insn) = 1;
      add_reg_note (insn, REG_CFA_FLUSH_QUEUE, NULL_RTX);
    }
  delete_insn (set_got_insn);
}

/* Expand the "epilogue" pattern.  */

void
or1k_expand_epilogue (void)
{
  HOST_WIDE_INT reg_offset, sp_offset;
  rtx insn, cfa_restores = NULL;

  sp_offset = cfun->machine->total_size;
  if (sp_offset == 0)
    return;

  reg_offset = cfun->machine->local_vars_size + cfun->machine->args_size;

  if (sp_offset >= 32768 || cfun->calls_alloca)
    {
      /* The saved registers are out of range of the stack pointer.
	 We need to partially deallocate the stack frame now.  */
      if (frame_pointer_needed)
	{
	  /* Reset the stack pointer to the bottom of the saved regs.  */
	  sp_offset -= reg_offset;
	  reg_offset = 0;
	  insn = emit_insn (gen_frame_addsi3 (stack_pointer_rtx,
					      hard_frame_pointer_rtx,
					      GEN_INT (-sp_offset)));
	  RTX_FRAME_RELATED_P (insn) = 1;
	  add_reg_note (insn, REG_CFA_DEF_CFA,
			plus_constant (Pmode, stack_pointer_rtx, sp_offset));
	}
      else if (sp_offset >= 3 * 32768)
	{
	  /* For very large offsets, we need a temporary register.  */
	  rtx tmp = gen_rtx_REG (Pmode, PE_TMP_REGNUM);
	  emit_move_insn (tmp, GEN_INT (reg_offset));
	  insn = emit_insn (gen_frame_addsi3 (stack_pointer_rtx,
					      stack_pointer_rtx, tmp));
	  sp_offset -= reg_offset;
	  reg_offset = 0;
	  RTX_FRAME_RELATED_P (insn) = 1;
	  add_reg_note (insn, REG_CFA_DEF_CFA,
			plus_constant (Pmode, stack_pointer_rtx, sp_offset));
	}
      else
	{
	  /* Otherwise, emit one or two sequential additions.  */
	  do
	    {
	      HOST_WIDE_INT this_offset = MIN (reg_offset, 32764);
	      reg_offset -= this_offset;
	      sp_offset -= this_offset;

	      insn = emit_insn (gen_frame_addsi3 (stack_pointer_rtx,
						  stack_pointer_rtx,
						  GEN_INT (this_offset)));
	      RTX_FRAME_RELATED_P (insn) = 1;
	      add_reg_note (insn, REG_CFA_DEF_CFA,
			    plus_constant (Pmode, stack_pointer_rtx,
					   sp_offset));
	    }
	  while (sp_offset >= 32768);
	}
    }

  /* Restore callee-saved registers.  */
  for (int regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (regno != HARD_FRAME_POINTER_REGNUM
	&& regno != LR_REGNUM
	&& callee_saved_regno_p (regno))
      {
	cfa_restores = or1k_restore_reg (regno, reg_offset, cfa_restores);
	reg_offset += UNITS_PER_WORD;
      }

  /* Restore frame pointer.  */
  if (callee_saved_regno_p (HARD_FRAME_POINTER_REGNUM))
    {
      cfa_restores = or1k_restore_reg (HARD_FRAME_POINTER_REGNUM,
				       reg_offset, cfa_restores);
      reg_offset += UNITS_PER_WORD;
    }

  /* Restore link register.  */
  if (callee_saved_regno_p (LR_REGNUM))
    {
      cfa_restores = or1k_restore_reg (LR_REGNUM, reg_offset, cfa_restores);
      reg_offset += UNITS_PER_WORD;
    }
  gcc_assert (reg_offset == sp_offset);

  /* Restore stack pointer.  */
  insn = emit_insn (gen_frame_addsi3 (stack_pointer_rtx, stack_pointer_rtx,
				      GEN_INT (sp_offset)));
  RTX_FRAME_RELATED_P (insn) = 1;
  REG_NOTES (insn) = cfa_restores;
  add_reg_note (insn, REG_CFA_DEF_CFA, stack_pointer_rtx);

  /* Move up to the stack frame of an exception handler.  */
  if (crtl->calls_eh_return)
    emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx,
			   EH_RETURN_STACKADJ_RTX));
}

/* Worker for PROFILE_HOOK.
   The OpenRISC profile hook uses the link register which will get clobbered by
   the GOT setup RTX.  This sets up a placeholder to allow injecting of the GOT
   setup RTX to avoid clobbering.  */

void
or1k_profile_hook (void)
{
  rtx a1 = gen_rtx_REG (Pmode, 3);
  rtx ra = get_hard_reg_initial_val (Pmode, LR_REGNUM);
  rtx fun = gen_rtx_SYMBOL_REF (Pmode, "_mcount");

  /* Keep track of where we setup the _mcount argument so we can insert the
     GOT setup RTX after it.  */
  cfun->machine->set_mcount_arg_insn = emit_move_insn (a1, ra);

  emit_library_call (fun, LCT_NORMAL, VOIDmode, a1, Pmode);
}

/* Worker for TARGET_INIT_PIC_REG.
   Initialize the cfun->machine->set_got_insn rtx and insert it at the entry
   of the current function.  The rtx is just a temporary placeholder for
   the GOT and will be replaced or removed during or1k_expand_prologue.  */

static void
or1k_init_pic_reg (void)
{

  if (crtl->profile)
    cfun->machine->set_got_insn =
      emit_insn_after (gen_set_got_tmp (pic_offset_table_rtx),
		       cfun->machine->set_mcount_arg_insn);
  else
    {
      start_sequence ();

      cfun->machine->set_got_insn =
	emit_insn (gen_set_got_tmp (pic_offset_table_rtx));

      rtx_insn *seq = get_insns ();
      end_sequence ();

      edge entry_edge = single_succ_edge (ENTRY_BLOCK_PTR_FOR_FN (cfun));
      insert_insn_on_edge (seq, entry_edge);
      commit_one_edge_insertion (entry_edge);
    }
}

#undef TARGET_INIT_PIC_REG
#define TARGET_INIT_PIC_REG  or1k_init_pic_reg
#undef TARGET_USE_PSEUDO_PIC_REG
#define TARGET_USE_PSEUDO_PIC_REG  hook_bool_void_true

/* Worker for INITIAL_FRAME_ADDRESS_RTX.
   Returns the RTX representing the address of the initial stack frame.  */

rtx
or1k_initial_frame_addr ()
{
  /* Use this to force a stack frame for the current function.  */
  crtl->accesses_prior_frames = 1;
  return arg_pointer_rtx;
}

/* Worker for DYNAMIC_CHAIN_ADDRESS.
   Returns the RTX representing the address of where the caller's frame pointer
   may be stored on the stack.  */

rtx
or1k_dynamic_chain_addr (rtx frame)
{
  return plus_constant (Pmode, frame, -2 * UNITS_PER_WORD);
}

/* Worker for RETURN_ADDR_RTX.
   Returns the RTX representing the address of where the link register may be
   stored on the stack.  */

rtx
or1k_return_addr (int, rtx frame)
{
  return gen_frame_mem (Pmode, plus_constant (Pmode, frame, -UNITS_PER_WORD));
}

/* Worker for TARGET_FRAME_POINTER_REQUIRED.
   Returns true if the current function must use a frame pointer.  */

static bool
or1k_frame_pointer_required ()
{
  /* ??? While IRA checks accesses_prior_frames, reload does not.
     We do want the frame pointer for this case.  */
  return (crtl->accesses_prior_frames);
}

/* Expand the "eh_return" pattern.
   Used for defining __builtin_eh_return, this will emit RTX to override the
   current function's return address stored on the stack.  The emitted RTX is
   inserted before the epilogue so we can't just update the link register.
   This is used when handling exceptions to jump into the exception handler
   catch block upon return from _Unwind_RaiseException.  */

void
or1k_expand_eh_return (rtx eh_addr)
{
  rtx lraddr;

  lraddr = gen_frame_mem (Pmode, plus_constant (Pmode,
						arg_pointer_rtx,
						-UNITS_PER_WORD));
  /* Set address to volatile to ensure the store doesn't get optimized out.  */
  MEM_VOLATILE_P (lraddr) = true;
  emit_move_insn (lraddr, eh_addr);
}

/* Helper for defining INITIAL_ELIMINATION_OFFSET.
   We allow the following eliminiations:
     FP -> HARD_FP or SP
     AP -> HARD_FP or SP

   HARD_FP and AP are the same which is handled below.  */

HOST_WIDE_INT
or1k_initial_elimination_offset (int from, int to)
{
  HOST_WIDE_INT offset;

  /* Set OFFSET to the offset from the stack pointer.  */
  switch (from)
    {
    /* Incoming args are all the way up at the previous frame.  */
    case ARG_POINTER_REGNUM:
      offset = cfun->machine->total_size;
      break;

    /* Local args grow downward from the saved registers.  */
    case FRAME_POINTER_REGNUM:
      offset = cfun->machine->args_size + cfun->machine->local_vars_size;
      break;

    default:
      gcc_unreachable ();
    }

  if (to == HARD_FRAME_POINTER_REGNUM)
    offset -= cfun->machine->total_size;

  return offset;
}

/* Worker for TARGET_LEGITIMATE_ADDRESS_P.
   Returns true if X is a legitimate address RTX on OpenRISC.  */

static bool
or1k_legitimate_address_p (machine_mode, rtx x, bool strict_p,
			   code_helper = ERROR_MARK)
{
  rtx base, addend;

  switch (GET_CODE (x))
    {
    case REG:
      base = x;
      break;

    case PLUS:
      base = XEXP (x, 0);
      addend = XEXP (x, 1);
      if (!REG_P (base))
	return false;
      /* Register elimination is going to adjust all of these offsets.
	 We might as well keep them as a unit until then.  */
      if (!strict_p && virtual_frame_reg_operand (base, VOIDmode))
	return CONST_INT_P (addend);
      if (!satisfies_constraint_I (addend))
	return false;
      break;

    case LO_SUM:
      base = XEXP (x, 0);
      if (!REG_P (base))
	return false;
      x = XEXP (x, 1);
      switch (GET_CODE (x))
	{
	case CONST:
	case SYMBOL_REF:
	case LABEL_REF:
	  /* Assume legitimize_address properly categorized
	     the symbol.  Continue to check the base.  */
	  break;

	case UNSPEC:
	  switch (XINT (x, 1))
	    {
	    case UNSPEC_GOT:
	    case UNSPEC_GOTOFF:
	    case UNSPEC_TPOFF:
	    case UNSPEC_GOTTPOFF:
	      /* Assume legitimize_address properly categorized
		 the symbol.  Continue to check the base.  */
	      break;
	    default:
	      return false;
	    }
	  break;

	default:
	  return false;
	}
      break;

    default:
      return false;
    }

  unsigned regno = REGNO (base);
  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      if (strict_p)
	regno = reg_renumber[regno];
      else
	return true;
    }
  if (strict_p)
    return regno <= 31;
  else
    return REGNO_OK_FOR_BASE_P (regno);
}

/* Return the TLS type for TLS symbols, 0 otherwise.  */

static tls_model
or1k_tls_symbolic_operand (rtx op)
{
  rtx sym, addend;
  split_const (op, &sym, &addend);
  if (SYMBOL_REF_P (sym))
    return SYMBOL_REF_TLS_MODEL (sym);
  return TLS_MODEL_NONE;
}

/* Get a reference to the '__tls_get_addr' symbol.  */

static GTY(()) rtx gen_tls_tga;

static rtx
gen_tls_get_addr (void)
{
  if (!gen_tls_tga)
    gen_tls_tga = init_one_libfunc ("__tls_get_addr");
  return gen_tls_tga;
}

/* Emit a call to '__tls_get_addr'.  */

static void
or1k_tls_call (rtx dest, rtx arg)
{
  emit_library_call_value (gen_tls_get_addr (), dest, LCT_CONST,
			   Pmode, arg, Pmode);
}

/* Helper for or1k_legitimize_address_1.  Wrap X in an unspec.  */

static rtx
gen_sym_unspec (rtx x, int kind)
{
  return gen_rtx_UNSPEC (Pmode, gen_rtvec (1, x), kind);
}

/* Worker for TARGET_LEGITIMIZE_ADDRESS_DISPLACEMENT.
   Split an out-of-range address displacement into hi and lo parts.
   The hi part will have to be loaded into a register separately,
   but the low part will be folded into the memory operand.  */

static bool
or1k_legitimize_address_displacement (rtx *off1, rtx *off2,
				      poly_int64 poly_offset, machine_mode)
{
  HOST_WIDE_INT orig_offset = poly_offset;
  HOST_WIDE_INT lo, hi;

  /* If the displacement is within range of 2 addi insns, prefer that.
     Otherwise split as per normal, at which point the register allocator
     will see that OFF1 is not a valid add3 operand and load it into
     a register, as desired.  */
  if (orig_offset >= 0 && orig_offset < 2 * 32767)
    {
      hi = 32767;
      lo = orig_offset - hi;
    }
  else if (orig_offset < 0 && orig_offset >= 2 * -32768)
    {
      hi = -32768;
      lo = orig_offset - hi;
    }
  else
    {
      lo = sext_hwi (orig_offset, 16);
      hi = orig_offset - lo;
    }

  *off1 = GEN_INT (hi);
  *off2 = GEN_INT (lo);
  return true;
}

#undef  TARGET_LEGITIMIZE_ADDRESS_DISPLACEMENT
#define TARGET_LEGITIMIZE_ADDRESS_DISPLACEMENT \
  or1k_legitimize_address_displacement

/* Helper function to implement both TARGET_LEGITIMIZE_ADDRESS and expand the
   patterns "movqi", "movqi" and "movsi".  Returns an valid OpenRISC RTX that
   represents the argument X which is an invalid address RTX.  The argument
   SCRATCH may be used as a temporary when building addresses.  */

static rtx
or1k_legitimize_address_1 (rtx x, rtx scratch)
{
  rtx base, addend, t1, t2;
  tls_model tls_kind = TLS_MODEL_NONE;
  bool is_local = true;

  split_const (x, &base, &addend);
  switch (GET_CODE (base))
    {
    default:
      gcc_assert (can_create_pseudo_p ());
      base = force_reg (Pmode, base);
      break;

    case REG:
    case SUBREG:
      break;

    case SYMBOL_REF:
      tls_kind = SYMBOL_REF_TLS_MODEL (base);
      is_local = SYMBOL_REF_LOCAL_P (base);
      /* FALLTHRU */

    case LABEL_REF:
      switch (tls_kind)
	{
	case TLS_MODEL_NONE:
	  t1 = can_create_pseudo_p () ? gen_reg_rtx (Pmode) : scratch;
	  if (!flag_pic)
	    {
	      emit_insn (gen_rtx_SET (t1, gen_rtx_HIGH (Pmode, x)));
	      return gen_rtx_LO_SUM (Pmode, t1, x);
	    }
	  else if (is_local)
	    {
	      crtl->uses_pic_offset_table = 1;
	      t2 = gen_sym_unspec (x, UNSPEC_GOTOFF);
	      emit_insn (gen_rtx_SET (t1, gen_rtx_HIGH (Pmode, t2)));
	      emit_insn (gen_add3_insn (t1, t1, pic_offset_table_rtx));
	      return gen_rtx_LO_SUM (Pmode, t1, copy_rtx (t2));
	    }
	  else
	    {
	      base = gen_sym_unspec (base, UNSPEC_GOT);
	      crtl->uses_pic_offset_table = 1;
	      if (TARGET_CMODEL_LARGE)
		{
	          emit_insn (gen_rtx_SET (t1, gen_rtx_HIGH (Pmode, base)));
	          emit_insn (gen_add3_insn (t1, t1, pic_offset_table_rtx));
	          t2 = gen_rtx_LO_SUM (Pmode, t1, base);
		}
	      else
	        t2 = gen_rtx_LO_SUM (Pmode, pic_offset_table_rtx, base);
	      t2 = gen_const_mem (Pmode, t2);
	      emit_insn (gen_rtx_SET (t1, t2));
	      base = t1;
	    }
	  break;

	case TLS_MODEL_GLOBAL_DYNAMIC:
	case TLS_MODEL_LOCAL_DYNAMIC:
	  /* TODO: For now, treat LD as GD.  */
	  t1 = gen_reg_rtx (Pmode);
	  base = gen_sym_unspec (base, UNSPEC_TLSGD);
	  emit_insn (gen_rtx_SET (t1, gen_rtx_HIGH (Pmode, base)));
	  emit_insn (gen_rtx_SET (t1, gen_rtx_LO_SUM (Pmode, t1, base)));
	  crtl->uses_pic_offset_table = 1;
	  emit_insn (gen_add3_insn (t1, t1, pic_offset_table_rtx));
	  base = gen_reg_rtx (Pmode);
	  or1k_tls_call (base, t1);
	  break;

	case TLS_MODEL_INITIAL_EXEC:
	  t1 = gen_reg_rtx (Pmode);
	  t2 = gen_reg_rtx (Pmode);
	  base = gen_sym_unspec (base, UNSPEC_GOTTPOFF);
	  emit_insn (gen_rtx_SET (t1, gen_rtx_HIGH (Pmode, base)));
	  crtl->uses_pic_offset_table = 1;
	  emit_insn (gen_add3_insn (t1, t1, pic_offset_table_rtx));
	  t1 = gen_rtx_LO_SUM (Pmode, t1, base);
	  emit_move_insn (t2, gen_const_mem (Pmode, t1));
	  t1 = gen_rtx_REG (Pmode, TLS_REGNUM);
	  emit_insn (gen_add3_insn (t2, t2, t1));
	  base = t2;
	  break;

	case TLS_MODEL_LOCAL_EXEC:
	  x = gen_sym_unspec (x, UNSPEC_TPOFF);
	  t1 = gen_reg_rtx (Pmode);
	  emit_insn (gen_rtx_SET (t1, gen_rtx_HIGH (Pmode, x)));
	  t2 = gen_rtx_REG (Pmode, TLS_REGNUM);
	  emit_insn (gen_add3_insn (t1, t1, t2));
	  return gen_rtx_LO_SUM (Pmode, t1, x);

	default:
	  gcc_unreachable ();
	}
      break;

    /* Accept what we may have already emitted.  */

    case LO_SUM:
    case UNSPEC:
      return x;
    }

  /* If we get here, we still have addend outstanding.  */
  gcc_checking_assert (register_operand (base, Pmode));
  if (addend == const0_rtx)
    return base;
  if (satisfies_constraint_I (addend)
      || virtual_frame_reg_operand (base, VOIDmode))
    return gen_rtx_PLUS (Pmode, base, addend);
  else
    {
      rtx hi, lo;
      bool ok = (or1k_legitimize_address_displacement
		 (&hi, &lo, INTVAL (addend), SImode));
      gcc_assert (ok);

      t2 = can_create_pseudo_p () ? gen_reg_rtx (Pmode) : scratch;
      if (satisfies_constraint_I (hi))
	emit_insn (gen_addsi3 (t2, base, hi));
      else
	{
	  t1 = can_create_pseudo_p () ? gen_reg_rtx (Pmode) : scratch;
	  emit_move_insn (t1, hi);
	  emit_insn (gen_add3_insn (t2, base, t1));
	}
      if (lo == const0_rtx)
	return t2;
      else
	return gen_rtx_PLUS (Pmode, t2, lo);
    }
}

/* Worker for TARGET_LEGITIMIZE_ADDRESS.
   This delegates implementation to or1k_legitimize_address_1.  */

static rtx
or1k_legitimize_address (rtx x, rtx /* oldx */, machine_mode)
{
  return or1k_legitimize_address_1 (x, NULL_RTX);
}

#undef  TARGET_LEGITIMIZE_ADDRESS
#define TARGET_LEGITIMIZE_ADDRESS or1k_legitimize_address

/* Worker for TARGET_DELEGITIMIZE_ADDRESS.
   In the name of slightly smaller debug output, and to cater to
   general assembler lossage, recognize PIC+GOTOFF and turn it back
   into a direct symbol reference.  */

static rtx
or1k_delegitimize_address (rtx x)
{
  if (GET_CODE (x) == UNSPEC)
    {
      /* The LO_SUM to which X was attached has been stripped.
	 Since the only legitimate address we could have been computing
	 is that of the symbol, assume that's what we've done.  */
      if (XINT (x, 1) == UNSPEC_GOTOFF)
	return XVECEXP (x, 0, 0);
    }
  else if (MEM_P (x))
    {
      rtx addr = XEXP (x, 0);
      if (GET_CODE (addr) == LO_SUM
	  && XEXP (addr, 0) == pic_offset_table_rtx)
	{
	  rtx inner = XEXP (addr, 1);
	  if (GET_CODE (inner) == UNSPEC
	      && XINT (inner, 1) == UNSPEC_GOT)
	    return XVECEXP (inner, 0, 0);
	}
    }
  return delegitimize_mem_from_attrs (x);
}

#undef  TARGET_DELEGITIMIZE_ADDRESS
#define TARGET_DELEGITIMIZE_ADDRESS or1k_delegitimize_address

/* Worker for TARGET_CANNOT_FORCE_CONST_MEM.
   Primarily this is required for TLS symbols, but given that our move
   patterns *ought* to be able to handle any symbol at any time, we
   should never be spilling symbolic operands to the constant pool, ever.  */

static bool
or1k_cannot_force_const_mem (machine_mode, rtx x)
{
  rtx_code code = GET_CODE (x);
  return (code == SYMBOL_REF
	  || code == LABEL_REF
	  || code == CONST
	  || code == HIGH);
}

#undef  TARGET_CANNOT_FORCE_CONST_MEM
#define TARGET_CANNOT_FORCE_CONST_MEM or1k_cannot_force_const_mem

/* Worker for TARGET_LEGITIMATE_CONSTANT_P.
   Returns true is the RTX X represents a constant that can be used as an
   immediate operand in OpenRISC.  */

static bool
or1k_legitimate_constant_p (machine_mode, rtx x)
{
  switch (GET_CODE (x))
    {
    case CONST_INT:
    case CONST_WIDE_INT:
    case HIGH:
      /* We construct these, rather than spilling to memory.  */
      return true;

    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      /* These may need to be split and not reconstructed.  */
      return or1k_tls_symbolic_operand (x) == TLS_MODEL_NONE;

    default:
      return false;
    }
}

#undef  TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P or1k_legitimate_constant_p

/* Worker for TARGET_PASS_BY_REFERENCE.
   Returns true if an argument ARG should be passed by reference as
   required by the OpenRISC ABI.  On OpenRISC structures, unions and
   arguments larger than 64-bits are passed by reference.  */

static bool
or1k_pass_by_reference (cumulative_args_t, const function_arg_info &arg)
{
  if (arg.aggregate_type_p ())
    return true;
  HOST_WIDE_INT size = arg.type_size_in_bytes ();
  return size < 0 || size > 8;
}

/* Worker for TARGET_FUNCTION_VALUE.
   Returns an RTX representing the location where function return values will
   be stored.  On OpenRISC this is the register r11.  64-bit return value's
   upper 32-bits are returned in r12, this is automatically done by GCC.  */

static rtx
or1k_function_value (const_tree valtype,
		     const_tree /* fn_decl_or_type */,
		     bool /* outgoing */)
{
  return gen_rtx_REG (TYPE_MODE (valtype), RV_REGNUM);
}

/* Worker for TARGET_LIBCALL_VALUE.
   Returns an RTX representing the location where function return values to
   external libraries will be stored.  On OpenRISC this the same as local
   function calls.  */

static rtx
or1k_libcall_value (machine_mode mode,
		    const_rtx /* fun */)
{
  return gen_rtx_REG (mode, RV_REGNUM);
}


/* Worker for TARGET_FUNCTION_VALUE_REGNO_P.
   Returns true if REGNO is a valid register for storing a function return
   value.  */

static bool
or1k_function_value_regno_p (const unsigned int regno)
{
  return (regno == RV_REGNUM);
}

/* Worker for TARGET_STRICT_ARGUMENT_NAMING.
   Return true always as on OpenRISC the last argument in a variatic function
   is named.  */

static bool
or1k_strict_argument_naming (cumulative_args_t /* ca */)
{
  return true;
}

#undef  TARGET_STRICT_ARGUMENT_NAMING
#define TARGET_STRICT_ARGUMENT_NAMING or1k_strict_argument_naming

/* Worker for TARGET_FUNCTION_ARG.
   Return the next register to be used to hold a function argument or NULL_RTX
   if there's no more space.  Arugment CUM_V represents the current argument
   offset, zero for the first function argument.  OpenRISC function arguments
   maybe be passed in registers r3 to r8.  */

static rtx
or1k_function_arg (cumulative_args_t cum_v, const function_arg_info &arg)
{
  /* Handle the special marker for the end of the arguments.  */
  if (arg.end_marker_p ())
    return NULL_RTX;

  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int nreg = CEIL (GET_MODE_SIZE (arg.mode), UNITS_PER_WORD);

  /* Note that all large arguments are passed by reference.  */
  gcc_assert (nreg <= 2);
  if (arg.named && *cum + nreg <= 6)
    return gen_rtx_REG (arg.mode, *cum + 3);
  else
    return NULL_RTX;
}

/* Worker for TARGET_FUNCTION_ARG_ADVANCE.
   Update the cumulative args descriptor CUM_V to advance past the next function
   argument.  Note, this is not called for arguments passed on the stack.  */

static void
or1k_function_arg_advance (cumulative_args_t cum_v,
			   const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int nreg = CEIL (GET_MODE_SIZE (arg.mode), UNITS_PER_WORD);

  /* Note that all large arguments are passed by reference.  */
  gcc_assert (nreg <= 2);
  if (arg.named)
    *cum += nreg;
}

/* worker function for TARGET_RETURN_IN_MEMORY.
   Returns true if the argument of TYPE should be returned in memory.  On
   OpenRISC this is any value larger than 64-bits.  */

static bool
or1k_return_in_memory (const_tree type, const_tree /* fntype */)
{
  const HOST_WIDE_INT size = int_size_in_bytes (type);
  return (size == -1 || size > (2 * UNITS_PER_WORD));
}

/* Print reloc (x + add).  */

static void
output_addr_reloc (FILE *stream, rtx x, HOST_WIDE_INT add, const char *reloc)
{
  if (*reloc)
    {
      fputs (reloc, stream);
      fputc ('(', stream);
    }
  output_addr_const (stream, x);
  if (add)
    {
      if (add > 0)
	fputc ('+', stream);
      fprintf (stream, HOST_WIDE_INT_PRINT_DEC, add);
    }
  if (*reloc)
    fputc (')', stream);
}

enum reloc_kind
{
  RKIND_LO,
  RKIND_HI,
  RKIND_MAX
};

enum reloc_type
{
  RTYPE_DIRECT,
  RTYPE_GOT,
  RTYPE_GOTOFF,
  RTYPE_TPOFF,
  RTYPE_GOTTPOFF,
  RTYPE_TLSGD,
  RTYPE_MAX
};

static void
print_reloc (FILE *stream, rtx x, HOST_WIDE_INT add, reloc_kind kind)
{
  /* All data relocations.  A NULL in this table indicates a form that
     we expect to never generate, while "" indicates a form that requires
     no special markup.  */
  static const char * const relocs[RKIND_MAX][RTYPE_MAX] = {
    { "lo", "got", "gotofflo", "tpofflo", "gottpofflo", "tlsgdlo" },
    { "ha", "gotha", "gotoffha", "tpoffha", "gottpoffha", "tlsgdhi" },
  };
  reloc_type type = RTYPE_DIRECT;

  if (GET_CODE (x) == UNSPEC)
    {
      switch (XINT (x, 1))
	{
	case UNSPEC_GOT:
	  type = RTYPE_GOT;
	  break;
	case UNSPEC_GOTOFF:
	  type = RTYPE_GOTOFF;
	  break;
	case UNSPEC_TPOFF:
	  type = RTYPE_TPOFF;
	  break;
	case UNSPEC_GOTTPOFF:
	  type = RTYPE_GOTTPOFF;
	  break;
	case UNSPEC_TLSGD:
	  type = RTYPE_TLSGD;
	  break;
	default:
	  output_operand_lossage ("invalid relocation");
	  return;
	}
      x = XVECEXP (x, 0, 0);
    }

  const char *reloc = relocs[kind][type];
  if (reloc == NULL)
    output_operand_lossage ("invalid relocation");
  else
    output_addr_reloc (stream, x, add, reloc);
}

/* Worker for TARGET_PRINT_OPERAND_ADDRESS.
   Prints the argument ADDR, an address RTX, to the file FILE.  The output is
   formed as expected by the OpenRISC assembler.  Examples:

     RTX							      OUTPUT
     (reg:SI 3)							       0(r3)
     (plus:SI (reg:SI 3) (const_int 4))				     0x4(r3)
     (lo_sum:SI (reg:SI 3) (symbol_ref:SI ("x"))))		   lo(x)(r3)  */

static void
or1k_print_operand_address (FILE *file, machine_mode, rtx addr)
{
  rtx offset;

  switch (GET_CODE (addr))
    {
    case REG:
      fputc ('0', file);
      break;

    case PLUS:
      offset = XEXP (addr, 1);
      addr = XEXP (addr, 0);
      gcc_assert (CONST_INT_P (offset));
      if (GET_CODE (addr) == LO_SUM)
	{
	  print_reloc (file, XEXP (addr, 1), INTVAL (offset), RKIND_LO);
	  addr = XEXP (addr, 0);
	}
      else
	output_addr_const (file, offset);
      break;

    case LO_SUM:
      offset = XEXP (addr, 1);
      addr = XEXP (addr, 0);
      print_reloc (file, offset, 0, RKIND_LO);
      break;

    default:
      output_addr_const (file, addr);
      return;
    }

  fprintf (file, "(%s)", reg_names[REGNO (addr)]);
}

/* Worker for TARGET_PRINT_OPERAND.
   Print operand X, an RTX, to the file FILE.  The output is formed as expected
   by the OpenRISC assember.  CODE is the letter following a '%' in an
   instrunction template used to control the RTX output.  Example(s):

     CODE   RTX                   OUTPUT     COMMENT
     0      (reg:SI 3)                r3     output an operand
     r      (reg:SI 3)                r3     output a register or const zero
     H      (reg:SI 3)                r4     output the high pair register
     h      (symbol_ref:SI ("x"))  ha(x)     output a signed high relocation
     L      (symbol_ref:SI ("x"))  lo(x)     output a low relocation

   Note, '#' is a special code used to fill the branch delay slot with an l.nop
   instruction.  The l.nop (no-op) instruction is only outputted when the delay
   slot has not been filled.  */

static void
or1k_print_operand (FILE *file, rtx x, int code)
{
  rtx operand = x;

  switch (code)
    {
    case '#':
      /* Conditionally add a nop in unfilled delay slot.  */
      if (final_sequence == NULL)
	fputs ("\n\t l.nop\n", file);
      break;

    case 'r':
      if (REG_P (x))
	fprintf (file, "%s", reg_names[REGNO (operand)]);
      else if (x == CONST0_RTX (GET_MODE (x)))
	fprintf (file, "r0");
      else
	output_operand_lossage ("invalid %%r value");
      break;

    case 'H':
      if (REG_P (x))
	fprintf (file, "%s", reg_names[REGNO (operand) + 1]);
      else
	output_operand_lossage ("invalid %%H value");
      break;

    case 'd':
      if (REG_P (x))
	{
	  if (GET_MODE (x) == DFmode || GET_MODE (x) == DImode)
	    fprintf (file, "%s,%s", reg_names[REGNO (operand)],
				    reg_names[REGNO (operand) + 1]);
	  else
	    fprintf (file, "%s", reg_names[REGNO (operand)]);
	}
      else
	output_operand_lossage ("invalid %%d value");
      break;

    case 'h':
      print_reloc (file, x, 0, RKIND_HI);
      break;
    case 'L':
      print_reloc (file, x, 0, RKIND_LO);
      break;
    case 'P':
      if (!flag_pic || SYMBOL_REF_LOCAL_P (x))
	output_addr_const (file, x);
      else
	output_addr_reloc (file, x, 0, "plt");
      break;

    case 0:
      /* Print an operand as without a modifier letter.  */
      switch (GET_CODE (operand))
	{
	case REG:
	  if (REGNO (operand) > 31)
	    internal_error ("internal error: bad register: %d",
			    REGNO (operand));
	  fprintf (file, "%s", reg_names[REGNO (operand)]);
	  break;

	case MEM:
	  output_address (GET_MODE (XEXP (operand, 0)), XEXP (operand, 0));
	  break;

	case CODE_LABEL:
	case LABEL_REF:
	  output_asm_label (operand);
	  break;

	default:
	  /* No need to handle all strange variants, let output_addr_const
	     do it for us.  */
	  if (CONSTANT_P (operand))
	    output_addr_const (file, operand);
	  else
	    internal_error ("unexpected operand: %d", GET_CODE (operand));
	  break;
	}
      break;

    default:
      output_operand_lossage ("unknown operand letter: '%c'", code);
      break;
    }
}

/* Worker for TARGET_TRAMPOLINE_INIT.
   This is called to initialize a trampoline.  The argument M_TRAMP is an RTX
   for the memory block to be initialized with trampoline code.  The argument
   FNDECL contains the definition of the nested function to be called, we use
   this to get the function's address.  The argument CHAIN is an RTX for the
   static chain value to be passed to the nested function.  */

static void
or1k_trampoline_init (rtx m_tramp, tree fndecl, rtx chain)
{
  const unsigned movhi_r13 = (0x06u << 26) | (13 << 21);
  const unsigned movhi_r11 = (0x06u << 26) | (11 << 21);
  const unsigned ori_r13_r13 = (0x2a << 26) | (13 << 21) | (13 << 16);
  const unsigned ori_r11_r11 = (0x2a << 26) | (11 << 21) | (11 << 16);
  const unsigned jr_r13 = (0x11 << 26) | (13 << 11);
  rtx tramp[5], fnaddr, f_hi, f_lo, c_hi, c_lo;

  fnaddr = force_operand (XEXP (DECL_RTL (fndecl), 0), NULL);
  f_hi = expand_binop (SImode, lshr_optab, fnaddr, GEN_INT (16),
		       NULL, true, OPTAB_DIRECT);
  f_lo = expand_binop (SImode, and_optab, fnaddr, GEN_INT (0xffff),
		       NULL, true, OPTAB_DIRECT);

  chain = force_operand (chain, NULL);
  c_hi = expand_binop (SImode, lshr_optab, chain, GEN_INT (16),
		       NULL, true, OPTAB_DIRECT);
  c_lo = expand_binop (SImode, and_optab, chain, GEN_INT (0xffff),
		       NULL, true, OPTAB_DIRECT);

  /* We want to generate

	l.movhi r13,hi(nested_func)
	l.movhi r11,hi(static_chain)
	l.ori	r13,r13,lo(nested_func)
	l.jr	r13
	 l.ori	r11,r11,lo(static_chain)
   */
  tramp[0] = expand_binop (SImode, ior_optab, f_hi,
			   gen_int_mode (movhi_r13, SImode),
			   f_hi, true, OPTAB_DIRECT);
  tramp[1] = expand_binop (SImode, ior_optab, c_hi,
			   gen_int_mode (movhi_r11, SImode),
			   c_hi, true, OPTAB_DIRECT);
  tramp[2] = expand_binop (SImode, ior_optab, f_lo,
			   gen_int_mode (ori_r13_r13, SImode),
			   f_lo, true, OPTAB_DIRECT);
  tramp[4] = expand_binop (SImode, ior_optab, c_lo,
			   gen_int_mode (ori_r11_r11, SImode),
			   c_lo, true, OPTAB_DIRECT);
  tramp[3] = gen_int_mode (jr_r13, SImode);

  for (int i = 0; i < 5; ++i)
    {
      rtx mem = adjust_address (m_tramp, SImode, i * 4);
      emit_move_insn (mem, tramp[i]);
    }

  /* Flushing the trampoline from the instruction cache needs
     to be done here. */
}

/* Worker for TARGET_HARD_REGNO_MODE_OK.
   Returns true if the hard register REGNO is ok for storing values of mode
   MODE.  */

static bool
or1k_hard_regno_mode_ok (unsigned int regno, machine_mode mode)
{
  /* For OpenRISC, GENERAL_REGS can hold anything, while
     FLAG_REGS are really single bits within SP[SR].  */
  if (REGNO_REG_CLASS (regno) == FLAG_REGS)
    return mode == BImode;
  return true;
}

#undef TARGET_HARD_REGNO_MODE_OK
#define TARGET_HARD_REGNO_MODE_OK or1k_hard_regno_mode_ok

/* Worker for TARGET_CAN_CHANGE_MODE_CLASS.
   Returns true if its ok to change a register in class RCLASS from mode FROM to
   mode TO.  In general OpenRISC registers, other than special flags, handle all
   supported classes.  */

static bool
or1k_can_change_mode_class (machine_mode from, machine_mode to,
			    reg_class_t rclass)
{
  if (rclass == FLAG_REGS)
    return from == to;
  return true;
}

#undef TARGET_CAN_CHANGE_MODE_CLASS
#define TARGET_CAN_CHANGE_MODE_CLASS or1k_can_change_mode_class

/* Expand the patterns "movqi", "movqi" and "movsi".  The argument OP0 is the
   destination and OP1 is the source.  This expands to set OP0 to OP1.  OpenRISC
   cannot do memory to memory assignments so for those cases we force one
   argument to a register.  Constants that can't fit into a 16-bit immediate are
   split.  Symbols are legitimized using split relocations.  */

void
or1k_expand_move (machine_mode mode, rtx op0, rtx op1)
{
  if (MEM_P (op0))
    {
      if (!const0_operand (op1, mode))
	op1 = force_reg (mode, op1);
    }
  else if (mode == QImode || mode == HImode)
    {
      /* ??? Maybe promote MEMs and CONST_INT to SImode,
	 and then squish back with gen_lowpart.  */
    }
  else
    {
      switch (GET_CODE (op1))
	{
	case CONST_INT:
	  if (!input_operand (op1, mode))
	    {
	      HOST_WIDE_INT i = INTVAL (op1);
	      HOST_WIDE_INT lo = i & 0xffff;
	      HOST_WIDE_INT hi = i ^ lo;
	      rtx subtarget = op0;

	      if (!cse_not_expected && can_create_pseudo_p ())
		subtarget = gen_reg_rtx (SImode);
	      emit_insn (gen_rtx_SET (subtarget, GEN_INT (hi)));
	      emit_insn (gen_iorsi3 (op0, subtarget, GEN_INT (lo)));
	      return;
	    }
	  break;

	case CONST:
	case SYMBOL_REF:
	case LABEL_REF:
	  op1 = or1k_legitimize_address_1 (op1, op0);
	  break;

	default:
	  break;
	}
    }
  emit_insn (gen_rtx_SET (op0, op1));
}

/* Used to expand patterns "movsicc", "movqicc", "movhicc", "cstoresi4" and
   "cbranchsi4".
   Expands a comparison where OPERANDS is an array of RTX describing the
   comparison.  The first argument OPERANDS[0] is the operator and OPERANDS[1]
   and OPERANDS[2] are the operands.  Split out the compare into SR[F] and
   return a new operation in OPERANDS[0].  The inputs OPERANDS[1] and
   OPERANDS[2] are not directly used, only overridden.  */

void
or1k_expand_compare (rtx *operands)
{
  rtx sr_f = gen_rtx_REG (BImode, SR_F_REGNUM);
  rtx righthand_op = XEXP (operands[0], 1);
  rtx_code cmp_code = GET_CODE (operands[0]);
  bool flag_check_ne = true;

  /* Integer RTL may receive an immediate in argument 1 of the compare, this is
     not supported unless we have l.sf*i instructions, force them into
     registers.  */
  if (!TARGET_SFIMM && CONST_INT_P (righthand_op))
    XEXP (operands[0], 1) = force_reg (SImode, righthand_op);

  /* Normalize comparison operators to ones OpenRISC support.  */
  switch (cmp_code)
    {
      case LTGT:
	cmp_code = UNEQ;
	flag_check_ne = false;
	break;

      case ORDERED:
	cmp_code = UNORDERED;
	flag_check_ne = false;
	break;

      default:
	break;
    }

  /* Emit the given comparison into the Flag bit.  */
  PUT_MODE (operands[0], BImode);
  PUT_CODE (operands[0], cmp_code);
  emit_insn (gen_rtx_SET (sr_f, operands[0]));

  /* Adjust the operands for use in the caller.  */
  operands[0] = flag_check_ne ? gen_rtx_NE (VOIDmode, sr_f, const0_rtx)
			      : gen_rtx_EQ (VOIDmode, sr_f, const0_rtx);
  operands[1] = sr_f;
  operands[2] = const0_rtx;
 }

/* Expand the patterns "call", "sibcall", "call_value" and "sibcall_value".
   Expands a function call where argument RETVAL is an optional RTX providing
   return value storage, the argument FNADDR is and RTX describing the function
   to call, the argument CALLARG1 is the number or registers used as operands
   and the argument SIBCALL should be true if this is a nested function call.
   If FNADDR is a non local symbol and FLAG_PIC is enabled this will generate
   a PLT call.  */

void
or1k_expand_call (rtx retval, rtx fnaddr, rtx callarg1, bool sibcall)
{
  rtx call, use = NULL;

  /* Calls via the PLT require the PIC register.  */
  if (flag_pic
      && GET_CODE (XEXP (fnaddr, 0)) == SYMBOL_REF
      && !SYMBOL_REF_LOCAL_P (XEXP (fnaddr, 0)))
    {
      crtl->uses_pic_offset_table = 1;
      rtx hard_pic = gen_rtx_REG (Pmode, REAL_PIC_OFFSET_TABLE_REGNUM);
      emit_move_insn (hard_pic, pic_offset_table_rtx);
      use_reg (&use, hard_pic);
    }

  if (!call_insn_operand (XEXP (fnaddr, 0), Pmode))
    {
      fnaddr = copy_to_mode_reg (Pmode, XEXP (fnaddr, 0));
      fnaddr = gen_rtx_MEM (SImode, fnaddr);
    }

  call = gen_rtx_CALL (VOIDmode, fnaddr, callarg1);
  if (retval)
    call = gen_rtx_SET (retval, call);

  /* Normal calls clobber LR.  This is required in order to
     prevent e.g. a prologue store of LR being placed into
     the delay slot of the call, after it has been updated.  */
  if (!sibcall)
    {
      rtx clob = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (Pmode, LR_REGNUM));
      call = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, call, clob));
    }
  call = emit_call_insn (call);

  CALL_INSN_FUNCTION_USAGE (call) = use;
}

/* Worker for TARGET_FUNCTION_OK_FOR_SIBCALL.
   Returns true if the function declared by DECL is ok for calling as a nested
   function.  */

static bool
or1k_function_ok_for_sibcall (tree decl, tree /* exp */)
{
  /* We can sibcall to any function if not PIC.  */
  if (!flag_pic)
    return true;

  /* We can sibcall any indirect function.  */
  if (decl == NULL)
    return true;

  /* If the call may go through the PLT, we need r16 live.  */
  return targetm.binds_local_p (decl);
}

#undef TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL or1k_function_ok_for_sibcall

/* Worker for TARGET_RTX_COSTS.  */

static bool
or1k_rtx_costs (rtx x, machine_mode mode, int outer_code, int /* opno */,
		int *total, bool /* speed */)
{
  switch (GET_CODE (x))
    {
    case CONST_INT:
      if (x == const0_rtx)
	*total = 0;
      else if ((outer_code == PLUS || outer_code == XOR || outer_code == MULT)
	       && satisfies_constraint_I (x))
	*total = 0;
      else if ((outer_code == AND || outer_code == IOR)
	       && satisfies_constraint_K (x))
	*total = 0;
      else if (satisfies_constraint_I (x)
	       || satisfies_constraint_K (x)
	       || satisfies_constraint_M (x))
	*total = 2;
      else
	*total = COSTS_N_INSNS (2);
      return true;

    case CONST_DOUBLE:
      *total = (x == CONST0_RTX (mode) ? 0 : COSTS_N_INSNS (2));
      return true;

    case HIGH:
      /* This is effectively an 'M' constraint.  */
      *total = 2;
      return true;

    case LO_SUM:
      /* This is effectively an 'I' constraint.  */
      *total = (outer_code == MEM ? 0 : 2);
      return true;

    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      if (outer_code == LO_SUM || outer_code == HIGH)
	*total = 0;
      else
	{
	  /* ??? Extra cost for GOT or TLS symbols.  */
	  *total = COSTS_N_INSNS (1 + (outer_code != MEM));
	}
      return true;

    case PLUS:
      if (outer_code == MEM)
	*total = 0;
      break;

    default:
      break;
    }
  return false;
}

#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS or1k_rtx_costs


/* A subroutine of the atomic operation splitters.  Jump to LABEL if
   COND is true.  Mark the jump as unlikely to be taken.  */

static void
emit_unlikely_jump (rtx_code code, rtx label)
{
  rtx x;

  x = gen_rtx_REG (BImode, SR_F_REGNUM);
  x = gen_rtx_fmt_ee (code, VOIDmode, x, const0_rtx);
  x = gen_rtx_IF_THEN_ELSE (VOIDmode, x, label, pc_rtx);
  emit_jump_insn (gen_rtx_SET (pc_rtx, x));

  // Disable this for now -- producing verify_cfg failures on probabilities.
  // int very_unlikely = REG_BR_PROB_BASE / 100 - 1;
  // add_int_reg_note (insn, REG_BR_PROB, very_unlikely);
}

/* A subroutine of the atomic operation splitters.
   Emit a raw comparison for A CODE B.  */

static void
emit_compare (rtx_code code, rtx a, rtx b)
{
  emit_insn (gen_rtx_SET (gen_rtx_REG (BImode, SR_F_REGNUM),
			  gen_rtx_fmt_ee (code, BImode, a, b)));
}

/* A subroutine of the atomic operation splitters.
   Emit a load-locked instruction in MODE.  */

static void
emit_load_locked (machine_mode mode, rtx reg, rtx mem)
{
  gcc_assert (mode == SImode);
  emit_insn (gen_load_locked_si (reg, mem));
}

/* A subroutine of the atomic operation splitters.
   Emit a store-conditional instruction in MODE.  */

static void
emit_store_conditional (machine_mode mode, rtx mem, rtx val)
{
  gcc_assert (mode == SImode);
  emit_insn (gen_store_conditional_si (mem, val));
}

/* A subroutine of the various atomic expanders.  For sub-word operations,
   we must adjust things to operate on SImode.  Given the original MEM,
   return a new aligned memory.  Also build and return the quantities by
   which to shift and mask.  */

static rtx
or1k_adjust_atomic_subword (rtx orig_mem, rtx *pshift, rtx *pmask)
{
  rtx addr, align, shift, mask, mem;
  machine_mode mode = GET_MODE (orig_mem);

  addr = XEXP (orig_mem, 0);
  addr = force_reg (Pmode, addr);

  /* Aligned memory containing subword.  Generate a new memory.  We
     do not want any of the existing MEM_ATTR data, as we're now
     accessing memory outside the original object.  */
  align = expand_binop (Pmode, and_optab, addr, GEN_INT (-4),
			NULL_RTX, 1, OPTAB_LIB_WIDEN);
  mem = gen_rtx_MEM (SImode, align);
  MEM_VOLATILE_P (mem) = MEM_VOLATILE_P (orig_mem);
  if (MEM_ALIAS_SET (orig_mem) == ALIAS_SET_MEMORY_BARRIER)
    set_mem_alias_set (mem, ALIAS_SET_MEMORY_BARRIER);

  /* Shift amount for subword relative to aligned word.  */
  rtx mode_mask = GEN_INT (mode == QImode ? 3 : 2);
  shift = expand_binop (SImode, and_optab, gen_lowpart (SImode, addr),
			mode_mask, NULL_RTX, 1, OPTAB_LIB_WIDEN);
  if (BYTES_BIG_ENDIAN)
    shift = expand_binop (SImode, xor_optab, shift, mode_mask,
			  shift, 1, OPTAB_LIB_WIDEN);
  shift = expand_binop (SImode, ashl_optab, shift, GEN_INT (3),
			shift, 1, OPTAB_LIB_WIDEN);
  *pshift = shift;

  /* Mask for insertion.  */
  mask = expand_binop (SImode, ashl_optab, GEN_INT (GET_MODE_MASK (mode)),
		       shift, NULL_RTX, 1, OPTAB_LIB_WIDEN);
  *pmask = mask;

  return mem;
}

/* A subroutine of the various atomic expanders.  For sub-word operations,
   complete the operation by shifting result to the lsb of the SImode
   temporary and then extracting the result in MODE with a SUBREG.  */

static void
or1k_finish_atomic_subword (machine_mode mode, rtx o, rtx n, rtx shift)
{
  n = expand_binop (SImode, lshr_optab, n, shift,
		    NULL_RTX, 1, OPTAB_LIB_WIDEN);
  emit_move_insn (o, gen_lowpart (mode, n));
}

/* Expand an atomic compare and swap operation.
   Emits the RTX to perform a compare and swap operation.  This function takes
   8 RTX arguments in the OPERANDS array.  The compare and swap operation
   loads a value from memory (OPERANDS[2]) and compares it with an expected
   value (OPERANDS[3]), if the values are equal it stores a new value
   (OPERANDS[4]) to memory.  The argument OPERANDS[0] represents a boolean
   result which will be set to true if the operation succeeds.  A return value
   (OPERANDS[1]) will be set to what was loaded from memory.  The argument
   OPERAND[5] is used to indicate if the compare and swap is to be treated as
   weak.  OpenRISC does not use OPERANDS[5] or OPERANDS[6] which provide memory
   model details.
   For OpenRISC this emits RTX which will translate to assembly using the
   'l.lwa' (load word atomic) and 'l.swa' (store word atomic) instructions.  */

void
or1k_expand_atomic_compare_and_swap (rtx operands[])
{
  rtx boolval, retval, mem, oldval, newval;
  rtx label1, label2;
  machine_mode mode;
  bool is_weak;

  boolval = operands[0];
  retval = operands[1];
  mem = operands[2];
  oldval = operands[3];
  newval = operands[4];
  is_weak = (INTVAL (operands[5]) != 0);
  mode = GET_MODE (mem);

  if (reg_overlap_mentioned_p (retval, oldval))
    oldval = copy_to_reg (oldval);

  label1 = NULL_RTX;
  /* If strong, create a label to try again.  */
  if (!is_weak)
    {
      label1 = gen_rtx_LABEL_REF (VOIDmode, gen_label_rtx ());
      emit_label (XEXP (label1, 0));
    }
  label2 = gen_rtx_LABEL_REF (VOIDmode, gen_label_rtx ());

  emit_load_locked (mode, retval, mem);
  emit_compare (EQ, retval, oldval);
  emit_unlikely_jump (EQ, label2);
  emit_store_conditional (mode, mem, newval);

  /* If strong, jump back to try again on fails.  */
  if (!is_weak)
    emit_unlikely_jump (EQ, label1);
  emit_label (XEXP (label2, 0));

  /* In all cases, SR_F contains 1 on success, and 0 on failure.  */
  emit_insn (gen_sne_sr_f (boolval));
}

void
or1k_expand_atomic_compare_and_swap_qihi (rtx operands[])
{
  rtx boolval, orig_retval, retval, scratch, mem, oldval, newval;
  rtx label1, label2, mask, shift;
  machine_mode mode;
  bool is_weak;

  boolval = operands[0];
  orig_retval = operands[1];
  mem = operands[2];
  oldval = operands[3];
  newval = operands[4];
  is_weak = (INTVAL (operands[5]) != 0);
  mode = GET_MODE (mem);

  mem = or1k_adjust_atomic_subword (mem, &shift, &mask);

  /* Shift and mask OLDVAL and NEWVAL into position with the word.  */
  if (oldval != const0_rtx)
    {
      oldval = convert_modes (SImode, mode, oldval, 1);
      oldval = expand_binop (SImode, ashl_optab, oldval, shift,
			     NULL_RTX, 1, OPTAB_LIB_WIDEN);
    }
  if (newval != const0_rtx)
    {
      newval = convert_modes (SImode, mode, newval, 1);
      newval = expand_binop (SImode, ashl_optab, newval, shift,
			     NULL_RTX, 1, OPTAB_LIB_WIDEN);
    }

  label1 = NULL_RTX;
  if (!is_weak)
    {
      label1 = gen_rtx_LABEL_REF (VOIDmode, gen_label_rtx ());
      emit_label (XEXP (label1, 0));
    }
  label2 = gen_rtx_LABEL_REF (VOIDmode, gen_label_rtx ());

  scratch = gen_reg_rtx (SImode);
  emit_load_locked (SImode, scratch, mem);

  retval = expand_binop (SImode, and_optab, scratch, mask,
			 NULL_RTX, 1, OPTAB_LIB_WIDEN);
  scratch = expand_binop (SImode, xor_optab, scratch, retval,
			  scratch, 1, OPTAB_LIB_WIDEN);

  emit_compare (EQ, retval, oldval);
  emit_unlikely_jump (EQ, label2);

  if (newval != const0_rtx)
    scratch = expand_binop (SImode, ior_optab, scratch, newval,
			    scratch, 1, OPTAB_LIB_WIDEN);

  emit_store_conditional (SImode, mem, scratch);

  if (!is_weak)
    emit_unlikely_jump (EQ, label1);
  emit_label (XEXP (label2, 0));

  or1k_finish_atomic_subword (mode, orig_retval, retval, shift);

  /* In all cases, SR_F contains 1 on success, and 0 on failure.  */
  emit_insn (gen_sne_sr_f (boolval));
}

/* Expand an atomic exchange operation.
   Emits the RTX to perform an exchange operation.  This function takes 4 RTX
   arguments in the OPERANDS array.  The exchange operation atomically loads a
   value from memory (OPERANDS[1]) to a return value (OPERANDS[0]) and stores a
   new value (OPERANDS[2]) back to the memory location.
   Another argument (OPERANDS[3]) is used to indicate the memory model and
   is not used by OpenRISC.
   For OpenRISC this emits RTX which will translate to assembly using the
   'l.lwa' (load word atomic) and 'l.swa' (store word atomic) instructions.  */

void
or1k_expand_atomic_exchange (rtx operands[])
{
  rtx retval, mem, val, label;
  machine_mode mode;

  retval = operands[0];
  mem = operands[1];
  val = operands[2];
  mode = GET_MODE (mem);

  if (reg_overlap_mentioned_p (retval, val))
    val = copy_to_reg (val);

  label = gen_rtx_LABEL_REF (VOIDmode, gen_label_rtx ());
  emit_label (XEXP (label, 0));

  emit_load_locked (mode, retval, mem);
  emit_store_conditional (mode, mem, val);
  emit_unlikely_jump (EQ, label);
}

void
or1k_expand_atomic_exchange_qihi (rtx operands[])
{
  rtx orig_retval, retval, mem, val, scratch;
  rtx label, mask, shift;
  machine_mode mode;

  orig_retval = operands[0];
  mem = operands[1];
  val = operands[2];
  mode = GET_MODE (mem);

  mem = or1k_adjust_atomic_subword (mem, &shift, &mask);

  /* Shift and mask VAL into position with the word.  */
  if (val != const0_rtx)
    {
      val = convert_modes (SImode, mode, val, 1);
      val = expand_binop (SImode, ashl_optab, val, shift,
			  NULL_RTX, 1, OPTAB_LIB_WIDEN);
    }

  label = gen_rtx_LABEL_REF (VOIDmode, gen_label_rtx ());
  emit_label (XEXP (label, 0));

  scratch = gen_reg_rtx (SImode);
  emit_load_locked (SImode, scratch, mem);

  retval = expand_binop (SImode, and_optab, scratch, mask,
			 NULL_RTX, 1, OPTAB_LIB_WIDEN);
  scratch = expand_binop (SImode, xor_optab, scratch, retval,
			  scratch, 1, OPTAB_LIB_WIDEN);
  if (val != const0_rtx)
    scratch = expand_binop (SImode, ior_optab, scratch, val,
			    scratch, 1, OPTAB_LIB_WIDEN);

  emit_store_conditional (SImode, mem, scratch);
  emit_unlikely_jump (EQ, label);

  or1k_finish_atomic_subword (mode, orig_retval, retval, shift);
}

/* Expand an atomic fetch-and-operate pattern.  CODE is the binary operation
   to perform (with MULT as a stand-in for NAND).  MEM is the memory on which
   to operate.  VAL is the second operand of the binary operator.  BEFORE and
   AFTER are optional locations to return the value of MEM either before of
   after the operation.  */

void
or1k_expand_atomic_op (rtx_code code, rtx mem, rtx val,
		       rtx orig_before, rtx orig_after)
{
  machine_mode mode = GET_MODE (mem);
  rtx before = orig_before, after = orig_after;
  rtx label;

  label = gen_rtx_LABEL_REF (VOIDmode, gen_label_rtx ());
  emit_label (XEXP (label, 0));

  if (before == NULL_RTX)
    before = gen_reg_rtx (mode);

  emit_load_locked (mode, before, mem);

  if (code == MULT)
    {
      after = expand_binop (mode, and_optab, before, val,
			    after, 1, OPTAB_LIB_WIDEN);
      after = expand_unop (mode, one_cmpl_optab, after, after, 1);
    }
  else
    after = expand_simple_binop (mode, code, before, val,
				 after, 1, OPTAB_LIB_WIDEN);

  emit_store_conditional (mode, mem, after);
  emit_unlikely_jump (EQ, label);

  if (orig_before)
    emit_move_insn (orig_before, before);
  if (orig_after)
    emit_move_insn (orig_after, after);
}

void
or1k_expand_atomic_op_qihi (rtx_code code, rtx mem, rtx val,
			    rtx orig_before, rtx orig_after)
{
  machine_mode mode = GET_MODE (mem);
  rtx label, mask, shift, x;
  rtx before, after, scratch;

  mem = or1k_adjust_atomic_subword (mem, &shift, &mask);

  /* Shift and mask VAL into position with the word.  */
  val = convert_modes (SImode, mode, val, 1);
  val = expand_binop (SImode, ashl_optab, val, shift,
		      NULL_RTX, 1, OPTAB_LIB_WIDEN);

  switch (code)
    {
    case IOR:
    case XOR:
      /* We've already zero-extended VAL.  That is sufficient to
	 make certain that it does not affect other bits.  */
      break;

    case AND:
    case MULT: /* NAND */
      /* If we make certain that all of the other bits in VAL are
	 set, that will be sufficient to not affect other bits.  */
      x = expand_unop (SImode, one_cmpl_optab, mask, NULL_RTX, 1);
      val = expand_binop (SImode, ior_optab, val, x,
			  val, 1, OPTAB_LIB_WIDEN);
      break;

    case PLUS:
    case MINUS:
      /* These will all affect bits outside the field and need
	 adjustment via MASK within the loop.  */
      break;

    default:
      gcc_unreachable ();
    }

  label = gen_rtx_LABEL_REF (VOIDmode, gen_label_rtx ());
  emit_label (XEXP (label, 0));

  before = scratch = gen_reg_rtx (SImode);
  emit_load_locked (SImode, before, mem);

  switch (code)
    {
    case IOR:
    case XOR:
    case AND:
      after = expand_simple_binop (SImode, code, before, val,
				   NULL_RTX, 1, OPTAB_LIB_WIDEN);
      scratch = after;
      break;

    case PLUS:
    case MINUS:
      before = expand_binop (SImode, and_optab, scratch, mask,
			     NULL_RTX, 1, OPTAB_LIB_WIDEN);
      scratch = expand_binop (SImode, xor_optab, scratch, before,
			      scratch, 1, OPTAB_LIB_WIDEN);
      after = expand_simple_binop (SImode, code, before, val,
				   NULL_RTX, 1, OPTAB_LIB_WIDEN);
      after = expand_binop (SImode, and_optab, after, mask,
			    after, 1, OPTAB_LIB_WIDEN);
      scratch = expand_binop (SImode, ior_optab, scratch, after,
			      scratch, 1, OPTAB_LIB_WIDEN);
      break;

    case MULT: /* NAND */
      after = expand_binop (SImode, and_optab, before, val,
			    NULL_RTX, 1, OPTAB_LIB_WIDEN);
      after = expand_binop (SImode, xor_optab, after, mask,
			    after, 1, OPTAB_LIB_WIDEN);
      scratch = after;
      break;

    default:
      gcc_unreachable ();
    }

  emit_store_conditional (SImode, mem, scratch);
  emit_unlikely_jump (EQ, label);

  if (orig_before)
    or1k_finish_atomic_subword (mode, orig_before, before, shift);
  if (orig_after)
    or1k_finish_atomic_subword (mode, orig_after, after, shift);
}

/* Worker for TARGET_ASM_OUTPUT_MI_THUNK.
   Output the assembler code for a thunk function.  THUNK_DECL is the
   declaration for the thunk function itself, FUNCTION is the decl for
   the target function.  DELTA is an immediate constant offset to be
   added to THIS.  If VCALL_OFFSET is nonzero, the word at address
   (*THIS + VCALL_OFFSET) should be additionally added to THIS.  */

static void
or1k_output_mi_thunk (FILE *file, tree thunk_fndecl,
		      HOST_WIDE_INT delta, HOST_WIDE_INT vcall_offset,
		      tree function)
{
  const char *fnname = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (thunk_fndecl));
  rtx this_rtx, funexp;
  rtx_insn *insn;

  reload_completed = 1;
  epilogue_completed = 1;

  emit_note (NOTE_INSN_PROLOGUE_END);

  /* Find the "this" pointer.  Normally in r3, but if the function
     returns a structure, the structure return pointer is in r3 and
     the "this" pointer is in r4 instead.  */
  if (aggregate_value_p (TREE_TYPE (TREE_TYPE (function)), function))
    this_rtx = gen_rtx_REG (Pmode, 4);
  else
    this_rtx = gen_rtx_REG (Pmode, 3);

  /* Add DELTA.  When possible use a plain add, otherwise load it
     into a register first.  */
  if (delta)
    {
      rtx delta_rtx = GEN_INT (delta);

      if (!satisfies_constraint_I (delta_rtx))
	{
	  rtx scratch = gen_rtx_REG (Pmode, PE_TMP_REGNUM);
	  emit_move_insn (scratch, delta_rtx);
	  delta_rtx = scratch;
	}

      /* THIS_RTX += DELTA.  */
      emit_insn (gen_add2_insn (this_rtx, delta_rtx));
    }

  /* Add the word at address (*THIS_RTX + VCALL_OFFSET).  */
  if (vcall_offset)
    {
      rtx scratch = gen_rtx_REG (Pmode, PE_TMP_REGNUM);
      HOST_WIDE_INT lo = sext_hwi (vcall_offset, 16);
      HOST_WIDE_INT hi = vcall_offset - lo;
      rtx tmp;

      /* SCRATCH = *THIS_RTX.  */
      tmp = gen_rtx_MEM (Pmode, this_rtx);
      emit_move_insn (scratch, tmp);

      if (hi != 0)
	{
	  rtx scratch2 = gen_rtx_REG (Pmode, RV_REGNUM);
	  emit_move_insn (scratch2, GEN_INT (hi));
	  emit_insn (gen_add2_insn (scratch, scratch2));
	}

      /* SCRATCH = *(*THIS_RTX + VCALL_OFFSET).  */
      tmp = plus_constant (Pmode, scratch, lo);
      tmp = gen_rtx_MEM (Pmode, tmp);
      emit_move_insn (scratch, tmp);

      /* THIS_RTX += *(*THIS_RTX + VCALL_OFFSET).  */
      emit_insn (gen_add2_insn (this_rtx, scratch));
    }

  /* Generate a tail call to the target function.  */
  if (!TREE_USED (function))
    {
      assemble_external (function);
      TREE_USED (function) = 1;
    }
  funexp = XEXP (DECL_RTL (function), 0);

  /* The symbol will be a local alias and therefore always binds local.  */
  gcc_assert (SYMBOL_REF_LOCAL_P (funexp));

  funexp = gen_rtx_MEM (FUNCTION_MODE, funexp);
  insn = emit_call_insn (gen_sibcall (funexp, const0_rtx));
  SIBLING_CALL_P (insn) = 1;
  emit_barrier ();

  /* Run just enough of rest_of_compilation to get the insns emitted.
     There's not really enough bulk here to make other passes such as
     instruction scheduling worth while.  */
  insn = get_insns ();
  shorten_branches (insn);
  assemble_start_function (thunk_fndecl, fnname);
  final_start_function (insn, file, 1);
  final (insn, file, 1);
  final_end_function ();
  assemble_end_function (thunk_fndecl, fnname);

  reload_completed = 0;
  epilogue_completed = 0;
}

static unsigned
or1k_libm_function_max_error (unsigned cfn, machine_mode mode,
			      bool boundary_p)
{
#ifdef OPTION_GLIBC
  bool glibc_p = OPTION_GLIBC;
#else
  bool glibc_p = false;
#endif
  if (glibc_p)
    {
      switch (cfn)
	{
	CASE_CFN_SIN:
	CASE_CFN_SIN_FN:
	  if (!boundary_p && mode == DFmode && flag_rounding_math)
	    return 7;
	  break;
	default:
	  break;
	}
      return glibc_linux_libm_function_max_error (cfn, mode, boundary_p);
    }
  return default_libm_function_max_error (cfn, mode, boundary_p);
}

#undef  TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK or1k_output_mi_thunk
#undef  TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK \
  hook_bool_const_tree_hwi_hwi_const_tree_true

#undef  TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE or1k_option_override

#undef  TARGET_COMPUTE_FRAME_LAYOUT
#define TARGET_COMPUTE_FRAME_LAYOUT or1k_compute_frame_layout

#undef  TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P or1k_legitimate_address_p

#ifdef HAVE_AS_TLS
#undef  TARGET_HAVE_TLS
#define TARGET_HAVE_TLS true
#endif

#undef  TARGET_HAVE_SPECULATION_SAFE_VALUE
#define TARGET_HAVE_SPECULATION_SAFE_VALUE speculation_safe_value_not_needed

#undef  TARGET_LIBM_FUNCTION_MAX_ERROR
#define TARGET_LIBM_FUNCTION_MAX_ERROR or1k_libm_function_max_error

/* Calling Conventions.  */
#undef  TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE or1k_function_value
#undef  TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE or1k_libcall_value
#undef  TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P or1k_function_value_regno_p
#undef  TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG or1k_function_arg
#undef  TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE or1k_function_arg_advance
#undef  TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY or1k_return_in_memory
#undef  TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE or1k_pass_by_reference
#undef  TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT or1k_trampoline_init
#undef  TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED or1k_frame_pointer_required
#undef  TARGET_CUSTOM_FUNCTION_DESCRIPTORS
#define TARGET_CUSTOM_FUNCTION_DESCRIPTORS 1

/* Assembly generation.  */
#undef  TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND or1k_print_operand
#undef  TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS or1k_print_operand_address

/* Section anchor support.  */
#undef  TARGET_MIN_ANCHOR_OFFSET
#define TARGET_MIN_ANCHOR_OFFSET  -32768
#undef  TARGET_MAX_ANCHOR_OFFSET
#define TARGET_MAX_ANCHOR_OFFSET  32767

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-or1k.h"
