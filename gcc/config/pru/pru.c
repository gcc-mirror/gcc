/* Target machine subroutines for TI PRU.
   Copyright (C) 2014-2020 Free Software Foundation, Inc.
   Dimitar Dimitrov <dimitar@dinux.eu>

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
#include "memmodel.h"
#include "tm_p.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "explow.h"
#include "calls.h"
#include "varasm.h"
#include "expr.h"
#include "toplev.h"
#include "langhooks.h"
#include "cfgrtl.h"
#include "stor-layout.h"
#include "dumpfile.h"
#include "builtins.h"
#include "pru-protos.h"

/* This file should be included last.  */
#include "target-def.h"

#define INIT_ARRAY_ENTRY_BYTES	2

/* Global PRU CTABLE entries, filled in by pragmas, and used for fast
   addressing via LBCO/SBCO instructions.  */
struct pru_ctable_entry pru_ctable[32];

/* Forward function declarations.  */
static bool prologue_saved_reg_p (int);
static void pru_reorg_loop (rtx_insn *);

struct GTY (()) machine_function
{
  /* Current frame information, to be filled in by pru_compute_frame_layout
     with register save masks, and offsets for the current function.  */

  /* Mask of registers to save.  */
  HARD_REG_SET save_mask;
  /* Number of bytes that the entire frame takes up.  */
  int total_size;
  /* Number of bytes that variables take up.  */
  int var_size;
  /* Number of bytes that outgoing arguments take up.  */
  int out_args_size;
  /* Number of bytes needed to store registers in frame.  */
  int save_reg_size;
  /* Offset from new stack pointer to store registers.  */
  int save_regs_offset;
  /* True if final frame layout is already calculated.  */
  bool initialized;
  /* Number of doloop tags used so far.  */
  int doloop_tags;
  /* True if the last tag was allocated to a doloop_end.  */
  bool doloop_tag_from_end;
};

/* Stack layout and calling conventions.

   The PRU ABI defines r4 as Argument Pointer.  GCC implements the same
   semantics, but represents it with HARD_FRAME_POINTER_REGNUM and
   names it FP.  The stack layout is shown below:

       ---------------------- high address
	| incoming args
       ------call-boundary---
	| pretend_args	    ^
    FP ----------------     | total
	| save_regs	    | frame
	---------------	    | size
	| local vars	    |
	---------------	    |
	| outgoing args     V
    SP ---------------------- low address

 */

#define PRU_STACK_ALIGN(LOC)  ROUND_UP ((LOC), STACK_BOUNDARY / BITS_PER_UNIT)

/* Implement TARGET_COMPUTE_FRAME_LAYOUT.  */
static void
pru_compute_frame_layout (void)
{
  int regno;
  HARD_REG_SET *save_mask;
  int total_size;
  int var_size;
  int out_args_size;
  int save_reg_size;

  gcc_assert (!cfun->machine->initialized);

  save_mask = &cfun->machine->save_mask;
  CLEAR_HARD_REG_SET (*save_mask);

  var_size = PRU_STACK_ALIGN ((HOST_WIDE_INT) get_frame_size ());
  out_args_size = PRU_STACK_ALIGN ((HOST_WIDE_INT) crtl->outgoing_args_size);
  total_size = var_size + out_args_size;

  /* Calculate space needed for gp registers.  */
  save_reg_size = 0;
  for (regno = 0; regno <= LAST_GP_REGNUM; regno++)
    if (prologue_saved_reg_p (regno))
      {
	SET_HARD_REG_BIT (*save_mask, regno);
	save_reg_size += 1;
      }

  save_reg_size = PRU_STACK_ALIGN (save_reg_size);
  total_size += save_reg_size;
  total_size += PRU_STACK_ALIGN (crtl->args.pretend_args_size);

  /* Save other computed information.  */
  cfun->machine->total_size = total_size;
  cfun->machine->var_size = var_size;
  cfun->machine->out_args_size = out_args_size;
  cfun->machine->save_reg_size = save_reg_size;
  cfun->machine->initialized = reload_completed;
  cfun->machine->save_regs_offset = out_args_size + var_size;
}

/* Emit efficient RTL equivalent of ADD3 with the given const_int for
   frame-related registers.
     op0	  - Destination register.
     op1	  - First addendum operand (a register).
     addendum	  - Second addendum operand (a constant).
     kind	  - Note kind.  REG_NOTE_MAX if no note must be added.
 */
static rtx
pru_add3_frame_adjust (rtx op0, rtx op1, int addendum,
		       const enum reg_note kind)
{
  rtx insn;

  rtx op0_adjust = gen_rtx_SET (op0, plus_constant (Pmode, op1, addendum));

  if (UBYTE_INT (addendum) || UBYTE_INT (-addendum))
    insn = emit_insn (op0_adjust);
  else
    {
      /* Help the compiler to cope with an arbitrary integer constant.
	 Reload has finished so we can't expect the compiler to
	 auto-allocate a temporary register.  But we know that call-saved
	 registers are not live yet, so we utilize them.  */
      rtx tmpreg = gen_rtx_REG (Pmode, PROLOGUE_TEMP_REGNUM);
      if (addendum < 0)
	{
	  emit_insn (gen_rtx_SET (tmpreg, gen_int_mode (-addendum, Pmode)));
	  insn = emit_insn (gen_sub3_insn (op0, op1, tmpreg));
	}
      else
	{
	  emit_insn (gen_rtx_SET (tmpreg, gen_int_mode (addendum, Pmode)));
	  insn = emit_insn (gen_add3_insn (op0, op1, tmpreg));
	}
    }

  /* Attach a note indicating what happened.  */
  if (kind != REG_NOTE_MAX)
    add_reg_note (insn, kind, copy_rtx (op0_adjust));

  RTX_FRAME_RELATED_P (insn) = 1;

  return insn;
}

/* Add a const_int to the stack pointer register.  */
static rtx
pru_add_to_sp (int addendum, const enum reg_note kind)
{
  return pru_add3_frame_adjust (stack_pointer_rtx, stack_pointer_rtx,
				addendum, kind);
}

/* Helper function used during prologue/epilogue.  Emits a single LBBO/SBBO
   instruction for load/store of the next group of consecutive registers.  */
static int
xbbo_next_reg_cluster (int regno_start, int *sp_offset, bool do_store)
{
  int regno, nregs, i;
  rtx addr;
  rtx_insn *insn;

  nregs = 0;

  /* Skip the empty slots.  */
  for (; regno_start <= LAST_GP_REGNUM;)
    if (TEST_HARD_REG_BIT (cfun->machine->save_mask, regno_start))
      break;
    else
      regno_start++;

  /* Find the largest consecutive group of registers to save.  */
  for (regno = regno_start; regno <= LAST_GP_REGNUM;)
    if (TEST_HARD_REG_BIT (cfun->machine->save_mask, regno))
      {
	regno++;
	nregs++;
      }
    else
      break;

  if (!nregs)
    return -1;

  gcc_assert (UBYTE_INT (*sp_offset));

  /* Ok, save this bunch.  */
  addr = plus_constant (Pmode, stack_pointer_rtx, *sp_offset);

  if (do_store)
    insn = targetm.gen_store_multiple (gen_frame_mem (BLKmode, addr),
				       gen_rtx_REG (QImode, regno_start),
				       GEN_INT (nregs));
  else
    insn = targetm.gen_load_multiple (gen_rtx_REG (QImode, regno_start),
				      gen_frame_mem (BLKmode, addr),
				      GEN_INT (nregs));

  gcc_assert (reload_completed);
  gcc_assert (insn);
  emit_insn (insn);

  /* Tag as frame-related.  */
  RTX_FRAME_RELATED_P (insn) = 1;

  if (!do_store)
    {
      /* Tag epilogue unwind notes.  */
      for (i = regno_start; i < (regno_start + nregs); i++)
	add_reg_note (insn, REG_CFA_RESTORE, gen_rtx_REG (QImode, i));
    }

  /* Increment and save offset in anticipation of the next register group.  */
  *sp_offset += nregs * UNITS_PER_WORD;

  return regno_start + nregs;
}

/* Emit function prologue.  */
void
pru_expand_prologue (void)
{
  int regno_start;
  int total_frame_size;
  int sp_offset;      /* Offset from base_reg to final stack value.  */
  int save_regs_base; /* Offset from base_reg to register save area.  */
  int save_offset;    /* Temporary offset to currently saved register group.  */

  total_frame_size = cfun->machine->total_size;

  if (flag_stack_usage_info)
    current_function_static_stack_size = total_frame_size;

  /* Decrement the stack pointer.  */
  if (!UBYTE_INT (total_frame_size))
    {
      /* We need an intermediary point, this will point at the spill block.  */
      pru_add_to_sp (cfun->machine->save_regs_offset - total_frame_size,
		     REG_NOTE_MAX);
      save_regs_base = 0;
      sp_offset = -cfun->machine->save_regs_offset;
    }
  else if (total_frame_size)
    {
      pru_add_to_sp (- total_frame_size, REG_NOTE_MAX);
      save_regs_base = cfun->machine->save_regs_offset;
      sp_offset = 0;
    }
  else
    save_regs_base = sp_offset = 0;

  regno_start = 0;
  save_offset = save_regs_base;
  do
    regno_start = xbbo_next_reg_cluster (regno_start, &save_offset, true);
  while (regno_start >= 0);

  /* Set FP before adjusting SP.  This way fp_offset has
     better chance to fit in UBYTE.  */
  if (frame_pointer_needed)
    {
      int fp_offset = total_frame_size
	- crtl->args.pretend_args_size
	+ sp_offset;

      pru_add3_frame_adjust (hard_frame_pointer_rtx, stack_pointer_rtx,
			     fp_offset, REG_NOTE_MAX);
    }

  if (sp_offset)
    pru_add_to_sp (sp_offset, REG_FRAME_RELATED_EXPR);

  /* If we are profiling, make sure no instructions are scheduled before
     the call to mcount.  */
  if (crtl->profile)
    emit_insn (gen_blockage ());
}

/* Emit function epilogue.  */
void
pru_expand_epilogue (bool sibcall_p)
{
  int total_frame_size;
  int sp_adjust, save_offset;
  int regno_start;

  if (!sibcall_p && pru_can_use_return_insn ())
    {
      emit_jump_insn (gen_return ());
      return;
    }

  emit_insn (gen_blockage ());

  total_frame_size = cfun->machine->total_size;

  if (frame_pointer_needed)
    {
      /* Recover the stack pointer.  */
      pru_add3_frame_adjust (stack_pointer_rtx, hard_frame_pointer_rtx,
			     - cfun->machine->save_reg_size,
			     REG_CFA_ADJUST_CFA);

      save_offset = 0;
      sp_adjust = total_frame_size - cfun->machine->save_regs_offset;
    }
  else if (!UBYTE_INT (total_frame_size))
    {
      pru_add_to_sp (cfun->machine->save_regs_offset, REG_CFA_ADJUST_CFA);
      save_offset = 0;
      sp_adjust = total_frame_size - cfun->machine->save_regs_offset;
    }
  else
    {
      save_offset = cfun->machine->save_regs_offset;
      sp_adjust = total_frame_size;
    }

  regno_start = 0;
  do
    regno_start = xbbo_next_reg_cluster (regno_start, &save_offset, false);
  while (regno_start >= 0);

  /* Emit a blockage insn here to keep these insns from being moved to
     an earlier spot in the epilogue.

     This is necessary as we must not cut the stack back before all the
     restores are finished.  */
  emit_insn (gen_blockage ());

  if (sp_adjust)
    pru_add_to_sp (sp_adjust, REG_CFA_ADJUST_CFA);

  if (!sibcall_p)
    emit_jump_insn (gen_simple_return ());
}

/* Implement RETURN_ADDR_RTX.  Note, we do not support moving
   back to a previous frame.  */
rtx
pru_get_return_address (int count)
{
  if (count != 0)
    return NULL_RTX;

  /* Return r3.w2.  */
  return get_hard_reg_initial_val (HImode, RA_REGNUM);
}

/* Implement FUNCTION_PROFILER macro.  */
void
pru_function_profiler (FILE *file, int labelno ATTRIBUTE_UNUSED)
{
  fprintf (file, "\tmov\tr1, ra\n");
  fprintf (file, "\tcall\t_mcount\n");
  fprintf (file, "\tmov\tra, r1\n");
}

/* Dump stack layout.  */
static void
pru_dump_frame_layout (FILE *file)
{
  fprintf (file, "\t%s Current Frame Info\n", ASM_COMMENT_START);
  fprintf (file, "\t%s total_size = %d\n", ASM_COMMENT_START,
	   cfun->machine->total_size);
  fprintf (file, "\t%s var_size = %d\n", ASM_COMMENT_START,
	   cfun->machine->var_size);
  fprintf (file, "\t%s out_args_size = %d\n", ASM_COMMENT_START,
	   cfun->machine->out_args_size);
  fprintf (file, "\t%s save_reg_size = %d\n", ASM_COMMENT_START,
	   cfun->machine->save_reg_size);
  fprintf (file, "\t%s initialized = %d\n", ASM_COMMENT_START,
	   cfun->machine->initialized);
  fprintf (file, "\t%s save_regs_offset = %d\n", ASM_COMMENT_START,
	   cfun->machine->save_regs_offset);
  fprintf (file, "\t%s is_leaf = %d\n", ASM_COMMENT_START,
	   crtl->is_leaf);
  fprintf (file, "\t%s frame_pointer_needed = %d\n", ASM_COMMENT_START,
	   frame_pointer_needed);
  fprintf (file, "\t%s pretend_args_size = %d\n", ASM_COMMENT_START,
	   crtl->args.pretend_args_size);
}

/* Return true if REGNO should be saved in the prologue.  */
static bool
prologue_saved_reg_p (int regno)
{
  gcc_assert (GP_REG_P (regno));

  if (df_regs_ever_live_p (regno) && !call_used_or_fixed_reg_p (regno))
    return true;

  /* 32-bit FP.  */
  if (frame_pointer_needed
      && regno >= HARD_FRAME_POINTER_REGNUM
      && regno < HARD_FRAME_POINTER_REGNUM + GET_MODE_SIZE (Pmode))
    return true;

  /* 16-bit RA.  */
  if (regno == RA_REGNUM && df_regs_ever_live_p (RA_REGNUM))
    return true;
  if (regno == RA_REGNUM + 1 && df_regs_ever_live_p (RA_REGNUM + 1))
    return true;

  return false;
}

/* Implement TARGET_CAN_ELIMINATE.  */
static bool
pru_can_eliminate (const int from ATTRIBUTE_UNUSED, const int to)
{
  if (to == STACK_POINTER_REGNUM)
    return !frame_pointer_needed;
  return true;
}

/* Implement INITIAL_ELIMINATION_OFFSET macro.  */
int
pru_initial_elimination_offset (int from, int to)
{
  int offset;

  /* Set OFFSET to the offset from the stack pointer.  */
  switch (from)
    {
    case FRAME_POINTER_REGNUM:
      offset = cfun->machine->out_args_size;
      break;

    case ARG_POINTER_REGNUM:
      offset = cfun->machine->total_size;
      offset -= crtl->args.pretend_args_size;
      break;

    default:
      gcc_unreachable ();
    }

  /* If we are asked for the frame pointer offset, then adjust OFFSET
     by the offset from the frame pointer to the stack pointer.  */
  if (to == HARD_FRAME_POINTER_REGNUM)
    offset -= cfun->machine->total_size - crtl->args.pretend_args_size;


  return offset;
}

/* Return nonzero if this function is known to have a null epilogue.
   This allows the optimizer to omit jumps to jumps if no stack
   was created.  */
int
pru_can_use_return_insn (void)
{
  if (!reload_completed || crtl->profile)
    return 0;

  return cfun->machine->total_size == 0;
}

/* Implement TARGET_HARD_REGNO_MODE_OK.  */

static bool
pru_hard_regno_mode_ok (unsigned int regno, machine_mode mode)
{
  switch (GET_MODE_SIZE (mode))
    {
    case 1: return true;
    case 2: return (regno % 4) <= 2;
    case 4: return (regno % 4) == 0;
    case 8: return (regno % 4) == 0;
    case 16: return (regno % 4) == 0; /* Not sure why TImode is used.  */
    case 32: return (regno % 4) == 0; /* Not sure why CTImode is used.  */
    default:
      /* TODO: Find out why VOIDmode and BLKmode are passed.  */
      gcc_assert (mode == BLKmode || mode == VOIDmode);
      return (regno % 4) == 0;
    }
}

/* Implement `TARGET_HARD_REGNO_SCRATCH_OK'.
   Returns true if REGNO is safe to be allocated as a scratch
   register (for a define_peephole2) in the current function.  */

static bool
pru_hard_regno_scratch_ok (unsigned int regno)
{
  /* Don't allow hard registers that might be part of the frame pointer.
     Some places in the compiler just test for [HARD_]FRAME_POINTER_REGNUM
     and don't handle a frame pointer that spans more than one register.
     TODO: Fix those faulty places.  */

  if ((!reload_completed || frame_pointer_needed)
      && (IN_RANGE (regno, HARD_FRAME_POINTER_REGNUM,
		    HARD_FRAME_POINTER_REGNUM + 3)
	  || IN_RANGE (regno, FRAME_POINTER_REGNUM,
		       FRAME_POINTER_REGNUM + 3)))
    return false;

  return true;
}


/* Worker function for `HARD_REGNO_RENAME_OK'.
   Return nonzero if register OLD_REG can be renamed to register NEW_REG.  */

int
pru_hard_regno_rename_ok (unsigned int old_reg,
			  unsigned int new_reg)
{
  /* Don't allow hard registers that might be part of the frame pointer.
     Some places in the compiler just test for [HARD_]FRAME_POINTER_REGNUM
     and don't care for a frame pointer that spans more than one register.
     TODO: Fix those faulty places.  */
  if ((!reload_completed || frame_pointer_needed)
      && (IN_RANGE (old_reg, HARD_FRAME_POINTER_REGNUM,
		    HARD_FRAME_POINTER_REGNUM + 3)
	  || IN_RANGE (old_reg, FRAME_POINTER_REGNUM,
		       FRAME_POINTER_REGNUM + 3)
	  || IN_RANGE (new_reg, HARD_FRAME_POINTER_REGNUM,
		       HARD_FRAME_POINTER_REGNUM + 3)
	  || IN_RANGE (new_reg, FRAME_POINTER_REGNUM,
		       FRAME_POINTER_REGNUM + 3)))
    return 0;

  return 1;
}

/* Allocate a chunk of memory for per-function machine-dependent data.  */
static struct machine_function *
pru_init_machine_status (void)
{
  return ggc_cleared_alloc<machine_function> ();
}

/* Implement TARGET_OPTION_OVERRIDE.  */
static void
pru_option_override (void)
{
#ifdef SUBTARGET_OVERRIDE_OPTIONS
  SUBTARGET_OVERRIDE_OPTIONS;
#endif

  /* Check for unsupported options.  */
  if (flag_pic == 1)
    warning (OPT_fpic, "%<-fpic%> is not supported");
  if (flag_pic == 2)
    warning (OPT_fPIC, "%<-fPIC%> is not supported");
  if (flag_pie == 1)
    warning (OPT_fpie, "%<-fpie%> is not supported");
  if (flag_pie == 2)
    warning (OPT_fPIE, "%<-fPIE%> is not supported");

  /* QBxx conditional branching cannot cope with block reordering.  */
  if (flag_reorder_blocks_and_partition)
    {
      inform (input_location, "%<-freorder-blocks-and-partition%> "
			      "not supported on this architecture");
      flag_reorder_blocks_and_partition = 0;
      flag_reorder_blocks = 1;
    }

  /* Function to allocate machine-dependent function status.  */
  init_machine_status = &pru_init_machine_status;

  /* Save the initial options in case the user does function specific
     options.  */
  target_option_default_node = target_option_current_node
    = build_target_option_node (&global_options);

  /* Due to difficulties in implementing the TI ABI with GCC,
     at least check and error-out if GCC cannot compile a
     compliant output.  */
  pru_register_abicheck_pass ();
}

/* Compute a (partial) cost for rtx X.  Return true if the complete
   cost has been computed, and false if subexpressions should be
   scanned.  In either case, *TOTAL contains the cost result.  */
static bool
pru_rtx_costs (rtx x, machine_mode mode,
	       int outer_code, int opno ATTRIBUTE_UNUSED,
	       int *total, bool speed ATTRIBUTE_UNUSED)
{
  const int code = GET_CODE (x);

  switch (code)
    {
    case CONST_INT:
      if ((mode == VOIDmode && UBYTE_INT (INTVAL (x)))
	  || (mode != VOIDmode && const_ubyte_operand (x, mode)))
	{
	  *total = COSTS_N_INSNS (0);
	  return true;
	}
      else if ((mode == VOIDmode && UHWORD_INT (INTVAL (x)))
	       || (mode != VOIDmode && const_uhword_operand (x, mode)))
	{
	  *total = COSTS_N_INSNS (1);
	  return true;
	}
      else if (outer_code == MEM && ctable_addr_operand (x, VOIDmode))
	{
	  *total = COSTS_N_INSNS (0);
	  return true;
	}
      else
	{
	  *total = COSTS_N_INSNS (2);
	  return true;
	}

    case LABEL_REF:
    case SYMBOL_REF:
    case CONST:
      {
	*total = COSTS_N_INSNS (1);
	return true;
      }
    case CONST_DOUBLE:
      {
	*total = COSTS_N_INSNS (2);
	return true;
      }
    case CONST_WIDE_INT:
      {
	/* PRU declares no vector or very large integer types.  */
	gcc_unreachable ();
	return true;
      }
    case SET:
      {
	int factor;

	/* A SET doesn't have a mode, so let's look at the SET_DEST to get
	   the mode for the factor.  */
	mode = GET_MODE (SET_DEST (x));

	/* SI move has the same cost as a QI move.  Moves larger than
	   64 bits are costly.  */
	factor = CEIL (GET_MODE_SIZE (mode), GET_MODE_SIZE (SImode));
	*total = factor * COSTS_N_INSNS (1);

	return false;
      }

    case MULT:
      {
	/* Factor in that "mul" requires fixed registers, which
	   would likely require register moves.  */
	*total = COSTS_N_INSNS (7);
	return false;
      }
    case PLUS:
      {
	rtx op0 = XEXP (x, 0);
	rtx op1 = XEXP (x, 1);
	machine_mode op1_mode = GET_MODE (op1);

	/* Generic RTL address expressions do not enforce mode for
	   offsets, yet our UBYTE constraint requires it.  Fix it here.  */
	if (op1_mode == VOIDmode && CONST_INT_P (op1) && outer_code == MEM)
	  op1_mode = Pmode;
	if (outer_code == MEM
	    && ((REG_P (op0) && reg_or_ubyte_operand (op1, op1_mode))
		|| ctable_addr_operand (op0, VOIDmode)
		|| ctable_addr_operand (op1, VOIDmode)
		|| (ctable_base_operand (op0, VOIDmode) && REG_P (op1))
		|| (ctable_base_operand (op1, VOIDmode) && REG_P (op0))))
	  {
	    /* CTABLE or REG base addressing - PLUS comes for free.  */
	    *total = COSTS_N_INSNS (0);
	    return true;
	  }
	else
	  {
	    *total = COSTS_N_INSNS (1);
	    return false;
	  }
	}
    case SIGN_EXTEND:
      {
	*total = COSTS_N_INSNS (3);
	return false;
      }
    case ASHIFTRT:
      {
	rtx op1 = XEXP (x, 1);
	if (const_1_operand (op1, VOIDmode))
	  *total = COSTS_N_INSNS (3);
	else
	  *total = COSTS_N_INSNS (7);
	return false;
      }
    case ZERO_EXTRACT:
      {
	rtx op2 = XEXP (x, 2);
	if ((outer_code == EQ || outer_code == NE)
	    && CONST_INT_P (op2)
	    && INTVAL (op2) == 1)
	  {
	    /* Branch if bit is set/clear is a single instruction.  */
	    *total = COSTS_N_INSNS (0);
	    return true;
	  }
	else
	  {
	    *total = COSTS_N_INSNS (2);
	    return false;
	  }
      }
    case ZERO_EXTEND:
      {
	*total = COSTS_N_INSNS (0);
	return false;
      }

    default:
      {
	/* PRU ALU is 32 bit, despite GCC's UNITS_PER_WORD=1.  */
	int factor = CEIL (GET_MODE_SIZE (mode), GET_MODE_SIZE (SImode));
	*total = factor * COSTS_N_INSNS (1);
	return false;
      }
    }
}

static GTY(()) rtx eqdf_libfunc;
static GTY(()) rtx nedf_libfunc;
static GTY(()) rtx ledf_libfunc;
static GTY(()) rtx ltdf_libfunc;
static GTY(()) rtx gedf_libfunc;
static GTY(()) rtx gtdf_libfunc;
static GTY(()) rtx eqsf_libfunc;
static GTY(()) rtx nesf_libfunc;
static GTY(()) rtx lesf_libfunc;
static GTY(()) rtx ltsf_libfunc;
static GTY(()) rtx gesf_libfunc;
static GTY(()) rtx gtsf_libfunc;

/* Implement the TARGET_INIT_LIBFUNCS macro.  We use this to rename library
   functions to match the PRU ABI.  */

static void
pru_init_libfuncs (void)
{
  /* Double-precision floating-point arithmetic.  */
  set_optab_libfunc (add_optab, DFmode, "__pruabi_addd");
  set_optab_libfunc (sdiv_optab, DFmode, "__pruabi_divd");
  set_optab_libfunc (smul_optab, DFmode, "__pruabi_mpyd");
  set_optab_libfunc (neg_optab, DFmode, "__pruabi_negd");
  set_optab_libfunc (sub_optab, DFmode, "__pruabi_subd");

  /* Single-precision floating-point arithmetic.  */
  set_optab_libfunc (add_optab, SFmode, "__pruabi_addf");
  set_optab_libfunc (sdiv_optab, SFmode, "__pruabi_divf");
  set_optab_libfunc (smul_optab, SFmode, "__pruabi_mpyf");
  set_optab_libfunc (neg_optab, SFmode, "__pruabi_negf");
  set_optab_libfunc (sub_optab, SFmode, "__pruabi_subf");

  /* Floating-point comparisons.  */
  eqsf_libfunc = init_one_libfunc ("__pruabi_eqf");
  nesf_libfunc = init_one_libfunc ("__pruabi_neqf");
  lesf_libfunc = init_one_libfunc ("__pruabi_lef");
  ltsf_libfunc = init_one_libfunc ("__pruabi_ltf");
  gesf_libfunc = init_one_libfunc ("__pruabi_gef");
  gtsf_libfunc = init_one_libfunc ("__pruabi_gtf");
  eqdf_libfunc = init_one_libfunc ("__pruabi_eqd");
  nedf_libfunc = init_one_libfunc ("__pruabi_neqd");
  ledf_libfunc = init_one_libfunc ("__pruabi_led");
  ltdf_libfunc = init_one_libfunc ("__pruabi_ltd");
  gedf_libfunc = init_one_libfunc ("__pruabi_ged");
  gtdf_libfunc = init_one_libfunc ("__pruabi_gtd");

  /* In PRU ABI, much like other TI processors, floating point
     comparisons return non-standard values.  This quirk is handled
     by disabling the optab library functions, and handling the
     comparison during RTL expansion.  */
  set_optab_libfunc (eq_optab, SFmode, NULL);
  set_optab_libfunc (ne_optab, SFmode, NULL);
  set_optab_libfunc (gt_optab, SFmode, NULL);
  set_optab_libfunc (ge_optab, SFmode, NULL);
  set_optab_libfunc (lt_optab, SFmode, NULL);
  set_optab_libfunc (le_optab, SFmode, NULL);
  set_optab_libfunc (eq_optab, DFmode, NULL);
  set_optab_libfunc (ne_optab, DFmode, NULL);
  set_optab_libfunc (gt_optab, DFmode, NULL);
  set_optab_libfunc (ge_optab, DFmode, NULL);
  set_optab_libfunc (lt_optab, DFmode, NULL);
  set_optab_libfunc (le_optab, DFmode, NULL);

  /* The isunordered function appears to be supported only by GCC.  */
  set_optab_libfunc (unord_optab, SFmode, "__pruabi_unordf");
  set_optab_libfunc (unord_optab, DFmode, "__pruabi_unordd");

  /* Floating-point to integer conversions.  */
  set_conv_libfunc (sfix_optab, SImode, DFmode, "__pruabi_fixdi");
  set_conv_libfunc (ufix_optab, SImode, DFmode, "__pruabi_fixdu");
  set_conv_libfunc (sfix_optab, DImode, DFmode, "__pruabi_fixdlli");
  set_conv_libfunc (ufix_optab, DImode, DFmode, "__pruabi_fixdull");
  set_conv_libfunc (sfix_optab, SImode, SFmode, "__pruabi_fixfi");
  set_conv_libfunc (ufix_optab, SImode, SFmode, "__pruabi_fixfu");
  set_conv_libfunc (sfix_optab, DImode, SFmode, "__pruabi_fixflli");
  set_conv_libfunc (ufix_optab, DImode, SFmode, "__pruabi_fixfull");

  /* Conversions between floating types.  */
  set_conv_libfunc (trunc_optab, SFmode, DFmode, "__pruabi_cvtdf");
  set_conv_libfunc (sext_optab, DFmode, SFmode, "__pruabi_cvtfd");

  /* Integer to floating-point conversions.  */
  set_conv_libfunc (sfloat_optab, DFmode, SImode, "__pruabi_fltid");
  set_conv_libfunc (ufloat_optab, DFmode, SImode, "__pruabi_fltud");
  set_conv_libfunc (sfloat_optab, DFmode, DImode, "__pruabi_fltllid");
  set_conv_libfunc (ufloat_optab, DFmode, DImode, "__pruabi_fltulld");
  set_conv_libfunc (sfloat_optab, SFmode, SImode, "__pruabi_fltif");
  set_conv_libfunc (ufloat_optab, SFmode, SImode, "__pruabi_fltuf");
  set_conv_libfunc (sfloat_optab, SFmode, DImode, "__pruabi_fltllif");
  set_conv_libfunc (ufloat_optab, SFmode, DImode, "__pruabi_fltullf");

  /* Long long.  */
  set_optab_libfunc (ashr_optab, DImode, "__pruabi_asrll");
  set_optab_libfunc (smul_optab, DImode, "__pruabi_mpyll");
  set_optab_libfunc (ashl_optab, DImode, "__pruabi_lslll");
  set_optab_libfunc (lshr_optab, DImode, "__pruabi_lsrll");

  set_optab_libfunc (sdiv_optab, SImode, "__pruabi_divi");
  set_optab_libfunc (udiv_optab, SImode, "__pruabi_divu");
  set_optab_libfunc (smod_optab, SImode, "__pruabi_remi");
  set_optab_libfunc (umod_optab, SImode, "__pruabi_remu");
  set_optab_libfunc (sdivmod_optab, SImode, "__pruabi_divremi");
  set_optab_libfunc (udivmod_optab, SImode, "__pruabi_divremu");
  set_optab_libfunc (sdiv_optab, DImode, "__pruabi_divlli");
  set_optab_libfunc (udiv_optab, DImode, "__pruabi_divull");
  set_optab_libfunc (smod_optab, DImode, "__pruabi_remlli");
  set_optab_libfunc (umod_optab, DImode, "__pruabi_remull");
  set_optab_libfunc (udivmod_optab, DImode, "__pruabi_divremull");
}


/* Emit comparison instruction if necessary, returning the expression
   that holds the compare result in the proper mode.  Return the comparison
   that should be used in the jump insn.  */

rtx
pru_expand_fp_compare (rtx comparison, machine_mode mode)
{
  enum rtx_code code = GET_CODE (comparison);
  rtx op0 = XEXP (comparison, 0);
  rtx op1 = XEXP (comparison, 1);
  rtx cmp;
  enum rtx_code jump_code = code;
  machine_mode op_mode = GET_MODE (op0);
  rtx_insn *insns;
  rtx libfunc;

  gcc_assert (op_mode == DFmode || op_mode == SFmode);

  /* FP exceptions are not raised by PRU's softfp implementation.  So the
     following transformations are safe.  */
  if (code == UNGE)
    {
      code = LT;
      jump_code = EQ;
    }
  else if (code == UNLE)
    {
      code = GT;
      jump_code = EQ;
    }
  else
    jump_code = NE;

  switch (code)
    {
    case EQ:
      libfunc = op_mode == DFmode ? eqdf_libfunc : eqsf_libfunc;
      break;
    case NE:
      libfunc = op_mode == DFmode ? nedf_libfunc : nesf_libfunc;
      break;
    case GT:
      libfunc = op_mode == DFmode ? gtdf_libfunc : gtsf_libfunc;
      break;
    case GE:
      libfunc = op_mode == DFmode ? gedf_libfunc : gesf_libfunc;
      break;
    case LT:
      libfunc = op_mode == DFmode ? ltdf_libfunc : ltsf_libfunc;
      break;
    case LE:
      libfunc = op_mode == DFmode ? ledf_libfunc : lesf_libfunc;
      break;
    default:
      gcc_unreachable ();
    }
  start_sequence ();

  cmp = emit_library_call_value (libfunc, 0, LCT_CONST, SImode,
				 op0, op_mode, op1, op_mode);
  insns = get_insns ();
  end_sequence ();

  emit_libcall_block (insns, cmp, cmp,
		      gen_rtx_fmt_ee (code, SImode, op0, op1));

  return gen_rtx_fmt_ee (jump_code, mode, cmp, const0_rtx);
}

/* Return the sign bit position for given OP's mode.  */
static int
sign_bit_position (const rtx op)
{
  const int sz = GET_MODE_SIZE (GET_MODE (op));

  return sz * 8 - 1;
}

/* Output asm code for sign_extend operation.  */
const char *
pru_output_sign_extend (rtx *operands)
{
  static char buf[512];
  int bufi;
  const int dst_sz = GET_MODE_SIZE (GET_MODE (operands[0]));
  const int src_sz = GET_MODE_SIZE (GET_MODE (operands[1]));
  char ext_start;

  switch (src_sz)
    {
    case 1: ext_start = 'y'; break;
    case 2: ext_start = 'z'; break;
    default: gcc_unreachable ();
    }

  gcc_assert (dst_sz > src_sz);

  /* Note that src and dst can be different parts of the same
     register, e.g. "r7, r7.w1".  */
  bufi = snprintf (buf, sizeof (buf),
	  "mov\t%%0, %%1\n\t"		      /* Copy AND make positive.  */
	  "qbbc\t.+8, %%0, %d\n\t"	      /* Check sign bit.  */
	  "fill\t%%%c0, %d",		      /* Make negative.  */
	  sign_bit_position (operands[1]),
	  ext_start,
	  dst_sz - src_sz);

  gcc_assert (bufi > 0);
  gcc_assert ((unsigned int) bufi < sizeof (buf));

  return buf;
}

/* Branches and compares.  */

/* PRU's ALU does not support signed comparison operations.  That's why we
   emulate them.  By first checking the sign bit and handling every possible
   operand sign combination, we can simulate signed comparisons in just
   5 instructions.  See table below.

.-------------------.---------------------------------------------------.
| Operand sign bit  | Mapping the signed comparison to an unsigned one  |
|---------+---------+------------+------------+------------+------------|
| OP1.b31 | OP2.b31 | OP1 < OP2  | OP1 <= OP2 | OP1 > OP2  | OP1 >= OP2 |
|---------+---------+------------+------------+------------+------------|
| 0       | 0       | OP1 < OP2  | OP1 <= OP2 | OP1 > OP2  | OP1 >= OP2 |
|---------+---------+------------+------------+------------+------------|
| 0       | 1       | false      | false      | true       | true       |
|---------+---------+------------+------------+------------+------------|
| 1       | 0       | true       | true       | false      | false      |
|---------+---------+------------+------------+------------+------------|
| 1       | 1       | OP1 < OP2  | OP1 <= OP2 | OP1 > OP2  | OP1 >= OP2 |
`---------'---------'------------'------------'------------+------------'


Given the table above, here is an example for a concrete op:
  LT:
		    qbbc OP1_POS, OP1, 31
  OP1_NEG:	    qbbc BRANCH_TAKEN_LABEL, OP2, 31
  OP1_NEG_OP2_NEG:  qblt BRANCH_TAKEN_LABEL, OP2, OP1
		    ; jmp OUT -> can be eliminated because we'll take the
		    ; following branch.  OP2.b31 is guaranteed to be 1
		    ; by the time we get here.
  OP1_POS:	    qbbs OUT, OP2, 31
  OP1_POS_OP2_POS:  qblt BRANCH_TAKEN_LABEL, OP2, OP1
#if FAR_JUMP
		    jmp OUT
BRANCH_TAKEN_LABEL: jmp REAL_BRANCH_TAKEN_LABEL
#endif
  OUT:

*/

/* Output asm code for a signed-compare LT/LE conditional branch.  */
static const char *
pru_output_ltle_signed_cbranch (rtx *operands, bool is_near)
{
  static char buf[1024];
  enum rtx_code code = GET_CODE (operands[0]);
  rtx op1;
  rtx op2;
  const char *cmp_opstr;
  int bufi = 0;

  op1 = operands[1];
  op2 = operands[2];

  gcc_assert (GET_CODE (op1) == REG && GET_CODE (op2) == REG);

  /* Determine the comparison operators for positive and negative operands.  */
  if (code == LT)
    cmp_opstr = "qblt";
  else if (code == LE)
    cmp_opstr = "qble";
  else
    gcc_unreachable ();

  if (is_near)
    bufi = snprintf (buf, sizeof (buf),
		     "qbbc\t.+12, %%1, %d\n\t"
		     "qbbc\t%%l3, %%2, %d\n\t"  /* OP1_NEG.  */
		     "%s\t%%l3, %%2, %%1\n\t"   /* OP1_NEG_OP2_NEG.  */
		     "qbbs\t.+8, %%2, %d\n\t"   /* OP1_POS.  */
		     "%s\t%%l3, %%2, %%1",	/* OP1_POS_OP2_POS.  */
		     sign_bit_position (op1),
		     sign_bit_position (op2),
		     cmp_opstr,
		     sign_bit_position (op2),
		     cmp_opstr);
  else
    bufi = snprintf (buf, sizeof (buf),
		     "qbbc\t.+12, %%1, %d\n\t"
		     "qbbc\t.+20, %%2, %d\n\t"  /* OP1_NEG.  */
		     "%s\t.+16, %%2, %%1\n\t"   /* OP1_NEG_OP2_NEG.  */
		     "qbbs\t.+16, %%2, %d\n\t"  /* OP1_POS.  */
		     "%s\t.+8, %%2, %%1\n\t"    /* OP1_POS_OP2_POS.  */
		     "jmp\t.+8\n\t"		/* jmp OUT.  */
		     "jmp\t%%%%label(%%l3)",	/* BRANCH_TAKEN_LABEL.  */
		     sign_bit_position (op1),
		     sign_bit_position (op2),
		     cmp_opstr,
		     sign_bit_position (op2),
		     cmp_opstr);

  gcc_assert (bufi > 0);
  gcc_assert ((unsigned int) bufi < sizeof (buf));

  return buf;
}

/* Output asm code for a signed-compare GT/GE conditional branch.  */
static const char *
pru_output_gtge_signed_cbranch (rtx *operands, bool is_near)
{
  static char buf[1024];
  enum rtx_code code = GET_CODE (operands[0]);
  rtx op1;
  rtx op2;
  const char *cmp_opstr;
  int bufi = 0;

  op1 = operands[1];
  op2 = operands[2];

  gcc_assert (GET_CODE (op1) == REG && GET_CODE (op2) == REG);

  /* Determine the comparison operators for positive and negative operands.  */
  if (code == GT)
    cmp_opstr = "qbgt";
  else if (code == GE)
    cmp_opstr = "qbge";
  else
    gcc_unreachable ();

  if (is_near)
    bufi = snprintf (buf, sizeof (buf),
		     "qbbs\t.+12, %%1, %d\n\t"
		     "qbbs\t%%l3, %%2, %d\n\t"  /* OP1_POS.  */
		     "%s\t%%l3, %%2, %%1\n\t"   /* OP1_POS_OP2_POS.  */
		     "qbbc\t.+8, %%2, %d\n\t"   /* OP1_NEG.  */
		     "%s\t%%l3, %%2, %%1",      /* OP1_NEG_OP2_NEG.  */
		     sign_bit_position (op1),
		     sign_bit_position (op2),
		     cmp_opstr,
		     sign_bit_position (op2),
		     cmp_opstr);
  else
    bufi = snprintf (buf, sizeof (buf),
		     "qbbs\t.+12, %%1, %d\n\t"
		     "qbbs\t.+20, %%2, %d\n\t"  /* OP1_POS.  */
		     "%s\t.+16, %%2, %%1\n\t"   /* OP1_POS_OP2_POS.  */
		     "qbbc\t.+16, %%2, %d\n\t"  /* OP1_NEG.  */
		     "%s\t.+8, %%2, %%1\n\t"    /* OP1_NEG_OP2_NEG.  */
		     "jmp\t.+8\n\t"		/* jmp OUT.  */
		     "jmp\t%%%%label(%%l3)",	/* BRANCH_TAKEN_LABEL.  */
		     sign_bit_position (op1),
		     sign_bit_position (op2),
		     cmp_opstr,
		     sign_bit_position (op2),
		     cmp_opstr);

  gcc_assert (bufi > 0);
  gcc_assert ((unsigned int) bufi < sizeof (buf));

  return buf;
}

/* Output asm code for a signed-compare conditional branch.

   If IS_NEAR is true, then QBBx instructions may be used for reaching
   the destination label.  Otherwise JMP is used, at the expense of
   increased code size.  */
const char *
pru_output_signed_cbranch (rtx *operands, bool is_near)
{
  enum rtx_code code = GET_CODE (operands[0]);

  if (code == LT || code == LE)
    return pru_output_ltle_signed_cbranch (operands, is_near);
  else if (code == GT || code == GE)
    return pru_output_gtge_signed_cbranch (operands, is_near);
  else
    gcc_unreachable ();
}

/* Optimized version of pru_output_signed_cbranch for constant second
   operand.  */

const char *
pru_output_signed_cbranch_ubyteop2 (rtx *operands, bool is_near)
{
  static char buf[1024];
  enum rtx_code code = GET_CODE (operands[0]);
  int regop_sign_bit_pos = sign_bit_position (operands[1]);
  const char *cmp_opstr;
  const char *rcmp_opstr;

  /* We must swap operands due to PRU's demand OP1 to be the immediate.  */
  code = swap_condition (code);

  /* Determine normal and reversed comparison operators for both positive
     operands.  This enables us to go completely unsigned.

     NOTE: We cannot use the R print modifier because we convert signed
     comparison operators to unsigned ones.  */
  switch (code)
    {
    case LT: cmp_opstr = "qblt"; rcmp_opstr = "qbge"; break;
    case LE: cmp_opstr = "qble"; rcmp_opstr = "qbgt"; break;
    case GT: cmp_opstr = "qbgt"; rcmp_opstr = "qble"; break;
    case GE: cmp_opstr = "qbge"; rcmp_opstr = "qblt"; break;
    default: gcc_unreachable ();
    }

  /* OP2 is a constant unsigned byte - utilize this info to generate
     optimized code.  We can "remove half" of the op table above because
     we know that OP2.b31 = 0 (remember that 0 <= OP2 <= 255).  */
  if (code == LT || code == LE)
    {
      if (is_near)
	snprintf (buf, sizeof (buf),
		  "qbbs\t.+8, %%1, %d\n\t"
		  "%s\t%%l3, %%1, %%u2",
		  regop_sign_bit_pos,
		  cmp_opstr);
      else
	snprintf (buf, sizeof (buf),
		  "qbbs\t.+12, %%1, %d\n\t"
		  "%s\t.+8, %%1, %%u2\n\t"
		  "jmp\t%%%%label(%%l3)",
		  regop_sign_bit_pos,
		  rcmp_opstr);
    }
  else if (code == GT || code == GE)
    {
      if (is_near)
	snprintf (buf, sizeof (buf),
		  "qbbs\t%%l3, %%1, %d\n\t"
		  "%s\t%%l3, %%1, %%u2",
		  regop_sign_bit_pos,
		  cmp_opstr);
      else
	snprintf (buf, sizeof (buf),
		  "qbbs\t.+8, %%1, %d\n\t"
		  "%s\t.+8, %%1, %%u2\n\t"
		  "jmp\t%%%%label(%%l3)",
		  regop_sign_bit_pos,
		  rcmp_opstr);
    }
  else
    gcc_unreachable ();

  return buf;
}

/* Optimized version of pru_output_signed_cbranch_ubyteop2 for constant
   zero second operand.  */

const char *
pru_output_signed_cbranch_zeroop2 (rtx *operands, bool is_near)
{
  static char buf[1024];
  enum rtx_code code = GET_CODE (operands[0]);
  int regop_sign_bit_pos = sign_bit_position (operands[1]);

  /* OP2 is a constant zero - utilize this info to simply check the
     OP1 sign bit when comparing for LT or GE.  */
  if (code == LT)
    {
      if (is_near)
	snprintf (buf, sizeof (buf),
		  "qbbs\t%%l3, %%1, %d\n\t",
		  regop_sign_bit_pos);
      else
	snprintf (buf, sizeof (buf),
		  "qbbc\t.+8, %%1, %d\n\t"
		  "jmp\t%%%%label(%%l3)",
		  regop_sign_bit_pos);
    }
  else if (code == GE)
    {
      if (is_near)
	snprintf (buf, sizeof (buf),
		  "qbbc\t%%l3, %%1, %d\n\t",
		  regop_sign_bit_pos);
      else
	snprintf (buf, sizeof (buf),
		  "qbbs\t.+8, %%1, %d\n\t"
		  "jmp\t%%%%label(%%l3)",
		  regop_sign_bit_pos);
    }
  else
    gcc_unreachable ();

  return buf;
}

/* Addressing Modes.  */

/* Return true if register REGNO is a valid base register.
   STRICT_P is true if REG_OK_STRICT is in effect.  */

bool
pru_regno_ok_for_base_p (int regno, bool strict_p)
{
  if (!HARD_REGISTER_NUM_P (regno) && !strict_p)
    return true;

  /* The fake registers will be eliminated to either the stack or
     hard frame pointer, both of which are usually valid base registers.
     Reload deals with the cases where the eliminated form isn't valid.  */
  return (GP_REG_P (regno)
	  || regno == FRAME_POINTER_REGNUM
	  || regno == ARG_POINTER_REGNUM);
}

/* Return true if given xbbo constant OFFSET is valid.  */
static bool
pru_valid_const_ubyte_offset (machine_mode mode, HOST_WIDE_INT offset)
{
  bool valid = UBYTE_INT (offset);

  /* Reload can split multi word accesses, so make sure we can address
     the second word in a DI.  */
  if (valid && GET_MODE_SIZE (mode) > GET_MODE_SIZE (SImode))
    valid = UBYTE_INT (offset + GET_MODE_SIZE (mode) - 1);

  return valid;
}

/* Recognize a CTABLE base address.  Return CTABLE entry index, or -1 if
   base was not found in the pragma-filled pru_ctable.  */
int
pru_get_ctable_exact_base_index (unsigned HOST_WIDE_INT caddr)
{
  unsigned int i;

  for (i = 0; i < ARRAY_SIZE (pru_ctable); i++)
    {
      if (pru_ctable[i].valid && pru_ctable[i].base == caddr)
	return i;
    }
  return -1;
}


/* Check if the given address can be addressed via CTABLE_BASE + UBYTE_OFFS,
   and return the base CTABLE index if possible.  */
int
pru_get_ctable_base_index (unsigned HOST_WIDE_INT caddr)
{
  unsigned int i;

  for (i = 0; i < ARRAY_SIZE (pru_ctable); i++)
    {
      if (pru_ctable[i].valid && IN_RANGE (caddr,
					   pru_ctable[i].base,
					   pru_ctable[i].base + 0xff))
	return i;
    }
  return -1;
}


/* Return the offset from some CTABLE base for this address.  */
int
pru_get_ctable_base_offset (unsigned HOST_WIDE_INT caddr)
{
  int i;

  i = pru_get_ctable_base_index (caddr);
  gcc_assert (i >= 0);

  return caddr - pru_ctable[i].base;
}

/* Return true if the address expression formed by BASE + OFFSET is
   valid.

   Note that the following address is not handled here:
	  base CTABLE constant base + UBYTE constant offset
   The constants will be folded.  The ctable_addr_operand predicate will take
   care of the validation.  The CTABLE base+offset split will happen during
   operand printing.  */
static bool
pru_valid_addr_expr_p (machine_mode mode, rtx base, rtx offset, bool strict_p)
{
  if (!strict_p && GET_CODE (base) == SUBREG)
    base = SUBREG_REG (base);
  if (!strict_p && GET_CODE (offset) == SUBREG)
    offset = SUBREG_REG (offset);

  if (REG_P (base)
      && pru_regno_ok_for_base_p (REGNO (base), strict_p)
      && ((CONST_INT_P (offset)
	      && pru_valid_const_ubyte_offset (mode, INTVAL (offset)))
	  || (REG_P (offset)
	      && pru_regno_ok_for_index_p (REGNO (offset), strict_p))))
    /*     base register + register offset
     * OR  base register + UBYTE constant offset.  */
    return true;
  else if (REG_P (base)
	   && pru_regno_ok_for_index_p (REGNO (base), strict_p)
	   && ctable_base_operand (offset, VOIDmode))
    /*     base CTABLE constant base + register offset
     * Note: GCC always puts the register as a first operand of PLUS.  */
    return true;
  else
    return false;
}

/* Implement TARGET_LEGITIMATE_ADDRESS_P.  */
static bool
pru_legitimate_address_p (machine_mode mode,
			    rtx operand, bool strict_p)
{
  switch (GET_CODE (operand))
    {
    /* Direct.  */
    case SYMBOL_REF:
    case LABEL_REF:
    case CONST:
    case CONST_WIDE_INT:
      return false;

    case CONST_INT:
      return ctable_addr_operand (operand, VOIDmode);

      /* Register indirect.  */
    case REG:
      return pru_regno_ok_for_base_p (REGNO (operand), strict_p);

      /* Register indirect with displacement.  */
    case PLUS:
      {
	rtx op0 = XEXP (operand, 0);
	rtx op1 = XEXP (operand, 1);

	return pru_valid_addr_expr_p (mode, op0, op1, strict_p);
      }

    default:
      break;
    }
  return false;
}

/* Output assembly language related definitions.  */

/* Implement TARGET_ASM_CONSTRUCTOR.  */
static void
pru_elf_asm_constructor (rtx symbol, int priority)
{
  char buf[23];
  section *s;

  if (priority == DEFAULT_INIT_PRIORITY)
    snprintf (buf, sizeof (buf), ".init_array");
  else
    {
      /* While priority is known to be in range [0, 65535], so 18 bytes
	 would be enough, the compiler might not know that.  To avoid
	 -Wformat-truncation false positive, use a larger size.  */
      snprintf (buf, sizeof (buf), ".init_array.%.5u", priority);
    }
  s = get_section (buf, SECTION_WRITE | SECTION_NOTYPE, NULL);
  switch_to_section (s);
  assemble_aligned_integer (INIT_ARRAY_ENTRY_BYTES, symbol);
}

/* Implement TARGET_ASM_DESTRUCTOR.  */
static void
pru_elf_asm_destructor (rtx symbol, int priority)
{
  char buf[23];
  section *s;

  if (priority == DEFAULT_INIT_PRIORITY)
    snprintf (buf, sizeof (buf), ".fini_array");
  else
    {
      /* While priority is known to be in range [0, 65535], so 18 bytes
	 would be enough, the compiler might not know that.  To avoid
	 -Wformat-truncation false positive, use a larger size.  */
      snprintf (buf, sizeof (buf), ".fini_array.%.5u", priority);
    }
  s = get_section (buf, SECTION_WRITE | SECTION_NOTYPE, NULL);
  switch_to_section (s);
  assemble_aligned_integer (INIT_ARRAY_ENTRY_BYTES, symbol);
}

/* Map rtx_code to unsigned PRU branch op suffix.  Callers must
   handle sign comparison themselves for signed operations.  */
static const char *
pru_comparison_str (enum rtx_code cond)
{
  switch (cond)
    {
    case NE:  return "ne";
    case EQ:  return "eq";
    case GEU: return "ge";
    case GTU: return "gt";
    case LEU: return "le";
    case LTU: return "lt";
    default: gcc_unreachable ();
    }
}

/* Access some RTX as INT_MODE.  If X is a CONST_FIXED we can get
   the bit representation of X by "casting" it to CONST_INT.  */

static rtx
pru_to_int_mode (rtx x)
{
  machine_mode mode = GET_MODE (x);

  return VOIDmode == mode
    ? x
    : simplify_gen_subreg (int_mode_for_mode (mode).require (), x, mode, 0);
}

/* Translate between the MachineDescription notion
   of 8-bit consecutive registers, to the PRU
   assembler syntax of REGWORD[.SUBREG].  */
static const char *
pru_asm_regname (rtx op)
{
  static char canon_reg_names[3][LAST_GP_REGNUM][8];
  int speci, regi;

  gcc_assert (REG_P (op));

  if (!canon_reg_names[0][0][0])
    {
      for (regi = 0; regi < LAST_GP_REGNUM; regi++)
	for (speci = 0; speci < 3; speci++)
	  {
	    const int sz = (speci == 0) ? 1 : ((speci == 1) ? 2 : 4);
	    if ((regi + sz) > (32 * 4))
	      continue;	/* Invalid entry.  */

	    /* Construct the lookup table.  */
	    const char *suffix = "";

	    switch ((sz << 8) | (regi % 4))
	      {
	      case (1 << 8) | 0: suffix = ".b0"; break;
	      case (1 << 8) | 1: suffix = ".b1"; break;
	      case (1 << 8) | 2: suffix = ".b2"; break;
	      case (1 << 8) | 3: suffix = ".b3"; break;
	      case (2 << 8) | 0: suffix = ".w0"; break;
	      case (2 << 8) | 1: suffix = ".w1"; break;
	      case (2 << 8) | 2: suffix = ".w2"; break;
	      case (4 << 8) | 0: suffix = ""; break;
	      default:
		/* Invalid entry.  */
		continue;
	      }
	    sprintf (&canon_reg_names[speci][regi][0],
		     "r%d%s", regi / 4, suffix);
	  }
    }

  switch (GET_MODE_SIZE (GET_MODE (op)))
    {
    case 1: speci = 0; break;
    case 2: speci = 1; break;
    case 4: speci = 2; break;
    case 8: speci = 2; break; /* Existing GCC test cases are not using %F.  */
    default: gcc_unreachable ();
    }
  regi = REGNO (op);
  gcc_assert (regi < LAST_GP_REGNUM);
  gcc_assert (canon_reg_names[speci][regi][0]);

  return &canon_reg_names[speci][regi][0];
}

/* Print the operand OP to file stream FILE modified by LETTER.
   LETTER can be one of:

     b: prints the register byte start (used by LBBO/SBBO).
     B: prints 'c' or 'b' for CTABLE or REG base in a memory address.
     F: Full 32-bit register.
     H: Higher 16-bits of a const_int operand.
     L: Lower 16-bits of a const_int operand.
     N: prints next 32-bit register (upper 32bits of a 64bit REG couple).
     P: prints swapped condition.
     Q: prints swapped and reversed condition.
     R: prints reversed condition.
     S: print operand mode size (but do not print the operand itself).
     T: print exact_log2 () for const_int operands.
     u: print QI constant integer as unsigned.  No transformation for regs.
     V: print exact_log2 () of negated const_int operands.
     w: Lower 32-bits of a const_int operand.
     W: Upper 32-bits of a const_int operand.
     y: print the next 8-bit register (regardless of op size).
     z: print the second next 8-bit register (regardless of op size).
*/
static void
pru_print_operand (FILE *file, rtx op, int letter)
{
  switch (letter)
    {
    case 'S':
      fprintf (file, "%d", GET_MODE_SIZE (GET_MODE (op)));
      return;

    default:
      break;
    }

  if (comparison_operator (op, VOIDmode))
    {
      enum rtx_code cond = GET_CODE (op);
      gcc_assert (!pru_signed_cmp_operator (op, VOIDmode));

      switch (letter)
	{
	case 0:
	  fprintf (file, "%s", pru_comparison_str (cond));
	  return;
	case 'P':
	  fprintf (file, "%s", pru_comparison_str (swap_condition (cond)));
	  return;
	case 'Q':
	  cond = swap_condition (cond);
	  /* Fall through.  */
	case 'R':
	  fprintf (file, "%s", pru_comparison_str (reverse_condition (cond)));
	  return;
	}
    }

  switch (GET_CODE (op))
    {
    case REG:
      if (letter == 0 || letter == 'u')
	{
	  fprintf (file, "%s", pru_asm_regname (op));
	  return;
	}
      else if (letter == 'b')
	{
	  if (REGNO (op) > LAST_NONIO_GP_REGNUM)
	    {
	      output_operand_lossage ("I/O register operand for '%%%c'",
				      letter);
	      return;
	    }
	  fprintf (file, "r%d.b%d", REGNO (op) / 4, REGNO (op) % 4);
	  return;
	}
      else if (letter == 'F' || letter == 'N')
	{
	  if (REGNO (op) > LAST_NONIO_GP_REGNUM - 1)
	    {
	      output_operand_lossage ("I/O register operand for '%%%c'",
				      letter);
	      return;
	    }
	  if (REGNO (op) % 4 != 0)
	    {
	      output_operand_lossage ("non 32 bit register operand for '%%%c'",
				      letter);
	      return;
	    }
	  fprintf (file, "r%d", REGNO (op) / 4 + (letter == 'N' ? 1 : 0));
	  return;
	}
      else if (letter == 'y')
	{
	  if (REGNO (op) > LAST_NONIO_GP_REGNUM - 1)
	    {
	      output_operand_lossage ("invalid operand for '%%%c'", letter);
	      return;
	    }
	  fprintf (file, "%s", reg_names[REGNO (op) + 1]);
	  return;
	}
      else if (letter == 'z')
	{
	  if (REGNO (op) > LAST_NONIO_GP_REGNUM - 2)
	    {
	      output_operand_lossage ("invalid operand for '%%%c'", letter);
	      return;
	    }
	  fprintf (file, "%s", reg_names[REGNO (op) + 2]);
	  return;
	}
      break;

    case CONST_INT:
      if (letter == 'H')
	{
	  HOST_WIDE_INT val = INTVAL (op);
	  val = (val >> 16) & 0xFFFF;
	  output_addr_const (file, gen_int_mode (val, SImode));
	  return;
	}
      else if (letter == 'L')
	{
	  HOST_WIDE_INT val = INTVAL (op);
	  val &= 0xFFFF;
	  output_addr_const (file, gen_int_mode (val, SImode));
	  return;
	}
      else if (letter == 'T')
	{
	  /* The predicate should have already validated the 1-high-bit
	     requirement.  Use CTZ here to deal with constant's sign
	     extension.  */
	  HOST_WIDE_INT val = wi::ctz (INTVAL (op));
	  if (val < 0 || val > 31)
	    {
	      output_operand_lossage ("invalid operand for '%%%c'", letter);
	      return;
	    }
	  output_addr_const (file, gen_int_mode (val, SImode));
	  return;
	}
      else if (letter == 'V')
	{
	  HOST_WIDE_INT val = wi::ctz (~INTVAL (op));
	  if (val < 0 || val > 31)
	    {
	      output_operand_lossage ("invalid operand for '%%%c'", letter);
	      return;
	    }
	  output_addr_const (file, gen_int_mode (val, SImode));
	  return;
	}
      else if (letter == 'w')
	{
	  HOST_WIDE_INT val = INTVAL (op) & 0xffffffff;
	  output_addr_const (file, gen_int_mode (val, SImode));
	  return;
	}
      else if (letter == 'W')
	{
	  HOST_WIDE_INT val = (INTVAL (op) >> 32) & 0xffffffff;
	  output_addr_const (file, gen_int_mode (val, SImode));
	  return;
	}
      else if (letter == 'u')
	{
	  /* Workaround GCC's representation of QI constants in sign-extended
	     form, and PRU's assembler insistence on unsigned constant
	     integers.  See the notes about O constraint.  */
	  fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (op) & 0xff);
	  return;
	}
      /* Else, fall through.  */

    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      if (letter == 0)
	{
	  output_addr_const (file, op);
	  return;
	}
      break;

    case CONST_FIXED:
	{
	  HOST_WIDE_INT ival = INTVAL (pru_to_int_mode (op));
	  if (letter != 0)
	    output_operand_lossage ("unsupported code '%c' for fixed-point:",
				    letter);
	  fprintf (file, HOST_WIDE_INT_PRINT_DEC, ival);
	  return;
	}
      break;

    case CONST_DOUBLE:
      if (letter == 0)
	{
	  long val;

	  if (GET_MODE (op) != SFmode)
	    {
	      output_operand_lossage ("double constants not supported");
	      return;
	    }
	  REAL_VALUE_TO_TARGET_SINGLE (*CONST_DOUBLE_REAL_VALUE (op), val);
	  fprintf (file, "0x%lx", val);
	  return;
	}
      else if (letter == 'w' || letter == 'W')
	{
	  long t[2];
	  REAL_VALUE_TO_TARGET_DOUBLE (*CONST_DOUBLE_REAL_VALUE (op), t);
	  fprintf (file, "0x%lx", t[letter == 'w' ? 0 : 1]);
	  return;
	}
      else
	{
	  output_operand_lossage ("invalid operand for '%%%c'", letter);
	  return;
	}
      break;

    case SUBREG:
      /* Subregs should not appear at so late stage.  */
      gcc_unreachable ();
      break;

    case MEM:
      if (letter == 0)
	{
	  output_address (VOIDmode, op);
	  return;
	}
      else if (letter == 'B')
	{
	  rtx base = XEXP (op, 0);
	  if (GET_CODE (base) == PLUS)
	    {
	      rtx op0 = XEXP (base, 0);
	      rtx op1 = XEXP (base, 1);

	      /* PLUS cannot have two constant operands, so first one
		 of them must be a REG, hence we must check for an
		 exact base address.  */
	      if (ctable_base_operand (op1, VOIDmode))
		{
		  fprintf (file, "c");
		  return;
		}
	      else if (REG_P (op0))
		{
		  fprintf (file, "b");
		  return;
		}
	      else
		gcc_unreachable ();
	    }
	  else if (REG_P (base))
	    {
	      fprintf (file, "b");
	      return;
	    }
	  else if (ctable_addr_operand (base, VOIDmode))
	    {
	      fprintf (file, "c");
	      return;
	    }
	  else
	    gcc_unreachable ();
	}
      break;

    case CODE_LABEL:
      if (letter == 0)
	{
	  output_addr_const (file, op);
	  return;
	}
      break;

    default:
      break;
    }

  output_operand_lossage ("unsupported operand %s for code '%c'",
			  GET_RTX_NAME (GET_CODE (op)), letter);
}

/* Implement TARGET_PRINT_OPERAND_ADDRESS.  */
static void
pru_print_operand_address (FILE *file, machine_mode mode, rtx op)
{
  if (CONSTANT_ADDRESS_P (op) && text_segment_operand (op, VOIDmode))
    {
      output_operand_lossage ("unexpected text address:");
      return;
    }

  switch (GET_CODE (op))
    {
    case CONST:
    case LABEL_REF:
    case CONST_WIDE_INT:
    case SYMBOL_REF:
      break;

    case CONST_INT:
      {
	unsigned HOST_WIDE_INT caddr = INTVAL (op);
	int base = pru_get_ctable_base_index (caddr);
	int offs = pru_get_ctable_base_offset (caddr);
	if (base < 0)
	  {
	    output_operand_lossage ("unsupported constant address:");
	    return;
	  }
	fprintf (file, "%d, %d", base, offs);
	return;
      }
      break;

    case PLUS:
      {
	int base;
	rtx op0 = XEXP (op, 0);
	rtx op1 = XEXP (op, 1);

	if (REG_P (op0) && CONST_INT_P (op1)
	    && pru_get_ctable_exact_base_index (INTVAL (op1)) >= 0)
	  {
	    base = pru_get_ctable_exact_base_index (INTVAL (op1));
	    fprintf (file, "%d, %s", base, pru_asm_regname (op0));
	    return;
	  }
	else if (REG_P (op1) && CONST_INT_P (op0)
		 && pru_get_ctable_exact_base_index (INTVAL (op0)) >= 0)
	  {
	    /* Not a valid RTL.  */
	    gcc_unreachable ();
	  }
	else if (REG_P (op0) && CONSTANT_P (op1))
	  {
	    fprintf (file, "%s, ", pru_asm_regname (op0));
	    output_addr_const (file, op1);
	    return;
	  }
	else if (REG_P (op1) && CONSTANT_P (op0))
	  {
	    /* Not a valid RTL.  */
	    gcc_unreachable ();
	  }
	else if (REG_P (op1) && REG_P (op0))
	  {
	    fprintf (file, "%s, %s", pru_asm_regname (op0),
				     pru_asm_regname (op1));
	    return;
	  }
      }
      break;

    case REG:
      fprintf (file, "%s, 0", pru_asm_regname (op));
      return;

    case MEM:
      {
	rtx base = XEXP (op, 0);
	pru_print_operand_address (file, mode, base);
	return;
      }
    default:
      break;
    }

  output_operand_lossage ("unsupported memory expression:");
}

/* Implement TARGET_ASM_FUNCTION_PROLOGUE.  */
static void
pru_asm_function_prologue (FILE *file)
{
  if (flag_verbose_asm || flag_debug_asm)
    pru_dump_frame_layout (file);
}

/* Implement `TARGET_ASM_INTEGER'.
   Target hook for assembling integer objects.  PRU version needs
   special handling for references to pmem.  Code copied from AVR.  */

static bool
pru_assemble_integer (rtx x, unsigned int size, int aligned_p)
{
  if (size == POINTER_SIZE / BITS_PER_UNIT
      && aligned_p
      && text_segment_operand (x, VOIDmode))
    {
      fputs ("\t.4byte\t%pmem(", asm_out_file);
      output_addr_const (asm_out_file, x);
      fputs (")\n", asm_out_file);

      return true;
    }
  else if (size == INIT_ARRAY_ENTRY_BYTES
	   && aligned_p
	   && text_segment_operand (x, VOIDmode))
    {
      fputs ("\t.2byte\t%pmem(", asm_out_file);
      output_addr_const (asm_out_file, x);
      fputs (")\n", asm_out_file);

      return true;
    }
  else
    {
      return default_assemble_integer (x, size, aligned_p);
    }
}

/* Implement TARGET_ASM_FILE_START.  */

static void
pru_file_start (void)
{
  default_file_start ();

  /* Compiler will take care of placing %label, so there is no
     need to confuse users with this warning.  */
  fprintf (asm_out_file, "\t.set no_warn_regname_label\n");
}

/* Function argument related.  */

/* Return the number of bytes needed for storing an argument with
   the given MODE and TYPE.  */
static int
pru_function_arg_size (machine_mode mode, const_tree type)
{
  HOST_WIDE_INT param_size;

  if (mode == BLKmode)
    param_size = int_size_in_bytes (type);
  else
    param_size = GET_MODE_SIZE (mode);

  /* Convert to words (round up).  */
  param_size = (UNITS_PER_WORD - 1 + param_size) / UNITS_PER_WORD;
  gcc_assert (param_size >= 0);

  return param_size;
}

/* Check if argument with the given size must be
   passed/returned in a register.

   Reference:
   https://e2e.ti.com/support/development_tools/compiler/f/343/p/650176/2393029

   Arguments other than 8/16/24/32/64bits are passed on stack.  */
static bool
pru_arg_in_reg_bysize (size_t sz)
{
  return sz == 1 || sz == 2 || sz == 3 || sz == 4 || sz == 8;
}

/* Helper function to get the starting storage HW register for an argument,
   or -1 if it must be passed on stack.  The cum_v state is not changed.  */
static int
pru_function_arg_regi (cumulative_args_t cum_v,
		       machine_mode mode, const_tree type,
		       bool named)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  size_t argsize = pru_function_arg_size (mode, type);
  size_t i, bi;
  int regi = -1;

  if (!pru_arg_in_reg_bysize (argsize))
    return -1;

  if (!named)
    return -1;

  /* Find the first available slot that fits.  Yes, that's the PRU ABI.  */
  for (i = 0; regi < 0 && i < ARRAY_SIZE (cum->regs_used); i++)
    {
      /* VLAs and vector types are not defined in the PRU ABI.  Let's
	 handle them the same as their same-sized counterparts.  This way
	 we do not need to treat BLKmode differently, and need only to check
	 the size.  */
      gcc_assert (argsize == 1 || argsize == 2 || argsize == 3
		  || argsize == 4 || argsize == 8);

      /* Ensure SI and DI arguments are stored in full registers only.  */
      if ((argsize >= 4) && (i % 4) != 0)
	continue;

      /* Structures with size 24 bits are passed starting at a full
	 register boundary.  */
      if (argsize == 3 && (i % 4) != 0)
	continue;

      /* rX.w0/w1/w2 are OK.  But avoid spreading the second byte
	 into a different full register.  */
      if (argsize == 2 && (i % 4) == 3)
	continue;

      for (bi = 0;
	   bi < argsize && (bi + i) < ARRAY_SIZE (cum->regs_used);
	   bi++)
	{
	  if (cum->regs_used[bi + i])
	    break;
	}
      if (bi == argsize)
	regi = FIRST_ARG_REGNUM + i;
    }

  return regi;
}

/* Mark CUM_V that a function argument will occupy HW register slot starting
   at REGI.  The number of consecutive 8-bit HW registers marked as occupied
   depends on the MODE and TYPE of the argument.  */
static void
pru_function_arg_regi_mark_slot (int regi,
				 cumulative_args_t cum_v,
				 machine_mode mode, const_tree type,
				 bool named)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  HOST_WIDE_INT param_size = pru_function_arg_size (mode, type);

  gcc_assert (named);

  /* Mark all byte sub-registers occupied by argument as used.  */
  while (param_size--)
    {
      gcc_assert (regi >= FIRST_ARG_REGNUM && regi <= LAST_ARG_REGNUM);
      gcc_assert (!cum->regs_used[regi - FIRST_ARG_REGNUM]);
      cum->regs_used[regi - FIRST_ARG_REGNUM] = true;
      regi++;
    }
}

/* Define where to put the arguments to a function.  Value is zero to
   push the argument on the stack, or a hard register in which to
   store the argument.

   CUM is a variable of type CUMULATIVE_ARGS which gives info about
   the preceding args and about the function being called.
   ARG is a description of the argument.  */

static rtx
pru_function_arg (cumulative_args_t cum_v, const function_arg_info &arg)
{
  rtx return_rtx = NULL_RTX;
  int regi = pru_function_arg_regi (cum_v, arg.mode, arg.type, arg.named);

  if (regi >= 0)
    return_rtx = gen_rtx_REG (arg.mode, regi);

  return return_rtx;
}

/* Implement TARGET_ARG_PARTIAL_BYTES.  PRU never splits any arguments
   between registers and memory, so we can return 0.  */

static int
pru_arg_partial_bytes (cumulative_args_t, const function_arg_info &)
{
  return 0;
}

/* Update the data in CUM to advance over argument ARG.  */

static void
pru_function_arg_advance (cumulative_args_t cum_v,
			  const function_arg_info &arg)
{
  int regi = pru_function_arg_regi (cum_v, arg.mode, arg.type, arg.named);

  if (regi >= 0)
    pru_function_arg_regi_mark_slot (regi, cum_v, arg.mode,
				     arg.type, arg.named);
}

/* Implement TARGET_FUNCTION_VALUE.  */
static rtx
pru_function_value (const_tree ret_type, const_tree fn ATTRIBUTE_UNUSED,
		      bool outgoing ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (TYPE_MODE (ret_type), FIRST_RETVAL_REGNUM);
}

/* Implement TARGET_LIBCALL_VALUE.  */
static rtx
pru_libcall_value (machine_mode mode, const_rtx fun ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (mode, FIRST_RETVAL_REGNUM);
}

/* Implement TARGET_FUNCTION_VALUE_REGNO_P.  */
static bool
pru_function_value_regno_p (const unsigned int regno)
{
  return regno == FIRST_RETVAL_REGNUM;
}

/* Implement TARGET_RETURN_IN_MEMORY.  */
bool
pru_return_in_memory (const_tree type, const_tree fntype ATTRIBUTE_UNUSED)
{
  bool in_memory = (!pru_arg_in_reg_bysize (int_size_in_bytes (type))
		    || int_size_in_bytes (type) == -1);

  return in_memory;
}

/* Implement TARGET_CAN_USE_DOLOOP_P.  */

static bool
pru_can_use_doloop_p (const widest_int &, const widest_int &iterations_max,
		      unsigned int loop_depth, bool)
{
  /* Considering limitations in the hardware, only use doloop
     for innermost loops which must be entered from the top.  */
  if (loop_depth > 1)
    return false;
  /* PRU internal loop counter is 16bits wide.  Remember that iterations_max
     holds the maximum number of loop latch executions, while PRU loop
     instruction needs the count of loop body executions.  */
  if (iterations_max == 0 || wi::geu_p (iterations_max, 0xffff))
    return false;

  return true;
}

/* NULL if INSN insn is valid within a low-overhead loop.
   Otherwise return why doloop cannot be applied.  */

static const char *
pru_invalid_within_doloop (const rtx_insn *insn)
{
  if (CALL_P (insn))
    return "Function call in the loop.";

  if (JUMP_P (insn) && INSN_CODE (insn) == CODE_FOR_return)
    return "Return from a call instruction in the loop.";

  if (NONDEBUG_INSN_P (insn)
      && INSN_CODE (insn) < 0
      && (GET_CODE (PATTERN (insn)) == ASM_INPUT
	  || asm_noperands (PATTERN (insn)) >= 0))
    return "Loop contains asm statement.";

  return NULL;
}


/* Figure out where to put LABEL, which is the label for a repeat loop.
   The loop ends just before LAST_INSN.  If SHARED, insns other than the
   "repeat" might use LABEL to jump to the loop's continuation point.

   Return the last instruction in the adjusted loop.  */

static rtx_insn *
pru_insert_loop_label_last (rtx_insn *last_insn, rtx_code_label *label,
			    bool shared)
{
  rtx_insn *next, *prev;
  int count = 0, code, icode;

  if (dump_file)
    fprintf (dump_file, "considering end of repeat loop at insn %d\n",
	     INSN_UID (last_insn));

  /* Set PREV to the last insn in the loop.  */
  prev = PREV_INSN (last_insn);

  /* Set NEXT to the next insn after the loop label.  */
  next = last_insn;
  if (!shared)
    while (prev != 0)
      {
	code = GET_CODE (prev);
	if (code == CALL_INSN || code == CODE_LABEL || code == BARRIER)
	  break;

	if (INSN_P (prev))
	  {
	    if (GET_CODE (PATTERN (prev)) == SEQUENCE)
	      prev = as_a <rtx_insn *> (XVECEXP (PATTERN (prev), 0, 1));

	    /* Other insns that should not be in the last two opcodes.  */
	    icode = recog_memoized (prev);
	    if (icode < 0
		|| icode == CODE_FOR_pruloophi
		|| icode == CODE_FOR_pruloopsi)
	      break;

	    count++;
	    next = prev;
	    if (dump_file)
	      print_rtl_single (dump_file, next);
	    if (count == 2)
	      break;
	  }
	prev = PREV_INSN (prev);
      }

  /* Insert the nops.  */
  if (dump_file && count < 2)
    fprintf (dump_file, "Adding %d nop%s inside loop\n\n",
	     2 - count, count == 1 ? "" : "s");

  for (; count < 2; count++)
    emit_insn_before (gen_nop (), last_insn);

  /* Insert the label.  */
  emit_label_before (label, last_insn);

  return last_insn;
}

/* If IS_END is false, expand a canonical doloop_begin RTL into the
   PRU-specific doloop_begin_internal.  Otherwise expand doloop_end to
   doloop_end_internal.  */
void
pru_emit_doloop (rtx *operands, int is_end)
{
  rtx tag;

  if (cfun->machine->doloop_tags == 0
      || cfun->machine->doloop_tag_from_end == is_end)
    {
      cfun->machine->doloop_tags++;
      cfun->machine->doloop_tag_from_end = is_end;
    }

  tag = GEN_INT (cfun->machine->doloop_tags - 1);
  machine_mode opmode = GET_MODE (operands[0]);
  gcc_assert (opmode == HImode || opmode == SImode);

  if (is_end)
    emit_jump_insn (gen_doloop_end_internal (opmode, operands[0],
					     operands[1], tag));
  else
    emit_insn (gen_doloop_begin_internal (opmode, operands[0],
					  operands[0], tag));
}


/* Code for converting doloop_begins and doloop_ends into valid
   PRU instructions.  Idea and code snippets borrowed from mep port.

   A doloop_begin is just a placeholder:

	$count = unspec ($count)

   where $count is initially the number of iterations.
   doloop_end has the form:

	if (--$count == 0) goto label

   The counter variable is private to the doloop insns, nothing else
   relies on its value.

   There are three cases, in decreasing order of preference:

      1.  A loop has exactly one doloop_begin and one doloop_end.
	 The doloop_end branches to the first instruction after
	 the doloop_begin.

	 In this case we can replace the doloop_begin with a LOOP
	 instruction and remove the doloop_end.  I.e.:

		$count1 = unspec ($count1)
	    label:
		...
		if (--$count2 != 0) goto label

	  becomes:

		LOOP end_label,$count1
	    label:
		...
	    end_label:
		# end loop

      2.  As for (1), except there are several doloop_ends.  One of them
	 (call it X) falls through to a label L.  All the others fall
	 through to branches to L.

	 In this case, we remove X and replace the other doloop_ends
	 with branches to the LOOP label.  For example:

		$count1 = unspec ($count1)
	    label:
		...
		if (--$count1 != 0) goto label
	    end_label:
		...
		if (--$count2 != 0) goto label
		goto end_label

	 becomes:

		LOOP end_label,$count1
	    label:
		...
	    end_label:
		# end repeat
		...
		goto end_label

      3.  The fallback case.  Replace doloop_begins with:

		$count = $count

	 Replace doloop_ends with the equivalent of:

		$count = $count - 1
		if ($count != 0) goto loop_label

	 */

/* A structure describing one doloop_begin.  */
struct pru_doloop_begin {
  /* The next doloop_begin with the same tag.  */
  struct pru_doloop_begin *next;

  /* The instruction itself.  */
  rtx_insn *insn;

  /* The initial counter value.  */
  rtx loop_count;

  /* The counter register.  */
  rtx counter;
};

/* A structure describing a doloop_end.  */
struct pru_doloop_end {
  /* The next doloop_end with the same loop tag.  */
  struct pru_doloop_end *next;

  /* The instruction itself.  */
  rtx_insn *insn;

  /* The first instruction after INSN when the branch isn't taken.  */
  rtx_insn *fallthrough;

  /* The location of the counter value.  Since doloop_end_internal is a
     jump instruction, it has to allow the counter to be stored anywhere
     (any non-fixed register).  */
  rtx counter;

  /* The target label (the place where the insn branches when the counter
     isn't zero).  */
  rtx label;

  /* A scratch register.  Only available when COUNTER isn't stored
     in a general register.  */
  rtx scratch;
};


/* One do-while loop.  */
struct pru_doloop {
  /* All the doloop_begins for this loop (in no particular order).  */
  struct pru_doloop_begin *begin;

  /* All the doloop_ends.  When there is more than one, arrange things
     so that the first one is the most likely to be X in case (2) above.  */
  struct pru_doloop_end *end;
};


/* Return true if LOOP can be converted into LOOP form
   (that is, if it matches cases (1) or (2) above).  */

static bool
pru_repeat_loop_p (struct pru_doloop *loop)
{
  struct pru_doloop_end *end;
  rtx_insn *fallthrough;

  /* There must be exactly one doloop_begin and at least one doloop_end.  */
  if (loop->begin == 0 || loop->end == 0 || loop->begin->next != 0)
    return false;

  /* The first doloop_end (X) must branch back to the insn after
     the doloop_begin.  */
  if (prev_real_insn (as_a<rtx_insn *> (loop->end->label)) != loop->begin->insn)
    return false;

  /* Check that the first doloop_end (X) can actually reach
     doloop_begin () with U8_PCREL relocation for LOOP instruction.  */
  if (get_attr_length (loop->end->insn) != 4)
    return false;

  /* All the other doloop_ends must branch to the same place as X.
     When the branch isn't taken, they must jump to the instruction
     after X.  */
  fallthrough = loop->end->fallthrough;
  for (end = loop->end->next; end != 0; end = end->next)
    if (end->label != loop->end->label
	|| !simplejump_p (end->fallthrough)
	|| fallthrough
	   != next_real_insn (JUMP_LABEL_AS_INSN (end->fallthrough)))
      return false;

  return true;
}


/* The main repeat reorg function.  See comment above for details.  */

static void
pru_reorg_loop (rtx_insn *insns)
{
  rtx_insn *insn;
  struct pru_doloop *loops, *loop;
  struct pru_doloop_begin *begin;
  struct pru_doloop_end *end;
  size_t tmpsz;

  /* Quick exit if we haven't created any loops.  */
  if (cfun->machine->doloop_tags == 0)
    return;

  /* Create an array of pru_doloop structures.  */
  tmpsz = sizeof (loops[0]) * cfun->machine->doloop_tags;
  loops = (struct pru_doloop *) alloca (tmpsz);
  memset (loops, 0, sizeof (loops[0]) * cfun->machine->doloop_tags);

  /* Search the function for do-while insns and group them by loop tag.  */
  for (insn = insns; insn; insn = NEXT_INSN (insn))
    if (INSN_P (insn))
      switch (recog_memoized (insn))
	{
	case CODE_FOR_doloop_begin_internalhi:
	case CODE_FOR_doloop_begin_internalsi:
	  insn_extract (insn);
	  loop = &loops[INTVAL (recog_data.operand[2])];

	  tmpsz = sizeof (struct pru_doloop_begin);
	  begin = (struct pru_doloop_begin *) alloca (tmpsz);
	  begin->next = loop->begin;
	  begin->insn = insn;
	  begin->loop_count = recog_data.operand[1];
	  begin->counter = recog_data.operand[0];

	  loop->begin = begin;
	  break;

	case CODE_FOR_doloop_end_internalhi:
	case CODE_FOR_doloop_end_internalsi:
	  insn_extract (insn);
	  loop = &loops[INTVAL (recog_data.operand[2])];

	  tmpsz = sizeof (struct pru_doloop_end);
	  end = (struct pru_doloop_end *) alloca (tmpsz);
	  end->insn = insn;
	  end->fallthrough = next_real_insn (insn);
	  end->counter = recog_data.operand[0];
	  end->label = recog_data.operand[1];
	  end->scratch = recog_data.operand[3];

	  /* If this insn falls through to an unconditional jump,
	     give it a lower priority than the others.  */
	  if (loop->end != 0 && simplejump_p (end->fallthrough))
	    {
	      end->next = loop->end->next;
	      loop->end->next = end;
	    }
	  else
	    {
	      end->next = loop->end;
	      loop->end = end;
	    }
	  break;
	}

  /* Convert the insns for each loop in turn.  */
  for (loop = loops; loop < loops + cfun->machine->doloop_tags; loop++)
    if (pru_repeat_loop_p (loop))
      {
	/* Case (1) or (2).  */
	rtx_code_label *repeat_label;
	rtx label_ref;
	rtx loop_rtx;

	/* Create a new label for the repeat insn.  */
	repeat_label = gen_label_rtx ();

	/* Replace the doloop_begin with a repeat.  We get rid
	   of the iteration register because LOOP instruction
	   will utilize an internal for the PRU core LOOP register.  */
	label_ref = gen_rtx_LABEL_REF (VOIDmode, repeat_label);
	machine_mode loop_mode = GET_MODE (loop->begin->loop_count);
	if (loop_mode == VOIDmode)
	  {
	    gcc_assert (CONST_INT_P (loop->begin->loop_count));
	    gcc_assert (UBYTE_INT ( INTVAL (loop->begin->loop_count)));
	    loop_mode = SImode;
	  }
	gcc_assert (loop_mode == HImode || loop_mode == SImode);
	loop_rtx = gen_pruloop (loop_mode, loop->begin->loop_count, label_ref);
	emit_insn_before (loop_rtx, loop->begin->insn);

	delete_insn (loop->begin->insn);

	/* Insert the repeat label before the first doloop_end.
	   Fill the gap with nops if LOOP insn is less than 2
	   instructions away than loop->end.  */
	pru_insert_loop_label_last (loop->end->insn, repeat_label,
				    loop->end->next != 0);

	/* Emit a pruloop_end (to improve the readability of the output).  */
	emit_insn_before (gen_pruloop_end (), loop->end->insn);

	/* HACK: TODO: This is usually not needed, but is required for
	   a few rare cases where a JUMP that breaks the loop
	   references the LOOP_END address.  In other words, since
	   we're missing a real "loop_end" instruction, a loop "break"
	   may accidentally reference the loop end itself, and thus
	   continuing the cycle.  */
	for (insn = NEXT_INSN (loop->end->insn);
	     insn != next_real_insn (loop->end->insn);
	     insn = NEXT_INSN (insn))
	  {
	    if (LABEL_P (insn) && LABEL_NUSES (insn) > 0)
	      emit_insn_before (gen_nop_loop_guard (), loop->end->insn);
	  }

	/* Delete the first doloop_end.  */
	delete_insn (loop->end->insn);

	/* Replace the others with branches to REPEAT_LABEL.  */
	for (end = loop->end->next; end != 0; end = end->next)
	  {
	    rtx_insn *newjmp;
	    newjmp = emit_jump_insn_before (gen_jump (repeat_label), end->insn);
	    JUMP_LABEL (newjmp) = repeat_label;
	    delete_insn (end->insn);
	    delete_insn (end->fallthrough);
	  }
      }
    else
      {
	/* Case (3).  First replace all the doloop_begins with setting
	   the HW register used for loop counter.  */
	for (begin = loop->begin; begin != 0; begin = begin->next)
	  {
	    insn = gen_move_insn (copy_rtx (begin->counter),
				  copy_rtx (begin->loop_count));
	    emit_insn_before (insn, begin->insn);
	    delete_insn (begin->insn);
	  }

	/* Replace all the doloop_ends with decrement-and-branch sequences.  */
	for (end = loop->end; end != 0; end = end->next)
	  {
	    rtx reg;

	    start_sequence ();

	    /* Load the counter value into a general register.  */
	    reg = end->counter;
	    if (!REG_P (reg) || REGNO (reg) > LAST_NONIO_GP_REGNUM)
	      {
		reg = end->scratch;
		emit_move_insn (copy_rtx (reg), copy_rtx (end->counter));
	      }

	    /* Decrement the counter.  */
	    emit_insn (gen_add3_insn (copy_rtx (reg), copy_rtx (reg),
				      constm1_rtx));

	    /* Copy it back to its original location.  */
	    if (reg != end->counter)
	      emit_move_insn (copy_rtx (end->counter), copy_rtx (reg));

	    /* Jump back to the start label.  */
	    insn = emit_jump_insn (gen_cbranchsi4 (gen_rtx_NE (VOIDmode, reg,
							       const0_rtx),
						   reg,
						   const0_rtx,
						   end->label));

	    JUMP_LABEL (insn) = end->label;
	    LABEL_NUSES (end->label)++;

	    /* Emit the whole sequence before the doloop_end.  */
	    insn = get_insns ();
	    end_sequence ();
	    emit_insn_before (insn, end->insn);

	    /* Delete the doloop_end.  */
	    delete_insn (end->insn);
	  }
      }
}

/* Implement TARGET_MACHINE_DEPENDENT_REORG.  */
static void
pru_reorg (void)
{
  rtx_insn *insns = get_insns ();

  compute_bb_for_insn ();
  df_analyze ();

  /* Need correct insn lengths for allowing LOOP instruction
     emitting due to U8_PCREL limitations.  */
  shorten_branches (get_insns ());

  /* The generic reorg_loops () is not suitable for PRU because
     it doesn't handle doloop_begin/end tying.  And we need our
     doloop_begin emitted before reload.  It is difficult to coalesce
     UBYTE constant initial loop values into the LOOP insn during
     machine reorg phase.  */
  pru_reorg_loop (insns);

  df_finish_pass (false);
}

/* Enumerate all PRU-specific builtins.  */
enum pru_builtin
{
  PRU_BUILTIN_DELAY_CYCLES,
  PRU_BUILTIN_max
};

static GTY(()) tree pru_builtins [(int) PRU_BUILTIN_max];

/* Implement TARGET_INIT_BUILTINS.  */

static void
pru_init_builtins (void)
{
  tree void_ftype_longlong
    = build_function_type_list (void_type_node,
				long_long_integer_type_node,
				NULL);

  pru_builtins[PRU_BUILTIN_DELAY_CYCLES]
    = add_builtin_function ("__delay_cycles", void_ftype_longlong,
			    PRU_BUILTIN_DELAY_CYCLES, BUILT_IN_MD, NULL,
			    NULL_TREE);
}

/* Implement TARGET_BUILTIN_DECL.  */

static tree
pru_builtin_decl (unsigned code, bool initialize_p ATTRIBUTE_UNUSED)
{
  switch (code)
    {
    case PRU_BUILTIN_DELAY_CYCLES:
      return pru_builtins[code];
    default:
      return error_mark_node;
    }
}

/* Emit a sequence of one or more delay_cycles_X insns, in order to generate
   code that delays exactly ARG cycles.  */

static rtx
pru_expand_delay_cycles (rtx arg)
{
  HOST_WIDE_INT c, n;

  if (GET_CODE (arg) != CONST_INT)
    {
      error ("%<__delay_cycles%> only takes constant arguments");
      return NULL_RTX;
    }

  c = INTVAL (arg);

  gcc_assert (HOST_BITS_PER_WIDE_INT > 32);
  if (c < 0)
    {
      error ("%<__delay_cycles%> only takes non-negative cycle counts");
      return NULL_RTX;
    }

  emit_insn (gen_delay_cycles_start (arg));

  /* For 32-bit loops, there's 2 + 2x cycles.  */
  if (c > 2 * 0xffff + 1)
    {
      n = (c - 2) / 2;
      c -= (n * 2) + 2;
      if ((unsigned long long) n > 0xffffffffULL)
	{
	  error ("%<__delay_cycles%> is limited to 32-bit loop counts");
	  return NULL_RTX;
	}
      emit_insn (gen_delay_cycles_2x_plus2_si (GEN_INT (n)));
    }

  /* For 16-bit loops, there's 1 + 2x cycles.  */
  if (c > 2)
    {
      n = (c - 1) / 2;
      c -= (n * 2) + 1;

      emit_insn (gen_delay_cycles_2x_plus1_hi (GEN_INT (n)));
    }

  while (c > 0)
    {
      emit_insn (gen_delay_cycles_1 ());
      c -= 1;
    }

  emit_insn (gen_delay_cycles_end (arg));

  return NULL_RTX;
}


/* Implement TARGET_EXPAND_BUILTIN.  Expand an expression EXP that calls
   a built-in function, with result going to TARGET if that's convenient
   (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.  */

static rtx
pru_expand_builtin (tree exp, rtx target ATTRIBUTE_UNUSED,
		    rtx subtarget ATTRIBUTE_UNUSED,
		    machine_mode mode ATTRIBUTE_UNUSED,
		    int ignore ATTRIBUTE_UNUSED)
{
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  unsigned int fcode = DECL_MD_FUNCTION_CODE (fndecl);
  rtx arg1 = expand_normal (CALL_EXPR_ARG (exp, 0));

  if (fcode == PRU_BUILTIN_DELAY_CYCLES)
    return pru_expand_delay_cycles (arg1);

  internal_error ("bad builtin code");

  return NULL_RTX;
}

/* Remember the last target of pru_set_current_function.  */
static GTY(()) tree pru_previous_fndecl;

/* Establish appropriate back-end context for processing the function
   FNDECL.  The argument might be NULL to indicate processing at top
   level, outside of any function scope.  */
static void
pru_set_current_function (tree fndecl)
{
  tree old_tree = (pru_previous_fndecl
		   ? DECL_FUNCTION_SPECIFIC_TARGET (pru_previous_fndecl)
		   : NULL_TREE);

  tree new_tree = (fndecl
		   ? DECL_FUNCTION_SPECIFIC_TARGET (fndecl)
		   : NULL_TREE);

  if (fndecl && fndecl != pru_previous_fndecl)
    {
      pru_previous_fndecl = fndecl;
      if (old_tree == new_tree)
	;

      else if (new_tree)
	{
	  cl_target_option_restore (&global_options,
				    TREE_TARGET_OPTION (new_tree));
	  target_reinit ();
	}

      else if (old_tree)
	{
	  struct cl_target_option *def
	    = TREE_TARGET_OPTION (target_option_current_node);

	  cl_target_option_restore (&global_options, def);
	  target_reinit ();
	}
    }
}

/* Implement TARGET_UNWIND_WORD_MODE.

   Since PRU is really a 32-bit CPU, the default word_mode is not suitable.  */
static scalar_int_mode
pru_unwind_word_mode (void)
{
  return SImode;
}


/* Initialize the GCC target structure.  */
#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE pru_asm_function_prologue
#undef TARGET_ASM_INTEGER
#define TARGET_ASM_INTEGER pru_assemble_integer

#undef TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START pru_file_start

#undef TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS pru_init_builtins
#undef TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN pru_expand_builtin
#undef TARGET_BUILTIN_DECL
#define TARGET_BUILTIN_DECL pru_builtin_decl

#undef TARGET_COMPUTE_FRAME_LAYOUT
#define TARGET_COMPUTE_FRAME_LAYOUT pru_compute_frame_layout

#undef TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL hook_bool_tree_tree_true

#undef TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE pru_can_eliminate

#undef TARGET_HARD_REGNO_MODE_OK
#define TARGET_HARD_REGNO_MODE_OK pru_hard_regno_mode_ok

#undef  TARGET_HARD_REGNO_SCRATCH_OK
#define TARGET_HARD_REGNO_SCRATCH_OK pru_hard_regno_scratch_ok

#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG pru_function_arg

#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE pru_function_arg_advance

#undef TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES pru_arg_partial_bytes

#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE pru_function_value

#undef TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE pru_libcall_value

#undef TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P pru_function_value_regno_p

#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY pru_return_in_memory

#undef TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK must_pass_in_stack_var_size

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P pru_legitimate_address_p

#undef TARGET_INIT_LIBFUNCS
#define TARGET_INIT_LIBFUNCS pru_init_libfuncs
#undef TARGET_LIBFUNC_GNU_PREFIX
#define TARGET_LIBFUNC_GNU_PREFIX true

#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS pru_rtx_costs

#undef TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND pru_print_operand

#undef TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS pru_print_operand_address

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE pru_option_override

#undef TARGET_SET_CURRENT_FUNCTION
#define TARGET_SET_CURRENT_FUNCTION pru_set_current_function

#undef  TARGET_MACHINE_DEPENDENT_REORG
#define TARGET_MACHINE_DEPENDENT_REORG  pru_reorg

#undef  TARGET_CAN_USE_DOLOOP_P
#define TARGET_CAN_USE_DOLOOP_P		pru_can_use_doloop_p

#undef TARGET_INVALID_WITHIN_DOLOOP
#define TARGET_INVALID_WITHIN_DOLOOP  pru_invalid_within_doloop

#undef  TARGET_UNWIND_WORD_MODE
#define TARGET_UNWIND_WORD_MODE pru_unwind_word_mode

#undef TARGET_HAVE_SPECULATION_SAFE_VALUE
#define TARGET_HAVE_SPECULATION_SAFE_VALUE speculation_safe_value_not_needed

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-pru.h"
