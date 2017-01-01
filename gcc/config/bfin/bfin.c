/* The Blackfin code generation auxiliary output file.
   Copyright (C) 2005-2017 Free Software Foundation, Inc.
   Contributed by Analog Devices.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "cfghooks.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "cgraph.h"
#include "diagnostic-core.h"
#include "output.h"
#include "insn-attr.h"
#include "varasm.h"
#include "calls.h"
#include "explow.h"
#include "expr.h"
#include "cfgrtl.h"
#include "langhooks.h"
#include "tm-constrs.h"
#include "gt-bfin.h"
#include "sel-sched.h"
#include "hw-doloop.h"
#include "dumpfile.h"
#include "builtins.h"

/* This file should be included last.  */
#include "target-def.h"

/* A C structure for machine-specific, per-function data.
   This is added to the cfun structure.  */
struct GTY(()) machine_function
{
  /* Set if we are notified by the doloop pass that a hardware loop
     was created.  */
  int has_hardware_loops;

  /* Set if we create a memcpy pattern that uses loop registers.  */
  int has_loopreg_clobber;
};

/* RTX for condition code flag register and RETS register */
extern GTY(()) rtx bfin_cc_rtx;
extern GTY(()) rtx bfin_rets_rtx;
rtx bfin_cc_rtx, bfin_rets_rtx;

int max_arg_registers = 0;

/* Arrays used when emitting register names.  */
const char *short_reg_names[]  =  SHORT_REGISTER_NAMES;
const char *high_reg_names[]   =  HIGH_REGISTER_NAMES;
const char *dregs_pair_names[] =  DREGS_PAIR_NAMES;
const char *byte_reg_names[]   =  BYTE_REGISTER_NAMES;

static int arg_regs[] = FUNCTION_ARG_REGISTERS;
static int ret_regs[] = FUNCTION_RETURN_REGISTERS;

int splitting_for_sched, splitting_loops;

static void
bfin_globalize_label (FILE *stream, const char *name)
{
  fputs (".global ", stream);
  assemble_name (stream, name);
  fputc (';',stream);
  fputc ('\n',stream);
}

static void 
output_file_start (void) 
{
  FILE *file = asm_out_file;
  int i;

  fprintf (file, ".file \"%s\";\n", LOCATION_FILE (input_location));
  
  for (i = 0; arg_regs[i] >= 0; i++)
    ;
  max_arg_registers = i;	/* how many arg reg used  */
}

/* Examine machine-dependent attributes of function type FUNTYPE and return its
   type.  See the definition of E_FUNKIND.  */

static e_funkind
funkind (const_tree funtype)
{
  tree attrs = TYPE_ATTRIBUTES (funtype);
  if (lookup_attribute ("interrupt_handler", attrs))
    return INTERRUPT_HANDLER;
  else if (lookup_attribute ("exception_handler", attrs))
    return EXCPT_HANDLER;
  else if (lookup_attribute ("nmi_handler", attrs))
    return NMI_HANDLER;
  else
    return SUBROUTINE;
}

/* Legitimize PIC addresses.  If the address is already position-independent,
   we return ORIG.  Newly generated position-independent addresses go into a
   reg.  This is REG if nonzero, otherwise we allocate register(s) as
   necessary.  PICREG is the register holding the pointer to the PIC offset
   table.  */

static rtx
legitimize_pic_address (rtx orig, rtx reg, rtx picreg)
{
  rtx addr = orig;
  rtx new_rtx = orig;

  if (GET_CODE (addr) == SYMBOL_REF || GET_CODE (addr) == LABEL_REF)
    {
      int unspec;
      rtx tmp;

      if (TARGET_ID_SHARED_LIBRARY)
	unspec = UNSPEC_MOVE_PIC;
      else if (GET_CODE (addr) == SYMBOL_REF
	       && SYMBOL_REF_FUNCTION_P (addr))
	unspec = UNSPEC_FUNCDESC_GOT17M4;
      else
	unspec = UNSPEC_MOVE_FDPIC;

      if (reg == 0)
	{
	  gcc_assert (can_create_pseudo_p ());
	  reg = gen_reg_rtx (Pmode);
	}

      tmp = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), unspec);
      new_rtx = gen_const_mem (Pmode, gen_rtx_PLUS (Pmode, picreg, tmp));

      emit_move_insn (reg, new_rtx);
      if (picreg == pic_offset_table_rtx)
	crtl->uses_pic_offset_table = 1;
      return reg;
    }

  else if (GET_CODE (addr) == CONST || GET_CODE (addr) == PLUS)
    {
      rtx base;

      if (GET_CODE (addr) == CONST)
	{
	  addr = XEXP (addr, 0);
	  gcc_assert (GET_CODE (addr) == PLUS);
	}

      if (XEXP (addr, 0) == picreg)
	return orig;

      if (reg == 0)
	{
	  gcc_assert (can_create_pseudo_p ());
	  reg = gen_reg_rtx (Pmode);
	}

      base = legitimize_pic_address (XEXP (addr, 0), reg, picreg);
      addr = legitimize_pic_address (XEXP (addr, 1),
				     base == reg ? NULL_RTX : reg,
				     picreg);

      if (GET_CODE (addr) == CONST_INT)
	{
	  gcc_assert (! reload_in_progress && ! reload_completed);
	  addr = force_reg (Pmode, addr);
	}

      if (GET_CODE (addr) == PLUS && CONSTANT_P (XEXP (addr, 1)))
	{
	  base = gen_rtx_PLUS (Pmode, base, XEXP (addr, 0));
	  addr = XEXP (addr, 1);
	}

      return gen_rtx_PLUS (Pmode, base, addr);
    }

  return new_rtx;
}

/* Stack frame layout. */

/* For a given REGNO, determine whether it must be saved in the function
   prologue.  IS_INTHANDLER specifies whether we're generating a normal
   prologue or an interrupt/exception one.  */
static bool
must_save_p (bool is_inthandler, unsigned regno)
{
  if (D_REGNO_P (regno))
    {
      bool is_eh_return_reg = false;
      if (crtl->calls_eh_return)
	{
	  unsigned j;
	  for (j = 0; ; j++)
	    {
	      unsigned test = EH_RETURN_DATA_REGNO (j);
	      if (test == INVALID_REGNUM)
		break;
	      if (test == regno)
		is_eh_return_reg = true;
	    }
	}

      return (is_eh_return_reg
	      || (df_regs_ever_live_p (regno)
		  && !fixed_regs[regno]
		  && (is_inthandler || !call_used_regs[regno])));
    }
  else if (P_REGNO_P (regno))
    {
      return ((df_regs_ever_live_p (regno)
	       && !fixed_regs[regno]
	       && (is_inthandler || !call_used_regs[regno]))
	      || (is_inthandler
		  && (ENABLE_WA_05000283 || ENABLE_WA_05000315)
		  && regno == REG_P5)
	      || (!TARGET_FDPIC
		  && regno == PIC_OFFSET_TABLE_REGNUM
		  && (crtl->uses_pic_offset_table
		      || (TARGET_ID_SHARED_LIBRARY && !crtl->is_leaf))));
    }
  else
    return ((is_inthandler || !call_used_regs[regno])
	    && (df_regs_ever_live_p (regno)
		|| (!leaf_function_p () && call_used_regs[regno])));

}

/* Compute the number of DREGS to save with a push_multiple operation.
   This could include registers that aren't modified in the function,
   since push_multiple only takes a range of registers.
   If IS_INTHANDLER, then everything that is live must be saved, even
   if normally call-clobbered.
   If CONSECUTIVE, return the number of registers we can save in one
   instruction with a push/pop multiple instruction.  */

static int
n_dregs_to_save (bool is_inthandler, bool consecutive)
{
  int count = 0;
  unsigned i;

  for (i = REG_R7 + 1; i-- != REG_R0;)
    {
      if (must_save_p (is_inthandler, i))
	count++;
      else if (consecutive)
	return count;
    }
  return count;
}

/* Like n_dregs_to_save, but compute number of PREGS to save.  */

static int
n_pregs_to_save (bool is_inthandler, bool consecutive)
{
  int count = 0;
  unsigned i;

  for (i = REG_P5 + 1; i-- != REG_P0;)
    if (must_save_p (is_inthandler, i))
      count++;
    else if (consecutive)
      return count;
  return count;
}

/* Determine if we are going to save the frame pointer in the prologue.  */

static bool
must_save_fp_p (void)
{
  return df_regs_ever_live_p (REG_FP);
}

/* Determine if we are going to save the RETS register.  */
static bool
must_save_rets_p (void)
{
  return df_regs_ever_live_p (REG_RETS);
}

static bool
stack_frame_needed_p (void)
{
  /* EH return puts a new return address into the frame using an
     address relative to the frame pointer.  */
  if (crtl->calls_eh_return)
    return true;
  return frame_pointer_needed;
}

/* Emit code to save registers in the prologue.  SAVEALL is nonzero if we
   must save all registers; this is used for interrupt handlers.
   SPREG contains (reg:SI REG_SP).  IS_INTHANDLER is true if we're doing
   this for an interrupt (or exception) handler.  */

static void
expand_prologue_reg_save (rtx spreg, int saveall, bool is_inthandler)
{
  rtx predec1 = gen_rtx_PRE_DEC (SImode, spreg);
  rtx predec = gen_rtx_MEM (SImode, predec1);
  int ndregs = saveall ? 8 : n_dregs_to_save (is_inthandler, false);
  int npregs = saveall ? 6 : n_pregs_to_save (is_inthandler, false);
  int ndregs_consec = saveall ? 8 : n_dregs_to_save (is_inthandler, true);
  int npregs_consec = saveall ? 6 : n_pregs_to_save (is_inthandler, true);
  int dregno, pregno;
  int total_consec = ndregs_consec + npregs_consec;
  int i, d_to_save;

  if (saveall || is_inthandler)
    {
      rtx_insn *insn = emit_move_insn (predec, gen_rtx_REG (SImode, REG_ASTAT));

      RTX_FRAME_RELATED_P (insn) = 1;
      for (dregno = REG_LT0; dregno <= REG_LB1; dregno++)
	if (! crtl->is_leaf
	    || cfun->machine->has_hardware_loops
	    || cfun->machine->has_loopreg_clobber
	    || (ENABLE_WA_05000257
		&& (dregno == REG_LC0 || dregno == REG_LC1)))
	  {
	    insn = emit_move_insn (predec, gen_rtx_REG (SImode, dregno));
	    RTX_FRAME_RELATED_P (insn) = 1;
	  }
    }

  if (total_consec != 0)
    {
      rtx_insn *insn;
      rtx val = GEN_INT (-total_consec * 4);
      rtx pat = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (total_consec + 2));

      XVECEXP (pat, 0, 0) = gen_rtx_UNSPEC (VOIDmode, gen_rtvec (1, val),
					    UNSPEC_PUSH_MULTIPLE);
      XVECEXP (pat, 0, total_consec + 1) = gen_rtx_SET (spreg,
							gen_rtx_PLUS (Pmode,
								      spreg,
								      val));
      RTX_FRAME_RELATED_P (XVECEXP (pat, 0, total_consec + 1)) = 1;
      d_to_save = ndregs_consec;
      dregno = REG_R7 + 1 - ndregs_consec;
      pregno = REG_P5 + 1 - npregs_consec;
      for (i = 0; i < total_consec; i++)
	{
	  rtx memref = gen_rtx_MEM (word_mode,
				    gen_rtx_PLUS (Pmode, spreg,
						  GEN_INT (- i * 4 - 4)));
	  rtx subpat;
	  if (d_to_save > 0)
	    {
	      subpat = gen_rtx_SET (memref, gen_rtx_REG (word_mode, dregno++));
	      d_to_save--;
	    }
	  else
	    {
	      subpat = gen_rtx_SET (memref, gen_rtx_REG (word_mode, pregno++));
	    }
	  XVECEXP (pat, 0, i + 1) = subpat;
	  RTX_FRAME_RELATED_P (subpat) = 1;
	}
      insn = emit_insn (pat);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  for (dregno = REG_R0; ndregs != ndregs_consec; dregno++)
    {
      if (must_save_p (is_inthandler, dregno))
	{
	  rtx_insn *insn =
	    emit_move_insn (predec, gen_rtx_REG (word_mode, dregno));
	  RTX_FRAME_RELATED_P (insn) = 1;
	  ndregs--;
	}
    }
  for (pregno = REG_P0; npregs != npregs_consec; pregno++)
    {
      if (must_save_p (is_inthandler, pregno))
	{
	  rtx_insn *insn =
	    emit_move_insn (predec, gen_rtx_REG (word_mode, pregno));
	  RTX_FRAME_RELATED_P (insn) = 1;
	  npregs--;
	}
    }
  for (i = REG_P7 + 1; i < REG_CC; i++)
    if (saveall 
	|| (is_inthandler
	    && (df_regs_ever_live_p (i)
		|| (!leaf_function_p () && call_used_regs[i]))))
      {
	rtx_insn *insn;
	if (i == REG_A0 || i == REG_A1)
	  insn = emit_move_insn (gen_rtx_MEM (PDImode, predec1),
				 gen_rtx_REG (PDImode, i));
	else
	  insn = emit_move_insn (predec, gen_rtx_REG (SImode, i));
	RTX_FRAME_RELATED_P (insn) = 1;
      }
}

/* Emit code to restore registers in the epilogue.  SAVEALL is nonzero if we
   must save all registers; this is used for interrupt handlers.
   SPREG contains (reg:SI REG_SP).  IS_INTHANDLER is true if we're doing
   this for an interrupt (or exception) handler.  */

static void
expand_epilogue_reg_restore (rtx spreg, bool saveall, bool is_inthandler)
{
  rtx postinc1 = gen_rtx_POST_INC (SImode, spreg);
  rtx postinc = gen_rtx_MEM (SImode, postinc1);

  int ndregs = saveall ? 8 : n_dregs_to_save (is_inthandler, false);
  int npregs = saveall ? 6 : n_pregs_to_save (is_inthandler, false);
  int ndregs_consec = saveall ? 8 : n_dregs_to_save (is_inthandler, true);
  int npregs_consec = saveall ? 6 : n_pregs_to_save (is_inthandler, true);
  int total_consec = ndregs_consec + npregs_consec;
  int i, regno;
  rtx_insn *insn;

  /* A slightly crude technique to stop flow from trying to delete "dead"
     insns.  */
  MEM_VOLATILE_P (postinc) = 1;

  for (i = REG_CC - 1; i > REG_P7; i--)
    if (saveall
	|| (is_inthandler
	    && (df_regs_ever_live_p (i)
		|| (!leaf_function_p () && call_used_regs[i]))))
      {
	if (i == REG_A0 || i == REG_A1)
	  {
	    rtx mem = gen_rtx_MEM (PDImode, postinc1);
	    MEM_VOLATILE_P (mem) = 1;
	    emit_move_insn (gen_rtx_REG (PDImode, i), mem);
	  }
	else
	  emit_move_insn (gen_rtx_REG (SImode, i), postinc);
      }

  regno = REG_P5 - npregs_consec;
  for (; npregs != npregs_consec; regno--)
    {
      if (must_save_p (is_inthandler, regno))
	{
	  emit_move_insn (gen_rtx_REG (word_mode, regno), postinc);
	  npregs--;
	}
    }
  regno = REG_R7 - ndregs_consec;
  for (; ndregs != ndregs_consec; regno--)
    {
      if (must_save_p (is_inthandler, regno))
	{
	  emit_move_insn (gen_rtx_REG (word_mode, regno), postinc);
	  ndregs--;
	}
    }

  if (total_consec != 0)
    {
      rtx pat = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (total_consec + 1));
      XVECEXP (pat, 0, 0)
	= gen_rtx_SET (spreg, gen_rtx_PLUS (Pmode, spreg,
					    GEN_INT (total_consec * 4)));

      if (npregs_consec > 0)
	regno = REG_P5 + 1;
      else
	regno = REG_R7 + 1;

      for (i = 0; i < total_consec; i++)
	{
	  rtx addr = (i > 0
		      ? gen_rtx_PLUS (Pmode, spreg, GEN_INT (i * 4))
		      : spreg);
	  rtx memref = gen_rtx_MEM (word_mode, addr);

	  regno--;
	  XVECEXP (pat, 0, i + 1)
	    = gen_rtx_SET (gen_rtx_REG (word_mode, regno), memref);

	  if (npregs_consec > 0)
	    {
	      if (--npregs_consec == 0)
		regno = REG_R7 + 1;
	    }
	}

      insn = emit_insn (pat);
      RTX_FRAME_RELATED_P (insn) = 1;
    }
  if (saveall || is_inthandler)
    {
      for (regno = REG_LB1; regno >= REG_LT0; regno--)
	if (! crtl->is_leaf
	    || cfun->machine->has_hardware_loops
	    || cfun->machine->has_loopreg_clobber
	    || (ENABLE_WA_05000257 && (regno == REG_LC0 || regno == REG_LC1)))
	  emit_move_insn (gen_rtx_REG (SImode, regno), postinc);

      emit_move_insn (gen_rtx_REG (SImode, REG_ASTAT), postinc);
    }
}

/* Perform any needed actions needed for a function that is receiving a
   variable number of arguments.

   CUM is as above.

   MODE and TYPE are the mode and type of the current parameter.

   PRETEND_SIZE is a variable that should be set to the amount of stack
   that must be pushed by the prolog to pretend that our caller pushed
   it.

   Normally, this macro will push all remaining incoming registers on the
   stack and set PRETEND_SIZE to the length of the registers pushed.  

   Blackfin specific :
   - VDSP C compiler manual (our ABI) says that a variable args function
     should save the R0, R1 and R2 registers in the stack.
   - The caller will always leave space on the stack for the
     arguments that are passed in registers, so we dont have
     to leave any extra space.
   - now, the vastart pointer can access all arguments from the stack.  */

static void
setup_incoming_varargs (cumulative_args_t cum,
			machine_mode mode ATTRIBUTE_UNUSED,
			tree type ATTRIBUTE_UNUSED, int *pretend_size,
			int no_rtl)
{
  rtx mem;
  int i;

  if (no_rtl)
    return;

  /* The move for named arguments will be generated automatically by the
     compiler.  We need to generate the move rtx for the unnamed arguments
     if they are in the first 3 words.  We assume at least 1 named argument
     exists, so we never generate [ARGP] = R0 here.  */

  for (i = get_cumulative_args (cum)->words + 1; i < max_arg_registers; i++)
    {
      mem = gen_rtx_MEM (Pmode,
			 plus_constant (Pmode, arg_pointer_rtx,
					(i * UNITS_PER_WORD)));
      emit_move_insn (mem, gen_rtx_REG (Pmode, i));
    }

  *pretend_size = 0;
}

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms may
   be accessed via the stack pointer) in functions that seem suitable.  */

static bool
bfin_frame_pointer_required (void) 
{
  e_funkind fkind = funkind (TREE_TYPE (current_function_decl));

  if (fkind != SUBROUTINE)
    return true;

  /* We turn on -fomit-frame-pointer if -momit-leaf-frame-pointer is used,
     so we have to override it for non-leaf functions.  */
  if (TARGET_OMIT_LEAF_FRAME_POINTER && ! crtl->is_leaf)
    return true;

  return false;
}

/* Return the number of registers pushed during the prologue.  */

static int
n_regs_saved_by_prologue (void)
{
  e_funkind fkind = funkind (TREE_TYPE (current_function_decl));
  bool is_inthandler = fkind != SUBROUTINE;
  tree attrs = TYPE_ATTRIBUTES (TREE_TYPE (current_function_decl));
  bool all = (lookup_attribute ("saveall", attrs) != NULL_TREE
	      || (is_inthandler && !crtl->is_leaf));
  int ndregs = all ? 8 : n_dregs_to_save (is_inthandler, false);
  int npregs = all ? 6 : n_pregs_to_save (is_inthandler, false);
  int n = ndregs + npregs;
  int i;

  if (all || stack_frame_needed_p ())
    n += 2;
  else
    {
      if (must_save_fp_p ())
	n++;
      if (must_save_rets_p ())
	n++;
    }

  if (fkind != SUBROUTINE || all)
    {
      /* Increment once for ASTAT.  */
      n++;
      if (! crtl->is_leaf
	  || cfun->machine->has_hardware_loops
	  || cfun->machine->has_loopreg_clobber)
	{
	  n += 6;
	}
    }

  if (fkind != SUBROUTINE)
    {
      /* RETE/X/N.  */
      if (lookup_attribute ("nesting", attrs))
	n++;
    }

  for (i = REG_P7 + 1; i < REG_CC; i++)
    if (all
	|| (fkind != SUBROUTINE
	    && (df_regs_ever_live_p (i)
		|| (!leaf_function_p () && call_used_regs[i]))))
      n += i == REG_A0 || i == REG_A1 ? 2 : 1;

  return n;
}

/* Given FROM and TO register numbers, say whether this elimination is
   allowed.  Frame pointer elimination is automatically handled.

   All other eliminations are valid.  */

static bool
bfin_can_eliminate (const int from ATTRIBUTE_UNUSED, const int to)
{
  return (to == STACK_POINTER_REGNUM ? ! frame_pointer_needed : true);
}

/* Return the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */

HOST_WIDE_INT
bfin_initial_elimination_offset (int from, int to)
{
  HOST_WIDE_INT offset = 0;

  if (from == ARG_POINTER_REGNUM)
    offset = n_regs_saved_by_prologue () * 4;

  if (to == STACK_POINTER_REGNUM)
    {
      if (crtl->outgoing_args_size >= FIXED_STACK_AREA)
	offset += crtl->outgoing_args_size;
      else if (crtl->outgoing_args_size)
	offset += FIXED_STACK_AREA;

      offset += get_frame_size ();
    }

  return offset;
}

/* Emit code to load a constant CONSTANT into register REG; setting
   RTX_FRAME_RELATED_P on all insns we generate if RELATED is true.
   Make sure that the insns we generate need not be split.  */

static void
frame_related_constant_load (rtx reg, HOST_WIDE_INT constant, bool related)
{
  rtx_insn *insn;
  rtx cst = GEN_INT (constant);

  if (constant >= -32768 && constant < 65536)
    insn = emit_move_insn (reg, cst);
  else
    {
      /* We don't call split_load_immediate here, since dwarf2out.c can get
	 confused about some of the more clever sequences it can generate.  */
      insn = emit_insn (gen_movsi_high (reg, cst));
      if (related)
	RTX_FRAME_RELATED_P (insn) = 1;
      insn = emit_insn (gen_movsi_low (reg, reg, cst));
    }
  if (related)
    RTX_FRAME_RELATED_P (insn) = 1;
}

/* Generate efficient code to add a value to a P register.
   Set RTX_FRAME_RELATED_P on the generated insns if FRAME is nonzero.
   EPILOGUE_P is zero if this function is called for prologue,
   otherwise it's nonzero. And it's less than zero if this is for
   sibcall epilogue.  */

static void
add_to_reg (rtx reg, HOST_WIDE_INT value, int frame, int epilogue_p)
{
  if (value == 0)
    return;

  /* Choose whether to use a sequence using a temporary register, or
     a sequence with multiple adds.  We can add a signed 7-bit value
     in one instruction.  */
  if (value > 120 || value < -120)
    {
      rtx tmpreg;
      rtx tmpreg2;
      rtx_insn *insn;

      tmpreg2 = NULL_RTX;

      /* For prologue or normal epilogue, P1 can be safely used
	 as the temporary register. For sibcall epilogue, we try to find
	 a call used P register, which will be restored in epilogue.
	 If we cannot find such a P register, we have to use one I register
	 to help us.  */

      if (epilogue_p >= 0)
	tmpreg = gen_rtx_REG (SImode, REG_P1);
      else
	{
	  int i;
	  for (i = REG_P0; i <= REG_P5; i++)
	    if ((df_regs_ever_live_p (i) && ! call_used_regs[i])
		|| (!TARGET_FDPIC
		    && i == PIC_OFFSET_TABLE_REGNUM
		    && (crtl->uses_pic_offset_table
			|| (TARGET_ID_SHARED_LIBRARY
			    && ! crtl->is_leaf))))
	      break;
	  if (i <= REG_P5)
	    tmpreg = gen_rtx_REG (SImode, i);
	  else
	    {
	      tmpreg = gen_rtx_REG (SImode, REG_P1);
	      tmpreg2 = gen_rtx_REG (SImode, REG_I0);
	      emit_move_insn (tmpreg2, tmpreg);
	    }
	}

      if (frame)
	frame_related_constant_load (tmpreg, value, TRUE);
      else
	insn = emit_move_insn (tmpreg, GEN_INT (value));

      insn = emit_insn (gen_addsi3 (reg, reg, tmpreg));
      if (frame)
	RTX_FRAME_RELATED_P (insn) = 1;

      if (tmpreg2 != NULL_RTX)
	emit_move_insn (tmpreg, tmpreg2);
    }
  else
    do
      {
	int size = value;
	rtx_insn *insn;

	if (size > 60)
	  size = 60;
	else if (size < -60)
	  /* We could use -62, but that would leave the stack unaligned, so
	     it's no good.  */
	  size = -60;

	insn = emit_insn (gen_addsi3 (reg, reg, GEN_INT (size)));
	if (frame)
	  RTX_FRAME_RELATED_P (insn) = 1;
	value -= size;
      }
    while (value != 0);
}

/* Generate a LINK insn for a frame sized FRAME_SIZE.  If this constant
   is too large, generate a sequence of insns that has the same effect.
   SPREG contains (reg:SI REG_SP).  */

static void
emit_link_insn (rtx spreg, HOST_WIDE_INT frame_size)
{
  HOST_WIDE_INT link_size = frame_size;
  rtx_insn *insn;
  int i;

  if (link_size > 262140)
    link_size = 262140;

  /* Use a LINK insn with as big a constant as possible, then subtract
     any remaining size from the SP.  */
  insn = emit_insn (gen_link (GEN_INT (-8 - link_size)));
  RTX_FRAME_RELATED_P (insn) = 1;

  for (i = 0; i < XVECLEN (PATTERN (insn), 0); i++)
    {
      rtx set = XVECEXP (PATTERN (insn), 0, i);
      gcc_assert (GET_CODE (set) == SET);
      RTX_FRAME_RELATED_P (set) = 1;
    }

  frame_size -= link_size;

  if (frame_size > 0)
    {
      /* Must use a call-clobbered PREG that isn't the static chain.  */
      rtx tmpreg = gen_rtx_REG (Pmode, REG_P1);

      frame_related_constant_load (tmpreg, -frame_size, TRUE);
      insn = emit_insn (gen_addsi3 (spreg, spreg, tmpreg));
      RTX_FRAME_RELATED_P (insn) = 1;
    }
}

/* Return the number of bytes we must reserve for outgoing arguments
   in the current function's stack frame.  */

static HOST_WIDE_INT
arg_area_size (void)
{
  if (crtl->outgoing_args_size)
    {
      if (crtl->outgoing_args_size >= FIXED_STACK_AREA)
	return crtl->outgoing_args_size;
      else
	return FIXED_STACK_AREA;
    }
  return 0;
}

/* Save RETS and FP, and allocate a stack frame.  ALL is true if the
   function must save all its registers (true only for certain interrupt
   handlers).  */

static void
do_link (rtx spreg, HOST_WIDE_INT frame_size, bool all)
{
  frame_size += arg_area_size ();

  if (all
      || stack_frame_needed_p ()
      || (must_save_rets_p () && must_save_fp_p ()))
    emit_link_insn (spreg, frame_size);
  else
    {
      if (must_save_rets_p ())
	{
	  rtx pat = gen_movsi (gen_rtx_MEM (Pmode,
					    gen_rtx_PRE_DEC (Pmode, spreg)),
			       bfin_rets_rtx);
	  rtx_insn *insn = emit_insn (pat);
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
      if (must_save_fp_p ())
	{
	  rtx pat = gen_movsi (gen_rtx_MEM (Pmode,
					    gen_rtx_PRE_DEC (Pmode, spreg)),
			       gen_rtx_REG (Pmode, REG_FP));
	  rtx_insn *insn = emit_insn (pat);
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
      add_to_reg (spreg, -frame_size, 1, 0);
    }
}

/* Like do_link, but used for epilogues to deallocate the stack frame.
   EPILOGUE_P is zero if this function is called for prologue,
   otherwise it's nonzero. And it's less than zero if this is for
   sibcall epilogue.  */

static void
do_unlink (rtx spreg, HOST_WIDE_INT frame_size, bool all, int epilogue_p)
{
  frame_size += arg_area_size ();

  if (stack_frame_needed_p ())
    emit_insn (gen_unlink ());
  else 
    {
      rtx postinc = gen_rtx_MEM (Pmode, gen_rtx_POST_INC (Pmode, spreg));

      add_to_reg (spreg, frame_size, 0, epilogue_p);
      if (all || must_save_fp_p ())
	{
	  rtx fpreg = gen_rtx_REG (Pmode, REG_FP);
	  emit_move_insn (fpreg, postinc);
	  emit_use (fpreg);
	}
      if (all || must_save_rets_p ())
	{
	  emit_move_insn (bfin_rets_rtx, postinc);
	  emit_use (bfin_rets_rtx);
	}
    }
}

/* Generate a prologue suitable for a function of kind FKIND.  This is
   called for interrupt and exception handler prologues.
   SPREG contains (reg:SI REG_SP).  */

static void
expand_interrupt_handler_prologue (rtx spreg, e_funkind fkind, bool all)
{
  HOST_WIDE_INT frame_size = get_frame_size ();
  rtx predec1 = gen_rtx_PRE_DEC (SImode, spreg);
  rtx predec = gen_rtx_MEM (SImode, predec1);
  rtx_insn *insn;
  tree attrs = TYPE_ATTRIBUTES (TREE_TYPE (current_function_decl));
  tree kspisusp = lookup_attribute ("kspisusp", attrs);

  if (kspisusp)
    {
      insn = emit_move_insn (spreg, gen_rtx_REG (Pmode, REG_USP));
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  /* We need space on the stack in case we need to save the argument
     registers.  */
  if (fkind == EXCPT_HANDLER)
    {
      insn = emit_insn (gen_addsi3 (spreg, spreg, GEN_INT (-12)));
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  /* If we're calling other functions, they won't save their call-clobbered
     registers, so we must save everything here.  */
  if (!crtl->is_leaf)
    all = true;
  expand_prologue_reg_save (spreg, all, true);

  if (ENABLE_WA_05000283 || ENABLE_WA_05000315)
    {
      rtx chipid = GEN_INT (trunc_int_for_mode (0xFFC00014, SImode));
      rtx p5reg = gen_rtx_REG (Pmode, REG_P5);
      emit_insn (gen_movbi (bfin_cc_rtx, const1_rtx));
      emit_insn (gen_movsi_high (p5reg, chipid));
      emit_insn (gen_movsi_low (p5reg, p5reg, chipid));
      emit_insn (gen_dummy_load (p5reg, bfin_cc_rtx));
    }
  
  if (lookup_attribute ("nesting", attrs))
    {
      rtx srcreg = gen_rtx_REG (Pmode, ret_regs[fkind]);
      insn = emit_move_insn (predec, srcreg);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  do_link (spreg, frame_size, all);

  if (fkind == EXCPT_HANDLER)
    {
      rtx r0reg = gen_rtx_REG (SImode, REG_R0);
      rtx r1reg = gen_rtx_REG (SImode, REG_R1);
      rtx r2reg = gen_rtx_REG (SImode, REG_R2);

      emit_move_insn (r0reg, gen_rtx_REG (SImode, REG_SEQSTAT));
      emit_insn (gen_ashrsi3 (r0reg, r0reg, GEN_INT (26)));
      emit_insn (gen_ashlsi3 (r0reg, r0reg, GEN_INT (26)));
      emit_move_insn (r1reg, spreg);
      emit_move_insn (r2reg, gen_rtx_REG (Pmode, REG_FP));
      emit_insn (gen_addsi3 (r2reg, r2reg, GEN_INT (8)));
    }
}

/* Generate an epilogue suitable for a function of kind FKIND.  This is
   called for interrupt and exception handler epilogues.
   SPREG contains (reg:SI REG_SP).  */

static void
expand_interrupt_handler_epilogue (rtx spreg, e_funkind fkind, bool all)
{
  tree attrs = TYPE_ATTRIBUTES (TREE_TYPE (current_function_decl));
  rtx postinc1 = gen_rtx_POST_INC (SImode, spreg);
  rtx postinc = gen_rtx_MEM (SImode, postinc1);

  /* A slightly crude technique to stop flow from trying to delete "dead"
     insns.  */
  MEM_VOLATILE_P (postinc) = 1;

  do_unlink (spreg, get_frame_size (), all, 1);

  if (lookup_attribute ("nesting", attrs))
    {
      rtx srcreg = gen_rtx_REG (Pmode, ret_regs[fkind]);
      emit_move_insn (srcreg, postinc);
    }

  /* If we're calling other functions, they won't save their call-clobbered
     registers, so we must save (and restore) everything here.  */
  if (!crtl->is_leaf)
    all = true;

  expand_epilogue_reg_restore (spreg, all, true);

  /* Deallocate any space we left on the stack in case we needed to save the
     argument registers.  */
  if (fkind == EXCPT_HANDLER)
    emit_insn (gen_addsi3 (spreg, spreg, GEN_INT (12)));

  emit_jump_insn (gen_return_internal (gen_rtx_REG (Pmode, ret_regs[fkind])));
}

/* Used while emitting the prologue to generate code to load the correct value
   into the PIC register, which is passed in DEST.  */

static rtx
bfin_load_pic_reg (rtx dest)
{
  struct cgraph_local_info *i = NULL;
  rtx addr;
 
  i = cgraph_node::local_info (current_function_decl);
 
  /* Functions local to the translation unit don't need to reload the
     pic reg, since the caller always passes a usable one.  */
  if (i && i->local)
    return pic_offset_table_rtx;
      
  if (global_options_set.x_bfin_library_id)
    addr = plus_constant (Pmode, pic_offset_table_rtx,
			   -4 - bfin_library_id * 4);
  else
    addr = gen_rtx_PLUS (Pmode, pic_offset_table_rtx,
			 gen_rtx_UNSPEC (Pmode, gen_rtvec (1, const0_rtx),
					 UNSPEC_LIBRARY_OFFSET));
  emit_insn (gen_movsi (dest, gen_rtx_MEM (Pmode, addr)));
  return dest;
}

/* Generate RTL for the prologue of the current function.  */

void
bfin_expand_prologue (void)
{
  HOST_WIDE_INT frame_size = get_frame_size ();
  rtx spreg = gen_rtx_REG (Pmode, REG_SP);
  e_funkind fkind = funkind (TREE_TYPE (current_function_decl));
  rtx pic_reg_loaded = NULL_RTX;
  tree attrs = TYPE_ATTRIBUTES (TREE_TYPE (current_function_decl));
  bool all = lookup_attribute ("saveall", attrs) != NULL_TREE;

  if (flag_stack_usage_info)
    current_function_static_stack_size = frame_size;

  if (fkind != SUBROUTINE)
    {
      expand_interrupt_handler_prologue (spreg, fkind, all);
      return;
    }

  if (crtl->limit_stack
      || (TARGET_STACK_CHECK_L1
	  && !DECL_NO_LIMIT_STACK (current_function_decl)))
    {
      HOST_WIDE_INT offset
	= bfin_initial_elimination_offset (ARG_POINTER_REGNUM,
					   STACK_POINTER_REGNUM);
      rtx lim = crtl->limit_stack ? stack_limit_rtx : NULL_RTX;
      rtx tmp = gen_rtx_REG (Pmode, REG_R3);
      rtx p2reg = gen_rtx_REG (Pmode, REG_P2);

      emit_move_insn (tmp, p2reg);
      if (!lim)
	{
	  emit_move_insn (p2reg, gen_int_mode (0xFFB00000, SImode));
	  emit_move_insn (p2reg, gen_rtx_MEM (Pmode, p2reg));
	  lim = p2reg;
	}
      if (GET_CODE (lim) == SYMBOL_REF)
	{
	  if (TARGET_ID_SHARED_LIBRARY)
	    {
	      rtx p1reg = gen_rtx_REG (Pmode, REG_P1);
	      rtx val;
	      pic_reg_loaded = bfin_load_pic_reg (p2reg);
	      val = legitimize_pic_address (stack_limit_rtx, p1reg,
					    pic_reg_loaded);
	      emit_move_insn (p1reg, val);
	      frame_related_constant_load (p2reg, offset, FALSE);
	      emit_insn (gen_addsi3 (p2reg, p2reg, p1reg));
	      lim = p2reg;
	    }
	  else
	    {
	      rtx limit = plus_constant (Pmode, lim, offset);
	      emit_move_insn (p2reg, limit);
	      lim = p2reg;
	    }
	}
      else
	{
	  if (lim != p2reg)
	    emit_move_insn (p2reg, lim);
	  add_to_reg (p2reg, offset, 0, 0);
	  lim = p2reg;
	}
      emit_insn (gen_compare_lt (bfin_cc_rtx, spreg, lim));
      emit_insn (gen_trapifcc ());
      emit_move_insn (p2reg, tmp);
    }
  expand_prologue_reg_save (spreg, all, false);

  do_link (spreg, frame_size, all);

  if (TARGET_ID_SHARED_LIBRARY
      && !TARGET_SEP_DATA
      && (crtl->uses_pic_offset_table
	  || !crtl->is_leaf))
    bfin_load_pic_reg (pic_offset_table_rtx);
}

/* Generate RTL for the epilogue of the current function.  NEED_RETURN is zero
   if this is for a sibcall.  EH_RETURN is nonzero if we're expanding an
   eh_return pattern. SIBCALL_P is true if this is a sibcall epilogue,
   false otherwise.  */

void
bfin_expand_epilogue (int need_return, int eh_return, bool sibcall_p)
{
  rtx spreg = gen_rtx_REG (Pmode, REG_SP);
  e_funkind fkind = funkind (TREE_TYPE (current_function_decl));
  int e = sibcall_p ? -1 : 1;
  tree attrs = TYPE_ATTRIBUTES (TREE_TYPE (current_function_decl));
  bool all = lookup_attribute ("saveall", attrs) != NULL_TREE;

  if (fkind != SUBROUTINE)
    {
      expand_interrupt_handler_epilogue (spreg, fkind, all);
      return;
    }

  do_unlink (spreg, get_frame_size (), all, e);

  expand_epilogue_reg_restore (spreg, all, false);

  /* Omit the return insn if this is for a sibcall.  */
  if (! need_return)
    return;

  if (eh_return)
    emit_insn (gen_addsi3 (spreg, spreg, gen_rtx_REG (Pmode, REG_P2)));

  emit_jump_insn (gen_return_internal (gen_rtx_REG (Pmode, REG_RETS)));
}

/* Return nonzero if register OLD_REG can be renamed to register NEW_REG.  */

int
bfin_hard_regno_rename_ok (unsigned int old_reg ATTRIBUTE_UNUSED,
			   unsigned int new_reg)
{
  /* Interrupt functions can only use registers that have already been
     saved by the prologue, even if they would normally be
     call-clobbered.  */

  if (funkind (TREE_TYPE (current_function_decl)) != SUBROUTINE
      && !df_regs_ever_live_p (new_reg))
    return 0;

  return 1;
}

/* Implement TARGET_EXTRA_LIVE_ON_ENTRY.  */
static void
bfin_extra_live_on_entry (bitmap regs)
{
  if (TARGET_FDPIC)
    bitmap_set_bit (regs, FDPIC_REGNO);
}

/* Return the value of the return address for the frame COUNT steps up
   from the current frame, after the prologue.
   We punt for everything but the current frame by returning const0_rtx.  */

rtx
bfin_return_addr_rtx (int count)
{
  if (count != 0)
    return const0_rtx;

  return get_hard_reg_initial_val (Pmode, REG_RETS);
}

static rtx
bfin_delegitimize_address (rtx orig_x)
{
  rtx x = orig_x;

  if (GET_CODE (x) != MEM)
    return orig_x;

  x = XEXP (x, 0);
  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 1)) == UNSPEC
      && XINT (XEXP (x, 1), 1) == UNSPEC_MOVE_PIC
      && GET_CODE (XEXP (x, 0)) == REG
      && REGNO (XEXP (x, 0)) == PIC_OFFSET_TABLE_REGNUM)
    return XVECEXP (XEXP (x, 1), 0, 0);

  return orig_x;
}

/* This predicate is used to compute the length of a load/store insn.
   OP is a MEM rtx, we return nonzero if its addressing mode requires a
   32-bit instruction.  */

int
effective_address_32bit_p (rtx op, machine_mode mode) 
{
  HOST_WIDE_INT offset;

  mode = GET_MODE (op);
  op = XEXP (op, 0);

  if (GET_CODE (op) != PLUS)
    {
      gcc_assert (REG_P (op) || GET_CODE (op) == POST_INC
		  || GET_CODE (op) == PRE_DEC || GET_CODE (op) == POST_DEC);
      return 0;
    }

  if (GET_CODE (XEXP (op, 1)) == UNSPEC)
    return 1;

  offset = INTVAL (XEXP (op, 1));

  /* All byte loads use a 16-bit offset.  */
  if (GET_MODE_SIZE (mode) == 1)
    return 1;

  if (GET_MODE_SIZE (mode) == 4)
    {
      /* Frame pointer relative loads can use a negative offset, all others
	 are restricted to a small positive one.  */
      if (XEXP (op, 0) == frame_pointer_rtx)
	return offset < -128 || offset > 60;
      return offset < 0 || offset > 60;
    }

  /* Must be HImode now.  */
  return offset < 0 || offset > 30;
}

/* Returns true if X is a memory reference using an I register.  */
bool
bfin_dsp_memref_p (rtx x)
{
  if (! MEM_P (x))
    return false;
  x = XEXP (x, 0);
  if (GET_CODE (x) == POST_INC || GET_CODE (x) == PRE_INC
      || GET_CODE (x) == POST_DEC || GET_CODE (x) == PRE_DEC)
    x = XEXP (x, 0);
  return IREG_P (x);
}

/* Return cost of the memory address ADDR.
   All addressing modes are equally cheap on the Blackfin.  */

static int
bfin_address_cost (rtx addr ATTRIBUTE_UNUSED,
		   machine_mode mode ATTRIBUTE_UNUSED,
		   addr_space_t as ATTRIBUTE_UNUSED,
		   bool speed ATTRIBUTE_UNUSED)
{
  return 1;
}

/* Subroutine of print_operand; used to print a memory reference X to FILE.  */

void
print_address_operand (FILE *file, rtx x)
{
  switch (GET_CODE (x))
    {
    case PLUS:
      output_address (VOIDmode, XEXP (x, 0));
      fprintf (file, "+");
      output_address (VOIDmode, XEXP (x, 1));
      break;

    case PRE_DEC:
      fprintf (file, "--");
      output_address (VOIDmode, XEXP (x, 0));    
      break;
    case POST_INC:
      output_address (VOIDmode, XEXP (x, 0));
      fprintf (file, "++");
      break;
    case POST_DEC:
      output_address (VOIDmode, XEXP (x, 0));
      fprintf (file, "--");
      break;

    default:
      gcc_assert (GET_CODE (x) != MEM);
      print_operand (file, x, 0);
      break;
    }
}

/* Adding intp DImode support by Tony
 * -- Q: (low  word)
 * -- R: (high word)
 */

void
print_operand (FILE *file, rtx x, char code)
{
  machine_mode mode;

  if (code == '!')
    {
      if (GET_MODE (current_output_insn) == SImode)
	fprintf (file, " ||");
      else
	fprintf (file, ";");
      return;
    }

  mode = GET_MODE (x);

  switch (code)
    {
    case 'j':
      switch (GET_CODE (x))
	{
	case EQ:
	  fprintf (file, "e");
	  break;
	case NE:
	  fprintf (file, "ne");
	  break;
	case GT:
	  fprintf (file, "g");
	  break;
	case LT:
	  fprintf (file, "l");
	  break;
	case GE:
	  fprintf (file, "ge");
	  break;
	case LE:
	  fprintf (file, "le");
	  break;
	case GTU:
	  fprintf (file, "g");
	  break;
	case LTU:
	  fprintf (file, "l");
	  break;
	case GEU:
	  fprintf (file, "ge");
	  break;
	case LEU:
	  fprintf (file, "le");
	  break;
	default:
	  output_operand_lossage ("invalid %%j value");
	}
      break;
    
    case 'J':					 /* reverse logic */
      switch (GET_CODE(x))
	{
	case EQ:
	  fprintf (file, "ne");
	  break;
	case NE:
	  fprintf (file, "e");
	  break;
	case GT:
	  fprintf (file, "le");
	  break;
	case LT:
	  fprintf (file, "ge");
	  break;
	case GE:
	  fprintf (file, "l");
	  break;
	case LE:
	  fprintf (file, "g");
	  break;
	case GTU:
	  fprintf (file, "le");
	  break;
	case LTU:
	  fprintf (file, "ge");
	  break;
	case GEU:
	  fprintf (file, "l");
	  break;
	case LEU:
	  fprintf (file, "g");
	  break;
	default:
	  output_operand_lossage ("invalid %%J value");
	}
      break;

    default:
      switch (GET_CODE (x))
	{
	case REG:
	  if (code == 'h')
	    {
	      if (REGNO (x) < 32)
		fprintf (file, "%s", short_reg_names[REGNO (x)]);
	      else
		output_operand_lossage ("invalid operand for code '%c'", code);
	    }
	  else if (code == 'd')
	    {
	      if (REGNO (x) < 32)
		fprintf (file, "%s", high_reg_names[REGNO (x)]);
	      else
		output_operand_lossage ("invalid operand for code '%c'", code);
	    }
	  else if (code == 'w')
	    {
	      if (REGNO (x) == REG_A0 || REGNO (x) == REG_A1)
		fprintf (file, "%s.w", reg_names[REGNO (x)]);
	      else
		output_operand_lossage ("invalid operand for code '%c'", code);
	    }
	  else if (code == 'x')
	    {
	      if (REGNO (x) == REG_A0 || REGNO (x) == REG_A1)
		fprintf (file, "%s.x", reg_names[REGNO (x)]);
	      else
		output_operand_lossage ("invalid operand for code '%c'", code);
	    }
	  else if (code == 'v')
	    {
	      if (REGNO (x) == REG_A0)
		fprintf (file, "AV0");
	      else if (REGNO (x) == REG_A1)
		fprintf (file, "AV1");
	      else
		output_operand_lossage ("invalid operand for code '%c'", code);
	    }
	  else if (code == 'D')
	    {
	      if (D_REGNO_P (REGNO (x)))
		fprintf (file, "%s", dregs_pair_names[REGNO (x)]);
	      else
		output_operand_lossage ("invalid operand for code '%c'", code);
	    }
	  else if (code == 'H')
	    {
	      if ((mode == DImode || mode == DFmode) && REG_P (x))
		fprintf (file, "%s", reg_names[REGNO (x) + 1]);
	      else
		output_operand_lossage ("invalid operand for code '%c'", code);
	    }
	  else if (code == 'T')
	    {
	      if (D_REGNO_P (REGNO (x)))
		fprintf (file, "%s", byte_reg_names[REGNO (x)]);
	      else
		output_operand_lossage ("invalid operand for code '%c'", code);
	    }
	  else 
	    fprintf (file, "%s", reg_names[REGNO (x)]);
	  break;

	case MEM:
	  fputc ('[', file);
	  x = XEXP (x,0);
	  print_address_operand (file, x);
	  fputc (']', file);
	  break;

	case CONST_INT:
	  if (code == 'M')
	    {
	      switch (INTVAL (x))
		{
		case MACFLAG_NONE:
		  break;
		case MACFLAG_FU:
		  fputs ("(FU)", file);
		  break;
		case MACFLAG_T:
		  fputs ("(T)", file);
		  break;
		case MACFLAG_TFU:
		  fputs ("(TFU)", file);
		  break;
		case MACFLAG_W32:
		  fputs ("(W32)", file);
		  break;
		case MACFLAG_IS:
		  fputs ("(IS)", file);
		  break;
		case MACFLAG_IU:
		  fputs ("(IU)", file);
		  break;
		case MACFLAG_IH:
		  fputs ("(IH)", file);
		  break;
		case MACFLAG_M:
		  fputs ("(M)", file);
		  break;
		case MACFLAG_IS_M:
		  fputs ("(IS,M)", file);
		  break;
		case MACFLAG_ISS2:
		  fputs ("(ISS2)", file);
		  break;
		case MACFLAG_S2RND:
		  fputs ("(S2RND)", file);
		  break;
		default:
		  gcc_unreachable ();
		}
	      break;
	    }
	  else if (code == 'b')
	    {
	      if (INTVAL (x) == 0)
		fputs ("+=", file);
	      else if (INTVAL (x) == 1)
		fputs ("-=", file);
	      else
		gcc_unreachable ();
	      break;
	    }
	  /* Moves to half registers with d or h modifiers always use unsigned
	     constants.  */
	  else if (code == 'd')
	    x = GEN_INT ((INTVAL (x) >> 16) & 0xffff);
	  else if (code == 'h')
	    x = GEN_INT (INTVAL (x) & 0xffff);
	  else if (code == 'N')
	    x = GEN_INT (-INTVAL (x));
	  else if (code == 'X')
	    x = GEN_INT (exact_log2 (0xffffffff & INTVAL (x)));
	  else if (code == 'Y')
	    x = GEN_INT (exact_log2 (0xffffffff & ~INTVAL (x)));
	  else if (code == 'Z')
	    /* Used for LINK insns.  */
	    x = GEN_INT (-8 - INTVAL (x));

	  /* fall through */

	case SYMBOL_REF:
	  output_addr_const (file, x);
	  break;

	case CONST_DOUBLE:
	  output_operand_lossage ("invalid const_double operand");
	  break;

	case UNSPEC:
	  switch (XINT (x, 1))
	    {
	    case UNSPEC_MOVE_PIC:
	      output_addr_const (file, XVECEXP (x, 0, 0));
	      fprintf (file, "@GOT");
	      break;

	    case UNSPEC_MOVE_FDPIC:
	      output_addr_const (file, XVECEXP (x, 0, 0));
	      fprintf (file, "@GOT17M4");
	      break;

	    case UNSPEC_FUNCDESC_GOT17M4:
	      output_addr_const (file, XVECEXP (x, 0, 0));
	      fprintf (file, "@FUNCDESC_GOT17M4");
	      break;

	    case UNSPEC_LIBRARY_OFFSET:
	      fprintf (file, "_current_shared_library_p5_offset_");
	      break;

	    default:
	      gcc_unreachable ();
	    }
	  break;

	default:
	  output_addr_const (file, x);
	}
    }
}

/* Argument support functions.  */

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  
   VDSP C Compiler manual, our ABI says that
   first 3 words of arguments will use R0, R1 and R2.
*/

void
init_cumulative_args (CUMULATIVE_ARGS *cum, tree fntype,
		      rtx libname ATTRIBUTE_UNUSED)
{
  static CUMULATIVE_ARGS zero_cum;

  *cum = zero_cum;

  /* Set up the number of registers to use for passing arguments.  */

  cum->nregs = max_arg_registers;
  cum->arg_regs = arg_regs;

  cum->call_cookie = CALL_NORMAL;
  /* Check for a longcall attribute.  */
  if (fntype && lookup_attribute ("shortcall", TYPE_ATTRIBUTES (fntype)))
    cum->call_cookie |= CALL_SHORT;
  else if (fntype && lookup_attribute ("longcall", TYPE_ATTRIBUTES (fntype)))
    cum->call_cookie |= CALL_LONG;

  return;
}

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

static void
bfin_function_arg_advance (cumulative_args_t cum_v, machine_mode mode,
			   const_tree type, bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int count, bytes, words;

  bytes = (mode == BLKmode) ? int_size_in_bytes (type) : GET_MODE_SIZE (mode);
  words = (bytes + UNITS_PER_WORD - 1) / UNITS_PER_WORD;

  cum->words += words;
  cum->nregs -= words;

  if (cum->nregs <= 0)
    {
      cum->nregs = 0;
      cum->arg_regs = NULL;
    }
  else
    {
      for (count = 1; count <= words; count++)
        cum->arg_regs++;
    }

  return;
}

/* Define where to put the arguments to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   NAMED is nonzero if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis).  */

static rtx
bfin_function_arg (cumulative_args_t cum_v, machine_mode mode,
		   const_tree type, bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int bytes
    = (mode == BLKmode) ? int_size_in_bytes (type) : GET_MODE_SIZE (mode);

  if (mode == VOIDmode)
    /* Compute operand 2 of the call insn.  */
    return GEN_INT (cum->call_cookie);

  if (bytes == -1)
    return NULL_RTX;

  if (cum->nregs)
    return gen_rtx_REG (mode, *(cum->arg_regs));

  return NULL_RTX;
}

/* For an arg passed partly in registers and partly in memory,
   this is the number of bytes passed in registers.
   For args passed entirely in registers or entirely in memory, zero.

   Refer VDSP C Compiler manual, our ABI.
   First 3 words are in registers. So, if an argument is larger
   than the registers available, it will span the register and
   stack.   */

static int
bfin_arg_partial_bytes (cumulative_args_t cum, machine_mode mode,
			tree type ATTRIBUTE_UNUSED,
			bool named ATTRIBUTE_UNUSED)
{
  int bytes
    = (mode == BLKmode) ? int_size_in_bytes (type) : GET_MODE_SIZE (mode);
  int bytes_left = get_cumulative_args (cum)->nregs * UNITS_PER_WORD;
  
  if (bytes == -1)
    return 0;

  if (bytes_left == 0)
    return 0;
  if (bytes > bytes_left)
    return bytes_left;
  return 0;
}

/* Variable sized types are passed by reference.  */

static bool
bfin_pass_by_reference (cumulative_args_t cum ATTRIBUTE_UNUSED,
			machine_mode mode ATTRIBUTE_UNUSED,
			const_tree type, bool named ATTRIBUTE_UNUSED)
{
  return type && TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST;
}

/* Decide whether a type should be returned in memory (true)
   or in a register (false).  This is called by the macro
   TARGET_RETURN_IN_MEMORY.  */

static bool
bfin_return_in_memory (const_tree type, const_tree fntype ATTRIBUTE_UNUSED)
{
  int size = int_size_in_bytes (type);
  return size > 2 * UNITS_PER_WORD || size == -1;
}

/* Register in which address to store a structure value
   is passed to a function.  */
static rtx
bfin_struct_value_rtx (tree fntype ATTRIBUTE_UNUSED,
		      int incoming ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (Pmode, REG_P0);
}

/* Return true when register may be used to pass function parameters.  */

bool 
function_arg_regno_p (int n)
{
  int i;
  for (i = 0; arg_regs[i] != -1; i++)
    if (n == arg_regs[i])
      return true;
  return false;
}

/* Returns 1 if OP contains a symbol reference */

int
symbolic_reference_mentioned_p (rtx op)
{
  register const char *fmt;
  register int i;

  if (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == LABEL_REF)
    return 1;

  fmt = GET_RTX_FORMAT (GET_CODE (op));
  for (i = GET_RTX_LENGTH (GET_CODE (op)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  register int j;

	  for (j = XVECLEN (op, i) - 1; j >= 0; j--)
	    if (symbolic_reference_mentioned_p (XVECEXP (op, i, j)))
	      return 1;
	}

      else if (fmt[i] == 'e' && symbolic_reference_mentioned_p (XEXP (op, i)))
	return 1;
    }

  return 0;
}

/* Decide whether we can make a sibling call to a function.  DECL is the
   declaration of the function being targeted by the call and EXP is the
   CALL_EXPR representing the call.  */

static bool
bfin_function_ok_for_sibcall (tree decl ATTRIBUTE_UNUSED,
			      tree exp ATTRIBUTE_UNUSED)
{
  struct cgraph_local_info *this_func, *called_func;
  e_funkind fkind = funkind (TREE_TYPE (current_function_decl));
  if (fkind != SUBROUTINE)
    return false;
  if (!TARGET_ID_SHARED_LIBRARY || TARGET_SEP_DATA)
    return true;

  /* When compiling for ID shared libraries, can't sibcall a local function
     from a non-local function, because the local function thinks it does
     not need to reload P5 in the prologue, but the sibcall wil pop P5 in the
     sibcall epilogue, and we end up with the wrong value in P5.  */

  if (!decl)
    /* Not enough information.  */
    return false;
 
  this_func = cgraph_node::local_info (current_function_decl);
  called_func = cgraph_node::local_info (decl);
  if (!called_func)
    return false;
  return !called_func->local || this_func->local;
}

/* Write a template for a trampoline to F.  */

static void
bfin_asm_trampoline_template (FILE *f)
{
  if (TARGET_FDPIC)
    {
      fprintf (f, "\t.dd\t0x00000000\n");	/* 0 */
      fprintf (f, "\t.dd\t0x00000000\n");	/* 0 */
      fprintf (f, "\t.dd\t0x0000e109\n");	/* p1.l = fn low */
      fprintf (f, "\t.dd\t0x0000e149\n");	/* p1.h = fn high */
      fprintf (f, "\t.dd\t0x0000e10a\n");	/* p2.l = sc low */
      fprintf (f, "\t.dd\t0x0000e14a\n");	/* p2.h = sc high */
      fprintf (f, "\t.dw\t0xac4b\n");		/* p3 = [p1 + 4] */
      fprintf (f, "\t.dw\t0x9149\n");		/* p1 = [p1] */
      fprintf (f, "\t.dw\t0x0051\n");		/* jump (p1)*/
    }
  else
    {
      fprintf (f, "\t.dd\t0x0000e109\n");	/* p1.l = fn low */
      fprintf (f, "\t.dd\t0x0000e149\n");	/* p1.h = fn high */
      fprintf (f, "\t.dd\t0x0000e10a\n");	/* p2.l = sc low */
      fprintf (f, "\t.dd\t0x0000e14a\n");	/* p2.h = sc high */
      fprintf (f, "\t.dw\t0x0051\n");		/* jump (p1)*/
    }
}

/* Emit RTL insns to initialize the variable parts of a trampoline at
   M_TRAMP. FNDECL is the target function.  CHAIN_VALUE is an RTX for
   the static chain value for the function.  */

static void
bfin_trampoline_init (rtx m_tramp, tree fndecl, rtx chain_value)
{
  rtx t1 = copy_to_reg (XEXP (DECL_RTL (fndecl), 0));
  rtx t2 = copy_to_reg (chain_value);
  rtx mem;
  int i = 0;

  emit_block_move (m_tramp, assemble_trampoline_template (),
		   GEN_INT (TRAMPOLINE_SIZE), BLOCK_OP_NORMAL);

  if (TARGET_FDPIC)
    {
      rtx a = force_reg (Pmode, plus_constant (Pmode, XEXP (m_tramp, 0), 8));
      mem = adjust_address (m_tramp, Pmode, 0);
      emit_move_insn (mem, a);
      i = 8;
    }

  mem = adjust_address (m_tramp, HImode, i + 2);
  emit_move_insn (mem, gen_lowpart (HImode, t1));
  emit_insn (gen_ashrsi3 (t1, t1, GEN_INT (16)));
  mem = adjust_address (m_tramp, HImode, i + 6);
  emit_move_insn (mem, gen_lowpart (HImode, t1));

  mem = adjust_address (m_tramp, HImode, i + 10);
  emit_move_insn (mem, gen_lowpart (HImode, t2));
  emit_insn (gen_ashrsi3 (t2, t2, GEN_INT (16)));
  mem = adjust_address (m_tramp, HImode, i + 14);
  emit_move_insn (mem, gen_lowpart (HImode, t2));
}

/* Emit insns to move operands[1] into operands[0].  */

void
emit_pic_move (rtx *operands, machine_mode mode ATTRIBUTE_UNUSED)
{
  rtx temp = reload_in_progress ? operands[0] : gen_reg_rtx (Pmode);

  gcc_assert (!TARGET_FDPIC || !(reload_in_progress || reload_completed));
  if (GET_CODE (operands[0]) == MEM && SYMBOLIC_CONST (operands[1]))
    operands[1] = force_reg (SImode, operands[1]);
  else
    operands[1] = legitimize_pic_address (operands[1], temp,
					  TARGET_FDPIC ? OUR_FDPIC_REG
					  : pic_offset_table_rtx);
}

/* Expand a move operation in mode MODE.  The operands are in OPERANDS.
   Returns true if no further code must be generated, false if the caller
   should generate an insn to move OPERANDS[1] to OPERANDS[0].  */

bool
expand_move (rtx *operands, machine_mode mode)
{
  rtx op = operands[1];
  if ((TARGET_ID_SHARED_LIBRARY || TARGET_FDPIC)
      && SYMBOLIC_CONST (op))
    emit_pic_move (operands, mode);
  else if (mode == SImode && GET_CODE (op) == CONST
	   && GET_CODE (XEXP (op, 0)) == PLUS
	   && GET_CODE (XEXP (XEXP (op, 0), 0)) == SYMBOL_REF
	   && !targetm.legitimate_constant_p (mode, op))
    {
      rtx dest = operands[0];
      rtx op0, op1;
      gcc_assert (!reload_in_progress && !reload_completed);
      op = XEXP (op, 0);
      op0 = force_reg (mode, XEXP (op, 0));
      op1 = XEXP (op, 1);
      if (!insn_data[CODE_FOR_addsi3].operand[2].predicate (op1, mode))
	op1 = force_reg (mode, op1);
      if (GET_CODE (dest) == MEM)
	dest = gen_reg_rtx (mode);
      emit_insn (gen_addsi3 (dest, op0, op1));
      if (dest == operands[0])
	return true;
      operands[1] = dest;
    }
  /* Don't generate memory->memory or constant->memory moves, go through a
     register */
  else if ((reload_in_progress | reload_completed) == 0
	   && GET_CODE (operands[0]) == MEM
    	   && GET_CODE (operands[1]) != REG)
    operands[1] = force_reg (mode, operands[1]);
  return false;
}

/* Split one or more DImode RTL references into pairs of SImode
   references.  The RTL can be REG, offsettable MEM, integer constant, or
   CONST_DOUBLE.  "operands" is a pointer to an array of DImode RTL to
   split and "num" is its length.  lo_half and hi_half are output arrays
   that parallel "operands".  */

void
split_di (rtx operands[], int num, rtx lo_half[], rtx hi_half[])
{
  while (num--)
    {
      rtx op = operands[num];

      /* simplify_subreg refuse to split volatile memory addresses,
         but we still have to handle it.  */
      if (GET_CODE (op) == MEM)
	{
	  lo_half[num] = adjust_address (op, SImode, 0);
	  hi_half[num] = adjust_address (op, SImode, 4);
	}
      else
	{
	  lo_half[num] = simplify_gen_subreg (SImode, op,
					      GET_MODE (op) == VOIDmode
					      ? DImode : GET_MODE (op), 0);
	  hi_half[num] = simplify_gen_subreg (SImode, op,
					      GET_MODE (op) == VOIDmode
					      ? DImode : GET_MODE (op), 4);
	}
    }
}

bool
bfin_longcall_p (rtx op, int call_cookie)
{
  gcc_assert (GET_CODE (op) == SYMBOL_REF);
  if (SYMBOL_REF_WEAK (op))
    return 1;
  if (call_cookie & CALL_SHORT)
    return 0;
  if (call_cookie & CALL_LONG)
    return 1;
  if (TARGET_LONG_CALLS)
    return 1;
  return 0;
}

/* Expand a call instruction.  FNADDR is the call target, RETVAL the return value.
   COOKIE is a CONST_INT holding the call_cookie prepared init_cumulative_args.
   SIBCALL is nonzero if this is a sibling call.  */

void
bfin_expand_call (rtx retval, rtx fnaddr, rtx callarg1, rtx cookie, int sibcall)
{
  rtx use = NULL, call;
  rtx callee = XEXP (fnaddr, 0);
  int nelts = 3;
  rtx pat;
  rtx picreg = get_hard_reg_initial_val (SImode, FDPIC_REGNO);
  rtx retsreg = gen_rtx_REG (Pmode, REG_RETS);
  int n;

  /* In an untyped call, we can get NULL for operand 2.  */
  if (cookie == NULL_RTX)
    cookie = const0_rtx;

  /* Static functions and indirect calls don't need the pic register.  */
  if (!TARGET_FDPIC && flag_pic
      && GET_CODE (callee) == SYMBOL_REF
      && !SYMBOL_REF_LOCAL_P (callee))
    use_reg (&use, pic_offset_table_rtx);

  if (TARGET_FDPIC)
    {
      int caller_in_sram, callee_in_sram;

      /* 0 is not in sram, 1 is in L1 sram, 2 is in L2 sram.  */
      caller_in_sram = callee_in_sram = 0;

      if (lookup_attribute ("l1_text",
			    DECL_ATTRIBUTES (cfun->decl)) != NULL_TREE)
	caller_in_sram = 1;
      else if (lookup_attribute ("l2",
				 DECL_ATTRIBUTES (cfun->decl)) != NULL_TREE)
	caller_in_sram = 2;

      if (GET_CODE (callee) == SYMBOL_REF
	  && SYMBOL_REF_DECL (callee) && DECL_P (SYMBOL_REF_DECL (callee)))
	{
	  if (lookup_attribute
	      ("l1_text",
	       DECL_ATTRIBUTES (SYMBOL_REF_DECL (callee))) != NULL_TREE)
	    callee_in_sram = 1;
	  else if (lookup_attribute
		   ("l2",
		    DECL_ATTRIBUTES (SYMBOL_REF_DECL (callee))) != NULL_TREE)
	    callee_in_sram = 2;
	}

      if (GET_CODE (callee) != SYMBOL_REF
	  || bfin_longcall_p (callee, INTVAL (cookie))
	  || (GET_CODE (callee) == SYMBOL_REF
	      && !SYMBOL_REF_LOCAL_P (callee)
	      && TARGET_INLINE_PLT)
	  || caller_in_sram != callee_in_sram
	  || (caller_in_sram && callee_in_sram
	      && (GET_CODE (callee) != SYMBOL_REF
		  || !SYMBOL_REF_LOCAL_P (callee))))
	{
	  rtx addr = callee;
	  if (! address_operand (addr, Pmode))
	    addr = force_reg (Pmode, addr);

	  fnaddr = gen_reg_rtx (SImode);
	  emit_insn (gen_load_funcdescsi (fnaddr, addr));
	  fnaddr = gen_rtx_MEM (Pmode, fnaddr);

	  picreg = gen_reg_rtx (SImode);
	  emit_insn (gen_load_funcdescsi (picreg,
					  plus_constant (Pmode, addr, 4)));
	}

      nelts++;
    }
  else if ((!register_no_elim_operand (callee, Pmode)
	    && GET_CODE (callee) != SYMBOL_REF)
	   || (GET_CODE (callee) == SYMBOL_REF
	       && ((TARGET_ID_SHARED_LIBRARY && !TARGET_LEAF_ID_SHARED_LIBRARY)
		   || bfin_longcall_p (callee, INTVAL (cookie)))))
    {
      callee = copy_to_mode_reg (Pmode, callee);
      fnaddr = gen_rtx_MEM (Pmode, callee);
    }
  call = gen_rtx_CALL (VOIDmode, fnaddr, callarg1);

  if (retval)
    call = gen_rtx_SET (retval, call);

  pat = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (nelts));
  n = 0;
  XVECEXP (pat, 0, n++) = call;
  if (TARGET_FDPIC)
    XVECEXP (pat, 0, n++) = gen_rtx_USE (VOIDmode, picreg);
  XVECEXP (pat, 0, n++) = gen_rtx_USE (VOIDmode, cookie);
  if (sibcall)
    XVECEXP (pat, 0, n++) = ret_rtx;
  else
    XVECEXP (pat, 0, n++) = gen_rtx_CLOBBER (VOIDmode, retsreg);
  call = emit_call_insn (pat);
  if (use)
    CALL_INSN_FUNCTION_USAGE (call) = use;
}

/* Return 1 if hard register REGNO can hold a value of machine-mode MODE.  */

int
hard_regno_mode_ok (int regno, machine_mode mode)
{
  /* Allow only dregs to store value of mode HI or QI */
  enum reg_class rclass = REGNO_REG_CLASS (regno);

  if (mode == CCmode)
    return 0;

  if (mode == V2HImode)
    return D_REGNO_P (regno);
  if (rclass == CCREGS)
    return mode == BImode;
  if (mode == PDImode || mode == V2PDImode)
    return regno == REG_A0 || regno == REG_A1;

  /* Allow all normal 32-bit regs, except REG_M3, in case regclass ever comes
     up with a bad register class (such as ALL_REGS) for DImode.  */
  if (mode == DImode)
    return regno < REG_M3;

  if (mode == SImode
      && TEST_HARD_REG_BIT (reg_class_contents[PROLOGUE_REGS], regno))
    return 1;

  return TEST_HARD_REG_BIT (reg_class_contents[MOST_REGS], regno);
}

/* Implements target hook vector_mode_supported_p.  */

static bool
bfin_vector_mode_supported_p (machine_mode mode)
{
  return mode == V2HImode;
}

/* Worker function for TARGET_REGISTER_MOVE_COST.  */

static int
bfin_register_move_cost (machine_mode mode,
			 reg_class_t class1, reg_class_t class2)
{
  /* These need secondary reloads, so they're more expensive.  */
  if ((class1 == CCREGS && !reg_class_subset_p (class2, DREGS))
      || (class2 == CCREGS && !reg_class_subset_p (class1, DREGS)))
    return 4;

  /* If optimizing for size, always prefer reg-reg over reg-memory moves.  */
  if (optimize_size)
    return 2;

  if (GET_MODE_CLASS (mode) == MODE_INT)
    {
      /* Discourage trying to use the accumulators.  */
      if (TEST_HARD_REG_BIT (reg_class_contents[class1], REG_A0)
	  || TEST_HARD_REG_BIT (reg_class_contents[class1], REG_A1)
	  || TEST_HARD_REG_BIT (reg_class_contents[class2], REG_A0)
	  || TEST_HARD_REG_BIT (reg_class_contents[class2], REG_A1))
	return 20;
    }
  return 2;
}

/* Worker function for TARGET_MEMORY_MOVE_COST.

   ??? In theory L1 memory has single-cycle latency.  We should add a switch
   that tells the compiler whether we expect to use only L1 memory for the
   program; it'll make the costs more accurate.  */

static int
bfin_memory_move_cost (machine_mode mode ATTRIBUTE_UNUSED,
		       reg_class_t rclass,
		       bool in ATTRIBUTE_UNUSED)
{
  /* Make memory accesses slightly more expensive than any register-register
     move.  Also, penalize non-DP registers, since they need secondary
     reloads to load and store.  */
  if (! reg_class_subset_p (rclass, DPREGS))
    return 10;

  return 8;
}

/* Inform reload about cases where moving X with a mode MODE to a register in
   RCLASS requires an extra scratch register.  Return the class needed for the
   scratch register.  */

static reg_class_t
bfin_secondary_reload (bool in_p, rtx x, reg_class_t rclass_i,
		       machine_mode mode, secondary_reload_info *sri)
{
  /* If we have HImode or QImode, we can only use DREGS as secondary registers;
     in most other cases we can also use PREGS.  */
  enum reg_class default_class = GET_MODE_SIZE (mode) >= 4 ? DPREGS : DREGS;
  enum reg_class x_class = NO_REGS;
  enum rtx_code code = GET_CODE (x);
  enum reg_class rclass = (enum reg_class) rclass_i;

  if (code == SUBREG)
    x = SUBREG_REG (x), code = GET_CODE (x);
  if (REG_P (x))
    {
      int regno = REGNO (x);
      if (regno >= FIRST_PSEUDO_REGISTER)
	regno = reg_renumber[regno];

      if (regno == -1)
	code = MEM;
      else
	x_class = REGNO_REG_CLASS (regno);
    }

  /* We can be asked to reload (plus (FP) (large_constant)) into a DREG.
     This happens as a side effect of register elimination, and we need
     a scratch register to do it.  */
  if (fp_plus_const_operand (x, mode))
    {
      rtx op2 = XEXP (x, 1);
      int large_constant_p = ! satisfies_constraint_Ks7 (op2);

      if (rclass == PREGS || rclass == PREGS_CLOBBERED)
	return NO_REGS;
      /* If destination is a DREG, we can do this without a scratch register
	 if the constant is valid for an add instruction.  */
      if ((rclass == DREGS || rclass == DPREGS)
	  && ! large_constant_p)
	return NO_REGS;
      /* Reloading to anything other than a DREG?  Use a PREG scratch
	 register.  */
      sri->icode = CODE_FOR_reload_insi;
      return NO_REGS;
    }

  /* Data can usually be moved freely between registers of most classes.
     AREGS are an exception; they can only move to or from another register
     in AREGS or one in DREGS.  They can also be assigned the constant 0.  */
  if (x_class == AREGS || x_class == EVEN_AREGS || x_class == ODD_AREGS)
    return (rclass == DREGS || rclass == AREGS || rclass == EVEN_AREGS
	    || rclass == ODD_AREGS
	    ? NO_REGS : DREGS);

  if (rclass == AREGS || rclass == EVEN_AREGS || rclass == ODD_AREGS)
    {
      if (code == MEM)
	{
	  sri->icode = in_p ? CODE_FOR_reload_inpdi : CODE_FOR_reload_outpdi;
	  return NO_REGS;
	}

      if (x != const0_rtx && x_class != DREGS)
	{
	  return DREGS;
	}
      else
	return NO_REGS;
    }

  /* CCREGS can only be moved from/to DREGS.  */
  if (rclass == CCREGS && x_class != DREGS)
    return DREGS;
  if (x_class == CCREGS && rclass != DREGS)
    return DREGS;

  /* All registers other than AREGS can load arbitrary constants.  The only
     case that remains is MEM.  */
  if (code == MEM)
    if (! reg_class_subset_p (rclass, default_class))
      return default_class;

  return NO_REGS;
}

/* Implement TARGET_CLASS_LIKELY_SPILLED_P.  */

static bool
bfin_class_likely_spilled_p (reg_class_t rclass)
{
  switch (rclass)
    {
      case PREGS_CLOBBERED:
      case PROLOGUE_REGS:
      case P0REGS:
      case D0REGS:
      case D1REGS:
      case D2REGS:
      case CCREGS:
        return true;

      default:
        break;
    }

  return false;
}

static struct machine_function *
bfin_init_machine_status (void)
{
  return ggc_cleared_alloc<machine_function> ();
}

/* Implement the TARGET_OPTION_OVERRIDE hook.  */

static void
bfin_option_override (void)
{
  /* If processor type is not specified, enable all workarounds.  */
  if (bfin_cpu_type == BFIN_CPU_UNKNOWN)
    {
      int i;

      for (i = 0; bfin_cpus[i].name != NULL; i++)
	bfin_workarounds |= bfin_cpus[i].workarounds;

      bfin_si_revision = 0xffff;
    }

  if (bfin_csync_anomaly == 1)
    bfin_workarounds |= WA_SPECULATIVE_SYNCS;
  else if (bfin_csync_anomaly == 0)
    bfin_workarounds &= ~WA_SPECULATIVE_SYNCS;

  if (bfin_specld_anomaly == 1)
    bfin_workarounds |= WA_SPECULATIVE_LOADS;
  else if (bfin_specld_anomaly == 0)
    bfin_workarounds &= ~WA_SPECULATIVE_LOADS;

  if (TARGET_OMIT_LEAF_FRAME_POINTER)
    flag_omit_frame_pointer = 1;

#ifdef SUBTARGET_FDPIC_NOT_SUPPORTED
  if (TARGET_FDPIC)
    error ("-mfdpic is not supported, please use a bfin-linux-uclibc target");
#endif

  /* Library identification */
  if (global_options_set.x_bfin_library_id && ! TARGET_ID_SHARED_LIBRARY)
    error ("-mshared-library-id= specified without -mid-shared-library");

  if (stack_limit_rtx && TARGET_FDPIC)
    {
      warning (0, "-fstack-limit- options are ignored with -mfdpic; use -mstack-check-l1");
      stack_limit_rtx = NULL_RTX;
    }

  if (stack_limit_rtx && TARGET_STACK_CHECK_L1)
    error ("can%'t use multiple stack checking methods together");

  if (TARGET_ID_SHARED_LIBRARY && TARGET_FDPIC)
    error ("ID shared libraries and FD-PIC mode can%'t be used together");

  /* Don't allow the user to specify -mid-shared-library and -msep-data
     together, as it makes little sense from a user's point of view...  */
  if (TARGET_SEP_DATA && TARGET_ID_SHARED_LIBRARY)
    error ("cannot specify both -msep-data and -mid-shared-library");
  /* ... internally, however, it's nearly the same.  */
  if (TARGET_SEP_DATA)
    target_flags |= MASK_ID_SHARED_LIBRARY | MASK_LEAF_ID_SHARED_LIBRARY;

  if (TARGET_ID_SHARED_LIBRARY && flag_pic == 0)
    flag_pic = 1;

  /* There is no single unaligned SI op for PIC code.  Sometimes we
     need to use ".4byte" and sometimes we need to use ".picptr".
     See bfin_assemble_integer for details.  */
  if (TARGET_FDPIC)
    targetm.asm_out.unaligned_op.si = 0;

  /* Silently turn off flag_pic if not doing FDPIC or ID shared libraries,
     since we don't support it and it'll just break.  */
  if (flag_pic && !TARGET_FDPIC && !TARGET_ID_SHARED_LIBRARY)
    flag_pic = 0;

  if (TARGET_MULTICORE && bfin_cpu_type != BFIN_CPU_BF561)
    error ("-mmulticore can only be used with BF561");

  if (TARGET_COREA && !TARGET_MULTICORE)
    error ("-mcorea should be used with -mmulticore");

  if (TARGET_COREB && !TARGET_MULTICORE)
    error ("-mcoreb should be used with -mmulticore");

  if (TARGET_COREA && TARGET_COREB)
    error ("-mcorea and -mcoreb can%'t be used together");

  flag_schedule_insns = 0;

  init_machine_status = bfin_init_machine_status;
}

/* Return the destination address of BRANCH.
   We need to use this instead of get_attr_length, because the
   cbranch_with_nops pattern conservatively sets its length to 6, and
   we still prefer to use shorter sequences.  */

static int
branch_dest (rtx_insn *branch)
{
  rtx dest;
  int dest_uid;
  rtx pat = PATTERN (branch);
  if (GET_CODE (pat) == PARALLEL)
    pat = XVECEXP (pat, 0, 0);
  dest = SET_SRC (pat);
  if (GET_CODE (dest) == IF_THEN_ELSE)
    dest = XEXP (dest, 1);
  dest = XEXP (dest, 0);
  dest_uid = INSN_UID (dest);
  return INSN_ADDRESSES (dest_uid);
}

/* Return nonzero if INSN is annotated with a REG_BR_PROB note that indicates
   it's a branch that's predicted taken.  */

static int
cbranch_predicted_taken_p (rtx insn)
{
  rtx x = find_reg_note (insn, REG_BR_PROB, 0);

  if (x)
    {
      int pred_val = XINT (x, 0);

      return pred_val >= REG_BR_PROB_BASE / 2;
    }

  return 0;
}

/* Templates for use by asm_conditional_branch.  */

static const char *ccbranch_templates[][3] = {
  { "if !cc jump %3;",  "if cc jump 4 (bp); jump.s %3;",  "if cc jump 6 (bp); jump.l %3;" },
  { "if cc jump %3;",   "if !cc jump 4 (bp); jump.s %3;", "if !cc jump 6 (bp); jump.l %3;" },
  { "if !cc jump %3 (bp);",  "if cc jump 4; jump.s %3;",  "if cc jump 6; jump.l %3;" },
  { "if cc jump %3 (bp);",  "if !cc jump 4; jump.s %3;",  "if !cc jump 6; jump.l %3;" },
};

/* Output INSN, which is a conditional branch instruction with operands
   OPERANDS.

   We deal with the various forms of conditional branches that can be generated
   by bfin_reorg to prevent the hardware from doing speculative loads, by
   - emitting a sufficient number of nops, if N_NOPS is nonzero, or
   - always emitting the branch as predicted taken, if PREDICT_TAKEN is true.
   Either of these is only necessary if the branch is short, otherwise the
   template we use ends in an unconditional jump which flushes the pipeline
   anyway.  */

void
asm_conditional_branch (rtx_insn *insn, rtx *operands, int n_nops, int predict_taken)
{
  int offset = branch_dest (insn) - INSN_ADDRESSES (INSN_UID (insn));
  /* Note : offset for instructions like if cc jmp; jump.[sl] offset
            is to be taken from start of if cc rather than jump.
            Range for jump.s is (-4094, 4096) instead of (-4096, 4094)
  */
  int len = (offset >= -1024 && offset <= 1022 ? 0
	     : offset >= -4094 && offset <= 4096 ? 1
	     : 2);
  int bp = predict_taken && len == 0 ? 1 : cbranch_predicted_taken_p (insn);
  int idx = (bp << 1) | (GET_CODE (operands[0]) == EQ ? BRF : BRT);
  output_asm_insn (ccbranch_templates[idx][len], operands);
  gcc_assert (n_nops == 0 || !bp);
  if (len == 0)
    while (n_nops-- > 0)
      output_asm_insn ("nop;", NULL);
}

/* Emit rtl for a comparison operation CMP in mode MODE.  Operands have been
   stored in bfin_compare_op0 and bfin_compare_op1 already.  */

rtx
bfin_gen_compare (rtx cmp, machine_mode mode ATTRIBUTE_UNUSED)
{
  enum rtx_code code1, code2;
  rtx op0 = XEXP (cmp, 0), op1 = XEXP (cmp, 1);
  rtx tem = bfin_cc_rtx;
  enum rtx_code code = GET_CODE (cmp);

  /* If we have a BImode input, then we already have a compare result, and
     do not need to emit another comparison.  */
  if (GET_MODE (op0) == BImode)
    {
      gcc_assert ((code == NE || code == EQ) && op1 == const0_rtx);
      tem = op0, code2 = code;
    }
  else
    {
      switch (code) {
	/* bfin has these conditions */
      case EQ:
      case LT:
      case LE:
      case LEU:
      case LTU:
	code1 = code;
	code2 = NE;
	break;
      default:
	code1 = reverse_condition (code);
	code2 = EQ;
	break;
      }
      emit_insn (gen_rtx_SET (tem, gen_rtx_fmt_ee (code1, BImode, op0, op1)));
    }

  return gen_rtx_fmt_ee (code2, BImode, tem, CONST0_RTX (BImode));
}

/* Return nonzero iff C has exactly one bit set if it is interpreted
   as a 32-bit constant.  */

int
log2constp (unsigned HOST_WIDE_INT c)
{
  c &= 0xFFFFFFFF;
  return c != 0 && (c & (c-1)) == 0;
}

/* Returns the number of consecutive least significant zeros in the binary
   representation of *V.
   We modify *V to contain the original value arithmetically shifted right by
   the number of zeroes.  */

static int
shiftr_zero (HOST_WIDE_INT *v)
{
  unsigned HOST_WIDE_INT tmp = *v;
  unsigned HOST_WIDE_INT sgn;
  int n = 0;

  if (tmp == 0)
    return 0;

  sgn = tmp & ((unsigned HOST_WIDE_INT) 1 << (HOST_BITS_PER_WIDE_INT - 1));
  while ((tmp & 0x1) == 0 && n <= 32)
    {
      tmp = (tmp >> 1) | sgn;
      n++;
    }
  *v = tmp;
  return n;
}

/* After reload, split the load of an immediate constant.  OPERANDS are the
   operands of the movsi_insn pattern which we are splitting.  We return
   nonzero if we emitted a sequence to load the constant, zero if we emitted
   nothing because we want to use the splitter's default sequence.  */

int
split_load_immediate (rtx operands[])
{
  HOST_WIDE_INT val = INTVAL (operands[1]);
  HOST_WIDE_INT tmp;
  HOST_WIDE_INT shifted = val;
  HOST_WIDE_INT shifted_compl = ~val;
  int num_zero = shiftr_zero (&shifted);
  int num_compl_zero = shiftr_zero (&shifted_compl);
  unsigned int regno = REGNO (operands[0]);

  /* This case takes care of single-bit set/clear constants, which we could
     also implement with BITSET/BITCLR.  */
  if (num_zero
      && shifted >= -32768 && shifted < 65536
      && (D_REGNO_P (regno)
	  || (regno >= REG_P0 && regno <= REG_P7 && num_zero <= 2)))
    {
      emit_insn (gen_movsi (operands[0], gen_int_mode (shifted, SImode)));
      emit_insn (gen_ashlsi3 (operands[0], operands[0], GEN_INT (num_zero)));
      return 1;
    }

  tmp = val & 0xFFFF;
  tmp |= -(tmp & 0x8000);

  /* If high word has one bit set or clear, try to use a bit operation.  */
  if (D_REGNO_P (regno))
    {
      if (log2constp (val & 0xFFFF0000))
	{
	  emit_insn (gen_movsi (operands[0], GEN_INT (val & 0xFFFF)));
	  emit_insn (gen_iorsi3 (operands[0], operands[0],
				 gen_int_mode (val & 0xFFFF0000, SImode)));
	  return 1;
	}
      else if (log2constp (val | 0xFFFF) && (val & 0x8000) != 0)
	{
	  emit_insn (gen_movsi (operands[0], GEN_INT (tmp)));
	  emit_insn (gen_andsi3 (operands[0], operands[0],
				 gen_int_mode (val | 0xFFFF, SImode)));
	}
    }

  if (D_REGNO_P (regno))
    {
      if (tmp >= -64 && tmp <= 63)
	{
	  emit_insn (gen_movsi (operands[0], GEN_INT (tmp)));
	  emit_insn (gen_movstricthi_high (operands[0],
					   gen_int_mode (val & -65536,
							 SImode)));
	  return 1;
	}

      if ((val & 0xFFFF0000) == 0)
	{
	  emit_insn (gen_movsi (operands[0], const0_rtx));
	  emit_insn (gen_movsi_low (operands[0], operands[0], operands[1]));
	  return 1;
	}

      if ((val & 0xFFFF0000) == 0xFFFF0000)
	{
	  emit_insn (gen_movsi (operands[0], constm1_rtx));
	  emit_insn (gen_movsi_low (operands[0], operands[0], operands[1]));
	  return 1;
	}
    }

  /* Need DREGs for the remaining case.  */
  if (regno > REG_R7)
    return 0;

  if (optimize_size
      && num_compl_zero && shifted_compl >= -64 && shifted_compl <= 63)
    {
      /* If optimizing for size, generate a sequence that has more instructions
	 but is shorter.  */
      emit_insn (gen_movsi (operands[0], gen_int_mode (shifted_compl, SImode)));
      emit_insn (gen_ashlsi3 (operands[0], operands[0],
			      GEN_INT (num_compl_zero)));
      emit_insn (gen_one_cmplsi2 (operands[0], operands[0]));
      return 1;
    }
  return 0;
}

/* Return true if the legitimate memory address for a memory operand of mode
   MODE.  Return false if not.  */

static bool
bfin_valid_add (machine_mode mode, HOST_WIDE_INT value)
{
  unsigned HOST_WIDE_INT v = value > 0 ? value : -value;
  int sz = GET_MODE_SIZE (mode);
  int shift = sz == 1 ? 0 : sz == 2 ? 1 : 2;
  /* The usual offsettable_memref machinery doesn't work so well for this
     port, so we deal with the problem here.  */
  if (value > 0 && sz == 8)
    v += 4;
  return (v & ~(0x7fff << shift)) == 0;
}

static bool
bfin_valid_reg_p (unsigned int regno, int strict, machine_mode mode,
		  enum rtx_code outer_code)
{
  if (strict)
    return REGNO_OK_FOR_BASE_STRICT_P (regno, mode, outer_code, SCRATCH);
  else
    return REGNO_OK_FOR_BASE_NONSTRICT_P (regno, mode, outer_code, SCRATCH);
}

/* Recognize an RTL expression that is a valid memory address for an
   instruction.  The MODE argument is the machine mode for the MEM expression
   that wants to use this address. 

   Blackfin addressing modes are as follows:

      [preg]
      [preg + imm16]

      B [ Preg + uimm15 ]
      W [ Preg + uimm16m2 ]
      [ Preg + uimm17m4 ] 

      [preg++]
      [preg--]
      [--sp]
*/

static bool
bfin_legitimate_address_p (machine_mode mode, rtx x, bool strict)
{
  switch (GET_CODE (x)) {
  case REG:
    if (bfin_valid_reg_p (REGNO (x), strict, mode, MEM))
      return true;
    break;
  case PLUS:
    if (REG_P (XEXP (x, 0))
	&& bfin_valid_reg_p (REGNO (XEXP (x, 0)), strict, mode, PLUS)
	&& ((GET_CODE (XEXP (x, 1)) == UNSPEC && mode == SImode)
	    || (GET_CODE (XEXP (x, 1)) == CONST_INT
		&& bfin_valid_add (mode, INTVAL (XEXP (x, 1))))))
      return true;
    break;
  case POST_INC:
  case POST_DEC:
    if (LEGITIMATE_MODE_FOR_AUTOINC_P (mode)
	&& REG_P (XEXP (x, 0))
	&& bfin_valid_reg_p (REGNO (XEXP (x, 0)), strict, mode, POST_INC))
      return true;
    break;
  case PRE_DEC:
    if (LEGITIMATE_MODE_FOR_AUTOINC_P (mode)
	&& XEXP (x, 0) == stack_pointer_rtx
	&& REG_P (XEXP (x, 0))
	&& bfin_valid_reg_p (REGNO (XEXP (x, 0)), strict, mode, PRE_DEC))
      return true;
    break;
  default:
    break;
  }
  return false;
}

/* Decide whether we can force certain constants to memory.  If we
   decide we can't, the caller should be able to cope with it in
   another way.  */

static bool
bfin_cannot_force_const_mem (machine_mode mode ATTRIBUTE_UNUSED,
			     rtx x ATTRIBUTE_UNUSED)
{
  /* We have only one class of non-legitimate constants, and our movsi
     expander knows how to handle them.  Dropping these constants into the
     data section would only shift the problem - we'd still get relocs
     outside the object, in the data section rather than the text section.  */
  return true;
}

/* Ensure that for any constant of the form symbol + offset, the offset
   remains within the object.  Any other constants are ok.
   This ensures that flat binaries never have to deal with relocations
   crossing section boundaries.  */

static bool
bfin_legitimate_constant_p (machine_mode mode ATTRIBUTE_UNUSED, rtx x)
{
  rtx sym;
  HOST_WIDE_INT offset;

  if (GET_CODE (x) != CONST)
    return true;

  x = XEXP (x, 0);
  gcc_assert (GET_CODE (x) == PLUS);

  sym = XEXP (x, 0);
  x = XEXP (x, 1);
  if (GET_CODE (sym) != SYMBOL_REF
      || GET_CODE (x) != CONST_INT)
    return true;
  offset = INTVAL (x);

  if (SYMBOL_REF_DECL (sym) == 0)
    return true;
  if (offset < 0
      || offset >= int_size_in_bytes (TREE_TYPE (SYMBOL_REF_DECL (sym))))
    return false;

  return true;
}

static bool
bfin_rtx_costs (rtx x, machine_mode mode, int outer_code_i, int opno,
		int *total, bool speed)
{
  enum rtx_code code = GET_CODE (x);
  enum rtx_code outer_code = (enum rtx_code) outer_code_i;
  int cost2 = COSTS_N_INSNS (1);
  rtx op0, op1;

  switch (code)
    {
    case CONST_INT:
      if (outer_code == SET || outer_code == PLUS)
        *total = satisfies_constraint_Ks7 (x) ? 0 : cost2;
      else if (outer_code == AND)
        *total = log2constp (~INTVAL (x)) ? 0 : cost2;
      else if (outer_code == LE || outer_code == LT || outer_code == EQ)
        *total = (INTVAL (x) >= -4 && INTVAL (x) <= 3) ? 0 : cost2;
      else if (outer_code == LEU || outer_code == LTU)
        *total = (INTVAL (x) >= 0 && INTVAL (x) <= 7) ? 0 : cost2;
      else if (outer_code == MULT)
        *total = (INTVAL (x) == 2 || INTVAL (x) == 4) ? 0 : cost2;
      else if (outer_code == ASHIFT && (INTVAL (x) == 1 || INTVAL (x) == 2))
        *total = 0;
      else if (outer_code == ASHIFT || outer_code == ASHIFTRT
	       || outer_code == LSHIFTRT)
        *total = (INTVAL (x) >= 0 && INTVAL (x) <= 31) ? 0 : cost2;
      else if (outer_code == IOR || outer_code == XOR)
        *total = (INTVAL (x) & (INTVAL (x) - 1)) == 0 ? 0 : cost2;
      else
	*total = cost2;
      return true;

    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST_DOUBLE:
      *total = COSTS_N_INSNS (2);
      return true;

    case PLUS:
      op0 = XEXP (x, 0);
      op1 = XEXP (x, 1);
      if (mode == SImode)
	{
	  if (GET_CODE (op0) == MULT
	      && GET_CODE (XEXP (op0, 1)) == CONST_INT)
	    {
	      HOST_WIDE_INT val = INTVAL (XEXP (op0, 1));
	      if (val == 2 || val == 4)
		{
		  *total = cost2;
		  *total += rtx_cost (XEXP (op0, 0), mode, outer_code,
				      opno, speed);
		  *total += rtx_cost (op1, mode, outer_code, opno, speed);
		  return true;
		}
	    }
	  *total = cost2;
	  if (GET_CODE (op0) != REG
	      && (GET_CODE (op0) != SUBREG || GET_CODE (SUBREG_REG (op0)) != REG))
	    *total += set_src_cost (op0, mode, speed);
#if 0 /* We'd like to do this for accuracy, but it biases the loop optimizer
	 towards creating too many induction variables.  */
	  if (!reg_or_7bit_operand (op1, SImode))
	    *total += set_src_cost (op1, mode, speed);
#endif
	}
      else if (mode == DImode)
	{
	  *total = 6 * cost2;
	  if (GET_CODE (op1) != CONST_INT
	      || !satisfies_constraint_Ks7 (op1))
	    *total += rtx_cost (op1, mode, PLUS, 1, speed);
	  if (GET_CODE (op0) != REG
	      && (GET_CODE (op0) != SUBREG || GET_CODE (SUBREG_REG (op0)) != REG))
	    *total += rtx_cost (op0, mode, PLUS, 0, speed);
	}
      return true;

    case MINUS:
      if (mode == DImode)
	*total = 6 * cost2;
      else
	*total = cost2;
      return true;
      
    case ASHIFT: 
    case ASHIFTRT:
    case LSHIFTRT:
      if (mode == DImode)
	*total = 6 * cost2;
      else
	*total = cost2;

      op0 = XEXP (x, 0);
      op1 = XEXP (x, 1);
      if (GET_CODE (op0) != REG
	  && (GET_CODE (op0) != SUBREG || GET_CODE (SUBREG_REG (op0)) != REG))
	*total += rtx_cost (op0, mode, code, 0, speed);

      return true;
	  
    case IOR:
    case AND:
    case XOR:
      op0 = XEXP (x, 0);
      op1 = XEXP (x, 1);

      /* Handle special cases of IOR: rotates, ALIGN insns, movstricthi_high.  */
      if (code == IOR)
	{
	  if ((GET_CODE (op0) == LSHIFTRT && GET_CODE (op1) == ASHIFT)
	      || (GET_CODE (op0) == ASHIFT && GET_CODE (op1) == ZERO_EXTEND)
	      || (GET_CODE (op0) == ASHIFT && GET_CODE (op1) == LSHIFTRT)
	      || (GET_CODE (op0) == AND && GET_CODE (op1) == CONST_INT))
	    {
	      *total = cost2;
	      return true;
	    }
	}

      if (GET_CODE (op0) != REG
	  && (GET_CODE (op0) != SUBREG || GET_CODE (SUBREG_REG (op0)) != REG))
	*total += rtx_cost (op0, mode, code, 0, speed);

      if (mode == DImode)
	{
	  *total = 2 * cost2;
	  return true;
	}
      *total = cost2;
      if (mode != SImode)
	return true;

      if (code == AND)
	{
	  if (! rhs_andsi3_operand (XEXP (x, 1), SImode))
	    *total += rtx_cost (XEXP (x, 1), mode, code, 1, speed);
	}
      else
	{
	  if (! regorlog2_operand (XEXP (x, 1), SImode))
	    *total += rtx_cost (XEXP (x, 1), mode, code, 1, speed);
	}

      return true;

    case ZERO_EXTRACT:
    case SIGN_EXTRACT:
      if (outer_code == SET
	  && XEXP (x, 1) == const1_rtx
	  && GET_CODE (XEXP (x, 2)) == CONST_INT)
	{
	  *total = 2 * cost2;
	  return true;
	}
      /* fall through */

    case SIGN_EXTEND:
    case ZERO_EXTEND:
      *total = cost2;
      return true;

    case MULT:
	{
	  op0 = XEXP (x, 0);
	  op1 = XEXP (x, 1);
	  if (GET_CODE (op0) == GET_CODE (op1)
	      && (GET_CODE (op0) == ZERO_EXTEND
		  || GET_CODE (op0) == SIGN_EXTEND))
	    {
	      *total = COSTS_N_INSNS (1);
	      op0 = XEXP (op0, 0);
	      op1 = XEXP (op1, 0);
	    }
	  else if (!speed)
	    *total = COSTS_N_INSNS (1);
	  else
	    *total = COSTS_N_INSNS (3);

	  if (GET_CODE (op0) != REG
	      && (GET_CODE (op0) != SUBREG || GET_CODE (SUBREG_REG (op0)) != REG))
	    *total += rtx_cost (op0, mode, MULT, 0, speed);
	  if (GET_CODE (op1) != REG
	      && (GET_CODE (op1) != SUBREG || GET_CODE (SUBREG_REG (op1)) != REG))
	    *total += rtx_cost (op1, mode, MULT, 1, speed);
	}
      return true;

    case UDIV:
    case UMOD:
      *total = COSTS_N_INSNS (32);
      return true;

    case VEC_CONCAT:
    case VEC_SELECT:
      if (outer_code == SET)
	*total = cost2;
      return true;

    default:
      return false;
    }
}

/* Used for communication between {push,pop}_multiple_operation (which
   we use not only as a predicate) and the corresponding output functions.  */
static int first_preg_to_save, first_dreg_to_save;
static int n_regs_to_save;

int
analyze_push_multiple_operation (rtx op)
{
  int lastdreg = 8, lastpreg = 6;
  int i, group;

  first_preg_to_save = lastpreg;
  first_dreg_to_save = lastdreg;
  for (i = 1, group = 0; i < XVECLEN (op, 0) - 1; i++)
    {
      rtx t = XVECEXP (op, 0, i);
      rtx src, dest;
      int regno;

      if (GET_CODE (t) != SET)
	return 0;

      src = SET_SRC (t);
      dest = SET_DEST (t);
      if (GET_CODE (dest) != MEM || ! REG_P (src))
	return 0;
      dest = XEXP (dest, 0);
      if (GET_CODE (dest) != PLUS
	  || ! REG_P (XEXP (dest, 0))
	  || REGNO (XEXP (dest, 0)) != REG_SP
	  || GET_CODE (XEXP (dest, 1)) != CONST_INT
	  || INTVAL (XEXP (dest, 1)) != -i * 4)
	return 0;

      regno = REGNO (src);
      if (group == 0)
	{
	  if (D_REGNO_P (regno))
	    {
	      group = 1;
	      first_dreg_to_save = lastdreg = regno - REG_R0;
	    }
	  else if (regno >= REG_P0 && regno <= REG_P7)
	    {
	      group = 2;
	      first_preg_to_save = lastpreg = regno - REG_P0;
	    }
	  else
	    return 0;

	  continue;
	}

      if (group == 1)
	{
	  if (regno >= REG_P0 && regno <= REG_P7)
	    {
	      group = 2;
	      first_preg_to_save = lastpreg = regno - REG_P0;
	    }
	  else if (regno != REG_R0 + lastdreg + 1)
	    return 0;
	  else
	    lastdreg++;
	}
      else if (group == 2)
	{
	  if (regno != REG_P0 + lastpreg + 1)
	    return 0;
	  lastpreg++;
	}
    }
  n_regs_to_save = 8 - first_dreg_to_save + 6 - first_preg_to_save;
  return 1;
}

int
analyze_pop_multiple_operation (rtx op)
{
  int lastdreg = 8, lastpreg = 6;
  int i, group;

  for (i = 1, group = 0; i < XVECLEN (op, 0); i++)
    {
      rtx t = XVECEXP (op, 0, i);
      rtx src, dest;
      int regno;

      if (GET_CODE (t) != SET)
	return 0;

      src = SET_SRC (t);
      dest = SET_DEST (t);
      if (GET_CODE (src) != MEM || ! REG_P (dest))
	return 0;
      src = XEXP (src, 0);

      if (i == 1)
	{
	  if (! REG_P (src) || REGNO (src) != REG_SP)
	    return 0;
	}
      else if (GET_CODE (src) != PLUS
	       || ! REG_P (XEXP (src, 0))
	       || REGNO (XEXP (src, 0)) != REG_SP
	       || GET_CODE (XEXP (src, 1)) != CONST_INT
	       || INTVAL (XEXP (src, 1)) != (i - 1) * 4)
	return 0;

      regno = REGNO (dest);
      if (group == 0)
	{
	  if (regno == REG_R7)
	    {
	      group = 1;
	      lastdreg = 7;
	    }
	  else if (regno != REG_P0 + lastpreg - 1)
	    return 0;
	  else
	    lastpreg--;
	}
      else if (group == 1)
	{
	  if (regno != REG_R0 + lastdreg - 1)
	    return 0;
	  else
	    lastdreg--;
	}
    }
  first_dreg_to_save = lastdreg;
  first_preg_to_save = lastpreg;
  n_regs_to_save = 8 - first_dreg_to_save + 6 - first_preg_to_save;
  return 1;
}

/* Emit assembly code for one multi-register push described by INSN, with
   operands in OPERANDS.  */

void
output_push_multiple (rtx insn, rtx *operands)
{
  char buf[80];
  int ok;
  
  /* Validate the insn again, and compute first_[dp]reg_to_save. */
  ok = analyze_push_multiple_operation (PATTERN (insn));
  gcc_assert (ok);
  
  if (first_dreg_to_save == 8)
    sprintf (buf, "[--sp] = ( p5:%d );\n", first_preg_to_save);
  else if (first_preg_to_save == 6)
    sprintf (buf, "[--sp] = ( r7:%d );\n", first_dreg_to_save);
  else
    sprintf (buf, "[--sp] = ( r7:%d, p5:%d );\n",
	     first_dreg_to_save, first_preg_to_save);

  output_asm_insn (buf, operands);
}

/* Emit assembly code for one multi-register pop described by INSN, with
   operands in OPERANDS.  */

void
output_pop_multiple (rtx insn, rtx *operands)
{
  char buf[80];
  int ok;
  
  /* Validate the insn again, and compute first_[dp]reg_to_save. */
  ok = analyze_pop_multiple_operation (PATTERN (insn));
  gcc_assert (ok);

  if (first_dreg_to_save == 8)
    sprintf (buf, "( p5:%d ) = [sp++];\n", first_preg_to_save);
  else if (first_preg_to_save == 6)
    sprintf (buf, "( r7:%d ) = [sp++];\n", first_dreg_to_save);
  else
    sprintf (buf, "( r7:%d, p5:%d ) = [sp++];\n",
	     first_dreg_to_save, first_preg_to_save);

  output_asm_insn (buf, operands);
}

/* Adjust DST and SRC by OFFSET bytes, and generate one move in mode MODE.  */

static void
single_move_for_movmem (rtx dst, rtx src, machine_mode mode, HOST_WIDE_INT offset)
{
  rtx scratch = gen_reg_rtx (mode);
  rtx srcmem, dstmem;

  srcmem = adjust_address_nv (src, mode, offset);
  dstmem = adjust_address_nv (dst, mode, offset);
  emit_move_insn (scratch, srcmem);
  emit_move_insn (dstmem, scratch);
}

/* Expand a string move operation of COUNT_EXP bytes from SRC to DST, with
   alignment ALIGN_EXP.  Return true if successful, false if we should fall
   back on a different method.  */

bool
bfin_expand_movmem (rtx dst, rtx src, rtx count_exp, rtx align_exp)
{
  rtx srcreg, destreg, countreg;
  HOST_WIDE_INT align = 0;
  unsigned HOST_WIDE_INT count = 0;

  if (GET_CODE (align_exp) == CONST_INT)
    align = INTVAL (align_exp);
  if (GET_CODE (count_exp) == CONST_INT)
    {
      count = INTVAL (count_exp);
#if 0
      if (!TARGET_INLINE_ALL_STRINGOPS && count > 64)
	return false;
#endif
    }

  /* If optimizing for size, only do single copies inline.  */
  if (optimize_size)
    {
      if (count == 2 && align < 2)
	return false;
      if (count == 4 && align < 4)
	return false;
      if (count != 1 && count != 2 && count != 4)
	return false;
    }
  if (align < 2 && count != 1)
    return false;

  destreg = copy_to_mode_reg (Pmode, XEXP (dst, 0));
  if (destreg != XEXP (dst, 0))
    dst = replace_equiv_address_nv (dst, destreg);
  srcreg = copy_to_mode_reg (Pmode, XEXP (src, 0));
  if (srcreg != XEXP (src, 0))
    src = replace_equiv_address_nv (src, srcreg);

  if (count != 0 && align >= 2)
    {
      unsigned HOST_WIDE_INT offset = 0;

      if (align >= 4)
	{
	  if ((count & ~3) == 4)
	    {
	      single_move_for_movmem (dst, src, SImode, offset);
	      offset = 4;
	    }
	  else if (count & ~3)
	    {
	      HOST_WIDE_INT new_count = ((count >> 2) & 0x3fffffff) - 1;
	      countreg = copy_to_mode_reg (Pmode, GEN_INT (new_count));

	      emit_insn (gen_rep_movsi (destreg, srcreg, countreg, destreg, srcreg));
	      cfun->machine->has_loopreg_clobber = true;
	    }
	  if (count & 2)
	    {
	      single_move_for_movmem (dst, src, HImode, offset);
	      offset += 2;
	    }
	}
      else
	{
	  if ((count & ~1) == 2)
	    {
	      single_move_for_movmem (dst, src, HImode, offset);
	      offset = 2;
	    }
	  else if (count & ~1)
	    {
	      HOST_WIDE_INT new_count = ((count >> 1) & 0x7fffffff) - 1;
	      countreg = copy_to_mode_reg (Pmode, GEN_INT (new_count));

	      emit_insn (gen_rep_movhi (destreg, srcreg, countreg, destreg, srcreg));
	      cfun->machine->has_loopreg_clobber = true;
	    }
	}
      if (count & 1)
	{
	  single_move_for_movmem (dst, src, QImode, offset);
	}
      return true;
    }
  return false;
}

/* Compute the alignment for a local variable.
   TYPE is the data type, and ALIGN is the alignment that
   the object would ordinarily have.  The value of this macro is used
   instead of that alignment to align the object.  */

unsigned
bfin_local_alignment (tree type, unsigned align)
{
  /* Increasing alignment for (relatively) big types allows the builtin
     memcpy can use 32 bit loads/stores.  */
  if (TYPE_SIZE (type)
      && TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST
      && wi::gtu_p (TYPE_SIZE (type), 8)
      && align < 32)
    return 32;
  return align;
}

/* Implement TARGET_SCHED_ISSUE_RATE.  */

static int
bfin_issue_rate (void)
{
  return 3;
}

static int
bfin_adjust_cost (rtx_insn *insn, int dep_type, rtx_insn *dep_insn, int cost,
		  unsigned int)
{
  enum attr_type dep_insn_type;
  int dep_insn_code_number;

  /* Anti and output dependencies have zero cost.  */
  if (dep_type != 0)
    return 0;

  dep_insn_code_number = recog_memoized (dep_insn);

  /* If we can't recognize the insns, we can't really do anything.  */
  if (dep_insn_code_number < 0 || recog_memoized (insn) < 0)
    return cost;

  dep_insn_type = get_attr_type (dep_insn);

  if (dep_insn_type == TYPE_MOVE || dep_insn_type == TYPE_MCLD)
    {
      rtx pat = PATTERN (dep_insn);
      rtx dest, src;

      if (GET_CODE (pat) == PARALLEL)
	pat = XVECEXP (pat, 0, 0);
      dest = SET_DEST (pat);
      src = SET_SRC (pat);
      if (! ADDRESS_REGNO_P (REGNO (dest))
	  || ! (MEM_P (src) || D_REGNO_P (REGNO (src))))
	return cost;
      return cost + (dep_insn_type == TYPE_MOVE ? 4 : 3);
    }

  return cost;
}

/* This function acts like NEXT_INSN, but is aware of three-insn bundles and
   skips all subsequent parallel instructions if INSN is the start of such
   a group.  */
static rtx_insn *
find_next_insn_start (rtx_insn *insn)
{
  if (GET_MODE (insn) == SImode)
    {
      while (GET_MODE (insn) != QImode)
	insn = NEXT_INSN (insn);
    }
  return NEXT_INSN (insn);
}

/* This function acts like PREV_INSN, but is aware of three-insn bundles and
   skips all subsequent parallel instructions if INSN is the start of such
   a group.  */
static rtx_insn *
find_prev_insn_start (rtx_insn *insn)
{
  insn = PREV_INSN (insn);
  gcc_assert (GET_MODE (insn) != SImode);
  if (GET_MODE (insn) == QImode)
    {
      while (GET_MODE (PREV_INSN (insn)) == SImode)
	insn = PREV_INSN (insn);
    }
  return insn;
}

/* Implement TARGET_CAN_USE_DOLOOP_P.  */

static bool
bfin_can_use_doloop_p (const widest_int &, const widest_int &iterations_max,
		       unsigned int, bool)
{
  /* Due to limitations in the hardware (an initial loop count of 0
     does not loop 2^32 times) we must avoid to generate a hardware
     loops when we cannot rule out this case.  */
  return (wi::ltu_p (iterations_max, 0xFFFFFFFF));
}

/* Increment the counter for the number of loop instructions in the
   current function.  */

void
bfin_hardware_loop (void)
{
  cfun->machine->has_hardware_loops++;
}

/* Maximum loop nesting depth.  */
#define MAX_LOOP_DEPTH 2

/* Maximum size of a loop.  */
#define MAX_LOOP_LENGTH 2042

/* Maximum distance of the LSETUP instruction from the loop start.  */
#define MAX_LSETUP_DISTANCE 30

/* Estimate the length of INSN conservatively.  */

static int
length_for_loop (rtx_insn *insn)
{
  int length = 0;
  if (JUMP_P (insn) && any_condjump_p (insn) && !optimize_size)
    {
      if (ENABLE_WA_SPECULATIVE_SYNCS)
	length = 8;
      else if (ENABLE_WA_SPECULATIVE_LOADS)
	length = 6;
    }
  else if (LABEL_P (insn))
    {
      if (ENABLE_WA_SPECULATIVE_SYNCS)
	length = 4;
    }

  if (NONDEBUG_INSN_P (insn))
    length += get_attr_length (insn);

  return length;
}

/* Optimize LOOP.  */

static bool
hwloop_optimize (hwloop_info loop)
{
  basic_block bb;
  rtx_insn *insn, *last_insn;
  rtx loop_init, start_label, end_label;
  rtx iter_reg, scratchreg, scratch_init;
  rtx_insn *scratch_init_insn;
  rtx lc_reg, lt_reg, lb_reg;
  rtx seq_end;
  rtx_insn *seq;
  int length;
  bool clobber0, clobber1;

  if (loop->depth > MAX_LOOP_DEPTH)
    {
      if (dump_file)
	fprintf (dump_file, ";; loop %d too deep\n", loop->loop_no);
      return false;
    }

  /* Get the loop iteration register.  */
  iter_reg = loop->iter_reg;

  gcc_assert (REG_P (iter_reg));

  scratchreg = NULL_RTX;
  scratch_init = iter_reg;
  scratch_init_insn = NULL;
  if (!PREG_P (iter_reg) && loop->incoming_src)
    {
      basic_block bb_in = loop->incoming_src;
      int i;
      for (i = REG_P0; i <= REG_P5; i++)
	if ((df_regs_ever_live_p (i)
	     || (funkind (TREE_TYPE (current_function_decl)) == SUBROUTINE
		 && call_used_regs[i]))
	    && !REGNO_REG_SET_P (df_get_live_out (bb_in), i))
	  {
	    scratchreg = gen_rtx_REG (SImode, i);
	    break;
	  }
      for (insn = BB_END (bb_in); insn != BB_HEAD (bb_in);
	   insn = PREV_INSN (insn))
	{
	  rtx set;
	  if (NOTE_P (insn) || BARRIER_P (insn))
	    continue;
	  set = single_set (insn);
	  if (set && rtx_equal_p (SET_DEST (set), iter_reg))
	    {
	      if (CONSTANT_P (SET_SRC (set)))
		{
		  scratch_init = SET_SRC (set);
		  scratch_init_insn = insn;
		}
	      break;
	    }
	  else if (reg_mentioned_p (iter_reg, PATTERN (insn)))
	    break;
	}
    }

  if (loop->incoming_src)
    {
      /* Make sure the predecessor is before the loop start label, as required by
	 the LSETUP instruction.  */
      length = 0;
      insn = BB_END (loop->incoming_src);
      /* If we have to insert the LSETUP before a jump, count that jump in the
	 length.  */
      if (vec_safe_length (loop->incoming) > 1
	  || !(loop->incoming->last ()->flags & EDGE_FALLTHRU))
	{
	  gcc_assert (JUMP_P (insn));
	  insn = PREV_INSN (insn);
	}

      for (; insn && insn != loop->start_label; insn = NEXT_INSN (insn))
	length += length_for_loop (insn);

      if (!insn)
	{
	  if (dump_file)
	    fprintf (dump_file, ";; loop %d lsetup not before loop_start\n",
		     loop->loop_no);
	  return false;
	}

      /* Account for the pop of a scratch register where necessary.  */
      if (!PREG_P (iter_reg) && scratchreg == NULL_RTX
	  && ENABLE_WA_LOAD_LCREGS)
	length += 2;

      if (length > MAX_LSETUP_DISTANCE)
	{
	  if (dump_file)
	    fprintf (dump_file, ";; loop %d lsetup too far away\n", loop->loop_no);
	  return false;
	}
    }

  /* Check if start_label appears before loop_end and calculate the
     offset between them.  We calculate the length of instructions
     conservatively.  */
  length = 0;
  for (insn = loop->start_label;
       insn && insn != loop->loop_end;
       insn = NEXT_INSN (insn))
    length += length_for_loop (insn);

  if (!insn)
    {
      if (dump_file)
	fprintf (dump_file, ";; loop %d start_label not before loop_end\n",
		 loop->loop_no);
      return false;
    }

  loop->length = length;
  if (loop->length > MAX_LOOP_LENGTH)
    {
      if (dump_file)
	fprintf (dump_file, ";; loop %d too long\n", loop->loop_no);
      return false;
    }

  /* Scan all the blocks to make sure they don't use iter_reg.  */
  if (loop->iter_reg_used || loop->iter_reg_used_outside)
    {
      if (dump_file)
	fprintf (dump_file, ";; loop %d uses iterator\n", loop->loop_no);
      return false;
    }

  clobber0 = (TEST_HARD_REG_BIT (loop->regs_set_in_loop, REG_LC0)
	      || TEST_HARD_REG_BIT (loop->regs_set_in_loop, REG_LB0)
	      || TEST_HARD_REG_BIT (loop->regs_set_in_loop, REG_LT0));
  clobber1 = (TEST_HARD_REG_BIT (loop->regs_set_in_loop, REG_LC1)
	      || TEST_HARD_REG_BIT (loop->regs_set_in_loop, REG_LB1)
	      || TEST_HARD_REG_BIT (loop->regs_set_in_loop, REG_LT1));
  if (clobber0 && clobber1)
    {
      if (dump_file)
	fprintf (dump_file, ";; loop %d no loop reg available\n",
		 loop->loop_no);
      return false;
    }

  /* There should be an instruction before the loop_end instruction
     in the same basic block. And the instruction must not be
     - JUMP
     - CONDITIONAL BRANCH
     - CALL
     - CSYNC
     - SSYNC
     - Returns (RTS, RTN, etc.)  */

  bb = loop->tail;
  last_insn = find_prev_insn_start (loop->loop_end);

  while (1)
    {
      for (; last_insn != BB_HEAD (bb);
	   last_insn = find_prev_insn_start (last_insn))
	if (NONDEBUG_INSN_P (last_insn))
	  break;

      if (last_insn != BB_HEAD (bb))
	break;

      if (single_pred_p (bb)
	  && single_pred_edge (bb)->flags & EDGE_FALLTHRU
	  && single_pred (bb) != ENTRY_BLOCK_PTR_FOR_FN (cfun))
	{
	  bb = single_pred (bb);
	  last_insn = BB_END (bb);
	  continue;
	}
      else
	{
	  last_insn = NULL;
	  break;
	}
    }

  if (!last_insn)
    {
      if (dump_file)
	fprintf (dump_file, ";; loop %d has no last instruction\n",
		 loop->loop_no);
      return false;
    }

  if (JUMP_P (last_insn) && !any_condjump_p (last_insn))
    {
      if (dump_file)
	fprintf (dump_file, ";; loop %d has bad last instruction\n",
		 loop->loop_no);
      return false;
    }
  /* In all other cases, try to replace a bad last insn with a nop.  */
  else if (JUMP_P (last_insn)
	   || CALL_P (last_insn)
	   || get_attr_type (last_insn) == TYPE_SYNC
	   || get_attr_type (last_insn) == TYPE_CALL
	   || get_attr_seq_insns (last_insn) == SEQ_INSNS_MULTI
	   || recog_memoized (last_insn) == CODE_FOR_return_internal
	   || GET_CODE (PATTERN (last_insn)) == ASM_INPUT
	   || asm_noperands (PATTERN (last_insn)) >= 0)
    {
      if (loop->length + 2 > MAX_LOOP_LENGTH)
	{
	  if (dump_file)
	    fprintf (dump_file, ";; loop %d too long\n", loop->loop_no);
	  return false;
	}
      if (dump_file)
	fprintf (dump_file, ";; loop %d has bad last insn; replace with nop\n",
		 loop->loop_no);

      last_insn = emit_insn_after (gen_forced_nop (), last_insn);
    }

  loop->last_insn = last_insn;

  /* The loop is good for replacement.  */
  start_label = loop->start_label;
  end_label = gen_label_rtx ();
  iter_reg = loop->iter_reg;

  if (loop->depth == 1 && !clobber1)
    {
      lc_reg = gen_rtx_REG (SImode, REG_LC1);
      lb_reg = gen_rtx_REG (SImode, REG_LB1);
      lt_reg = gen_rtx_REG (SImode, REG_LT1);
      SET_HARD_REG_BIT (loop->regs_set_in_loop, REG_LC1);
    }
  else
    {
      lc_reg = gen_rtx_REG (SImode, REG_LC0);
      lb_reg = gen_rtx_REG (SImode, REG_LB0);
      lt_reg = gen_rtx_REG (SImode, REG_LT0);
      SET_HARD_REG_BIT (loop->regs_set_in_loop, REG_LC0);
    }

  loop->end_label = end_label;

  /* Create a sequence containing the loop setup.  */
  start_sequence ();

  /* LSETUP only accepts P registers.  If we have one, we can use it,
     otherwise there are several ways of working around the problem.
     If we're not affected by anomaly 312, we can load the LC register
     from any iteration register, and use LSETUP without initialization.
     If we've found a P scratch register that's not live here, we can
     instead copy the iter_reg into that and use an initializing LSETUP.
     If all else fails, push and pop P0 and use it as a scratch.  */
  if (P_REGNO_P (REGNO (iter_reg)))
    {
      loop_init = gen_lsetup_with_autoinit (lt_reg, start_label,
					    lb_reg, end_label,
					    lc_reg, iter_reg);
      seq_end = emit_insn (loop_init);
    }
  else if (!ENABLE_WA_LOAD_LCREGS && DPREG_P (iter_reg))
    {
      emit_insn (gen_movsi (lc_reg, iter_reg));
      loop_init = gen_lsetup_without_autoinit (lt_reg, start_label,
					       lb_reg, end_label,
					       lc_reg);
      seq_end = emit_insn (loop_init);
    }
  else if (scratchreg != NULL_RTX)
    {
      emit_insn (gen_movsi (scratchreg, scratch_init));
      loop_init = gen_lsetup_with_autoinit (lt_reg, start_label,
					    lb_reg, end_label,
					    lc_reg, scratchreg);
      seq_end = emit_insn (loop_init);
      if (scratch_init_insn != NULL_RTX)
	delete_insn (scratch_init_insn);
    }
  else
    {
      rtx p0reg = gen_rtx_REG (SImode, REG_P0);
      rtx push = gen_frame_mem (SImode,
				gen_rtx_PRE_DEC (SImode, stack_pointer_rtx));
      rtx pop = gen_frame_mem (SImode,
			       gen_rtx_POST_INC (SImode, stack_pointer_rtx));
      emit_insn (gen_movsi (push, p0reg));
      emit_insn (gen_movsi (p0reg, scratch_init));
      loop_init = gen_lsetup_with_autoinit (lt_reg, start_label,
					    lb_reg, end_label,
					    lc_reg, p0reg);
      emit_insn (loop_init);
      seq_end = emit_insn (gen_movsi (p0reg, pop));
      if (scratch_init_insn != NULL_RTX)
	delete_insn (scratch_init_insn);
    }

  if (dump_file)
    {
      fprintf (dump_file, ";; replacing loop %d initializer with\n",
	       loop->loop_no);
      print_rtl_single (dump_file, loop_init);
      fprintf (dump_file, ";; replacing loop %d terminator with\n",
	       loop->loop_no);
      print_rtl_single (dump_file, loop->loop_end);
    }

  /* If the loop isn't entered at the top, also create a jump to the entry
     point.  */
  if (!loop->incoming_src && loop->head != loop->incoming_dest)
    {
      rtx label = BB_HEAD (loop->incoming_dest);
      /* If we're jumping to the final basic block in the loop, and there's
	 only one cheap instruction before the end (typically an increment of
	 an induction variable), we can just emit a copy here instead of a
	 jump.  */
      if (loop->incoming_dest == loop->tail
	  && next_real_insn (label) == last_insn
	  && asm_noperands (last_insn) < 0
	  && GET_CODE (PATTERN (last_insn)) == SET)
	{
	  seq_end = emit_insn (copy_rtx (PATTERN (last_insn)));
	}
      else
	{
	  rtx_insn *ret = emit_jump_insn (gen_jump (label));
	  JUMP_LABEL (ret) = label;
	  LABEL_NUSES (label)++;
	  seq_end = emit_barrier ();
	}
    }

  seq = get_insns ();
  end_sequence ();

  if (loop->incoming_src)
    {
      rtx_insn *prev = BB_END (loop->incoming_src);
      if (vec_safe_length (loop->incoming) > 1
	  || !(loop->incoming->last ()->flags & EDGE_FALLTHRU))
	{
	  gcc_assert (JUMP_P (prev));
	  prev = PREV_INSN (prev);
	  emit_insn_after (seq, prev);
	}
      else
	{
	  emit_insn_after (seq, prev);
	  BB_END (loop->incoming_src) = prev;
	  basic_block new_bb = create_basic_block (seq, seq_end,
						   loop->head->prev_bb);
	  edge e = loop->incoming->last ();
	  gcc_assert (e->flags & EDGE_FALLTHRU);
	  redirect_edge_succ (e, new_bb);
	  make_edge (new_bb, loop->head, 0);
	}
    }
  else
    {
      basic_block new_bb;
      edge e;
      edge_iterator ei;

      if (flag_checking && loop->head != loop->incoming_dest)
	{
	  /* We aren't entering the loop at the top.  Since we've established
	     that the loop is entered only at one point, this means there
	     can't be fallthru edges into the head.  Any such fallthru edges
	     would become invalid when we insert the new block, so verify
	     that this does not in fact happen.  */
	  FOR_EACH_EDGE (e, ei, loop->head->preds)
	    gcc_assert (!(e->flags & EDGE_FALLTHRU));
	}

      emit_insn_before (seq, BB_HEAD (loop->head));
      seq = emit_label_before (gen_label_rtx (), seq);

      new_bb = create_basic_block (seq, seq_end, loop->head->prev_bb);
      FOR_EACH_EDGE (e, ei, loop->incoming)
	{
	  if (!(e->flags & EDGE_FALLTHRU)
	      || e->dest != loop->head)
	    redirect_edge_and_branch_force (e, new_bb);
	  else
	    redirect_edge_succ (e, new_bb);
	}
      e = make_edge (new_bb, loop->head, 0);
    }

  delete_insn (loop->loop_end);
  /* Insert the loop end label before the last instruction of the loop.  */
  emit_label_before (as_a <rtx_code_label *> (loop->end_label),
		     loop->last_insn);

  return true;
}

/* A callback for the hw-doloop pass.  Called when a loop we have discovered
   turns out not to be optimizable; we have to split the doloop_end pattern
   into a subtract and a test.  */
static void
hwloop_fail (hwloop_info loop)
{
  rtx insn = loop->loop_end;
  
  if (DPREG_P (loop->iter_reg))
    {
      /* If loop->iter_reg is a DREG or PREG, we can split it here
	 without scratch register.  */
      rtx insn, test;

      emit_insn_before (gen_addsi3 (loop->iter_reg,
				    loop->iter_reg,
				    constm1_rtx),
			loop->loop_end);

      test = gen_rtx_NE (VOIDmode, loop->iter_reg, const0_rtx);
      insn = emit_jump_insn_before (gen_cbranchsi4 (test,
						    loop->iter_reg, const0_rtx,
						    loop->start_label),
				    loop->loop_end);

      JUMP_LABEL (insn) = loop->start_label;
      LABEL_NUSES (loop->start_label)++;
      delete_insn (loop->loop_end);
    }
  else
    {
      splitting_loops = 1;  
      try_split (PATTERN (insn), safe_as_a <rtx_insn *> (insn), 1);
      splitting_loops = 0;
    }
}

/* A callback for the hw-doloop pass.  This function examines INSN; if
   it is a loop_end pattern we recognize, return the reg rtx for the
   loop counter.  Otherwise, return NULL_RTX.  */

static rtx
hwloop_pattern_reg (rtx_insn *insn)
{
  rtx reg;

  if (!JUMP_P (insn) || recog_memoized (insn) != CODE_FOR_loop_end)
    return NULL_RTX;

  reg = SET_DEST (XVECEXP (PATTERN (insn), 0, 1));
  if (!REG_P (reg))
    return NULL_RTX;
  return reg;
}

static struct hw_doloop_hooks bfin_doloop_hooks =
{
  hwloop_pattern_reg,
  hwloop_optimize,
  hwloop_fail
};

/* Run from machine_dependent_reorg, this pass looks for doloop_end insns
   and tries to rewrite the RTL of these loops so that proper Blackfin
   hardware loops are generated.  */

static void
bfin_reorg_loops (void)
{
  reorg_loops (true, &bfin_doloop_hooks);
}

/* Possibly generate a SEQUENCE out of three insns found in SLOT.
   Returns true if we modified the insn chain, false otherwise.  */
static bool
gen_one_bundle (rtx_insn *slot[3])
{
  gcc_assert (slot[1] != NULL_RTX);

  /* Don't add extra NOPs if optimizing for size.  */
  if (optimize_size
      && (slot[0] == NULL_RTX || slot[2] == NULL_RTX))
    return false;

  /* Verify that we really can do the multi-issue.  */
  if (slot[0])
    {
      rtx_insn *t = NEXT_INSN (slot[0]);
      while (t != slot[1])
	{
	  if (! NOTE_P (t) || NOTE_KIND (t) != NOTE_INSN_DELETED)
	    return false;
	  t = NEXT_INSN (t);
	}
    }
  if (slot[2])
    {
      rtx_insn *t = NEXT_INSN (slot[1]);
      while (t != slot[2])
	{
	  if (! NOTE_P (t) || NOTE_KIND (t) != NOTE_INSN_DELETED)
	    return false;
	  t = NEXT_INSN (t);
	}
    }

  if (slot[0] == NULL_RTX)
    {
      slot[0] = emit_insn_before (gen_mnop (), slot[1]);
      df_insn_rescan (slot[0]);
    }
  if (slot[2] == NULL_RTX)
    {
      slot[2] = emit_insn_after (gen_forced_nop (), slot[1]);
      df_insn_rescan (slot[2]);
    }

  /* Avoid line number information being printed inside one bundle.  */
  if (INSN_LOCATION (slot[1])
      && INSN_LOCATION (slot[1]) != INSN_LOCATION (slot[0]))
    INSN_LOCATION (slot[1]) = INSN_LOCATION (slot[0]);
  if (INSN_LOCATION (slot[2])
      && INSN_LOCATION (slot[2]) != INSN_LOCATION (slot[0]))
    INSN_LOCATION (slot[2]) = INSN_LOCATION (slot[0]);

  /* Terminate them with "|| " instead of ";" in the output.  */
  PUT_MODE (slot[0], SImode);
  PUT_MODE (slot[1], SImode);
  /* Terminate the bundle, for the benefit of reorder_var_tracking_notes.  */
  PUT_MODE (slot[2], QImode);
  return true;
}

/* Go through all insns, and use the information generated during scheduling
   to generate SEQUENCEs to represent bundles of instructions issued
   simultaneously.  */

static void
bfin_gen_bundles (void)
{
  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    {
      rtx_insn *insn, *next;
      rtx_insn *slot[3];
      int n_filled = 0;

      slot[0] = slot[1] = slot[2] = NULL;
      for (insn = BB_HEAD (bb);; insn = next)
	{
	  int at_end;
	  rtx_insn *delete_this = NULL;

	  if (NONDEBUG_INSN_P (insn))
	    {
	      enum attr_type type = get_attr_type (insn);

	      if (type == TYPE_STALL)
		{
		  gcc_assert (n_filled == 0);
		  delete_this = insn;
		}
	      else
		{
		  if (type == TYPE_DSP32 || type == TYPE_DSP32SHIFTIMM)
		    slot[0] = insn;
		  else if (slot[1] == NULL_RTX)
		    slot[1] = insn;
		  else
		    slot[2] = insn;
		  n_filled++;
		}
	    }

	  next = NEXT_INSN (insn);
	  while (next && insn != BB_END (bb)
		 && !(INSN_P (next)
		      && GET_CODE (PATTERN (next)) != USE
		      && GET_CODE (PATTERN (next)) != CLOBBER))
	    {
	      insn = next;
	      next = NEXT_INSN (insn);
	    }

	  /* BB_END can change due to emitting extra NOPs, so check here.  */
	  at_end = insn == BB_END (bb);
	  if (delete_this == NULL_RTX && (at_end || GET_MODE (next) == TImode))
	    {
	      if ((n_filled < 2
		   || !gen_one_bundle (slot))
		  && slot[0] != NULL_RTX)
		{
		  rtx pat = PATTERN (slot[0]);
		  if (GET_CODE (pat) == SET
		      && GET_CODE (SET_SRC (pat)) == UNSPEC
		      && XINT (SET_SRC (pat), 1) == UNSPEC_32BIT)
		    {
		      SET_SRC (pat) = XVECEXP (SET_SRC (pat), 0, 0);
		      INSN_CODE (slot[0]) = -1;
		      df_insn_rescan (slot[0]);
		    }
		}
	      n_filled = 0;
	      slot[0] = slot[1] = slot[2] = NULL;
	    }
	  if (delete_this != NULL_RTX)
	    delete_insn (delete_this);
	  if (at_end)
	    break;
	}
    }
}

/* Ensure that no var tracking notes are emitted in the middle of a
   three-instruction bundle.  */

static void
reorder_var_tracking_notes (void)
{
  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    {
      rtx_insn *insn, *next;
      rtx_insn *queue = NULL;
      bool in_bundle = false;

      for (insn = BB_HEAD (bb); insn != BB_END (bb); insn = next)
	{
	  next = NEXT_INSN (insn);

	  if (INSN_P (insn))
	    {
	      /* Emit queued up notes at the last instruction of a bundle.  */
	      if (GET_MODE (insn) == QImode)
		{
		  while (queue)
		    {
		      rtx_insn *next_queue = PREV_INSN (queue);
		      SET_PREV_INSN (NEXT_INSN (insn)) = queue;
		      SET_NEXT_INSN (queue) = NEXT_INSN (insn);
		      SET_NEXT_INSN (insn) = queue;
		      SET_PREV_INSN (queue) = insn;
		      queue = next_queue;
		    }
		  in_bundle = false;
		}
	      else if (GET_MODE (insn) == SImode)
		in_bundle = true;
	    }
	  else if (NOTE_P (insn) && NOTE_KIND (insn) == NOTE_INSN_VAR_LOCATION)
	    {
	      if (in_bundle)
		{
		  rtx_insn *prev = PREV_INSN (insn);
		  SET_PREV_INSN (next) = prev;
		  SET_NEXT_INSN (prev) = next;

		  SET_PREV_INSN (insn) = queue;
		  queue = insn;
		}
	    }
	}
    }
}

/* On some silicon revisions, functions shorter than a certain number of cycles
   can cause unpredictable behavior.  Work around this by adding NOPs as
   needed.  */
static void
workaround_rts_anomaly (void)
{
  rtx_insn *insn, *first_insn = NULL;
  int cycles = 4;

  if (! ENABLE_WA_RETS)
    return;

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      rtx pat;

      if (BARRIER_P (insn))
	return;
      
      if (NOTE_P (insn) || LABEL_P (insn))
	continue;

      if (JUMP_TABLE_DATA_P (insn))
	continue;

      if (first_insn == NULL_RTX)
	first_insn = insn;
      pat = PATTERN (insn);
      if (GET_CODE (pat) == USE || GET_CODE (pat) == CLOBBER
	  || GET_CODE (pat) == ASM_INPUT
	  || asm_noperands (pat) >= 0)
	continue;

      if (CALL_P (insn))
	return;

      if (JUMP_P (insn))
	{
	  if (recog_memoized (insn) == CODE_FOR_return_internal)
	    break;

	  /* Nothing to worry about for direct jumps.  */
	  if (!any_condjump_p (insn))
	    return;
	  if (cycles <= 1)
	    return;
	  cycles--;
	}
      else if (INSN_P (insn))
	{
	  rtx pat = PATTERN (insn);
	  int this_cycles = 1;

	  if (GET_CODE (pat) == PARALLEL)
	    {
	      if (analyze_push_multiple_operation (pat)
		  || analyze_pop_multiple_operation (pat))
		this_cycles = n_regs_to_save;
	    }
	  else
	    {
	      int icode = recog_memoized (insn);

	      if (icode == CODE_FOR_link)
		this_cycles = 4;
	      else if (icode == CODE_FOR_unlink)
		this_cycles = 3;
	      else if (icode == CODE_FOR_mulsi3)
		this_cycles = 5;
	    }
	  if (this_cycles >= cycles)
	    return;

	  cycles -= this_cycles;
	}
    }
  while (cycles > 0)
    {
      emit_insn_before (gen_nop (), first_insn);
      cycles--;
    }
}

/* Return an insn type for INSN that can be used by the caller for anomaly
   workarounds.  This differs from plain get_attr_type in that it handles
   SEQUENCEs.  */

static enum attr_type
type_for_anomaly (rtx_insn *insn)
{
  rtx pat = PATTERN (insn);
  if (rtx_sequence *seq = dyn_cast <rtx_sequence *> (pat))
    {
      enum attr_type t;
      t = get_attr_type (seq->insn (1));
      if (t == TYPE_MCLD)
	return t;
      t = get_attr_type (seq->insn (2));
      if (t == TYPE_MCLD)
	return t;
      return TYPE_MCST;
    }
  else
    return get_attr_type (insn);
}

/* Return true iff the address found in MEM is based on the register
   NP_REG and optionally has a positive offset.  */
static bool
harmless_null_pointer_p (rtx mem, int np_reg)
{
  mem = XEXP (mem, 0);
  if (GET_CODE (mem) == POST_INC || GET_CODE (mem) == POST_DEC)
    mem = XEXP (mem, 0);
  if (REG_P (mem) && (int) REGNO (mem) == np_reg)
    return true;
  if (GET_CODE (mem) == PLUS
      && REG_P (XEXP (mem, 0)) && (int) REGNO (XEXP (mem, 0)) == np_reg)
    {
      mem = XEXP (mem, 1);
      if (GET_CODE (mem) == CONST_INT && INTVAL (mem) > 0)
	return true;
    }
  return false;
}

/* Return nonzero if INSN contains any loads that may trap.  */

static bool
trapping_loads_p (rtx_insn *insn, int np_reg, bool after_np_branch)
{
  rtx mem = SET_SRC (single_set (insn));

  if (!after_np_branch)
    np_reg = -1;
  return ((np_reg == -1 || !harmless_null_pointer_p (mem, np_reg))
	  && may_trap_p (mem));
}

/* Return INSN if it is of TYPE_MCLD.  Alternatively, if INSN is the start of
   a three-insn bundle, see if one of them is a load and return that if so.
   Return NULL if the insn does not contain loads.  */
static rtx_insn *
find_load (rtx_insn *insn)
{
  if (!NONDEBUG_INSN_P (insn))
    return NULL;
  if (get_attr_type (insn) == TYPE_MCLD)
    return insn;
  if (GET_MODE (insn) != SImode)
    return NULL;
  do {
    insn = NEXT_INSN (insn);
    if ((GET_MODE (insn) == SImode || GET_MODE (insn) == QImode)
	&& get_attr_type (insn) == TYPE_MCLD)
      return insn;
  } while (GET_MODE (insn) != QImode);
  return NULL;
}

/* Determine whether PAT is an indirect call pattern.  */
static bool
indirect_call_p (rtx pat)
{
  if (GET_CODE (pat) == PARALLEL)
    pat = XVECEXP (pat, 0, 0);
  if (GET_CODE (pat) == SET)
    pat = SET_SRC (pat);
  gcc_assert (GET_CODE (pat) == CALL);
  pat = XEXP (pat, 0);
  gcc_assert (GET_CODE (pat) == MEM);
  pat = XEXP (pat, 0);
  
  return REG_P (pat);
}

/* During workaround_speculation, track whether we're in the shadow of a
   conditional branch that tests a P register for NULL.  If so, we can omit
   emitting NOPs if we see a load from that P register, since a speculative
   access at address 0 isn't a problem, and the load is executed in all other
   cases anyway.
   Global for communication with note_np_check_stores through note_stores.
   */
int np_check_regno = -1;
bool np_after_branch = false;

/* Subroutine of workaround_speculation, called through note_stores.  */
static void
note_np_check_stores (rtx x, const_rtx pat ATTRIBUTE_UNUSED,
		      void *data ATTRIBUTE_UNUSED)
{
  if (REG_P (x) && (REGNO (x) == REG_CC || (int) REGNO (x) == np_check_regno))
    np_check_regno = -1;
}

static void
workaround_speculation (void)
{
  rtx_insn *insn, *next;
  rtx_insn *last_condjump = NULL;
  int cycles_since_jump = INT_MAX;
  int delay_added = 0;

  if (! ENABLE_WA_SPECULATIVE_LOADS && ! ENABLE_WA_SPECULATIVE_SYNCS
      && ! ENABLE_WA_INDIRECT_CALLS)
    return;

  /* First pass: find predicted-false branches; if something after them
     needs nops, insert them or change the branch to predict true.  */
  for (insn = get_insns (); insn; insn = next)
    {
      rtx pat;
      int delay_needed = 0;

      next = find_next_insn_start (insn);
      
      if (NOTE_P (insn) || BARRIER_P (insn))
	continue;
      if (JUMP_TABLE_DATA_P (insn))
	continue;

      if (LABEL_P (insn))
	{
	  np_check_regno = -1;
	  continue;
	}

      pat = PATTERN (insn);
      if (GET_CODE (pat) == USE || GET_CODE (pat) == CLOBBER)
	continue;
      
      if (GET_CODE (pat) == ASM_INPUT || asm_noperands (pat) >= 0)
	{
	  np_check_regno = -1;
	  continue;
	}

      if (JUMP_P (insn))
	{
	  /* Is this a condjump based on a null pointer comparison we saw
	     earlier?  */
	  if (np_check_regno != -1
	      && recog_memoized (insn) == CODE_FOR_cbranchbi4)
	    {
	      rtx op = XEXP (SET_SRC (PATTERN (insn)), 0);
	      gcc_assert (GET_CODE (op) == EQ || GET_CODE (op) == NE);
	      if (GET_CODE (op) == NE)
		np_after_branch = true;
	    }
	  if (any_condjump_p (insn)
	      && ! cbranch_predicted_taken_p (insn))
	    {
	      last_condjump = insn;
	      delay_added = 0;
	      cycles_since_jump = 0;
	    }
	  else
	    cycles_since_jump = INT_MAX;
	}
      else if (CALL_P (insn))
	{
	  np_check_regno = -1;
	  if (cycles_since_jump < INT_MAX)
	    cycles_since_jump++;
	  if (indirect_call_p (pat) && ENABLE_WA_INDIRECT_CALLS)
	    {
	      delay_needed = 3;
	    }
	}
      else if (NONDEBUG_INSN_P (insn))
	{
	  rtx_insn *load_insn = find_load (insn);
	  enum attr_type type = type_for_anomaly (insn);

	  if (cycles_since_jump < INT_MAX)
	    cycles_since_jump++;

	  /* Detect a comparison of a P register with zero.  If we later
	     see a condjump based on it, we have found a null pointer
	     check.  */
	  if (recog_memoized (insn) == CODE_FOR_compare_eq)
	    {
	      rtx src = SET_SRC (PATTERN (insn));
	      if (REG_P (XEXP (src, 0))
		  && P_REGNO_P (REGNO (XEXP (src, 0)))
		  && XEXP (src, 1) == const0_rtx)
		{
		  np_check_regno = REGNO (XEXP (src, 0));
		  np_after_branch = false;
		}
	      else
		np_check_regno = -1;
	    }

	  if (load_insn && ENABLE_WA_SPECULATIVE_LOADS)
	    {
	      if (trapping_loads_p (load_insn, np_check_regno,
				    np_after_branch))
		delay_needed = 4;
	    }
	  else if (type == TYPE_SYNC && ENABLE_WA_SPECULATIVE_SYNCS)
	    delay_needed = 3;

	  /* See if we need to forget about a null pointer comparison
	     we found earlier.  */
	  if (recog_memoized (insn) != CODE_FOR_compare_eq)
	    {
	      note_stores (PATTERN (insn), note_np_check_stores, NULL);
	      if (np_check_regno != -1)
		{
		  if (find_regno_note (insn, REG_INC, np_check_regno))
		    np_check_regno = -1;
		}
	    }

	}

      if (delay_needed > cycles_since_jump
	  && (delay_needed - cycles_since_jump) > delay_added)
	{
	  rtx pat1;
	  int num_clobbers;
	  rtx *op = recog_data.operand;

	  delay_needed -= cycles_since_jump;

	  extract_insn (last_condjump);
	  if (optimize_size)
	    {
	      pat1 = gen_cbranch_predicted_taken (op[0], op[1], op[2],
						 op[3]);
	      cycles_since_jump = INT_MAX;
	    }
	  else
	    {
	      /* Do not adjust cycles_since_jump in this case, so that
		 we'll increase the number of NOPs for a subsequent insn
		 if necessary.  */
	      pat1 = gen_cbranch_with_nops (op[0], op[1], op[2], op[3],
					    GEN_INT (delay_needed));
	      delay_added = delay_needed;
	    }
	  PATTERN (last_condjump) = pat1;
	  INSN_CODE (last_condjump) = recog (pat1, insn, &num_clobbers);
	}
      if (CALL_P (insn))
	{
	  cycles_since_jump = INT_MAX;
	  delay_added = 0;
	}
    }

  /* Second pass: for predicted-true branches, see if anything at the
     branch destination needs extra nops.  */
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      int cycles_since_jump;
      if (JUMP_P (insn)
	  && any_condjump_p (insn)
	  && (INSN_CODE (insn) == CODE_FOR_cbranch_predicted_taken
	      || cbranch_predicted_taken_p (insn)))
	{
	  rtx_insn *target = JUMP_LABEL_AS_INSN (insn);
	  rtx_insn *label = target;
	  rtx_insn *next_tgt;

	  cycles_since_jump = 0;
	  for (; target && cycles_since_jump < 3; target = next_tgt)
	    {
	      rtx pat;

	      next_tgt = find_next_insn_start (target);

	      if (NOTE_P (target) || BARRIER_P (target) || LABEL_P (target))
		continue;

	      if (JUMP_TABLE_DATA_P (target))
		continue;

	      pat = PATTERN (target);
	      if (GET_CODE (pat) == USE || GET_CODE (pat) == CLOBBER
		  || GET_CODE (pat) == ASM_INPUT
		  || asm_noperands (pat) >= 0)
		continue;

	      if (NONDEBUG_INSN_P (target))
		{
		  rtx_insn *load_insn = find_load (target);
		  enum attr_type type = type_for_anomaly (target);
		  int delay_needed = 0;
		  if (cycles_since_jump < INT_MAX)
		    cycles_since_jump++;

		  if (load_insn && ENABLE_WA_SPECULATIVE_LOADS)
		    {
		      if (trapping_loads_p (load_insn, -1, false))
			delay_needed = 2;
		    }
		  else if (type == TYPE_SYNC && ENABLE_WA_SPECULATIVE_SYNCS)
		    delay_needed = 2;

		  if (delay_needed > cycles_since_jump)
		    {
		      rtx_insn *prev = prev_real_insn (label);
		      delay_needed -= cycles_since_jump;
		      if (dump_file)
			fprintf (dump_file, "Adding %d nops after %d\n",
				 delay_needed, INSN_UID (label));
		      if (JUMP_P (prev)
			  && INSN_CODE (prev) == CODE_FOR_cbranch_with_nops)
			{
			  rtx x;
			  HOST_WIDE_INT v;

			  if (dump_file)
			    fprintf (dump_file,
				     "Reducing nops on insn %d.\n",
				     INSN_UID (prev));
			  x = PATTERN (prev);
			  x = XVECEXP (x, 0, 1);
			  v = INTVAL (XVECEXP (x, 0, 0)) - delay_needed;
			  XVECEXP (x, 0, 0) = GEN_INT (v);
			}
		      while (delay_needed-- > 0)
			emit_insn_after (gen_nop (), label);
		      break;
		    }
		}
	    }
	}
    }
}

/* Called just before the final scheduling pass.  If we need to insert NOPs
   later on to work around speculative loads, insert special placeholder
   insns that cause loads to be delayed for as many cycles as necessary
   (and possible).  This reduces the number of NOPs we need to add.
   The dummy insns we generate are later removed by bfin_gen_bundles.  */
static void
add_sched_insns_for_speculation (void)
{
  rtx_insn *insn;

  if (! ENABLE_WA_SPECULATIVE_LOADS && ! ENABLE_WA_SPECULATIVE_SYNCS
      && ! ENABLE_WA_INDIRECT_CALLS)
    return;

  /* First pass: find predicted-false branches; if something after them
     needs nops, insert them or change the branch to predict true.  */
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      rtx pat;

      if (NOTE_P (insn) || BARRIER_P (insn) || LABEL_P (insn))
	continue;
      if (JUMP_TABLE_DATA_P (insn))
	continue;

      pat = PATTERN (insn);
      if (GET_CODE (pat) == USE || GET_CODE (pat) == CLOBBER
	  || GET_CODE (pat) == ASM_INPUT
	  || asm_noperands (pat) >= 0)
	continue;

      if (JUMP_P (insn))
	{
	  if (any_condjump_p (insn)
	      && !cbranch_predicted_taken_p (insn))
	    {
	      rtx_insn *n = next_real_insn (insn);
	      emit_insn_before (gen_stall (GEN_INT (3)), n);
	    }
	}
    }

  /* Second pass: for predicted-true branches, see if anything at the
     branch destination needs extra nops.  */
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      if (JUMP_P (insn)
	  && any_condjump_p (insn)
	  && (cbranch_predicted_taken_p (insn)))
	{
	  rtx target = JUMP_LABEL (insn);
	  rtx_insn *next = next_real_insn (target);

	  if (GET_CODE (PATTERN (next)) == UNSPEC_VOLATILE
	      && get_attr_type (next) == TYPE_STALL)
	    continue;
	  emit_insn_before (gen_stall (GEN_INT (1)), next);	  
	}
    }
}

/* We use the machine specific reorg pass for emitting CSYNC instructions
   after conditional branches as needed.

   The Blackfin is unusual in that a code sequence like
     if cc jump label
     r0 = (p0)
   may speculatively perform the load even if the condition isn't true.  This
   happens for a branch that is predicted not taken, because the pipeline
   isn't flushed or stalled, so the early stages of the following instructions,
   which perform the memory reference, are allowed to execute before the
   jump condition is evaluated.
   Therefore, we must insert additional instructions in all places where this
   could lead to incorrect behavior.  The manual recommends CSYNC, while
   VDSP seems to use NOPs (even though its corresponding compiler option is
   named CSYNC).

   When optimizing for speed, we emit NOPs, which seems faster than a CSYNC.
   When optimizing for size, we turn the branch into a predicted taken one.
   This may be slower due to mispredicts, but saves code size.  */

static void
bfin_reorg (void)
{
  /* We are freeing block_for_insn in the toplev to keep compatibility
     with old MDEP_REORGS that are not CFG based.  Recompute it now.  */
  compute_bb_for_insn ();

  if (flag_schedule_insns_after_reload)
    {
      splitting_for_sched = 1;
      split_all_insns ();
      splitting_for_sched = 0;

      add_sched_insns_for_speculation ();

      timevar_push (TV_SCHED2);
      if (flag_selective_scheduling2
	  && !maybe_skip_selective_scheduling ())
        run_selective_scheduling ();
      else
	schedule_insns ();
      timevar_pop (TV_SCHED2);

      /* Examine the schedule and insert nops as necessary for 64-bit parallel
	 instructions.  */
      bfin_gen_bundles ();
    }

  df_analyze ();

  /* Doloop optimization */
  if (cfun->machine->has_hardware_loops)
    bfin_reorg_loops ();

  workaround_speculation ();

  if (flag_var_tracking)
    {
      timevar_push (TV_VAR_TRACKING);
      variable_tracking_main ();
      reorder_var_tracking_notes ();
      timevar_pop (TV_VAR_TRACKING);
    }

  df_finish_pass (false);

  workaround_rts_anomaly ();
}

/* Handle interrupt_handler, exception_handler and nmi_handler function
   attributes; arguments as in struct attribute_spec.handler.  */

static tree
handle_int_attribute (tree *node, tree name,
		      tree args ATTRIBUTE_UNUSED,
		      int flags ATTRIBUTE_UNUSED,
		      bool *no_add_attrs)
{
  tree x = *node;
  if (TREE_CODE (x) == FUNCTION_DECL)
    x = TREE_TYPE (x);

  if (TREE_CODE (x) != FUNCTION_TYPE)
    {
      warning (OPT_Wattributes, "%qE attribute only applies to functions",
	       name);
      *no_add_attrs = true;
    }
  else if (funkind (x) != SUBROUTINE)
    error ("multiple function type attributes specified");

  return NULL_TREE;
}

/* Return 0 if the attributes for two types are incompatible, 1 if they
   are compatible, and 2 if they are nearly compatible (which causes a
   warning to be generated).  */

static int
bfin_comp_type_attributes (const_tree type1, const_tree type2)
{
  e_funkind kind1, kind2;

  if (TREE_CODE (type1) != FUNCTION_TYPE)
    return 1;

  kind1 = funkind (type1);
  kind2 = funkind (type2);

  if (kind1 != kind2)
    return 0;
  
  /*  Check for mismatched modifiers */
  if (!lookup_attribute ("nesting", TYPE_ATTRIBUTES (type1))
      != !lookup_attribute ("nesting", TYPE_ATTRIBUTES (type2)))
    return 0;

  if (!lookup_attribute ("saveall", TYPE_ATTRIBUTES (type1))
      != !lookup_attribute ("saveall", TYPE_ATTRIBUTES (type2)))
    return 0;

  if (!lookup_attribute ("kspisusp", TYPE_ATTRIBUTES (type1))
      != !lookup_attribute ("kspisusp", TYPE_ATTRIBUTES (type2)))
    return 0;

  if (!lookup_attribute ("longcall", TYPE_ATTRIBUTES (type1))
      != !lookup_attribute ("longcall", TYPE_ATTRIBUTES (type2)))
    return 0;

  return 1;
}

/* Handle a "longcall" or "shortcall" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
bfin_handle_longcall_attribute (tree *node, tree name, 
				tree args ATTRIBUTE_UNUSED, 
				int flags ATTRIBUTE_UNUSED, 
				bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_TYPE
      && TREE_CODE (*node) != FIELD_DECL
      && TREE_CODE (*node) != TYPE_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute only applies to functions",
	       name);
      *no_add_attrs = true;
    }

  if ((strcmp (IDENTIFIER_POINTER (name), "longcall") == 0
       && lookup_attribute ("shortcall", TYPE_ATTRIBUTES (*node)))
      || (strcmp (IDENTIFIER_POINTER (name), "shortcall") == 0
	  && lookup_attribute ("longcall", TYPE_ATTRIBUTES (*node))))
    {
      warning (OPT_Wattributes,
	       "can%'t apply both longcall and shortcall attributes to the same function");
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "l1_text" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
bfin_handle_l1_text_attribute (tree *node, tree name, tree ARG_UNUSED (args),
			       int ARG_UNUSED (flags), bool *no_add_attrs)
{
  tree decl = *node;

  if (TREE_CODE (decl) != FUNCTION_DECL)
    {
      error ("%qE attribute only applies to functions",
	     name);
      *no_add_attrs = true;
    }

  /* The decl may have already been given a section attribute
     from a previous declaration. Ensure they match.  */
  else if (DECL_SECTION_NAME (decl) != NULL
	   && strcmp (DECL_SECTION_NAME (decl),
		      ".l1.text") != 0)
    {
      error ("section of %q+D conflicts with previous declaration",
	     decl);
      *no_add_attrs = true;
    }
  else
    set_decl_section_name (decl, ".l1.text");

  return NULL_TREE;
}

/* Handle a "l1_data", "l1_data_A" or "l1_data_B" attribute;
   arguments as in struct attribute_spec.handler.  */

static tree
bfin_handle_l1_data_attribute (tree *node, tree name, tree ARG_UNUSED (args),
			       int ARG_UNUSED (flags), bool *no_add_attrs)
{
  tree decl = *node;

  if (TREE_CODE (decl) != VAR_DECL)
    {
      error ("%qE attribute only applies to variables",
	     name);
      *no_add_attrs = true;
    }
  else if (current_function_decl != NULL_TREE
	   && !TREE_STATIC (decl))
    {
      error ("%qE attribute cannot be specified for local variables",
	     name);
      *no_add_attrs = true;
    }
  else
    {
      const char *section_name;

      if (strcmp (IDENTIFIER_POINTER (name), "l1_data") == 0)
	section_name = ".l1.data";
      else if (strcmp (IDENTIFIER_POINTER (name), "l1_data_A") == 0)
	section_name = ".l1.data.A";
      else if (strcmp (IDENTIFIER_POINTER (name), "l1_data_B") == 0)
	section_name = ".l1.data.B";
      else
	gcc_unreachable ();

      /* The decl may have already been given a section attribute
	 from a previous declaration. Ensure they match.  */
      if (DECL_SECTION_NAME (decl) != NULL
	  && strcmp (DECL_SECTION_NAME (decl),
		     section_name) != 0)
	{
	  error ("section of %q+D conflicts with previous declaration",
		 decl);
	  *no_add_attrs = true;
	}
      else
	set_decl_section_name (decl, section_name);
    }

 return NULL_TREE;
}

/* Handle a "l2" attribute; arguments as in struct attribute_spec.handler.  */

static tree
bfin_handle_l2_attribute (tree *node, tree ARG_UNUSED (name),
			  tree ARG_UNUSED (args), int ARG_UNUSED (flags),
			  bool *no_add_attrs)
{
  tree decl = *node;

  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      if (DECL_SECTION_NAME (decl) != NULL
	  && strcmp (DECL_SECTION_NAME (decl),
		     ".l2.text") != 0)
	{
	  error ("section of %q+D conflicts with previous declaration",
		 decl);
	  *no_add_attrs = true;
	}
      else
	set_decl_section_name (decl, ".l2.text");
    }
  else if (TREE_CODE (decl) == VAR_DECL)
    {
      if (DECL_SECTION_NAME (decl) != NULL
	  && strcmp (DECL_SECTION_NAME (decl),
		     ".l2.data") != 0)
	{
	  error ("section of %q+D conflicts with previous declaration",
		 decl);
	  *no_add_attrs = true;
	}
      else
	set_decl_section_name (decl, ".l2.data");
    }

  return NULL_TREE;
}

/* Table of valid machine attributes.  */
static const struct attribute_spec bfin_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler,
       affects_type_identity } */
  { "interrupt_handler", 0, 0, false, true,  true, handle_int_attribute,
    false },
  { "exception_handler", 0, 0, false, true,  true, handle_int_attribute,
    false },
  { "nmi_handler", 0, 0, false, true,  true, handle_int_attribute, false },
  { "nesting", 0, 0, false, true,  true, NULL, false },
  { "kspisusp", 0, 0, false, true,  true, NULL, false },
  { "saveall", 0, 0, false, true,  true, NULL, false },
  { "longcall",  0, 0, false, true,  true,  bfin_handle_longcall_attribute,
    false },
  { "shortcall", 0, 0, false, true,  true,  bfin_handle_longcall_attribute,
    false },
  { "l1_text", 0, 0, true, false, false,  bfin_handle_l1_text_attribute,
    false },
  { "l1_data", 0, 0, true, false, false,  bfin_handle_l1_data_attribute,
    false },
  { "l1_data_A", 0, 0, true, false, false, bfin_handle_l1_data_attribute,
    false },
  { "l1_data_B", 0, 0, true, false, false,  bfin_handle_l1_data_attribute,
    false },
  { "l2", 0, 0, true, false, false,  bfin_handle_l2_attribute, false },
  { NULL, 0, 0, false, false, false, NULL, false }
};

/* Implementation of TARGET_ASM_INTEGER.  When using FD-PIC, we need to
   tell the assembler to generate pointers to function descriptors in
   some cases.  */

static bool
bfin_assemble_integer (rtx value, unsigned int size, int aligned_p)
{
  if (TARGET_FDPIC && size == UNITS_PER_WORD)
    {
      if (GET_CODE (value) == SYMBOL_REF
	  && SYMBOL_REF_FUNCTION_P (value))
	{
	  fputs ("\t.picptr\tfuncdesc(", asm_out_file);
	  output_addr_const (asm_out_file, value);
	  fputs (")\n", asm_out_file);
	  return true;
	}
      if (!aligned_p)
	{
	  /* We've set the unaligned SI op to NULL, so we always have to
	     handle the unaligned case here.  */
	  assemble_integer_with_op ("\t.4byte\t", value);
	  return true;
	}
    }
  return default_assemble_integer (value, size, aligned_p);
}

/* Output the assembler code for a thunk function.  THUNK_DECL is the
   declaration for the thunk function itself, FUNCTION is the decl for
   the target function.  DELTA is an immediate constant offset to be
   added to THIS.  If VCALL_OFFSET is nonzero, the word at
   *(*this + vcall_offset) should be added to THIS.  */

static void
bfin_output_mi_thunk (FILE *file ATTRIBUTE_UNUSED,
		      tree thunk ATTRIBUTE_UNUSED, HOST_WIDE_INT delta,
		      HOST_WIDE_INT vcall_offset, tree function)
{
  rtx xops[3];
  /* The this parameter is passed as the first argument.  */
  rtx this_rtx = gen_rtx_REG (Pmode, REG_R0);

  /* Adjust the this parameter by a fixed constant.  */
  if (delta)
    {
      xops[1] = this_rtx;
      if (delta >= -64 && delta <= 63)
	{
	  xops[0] = GEN_INT (delta);
	  output_asm_insn ("%1 += %0;", xops);
	}
      else if (delta >= -128 && delta < -64)
	{
	  xops[0] = GEN_INT (delta + 64);
	  output_asm_insn ("%1 += -64; %1 += %0;", xops);
	}
      else if (delta > 63 && delta <= 126)
	{
	  xops[0] = GEN_INT (delta - 63);
	  output_asm_insn ("%1 += 63; %1 += %0;", xops);
	}
      else
	{
	  xops[0] = GEN_INT (delta);
	  output_asm_insn ("r3.l = %h0; r3.h = %d0; %1 = %1 + r3;", xops);
	}
    }

  /* Adjust the this parameter by a value stored in the vtable.  */
  if (vcall_offset)
    {
      rtx p2tmp = gen_rtx_REG (Pmode, REG_P2);
      rtx tmp = gen_rtx_REG (Pmode, REG_R3);

      xops[1] = tmp;
      xops[2] = p2tmp;
      output_asm_insn ("%2 = r0; %2 = [%2];", xops);

      /* Adjust the this parameter.  */
      xops[0] = gen_rtx_MEM (Pmode, plus_constant (Pmode, p2tmp,
						   vcall_offset));
      if (!memory_operand (xops[0], Pmode))
	{
	  rtx tmp2 = gen_rtx_REG (Pmode, REG_P1);
	  xops[0] = GEN_INT (vcall_offset);
	  xops[1] = tmp2;
	  output_asm_insn ("%h1 = %h0; %d1 = %d0; %2 = %2 + %1", xops);
	  xops[0] = gen_rtx_MEM (Pmode, p2tmp);
	}
      xops[2] = this_rtx;
      output_asm_insn ("%1 = %0; %2 = %2 + %1;", xops);
    }

  xops[0] = XEXP (DECL_RTL (function), 0);
  if (1 || !flag_pic || (*targetm.binds_local_p) (function))
    output_asm_insn ("jump.l\t%P0", xops);
}

/* Codes for all the Blackfin builtins.  */
enum bfin_builtins
{
  BFIN_BUILTIN_CSYNC,
  BFIN_BUILTIN_SSYNC,
  BFIN_BUILTIN_ONES,
  BFIN_BUILTIN_COMPOSE_2X16,
  BFIN_BUILTIN_EXTRACTLO,
  BFIN_BUILTIN_EXTRACTHI,

  BFIN_BUILTIN_SSADD_2X16,
  BFIN_BUILTIN_SSSUB_2X16,
  BFIN_BUILTIN_SSADDSUB_2X16,
  BFIN_BUILTIN_SSSUBADD_2X16,
  BFIN_BUILTIN_MULT_2X16,
  BFIN_BUILTIN_MULTR_2X16,
  BFIN_BUILTIN_NEG_2X16,
  BFIN_BUILTIN_ABS_2X16,
  BFIN_BUILTIN_MIN_2X16,
  BFIN_BUILTIN_MAX_2X16,

  BFIN_BUILTIN_SSADD_1X16,
  BFIN_BUILTIN_SSSUB_1X16,
  BFIN_BUILTIN_MULT_1X16,
  BFIN_BUILTIN_MULTR_1X16,
  BFIN_BUILTIN_NORM_1X16,
  BFIN_BUILTIN_NEG_1X16,
  BFIN_BUILTIN_ABS_1X16,
  BFIN_BUILTIN_MIN_1X16,
  BFIN_BUILTIN_MAX_1X16,

  BFIN_BUILTIN_SUM_2X16,
  BFIN_BUILTIN_DIFFHL_2X16,
  BFIN_BUILTIN_DIFFLH_2X16,

  BFIN_BUILTIN_SSADD_1X32,
  BFIN_BUILTIN_SSSUB_1X32,
  BFIN_BUILTIN_NORM_1X32,
  BFIN_BUILTIN_ROUND_1X32,
  BFIN_BUILTIN_NEG_1X32,
  BFIN_BUILTIN_ABS_1X32,
  BFIN_BUILTIN_MIN_1X32,
  BFIN_BUILTIN_MAX_1X32,
  BFIN_BUILTIN_MULT_1X32,
  BFIN_BUILTIN_MULT_1X32X32,
  BFIN_BUILTIN_MULT_1X32X32NS,

  BFIN_BUILTIN_MULHISILL,
  BFIN_BUILTIN_MULHISILH,
  BFIN_BUILTIN_MULHISIHL,
  BFIN_BUILTIN_MULHISIHH,

  BFIN_BUILTIN_LSHIFT_1X16,
  BFIN_BUILTIN_LSHIFT_2X16,
  BFIN_BUILTIN_SSASHIFT_1X16,
  BFIN_BUILTIN_SSASHIFT_2X16,
  BFIN_BUILTIN_SSASHIFT_1X32,

  BFIN_BUILTIN_CPLX_MUL_16,
  BFIN_BUILTIN_CPLX_MAC_16,
  BFIN_BUILTIN_CPLX_MSU_16,

  BFIN_BUILTIN_CPLX_MUL_16_S40,
  BFIN_BUILTIN_CPLX_MAC_16_S40,
  BFIN_BUILTIN_CPLX_MSU_16_S40,

  BFIN_BUILTIN_CPLX_SQU,

  BFIN_BUILTIN_LOADBYTES,

  BFIN_BUILTIN_MAX
};

#define def_builtin(NAME, TYPE, CODE)					\
do {									\
  add_builtin_function ((NAME), (TYPE), (CODE), BUILT_IN_MD,		\
		       NULL, NULL_TREE);				\
} while (0)

/* Set up all builtin functions for this target.  */
static void
bfin_init_builtins (void)
{
  tree V2HI_type_node = build_vector_type_for_mode (intHI_type_node, V2HImode);
  tree void_ftype_void
    = build_function_type_list (void_type_node, NULL_TREE);
  tree short_ftype_short
    = build_function_type_list (short_integer_type_node, short_integer_type_node,
				NULL_TREE);
  tree short_ftype_int_int
    = build_function_type_list (short_integer_type_node, integer_type_node,
				integer_type_node, NULL_TREE);
  tree int_ftype_int_int
    = build_function_type_list (integer_type_node, integer_type_node,
				integer_type_node, NULL_TREE);
  tree int_ftype_int
    = build_function_type_list (integer_type_node, integer_type_node,
				NULL_TREE);
  tree short_ftype_int
    = build_function_type_list (short_integer_type_node, integer_type_node,
				NULL_TREE);
  tree int_ftype_v2hi_v2hi
    = build_function_type_list (integer_type_node, V2HI_type_node,
				V2HI_type_node, NULL_TREE);
  tree v2hi_ftype_v2hi_v2hi
    = build_function_type_list (V2HI_type_node, V2HI_type_node,
				V2HI_type_node, NULL_TREE);
  tree v2hi_ftype_v2hi_v2hi_v2hi
    = build_function_type_list (V2HI_type_node, V2HI_type_node,
				V2HI_type_node, V2HI_type_node, NULL_TREE);
  tree v2hi_ftype_int_int
    = build_function_type_list (V2HI_type_node, integer_type_node,
				integer_type_node, NULL_TREE);
  tree v2hi_ftype_v2hi_int
    = build_function_type_list (V2HI_type_node, V2HI_type_node,
				integer_type_node, NULL_TREE);
  tree int_ftype_short_short
    = build_function_type_list (integer_type_node, short_integer_type_node,
				short_integer_type_node, NULL_TREE);
  tree v2hi_ftype_v2hi
    = build_function_type_list (V2HI_type_node, V2HI_type_node, NULL_TREE);
  tree short_ftype_v2hi
    = build_function_type_list (short_integer_type_node, V2HI_type_node,
				NULL_TREE);
  tree int_ftype_pint
    = build_function_type_list (integer_type_node,
				build_pointer_type (integer_type_node),
				NULL_TREE);
  
  /* Add the remaining MMX insns with somewhat more complicated types.  */
  def_builtin ("__builtin_bfin_csync", void_ftype_void, BFIN_BUILTIN_CSYNC);
  def_builtin ("__builtin_bfin_ssync", void_ftype_void, BFIN_BUILTIN_SSYNC);

  def_builtin ("__builtin_bfin_ones", short_ftype_int, BFIN_BUILTIN_ONES);

  def_builtin ("__builtin_bfin_compose_2x16", v2hi_ftype_int_int,
	       BFIN_BUILTIN_COMPOSE_2X16);
  def_builtin ("__builtin_bfin_extract_hi", short_ftype_v2hi,
	       BFIN_BUILTIN_EXTRACTHI);
  def_builtin ("__builtin_bfin_extract_lo", short_ftype_v2hi,
	       BFIN_BUILTIN_EXTRACTLO);

  def_builtin ("__builtin_bfin_min_fr2x16", v2hi_ftype_v2hi_v2hi,
	       BFIN_BUILTIN_MIN_2X16);
  def_builtin ("__builtin_bfin_max_fr2x16", v2hi_ftype_v2hi_v2hi,
	       BFIN_BUILTIN_MAX_2X16);

  def_builtin ("__builtin_bfin_add_fr2x16", v2hi_ftype_v2hi_v2hi,
	       BFIN_BUILTIN_SSADD_2X16);
  def_builtin ("__builtin_bfin_sub_fr2x16", v2hi_ftype_v2hi_v2hi,
	       BFIN_BUILTIN_SSSUB_2X16);
  def_builtin ("__builtin_bfin_dspaddsubsat", v2hi_ftype_v2hi_v2hi,
	       BFIN_BUILTIN_SSADDSUB_2X16);
  def_builtin ("__builtin_bfin_dspsubaddsat", v2hi_ftype_v2hi_v2hi,
	       BFIN_BUILTIN_SSSUBADD_2X16);
  def_builtin ("__builtin_bfin_mult_fr2x16", v2hi_ftype_v2hi_v2hi,
	       BFIN_BUILTIN_MULT_2X16);
  def_builtin ("__builtin_bfin_multr_fr2x16", v2hi_ftype_v2hi_v2hi,
	       BFIN_BUILTIN_MULTR_2X16);
  def_builtin ("__builtin_bfin_negate_fr2x16", v2hi_ftype_v2hi,
	       BFIN_BUILTIN_NEG_2X16);
  def_builtin ("__builtin_bfin_abs_fr2x16", v2hi_ftype_v2hi,
	       BFIN_BUILTIN_ABS_2X16);

  def_builtin ("__builtin_bfin_min_fr1x16", short_ftype_int_int,
	       BFIN_BUILTIN_MIN_1X16);
  def_builtin ("__builtin_bfin_max_fr1x16", short_ftype_int_int,
	       BFIN_BUILTIN_MAX_1X16);

  def_builtin ("__builtin_bfin_add_fr1x16", short_ftype_int_int,
	       BFIN_BUILTIN_SSADD_1X16);
  def_builtin ("__builtin_bfin_sub_fr1x16", short_ftype_int_int,
	       BFIN_BUILTIN_SSSUB_1X16);
  def_builtin ("__builtin_bfin_mult_fr1x16", short_ftype_int_int,
	       BFIN_BUILTIN_MULT_1X16);
  def_builtin ("__builtin_bfin_multr_fr1x16", short_ftype_int_int,
	       BFIN_BUILTIN_MULTR_1X16);
  def_builtin ("__builtin_bfin_negate_fr1x16", short_ftype_short,
	       BFIN_BUILTIN_NEG_1X16);
  def_builtin ("__builtin_bfin_abs_fr1x16", short_ftype_short,
	       BFIN_BUILTIN_ABS_1X16);
  def_builtin ("__builtin_bfin_norm_fr1x16", short_ftype_int,
	       BFIN_BUILTIN_NORM_1X16);

  def_builtin ("__builtin_bfin_sum_fr2x16", short_ftype_v2hi,
	       BFIN_BUILTIN_SUM_2X16);
  def_builtin ("__builtin_bfin_diff_hl_fr2x16", short_ftype_v2hi,
	       BFIN_BUILTIN_DIFFHL_2X16);
  def_builtin ("__builtin_bfin_diff_lh_fr2x16", short_ftype_v2hi,
	       BFIN_BUILTIN_DIFFLH_2X16);

  def_builtin ("__builtin_bfin_mulhisill", int_ftype_v2hi_v2hi,
	       BFIN_BUILTIN_MULHISILL);
  def_builtin ("__builtin_bfin_mulhisihl", int_ftype_v2hi_v2hi,
	       BFIN_BUILTIN_MULHISIHL);
  def_builtin ("__builtin_bfin_mulhisilh", int_ftype_v2hi_v2hi,
	       BFIN_BUILTIN_MULHISILH);
  def_builtin ("__builtin_bfin_mulhisihh", int_ftype_v2hi_v2hi,
	       BFIN_BUILTIN_MULHISIHH);

  def_builtin ("__builtin_bfin_min_fr1x32", int_ftype_int_int,
	       BFIN_BUILTIN_MIN_1X32);
  def_builtin ("__builtin_bfin_max_fr1x32", int_ftype_int_int,
	       BFIN_BUILTIN_MAX_1X32);

  def_builtin ("__builtin_bfin_add_fr1x32", int_ftype_int_int,
	       BFIN_BUILTIN_SSADD_1X32);
  def_builtin ("__builtin_bfin_sub_fr1x32", int_ftype_int_int,
	       BFIN_BUILTIN_SSSUB_1X32);
  def_builtin ("__builtin_bfin_negate_fr1x32", int_ftype_int,
	       BFIN_BUILTIN_NEG_1X32);
  def_builtin ("__builtin_bfin_abs_fr1x32", int_ftype_int,
	       BFIN_BUILTIN_ABS_1X32);
  def_builtin ("__builtin_bfin_norm_fr1x32", short_ftype_int,
	       BFIN_BUILTIN_NORM_1X32);
  def_builtin ("__builtin_bfin_round_fr1x32", short_ftype_int,
	       BFIN_BUILTIN_ROUND_1X32);
  def_builtin ("__builtin_bfin_mult_fr1x32", int_ftype_short_short,
	       BFIN_BUILTIN_MULT_1X32);
  def_builtin ("__builtin_bfin_mult_fr1x32x32", int_ftype_int_int,
	       BFIN_BUILTIN_MULT_1X32X32);
  def_builtin ("__builtin_bfin_mult_fr1x32x32NS", int_ftype_int_int,
	       BFIN_BUILTIN_MULT_1X32X32NS);

  /* Shifts.  */
  def_builtin ("__builtin_bfin_shl_fr1x16", short_ftype_int_int,
	       BFIN_BUILTIN_SSASHIFT_1X16);
  def_builtin ("__builtin_bfin_shl_fr2x16", v2hi_ftype_v2hi_int,
	       BFIN_BUILTIN_SSASHIFT_2X16);
  def_builtin ("__builtin_bfin_lshl_fr1x16", short_ftype_int_int,
	       BFIN_BUILTIN_LSHIFT_1X16);
  def_builtin ("__builtin_bfin_lshl_fr2x16", v2hi_ftype_v2hi_int,
	       BFIN_BUILTIN_LSHIFT_2X16);
  def_builtin ("__builtin_bfin_shl_fr1x32", int_ftype_int_int,
	       BFIN_BUILTIN_SSASHIFT_1X32);

  /* Complex numbers.  */
  def_builtin ("__builtin_bfin_cmplx_add", v2hi_ftype_v2hi_v2hi,
	       BFIN_BUILTIN_SSADD_2X16);
  def_builtin ("__builtin_bfin_cmplx_sub", v2hi_ftype_v2hi_v2hi,
	       BFIN_BUILTIN_SSSUB_2X16);
  def_builtin ("__builtin_bfin_cmplx_mul", v2hi_ftype_v2hi_v2hi,
	       BFIN_BUILTIN_CPLX_MUL_16);
  def_builtin ("__builtin_bfin_cmplx_mac", v2hi_ftype_v2hi_v2hi_v2hi,
	       BFIN_BUILTIN_CPLX_MAC_16);
  def_builtin ("__builtin_bfin_cmplx_msu", v2hi_ftype_v2hi_v2hi_v2hi,
	       BFIN_BUILTIN_CPLX_MSU_16);
  def_builtin ("__builtin_bfin_cmplx_mul_s40", v2hi_ftype_v2hi_v2hi,
	       BFIN_BUILTIN_CPLX_MUL_16_S40);
  def_builtin ("__builtin_bfin_cmplx_mac_s40", v2hi_ftype_v2hi_v2hi_v2hi,
	       BFIN_BUILTIN_CPLX_MAC_16_S40);
  def_builtin ("__builtin_bfin_cmplx_msu_s40", v2hi_ftype_v2hi_v2hi_v2hi,
	       BFIN_BUILTIN_CPLX_MSU_16_S40);
  def_builtin ("__builtin_bfin_csqu_fr16", v2hi_ftype_v2hi,
	       BFIN_BUILTIN_CPLX_SQU);

  /* "Unaligned" load.  */
  def_builtin ("__builtin_bfin_loadbytes", int_ftype_pint,
	       BFIN_BUILTIN_LOADBYTES);

}


struct builtin_description
{
  const enum insn_code icode;
  const char *const name;
  const enum bfin_builtins code;
  int macflag;
};

static const struct builtin_description bdesc_2arg[] =
{
  { CODE_FOR_composev2hi, "__builtin_bfin_compose_2x16", BFIN_BUILTIN_COMPOSE_2X16, -1 },

  { CODE_FOR_ssashiftv2hi3, "__builtin_bfin_shl_fr2x16", BFIN_BUILTIN_SSASHIFT_2X16, -1 },
  { CODE_FOR_ssashifthi3, "__builtin_bfin_shl_fr1x16", BFIN_BUILTIN_SSASHIFT_1X16, -1 },
  { CODE_FOR_lshiftv2hi3, "__builtin_bfin_lshl_fr2x16", BFIN_BUILTIN_LSHIFT_2X16, -1 },
  { CODE_FOR_lshifthi3, "__builtin_bfin_lshl_fr1x16", BFIN_BUILTIN_LSHIFT_1X16, -1 },
  { CODE_FOR_ssashiftsi3, "__builtin_bfin_shl_fr1x32", BFIN_BUILTIN_SSASHIFT_1X32, -1 },

  { CODE_FOR_sminhi3, "__builtin_bfin_min_fr1x16", BFIN_BUILTIN_MIN_1X16, -1 },
  { CODE_FOR_smaxhi3, "__builtin_bfin_max_fr1x16", BFIN_BUILTIN_MAX_1X16, -1 },
  { CODE_FOR_ssaddhi3, "__builtin_bfin_add_fr1x16", BFIN_BUILTIN_SSADD_1X16, -1 },
  { CODE_FOR_sssubhi3, "__builtin_bfin_sub_fr1x16", BFIN_BUILTIN_SSSUB_1X16, -1 },

  { CODE_FOR_sminsi3, "__builtin_bfin_min_fr1x32", BFIN_BUILTIN_MIN_1X32, -1 },
  { CODE_FOR_smaxsi3, "__builtin_bfin_max_fr1x32", BFIN_BUILTIN_MAX_1X32, -1 },
  { CODE_FOR_ssaddsi3, "__builtin_bfin_add_fr1x32", BFIN_BUILTIN_SSADD_1X32, -1 },
  { CODE_FOR_sssubsi3, "__builtin_bfin_sub_fr1x32", BFIN_BUILTIN_SSSUB_1X32, -1 },

  { CODE_FOR_sminv2hi3, "__builtin_bfin_min_fr2x16", BFIN_BUILTIN_MIN_2X16, -1 },
  { CODE_FOR_smaxv2hi3, "__builtin_bfin_max_fr2x16", BFIN_BUILTIN_MAX_2X16, -1 },
  { CODE_FOR_ssaddv2hi3, "__builtin_bfin_add_fr2x16", BFIN_BUILTIN_SSADD_2X16, -1 },
  { CODE_FOR_sssubv2hi3, "__builtin_bfin_sub_fr2x16", BFIN_BUILTIN_SSSUB_2X16, -1 },
  { CODE_FOR_ssaddsubv2hi3, "__builtin_bfin_dspaddsubsat", BFIN_BUILTIN_SSADDSUB_2X16, -1 },
  { CODE_FOR_sssubaddv2hi3, "__builtin_bfin_dspsubaddsat", BFIN_BUILTIN_SSSUBADD_2X16, -1 },

  { CODE_FOR_flag_mulhisi, "__builtin_bfin_mult_fr1x32", BFIN_BUILTIN_MULT_1X32, MACFLAG_NONE },
  { CODE_FOR_flag_mulhi, "__builtin_bfin_mult_fr1x16", BFIN_BUILTIN_MULT_1X16, MACFLAG_T },
  { CODE_FOR_flag_mulhi, "__builtin_bfin_multr_fr1x16", BFIN_BUILTIN_MULTR_1X16, MACFLAG_NONE },
  { CODE_FOR_flag_mulv2hi, "__builtin_bfin_mult_fr2x16", BFIN_BUILTIN_MULT_2X16, MACFLAG_T },
  { CODE_FOR_flag_mulv2hi, "__builtin_bfin_multr_fr2x16", BFIN_BUILTIN_MULTR_2X16, MACFLAG_NONE },

  { CODE_FOR_mulhisi_ll, "__builtin_bfin_mulhisill", BFIN_BUILTIN_MULHISILL, -1 },
  { CODE_FOR_mulhisi_lh, "__builtin_bfin_mulhisilh", BFIN_BUILTIN_MULHISILH, -1 },
  { CODE_FOR_mulhisi_hl, "__builtin_bfin_mulhisihl", BFIN_BUILTIN_MULHISIHL, -1 },
  { CODE_FOR_mulhisi_hh, "__builtin_bfin_mulhisihh", BFIN_BUILTIN_MULHISIHH, -1 }

};

static const struct builtin_description bdesc_1arg[] =
{
  { CODE_FOR_loadbytes, "__builtin_bfin_loadbytes", BFIN_BUILTIN_LOADBYTES, 0 },

  { CODE_FOR_ones, "__builtin_bfin_ones", BFIN_BUILTIN_ONES, 0 },

  { CODE_FOR_clrsbhi2, "__builtin_bfin_norm_fr1x16", BFIN_BUILTIN_NORM_1X16, 0 },
  { CODE_FOR_ssneghi2, "__builtin_bfin_negate_fr1x16", BFIN_BUILTIN_NEG_1X16, 0 },
  { CODE_FOR_abshi2, "__builtin_bfin_abs_fr1x16", BFIN_BUILTIN_ABS_1X16, 0 },

  { CODE_FOR_clrsbsi2, "__builtin_bfin_norm_fr1x32", BFIN_BUILTIN_NORM_1X32, 0 },
  { CODE_FOR_ssroundsi2, "__builtin_bfin_round_fr1x32", BFIN_BUILTIN_ROUND_1X32, 0 },
  { CODE_FOR_ssnegsi2, "__builtin_bfin_negate_fr1x32", BFIN_BUILTIN_NEG_1X32, 0 },
  { CODE_FOR_ssabssi2, "__builtin_bfin_abs_fr1x32", BFIN_BUILTIN_ABS_1X32, 0 },

  { CODE_FOR_movv2hi_hi_low, "__builtin_bfin_extract_lo", BFIN_BUILTIN_EXTRACTLO, 0 },
  { CODE_FOR_movv2hi_hi_high, "__builtin_bfin_extract_hi", BFIN_BUILTIN_EXTRACTHI, 0 },
  { CODE_FOR_ssnegv2hi2, "__builtin_bfin_negate_fr2x16", BFIN_BUILTIN_NEG_2X16, 0 },
  { CODE_FOR_ssabsv2hi2, "__builtin_bfin_abs_fr2x16", BFIN_BUILTIN_ABS_2X16, 0 }
};

/* Errors in the source file can cause expand_expr to return const0_rtx
   where we expect a vector.  To avoid crashing, use one of the vector
   clear instructions.  */
static rtx
safe_vector_operand (rtx x, machine_mode mode)
{
  if (x != const0_rtx)
    return x;
  x = gen_reg_rtx (SImode);

  emit_insn (gen_movsi (x, CONST0_RTX (SImode)));
  return gen_lowpart (mode, x);
}

/* Subroutine of bfin_expand_builtin to take care of binop insns.  MACFLAG is -1
   if this is a normal binary op, or one of the MACFLAG_xxx constants.  */

static rtx
bfin_expand_binop_builtin (enum insn_code icode, tree exp, rtx target,
			   int macflag)
{
  rtx pat;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  machine_mode op0mode = GET_MODE (op0);
  machine_mode op1mode = GET_MODE (op1);
  machine_mode tmode = insn_data[icode].operand[0].mode;
  machine_mode mode0 = insn_data[icode].operand[1].mode;
  machine_mode mode1 = insn_data[icode].operand[2].mode;

  if (VECTOR_MODE_P (mode0))
    op0 = safe_vector_operand (op0, mode0);
  if (VECTOR_MODE_P (mode1))
    op1 = safe_vector_operand (op1, mode1);

  if (! target
      || GET_MODE (target) != tmode
      || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);

  if ((op0mode == SImode || op0mode == VOIDmode) && mode0 == HImode)
    {
      op0mode = HImode;
      op0 = gen_lowpart (HImode, op0);
    }
  if ((op1mode == SImode || op1mode == VOIDmode) && mode1 == HImode)
    {
      op1mode = HImode;
      op1 = gen_lowpart (HImode, op1);
    }
  /* In case the insn wants input operands in modes different from
     the result, abort.  */
  gcc_assert ((op0mode == mode0 || op0mode == VOIDmode)
	      && (op1mode == mode1 || op1mode == VOIDmode));

  if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);
  if (! (*insn_data[icode].operand[2].predicate) (op1, mode1))
    op1 = copy_to_mode_reg (mode1, op1);

  if (macflag == -1)
    pat = GEN_FCN (icode) (target, op0, op1);
  else
    pat = GEN_FCN (icode) (target, op0, op1, GEN_INT (macflag));
  if (! pat)
    return 0;

  emit_insn (pat);
  return target;
}

/* Subroutine of bfin_expand_builtin to take care of unop insns.  */

static rtx
bfin_expand_unop_builtin (enum insn_code icode, tree exp,
			  rtx target)
{
  rtx pat;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  rtx op0 = expand_normal (arg0);
  machine_mode op0mode = GET_MODE (op0);
  machine_mode tmode = insn_data[icode].operand[0].mode;
  machine_mode mode0 = insn_data[icode].operand[1].mode;

  if (! target
      || GET_MODE (target) != tmode
      || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);

  if (VECTOR_MODE_P (mode0))
    op0 = safe_vector_operand (op0, mode0);

  if (op0mode == SImode && mode0 == HImode)
    {
      op0mode = HImode;
      op0 = gen_lowpart (HImode, op0);
    }
  gcc_assert (op0mode == mode0 || op0mode == VOIDmode);

  if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);

  pat = GEN_FCN (icode) (target, op0);
  if (! pat)
    return 0;
  emit_insn (pat);
  return target;
}

/* Expand an expression EXP that calls a built-in function,
   with result going to TARGET if that's convenient
   (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.  */

static rtx
bfin_expand_builtin (tree exp, rtx target ATTRIBUTE_UNUSED,
		     rtx subtarget ATTRIBUTE_UNUSED,
		     machine_mode mode ATTRIBUTE_UNUSED,
		     int ignore ATTRIBUTE_UNUSED)
{
  size_t i;
  enum insn_code icode;
  const struct builtin_description *d;
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  unsigned int fcode = DECL_FUNCTION_CODE (fndecl);
  tree arg0, arg1, arg2;
  rtx op0, op1, op2, accvec, pat, tmp1, tmp2, a0reg, a1reg;
  machine_mode tmode, mode0;

  switch (fcode)
    {
    case BFIN_BUILTIN_CSYNC:
      emit_insn (gen_csync ());
      return 0;
    case BFIN_BUILTIN_SSYNC:
      emit_insn (gen_ssync ());
      return 0;

    case BFIN_BUILTIN_DIFFHL_2X16:
    case BFIN_BUILTIN_DIFFLH_2X16:
    case BFIN_BUILTIN_SUM_2X16:
      arg0 = CALL_EXPR_ARG (exp, 0);
      op0 = expand_normal (arg0);
      icode = (fcode == BFIN_BUILTIN_DIFFHL_2X16 ? CODE_FOR_subhilov2hi3
	       : fcode == BFIN_BUILTIN_DIFFLH_2X16 ? CODE_FOR_sublohiv2hi3
	       : CODE_FOR_ssaddhilov2hi3);
      tmode = insn_data[icode].operand[0].mode;
      mode0 = insn_data[icode].operand[1].mode;

      if (! target
	  || GET_MODE (target) != tmode
	  || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
	target = gen_reg_rtx (tmode);

      if (VECTOR_MODE_P (mode0))
	op0 = safe_vector_operand (op0, mode0);

      if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);

      pat = GEN_FCN (icode) (target, op0, op0);
      if (! pat)
	return 0;
      emit_insn (pat);
      return target;

    case BFIN_BUILTIN_MULT_1X32X32:
    case BFIN_BUILTIN_MULT_1X32X32NS:
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);
      if (! target
	  || !register_operand (target, SImode))
	target = gen_reg_rtx (SImode);
      if (! register_operand (op0, SImode))
	op0 = copy_to_mode_reg (SImode, op0);
      if (! register_operand (op1, SImode))
	op1 = copy_to_mode_reg (SImode, op1);

      a1reg = gen_rtx_REG (PDImode, REG_A1);
      a0reg = gen_rtx_REG (PDImode, REG_A0);
      tmp1 = gen_lowpart (V2HImode, op0);
      tmp2 = gen_lowpart (V2HImode, op1);
      emit_insn (gen_flag_macinit1hi (a1reg,
				      gen_lowpart (HImode, op0),
				      gen_lowpart (HImode, op1),
				      GEN_INT (MACFLAG_FU)));
      emit_insn (gen_lshrpdi3 (a1reg, a1reg, GEN_INT (16)));

      if (fcode == BFIN_BUILTIN_MULT_1X32X32)
	emit_insn (gen_flag_mul_macv2hi_parts_acconly (a0reg, a1reg, tmp1, tmp2,
						       const1_rtx, const1_rtx,
						       const1_rtx, const0_rtx, a1reg,
						       const0_rtx, GEN_INT (MACFLAG_NONE),
						       GEN_INT (MACFLAG_M)));
      else
	{
	  /* For saturating multiplication, there's exactly one special case
	     to be handled: multiplying the smallest negative value with
	     itself.  Due to shift correction in fractional multiplies, this
	     can overflow.  Iff this happens, OP2 will contain 1, which, when
	     added in 32 bits to the smallest negative, wraps to the largest
	     positive, which is the result we want.  */
	  op2 = gen_reg_rtx (V2HImode);
	  emit_insn (gen_packv2hi (op2, tmp1, tmp2, const0_rtx, const0_rtx));
	  emit_insn (gen_movsibi (gen_rtx_REG (BImode, REG_CC),
				  gen_lowpart (SImode, op2)));
	  emit_insn (gen_flag_mul_macv2hi_parts_acconly_andcc0 (a0reg, a1reg, tmp1, tmp2,
								const1_rtx, const1_rtx,
								const1_rtx, const0_rtx, a1reg,
								const0_rtx, GEN_INT (MACFLAG_NONE),
								GEN_INT (MACFLAG_M)));
	  op2 = gen_reg_rtx (SImode);
	  emit_insn (gen_movbisi (op2, gen_rtx_REG (BImode, REG_CC)));
	}
      emit_insn (gen_flag_machi_parts_acconly (a1reg, tmp2, tmp1,
					       const1_rtx, const0_rtx,
					       a1reg, const0_rtx, GEN_INT (MACFLAG_M)));
      emit_insn (gen_ashrpdi3 (a1reg, a1reg, GEN_INT (15)));
      emit_insn (gen_sum_of_accumulators (target, a0reg, a0reg, a1reg));
      if (fcode == BFIN_BUILTIN_MULT_1X32X32NS)
	emit_insn (gen_addsi3 (target, target, op2));
      return target;

    case BFIN_BUILTIN_CPLX_MUL_16:
    case BFIN_BUILTIN_CPLX_MUL_16_S40:
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);
      accvec = gen_reg_rtx (V2PDImode);
      icode = CODE_FOR_flag_macv2hi_parts;
      tmode = insn_data[icode].operand[0].mode;

      if (! target
	  || GET_MODE (target) != V2HImode
	  || ! (*insn_data[icode].operand[0].predicate) (target, V2HImode))
	target = gen_reg_rtx (tmode);
      if (! register_operand (op0, GET_MODE (op0)))
	op0 = copy_to_mode_reg (GET_MODE (op0), op0);
      if (! register_operand (op1, GET_MODE (op1)))
	op1 = copy_to_mode_reg (GET_MODE (op1), op1);

      if (fcode == BFIN_BUILTIN_CPLX_MUL_16)
	emit_insn (gen_flag_macinit1v2hi_parts (accvec, op0, op1, const0_rtx,
						const0_rtx, const0_rtx,
						const1_rtx, GEN_INT (MACFLAG_W32)));
      else
	emit_insn (gen_flag_macinit1v2hi_parts (accvec, op0, op1, const0_rtx,
						const0_rtx, const0_rtx,
						const1_rtx, GEN_INT (MACFLAG_NONE)));
      emit_insn (gen_flag_macv2hi_parts (target, op0, op1, const1_rtx,
					 const1_rtx, const1_rtx,
					 const0_rtx, accvec, const1_rtx, const0_rtx,
					 GEN_INT (MACFLAG_NONE), accvec));

      return target;

    case BFIN_BUILTIN_CPLX_MAC_16:
    case BFIN_BUILTIN_CPLX_MSU_16:
    case BFIN_BUILTIN_CPLX_MAC_16_S40:
    case BFIN_BUILTIN_CPLX_MSU_16_S40:
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      arg2 = CALL_EXPR_ARG (exp, 2);
      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);
      op2 = expand_normal (arg2);
      accvec = gen_reg_rtx (V2PDImode);
      icode = CODE_FOR_flag_macv2hi_parts;
      tmode = insn_data[icode].operand[0].mode;

      if (! target
	  || GET_MODE (target) != V2HImode
	  || ! (*insn_data[icode].operand[0].predicate) (target, V2HImode))
	target = gen_reg_rtx (tmode);
      if (! register_operand (op1, GET_MODE (op1)))
	op1 = copy_to_mode_reg (GET_MODE (op1), op1);
      if (! register_operand (op2, GET_MODE (op2)))
	op2 = copy_to_mode_reg (GET_MODE (op2), op2);

      tmp1 = gen_reg_rtx (SImode);
      tmp2 = gen_reg_rtx (SImode);
      emit_insn (gen_ashlsi3 (tmp1, gen_lowpart (SImode, op0), GEN_INT (16)));
      emit_move_insn (tmp2, gen_lowpart (SImode, op0));
      emit_insn (gen_movstricthi_1 (gen_lowpart (HImode, tmp2), const0_rtx));
      emit_insn (gen_load_accumulator_pair (accvec, tmp1, tmp2));
      if (fcode == BFIN_BUILTIN_CPLX_MAC_16
	  || fcode == BFIN_BUILTIN_CPLX_MSU_16)
	emit_insn (gen_flag_macv2hi_parts_acconly (accvec, op1, op2, const0_rtx,
						   const0_rtx, const0_rtx,
						   const1_rtx, accvec, const0_rtx,
						   const0_rtx,
						   GEN_INT (MACFLAG_W32)));
      else
	emit_insn (gen_flag_macv2hi_parts_acconly (accvec, op1, op2, const0_rtx,
						   const0_rtx, const0_rtx,
						   const1_rtx, accvec, const0_rtx,
						   const0_rtx,
						   GEN_INT (MACFLAG_NONE)));
      if (fcode == BFIN_BUILTIN_CPLX_MAC_16
	  || fcode == BFIN_BUILTIN_CPLX_MAC_16_S40)
	{
	  tmp1 = const1_rtx;
	  tmp2 = const0_rtx;
	}
      else
	{
	  tmp1 = const0_rtx;
	  tmp2 = const1_rtx;
	}
      emit_insn (gen_flag_macv2hi_parts (target, op1, op2, const1_rtx,
					 const1_rtx, const1_rtx,
					 const0_rtx, accvec, tmp1, tmp2,
					 GEN_INT (MACFLAG_NONE), accvec));

      return target;

    case BFIN_BUILTIN_CPLX_SQU:
      arg0 = CALL_EXPR_ARG (exp, 0);
      op0 = expand_normal (arg0);
      accvec = gen_reg_rtx (V2PDImode);
      icode = CODE_FOR_flag_mulv2hi;
      tmp1 = gen_reg_rtx (V2HImode);
      tmp2 = gen_reg_rtx (V2HImode);

      if (! target
	  || GET_MODE (target) != V2HImode
	  || ! (*insn_data[icode].operand[0].predicate) (target, V2HImode))
	target = gen_reg_rtx (V2HImode);
      if (! register_operand (op0, GET_MODE (op0)))
	op0 = copy_to_mode_reg (GET_MODE (op0), op0);

      emit_insn (gen_flag_mulv2hi (tmp1, op0, op0, GEN_INT (MACFLAG_NONE)));

      emit_insn (gen_flag_mulhi_parts (gen_lowpart (HImode, tmp2), op0, op0,
				       const0_rtx, const1_rtx,
				       GEN_INT (MACFLAG_NONE)));

      emit_insn (gen_ssaddhi3_high_parts (target, tmp2, tmp2, tmp2, const0_rtx,
					  const0_rtx));
      emit_insn (gen_sssubhi3_low_parts (target, target, tmp1, tmp1,
					 const0_rtx, const1_rtx));

      return target;

    default:
      break;
    }

  for (i = 0, d = bdesc_2arg; i < ARRAY_SIZE (bdesc_2arg); i++, d++)
    if (d->code == fcode)
      return bfin_expand_binop_builtin (d->icode, exp, target,
					d->macflag);

  for (i = 0, d = bdesc_1arg; i < ARRAY_SIZE (bdesc_1arg); i++, d++)
    if (d->code == fcode)
      return bfin_expand_unop_builtin (d->icode, exp, target);

  gcc_unreachable ();
}

static void
bfin_conditional_register_usage (void)
{
  /* initialize condition code flag register rtx */
  bfin_cc_rtx = gen_rtx_REG (BImode, REG_CC);
  bfin_rets_rtx = gen_rtx_REG (Pmode, REG_RETS);
  if (TARGET_FDPIC)
    call_used_regs[FDPIC_REGNO] = 1;
  if (!TARGET_FDPIC && flag_pic)
    {
      fixed_regs[PIC_OFFSET_TABLE_REGNUM] = 1;
      call_used_regs[PIC_OFFSET_TABLE_REGNUM] = 1;
    }
}

#undef TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS bfin_init_builtins

#undef TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN bfin_expand_builtin

#undef TARGET_ASM_GLOBALIZE_LABEL
#define TARGET_ASM_GLOBALIZE_LABEL bfin_globalize_label 

#undef TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START output_file_start

#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE bfin_attribute_table

#undef TARGET_COMP_TYPE_ATTRIBUTES
#define TARGET_COMP_TYPE_ATTRIBUTES bfin_comp_type_attributes

#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS bfin_rtx_costs

#undef  TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST bfin_address_cost

#undef TARGET_REGISTER_MOVE_COST
#define TARGET_REGISTER_MOVE_COST bfin_register_move_cost

#undef TARGET_MEMORY_MOVE_COST
#define TARGET_MEMORY_MOVE_COST bfin_memory_move_cost

#undef  TARGET_ASM_INTEGER
#define TARGET_ASM_INTEGER bfin_assemble_integer

#undef TARGET_MACHINE_DEPENDENT_REORG
#define TARGET_MACHINE_DEPENDENT_REORG bfin_reorg

#undef TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL bfin_function_ok_for_sibcall

#undef TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK bfin_output_mi_thunk
#undef TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK hook_bool_const_tree_hwi_hwi_const_tree_true

#undef TARGET_SCHED_ADJUST_COST
#define TARGET_SCHED_ADJUST_COST bfin_adjust_cost

#undef TARGET_SCHED_ISSUE_RATE
#define TARGET_SCHED_ISSUE_RATE bfin_issue_rate

#undef TARGET_PROMOTE_FUNCTION_MODE
#define TARGET_PROMOTE_FUNCTION_MODE default_promote_function_mode_always_promote

#undef TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES bfin_arg_partial_bytes

#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG bfin_function_arg

#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE bfin_function_arg_advance

#undef TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE bfin_pass_by_reference

#undef TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS setup_incoming_varargs

#undef TARGET_STRUCT_VALUE_RTX
#define TARGET_STRUCT_VALUE_RTX bfin_struct_value_rtx

#undef TARGET_VECTOR_MODE_SUPPORTED_P
#define TARGET_VECTOR_MODE_SUPPORTED_P bfin_vector_mode_supported_p

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE bfin_option_override

#undef TARGET_SECONDARY_RELOAD
#define TARGET_SECONDARY_RELOAD bfin_secondary_reload

#undef TARGET_CLASS_LIKELY_SPILLED_P
#define TARGET_CLASS_LIKELY_SPILLED_P bfin_class_likely_spilled_p

#undef TARGET_DELEGITIMIZE_ADDRESS
#define TARGET_DELEGITIMIZE_ADDRESS bfin_delegitimize_address

#undef TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P bfin_legitimate_constant_p

#undef TARGET_CANNOT_FORCE_CONST_MEM
#define TARGET_CANNOT_FORCE_CONST_MEM bfin_cannot_force_const_mem

#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY bfin_return_in_memory

#undef TARGET_LRA_P
#define TARGET_LRA_P hook_bool_void_false

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P	bfin_legitimate_address_p

#undef TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED bfin_frame_pointer_required

#undef TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE bfin_can_eliminate

#undef TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE bfin_conditional_register_usage

#undef TARGET_ASM_TRAMPOLINE_TEMPLATE
#define TARGET_ASM_TRAMPOLINE_TEMPLATE bfin_asm_trampoline_template
#undef TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT bfin_trampoline_init

#undef TARGET_EXTRA_LIVE_ON_ENTRY
#define TARGET_EXTRA_LIVE_ON_ENTRY bfin_extra_live_on_entry

/* Passes after sched2 can break the helpful TImode annotations that
   haifa-sched puts on every insn.  Just do scheduling in reorg.  */
#undef TARGET_DELAY_SCHED2
#define TARGET_DELAY_SCHED2 true

/* Variable tracking should be run after all optimizations which
   change order of insns.  It also needs a valid CFG.  */
#undef TARGET_DELAY_VARTRACK
#define TARGET_DELAY_VARTRACK true

#undef TARGET_CAN_USE_DOLOOP_P
#define TARGET_CAN_USE_DOLOOP_P bfin_can_use_doloop_p

struct gcc_target targetm = TARGET_INITIALIZER;
