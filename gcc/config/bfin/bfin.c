/* The Blackfin code generation auxiliary output file.
   Copyright (C) 2005  Free Software Foundation, Inc.
   Contributed by Analog Devices.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"
#include "tree.h"
#include "flags.h"
#include "except.h"
#include "function.h"
#include "input.h"
#include "target.h"
#include "target-def.h"
#include "expr.h"
#include "toplev.h"
#include "recog.h"
#include "ggc.h"
#include "integrate.h"
#include "bfin-protos.h"
#include "tm-preds.h"
#include "gt-bfin.h"

/* Test and compare insns in bfin.md store the information needed to
   generate branch and scc insns here.  */
rtx bfin_compare_op0, bfin_compare_op1;

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

/* The value passed to -mshared-library-id=.  */
static int bfin_library_id;
/* Nonzero if -mshared-library-id was given.  */
static int bfin_lib_id_given;

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

  fprintf (file, ".file \"%s\";\n", input_filename);
  
  for (i = 0; arg_regs[i] >= 0; i++)
    ;
  max_arg_registers = i;	/* how many arg reg used  */
}

/* Called early in the compilation to conditionally modify
   fixed_regs/call_used_regs.  */

void 
conditional_register_usage (void)
{
  /* initialize condition code flag register rtx */
  bfin_cc_rtx = gen_rtx_REG (BImode, REG_CC);
  bfin_rets_rtx = gen_rtx_REG (Pmode, REG_RETS);
}

/* Examine machine-dependent attributes of function type FUNTYPE and return its
   type.  See the definition of E_FUNKIND.  */

static e_funkind funkind (tree funtype)
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

/* Stack frame layout. */

/* Compute the number of DREGS to save with a push_multiple operation.
   This could include registers that aren't modified in the function,
   since push_multiple only takes a range of registers.  */

static int
n_dregs_to_save (void)
{
  unsigned i;

  for (i = REG_R0; i <= REG_R7; i++)
    {
      if (regs_ever_live[i] && ! call_used_regs[i])
	return REG_R7 - i + 1;

      if (current_function_calls_eh_return)
	{
	  unsigned j;
	  for (j = 0; ; j++)
	    {
	      unsigned test = EH_RETURN_DATA_REGNO (j);
	      if (test == INVALID_REGNUM)
		break;
	      if (test == i)
		return REG_R7 - i + 1;
	    }
	}

    }
  return 0;
}

/* Like n_dregs_to_save, but compute number of PREGS to save.  */

static int
n_pregs_to_save (void)
{
  unsigned i;

  for (i = REG_P0; i <= REG_P5; i++)
    if ((regs_ever_live[i] && ! call_used_regs[i])
	|| (i == PIC_OFFSET_TABLE_REGNUM
	    && (current_function_uses_pic_offset_table
		|| (TARGET_ID_SHARED_LIBRARY && ! current_function_is_leaf))))
      return REG_P5 - i + 1;
  return 0;
}

/* Determine if we are going to save the frame pointer in the prologue.  */

static bool
must_save_fp_p (void)
{
  return (frame_pointer_needed || regs_ever_live[REG_FP]);
}

static bool
stack_frame_needed_p (void)
{
  /* EH return puts a new return address into the frame using an
     address relative to the frame pointer.  */
  if (current_function_calls_eh_return)
    return true;
  return frame_pointer_needed;
}

/* Emit code to save registers in the prologue.  SAVEALL is nonzero if we
   must save all registers; this is used for interrupt handlers.
   SPREG contains (reg:SI REG_SP).  */

static void
expand_prologue_reg_save (rtx spreg, int saveall)
{
  int ndregs = saveall ? 8 : n_dregs_to_save ();
  int npregs = saveall ? 6 : n_pregs_to_save ();
  int dregno = REG_R7 + 1 - ndregs;
  int pregno = REG_P5 + 1 - npregs;
  int total = ndregs + npregs;
  int i;
  rtx pat, insn, val;

  if (total == 0)
    return;

  val = GEN_INT (-total * 4);
  pat = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (total + 2));
  XVECEXP (pat, 0, 0) = gen_rtx_UNSPEC (VOIDmode, gen_rtvec (1, val),
					UNSPEC_PUSH_MULTIPLE);
  XVECEXP (pat, 0, total + 1) = gen_rtx_SET (VOIDmode, spreg,
					     gen_rtx_PLUS (Pmode, spreg,
							   val));
  RTX_FRAME_RELATED_P (XVECEXP (pat, 0, total + 1)) = 1;
  for (i = 0; i < total; i++)
    {
      rtx memref = gen_rtx_MEM (word_mode,
				gen_rtx_PLUS (Pmode, spreg,
					      GEN_INT (- i * 4 - 4)));
      rtx subpat;
      if (ndregs > 0)
	{
	  subpat = gen_rtx_SET (VOIDmode, memref, gen_rtx_REG (word_mode,
							       dregno++));
	  ndregs--;
	}
      else
	{
	  subpat = gen_rtx_SET (VOIDmode, memref, gen_rtx_REG (word_mode,
							       pregno++));
	  npregs++;
	}
      XVECEXP (pat, 0, i + 1) = subpat;
      RTX_FRAME_RELATED_P (subpat) = 1;
    }
  insn = emit_insn (pat);
  RTX_FRAME_RELATED_P (insn) = 1;
}

/* Emit code to restore registers in the epilogue.  SAVEALL is nonzero if we
   must save all registers; this is used for interrupt handlers.
   SPREG contains (reg:SI REG_SP).  */

static void
expand_epilogue_reg_restore (rtx spreg, int saveall)
{
  int ndregs = saveall ? 8 : n_dregs_to_save ();
  int npregs = saveall ? 6 : n_pregs_to_save ();
  int total = ndregs + npregs;
  int i, regno;
  rtx pat, insn;

  if (total == 0)
    return;

  pat = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (total + 1));
  XVECEXP (pat, 0, 0) = gen_rtx_SET (VOIDmode, spreg,
				     gen_rtx_PLUS (Pmode, spreg,
						   GEN_INT (total * 4)));

  if (npregs > 0)
    regno = REG_P5 + 1;
  else
    regno = REG_R7 + 1;

  for (i = 0; i < total; i++)
    {
      rtx addr = (i > 0
		  ? gen_rtx_PLUS (Pmode, spreg, GEN_INT (i * 4))
		  : spreg);
      rtx memref = gen_rtx_MEM (word_mode, addr);

      regno--;
      XVECEXP (pat, 0, i + 1)
	= gen_rtx_SET (VOIDmode, gen_rtx_REG (word_mode, regno), memref);

      if (npregs > 0)
	{
	  if (--npregs == 0)
	    regno = REG_R7 + 1;
	}
    }

  insn = emit_insn (pat);
  RTX_FRAME_RELATED_P (insn) = 1;
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
setup_incoming_varargs (CUMULATIVE_ARGS *cum,
			enum machine_mode mode ATTRIBUTE_UNUSED,
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

  for (i = cum->words + 1; i < max_arg_registers; i++)
    {
      mem = gen_rtx_MEM (Pmode,
			 plus_constant (arg_pointer_rtx, (i * UNITS_PER_WORD)));
      emit_move_insn (mem, gen_rtx_REG (Pmode, i));
    }

  *pretend_size = 0;
}

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms may
   be accessed via the stack pointer) in functions that seem suitable.  */

int
bfin_frame_pointer_required (void) 
{
  e_funkind fkind = funkind (TREE_TYPE (current_function_decl));

  if (fkind != SUBROUTINE)
    return 1;

  /* We turn on on -fomit-frame-pointer if -momit-leaf-frame-pointer is used,
     so we have to override it for non-leaf functions.  */
  if (TARGET_OMIT_LEAF_FRAME_POINTER && ! current_function_is_leaf)
    return 1;

  return 0;
}

/* Return the number of registers pushed during the prologue.  */

static int
n_regs_saved_by_prologue (void)
{
  e_funkind fkind = funkind (TREE_TYPE (current_function_decl));
  int n = n_dregs_to_save () + n_pregs_to_save ();

  if (stack_frame_needed_p ())
    /* We use a LINK instruction in this case.  */
    n += 2;
  else
    {
      if (must_save_fp_p ())
	n++;
      if (! current_function_is_leaf)
	n++;
    }

  if (fkind != SUBROUTINE)
    {
      tree attrs = TYPE_ATTRIBUTES (TREE_TYPE (current_function_decl));
      tree all = lookup_attribute ("saveall", attrs);
      int i;

      /* Increment once for ASTAT.  */
      n++;

      /* RETE/X/N.  */
      if (lookup_attribute ("nesting", attrs))
	n++;

      for (i = REG_P7 + 1; i < REG_CC; i++)
	if (all 
	    || regs_ever_live[i]
	    || (!leaf_function_p () && call_used_regs[i]))
	  n += i == REG_A0 || i == REG_A1 ? 2 : 1;
    }
  return n;
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
      if (current_function_outgoing_args_size >= FIXED_STACK_AREA)
	offset += current_function_outgoing_args_size;
      else if (current_function_outgoing_args_size)
	offset += FIXED_STACK_AREA;

      offset += get_frame_size ();
    }

  return offset;
}

/* Emit code to load a constant CONSTANT into register REG; setting
   RTX_FRAME_RELATED_P on all insns we generate.  Make sure that the insns
   we generate need not be split.  */

static void
frame_related_constant_load (rtx reg, HOST_WIDE_INT constant)
{
  rtx insn;
  rtx cst = GEN_INT (constant);

  if (constant >= -32768 && constant < 65536)
    insn = emit_move_insn (reg, cst);
  else
    {
      /* We don't call split_load_immediate here, since dwarf2out.c can get
	 confused about some of the more clever sequences it can generate.  */
      insn = emit_insn (gen_movsi_high (reg, cst));
      RTX_FRAME_RELATED_P (insn) = 1;
      insn = emit_insn (gen_movsi_low (reg, reg, cst));
    }
  RTX_FRAME_RELATED_P (insn) = 1;
}

/* Generate efficient code to add a value to the frame pointer.  We
   can use P1 as a scratch register.  Set RTX_FRAME_RELATED_P on the
   generated insns if FRAME is nonzero.  */

static void
add_to_sp (rtx spreg, HOST_WIDE_INT value, int frame)
{
  if (value == 0)
    return;

  /* Choose whether to use a sequence using a temporary register, or
     a sequence with multiple adds.  We can add a signed 7 bit value
     in one instruction.  */
  if (value > 120 || value < -120)
    {
      rtx tmpreg = gen_rtx_REG (SImode, REG_P1);
      rtx insn;

      if (frame)
	frame_related_constant_load (tmpreg, value);
      else
	{
	  insn = emit_move_insn (tmpreg, GEN_INT (value));
	  if (frame)
	    RTX_FRAME_RELATED_P (insn) = 1;
	}

      insn = emit_insn (gen_addsi3 (spreg, spreg, tmpreg));
      if (frame)
	RTX_FRAME_RELATED_P (insn) = 1;
    }
  else
    do
      {
	int size = value;
	rtx insn;

	if (size > 60)
	  size = 60;
	else if (size < -60)
	  /* We could use -62, but that would leave the stack unaligned, so
	     it's no good.  */
	  size = -60;

	insn = emit_insn (gen_addsi3 (spreg, spreg, GEN_INT (size)));
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
  rtx insn;
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

      frame_related_constant_load (tmpreg, -frame_size);
      insn = emit_insn (gen_addsi3 (spreg, spreg, tmpreg));
      RTX_FRAME_RELATED_P (insn) = 1;
    }
}

/* Return the number of bytes we must reserve for outgoing arguments
   in the current function's stack frame.  */

static HOST_WIDE_INT
arg_area_size (void)
{
  if (current_function_outgoing_args_size)
    {
      if (current_function_outgoing_args_size >= FIXED_STACK_AREA)
	return current_function_outgoing_args_size;
      else
	return FIXED_STACK_AREA;
    }
  return 0;
}

/* Save RETS and FP, and allocate a stack frame.  */

static void
do_link (rtx spreg, HOST_WIDE_INT frame_size)
{
  frame_size += arg_area_size ();

  if (stack_frame_needed_p ()
      || (must_save_fp_p () && ! current_function_is_leaf))
    emit_link_insn (spreg, frame_size);
  else
    {
      if (! current_function_is_leaf)
	{
	  rtx pat = gen_movsi (gen_rtx_MEM (Pmode,
					    gen_rtx_PRE_DEC (Pmode, spreg)),
			       bfin_rets_rtx);
	  rtx insn = emit_insn (pat);
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
      if (must_save_fp_p ())
	{
	  rtx pat = gen_movsi (gen_rtx_MEM (Pmode,
					    gen_rtx_PRE_DEC (Pmode, spreg)),
			       gen_rtx_REG (Pmode, REG_FP));
	  rtx insn = emit_insn (pat);
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
      add_to_sp (spreg, -frame_size, 1);
    }
}

/* Like do_link, but used for epilogues to deallocate the stack frame.  */

static void
do_unlink (rtx spreg, HOST_WIDE_INT frame_size)
{
  frame_size += arg_area_size ();

  if (stack_frame_needed_p ())
    emit_insn (gen_unlink ());
  else 
    {
      rtx postinc = gen_rtx_MEM (Pmode, gen_rtx_POST_INC (Pmode, spreg));

      add_to_sp (spreg, frame_size, 0);
      if (must_save_fp_p ())
	{
	  rtx fpreg = gen_rtx_REG (Pmode, REG_FP);
	  emit_move_insn (fpreg, postinc);
	  emit_insn (gen_rtx_USE (VOIDmode, fpreg));
	}
      if (! current_function_is_leaf)
	{
	  emit_move_insn (bfin_rets_rtx, postinc);
	  emit_insn (gen_rtx_USE (VOIDmode, bfin_rets_rtx));
	}
    }
}

/* Generate a prologue suitable for a function of kind FKIND.  This is
   called for interrupt and exception handler prologues.
   SPREG contains (reg:SI REG_SP).  */

static void
expand_interrupt_handler_prologue (rtx spreg, e_funkind fkind)
{
  int i;
  HOST_WIDE_INT frame_size = get_frame_size ();
  rtx predec1 = gen_rtx_PRE_DEC (SImode, spreg);
  rtx predec = gen_rtx_MEM (SImode, predec1);
  rtx insn;
  tree attrs = TYPE_ATTRIBUTES (TREE_TYPE (current_function_decl));
  tree all = lookup_attribute ("saveall", attrs);
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

  insn = emit_move_insn (predec, gen_rtx_REG (SImode, REG_ASTAT));
  RTX_FRAME_RELATED_P (insn) = 1;

  expand_prologue_reg_save (spreg, all != NULL_TREE);

  for (i = REG_P7 + 1; i < REG_CC; i++)
    if (all 
	|| regs_ever_live[i]
	|| (!leaf_function_p () && call_used_regs[i]))
      {
	if (i == REG_A0 || i == REG_A1)
	  insn = emit_move_insn (gen_rtx_MEM (PDImode, predec1),
				 gen_rtx_REG (PDImode, i));
	else
	  insn = emit_move_insn (predec, gen_rtx_REG (SImode, i));
	RTX_FRAME_RELATED_P (insn) = 1;
      }

  if (lookup_attribute ("nesting", attrs))
    {
      rtx srcreg = gen_rtx_REG (Pmode, (fkind == EXCPT_HANDLER ? REG_RETX
					: fkind == NMI_HANDLER ? REG_RETN
					: REG_RETI));
      insn = emit_move_insn (predec, srcreg);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  do_link (spreg, frame_size);

  if (fkind == EXCPT_HANDLER)
    {
      rtx r0reg = gen_rtx_REG (SImode, REG_R0);
      rtx r1reg = gen_rtx_REG (SImode, REG_R1);
      rtx r2reg = gen_rtx_REG (SImode, REG_R2);
      rtx insn;

      insn = emit_move_insn (r0reg, gen_rtx_REG (SImode, REG_SEQSTAT));
      REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_MAYBE_DEAD, const0_rtx,
					    NULL_RTX);
      insn = emit_insn (gen_ashrsi3 (r0reg, r0reg, GEN_INT (26)));
      REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_MAYBE_DEAD, const0_rtx,
					    NULL_RTX);
      insn = emit_insn (gen_ashlsi3 (r0reg, r0reg, GEN_INT (26)));
      REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_MAYBE_DEAD, const0_rtx,
					    NULL_RTX);
      insn = emit_move_insn (r1reg, spreg);
      REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_MAYBE_DEAD, const0_rtx,
					    NULL_RTX);
      insn = emit_move_insn (r2reg, gen_rtx_REG (Pmode, REG_FP));
      REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_MAYBE_DEAD, const0_rtx,
					    NULL_RTX);
      insn = emit_insn (gen_addsi3 (r2reg, r2reg, GEN_INT (8)));
      REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_MAYBE_DEAD, const0_rtx,
					    NULL_RTX);
    }
}

/* Generate an epilogue suitable for a function of kind FKIND.  This is
   called for interrupt and exception handler epilogues.
   SPREG contains (reg:SI REG_SP).  */

static void
expand_interrupt_handler_epilogue (rtx spreg, e_funkind fkind) 
{
  int i;
  rtx postinc1 = gen_rtx_POST_INC (SImode, spreg);
  rtx postinc = gen_rtx_MEM (SImode, postinc1);
  tree attrs = TYPE_ATTRIBUTES (TREE_TYPE (current_function_decl));
  tree all = lookup_attribute ("saveall", attrs);

  /* A slightly crude technique to stop flow from trying to delete "dead"
     insns.  */
  MEM_VOLATILE_P (postinc) = 1;

  do_unlink (spreg, get_frame_size ());

  if (lookup_attribute ("nesting", attrs))
    {
      rtx srcreg = gen_rtx_REG (Pmode, (fkind == EXCPT_HANDLER ? REG_RETX
					: fkind == NMI_HANDLER ? REG_RETN
					: REG_RETI));
      emit_move_insn (srcreg, postinc);
    }

  for (i = REG_CC - 1; i > REG_P7; i--)
    if (all
	|| regs_ever_live[i] 
	|| (!leaf_function_p () && call_used_regs[i]))
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

  expand_epilogue_reg_restore (spreg, all != NULL_TREE);

  emit_move_insn (gen_rtx_REG (SImode, REG_ASTAT), postinc);

  /* Deallocate any space we left on the stack in case we needed to save the
     argument registers.  */
  if (fkind == EXCPT_HANDLER)
    emit_insn (gen_addsi3 (spreg, spreg, GEN_INT (12)));

  emit_jump_insn (gen_return_internal (GEN_INT (fkind)));
}

/* Generate RTL for the prologue of the current function.  */

void
bfin_expand_prologue (void)
{
  rtx insn;
  HOST_WIDE_INT frame_size = get_frame_size ();
  rtx spreg = gen_rtx_REG (Pmode, REG_SP);
  e_funkind fkind = funkind (TREE_TYPE (current_function_decl));

  if (fkind != SUBROUTINE)
    {
      expand_interrupt_handler_prologue (spreg, fkind);
      return;
    }

  expand_prologue_reg_save (spreg, 0);

  do_link (spreg, frame_size);

  if (TARGET_ID_SHARED_LIBRARY
      && (current_function_uses_pic_offset_table
	  || !current_function_is_leaf))
    {
      rtx addr;
      
      if (bfin_lib_id_given)
	addr = plus_constant (pic_offset_table_rtx, -4 - bfin_library_id * 4);
      else
	addr = gen_rtx_PLUS (Pmode, pic_offset_table_rtx,
			     gen_rtx_UNSPEC (Pmode, gen_rtvec (1, const0_rtx),
					     UNSPEC_LIBRARY_OFFSET));
      insn = emit_insn (gen_movsi (pic_offset_table_rtx,
				   gen_rtx_MEM (Pmode, addr)));
      REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_MAYBE_DEAD, const0_rtx, NULL);
    }
}

/* Generate RTL for the epilogue of the current function.  NEED_RETURN is zero
   if this is for a sibcall.  EH_RETURN is nonzero if we're expanding an
   eh_return pattern.  */

void
bfin_expand_epilogue (int need_return, int eh_return)
{
  rtx spreg = gen_rtx_REG (Pmode, REG_SP);
  e_funkind fkind = funkind (TREE_TYPE (current_function_decl));

  if (fkind != SUBROUTINE)
    {
      expand_interrupt_handler_epilogue (spreg, fkind);
      return;
    }

  do_unlink (spreg, get_frame_size ());

  expand_epilogue_reg_restore (spreg, 0);

  /* Omit the return insn if this is for a sibcall.  */
  if (! need_return)
    return;

  if (eh_return)
    emit_insn (gen_addsi3 (spreg, spreg, gen_rtx_REG (Pmode, REG_P2)));

  emit_jump_insn (gen_return_internal (GEN_INT (SUBROUTINE)));
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
      && !regs_ever_live[new_reg])
    return 0;

  return 1;
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

/* Try machine-dependent ways of modifying an illegitimate address X
   to be legitimate.  If we find one, return the new, valid address,
   otherwise return NULL_RTX.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE is the mode of the memory reference.  */

rtx
legitimize_address (rtx x ATTRIBUTE_UNUSED, rtx oldx ATTRIBUTE_UNUSED,
		    enum machine_mode mode ATTRIBUTE_UNUSED)
{
  return NULL_RTX;
}

/* This predicate is used to compute the length of a load/store insn.
   OP is a MEM rtx, we return nonzero if its addressing mode requires a
   32 bit instruction.  */

int
effective_address_32bit_p (rtx op, enum machine_mode mode) 
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

  offset = INTVAL (XEXP (op, 1));

  /* All byte loads use a 16 bit offset.  */
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

/* Return cost of the memory address ADDR.
   All addressing modes are equally cheap on the Blackfin.  */

static int
bfin_address_cost (rtx addr ATTRIBUTE_UNUSED)
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
      output_address (XEXP (x, 0));
      fprintf (file, "+");
      output_address (XEXP (x, 1));
      break;

    case PRE_DEC:
      fprintf (file, "--");
      output_address (XEXP (x, 0));    
      break;
    case POST_INC:
      output_address (XEXP (x, 0));
      fprintf (file, "++");
      break;
    case POST_DEC:
      output_address (XEXP (x, 0));
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
  enum machine_mode mode = GET_MODE (x);

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
	      gcc_assert (REGNO (x) < 32);
	      fprintf (file, "%s", short_reg_names[REGNO (x)]);
	      /*fprintf (file, "\n%d\n ", REGNO (x));*/
	      break;
	    }
	  else if (code == 'd')
	    {
	      gcc_assert (REGNO (x) < 32);
	      fprintf (file, "%s", high_reg_names[REGNO (x)]);
	      break;
	    }
	  else if (code == 'w')
	    {
	      gcc_assert (REGNO (x) == REG_A0 || REGNO (x) == REG_A1);
	      fprintf (file, "%s.w", reg_names[REGNO (x)]);
	    }
	  else if (code == 'x')
	    {
	      gcc_assert (REGNO (x) == REG_A0 || REGNO (x) == REG_A1);
	      fprintf (file, "%s.x", reg_names[REGNO (x)]);
	    }
	  else if (code == 'D')
	    {
	      fprintf (file, "%s", dregs_pair_names[REGNO (x)]);
	    }
	  else if (code == 'H')
	    {
	      gcc_assert (mode == DImode || mode == DFmode);
	      gcc_assert (REG_P (x));
	      fprintf (file, "%s", reg_names[REGNO (x) + 1]);
	    }
	  else if (code == 'T')
	    {
	      gcc_assert (D_REGNO_P (REGNO (x)));
	      fprintf (file, "%s", byte_reg_names[REGNO (x)]);
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
	  /* Moves to half registers with d or h modifiers always use unsigned
	     constants.  */
	  if (code == 'd')
	    x = GEN_INT ((INTVAL (x) >> 16) & 0xffff);
	  else if (code == 'h')
	    x = GEN_INT (INTVAL (x) & 0xffff);
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
	  if (code == 'G' && flag_pic)
	    fprintf (file, "@GOT");
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
init_cumulative_args (CUMULATIVE_ARGS *cum, tree fntype ATTRIBUTE_UNUSED,
		      rtx libname ATTRIBUTE_UNUSED)
{
  static CUMULATIVE_ARGS zero_cum;

  *cum = zero_cum;

  /* Set up the number of registers to use for passing arguments.  */

  cum->nregs = max_arg_registers;
  cum->arg_regs = arg_regs;

  return;
}

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

void
function_arg_advance (CUMULATIVE_ARGS *cum, enum machine_mode mode, tree type,
		      int named ATTRIBUTE_UNUSED)
{
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

struct rtx_def *
function_arg (CUMULATIVE_ARGS *cum, enum machine_mode mode, tree type,
	      int named ATTRIBUTE_UNUSED)
{
  int bytes
    = (mode == BLKmode) ? int_size_in_bytes (type) : GET_MODE_SIZE (mode);

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
   First 3 words are in registers. So, if a an argument is larger
   than the registers available, it will span the register and
   stack.   */

static int
bfin_arg_partial_bytes (CUMULATIVE_ARGS *cum, enum machine_mode mode,
			tree type ATTRIBUTE_UNUSED,
			bool named ATTRIBUTE_UNUSED)
{
  int bytes
    = (mode == BLKmode) ? int_size_in_bytes (type) : GET_MODE_SIZE (mode);
  int bytes_left = cum->nregs * UNITS_PER_WORD;
  
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
bfin_pass_by_reference (CUMULATIVE_ARGS *cum ATTRIBUTE_UNUSED,
			enum machine_mode mode ATTRIBUTE_UNUSED,
			tree type, bool named ATTRIBUTE_UNUSED)
{
  return type && TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST;
}

/* Decide whether a type should be returned in memory (true)
   or in a register (false).  This is called by the macro
   RETURN_IN_MEMORY.  */

int
bfin_return_in_memory (tree type)
{
  int size;
  enum machine_mode mode = TYPE_MODE (type);

  if (mode == BLKmode)
    return 1;
  size = int_size_in_bytes (type);	
  if (VECTOR_MODE_P (mode) || mode == TImode)
    {
      /* User-created vectors small enough to fit in REG.  */
      if (size < 8)
        return 0;
      if (size == 8 || size == 16)
	return 1;
    }

  if (size > 12)
    return 1;
  return 0;
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
  return true;
}

/* Emit RTL insns to initialize the variable parts of a trampoline at
   TRAMP. FNADDR is an RTX for the address of the function's pure
   code.  CXT is an RTX for the static chain value for the function.  */

void
initialize_trampoline (tramp, fnaddr, cxt)
     rtx tramp, fnaddr, cxt;
{
  rtx t1 = copy_to_reg (fnaddr);
  rtx t2 = copy_to_reg (cxt);
  rtx addr;

  addr = memory_address (Pmode, plus_constant (tramp, 2));
  emit_move_insn (gen_rtx_MEM (HImode, addr), gen_lowpart (HImode, t1));
  emit_insn (gen_ashrsi3 (t1, t1, GEN_INT (16)));
  addr = memory_address (Pmode, plus_constant (tramp, 6));
  emit_move_insn (gen_rtx_MEM (HImode, addr), gen_lowpart (HImode, t1));

  addr = memory_address (Pmode, plus_constant (tramp, 10));
  emit_move_insn (gen_rtx_MEM (HImode, addr), gen_lowpart (HImode, t2));
  emit_insn (gen_ashrsi3 (t2, t2, GEN_INT (16)));
  addr = memory_address (Pmode, plus_constant (tramp, 14));
  emit_move_insn (gen_rtx_MEM (HImode, addr), gen_lowpart (HImode, t2));
}

/* Legitimize PIC addresses.  If the address is already position-independent,
   we return ORIG.  Newly generated position-independent addresses go into a
   reg.  This is REG if nonzero, otherwise we allocate register(s) as
   necessary.  */

rtx
legitimize_pic_address (rtx orig, rtx reg)
{
  rtx addr = orig;
  rtx new = orig;

  if (GET_CODE (addr) == SYMBOL_REF || GET_CODE (addr) == LABEL_REF)
    {
      if (GET_CODE (addr) == SYMBOL_REF && CONSTANT_POOL_ADDRESS_P (addr))
	reg = new = orig;
      else
	{
	  if (reg == 0)
	    {
	      gcc_assert (!no_new_pseudos);
	      reg = gen_reg_rtx (Pmode);
	    }

	  if (flag_pic == 2)
	    {
	      emit_insn (gen_movsi_high_pic (reg, addr));
	      emit_insn (gen_movsi_low_pic (reg, reg, addr));
	      emit_insn (gen_addsi3 (reg, reg, pic_offset_table_rtx));
	      new = gen_rtx_MEM (Pmode, reg);
	    }
	  else
	    {
	      rtx tmp = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr),
					UNSPEC_MOVE_PIC);
	      new = gen_rtx_MEM (Pmode,
				 gen_rtx_PLUS (Pmode, pic_offset_table_rtx,
					       tmp));
	    }
	  emit_move_insn (reg, new);
	}
      current_function_uses_pic_offset_table = 1;
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

      if (XEXP (addr, 0) == pic_offset_table_rtx)
	return orig;

      if (reg == 0)
	{
	  gcc_assert (!no_new_pseudos);
	  reg = gen_reg_rtx (Pmode);
	}

      base = legitimize_pic_address (XEXP (addr, 0), reg);
      addr = legitimize_pic_address (XEXP (addr, 1),
				     base == reg ? NULL_RTX : reg);

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

  return new;
}

/* Emit insns to move operands[1] into operands[0].  */

void
emit_pic_move (rtx *operands, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  rtx temp = reload_in_progress ? operands[0] : gen_reg_rtx (Pmode);

  if (GET_CODE (operands[0]) == MEM && SYMBOLIC_CONST (operands[1]))
    operands[1] = force_reg (SImode, operands[1]);
  else
    operands[1] = legitimize_pic_address (operands[1], temp);
}

/* Expand a move operation in mode MODE.  The operands are in OPERANDS.  */

void
expand_move (rtx *operands, enum machine_mode mode)
{
  if (flag_pic && SYMBOLIC_CONST (operands[1]))
    emit_pic_move (operands, mode);

  /* Don't generate memory->memory or constant->memory moves, go through a
     register */
  else if ((reload_in_progress | reload_completed) == 0
	   && GET_CODE (operands[0]) == MEM
    	   && GET_CODE (operands[1]) != REG)
    operands[1] = force_reg (mode, operands[1]);
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

/* Expand a call instruction.  FNADDR is the call target, RETVAL the return value.
   SIBCALL is nonzero if this is a sibling call.  */

void
bfin_expand_call (rtx retval, rtx fnaddr, rtx callarg1, int sibcall)
{
  rtx use = NULL, call;

  /* Static functions and indirect calls don't need the pic register.  */
  if (flag_pic
      && GET_CODE (XEXP (fnaddr, 0)) == SYMBOL_REF
      && ! SYMBOL_REF_LOCAL_P (XEXP (fnaddr, 0)))
    use_reg (&use, pic_offset_table_rtx);

  if (! call_insn_operand (XEXP (fnaddr, 0), Pmode))
    {
      fnaddr = copy_to_mode_reg (Pmode, XEXP (fnaddr, 0));
      fnaddr = gen_rtx_MEM (Pmode, fnaddr);
    }
  call = gen_rtx_CALL (VOIDmode, fnaddr, callarg1);

  if (retval)
    call = gen_rtx_SET (VOIDmode, retval, call);
  if (sibcall)
    {
      rtx pat = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (2));
      XVECEXP (pat, 0, 0) = call;
      XVECEXP (pat, 0, 1) = gen_rtx_RETURN (VOIDmode);
      call = pat;
    }
  call = emit_call_insn (call);
  if (use)
    CALL_INSN_FUNCTION_USAGE (call) = use;
}

/* Return 1 if hard register REGNO can hold a value of machine-mode MODE.  */

int
hard_regno_mode_ok (int regno, enum machine_mode mode)
{
  /* Allow only dregs to store value of mode HI or QI */
  enum reg_class class = REGNO_REG_CLASS (regno);

  if (mode == CCmode)
    return 0;

  if (mode == V2HImode)
    return D_REGNO_P (regno);
  if (class == CCREGS)
    return mode == BImode;
  if (mode == PDImode)
    return regno == REG_A0 || regno == REG_A1;
  if (mode == SImode
      && TEST_HARD_REG_BIT (reg_class_contents[PROLOGUE_REGS], regno))
    return 1;
      
  return TEST_HARD_REG_BIT (reg_class_contents[MOST_REGS], regno);
}

/* Implements target hook vector_mode_supported_p.  */

static bool
bfin_vector_mode_supported_p (enum machine_mode mode)
{
  return mode == V2HImode;
}

/* Return the cost of moving data from a register in class CLASS1 to
   one in class CLASS2.  A cost of 2 is the default.  */

int
bfin_register_move_cost (enum machine_mode mode ATTRIBUTE_UNUSED,
			 enum reg_class class1, enum reg_class class2)
{
  /* If optimizing for size, always prefer reg-reg over reg-memory moves.  */
  if (optimize_size)
    return 2;

  /* There are some stalls involved when moving from a DREG to a different
     class reg, and using the value in one of the following instructions.
     Attempt to model this by slightly discouraging such moves.  */
  if (class1 == DREGS && class2 != DREGS)
    return 2 * 2;

  return 2;
}

/* Return the cost of moving data of mode M between a
   register and memory.  A value of 2 is the default; this cost is
   relative to those in `REGISTER_MOVE_COST'.

   ??? In theory L1 memory has single-cycle latency.  We should add a switch
   that tells the compiler whether we expect to use only L1 memory for the
   program; it'll make the costs more accurate.  */

int
bfin_memory_move_cost (enum machine_mode mode ATTRIBUTE_UNUSED,
		       enum reg_class class,
		       int in ATTRIBUTE_UNUSED)
{
  /* Make memory accesses slightly more expensive than any register-register
     move.  Also, penalize non-DP registers, since they need secondary
     reloads to load and store.  */
  if (! reg_class_subset_p (class, DPREGS))
    return 10;

  return 8;
}

/* Inform reload about cases where moving X with a mode MODE to a register in
   CLASS requires an extra scratch register.  Return the class needed for the
   scratch register.  */

enum reg_class
secondary_input_reload_class (enum reg_class class, enum machine_mode mode,
			      rtx x)
{
  /* If we have HImode or QImode, we can only use DREGS as secondary registers;
     in most other cases we can also use PREGS.  */
  enum reg_class default_class = GET_MODE_SIZE (mode) >= 4 ? DPREGS : DREGS;
  enum reg_class x_class = NO_REGS;
  enum rtx_code code = GET_CODE (x);

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
      int large_constant_p = ! CONST_7BIT_IMM_P (INTVAL (op2));

      if (class == PREGS || class == PREGS_CLOBBERED)
	return NO_REGS;
      /* If destination is a DREG, we can do this without a scratch register
	 if the constant is valid for an add instruction.  */
      if (class == DREGS || class == DPREGS)
	return large_constant_p ? PREGS : NO_REGS;
      /* Reloading to anything other than a DREG?  Use a PREG scratch
	 register.  */
      return PREGS;
    }

  /* Data can usually be moved freely between registers of most classes.
     AREGS are an exception; they can only move to or from another register
     in AREGS or one in DREGS.  They can also be assigned the constant 0.  */
  if (x_class == AREGS)
    return class == DREGS || class == AREGS ? NO_REGS : DREGS;

  if (class == AREGS)
    {
      if (x != const0_rtx && x_class != DREGS)
	return DREGS;
      else
	return NO_REGS;
    }

  /* CCREGS can only be moved from/to DREGS.  */
  if (class == CCREGS && x_class != DREGS)
    return DREGS;
  if (x_class == CCREGS && class != DREGS)
    return DREGS;
  /* All registers other than AREGS can load arbitrary constants.  The only
     case that remains is MEM.  */
  if (code == MEM)
    if (! reg_class_subset_p (class, default_class))
      return default_class;
  return NO_REGS;
}

/* Like secondary_input_reload_class; and all we do is call that function.  */

enum reg_class
secondary_output_reload_class (enum reg_class class, enum machine_mode mode,
			       rtx x)
{
  return secondary_input_reload_class (class, mode, x);
}

/* Implement TARGET_HANDLE_OPTION.  */

static bool
bfin_handle_option (size_t code, const char *arg, int value)
{
  switch (code)
    {
    case OPT_mshared_library_id_:
      if (value > MAX_LIBRARY_ID)
	error ("-mshared-library-id=%s is not between 0 and %d",
	       arg, MAX_LIBRARY_ID);
      else
	bfin_library_id = value;
      bfin_lib_id_given = 1;
      return true;

    default:
      return true;
    }
}

/* Implement the macro OVERRIDE_OPTIONS.  */

void
override_options (void)
{
  if (TARGET_OMIT_LEAF_FRAME_POINTER)
    flag_omit_frame_pointer = 1;

  /* Library identification */
  if (bfin_lib_id_given && ! TARGET_ID_SHARED_LIBRARY)
    error ("-mshared-library-id= specified without -mid-shared-library");

  if (TARGET_ID_SHARED_LIBRARY)
    /* ??? Provide a way to use a bigger GOT.  */
    flag_pic = 1;

  flag_schedule_insns = 0;
}

/* Return the destination address of BRANCH.
   We need to use this instead of get_attr_length, because the
   cbranch_with_nops pattern conservatively sets its length to 6, and
   we still prefer to use shorter sequences.  */

static int
branch_dest (rtx branch)
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
      int pred_val = INTVAL (XEXP (x, 0));

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
asm_conditional_branch (rtx insn, rtx *operands, int n_nops, int predict_taken)
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
bfin_gen_compare (rtx cmp, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  enum rtx_code code1, code2;
  rtx op0 = bfin_compare_op0, op1 = bfin_compare_op1;
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
      emit_insn (gen_rtx_SET (BImode, tem,
			      gen_rtx_fmt_ee (code1, BImode, op0, op1)));
    }

  return gen_rtx_fmt_ee (code2, BImode, tem, CONST0_RTX (BImode));
}

/* Return nonzero iff C has exactly one bit set if it is interpreted
   as a 32 bit constant.  */

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
  enum reg_class class1 = REGNO_REG_CLASS (regno);

  /* This case takes care of single-bit set/clear constants, which we could
     also implement with BITSET/BITCLR.  */
  if (num_zero
      && shifted >= -32768 && shifted < 65536
      && (D_REGNO_P (regno)
	  || (regno >= REG_P0 && regno <= REG_P7 && num_zero <= 2)))
    {
      emit_insn (gen_movsi (operands[0], GEN_INT (shifted)));
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
	  emit_insn (gen_iorsi3 (operands[0], operands[0], GEN_INT (val & 0xFFFF0000)));
	  return 1;
	}
      else if (log2constp (val | 0xFFFF) && (val & 0x8000) != 0)
	{
	  emit_insn (gen_movsi (operands[0], GEN_INT (tmp)));
	  emit_insn (gen_andsi3 (operands[0], operands[0], GEN_INT (val | 0xFFFF)));
	}
    }

  if (D_REGNO_P (regno))
    {
      if (CONST_7BIT_IMM_P (tmp))
	{
	  emit_insn (gen_movsi (operands[0], GEN_INT (tmp)));
	  emit_insn (gen_movstricthi_high (operands[0], GEN_INT (val & -65536)));
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
      && num_compl_zero && CONST_7BIT_IMM_P (shifted_compl))
    {
      /* If optimizing for size, generate a sequence that has more instructions
	 but is shorter.  */
      emit_insn (gen_movsi (operands[0], GEN_INT (shifted_compl)));
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
bfin_valid_add (enum machine_mode mode, HOST_WIDE_INT value)
{
  unsigned HOST_WIDE_INT v = value > 0 ? value : -value;
  int sz = GET_MODE_SIZE (mode);
  int shift = sz == 1 ? 0 : sz == 2 ? 1 : 2;
  /* The usual offsettable_memref machinery doesn't work so well for this
     port, so we deal with the problem here.  */
  unsigned HOST_WIDE_INT mask = sz == 8 ? 0x7ffe : 0x7fff;
  return (v & ~(mask << shift)) == 0;
}

static bool
bfin_valid_reg_p (unsigned int regno, int strict)
{
  return ((strict && REGNO_OK_FOR_BASE_STRICT_P (regno))
	  || (!strict && REGNO_OK_FOR_BASE_NONSTRICT_P (regno)));
}

bool
bfin_legitimate_address_p (enum machine_mode mode, rtx x, int strict)
{
  switch (GET_CODE (x)) {
  case REG:
    if (bfin_valid_reg_p (REGNO (x), strict))
      return true;
    break;
  case PLUS:
    if (REG_P (XEXP (x, 0))
	&& bfin_valid_reg_p (REGNO (XEXP (x, 0)), strict)
	&& (GET_CODE (XEXP (x, 1)) == UNSPEC
	    || (GET_CODE (XEXP (x, 1)) == CONST_INT
		&& bfin_valid_add (mode, INTVAL (XEXP (x, 1))))))
      return true;
    break;
  case POST_INC:
  case POST_DEC:
    if (LEGITIMATE_MODE_FOR_AUTOINC_P (mode)
	&& REG_P (XEXP (x, 0))
	&& bfin_valid_reg_p (REGNO (XEXP (x, 0)), strict))
      return true;
  case PRE_DEC:
    if (LEGITIMATE_MODE_FOR_AUTOINC_P (mode)
	&& XEXP (x, 0) == stack_pointer_rtx
	&& REG_P (XEXP (x, 0))
	&& bfin_valid_reg_p (REGNO (XEXP (x, 0)), strict))
      return true;
    break;
  default:
    break;
  }
  return false;
}

static bool
bfin_rtx_costs (rtx x, int code, int outer_code, int *total)
{
  int cost2 = COSTS_N_INSNS (1);

  switch (code)
    {
    case CONST_INT:
      if (outer_code == SET || outer_code == PLUS)
        *total = CONST_7BIT_IMM_P (INTVAL (x)) ? 0 : cost2;
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
      if (GET_MODE (x) == Pmode)
	{
	  if (GET_CODE (XEXP (x, 0)) == MULT
	      && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT)
	    {
	      HOST_WIDE_INT val = INTVAL (XEXP (XEXP (x, 0), 1));
	      if (val == 2 || val == 4)
		{
		  *total = cost2;
		  *total += rtx_cost (XEXP (XEXP (x, 0), 0), outer_code);
		  *total += rtx_cost (XEXP (x, 1), outer_code);
		  return true;
		}
	    }
	}

      /* fall through */

    case MINUS:
    case ASHIFT: 
    case ASHIFTRT:
    case LSHIFTRT:
      if (GET_MODE (x) == DImode)
	*total = 6 * cost2;
      return false;
	  
    case AND:
    case IOR:
    case XOR:
      if (GET_MODE (x) == DImode)
	*total = 2 * cost2;
      return false;

    case MULT:
      if (GET_MODE_SIZE (GET_MODE (x)) <= UNITS_PER_WORD)
	*total = COSTS_N_INSNS (3);
      return false;

    default:
      return false;
    }
}

static void
bfin_internal_label (FILE *stream, const char *prefix, unsigned long num)
{
  fprintf (stream, "%s%s$%ld:\n", LOCAL_LABEL_PREFIX, prefix, num);
}

/* Used for communication between {push,pop}_multiple_operation (which
   we use not only as a predicate) and the corresponding output functions.  */
static int first_preg_to_save, first_dreg_to_save;

int
push_multiple_operation (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
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
  return 1;
}

int
pop_multiple_operation (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
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
  ok = push_multiple_operation (PATTERN (insn), VOIDmode);
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
  ok = pop_multiple_operation (PATTERN (insn), VOIDmode);
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
single_move_for_strmov (rtx dst, rtx src, enum machine_mode mode, HOST_WIDE_INT offset)
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
bfin_expand_strmov (rtx dst, rtx src, rtx count_exp, rtx align_exp)
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
	      single_move_for_strmov (dst, src, SImode, offset);
	      offset = 4;
	    }
	  else if (count & ~3)
	    {
	      HOST_WIDE_INT new_count = ((count >> 2) & 0x3fffffff) - 1;
	      countreg = copy_to_mode_reg (Pmode, GEN_INT (new_count));

	      emit_insn (gen_rep_movsi (destreg, srcreg, countreg, destreg, srcreg));
	    }
	}
      else
	{
	  if ((count & ~1) == 2)
	    {
	      single_move_for_strmov (dst, src, HImode, offset);
	      offset = 2;
	    }
	  else if (count & ~1)
	    {
	      HOST_WIDE_INT new_count = ((count >> 1) & 0x7fffffff) - 1;
	      countreg = copy_to_mode_reg (Pmode, GEN_INT (new_count));

	      emit_insn (gen_rep_movhi (destreg, srcreg, countreg, destreg, srcreg));
	    }
	}
      if (count & 2)
	{
	  single_move_for_strmov (dst, src, HImode, offset);
	  offset += 2;
	}
      if (count & 1)
	{
	  single_move_for_strmov (dst, src, QImode, offset);
	}
      return true;
    }
  return false;
}


static int
bfin_adjust_cost (rtx insn, rtx link, rtx dep_insn, int cost)
{
  enum attr_type insn_type, dep_insn_type;
  int dep_insn_code_number;

  /* Anti and output dependencies have zero cost.  */
  if (REG_NOTE_KIND (link) != 0)
    return 0;

  dep_insn_code_number = recog_memoized (dep_insn);

  /* If we can't recognize the insns, we can't really do anything.  */
  if (dep_insn_code_number < 0 || recog_memoized (insn) < 0)
    return cost;

  insn_type = get_attr_type (insn);
  dep_insn_type = get_attr_type (dep_insn);

  if (dep_insn_type == TYPE_MOVE || dep_insn_type == TYPE_MCLD)
    {
      rtx pat = PATTERN (dep_insn);
      rtx dest = SET_DEST (pat);
      rtx src = SET_SRC (pat);
      if (! ADDRESS_REGNO_P (REGNO (dest)) || ! D_REGNO_P (REGNO (src)))
	return cost;
      return cost + (dep_insn_type == TYPE_MOVE ? 4 : 3);
    }

  return cost;
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
   could lead to incorrect behaviour.  The manual recommends CSYNC, while
   VDSP seems to use NOPs (even though its corresponding compiler option is
   named CSYNC).

   When optimizing for speed, we emit NOPs, which seems faster than a CSYNC.
   When optimizing for size, we turn the branch into a predicted taken one.
   This may be slower due to mispredicts, but saves code size.  */

static void
bfin_reorg (void)
{
  rtx insn, last_condjump = NULL_RTX;
  int cycles_since_jump = INT_MAX;

  if (! TARGET_CSYNC)
    return;

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      rtx pat;

      if (NOTE_P (insn) || BARRIER_P (insn) || LABEL_P (insn))
	continue;

      pat = PATTERN (insn);
      if (GET_CODE (pat) == USE || GET_CODE (pat) == CLOBBER
	  || GET_CODE (pat) == ASM_INPUT || GET_CODE (pat) == ADDR_VEC
	  || GET_CODE (pat) == ADDR_DIFF_VEC || asm_noperands (pat) >= 0)
	continue;

      if (JUMP_P (insn))
	{
	  if (any_condjump_p (insn)
	      && ! cbranch_predicted_taken_p (insn))
	    {
	      last_condjump = insn;
	      cycles_since_jump = 0;
	    }
	  else
	    cycles_since_jump = INT_MAX;
	}
      else if (INSN_P (insn))
	{
	  enum attr_type type = get_attr_type (insn);
	  if (cycles_since_jump < INT_MAX)
	    cycles_since_jump++;

	  if (type == TYPE_MCLD && cycles_since_jump < 3)
	    {
	      rtx pat;

	      pat = single_set (insn);
	      if (may_trap_p (SET_SRC (pat)))
		{
		  int num_clobbers;
		  rtx *op = recog_data.operand;

		  extract_insn (last_condjump);
		  if (optimize_size)
		    pat = gen_cbranch_predicted_taken (op[0], op[1], op[2],
						       op[3]);
		  else
		    pat = gen_cbranch_with_nops (op[0], op[1], op[2], op[3],
						 GEN_INT (3 - cycles_since_jump));
		  PATTERN (last_condjump) = pat;
		  INSN_CODE (last_condjump) = recog (pat, insn, &num_clobbers);
		  cycles_since_jump = INT_MAX;
		}
	    }
	}
    }
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
      warning (0, "%qs attribute only applies to functions",
	       IDENTIFIER_POINTER (name));
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
bfin_comp_type_attributes (tree type1, tree type2)
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

  return 1;
}

/* Table of valid machine attributes.  */
const struct attribute_spec bfin_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler } */
  { "interrupt_handler", 0, 0, false, true,  true, handle_int_attribute },
  { "exception_handler", 0, 0, false, true,  true, handle_int_attribute },
  { "nmi_handler", 0, 0, false, true,  true, handle_int_attribute },
  { "nesting", 0, 0, false, true,  true, NULL },
  { "kspisusp", 0, 0, false, true,  true, NULL },
  { "saveall", 0, 0, false, true,  true, NULL },
  { NULL, 0, 0, false, false, false, NULL }
};

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
  rtx this = gen_rtx_REG (Pmode, REG_R0);

  /* Adjust the this parameter by a fixed constant.  */
  if (delta)
    {
      xops[1] = this;
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
      rtx tmp = gen_rtx_REG (Pmode, REG_R2);

      xops[1] = tmp;
      xops[2] = p2tmp;
      output_asm_insn ("%2 = r0; %2 = [%2];", xops);

      /* Adjust the this parameter.  */
      xops[0] = gen_rtx_MEM (Pmode, plus_constant (p2tmp, vcall_offset));
      if (!memory_operand (xops[0], Pmode))
	{
	  rtx tmp2 = gen_rtx_REG (Pmode, REG_P1);
	  xops[0] = GEN_INT (vcall_offset);
	  xops[1] = tmp2;
	  output_asm_insn ("%h1 = %h0; %d1 = %d0; %2 = %2 + %1", xops);
	  xops[0] = gen_rtx_MEM (Pmode, p2tmp);
	}
      xops[2] = this;
      output_asm_insn ("%1 = %0; %2 = %2 + %1;", xops);
    }

  xops[0] = XEXP (DECL_RTL (function), 0);
  if (1 || !flag_pic || (*targetm.binds_local_p) (function))
    output_asm_insn ("jump.l\t%P0", xops);
}

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

#undef TARGET_ASM_INTERNAL_LABEL
#define TARGET_ASM_INTERNAL_LABEL bfin_internal_label

#undef TARGET_MACHINE_DEPENDENT_REORG
#define TARGET_MACHINE_DEPENDENT_REORG bfin_reorg

#undef TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL bfin_function_ok_for_sibcall

#undef TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK bfin_output_mi_thunk
#undef TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK hook_bool_tree_hwi_hwi_tree_true

#undef TARGET_SCHED_ADJUST_COST
#define TARGET_SCHED_ADJUST_COST bfin_adjust_cost

#undef TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES hook_bool_tree_true
#undef TARGET_PROMOTE_FUNCTION_ARGS
#define TARGET_PROMOTE_FUNCTION_ARGS hook_bool_tree_true
#undef TARGET_PROMOTE_FUNCTION_RETURN
#define TARGET_PROMOTE_FUNCTION_RETURN hook_bool_tree_true

#undef TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES bfin_arg_partial_bytes

#undef TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE bfin_pass_by_reference

#undef TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS setup_incoming_varargs

#undef TARGET_STRUCT_VALUE_RTX
#define TARGET_STRUCT_VALUE_RTX bfin_struct_value_rtx

#undef TARGET_VECTOR_MODE_SUPPORTED_P
#define TARGET_VECTOR_MODE_SUPPORTED_P bfin_vector_mode_supported_p

#undef TARGET_HANDLE_OPTION
#define TARGET_HANDLE_OPTION bfin_handle_option

struct gcc_target targetm = TARGET_INITIALIZER;
