/* Output routines for GCC for Hitachi Super-H.
   Copyright (C) 1993-1998 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Contributed by Steve Chamberlain (sac@cygnus.com).
   Improved by Jim Wilson (wilson@cygnus.com).  */

#include "config.h"

#include <stdio.h>

#include "rtl.h"
#include "tree.h"
#include "flags.h"
#include "insn-flags.h"
#include "expr.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "output.h"
#include "insn-attr.h"

int code_for_indirect_jump_scratch = CODE_FOR_indirect_jump_scratch;

#define MSW (TARGET_LITTLE_ENDIAN ? 1 : 0)
#define LSW (TARGET_LITTLE_ENDIAN ? 0 : 1)

/* ??? The pragma interrupt support will not work for SH3.  */
/* This is set by #pragma interrupt and #pragma trapa, and causes gcc to
   output code for the next function appropriate for an interrupt handler.  */
int pragma_interrupt;

/* This is set by the trap_exit attribute for functions.   It specifies
   a trap number to be used in a trapa instruction at function exit
   (instead of an rte instruction).  */
int trap_exit;

/* This is used by the sp_switch attribute for functions.  It specifies
   a variable holding the address of the stack the interrupt function
   should switch to/from at entry/exit.  */
rtx sp_switch;

/* This is set by #pragma trapa, and is similar to the above, except that
   the compiler doesn't emit code to preserve all registers.  */
static int pragma_trapa;

/* This is set by #pragma nosave_low_regs.  This is useful on the SH3,
   which has a separate set of low regs for User and Supervisor modes.
   This should only be used for the lowest level of interrupts.  Higher levels
   of interrupts must save the registers in case they themselves are
   interrupted.  */
int pragma_nosave_low_regs;

/* This is used for communication between SETUP_INCOMING_VARARGS and
   sh_expand_prologue.  */
int current_function_anonymous_args;

/* Global variables from toplev.c and final.c that are used within, but
   not declared in any header file.  */
extern char *version_string;
extern int *insn_addresses;

/* Global variables for machine-dependent things. */

/* Which cpu are we scheduling for.  */
enum processor_type sh_cpu;

/* Saved operands from the last compare to use when we generate an scc
   or bcc insn.  */

rtx sh_compare_op0;
rtx sh_compare_op1;

enum machine_mode sh_addr_diff_vec_mode;

/* Provides the class number of the smallest class containing
   reg number.  */

int regno_reg_class[FIRST_PSEUDO_REGISTER] =
{
  R0_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, PR_REGS, T_REGS, NO_REGS,
  MAC_REGS, MAC_REGS, FPUL_REGS, GENERAL_REGS,
  FP0_REGS,FP_REGS, FP_REGS, FP_REGS,
  FP_REGS, FP_REGS, FP_REGS, FP_REGS,
  FP_REGS, FP_REGS, FP_REGS, FP_REGS,
  FP_REGS, FP_REGS, FP_REGS, FP_REGS,
  DF_REGS, DF_REGS, DF_REGS, DF_REGS,
  DF_REGS, DF_REGS, DF_REGS, DF_REGS,
  FPSCR_REGS,
};

char fp_reg_names[][5] =
{
  "fr0", "fr1", "fr2", "fr3", "fr4", "fr5", "fr6", "fr7",
  "fr8", "fr9", "fr10", "fr11", "fr12", "fr13", "fr14", "fr15",
  "fpul",
  "xd0","xd2","xd4", "xd6", "xd8", "xd10", "xd12", "xd14",
};

/* Provide reg_class from a letter such as appears in the machine
   description.  */

enum reg_class reg_class_from_letter[] =
{
  /* a */ ALL_REGS, /* b */ NO_REGS, /* c */ FPSCR_REGS, /* d */ DF_REGS,
  /* e */ NO_REGS, /* f */ FP_REGS, /* g */ NO_REGS, /* h */ NO_REGS,
  /* i */ NO_REGS, /* j */ NO_REGS, /* k */ NO_REGS, /* l */ PR_REGS,
  /* m */ NO_REGS, /* n */ NO_REGS, /* o */ NO_REGS, /* p */ NO_REGS,
  /* q */ NO_REGS, /* r */ NO_REGS, /* s */ NO_REGS, /* t */ T_REGS,
  /* u */ NO_REGS, /* v */ NO_REGS, /* w */ FP0_REGS, /* x */ MAC_REGS,
  /* y */ FPUL_REGS, /* z */ R0_REGS
};

int assembler_dialect;

rtx get_fpscr_rtx ();
void emit_sf_insn ();
void emit_df_insn ();

static void split_branches PROTO ((rtx));

/* Print the operand address in x to the stream.  */

void
print_operand_address (stream, x)
     FILE *stream;
     rtx x;
{
  switch (GET_CODE (x))
    {
    case REG:
    case SUBREG:
      fprintf (stream, "@%s", reg_names[true_regnum (x)]);
      break;

    case PLUS:
      {
	rtx base = XEXP (x, 0);
	rtx index = XEXP (x, 1);

	switch (GET_CODE (index))
	  {
	  case CONST_INT:
	    fprintf (stream, "@(%d,%s)", INTVAL (index),
		     reg_names[true_regnum (base)]);
	    break;

	  case REG:
	  case SUBREG:
	    {
	      int base_num = true_regnum (base);
	      int index_num = true_regnum (index);

	      fprintf (stream, "@(r0,%s)",
		       reg_names[MAX (base_num, index_num)]);
	      break;
	    }

	  default:
	    debug_rtx (x);
	    abort ();
	  }
      }
      break;

    case PRE_DEC:
      fprintf (stream, "@-%s", reg_names[true_regnum (XEXP (x, 0))]);
      break;

    case POST_INC:
      fprintf (stream, "@%s+", reg_names[true_regnum (XEXP (x, 0))]);
      break;

    default:
      output_addr_const (stream, x);
      break;
    }
}

/* Print operand x (an rtx) in assembler syntax to file stream
   according to modifier code.

   '.'  print a .s if insn needs delay slot
   ','  print LOCAL_LABEL_PREFIX
   '@'  print trap, rte or rts depending upon pragma interruptness
   '#'  output a nop if there is nothing to put in the delay slot
   'O'  print a constant without the #
   'R'  print the LSW of a dp value - changes if in little endian
   'S'  print the MSW of a dp value - changes if in little endian
   'T'  print the next word of a dp value - same as 'R' in big endian mode.
   'o'  output an operator.  */

void
print_operand (stream, x, code)
     FILE *stream;
     rtx x;
     int code;
{
  switch (code)
    {
    case '.':
      if (final_sequence
	  && ! INSN_ANNULLED_BRANCH_P (XVECEXP (final_sequence, 0, 0)))
	fprintf (stream, ASSEMBLER_DIALECT ? "/s" : ".s");
      break;
    case ',':
      fprintf (stream, "%s", LOCAL_LABEL_PREFIX);
      break;
    case '@':
      {
	int interrupt_handler;

	if ((lookup_attribute
	     ("interrupt_handler",
	      DECL_MACHINE_ATTRIBUTES (current_function_decl)))
	    != NULL_TREE)
	  interrupt_handler = 1;
	else
	  interrupt_handler = 0;
	
      if (trap_exit)
	fprintf (stream, "trapa #%d", trap_exit);
      else if (interrupt_handler)
	fprintf (stream, "rte");
      else
	fprintf (stream, "rts");
      break;
      }
    case '#':
      /* Output a nop if there's nothing in the delay slot.  */
      if (dbr_sequence_length () == 0)
	fprintf (stream, "\n\tnop");
      break;
    case 'O':
      output_addr_const (stream, x);
      break;
    case 'R':
      fputs (reg_names[REGNO (x) + LSW], (stream));
      break;
    case 'S':
      fputs (reg_names[REGNO (x) + MSW], (stream));
      break;
    case 'T':
      /* Next word of a double.  */
      switch (GET_CODE (x))
	{
	case REG:
	  fputs (reg_names[REGNO (x) + 1], (stream));
	  break;
	case MEM:
	  if (GET_CODE (XEXP (x, 0)) != PRE_DEC
	      && GET_CODE (XEXP (x, 0)) != POST_INC)
	    x = adj_offsettable_operand (x, 4);
	  print_operand_address (stream, XEXP (x, 0));
	  break;
	}
      break;
    case 'o':
      switch (GET_CODE (x))
	{
	case PLUS:  fputs ("add", stream); break;
	case MINUS: fputs ("sub", stream); break;
	case MULT:  fputs ("mul", stream); break;
	case DIV:   fputs ("div", stream); break;
	}
      break;
    default:
      switch (GET_CODE (x))
	{
	case REG:
	  if (REGNO (x) >= FIRST_FP_REG && REGNO (x) <= LAST_FP_REG
	      && GET_MODE_SIZE (GET_MODE (x)) > 4)
	    fprintf ((stream), "d%s", reg_names[REGNO (x)]+1);
	  else
	    fputs (reg_names[REGNO (x)], (stream));
	  break;
	case MEM:
	  output_address (XEXP (x, 0));
	  break;
	default:
	  fputc ('#', stream);
	  output_addr_const (stream, x);
	  break;
	}
      break;
    }
}

static void force_into PROTO ((rtx, rtx));

/* Like force_operand, but guarantees that VALUE ends up in TARGET.  */
static void
force_into (value, target)
     rtx value, target;
{
  value = force_operand (value, target);
  if (! rtx_equal_p (value, target))
    emit_insn (gen_move_insn (target, value));
}

/* Emit code to perform a block move.  Choose the best method.

   OPERANDS[0] is the destination.
   OPERANDS[1] is the source.
   OPERANDS[2] is the size.
   OPERANDS[3] is the alignment safe to use.  */

int
expand_block_move (operands)
     rtx *operands;
{
  int align = INTVAL (operands[3]);
  int constp = (GET_CODE (operands[2]) == CONST_INT);
  int bytes = (constp ? INTVAL (operands[2]) : 0);

  /* If it isn't a constant number of bytes, or if it doesn't have 4 byte
     alignment, or if it isn't a multiple of 4 bytes, then fail.  */
  if (! constp || align < 4 || (bytes % 4 != 0))
    return 0;

  if (TARGET_HARD_SH4)
    {
      if (bytes < 12)
	return 0;
      else if (bytes == 12)
	{
	  tree entry_name;
	  rtx func_addr_rtx;
	  rtx r4 = gen_rtx (REG, SImode, 4);
	  rtx r5 = gen_rtx (REG, SImode, 5);

	  entry_name = get_identifier ("__movstrSI12_i4");

	  func_addr_rtx
	    = copy_to_mode_reg (Pmode,
				gen_rtx_SYMBOL_REF (Pmode,
						    IDENTIFIER_POINTER (entry_name)));
	  force_into (XEXP (operands[0], 0), r4);
	  force_into (XEXP (operands[1], 0), r5);
	  emit_insn (gen_block_move_real_i4 (func_addr_rtx));
	  return 1;
	}
      else if (! TARGET_SMALLCODE)
	{
	  tree entry_name;
	  rtx func_addr_rtx;
	  int dwords;
	  rtx r4 = gen_rtx (REG, SImode, 4);
	  rtx r5 = gen_rtx (REG, SImode, 5);
	  rtx r6 = gen_rtx (REG, SImode, 6);

	  entry_name = get_identifier (bytes & 4
				       ? "__movstr_i4_odd"
				       : "__movstr_i4_even");
	  func_addr_rtx
	    = copy_to_mode_reg (Pmode,
				gen_rtx_SYMBOL_REF (Pmode,
						    IDENTIFIER_POINTER (entry_name)));
	  force_into (XEXP (operands[0], 0), r4);
	  force_into (XEXP (operands[1], 0), r5);

	  dwords = bytes >> 3;
	  emit_insn (gen_move_insn (r6, GEN_INT (dwords - 1)));
	  emit_insn (gen_block_lump_real_i4 (func_addr_rtx));
	  return 1;
	}
      else
	return 0;
    }
  if (bytes < 64)
    {
      char entry[30];
      tree entry_name;
      rtx func_addr_rtx;
      rtx r4 = gen_rtx (REG, SImode, 4);
      rtx r5 = gen_rtx (REG, SImode, 5);

      sprintf (entry, "__movstrSI%d", bytes);
      entry_name = get_identifier (entry);

      func_addr_rtx
	= copy_to_mode_reg (Pmode,
			    gen_rtx (SYMBOL_REF, Pmode,
				     IDENTIFIER_POINTER (entry_name)));
      force_into (XEXP (operands[0], 0), r4);
      force_into (XEXP (operands[1], 0), r5);
      emit_insn (gen_block_move_real (func_addr_rtx));
      return 1;
    }

  /* This is the same number of bytes as a memcpy call, but to a different
     less common function name, so this will occasionally use more space.  */
  if (! TARGET_SMALLCODE)
    {
      tree entry_name;
      rtx func_addr_rtx;
      int final_switch, while_loop;
      rtx r4 = gen_rtx (REG, SImode, 4);
      rtx r5 = gen_rtx (REG, SImode, 5);
      rtx r6 = gen_rtx (REG, SImode, 6);

      entry_name = get_identifier ("__movstr");
      func_addr_rtx
	= copy_to_mode_reg (Pmode,
			    gen_rtx (SYMBOL_REF, Pmode,
				     IDENTIFIER_POINTER (entry_name)));
      force_into (XEXP (operands[0], 0), r4);
      force_into (XEXP (operands[1], 0), r5);

      /* r6 controls the size of the move.  16 is decremented from it
	 for each 64 bytes moved.  Then the negative bit left over is used
	 as an index into a list of move instructions.  e.g., a 72 byte move
	 would be set up with size(r6) = 14, for one iteration through the
	 big while loop, and a switch of -2 for the last part.  */

      final_switch = 16 - ((bytes / 4) % 16);
      while_loop = ((bytes / 4) / 16 - 1) * 16;
      emit_insn (gen_move_insn (r6, GEN_INT (while_loop + final_switch)));
      emit_insn (gen_block_lump_real (func_addr_rtx));
      return 1;
    }

  return 0;
}

/* Prepare operands for a move define_expand; specifically, one of the
   operands must be in a register.  */

int
prepare_move_operands (operands, mode)
     rtx operands[];
     enum machine_mode mode;
{
  if (! reload_in_progress && ! reload_completed)
    {
      /* Copy the source to a register if both operands aren't registers.  */
      if (! register_operand (operands[0], mode)
	  && ! register_operand (operands[1], mode))
	operands[1] = copy_to_mode_reg (mode, operands[1]);

      /* This case can happen while generating code to move the result
	 of a library call to the target.  Reject `st r0,@(rX,rY)' because
	 reload will fail to find a spill register for rX, since r0 is already
	 being used for the source.  */
      else if (GET_CODE (operands[1]) == REG && REGNO (operands[1]) == 0
	       && GET_CODE (operands[0]) == MEM
	       && GET_CODE (XEXP (operands[0], 0)) == PLUS
	       && GET_CODE (XEXP (XEXP (operands[0], 0), 1)) == REG)
	operands[1] = copy_to_mode_reg (mode, operands[1]);
    }

  return 0;
}

/* Prepare the operands for an scc instruction; make sure that the
   compare has been done.  */
rtx
prepare_scc_operands (code)
     enum rtx_code code;
{
  rtx t_reg = gen_rtx (REG, SImode, T_REG);
  enum rtx_code oldcode = code;
  enum machine_mode mode;

  /* First need a compare insn.  */
  switch (code)
    {
    case NE:
      /* It isn't possible to handle this case.  */
      abort ();
    case LT:
      code = GT;
      break;
    case LE:
      code = GE;
      break;
    case LTU:
      code = GTU;
      break;
    case LEU:
      code = GEU;
      break;
    }
  if (code != oldcode)
    {
      rtx tmp = sh_compare_op0;
      sh_compare_op0 = sh_compare_op1;
      sh_compare_op1 = tmp;
    }

  mode = GET_MODE (sh_compare_op0);
  if (mode == VOIDmode)
    mode = GET_MODE (sh_compare_op1);

  sh_compare_op0 = force_reg (mode, sh_compare_op0);
  if ((code != EQ && code != NE
       && (sh_compare_op1 != const0_rtx
	   || code == GTU  || code == GEU || code == LTU || code == LEU))
      || TARGET_SH3E && GET_MODE_CLASS (mode) == MODE_FLOAT)
    sh_compare_op1 = force_reg (mode, sh_compare_op1);

  if (TARGET_SH4 && GET_MODE_CLASS (mode) == MODE_FLOAT)
    (mode == SFmode ? emit_sf_insn : emit_df_insn)
     (gen_rtx (PARALLEL, VOIDmode, gen_rtvec (2,
		gen_rtx (SET, VOIDmode, t_reg,
			 gen_rtx (code, SImode,
				  sh_compare_op0, sh_compare_op1)),
		gen_rtx (USE, VOIDmode, get_fpscr_rtx ()))));
  else
    emit_insn (gen_rtx (SET, VOIDmode, t_reg,
			gen_rtx (code, SImode, sh_compare_op0,
				 sh_compare_op1)));

  return t_reg;
}

/* Called from the md file, set up the operands of a compare instruction.  */

void
from_compare (operands, code)
     rtx *operands;
     int code;
{
  enum machine_mode mode = GET_MODE (sh_compare_op0);
  rtx insn;
  if (mode == VOIDmode)
    mode = GET_MODE (sh_compare_op1);
  if (code != EQ
      || mode == DImode
      || (TARGET_SH3E && GET_MODE_CLASS (mode) == MODE_FLOAT))
    {
      /* Force args into regs, since we can't use constants here.  */
      sh_compare_op0 = force_reg (mode, sh_compare_op0);
      if (sh_compare_op1 != const0_rtx
	  || code == GTU  || code == GEU
	  || (TARGET_SH3E && GET_MODE_CLASS (mode) == MODE_FLOAT))
	sh_compare_op1 = force_reg (mode, sh_compare_op1);
    }
  if (TARGET_SH3E && GET_MODE_CLASS (mode) == MODE_FLOAT && code == GE)
    {
      from_compare (operands, GT);
      insn = gen_ieee_ccmpeqsf_t (sh_compare_op0, sh_compare_op1);
    }
  else
    insn = gen_rtx (SET, VOIDmode,
		    gen_rtx (REG, SImode, 18),
		    gen_rtx (code, SImode, sh_compare_op0, sh_compare_op1));
  if (TARGET_SH4 && GET_MODE_CLASS (mode) == MODE_FLOAT)
    {
      insn = gen_rtx (PARALLEL, VOIDmode,
		      gen_rtvec (2, insn,
				 gen_rtx (USE, VOIDmode, get_fpscr_rtx ())));
      (mode == SFmode ? emit_sf_insn : emit_df_insn) (insn);
    }
  else
    emit_insn (insn);
}

/* Functions to output assembly code.  */

/* Return a sequence of instructions to perform DI or DF move.

   Since the SH cannot move a DI or DF in one instruction, we have
   to take care when we see overlapping source and dest registers.  */

char *
output_movedouble (insn, operands, mode)
     rtx insn;
     rtx operands[];
     enum machine_mode mode;
{
  rtx dst = operands[0];
  rtx src = operands[1];

  if (GET_CODE (dst) == MEM
      && GET_CODE (XEXP (dst, 0)) == PRE_DEC)
    return "mov.l	%T1,%0\n\tmov.l	%1,%0";

  if (register_operand (dst, mode)
      && register_operand (src, mode))
    {
      if (REGNO (src) == MACH_REG)
	return "sts	mach,%S0\n\tsts	macl,%R0";

      /* When mov.d r1,r2 do r2->r3 then r1->r2;
         when mov.d r1,r0 do r1->r0 then r2->r1.  */

      if (REGNO (src) + 1 == REGNO (dst))
	return "mov	%T1,%T0\n\tmov	%1,%0";
      else
	return "mov	%1,%0\n\tmov	%T1,%T0";
    }
  else if (GET_CODE (src) == CONST_INT)
    {
      if (INTVAL (src) < 0)
	output_asm_insn ("mov	#-1,%S0", operands);
      else
	output_asm_insn ("mov	#0,%S0", operands);

      return "mov	%1,%R0";
    }
  else if (GET_CODE (src) == MEM)
    {
      int ptrreg = -1;
      int dreg = REGNO (dst);
      rtx inside = XEXP (src, 0);

      if (GET_CODE (inside) == REG)
	ptrreg = REGNO (inside);
      else if (GET_CODE (inside) == SUBREG)
	ptrreg = REGNO (SUBREG_REG (inside)) + SUBREG_WORD (inside);
      else if (GET_CODE (inside) == PLUS)
	{
	  ptrreg = REGNO (XEXP (inside, 0));
	  /* ??? A r0+REG address shouldn't be possible here, because it isn't
	     an offsettable address.  Unfortunately, offsettable addresses use
	     QImode to check the offset, and a QImode offsettable address
	     requires r0 for the other operand, which is not currently
	     supported, so we can't use the 'o' constraint.
	     Thus we must check for and handle r0+REG addresses here.
	     We punt for now, since this is likely very rare.  */
	  if (GET_CODE (XEXP (inside, 1)) == REG)
	    abort ();
	}
      else if (GET_CODE (inside) == LABEL_REF)
	return "mov.l	%1,%0\n\tmov.l	%1+4,%T0";
      else if (GET_CODE (inside) == POST_INC)
	return "mov.l	%1,%0\n\tmov.l	%1,%T0";
      else
	abort ();

      /* Work out the safe way to copy.  Copy into the second half first.  */
      if (dreg == ptrreg)
	return "mov.l	%T1,%T0\n\tmov.l	%1,%0";
    }

  return "mov.l	%1,%0\n\tmov.l	%T1,%T0";
}

/* Print an instruction which would have gone into a delay slot after
   another instruction, but couldn't because the other instruction expanded
   into a sequence where putting the slot insn at the end wouldn't work.  */

static void
print_slot (insn)
     rtx insn;
{
  final_scan_insn (XVECEXP (insn, 0, 1), asm_out_file, optimize, 0, 1);

  INSN_DELETED_P (XVECEXP (insn, 0, 1)) = 1;
}

char *
output_far_jump (insn, op)
     rtx insn;
     rtx op;
{
  struct { rtx lab, reg, op; } this;
  char *jump;
  int far;
  int offset = branch_dest (insn) - insn_addresses[INSN_UID (insn)];

  this.lab = gen_label_rtx ();

  if (TARGET_SH2
      && offset >= -32764
      && offset - get_attr_length (insn) <= 32766)
    {
      far = 0;
      jump = "mov.w	%O0,%1;braf	%1";
    }
  else
    {
      far = 1;
      jump = "mov.l	%O0,%1;jmp	@%1";
    }
  /* If we have a scratch register available, use it.  */
  if (GET_CODE (PREV_INSN (insn)) == INSN
      && INSN_CODE (PREV_INSN (insn)) == CODE_FOR_indirect_jump_scratch)
    {
      this.reg = SET_DEST (PATTERN (PREV_INSN (insn)));
      output_asm_insn (jump, &this.lab);
      if (dbr_sequence_length ())
	print_slot (final_sequence);
      else
	output_asm_insn ("nop", 0);
    }
  else
    {
      /* Output the delay slot insn first if any.  */
      if (dbr_sequence_length ())
	print_slot (final_sequence);

      this.reg = gen_rtx (REG, SImode, 13);
      output_asm_insn ("mov.l	r13,@-r15", 0);
      output_asm_insn (jump, &this.lab);
      output_asm_insn ("mov.l	@r15+,r13", 0);
    }
  if (far)
    output_asm_insn (".align	2", 0);
  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L", CODE_LABEL_NUMBER (this.lab));
  this.op = op;
  output_asm_insn (far ? ".long	%O2" : ".word %O2-%O0", &this.lab);
  return "";
}

/* Local label counter, used for constants in the pool and inside
   pattern branches.  */

static int lf = 100;

/* Output code for ordinary branches.  */

char *
output_branch (logic, insn, operands)
     int logic;
     rtx insn;
     rtx *operands;
{
  switch (get_attr_length (insn))
    {
    case 6:
      /* This can happen if filling the delay slot has caused a forward
	 branch to exceed its range (we could reverse it, but only
	 when we know we won't overextend other branches; this should
	 best be handled by relaxation).
	 It can also happen when other condbranches hoist delay slot insn
	 from their destination, thus leading to code size increase.
	 But the branch will still be in the range -4092..+4098 bytes.  */

      if (! TARGET_RELAX)
	{
	  int label = lf++;
	  /* The call to print_slot will clobber the operands.  */
	  rtx op0 = operands[0];
    
	  /* If the instruction in the delay slot is annulled (true), then
	     there is no delay slot where we can put it now.  The only safe
	     place for it is after the label.  final will do that by default.  */
    
	  if (final_sequence
	      && ! INSN_ANNULLED_BRANCH_P (XVECEXP (final_sequence, 0, 0)))
	    {
	      asm_fprintf (asm_out_file, "\tb%s%ss\t%LLF%d\n", logic ? "f" : "t",
	                   ASSEMBLER_DIALECT ? "/" : ".", label);
	      print_slot (final_sequence);
	    }
	  else
	    asm_fprintf (asm_out_file, "\tb%s\t%LLF%d\n", logic ? "f" : "t", label);
    
	  output_asm_insn ("bra\t%l0", &op0);
	  fprintf (asm_out_file, "\tnop\n");
	  ASM_OUTPUT_INTERNAL_LABEL(asm_out_file, "LF", label);
    
	  return "";
	}
      /* When relaxing, handle this like a short branch.  The linker
	 will fix it up if it still doesn't fit after relaxation.  */
    case 2:
      return logic ? "bt%.\t%l0" : "bf%.\t%l0";
    default:
      abort ();
    }
}

char *
output_branchy_insn (code, template, insn, operands)
     char *template;
     enum rtx_code code;
     rtx insn;
     rtx *operands;
{
  rtx next_insn = NEXT_INSN (insn);
  int label_nr;

  if (next_insn && GET_CODE (next_insn) == JUMP_INSN && condjump_p (next_insn))
    {
      rtx src = SET_SRC (PATTERN (next_insn));
      if (GET_CODE (src) == IF_THEN_ELSE && GET_CODE (XEXP (src, 0)) != code)
	{
	  /* Following branch not taken */
	  operands[9] = gen_label_rtx ();
	  emit_label_after (operands[9], next_insn);
	  return template;
	}
      else
	{
	  int offset = (branch_dest (next_insn)
			- insn_addresses[INSN_UID (next_insn)] + 4);
	  if (offset >= -252 && offset <= 258)
	    {
	      if (GET_CODE (src) == IF_THEN_ELSE)
		/* branch_true */
		src = XEXP (src, 1);
	      operands[9] = src;
	      return template;
	    }
	}
    }
  operands[9] = gen_label_rtx ();
  emit_label_after (operands[9], insn);
  return template;
}

char *
output_ieee_ccmpeq (insn, operands)
     rtx insn, operands;
{
  output_branchy_insn (NE, "bt\t%l9\\;fcmp/eq\t%1,%0", insn, operands);
}

/* Output to FILE the start of the assembler file.  */

void
output_file_start (file)
     FILE *file;
{
  register int pos;

  output_file_directive (file, main_input_filename);

  /* Switch to the data section so that the coffsem symbol and the
     gcc2_compiled. symbol aren't in the text section.  */
  data_section ();

  if (TARGET_LITTLE_ENDIAN)
    fprintf (file, "\t.little\n");
}

/* Actual number of instructions used to make a shift by N.  */
static char ashiftrt_insns[] =
  { 0,1,2,3,4,5,8,8,8,8,8,8,8,8,8,8,2,3,4,5,8,8,8,8,8,8,8,8,8,8,8,2};

/* Left shift and logical right shift are the same.  */
static char shift_insns[]    =
  { 0,1,1,2,2,3,3,4,1,2,2,3,3,4,3,3,1,2,2,3,3,4,3,3,2,3,3,4,4,4,3,3};

/* Individual shift amounts needed to get the above length sequences.
   One bit right shifts clobber the T bit, so when possible, put one bit
   shifts in the middle of the sequence, so the ends are eligible for
   branch delay slots.  */
static short shift_amounts[32][5] = {
  {0}, {1}, {2}, {2, 1},
  {2, 2}, {2, 1, 2}, {2, 2, 2}, {2, 2, 1, 2},
  {8}, {8, 1}, {8, 2}, {8, 1, 2},
  {8, 2, 2}, {8, 2, 1, 2}, {8, -2, 8}, {8, -1, 8},
  {16}, {16, 1}, {16, 2}, {16, 1, 2},
  {16, 2, 2}, {16, 2, 1, 2}, {16, -2, 8}, {16, -1, 8},
  {16, 8}, {16, 1, 8}, {16, 8, 2}, {16, 8, 1, 2},
  {16, 8, 2, 2}, {16, -1, -2, 16}, {16, -2, 16}, {16, -1, 16}};

/* Likewise, but for shift amounts < 16, up to three highmost bits
   might be clobbered.  This is typically used when combined with some
   kind of sign or zero extension.  */
   
static char ext_shift_insns[]    =
  { 0,1,1,2,2,3,2,2,1,2,2,3,3,3,2,2,1,2,2,3,3,4,3,3,2,3,3,4,4,4,3,3};

static short ext_shift_amounts[32][4] = {
  {0}, {1}, {2}, {2, 1},
  {2, 2}, {2, 1, 2}, {8, -2}, {8, -1},
  {8}, {8, 1}, {8, 2}, {8, 1, 2},
  {8, 2, 2}, {16, -2, -1}, {16, -2}, {16, -1},
  {16}, {16, 1}, {16, 2}, {16, 1, 2},
  {16, 2, 2}, {16, 2, 1, 2}, {16, -2, 8}, {16, -1, 8},
  {16, 8}, {16, 1, 8}, {16, 8, 2}, {16, 8, 1, 2},
  {16, 8, 2, 2}, {16, -1, -2, 16}, {16, -2, 16}, {16, -1, 16}};

/* Assuming we have a value that has been sign-extended by at least one bit,
   can we use the ext_shift_amounts with the last shift turned to an arithmetic shift
   to shift it by N without data loss, and quicker than by other means?  */
#define EXT_SHIFT_SIGNED(n) (((n) | 8) == 15)

/* This is used in length attributes in sh.md to help compute the length
   of arbitrary constant shift instructions.  */

int
shift_insns_rtx (insn)
     rtx insn;
{
  rtx set_src = SET_SRC (XVECEXP (PATTERN (insn), 0, 0));
  int shift_count = INTVAL (XEXP (set_src, 1));
  enum rtx_code shift_code = GET_CODE (set_src);

  switch (shift_code)
    {
    case ASHIFTRT:
      return ashiftrt_insns[shift_count];
    case LSHIFTRT:
    case ASHIFT:
      return shift_insns[shift_count];
    default:
      abort();
    }
}

/* Return the cost of a shift.  */

int
shiftcosts (x)
     rtx x;
{
  int value = INTVAL (XEXP (x, 1));

  /* If shift by a non constant, then this will be expensive.  */
  if (GET_CODE (XEXP (x, 1)) != CONST_INT)
    return SH_DYNAMIC_SHIFT_COST;

  /* Otherwise, return the true cost in instructions.  */
  if (GET_CODE (x) == ASHIFTRT)
    {
      int cost = ashiftrt_insns[value];
      /* If SH3, then we put the constant in a reg and use shad.  */
      if (cost > 1 + SH_DYNAMIC_SHIFT_COST)
	cost = 1 + SH_DYNAMIC_SHIFT_COST;
      return cost;
    }
  else
    return shift_insns[value];
}

/* Return the cost of an AND operation.  */

int
andcosts (x)
     rtx x;
{
  int i;

  /* Anding with a register is a single cycle and instruction.  */
  if (GET_CODE (XEXP (x, 1)) != CONST_INT)
    return 1;

  i = INTVAL (XEXP (x, 1));
  /* These constants are single cycle extu.[bw] instructions.  */
  if (i == 0xff || i == 0xffff)
    return 1;
  /* Constants that can be used in an and immediate instruction is a single
     cycle, but this requires r0, so make it a little more expensive.  */
  if (CONST_OK_FOR_L (i))
    return 2;
  /* Constants that can be loaded with a mov immediate and an and.
     This case is probably unnecessary.  */
  if (CONST_OK_FOR_I (i))
    return 2;
  /* Any other constants requires a 2 cycle pc-relative load plus an and.
     This case is probably unnecessary.  */
  return 3;
}

/* Return the cost of a multiply.  */
int
multcosts (x)
     rtx x;
{
  if (TARGET_SH2)
    {
      /* We have a mul insn, so we can never take more than the mul and the
	 read of the mac reg, but count more because of the latency and extra
	 reg usage.  */
      if (TARGET_SMALLCODE)
	return 2;
      return 3;
    }

  /* If we're aiming at small code, then just count the number of
     insns in a multiply call sequence.  */
  if (TARGET_SMALLCODE)
    return 5;

  /* Otherwise count all the insns in the routine we'd be calling too.  */
  return 20;
}

/* Code to expand a shift.  */

void
gen_ashift (type, n, reg)
     int type;
     int n;
     rtx reg;
{
  /* Negative values here come from the shift_amounts array.  */
  if (n < 0)
    {
      if (type == ASHIFT)
	type = LSHIFTRT;
      else
	type = ASHIFT;
      n = -n;
    }

  switch (type)
    {
    case ASHIFTRT:
      emit_insn (gen_ashrsi3_k (reg, reg, GEN_INT (n)));
      break;
    case LSHIFTRT:
      if (n == 1)
	emit_insn (gen_lshrsi3_m (reg, reg, GEN_INT (n)));
      else
	emit_insn (gen_lshrsi3_k (reg, reg, GEN_INT (n)));
      break;
    case ASHIFT:
      emit_insn (gen_ashlsi3_k (reg, reg, GEN_INT (n)));
      break;
    }
}

/* Same for HImode */

void
gen_ashift_hi (type, n, reg)
     int type;
     int n;
     rtx reg;
{
  /* Negative values here come from the shift_amounts array.  */
  if (n < 0)
    {
      if (type == ASHIFT)
	type = LSHIFTRT;
      else
	type = ASHIFT;
      n = -n;
    }

  switch (type)
    {
    case ASHIFTRT:
    case LSHIFTRT:
      /* We don't have HImode right shift operations because using the
	 ordinary 32 bit shift instructions for that doesn't generate proper
	 zero/sign extension.
	 gen_ashift_hi is only called in contexts where we know that the
	 sign extension works out correctly.  */
      {
	int word = 0;
	if (GET_CODE (reg) == SUBREG)
	  {
	    word = SUBREG_WORD (reg);
	    reg = SUBREG_REG (reg);
	  }
	gen_ashift (type, n, gen_rtx_SUBREG (SImode, reg, word));
	break;
      }
    case ASHIFT:
      emit_insn (gen_ashlhi3_k (reg, reg, GEN_INT (n)));
      break;
    }
}

/* Output RTL to split a constant shift into its component SH constant
   shift instructions.  */
   
int
gen_shifty_op (code, operands)
     int code;
     rtx *operands;
{
  int value = INTVAL (operands[2]);
  int max, i;

  /* Truncate the shift count in case it is out of bounds.  */
  value = value & 0x1f;
 
  if (value == 31)
    {
      if (code == LSHIFTRT)
	{
	  emit_insn (gen_rotlsi3_1 (operands[0], operands[0]));
	  emit_insn (gen_movt (operands[0]));
	  return;
	}
      else if (code == ASHIFT)
	{
	  /* There is a two instruction sequence for 31 bit left shifts,
	     but it requires r0.  */
	  if (GET_CODE (operands[0]) == REG && REGNO (operands[0]) == 0)
	    {
	      emit_insn (gen_andsi3 (operands[0], operands[0], const1_rtx));
	      emit_insn (gen_rotlsi3_31 (operands[0], operands[0]));
	      return;
	    }
	}
    }
  else if (value == 0)
    {
      /* This can happen when not optimizing.  We must output something here
	 to prevent the compiler from aborting in final.c after the try_split
	 call.  */
      emit_insn (gen_nop ());
      return;
    }

  max = shift_insns[value];
  for (i = 0; i < max; i++)
    gen_ashift (code, shift_amounts[value][i], operands[0]);
}
   
/* Same as above, but optimized for values where the topmost bits don't
   matter.  */

int
gen_shifty_hi_op (code, operands)
     int code;
     rtx *operands;
{
  int value = INTVAL (operands[2]);
  int max, i;
  void (*gen_fun)();

  /* This operation is used by and_shl for SImode values with a few
     high bits known to be cleared.  */
  value &= 31;
  if (value == 0)
    {
      emit_insn (gen_nop ());
      return;
    }

  gen_fun = GET_MODE (operands[0]) == HImode ? gen_ashift_hi : gen_ashift;
  if (code == ASHIFT)
    {
      max = ext_shift_insns[value];
      for (i = 0; i < max; i++)
	gen_fun (code, ext_shift_amounts[value][i], operands[0]);
    }
  else
    /* When shifting right, emit the shifts in reverse order, so that
       solitary negative values come first.  */
    for (i = ext_shift_insns[value] - 1; i >= 0; i--)
      gen_fun (code, ext_shift_amounts[value][i], operands[0]);
}

/* Output RTL for an arithmetic right shift.  */

/* ??? Rewrite to use super-optimizer sequences.  */

int
expand_ashiftrt (operands)
     rtx *operands;
{
  rtx wrk;
  char func[18];
  tree func_name;
  int value;

  if (TARGET_SH3)
    {
      if (GET_CODE (operands[2]) != CONST_INT)
	{
	  rtx count = copy_to_mode_reg (SImode, operands[2]);
	  emit_insn (gen_negsi2 (count, count));
	  emit_insn (gen_ashrsi3_d (operands[0], operands[1], count));
	  return 1;
	}
      else if (ashiftrt_insns[INTVAL (operands[2]) & 31]
	       > 1 + SH_DYNAMIC_SHIFT_COST)
	{
	  rtx count
	    = force_reg (SImode, GEN_INT (- (INTVAL (operands[2]) & 31)));
	  emit_insn (gen_ashrsi3_d (operands[0], operands[1], count));
	  return 1;
	}
    }
  if (GET_CODE (operands[2]) != CONST_INT)
    return 0;

  value = INTVAL (operands[2]) & 31;

  if (value == 31)
    {
      emit_insn (gen_ashrsi2_31 (operands[0], operands[1]));
      return 1;
    }
  else if (value >= 16 && value <= 19)
    {
      wrk = gen_reg_rtx (SImode);
      emit_insn (gen_ashrsi2_16 (wrk, operands[1]));
      value -= 16;
      while (value--)
	gen_ashift (ASHIFTRT, 1, wrk);
      emit_move_insn (operands[0], wrk);
      return 1;
    }
  /* Expand a short sequence inline, longer call a magic routine.  */
  else if (value <= 5)
    {
      wrk = gen_reg_rtx (SImode);
      emit_move_insn (wrk, operands[1]);
      while (value--)
	gen_ashift (ASHIFTRT, 1, wrk);
      emit_move_insn (operands[0], wrk);
      return 1;
    }

  wrk = gen_reg_rtx (Pmode);

  /* Load the value into an arg reg and call a helper.  */
  emit_move_insn (gen_rtx (REG, SImode, 4), operands[1]);
  sprintf (func, "__ashiftrt_r4_%d", value);
  func_name = get_identifier (func);
  emit_move_insn (wrk, gen_rtx (SYMBOL_REF, Pmode,
				IDENTIFIER_POINTER (func_name)));
  emit_insn (gen_ashrsi3_n (GEN_INT (value), wrk));
  emit_move_insn (operands[0], gen_rtx (REG, SImode, 4));
  return 1;
}

int sh_dynamicalize_shift_p (count)
     rtx count;
{
  return shift_insns[INTVAL (count)] > 1 + SH_DYNAMIC_SHIFT_COST;
}

/* Try to find a good way to implement the combiner pattern
  [(set (match_operand:SI 0 "register_operand" "r")
        (and:SI (ashift:SI (match_operand:SI 1 "register_operand" "r")
                           (match_operand:SI 2 "const_int_operand" "n"))
                (match_operand:SI 3 "const_int_operand" "n"))) .
  LEFT_RTX is operand 2 in the above pattern, and MASK_RTX is operand 3.
  return 0 for simple right / left or left/right shift combination.
  return 1 for a combination of shifts with zero_extend.
  return 2 for a combination of shifts with an AND that needs r0.
  return 3 for a combination of shifts with an AND that needs an extra
    scratch register, when the three highmost bits of the AND mask are clear.
  return 4 for a combination of shifts with an AND that needs an extra
    scratch register, when any of the three highmost bits of the AND mask
    is set.
  If ATTRP is set, store an initial right shift width in ATTRP[0],
  and the instruction length in ATTRP[1] .  These values are not valid
  when returning 0.
  When ATTRP is set and returning 1, ATTRP[2] gets set to the index into
  shift_amounts for the last shift value that is to be used before the
  sign extend.  */
int
shl_and_kind (left_rtx, mask_rtx, attrp)
     rtx left_rtx, mask_rtx;
     int *attrp;
{
  unsigned HOST_WIDE_INT mask, lsb, mask2, lsb2;
  int left = INTVAL (left_rtx), right;
  int best = 0;
  int cost, best_cost = 10000;
  int best_right = 0, best_len = 0;
  int i;
  int can_ext;

  if (left < 0 || left > 31)
    return 0;
  if (GET_CODE (mask_rtx) == CONST_INT)
    mask = (unsigned HOST_WIDE_INT) INTVAL (mask_rtx) >> left;
  else
    mask = (unsigned HOST_WIDE_INT) GET_MODE_MASK (SImode) >> left;
  /* Can this be expressed as a right shift / left shift pair ? */
  lsb = ((mask ^ (mask - 1)) >> 1) + 1;
  right = exact_log2 (lsb);
  mask2 = ~(mask + lsb - 1);
  lsb2 = ((mask2 ^ (mask2 - 1)) >> 1) + 1;
  /* mask has no zeroes but trailing zeroes <==> ! mask2 */
  if (! mask2)
    best_cost = shift_insns[right] + shift_insns[right + left];
  /* mask has no trailing zeroes <==> ! right */
  else if (! right && mask2 == ~(lsb2 - 1))
    {
      int late_right = exact_log2 (lsb2);
      best_cost = shift_insns[left + late_right] + shift_insns[late_right];
    }
  /* Try to use zero extend */
  if (mask2 == ~(lsb2 - 1))
    {
      int width, first;

      for (width = 8; width <= 16; width += 8)
	{
	  /* Can we zero-extend right away? */
	  if (lsb2 == (HOST_WIDE_INT)1 << width)
	    {
	      cost
		= 1 + ext_shift_insns[right] + ext_shift_insns[left + right];
	      if (cost < best_cost)
		{
		  best = 1;
		  best_cost = cost;
		  best_right = right;
		  best_len = cost;
		  if (attrp)
		    attrp[2] = -1;
		}
	      continue;
	    }
	  /* ??? Could try to put zero extend into initial right shift,
	     or even shift a bit left before the right shift. */
	  /* Determine value of first part of left shift, to get to the
	     zero extend cut-off point.  */
	  first = width - exact_log2 (lsb2) + right;
	  if (first >= 0 && right + left - first >= 0)
	    {
	      cost = ext_shift_insns[right] + ext_shift_insns[first] + 1
		+ ext_shift_insns[right + left - first];
	      if (cost < best_cost)
		{
		  best = 1;
		  best_cost = cost;
		  best_right = right;
		  best_len = cost;
		  if (attrp)
		    attrp[2] = first;
		  }
	    }
	}
    }
  /* Try to use r0 AND pattern */
  for (i = 0; i <= 2; i++)
    {
      if (i > right)
	break;
      if (! CONST_OK_FOR_L (mask >> i))
	continue;
      cost = (i != 0) + 2 + ext_shift_insns[left + i];
      if (cost < best_cost)
	{
	  best = 2;
	  best_cost = cost;
	  best_right = i;
	  best_len = cost - 1;
	}
    }
  /* Try to use a scratch register to hold the AND operand.  */
  can_ext = ((mask << left) & 0xe0000000) == 0;
  for (i = 0; i <= 2; i++)
    {
      if (i > right)
	break;
      cost = (i != 0) + (CONST_OK_FOR_I (mask >> i) ? 2 : 3)
	+ (can_ext ? ext_shift_insns : shift_insns)[left + i];
      if (cost < best_cost)
	{
	  best = 4 - can_ext;
	  best_cost = cost;
	  best_right = i;
	  best_len = cost - 1 - ! CONST_OK_FOR_I (mask >> i);
	}
    }

  if (attrp)
    {
      attrp[0] = best_right;
      attrp[1] = best_len;
    }
  return best;
}

/* This is used in length attributes of the unnamed instructions
   corresponding to shl_and_kind return values of 1 and 2.  */
int
shl_and_length (insn)
     rtx insn;
{
  rtx set_src, left_rtx, mask_rtx;
  int attributes[3];

  set_src = SET_SRC (XVECEXP (PATTERN (insn), 0, 0));
  left_rtx = XEXP (XEXP (set_src, 0), 1);
  mask_rtx = XEXP (set_src, 1);
  shl_and_kind (left_rtx, mask_rtx, attributes);
  return attributes[1];
}

/* This is used in length attribute of the and_shl_scratch instruction.  */

int
shl_and_scr_length (insn)
     rtx insn;
{
  rtx set_src = SET_SRC (XVECEXP (PATTERN (insn), 0, 0));
  int len = shift_insns[INTVAL (XEXP (set_src, 1))];
  rtx op = XEXP (set_src, 0);
  len += shift_insns[INTVAL (XEXP (op, 1))] + 1;
  op = XEXP (XEXP (op, 0), 0);
  return len + shift_insns[INTVAL (XEXP (op, 1))];
}

/* Generating rtl? */
extern int rtx_equal_function_value_matters;

/* Generate rtl for instructions for which shl_and_kind advised a particular
   method of generating them, i.e. returned zero.  */

int
gen_shl_and (dest, left_rtx, mask_rtx, source)
     rtx dest, left_rtx, mask_rtx, source;
{
  int attributes[3];
  unsigned HOST_WIDE_INT mask;
  int kind = shl_and_kind (left_rtx, mask_rtx, attributes);
  int right, total_shift;
  int (*shift_gen_fun) PROTO((int, rtx*)) = gen_shifty_hi_op;

  right = attributes[0];
  total_shift = INTVAL (left_rtx) + right;
  mask = (unsigned HOST_WIDE_INT) INTVAL (mask_rtx) >> total_shift;
  switch (kind)
    {
    default:
      return -1;
    case 1:
      {
	int first = attributes[2];
	rtx operands[3];

	if (first < 0)
	  {
	    emit_insn ((mask << right) <= 0xff
		       ? gen_zero_extendqisi2(dest,
					      gen_lowpart (QImode, source))
		       : gen_zero_extendhisi2(dest,
					      gen_lowpart (HImode, source)));
	    source = dest;
	  }
	if (source != dest)
	  emit_insn (gen_movsi (dest, source));
	operands[0] = dest;
	if (right)
	  {
	    operands[2] = GEN_INT (right);
	    gen_shifty_hi_op (LSHIFTRT, operands);
	  }
	if (first > 0)
	  {
	    operands[2] = GEN_INT (first);
	    gen_shifty_hi_op (ASHIFT, operands);
	    total_shift -= first;
	    mask <<= first;
	  }
	if (first >= 0)
	  emit_insn (mask <= 0xff
		     ? gen_zero_extendqisi2(dest, gen_lowpart (QImode, dest))
		     : gen_zero_extendhisi2(dest, gen_lowpart (HImode, dest)));
	if (total_shift > 0)
	  {
	    operands[2] = GEN_INT (total_shift);
	    gen_shifty_hi_op (ASHIFT, operands);
	  }
	break;
      }
    case 4:
      shift_gen_fun = gen_shifty_op;
    case 3:
      /* If the topmost bit that matters is set, set the topmost bits
	 that don't matter.  This way, we might be able to get a shorter
	 signed constant.  */
      if (mask & ((HOST_WIDE_INT)1 << 31 - total_shift))
	mask |= (HOST_WIDE_INT)~0 << (31 - total_shift);
    case 2:
      /* Don't expand fine-grained when combining, because that will
         make the pattern fail.  */
      if (rtx_equal_function_value_matters
	  || reload_in_progress || reload_completed)
	{
	  rtx operands[3];
  
	  /* Cases 3 and 4 should be handled by this split
	     only while combining  */
	  if (kind > 2)
	    abort ();
	  if (right)
	    {
	      emit_insn (gen_lshrsi3 (dest, source, GEN_INT (right)));
	      source = dest;
	    }
	  emit_insn (gen_andsi3 (dest, source, GEN_INT (mask)));
	  if (total_shift)
	    {
	      operands[0] = dest;
	      operands[1] = dest;
	      operands[2] = GEN_INT (total_shift);
	      shift_gen_fun (ASHIFT, operands);
	    }
	  break;
	}
      else
	{
	  int neg = 0;
	  if (kind != 4 && total_shift < 16)
	    {
	      neg = -ext_shift_amounts[total_shift][1];
	      if (neg > 0)
		neg -= ext_shift_amounts[total_shift][2];
	      else
		neg = 0;
	    }
	  emit_insn (gen_and_shl_scratch (dest, source,
					  GEN_INT (right),
					  GEN_INT (mask),
					  GEN_INT (total_shift + neg),
					  GEN_INT (neg)));
	  emit_insn (gen_movsi (dest, dest));
	  break;
	}
    }
  return 0;
}

/* Try to find a good way to implement the combiner pattern
  [(set (match_operand:SI 0 "register_operand" "=r")
        (sign_extract:SI (ashift:SI (match_operand:SI 1 "register_operand" "r")
                                    (match_operand:SI 2 "const_int_operand" "n")
                         (match_operand:SI 3 "const_int_operand" "n")
                         (const_int 0)))
   (clobber (reg:SI 18))]
  LEFT_RTX is operand 2 in the above pattern, and SIZE_RTX is operand 3.
  return 0 for simple left / right shift combination.
  return 1 for left shift / 8 bit sign extend / left shift.
  return 2 for left shift / 16 bit sign extend / left shift.
  return 3 for left shift / 8 bit sign extend / shift / sign extend.
  return 4 for left shift / 16 bit sign extend / shift / sign extend.
  return 5 for left shift / 16 bit sign extend / right shift
  return 6 for < 8 bit sign extend / left shift.
  return 7 for < 8 bit sign extend / left shift / single right shift.
  If COSTP is nonzero, assign the calculated cost to *COSTP.  */

int
shl_sext_kind (left_rtx, size_rtx, costp)
     rtx left_rtx, size_rtx;
     int *costp;
{
  int left, size, insize, ext;
  int cost, best_cost;
  int kind;

  left = INTVAL (left_rtx);
  size = INTVAL (size_rtx);
  insize = size - left;
  if (insize <= 0)
    abort ();
  /* Default to left / right shift.  */
  kind = 0;
  best_cost = shift_insns[32 - insize] + ashiftrt_insns[32 - size];
  if (size <= 16)
    {
      /* 16 bit shift / sign extend / 16 bit shift */
      cost = shift_insns[16 - insize] + 1 + ashiftrt_insns[16 - size];
      /* If ashiftrt_insns[16 - size] is 8, this choice will be overridden
	 below, by alternative 3 or something even better.  */
      if (cost < best_cost)
	{
	  kind = 5;
	  best_cost = cost;
	}
    }
  /* Try a plain sign extend between two shifts.  */
  for (ext = 16; ext >= insize; ext -= 8)
    {
      if (ext <= size)
	{
	  cost = ext_shift_insns[ext - insize] + 1 + shift_insns[size - ext];
	  if (cost < best_cost)
	    {
	      kind = ext / 8U;
	      best_cost = cost;
	    }
	}
      /* Check if we can do a sloppy shift with a final signed shift
	 restoring the sign.  */
      if (EXT_SHIFT_SIGNED (size - ext))
	cost = ext_shift_insns[ext - insize] + ext_shift_insns[size - ext] + 1;
      /* If not, maybe it's still cheaper to do the second shift sloppy,
	 and do a final sign extend?  */
      else if (size <= 16)
	cost = ext_shift_insns[ext - insize] + 1
	  + ext_shift_insns[size > ext ? size - ext : ext - size] + 1;
      else
	continue;
      if (cost < best_cost)
	{
	  kind = ext / 8U + 2;
	  best_cost = cost;
	}
    }
  /* Check if we can sign extend in r0 */
  if (insize < 8)
    {
      cost = 3 + shift_insns[left];
      if (cost < best_cost)
	{
	  kind = 6;
	  best_cost = cost;
	}
      /* Try the same with a final signed shift.  */
      if (left < 31)
	{
	  cost = 3 + ext_shift_insns[left + 1] + 1;
	  if (cost < best_cost)
	    {
	      kind = 7;
	      best_cost = cost;
	    }
	}
    }
  if (TARGET_SH3)
    {
      /* Try to use a dynamic shift.  */
      cost = shift_insns[32 - insize] + 1 + SH_DYNAMIC_SHIFT_COST;
      if (cost < best_cost)
	{
	  kind = 0;
	  best_cost = cost;
	}
    }
  if (costp)
    *costp = cost;
  return kind;
}

/* Function to be used in the length attribute of the instructions
   implementing this pattern.  */

int
shl_sext_length (insn)
     rtx insn;
{
  rtx set_src, left_rtx, size_rtx;
  int cost;

  set_src = SET_SRC (XVECEXP (PATTERN (insn), 0, 0));
  left_rtx = XEXP (XEXP (set_src, 0), 1);
  size_rtx = XEXP (set_src, 1);
  shl_sext_kind (left_rtx, size_rtx, &cost);
  return cost;
}

/* Generate rtl for this pattern */

int
gen_shl_sext (dest, left_rtx, size_rtx, source)
     rtx dest, left_rtx, size_rtx, source;
{
  int kind;
  int left, size, insize, cost;
  rtx operands[3];

  kind = shl_sext_kind (left_rtx, size_rtx, &cost);
  left = INTVAL (left_rtx);
  size = INTVAL (size_rtx);
  insize = size - left;
  switch (kind)
    {
    case 1:
    case 2:
    case 3:
    case 4:
      {
	int ext = kind & 1 ? 8 : 16;
	int shift2 = size - ext;

	/* Don't expand fine-grained when combining, because that will
	   make the pattern fail.  */
	if (! rtx_equal_function_value_matters
	    && ! reload_in_progress && ! reload_completed)
	  {
	    emit_insn (gen_shl_sext_ext (dest, source, left_rtx, size_rtx));
	    emit_insn (gen_movsi (dest, source));
	    break;
	  }
	if (dest != source)
	  emit_insn (gen_movsi (dest, source));
	operands[0] = dest;
	if (ext - insize)
	  {
	    operands[2] = GEN_INT (ext - insize);
	    gen_shifty_hi_op (ASHIFT, operands);
	  }
	emit_insn (kind & 1
		   ? gen_extendqisi2(dest, gen_lowpart (QImode, dest))
		   : gen_extendhisi2(dest, gen_lowpart (HImode, dest)));
	if (kind <= 2)
	  {
	    if (shift2)
	      {
		operands[2] = GEN_INT (shift2);
		gen_shifty_op (ASHIFT, operands);
	      }
	  }
	else
	  {
	    if (shift2 > 0)
	      {
		if (EXT_SHIFT_SIGNED (shift2))
		  {
		    operands[2] = GEN_INT (shift2 + 1);
		    gen_shifty_op (ASHIFT, operands);
		    operands[2] = GEN_INT (1);
		    gen_shifty_op (ASHIFTRT, operands);
		    break;
		  }
		operands[2] = GEN_INT (shift2);
		gen_shifty_hi_op (ASHIFT, operands);
	      }
	    else if (shift2)
	      {
		operands[2] = GEN_INT (-shift2);
		gen_shifty_hi_op (LSHIFTRT, operands);
	      }
	    emit_insn (size <= 8
		       ? gen_extendqisi2 (dest, gen_lowpart (QImode, dest))
		       : gen_extendhisi2 (dest, gen_lowpart (HImode, dest)));
	  }
	break;
      }
    case 5:
      {
	int i = 16 - size;
	if (! rtx_equal_function_value_matters
	    && ! reload_in_progress && ! reload_completed)
	  emit_insn (gen_shl_sext_ext (dest, source, left_rtx, size_rtx));
	else
	  {
	    operands[0] = dest;
	    operands[2] = GEN_INT (16 - insize);
	    gen_shifty_hi_op (ASHIFT, operands);
	    emit_insn (gen_extendhisi2 (dest, gen_lowpart (HImode, dest)));
	  }
	/* Don't use gen_ashrsi3 because it generates new pseudos.  */
	while (--i >= 0)
	  gen_ashift (ASHIFTRT, 1, dest);
	break;
      }
    case 6:
    case 7:
      /* Don't expand fine-grained when combining, because that will
	 make the pattern fail.  */
      if (! rtx_equal_function_value_matters
	  && ! reload_in_progress && ! reload_completed)
	{
	  emit_insn (gen_shl_sext_ext (dest, source, left_rtx, size_rtx));
	  emit_insn (gen_movsi (dest, source));
	  break;
	}
      emit_insn (gen_andsi3 (dest, source, GEN_INT ((1 << insize) - 1)));
      emit_insn (gen_xorsi3 (dest, dest, GEN_INT (1 << (insize - 1))));
      emit_insn (gen_addsi3 (dest, dest, GEN_INT (-1 << (insize - 1))));
      operands[0] = dest;
      operands[2] = kind == 7 ? GEN_INT (left + 1) : left_rtx;
      gen_shifty_op (ASHIFT, operands);
      if (kind == 7)
	emit_insn (gen_ashrsi3_k (dest, dest, GEN_INT (1)));
      break;
    default:
      return -1;
    }
  return 0;
}

/* The SH cannot load a large constant into a register, constants have to
   come from a pc relative load.  The reference of a pc relative load
   instruction must be less than 1k infront of the instruction.  This
   means that we often have to dump a constant inside a function, and
   generate code to branch around it.

   It is important to minimize this, since the branches will slow things
   down and make things bigger.

   Worst case code looks like:

   mov.l L1,rn
   bra   L2
   nop
   align
   L1:   .long value
   L2:
   ..

   mov.l L3,rn
   bra   L4
   nop
   align
   L3:   .long value
   L4:
   ..

   We fix this by performing a scan before scheduling, which notices which
   instructions need to have their operands fetched from the constant table
   and builds the table.

   The algorithm is:

   scan, find an instruction which needs a pcrel move.  Look forward, find the
   last barrier which is within MAX_COUNT bytes of the requirement.
   If there isn't one, make one.  Process all the instructions between
   the find and the barrier.

   In the above example, we can tell that L3 is within 1k of L1, so
   the first move can be shrunk from the 3 insn+constant sequence into
   just 1 insn, and the constant moved to L3 to make:

   mov.l        L1,rn
   ..
   mov.l        L3,rn
   bra          L4
   nop
   align
   L3:.long value
   L4:.long value

   Then the second move becomes the target for the shortening process.  */

typedef struct
{
  rtx value;			/* Value in table.  */
  rtx label;			/* Label of value.  */
  enum machine_mode mode;	/* Mode of value.  */
} pool_node;

/* The maximum number of constants that can fit into one pool, since
   the pc relative range is 0...1020 bytes and constants are at least 4
   bytes long.  */

#define MAX_POOL_SIZE (1020/4)
static pool_node pool_vector[MAX_POOL_SIZE];
static int pool_size;

/* ??? If we need a constant in HImode which is the truncated value of a
   constant we need in SImode, we could combine the two entries thus saving
   two bytes.  Is this common enough to be worth the effort of implementing
   it?  */

/* ??? This stuff should be done at the same time that we shorten branches.
   As it is now, we must assume that all branches are the maximum size, and
   this causes us to almost always output constant pools sooner than
   necessary.  */

/* Add a constant to the pool and return its label.  */

static rtx
add_constant (x, mode, last_value)
     rtx last_value;
     rtx x;
     enum machine_mode mode;
{
  int i;
  rtx lab;

  /* First see if we've already got it.  */
  for (i = 0; i < pool_size; i++)
    {
      if (x->code == pool_vector[i].value->code
	  && mode == pool_vector[i].mode)
	{
	  if (x->code == CODE_LABEL)
	    {
	      if (XINT (x, 3) != XINT (pool_vector[i].value, 3))
		continue;
	    }
	  if (rtx_equal_p (x, pool_vector[i].value))
	    {
	      lab = 0;
	      if (! last_value
		  || ! i
		  || ! rtx_equal_p (last_value, pool_vector[i-1].value))
		{
		  lab = pool_vector[i].label;
		  if (! lab)
		    pool_vector[i].label = lab = gen_label_rtx ();
		}
	      return lab;
	    }
	}
    }

  /* Need a new one.  */
  pool_vector[pool_size].value = x;
  if (last_value && rtx_equal_p (last_value, pool_vector[pool_size - 1].value))
    lab = 0;
  else
    lab = gen_label_rtx ();
  pool_vector[pool_size].mode = mode;
  pool_vector[pool_size].label = lab;
  pool_size++;
  return lab;
}

/* Output the literal table.  */

static void
dump_table (scan)
     rtx scan;
{
  int i;
  int need_align = 1;

  /* Do two passes, first time dump out the HI sized constants.  */

  for (i = 0; i < pool_size; i++)
    {
      pool_node *p = &pool_vector[i];

      if (p->mode == HImode)
	{
	  if (need_align)
	    {
	      scan = emit_insn_after (gen_align_2 (), scan);
	      need_align = 0;
	    }
	  scan = emit_label_after (p->label, scan);
	  scan = emit_insn_after (gen_consttable_2 (p->value), scan);
	}
    }

  need_align = 1;

  for (i = 0; i < pool_size; i++)
    {
      pool_node *p = &pool_vector[i];

      switch (p->mode)
	{
	case HImode:
	  break;
	case SImode:
	case SFmode:
	  if (need_align)
	    {
	      need_align = 0;
	      scan = emit_label_after (gen_label_rtx (), scan);
	      scan = emit_insn_after (gen_align_4 (), scan);
	    }
	  if (p->label)
	    scan = emit_label_after (p->label, scan);
	  scan = emit_insn_after (gen_consttable_4 (p->value), scan);
	  break;
	case DFmode:
	case DImode:
	  if (need_align)
	    {
	      need_align = 0;
	      scan = emit_label_after (gen_label_rtx (), scan);
	      scan = emit_insn_after (gen_align_4 (), scan);
	    }
	  if (p->label)
	    scan = emit_label_after (p->label, scan);
	  scan = emit_insn_after (gen_consttable_8 (p->value), scan);
	  break;
	default:
	  abort ();
	  break;
	}
    }

  scan = emit_insn_after (gen_consttable_end (), scan);
  scan = emit_barrier_after (scan);
  pool_size = 0;
}

/* Return non-zero if constant would be an ok source for a
   mov.w instead of a mov.l.  */

static int
hi_const (src)
     rtx src;
{
  return (GET_CODE (src) == CONST_INT
	  && INTVAL (src) >= -32768
	  && INTVAL (src) <= 32767);
}

/* Non-zero if the insn is a move instruction which needs to be fixed.  */

/* ??? For a DImode/DFmode moves, we don't need to fix it if each half of the
   CONST_DOUBLE input value is CONST_OK_FOR_I.  For a SFmode move, we don't
   need to fix it if the input value is CONST_OK_FOR_I.  */

static int
broken_move (insn)
     rtx insn;
{
  if (GET_CODE (insn) == INSN)
    {
      rtx pat = PATTERN (insn);
      if (GET_CODE (pat) == PARALLEL)
	pat = XVECEXP (pat, 0, 0);
      if (GET_CODE (pat) == SET
	  /* We can load any 8 bit value if we don't care what the high
	     order bits end up as.  */
	  && GET_MODE (SET_DEST (pat)) != QImode
	  && CONSTANT_P (SET_SRC (pat))
	  && ! (TARGET_SH3E
		&& GET_CODE (SET_SRC (pat)) == CONST_DOUBLE
		&& (fp_zero_operand (SET_SRC (pat))
		    || fp_one_operand (SET_SRC (pat)))
		&& GET_CODE (SET_DEST (pat)) == REG
		&& REGNO (SET_DEST (pat)) >= FIRST_FP_REG
		&& REGNO (SET_DEST (pat)) <= LAST_FP_REG)
	  && (GET_CODE (SET_SRC (pat)) != CONST_INT
	      || ! CONST_OK_FOR_I (INTVAL (SET_SRC (pat)))))
	return 1;
    }

  return 0;
}

static int
mova_p (insn)
     rtx insn;
{
  return (GET_CODE (insn) == INSN
	  && GET_CODE (PATTERN (insn)) == SET
	  && GET_CODE (SET_SRC (PATTERN (insn))) == UNSPEC
	  && XINT (SET_SRC (PATTERN (insn)), 1) == 1);
}

/* Find the last barrier from insn FROM which is close enough to hold the
   constant pool.  If we can't find one, then create one near the end of
   the range.  */

static rtx
find_barrier (num_mova, mova, from)
     int num_mova;
     rtx mova, from;
{
  int count_si = 0;
  int count_hi = 0;
  int found_hi = 0;
  int found_si = 0;
  int hi_align = 2;
  int si_align = 2;
  int leading_mova = num_mova;
  rtx barrier_before_mova, found_barrier = 0, good_barrier = 0;
  int si_limit;
  int hi_limit;

  /* For HImode: range is 510, add 4 because pc counts from address of
     second instruction after this one, subtract 2 for the jump instruction
     that we may need to emit before the table, subtract 2 for the instruction
     that fills the jump delay slot (in very rare cases, reorg will take an
     instruction from after the constant pool or will leave the delay slot
     empty).  This gives 510.
     For SImode: range is 1020, add 4 because pc counts from address of
     second instruction after this one, subtract 2 in case pc is 2 byte
     aligned, subtract 2 for the jump instruction that we may need to emit
     before the table, subtract 2 for the instruction that fills the jump
     delay slot.  This gives 1018.  */

  /* The branch will always be shortened now that the reference address for
     forward branches is the successor address, thus we need no longer make
     adjustments to the [sh]i_limit for -O0.  */

  si_limit = 1018;
  hi_limit = 510;

  while (from && count_si < si_limit && count_hi < hi_limit)
    {
      int inc = get_attr_length (from);
      int new_align = 1;

      if (GET_CODE (from) == CODE_LABEL)
	{
	  if (optimize)
	    new_align = 1 << label_to_alignment (from);
	  else if (GET_CODE (prev_nonnote_insn (from)) == BARRIER)
	    new_align = 1 << barrier_align (from);
	  else
	    new_align = 1;
	  inc = 0;
	}

      if (GET_CODE (from) == BARRIER)
	{

	  found_barrier = from;

	  /* If we are at the end of the function, or in front of an alignment
	     instruction, we need not insert an extra alignment.  We prefer
	     this kind of barrier.  */
	  if (barrier_align (from) > 2)
	    good_barrier = from;
	}

      if (broken_move (from))
	{
	  rtx pat, src, dst;
	  enum machine_mode mode;

	  pat = PATTERN (from);
	  if (GET_CODE (pat) == PARALLEL)
	    pat = XVECEXP (pat, 0, 0);
	  src = SET_SRC (pat);
	  dst = SET_DEST (pat);
	  mode = GET_MODE (dst);

	  /* We must explicitly check the mode, because sometimes the
	     front end will generate code to load unsigned constants into
	     HImode targets without properly sign extending them.  */
	  if (mode == HImode
	      || (mode == SImode && hi_const (src) && REGNO (dst) != FPUL_REG))
	    {
	      found_hi += 2;
	      /* We put the short constants before the long constants, so
		 we must count the length of short constants in the range
		 for the long constants.  */
	      /* ??? This isn't optimal, but is easy to do.  */
	      si_limit -= 2;
	    }
	  else
	    {
	      while (si_align > 2 && found_si + si_align - 2 > count_si)
		si_align >>= 1;
	      if (found_si > count_si)
		count_si = found_si;
	      found_si += GET_MODE_SIZE (mode);
	      if (num_mova)
		si_limit -= GET_MODE_SIZE (mode);
	    }
	}

      if (mova_p (from))
	{
	  if (! num_mova++)
	    {
	      leading_mova = 0;
	      mova = from;
	      barrier_before_mova = good_barrier ? good_barrier : found_barrier;
	    }
	  if (found_si > count_si)
	    count_si = found_si;
	}
      else if (GET_CODE (from) == JUMP_INSN
	       && (GET_CODE (PATTERN (from)) == ADDR_VEC
		   || GET_CODE (PATTERN (from)) == ADDR_DIFF_VEC))
	{
	  if (num_mova)
	    num_mova--;
	  if (barrier_align (next_real_insn (from)) == CACHE_LOG)
	    {
	      /* We have just passed the barrier in front of the
		 ADDR_DIFF_VEC, which is stored in found_barrier.  Since
		 the ADDR_DIFF_VEC is accessed as data, just like our pool
		 constants, this is a good opportunity to accommodate what
		 we have gathered so far.
		 If we waited any longer, we could end up at a barrier in
		 front of code, which gives worse cache usage for separated
		 instruction / data caches.  */
	      good_barrier = found_barrier;
	      break;
	    }
	  else
	    {
	      rtx body = PATTERN (from);
	      inc = XVECLEN (body, 1) * GET_MODE_SIZE (GET_MODE (body));
	    }
	}

      if (found_si)
	{
	  if (new_align > si_align)
	    {
	      si_limit -= count_si - 1 & new_align - si_align;
	      si_align = new_align;
	    }
	  count_si = count_si + new_align - 1 & -new_align;
	  count_si += inc;
	}
      if (found_hi)
	{
	  if (new_align > hi_align)
	    {
	      hi_limit -= count_hi - 1 & new_align - hi_align;
	      hi_align = new_align;
	    }
	  count_hi = count_hi + new_align - 1 & -new_align;
	  count_hi += inc;
	}
      from = NEXT_INSN (from);
    }

  if (num_mova)
    if (leading_mova)
      {
	/* Try as we might, the leading mova is out of range.  Change
	   it into a load (which will become a pcload) and retry.  */
	SET_SRC (PATTERN (mova)) = XVECEXP (SET_SRC (PATTERN (mova)), 0, 0);
	INSN_CODE (mova) = -1;
        return find_barrier (0, 0, mova);
      }
    else
      {
	/* Insert the constant pool table before the mova instruction,
	   to prevent the mova label reference from going out of range.  */
	from = mova;
	good_barrier = found_barrier = barrier_before_mova;
      }

  if (found_barrier)
    {
      if (good_barrier && next_real_insn (found_barrier))
	found_barrier = good_barrier;
    }
  else
    {
      /* We didn't find a barrier in time to dump our stuff,
	 so we'll make one.  */
      rtx label = gen_label_rtx ();

      /* If we exceeded the range, then we must back up over the last
	 instruction we looked at.  Otherwise, we just need to undo the
	 NEXT_INSN at the end of the loop.  */
      if (count_hi > hi_limit || count_si > si_limit)
	from = PREV_INSN (PREV_INSN (from));
      else
	from = PREV_INSN (from);

      /* Walk back to be just before any jump or label.
	 Putting it before a label reduces the number of times the branch
	 around the constant pool table will be hit.  Putting it before
	 a jump makes it more likely that the bra delay slot will be
	 filled.  */
      while (GET_CODE (from) == JUMP_INSN || GET_CODE (from) == NOTE
	     || GET_CODE (from) == CODE_LABEL)
	from = PREV_INSN (from);

      from = emit_jump_insn_after (gen_jump (label), from);
      JUMP_LABEL (from) = label;
      LABEL_NUSES (label) = 1;
      found_barrier = emit_barrier_after (from);
      emit_label_after (label, found_barrier);
    }

  return found_barrier;
}

/* If the instruction INSN is implemented by a special function, and we can
   positively find the register that is used to call the sfunc, and this
   register is not used anywhere else in this instruction - except as the
   destination of a set, return this register; else, return 0.  */
rtx
sfunc_uses_reg (insn)
     rtx insn;
{
  int i;
  rtx pattern, part, reg_part, reg;

  if (GET_CODE (insn) != INSN)
    return 0;
  pattern = PATTERN (insn);
  if (GET_CODE (pattern) != PARALLEL || get_attr_type (insn) != TYPE_SFUNC)
    return 0;

  for (reg_part = 0, i = XVECLEN (pattern, 0) - 1; i >= 1; i--)
    {
      part = XVECEXP (pattern, 0, i);
      if (GET_CODE (part) == USE && GET_MODE (XEXP (part, 0)) == SImode)
	reg_part = part;
    }
  if (! reg_part)
    return 0;
  reg = XEXP (reg_part, 0);
  for (i = XVECLEN (pattern, 0) - 1; i >= 0; i--)
    {
      part = XVECEXP (pattern, 0, i);
      if (part == reg_part || GET_CODE (part) == CLOBBER)
	continue;
      if (reg_mentioned_p (reg, ((GET_CODE (part) == SET
				  && GET_CODE (SET_DEST (part)) == REG)
				 ? SET_SRC (part) : part)))
	return 0;
    }
  return reg;
}

/* See if the only way in which INSN uses REG is by calling it, or by
   setting it while calling it.  Set *SET to a SET rtx if the register
   is set by INSN.  */

static int
noncall_uses_reg (reg, insn, set)
     rtx reg;
     rtx insn;
     rtx *set;
{
  rtx pattern, reg2;

  *set = NULL_RTX;

  reg2 = sfunc_uses_reg (insn);
  if (reg2 && REGNO (reg2) == REGNO (reg))
    {
      pattern = single_set (insn);
      if (pattern
	  && GET_CODE (SET_DEST (pattern)) == REG
	  && REGNO (reg) == REGNO (SET_DEST (pattern)))
	*set = pattern;
      return 0;
    }
  if (GET_CODE (insn) != CALL_INSN)
    {
      /* We don't use rtx_equal_p because we don't care if the mode is
	 different.  */
      pattern = single_set (insn);
      if (pattern
	  && GET_CODE (SET_DEST (pattern)) == REG
	  && REGNO (reg) == REGNO (SET_DEST (pattern)))
	{
	  rtx par, part;
	  int i;

	  *set = pattern;
	  par = PATTERN (insn);
	  if (GET_CODE (par) == PARALLEL)
	    for (i = XVECLEN (par, 0) - 1; i >= 0; i--)
	      {
		part = XVECEXP (par, 0, i);
		if (GET_CODE (part) != SET && reg_mentioned_p (reg, part))
		  return 1;
	      }
	  return reg_mentioned_p (reg, SET_SRC (pattern));
	}

      return 1;
    }

  pattern = PATTERN (insn);

  if (GET_CODE (pattern) == PARALLEL)
    {
      int i;

      for (i = XVECLEN (pattern, 0) - 1; i >= 1; i--)
	if (reg_mentioned_p (reg, XVECEXP (pattern, 0, i)))
	  return 1;
      pattern = XVECEXP (pattern, 0, 0);
    }

  if (GET_CODE (pattern) == SET)
    {
      if (reg_mentioned_p (reg, SET_DEST (pattern)))
	{
	  /* We don't use rtx_equal_p, because we don't care if the
             mode is different.  */
	  if (GET_CODE (SET_DEST (pattern)) != REG
	      || REGNO (reg) != REGNO (SET_DEST (pattern)))
	    return 1;

	  *set = pattern;
	}

      pattern = SET_SRC (pattern);
    }

  if (GET_CODE (pattern) != CALL
      || GET_CODE (XEXP (pattern, 0)) != MEM
      || ! rtx_equal_p (reg, XEXP (XEXP (pattern, 0), 0)))
    return 1;

  return 0;
}

/* Given a X, a pattern of an insn or a part of it, return a mask of used
   general registers.  Bits 0..15 mean that the respective registers
   are used as inputs in the instruction.  Bits 16..31 mean that the
   registers 0..15, respectively, are used as outputs, or are clobbered.
   IS_DEST should be set to 16 if X is the destination of a SET, else to 0.  */
int
regs_used (x, is_dest)
     rtx x; int is_dest;
{
  enum rtx_code code;
  char *fmt;
  int i, used = 0;

  if (! x)
    return used;
  code = GET_CODE (x);
  switch (code)
    {
    case REG:
      if (REGNO (x) < 16)
	return (((1 << HARD_REGNO_NREGS (0, GET_MODE (x))) - 1)
		<< (REGNO (x) + is_dest));
      return 0;
    case SUBREG:
      {
	rtx y = SUBREG_REG (x);
     
	if (GET_CODE (y) != REG)
	  break;
	if (REGNO (y) < 16)
	  return (((1 << HARD_REGNO_NREGS (0, GET_MODE (x))) - 1)
		  << (REGNO (y) + SUBREG_WORD (x) + is_dest));
	return 0;
      }
    case SET:
      return regs_used (SET_SRC (x), 0) | regs_used (SET_DEST (x), 16);
    case RETURN:
      /* If there was a return value, it must have been indicated with USE.  */
      return 0x00ffff00;
    case CLOBBER:
      is_dest = 1;
      break;
    case MEM:
      is_dest = 0;
      break;
    case CALL:
      used |= 0x00ff00f0;
      break;
    }

  fmt = GET_RTX_FORMAT (code);

  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    used |= regs_used (XVECEXP (x, i, j), is_dest);
	}
      else if (fmt[i] == 'e')
	used |= regs_used (XEXP (x, i), is_dest);
    }
  return used;
}

/* Create an instruction that prevents redirection of a conditional branch
   to the destination of the JUMP with address ADDR.
   If the branch needs to be implemented as an indirect jump, try to find
   a scratch register for it.
   If NEED_BLOCK is 0, don't do anything unless we need a scratch register.
   If any preceding insn that doesn't fit into a delay slot is good enough,
   pass 1.  Pass 2 if a definite blocking insn is needed.
   -1 is used internally to avoid deep recursion.
   If a blocking instruction is made or recognized, return it.  */
   
static rtx
gen_block_redirect (jump, addr, need_block)
     rtx jump;
     int addr, need_block;
{
  int dead = 0;
  rtx prev = prev_nonnote_insn (jump);
  rtx dest;

  /* First, check if we already have an instruction that satisfies our need.  */
  if (prev && GET_CODE (prev) == INSN && ! INSN_DELETED_P (prev))
    {
      if (INSN_CODE (prev) == CODE_FOR_indirect_jump_scratch)
	return prev;
      if (GET_CODE (PATTERN (prev)) == USE
	  || GET_CODE (PATTERN (prev)) == CLOBBER
	  || get_attr_in_delay_slot (prev) == IN_DELAY_SLOT_YES)
	prev = jump;
      else if ((need_block &= ~1) < 0)
	return prev;
      else if (recog_memoized (prev) == CODE_FOR_block_branch_redirect)
	need_block = 0;
    }
  /* We can't use JUMP_LABEL here because it might be undefined
     when not optimizing.  */
  dest = XEXP (SET_SRC (PATTERN (jump)), 0);
  /* If the branch is out of range, try to find a scratch register for it.  */
  if (optimize
      && (insn_addresses[INSN_UID (dest)] - addr + 4092U > 4092 + 4098))
    {
      rtx scan;
      /* Don't look for the stack pointer as a scratch register,
	 it would cause trouble if an interrupt occurred.  */
      unsigned try = 0x7fff, used;
      int jump_left = flag_expensive_optimizations + 1;
    
      /* It is likely that the most recent eligible instruction is wanted for
	 the delay slot.  Therefore, find out which registers it uses, and
	 try to avoid using them.  */
	 
      for (scan = jump; scan = PREV_INSN (scan); )
	{
	  enum rtx_code code;

	  if (INSN_DELETED_P (scan))
	    continue;
	  code = GET_CODE (scan);
	  if (code == CODE_LABEL || code == JUMP_INSN)
	    break;
	  if (code == INSN
	      && GET_CODE (PATTERN (scan)) != USE
	      && GET_CODE (PATTERN (scan)) != CLOBBER
	      && get_attr_in_delay_slot (scan) == IN_DELAY_SLOT_YES)
	    {
	      try &= ~regs_used (PATTERN (scan), 0);
	      break;
	    }
	}
      for (used = dead = 0, scan = JUMP_LABEL (jump); scan = NEXT_INSN (scan); )
	{
	  enum rtx_code code;

	  if (INSN_DELETED_P (scan))
	    continue;
	  code = GET_CODE (scan);
	  if (GET_RTX_CLASS (code) == 'i')
	    {
	      used |= regs_used (PATTERN (scan), 0);
	      if (code == CALL_INSN)
		used |= regs_used (CALL_INSN_FUNCTION_USAGE (scan), 0);
	      dead |= (used >> 16) & ~used;
	      if (dead & try)
		{
		  dead &= try;
		  break;
		}
	      if (code == JUMP_INSN)
		if (jump_left-- && simplejump_p (scan))
		  scan = JUMP_LABEL (scan);
		else
		  break;
	    }
	}
      /* Mask out the stack pointer again, in case it was
	 the only 'free' register we have found.  */
      dead &= 0x7fff;
    }
  /* If the immediate destination is still in range, check for possible
     threading with a jump beyond the delay slot insn.
     Don't check if we are called recursively; the jump has been or will be
     checked in a different invocation then.  */
	
  else if (optimize && need_block >= 0)
    {
      rtx next = next_active_insn (next_active_insn (dest));
      if (next && GET_CODE (next) == JUMP_INSN
	  && GET_CODE (PATTERN (next)) == SET
	  && recog_memoized (next) == CODE_FOR_jump)
	{
	  dest = JUMP_LABEL (next);
	  if (dest
	      && insn_addresses[INSN_UID (dest)] - addr + 4092U > 4092 + 4098)
	    gen_block_redirect (next, insn_addresses[INSN_UID (next)], -1);
	}
    }

  if (dead)
    {
      rtx reg = gen_rtx (REG, SImode, exact_log2 (dead & -dead));

      /* It would be nice if we could convert the jump into an indirect
	 jump / far branch right now, and thus exposing all constituent
	 instructions to further optimization.  However, reorg uses
	 simplejump_p to determine if there is an unconditional jump where
	 it should try to schedule instructions from the target of the
	 branch; simplejump_p fails for indirect jumps even if they have
	 a JUMP_LABEL.  */
      rtx insn = emit_insn_before (gen_indirect_jump_scratch
				   (reg, GEN_INT (INSN_UID (JUMP_LABEL (jump))))
				   , jump);
      INSN_CODE (insn) = CODE_FOR_indirect_jump_scratch;
      return insn;
    }
  else if (need_block)
    /* We can't use JUMP_LABEL here because it might be undefined
       when not optimizing.  */
    return emit_insn_before (gen_block_branch_redirect
		      (GEN_INT (INSN_UID (XEXP (SET_SRC (PATTERN (jump)), 0))))
		      , jump);
  return prev;
}

#define CONDJUMP_MIN -252
#define CONDJUMP_MAX 262
struct far_branch
{
  /* A label (to be placed) in front of the jump
     that jumps to our ultimate destination.  */
  rtx near_label;
  /* Where we are going to insert it if we cannot move the jump any farther,
     or the jump itself if we have picked up an existing jump.  */
  rtx insert_place;
  /* The ultimate destination.  */
  rtx far_label;
  struct far_branch *prev;
  /* If the branch has already been created, its address;
     else the address of its first prospective user.  */
  int address;
};

enum mdep_reorg_phase_e mdep_reorg_phase;
void
gen_far_branch (bp)
     struct far_branch *bp;
{
  rtx insn = bp->insert_place;
  rtx jump;
  rtx label = gen_label_rtx ();

  emit_label_after (label, insn);
  if (bp->far_label)
    {
      jump = emit_jump_insn_after (gen_jump (bp->far_label), insn);
      LABEL_NUSES (bp->far_label)++;
    }
  else
    jump = emit_jump_insn_after (gen_return (), insn);
  /* Emit a barrier so that reorg knows that any following instructions
     are not reachable via a fall-through path.
     But don't do this when not optimizing, since we wouldn't supress the
     alignment for the barrier then, and could end up with out-of-range
     pc-relative loads.  */
  if (optimize)
    emit_barrier_after (jump);
  emit_label_after (bp->near_label, insn);
  JUMP_LABEL (jump) = bp->far_label;
  if (! invert_jump (insn, label))
    abort ();
  /* Prevent reorg from undoing our splits.  */
  gen_block_redirect (jump, bp->address += 2, 2);
}

/* Fix up ADDR_DIFF_VECs.  */
void
fixup_addr_diff_vecs (first)
     rtx first;
{
  rtx insn;

  for (insn = first; insn; insn = NEXT_INSN (insn))
    {
      rtx vec_lab, pat, prev, prevpat, x, braf_label;

      if (GET_CODE (insn) != JUMP_INSN
	  || GET_CODE (PATTERN (insn)) != ADDR_DIFF_VEC)
	continue;
      pat = PATTERN (insn);
      vec_lab = XEXP (XEXP (pat, 0), 0);

      /* Search the matching casesi_jump_2.  */
      for (prev = vec_lab; ; prev = PREV_INSN (prev))
	{
	  if (GET_CODE (prev) != JUMP_INSN)
	    continue;
	  prevpat = PATTERN (prev);
	  if (GET_CODE (prevpat) != PARALLEL || XVECLEN (prevpat, 0) != 2)
	    continue;
	  x = XVECEXP (prevpat, 0, 1);
	  if (GET_CODE (x) != USE)
	    continue;
	  x = XEXP (x, 0);
	  if (GET_CODE (x) == LABEL_REF && XEXP (x, 0) == vec_lab)
	    break;
	}

      /* Emit the reference label of the braf where it belongs, right after
	 the casesi_jump_2 (i.e. braf).  */
      braf_label = XEXP (XEXP (SET_SRC (XVECEXP (prevpat, 0, 0)), 1), 0);
      emit_label_after (braf_label, prev);

      /* Fix up the ADDR_DIF_VEC to be relative
	 to the reference address of the braf.  */
      XEXP (XEXP (pat, 0), 0) = braf_label;
    }
}

/* BARRIER_OR_LABEL is either a BARRIER or a CODE_LABEL immediately following
   a barrier.  Return the base 2 logarithm of the desired alignment.  */
int
barrier_align (barrier_or_label)
     rtx barrier_or_label;
{
  rtx next = next_real_insn (barrier_or_label), pat, prev;
  int slot, credit;
 
  if (! next)
    return 0;

  pat = PATTERN (next);

  if (GET_CODE (pat) == ADDR_DIFF_VEC)
    return 2;

  if (GET_CODE (pat) == UNSPEC_VOLATILE && XINT (pat, 1) == 1)
    /* This is a barrier in front of a constant table.  */
    return 0;

  prev = prev_real_insn (barrier_or_label);
  if (GET_CODE (PATTERN (prev)) == ADDR_DIFF_VEC)
    {
      pat = PATTERN (prev);
      /* If this is a very small table, we want to keep the alignment after
	 the table to the minimum for proper code alignment.  */
      return ((TARGET_SMALLCODE
	       || (XVECLEN (pat, 1) * GET_MODE_SIZE (GET_MODE (pat))
		   <= 1 << (CACHE_LOG - 2)))
	      ? 1 : CACHE_LOG);
    }

  if (TARGET_SMALLCODE)
    return 0;

  if (! TARGET_SH3 || ! optimize)
    return CACHE_LOG;

  /* When fixing up pcloads, a constant table might be inserted just before
     the basic block that ends with the barrier.  Thus, we can't trust the
     instruction lengths before that.  */
  if (mdep_reorg_phase > SH_FIXUP_PCLOAD)
    {
      /* Check if there is an immediately preceding branch to the insn beyond
	 the barrier.  We must weight the cost of discarding useful information
	 from the current cache line when executing this branch and there is
	 an alignment, against that of fetching unneeded insn in front of the
	 branch target when there is no alignment.  */

      /* PREV is presumed to be the JUMP_INSN for the barrier under
	 investigation.  Skip to the insn before it.  */
      prev = prev_real_insn (prev);

      for (slot = 2, credit = 1 << (CACHE_LOG - 2) + 2;
	   credit >= 0 && prev && GET_CODE (prev) == INSN;
	   prev = prev_real_insn (prev))
	{
	  if (GET_CODE (PATTERN (prev)) == USE
	      || GET_CODE (PATTERN (prev)) == CLOBBER)
	    continue;
	  if (GET_CODE (PATTERN (prev)) == SEQUENCE)
	    prev = XVECEXP (PATTERN (prev), 0, 1);
	  if (slot &&
	      get_attr_in_delay_slot (prev) == IN_DELAY_SLOT_YES)
	    slot = 0;
	  credit -= get_attr_length (prev);
	}
      if (prev
	  && GET_CODE (prev) == JUMP_INSN
	  && JUMP_LABEL (prev)
	  && next_real_insn (JUMP_LABEL (prev)) == next_real_insn (barrier_or_label)
	  && (credit - slot >= (GET_CODE (SET_SRC (PATTERN (prev))) == PC ? 2 : 0)))
	return 0;
    }

  return CACHE_LOG;
}

/* Exported to toplev.c.

   Do a final pass over the function, just before delayed branch
   scheduling.  */

void
machine_dependent_reorg (first)
     rtx first;
{
  rtx insn, mova;
  int num_mova;
  rtx r0_rtx = gen_rtx (REG, Pmode, 0);
  rtx r0_inc_rtx = gen_rtx (POST_INC, Pmode, r0_rtx);

  /* If relaxing, generate pseudo-ops to associate function calls with
     the symbols they call.  It does no harm to not generate these
     pseudo-ops.  However, when we can generate them, it enables to
     linker to potentially relax the jsr to a bsr, and eliminate the
     register load and, possibly, the constant pool entry.  */

  mdep_reorg_phase = SH_INSERT_USES_LABELS;
  if (TARGET_RELAX)
    {
      /* Remove all REG_LABEL notes.  We want to use them for our own
	 purposes.  This works because none of the remaining passes
	 need to look at them.

	 ??? But it may break in the future.  We should use a machine
	 dependent REG_NOTE, or some other approach entirely.  */
      for (insn = first; insn; insn = NEXT_INSN (insn))
	{
	  if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
	    {
	      rtx note;

	      while ((note = find_reg_note (insn, REG_LABEL, NULL_RTX)) != 0)
		remove_note (insn, note);
	    }
	}

      for (insn = first; insn; insn = NEXT_INSN (insn))
	{
	  rtx pattern, reg, link, set, scan, dies, label;
	  int rescan = 0, foundinsn = 0;

	  if (GET_CODE (insn) == CALL_INSN)
	    {
	      pattern = PATTERN (insn);

	      if (GET_CODE (pattern) == PARALLEL)
		pattern = XVECEXP (pattern, 0, 0);
	      if (GET_CODE (pattern) == SET)
		pattern = SET_SRC (pattern);

	      if (GET_CODE (pattern) != CALL
		  || GET_CODE (XEXP (pattern, 0)) != MEM)
		continue;

	      reg = XEXP (XEXP (pattern, 0), 0);
	    }
	  else
	    {
	      reg = sfunc_uses_reg (insn);
	      if (! reg)
		continue;
	    }

	  if (GET_CODE (reg) != REG)
	    continue;

	  /* This is a function call via REG.  If the only uses of REG
	     between the time that it is set and the time that it dies
	     are in function calls, then we can associate all the
	     function calls with the setting of REG.  */

	  for (link = LOG_LINKS (insn); link; link = XEXP (link, 1))
	    {
	      if (REG_NOTE_KIND (link) != 0)
		continue;
	      set = single_set (XEXP (link, 0));
	      if (set && rtx_equal_p (reg, SET_DEST (set)))
		{
		  link = XEXP (link, 0);
		  break;
		}
	    }

	  if (! link)
	    {
	      /* ??? Sometimes global register allocation will have
                 deleted the insn pointed to by LOG_LINKS.  Try
                 scanning backward to find where the register is set.  */
	      for (scan = PREV_INSN (insn);
		   scan && GET_CODE (scan) != CODE_LABEL;
		   scan = PREV_INSN (scan))
		{
		  if (GET_RTX_CLASS (GET_CODE (scan)) != 'i')
		    continue;

		  if (! reg_mentioned_p (reg, scan))
		    continue;

		  if (noncall_uses_reg (reg, scan, &set))
		    break;

		  if (set)
		    {
		      link = scan;
		      break;
		    }
		}
	    }

	  if (! link)
	    continue;

	  /* The register is set at LINK.  */

	  /* We can only optimize the function call if the register is
             being set to a symbol.  In theory, we could sometimes
             optimize calls to a constant location, but the assembler
             and linker do not support that at present.  */
	  if (GET_CODE (SET_SRC (set)) != SYMBOL_REF
	      && GET_CODE (SET_SRC (set)) != LABEL_REF)
	    continue;

	  /* Scan forward from LINK to the place where REG dies, and
             make sure that the only insns which use REG are
             themselves function calls.  */

	  /* ??? This doesn't work for call targets that were allocated
	     by reload, since there may not be a REG_DEAD note for the
	     register.  */

	  dies = NULL_RTX;
	  for (scan = NEXT_INSN (link); scan; scan = NEXT_INSN (scan))
	    {
	      rtx scanset;

	      /* Don't try to trace forward past a CODE_LABEL if we haven't
		 seen INSN yet.  Ordinarily, we will only find the setting insn
		 in LOG_LINKS if it is in the same basic block.  However,
		 cross-jumping can insert code labels in between the load and
		 the call, and can result in situations where a single call
		 insn may have two targets depending on where we came from.  */

	      if (GET_CODE (scan) == CODE_LABEL && ! foundinsn)
		break;

	      if (GET_RTX_CLASS (GET_CODE (scan)) != 'i')
		continue;

	      /* Don't try to trace forward past a JUMP.  To optimize
                 safely, we would have to check that all the
                 instructions at the jump destination did not use REG.  */

	      if (GET_CODE (scan) == JUMP_INSN)
		break;

	      if (! reg_mentioned_p (reg, scan))
		continue;

	      if (noncall_uses_reg (reg, scan, &scanset))
		break;

	      if (scan == insn)
		foundinsn = 1;

	      if (scan != insn
		  && (GET_CODE (scan) == CALL_INSN || sfunc_uses_reg (scan)))
		{
		  /* There is a function call to this register other
                     than the one we are checking.  If we optimize
                     this call, we need to rescan again below.  */
		  rescan = 1;
		}

	      /* ??? We shouldn't have to worry about SCANSET here.
		 We should just be able to check for a REG_DEAD note
		 on a function call.  However, the REG_DEAD notes are
		 apparently not dependable around libcalls; c-torture
		 execute/920501-2 is a test case.  If SCANSET is set,
		 then this insn sets the register, so it must have
		 died earlier.  Unfortunately, this will only handle
		 the cases in which the register is, in fact, set in a
		 later insn.  */

	      /* ??? We shouldn't have to use FOUNDINSN here.
		 However, the LOG_LINKS fields are apparently not
		 entirely reliable around libcalls;
		 newlib/libm/math/e_pow.c is a test case.  Sometimes
		 an insn will appear in LOG_LINKS even though it is
		 not the most recent insn which sets the register. */

	      if (foundinsn
		  && (scanset
		      || find_reg_note (scan, REG_DEAD, reg)))
		{
		  dies = scan;
		  break;
		}
	    }

	  if (! dies)
	    {
	      /* Either there was a branch, or some insn used REG
                 other than as a function call address.  */
	      continue;
	    }

	  /* Create a code label, and put it in a REG_LABEL note on
             the insn which sets the register, and on each call insn
             which uses the register.  In final_prescan_insn we look
             for the REG_LABEL notes, and output the appropriate label
             or pseudo-op.  */

	  label = gen_label_rtx ();
	  REG_NOTES (link) = gen_rtx (EXPR_LIST, REG_LABEL, label,
				      REG_NOTES (link));
	  REG_NOTES (insn) = gen_rtx (EXPR_LIST, REG_LABEL, label,
				      REG_NOTES (insn));
	  if (rescan)
	    {
	      scan = link;
	      do
		{
		  rtx reg2;

		  scan = NEXT_INSN (scan);
		  if (scan != insn
		      && ((GET_CODE (scan) == CALL_INSN
			   && reg_mentioned_p (reg, scan))
			  || ((reg2 = sfunc_uses_reg (scan))
			      && REGNO (reg2) == REGNO (reg))))
		    REG_NOTES (scan) = gen_rtx (EXPR_LIST, REG_LABEL,
						label, REG_NOTES (scan));
		}
	      while (scan != dies);
	    }
	}
    }

  if (TARGET_SH2)
    fixup_addr_diff_vecs (first);

  if (optimize)
    {
      mdep_reorg_phase = SH_SHORTEN_BRANCHES0;
      shorten_branches (first);
    }
  /* Scan the function looking for move instructions which have to be
     changed to pc-relative loads and insert the literal tables.  */

  mdep_reorg_phase = SH_FIXUP_PCLOAD;
  for (insn = first, num_mova = 0; insn; insn = NEXT_INSN (insn))
    {
      if (mova_p (insn))
	{
	  if (! num_mova++)
	    mova = insn;
	}
      else if (GET_CODE (insn) == JUMP_INSN
	       && GET_CODE (PATTERN (insn)) == ADDR_DIFF_VEC
	       && num_mova)
	{
	  rtx scan;
	  int total;

	  num_mova--;

	  /* Some code might have been inserted between the mova and
	     its ADDR_DIFF_VEC.  Check if the mova is still in range.  */
	  for (scan = mova, total = 0; scan != insn; scan = NEXT_INSN (scan))
	    total += get_attr_length (scan);

	  /* range of mova is 1020, add 4 because pc counts from address of
	     second instruction after this one, subtract 2 in case pc is 2
	     byte aligned.  Possible alignment needed for the ADDR_DIFF_VEC
	     cancels out with alignment effects of the mova itself.  */
	  if (total > 1022)
	    {
	      /* Change the mova into a load, and restart scanning
		 there.  broken_move will then return true for mova.  */
	      SET_SRC (PATTERN (mova))
		= XVECEXP (SET_SRC (PATTERN (mova)), 0, 0);
	      INSN_CODE (mova) = -1;
	      insn = mova;
	    }
	}
      if (broken_move (insn))
	{
	  rtx scan;
	  /* Scan ahead looking for a barrier to stick the constant table
	     behind.  */
	  rtx barrier = find_barrier (num_mova, mova, insn);
	  rtx last_float_move, last_float = 0, *last_float_addr;

	  if (num_mova && ! mova_p (mova))
	    {
	      /* find_barrier had to change the first mova into a
		 pcload; thus, we have to start with this new pcload.  */
	      insn = mova;
	      num_mova = 0;
	    }
	  /* Now find all the moves between the points and modify them.  */
	  for (scan = insn; scan != barrier; scan = NEXT_INSN (scan))
	    {
	      if (GET_CODE (scan) == CODE_LABEL)
		last_float = 0;
	      if (broken_move (scan))
		{
		  rtx *patp = &PATTERN (scan), pat = *patp;
		  rtx src, dst;
		  rtx lab;
		  rtx newinsn;
		  rtx newsrc;
		  enum machine_mode mode;

		  if (GET_CODE (pat) == PARALLEL)
		    patp = &XVECEXP (pat, 0, 0), pat = *patp;
		  src = SET_SRC (pat);
		  dst = SET_DEST (pat);
		  mode = GET_MODE (dst);

		  if (mode == SImode && hi_const (src)
		      && REGNO (dst) != FPUL_REG)
		    {
		      int offset = 0;

		      mode = HImode;
		      while (GET_CODE (dst) == SUBREG)
			{
			  offset += SUBREG_WORD (dst);
			  dst = SUBREG_REG (dst);
			}
		      dst = gen_rtx (REG, HImode, REGNO (dst) + offset);
		    }

		  if (GET_CODE (dst) == REG
		      && ((REGNO (dst) >= FIRST_FP_REG
			   && REGNO (dst) <= LAST_XD_REG)
			  || REGNO (dst) == FPUL_REG))
		    {
		      if (last_float
			  && reg_set_between_p (r0_rtx, last_float_move, scan))
			last_float = 0;
		      lab = add_constant (src, mode, last_float);
		      if (lab)
			emit_insn_before (gen_mova (lab), scan);
		      else
			*last_float_addr = r0_inc_rtx;
		      last_float_move = scan;
		      last_float = src;
		      newsrc = gen_rtx (MEM, mode,
					((TARGET_SH4 && ! TARGET_FMOVD
					  || REGNO (dst) == FPUL_REG)
					 ? r0_inc_rtx
					 : r0_rtx));
		      last_float_addr = &XEXP (newsrc, 0);
		    }
		  else
		    {
		      lab = add_constant (src, mode, 0);
		      newsrc = gen_rtx (MEM, mode,
					gen_rtx (LABEL_REF, VOIDmode, lab));
		    }
		  RTX_UNCHANGING_P (newsrc) = 1;
		  *patp = gen_rtx (SET, VOIDmode, dst, newsrc);
		  INSN_CODE (scan) = -1;
		}
	    }
	  dump_table (barrier);
	  insn = barrier;
	}
    }

  mdep_reorg_phase = SH_SHORTEN_BRANCHES1;
  insn_addresses = 0;
  split_branches (first);

  /* The INSN_REFERENCES_ARE_DELAYED in sh.h is problematic because it
     also has an effect on the register that holds the addres of the sfunc.
     Insert an extra dummy insn in front of each sfunc that pretends to
     use this register.  */
  if (flag_delayed_branch)
    {
      for (insn = first; insn; insn = NEXT_INSN (insn))
	{
	  rtx reg = sfunc_uses_reg (insn);

	  if (! reg)
	    continue;
	  emit_insn_before (gen_use_sfunc_addr (reg), insn);
	}
    }
#if 0
  /* fpscr is not actually a user variable, but we pretend it is for the
     sake of the previous optimization passes, since we want it handled like
     one.  However, we don't have eny debugging information for it, so turn
     it into a non-user variable now.  */
  if (TARGET_SH4)
    REG_USERVAR_P (get_fpscr_rtx ()) = 0;
#endif
  if (optimize)
    sh_flag_remove_dead_before_cse = 1;
  mdep_reorg_phase = SH_AFTER_MDEP_REORG;
}

int
get_dest_uid (label, max_uid)
     rtx label;
     int max_uid;
{
  rtx dest = next_real_insn (label);
  int dest_uid;
  if (! dest)
    /* This can happen for an undefined label.  */
    return 0;
  dest_uid = INSN_UID (dest);
  /* If this is a newly created branch redirection blocking instruction,
     we cannot index the branch_uid or insn_addresses arrays with its
     uid.  But then, we won't need to, because the actual destination is
     the following branch.  */
  while (dest_uid >= max_uid)
    {
      dest = NEXT_INSN (dest);
      dest_uid = INSN_UID (dest);
    }
  if (GET_CODE (dest) == JUMP_INSN && GET_CODE (PATTERN (dest)) == RETURN)
    return 0;
  return dest_uid;
}

/* Split condbranches that are out of range.  Also add clobbers for
   scratch registers that are needed in far jumps.
   We do this before delay slot scheduling, so that it can take our
   newly created instructions into account.  It also allows us to
   find branches with common targets more easily.  */

static void
split_branches (first)
     rtx first;
{
  rtx insn;
  struct far_branch **uid_branch, *far_branch_list = 0;
  int max_uid = get_max_uid ();

  /* Find out which branches are out of range.  */
  shorten_branches (first);

  uid_branch = (struct far_branch **) alloca (max_uid * sizeof *uid_branch);
  bzero ((char *) uid_branch, max_uid * sizeof *uid_branch);

  for (insn = first; insn; insn = NEXT_INSN (insn))
    if (GET_RTX_CLASS (GET_CODE (insn)) != 'i')
      continue;
    else if (INSN_DELETED_P (insn))
      {
	/* Shorten_branches would split this instruction again,
	   so transform it into a note.  */
	PUT_CODE (insn, NOTE);
	NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED;
	NOTE_SOURCE_FILE (insn) = 0;
      }
    else if (GET_CODE (insn) == JUMP_INSN
	     /* Don't mess with ADDR_DIFF_VEC */
	     && (GET_CODE (PATTERN (insn)) == SET
		 || GET_CODE (PATTERN (insn)) == RETURN))
      {
	enum attr_type type = get_attr_type (insn);
	if (type == TYPE_CBRANCH)
	  {
	    rtx next, beyond;
    
	    if (get_attr_length (insn) > 4)
	      {
		rtx src = SET_SRC (PATTERN (insn));
		rtx cond = XEXP (src, 0);
		rtx olabel = XEXP (XEXP (src, 1), 0);
		rtx jump;
		int addr = insn_addresses[INSN_UID (insn)];
		rtx label = 0;
		int dest_uid = get_dest_uid (olabel, max_uid);
		struct far_branch *bp = uid_branch[dest_uid];
    
		/* redirect_jump needs a valid JUMP_LABEL, and it might delete
		   the label if the LABEL_NUSES count drops to zero.  There is
		   always a jump_optimize pass that sets these values, but it
		   proceeds to delete unreferenced code, and then if not
		   optimizing, to un-delete the deleted instructions, thus
		   leaving labels with too low uses counts.  */
		if (! optimize)
		  {
		    JUMP_LABEL (insn) = olabel;
		    LABEL_NUSES (olabel)++;
		  }
		if (! bp)
		  {
		    bp = (struct far_branch *) alloca (sizeof *bp);
		    uid_branch[dest_uid] = bp;
		    bp->prev = far_branch_list;
		    far_branch_list = bp;
		    bp->far_label
		      = XEXP (XEXP (SET_SRC (PATTERN (insn)), 1), 0);
		    LABEL_NUSES (bp->far_label)++;
		  }
		else
		  {
		    label = bp->near_label;
		    if (! label && bp->address - addr >= CONDJUMP_MIN)
		      {
			rtx block = bp->insert_place;

			if (GET_CODE (PATTERN (block)) == RETURN)
			  block = PREV_INSN (block);
			else
			  block = gen_block_redirect (block,
						      bp->address, 2);
			label = emit_label_after (gen_label_rtx (),
						  PREV_INSN (block));
			bp->near_label = label;
		      }
		    else if (label && ! NEXT_INSN (label))
		      if (addr + 2 - bp->address <= CONDJUMP_MAX)
			bp->insert_place = insn;
		      else
			gen_far_branch (bp);
		  }
		if (! label
		    || NEXT_INSN (label) && bp->address - addr < CONDJUMP_MIN)
		  {
		    bp->near_label = label = gen_label_rtx ();
		    bp->insert_place = insn;
		    bp->address = addr;
		  }
		if (! redirect_jump (insn, label))
		  abort ();
	      }
	    else
	      {
		/* get_attr_length (insn) == 2 */
		/* Check if we have a pattern where reorg wants to redirect
		   the branch to a label from an unconditional branch that
		   is too far away.  */
		/* We can't use JUMP_LABEL here because it might be undefined
		   when not optimizing.  */
		/* A syntax error might cause beyond to be NULL_RTX.  */
		beyond
		  = next_active_insn (XEXP (XEXP (SET_SRC (PATTERN (insn)), 1),
					    0));
	
		if (beyond
		    && (GET_CODE (beyond) == JUMP_INSN
			|| (GET_CODE (beyond = next_active_insn (beyond))
			    == JUMP_INSN))
		    && GET_CODE (PATTERN (beyond)) == SET
		    && recog_memoized (beyond) == CODE_FOR_jump
		    && ((insn_addresses[INSN_UID (XEXP (SET_SRC (PATTERN (beyond)), 0))]
			 - insn_addresses[INSN_UID (insn)] + 252U)
			> 252 + 258 + 2))
		  gen_block_redirect (beyond,
				      insn_addresses[INSN_UID (beyond)], 1);
	      }
    
	    next = next_active_insn (insn);

	    if ((GET_CODE (next) == JUMP_INSN
		 || GET_CODE (next = next_active_insn (next)) == JUMP_INSN)
		&& GET_CODE (PATTERN (next)) == SET
		&& recog_memoized (next) == CODE_FOR_jump
		&& ((insn_addresses[INSN_UID (XEXP (SET_SRC (PATTERN (next)), 0))]
		     - insn_addresses[INSN_UID (insn)] + 252U)
		    > 252 + 258 + 2))
	      gen_block_redirect (next, insn_addresses[INSN_UID (next)], 1);
	  }
	else if (type == TYPE_JUMP || type == TYPE_RETURN)
	  {
	    int addr = insn_addresses[INSN_UID (insn)];
	    rtx far_label = 0;
	    int dest_uid = 0;
	    struct far_branch *bp;

	    if (type == TYPE_JUMP)
	      {
		far_label = XEXP (SET_SRC (PATTERN (insn)), 0);
		dest_uid = get_dest_uid (far_label, max_uid);
		if (! dest_uid)
		  {
		    /* Parse errors can lead to labels outside
		      the insn stream.  */
		    if (! NEXT_INSN (far_label))
		      continue;

		    if (! optimize)
		      {
			JUMP_LABEL (insn) = far_label;
			LABEL_NUSES (far_label)++;
		      }
		    redirect_jump (insn, NULL_RTX);
		    far_label = 0;
		  }
	      }
	    bp = uid_branch[dest_uid];
	    if (! bp)
	      {
		bp = (struct far_branch *) alloca (sizeof *bp);
		uid_branch[dest_uid] = bp;
		bp->prev = far_branch_list;
		far_branch_list = bp;
		bp->near_label = 0;
		bp->far_label = far_label;
		if (far_label)
		  LABEL_NUSES (far_label)++;
	      }
	    else if (bp->near_label && ! NEXT_INSN (bp->near_label))
	      if (addr - bp->address <= CONDJUMP_MAX)
		emit_label_after (bp->near_label, PREV_INSN (insn));
	      else
		{
		  gen_far_branch (bp);
		  bp->near_label = 0;
		}
	    else
	      bp->near_label = 0;
	    bp->address = addr;
	    bp->insert_place = insn;
	    if (! far_label)
	      emit_insn_before (gen_block_branch_redirect (const0_rtx), insn);
	    else
	      gen_block_redirect (insn, addr, bp->near_label ? 2 : 0);
	  }
      }
  /* Generate all pending far branches,
     and free our references to the far labels.  */
  while (far_branch_list)
    {
      if (far_branch_list->near_label
	  && ! NEXT_INSN (far_branch_list->near_label))
	gen_far_branch (far_branch_list);
      if (optimize
	  && far_branch_list->far_label
	  && ! --LABEL_NUSES (far_branch_list->far_label))
	delete_insn (far_branch_list->far_label);
      far_branch_list = far_branch_list->prev;
    }

  /* Instruction length information is no longer valid due to the new
     instructions that have been generated.  */
  init_insn_lengths ();
}

/* Dump out instruction addresses, which is useful for debugging the
   constant pool table stuff.

   If relaxing, output the label and pseudo-ops used to link together
   calls and the instruction which set the registers.  */

/* ??? This is unnecessary, and probably should be deleted.  This makes
   the insn_addresses declaration above unnecessary.  */

/* ??? The addresses printed by this routine for insns are nonsense for
   insns which are inside of a sequence where none of the inner insns have
   variable length.  This is because the second pass of shorten_branches
   does not bother to update them.  */

void
final_prescan_insn (insn, opvec, noperands)
     rtx insn;
     rtx *opvec;
     int noperands;
{
  if (TARGET_DUMPISIZE)
    fprintf (asm_out_file, "\n! at %04x\n", insn_addresses[INSN_UID (insn)]);

  if (TARGET_RELAX)
    {
      rtx note;

      note = find_reg_note (insn, REG_LABEL, NULL_RTX);
      if (note)
	{
	  rtx pattern;

	  pattern = PATTERN (insn);
	  if (GET_CODE (pattern) == PARALLEL)
	    pattern = XVECEXP (pattern, 0, 0);
	  if (GET_CODE (pattern) == CALL
	      || (GET_CODE (pattern) == SET
		  && (GET_CODE (SET_SRC (pattern)) == CALL
		      || get_attr_type (insn) == TYPE_SFUNC)))
	    asm_fprintf (asm_out_file, "\t.uses %LL%d\n",
			 CODE_LABEL_NUMBER (XEXP (note, 0)));
	  else if (GET_CODE (pattern) == SET)
	    ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L",
				       CODE_LABEL_NUMBER (XEXP (note, 0)));
	  else
	    abort ();
	}
    }
}

/* Dump out any constants accumulated in the final pass.  These will
   only be labels.  */

char *
output_jump_label_table ()
{
  int i;

  if (pool_size)
    {
      fprintf (asm_out_file, "\t.align 2\n");
      for (i = 0; i < pool_size; i++)
	{
	  pool_node *p = &pool_vector[i];

	  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L",
				     CODE_LABEL_NUMBER (p->label));
	  output_asm_insn (".long	%O0", &p->value);
	}
      pool_size = 0;
    }

  return "";
}

/* A full frame looks like:

   arg-5
   arg-4
   [ if current_function_anonymous_args
   arg-3
   arg-2
   arg-1
   arg-0 ]
   saved-fp
   saved-r10
   saved-r11
   saved-r12
   saved-pr
   local-n
   ..
   local-1
   local-0        <- fp points here.  */

/* Number of bytes pushed for anonymous args, used to pass information
   between expand_prologue and expand_epilogue.  */

static int extra_push;

/* Adjust the stack by SIZE bytes.  REG holds the rtl of the register
  to be adjusted, and TEMP, if nonnegative, holds the register number
  of a general register that we may clobber.  */

static void
output_stack_adjust (size, reg, temp)
     int size;
     rtx reg;
     int temp;
{
  if (size)
    {
      if (CONST_OK_FOR_I (size))
	emit_insn (gen_addsi3 (reg, reg, GEN_INT (size)));
      /* Try to do it with two partial adjustments; however, we must make
	 sure that the stack is properly aligned at all times, in case
	 an interrupt occurs between the two partial adjustments. */
      else if (CONST_OK_FOR_I (size / 2 & -4)
	       && CONST_OK_FOR_I (size - (size / 2 & -4)))
	{
	  emit_insn (gen_addsi3 (reg, reg, GEN_INT (size / 2 & -4)));
	  emit_insn (gen_addsi3 (reg, reg, GEN_INT (size - (size / 2 & -4))));
	}
      else
	{
	  rtx const_reg;

	  /* If TEMP is invalid, we could temporarily save a general
	     register to MACL.  However, there is currently no need
	     to handle this case, so just abort when we see it.  */
	  if (temp < 0)
	    abort ();
	  const_reg = gen_rtx (REG, SImode, temp);

	  /* If SIZE is negative, subtract the positive value.
	     This sometimes allows a constant pool entry to be shared
	     between prologue and epilogue code.  */
	  if (size < 0)
	    {
	      emit_insn (gen_movsi (const_reg, GEN_INT (-size)));
	      emit_insn (gen_subsi3 (reg, reg, const_reg));
	    }
	  else
	    {
	      emit_insn (gen_movsi (const_reg, GEN_INT (size)));
	      emit_insn (gen_addsi3 (reg, reg, const_reg));
	    }
	}
    }
}

/* Output RTL to push register RN onto the stack.  */

static void
push (rn)
     int rn;
{
  rtx x;
  if (rn == FPUL_REG)
    x = gen_push_fpul ();
  else if (TARGET_SH4 && TARGET_FMOVD && ! TARGET_FPU_SINGLE
	   && rn >= FIRST_FP_REG && rn <= LAST_XD_REG)
    {
      if ((rn - FIRST_FP_REG) & 1 && rn <= LAST_FP_REG)
	return;
      x = gen_push_4 (gen_rtx (REG, DFmode, rn));
    }
  else if (TARGET_SH3E && rn >= FIRST_FP_REG && rn <= LAST_FP_REG)
    x = gen_push_e (gen_rtx (REG, SFmode, rn));
  else
    x = gen_push (gen_rtx (REG, SImode, rn));

  x = emit_insn (x);
  REG_NOTES (x) = gen_rtx (EXPR_LIST, REG_INC,
			   gen_rtx(REG, SImode, STACK_POINTER_REGNUM), 0);
}

/* Output RTL to pop register RN from the stack.  */

static void
pop (rn)
     int rn;
{
  rtx x;
  if (rn == FPUL_REG)
    x = gen_pop_fpul ();
  else if (TARGET_SH4 && TARGET_FMOVD && ! TARGET_FPU_SINGLE
	   && rn >= FIRST_FP_REG && rn <= LAST_XD_REG)
    {
      if ((rn - FIRST_FP_REG) & 1 && rn <= LAST_FP_REG)
	return;
      x = gen_pop_4 (gen_rtx (REG, DFmode, rn));
    }
  else if (TARGET_SH3E && rn >= FIRST_FP_REG && rn <= LAST_FP_REG)
    x = gen_pop_e (gen_rtx (REG, SFmode, rn));
  else
    x = gen_pop (gen_rtx (REG, SImode, rn));
    
  x = emit_insn (x);
  REG_NOTES (x) = gen_rtx (EXPR_LIST, REG_INC,
			   gen_rtx(REG, SImode, STACK_POINTER_REGNUM), 0);
}

/* Generate code to push the regs specified in the mask.  */

static void
push_regs (mask, mask2)
     int mask, mask2;
{
  int i;

  /* Push PR last; this gives better latencies after the prologue, and
     candidates for the return delay slot when there are no general
     registers pushed.  */
  for (i = 0; i < 32; i++)
    if (mask & (1 << i) && i != PR_REG)
      push (i);
  for (i = 32; i < FIRST_PSEUDO_REGISTER; i++)
    if (mask2 & (1 << (i - 32)))
      push (i);
  if (mask & (1 << PR_REG))
    push (PR_REG);
}

/* Work out the registers which need to be saved, both as a mask and a
   count of saved words.

   If doing a pragma interrupt function, then push all regs used by the
   function, and if we call another function (we can tell by looking at PR),
   make sure that all the regs it clobbers are safe too.  */

static int
calc_live_regs (count_ptr, live_regs_mask2)
     int *count_ptr;
     int *live_regs_mask2;
{
  int reg;
  int live_regs_mask = 0;
  int count;
  int interrupt_handler;

  if ((lookup_attribute
       ("interrupt_handler",
	DECL_MACHINE_ATTRIBUTES (current_function_decl)))
      != NULL_TREE)
    interrupt_handler = 1;
  else
    interrupt_handler = 0;

  *live_regs_mask2 = 0;
  /* If we can save a lot of saves by switching to double mode, do that.  */
  if (TARGET_SH4 && TARGET_FMOVD && TARGET_FPU_SINGLE)
    for (count = 0, reg = FIRST_FP_REG; reg <= LAST_FP_REG; reg += 2)
      if (regs_ever_live[reg] && regs_ever_live[reg+1]
	  && (! call_used_regs[reg] || (interrupt_handler && ! pragma_trapa))
	  && ++count > 2)
	{
	  target_flags &= ~FPU_SINGLE_BIT;
	  break;
	}
  for (count = 0, reg = FIRST_PSEUDO_REGISTER - 1; reg >= 0; reg--)
    {
      if ((interrupt_handler && ! pragma_trapa)
	  ? (/* Need to save all the regs ever live.  */
	     (regs_ever_live[reg]
	      || (call_used_regs[reg]
		  && (! fixed_regs[reg] || reg == MACH_REG || reg == MACL_REG)
		  && regs_ever_live[PR_REG]))
	     && reg != STACK_POINTER_REGNUM && reg != ARG_POINTER_REGNUM
	     && reg != RETURN_ADDRESS_POINTER_REGNUM
	     && reg != T_REG && reg != GBR_REG && reg != FPSCR_REG)
	  : (/* Only push those regs which are used and need to be saved.  */
	     regs_ever_live[reg] && ! call_used_regs[reg]))
	{
	  if (reg >= 32)
	    *live_regs_mask2 |= 1 << (reg - 32);
	  else
	    live_regs_mask |= 1 << reg;
	  count++;
	  if (TARGET_SH4 && TARGET_FMOVD && reg >= FIRST_FP_REG)
	    if (reg <= LAST_FP_REG)
	      {
		if (! TARGET_FPU_SINGLE && ! regs_ever_live[reg ^ 1])
		  {
		    if (reg >= 32)
		      *live_regs_mask2 |= 1 << ((reg ^ 1) - 32);
		    else
		      live_regs_mask |= 1 << (reg ^ 1);
		    count++;
		  }
	      }
	    else if (reg <= LAST_XD_REG)
	      {
		/* Must switch to double mode to access these registers.  */
		target_flags &= ~FPU_SINGLE_BIT;
		count++;
	      }
	}
    }

  *count_ptr = count;
  return live_regs_mask;
}

/* Code to generate prologue and epilogue sequences */

void
sh_expand_prologue ()
{
  int live_regs_mask;
  int d, i;
  int live_regs_mask2;
  int save_flags = target_flags;
  int double_align = 0;

  /* We have pretend args if we had an object sent partially in registers
     and partially on the stack, e.g. a large structure.  */
  output_stack_adjust (-current_function_pretend_args_size,
		       stack_pointer_rtx, 3);

  extra_push = 0;

  /* This is set by SETUP_VARARGS to indicate that this is a varargs
     routine.  Clear it here so that the next function isn't affected. */
  if (current_function_anonymous_args)
    {
      current_function_anonymous_args = 0;

      /* This is not used by the SH3E calling convention  */
      if (! TARGET_SH3E && ! TARGET_HITACHI)
	{
	  /* Push arg regs as if they'd been provided by caller in stack.  */
	  for (i = 0; i < NPARM_REGS(SImode); i++)
	    {
	      int rn = NPARM_REGS(SImode) + FIRST_PARM_REG - i - 1;
	      if (i >= (NPARM_REGS(SImode) 
			- current_function_args_info.arg_count[(int) SH_ARG_INT]
			))
		break;
	      push (rn);
	      extra_push += 4;
	    }
	}
    }

  /* If we're supposed to switch stacks at function entry, do so now.  */
  if (sp_switch)
    emit_insn (gen_sp_switch_1 ());

  live_regs_mask = calc_live_regs (&d, &live_regs_mask2);
  /* ??? Maybe we could save some switching if we can move a mode switch
     that already happens to be at the function start into the prologue.  */
  if (target_flags != save_flags)
    emit_insn (gen_toggle_sz ());
  push_regs (live_regs_mask, live_regs_mask2);
  if (target_flags != save_flags)
    emit_insn (gen_toggle_sz ());

  if (TARGET_ALIGN_DOUBLE && d & 1)
    double_align = 4;

  target_flags = save_flags;

  output_stack_adjust (-get_frame_size () - double_align,
		       stack_pointer_rtx, 3);

  if (frame_pointer_needed)
    emit_insn (gen_movsi (frame_pointer_rtx, stack_pointer_rtx));
}

void
sh_expand_epilogue ()
{
  int live_regs_mask;
  int d, i;

  int live_regs_mask2;
  int save_flags = target_flags;
  int frame_size = get_frame_size ();

  live_regs_mask = calc_live_regs (&d, &live_regs_mask2);

  if (TARGET_ALIGN_DOUBLE && d & 1)
    frame_size += 4;

  if (frame_pointer_needed)
    {
      output_stack_adjust (frame_size, frame_pointer_rtx, 7);

      /* We must avoid moving the stack pointer adjustment past code
	 which reads from the local frame, else an interrupt could
	 occur after the SP adjustment and clobber data in the local
	 frame.  */
      emit_insn (gen_blockage ());
      emit_insn (gen_movsi (stack_pointer_rtx, frame_pointer_rtx));
    }
  else if (frame_size)
    {
      /* We must avoid moving the stack pointer adjustment past code
	 which reads from the local frame, else an interrupt could
	 occur after the SP adjustment and clobber data in the local
	 frame.  */
      emit_insn (gen_blockage ());
      output_stack_adjust (frame_size, stack_pointer_rtx, 7);
    }

  /* Pop all the registers.  */

  if (target_flags != save_flags)
    emit_insn (gen_toggle_sz ());
  if (live_regs_mask & (1 << PR_REG))
    pop (PR_REG);
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      int j = (FIRST_PSEUDO_REGISTER - 1) - i;
      if (j < 32 && (live_regs_mask & (1 << j)) && j != PR_REG)
	pop (j);
      else if (j >= 32 && (live_regs_mask2 & (1 << (j - 32))))
	pop (j);
    }
  if (target_flags != save_flags)
    emit_insn (gen_toggle_sz ());
  target_flags = save_flags;

  output_stack_adjust (extra_push + current_function_pretend_args_size,
		       stack_pointer_rtx, 7);

  /* Switch back to the normal stack if necessary.  */
  if (sp_switch)
    emit_insn (gen_sp_switch_2 ());
}

/* Clear variables at function end.  */

void
function_epilogue (stream, size)
     FILE *stream;
     int size;
{
  trap_exit = pragma_interrupt = pragma_trapa = pragma_nosave_low_regs = 0;
  sp_switch = NULL_RTX;
}

rtx
sh_builtin_saveregs (arglist)
     tree arglist;
{
  tree fntype = TREE_TYPE (current_function_decl);
  /* First unnamed integer register.  */
  int first_intreg = current_function_args_info.arg_count[(int) SH_ARG_INT];
  /* Number of integer registers we need to save.  */
  int n_intregs = MAX (0, NPARM_REGS (SImode) - first_intreg);
  /* First unnamed SFmode float reg */
  int first_floatreg = current_function_args_info.arg_count[(int) SH_ARG_FLOAT];
  /* Number of SFmode float regs to save.  */
  int n_floatregs = MAX (0, NPARM_REGS (SFmode) - first_floatreg);
  int ptrsize = GET_MODE_SIZE (Pmode);
  rtx valist, regbuf, fpregs;
  int bufsize, regno;

  /* Allocate block of memory for the regs. */
  /* ??? If n_intregs + n_floatregs == 0, should we allocate at least 1 byte?
     Or can assign_stack_local accept a 0 SIZE argument?  */
  bufsize = (n_intregs * UNITS_PER_WORD) + (n_floatregs * UNITS_PER_WORD);

  regbuf = assign_stack_local (BLKmode, bufsize, 0);
  MEM_SET_IN_STRUCT_P (regbuf, 1);

  /* Save int args.
     This is optimized to only save the regs that are necessary.  Explicitly
     named args need not be saved.  */
  if (n_intregs > 0)
    move_block_from_reg (BASE_ARG_REG (SImode) + first_intreg,
			 gen_rtx (MEM, BLKmode, 
			 	plus_constant (XEXP (regbuf, 0),
					n_floatregs * UNITS_PER_WORD)), 
			 n_intregs, n_intregs * UNITS_PER_WORD);

  /* Save float args.
     This is optimized to only save the regs that are necessary.  Explicitly
     named args need not be saved.
     We explicitly build a pointer to the buffer because it halves the insn
     count when not optimizing (otherwise the pointer is built for each reg
     saved).
     We emit the moves in reverse order so that we can use predecrement.  */

  fpregs = gen_reg_rtx (Pmode);
  emit_move_insn (fpregs, XEXP (regbuf, 0));
  emit_insn (gen_addsi3 (fpregs, fpregs,
			 GEN_INT (n_floatregs * UNITS_PER_WORD)));
  if (TARGET_SH4)
    {
      for (regno = NPARM_REGS (DFmode) - 2; regno >= first_floatreg; regno -= 2)
	{
	  emit_insn (gen_addsi3 (fpregs, fpregs,
				 GEN_INT (-2 * UNITS_PER_WORD)));
	  emit_move_insn (gen_rtx (MEM, DFmode, fpregs),
			  gen_rtx (REG, DFmode, BASE_ARG_REG (DFmode) + regno));
	}
      regno = first_floatreg;
      if (regno & 1)
	{
	  emit_insn (gen_addsi3 (fpregs, fpregs, GEN_INT (- UNITS_PER_WORD)));
	  emit_move_insn (gen_rtx (MEM, SFmode, fpregs),
			  gen_rtx (REG, SFmode, BASE_ARG_REG (SFmode) + regno
						- (TARGET_LITTLE_ENDIAN != 0)));
	}
    }
  else
    for (regno = NPARM_REGS (SFmode) - 1; regno >= first_floatreg; regno--)
      {
	emit_insn (gen_addsi3 (fpregs, fpregs, GEN_INT (- UNITS_PER_WORD)));
	emit_move_insn (gen_rtx (MEM, SFmode, fpregs),
			gen_rtx (REG, SFmode, BASE_ARG_REG (SFmode) + regno));
      }

  /* Return the address of the regbuf.  */
  return XEXP (regbuf, 0);
}

/* Define the offset between two registers, one to be eliminated, and
   the other its replacement, at the start of a routine.  */

int
initial_elimination_offset (from, to)
     int from;
     int to;
{
  int regs_saved;
  int total_saved_regs_space;
  int total_auto_space = get_frame_size ();
  int save_flags = target_flags;

  int live_regs_mask, live_regs_mask2;
  live_regs_mask = calc_live_regs (&regs_saved, &live_regs_mask2);
  if (TARGET_ALIGN_DOUBLE && regs_saved & 1)
    total_auto_space += 4;
  target_flags = save_flags;

  total_saved_regs_space = (regs_saved) * 4;

  if (from == ARG_POINTER_REGNUM && to == FRAME_POINTER_REGNUM)
    return total_saved_regs_space + total_auto_space;

  if (from == ARG_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    return total_saved_regs_space + total_auto_space;

  /* Initial gap between fp and sp is 0.  */
  if (from == FRAME_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    return 0;

  if (from == RETURN_ADDRESS_POINTER_REGNUM
      && (to == FRAME_POINTER_REGNUM || to == STACK_POINTER_REGNUM))
    {
      int i, n = total_saved_regs_space;
      for (i = PR_REG-1; i >= 0; i--)
	if (live_regs_mask & (1 << i))
	  n -= 4;
      return n + total_auto_space;
    }

  abort ();
}

/* Handle machine specific pragmas to be semi-compatible with Hitachi
   compiler.  */

int
sh_handle_pragma (p_getc, p_ungetc, pname)
     int (*  p_getc)   PROTO((void));
     void (* p_ungetc) PROTO((int));
     char *  pname;
{
  int retval = 0;

  if (strcmp (pname, "interrupt") == 0)
    pragma_interrupt = retval = 1;
  else if (strcmp (pname, "trapa") == 0)
    pragma_interrupt = pragma_trapa = retval = 1;
  else if (strcmp (pname, "nosave_low_regs") == 0)
    pragma_nosave_low_regs = retval = 1;

  return retval;
}

/* Generate 'handle_interrupt' attribute for decls */

void
sh_pragma_insert_attributes (node, attributes, prefix)
     tree node;
     tree * attributes;
     tree * prefix;
{
  tree a;

  if (! pragma_interrupt
      || TREE_CODE (node) != FUNCTION_DECL)
    return;

  /* We are only interested in fields.  */
  if (TREE_CODE_CLASS (TREE_CODE (node)) != 'd')
    return;

  /* Add a 'handle_interrupt' attribute.  */
  * attributes = tree_cons (get_identifier ("interrupt_handler"), NULL, * attributes);

  return;
}

/* Return nonzero if ATTR is a valid attribute for DECL.
   ATTRIBUTES are any existing attributes and ARGS are the arguments
   supplied with ATTR.

   Supported attributes:

   interrupt_handler -- specifies this function is an interrupt handler.

   sp_switch -- specifies an alternate stack for an interrupt handler
   to run on.

   trap_exit -- use a trapa to exit an interrupt function instead of
   an rte instruction.  */

int
sh_valid_machine_decl_attribute (decl, attributes, attr, args)
     tree decl;
     tree attributes;
     tree attr;
     tree args;
{
  int retval = 0;

  if (TREE_CODE (decl) != FUNCTION_DECL)
    return 0;

  if (is_attribute_p ("interrupt_handler", attr))
    {
      return 1;
    }

  if (is_attribute_p ("sp_switch", attr))
    {
      /* The sp_switch attribute only has meaning for interrupt functions.  */
      if (!pragma_interrupt)
	return 0;

      /* sp_switch must have an argument.  */
      if (!args || TREE_CODE (args) != TREE_LIST)
	return 0;

      /* The argument must be a constant string.  */
      if (TREE_CODE (TREE_VALUE (args)) != STRING_CST)
	return 0;

      sp_switch = gen_rtx (SYMBOL_REF, VOIDmode,
			   TREE_STRING_POINTER (TREE_VALUE (args)));
      return 1;
    }

  if (is_attribute_p ("trap_exit", attr))
    {
      /* The trap_exit attribute only has meaning for interrupt functions.  */
      if (!pragma_interrupt)
	return 0;

      /* trap_exit must have an argument.  */
      if (!args || TREE_CODE (args) != TREE_LIST)
	return 0;

      /* The argument must be a constant integer.  */
      if (TREE_CODE (TREE_VALUE (args)) != INTEGER_CST)
	return 0;

      trap_exit = TREE_INT_CST_LOW (TREE_VALUE (args));
      return 1;
    }
}


/* Predicates used by the templates.  */

/* Returns 1 if OP is MACL, MACH or PR.  The input must be a REG rtx.
   Used only in general_movsrc_operand.  */

int
system_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  switch (REGNO (op))
    {
    case PR_REG:
    case MACL_REG:
    case MACH_REG:
      return 1;
    }
  return 0;
}

/* Returns 1 if OP can be source of a simple move operation.
   Same as general_operand, but a LABEL_REF is valid, PRE_DEC is
   invalid as are subregs of system registers.  */

int
general_movsrc_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == MEM)
    {
      rtx inside = XEXP (op, 0);
      if (GET_CODE (inside) == CONST)
	inside = XEXP (inside, 0);

      if (GET_CODE (inside) == LABEL_REF)
	return 1;

      if (GET_CODE (inside) == PLUS
	  && GET_CODE (XEXP (inside, 0)) == LABEL_REF
	  && GET_CODE (XEXP (inside, 1)) == CONST_INT)
	return 1;

      /* Only post inc allowed.  */
      if (GET_CODE (inside) == PRE_DEC)
	return 0;
    }

  if ((mode == QImode || mode == HImode)
      && (GET_CODE (op) == SUBREG
	  && GET_CODE (XEXP (op, 0)) == REG
	  && system_reg_operand (XEXP (op, 0), mode)))
    return 0;

  return general_operand (op, mode);
}

/* Returns 1 if OP can be a destination of a move.
   Same as general_operand, but no preinc allowed.  */

int
general_movdst_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  /* Only pre dec allowed.  */
  if (GET_CODE (op) == MEM && GET_CODE (XEXP (op, 0)) == POST_INC)
    return 0;

  return general_operand (op, mode);
}

/* Returns 1 if OP is a normal arithmetic register.  */

int
arith_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (register_operand (op, mode))
    {
      int regno;

      if (GET_CODE (op) == REG)
	regno = REGNO (op);
      else if (GET_CODE (op) == SUBREG && GET_CODE (SUBREG_REG (op)) == REG)
	regno = REGNO (SUBREG_REG (op));
      else
	return 1;

      return (regno != T_REG && regno != PR_REG
	      && (regno != FPUL_REG || TARGET_SH4)
	      && regno != MACH_REG && regno != MACL_REG);
    }
  return 0;
}

int
fp_arith_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (register_operand (op, mode))
    {
      int regno;

      if (GET_CODE (op) == REG)
	regno = REGNO (op);
      else if (GET_CODE (op) == SUBREG && GET_CODE (SUBREG_REG (op)) == REG)
	regno = REGNO (SUBREG_REG (op));
      else
	return 1;

      return (regno >= FIRST_PSEUDO_REGISTER
	      || (regno >= FIRST_FP_REG && regno <= LAST_FP_REG));
    }
  return 0;
}

int
fp_extended_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == FLOAT_EXTEND && GET_MODE (op) == mode)
    {
      op = XEXP (op, 0);
      mode = GET_MODE (op);
    }
  if (register_operand (op, mode))
    {
      int regno;

      if (GET_CODE (op) == REG)
	regno = REGNO (op);
      else if (GET_CODE (op) == SUBREG && GET_CODE (SUBREG_REG (op)) == REG)
	regno = REGNO (SUBREG_REG (op));
      else
	return 1;

      return (regno != T_REG && regno != PR_REG && regno > 15
	      && regno != MACH_REG && regno != MACL_REG);
    }
  return 0;
}

/* Returns 1 if OP is a valid source operand for an arithmetic insn.  */

int
arith_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (arith_reg_operand (op, mode))
    return 1;

  if (GET_CODE (op) == CONST_INT && CONST_OK_FOR_I (INTVAL (op)))
    return 1;

  return 0;
}

/* Returns 1 if OP is a valid source operand for a compare insn.  */

int
arith_reg_or_0_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (arith_reg_operand (op, mode))
    return 1;

  if (GET_CODE (op) == CONST_INT && CONST_OK_FOR_N (INTVAL (op)))
    return 1;

  return 0;
}

/* Returns 1 if OP is a valid source operand for a logical operation.  */

int
logical_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (arith_reg_operand (op, mode))
    return 1;

  if (GET_CODE (op) == CONST_INT && CONST_OK_FOR_L (INTVAL (op)))
    return 1;

  return 0;
}

/* Nonzero if OP is a floating point value with value 0.0.  */

int
fp_zero_operand (op)
     rtx op;
{
  REAL_VALUE_TYPE r;

  if (GET_MODE (op) != SFmode)
    return 0;

  REAL_VALUE_FROM_CONST_DOUBLE (r, op);
  return REAL_VALUES_EQUAL (r, dconst0) && ! REAL_VALUE_MINUS_ZERO (r);
}

/* Nonzero if OP is a floating point value with value 1.0.  */

int
fp_one_operand (op)
     rtx op;
{
  REAL_VALUE_TYPE r;

  if (GET_MODE (op) != SFmode)
    return 0;

  REAL_VALUE_FROM_CONST_DOUBLE (r, op);
  return REAL_VALUES_EQUAL (r, dconst1);
}

int
tertiary_reload_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (op);
  return code == MEM || (TARGET_SH4 && code == CONST_DOUBLE);
}

int
fpscr_operand (op)
     rtx op;
{
  return (GET_CODE (op) == REG && REGNO (op) == FPSCR_REG
	  && GET_MODE (op) == PSImode);
}

int
commutative_float_operator (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_MODE (op) != mode)
    return 0;
  switch (GET_CODE (op))
    {
    case PLUS:
    case MULT:
      return 1;
    }
  return 0;
}

int
noncommutative_float_operator (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_MODE (op) != mode)
    return 0;
  switch (GET_CODE (op))
    {
    case MINUS:
    case DIV:
      return 1;
    }
  return 0;
}

int
binary_float_operator (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_MODE (op) != mode)
    return 0;
  switch (GET_CODE (op))
    {
    case PLUS:
    case MINUS:
    case MULT:
    case DIV:
      return 1;
    }
  return 0;
}

/* Return the destination address of a branch.  */
   
int
branch_dest (branch)
     rtx branch;
{
  rtx dest = SET_SRC (PATTERN (branch));
  int dest_uid;

  if (GET_CODE (dest) == IF_THEN_ELSE)
    dest = XEXP (dest, 1);
  dest = XEXP (dest, 0);
  dest_uid = INSN_UID (dest);
  return insn_addresses[dest_uid];
}

/* Return non-zero if REG is not used after INSN.
   We assume REG is a reload reg, and therefore does
   not live past labels.  It may live past calls or jumps though.  */
int
reg_unused_after (reg, insn)
     rtx reg;
     rtx insn;
{
  enum rtx_code code;
  rtx set;

  /* If the reg is set by this instruction, then it is safe for our
     case.  Disregard the case where this is a store to memory, since
     we are checking a register used in the store address.  */
  set = single_set (insn);
  if (set && GET_CODE (SET_DEST (set)) != MEM
      && reg_overlap_mentioned_p (reg, SET_DEST (set)))
    return 1;

  while (insn = NEXT_INSN (insn))
    {
      code = GET_CODE (insn);

#if 0
      /* If this is a label that existed before reload, then the register
	 if dead here.  However, if this is a label added by reorg, then
	 the register may still be live here.  We can't tell the difference,
	 so we just ignore labels completely.  */
      if (code == CODE_LABEL)
	return 1;
      /* else */
#endif

      if (code == JUMP_INSN)
	return 0;

      /* If this is a sequence, we must handle them all at once.
	 We could have for instance a call that sets the target register,
	 and a insn in a delay slot that uses the register.  In this case,
	 we must return 0.  */
      else if (code == INSN && GET_CODE (PATTERN (insn)) == SEQUENCE)
	{
	  int i;
	  int retval = 0;

	  for (i = 0; i < XVECLEN (PATTERN (insn), 0); i++)
	    {
	      rtx this_insn = XVECEXP (PATTERN (insn), 0, i);
	      rtx set = single_set (this_insn);

	      if (GET_CODE (this_insn) == CALL_INSN)
		code = CALL_INSN;
	      else if (GET_CODE (this_insn) == JUMP_INSN)
		{
		  if (INSN_ANNULLED_BRANCH_P (this_insn))
		    return 0;
		  code = JUMP_INSN;
		}

	      if (set && reg_overlap_mentioned_p (reg, SET_SRC (set)))
		return 0;
	      if (set && reg_overlap_mentioned_p (reg, SET_DEST (set)))
		{
		  if (GET_CODE (SET_DEST (set)) != MEM)
		    retval = 1;
		  else
		    return 0;
		}
	      if (set == 0
		  && reg_overlap_mentioned_p (reg, PATTERN (this_insn)))
		return 0;
	    }
	  if (retval == 1)
	    return 1;
	  else if (code == JUMP_INSN)
	    return 0;
	}
      else if (GET_RTX_CLASS (code) == 'i')
	{
	  rtx set = single_set (insn);

	  if (set && reg_overlap_mentioned_p (reg, SET_SRC (set)))
	    return 0;
	  if (set && reg_overlap_mentioned_p (reg, SET_DEST (set)))
	    return GET_CODE (SET_DEST (set)) != MEM;
	  if (set == 0 && reg_overlap_mentioned_p (reg, PATTERN (insn)))
	    return 0;
	}

      if (code == CALL_INSN && call_used_regs[REGNO (reg)])
	return 1;
    }
  return 1;
}

extern struct obstack permanent_obstack;

rtx
get_fpscr_rtx ()
{
  static rtx fpscr_rtx;

  if (! fpscr_rtx)
    {
      push_obstacks (&permanent_obstack, &permanent_obstack);
      fpscr_rtx = gen_rtx (REG, PSImode, 48);
      REG_USERVAR_P (fpscr_rtx) = 1;
      pop_obstacks ();
      mark_user_reg (fpscr_rtx);
    }
  if (! reload_completed || mdep_reorg_phase != SH_AFTER_MDEP_REORG)
    mark_user_reg (fpscr_rtx);
  return fpscr_rtx;
}

void
emit_sf_insn (pat)
     rtx pat;
{
  rtx addr;
  /* When generating reload insns,  we must not create new registers.  FPSCR
     should already have the correct value, so do nothing to change it.  */
  if (! TARGET_FPU_SINGLE && ! reload_in_progress)
    {
      addr = gen_reg_rtx (SImode);
      emit_insn (gen_fpu_switch0 (addr));
    }
  emit_insn (pat);
  if (! TARGET_FPU_SINGLE && ! reload_in_progress)
    {
      addr = gen_reg_rtx (SImode);
      emit_insn (gen_fpu_switch1 (addr));
    }
}

void
emit_df_insn (pat)
     rtx pat;
{
  rtx addr;
  if (TARGET_FPU_SINGLE && ! reload_in_progress)
    {
      addr = gen_reg_rtx (SImode);
      emit_insn (gen_fpu_switch0 (addr));
    }
  emit_insn (pat);
  if (TARGET_FPU_SINGLE && ! reload_in_progress)
    {
      addr = gen_reg_rtx (SImode);
      emit_insn (gen_fpu_switch1 (addr));
    }
}

void
expand_sf_unop (fun, operands)
     rtx (*fun)();
     rtx *operands;
{
  emit_sf_insn ((*fun) (operands[0], operands[1], get_fpscr_rtx ()));
}

void
expand_sf_binop (fun, operands)
     rtx (*fun)();
     rtx *operands;
{
  emit_sf_insn ((*fun) (operands[0], operands[1], operands[2],
			 get_fpscr_rtx ()));
}

void
expand_df_unop (fun, operands)
     rtx (*fun)();
     rtx *operands;
{
  emit_df_insn ((*fun) (operands[0], operands[1], get_fpscr_rtx ()));
}

void
expand_df_binop (fun, operands)
     rtx (*fun)();
     rtx *operands;
{
  emit_df_insn ((*fun) (operands[0], operands[1], operands[2],
			 get_fpscr_rtx ()));
}

void
expand_fp_branch (compare, branch)
     rtx (*compare) (), (*branch) ();
{
  (GET_MODE (sh_compare_op0)  == SFmode ? emit_sf_insn : emit_df_insn)
    ((*compare) ());
  emit_jump_insn ((*branch) ());
}

/* We don't want to make fpscr call-saved, because that would prevent
   channging it, and it would also cost an exstra instruction to save it.
   We don't want it to be known as a global register either, because
   that disables all flow analysis.  But it has to be live at the function
   return.  Thus, we need to insert a USE at the end of the function.  */
/* This should best be called at about the time FINALIZE_PIC is called,
   but not dependent on flag_pic.  Alas, there is no suitable hook there,
   so this gets called from HAVE_RETURN.  */
int
emit_fpscr_use ()
{
  static int fpscr_uses = 0;

  if (rtx_equal_function_value_matters)
    {
      emit_insn (gen_rtx (USE, VOIDmode, get_fpscr_rtx ()));
      fpscr_uses++;
    }
  else
    {
      if (fpscr_uses > 1)
	{
	  /* Due to he crude way we emit the USEs, we might end up with
	     some extra ones.  Delete all but the last one.  */
	  rtx insn;

	  for (insn = get_last_insn(); insn; insn = PREV_INSN (insn))
	    if (GET_CODE (insn) == INSN
		&& GET_CODE (PATTERN (insn)) == USE
		&& GET_CODE (XEXP (PATTERN (insn), 0)) == REG
		&& REGNO (XEXP (PATTERN (insn), 0)) == FPSCR_REG)
	      {
		insn = PREV_INSN (insn);
		break;
	      }
	  for (; insn; insn = PREV_INSN (insn))
	    if (GET_CODE (insn) == INSN
		&& GET_CODE (PATTERN (insn)) == USE
		&& GET_CODE (XEXP (PATTERN (insn), 0)) == REG
		&& REGNO (XEXP (PATTERN (insn), 0)) == FPSCR_REG)
	      {
		PUT_CODE (insn, NOTE);
		NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED;
		NOTE_SOURCE_FILE (insn) = 0;
	      }
	}
      fpscr_uses = 0;
    }
}

/* ??? gcc does flow analysis strictly after common subexpression
   elimination.  As a result, common subespression elimination fails
   when there are some intervening statements setting the same register.
   If we did nothing about this, this would hurt the precision switching
   for SH4 badly.  There is some cse after reload, but it is unable to
   undo the extra register pressure from the unused instructions, and
   it cannot remove auto-increment loads.

   A C code example that shows this flow/cse weakness for (at least) SH
   and sparc (as of gcc ss-970706) is this:

double
f(double a)
{
  double d;
  d = 0.1;
  a += d;
  d = 1.1;
  d = 0.1;
  a *= d;
  return a;
}

   So we add another pass before common subexpression elimination, to
   remove assignments that are dead due to a following assignment in the
   same basic block.  */

int sh_flag_remove_dead_before_cse;

static void 
mark_use (x, reg_set_block)
     rtx x, *reg_set_block;
{
  enum rtx_code code;

  if (! x)
    return;
  code = GET_CODE (x);
  switch (code)
    {
    case REG:
      {
	int regno = REGNO (x);
	int nregs = (regno < FIRST_PSEUDO_REGISTER
		     ? HARD_REGNO_NREGS (regno, GET_MODE (x))
		     : 1);
	do
	  {
	    reg_set_block[regno + nregs - 1] = 0;
	  }
	while (--nregs);
	break;
      }
    case SET:
      {
	rtx dest = SET_DEST (x);

	if (GET_CODE (dest) == SUBREG)
	  dest = SUBREG_REG (dest);
	if (GET_CODE (dest) != REG)
	  mark_use (dest, reg_set_block);
	mark_use (SET_SRC (x), reg_set_block);
	break;
      }
    case CLOBBER:
      break;
    default:
      {
	char *fmt = GET_RTX_FORMAT (code);
	int i, j;
	for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
	  {
	    if (fmt[i] == 'e')
	      mark_use (XEXP (x, i), reg_set_block);
	    else if (fmt[i] == 'E')
	      for (j = XVECLEN (x, i) - 1; j >= 0; j--)
		mark_use (XVECEXP (x, i, j), reg_set_block);
	  }
	break;
      }
    }
}

int
remove_dead_before_cse ()
{
  rtx *reg_set_block, last, last_call, insn, set;
  int in_libcall = 0;

  /* This pass should run just once, after rtl generation.  */

  if (! sh_flag_remove_dead_before_cse
      || rtx_equal_function_value_matters
      || reload_completed)
    return;

  sh_flag_remove_dead_before_cse = 0;

  reg_set_block = (rtx *)alloca (max_reg_num () * sizeof (rtx));
  bzero ((char *)reg_set_block, max_reg_num () * sizeof (rtx));
  last_call = last = get_last_insn ();
  for (insn = last; insn; insn = PREV_INSN (insn))
    {
      if (GET_RTX_CLASS (GET_CODE (insn)) != 'i')
	continue;
      if (GET_CODE (insn) == JUMP_INSN)
	{
	  last_call = last = insn;
	  continue;
	}
      set = single_set (insn);

      /* Don't delete parts of libcalls, since that would confuse cse, loop
	 and flow.  */
      if (find_reg_note (insn, REG_RETVAL, NULL_RTX))
	in_libcall = 1;
      else if (in_libcall)
	{
	  if (find_reg_note (insn, REG_LIBCALL, NULL_RTX))
	    in_libcall = 0;
	}
      else if (set && GET_CODE (SET_DEST (set)) == REG)
	{
	  int regno = REGNO (SET_DEST (set));
	  rtx ref_insn = (regno < FIRST_PSEUDO_REGISTER && call_used_regs[regno]
			  ? last_call
			  : last);
	  if (reg_set_block[regno] == ref_insn
	      && (regno >= FIRST_PSEUDO_REGISTER
		  || HARD_REGNO_NREGS (regno, GET_MODE (SET_DEST (set))) == 1)
	      && (GET_CODE (insn) != CALL_INSN || CONST_CALL_P (insn)))
	    {
	      PUT_CODE (insn, NOTE);
	      NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED;
	      NOTE_SOURCE_FILE (insn) = 0;
	      continue;
	    }
	  else
	    reg_set_block[REGNO (SET_DEST (set))] = ref_insn;
	}
      if (GET_CODE (insn) == CALL_INSN)
	{
	  last_call = insn;
	  mark_use (CALL_INSN_FUNCTION_USAGE (insn), reg_set_block);
	}
      mark_use (PATTERN (insn), reg_set_block);
    }
  return 0;
}
