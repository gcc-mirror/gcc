/* Output routines for GCC for Hitachi Super-H.
   Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.

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

#define MSW (TARGET_LITTLE_ENDIAN ? 1 : 0)
#define LSW (TARGET_LITTLE_ENDIAN ? 0 : 1)

/* ??? The pragma interrupt support will not work for SH3.  */
/* This is set by #pragma interrupt and #pragma trapa, and causes gcc to
   output code for the next function appropriate for an interrupt handler.  */
int pragma_interrupt;

/* This is set by #pragma trapa, and is similar to the above, except that
   the compiler doesn't emit code to preserve all registers.  */
static int pragma_trapa;

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

/* Provides the class number of the smallest class containing
   reg number.  */

int regno_reg_class[FIRST_PSEUDO_REGISTER] =
{
  R0_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, PR_REGS, T_REGS, NO_REGS,
  MAC_REGS, MAC_REGS,
};

/* Provide reg_class from a letter such as appears in the machine
   description.  */

enum reg_class reg_class_from_letter[] =
{
  /* a */ NO_REGS, /* b */ NO_REGS, /* c */ NO_REGS, /* d */ NO_REGS,
  /* e */ NO_REGS, /* f */ NO_REGS, /* g */ NO_REGS, /* h */ NO_REGS,
  /* i */ NO_REGS, /* j */ NO_REGS, /* k */ NO_REGS, /* l */ PR_REGS,
  /* m */ NO_REGS, /* n */ NO_REGS, /* o */ NO_REGS, /* p */ NO_REGS,
  /* q */ NO_REGS, /* r */ NO_REGS, /* s */ NO_REGS, /* t */ T_REGS,
  /* u */ NO_REGS, /* v */ NO_REGS, /* w */ NO_REGS, /* x */ MAC_REGS,
  /* y */ NO_REGS, /* z */ R0_REGS
};

/* Print the operand address in x to the stream.  */

void
print_operand_address (stream, x)
     FILE *stream;
     rtx x;
{
  switch (GET_CODE (x))
    {
    case REG:
      fprintf (stream, "@%s", reg_names[REGNO (x)]);
      break;

    case PLUS:
      {
	rtx base = XEXP (x, 0);
	rtx index = XEXP (x, 1);

	switch (GET_CODE (index))
	  {
	  case CONST_INT:
	    fprintf (stream, "@(%d,%s)", INTVAL (index),
		     reg_names[REGNO (base)]);
	    break;

	  case REG:
	    fprintf (stream, "@(r0,%s)",
		     reg_names[MAX (REGNO (base), REGNO (index))]);
	    break;

	  default:
	    debug_rtx (x);
	    abort ();
	  }
      }
      break;

    case PRE_DEC:
      fprintf (stream, "@-%s", reg_names[REGNO (XEXP (x, 0))]);
      break;

    case POST_INC:
      fprintf (stream, "@%s+", reg_names[REGNO (XEXP (x, 0))]);
      break;

    default:
      output_addr_const (stream, x);
      break;
    }
}

/* Print operand x (an rtx) in assembler syntax to file stream
   according to modifier code.

   '.'  print a .s if insn needs delay slot
   '@'  print rte or rts depending upon pragma interruptness
   '#'  output a nop if there is nothing to put in the delay slot
   'O'  print a constant without the #
   'R'  print the LSW of a dp value - changes if in little endian
   'S'  print the MSW of a dp value - changes if in little endian
   'T'  print the next word of a dp value - same as 'R' in big endian mode.  */

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
	fprintf (stream, ".s");
      break;
    case '@':
      if (pragma_interrupt)
	fprintf (stream, "rte");
      else
	fprintf (stream, "rts");
      break;
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
	  print_operand_address (stream,
				 XEXP (adj_offsettable_operand (x, 4), 0));
	  break;
	}
      break;
    default:
      switch (GET_CODE (x))
	{
	case REG:
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
      emit_insn (gen_move_insn (r4, XEXP (operands[0], 0)));
      emit_insn (gen_move_insn (r5, XEXP (operands[1], 0)));
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
      emit_insn (gen_move_insn (r4, XEXP (operands[0], 0)));
      emit_insn (gen_move_insn (r5, XEXP (operands[1], 0)));

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
  /* Copy the source to a register if both operands aren't registers.  */
  if (! reload_in_progress && ! reload_completed
      && ! register_operand (operands[0], mode)
      && ! register_operand (operands[1], mode))
    operands[1] = copy_to_mode_reg (mode, operands[1]);

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
  if (code != EQ && code != NE
      && (sh_compare_op1 != const0_rtx
	  || code == GTU  || code == GEU || code == LTU || code == LEU))
    sh_compare_op1 = force_reg (mode, sh_compare_op1);

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
  if (code != EQ && code != NE)
    {
      /* Force args into regs, since we can't use constants here.  */
      sh_compare_op0 = force_reg (SImode, sh_compare_op0);
      if (sh_compare_op1 != const0_rtx
	  || code == GTU  || code == GEU || code == LTU || code == LEU)
	sh_compare_op1 = force_reg (SImode, sh_compare_op1);
    }
  operands[1] = sh_compare_op0;
  operands[2] = sh_compare_op1;
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

/* We can't tell if we need a register as a scratch for the jump
   until after branch shortening, and then it's too late to allocate a
   register the 'proper' way.  These instruction sequences are rare
   anyway, so to avoid always using a reg up from our limited set, we'll
   grab one when we need one on output.  */

/* ??? Should fix compiler so that using a clobber scratch in jump
   instructions works, and then this will be unnecessary.  */

char *
output_far_jump (insn, op)
     rtx insn;
     rtx op;
{
  rtx thislab = gen_label_rtx ();

  /* Output the delay slot insn first if any.  */
  if (dbr_sequence_length ())
    print_slot (final_sequence);

  output_asm_insn ("mov.l	r13,@-r15", 0);
  output_asm_insn ("mov.l	%O0,r13", &thislab);
  output_asm_insn ("jmp	@r13", 0);
  output_asm_insn ("mov.l	@r15+,r13", 0);
  output_asm_insn (".align	2", 0);
  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L", CODE_LABEL_NUMBER (thislab));
  output_asm_insn (".long	%O0", &op);
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
  int label = lf++;

  switch (get_attr_length (insn))
    {
    case 2:
      /* A branch with an unfilled delay slot.  */
    case 4:
      /* Simple branch in range -252..+258 bytes */
      return logic ? "bt%.	%l0" : "bf%.	%l0";

    case 6:
      /* A branch with an unfilled delay slot.  */
    case 8:
      /* Branch in range -4092..+4098 bytes.  */
      {
	/* The call to print_slot will clobber the operands.  */
	rtx op0 = operands[0];

	/* If the instruction in the delay slot is annulled (true), then
	   there is no delay slot where we can put it now.  The only safe
	   place for it is after the label.  */

	if (final_sequence)
	  {
	    fprintf (asm_out_file, "\tb%c%s\tLF%d\n", logic ? 'f' : 't',
		     INSN_ANNULLED_BRANCH_P (XVECEXP (final_sequence, 0, 0))
		     ? "" : ".s", label);
	    if (! INSN_ANNULLED_BRANCH_P (XVECEXP (final_sequence, 0, 0)))
	      print_slot (final_sequence);
	  }
	else
	  fprintf (asm_out_file, "\tb%c\tLF%d\n", logic ? 'f' : 't', label);

	output_asm_insn ("bra	%l0", &op0);
	fprintf (asm_out_file, "\tnop\n");
	fprintf (asm_out_file, "LF%d:\n", label);

	if (final_sequence
	    && INSN_ANNULLED_BRANCH_P (XVECEXP (final_sequence, 0, 0)))
	  print_slot (final_sequence);
      }
      return "";

    case 16:
      /* A branch with an unfilled delay slot.  */
    case 18:
      /* Branches a long way away.  */
      {
	/* The call to print_slot will clobber the operands.  */
	rtx op0 = operands[0];

	/* If the instruction in the delay slot is annulled (true), then
	   there is no delay slot where we can put it now.  The only safe
	   place for it is after the label.  */

	if (final_sequence)
	  {
	    fprintf (asm_out_file, "\tb%c%s\tLF%d\n", logic ? 'f' : 't',
		     INSN_ANNULLED_BRANCH_P (XVECEXP (final_sequence, 0, 0))
		     ? "" : ".s", label);
	    if (! INSN_ANNULLED_BRANCH_P (XVECEXP (final_sequence, 0, 0)))
	      print_slot (final_sequence);
	  }
	else
	  fprintf (asm_out_file, "\tb%c\tLF%d\n", logic ? 'f' : 't', label);

	output_far_jump (insn, op0);
	fprintf (asm_out_file, "LF%d:\n", label);

	if (final_sequence
	    && INSN_ANNULLED_BRANCH_P (XVECEXP (final_sequence, 0, 0)))
	  print_slot (final_sequence);
      }
      return "";
    }
  return "bad";
}

/* A copy of the option structure defined in toplev.c.  */

struct option
{
  char *string;
  int *variable;
  int on_value;
};

/* Output a single output option string NAME to FILE, without generating
   lines longer than MAX.  */

static int
output_option (file, sep, type, name, indent, pos, max)
     FILE *file;
     char *sep;
     char *type;
     char *name;
     char *indent;
     int pos;
     int max;
{
  if (strlen (sep) + strlen (type) + strlen (name) + pos > max)
    {
      fprintf (file, indent);
      return fprintf (file, "%s%s", type, name);
    }
  return pos + fprintf (file, "%s%s%s", sep, type, name);
}

/* A copy of the target_switches variable in toplev.c.  */

static struct
{
  char *name;
  int value;
} m_options[] = TARGET_SWITCHES;

/* Output all options to the assembly language file.  */

static void
output_options (file, f_options, f_len, W_options, W_len,
		pos, max, sep, indent, term)
     FILE *file;
     struct option *f_options;
     struct option *W_options;
     int f_len, W_len;
     int pos;
     int max;
     char *sep;
     char *indent;
     char *term;
{
  register int j;

  if (optimize)
    pos = output_option (file, sep, "-O", "", indent, pos, max);
  if (write_symbols != NO_DEBUG)
    pos = output_option (file, sep, "-g", "", indent, pos, max);
  if (profile_flag)
    pos = output_option (file, sep, "-p", "", indent, pos, max);
  if (profile_block_flag)
    pos = output_option (file, sep, "-a", "", indent, pos, max);

  for (j = 0; j < f_len; j++)
    if (*f_options[j].variable == f_options[j].on_value)
      pos = output_option (file, sep, "-f", f_options[j].string,
			   indent, pos, max);

  for (j = 0; j < W_len; j++)
    if (*W_options[j].variable == W_options[j].on_value)
      pos = output_option (file, sep, "-W", W_options[j].string,
			   indent, pos, max);

  for (j = 0; j < sizeof m_options / sizeof m_options[0]; j++)
    if (m_options[j].name[0] != '\0'
	&& m_options[j].value > 0
	&& ((m_options[j].value & target_flags)
	    == m_options[j].value))
      pos = output_option (file, sep, "-m", m_options[j].name,
			   indent, pos, max);

  fprintf (file, term);
}

/* Output to FILE the start of the assembler file.  */

void
output_file_start (file, f_options, f_len, W_options, W_len)
     FILE *file;
     struct option *f_options;
     struct option *W_options;
     int f_len, W_len;
{
  register int pos;

  output_file_directive (file, main_input_filename);

  /* Switch to the data section so that the coffsem symbol and the
     gcc2_compiled. symbol aren't in the text section.  */
  data_section ();

  pos = fprintf (file, "\n! Hitachi SH cc1 (%s) arguments:", version_string);
  output_options (file, f_options, f_len, W_options, W_len,
		  pos, 75, " ", "\n! ", "\n\n");

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
    {
      if (TARGET_SH3)
	return 2;
      /* If not an sh3 then we don't even have an instruction for it.  */
      return 20;
    }

  /* Otherwise, return the true cost in instructions.  */
  if (GET_CODE (x) == ASHIFTRT)
    return ashiftrt_insns[value];
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

/* Output RTL to split a constant shift into its component SH constant
   shift instructions.  */
   
/* ??? For SH3, should reject constant shifts when slower than loading the
   shift count into a register?  */

int
gen_shifty_op (code, operands)
     int code;
     rtx *operands;
{
  int value = INTVAL (operands[2]);
  int max, i;

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

  max = shift_insns[value];
  for (i = 0; i < max; i++)
    gen_ashift (code, shift_amounts[value][i], operands[0]);
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

  if (TARGET_SH3 && GET_CODE (operands[2]) != CONST_INT)
    {
      rtx count = copy_to_mode_reg (SImode, operands[2]);
      emit_insn (gen_negsi2 (count, count));
      emit_insn (gen_ashrsi3_d (operands[0], operands[1], count));
      return 1;
    }
  if (GET_CODE (operands[2]) != CONST_INT)
    return 0;

  value = INTVAL (operands[2]);

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
add_constant (x, mode)
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
	    return pool_vector[i].label;
	}
    }

  /* Need a new one.  */
  pool_vector[pool_size].value = x;
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
	  if (need_align)
	    {
	      need_align = 0;
	      scan = emit_label_after (gen_label_rtx (), scan);
	      scan = emit_insn_after (gen_align_4 (), scan);
	    }
	  scan = emit_label_after (p->label, scan);
	  scan = emit_insn_after (gen_consttable_4 (p->value), scan);
	  break;
	case DImode:
	  if (need_align)
	    {
	      need_align = 0;
	      scan = emit_label_after (gen_label_rtx (), scan);
	      scan = emit_insn_after (gen_align_4 (), scan);
	    }
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
  if (GET_CODE (insn) == INSN
      && GET_CODE (PATTERN (insn)) == SET
      /* We can load any 8 bit value if we don't care what the high
	 order bits end up as.  */
      && GET_MODE (SET_DEST (PATTERN (insn))) != QImode
      && CONSTANT_P (SET_SRC (PATTERN (insn)))
      && (GET_CODE (SET_SRC (PATTERN (insn))) != CONST_INT
	  || ! CONST_OK_FOR_I (INTVAL (SET_SRC (PATTERN (insn))))))
    return 1;

  return 0;
}

/* Find the last barrier from insn FROM which is close enough to hold the
   constant pool.  If we can't find one, then create one near the end of
   the range.  */

/* ??? It would be good to put constant pool tables between a case jump and
   the jump table.  This fails for two reasons.  First, there is no
   barrier after the case jump.  This is a bug in the casesi pattern.
   Second, inserting the table here may break the mova instruction that
   loads the jump table address, by moving the jump table too far away.
   We fix that problem by never outputting the constant pool between a mova
   and its label.  */

static rtx
find_barrier (from)
     rtx from;
{
  int count_si = 0;
  int count_hi = 0;
  int found_hi = 0;
  int found_si = 0;
  rtx found_barrier = 0;
  rtx found_mova = 0;

  /* For HImode: range is 510, add 4 because pc counts from address of
     second instruction after this one, subtract 2 for the jump instruction
     that we may need to emit before the table.  This gives 512.
     For SImode: range is 1020, add 4 because pc counts from address of
     second instruction after this one, subtract 2 in case pc is 2 byte
     aligned, subtract 2 for the jump instruction that we may need to emit
     before the table.  This gives 1020.  */
  while (from && count_si < 1020 && count_hi < 512)
    {
      int inc = get_attr_length (from);

      if (GET_CODE (from) == BARRIER)
	found_barrier = from;

      if (broken_move (from))
	{
	  rtx src = SET_SRC (PATTERN (from));

	  if (hi_const (src))
	    {
	      found_hi = 1;
	      /* We put the short constants before the long constants, so
		 we must count the length of short constants in the range
		 for the long constants.  */
	      /* ??? This isn't optimal, but is easy to do.  */
	      if (found_si)
		count_si += 2;
	    }
	  else
	    found_si = 1;
	}

      if (GET_CODE (from) == INSN
	  && GET_CODE (PATTERN (from)) == SET
	  && GET_CODE (SET_SRC (PATTERN (from))) == UNSPEC
	  && XINT (SET_SRC (PATTERN (from)), 1) == 1)
	found_mova = from;
      else if (GET_CODE (from) == JUMP_INSN
	       && (GET_CODE (PATTERN (from)) == ADDR_VEC
		   || GET_CODE (PATTERN (from)) == ADDR_DIFF_VEC))
	found_mova = 0;

      if (found_si)
	count_si += inc;
      if (found_hi)
	count_hi += inc;
      from = NEXT_INSN (from);
    }

  /* Insert the constant pool table before the mova instruction, to prevent
     the mova label reference from going out of range.  */
  if (found_mova)
    from = found_mova;

  if (! found_barrier)
    {
      /* We didn't find a barrier in time to dump our stuff,
	 so we'll make one.  */
      rtx label = gen_label_rtx ();

      /* If we exceeded the range, then we must back up over the last
	 instruction we looked at.  Otherwise, we just need to undo the
	 NEXT_INSN at the end of the loop.  */
      if (count_hi > 512 || count_si > 1020)
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

/* Exported to toplev.c.

   Scan the function looking for move instructions which have to be changed to
   pc-relative loads and insert the literal tables.  */

void
machine_dependent_reorg (first)
     rtx first;
{
  rtx insn;

  for (insn = first; insn; insn = NEXT_INSN (insn))
    {
      if (broken_move (insn))
	{
	  rtx scan;
	  /* Scan ahead looking for a barrier to stick the constant table
	     behind.  */
	  rtx barrier = find_barrier (insn);

	  /* Now find all the moves between the points and modify them.  */
	  for (scan = insn; scan != barrier; scan = NEXT_INSN (scan))
	    {
	      if (broken_move (scan))
		{
		  rtx pat = PATTERN (scan);
		  rtx src = SET_SRC (pat);
		  rtx dst = SET_DEST (pat);
		  enum machine_mode mode = GET_MODE (dst);
		  rtx lab;
		  rtx newinsn;
		  rtx newsrc;

		  if (mode == SImode && hi_const (src))
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

		  lab = add_constant (src, mode);
		  newsrc = gen_rtx (MEM, mode,
				    gen_rtx (LABEL_REF, VOIDmode, lab));
		  RTX_UNCHANGING_P (newsrc) = 1;
		  newinsn = emit_insn_after (gen_rtx (SET, VOIDmode,
						      dst, newsrc), scan);

		  delete_insn (scan);
		  scan = newinsn;
		}
	    }
	  dump_table (barrier);
	}
    }
}

/* Dump out instruction addresses, which is useful for debugging the
   constant pool table stuff.  */

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
}

/* Dump out any constants accumulated in the final pass.  These will
   will only be labels.  */

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

/* Adjust the stack and return the number of bytes taken to do it.  */

static void
output_stack_adjust (size, reg)
     int size;
     rtx reg;
{
  if (size)
    {
      rtx val = GEN_INT (size);
      rtx insn;

      if (! CONST_OK_FOR_I (size))
	{
	  rtx reg = gen_rtx (REG, SImode, 3);
	  emit_insn (gen_movsi (reg, val));
	  val = reg;
	}

      insn = gen_addsi3 (reg, reg, val);
      emit_insn (insn);
    }
}

/* Output RTL to push register RN onto the stack.  */

static void
push (rn)
     int rn;
{
  rtx x;
  x = emit_insn (gen_push (gen_rtx (REG, SImode, rn)));
  REG_NOTES (x) = gen_rtx (EXPR_LIST, REG_INC,
			   gen_rtx(REG, SImode, STACK_POINTER_REGNUM), 0);
}

/* Output RTL to pop register RN from the stack.  */

static void
pop (rn)
     int rn;
{
  rtx x;
  x = emit_insn (gen_pop (gen_rtx (REG, SImode, rn)));
  REG_NOTES (x) = gen_rtx (EXPR_LIST, REG_INC,
			   gen_rtx(REG, SImode, STACK_POINTER_REGNUM), 0);
}

/* Generate code to push the regs specified in the mask, and return
   the number of bytes the insns take.  */

static void
push_regs (mask)
     int mask;
{
  int i;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (mask & (1 << i))
      push (i);
}

/* Work out the registers which need to be saved, both as a mask and a
   count.

   If doing a pragma interrupt function, then push all regs used by the
   function, and if we call another function (we can tell by looking at PR),
   make sure that all the regs it clobbers are safe too.  */

static int
calc_live_regs (count_ptr)
     int *count_ptr;
{
  int reg;
  int live_regs_mask = 0;
  int count = 0;

  for (reg = 0; reg < FIRST_PSEUDO_REGISTER; reg++)
    {
      if (pragma_interrupt && ! pragma_trapa)
	{
	  /* Need to save all the regs ever live.  */
	  if ((regs_ever_live[reg]
	       || (call_used_regs[reg] && regs_ever_live[PR_REG]))
	      && reg != STACK_POINTER_REGNUM && reg != ARG_POINTER_REGNUM
	      && reg != T_REG && reg != GBR_REG)
	    {
	      live_regs_mask |= 1 << reg;
	      count++;
	    }
	}
      else
	{
	  /* Only push those regs which are used and need to be saved.  */
	  if (regs_ever_live[reg] && ! call_used_regs[reg])
	    {
	      live_regs_mask |= (1 << reg);
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
  live_regs_mask = calc_live_regs (&d);

  /* We have pretend args if we had an object sent partially in registers
     and partially on the stack, e.g. a large structure.  */
  output_stack_adjust (-current_function_pretend_args_size, stack_pointer_rtx);

  extra_push = 0;

  /* This is set by SETUP_VARARGS to indicate that this is a varargs
     routine.  Clear it here so that the next function isn't affected.  */
  if (current_function_anonymous_args)
    {
      current_function_anonymous_args = 0;

      /* Push arg regs as if they'd been provided by caller in stack.  */
      for (i = 0; i < NPARM_REGS; i++)
	{
	  int rn = NPARM_REGS + FIRST_PARM_REG - i - 1;
	  if (i > (NPARM_REGS - current_function_args_info
		   - current_function_varargs))
	    break;
	  push (rn);
	  extra_push += 4;
	}
    }
  push_regs (live_regs_mask);
  output_stack_adjust (-get_frame_size (), stack_pointer_rtx);

  if (frame_pointer_needed)
    emit_insn (gen_movsi (frame_pointer_rtx, stack_pointer_rtx));
}

void
sh_expand_epilogue ()
{
  int live_regs_mask;
  int d, i;

  live_regs_mask = calc_live_regs (&d);

  if (frame_pointer_needed)
    {
      /* We deliberately make the add dependent on the frame_pointer,
	 to ensure that instruction scheduling won't move the stack pointer
	 adjust before instructions reading from the frame.  This can fail
	 if there is an interrupt which then writes to the stack.  */
      output_stack_adjust (get_frame_size (), frame_pointer_rtx);
      emit_insn (gen_movsi (stack_pointer_rtx, frame_pointer_rtx));
    }
  else
    output_stack_adjust (get_frame_size (), stack_pointer_rtx);

  /* Pop all the registers.  */

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      int j = (FIRST_PSEUDO_REGISTER - 1) - i;
      if (live_regs_mask & (1 << j))
	pop (j);
    }

  output_stack_adjust (extra_push + current_function_pretend_args_size,
		       stack_pointer_rtx);
}

/* Clear variables at function end.  */

void
function_epilogue (stream, size)
     FILE *stream;
     int size;
{
  pragma_interrupt = pragma_trapa = 0;
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

  calc_live_regs (&regs_saved);
  total_saved_regs_space = (regs_saved) * 4;

  if (from == ARG_POINTER_REGNUM && to == FRAME_POINTER_REGNUM)
    return total_saved_regs_space + total_auto_space;

  if (from == ARG_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    return total_saved_regs_space + total_auto_space;

  /* Initial gap between fp and sp is 0.  */
  if (from == FRAME_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    return 0;

  abort ();
}

/* Handle machine specific pragmas to be semi-compatible with Hitachi
   compiler.  */

int
handle_pragma (file)
     FILE *file;
{
  int c;
  char pbuf[200];
  int psize = 0;

  c = getc (file);
  while (c == ' ' || c == '\t')
    c = getc (file);

  if (c == '\n' || c == EOF)
    return c;

  while (psize < sizeof (pbuf) - 1 && c != '\n')
    {
      pbuf[psize++] = c;
      if (psize == 9 && strncmp (pbuf, "interrupt", 9) == 0)
	{
	  pragma_interrupt = 1;
	  return ' ';
	}
      if (psize == 5 && strncmp (pbuf, "trapa", 5) == 0)
	{
	  pragma_interrupt = pragma_trapa = 1;
	  return ' ';
	}
      c = getc (file);
    }
  return c;
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
      if (GET_CODE (op) == REG)
	return (REGNO (op) != T_REG
		&& REGNO (op) != PR_REG
		&& REGNO (op) != MACH_REG
		&& REGNO (op) != MACL_REG);
      return 1;
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

/* Determine where to put an argument to a function.
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

rtx
sh_function_arg (cum, mode, type, named)
     CUMULATIVE_ARGS cum;
     enum machine_mode mode;
     tree type;
     int named;
{
  if (named)
    {
      int rr = (ROUND_REG (cum, mode));

      if (rr < NPARM_REGS)
	return ((type == 0 || ! TREE_ADDRESSABLE (type))
		? gen_rtx (REG, mode, FIRST_PARM_REG + rr) : 0);
    }
  return 0;
}

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero.
   Any arg that starts in the first 4 regs but won't entirely fit in them
   needs partial registers on the SH.  */

int
sh_function_arg_partial_nregs (cum, mode, type, named)
     CUMULATIVE_ARGS cum;
     enum machine_mode mode;
     tree type;
     int named;
{
  if (cum < NPARM_REGS)
    {
      if ((type == 0 || ! TREE_ADDRESSABLE (type))
	  && (cum + (mode == BLKmode
		     ? ROUND_ADVANCE (int_size_in_bytes (type))
		     : ROUND_ADVANCE (GET_MODE_SIZE (mode))) - NPARM_REGS > 0))
	return NPARM_REGS - cum;
    }
  return 0;
}

/* Return non-zero if REG is not used after INSN.
   We assume REG is a reload reg, and therefore does
   not live past labels or calls or jumps.  */
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

      /* If this is a sequence, we must handle them all at once.
	 We could have for instance a call that sets the target register,
	 and a insn in a delay slot that uses the register.  In this case,
	 we must return 0.  */
      if (code == INSN && GET_CODE (PATTERN (insn)) == SEQUENCE)
	{
	  int i;
	  int retval = 0;

	  for (i = 0; i < XVECLEN (PATTERN (insn), 0); i++)
	    {
	      rtx this_insn = XVECEXP (PATTERN (insn), 0, i);
	      rtx set = single_set (this_insn);

	      if (GET_CODE (this_insn) == CALL_INSN)
		code = CALL_INSN;

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
