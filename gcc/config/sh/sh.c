/* Output routines for GCC for Hitachi Super-H
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.

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
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* Contributed by Steve Chamberlain (sac@cygnus.com) */

#include <stdio.h>
#include "assert.h"
#include "config.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "tree.h"
#include "output.h"

#include "insn-attr.h"
#include "flags.h"
#include "obstack.h"
#include "expr.h"

static rtx add_constant ();

int pragma_interrupt;
int pragma_trapa;

int current_function_anonymous_args;
extern int current_function_pretend_args_size;
extern char *version_string;
extern int flag_traditional;

static rtx shiftsyms[32];
struct rtx_def *table_lab;
enum attr_cpu sh_cpu;		/* target cpu */

/* Global variables for machine-dependent things. */

/* Saved operands from the last compare to use when we generate an scc
   or bcc insn. */

rtx sh_compare_op0;
rtx sh_compare_op1;

/* Provides the class number of the smallest class containing
   reg number */

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
   description. */

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

/* Value is 1 if register/mode pair is acceptable on SH.  Even
   registers can hold DIs and DF values. The rest can only hold
   SI's efficiently  */


#define REG_ODD \
 (  (1 << (int) QImode)  | (1 << (int) HImode) | (1 << (int) SImode)	\
  | (1 << (int) QFmode)  | (1 << (int) HFmode) | (1 << (int) SFmode)	\
  | (1 << (int) CQImode) | (1 << (int) CHImode)| (1<< (int)DFmode) | (1<<(int)DImode))

#define REG_EVEN \
  (REG_ODD | (1 << (int) CSImode) | (1 << (int) SCmode))

#define SI_ONLY (1<<(int)SImode)

int hard_regno_mode_ok[] =
{
  REG_EVEN, REG_ODD, REG_EVEN, REG_ODD,
  REG_EVEN, REG_ODD, REG_EVEN, REG_ODD,
  REG_EVEN, REG_ODD, REG_EVEN, REG_ODD,
  REG_EVEN, REG_ODD, REG_EVEN, REG_ODD,
  REG, 0, SI_ONLY, SI_ONLY,
  SI_ONLY, SI_ONLY
};

/* Local label counter, used for constants in the pool and inside
   pattern branches.  */
static int lf = 100;


/* Number of bytes pushed for anonymous args, used to pass information
   between expand_prologue and expand_epilogue. */
static int extra_push;



void
push (rn)
     int rn;
{
  rtx x ;
  x=  emit_insn (gen_push (gen_rtx (REG, SImode, rn)));
  REG_NOTES (x) = gen_rtx (EXPR_LIST, REG_INC, 
			   gen_rtx(REG, SImode, STACK_POINTER_REGNUM), 0);
}

void
pop (rn)
     int rn;
{
  rtx x;
  x =  emit_insn (gen_pop (gen_rtx (REG, SImode, rn)));
  REG_NOTES (x) = gen_rtx (EXPR_LIST, REG_INC, 
			   gen_rtx(REG, SImode, STACK_POINTER_REGNUM), 0);
}


/* Adjust the stack and return the number of bytes taken to do it */
static rtx lastreg;
int lastval;
static void
output_stack_adjust (size)
     int size;
{
  if (size)
    {
      rtx val = GEN_INT (size);
      rtx insn;

      if (!CONST_OK_FOR_I (size))
	{
	  lastreg = gen_rtx (REG, SImode, 3);
	  lastval = size;
	  emit_insn (gen_movsi (lastreg, val));
	  val = lastreg;

	}

      insn = gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx, val);
      emit_insn (insn);
    }
}


/* Generate code to push the regs specified in the mask, and return
   the number of bytes the insns take. */

static void
push_regs (mask)
     int mask;
{
  int i;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (mask & (1 << i))
	{
	  push (i);
	}
    }
}


/* Print an instruction which would have gone into a delay slot after
   an instructiuon, but couldn't because the instruction expanded into a
   sequence where putting the slot insn at the end wouldn't work. */

static void
print_slot (insn)
     rtx insn;
{
  final_scan_insn (XVECEXP (insn, 0, 1), asm_out_file, optimize, 0, 1);

  INSN_DELETED_P (XVECEXP (insn, 0, 1)) = 1;
}


/* Work out the registers which need to be saved, both as a mask and a
   count.

   If doing a pragma interrupt function, then push all regs used by the function,
   and if we call another function (we can tell by looking at PR), make sure that all the
   regs it clobbers are safe too.
 */
static int
calc_live_regs (count_ptr)
     int *count_ptr;
{
  int reg;
  int live_regs_mask = 0;
  int count = 0;
  for (reg = 0; reg < FIRST_PSEUDO_REGISTER; reg++)
    {
      if (reg == ARG_POINTER_REGNUM)
	continue;
      if (reg == T_REG)
	continue;
      if (reg == GBR_REG)
	continue;

      if (pragma_interrupt && !pragma_trapa)
	{
	  /* Need to save all the regs ever live */
	  if ((regs_ever_live[reg]
	       || (call_used_regs[reg] && regs_ever_live[PR_REG]))
	      && reg != 15)
	    {
	      live_regs_mask |= 1 << reg;
	      count++;
	    }
	}
      else if (TARGET_SMALLCALL)
	{
	  /* Don't need to push anthing, but count the regs which have
	     been pushed by the wrapper */
	  if (call_used_regs[reg])
	    count++;
	}
      else
	{
	  /* Only push those regs which are used and need to be saved */
	  if (regs_ever_live[reg] && !call_used_regs[reg])
	    {
	      count++;
	      live_regs_mask |= (1 << reg);
	    }
	}
    }


  *count_ptr = count;
  return live_regs_mask;
}


static int
need_slot (insn)
     rtx insn;
{
  return (insn && !INSN_ANNULLED_BRANCH_P (XVECEXP (insn, 0, 0)));
}

/* Print the operand address in x to the stream */

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

	if (GET_CODE (base) != REG)
	  {
	    /* Ensure that BASE is a register (one of them must be). */
	    rtx temp = base;
	    base = index;
	    index = temp;
	  }

	switch (GET_CODE (index))
	  {
	  case CONST_INT:
	    fprintf (stream, "@(%d,%s)",
		     INTVAL (index),
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
   '*'  print a local label
   '^'  increment the local label number
   '!'  dump the constant table
   '#'  output a nop if there is nothing to put in the delay slot
   '@'  print rte or rts depending upon pragma interruptness
   'R'  print the next register or memory location along, ie the lsw in
   a double word value
   'O'  print a constant without the #
   'M'  print a constant as its negative
   'N'  print insides of a @++ or @-- o */

void
print_operand (stream, x, code)
     FILE *stream;
     rtx x;
     int code;
{
  switch (code)
    {
    case '.':
      if (need_slot (final_sequence))
	fprintf (stream, ".s");
      break;
    case '*':
      fprintf (stream, "LF%d", lf);
      break;
    case '^':
      lf++;
      break;
    case '@':
      if (pragma_interrupt)
	fprintf (stream, "rte");
      else
	fprintf (stream, "rts");
      break;
    case '#':
      /* Output a nop if there's nothing in the delay slot */
      if (dbr_sequence_length () == 0)
	{
	  fprintf (stream, "\n\tnop");
	}
      break;
    case 'O':
      output_addr_const (stream, x);
      break;
    case 'M':
      fprintf (asm_out_file, "#%d", -INTVAL (x));
      break;
    case 'N':
      fputs (reg_names[REGNO (XEXP (XEXP (x, 0), 0))], (stream));
      break;
    case 'R':
      /* Next location along in memory or register */
      switch (GET_CODE (x))
	{
	case REG:
	  fputs (reg_names[REGNO (x) + 1], (stream));
	  break;
	case MEM:
	  print_operand_address (stream, XEXP (adj_offsettable_operand (x, 4), 0));
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


static int
sextb (x)
  int x;
{
  x &= 0xff;
  if (x > 127)
    {
      x = -256 + x;
    }
  return x;
}



/* Take a move with integer constant source in OPERANDS, see if it can be generated by
   devious shifting.  If so, generate the instruction sequence and return 1, otherwise
   return 0.

    OPERANDS[0] Destination register
    OPERANDS[1] Source constant

   00000000 00000000 00000000 0NNNNNNNN simple load
   00000000 00000000 00000000 NNNNNNNN0 load and shift by 1
   00000000 00000000 0000000N NNNNNNN00 load and shift by 2
   00000000 00000000 0NNNNNNN 000000000 load and shift by 8
   00000000 0NNNNNNN 00000000 000000000 load and shift by 16
   N0000000 00000000 00000000 00NNNNNNN load and rotate right

   11111111 11111111 11111111 1NNNNNNNN simple load
   11111111 11111111 11111111 NNNNNNNN0 load and shift by 1
   11111111 11111111 1111111N NNNNNNN00 load and shift by 2
   11111111 11111111 1NNNNNNN 000000000 load and shift by 8
   11111111 1NNNNNNN 00000000 000000000 load and shift by 16
   N1111111 11111111 11111111 11NNNNNNN load and rotate right

   00000000 00000000 00000000 1NNNNNNNN load and zero extend byte
   00000000 00000000 11111111 1NNNNNNNN load and zero extend word


*/

static int
synth_constant (operands, mode)
     rtx operands[];
     enum machine_mode mode;
{
  rtx dst;
  int i = INTVAL (operands[1]) & 0xffffffff;
    
  if (CONST_OK_FOR_I (i))
    return 0;

  if (TARGET_CLEN0 && mode != QImode)
    return 0;

  if (mode != SImode)
    {
      if (reload_in_progress)
	return 0;
      dst = gen_reg_rtx (SImode);
    }
  else
    {
      dst = operands[0];
    }


  /*  00000000 00000000 11111111 1NNNNNNNN load and zero extend word      */
  if ((i & 0xffffff80) == 0x0000ff80)
    {
      emit_move_insn (dst, GEN_INT (sextb (i)));
      emit_insn (gen_and_ffff (dst, dst));
    }
  /*    00000000 00000000 00000000 1NNNNNNNN load and zero extend byte */
  else if ((i & 0xffffff80) == 0x00000080)
    {
      emit_move_insn (dst, GEN_INT (sextb (i)));
      emit_insn (gen_and_ff (dst, dst));
    }
  /*   00000000 00000000 00000000 NNNNNNNN0 load and shift by 1
       11111111 11111111 11111111 NNNNNNNN0 load and shift by 1 */
  else if ((i & 0xffffff01) == 0
	   || (i & 0xffffff01) == 0xffffff00)
    {
      emit_move_insn (dst, GEN_INT (sextb (i >> 1)));
      emit_insn (gen_ashlsi3_n (dst, dst, GEN_INT (1)));
    }
  /*   00000000 00000000 0000000N NNNNNNN00 load and shift by 2
       11111111 11111111 1111111N NNNNNNN00 load and shift by 2*/
  else if ((i & 0xfffffe03) == 0
	   || (i & 0xfffffe03) == 0xfffffe00)
    {
      emit_move_insn (dst, GEN_INT (sextb (i >> 2)));
      emit_insn (gen_ashlsi3_n (dst, dst, GEN_INT (2)));
    }
  /*   00000000 00000000 0NNNNNNN 000000000 load and shift by 8
       11111111 11111111 1NNNNNNN 000000000 load and shift by 8 */

  else if ((i & 0xffff80ff) == 0
	   || (i & 0xffff80ff) == 0xffff8000)
    {
      emit_move_insn (dst, GEN_INT (sextb (i >> 8)));
      emit_insn (gen_ashlsi3_n (dst, dst, GEN_INT (8)));
    }
  /*     00000000 0NNNNNNN 00000000 000000000 load and shift by 16
	 11111111 1NNNNNNN 00000000 000000000 load and shift by 16 */
  else if ((i & 0xff80ffff) == 0x00000000
	   || (i & 0xff80ffff) == 0xff800000)
    {
      emit_move_insn (dst, GEN_INT (sextb (i >> 16)));
      emit_insn (gen_ashlsi3_n (dst, dst, GEN_INT (16)));
    }
  /*   00000000 00000000 0NNNNNNN 0NNNNNNNN load shift 8 and add */
  else if ((i & 0xffff8080) == 0 && TARGET_CLEN3)
    {
      emit_move_insn (dst, GEN_INT (sextb (i >> 8)));
      emit_insn (gen_ashlsi3_n (dst, dst, GEN_INT (8)));
      emit_insn (gen_addsi3 (dst, dst, GEN_INT (i & 0x7f)));
    }
  else
    return 0;

  if (mode == DImode)
    {
      /* Moving from SI to DI, we've got to zero out the high part */

      emit_insn (gen_rtx (SET, VOIDmode, 
			  gen_rtx (SUBREG, SImode, operands[0], 0),
			  dst));
      emit_insn (gen_rtx (SET, VOIDmode,
			  gen_rtx (SUBREG, SImode, operands[0], 1),
			  const0_rtx));

    }
  else if (mode != SImode)
    {
      emit_insn (gen_rtx (SET, VOIDmode, operands[0],
			  gen_rtx (SUBREG, mode, dst, 0)));

    }
  return 1;
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
  enum machine_mode mode;

  /* IF odd then fail */
  if (!constp || bytes <= 0)
    return 0;

  /* Don't expand if we'd make the code bigger and we don't want big code */

  if (bytes > 8 && TARGET_SMALLCODE)
    return 0;

  switch (align)
    {
    case 1:
      mode = QImode;
      break;
    case 2:
      mode = HImode;
      break;
    default:
      mode = SImode;
      align = 4;
    }

  if (mode == SImode && constp && bytes < 64 && (bytes % 4 == 0))
    {
      char entry[30];
      tree entry_name;
      rtx func_addr_rtx;
      rtx r4 = gen_rtx (REG, SImode, 4);
      rtx r5 = gen_rtx (REG, SImode, 5);
      sprintf (entry, "__movstr%s%d", GET_MODE_NAME (mode), bytes);
      entry_name = get_identifier (entry);

      func_addr_rtx = copy_to_mode_reg (Pmode,
	      gen_rtx (SYMBOL_REF, Pmode, IDENTIFIER_POINTER (entry_name)));
      emit_insn (gen_move_insn (r4, XEXP (operands[0], 0)));
      emit_insn (gen_move_insn (r5, XEXP (operands[1], 0)));
      emit_insn (gen_block_move_real (func_addr_rtx));
      return 1;
    }
  if (mode == SImode && constp && (bytes % 4 == 0))
    {
      tree entry_name;
      rtx func_addr_rtx;
      rtx r4 = gen_rtx (REG, SImode, 4);
      rtx r5 = gen_rtx (REG, SImode, 5);
      rtx r6 = gen_rtx (REG, SImode, 6);
      entry_name = get_identifier ("__movstr");

      func_addr_rtx = copy_to_mode_reg (Pmode,
					gen_rtx (SYMBOL_REF, Pmode,
					  IDENTIFIER_POINTER (entry_name)));
      emit_insn (gen_move_insn (r4, XEXP (operands[0], 0)));
      emit_insn (gen_move_insn (r5, XEXP (operands[1], 0)));

      /* r6 controls the size of the move, 16 is decremented from it
	 for each 64 bytes moved, then the -ve bit is used as an index into a
	 list of move instructions like this:
	
	 {
	 do {
	 *dst++ = *src++;
	 *dst++ = *src++;
	 *dst++ = *src++;
	 ..etc.. 16 in all
	 *dst++ = *src++;
	 *dst++ = *src++;
	 size -= 16;
	 } while (size > 0);
	
	 switch (size)
	 {
	 case -15:
	 *dst++ = *src++;
	 case -14:
	 *dst++ = *src++;
	 .. etc.. ;
	 case -2:
	 *dst++ = *src++;
	 case -1:
	 *dst++ = *src++;
	 case 0:
	 ;
	 }
	 }
	
	 eg, a 72 byte move would be set up with size(r6) = 14, for one
	 iteration through the big while loop, and a switch of -2 for the last part  */

      {
	int final_switch = 16 - ((bytes / 4) % 16);
	int while_loop = ((bytes / 4) / 16 - 1) * 16;
	emit_insn (gen_move_insn (r6, GEN_INT (while_loop + final_switch)));
	emit_insn (gen_block_lump_real (func_addr_rtx));
	return 1;
      }
    }

  return 0;
}

/* Prepare operands for a move define_expand; specifically, one of the
   operands must be in a register.  Take this chance to remove
   addressing modes which can't be coped with very well. */

int
prepare_move_operands (operands, mode)
     rtx operands[];
     enum machine_mode mode;
{
  if (!(reload_in_progress || reload_completed)
      && ((!register_operand (operands[0], mode)
	   && !register_operand (operands[1], mode))
	  || GET_CODE (operands[1]) == PLUS))
    {
      /* copy the source to a register */
      operands[1] = copy_to_mode_reg (mode, operands[1]);
    }
  if ((mode == SImode || mode == HImode || mode == QImode)
      && GET_CODE (operands[1]) == CONST_INT)
    {
      return synth_constant (operands, mode);
    }
  if (mode == DFmode || mode == DImode)
    {
      rtx src = operands[1];
      rtx dst = operands[0];
      rtx insns;

      if (src == dst)
	{
	  emit_insn (gen_rtx (SET, VOIDmode, dst, src));
	  return 1;
	}

      if (GET_CODE (src) == REG &&
	  REGNO (src) >= FIRST_PSEUDO_REGISTER)
	return 0;

      if (GET_CODE (dst) == REG &&
	  REGNO (dst) >= FIRST_PSEUDO_REGISTER)
	return 0;

      if (push_operand (dst, mode))
	return 0;

      if (GET_CODE (src) == CONST_DOUBLE)
	src = force_const_mem (DFmode, src);

      if (reload_in_progress)
	{
	  if (!(offsettable_memref_p (src) || register_operand (src, mode)))
	    return 0;
	  if (!(offsettable_memref_p (dst) || register_operand (dst,
								mode)))
	    return 0;
	}
      start_sequence ();
      if (GET_CODE (operands[0]) != REG
	  || !refers_to_regno_p (REGNO (operands[0]), REGNO (operands[0]) + 1, operands[1], 0))
	{
	  emit_move_insn (operand_subword (dst, 0, 1, mode),
			  operand_subword_force (src, 0, mode));
	  emit_move_insn (operand_subword (dst, 1, 1, mode),
			  operand_subword_force (src, 1, mode));
	}
      else
	{
	  emit_move_insn (operand_subword (dst, 1, 1, mode),
			  operand_subword_force (src, 1, mode));
	  emit_move_insn (operand_subword (dst, 0, 1, mode),
			  operand_subword_force (src, 0, mode));
	}

      insns = get_insns ();
      end_sequence ();

      emit_no_conflict_block (insns, dst, src, 0, src);
      return 1;
    }

  return 0;
}

/* Prepare the operands for an scc instruction; make sure that the
   compare has been done.  */
rtx
prepare_scc_operands (code)
     int code;
{
  if (GET_CODE (sh_compare_op0) != REG
      || REGNO (sh_compare_op0) != T_REG)
    {
      int newcode = code;
      /* First need a compare insn */
      switch (code)
	{
	case NE:
	  newcode = EQ;
	  break;
	case LT:
	  newcode = GT;
	  break;
	case LE:
	  newcode = GE;
	  break;
	case LTU:
	  newcode = GTU;
	  break;
	case LEU:
	  newcode = GEU;
	  break;
	}
      if (newcode != code)
	{
	  rtx tmp = sh_compare_op0;
	  sh_compare_op0 = sh_compare_op1;
	  sh_compare_op1 = tmp;
	  code = newcode;
	}

      sh_compare_op0 = force_reg (SImode, sh_compare_op0);
      emit_insn (gen_rtx (SET, VOIDmode,
			  gen_rtx (REG, SImode, T_REG),
		   gen_rtx (code, SImode, sh_compare_op0, sh_compare_op1)));
    }
  return gen_rtx (REG, SImode, T_REG);
}


/* Functions to output assembly code. */

/* Return a sequence of instructions to perform DI or DF move.

   Since the SH cannot move a DI or DF in one instruction, we have
   to take care when we see overlapping source and dest registers.

 */

char *
output_movedouble (insn, operands, mode)
     rtx insn;
     rtx operands[];
     enum machine_mode mode;
{
  rtx dst = operands[0];
  rtx src = operands[1];

/*   fprintf (asm_out_file, "! move double \n");
  fprintf (asm_out_file, "! pc %04x\n", insn_addresses[INSN_UID (insn)]);*/
  if (GET_CODE (dst) == MEM
      && GET_CODE (XEXP (dst, 0)) == POST_INC)
    {
      operands[0] = XEXP (XEXP (dst, 0), 0);
      return "mov.l	%R1,@(4,%0)\n\tmov.l	%1,@%0\n\tadd	#8,%0";
    }
  if (register_operand (dst, mode)
      && register_operand (src, mode))
    {
      if (REGNO (src) == MACH_REG)
	return "sts	mach,%0\n\tsts	macl,%R0";

      /*
         when mov.d r1,r2 do r2->r3 then r1->r2
         when mov.d r1,r0 do r1->r0 then r2->r1
       */

      if (REGNO (src) + 1 == REGNO (dst))
	return "mov	%R1,%R0\n\tmov	%1,%0 ! cra";
      else
	return "mov	%1,%0\n\tmov	%R1,%R0 ! crb";
    }
  else if (GET_CODE (src) == CONST_INT)
    {
      HOST_WIDE_INT val = INTVAL (src);
      int rn = REGNO (operands[0]);
      if (val < 0)
	{
	  fprintf (asm_out_file, "\tmov	#-1,r%d\n", rn);
	}
      else
	{
	  fprintf (asm_out_file, "\tmov	#0,r%d\n", rn);
	}

      fprintf (asm_out_file, "\tmov	#%d,r%d\n", val, rn + 1);
      return "";
    }
  else if (GET_CODE (src) == MEM)
    {
      int ptrreg1 = -1;
      int ptrreg2 = -1;
      int dreg = REGNO (dst);
      rtx inside = XEXP (src, 0);

      if (GET_CODE (inside) == REG)
	{
	  ptrreg1 = REGNO (inside);
	}
      else if (GET_CODE (inside) == PLUS)
	{
	  rtx lhs = XEXP (inside, 0);
	  rtx rhs = XEXP (inside, 1);
	  if (GET_CODE (lhs) == REG)
	    ptrreg1 = REGNO (lhs);
	  if (GET_CODE (rhs) == REG)
	    ptrreg2 = REGNO (rhs);
	}
      else if (GET_CODE (inside) == LABEL_REF)
	{
	  return "mov.l	%1,%0\n\tmov.l	%1+4,%R0";
	}
      else if (GET_CODE (inside) == POST_INC)
	{
	  return "mov.l	%1,%0\n\tmov.l	%1,%R0 !mdi\n";
	}
      else
	abort ();

      if ((ptrreg1 >= 0 && ptrreg2 >= 0)
	  && (dreg == ptrreg1
	      || dreg == ptrreg2
	      || dreg + 1 == ptrreg1
	      || dreg + 1 == ptrreg2))
	{
	  /* This move clobbers both index registers,
	     calculate the sum in one register.  */
	  fprintf (asm_out_file, "	add	%s,%s ! special fix\n",
		   reg_names[ptrreg2], reg_names[ptrreg1]);

	  if (dreg == ptrreg1)
	    {
	      /* Copy into dreg+1 first.  */
	      fprintf (asm_out_file, "	mov.l	@(4,%s),%s\n",
		       reg_names[ptrreg1],
		       reg_names[dreg + 1]);

	      fprintf (asm_out_file, "	mov.l	@(%s),%s\n",
		       reg_names[ptrreg1],
		       reg_names[dreg]);
	    }
	  else
	    {
	      /* Copy into dreg first. */
	      fprintf (asm_out_file, "	mov.l	@(%s),%s\n",
		       reg_names[ptrreg1],
		       reg_names[dreg]);

	      fprintf (asm_out_file, "	mov.l	@(4,%s),%s\n",
		       reg_names[ptrreg1],
		       reg_names[dreg + 1]);

	    }
	  warning ("generated complex amode");
	  return "";
	}

      /* Work out the safe way to copy */
      if (dreg == ptrreg1)
	{
	  /* Copy into the second half first */
	  return "mov.l	%R1,%R0\n\tmov.l	%1,%0 ! cr";
	}
    }

  return "mov.l	%1,%0\n\tmov.l	%R1,%R0";
}

/* Emit assembly to shift reg by k bits */

char *
output_shift (string, reg, k, code)
     char *string;
     rtx reg;
     rtx k;
     int code;

{
  int s = INTVAL (k);
  if (s < 0)
    {
      s = -s;
      switch (code)
	{
	case LSHIFTRT:
	case ASHIFTRT:
	  code = ASHIFT;
	  break;
	case ASHIFT:
	  code = ASHIFTRT;
	  break;
	default:
	  abort ();
	}
    }
  if (code == ASHIFT && s == 31)
    {
      /* Shift left by 31 moving into the t bit, clearing and rotating the other way */

      fprintf (asm_out_file, "\trotr	r%d\n", REGNO (reg));
      fprintf (asm_out_file, "\tmov	#0,r%d\n", REGNO (reg));
      fprintf (asm_out_file, "\trotcr	r%d\n", REGNO (reg));
      s = 0;
    }

  if (code == LSHIFTRT && s == 31)
    {
      fprintf (asm_out_file, "\trotl	r%d\n", REGNO (reg));
      fprintf (asm_out_file, "\tmov	#0,r%d\n", REGNO (reg));
      fprintf (asm_out_file, "\trotcl	r%d\n", REGNO (reg));
      s = 0;
    }

  while (s)
    {
      char *out;
      int d;

      if (s >= 16)
	{
	  d = 16;
	  out = "16";
	}
      else if (s >= 8)
	{
	  d = 8;
	  out = "8";
	}
      else if (s >= 2)
	{
	  d = 2;
	  out = "2";
	}
      else
	{
	  d = 1;
	  out = "";
	}
      fprintf (asm_out_file, "\t%s%s\tr%d\n", string, out, REGNO (reg));
      s -= d;
    }
  return "";
}


void
function_epilogue (stream, size)
     FILE *stream;
     int size;
{
  pragma_interrupt = pragma_trapa = 0;
}


/* Return the text of the branch instruction which matches its length
   attribute.

   This gets tricky if we have an insn in the delay slot of a branch
   and the branch needs more than 1 insn to complete. */

int pending_const_table;

 /* We can't tell if we need a register as a scratch for the jump
    until after branch shortening, and then it's too late to allocate a
    register the 'proper' way.  These instruction sequences are rare
    anyway, so to avoid always using a reg up from our limited set, we'll
    grab one when we need one on output. */

char *
output_far_jump (insn, op)
     rtx insn;
     rtx op;
{
  rtx thislab = gen_label_rtx ();

  if (dbr_sequence_length ())
    {
      /* Something to go in what would have been the delay
	 slot if this had been a short branch. Make sure the
	 reg we use to generate the branch target address
	 doesn't conflict */

      int i;
      rtx vec[2];
      vec[0] = thislab;

      for (i = 0; i < 8; i++)
	{
	  vec[1] = gen_rtx (REG, SImode, i);
	  if (!reg_referenced_p (vec[1],
				 PATTERN (XVECEXP (final_sequence, 0, 1))))
	    break;
	}


      print_slot (final_sequence);
      output_asm_insn ("mov.l	%1,@-r15", vec);
      output_asm_insn ("mov.l	%O0,%1", vec);

      output_asm_insn ("jmp	@%1 ! 32 xcond", vec);
      output_asm_insn ("mov.l	@r15+,%1", vec);
    }
  else
    {
      output_asm_insn ("mov.l	r13,@-r15", 0);
      output_asm_insn ("mov.l	%O0,r13", &thislab);
      output_asm_insn ("jmp	@r13 ! 32 zcond", 0);
      output_asm_insn ("mov.l	@r15+,r13", 0);
    }

  output_asm_insn (".align	2", 0);
  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L", CODE_LABEL_NUMBER (thislab));
  output_asm_insn (".long	%O0", &op);
  return "";
}

char *
output_branch (logic, insn)
     int logic;
     rtx insn;
{
  extern rtx recog_operand[];
  int label = lf++;

  /*  fprintf (asm_out_file, "! pc %04x\n", insn_addresses[INSN_UID (insn)]);*/

  switch (get_attr_length (insn))
    {
    case 2:
      /* Simple branch in range -200..+200 bytes */
      return logic ? "bt%.	%l0" : "bf%.	%l0";

    case 6:
      /* Branch in range -4000..+4000 bytes */
      {
	rtx oldop = recog_operand[0];


	if (need_slot (final_sequence))
	  {
	    fprintf (asm_out_file, "\tb%c.s\tLF%d\n", logic ? 'f' : 't',
		     label);

	    print_slot (final_sequence);
	  }

	else
	  {
	    fprintf (asm_out_file, "\tb%c\tLF%d\n", logic ? 'f' : 't',
		     label);
	  }
	recog_operand[0] = oldop;

	output_asm_insn ("bra	%l0	! 12 bit cond ", recog_operand);
	fprintf (asm_out_file, "\tor	r0,r0\n");
	fprintf (asm_out_file, "LF%d:\n", label);
      }
      return "";

    case 16:
      /* Branches a long way away */
      {
	rtx oldop = recog_operand[0];

	if (need_slot (final_sequence))
	  {
	    fprintf (asm_out_file, "\tb%c.s\tLF%d\n", logic ? 'f' : 't', label);
	    print_slot (final_sequence);

	  }
	else
	  {
	    fprintf (asm_out_file, "\tb%c\tLF%d\n", logic ? 'f' : 't', label);
	  }

	output_far_jump (insn, oldop);
	fprintf (asm_out_file, "LF%d:\n", label);
	return "";
      }
    }
  return "bad";
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

   Then the second move becomes the target for the shortening process.

 */

typedef struct
{
  rtx value;			/* Value in table */
  rtx label;			/* Label of value */
  enum machine_mode mode;	/* Mode of value */
}

pool_node;

/* The maximum number of constants that can fit into one pool, since
   the pc relative range is 0...1020 bytes and constants are at least 4
   bytes long */

#define MAX_POOL_SIZE (1020/4)
static pool_node pool_vector[MAX_POOL_SIZE];
static int pool_size;

/* Add a constant to the pool and return its label.  */

static rtx
add_constant (x, mode)
     rtx x;
     enum machine_mode mode;
{
  int i;
  rtx lab;
  /* First see if we've already got it */

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

  /* Need a new one */

  pool_vector[pool_size].value = x;
  lab = gen_label_rtx ();
  pool_vector[pool_size].mode = mode;
  pool_vector[pool_size].label = lab;
  pool_size++;
  return lab;
}

/* Dump out interesting debug info */

void
final_prescan_insn (insn, opvec, noperands)
     rtx insn;
     rtx *opvec;
     int noperands;
{
  if (target_flags & ISIZE_BIT)
    {
      extern int *insn_addresses;
      fprintf (asm_out_file, "\n! at %04x\n",
	       insn_addresses[INSN_UID (insn)]);
    }
}




/* Stuff taken from m88k.c */

/* Output to FILE the start of the assembler file.  */

struct option
{
  char *string;
  int *variable;
  int on_value;
};

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

static struct
{
  char *name;
  int value;
}

m_options[] = TARGET_SWITCHES;

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
  if (flag_traditional)
    pos = output_option (file, sep, "-traditional", "", indent, pos, max);
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
  fprintf (file, "! %d %d\n", max_count_si, max_count_hi);
}

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


  pos = fprintf (file, "\n! Hitachi SH cc1 (%s) (release I-1) arguments:", version_string);
  output_options (file, f_options, f_len, W_options, W_len,
		  pos, 75, " ", "\n! ", "\n\n");
}



/* Return the cost of a shift */

int
shiftcosts (RTX)
     rtx RTX;
{
  /* If shift by a non constant, then this will be expensive. */
  if (GET_CODE (XEXP (RTX, 1)) != CONST_INT)
    return 20;

  /* otherwise, it will be very cheap if by one of the constants
     we can cope with. */
  if (CONST_OK_FOR_K (INTVAL (XEXP (RTX, 1))))
    return 1;

  /* otherwise it will be several insns, but we pretend that it will be more than
     just the components, so that combine doesn't glue together a load of shifts into
     one shift which has to be emitted as a bunch anyway - breaking scheduling */
  return 1;
}

int
andcosts (RTX)
     rtx RTX;
{
  int i;
  if (GET_CODE (XEXP (RTX, 1)) != CONST_INT)
    return 2;
  i = INTVAL (XEXP (RTX, 1));
  /* And can use the extend insns cheaply */
  if (i == 0xff || i == 0xffff)
    return 2;
  /* Any small constant is reasonably cheap - but requires r0 */
  if (CONST_OK_FOR_I (i))
    return 3;
  return 5;
}

int 
howshift (i)
     int i;
{
  int total = 0;
  while (i > 0)
    {
      if (i >= 16)
	{
	  total++;
	  i -= 16;
	}
      else if (i >= 8)
	{
	  total++;
	  i -= 8;
	}
      else if (i >= 2)
	{
	  total++;
	  i -= 2;
	}
      else if (i >= 1)
	{
	  total++;
	  i--;
	}
    }
  return total;
}

/* Return the cost of a multiply */
int
multcosts (RTX)
     rtx RTX;
{
  /* If mult by a power of 2 then work out how we'd shift to make it */
  int insn_cost = 0;

  if (GET_CODE (XEXP (RTX, 1)) == CONST_INT)
    {
      int i = exact_log2 (INTVAL (XEXP (RTX, 1)));
      if (i >= 0)
	insn_cost = howshift (i);
      else
	insn_cost = 100000;
    }
  if (TARGET_SH2)
    {
      /* We have a mul insn, so we can never take more than the mul and the
	 read of the mac reg, but count more because of the latency and extra reg
	 usage */
      if (TARGET_SMALLCODE)
	return 2;
      if (insn_cost > 5)
	return 5;
      return insn_cost;
    }

  /* If we we're aiming at small code, then just count the number of
     insns in a multiply call sequence */

  if (TARGET_SMALLCODE)
    {
      if (insn_cost > 6)
	return 6;
      return insn_cost;
    }

  /* Otherwise count all the insns in the routine we'd be calling too */
  return 20;
}

/* Code to expand a shift */

void
gen_ashift (type, n, reg)
     int type;
     int n;
     rtx reg;
{
  switch (type)
    {
    case ASHIFTRT:
      emit_insn (gen_ashrsi3_k (reg, reg, GEN_INT (n)));
      break;
    case LSHIFTRT:
      emit_insn (gen_lshrsi3_k (reg, reg, GEN_INT (n)));
      break;
    case ASHIFT:
      if (n == 1)
	emit_insn (gen_addsi3 (reg, reg, reg));
      else
	emit_insn (gen_ashlsi3_k (reg, reg, GEN_INT (n)));
      break;
    }
}

int
gen_shifty_op (code, operands)
     int code;
     rtx *operands;
{
  rtx wrk = gen_reg_rtx (SImode);
  rtx t;
  char *func;
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      int value = INTVAL (operands[2]);
    top:
      switch (code)
	{
	case ASHIFTRT:
	  if (value < 0)
	    {
	      code = ASHIFT;
	      value = -value;
	      goto top;
	    }

	  /* Expand a short sequence inline, longer call a magic routine */
	  if (value <= 5)
	    {
	      emit_move_insn (wrk, operands[1]);
	      while (value--)
		{
		  gen_ashift (ASHIFTRT, 1, wrk);
		}
	      emit_move_insn (operands[0], wrk);
	      return 1;
	    }
	  t = gen_reg_rtx (Pmode);
	  /*  Load the value into an arg reg and call a helper */
	  emit_move_insn (gen_rtx (REG, SImode, 4), operands[1]);
	  if (!shiftsyms[value])
	    {
	      func = xmalloc (18);
	      sprintf (func, "__ashiftrt_r4_%d", value);
	      shiftsyms[value] = gen_rtx (SYMBOL_REF, Pmode, func);
	    }
	  emit_move_insn (t, shiftsyms[value]);
	  emit_insn (gen_ashrsi3_n (GEN_INT (value), t));
	  emit_move_insn (operands[0], gen_rtx (REG, SImode, 4));
	  return 1;

	case ASHIFT:
	  if (value < 0)
	    {
	      code = LSHIFTRT;
	      value = -value;
	      goto top;
	    }
	  /* Fall through */
	case LSHIFTRT:

	  if (value < 0)
	    {
	      code = ASHIFT;
	      value = -value;
	      goto top;
	    }

	  emit_move_insn (wrk, operands[1]);
	  while (value)
	    {
	      if (value >= 16)
		{
		  gen_ashift (code, 16, wrk);
		  value -= 16;
		}
	      else if (value >= 8)
		{
		  gen_ashift (code, 8, wrk);
		  value -= 8;
		}
	      else if (value >= 2)
		{
		  gen_ashift (code, 2, wrk);
		  value -= 2;
		}
	      else
		{
		  gen_ashift (code, 1, wrk);
		  value--;
		}
	    }
	  emit_move_insn (operands[0], wrk);
	  return 1;

	}
    }
  return 0;
}

/* Dump out any constants accumulated in the final pass -
   which will only be labels */
char *
output_jump_label_table ()
{
  int i;
  if (pool_size)
    {
      fprintf (asm_out_file, "\t.align 2\n");
      for (i = 0; i < pool_size; i++)
	{
	  pool_node *p = pool_vector + i;

	  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L", CODE_LABEL_NUMBER (p->label));
	  output_asm_insn (".long	%O0", &p->value);
	}
      pool_size = 0;
    }

  return "";
}
/* Output the literal table */

static void
dump_table (scan)
     rtx scan;
{
  int i;
  int need_align = 1;


  /* Do two passes, first time dump out the HI sized constants */

  for (i = 0; i < pool_size; i++)
    {
      pool_node *p = pool_vector + i;
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
      pool_node *p = pool_vector + i;

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



/* Non zero if the src operand needs to be fixed up */
static
int
fixit (src, mode)
     rtx src;
     enum machine_mode mode;
{
  if (mode == QImode)
    return 0;			/* QIs never need to be fixed */
  if (GET_CODE (src) == CONST)
    return 1;

  if (GET_CODE (src) == SYMBOL_REF)
    {
      return 1;
    }
  if (GET_CODE (src) == LABEL_REF)
    {
      return 1;
    }
  if (GET_CODE (src) == CONST_INT)
    {
      /* All QI insns are ok */
      if (mode == QImode)
	return 1;
      /* The rest may need to be fixed */
      return !CONST_OK_FOR_I (INTVAL (src));
    }
  return 0;
}

/* Return Non-zero if constant would be an ok source for a
   mov.w instead of a mov.l */
int
hi_const (src)
     rtx src;
{
  if (GET_CODE (src) == CONST
      && GET_CODE (XEXP (src, 0)) == SIGN_EXTEND
      && GET_CODE (XEXP (XEXP (src, 0), 0)) == SYMBOL_REF)
    return 1;

  if (TARGET_SHORTADDR
      && GET_CODE (src) == SYMBOL_REF)
    return 1;

  return (GET_CODE (src) == CONST_INT
	  && INTVAL (src) >= -32768
	  && INTVAL (src) <= 32767);
}

/* Find the last barrier less than MAX_COUNT bytes from FROM, or create one.
   If an HI move is found, then make sure that MAX_COUNT_HI isn't broken from that one. */

static
rtx
find_barrier (from)
     rtx from;
{
  int count_si = 0;
  int count_hi = 0;
  int found_hi = 0;
  int found_si = 0;
  rtx found_barrier = 0;
  while (from
	 && count_si < max_count_si
	 && count_hi < max_count_hi)
    {
      int inc;
      if (GET_CODE (from) == BARRIER)
	{
	  found_barrier = from;
	}
      /* Count the length of this insn - we assume that all moves will
	 be 2 bytes long, except the DIs */

      if (GET_CODE (from) == INSN &&
	  GET_CODE (PATTERN (from)) == SET)
	{
	  rtx src = SET_SRC (PATTERN (from));
	  if (hi_const (src))
	    found_hi = 1;
	  else
	    found_si = 1;
	  inc = (GET_MODE_SIZE (GET_MODE (src)) > 4) ? 4 : 2;
	}
      else
	{
	  inc = get_attr_length (from);
	}
      if (found_si)
	count_si += inc;
      if (found_hi)
	count_hi += inc;
      from = NEXT_INSN (from);
    }

  if (!found_barrier)
    {
      /* We didn't find a barrier in time to 
	 dump our stuff, so we'll make one */
      rtx label = gen_label_rtx ();
      /* Walk back to be just before any jump */
      from = PREV_INSN (from);
      while (GET_CODE (from) == JUMP_INSN
	     || GET_CODE (from) == NOTE
	     || GET_CODE (from) == CODE_LABEL)
	{
	  from = PREV_INSN (from);
	}
      from = emit_jump_insn_after (gen_jump (label), from);
      JUMP_LABEL (from) = label;
      found_barrier = emit_barrier_after (from);
      emit_label_after (label, found_barrier);
      return found_barrier;
    }
  return found_barrier;
}

/* Non zero if the insn is a move instruction which needs to be fixed. */

static
int
broken_move (insn)
     rtx insn;
{
  if (!INSN_DELETED_P (insn)
      && GET_CODE (insn) == INSN
      && GET_CODE (PATTERN (insn)) == SET)
    {
      rtx pat = PATTERN (insn);
      rtx src = SET_SRC (pat);
      rtx dst = SET_DEST (pat);
      enum machine_mode mode = GET_MODE (dst);
      if (dst == pc_rtx)
	return 0;
      return fixit (src, mode);
    }
  return 0;
}


/* Exported to toplev.c

   Scan the function looking for move instructions which have to be changed to
   pcrel loads and insert the literal tables. */

void
machine_dependent_reorg (first)
     rtx first;
{
  rtx insn;
  for (insn = first; insn; insn = NEXT_INSN (insn))
    {
      if (broken_move (insn))
	{
	  /* This is a broken move instruction, scan ahead looking for
	     a barrier to stick the constant table behind */
	  rtx scan;
	  rtx barrier = find_barrier (insn);

	  /* Now find all the moves between the points and modify them */
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
		  /* This is a broken move instruction, add it to the pool */

		  if (mode == SImode && hi_const (src))
		    {
		      /* This is an HI source, clobber the dest to get the mode right too */
		      mode = HImode;
		      while (GET_CODE (dst) == SUBREG)
			dst = SUBREG_REG (dst);
		      dst = gen_rtx (REG, HImode, REGNO (dst));
		    }
		  lab = add_constant (src, mode);
		  newsrc = gen_rtx (MEM, mode,
				    gen_rtx (LABEL_REF, VOIDmode, lab));

		  /* Build a jump insn wrapper around the move instead
		     of an ordinary insn, because we want to have room for
		     the target label rtx in fld[7], which an ordinary
		     insn doesn't have. */
		  newinsn = emit_jump_insn_after (gen_rtx (SET, VOIDmode,
							dst, newsrc), scan);
		  JUMP_LABEL (newinsn) = lab;

		  /* But it's still an ordinary insn */
		  PUT_CODE (newinsn, INSN);

		  /* Kill old insn */
		  delete_insn (scan);
		  scan = newinsn;
		}
	    }
	  dump_table (barrier);
	}
    }
}

/* Called from the md file, set up the operands of a compare instruction */

void
from_compare (operands, code)
     rtx *operands;
     int code;
{
  if (code != EQ && code != NE)
    {
      /* Force args into regs, since we can't use constants here */
      sh_compare_op0 = force_reg (SImode, sh_compare_op0);
      if (sh_compare_op1 != const0_rtx)
	sh_compare_op1 = force_reg (SImode, sh_compare_op1);
    }
  operands[1] = sh_compare_op0;
  operands[2] = sh_compare_op1;
}

/* Non-zero if x is EQ or NE */

int
equality_operator (x, mode)
     rtx x;
     enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (x);
  return (code == EQ || code == NE);
}


/* Add this function to the list of ones seen - temporary
   gross hack to try out bsrs. */
struct flist
{
  char *name;
  struct flist *next;
};
struct flist *head;

static void
add_function (name)
     char *name;
{
  struct flist *n = (struct flist *) xmalloc (sizeof (struct flist));
  int l = strlen (name) + 1;
  n->name = xmalloc (l);
  memcpy (n->name, name, l);
  n->next = head;
  head = n;
}

static int
seen_function (name)
     char *name;
{
  struct flist *p = head;
  for (p = head; p; p = p->next)
    {
      if (strcmp (p->name, name) == 0)
	return 1;
    }
  return 0;
}

 /* Framefull frame looks like:

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
    local-0        <- fp points here


    If TARGET_SMALLCALL, then the preserved registers are pushed by a
    wrapper before the routine is entered, so the regs are always pushed
    and there are two pr's on the stack - the caller and the wrapper.
  */


 /* Code to generate prologue and epilogue sequences */


void
sh_expand_prologue ()
{
  int live_regs_mask;
  int d;
  extern tree current_function_decl;
  live_regs_mask = calc_live_regs (&d);

  /* We have pretend args if we had an object sent partially in registers
     and partially on the stack - eg a large structure */
  output_stack_adjust (-current_function_pretend_args_size);

  if (current_function_anonymous_args)
    {
      /* Push arg regs as if they'd been provided by caller in stack */
      int i;
      for (i = 0; i < NPARM_REGS; i++)
	{
	  int rn = NPARM_REGS + FIRST_PARM_REG - i - 1;
	  if (i > NPARM_REGS - current_function_args_info)
	    break;
	  push (rn);
	  extra_push += 4;
	}
    }
  push_regs (live_regs_mask);
  output_stack_adjust (-get_frame_size ());

  if (frame_pointer_needed)
    {
      emit_insn (gen_movsi (frame_pointer_rtx, stack_pointer_rtx));
    }
  if (TARGET_BSR)
    {
      add_function (IDENTIFIER_POINTER (DECL_NAME (current_function_decl)));
    }


}

void
sh_expand_epilogue ()
{
  int live_regs_mask;
  int d;
  int i;

  live_regs_mask = calc_live_regs (&d);

  
  if (frame_pointer_needed)
    {
      emit_insn (gen_movsi (stack_pointer_rtx, frame_pointer_rtx));
    }
  output_stack_adjust (get_frame_size ());

  /* Pop all the registers */

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      int j = (FIRST_PSEUDO_REGISTER - 1) - i;
      if (live_regs_mask & (1 << j))
	{
	  pop (j);
	}
    }

  output_stack_adjust (extra_push + current_function_pretend_args_size);

  extra_push = 0;
  current_function_pretend_args_size = 0;
  current_function_anonymous_args = 0;
  for (i = 0; i < 32; i++)
    shiftsyms[i] = 0;

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
    {
      return total_saved_regs_space + total_auto_space;
    }
  if (from == ARG_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    {
      return total_saved_regs_space + total_auto_space;
    }
  if (from == FRAME_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    {
      /* Initial gap between fp and sp is 0 */
      return 0;
    }
  abort ();
}

/* Handle machine specific pragmas to be semi-compatible with Hitachi
   compiler  */

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

/* insn expand helpers */

/* Emit insns to perform a call.
   If TARGET_SHORTADDR then use a bsr. If TARGET_SMALLCALL, then load the
   target address into r1 and call __saveargs, otherwise
   perform the standard call sequence */

void
expand_acall (isa_retval, operands)
     int isa_retval;
     rtx *operands;
{
  rtx call;
  rtx ret = operands[0];
  rtx call_target = operands[isa_retval + 0];
  rtx numargs = operands[isa_retval + 1];

  if (TARGET_BSR && bsr_operand (call_target, VOIDmode))
    {
      call = gen_rtx (CALL, VOIDmode, call_target, numargs);
    }
  else
    {
      if (GET_CODE (call_target) == MEM)
	{
	  call_target = force_reg (Pmode,
				   XEXP (call_target, 0));
	}
      if (TARGET_SMALLCALL)
	{
	  rtx tmp = gen_reg_rtx (SImode);
	  rtx r1 = gen_rtx (REG, SImode, 1);
	  emit_move_insn (tmp, gen_rtx (SYMBOL_REF, SImode, "__saveargs"));
	  emit_move_insn (r1, call_target);
	  emit_insn (gen_rtx (USE, VOIDmode, r1));
	  call_target = tmp;
	}

      call = gen_rtx (CALL, VOIDmode, gen_rtx (MEM, SImode, call_target), numargs);
    }
  if (isa_retval)
    {
      call = gen_rtx (SET, VOIDmode, ret, call);
    }

  emit_call_insn (gen_rtx (PARALLEL, VOIDmode,
			   gen_rtvec (2,
				      call,
		  gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 17)))));

}


/* Predicates used by the templates */


/* Returns 1 if OP can be source of a simple move operation.
   Same as general_operand, but a LABEL_REF is valid, PRE_DEC is
   invalid as are subregs of system registers. */

int
general_movsrc_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  /* Any MEM(label_ref) is ok, that's a pcrel load */
  if (GET_CODE (op) == MEM
      && GET_CODE (XEXP (op, 0)) == LABEL_REF)
    return 1;

  if (GET_CODE (op) == MEM)
    {
      rtx inside = XEXP (op, 0);
      if (GET_CODE (inside) == CONST)
	inside = XEXP (inside, 0);

      if (GET_CODE (inside) == LABEL_REF)
	return 1;

      if (GET_CODE (inside) == PLUS
	  && GET_CODE (XEXP (inside,0)) == LABEL_REF
	  && GET_CODE (XEXP (inside,1)) == CONST_INT)
	return 1;
      
      /* No post inc allowed */
      if (GET_CODE (inside) == POST_DEC
	  || GET_CODE (inside) == PRE_INC
	  || GET_CODE (inside) == PRE_DEC)
	return 0;

      /* Can't do that with large modes */
      if (GET_CODE (inside) == POST_INC
	  && GET_MODE_SIZE (mode) > 4)
	return 0;
    }

  if ((mode == QImode || mode == HImode)
      && (GET_CODE (op) == SUBREG
	  && GET_CODE (XEXP (op, 0)) == REG
	  && system_reg_operand (XEXP (op, 0), mode)))
    return 0;

  if (GET_CODE (op) == CONST_INT)
    {
      int i = INTVAL (op);
      return CONST_OK_FOR_I (i);
    }
  return general_operand (op, mode);
}


/* Returns 1 if OP can be a destination of a move.
   Same as general_operand, but no preinc allowed.  */

int
general_movdst_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  /* No pre dec allowed */
  if (GET_CODE (op) == MEM
      && (GET_CODE (XEXP (op, 0)) == PRE_INC
	  || GET_CODE (XEXP (op, 0)) == POST_INC
	  || GET_CODE (XEXP (op, 0)) == POST_DEC))
    return 0;

  if (GET_CODE (op) == MEM
      && GET_CODE (XEXP (op, 0)) == PRE_DEC
      && GET_MODE_SIZE (mode) > 4)
    return 0;

  return general_operand (op, mode);
}



/* Returns 1 if OP is valid destination for a bsr.  */

int
bsr_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (TARGET_BSR)
    {
      if (GET_CODE (op) == SYMBOL_REF)
	{
	  if (!strcmp (XSTR (op, 0),
		      IDENTIFIER_POINTER (DECL_NAME (current_function_decl))))
	    return 1;
	  return (seen_function (XSTR (op, 0)));
	}
    }
  return 0;
}

/* Returns 1 if OP is an immediate ok for a byte index.  */

int
byte_index_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT
	  && INTVAL (op) >= 0
	  && INTVAL (op) <= 15);
}

/* Returns 1 if OP is a pop operand.   */

int
pop_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) != MEM)
    return 0;

  if (GET_MODE (op) != mode)
    return 0;

  op = XEXP (op, 0);

  if (GET_CODE (op) != POST_INC)
    return 0;

  return XEXP (op, 0) == stack_pointer_rtx;
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
		&& REGNO (op) != PR_REG);
      return 1;
    }
  return 0;
}

/* Returns 1 if OP is MACL, MACH or PR.  */

int
system_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == REG)
    {
      switch (REGNO (op))
	{
	case PR_REG:
	case MACL_REG:
	case MACH_REG:
	  return 1;
	}
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

  if (GET_CODE (op) == CONST_INT)
    {
      if (CONST_OK_FOR_I (INTVAL (op)))
	return 1;
    }
  return 0;
}


/* Returns 1 if OP is a valid source operand for a logical operation. */

int
logical_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (arith_reg_operand (op, mode))
    return 1;

  if (GET_CODE (op) == CONST_INT)
    {
      if (CONST_OK_FOR_L (INTVAL (op)))
	return 1;
    }
  return 0;
}

/* Returns 1 if OP is a valid operand for a MAC instruction,
   either a register or indirect memory.  For now we don't
   try and recognise a mac insn */

int
mac_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (arith_reg_operand (op, mode))
    return 1;
#if 0
  Turned off till mac is understood
  if (GET_CODE (op) == MEM)
    return 1;
#endif
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
      int rr = (ROUND_REG ((cum), (mode)));

      if (rr < NPARM_REGS)
	{
	  return (((type) == 0 || !TREE_ADDRESSABLE ((tree) (type)))
		  && ((type) == 0 || (mode) != BLKmode
		      || (TYPE_ALIGN ((type)) % PARM_BOUNDARY == 0))
		  ? gen_rtx (REG, (mode),
			     (FIRST_PARM_REG + rr)) 
		  : 0);

	}
    }
  return 0;
}

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero.
   Any arg that starts in the first 4 regs but won't entirely fit in them
   needs partial registers on the SH.  */

int
sh_function_arg_partial_nregs (CUM, MODE, TYPE, NAMED)
     CUMULATIVE_ARGS CUM;
     enum machine_mode MODE;
     tree TYPE;
     int NAMED;
{
  if ((CUM) < NPARM_REGS)
    {
      if (((TYPE) == 0 || !TREE_ADDRESSABLE ((tree) (TYPE)))
	  && ((TYPE) == 0 || (TYPE_ALIGN ((TYPE)) % PARM_BOUNDARY == 0))
	  && ((CUM) + ((MODE) == BLKmode
		       ? ROUND_ADVANCE (int_size_in_bytes (TYPE))
		  : ROUND_ADVANCE (GET_MODE_SIZE (MODE))) - NPARM_REGS > 0))
	{
	  return NPARM_REGS - CUM;
	}
    }
  return 0;
}

/* Turn this on to recognise shift insns which aren't supported in the
   hardware.  This will allow the combiner to notice more patterns,
   but the down side is that the asm outputter will have to emit
   several instructions for each shift which isn't possible in the
   hardware, this makes scheduling perform badly .*/ 

int fake_shift()
{
  return 0;
}
