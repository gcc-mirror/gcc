/* Subroutines for insn-output.c for HPPA.
   Copyright (C) 1992 Free Software Foundation, Inc.
   Contributed by Tim Moore (moore@cs.utah.edu), based on sparc.c

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

#include <stdio.h>
#include "config.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "tree.h"
#include "c-tree.h"
#include "expr.h"
#include "obstack.h"

/* Save the operands last given to a compare for use when we
   generate a scc or bcc insn.  */

rtx hppa_compare_op0, hppa_compare_op1;
enum cmp_type hppa_branch_type;

/* Set by the FUNCTION_PROFILER macro. */
int hp_profile_labelno;

/* Name of where we pretend to think the frame pointer points.
   Normally, this is "4", but if we are in a leaf procedure,
   this is "something(30)".  Will this work? */
char *frame_base_name;

static rtx find_addr_reg ();

/* Return non-zero only if OP is a register of mode MODE,
   or const0_rtx.  */
int
reg_or_0_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (op == const0_rtx || register_operand (op, mode));
}

int
call_operand_address (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (REG_P (op) 
	  || (CONSTANT_P (op) && ! TARGET_LONG_CALLS));
}

int
symbolic_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  switch (GET_CODE (op))
    {
    case SYMBOL_REF:
    case LABEL_REF:
      return 1;
    case CONST:
      op = XEXP (op, 0);
      return ((GET_CODE (XEXP (op, 0)) == SYMBOL_REF
	       || GET_CODE (XEXP (op, 0)) == LABEL_REF)
	      && GET_CODE (XEXP (op, 1)) == CONST_INT);
    default:
      return 0;
    }
}

/* Return truth value of statement that OP is a symbolic memory
   operand of mode MODE.  */

int
symbolic_memory_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  if (GET_CODE (op) != MEM)
    return 0;
  op = XEXP (op, 0);
  return (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == CONST
	  || GET_CODE (op) == HIGH || GET_CODE (op) == LABEL_REF);
}

/* Return 1 if the operand is either a register or a memory operand that is
   not symbolic.  */

int
reg_or_nonsymb_mem_operand (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  if (register_operand (op, mode))
    return 1;

  if (memory_operand (op, mode) && ! symbolic_memory_operand (op, mode))
    return 1;

  return 0;
}

int
move_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (register_operand (op, mode))
    return 1;

  if (op == CONST0_RTX (mode))
    return 1;

  if (GET_MODE (op) != mode)
    return 0;
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  if (GET_CODE (op) != MEM)
    return 0;

  op = XEXP (op, 0);
  if (GET_CODE (op) == LO_SUM)
    return (register_operand (XEXP (op, 0), Pmode)
	    && CONSTANT_P (XEXP (op, 1)));
  return memory_address_p (mode, op);
}

int
pic_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return flag_pic && GET_CODE (op) == LABEL_REF;
}

int
short_memory_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == MEM)
    {
      if (GET_CODE (XEXP (op, 0)) == REG)
	return 1;
      else if (GET_CODE (XEXP (op, 0)) == PLUS)
	{
	  rtx op1 = XEXP (XEXP (op, 0), 0);
	  rtx op2 = XEXP (XEXP (op, 0), 1);

	  if (GET_CODE (op1) == REG)
	    return (GET_CODE (op2) == CONST_INT && INT_5_BITS (op2));
	  else if (GET_CODE (op2) == REG)
	    return (GET_CODE (op1) == CONST_INT && INT_5_BITS (op1));
	}
    }
  return 0;
}

int
register_or_short_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (register_operand (op, mode))
    return 1;
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  return short_memory_operand (op, mode);
}

int
fp_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return reg_renumber && FP_REG_P (op);
}

int
check_fp_mov (operands)
     rtx *operands;
{
  enum machine_mode mode = GET_MODE (operands[0]);

  if (fp_reg_operand (operands[0], mode))
    return (register_operand (operands[1], mode)
	    || short_memory_operand (operands[1], mode));
  else if (fp_reg_operand (operands[1], mode))
    return (register_operand (operands[0], mode)
	    || short_memory_operand (operands[0], mode));
  else
    return 1;
}

extern int current_function_uses_pic_offset_table;
extern rtx force_reg (), validize_mem ();

/* The rtx for the global offset table which is a special form
   that *is* a position independent symbolic constant.  */
rtx pic_pc_rtx;

/* Ensure that we are not using patterns that are not OK with PIC.  */

int
check_pic (i)
     int i;
{
  extern rtx recog_operand[];
  switch (flag_pic)
    {
    case 1:
      if (GET_CODE (recog_operand[i]) == SYMBOL_REF
	  || (GET_CODE (recog_operand[i]) == CONST
	      && ! rtx_equal_p (pic_pc_rtx, recog_operand[i])))
	abort ();
    case 2:
    default:
      return 1;
    }
}

/* Return truth value of whether OP is EQ or NE.  */

int
eq_or_neq (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == EQ || GET_CODE (op) == NE);
}

/* Return truth value of whether OP can be used as an operand in a
   three operand arithmetic insn that accepts registers of mode MODE
   or 14-bit signed integers.  */
int
arith_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && INT_14_BITS (op)));
}

/* Return truth value of whether OP can be used as an operand in a
   three operand arithmetic insn that accepts registers of mode MODE
   or 11-bit signed integers.  */
int
arith11_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && INT_11_BITS (op)));
}

int
arith_double_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_DOUBLE
	      && GET_MODE (op) == mode
	      && VAL_14_BITS_P (CONST_DOUBLE_LOW (op))
	      && (CONST_DOUBLE_HIGH (op) >= 0
		  == ((CONST_DOUBLE_LOW (op) & 0x1000) == 0))));
}

/* Return truth value of whether OP is a integer which fits the
   range constraining immediate operands in three-address insns.  */

int
int5_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT && INT_5_BITS (op));
}

int
uint5_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT && INT_U5_BITS (op));
}

  
int
int11_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
    return (GET_CODE (op) == CONST_INT && INT_11_BITS (op));
}

int
arith5_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return register_operand (op, mode) || int5_operand (op, mode);
}

/* True iff zdepi can be used to generate this CONST_INT.  */
int
depi_cint_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  unsigned x;
  unsigned lbmask, t;

  if (GET_CODE (op) != CONST_INT)
    return 0;

  /* This might not be obvious, but it's at least fast.
     This function is critcal; we don't have the time loops would take.  */
  x = INTVAL (op);
  lbmask = x & -x;
  t = ((x >> 4) + lbmask) & ~(lbmask - 1);
  return ((t & (t - 1)) == 0);
}

/* True iff depi or extru can be used to compute (reg & mask).  */
int
consec_zeros_p (mask)
     unsigned mask;
{
  mask = ~mask;
  mask += mask & -mask;
  return (mask & (mask - 1)) == 0;
}

/* True iff depi or extru can be used to compute (reg & OP).  */
int
and_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && consec_zeros_p (INTVAL (op))));
}

/* True iff depi can be used to compute (reg | MASK).  */
int
ior_mask_p (mask)
     unsigned mask;
{
  mask += mask & -mask;
  return (mask & (mask - 1)) == 0;
}

/* True iff depi can be used to compute (reg | OP).  */
int
ior_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && ior_mask_p (INTVAL (op))));
}

int
arith32_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return register_operand (op, mode) || GET_CODE (op) == CONST_INT;
}

/* True iff OP can be the source of a move to a general register.  */
int
srcsi_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  /* Not intended for other modes than SImode.  */
  if (mode != SImode)
    return 0;

  /* Accept any register or memory reference.  */
  if (nonimmediate_operand (op, mode))
    return 1;

  if (depi_cint_operand (op, mode))
    return 1;

  /* OK if ldo or ldil can be used.  */
  return (GET_CODE (op) == CONST_INT
	  && (INT_14_BITS (op) || (INTVAL (op) & 0x7ff) == 0));
}


/* Legitimize PIC addresses.  If the address is already
   position-independent, we return ORIG.  Newly generated
   position-independent addresses go to REG.  If we need more
   than one register, we lose.  */

rtx
legitimize_pic_address (orig, mode, reg)
     rtx orig, reg;
     enum machine_mode mode;
{
  rtx pic_ref = orig;

  if (GET_CODE (orig) == SYMBOL_REF)
    {
      if (reg == 0)
	abort ();

      if (flag_pic == 2)
	{
	  emit_insn (gen_rtx (SET, VOIDmode, reg,
			      gen_rtx (HIGH, Pmode, orig)));
	  emit_insn (gen_rtx (SET, VOIDmode, reg,
			      gen_rtx (LO_SUM, Pmode, reg, orig)));
	  orig = reg;
	}
      pic_ref = gen_rtx (MEM, Pmode,
			 gen_rtx (PLUS, Pmode,
				  pic_offset_table_rtx, orig));
      current_function_uses_pic_offset_table = 1;
      RTX_UNCHANGING_P (pic_ref) = 1;
      emit_move_insn (reg, pic_ref);
      return reg;
    }
  else if (GET_CODE (orig) == CONST)
    {
      rtx base, offset;

      if (GET_CODE (XEXP (orig, 0)) == PLUS
	  && XEXP (XEXP (orig, 0), 0) == pic_offset_table_rtx)
	return orig;

      if (reg == 0)
	abort ();

      if (GET_CODE (XEXP (orig, 0)) == PLUS)
	{
	  base = legitimize_pic_address (XEXP (XEXP (orig, 0), 0), Pmode, reg);
	  orig = legitimize_pic_address (XEXP (XEXP (orig, 0), 1), Pmode,
					 base == reg ? 0 : reg);
	}
      else abort ();
      if (GET_CODE (orig) == CONST_INT)
	{
	  if (SMALL_INT (orig))
	    return plus_constant_for_output (base, INTVAL (orig));
	  orig = force_reg (Pmode, orig);
	}
      pic_ref = gen_rtx (PLUS, Pmode, base, orig);
      /* Likewise, should we set special REG_NOTEs here?  */
    }
  return pic_ref;
}

/* Set up PIC-specific rtl.  This should not cause any insns
   to be emitted.  */

void
initialize_pic ()
{
}

/* Emit special PIC prologues and epilogues.  */

void
finalize_pic ()
{
  /* Need to emit this whether or not we obey regdecls,
     since setjmp/longjmp can cause life info to screw up.  */
  emit_insn (gen_rtx (USE, VOIDmode, pic_offset_table_rtx));
}

/* For the HPPA, REG and REG+CONST is cost 0
   and addresses involving symbolic constants are cost 2.

   PIC addresses are very expensive.

   It is no coincidence that this has the same structure
   as GO_IF_LEGITIMATE_ADDRESS.  */
int
hppa_address_cost (X)
     rtx X;
{
  if (GET_CODE (X) == PLUS)
      return 1;
  else if (GET_CODE (X) == LO_SUM)
    return 1;
  else if (GET_CODE (X) == HIGH)
    return 2;
  return 4;
}

/* Emit insns to move operands[1] into operands[0].

   Return 1 if we have written out everything that needs to be done to
   do the move.  Otherwise, return 0 and the caller will emit the move
   normally.  */

int
emit_move_sequence (operands, mode, scratch_reg)
     rtx *operands;
     enum machine_mode mode;
     rtx scratch_reg;
{
  register rtx operand0 = operands[0];
  register rtx operand1 = operands[1];

  if (fp_reg_operand (operand0, mode)
      && GET_CODE (operand1) == MEM
      && !short_memory_operand  (operand1, mode)
      && scratch_reg)
    {
      emit_move_insn (scratch_reg, XEXP (operand1 , 0));
      emit_insn (gen_rtx (SET, VOIDmode, operand0, gen_rtx (MEM, mode,
							    scratch_reg)));
      return 1;
    }
  else if (fp_reg_operand (operand1, mode)
	   && GET_CODE (operand0) == MEM
	   && !short_memory_operand  (operand0, mode)
	   && scratch_reg)
    {
      emit_move_insn (scratch_reg, XEXP (operand0 , 0));
      emit_insn (gen_rtx (SET, VOIDmode, gen_rtx (MEM, mode,  scratch_reg),
			  operand1));
      return 1;
    }
  /* Handle most common case: storing into a register.  */
  else if (register_operand (operand0, mode))
    {
      if (register_operand (operand1, mode)
	  || (GET_CODE (operand1) == CONST_INT && SMALL_INT (operand1))
	  || (GET_CODE (operand1) == HIGH
	      && !symbolic_operand (XEXP (operand1, 0)))
	  /* Only `general_operands' can come here, so MEM is ok.  */
	  || GET_CODE (operand1) == MEM)
	{
	  /* Run this case quickly.  */
	  emit_insn (gen_rtx (SET, VOIDmode, operand0, operand1));
	  return 1;
	}
    }
  else if (GET_CODE (operand0) == MEM)
    {
      if (register_operand (operand1, mode) || operand1 == const0_rtx)
	{
	  /* Run this case quickly.  */
	  emit_insn (gen_rtx (SET, VOIDmode, operand0, operand1));
	  return 1;
	}
      if (! reload_in_progress)
	{
	  operands[0] = validize_mem (operand0);
	  operands[1] = operand1 = force_reg (mode, operand1);
	}
    }

  /* Simplify the source if we need to.  */
  if (GET_CODE (operand1) != HIGH && immediate_operand (operand1, mode))
    {
      if (symbolic_operand (operand1, mode))
	{
	  if (flag_pic)
	    {
	      rtx temp = reload_in_progress ? operand0 : gen_reg_rtx (Pmode);
	      operands[1] = legitimize_pic_address (operand1, mode, temp);
	    }
	  /* On the HPPA, references to data space are supposed to */
	  /* use dp, register 27. */
	  else if (read_only_operand (operand1))
	    {
	      rtx set = gen_rtx (SET, VOIDmode,
				  operand0,
				  gen_rtx (LO_SUM, mode, operand0, operand1));
				 
	      emit_insn (gen_rtx (SET, VOIDmode,
				  operand0,
				  gen_rtx (HIGH, mode, operand1)));
	      if (TARGET_SHARED_LIBS
		  && function_label_operand (operand1, mode))
		{
		  rtx temp = reload_in_progress ? scratch_reg
		    : gen_reg_rtx (mode);
		  if (!temp)
		    abort ();
		  emit_insn (gen_rtx (PARALLEL, VOIDmode,
				      gen_rtvec (2,
						 set,
						 gen_rtx (CLOBBER, VOIDmode,
							  temp))));
		}
	      else
		emit_insn (set);
	      return 1;
	    }
	  else
	    {
	      /* If reload_in_progress, we can't use addil and r1; we */
	      /* have to use the more expensive ldil sequence. */
	      if (reload_in_progress)
		{
		  emit_insn (gen_rtx (SET, VOIDmode,
				      operand0,
				      gen_rtx (HIGH, mode, operand1)));
		  emit_insn (gen_rtx (SET, VOIDmode,
				      operand0,
				      gen_rtx (PLUS, mode,
					       operand0,
					       gen_rtx (REG, mode, 27))));
		  emit_insn (gen_rtx (SET, VOIDmode,
				      operand0,
				      gen_rtx (LO_SUM, mode,
					       operand0, operand1)));
		}
	      else
		{
		  rtx temp1, temp2 = gen_reg_rtx (mode);

		  /* For 2.4 we could set RTX_UNCHANGING and add a 
		     REG_EQUAL note for the first insn.  This would 
		     allow the first insn to be moved out of loops.  */
		  temp1 = gen_rtx (HIGH, mode, operand1);
		  emit_insn (gen_rtx (SET, VOIDmode,
				      temp2,
				      gen_rtx (PLUS, mode,
					       gen_rtx (REG, mode, 27),
					       temp1)));
		  emit_insn (gen_rtx (SET, VOIDmode,
				      operand0,
				      gen_rtx (LO_SUM, mode,
					       temp2, operand1)));
		}
	      return 1;
	    }
	}
      else if (depi_cint_operand (operand1, VOIDmode))
	return 0;
      else if (GET_CODE (operand1) == CONST_INT
	       ? (! SMALL_INT (operand1)
		  && (INTVAL (operand1) & 0x7ff) != 0) : 1)
	{
	  rtx temp = reload_in_progress ? operand0 : gen_reg_rtx (mode);
	  emit_insn (gen_rtx (SET, VOIDmode, temp,
			      gen_rtx (HIGH, mode, operand1)));
	  operands[1] = gen_rtx (LO_SUM, mode, temp, operand1);
	}
    }
  /* Now have insn-emit do whatever it normally does.  */
  return 0;
}

/* Does operand (which is a symbolic_operand) live in text space? If
   so SYMBOL_REF_FLAG, which is set by ENCODE_SECTION_INFO, will be true.*/

int
read_only_operand (operand)
     rtx operand;
{
  if (GET_CODE (operand) == CONST)
    operand = XEXP (XEXP (operand, 0), 0);
  if (GET_CODE (operand) == SYMBOL_REF)
    return SYMBOL_REF_FLAG (operand) || CONSTANT_POOL_ADDRESS_P (operand);
  return 1;
}
     

/* Return the best assembler insn template
   for moving operands[1] into operands[0] as a fullword.  */

char *
singlemove_string (operands)
     rtx *operands;
{
  if (GET_CODE (operands[0]) == MEM)
    return "stw %r1,%0";
  if (GET_CODE (operands[1]) == MEM)
    return "ldw %1,%0";
  if (GET_CODE (operands[1]) == CONST_INT)
    if (INT_14_BITS (operands[1]))
      return (INTVAL (operands[1]) == 0 ? "copy 0,%0" : "ldi %1,%0");
    else
      return "ldil L'%1,%0\n\tldo R'%1(%0),%0";
  return "copy %1,%0";
}


/* Compute position (in OPERANDS[2]) and width (in OPERANDS[3])
   useful for copying or or'ing IMM to a register using bit field
   instructions.  Store the immediate value to insert in OPERANDS[1].  */
void
compute_xdepi_operands_from_integer (imm, operands)
     unsigned imm;
     rtx *operands;
{
  int lsb, len;

  /* Find the least significant set bit in IMM.  */
  for (lsb = 0; lsb < 32; lsb++)
    {
      if ((imm & 1) != 0)
        break;
      imm >>= 1;
    }

  /* Choose variants based on *sign* of the 5-bit field.  */
  if ((imm & 0x10) == 0)
    len = (lsb <= 28) ? 4 : 32 - lsb;
  else
    {
      /* Find the width of the bitstring in IMM.  */
      for (len = 5; len < 32; len++)
	{
	  if ((imm & (1 << len)) == 0)
	    break;
	}

      /* Sign extend IMM as a 5-bit value.  */
      imm = (imm & 0xf) - 0x10;
    }

  operands[1] = gen_rtx (CONST_INT, VOIDmode, imm);
  operands[2] = gen_rtx (CONST_INT, VOIDmode, 31 - lsb);
  operands[3] = gen_rtx (CONST_INT, VOIDmode, len);
}

/* Output assembler code to perform a doubleword move insn
   with operands OPERANDS.  */

char *
output_move_double (operands)
     rtx *operands;
{
  enum { REGOP, OFFSOP, MEMOP, CNSTOP, RNDOP } optype0, optype1;
  rtx latehalf[2];
  rtx addreg0 = 0, addreg1 = 0;

  /* First classify both operands.  */

  if (REG_P (operands[0]))
    optype0 = REGOP;
  else if (offsettable_memref_p (operands[0]))
    optype0 = OFFSOP;
  else if (GET_CODE (operands[0]) == MEM)
    optype0 = MEMOP;
  else
    optype0 = RNDOP;

  if (REG_P (operands[1]))
    optype1 = REGOP;
  else if (CONSTANT_P (operands[1]))
    optype1 = CNSTOP;
  else if (offsettable_memref_p (operands[1]))
    optype1 = OFFSOP;
  else if (GET_CODE (operands[1]) == MEM)
    optype1 = MEMOP;
  else
    optype1 = RNDOP;

  /* Check for the cases that the operand constraints are not
     supposed to allow to happen.  Abort if we get one,
     because generating code for these cases is painful.  */

  if (optype0 != REGOP && optype1 != REGOP)
    abort ();

   /* Handle auto decrementing and incrementing loads and stores
     specifically, since the structure of the function doesn't work
     for them without major modification.  Do it better when we learn
     this port about the general inc/dec addressing of PA.
     (This was written by tege.  Chide him if it doesn't work.)  */

  if (optype0 == MEMOP)
    {
      /* We have to output the address syntax ourselves, since print_operand
	 doesn't deal with the addresses we want to use.  Fix this later.  */

      rtx addr = XEXP (operands[0], 0);
      if (GET_CODE (addr) == POST_INC || GET_CODE (addr) == POST_DEC)
	{
	  rtx high_reg = gen_rtx (SUBREG, SImode, operands[1], 0);

	  operands[0] = XEXP (addr, 0);
	  if (GET_CODE (operands[1]) != REG || GET_CODE (operands[0]) != REG)
	    abort ();

	  if (!reg_overlap_mentioned_p (high_reg, addr))
	    {
	      /* No overlap between high target register and address
		 register.  (We do this in a non-obvious way to
		 save a register file writeback)  */
	      if (GET_CODE (addr) == POST_INC)
		return "stws,ma %1,8(0,%0)\n\tstw %R1,-4(0,%0)";
	      return "stws,ma %1,-8(0,%0)\n\tstw %R1,12(0,%0)";
	    }
	  else
	    abort();
	}
      else if (GET_CODE (addr) == PRE_INC || GET_CODE (addr) == PRE_DEC)
	{
	  rtx high_reg = gen_rtx (SUBREG, SImode, operands[1], 0);

	  operands[0] = XEXP (addr, 0);
	  if (GET_CODE (operands[1]) != REG || GET_CODE (operands[0]) != REG)
	    abort ();

	  if (!reg_overlap_mentioned_p (high_reg, addr))
	    {
	      /* No overlap between high target register and address
		 register.  (We do this in a non-obvious way to
		 save a register file writeback)  */
	      if (GET_CODE (addr) == PRE_INC)
		return "stws,mb %1,8(0,%0)\n\tstw %R1,4(0,%0)";
	      return "stws,mb %1,-8(0,%0)\n\tstw %R1,4(0,%0)";
	    }
	  else
	    abort();
	}
    }
  if (optype1 == MEMOP)
    {
      /* We have to output the address syntax ourselves, since print_operand
	 doesn't deal with the addresses we want to use.  Fix this later.  */

      rtx addr = XEXP (operands[1], 0);
      if (GET_CODE (addr) == POST_INC || GET_CODE (addr) == POST_DEC)
	{
	  rtx high_reg = gen_rtx (SUBREG, SImode, operands[0], 0);

	  operands[1] = XEXP (addr, 0);
	  if (GET_CODE (operands[0]) != REG || GET_CODE (operands[1]) != REG)
	    abort ();

	  if (!reg_overlap_mentioned_p (high_reg, addr))
	    {
	      /* No overlap between high target register and address
		 register.  (We do this in a non-obvious way to
		 save a register file writeback)  */
	      if (GET_CODE (addr) == POST_INC)
		return "ldws,ma 8(0,%1),%0\n\tldw -4(0,%1),%R0";
	      return "ldws,ma -8(0,%1),%0\n\tldw 12(0,%1),%R0";
	    }
	  else
	    {
	      /* This is an undefined situation.  We should load into the
		 address register *and* update that register.  Probably
		 we don't need to handle this at all.  */
	      if (GET_CODE (addr) == POST_INC)
		return "ldw 4(0,%1),%R0\n\tldws,ma 8(0,%1),%0";
	      return "ldw 4(0,%1),%R0\n\tldws,ma -8(0,%1),%0";
	    }
	}
      else if (GET_CODE (addr) == PRE_INC || GET_CODE (addr) == PRE_DEC)
	{
	  rtx high_reg = gen_rtx (SUBREG, SImode, operands[0], 0);

	  operands[1] = XEXP (addr, 0);
	  if (GET_CODE (operands[0]) != REG || GET_CODE (operands[1]) != REG)
	    abort ();

	  if (!reg_overlap_mentioned_p (high_reg, addr))
	    {
	      /* No overlap between high target register and address
		 register.  (We do this in a non-obvious way to
		 save a register file writeback)  */
	      if (GET_CODE (addr) == PRE_INC)
		return "ldws,mb 8(0,%1),%0\n\tldw 4(0,%1),%R0";
	      return "ldws,mb -8(0,%1),%0\n\tldw 4(0,%1),%R0";
	    }
	  else
	    {
	      /* This is an undefined situation.  We should load into the
		 address register *and* update that register.  Probably
		 we don't need to handle this at all.  */
	      if (GET_CODE (addr) == PRE_INC)
		return "ldw 12(0,%1),%R0\n\tldws,mb 8(0,%1),%0";
	      return "ldw -4(0,%1),%R0\n\tldws,mb -8(0,%1),%0";
	    }
	}
    }

  /* If an operand is an unoffsettable memory ref, find a register
     we can increment temporarily to make it refer to the second word.  */

  if (optype0 == MEMOP)
    addreg0 = find_addr_reg (XEXP (operands[0], 0));

  if (optype1 == MEMOP)
    addreg1 = find_addr_reg (XEXP (operands[1], 0));

  /* Ok, we can do one word at a time.
     Normally we do the low-numbered word first.

     In either case, set up in LATEHALF the operands to use
     for the high-numbered word and in some cases alter the
     operands in OPERANDS to be suitable for the low-numbered word.  */

  if (optype0 == REGOP)
    latehalf[0] = gen_rtx (REG, SImode, REGNO (operands[0]) + 1);
  else if (optype0 == OFFSOP)
    latehalf[0] = adj_offsettable_operand (operands[0], 4);
  else
    latehalf[0] = operands[0];

  if (optype1 == REGOP)
    latehalf[1] = gen_rtx (REG, SImode, REGNO (operands[1]) + 1);
  else if (optype1 == OFFSOP)
    latehalf[1] = adj_offsettable_operand (operands[1], 4);
  else if (optype1 == CNSTOP)
    split_double (operands[1], &operands[1], &latehalf[1]);
  else
    latehalf[1] = operands[1];

  /* If the first move would clobber the source of the second one,
     do them in the other order.

     RMS says "This happens only for registers;
     such overlap can't happen in memory unless the user explicitly
     sets it up, and that is an undefined circumstance."

     but it happens on the HP-PA when loading parameter registers,
     so I am going to define that circumstance, and make it work
     as expected.  */

  if (optype0 == REGOP && (optype1 == MEMOP || optype1 == OFFSOP)
	   && reg_overlap_mentioned_p (operands[0], XEXP (operands[1], 0)))
    {
      /* XXX THIS PROBABLY DOESN'T WORK.  */
      /* Do the late half first.  */
      if (addreg1)
	output_asm_insn ("ldo 4(%0),%0", &addreg1);
      output_asm_insn (singlemove_string (latehalf), latehalf);
      if (addreg1)
	output_asm_insn ("ldo -4(%0),%0", &addreg1);
      /* Then clobber.  */
      return singlemove_string (operands);
    }

  if (optype0 == REGOP && optype1 == REGOP
      && REGNO (operands[0]) == REGNO (operands[1]) + 1)
    {
      output_asm_insn (singlemove_string (latehalf), latehalf);
      return singlemove_string (operands);
    }

  /* Normal case: do the two words, low-numbered first.  */

  output_asm_insn (singlemove_string (operands), operands);

  /* Make any unoffsettable addresses point at high-numbered word.  */
  if (addreg0)
    output_asm_insn ("ldo 4(%0),%0", &addreg0);
  if (addreg1)
    output_asm_insn ("ldo 4(%0),%0", &addreg1);

  /* Do that word.  */
  output_asm_insn (singlemove_string (latehalf), latehalf);

  /* Undo the adds we just did.  */
  if (addreg0)
    output_asm_insn ("ldo -4(%0),%0", &addreg0);
  if (addreg1)
    output_asm_insn ("ldo -4(%0),%0", &addreg1);

  return "";
}

char *
output_fp_move_double (operands)
     rtx *operands;
{
  if (FP_REG_P (operands[0]))
    {
      if (FP_REG_P (operands[1]))
	output_asm_insn ("fcpy,dbl %1,%0", operands);
      else if (GET_CODE (operands[1]) == REG)
	{
	  rtx xoperands[3];
	  xoperands[0] = operands[0];
	  xoperands[1] = operands[1];
	  xoperands[2] = gen_rtx (REG, SImode, REGNO (operands[1]) + 1);
	  output_asm_insn
	    ("stw %1,-16(0,30)\n\tstw %2,-12(0,30)\n\tfldds -16(0,30),%0",
			   xoperands);
	}
      else 
	output_asm_insn ("fldds%F1 %1,%0", operands);
    }
  else if (FP_REG_P (operands[1]))
    {
      if (GET_CODE (operands[0]) == REG)
	{
	  rtx xoperands[3];
	  xoperands[2] = operands[1];
	  xoperands[1] = gen_rtx (REG, SImode, REGNO (operands[0]) + 1);
	  xoperands[0] = operands[0];
	  output_asm_insn
	    ("fstds %2,-16(0,30)\n\tldw -12(0,30),%1\n\tldw -16(0,30),%0",
	     xoperands);
	}
      else
	output_asm_insn ("fstds%F0 %1,%0", operands);
    }
  else abort ();
  return "";
}

/* Return a REG that occurs in ADDR with coefficient 1.
   ADDR can be effectively incremented by incrementing REG.  */

static rtx
find_addr_reg (addr)
     rtx addr;
{
  while (GET_CODE (addr) == PLUS)
    {
      if (GET_CODE (XEXP (addr, 0)) == REG)
	addr = XEXP (addr, 0);
      else if (GET_CODE (XEXP (addr, 1)) == REG)
	addr = XEXP (addr, 1);
      else if (CONSTANT_P (XEXP (addr, 0)))
	addr = XEXP (addr, 1);
      else if (CONSTANT_P (XEXP (addr, 1)))
	addr = XEXP (addr, 0);
      else
	abort ();
    }
  if (GET_CODE (addr) == REG)
    return addr;
  abort ();
}

/* Emit code to perform a block move.

   Restriction: If the length argument is non-constant, alignment
   must be 4.

   OPERANDS[0] is the destination pointer as a REG, clobbered.
   OPERANDS[1] is the source pointer as a REG, clobbered.
   if SIZE_IS_CONSTANT
     OPERANDS[2] is a register for temporary storage.
     OPERANDS[4] is the size as a CONST_INT
   else
     OPERANDS[2] is a REG which will contain the size, clobbered.
   OPERANDS[3] is a register for temporary storage.
   OPERANDS[5] is the alignment safe to use, as a CONST_INT.  */

char *
output_block_move (operands, size_is_constant)
     rtx *operands;
     int size_is_constant;
{
  int align = INTVAL (operands[5]);
  unsigned long n_bytes;

  /* We can't move more than four bytes at a time because the PA
     has no longer integer move insns.  (Could use fp mem ops?)  */
  if (align > 4)
    align = 4;

  if (size_is_constant)
    {
      unsigned long n_items;
      unsigned long offset;
      rtx temp;

      n_bytes = INTVAL (operands[4]);
      if (n_bytes == 0)
	return "";

      if (align >= 4)
	{
	  /* Don't unroll too large blocks.  */
	  if (n_bytes > 64)
	    goto copy_with_loop;

	  /* Read and store using two registers, and hide latency
	     by deferring the stores until three instructions after
	     the corresponding load.  The last load insn will read
	     the entire word were the last bytes are, possibly past
	     the end of the source block, but since loads are aligned,
	     this is harmless.  */

	  output_asm_insn ("ldws,ma 4(0,%1),%2", operands);

	  for (offset = 4; offset < n_bytes; offset += 4)
	    {
	      output_asm_insn ("ldws,ma 4(0,%1),%3", operands);
	      output_asm_insn ("stws,ma %2,4(0,%0)", operands);

	      temp = operands[2];
	      operands[2] = operands[3];
	      operands[3] = temp;
	    }
	  if (n_bytes % 4 == 0)
	    /* Store the last word.  */
	    output_asm_insn ("stw %2,0(0,%0)", operands);
	  else
	    {
	      /* Store the last, partial word.  */
	      operands[4] = gen_rtx (CONST_INT, VOIDmode, n_bytes % 4);
	      output_asm_insn ("stbys,e %2,%4(0,%0)", operands);
	    }
	  return "";
	}

      if (align >= 2 && n_bytes >= 2)
	{
	  output_asm_insn ("ldhs,ma 2(0,%1),%2", operands);

	  for (offset = 2; offset + 2 <= n_bytes; offset += 2)
	    {
	      output_asm_insn ("ldhs,ma 2(0,%1),%3", operands);
	      output_asm_insn ("sths,ma %2,2(0,%0)", operands);

	      temp = operands[2];
	      operands[2] = operands[3];
	      operands[3] = temp;
	    }
	  if (n_bytes % 2 != 0)
	    output_asm_insn ("ldb 0(0,%1),%3", operands);

	  output_asm_insn ("sths,ma %2,2(0,%0)", operands);

	  if (n_bytes % 2 != 0)
	    output_asm_insn ("stb %3,0(0,%0)", operands);

	  return "";
	}

      output_asm_insn ("ldbs,ma 1(0,%1),%2", operands);

      for (offset = 1; offset + 1 <= n_bytes; offset += 1)
	{
	  output_asm_insn ("ldbs,ma 1(0,%1),%3", operands);
	  output_asm_insn ("stbs,ma %2,1(0,%0)", operands);

	  temp = operands[2];
	  operands[2] = operands[3];
	  operands[3] = temp;
	}
      output_asm_insn ("stb %2,0(0,%0)", operands);

      return "";
    }

  if (align != 4)
    abort();
     
 copy_with_loop:

  if (size_is_constant)
    {
      /* Size is compile-time determined, and also not
	 very small (such small cases are handled above).  */
      operands[4] = gen_rtx (CONST_INT, VOIDmode, n_bytes - 4);
      output_asm_insn ("ldo %4(0),%2", operands);
    }
  else
    {
      /* Decrement counter by 4, and if it becomes negative, jump past the
	 word copying loop.  */
      output_asm_insn ("addib,<,n -4,%2,.+16", operands);
    }

  /* Copying loop.  Note that the first load is in the annulled delay slot
     of addib.  Is it OK on PA to have a load in a delay slot, i.e. is a
     possible page fault stopped in time?  */
  output_asm_insn ("ldws,ma 4(0,%1),%3", operands);
  output_asm_insn ("addib,>= -4,%2,.-4", operands);
  output_asm_insn ("stws,ma %3,4(0,%0)", operands);

  /* The counter is negative, >= -4.  The remaining number of bytes are
     determined by the two least significant bits.  */

  if (size_is_constant)
    {
      if (n_bytes % 4 != 0)
	{
	  /* Read the entire word of the source block tail.  */
	  output_asm_insn ("ldw 0(0,%1),%3", operands);
	  operands[4] = gen_rtx (CONST_INT, VOIDmode, n_bytes % 4);
	  output_asm_insn ("stbys,e %3,%4(0,%0)", operands);
	}
    }
  else
    {
      /* Add 4 to counter.  If it becomes zero, we're done.  */
      output_asm_insn ("addib,=,n 4,%2,.+16", operands);

      /* Read the entire word of the source block tail.  (Also this
	 load is in an annulled delay slot.)  */
      output_asm_insn ("ldw 0(0,%1),%3", operands);

      /* Make %0 point at the first byte after the destination block.  */
      output_asm_insn ("add %2,%0,%0", operands);
      /* Store the leftmost bytes, up to, but not including, the address
	 in %0.  */
      output_asm_insn ("stbys,e %3,0(0,%0)", operands);
    }
  return "";
}


char *
output_and (operands)
     rtx *operands;
{
  if (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) != 0)
    {
      unsigned mask = INTVAL (operands[2]);
      int ls0, ls1, ms0, p, len;

      for (ls0 = 0; ls0 < 32; ls0++)
	if ((mask & (1 << ls0)) == 0)
	  break;

      for (ls1 = ls0; ls1 < 32; ls1++)
	if ((mask & (1 << ls1)) != 0)
	  break;

      for (ms0 = ls1; ms0 < 32; ms0++)
	if ((mask & (1 << ms0)) == 0)
	  break;

      if (ms0 != 32)
	abort();

      if (ls1 == 32)
	{
	  len = ls0;

	  if (len == 0)
	    abort ();

	  operands[2] = gen_rtx (CONST_INT, VOIDmode, len);
	  return "extru %1,31,%2,%0";
	}
      else
	{
	  /* We could use this `depi' for the case above as well, but `depi'
	     requires one more register file access than an `extru'.  */

	  p = 31 - ls0;
	  len = ls1 - ls0;

	  operands[2] = gen_rtx (CONST_INT, VOIDmode, p);
	  operands[3] = gen_rtx (CONST_INT, VOIDmode, len);
	  return "depi 0,%2,%3,%0";
	}
    }
  else
    return "and %1,%2,%0";
}

char *
output_ior (operands)
     rtx *operands;
{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      unsigned mask = INTVAL (operands[2]);
      int bs0, bs1, bs2, p, len;
 
      if (INTVAL (operands[2]) == 0)
	return "copy %1,%0";

      for (bs0 = 0; bs0 < 32; bs0++)
	if ((mask & (1 << bs0)) != 0)
	  break;

      for (bs1 = bs0; bs1 < 32; bs1++)
	if ((mask & (1 << bs1)) == 0)
	  break;

      if (bs1 != 32 && ((unsigned) 1 << bs1) <= mask)
	abort();

      p = 31 - bs0;
      len = bs1 - bs0;

      operands[2] = gen_rtx (CONST_INT, VOIDmode, p);
      operands[3] = gen_rtx (CONST_INT, VOIDmode, len);
      return "depi -1,%2,%3,%0";
    }
  else
    return "or %1,%2,%0";
}

/* Output an ascii string.  */
output_ascii (file, p, size)
     FILE *file;
     unsigned char *p;
     int size;
{
  int i;
  int chars_output;
  unsigned char partial_output[16];	/* Max space 4 chars can occupy.   */

  /* The HP assembler can only take strings of 256 characters at one
     time.  This is a limitation on input line length, *not* the
     length of the string.  Sigh.  Even worse, it seems that the
     restriction is in number of input characters (see \xnn &
     \whatever).  So we have to do this very carefully.  */

  fprintf (file, "\t.STRING \"");

  chars_output = 0;
  for (i = 0; i < size; i += 4)
    {
      int co = 0;
      int io = 0;
      for (io = 0, co = 0; io < MIN (4, size - i); io++)
	{
	  register unsigned int c = p[i + io];

	  if (c == '\"' || c == '\\')
	    partial_output[co++] = '\\';
	  if (c >= ' ' && c < 0177)
	    partial_output[co++] = c;
	  else
	    {
	      unsigned int hexd;
	      partial_output[co++] = '\\';
	      partial_output[co++] = 'x';
	      hexd =  c  / 16 - 0 + '0';
	      if (hexd > '9')
		hexd -= '9' - 'a' + 1;
	      partial_output[co++] = hexd;
	      hexd =  c % 16 - 0 + '0';
	      if (hexd > '9')
		hexd -= '9' - 'a' + 1;
	      partial_output[co++] = hexd;
	    }
	}
      if (chars_output + co > 243)
	{
	  fprintf (file, "\"\n\t.STRING \"");
	  chars_output = 0;
	}
      fwrite (partial_output, 1, co, file);
      chars_output += co;
      co = 0;
    }
  fprintf (file, "\"\n");
}

/* You may have trouble believing this, but this is the HP825 stack
   layout.  Wow.

   Offset		Contents

   Variable arguments	(optional; any number may be allocated)

   SP-(4*(N+9))		arg word N
   	:		    :
      SP-56		arg word 5
      SP-52		arg word 4

   Fixed arguments	(must be allocated; may remain unused)

      SP-48		arg word 3
      SP-44		arg word 2
      SP-40		arg word 1
      SP-36		arg word 0

   Frame Marker

      SP-32		External Data Pointer (DP)
      SP-28		External sr4
      SP-24		External/stub RP (RP')
      SP-20		Current RP
      SP-16		Static Link
      SP-12		Clean up
      SP-8		Calling Stub RP (RP'')
      SP-4		Previous SP

   Top of Frame

      SP-0		Stack Pointer (points to next available address)

*/

/* This function saves registers as follows.  Registers marked with ' are
   this function's registers (as opposed to the previous function's).
   If a frame_pointer isn't needed, r4 is saved as a general register;
   the space for the frame pointer is still allocated, though, to keep
   things simple.


   Top of Frame

       SP (FP')		Previous FP
       SP + 4		Alignment filler (sigh)
       SP + 8		Space for locals reserved here.
       .
       .
       .
       SP + n		All call saved register used.
       .
       .
       .
       SP + o		All call saved fp registers used.
       .
       .
       .
       SP + p (SP')	points to next available address.
       
*/

/* Helper functions */
void
print_stw (file, r, disp, base)
     FILE *file;
     int r, disp, base;
{
  if (VAL_14_BITS_P (disp))
    fprintf (file, "\tstw %d,%d(0,%d)\n", r, disp, base);
  else
    fprintf (file, "\taddil L'%d,%d\n\tstw %d,R'%d(0,1)\n", disp, base,
	     r, disp);
}

void
print_ldw (file, r, disp, base)
     FILE *file;
     int r, disp, base;
{
  if (VAL_14_BITS_P (disp))
    fprintf (file, "\tldw %d(0,%d),%d\n", disp, base, r);
  else
    fprintf (file, "\taddil L'%d,%d\n\tldw R'%d(0,1),%d\n", disp, base,
	     disp, r);
}

/* Global variables set by FUNCTION_PROLOGUE.  */
/* Size of frame.  Need to know this to emit return insns from
   leaf procedures.  */
int apparent_fsize;
int actual_fsize;
int local_fsize, save_fregs;

int
compute_frame_size (size, leaf_function, fregs_live)
     int size;
     int leaf_function;
     int *fregs_live;
{
  extern int current_function_outgoing_args_size;
  int i;

  /* 8 is space for frame pointer + filler */
  local_fsize = actual_fsize = size + 8;

  /* fp is stored in a special place. */
  for (i = 18; i >= 5; i--)
    if (regs_ever_live[i])
      actual_fsize += 4;

  if (regs_ever_live[3])
    actual_fsize += 4;
  actual_fsize = (actual_fsize + 7) & ~7;

  if (!TARGET_SNAKE)
    {
      for (i = 47; i >= 44; i--)
	if (regs_ever_live[i])
	  {
	    actual_fsize += 8;
	    if (fregs_live)
	      *fregs_live = 1;
	  }
    }
  else
    {
      for (i = 90; i >= 72; i -= 2)
	if (regs_ever_live[i] || regs_ever_live[i + 1])
	  {
	    actual_fsize += 8;
	    if (fregs_live)
	      *fregs_live = 1;
	  }
    }
  return actual_fsize + current_function_outgoing_args_size;
}
     
void
output_function_prologue (file, size, leaf_function)
     FILE *file;
     int size;
     int leaf_function;
{
  extern char call_used_regs[];
  extern int frame_pointer_needed;
  extern int current_function_returns_struct;
  int i, offset;

  save_fregs = 0;
  actual_fsize = compute_frame_size (size, leaf_function, &save_fregs) + 32;
  if (TARGET_SNAKE)
    actual_fsize = (actual_fsize + 63) & ~63;

  /* Let's not try to bullshit more than we need to here. */
  /* This might be right a lot of the time */
  fprintf (file, "\t.PROC\n\t.CALLINFO FRAME=%d", actual_fsize);
    if (regs_ever_live[2] || profile_flag)
      fprintf (file, ",CALLS,SAVE_RP\n");
    else
      fprintf (file, ",NO_CALLS\n");
  fprintf (file, "\t.ENTRY\n");

  /* Some registers have places to go in the current stack
     structure.  */

  if (regs_ever_live[2] || profile_flag)
    fprintf (file, "\tstw 2,-20(0,30)\n");

  /* Reserve space for local variables.  */
  if (frame_pointer_needed)
    {
      if (VAL_14_BITS_P (actual_fsize))
	fprintf (file, "\tcopy 4,1\n\tcopy 30,4\n\tstwm 1,%d(0,30)\n",
		 actual_fsize);
      else
	{
	  fprintf (file, "\tcopy 4,1\n\tcopy 30,4\n\tstw 1,0(0,4)\n");
	  fprintf (file, "\taddil L'%d,30\n\tldo R'%d(1),30\n",
		   actual_fsize, actual_fsize);
	}
    }
  else
    /* Used to be abort ();  */
    {
      if (VAL_14_BITS_P (actual_fsize))
	fprintf (file, "\tldo %d(30),30\n", actual_fsize);
      else
	fprintf (file, "\taddil L'%d,30\n\tldo R'%d(1),30\n",
		 actual_fsize, actual_fsize);
    }
  /* The hppa calling conventions say that that %r19, the pic offset 
     register, is saved at sp - 32 (in this function's frame) */
  if (flag_pic)
    {
      fprintf (file, "\tstw %%r19,-32(%%r30)\n");
    }
  /* Instead of taking one argument, the counter label, as most normal
     mcounts do, _mcount appears to behave differently on the HPPA. It
     takes the return address of the caller, the address of this
     routine, and the address of the label. Also, it isn't magic, so
     argument registers have to be preserved. */

  if (profile_flag)
    {
      unsigned int pc_offset =
	(4 + (frame_pointer_needed
	      ? (VAL_14_BITS_P (actual_fsize) ? 12 : 20)
	      : (VAL_14_BITS_P (actual_fsize) ? 4 : 8)));
      int i, arg_offset;
      int basereg, offsetadj;

      /* When the function has a frame pointer, use that as the base 
	 register for saving/restoring registers.  Else use the stack
	 pointer.  Adjust the offset according to the frame size if this
	 function does not have a frame pointer.  */

      basereg = frame_pointer_needed ? FRAME_POINTER_REGNUM
				     : STACK_POINTER_REGNUM;
      offsetadj = frame_pointer_needed ? 0 : actual_fsize;

      if (current_function_returns_struct)
	print_stw (file, STRUCT_VALUE_REGNUM, - 12 - offsetadj, basereg);
      for (i = 26, arg_offset = -36 - offsetadj; i >= 23; i--, arg_offset -= 4)
	if (regs_ever_live[i])
	  {
	    print_stw (file, i, arg_offset, basereg);
	    /* It is possible for the arg_offset not to fit in 14 bits 
               when profiling a function without a frame pointer.  Deal
	       with such cases.  */
	    pc_offset += VAL_14_BITS_P (arg_offset) ? 4 : 8;
	  }
      fprintf (file,
	       "\tcopy %%r2,%%r26\n\taddil L'LP$%04d-$global$,%%r27\n\
\tldo R'LP$%04d-$global$(%%r1),%%r24\n\tbl _mcount,%%r2\n\
\tldo %d(%%r2),%%r25\n",
	       hp_profile_labelno, hp_profile_labelno, -pc_offset - 12 - 8);
      for (i = 26, arg_offset = -36 - offsetadj; i >= 23; i--, arg_offset -= 4)
	if (regs_ever_live[i])
	  print_ldw (file, i, arg_offset, basereg);
      if (current_function_returns_struct)
	print_ldw (file, STRUCT_VALUE_REGNUM, - 12 - offsetadj, basereg);
    }

  /* Normal register save. */
  if (frame_pointer_needed)
    {
      for (i = 18, offset = local_fsize; i >= 5; i--)
	if (regs_ever_live[i] && ! call_used_regs[i])
	  {
	    print_stw (file, i, offset, 4);  offset += 4;
	  }
      if (regs_ever_live[3] && ! call_used_regs[3])
	{
	  print_stw (file, 3, offset, 4);  offset += 4;
	}
    }
  else
    {
      for (i = 18, offset = local_fsize - actual_fsize; i >= 5; i--)
      	if (regs_ever_live[i] && ! call_used_regs[i])
	  {
	    print_stw (file, i, offset, 30);  offset += 4;
	  }
      if (regs_ever_live[3] && ! call_used_regs[3])
	{
	  print_stw (file, 3, offset, 30);  offset += 4;
	}
    }
      
  /* Align pointer properly (doubleword boundary).  */
  offset = (offset + 7) & ~7;

  /* Floating point register store.  */
  if (save_fregs)
    if (frame_pointer_needed)
      {
	if (VAL_14_BITS_P (offset))
	  fprintf (file, "\tldo %d(4),1\n", offset);
	else
	  fprintf (file, "\taddil L'%d,4\n\tldo R'%d(1),1\n", offset, offset);
      }
    else
      {
	if (VAL_14_BITS_P (offset))
	  fprintf (file, "\tldo %d(30),1\n", offset);
	else
	  fprintf (file, "\taddil L'%d,30\n\tldo R'%d(1),1\n", offset, offset);
      }
  if (!TARGET_SNAKE)
    {
      for (i = 47; i >= 44; i--)
	{
	  if (regs_ever_live[i])
	    fprintf (file, "\tfstds,ma %s,8(0,1)\n", reg_names[i]);
	}
    }
  else
    {
      for (i = 90; i >= 72; i -= 2)
	if (regs_ever_live[i] || regs_ever_live[i + 1])
	  {
	    fprintf (file, "\tfstds,ma %s,8(0,1)\n", reg_names[i]);
	  }
    }
}

void
output_function_epilogue (file, size, leaf_function)
     FILE *file;
     int size;
     int leaf_function;
{
  extern char call_used_regs[];
  extern int frame_pointer_needed;
  int  i, offset;

  if (frame_pointer_needed)
    {
      for (i = 18, offset = local_fsize; i >= 5; i--)
	if (regs_ever_live[i] && ! call_used_regs[i])
	  {
	    print_ldw (file, i, offset, 4);  offset += 4;
	  }
      if (regs_ever_live[3] && ! call_used_regs[3])
	{
	  print_ldw (file, 3, offset, 4);  offset += 4;	  
	}
    }
  else
    {
      for (i = 18, offset = local_fsize - actual_fsize; i >= 5; i--)
      	if (regs_ever_live[i] && ! call_used_regs[i])
	  {
	    print_ldw (file, i, offset, 30);  offset += 4;
	  }
      if (regs_ever_live[3] && ! call_used_regs[3])
	{
	  print_ldw (file, 3, offset, 30);  offset += 4;
	}
    }
      
  /* Align pointer properly (doubleword boundary).  */
  offset = (offset + 7) & ~7;

  /* Floating point register restore.  */
  if (save_fregs)
    if (frame_pointer_needed)
      {
	if (VAL_14_BITS_P (offset))
	  fprintf (file, "\tldo %d(4),1\n", offset);
	else
	  fprintf (file, "\taddil L'%d,4\n\tldo R'%d(1),1\n", offset, offset);
      }
    else
      {
	if (VAL_14_BITS_P (offset))
	  fprintf (file, "\tldo %d(30),1\n", offset);
	else
	  fprintf (file, "\taddil L'%d,30\n\tldo R'%d(1),1\n", offset, offset);
      }
  if (!TARGET_SNAKE)
    {
      for (i = 47; i >= 44; i--)
	{
	  if (regs_ever_live[i])
	    fprintf (file, "\tfldds,ma 8(0,1),%s\n", reg_names[i]);
	}
    }
  else
    {
      for (i = 90; i >= 72; i -= 2)
	if (regs_ever_live[i] || regs_ever_live[i + 1])
	  {
	    fprintf (file, "\tfldds,ma 8(0,1),%s\n", reg_names[i]);
	  }
    }
  /* Reset stack pointer (and possibly frame pointer).  The stack */
  /* pointer is initially set to fp + 8 to avoid a race condition. */
  if (frame_pointer_needed)
    {
      fprintf (file, "\tldo 8(4),30\n");
      if (regs_ever_live[2] || profile_flag)
	fprintf (file, "\tldw -28(0,30),2\n");
      fprintf (file, "\tbv 0(2)\n\tldwm -8(30),4\n");
    }
  else if (actual_fsize)
    {
      if ((regs_ever_live[2] || profile_flag)
          && VAL_14_BITS_P (actual_fsize + 20))
	fprintf (file, "\tldw %d(30),2\n\tbv 0(2)\n\tldo %d(30),30\n",
		 -(actual_fsize + 20), -actual_fsize);
      else if (regs_ever_live[2] || profile_flag)
	fprintf (file,
		 "\taddil L'%d,30\n\tldw %d(1),2\n\tbv 0(2)\n\tldo R'%d(1),30\n",
		 - actual_fsize,
		 - (actual_fsize + 20 + ((-actual_fsize) & ~0x7ff)),
		 /* - ((actual_fsize + 20) - (actual_fsize & ~0x7ff)), */
		 - actual_fsize);
      else if (VAL_14_BITS_P (actual_fsize))
	fprintf (file, "\tbv 0(2)\n\tldo %d(30),30\n", - actual_fsize);
      else
	fprintf (file, "\taddil L'%d,30\n\tbv 0(2)\n\tldo R'%d(1),30\n");
    }
  else if (current_function_epilogue_delay_list)
    {
      fprintf (file, "\tbv 0(2)\n");
      final_scan_insn (XEXP (current_function_epilogue_delay_list, 0),
		       file, write_symbols, 1, 0, 1);
    }
  else
    fprintf (file, "\tbv,n 0(2)\n");
  fprintf (file, "\t.EXIT\n\t.PROCEND\n");
}

rtx
gen_compare_reg (code, x, y)
     enum rtx_code code;
     rtx x, y;
{
  enum machine_mode mode = SELECT_CC_MODE (code, x, y);
  rtx cc_reg = gen_rtx (REG, mode, 0);

  emit_insn (gen_rtx (SET, VOIDmode, cc_reg,
		      gen_rtx (COMPARE, mode, x, y)));

  return cc_reg;
}

/* Return nonzero if TRIAL can go into the function epilogue's
   delay slot.  SLOT is the slot we are trying to fill.  */

int
eligible_for_epilogue_delay (trial, slot)
     rtx trial;
     int slot;
{
  if (slot >= 1)
    return 0;
  if (GET_CODE (trial) != INSN
      || GET_CODE (PATTERN (trial)) != SET)
    return 0;
  if (get_attr_length (trial) != 1)
    return 0;
  return (leaf_function &&
	  get_attr_in_branch_delay (trial) == IN_BRANCH_DELAY_TRUE);
}

rtx
gen_scond_fp (code, operand0)
     enum rtx_code code;
     rtx operand0;
{
  return gen_rtx (SET, VOIDmode, operand0,
		  gen_rtx (code, CCFPmode,
			   gen_rtx (REG, CCFPmode, 0), const0_rtx));
}

void
emit_bcond_fp (code, operand0)
     enum rtx_code code;
     rtx operand0;
{
  emit_jump_insn (gen_rtx (SET, VOIDmode, pc_rtx,
			   gen_rtx (IF_THEN_ELSE, VOIDmode,
				    gen_rtx (code, VOIDmode, 
					     gen_rtx (REG, CCFPmode, 0),
					     const0_rtx),
				    gen_rtx (LABEL_REF, VOIDmode, operand0),
				    pc_rtx)));

}

rtx
gen_cmp_fp (code, operand0, operand1)
     enum rtx_code code;
     rtx operand0, operand1;
{
  return gen_rtx (SET, VOIDmode, gen_rtx (REG, CCFPmode, 0),
		  gen_rtx (code, CCFPmode, operand0, operand1));
}


/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.  */

void
print_operand (file, x, code)
     FILE *file;
     rtx x;
     int code;
{
  switch (code)
    {
    case '#':
      /* Output a 'nop' if there's nothing for the delay slot.  */
      if (dbr_sequence_length () == 0)
	fputs ("\n\tnop", file);
      return;
    case '*':
      /* Output an nullification completer if there's nothing for the */
      /* delay slot or nullification is requested.  */ 
      if (dbr_sequence_length () == 0 ||
	  (final_sequence &&
	   INSN_ANNULLED_BRANCH_P (XVECEXP (final_sequence, 0, 0))))
        fputs (",n", file);
      return;
    case 'R':
      /* Print out the second register name of a register pair.
	 I.e., R (6) => 7.  */
      fputs (reg_names[REGNO (x)+1], file);
      return;
    case 'r':
      /* A register or zero. */
      if (x == const0_rtx)
	{
	  fputs ("0", file);
	  return;
	}
      else
	break;
    case 'O':
      switch (GET_CODE (x))
	{
	case PLUS:
	  fprintf (file, "add%s",
		   GET_CODE (XEXP (x, 1)) == CONST_INT ? "i" : "");  break;
	case MINUS:
	  fprintf (file, "sub%s",
		   GET_CODE (XEXP (x, 0)) == CONST_INT ? "i" : "");  break;
	case AND:
	  fprintf (file, "and%s",
		   GET_CODE (XEXP (x, 1)) == NOT ? "cm" : "");  break;
	case IOR:
	  fprintf (file, "or");  break;
	case XOR:
	  fprintf (file, "xor");  break;
	case ASHIFT:
	  fprintf (file, "sh%dadd", INTVAL (XEXP (x, 1)));  break;
	  /* Too lazy to handle bitfield conditions yet.  */
	default:
	  printf ("Can't grok '%c' operator:\n", code);
	  debug_rtx (x);
	  abort ();
	}
      return;
    case 'C':
    case 'X':
      switch (GET_CODE (x))
	{	
	case EQ:
	  fprintf (file, "=");  break;
	case NE:
	  if (code == 'C')
	    fprintf (file, "<>");
	  else
	    fprintf (file, "!=");
	  break;
	case GT:
	  fprintf (file, ">");  break;
	case GE:
	  fprintf (file, ">=");  break;
	case GEU:
	  fprintf (file, ">>=");  break;
	case GTU:
	  fprintf (file, ">>");  break;
	case LT:
	  fprintf (file, "<");  break;
	case LE:
	  fprintf (file, "<=");  break;
	case LEU:
	  fprintf (file, "<<=");  break;
	case LTU:
	  fprintf (file, "<<");  break;
	default:
	  printf ("Can't grok '%c' operator:\n", code);
	  debug_rtx (x);
	  abort ();
	}
      return;
    case 'N':
    case 'Y':
      switch (GET_CODE (x))
	{
	case EQ:
	  if (code == 'N')
	    fprintf (file, "<>");
	  else
	    fprintf (file, "!=");
	  break;
	case NE:
	  fprintf (file, "=");  break;
	case GT:
	  fprintf (file, "<=");  break;
	case GE:
	  fprintf (file, "<");  break;
	case GEU:
	  fprintf (file, "<<");  break;
	case GTU:
	  fprintf (file, "<<=");  break;
	case LT:
	  fprintf (file, ">=");  break;
	case LE:
	  fprintf (file, ">");  break;
	case LEU:
	  fprintf (file, ">>");  break;
	case LTU:
	  fprintf (file, ">>=");  break;
	default:
	  printf ("Can't grok '%c' operator:\n", code);
	  debug_rtx (x);
	  abort ();
	}
      return;
    case 'M':
      switch (GET_CODE (XEXP (x, 0)))
	{
	case PRE_DEC:
	case PRE_INC:
	  fprintf (file, "s,mb");
	  break;
	case POST_DEC:
	case POST_INC:
	  fprintf (file, "s,ma");
	  break;
	default:
	  break;
	}
      return;
    case 'F':
      switch (GET_CODE (XEXP (x, 0)))
	{
	case PRE_DEC:
	case PRE_INC:
	  fprintf (file, ",mb");
	  break;
	case POST_DEC:
	case POST_INC:
	  fprintf (file, ",ma");
	  break;
	default:
	  break;
	}
      return;
    case 'G':
      output_global_address (file, x);
      return;
    case 0:			/* Don't do anything special */
      break;
    default:
      abort ();
    }
  if (GET_CODE (x) == REG)
    fprintf (file, "%s", reg_names [REGNO (x)]);
  else if (GET_CODE (x) == MEM)
    {
      int size = GET_MODE_SIZE (GET_MODE (x));
      rtx base = XEXP (XEXP (x, 0), 0);
      switch (GET_CODE (XEXP (x, 0)))
	{
	case PRE_DEC:
	case POST_DEC:
	  fprintf (file, "-%d(0,%s)", size, reg_names [REGNO (base)]);
	  break;
	case PRE_INC:
	case POST_INC:
	  fprintf (file, "%d(0,%s)", size, reg_names [REGNO (base)]);
	  break;
	default:
	  output_address (XEXP (x, 0));
	  break;
	}
    }
  else if (GET_CODE (x) == CONST_DOUBLE && GET_MODE (x) == SFmode)
    {
      union { double d; int i[2]; } u;
      union { float f; int i; } u1;
      u.i[0] = XINT (x, 0); u.i[1] = XINT (x, 1);
      u1.f = u.d;
      if (code == 'f')
	fprintf (file, "0r%.9g", u1.f);
      else
	fprintf (file, "0x%x", u1.i);
    }
  else if (GET_CODE (x) == CONST_DOUBLE && GET_MODE (x) != DImode)
    {
      union { double d; int i[2]; } u;
      u.i[0] = XINT (x, 0); u.i[1] = XINT (x, 1);
      fprintf (file, "0r%.20g", u.d);
    }
  else
    output_addr_const (file, x);
}

/* output a SYMBOL_REF or a CONST expression involving a SYMBOL_REF. */

void
output_global_address (file, x)
     FILE *file;
     rtx x;
{
  if (GET_CODE (x) == SYMBOL_REF && read_only_operand (x))
    assemble_name (file, XSTR (x, 0));
  else if (GET_CODE (x) == SYMBOL_REF)
    {
      assemble_name (file, XSTR (x, 0));
      fprintf (file, "-$global$");
    }
  else if (GET_CODE (x) == CONST)
    {
      char *sep = "";
      int offset = 0;		/* assembler wants -$global$ at end */
      rtx base;
	  
      if (GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF)
	{
	  base = XEXP (XEXP (x, 0), 0);
	  output_addr_const (file, base);
	}
      else if (GET_CODE (XEXP (XEXP (x, 0), 0)) == CONST_INT)
	offset = INTVAL (XEXP (XEXP (x, 0), 0));
      else abort ();

      if (GET_CODE (XEXP (XEXP (x, 0), 1)) == SYMBOL_REF)
	{
	  base = XEXP (XEXP (x, 0), 1);
	  output_addr_const (file, base);
	}
      else if (GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT)
	offset = INTVAL (XEXP (XEXP (x, 0),1));
      else abort ();

      if (GET_CODE (XEXP (x, 0)) == PLUS)
	{
	  if (offset < 0)
	    {
	      offset = -offset;
	      sep = "-";
	    }
	  else
	    sep = "+";
	}
      else if (GET_CODE (XEXP (x, 0)) == MINUS
	       && (GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF))
	sep = "-";
      else abort ();

      if (!read_only_operand (base))
	fprintf (file, "-$global$");
      fprintf (file, "%s", sep);
      if (offset) fprintf (file,"%d", offset);
    }
  else
    output_addr_const (file, x);
}

/* MEM rtls here are never SYMBOL_REFs (I think), so fldws is safe. */

char *
output_floatsisf2 (operands)
     rtx *operands;
{
  if (GET_CODE (operands[1]) == MEM)
    return "fldws %1,%0\n\tfcnvxf,sgl,sgl %0,%0";
  else if (FP_REG_P (operands[1]))
    return "fcnvxf,sgl,sgl %1,%0";
  return "stwm %r1,4(0,30)\n\tfldws,mb -4(0,30),%0\n\tfcnvxf,sgl,sgl %0,%0";
}

char *
output_floatsidf2 (operands)
     rtx *operands;
{
  if (GET_CODE (operands[1]) == MEM)
    return "fldws %1,%0\n\tfcnvxf,sgl,dbl %0,%0";
  else if (FP_REG_P (operands[1]))
    return "fcnvxf,sgl,dbl %1,%0";
  return "stwm %r1,4(0,30)\n\tfldws,mb -4(0,30),%0\n\tfcnvxf,sgl,dbl %0,%0";
}

enum rtx_code
reverse_relop (code)
     enum rtx_code code;
{
  switch (code)
    {
    case GT:
      return LT;
    case LT:
      return GT;
    case GE:
      return LE;
    case LE:
      return GE;
    case LTU:
      return GTU;
    case GTU:
      return LTU;
    case GEU:
      return LEU;
    case LEU:
      return GEU;
    default:
      abort ();
    }
}

/* HP's millicode routines mean something special to the assembler.
   Keep track of which ones we have used.  */

enum millicodes { remI, remU, divI, divU, mulI, mulU, end1000 };
static char imported[(int)end1000];
static char *milli_names[] = {"remI", "remU", "divI", "divU", "mulI", "mulU"};
static char import_string[] = ".IMPORT $$....,MILLICODE";
#define MILLI_START 10

static int
import_milli (code)
     enum millicodes code;
{
  char str[sizeof (import_string)];
  
  if (!imported[(int)code])
    {
      imported[(int)code] = 1;
      strcpy (str, import_string);
      strncpy (str + MILLI_START, milli_names[(int)code], 4);
      output_asm_insn (str, 0);
    }
}

/* The register constraints have put the operands and return value in 
   the proper registers. */

char *
output_mul_insn (unsignedp)
     int unsignedp;
{
  if (unsignedp)
    {
      import_milli (mulU);
      return "bl $$mulU,31%#";
    }
  else
    {
      import_milli (mulI);
      return "bl $$mulI,31%#";
    }
}

/* If operands isn't NULL, then it's a CONST_INT with which we can do
   something */


/* Emit the rtl for doing a division by a constant. */

 /* Do magic division millicodes exist for this value? */

static int magic_milli[]= {0, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 0,
			     1, 1};

/* We'll use an array to keep track of the magic millicodes and 
   whether or not we've used them already. [n][0] is signed, [n][1] is
   unsigned. */


static int div_milli[16][2];

int
div_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (mode == SImode
	  && ((GET_CODE (op) == REG && REGNO (op) == 25)
	      || (GET_CODE (op) == CONST_INT && INTVAL (op) > 0
		  && INTVAL (op) < 16 && magic_milli[INTVAL (op)])));
}

int
emit_hpdiv_const (operands, unsignedp)
     rtx *operands;
     int unsignedp;
{
  if (GET_CODE (operands[2]) == CONST_INT
      && INTVAL (operands[2]) > 0
      && INTVAL (operands[2]) < 16
      && magic_milli[INTVAL (operands[2])])
    {
      emit_move_insn ( gen_rtx (REG, SImode, 26), operands[1]);
      emit
	(gen_rtx
	 (PARALLEL, VOIDmode,
	  gen_rtvec (5, gen_rtx (SET, VOIDmode, gen_rtx (REG, SImode, 29),
				 gen_rtx (unsignedp ? UDIV : DIV, SImode,
					  gen_rtx (REG, SImode, 26),
					  operands[2])),
		     gen_rtx (CLOBBER, VOIDmode, gen_rtx (SCRATCH, SImode, 0)),
		     gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 26)),
		     gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 25)),
		     gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 31)))));
      emit_move_insn (operands[0], gen_rtx (REG, SImode, 29));
      return 1;
    }
  return 0;
}

char *
output_div_insn (operands, unsignedp)
     rtx *operands;
     int unsignedp;
{
  int divisor;
  
  /* If the divisor is a constant, try to use one of the special 
     opcodes .*/
  if (GET_CODE (operands[0]) == CONST_INT)
    {
      divisor = INTVAL (operands[0]);
      if (!div_milli[divisor][unsignedp])
	{
	  if (unsignedp)
	    output_asm_insn (".IMPORT $$divU_%0,MILLICODE", operands);
	  else
	    output_asm_insn (".IMPORT $$divI_%0,MILLICODE", operands);
	  div_milli[divisor][unsignedp] = 1;
	}
      if (unsignedp)
	return "bl $$divU_%0,31%#";
      return "bl $$divI_%0,31%#";
    }
  /* Divisor isn't a special constant. */
  else
    {
      if (unsignedp)
	{
	  import_milli (divU);
	  return "bl $$divU,31%#";
	}
      else
	{
	  import_milli (divI);
	  return "bl $$divI,31%#";
	}
    }
}

/* Output a $$rem millicode to do mod. */

char *
output_mod_insn (unsignedp)
     int unsignedp;
{
  if (unsignedp)
    {
      import_milli (remU);
      return "bl $$remU,31%#";
    }
  else
    {
      import_milli (remI);
      return "bl $$remI,31%#";
    }
}

void
output_arg_descriptor (insn)
     rtx insn;
{
  char *arg_regs[4];
  enum machine_mode arg_mode;
  rtx prev_insn;
  int i, output_flag = 0;
  int regno;
  
  for (i = 0; i < 4; i++)
    arg_regs[i] = 0;

  for (prev_insn = PREV_INSN (insn); GET_CODE (prev_insn) == INSN;
       prev_insn = PREV_INSN (prev_insn))
    {
      if (!(GET_CODE (PATTERN (prev_insn)) == USE &&
	    GET_CODE (XEXP (PATTERN (prev_insn), 0)) == REG &&
	    FUNCTION_ARG_REGNO_P (REGNO (XEXP (PATTERN (prev_insn), 0)))))
	break;
      arg_mode = GET_MODE (XEXP (PATTERN (prev_insn), 0));
      regno = REGNO (XEXP (PATTERN (prev_insn), 0));
      if (regno >= 23 && regno <= 26)
	{
	  arg_regs[26 - regno] = "GR";
	  if (arg_mode == DImode)
	    arg_regs[25 - regno] = "GR";
	}
      else if (!TARGET_SNAKE)	/* fp args */
	{
	  if (arg_mode == SFmode)
	    arg_regs[regno - 36] = "FR";
	  else
	    {
#ifdef HP_FP_ARG_DESCRIPTOR_REVERSED
	      arg_regs[regno - 37] = "FR";
	      arg_regs[regno - 36] = "FU";
#else
	      arg_regs[regno - 37] = "FU";
	      arg_regs[regno - 36] = "FR";
#endif
	    }
	}
      else
	{
	  if (arg_mode == SFmode)
	    arg_regs[(regno - 56) / 2] = "FR";
	  else
	    {
#ifdef HP_FP_ARG_DESCRIPTOR_REVERSED
	      arg_regs[(regno - 58) / 2] = "FR";
	      arg_regs[(regno - 58) / 2 + 1] = "FU";
#else
	      arg_regs[(regno - 58) / 2] = "FU";
	      arg_regs[(regno - 58) / 2 + 1] = "FR";
#endif
	    }
	}
    }
  fputs ("\t.CALL ", asm_out_file);
  for (i = 0; i < 4; i++)
    {
      if (arg_regs[i])
	{
	  if (output_flag++)
	    fputc (',', asm_out_file);
	  fprintf (asm_out_file, "ARGW%d=%s", i, arg_regs[i]);
	}
    }
  fputc ('\n', asm_out_file);
}

/* Memory loads/stores to/from the shift need to go through
   the general registers.  */

enum reg_class
secondary_reload_class (class, mode, in)
     enum reg_class class;
     enum machine_mode mode;
     rtx in;
{
  int regno = true_regnum (in);

  if ((TARGET_SHARED_LIBS && function_label_operand (in, mode))
      || ((regno >= FIRST_PSEUDO_REGISTER || regno == -1)
	  && ((mode == QImode || mode == HImode || mode == SImode
	       || mode == DImode) 
	      && (class == FP_REGS || class == SNAKE_FP_REGS
		  || class == HI_SNAKE_FP_REGS)))
      || (class == SHIFT_REGS && (regno <= 0 || regno >= 32)))
    return GENERAL_REGS;

  return NO_REGS;
}

enum direction
function_arg_padding (mode, type)
     enum machine_mode mode;
     tree type;
{
  int size;

  if (mode == BLKmode)
    {
      if (type && TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST)
	size = int_size_in_bytes (type) * BITS_PER_UNIT;
      else
	return upward;		/* Don't know if this is right, but */
				/* same as old definition. */
    }
  else
    size = GET_MODE_BITSIZE (mode);
  if (size < PARM_BOUNDARY)
    return downward;
  else if (size % PARM_BOUNDARY)
    return upward;
  else
    return none;
}


/* Do what is necessary for `va_start'.  The argument is ignored;
   We look at the current function to determine if stdargs or varargs
   is used and fill in an initial va_list.  A pointer to this constructor
   is returned.  */

struct rtx_def *
hppa_builtin_saveregs (arglist)
     tree arglist;
{
  rtx block, float_addr, offset, float_mem;
  tree fntype = TREE_TYPE (current_function_decl);
  int argadj = ((!(TYPE_ARG_TYPES (fntype) != 0
		   && (TREE_VALUE (tree_last (TYPE_ARG_TYPES (fntype)))
		       != void_type_node)))
		? UNITS_PER_WORD : 0);

  if (argadj)
    offset = plus_constant (current_function_arg_offset_rtx, argadj);
  else
    offset = current_function_arg_offset_rtx;

  /* Store general registers on the stack. */
  move_block_from_reg (23,
		       gen_rtx (MEM, BLKmode,
				plus_constant
				(current_function_internal_arg_pointer, -16)),
		       4); 
  return copy_to_reg (expand_binop (Pmode, add_optab,
				    current_function_internal_arg_pointer,
				    offset, 0, 0, OPTAB_LIB_WIDEN));
}

extern struct obstack *saveable_obstack;

/* In HPUX 8.0's shared library scheme, special relocations are needed
   for function labels if they might be passed to a function 
   in a shared library (because shared libraries don't live in code
   space), and special magic is needed to construct their address. */

void
hppa_encode_label (sym)
     rtx sym;
{
  char *str = XSTR (sym, 0);
  int len = strlen (str);
  char *newstr = obstack_alloc (saveable_obstack, len + 2) ;

  if (str[0] == '*')
    *newstr++ = *str++;
  strcpy (newstr + 1, str);
  *newstr = '@';
  XSTR (sym,0) = newstr;
}
  
int
function_label_operand  (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return GET_CODE (op) == SYMBOL_REF && FUNCTION_NAME_P (XSTR (op, 0));
}

/* Return 1 if OP is suitable for the second add operand (the unshifed 
   operand) in an shadd instruction.   Allow CONST_INT to work around
   a reload bug.  */
int
shadd_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == REG)
    return 1;
  if (GET_CODE (op) == CONST_INT)
    return 1;
  return 0;
}
