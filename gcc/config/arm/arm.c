/* Output routines for GCC for ARM/RISCiX.
   Copyright (C) 1991, 1993 Free Software Foundation, Inc.
   Contributed by Pieter `Tiggr' Schoenmakers (rcpieter@win.tue.nl)
   	      and Martin Simmons (@harleqn.co.uk).
   More major hacks by Richard Earnshaw (rwe11@cl.cam.ac.uk)

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
#include "assert.h"
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
#include "reload.h"

/* The maximum number of insns skipped which will be conditionalised if
   possible.  */
#define MAX_INSNS_SKIPPED  5

/* Some function declarations.  */
extern FILE *asm_out_file;
extern char *output_multi_immediate ();
extern char *arm_output_asm_insn ();
extern void arm_increase_location ();

/*  Define the information needed to generate branch insns.  This is
   stored from the compare operation. */

rtx arm_compare_op0, arm_compare_op1;
int arm_compare_fp;

/* What type of cpu are we compiling for? */

enum processor_type arm_cpu;

/* In case of a PRE_INC, POST_INC, PRE_DEC, POST_DEC memory reference, we
   must report the mode of the memory reference from PRINT_OPERAND to
   PRINT_OPERAND_ADDRESS.  */
int output_memory_reference_mode;

/* Nonzero if the prologue must setup `fp'.  */
int current_function_anonymous_args;

/* Location counter of .text segment.  */
int arm_text_location = 0;

/* Set to one if we think that lr is only saved because of subroutine calls,
   but all of these can be `put after' return insns */
int lr_save_eliminated;

/* A hash table is used to store text segment labels and their associated
   offset from the start of the text segment.  */
struct label_offset
{
  char *name;
  int offset;
  struct label_offset *cdr;
};

#define LABEL_HASH_SIZE  257

static struct label_offset *offset_table[LABEL_HASH_SIZE];

/* Set to 1 when a return insn is output, this means that the epilogue
   is not needed. */

static int return_used_this_function;

/* For an explanation of these variables, see final_prescan_insn below.  */
int arm_ccfsm_state;
int arm_current_cc;
rtx arm_target_insn;
int arm_target_label;

/* Return 1 if it is possible to return using a single instruction */

int
use_return_insn ()
{
  int regno;

  if (!reload_completed ||current_function_pretend_args_size
      || current_function_anonymous_args
      || (get_frame_size () && !(TARGET_APCS || frame_pointer_needed)))
    return 0;

  /* Can't be done if any of the FPU regs are pushed, since this also
     requires an insn */
  for (regno = 20; regno < 24; regno++)
    if (regs_ever_live[regno])
      return 0;

  return 1;
}

/* Return the number of mov instructions needed to get the constant VALUE into
   a register.  */

int
arm_const_nmoves (value)
     register int value;
{
  register int i;

  if (value == 0)
    return (1);
  for (i = 0; value; i++, value &= ~0xff)
    while ((value & 3) == 0)
      value = (value >> 2) | ((value & 3) << 30);
  return (i);
} /* arm_const_nmoves */


/* Return TRUE if int I is a valid immediate ARM constant.  */

int
const_ok_for_arm (i)
     HOST_WIDE_INT i;
{
  unsigned HOST_WIDE_INT mask = ~0xFF;

  do
    {
      if ((i & mask & (unsigned HOST_WIDE_INT) 0xffffffff) == 0)
        return(TRUE);
      mask =
	  (mask << 2) | ((mask & (unsigned HOST_WIDE_INT) 0xffffffff)
			 >> (32 - 2)) | ~((unsigned HOST_WIDE_INT) 0xffffffff);
    } while (mask != ~0xFF);

  return (FALSE);
} /* const_ok_for_arm */

/* This code has been fixed for cross compilation. */

static int fpa_consts_inited = 0;

char *strings_fpa[8] = {
  "0.0",
  "1.0",
  "2.0",
  "3.0",
  "4.0",
  "5.0",
  "0.5",
  "10.0"
  };

static REAL_VALUE_TYPE values_fpa[8];

static void
init_fpa_table ()
{
  int i;
  REAL_VALUE_TYPE r;

  for (i = 0; i < 8; i++)
    {
      r = REAL_VALUE_ATOF (strings_fpa[i], DFmode);
      values_fpa[i] = r;
    }
  fpa_consts_inited = 1;
}

/* Return TRUE if rtx X is a valid immediate FPU constant. */

int
const_double_rtx_ok_for_fpu (x)
     rtx x;
{
  REAL_VALUE_TYPE r;
  int i;
  
  if (!fpa_consts_inited)
    init_fpa_table ();
  
  REAL_VALUE_FROM_CONST_DOUBLE (r, x);
  if (REAL_VALUE_MINUS_ZERO (r))
    return 0;
  for (i = 0; i < 8; i++)
    if (REAL_VALUES_EQUAL (r, values_fpa[i]))
      return 1;
  return 0;
} /* const_double_rtx_ok_for_fpu */

/* Return TRUE if rtx X is a valid immediate FPU constant. */

int
neg_const_double_rtx_ok_for_fpu (x)
     rtx x;
{
  REAL_VALUE_TYPE r;
  int i;
  
  if (!fpa_consts_inited)
    init_fpa_table ();
  
  REAL_VALUE_FROM_CONST_DOUBLE (r, x);
  r = REAL_VALUE_NEGATE (r);
  if (REAL_VALUE_MINUS_ZERO (r))
    return 0;
  for (i = 0; i < 8; i++)
    if (REAL_VALUES_EQUAL (r, values_fpa[i]))
      return 1;
  return 0;
} /* neg_const_double_rtx_ok_for_fpu */

/* Predicates for `match_operand' and `match_operator'.  */

/* s_register_operand is the same as register_operand, but it doesn't accept
   (SUBREG (MEM)...). */

int
s_register_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (GET_MODE (op) != mode && mode != VOIDmode)
    return 0;

  if (GET_CODE (op) == SUBREG)
    {
      op = SUBREG_REG (op);
    }

  /* We don't consider registers whose class is NO_REGS
     to be a register operand.  */
  return (GET_CODE (op) == REG
	  && (REGNO (op) >= FIRST_PSEUDO_REGISTER
	      || REGNO_REG_CLASS (REGNO (op)) != NO_REGS));
}

/* Return 1 if OP is an item in memory, given that we are in reload.  */

int
reload_memory_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  int regno = true_regnum (op);

  return (! CONSTANT_P (op)
	  && (regno == -1
	      || (GET_CODE (op) == REG
		  && REGNO (op) >= FIRST_PSEUDO_REGISTER)));
}

/* Return TRUE for valid operands for the rhs of an ARM instruction.  */

int
arm_rhs_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (s_register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && const_ok_for_arm (INTVAL (op))));
} /* arm_rhs_operand */

/* Return TRUE for valid operands for the rhs of an ARM instruction, or a load.
 */

int
arm_rhsm_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (s_register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && const_ok_for_arm (INTVAL (op)))
	  || memory_operand (op, mode));
} /* arm_rhs_operand */

/* Return TRUE for valid operands for the rhs of an ARM instruction, or if a
   constant that is valid when negated.  */

int
arm_add_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (s_register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT
	      && (const_ok_for_arm (INTVAL (op))
		  || const_ok_for_arm (-INTVAL (op)))));
} /* arm_rhs_operand */

int
arm_not_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (s_register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT
	      && (const_ok_for_arm (INTVAL (op))
		  || const_ok_for_arm (~INTVAL (op)))));
} /* arm_rhs_operand */

/* Return TRUE for valid operands for the rhs of an FPU instruction.  */

int
fpu_rhs_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (s_register_operand (op, mode))
    return(TRUE);
  else if (GET_CODE (op) == CONST_DOUBLE)
    return (const_double_rtx_ok_for_fpu (op));
  else return (FALSE);
} /* fpu_rhs_operand */

int
fpu_add_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (s_register_operand (op, mode))
    return(TRUE);
  else if (GET_CODE (op) == CONST_DOUBLE)
    return const_double_rtx_ok_for_fpu (op) 
	|| neg_const_double_rtx_ok_for_fpu (op);
  return (FALSE);
}

/* Return nonzero if OP is a constant power of two.  */

int
power_of_two_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT)
    {
      int value = INTVAL(op);
      return (value != 0  &&  (value & (value-1)) == 0);
    }
  return (FALSE);
} /* power_of_two_operand */

/* Return TRUE for a valid operand of a DImode operation.
   Either: REG, CONST_DOUBLE or MEM(DImode_address).
   Note that this disallows MEM(REG+REG), but allows
   MEM(PRE/POST_INC/DEC(REG)).  */

int
di_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (s_register_operand (op, mode))
    return (TRUE);

  switch (GET_CODE (op))
    {
    case CONST_DOUBLE:
    case CONST_INT:
      return (TRUE);
    case MEM:
      return (memory_address_p (DImode, XEXP (op, 0)));
    default:
      return (FALSE);
    }
} /* di_operand */

/* Return TRUE for valid index operands. */

int
index_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (s_register_operand(op, mode)
	  || (immediate_operand (op, mode)
	      && INTVAL (op) < 4096 && INTVAL (op) > -4096));
} /* index_operand */

/* Return TRUE for valid shifts by a constant. This also accepts any
   power of two on the (somewhat overly relaxed) assumption that the
   shift operator in this case was a mult. */

int
const_shift_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (power_of_two_operand (op, mode)
	  || (immediate_operand (op, mode)
	      && (INTVAL (op) < 32 && INTVAL (op) > 0)));
} /* const_shift_operand */

/* Return TRUE for arithmetic operators which can be combined with a multiply
   (shift).  */

int
shiftable_operator (x, mode)
     rtx x;
     enum machine_mode mode;
{
  if (GET_MODE (x) != mode)
    return FALSE;
  else
    {
      enum rtx_code code = GET_CODE (x);

      return (code == PLUS || code == MINUS
	      || code == IOR || code == XOR || code == AND);
    }
} /* shiftable_operator */

/* Return TRUE for shift operators. */

int
shift_operator (x, mode)
     rtx x;
     enum machine_mode mode;
{
  if (GET_MODE (x) != mode)
    return FALSE;
  else
    {
      enum rtx_code code = GET_CODE (x);

      if (code == MULT)
	return power_of_two_operand (XEXP (x, 1));
      return (code == ASHIFT || code == LSHIFT
	      || code == ASHIFTRT || code == LSHIFTRT);
    }
} /* shift_operator */

int equality_operator (x, mode)
rtx x;
enum machine_mode mode;
{
  return (GET_CODE (x) == EQ || GET_CODE (x) == NE);
}

/* Return TRUE for SMIN SMAX UMIN UMAX operators. */

int
minmax_operator (x, mode)
     rtx x;
     enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (x);

  if (GET_MODE (x) != mode)
    return FALSE;
  return code == SMIN || code == SMAX || code == UMIN || code == UMAX;
} /* minmax_operator */

/* return TRUE if x is EQ or NE */

/* Return TRUE if this is the condition code register, if we aren't given
   a mode, accept any class CCmode register */

int
cc_register (x, mode)
rtx x;
enum machine_mode mode;
{
  if (mode == VOIDmode)
    {
      mode = GET_MODE (x);
      if (GET_MODE_CLASS (mode) != MODE_CC)
	return FALSE;
    }
  if (mode == GET_MODE (x) && GET_CODE (x) == REG && REGNO (x) == 24)
    return TRUE;
  return FALSE;
}
       
enum rtx_code
minmax_code (x)
rtx x;
{
  enum rtx_code code = GET_CODE (x);

  if (code == SMAX)
    return GE;
  if (code == SMIN)
    return LE;
  if (code == UMIN)
    return LEU;
  if (code == UMAX)
    return GEU;
  abort ();
}

/* Return 1 if memory locations are adjacent */

adjacent_mem_locations (a, b)
     rtx a, b;
{
  int val0 = 0, val1 = 0;
  int reg0, reg1;
  
  if ((GET_CODE (XEXP (a, 0)) == REG
       || (GET_CODE (XEXP (a, 0)) == PLUS
	   && GET_CODE (XEXP (XEXP (a, 0), 1)) == CONST_INT))
      && (GET_CODE (XEXP (b, 0)) == REG
	  || (GET_CODE (XEXP (b, 0)) == PLUS
	      && GET_CODE (XEXP (XEXP (b, 0), 1)) == CONST_INT)))
    {
      if (GET_CODE (XEXP (a, 0)) == PLUS)
        {
	  reg0 = REGNO (XEXP (XEXP (a, 0), 0));
	  val0 = INTVAL (XEXP (XEXP (a, 0), 1));
        }
      else
	reg0 = REGNO (XEXP (a, 0));
      if (GET_CODE (XEXP (b, 0)) == PLUS)
        {
	  reg1 = REGNO (XEXP (XEXP (b, 0), 0));
	  val1 = INTVAL (XEXP (XEXP (b, 0), 1));
        }
      else
	reg1 = REGNO (XEXP (b, 0));
      return (reg0 == reg1) && ((val1 - val0) == 4 || (val0 - val1) == 4);
    }
  return 0;
}

/* Return 1 if OP is a load multiple operation.  It is known to be
   parallel and the first section will be tested. */

load_multiple_operation (op, mode)
     rtx op;
     enum machine_mode mode;
{
  int count = XVECLEN (op, 0);
  int dest_regno;
  rtx src_addr;
  int i = 1, base = 0;
  rtx elt;

  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET)
    return 0;

  /* Check to see if this might be a write-back */
  if (GET_CODE (SET_SRC (elt = XVECEXP (op, 0, 0))) == PLUS)
    {
      i++;
      base = 1;

      /* Now check it more carefully */
      if (GET_CODE (SET_DEST (elt)) != REG
          || GET_CODE (XEXP (SET_SRC (elt), 0)) != REG
          || REGNO (XEXP (SET_SRC (elt), 0)) != REGNO (SET_DEST (elt))
          || GET_CODE (XEXP (SET_SRC (elt), 1)) != CONST_INT
          || INTVAL (XEXP (SET_SRC (elt), 1)) != (count - 2) * 4
          || GET_CODE (XVECEXP (op, 0, count - 1)) != CLOBBER
          || GET_CODE (XEXP (XVECEXP (op, 0, count - 1), 0)) != REG
          || REGNO (XEXP (XVECEXP (op, 0, count - 1), 0))
              != REGNO (SET_DEST (elt)))
        return 0;
      count--;
    }

  /* Perform a quick check so we don't blow up below.  */
  if (count <= i
      || GET_CODE (XVECEXP (op, 0, i - 1)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, i - 1))) != REG
      || GET_CODE (SET_SRC (XVECEXP (op, 0, i - 1))) != MEM)
    return 0;

  dest_regno = REGNO (SET_DEST (XVECEXP (op, 0, i - 1)));
  src_addr = XEXP (SET_SRC (XVECEXP (op, 0, i - 1)), 0);

  for (; i < count; i++)
    {
      rtx elt = XVECEXP (op, 0, i);

      if (GET_CODE (elt) != SET
          || GET_CODE (SET_DEST (elt)) != REG
          || GET_MODE (SET_DEST (elt)) != SImode
          || REGNO (SET_DEST (elt)) != dest_regno + i - base
          || GET_CODE (SET_SRC (elt)) != MEM
          || GET_MODE (SET_SRC (elt)) != SImode
          || GET_CODE (XEXP (SET_SRC (elt), 0)) != PLUS
          || ! rtx_equal_p (XEXP (XEXP (SET_SRC (elt), 0), 0), src_addr)
          || GET_CODE (XEXP (XEXP (SET_SRC (elt), 0), 1)) != CONST_INT
          || INTVAL (XEXP (XEXP (SET_SRC (elt), 0), 1)) != (i - base) * 4)
        return 0;
    }

  return 1;
}

/* Return 1 if OP is a store multiple operation.  It is known to be
   parallel and the first section will be tested. */

store_multiple_operation (op, mode)
     rtx op;
     enum machine_mode mode;
{
  int count = XVECLEN (op, 0);
  int src_regno;
  rtx dest_addr;
  int i = 1, base = 0;
  rtx elt;

  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET)
    return 0;

  /* Check to see if this might be a write-back */
  if (GET_CODE (SET_SRC (elt = XVECEXP (op, 0, 0))) == PLUS)
    {
      i++;
      base = 1;

      /* Now check it more carefully */
      if (GET_CODE (SET_DEST (elt)) != REG
          || GET_CODE (XEXP (SET_SRC (elt), 0)) != REG
          || REGNO (XEXP (SET_SRC (elt), 0)) != REGNO (SET_DEST (elt))
          || GET_CODE (XEXP (SET_SRC (elt), 1)) != CONST_INT
          || INTVAL (XEXP (SET_SRC (elt), 1)) != (count - 2) * 4
          || GET_CODE (XVECEXP (op, 0, count - 1)) != CLOBBER
          || GET_CODE (XEXP (XVECEXP (op, 0, count - 1), 0)) != REG
          || REGNO (XEXP (XVECEXP (op, 0, count - 1), 0))
              != REGNO (SET_DEST (elt)))
        return 0;
      count--;
    }

  /* Perform a quick check so we don't blow up below.  */
  if (count <= i
      || GET_CODE (XVECEXP (op, 0, i - 1)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, i - 1))) != MEM
      || GET_CODE (SET_SRC (XVECEXP (op, 0, i - 1))) != REG)
    return 0;

  src_regno = REGNO (SET_SRC (XVECEXP (op, 0, i - 1)));
  dest_addr = XEXP (SET_DEST (XVECEXP (op, 0, i - 1)), 0);

  for (; i < count; i++)
    {
      elt = XVECEXP (op, 0, i);

      if (GET_CODE (elt) != SET
          || GET_CODE (SET_SRC (elt)) != REG
          || GET_MODE (SET_SRC (elt)) != SImode
          || REGNO (SET_SRC (elt)) != src_regno + i - base
          || GET_CODE (SET_DEST (elt)) != MEM
          || GET_MODE (SET_DEST (elt)) != SImode
          || GET_CODE (XEXP (SET_DEST (elt), 0)) != PLUS
          || ! rtx_equal_p (XEXP (XEXP (SET_DEST (elt), 0), 0), dest_addr)
          || GET_CODE (XEXP (XEXP (SET_DEST (elt), 0), 1)) != CONST_INT
          || INTVAL (XEXP (XEXP (SET_DEST (elt), 0), 1)) != (i - base) * 4)
        return 0;
    }

  return 1;
}

/* Routines for use in generating RTL */

rtx arm_gen_load_multiple (base_regno, count, from, up, write_back)
     int base_regno;
     int count;
     rtx from;
     int up;
     int write_back;
{
  int i = 0, j;
  rtx result;
  int sign = up ? 1 : -1;

  result = gen_rtx (PARALLEL, VOIDmode,
                    rtvec_alloc (count + (write_back ? 2 : 0)));
  if (write_back)
  {
      XVECEXP (result, 0, 0)
          = gen_rtx (SET, GET_MODE (from), from,
                     plus_constant (from, count * 4 * sign));
      i = 1;
      count++;
  }
  for (j = 0; i < count; i++, j++)
  {
      XVECEXP (result, 0, i)
          = gen_rtx (SET, VOIDmode, gen_rtx (REG, SImode, base_regno + j),
                     gen_rtx (MEM, SImode,
                              plus_constant (from, j * 4 * sign)));
  }
  if (write_back)
    XVECEXP (result, 0, i) = gen_rtx (CLOBBER, SImode, from);

  return result;
}

rtx arm_gen_store_multiple (base_regno, count, to, up, write_back)
     int base_regno;
     int count;
     rtx to;
     int up;
     int write_back;
{
  int i = 0, j;
  rtx result;
  int sign = up ? 1 : -1;

  result = gen_rtx (PARALLEL, VOIDmode,
                    rtvec_alloc (count + (write_back ? 2 : 0)));
  if (write_back)
  {
      XVECEXP (result, 0, 0)
          = gen_rtx (SET, GET_MODE (to), to,
                     plus_constant (to, count * 4 * sign));
      i = 1;
      count++;
  }
  for (j = 0; i < count; i++, j++)
  {
      XVECEXP (result, 0, i)
          = gen_rtx (SET, VOIDmode,
                     gen_rtx (MEM, SImode, plus_constant (to, j * 4 * sign)),
                     gen_rtx (REG, SImode, base_regno + j));
  }
  if (write_back)
    XVECEXP (result, 0, i) = gen_rtx (CLOBBER, SImode, to);

  return result;
}

/* X and Y are two things to compare using CODE.  Emit the compare insn and
   return the rtx for register 0 in the proper mode.  FP means this is a
   floating point compare: I don't think that it is needed on the arm.  */

rtx
gen_compare_reg (code, x, y, fp)
     enum rtx_code code;
     rtx x, y;
{
  enum machine_mode mode = SELECT_CC_MODE (code, x, y);
  rtx cc_reg = gen_rtx (REG, mode, 24);

  emit_insn (gen_rtx (SET, VOIDmode, cc_reg,
                      gen_rtx (COMPARE, mode, x, y)));

  return cc_reg;
}

arm_reload_out_hi (operands)
rtx operands[];
{
  rtx base = find_replacement (&XEXP (operands[0], 0));

  emit_insn (gen_rtx (SET, VOIDmode,
                      gen_rtx (MEM, QImode, base),
                      gen_rtx (SUBREG, QImode, operands[1], 0)));
  emit_insn (gen_rtx (SET, VOIDmode, operands[2],
                      gen_rtx (LSHIFTRT, SImode, 
                               gen_rtx (SUBREG, SImode, operands[1], 0),
                               GEN_INT (8))));
  emit_insn (gen_rtx (SET, VOIDmode,
                      gen_rtx (MEM, QImode,
                               plus_constant (base, 1)),
                      gen_rtx (SUBREG, QImode, operands[2], 0)));
}

/* Check to see if a branch is forwards or backwards.  Return TRUE if it
   is backwards.  */

int
arm_backwards_branch (from, to)
int from, to;
{
  return (insn_addresses[to] <= insn_addresses[from]);
}

/* Check to see if a branch is within the distance that can be done using
   an arithmetic expression. */
int
short_branch (from, to)
int from, to;
{
  int delta = insn_addresses[from] + 2 - insn_addresses[to];

  return abs (delta) < 245;	/* A small margin for safety */
}

/* Check to see that the insn isn't the target of the conditionalizing
   code */
int
arm_insn_not_targeted (insn)
rtx insn;
{
  return insn != arm_target_insn;
}


/* Routines to output assembly language.  */

/* fp_immediate_constant 
   if the rtx is the correct value then return the string of the number.
   In this way we can ensure that valid double constants are generated even
   when cross compiling. */
char *
fp_immediate_constant (x)
rtx (x);
{
  REAL_VALUE_TYPE r;
  int i;
  
  if (!fpa_consts_inited)
    init_fpa_table ();
  
  REAL_VALUE_FROM_CONST_DOUBLE (r, x);
  for (i = 0; i < 8; i++)
    if (REAL_VALUES_EQUAL (r, values_fpa[i]))
      return strings_fpa[i];
  abort ();
}


/* Output the operands of a LDM/STM instruction to STREAM.
   MASK is the ARM register set mask of which only bits 0-15 are important.
   INSTR is the possibly suffixed base register.  HAT unequals zero if a hat
   must follow the register list.  */

void
print_multi_reg (stream, instr, mask, hat)
     FILE *stream;
     char *instr;
     int mask, hat;
{
  int i;
  int not_first = FALSE;

  fprintf (stream, "\t%s, {", instr);
  for (i = 0; i < 16; i++)
    if (mask & (1 << i))
      {
	if (not_first)
	  fprintf (stream, ", ");
	fprintf (stream, "%s", reg_names[i]);
	not_first = TRUE;
      }
  fprintf (stream, "}%s\n", hat ? "^" : "");
} /* print_multi_reg */

/* Output a 'call' insn. */

char *
output_call (operands)
	rtx operands[];
{
  /* Handle calls to lr using ip (which may be clobbered in subr anyway). */

  if (REGNO (operands[0]) == 14)
    {
      operands[0] = gen_rtx (REG, SImode, 12);
      arm_output_asm_insn ("mov\t%0, lr", operands);
    }
  arm_output_asm_insn ("mov\tlr, pc", operands);
  arm_output_asm_insn ("mov\tpc, %0", operands);
  return ("");
} /* output_call */

static int
eliminate_lr2ip (x)
rtx *x;
{
  int something_changed = 0;
  rtx x0 = *x;
  int code = GET_CODE (x0);
  register int i, j;
  register char *fmt;
  
  switch (code)
    {
    case REG:
      if (REGNO (x0) == 14)
        {
	  *x = gen_rtx (REG, SImode, 12);
	  return 1;
        }
      return 0;
    default:
      /* Scan through the sub-elements and change any references there */
      fmt = GET_RTX_FORMAT (code);
      for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
	if (fmt[i] == 'e')
	  something_changed |= eliminate_lr2ip (&XEXP (x0, i));
	else if (fmt[i] == 'E')
	  for (j = 0; j < XVECLEN (x0, i); j++)
	    something_changed |= eliminate_lr2ip (&XVECEXP (x0, i, j));
      return something_changed;
    }
}
  
/* Output a 'call' insn that is a reference in memory. */

char *
output_call_mem (operands)
	rtx operands[];
{
  operands[0] = copy_rtx (operands[0]); /* Be ultra careful */
  /* Handle calls using lr by using ip (which may be clobbered in subr anyway).
   */
  if (eliminate_lr2ip (&operands[0]))
    arm_output_asm_insn ("mov\tip, lr", operands);
  arm_output_asm_insn ("mov\tlr, pc", operands);
  arm_output_asm_insn ("ldr\tpc, %0", operands);
  return ("");
} /* output_call */


/* Output a move from arm registers to an fpu registers.
   OPERANDS[0] is an fpu register.
   OPERANDS[1] is the first registers of an arm register pair.  */

char *
output_mov_long_double_fpu_from_arm (operands)
     rtx operands[];
{
  int arm_reg0 = REGNO (operands[1]);
  rtx ops[3];

  if (arm_reg0 == 12)
    abort();
  ops[0] = gen_rtx (REG, SImode, arm_reg0);
  ops[1] = gen_rtx (REG, SImode, 1 + arm_reg0);
  ops[2] = gen_rtx (REG, SImode, 2 + arm_reg0);
  
  arm_output_asm_insn ("stmfd\tsp!, {%0, %1, %2}", ops);
  arm_output_asm_insn ("ldfe\t%0, [sp], #12", operands);
  return ("");
} /* output_mov_long_double_fpu_from_arm */

/* Output a move from an fpu register to arm registers.
   OPERANDS[0] is the first registers of an arm register pair.
   OPERANDS[1] is an fpu register.  */

char *
output_mov_long_double_arm_from_fpu (operands)
     rtx operands[];
{
  int arm_reg0 = REGNO (operands[0]);
  rtx ops[3];

  if (arm_reg0 == 12)
    abort();
  ops[0] = gen_rtx (REG, SImode, arm_reg0);
  ops[1] = gen_rtx (REG, SImode, 1 + arm_reg0);
  ops[2] = gen_rtx (REG, SImode, 2 + arm_reg0);

  arm_output_asm_insn ("stfe\t%1, [sp, #-12]!", operands);
  arm_output_asm_insn ("ldmfd\tsp!, {%0, %1, %2}", ops);
  return("");
} /* output_mov_long_double_arm_from_fpu */

/* Output a move from arm registers to arm registers of a long double
   OPERANDS[0] is the destination.
   OPERANDS[1] is the source.  */
char *
output_mov_long_double_arm_from_arm (operands)
rtx operands[];
{
  /* We have to be careful here because the two might overlap */
  int dest_start = REGNO (operands[0]);
  int src_start = REGNO (operands[1]);
  rtx ops[2];
  int i;

  if (dest_start < src_start)
    {
      for (i = 0; i < 3; i++)
	{
	  ops[0] = gen_rtx (REG, SImode, dest_start + i);
	  ops[1] = gen_rtx (REG, SImode, src_start + i);
	  arm_output_asm_insn ("mov\t%0, %1", ops);
	}
    }
  else
    {
      for (i = 2; i >= 0; i--)
	{
	  ops[0] = gen_rtx (REG, SImode, dest_start + i);
	  ops[1] = gen_rtx (REG, SImode, src_start + i);
	  arm_output_asm_insn ("mov\t%0, %1", ops);
	}
    }
  return "";
}


/* Output a move from arm registers to an fpu registers.
   OPERANDS[0] is an fpu register.
   OPERANDS[1] is the first registers of an arm register pair.  */

char *
output_mov_double_fpu_from_arm (operands)
     rtx operands[];
{
  int arm_reg0 = REGNO (operands[1]);
  rtx ops[2];

  if (arm_reg0 == 12)
    abort();
  ops[0] = gen_rtx (REG, SImode, arm_reg0);
  ops[1] = gen_rtx (REG, SImode, 1 + arm_reg0);
  arm_output_asm_insn ("stmfd\tsp!, {%0, %1}", ops);
  arm_output_asm_insn ("ldfd\t%0, [sp], #8", operands);
  return ("");
} /* output_mov_double_fpu_from_arm */

/* Output a move from an fpu register to arm registers.
   OPERANDS[0] is the first registers of an arm register pair.
   OPERANDS[1] is an fpu register.  */

char *
output_mov_double_arm_from_fpu (operands)
     rtx operands[];
{
  int arm_reg0 = REGNO (operands[0]);
  rtx ops[2];

  if (arm_reg0 == 12)
    abort();
  ops[0] = gen_rtx (REG, SImode, arm_reg0);
  ops[1] = gen_rtx (REG, SImode, 1 + arm_reg0);
  arm_output_asm_insn ("stfd\t%1, [sp, #-8]!", operands);
  arm_output_asm_insn ("ldmfd\tsp!, {%0, %1}", ops);
  return("");
} /* output_mov_double_arm_from_fpu */

/* Output a move between double words.
   It must be REG<-REG, REG<-CONST_DOUBLE, REG<-CONST_INT, REG<-MEM
   or MEM<-REG and all MEMs must be offsettable addresses.  */

char *
output_move_double (operands)
     rtx operands[];
{
  enum rtx_code code0 = GET_CODE (operands[0]);
  enum rtx_code code1 = GET_CODE (operands[1]);
  rtx otherops[2];

  if (code0 == REG)
    {
      int reg0 = REGNO (operands[0]);

      otherops[0] = gen_rtx (REG, SImode, 1 + reg0);
      if (code1 == REG)
	{
	  int reg1 = REGNO (operands[1]);
	  if (reg1 == 12)
	    abort();
	  otherops[1] = gen_rtx (REG, SImode, 1 + reg1);

	  /* Ensure the second source is not overwritten */
	  if (reg0 == 1 + reg1)
	    {
	      arm_output_asm_insn("mov\t%0, %1", otherops);
	      arm_output_asm_insn("mov\t%0, %1", operands);
	    }
	  else
	    {
	      arm_output_asm_insn("mov\t%0, %1", operands);
	      arm_output_asm_insn("mov\t%0, %1", otherops);
	    }
	}
      else if (code1 == CONST_DOUBLE)
	{
	  otherops[1] = gen_rtx (CONST_INT, VOIDmode,
				 CONST_DOUBLE_HIGH (operands[1]));
	  operands[1] = gen_rtx (CONST_INT, VOIDmode,
				 CONST_DOUBLE_LOW (operands[1]));
	  output_mov_immediate (operands, FALSE, "");
	  output_mov_immediate (otherops, FALSE, "");
	}
      else if (code1 == CONST_INT)
	{
	  otherops[1] = const0_rtx;
	  /* sign extend the intval into the high-order word */
	  /* Note: output_mov_immediate may clobber operands[1], so we
	     put this out first */
	  if (INTVAL (operands[1]) < 0)
	    arm_output_asm_insn ("mvn\t%0, %1", otherops);
	  else
	    arm_output_asm_insn ("mov\t%0, %1", otherops);
	  output_mov_immediate (operands, FALSE, "");
	}
      else if (code1 == MEM)
	{
	  switch (GET_CODE (XEXP (operands[1], 0)))
	    {
	    case REG:
	      /* Handle the simple case where address is [r, #0] more
		 efficient.  */
	      operands[1] = XEXP (operands[1], 0);
	      arm_output_asm_insn ("ldmia\t%1, %M0", operands);
	      break;
  	    case PRE_INC:
	      operands[1] = XEXP (XEXP (operands[1], 0), 0);
	      arm_output_asm_insn ("add\t%1, %1, #8", operands);
	      arm_output_asm_insn ("ldmia\t%1, %M0", operands);
	      break;
	    case PRE_DEC:
	      operands[1] = XEXP (XEXP (operands[1], 0), 0);
	      arm_output_asm_insn ("sub\t%1, %1, #8", operands);
	      arm_output_asm_insn ("ldmia\t%1, %M0", operands);
	      break;
	    case POST_INC:
	      operands[1] = XEXP (XEXP (operands[1], 0), 0);
	      arm_output_asm_insn ("ldmia\t%1!, %M0", operands);
	      break;
	    case POST_DEC:
	      operands[1] = XEXP (XEXP (operands[1], 0), 0);
	      arm_output_asm_insn ("ldmia\t%1, %M0", operands);
	      arm_output_asm_insn ("sub\t%1, %1, #8", operands);
	      break;
	    default:
	      otherops[1] = adj_offsettable_operand (operands[1], 4);
	      /* Take care of overlapping base/data reg.  */
	      if (reg_mentioned_p (operands[0], operands[1]))
		{
		  arm_output_asm_insn ("ldr\t%0, %1", otherops);
		  arm_output_asm_insn ("ldr\t%0, %1", operands);
		}
	      else
		{
		  arm_output_asm_insn ("ldr\t%0, %1", operands);
		  arm_output_asm_insn ("ldr\t%0, %1", otherops);
		}
	    }
	}
      else abort();  /* Constraints should prevent this */
    }
  else if (code0 == MEM && code1 == REG)
    {
      if (REGNO (operands[1]) == 12)
	abort();
      switch (GET_CODE (XEXP (operands[0], 0)))
        {
	case REG:
	  operands[0] = XEXP (operands[0], 0);
	  arm_output_asm_insn ("stmia\t%0, %M1", operands);
	  break;
        case PRE_INC:
	  operands[0] = XEXP (XEXP (operands[0], 0), 0);
	  arm_output_asm_insn ("add\t%0, %0, #8", operands);
	  arm_output_asm_insn ("stmia\t%0, %M1", operands);
	  break;
        case PRE_DEC:
	  operands[0] = XEXP (XEXP (operands[0], 0), 0);
	  arm_output_asm_insn ("sub\t%0, %0, #8", operands);
	  arm_output_asm_insn ("stmia\t%0, %M1", operands);
	  break;
        case POST_INC:
	  operands[0] = XEXP (XEXP (operands[0], 0), 0);
	  arm_output_asm_insn ("stmia\t%0!, %M1", operands);
	  break;
        case POST_DEC:
	  operands[0] = XEXP (XEXP (operands[0], 0), 0);
	  arm_output_asm_insn ("stmia\t%0, %M1", operands);
	  arm_output_asm_insn ("sub\t%0, %0, #8", operands);
	  break;
        default:
	  otherops[0] = adj_offsettable_operand (operands[0], 4);
	  otherops[1] = gen_rtx (REG, SImode, 1 + REGNO (operands[1]));
	  arm_output_asm_insn ("str\t%1, %0", operands);
	  arm_output_asm_insn ("str\t%1, %0", otherops);
	}
    }
  else abort();  /* Constraints should prevent this */

  return("");
} /* output_move_double */


/* Output an arbitrary MOV reg, #n.
   OPERANDS[0] is a register.  OPERANDS[1] is a const_int.  */

char *
output_mov_immediate (operands)
     rtx operands[2];
{
  int n = INTVAL (operands[1]);
  int n_ones = 0;
  int i;

  /* Try to use one MOV */

  if (const_ok_for_arm (n))
    return (arm_output_asm_insn ("mov\t%0, %1", operands));

  /* Try to use one MVN */

  if (const_ok_for_arm(~n))
    {
      operands[1] = gen_rtx (CONST_INT, VOIDmode, ~n);
      return (arm_output_asm_insn ("mvn\t%0, %1", operands));
    }

  /* If all else fails, make it out of ORRs or BICs as appropriate. */

  for (i=0; i < 32; i++)
    if (n & 1 << i)
      n_ones++;

  if (n_ones > 16)  /* Shorter to use MVN with BIC in this case. */
    output_multi_immediate(operands, "mvn\t%0, %1", "bic\t%0, %0, %1", 1, ~n);
  else
    output_multi_immediate(operands, "mov\t%0, %1", "orr\t%0, %0, %1", 1, n);
  return("");
} /* output_mov_immediate */


/* Output an ADD r, s, #n where n may be too big for one instruction.  If
   adding zero to one register, output nothing.  */

char *
output_add_immediate (operands)
     rtx operands[3];
{
  int n = INTVAL (operands[2]);

  if (n != 0 || REGNO (operands[0]) != REGNO (operands[1]))
    {
      if (n < 0)
	output_multi_immediate (operands,
				"sub\t%0, %1, %2", "sub\t%0, %0, %2", 2, -n);
      else
	output_multi_immediate (operands,
				"add\t%0, %1, %2", "add\t%0, %0, %2", 2, n);
    }
  return("");
} /* output_add_immediate */


/* Output a multiple immediate operation.
   OPERANDS is the vector of operands referred to in the output patterns.
   INSTR1 is the output pattern to use for the first constant.
   INSTR2 is the output pattern to use for subsequent constants.
   IMMED_OP is the index of the constant slot in OPERANDS.
   N is the constant value.  */

char *
output_multi_immediate (operands, instr1, instr2, immed_op, n)
     rtx operands[];
     char *instr1, *instr2;
     int immed_op, n;
{
  if (n == 0)
    {
      operands[immed_op] = const0_rtx;
      arm_output_asm_insn (instr1, operands); /* Quick and easy output */
    }
  else
    {
      int i;
      char *instr = instr1;

      /* Note that n is never zero here (which would give no output) */

      for (i = 0; i < 32; i += 2)
	{
	  if (n & (3 << i))
	    {
	      operands[immed_op] = gen_rtx (CONST_INT, VOIDmode,
					    n & (255 << i));
	      arm_output_asm_insn (instr, operands);
	      instr = instr2;
	      i += 6;
	    }
	}
    }
  return ("");
} /* output_multi_immediate */


/* Return the appropriate ARM instruction for the operation code.
   The returned result should not be overwritten.  OP is the rtx of the
   operation.  SHIFT_FIRST_ARG is TRUE if the first argument of the operator
   was shifted.  */

char *
arithmetic_instr (op, shift_first_arg)
     rtx op;
{
  switch (GET_CODE(op))
    {
    case PLUS:
      return ("add");
    case MINUS:
      if (shift_first_arg)
	return ("rsb");
      else
	return ("sub");
    case IOR:
      return ("orr");
    case XOR:
      return ("eor");
    case AND:
      return ("and");
    default:
      abort();
    }
  return ("");			/* stupid cc */
} /* arithmetic_instr */


/* Ensure valid constant shifts and return the appropriate shift mnemonic
   for the operation code.  The returned result should not be overwritten.
   OP is the rtx code of the shift.
   SHIFT_PTR points to the shift size operand.  */

char *
shift_instr (op, shift_ptr)
     enum rtx_code op;
     rtx *shift_ptr;
{
  int min_shift = 0;
  int max_shift = 31;
  char *mnem;

  switch (op)
    {
    case ASHIFT:
      mnem = "asl";
      break;
    case LSHIFT:
      mnem = "lsl";
      break;
    case ASHIFTRT:
      mnem = "asr";
      max_shift = 32;
      break;
    case LSHIFTRT:
      mnem = "lsr";
      max_shift = 32;
      break;
    case MULT:
      *shift_ptr = gen_rtx (CONST_INT, VOIDmode,
			    int_log2 (INTVAL (*shift_ptr)));
      return ("asl");
    default:
      abort();
    }

  if (GET_CODE (*shift_ptr) == CONST_INT)
    {
      int shift = INTVAL (*shift_ptr);

      if (shift < min_shift)
	*shift_ptr = gen_rtx (CONST_INT, VOIDmode, 0);
      else if (shift > max_shift)
	*shift_ptr = gen_rtx (CONST_INT, VOIDmode, max_shift);
    }
  return (mnem);
} /* shift_instr */


/* Obtain the shift from the POWER of two. */

int
int_log2 (power)
     unsigned int power;
{
  int shift = 0;

  while (((1 << shift) & power) == 0)
    {
      if (shift > 31)
	abort();
      shift++;
    }
  return (shift);
} /* int_log2 */


/* Output an arithmetic instruction which may set the condition code.
   OPERANDS[0] is the destination register.
   OPERANDS[1] is the arithmetic operator expression.
   OPERANDS[2] is the left hand argument.
   OPERANDS[3] is the right hand argument.
   CONST_FIRST_ARG is TRUE if the first argument of the operator was constant.
   SET_COND is TRUE when the condition code should be set.  */

char *
output_arithmetic (operands, const_first_arg, set_cond)
     rtx operands[4];
     int const_first_arg;
     int set_cond;
{
  char mnemonic[80];
  char *instr = arithmetic_instr (operands[1], const_first_arg);

  sprintf (mnemonic, "%s%s\t%%0, %%2, %%3", instr, set_cond ? "s" : "");
  return (arm_output_asm_insn (mnemonic, operands));
} /* output_arithmetic */


/* Output an arithmetic instruction with a shift.
   OPERANDS[0] is the destination register.
   OPERANDS[1] is the arithmetic operator expression.
   OPERANDS[2] is the unshifted register.
   OPERANDS[3] is the shift operator expression.
   OPERANDS[4] is the shifted register.
   OPERANDS[5] is the shift constant or register.
   SHIFT_FIRST_ARG is TRUE if the first argument of the operator was shifted.
   SET_COND is TRUE when the condition code should be set.  */

char *
output_arithmetic_with_shift (operands, shift_first_arg, set_cond)
     rtx operands[6];
     int shift_first_arg;
     int set_cond;
{
  char mnemonic[80];
  char *instr = arithmetic_instr (operands[1], shift_first_arg);
  char *condbit = set_cond ? "s" : "";
  char *shift = shift_instr (GET_CODE (operands[3]), &operands[5]);

  sprintf (mnemonic, "%s%s\t%%0, %%2, %%4, %s %%5", instr, condbit, shift);
  return (arm_output_asm_insn (mnemonic, operands));
} /* output_arithmetic_with_shift */


/* Output an arithmetic instruction with a power of two multiplication.
   OPERANDS[0] is the destination register.
   OPERANDS[1] is the arithmetic operator expression.
   OPERANDS[2] is the unmultiplied register.
   OPERANDS[3] is the multiplied register.
   OPERANDS[4] is the constant multiple (power of two).
   SHIFT_FIRST_ARG is TRUE if the first arg of the operator was multiplied.  */

char *
output_arithmetic_with_immediate_multiply (operands, shift_first_arg)
     rtx operands[5];
     int shift_first_arg;
{
  char mnemonic[80];
  char *instr = arithmetic_instr (operands[1], shift_first_arg);
  int shift = int_log2 (INTVAL (operands[4]));

  sprintf (mnemonic, "%s\t%%0, %%2, %%3, asl#%d", instr, shift);
  return (arm_output_asm_insn (mnemonic, operands));
} /* output_arithmetic_with_immediate_multiply */


/* Output a move with a shift.
   OP is the shift rtx code.
   OPERANDS[0] = destination register.
   OPERANDS[1] = source register.
   OPERANDS[2] = shift constant or register.  */

char *
output_shifted_move (op, operands)
     enum rtx_code op;
     rtx operands[2];
{
  char mnemonic[80];

  if (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) == 0)
    sprintf (mnemonic, "mov\t%%0, %%1");
  else
    sprintf (mnemonic, "mov\t%%0, %%1, %s %%2",
	     shift_instr (op, &operands[2]));
  return (arm_output_asm_insn (mnemonic, operands));
} /* output_shifted_move */

char *
output_shift_compare (operands, neg)
rtx *operands;
int neg;
{
  char buf[80];

  if (neg)
    sprintf (buf, "cmn\t%%1, %%3, %s %%4", shift_instr (GET_CODE (operands[2]),
							&operands[4]));
  else
    sprintf (buf, "cmp\t%%1, %%3, %s %%4", shift_instr (GET_CODE (operands[2]),
							&operands[4]));
  return arm_output_asm_insn (buf, operands);
} /* output_shift_compare */

/* Output a .ascii pseudo-op, keeping track of lengths.  This is because
   /bin/as is horribly restrictive.  */

void
output_ascii_pseudo_op (stream, p, len)
     FILE *stream;
     unsigned char *p;
     int len;
{
  int i;
  int len_so_far = 1000;
  int chars_so_far = 0;

  for (i = 0; i < len; i++)
    {
      register int c = p[i];

      if (len_so_far > 50)
	{
	  if (chars_so_far)
	    fputs ("\"\n", stream);
	  fputs ("\t.ascii\t\"", stream);
	  len_so_far = 0;
	  arm_increase_location (chars_so_far);
	  chars_so_far = 0;
	}

      if (c == '\"' || c == '\\')
	{
	  putc('\\', stream);
	  len_so_far++;
	}
      if (c >= ' ' && c < 0177)
	{
	  putc (c, stream);
	  len_so_far++;
	}
      else
	{
	  fprintf (stream, "\\%03o", c);
	  len_so_far +=4;
	}
      chars_so_far++;
    }
  fputs ("\"\n", stream);
  arm_increase_location (chars_so_far);
} /* output_ascii_pseudo_op */


/* Try to determine whether a pattern really clobbers the link register.
   This information is useful when peepholing, so that lr need not be pushed
   if we combine a call followed by a return.
   NOTE: This code does not check for side-effect expressions in a SET_SRC:
   such a check should not be needed because these only update an existing
   value within a register; the register must still be set elsewhere within
   the function. */

static int
pattern_really_clobbers_lr (x)
rtx x;
{
  int i;
  
  switch (GET_CODE (x))
    {
    case SET:
      switch (GET_CODE (SET_DEST (x)))
	{
	case REG:
	  return REGNO (SET_DEST (x)) == 14;
        case SUBREG:
	  if (GET_CODE (XEXP (SET_DEST (x), 0)) == REG)
	    return REGNO (XEXP (SET_DEST (x), 0)) == 14;
	  if (GET_CODE (XEXP (SET_DEST (x), 0)) == MEM)
	    return 0;
	  abort ();
        default:
	  return 0;
        }
    case PARALLEL:
      for (i = 0; i < XVECLEN (x, 0); i++)
	if (pattern_really_clobbers_lr (XVECEXP (x, 0, i)))
	  return 1;
      return 0;
    case CLOBBER:
      switch (GET_CODE (XEXP (x, 0)))
        {
	case REG:
	  return REGNO (XEXP (x, 0)) == 14;
        case SUBREG:
	  if (GET_CODE (XEXP (XEXP (x, 0), 0)) == REG)
	    return REGNO (XEXP (XEXP (x, 0), 0)) == 14;
	  abort ();
        default:
	  return 0;
        }
    case UNSPEC:
      return 1;
    default:
      return 0;
    }
}

static int
function_really_clobbers_lr (first)
rtx first;
{
  rtx insn, next;
  
  for (insn = first; insn; insn = next_nonnote_insn (insn))
    {
      switch (GET_CODE (insn))
        {
	case BARRIER:
	case NOTE:
	case CODE_LABEL:
	case JUMP_INSN:		/* Jump insns only change the PC (and conds) */
	case INLINE_HEADER:
	  break;
        case INSN:
	  if (pattern_really_clobbers_lr (PATTERN (insn)))
	    return 1;
	  break;
        case CALL_INSN:
	  /* Don't yet know how to handle those calls that are not to a 
	     SYMBOL_REF */
	  if (GET_CODE (PATTERN (insn)) != PARALLEL)
	    abort ();
	  switch (GET_CODE (XVECEXP (PATTERN (insn), 0, 0)))
	    {
	    case CALL:
	      if (GET_CODE (XEXP (XEXP (XVECEXP (PATTERN (insn), 0, 0), 0), 0))
		  != SYMBOL_REF)
		return 1;
	      break;
	    case SET:
	      if (GET_CODE (XEXP (XEXP (SET_SRC (XVECEXP (PATTERN (insn),
							  0, 0)), 0), 0))
		  != SYMBOL_REF)
		return 1;
	      break;
	    default:	/* Don't recognize it, be safe */
	      return 1;
	    }
	  /* A call can be made (by peepholing) not to clobber lr iff it is
	     followed by a return.  There may, however, be a use insn iff
	     we are returning the result of the call. 
	     If we run off the end of the insn chain, then that means the
	     call was at the end of the function.  Unfortunately we don't
	     have a return insn for the peephole to recognize, so we
	     must reject this.  (Can this be fixed by adding our own insn?) */
	  if ((next = next_nonnote_insn (insn)) == NULL)
	    return 1;
	  if (GET_CODE (next) == INSN && GET_CODE (PATTERN (next)) == USE
	      && (GET_CODE (XVECEXP (PATTERN (insn), 0, 0)) == SET)
	      && (REGNO (SET_DEST (XVECEXP (PATTERN (insn), 0, 0)))
		  == REGNO (XEXP (PATTERN (next), 0))))
	    if ((next = next_nonnote_insn (next)) == NULL)
	      return 1;
	  if (GET_CODE (next) == JUMP_INSN
	      && GET_CODE (PATTERN (next)) == RETURN)
	    break;
	  return 1;
        default:
	  abort ();
        }
    }
  /* We have reached the end of the chain so lr was _not_ clobbered */
  return 0;
}

char *
output_return_instruction (operand, really_return)
rtx operand;
int really_return;
{
  char instr[100];
  int reg, live_regs = 0;

  if (current_function_calls_alloca && !really_return)
    abort();
    
  for (reg = 4; reg < 10; reg++)
    if (regs_ever_live[reg])
      live_regs++;

  if (live_regs || (regs_ever_live[14] && !lr_save_eliminated))
    live_regs++;

  if (frame_pointer_needed)
    live_regs += 4;

  if (live_regs)
    {
      if (lr_save_eliminated || !regs_ever_live[14])
        live_regs++;
      if (frame_pointer_needed)
        strcpy (instr, "ldm%d0ea\tfp, {");
      else
        strcpy (instr, "ldm%d0fd\tsp!, {");
      for (reg = 4; reg < 10; reg++)
        if (regs_ever_live[reg])
          {
            strcat (instr, reg_names[reg]);
	    if (--live_regs)
              strcat (instr, ", ");
          }
      if (frame_pointer_needed)
        {
          strcat (instr, reg_names[11]);
          strcat (instr, ", ");
          strcat (instr, reg_names[13]);
          strcat (instr, ", ");
          strcat (instr, really_return ? reg_names[15] : reg_names[14]);
        }
      else
        strcat (instr, really_return ? reg_names[15] : reg_names[14]);
      strcat (instr, (TARGET_6 || !really_return) ? "}" : "}^");
      arm_output_asm_insn (instr, &operand);
    }
  else if (really_return)
    {
      strcpy (instr, TARGET_6 ? "mov%d0\tpc, lr" : "mov%d0s\tpc, lr");
      arm_output_asm_insn (instr, &operand);
    }
  return_used_this_function = 1;
  return "";
}

/* The amount of stack adjustment that happens here, in output_return and in
   output_epilogue must be exactly the same as was calculated during reload,
   or things will point to the wrong place.  The only time we can safely
   ignore this constraint is when a function has no arguments on the stack,
   no stack frame requirement and no live registers execpt for `lr'.  If we
   can guarantee that by making all function calls into tail calls and that
   lr is not clobbered in any other way, then there is no need to push lr
   onto the stack. */
   
void
output_prologue (f, frame_size)
     FILE *f;
     int frame_size;
{
  int reg, live_regs_mask = 0, code_size = 0;
  rtx operands[3];

  /* Nonzero if we must stuff some register arguments onto the stack as if
     they were passed there.  */
  int store_arg_regs = 0;

  if (arm_ccfsm_state || arm_target_insn)
    abort ();					/* Sanity check */
  
  return_used_this_function = 0;
  lr_save_eliminated = 0;
  
  fprintf (f, "\t@ args = %d, pretend = %d, frame = %d\n",
	   current_function_args_size, current_function_pretend_args_size,
	   frame_size);
  fprintf (f, "\t@ frame_needed = %d, current_function_anonymous_args = %d\n",
	   frame_pointer_needed, current_function_anonymous_args);

  if (current_function_anonymous_args && current_function_pretend_args_size)
    store_arg_regs = 1;

  for (reg = 4; reg < 10; reg++)
    if (regs_ever_live[reg])
      live_regs_mask |= (1 << reg);

  if (frame_pointer_needed)
    {
      live_regs_mask |= 0xD800;
      fputs ("\tmov\tip, sp\n", f);
      code_size += 4;
    }
  else if (regs_ever_live[14])
    {
      if (! current_function_args_size
	  && !function_really_clobbers_lr (get_insns ()))
	{
	  fprintf (f,"\t@ I don't think this function clobbers lr\n");
	  lr_save_eliminated = 1;
        }
      else
        live_regs_mask |= 0x4000;
    }

  /* If CURRENT_FUNCTION_PRETEND_ARGS_SIZE, adjust the stack pointer to make
     room.  If also STORE_ARG_REGS store the argument registers involved in
     the created slot (this is for stdarg and varargs).  */
  if (current_function_pretend_args_size)
    {
      if (store_arg_regs)
	{
	  int arg_size, mask = 0;

	  assert (current_function_pretend_args_size <= 16);
	  for (reg = 3, arg_size = current_function_pretend_args_size;
	       arg_size > 0; reg--, arg_size -= 4)
	    mask |= (1 << reg);
	  print_multi_reg (f, "stmfd\tsp!", mask, FALSE);
	  code_size += 4;
	}
      else
	{
	  operands[0] = operands[1] = stack_pointer_rtx;
	  operands[2] = gen_rtx (CONST_INT, VOIDmode,
				 -current_function_pretend_args_size);
	  output_add_immediate (operands);
	}
    }

  if (live_regs_mask)
    {
      /* if a di mode load/store multiple is used, and the base register
	 is r3, then r4 can become an ever live register without lr
	 doing so,  in this case we need to push lr as well, or we
	 will fail to get a proper return. */

      live_regs_mask |= 0x4000;
      lr_save_eliminated = 0;
      print_multi_reg (f, "stmfd\tsp!", live_regs_mask, FALSE);
      code_size += 4;
    }

  for (reg = 23; reg > 19; reg--)
    if (regs_ever_live[reg])
      {
	fprintf (f, "\tstfe\t%s, [sp, #-12]!\n", reg_names[reg]);
	code_size += 4;
      }

  if (frame_pointer_needed)
    {
      /* Make `fp' point to saved value of `pc'. */

      operands[0] = gen_rtx (REG, SImode, HARD_FRAME_POINTER_REGNUM);
      operands[1] = gen_rtx (REG, SImode, 12);
      operands[2] = gen_rtx (CONST_INT, VOIDmode,
			     - (4 + current_function_pretend_args_size));
      output_add_immediate (operands);
    }

  if (frame_size)
    {
      operands[0] = operands[1] = stack_pointer_rtx;
      operands[2] = gen_rtx (CONST_INT, VOIDmode, -frame_size);
      output_add_immediate (operands);
    }

  arm_increase_location (code_size);
} /* output_prologue */


void
output_epilogue (f, frame_size)
     FILE *f;
     int frame_size;
{
  int reg, live_regs_mask = 0, code_size = 0;
  /* If we need this then it will always be at lesat this much */
  int floats_offset = 24;
  rtx operands[3];

  if (use_return_insn() && return_used_this_function)
    {
      if (frame_size && !(frame_pointer_needed || TARGET_APCS))
        {
          abort ();
        }
      return;
    }

  for (reg = 4; reg <= 10; reg++)
    if (regs_ever_live[reg])
      {
        live_regs_mask |= (1 << reg);
	floats_offset += 4;
      }


  if (frame_pointer_needed)
    {
      for (reg = 23; reg >= 20; reg--)
	if (regs_ever_live[reg])
	  {
	    fprintf (f, "\tldfe\t%s, [fp, #-%d]\n", reg_names[reg],
		     floats_offset);
	    floats_offset += 12;
	    code_size += 4;
	  }

      live_regs_mask |= 0xA800;
      print_multi_reg (f, "ldmea\tfp", live_regs_mask,
		       TARGET_6 ? FALSE : TRUE);
      code_size += 4;
    }
  else
    {
      /* Restore stack pointer if necessary.  */
      if (frame_size)
	{
	  operands[0] = operands[1] = stack_pointer_rtx;
	  operands[2] = gen_rtx (CONST_INT, VOIDmode, frame_size);
	  output_add_immediate (operands);
	}

      for (reg = 20; reg < 24; reg++)
	if (regs_ever_live[reg])
	  {
	    fprintf (f, "\tldfe\t%s, [sp], #12\n", reg_names[reg]);
	    code_size += 4;
	  }
      if (current_function_pretend_args_size == 0 && regs_ever_live[14])
	{
	  print_multi_reg (f, "ldmfd\tsp!", live_regs_mask | 0x8000,
			   TARGET_6 ? FALSE : TRUE);
	  code_size += 4;
	}
      else
	{
	  if (live_regs_mask || regs_ever_live[14])
	    {
	      live_regs_mask |= 0x4000;
	      print_multi_reg (f, "ldmfd\tsp!", live_regs_mask, FALSE);
	      code_size += 4;
	    }
	  if (current_function_pretend_args_size)
	    {
	      operands[0] = operands[1] = stack_pointer_rtx;
	      operands[2] = gen_rtx (CONST_INT, VOIDmode,
				     current_function_pretend_args_size);
	      output_add_immediate (operands);
	    }
	  fputs (TARGET_6 ? "\tmov\tpc, lr\n" : "\tmovs\tpc, lr\n", f);
	  code_size += 4;
	}
    }
  arm_increase_location (code_size);
  current_function_anonymous_args = 0;
} /* output_epilogue */

/* Increase the `arm_text_location' by AMOUNT if we're in the text
   segment.  */

void
arm_increase_location (amount)
     int amount;
{
  if (in_text_section ())
    arm_text_location += amount;
} /* arm_increase_location */


/* Like output_asm_insn (), but also increases the arm_text_location (if in
   the .text segment, of course, even though this will always be true).
   Returns the empty string.  */

char *
arm_output_asm_insn (template, operands)
     char *template;
     rtx *operands;
{
  extern FILE *asm_out_file;

  output_asm_insn (template, operands);
  if (in_text_section ())
    arm_text_location += 4;
  fflush (asm_out_file);
  return ("");
} /* arm_output_asm_insn */


/* Output a label definition.  If this label is within the .text segment, it
   is stored in OFFSET_TABLE, to be used when building `llc' instructions.
   Maybe GCC remembers names not starting with a `*' for a long time, but this
   is a minority anyway, so we just make a copy.  Do not store the leading `*'
   if the name starts with one.  */

void
arm_asm_output_label (stream, name)
     FILE *stream;
     char *name;
{
  char *real_name, *s;
  struct label_offset *cur;
  int hash = 0;

  assemble_name (stream, name);
  fputs (":\n", stream);
  if (! in_text_section ())
    return;

  if (name[0] == '*')
    {
      real_name = xmalloc (1 + strlen (&name[1]));
      strcpy (real_name, &name[1]);
    }
  else
    {
      real_name = xmalloc (2 + strlen (name));
      strcpy (real_name, "_");
      strcat (real_name, name);
    }
  for (s = real_name; *s; s++)
    hash += *s;
  hash = hash % LABEL_HASH_SIZE;
  cur = (struct label_offset *) xmalloc (sizeof (struct label_offset));
  cur->name = real_name;
  cur->offset = arm_text_location;
  cur->cdr = offset_table[hash];
  offset_table[hash] = cur;
} /* arm_asm_output_label */


/* Output the instructions needed to perform what Martin's /bin/as called
   llc: load an SImode thing from the function's constant pool.

   XXX This could be enhanced in that we do not really need a pointer in the
   constant pool pointing to the real thing.  If we can address this pointer,
   we can also address what it is pointing at, in fact, anything in the text
   segment which has been defined already within this .s file.  */

char *
arm_output_llc (operands)
     rtx *operands;
{
  char *s, *name = XSTR (XEXP (operands[1], 0), 0);
  struct label_offset *he;
  int hash = 0, conditional = (arm_ccfsm_state == 3 || arm_ccfsm_state == 4);

  if (*name != '*')
    abort ();

  for (s = &name[1]; *s; s++)
    hash += *s;
  hash = hash % LABEL_HASH_SIZE;
  he = offset_table[hash];
  while (he && strcmp (he->name, &name[1]))
    he = he->cdr;

  if (!he)
    abort ();

  if (arm_text_location + 8 - he->offset < 4095)
    {
      fprintf (asm_out_file, "\tldr%s\t%s, [pc, #%s - . - 8]\n",
	       conditional ? arm_condition_codes[arm_current_cc] : "",
	       reg_names[REGNO (operands[0])], &name[1]);
      arm_increase_location (4);
      return ("");
    }
  else
    {
      int offset = - (arm_text_location + 8 - he->offset);
      char *reg_name = reg_names[REGNO (operands[0])];

      /* ??? This is a hack, assuming the constant pool never is more than
	 (1 + 255) * 4096 == 1Meg away from the PC.  */

      if (offset > 1000000)
	abort ();

      fprintf (asm_out_file, "\tsub%s\t%s, pc, #(8 + . - %s) & ~4095\n",
	       conditional ? arm_condition_codes[arm_current_cc] : "",
	       reg_name, &name[1]);
      fprintf (asm_out_file, "\tldr%s\t%s, [%s, #- ((4 + . - %s) & 4095)]\n",
	       conditional ? arm_condition_codes[arm_current_cc] : "",
	       reg_name, reg_name, &name[1]);
      arm_increase_location (8);
    }
  return ("");
} /* arm_output_llc */

/* output_load_symbol ()
   load a symbol that is known to be in the text segment into a register */

char *
output_load_symbol (operands)
rtx *operands;
{
  char *s, *name = XSTR (operands[1], 0);
  struct label_offset *he;
  int hash = 0;
  int offset;
  
  if (*name != '*')
    abort ();

  for (s = &name[1]; *s; s++)
    hash += *s;
  hash = hash % LABEL_HASH_SIZE;
  he = offset_table[hash];
  while (he && strcmp (he->name, &name[1]))
    he = he->cdr;
  
  if (!he)
    abort ();
  
  offset = (arm_text_location + 8 - he->offset);
  if (offset < 0)
    abort ();

  /* If the symbol is word aligned then we might be able to reduce the
     number of loads */
  if ((offset & 3) == 0)
    {
      arm_output_asm_insn ("sub\t%0, pc, #(8 + . -%a1) & 1023", operands);
      if (offset > 0x3ff)
        {
	  arm_output_asm_insn ("sub\t%0, %0, #(4 + . -%a1) & 261120",
			       operands);
	  if (offset > 0x3ffff)
	    {
	      arm_output_asm_insn ("sub\t%0, %0, #(. -%a1) & 66846720",
				   operands);
	      if (offset > 0x3ffffff)
		arm_output_asm_insn ("sub\t%0, %0, #(. - 4 -%a1) & -67108864",
				       operands);
	    }
        }
    }
  else
    {
      arm_output_asm_insn ("sub\t%0, pc, #(8 + . -%a1) & 255", operands);
      if (offset > 0x0ff)
        {
	  arm_output_asm_insn ("sub\t%0, %0, #(4 + . -%a1) & 65280", operands);
	  if (offset > 0x0ffff)
	    {
	      arm_output_asm_insn ("sub\t%0, %0, #(. -%a1) & 16711680",
				   operands);
	      if (offset > 0x0ffffff)
		arm_output_asm_insn ("sub\t%0, %0, #(. - 4 -%a1) & -16777216",
				     operands);
	    }
        }
    }
  return "";
}

/* Output code resembling an .lcomm directive.  /bin/as doesn't have this
   directive hence this hack, which works by reserving some `.space' in the
   bss segment directly.

   XXX This is a severe hack, which is guaranteed NOT to work since it doesn't
   define STATIC COMMON space but merely STATIC BSS space.  */

void
output_lcomm_directive (stream, name, size, rounded)
     FILE *stream;
     char *name;
     int size, rounded;
{
  fputs ("\n\t.bss\t@ .lcomm\n", stream);
  assemble_name (stream, name);
  fprintf (stream, ":\t.space\t%d\n", rounded);
  if (in_text_section ())
    fputs ("\n\t.text\n", stream);
  else
    fputs ("\n\t.data\n", stream);
} /* output_lcomm_directive */

/* A finite state machine takes care of noticing whether or not instructions
   can be conditionally executed, and thus decrease execution time and code
   size by deleting branch instructions.  The fsm is controlled by
   final_prescan_insn, and controls the actions of ASM_OUTPUT_OPCODE.  */

/* The state of the fsm controlling condition codes are:
   0: normal, do nothing special
   1: make ASM_OUTPUT_OPCODE not output this instruction
   2: make ASM_OUTPUT_OPCODE not output this instruction
   3: make instructions conditional
   4: make instructions conditional

   State transitions (state->state by whom under condition):
   0 -> 1 final_prescan_insn if the `target' is a label
   0 -> 2 final_prescan_insn if the `target' is an unconditional branch
   1 -> 3 ASM_OUTPUT_OPCODE after not having output the conditional branch
   2 -> 4 ASM_OUTPUT_OPCODE after not having output the conditional branch
   3 -> 0 ASM_OUTPUT_INTERNAL_LABEL if the `target' label is reached
          (the target label has CODE_LABEL_NUMBER equal to arm_target_label).
   4 -> 0 final_prescan_insn if the `target' unconditional branch is reached
          (the target insn is arm_target_insn).

   If the jump clobbers the conditions then we use states 2 and 4.

   A similar thing can be done with conditional return insns.

   XXX In case the `target' is an unconditional branch, this conditionalising
   of the instructions always reduces code size, but not always execution
   time.  But then, I want to reduce the code size to somewhere near what
   /bin/cc produces.  */

/* The condition codes of the ARM, and the inverse function.  */
char *arm_condition_codes[] =
{
  "eq", "ne", "cs", "cc", "mi", "pl", "vs", "vc",
  "hi", "ls", "ge", "lt", "gt", "le", "al", "nv"
};

#define ARM_INVERSE_CONDITION_CODE(X)  ((X) ^ 1)

/* Returns the index of the ARM condition code string in
   `arm_condition_codes'.  COMPARISON should be an rtx like
   `(eq (...) (...))'.  */

int
get_arm_condition_code (comparison)
     rtx comparison;
{
  switch (GET_CODE (comparison))
    {
    case NE: return (1);
    case EQ: return (0);
    case GE: return (10);
    case GT: return (12);
    case LE: return (13);
    case LT: return (11);
    case GEU: return (2);
    case GTU: return (8);
    case LEU: return (9);
    case LTU: return (3);
    default: abort ();
    }
  /*NOTREACHED*/
  return (42);
} /* get_arm_condition_code */


void
final_prescan_insn (insn, opvec, noperands)
     rtx insn;
     rtx *opvec;
     int noperands;
{
  /* BODY will hold the body of INSN.  */
  register rtx body = PATTERN (insn);

  /* This will be 1 if trying to repeat the trick, and things need to be
     reversed if it appears to fail.  */
  int reverse = 0;

  /* JUMP_CLOBBERS will be one implies that the conditions if a branch is
     taken are clobbered, even if the rtl suggests otherwise.  It also
     means that we have to grub around within the jump expression to find
     out what the conditions are when the jump isn't taken.  */
  int jump_clobbers = 0;
  
  /* If we start with a return insn, we only succeed if we find another one. */
  int seeking_return = 0;
  
  /* START_INSN will hold the insn from where we start looking.  This is the
     first insn after the following code_label if REVERSE is true.  */
  rtx start_insn = insn;

  /* If in state 4, check if the target branch is reached, in order to
     change back to state 0.  */
  if (arm_ccfsm_state == 4)
    {
      if (insn == arm_target_insn)
      {
	arm_target_insn = NULL;
	arm_ccfsm_state = 0;
      }
      return;
    }

  /* If in state 3, it is possible to repeat the trick, if this insn is an
     unconditional branch to a label, and immediately following this branch
     is the previous target label which is only used once, and the label this
     branch jumps to is not too far off.  */
  if (arm_ccfsm_state == 3)
    {
      if (simplejump_p (insn))
	{
	  start_insn = next_nonnote_insn (start_insn);
	  if (GET_CODE (start_insn) == BARRIER)
	    {
	      /* XXX Isn't this always a barrier?  */
	      start_insn = next_nonnote_insn (start_insn);
	    }
	  if (GET_CODE (start_insn) == CODE_LABEL
	      && CODE_LABEL_NUMBER (start_insn) == arm_target_label
	      && LABEL_NUSES (start_insn) == 1)
	    reverse = TRUE;
	  else
	    return;
	}
      else if (GET_CODE (body) == RETURN)
        {
	  start_insn = next_nonnote_insn (start_insn);
	  if (GET_CODE (start_insn) == BARRIER)
	    start_insn = next_nonnote_insn (start_insn);
	  if (GET_CODE (start_insn) == CODE_LABEL
	      && CODE_LABEL_NUMBER (start_insn) == arm_target_label
	      && LABEL_NUSES (start_insn) == 1)
	    {
	      reverse = TRUE;
	      seeking_return = 1;
	    }
	  else
	    return;
        }
      else
	return;
    }

  if (arm_ccfsm_state != 0 && !reverse)
    abort ();
  if (GET_CODE (insn) != JUMP_INSN)
    return;

  /* This jump might be paralled with a clobber of the condition codes 
     the jump should always come first */
  if (GET_CODE (body) == PARALLEL && XVECLEN (body, 0) > 0)
    body = XVECEXP (body, 0, 0);

#if 0  
  /* If this is a conditional return then we don't want to know */
  if (GET_CODE (body) == SET && GET_CODE (SET_DEST (body)) == PC
      && GET_CODE (SET_SRC (body)) == IF_THEN_ELSE
      && (GET_CODE (XEXP (SET_SRC (body), 1)) == RETURN
          || GET_CODE (XEXP (SET_SRC (body), 2)) == RETURN))
    return;
#endif

  if (reverse
      || (GET_CODE (body) == SET && GET_CODE (SET_DEST (body)) == PC
	  && GET_CODE (SET_SRC (body)) == IF_THEN_ELSE))
    {
      int insns_skipped = 0, fail = FALSE, succeed = FALSE;
      /* Flag which part of the IF_THEN_ELSE is the LABEL_REF.  */
      int then_not_else = TRUE;
      rtx this_insn = start_insn, label = 0;

      if (get_attr_conds (insn) == CONDS_JUMP_CLOB)
	jump_clobbers = 1;
      
      /* Register the insn jumped to.  */
      if (reverse)
        {
	  if (!seeking_return)
	    label = XEXP (SET_SRC (body), 0);
        }
      else if (GET_CODE (XEXP (SET_SRC (body), 1)) == LABEL_REF)
	label = XEXP (XEXP (SET_SRC (body), 1), 0);
      else if (GET_CODE (XEXP (SET_SRC (body), 2)) == LABEL_REF)
	{
	  label = XEXP (XEXP (SET_SRC (body), 2), 0);
	  then_not_else = FALSE;
	}
      else if (GET_CODE (XEXP (SET_SRC (body), 1)) == RETURN)
	seeking_return = 1;
      else if (GET_CODE (XEXP (SET_SRC (body), 2)) == RETURN)
        {
	  seeking_return = 1;
	  then_not_else = FALSE;
        }
      else
	abort ();

      /* See how many insns this branch skips, and what kind of insns.  If all
	 insns are okay, and the label or unconditional branch to the same
	 label is not too far away, succeed.  */
      for (insns_skipped = 0;
	   !fail && !succeed && insns_skipped < MAX_INSNS_SKIPPED;
	   insns_skipped++)
	{
	  rtx scanbody;

	  this_insn = next_nonnote_insn (this_insn);
	  if (!this_insn)
	    break;

	  scanbody = PATTERN (this_insn);

	  switch (GET_CODE (this_insn))
	    {
	    case CODE_LABEL:
	      /* Succeed if it is the target label, otherwise fail since
		 control falls in from somewhere else.  */
	      if (this_insn == label)
		{
		  if (jump_clobbers)
		    {
		      arm_ccfsm_state = 2;
		      this_insn = next_nonnote_insn (this_insn);
		    }
		  else
		    arm_ccfsm_state = 1;
		  succeed = TRUE;
		}
	      else
		fail = TRUE;
	      break;

	    case BARRIER:
	      /* Succeed if the following insn is the target label.
		 Otherwise fail.  
		 If return insns are used then the last insn in a function 
		 will be a barrier. */
	      this_insn = next_nonnote_insn (this_insn);
	      if (this_insn && this_insn == label)
		{
		  if (jump_clobbers)
		    {
		      arm_ccfsm_state = 2;
		      this_insn = next_nonnote_insn (this_insn);
		    }
		  else
		    arm_ccfsm_state = 1;
		  succeed = TRUE;
		}
	      else
		fail = TRUE;
	      break;

	    case CALL_INSN:
	      /* The arm 6xx uses full 32 bit addresses so the cc is not 
		 preserved over calls */
	      if (TARGET_6)
		fail = TRUE;
	      break;
	    case JUMP_INSN:
      	      /* If this is an unconditional branch to the same label, succeed.
		 If it is to another label, do nothing.  If it is conditional,
		 fail.  */
	      /* XXX Probably, the test for the SET and the PC are unnecessary. */

	      if (GET_CODE (scanbody) == SET
		  && GET_CODE (SET_DEST (scanbody)) == PC)
		{
		  if (GET_CODE (SET_SRC (scanbody)) == LABEL_REF
		      && XEXP (SET_SRC (scanbody), 0) == label && !reverse)
		    {
		      arm_ccfsm_state = 2;
		      succeed = TRUE;
		    }
		  else if (GET_CODE (SET_SRC (scanbody)) == IF_THEN_ELSE)
		    fail = TRUE;
		}
	      else if (GET_CODE (scanbody) == RETURN
		       && seeking_return)
	        {
		  arm_ccfsm_state = 2;
		  succeed = TRUE;
	        }
	      else if (GET_CODE (scanbody) == PARALLEL)
	        {
		  switch (get_attr_conds (this_insn))
		    {
		    case CONDS_NOCOND:
		      break;
		    default:
		      fail = TRUE;
		      break;
		    }
		}
	      break;

	    case INSN:
	      /* Instructions using or affecting the condition codes make it
		 fail.  */
	      if ((GET_CODE (scanbody) == SET
		   || GET_CODE (scanbody) == PARALLEL)
		  && get_attr_conds (this_insn) != CONDS_NOCOND)
		fail = TRUE;
	      break;

	    default:
	      break;
	    }
	}
      if (succeed)
	{
	  if ((!seeking_return) && (arm_ccfsm_state == 1 || reverse))
	    arm_target_label = CODE_LABEL_NUMBER (label);
	  else if (seeking_return || arm_ccfsm_state == 2)
	    {
	      while (this_insn && GET_CODE (PATTERN (this_insn)) == USE)
	        {
		  this_insn = next_nonnote_insn (this_insn);
		  if (this_insn && (GET_CODE (this_insn) == BARRIER
				    || GET_CODE (this_insn) == CODE_LABEL))
		    abort ();
	        }
	      if (!this_insn)
	        {
		  /* Oh, dear! we ran off the end.. give up */
		  recog (PATTERN (insn), insn, NULL_PTR);
		  arm_ccfsm_state = 0;
		  arm_target_insn = NULL;
		  return;
	        }
	      arm_target_insn = this_insn;
	    }
	  else
	    abort ();
	  if (jump_clobbers)
	    {
	      if (reverse)
		abort ();
	      arm_current_cc = 
		  get_arm_condition_code (XEXP (XEXP (XEXP (SET_SRC (body),
							    0), 0), 1));
	      if (GET_CODE (XEXP (XEXP (SET_SRC (body), 0), 0)) == AND)
		arm_current_cc = ARM_INVERSE_CONDITION_CODE (arm_current_cc);
	      if (GET_CODE (XEXP (SET_SRC (body), 0)) == NE)
		arm_current_cc = ARM_INVERSE_CONDITION_CODE (arm_current_cc);
	    }
	  else
	    {
	      /* If REVERSE is true, ARM_CURRENT_CC needs to be inverted from
		 what it was.  */
	      if (!reverse)
		arm_current_cc = get_arm_condition_code (XEXP (SET_SRC (body),
							       0));
	    }

	  if (reverse || then_not_else)
	    arm_current_cc = ARM_INVERSE_CONDITION_CODE (arm_current_cc);
	}
      /* restore recog_operand (getting the attributes of other insns can
	 destroy this array, but final.c assumes that it remains intact
	 accross this call; since the insn has been recognized already we
	 call recog direct). */
      recog (PATTERN (insn), insn, NULL_PTR);
    }
} /* final_prescan_insn */

/* EOF */
